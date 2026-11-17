(define-record-type opener
  (nongenerative)
  (sealed #t)
  (fields (mutable match-index) start end (mutable kind)
          (mutable sub-match-index) (mutable sub-start) (mutable sub-end)))

(define (new-opener match-index start end)
  (make-opener match-index start end #f match-index #f #f))

(define delimiter-chars "_*^~+='\"-")

(define (opener-slot c braced?)
  (case c
    [(#\[) 0]
    [(#\() 1]
    [else
     (let loop ([i 0])
       (if (char=? (string-ref delimiter-chars i) c)
           (if braced? (fx+ i 2 (string-length delimiter-chars)) (fx+ i 2))
           (loop (fx+ i 1))))]))

(define opener-slot-count (fx+ 2 (fx* 2 (string-length delimiter-chars))))

(define-record-type inline-parser
  (nongenerative)
  (sealed #t)
  (fields subject matches openers attribute-slices
          (mutable verbatim) (mutable verbatim-close) (mutable destination?)
          (mutable firstpos) (mutable lastpos) (mutable allow-attributes?)
          (mutable attribute-parser) (mutable attribute-start)
          (mutable pending-span) (mutable spare-attribute-parser)
          (mutable opener-slots))
  (protocol
    (lambda (new)
      (lambda (subject)
        (let ([p (new subject (make-events 64) (make-vector opener-slot-count) (make-events 8)
                      0 0 #f 0 0 #f #f #f #f #f 0)])
          (inline-parser-reset! p)
          p)))))

;; A document has many short paragraphs, headings, cells and attribute
;; sets, so their parsers are reused rather than made for each.
(define (inline-parser-reset! p)
  (events-truncate! (inline-parser-matches p) 0)
  (vector-fill! (inline-parser-openers p) '())
  (inline-parser-opener-slots-set! p 0)
  (events-truncate! (inline-parser-attribute-slices p) 0)
  (inline-parser-verbatim-set! p 0)
  (inline-parser-verbatim-close-set! p (annotation-code close-verbatim))
  (inline-parser-destination?-set! p #f)
  (inline-parser-firstpos-set! p -1)
  (inline-parser-lastpos-set! p 0)
  (inline-parser-allow-attributes?-set! p #t)
  (inline-parser-attribute-parser-set! p #f)
  (inline-parser-attribute-start-set! p #f)
  (inline-parser-pending-span-set! p #f))

(define (reused-attribute-parser p)
  (let ([ap (or (inline-parser-spare-attribute-parser p)
                (let ([ap (make-attribute-parser (inline-parser-subject p))])
                  (inline-parser-spare-attribute-parser-set! p ap)
                  ap))])
    (attribute-parser-reset! ap)
    ap))

(define (add-attribute-slice! p start end)
  (events-add! (inline-parser-attribute-slices p) start end (annotation-code str)))

(define inline-special
  (let ([table (make-bytevector 128 0)])
    (for-each
      (lambda (c) (bytevector-u8-set! table (char->integer c) 1))
      (string->list "\r\n\"'()*+.:<=[\\]^_`${}~-"))
    table))

(define (find-special s start end)
  (let loop ([i start])
    (and (fx<= i end)
         (let ([n (char->integer (string-ref s i))])
           (if (and (fx< n 128) (fx= (bytevector-u8-ref inline-special n) 1))
               i
               (loop (fx+ i 1)))))))

(define (nonspace-at? s pos)
  (and (fx< pos (string-length s))
       (not (memv (string-ref s pos) '(#\space #\tab #\return #\newline)))))

(define (ascii-punctuation? c)
  (and (char? c)
       (let ([n (char->integer c)])
         (or (fx<= 33 n 47) (fx<= 58 n 64) (fx<= 91 n 96) (fx<= 123 n 126)))))

(define (add-match! p start end annotation)
  (events-add! (inline-parser-matches p) start end annotation))

(define (match-count p) (events-count (inline-parser-matches p)))

(define (openers-ref p slot) (vector-ref (inline-parser-openers p) slot))

;; clear-openers! runs at every closer and most stacks are empty, so it
;; visits only the slots marked in opener-slots.
(define (openers-set! p slot stack)
  (vector-set! (inline-parser-openers p) slot stack)
  (inline-parser-opener-slots-set! p
    (fxcopy-bit (inline-parser-opener-slots p) slot (if (null? stack) 0 1))))

(define (add-opener! p slot start end default)
  (openers-set! p slot (cons (new-opener (match-count p) start end) (openers-ref p slot)))
  (add-match! p start end default))

;; Most stacks are untouched, and storing into the openers vector, which
;; outlives many collections, costs a write barrier.
(define (clear-openers! p start end)
  (let next ([slots (inline-parser-opener-slots p)])
    (unless (fx= slots 0)
      (let* ([slot (fxfirst-bit-set slots)]
             [stack (openers-ref p slot)]
             [cleared (let loop ([stack stack])
                        (if (null? stack)
                            '()
                            (let ([o (car stack)])
                              (cond
                                [(and (fx>= (opener-start o) start) (fx<= (opener-end o) end))
                                 (loop (cdr stack))]
                                [(and (opener-sub-start o) (fx>= (opener-sub-start o) start)
                                      (opener-sub-end o) (fx<= (opener-sub-end o) end))
                                 (opener-sub-start-set! o #f)
                                 (opener-sub-end-set! o #f)
                                 (opener-kind-set! o #f)
                                 (cons o (loop (cdr stack)))]
                                [else stack]))))])
        (unless (eq? cleared stack)
          (openers-set! p slot cleared))
        (next (fxcopy-bit slots slot 0))))))

(define (str-matches! p start end)
  (let ([m (inline-parser-matches p)])
    (let ([i (let back ([i (fx- (events-count m) 1)])
               (if (and (fx> i 0) (fx>= (events-start m i) start)) (back (fx- i 1)) i))])
      (let loop ([i (if (fx< (events-start m i) start) (fx+ i 1) i)])
        (when (and (fx< i (events-count m)) (fx<= (events-end m i) end))
          (annotation-case (events-annotation m i)
            [(escape str open-marker) (void)]
            [else (events-annotation-set! m i (annotation-code str))])
          (loop (fx+ i 1)))))))

(define (add-image-marker! p o)
  (let* ([m (inline-parser-matches p)]
         [previous (fx- (opener-match-index o) 1)])
    (if (and (fx>= previous 0)
             (fx< previous (events-count m))
             (fx= (events-annotation m previous) (annotation-code str))
             (fx< (events-start m previous) (fx- (opener-start o) 1)))
        (begin
          (events-end-set! m previous (fx- (opener-start o) 2))
          (events-insert! m (fx+ previous 1) (fx- (opener-start o) 1) (fx- (opener-start o) 1)
            (annotation-code image-marker))
          (opener-match-index-set! o (fx+ (opener-match-index o) 1))
          (opener-sub-match-index-set! o (fx+ (opener-sub-match-index o) 1)))
        (events-set! m previous (fx- (opener-start o) 1) (fx- (opener-start o) 1)
          (annotation-code image-marker)))))

(define (reparse-attributes! p)
  (let ([slices (inline-parser-attribute-slices p)])
    (cond
      [(inline-parser-pending-span p)
       => (lambda (span)
            (events-annotation-set! (inline-parser-matches p) (car span) (annotation-code str))
            (events-annotation-set! (inline-parser-matches p) (cdr span) (annotation-code str))
            (inline-parser-pending-span-set! p #f))])
    (inline-parser-allow-attributes?-set! p #f)
    (inline-parser-attribute-parser-set! p #f)
    (inline-parser-attribute-start-set! p #f)
    (do ([i 0 (fx+ i 1)]) ((fx= i (events-count slices)))
      (inline-feed! p (events-start slices i) (events-end slices i)))
    (inline-parser-allow-attributes?-set! p #t)
    (events-truncate! slices 0)))

(define (inline-parser-finish! p)
  (let ([s (inline-parser-subject p)] [m (inline-parser-matches p)])
    (when (inline-parser-attribute-parser p)
      (reparse-attributes! p))
    (when (eqv? (events-last-annotation m) (annotation-code soft-break))
      (events-pop! m)
      (let ([n (events-count m)])
        (when (and (fx> n 0)
                   (fx= (events-annotation m (fx- n 1)) (annotation-code str))
                   (eqv? (char-at s (events-end m (fx- n 1))) #\space))
          (let ([start (events-start m (fx- n 1))])
            (let loop ([end (events-end m (fx- n 1))])
              (if (and (fx>= end start) (eqv? (char-at s end) #\space))
                  (loop (fx- end 1))
                  (if (fx< end start)
                      (events-pop! m)
                      (events-end-set! m (fx- n 1) end))))))))
    (let ([n (events-count m)])
      (when (and (fx> n 0) (fx> (inline-parser-verbatim p) 0))
        (let ([end (events-end m (fx- n 1))])
          (add-match! p end end (inline-parser-verbatim-close p)))))
    m))

(define (between-matched p pos end c open close default open-test)
  (let* ([s (inline-parser-subject p)]
         [m (inline-parser-matches p)]
         [has-open-marker? (eqv? (events-last-annotation m) (annotation-code open-marker))]
         [has-close-marker? (and (fx<= (fx+ pos 1) end) (eqv? (string-ref s (fx+ pos 1)) #\}))]
         [can-open? (cond
                      [has-open-marker? #t]
                      [has-close-marker? #f]
                      [else (and (nonspace-at? s (fx+ pos 1)) (open-test p pos))])]
         [can-close? (cond
                       [has-open-marker? #f]
                       [has-close-marker? #t]
                       [else (nonspace-at? s (if (fx> pos 0) (fx- pos 1) 0))])]
         [opener-position (if has-open-marker? (fx- pos 1) pos)]
         [closer-end (if (and (not has-open-marker?) has-close-marker?) (fx+ pos 1) pos)]
         [default (cond
                    [(and has-open-marker? (fx= default (annotation-code right-single-quote)))
                     (annotation-code left-single-quote)]
                    [(and has-close-marker? (fx= default (annotation-code left-double-quote)))
                     (annotation-code right-double-quote)]
                    [else default])]
         [stack (openers-ref p (opener-slot c has-close-marker?))])
    (define (unmatched)
      (cond
        [can-open?
         (add-opener! p (opener-slot c has-open-marker?) opener-position pos default)
         (fx+ pos 1)]
        [else
         (add-match! p pos closer-end default)
         (fx+ closer-end 1)]))
    (if (and can-close? (pair? stack) (not (fx= (opener-end (car stack)) (fx- pos 1))))
        (let ([o (car stack)])
          (if (and (inline-parser-destination? p)
                   (let ([links (openers-ref p (opener-slot #\[ #f))])
                     (and (pair? links)
                          (eq? (opener-kind (car links)) 'explicit-link)
                          (fx< (opener-start o) (opener-start (car links))))))
              (begin
                (add-match! p pos closer-end default)
                (fx+ closer-end 1))
              (begin
                (clear-openers! p (opener-start o) pos)
                (events-set! m (opener-match-index o) (opener-start o) (opener-end o) open)
                (add-match! p pos closer-end close)
                (fx+ closer-end 1))))
        (unmatched))))

(define (always p pos) #t)

(define (has-brace? p pos)
  (let ([s (inline-parser-subject p)])
    (or (and (fx> pos 0) (eqv? (char-at s (fx- pos 1)) #\{))
        (eqv? (char-at s (fx+ pos 1)) #\}))))

(define (single-quote-opens? p pos)
  (or (fx= pos 0)
      (memv (string-ref (inline-parser-subject p) (fx- pos 1))
            '(#\space #\tab #\return #\newline #\" #\' #\- #\( #\[))))

(define (run-end s pos end c)
  (let loop ([i pos])
    (if (and (fx<= (fx+ i 1) end) (char=? (string-ref s (fx+ i 1)) c))
        (loop (fx+ i 1))
        i)))

(define (match-backtick p pos end)
  (let* ([s (inline-parser-subject p)]
         [m (inline-parser-matches p)]
         [last (run-end s pos end #\`)]
         [n (events-count m)]
         [dollar-escaped? (and (fx>= n 2)
                               (fx= (events-annotation m (fx- n 2)) (annotation-code escape))
                               (fx= (events-end m (fx- n 2)) (fx- pos 2)))])
    (cond
      [(and (fx>= pos 2) (char=? (string-ref s (fx- pos 2)) #\$)
            (char=? (string-ref s (fx- pos 1)) #\$)
            (not (and (fx>= pos 3) (char=? (string-ref s (fx- pos 3)) #\\))))
       (events-pop! m)
       (events-pop! m)
       (add-match! p (fx- pos 2) last (annotation-code open-display-math))
       (inline-parser-verbatim-close-set! p (annotation-code close-display-math))]
      [(and (fx>= pos 1) (char=? (string-ref s (fx- pos 1)) #\$) (not dollar-escaped?))
       (events-pop! m)
       (add-match! p (fx- pos 1) last (annotation-code open-inline-math))
       (inline-parser-verbatim-close-set! p (annotation-code close-inline-math))]
      [else
       (add-match! p pos last (annotation-code open-verbatim))
       (inline-parser-verbatim-close-set! p (annotation-code close-verbatim))])
    (inline-parser-verbatim-set! p (fx+ (fx- last pos) 1))
    (fx+ last 1)))

(define (line-end-at s pos end)
  (let loop ([i pos])
    (and (fx<= i end)
         (case (string-ref s i)
           [(#\space #\tab) (loop (fx+ i 1))]
           [(#\return) (and (fx<= (fx+ i 1) end) (char=? (string-ref s (fx+ i 1)) #\newline) (fx+ i 1))]
           [(#\newline) i]
           [else #f]))))

(define (match-backslash p pos end)
  (let ([s (inline-parser-subject p)] [m (inline-parser-matches p)])
    (cond
      [(line-end-at s (fx+ pos 1) end)
       => (lambda (line-end)
            (when (eqv? (events-last-annotation m) (annotation-code str))
              (let* ([i (fx- (events-count m) 1)] [start (events-start m i)])
                (let loop ([e (events-end m i)])
                  (if (and (fx>= e start) (memv (string-ref s e) '(#\space #\tab)))
                      (loop (fx- e 1))
                      (if (fx< e start) (events-pop! m) (events-end-set! m i e))))))
            (add-match! p pos pos (annotation-code escape))
            (add-match! p (fx+ pos 1) line-end (annotation-code hard-break))
            (fx+ line-end 1))]
      [(and (fx<= (fx+ pos 1) end) (ascii-punctuation? (string-ref s (fx+ pos 1))))
       (add-match! p pos pos (annotation-code escape))
       (add-match! p (fx+ pos 1) (fx+ pos 1) (annotation-code str))
       (fx+ pos 2)]
      [(and (fx<= (fx+ pos 1) end) (char=? (string-ref s (fx+ pos 1)) #\space))
       (add-match! p pos pos (annotation-code escape))
       (add-match! p (fx+ pos 1) (fx+ pos 1) (annotation-code non-breaking-space))
       (fx+ pos 2)]
      [else
       (add-match! p pos pos (annotation-code str))
       (fx+ pos 1)])))

(define (url-scheme-char? c)
  (or (ascii-alphanumeric? c) (memv c '(#\+ #\. #\-))))

(define (match-less-than p pos end)
  (let* ([s (inline-parser-subject p)]
         [url-end (let loop ([i (fx+ pos 1)])
                    (and (fx<= i end)
                         (let ([c (string-ref s i)])
                           (cond
                             [(char=? c #\>) (and (fx> i (fx+ pos 1)) i)]
                             [(or (char=? c #\<) (js-whitespace? c)) #f]
                             [else (loop (fx+ i 1))]))))])
    (define (autolink open close)
      (add-match! p pos pos open)
      (add-match! p (fx+ pos 1) (fx- url-end 1) (annotation-code str))
      (add-match! p url-end url-end close)
      (fx+ url-end 1))
    (and url-end
         (cond
           [(let loop ([i (fx+ pos 1)])
              (and (fx< i url-end)
                   (let ([c (string-ref s i)])
                     (cond
                       [(char=? c #\:) #f]
                       [(and (char=? c #\@) (fx> i (fx+ pos 1))) #t]
                       [else (loop (fx+ i 1))]))))
            (autolink (annotation-code open-email) (annotation-code close-email))]
           [(let loop ([i (fx+ pos 1)])
              (and (fx<= i url-end)
                   (let ([c (string-ref s i)])
                     (cond
                       [(url-scheme-char? c) (loop (fx+ i 1))]
                       [else (and (char=? c #\:) (fx> i (fx+ pos 1)))]))))
            (autolink (annotation-code open-url) (annotation-code close-url))]
           [else #f]))))

(define (match-left-brace p pos end)
  (let ([s (inline-parser-subject p)])
    (cond
      [(and (fx<= (fx+ pos 1) end) (memv (string-ref s (fx+ pos 1)) '(#\* #\+ #\- #\= #\^ #\_ #\~ #\' #\")))
       (add-match! p pos pos (annotation-code open-marker))
       (fx+ pos 1)]
      [(inline-parser-allow-attributes? p)
       (inline-parser-attribute-parser-set! p (reused-attribute-parser p))
       (inline-parser-attribute-start-set! p pos)
       (events-truncate! (inline-parser-attribute-slices p) 0)
       pos]
      [else
       (add-match! p pos pos (annotation-code str))
       (fx+ pos 1)])))

(define (symbol-char? c)
  (or (word-char? c) (eqv? c #\+) (eqv? c #\-)))

(define (match-colon p pos end)
  (let* ([s (inline-parser-subject p)]
         [close (let loop ([i (fx+ pos 1)])
                  (and (fx<= i end)
                       (let ([c (string-ref s i)])
                         (cond
                           [(symbol-char? c) (loop (fx+ i 1))]
                           [(char=? c #\:) (and (fx> i (fx+ pos 1)) i)]
                           [else #f]))))])
    (cond
      [close (add-match! p pos close (annotation-code symb)) (fx+ close 1)]
      [else (add-match! p pos pos (annotation-code str)) (fx+ pos 1)])))

(define (match-period p pos end)
  (let ([s (inline-parser-subject p)])
    (and (fx<= (fx+ pos 2) end)
         (char=? (string-ref s (fx+ pos 1)) #\.)
         (char=? (string-ref s (fx+ pos 2)) #\.)
         (begin
           (add-match! p pos (fx+ pos 2) (annotation-code ellipses))
           (fx+ pos 3)))))

(define (image-opener? s o)
  (and (eqv? (char-at s (fx- (opener-start o) 1)) #\!)
       (not (eqv? (char-at s (fx- (opener-start o) 2)) #\\))))

(define (close-link! p o pos open close)
  (let ([s (inline-parser-subject p)] [m (inline-parser-matches p)])
    (str-matches! p (fx+ (or (opener-sub-end o) (opener-end o)) 1) (fx- pos 1))
    (let ([text-end (or (opener-sub-start o) (opener-start o))])
      (cond
        [(image-opener? s o)
         (add-image-marker! p o)
         (events-set! m (opener-match-index o) (opener-start o) (opener-end o) (annotation-code open-imagetext))
         (events-set! m (opener-sub-match-index o) text-end text-end (annotation-code close-imagetext))]
        [else
         (events-set! m (opener-match-index o) (opener-start o) (opener-end o) (annotation-code open-linktext))
         (events-set! m (opener-sub-match-index o) text-end text-end (annotation-code close-linktext))]))
    (let ([e (or (opener-sub-end o) (opener-end o))])
      (events-set! m (fx+ (opener-sub-match-index o) 1) e e open))
    (add-match! p pos pos close)))

(define (match-right-bracket p pos end)
  (let ([s (inline-parser-subject p)]
        [m (inline-parser-matches p)]
        [stack (openers-ref p (opener-slot #\[ #f))])
    (and (pair? stack)
         (let ([o (car stack)])
           (define (next-is? c) (and (fx<= (fx+ pos 1) end) (char=? (string-ref s (fx+ pos 1)) c)))
           (define (mark-link! kind)
             (opener-kind-set! o kind)
             (add-match! p pos pos (annotation-code str))
             (opener-sub-match-index-set! o (fx- (match-count p) 1))
             (add-match! p (fx+ pos 1) (fx+ pos 1) (annotation-code str))
             (opener-sub-start-set! o pos)
             (opener-sub-end-set! o (fx+ pos 1))
             (clear-openers! p (fx+ (opener-start o) 1) (fx- pos 1))
             (fx+ pos 2))
           (cond
             [(eqv? (char-at s (fx+ (opener-start o) 1)) #\^)
              (let loop ([i (fx- (events-count m) 1)])
                (if (and (fx> i 0) (fx> (events-start m i) (opener-start o)))
                    (begin (events-pop! m) (loop (fx- i 1)))
                    (begin
                      (clear-openers! p (opener-start o) pos)
                      (events-annotation-set! m i (annotation-code footnote-reference))
                      (events-end-set! m i pos))))
              (fx+ pos 1)]
             [(eq? (opener-kind o) 'reference-link)
              (close-link! p o pos (annotation-code open-reference) (annotation-code close-reference))
              (clear-openers! p (opener-start o) pos)
              (fx+ pos 1)]
             [(next-is? #\[) (mark-link! 'reference-link)]
             [(next-is? #\()
              (openers-set! p (opener-slot #\( #f) '())
              (inline-parser-destination?-set! p #t)
              (mark-link! 'explicit-link)]
             [(next-is? #\{)
              (events-set! m (opener-match-index o) (opener-start o) (opener-end o) (annotation-code open-span))
              (add-match! p pos pos (annotation-code close-span))
              (when (inline-parser-allow-attributes? p)
                (inline-parser-pending-span-set! p (cons (opener-match-index o) (fx- (match-count p) 1))))
              (clear-openers! p (opener-start o) pos)
              (fx+ pos 1)]
             [else #f])))))

(define (match-left-paren p pos end)
  (and (inline-parser-destination? p)
       (begin
         (add-opener! p (opener-slot #\( #f) pos pos (annotation-code str))
         (fx+ pos 1))))

(define (match-right-paren p pos end)
  (and (inline-parser-destination? p)
       (let ([parens (openers-ref p (opener-slot #\( #f))])
         (if (pair? parens)
             (begin
               (openers-set! p (opener-slot #\( #f) (cdr parens))
               (add-match! p pos pos (annotation-code str))
               (fx+ pos 1))
             (let ([stack (openers-ref p (opener-slot #\[ #f))])
               (and (pair? stack)
                    (eq? (opener-kind (car stack)) 'explicit-link)
                    (let ([o (car stack)])
                      (close-link! p o pos (annotation-code open-destination) (annotation-code close-destination))
                      (inline-parser-destination?-set! p #f)
                      (clear-openers! p (opener-start o) pos)
                      (fx+ pos 1))))))))

(define (match-hyphen p pos end)
  (let ([s (inline-parser-subject p)])
    (if (or (eqv? (char-at s (fx- pos 1)) #\{) (eqv? (char-at s (fx+ pos 1)) #\}))
        (between-matched p pos end #\- (annotation-code open-delete) (annotation-code close-delete)
          (annotation-code str) has-brace?)
        (let* ([run (let loop ([i pos])
                      (if (and (fx<= i end) (char=? (string-ref s i) #\-)) (loop (fx+ i 1)) i))]
               [hyphens (if (eqv? (char-at s run) #\})
                            (fx- (fx- run pos) 1)
                            (fx- run pos))])
          (if (fx= hyphens 0)
              (begin (add-match! p pos (fx+ pos 1) (annotation-code str)) (fx+ pos 2))
              (let ([all-em? (fx= (fxmod hyphens 3) 0)] [all-en? (fx= (fxmod hyphens 2) 0)])
                (let loop ([pos pos] [hyphens hyphens])
                  (define (dash n annotation)
                    (add-match! p pos (fx+ pos (fx- n 1)) annotation)
                    (loop (fx+ pos n) (fx- hyphens n)))
                  (cond
                    [(fx<= hyphens 0) pos]
                    [all-em? (dash 3 (annotation-code em-dash))]
                    [all-en? (dash 2 (annotation-code en-dash))]
                    [(and (fx>= hyphens 3) (or (fxodd? hyphens) (fx> hyphens 4)))
                     (dash 3 (annotation-code em-dash))]
                    [(fx>= hyphens 2) (dash 2 (annotation-code en-dash))]
                    [else (dash 1 (annotation-code str))]))))))))

(define (match-special p c pos end)
  (case c
    [(#\`) (match-backtick p pos end)]
    [(#\\) (match-backslash p pos end)]
    [(#\<) (match-less-than p pos end)]
    [(#\~) (between-matched p pos end c (annotation-code open-subscript) (annotation-code close-subscript)
             (annotation-code str) always)]
    [(#\^) (between-matched p pos end c (annotation-code open-superscript) (annotation-code close-superscript)
             (annotation-code str) always)]
    [(#\_) (between-matched p pos end c (annotation-code open-emph) (annotation-code close-emph)
             (annotation-code str) always)]
    [(#\*) (between-matched p pos end c (annotation-code open-strong) (annotation-code close-strong)
             (annotation-code str) always)]
    [(#\+) (between-matched p pos end c (annotation-code open-insert) (annotation-code close-insert)
             (annotation-code str) has-brace?)]
    [(#\=) (between-matched p pos end c (annotation-code open-mark) (annotation-code close-mark)
             (annotation-code str) has-brace?)]
    [(#\') (between-matched p pos end c (annotation-code open-single-quoted) (annotation-code close-single-quoted)
             (annotation-code right-single-quote) single-quote-opens?)]
    [(#\") (between-matched p pos end c (annotation-code open-double-quoted) (annotation-code close-double-quoted)
             (annotation-code left-double-quote) always)]
    [(#\{) (match-left-brace p pos end)]
    [(#\:) (match-colon p pos end)]
    [(#\.) (match-period p pos end)]
    [(#\[) (add-opener! p (opener-slot #\[ #f) pos pos (annotation-code str)) (fx+ pos 1)]
    [(#\]) (match-right-bracket p pos end)]
    [(#\() (match-left-paren p pos end)]
    [(#\)) (match-right-paren p pos end)]
    [(#\-) (match-hyphen p pos end)]
    [else #f]))

(define (raw-attribute-end s pos end)
  (and (fx<= (fx+ pos 1) end)
       (char=? (string-ref s pos) #\{)
       (char=? (string-ref s (fx+ pos 1)) #\=)
       (let loop ([i (fx+ pos 2)])
         (and (fx<= i end)
              (let ([c (string-ref s i)])
                (cond
                  [(char=? c #\}) (and (fx> i (fx+ pos 2)) i)]
                  [(or (js-whitespace? c) (memv c '(#\{ #\`))) #f]
                  [else (loop (fx+ i 1))]))))))

(define (feed-attributes! p pos end)
  (let* ([ap (inline-parser-attribute-parser p)]
         [stop (or (find-special (inline-parser-subject p) pos end) end)])
    (let-values ([(status last) (attribute-parser-feed! ap pos stop)])
      (case status
        [(done)
         (let ([start (inline-parser-attribute-start p)] [m (attribute-parser-events ap)])
           (when start (add-match! p start start (annotation-code open-attributes)))
           (do ([i 0 (fx+ i 1)]) ((fx= i (events-count m)))
             (add-match! p (events-start m i) (events-end m i) (events-annotation m i)))
           (add-match! p last last (annotation-code close-attributes))
           (inline-parser-attribute-parser-set! p #f)
           (inline-parser-attribute-start-set! p #f)
           (events-truncate! (inline-parser-attribute-slices p) 0)
           (inline-parser-pending-span-set! p #f)
           (fx+ last 1))]
        [(fail) (reparse-attributes! p) pos]
        [else
         (add-attribute-slice! p pos last)
         (fx+ last 1)]))))

(define (feed-verbatim! p c pos end)
  (let ([s (inline-parser-subject p)])
    (cond
      [(char=? c #\`)
       (let ([last (run-end s pos end #\`)])
         (cond
           [(fx= (fx+ (fx- last pos) 1) (inline-parser-verbatim p))
            (let ([raw-end (raw-attribute-end s (fx+ last 1) end)]
                  [close (inline-parser-verbatim-close p)])
              (inline-parser-verbatim-set! p 0)
              (inline-parser-verbatim-close-set! p (annotation-code close-verbatim))
              (add-match! p pos last close)
              (cond
                [(and raw-end (fx= close (annotation-code close-verbatim)))
                 (add-match! p (fx+ last 1) raw-end (annotation-code raw-format))
                 (fx+ raw-end 1)]
                [else (fx+ last 1)]))]
           [else (add-match! p pos last (annotation-code str)) (fx+ last 1)]))]
      [else (add-match! p pos pos (annotation-code str)) (fx+ pos 1)])))

(define (inline-feed! p start end)
  (let ([s (inline-parser-subject p)])
    (when (or (fx= (inline-parser-firstpos p) -1) (fx< start (inline-parser-firstpos p)))
      (inline-parser-firstpos-set! p start))
    (when (or (fx= (inline-parser-lastpos p) 0) (fx> end (inline-parser-lastpos p)))
      (inline-parser-lastpos-set! p end))
    (let loop ([pos start])
      (when (fx<= pos end)
        (if (inline-parser-attribute-parser p)
            (loop (feed-attributes! p pos end))
            (let* ([next (or (find-special s pos end) (fx+ end 1))]
                   [pos (cond
                          [(fx> next pos) (add-match! p pos (fx- next 1) (annotation-code str)) next]
                          [else pos])])
              (when (fx<= pos end)
                (let ([c (string-ref s pos)])
                  (loop
                    (cond
                      [(char=? c #\return)
                       (cond
                         [(and (fx<= (fx+ pos 1) end) (char=? (string-ref s (fx+ pos 1)) #\newline))
                          (add-match! p pos (fx+ pos 1) (annotation-code soft-break))
                          (fx+ pos 2)]
                         [else (add-match! p pos pos (annotation-code soft-break)) (fx+ pos 1)])]
                      [(char=? c #\newline) (add-match! p pos pos (annotation-code soft-break)) (fx+ pos 1)]
                      [(fx> (inline-parser-verbatim p) 0) (feed-verbatim! p c pos end)]
                      [(match-special p c pos end)]
                      [else (add-match! p pos pos (annotation-code str)) (fx+ pos 1)]))))))))))
