(define (space-or-tab? c) (memv c '(#\space #\tab)))
(define (end-of-line-char? c) (memv c '(#\return #\newline)))
(define (block-whitespace? c) (memv c '(#\space #\tab #\return #\newline)))

(define (skip-spaces-and-tabs s pos)
  (if (space-or-tab? (char-at s pos)) (skip-spaces-and-tabs s (fx+ pos 1)) pos))

(define (run-of s pos c)
  (if (eqv? (char-at s pos) c) (run-of s (fx+ pos 1) c) pos))

(define (newline-at s pos)
  (case (char-at s pos)
    [(#\newline) pos]
    [(#\return) (and (eqv? (char-at s (fx+ pos 1)) #\newline) (fx+ pos 1))]
    [else #f]))

(define (line-end-from s pos)
  (newline-at s (skip-spaces-and-tabs s pos)))

(define (roman-lower? c) (and (memv c '(#\i #\v #\x #\l #\c #\d #\m)) #t))
(define (roman-upper? c) (and (memv c '(#\I #\V #\X #\L #\C #\D #\M)) #t))
(define (roman? c) (or (roman-lower? c) (roman-upper? c)))
(define (ascii-digit? c) (and (char? c) (char<=? #\0 c #\9)))
(define (ascii-lower? c) (and (char? c) (char<=? #\a c #\z)))
(define (ascii-upper? c) (and (char? c) (char<=? #\A c #\Z)))
(define (ascii-letter? c) (or (ascii-lower? c) (ascii-upper? c)))
(define (list-delimiter? c) (and (memv c '(#\. #\))) #t))

;; The alternatives are djot.js's, in its order: the first that matches wins.
(define (list-marker-end s pos)
  (let ([c (char-at s pos)])
    (define (followed? end) (and (block-whitespace? (char-at s end)) end))
    (define (run-end pos ok?) (if (ok? (char-at s pos)) (run-end (fx+ pos 1) ok?) pos))
    (define (delimited pos ok?)
      (let ([end (run-end pos ok?)])
        (and (fx> end pos) (list-delimiter? (char-at s end)) (followed? (fx+ end 1)))))
    (define (parenthesized pos ok? one?)
      (and (eqv? (char-at s pos) #\()
           (let ([end (if one?
                          (and (ok? (char-at s (fx+ pos 1))) (fx+ pos 2))
                          (let ([end (run-end (fx+ pos 1) ok?)]) (and (fx> end (fx+ pos 1)) end)))])
             (and end (eqv? (char-at s end) #\)) (followed? (fx+ end 1))))))
    (or (and (eqv? c #\:) (memv (char-at s (fx+ pos 1)) '(#\- #\* #\+ #\:)) (followed? (fx+ pos 2)))
        (and (memv c '(#\- #\* #\+ #\:)) (followed? (fx+ pos 1)))
        (parenthesized pos ascii-digit? #f)
        (delimited pos ascii-digit?)
        (delimited pos roman?)
        (parenthesized pos roman? #f)
        (and (ascii-letter? c) (list-delimiter? (char-at s (fx+ pos 1))) (followed? (fx+ pos 2)))
        (parenthesized pos ascii-letter? #t))))

(define (task-marker? s pos)
  (and (memv (char-at s pos) '(#\* #\+ #\-))
       (eqv? (char-at s (fx+ pos 1)) #\space)
       (eqv? (char-at s (fx+ pos 2)) #\[)
       (memv (char-at s (fx+ pos 3)) '(#\X #\x #\space))
       (eqv? (char-at s (fx+ pos 4)) #\])
       (block-whitespace? (char-at s (fx+ pos 5)))
       #t))

(define (marker-styles s start end)
  (if (task-marker? s start)
      (list (string (string-ref s start) #\X))
      (let* ([marker (substring s start end)]
             [n (string-length marker)])
        (if (and (fx= n 1) (memv (string-ref marker 0) '(#\+ #\- #\* #\:)))
            (list marker)
            (let* ([open? (char=? (string-ref marker 0) #\()]
                   [body-start (if open? 1 0)]
                   [body-end (fx- n 1)]
                   [delimiter (string-ref marker body-end)])
              (define (all? ok?)
                (let loop ([i body-start])
                  (or (fx= i body-end) (and (ok? (string-ref marker i)) (loop (fx+ i 1))))))
              (define (style kind)
                (string-append (if open? "(" "") kind (string delimiter)))
              (define (single? ok?) (and (fx= (fx- body-end body-start) 1) (all? ok?)))
              (cond
                [(or (fx<= body-end body-start) (not (memv delimiter '(#\) #\.)))) '()]
                [(all? ascii-digit?) (list (style "1"))]
                [(single? roman-lower?) (list (style "i") (style "a"))]
                [(single? roman-upper?) (list (style "I") (style "A"))]
                [(all? roman-lower?) (list (style "i"))]
                [(all? roman-upper?) (list (style "I"))]
                [(single? ascii-lower?) (list (style "a"))]
                [(single? ascii-upper?) (list (style "A"))]
                [else '()]))))))

(define-record-type open-block
  (nongenerative)
  (sealed #t)
  (fields kind (mutable indent) (mutable inline-parser) (mutable attribute-parser)
          (mutable opening-indent)
          (mutable level) (mutable styles) (mutable colons)
          (mutable fence-char) (mutable fence-length)
          (mutable end-fence-start) (mutable end-fence-end)
          (mutable status) (mutable start) (mutable slices))
  (protocol
    (lambda (new)
      (lambda (kind)
        (new kind 0 #f #f 0 0 '() 0 #f 0 #f #f #f 0 '())))))

(define (open-block-type c)
  (if (eq? (open-block-kind c) 'list-item) 'list-item 'block))

(define (kind-content kind)
  (case kind
    [(para heading caption) 'inline]
    [(block-quote footnote list-item fenced-div) 'block]
    [(list) 'list-item]
    [(table) 'cells]
    [(attributes) 'attributes]
    [(code-block) 'text]
    [else 'none]))

(define (open-block-content c) (kind-content (open-block-kind c)))

(define-record-type block-parser
  (nongenerative)
  (sealed #t)
  (fields subject maxoffset
          (mutable indent) (mutable startline) (mutable starteol) (mutable endeol)
          events (mutable containers) (mutable depth) (mutable pos)
          (mutable last-matched) (mutable finished-line?)
          (mutable spare-inline-parser))
  (protocol
    (lambda (new)
      (lambda (subject)
        (new subject (fx- (string-length subject) 1)
             0 0 0 0 (make-events 64) (make-vector 16) 0 0 -1 #f #f)))))

(define (add-event! bp start end annotation)
  (let ([max (block-parser-maxoffset bp)])
    (events-add! (block-parser-events bp) (fxmin start max) (fxmin end max) annotation)))

(define (tip bp)
  (let ([depth (block-parser-depth bp)])
    (and (fx> depth 0) (vector-ref (block-parser-containers bp) (fx- depth 1)))))

(define (container-at bp i)
  (and (fx>= i 0) (fx< i (block-parser-depth bp)) (vector-ref (block-parser-containers bp) i)))

(define (pop-container! bp)
  (let ([depth (fx- (block-parser-depth bp) 1)])
    (block-parser-depth-set! bp depth)
    (let ([c (vector-ref (block-parser-containers bp) depth)])
      (vector-set! (block-parser-containers bp) depth #f)
      c)))

(define (push-container! bp c)
  (let ([depth (block-parser-depth bp)] [v (block-parser-containers bp)])
    (when (fx= depth (vector-length v))
      (let ([new (make-vector (fx* 2 depth) #f)])
        (do ([i 0 (fx+ i 1)]) ((fx= i depth)) (vector-set! new i (vector-ref v i)))
        (block-parser-containers-set! bp new)))
    (vector-set! (block-parser-containers bp) depth c)
    (block-parser-depth-set! bp (fx+ depth 1))))

(define (new-inline-parser bp)
  (let ([p (block-parser-spare-inline-parser bp)])
    (cond
      [p (block-parser-spare-inline-parser-set! bp #f)
         (inline-parser-reset! p)
         p]
      [else (make-inline-parser (block-parser-subject bp))])))

(define (release-inline-parser! bp p)
  (block-parser-spare-inline-parser-set! bp p))

(define (close-unmatched-containers! bp)
  (let loop ()
    (let ([t (tip bp)])
      (when (and t (fx> (fx- (block-parser-depth bp) 1) (block-parser-last-matched bp)))
        (close-container! bp t)
        (loop)))))

(define (add-container! bp c skip-close-unmatched?)
  (unless skip-close-unmatched? (close-unmatched-containers! bp))
  (let loop ()
    (let ([t (tip bp)])
      (when (and t (not (eq? (open-block-content t) (open-block-type c))))
        (close-container! bp t)
        (loop))))
  (when (eq? (open-block-content c) 'inline)
    (open-block-inline-parser-set! c (new-inline-parser bp)))
  (push-container! bp c)
  c)

(define (skip-space! bp)
  (let ([pos (skip-spaces-and-tabs (block-parser-subject bp) (block-parser-pos bp))])
    (block-parser-indent-set! bp (fx- pos (block-parser-startline bp)))
    (block-parser-pos-set! bp pos)))

(define (get-eol! bp)
  (let* ([s (block-parser-subject bp)]
         [i (let loop ([i (block-parser-pos bp)]) (if (end-of-line-char? (string-ref s i)) i (loop (fx+ i 1))))])
    (block-parser-starteol-set! bp i)
    (block-parser-endeol-set! bp
      (if (and (char=? (string-ref s i) #\return) (eqv? (char-at s (fx+ i 1)) #\newline))
          (fx+ i 1)
          i))))

(define (add-inline-matches! bp c)
  (let ([p (open-block-inline-parser c)])
    (when p
      (let ([m (inline-parser-finish! p)] [events (block-parser-events bp)])
        (let loop ([i 0] [last-str-end #f])
          (when (fx< i (events-count m))
            (let ([start (events-start m i)] [end (events-end m i)] [annotation (events-annotation m i)])
              (let ([str? (fx= annotation (annotation-code str))])
                (if (and str? last-str-end (fx= start (fx+ last-str-end 1)))
                    (events-end-set! events (fx- (events-count events) 1) end)
                    (events-add! events start end annotation))
                (loop (fx+ i 1) (and str? end))))))
        (release-inline-parser! bp p)
        (open-block-inline-parser-set! c #f)))))

(define (close-after-last-event! bp close)
  (let* ([events (block-parser-events bp)] [n (events-count events)]
         [pos (if (fx> n 0) (fx+ (events-end events (fx- n 1)) 1) (block-parser-pos bp))])
    (add-event! bp pos pos close)))

(define (close-container! bp c)
  (let ([pos (block-parser-pos bp)])
    (case (open-block-kind c)
      [(para)
       (add-inline-matches! bp c)
       (pop-container! bp)
       (close-after-last-event! bp (annotation-code close-para))]
      [(heading)
       (add-inline-matches! bp c)
       (pop-container! bp)
       (close-after-last-event! bp (annotation-code close-heading))]
      [(caption)
       (add-inline-matches! bp c)
       (pop-container! bp)
       (add-event! bp (fx- pos 1) (fx- pos 1) (annotation-code close-caption))]
      [(block-quote) (pop-container! bp) (add-event! bp pos pos (annotation-code close-block-quote))]
      [(footnote) (pop-container! bp) (add-event! bp pos pos (annotation-code close-footnote))]
      [(reference-definition)
       (pop-container! bp)
       (add-event! bp pos pos (annotation-code close-reference-definition))]
      [(thematic-break) (pop-container! bp)]
      [(list) (pop-container! bp) (add-event! bp pos pos (annotation-code close-list))]
      [(list-item)
       (pop-container! bp)
       (add-event! bp (fx- pos 1) (fx- pos 1) (annotation-code close-list-item))]
      [(table) (pop-container! bp) (add-event! bp pos pos (annotation-code close-table))]
      [(attributes) (pop-container! bp) (close-attributes! bp c)]
      [(fenced-div code-block)
       (pop-container! bp)
       (add-event! bp (or (open-block-end-fence-start c) pos) (or (open-block-end-fence-end c) pos)
         (if (eq? (open-block-kind c) 'fenced-div) (annotation-code close-div) (annotation-code close-code-block)))])))

;; An attribute block that never closed is a paragraph's text after all.
(define (attributes->para! bp c skip-close-unmatched?)
  (let ([start (open-block-start c)])
    (add-event! bp start start (annotation-code open-para))
    (let* ([para (add-container! bp (make-open-block 'para) skip-close-unmatched?)]
           [p (open-block-inline-parser para)])
      (for-each (lambda (slice) (add-attribute-slice! p (car slice) (cdr slice)))
        (reverse (open-block-slices c)))
      (reparse-attributes! p)
      para)))

(define (close-attributes! bp c)
  (case (open-block-status c)
    [(continue) (close-container! bp (attributes->para! bp c #t))]
    [else
     (let ([start (open-block-start c)] [events (block-parser-events bp)])
       (add-event! bp start start (annotation-code open-block-attributes))
       (let ([m (attribute-parser-events (open-block-attribute-parser c))])
         (do ([i 0 (fx+ i 1)]) ((fx= i (events-count m)))
           (events-add! events (events-start m i) (events-end m i) (events-annotation m i))))
       (let ([pos (block-parser-pos bp)])
         (add-event! bp pos pos (annotation-code close-block-attributes))))]))

(define (continue-container? bp c)
  (let ([s (block-parser-subject bp)] [pos (block-parser-pos bp)])
    (case (open-block-kind c)
      [(para caption) (not (block-whitespace? (char-at s pos)))]
      [(block-quote)
       (and (eqv? (char-at s pos) #\>) (block-whitespace? (char-at s (fx+ pos 1)))
            (begin (block-parser-pos-set! bp (fx+ pos 1)) #t))]
      [(heading)
       (let ([end (run-of s pos #\#)])
         (and (fx> end pos)
              (fx= (open-block-level c) (fx- end pos))
              (block-whitespace? (char-at s end))
              (begin (block-parser-pos-set! bp end) #t)))]
      [(footnote list-item)
       (or (fx> (block-parser-indent bp) (open-block-opening-indent c))
           (fx= pos (block-parser-starteol bp)))]
      [(reference-definition)
       (and (fx< (open-block-opening-indent c) (block-parser-indent bp))
            (let ([starteol (block-parser-starteol bp)])
              (and (fx< pos starteol)
                   (let ([end (let loop ([i pos]) (if (block-whitespace? (char-at s i)) i (loop (fx+ i 1))))])
                     (and (fx> end pos) (fx= end starteol)))
                   (begin
                     (add-event! bp pos (fx- starteol 1) (annotation-code reference-value))
                     (block-parser-pos-set! bp starteol)
                     #t))))]
      [(thematic-break) #f]
      [(list)
       (or (fx> (block-parser-indent bp) (open-block-opening-indent c))
           (fx= pos (block-parser-starteol bp))
           (let ([end (list-marker-end s pos)])
             (and end
                  (let* ([styles (marker-styles s pos end)]
                         [narrowed (filter (lambda (style) (member style styles)) (open-block-styles c))])
                    (and (pair? narrowed)
                         (begin (open-block-styles-set! c narrowed) #t))))))]
      [(table)
       (let ([end (table-row-end s pos)])
         (and end (parse-table-row! bp pos end)))]
      [(attributes) (continue-attributes? bp c)]
      [(fenced-div)
       (let ([t (tip bp)])
         (or (and t (eq? (open-block-kind t) 'code-block))
             (let* ([end (run-of s pos #\:)] [colons (fx- end pos)])
               (if (and (fx>= colons 3) (line-end-from s end) (fx>= colons (open-block-colons c)))
                   (begin
                     (open-block-end-fence-start-set! c pos)
                     (open-block-end-fence-end-set! c (fx- end 1))
                     (block-parser-pos-set! bp (block-parser-starteol bp))
                     #f)
                   #t))))]
      [(code-block)
       (let* ([fence (open-block-fence-char c)]
              [end (run-of s pos fence)])
         (if (and (fx>= (fx- end pos) (open-block-fence-length c))
                  (end-of-line-char? (char-at s (skip-spaces-and-tabs s end))))
             (begin
               (open-block-end-fence-start-set! c pos)
               (open-block-end-fence-end-set! c (fx- end 1))
               (block-parser-pos-set! bp (skip-spaces-and-tabs s end))
               (block-parser-finished-line?-set! bp #t)
               #f)
             #t))])))

(define (continue-attributes? bp c)
  (let ([s (block-parser-subject bp)] [starteol (block-parser-starteol bp)])
    (cond
      [(eq? (open-block-status c) 'done) #f]
      [(and (open-block-attribute-parser c)
            (fx> (block-parser-indent bp) (open-block-opening-indent c))
            (begin
              (open-block-slices-set! c (cons (cons (block-parser-pos bp) starteol) (open-block-slices c)))
              (let-values ([(status position)
                            (attribute-parser-feed! (open-block-attribute-parser c) (block-parser-pos bp)
                              (block-parser-endeol bp))])
                (open-block-status-set! c status)
                (or (not (eq? status 'fail)) (not (line-end-from s (fx+ position 1)))))))
       (block-parser-pos-set! bp starteol)
       #t]
      [else
       (pop-container! bp)
       (let ([p (open-block-inline-parser (attributes->para! bp c #f))])
         (block-parser-pos-set! bp (fxmax (block-parser-pos bp) (fx+ (inline-parser-lastpos p) 1)))
         (not (fx= (block-parser-pos bp) starteol)))])))

(define (table-row-end s pos)
  (and (eqv? (char-at s pos) #\|)
       (let ([eol (let loop ([i pos]) (if (end-of-line-char? (char-at s i)) i (loop (fx+ i 1))))])
         (and (newline-at s eol)
              (let ([end (let loop ([i eol]) (if (space-or-tab? (char-at s (fx- i 1))) (loop (fx- i 1)) i))])
                (and (fx> (fx- end 1) pos) (char=? (string-ref s (fx- end 1)) #\|) (fx- end 1)))))))

(define (row-separator s pos)
  (let* ([left? (eqv? (char-at s pos) #\:)]
         [dashes (if left? (fx+ pos 1) pos)]
         [dashes-end (run-of s dashes #\-)])
    (if (fx> dashes-end dashes)
        (let* ([right? (eqv? (char-at s dashes-end) #\:)]
               [mark-end (if right? (fx+ dashes-end 1) dashes-end)]
               [bar (skip-spaces-and-tabs s mark-end)])
          (if (eqv? (char-at s bar) #\|)
              (values
                (cond
                  [(and left? right?) (annotation-code separator-center)]
                  [right? (annotation-code separator-right)]
                  [left? (annotation-code separator-left)]
                  [else (annotation-code separator-default)])
                (fx- mark-end 1)
                (fx- (skip-spaces-and-tabs s (fx+ bar 1)) 1))
              (values #f #f #f)))
        (values #f #f #f))))

(define (next-bar-or-ticks s pos)
  (let loop ([i pos])
    (let ([c (char-at s i)])
      (cond
        [(eqv? c #\|) i]
        [(eqv? c #\`) (fx- (run-of s i #\`) 1)]
        [(or (not c) (end-of-line-char? c)) #f]
        [else (loop (fx+ i 1))]))))

(define (parse-table-cell! bp)
  (let ([s (block-parser-subject bp)]
        [p (new-inline-parser bp)]
        [start (fx- (block-parser-pos bp) 1)])
    (skip-space! bp)
    (let loop ()
      (let ([bar (next-bar-or-ticks s (block-parser-pos bp))])
        (cond
          [(not bar) (release-inline-parser! bp p) (values #f #f #f)]
          [(or (char=? (string-ref s bar) #\`) (fx> (inline-parser-verbatim p) 0)
               (eqv? (char-at s (fx- bar 1)) #\\))
           (inline-feed! p (block-parser-pos bp) bar)
           (block-parser-pos-set! bp (fx+ bar 1))
           (loop)]
          [else
           (inline-feed! p (block-parser-pos bp) (fx- bar 1))
           (block-parser-pos-set! bp (fx+ bar 1))
           (values start bar p)])))))

(define (parse-table-row! bp start end)
  (let* ([s (block-parser-subject bp)]
         [events (block-parser-events bp)]
         [original-count (events-count events)]
         [original-pos (block-parser-pos bp)]
         [starteol (block-parser-starteol bp)])
    (define (finish-row! close-pos)
      (add-event! bp close-pos close-pos (annotation-code close-row))
      (block-parser-pos-set! bp starteol)
      (block-parser-finished-line?-set! bp #t)
      #t)
    (define (separators-added?)
      (let ([after-open-row (events-count events)])
        (let loop ([p (block-parser-pos bp)])
          (let-values ([(alignment mark-end match-end) (row-separator s p)])
            (cond
              [alignment
               (add-event! bp p mark-end alignment)
               (let ([next (fx+ match-end 1)])
                 (or (fx= next starteol) (loop next)))]
              [else (events-truncate! events after-open-row) #f])))))
    (add-event! bp start start (annotation-code open-row))
    (block-parser-pos-set! bp (fx+ (block-parser-pos bp) 1))
    (if (separators-added?)
        (finish-row! (fx- starteol 1))
        (let loop ()
          (if (fx<= (block-parser-pos bp) end)
              (let-values ([(cell-start cell-end p) (parse-table-cell! bp)])
                (cond
                  [cell-start
                   (add-event! bp cell-start cell-start (annotation-code open-cell))
                   (let* ([m (inline-parser-finish! p)] [n (events-count m)])
                     (do ([i 0 (fx+ i 1)]) ((fx= i n))
                       (let ([start (events-start m i)] [annotation (events-annotation m i)])
                         (add-event! bp start
                           (if (and (fx= i (fx- n 1)) (fx= annotation (annotation-code str)))
                               (let trim ([e (events-end m i)])
                                 (if (and (fx>= e start) (space-or-tab? (string-ref s e)))
                                     (trim (fx- e 1))
                                     e))
                               (events-end m i))
                           annotation))))
                   (release-inline-parser! bp p)
                   (add-event! bp cell-end cell-end (annotation-code close-cell))
                   (loop)]
                  [else
                   (block-parser-pos-set! bp original-pos)
                   (events-truncate! events original-count)
                   #f]))
              (finish-row! (block-parser-pos bp)))))))

(define (open-block-quote! bp)
  (let ([s (block-parser-subject bp)] [pos (block-parser-pos bp)])
    (and (block-whitespace? (char-at s (fx+ pos 1)))
         (begin
           (add-container! bp (make-open-block 'block-quote) #f)
           (add-event! bp pos pos (annotation-code open-block-quote))
           (block-parser-pos-set! bp (fx+ pos 1))
           #t))))

(define (open-heading! bp)
  (let* ([s (block-parser-subject bp)] [pos (block-parser-pos bp)] [end (run-of s pos #\#)])
    (and (block-whitespace? (char-at s end))
         (let ([c (make-open-block 'heading)])
           (open-block-level-set! c (fx- end pos))
           (add-container! bp c #f)
           (add-event! bp pos (fx- end 1) (annotation-code open-heading))
           (block-parser-pos-set! bp end)
           #t))))

(define (open-caption! bp)
  (let* ([s (block-parser-subject bp)] [pos (block-parser-pos bp)] [end (skip-spaces-and-tabs s (fx+ pos 1))])
    (and (fx> end (fx+ pos 1))
         (begin
           (block-parser-pos-set! bp end)
           (add-container! bp (make-open-block 'caption) #f)
           (add-event! bp end end (annotation-code open-caption))
           #t))))

(define (open-footnote! bp)
  (let ([s (block-parser-subject bp)] [pos (block-parser-pos bp)])
    (and (eqv? (char-at s (fx+ pos 1)) #\^)
         (let ([close (let loop ([i (fx+ pos 2)])
                        (let ([c (char-at s i)])
                          (cond
                            [(eqv? c #\]) (and (fx> i (fx+ pos 2)) i)]
                            [(or (not c) (end-of-line-char? c)) #f]
                            [else (loop (fx+ i 1))])))])
           (and close
                (eqv? (char-at s (fx+ close 1)) #\:)
                (block-whitespace? (char-at s (fx+ close 2)))
                (let ([c (make-open-block 'footnote)] [end (fx+ close 2)])
                  (open-block-opening-indent-set! c (block-parser-indent bp))
                  (add-container! bp c #f)
                  (add-event! bp pos pos (annotation-code open-footnote))
                  (add-event! bp (fx+ pos 2) (fx- end 3) (annotation-code note-label))
                  (block-parser-pos-set! bp end)
                  #t))))))

(define (open-reference-definition! bp)
  (let ([s (block-parser-subject bp)] [pos (block-parser-pos bp)] [starteol (block-parser-starteol bp)])
    (let ([close (let loop ([i (fx+ pos 1)])
                   (let ([c (char-at s i)])
                     (cond
                       [(eqv? c #\]) i]
                       [(or (not c) (end-of-line-char? c)) #f]
                       [else (loop (fx+ i 1))])))])
      (and close
           (eqv? (char-at s (fx+ close 1)) #\:)
           (let* ([after (fx+ close 2)]
                  [spaces-end (skip-spaces-and-tabs s after)]
                  [value-end (if (fx> spaces-end after)
                                 (let loop ([i spaces-end])
                                   (if (block-whitespace? (char-at s i)) i (loop (fx+ i 1))))
                                 after)])
             (and (end-of-line-char? (char-at s value-end))
                  (let ([value-start (let loop ([i after])
                                       (if (and (fx< i value-end) (js-whitespace? (string-ref s i)))
                                           (loop (fx+ i 1))
                                           i))]
                        [c (make-open-block 'reference-definition)])
                    (open-block-opening-indent-set! c (block-parser-indent bp))
                    (add-container! bp c #f)
                    (add-event! bp pos pos (annotation-code open-reference-definition))
                    (add-event! bp pos close (annotation-code reference-key))
                    (let ([value-length (fx- value-end value-start)])
                      (when (fx> value-length 0)
                        (add-event! bp (fx- starteol value-length) (fx- starteol 1)
                          (annotation-code reference-value))))
                    (block-parser-pos-set! bp (fx- starteol 1))
                    #t)))))))

(define (open-thematic-break! bp)
  (let* ([s (block-parser-subject bp)] [pos (block-parser-pos bp)])
    (let loop ([i pos] [marks 0])
      (let ([c (char-at s i)])
        (cond
          [(memv c '(#\- #\*)) (loop (fx+ i 1) (fx+ marks 1))]
          [(space-or-tab? c) (loop (fx+ i 1) marks)]
          [else
           (let ([end (newline-at s i)])
             (and end (fx>= marks 3)
                  (begin
                    (add-container! bp (make-open-block 'thematic-break) #f)
                    (add-event! bp pos end (annotation-code thematic-break))
                    (block-parser-pos-set! bp end)
                    #t)))])))))

(define (open-list! bp item?)
  (let* ([s (block-parser-subject bp)] [pos (block-parser-pos bp)] [end (list-marker-end s pos)])
    (and end
         (let ([styles (marker-styles s pos end)])
           (and (pair? styles)
                (let ([c (make-open-block (if item? 'list-item 'list))])
                  (open-block-styles-set! c styles)
                  (open-block-opening-indent-set! c (block-parser-indent bp))
                  (add-container! bp c #f)
                  (add-event! bp pos (fx- end 1)
                    (if item? (annotation-code open-list-item) (annotation-code open-list)))
                  (when item?
                    (block-parser-pos-set! bp end)
                    (when (task-marker? s pos)
                      (add-event! bp (fx+ pos 2) (fx+ pos 4)
                        (if (char=? (string-ref s (fx+ pos 3)) #\space)
                            (annotation-code checkbox-unchecked)
                            (annotation-code checkbox-checked)))
                      (block-parser-pos-set! bp (fx+ pos 5))))
                  #t))))))

(define (open-table! bp)
  (let* ([s (block-parser-subject bp)] [pos (block-parser-pos bp)] [end (table-row-end s pos)])
    (and end
         (begin
           (add-container! bp (make-open-block 'table) #f)
           (add-event! bp pos pos (annotation-code open-table))
           (or (parse-table-row! bp pos end)
               (begin
                 (events-pop! (block-parser-events bp))
                 (pop-container! bp)
                 #f))))))

(define (open-attributes! bp)
  (let* ([s (block-parser-subject bp)] [pos (block-parser-pos bp)] [starteol (block-parser-starteol bp)]
         [ap (make-attribute-parser s)])
    (let-values ([(status position) (attribute-parser-feed! ap pos starteol)])
      (and (not (eq? status 'fail))
           (not (and (eq? status 'done) (not (line-end-from s (fx+ position 1)))))
           (let ([c (make-open-block 'attributes)])
             (open-block-status-set! c status)
             (open-block-opening-indent-set! c (block-parser-indent bp))
             (open-block-start-set! c pos)
             (add-container! bp c #f)
             (open-block-attribute-parser-set! c ap)
             (open-block-slices-set! c (list (cons pos starteol)))
             (block-parser-pos-set! bp starteol)
             #t)))))

(define (class-name-char? c)
  (or (word-char? c) (eqv? c #\-)))

(define (open-fenced-div! bp)
  (let* ([s (block-parser-subject bp)] [pos (block-parser-pos bp)]
         [colons-end (run-of s pos #\:)])
    (and (fx>= (fx- colons-end pos) 3)
         (let* ([fence-end (skip-spaces-and-tabs s colons-end)]
                [class-end (let loop ([i fence-end]) (if (class-name-char? (char-at s i)) (loop (fx+ i 1)) i))]
                [newline (line-end-from s class-end)])
           (and newline
                (let ([c (make-open-block 'fenced-div)])
                  (open-block-colons-set! c (fx- colons-end pos))
                  (add-container! bp c #f)
                  (add-event! bp pos (fx- fence-end 1) (annotation-code open-div))
                  (when (fx> class-end fence-end)
                    (add-event! bp fence-end (fx- class-end 1) (annotation-code class)))
                  (block-parser-pos-set! bp (fx+ newline 1))
                  (block-parser-finished-line?-set! bp #t)
                  #t))))))

(define (open-code-block! bp)
  (let* ([s (block-parser-subject bp)] [pos (block-parser-pos bp)]
         [fence (string-ref s pos)]
         [fence-end (run-of s pos fence)])
    (and (fx>= (fx- fence-end pos) 3)
         (let* ([lang-start (skip-spaces-and-tabs s fence-end)]
                [lang-end (let loop ([i lang-start])
                            (let ([c (char-at s i)])
                              (if (or (not c) (block-whitespace? c) (char=? c #\`)) i (loop (fx+ i 1)))))]
                [newline (line-end-from s lang-end)])
           (and newline
                (let ([c (make-open-block 'code-block)])
                  (open-block-fence-char-set! c fence)
                  (open-block-fence-length-set! c (fx- fence-end pos))
                  (add-container! bp c #f)
                  (open-block-indent-set! c (block-parser-indent bp))
                  (add-event! bp pos (fx- fence-end 1) (annotation-code open-code-block))
                  (when (fx> lang-end lang-start)
                    (add-event! bp lang-start (fx- lang-end 1)
                      (if (char=? (string-ref s lang-start) #\=)
                          (annotation-code raw-format)
                          (annotation-code code-language))))
                  (block-parser-pos-set! bp newline)
                  (block-parser-finished-line?-set! bp #t)
                  #t))))))

;; djot.js tries the block kinds in this order, and the first to open wins.
(define (try-open! bp c last-match)
  (define (allowed? type)
    (if last-match (eq? (open-block-content last-match) type) (eq? type 'block)))
  (let ([block? (allowed? 'block)])
    (cond
      [(and block? (eqv? c #\>) (open-block-quote! bp)) 'block-quote]
      [(and block? (eqv? c #\#) (open-heading! bp)) 'heading]
      [(and block? (eqv? c #\^) (open-caption! bp)) 'caption]
      [(and block? (eqv? c #\[) (open-footnote! bp)) 'footnote]
      [(and block? (eqv? c #\[) (open-reference-definition! bp)) 'reference-definition]
      [(and block? (memv c '(#\- #\*)) (open-thematic-break! bp)) 'thematic-break]
      [(not (or (memv c '(#\- #\* #\+ #\: #\()) (ascii-digit? c) (ascii-letter? c)))
       (cond
         [(and block? (eqv? c #\|) (open-table! bp)) 'table]
         [(and block? (eqv? c #\{) (open-attributes! bp)) 'attributes]
         [(and block? (memv c '(#\~ #\`)) (open-code-block! bp)) 'code-block]
         [else #f])]
      [(and block? (open-list! bp #f)) 'list]
      [(and (allowed? 'list-item) (open-list! bp #t)) 'list-item]
      [(and block? (eqv? c #\:) (open-fenced-div! bp)) 'fenced-div]
      [else #f])))

(define (open-para! bp)
  (add-container! bp (make-open-block 'para) #f)
  (add-event! bp (block-parser-pos bp) (block-parser-pos bp) (annotation-code open-para)))

(define (parse-line! bp)
  (let ([s (block-parser-subject bp)])
    (block-parser-indent-set! bp 0)
    (block-parser-startline-set! bp (block-parser-pos bp))
    (block-parser-finished-line?-set! bp #f)
    (get-eol! bp)
    (block-parser-last-matched-set! bp -1)
    (let loop ([i 0])
      (when (fx< i (block-parser-depth bp))
        (skip-space! bp)
        (when (continue-container? bp (container-at bp i))
          (block-parser-last-matched-set! bp i)
          (loop (fx+ i 1)))))
    (when (block-parser-finished-line? bp)
      (let loop ()
        (when (and (fx> (block-parser-depth bp) 0)
                   (fx< (block-parser-last-matched bp) (fx- (block-parser-depth bp) 1)))
          (close-container! bp (tip bp))
          (loop))))
    (unless (block-parser-finished-line? bp)
      (skip-space! bp)
      (let ([new-starts?
             (and (not (fx= (block-parser-pos bp) (block-parser-starteol bp)))
                  (let ([last-match (container-at bp (block-parser-last-matched bp))])
                    (and (or (not last-match) (memq (open-block-content last-match) '(block list-item)))
                         (let loop ([last-match last-match] [new-starts? #f])
                           (let ([opened (try-open! bp (char-at s (block-parser-pos bp)) last-match)])
                             (cond
                               [(not opened) new-starts?]
                               [else
                                (block-parser-last-matched-set! bp (fx- (block-parser-depth bp) 1))
                                (cond
                                  [(block-parser-finished-line? bp) new-starts?]
                                  [else
                                   (skip-space! bp)
                                   (if (memq (kind-content opened) '(block list-item))
                                       (loop (tip bp) #t)
                                       #t)])]))))))])
        (unless (block-parser-finished-line? bp)
          (skip-space! bp)
          (let* ([blank? (fx= (block-parser-pos bp) (block-parser-starteol bp))]
                 [t (tip bp)]
                 [lazy? (and (not blank?) (not new-starts?)
                             (fx< (block-parser-last-matched bp) (fx- (block-parser-depth bp) 1))
                             t (eq? (open-block-content t) 'inline))])
            (unless lazy? (close-unmatched-containers! bp))
            (let ([t (let ([t (tip bp)])
                       (cond
                         [(and t (not (eq? (open-block-content t) 'block))) t]
                         [blank?
                          (unless new-starts?
                            (add-event! bp (block-parser-pos bp) (block-parser-endeol bp) (annotation-code blankline)))
                          t]
                         [else (open-para! bp) (tip bp)]))])
              (when t
                (case (open-block-content t)
                  [(text)
                   (let ([indent (block-parser-indent bp)] [pos (block-parser-pos bp)])
                     (add-event! bp
                       (if (fx> indent (open-block-indent t)) (fx- pos (fx- indent (open-block-indent t))) pos)
                       (block-parser-endeol bp)
                       (annotation-code str)))]
                  [(inline)
                   (unless blank?
                     (inline-feed! (open-block-inline-parser t) (block-parser-pos bp) (block-parser-endeol bp)))]
                  [else (void)])))))))
    (block-parser-pos-set! bp (fx+ (let ([endeol (block-parser-endeol bp)])
                           (if (fx= endeol 0) (block-parser-pos bp) endeol))
                         1))))

;; After each line the buffer keeps only the latest event, which a closing
;; paragraph still needs.
(define (parse-blocks subject consume)
  (let ([bp (make-block-parser subject)] [n (string-length subject)])
    (define (hand-over! handed)
      (let* ([events (block-parser-events bp)] [count (events-count events)])
        (if (fx> count handed)
            (let ([last (fx- count 1)])
              (consume events handed count)
              (events-set! events 0 (events-start events last) (events-end events last)
                (events-annotation events last))
              (events-truncate! events 1)
              1)
            handed)))
    (let loop ([handed 0])
      (if (fx< (block-parser-pos bp) n)
          (begin
            (parse-line! bp)
            (loop (hand-over! handed)))
          (begin
            (block-parser-last-matched-set! bp -1)
            (close-unmatched-containers! bp)
            (hand-over! handed))))))
