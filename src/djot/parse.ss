;; Appending at the last pair spares reversing every container's children.
(define-record-type frame
  (nongenerative)
  (fields (mutable children) (mutable last) (mutable attributes) start (mutable end))
  (protocol
    (lambda (new)
      (lambda (start end)
        (new '() #f '() start end)))))

;; Most frames are inline containers, which need only the base fields.
(define-syntax define-frame
  (syntax-rules ()
    [(_ name [field initial] ...)
     (define-record-type name
       (parent frame)
       (nongenerative)
       (sealed #t)
       (fields (mutable field) ...)
       (protocol
         (lambda (pargs->new)
           (lambda (start end)
             ((pargs->new start end) initial ...)))))]))

(define-frame section-frame [level 0] [auto-id #f])
(define-frame heading-frame [level 0] [auto-id #f])
(define-frame list-frame [styles '()] [tight? #t] [blank-lines? #f] [first-marker #f])
(define-frame item-frame [definition? #f] [checkbox #f])
(define-frame table-frame [aligns '()])
(define-frame attributes-frame [key #f])
(define-frame reference-frame [key #f] [value #f])
(define-frame footnote-frame [label #f])
(define-frame code-frame [format #f] [lang #f])
(define-frame link-frame [image? #f])

;; A label table that keeps the first position of a label and its last
;; value, as JavaScript objects do.
(define-record-type label-table
  (nongenerative)
  (sealed #t)
  (fields hashtable (mutable entries))
  (protocol
    (lambda (new)
      (lambda ()
        (new (make-hashtable string-hash string=?) '())))))

(define (label-table-ref t label)
  (cond
    [(hashtable-ref (label-table-hashtable t) label #f) => cdr]
    [else #f]))

(define (label-table-set! t label value)
  (let ([entry (hashtable-ref (label-table-hashtable t) label #f)])
    (if entry
        (set-cdr! entry value)
        (let ([entry (cons label value)])
          (hashtable-set! (label-table-hashtable t) label entry)
          (label-table-entries-set! t (cons entry (label-table-entries t)))))))

(define (label-table->alist t)
  (reverse (label-table-entries t)))

(define-record-type builder
  (nongenerative)
  (sealed #t)
  (fields source subject
          (mutable frames) (mutable context) text
          references auto-references footnotes identifiers strings
          (mutable block-attributes) (mutable list-depth) (mutable last-end))
  (protocol
    (lambda (new)
      (lambda (source subject)
        (let ([frames (list (make-section-frame 0 0))])
          (new source subject frames 'normal (open-output-string)
               (make-label-table) (make-label-table) (make-label-table)
               (make-hashtable string-hash string=?) (make-hashtable string-hash string=?)
               '() 0 0))))))

;; Offsets past the source fall on the newline added to end the subject.
(define (builder-position b offset)
  (fxmin offset (string-length (builder-source b))))

;; JavaScript's substring, which djot.js takes of the source without the
;; added newline.
(define (source-bounds b from to)
  (let* ([n (string-length (builder-source b))]
         [from (fxmax 0 (fxmin from n))]
         [to (fxmax 0 (fxmin to n))])
    (if (fx<= from to) (values from to) (values to from))))

(define (source-substring b from to)
  (let-values ([(from to) (source-bounds b from to)])
    (substring (builder-source b) from to)))

;; Attribute keys and values repeat across a document, so each text is kept
;; once per parse.  Nodes share these strings and the constants below, so
;; they are immutable: changing one raises rather than changing every node
;; that holds it.  A string literal in Chez is mutable.
(define (shared-string b s)
  (let ([cell (hashtable-cell (builder-strings b) s #f)])
    (or (cdr cell)
        (let ([shared (string->immutable-string s)])
          (set-cdr! cell shared)
          shared))))

(define class-key (string->immutable-string "class"))
(define id-key (string->immutable-string "id"))
(define no-value (string->immutable-string ""))
(define single-quote-text (string->immutable-string "'"))
(define double-quote-text (string->immutable-string "\""))
(define ellipses-text (string->immutable-string "..."))
(define en-dash-text (string->immutable-string "--"))
(define em-dash-text (string->immutable-string "---"))

(define (top b) (car (builder-frames b)))

(define (under-top b)
  (let ([frames (builder-frames b)])
    (and (pair? (cdr frames)) (cadr frames))))

(define (innermost-list-frame b)
  (let ([f (top b)] [g (under-top b)])
    (cond
      [(list-frame? f) f]
      [(list-frame? g) g]
      [else #f])))

(define (add-child! b node)
  (let ([f (top b)] [pair (list node)])
    (if (frame-last f)
        (set-cdr! (frame-last f) pair)
        (frame-children-set! f pair))
    (frame-last-set! f pair)))

(define (last-child b)
  (let ([last (frame-last (top b))])
    (and last (car last))))

(define (replace-last-child! b node)
  (set-car! (frame-last (top b)) node))

;; As setting a JavaScript object's property: a key keeps its first
;; position.
(define (attributes-set attributes key value)
  (if (assoc key attributes)
      (map (lambda (entry) (if (string=? (car entry) key) (cons key value) entry)) attributes)
      (append attributes (list (cons key value)))))

(define (attributes-add-class attributes class)
  (let ([old (attribute-ref attributes "class")])
    (attributes-set attributes class-key
      (if (non-empty-string? old) (string->immutable-string (string-append old " " class)) class))))

(define (attributes-merge attributes extra)
  (if (null? attributes)
      extra
      (fold-left
        (lambda (attributes entry)
          (if (string=? (car entry) "class")
              (attributes-add-class attributes (cdr entry))
              (attributes-set attributes (car entry) (cdr entry))))
        attributes
        extra)))

(define (push-frame! b make start end)
  (let ([f (make start (fx+ end 1))])
    (unless (null? (builder-block-attributes b))
      (frame-attributes-set! f (builder-block-attributes b))
      (builder-block-attributes-set! b '()))
    (builder-frames-set! b (cons f (builder-frames b)))
    f))

(define (pop-frame! b end)
  (let ([f (top b)])
    (builder-frames-set! b (cdr (builder-frames b)))
    (frame-end-set! f (fx+ end 1))
    f))

(define (tighten-end! f)
  (let ([last (frame-last f)])
    (when (and last (djot-node-end (car last)))
      (frame-end-set! f (djot-node-end (car last))))))

(define (frame-start* b f) (builder-position b (frame-start f)))
(define (frame-end* b f) (builder-position b (frame-end f)))

(define (take-text! b)
  (get-output-string (builder-text b)))

(define (remove-line-endings s)
  (let ([n (string-length s)])
    (if (let loop ([i 0]) (and (fx< i n) (or (end-of-line-char? (string-ref s i)) (loop (fx+ i 1)))))
        (list->string (remp end-of-line-char? (string->list s)))
        s)))

(define (trim-verbatim s)
  (let* ([n (string-length s)]
         [start (if (and (fx>= n 2) (char=? (string-ref s 0) #\space) (char=? (string-ref s 1) #\`)) 1 0)]
         [end (if (and (fx>= (fx- n start) 2) (char=? (string-ref s (fx- n 1)) #\space)
                       (char=? (string-ref s (fx- n 2)) #\`))
                  (fx- n 1)
                  n)])
    (if (and (fx= start 0) (fx= end n)) s (substring s start end))))

(define (js-trim s)
  (let* ([n (string-length s)]
         [start (let loop ([i 0]) (if (and (fx< i n) (js-whitespace? (string-ref s i))) (loop (fx+ i 1)) i))]
         [end (let loop ([i n]) (if (and (fx> i start) (js-whitespace? (string-ref s (fx- i 1)))) (loop (fx- i 1)) i))])
    (if (and (fx= start 0) (fx= end n)) s (substring s start end))))

(define (unique-identifier b s)
  (let ([base (call-with-string-output-port
                (lambda (p)
                  (let ([n (string-length s)])
                    (let loop ([i 0] [pending? #f] [started? #f])
                      (when (fx< i n)
                        (let ([c (string-ref s i)])
                          (if (identifier-char? c)
                              (begin
                                (when pending? (put-char p #\-))
                                (put-char p c)
                                (loop (fx+ i 1) #f #t))
                              (loop (fx+ i 1) started? started?))))))))]
        [identifiers (builder-identifiers b)])
    (let loop ([i 0] [id base])
      (if (or (string=? id "") (hashtable-contains? identifiers id))
          (let ([i (fx+ i 1)])
            (loop i (string-append (if (string=? base "") "s" base) "-" (number->string i))))
          id))))

(define (roman->number s)
  (let loop ([i (fx- (string-length s) 1)] [total 0] [previous 0])
    (if (fx< i 0)
        total
        (let ([n (case (char-downcase (string-ref s i))
                   [(#\i) 1] [(#\v) 5] [(#\x) 10] [(#\l) 50] [(#\c) 100] [(#\d) 500] [(#\m) 1000]
                   [else 0])])
          (loop (fx- i 1) (if (fx< n previous) (- total n) (+ total n)) n)))))

(define (list-start marker style)
  (let ([s (list->string (remp (lambda (c) (memv c '(#\( #\) #\.))) (string->list marker)))])
    (case (string-ref (list->string (remp (lambda (c) (memv c '(#\( #\) #\.))) (string->list style))) 0)
      [(#\1) (string->number s 10)]
      [(#\A) (fx- (char->integer (string-ref s 0)) 64)]
      [(#\a) (fx- (char->integer (string-ref s 0)) 96)]
      [(#\I #\i) (roman->number s)]
      [else #f])))

(define (list-ref-or items i default)
  (cond
    [(null? items) default]
    [(fx= i 0) (car items)]
    [else (list-ref-or (cdr items) (fx- i 1) default)]))

(define (attach-attributes! b attributes)
  (let ([tip (last-child b)])
    (when (and tip
               (or (not (djot-str? tip))
                   (let* ([text (djot-str-text tip)]
                          [n (string-length text)]
                          [word (let loop ([i n])
                                  (if (and (fx> i 0) (not (js-whitespace? (string-ref text (fx- i 1)))))
                                      (loop (fx- i 1))
                                      i))])
                     (cond
                       [(fx= word n) #f]
                       [(fx= word 0) #t]
                       [else
                        (let* ([end (djot-node-end tip)]
                               [split (fx- end (fx- n word))])
                          (replace-last-child! b
                            (%make-djot-str (djot-node-attributes tip) (djot-node-start tip) split
                              (substring text 0 word)))
                          (add-child! b (%make-djot-str '() split end (substring text word n))))
                        #t]))))
      (let ([tip (last-child b)])
        (set-node-attributes! tip (attributes-merge (djot-node-attributes tip) attributes))))))

(define (heading-closed! b f end)
  (let* ([level (heading-frame-level f)]
         [heading-string (js-trim (nodes-string-content (frame-children f)))]
         [own-id (attribute-ref (frame-attributes f) "id")])
    (unless (non-empty-string? own-id)
      (let ([id (unique-identifier b heading-string)])
        (heading-frame-auto-id-set! f id)
        (hashtable-set! (builder-identifiers b) id #t)))
    (let ([label (normalize-label heading-string)])
      (unless (or (label-table-ref (builder-references b) label)
                  (label-table-ref (builder-auto-references b) label))
        (label-table-set! (builder-auto-references b) label
          (%make-djot-reference '() #f #f label
            (string-append "#" (if (non-empty-string? own-id) own-id (heading-frame-auto-id f)))))))
    (when (section-frame? (top b))
      (let loop ()
        (let ([p (top b)])
          (when (and (section-frame? p) (fx>= (section-frame-level p) level))
            (let ([section (pop-frame! b end)])
              (add-child! b (section-node b section))
              (loop)))))
      (let ([section (push-frame! b make-section-frame (frame-start f) (fx- (frame-end f) 1))])
        (section-frame-level-set! section level)
        (when (heading-frame-auto-id f)
          (section-frame-auto-id-set! section (heading-frame-auto-id f))
          (heading-frame-auto-id-set! f #f))
        (when (non-empty-string? own-id)
          (frame-attributes-set! section (frame-attributes f))
          (frame-attributes-set! f '()))))
    (add-child! b
      (%make-djot-heading (frame-attributes f) (frame-start* b f) (frame-end* b f)
        (frame-children f) level (heading-frame-auto-id f)))))

(define (section-node b f)
  (%make-djot-section (frame-attributes f) (frame-start* b f) (frame-end* b f)
    (frame-children f) (section-frame-auto-id f)))

(define (list-closed! b f)
  (let ([style (car (list-frame-styles f))] [tight? (list-frame-tight? f)])
    (tighten-end! f)
    (let ([attributes (frame-attributes f)] [start (frame-start* b f)] [end (frame-end* b f)]
          [children (frame-children f)])
      (add-child! b
        (cond
          [(string=? style ":") (%make-djot-definition-list attributes start end children)]
          [(char=? (string-ref style (fx- (string-length style) 1)) #\X)
           (%make-djot-task-list attributes start end children tight?)]
          [(member style '("+" "*" "-")) (%make-djot-bullet-list attributes start end children style tight?)]
          [else
           (%make-djot-ordered-list attributes start end children style tight?
             (list-start (list-frame-first-marker f) style))])))))

(define (list-item-closed! b f)
  (tighten-end! f)
  (let ([attributes (frame-attributes f)] [start (frame-start* b f)] [end (frame-end* b f)]
        [children (frame-children f)])
    (add-child! b
      (cond
        [(item-frame-definition? f)
         (let-values ([(term definitions)
                       (if (and (pair? children) (djot-para? (car children)))
                           (let ([para (car children)])
                             (values (%make-djot-term '() (djot-node-start para) (djot-node-end para)
                                       (djot-node-children para))
                                     (cdr children)))
                           (values (%make-djot-term '() start start '()) children))])
           (%make-djot-definition-list-item attributes start end
             (list term
                   (%make-djot-definition '()
                     (if (pair? definitions) (djot-node-start (car definitions)) (djot-node-end term))
                     end definitions))))]
        [(item-frame-checkbox f)
         (%make-djot-task-list-item attributes start end children (eq? (item-frame-checkbox f) 'checked))]
        [else (%make-djot-list-item attributes start end children)]))))

(define (row-closed! b f)
  (let ([table (top b)])
    (if (null? (frame-children f))
        (let ([aligns (reverse (table-frame-aligns f))] [tip (last-child b)])
          (table-frame-aligns-set! table aligns)
          (when (and tip (djot-row? tip))
            (replace-last-child! b
              (%make-djot-row (djot-node-attributes tip) (djot-node-start tip) (djot-node-end tip)
                (let loop ([cells (djot-node-children tip)] [i 0])
                  (if (null? cells)
                      '()
                      (let ([cell (car cells)])
                        (cons (%make-djot-cell (djot-node-attributes cell) (djot-node-start cell)
                                (djot-node-end cell) (djot-node-children cell) #t
                                (list-ref-or aligns i 'default))
                              (loop (cdr cells) (fx+ i 1))))))
                #t))))
        (add-child! b
          (%make-djot-row (frame-attributes f) (frame-start* b f) (frame-end* b f)
            (frame-children f) #f)))))

(define (cell-closed! b f)
  (let* ([row (top b)]
         [table (under-top b)]
         [align (list-ref-or (table-frame-aligns table) (length (frame-children row)) 'default)])
    (add-child! b
      (%make-djot-cell (frame-attributes f) (frame-start* b f) (frame-end* b f)
        (frame-children f) #f align))))

(define (caption-closed! b f)
  (let ([tip (last-child b)])
    (when (and tip (djot-table? tip))
      (replace-last-child! b
        (%make-djot-table (djot-node-attributes tip) (djot-node-start tip) (djot-node-end tip)
          (cons (%make-djot-caption (frame-attributes f) (frame-start* b f) (frame-end* b f)
                  (frame-children f))
                (cdr (djot-node-children tip))))))))

(define (raw-format-text s)
  (let* ([n (string-length s)]
         [start (cond
                  [(and (fx>= n 2) (char=? (string-ref s 0) #\{) (char=? (string-ref s 1) #\=)) 2]
                  [(and (fx>= n 1) (char=? (string-ref s 0) #\=)) 1]
                  [else 0])]
         [end (if (and (fx> n start) (char=? (string-ref s (fx- n 1)) #\})) (fx- n 1) n)])
    (substring s start end)))

;; djot.js unescapes these in attribute values.
(define (attribute-escapable? c)
  (memv c '(#\. #\, #\\ #\/ #\# #\! #\$ #\% #\^ #\& #\* #\; #\: #\{ #\} #\= #\- #\_ #\` #\~ #\+
            #\[ #\] #\( #\) #\' #\" #\? #\|)))

(define (attribute-value-space? c)
  (memv c '(#\space #\return #\newline)))

(define (attribute-value-text s start end)
  (define (plain? i)
    (or (fx= i end)
        (let ([c (string-ref s i)])
          (and (not (memv c '(#\\ #\return #\newline)))
               (not (and (char=? c #\space) (fx< (fx+ i 1) end) (char=? (string-ref s (fx+ i 1)) #\space)))
               (plain? (fx+ i 1))))))
  (if (plain? start)
      (substring s start end)
      (let ([text (make-string (fx- end start))])
        (let loop ([i start] [j 0])
          (if (fx= i end)
              (string-truncate! text j)
              (let ([c (string-ref s i)])
                (cond
                  [(attribute-value-space? c)
                   (string-set! text j #\space)
                   (let skip ([i (fx+ i 1)])
                     (if (and (fx< i end) (attribute-value-space? (string-ref s i)))
                         (skip (fx+ i 1))
                         (loop i (fx+ j 1))))]
                  [(and (char=? c #\\) (fx< (fx+ i 1) end) (attribute-escapable? (string-ref s (fx+ i 1))))
                   (string-set! text j (string-ref s (fx+ i 1)))
                   (loop (fx+ i 2) (fx+ j 1))]
                  [else (string-set! text j c) (loop (fx+ i 1) (fx+ j 1))])))))))

(define (smart-punctuation! b start end type text)
  (add-child! b
    (%make-djot-smart-punctuation '() (builder-position b start) (builder-position b (fx+ end 1))
      type text)))

(define (handle-event! b start end annotation)
  (let ([s (builder-subject b)] [context (builder-context b)])
    (define (text) (source-substring b start (fx+ end 1)))
    (define (node-start) (builder-position b start))
    (define (node-end) (builder-position b (fx+ end 1)))
    (define (push! make) (push-frame! b make start end))
    (define (pop!) (pop-frame! b end))
    (define (accumulate! str) (put-string (builder-text b) str))
    (define (accumulate-text!)
      (let-values ([(from to) (source-bounds b start (fx+ end 1))])
        (put-string (builder-text b) (builder-source b) from (fx- to from))))
    (define (inline-container! make)
      (let ([f (pop!)])
        (add-child! b (make (frame-attributes f) (frame-start* b f) (frame-end* b f) (frame-children f)))))
    (define (verbatim! context)
      (builder-context-set! b context)
      (push! make-frame))
    (define (end-verbatim! make)
      (let ([f (pop!)])
        (add-child! b (make '() (frame-start* b f) (frame-end* b f) (trim-verbatim (take-text! b))))
        (builder-context-set! b 'normal)))
    (define (end-autolink! make)
      (let ([f (pop!)])
        (add-child! b (make '() (frame-start* b f) (frame-end* b f) (remove-line-endings (take-text! b))))
        (builder-context-set! b 'normal)))
    (when (fx= annotation (annotation-code blankline))
      (builder-block-attributes-set! b '()))
    (when (and (fx> (builder-list-depth b) 0) (not (fx= annotation (annotation-code blankline))))
      (let ([list-frame (innermost-list-frame b)])
        (when list-frame
          (annotation-case annotation
            [(open-list close-list open-list-item close-list-item) (void)]
            [else
             (when (list-frame-blank-lines? list-frame)
               (list-frame-tight?-set! list-frame #f))])
          (annotation-case annotation
            [(open-list-item close-list-item) (void)]
            [else (list-frame-blank-lines?-set! list-frame #f)]))))
    (annotation-case annotation
      [(str)
       (if (eq? context 'normal)
           (add-child! b (%make-djot-str '() (node-start) (node-end) (text)))
           (accumulate-text!))]
      [(soft-break)
       (if (eq? context 'normal)
           (add-child! b (%make-djot-soft-break '() (node-start) (node-end)))
           (accumulate! "\n"))]
      [(escape)
       (when (eq? context 'verbatim) (accumulate! "\\"))]
      [(hard-break)
       (if (eq? context 'normal)
           (add-child! b (%make-djot-hard-break '() (node-start) (node-end)))
           (accumulate! "\n"))]
      [(non-breaking-space)
       (if (eq? context 'verbatim)
           (accumulate! "\\ ")
           (add-child! b (%make-djot-non-breaking-space '() (node-start) (node-end))))]
      [(symb)
       (if (eq? context 'normal)
           (add-child! b (%make-djot-symb '() (node-start) (node-end) (source-substring b (fx+ start 1) end)))
           (accumulate-text!))]
      [(footnote-reference)
       (add-child! b
         (%make-djot-footnote-reference '() (node-start) (node-end)
           (normalize-label (source-substring b (fx+ start 2) end))))]
      [(open-reference-definition) (push! make-reference-frame)]
      [(close-reference-definition)
       (let* ([f (pop!)] [key (reference-frame-key f)])
         (when (non-empty-string? key)
           (let ([label (normalize-label key)])
             (label-table-set! (builder-references b) label
               (%make-djot-reference (frame-attributes f) (frame-start* b f) (frame-end* b f)
                 label (or (reference-frame-value f) ""))))))]
      [(reference-key)
       (reference-frame-key-set! (top b) (source-substring b (fx+ start 1) end))
       (reference-frame-value-set! (top b) "")]
      [(reference-value)
       (reference-frame-value-set! (top b) (string-append (or (reference-frame-value (top b)) "") (text)))]
      [(open-emph open-strong open-span open-mark open-superscript open-subscript open-delete
        open-insert open-double-quoted open-single-quoted open-para open-block-quote open-cell
        open-caption open-div)
       (push! make-frame)]
      [(open-attributes open-block-attributes) (push! make-attributes-frame)]
      [(open-table open-row) (push! make-table-frame)]
      [(open-footnote) (push! make-footnote-frame)]
      [(close-emph) (inline-container! %make-djot-emph)]
      [(close-strong) (inline-container! %make-djot-strong)]
      [(close-span) (inline-container! %make-djot-span)]
      [(close-mark) (inline-container! %make-djot-mark)]
      [(close-superscript) (inline-container! %make-djot-superscript)]
      [(close-subscript) (inline-container! %make-djot-subscript)]
      [(close-delete) (inline-container! %make-djot-delete)]
      [(close-insert) (inline-container! %make-djot-insert)]
      [(close-double-quoted) (inline-container! %make-djot-double-quoted)]
      [(close-single-quoted) (inline-container! %make-djot-single-quoted)]
      [(close-para) (inline-container! %make-djot-para)]
      [(close-div) (inline-container! %make-djot-div)]
      [(close-attributes)
       (let* ([f (pop!)] [attributes (frame-attributes f)])
         (unless (null? attributes)
           (let ([id (attribute-ref attributes "id")])
             (when (non-empty-string? id) (hashtable-set! (builder-identifiers b) id #t)))
           (attach-attributes! b attributes)))]
      [(close-block-attributes)
       (let* ([f (pop!)] [attributes (frame-attributes f)])
         (unless (null? attributes)
           (let ([id (attribute-ref attributes "id")])
             (when (non-empty-string? id) (hashtable-set! (builder-identifiers b) id #t)))
           (builder-block-attributes-set! b
             (attributes-merge (builder-block-attributes b) attributes))))]
      [(class)
       (let ([f (top b)])
         (frame-attributes-set! f
           (attributes-add-class (frame-attributes f) (shared-string b (text)))))]
      [(id)
       (let ([f (top b)])
         (frame-attributes-set! f (attributes-set (frame-attributes f) id-key (shared-string b (text)))))]
      [(key)
       (let ([f (top b)] [key (shared-string b (text))])
         (attributes-frame-key-set! f key)
         (frame-attributes-set! f (attributes-set (frame-attributes f) key no-value)))]
      [(value)
       (let* ([f (top b)] [key (attributes-frame-key f)])
         (when (non-empty-string? key)
           (let-values ([(from to) (source-bounds b start (fx+ end 1))])
             (let ([value (attribute-value-text (builder-source b) from to)]
                   [old (attribute-ref (frame-attributes f) key)])
               (frame-attributes-set! f
                 (attributes-set (frame-attributes f) key
                   (shared-string b (if (non-empty-string? old) (string-append old value) value))))))))]
      [(open-linktext) (push! make-link-frame)]
      [(open-imagetext) (link-frame-image?-set! (push! make-link-frame) #t)]
      [(open-destination open-reference) (builder-context-set! b 'literal)]
      [(close-destination)
       (let ([f (pop!)] [destination (remove-line-endings (take-text! b))])
         (add-child! b
           ((if (link-frame-image? f) %make-djot-image %make-djot-link)
            '() (frame-start* b f) (frame-end* b f) (frame-children f) destination #f))
         (builder-context-set! b 'normal))]
      [(close-reference)
       (let* ([f (pop!)]
              [reference (let ([accumulated (take-text! b)])
                           (if (string=? accumulated "")
                               (nodes-string-content (frame-children f))
                               accumulated))])
         (add-child! b
           ((if (link-frame-image? f) %make-djot-image %make-djot-link)
            '() (frame-start* b f) (frame-end* b f) (frame-children f) #f (normalize-label reference)))
         (builder-context-set! b 'normal))]
      [(open-verbatim open-inline-math open-display-math) (verbatim! 'verbatim)]
      [(close-verbatim) (end-verbatim! %make-djot-verbatim)]
      [(close-inline-math) (end-verbatim! %make-djot-inline-math)]
      [(close-display-math) (end-verbatim! %make-djot-display-math)]
      [(raw-format)
       (let ([raw-format (raw-format-text (text))])
         (if (eq? context 'verbatim)
             (code-frame-format-set! (top b) raw-format)
             (let ([tip (last-child b)])
               (when (and tip (djot-verbatim? tip))
                 (replace-last-child! b
                   (%make-djot-raw-inline (djot-node-attributes tip) (djot-node-start tip) (djot-node-end tip)
                     raw-format (djot-verbatim-text tip)))))))]
      [(open-url open-email) (verbatim! 'literal)]
      [(close-url) (end-autolink! %make-djot-url)]
      [(close-email) (end-autolink! %make-djot-email)]
      [(open-heading)
       (heading-frame-level-set! (push! make-heading-frame) (fx+ (fx- end start) 1))]
      [(close-heading) (heading-closed! b (pop!) end)]
      [(open-list)
       (let ([f (push! make-list-frame)])
         (list-frame-styles-set! f (marker-styles s start (fx+ end 1)))
         (builder-list-depth-set! b (fx+ (builder-list-depth b) 1)))]
      [(close-list)
       (list-closed! b (pop!))
       (builder-list-depth-set! b (fx- (builder-list-depth b) 1))]
      [(open-list-item)
       (let ([styles (marker-styles s start (fx+ end 1))] [l (top b)])
         (when (fx< (length styles) (length (list-frame-styles l)))
           (list-frame-styles-set! l styles))
         (unless (list-frame-first-marker l)
           (list-frame-first-marker-set! l (text)))
         (let ([f (push! make-item-frame)])
           (when (equal? styles '(":"))
             (item-frame-definition?-set! f #t))))]
      [(close-list-item) (list-item-closed! b (pop!))]
      [(checkbox-checked) (item-frame-checkbox-set! (top b) 'checked)]
      [(checkbox-unchecked) (item-frame-checkbox-set! (top b) 'unchecked)]
      [(close-block-quote)
       (let ([f (pop!)])
         (tighten-end! f)
         (add-child! b
           (%make-djot-block-quote (frame-attributes f) (frame-start* b f) (frame-end* b f)
             (frame-children f))))]
      [(close-table)
       (let ([f (pop!)])
         (tighten-end! f)
         (add-child! b
           (%make-djot-table (frame-attributes f) (frame-start* b f) (frame-end* b f)
             (cons (%make-djot-caption '() (frame-start* b f) (frame-start* b f) '())
                   (frame-children f)))))]
      [(close-row) (row-closed! b (pop!))]
      [(separator-default) (table-frame-aligns-set! (top b) (cons 'default (table-frame-aligns (top b))))]
      [(separator-left) (table-frame-aligns-set! (top b) (cons 'left (table-frame-aligns (top b))))]
      [(separator-right) (table-frame-aligns-set! (top b) (cons 'right (table-frame-aligns (top b))))]
      [(separator-center) (table-frame-aligns-set! (top b) (cons 'center (table-frame-aligns (top b))))]
      [(close-cell) (cell-closed! b (pop!))]
      [(close-caption) (caption-closed! b (pop!))]
      [(close-footnote)
       (let ([f (pop!)])
         (tighten-end! f)
         (when (non-empty-string? (footnote-frame-label f))
           (let ([label (normalize-label (footnote-frame-label f))])
             (label-table-set! (builder-footnotes b) label
               (%make-djot-footnote (frame-attributes f) (frame-start* b f) (frame-end* b f)
                 (frame-children f) label)))))]
      [(note-label) (footnote-frame-label-set! (top b) (text))]
      [(open-code-block)
       (push! make-code-frame)
       (builder-context-set! b 'verbatim)]
      [(close-code-block)
       (let ([f (pop!)] [accumulated (take-text! b)])
         (add-child! b
           (if (non-empty-string? (code-frame-format f))
               (%make-djot-raw-block (frame-attributes f) (frame-start* b f) (frame-end* b f)
                 (code-frame-format f) accumulated)
               (%make-djot-code-block (frame-attributes f) (frame-start* b f) (frame-end* b f)
                 (code-frame-lang f) accumulated)))
         (builder-context-set! b 'normal))]
      [(code-language) (code-frame-lang-set! (top b) (text))]
      [(thematic-break)
       (let ([attributes (builder-block-attributes b)])
         (builder-block-attributes-set! b '())
         (add-child! b (%make-djot-thematic-break attributes (node-start) (node-end))))]
      [(left-single-quote) (smart-punctuation! b start end 'left-single-quote single-quote-text)]
      [(right-single-quote) (smart-punctuation! b start end 'right-single-quote single-quote-text)]
      [(left-double-quote) (smart-punctuation! b start end 'left-double-quote double-quote-text)]
      [(right-double-quote) (smart-punctuation! b start end 'right-double-quote double-quote-text)]
      [(ellipses) (smart-punctuation! b start end 'ellipses ellipses-text)]
      [(en-dash) (smart-punctuation! b start end 'en-dash en-dash-text)]
      [(em-dash) (smart-punctuation! b start end 'em-dash em-dash-text)]
      [(blankline)
       (let ([list-frame (innermost-list-frame b)])
         (when list-frame (list-frame-blank-lines?-set! list-frame #t)))]
      [else (void)])
    (builder-last-end-set! b end)))

(define (finish-document b source)
  (let loop ()
    (let ([f (top b)])
      (when (and (section-frame? f) (fx> (section-frame-level f) 0))
        (pop-frame! b (builder-last-end b))
        (add-child! b (section-node b f))
        (loop))))
  (let ([doc (top b)])
    (%make-djot-doc (frame-attributes doc) 0 (string-length source) (frame-children doc)
      (label-table->alist (builder-references b))
      (label-table->alist (builder-auto-references b))
      (label-table->alist (builder-footnotes b))
      source)))

(define (parse-document source)
  (let* ([n (string-length source)]
         [subject (if (and (fx> n 0) (char=? (string-ref source (fx- n 1)) #\newline))
                      source
                      (string-append source "\n"))]
         [b (make-builder source subject)])
    (parse-blocks subject
      (lambda (events start end)
        (do ([i start (fx+ i 1)]) ((fx= i end))
          (handle-event! b (events-start events i) (events-end events i) (events-annotation events i)))))
    (finish-document b source)))

(define (string->djot string)
  (unless (string? string)
    (assertion-violationf 'string->djot "~s is not a string" string))
  (parse-document string))

;; utf8->string drops one leading byte order mark; a second is text.
(define (bytevector->djot bytevector)
  (unless (bytevector? bytevector)
    (assertion-violationf 'bytevector->djot "~s is not a bytevector" bytevector))
  (parse-document (utf8->string bytevector)))
