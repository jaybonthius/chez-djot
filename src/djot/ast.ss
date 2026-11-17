(define-record-type djot-node
  (nongenerative)
  ;; Only the parser sets attributes, on a node it has just made, when
  ;; attributes follow it in the source.
  (fields (mutable attributes djot-node-attributes set-node-attributes!) start end))

(define-record-type (container make-container container?)
  (parent djot-node)
  (nongenerative)
  (fields children))

(define (djot-node-children node)
  (cond
    [(container? node) (container-children node)]
    [(djot-node? node) '()]
    [else (assertion-violationf 'djot-node-children "~s is not a djot node" node)]))

(define-record-type (djot-doc %make-djot-doc djot-doc?)
  (parent container)
  (nongenerative)
  (sealed #t)
  (fields references auto-references footnotes source reference-table (mutable line-starts))
  (protocol
    (lambda (pargs->new)
      (lambda (attributes start end children references auto-references footnotes source)
        ((pargs->new attributes start end children)
         references auto-references footnotes source
         (make-reference-table references auto-references)
         #f)))))

(define (check-field who field value valid?)
  (unless (valid? value)
    (assertion-violationf who "invalid ~a ~s" field value)))

(define (string-pairs? x)
  (and (list? x)
       (for-all (lambda (entry) (and (pair? entry) (string? (car entry)) (string? (cdr entry)))) x)))

(define (position? x)
  (or (not x) (and (fixnum? x) (fx>= x 0))))

(define (nodes? x)
  (and (list? x) (for-all djot-node? x)))

(define (check-base who attributes start end)
  (check-field who 'attributes attributes string-pairs?)
  (check-field who 'start start position?)
  (check-field who 'end end position?))

(define (optional-string? x)
  (or (not x) (string? x)))

(define (heading-level? x)
  (and (fixnum? x) (fx> x 0)))

(define (list-number? x)
  (or (not x) (and (integer? x) (exact? x))))

(define (cell-align? x)
  (and (memq x '(default left right center)) #t))

(define (smart-punctuation-type? x)
  (and (memq x '(left-single-quote right-single-quote left-double-quote right-double-quote
                 ellipses em-dash en-dash))
       #t))

(define (labelled? valid?)
  (lambda (x)
    (and (list? x)
         (for-all (lambda (entry) (and (pair? entry) (string? (car entry)) (valid? (cdr entry)))) x))))

(define-syntax define-leaf
  (syntax-rules ()
    [(_ (name raw-make make predicate) [field valid?] ...)
     (begin
       (define-record-type (name raw-make predicate)
         (parent djot-node)
         (nongenerative)
         (sealed #t)
         (fields field ...))
       (define (make attributes start end field ...)
         (check-base 'make attributes start end)
         (check-field 'make 'field field valid?) ...
         (raw-make attributes start end field ...)))]))

(define-syntax define-container
  (syntax-rules ()
    [(_ (name raw-make make predicate) [field valid?] ...)
     (begin
       (define-record-type (name raw-make predicate)
         (parent container)
         (nongenerative)
         (sealed #t)
         (fields field ...))
       (define (make attributes start end children field ...)
         (check-base 'make attributes start end)
         (check-field 'make 'children children nodes?)
         (check-field 'make 'field field valid?) ...
         (raw-make attributes start end children field ...)))]))

(define-container (djot-para %make-djot-para make-djot-para djot-para?))
(define-container (djot-heading %make-djot-heading make-djot-heading djot-heading?)
  [level heading-level?] [auto-id optional-string?])
(define-leaf (djot-thematic-break %make-djot-thematic-break make-djot-thematic-break djot-thematic-break?))
(define-container (djot-section %make-djot-section make-djot-section djot-section?)
  [auto-id optional-string?])
(define-container (djot-div %make-djot-div make-djot-div djot-div?))
(define-leaf (djot-code-block %make-djot-code-block make-djot-code-block djot-code-block?)
  [lang optional-string?] [text string?])
(define-leaf (djot-raw-block %make-djot-raw-block make-djot-raw-block djot-raw-block?)
  [format string?] [text string?])
(define-container (djot-block-quote %make-djot-block-quote make-djot-block-quote djot-block-quote?))
(define-container (djot-bullet-list %make-djot-bullet-list make-djot-bullet-list djot-bullet-list?)
  [style string?] [tight? boolean?])
(define-container (djot-ordered-list %make-djot-ordered-list make-djot-ordered-list djot-ordered-list?)
  [style string?] [tight? boolean?] [number list-number?])
(define-container (djot-task-list %make-djot-task-list make-djot-task-list djot-task-list?)
  [tight? boolean?])
(define-container (djot-list-item %make-djot-list-item make-djot-list-item djot-list-item?))
(define-container (djot-task-list-item %make-djot-task-list-item make-djot-task-list-item djot-task-list-item?)
  [checked? boolean?])
(define-container (djot-definition-list %make-djot-definition-list make-djot-definition-list djot-definition-list?))
(define-container (djot-definition-list-item %make-djot-definition-list-item make-djot-definition-list-item djot-definition-list-item?))
(define-container (djot-term %make-djot-term make-djot-term djot-term?))
(define-container (djot-definition %make-djot-definition make-djot-definition djot-definition?))
(define-container (djot-table %make-djot-table make-djot-table djot-table?))
(define-container (djot-caption %make-djot-caption make-djot-caption djot-caption?))
(define-container (djot-row %make-djot-row make-djot-row djot-row?) [head? boolean?])
(define-container (djot-cell %make-djot-cell make-djot-cell djot-cell?)
  [head? boolean?] [align cell-align?])
(define-leaf (djot-reference %make-djot-reference make-djot-reference djot-reference?)
  [label string?] [destination string?])
(define-container (djot-footnote %make-djot-footnote make-djot-footnote djot-footnote?)
  [label string?])

(define-leaf (djot-str %make-djot-str make-djot-str djot-str?) [text string?])
(define-leaf (djot-soft-break %make-djot-soft-break make-djot-soft-break djot-soft-break?))
(define-leaf (djot-hard-break %make-djot-hard-break make-djot-hard-break djot-hard-break?))
(define-leaf (djot-non-breaking-space %make-djot-non-breaking-space make-djot-non-breaking-space djot-non-breaking-space?))
(define-leaf (djot-symb %make-djot-symb make-djot-symb djot-symb?) [alias string?])
(define-leaf (djot-verbatim %make-djot-verbatim make-djot-verbatim djot-verbatim?) [text string?])
(define-leaf (djot-raw-inline %make-djot-raw-inline make-djot-raw-inline djot-raw-inline?)
  [format string?] [text string?])
(define-leaf (djot-inline-math %make-djot-inline-math make-djot-inline-math djot-inline-math?)
  [text string?])
(define-leaf (djot-display-math %make-djot-display-math make-djot-display-math djot-display-math?)
  [text string?])
(define-leaf (djot-url %make-djot-url make-djot-url djot-url?) [text string?])
(define-leaf (djot-email %make-djot-email make-djot-email djot-email?) [text string?])
(define-leaf (djot-footnote-reference %make-djot-footnote-reference make-djot-footnote-reference djot-footnote-reference?)
  [label string?])
(define-leaf (djot-smart-punctuation %make-djot-smart-punctuation make-djot-smart-punctuation djot-smart-punctuation?)
  [type smart-punctuation-type?] [text string?])
(define-container (djot-link %make-djot-link make-djot-link djot-link?)
  [destination optional-string?] [reference optional-string?])
(define-container (djot-image %make-djot-image make-djot-image djot-image?)
  [destination optional-string?] [reference optional-string?])
(define-container (djot-emph %make-djot-emph make-djot-emph djot-emph?))
(define-container (djot-strong %make-djot-strong make-djot-strong djot-strong?))
(define-container (djot-span %make-djot-span make-djot-span djot-span?))
(define-container (djot-mark %make-djot-mark make-djot-mark djot-mark?))
(define-container (djot-superscript %make-djot-superscript make-djot-superscript djot-superscript?))
(define-container (djot-subscript %make-djot-subscript make-djot-subscript djot-subscript?))
(define-container (djot-insert %make-djot-insert make-djot-insert djot-insert?))
(define-container (djot-delete %make-djot-delete make-djot-delete djot-delete?))
(define-container (djot-double-quoted %make-djot-double-quoted make-djot-double-quoted djot-double-quoted?))
(define-container (djot-single-quoted %make-djot-single-quoted make-djot-single-quoted djot-single-quoted?))

(define (make-djot-doc attributes start end children references auto-references footnotes source)
  (check-base 'make-djot-doc attributes start end)
  (check-field 'make-djot-doc 'children children nodes?)
  (check-field 'make-djot-doc 'references references (labelled? djot-reference?))
  (check-field 'make-djot-doc 'auto-references auto-references (labelled? djot-reference?))
  (check-field 'make-djot-doc 'footnotes footnotes (labelled? djot-footnote?))
  (check-field 'make-djot-doc 'source source optional-string?)
  (%make-djot-doc attributes start end children references auto-references footnotes source))

;; JavaScript's trim, which djot.js applies to labels, removes this set;
;; char-whitespace? leaves out U+FEFF and includes U+0085.
(define (js-whitespace? c)
  (let ([n (char->integer c)])
    (if (fx< n #x80)
        (or (fx= n 32) (and (fx>= n 9) (fx<= n 13)))
        (or (fx= n #xA0) (fx= n #x1680) (and (fx>= n #x2000) (fx<= n #x200A))
            (fx= n #x2028) (fx= n #x2029) (fx= n #x202F) (fx= n #x205F)
            (fx= n #x3000) (fx= n #xFEFF)))))

(define (label-space? c)
  (case c
    [(#\space #\tab #\return #\newline) #t]
    [else #f]))

;; Links in a parsed document already hold normalised labels, so that case
;; allocates nothing.
(define (normalize-label label)
  (let* ([n (string-length label)]
         [start (let loop ([i 0])
                  (if (and (fx< i n) (js-whitespace? (string-ref label i))) (loop (fx+ i 1)) i))]
         [end (let loop ([i n])
                (if (and (fx> i start) (js-whitespace? (string-ref label (fx- i 1))))
                    (loop (fx- i 1))
                    i))])
    (define (collapsed? i)
      (or (fx= i end)
          (let ([c (string-ref label i)])
            (if (label-space? c)
                (and (char=? c #\space)
                     (not (label-space? (string-ref label (fx+ i 1))))
                     (collapsed? (fx+ i 1)))
                (collapsed? (fx+ i 1))))))
    (if (and (fx= start 0) (fx= end n) (collapsed? 0))
        label
        (call-with-string-output-port
          (lambda (p)
            (let loop ([i start])
              (when (fx< i end)
                (let ([c (string-ref label i)])
                  (cond
                    [(label-space? c)
                     (put-char p #\space)
                     (let skip ([i (fx+ i 1)])
                       (if (label-space? (string-ref label i)) (skip (fx+ i 1)) (loop i)))]
                    [else (put-char p c) (loop (fx+ i 1))])))))))))

;; As djot.js's objects: a later entry for a label replaces an earlier one,
;; and a link looks in the references before those made for headings.
(define (make-reference-table references auto-references)
  (let ([table (make-hashtable string-hash string=?)])
    (define (add! entries)
      (for-each
        (lambda (entry) (hashtable-set! table (normalize-label (car entry)) (cdr entry)))
        entries))
    (add! auto-references)
    (add! references)
    (hashtable-copy table)))

(define (djot-doc-reference doc label)
  (unless (djot-doc? doc)
    (assertion-violationf 'djot-doc-reference "~s is not a djot document" doc))
  (unless (string? label)
    (assertion-violationf 'djot-doc-reference "~s is not a string" label))
  (doc-reference doc label))

(define (doc-reference doc label)
  (hashtable-ref (djot-doc-reference-table doc) (normalize-label label) #f))

(define (line-starts source)
  (let ([n (string-length source)])
    (let count ([i 0] [lines 1])
      (if (fx= i n)
          (let ([starts (make-fxvector lines 0)])
            (let fill ([i 0] [line 1])
              (cond
                [(fx= i n) starts]
                [(char=? (string-ref source i) #\newline)
                 (fxvector-set! starts line (fx+ i 1))
                 (fill (fx+ i 1) (fx+ line 1))]
                [else (fill (fx+ i 1) line)])))
          (count (fx+ i 1) (if (char=? (string-ref source i) #\newline) (fx+ lines 1) lines))))))

;; Two threads may each build the table and store it; either result is
;; correct, and a record field set is safe across threads.
(define (djot-line-column doc offset)
  (unless (djot-doc? doc)
    (assertion-violationf 'djot-line-column "~s is not a djot document" doc))
  (let ([source (djot-doc-source doc)])
    (unless (string? source)
      (assertion-violationf 'djot-line-column "the document has no source text"))
    (unless (and (fixnum? offset) (fx<= 0 offset (string-length source)))
      (assertion-violationf 'djot-line-column "~s is not an offset into the source" offset))
    (let ([starts (or (djot-doc-line-starts doc)
                      (let ([starts (line-starts source)])
                        (djot-doc-line-starts-set! doc starts)
                        starts))])
      (let search ([low 0] [high (fx- (fxvector-length starts) 1)])
        (if (fx= low high)
            (values (fx+ low 1) (fx+ (fx- offset (fxvector-ref starts low)) 1))
            (let ([middle (fxsrl (fx+ low high 1) 1)])
              (if (fx<= (fxvector-ref starts middle) offset)
                  (search middle high)
                  (search low (fx- middle 1)))))))))
