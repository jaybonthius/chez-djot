(define-syntax put-literal
  (lambda (x)
    (syntax-case x ()
      [(k port s)
       (string? (syntax->datum #'s))
       (let ([bv (string->utf8 (syntax->datum #'s))])
         (if (fx= (bytevector-length bv) 1)
             #`(put-u8 port #,(bytevector-u8-ref bv 0))
             #`(put-bytevector port '#,(datum->syntax #'k bv))))])))

(define (put-utf8 port n)
  (cond
    [(fx< n #x80) (put-u8 port n)]
    [(fx< n #x800)
     (put-u8 port (fxior #xC0 (fxsrl n 6)))
     (put-u8 port (fxior #x80 (fxand n #x3F)))]
    [(fx< n #x10000)
     (put-u8 port (fxior #xE0 (fxsrl n 12)))
     (put-u8 port (fxior #x80 (fxand (fxsrl n 6) #x3F)))
     (put-u8 port (fxior #x80 (fxand n #x3F)))]
    [else
     (put-u8 port (fxior #xF0 (fxsrl n 18)))
     (put-u8 port (fxior #x80 (fxand (fxsrl n 12) #x3F)))
     (put-u8 port (fxior #x80 (fxand (fxsrl n 6) #x3F)))
     (put-u8 port (fxior #x80 (fxand n #x3F)))]))

;; Every character that needs escaping is below #\?, so one comparison
;; passes most text through.
(define-syntax define-escaping-writer
  (syntax-rules ()
    [(_ name [char replacement] ...)
     (define (name port s)
       (let ([n (string-length s)])
         (let loop ([i 0])
           (when (fx< i n)
             (let ([c (char->integer (string-ref s i))])
               (cond
                 [(fx> c 62) (if (fx< c #x80) (put-u8 port c) (put-utf8 port c))]
                 [(fx= c (char->integer char)) (put-literal port replacement)]
                 ...
                 [else (put-u8 port c)])
               (loop (fx+ i 1)))))))]))

(define-escaping-writer put-text [#\& "&amp;"] [#\< "&lt;"] [#\> "&gt;"])
(define-escaping-writer put-attribute-value [#\& "&amp;"] [#\< "&lt;"] [#\> "&gt;"] [#\" "&quot;"])
(define-escaping-writer put-raw)

(define (non-empty-string? s)
  (and s (not (string=? s ""))))

(define (attribute-ref attributes key)
  (cond
    [(assoc key attributes) => cdr]
    [else #f]))

;; JavaScript objects list keys that are array indexes (canonical decimal
;; numbers below 2^32 - 1) first, in numeric order, then the other keys in
;; insertion order.  djot.js writes attributes and numbers footnotes in that
;; order, so a label such as "2" comes before "a" even when defined after it.
(define (array-index-key? key)
  (let ([n (string-length key)])
    (and (fx> n 0)
         (fx<= n 10)
         (or (fx= n 1) (not (char=? (string-ref key 0) #\0)))
         (let loop ([i 0])
           (or (fx= i n)
               (and (char<=? #\0 (string-ref key i) #\9) (loop (fx+ i 1)))))
         (< (string->number key) 4294967295))))

(define (js-key<? a b)
  (and (array-index-key? a)
       (or (not (array-index-key? b))
           (< (string->number a) (string->number b)))))

(define (js-key-order entries)
  (if (exists (lambda (entry) (array-index-key? (car entry))) entries)
      (list-sort (lambda (a b) (js-key<? (car a) (car b))) entries)
      entries))

(define (js-object-set entries key value)
  (cond
    [(assoc key entries)
     (map (lambda (entry) (if (string=? (car entry) key) (cons key value) entry)) entries)]
    [(array-index-key? key)
     (let loop ([entries entries])
       (if (or (null? entries) (js-key<? key (caar entries)))
           (cons (cons key value) entries)
           (cons (car entries) (loop (cdr entries)))))]
    [else (append entries (list (cons key value)))]))

(define (put-attribute port key value)
  (put-u8 port 32)
  (put-raw port key)
  (put-literal port "=\"")
  (put-attribute-value port value)
  (put-u8 port 34))

(define (put-attributes port attributes extras auto-id)
  (let ([skip-class?
         (let loop ([extras extras] [skip-class? #f])
           (if (null? extras)
               skip-class?
               (let ([key (caar extras)] [value (cdar extras)])
                 (cond
                   [(string=? key "class")
                    (let ([own (attribute-ref attributes "class")])
                      (put-attribute port key
                        (if (non-empty-string? own) (string-append value " " own) value)))
                    (loop (cdr extras) (or skip-class? (non-empty-string? value)))]
                   [else
                    (put-attribute port key value)
                    (loop (cdr extras) skip-class?)]))))])
    ;; As djot.js's object spread of the generated attributes and then the
    ;; node's own: the id keeps the generated one's position, after
    ;; array-index keys and before the rest.
    (let loop ([attributes (js-key-order attributes)] [id-pending? (and auto-id #t)])
      (cond
        [(null? attributes)
         (when id-pending? (put-attribute port "id" auto-id))]
        [(and id-pending? (not (array-index-key? (caar attributes))))
         (put-attribute port "id" (or (attribute-ref attributes "id") auto-id))
         (loop attributes #f)]
        [else
         (let ([key (caar attributes)])
           (unless (or (and auto-id (string=? key "id")) (and skip-class? (string=? key "class")))
             (put-attribute port key (cdar attributes))))
         (loop (cdr attributes) id-pending?)]))))

(define-syntax put-open-tag
  (lambda (x)
    (syntax-case x ()
      [(_ port name node extras auto-id)
       (string? (syntax->datum #'name))
       (with-syntax ([bare (string-append "<" (syntax->datum #'name) ">")]
                     [opening (string-append "<" (syntax->datum #'name))])
         #'(let ([p port] [attributes (djot-node-attributes node)] [e extras] [id auto-id])
             (cond
               [(and (null? attributes) (null? e) (not id)) (put-literal p bare)]
               [else
                (put-literal p opening)
                (put-attributes p attributes e id)
                (put-u8 p 62)])))])))

(define-syntax put-element
  (lambda (x)
    (syntax-case x ()
      [(_ port w tight? node name newlines extras auto-id)
       (and (string? (syntax->datum #'name)) (memv (syntax->datum #'newlines) '(0 1 2)))
       (let ([newlines (syntax->datum #'newlines)])
         (with-syntax ([closing (string-append "</" (syntax->datum #'name) ">"
                                  (if (fx> newlines 0) "\n" ""))]
                       [after-open (if (fx= newlines 2) #'(put-u8 p 10) #'(void))])
           #'(let ([p port] [n node])
               (put-open-tag p name n extras auto-id)
               after-open
               (put-children p w tight? n)
               (put-literal p closing))))])))

(define-record-type writer
  (nongenerative)
  (sealed #t)
  (fields doc footnote-numbers (mutable next-footnote) (mutable footnote-labels)))

(define (put-children port w tight? node)
  (let loop ([children (container-children node)])
    (unless (null? children)
      (put-node port w tight? (car children))
      (loop (cdr children)))))

(define (put-number port n)
  (put-raw port (number->string n)))

(define (node-text node)
  (cond
    [(djot-str? node) (djot-str-text node)]
    [(djot-verbatim? node) (djot-verbatim-text node)]
    [(djot-smart-punctuation? node) (djot-smart-punctuation-text node)]
    [(djot-raw-inline? node) (djot-raw-inline-text node)]
    [(djot-inline-math? node) (djot-inline-math-text node)]
    [(djot-display-math? node) (djot-display-math-text node)]
    [(djot-url? node) (djot-url-text node)]
    [(djot-email? node) (djot-email-text node)]
    [(djot-code-block? node) (djot-code-block-text node)]
    [(djot-raw-block? node) (djot-raw-block-text node)]
    [else #f]))

(define (nodes-string-content nodes)
  (call-with-string-output-port
    (lambda (p)
      (let walk ([nodes nodes])
        (for-each
          (lambda (node)
            (cond
              [(djot-footnote-reference? node) (void)]
              [(node-text node) => (lambda (text) (put-string p text))]
              [(or (djot-soft-break? node) (djot-hard-break? node)) (put-char p #\newline)]
              [else (walk (djot-node-children node))]))
          nodes)))))

(define (string-content node)
  (nodes-string-content (list node)))

;; djot.js ignores a link's destination when it names a reference, and
;; writes nothing for a reference it cannot find, not even an image's alt.
(define (link-extras w node image? destination label)
  (cond
    [(non-empty-string? label)
     (let ([reference (doc-reference (writer-doc w) label)])
       (if reference
           (let ([destination (djot-reference-destination reference)]
                 [own (djot-node-attributes node)])
             (fold-left
               (lambda (extras entry)
                 (if (non-empty-string? (attribute-ref own (car entry)))
                     extras
                     (js-object-set extras (car entry) (cdr entry))))
               (if image?
                   (list (cons "alt" (string-content node)) (cons "src" destination))
                   (list (cons "href" destination)))
               (djot-node-attributes reference)))
           '()))]
    [image?
     (cons (cons "alt" (string-content node))
           (if destination (list (cons "src" destination)) '()))]
    [destination (list (cons "href" destination))]
    [else '()]))

(define (ordered-list-extras node)
  (let ([number (djot-ordered-list-number node)]
        [style (djot-ordered-list-style node)])
    (append
      (if (and number (not (eqv? number 0)) (not (eqv? number 1)))
          (list (cons "start" (number->string number)))
          '())
      (let ([chars (string->list style)])
        (if (and (pair? chars) (not (memv #\1 chars)))
            (list (cons "type" (list->string (remp (lambda (c) (memv c '(#\( #\) #\.))) chars))))
            '())))))

(define (cell-extras node)
  (let ([align (djot-cell-align node)])
    (if (and align (not (eq? align 'default)))
        (list (cons "style" (string-append "text-align: " (symbol->string align) ";")))
        '())))

(define (footnote-number! w label)
  (let ([table (writer-footnote-numbers w)])
    (cond
      [(hashtable-ref table label #f) => (lambda (n) (values n #f))]
      [else
       (let ([n (writer-next-footnote w)])
         (hashtable-set! table label n)
         (writer-next-footnote-set! w (fx+ n 1))
         (writer-footnote-labels-set! w (cons label (writer-footnote-labels w)))
         (values n #t))])))

(define (put-footnote-reference port w node)
  (let-values ([(n first?) (footnote-number! w (djot-footnote-reference-label node))])
    (let ([s (number->string n)])
      (put-open-tag port "a" node
        (let ([rest (list (cons "href" (string-append "#fn" s)) (cons "role" "doc-noteref"))])
          (if first? (cons (cons "id" (string-append "fnref" s)) rest) rest))
        #f)
      (put-literal port "<sup>")
      (put-raw port s)
      (put-literal port "</sup></a>"))))

(define (put-heading port w tight? node)
  (let ([auto-id (djot-heading-auto-id node)])
    (case (djot-heading-level node)
      [(1) (put-element port w tight? node "h1" 1 '() auto-id)]
      [(2) (put-element port w tight? node "h2" 1 '() auto-id)]
      [(3) (put-element port w tight? node "h3" 1 '() auto-id)]
      [(4) (put-element port w tight? node "h4" 1 '() auto-id)]
      [(5) (put-element port w tight? node "h5" 1 '() auto-id)]
      [(6) (put-element port w tight? node "h6" 1 '() auto-id)]
      [else
       (let ([level (number->string (djot-heading-level node))]
             [attributes (djot-node-attributes node)])
         (put-literal port "<h")
         (put-raw port level)
         (put-attributes port attributes '() auto-id)
         (put-u8 port 62)
         (put-children port w tight? node)
         (put-literal port "</h")
         (put-raw port level)
         (put-literal port ">\n"))])))

(define (put-code-block port node)
  (put-open-tag port "pre" node '() #f)
  (let ([lang (djot-code-block-lang node)])
    (cond
      [(non-empty-string? lang)
       (put-literal port "<code class=\"language-")
       (put-attribute-value port lang)
       (put-literal port "\">")]
      [else (put-literal port "<code>")]))
  (put-text port (djot-code-block-text node))
  (put-literal port "</code></pre>\n"))

(define (put-smart-punctuation port node)
  (case (djot-smart-punctuation-type node)
    [(left-single-quote) (put-literal port "\x2018;")]
    [(right-single-quote) (put-literal port "\x2019;")]
    [(left-double-quote) (put-literal port "\x201C;")]
    [(right-double-quote) (put-literal port "\x201D;")]
    [(ellipses) (put-literal port "\x2026;")]
    [(em-dash) (put-literal port "\x2014;")]
    [(en-dash) (put-literal port "\x2013;")]
    [else (put-raw port (djot-smart-punctuation-text node))]))

(define (put-node port w tight? node)
  (cond
    [(djot-str? node)
     (cond
       [(null? (djot-node-attributes node)) (put-text port (djot-str-text node))]
       [else
        (put-open-tag port "span" node '() #f)
        (put-text port (djot-str-text node))
        (put-literal port "</span>")])]
    [(djot-soft-break? node) (put-u8 port 10)]
    [(djot-emph? node) (put-element port w tight? node "em" 0 '() #f)]
    [(djot-strong? node) (put-element port w tight? node "strong" 0 '() #f)]
    [(djot-para? node)
     (cond
       [tight? (put-children port w tight? node) (put-u8 port 10)]
       [else (put-element port w tight? node "p" 1 '() #f)])]
    [(djot-link? node)
     (put-element port w tight? node "a" 0
       (link-extras w node #f (djot-link-destination node) (djot-link-reference node))
       #f)]
    [(djot-verbatim? node)
     (put-open-tag port "code" node '() #f)
     (put-text port (djot-verbatim-text node))
     (put-literal port "</code>")]
    [(djot-smart-punctuation? node) (put-smart-punctuation port node)]
    [(djot-section? node) (put-element port w tight? node "section" 2 '() (djot-section-auto-id node))]
    [(djot-heading? node) (put-heading port w tight? node)]
    [(djot-list-item? node) (put-element port w tight? node "li" 2 '() #f)]
    [(djot-span? node) (put-element port w tight? node "span" 0 '() #f)]
    [(djot-double-quoted? node)
     (put-literal port "\x201C;")
     (put-children port w tight? node)
     (put-literal port "\x201D;")]
    [(djot-single-quoted? node)
     (put-literal port "\x2018;")
     (put-children port w tight? node)
     (put-literal port "\x2019;")]
    [(djot-code-block? node) (put-code-block port node)]
    [(djot-footnote-reference? node) (put-footnote-reference port w node)]
    [(djot-bullet-list? node)
     (put-element port w (djot-bullet-list-tight? node) node "ul" 2 '() #f)]
    [(djot-ordered-list? node)
     (put-element port w (djot-ordered-list-tight? node) node "ol" 2 (ordered-list-extras node) #f)]
    [(djot-task-list? node)
     (put-element port w (djot-task-list-tight? node) node "ul" 2 '(("class" . "task-list")) #f)]
    [(djot-task-list-item? node)
     (if (djot-task-list-item-checked? node)
         (put-literal port "<li>\n<input disabled=\"\" type=\"checkbox\" checked=\"\"/>\n")
         (put-literal port "<li>\n<input disabled=\"\" type=\"checkbox\"/>\n"))
     (put-children port w tight? node)
     (put-literal port "</li>\n")]
    [(djot-block-quote? node) (put-element port w #f node "blockquote" 2 '() #f)]
    [(djot-div? node) (put-element port w #f node "div" 2 '() #f)]
    [(djot-image? node)
     (put-open-tag port "img" node
       (link-extras w node #t (djot-image-destination node) (djot-image-reference node))
       #f)]
    [(djot-url? node)
     (put-open-tag port "a" node (list (cons "href" (djot-url-text node))) #f)
     (put-text port (djot-url-text node))
     (put-literal port "</a>")]
    [(djot-email? node)
     (put-open-tag port "a" node (list (cons "href" (string-append "mailto:" (djot-email-text node)))) #f)
     (put-text port (djot-email-text node))
     (put-literal port "</a>")]
    [(djot-mark? node) (put-element port w tight? node "mark" 0 '() #f)]
    [(djot-superscript? node) (put-element port w tight? node "sup" 0 '() #f)]
    [(djot-subscript? node) (put-element port w tight? node "sub" 0 '() #f)]
    [(djot-insert? node) (put-element port w tight? node "ins" 0 '() #f)]
    [(djot-delete? node) (put-element port w tight? node "del" 0 '() #f)]
    [(djot-hard-break? node) (put-literal port "<br>\n")]
    [(djot-non-breaking-space? node) (put-literal port "&nbsp;")]
    [(djot-symb? node)
     (put-u8 port 58)
     (put-text port (djot-symb-alias node))
     (put-u8 port 58)]
    [(djot-inline-math? node)
     (put-open-tag port "span" node '(("class" . "math inline")) #f)
     (put-literal port "\\(")
     (put-text port (djot-inline-math-text node))
     (put-literal port "\\)</span>")]
    [(djot-display-math? node)
     (put-open-tag port "span" node '(("class" . "math display")) #f)
     (put-literal port "\\[")
     (put-text port (djot-display-math-text node))
     (put-literal port "\\]</span>")]
    [(djot-raw-inline? node)
     (when (equal? (djot-raw-inline-format node) "html")
       (put-raw port (djot-raw-inline-text node)))]
    [(djot-raw-block? node)
     (when (equal? (djot-raw-block-format node) "html")
       (put-raw port (djot-raw-block-text node)))]
    [(djot-thematic-break? node)
     (put-open-tag port "hr" node '() #f)
     (put-u8 port 10)]
    [(djot-definition-list? node) (put-element port w tight? node "dl" 2 '() #f)]
    [(djot-definition-list-item? node) (put-children port w tight? node)]
    [(djot-term? node) (put-element port w tight? node "dt" 1 '() #f)]
    [(djot-definition? node) (put-element port w tight? node "dd" 2 '() #f)]
    [(djot-table? node) (put-element port w tight? node "table" 2 '() #f)]
    [(djot-caption? node)
     (unless (null? (container-children node))
       (put-element port w tight? node "caption" 1 '() #f))]
    [(djot-row? node) (put-element port w tight? node "tr" 2 '() #f)]
    [(djot-cell? node)
     (if (djot-cell-head? node)
         (put-element port w tight? node "th" 1 (cell-extras node) #f)
         (put-element port w tight? node "td" 1 (cell-extras node) #f))]
    [(djot-doc? node) (put-children port w tight? node)]
    [(or (djot-reference? node) (djot-footnote? node)) (void)]
    [else (assertion-violationf 'djot-write-html "~s is not a djot node" node)]))

(define (put-footnote port bv n)
  (let* ([size (bytevector-length bv)]
         [end (let loop ([end size])
                (if (and (fx> end 0) (memv (bytevector-u8-ref bv (fx- end 1)) '(10 13)))
                    (loop (fx- end 1))
                    end))]
         [paragraph-end (fx- end 4)])
    (define (put-backlink)
      (put-literal port "<a href=\"#fnref")
      (put-number port n)
      (put-literal port "\" role=\"doc-backlink\">\x21A9;\xFE0E;</a>"))
    (cond
      [(and (fx>= paragraph-end 0)
            (fx= (bytevector-u8-ref bv paragraph-end) 60)
            (fx= (bytevector-u8-ref bv (fx+ paragraph-end 1)) 47)
            (fx= (bytevector-u8-ref bv (fx+ paragraph-end 2)) 112)
            (fx= (bytevector-u8-ref bv (fx+ paragraph-end 3)) 62))
       (put-bytevector port bv 0 paragraph-end)
       (put-backlink)
       (put-bytevector port bv paragraph-end (fx- size paragraph-end))]
      [else
       (put-bytevector port bv)
       (put-literal port "<p>")
       (put-backlink)
       (put-literal port "</p>\n")])))

;; As djot.js's objects: a later entry for a label replaces an earlier one
;; and keeps the earlier one's position.
(define (unique-entries entries)
  (let ([latest (make-hashtable string-hash string=?)])
    (for-each (lambda (entry) (hashtable-set! latest (car entry) (cdr entry))) entries)
    (let loop ([entries entries] [unique '()])
      (if (null? entries)
          (reverse unique)
          (let ([label (caar entries)])
            (if (hashtable-contains? latest label)
                (let ([node (hashtable-ref latest label #f)])
                  (hashtable-delete! latest label)
                  (loop (cdr entries) (cons (cons label node) unique)))
                (loop (cdr entries) unique)))))))

;; Every note is written before any is put out, in djot.js's order, because
;; a note can number notes that later ones refer to.
(define (put-footnotes port w)
  (let ([rendered (make-hashtable string-hash string=?)])
    (for-each
      (lambda (entry)
        (let-values ([(p extract) (open-bytevector-output-port)])
          (put-children p w #f (cdr entry))
          (hashtable-set! rendered (car entry) (extract))))
      (js-key-order (unique-entries (djot-doc-footnotes (writer-doc w)))))
    (put-literal port "<section role=\"doc-endnotes\">\n<hr>\n<ol>\n")
    (let loop ([n 1] [labels (reverse (writer-footnote-labels w))])
      (unless (null? labels)
        (put-literal port "<li id=\"fn")
        (put-number port n)
        (put-literal port "\">\n")
        (put-footnote port (hashtable-ref rendered (car labels) '#vu8()) n)
        (put-literal port "</li>\n")
        (loop (fx+ n 1) (cdr labels))))
    (put-literal port "</ol>\n</section>\n")))

(define (put-document port doc)
  (let ([w (make-writer doc (make-hashtable string-hash string=?) 1 '())])
    (put-children port w #f doc)
    (unless (null? (writer-footnote-labels w))
      (put-footnotes port w))))

(define (document->bytevector doc)
  (let-values ([(port extract) (open-bytevector-output-port)])
    (put-document port doc)
    (extract)))

;; utf8->string drops one leading byte order mark, which is put back:
;; HTML that begins with U+FEFF begins with it as text.
(define (html-bytes->string bytes)
  (if (and (fx>= (bytevector-length bytes) 3)
           (fx= (bytevector-u8-ref bytes 0) #xEF)
           (fx= (bytevector-u8-ref bytes 1) #xBB)
           (fx= (bytevector-u8-ref bytes 2) #xBF))
      (string-append "\xFEFF;" (utf8->string bytes))
      (utf8->string bytes)))

(define (check-document who doc)
  (unless (djot-doc? doc)
    (assertion-violationf who "~s is not a djot document" doc)))

(define (djot-write-html doc port)
  (check-document 'djot-write-html doc)
  (unless (output-port? port)
    (assertion-violationf 'djot-write-html "~s is not an output port" port))
  (if (binary-port? port)
      (put-document port doc)
      (put-string port (html-bytes->string (document->bytevector doc)))))

(define (djot->html-bytevector doc)
  (check-document 'djot->html-bytevector doc)
  (document->bytevector doc))

(define (djot->html-string doc)
  (check-document 'djot->html-string doc)
  (html-bytes->string (document->bytevector doc)))
