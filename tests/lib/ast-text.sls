(library (ast-text)
  (export djot->ast-text)
  (import (chezscheme) (djot))

  (define (put-json-string p s)
    (put-char p #\")
    (string-for-each
      (lambda (c)
        (case c
          [(#\") (put-string p "\\\"")]
          [(#\\) (put-string p "\\\\")]
          [(#\newline) (put-string p "\\n")]
          [(#\return) (put-string p "\\r")]
          [(#\tab) (put-string p "\\t")]
          [(#\backspace) (put-string p "\\b")]
          [(#\page) (put-string p "\\f")]
          [else
           (if (char<? c #\space)
               (put-string p (format "\\u~4,'0x" (char->integer c)))
               (put-char p c))]))
      s)
    (put-char p #\"))

  (define (put-json p x)
    (cond
      [(string? x) (put-json-string p x)]
      [(boolean? x) (put-string p (if x "true" "false"))]
      [else (display x p)]))

  ;; JavaScript objects list array-index keys first, in numeric order.
  (define (index-key? key)
    (and (fx> (string-length key) 0)
         (for-all char-numeric? (string->list key))
         (or (fx= (string-length key) 1) (not (char=? (string-ref key 0) #\0)))
         (< (string->number key) 4294967295)))

  (define (js-order entries)
    (append
      (list-sort (lambda (a b) (< (string->number (car a)) (string->number (car b))))
        (filter (lambda (entry) (index-key? (car entry))) entries))
      (remp (lambda (entry) (index-key? (car entry))) entries)))

  (define (unique-entries entries)
    (let loop ([entries entries] [unique '()])
      (cond
        [(null? entries) (reverse unique)]
        [(assoc (caar entries) unique)
         (loop (cdr entries)
           (map (lambda (entry) (if (string=? (car entry) (caar entries)) (car entries) entry)) unique))]
        [else (loop (cdr entries) (cons (car entries) unique))])))

  (define (tag-and-fields node)
    (cond
      [(djot-doc? node) (values "doc" '())]
      [(djot-str? node) (values "str" (list (cons "text" (djot-str-text node))))]
      [(djot-soft-break? node) (values "soft_break" '())]
      [(djot-hard-break? node) (values "hard_break" '())]
      [(djot-non-breaking-space? node) (values "non_breaking_space" '())]
      [(djot-symb? node) (values "symb" (list (cons "alias" (djot-symb-alias node))))]
      [(djot-footnote-reference? node)
       (values "footnote_reference" (list (cons "text" (djot-footnote-reference-label node))))]
      [(djot-emph? node) (values "emph" '())]
      [(djot-strong? node) (values "strong" '())]
      [(djot-span? node) (values "span" '())]
      [(djot-mark? node) (values "mark" '())]
      [(djot-superscript? node) (values "superscript" '())]
      [(djot-subscript? node) (values "subscript" '())]
      [(djot-delete? node) (values "delete" '())]
      [(djot-insert? node) (values "insert" '())]
      [(djot-double-quoted? node) (values "double_quoted" '())]
      [(djot-single-quoted? node) (values "single_quoted" '())]
      [(djot-link? node)
       (values "link" (if (djot-link-destination node)
                          (list (cons "destination" (djot-link-destination node)))
                          (list (cons "reference" (djot-link-reference node)))))]
      [(djot-image? node)
       (values "image" (if (djot-image-destination node)
                           (list (cons "destination" (djot-image-destination node)))
                           (list (cons "reference" (djot-image-reference node)))))]
      [(djot-verbatim? node) (values "verbatim" (list (cons "text" (djot-verbatim-text node))))]
      [(djot-raw-inline? node)
       (values "raw_inline" (list (cons "text" (djot-raw-inline-text node))
                                  (cons "format" (djot-raw-inline-format node))))]
      [(djot-inline-math? node) (values "inline_math" (list (cons "text" (djot-inline-math-text node))))]
      [(djot-display-math? node) (values "display_math" (list (cons "text" (djot-display-math-text node))))]
      [(djot-url? node) (values "url" (list (cons "text" (djot-url-text node))))]
      [(djot-email? node) (values "email" (list (cons "text" (djot-email-text node))))]
      [(djot-smart-punctuation? node)
       (values "smart_punctuation"
         (list (cons "type" (list->string
                              (map (lambda (c) (if (char=? c #\-) #\_ c))
                                   (string->list (symbol->string (djot-smart-punctuation-type node))))))
               (cons "text" (djot-smart-punctuation-text node))))]
      [(djot-para? node) (values "para" '())]
      [(djot-heading? node) (values "heading" (list (cons "level" (djot-heading-level node))))]
      [(djot-section? node) (values "section" '())]
      [(djot-reference? node)
       (values "reference" (list (cons "label" (djot-reference-label node))
                                 (cons "destination" (djot-reference-destination node))))]
      [(djot-bullet-list? node)
       (values "bullet_list" (list (cons "tight" (djot-bullet-list-tight? node))
                                   (cons "style" (djot-bullet-list-style node))))]
      [(djot-ordered-list? node)
       (values "ordered_list"
         (append (list (cons "style" (djot-ordered-list-style node)))
                 (if (djot-ordered-list-number node)
                     (list (cons "start" (djot-ordered-list-number node)))
                     '())
                 (list (cons "tight" (djot-ordered-list-tight? node)))))]
      [(djot-task-list? node) (values "task_list" (list (cons "tight" (djot-task-list-tight? node))))]
      [(djot-definition-list? node) (values "definition_list" '())]
      [(djot-list-item? node) (values "list_item" '())]
      [(djot-task-list-item? node)
       (values "task_list_item"
         (list (cons "checkbox" (if (djot-task-list-item-checked? node) "checked" "unchecked"))))]
      [(djot-definition-list-item? node) (values "definition_list_item" '())]
      [(djot-term? node) (values "term" '())]
      [(djot-definition? node) (values "definition" '())]
      [(djot-block-quote? node) (values "block_quote" '())]
      [(djot-table? node) (values "table" '())]
      [(djot-row? node) (values "row" (list (cons "head" (djot-row-head? node))))]
      [(djot-cell? node)
       (values "cell" (list (cons "head" (djot-cell-head? node))
                            (cons "align" (symbol->string (djot-cell-align node)))))]
      [(djot-caption? node) (values "caption" '())]
      [(djot-footnote? node) (values "footnote" (list (cons "label" (djot-footnote-label node))))]
      [(djot-raw-block? node)
       (values "raw_block" (list (cons "format" (djot-raw-block-format node))
                                 (cons "text" (djot-raw-block-text node))))]
      [(djot-code-block? node)
       (values "code_block"
         (cons (cons "text" (djot-code-block-text node))
               (if (djot-code-block-lang node) (list (cons "lang" (djot-code-block-lang node))) '())))]
      [(djot-div? node) (values "div" '())]
      [(djot-thematic-break? node) (values "thematic_break" '())]
      [else (assertion-violationf 'djot->ast-text "unknown node ~s" node)]))

  ;; djot.js gives no position to these.
  (define (positioned? node)
    (not (or (djot-doc? node) (djot-term? node) (djot-definition? node)
             (and (djot-caption? node) (djot-node-start node)
                  (fx= (djot-node-start node) (djot-node-end node)))
             (not (djot-node-start node)))))

  ;; djot.js's line and column: a newline begins the next line, at column 0.
  (define (put-location p source offset)
    (let loop ([i 0] [line 1] [line-start -1])
      (if (and (fx< i (string-length source)) (fx<= i offset))
          (if (char=? (string-ref source i) #\newline)
              (loop (fx+ i 1) (fx+ line 1) i)
              (loop (fx+ i 1) line line-start))
          (format p "~a:~a:~a" line (fx- offset line-start) offset))))

  (define (put-node p source positions? node indent)
    (put-string p (make-string indent #\space))
    (if (> indent 128)
        (put-string p "(((DEEPLY NESTED CONTENT OMITTED)))\n")
        (let-values ([(tag fields) (tag-and-fields node)])
          (put-string p tag)
          (when (and positions? (positioned? node))
            (put-string p " (")
            (put-location p source (djot-node-start node))
            (put-char p #\-)
            (put-location p source (fx- (djot-node-end node) 1))
            (put-char p #\)))
          (for-each
            (lambda (field)
              (format p " ~a=" (car field))
              (put-json p (cdr field)))
            fields)
          (for-each
            (lambda (attribute)
              (format p " ~a=" (car attribute))
              (put-json p (cdr attribute)))
            (js-order (djot-node-attributes node)))
          (newline p)
          (for-each (lambda (child) (put-node p source positions? child (+ indent 2)))
            (djot-node-children node)))))

  (define (djot->ast-text doc positions?)
    (let ([source (djot-doc-source doc)])
      (call-with-string-output-port
        (lambda (p)
          (put-node p source positions? doc 0)
          (for-each
            (lambda (section entries)
              (let ([entries (js-order (unique-entries entries))])
                (unless (null? entries)
                  (format p "~a\n" section)
                  (for-each
                    (lambda (entry)
                      (put-string p "  [")
                      (put-json-string p (car entry))
                      (put-string p "] =\n")
                      (put-node p source positions? (cdr entry) 4))
                    entries))))
            '("references" "footnotes")
            (list (djot-doc-references doc) (djot-doc-footnotes doc))))))))
