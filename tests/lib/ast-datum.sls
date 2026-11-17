(library (ast-datum)
  (export datum->djot read-ast-file)
  (import (chezscheme) (djot))

  (define (datum->djot datum)
    (define (nodes data) (map datum->djot data))
    (define (entries data)
      (map (lambda (entry) (cons (car entry) (datum->djot (cdr entry)))) data))
    (let ([kind (car datum)] [base (list-head (cdr datum) 3)] [fields (list-tail datum 4)])
      (define (leaf make) (apply make (append base fields)))
      (define (container make)
        (apply make (append base (list (nodes (car fields))) (cdr fields))))
      (case kind
        [(doc)
         (apply make-djot-doc
           (append base
             (list (nodes (list-ref fields 0)) (entries (list-ref fields 1))
                   (entries (list-ref fields 2)) (entries (list-ref fields 3))
                   (list-ref fields 4))))]
        [(para) (container make-djot-para)]
        [(heading) (container make-djot-heading)]
        [(thematic-break) (leaf make-djot-thematic-break)]
        [(section) (container make-djot-section)]
        [(div) (container make-djot-div)]
        [(code-block) (leaf make-djot-code-block)]
        [(raw-block) (leaf make-djot-raw-block)]
        [(block-quote) (container make-djot-block-quote)]
        [(bullet-list) (container make-djot-bullet-list)]
        [(ordered-list) (container make-djot-ordered-list)]
        [(task-list) (container make-djot-task-list)]
        [(list-item) (container make-djot-list-item)]
        [(task-list-item) (container make-djot-task-list-item)]
        [(definition-list) (container make-djot-definition-list)]
        [(definition-list-item) (container make-djot-definition-list-item)]
        [(term) (container make-djot-term)]
        [(definition) (container make-djot-definition)]
        [(table) (container make-djot-table)]
        [(caption) (container make-djot-caption)]
        [(row) (container make-djot-row)]
        [(cell) (container make-djot-cell)]
        [(reference) (leaf make-djot-reference)]
        [(footnote) (container make-djot-footnote)]
        [(str) (leaf make-djot-str)]
        [(soft-break) (leaf make-djot-soft-break)]
        [(hard-break) (leaf make-djot-hard-break)]
        [(non-breaking-space) (leaf make-djot-non-breaking-space)]
        [(symb) (leaf make-djot-symb)]
        [(verbatim) (leaf make-djot-verbatim)]
        [(raw-inline) (leaf make-djot-raw-inline)]
        [(inline-math) (leaf make-djot-inline-math)]
        [(display-math) (leaf make-djot-display-math)]
        [(url) (leaf make-djot-url)]
        [(email) (leaf make-djot-email)]
        [(footnote-reference) (leaf make-djot-footnote-reference)]
        [(smart-punctuation) (leaf make-djot-smart-punctuation)]
        [(link) (container make-djot-link)]
        [(image) (container make-djot-image)]
        [(emph) (container make-djot-emph)]
        [(strong) (container make-djot-strong)]
        [(span) (container make-djot-span)]
        [(mark) (container make-djot-mark)]
        [(superscript) (container make-djot-superscript)]
        [(subscript) (container make-djot-subscript)]
        [(insert) (container make-djot-insert)]
        [(delete) (container make-djot-delete)]
        [(double-quoted) (container make-djot-double-quoted)]
        [(single-quoted) (container make-djot-single-quoted)]
        [else (assertion-violationf 'datum->djot "unknown node kind ~s" kind)])))

  (define (read-ast-file path)
    (datum->djot
      (call-with-port
        (open-file-input-port path (file-options) (buffer-mode block)
          (make-transcoder (utf-8-codec)))
        read))))
