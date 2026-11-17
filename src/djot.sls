(library (djot)
  (export
    ;; Parsing
    string->djot bytevector->djot
    ;; Writing HTML
    djot-write-html djot->html-string djot->html-bytevector
    ;; Documents
    djot-doc-reference djot-line-column
    ;; Every node
    djot-node? djot-node-attributes djot-node-start djot-node-end djot-node-children
    ;; Document
    make-djot-doc djot-doc? djot-doc-references djot-doc-auto-references
    djot-doc-footnotes djot-doc-source
    ;; Blocks
    make-djot-para djot-para?
    make-djot-heading djot-heading? djot-heading-level djot-heading-auto-id
    make-djot-thematic-break djot-thematic-break?
    make-djot-section djot-section? djot-section-auto-id
    make-djot-div djot-div?
    make-djot-code-block djot-code-block? djot-code-block-lang djot-code-block-text
    make-djot-raw-block djot-raw-block? djot-raw-block-format djot-raw-block-text
    make-djot-block-quote djot-block-quote?
    make-djot-bullet-list djot-bullet-list? djot-bullet-list-style djot-bullet-list-tight?
    make-djot-ordered-list djot-ordered-list? djot-ordered-list-style
    djot-ordered-list-tight? djot-ordered-list-number
    make-djot-task-list djot-task-list? djot-task-list-tight?
    make-djot-list-item djot-list-item?
    make-djot-task-list-item djot-task-list-item? djot-task-list-item-checked?
    make-djot-definition-list djot-definition-list?
    make-djot-definition-list-item djot-definition-list-item?
    make-djot-term djot-term?
    make-djot-definition djot-definition?
    make-djot-table djot-table?
    make-djot-caption djot-caption?
    make-djot-row djot-row? djot-row-head?
    make-djot-cell djot-cell? djot-cell-head? djot-cell-align
    make-djot-reference djot-reference? djot-reference-label djot-reference-destination
    make-djot-footnote djot-footnote? djot-footnote-label
    ;; Inlines
    make-djot-str djot-str? djot-str-text
    make-djot-soft-break djot-soft-break?
    make-djot-hard-break djot-hard-break?
    make-djot-non-breaking-space djot-non-breaking-space?
    make-djot-symb djot-symb? djot-symb-alias
    make-djot-verbatim djot-verbatim? djot-verbatim-text
    make-djot-raw-inline djot-raw-inline? djot-raw-inline-format djot-raw-inline-text
    make-djot-inline-math djot-inline-math? djot-inline-math-text
    make-djot-display-math djot-display-math? djot-display-math-text
    make-djot-url djot-url? djot-url-text
    make-djot-email djot-email? djot-email-text
    make-djot-footnote-reference djot-footnote-reference? djot-footnote-reference-label
    make-djot-smart-punctuation djot-smart-punctuation? djot-smart-punctuation-type
    djot-smart-punctuation-text
    make-djot-link djot-link? djot-link-destination djot-link-reference
    make-djot-image djot-image? djot-image-destination djot-image-reference
    make-djot-emph djot-emph?
    make-djot-strong djot-strong?
    make-djot-span djot-span?
    make-djot-mark djot-mark?
    make-djot-superscript djot-superscript?
    make-djot-subscript djot-subscript?
    make-djot-insert djot-insert?
    make-djot-delete djot-delete?
    make-djot-double-quoted djot-double-quoted?
    make-djot-single-quoted djot-single-quoted?)
  (import (chezscheme))

  (include "djot/ast.ss")
  (include "djot/events.ss")
  (include "djot/attributes.ss")
  (include "djot/inline.ss")
  (include "djot/block.ss")
  (include "djot/parse.ss")
  (include "djot/html.ss"))
