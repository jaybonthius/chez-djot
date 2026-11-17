(import (chezscheme) (djot) (check))

(define (html s) (djot->html-string (string->djot s)))

(test "string->djot keeps the source and parses text without a final newline"
  (let ([doc (string->djot "Some _text_")])
    (check-equal? "Some _text_" (djot-doc-source doc))
    (check-equal? "<p>Some <em>text</em></p>\n" (djot->html-string doc))
    (check-equal? '(0 11) (list (djot-node-start doc) (djot-node-end doc)))
    (let ([para (car (djot-node-children doc))])
      (check-equal? '(0 11) (list (djot-node-start para) (djot-node-end para)))))
  (check-equal? "" (html ""))
  (check-equal? "<pre><code>x</code></pre>\n" (html "```\nx")))

(test "a carriage return before a line feed is dropped"
  (check-equal? (html "# A\n\n- b\n- c\n\n> d\n") (html "# A\r\n\r\n- b\r\n- c\r\n\r\n> d\r\n")))

(test "bytevector->djot skips a byte order mark; string->djot keeps it as text"
  (let ([doc (bytevector->djot (string->utf8 "\xFEFF;# Hi\n"))])
    (check-equal? "# Hi\n" (djot-doc-source doc))
    (check-equal? "<section id=\"Hi\">\n<h1>Hi</h1>\n</section>\n" (djot->html-string doc)))
  (check-equal? "<p>\xFEFF;# Hi</p>\n" (html "\xFEFF;# Hi\n"))
  (check-equal? "\xFEFF;a" (djot-doc-source (bytevector->djot #vu8(#xEF #xBB #xBF #xEF #xBB #xBF 97)))))

(test "a task list item at the end of a text without a final newline"
  (check-equal? "<ul class=\"task-list\">\n<li>\n<input disabled=\"\" type=\"checkbox\"/>\n</li>\n</ul>\n"
    (html "* [ ]"))
  (check-equal? (html "- [ ] a\n- [x]\n") (html "- [ ] a\n- [x]")))

(test "a fenced block with an empty raw format is code"
  (check-equal? "<pre><code>x\n</code></pre>\n" (html "``` =\nx\n```\n"))
  (check-equal? "<pre><code>x\n</code></pre>\n" (html "~~~ =\nx\n~~~\n")))

(test "bytevector->djot decodes invalid bytes as U+FFFD"
  (check-equal? "<p>a\xFFFD;b</p>\n" (djot->html-string (bytevector->djot #vu8(97 #xFF 98)))))

(test "the parser checks its argument"
  (check-raises assertion-violation? (string->djot 'x))
  (check-raises assertion-violation? (bytevector->djot "x")))

(test "references, generated references and footnotes are in definition order"
  (let ([doc (string->djot "# Head\n\n[b]: /2\n[a]: /1\n[b]: /3\n\n[^n]: note\n")])
    (check-equal? '("b" "a") (map car (djot-doc-references doc)))
    (check-equal? "/3" (djot-reference-destination (cdr (assoc "b" (djot-doc-references doc)))))
    (check-equal? '("Head") (map car (djot-doc-auto-references doc)))
    (check-equal? "#Head" (djot-reference-destination (djot-doc-reference doc "Head")))
    (check-equal? '("n") (map car (djot-doc-footnotes doc)))))

(test "attribute names and values are immutable strings that nodes can share"
  (let* ([doc (string->djot "[a]{#h .c k=v}[b]{.c .d k=v id=i x=\"\"}\n")]
         [spans (djot-node-children (car (djot-node-children doc)))]
         [first (djot-node-attributes (car spans))]
         [second (djot-node-attributes (cadr spans))])
    (check-equal? '(("id" . "h") ("class" . "c") ("k" . "v")) first)
    (check-equal? '(("class" . "c d") ("k" . "v") ("id" . "i") ("x" . "")) second)
    (check (eq? (cdr (assoc "k" first)) (cdr (assoc "k" second))))
    (check (for-all (lambda (entry) (and (immutable-string? (car entry)) (immutable-string? (cdr entry))))
                    (append first second))))
  (let ([attributes (djot-node-attributes (car (djot-node-children (string->djot "{.a}\n{.b k=v}\npara\n"))))])
    (check-equal? '(("class" . "a b") ("k" . "v")) attributes)
    (check (for-all (lambda (entry) (immutable-string? (cdr entry))) attributes))))

(test "smart punctuation texts are immutable strings"
  (let ([marks (filter djot-smart-punctuation?
                 (djot-node-children (car (djot-node-children (string->djot "a... it's -- ---\n")))))])
    (check-equal? '(ellipses right-single-quote en-dash em-dash) (map djot-smart-punctuation-type marks))
    (check-equal? '("..." "'" "--" "---") (map djot-smart-punctuation-text marks))
    (check (for-all (lambda (mark) (immutable-string? (djot-smart-punctuation-text mark))) marks))))

(test-exit)
