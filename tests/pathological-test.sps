(import (chezscheme) (djot) (check))

(define (repeat s n)
  (call-with-string-output-port
    (lambda (p) (do ([i 0 (+ i 1)]) ((= i n)) (put-string p s)))))

(define (concat . strings) (apply string-append strings))

(define n 500)

(define inputs
  (list
    (cons "nested strong emph" (concat (repeat "_a *a " (* 65 n)) "b" (repeat " a* a_" (* 65 n))))
    (cons "many emph closers with no openers" (repeat "a_ " (* 65 n)))
    (cons "many emph openers with no closers" (repeat "_a " (* 65 n)))
    (cons "many link closers with no openers" (repeat "a]" (* 65 n)))
    (cons "many link openers with no closers" (repeat "[a" (* 65 n)))
    (cons "mismatched openers and closers" (repeat "*a_ " (* 50 n)))
    (cons "issue cmark#389" (concat (repeat "*a " (* 20 n)) (repeat "_a*_ " (* 20 n))))
    (cons "openers and closers multiple of 3" (concat "a**b" (repeat "8* " (* 50 n))))
    (cons "link openers and emph closers" (repeat "[ a_" (* 50 n)))
    (cons "pattern [ (]( repeated" (repeat "[ (](" (* 80 n)))
    (cons "nested brackets" (concat (repeat "[" (* 50 n)) "a" (repeat "]" (* 50 n))))
    (cons "nested block quotes" (concat (repeat "> " (* 50 n)) "a"))
    (cons "deeply nested lists"
      (apply string-append (map (lambda (i) (concat (make-string (+ i 1) #\space) "* a\n")) (iota n))))
    (cons "backticks"
      (apply string-append (map (lambda (i) (concat "e" (make-string (+ i 1) #\`))) (iota (* 5 n)))))
    (cons "unclosed links" (repeat "[a](<b" (* 30 n)))
    (cons "unclosed attributes" (repeat "a{#id k=" (* 30 n)))
    (cons "unclosed link destinations" (repeat "[label](blah  " (* 30 n)))
    (cons "unbalanced paren in link label" (repeat "[label)](foo " (* 30 n)))
    (cons "balanced nested parens in destinations" (repeat "[label](foo (bar (baz)) " (* 30 n)))
    (cons "footnote reference starts" (repeat "[^" (* 200 n)))))

(define (milliseconds thunk)
  (let ([start (current-time 'time-monotonic)])
    (thunk)
    (let ([d (time-difference (current-time 'time-monotonic) start)])
      (+ (* 1000. (time-second d)) (/ (time-nanosecond d) 1e6)))))

(for-each
  (lambda (entry)
    (test (car entry)
      (let* ([doc #f]
             [html #f]
             [parse-ms (milliseconds (lambda () (set! doc (string->djot (cdr entry)))))]
             [render-ms (milliseconds (lambda () (set! html (djot->html-bytevector doc))))])
        (check (djot-doc? doc))
        (check (> (bytevector-length html) 0))
        (printf "~a: ~a characters, parse ~,1f ms, render ~,1f ms\n"
          (car entry) (string-length (cdr entry)) parse-ms render-ms))))
  inputs)

;; djot.js once looked labels up in objects that inherit these names.
(define (html s) (djot->html-string (string->djot s)))

(define inherited-names '("constructor" "toString" "hasOwnProperty" "__proto__"))

(test "undefined labels named like Object.prototype properties are undefined"
  (for-each
    (lambda (label)
      (check-equal? (html "hi[^zzz]\n") (html (format "hi[^~a]\n" label)))
      (check-equal? (html "[x][zzz]\n") (html (format "[x][~a]\n" label))))
    inherited-names))

(test "defined labels named like Object.prototype properties resolve"
  (for-each
    (lambda (label)
      (check-equal? "<p><a href=\"/url\">x</a></p>\n" (html (format "[x][~a]\n\n[~a]: /url\n" label label)))
      (check-equal? (html "hi[^zzz]\n\n[^zzz]: note\n") (html (format "hi[^~a]\n\n[^~a]: note\n" label label))))
    inherited-names)
  (check-equal? "<section id=\"constructor\">\n<h1>constructor</h1>\n</section>\n" (html "# constructor\n")))

(test-exit)
