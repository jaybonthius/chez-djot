(import (chezscheme) (djot) (ast-datum) (ast-text) (check))

(define (file-bytes path)
  (let ([bv (call-with-port (open-file-input-port path) get-bytevector-all)])
    (if (eof-object? bv) (bytevector) bv)))

(define (lines s)
  (let ([p (open-string-input-port s)])
    (let loop ([acc '()])
      (let ([line (get-line p)])
        (if (eof-object? line) (reverse acc) (loop (cons line acc)))))))

(define (first-difference expected actual)
  (let loop ([n 1] [expected (lines expected)] [actual (lines actual)])
    (cond
      [(and (null? expected) (null? actual)) #f]
      [(or (null? expected) (null? actual)
           (not (string=? (car expected) (car actual))))
       (list n
             (if (null? expected) 'end (car expected))
             (if (null? actual) 'end (car actual)))]
      [else (loop (+ n 1) (cdr expected) (cdr actual))])))

(define (check-corpus source prefix)
  (test prefix
    (let* ([djot-js-doc (read-ast-file (string-append prefix ".ast"))]
           [expected (file-bytes (string-append prefix ".html"))]
           [source-bytes (file-bytes source)]
           [doc (bytevector->djot source-bytes)])
      (check (bytevector=? expected (djot->html-bytevector djot-js-doc)))
      (check-equal? #f
        (first-difference (djot->ast-text djot-js-doc #f) (djot->ast-text doc #f)))
      (check-equal? #f
        (first-difference (utf8->string expected) (djot->html-string doc)))
      (check (bytevector=? expected (djot->html-bytevector doc)))
      (check (bytevector=? expected (djot->html-bytevector (string->djot (utf8->string source-bytes))))))))

(if (null? (command-line-arguments))
    (check-corpus "tests/djot.js/bench/readme.dj" "tests/data/readme")
    (for-each (lambda (prefix) (check-corpus (string-append prefix ".dj") prefix))
      (command-line-arguments)))

(test-exit)
