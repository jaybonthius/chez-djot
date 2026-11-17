(import (chezscheme) (djot) (check))

(define (file-string path)
  (utf8->string (call-with-port (open-file-input-port path) get-bytevector-all)))

(define (test-file-inputs path)
  (let ([p (open-string-input-port (file-string path))])
    (let loop ([inputs '()] [current #f])
      (let ([line (get-line p)])
        (cond
          [(eof-object? line) (reverse inputs)]
          [(and (not current) (fx>= (string-length line) 3) (string=? (substring line 0 3) "```"))
           (loop inputs "")]
          [(and current (member line '("." "!"))) (loop (cons current inputs) #f)]
          [current (loop inputs (string-append current line "\n"))]
          [else (loop inputs #f)])))))

(define problems 0)

(define (check-document name doc)
  (let ([source (djot-doc-source doc)] [n (string-length (djot-doc-source doc))])
    (define (problem fmt . args)
      (set! problems (+ problems 1))
      (when (<= problems 10)
        (printf "~a: ~?\n" name fmt args)))
    (define (line-end i)
      (cond
        [(fx>= i n) n]
        [(char=? (string-ref source i) #\newline) (fx+ i 1)]
        [else (line-end (fx+ i 1))]))
    (define (walk node low high)
      (let ([start (djot-node-start node)] [end (djot-node-end node)])
        (cond
          [(not (and (fixnum? start) (fixnum? end) (fx<= low start end high)))
           (problem "~s from ~s to ~s is outside ~s to ~s" node start end low high)]
          [(and (djot-str? node) (not (string=? (djot-str-text node) (substring source start end))))
           (problem "str ~s has the source ~s" (djot-str-text node) (substring source start end))]
          [else
           (for-each
             (lambda (child)
               ;; djot.js ends a div that its closing fence ends at the
               ;; fence, but blocks still open inside it with the fence's
               ;; line, so they may run past the div up to that line's end.
               (walk child start
                 (if (and (djot-div? node) (fx> end 0) (char=? (string-ref source (fx- end 1)) #\:))
                     (line-end end)
                     end)))
             (djot-node-children node))])))
    (walk doc 0 n)
    (for-each
      (lambda (entry) (walk (cdr entry) 0 n))
      (append (djot-doc-references doc) (djot-doc-footnotes doc)))))

(define sources
  (if (null? (command-line-arguments))
      (cons (cons "readme.dj" (file-string "tests/djot.js/bench/readme.dj"))
            (apply append
              (map (lambda (path)
                     (let ([inputs (test-file-inputs path)])
                       (map (lambda (input i) (cons (format "~a input ~a" path (+ i 1)) input))
                            inputs (iota (length inputs)))))
                   (map (lambda (name) (string-append "tests/djot.js/test/" name))
                        (filter (lambda (f) (equal? (path-extension f) "test"))
                          (sort string<? (directory-list "tests/djot.js/test")))))))
      (map (lambda (path) (cons path (file-string path))) (command-line-arguments))))

(test "positions"
  (for-each (lambda (entry) (check-document (car entry) (string->djot (cdr entry)))) sources)
  (check-equal? 0 problems)
  (printf "~a documents\n" (length sources)))

(test "line and column"
  (let ([doc (string->djot "ab\ncd\n")])
    (let-values ([(line column) (djot-line-column doc 0)]) (check-equal? '(1 1) (list line column)))
    (let-values ([(line column) (djot-line-column doc 2)]) (check-equal? '(1 3) (list line column)))
    (let-values ([(line column) (djot-line-column doc 4)]) (check-equal? '(2 2) (list line column)))
    (let* ([para (car (djot-node-children doc))] [str (car (djot-node-children para))])
      (check-equal? '(0 6) (list (djot-node-start para) (djot-node-end para)))
      (check-equal? '(0 2) (list (djot-node-start str) (djot-node-end str))))))

(test-exit)
