;; filters.test is left out: it tests djot.js's filters, which this library
;; does not have.
(import (chezscheme) (djot) (ast-text) (check))

(define test-files
  '("attributes" "block_quote" "code_blocks" "definition_lists" "symb" "emphasis" "escapes"
    "fenced_divs" "footnotes" "headings" "insert_delete_mark" "links_and_images" "lists" "math"
    "para" "raw" "regression" "smart" "spans" "sourcepos" "super_subscript" "tables"
    "task_lists" "thematic_breaks" "verbatim"))

(define (read-lines path)
  (let ([text (call-with-port
                (open-file-input-port path (file-options) (buffer-mode block)
                  (make-transcoder (utf-8-codec) (eol-style none)))
                get-string-all)])
    (let loop ([start 0] [i 0] [lines '()])
      (cond
        [(fx= i (string-length text)) (reverse (cons (substring text start i) lines))]
        [(char=? (string-ref text i) #\newline)
         (loop (fx+ i 1) (fx+ i 1) (cons (substring text start i) lines))]
        [else (loop start (fx+ i 1) lines)]))))

(define (backticks line)
  (let loop ([i 0])
    (if (and (fx< i (string-length line)) (char=? (string-ref line i) #\`)) (loop (fx+ i 1)) i)))

(define (starts-with-backticks? line n)
  (fx>= (backticks line) n))

(define (options-of line)
  (let loop ([i (backticks line)])
    (if (and (fx< i (string-length line)) (char-whitespace? (string-ref line i)))
        (loop (fx+ i 1))
        (substring line i (string-length line)))))

(define (string-has? s c)
  (and (memv c (string->list s)) #t))

(define (split-tests lines)
  (let loop ([lines lines] [number 0] [tests '()])
    (let skip ([lines lines] [number number])
      (cond
        [(null? lines) (reverse tests)]
        [(not (starts-with-backticks? (car lines) 3)) (skip (cdr lines) (fx+ number 1))]
        [else
         (let ([ticks (backticks (car lines))] [options (options-of (car lines))] [line (fx+ number 1)])
           (let input ([lines (cdr lines)] [number (fx+ number 1)] [acc '()])
             (if (or (null? lines) (member (car lines) '("." "!")))
                 (let output ([lines (if (null? lines) lines (cdr lines))]
                              [number (if (null? lines) number (fx+ number 1))]
                              [out '()])
                   (if (or (null? lines) (starts-with-backticks? (car lines) ticks))
                       (loop (if (null? lines) lines (cdr lines))
                         (if (null? lines) number (fx+ number 1))
                         (cons (list line options
                                 (apply string-append (reverse acc))
                                 (apply string-append (reverse out)))
                               tests))
                       (output (cdr lines) (fx+ number 1) (cons (string-append (car lines) "\n") out))))
                 (input (cdr lines) (fx+ number 1) (cons (string-append (car lines) "\n") acc)))))]))))

(define cases 0)

(for-each
  (lambda (name)
    (for-each
      (lambda (t)
        (let ([line (car t)] [options (cadr t)] [input (caddr t)] [expected (cadddr t)])
          (set! cases (+ cases 1))
          (test (format "~a.test:~a" name line)
            (let ([doc (string->djot input)])
              (check-equal? expected
                (if (string-has? options #\a)
                    (djot->ast-text doc (string-has? options #\p))
                    (djot->html-string doc)))))))
      (split-tests (read-lines (string-append "tests/djot.js/test/" name ".test")))))
  test-files)

(printf "~a cases from ~a files\n" cases (length test-files))
(test-exit)
