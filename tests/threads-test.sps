(import (chezscheme) (djot) (check))

(define (file-string path)
  (utf8->string (call-with-port (open-file-input-port path) get-bytevector-all)))

(define inputs
  (cons (file-string "tests/djot.js/bench/readme.dj")
        (map (lambda (name) (file-string (string-append "tests/djot.js/test/" name)))
             (filter (lambda (f) (equal? (path-extension f) "test"))
               (sort string<? (directory-list "tests/djot.js/test"))))))

(define (convert-all)
  (map (lambda (input) (djot->html-bytevector (string->djot input))) inputs))

(define thread-count 4)
(define rounds 100)

(test "threads"
  (let ([expected (convert-all)]
        [mutex (make-mutex)]
        [done (make-condition)]
        [finished 0]
        [results '()])
    (do ([i 0 (+ i 1)]) ((= i thread-count))
      (fork-thread
        (lambda ()
          (let ([ok? (guard (c [#t #f])
                       (let loop ([round 0])
                         (or (= round rounds)
                             (and (equal? expected (convert-all)) (loop (+ round 1))))))])
            (with-mutex mutex
              (set! results (cons ok? results))
              (set! finished (+ finished 1))
              (condition-signal done))))))
    (with-mutex mutex
      (let wait ()
        (unless (= finished thread-count)
          (condition-wait done mutex)
          (wait))))
    (check-equal? (make-list thread-count #t) results)))

(test-exit)
