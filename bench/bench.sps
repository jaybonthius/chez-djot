(import (chezscheme) (djot))

(define (seconds t) (+ (time-second t) (/ (time-nanosecond t) 1e9)))

(define batches 8)

;; Collections fall at fixed allocation offsets after a full collection, so
;; batches that all start there see them at the same points of every call,
;; and a call that allocates near a multiple of collect-trip-bytes is timed
;; at one lucky or unlucky phase.
(define (run-batch op inner phase)
  (collect (collect-maximum-generation))
  (let ([pad (make-bytevector (fx+ 1 (quotient (* phase (collect-trip-bytes)) batches)))])
    (bytevector-u8-set! pad 0 0))
  (let ([before (statistics)])
    (do ([i 0 (+ i 1)]) ((= i inner)) (op))
    (let ([d (sstats-difference (statistics) before)])
      (list (/ (seconds (sstats-real d)) inner)
            (/ (sstats-bytes d) inner)
            (/ (seconds (sstats-gc-real d)) inner)))))

(define (mean xs) (/ (apply + xs) (length xs)))

;; Batches of at least 100 ms keep the clock's resolution and one
;; collection from dominating a short call.
(define (measure op)
  (let* ([once (car (run-batch op 1 0))]
         [inner (max 1 (exact (ceiling (/ 0.1 (max once 1e-6)))))])
    (run-batch op inner 0)
    (let ([runs (map (lambda (phase) (run-batch op inner phase)) (iota batches))])
      (values inner (mean (map car runs)) (mean (map cadr runs)) (mean (map caddr runs))))))

(define (report file operation op)
  (let-values ([(inner seconds bytes gc) (measure op)])
    (printf "| ~a | ~a | ~a | ~,3f ms | ~,2f MB | ~a% |\n"
      file operation inner (* seconds 1000) (/ bytes 1e6) (exact (round (* (/ gc seconds) 100))))
    (flush-output-port)))

(printf "Mean of ~a batches after a warm batch, each after a full collection and started a further 1/~a of collect-trip-bytes (~a) later; times and allocation per call.\n\n"
  batches batches (collect-trip-bytes))
(printf "| File | Operation | Calls per batch | Mean | Allocated | Collection |\n|---|---|--:|--:|--:|--:|\n")
(for-each
  (lambda (path)
    (let* ([bytes (call-with-port (open-file-input-port path) get-bytevector-all)]
           [text (utf8->string bytes)]
           [doc (string->djot text)]
           [file (path-last path)])
      (report file "string->djot" (lambda () (string->djot text)))
      (report file "djot->html-bytevector" (lambda () (djot->html-bytevector doc)))
      (report file "bytevector->djot, djot->html-bytevector"
        (lambda () (djot->html-bytevector (bytevector->djot bytes))))
      (report file "utf8->string" (lambda () (utf8->string bytes)))))
  (command-line-arguments))
