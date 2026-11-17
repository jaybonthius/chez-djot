(define (js-space? c)
  (and (char? c) (js-whitespace? c)))

(define (ascii-alphanumeric? c)
  (and (char? c)
       (or (char<=? #\a c #\z) (char<=? #\A c #\Z) (char<=? #\0 c #\9))))

;; ASCII only, as JavaScript's \w is.
(define (word-char? c)
  (or (ascii-alphanumeric? c) (eqv? c #\_)))

(define (key-char? c)
  (or (word-char? c) (eqv? c #\:) (eqv? c #\-)))

(define (identifier-char? c)
  (and (char? c)
       (not (js-whitespace? c))
       (not (memv c '(#\] #\[ #\~ #\! #\@ #\# #\$ #\% #\^ #\& #\* #\( #\) #\{ #\} #\`
                      #\, #\. #\< #\> #\\ #\| #\= #\+ #\/ #\?)))))

;; The character at i, or #f past either end, as JavaScript's charAt and
;; codePointAt match nothing there.
(define (char-at s i)
  (and (fx>= i 0) (fx< i (string-length s)) (string-ref s i)))

;; States are symbols, as djot.js's are names; the parser runs once per
;; attribute character, far less often than the inline scan.
(define-record-type attribute-parser
  (nongenerative)
  (sealed #t)
  (fields subject (mutable state) (mutable token-start) (mutable lastpos) events)
  (protocol
    (lambda (new)
      (lambda (subject)
        (new subject 'start #f #f (make-events 8))))))

(define (attribute-parser-reset! p)
  (attribute-parser-state-set! p 'start)
  (attribute-parser-token-start-set! p #f)
  (attribute-parser-lastpos-set! p #f)
  (events-truncate! (attribute-parser-events p) 0))

(define (attribute-step p pos)
  (let ([c (char-at (attribute-parser-subject p) pos)]
        [events (attribute-parser-events p)])
    (define (add! start end annotation) (events-add! events start end annotation))
    (define (token-start! pos) (attribute-parser-token-start-set! p pos))
    (define (scanned?)
      (let ([token-start (attribute-parser-token-start p)] [lastpos (attribute-parser-lastpos p)])
        (and token-start lastpos (fx> lastpos token-start))))
    (define (finish-name annotation)
      (when (scanned?)
        (add! (fx+ (attribute-parser-token-start p) 1) (attribute-parser-lastpos p) annotation))
      (token-start! #f))
    (define (end-name annotation)
      (cond
        [(eqv? c #\}) (finish-name annotation) 'done]
        [(js-space? c)
         (finish-name annotation)
         (unless (memv c '(#\return #\newline)) (add! pos pos (annotation-code attribute-space)))
         'scanning]
        [else 'fail]))
    (case (attribute-parser-state p)
      [(start) (if (eqv? c #\{) 'scanning 'fail)]
      [(fail) 'fail]
      [(done) 'done]
      [(scanning)
       (cond
         [(memv c '(#\newline #\return)) 'scanning]
         [(memv c '(#\space #\tab)) (add! pos pos (annotation-code attribute-space)) 'scanning]
         [(eqv? c #\}) 'done]
         [(eqv? c #\#) (token-start! pos) (add! pos pos (annotation-code id-marker)) 'scanning-id]
         [(eqv? c #\%) (token-start! pos) 'scanning-comment]
         [(eqv? c #\.) (token-start! pos) (add! pos pos (annotation-code class-marker)) 'scanning-class]
         [(key-char? c) (token-start! pos) 'scanning-key]
         [else 'fail])]
      [(scanning-comment)
       (cond
         [(eqv? c #\%)
          (let ([token-start (attribute-parser-token-start p)])
            (when (and token-start (fx> token-start 0) (fx> pos token-start))
              (add! token-start pos (annotation-code comment))))
          'scanning]
         [(eqv? c #\}) 'done]
         [else 'scanning-comment])]
      [(scanning-id)
       (if (identifier-char? c) 'scanning-id (end-name (annotation-code id)))]
      [(scanning-class)
       (if (key-char? c) 'scanning-class (end-name (annotation-code class)))]
      [(scanning-key)
       (cond
         [(and (eqv? c #\=) (attribute-parser-token-start p) (attribute-parser-lastpos p))
          (add! (attribute-parser-token-start p) (attribute-parser-lastpos p) (annotation-code key))
          (add! pos pos (annotation-code equal-marker))
          (token-start! #f)
          'scanning-value]
         [(key-char? c) 'scanning-key]
         [else 'fail])]
      [(scanning-value)
       (cond
         [(eqv? c #\") (token-start! pos) (add! pos pos (annotation-code quote-marker)) 'scanning-quoted-value]
         [(key-char? c) (token-start! pos) 'scanning-bare-value]
         [else 'fail])]
      [(scanning-bare-value)
       (let ([value! (lambda ()
                       (add! (attribute-parser-token-start p) (attribute-parser-lastpos p) (annotation-code value))
                       (token-start! #f))]
             [ready? (and (attribute-parser-token-start p) (attribute-parser-lastpos p))])
         (cond
           [(key-char? c) 'scanning-bare-value]
           [(and (eqv? c #\}) ready?) (value!) 'done]
           [(and (js-space? c) ready?)
            (value!)
            (unless (memv c '(#\return #\newline)) (add! pos pos (annotation-code attribute-space)))
            'scanning]
           [else 'fail]))]
      [(scanning-escaped) 'scanning-quoted-value]
      [(scanning-escaped-in-continuation) 'scanning-quoted-value-continuation]
      [(scanning-quoted-value)
       (let ([token-start (attribute-parser-token-start p)] [lastpos (attribute-parser-lastpos p)])
         (cond
           [(and (eqv? c #\") token-start lastpos)
            (add! (fx+ token-start 1) lastpos (annotation-code value))
            (add! pos pos (annotation-code quote-marker))
            (token-start! #f)
            'scanning]
           [(and (eqv? c #\newline) token-start lastpos)
            (add! (fx+ token-start 1) pos (annotation-code value))
            (token-start! #f)
            'scanning-quoted-value-continuation]
           [(eqv? c #\\) 'scanning-escaped]
           [else 'scanning-quoted-value]))]
      [(scanning-quoted-value-continuation)
       (unless (attribute-parser-token-start p) (token-start! pos))
       (let ([token-start (attribute-parser-token-start p)] [lastpos (attribute-parser-lastpos p)])
         (cond
           [(and (eqv? c #\") lastpos)
            (add! pos pos (annotation-code quote-marker))
            (add! token-start lastpos (annotation-code value))
            (token-start! #f)
            'scanning]
           [(and (eqv? c #\newline) lastpos)
            (add! token-start pos (annotation-code value))
            (token-start! #f)
            'scanning-quoted-value-continuation]
           [(eqv? c #\\) 'scanning-escaped-in-continuation]
           [else 'scanning-quoted-value-continuation]))])))

(define (quoted-value-char? c)
  (not (memv c '(#\" #\newline #\\))))

(define (comment-char? c)
  (not (memv c '(#\% #\}))))

;; Most of an attribute's characters are inside values and class names,
;; where a step changes nothing, so runs of them are skipped without one.
(define (attribute-run-end p pos end)
  (let ([s (attribute-parser-subject p)])
    (define-syntax run
      (syntax-rules ()
        [(_ ok?)
         (let loop ([i pos])
           (if (and (fx<= i end) (let ([c (char-at s i)]) (and c (ok? c))))
               (loop (fx+ i 1))
               i))]))
    (case (attribute-parser-state p)
      [(scanning-quoted-value) (run quoted-value-char?)]
      [(scanning-quoted-value-continuation)
       (if (attribute-parser-token-start p) (run quoted-value-char?) pos)]
      [(scanning-class scanning-key scanning-bare-value) (run key-char?)]
      [(scanning-id) (run identifier-char?)]
      [(scanning-comment) (run comment-char?)]
      [else pos])))

(define (attribute-parser-feed! p start end)
  (let loop ([pos start])
    (if (fx> pos end)
        (values 'continue end)
        (let ([run-end (attribute-run-end p pos end)])
          (if (fx> run-end pos)
              (begin
                (attribute-parser-lastpos-set! p (fx- run-end 1))
                (loop run-end))
              (let ([state (attribute-step p pos)])
                (attribute-parser-state-set! p state)
                (case state
                  [(done) (values 'done pos)]
                  [(fail) (attribute-parser-lastpos-set! p pos) (values 'fail pos)]
                  [else (attribute-parser-lastpos-set! p pos) (loop (fx+ pos 1))])))))))
