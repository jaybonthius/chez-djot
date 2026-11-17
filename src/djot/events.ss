;; An annotation is a fixnum so that an event is three fixnums in fxvectors
;; and costs no allocation.  The two macros map names to the fixnums at
;; expansion time, so a misspelt name fails the build.
;;
;; case tests its clauses in order, and a clause of more than eight keys
;; calls member, so annotation-case looks up the clause number in a table
;; and finds its body by binary search.
(define-syntax define-annotations
  (lambda (x)
    (syntax-case x ()
      [(_ annotation-name case-name name ...)
       (with-syntax ([(code ...) (iota (length #'(name ...)))]
                     [count (length #'(name ...))])
         #'(begin
             (meta define annotation-codes '((name . code) ...))
             (meta define (lookup-annotation who form key)
               (cond
                 [(assq (syntax->datum key) annotation-codes) => cdr]
                 [else (syntax-violation who "unknown annotation" form key)]))
             (meta define (clause-dispatch index bodies low high)
               (if (fx= (fx- high low) 1)
                   #`(let () #,@(list-ref bodies low))
                   (let ([middle (fxsrl (fx+ low high) 1)])
                     #`(if (fx< #,index #,middle)
                           #,(clause-dispatch index bodies low middle)
                           #,(clause-dispatch index bodies middle high)))))
             (define-syntax annotation-name
               (lambda (y)
                 (syntax-case y ()
                   [(_ key) (lookup-annotation 'annotation-name y #'key)])))
             (define-syntax case-name
               (lambda (y)
                 (syntax-case y ()
                   [(_ e clause (... ...))
                    (let* ([clauses #'(clause (... ...))]
                           [last-clause (list-ref clauses (fx- (length clauses) 1))]
                           [keyed (filter
                                    (lambda (c)
                                      (syntax-case c (else)
                                        [(else body (... ...))
                                         (if (eq? c last-clause)
                                             #f
                                             (syntax-violation 'case-name "else must be the last clause" y c))]
                                        [_ #t]))
                                    clauses)]
                           [default (syntax-case last-clause (else)
                                      [(else body (... ...)) #'(body (... ...))]
                                      [_ #'((void))])]
                           [bodies (append (map (lambda (c) (syntax-case c () [(keys body (... ...)) #'(body (... ...))]))
                                                keyed)
                                           (list default))]
                           [table (make-bytevector count (length keyed))])
                      (for-each
                        (lambda (c i)
                          (syntax-case c ()
                            [((key (... ...)) body (... ...))
                             (for-each
                               (lambda (k)
                                 (let ([n (lookup-annotation 'case-name y k)])
                                   (when (fx< (bytevector-u8-ref table n) (length keyed))
                                     (syntax-violation 'case-name "annotation in two clauses" y k))
                                   (bytevector-u8-set! table n i)))
                               #'(key (... ...)))]))
                        keyed
                        (iota (length keyed)))
                      #`(let ([index (bytevector-u8-ref '#,table e)])
                          #,(clause-dispatch #'index bodies 0 (length bodies))))])))))])))

(define-annotations annotation-code annotation-case
  open-para close-para open-heading close-heading open-block-quote close-block-quote
  open-caption close-caption open-footnote close-footnote note-label
  open-reference-definition close-reference-definition reference-key reference-value
  thematic-break open-list close-list open-list-item close-list-item
  checkbox-checked checkbox-unchecked open-table close-table open-row close-row
  separator-default separator-left separator-right separator-center open-cell close-cell
  open-block-attributes close-block-attributes open-div close-div class
  open-code-block close-code-block raw-format code-language blankline
  str soft-break hard-break escape non-breaking-space symb footnote-reference
  open-emph close-emph open-strong close-strong open-span close-span open-mark close-mark
  open-superscript close-superscript open-subscript close-subscript
  open-insert close-insert open-delete close-delete
  open-double-quoted close-double-quoted open-single-quoted close-single-quoted
  open-linktext close-linktext open-imagetext close-imagetext image-marker
  open-destination close-destination open-reference close-reference
  open-verbatim close-verbatim open-inline-math close-inline-math
  open-display-math close-display-math open-url close-url open-email close-email
  open-marker left-single-quote right-single-quote left-double-quote right-double-quote
  ellipses en-dash em-dash
  open-attributes close-attributes id key value comment
  attribute-space id-marker class-marker equal-marker quote-marker)

(define-record-type events
  (nongenerative)
  (sealed #t)
  (fields (mutable starts) (mutable ends) (mutable annotations) (mutable count))
  (protocol
    (lambda (new)
      (lambda (size)
        (new (make-fxvector size) (make-fxvector size) (make-fxvector size) 0)))))

(define (events-grow! e)
  (define (grow v)
    (let ([new (make-fxvector (fx* 2 (fxvector-length v)))])
      (do ([i 0 (fx+ i 1)]) ((fx= i (fxvector-length v)) new)
        (fxvector-set! new i (fxvector-ref v i)))))
  (events-starts-set! e (grow (events-starts e)))
  (events-ends-set! e (grow (events-ends e)))
  (events-annotations-set! e (grow (events-annotations e))))

(define (events-add! e start end annotation)
  (let ([n (events-count e)])
    (when (fx= n (fxvector-length (events-starts e)))
      (events-grow! e))
    (fxvector-set! (events-starts e) n start)
    (fxvector-set! (events-ends e) n end)
    (fxvector-set! (events-annotations e) n annotation)
    (events-count-set! e (fx+ n 1))))

(define (events-start e i) (fxvector-ref (events-starts e) i))
(define (events-end e i) (fxvector-ref (events-ends e) i))
(define (events-annotation e i) (fxvector-ref (events-annotations e) i))
(define (events-end-set! e i end) (fxvector-set! (events-ends e) i end))
(define (events-annotation-set! e i annotation) (fxvector-set! (events-annotations e) i annotation))

(define (events-set! e i start end annotation)
  (if (fx= i (events-count e))
      (events-add! e start end annotation)
      (begin
        (fxvector-set! (events-starts e) i start)
        (fxvector-set! (events-ends e) i end)
        (fxvector-set! (events-annotations e) i annotation))))

(define (events-insert! e i start end annotation)
  (let ([n (events-count e)])
    (events-add! e 0 0 0)
    (let ([starts (events-starts e)] [ends (events-ends e)] [annotations (events-annotations e)])
      (do ([j n (fx- j 1)]) ((fx= j i))
        (fxvector-set! starts j (fxvector-ref starts (fx- j 1)))
        (fxvector-set! ends j (fxvector-ref ends (fx- j 1)))
        (fxvector-set! annotations j (fxvector-ref annotations (fx- j 1))))
      (events-set! e i start end annotation))))

(define (events-pop! e)
  (let ([n (events-count e)])
    (when (fx> n 0) (events-count-set! e (fx- n 1)))))

(define (events-truncate! e n)
  (events-count-set! e n))

(define (events-last-annotation e)
  (let ([n (events-count e)])
    (and (fx> n 0) (events-annotation e (fx- n 1)))))
