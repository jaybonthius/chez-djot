(import (chezscheme) (djot) (check))

(define (str text) (make-djot-str '() #f #f text))

(test "constructors take the base fields, then children, then the kind's fields"
  (let ([list-node (make-djot-ordered-list '(("class" . "c")) 3 17 '() "1." #t 4)])
    (check (djot-node? list-node))
    (check (djot-ordered-list? list-node))
    (check-equal? '(("class" . "c")) (djot-node-attributes list-node))
    (check-equal? 3 (djot-node-start list-node))
    (check-equal? 17 (djot-node-end list-node))
    (check-equal? "1." (djot-ordered-list-style list-node))
    (check-equal? #t (djot-ordered-list-tight? list-node))
    (check-equal? 4 (djot-ordered-list-number list-node)))
  (let ([cell (make-djot-cell '() #f #f (list (str "a")) #t 'center)])
    (check-equal? #t (djot-cell-head? cell))
    (check-equal? 'center (djot-cell-align cell))
    (check (not (djot-node-start cell))))
  (let ([punctuation (make-djot-smart-punctuation '() 0 2 'en-dash "--")])
    (check-equal? 'en-dash (djot-smart-punctuation-type punctuation))
    (check-equal? "--" (djot-smart-punctuation-text punctuation)))
  (let ([link (make-djot-link '() #f #f (list (str "a")) #f "label")])
    (check-equal? #f (djot-link-destination link))
    (check-equal? "label" (djot-link-reference link)))
  (check-equal? "x" (djot-footnote-reference-label (make-djot-footnote-reference '() #f #f "x")))
  (check-equal? 2 (djot-heading-level (make-djot-heading '() #f #f '() 2 "id")))
  (check-equal? "id" (djot-heading-auto-id (make-djot-heading '() #f #f '() 2 "id")))
  (check-equal? #t (djot-task-list-item-checked? (make-djot-task-list-item '() #f #f '() #t))))

(test "constructors check their arguments"
  (let ([note (make-djot-footnote '() #f #f '() "n")]
        [reference (make-djot-reference '() #f #f "r" "/r")])
    (check-raises assertion-violation? (make-djot-str '(1) #f #f "x"))
    (check-raises assertion-violation? (make-djot-str '(("a" . 1)) #f #f "x"))
    (check-raises assertion-violation? (make-djot-str '(("a" . "b") . "c") #f #f "x"))
    (check-raises assertion-violation? (make-djot-str '() -1 #f "x"))
    (check-raises assertion-violation? (make-djot-str '() #f 1.5 "x"))
    (check-raises assertion-violation? (make-djot-str '() #f #f 'x))
    (check-raises assertion-violation? (make-djot-para '() #f #f "x"))
    (check-raises assertion-violation? (make-djot-para '() #f #f '("x")))
    (check-raises assertion-violation? (make-djot-heading '() #f #f '() 0 #f))
    (check-raises assertion-violation? (make-djot-heading '() #f #f '() 1 'id))
    (check-raises assertion-violation? (make-djot-code-block '() #f #f 1 "x"))
    (check-raises assertion-violation? (make-djot-bullet-list '() #f #f '() "-" 'yes))
    (check-raises assertion-violation? (make-djot-ordered-list '() #f #f '() "1." #t 1.5))
    (check-raises assertion-violation? (make-djot-cell '() #f #f '() #t 'middle))
    (check-raises assertion-violation? (make-djot-smart-punctuation '() #f #f 'other "x"))
    (check-raises assertion-violation? (make-djot-link '() #f #f '() 1 #f))
    (check-raises assertion-violation? (make-djot-doc '() #f #f '() '(("a" . 1)) '() '() #f))
    (check-raises assertion-violation? (make-djot-doc '() #f #f '() (list (cons 'a reference)) '() '() #f))
    (check-raises assertion-violation? (make-djot-doc '() #f #f '() '() (list (cons "n" note)) '() #f))
    (check-raises assertion-violation? (make-djot-doc '() #f #f '() '() '() (list (cons "r" reference)) #f))
    (check-raises assertion-violation? (make-djot-doc '() #f #f '() '() '() '() 'source))
    (check-equal? 'make-djot-para
      (guard (c [(assertion-violation? c) (condition-who c)]) (make-djot-para '() #f #f '("x"))))
    (check (djot-doc? (make-djot-doc '(("k" . "v")) 0 0 '()
                        (list (cons "r" reference)) '() (list (cons "n" note)) "")))
    (check (djot-ordered-list? (make-djot-ordered-list '() #f #f '() "1." #f (expt 10 30))))))

(test "djot-node-children is () for leaves and the children for containers"
  (let* ([text (str "a")] [para (make-djot-para '() #f #f (list text))])
    (check-equal? (list text) (djot-node-children para))
    (check-equal? '() (djot-node-children text))
    (check-equal? '() (djot-node-children (make-djot-thematic-break '() #f #f)))
    (check-raises assertion-violation? (djot-node-children "a"))))

(test "the predicates tell the kinds apart"
  (let ([nodes (list (make-djot-para '() #f #f '()) (str "a") (make-djot-emph '() #f #f '())
                     (make-djot-reference '() #f #f "a" "/a")
                     (make-djot-doc '() #f #f '() '() '() '() #f))])
    (check-equal? '(#t #t #t #t #t) (map djot-node? nodes))
    (check-equal? '(#t #f #f #f #f) (map djot-para? nodes))
    (check-equal? '(#f #t #f #f #f) (map djot-str? nodes))
    (check-equal? '(#f #f #f #f #t) (map djot-doc? nodes))
    (check (not (djot-node? "a")))
    (check (not (djot-node? (make-hashtable string-hash string=?))))))

(test "djot-doc-reference normalises the label and prefers explicit references"
  (let* ([explicit (make-djot-reference '() #f #f "Foo bar" "/explicit")]
         [heading (make-djot-reference '() #f #f "Foo bar" "#Foo-bar")]
         [other (make-djot-reference '() #f #f "Other" "#Other")]
         [doc (make-djot-doc '() #f #f '()
                (list (cons "Foo bar" explicit))
                (list (cons "Foo bar" heading) (cons "Other" other))
                '() #f)])
    (check-equal? explicit (djot-doc-reference doc "Foo bar"))
    (check-equal? explicit (djot-doc-reference doc " Foo \n\t bar\xFEFF;"))
    (check-equal? other (djot-doc-reference doc "\xA0;Other "))
    (check-equal? #f (djot-doc-reference doc "foo bar"))
    (check-equal? #f (djot-doc-reference doc "Foo\xA0;bar"))
    (check-equal? #f (djot-doc-reference doc ""))
    (check-raises assertion-violation? (djot-doc-reference doc 'Other))
    (check-raises assertion-violation? (djot-doc-reference "doc" "Other"))))

(test "a later reference for a label replaces an earlier one"
  (let* ([first (make-djot-reference '() #f #f "a  b" "/1")]
         [second (make-djot-reference '() #f #f "a b" "/2")]
         [doc (make-djot-doc '() #f #f '() (list (cons "a  b" first) (cons "a b" second)) '() '() #f)])
    (check-equal? second (djot-doc-reference doc "a b"))))

(test "djot-line-column counts lines and columns from 1"
  (let ([doc (make-djot-doc '() #f #f '() '() '() '() "ab\ncd\n\n\xE9;f")])
    (define (at offset) (call-with-values (lambda () (djot-line-column doc offset)) list))
    (check-equal? '(1 1) (at 0))
    (check-equal? '(1 3) (at 2))
    (check-equal? '(2 1) (at 3))
    (check-equal? '(2 3) (at 5))
    (check-equal? '(3 1) (at 6))
    (check-equal? '(4 1) (at 7))
    (check-equal? '(4 2) (at 8))
    (check-equal? '(4 3) (at 9))
    (check-equal? '(2 2) (at 4))
    (check-raises assertion-violation? (djot-line-column doc 10))
    (check-raises assertion-violation? (djot-line-column doc -1))
    (check-raises assertion-violation? (djot-line-column doc 1.0))
    (check-raises assertion-violation?
      (djot-line-column (make-djot-doc '() #f #f '() '() '() '() #f) 0)))
  (let ([doc (make-djot-doc '() #f #f '() '() '() '() "")])
    (check-equal? '(1 1) (call-with-values (lambda () (djot-line-column doc 0)) list))))

(test "djot-line-column agrees with Chez's locate-source"
  (let* ([text "one\ntwo  three\n\nfour\nfive\n"]
         [path "build/ast-test-line-column.txt"]
         [doc (make-djot-doc '() #f #f '() '() '() '() text)])
    (dynamic-wind
      (lambda () (call-with-output-file path (lambda (p) (put-string p text)) '(replace)))
      (lambda ()
        (let ([sfd (call-with-port (open-file-input-port path)
                     (lambda (p) (make-source-file-descriptor path p)))])
          (do ([offset 0 (+ offset 1)]) ((= offset (string-length text)))
            (check-equal?
              (call-with-values (lambda () (locate-source sfd offset)) (lambda (path line column) (list line column)))
              (call-with-values (lambda () (djot-line-column doc offset)) list)))))
      (lambda () (delete-file path)))))

(test-exit)
