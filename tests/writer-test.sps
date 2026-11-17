(import (chezscheme) (djot) (ast-datum) (check))

;; Each expected string is djot.js's renderHTML output for the same tree.

(define (html blocks)
  (djot->html-string (make-djot-doc '() #f #f blocks '() '() '() #f)))

(define (datum-html datum)
  (djot->html-string (datum->djot datum)))

(define (str text) (make-djot-str '() #f #f text))
(define (para . inlines) (make-djot-para '() #f #f inlines))

(test "sections, headings and generated ids"
  (check-equal?
    "<section id=\"Hi\">\n<h1>Hi</h1>\n<p>Some <em>text</em> and <a href=\"https://x.org\">a link</a>.</p>\n<section id=\"Sub\">\n<h2>Sub </h2>\n<div class=\"d\">\n<h1 id=\"Inner\">Inner</h1>\n</div>\n</section>\n</section>\n"
    (datum-html
      '(doc () #f #f
         ((section () #f #f
            ((heading () #f #f ((str () #f #f "Hi")) 1 #f)
             (para () #f #f
               ((str () #f #f "Some ") (emph () #f #f ((str () #f #f "text"))) (str () #f #f " and ")
                (link () #f #f ((str () #f #f "a link")) "https://x.org" #f) (str () #f #f ".")))
             (section () #f #f
               ((heading () #f #f ((str () #f #f "Sub ")) 2 #f)
                (div (("class" . "d")) #f #f ((heading () #f #f ((str () #f #f "Inner")) 1 "Inner"))))
               "Sub"))
            "Hi"))
         () () () #f)))
  (check-equal? "<section id=\"sec\">\n</section>\n"
    (html (list (make-djot-section '() #f #f '() "sec")))))

(test "a generated id goes after array-index keys and before the rest"
  (check-equal? "<div class=\"d\">\n<h1 1=\"one\" id=\"Head\" a=\"b\">Head</h1>\n</div>\n"
    (html (list (make-djot-div '(("class" . "d")) #f #f
                  (list (make-djot-heading '(("1" . "one") ("a" . "b")) #f #f (list (str "Head")) 1 "Head"))))))
  (check-equal? "<h7 1=\"x\" id=\"mine\" a=\"b\">h</h7>\n<h2 0=\"zero\" id=\"auto\" z=\"1\">h</h2>\n"
    (html (list (make-djot-heading '(("1" . "x") ("a" . "b") ("id" . "mine")) #f #f (list (str "h")) 7 "auto")
                (make-djot-heading '(("0" . "zero") ("z" . "1")) #f #f (list (str "h")) 2 "auto"))))
  (check-equal? "<section 1=\"one\" id=\"i\" a=\"b\">\n<h1>Head</h1>\n</section>\n"
    (html (list (make-djot-section '(("1" . "one") ("id" . "i") ("a" . "b")) #f #f
                  (list (make-djot-heading '() #f #f (list (str "Head")) 1 #f))
                  #f)))))

(test "attributes keep their order and are escaped"
  (check-equal? "<p><span id=\"b\" class=\"x y\" k=\"2\">t</span><span class=\"c\">&lt;a&amp;b&gt;</span><code class=\"c\">v</code></p>\n"
    (html (list (para (make-djot-span '(("id" . "b") ("class" . "x y") ("k" . "2")) #f #f (list (str "t")))
                      (make-djot-str '(("class" . "c")) #f #f "<a&b>")
                      (make-djot-verbatim '(("class" . "c")) #f #f "v")))))
  (check-equal? "<p><span k=\"&lt;&quot;&amp;&gt;\">t</span></p>\n"
    (html (list (para (make-djot-span '(("k" . "<\"&>")) #f #f (list (str "t"))))))))

(test "array-index attribute keys come first, in numeric order"
  (check-equal? "<p><span 1=\"y\" 2=\"w\" 10=\"z\" a=\"x\">t</span></p>\n"
    (html (list (para (make-djot-span '(("a" . "x") ("10" . "z") ("2" . "w") ("1" . "y")) #f #f
                        (list (str "t")))))))
  (check-equal? "<div class=\"d\">\n<h1 1=\"y\" 2=\"w\" 10=\"z\" id=\"H\" b=\"x\">H</h1>\n</div>\n"
    (html (list (make-djot-div '(("class" . "d")) #f #f
                  (list (make-djot-heading '(("b" . "x") ("10" . "z") ("2" . "w") ("1" . "y")) #f #f
                          (list (str "H")) 1 "H")))))))

(test "tight lists drop paragraph tags; block quotes and divs restore them"
  (check-equal?
    "<ul>\n<li>\na\n</li>\n<li>\nb\n</li>\n</ul>\n<ol>\n<li>\n<p>x</p>\n</li>\n<li>\n<p>y</p>\n</li>\n</ol>\n<ul class=\"task-list\">\n<li>\n<input disabled=\"\" type=\"checkbox\"/>\nt\n</li>\n<li>\n<input disabled=\"\" type=\"checkbox\" checked=\"\"/>\nu\n</li>\n</ul>\n"
    (datum-html
      '(doc () #f #f
         ((bullet-list () #f #f
            ((list-item () #f #f ((para () #f #f ((str () #f #f "a")))))
             (list-item () #f #f ((para () #f #f ((str () #f #f "b"))))))
            "-" #t)
          (ordered-list () #f #f
            ((list-item () #f #f ((para () #f #f ((str () #f #f "x")))))
             (list-item () #f #f ((para () #f #f ((str () #f #f "y"))))))
            "1." #f 1)
          (task-list () #f #f
            ((task-list-item () #f #f ((para () #f #f ((str () #f #f "t")))) #f)
             (task-list-item () #f #f ((para () #f #f ((str () #f #f "u")))) #t))
            #t))
         () () () #f)))
  (check-equal?
    "<ul>\n<li>\nt\n<blockquote>\n<p>loose</p>\n</blockquote>\n</li>\n</ul>\n<ul class=\"task-list own\" k=\"v\">\n<li>\n<input disabled=\"\" type=\"checkbox\" checked=\"\"/>\n<p>t</p>\n</li>\n</ul>\n"
    (html (list (make-djot-bullet-list '() #f #f
                  (list (make-djot-list-item '() #f #f
                          (list (make-djot-para '(("class" . "p")) #f #f (list (str "t")))
                                (make-djot-block-quote '() #f #f (list (para (str "loose")))))))
                  "-" #t)
                (make-djot-task-list '(("class" . "own") ("k" . "v")) #f #f
                  (list (make-djot-task-list-item '(("class" . "ignored")) #f #f (list (para (str "t"))) #t))
                  #f)))))

(test "ordered lists write start and type"
  (define (item text) (make-djot-list-item '() #f #f (list (para (str text)))))
  (check-equal?
    "<ol start=\"3\">\n<li>\na\n</li>\n</ol>\n<ol type=\"i\">\n<li>\nx\n</li>\n</ol>\n<ol start=\"2\" type=\"A\">\n<li>\ny\n</li>\n</ol>\n<ol>\n<li>\nz\n</li>\n</ol>\n"
    (html (list (make-djot-ordered-list '() #f #f (list (item "a")) "1)" #t 3)
                (make-djot-ordered-list '() #f #f (list (item "x")) "i." #t 1)
                (make-djot-ordered-list '() #f #f (list (item "y")) "A." #t 2)
                (make-djot-ordered-list '() #f #f (list (item "z")) "1." #t 0)))))

(test "footnotes are numbered by first reference and written after the body"
  (check-equal?
    "<p>a<a id=\"fnref1\" href=\"#fn1\" role=\"doc-noteref\"><sup>1</sup></a> c<a id=\"fnref2\" href=\"#fn2\" role=\"doc-noteref\"><sup>2</sup></a> d<a href=\"#fn1\" role=\"doc-noteref\"><sup>1</sup></a> e<a id=\"fnref3\" href=\"#fn3\" role=\"doc-noteref\"><sup>3</sup></a></p>\n<section role=\"doc-endnotes\">\n<hr>\n<ol>\n<li id=\"fn1\">\n<p>note b<a id=\"fnref4\" href=\"#fn4\" role=\"doc-noteref\"><sup>4</sup></a><a href=\"#fnref1\" role=\"doc-backlink\">\x21A9;\xFE0E;</a></p>\n</li>\n<li id=\"fn2\">\n<p>note one</p>\n<blockquote>\n<p>q</p>\n</blockquote>\n<p><a href=\"#fnref2\" role=\"doc-backlink\">\x21A9;\xFE0E;</a></p>\n</li>\n<li id=\"fn3\">\n<p><a href=\"#fnref3\" role=\"doc-backlink\">\x21A9;\xFE0E;</a></p>\n</li>\n<li id=\"fn4\">\n<p>two<a href=\"#fnref4\" role=\"doc-backlink\">\x21A9;\xFE0E;</a></p>\n</li>\n<li id=\"fn5\">\n<p>three<a href=\"#fnref5\" role=\"doc-backlink\">\x21A9;\xFE0E;</a></p>\n</li>\n</ol>\n</section>\n"
    (datum-html
      '(doc () #f #f
         ((para () #f #f
            ((str () #f #f "a") (footnote-reference () #f #f "b") (str () #f #f " c")
             (footnote-reference () #f #f "1") (str () #f #f " d") (footnote-reference () #f #f "b")
             (str () #f #f " e") (footnote-reference () #f #f "missing"))))
         () ()
         (("b" . (footnote () #f #f ((para () #f #f ((str () #f #f "note b") (footnote-reference () #f #f "2")))) "b"))
          ("1" . (footnote () #f #f ((para () #f #f ((str () #f #f "note one")))
                                     (block-quote () #f #f ((para () #f #f ((str () #f #f "q"))))))
                   "1"))
          ("2" . (footnote () #f #f ((para () #f #f ((str () #f #f "two")))) "2"))
          ("u" . (footnote () #f #f ((para () #f #f ((str () #f #f "unreferenced") (footnote-reference () #f #f "3")))) "u"))
          ("3" . (footnote () #f #f ((para () #f #f ((str () #f #f "three")))) "3")))
         #f)))
  (check-equal? "<p>no notes</p>\n"
    (djot->html-string
      (make-djot-doc '() #f #f (list (para (str "no notes"))) '() '()
        (list (cons "a" (make-djot-footnote '() #f #f (list (para (str "unused"))) "a")))
        #f))))

(test "notes are written in the order of JavaScript object keys"
  (check-equal?
    "<p>x<a id=\"fnref1\" href=\"#fn1\" role=\"doc-noteref\"><sup>1</sup></a> y<a id=\"fnref2\" href=\"#fn2\" role=\"doc-noteref\"><sup>2</sup></a></p>\n<section role=\"doc-endnotes\">\n<hr>\n<ol>\n<li id=\"fn1\">\n<p>A<a id=\"fnref4\" href=\"#fn4\" role=\"doc-noteref\"><sup>4</sup></a><a href=\"#fnref1\" role=\"doc-backlink\">\x21A9;\xFE0E;</a></p>\n</li>\n<li id=\"fn2\">\n<p>Five<a id=\"fnref3\" href=\"#fn3\" role=\"doc-noteref\"><sup>3</sup></a><a href=\"#fnref2\" role=\"doc-backlink\">\x21A9;\xFE0E;</a></p>\n</li>\n<li id=\"fn3\">\n<p>X<a href=\"#fnref3\" role=\"doc-backlink\">\x21A9;\xFE0E;</a></p>\n</li>\n<li id=\"fn4\">\n<p>Y<a href=\"#fnref4\" role=\"doc-backlink\">\x21A9;\xFE0E;</a></p>\n</li>\n</ol>\n</section>\n"
    (datum-html
      '(doc () #f #f
         ((para () #f #f
            ((str () #f #f "x") (footnote-reference () #f #f "a") (str () #f #f " y")
             (footnote-reference () #f #f "5"))))
         () ()
         (("a" . (footnote () #f #f ((para () #f #f ((str () #f #f "A") (footnote-reference () #f #f "y")))) "a"))
          ("5" . (footnote () #f #f ((para () #f #f ((str () #f #f "Five") (footnote-reference () #f #f "x")))) "5"))
          ("y" . (footnote () #f #f ((para () #f #f ((str () #f #f "Y")))) "y"))
          ("x" . (footnote () #f #f ((para () #f #f ((str () #f #f "X")))) "x")))
         #f))))

(test "links and images resolve references through the document"
  (check-equal?
    "<p><a href=\"/url\">a</a> <img alt=\"img e\" src=\"/url\"> <a href=\"/b\">b</a> <a>nope</a> <img alt=\"alt\" src=\"src.png\" class=\"k\"></p>\n<section id=\"H-1\">\n<h1>H 1</h1>\n<p><a href=\"#H-1\">H 1</a></p>\n</section>\n"
    (datum-html
      '(doc () #f #f
         ((para () #f #f
            ((link () #f #f ((str () #f #f "a")) #f "x") (str () #f #f " ")
             (image () #f #f ((str () #f #f "img ") (strong () #f #f ((str () #f #f "e")))) #f "x")
             (str () #f #f " ") (link () #f #f ((str () #f #f "b")) #f "b") (str () #f #f " ")
             (link () #f #f ((str () #f #f "nope")) #f "zz") (str () #f #f " ")
             (image (("class" . "k")) #f #f ((str () #f #f "alt")) "src.png" #f)))
          (section () #f #f
            ((heading () #f #f ((str () #f #f "H 1")) 1 #f)
             (para () #f #f ((link () #f #f ((str () #f #f "H 1")) #f "H 1"))))
            "H-1"))
         (("x" . (reference () #f #f "x" "/url")) ("b" . (reference () #f #f "b" "/b")))
         (("H 1" . (reference () #f #f "H 1" "#H-1")))
         () #f)))
  (check-equal? "<p><a href=\"/url\" class=\"r\" k=\"v\">a</a></p>\n"
    (datum-html
      '(doc () #f #f ((para () #f #f ((link () #f #f ((str () #f #f "a")) #f "x"))))
         (("x" . (reference (("class" . "r") ("k" . "v")) #f #f "x" "/url"))) () () #f)))
  (check-equal?
    "<p><a 9=\"n\" href=\"/u?a&amp;b\" class=\"r\" title=\"own\">a</a><img 9=\"n\" alt=\"i\nv\" src=\"/u?a&amp;b\" class=\"r\" title=\"t\"><img><img alt=\"k\"></p>\n"
    (djot->html-string
      (make-djot-doc '() #f #f
        (list (para (make-djot-link '(("title" . "own") ("class" . "")) #f #f (list (str "a")) #f "x")
                    (make-djot-image '() #f #f
                      (list (str "i") (make-djot-soft-break '() #f #f)
                            (make-djot-footnote-reference '() #f #f "q")
                            (make-djot-symb '() #f #f "s")
                            (make-djot-verbatim '() #f #f "v"))
                      "/ignored" "x")
                    (make-djot-image '() #f #f (list (str "j")) #f "none")
                    (make-djot-image '() #f #f (list (str "k")) #f #f)))
        (list (cons "x" (make-djot-reference '(("9" . "n") ("class" . "r") ("title" . "t")) #f #f "x" "/u?a&b")))
        '() '() #f)))
  (check-equal? "<p><a href=\"/url\">a</a></p>\n"
    (djot->html-string
      (make-djot-doc '() #f #f
        (list (para (make-djot-link '() #f #f (list (str "a")) #f " x \n\t y ")))
        (list (cons "x y" (make-djot-reference '() #f #f "x y" "/url")))
        '() '() #f))))

(test "a later entry for a label replaces an earlier one and keeps its position"
  (check-equal? "<p><a href=\"/2\">l</a> <img alt=\"i\" src=\"/2\"></p>\n"
    (djot->html-string
      (make-djot-doc '() #f #f
        (list (para (make-djot-link '() #f #f (list (str "l")) #f "a") (str " ")
                    (make-djot-image '() #f #f (list (str "i")) #f "a")))
        (list (cons "a" (make-djot-reference '() #f #f "a" "/1"))
              (cons "a" (make-djot-reference '() #f #f "a" "/2")))
        '() '() #f)))
  (check-equal?
    "<p>x<a id=\"fnref1\" href=\"#fn1\" role=\"doc-noteref\"><sup>1</sup></a></p>\n<section role=\"doc-endnotes\">\n<hr>\n<ol>\n<li id=\"fn1\">\n<p>A<a id=\"fnref3\" href=\"#fn3\" role=\"doc-noteref\"><sup>3</sup></a><a href=\"#fnref1\" role=\"doc-backlink\">\x21A9;\xFE0E;</a></p>\n</li>\n<li id=\"fn2\">\n<p>d<a href=\"#fnref2\" role=\"doc-backlink\">\x21A9;\xFE0E;</a></p>\n</li>\n<li id=\"fn3\">\n<p>c<a href=\"#fnref3\" role=\"doc-backlink\">\x21A9;\xFE0E;</a></p>\n</li>\n</ol>\n</section>\n"
    (datum-html
      '(doc () #f #f
         ((para () #f #f ((str () #f #f "x") (footnote-reference () #f #f "a"))))
         () ()
         (("b" . (footnote () #f #f ((para () #f #f ((str () #f #f "one") (footnote-reference () #f #f "e")))) "b"))
          ("a" . (footnote () #f #f ((para () #f #f ((str () #f #f "A") (footnote-reference () #f #f "c")))) "a"))
          ("b" . (footnote () #f #f ((para () #f #f ((str () #f #f "two") (footnote-reference () #f #f "d")))) "b"))
          ("c" . (footnote () #f #f ((para () #f #f ((str () #f #f "c")))) "c"))
          ("d" . (footnote () #f #f ((para () #f #f ((str () #f #f "d")))) "d"))
          ("e" . (footnote () #f #f ((para () #f #f ((str () #f #f "e")))) "e")))
         #f))))

(test "inline kinds"
  (check-equal?
    "<p><strong>s</strong> <em>e</em> <mark>m</mark> <sup>sup</sup> <sub>sub</sub> <ins>i</ins> <del>d</del> \x201C;q\x201D; \x2018;q\x2019; \x2013; \x2014; \x2026; :sm: <span class=\"math inline\">\\(x&lt;y\\)</span> <span class=\"math display\">\\[z\\]</span> <a href=\"http://u.x?a&amp;b\">http://u.x?a&amp;b</a> <a href=\"mailto:me@x.org\">me@x.org</a><br>\na&nbsp;b <code>&lt;&amp;&gt;</code> <b> </p>\n"
    (datum-html
      '(doc () #f #f
         ((para () #f #f
            ((strong () #f #f ((str () #f #f "s"))) (str () #f #f " ") (emph () #f #f ((str () #f #f "e")))
             (str () #f #f " ") (mark () #f #f ((str () #f #f "m"))) (str () #f #f " ")
             (superscript () #f #f ((str () #f #f "sup"))) (str () #f #f " ")
             (subscript () #f #f ((str () #f #f "sub"))) (str () #f #f " ")
             (insert () #f #f ((str () #f #f "i"))) (str () #f #f " ") (delete () #f #f ((str () #f #f "d")))
             (str () #f #f " ") (double-quoted () #f #f ((str () #f #f "q"))) (str () #f #f " ")
             (single-quoted () #f #f ((str () #f #f "q"))) (str () #f #f " ")
             (smart-punctuation () #f #f en-dash "--") (str () #f #f " ")
             (smart-punctuation () #f #f em-dash "---") (str () #f #f " ")
             (smart-punctuation () #f #f ellipses "...") (str () #f #f " ") (symb () #f #f "sm")
             (str () #f #f " ") (inline-math () #f #f "x<y") (str () #f #f " ") (display-math () #f #f "z")
             (str () #f #f " ") (url () #f #f "http://u.x?a&b") (str () #f #f " ") (email () #f #f "me@x.org")
             (hard-break () #f #f) (str () #f #f "a") (non-breaking-space () #f #f) (str () #f #f "b ")
             (verbatim () #f #f "<&>") (str () #f #f " ") (raw-inline () #f #f "html" "<b>") (str () #f #f " ")
             (raw-inline () #f #f "latex" "x"))))
         () () () #f)))
  (check-equal? "<p>\x2018;\x2019;\x201C;\x201D;<span class=\"math inline k\">\\(a\\)</span></p>\n"
    (html (list (para (make-djot-smart-punctuation '() #f #f 'left-single-quote "'")
                      (make-djot-smart-punctuation '() #f #f 'right-single-quote "'")
                      (make-djot-smart-punctuation '() #f #f 'left-double-quote "\"")
                      (make-djot-smart-punctuation '() #f #f 'right-double-quote "\"")
                      (make-djot-inline-math '(("class" . "k")) #f #f "a"))))))

(test "block kinds"
  (check-equal?
    "<blockquote>\n<p>q\n- t</p>\n</blockquote>\n<pre><code class=\"language-lua\">x &lt; 1\n</code></pre>\n<pre><code>plain\n</code></pre>\n<div>raw</div>\n<hr>\n<dl>\n<dt>term</dt>\n<dd>\n<p>def</p>\n</dd>\n</dl>\n<table>\n<caption>cap</caption>\n<tr>\n<th style=\"text-align: left;\">a</th>\n<th style=\"text-align: right;\">b</th>\n</tr>\n<tr>\n<td style=\"text-align: left;\">c</td>\n<td style=\"text-align: right;\">d</td>\n</tr>\n</table>\n<table>\n<tr>\n<td>x</td>\n</tr>\n</table>\n"
    (datum-html
      '(doc () #f #f
         ((block-quote () #f #f
            ((para () #f #f ((str () #f #f "q") (soft-break () #f #f) (str () #f #f "- t")))))
          (code-block () #f #f "lua" "x < 1\n")
          (code-block () #f #f #f "plain\n")
          (raw-block () #f #f "html" "<div>raw</div>\n")
          (thematic-break () #f #f)
          (definition-list () #f #f
            ((definition-list-item () #f #f
               ((term () #f #f ((str () #f #f "term")))
                (definition () #f #f ((para () #f #f ((str () #f #f "def")))))))))
          (table () #f #f
            ((caption () #f #f ((str () #f #f "cap")))
             (row () #f #f ((cell () #f #f ((str () #f #f "a")) #t left)
                            (cell () #f #f ((str () #f #f "b")) #t right)) #t)
             (row () #f #f ((cell () #f #f ((str () #f #f "c")) #f left)
                            (cell () #f #f ((str () #f #f "d")) #f right)) #f)))
          (table () #f #f
            ((caption () #f #f ())
             (row () #f #f ((cell () #f #f ((str () #f #f "x")) #f default)) #f))))
         () () () #f)))
  (check-equal?
    "<pre class=\"k\"><code class=\"language-a&quot;b\">&lt;x&gt;</code></pre>\n<hr class=\"t\">\n<div class=\"d\">\n</div>\n<p><span class=\"math display\">\\[c\\]</span></p>\n"
    (html (list (make-djot-code-block '(("class" . "k")) #f #f "a\"b" "<x>")
                (make-djot-raw-block '() #f #f "latex" "\\x")
                (make-djot-thematic-break '(("class" . "t")) #f #f)
                (make-djot-div '(("class" . "d")) #f #f
                  (list (make-djot-reference '() #f #f "x" "y")
                        (make-djot-footnote '() #f #f '() "x")))
                (para (make-djot-display-math '() #f #f "c"))))))

(test "text is written as UTF-8"
  (let ([doc (make-djot-doc '() #f #f (list (para (str "\xE9;\x3B1;\x20AC;\x1F600;<"))) '() '() '() #f)])
    (check-equal? (string->utf8 "<p>\xE9;\x3B1;\x20AC;\x1F600;&lt;</p>\n") (djot->html-bytevector doc))
    (check-equal? "<p>\xE9;\x3B1;\x20AC;\x1F600;&lt;</p>\n" (djot->html-string doc))))

(test "djot-write-html writes to binary and textual ports"
  (let ([doc (make-djot-doc '() #f #f (list (para (str "caf\xE9;"))) '() '() '() #f)])
    (check-equal? (string->utf8 "<p>caf\xE9;</p>\n")
      (call-with-bytevector-output-port (lambda (p) (djot-write-html doc p))))
    (check-equal? "<p>caf\xE9;</p>\n"
      (call-with-string-output-port (lambda (p) (djot-write-html doc p))))))

(test "HTML that begins with U+FEFF keeps it as text"
  (let ([doc (make-djot-doc '() #f #f (list (make-djot-raw-block '() #f #f "html" "\xFEFF;x\n")) '() '() '() #f)])
    (check-equal? (string->utf8 "\xFEFF;x\n") (djot->html-bytevector doc))
    (check-equal? "\xFEFF;x\n" (djot->html-string doc))
    (check-equal? "\xFEFF;x\n" (call-with-string-output-port (lambda (p) (djot-write-html doc p))))))

(test "deep nesting"
  (let ([doc (make-djot-doc '() #f #f
               (let loop ([n 100000] [inner (list (para (str "x")))])
                 (if (= n 0) inner (loop (- n 1) (list (make-djot-block-quote '() #f #f inner)))))
               '() '() '() #f)])
    (check-equal? (+ (* 100000 (string-length "<blockquote>\n</blockquote>\n")) (string-length "<p>x</p>\n"))
      (string-length (djot->html-string doc)))))

(test "bad arguments raise assertion violations"
  (define doc (make-djot-doc '() #f #f '() '() '() '() #f))
  (check-raises assertion-violation? (djot->html-string "text"))
  (check-raises assertion-violation? (djot->html-bytevector (para)))
  (check-raises assertion-violation? (djot-write-html "text" (open-output-string)))
  (check-raises assertion-violation? (djot-write-html doc 'port))
  (check-raises assertion-violation? (djot-write-html doc (open-string-input-port "")))
  (check-raises assertion-violation? (html (list (para "not a node")))))

(test-exit)
