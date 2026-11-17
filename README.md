# chez-djot

A [Djot](https://djot.net) parser and HTML writer for threaded Chez Scheme 10.4.1 that gives the tree and the HTML of [djot.js](https://github.com/jgm/djot.js) at commit `0043d73`.

```scheme
(import (chezscheme) (djot))

(define doc (string->djot "# Hi\n\nSome _text_ and [a link](https://x.org).\n"))

(djot->html-string doc)
;; "<section id=\"Hi\">\n<h1>Hi</h1>\n<p>Some <em>text</em> and <a href=\"https://x.org\">a link</a>.</p>\n</section>\n"

(djot-section-auto-id (car (djot-node-children doc)))
;; "Hi"
```

## Building

The build needs threaded Chez Scheme 10.4.1; the library needs nothing else. djot.js is a git submodule that holds the test files, so clone with `--recursive`.

```sh
git clone --recursive <this repository>
make SCHEME=/path/to/chez-10.4.1/scheme build
make SCHEME=/path/to/chez-10.4.1/scheme test
make SCHEME=/path/to/chez-10.4.1/scheme corpus
make SCHEME=/path/to/chez-10.4.1/scheme bench
```

| Target | What it does | What it needs |
|---|---|---|
| `make build` | compiles the library, the tests and the benchmark into `build/obj`. A compiler warning fails the build | Chez |
| `make test` | runs each `tests/*-test.sps` in its own process: the 317 cases in djot.js's 25 test files (all but `filters.test`, which tests djot.js's filters), djot.js's pathological inputs, and checks of constructors, positions, threads and the writer. `tests/data` holds djot.js's AST and HTML for its `bench/readme.dj`, which the parser and the writer must match | Chez and the submodule; no network |
| `make corpus` | fetches the Pandoc manual and a Wikipedia article on tartan from djot-implementations into `build/data`, checks them against `bench/data.sha256`, makes djot.js's AST and HTML of each, and checks that the parser gives djot.js's tree and HTML, that the writer gives djot.js's HTML from djot.js's tree, and that each node's range lies within its parent's | Chez, the submodule, git, `curl`, Node.js, npm and network access |
| `make bench` | times `string->djot`, `djot->html-bytevector`, conversion from bytes and `utf8->string` on djot.js's `bench/readme.dj` and on the files `make corpus` fetched, with bytes allocated and the share spent collecting | Chez and the submodule |
| `make test-data` | remakes `tests/data`, naming the djot.js commit on the AST file's first line. It compiles djot.js from the submodule in `build/djot.js` with TypeScript 4.9.4, the version djot.js pins and the only package installed, and converts with `tests/djot-ast.mjs` | Node.js, npm and network access |

`CAP` prefixes every build, test and benchmark process, for example `CAP="systemd-run --user --scope -q -p MemoryMax=8G"`.

Programs import `(djot)` with `--libdirs /path/to/chez-djot/src::/path/to/chez-djot/build/obj/src`.

## API

| Form | What it does |
|---|---|
| `(string->djot string)` | parses a document. Every string is a valid document, so parsing never fails |
| `(bytevector->djot bytevector)` | decodes UTF-8 as `utf8->string` does, invalid bytes becoming U+FFFD, skips one leading byte order mark, and parses |
| `(djot-write-html doc port)` | writes the document's HTML to `port`: UTF-8 to a binary port, text to a textual one |
| `(djot->html-string doc)`, `(djot->html-bytevector doc)` | the HTML as a string or as UTF-8 bytes |
| `(djot-doc-reference doc label)` | the `reference` node for `label`, normalised as djot.js does, from the document's `references` and then its `auto-references`; `#f` if there is none. The writer resolves links through it |
| `(djot-line-column doc offset)` | two values, the line and the column, both from 1, of a character offset into the document's `source` |
| `(djot-node? x)` | whether `x` is a node of any kind |
| `(djot-node-attributes node)` | the node's attributes |
| `(djot-node-start node)`, `(djot-node-end node)` | the node's source range |
| `(djot-node-children node)` | the node's children; `()` for a leaf |
| `(make-djot-K attributes start end [children] field ...)` | makes a node of kind `K`, from the [AST](#ast) table. `children` is there only for a container |
| `(djot-K? x)`, `(djot-K-field node)` | the predicate and each field accessor of kind `K` |

Every procedure checks its arguments and raises `assertion-violation` naming itself. The library has no global mutable state, so threads may parse and write at once.

## AST

A document is a tree of records, one sealed record type per djot.js node kind. Each kind `K` is djot.js's tag with `_` written `-`. No exported procedure changes a node.

Every node has these fields:

- `attributes`: an association list of string pairs, `()` for none.
- `start` and `end`: a half-open range of character offsets into the document's `source`, or `#f`, as in nodes built by user code.
- `children`, in a container only: a list of nodes, `()` for none.

Each kind's own fields follow those in its constructor, in this order:

| Kind | Container | Own fields |
|---|---|---|
| `doc` | yes | `references`, `auto-references`: association lists from label to `reference` node; `footnotes`: from label to `footnote` node; `source`: the parsed string, or `#f` |
| `para`, `div`, `block-quote`, `list-item`, `definition-list`, `definition-list-item`, `term`, `definition`, `table`, `caption` | yes | none |
| `heading` | yes | `level`: a positive fixnum; `auto-id`: a string or `#f` |
| `section` | yes | `auto-id`: a string or `#f` |
| `thematic-break` | no | none |
| `code-block` | no | `lang`: a string or `#f`; `text` |
| `raw-block`, `raw-inline` | no | `format`, `text` |
| `bullet-list` | yes | `style`: `"-"`, `"+"` or `"*"`; `tight?` |
| `ordered-list` | yes | `style`: the marker's form, such as `"1."`, `"a)"` or `"(I)"`; `tight?`; `number`: an exact integer or `#f` |
| `task-list` | yes | `tight?` |
| `task-list-item` | yes | `checked?` |
| `row` | yes | `head?` |
| `cell` | yes | `head?`; `align`: one of the symbols `default`, `left`, `right`, `center` |
| `reference` | no | `label`, `destination` |
| `footnote` | yes | `label` |
| `str`, `verbatim`, `inline-math`, `display-math`, `url`, `email` | no | `text` |
| `soft-break`, `hard-break`, `non-breaking-space` | no | none |
| `symb` | no | `alias` |
| `footnote-reference` | no | `label` |
| `smart-punctuation` | no | `type`: one of the symbols `left-single-quote`, `right-single-quote`, `left-double-quote`, `right-double-quote`, `ellipses`, `em-dash`, `en-dash`; `text` |
| `link`, `image` | yes | `destination`: a string or `#f`; `reference`: the normalised label, or `#f` |
| `emph`, `strong`, `span`, `mark`, `superscript`, `subscript`, `insert`, `delete`, `double-quoted`, `single-quoted` | yes | none |

Fields not described are strings, and fields ending in `?` are booleans.

In a parsed tree, attribute names and values, and smart-punctuation texts, are immutable strings, which nodes share, so `string-set!` on one raises.

## Data mapping

| djot.js | `(djot)` |
|---|---|
| `tag` and optional fields on one object | a record type per kind; an absent optional field is `#f` |
| `attributes`, an object of strings | an association list of string pairs. Each key keeps its first position and its last value; classes are joined with a space |
| `pos`: line, column and UTF-16 offset of the start and of the inclusive end | `start` and an exclusive `end`, in characters; `djot-line-column` gives the line and column |
| `autoAttributes` | `auto-id` on `heading` and `section`: the id made from the heading's text when it has none of its own. When the heading's parent is the document or a section, the id belongs to the section made for it, and the heading's `auto-id` is `#f` |
| `references`, `autoReferences`, `footnotes`: objects | association lists, one entry per label, in the order labels first appear, each holding the label's last definition |
| `tight`, `head` | `tight?`, `head?` |
| `checkbox`: `"checked"` or `"unchecked"` | `checked?`: `#t` or `#f` |
| `align`, smart punctuation `type`: strings | symbols, `_` written `-` |
| `ordered_list.start` | `number` |
| `footnote_reference.text` | `label` |

A label given more than once in a document's `references`, `auto-references` or `footnotes` resolves to its last entry, which takes the first entry's place in the order, as in djot.js's objects. These lists, and attribute lists, keep source order even for labels and names that are array indexes (`"2"`), which djot.js's objects list first, in numeric order. The writer follows djot.js's order where it shows: it writes such attributes first, and numbers notes that only other notes refer to in that order.

## Customising the HTML

The library has no hook into the writer and no tree transforms. To change the HTML, build a new tree with the constructors and write it, or write another writer over the tree. The example below writes Scheme code blocks with the caller's `highlight`, which returns HTML; a `raw-block` of format `"html"` is written as it is:

```scheme
(define (replace node)
  (cond
    [(and (djot-code-block? node) (equal? (djot-code-block-lang node) "scheme"))
     (make-djot-raw-block '() (djot-node-start node) (djot-node-end node) "html"
       (highlight (djot-code-block-text node)))]
    [(djot-section? node)
     (make-djot-section (djot-node-attributes node) (djot-node-start node) (djot-node-end node)
       (map replace (djot-node-children node)) (djot-section-auto-id node))]
    [else node]))

(define (replace-code doc)
  (make-djot-doc (djot-node-attributes doc) (djot-node-start doc) (djot-node-end doc)
    (map replace (djot-node-children doc))
    (djot-doc-references doc) (djot-doc-auto-references doc) (djot-doc-footnotes doc)
    (djot-doc-source doc)))
```

It replaces code blocks in the document and its sections; one inside a list, a quote or a div needs that kind rebuilt too.

## Differences from djot.js

The parser follows djot.js where it and Djot's syntax description disagree. It differs from djot.js in these ways:

- `bytevector->djot` skips a leading byte order mark, which marks the encoding of the bytes and is not text. `string->djot` keeps one as text, as djot.js does.
- Positions count characters (code points); djot.js counts UTF-16 code units, so the two differ after any character above U+FFFF.
- A node's range is djot.js's with its inclusive end made exclusive. djot.js gives a range that can end on the newline it adds to a text that lacks one; here such a range ends at the end of the text.
- djot.js gives no range to a term, a definition, or the empty caption of a table that has none. Here a term has its paragraph's range; a definition runs from its first block, or the term's end, to its item's end; and the empty caption has an empty range at its table's start.
- An ordered list's start number is an exact integer; djot.js's is a JavaScript number, which loses digits beyond 2^53.

Two djot.js behaviours that may surprise:

- A div that its closing fence ends stops at the fence, but blocks still open inside it end with the fence's line, so they can end after the div, up to that line's end.
- A section ends where the heading that closes it ends.

## Licences

djot.js, whose test files and `bench/readme.dj` the tests use, is MIT-licensed, by John MacFarlane; `tests/data` is made from it. `make corpus` fetches the Pandoc manual, part of Pandoc (GPL), and Wikipedia text (CC BY-SA) into `build/data`; neither is committed.
