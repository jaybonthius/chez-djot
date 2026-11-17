import { readFileSync, writeFileSync } from "fs";
import { resolve } from "path";

const [djotjs, commit, input, prefix] = process.argv.slice(2);
const { parse, renderHTML } = await import(resolve(djotjs));
const doc = parse(readFileSync(input, "utf8"), { warn: () => {} });

const str = (s) => {
  let r = '"';
  for (const ch of s) {
    const c = ch.codePointAt(0);
    if (c >= 0xd800 && c < 0xe000) throw new Error("lone surrogate in " + input);
    if (ch === '"') r += '\\"';
    else if (ch === "\\") r += "\\\\";
    // Chez's reader turns these, and CR LF, into a newline inside a string.
    else if (c < 0x20 || c === 0x85 || c === 0x2028 || c === 0x2029)
      r += "\\x" + c.toString(16) + ";";
    else r += ch;
  }
  return r + '"';
};
const opt = (s) => (s === undefined ? "#f" : str(s));
const bool = (b) => (b ? "#t" : "#f");
const sym = (s) => s.replace(/_/g, "-");

const fields = {
  heading: (n) => [n.level, autoId(n)],
  section: (n) => [autoId(n)],
  code_block: (n) => [opt(n.lang), str(n.text)],
  raw_block: (n) => [str(n.format), str(n.text)],
  bullet_list: (n) => [str(n.style), bool(n.tight)],
  ordered_list: (n) => [str(n.style), bool(n.tight), n.start === undefined ? "#f" : n.start],
  task_list: (n) => [bool(n.tight)],
  task_list_item: (n) => [bool(n.checkbox === "checked")],
  row: (n) => [bool(n.head)],
  cell: (n) => [bool(n.head), n.align],
  reference: (n) => [str(n.label), str(n.destination)],
  footnote: (n) => [str(n.label)],
  str: (n) => [str(n.text)],
  symb: (n) => [str(n.alias)],
  verbatim: (n) => [str(n.text)],
  raw_inline: (n) => [str(n.format), str(n.text)],
  inline_math: (n) => [str(n.text)],
  display_math: (n) => [str(n.text)],
  url: (n) => [str(n.text)],
  email: (n) => [str(n.text)],
  footnote_reference: (n) => [str(n.text)],
  smart_punctuation: (n) => [sym(n.type), str(n.text)],
  link: (n) => [opt(n.destination), opt(n.reference)],
  image: (n) => [opt(n.destination), opt(n.reference)],
};
const known = new Set(["tag", "attributes", "autoAttributes", "children", "pos",
  "level", "lang", "text", "format", "style", "tight", "start", "checkbox", "head",
  "align", "label", "destination", "reference", "alias", "type"]);

// The writer wraps a str in a span when it has any attributes object, even
// an empty one, which an association list cannot tell from none.
const attributes = (n) => {
  const a = n.attributes || {};
  if (n.tag === "str" && n.attributes && Object.keys(a).length === 0)
    throw new Error("str with an empty attributes object");
  return "(" + Object.keys(a).map((k) => `(${str(k)} . ${str(a[k])})`).join(" ") + ")";
};
const autoId = (n) => {
  const a = n.autoAttributes || {};
  for (const k of Object.keys(a)) if (k !== "id") throw new Error("auto attribute " + k);
  return opt(a.id);
};

const node = (n) => {
  for (const k of Object.keys(n)) if (!known.has(k)) throw new Error(`${n.tag}.${k}`);
  if (n.autoAttributes && !(n.tag === "heading" || n.tag === "section"))
    throw new Error("autoAttributes on " + n.tag);
  const parts = [sym(n.tag), attributes(n), "#f", "#f"];
  if (n.children) parts.push("(" + n.children.map(node).join(" ") + ")");
  if (fields[n.tag]) parts.push(...fields[n.tag](n));
  return "(" + parts.join(" ") + ")";
};
const entries = (o) =>
  "(" + Object.entries(o).map(([k, v]) => `(${str(k)} . ${node(v)})`).join(" ") + ")";

writeFileSync(prefix + ".ast",
  `; djot.js ${commit}\n(doc ${attributes(doc)} #f #f (${doc.children.map(node).join(" ")}) ` +
  `${entries(doc.references)} ${entries(doc.autoReferences)} ${entries(doc.footnotes)} #f)\n`);
writeFileSync(prefix + ".html", renderHTML(doc));
