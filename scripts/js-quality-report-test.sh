#!/usr/bin/env bash
# scripts/js-quality-report over a small fixture checkout: every measure's over-limit row is found (cognitive and
# cyclomatic complexity, inner defs, function and file length, a clone group, fan-out, a module cycle, a directory
# cycle, a directory's distance), what must not count does not (callbacks as inner defs, two files importing alike as
# a clone), and the classic-script globals graph resolves `/* global */` names to their `/* exported */` files.
#
# Two legs: the coupling and clone-grouping logic needs node alone and always runs; the end-to-end leg needs the
# pinned tools (`npm ci`, outside the checkout: `js-quality-report --tools-dir`), so it runs where they are installed or
# npm can install them -- not in run-script-tests' --linux container, which has node but no npm.
# Run: bash scripts/js-quality-report-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m js-quality-report\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# --- the logic, node alone ---
out="$(cd "$REPO_ROOT/scripts/js-quality" && node -e '
const { cycles, globalEdges, measureCoupling } = require("./coupling");
const { groupPairs } = require("./clones");
const edges = new Map([["a", new Set(["b"])], ["b", new Set(["a"])], ["c", new Set(["a"])]]);
console.log("cycles " + JSON.stringify(cycles(["a", "b", "c"], edges)));
const texts = new Map([
  ["s/x.js", "/* global bar */\n/* exported foo */\nfunction foo() {}\n"],
  ["s/y.js", "/* global foo, PAGE_CONST */  // x.js\n/* exported bar,\n   baz */\n"],
  ["p/t.js", "/* global baz, foo:writable */\n"],
]);
const g = globalEdges([...texts.keys()], [...texts.keys()], texts);
console.log("globals " + [...g.edges].map(([f, s]) => f + ">" + [...s].join("+")).join(" ") + " templates " + g.templateNames.get("s/y.js"));
const m = measureCoupling(g.edges, texts, g.templateNames);
console.log("module cycles " + JSON.stringify(m.moduleCycles) + " dirs " + m.packages.map((p) => p.dir + ":" + p.ca + "/" + p.ce).join(" "));
const pairs = [{ tokens: 60, copies: [{ file: "a", start: 1, end: 9 }, { file: "b", start: 3, end: 11 }] },
               { tokens: 70, copies: [{ file: "b", start: 3, end: 11 }, { file: "c", start: 5, end: 13 }] }];
console.log("groups " + JSON.stringify(groupPairs(pairs).map((group) => [group.tokens, group.copies.map((copy) => copy.file).join("")])));
' 2>&1)"
check "a cycle is its strongly connected component, a node outside it none" 'cycles [["a","b"]]' "$(grep '^cycles' <<< "$out")"
check "/* global */ names resolve to the /* exported */ (or top-level function) file; others are template globals" \
  "globals s/x.js>s/y.js s/y.js>s/x.js p/t.js>s/y.js+s/x.js templates 1" "$(grep '^globals' <<< "$out")"
check "...and form a module cycle; each directory's Ca/Ce" 'module cycles [["s/x.js","s/y.js"]] dirs s:1/0 p:0/2' \
  "$(grep '^module cycles' <<< "$out")"
check "pairs sharing a copy are one clone group, at its largest token count" 'groups [[70,"abc"]]' "$(grep '^groups' <<< "$out")"

# --- where the tools live: outside the checkout (a node_modules left in it was read by the next test's `grep -r`,
# Order independence #99), keyed by the manifest and its lock ---
tools_dir="$("$REPO_ROOT/scripts/js-quality-report" --tools-dir)"
lock_key="${tools_dir##*/}"
where_is() { case "$1" in "$REPO_ROOT"/*) echo inside ;; *) echo outside ;; esac; } # never a case inside $( ): 3.2 misparses it
node_modules_before="$([ -e "$REPO_ROOT/scripts/js-quality/node_modules" ] && echo present || echo absent)"
check "the tools install outside the checkout, under ~/.cache/kinowo/js-quality/<lock hash>" "outside $HOME/.cache/kinowo/js-quality/ 16 hex" \
  "$(where_is "$tools_dir") $(env -u KINOWO_TOOL_CACHE -u XDG_CACHE_HOME "$REPO_ROOT/scripts/js-quality-report" --tools-dir | sed 's#[^/]*$##') $(printf '%s' "$lock_key" | grep -cxE '[0-9a-f]{16}' | sed 's/^1$/16 hex/')"
check "...\$KINOWO_TOOL_CACHE moves them" "$work/cache/js-quality/$lock_key" "$(KINOWO_TOOL_CACHE="$work/cache" "$REPO_ROOT/scripts/js-quality-report" --tools-dir)"

# the pre-push hook's `jsquality` check leaves the install to the report: its own `npm ci` in scripts/js-quality put
# node_modules in every landing check's tree after the report had moved out
check "scripts/hooks/pre-push installs no js-quality tools in the tree" "0" \
  "$(sed -n '/^check_jsquality()/,/^}/p' "$REPO_ROOT/scripts/hooks/pre-push" | grep -v '^[[:space:]]*#' | grep -c 'npm ci')"

# --- end to end, the pinned tools ---
if [ ! -x "$tools_dir/node_modules/.bin/eslint" ] && ! command -v npm > /dev/null; then
  echo "  (end-to-end leg not run: the tools are not installed and npm cannot install them here)"
  spec_summary
fi

fixture="$work/checkout"
mkdir -p "$fixture/app/core" "$fixture/app/feature" "$fixture/app/types" "$fixture/web/shared" "$fixture/hub"
branchy() { # <name>: a function of six `if`s each holding one, cognitive 6 × (1 + 2) = 18, cyclomatic 13
  printf 'export function %s(a, b) {\n' "$1"
  for i in 1 2 3 4 5 6; do printf '  if (a > %d) { if (b > %d) { a += 1; } }\n' "$i" "$i"; done
  printf '  return a;\n}\n'
}
{
  printf "import { helper } from '../feature/f.js';\n"
  branchy complicated
  printf 'export function holder(list) {\n  function one() { return 1; }\n  const two = () => 2;\n  const three = function () { return 3; };\n'
  printf '  list.forEach((x) => x); list.map(function (y) { return y; });\n  return one() + two() + three() + helper();\n}\n'
  printf 'export function long() {\n'; for i in $(seq 1 55); do printf '  let v%d = %d;\n' "$i" "$i"; done; printf '}\n'
} > "$fixture/app/core/c.js"
printf "import { complicated } from '../core/c.js';\nexport function helper() { return complicated(1, 2); }\n" > "$fixture/app/feature/f.js"
for i in $(seq 1 600); do printf '// line %d\n' "$i"; done > "$fixture/app/core/big.js"
cloned='export function render(items) {
  const rows = [];
  for (const item of items) {
    const cell = { id: item.id, name: item.name, value: item.value * 2, label: `${item.name}: ${item.value}` };
    rows.push(cell);
  }
  return rows.filter((row) => row.value > 10).map((row) => row.label).join(", ");
}'
imports="$(for i in $(seq 1 12); do printf "import { name%d, other%d } from './module-%d.js';\n" "$i" "$i" "$i"; done)"
printf '%s\n%s\n' "$imports" "$cloned" > "$fixture/app/core/clone-a.js"
printf '%s\nexport const x = 1;\n' "$imports" > "$fixture/app/feature/imports-only-a.js"
printf '%s\nexport const y = 2;\n' "$imports" > "$fixture/app/feature/imports-only-b.js"
printf '%s\n' "$cloned" > "$fixture/app/feature/clone-b.js"
printf 'export interface Shape { side: number }\nexport type Id = string;\n' > "$fixture/app/types/shape.ts"
for i in $(seq 1 10); do printf 'export const leaf%d = %d;\n' "$i" "$i" > "$fixture/hub/leaf$i.mjs"; done
{ for i in $(seq 1 10); do printf "import { leaf%d } from './leaf%d.mjs';\n" "$i" "$i"; done; printf 'export const all = [leaf1];\n'; } > "$fixture/hub/hub.mjs"
{ for outer in one two; do
    printf 'export function %s() {\n  function twin() {\n' "$outer"; for i in $(seq 1 55); do printf '    // %s %d\n' "$outer" "$i"; done
    printf '  }\n  return twin;\n}\n'; done; } > "$fixture/app/feature/twins.js"
printf '/* global second */  // b.js\n/* exported first */\nfunction first() { return second(); }\n' > "$fixture/web/shared/a.js"
printf '/* global first, FROM_TEMPLATE */\n/* exported second */\nfunction second() { return first() + FROM_TEMPLATE; }\n' > "$fixture/web/shared/b.js"

out="$("$REPO_ROOT/scripts/js-quality-report" --root "$fixture" --out "$work/out" \
  --area app=app --area 'web:globals=web' --area hub=hub 2>&1)"; status=$?
check "the report runs and prints its summary table" "0 1" "$status $(grep -c '^| app (imports) |' <<< "$out")"
row() { grep -E "^$1	" "$work/out/over-limit.tsv" | cut -f2,3 | LC_ALL=C sort | tr '\n' ';'; }
check "cognitive complexity over 10" "app/core/c.js:complicated	18;" "$(row cognitive)"
check "...cyclomatic" "app/core/c.js:complicated	13;" "$(row cyclomatic)"
check "inner defs: the three named ones, not the two callbacks" "app/core/c.js:holder	3;" "$(row inner-defs)"
check "a function over 50 lines, keyed by file and name (no line: an edit above never re-keys it), a later same-named one #2" "app/core/c.js:long	57;app/feature/twins.js:twin	57;app/feature/twins.js:twin#2	57;" \
  "$(row method-lines | sed -E 's/app.feature.twins.js:(one|two)	[0-9]+;//g')"
check "every limit is a row of its own, the ratchet's to read" \
  "clone-tokens	50;cognitive	10;copies	0;cycle	0;cyclomatic	10;distance	0.2;fanout	9;file-lines	500;inner-defs	2;method-lines	50;" \
  "$(row limit)"
check "a file over 500 lines" "app/core/big.js	600;" "$(row file-lines)"
check "the one clone group (its files joined by |, its copies), the files importing alike none" \
  "app/core/clone-a.js|app/feature/clone-b.js	2;" "$(row copies)"
check "a module importing 10 modules is over the fan-out of 9" "hub/hub.mjs	10;" "$(row fanout)"
check "the module cycles (imports and globals) and the directory cycle" \
  "app/core/c.js|app/feature/f.js	2;app/core|app/feature	2;web/shared/a.js|web/shared/b.js	2;" "$(row cycle)"
check "the globals graph counts a template-only name apart" \
  "module	web	web/shared/b.js	fan-out=1 template-globals=1" "$(grep 'web/shared/b.js' "$work/out/coupling.tsv")"
check "a type-only TypeScript module is abstract (A = 1 in its directory)" "A=1.00" \
  "$(grep 'directory	app	app/types' "$work/out/coupling.tsv" | grep -o 'A=[0-9.]*')"
# (a checkout from before may hold an old install's node_modules: none is made, nor one there removed)
check "...and the run made no node_modules in the checkout" "$node_modules_before" \
  "$([ -e "$REPO_ROOT/scripts/js-quality/node_modules" ] && echo present || echo absent)"
check "--area with a bad shape is refused" "2" \
  "$("$REPO_ROOT/scripts/js-quality-report" --root "$fixture" --area 'bad shape' > /dev/null 2>&1; echo $?)"

spec_summary
