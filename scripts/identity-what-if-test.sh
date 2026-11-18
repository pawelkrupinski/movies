#!/usr/bin/env bash
# identity-what-if --bisect: over a scratch repository's commits, each side read from the what-if's kept-side cache (the
# seam a whole resolve fills: <cache>/what-if/<cc>/<sha>-<tools hash>-live/<snapshot>/), it names the first commit whose
# take of the listing differs from the good one's -- and, when good and bad take alike, says the code did not move it.
# No JVM, no mirror: every side is kept, and the mirror's freshness check is a stub.
# Then THE MAIN PATH over stub sbt and java: an inert change starts no head JVM and compiles no head, the base's whole
# side runs in a one-shot JVM (never a kept one), and a head that resolves compiles beside the base's resolve.
# Run: bash scripts/identity-what-if-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-what-if: --bisect, and the main path over stub JVMs\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo"
mkdir -p "$repo/scripts/ci" "$repo/scripts/local-mirror"
cp "$REPO_ROOT"/scripts/identity-what-if "$REPO_ROOT"/scripts/pure-move-check "$REPO_ROOT"/scripts/identity-what-if-roots.tsv "$REPO_ROOT"/scripts/*.sh "$REPO_ROOT"/scripts/*.py "$repo/scripts/"
cp "$REPO_ROOT"/scripts/ci/*.py "$repo/scripts/ci/"
# the tool files the tool keys its sides by (TOOL_FILES), as the checkout holds them
eval "$(sed -n '/^TOOL_FILES=(/,/)/p' "$REPO_ROOT/scripts/identity-what-if")"
for f in "${TOOL_FILES[@]}"; do mkdir -p "$repo/$(dirname "$f")"; cp "$REPO_ROOT/$f" "$repo/$f"; done
printf '#!/usr/bin/env bash\nexit 0\n' > "$repo/scripts/local-mirror/freshness"
chmod +x "$repo/scripts/local-mirror/freshness"
scratch_repo "$repo" -q
commit() { echo "$1" > "$repo/change.txt"; scratch_git "$repo" add -A; scratch_git "$repo" commit -q -m "$1"; scratch_git "$repo" rev-parse HEAD; }
c0=$(commit "good") c1=$(commit "inert") c2=$(commit "moves the take") c3=$(commit "after it")

# the tool files' hash, as the tool keys its kept sides
hash=$(cd "$repo" && bash -c '. scripts/identity-tools.sh; identity_tools_hash "$@"' _ "${TOOL_FILES[@]}")
at=2026-11-05T20:10:00Z since=2026-11-04T20:10:00Z
key=$'Pthe venue\x1ethe film'
side() { # <sha> <snapshot> <model take> <final take>: a kept whole side of that commit
  local d="$work/cache/what-if/uk/$1-$hash-live/$2"
  mkdir -p "$d"
  printf 'key\tvenue\trawTitle\tfamily\ttake\trules\n%s\tThe Venue\tThe Film\t%s\t%s\t\n' "$key" "$key" "$3" > "$d/takes.tsv"
  printf 'key\tfinal\tbasis\n%s\t%s\tAgreed\n' "$key" "$4" > "$d/final.tsv"
  printf 'key\tprod\tbasis\n%s\t%s\t-\n' "$key" "$4" > "$d/prod.tsv"
}
bisect() {
  ( cd "$repo" && KINOWO_IDENTITY_CACHE="$work/cache" KINOWO_JVM_GATE_FOLLOW_MAIN=0 TMDB_API_KEY=stub spec_bounded \
      scripts/identity-what-if --base "$c0" --bisect "$c0" "$c3" --listing 'The Venue|The Film' --at "$at" "$@" uk 2> "$work/err" )
}
side "$c0" "$at" none none; side "$c1" "$at" none none; side "$c2" "$at" tmdb:5 tmdb:5; side "$c3" "$at" tmdb:5 tmdb:5
out=$(bisect)
check "--bisect names the first commit whose take of the listing moved" "FIRST MOVED BY ${c2:0:7} moves the take" \
  "$(grep -o 'FIRST MOVED BY .*' <<<"$out" | sed -E 's/BY ([0-9a-f]{7})[0-9a-f]* /BY \1 /')"
check "...with the take before and after it" "model none, final none|model tmdb:5, final tmdb:5" \
  "$(grep -E '^ +(before|after):' <<<"$out" | sed -E 's/^ +(before|after): +//' | paste -sd'|' -)"

# the code takes alike at good and bad: the corpus moved it, read at --since
side "$c2" "$at" none none; side "$c3" "$at" none none; side "$c3" "$since" tmdb:5 tmdb:5
out=$(bisect --since "$since")
check "...and when no commit moves it, the corpus did: the bad code at --since takes otherwise" "yes" \
  "$(grep -q 'CORPUS: the corpus moved it between' <<<"$out" && echo yes || echo "no: $out $(cat "$work/err")")"


# THE MAIN PATH over stub JVMs: `sbt` prints a classpath, `java` is WhatIfResolve (a whole side, or the head's re-resolve
# of the selection: every listing taken alike), each appending what it ran to $STUB/calls, in order.
mkdir -p "$work/bin"
cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo "sbt-start $(basename "$PWD")" >> "$STUB/calls"; sleep 1
echo "$PWD/worker/target/scala-3.x/test-classes:/lib/a.jar"
echo "sbt-end $(basename "$PWD")" >> "$STUB/calls"
STUB
cat > "$work/bin/java" <<'STUB'
#!/usr/bin/env bash
case " $* " in *" --serve "*) echo "java-serve $(basename "$PWD")" >> "$STUB/calls"; exit 1 ;; esac
out="" select=""
while [ "$#" -gt 0 ]; do case "$1" in --out) out=$2 ;; --select) select=$2 ;; esac; shift; done
echo "java-start $(basename "$PWD")" >> "$STUB/calls"; sleep 2
key=$'Pthe venue\x1ethe film'
mkdir -p "$out"
printf 'key\tvenue\trawTitle\tfamily\ttake\trules\n' > "$out/takes.tsv"
if [ -z "$select" ] || [ -s "$select" ]; then printf '%s\tThe Venue\tThe Film\t%s\ttmdb:5\t\n' "$key" "$key" >> "$out/takes.tsv"; fi
printf 'key\tfinal\tbasis\n%s\ttmdb:5\tAgreed\n' "$key" > "$out/final.tsv"
if [ -z "$select" ]; then
  printf 'family\tblockKeys\n%s\t\n' "$key" > "$out/families.tsv"
  printf 'key\tprod\tbasis\n%s\ttmdb:5\t-\n' "$key" > "$out/prod.tsv"
else
  printf 'selected\tre-resolved\tgroups\trounds\n1\t1\t1\t1\n' > "$out/region.tsv"
fi
echo > "$out/answers.txt"
echo "java-end $(basename "$PWD")" >> "$STUB/calls"
STUB
chmod +x "$work/bin/sbt" "$work/bin/java"
c4=$(echo row > "$repo/common-row.txt"; mkdir -p "$repo/common/src/main/resources"; echo row > "$repo/common/src/main/resources/rows.tsv"
  scratch_git "$repo" add -A; scratch_git "$repo" commit -q -m "a resource the lookups read"; scratch_git "$repo" rev-parse HEAD)
what_if() { # <base> <head> <snapshot> [args...]
  local b=$1 h=$2 s=$3; shift 3; rm -f "$work/calls"
  ( cd "$repo" && STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$work/cache" KINOWO_JVM_GATE_FOLLOW_MAIN=0 \
      KINOWO_GOVERNOR=0 KINOWO_JVM_SLOTS=0 KINOWO_MEMORY_GATE=0 TMDB_API_KEY=stub spec_bounded scripts/identity-what-if \
      --base "$b" --head "$h" --at "$s" --judgements "$REPO_ROOT/test/resources/fixtures/identity-judgements.tsv" "$@" uk 2> "$work/err" )
}
calls() { sed -e "s/$c0/base/" -e "s/$c1/head/" -e "s/$c4/head/" "$work/calls" 2>/dev/null | paste -sd'|' -; }
# A function, not a `case` inside `$( )`: macOS /bin/bash 3.2 reads the pattern's `)` as the substitution's end.
head_compiled_beside_base() {
  local c; c=$(calls)
  case "$c" in *"sbt-start head"*"java-end base"*) echo yes ;; *) echo "no: $c $(cat "$work/err")" ;; esac
}

# an inert change (nothing reaches a take) over a base side not yet kept: the base resolves in a JVM of its own (never a
# kept one, which would hold a JVM slot idle for nothing), and the head is neither compiled nor started
out=$(what_if "$c0" "$c1" 2026-11-06T10:00:00Z)
check "an inert change: the base resolves in a one-shot JVM, the head starts no JVM and compiles nothing" \
  "sbt-start base|sbt-end base|java-start base|java-end base" "$(calls)"
check "...and the what-if is CLEAN, 0 moved" "yes" \
  "$(grep -q 'CLEAN' <<<"$out" && grep -q '0 take(s) moved' <<<"$out" && echo yes || echo "no: $out $(cat "$work/err")")"

# a change reaching every take over a base side not yet kept: the head compiles BESIDE the base's resolve, not after it
out=$(what_if "$c0" "$c4" 2026-11-06T11:00:00Z --no-serve)
check "a change the head resolves: its compile starts before the base's resolve ends" "yes" \
  "$(head_compiled_beside_base)"
check "...and the head resolves once both are done" "java-start head|java-end head" \
  "$(calls | tr '|' '\n' | grep 'java-.* head' | paste -sd'|' -)"

# A PURE MOVE (scripts/pure-move-check) measures nothing: no compile, no JVM, a `pure-move` verdict a country recorded for
# identity-measure's what-if gate
c5=$(mkdir -p "$repo/moved"; scratch_git "$repo" mv common-row.txt moved/common-row.txt; scratch_git "$repo" commit -q -m "a pure move"
  scratch_git "$repo" rev-parse HEAD)
out=$(what_if "$c4" "$c5" 2026-11-06T12:00:00Z)
check "a pure move: what-if says so and is skipped, compiling and starting nothing" \
  "PURE MOVE: what-if skipped (scripts/pure-move-check)|" "$(grep 'PURE MOVE' <<<"$out")|$(calls)"
check "...and records a pure-move verdict for identity-measure's gate" "uk pure-move" \
  "$(cd "$repo" && KINOWO_IDENTITY_CACHE="$work/cache" bash -c '. scripts/identity-tools.sh; what_if_verdicts "$1" "$2"' _ "$c5" "$c4")"
# ...and a change that is no move still resolves (the inert one above was no move either, and resolved)
check "a range that moves nothing says so and resolves as before" "yes" \
  "$(out=$(what_if "$c0" "$c1" 2026-11-06T10:00:00Z)
     grep -q 'not a pure move (scripts/pure-move-check exit 1' "$work/err" && grep -q CLEAN <<<"$out" && echo yes || echo "no: $(cat "$work/err")")"

spec_summary
