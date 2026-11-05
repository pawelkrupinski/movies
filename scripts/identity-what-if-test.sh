#!/usr/bin/env bash
# identity-what-if --bisect: over a scratch repository's commits, each side read from the what-if's kept-side cache (the
# seam a whole resolve fills: <cache>/what-if/<cc>/<sha>-<tools hash>-live/<snapshot>/), it names the first commit whose
# take of the listing differs from the good one's -- and, when good and bad take alike, says the code did not move it.
# No JVM, no mirror: every side is kept, and the mirror's freshness check is a stub.
# Run: bash scripts/identity-what-if-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-what-if --bisect\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo"
mkdir -p "$repo/scripts/ci" "$repo/scripts/local-mirror"
cp "$REPO_ROOT"/scripts/identity-what-if "$REPO_ROOT"/scripts/*.sh "$REPO_ROOT"/scripts/*.py "$repo/scripts/"
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

spec_summary
