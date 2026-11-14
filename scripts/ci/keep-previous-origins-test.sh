#!/usr/bin/env bash
# keep-previous-origins.sh: a down origin's previous recording is put back, nothing this recording wrote is replaced.
# Run: bash scripts/ci/keep-previous-origins-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m keep-previous-origins.sh\n'
keep="$REPO_ROOT/scripts/ci/keep-previous-origins.sh"
work="$(mktemp -d)"; trap 'rm -rf "$work"' EXIT
tree=test/resources/fixtures/enrichment-pl

# The previous recording, packed as pack-enrichment-tree.sh packs it (the tree's own paths): two Kino Muza pages and
# another origin's response.
mkdir -p "$work/old/$tree/www.kinomuza.pl/movie/a" "$work/old/$tree/www.kinomuza.pl/movie/b" "$work/old/$tree/api.themoviedb.org"
echo old-a > "$work/old/$tree/www.kinomuza.pl/movie/a/index.html"
echo old-b > "$work/old/$tree/www.kinomuza.pl/movie/b/index.html"
echo old-tmdb > "$work/old/$tree/api.themoviedb.org/1.json"
mkdir -p "$work/stage.archive"
( cd "$work/old" && tar -cf - "$tree" | gzip > "$work/stage.archive/enrichment-pl.tar.gz" )

# This recording's tree: the prune took every Kino Muza page but one, which this recording rewrote; TMDB answered anew.
mkdir -p "$work/ws/$tree/www.kinomuza.pl/movie/b" "$work/ws/$tree/api.themoviedb.org"
echo new-b > "$work/ws/$tree/www.kinomuza.pl/movie/b/index.html"
echo new-tmdb > "$work/ws/$tree/api.themoviedb.org/1.json"
out=$(cd "$work/ws" && "$keep" "$work/stage.archive" "$tree" www.kinomuza.pl); code=$?

check "a down origin's pruned page comes back from its previous recording" "old-a" "$(cat "$work/ws/$tree/www.kinomuza.pl/movie/a/index.html" 2>/dev/null)"
check "...one this recording wrote is never replaced" "new-b" "$(cat "$work/ws/$tree/www.kinomuza.pl/movie/b/index.html")"
check "...another origin is left alone" "new-tmdb" "$(cat "$work/ws/$tree/api.themoviedb.org/1.json")"
check "...and it says what it kept" "0 1" "$code $(grep -c 'kept 1 response(s) of its previous recording (2 in the tree)' <<< "$out")"
check "no previous recording (a first one): nothing to keep, no error" "0" \
  "$(mkdir -p "$work/none" && cd "$work/ws" && "$keep" "$work/none" "$tree" www.kinomuza.pl > /dev/null; echo $?)"
check "a host the previous recording never held: nothing kept, no error" "0" \
  "$(cd "$work/ws" && "$keep" "$work/stage.archive" "$tree" www.example.org > /dev/null; echo $?)"

spec_summary
