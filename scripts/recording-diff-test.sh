#!/usr/bin/env bash
# scripts/recording-diff: two cached recordings compared per country — re-spelled listings, the trees' origins judged
# from each run's own start, the manifests kept — and a corpus-only comparison downloading the scrape corpus alone.
# gh is a stub; the recordings are fabricated in a scratch cache.
# Run: bash scripts/recording-diff-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m recording-diff\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
cache="$work/cache"

# recording <run> <cc> <title> <tree origin> <refused?> — a recording in the cache: a one-venue corpus and its tree
recording() {
  local fixtures=$cache/recordings/$1/pair/test/resources/fixtures
  mkdir -p "$fixtures/corpus" "$fixtures/enrichment-$2/$4/path" "$fixtures/enrichment-$2/.enrichment-cache"
  python3 - "$fixtures/corpus/cinema-scrapes-$2.json.gz" "$3" <<'PY'
import gzip, json, sys
films = [{"movie": {"title": sys.argv[2]}, "showtimes": []}]
with gzip.open(sys.argv[1], "wt") as out:
    json.dump([{"_id": "Astor Film Lounge", "films": films}], out)
PY
  for i in $(seq 1 25); do
    echo '{}' > "$fixtures/enrichment-$2/$4/path/r$i"
    if [ "$5" = refused ]; then
      printf 'GET https://%s/x%s\nfailed\n1893456000000\n403\nGET\nrefused' "$4" "$i" | gzip > "$fixtures/enrichment-$2/.enrichment-cache/e$i.entry"
    fi
  done
}
recording 100 de "Lieblingsmenschen - Die  Freundschaft" www.cineworld.co.uk ok
recording 200 de "Lieblingsmenschen - Die Freundschaft" www.cineworld.co.uk refused

cat > "$work/bin/gh" <<STUB
#!/usr/bin/env bash
echo "gh \$*" >> "$work/calls"
case "\$1 \$2" in
  "api repos/pawelkrupinski/movies/actions/runs/100") echo 1970-01-01T00:00:00Z ;;
  "api repos/pawelkrupinski/movies/actions/runs/200") echo 1970-01-01T00:00:00Z ;;
  "run download")
    while [ "\$#" -gt 0 ]; do [ "\$1" = --dir ] && dir=\$2; shift; done
    src=$work/src; rm -rf "\$src"; mkdir -p "\$src/test/resources/fixtures/corpus"
    cp "$cache/recordings/100/pair/test/resources/fixtures/corpus/cinema-scrapes-de.json.gz" "\$src/test/resources/fixtures/corpus/cinema-scrapes-uk.json.gz"
    tar -czf "\$dir/scrapes-uk.tar.gz" -C "\$src" test ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"

diff_() { PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" "$REPO_ROOT/scripts/recording-diff" "$@" 2> "$work/err"; }

out=$(diff_ 100 200 --country DE)
check "a listing re-spelled by whitespace is named so" "1" "$(grep -c 'RE-SPELLED (whitespace) at 1 venue' <<< "$out")"
check "an origin refusing in the later run is flagged" "1" "$(grep -c 'REFUSING MORE *www.cineworld.co.uk: 0/25 (0%) → 25/50 (50%)' <<< "$out")"
check "...each tree's manifest is kept in the cache" "2" "$(find "$cache/recording-diff" -name '*-de.json' | wc -l | tr -d ' ')"
check "...each run's start is asked once" "2" "$(grep -c 'gh api' "$work/calls")"
diff_ 100 200 --country de > /dev/null
check "...and kept: a second comparison asks for nothing" "2" "$(grep -c 'gh api' "$work/calls")"

out=$(diff_ 100 200 --country de --title lieblings)
check "--title shows the listing one by one" "1" "$(grep -c '«Lieblingsmenschen - Die  Freundschaft» → «Lieblingsmenschen - Die Freundschaft»: RE-SPELLED (whitespace), fields alike' <<< "$out")"

rm -f "$work/calls"
mkdir -p "$cache/recordings/300/pair/test/resources/fixtures/corpus"
cp "$cache/recordings/200/pair/test/resources/fixtures/corpus/cinema-scrapes-de.json.gz" "$cache/recordings/300/pair/test/resources/fixtures/corpus/cinema-scrapes-uk.json.gz"
out=$(diff_ 300 400 --country uk --corpus-only)
check "--corpus-only downloads only the missing run's scrape corpus" "gh run download 400 -R pawelkrupinski/movies --name scrape-fixtures-uk" \
  "$(grep -o 'gh run download 400 -R pawelkrupinski/movies --name scrape-fixtures-uk' "$work/calls")"
check "...and compares no origins" "1" "$(grep -c 'ORIGINS: not compared' <<< "$out")"
check "...the two corpora still compared" "1" "$(grep -c 'RE-SPELLED (whitespace) at 1 venue' <<< "$out")"

check "a run id that is not one is refused" "2" "$(diff_ 100 abc > /dev/null; echo $?)"

spec_summary
