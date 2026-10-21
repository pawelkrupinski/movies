#!/usr/bin/env bash
# identity-fetch-recording.sh against a stub `gh` holding one recording run's assets and artifact.
# Run: bash scripts/identity-fetch-recording-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-fetch-recording.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/assets" "$work/artifacts/scrape-fixtures-uk"
# pack <archive> <path under test/resources/fixtures> <content> <compressor>
pack() {
  local src; src="$(mktemp -d)"
  mkdir -p "$src/test/resources/fixtures/$(dirname "$2")"
  printf '%s\n' "$3" > "$src/test/resources/fixtures/$2"
  ( cd "$src" && tar -cf - test | $4 -q -c > "$1" )
  rm -rf "$src"
}
pack "$work/assets/enrichment-uk-77.tar.zst" enrichment-uk/api.themoviedb.org/search tree zstd
pack "$work/assets/identity-overlay-uk-77.tar.gz" enrichment-uk/identity/answers overlay gzip
pack "$work/artifacts/scrape-fixtures-uk/scrapes-uk.tar.gz" corpus/cinema-scrapes-uk.json.gz corpus gzip

# `release download` hands over the asset its --pattern names when the release holds it; `run download`
# copies the named artifact's files into --dir.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
cmd="$1 $2"; shift 2
tag=""; [ "$cmd" = "release download" ] && { tag="$1"; shift; }
while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; --pattern) pattern="$2" ;; --name) name="$2" ;; esac; shift; done
case "$cmd" in
  "release download")
    [ -f "$STUB/down" ] && { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
    [ -f "$STUB/blip" ] && { rm -f "$STUB/blip"; echo "HTTP 500: Internal Server Error" >&2; exit 1; }
    assets="$STUB/assets"; [ "$tag" = convergence-fixtures ] || assets="$STUB/assets-$tag"
    [ -f "$assets/$pattern" ] || { echo "no assets match the file pattern" >&2; exit 1; }
    mkdir -p "$dir"; cp "$assets/$pattern" "$dir/" ;;
  "run download")     [ -f "$STUB/run-blip" ] && { rm -f "$STUB/run-blip"; mkdir -p "$dir"; echo half > "$dir/scrapes-uk.tar.gz"; echo "HTTP 502" >&2; exit 1; }
                      [ -d "$STUB/artifacts/$name" ] || { echo "no artifact matches any of the names or patterns provided" >&2; exit 1; }; mkdir -p "$dir"; cp "$STUB/artifacts/$name/"* "$dir/" ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"

fetch() { STUB="$work" PATH="$work/bin:$PATH" GH_DOWNLOAD_BACKOFF=0 bash "$REPO_ROOT/scripts/identity-fetch-recording.sh" "$@" > "$work/out" 2>&1; echo $?; }

check "a run's pair is laid out" "0" "$(fetch 77 "$work/dest" uk)"
pair="$work/dest/pair/test/resources/fixtures"
check "...its enrichment tree, unpacked from zstd" "tree" "$(cat "$pair/enrichment-uk/api.themoviedb.org/search" 2>/dev/null)"
check "...its identity overlay, unpacked from gzip" "overlay" "$(cat "$pair/enrichment-uk/identity/answers" 2>/dev/null)"
check "...and its scrape corpus" "corpus" "$(cat "$pair/corpus/cinema-scrapes-uk.json.gz" 2>/dev/null)"

check "--corpus-only lays out the scrape corpus alone" "0 corpus no-tree" \
  "$(fetch --corpus-only 79 "$work/corpus-only" uk) $(cat "$work/corpus-only/pair/test/resources/fixtures/corpus/cinema-scrapes-uk.json.gz") \
$([ -d "$work/corpus-only/pair/test/resources/fixtures/enrichment-uk" ] && echo tree || echo no-tree)"
# a download an earlier run left behind (cut short) does not stop this one
mkdir -p "$work/dest/archive/scrape-uk"; echo junk > "$work/dest/archive/scrape-uk/scrapes-uk.tar.gz"
check "a scrape download an earlier run left behind is fetched again" "0 corpus" \
  "$(fetch 77 "$work/dest" uk) $(cat "$pair/corpus/cinema-scrapes-uk.json.gz" 2>/dev/null)"
# a scrape archive without the country's corpus file lays out no corpus: identity_recorded reads the file as "laid out whole"
pack "$work/artifacts/scrape-fixtures-uk/scrapes-uk.tar.gz" corpus/other.json other gzip
check "a scrape archive holding no cinema-scrapes-<cc>.json.gz fails, laying no corpus out" "1 none" \
  "$(fetch --corpus-only 80 "$work/no-corpus" uk) $(ls "$work/no-corpus/pair/test/resources/fixtures/corpus" 2>/dev/null | head -1 || true)none"
check "...naming what is missing" "1" "$(grep -c 'holds no cinema-scrapes-uk.json.gz' "$work/out")"
pack "$work/artifacts/scrape-fixtures-uk/scrapes-uk.tar.gz" corpus/cinema-scrapes-uk.json.gz corpus gzip
printf '#!/usr/bin/env bash\nsleep 20\n' > "$work/bin/slow-gh"; chmod +x "$work/bin/slow-gh"
cp "$work/bin/gh" "$work/gh.real"; cp "$work/bin/slow-gh" "$work/bin/gh"
check "a gh that hangs is cut off, and the fetch fails" "1" "$(KINOWO_IDENTITY_DOWNLOAD_TIMEOUT=1 fetch --corpus-only 81 "$work/slow" uk)"
cp "$work/gh.real" "$work/bin/gh"

check "--no-overlay lays out the tree without the identity overlay" "0 tree no-overlay" \
  "$(fetch --no-overlay 77 "$work/plain" uk) $(cat "$work/plain/pair/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search" 2>/dev/null) \
$([ -e "$work/plain/pair/test/resources/fixtures/enrichment-uk/identity/answers" ] && echo overlay || echo no-overlay)"

rm "$work/assets/identity-overlay-uk-77.tar.gz"
check "a run without an overlay is laid out too" "0" "$(fetch 77 "$work/no-overlay" uk)"
check "a run the release holds no tree for fails" "1" "$(fetch 78 "$work/missing" uk)"
check "...naming what is missing" "1" "$(grep -c 'holds enrichment-uk-78' "$work/out")"
touch "$work/down"
check "a release that cannot be read fails, not as a missing tree" "1" "$(fetch 77 "$work/down-dest" uk)"
check "...saying the read failed" "1" "$(grep -c 'uk: could not read release convergence-fixtures' "$work/out")"
rm "$work/down"
touch "$work/blip" "$work/run-blip"
check "a release read and a run download that each fail once are asked again" "0 tree corpus" \
  "$(fetch 77 "$work/blip-dest" uk) $(cat "$work/blip-dest/pair/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search" 2>/dev/null) \
$(cat "$work/blip-dest/pair/test/resources/fixtures/corpus/cinema-scrapes-uk.json.gz" 2>/dev/null)"
# a recording only the archive release holds (github.com's 38713274617): its tree and its corpus from there, no run read
mkdir -p "$work/assets-identity-recordings"
pack "$work/assets-identity-recordings/enrichment-uk-90.tar.zst" enrichment-uk/api.themoviedb.org/search archived-tree zstd
pack "$work/assets-identity-recordings/scrapes-uk-90.tar.gz" corpus/cinema-scrapes-uk.json.gz archived-corpus gzip
check "a recording only the archive release holds is laid out from it" "0 archived-tree archived-corpus" \
  "$(fetch --no-overlay 90 "$work/archived" uk) $(cat "$work/archived/pair/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search" 2>/dev/null) \
$(cat "$work/archived/pair/test/resources/fixtures/corpus/cinema-scrapes-uk.json.gz" 2>/dev/null)"
check "...naming where its tree came from" "1" "$(grep -c 'uk: tree from the archive release identity-recordings' "$work/out")"
check "a run neither release holds names both" "1 1" \
  "$(fetch 91 "$work/neither" uk) $(grep -c 'neither release convergence-fixtures nor identity-recordings holds enrichment-uk-91' "$work/out")"
check "--help prints the usage" "0" "$(fetch --help)"
check "...which names the arguments" "1" "$(grep -c '<recording run id> <dest dir>' "$work/out")"
check "too few arguments is a usage error" "2" "$(fetch 77)"

spec_summary
