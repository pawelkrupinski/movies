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
pack "$work/artifacts/scrape-fixtures-uk/scrapes-uk.tar.gz" corpus/uk.json corpus gzip

# `release download` hands over the asset its --pattern names when the release holds it; `run download`
# copies the named artifact's files into --dir.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
cmd="$1 $2"; shift 2
while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; --pattern) pattern="$2" ;; --name) name="$2" ;; esac; shift; done
case "$cmd" in
  "release download")
    [ -f "$STUB/down" ] && { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
    [ -f "$STUB/assets/$pattern" ] || { echo "no assets match the file pattern" >&2; exit 1; }
    mkdir -p "$dir"; cp "$STUB/assets/$pattern" "$dir/" ;;
  "run download")     [ -d "$STUB/artifacts/$name" ] || exit 1; mkdir -p "$dir"; cp "$STUB/artifacts/$name/"* "$dir/" ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"

fetch() { STUB="$work" PATH="$work/bin:$PATH" bash "$REPO_ROOT/scripts/identity-fetch-recording.sh" "$@" > "$work/out" 2>&1; echo $?; }

check "a run's pair is laid out" "0" "$(fetch 77 "$work/dest" uk)"
pair="$work/dest/pair/test/resources/fixtures"
check "...its enrichment tree, unpacked from zstd" "tree" "$(cat "$pair/enrichment-uk/api.themoviedb.org/search" 2>/dev/null)"
check "...its identity overlay, unpacked from gzip" "overlay" "$(cat "$pair/enrichment-uk/identity/answers" 2>/dev/null)"
check "...and its scrape corpus" "corpus" "$(cat "$pair/corpus/uk.json" 2>/dev/null)"

rm "$work/assets/identity-overlay-uk-77.tar.gz"
check "a run without an overlay is laid out too" "0" "$(fetch 77 "$work/no-overlay" uk)"
check "a run the release holds no tree for fails" "1" "$(fetch 78 "$work/missing" uk)"
check "...naming what is missing" "1" "$(grep -c 'uk: release convergence-fixtures holds no enrichment-uk-78' "$work/out")"
touch "$work/down"
check "a release that cannot be read fails, not as a missing tree" "1" "$(fetch 77 "$work/down-dest" uk)"
check "...saying the read failed" "1" "$(grep -c 'uk: could not read release convergence-fixtures' "$work/out")"
rm "$work/down"
check "--help prints the usage" "0" "$(fetch --help)"
check "...which names the arguments" "1" "$(grep -c '<recording run id> <dest dir>' "$work/out")"
check "too few arguments is a usage error" "2" "$(fetch 77)"

spec_summary
