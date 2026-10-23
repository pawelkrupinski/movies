#!/usr/bin/env bash
# convergence-preflight.sh: a missing pin, pinned tree, named recording or its corpus fails the run in seconds; a gh
# read that fails is no evidence and passes. gh is a stub answering from files under $STUB.
# Run: bash scripts/ci/convergence-preflight-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m convergence-preflight.sh\n'

stub="$(mktemp -d)"
trap 'rm -rf "$stub"' EXIT
mkdir -p "$stub/bin" "$stub/pins" "$stub/runs"
cat >"$stub/bin/gh" <<'GH'
#!/usr/bin/env bash
case "$1 $2" in
    "release view") [ -f "$STUB/release-fails" ] && { echo "HTTP 502" >&2; exit 1; }; cat "$STUB/assets" ;;
    "release download")
        dir=""; while [ $# -gt 0 ]; do [ "$1" = --dir ] && dir=$2; shift; done
        mkdir -p "$dir" && cp "$STUB"/pins/* "$dir"/ 2>/dev/null; exit 0 ;;
    "api "*)
        run=$(echo "$2" | sed -E 's|.*/runs/([0-9]+)/artifacts.*|\1|')
        [ -f "$STUB/api-fails" ] && { echo "HTTP 503" >&2; exit 1; }
        [ -f "$STUB/runs/$run" ] || { echo "gh: Not Found (HTTP 404)" >&2; exit 1; }
        cat "$STUB/runs/$run" ;;
esac
GH
chmod +x "$stub/bin/gh"

# run <args...> -> "<exit status>", the output in $stub/out
run() {
    env PATH="$stub/bin:/usr/bin:/bin" STUB="$stub" GH_REPO=o/r FIXTURE_RELEASE_TAG=convergence-fixtures \
        bash "$REPO_ROOT/scripts/ci/convergence-preflight.sh" "$@" >"$stub/out" 2>&1
    echo $?
}
said() { grep -c -- "$1" "$stub/out"; }

printf '%s\n' hermetic-pl.txt hermetic-de.txt enrichment-pl-573.tar.zst enrichment-de-573.tar.zst \
    enrichment-pl-528.tar.zst >"$stub/assets"
echo "573 2026-10-20T13:35:26Z enrichment-pl-573.tar.zst" >"$stub/pins/hermetic-pl.txt"
echo "573 2026-10-20T13:35:31Z enrichment-de-573.tar.*" >"$stub/pins/hermetic-de.txt"
printf '%s\n' scrape-fixtures-pl scrape-fixtures-de >"$stub/runs/573"
printf '%s\n' scrape-fixtures-pl >"$stub/runs/528"

check "every pinned pair there passes" "0" "$(run pl,de)"
check "a named recording with its tree and corpus passes" "0" "$(run pl 528)"
check "a country with no pin fails" "1" "$(run pl,us)"
check "and says so as the leg would" "1" "$(said '::error::No recorded pair pinned for us')"
echo "573 2026-10-20T13:35:31Z enrichment-de-500.tar.zst" >"$stub/pins/hermetic-de.txt"
check "a pinned tree the release no longer keeps fails" "1" "$(run de)"
check "and names it" "1" "$(said 'the pinned tree enrichment-de-500.tar.zst is not in release')"
echo "573" >"$stub/pins/hermetic-de.txt"
check "a malformed pin fails" "1" "$(run de)"
echo "573 2026-10-20T13:35:31Z enrichment-de-573.tar.zst" >"$stub/pins/hermetic-de.txt"
check "a recording this forge never held fails" "1" "$(run pl 38713274617)"
check "and says it is not on this forge" "1" "$(said 'recording run 38713274617 is not on this forge')"
check "a recording without that country's corpus fails" "1" "$(run de 528)"
check "and names the missing tree and corpus" "2" "$(said 'enrichment-de-528.tar\|no scrape-fixtures-de')"
# The archive release (the stub answers every release view alike) keeps the landing recordings past their artifacts.
printf '%s\n' enrichment-de-528.tar.zst scrapes-de-528.tar.gz >>"$stub/assets"
check "a recording whose tree and corpus the archive release keeps passes" "0" "$(run de 528)"
touch "$stub/api-fails"
check "an unreadable artifacts list is no evidence: it passes" "0" "$(run pl,de)"
check "and warns" "1" "$(said '::warning::could not list recording run 573')"
rm -f "$stub/api-fails"
touch "$stub/release-fails"
check "an unreadable release is no evidence: it passes" "0" "$(run pl,us)"

spec_summary
