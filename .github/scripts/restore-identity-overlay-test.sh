#!/usr/bin/env bash
# restore-identity-overlay.sh against a stub `gh` whose release holds the leg's own overlay, an
# earlier one, none, or cannot be read at all.
# Run: bash .github/scripts/restore-identity-overlay-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m restore-identity-overlay.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/assets" "$work/src/test/resources/fixtures/enrichment-pl/api.themoviedb.org"
printf 'answered\n' > "$work/src/test/resources/fixtures/enrichment-pl/api.themoviedb.org/search"
( cd "$work/src" && tar -czf "$work/assets/identity-overlay-pl-7.tar.gz" test )
cp "$work/assets/identity-overlay-pl-7.tar.gz" "$work/assets/identity-overlay-pl-3.tar.gz"

# The release holds the files under $STUB_RELEASE; $STUB_UNREACHABLE names the call ("download" or
# "view") that fails as an unreachable release does.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
[ "${STUB_UNREACHABLE:-}" = "$2" ] && { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
case "$1 $2" in
  "release download")
    while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; --pattern) pattern="$2" ;; esac; shift; done
    [ -f "$STUB_RELEASE/$pattern" ] || { echo "no assets match the file pattern" >&2; exit 1; }
    cp "$STUB_RELEASE/$pattern" "$dir/" ;;
  "release view") ls "$STUB_RELEASE" ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"

# restore <label> <release dir> [unreachable call] -> exit status; run in $work/ws-<label>
restore() {
  mkdir -p "$work/ws-$1" "$2"
  ( cd "$work/ws-$1" && STUB_RELEASE="$2" STUB_UNREACHABLE="${3:-}" PATH="$work/bin:$PATH" FIXTURE_RELEASE_TAG=convergence-fixtures \
      bash "$REPO_ROOT/.github/scripts/restore-identity-overlay.sh" pl identity-overlay-pl-7.tar.gz "$work/files-$1" > "$work/out-$1" 2>&1 )
  echo $?
}
answer=test/resources/fixtures/enrichment-pl/api.themoviedb.org/search

check "a release holding the leg's own overlay unpacks it" "0" "$(restore own "$work/assets")"
check "...into the workspace" "answered" "$(cat "$work/ws-own/$answer" 2>/dev/null)"
check "...listing what it placed" "$answer" "$(cat "$work/files-own")"

mkdir -p "$work/earlier" && cp "$work/assets/identity-overlay-pl-3.tar.gz" "$work/earlier/"
check "a release with only an earlier overlay carries it forward" "0" "$(restore carried "$work/earlier")"
check "...into the workspace" "answered" "$(cat "$work/ws-carried/$answer" 2>/dev/null)"

check "a release with no overlay at all leaves the leg to fill its gaps live" "0" "$(restore none "$work/empty")"
check "...having placed nothing" "" "$(cat "$work/files-none")"

check "a release that cannot be downloaded from fails, rather than asking every gap live" "4" \
  "$(restore unreachable "$work/assets" download)"
check "...and says why" "true" "$(grep -q 'HTTP 502' "$work/out-unreachable" && echo true || echo false)"
check "a release whose asset list cannot be read fails too" "4" "$(restore unlisted "$work/earlier" view)"
check "...having placed nothing" "" "$(cat "$work/files-unlisted")"

spec_summary
