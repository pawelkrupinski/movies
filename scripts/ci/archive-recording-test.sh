#!/usr/bin/env bash
# archive-recording.sh against a stub `gh` whose releases and run artifacts are directories.
# Run: bash scripts/ci/archive-recording-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m archive-recording.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/convergence-fixtures" "$work/identity-recordings" "$work/runs"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
root="$(dirname "$0")/.."
case "$1 $2" in
  "release view") for f in "$root/$3"/*; do [ -e "$f" ] && printf '%s\t%s\n' "$(basename "$f")" "$(wc -c < "$f" | tr -d ' ')"; done; exit 0 ;;
  "release download")
    while [ "$#" -gt 0 ]; do case "$1" in --pattern) p=$2 ;; --dir) d=$2 ;; esac; shift; done
    cp "$root/convergence-fixtures/$p" "$d/" ;;
  "release upload") echo "upload $4" >> "$root/calls"; cp "$4" "$root/$3/" ;;
  "run download")
    run=$3
    while [ "$#" -gt 0 ]; do case "$1" in --name) n=$2 ;; --dir) d=$2 ;; esac; shift; done
    [ -d "$root/runs/$run/$n" ] || { echo "no artifact matches" >&2; exit 1; }
    cp -R "$root/runs/$run/$n/." "$d/" ;;
  *) exit 2 ;;
esac
STUB
chmod +x "$work/bin/gh"
run() { env PATH="$work/bin:$PATH" GH_RELEASE_RETRY_DELAY=0 bash "$REPO_ROOT/scripts/ci/archive-recording.sh" "$@" > "$work/out" 2>&1; echo $?; }
assets() { find "$work/identity-recordings" -type f -exec basename {} \; | sort | tr '\n' ' ' | sed 's/ $//'; }

echo tree-pl > "$work/convergence-fixtures/enrichment-pl-528.tar.zst"
echo tree-de > "$work/convergence-fixtures/enrichment-de-528.tar.zst"
mkdir -p "$work/runs/528/scrape-fixtures-pl" "$work/runs/528/scrape-fixtures-de"
echo corpus-pl > "$work/runs/528/scrape-fixtures-pl/scrapes-pl.tar.gz"
echo corpus-de > "$work/runs/528/scrape-fixtures-de/scrapes-de.tar.zst"
echo part > "$work/identity-recordings/enrichment-de-528.tar.zst"

check "a recording's trees and corpora are archived, the countries it lacks skipped" "0" "$(run 528)"
check "...under the names the readers ask for, a partial upload replaced" \
  "enrichment-de-528.tar.zst enrichment-pl-528.tar.zst scrapes-de-528.tar.zst scrapes-pl-528.tar.gz|tree-de" \
  "$(assets)|$(cat "$work/identity-recordings/enrichment-de-528.tar.zst")"
: > "$work/calls"
check "a second run uploads nothing" "0|0" "$(run 528)|$(wc -l < "$work/calls" | tr -d ' ')"
rm -rf "$work/runs/528/scrape-fixtures-pl"; rm "$work/identity-recordings/scrapes-pl-528.tar.gz"
check "a tree whose corpus artifact is gone fails, naming it" "1|yes" \
  "$(run 528)|$(grep -q 'scrape-fixtures-pl artifact could not be read' "$work/out" && echo yes)"
check "a run holding nothing fails" "1" "$(run 999)"
check "no run is a usage error" "64" "$(run)"

spec_summary
