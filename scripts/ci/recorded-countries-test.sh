#!/usr/bin/env bash
# recorded-countries.sh against a stub `gh` answering the release's asset names from $STUB_ASSETS.
# Run: bash scripts/ci/recorded-countries-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
cat > "$stub_dir/gh" <<'STUB'
#!/usr/bin/env bash
[ -z "${STUB_FAIL:-}" ] || { echo "HTTP 502" >&2; exit 1; }
printf '%s\n' $STUB_ASSETS
STUB
chmod +x "$stub_dir/gh"

recorded() { PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/recorded-countries.sh" "$@" 2>/dev/null; }

export STUB_ASSETS="enrichment-pl-700.tar.zst enrichment-de-700.tar.zst enrichment-de.tar.zst enrichment-uk-699.tar.zst enrichment-us-700.tar.gz hermetic-pl.txt"
check "names every country whose tree the run published, in the default order" "pl,de,us" "$(recorded 700)"
check "...and only among the codes asked" "de" "$(recorded 700 de,uk)"
check "an empty code list is every country" "pl,de,us" "$(recorded 700 '')"
check "another run's tree is not this run's" "uk" "$(recorded 699)"
check "a run with no complete country prints nothing" "" "$(recorded 701)"
check "a longer run id sharing a prefix is not this run" "" "$(recorded 70)"
check "an unreadable release fails, never reads as nothing captured" "1" \
  "$(STUB_FAIL=1 PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/recorded-countries.sh" 700 >/dev/null 2>&1; echo $?)"

spec_summary
