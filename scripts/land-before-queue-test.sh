#!/usr/bin/env bash
# scripts/land-before-queue over a stub identity-stamp-carry: the stamp carry's bound overridden is a BYPASS said in the
# land's output (2026-11-21: a lane landed with KINOWO_LAND_STAMP_CARRY_SECONDS=1, its stamps bumped, unsaid); unset,
# nothing is said; a carry that fails never fails the land.
# Run: bash scripts/land-before-queue-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m land-before-queue\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cp "$REPO_ROOT/scripts/land-before-queue" "$REPO_ROOT/scripts/tool-conventions.sh" "$work/"
printf '#!/usr/bin/env bash\necho "carry $*"\nexit "${STUB_RC:-0}"\n' > "$work/identity-stamp-carry"
chmod +x "$work/identity-stamp-carry"

out="$(env -u KINOWO_LAND_STAMP_CARRY_SECONDS "$work/land-before-queue" a b 2>&1)"; status=$?
check "the carry runs over the range, no bypass said" "0 1 0" \
  "$status $(grep -c '^carry auto a..b$' <<< "$out") $(grep -c BYPASS <<< "$out")"
out="$(KINOWO_LAND_STAMP_CARRY_SECONDS=1 "$work/land-before-queue" a b 2>&1)"; status=$?
check "the bound overridden is a BYPASS, named in the output" "0 1" \
  "$status $(grep -c 'BYPASS KINOWO_LAND_STAMP_CARRY_SECONDS=1' <<< "$out")"
out="$(STUB_RC=3 "$work/land-before-queue" a b 2>&1)"; status=$?
check "a carry that fails is said and never fails the land" "0 1" "$status $(grep -c 'did not finish (exit 3)' <<< "$out")"
out="$("$work/land-before-queue" a 2>&1)"; status=$?
check "one ref only is a usage error (2), not a silent success" "2 1" "$status $(grep -c '^land-before-queue <base> <head>$' <<< "$out")"

spec_summary
