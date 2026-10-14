#!/usr/bin/env bash
# scripts/identity-slice drops THIS checkout's tool files (IdentitySlice, FiledAnswers, …) into every ref it slices, so
# they must compile against old code too: a deployed worker's build, a bisect's first commit. 83b869548's FiledAnswers
# read `Capture.live`, which no older ref has, and every `--ref <old>`, `--bisect` and listing-history replay failed to
# compile. This compiles the tool into OLD_REF (default c2974e3aa, 2026-10-12: before Capture.live and before the
# catalog was keyed by City) through the real sbt — minutes cold, seconds once the ref is laid out in the shared cache.
# Run: bash scripts/identity-slice-old-ref-test.sh   (OLD_REF=<sha> to check another)
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-slice compiles into an old ref\n'
ref=${OLD_REF:-c2974e3aa}
"$REPO_ROOT/scripts/identity-slice" --compiles "$ref" > /dev/null 2>&1; code=$?
check "this checkout's tool files compile into $ref" "0" "$code"

spec_summary
