#!/usr/bin/env bash
# script-test: own-job compiles the tools into old refs through the real sbt (minutes cold); ci.yml's integration job runs it where sbt and a compiled target/ are
# scripts/identity-slice and scripts/title-trace drop THIS checkout's tool files (IdentitySlice, FiledAnswers, TitleTrace,
# …) into every ref they decide under, so those must compile against old code too: a deployed worker's build, a bisect's
# first commit. 83b869548's FiledAnswers read `Capture.live`, 2026-10-17's IdentitySlice and IdentityExplain
# `Listing.properTitle`, and 2026-10-18's TitleTrace `TitleNormalizer.facts`, none of which an older ref has: every
# `--ref <old>`, `--bisect` and listing-history replay failed to compile. A member newer than the oldest ref is reached
# by name (services.identity.OlderRefs). This compiles each tool into its floor through the real sbt — minutes cold,
# seconds once a ref is laid out in the shared cache: identity-slice into c2974e3aa (2026-10-12, before all three),
# and into 9bb54c1a4 (2026-10-22, the first ref with its OWN scripts.IdentityDrift: IdentityDrift is no tool file, so the ref's
# copy is compiled against this checkout's IdentitySlice -- c5b0a6b70 gave `Reads` a `prod` parameter and broke every ref
# carrying an older IdentityDrift, unseen, since the floor above has no IdentityDrift to break),
# title-trace into e90d2b498 (2026-10-14, its own first commit: an older ref has no tracer to compare against).
# Run: bash scripts/identity-slice-old-ref-test.sh   (OLD_REFS="<sha> …" to check both tools at others, e.g. a bisect's range)
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

floor() { case $1 in identity-slice) echo c2974e3aaea73fb841096a24316fb5e872154de9 9bb54c1a4912644383d5ed2f02686fe0ed720ece ;; title-trace) echo e90d2b498a0f7a26faa55b7d7cdea51d48285df8 ;; esac; }
for tool in identity-slice title-trace; do
  for ref in ${OLD_REFS:-$(floor "$tool")}; do
    printf '\033[36m▸\033[0m %s compiles into %s\n' "$tool" "$ref"
    out=$("$REPO_ROOT/scripts/$tool" --compiles "$ref" 2>&1); code=$?
    [ "$code" = 0 ] || printf '%s\n' "$out" | grep -v $'^\\[error\\] \t*at ' | tail -40
    check "this checkout's $tool tool files compile into $ref" "0" "$code"
  done
done

spec_summary
