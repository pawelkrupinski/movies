#!/usr/bin/env bash
# drift-guards.sh over scratch histories: a drift/flip fix lands only with its drift slice, or an explicit
# `Drift-Slice: none -- <why>`; a red fix changing a resolver rule names its design doc (`Identity-Design:`); a drift
# harness change carries its self-test or `Drift-Harness-Refactor: <why>`; anything else passes.
# Run: bash scripts/ci/drift-guards-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m drift-guards.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo"
scratch_repo "$repo"
commit() { # <path> <message>
    mkdir -p "$repo/$(dirname "$1")"; echo "$RANDOM" >> "$repo/$1"; scratch_git "$repo" add "$1"; scratch_git "$repo" commit -qm "$2"
}
commit README base
base=$(scratch_git "$repo" rev-parse HEAD)
# the guard reads the scratch repository, never a GIT_DIR the hook running this spec was handed (scratch-git.sh)
ungit() { local v; for v in $(compgen -e | grep '^GIT_'); do unset "$v"; done; }
guard() { (cd "$repo" && ungit; bash "$REPO_ROOT/scripts/ci/drift-guards.sh" "$base..HEAD" > /dev/null 2>&1; echo $?); }
reset() { scratch_git "$repo" reset -q --hard "$base"; }

commit identity/src/main/A.scala "$(printf 'Identity: a feature\n\nNo red.')"
check "a commit fixing no red passes" 0 "$(guard)"; reset

commit identity/src/main/A.scala "$(printf 'Identity: the agreement keeps its verdicts\n\nFixes-Red: 3726')"
check "a red fix not about drift passes" 0 "$(guard)"; reset

commit identity/src/main/A.scala "$(printf 'Identity: CAMOUFLAGE no longer flips\n\nFixes-Red: 3726')"
check "a flip fix without its slice fails" 1 "$(guard)"; reset

commit identity/src/main/A.scala "$(printf 'Identity: the incremental state ends where a whole resolve does (drift)\n\nFixes-Red: 3726')"
check "a drift fix without its slice fails" 1 "$(guard)"; reset

commit identity/src/main/A.scala "$(printf 'Identity: CAMOUFLAGE no longer flips\n\nFixes-Red: 3726')"
commit test/resources/fixtures/drift-slices/de/werkstattkino-camouflage/slice.json.gz "Drift slice: CAMOUFLAGE"
check "a flip fix with its slice later in the push passes" 0 "$(guard)"; reset

commit identity/src/main/A.scala "$(printf 'CI: the drift row times out later\n\nFixes-Red: 3134\nDrift-Slice: none -- a CI script, no listing')"
check "a drift fix with a reasoned Drift-Slice: none passes" 0 "$(guard)"; reset

commit identity/src/main/A.scala "$(printf 'CI: the drift row times out later\n\nFixes-Red: 3134\nDrift-Slice: none')"
check "Drift-Slice: none without a reason fails" 1 "$(guard)"; reset

rule=identity/src/main/scala/services/identity/FeedLink.scala
commit "$rule" "$(printf 'Identity: a feed link overruled by one source\n\nFixes-Red: 2473\nDrift-Slice: none -- spec case')"
check "a red fix changing a resolver rule fails" 1 "$(guard)"; reset

commit "$rule" "$(printf 'Identity: a feed link overruled by one source\n\nFixes-Red: 2473\nDrift-Slice: none -- spec case\nIdentity-Design: docs/design/feed-links.md')"
check "...and passes with the design doc that measured it" 0 "$(guard)"; reset

commit common/src/main/resources/title-rules.tsv "$(printf 'Title rules: a row\n\nFixes-Red: 2473')"
check "a title rule is a rule too" 1 "$(guard)"; reset

commit "$rule" "$(printf 'Identity: a feed link overruled by one source\n\nMeasured on every country.')"
check "a rule change fixing no red passes" 0 "$(guard)"; reset

commit identity/src/main/scala/services/identity/agreement/AgreementStage.scala "$(printf 'Agreement: the corrections budget no longer lags\n\nFixes-Red: 2473\nDrift-Slice: none -- spec case')"
check "a red fix in the stage's state handling (a mechanism) needs no design doc" 0 "$(guard)"; reset

harness=worker/src/test/scala/scripts/IdentityDrift.scala
commit "$harness" "Drift harness: the queue files a held answer first"
check "a harness change without its self-test fails" 1 "$(guard)"; reset

mkdir -p "$repo/worker/src/test/scala/scripts"; echo a >> "$repo/$harness"; echo b >> "$repo/worker/src/test/scala/scripts/IdentityDriftSpec.scala"
scratch_git "$repo" add -A; scratch_git "$repo" commit -qm "Drift harness: the queue files a held answer first, and its replay case"
check "...with a capture-replay case in the same commit it passes" 0 "$(guard)"; reset

commit scripts/ci/identity-drift-check.sh "$(printf 'Drift check: one helper for the verdict line\n\nDrift-Harness-Refactor: no behaviour moves')"
check "...a pure refactor says so and passes" 0 "$(guard)"; reset

commit identity/src/main/scala/services/identity/TakeFlips.scala "Flips: a re-scrape is no input change"
check "...the flip predicate is the harness too" 1 "$(guard)"; reset

commit identity/src/main/scala/services/identity/TakeFlips.scala "Flips: a re-scrape is no input change"
commit identity/src/test/scala/services/identity/TakeFlipsSpec.scala "Flips: its spec, in a later commit"
check "...a self-test in another commit of the push does not count" 1 "$(guard)"; reset

check "an unreadable range is no verdict" 2 "$(cd "$repo" && ungit; bash "$REPO_ROOT/scripts/ci/drift-guards.sh" "nosuch..HEAD" > /dev/null 2>&1; echo $?)"

spec_summary
