#!/usr/bin/env bash
# precommit-share.sh: each landing-check job's hook environment, and the checks the hook runs there -- the hook's own
# selection, so a job set up only for a non-empty share runs what it always ran.
# Run: bash scripts/ci/precommit-share-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m precommit-share.sh\n'
dir="$(mktemp -d)"; trap 'rm -rf "$dir"' EXIT
repo="$dir/repo"; mkdir -p "$repo/scripts/hooks" "$repo/scripts/ci"
for f in hooks/pre-push hooks/local-tests.txt hooks-path.sh tool-conventions.sh ci/commit_rules.py ci/ci-row-scope.sh \
         ci/precommit-share.sh; do
    cp "$REPO_ROOT/scripts/$f" "$repo/scripts/$f"
done
cp "$REPO_ROOT/build.sbt" "$repo/build.sbt" # the e2eCorpus alias the hook reads (corpus_in_basic)
g() { scratch_git "$repo" "$@"; }
scratch_repo "$repo" -b main; g add -A; g commit -qm base; g update-ref refs/remotes/origin/main HEAD
change() { # <path>: a branch off main touching <path>
    g checkout -q -B "c$RANDOM" refs/remotes/origin/main
    mkdir -p "$repo/$(dirname "$1")"; echo "x $RANDOM" >> "$repo/$1"; g add -A; g commit -qm "touches $1"
}
share() { (cd "$repo" && GITHUB_REF_NAME="${REF:-check/x}" env -u PREPUSH_ONLY -u PREPUSH_ELSEWHERE -u PREPUSH_HEAVY_ROWS \
    bash scripts/ci/precommit-share.sh "$@" 2>/dev/null); }

change identity/src/main/scala/services/identity/Resolver.scala
check "an identity change: the rest's share leaves identity to its own jobs, the heavy rows its own" "driftguards sbt itrest itidentity" \
    "$(share checks rest)"
check "...the identity share is identity" "identity" "$(share checks identity)"
check "...no old-ref share" "" "$(share checks oldrefs)"
check "...the plan names the checks of the jobs after it" "driftguards sbt identity" "$(share checks plan)"
check "...the rest's environment: the heavy rows its own, in no job of their own waiting for a slot (2026-11-18)" \
    "PREPUSH_MODE=--checks-only PREPUSH_RANGE=$(g merge-base HEAD origin/main)..HEAD PREPUSH_HEAVY_ROWS=1 PREPUSH_ELSEWHERE=identity" \
    "$(share env rest | tr '\n' ' ' | sed 's/ $//')"
check "...an identity shard's" "PREPUSH_ONLY=identity" "$(share env identity | tail -1)"
check "a batch's fast check: everything is the rest's, its fast set" "sbt|" "$(REF=check/fast-ab12 share checks rest)|$(REF=check/fast-ab12 share checks identity)"
check "...and its rest leaves nothing to another job (the fast set has no identity; the old refs compile on Main)" "PREPUSH_ELSEWHERE=" \
    "$(REF=check/fast-ab12 share env rest | tail -1)"
full="$(g rev-parse origin/main)"
g update-ref refs/remotes/origin/main HEAD # main moved: the identity change landed
change worker/src/test/resources/fixture.json
check "a change's combined-tree verification: the rest's, the sbt check (its unit tests) for a resource alone" "sbt|" \
    "$(REF="check/verify-$full-x" share checks rest)|$(REF="check/verify-$full-x" share checks identity)"
check "...over what landed since its full check and the change, together" \
    "PREPUSH_MODE=--verify-only PREPUSH_RANGE=$full..HEAD PREPUSH_HEAVY_ROWS=1 PREPUSH_ELSEWHERE=" \
    "$(REF="check/verify-$full-x" share env rest | tr '\n' ' ' | sed 's/ $//')"
check "...a branch naming no commit fails" "1" \
    "$( (cd "$repo" && GITHUB_REF_NAME="check/verify-$(printf '0%.0s' $(seq 40))-x" bash scripts/ci/precommit-share.sh env rest > /dev/null 2>&1); echo $?)"
check "a gate-red fix's branch (check/red-...) names the runner lane only: the same mode and range" \
    "$(REF="check/verify-$full-x" share env rest | tr '\n' ' ')|$(REF=check/fast-ab12 share env rest | tr '\n' ' ')" \
    "$(REF="check/red-verify-$full-x" share env rest | tr '\n' ' ')|$(REF=check/red-fast-ab12 share env rest | tr '\n' ' ')"
# MODULE OVERLAP (agentctl, the owner, 2026-11-18): check/verifyareas-<full>-<id> is the verify round plus the area tests
change identity/src/main/scala/services/identity/Other.scala
va="check/verifyareas-$full-x"
check "a module both sides touch: the rest's share is the verify round's" "$(REF="check/verify-$full-x" share checks rest)" \
    "$(REF="$va" share checks rest)"
check "...and the identity share runs the area's tests" "identity" "$(REF="$va" share checks identity)"
check "...which the plan names" "1" "$(REF="$va" share checks plan | tr ' ' '\n' | grep -cx identity)"
check "...as the change's own checks over what landed since its full check and the change, together" \
    "PREPUSH_MODE=--checks-only PREPUSH_RANGE=$full..HEAD PREPUSH_ONLY=identity" \
    "$(REF="$va" share env identity | tr '\n' ' ' | sed 's/ $//')"
check "...the rest's environment the verify round's" "$(REF="check/verify-$full-x" share env rest | tr '\n' ' ')" \
    "$(REF="$va" share env rest | tr '\n' ' ')"
check "...no old-ref share" "" "$(REF="$va" share checks oldrefs)"
check "...a gate-red fix's too" "identity" "$(REF="check/red-verifyareas-$full-x" share checks identity)"
g update-ref refs/remotes/origin/main "$full"
change web/src/main/twirl/views/x.scala.html
check "a page change: no share of its own -- the page tests run on Main, after landing (the owner, 2026-11-17)" "sbt itrest|" \
    "$(share checks rest)|$(share checks identity)"
check "...and the page share is gone" "2" "$( (cd "$repo" && bash scripts/ci/precommit-share.sh checks page > /dev/null 2>&1); echo $?)"
change docs/notes.md
check "a docs change: no identity, old-ref or heavy share" "||" \
    "$(share checks identity)|$(share checks oldrefs)|$(share checks rest | tr ' ' '\n' | grep -xE 'itrest|itidentity|e2ecorpus' || true)"
check "a share it does not know is refused" "2" "$(share checks everything > /dev/null; (cd "$repo" && bash scripts/ci/precommit-share.sh checks everything > /dev/null 2>&1); echo $?)"
check "the heavy share is gone: the rest runs the heavy rows" "2" "$( (cd "$repo" && GITHUB_REF_NAME=check/x bash scripts/ci/precommit-share.sh env heavy > /dev/null 2>&1); echo $?)"

spec_summary
