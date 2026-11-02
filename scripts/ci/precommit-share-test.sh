#!/usr/bin/env bash
# precommit-share.sh: each landing-check job's hook environment, and the checks the hook runs there -- the hook's own
# selection, so a job set up only for a non-empty share runs what it always ran.
# Run: bash scripts/ci/precommit-share-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m precommit-share.sh\n'
dir="$(mktemp -d)"; trap 'rm -rf "$dir"' EXIT
repo="$dir/repo"; mkdir -p "$repo/scripts/hooks" "$repo/scripts/ci"
for f in hooks/pre-push hooks/local-tests.txt hooks-path.sh tool-conventions.sh ci/commit_rules.py ci/ci-row-scope.sh \
         ci/precommit-share.sh; do
    cp "$REPO_ROOT/scripts/$f" "$repo/scripts/$f"
done
g() { git -C "$repo" -c user.name=t -c user.email=t@t -c commit.gpgsign=false "$@"; }
g init -q -b main; g add -A; g commit -qm base; g update-ref refs/remotes/origin/main HEAD
change() { # <path>: a branch off main touching <path>
    g checkout -q -B "c$RANDOM" refs/remotes/origin/main
    mkdir -p "$repo/$(dirname "$1")"; echo "x $RANDOM" >> "$repo/$1"; g add -A; g commit -qm "touches $1"
}
share() { (cd "$repo" && GITHUB_REF_NAME="${REF:-check/x}" env -u PREPUSH_ONLY -u PREPUSH_ELSEWHERE ROWS="${ROWS:-}" \
    bash scripts/ci/precommit-share.sh "$@" 2>/dev/null); }

change identity/src/main/scala/services/identity/Resolver.scala
check "an identity change: the rest's share leaves identity to its own jobs" "driftguards sbt" "$(share checks rest)"
check "...the identity share is identity" "identity" "$(share checks identity)"
check "...no page or old-ref share" "|" "$(share checks page)|$(share checks oldrefs)"
check "...the plan names every check, the heavy rows among them" "driftguards sbt identity itrest itidentity e2ecorpus e2e" \
    "$(share checks plan)"
check "...the heavy job runs the rows it is given" "itidentity e2ecorpus" "$(ROWS='itidentity e2ecorpus' share checks heavy)"
check "...the rest's environment" "PREPUSH_MODE=--checks-only PREPUSH_RANGE=$(g merge-base HEAD origin/main)..HEAD PREPUSH_ELSEWHERE=page oldrefs identity" \
    "$(share env rest | tr '\n' ' ' | sed 's/ $//')"
check "...an identity shard's" "PREPUSH_ONLY=identity" "$(share env identity | tail -1)"
check "a batch's fast check: everything is the rest's, its fast set" "sbt|" "$(REF=check/fast-ab12 share checks rest)|$(REF=check/fast-ab12 share checks identity)"
check "...and its rest leaves identity to nobody (the fast set has none)" "PREPUSH_ELSEWHERE=page oldrefs" \
    "$(REF=check/fast-ab12 share env rest | tail -1)"
change web/src/main/twirl/views/x.scala.html
check "a page change: the page share" "page" "$(share checks page)"
check "...and no identity share" "" "$(share checks identity)"
change docs/notes.md
check "a docs change: no identity, page, old-ref or heavy share" "|||" \
    "$(share checks identity)|$(share checks page)|$(share checks oldrefs)|$(share checks plan | tr ' ' '\n' | grep -xE 'itrest|itidentity|e2e|e2ecorpus' || true)"
check "a share it does not know is refused" "2" "$(share checks everything > /dev/null; (cd "$repo" && bash scripts/ci/precommit-share.sh checks everything > /dev/null 2>&1); echo $?)"
check "the heavy share without its rows fails" "1" "$( (cd "$repo" && GITHUB_REF_NAME=check/x bash scripts/ci/precommit-share.sh env heavy > /dev/null 2>&1); echo $?)"

spec_summary
