#!/usr/bin/env bash
# THE DRIFT GATE'S COMMIT RULES, ON CI (scripts/ci/commit_rules.py: `allowlists` and `driftguards`). The pre-push hook
# runs them on the laptop, but `git push --no-verify` or PREPUSH_SKIP skips the hook, and with it the allow-list ratchet,
# the shrink-only known-flips baseline and the design-trailer rule. Here they run over every commit main gained since
# the rules last held on main (a superseded or red run's commits stay in the range until a run passes with them), in
# Main's CI and in the landing check (precommit-check.yml), whatever paths the push touched: a commit that breaks one
# turns the run red.
#
# THE BASE COMES FROM GIT (simplification-2026-10-31 item 5): refs/verified/commit-rules, the commit the rules last held
# on, which a run on main ADVANCES when they hold (a fast-forward push of the ref: a slower, older run never moves it
# back). It used to be "the newest green Main run on this forge", which broke three ways in a week: 1,392 commits back
# after the forge move (a floor file patched that), a measurement run taken for main's, and a red Main holding the
# range open for every other check.
#
# usage: scripts/ci/commit-rules-ci.sh [--base <commit>]
#   --base   the range's start (precommit-check: the merge-base with main); default refs/verified/commit-rules, else
#            $BEFORE (the push's own, before the ref's first advance); none of them (a dispatch): nothing to judge, said
# A run on main ($GITHUB_REF refs/heads/main, a push) whose rules hold advances the ref to HEAD; it failing to is an
# error (exit 2), never a silent stale base.
# Exit: 0 every rule holds, 1 a commit breaks one (each named), 2 the range could not be read.
set -uo pipefail

base=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        --base) base=${2:?--base needs a commit}; shift ;;
        *) echo "commit-rules-ci: unknown argument $1" >&2; exit 2 ;;
    esac
    shift
done
zero=0000000000000000000000000000000000000000
Verified=refs/verified/commit-rules
if [ -z "$base" ]; then
    # ls-remote first: 2 is "no such ref yet" (the ref's first run), anything else non-zero a failed read -- exit 2
    timeout 60 git ls-remote --exit-code origin "$Verified" > /dev/null; ls=$?
    case $ls in
        0) timeout 120 git fetch --quiet --no-tags origin "+$Verified:$Verified" \
               || { echo "::error title=commit rules::could not fetch $Verified"; exit 2; }
           base=$(git rev-parse "$Verified") ;;
        2) [ "${BEFORE:-$zero}" != "$zero" ] && base=$BEFORE ;;
        *) echo "::error title=commit rules::could not read $Verified (git ls-remote exit $ls)"; exit 2 ;;
    esac
fi
[ -n "$base" ] || { echo "commit-rules-ci: no green Main run and no push base: nothing to judge"; exit 0; }

# A shallow checkout holds neither the range nor the lists' history the ratchet reads: the commits, not their blobs
# (fetched as read), once.
if [ "$(git rev-parse --is-shallow-repository 2>/dev/null)" = true ]; then
    timeout 300 git fetch --quiet --no-tags --unshallow --filter=blob:none origin \
        || { echo "::error title=commit rules::could not deepen the checkout to read $base..HEAD"; exit 2; }
fi
git cat-file -e "$base^{commit}" 2>/dev/null \
    || timeout 120 git fetch --quiet --no-tags --filter=blob:none origin "$base" \
    || { echo "::error title=commit rules::no commit $base to judge from"; exit 2; }
git merge-base --is-ancestor "$base" HEAD || { echo "commit-rules-ci: $base is no ancestor of HEAD: nothing to judge"; exit 0; }

echo "commit rules over ${base:0:9}..$(git rev-parse --short=9 HEAD) ($(git rev-list --count "$base..HEAD") commit(s))"
rc=0
for set in allowlists driftguards; do
    python3 "$(dirname "${BASH_SOURCE[0]}")/commit_rules.py" "$set" --range "$base..HEAD"; r=$?
    case $r in
        0) echo "  $set: every commit holds" ;;
        1) echo "::error title=commit rules ($set)::a commit breaks the drift gate's $set rule -- see the step log"; rc=1 ;;
        *) echo "::error title=commit rules ($set)::the range could not be read"; [ "$rc" = 1 ] || rc=2 ;;
    esac
done
# A NEW CHECK GATES ONLY ONCE IT HAS BEEN GREEN: the gating-checks registry in step with the gate's workflows
# (scripts/ci/gating_checks.py lint), here too, so a push the hook never saw is held to it
if [ -f .github/gating-checks.tsv ]; then
    if python3 "$(dirname "${BASH_SOURCE[0]}")/gating_checks.py" lint; then echo "  gating checks: every job, label and gate workflow registered"
    else echo "::error title=commit rules (gating checks)::.github/gating-checks.tsv is out of step with the gate's workflows -- see the step log"; rc=1
    fi
fi
# The rules held on main: the verified ref moves to HEAD (fast-forward only).
if [ "$rc" = 0 ] && [ "${GITHUB_REF:-}" = refs/heads/main ] && [ "${GITHUB_EVENT_NAME:-}" = push ]; then
    if timeout 120 git push --quiet origin "HEAD:$Verified"; then echo "  $Verified -> $(git rev-parse --short=9 HEAD)"
    elif git fetch --quiet --no-tags origin "+$Verified:$Verified" && git merge-base --is-ancestor HEAD "$Verified"; then
        echo "  $Verified is past $(git rev-parse --short=9 HEAD) already (a newer run advanced it)"
    else
        echo "::error title=commit rules::could not advance $Verified to $(git rev-parse --short=9 HEAD): the next run would judge from a stale base"
        rc=2
    fi
fi
exit "$rc"
