#!/usr/bin/env bash
# Delete bot branch $BRANCH in $GH_REPO if it exists (see action.yml for why).
#
# ONLY a 404 means "no branch yet". Any other failure to read it -- a 403, a 5xx, a network blip --
# says nothing about the branch, and treating it as absent let create-pull-request force-push over
# the stale branch this action exists to remove. So that fails the step instead.
# Tested by .github/actions/drop-bot-branch/drop-test.sh against a stub `gh`.
set -uo pipefail
: "${GH_REPO:?}" "${BRANCH:?}"

if err=$(gh api "repos/$GH_REPO/git/ref/heads/$BRANCH" --silent 2>&1); then
    gh api -X DELETE "repos/$GH_REPO/git/refs/heads/$BRANCH" || exit 1
    echo "Dropped $BRANCH; the PR step recreates it off main."
elif grep -q 'HTTP 404' <<< "$err"; then
    echo "No $BRANCH yet; nothing to drop."
else
    echo "::error::could not tell whether $BRANCH exists: $err"
    exit 1
fi
