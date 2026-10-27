#!/usr/bin/env bash
# Delete bot branch $BRANCH in $GH_REPO if it exists (see action.yml for why).
#
# ONLY a 404 means "no branch yet". Any other failure to read it -- a 403, a 5xx, a network blip --
# says nothing about the branch, and treating it as absent let create-pull-request force-push over
# the stale branch this action exists to remove. So that fails the step instead.
# Tested by .github/actions/drop-bot-branch/drop-test.sh against a stub `gh`.
set -uo pipefail
: "${GH_REPO:?}" "${BRANCH:?}"

# ON FORGEJO the job image's gh shim has no git/refs API for a branch: git asks the remote itself, with the checkout's
# credentials (`ls-remote --exit-code`: 2 is no such branch, anything else nonzero a failed read, as above).
if [ "${GITHUB_SERVER_URL:-https://github.com}" != "https://github.com" ]; then
    git ls-remote --exit-code --heads origin "refs/heads/$BRANCH" > /dev/null
    case $? in
        0) git push origin --delete "$BRANCH" || exit 1
           echo "Dropped $BRANCH; the publish step recreates it off main." ;;
        2) echo "No $BRANCH yet; nothing to drop." ;;
        *) echo "::error::could not tell whether $BRANCH exists (git ls-remote failed)"
           exit 1 ;;
    esac
    exit 0
fi

if err=$(gh api "repos/$GH_REPO/git/ref/heads/$BRANCH" --silent 2>&1); then
    gh api -X DELETE "repos/$GH_REPO/git/refs/heads/$BRANCH" || exit 1
    echo "Dropped $BRANCH; the PR step recreates it off main."
elif grep -q 'HTTP 404' <<< "$err"; then
    echo "No $BRANCH yet; nothing to drop."
else
    echo "::error::could not tell whether $BRANCH exists: $err"
    exit 1
fi
