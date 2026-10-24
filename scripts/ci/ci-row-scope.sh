#!/usr/bin/env bash
#
# CAN THIS PUSH MOVE ci.yml's SLOW ROWS? Reads changed paths on stdin and prints `true` when one of them can change
# what the named rows test (or, for `deploy`, what main.yml ships), `false` when every one is inert for them.
#
#   ci-row-scope.sh pages    < paths   the Scala page-test row (`page tests (chrome / stable)`: ReviewPageSpec and
#                                      IdentityAdminPageSpec render identity code)
#   ci-row-scope.sh browsers < paths   the Playwright page-test rows and mobile-local-server: the public pages,
#                                      served by FixtureServerMain from the checked-in read-model snapshot
#   ci-row-scope.sh e2e      < paths   the e2e rows: what `pages` takes, and identity's tests too, because the
#                                      `e2e (corpus)` row builds the images main.yml ships and its web tier
#                                      filter takes all of identity/**
#   ci-row-scope.sh --outputs          ci.yml's `scope` job: diffs this commit against the newest green Main run
#                                      (and, for e2e, each tier's deployed marker) and writes e2e=, pages=,
#                                      browsers= to $GITHUB_OUTPUT; every row on anything it cannot read
#
# Main runs ~65 pushes a day, and about a third touch only identity, the judgements, docs or scripts: each still ran
# 9 page/mobile/e2e rows (~44 runner-minutes) that could not have moved (2026-10-22). ci.yml's `scope` job asks this
# script and skips those rows; a skipped row counts as a success for main.yml's `needs: ci` and for the run's
# conclusion the pre-push red gate reads.
#
# INERT, for every kind:
#   docs/**, .claude/**, a top-level *.md       read by no row's code (lint specs reading docs run in `test`)
#   test/resources/fixtures/identity-judgements*  read by the convergence legs and `test`, not by these rows
#   scripts/** that no row can run               a script is LIVE when its name appears in ci.yml, the composite
#                                                actions these rows use, fixture-server.sh, the e2e or page-test
#                                                sources or the build, or a live script sources it
# and also:
#   identity/src/test/**   for pages and browsers (not e2e: main.yml's web tier filter takes all of identity/**)
#   identity/src/main/**   for browsers only (the public pages render the snapshot; identity runs in e2e and in the
#                          review/admin pages of the Scala page row)
# Everything else is LIVE: an unknown path runs every row. (CiRowScopeSpec reads the wiring; ci-row-scope-test.sh
# the classification.)
#
# Bash 3.2: the pre-push hook runs the self-test on macOS too.
set -euo pipefail

kind=${1:?usage: ci-row-scope.sh pages|browsers|e2e [repo root] < changed paths | --outputs}
root=${2:-.}

if [ "$kind" = --outputs ]; then
    self="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
    out=${GITHUB_OUTPUT:-/dev/stdout}
    work=$(mktemp -d)
    every_row() {
        echo "$1: every row runs"
        printf 'e2e=true\npages=true\nbrowsers=true\n' >>"$out"
        exit 0
    }
    [ -z "${PULL_REQUEST:-}" ] || every_row "a pull request"
    [ -n "${BEFORE:-}" ] && [ "$BEFORE" != 0000000000000000000000000000000000000000 ] || every_row "not a push (a dispatch)"
    head=${GITHUB_SHA:?}
    # gh_optional's convention: an unreadable list is said, and runs everything rather than guessing.
    green=$(timeout 60 gh run list --workflow main.yml --branch main --status success --limit 1 \
        --json headSha --jq '.[0].headSha // empty' 2>"$work/err") || every_row "could not read Main's runs ($(head -c 200 "$work/err"))"
    [ -n "$green" ] || every_row "no green Main run to diff from"
    # GIT_NO_LAZY_FETCH: the checkout is a blob:none partial clone, so a plain `cat-file -e` of a commit it lacks asks
    # the promisor remote for it, one lazy fetch spawning the next: 60-140 s of the job's ~88 s (2026-10-22). The
    # explicit depth-1 fetch below brings the same commit in under a second.
    GIT_NO_LAZY_FETCH=1 git cat-file -e "$green^{commit}" 2>/dev/null ||
        timeout 120 git fetch --no-tags --depth=1 --filter=blob:none origin "$green" >/dev/null 2>&1 ||
        every_row "could not fetch the newest green Main run's commit $green"
    git diff --name-only "$green" "$head" >"$work/since-green" || every_row "could not diff $green..$head"
    cp "$work/since-green" "$work/e2e"
    # A tier still to ship: whatever moved since its deployed marker (deployed-base) builds in `e2e (corpus)`.
    for tier in web worker; do
        if timeout 60 git fetch --no-tags --depth=1 --filter=blob:none origin "+refs/tags/deployed-$tier:refs/tags/deployed-$tier" >/dev/null 2>&1; then
            git diff --name-only "refs/tags/deployed-$tier" "$head" >>"$work/e2e" 2>/dev/null ||
                every_row "could not diff deployed-$tier..$head"
        fi
    done
    echo "since the newest green Main run ($green): $(wc -l <"$work/since-green" | tr -d ' ') paths"
    sed -n '1,50s/^/  /p' "$work/since-green"
    for row in e2e pages browsers; do
        file="$work/since-green"; [ "$row" != e2e ] || file="$work/e2e"
        verdict=$("$self" "$row" . <"$file") || every_row "ci-row-scope.sh $row failed"
        echo "$row=$verdict" | tee -a "$out"
    done
    rm -rf "$work"
    exit 0
fi
case "$kind" in pages | browsers | e2e) ;; *) echo "unknown kind '$kind'" >&2; exit 2 ;; esac

changed=$(sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' | grep -v '^$' || true)

# The scripts some row can run, by file name, one per line (computed only when a script changed).
live_scripts=""
compute_live_scripts() {
    local consumers found name
    consumers=$(mktemp -d)
    # What the rows themselves read: ci.yml (whole: a script any of its rows names counts; over-running is safe), the
    # composite actions and helper those rows use, the e2e and page-test sources, and the build.
    for path in .github/workflows/ci.yml .github/actions/setup-sbt-build .github/actions/sbt-target-cache \
        .github/actions/setup-jdk .github/actions/setup-buildx .github/actions/run-page-test \
        .github/scripts/fixture-server.sh e2e/src web/src/page page-tests-playwright build.sbt project; do
        [ -e "$root/$path" ] || continue
        grep -rhI --exclude-dir=node_modules --exclude-dir=target . "$root/$path" 2>/dev/null >>"$consumers/blob" || true
    done
    touch "$consumers/blob"
    (cd "$root" && find scripts -type f 2>/dev/null) | sed 's|.*/||' | sort -u >"$consumers/names"
    # Breadth first: a script named by the text found so far is live, and the files it sources are searched next. One fixed-string search a name a round (BSD grep's -F -f over the whole text took seconds).
    found=""
    while [ -s "$consumers/blob" ]; do
        : >"$consumers/next"
        while IFS= read -r name; do
            if grep -qF -- "$name" "$consumers/blob"; then
                found="$found$name"$'\n'
                # Only what it SOURCES: its comments and messages name half the toolbox, and following those
                # made every script live. What a live script runs by name is named in ci.yml or the specs too.
                (cd "$root" && find scripts -type f -name "$name" -exec cat {} + 2>/dev/null) |
                    grep -E '^[[:space:]]*(\.|source)[[:space:]]' >>"$consumers/next" || true
            else
                echo "$name" >>"$consumers/rest"
            fi
        done <"$consumers/names"
        mv "$consumers/next" "$consumers/blob"
        touch "$consumers/rest"
        mv "$consumers/rest" "$consumers/names"
    done
    rm -rf "$consumers"
    live_scripts=$found
}

script_is_live() {
    [ -n "$live_scripts" ] || compute_live_scripts
    printf '%s\n' "$live_scripts" | grep -qxF "${1##*/}"
}

inert() {
    case "$1" in
        docs/* | .claude/* | test/resources/fixtures/identity-judgements*) return 0 ;;
        */*) ;;
        *.md) return 0 ;;
    esac
    case "$1" in
        scripts/*) script_is_live "$1" && return 1 || return 0 ;;
        identity/src/test/*) [ "$kind" != e2e ] ;;
        identity/src/main/*) [ "$kind" = browsers ] ;;
        *) return 1 ;;
    esac
}

while IFS= read -r path; do
    [ -n "$path" ] || continue
    if ! inert "$path"; then
        echo true
        exit 0
    fi
done <<<"$changed"
echo false
