#!/usr/bin/env bash
# Find the commit that turned a country's HERMETIC convergence leg red.
#
# WHY. A convergence leg is 60 minutes (a country) to 200 (the US), the lane keeps one run
# in flight and one pending, and a day of pushes collapses into a handful of verdicts — so a
# red leg arrives naming a SHA with dozens of untested commits under it, and the red streaks
# ran from three hours to two and a half days with the breaking commit unknown. A hermetic
# leg replays the same recorded corpus and enrichment tree whatever the code, so once it goes
# red the question "which commit?" has a mechanical answer: replay the fast SAMPLE leg over
# the untested range, halving it each time.
#
# HOW. `git bisect run` over (last green, first red], limited to commits that touch the
# pipeline (`.github/convergence-paths.txt`) and to first-parent history — once the last green
# commit replays green under the red leg's recording (it may have been green under an older
# one, and then no commit is to blame). Cost is CAPPED: at
# most `MAX_STEPS` bisection replays, and none started once the budget cannot fit another.
# What it could not narrow further it reports as a range, never as a guess.
#
# Usage:
#   convergence-bisect.sh last-green              print the newest SHA whose leg was green
#   convergence-bisect.sh bisect <good> <bad>     bisect and write the verdict
#
# Environment:
#   COUNTRY            the leg's country as its job is named (`poland`, `united-states`)
#   WORKFLOW           the calling workflow's name (`Country convergence`)
#   RUN_ID             this run, excluded from the green search
#   STEP_COMMAND       run once per candidate with the working tree at that commit; exits
#                      0 good, 1 bad, 125 untestable (git bisect's own convention)
#   STEP_MINUTES       how long one STEP_COMMAND may take
#   BUDGET_MINUTES     the whole bisection's ceiling (default 90)
#   MAX_STEPS          bisection replays at most (default 3)
#   SAMPLE_FAILED      'true' when the SAMPLE was red at <bad> — it is then known bad and
#                      is not replayed; otherwise <bad> is replayed once first, because a
#                      full leg's failure the sample cannot see cannot be bisected with it
#   VERDICT_FILE       where the markdown verdict goes (default: $GITHUB_STEP_SUMMARY)
#   FIRST_BAD_FILE     where the first bad SHA goes when it is pinned exactly (optional)
#
# Tested by ConvergenceBisectSpec, which runs it against a scratch repository and a
# stub `gh`.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
paths_file="${CONVERGENCE_PATHS_FILE:-$here/../convergence-paths.txt}"

# The pipeline paths, as git pathspecs (`dir/**` becomes `dir/`), one per line. No
# `mapfile`: the spec runs this on macOS too, where /bin/bash is still 3.2.
pathspecs() { grep -Ev '^[[:space:]]*(#|$)' "$paths_file" | sed -E 's#/\*\*$#/#'; }

last_green() {
    local runs run sha
    runs=$(gh run list --workflow "$WORKFLOW" --branch main --limit 60 \
               --json databaseId,headSha --jq '.[] | "\(.databaseId) \(.headSha)"') || return 1
    while read -r run sha; do
        [ -z "$run" ] && continue
        [ "$run" = "${RUN_ID:-}" ] && continue
        # The FULL leg's job, green — a sample alone passing says nothing about the full leg.
        if gh run view "$run" --json jobs \
               --jq ".jobs[] | select(.name == \"$COUNTRY / convergence\" and .conclusion == \"success\") | .name" \
               2>/dev/null | grep -q .; then
            echo "$sha"
            return 0
        fi
    done <<< "$runs"
    return 1
}

verdict() { printf '%s\n' "$@" >> "${VERDICT_FILE:-${GITHUB_STEP_SUMMARY:-/dev/stdout}}"; }

bisect() {
    local good="$1" bad="$2"
    local started
    started=$(date +%s)
    verdict "### Convergence bisect — $COUNTRY" ""

    if [ "$good" = "$bad" ]; then
        verdict "The same commit \`${bad:0:9}\` was GREEN in an earlier run — this failure is not a commit's doing (a flake or a recording change). Not bisected."
        return 0
    fi
    if ! git merge-base --is-ancestor "$good" "$bad"; then
        verdict "Last green \`${good:0:9}\` is not an ancestor of \`${bad:0:9}\` — no range to bisect."
        return 0
    fi

    local specs=() line
    while IFS= read -r line; do specs+=("$line"); done < <(pathspecs)
    local range; range=$(git rev-list --first-parent "$good..$bad" -- "${specs[@]}")
    local count; count=$(printf '%s' "$range" | grep -c . || true)
    if [ "$count" -eq 0 ]; then
        verdict "No commit in \`${good:0:9}..${bad:0:9}\` touches the pipeline — the red comes from data or the recording, not code. Not bisected."
        return 0
    fi
    verdict "Range: \`${good:0:9}..${bad:0:9}\`, $count pipeline commit(s)."

    # A full leg's failure is only bisectable with the sample if the sample reproduces it.
    local code
    if [ "${SAMPLE_FAILED:-false}" != "true" ]; then
        git checkout -q --force "$bad"
        "$STEP_COMMAND"; code=$?
        if [ "$code" -ne 1 ]; then
            verdict "The sample leg does not reproduce the failure at \`${bad:0:9}\` (exit $code) — only the full leg sees it, and bisecting with the full leg is not worth its cost. Not bisected."
            return 0
        fi
    fi

    # The last green leg may have replayed an OLDER recorded pair: the pair is re-pinned
    # nightly, and a red that the new pair brings is red at every commit. Without this replay
    # the bisect would pin the range's first pipeline commit (or its only one, unreplayed)
    # and tell its author the change was theirs. Only a good commit that is green under THIS
    # pair makes the range's answer a commit's.
    if ! fits_budget; then
        verdict "No time left in the ${BUDGET_MINUTES:-90}-minute budget to confirm \`${good:0:9}\` is green under this recording. Not bisected."
        return 0
    fi
    git checkout -q --force "$good"
    "$STEP_COMMAND"; code=$?
    if [ "$code" -eq 1 ]; then
        verdict "The sample is red at the last green commit \`${good:0:9}\` too, under this recording — the red comes from the recorded pair, not from a commit. Not bisected."
        return 0
    elif [ "$code" -ne 0 ]; then
        verdict "The sample could not confirm \`${good:0:9}\` green under this recording (exit $code), so no commit in the range can be blamed. Not bisected."
        return 0
    fi

    if [ "$count" -eq 1 ]; then
        finish_exact "$range" "the only pipeline commit in the range"
        return 0
    fi

    # `git bisect run` drives the search; the wrapper enforces the caps. Exit 255 aborts
    # the run cleanly, leaving the remaining candidates in `refs/bisect/*`.
    local counter; counter="$(mktemp)"; echo 0 > "$counter"
    local wrapper; wrapper="$(mktemp)"
    cat > "$wrapper" <<EOF
#!/usr/bin/env bash
steps=\$(cat "$counter")
if [ "\$steps" -ge "${MAX_STEPS:-3}" ]; then exit 255; fi
now=\$(date +%s)
if [ \$(( ${BUDGET_MINUTES:-90} * 60 - (now - $started) )) -lt $(( ${STEP_MINUTES:-20} * 60 )) ]; then exit 255; fi
echo \$((steps + 1)) > "$counter"
"$STEP_COMMAND"
EOF
    chmod +x "$wrapper"

    git bisect start --first-parent "$bad" "$good" -- "${specs[@]}" >/dev/null
    git bisect run "$wrapper" >/dev/null 2>&1
    # Decided from bisect's own refs, not its prose: git 2.55 reworded "<sha> is the first
    # bad commit" to "… first 'bad' commit", and a parser keyed to the old wording reported
    # every pinned commit as an open range. What is left is everything under the newest bad
    # and above every good; when nothing but that bad remains, it is the first bad commit.
    local goods skips newest_bad remaining
    goods=$(git for-each-ref --format='^%(objectname)' 'refs/bisect/good-*')
    skips=$(git for-each-ref --format='%(objectname)' 'refs/bisect/skip-*')
    newest_bad=$(git rev-parse refs/bisect/bad)
    # shellcheck disable=SC2086
    remaining=$(git rev-list --first-parent "$newest_bad" $goods -- "${specs[@]}" | grep -vx "$newest_bad" || true)
    if [ -z "$remaining" ]; then
        finish_exact "$newest_bad" "after $(cat "$counter") bisection replay(s)"
    else
        verdict "Stopped after $(cat "$counter") replay(s) (cap: ${MAX_STEPS:-3} replays, ${BUDGET_MINUTES:-90} min). The first bad commit is one of:" ""
        # shellcheck disable=SC2086
        # A skipped commit stays listed: the step could not test it, so it may well be the
        # first bad one (bisect itself names it among the candidates when only skips remain).
        git rev-list --first-parent "$newest_bad" $goods -- "${specs[@]}" | while read -r sha; do
            local note=""
            if printf '%s\n' "$skips" | grep -qx "$sha"; then note=" — untested (the sample could not run here)"; fi
            verdict "- $(git log -1 --format='`%h` %s (%an)' "$sha")$note"
        done
    fi
    git bisect reset >/dev/null 2>&1 || true
    rm -f "$counter" "$wrapper"
}

# Whether another STEP_MINUTES replay still fits in BUDGET_MINUTES since `started`.
fits_budget() {
    [ $(( ${BUDGET_MINUTES:-90} * 60 - ($(date +%s) - started) )) -ge $(( ${STEP_MINUTES:-20} * 60 )) ]
}

finish_exact() {
    local sha="$1" why="$2"
    verdict "**First bad commit: $(git log -1 --format='`%h` %s (%an)' "$sha")** — $why."
    [ -n "${FIRST_BAD_FILE:-}" ] && echo "$sha" > "$FIRST_BAD_FILE"
    return 0
}

case "${1:-}" in
    last-green) last_green ;;
    bisect)     bisect "$2" "$3" ;;
    *) echo "usage: $0 last-green | bisect <good> <bad>" >&2; exit 2 ;;
esac
