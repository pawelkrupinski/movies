#!/usr/bin/env bash
# IS THE IDENTITY CI STILL TELLING US ANYTHING? Exits 1, naming each check that has gone stale.
#
# WHY. The identity resolver's code-caused regressions are caught by CI, never by Prometheus: the
# identity takes check and the suspect-take ratchet in every leg of `Identity model convergence`,
# replaying the pairs the nightly `Record scrape fixtures` pinned. Both can go quiet without anyone
# being told:
#
#   - convergence is DISPATCHED by Main (workflow_dispatch), so its `report` job -- which runs on
#     `schedule` only -- never fires: a red leg sat red from 2026-10-13 12:50Z to 22:30Z (~10h of
#     pushes judged against it) and again from 2026-10-14 02:52Z, with no issue opened either time;
#   - a pipeline commit whose dispatch was refused, or whose run was cancelled and never replaced,
#     simply has no verdict;
#   - the nightly recording's own report opens an issue for a FAILED scheduled run, but not for one
#     that was cancelled, timed out before reporting, or never started -- and the legs then replay
#     an ever older pinned pair.
#
# The checks, each against `gh run list` and, for coverage, this checkout's history:
#
#   convergence-red     the newest decided (success|failure) convergence run on main failed, and
#                       the red streak began (its first failure completed) over
#                       CONVERGENCE_RED_HOURS ago;
#   convergence-unrun   the newest first-parent commit touching `.github/convergence-paths.txt`
#                       landed over CONVERGENCE_UNRUN_HOURS ago and no convergence run that is
#                       queued, running or decided (not cancelled) is at it or a descendant of it;
#   recording-stale     no `Record scrape fixtures` run on main has succeeded in
#                       RECORDING_STALE_HOURS (36: one missed nightly plus slack).
#
# Exit 1 is the whole report: `.github/workflows/identity-ci-staleness.yml` runs this on a
# schedule, and its `report` job turns a red run into the owner-assigned issue every scheduled
# workflow files (scripts/ci/scheduled-failure-issue.sh), closed by the next green run.
#
# Needs GH_TOKEN and GH_REPO, jq, and a checkout of main with its history (fetch-depth: 0).
# NOW (epoch seconds) is the clock, injectable for the test.
# Tested by scripts/ci/identity-ci-staleness-test.sh against a stub `gh` and a scratch repository.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
now="${NOW:-$(date +%s)}"
red_hours="${CONVERGENCE_RED_HOURS:-6}"
unrun_hours="${CONVERGENCE_UNRUN_HOURS:-3}"
recording_hours="${RECORDING_STALE_HOURS:-36}"
paths_file="${CONVERGENCE_PATHS_FILE:-$here/../../.github/convergence-paths.txt}"
convergence_workflow="${CONVERGENCE_WORKFLOW:-identity-model-convergence.yml}"
recording_workflow="${RECORDING_WORKFLOW:-record-scrape-fixtures.yml}"

stale=0
verdict() {  # verdict <check> <stale|fresh> <message>
  if [ "$2" = stale ]; then echo "::error::$1: $3"; stale=1; else echo "ok  $1: $3"; fi
}

runs() {  # runs <workflow file> -> JSON array, newest first
  gh run list --workflow "$1" --branch main --limit 100 \
    --json headSha,status,conclusion,createdAt,updatedAt,url
}

# A failed read is not "nothing is wrong": it is reported as stale, so the check cannot go quiet.
if ! convergence_runs="$(runs "$convergence_workflow")"; then
  verdict convergence "stale" "could not list $convergence_workflow runs"
  convergence_runs='[]'
fi
if ! recording_runs="$(runs "$recording_workflow")"; then
  verdict recording-stale "stale" "could not list $recording_workflow runs"
  recording_runs='[]'
fi

# ── convergence-red ──────────────────────────────────────────────────────────────────────────────
# The streak: decided runs, newest first, up to the newest success. Its start is the oldest one's
# completion -- when CI first said red -- not the newest's.
red="$(jq -r --argjson now "$now" --argjson hours "$red_hours" '
  [ sort_by(.createdAt) | reverse | .[]
    | select((.updatedAt | fromdateiso8601) <= $now)
    | select(.status == "completed" and (.conclusion == "success" or .conclusion == "failure")) ]
  | (map(.conclusion == "failure") | index(false) // length) as $n
  | .[0:$n] as $streak
  | if ($streak | length) == 0 then "fresh newest decided run is green (or there is none)"
    else ($streak | last | .updatedAt) as $since
      | (($now - ($since | fromdateiso8601)) / 3600 | floor) as $age
      | (if $age >= $hours then "stale" else "fresh" end)
        + " red since \($since) (\($age)h, \($streak | length) failed run(s)); newest \($streak[0].url)"
    end' <<<"$convergence_runs")"
verdict convergence-red "${red%% *}" "${red#* }"

# ── convergence-unrun ────────────────────────────────────────────────────────────────────────────
# `dir/**` is git's `dir/`; any other entry is an exact path (.github/actions/changed-paths/matches.sh).
pathspecs=()
while IFS= read -r pattern; do
  pathspecs+=("${pattern%\*\*}")
done < <(sed -e 's/[[:space:]]*#.*//' -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' "$paths_file" | grep -v '^$' | grep -v '^!')

read -r pipeline_sha pipeline_time < <(git log -1 --first-parent --format='%H %ct' HEAD -- "${pathspecs[@]}")
if [ -z "${pipeline_sha:-}" ]; then
  verdict convergence-unrun fresh "no commit touches the convergence paths"
else
  covered=""
  # Queued, running or decided runs only: a cancelled one was replaced, or never ran.
  for sha in $(jq -r '.[] | select(.conclusion != "cancelled" and .conclusion != "skipped") | .headSha' <<<"$convergence_runs"); do
    if git merge-base --is-ancestor "$pipeline_sha" "$sha" 2>/dev/null; then covered="$sha"; break; fi
  done
  age=$(( (now - pipeline_time) / 3600 ))
  if [ -n "$covered" ]; then
    verdict convergence-unrun fresh "pipeline commit ${pipeline_sha:0:9} is covered by a run at ${covered:0:9}"
  elif [ "$age" -ge "$unrun_hours" ]; then
    verdict convergence-unrun stale "pipeline commit ${pipeline_sha:0:9} landed ${age}h ago and no convergence run covers it"
  else
    verdict convergence-unrun fresh "pipeline commit ${pipeline_sha:0:9} is ${age}h old, not yet due a run"
  fi
fi

# ── recording-stale ──────────────────────────────────────────────────────────────────────────────
recording="$(jq -r --argjson now "$now" --argjson hours "$recording_hours" '
  [ .[] | select(.status == "completed" and .conclusion == "success")
    | select((.updatedAt | fromdateiso8601) <= $now) ] | sort_by(.updatedAt) | last
  | if . == null then "stale no successful run among the last 100"
    else (($now - (.updatedAt | fromdateiso8601)) / 3600 | floor) as $age
      | (if $age >= $hours then "stale" else "fresh" end) + " last success \(.updatedAt) (\($age)h ago): \(.url)"
    end' <<<"$recording_runs")"
verdict recording-stale "${recording%% *}" "${recording#* }"

exit "$stale"
