#!/usr/bin/env bash
# ALERT, NOT RED: run a check whose failure of a given kind is a THIRD PARTY's or a BACKLOG's -- a vendor down, a quota
# or key refused, an upstream run with nothing to report, a checker that cannot read Forgejo -- and turn that kind into
# an ALERT: the step (and the run) stays green, and `alert=true` plus `found=<its first ::error::/::warning:: line>` go
# to $GITHUB_OUTPUT for the workflow's report job (.github/actions/report-scheduled-run `alert`: one tracked issue,
# refreshed while it lasts, closed by the next clean run). Any other non-zero exit is the check's own failure -- a
# regression of ours, a broken script -- and fails the step as ever.
#
# WHY (ci-failures-2026-10-30, recommendation 2): in five days, 34 red runs and ~45 red hours were providers' (Decodo
# 21 runs in a row, security headers 6, OMDb's quota, Cineworld), and 11 were backlogs and meta-alarms; each red was a
# dispatch candidate, and the fixer budget went on outages only the owner could act on.
#
# Usage: alert-step.sh <alert exit codes, comma-separated> -- <command> [args...]
#   e.g. alert-step.sh 1,2 -- python3 scripts/ci/decodo-probe.py
# Exit: 0 when the command passed or exited with an alert code; else the command's exit.
# Tested by scripts/ci/alert-step-test.sh.
set -uo pipefail

codes="${1:?alert exit codes, comma-separated}"; shift
[ "${1:-}" = "--" ] && shift
[ $# -gt 0 ] || { echo "alert-step.sh: no command" >&2; exit 2; }

log="$(mktemp)"
trap 'rm -f "$log"' EXIT
"$@" 2>&1 | tee "$log"
rc=${PIPESTATUS[0]}

out() { [ -z "${GITHUB_OUTPUT:-}" ] || printf '%s\n' "$@" >> "$GITHUB_OUTPUT"; }
if [ "$rc" = 0 ]; then
    out "alert=false"
    exit 0
fi
case ",$codes," in
    *",$rc,"*)
        found="$(grep -m1 -E '::(error|warning)[^:]*::' "$log" | sed -E 's/^.*::(error|warning)[^:]*:://' | cut -c1-300)"
        [ -n "$found" ] || found="$(grep -v '^[[:space:]]*$' "$log" | tail -1 | cut -c1-300)"
        echo "::warning::ALERT (exit $rc: a third party's or a backlog's, not a regression of ours; the run stays green): $found"
        out "alert=true" "found=$found"
        exit 0 ;;
esac
out "alert=false"
exit "$rc"
