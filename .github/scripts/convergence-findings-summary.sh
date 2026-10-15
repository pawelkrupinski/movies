#!/usr/bin/env bash
#
# One convergence run's ScalaTest narration and phase timings, as markdown on stdout — the job summary
# .github/actions/convergence-findings appends.
#
#   convergence-findings-summary.sh <title> <log>
#
# The filter below is the part that must not drift: it is the list of what counts as narration versus
# application logging, and a copy that fell behind would silently stop rendering the phase timings that
# are the only way to diagnose a leg while it is still running.
# Tested by .github/scripts/convergence-findings-summary-test.sh.
set -uo pipefail

[ "$#" -eq 2 ] || { echo "usage: $0 <title> <log>" >&2; exit 64; }
title="$1" log="$2"

if [ ! -s "$log" ]; then
    echo "### $title"
    echo "The suite produced no output — see the job log."
    exit 0
fi
# sbt colours its output; the escapes are noise in markdown.
# `|| true` on every grep: grep exits 1 when it matches NOTHING, and under `-e` a run with no `Tests:` line
# killed the report before it printed a character — precisely the cancelled-or-crashed run whose partial
# output the report exists to salvage.
clean=$(sed -E 's/\x1b\[[0-9;]*m//g' "$log")
totals=$(printf '%s\n' "$clean" | { grep -E '^\[info\] Tests: ' || true; } | tail -1 | sed 's/^\[info\] //')
echo "### $title"
echo
if [ -n "$totals" ]; then echo "**$totals**"; else echo "**No test totals — the suite did not finish.**"; fi
echo
echo '```'
# The spec's own structure — suite, `should` clauses, the `+` info lines — PLUS the `[pl] phase done in …`
# timings the harness prints as it goes. Everything else on stdout is application logging, not a report.
#
# The scope is a COUNTRY CODE, OPTIONALLY `p<pass>`: PhaseTimer tags TestWiring's phases `[us]`, and the
# order-independence replays run `Passes` of them concurrently, each naming itself `[usp0]`, `[usp1]`, …
# (CountryConvergenceBehaviour's `scope`). A `[a-z]{2}`-only filter dropped every pass, and so did the
# `[a-z]{2}[0-9]*` that was meant to admit them: the `p` is not a digit.
#
# `sed -n …; 1,150p` keeps the first 150 lines but READS TO THE END. A `| head -150` closed the pipe early,
# sed died writing into it ("couldn't flush stdout: Broken pipe", exit 4), and under `pipefail` that failed a
# green leg the moment its report passed 150 lines (Spain, run 37096642753).
printf '%s\n' "$clean" \
  | { grep -E '^(\[info\] )?([A-Za-z0-9]+Spec:|- should |  \+ |the |\[[a-z]{2}(p[0-9]+)?\] )' || true; } \
  | sed -n 's/^\[info\] //; 1,150p'
echo '```'
