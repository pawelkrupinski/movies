#!/usr/bin/env bash
# ONE CHECK BESIDE THE REST (.github/workflows/precommit-check.yml's `check (rest)`): the pre-push hook runs its checks
# one after another, and in the rest's job `scripttests` (143-186 s) and then `sbt` (96-385 s) ran serially in 5 of 5
# sampled runs (2026-11-18) -- two checks that share nothing: the script self-tests are bash and Python, the sbt check
# a JVM. So the job runs the hook twice at once: once for <check> alone (PREPUSH_ONLY) and once for every other check
# of its share (PREPUSH_ELSEWHERE, plus <check>), each with its own temp dir; the job's verdict is both.
#
#   hook-beside.sh <check> -- <hook command...>
#
# $CHECKS names the share's checks (space-separated; precommit-share.sh checks): only when it names <check> AND another
# check do the two runs happen; else the command runs as it is. <check>'s output is kept in
# $TMPDIR/<check>-beside.log and printed whole after the rest's, so the two never interleave (the job's failure steps
# read $TMPDIR's logs too). Exit: a run's failure (anything but 0 and 75) first, else the stall signal (75), else 0.
set -uo pipefail

usage() { echo "usage: $0 <check> -- <hook command...>" >&2; exit 2; }
[ $# -ge 3 ] && [ "$2" = -- ] || usage
check=$1; shift 2

others=0 named=0
for c in ${CHECKS:-}; do if [ "$c" = "$check" ]; then named=1; else others=1; fi; done
if [ $named = 0 ] || [ $others = 0 ]; then exec "$@"; fi

log="${TMPDIR:-/tmp}/$check-beside.log"
mkdir -p "$(dirname "$log")"
PREPUSH_ONLY="$check" "$@" > "$log" 2>&1 &
beside=$!
# the job's timeout (or a cancel) ends both runs, not the foreground one alone
trap 'kill "$beside" 2>/dev/null; exit 143' TERM INT
echo "hook-beside: $check runs beside the rest's checks (its output below, once both end)"
PREPUSH_ELSEWHERE="${PREPUSH_ELSEWHERE:+$PREPUSH_ELSEWHERE }$check" "$@"
rest_rc=$?
wait "$beside"
beside_rc=$?
echo "── $check, run beside the rest (exit $beside_rc):"
cat "$log"
for rc in $rest_rc $beside_rc; do [ "$rc" = 0 ] || [ "$rc" = 75 ] || exit "$rc"; done
for rc in $rest_rc $beside_rc; do [ "$rc" = 0 ] || exit "$rc"; done
exit 0
