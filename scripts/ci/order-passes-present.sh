#!/usr/bin/env bash
# order-passes-present.sh <dir> <passes> <row>: are both corpus files the order comparison reads there?
#
# `order-compare` (country-convergence-leg.yml) reads pass<N>.bin.gz (ReplayCorpusFile) for each pass of
# <passes> ("0,1"), written by the row's pass jobs (`<row>-pass<N>`), which upload theirs only when their suite
# passed. A pass job that died -- ci-runner's OOM killing it with exit 137, Verify scrape recording #14 -- left
# its file out, and the comparison then failed as a TEST (NoSuchFileException at ReplayCorpusFile), a red with no
# order finding in it. So the comparison runs only when both files are here; otherwise this names the pass row
# that did not finish, as the leg's verdict ($RUNNER_TEMP/leg-verdict-why) and as an ::error::, and exits 1.
# Tested by scripts/ci/order-passes-present-test.sh.
set -uo pipefail
dir=${1:?usage: order-passes-present.sh <dir> <passes> <row>}
passes=${2:?usage: order-passes-present.sh <dir> <passes> <row>}
row=${3:?usage: order-passes-present.sh <dir> <passes> <row>}

missing=()
IFS=, read -r -a wanted <<< "$passes"
for pass in "${wanted[@]}"; do
    [ -s "$dir/pass$pass.bin.gz" ] || missing+=("$row-pass$pass")
done
[ "${#missing[@]}" = 0 ] && exit 0

why="pass row(s) ${missing[*]} did not finish (no corpus file): the order comparison was not run -- read that row's own failure"
echo "::error::$why"
[ -n "${RUNNER_TEMP:-}" ] && echo "$why" > "$RUNNER_TEMP/leg-verdict-why"
exit 1
