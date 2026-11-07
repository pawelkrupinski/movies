#!/usr/bin/env bash
# THE TAKES SKIP GATE'S PROOF (scripts/ci/takes-skip-gate.sh, step 7): services.movies.NormalizerTraceCheck, compiled in
# this checkout -- the head -- and run with the arguments the gate hands it (`--out <differ.tsv> <trace>`). The gate asks
# for it only once every cheaper condition holds, so a row whose change is not a title-rule one never compiles it.
# Its exit is the check's (0 every reading alike, 1 some differ, 3 some cannot be made again); 2 when it could not run.
#
# usage: takes-skip-proof.sh --out <differ.tsv> <trace.jsonl.gz>     env: TAKES_SKIP_PROOF_ROOT (default: this checkout)
set -uo pipefail
root=${TAKES_SKIP_PROOF_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}
# identity-tools.sh's IDENTITY_SBT and IDENTITY_JAVA, its locale flags and all: a title read under another default locale
# can read otherwise (not sourced: it reaches the laptop tooling, none of which this needs)
locale=(-Duser.language=en -Duser.country=US)
sbt_cmd=(sbt "${locale[@]/#/-J}")
java_cmd=(java -Xmx6g "${locale[@]}" -Djava.awt.headless=true --sun-misc-unsafe-memory-access=allow)
log=$(mktemp) || exit 2
trap 'rm -f "$log"' EXIT
( cd "$root" && "${sbt_cmd[@]}" -batch "testkit/compile" "export testkit/Runtime/fullClasspath" ) > "$log" 2>&1 \
  || { tail -20 "$log" >&2; echo "takes-skip-proof: testkit did not compile" >&2; exit 2; }
cp=$(sed 's/\x1b\[[0-9;]*m//g' "$log" | grep -E '^/[^ ]*testkit/target/' | tail -n 1)
[ -n "$cp" ] || { echo "takes-skip-proof: no testkit classpath in sbt's output" >&2; exit 2; }
exec "${java_cmd[@]}" -cp "$cp" services.movies.NormalizerTraceCheck "$@"
