#!/usr/bin/env bash
# script-test-width: 4 (its mutants run 4 at a time: scripts/run-script-tests charges the run its extra cores)
# THE JS/TS QUALITY RATCHET HOLDS, proved by mutation: each mutant below is js_quality_ratchet.py, or the engine it runs
# (baseline_ratchet.py), with ONE check taken out, and js_quality_ratchet_test.py must FAIL against every one -- a check whose removal no test notices could silently
# stop gating and let the production JS/TS grow unreviewed. Each mutant is applied by exact text; a mutant whose text is
# no longer in the tool exactly once fails this test too (the tool changed: re-aim the mutant at the new check).
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
tool="$REPO_ROOT/scripts/ci/js_quality_ratchet.py"
printf '\033[36m▸\033[0m js_quality_ratchet.py, mutated\n'
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# <name> @@ <exact text in the tool> @@ <what replaces it>
mutants=(
  'the gate is gone: no violation fails @@ ok = False
        print(f"{spec.title} GREW @@ ok = True
        print(f"{spec.title} GREW'
  'a frozen row rising passes @@ if value > base[key]: @@ if False:'
  'a new key past the limit passes @@ elif value > limit and not moved_from @@ elif False and not moved_from'
  'the measure'"'"'s limit is not compared @@ if limit_of(current, kind) > limit: @@ if False:'
  'tests and scripts are gated too @@ held=lambda p: p.startswith(production) or p + "/" in production) @@ held=lambda p: True)'
  'a directory key is not production @@ or p + "/" in production @@ or False'
  'a raised limit is no growth @@ if old_limit is not None and new_limit > old_limit: @@ if False:'
  'an added or raised row is no growth @@ if value > before.get(key, allowed) and not carried @@ if False and not carried'
  'growth never fails @@ if unjustified:
            found = True @@ if unjustified:
            found = False'
  'growth is judged on one trailer @@ trailers.unjustified_rises(rng, spec.baseline, trailer, rose) @@ trailers.unjustified_rises(rng, spec.baseline, "Complexity", rose)'
  'an unrecorded fall passes (THE FALL LOCK) @@     if not down:
        return False @@     if True:
        return False'
  'update keeps no production row @@ for key, value in sorted(of_kind(spec, current, kind).items())] @@ for key, value in []]'
  'a malformed row is skipped @@ raise ValueError(f"line {n}: not @@ return rows  # (f"line {n}: not'
)

mkdir -p "$work/mutants"
i=0
for m in "${mutants[@]}"; do
  i=$((i + 1))
  name=${m%% @@ *}; rest=${m#* @@ }; from=${rest%% @@ *}; to=${rest#* @@ }
  printf '%s' "$name" > "$work/mutants/$i.name"
  mkdir -p "$work/mutants/$i"
  # the tool and the modules it imports, side by side
  cp "$tool" "$REPO_ROOT/scripts/ci/baseline_ratchet.py" "$REPO_ROOT/scripts/ci/trailers.py" "$work/mutants/$i/"
  # the mutant is applied to whichever of the tool and its engine holds its text (exactly once, in exactly one)
  python3 -I -B - "$work/mutants/$i/js_quality_ratchet.py" "$work/mutants/$i/baseline_ratchet.py" "$from" "$to" <<'PY'
import sys
tool, engine, frm, to = sys.argv[1:]
texts = {path: open(path).read() for path in (tool, engine)}
counts = {path: text.count(frm) for path, text in texts.items()}
if sorted(counts.values()) != [0, 1]:
    open(tool, "w").write("raise SystemExit(99)  # mutant text not found exactly once\n")
    sys.exit(1)
path = next(p for p, n in counts.items() if n == 1)
open(path, "w").write(texts[path].replace(frm, to, 1))
PY
  check "the mutant '$name' is aimed at exactly one check in the tool" 0 "$?"
done
# -f: unittest stops at a mutant's first failing test, all it takes to kill one (the clean run below stays whole)
# shellcheck disable=SC2016 # expanded by the inner bash
seq 1 "$i" | "$REPO_ROOT/scripts/gov-parallel" --job mutation-tests -P 4 -I{} bash -c 'JS_QUALITY_RATCHET_UNDER_TEST="$1/mutants/$2/js_quality_ratchet.py" python3 "$3" -f > "$1/mutants/$2.out" 2>&1; echo $? > "$1/mutants/$2.rc"' \
  _ "$work" {} "$REPO_ROOT/scripts/ci/js_quality_ratchet_test.py"
for n in $(seq 1 "$i"); do
  check "js_quality_ratchet_test.py fails against the mutant: $(cat "$work/mutants/$n.name")" 1 \
    "$([ "$(cat "$work/mutants/$n.rc" 2> /dev/null)" != 0 ] && echo 1 || echo 0)"
done
# one failing test kills a mutant: each mutant's run stopped at it (-f), so the job stays inside its timeout
# (CiJobNearTimeout, Alert backtest #28: the whole suite against every mutant took 61 min on macOS)
check "js_quality_ratchet_test.py stopped at the first failure against every mutant" 0 \
  "$(grep -LE '^FAILED \((failures|errors)=1\)$' "$work"/mutants/*.out | wc -l | tr -d ' ')"
# ...and passes against the tool itself, so the failures above are the mutants'
python3 "$REPO_ROOT/scripts/ci/js_quality_ratchet_test.py" > "$work/out" 2>&1
check "js_quality_ratchet_test.py passes against the tool itself" 0 "$?"
spec_summary
