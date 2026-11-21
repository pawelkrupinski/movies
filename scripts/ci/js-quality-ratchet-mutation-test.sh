#!/usr/bin/env bash
# THE JS/TS QUALITY RATCHET HOLDS, proved by mutation: each mutant below is js_quality_ratchet.py with ONE check taken
# out, and js_quality_ratchet_test.py must FAIL against every one -- a check whose removal no test notices could silently
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
        print(f"JS/TS QUALITY GREW @@ ok = True
        print(f"JS/TS QUALITY GREW'
  'a frozen row rising passes @@ if value > base[key]: @@ if False:'
  'a new key past the limit passes @@ elif value > limit: @@ elif False:'
  'the measure'"'"'s limit is not compared @@ if limit_of(current, kind) > limit: @@ if False:'
  'tests and scripts are gated too @@ return any(p.startswith(production) or p + "/" in production for p in key.split("|")) @@ return True'
  'a directory key is not production @@ or p + "/" in production @@ or False'
  'a raised limit is no growth @@ if old_limit is not None and new_limit > old_limit: @@ if False:'
  'an added or raised row is no growth @@ if value > before.get(key, allowed): @@ if False:'
  'growth never fails @@ if unjustified:
                ok = False @@ if unjustified:
                ok = True'
  'growth is judged on one trailer @@ trailers.unjustified_rises(rng, BASELINE, trailer, rose) @@ trailers.unjustified_rises(rng, BASELINE, "Complexity", rose)'
  'falls are not said @@     if down:
        print @@     if False:
        print'
  'update keeps no production row @@ for key, value in sorted(of_kind(current, kind, production).items())] @@ for key, value in []]'
  'a malformed row is skipped @@ raise ValueError(f"line {n}: not @@ return rows  # (f"line {n}: not'
)

mkdir -p "$work/mutants"
i=0
for m in "${mutants[@]}"; do
  i=$((i + 1))
  name=${m%% @@ *}; rest=${m#* @@ }; from=${rest%% @@ *}; to=${rest#* @@ }
  printf '%s' "$name" > "$work/mutants/$i.name"
  mkdir -p "$work/mutants/$i"
  cp "$tool" "$REPO_ROOT/scripts/ci/trailers.py" "$work/mutants/$i/"  # the tool and the module it imports, side by side
  target="$work/mutants/$i/js_quality_ratchet.py"
  python3 -I - "$target" "$from" "$to" <<'PY'
import sys
path, frm, to = sys.argv[1:]
text = open(path).read()
n = text.count(frm)
open(path, "w").write(text.replace(frm, to, 1) if n == 1 else "raise SystemExit(99)  # mutant text not found exactly once\n")
sys.exit(0 if n == 1 else 1)
PY
  check "the mutant '$name' is aimed at exactly one check in the tool" 0 "$?"
done
# shellcheck disable=SC2016 # expanded by the inner bash
seq 1 "$i" | xargs -P 4 -I{} bash -c 'JS_QUALITY_RATCHET_UNDER_TEST="$1/mutants/$2/js_quality_ratchet.py" python3 "$3" > "$1/mutants/$2.out" 2>&1; echo $? > "$1/mutants/$2.rc"' \
  _ "$work" {} "$REPO_ROOT/scripts/ci/js_quality_ratchet_test.py"
for n in $(seq 1 "$i"); do
  check "js_quality_ratchet_test.py fails against the mutant: $(cat "$work/mutants/$n.name")" 1 \
    "$([ "$(cat "$work/mutants/$n.rc" 2> /dev/null)" != 0 ] && echo 1 || echo 0)"
done
# ...and passes against the tool itself, so the failures above are the mutants'
python3 "$REPO_ROOT/scripts/ci/js_quality_ratchet_test.py" > "$work/out" 2>&1
check "js_quality_ratchet_test.py passes against the tool itself" 0 "$?"
spec_summary
