#!/usr/bin/env bash
# THE COUPLING RATCHET HOLDS, proved by mutation: each mutant below is coupling_ratchet.py with ONE check taken out, and
# coupling_ratchet_test.py must FAIL against every one -- a check whose removal no test notices could silently stop
# working and let coupling grow unreviewed. Each mutant is applied by exact text; a mutant whose text is no longer in
# the tool exactly once fails this test too (the tool changed: re-aim the mutant at the new check).
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
tool="$REPO_ROOT/scripts/ci/coupling_ratchet.py"
printf '\033[36m▸\033[0m coupling_ratchet.py, mutated\n'
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# <name> @@ <exact text in the tool> @@ <what replaces it>
mutants=(
  'a new module edge passes @@ if key not in of_kind(baseline, "module-edge"): @@ if False:'
  'a new cycle edge passes @@ if key not in of_kind(baseline, "cycle-edge"): @@ if False:'
  'Ce is not compared @@         if ce > allowed:
            was @@         if False:
            was'
  'a new package has no limit @@ allowed = base_ce.get(key, lim["new-package-ce"]) @@ allowed = base_ce.get(key, 10**9)'
  'a fan-out row is not compared @@ if fan > base_fan[key]: @@ if False:'
  'an unlisted class has no fan-out limit @@ elif fan >= lim["fanout"]: @@ elif False:'
  'the baseline limits are not read @@ out.update({key: value for (kind, key), value in rows.items() if kind == "limit"}) @@ pass'
  'a raised limit is no growth @@ if new_lim[key] > old_lim.get(key, 0): @@ if False:'
  'an added edge row is no growth @@ for key in sorted(set(of_kind(new, kind)) - set(of_kind(old, kind))): @@ for key in []:'
  'a raised Ce row is no growth @@         if ce > allowed:
            out.append(f"ce @@         if False:
            out.append(f"ce'
  'a raised fan-out row is no growth @@ if fan > old_fan.get(key, old_lim["fanout"] - 1): @@ if False:'
  'the trailer is not read @@ return any(entry.strip() for entry in log.split("\x00")) @@ return True'
  'any commit counts for the trailer @@ rng, "--", BASELINE_PATH) @@ rng)'
  'growth never fails @@ if grown and not justified(rng): @@ if False:'
  'violations never fail @@     if found:
        failed = True @@     if found:
        failed = False'
  'a malformed row is skipped @@ raise ValueError(f"line {n}: not @@ continue  # (f"line {n}: not'
)

mkdir -p "$work/mutants"
i=0
for m in "${mutants[@]}"; do
  i=$((i + 1))
  name=${m%% @@ *}; rest=${m#* @@ }; from=${rest%% @@ *}; to=${rest#* @@ }
  printf '%s' "$name" > "$work/mutants/$i.name"
  python3 -I - "$tool" "$work/mutants/$i.py" "$from" "$to" <<'PY'
import sys
src, dst, frm, to = sys.argv[1:]
text = open(src).read()
n = text.count(frm)
open(dst, "w").write(text.replace(frm, to, 1) if n == 1 else "raise SystemExit(99)  # mutant text not found exactly once\n")
sys.exit(0 if n == 1 else 1)
PY
  check "the mutant '$name' is aimed at exactly one check in the tool" 0 "$?"
done
# shellcheck disable=SC2016 # expanded by the inner bash
seq 1 "$i" | xargs -P 4 -I{} bash -c 'COUPLING_RATCHET_UNDER_TEST="$1/mutants/$2.py" python3 "$3" > "$1/mutants/$2.out" 2>&1; echo $? > "$1/mutants/$2.rc"' \
  _ "$work" {} "$REPO_ROOT/scripts/ci/coupling_ratchet_test.py"
for n in $(seq 1 "$i"); do
  check "coupling_ratchet_test.py fails against the mutant: $(cat "$work/mutants/$n.name")" 1 \
    "$([ "$(cat "$work/mutants/$n.rc" 2> /dev/null)" != 0 ] && echo 1 || echo 0)"
done
# ...and passes against the tool itself, so the failures above are the mutants'
python3 "$REPO_ROOT/scripts/ci/coupling_ratchet_test.py" > "$work/out" 2>&1
check "coupling_ratchet_test.py passes against the tool itself" 0 "$?"
spec_summary
