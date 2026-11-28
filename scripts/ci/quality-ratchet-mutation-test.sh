#!/usr/bin/env bash
# script-test-width: 4 (its mutants run 4 at a time: scripts/run-script-tests charges the run its extra cores)
# THE CODE-QUALITY RATCHETS HOLD, proved by mutation: each mutant below is quality_ratchet.py with ONE check taken out,
# and quality_ratchet_test.py must FAIL against every one -- a check whose removal no test notices could silently stop
# working and let complexity, length or duplication grow unreviewed (or a data table hold logic, or be declared
# without a why). Each mutant is applied by exact text; a mutant
# whose text is no longer in the tool exactly once fails this test too (the tool changed: re-aim the mutant at the new
# check). A mutant named `trailers.py: ...` or `baseline_ratchet.py: ...` mutates the module the tool imports (the
# attribution rule, the rename credit live there), each
# mutant in a directory of its own beside its own copy of that module. Row parsing is the engine's
# (baseline_ratchet.parse_rows), mutated by js-quality-ratchet-mutation-test.sh.
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
tool="$REPO_ROOT/scripts/ci/quality_ratchet.py"
printf '\033[36m▸\033[0m quality_ratchet.py, mutated\n'
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# <name> @@ <exact text in the tool> @@ <what replaces it>
mutants=(
  'a frozen row rising passes @@ if key in base and value > base[key]: @@ if False:'
  'a new key past the limit passes @@ elif key not in base and value > limit and not moved_from @@ elif False and not moved_from'
  'the measure'"'"'s limit is not compared @@ if limit_of(current, kind) > limit else [] @@ if False else []'
  'a missing limit row passes @@ raise ValueError(f"no `limit @@ return 10**9  # (f"no `limit'
  'a raised limit is no growth @@ if old_limit is not None and new_limit > old_limit: @@ if False:'
  'an added or raised row is no growth @@ if value > before.get(key, allowed_new) and not @@ if False and not'
  'trailers.py: the trailer is not read @@ return bool(value.strip()) @@ return True'
  'trailers.py: any trailer counts @@ repository(repo).trailer_values(sha, key) @@ [v for _k, v in repository(repo).trailers(sha)]'
  'trailers.py: any commit of the range justifies a rise @@ if own and not carries(sha, key, repo) @@ if own and not any(carries(s, key, repo) for s in changed)'
  'trailers.py: a step is judged against the range'"'"'s base @@ return rose(text_at(f"{sha}^", path, repo), @@ return rose(text_at("HEAD~1000", path, repo),'
  'growth is judged on another ratchet'"'"'s trailer @@ trailers.unjustified_rises(rng, ratchet.baseline, ratchet.trailer, @@ trailers.unjustified_rises(rng, ratchet.baseline, "Complexity",'
  'growth never fails @@ if not unjustified: @@ if True:'
  'violations never fail @@ failed = bool(found) or any(grew) @@ failed = any(grew)'
  'baseline_ratchet.py: a move needs no departure @@ if old not in present and moved_key(old, renames) == key and was >= value), None) @@ if moved_key(old, renames) == key and was >= value), None)'
  'baseline_ratchet.py: a move may grow @@ if old not in present and moved_key(old, renames) == key and was >= value), None) @@ if old not in present and moved_key(old, renames) == key), None)'
  'baseline_ratchet.py: a same-named source is a move @@ if old not in present and moved_key(old, renames) == key and was >= value), None) @@ if old not in present and old.rsplit("/", 1)[-1] == key.rsplit("/", 1)[-1] and was >= value), None)'
  'baseline_ratchet.py: a deleted source counts as renamed @@             if path not in renames:
                return None @@             if path not in renames:
                return path.replace("a/src/main/scala/", "b/src/main/scala/x/")'
  'baseline_ratchet.py: a move may change the method name @@ if old not in present and moved_key(old, renames) == key and was >= value), None) @@ if old not in present and (moved_key(old, renames) or "").partition(":")[0] == key.partition(":")[0] and was >= value), None)'
  'a move is growth @@ and not (key not in before and moved_from(key, value, before, after, renames)): @@ :'
  'a declared table holding logic passes @@         if key in data:
            continue @@         if True:
            continue'
  'a table row'"'"'s why is not linted @@ return cols, problems + [f"{where}: {cols[1]}'"'"'s why: {p}" for p in table_reason_problems(cols[1], cols[3])] @@ return cols, problems'
  'a table row needs no date @@ if not ISO_DATE.match(cols[2]): @@ if False:'
  'a generic why passes @@ out += [f"`{phrase}` is generic: say what the table holds and what reads it" for rx, phrase in GENERIC_REASONS @@ out += [f"`{phrase}` is generic: say what the table holds and what reads it" for rx, phrase in ()'
  'a malformed table row is skipped @@ return None, [f"{where}: not `table<TAB> @@ return None, [] and [f"{where}: not `table<TAB>'
  'a table added needs no trailer @@         if refused:
            problems.append @@         if False:
            problems.append'
  'the data tables never fail @@     if problems:
        print(f"THE DATA TABLES @@     if False:
        print(f"THE DATA TABLES'
)

mkdir -p "$work/mutants"
i=0
for m in "${mutants[@]}"; do
  i=$((i + 1))
  name=${m%% @@ *}; rest=${m#* @@ }; from=${rest%% @@ *}; to=${rest#* @@ }
  printf '%s' "$name" > "$work/mutants/$i.name"
  mkdir -p "$work/mutants/$i"
  # the tool and the modules it imports, side by side
  cp "$tool" "$REPO_ROOT/scripts/ci/trailers.py" "$REPO_ROOT/scripts/ci/coupling_reasons.py" \
     "$REPO_ROOT/scripts/ci/baseline_ratchet.py" "$work/mutants/$i/"
  target="$work/mutants/$i/quality_ratchet.py"
  case "$name" in
    "trailers.py: "*) target="$work/mutants/$i/trailers.py" ;;
    "baseline_ratchet.py: "*) target="$work/mutants/$i/baseline_ratchet.py" ;; # the engine's rename credit (moved_from, Renames)
  esac
  python3 -I -B - "$target" "$target" "$from" "$to" <<'PY'
import sys
src, dst, frm, to = sys.argv[1:]
text = open(src).read()
n = text.count(frm)
open(dst, "w").write(text.replace(frm, to, 1) if n == 1 else "raise SystemExit(99)  # mutant text not found exactly once\n")
sys.exit(0 if n == 1 else 1)
PY
  check "the mutant '$name' is aimed at exactly one check in the tool" 0 "$?"
done
# -f: unittest stops at a mutant's first failing test, all it takes to kill one (the clean run below stays whole)
# shellcheck disable=SC2016 # expanded by the inner bash
seq 1 "$i" | "$REPO_ROOT/scripts/gov-parallel" --job mutation-tests -P 4 -I{} bash -c 'QUALITY_RATCHET_UNDER_TEST="$1/mutants/$2/quality_ratchet.py" python3 "$3" -f > "$1/mutants/$2.out" 2>&1; echo $? > "$1/mutants/$2.rc"' \
  _ "$work" {} "$REPO_ROOT/scripts/ci/quality_ratchet_test.py"
for n in $(seq 1 "$i"); do
  check "quality_ratchet_test.py fails against the mutant: $(cat "$work/mutants/$n.name")" 1 \
    "$([ "$(cat "$work/mutants/$n.rc" 2> /dev/null)" != 0 ] && echo 1 || echo 0)"
done
# one failing test kills a mutant: each mutant's run stopped at it (-f), so the job stays inside its timeout
# (CiJobNearTimeout, Alert backtest #28: the whole suite against every mutant took 61 min on macOS)
check "quality_ratchet_test.py stopped at the first failure against every mutant" 0 \
  "$(grep -LE '^FAILED \((failures|errors)=1\)$' "$work"/mutants/*.out | wc -l | tr -d ' ')"
# ...and passes against the tool itself, so the failures above are the mutants'
python3 "$REPO_ROOT/scripts/ci/quality_ratchet_test.py" > "$work/out" 2>&1
check "quality_ratchet_test.py passes against the tool itself" 0 "$?"
spec_summary
