#!/usr/bin/env bash
# script-test-width: 4 (its mutants run 4 at a time: scripts/run-script-tests charges the run its extra cores)
# THE COUPLING RATCHET HOLDS, proved by mutation: each mutant below is coupling_ratchet.py with ONE check taken out, and
# coupling_ratchet_test.py must FAIL against every one -- a check whose removal no test notices could silently stop
# working and let coupling grow unreviewed. Each mutant is applied by exact text; a mutant whose text is no longer in
# the tool exactly once fails this test too (the tool changed: re-aim the mutant at the new check).
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m coupling_ratchet.py, mutated\n'
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# <name> @@ <exact text in the tool's modules (coupling_*.py)> @@ <what replaces it>
mutants=(
  'a deeper package with a row of its own is cut to two segments @@ held = [u for u in units if full == u or full.startswith(u + ".")] @@ held = []'
  'a distance re-measure raises the ceilings with the rows @@ if not refrozen(old, new, "distance") or remeasured_only(old, new, "distance"): @@ if not refrozen(old, new, "distance"):'
  'a new module edge passes @@ if key not in known] @@ if False]'
  'a new cycle passes @@ for key, refs in sorted(of_kind(current, "cycle-edge").items())] @@ for key, refs in []]'
  'a cycle recorded in the baseline passes @@ recorded = sorted(of_kind(baseline, "cycle-edge")) @@ recorded = []'
  'a refused edge names no class reference @@ }" if refs else "" @@ }" if False else ""'
  'an unlayered new package passes @@ for key, refs in sorted(of_kind(current, "layer-violation").items()):
        a, b = key.split("->")
        if b == "?": @@ for key, refs in sorted(of_kind(current, "layer-violation").items()):
        a, b = key.split("->")
        if b == "?" and False:'
  'an upward edge inside a layer passes @@ for key, refs in sorted(of_kind(current, "rank-violation").items()): @@ for key, refs in [kv for kv in sorted(of_kind(current, "rank-violation").items()) if kv[0].endswith("->?")]:'
  'a package without its rank passes @@ out.append(f"package {a} has no rank in its layer @@ pass  # (f"package {a} has no rank in its layer'
  'a re-ranked package is no loosening @@ if old[2][pkg] != new[2][pkg]: @@ if False:'
  'a layer violation passes @@ for key, refs in sorted(of_kind(current, "layer-violation").items()): @@ for key, refs in []:'
  'Ce is not compared @@         if ce > allowed:
            was @@         if False:
            was'
  'a new package has no limit @@ base_ce.get(key, limits(baseline)["new-package-ce"]) @@ base_ce.get(key, 10**9)'
  'a frozen row is not compared @@ if value > base[key]:
                out.append(f"{noun} @@ if False:
                out.append(f"{noun}'
  'a class or package newly past a limit passes @@ elif not moved(key, value, base, cur) and (kind, key) not in renamed: @@ elif False:'
  'any frozen row counts as moved @@ return any(v >= value for v in gone) and len(arrived) <= len(gone) @@ return True'
  'one frozen row moves to two classes @@ return any(v >= value for v in gone) and len(arrived) <= len(gone) @@ return any(v >= value for v in gone)'
  'the baseline limits are not read @@ out.update({key: value for (kind, key), value in rows.items() if kind == "limit"}) @@ pass'
  'a raised limit is no growth @@ if new_lim[key] > old_lim.get(key, 0): @@ if False:'
  'an added edge row is no growth @@ for key in sorted(set(of_kind(new, kind)) - set(of_kind(old, kind)))] @@ for key in []]'
  'a raised Ce row is no growth @@         if ce > allowed:
            out.append(f"ce @@         if False:
            out.append(f"ce'
  'a raised frozen row is admitted @@ if value <= was or (kind @@ if True or (kind'
  'a new frozen row is admitted @@ elif not is_credited(kind, key, value, before, after, split, renamed): @@ elif False:'
  'a measurement fix takes no rows again @@ LIMIT_KEYS = {"fanout": ("fanout", "fanout-measure"), @@ LIMIT_KEYS = {"fanout": ("fanout",),'
  'an absent measure row re-freezes every range @@ and key not in MEASURES) @@ and True)'
  'every change re-freezes @@ return any((("limit", key) not in old and key not in MEASURES) or old_lim.get(key) != new_lim.get(key) @@ return True or any((("limit", key) not in old and key not in MEASURES) or old_lim.get(key) != new_lim.get(key)'
  'admitted frozen rows never fail @@ return bool(frozen) @@ return False'
  'an added exemption is no loosening @@         if key not in old:
            out.append(f"{key[0]} {key[1]} ({value}) added") @@         if False:
            out.append(f"{key[0]} {key[1]} ({value}) added")'
  'a changed exemption limit is no loosening @@ elif old[key] != value: @@ elif False:'
  'a loosened exemption table never fails @@ rise_refused(rng, EXEMPTIONS_PATH, @@ False and rise_refused(rng, EXEMPTIONS_PATH,'
  'the trailer is not read @@ return trailers.unjustified_rises(rng, path, "Coupling", rose) @@ return []'
  'growth never fails @@ rise_refused(rng, BASELINE_PATH, @@ False and rise_refused(rng, BASELINE_PATH,'
  'violations never fail @@ return bool(found) @@ return False'
  'an added allow row is no loosening @@ out = [f"allow {edge} added" for edge in sorted(new[1] - old[1])] @@ out = []'
  'a moved layer is no loosening @@ if was is not None and now is not None and was != now: @@ if False:'
  'a sub-package lifted by a new row is no loosening @@ named = [p for p in rows if pkg == p or pkg.startswith(p + ".")] @@ named = [p for p in rows if pkg == p]'
  'a loosened layer table never fails @@ rise_refused(rng, LAYERS_PATH, @@ False and rise_refused(rng, LAYERS_PATH,'
  'the layer table judged by the baseline'"'"'s commits @@ rise_refused(rng, LAYERS_PATH, @@ rise_refused(rng, BASELINE_PATH,'
  'the exemption table judged by the baseline'"'"'s commits @@ rise_refused(rng, EXEMPTIONS_PATH, @@ rise_refused(rng, BASELINE_PATH,'
  'a split piece is never admitted @@ or (kind == "fanout" and key in split) or @@ or False or'
  'a split claim needs no frozen original @@ if original is None:
            refused.append @@ if original is None:
            admitted |= set(named); continue
            refused.append'
  'a piece at the original passes @@ if value >= self.frozen[original]: @@ if value > self.frozen[original]:'
  'a piece adding a dependency passes @@ new = sorted(t for t in union if t not in was) @@ new = []'
  'what the original keeps is not held @@ return self.targets[original], [] @@ return set(), []'
  'the pieces referencing each other add dependencies @@  - (set(pieces) | {original}) @@ '
  'a reference set out of step with the row is trusted @@ if len(was) != self.frozen[original]: @@ if False:'
  'an unmeasured piece is trusted @@ why.append(f"{piece}'"'"'s references are not measured @@ pass  # (f"{piece}'"'"'s references are not measured'
  'a split may raise distance past its ceiling @@ and value <= cap @@ and True'
  'a raised ceiling passes @@ if key in was and cap > was[key]: @@ if False:'
  'a row without a ceiling has none @@ caps.get(pkg, d + DISTANCE_SPLIT_TOLERANCE) @@ caps.get(pkg, 10**6)'
  'update forgets the ceiling @@ min([c[pkg] for c in caps if pkg in c] + @@ min(['
  'update reads no side of a conflict @@ for stage in (1, 2, 3): @@ for stage in ():'
  'a split may raise distance without bound @@ value - was <= DISTANCE_SPLIT_TOLERANCE @@ True'
  'the distance tolerance is 3 @@ DISTANCE_SPLIT_TOLERANCE = 2 @@ DISTANCE_SPLIT_TOLERANCE = 3'
  'a refused split raises distance @@ return {package_of(c, units) for c in internal} | {package_of(t, units) for t in union} @@ return set()'
  'a split credits no package it uses @@ | {package_of(t, units) for t in union} @@ | set()'
  'a split may raise any package distance @@ return key in split_packages and value @@ return value'
  'any piece takes over code @@ if not ok:
            why.append @@ if False:
            why.append'
  'git copy detection is not read @@ if (origin, path) in copies: @@ if False:'
  'moved members are not read @@ return len(taken) >= MIN_MOVED_MEMBERS, "" @@ return False, ""'
  'one shared member is enough @@ MIN_MOVED_MEMBERS = 2 @@ MIN_MOVED_MEMBERS = 1'
  'members the original keeps count as moved @@ moved_away = members(file_at(base, origin)) - members(file_at(head, origin)) @@ moved_away = members(file_at(base, origin))'
  'a refused split never fails @@ return bool(refused), split, split_packages @@ return False, split, split_packages'
  'any Coupling trailer claims a split @@ m = SPLIT_TRAILER.match(value) @@ m = re.match(r"(\S+)()", "services.readmodel.ReadModelSweeper") if value else None'
  'a rename is never credited @@ credited[key] = old @@ pass'
  'a renamed row is new in the baseline @@  or (kind, key) in renamed @@ '
  'a renamed row past the limit still fails @@ elif not moved(key, value, base, cur) and (kind, key) not in renamed: @@ elif not moved(key, value, base, cur):'
  'a rename may raise its row @@ if new[key] > gone[old]: @@ if False:'
  'one name gone to two is a rename @@ if len(found) > 1: @@ if False:'
  'a split posing as a rename is credited @@         if others:
            return None, (f"fanout @@         if False:
            return None, (f"fanout'
  'a new class beside any vanished one is its rename @@ if (origin, dest) in self.renames(): @@ if True:'
  'a pure commit pairs any two files @@ (was == now or (was, now) in self.commit_renames(sha)) @@ True'
  'any commit is a pure move @@ self._pure[sha] = done.returncode == 0 @@ self._pure[sha] = True'
  'a renamed package takes a fresh ceiling @@ elif old_name is not None and @@ elif False and'
  'a split claim of a rename is judged as a split @@ return bool(targets) and len(named) == 1 @@ return False and len(named) == 1'
  'any why admits an exemption row @@ out = [f"{where}: its why ({why!r}): {p}" for p in reason_problems(pattern, why)] @@ out = []'
  'a generic why passes @@ for rx, phrase in GENERIC_REASONS if re.search(rx, lowered)] @@ for rx, phrase in GENERIC_REASONS if False]'
  'a short why passes @@ elif len(rest.strip()) < REASON_MIN_CHARS or len(rest.split()) < REASON_MIN_WORDS: @@ elif False:'
  'the class name alone passes @@ if not re.sub(r"[\W_]+", "", rest): @@ if False:'
  'a why without its role passes @@ if not sep or not role.strip() or len(role.split()) > ROLE_MAX_WORDS: @@ if False:'
  'an exemption row needs no date @@ if len(cols) < 6 or not ISO_DATE.match(cols[5]): @@ if False:'
  'an exemption row needs no exempt trailer @@     if reason is None:
        out.append @@     if False:
        out.append'
  'the trailer reason is not linted @@ out += [f"{where}: its trailer'"'"'s reason ({reason!r}): {p}" for p in reason_problems(pattern, reason)] @@ pass'
  'an unadmitted exemption never fails @@ return bool(unadmitted) @@ return False'
)

mkdir -p "$work/mutants/lib"
# the tool's modules and the ones they import, side by side in a directory of each mutant (row parsing is
# baseline_ratchet.parse_rows, the engine's: mutated by js-quality-ratchet-mutation-test.sh); tool_python is in ../lib
modules=(coupling_ratchet coupling_rows coupling_violations coupling_growth coupling_exemption_table
  coupling_exemption_history coupling_reasons coupling_renames coupling_splits trailers baseline_ratchet)
cp "$REPO_ROOT/scripts/lib/tool_python.py" "$work/mutants/lib/"
i=0
for m in "${mutants[@]}"; do
  i=$((i + 1))
  name=${m%% @@ *}; rest=${m#* @@ }; from=${rest%% @@ *}; to=${rest#* @@ }
  printf '%s' "$name" > "$work/mutants/$i.name"
  mkdir -p "$work/mutants/$i"
  for module in "${modules[@]}"; do cp "$REPO_ROOT/scripts/ci/$module.py" "$work/mutants/$i/"; done
  python3 -I -B - "$work/mutants/$i" "$from" "$to" <<'PY'
import glob
import os
import sys
directory, frm, to = sys.argv[1:]
# the one module holding the text exactly once among the tool's modules
holders = [p for p in sorted(glob.glob(os.path.join(directory, "coupling_*.py"))) if open(p).read().count(frm) == 1]
total = sum(open(p).read().count(frm) for p in glob.glob(os.path.join(directory, "*.py")))
if len(holders) == 1 and total == 1:
    text = open(holders[0]).read()
    open(holders[0], "w").write(text.replace(frm, to, 1))
    sys.exit(0)
open(os.path.join(directory, "coupling_ratchet.py"), "w").write("raise SystemExit(99)  # mutant text not found exactly once\n")
sys.exit(1)
PY
  check "the mutant '$name' is aimed at exactly one check in the tool" 0 "$?"
done
export COUPLING_PURE_MOVE_CHECK="$REPO_ROOT/scripts/pure_move_check.py"  # beside the tool, not beside each mutant
# -f: unittest stops at a mutant's first failing test, all it takes to kill one (the clean run below stays whole)
# shellcheck disable=SC2016 # expanded by the inner bash
seq 1 "$i" | "$REPO_ROOT/scripts/gov-parallel" --job mutation-tests -P 4 -I{} bash -c 'COUPLING_RATCHET_UNDER_TEST="$1/mutants/$2/coupling_ratchet.py" python3 "$3" -f > "$1/mutants/$2.out" 2>&1; echo $? > "$1/mutants/$2.rc"' \
  _ "$work" {} "$REPO_ROOT/scripts/ci/coupling_ratchet_test.py"
for n in $(seq 1 "$i"); do
  check "coupling_ratchet_test.py fails against the mutant: $(cat "$work/mutants/$n.name")" 1 \
    "$([ "$(cat "$work/mutants/$n.rc" 2> /dev/null)" != 0 ] && echo 1 || echo 0)"
done
# one failing test kills a mutant: each mutant's run stopped at it (-f), so the job stays inside its timeout
# (CiJobNearTimeout, Alert backtest #28: the whole suite against every mutant took 61 min on macOS)
check "coupling_ratchet_test.py stopped at the first failure against every mutant" 0 \
  "$(grep -LE '^FAILED \((failures|errors)=1\)$' "$work"/mutants/*.out | wc -l | tr -d ' ')"
# ...and passes against the tool itself, so the failures above are the mutants'
python3 "$REPO_ROOT/scripts/ci/coupling_ratchet_test.py" > "$work/out" 2>&1
check "coupling_ratchet_test.py passes against the tool itself" 0 "$?"
spec_summary
