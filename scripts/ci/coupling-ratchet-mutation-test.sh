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
  'a new cycle passes @@ for key, refs in sorted(of_kind(current, "cycle-edge").items()): @@ for key, refs in []:'
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
  'a new package has no limit @@ allowed = base_ce.get(key, lim["new-package-ce"]) @@ allowed = base_ce.get(key, 10**9)'
  'a frozen row is not compared @@ if value > base[key]: @@ if False:'
  'a class or package newly past a limit passes @@ elif not moved(key, value, base, cur): @@ elif False:'
  'any frozen row counts as moved @@ return any(k.rsplit(".", 1)[-1] == simple and k not in new and v >= value @@ return any(True'
  'the baseline limits are not read @@ out.update({key: value for (kind, key), value in rows.items() if kind == "limit"}) @@ pass'
  'a raised limit is no growth @@ if new_lim[key] > old_lim.get(key, 0): @@ if False:'
  'an added edge row is no growth @@ for key in sorted(set(of_kind(new, kind)) - set(of_kind(old, kind))): @@ for key in []:'
  'a raised Ce row is no growth @@         if ce > allowed:
            out.append(f"ce @@         if False:
            out.append(f"ce'
  'a raised frozen row is admitted @@ if value > before[key]: @@ if False:'
  'a new frozen row is admitted @@ elif not moved(key, value, before, after) and not (kind == "fanout" and key in split): @@ elif False:'
  'a measurement fix takes no rows again @@ LIMIT_KEYS = {"fanout": ("fanout", "fanout-measure"), @@ LIMIT_KEYS = {"fanout": ("fanout",),'
  'an absent measure row re-freezes every range @@ and key not in MEASURES) @@ and True)'
  'every change re-freezes @@ return any((("limit", key) not in old and key not in MEASURES) or old_lim.get(key) != new_lim.get(key) @@ return True or any((("limit", key) not in old and key not in MEASURES) or old_lim.get(key) != new_lim.get(key)'
  'admitted frozen rows never fail @@         if frozen:
            failed = True @@         if frozen:
            failed = False'
  'an added exemption is no loosening @@         if key not in old:
            out.append(f"{key[0]} {key[1]} ({value}) added") @@         if False:
            out.append(f"{key[0]} {key[1]} ({value}) added")'
  'a changed exemption limit is no loosening @@ elif old[key] != value: @@ elif False:'
  'a loosened exemption table never fails @@ if loose and not justified(rng, EXEMPTIONS_PATH): @@ if False:'
  'the trailer is not read @@ return any(entry.strip() for entry in log.split("\x00")) @@ return True'
  'growth never fails @@ if grown and not justified(rng): @@ if False:'
  'violations never fail @@     if found:
        failed = True @@     if found:
        failed = False'
  'an added allow row is no loosening @@ out = [f"allow {edge} added" for edge in sorted(new[1] - old[1])] @@ out = []'
  'a moved layer is no loosening @@ if was is not None and now is not None and was != now: @@ if False:'
  'a sub-package lifted by a new row is no loosening @@ named = [p for p in rows if pkg == p or pkg.startswith(p + ".")] @@ named = [p for p in rows if pkg == p]'
  'a loosened layer table never fails @@ if loosened and not justified(rng, LAYERS_PATH): @@ if False:'
  'any commit counts for the layer table trailer @@ rng, "--", path) @@ rng)'
  'a split piece is never admitted @@ and not (kind == "fanout" and key in split): @@ and True:'
  'a split claim needs no frozen original @@ if original is None:
            refused.append @@ if original is None:
            admitted |= set(named); continue
            refused.append'
  'a piece at the original passes @@ if v >= frozen[original]: @@ if v > frozen[original]:'
  'the split sum is not held @@ if total > frozen[original]: @@ if False:'
  'what the original keeps is not summed @@ total = sum(values.values()) + left @@ total = sum(values.values())'
  'any piece takes over code @@ if not ok:
                why.append @@ if False:
                why.append'
  'git copy detection is not read @@ if (origin, path) in copies: @@ if False:'
  'moved members are not read @@ return len(taken) >= MIN_MOVED_MEMBERS, "" @@ return False, ""'
  'one shared member is enough @@ MIN_MOVED_MEMBERS = 2 @@ MIN_MOVED_MEMBERS = 1'
  'members the original keeps count as moved @@ moved = members(file_at(base, origin)) - members(file_at(head, origin)) @@ moved = members(file_at(base, origin))'
  'a refused split never fails @@         if refused:
            failed = True @@         if refused:
            failed = False'
  'any Coupling trailer claims a split @@ m = SPLIT_TRAILER.match(value) @@ m = re.match(r"(\S+)()", "services.readmodel.ReadModelSweeper") if value else None'
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
