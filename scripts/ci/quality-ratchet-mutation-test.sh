#!/usr/bin/env bash
# THE CODE-QUALITY RATCHETS HOLD, proved by mutation: each mutant below is quality_ratchet.py with ONE check taken out,
# and quality_ratchet_test.py must FAIL against every one -- a check whose removal no test notices could silently stop
# working and let complexity, length or duplication grow unreviewed. Each mutant is applied by exact text; a mutant
# whose text is no longer in the tool exactly once fails this test too (the tool changed: re-aim the mutant at the new
# check). A mutant named `trailers.py: ...` mutates the module the tool imports (the attribution rule lives there), each
# mutant in a directory of its own beside its own copy of that module.
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
tool="$REPO_ROOT/scripts/ci/quality_ratchet.py"
printf '\033[36m▸\033[0m quality_ratchet.py, mutated\n'
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# <name> @@ <exact text in the tool> @@ <what replaces it>
mutants=(
  'a frozen row rising passes @@ if value > base[key]: @@ if False:'
  'a new key past the limit passes @@ elif value > limit and not moved_from @@ elif False and not moved_from'
  'the measure'"'"'s limit is not compared @@ if limit_of(current, kind) > limit: @@ if False:'
  'a missing limit row passes @@ raise ValueError(f"no `limit @@ return 10**9  # (f"no `limit'
  'a raised limit is no growth @@ if old_limit is not None and new_limit > old_limit: @@ if False:'
  'an added or raised row is no growth @@ if value > before.get(key, allowed_new) and not @@ if False and not'
  'trailers.py: the trailer is not read @@ return bool(value.strip()) @@ return True'
  'trailers.py: any trailer counts @@ trailers:key={key},valueonly @@ trailers:valueonly'
  'trailers.py: any commit of the range justifies a rise @@ if not carries(sha, key): @@ if not any(carries(s, key) for s in changed):'
  'trailers.py: a step is judged against the range'"'"'s base @@ before, after = text_at(f"{sha}^", path), text_at(sha, path) @@ before, after = text_at(rng.split("..")[0], path), text_at(sha, path)'
  'growth is judged on another ratchet'"'"'s trailer @@ trailers.unjustified_rises(rng, ratchet.baseline, ratchet.trailer, @@ trailers.unjustified_rises(rng, ratchet.baseline, "Complexity",'
  'growth never fails @@ if unjustified: @@ if False:'
  'violations never fail @@     if found:
        failed = True @@     if found:
        failed = False'
  'a move needs no departure @@ if old not in present and moved_key(old, renames) == key and was >= value), None) @@ if moved_key(old, renames) == key and was >= value), None)'
  'a move may grow @@ if old not in present and moved_key(old, renames) == key and was >= value), None) @@ if old not in present and moved_key(old, renames) == key), None)'
  'a same-named source is a move @@ if old not in present and moved_key(old, renames) == key and was >= value), None) @@ if old not in present and old.rsplit("/", 1)[-1] == key.rsplit("/", 1)[-1] and was >= value), None)'
  'a deleted source counts as renamed @@             if path not in renames:
                return None @@             if path not in renames:
                return path.replace("a/src/main/scala/", "b/src/main/scala/x/")'
  'a move may change the method name @@ if old not in present and moved_key(old, renames) == key and was >= value), None) @@ if old not in present and (moved_key(old, renames) or "").partition(":")[0] == key.partition(":")[0] and was >= value), None)'
  'a move is growth @@ and not (key not in before and moved_from(key, value, before, after, renames)): @@ :'
  'a malformed row is skipped @@ raise ValueError(f"line {n}: not @@ continue  # (f"line {n}: not'
)

mkdir -p "$work/mutants"
i=0
for m in "${mutants[@]}"; do
  i=$((i + 1))
  name=${m%% @@ *}; rest=${m#* @@ }; from=${rest%% @@ *}; to=${rest#* @@ }
  printf '%s' "$name" > "$work/mutants/$i.name"
  mkdir -p "$work/mutants/$i"
  cp "$tool" "$REPO_ROOT/scripts/ci/trailers.py" "$work/mutants/$i/"  # the tool and the module it imports, side by side
  target="$work/mutants/$i/quality_ratchet.py"
  case "$name" in "trailers.py: "*) target="$work/mutants/$i/trailers.py" ;; esac
  python3 -I - "$target" "$target" "$from" "$to" <<'PY'
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
seq 1 "$i" | xargs -P 4 -I{} bash -c 'QUALITY_RATCHET_UNDER_TEST="$1/mutants/$2/quality_ratchet.py" python3 "$3" > "$1/mutants/$2.out" 2>&1; echo $? > "$1/mutants/$2.rc"' \
  _ "$work" {} "$REPO_ROOT/scripts/ci/quality_ratchet_test.py"
for n in $(seq 1 "$i"); do
  check "quality_ratchet_test.py fails against the mutant: $(cat "$work/mutants/$n.name")" 1 \
    "$([ "$(cat "$work/mutants/$n.rc" 2> /dev/null)" != 0 ] && echo 1 || echo 0)"
done
# ...and passes against the tool itself, so the failures above are the mutants'
python3 "$REPO_ROOT/scripts/ci/quality_ratchet_test.py" > "$work/out" 2>&1
check "quality_ratchet_test.py passes against the tool itself" 0 "$?"
spec_summary
