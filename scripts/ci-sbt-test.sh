#!/usr/bin/env bash
# scripts/ci-sbt against a scratch repository, a stub identity-ci-run and a stub gh: the verdict is the "Run the command"
# step's (pass 0, fail 1), never the run's -- an earlier step's failure, a cancelled run, no command step are NO
# VERDICT (2), so scripts/fail-before --ci cannot read an infrastructure red as "fails before" -- and the working-tree
# snapshot carries tracked changes and new sources, never another untracked file.
# Run: bash scripts/ci-sbt-test.sh
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/scratch-git.sh"
. "$HERE/shell-spec.sh"

printf '\033[36m▸\033[0m ci-sbt\n'
tool="$HERE/ci-sbt"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo"
scratch_repo "$repo"
mkdir -p "$repo/worker/src/main" && echo old > "$repo/worker/src/main/A.scala"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm base
head=$(scratch_git "$repo" rev-parse HEAD)

mkdir -p "$work/bin"
# identity-ci-run: notes the commit and command it was asked to run, prints the run id ($RUN)
cat > "$work/bin/identity-ci-run" <<STUB
#!/usr/bin/env bash
printf '%s\n' "\$@" > "$work/dispatched"
echo "identity-ci-run: pushed" >&2
echo "\${RUN:-101}"
STUB
# gh: `run view` answers the run's JSON ($VIEW), `run download` lays an sbt.log
cat > "$work/bin/gh" <<STUB
#!/usr/bin/env bash
case "\$1 \$2" in
  "run view") cat "\$VIEW" ;;
  "run download") d=""; p=""; for a in "\$@"; do [ "\$p" = --dir ] && d=\$a; p=\$a; done
                  mkdir -p "\$d/identity-run-all"; echo "[info] sbt said this" > "\$d/identity-run-all/sbt.log" ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/identity-ci-run" "$work/bin/gh"
export KINOWO_IDENTITY_CI_RUN="$work/bin/identity-ci-run" KINOWO_GH="$work/bin/gh" KINOWO_CI_SBT_POLL=0 \
  GH_DOWNLOAD_PUBLIC=0
view() { # <run conclusion> <command step conclusion> [<setup step conclusion>]
  printf '{"status":"completed","conclusion":"%s","jobs":[{"name":"plan","steps":[]},{"name":"run (all)","steps":[{"name":"Set up sbt","conclusion":"%s"},{"name":"Run the command","conclusion":"%s"}]}]}' \
    "$1" "${3:-success}" "$2" > "$work/view.json"
}
export VIEW="$work/view.json"

view success success
out=$(cd "$repo" && "$tool" -- 'worker/Test/test' 2>&1); rc=$?
check "the command step passed: exit 0" "0" "$rc"
check "...the sbt log is printed" "1" "$(printf '%s\n' "$out" | grep -c 'sbt said this')"
check "...a clean checkout runs HEAD itself" "$head" "$(sed -n '2p' "$work/dispatched")"
check "...the command is dispatched whole" "worker/Test/test" "$(grep -A1 -- '--command' "$work/dispatched" | tail -1)"

view failure failure
(cd "$repo" && "$tool" --quiet -- 'worker/Test/test' > /dev/null 2>&1); rc=$?
check "the command step failed: exit 1 (what a local nonzero exit is)" "1" "$rc"

view failure skipped failure
out=$(cd "$repo" && "$tool" --quiet -- 'worker/Test/test' 2>&1); rc=$?
check "a step before the command failed: NO VERDICT, exit 2" "2" "$rc"
check "...said" "1" "$(printf '%s\n' "$out" | grep -c 'NO VERDICT')"
view cancelled cancelled
(cd "$repo" && "$tool" --quiet -- 'worker/Test/test' > /dev/null 2>&1); rc=$?
check "a cancelled command: exit 2" "2" "$rc"
printf '{"status":"completed","conclusion":"failure","jobs":[{"name":"plan","steps":[{"name":"plan","conclusion":"failure"}]}]}' > "$work/view.json"
(cd "$repo" && "$tool" --quiet -- 'worker/Test/test' > /dev/null 2>&1); rc=$?
check "a run that never reached the command: exit 2" "2" "$rc"
printf '{"status":"in_progress","jobs":[]}' > "$work/view.json"
(cd "$repo" && KINOWO_CI_SBT_TIMEOUT=0 "$tool" --quiet -- 'worker/Test/test' > /dev/null 2>&1); rc=$?
check "a run past the deadline: exit 2" "2" "$rc"

# The snapshot: a tracked change and a new source go; a stray untracked file never does
echo new > "$repo/worker/src/main/A.scala"
mkdir -p "$repo/worker/src/test" && echo spec > "$repo/worker/src/test/ASpec.scala"
echo SECRET=1 > "$repo/.env"
view success success
log="$work/out.log"
out=$(cd "$repo" && "$tool" --log "$log" -- 'worker/Test/test' 2>&1); rc=$?
snap=$(sed -n '2p' "$work/dispatched")
check "a dirty checkout runs a snapshot commit on HEAD" "0 $head" "$rc $(scratch_git "$repo" rev-parse "$snap^")"
check "...holding the tracked change" "new" "$(scratch_git "$repo" show "$snap:worker/src/main/A.scala")"
check "...and the new source" "spec" "$(scratch_git "$repo" show "$snap:worker/src/test/ASpec.scala" 2>/dev/null)"
check "...never another untracked file" "absent" "$(scratch_git "$repo" cat-file -e "$snap:.env" 2>/dev/null && echo present || echo absent)"
check "...which is named" "1" "$(printf '%s\n' "$out" | grep -c 'not run: .env')"
check "...the checkout itself untouched (nothing staged)" "" "$(scratch_git "$repo" diff --cached --name-only)"
check "--log takes the sbt output" "1" "$(grep -c 'sbt said this' "$log")"
(cd "$repo" && "$tool" --commit "$head" --quiet -- 'worker/Test/test' > /dev/null 2>&1)
check "--commit runs that commit" "$head" "$(sed -n '2p' "$work/dispatched")"

spec_summary
