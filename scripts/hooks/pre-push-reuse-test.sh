#!/usr/bin/env bash
# shellcheck disable=SC2154 # the stubs' and the fixture's variables, set in pre-push-test-lib.sh
# pre-push's reuse: a passed run answers a re-push of the SAME tree, and nothing else.
# Run: bash scripts/hooks/pre-push-reuse-test.sh (the shared harness: pre-push-test-lib.sh)
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/pre-push-test-lib.sh"

gate_setup
gate_fixture
# ── THE REUSE: a passed run answers a re-push of the SAME tree, and nothing else ─────────────────────────────────────
# A re-push after a no-content rebase or a reworded message ran every check again. A pass is stored under a key of the
# pushed tree, the base's, the changed paths, the checks, the hook's bytes, the red-gate state and PREPUSH_SKIP; the
# expensive check here is the counting script-test runner. What must hold: ANY difference runs everything.
printf '\033[36m▸\033[0m pre-push: a passed run is reused for the same tree only\n'
reuse="$gate/reuse"; mkdir -p "$reuse"
scratch_git "$grepo" reset -q --hard "$oldrefs_base"
printf '#!/usr/bin/env bash\necho ran >> "$STUB_RUNS"\nexit "${STUB_RUNNER_EXIT:-0}"\n' > "$grepo/scripts/run-script-tests"
chmod +x "$grepo/scripts/run-script-tests"
scratch_git "$grepo" add scripts/run-script-tests && scratch_git "$grepo" commit -qm "a counting runner"
rbase="$(scratch_git "$grepo" rev-parse HEAD)"
rhead="$(commit_paths "a script change" -- scripts/z.sh)"
# <range> [<env>...] -> "<exit> <runner runs so far>"; stderr in $gate/err. $HOOK: the hook to run (default this checkout's).
reuse_push() {
  local range=$1; shift
  (cd "$grepo" && all_green env PREPUSH_NO_REUSE= PREPUSH_REUSE_DIR="$reuse/cache" PREPUSH_SKIP="${pathchecks/scripttests/}" \
    STUB_RUNS="$reuse/runs" PREPUSH_FLOOR_BASH="$gate/no-such-bash" "$@" \
    bash "${HOOK:-$REPO_ROOT/scripts/hooks/pre-push}" --range "$range" >/dev/null 2>"$gate/err"); echo "$? $(cat "$reuse/runs" 2>/dev/null | wc -l | tr -d ' ')"; }
check "a first push runs the checks" "0 1" "$(reuse_push "$rbase..$rhead")"
check "the same push again reuses the pass: nothing runs, said once" "0 1 1" \
  "$(reuse_push "$rbase..$rhead") $(grep -c 'reused, not run again' "$gate/err")"
reworded="$(scratch_git "$grepo" commit -q --amend -m "the same change, reworded" && scratch_git "$grepo" rev-parse HEAD)"
check "a new commit of the SAME tree (no-content rebase, reworded message) reuses it" "0 1" "$(reuse_push "$rbase..$reworded")"
check "PREPUSH_NO_REUSE=1 runs everything regardless" "0 2" "$(reuse_push "$rbase..$reworded" PREPUSH_NO_REUSE=1)"
changed_tree="$(commit_paths "one more byte" -- scripts/z.sh)"
check "a changed tree runs everything" "0 3" "$(reuse_push "$rbase..$changed_tree")"
docs_only="$(commit_paths "a change outside what the checks read" -- docs/x.md)"
check "...even a change outside every checked path" "0 4" "$(reuse_push "$rbase..$docs_only")"
check "...and once passed, that tree is reused in turn" "0 4" "$(reuse_push "$rbase..$docs_only")"
check "another base (same tree) runs everything" "0 5" "$(reuse_push "$rhead..$docs_only")"
check "a red-gate state that differs runs everything (a red run appeared; tools land while red)" "0 6" \
  "$(reuse_push "$rbase..$docs_only" GH_IMC="[$red]")"
check "another PREPUSH_SKIP runs everything" "0 7" "$(reuse_push "$rbase..$docs_only" PREPUSH_SKIP="${pathchecks/scripttests/} linux")"
# --fast-only: the `waiting` batch's combined check (sbt alone: compiles, lints, the `fast` set), no red gate; a push
# with PREPUSH_REUSE_FAST=1 (agentctl's batch push) of that tree on that base reuses its pass after the gate
fast_head="$(commit_paths "a batch of script changes" -- scripts/z.sh)"
fast_run() { (cd "$grepo" && env PREPUSH_NO_REUSE= PREPUSH_REUSE_DIR="$reuse/cache" PREPUSH_SKIP="${pathchecks/scripttests/}" \
  STUB_RUNS="$reuse/runs" GH_IMC="[$red]" bash "$REPO_ROOT/scripts/hooks/pre-push" --fast-only "$rbase..$fast_head" >/dev/null 2>"$gate/err")
  echo "$? $(cat "$reuse/runs" 2>/dev/null | wc -l | tr -d ' ')"; }
check "--fast-only judges no red gate and runs no path check but sbt (scripttests not run)" "0 7 1" \
  "$(fast_run) $(grep -c 'fast-only: the red gate is not judged' "$gate/err")"
check "a batch push with PREPUSH_REUSE_FAST=1 reuses the fast pass: nothing runs, said once" "0 7 1" \
  "$(reuse_push "$rbase..$fast_head" PREPUSH_REUSE_FAST=1) $(grep -c "the waiting batch's fast check passed" "$gate/err")"
check "...an ordinary push of it runs its checks" "0 8" "$(reuse_push "$rbase..$fast_head")"
head -7 "$reuse/runs" > "$reuse/runs.kept" && mv "$reuse/runs.kept" "$reuse/runs"  # the counts below start from 7
scratch_git "$grepo" reset -q --hard "$docs_only"
# A hook that differs by one byte: a copy beside the helpers it sources, a comment appended.
mkdir -p "$reuse/hook/scripts/hooks"
cp "$REPO_ROOT/scripts/hooks-path.sh" "$REPO_ROOT/scripts/tool-conventions.sh" "$reuse/hook/scripts/"
cp "$REPO_ROOT/scripts/hooks/pre-push" "$reuse/hook/scripts/hooks/pre-push"; echo "# one more byte" >> "$reuse/hook/scripts/hooks/pre-push"
check "a hook that differs runs everything" "0 8" "$(HOOK="$reuse/hook/scripts/hooks/pre-push" reuse_push "$rbase..$docs_only")"
echo "# and a helper" >> "$reuse/hook/scripts/tool-conventions.sh"
check "...as does a helper it sources" "0 9" "$(HOOK="$reuse/hook/scripts/hooks/pre-push" reuse_push "$rbase..$docs_only")"
failing="$(commit_paths "a change whose checks fail" -- scripts/z.sh)"
check "a failed run stores nothing: the same tree runs again" "1 10 0 11" \
  "$(reuse_push "$rbase..$failing" STUB_RUNNER_EXIT=1) $(reuse_push "$rbase..$failing")"
# THE MUTANT: the same hook keyed without the pushed tree. It would answer a changed tree from the stored pass -- so
# "a changed tree runs everything" above is a check that fails on a hook that could skip a changed tree.
sed 's/^\(    tree=\)".*head_sha^{tree}")"/\1mutant/' "$REPO_ROOT/scripts/hooks/pre-push" > "$reuse/hook/scripts/hooks/pre-push"
cp "$REPO_ROOT/scripts/tool-conventions.sh" "$reuse/hook/scripts/"
check "the mutant differs from the hook in the tree's key only" "1" \
  "$(diff "$REPO_ROOT/scripts/hooks/pre-push" "$reuse/hook/scripts/hooks/pre-push" | grep -c '^>  *tree=mutant && ')"
HOOK="$reuse/hook/scripts/hooks/pre-push" reuse_push "$rbase..$docs_only" >/dev/null # the same paths as the next push
mutated="$(commit_paths "a tree the mutant has not seen" -- scripts/z.sh)"
check "...and it reuses a changed tree, which the check above refuses" "0 12" \
  "$(HOOK="$reuse/hook/scripts/hooks/pre-push" reuse_push "$rbase..$mutated")"
# CHECK FIRST: git opens the push's connection before the hook runs, and a long hook outlives it (2026-10-23). A dry
# run of `<remote>/main..HEAD` is keyed like the push to it, so the push that follows reuses its pass and holds the
# connection for seconds.
ghremote=git@github.com:pawelkrupinski/movies.git
scratch_git "$grepo" remote add or "$ghremote"
scratch_git "$grepo" update-ref refs/remotes/or/main "$rbase"
checkfirst="$(commit_paths "a change checked before its push" -- scripts/z.sh)"
greens() { GH_MAIN="[$green]" GH_IMC="[$green]" GH_REC="[$green]" "$@"; }
check "a dry run of or/main..HEAD runs the checks" "0 13" "$(greens reuse_push "or/main..$checkfirst")"
out="$(cd "$grepo" && printf 'refs/heads/main %s refs/heads/main %s\n' "$checkfirst" "$rbase" \
        | greens env PREPUSH_NO_REUSE= PREPUSH_REUSE_DIR="$reuse/cache" PREPUSH_SKIP="${pathchecks/scripttests/}" \
          STUB_RUNS="$reuse/runs" PREPUSH_FLOOR_BASH="$gate/no-such-bash" bash "$REPO_ROOT/scripts/hooks/pre-push" or "$ghremote" 2>&1)"; code=$?
check "...and the push to or that follows reuses that pass: nothing runs again" "0 13 1" \
  "$code $(cat "$reuse/runs" | wc -l | tr -d ' ') $(printf '%s\n' "$out" | grep -c 'reused, not run again')"
# A hook that ran long says the connection git opened before it may be gone, and how to re-push cheaply; a quick one,
# and a dry run (no connection), do not.
long_push() { # <threshold seconds> <local sha> -> the warning's count
  (cd "$grepo" && printf 'refs/heads/main %s refs/heads/main %s\n' "$2" "$rbase" \
    | greens env PREPUSH_LONG_SECONDS="$1" PREPUSH_SKIP="${pathchecks/scripttests/}" STUB_RUNS="$reuse/runs" \
      PREPUSH_FLOOR_BASH="$gate/no-such-bash" bash "$REPO_ROOT/scripts/hooks/pre-push" or "$ghremote" 2>&1) \
    | grep -c 'with the push.s connection open .*run the same git push again.*scripts/hooks/pre-push --range or/main..HEAD'; }
check "a push whose hook ran past PREPUSH_LONG_SECONDS says to re-push, and to check first next time" "1" "$(long_push 0 "$checkfirst")"
check "...a quick one does not" "0" "$(long_push 3600 "$checkfirst")"
check "...nor does a dry run, which holds no connection" "0 0" \
  "$(greens reuse_push "or/main..$checkfirst" PREPUSH_NO_REUSE=1 PREPUSH_LONG_SECONDS=0 | cut -d' ' -f1) $(grep -c 'run the same git push again' "$gate/err")"
rm -rf "$gate"

spec_summary
