#!/usr/bin/env bash
# The convergence row's "Check out the commit" step (.github/workflows/country-convergence-leg.yml), run as written
# against a local origin: a fetch that hangs is ended and asked again, a branch is checked out by name, a workspace a
# self-hosted runner left behind is emptied first, and a fetch that never succeeds fails the step.
#
# Why the step exists: GitHub's git server left some of a run's simultaneous depth-1 fetches silent for 5-7 minutes
# (run 41872808505: 10 of 18 rows), which actions/checkout waits out with no bound of its own.
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh" # every git here acts on a scratch repository, never this checkout
leg="$REPO_ROOT/.github/workflows/country-convergence-leg.yml"
printf '\033[36m▸\033[0m country-convergence-leg.yml: Check out the commit\n'
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# The step's `run:` block, its indentation taken off: every line after `run: |` indented deeper than `run:` itself.
awk '
  /^ *- name: Check out the commit$/ { in_step = 1; next }
  in_step && /^ *run: \|$/ { match($0, /^ */); indent = RLENGTH; in_run = 1; next }
  in_run { match($0, /^ */); if ($0 != "" && RLENGTH <= indent) exit; print substr($0, indent + 3) }
' "$leg" > "$work/step.sh"
check "the leg has the step" "yes" "$([ -s "$work/step.sh" ] && echo yes || echo no)"

# The origin: one commit on main, served from disk as $GITHUB_SERVER_URL/$GITHUB_REPOSITORY.
scratch_repo "$work/src" -b main
echo tree > "$work/src/file.txt"
scratch_git "$work/src" add file.txt
scratch_git "$work/src" commit -q -m one
sha=$(scratch_git "$work/src" rev-parse HEAD)
scratch_git "$work/src" clone -q --bare . "$work/server/movies.git"

real_git=$(command -v git)
mkdir -p "$work/bin"
# A git whose fetches hang while a counter file says so (`echo $$ >> HUNG; exec sleep`: HUNG STUBS, shell-spec.sh),
# or always fail when FAIL_FETCH is set; everything else is the real git.
cat > "$work/bin/git" <<EOF
#!/usr/bin/env bash
for a in "\$@"; do
  if [ "\$a" = fetch ]; then
    [ -n "\${FAIL_FETCH:-}" ] && { echo "fatal: unable to access" >&2; exit 128; }
    left=\$(cat "$work/hangs" 2>/dev/null || echo 0)
    if [ "\$left" -gt 0 ]; then echo \$((left - 1)) > "$work/hangs"; echo \$\$ >> "$work/hung"; exec sleep 86400; fi
    break
  fi
done
exec "$real_git" "\$@"
EOF
chmod +x "$work/bin/git"
# GNU timeout is `gtimeout` on a Mac with coreutils from Homebrew and absent otherwise; the runner has it.
if ! command -v timeout > /dev/null; then
  gtimeout=$(command -v gtimeout) || { echo "SKIP: no timeout or gtimeout on PATH"; exit 0; }
  ln -s "$gtimeout" "$work/bin/timeout"
fi

# run_step <workspace> <ref> [env...] -> the step's output in $work/out, its status echoed (spec_bounded: a step that
# hangs fails the case instead of wedging the spec)
run_step() {
  local ws=$1 ref=$2; shift 2
  mkdir -p "$ws"
  (cd "$ws" && spec_bounded "$(command -v env)" PATH="$work/bin:$PATH" GITHUB_WORKSPACE="$ws" GITHUB_SERVER_URL="file://$work/server" \
      GITHUB_REPOSITORY=movies.git GITHUB_SHA="$sha" GITHUB_REF="$ref" CHECKOUT_TOKEN=token "$@" \
      bash "$work/step.sh" > "$work/out" 2>&1)
  echo $?
}

# A plain fetch: one attempt, the branch checked out by name at the commit, the token kept for later fetches.
check "a fetch that answers: the step passes" 0 "$(run_step "$work/ws1" refs/heads/main)"
check "  ...in one attempt" 1 "$(grep -c 'attempt 1)' "$work/out")"
check "  ...at the run's commit" "$sha" "$(scratch_git "$work/ws1" rev-parse HEAD)"
check "  ...on the branch by name" main "$(scratch_git "$work/ws1" rev-parse --abbrev-ref HEAD)"
check "  ...with the tree" tree "$(cat "$work/ws1/file.txt")"
check "  ...and the token for later fetches" "AUTHORIZATION: basic $(printf 'x-access-token:token' | base64)" \
  "$(scratch_git "$work/ws1" config --get "http.file://$work/server/.extraheader")"

# The first fetch hangs: ended at its bound and asked again.
echo 1 > "$work/hangs"; : > "$work/hung"
check "a hung fetch: the step passes" 0 "$(run_step "$work/ws2" refs/heads/main CHECKOUT_ATTEMPT_SECONDS=2)"
check "  ...after its bound ended the first attempt" 1 "$(grep -c '::warning::fetch attempt 1 ended' "$work/out")"
check "  ...and the second fetched it" 1 "$(grep -c 'attempt 2)' "$work/out")"
check "  ...at the run's commit" "$sha" "$(scratch_git "$work/ws2" rev-parse HEAD)"
check "  ...leaving no hung fetch behind" 0 "$(spec_still_running "$work/hung")"

# A workspace a self-hosted runner left: emptied before the fetch, as actions/checkout does.
mkdir -p "$work/ws3/target" && echo stale > "$work/ws3/target/left.txt" && echo stale > "$work/ws3/.hidden"
check "a left workspace: the step passes" 0 "$(run_step "$work/ws3" refs/heads/main)"
check "  ...without what the last job left" "no" "$([ -e "$work/ws3/target" ] || [ -e "$work/ws3/.hidden" ] && echo yes || echo no)"

# Not a branch (a tag, a pull request ref): the commit detached.
check "a ref that is no branch: the step passes" 0 "$(run_step "$work/ws4" refs/tags/v1)"
check "  ...detached at the commit" "HEAD $sha" "$(scratch_git "$work/ws4" rev-parse --abbrev-ref HEAD) $(scratch_git "$work/ws4" rev-parse HEAD)"

# Every attempt fails: so does the step, naming it.
check "a fetch that never succeeds: the step fails" 128 "$(run_step "$work/ws5" refs/heads/main CHECKOUT_ATTEMPT_SECONDS=2 FAIL_FETCH=1)"
check "  ...after three attempts" 2 "$(grep -c '::warning::fetch attempt' "$work/out")"
check "  ...saying so" 1 "$(grep -c "::error::could not fetch $sha" "$work/out")"

spec_summary
