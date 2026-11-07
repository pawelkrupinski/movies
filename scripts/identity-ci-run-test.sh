#!/usr/bin/env bash
# scripts/identity-ci-run against scratch repositories, a git that watches its measure/ pushes and a stub gh: several
# runs of ONE commit at once (identity-landing-check's groups, beside its identity-measure) push their measure/
# branches one at a time, never together, and a push the remote rejects names the remote's reason.
#
# Why one at a time (2026-10-23): the remote took two of four concurrent pushes of the same new commit as "unable to migrate
# objects to permanent storage" -- receive-packs of the same objects (100+, so kept as a pack) write the same
# pack-<hash>.keep with different contents (each its own pid), and the second one's quarantine cannot move in. A push
# after the first finds the commit on the remote already and sends no objects at all.
# Run: bash scripts/identity-ci-run-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-ci-run\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
origin="$work/origin.git" repo="$work/repo"
scratch_repo "$origin" --bare -b main
scratch_repo "$repo" -b main
scratch_git "$repo" remote add origin "$origin"
mkdir -p "$repo/.github/workflows"
echo "on: workflow_dispatch" > "$repo/.github/workflows/identity-run.yml"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm main
scratch_git "$repo" push -q origin main
echo change > "$repo/change.txt" && scratch_git "$repo" add change.txt && scratch_git "$repo" commit -qm "a change"
tip=$(scratch_git "$repo" rev-parse HEAD)

mkdir -p "$work/bin" "$work/pushes/active"
# gh: refuses every call (the dispatch fails after the push, and the run ends there: the push is what is under test),
# noting a dispatch's arguments
printf '#!/usr/bin/env bash\n[ "$1 $2" != "workflow run" ] || echo "$*" >> "%s/dispatches"\nexit 1\n' "$work" > "$work/bin/gh"
# git: the real one, but a measure/ push says when another push was under way meanwhile (an overlap), or -- with
# $REJECT -- is rejected the way the remote rejected the racing ones
real_git=$(command -v git)
cat > "$work/bin/git" <<STUB
#!/usr/bin/env bash
if [ "\$1" = push ] && [[ " \$* " == *refs/heads/measure/* ]] && [[ " \$* " != *--delete* ]]; then
  if [ -n "\${REJECT:-}" ]; then
    echo "error: files './objects/tmp_objdir-incoming-x/pack/pack-1.keep' and './objects/pack/pack-1.keep' differ in contents" >&2
    echo " ! [remote rejected] \$(printf '%s' "\$*" | sed 's/.* //') (unable to migrate objects to permanent storage)" >&2
    echo "error: failed to push some refs to 'origin'" >&2
    exit 1
  fi
  touch "$work/pushes/active/\$\$"
  sleep 1
  n=\$(ls "$work/pushes/active" | wc -l | tr -d ' ')
  [ "\$n" -le 1 ] || echo overlap >> "$work/pushes/log"
  "$real_git" "\$@"; rc=\$?
  rm -f "$work/pushes/active/\$\$"
  echo pushed >> "$work/pushes/log"
  exit \$rc
fi
exec "$real_git" "\$@"
STUB
chmod +x "$work/bin/gh" "$work/bin/git"

ci_run() { # <name> [flags...]: identity-ci-run of the tip; exit code in $work/<name>.rc, stderr in $work/<name>.err
  local name=$1; shift
  ( cd "$repo" && KINOWO_GH="$work/bin/gh" KINOWO_IDENTITY_CACHE="$work/cache" IDENTITY_MEASURE_REMOTE=origin \
      PATH="$work/bin:$PATH" "$REPO_ROOT/scripts/identity-ci-run" --head "$tip" "$@" -- 'Test/compile' > /dev/null 2> "$work/$name.err" )
  echo $? > "$work/$name.rc"
}

ci_run a & ci_run b & ci_run c & wait
check "three runs of one commit at once each push it" "3" "$(grep -c pushed "$work/pushes/log")"
check "...one push at a time: none overlaps another" "0" "$(grep -c overlap "$work/pushes/log")"
check "...and each reaches the dispatch (refused here), its branch deleted as before" "2 2 2 0" \
  "$(cat "$work/a.rc") $(cat "$work/b.rc") $(cat "$work/c.rc") $(scratch_git "$origin" for-each-ref refs/heads/measure | wc -l | tr -d ' ')"

REJECT=1 ci_run rejected
check "a push the remote rejects is no run" "2" "$(cat "$work/rejected.rc")"
# every line the tool said (the branch name masked), so a failure shows what it said instead (one red, 2026-11-07, did not)
check "...and says why, in the remote's words" \
  "identity-ci-run: could not push measure/B: remote rejected (unable to migrate objects to permanent storage)" \
  "$(grep '^identity-ci-run: ' "$work/rejected.err" | sed -E 's#measure/[^ :]+#measure/B#' | paste -sd'|' -)"


spec_summary
