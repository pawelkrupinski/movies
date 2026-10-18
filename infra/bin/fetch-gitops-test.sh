#!/usr/bin/env bash
# infra/bin/fetch-gitops: the manifests come from the config repo, or -- GitHub unreachable -- from a local clone of it.
# Run: bash infra/bin/fetch-gitops-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m fetch-gitops\n'
work="$(mktemp -d)"; work="$(cd "$work" && pwd -P)"
trap 'rm -rf "$work"' EXIT

remote="$work/remote"; local_clone="$work/local"
scratch_repo "$remote"; echo remote > "$remote/README.md"; scratch_git "$remote" add -A; scratch_git "$remote" commit -qm remote
scratch_repo "$local_clone"; echo local > "$local_clone/README.md"; scratch_git "$local_clone" add -A; scratch_git "$local_clone" commit -qm local
for r in "$remote" "$local_clone"; do scratch_git "$r" branch -q -M main; done

gitops() { KINOWO_GITOPS_DEST="$work/dest" KINOWO_GITOPS_LOCAL="$local_clone" "$REPO_ROOT/infra/bin/fetch-gitops" > "$work/out" 2>&1; echo $?; }

check "the config repo reached: its manifests" "0 remote" "$(KINOWO_GITOPS_REPO="$remote" gitops) $(cat "$work/dest/README.md")"
check "...updated in place on the next run" "0 remote" "$(KINOWO_GITOPS_REPO="$remote" gitops) $(cat "$work/dest/README.md")"
check "the config repo unreachable: the local clone's, said so" "0 local 1" \
  "$(KINOWO_GITOPS_REPO="$work/gone" gitops) $(cat "$work/dest/README.md") $(grep -c 'using the local clone' "$work/out")"
rm -rf "$work/dest"
check "...on a fresh checkout too" "0 local" "$(KINOWO_GITOPS_REPO="$work/gone" gitops) $(cat "$work/dest/README.md")"
check "unreachable, and no local clone: it fails, naming both" "1 1" \
  "$(KINOWO_GITOPS_REPO="$work/gone" KINOWO_GITOPS_DEST="$work/dest" KINOWO_GITOPS_LOCAL="$work/none" "$REPO_ROOT/infra/bin/fetch-gitops" > "$work/out" 2>&1; echo $?) $(grep -c 'no local clone' "$work/out")"

spec_summary
