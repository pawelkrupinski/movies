#!/usr/bin/env bash
# infra/bin/fetch-gitops: the manifests come from the config repo -- Forgejo first, then GitHub -- and a fetch that
# fails is a failure, never a silent run against whatever stale checkout or local clone happens to be on disk.
# Run: bash infra/bin/fetch-gitops-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m fetch-gitops\n'
work="$(mktemp -d)"; work="$(cd "$work" && pwd -P)"
trap 'rm -rf "$work"' EXIT

remote="$work/remote"; local_clone="$work/local"
forge="$work/forge/pawelkrupinski/movies-gitops.git"; ci="$work/ci/pawelkrupinski/movies-gitops.git"
for r in "$remote" "$local_clone" "$forge" "$ci"; do
  scratch_repo "$r"; basename "$(dirname "$(dirname "$r")")" > "$r/README.md"
  scratch_git "$r" add -A; scratch_git "$r" commit -qm init; scratch_git "$r" branch -q -M main
done
echo remote > "$remote/README.md"; scratch_git "$remote" commit -qam remote
echo local > "$local_clone/README.md"; scratch_git "$local_clone" commit -qam local

# GITHUB_SERVER_URL unset: the laptop, whatever CI this test itself runs on
gitops() { env -u GITHUB_SERVER_URL KINOWO_GITOPS_DEST="$work/dest" KINOWO_GITOPS_LOCAL="$local_clone" "$@" \
  "$REPO_ROOT/infra/bin/fetch-gitops" > "$work/out" 2>&1; echo $?; }

check "the config repo reached: its manifests" "0 remote" "$(gitops KINOWO_GITOPS_REPO="$remote") $(cat "$work/dest/README.md")"
echo remote2 > "$remote/README.md"; scratch_git "$remote" commit -qam remote2
check "...updated in place on the next run" "0 remote2" "$(gitops KINOWO_GITOPS_REPO="$remote") $(cat "$work/dest/README.md")"

check "the config repo unreachable: it FAILS, naming the stale checkout and the fix, not the local clone silently" "1 1 1 remote2" \
  "$(gitops KINOWO_GITOPS_REPO="$work/gone") $(grep -c 'STALE' "$work/out") $(grep -c 'forgejo-tunnel' "$work/out") $(cat "$work/dest/README.md")"
check "...KINOWO_GITOPS_OFFLINE=1 opts into the local clone, said so" "0 local 1" \
  "$(gitops KINOWO_GITOPS_REPO="$work/gone" KINOWO_GITOPS_OFFLINE=1) $(cat "$work/dest/README.md") $(grep -c 'using the local clone' "$work/out")"
rm -rf "$work/dest"
check "...on a fresh checkout too" "0 local" "$(gitops KINOWO_GITOPS_REPO="$work/gone" KINOWO_GITOPS_OFFLINE=1) $(cat "$work/dest/README.md")"
check "...and no local clone: it fails, naming it" "1 1" \
  "$(gitops KINOWO_GITOPS_REPO="$work/gone" KINOWO_GITOPS_OFFLINE=1 KINOWO_GITOPS_LOCAL="$work/none") $(grep -c 'no local clone' "$work/out")"

rm -rf "$work/dest"
check "no override, on the laptop: Forgejo first (the tunnel's HTTP), not GitHub (suspended 2026-10-19)" "0 forge" \
  "$(gitops KINOWO_FORGEJO_URL="$work/forge") $(cat "$work/dest/README.md")"
check "...the checkout's origin left on Forgejo, with no token in it" "file://$forge 0" \
  "$(scratch_git "$work/dest" remote get-url origin) $(FORGEJO_API_TOKEN=s3cret gitops KINOWO_FORGEJO_URL="$work/forge" >/dev/null; grep -c s3cret "$work/dest/.git/config")"
rm -rf "$work/dest"
check "...the tunnel's HTTP down: the forgejo-git ssh alias, still not the local clone" "0 remote2 0" \
  "$(GIT_CONFIG_COUNT=1 GIT_CONFIG_KEY_0="url.$remote.insteadOf" GIT_CONFIG_VALUE_0=ssh://forgejo-git/pawelkrupinski/movies-gitops.git \
     gitops KINOWO_FORGEJO_URL="$work/gone") $(cat "$work/dest/README.md") $(grep -c 'using the local clone' "$work/out")"
check "on CI: the server the job runs on (\$GITHUB_SERVER_URL -- github.com or Forgejo)" "0 ci" \
  "$(env GITHUB_SERVER_URL="$work/ci" KINOWO_GITOPS_DEST="$work/dest" KINOWO_FORGEJO_URL="$work/forge" \
       "$REPO_ROOT/infra/bin/fetch-gitops" > "$work/out" 2>&1; echo $?) $(cat "$work/dest/README.md")"

spec_summary
