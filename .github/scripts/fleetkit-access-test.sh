#!/usr/bin/env bash
# fleetkit-access.sh against a scratch HOME: the alias it writes resolves where npm ci's
# git+ssh://forgejo-git/... must go, with the deploy key and the pinned host key, and an empty
# secret fails loudly instead of leaving npm ci to fail on an unresolvable host.
# Run: bash .github/scripts/fleetkit-access-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m fleetkit-access.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
script="$REPO_ROOT/.github/scripts/fleetkit-access.sh"

HOME="$work/home" FLEETKIT_DEPLOY_KEY="-----BEGIN OPENSSH PRIVATE KEY-----
fake
-----END OPENSSH PRIVATE KEY-----" bash "$script" >/dev/null 2>&1
check "a key writes the alias" "0" "$?"

# Read from the `Host forgejo-git` block itself: the script-test container has no ssh client for
# `ssh -G`, and the CI run's own fetch is what proves the connection.
field() { awk -v k="$1" '$1 == "Host" { inside = ($2 == "forgejo-git") } inside && $1 == k { print $2; exit }' "$work/home/.ssh/config"; }
check "the alias reaches Forgejo's internal host" "forgejo.internal" "$(field HostName)"
check "on Forgejo's ssh port" "2222" "$(field Port)"
check "as Forgejo's ssh user" "forgejo" "$(field User)"
check "leaving Forgejo's host key algorithms to ssh" "" "$(field HostKeyAlgorithms)"
check "with the deploy key" "$work/home/.ssh/fleetkit_deploy" "$(field IdentityFile)"
check "and that key alone" "yes" "$(field IdentitiesOnly)"
check "against the pinned host key" "$work/home/.ssh/fleetkit_known_hosts" "$(field UserKnownHostsFile)"
check "refusing an unknown host key" "yes" "$(field StrictHostKeyChecking)"
check "the key is private to its owner" "600" "$(stat -c '%a' "$work/home/.ssh/fleetkit_deploy" 2>/dev/null || stat -f '%Lp' "$work/home/.ssh/fleetkit_deploy")"
check "the key is written whole" "fake" "$(sed -n 2p "$work/home/.ssh/fleetkit_deploy")"
check "the pinned key names the host and port" "[forgejo.internal]:2222" "$(cut -d' ' -f1 "$work/home/.ssh/fleetkit_known_hosts")"

HOME="$work/gh" GITHUB_SERVER_URL=https://github.com FLEETKIT_DEPLOY_KEY="k" bash "$script" >/dev/null 2>&1
ghfield() { awk -v k="$1" '$1 == "Host" { inside = ($2 == "forgejo-git") } inside && $1 == k { print $2; exit }' "$work/gh/.ssh/config"; }
check "on github.com the alias reaches github.com" "github.com" "$(ghfield HostName)"
check "...on port 22" "22" "$(ghfield Port)"
check "...as git" "git" "$(ghfield User)"
check "...pinned to GitHub's ed25519 host key, under the bare name ssh looks port 22 up by" "github.com ssh-ed25519" "$(cut -d' ' -f1,2 "$work/gh/.ssh/fleetkit_known_hosts")"
check "...and only that algorithm" "ssh-ed25519" "$(ghfield HostKeyAlgorithms)"

HOME="$work/empty" FLEETKIT_DEPLOY_KEY="" bash "$script" >/dev/null 2>"$work/err"
check "an empty secret fails" "1" "$?"
check "and says which secret" "yes" "$(grep -q 'FLEETKIT_DEPLOY_KEY is empty' "$work/err" && echo yes || echo no)"
check "and writes no alias" "no" "$([ -e "$work/empty/.ssh/config" ] && echo yes || echo no)"

spec_summary
