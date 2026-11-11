#!/usr/bin/env bash
# Gives a CI job ssh access to github.com with the fleetkit deploy key, so the `npm ci` of
# infra/version-dashboard can fetch the dependency its package.json names
# (`git+ssh://git@github.com/bitcashier/fleetkit.git`).
#
# WHY A DEPLOY KEY. fleetkit is private and in the bitcashier org: this repository's job token
# cannot read it. FLEETKIT_DEPLOY_KEY is the private half of a READ-ONLY deploy key on
# bitcashier/fleetkit (Settings -> Deploy keys, "movies-ci"), so the secret reads that one
# repository and nothing else. Rotate: new key pair, replace the deploy key and the secret.
#
# The host block is github.com:22 as `git`, pinned to GitHub's published ed25519 host key (api.github.com/meta).
#
# Usage: FLEETKIT_DEPLOY_KEY=<private key> fleetkit-access.sh
# Env: HOME (where ~/.ssh is written).
set -euo pipefail

host=github.com
port=22
user=git
host_key='ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl'

if [ -z "${FLEETKIT_DEPLOY_KEY:-}" ]; then
  echo "::error title=No fleetkit deploy key::FLEETKIT_DEPLOY_KEY is empty, so npm ci cannot fetch bitcashier/fleetkit. Add the repository secret (read-only deploy key on bitcashier/fleetkit)." >&2
  exit 1
fi

ssh_dir="$HOME/.ssh"
mkdir -p "$ssh_dir"
chmod 700 "$ssh_dir"
key="$ssh_dir/fleetkit_deploy"
( umask 077; printf '%s\n' "$FLEETKIT_DEPLOY_KEY" > "$key" )
known="$ssh_dir/fleetkit_known_hosts"
printf '%s %s\n' "$host" "$host_key" > "$known"

cat >> "$ssh_dir/config" <<EOF

Host github.com
    HostName $host
    Port $port
    User $user
    IdentityFile $key
    IdentitiesOnly yes
    UserKnownHostsFile $known
    StrictHostKeyChecking yes
    BatchMode yes
    # Ask for the one key pinned, not the RSA or ECDSA github.com would otherwise offer first.
    HostKeyAlgorithms ssh-ed25519
EOF
chmod 600 "$ssh_dir/config"
echo "$host:$port with the fleetkit deploy key"
