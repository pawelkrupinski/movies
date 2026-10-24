#!/usr/bin/env bash
# Gives a CI job the `forgejo-git` ssh alias that infra/version-dashboard's package.json names
# (`git+ssh://forgejo-git/bitcashier/fleetkit.git`), so its `npm ci` can fetch fleetkit.
#
# WHY AN ALIAS AND NOT A REWRITTEN URL. On the laptop `forgejo-git` is an ~/.ssh/config Host that
# reaches Forgejo's ssh through the tunnel; a runner has no such Host, could not resolve the name, and
# nix-evaluate failed at `npm ci` (run 1227, 2026-10-24). Defining the same alias here, pointed at
# Forgejo's own address, keeps package.json and its lock identical on both sides.
#
# WHY A DEPLOY KEY. fleetkit is private and in the bitcashier org: this repository's job token
# cannot read it. FLEETKIT_DEPLOY_KEY is the private half of a READ-ONLY deploy key on
# bitcashier/fleetkit (Settings -> Deploy keys, "movies-ci"), so the secret reads that one
# repository and nothing else. Rotate: new key pair, replace the deploy key and the secret.
#
# Usage: FLEETKIT_DEPLOY_KEY=<private key> fleetkit-access.sh
# Env: FORGEJO_SSH_HOST (forgejo.internal), FORGEJO_SSH_PORT (2222), HOME (where ~/.ssh is written).
set -euo pipefail

host="${FORGEJO_SSH_HOST:-forgejo.internal}"
port="${FORGEJO_SSH_PORT:-2222}"
# Forgejo's built-in ssh server key, pinned: the same key the laptop's known_hosts holds for
# [localhost]:2222 (the tunnel's far end is this server).
host_key='ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC7Jfn/U6TV7ajTa91hMFQRvqXAma/wbMWema0E5FysjaMuGX6RCCMnRGzjZKpjip6VASIpS0xzkcuvrUTTKoRPdGxj3BfITHfQZdKI0OYX28WfoXX+MDM0zFcfNYhE32guD2tcR+YRWRBVGU/aeYdrmCoR4bq2dgdDhp1NocnUdWoCqXg+mKFRYp7ilJxXvx61mB3zIxVl8Fax6BQs8TMoglAWNqMNLQixameZj3dxJuHqTVFa2doMKWiNhItZcGvnltd/A6B4fP8+dc/N6evYgE02R5nNOHB6DneHGugtGpGcCaCJui+710qn7xMpg4yhUo8+cvdAQktYXEZspZwWzNIZYJGxLMhJD1Aog++c+kbBCt0uECN+03HcFckuUcd8tKVOpQQ+sHdLVfIJPCFfhVkUfcbMHTPtAMfR51d1JcVXZSqt7+g4vOXAYHUFhPZJYU1YsbIqqpduwCdIUerKbtdDQRm0kCFJaUzIUR6KmN248OwCSaicTqLDQRzTjzEdesPPJoy60kYPvxrhlNvlFCK5G/GUfR915hbPDTpDPGVxYfcpJBsd+qU6/rWakFQGac6r2eNEbTr673Yo3EnWrACm/89+A1SgxkC+J69Y34/4UQ6HuEH9nzuCRLeco/HhwOc0tTsd+tJSTawqrpsTnAvj2bp233AbE9KrD5PxqQ=='

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
printf '[%s]:%s %s\n' "$host" "$port" "$host_key" > "$known"

cat >> "$ssh_dir/config" <<EOF

Host forgejo-git
    HostName $host
    Port $port
    User forgejo
    IdentityFile $key
    IdentitiesOnly yes
    UserKnownHostsFile $known
    StrictHostKeyChecking yes
    BatchMode yes
EOF
chmod 600 "$ssh_dir/config"
echo "forgejo-git -> $host:$port with the fleetkit deploy key"
