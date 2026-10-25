#!/usr/bin/env bash
# Assertions for scripts/with-prod-mongo.sh and prod-tunnel.sh's prod_tunnel_uri. No vault, no ssh,
# no Mongo: `secrets`, `ssh`, `nc` and `mongosh` are fakes on PATH that record what they were asked.
#
#   scripts/with-prod-mongo-test.sh
set -uo pipefail
# the URIs these checks assert name the pinned port; the free-port pick has its own spec (prod-tunnel-test.sh)
export PROD_TUNNEL_PORT=27017
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-with-prod-mongo.XXXXXX")"
trap 'rm -rf "$SCRATCH"' EXIT
FAKE="$SCRATCH/bin"; mkdir -p "$FAKE"
export CALLS="$SCRATCH/calls" TUNNEL_UP="$SCRATCH/tunnel-up" SSH_PID="$SCRATCH/ssh.pid"

# `secrets run movies SPEC... -- CMD`: records the call, then runs CMD with each SPEC's variable set
# to "vault:<KEY>" — enough to prove the re-exec carries the value back in, never a real secret.
cat > "$FAKE/secrets" <<'EOF'
#!/usr/bin/env bash
echo "secrets $*" >> "$CALLS"
shift 2
while [ "$1" != "--" ]; do var="${1%%=*}"; key="${1#*=}"; export "$var=mongodb://u:p@10.20.0.13:27017/kinowo?authSource=$key"; shift; done
shift
exec "$@"
EOF
# The tunnel: nothing answers on :27017 until our ssh forward is "up".
cat > "$FAKE/ssh" <<'EOF'
#!/usr/bin/env bash
echo "ssh ${*: -1}" >> "$CALLS"; echo $$ > "$SSH_PID"; touch "$TUNNEL_UP"; exec sleep 30
EOF
printf '#!/bin/sh\n[ -e "$TUNNEL_UP" ]\n' > "$FAKE/nc"
printf '#!/bin/sh\n[ -e "$TUNNEL_UP" ]\n' > "$FAKE/mongosh"
chmod +x "$FAKE"/*

# What the wrapped command saw.
show_env() { printf 'URI=%s DB=%s' "$MONGODB_URI" "$MONGODB_DB"; }
export -f show_env

run() { rm -f "$CALLS" "$TUNNEL_UP"; env -u MONGODB_URI -u MONGO_ROOT_URI -u MONGODB_DB -u KINOWO_MONGO_SSH PATH="$FAKE:$PATH" "$@"; }

echo "[spec] prod_tunnel_uri"
uri() { (. "$REPO_ROOT/scripts/local-mirror/prod-tunnel.sh"; prod_tunnel_uri "$1"); }
check "the in-cluster host becomes the tunnel, query kept" \
  "mongodb://u:p@127.0.0.1:27017/kinowo?authSource=admin&replicaSet=rs0&directConnection=true" \
  "$(uri 'mongodb://u:p@10.20.0.13:27017/kinowo?authSource=admin&replicaSet=rs0')"
check "a host list collapses to the one tunnel" \
  "mongodb://u:p@127.0.0.1:27017/kinowo?directConnection=true" \
  "$(uri 'mongodb://u:p@10.20.0.13:27017,10.20.0.14:27017/kinowo')"
check "an existing directConnection is replaced, not duplicated" \
  "mongodb://u:p@127.0.0.1:27017/kinowo?w=1&directConnection=true" \
  "$(uri 'mongodb://u:p@127.0.0.1:27017/kinowo?directConnection=false&w=1')"
check "a URI already in tunnel form passes through" \
  "mongodb://u:p@127.0.0.1:27017/kinowo?directConnection=true" \
  "$(uri 'mongodb://u:p@127.0.0.1:27017/kinowo?directConnection=true')"

echo "[spec] with-prod-mongo.sh"
out="$(run "$REPO_ROOT/scripts/with-prod-mongo.sh" bash -c show_env 2>/dev/null)"
check "an unset MONGODB_URI is read from the vault, once" \
  "secrets run movies MONGODB_URI -- $BASH $REPO_ROOT/scripts/with-prod-mongo.sh bash -c show_env" \
  "$(grep '^secrets' "$CALLS")"
check "the command sees the tunnel form of the vault's URI and MONGODB_DB=kinowo" \
  "URI=mongodb://u:p@127.0.0.1:27017/kinowo?authSource=MONGODB_URI&directConnection=true DB=kinowo" "$out"
check "the tunnel was opened to mongo-1" "ssh root@178.105.221.61" "$(grep '^ssh' "$CALLS")"

out="$(run MONGODB_URI='mongodb://me:pw@10.20.0.13:27017/kinowo_uk?authSource=admin' MONGODB_DB=kinowo_uk \
  "$REPO_ROOT/scripts/with-prod-mongo.sh" bash -c show_env 2>/dev/null)"
check "a pre-set MONGODB_URI skips the vault" "" "$(grep '^secrets' "$CALLS")"
check "…and is still rewritten to the tunnel, MONGODB_DB kept" \
  "URI=mongodb://me:pw@127.0.0.1:27017/kinowo_uk?authSource=admin&directConnection=true DB=kinowo_uk" "$out"

# macOS's own /bin/bash is 3.2, where an empty array under `set -u` is "unbound": run without
# --root (no flags) it aborted before reaching the vault.
if [ -x /bin/bash ] && [ "$(/bin/bash -c 'echo ${BASH_VERSINFO[0]}')" -lt 4 ]; then
  out="$(run /bin/bash "$REPO_ROOT/scripts/with-prod-mongo.sh" bash -c show_env 2>/dev/null)"
  check "runs under bash 3.2 without --root" \
    "URI=mongodb://u:p@127.0.0.1:27017/kinowo?authSource=MONGODB_URI&directConnection=true DB=kinowo" "$out"
fi

out="$(run "$REPO_ROOT/scripts/with-prod-mongo.sh" --root bash -c show_env 2>/dev/null)"
check "--root asks the vault for MONGO_ROOT_URI" \
  "secrets run movies MONGO_ROOT_URI -- $BASH $REPO_ROOT/scripts/with-prod-mongo.sh --root bash -c show_env" \
  "$(grep '^secrets' "$CALLS")"
check "…and exports it as MONGODB_URI" \
  "URI=mongodb://u:p@127.0.0.1:27017/kinowo?authSource=MONGO_ROOT_URI&directConnection=true DB=kinowo" "$out"

show_both() { printf 'APP=%s ROOT=%s' "$MONGODB_URI" "$MONGO_ROOT_URI"; }
export -f show_both
out="$(run "$REPO_ROOT/scripts/with-prod-mongo.sh" --both bash -c show_both 2>/dev/null)"
check "--both asks the vault for both keys in ONE run (one Touch ID)" \
  "secrets run movies MONGODB_URI MONGO_ROOT_URI -- $BASH $REPO_ROOT/scripts/with-prod-mongo.sh --both bash -c show_both" \
  "$(grep '^secrets' "$CALLS")"
check "…exports the app's URI as MONGODB_URI and the root's as MONGO_ROOT_URI, both through the tunnel" \
  "APP=mongodb://u:p@127.0.0.1:27017/kinowo?authSource=MONGODB_URI&directConnection=true ROOT=mongodb://u:p@127.0.0.1:27017/kinowo?authSource=MONGO_ROOT_URI&directConnection=true" \
  "$out"
check "…over one tunnel" "1" "$(grep -c '^ssh' "$CALLS")"

out="$(run MONGODB_URI='mongodb://app:pw@10.20.0.13:27017/kinowo?authSource=kinowo' \
  "$REPO_ROOT/scripts/with-prod-mongo.sh" --both bash -c show_both 2>/dev/null)"
check "--both with the app's URI already set asks the vault for the root's only" \
  "secrets run movies MONGO_ROOT_URI -- $BASH $REPO_ROOT/scripts/with-prod-mongo.sh --both bash -c show_both" \
  "$(grep '^secrets' "$CALLS")"

# Unpinned, the tunnel takes a free port of its own: both URIs must name that same one.
out="$(run env -u PROD_TUNNEL_PORT "$REPO_ROOT/scripts/with-prod-mongo.sh" --both bash -c show_both 2>/dev/null)"
ports="$(printf '%s' "$out" | grep -oE '127\.0\.0\.1:[0-9]+' | sort -u)"
check "--both on a free port puts both URIs on the one port the tunnel took" "1" "$(printf '%s\n' "$ports" | grep -c .)"
check "…and that port is not the pinned default" "" "$(printf '%s' "$ports" | grep ':27017$')"

run "$REPO_ROOT/scripts/with-prod-mongo.sh" bash -c 'exit 7' >/dev/null 2>&1
check "the command's exit status is the wrapper's" "7" "$?"
sleep 0.2
check "the tunnel it opened is closed when the command ends" "gone" \
  "$(if [ ! -f "$SSH_PID" ]; then echo never-opened; elif kill -0 "$(cat "$SSH_PID")" 2>/dev/null; then echo running; else echo gone; fi)"
spec_summary
