#!/usr/bin/env bash
# Assertions for scripts/dev-server.sh: its Mongo is the local one (:28017, or $LOCAL_MIRROR_PORT), never from the vault
# or a .env.local; its TMDB_API_KEY (the /debug/review cards' live TMDB posters) is from the vault. `sbt`
# is a fake on PATH that prints the variables it was started with; `secrets` is a fake vault that records
# how it was called and runs the command with the key set; the local-Mongo starter is never reached
# because the spec says :28017 already answers.
#
#   scripts/dev-server-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-dev-server.XXXXXX")"
trap 'rm -rf "$SCRATCH"' EXIT
# The script runs in a checkout of this spec's own, never this one: it deletes the wildcard exporters' classes under the
# checkout it runs from, so here it would delete a warm build's, and on a read-only checkout (run-script-tests --linux,
# CI's check tree with its restored target/) that `rm` fails and the script exits before sbt (precommit-check run
# 43345620431).
DEV="$SCRATCH/repo"
mkdir -p "$DEV/scripts/ci" "$DEV/identity/src/main/scala/services/identity" || exit 1
cp "$REPO_ROOT/scripts/dev-server.sh" "$REPO_ROOT/scripts/vault-env.sh" "$REPO_ROOT/scripts/tool-conventions.sh" "$DEV/scripts/" &&
  cp "$REPO_ROOT/scripts/ci/sbt-invalidate-exporters.sh" "$DEV/scripts/ci/" &&
  cp "$REPO_ROOT/identity/src/main/scala/services/identity/IdentityMeasures.scala" \
    "$DEV/identity/src/main/scala/services/identity/" || exit 1
# A compiled exporter (IdentityMeasures, `export CreditMeasures.*`), as a warm build leaves it.
STALE_DIR="$DEV/identity/target/scala-3/classes/services/identity"
FAKE="$SCRATCH/bin"; mkdir -p "$FAKE" || exit 1
cat > "$FAKE/sbt" <<'SBT'
#!/usr/bin/env bash
printf 'URI=%s DB=%s MIRROR=%s ARGS=%s' "${MONGODB_URI-}" "${MONGODB_DB-}" "${MONGODB_MOVIES_MIRROR_URI-}" "$*"
SBT
chmod +x "$FAKE/sbt"
cat > "$FAKE/secrets" <<'SECRETS'
#!/usr/bin/env bash
echo "$*" >> "$VAULT_CALLS"
while [ "$1" != "--" ]; do shift; done; shift
TMDB_API_KEY=vaulted exec "$@"
SECRETS
chmod +x "$FAKE/secrets"
export VAULT_CALLS="$SCRATCH/vault-calls"

run() { env -u LOCAL_MIRROR_PORT -u MONGODB_URI -u MONGODB_DB -u MONGODB_MOVIES_MIRROR_URI -u TMDB_API_KEY DEV_SERVER_MONGO_UP=1 PATH="$FAKE:$PATH" "$@" 2>/dev/null; }
# The TMDB_API_KEY web/run got, and what the vault was asked for.
tmdb_key() {
  : > "$VAULT_CALLS"
  cat > "$FAKE/sbt" <<'SBT'
#!/usr/bin/env bash
printf 'TMDB=%s' "${TMDB_API_KEY-}"
SBT
  printf '%s VAULT=%s' "$(run "$@")" "$(tr '\n' ';' < "$VAULT_CALLS")"
}

echo "[spec] dev-server.sh"
check "with nothing set, web/run gets the local Mongo, its playground db and the prod mirror for /debug" \
  "URI=mongodb://127.0.0.1:28017/?directConnection=true DB=kinowo_local MIRROR=mongodb://127.0.0.1:28017/kinowo_prod_mirror?directConnection=true ARGS=-J-Xmx2500m web/run" \
  "$(run "$DEV/scripts/dev-server.sh")"
check "LOCAL_MIRROR_PORT moves both URIs, as it moves every other tool's local Mongo" \
  "URI=mongodb://127.0.0.1:28999/?directConnection=true DB=kinowo_local MIRROR=mongodb://127.0.0.1:28999/kinowo_prod_mirror?directConnection=true ARGS=-J-Xmx2500m web/run" \
  "$(run LOCAL_MIRROR_PORT=28999 "$DEV/scripts/dev-server.sh")"
check "an explicit environment wins, and extra sbt flags pass through" \
  "URI=mongodb://127.0.0.1:27017/?directConnection=true DB=kinowo_uk MIRROR=m ARGS=-J-Xmx2500m -J-Xmx4g web/run" \
  "$(run MONGODB_URI='mongodb://127.0.0.1:27017/?directConnection=true' MONGODB_DB=kinowo_uk MONGODB_MOVIES_MIRROR_URI=m \
       "$DEV/scripts/dev-server.sh" -J-Xmx4g)"
check "with no TMDB_API_KEY set, web/run gets it from the vault (the review cards' TMDB posters)" \
  "TMDB=vaulted VAULT=run movies TMDB_API_KEY -- $BASH $DEV/scripts/dev-server.sh;" \
  "$(tmdb_key "$DEV/scripts/dev-server.sh")"
check "an exported TMDB_API_KEY wins, with no vault lookup" \
  "TMDB=mine VAULT=" \
  "$(tmdb_key env TMDB_API_KEY=mine "$DEV/scripts/dev-server.sh")"
# Zinc misses a member added behind a wildcard export, so a warm target/ fails with "value X is not a member of object
# IdentityMeasures" until the exporter's classes go (scripts/ci/sbt-invalidate-exporters.sh, which CI runs after a restore).
mkdir -p "$STALE_DIR" && : > "$STALE_DIR/IdentityMeasures.class" || exit 1
cat > "$FAKE/sbt" <<SBT
#!/usr/bin/env bash
if [ -e "$STALE_DIR/IdentityMeasures.class" ]; then echo stale; else echo invalidated; fi
SBT
check "the wildcard exporters' compiled classes are gone before sbt starts, so zinc recompiles them" \
  "invalidated" \
  "$(run "$DEV/scripts/dev-server.sh" | tail -1)"
spec_summary
