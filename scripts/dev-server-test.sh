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
FAKE="$SCRATCH/bin"; mkdir -p "$FAKE"
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
  "$(run "$REPO_ROOT/scripts/dev-server.sh")"
check "LOCAL_MIRROR_PORT moves both URIs, as it moves every other tool's local Mongo" \
  "URI=mongodb://127.0.0.1:28999/?directConnection=true DB=kinowo_local MIRROR=mongodb://127.0.0.1:28999/kinowo_prod_mirror?directConnection=true ARGS=-J-Xmx2500m web/run" \
  "$(run LOCAL_MIRROR_PORT=28999 "$REPO_ROOT/scripts/dev-server.sh")"
check "an explicit environment wins, and extra sbt flags pass through" \
  "URI=mongodb://127.0.0.1:27017/?directConnection=true DB=kinowo_uk MIRROR=m ARGS=-J-Xmx2500m -J-Xmx4g web/run" \
  "$(run MONGODB_URI='mongodb://127.0.0.1:27017/?directConnection=true' MONGODB_DB=kinowo_uk MONGODB_MOVIES_MIRROR_URI=m \
       "$REPO_ROOT/scripts/dev-server.sh" -J-Xmx4g)"
check "with no TMDB_API_KEY set, web/run gets it from the vault (the review cards' TMDB posters)" \
  "TMDB=vaulted VAULT=run movies TMDB_API_KEY -- $BASH $REPO_ROOT/scripts/dev-server.sh;" \
  "$(tmdb_key "$REPO_ROOT/scripts/dev-server.sh")"
check "an exported TMDB_API_KEY wins, with no vault lookup" \
  "TMDB=mine VAULT=" \
  "$(tmdb_key env TMDB_API_KEY=mine "$REPO_ROOT/scripts/dev-server.sh")"
spec_summary
