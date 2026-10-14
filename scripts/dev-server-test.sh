#!/usr/bin/env bash
# Assertions for scripts/dev-server.sh's Mongo: the local :28017 one, hardcoded, no vault and no
# .env.local. `sbt` is a fake on PATH that prints the Mongo variables it was started with; the
# local-Mongo starter is never reached because the spec says :28017 already answers.
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

run() { env -u MONGODB_URI -u MONGODB_DB -u MONGODB_MOVIES_MIRROR_URI DEV_SERVER_MONGO_UP=1 PATH="$FAKE:$PATH" "$@" 2>/dev/null; }

echo "[spec] dev-server.sh"
check "with nothing set, web/run gets the local Mongo, its playground db and the prod mirror for /debug" \
  "URI=mongodb://127.0.0.1:28017/?directConnection=true DB=kinowo_local MIRROR=mongodb://127.0.0.1:28017/kinowo_prod_mirror?directConnection=true ARGS=-J-Xmx2500m web/run" \
  "$(run "$REPO_ROOT/scripts/dev-server.sh")"
check "an explicit environment wins, and extra sbt flags pass through" \
  "URI=mongodb://127.0.0.1:27017/?directConnection=true DB=kinowo_uk MIRROR=m ARGS=-J-Xmx2500m -J-Xmx4g web/run" \
  "$(run MONGODB_URI='mongodb://127.0.0.1:27017/?directConnection=true' MONGODB_DB=kinowo_uk MONGODB_MOVIES_MIRROR_URI=m \
       "$REPO_ROOT/scripts/dev-server.sh" -J-Xmx4g)"
spec_summary
