#!/usr/bin/env bash
# Assertions for scripts/identity-capture.sh's vault and tunnel handling. No vault, no ssh, no sbt,
# no JVM: `secrets`, `ssh`, `nc`, `mongosh`, `sbt` and `java` are fakes on PATH that record what they
# were asked, and `java` records the environment the capture JVMs would have inherited.
#
#   scripts/identity-capture-test.sh
set -uo pipefail
# the URIs these checks assert name the pinned port; the free-port pick has its own spec (prod-tunnel-test.sh)
export PROD_TUNNEL_PORT=27017
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-identity-capture.XXXXXX")"
trap 'rm -rf "$SCRATCH"' EXIT
FAKE="$SCRATCH/bin"; mkdir -p "$FAKE"
export CALLS="$SCRATCH/calls" TUNNEL_UP="$SCRATCH/tunnel-up" CLASSES="$SCRATCH/worker/target/scala-3/test-classes"
mkdir -p "$CLASSES/scripts"; touch "$CLASSES/scripts/IdentityCapture.class"

# `secrets run movies SPEC... -- CMD`: runs CMD with each SPEC's variable set to a fake in-cluster URI
# (or a fake key), never a real secret.
cat > "$FAKE/secrets" <<'FAKE_EOF'
#!/usr/bin/env bash
echo "secrets $*" >> "$CALLS"
shift 2
while [ "$1" != "--" ]; do export "${1%%=*}=mongodb://u:p@10.20.0.13:27017/kinowo?authSource=admin"; shift; done
shift
exec "$@"
FAKE_EOF
cat > "$FAKE/ssh" <<'FAKE_EOF'
#!/usr/bin/env bash
touch "$TUNNEL_UP"; exec sleep 30
FAKE_EOF
printf '#!/bin/sh\n[ -e "$TUNNEL_UP" ]\n' > "$FAKE/nc"
printf '#!/bin/sh\n[ -e "$TUNNEL_UP" ]\n' > "$FAKE/mongosh"
# sbt "compiles" and prints the one classpath line the script keeps.
printf '#!/bin/sh\necho "%s"\n' "$CLASSES" > "$FAKE/sbt"
# java records what the capture would have inherited, then exits clean.
cat > "$FAKE/java" <<'FAKE_EOF'
#!/usr/bin/env bash
echo "java MONGODB_URI=${MONGODB_URI:-<unset>} FAMILY=${KINOWO_IDENTITY_FAMILY_URI:-<unset>}" >> "$CALLS"
FAKE_EOF
chmod +x "$FAKE"/*

echo "[spec] identity-capture.sh"
( cd "$REPO_ROOT" && env -u MONGODB_URI -u KINOWO_IDENTITY_FAMILY_URI -u KINOWO_IDENTITY_FAMILY_SEED \
    -u KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY -u KINOWO_PROD_MONGODB_URI -u KINOWO_MONGO_SSH \
    PATH="$FAKE:$PATH" KINOWO_IDENTITY_CAPTURE_WORK="$SCRATCH/work" \
    scripts/identity-capture.sh pl > "$SCRATCH/out" 2>&1 )
check "a capture that reads prod exits clean" "0" "$?"
check "the capture JVMs inherit NO MONGODB_URI, so each seeds its local throwaway database, never prod" \
  "<unset>" "$(sed -n 's/^java MONGODB_URI=\([^ ]*\) .*/\1/p' "$CALLS")"
check "prod reaches the capture only as the family export's route, through the tunnel" \
  "mongodb://u:p@127.0.0.1:27017/kinowo?authSource=admin&directConnection=true" \
  "$(sed -n 's/^java .* FAMILY=\(.*\)$/\1/p' "$CALLS")"

spec_summary
