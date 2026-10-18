#!/usr/bin/env bash
# scripts/local-mirror/sync-enrichment-cache.sh: which countries it dumps, that a country prod has no cache for is
# skipped, that --dry-run restores nothing, and that the dump goes through a tunnel of its own to mongo-1. ssh, nc,
# mongosh, mongodump, bsondump and mongorestore are stubs: no prod, no Mongo, no network.
# Run: bash scripts/local-mirror/sync-enrichment-cache-test.sh
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$HERE/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

scratch="$(mktemp -d)"; trap 'rm -rf "$scratch"' EXIT
mkdir -p "$scratch/bin"
export SSH_PID="$scratch/ssh-pid" CALLS="$scratch/calls" TUNNEL_UP="$scratch/tunnel-up"
# the tunnel "answers" once the ssh stub ran; the local mirror (:28017) always does
cat > "$scratch/bin/nc" <<'STUB'
#!/bin/sh
for a; do port=$a; done
[ "$port" = 28017 ] || [ -e "$TUNNEL_UP" ]
STUB
printf '#!/bin/sh\necho "ssh $*" >> "$CALLS"; echo $$ > "$SSH_PID"; touch "$TUNNEL_UP"; exec sleep 30\n' > "$scratch/bin/ssh"
printf '#!/bin/sh\nexit 0\n' > "$scratch/bin/mongosh"
# prod holds an enrichment cache for pl and de only
cat > "$scratch/bin/mongodump" <<'STUB'
#!/bin/bash
for a; do case "$a" in --uri=*) uri=${a#--uri=} ;; --db=*) db=${a#--db=} ;; --collection=*) coll=${a#--collection=} ;; --out=*) out=${a#--out=} ;; esac; done
echo "mongodump $db.$coll $uri" >> "$CALLS"
case "$coll" in *_pl|*_de) mkdir -p "$out/$db"; echo x > "$out/$db/$coll.bson" ;; esac
STUB
printf '#!/bin/sh\nprintf "a\\nb\\nc\\n"\n' > "$scratch/bin/bsondump"
cat > "$scratch/bin/mongorestore" <<'STUB'
#!/bin/sh
echo "mongorestore $*" >> "$CALLS"
STUB
chmod +x "$scratch/bin"/*
export PATH="$scratch/bin:$PATH"

sync() { rm -f "$CALLS" "$TUNNEL_UP"
  env -u KINOWO_MONGO_SSH -u MONGO_ROOT_URI -u LOCAL_MONGO_URI -u LOCAL_MIRROR_PORT PROD_TUNNEL_PORT=27017 \
    MONGODB_URI="mongodb://u:p@10.20.0.13:27017/kinowo?authSource=admin" bash "$HERE/sync-enrichment-cache.sh" "$@" > "$scratch/out" 2>&1; }

sync pl uk de; rc=$?
check "a sync exits clean" "0" "$rc"
check "each requested country's cache is dumped, in the order given" "convergence_test.enrichment_cache_pl convergence_test.enrichment_cache_uk convergence_test.enrichment_cache_de" \
  "$(sed -n 's/^mongodump \([^ ]*\) .*/\1/p' "$CALLS" | tr '\n' ' ' | sed 's/ $//')"
check "...from prod through the tunnel, not the in-cluster address" "yes" \
  "$(grep '^mongodump' "$CALLS" | grep -q '@127.0.0.1:27017/' && ! grep -q '10.20.0.13' "$CALLS" && echo yes || echo no)"
check "...the tunnel being an ssh forward to mongo-1" "yes" "$(grep -q '^ssh .*-L 27017:127.0.0.1:27017.* root@178.105.221.61' "$CALLS" && echo yes || echo no)"
check "a country prod has no cache for is skipped, saying so" "yes" "$(grep -q 'enrichment_cache_uk: nothing dumped' "$scratch/out" && echo yes || echo no)"
check "the others are restored into the local mirror" "convergence_test.enrichment_cache_pl convergence_test.enrichment_cache_de" \
  "$(sed -n 's/^mongorestore .*--db=\([^ ]*\) --collection=\([^ ]*\) .*/\1.\2/p' "$CALLS" | tr '\n' ' ' | sed 's/ $//')"
check "...by upsert, never --drop" "0" "$(grep -c -- '--drop' "$CALLS")"
check "...into the local mirror on :28017" "yes" "$(grep '^mongorestore' "$CALLS" | grep -q 'mongodb://127.0.0.1:28017' && echo yes || echo no)"
check "the total is reported" "yes" "$(grep -q 'synced 6 entries' "$scratch/out" && echo yes || echo no)"

sync --dry-run pl; rc=$?
check "--dry-run exits clean, dumps, and restores nothing" "0 1 0" "$rc $(grep -c '^mongodump' "$CALLS") $(grep -c '^mongorestore' "$CALLS")"
check "...reporting what is available" "yes" "$(grep -q 'dry run: 3 entries available' "$scratch/out" && echo yes || echo no)"

sync all-of-them; rc=$?
check "an unknown argument is refused" "1" "$rc"
check "...before any tunnel or dump" "0" "$(grep -c . "$CALLS" 2>/dev/null || echo 0)"
sync pl
sleep 0.5
check "the tunnel's ssh is closed on exit, none stranded" "gone" "$(kill -0 "$(cat "$SSH_PID")" 2>/dev/null && echo alive || echo gone)"
spec_summary
