#!/usr/bin/env bash
# Assertions for scripts/identity-capture.sh's vault and tunnel handling, and its --offline mode. No vault, no ssh,
# no sbt, no JVM, no Mongo: `secrets`, `ssh`, `nc`, `gh`, `mongosh`, `mongod`, `mongoexport`, `sbt` and `java` are
# fakes on PATH that record what they were asked, and `java` records the environment the capture JVMs would have
# inherited.
#
#   scripts/identity-capture-test.sh
set -uo pipefail
# the URIs these checks assert name the pinned port; the free-port pick has its own spec (prod-tunnel-test.sh)
export PROD_TUNNEL_PORT=27017
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-identity-capture.XXXXXX")"
trap 'rm -rf "$SCRATCH"' EXIT
SCRATCH="$(cd "$SCRATCH" && pwd)" # as the script names its work directory: no `//` from a trailing-slash TMPDIR
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
# mongosh: the tunnel probe answers while the tunnel is up; the offline capture's own mongod answers with its dbpath
# and is primary; the mirror's freshness exits $FRESHNESS_EXIT (0 fresh, 3 stale, 2 no mirror)
cat > "$FAKE/mongosh" <<'FAKE_EOF'
#!/usr/bin/env bash
case "$*" in
  *freshness.js*) echo "freshness $*" >> "$CALLS"; exit "${FRESHNESS_EXIT:-0}" ;;
  *serverCmdLineOpts*) cat "$CALLS.dbpath" 2>/dev/null ;;
  *myState*) echo 1 ;;
  *rs.initiate*) echo ok ;;
  *) [ -e "$TUNNEL_UP" ] ;;
esac
FAKE_EOF
# mongod: records its arguments, --dbpath and pid, then runs until killed
cat > "$FAKE/mongod" <<'FAKE_EOF'
#!/usr/bin/env bash
[ "${1:-}" = --version ] && { echo "db version v8.0.0"; exit 0; }
echo "mongod $*" >> "$CALLS"
while [ $# -gt 0 ]; do [ "$1" = --dbpath ] && echo "$2" > "$CALLS.dbpath"; shift; done
echo $$ > "$CALLS.mongod-pid"
exec sleep 300
FAKE_EOF
# mongoexport: records what it was asked and writes one family answer where it was told
cat > "$FAKE/mongoexport" <<'FAKE_EOF'
#!/usr/bin/env bash
echo "mongoexport $*" >> "$CALLS"
while [ $# -gt 0 ]; do [ "$1" = --out ] && echo '{"_id":"family"}' > "$2"; shift; done
FAKE_EOF
# sbt "compiles" and prints the one classpath line the script keeps.
printf '#!/bin/sh\necho "%s"\n' "$CLASSES" > "$FAKE/sbt"
# java records what the capture would have inherited, then exits clean. Handed a corpus of the caller's own, it stamps
# <cc>.inputs `recording<TAB>corpus <hash>` as IdentityCapture does, and adds a page to the enrichment tree as a capture
# that read venue pages live would.
cat > "$FAKE/java" <<'FAKE_EOF'
#!/usr/bin/env bash
echo "java MONGODB_URI=${MONGODB_URI:-<unset>} FAMILY=${KINOWO_IDENTITY_FAMILY_URI:-<unset>} NEWEST=${KINOWO_IDENTITY_NEWEST_RECORDING:-<unset>}" >> "$CALLS"
echo "java-env SEED=${KINOWO_IDENTITY_FAMILY_SEED:-<unset>} CORPUS=${KINOWO_IDENTITY_CORPUS_DIR:-<unset>} TREE=${KINOWO_FIXTURE_ROOT:-<unset>}" >> "$CALLS"
echo "java-args ${*: -2}" >> "$CALLS"
if [ -n "${KINOWO_IDENTITY_CORPUS_DIR:-}" ] && [ "${*: -2:1}" = --capture ]; then
  cc=${*: -1}
  hash=$(python3 -I -c 'import hashlib,sys; print(hashlib.sha256(open(sys.argv[1],"rb").read()).hexdigest()[:12])' \
    "$KINOWO_IDENTITY_CORPUS_DIR/cinema-scrapes-$cc.json.gz")
  printf 'recording\tcorpus %s\ncode\tabc123\n' "$hash" > "$KINOWO_IDENTITY_UNMATCHED_CAPTURE/$cc.inputs"
  echo page > "$KINOWO_FIXTURE_ROOT/enrichment-$cc/added-by-capture"
fi
FAKE_EOF
# gh lists the runs out of creation order, as GitHub does
cat > "$FAKE/gh" <<'FAKE_EOF'
#!/usr/bin/env bash
echo "gh $*" >> "$CALLS"
printf '41\t2026-10-01T00:00:00Z\n43\t2026-10-03T00:00:00Z\n42\t2026-10-02T00:00:00Z\n'
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
  "$(sed -n 's/^java .* FAMILY=\([^ ]*\) .*$/\1/p' "$CALLS")"
check "the capture JVM is handed the newest recording (by creation time), found by the shared helper" "43" \
  "$(sed -n 's/^java .* NEWEST=\(.*\)$/\1/p' "$CALLS")"

# ── --offline: the family answers from the local prod mirror, the recording from the cache, a mongod of its own ──
CACHE="$SCRATCH/cache"; RUN=38713274617
for cc in uk pl; do
  mkdir -p "$CACHE/recordings/$RUN/pair/test/resources/fixtures/enrichment-$cc" "$CACHE/recordings/$RUN/pair/test/resources/fixtures/corpus"
  echo tree > "$CACHE/recordings/$RUN/pair/test/resources/fixtures/enrichment-$cc/page"
  echo "corpus $cc" > "$CACHE/recordings/$RUN/pair/test/resources/fixtures/corpus/cinema-scrapes-$cc.json.gz"
done
cache_sum() { (cd "$CACHE" && find . -type f | LC_ALL=C sort | while read -r f; do echo "$f"; cat "$f"; done | cksum); }
cached_before=$(cache_sum)
offline() {
  rm -f "$CALLS" "$CALLS".* "$TUNNEL_UP"; rm -rf "$SCRATCH/work" "$SCRATCH/fixtures"; mkdir -p "$SCRATCH/fixtures"
  ( cd "$REPO_ROOT" && env -u MONGODB_URI -u KINOWO_IDENTITY_FAMILY_URI -u KINOWO_IDENTITY_FAMILY_SEED \
      -u KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY -u KINOWO_PROD_MONGODB_URI -u KINOWO_MONGO_SSH \
      -u KINOWO_IDENTITY_CORPUS_DIR -u KINOWO_FIXTURE_ROOT -u LOCAL_MIRROR_PORT \
      PATH="$FAKE:$PATH" KINOWO_IDENTITY_CAPTURE_WORK="$SCRATCH/work" KINOWO_IDENTITY_CACHE="$CACHE" \
      KINOWO_IDENTITY_UNMATCHED_CAPTURE="$SCRATCH/fixtures" IT_LOCAL_START_SECONDS=2 "$@" ) > "$SCRATCH/out" 2>&1
}
calls() { cat "$CALLS" 2>/dev/null | grep -c "^\($1\)"; }
mongod_state() { kill -0 "$(cat "$CALLS.mongod-pid" 2>/dev/null)" 2>/dev/null && echo running || echo stopped; }

offline FRESHNESS_EXIT=3 scripts/identity-capture.sh --offline --recording "$RUN" --capture uk
check "an offline capture exits clean" "0" "$?"
check "...asks the vault for the TMDB key alone, never prod's Mongo URI" \
  "secrets run movies KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY=TMDB_API_KEY --" "$(grep '^secrets' "$CALLS" | sed 's/ -- .*/ --/')"
check "...opens no tunnel to prod and asks GitHub nothing" "no 0" "$([ -e "$TUNNEL_UP" ] && echo yes || echo no) $(calls gh)"
check "...checks the mirror's freshness for the country first, and warns that it is stale" "1 1" \
  "$(grep -c "^freshness .*CCS=\['uk'\]" "$CALLS") $(grep -c 'local prod mirror is stale' "$SCRATCH/out")"
check "...exports uk's family answers READ-ONLY from the local prod mirror" \
  "mongoexport --uri mongodb://127.0.0.1:28017/?directConnection=true&readPreference=secondaryPreferred --db kinowo_uk_prod_mirror --collection identity_family_answers --out $SCRATCH/work/families/kinowo_uk.jsonl" \
  "$(grep '^mongoexport' "$CALLS")"
check "...and seeds the capture from that export and the recording's clone" \
  "SEED=$SCRATCH/work/families CORPUS=$SCRATCH/work/recording-$RUN/corpus TREE=$SCRATCH/work/recording-$RUN" \
  "$(sed -n 's/^java-env //p' "$CALLS")"
check "...the capture's working Mongo is a mongod of its own, never :28017" "yes" \
  "$(grep -qE '^java MONGODB_URI=mongodb://127\.0\.0\.1:[0-9]+/\?directConnection=true FAMILY=<unset> ' "$CALLS" &&
     ! grep -q '^java MONGODB_URI=[^ ]*:28017' "$CALLS" && echo yes || echo no)"
check "...which is stopped when the run ends" "stopped" "$(mongod_state)"
check "...--offline and --recording are the script's alone, not the JVM's" "java-args --capture uk" "$(grep '^java-args' "$CALLS")"
check "...the cached recording is cloned, never written to" "$cached_before" "$(cache_sum)"
check "...and uk.inputs names the recording, not the corpus hash, for identity-slice" "recording	$RUN" "$(head -1 "$SCRATCH/fixtures/uk.inputs")"

offline scripts/identity-capture.sh --offline --recording "$RUN" --capture pl
check "an offline pl capture reads the shared kinowo database's mirror as kinowo.jsonl" "0 kinowo_prod_mirror $SCRATCH/work/families/kinowo.jsonl" \
  "$? $(sed -n 's/^mongoexport .*--db \([^ ]*\) .*--out \(.*\)$/\1 \2/p' "$CALLS")"

offline scripts/identity-capture.sh --offline --recording "$RUN" --fill uk
check "an offline fill exports no family answers (it reads none), starts no mongod, asks GitHub nothing" "0 0 0 0" \
  "$? $(calls mongoexport) $(calls mongod) $(calls gh)"
check "...but names the cached recording, so no newest one is looked up" \
  "SEED=<unset> CORPUS=$CACHE/recordings/$RUN/pair/test/resources/fixtures/corpus TREE=$CACHE/recordings/$RUN/pair/test/resources/fixtures" \
  "$(sed -n 's/^java-env //p' "$CALLS")"

offline scripts/identity-capture.sh --offline --capture uk
check "--offline without --recording: refused before anything runs" "2 0" "$? $(calls 'java\|mongod\|mongoexport\|secrets')"

offline scripts/identity-capture.sh --offline --recording 1 --capture uk
check "a recording not in the cache: refused, naming it, before anything runs" "2 1 0" \
  "$? $(grep -c 'recording 1 holds no uk' "$SCRATCH/out") $(calls 'java\|mongod\|mongoexport\|secrets\|gh')"

offline FRESHNESS_EXIT=2 scripts/identity-capture.sh --offline --recording "$RUN" --capture uk
check "no local prod mirror to read: refused, said, before any export or capture" "1 1 0" \
  "$? $(grep -c 'no local prod mirror' "$SCRATCH/out") $(calls 'mongoexport\|java\|mongod')"

spec_summary
