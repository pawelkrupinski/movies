#!/usr/bin/env bash
#
# Re-capture the unmatched-cluster fixture (test/resources/fixtures/identity-unmatched/<cc>.json.gz)
# in one command — docs/design/identity-resolver.md §20.14 item 5.
#
#   scripts/identity-capture.sh [--dry-run] [--capture|--fill] [--parallel 2] [--heap 12g] [cc...]
#   scripts/identity-capture.sh --offline --recording <run> [--capture|--fill] [cc...]
#
#   cc...       countries to capture (default: us de uk pl es, run largest first)
#   --capture   capture every country named, whatever its fixture's inputs
#   --fill      fill every country named (its <cc>.json.gz must exist)
#   --parallel  countries run side by side, one JVM each (default 2; fewer when memory holds fewer
#               12g heaps beside 4 GB for the rest), sharing 4 live reads per host between them
#   --heap      each country's JVM heap (default 12g)
#   --dry-run   print the plan — what it would download, export and run, with every variable — and
#               touch neither prod nor the network
#   --offline   touch neither GitHub nor prod, and write nothing to the shared :28017 (OFFLINE below);
#               needs --recording
#   --recording the "Record scrape fixtures" run to capture from, already in the identity cache
#               (~/.cache/kinowo-identity/recordings/<run>; scripts/identity-fetch-recording.sh fetches one)
#
# CAPTURE OR FILL, per country, said and logged: a FILL (integration.UnmatchedClustersFill, minutes)
# answers only the questions a change newly asks of the fixture's clusters; a CAPTURE resolves the
# whole corpus again. It fills when <cc>.json.gz exists and its decisions are unchanged under the
# current code — <cc>.inputs, stamped by each capture, names the same recording and the same hash of
# the code deciding them (scripts.IdentityCapture.DecisionInputs) — and captures otherwise.
#
# What a capture does (scripts.IdentityCapture, worker/src/test/scala/scripts, does the work):
#   1. finds the newest successful "Record scrape fixtures" run (identity_newest_recording, passed on as
#      $KINOWO_IDENTITY_NEWEST_RECORDING) and fetches its corpora (artifact
#      scrape-fixtures-<cc>) and the enrichment trees recorded with them (release convergence-fixtures,
#      enrichment-<cc>-<run>.tar.*) into $KINOWO_IDENTITY_CAPTURE_WORK (default target/identity-capture),
#      skipping a country whose download is already that run's;
#   2. exports prod's identity_family_answers (kinowo, kinowo_<cc>) READ-ONLY over the prod tunnel
#      (scripts/local-mirror/prod-tunnel.sh; the URI is the Touch ID vault's MONGODB_URI, unless
#      exported, rewritten to the tunnel by prod_tunnel_uri and never printed) — or over
#      $KINOWO_IDENTITY_FAMILY_URI where the caller (CI) already holds a read-only route;
#   3. runs each country's integration.UnmatchedClustersCapture in a JVM of its own, against the
#      it/ Mongo (127.0.0.1:28017) in a database of its own, every directory defaulted (a variable you
#      set is kept), the TMDB key read from the Touch ID vault (TMDB_API_KEY) unless
#      KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY is set — both secrets in ONE `secrets run` (one Touch ID):
#      the script re-runs itself under it before compiling, naming only the keys still missing; a country whose capture read venue pages live
#      ("capture again") is captured again to answer them, while each capture reads more (at most 4 captures);
#   4. prints each phase's time and throughput, and exits non-zero, naming them, when any country's JVM
#      failed, was killed or printed no capture.
#
# OFFLINE (--offline --recording <run>), what 0a6079eb3's UK re-capture did by hand while GitHub was suspended:
#   - checks the local prod mirror's freshness (scripts/local-mirror/freshness <cc>...), warning when it is stale and
#     failing when there is no mirror to read, then exports each capture's identity_family_answers READ-ONLY from it
#     (mongoexport, secondaryPreferred: database kinowo[_<cc>]_prod_mirror into <work>/families/kinowo[_<cc>].jsonl)
#     and hands them over as KINOWO_IDENTITY_FAMILY_SEED — no tunnel, and no prod URI asked of the vault;
#   - APFS-clones (cp -c; a reflink copy elsewhere) the cached recording's corpus/ and enrichment-<cc>/ into
#     <work>/recording-<run> as KINOWO_IDENTITY_CORPUS_DIR and KINOWO_FIXTURE_ROOT, so no newest recording is looked
#     up and the cache is never written (a capture adds the venue pages it read to its tree); a --fill reads neither
#     and names the cache in place;
#   - runs the captures on a mongod of their own on a free port (scripts/own-mongod.sh), stopped and removed on exit;
#   - stamps <cc>.inputs `recording <run>` where the capture, handed a corpus of the caller's own, wrote
#     `corpus <hash>`: identity-slice reads the recording from it.
# The TMDB key still comes from the vault. Without --fill an offline run captures: the stamp it compares with names a
# recording, its own corpus a hash.
#
# Then judge every new take into identity-judgements.tsv and re-baseline expected-matches.tsv (the ratchet,
# worker/src/test/scala/services/identity/UnmatchedClustersRatchetSpec.scala).
set -euo pipefail
self="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"
# shellcheck source=scripts/identity-tools.sh
. "$(dirname "$self")/identity-tools.sh" # identity_classpath_line; tool-conventions.sh's usage_from_header
cd "$(dirname "$0")/.."

original_args=("$@")
dry=false
fill_only=false
offline=false
recording=""
java_args=() ccs=()
while [ "$#" -gt 0 ]; do
    case "$1" in
        --offline) offline=true; shift; continue ;;
        --recording) recording=${2:-}; shift; [ "$#" -gt 0 ] && shift; continue ;;
        --heap|--parallel) java_args+=("$1"); shift; [ "$#" -gt 0 ] || continue ;;
        --dry-run) dry=true ;;
        --fill) fill_only=true ;;
        -h|--help) usage_from_header "$self"; exit 0 ;;
        -*) ;;
        *) ccs+=("$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')") ;;
    esac
    java_args+=("$1"); shift
done
[ "${#ccs[@]}" -gt 0 ] || ccs=("${KINOWO_COUNTRIES[@]}")

# Offline: refused before the vault or a compile when the recording it reads is not in the cache.
recorded=""
if $offline; then
    [ -n "$recording" ] || { echo "[identity-capture] --offline needs --recording <run>, a recording in $IDENTITY_CACHE/recordings" >&2; exit 2; }
    for cc in "${ccs[@]}"; do recorded=$(identity_recording_cached "$recording" "$cc") || exit 2; done
elif [ -n "$recording" ]; then
    echo "[identity-capture] --recording is for --offline: online, the newest recording is fetched" >&2; exit 2
fi

# The secrets, up front and together, so a missing one costs one Touch ID before the compile rather
# than a failure after it. A fill reads no family answers from prod, and a caller-supplied route or
# seed replaces the prod read.
# shellcheck source=scripts/local-mirror/prod-tunnel.sh
. scripts/local-mirror/prod-tunnel.sh
reads_prod=false
if ! $dry && ! $fill_only && ! $offline && [ -z "${KINOWO_IDENTITY_FAMILY_URI:-}" ] && [ -z "${KINOWO_IDENTITY_FAMILY_SEED:-}" ]; then
    reads_prod=true
fi
if ! $dry; then
    needed=(KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY=TMDB_API_KEY)
    # Under its own name, NOT MONGODB_URI: the capture JVMs inherit this environment, and a
    # MONGODB_URI there replaces each one's local throwaway database (IdentityCapture.environment).
    $reads_prod && needed+=(KINOWO_PROD_MONGODB_URI=MONGODB_URI)
    ensure_vault_env "${needed[@]}" -- "$BASH" "$self" "${original_args[@]}" || exit 2
fi

work=${KINOWO_IDENTITY_CAPTURE_WORK:-target/identity-capture}
mkdir -p "$work"
work=$(cd "$work" && pwd)

# The newest recording is found HERE, by the shared helper (identity-tools.sh: bounded, newest by creation time), and
# handed to scripts.IdentityCapture. A lookup that fails leaves it unset: the JVM then keeps the downloads it has and
# fails a country with none, saying the newest was not looked up.
if ! $dry && ! $offline; then
    if KINOWO_IDENTITY_NEWEST_RECORDING=$(identity_newest_recording); then export KINOWO_IDENTITY_NEWEST_RECORDING
    else unset KINOWO_IDENTITY_NEWEST_RECORDING; echo "[identity-capture] the newest recording could not be looked up (above)" >&2; fi
fi

# The integration classpath, compiled once: every country's JVM runs on it.
echo "[identity-capture] compiling the integration classpath"
# sbt's whole output goes to a log, and only a line that IS a classpath is kept: on 2026-10-12 the
# first scheduled run handed `java -cp` whatever sbt printed last and died "Could not find or load
# main class scripts.IdentityCapture" with the reason swallowed.
if ! sbt -batch "worker/IntegrationTest/compile" "export worker/IntegrationTest/fullClasspath" > "$work/sbt.log" 2>&1; then
    cat "$work/sbt.log" >&2
    echo "[identity-capture] sbt failed to compile the integration classpath (log above)" >&2
    exit 1
fi
identity_classpath_line "$work/sbt.log" > "$work/classpath" || true
classes=$(tr ':' '\n' < "$work/classpath" | grep -E '/worker/target/[^/]+/test-classes$' || true)
if [ -z "$classes" ] || [ ! -f "$classes/scripts/IdentityCapture.class" ]; then
    cat "$work/sbt.log" >&2
    echo "[identity-capture] no classpath holding scripts.IdentityCapture in sbt's output (log above)" >&2
    exit 1
fi

if ! $dry; then
    export KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY
fi
# The tunnel is for a capture's export only.
if $reads_prod; then
    init_prod_tunnel identity-capture "$(prod_tunnel_uri "$KINOWO_PROD_MONGODB_URI")"
    unset KINOWO_PROD_MONGODB_URI
    trap close_prod_tunnel EXIT INT TERM
    ensure_prod_tunnel || { echo "[identity-capture] prod Mongo unreachable — can you 'ssh' to the mongo host? (see prod-tunnel.sh)" >&2; exit 1; }
    KINOWO_IDENTITY_FAMILY_URI=$TUNNEL_PROBE_URI
    export KINOWO_IDENTITY_FAMILY_URI
fi

# Offline: the family answers from the local prod mirror, the recording cloned from the cache, a mongod of its own.
if $offline; then
    rec="$work/recording-$recording"
    if $fill_only; then
        # a fill reads neither, but naming both keeps the JVM from wanting a newest recording
        KINOWO_IDENTITY_CORPUS_DIR="$recorded/corpus" KINOWO_FIXTURE_ROOT="$recorded"
    elif $dry; then
        echo "[identity-capture] would check the local prod mirror's freshness, export its family answers into" \
            "$work/families, clone recording $recording into $rec and start a mongod of the run's own"
        KINOWO_IDENTITY_FAMILY_SEED="$work/families" KINOWO_IDENTITY_CORPUS_DIR="$rec/corpus" KINOWO_FIXTURE_ROOT="$rec"
    else
        fresh=0
        scripts/local-mirror/freshness "${ccs[@]}" --quiet || fresh=$?
        case "$fresh" in
            0) ;;
            3) echo "[identity-capture] WARNING: the local prod mirror is stale (above): the family answers exported from it lag prod's" >&2 ;;
            *) echo "[identity-capture] no local prod mirror to export the family answers from (scripts/local-mirror/freshness ${ccs[*]})" >&2; exit 1 ;;
        esac
        mkdir -p "$work/families"
        for db in $(for cc in "${ccs[@]}"; do if [ "$cc" = pl ]; then echo kinowo; else echo "kinowo_$cc"; fi; done | sort -u); do
            echo "[identity-capture] exporting ${db}_prod_mirror.identity_family_answers read-only from the local prod mirror"
            mongoexport --uri "mongodb://127.0.0.1:$(local_mongo_port)/?directConnection=true&readPreference=secondaryPreferred" \
                --db "${db}_prod_mirror" --collection identity_family_answers --out "$work/families/$db.jsonl" ||
                { rm -f "$work/families/$db.jsonl"; echo "[identity-capture] mongoexport of ${db}_prod_mirror failed" >&2; exit 1; }
        done
        # A clone, so the venue pages a capture adds to its tree never reach the shared cache.
        clone() { if [ "$(uname)" = Darwin ]; then cp -cR "$1" "$2"; else cp -R --reflink=auto "$1" "$2"; fi; }
        echo "[identity-capture] cloning recording $recording's corpus and tree into $rec"
        rm -rf "$rec"; mkdir -p "$rec"
        clone "$recorded/corpus" "$rec/corpus"
        for cc in "${ccs[@]}"; do clone "$recorded/enrichment-$cc" "$rec/enrichment-$cc"; done
        # shellcheck source=scripts/own-mongod.sh
        . scripts/own-mongod.sh
        trap own_mongod_stop EXIT INT TERM
        own_mongod_start identity-capture || exit 1
        KINOWO_IDENTITY_FAMILY_SEED="$work/families" KINOWO_IDENTITY_CORPUS_DIR="$rec/corpus" KINOWO_FIXTURE_ROOT="$rec"
        # the capture JVMs' throwaway database: this run's own server (IdentityCapture.environment)
        MONGODB_URI=$OWN_MONGOD_URI
        export MONGODB_URI
    fi
    export KINOWO_IDENTITY_CORPUS_DIR KINOWO_FIXTURE_ROOT
    if [ -n "${KINOWO_IDENTITY_FAMILY_SEED:-}" ]; then export KINOWO_IDENTITY_FAMILY_SEED; fi
fi

status=0
java -Xmx2g -Djava.awt.headless=true -cp "$(cat "$work/classpath")" scripts.IdentityCapture ${java_args[@]+"${java_args[@]}"} || status=$?

# Handed a corpus of its own, a capture stamps <cc>.inputs with the corpus's hash; identity-slice reads the recording.
if $offline && ! $dry; then
    fixtures=${KINOWO_IDENTITY_UNMATCHED_CAPTURE:-test/resources/fixtures/identity-unmatched}
    for cc in "${ccs[@]}"; do
        inputs="$fixtures/$cc.inputs"
        [ -f "$inputs" ] || continue
        hash=$(python3 -I -c 'import hashlib,sys; print(hashlib.sha256(open(sys.argv[1],"rb").read()).hexdigest()[:12])' \
            "$KINOWO_IDENTITY_CORPUS_DIR/cinema-scrapes-$cc.json.gz")
        if [ "$(head -1 "$inputs")" = "$(printf 'recording\tcorpus %s' "$hash")" ]; then
            { printf 'recording\t%s\n' "$recording"; tail -n +2 "$inputs"; } > "$inputs.part" && mv "$inputs.part" "$inputs"
            echo "[identity-capture] $cc: stamped $inputs with recording $recording"
        fi
    done
fi
exit "$status"
