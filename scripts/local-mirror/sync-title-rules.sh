#!/usr/bin/env bash
#
# One-way sync of the admin-curated `titleRules` collection from PROD (kinowo,
# over the ssh tunnel prod-tunnel.sh opens to the Mongo host) into the local dev DB
# (kinowo_local on :28017). One-shot, on-demand — title rules change rarely.
#
# Why this is needed: `titleRules` is the live rule set TitleNormalizer runs,
# edited from the admin UI. reset-corpus.sh deliberately does NOT touch it, and
# nothing else seeds it — so a fresh `kinowo_local` has an EMPTY collection and
# the local stack falls back to the frozen TitleRuleDefaults, diverging from
# prod's normalisation. This pulls the live set across so local matches prod.
#
# Mechanism: `mongodump` the one collection from prod, guard on the document
# count (≥10 — same floor scripts.DumpTitleRules uses; a tiny read means the
# tunnel didn't reach the seeded prod db), then `mongorestore --drop` it into
# kinowo_local. --drop makes it a TRUE one-way mirror: a rule deleted in prod
# also disappears locally. The local web+worker watch kinowo_local's titleRules
# change stream, so re-running against a live local stack hot-swaps the rules.
# titleRules is tiny (~50 docs) so the plain mongodump cursor is fine over the
# tunnel (unlike the 1300-doc `movies` seed, which needs the zlib path in seed.js).
#
# Reads from .env.local (line-by-line, NOT sourced — the Mongo URIs contain
# `&`/`?`):
#   MONGODB_URI       prod tunnel = sync SOURCE (required)
#   KINOWO_MONGO_SSH  ssh target for the prod tunnel (optional; see prod-tunnel.sh)
#   MONGODB_DB        prod db name (default: kinowo)
#   LOCAL_MONGO_URI   sync TARGET (default: mongodb://127.0.0.1:${LOCAL_MIRROR_PORT:-28017}/?directConnection=true)
#   LOCAL_MONGO_DB    local db name (default: kinowo_local)   # matches reset-corpus.sh
#
# Usage:  scripts/local-mirror/sync-title-rules.sh [--dry-run]
#           --dry-run   dump + count only; don't drop/refill the local collection
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
# Shared plumbing for every prod-sourced local script: envval (the .env.local
# reader), the tunnel, and — where relevant — the local Mongo starter. Sourced
# HERE, above the first envval call, because it is what defines it. Sourcing is
# side-effect free by contract, so an early source starts nothing.
. "$HERE/prod-tunnel.sh"
. "$HERE/local-mongo.sh"

COLL="titleRules"
MIN_RULES=10
DRY=""
for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY=1 ;;
    *) echo "[title-rules] unknown argument: $arg (see the header for usage)" >&2; exit 1 ;;
  esac
done


SRC="$(envval MONGODB_URI)"
SRC_DB="$(envval MONGODB_DB)"; SRC_DB="${SRC_DB:-kinowo}"
DST="$(envval LOCAL_MONGO_URI)"; DST="${DST:-mongodb://127.0.0.1:${LOCAL_MIRROR_PORT:-28017}/?directConnection=true}"
DST_DB="$(envval LOCAL_MONGO_DB)"; DST_DB="${DST_DB:-kinowo_local}"
[ -n "$SRC" ] || { echo "[title-rules] set MONGODB_URI in .env.local (prod tunnel = sync source)" >&2; exit 1; }

# ── ensure the prod tunnel (source), starting our OWN ssh forward only when
# nothing already serves :27017 — never fighting a tunnel someone else owns
# (sbt run's, or a manual one). cleanup kills only ours. Shared with mirror.sh
# and sync-enrichment-cache.sh so prod moving hosts is one edit, not four.
TUNNEL_TAG="title-rules"
TUNNEL_PROBE_URI="$SRC"
PROD_TUNNEL_ENV_FILE="$ROOT/.env.local"
TMP=""
cleanup() {
  close_prod_tunnel
  [ -n "$TMP" ] && rm -rf "$TMP" || true
}
trap cleanup EXIT INT TERM


ensure_prod_tunnel  || { echo "[title-rules] prod Mongo unreachable — can you 'ssh' to the mongo host? (see prod-tunnel.sh)" >&2; exit 1; }
[ -n "$DRY" ] || ensure_local_mongo

# ── dump the one collection from prod ───────────────────────────────────────
TMP="$(mktemp -d)"
echo "[title-rules] dumping prod ${SRC_DB}.${COLL}…"
mongodump --uri="$SRC" --db="$SRC_DB" --collection="$COLL" --out="$TMP" --quiet

BSON="$TMP/$SRC_DB/$COLL.bson"
[ -f "$BSON" ] || { echo "[title-rules] no dump produced at $BSON — wrong db or empty collection?" >&2; exit 1; }

# Count docs straight from the dumped BSON (one JSON line per doc) — no JS, no
# extra round-trip. Guard against overwriting local with junk from a misrouted
# tunnel (same ≥${MIN_RULES} floor as scripts.DumpTitleRules).
N="$(bsondump --quiet "$BSON" 2>/dev/null | grep -c '^{' || true)"
echo "[title-rules] prod has ${N} title-rule records"
if [ "${N:-0}" -lt "$MIN_RULES" ]; then
  echo "[title-rules] only ${N} records (< ${MIN_RULES}) — refusing to overwrite local ${DST_DB}.${COLL}." >&2
  echo "[title-rules] Is MONGODB_URI / the prod tunnel pointed at the seeded prod kinowo db?" >&2
  exit 3
fi

if [ -n "$DRY" ]; then
  echo "[title-rules] --dry-run: would drop+refill ${DST_DB}.${COLL} on ${DST%%\?*} with ${N} records. Nothing changed."
  exit 0
fi

# ── restore into kinowo_local, dropping the local collection first (true mirror:
# rules deleted in prod also vanish locally). -d/-c renames the namespace across
# databases (kinowo.titleRules → kinowo_local.titleRules).
echo "[title-rules] restoring → ${DST_DB}.${COLL} (drop + refill)…"
mongorestore --uri="$DST" -d "$DST_DB" -c "$COLL" --drop --quiet "$BSON"

LOCAL_N="$(mongosh "$DST" --quiet --eval "print(db.getSiblingDB('$DST_DB').getCollection('$COLL').countDocuments())" 2>/dev/null | tail -1)"
echo "[title-rules] done — local ${DST_DB}.${COLL} now has ${LOCAL_N} records (synced from prod ${SRC_DB})"
