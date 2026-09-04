#!/usr/bin/env bash
#
# Full corpus reset. Wipes the scrape / enrichment / staging / read-model
# collections so the worker re-scrapes every cinema from scratch and re-projects
# the read model; serving is empty until that completes (a few minutes of sparse
# repertoire — acceptable per the "short downtime is fine" rule).
#
# Two targets:
#   (default)   PROD  — the `web-<cc>` + `worker-<cc>` Deployments on k3s, prod
#                       Mongo over the ssh tunnel scripts/local-mirror/prod-tunnel.sh
#                       opens to the Hetzner Mongo host. Both are scaled to ZERO
#                       first so the worker can't race a half-finished scrape into a
#                       collection we're dropping, and scaled back only after the
#                       wipe lands. Needs a kubeconfig for the cluster.
#
#                       This used to stop and start FLY machines named `kinowo` and
#                       `kinowo-worker`. Both tiers moved to k3s on 2026-08-29, which
#                       made that both useless and harmful: the running worker was
#                       never stopped (so it raced the wipe) and the restart STARTED
#                       the stopped Fly worker, giving the fleet two workers holding
#                       change streams against one database.
#   --local           — the native brew Mongo the local web+worker share (:28017,
#                       db `kinowo_local`; see scripts/local-mirror/start-local-mongo.sh).
#                       No cluster and no tunnel — stop your local `sbt web/run` +
#                       `sbt worker/run` yourself first so the worker doesn't
#                       re-scrape into the wipe.
#
# Drops: detailCache, freshness, movies, pending_movies, tasks, web_movies,
#        web_screenings — i.e. everything the scrape→enrich→project pipeline
#        rebuilds from scratch (`pending_movies` is the staging incubator added
#        alongside `movies`). --local ALSO drops scheduled_runs (see below).
#   NOT dropped — operational state, curated config, and accounts the worker does
#   not rebuild: uptimeBuckets, uptimeServiceTags (monitoring history), titleRules
#   (admin-curated — seed it locally from prod with
#   scripts/local-mirror/sync-title-rules.sh), users, userStates (accounts). Add
#   filmwebFallback /
#   filmwebFallbackMeta (per-cinema fallback prober state) and/or
#   normalizationReports (a backfill report) to COLLECTIONS below if you want a
#   fully-clean slate that re-derives those too.
#   scheduled_runs is kept in PROD (worker scheduling — dropping it re-fires every
#   sweep at once) but DROPPED in --local: the fixture worker's reapers are
#   once-daily and gate on it, so a leftover `scrape@<today>` record makes a
#   restarted local stack skip the scrape and never repopulate kinowo_local.
#
# PROD reads MONGODB_URI (prod tunnel) + MONGODB_DB from .env.local.
# LOCAL defaults to mongodb://127.0.0.1:${LOCAL_MIRROR_PORT:-28017}/?directConnection=true
#       and db `kinowo_local`; override with LOCAL_MONGO_URI / LOCAL_MONGO_DB.
#
# Usage:  scripts/reset-corpus.sh [--local] [--dry-run] [--yes]
#           --local     reset the local brew Mongo instead of prod
#           --dry-run   list which collections WOULD be dropped, change nothing
#           --yes       skip the confirmation prompt
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
# Shared plumbing for every prod-sourced local script: envval (the .env.local
# reader), the tunnel, and — where relevant — the local Mongo starter. Sourced
# HERE, above the first envval call, because it is what defines it. Sourcing is
# side-effect free by contract, so an early source starts nothing.
. "$HERE/local-mirror/prod-tunnel.sh"

# The country whose Deployments are scaled around the wipe. `MONGODB_DB` names the
# database (kinowo / kinowo_uk / …); the overlays are suffixed by country code.
KUBE_NAMESPACE="${KUBE_NAMESPACE:-kinowo}"
COUNTRY="${KINOWO_RESET_COUNTRY:-pl}"
COLLECTIONS=(detailCache freshness movies pending_movies tasks web_movies web_screenings)

MODE="prod"
ASSUME_YES=""
DRY=""
for arg in "$@"; do
  case "$arg" in
    --local)   MODE="local" ;;
    --dry-run) DRY=1 ;;
    --yes)     ASSUME_YES=1 ;;
    *) echo "[reset] unknown argument: $arg (see the header for usage)" >&2; exit 1 ;;
  esac
done

# LOCAL also drops scheduled_runs: the local fixture worker's scrape/enrich
# reapers run once-daily and gate on this collection, so a leftover
# `scrape@<today>` record makes a freshly-restarted local stack skip the scrape —
# leaving an empty kinowo_local that never repopulates. Dropping it forces the
# next boot to re-scrape from scratch. PROD deliberately keeps it: dropping it
# there would re-fire every scheduled sweep at once.
[ "$MODE" = "local" ] && COLLECTIONS+=(scheduled_runs)


if [ "$MODE" = "local" ]; then
  URI="${LOCAL_MONGO_URI:-mongodb://127.0.0.1:${LOCAL_MIRROR_PORT:-28017}/?directConnection=true}"
  DB="${LOCAL_MONGO_DB:-kinowo_local}"
else
  URI="$(envval MONGODB_URI)"
  DB="$(envval MONGODB_DB)"
  [ -n "$URI" ] || { echo "[reset] set MONGODB_URI in .env.local (prod tunnel)" >&2; exit 1; }
  [ -n "$DB" ]  || { echo "[reset] set MONGODB_DB in .env.local" >&2; exit 1; }
fi

echo "[reset] $MODE — target db '$DB'; ${DRY:+would }drop: ${COLLECTIONS[*]}"
if [ "$MODE" = "local" ]; then
  echo "[reset] LOCAL: stop your local web+worker (sbt) first so the worker can't re-scrape into the wipe."
  CONFIRM_WORD="wipe-local"
else
  echo "[reset] will scale down+up: deploy/web-$COUNTRY, deploy/worker-$COUNTRY (ns $KUBE_NAMESPACE)"
  CONFIRM_WORD="wipe"
fi
if [ -z "$DRY" ] && [ -z "$ASSUME_YES" ]; then
  read -r -p "[reset] This wipes the $MODE corpus. Type '$CONFIRM_WORD' to proceed: " ans
  [ "$ans" = "$CONFIRM_WORD" ] || { echo "[reset] aborted."; exit 1; }
fi

# --- scale a Deployment down / up (prod only) -------------------------------
# Fatal on failure, unlike most of this script's helpers: the point of scaling to
# zero is that nothing is writing while collections are dropped, so a scale that
# silently failed would leave the worker racing the wipe — the exact thing this
# guards against.
scale_deploy() {
  local name="$1" replicas="$2"
  echo "[reset] scaling $name to $replicas..."
  kubectl -n "$KUBE_NAMESPACE" scale "deploy/$name" --replicas="$replicas" \
    || { echo "[reset] could not scale $name — is your kubeconfig pointed at the cluster?" >&2; exit 1; }
  [ "$replicas" = "0" ] && kubectl -n "$KUBE_NAMESPACE" rollout status "deploy/$name" --timeout=120s
  return 0
}

# --- tunnel to prod Mongo (prod only), torn down on any exit ----------------
# The database moved to the Hetzner host mongo-1 on 2026-08-29, so the tunnel is
# an ssh forward. See scripts/local-mirror/prod-tunnel.sh, the single definition
# every prod-sourced local script shares.
TUNNEL_TAG="reset"
TUNNEL_PROBE_URI="$URI"
PROD_TUNNEL_ENV_FILE="$ROOT/.env.local"
cleanup() { close_prod_tunnel; }
trap cleanup EXIT

open_tunnel() {
  # Fatal, unlike the mirror daemon's retry: this script is about to DROP
  # collections, and doing that against the wrong (or no) database is not a
  # thing to keep retrying into.
  ensure_prod_tunnel || { echo "[reset] prod Mongo not reachable via the tunnel" >&2; exit 1; }
}

require_local_mongo() {
  mongosh "$URI" --quiet --eval 'db.runCommand({ping:1})' >/dev/null 2>&1 && return 0
  echo "[reset] local Mongo not reachable at $URI — start it with scripts/local-mirror/start-local-mongo.sh" >&2
  exit 1
}

# --- run ---------------------------------------------------------------------
if [ "$MODE" = "prod" ]; then
  [ -z "$DRY" ] && { scale_deploy "web-$COUNTRY" 0; scale_deploy "worker-$COUNTRY" 0; }
  open_tunnel               # needed to query, even for --dry-run
else
  require_local_mongo
fi

echo "[reset] ${DRY:+[dry-run] }collections in '$DB':"
# `getSiblingDB($DB)` targets the db explicitly — the --local URI carries no db,
# so a bare `db` would resolve to `test`.
COLLS_JS="$(printf '"%s",' "${COLLECTIONS[@]}")"
mongosh "$URI" --quiet --eval "
  var dry = ${DRY:-0} == 1;
  var target = db.getSiblingDB('$DB');
  var wanted = [${COLLS_JS%,}];
  var existing = target.getCollectionNames();
  wanted.forEach(function (c) {
    if (existing.indexOf(c) === -1) { print('  skip       ' + c + ' (absent)'); return; }
    if (!dry) target.getCollection(c).drop();
    print('  ' + (dry ? 'would drop ' : 'dropped   ') + c);
  });
"

if [ "$MODE" = "prod" ] && [ -z "$DRY" ]; then
  scale_deploy "worker-$COUNTRY" 1
  scale_deploy "web-$COUNTRY" 1
fi

# Local resets preserve the admin-curated titleRules (deliberately not in
# COLLECTIONS), but a fresh kinowo_local has none and falls back to the frozen
# TitleRuleDefaults — so re-pull prod's live set with the one-way sync (it opens
# its OWN read-only tunnel to prod; prod is never written). Non-fatal: a
# failed sync (offline / no ssh access) must not fail the corpus reset.
if [ "$MODE" = "local" ]; then
  if [ -n "$DRY" ]; then
    echo "[reset] [dry-run] would sync admin-curated titleRules prod → '$DB' (scripts/local-mirror/sync-title-rules.sh)"
  else
    echo "[reset] syncing admin-curated titleRules prod → '$DB'…"
    "$HERE/local-mirror/sync-title-rules.sh" \
      || echo "[reset] WARN: title-rules sync failed — run scripts/local-mirror/sync-title-rules.sh once prod is reachable."
  fi
fi

if [ -n "$DRY" ]; then
  echo "[reset] dry-run only — nothing changed."
else
  echo "[reset] done. Worker will re-scrape + re-project; serving is sparse until it catches up."
fi
