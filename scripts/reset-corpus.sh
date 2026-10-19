#!/usr/bin/env bash
#
# Full corpus reset. Wipes the scrape / enrichment / staging / read-model
# collections so the worker re-scrapes every cinema from scratch and re-projects
# the read model; serving is empty until that completes (a few minutes of sparse
# repertoire — acceptable per the "short downtime is fine" rule).
#
# Target: PROD — the `web-<cc>` + `worker-<cc>` Deployments on k3s, prod Mongo over
# the ssh tunnel scripts/local-mirror/prod-tunnel.sh opens to the Hetzner Mongo host.
# Both are scaled to ZERO first so the worker can't race a half-finished scrape into a
# collection we're dropping, and scaled back only after the wipe lands. Needs a
# kubeconfig for the cluster.
#
# This used to stop and start FLY machines named `kinowo` and `kinowo-worker`. Both
# tiers moved to k3s on 2026-08-29, which made that both useless and harmful: the
# running worker was never stopped (so it raced the wipe) and the restart STARTED the
# stopped Fly worker, giving the fleet two workers holding change streams against one
# database.
#
# Drops: detailCache, freshness, movies, tasks, web_movies, web_screenings and
#        the identity model's derived state (identity_model_families,
#        identity_model_meta, identity_traces, identity_slot_fingerprints) — i.e.
#        everything the scrape→identity projection pipeline rebuilds from scratch.
#   NOT dropped — operational state, curated config, and accounts the worker does
#   not rebuild: uptimeBuckets, uptimeServiceTags (monitoring history), users,
#   userStates (accounts). Add filmwebFallback / filmwebFallbackMeta (per-cinema
#   fallback prober state) to COLLECTIONS below if you want a fully-clean slate
#   that re-derives those too. (Title rules are code, not a collection: see
#   common/src/main/scala/services/titlerules/.)
#   scheduled_runs is kept too (worker scheduling — dropping it re-fires every
#   sweep at once).
#
# Reads MONGODB_URI (prod tunnel) from the environment, else from the Touch ID vault
# (the script re-runs itself under `secrets run movies MONGODB_URI`), and MONGODB_DB
# from the environment (default `kinowo`, Poland's database).
#
# Usage:  scripts/reset-corpus.sh [--dry-run] [--yes]
#           --dry-run   list which collections WOULD be dropped, change nothing
#           --yes       skip the confirmation prompt
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Shared plumbing for every prod-sourced local script: ensure_vault_env (the
# environment-else-vault secret lookup) and the tunnel. Sourcing is side-effect
# free by contract, so an early source starts nothing.
. "$HERE/local-mirror/prod-tunnel.sh"

# The country whose Deployments are scaled around the wipe. `MONGODB_DB` names the
# database (kinowo / kinowo_uk / …); the overlays are suffixed by country code.
KUBE_NAMESPACE="${KUBE_NAMESPACE:-kinowo}"
COUNTRY="${KINOWO_RESET_COUNTRY:-pl}"
COLLECTIONS=(detailCache freshness movies tasks web_movies web_screenings
             identity_model_families identity_model_meta identity_traces identity_slot_fingerprints)

ASSUME_YES=""
DRY=""
for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY=1 ;;
    --yes)     ASSUME_YES=1 ;;
    *) echo "[reset] unknown argument: $arg (see the header for usage)" >&2; exit 1 ;;
  esac
done

ensure_vault_env MONGODB_URI -- "$BASH" "$0" "$@" || exit 1
URI="$(prod_tunnel_uri "$MONGODB_URI")"
# The database is the COUNTRY's (models.Country.mongoDb): Poland's is `kinowo`, every other
# `kinowo_<cc>`. It must be the one whose web/worker get scaled to zero below, so an exported
# MONGODB_DB naming another country's is refused rather than wiped while that country serves.
if [ "$COUNTRY" = pl ]; then country_db=kinowo; else country_db="kinowo_$COUNTRY"; fi
if [ -n "${MONGODB_DB:-}" ] && [ "$MONGODB_DB" != "$country_db" ]; then
  echo "[reset] MONGODB_DB=$MONGODB_DB is not $COUNTRY's database ($country_db) — refusing" >&2
  exit 1
fi
DB="$country_db"

echo "[reset] prod — target db '$DB'; ${DRY:+would }drop: ${COLLECTIONS[*]}"
echo "[reset] will scale down+up: deploy/web-$COUNTRY, deploy/worker-$COUNTRY (ns $KUBE_NAMESPACE)"
if [ -z "$DRY" ] && [ -z "$ASSUME_YES" ]; then
  read -r -p "[reset] This wipes the prod corpus. Type 'wipe' to proceed: " ans
  [ "$ans" = "wipe" ] || { echo "[reset] aborted."; exit 1; }
fi

# --- scale a Deployment down / up -------------------------------------------
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

# --- tunnel to prod Mongo, torn down on any exit ----------------------------
# The database moved to the Hetzner host mongo-1 on 2026-08-29, so the tunnel is
# an ssh forward. See scripts/local-mirror/prod-tunnel.sh, the single definition
# every prod-sourced local script shares.
init_prod_tunnel "reset" "$URI"
# Set from the first scale-down until the scale-up: any exit in between (a failed drop, a rollout timeout) brings both
# back, rather than leaving the country's web tier at zero.
SCALED_DOWN=""
cleanup() {
  if [ -n "$SCALED_DOWN" ]; then
    echo "[reset] the reset did not finish — scaling worker-$COUNTRY and web-$COUNTRY back up" >&2
    kubectl -n "$KUBE_NAMESPACE" scale "deploy/worker-$COUNTRY" --replicas=1 || true
    kubectl -n "$KUBE_NAMESPACE" scale "deploy/web-$COUNTRY" --replicas=1 || true
  fi
  close_prod_tunnel
}
trap cleanup EXIT

open_tunnel() {
  # Fatal, unlike the mirror daemon's retry: this script is about to DROP
  # collections, and doing that against the wrong (or no) database is not a
  # thing to keep retrying into.
  ensure_prod_tunnel || { echo "[reset] prod Mongo not reachable via the tunnel" >&2; exit 1; }
}

# --- run ---------------------------------------------------------------------
# The tunnel FIRST: scaled to zero and then finding prod unreachable, the script exited with the web tier down.
open_tunnel               # needed to query, even for --dry-run
if [ -z "$DRY" ]; then SCALED_DOWN=1; scale_deploy "web-$COUNTRY" 0; scale_deploy "worker-$COUNTRY" 0; fi

echo "[reset] ${DRY:+[dry-run] }collections in '$DB':"
# `getSiblingDB($DB)` targets the db explicitly — the tunnel URI may carry no db,
# so a bare `db` would resolve to `test`.
COLLS_JS="$(printf '"%s",' "${COLLECTIONS[@]}")"
mongosh_at "$URI" --quiet --eval "
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

if [ -z "$DRY" ]; then
  SCALED_DOWN=""
  scale_deploy "worker-$COUNTRY" 1
  scale_deploy "web-$COUNTRY" 1
fi

if [ -n "$DRY" ]; then
  echo "[reset] dry-run only — nothing changed."
else
  echo "[reset] done. Worker will re-scrape + re-project; serving is sparse until it catches up."
fi
