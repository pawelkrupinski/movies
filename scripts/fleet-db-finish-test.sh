#!/usr/bin/env bash
# Assertions for scripts/fleet-db-finish. No vault, no ssh, no Mongo: `secrets`, `ssh`, `nc` and `mongosh` are fakes
# on PATH. The fake mongosh answers the counts from a JSON state file, records every drop with the user it ran as and
# removes the collection from the state, so the specs see what was dropped, from where, and as whom.
#
#   scripts/fleet-db-finish-test.sh
set -uo pipefail
export PROD_TUNNEL_PORT=27017
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

. "$REPO_ROOT/scripts/prod-ops-fixture.sh"
prod_ops_fixture_init fleet-db-finish
export STATE="$SCRATCH/state.json" APP_DENIED="$SCRATCH/app-denied"

# The fake mongosh answers the counts from a JSON state file, records every drop with the user it ran as and removes
# the collection from the state.
fake_mongosh <<'BODY'
case "$*" in
  *appReads*)
    echo "probe as $as" >> "$CALLS"
    if [ -e "$APP_DENIED" ]; then echo "MongoServerError: not authorized on kinowo_fleet" >&2; exit 1; fi
    echo '{"appReads":3}' ;;
  *countDocuments*)
    echo "counts as $as" >> "$CALLS"
    python3 -I -c 'import json,sys; s=json.load(open(sys.argv[1]))
C=["facebook_rescrapes","facebook_fetches","fleet_host_pace"]
print(json.dumps({"counts":{d:{c:s[d].get(c) for c in C} for d in s},"newest":{d:"2026-10-25" for d in s}}))' "$STATE" ;;
  *"drop()"*)
    echo "drop kinowo.$G_COLL as $as" >> "$CALLS"
    python3 -I -c 'import json,sys; s=json.load(open(sys.argv[1])); s["kinowo"].pop(sys.argv[2],None); json.dump(s,open(sys.argv[1],"w"))' "$STATE" "$G_COLL"
    echo "$G_COLL dropped from kinowo: true" ;;
  *) echo "fake mongosh: unexpected $*" >&2; exit 3 ;;
esac
BODY

seed() {  # seed <kinowo_fleet's facebook_fetches count, or null for absent>
  printf '{"kinowo":{"facebook_rescrapes":12,"facebook_fetches":900,"fleet_host_pace":4},"kinowo_fleet":{"facebook_rescrapes":14,"facebook_fetches":%s,"fleet_host_pace":4},"kinowo_uk":{}}' \
    "${1:-950}" > "$STATE"
  rm -f "$APP_DENIED"; prod_ops_fixture_fresh
}
finish() { prod_ops_run "$REPO_ROOT/scripts/fleet-db-finish" "$@"; }
drops() { grep '^drop' "$CALLS" 2>/dev/null; }

echo "[spec] counts (no flag)"
seed; out="$(finish)"; status=$?
check "read-only by default: exits 0 and drops nothing" "0 " "$status $(drops)"
check "…prints each collection's count in kinowo and kinowo_fleet" "facebook_fetches 900 950" \
  "$(printf '%s\n' "$out" | awk '$1 == "facebook_fetches" { print $1, $2, $3 }')"
check "…counts as root and probes kinowo_fleet as the app, over one vault run and one tunnel" \
  "counts as kinowo_root|probe as kinowo_app_2|1|1" \
  "$(grep -E '^(counts|probe)' "$CALLS" | paste -sd'|' -)|$(grep -c '^secrets run movies MONGODB_URI MONGO_ROOT_URI ' "$CALLS")|$(grep -c '^ssh' "$CALLS")"

echo "[spec] --drop"
seed; out="$(finish --drop fleet_host_pace facebook_rescrapes facebook_fetches)"; status=$?
check "the three exact names, any order: drops each from kinowo, as root" \
  "drop kinowo.facebook_rescrapes as kinowo_root
drop kinowo.facebook_fetches as kinowo_root
drop kinowo.fleet_host_pace as kinowo_root" "$(drops)"
check "…exits 0 and leaves kinowo_fleet and every other database alone" \
  '0 {"facebook_rescrapes": 14, "facebook_fetches": 950, "fleet_host_pace": 4} {}' \
  "$status $(python3 -I -c 'import json,sys; s=json.load(open(sys.argv[1])); print(json.dumps(s["kinowo_fleet"]), json.dumps(s["kinowo_uk"]))' "$STATE")"

for bad in "--drop" "--drop facebook_rescrapes facebook_fetches" "--drop facebook_* facebook_fetches fleet_host_pace" \
           "--drop facebook_rescrapes facebook_rescrapes fleet_host_pace" \
           "--drop facebook_rescrapes facebook_fetches fleet_host_pace users" "--drop-all"; do
  seed; read -r -a words <<< "$bad"
  finish "${words[@]}" >/dev/null; status=$?
  check "refused, exit 2, nothing asked of the vault or Mongo: $bad" "2 " "$status $(cat "$CALLS" 2>/dev/null)"
done

seed null; out="$(finish --drop facebook_rescrapes facebook_fetches fleet_host_pace)"; status=$?
check "kinowo_fleet lacking one of the three: refused, nothing dropped" "1 " "$status $(drops)"
check "…saying which" "REFUSE: kinowo_fleet lacks or holds nothing in facebook_fetches" "$(printf '%s\n' "$out" | grep '^REFUSE')"
seed 0; finish --drop facebook_rescrapes facebook_fetches fleet_host_pace >/dev/null; status=$?
check "kinowo_fleet holding none of one: refused, nothing dropped" "1 " "$status $(drops)"
seed; touch "$APP_DENIED"; out="$(finish --drop facebook_rescrapes facebook_fetches fleet_host_pace)"; status=$?
check "the app's user unable to read kinowo_fleet: refused, nothing dropped" "1 " "$status $(drops)"
spec_summary
