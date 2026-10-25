#!/usr/bin/env bash
# Assertions for scripts/mongo-grants. No vault, no ssh, no Mongo: `secrets`, `ssh`, `nc` and `mongosh` are fakes on
# PATH. The fake mongosh keeps each user's roles in a JSON file and records every getUser and grant, with the user
# it was connected as, so the specs see what the tool read, what it granted and to whom.
#
#   scripts/mongo-grants-test.sh
set -uo pipefail
export PROD_TUNNEL_PORT=27017
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-mongo-grants.XXXXXX")"
trap 'rm -rf "$SCRATCH"' EXIT
FAKE="$SCRATCH/bin"; mkdir -p "$FAKE"
export CALLS="$SCRATCH/calls" TUNNEL_UP="$SCRATCH/tunnel-up" USERS="$SCRATCH/users"
LIST="$REPO_ROOT/scripts/mongo-grants.tsv"

# `secrets run movies SPEC... -- CMD`: records the call, sets each asked-for key from FAKE_<KEY>, runs CMD.
cat > "$FAKE/secrets" <<'FAKE'
#!/usr/bin/env bash
echo "secrets $*" >> "$CALLS"
shift 2
while [ "$1" != "--" ]; do v="FAKE_$1"; export "$1=${!v}"; shift; done
shift
exec "$@"
FAKE
cat > "$FAKE/ssh" <<'FAKE'
#!/usr/bin/env bash
echo "ssh ${*: -1}" >> "$CALLS"; touch "$TUNNEL_UP"; exec sleep 30
FAKE
printf '#!/bin/sh\n[ -e "$TUNNEL_UP" ]\n' > "$FAKE/nc"
cat > "$FAKE/mongosh" <<'FAKE'
#!/usr/bin/env bash
[ -e "$TUNNEL_UP" ] || exit 1
as="$(python3 -I -c 'import sys,urllib.parse as u; print(u.unquote(u.urlsplit(sys.argv[1]).username or ""))' "$KINOWO_MONGOSH_URI")"
f="$USERS/$G_DB.$G_USER.json"
case "$*" in
  *ping*) exit 0 ;;
  *getUser*) echo "getUser $G_USER@$G_DB as $as" >> "$CALLS"; if [ -f "$f" ]; then cat "$f"; else echo null; fi ;;
  *grantRolesToUser*)
    echo "grant $G_USER@$G_DB $G_ROLES as $as" >> "$CALLS"
    python3 -I -c 'import json,sys; d=json.load(open(sys.argv[1])); d["roles"]+=json.loads(sys.argv[2]); json.dump(d,open(sys.argv[1],"w"))' "$f" "$G_ROLES" ;;
  *) echo "fake mongosh: unexpected $*" >&2; exit 3 ;;
esac
FAKE
chmod +x "$FAKE"/*

APP_URI='mongodb://kinowo_app_2:pw@10.20.0.13:27017/kinowo?authSource=kinowo'
ROOT_URI='mongodb://kinowo_root:rootpw@10.20.0.13:27017/admin?authSource=admin'

# user <db> <name> <principal> [skip-db...]: the user holding every role the list gives <principal>, but those on skip-dbs
user() {
  local db="$1" name="$2" who="$3"; shift 3
  python3 -I - "$LIST" "$who" "$@" > "$USERS/$db.$name.json" <<'PY'
import json, sys
path, who, skip = sys.argv[1], sys.argv[2], set(sys.argv[3:])
rows = [l.rstrip("\n").split("\t") for l in open(path) if l.strip() and not l.startswith("#")]
print(json.dumps({"roles": [{"role": r, "db": d} for w, r, d in rows if w == who and d not in skip]}))
PY
}
fresh() { rm -rf "$USERS" "$CALLS" "$TUNNEL_UP"; mkdir -p "$USERS"; }
grants() {
  env -u MONGODB_URI -u MONGO_ROOT_URI -u MONGO_CI_READ_URI -u KINOWO_MONGO_SSH \
    PATH="$FAKE:$PATH" FAKE_MONGODB_URI="${FAKE_MONGODB_URI:-$APP_URI}" FAKE_MONGO_ROOT_URI="$ROOT_URI" \
    "$REPO_ROOT/scripts/mongo-grants" "$@" 2>&1
}

echo "[spec] the list"
check "the app is asked for readWrite on the fleet's own database" "1" "$(grep -c $'^app\treadWrite\tkinowo_fleet$' "$LIST")"

echo "[spec] mongo-grants --check"
fresh; user kinowo kinowo_app_2 app; user admin kinowo-ci-corpus ci-read
out="$(grants --check)"; status=$?
check "everything granted: exits 0" "0" "$status"
check "…reads the app's user off MONGODB_URI and the CI reader off its URI, as root" \
  "getUser kinowo_app_2@kinowo as kinowo_root
getUser kinowo-ci-corpus@admin as kinowo_root" "$(grep '^getUser' "$CALLS")"
check "…one secrets run for both credentials, one tunnel" \
  "1 1" "$(grep -c '^secrets run movies MONGODB_URI MONGO_ROOT_URI ' "$CALLS") $(grep -c '^ssh' "$CALLS")"
check "…prints no password" "" "$(printf '%s' "$out" | grep -E 'rootpw|:pw@')"

fresh; user kinowo kinowo_app_2 app kinowo_fleet; user admin kinowo-ci-corpus ci-read kinowo_es
out="$(grants --check)"; status=$?
check "something missing: exits 1" "1" "$status"
check "…names each missing role, user and auth database" \
  "missing app kinowo_app_2@kinowo: readWrite on kinowo_fleet
missing ci-read kinowo-ci-corpus@admin: read on kinowo_es" "$(printf '%s\n' "$out" | grep '^missing')"
check "…and grants nothing" "" "$(grep '^grant' "$CALLS")"

echo "[spec] mongo-grants --apply"
out="$(grants --apply)"; status=$?
check "grants exactly the missing roles, to each user's own auth database, as root" \
  'grant kinowo_app_2@kinowo [{"role": "readWrite", "db": "kinowo_fleet"}] as kinowo_root
grant kinowo-ci-corpus@admin [{"role": "read", "db": "kinowo_es"}] as kinowo_root' "$(grep '^grant' "$CALLS")"
check "…and exits 0 once the users read back complete" "0" "$status"
rm -f "$CALLS"
out="$(grants --apply)"; status=$?
check "a second --apply grants nothing (idempotent)" "0 " "$status $(grep '^grant' "$CALLS")"
out="$(grants --check)"; status=$?
check "…and --check then passes" "0" "$status"

fresh; user appdb someone app kinowo_uk; user admin kinowo-ci-corpus ci-read
out="$(FAKE_MONGODB_URI='mongodb://someone:pw@10.20.0.13:27017/x?authSource=appdb' grants --apply)"; status=$?
check "a different app user in MONGODB_URI is the one granted, in its own authSource" \
  'grant someone@appdb [{"role": "readWrite", "db": "kinowo_uk"}] as kinowo_root' "$(grep '^grant' "$CALLS")"

fresh; user admin kinowo-ci-corpus ci-read
out="$(grants --apply)"; status=$?
check "a missing user is reported, never created, and fails the run" "1 MISSING USER app kinowo_app_2@kinowo" \
  "$status $(printf '%s\n' "$out" | grep -o '^MISSING USER app kinowo_app_2@kinowo')"
check "…with no grant attempted for it" "" "$(grep '^grant kinowo_app_2' "$CALLS")"

echo "[spec] usage"
grants >/dev/null; check "no mode: usage, exit 2" "2" "$?"
grants --check --apply >/dev/null; check "two modes: usage, exit 2" "2" "$?"
spec_summary
