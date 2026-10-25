#!/usr/bin/env bash
# The fake prod the prod-ops scripts' tests share (see scripts/prod-ops.sh). No vault, no ssh, no Mongo: `secrets`,
# `ssh`, `nc` and `mongosh` are fakes on PATH. Source it after scripts/shell-spec.sh:
#
#   . "$REPO_ROOT/scripts/prod-ops-fixture.sh"
#   prod_ops_fixture_init <tag>      SCRATCH (removed on exit), FAKE (the fake bin), CALLS (what the fakes were asked)
#   fake_mongosh <<'BODY' ... BODY   what the fake mongosh does for one --eval: a `case "$*" in` over the script text,
#                                    with $as (the user it connected as, read off the URI it was given) and the test's
#                                    own variables (G_*, files under SCRATCH). `*ping*` is answered already; the
#                                    tunnel must be up (the fake ssh brings it up) or it fails as a dead tunnel does.
#   prod_ops_fixture_fresh           forget CALLS and the tunnel: the next run starts cold
#   prod_ops_run <command...>        run it with the fakes first on PATH and no real URI or ssh override in the
#                                    environment. `secrets run movies KEY... -- CMD` records itself (one Touch ID =
#                                    one line) and sets each KEY from FAKE_<KEY>: FAKE_MONGODB_URI (default the app
#                                    kinowo_app_2@kinowo) and FAKE_MONGO_ROOT_URI (kinowo_root@admin) unless a test sets
#                                    them. Output is stdout+stderr together.
# CALLS lines: "secrets <args>", "ssh <target>" (one per tunnel opened), and whatever the mongosh body records.

export PROD_TUNNEL_PORT=27017 # the URIs the specs assert name the pinned port; the free-port pick has its own spec

prod_ops_fixture_init() {
  SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-$1.XXXXXX")"
  trap 'rm -rf "$SCRATCH"' EXIT
  FAKE="$SCRATCH/bin"; mkdir -p "$FAKE"
  export CALLS="$SCRATCH/calls" TUNNEL_UP="$SCRATCH/tunnel-up"
  cat > "$FAKE/secrets" <<'FAKE_SECRETS'
#!/usr/bin/env bash
echo "secrets $*" >> "$CALLS"
shift 2
while [ "$1" != "--" ]; do v="FAKE_$1"; export "$1=${!v}"; shift; done
shift
exec "$@"
FAKE_SECRETS
  cat > "$FAKE/ssh" <<'FAKE_SSH'
#!/usr/bin/env bash
echo "ssh ${*: -1}" >> "$CALLS"; touch "$TUNNEL_UP"; exec sleep 30
FAKE_SSH
  printf '#!/bin/sh\n[ -e "$TUNNEL_UP" ]\n' > "$FAKE/nc"
  cat > "$FAKE/mongosh" <<'FAKE_MONGOSH'
#!/usr/bin/env bash
[ -e "$TUNNEL_UP" ] || exit 1
as="$(python3 -I -c 'import sys,urllib.parse as u; print(u.unquote(u.urlsplit(sys.argv[1]).username or ""))' "$KINOWO_MONGOSH_URI")"
case "$*" in *ping*) exit 0 ;; esac
. "$(dirname "$0")/mongosh-body"
FAKE_MONGOSH
  chmod +x "$FAKE"/*
  fake_mongosh <<'BODY'
echo "fake mongosh: no body set: $*" >&2; exit 3
BODY
}

fake_mongosh() { cat > "$FAKE/mongosh-body"; }

prod_ops_fixture_fresh() { rm -f "$CALLS" "$TUNNEL_UP"; }

prod_ops_run() {
  env -u MONGODB_URI -u MONGO_ROOT_URI -u MONGO_CI_READ_URI -u KINOWO_MONGO_SSH -u PROD_OPS_TUNNELLED PATH="$FAKE:$PATH" \
    FAKE_MONGODB_URI="${FAKE_MONGODB_URI:-mongodb://kinowo_app_2:pw@10.20.0.13:27017/kinowo?authSource=kinowo}" \
    FAKE_MONGO_ROOT_URI="${FAKE_MONGO_ROOT_URI:-mongodb://kinowo_root:rootpw@10.20.0.13:27017/admin?authSource=admin}" \
    "$@" 2>&1
}
