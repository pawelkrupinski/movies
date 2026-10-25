#!/usr/bin/env python3
"""THE MONGO ROLES EVERY DATABASE NEEDS, CHECKED OR GRANTED ON PROD (scripts/mongo-grants).

    mongo_grants.py --check | --apply

Run by scripts/mongo-grants inside `scripts/with-prod-mongo.sh --both`, so the environment holds the app's
MONGODB_URI and MONGO_ROOT_URI, both through one tunnel.

WHAT IS REQUIRED comes from scripts/mongo-grants.tsv (generated from models.Country.all and services.FleetDatabase by
deploy.MongoGrantsList; MongoGrantsListSpec keeps it current): rows of principal, role, database.

WHO EACH PRINCIPAL IS comes from its connection string, never from a name written here:
  app      the user and auth database of the app's MONGODB_URI (authSource, else the URI's database, else admin);
  ci-read  the same, of $MONGO_CI_READ_URI when set, else of the URI the CI tunnel builds
           (scripts/ci/mongo-tunnel-paths.sh: mongo_ci_read_uri) for the user the mongo-ci-read endpoint hands out
           (infra/nix/modules/roles/mongo-ci-read.nix, its `username` option's default).

--check prints each missing role and exits 1 if there is any. --apply grants exactly the missing roles, as root,
to the user's own auth database, then reads the user back: idempotent, a second run grants nothing. A missing USER is
never created; it is reported and the run fails. Every read and grant runs as MONGO_ROOT_URI; no password is printed.
"""
import json
import os
import re
import subprocess
import sys
import urllib.parse

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
MONGOSH_AT = os.path.join(HERE, "local-mirror", "mongosh-at.sh")

# A role that carries another on the same database: holding readWrite satisfies a required read.
IMPLIES = {"read": {"read", "readWrite", "dbOwner"}, "readWrite": {"readWrite", "dbOwner"}}


def die(message):
    print(f"[mongo-grants] {message}", file=sys.stderr)
    sys.exit(1)


def user_of(uri):
    """(user, auth database) of a mongodb:// URI, the auth database as the driver picks it."""
    parts = urllib.parse.urlsplit(uri)
    user = urllib.parse.unquote(parts.username or "")
    query = urllib.parse.parse_qs(parts.query)
    auth_db = query.get("authSource", [parts.path.strip("/") or "admin"])[0]
    return user, auth_db


def ci_read_username():
    path = os.path.join(REPO, "infra/nix/modules/roles/mongo-ci-read.nix")
    with open(path, encoding="utf-8") as f:
        text = f.read()
    m = re.search(r'username\s*=\s*lib\.mkOption\s*\{.*?default\s*=\s*"([^"]+)"', text, re.S)
    if not m:
        die(f"no default username in {path}; set MONGO_CI_READ_URI")
    return m.group(1)


def ci_read_uri():
    if os.environ.get("MONGO_CI_READ_URI"):
        return os.environ["MONGO_CI_READ_URI"]
    script = '. "$1/scripts/ci/mongo-tunnel-paths.sh"; mongo_ci_read_uri "$2:-" 0'
    return subprocess.run(["bash", "-c", script, "_", REPO, ci_read_username()],
                          check=True, capture_output=True, text=True).stdout.strip()


def required(path):
    """principal -> [{role, db}], in the file's order."""
    need = {}
    with open(path, encoding="utf-8") as f:
        for line in f:
            line = line.rstrip("\n")
            if not line or line.startswith("#"):
                continue
            who, role, db = line.split("\t")
            need.setdefault(who, []).append({"role": role, "db": db})
    return need


def mongosh(js, **env):
    """One mongosh as root; the arguments travel in G_* variables, never spliced into the script."""
    result = subprocess.run([MONGOSH_AT, os.environ["MONGO_ROOT_URI"], "--quiet", "--eval", js],
                            env={**os.environ, **{f"G_{k.upper()}": v for k, v in env.items()}},
                            capture_output=True, text=True)
    if result.returncode != 0:
        die(f"mongosh failed: {result.stderr.strip() or result.stdout.strip()}")
    return result.stdout.strip()


def held_roles(user, auth_db):
    out = mongosh("print(JSON.stringify(db.getSiblingDB(process.env.G_DB).getUser(process.env.G_USER)))",
                  user=user, db=auth_db)
    record = json.loads(out.splitlines()[-1]) if out else None
    return None if record is None else [{"role": r["role"], "db": r["db"]} for r in record.get("roles", [])]


def missing(need, held):
    return [r for r in need if not any(h["db"] == r["db"] and h["role"] in IMPLIES.get(r["role"], {r["role"]})
                                       for h in held)]


def main(argv):
    if len(argv) != 1 or argv[0] not in ("--check", "--apply"):
        print("usage: mongo_grants.py --check | --apply", file=sys.stderr)
        return 2
    apply = argv[0] == "--apply"
    for var in ("MONGODB_URI", "MONGO_ROOT_URI"):
        if not os.environ.get(var):
            die(f"{var} is not set: run this through scripts/mongo-grants")
    need = required(os.environ.get("MONGO_GRANTS_LIST", os.path.join(HERE, "mongo-grants.tsv")))
    uris = {"app": os.environ["MONGODB_URI"], "ci-read": ci_read_uri()}
    unknown = set(need) - set(uris)
    if unknown:
        die(f"no connection string for principal(s) {', '.join(sorted(unknown))}")

    failed = False
    for who, roles in need.items():
        user, auth_db = user_of(uris[who])
        if not user:
            die(f"no user name in the {who} connection string")
        name = f"{who} {user}@{auth_db}"
        held = held_roles(user, auth_db)
        if held is None:
            print(f"MISSING USER {name}: not created here; create it by hand (see docs/adding-a-country.md)")
            failed = True
            continue
        gap = missing(roles, held)
        if not gap:
            print(f"ok {name}: {len(roles)} roles")
            continue
        for r in gap:
            print(f"missing {name}: {r['role']} on {r['db']}")
        if not apply:
            failed = True
            continue
        mongosh("db.getSiblingDB(process.env.G_DB).grantRolesToUser(process.env.G_USER, JSON.parse(process.env.G_ROLES))",
                user=user, db=auth_db, roles=json.dumps(gap))
        still = missing(roles, held_roles(user, auth_db) or [])
        if still:
            print(f"STILL MISSING {name} after the grant: {json.dumps(still)}")
            failed = True
        else:
            print(f"granted {name}: {len(gap)} roles")
    if failed:
        print("[mongo-grants] missing access" + ("" if apply else ": run scripts/mongo-grants --apply"), file=sys.stderr)
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
