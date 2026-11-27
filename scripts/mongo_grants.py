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
           (scripts/ci/mongo_tunnel.py: ci_read_uri) for the user the mongo-ci-read endpoint hands out
           (infra/nix/modules/roles/mongo-ci-read.nix, its `username` option's default).

--check prints each missing role and exits 1 if there is any. --apply grants exactly the missing roles, as root,
to the user's own auth database, then reads the user back: idempotent, a second run grants nothing. A missing USER is
never created; it is reported and the run fails. Every read and grant runs as MONGO_ROOT_URI; no password is printed.
"""
from __future__ import annotations

import json
import os
import re
import sys
import urllib.parse
from typing import Dict, List, Mapping, NoReturn, Optional, Sequence, Set

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
sys.path.insert(0, os.path.join(HERE, "ci"))
sys.path.insert(0, os.path.join(HERE, "lib"))
import machine  # noqa: E402
import tool_process  # noqa: E402
from mongo_tunnel import ci_read_uri as tunnel_uri  # noqa: E402
from tool_files import Files  # noqa: E402

MONGOSH_AT = os.path.join(HERE, "local-mirror", "mongosh-at.sh")
CI_READ_NIX = "infra/nix/modules/roles/mongo-ci-read.nix"

# A role that carries another on the same database: holding readWrite satisfies a required read.
IMPLIES: Dict[str, Set[str]] = {"read": {"read", "readWrite", "dbOwner"}, "readWrite": {"readWrite", "dbOwner"}}

Role = Dict[str, str]


def die(message: str) -> NoReturn:
    print(f"[mongo-grants] {message}", file=sys.stderr)
    sys.exit(1)


def user_of(uri: str) -> "tuple[str, str]":
    """(user, auth database) of a mongodb:// URI, the auth database as the driver picks it."""
    parts = urllib.parse.urlsplit(uri)
    user = urllib.parse.unquote(parts.username or "")
    query = urllib.parse.parse_qs(parts.query)
    auth_db = query.get("authSource", [parts.path.strip("/") or "admin"])[0]
    return user, auth_db


def default_username(nix_text: str) -> Optional[str]:
    m = re.search(r'username\s*=\s*lib\.mkOption\s*\{.*?default\s*=\s*"([^"]+)"', nix_text, re.S)
    return m.group(1) if m else None


def required(text: str) -> Dict[str, List[Role]]:
    """principal -> [{role, db}], in the grants list's order."""
    need: Dict[str, List[Role]] = {}
    for line in text.splitlines():
        if not line or line.startswith("#"):
            continue
        who, role, db = line.split("\t")
        need.setdefault(who, []).append({"role": role, "db": db})
    return need


def missing(need: Sequence[Role], held: Sequence[Role]) -> List[Role]:
    return [r for r in need if not any(h["db"] == r["db"] and h["role"] in IMPLIES.get(r["role"], {r["role"]})
                                       for h in held)]


def with_arguments(env: Mapping[str, str], args: Mapping[str, str]) -> Dict[str, str]:
    return {**env, **{f"G_{k.upper()}": v for k, v in args.items()}}


def said(result: tool_process.Captured) -> str:
    return result.stderr.strip() or result.stdout.strip()


class Mongo:
    """The prod mongo as root, through mongosh-at.sh: its roles read and granted."""

    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str]) -> None:
        self.runner, self.env = runner, env

    def mongosh(self, js: str, **args: str) -> str:
        """One mongosh as root; the arguments travel in G_* variables, never spliced into the script."""
        result = self.runner.capture(tool_process.Command([MONGOSH_AT, self.env["MONGO_ROOT_URI"], "--quiet", "--eval", js],
                                                          with_arguments(self.env, args)))
        if result.code != 0:
            die(f"mongosh failed: {said(result)}")
        return result.stdout.strip()

    def held_roles(self, user: str, auth_db: str) -> Optional[List[Role]]:
        out = self.mongosh("print(JSON.stringify(db.getSiblingDB(process.env.G_DB).getUser(process.env.G_USER)))",
                           user=user, db=auth_db)
        record = json.loads(out.splitlines()[-1]) if out else None
        return None if record is None else [{"role": r["role"], "db": r["db"]} for r in record.get("roles", [])]

    def grant(self, user: str, auth_db: str, roles: Sequence[Role]) -> None:
        self.mongosh("db.getSiblingDB(process.env.G_DB).grantRolesToUser(process.env.G_USER, JSON.parse(process.env.G_ROLES))",
                     user=user, db=auth_db, roles=json.dumps(list(roles)))


class Grants:
    """Each principal's required roles checked, or granted, against what prod holds."""

    def __init__(self, mongo: Mongo, files: Files, env: Mapping[str, str]) -> None:
        self.mongo, self.files, self.env = mongo, files, env

    def ci_read_uri(self) -> str:
        if self.env.get("MONGO_CI_READ_URI"):
            return self.env["MONGO_CI_READ_URI"]
        path = os.path.join(REPO, CI_READ_NIX)
        username = default_username(self.files.read_text(path))
        if username is None:
            die(f"no default username in {path}; set MONGO_CI_READ_URI")
        return tunnel_uri(f"{username}:-", "0")

    def principal(self, who: str, roles: Sequence[Role], uri: str, apply: bool) -> bool:
        """Whether this principal is short of its roles still (printed as it goes)."""
        user, auth_db = user_of(uri)
        if not user:
            die(f"no user name in the {who} connection string")
        name = f"{who} {user}@{auth_db}"
        held = self.mongo.held_roles(user, auth_db)
        if held is None:
            print(f"MISSING USER {name}: not created here; create it by hand (see docs/adding-a-country.md)")
            return True
        gap = missing(roles, held)
        if not gap:
            print(f"ok {name}: {len(roles)} roles")
            return False
        for r in gap:
            print(f"missing {name}: {r['role']} on {r['db']}")
        if not apply:
            return True
        self.mongo.grant(user, auth_db, gap)
        still = missing(roles, self.mongo.held_roles(user, auth_db) or [])
        if still:
            print(f"STILL MISSING {name} after the grant: {json.dumps(still)}")
            return True
        print(f"granted {name}: {len(gap)} roles")
        return False

    def run(self, apply: bool) -> int:
        for var in ("MONGODB_URI", "MONGO_ROOT_URI"):
            if not self.env.get(var):
                die(f"{var} is not set: run this through scripts/mongo-grants")
        need = required(self.files.read_text(self.env.get("MONGO_GRANTS_LIST", os.path.join(HERE, "mongo-grants.tsv"))))
        uris = {"app": self.env["MONGODB_URI"], "ci-read": self.ci_read_uri()}
        unknown = set(need) - set(uris)
        if unknown:
            die(f"no connection string for principal(s) {', '.join(sorted(unknown))}")
        failed = [self.principal(who, roles, uris[who], apply) for who, roles in need.items()]
        if any(failed):
            print("[mongo-grants] missing access" + ("" if apply else ": run scripts/mongo-grants --apply"), file=sys.stderr)
        return 1 if any(failed) else 0


def main(argv: Sequence[str], env: Mapping[str, str]) -> int:
    if len(argv) != 1 or argv[0] not in ("--check", "--apply"):
        print("usage: mongo_grants.py --check | --apply", file=sys.stderr)
        return 2
    return Grants(Mongo(tool_process.SubprocessRunner(), env), Files.here(), env).run(argv[0] == "--apply")


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment()))
