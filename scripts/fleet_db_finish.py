#!/usr/bin/env python3
"""Count the fleet's three old collections in Poland's kinowo and drop them on an exact request (fleet DB migration, step 9)

ONE-SHOT: delete this tool (and fleet_db_finish_test.py) once its --drop has run; the drop waits for the owner's word
that the fleet-database soak is over (docs/fleet-database-migration.md, step 9).
Step 9 of docs/fleet-database-migration.md: the fleet's three old collections in Poland's `kinowo`, counted, and
dropped only on an explicit, exact request.

  scripts/fleet-db-finish
      read-only: each of facebook_rescrapes, facebook_fetches and fleet_host_pace in kinowo and in kinowo_fleet
      (document counts, the newest facebook_fetches.fetchedAt in each), and whether the app's user reads kinowo_fleet
  scripts/fleet-db-finish --drop facebook_rescrapes facebook_fetches fleet_host_pace
      the same, then drops those three collections FROM kinowo. All three names, spelled exactly, in any order;
      nothing else is accepted (no pattern, no other collection, no other database). Refused when kinowo_fleet
      lacks any of the three or holds no documents in one, or when the app's user cannot read kinowo_fleet.

Run --drop only after the soak, after the step-8 code (kinowo_fleet as the default) is deployed everywhere, and after
a nightly dump that contains kinowo_fleet: everything else in kinowo is Poland's live corpus.

One Touch ID, one tunnel: the tool runs itself again under scripts/with-prod-mongo.sh --both (the harness of
scripts/prod-ops.sh). Counts and drops run as root (MONGO_ROOT_URI); the read probe runs as the app (MONGODB_URI).
Tests: scripts/fleet_db_finish_test.py."""
from __future__ import annotations

import json
import os
import sys
from typing import List, Mapping, Optional, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
import tool_process  # noqa: E402

TOOL = "fleet-db-finish"
COLLECTIONS = ["facebook_rescrapes", "facebook_fetches", "fleet_host_pace"]
WITH_PROD_MONGO = os.path.join(HERE, "with-prod-mongo.sh")
MONGOSH_AT = os.path.join(HERE, "local-mirror", "mongosh-at.sh")
COUNTS_JS = """
  const C = ["facebook_rescrapes", "facebook_fetches", "fleet_host_pace"], out = { counts: {}, newest: {} };
  for (const d of ["kinowo", "kinowo_fleet"]) {
    const s = db.getSiblingDB(d), names = s.getCollectionNames();
    out.counts[d] = {};
    for (const c of C) out.counts[d][c] = names.includes(c) ? s.getCollection(c).countDocuments({}) : null;
    const top = s.facebook_fetches.find({}, { fetchedAt: 1 }).sort({ fetchedAt: -1 }).limit(1).toArray();
    out.newest[d] = top.length ? String(top[0].fetchedAt) : null;
  }
  print(JSON.stringify(out));"""
# As the app: the fleet database it now runs on must be readable by it before its old copy goes.
APP_PROBE_JS = 'print(JSON.stringify({ appReads: db.getSiblingDB("kinowo_fleet").getCollection("fleet_host_pace").countDocuments({}) }))'
# The collection name arrives in G_COLL and is checked against the three AGAIN here, beside the drop.
DROP_JS = """
  const c = process.env.G_COLL;
  if (!["facebook_rescrapes", "facebook_fetches", "fleet_host_pace"].includes(c)) { print("refused: " + c); quit(2); }
  print(c + " dropped from kinowo: " + db.getSiblingDB("kinowo").getCollection(c).drop());"""


def usage_error(line: str) -> int:
    print(f"[{TOOL}] {line}", file=sys.stderr)
    return 2


def drop_request(names: List[str]) -> Optional[str]:
    """Why `names` (the arguments after --drop) are not exactly the three collections, each once; None when they are."""
    want = " ".join(COLLECTIONS)
    if len(names) != len(COLLECTIONS) or any(names.count(c) != 1 for c in COLLECTIONS):
        return f"--drop takes exactly the {len(COLLECTIONS)} names, each once: {want} (got: {' '.join(names)})"
    return None


def last_line(text: str) -> str:
    lines = text.splitlines()
    return lines[-1] if lines else ""


def shown(count: Optional[int]) -> str:
    return "absent" if count is None else str(count)


def verdict(counts: Mapping[str, Mapping[str, Mapping[str, Optional[int]]]], probe: str) -> Tuple[List[str], bool]:
    """The table, and whether a drop may go ahead (else its last line says why not)."""
    lines = [f"{'collection':<20} {'kinowo':>10} {'kinowo_fleet':>13}"]
    for c in COLLECTIONS:
        lines.append(f"{c:<20} {shown(counts['counts']['kinowo'][c]):>10} {shown(counts['counts']['kinowo_fleet'][c]):>13}")
    for d in ("kinowo", "kinowo_fleet"):
        lines.append(f"newest facebook_fetches.fetchedAt in {d}: {counts['newest'][d]}")
    try:
        reads: Optional[int] = json.loads(probe)["appReads"]
        lines.append(f"the app's user reads kinowo_fleet ({reads} fleet_host_pace docs)")
    except (ValueError, KeyError, TypeError):
        reads = None
        lines.append(f"the app's user could NOT read kinowo_fleet: {probe}")
    empty = [c for c in COLLECTIONS if not counts["counts"]["kinowo_fleet"][c]]
    if empty:
        return lines + ["REFUSE: kinowo_fleet lacks or holds nothing in " + ", ".join(empty)], False
    if reads is None:
        return lines + ["REFUSE: the app's user cannot read kinowo_fleet"], False
    return lines, True


class Finish:
    def __init__(self, env: Mapping[str, str], runner: tool_process.Runner) -> None:
        self.env, self.runner = env, runner

    def mongosh(self, uri: str, js: str, extra: Optional[Mapping[str, str]] = None) -> tool_process.Captured:
        command = tool_process.Command([MONGOSH_AT, uri, "--quiet", "--eval", js], {**self.env, **(extra or {})},
                                       stderr_through=True)
        return self.runner.capture(command)

    def as_root(self, js: str, extra: Optional[Mapping[str, str]] = None) -> str:
        """The last line mongosh printed as root (it prints warnings first); the tool ends with mongosh's status when
        it fails."""
        done = self.mongosh(self.env["MONGO_ROOT_URI"], js, extra)
        if done.code != 0:
            raise SystemExit(done.code)
        return last_line(done.stdout)

    def as_app(self, js: str) -> str:
        done = self.mongosh(self.env["MONGODB_URI"], js)
        return last_line(done.stdout or done.stderr)

    def run(self, drop: bool) -> int:
        counts = self.as_root(COUNTS_JS)
        lines, safe = verdict(json.loads(counts), self.as_app(APP_PROBE_JS))
        print("\n".join(lines), flush=True)
        if not drop:
            print(f"read-only; to drop the three from kinowo: scripts/fleet-db-finish --drop {' '.join(COLLECTIONS)}")
            return 0
        if not safe:
            print(f"[{TOOL}] not dropping anything", file=sys.stderr)
            return 1
        for c in COLLECTIONS:
            print(self.as_root(DROP_JS, {"G_COLL": c}))
        print("== after")
        print(self.as_root(COUNTS_JS))
        return 0


def main(argv: List[str], env: Mapping[str, str]) -> int:
    if argv[:1] in (["-h"], ["--help"]):
        print(__doc__)
        return 0
    if argv and argv[0] != "--drop":
        print(__doc__, file=sys.stderr)
        return 2
    refusal = drop_request(argv[1:]) if argv else None
    if refusal:
        return usage_error(refusal)
    if not env.get("PROD_OPS_TUNNELLED"):
        # One Touch ID, one tunnel: with-prod-mongo.sh runs this tool again with both URIs rewritten to the tunnel.
        os.execv(WITH_PROD_MONGO, [WITH_PROD_MONGO, "--both", "env", "PROD_OPS_TUNNELLED=1", sys.executable,
                                   os.path.abspath(__file__), *argv])
    return Finish(env, tool_process.SubprocessRunner()).run(bool(argv))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
