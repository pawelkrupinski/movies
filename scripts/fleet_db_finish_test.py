#!/usr/bin/env python3
"""scripts/fleet-db-finish, black-box: the counts, the exact-names --drop, and every refusal. No vault, ssh or Mongo:
`secrets`, `ssh` and `mongosh` are fakes on PATH (the fake ssh listens on the port the tool forwards, as a tunnel would).
The fake mongosh answers the counts from a JSON state file, records every drop with the user it ran as and removes the
collection from the state, so the cases see what was dropped, from where, and as whom. Ported from
fleet-db-finish-test.sh case for case; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/fleet_db_finish.py", "scripts/fleet-db-finish")
THREE = ["facebook_rescrapes", "facebook_fetches", "fleet_host_pace"]
SECRETS = '''
shift 2
while [ "$1" != "--" ]; do v="FAKE_$1"; export "$1=${!v}"; shift; done
shift
exec "$@"
'''
SSH = '''
import signal, socket
listener = socket.socket()
listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
listener.bind(("127.0.0.1", int(argv[argv.index("-L") + 1].split(":")[0])))
listener.listen(5)
signal.pause()
'''
MONGOSH = '''
import urllib.parse
user = urllib.parse.unquote(urllib.parse.urlsplit(env["KINOWO_MONGOSH_URI"]).username or "")
text = " ".join(argv)
if "ping" in text:
    exit(0)
def log(line):
    with open(env["CALLS"], "a") as f:
        f.write(line + "\\n")
state = json.load(open(env["STATE"]))
if "appReads" in text:
    log("probe as " + user)
    if os.path.exists(env["APP_DENIED"]):
        err("MongoServerError: not authorized on kinowo_fleet"); exit(1)
    out('{"appReads":3}')
elif "countDocuments" in text:
    log("counts as " + user)
    names = ["facebook_rescrapes", "facebook_fetches", "fleet_host_pace"]
    out(json.dumps({"counts": {d: {c: state[d].get(c) for c in names} for d in state},
                    "newest": {d: "2026-10-25" for d in state}}))
elif "drop()" in text:
    log("drop kinowo." + env["G_COLL"] + " as " + user)
    state["kinowo"].pop(env["G_COLL"], None)
    json.dump(state, open(env["STATE"], "w"))
    out(env["G_COLL"] + " dropped from kinowo: true")
else:
    err("fake mongosh: unexpected " + text); exit(3)
'''


class FleetDbFinishTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.state = self.path("state.json")
        self.calls_file = self.path("mongosh-calls")
        self.denied = self.path("app-denied")
        self.fakes.script("secrets", SECRETS, shell="/bin/bash")
        self.fakes.program("ssh", SSH)
        self.fakes.program("mongosh", MONGOSH)

    def seed(self, fleet_fetches: Optional[int] = 950) -> None:
        with open(self.state, "w") as f:
            json.dump({"kinowo": {"facebook_rescrapes": 12, "facebook_fetches": 900, "fleet_host_pace": 4},
                       "kinowo_fleet": {"facebook_rescrapes": 14, "facebook_fetches": fleet_fetches,
                                        "fleet_host_pace": 4}, "kinowo_uk": {}}, f)

    def finish(self, *args: str) -> cli_harness.Done:
        return self.run_tool(*args, env={
            "STATE": self.state, "CALLS": self.calls_file, "APP_DENIED": self.denied,
            "MONGODB_URI": None, "MONGO_ROOT_URI": None, "MONGO_CI_READ_URI": None, "KINOWO_MONGO_SSH": None,
            "PROD_OPS_TUNNELLED": None, "PROD_TUNNEL_PORT": None,
            "FAKE_MONGODB_URI": "mongodb://kinowo_app_2:pw@10.20.0.13:27017/kinowo?authSource=kinowo",
            "FAKE_MONGO_ROOT_URI": "mongodb://kinowo_root:rootpw@10.20.0.13:27017/admin?authSource=admin"})

    def mongosh_log(self) -> List[str]:
        return self.read("mongosh-calls").splitlines() if os.path.exists(self.calls_file) else []

    def drops(self) -> List[str]:
        return [line for line in self.mongosh_log() if line.startswith("drop")]

    def kinowo_fleet_and_uk(self) -> Dict[str, object]:
        with open(self.state) as f:
            state = json.load(f)
        return {"fleet": state["kinowo_fleet"], "uk": state["kinowo_uk"]}

    def test_read_only_by_default_exits_0_and_drops_nothing(self) -> None:
        self.seed()
        done = self.finish()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([], self.drops())

    def test_it_prints_each_collections_count_in_kinowo_and_kinowo_fleet(self) -> None:
        self.seed()
        done = self.finish()
        rows = [line.split() for line in done.stdout.splitlines()]
        self.assertIn(["facebook_fetches", "900", "950"], rows)

    def test_counts_as_root_and_probes_as_the_app_over_one_vault_run_and_one_tunnel(self) -> None:
        self.seed()
        self.finish()
        self.assertEqual(["counts as kinowo_root", "probe as kinowo_app_2"],
                         [line for line in self.mongosh_log() if line.startswith(("counts", "probe"))])
        self.assertEqual([["run", "movies", "MONGODB_URI", "MONGO_ROOT_URI"]],
                         [c[:4] for c in self.fakes.calls("secrets")])
        self.assertEqual(1, len(self.fakes.calls("ssh")))

    def test_the_three_exact_names_in_any_order_drop_each_from_kinowo_as_root(self) -> None:
        self.seed()
        done = self.finish("--drop", "fleet_host_pace", "facebook_rescrapes", "facebook_fetches")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["drop kinowo.facebook_rescrapes as kinowo_root", "drop kinowo.facebook_fetches as kinowo_root",
                          "drop kinowo.fleet_host_pace as kinowo_root"], self.drops())
        self.assertEqual({"fleet": {"facebook_rescrapes": 14, "facebook_fetches": 950, "fleet_host_pace": 4}, "uk": {}},
                         self.kinowo_fleet_and_uk())

    def test_anything_but_exactly_the_three_names_is_refused_before_the_vault_or_mongo_is_asked(self) -> None:
        for bad in (["--drop"], ["--drop", "facebook_rescrapes", "facebook_fetches"],
                    ["--drop", "facebook_*", "facebook_fetches", "fleet_host_pace"],
                    ["--drop", "facebook_rescrapes", "facebook_rescrapes", "fleet_host_pace"],
                    ["--drop", "facebook_rescrapes", "facebook_fetches", "fleet_host_pace", "users"], ["--drop-all"]):
            with self.subTest(bad=bad):
                self.seed()
                self.fakes.clear()
                done = self.finish(*bad)
                self.assertEqual(2, done.code, done.output)
                self.assertEqual([], self.fakes.calls())
                self.assertEqual([], self.mongosh_log())

    def test_help_prints_the_header_and_exits_0(self) -> None:
        done = self.finish("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("--drop facebook_rescrapes facebook_fetches fleet_host_pace", done.stdout)
        self.assertEqual([], self.fakes.calls())

    def test_kinowo_fleet_lacking_one_of_the_three_refuses_and_drops_nothing_saying_which(self) -> None:
        self.seed(None)
        done = self.finish("--drop", *THREE)
        self.assertEqual(1, done.code, done.output)
        self.assertEqual([], self.drops())
        self.assertIn("REFUSE: kinowo_fleet lacks or holds nothing in facebook_fetches", done.stdout.splitlines())

    def test_kinowo_fleet_holding_none_of_one_refuses_and_drops_nothing(self) -> None:
        self.seed(0)
        done = self.finish("--drop", *THREE)
        self.assertEqual(1, done.code, done.output)
        self.assertEqual([], self.drops())

    def test_the_apps_user_unable_to_read_kinowo_fleet_refuses_and_drops_nothing(self) -> None:
        self.seed()
        open(self.denied, "w").close()
        done = self.finish("--drop", *THREE)
        self.assertEqual(1, done.code, done.output)
        self.assertEqual([], self.drops())
        self.assertIn("REFUSE: the app's user cannot read kinowo_fleet", done.stdout.splitlines())


if __name__ == "__main__":
    unittest.main()
