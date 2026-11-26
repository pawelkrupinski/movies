#!/usr/bin/env python3
"""scripts/local-mirror/start-local-mongo.sh, black-box: which formula it asks `brew services` for (the installed one,
the newest, or LOCAL_MONGO_FORMULA's), that it still brings Mongo up when `brew services` cannot (`mongod --config …
--fork`, the formula's own mongod when it has one) or no formula is installed, the config it writes (loopback, the
port, the single-node replica set) and the lines it says up to PRIMARY. No real brew, mongod or mongosh: all three are
fakes recording what they were asked, and the config lands in a scratch prefix, so this never touches the machine's
own Mongo (the prod mirror included). Ported from start-local-mongo-test.sh case for case; the same cases hold the
shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import time
import unittest
import unittest.mock
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/local-mirror/start_local_mongo.py", "scripts/local-mirror/start-local-mongo.sh")
# brew: its prefix, the formulas $FORMULAS lists, `services` exiting $SERVICES_EXIT
BREW = '''
if argv[:1] == ["--prefix"]:
    out(env["PREFIX"])
elif argv[:1] == ["list"]:
    out(env.get("FORMULAS", ""), end="")
elif argv[:1] == ["services"]:
    exit(int(env.get("SERVICES_EXIT", "0")))
'''
# mongosh: a healthy single-node primary (the set already initiated unless $UNINITIATED)
MONGOSH = '''
script = " ".join(argv)
if "ping" in script:
    out("1")
elif "isWritablePrimary" in script:
    out("true")
elif env.get("UNINITIATED"):
    out("[local-mongo] replica set initiated")
else:
    out("[local-mongo] replica set already initiated")
'''


class StartLocalMongoTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.prefix = self.path("prefix")
        os.makedirs(os.path.join(self.prefix, "etc"))
        self.fakes.program("brew", BREW)
        self.fakes.program("mongod", "")
        self.fakes.program("mongosh", MONGOSH)

    def start(self, **extra: str) -> cli_harness.Done:
        return self.run_tool(env={"PREFIX": self.prefix, "LOCAL_MIRROR_PORT": "28999", "LOCAL_MONGO_FORMULA": None,
                                  **extra})

    def calls(self, name: str) -> List[List[str]]:
        return self.fakes.calls(name)

    def test_asks_brew_services_for_the_installed_formula(self) -> None:
        done = self.start(FORMULAS="mongodb-community@8.0\nmongodb-database-tools\nmongosh\n")
        self.assertEqual(0, done.code, done.output)
        self.assertIn(["services", "restart", "mongodb-community@8.0"], self.calls("brew"))
        self.assertEqual([], self.calls("mongod"))
        self.assertEqual("[local-mongo] (re)starting mongodb-community@8.0 via brew services on :28999\n"
                         "[local-mongo] replica set already initiated\n"
                         "[local-mongo] PRIMARY ready on :28999 (brew services (mongodb-community@8.0))\n", done.stdout)
        self.assertEqual("", done.stderr)

    def test_the_newest_formula_or_the_one_named(self) -> None:
        self.start(FORMULAS="mongodb-community@7.0\nmongodb-community@8.0\nmongodb-community\n")
        self.assertIn(["services", "restart", "mongodb-community@8.0"], self.calls("brew"))
        self.fakes.clear()
        self.start(FORMULAS="mongodb-community@8.0\n", LOCAL_MONGO_FORMULA="mongodb-community@7.0")
        self.assertIn(["services", "restart", "mongodb-community@7.0"], self.calls("brew"))

    def test_writes_the_replica_set_config(self) -> None:
        self.start(FORMULAS="mongodb-community@8.0\n")
        with open(os.path.join(self.prefix, "etc", "mongod.conf")) as f:
            self.assertEqual(f"""systemLog:
  destination: file
  path: {self.prefix}/var/log/mongodb/mongo.log
  logAppend: true
storage:
  dbPath: {self.prefix}/var/mongodb
net:
  bindIp: 127.0.0.1
  port: 28999
replication:
  replSetName: rs0
""", f.read())
        self.assertTrue(os.path.isdir(os.path.join(self.prefix, "var", "mongodb")))
        self.assertTrue(os.path.isdir(os.path.join(self.prefix, "var", "log", "mongodb")))

    def test_falls_back_to_mongod_fork_when_brew_services_fails(self) -> None:
        done = self.start(FORMULAS="mongodb-community@7.0\n", SERVICES_EXIT="1")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["--config", f"{self.prefix}/etc/mongod.conf", "--fork"]], self.calls("mongod"))
        self.assertEqual("[local-mongo] brew services could not start mongodb-community@7.0 — falling back to mongod "
                         "--fork\n", done.stderr)
        self.assertTrue(done.stdout.endswith("[local-mongo] PRIMARY ready on :28999 (mongod --fork)\n"), done.stdout)

    def test_the_fallback_runs_the_formulas_own_mongod(self) -> None:
        own = self.path("prefix", "opt", "mongodb-community@7.0", "bin", "mongod")
        os.makedirs(os.path.dirname(own))
        with open(own, "w") as f:
            f.write(f"#!/bin/sh\necho \"$@\" > {self.path('own-mongod')}\n")
        os.chmod(own, 0o755)
        done = self.start(FORMULAS="mongodb-community@7.0\n", SERVICES_EXIT="1")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(f"--config {self.prefix}/etc/mongod.conf --fork\n", self.read("own-mongod"))
        self.assertEqual([], self.calls("mongod"))

    def test_starts_mongod_without_a_formula(self) -> None:
        done = self.start(FORMULAS="mongosh\n")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([], [c for c in self.calls("brew") if c[:1] == ["services"]])
        self.assertEqual([["--config", f"{self.prefix}/etc/mongod.conf", "--fork"]], self.calls("mongod"))
        self.assertEqual("[local-mongo] no mongodb-community formula installed — starting mongod directly\n",
                         done.stderr)

    def test_initiates_the_set_once(self) -> None:
        done = self.start(FORMULAS="mongodb-community@8.0\n", UNINITIATED="1")
        self.assertIn("[local-mongo] replica set initiated\n", done.stdout)
        initiate = [c for c in self.calls("mongosh") if "rs.initiate" in " ".join(c)]
        self.assertEqual(1, len(initiate))
        self.assertEqual(["mongodb://127.0.0.1:28999/?directConnection=true", "--quiet", "--eval"], initiate[0][:3])
        self.assertIn('rs.initiate({_id:"rs0", members:[{_id:0, host:"127.0.0.1:28999"}]});', initiate[0][3])


class PingDeadlineTest(cli_harness.ToolCase):
    """The Python tool's own bound: a mongosh that hangs (as one does against a down port) must not stretch a ping
    deadline past its seconds."""
    TOOL = TOOL

    def test_a_hanging_mongosh_cannot_stretch_the_ping_deadline(self) -> None:
        sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
        import start_local_mongo
        self.fakes.script("mongosh", "sleep 60")
        with unittest.mock.patch.dict(os.environ, {"PATH": self.fakes.bin + os.pathsep + os.environ["PATH"]}):
            began = time.monotonic()
            answered = start_local_mongo.LocalMongo(self.path("prefix"), "28999").answers_ping(2)
        self.assertFalse(answered)
        self.assertLess(time.monotonic() - began, 6)


if __name__ == "__main__":
    unittest.main()
