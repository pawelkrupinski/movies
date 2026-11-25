#!/usr/bin/env python3
"""infra/bin/backtest-alerts-local over a tiny synthetic copy of ci-runner's snapshot directory.

The remote is infra/test/fixtures/backtest-snapshot-remote/, laid out as ci-runner's /mnt/data/prometheus-snapshot
(generations/<stamp>/{tsdb/<ulid>/, rules.json, buildinfo.json, pull.json}); rsync reads a local path as it reads
`host:`. Its three blocks are REAL Prometheus 3.12.0 blocks, two hours each from 2025-11-01T00:00Z, written by
`promtool tsdb create-blocks-from openmetrics` from node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes of
two instances, `b` at 10% available from 03:00 to 03:30 and both at 50% otherwise -- so MemoryAvailableLow (< 15% for
15m) fires once, for 16 minutes.

Most cases stub the backtest itself (BACKTEST_ALERTS) and the binary, so they run anywhere; the last runs the real
backtest-alerts on the real Prometheus when nix can build the flake's (the Mac), and proves the cache is never
written by the replay.
Run: python3 infra/bin/backtest_alerts_local_test.py
"""

import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
TOOL = os.path.join(HERE, "backtest-alerts-local")
REMOTE = os.path.join(ROOT, "infra", "test", "fixtures", "backtest-snapshot-remote") + "/"
GENERATION = os.path.join(REMOTE, "generations", "20251101T060000Z")
FIRST, SECOND, THIRD = sorted(os.listdir(os.path.join(GENERATION, "tsdb")))
END = "2025-11-01T05:59:00Z"

STUB = r"""#!/usr/bin/env python3
import json, os, sys
args = sys.argv[1:]
snapshot = args[args.index("--snapshot") + 1]
tsdb = os.path.join(snapshot, "tsdb")
with open(os.environ["STUB_RECORD"], "w") as handle:
    json.dump({"args": args, "blocks": sorted(os.listdir(tsdb)),
               "links": sorted(os.path.realpath(os.path.join(tsdb, b)) for b in os.listdir(tsdb)),
               "pull": json.load(open(os.path.join(snapshot, "pull.json"))),
               "rules": os.path.exists(os.path.join(snapshot, "rules.json"))}, handle)
print("STUB BACKTEST")
sys.exit(int(os.environ.get("STUB_EXIT", "0")))
"""

AGENTCTL = r"""#!/bin/sh
# its probe answers as the real one's does (scripts/jvm-gate.sh governor_bin trusts nothing else)
[ "$1 $2" = "gov probe" ] && { echo "agentctl governor: admit cores route width"; exit 0; }
echo "$*" >> "$GOV_LOG"
exit 0
"""


def tree_hash(directory):
    digest = hashlib.sha256()
    for root, dirs, files in sorted(os.walk(directory)):
        dirs.sort()
        for name in sorted(files):
            path = os.path.join(root, name)
            digest.update(os.path.relpath(path, directory).encode())
            with open(path, "rb") as handle:
                digest.update(handle.read())
    return digest.hexdigest()


class LocalBacktest(unittest.TestCase):
    def setUp(self):
        self.work = tempfile.mkdtemp(prefix="backtest-local-test-")
        self.cache = os.path.join(self.work, "cache")
        self.record = os.path.join(self.work, "record.json")
        stub = os.path.join(self.work, "backtest-alerts-stub")
        with open(stub, "w") as handle:
            handle.write(STUB)
        os.chmod(stub, 0o755)
        self.env = dict(os.environ, BACKTEST_SNAPSHOT_REMOTE=REMOTE, BACKTEST_LOCAL_CACHE=self.cache,
                        BACKTEST_ALERTS=stub, BACKTEST_PROMETHEUS_BINARY="/bin/true", STUB_RECORD=self.record,
                        KINOWO_GOVERNOR="0")

    def tearDown(self):
        for root, dirs, files in os.walk(self.work):
            for name in files:
                os.chmod(os.path.join(root, name), 0o644)
        shutil.rmtree(self.work, ignore_errors=True)

    def run_tool(self, *args, **env):
        done = subprocess.run([sys.executable, TOOL] + list(args), capture_output=True, text=True,
                              env=dict(self.env, **env), cwd=ROOT)
        return done.returncode, done.stdout + done.stderr

    def held(self):
        return sorted(os.listdir(os.path.join(self.cache, "blocks")))

    def recorded(self):
        with open(self.record) as handle:
            return json.load(handle)

    def test_copies_only_the_blocks_the_window_and_its_lead_need(self):
        # 72 min ending 05:59, widened by MemoryAvailableLow's 15m hold, its group's interval and an hour: from
        # about 03:30, so the 00:00-02:00 block is never copied.
        status, out = self.run_tool("--alert", "MemoryAvailableLow", "--days", "0.05", "--end", END)
        self.assertEqual(status, 0, out)
        self.assertEqual(self.held(), [SECOND, THIRD])
        record = self.recorded()
        self.assertEqual(record["blocks"], [SECOND, THIRD])
        self.assertEqual(record["links"], [os.path.join(os.path.realpath(self.cache), "blocks", u)
                                           for u in (SECOND, THIRD)])
        # the replay's floor is where THESE blocks start, not where ci-runner's copy does
        self.assertEqual(record["pull"]["min_time"], 1761962400.0)
        self.assertTrue(record["rules"])
        args = record["args"]
        self.assertEqual(args[:2], ["--source", "snapshot"])
        self.assertIn("--no-write", args)
        self.assertEqual(args[args.index("--alert") + 1], "MemoryAvailableLow")
        self.assertEqual(args[args.index("--prometheus-binary") + 1], "/bin/true")
        self.assertIn("copying 2 of 2 block(s)", out)
        self.assertNotIn(os.path.join("cache", "run-"), " ".join(os.listdir(self.cache)))

    def test_a_second_run_copies_nothing_and_a_wider_one_only_what_is_new(self):
        self.assertEqual(self.run_tool("--alert", "MemoryAvailableLow", "--days", "0.05", "--end", END)[0], 0)
        cached = os.path.join(self.cache, "blocks", THIRD, "index")
        before = os.stat(cached).st_mtime_ns
        status, out = self.run_tool("--alert", "MemoryAvailableLow", "--days", "0.05", "--end", END)
        self.assertEqual(status, 0, out)
        self.assertNotIn("copying", out)
        self.assertIn("(0 copied", out)
        self.assertEqual(os.stat(cached).st_mtime_ns, before)
        status, out = self.run_tool("--alert", "MemoryAvailableLow", "--days", "0.2", "--end", END)
        self.assertEqual(status, 0, out)
        self.assertIn("copying 1 of 3 block(s)", out)
        self.assertEqual(self.held(), [FIRST, SECOND, THIRD])

    def test_a_block_no_newer_generation_lists_is_dropped(self):
        remote = os.path.join(self.work, "remote")
        shutil.copytree(REMOTE, remote, symlinks=True)
        self.assertEqual(self.run_tool("--alert", "MemoryAvailableLow", "--end", END,
                                       BACKTEST_SNAPSHOT_REMOTE=remote + "/")[0], 0)
        self.assertEqual(self.held(), [FIRST, SECOND, THIRD])
        for stamp in ("20251102T060000Z", "20251103T060000Z"):
            # the next nights: FIRST compacted away (the copy's floor moves on, as retention does)
            generation = os.path.join(remote, "generations", stamp)
            shutil.copytree(GENERATION, generation)
            shutil.rmtree(os.path.join(generation, "tsdb", FIRST))
            with open(os.path.join(generation, "pull.json")) as handle:
                pull = json.load(handle)
            pull["blocks"].remove(FIRST)
            with open(os.path.join(generation, "pull.json"), "w") as handle:
                json.dump(pull, handle)
            status, out = self.run_tool("--alert", "MemoryAvailableLow", "--end", END,
                                        BACKTEST_SNAPSHOT_REMOTE=remote + "/")
            self.assertEqual(status, 0, out)
            self.assertIn("generation %s" % stamp, out)
        # kept while the 11-02 generation and the 11-01 one were the newest two, gone once 11-01 is not
        self.assertEqual(self.held(), [SECOND, THIRD])
        self.assertEqual(sorted(os.listdir(os.path.join(self.cache, "generations"))),
                         ["20251102T060000Z", "20251103T060000Z"])
        self.assertEqual(self.recorded()["blocks"], [SECOND, THIRD])

    def test_cached_blocks_are_read_only(self):
        self.assertEqual(self.run_tool("--alert", "MemoryAvailableLow", "--end", END)[0], 0)
        for ulid in self.held():
            for name in ("index", "meta.json", os.path.join("chunks", "000001")):
                mode = os.stat(os.path.join(self.cache, "blocks", ulid, name)).st_mode
                self.assertEqual(mode & 0o222, 0, "%s/%s is writable" % (ulid, name))

    def test_short_disk_refuses_before_copying_anything(self):
        status, out = self.run_tool("--alert", "MemoryAvailableLow", "--keep-free-gb", "1000000000")
        self.assertEqual(status, 2, out)
        self.assertIn("not enough disk for the copy", out)
        self.assertEqual(self.held(), [])
        self.assertFalse(os.path.exists(self.record))

    def test_offline_uses_the_cache_and_refuses_a_block_it_lacks(self):
        status, out = self.run_tool("--offline", "--alert", "MemoryAvailableLow")
        self.assertEqual(status, 2, out)
        self.assertIn("run once without --offline", out)
        self.assertEqual(self.run_tool("--alert", "MemoryAvailableLow", "--days", "0.05", "--end", END)[0], 0)
        status, out = self.run_tool("--offline", "--alert", "MemoryAvailableLow", "--days", "0.05", "--end", END,
                                    BACKTEST_SNAPSHOT_REMOTE="/nonexistent/")
        self.assertEqual(status, 0, out)
        status, out = self.run_tool("--offline", "--alert", "MemoryAvailableLow", "--days", "0.2", "--end", END)
        self.assertEqual(status, 2, out)
        self.assertIn("not cached: %s" % FIRST, out)

    def test_a_block_whose_meta_changed_since_its_listing_is_refused(self):
        os.makedirs(os.path.join(self.cache, "metas"))
        with open(os.path.join(GENERATION, "tsdb", THIRD, "meta.json")) as handle:
            meta = json.load(handle)
        meta["stats"]["numSamples"] += 1
        with open(os.path.join(self.cache, "metas", THIRD + ".json"), "w") as handle:
            json.dump(meta, handle)
        status, out = self.run_tool("--alert", "MemoryAvailableLow", "--days", "0.05", "--end", END)
        self.assertEqual(status, 2, out)
        self.assertIn("block %s changed between its listing and its copy" % THIRD, out)
        self.assertNotIn(THIRD, self.held())

    def test_the_backtest_exit_status_is_the_tools(self):
        status, out = self.run_tool("--alert", "MemoryAvailableLow", "--end", END, STUB_EXIT="1")
        self.assertEqual(status, 1, out)

    def test_the_run_takes_a_governor_ticket(self):
        agentctl = os.path.join(self.work, "agentctl")
        with open(agentctl, "w") as handle:
            handle.write(AGENTCTL)
        os.chmod(agentctl, 0o755)
        log = os.path.join(self.work, "gov.log")
        status, out = self.run_tool("--alert", "MemoryAvailableLow", "--end", END, KINOWO_GOVERNOR="1",
                                    KINOWO_GOVERNOR_BIN=agentctl, KINOWO_JVM_GATE_OS="Darwin", GOV_LOG=log,
                                    AGENTCTL_GOV_DIR=os.path.join(self.work, "gov"), KINOWO_GOV_CPU_HELD="",
                                    KINOWO_JVM_SLOT_HELD="", KINOWO_JVM_GATE_FOLLOW_MAIN="0")
        self.assertEqual(status, 0, out)
        with open(log) as handle:
            admitted = handle.read()
        self.assertIn("gov admit", admitted)
        self.assertIn("--label backtest-alerts-local", admitted)
        self.assertIn("--job backtest --cores 3", admitted)
        self.assertIn("STUB BACKTEST", out)

    def test_in_a_check_or_under_a_parents_ticket_it_takes_none(self):
        agentctl = os.path.join(self.work, "agentctl")
        with open(agentctl, "w") as handle:
            handle.write(AGENTCTL)
        os.chmod(agentctl, 0o755)
        log = os.path.join(self.work, "gov.log")
        governed = dict(KINOWO_GOVERNOR="1", KINOWO_GOVERNOR_BIN=agentctl, KINOWO_JVM_GATE_OS="Darwin", GOV_LOG=log,
                        AGENTCTL_GOV_DIR=os.path.join(self.work, "gov"), KINOWO_JVM_GATE_FOLLOW_MAIN="0")
        status, out = self.run_tool("--alert", "MemoryAvailableLow", "--end", END, KINOWO_GOV_CPU_HELD="1", **governed)
        self.assertEqual(status, 0, out)
        self.assertIn("STUB BACKTEST", out)
        self.assertFalse(os.path.exists(log), "a check's hook holds the ticket: none of its own")

    def test_the_real_replay_over_the_fixture_and_the_cache_is_never_written(self):
        nix = next((p for p in ("/nix/var/nix/profiles/default/bin/nix", shutil.which("nix") or "") if
                    p and os.access(p, os.X_OK)), None)
        if not nix:
            self.skipTest("no nix to build the flake's Prometheus (the Linux container): the stubbed cases ran")
        env = {k: v for k, v in self.env.items() if k not in ("BACKTEST_ALERTS", "BACKTEST_PROMETHEUS_BINARY")}
        self.env = env
        status, out = self.run_tool("--alert", "MemoryAvailableLow", "--days", "0.2", "--end", END,
                                    "--no-incidents")
        self.assertEqual(status, 0, out)
        self.assertIn("replay: Prometheus 3.12.0", out)
        row = [line for line in out.splitlines() if line.split()[:1] == ["MemoryAvailableLow"]]
        self.assertEqual(len(row), 1, out)
        # 16 firing minutes (15m hold, then 03:15..03:30) of the 4.8-hour window's 289 steps
        self.assertEqual(row[0].split()[1:4], ["young", "5.19%", "0.00%"], row[0])
        # after a real Prometheus ran over them, every cached block is still byte for byte the remote's
        self.assertEqual(self.held(), [FIRST, SECOND, THIRD])
        for ulid in self.held():
            self.assertEqual(tree_hash(os.path.join(self.cache, "blocks", ulid)),
                             tree_hash(os.path.join(GENERATION, "tsdb", ulid)), ulid)

if __name__ == "__main__":
    unittest.main()
