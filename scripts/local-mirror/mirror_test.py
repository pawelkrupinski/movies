#!/usr/bin/env python3
# script-test-reads: scripts/local-mirror/mirror* scripts/local-mirror/*.js scripts/local-mirror/prod-tunnel.sh scripts/local-mirror/local-mongo.sh scripts/local-mirror/log-path.sh scripts/local-mirror/mongosh-at.sh scripts/lib/prod_tunnel.py scripts/lib/mongo_local.py scripts/jvm-gate.sh scripts/vault-env.sh
"""scripts/local-mirror/mirror.sh black-box (see mirror_fixture.py): the startup, which databases it mirrors, the upkeep and the code-change watch.
Run: python3 scripts/local-mirror/mirror_test.py
"""
from __future__ import annotations

import glob
import os
import re
import shutil
import signal
import subprocess
import unittest

from mirror_fixture import GOVERNOR, MirrorCase, TARGET, TOOL, described, gone, listening
import cli_harness  # noqa: E402  (on the path once mirror_fixture is imported)


class StartupTest(MirrorCase):
    def test_a_missing_mongodb_uri_reexecs_the_daemon_under_the_vault_and_nothing_else(self) -> None:
        done = self.run_tool("--reseed", env=self.daemon_env({"MONGODB_URI": None}))
        self.assertEqual(0, done.code, done.output)
        [call] = self.fakes.calls("secrets")
        self.assertEqual(["run", "movies", "MONGODB_URI", "--"], call[:4], call)
        self.assertEqual("--reseed", call[-1])
        self.assertEqual(os.path.dirname(TOOL), os.path.dirname(call[-2]))
        self.assertIn(os.path.basename(call[-2]), ("mirror.sh", "mirror.py"))
        self.assertEqual(([], []), (self.mongosh_calls(), self.ssh_pids()))

    @unittest.skipIf(shutil.which("secrets", path="/usr/bin:/bin"), "a secrets CLI in /usr/bin or /bin")
    def test_no_mongodb_uri_and_no_vault_cli_is_refused(self) -> None:
        os.remove(os.path.join(self.fakes.bin, "secrets"))
        env = self.env(self.daemon_env({"MONGODB_URI": None}))
        env["PATH"] = self.fakes.bin + ":/usr/bin:/bin"
        done = cli_harness.run_bounded([TOOL], env=env, cwd=self.work, stdin=None)
        self.assertEqual(1, done.code, done.output)
        self.assertIn("MONGODB_URI not set, and no `secrets` CLI on PATH", done.output)

    def test_no_database_through_the_tunnel_ends_the_run_and_leaves_nothing_behind(self) -> None:
        done = self.run_tool(env=self.daemon_env({"KINOWO_MIRROR_DBS": "", "FAKE_LISTING": ""}))
        self.assertEqual(1, done.code, done.output)
        self.assertIn("no kinowo* databases found via the tunnel — is MONGODB_URI right?", done.output)
        self.assertEqual([], os.listdir(self.path("tmp")))
        for pid in self.ssh_pids():
            self.until(f"the keeper's ssh outlived the run: {described(pid)}", lambda: gone(pid))
        port = self.tunnel_port
        [listing] = self.mongosh_calls("list")
        self.assertEqual(f"mongodb://u:p@127.0.0.1:{port}/kinowo?authSource=admin&directConnection=true"
                         "&compressors=zlib&serverSelectionTimeoutMS=10000", listing["uri"])
        self.assertEqual(f"mongodb://u:p@127.0.0.1:{port}/kinowo?authSource=admin&directConnection=true"
                         "&serverSelectionTimeoutMS=3000", self.mongosh_calls("ping")[0]["uri"])
        [ssh] = self.fakes.calls("ssh")
        self.assertEqual(["-N", "-L", f"{port}:127.0.0.1:27017", "-o", "ExitOnForwardFailure=yes", "-o", "BatchMode=yes",
                          "-o", "ConnectTimeout=10", "-o", "ServerAliveInterval=15", "-o", "ServerAliveCountMax=3",
                          "-o", "ControlMaster=no", "-o", "ControlPath=none", TARGET], ssh)

    def test_a_tuned_uri_is_not_tuned_twice_and_the_ssh_target_is_the_environments(self) -> None:
        done = self.run_tool(env=self.daemon_env({
            "KINOWO_MIRROR_DBS": "", "FAKE_LISTING": "", "KINOWO_MONGO_SSH": "root@override",
            "MONGODB_URI": "mongodb://h/kinowo?compressors=snappy&serverSelectionTimeoutMS=500"}))
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(f"mongodb://127.0.0.1:{self.tunnel_port}/kinowo?compressors=snappy&serverSelectionTimeoutMS=500"
                         "&directConnection=true", self.mongosh_calls("list")[0]["uri"])
        self.assertEqual("root@override", self.fakes.calls("ssh")[0][-1])

    def test_a_port_another_process_holds_with_a_dead_backend_is_reported_never_bound_over(self) -> None:
        holder = listening()
        self.held.append(holder)
        self.tunnel_port = int(holder.getsockname()[1])
        self.write("ping-fails", "")
        self.start()
        self.until_said(f":{self.tunnel_port} is held by another process but prod does NOT answer through it.")
        self.until_said("waiting for local Mongo + tunnel")
        os.kill(self.daemon.pid if self.daemon else 0, signal.SIGTERM)
        self.assertEqual(143, self.ended())
        self.assertEqual([], self.ssh_pids())

    def test_an_ssh_that_exits_at_once_is_reported_with_its_code_and_retried_later(self) -> None:
        self.start(FAKE_SSH_EXIT="255")
        self.until_said(f"ssh to {TARGET} exited (code 255) before prod answered — 255 is a connection or login failure")
        self.until_said("tunnel keeper: attempt 1 failed — next try in 2s")


class DatabasesTest(MirrorCase):
    def test_configured_databases_are_mirrored_comma_separated_without_a_listing(self) -> None:
        self.start(KINOWO_MIRROR_DBS="kinowo,kinowo_uk")
        self.until_said("2 tailer(s) running")
        self.until_tailing(2)
        self.assertEqual(1, len(self.lines("[mirror] mirroring: kinowo kinowo_uk")), self.output())
        self.assertEqual([], self.mongosh_calls("list"))
        self.assertEqual({"kinowo", "kinowo_uk"}, {str(c["db"]) for c in self.mongosh_calls("tail")})

    def test_unconfigured_the_tunnels_kinowo_databases_are_discovered_from_the_listings_last_line(self) -> None:
        self.start(KINOWO_MIRROR_DBS="", FAKE_LISTING="Current Mongosh Log ID: x\nkinowo kinowo_uk kinowo_de")
        self.until_said("[mirror] mirroring: kinowo kinowo_uk kinowo_de")
        self.until_said("3 tailer(s) running")


class UpkeepTest(MirrorCase):
    def spawn(self) -> int:
        child = subprocess.Popen(["/bin/sleep", "3600"], text=True)
        self.spawned.append(child)
        return child.pid

    def test_the_sweep_stops_orphaned_mirror_trees_and_prod_forwards_and_nothing_live_or_foreign(self) -> None:
        p = {name: self.spawn() for name in ("service", "live", "live_sub", "live_tailer", "live_ssh", "orphan",
                                              "orphan_tailer", "orphan_child", "tunnel", "foreign", "unranged")}
        tun = ("-o ExitOnForwardFailure=yes -o BatchMode=yes -o ConnectTimeout=10 -o ServerAliveInterval=15 "
               "-o ServerAliveCountMax=3 root@178.105.221.61")
        mirror = "/bin/bash /Users/pawel/projects/movies/scripts/local-mirror/mirror.sh"
        self.write("ps-table", f"""\
  {p['service']}     1 /bin/bash /Users/pawel/projects/movies/scripts/local-mirror/service.sh agent
 {p['live']}  {p['service']} {mirror}
 {p['live_sub']} {p['live']} {mirror}
 {p['live_tailer']} {p['live_sub']} mongosh
 {p['live_ssh']} {p['live']} ssh -N -L 27454:127.0.0.1:27017 {tun}
 {p['orphan']}     1 {mirror}
 {p['orphan_tailer']} {p['orphan']} mongosh
 {p['orphan_child']} {p['orphan_tailer']} sleep 30
 {p['tunnel']}     1 ssh -N -L 27607:127.0.0.1:27017 {tun}
 {p['foreign']}     1 ssh -N -L 8080:127.0.0.1:80 somebody@example.org
 {p['unranged']}     1 ssh -N -L 27017:127.0.0.1:27017 {tun}
""")
        self.start()
        self.until_said("sweep: stopping pid", 4)
        swept = {"orphan", "orphan_tailer", "orphan_child", "tunnel"}
        for name in swept:
            self.until(f"{name} was not stopped", lambda: self.spawned[list(p).index(name)].poll() is not None)
        self.assertEqual(set(), {name for name in p if name not in swept and self.spawned[list(p).index(name)].poll()
                                 is not None})
        self.assertEqual(4, len(self.lines("sweep: stopping pid")), self.output())
        self.assertEqual(1, len(self.lines(f"sweep: stopping pid {p['tunnel']} (orphaned prod tunnel): ssh -N -L 27607")))
        self.assertEqual(1, len(self.lines(f"sweep: stopping pid {p['orphan']} (orphaned mirror.sh): {mirror}")))

    def test_a_log_past_its_cap_is_trimmed_in_place_bsd_stat_or_not(self) -> None:
        self.fakes.script("stat", 'case "$1" in -f*) echo "stat: cannot read file system information" >&2; exit 1 ;; '
                                  'esac\nexec /usr/bin/stat "$@"')
        log = self.write("home/Library/Logs/kinowo-local-mirror.log",
                         "".join(f"line {n:07d}\n" for n in range(1, 700001)))
        inode = os.stat(log).st_ino
        self.start()
        self.until_said("log passed 8MB — trimmed to its last 2000 lines")
        with open(log) as f:
            kept = f.read().splitlines()
        self.assertEqual((2000, "line 0698001", "line 0700000"), (len(kept), kept[0], kept[-1]))
        self.assertEqual(inode, os.stat(log).st_ino)

    def test_a_seed_waits_for_its_governor_ticket_then_runs_holding_it(self) -> None:
        governor = self.fakes.program("agentctl", GOVERNOR)
        self.start("--reseed", KINOWO_MIRROR_DBS="kinowo_pl", KINOWO_GOVERNOR="1", KINOWO_GOVERNOR_BIN=governor,
                   KINOWO_JVM_GATE_OS="Darwin", AGENTCTL_GOV_DIR=self.path("gov"), GOV_HOLD=self.path("admit"))
        self.until("the seed never asked the governor", lambda: os.path.exists(self.path("admit.waiting")))
        self.assertEqual([], self.mongosh_calls("seed"))
        self.write("admit", "")
        self.until_tailing()
        [seed] = self.mongosh_calls("seed")
        self.assertEqual((True, "kinowo_pl"), (seed["ticket"], seed["db"]))
        admits = [" ".join(c) for c in self.fakes.calls("agentctl") if c[:2] == ["gov", "admit"]]
        self.assertEqual(1, len([a for a in admits if re.search(
            r"--mem 1 --label mirror seed kinowo_pl .*--job mirror-seed --cores 1$", a)]), admits)


class CodeWatchTest(MirrorCase):
    """Against a copy of the daemon's tree, so a case can edit the files it runs."""

    def setUp(self) -> None:
        super().setUp()
        tree = self.path("tree")
        for relative in ("scripts/local-mirror", "scripts/lib"):
            shutil.copytree(os.path.join(cli_harness.ROOT, relative), os.path.join(tree, relative),
                            ignore=shutil.ignore_patterns("__pycache__"))
        for library in glob.glob(os.path.join(cli_harness.ROOT, "scripts", "*.sh")):
            shutil.copy2(library, os.path.join(tree, "scripts"))
        listing = "common/src/main/resources/debug-mirror-collections.json"
        os.makedirs(os.path.dirname(os.path.join(tree, listing)))
        shutil.copy2(os.path.join(cli_harness.ROOT, listing), os.path.join(tree, listing))
        self.tree_tool = os.path.join(tree, os.path.relpath(TOOL, cli_harness.ROOT))
        self.tree_here = os.path.join(tree, "scripts", "local-mirror")

    def digests(self) -> int:
        return len([c for c in self.fakes.calls("node") if c[-1].endswith("targets-fingerprint.js")])

    def append(self, name: str) -> None:
        with open(os.path.join(self.tree_here, name), "a") as f:
            f.write("// an edit\n")

    def test_an_edit_to_a_spec_restarts_nothing_and_one_to_code_it_runs_restarts_the_daemon(self) -> None:
        self.start(tool=self.tree_tool)
        self.until_tailing()
        self.until_said("watching mirror-targets.js for collection changes")
        self.append("staleness-rule-spec.js")
        polled = self.digests()
        self.until("no two polls after the spec edit", lambda: self.digests() >= polled + 2)
        assert self.daemon is not None
        self.assertIsNone(self.daemon.poll(), self.output())
        self.append("tail.js")
        self.until_said("sync scripts changed on disk — exiting so the supervisor restarts on them")
        self.assertEqual(143, self.ended())

    def test_an_unreadable_collection_list_publishes_nothing_never_a_digest(self) -> None:
        with open(os.path.join(self.path("tree"), "common/src/main/resources/debug-mirror-collections.json"), "w") as f:
            f.write('{"collections": [')
        self.start(tool=self.tree_tool)
        self.until_tailing()
        [published] = glob.glob(self.path("tmp", "kinowo-mirror-targets.*"))
        with open(published) as f:
            self.assertEqual("", f.read().strip())


if __name__ == "__main__":
    unittest.main()
