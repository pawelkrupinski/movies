"""scripts/sbt-warm, black-box (ported from sbt-warm-test.sh): a fresh checkout is seeded from the newest local build of a
main commit (never a branch's, never its own), the commands reach this checkout's server, the shared build cache takes
only clean builds of main, a changed build or other KINOWO_* settings restart or reload the server, and --stop, --reap
and --status act on this checkout's own server only. sbt is a stub (sbt_warm_fixture.py).
Run: python3 scripts/sbt_warm_test.py"""
from __future__ import annotations

import fcntl
import glob
import os
import re
import subprocess
import unittest

from sbt_warm_fixture import ROOT, SbtWarmCase, checkout_key, lock_state, state_word
import cli_harness


class SeedAndCommandsTest(SbtWarmCase):
    def test_a_fresh_checkout_is_seeded_and_its_commands_reach_its_own_server(self) -> None:
        fresh = self.worktree("fresh")
        done = self.warm(fresh, "worker/testOnly X", "web/compile")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("newest", self.text(fresh + "/worker/target/scala-3.x/zinc/from"), done.output)
        self.assertTrue(os.path.isdir(fresh + "/project/target"))
        self.assertEqual(f"{fresh}|--client worker/testOnly X; web/compile", self.calls()[-1])
        self.assertEqual(fresh, self.text(glob.glob(self.state + "/*.root")[0]))
        # 2026-10-14: the seed copied active.json, so sbt --client ran in the seed source's live server
        self.put(fresh + "/worker/src/s", "edited\n")
        self.warm(fresh, "worker/test")
        self.assertEqual(f"{fresh}|edited", self.text(self.path("sbt-sources")).splitlines()[-1])
        done = self.warm(fresh, "worker/test")
        self.assertIn(f"checkout {fresh}, server ", done.stderr)

    def test_an_it_run_without_mongodb_is_refused_naming_it_local(self) -> None:
        fresh = self.worktree("fresh")
        self.warm(fresh, "compile")
        before = len(self.calls())
        done = self.warm(fresh, "itAll")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("scripts/it-local.sh 'itAll'", done.stderr)
        self.assertEqual(before, len(self.calls()))
        self.assertEqual(2, self.warm(fresh, "worker/IntegrationTest/testOnly X").code)
        self.warm(fresh, "IntegrationTest/compile")
        self.assertEqual("--client IntegrationTest/compile", self.last_call())
        self.warm(fresh, "itAll", env={"MONGODB_URI": "mongodb://x"})
        self.assertEqual("--client itAll", self.last_call())

    def test_a_test_command_fetches_missing_gitops_manifests_once(self) -> None:
        fresh = self.worktree("fresh")
        fetch = self.write("fresh/infra/bin/fetch-gitops",
                           '#!/bin/sh\necho fetched >> "%s/gitops-calls"; mkdir -p "$KINOWO_GITOPS_DEST"\n' % self.work)
        os.chmod(fetch, 0o755)
        self.warm(fresh, "compile")
        self.assertEqual("", self.text(self.path("gitops-calls")))
        self.warm(fresh, "testUnit")
        self.warm(fresh, "worker/testOnly deploy.X")
        self.assertEqual(["fetched"], self.text(self.path("gitops-calls")).splitlines())
        self.assertTrue(os.path.isdir(fresh + "/infra/kubernetes"))
        self.assertEqual("--client worker/testOnly deploy.X", self.last_call())

    def test_a_built_checkout_is_never_seeded_over(self) -> None:
        fresh = self.worktree("fresh")
        self.warm(fresh, "compile")
        self.put(fresh + "/worker/target/scala-3.x/zinc/from", "stale\n")
        self.warm(fresh, "compile")
        self.assertEqual("stale", self.text(fresh + "/worker/target/scala-3.x/zinc/from"))
        # 2026-10-19: e2e has test sources only, its Zinc state test-zinc alone: counted unbuilt, it was re-seeded
        os.makedirs(fresh + "/e2e/src")
        self.put(fresh + "/e2e/target/scala-3.x/test-zinc/from", "mine\n")
        os.makedirs(self.newest + "/e2e/target/scala-3.x/test-zinc")
        self.warm(fresh, "compile")
        self.assertEqual("mine", self.text(fresh + "/e2e/target/scala-3.x/test-zinc/from"))


class SeedSourcesTest(SbtWarmCase):
    def test_the_seed_is_the_nearest_build_head_contains_or_the_one_named(self) -> None:
        other = self.worktree("other")
        self.warm(other, "--seed", env={"KINOWO_SBT_WARM_SEED": self.path("old")})
        self.assertEqual("old", self.text(other + "/worker/target/scala-3.x/zinc/from"))
        # 2026-10-18: local main ran ahead of origin/main; the branch's own build is the nearest its HEAD contains
        past = self.worktree("past", "ahead")
        self.write("past/a", "a\nb\nc\nd\n")
        self.git(past, "commit", "-qam", "four")
        done = self.warm(past, "--seed")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("branch", self.text(past + "/worker/target/scala-3.x/zinc/from"))
        # a module new since the checkout was seeded is seeded on its own; the modules built stay
        self.put(self.newest + "/identity/target/scala-3.x/zinc/from", "newest\n")
        os.makedirs(other + "/identity/src")
        self.warm(other, "compile")
        self.assertEqual("newest old", self.text(other + "/identity/target/scala-3.x/zinc/from") + " " +
                         self.text(other + "/worker/target/scala-3.x/zinc/from"))

    def test_a_seed_drops_class_files_its_sources_analysis_never_produced(self) -> None:
        polluted = self.path("polluted")
        subprocess.run(["bash", "-c", '. "$1"; scratch_zinc_build "$2" worker tools/Kept.class', "_",
                        os.path.join(ROOT, "scripts/scratch-zinc.sh"), polluted], check=True)
        classes = polluted + "/worker/target/scala-3.x/classes/"
        for name, text in (("tools/Orphan.class", "orphan"), ("tools/Orphan.tasty", "orphan"), ("logback.xml", "r")):
            self.put(classes + name, text + "\n")
        clean = self.worktree("clean")
        self.warm(clean, "--seed", env={"KINOWO_SBT_WARM_SEED": polluted})
        seeded = clean + "/worker/target/scala-3.x/classes/"
        self.assertEqual(["Kept.class", "Kept.tasty"], sorted(os.listdir(seeded + "tools")))
        self.assertTrue(os.path.exists(seeded + "logback.xml"))

    def test_a_seed_leaves_no_active_json_naming_the_sources_server(self) -> None:
        idle = self.worktree("idle")
        self.warm(idle, "--seed")
        self.assertFalse(os.path.exists(idle + "/project/target/active.json"))


class SharedBuildCacheTest(SbtWarmCase):
    def setUp(self) -> None:
        super().setUp()
        self.extra["KINOWO_SBT_CACHE_PUBLISH"] = "1"
        self.pub = self.worktree("pub")
        self.build(self.pub, "pub")
        os.makedirs(self.pub + "/web/target/scala-3.x/zinc")
        self.entry = os.path.join(self.cache, self.git(self.main, "rev-parse", "origin/main"))

    def entry_says(self) -> str:
        return self.text(self.entry + "/worker/target/scala-3.x/zinc/from") + " " + self.text(self.entry + "/.zincs")

    def test_only_a_passing_call_in_a_clean_checkout_of_main_publishes(self) -> None:
        self.assertEqual(0, self.warm(self.pub, "compile").code)
        self.assertEqual("pub 2", self.entry_says())
        self.assertFalse(os.path.exists(self.entry + "/project/target/active.json"))
        self.assertEqual([], glob.glob(self.cache + "/*.*"))
        self.put(self.pub + "/worker/target/scala-3.x/zinc/from", "pubbed\n")
        self.warm(self.pub, "compile")
        self.assertEqual("pub 2", self.entry_says(), "no more configurations: not replaced")
        os.makedirs(self.pub + "/identity/target/scala-3.x/zinc")
        self.write("pub/a", "edited\n")
        self.warm(self.pub, "compile")
        self.assertEqual("pub 2", self.entry_says(), "a changed tracked file publishes nothing")
        self.git(self.pub, "checkout", "-q", "--", "a")
        self.assertEqual(1, self.warm(self.pub, "compile", env={"STUB_FAIL": "1"}).code)
        self.assertEqual("pub 2", self.entry_says(), "a failed command publishes nothing")
        self.warm(self.pub, "compile")
        self.assertEqual("pubbed 3", self.entry_says())
        self.assertEqual(1, len(glob.glob(self.cache + "/*.retired-*")))
        self.warm(self.ahead, "compile")
        self.assertFalse(os.path.isdir(os.path.join(self.cache, self.git(self.ahead, "rev-parse", "HEAD"))))

    def test_a_seed_prefers_the_entry_and_publish_waits_for_an_idle_server(self) -> None:
        self.warm(self.pub, "compile")
        self.put(self.pub + "/worker/target/scala-3.x/zinc/from", "pubbed\n")
        os.makedirs(self.pub + "/identity/target/scala-3.x/zinc")
        self.warm(self.pub, "compile")
        fromcache = self.worktree("fromcache")
        self.warm(fromcache, "--seed")
        self.assertEqual("pubbed", self.text(fromcache + "/worker/target/scala-3.x/zinc/from"))
        for module in ("scrapers", "geodata", "common", "identity"):
            os.makedirs(fromcache + f"/{module}/target/scala-3.x/zinc", exist_ok=True)
        os.makedirs(self.state, exist_ok=True)
        with open(self.state_file(fromcache, ".busy"), "a") as busy:
            fcntl.flock(busy, fcntl.LOCK_SH)
            done = self.warm(fromcache, "--publish")
            self.assertIn("nothing published", done.stderr)
            self.assertEqual("3", self.text(self.entry + "/.zincs"))
        done = self.warm(fromcache, "--publish")
        self.assertEqual("5", self.text(self.entry + "/.zincs"), done.output)

    def test_an_entry_retired_twice_in_one_second_is_still_replaced(self) -> None:
        """ci-runner, 2026-11-27: two publishes in one second retired the entry onto the first's retired copy, a
        rename onto a full directory, and the newer build was never published."""
        self.warm(self.pub, "compile")
        os.makedirs(self.pub + "/identity/target/scala-3.x/zinc")
        now = int(os.stat(self.write("clock-mark", "")).st_mtime)  # this second, read off the disk
        for second in range(now - 1, now + 30):
            os.makedirs(f"{self.entry}.retired-{second}/x", exist_ok=True)
        done = self.warm(self.pub, "compile")
        self.assertEqual("3", self.text(self.entry + "/.zincs"), done.output)

    def test_a_stale_tasty_read_is_recovered_through_sbt_guard(self) -> None:
        done = self.warm(self.pub, "worker/compile", env={"STUB_STALE_ONCE": self.path("stale-once")})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["--client worker/compile", "--client staleTastyDrop", "--client worker/compile"],
                         [c.split("|", 1)[1] for c in self.calls()])


class ServerSettingsTest(SbtWarmCase):
    def test_a_changed_build_is_reloaded_first_once(self) -> None:
        other = self.worktree("other")
        server = self.fake_server(other)
        self.warm(other, "compile")
        self.put(other + "/build.sbt", "lazy val x = 1\n")
        self.warm(other, "compile")
        self.assertEqual("--client reload; compile", self.last_call())
        self.warm(other, "compile")
        self.assertEqual("--client compile", self.last_call())
        self.end_server(server)

    def test_other_kinowo_settings_restart_the_server_but_the_tool_knobs_do_not(self) -> None:
        other = self.worktree("other")
        self.warm(other, "compile")  # a fresh server's: the settings it starts with recorded
        server = self.fake_server(other)
        self.warm(other, "compile")
        self.assertEqual("running", state_word(server))
        # land-precheck's KINOWO_GOV_CLASS=check restarted the agent's warm server on every land (2026-11-22)
        self.warm(other, "compile", env={"KINOWO_GOV_CLASS": "check", "KINOWO_GOV_CORES": "4"})
        self.assertFalse(self.gone(server), "the governor's knobs restarted the server")
        done = self.warm(other, "compile", env={"KINOWO_IDENTITY_CORPUS_DIR": "/corpora"})
        self.wait_gone(server, "the restarted server")
        self.assertIn("settings differ", done.stderr)
        line = re.compile(r"^[0-9-]{10} [0-9:]{8} server pid=%d checkout=%s why=\"this call's KINOWO_.* settings "
                          r"differ.*\" by=[0-9]+:.*sbt-warm compile" % (server, re.escape(other)), re.M)
        self.assertEqual(1, len(line.findall(self.stops())), self.stops())

    def test_a_settings_restart_waits_for_another_calls_running_command(self) -> None:
        other = self.worktree("other")
        corpora = {"KINOWO_IDENTITY_CORPUS_DIR": "/corpora"}
        self.warm(other, "compile", env=corpora)  # a fresh server's: started with these settings
        server = self.fake_server(other)
        os.mkfifo(self.path("slow-go"))
        slow = self.start(other, "test", env={**corpora, "STUB_SLOW_SERVER": str(server)})
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(self.path("slow-started"))))
        restart = self.start(other, "compile", stderr=self.path("restart.err"))
        self.assertTrue(cli_harness.wait_until(lambda: "waiting for it to end" in self.text(self.path("restart.err"))))
        with open(self.path("slow-go"), "w") as go:
            go.write("go\n")
        slow.wait()
        restart.wait()
        self.assertEqual("running", self.text(self.path("slow")))


class OneServerTest(SbtWarmCase):
    def test_a_seeding_call_leaves_a_live_servers_active_json_and_several_servers_are_stopped(self) -> None:
        other = self.worktree("other")
        self.warm(other, "--seed", env={"KINOWO_SBT_WARM_SEED": self.path("old")})
        os.makedirs(other + "/e2e/src")
        server = self.fake_server(other)
        self.put(other + "/project/target/active.json", '{"uri":"local://elsewhere/sock"}\n')
        self.warm(other, "compile")
        self.assertIn("local://elsewhere/sock", self.text(other + "/project/target/active.json"))
        second = self.fake_server(other)
        self.warm(other, "compile")
        self.wait_gone(server, "the first of two servers")
        self.wait_gone(second, "the second of two servers")


class StartLockTest(SbtWarmCase):
    def test_a_second_call_without_a_server_waits_for_the_first_ones_start(self) -> None:
        locked = self.worktree("locked")
        self.warm(locked, "--seed")
        self.stub('#!/usr/bin/env bash\necho "start $(pwd -P)" >> "@S@/spans"; sleep 1; echo "end $(pwd -P)" >> "@S@/spans"\n')
        first = self.start(locked, "compile")
        self.assertTrue(cli_harness.wait_until(lambda: self.text(self.path("spans")) != ""))
        second = self.start(locked, "compile")
        first.wait()
        second.wait()
        self.assertEqual("start end start end", " ".join(w.split()[0] for w in self.text(self.path("spans")).splitlines()))
        # a call killed while it held the start lock (kill -9: no clean-up ran) leaves it free at once
        holder = self.hold_lock(self.state_file(locked, ".flock"))
        holder.kill()
        holder.wait()
        done = self.warm(locked, "compile", env={"KINOWO_SBT_WARM_LOCK_SECONDS": "0"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("free", lock_state(self.state_file(locked, ".flock")))


class ReapTest(SbtWarmCase):
    def test_a_removed_checkouts_server_is_stopped_and_forgotten_never_a_bystander(self) -> None:
        fresh, other = self.worktree("fresh"), self.worktree("other")
        self.warm(fresh, "compile")
        server = self.fake_server(fresh)
        self.put(self.state_file(fresh, ".pid"), f"{server}\n")
        subprocess.run(["rm", "-rf", fresh], check=True)
        self.git(self.main, "worktree", "prune")
        self.warm(other, "compile")
        self.wait_gone(server, "the removed checkout's server")
        self.assertEqual([], [f for f in glob.glob(self.state + "/*.root") if self.text(f) == fresh])
        # a pid file outlives its process, and the pid comes back as someone else's
        bystander = self.background(["sleep", "300"])
        self.put(self.state + "/gone.root", self.path("gone") + "\n")
        self.put(self.state + "/gone.pid", f"{bystander.pid}\n")
        self.warm(other, "compile")
        self.assertIsNone(bystander.poll(), "a bystander was killed")


class StopAndStatusTest(SbtWarmCase):
    def test_stop_and_reap_stop_only_this_checkouts_own_server(self) -> None:
        idle = self.worktree("idle")
        theirs = self.fake_server(self.path("old"))
        self.warm(idle, "--seed")
        self.put(idle + "/project/target/active.json", '{"uri":"local://%s/sock"}\n' % self.path("old"))
        self.warm(idle, "--stop")
        self.put(self.state_file(idle, ".root"), idle + "\n")
        subprocess.run(["touch", "-t", "202001010000", self.state_file(idle, ".used")], check=True)
        status = self.warm(idle, "--status")
        self.assertEqual(0, status.code)
        self.assertRegex(status.stdout, r"kept: %s \(.*, last used 2020-01-01 00:00\)" % re.escape(idle))
        self.assertIn(f"this checkout ({idle}): no server", status.stdout)
        self.warm(idle, "--reap")
        self.assertNotIn("shutdown", self.text(self.path("sbt-calls")))
        self.assertFalse(self.gone(theirs))
        mine = self.fake_server(idle)
        self.assertIn(f"this checkout ({idle}): server pid {mine}", self.warm(idle, "--status").stdout)
        self.warm(idle, "--stop")
        self.wait_gone(mine, "this checkout's own server")
        self.assertFalse(self.gone(theirs))

    def test_stop_in_the_root_checkout_is_refused_unless_root_is_given(self) -> None:
        roots = self.fake_server(self.main)
        done = self.warm(self.main, "--stop")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("refusing --stop in the root checkout", done.stderr)
        self.assertFalse(self.gone(roots))
        self.assertEqual(0, self.warm(self.main, "--stop", "--root").code)
        self.wait_gone(roots, "the root checkout's server")
        self.assertIn(f"server pid={roots} checkout={self.main} why=\"--stop (SIGTERM)\"", self.stops())


class UsageTest(SbtWarmCase):
    def test_help_no_command_and_bad_flags(self) -> None:
        done = self.warm(self.main, "--help")
        self.assertEqual(0, done.code)
        self.assertTrue(done.stdout.startswith("Purpose: sbt for agents, warm"), done.stdout[:200])
        self.assertIn("scripts/sbt-warm --stop ", done.stdout)
        self.assertEqual(done.stdout, self.warm(self.main).stdout)
        for n, (args, said) in enumerate(((["--local"], "no command to run"), (["--serve"], "--serve needs the command it serves"),
                           (["--exclusive", "me"], "--exclusive needs who runs it and the command"),
                           (["--ci", "--serve", "x"], "--ci cannot serve"))):
            done = self.warm(self.worktree(f"usage{n}"), *args)
            self.assertEqual(2, done.code, done.output)
            self.assertIn(f"[sbt-warm] {said}", done.stderr)
        self.assertEqual(checkout_key(self.main), checkout_key(os.path.realpath(self.main)))


if __name__ == "__main__":
    unittest.main()
