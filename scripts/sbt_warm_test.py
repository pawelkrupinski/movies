"""scripts/sbt-warm, black-box (ported from sbt-warm-test.sh; sbt is a stub, sbt_warm_fixture.py):
a fresh checkout is seeded from the newest local build of a
main commit (never a branch's, never its own), and the commands reach this checkout's server.
Run: python3 scripts/sbt_warm_test.py
"""
from __future__ import annotations

import glob
import os
import subprocess
import unittest

from sbt_warm_fixture import ROOT, SbtWarmCase, checkout_key


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
