"""scripts/sbt-warm, black-box (ported from sbt-warm-test.sh; sbt is a stub, sbt_warm_fixture.py):
the shared build cache takes only clean builds of main.
Run: python3 scripts/sbt_warm_cache_test.py
"""
from __future__ import annotations

import fcntl
import glob
import os
import unittest

from sbt_warm_fixture import SbtWarmCase


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


if __name__ == "__main__":
    unittest.main()
