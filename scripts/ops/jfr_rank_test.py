#!/usr/bin/env python3
"""jfr_rank.py over `jfr print` text in the shape the JDK prints it.
Run: python3 scripts/ops/jfr_rank_test.py"""
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import jfr_rank  # noqa: E402


def event(thread, *frames):
    body = "".join(f"\n    {f}(Arg) line: 1" for f in frames)
    return f"""jdk.ExecutionSample {{
  startTime = 21:53:46.238 (2026-10-03)
  sampledThread = "{thread}" (javaThreadId = 3)
  state = "STATE_RUNNABLE"
  stackTrace = [{body}
  ]
}}
"""


TEXT = (event("pool-1-thread-7", "java.util.HashMap.get", "services.identity.Agreement.take", "modules.WorkerMain.main")
        + event("pool-1-thread-12", "services.identity.Agreement.take", "services.identity.Agreement.take", "tools.Pool.run")
        + event("main", "java.lang.String.hashCode", "java.util.HashMap.get"))


class JfrRankTest(unittest.TestCase):
    def setUp(self):
        self.events = jfr_rank.samples(TEXT)

    def test_each_sample_is_its_thread_and_frames_top_first(self):
        self.assertEqual([(t, f) for t, f, _ in self.events], [
            ("pool-1-thread-7", ["java.util.HashMap.get", "services.identity.Agreement.take", "modules.WorkerMain.main"]),
            ("pool-1-thread-12", ["services.identity.Agreement.take", "services.identity.Agreement.take", "tools.Pool.run"]),
            ("main", ["java.lang.String.hashCode", "java.util.HashMap.get"]),
        ])

    def test_a_pools_threads_add_up_and_our_frames_rank_first_and_inclusively(self):
        ranks, n = jfr_rank.rank(self.events, jfr_rank.APP_PACKAGES.split(","))
        self.assertEqual(n, 3)
        self.assertEqual(ranks["by thread"], {"pool-N-thread-N": 2, "main": 1})
        self.assertEqual(ranks["top frame"]["java.util.HashMap.get"], 1)
        self.assertEqual(ranks["first app frame"], {"services.identity.Agreement.take": 2})
        # a recursive frame counts once per sample; a sample with none of our code adds nothing
        self.assertEqual(ranks["inclusive app"], {"services.identity.Agreement.take": 2, "modules.WorkerMain.main": 1, "tools.Pool.run": 1})

    def test_focus_keeps_only_the_samples_naming_it(self):
        ranks, n = jfr_rank.rank(self.events, ["services"], focus="tools.Pool")
        self.assertEqual(n, 1)
        self.assertEqual(ranks["first app frame"], {"services.identity.Agreement.take": 1})

    def test_the_app_packages_are_a_parameter(self):
        ranks, _ = jfr_rank.rank(self.events, ["java"])
        self.assertEqual(ranks["first app frame"], {"java.util.HashMap.get": 1, "java.lang.String.hashCode": 1})


if __name__ == "__main__":
    unittest.main()
