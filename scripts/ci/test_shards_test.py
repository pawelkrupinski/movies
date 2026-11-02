#!/usr/bin/env python3
"""test_shards.py: the n shards together run exactly the input's classes and lines, each once, each class under its
own line's command and arguments; one shard is the input; the split is deterministic and balanced by weight.
Run: python3 scripts/ci/test_shards_test.py"""
import os
import random
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import test_shards as t  # noqa: E402

TOOL = os.path.join(os.path.dirname(os.path.abspath(__file__)), "test_shards.py")


def runs(lines):
    """Every (command, class, args) a list of lines runs, and the lines it runs whole."""
    classes, whole = [], []
    for line in lines:
        p = t.parse(line)
        if p is None:
            whole.append(line)
        else:
            classes += [(p[0], c, p[2]) for c in p[1]]
    return classes, whole


class ShardsTest(unittest.TestCase):
    PLAN = [
        "worker/testOnly scripts.IdentityRefitMeasureAllSpec",
        "identity/testOnly services.identity.A services.identity.B services.identity.C -- -Didentity.countries=pl,de",
        "worker/testOnly scripts.IdentityDriftSlicesSpec services.identity.D tools.HardClusterRatchetSpec",
    ]

    def test_the_shards_together_run_every_class_once_under_its_own_line(self):
        for n in (1, 2, 3, 5):
            got = [x for k in range(n) for x in runs(t.shard(self.PLAN, k, n))[0]]
            self.assertEqual(sorted(got), sorted(runs(self.PLAN)[0]), n)

    def test_one_shard_is_the_input(self):
        self.assertEqual(t.shard(self.PLAN, 0, 1), self.PLAN)

    def test_a_line_naming_no_class_outright_goes_whole_to_one_shard(self):
        lines = ["identity/test", "testOnly services.identity.* services.titlerules.* scripts.* tools.HardClusterRatchetSpec",
                 "worker/testOnly deploy.* -deploy.AotCacheOptionsSpec", "Test/compile"] + self.PLAN
        for n in (2, 3, 4):
            shards = [t.shard(lines, k, n) for k in range(n)]
            whole = [x for s in shards for x in runs(s)[1]]
            self.assertEqual(sorted(whole), sorted(lines[:4]))
            self.assertEqual(sorted(x for s in shards for x in runs(s)[0]), sorted(runs(lines)[0]))

    def test_random_plans_split_exactly(self):
        rnd = random.Random(7)
        for _ in range(300):
            lines = []
            for m in range(rnd.randint(0, 4)):
                cls = [f"p{m}.S{j}" for j in rnd.sample(range(40), rnd.randint(1, 12))]
                args = rnd.choice(["", " -- -Didentity.countries=pl", " -- -oD"])
                lines.append(f"{rnd.choice(['worker/', 'identity/', ''])}testOnly {' '.join(cls)}{args}")
            if rnd.random() < 0.3:
                lines.insert(rnd.randint(0, len(lines)), rnd.choice(["identity/test", "testOnly services.identity.*"]))
            weights = {f"p0.S{j}": rnd.choice([1, 50, 300]) for j in range(40)}
            n = rnd.randint(1, 4)
            shards = [t.shard(lines, k, n, weights) for k in range(n)]
            self.assertEqual(sorted(x for s in shards for x in runs(s)[0]), sorted(runs(lines)[0]))
            self.assertEqual(sorted(x for s in shards for x in runs(s)[1]), sorted(runs(lines)[1]))
            self.assertEqual(shards, [t.shard(lines, k, n, weights) for k in range(n)])

    def test_the_heaviest_classes_land_on_different_shards(self):
        weights = {"scripts.IdentityDriftSlicesSpec": 500, "scripts.IdentityRefitMeasureAllSpec": 400,
                   "services.identity.A": 300}
        shards = [" ".join(t.shard(self.PLAN, k, 3, weights)) for k in range(3)]
        for heavy in weights:
            self.assertEqual(sum(heavy in s for s in shards), 1)
        self.assertTrue(all(sum(h in s for h in weights) == 1 for s in shards), shards)

    def test_the_cli_reads_its_weights_and_numbers_shards_from_one(self):
        with tempfile.NamedTemporaryFile("w", suffix=".tsv", delete=False) as fh:
            fh.write("# class seconds\nservices.identity.B\t900\n")
        try:
            out = [subprocess.run([sys.executable, TOOL, "--shard", f"{k}/2", "--weights", fh.name],
                                  input="\n".join(self.PLAN) + "\n", capture_output=True, text=True, check=True).stdout
                   for k in (1, 2)]
        finally:
            os.unlink(fh.name)
        self.assertEqual(out[0], "identity/testOnly services.identity.B -- -Didentity.countries=pl,de\n")
        self.assertNotIn("services.identity.B", out[1])
        bad = subprocess.run([sys.executable, TOOL, "--shard", "3/2"], input="", capture_output=True, text=True)
        self.assertNotEqual(bad.returncode, 0)


if __name__ == "__main__":
    unittest.main()
