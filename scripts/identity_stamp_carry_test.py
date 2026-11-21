#!/usr/bin/env python3
"""identity_stamp_carry.py: a pure move's rows are written and proven again; an unproven change writes none (its stamps
bump); a hand-written or forged row is refused, as is one whose pure-move claim the range does not bear out; a what-if
row holds only for the very head and base its verdict was recorded under, every country whole and nothing moved, and
never carries a venue slot; a range saying Identity-Design or a what-if over the model's data carries nothing."""

import hashlib
import json
import os
import re
import stat
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
TOOL = os.path.join(HERE, "identity_stamp_carry.py")
sys.path.insert(0, HERE)
import identity_stamp_carry as stamp_carry  # noqa: E402
from pure_move_check_test import CLEAN_ENV, FOO, FOO_PATH, NEW_FOO_PATH, SIBLING, USER, Repo  # noqa: E402

TABLE = "test/resources/identity-stamp-carry.tsv"
STAMPS = ("identity-rules", "venue-slot", "agreement-rules")
COUNTRIES = ("pl", "de", "uk", "es", "us")

# The build's stamps, stood in for: each stamp a digest of the commit's CODE (the tree of common/src/main, as the real build
# digests the code it versions), read from a map the test writes of every commit -- what a row must match is the same, a
# digest the tool makes itself from the commit, never one a row names.
FAKE_STAMPS = """#!/usr/bin/env python3
import json, os, sys
for stamp, digest in json.load(open(os.environ["KINOWO_FAKE_STAMPS_MAP"]))[sys.argv[2]].items():
    print(stamp + "\\t" + digest)
"""


def fake_stamp(repo: Repo, sha: str, stamp: str) -> str:
    code = repo.git("rev-parse", f"{sha}:common/src/main").strip()
    return hashlib.sha256((stamp + code).encode()).hexdigest()


class StampCarryTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = os.path.join(self.tmp.name, "repo")
        os.makedirs(root)
        self.repo = Repo(root)
        self.cache = os.path.join(self.tmp.name, "cache")
        self.maker = os.path.join(self.tmp.name, "stamps")
        self.stamps_map = os.path.join(self.tmp.name, "stamps.json")
        with open(self.maker, "w") as f:
            f.write(FAKE_STAMPS)
        os.chmod(self.maker, os.stat(self.maker).st_mode | stat.S_IEXEC)
        r = self.repo
        r.write(FOO_PATH, FOO)
        r.write("common/src/main/scala/services/users/User.scala", USER)
        r.write("common/src/main/scala/services/old/Sibling.scala", SIBLING)
        r.write(TABLE, "# stamp\tdigest\tequals\tproof\tbase\thead\n")
        self.base = r.commit("base")

    def tearDown(self):
        self.tmp.cleanup()

    def run_tool(self, *args: str, cache: str | None = None) -> tuple[int, str]:
        shas = set(self.repo.git("rev-list", "--all", "--reflog").split())
        with open(self.stamps_map, "w") as f:
            json.dump({sha: {stamp: fake_stamp(self.repo, sha, stamp) for stamp in STAMPS} for sha in shas}, f)
        env = dict(CLEAN_ENV, KINOWO_STAMP_CARRY_STAMPS=self.maker, KINOWO_IDENTITY_CACHE=cache or self.cache,
                   KINOWO_FAKE_STAMPS_MAP=self.stamps_map,
                   # `auto` commits its rows: an identity of its own, as a container with no git config has none
                   GIT_AUTHOR_NAME="Spec", GIT_AUTHOR_EMAIL="spec@example.com",
                   GIT_COMMITTER_NAME="Spec", GIT_COMMITTER_EMAIL="spec@example.com")
        done = subprocess.run([sys.executable, "-I", TOOL, *args], cwd=self.repo.root, capture_output=True, text=True, env=env)
        return done.returncode, done.stdout + done.stderr

    def move_foo(self) -> str:
        r = self.repo
        r.move(FOO_PATH, NEW_FOO_PATH)
        r.write(NEW_FOO_PATH, FOO.replace("package services.old", "package models").replace("private[old]", "private[models]"))
        r.write("common/src/main/scala/services/users/User.scala",
                USER.replace("import services.old.Foo", "import models.Foo").replace("services.old.Foo(1)", "models.Foo(1)"))
        r.write("common/src/main/scala/services/old/Sibling.scala", SIBLING.replace("object Sibling", "import models.Foo\n\nobject Sibling"))
        return r.commit("move Foo to models")

    def change_foo(self, message: str = "change Foo") -> str:
        self.repo.write(FOO_PATH, FOO.replace("value * 2", "value * 3"))
        return self.repo.commit(message)

    def table_rows(self) -> list[list[str]]:
        return [line.split("\t") for line in self.repo.read(TABLE).splitlines() if line and not line.startswith("#")]

    def record_what_if(self, head: str, base: str, moved: str = "0", mode: str = "whole,final", countries=COUNTRIES):
        path = os.path.join(self.cache, "what-if-verdicts", f"{head}-{base}.tsv")
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "a") as f:
            for cc in countries:
                f.write(f"{cc}\tdone\t2026-11-20T10:00:00Z\t/tmp/takes-diff-{cc}.tsv\t{moved}\t{mode}\n")

    def land_rows(self) -> tuple[str, str]:
        """The table's rows committed on their own, as a carry lands: the range the hook verifies."""
        before = self.repo.git("rev-parse", "HEAD").strip()
        return before, self.repo.commit("carry the identity stamps")

    # ── a pure move ──────────────────────────────────────────────────────────────────────────────────────────────────

    def test_a_pure_move_keeps_every_stamp_and_its_rows_are_proven_again(self):
        head = self.move_foo()
        code, out = self.run_tool("propose", f"{self.base}..{head}")
        self.assertEqual(0, code, out)
        rows = self.table_rows()
        self.assertEqual(sorted(STAMPS), sorted(r[0] for r in rows))
        for stamp, digest, equals, proof, base, head_sha in rows:
            self.assertEqual((fake_stamp(self.repo, head, stamp), fake_stamp(self.repo, self.base, stamp)), (digest, equals))
            self.assertEqual(("pure-move", self.base, head), (proof, base, head_sha))
        before, after = self.land_rows()
        code, out = self.run_tool("verify", f"{before}..{after}")
        self.assertEqual(0, code, out)
        self.assertEqual(3, out.count(" proven"), out)

    def test_an_unproven_change_writes_no_row_so_its_stamps_bump(self):
        head = self.change_foo()
        code, out = self.run_tool("propose", f"{self.base}..{head}")
        self.assertEqual(1, code, out)
        self.assertIn("REFUSED: no proof holds", out)
        self.assertIn("pure-move-check does not call", out)
        self.assertIn("pl: no identity-what-if verdict recorded", out)
        self.assertEqual([], self.table_rows())

    # ── the hook refuses what no proof bears out ─────────────────────────────────────────────────────────────────────

    def test_a_hand_written_row_with_forged_digests_is_refused(self):
        head = self.move_foo()
        self.repo.write(TABLE, self.repo.read(TABLE) + "\t".join(
            ("identity-rules", "a" * 64, fake_stamp(self.repo, self.base, "identity-rules"), "pure-move", self.base, head)) + "\n")
        before, after = self.land_rows()
        code, out = self.run_tool("verify", f"{before}..{after}")
        self.assertEqual(1, code, out)
        self.assertIn(f"digest is not {head[:12]}'s", out)

    def test_a_pure_move_claim_the_range_does_not_bear_out_is_refused(self):
        head = self.change_foo()
        self.repo.write(TABLE, self.repo.read(TABLE) + "\t".join(
            ("identity-rules", fake_stamp(self.repo, head, "identity-rules"), fake_stamp(self.repo, self.base, "identity-rules"),
             "pure-move", self.base, head)) + "\n")
        before, after = self.land_rows()
        code, out = self.run_tool("verify", f"{before}..{after}")
        self.assertEqual(1, code, out)
        self.assertIn("pure-move-check does not call", out)

    def test_a_row_edited_in_place_is_proven_as_a_new_row(self):
        head = self.move_foo()
        self.assertEqual(0, self.run_tool("propose", f"{self.base}..{head}")[0])
        _, landed = self.land_rows()
        self.repo.write(TABLE, self.repo.read(TABLE).replace(fake_stamp(self.repo, self.base, "venue-slot"), "b" * 64))
        _, edited = self.land_rows()
        code, out = self.run_tool("verify", f"{landed}..{edited}")
        self.assertEqual(1, code, out)
        self.assertIn(f"equals is not {self.base[:12]}'s", out)

    def test_a_row_taken_out_needs_no_proof(self):
        head = self.move_foo()
        self.assertEqual(0, self.run_tool("propose", f"{self.base}..{head}")[0])
        _, landed = self.land_rows()
        self.repo.write(TABLE, "\n".join(l for l in self.repo.read(TABLE).splitlines() if not l.startswith("venue-slot")) + "\n")
        _, removed = self.land_rows()
        self.assertEqual(0, self.run_tool("verify", f"{landed}..{removed}")[0])

    # ── a what-if ────────────────────────────────────────────────────────────────────────────────────────────────────

    def test_a_whole_what_if_with_nothing_moved_carries_the_rules_but_no_venue_slot(self):
        head = self.change_foo()
        self.record_what_if(head, self.base)
        code, out = self.run_tool("propose", f"{self.base}..{head}", "--proof", "what-if")
        self.assertEqual(0, code, out)
        self.assertEqual(["agreement-rules", "identity-rules"], sorted(r[0] for r in self.table_rows()))
        self.assertIn("venue-slot: a what-if proof does not carry it: it bumps", out)
        before, after = self.land_rows()
        self.assertEqual(0, self.run_tool("verify", f"{before}..{after}")[0])

    def test_a_what_if_row_is_proven_where_no_laptop_cache_is_as_on_ci_runner(self):
        head = self.change_foo()
        self.record_what_if(head, self.base)
        self.assertEqual(0, self.run_tool("propose", f"{self.base}..{head}", "--proof", "what-if")[0])
        before, after = self.land_rows()
        empty = os.path.join(self.tmp.name, "ci-runner-cache")       # ci-runner: no what-if was ever run there
        code, out = self.run_tool("verify", f"{before}..{after}", cache=empty)
        self.assertEqual(0, code, out)
        self.assertIn("identity-rules", out)

    def test_a_committed_what_if_verdict_with_a_take_moved_is_refused(self):
        head = self.change_foo()
        self.record_what_if(head, self.base, moved="3")               # the what-if itself saw takes move
        self.repo.write(TABLE, "# stamp\n" + "\t".join(
            ("identity-rules", fake_stamp(self.repo, head, "identity-rules"), fake_stamp(self.repo, self.base, "identity-rules"),
             "what-if", self.base, head)) + "\n")
        verdict = f"test/resources/identity-what-if-verdicts/{head}-{self.base}.tsv"
        self.repo.write(verdict, "".join(f"{cc}\tdone\t2026-11-20T10:00:00Z\t-\t3\twhole,final\n" for cc in COUNTRIES))
        before, after = self.land_rows()
        code, out = self.run_tool("verify", f"{before}..{after}", cache=os.path.join(self.tmp.name, "none"))
        self.assertEqual(1, code, out)
        self.assertIn("3 take(s) moved", out)

    def test_a_what_if_verdict_for_another_base_or_head_is_refused(self):
        head = self.change_foo()
        later = self.change_foo_again()
        self.record_what_if(head, self.base)                      # the verdict: head over base
        for base, claimed_head in ((head, later), (self.base, later)):
            self.repo.write(TABLE, "# stamp\n" + "\t".join(
                ("identity-rules", fake_stamp(self.repo, claimed_head, "identity-rules"),
                 fake_stamp(self.repo, base, "identity-rules"), "what-if", base, claimed_head)) + "\n")
            before, after = self.land_rows()
            code, out = self.run_tool("verify", f"{before}..{after}")
            self.assertEqual(1, code, out)
            self.assertIn(f"no identity-what-if verdict recorded for head {claimed_head[:12]} over base {base[:12]}", out)

    def change_foo_again(self) -> str:
        self.repo.write(FOO_PATH, FOO.replace("value * 2", "value * 4"))
        return self.repo.commit("change Foo again")

    def test_a_what_if_of_its_selection_or_with_a_take_moved_or_a_country_missing_is_refused(self):
        head = self.change_foo()
        for moved, mode, countries, why in (("0", "selected,final", COUNTRIES, "not every family"),
                                            ("3", "whole,final", COUNTRIES, "3 take(s) moved"),
                                            ("0", "whole,final", COUNTRIES[:4], "us: no identity-what-if verdict")):
            path = os.path.join(self.cache, "what-if-verdicts", f"{head}-{self.base}.tsv")
            if os.path.exists(path):
                os.remove(path)
            self.record_what_if(head, self.base, moved, mode, countries)
            code, out = self.run_tool("propose", f"{self.base}..{head}", "--proof", "what-if")
            self.assertEqual(1, code, out)
            self.assertIn(why, out)

    def test_a_model_only_what_if_carries_the_resolver_rules_alone(self):
        head = self.change_foo()
        self.record_what_if(head, self.base, mode="whole,model-only")
        self.assertEqual(0, self.run_tool("propose", f"{self.base}..{head}", "--proof", "what-if")[0])
        self.assertEqual(["identity-rules"], [r[0] for r in self.table_rows()])

    # ── what never carries ───────────────────────────────────────────────────────────────────────────────────────────

    def test_a_range_saying_identity_design_never_carries(self):
        head = self.change_foo("a new rule\n\nIdentity-Design: docs/design/x.md")
        self.record_what_if(head, self.base)
        code, out = self.run_tool("propose", f"{self.base}..{head}", "--proof", "what-if")
        self.assertEqual(1, code, out)
        self.assertIn("Identity-Design", out)

    def test_a_what_if_over_the_models_data_or_a_store_never_carries(self):
        for path in ("identity/src/main/resources/identity-weights.json",
                     "identity/src/main/scala/services/identity/IdentityModelStore.scala",
                     "common/src/main/resources/title-rules.tsv"):
            start = self.repo.git("rev-parse", "HEAD").strip()
            self.repo.write(path, f"changed {path}\n")
            head = self.repo.commit(f"change {path}")
            self.record_what_if(head, start)
            code, out = self.run_tool("propose", f"{start}..{head}", "--proof", "what-if")
            self.assertEqual(1, code, out)
            self.assertIn(f"cannot vouch for: {path}", out)

    def test_a_range_touching_the_drift_harness_never_carries(self):
        start = self.repo.git("rev-parse", "HEAD").strip()
        self.repo.write("scripts/ci/identity-drift-check.sh", "echo changed\n")
        head = self.repo.commit("harness")
        self.record_what_if(head, start)
        code, out = self.run_tool("propose", f"{start}..{head}", "--proof", "what-if")
        self.assertEqual(1, code, out)
        self.assertIn("drift harness", out)

    # ── auto: what agentctl land runs after its rebase ───────────────────────────────────────────────────────────────

    def test_auto_carries_a_stack_of_pure_moves_commit_by_commit_and_commits_the_rows(self):
        first = self.move_foo()
        self.repo.move("common/src/main/scala/services/old/Sibling.scala", "common/src/main/scala/services/old/Kin.scala")
        second = self.repo.commit("rename the sibling's file")
        code, out = self.run_tool("auto", f"{self.base}..{second}")
        self.assertEqual(0, code, out)
        rows = self.table_rows()
        self.assertEqual({(self.base, first), (first, second)}, {(r[4], r[5]) for r in rows}, out)
        self.assertEqual(6, len(rows), out)                          # each stamp, carried a commit at a time
        self.assertIn("Carry the identity stamps", self.repo.git("log", "-1", "--format=%s"))
        committed = self.repo.git("rev-parse", "HEAD").strip()
        self.assertEqual(0, self.run_tool("verify", f"{second}..{committed}")[0])

    def test_auto_carries_a_pure_move_beside_a_web_or_worker_only_commit_which_moves_no_stamp(self):
        first = self.move_foo()
        self.repo.write("web/src/main/assets/js/shared.js", "export const x = 1;\n")
        self.repo.write("worker/src/main/scala/services/tasks/TaskWorker.scala", "object TaskWorker { val n = 2 }\n")
        second = self.repo.commit("web and worker only: neither module is in any stamp's closure")
        code, out = self.run_tool("auto", f"{self.base}..{second}")
        self.assertEqual(0, code, out)
        self.assertNotIn("proves nothing", out)
        self.assertEqual({(self.base, first)}, {(r[4], r[5]) for r in self.table_rows()}, out)

    def test_the_digested_modules_are_the_builds_own(self):
        with open(os.path.join(os.path.dirname(HERE), "build.sbt")) as f:
            built = set(re.findall(r'IdentityRulesSources\.Module\("(\w+)"', f.read()))
        self.assertEqual(built, set(stamp_carry.DIGESTED_MODULES))

    def test_auto_writes_nothing_when_one_commit_proves_nothing(self):
        self.move_foo()
        head = self.change_foo_moved()
        code, out = self.run_tool("auto", f"{self.base}..{head}")
        self.assertEqual(0, code, out)
        self.assertIn("proves nothing", out)
        self.assertEqual([], self.table_rows())
        self.assertEqual(head, self.repo.git("rev-parse", "HEAD").strip())

    def change_foo_moved(self) -> str:
        self.repo.write(NEW_FOO_PATH, self.repo.read(NEW_FOO_PATH).replace("value * 2", "value * 3"))
        return self.repo.commit("change Foo")

    def test_auto_carries_a_rebased_change_by_the_what_if_taken_before_the_rebase(self):
        self.repo.git("checkout", "-q", "-b", "lane")
        before_rebase = self.change_foo()
        self.record_what_if(before_rebase, self.base)                  # the lane's what-if, on its own base
        self.repo.git("checkout", "-q", "-")
        self.repo.write("docs/notes.md", "main moved, outside the code\n")
        main = self.repo.commit("main moves on")
        self.repo.git("cherry-pick", before_rebase)              # agentctl land's rebase
        head = self.repo.git("rev-parse", "HEAD").strip()
        code, out = self.run_tool("auto", f"{main}..{head}")
        self.assertEqual(0, code, out)
        rows = self.table_rows()
        self.assertEqual(["agreement-rules", "identity-rules"], sorted(r[0] for r in rows), out)
        self.assertEqual({("what-if", self.base, before_rebase)}, {(r[3], r[4], r[5]) for r in rows})
        self.assertEqual(fake_stamp(self.repo, head, "identity-rules"), next(r[1] for r in rows if r[0] == "identity-rules"))
        committed = self.repo.git("rev-parse", "HEAD").strip()
        # the carry commit holds the record it stands on: ci-runner, with no what-if cache, proves it too
        self.assertEqual(0, self.run_tool("verify", f"{head}..{committed}", cache=os.path.join(self.tmp.name, "none"))[0])

    def test_auto_leaves_a_change_with_no_code_alone(self):
        self.repo.write("docs/notes.md", "words\n")
        head = self.repo.commit("docs")
        code, out = self.run_tool("auto", f"{self.base}..{head}")
        self.assertEqual(0, code, out)
        self.assertIn("no commit", out)


if __name__ == "__main__":
    unittest.main()
