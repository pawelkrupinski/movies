#!/usr/bin/env python3
"""population_walk_diff.py: a flip only the head has refuses (the listing, slice and chain named); one the base has too,
a known one, or a stale known row passes; a known row the change cures refuses; a slice the base could not walk is
judged on the known rows alone; no candidate passes.
Run: python3 scripts/ci/population_walk_diff_test.py"""
import os
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import population_walk_diff as t  # noqa: E402

TOOL = os.path.join(os.path.dirname(os.path.abspath(__file__)), "population_walk_diff.py")
SLICE = "drift-slices/uk/poole-hen-0a1b2c3d"
# the tool's own git, in a scratch repository: never a hook's exported GIT_DIR
NO_GIT_ENV = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
SCRATCH_GIT = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "scratch-git.sh")


def candidate(directory, slice_name, head=(), known=()):
    lines = [f"slice\t{slice_name}"]
    lines += [f"head\t{v}\t{r}\t{v} | {r}: 5 -> none, 6 -> 7, 8 -> none [chain {c}, film-none-film]" for v, r, c in head]
    lines += [f"known\t{v}\t{r}\t{n}" for v, r, n in known]
    with open(os.path.join(directory, slice_name.replace("/", "__") + ".tsv"), "w", encoding="utf-8") as fh:
        fh.write("\n".join(lines) + "\n")


def base_rows(path, walked=(), failed=()):
    with open(path, "w", encoding="utf-8") as fh:
        for s, rows in walked:
            fh.write(f"# {s}: {len(rows)} flip(s) in 1.0s\n")
            for v, r in rows:
                fh.write(f"{s}\t{v}\t{r}\tfilm-none-film: 5 -> none\n")
        for s in failed:
            fh.write(f"# {s}: FAILED java.lang.RuntimeException: boom\n")


class Judge(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = os.path.join(self.tmp.name, "cands")
        os.makedirs(self.dir)
        self.base = os.path.join(self.tmp.name, "base.txt")

    def tearDown(self):
        self.tmp.cleanup()

    def run_tool(self):
        p = subprocess.run([sys.executable, TOOL, "judge", self.dir, self.base], capture_output=True, text=True)
        return p.returncode, p.stdout

    def test_a_flip_only_the_head_has_is_refused_with_listing_slice_and_chain(self):
        candidate(self.dir, SLICE, head=[("Lighthouse Poole", "PFF: Hen", 3)])
        base_rows(self.base, walked=[(SLICE, [])])
        rc, out = self.run_tool()
        self.assertEqual(rc, 1, out)
        self.assertIn(f"NEW flip the base does not have: {SLICE}: Lighthouse Poole | PFF: Hen", out)
        self.assertIn("[chain 3,", out)

    def test_a_flip_the_base_has_too_passes_said_as_pre_existing(self):
        candidate(self.dir, SLICE, head=[("Lighthouse Poole", "PFF: Hen", 3)])
        base_rows(self.base, walked=[(SLICE, [("Lighthouse Poole", "PFF: Hen")])])
        rc, out = self.run_tool()
        self.assertEqual(rc, 0, out)
        self.assertIn("pre-existing", out)

    def test_a_known_flip_passes_and_only_the_new_one_refuses(self):
        candidate(self.dir, SLICE, head=[("A", "Hen", 0), ("B", "Hen", 1)], known=[("A", "Hen", "note")])
        base_rows(self.base, walked=[(SLICE, [("A", "Hen")])])
        rc, out = self.run_tool()
        self.assertEqual(rc, 1, out)
        self.assertIn("B | Hen", out)
        self.assertNotIn("A | Hen", out.replace("population walk: 1", ""))

    def test_a_known_row_the_change_cures_refuses_until_removed(self):
        candidate(self.dir, SLICE, head=[], known=[("A", "Hen", "note")])
        base_rows(self.base, walked=[(SLICE, [("A", "Hen")])])
        rc, out = self.run_tool()
        self.assertEqual(rc, 1, out)
        self.assertIn("CURED known row", out)

    def test_a_stale_known_row_main_holds_passes(self):
        candidate(self.dir, SLICE, head=[], known=[("A", "Hen", "note")])
        base_rows(self.base, walked=[(SLICE, [])])
        rc, out = self.run_tool()
        self.assertEqual(rc, 0, out)
        self.assertIn("stale known row", out)

    def test_a_slice_the_base_could_not_walk_is_judged_on_the_known_rows(self):
        candidate(self.dir, SLICE, head=[("B", "Hen", 1)])
        base_rows(self.base, failed=[SLICE])
        rc, out = self.run_tool()
        self.assertEqual(rc, 1, out)
        self.assertIn("base unwalked", out)
        self.assertIn("FAILED", out)

    def test_no_base_file_is_strict(self):
        candidate(self.dir, SLICE, head=[("B", "Hen", 1)])
        p = subprocess.run([sys.executable, TOOL, "judge", self.dir, os.devnull], capture_output=True, text=True)
        self.assertEqual(p.returncode, 1, p.stdout)

    def test_no_candidate_passes(self):
        base_rows(self.base)
        self.assertEqual(self.run_tool()[0], 0)

    def test_slices_lists_the_candidates(self):
        candidate(self.dir, SLICE, head=[("B", "Hen", 1)])
        p = subprocess.run([sys.executable, TOOL, "slices", self.dir], capture_output=True, text=True)
        self.assertEqual(p.stdout.split(), [SLICE])

    def run_brought(self, brought):
        listed = os.path.join(self.tmp.name, "brought.txt")
        with open(listed, "w", encoding="utf-8") as fh:
            fh.write("".join(f"{b}\n" for b in brought))
        p = subprocess.run([sys.executable, TOOL, "judge", self.dir, self.base, "--brought", listed],
                           capture_output=True, text=True)
        return p.returncode, p.stdout

    # Main #2515: 97d260d67 committed the Ratzeburg Grinch slice with a drift row only; its flips walked alike at the base
    # (the base walks the HEAD's slice) and passed as "pre-existing" -- but main's spec judges on the known rows alone
    def test_a_flip_the_base_has_too_refuses_in_a_slice_the_change_brings_with_the_row_to_add(self):
        candidate(self.dir, SLICE, head=[("Lighthouse Poole", "PFF: Hen", 3)])
        base_rows(self.base, walked=[(SLICE, [("Lighthouse Poole", "PFF: Hen")])])
        rc, out = self.run_brought([SLICE])
        self.assertEqual(rc, 1, out)
        self.assertIn(f"NEW flip in a slice or known rows this change brings: {SLICE}: Lighthouse Poole | PFF: Hen", out)
        self.assertIn(f"population\tuk\t\t{SLICE}\tLighthouse Poole\tPFF: Hen\t\t", out)
        self.assertNotIn("pre-existing", out)

    def test_a_stale_known_row_refuses_in_a_slice_the_change_brings(self):
        candidate(self.dir, SLICE, head=[], known=[("A", "Hen", "note")])
        base_rows(self.base, walked=[(SLICE, [])])
        rc, out = self.run_brought([SLICE])
        self.assertEqual(rc, 1, out)
        self.assertIn("CURED known row", out)

    def test_a_slice_the_change_does_not_bring_keeps_the_differential(self):
        candidate(self.dir, SLICE, head=[("Lighthouse Poole", "PFF: Hen", 3)])
        base_rows(self.base, walked=[(SLICE, [("Lighthouse Poole", "PFF: Hen")])])
        rc, out = self.run_brought(["drift-slices/de/other-00000000"])
        self.assertEqual(rc, 0, out)
        self.assertIn("pre-existing", out)

    def test_backlog_row_reads_the_slices_country(self):
        self.assertEqual(t.backlog_row("drift-slices/de/x-1", "V", "T", "film-film-film: a -> b -> a", "2026-11-16").split("\t"),
                         ["population", "de", "", "drift-slices/de/x-1", "V", "T", "", "2026-11-16", "identity",
                          "film-film-film: a -> b -> a"])
        self.assertEqual(t.backlog_row("identity-slices/uk-mowlem-hen-7", "V", "T", "n", "d").split("\t")[1], "uk")

    def test_rows_turns_a_walk_into_backlog_rows(self):
        walk = (f"# {SLICE}: 1 flip(s) in 1.0s\n{SLICE}\tLighthouse Poole\tPFF: Hen\tfilm-none-film: model 5 -> none\n"
                "# drift-slices/de/clean-1: 0 flip(s) in 1.0s\n")
        p = subprocess.run([sys.executable, TOOL, "rows", "--first-seen", "2026-11-16"], input=walk,
                           capture_output=True, text=True)
        self.assertEqual(p.returncode, 0, p.stderr)
        self.assertEqual(p.stdout, f"population\tuk\t\t{SLICE}\tLighthouse Poole\tPFF: Hen\t\t2026-11-16\tidentity\t"
                                   "film-none-film: model 5 -> none\n")

    def test_rows_into_the_backlog_files_each_missing_row_once(self):
        backlog = os.path.join(self.tmp.name, "backlog.tsv")
        with open(backlog, "w", encoding="utf-8") as fh:
            fh.write("# comment\nkind\tcountry\tseed\tslice\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote\n"
                     f"population\tuk\t\t{SLICE}\tA\tHen\t\t2026-11-01\tidentity\told note\n")
        walk = f"{SLICE}\tA\tHen\tfilm-none-film: x\n{SLICE}\tB\tHen\tfilm-film-film: y\n"
        for _ in range(2):
            p = subprocess.run([sys.executable, TOOL, "rows", "--first-seen", "2026-11-16", "--into", backlog],
                               input=walk, capture_output=True, text=True)
            self.assertEqual(p.returncode, 0, p.stderr)
        with open(backlog, encoding="utf-8") as fh:
            lines = fh.read().splitlines()
        self.assertEqual(lines[2:], [f"population\tuk\t\t{SLICE}\tA\tHen\t\t2026-11-01\tidentity\told note",
                                     f"population\tuk\t\t{SLICE}\tB\tHen\t\t2026-11-16\tidentity\tfilm-film-film: y"])

    def test_rows_fails_on_a_walk_that_failed_or_breached(self):
        for what in ("FAILED boom", "BREACH X | T: undeclared"):
            p = subprocess.run([sys.executable, TOOL, "rows"], input=f"# {SLICE}: {what}\n", capture_output=True, text=True)
            self.assertEqual(p.returncode, 1)
            self.assertIn(what, p.stderr)


class Brought(unittest.TestCase):
    """`brought`: the candidate slices whose data or known rows differ from the base's -- a new or edited slice, a
    population row of it added or removed."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.tmp.name, "repo")
        self.fx = os.path.join(self.repo, "test", "resources", "fixtures")
        os.makedirs(os.path.join(self.fx, "drift-slices", "uk", "kept-1"))
        self.write("drift-slices/uk/kept-1/slice.json", "{}")
        self.write("identity-flip-backlog.tsv", "kind\tcountry\tseed\tslice\tvenue\trawTitle\n")
        self.scratch("scratch_repo", "-q")
        self.scratch("scratch_git", "add", "-A")
        self.scratch("scratch_git", "-c", "user.name=t", "-c", "user.email=t@t", "commit", "-qm", "base")
        self.cands = os.path.join(self.tmp.name, "cands")
        os.makedirs(self.cands)

    def tearDown(self):
        self.tmp.cleanup()

    def scratch(self, helper, *args):
        """scripts/scratch-git.sh's isolating helpers: a raw git would inherit a hook's GIT_DIR."""
        subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.repo, *args],
                       check=True, capture_output=True, text=True)

    def write(self, rel, text):
        path = os.path.join(self.fx, rel)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w", encoding="utf-8") as fh:
            fh.write(text)

    def brought(self):
        p = subprocess.run([sys.executable, TOOL, "brought", self.cands, "HEAD"], cwd=self.repo, capture_output=True,
                           text=True, env=NO_GIT_ENV)
        self.assertEqual(p.returncode, 0, p.stderr)
        return p.stdout.split()

    def test_a_new_slice_is_brought_an_unchanged_one_is_not(self):
        self.write("drift-slices/de/new-2/slice.json", "{}")
        candidate(self.cands, "drift-slices/de/new-2")
        candidate(self.cands, "drift-slices/uk/kept-1")
        self.assertEqual(self.brought(), ["drift-slices/de/new-2"])

    def test_an_edited_slice_is_brought(self):
        self.write("drift-slices/uk/kept-1/slice.json", "{\"more\": 1}")
        candidate(self.cands, "drift-slices/uk/kept-1")
        self.assertEqual(self.brought(), ["drift-slices/uk/kept-1"])

    def test_a_slice_whose_population_rows_changed_is_brought(self):
        self.write("identity-flip-backlog.tsv", "kind\tcountry\tseed\tslice\tvenue\trawTitle\n"
                                                "population\tuk\t\tdrift-slices/uk/kept-1\tV\tT\n")
        candidate(self.cands, "drift-slices/uk/kept-1")
        self.assertEqual(self.brought(), ["drift-slices/uk/kept-1"])

    def changed(self):
        p = subprocess.run([sys.executable, TOOL, "changed-slices", "HEAD"], cwd=self.repo, capture_output=True, text=True,
                           env=NO_GIT_ENV)
        self.assertEqual(p.returncode, 0, p.stderr)
        return p.stdout.split()

    def test_changed_slices_names_every_slice_the_change_brings_and_none_else(self):
        self.assertEqual(self.changed(), [])
        self.write("drift-slices/de/new-2/slice.json", "{}")
        self.write("identity-slices/uk-case-3/slice.json", "{}")
        self.assertEqual(self.changed(), ["drift-slices/de/new-2", "identity-slices/uk-case-3"])

    def test_changed_slices_names_a_slice_whose_population_rows_changed(self):
        self.write("identity-flip-backlog.tsv", "kind\tcountry\tseed\tslice\tvenue\trawTitle\n"
                                                "population\tuk\t\tdrift-slices/uk/kept-1\tV\tT\n")
        self.assertEqual(self.changed(), ["drift-slices/uk/kept-1"])

    def test_changed_slices_leaves_out_a_deleted_slice(self):
        os.remove(os.path.join(self.fx, "drift-slices/uk/kept-1/slice.json"))
        self.assertEqual(self.changed(), [])

    def walk_judge(self, walk):
        path = os.path.join(self.tmp.name, "walk.txt")
        with open(path, "w", encoding="utf-8") as fh:
            fh.write(walk)
        p = subprocess.run([sys.executable, TOOL, "judge-walk", path], cwd=self.repo, capture_output=True, text=True,
                           env=NO_GIT_ENV)
        return p.returncode, p.stdout

    HEADER = "kind\tcountry\tseed\tslice\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote\n"

    # Main #2515, the hook's `flipbacklog` check: a slice walked and judged on the head's known rows, as main's spec does
    def test_judge_walk_refuses_a_flip_no_population_row_names_printing_the_row(self):
        g = "drift-slices/de/grinch-05c3cfc6"
        rc, out = self.walk_judge(f"# {g}: 1 flip(s) in 3.1s\n{g}\tBurgtheater Ratzeburg\tAdventskino: Der Grinch\t"
                                  "film-film-film: model tmdb:8871@30 -> tmdb:360920@35 -> tmdb:8871@39 [chain 3]\n")
        self.assertEqual(rc, 1, out)
        self.assertIn(f"population\tde\t\t{g}\tBurgtheater Ratzeburg\tAdventskino: Der Grinch\t\t", out)
        self.assertIn(f"{g}: Burgtheater Ratzeburg | Adventskino: Der Grinch: film-film-film", out)

    def test_judge_walk_passes_a_known_or_struck_flip_and_refuses_a_cured_row(self):
        g = "drift-slices/de/grinch-05c3cfc6"
        self.write("identity-flip-backlog.tsv", self.HEADER + f"population\tde\t\t{g}\tA\tT\t\td\to\tn\n"
                                                              f"population\tde\t\t{g}\tGone\tT\t\td\to\tn\n")
        self.write("identity-flips-struck.tsv", "country\tvenue\trawTitle\tdecision\nde\tS\tT\tOWNER-APPROVED x\n")
        rc, out = self.walk_judge(f"# {g}: 2 flip(s) in 1s\n{g}\tA\tT\tfilm-none-film: x\n{g}\tS\tT\tfilm-none-film: y\n")
        self.assertEqual(rc, 1, out)
        self.assertIn("CURED known row", out)
        self.assertIn("Gone | T", out)
        self.assertNotIn("NEW flip", out)

    def test_judge_walk_refuses_a_failed_walk_and_a_rule_contract_breach(self):
        g = "drift-slices/de/grinch-05c3cfc6"
        rc, out = self.walk_judge(f"# {g}: FAILED boom\n")
        self.assertEqual(rc, 1, out)
        rc, out = self.walk_judge(f"# {g}: 0 flip(s) in 1s\n# {g}: BREACH X | T: undeclared rule\n")
        self.assertEqual(rc, 1, out)
        self.assertIn("undeclared rule", out)

    def test_judge_walk_passes_a_clean_walk(self):
        rc, out = self.walk_judge("# drift-slices/de/clean-1: 0 flip(s) in 1s\n")
        self.assertEqual(rc, 0, out)

    def test_differential(self):
        self.assertEqual(t.differential([1, 2, 3], [3, 4]), ([1, 2], [4], [3]))


if __name__ == "__main__":
    unittest.main()
