#!/usr/bin/env python3
"""coupling_ratchet.py: any package cycle edge fails, measured or recorded in the baseline, whatever the trailer; so
does measured coupling past the baseline (a new cross-module edge kind, a package's Ce past its row or the new-package
limit), a class newly past the fan-out limit or a large package newly past the distance limit (only an exemption
row admits one, never --update), a frozen fan-out or distance row rising, any package edge into a higher layer or, inside an ordered
layer, a higher rank (naming the direction allowed), and a package without its layer or rank; a baseline grown in a
range passes only when a commit changing it carries `Coupling: <why>`, and so does a layer table loosened in a range (an
`allow` row added, a package given another layer or rank) and an exemption table loosened (a row added, a limit
changed, a contract's member added or dropped); a contract past 8 members (or its grandfathered `contract-size`) fails;
falls pass and are said; a frozen class split honestly (`Coupling: split of <X>`, the pieces taking over X's
code, each below X's row, the union of what they and X's remainder reference within X's frozen reference set) freezes
its pieces, and lets the distance row of a package holding X or a piece rise by 0.02 at most; nothing else does.
COUPLING_RATCHET_UNDER_TEST names another copy of the tool (coupling-ratchet-mutation-test.sh's mutants)."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
TOOL = os.environ.get("COUPLING_RATCHET_UNDER_TEST") or os.path.join(HERE, "coupling_ratchet.py")
BASELINE = "test/resources/coupling-baseline.tsv"
LAYERS = "test/resources/coupling-layers.tsv"
EXEMPTIONS = "test/resources/coupling-exemptions.tsv"
REFS = "test/resources/coupling-fanout-refs.tsv"

EXEMPTION_ROWS = """# kind\tpattern\tlimit|exempt\towner\twhy
root\tmodules.*\texempt\tcoupling\tthe composition roots
"""

LAYER_ROWS = """# a comment
layer\t0\ttools\tgeneric
layer\t1\tmodels\tthe data
layer\t3\tservices\tthe application
layer\t4\tcontrollers\tthe web tier
rank\t0\tservices.movies\tthe corpus
rank\t1\tservices.tasks\tthe queue, over the corpus
"""

ROWS = """# a comment
limit\tfanout\t9
limit\tnew-package-ce\t15
limit\tdistance\t20
limit\tdistance-min-types\t8
module-edge\tworker->common\t40
ce\tmodels\t12
ce\tservices.movies\t20
fanout\tservices.movies.MovieService\t35
distance\tservices.movies\t48
"""


def rows(*extra: str, drop: tuple = ()) -> str:
    kept = [line for line in ROWS.splitlines() if not any(line.startswith(d) for d in drop)]
    return "\n".join(kept + list(extra)) + "\n"


def fanout_line(cls: str, value) -> str:
    """A fanouts row: `<class><TAB><n>`, or with what it references (`value` a list of targets) `<TAB><t>,<t>...`."""
    if isinstance(value, int):
        return f"{cls}\t{value}\n"
    return f"{cls}\t{len(value)}\t{','.join(sorted(value))}\n"


class RatchetHarness(unittest.TestCase):
    """A scratch repository with the baseline, the layer table and the exemptions committed, and the tool run in it."""

    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.scratch("scratch_repo", "-q")
        self.base = self.commit({BASELINE: ROWS, LAYERS: LAYER_ROWS, EXEMPTIONS: EXEMPTION_ROWS}, "base")

    def tearDown(self):
        self.dir.cleanup()
        os.environ.update(self.saved)

    def scratch(self, helper, *args):
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.repo, *args],
                              check=True, capture_output=True, text=True).stdout.strip()

    def commit(self, files, message):
        for path, text in files.items():
            full = os.path.join(self.repo, path)
            if text is None:
                os.remove(full)
                continue
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "w") as f:
                f.write(text)
        self.scratch("scratch_git", "add", "-A")
        self.scratch("scratch_git", "commit", "-qm", message)
        return self.scratch("scratch_git", "rev-parse", "HEAD")

    def check(self, current, rng=None, edges=None, fanouts=None):
        path = os.path.join(self.dir.name, "current.tsv")
        with open(path, "w") as f:
            f.write(current)
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        args = [sys.executable, "-I", TOOL, "check", "--current", path, "--baseline", BASELINE]
        if rng:
            args += ["--range", rng]
        if edges is not None:
            edges_path = os.path.join(self.dir.name, "edges.tsv")
            with open(edges_path, "w") as f:
                f.write(edges)
            args += ["--edges", edges_path]
        if fanouts is not None:
            fanouts_path = os.path.join(self.dir.name, "fanouts.tsv")
            with open(fanouts_path, "w") as f:
                f.write("".join(fanout_line(c, n) for c, n in fanouts.items()))
            args += ["--fanouts", fanouts_path]
        done = subprocess.run(args, cwd=self.repo, env=env, capture_output=True, text=True)
        return done.returncode, done.stdout + done.stderr

    def assert_fails(self, current, *said, rng=None, edges=None, fanouts=None):
        rc, out = self.check(current, rng, edges, fanouts)
        self.assertEqual(1, rc, out)
        for s in said:
            self.assertIn(s, out)

    def assert_passes(self, current, rng=None, fanouts=None):
        rc, out = self.check(current, rng, fanouts=fanouts)
        self.assertEqual(0, rc, out)
        return out

class CouplingRatchetTest(RatchetHarness):
    def test_the_baseline_itself_passes(self):
        self.assertIn("coupling: within", self.assert_passes(ROWS))

    def test_a_new_cross_module_edge_kind_fails(self):
        self.assert_fails(rows("module-edge\tcommon->worker\t1"), "a new cross-module edge kind: common->worker")

    def test_a_heavier_known_module_edge_passes(self):
        self.assert_passes(rows("module-edge\tworker->common\t400", drop=("module-edge",)))

    def test_a_new_package_cycle_fails(self):
        self.assert_fails(rows("cycle-edge\tmodels->tools\t1", "cycle-edge\ttools->models\t2"),
                          "a package cycle edge: models->tools", "a package cycle edge: tools->models", "no `Coupling:` trailer")

    def test_a_cycle_fails_naming_the_class_references_behind_it(self):
        edges = "models->tools\tmodels.Film->tools.Slugify\nmodels->tools\tmodels.Codec->tools.Digest\n"
        self.assert_fails(rows("cycle-edge\tmodels->tools\t2"), "models.Film -> tools.Slugify, models.Codec -> tools.Digest",
                          edges=edges)

    def test_a_cycle_recorded_in_the_baseline_fails_even_with_a_trailer(self):
        recorded = rows("cycle-edge\tmodels->tools\t1", "cycle-edge\ttools->models\t1")
        head = self.commit({BASELINE: recorded}, "record the cycle\n\nCoupling: it is only one class")
        self.assert_fails(recorded, "THE COUPLING BASELINE RECORDS A PACKAGE CYCLE", "cycle-edge models->tools",
                          rng=f"{self.base}..{head}")
        # ...and the code without the cycle still fails while the baseline keeps it
        self.assert_fails(ROWS, "THE COUPLING BASELINE RECORDS A PACKAGE CYCLE")

    def test_a_package_ce_past_its_row_fails(self):
        self.assert_fails(rows("ce\tmodels\t13", drop=("ce\tmodels",)), "package models: Ce 13 past its baseline 12")

    def test_a_package_ce_at_its_row_passes(self):
        self.assert_passes(rows("ce\tmodels\t12", drop=("ce\tmodels",)))

    def test_a_new_package_past_the_new_package_limit_fails(self):
        self.assert_fails(rows("ce\tservices.fresh\t16"), "package services.fresh: Ce 16 past the new-package limit 15")

    def test_a_new_package_within_the_new_package_limit_passes(self):
        self.assert_passes(rows("ce\tservices.fresh\t15"))

    # THE LIMITS (mutations the owner named, 2026-11-18): each must fail
    def test_a_frozen_fan_out_row_rising_fails(self):
        self.assert_fails(rows("fanout\tservices.movies.MovieService\t36", drop=("fanout",)),
                          "class services.movies.MovieService: fan-out 36 past its frozen row 35")

    def test_a_new_class_with_fan_out_10_fails(self):
        self.assert_fails(rows("fanout\tworker.Wiring\t10"), "class worker.Wiring: fan-out 10 past the limit 9",
                          "only an exemption row admits it")

    def test_a_new_package_with_distance_30_fails(self):
        self.assert_fails(rows("distance\tservices.fresh\t30"),
                          "package services.fresh: distance D (hundredths) 30 past the limit 20")

    def test_a_frozen_distance_row_rising_fails(self):
        self.assert_fails(rows("distance\tservices.movies\t49", drop=("distance",)),
                          "package services.movies: distance D (hundredths) 49 past its frozen row 48")

    def test_a_frozen_row_falling_passes(self):
        self.assert_passes(rows("fanout\tservices.movies.MovieService\t20", "distance\tservices.movies\t30",
                                drop=("fanout", "distance")))

    def test_a_frozen_class_that_moved_keeps_its_row(self):
        self.assert_passes(rows("fanout\tservices.corpus.MovieService\t35", drop=("fanout",)))
        self.assert_fails(rows("fanout\tservices.corpus.MovieService\t36", drop=("fanout",)), "past the limit 9")

    def test_update_never_admits_a_class_past_the_limit_whatever_the_trailer(self):
        grown = rows("fanout\tworker.Wiring\t10")
        head = self.commit({BASELINE: grown}, "record it\n\nCoupling: it is only one more")
        self.assert_fails(grown, "ADMITS A CLASS OR PACKAGE PAST A LIMIT", "fanout worker.Wiring added at 10",
                          rng=f"{self.base}..{head}")

    def test_a_raised_frozen_row_fails_whatever_the_trailer(self):
        grown = rows("distance\tservices.movies\t50", drop=("distance",))
        head = self.commit({BASELINE: grown}, "record it\n\nCoupling: the corpus grew")
        self.assert_fails(grown, "distance services.movies raised 48 -> 50", rng=f"{self.base}..{head}")

    def test_a_lowered_limit_takes_the_rows_again_with_a_trailer(self):
        tighter = rows("limit\tfanout\t8", "fanout\tworker.Wiring\t9", drop=("limit\tfanout",))
        head = self.commit({BASELINE: tighter}, "tighten\n\nCoupling: the owner lowered the fan-out limit")
        self.assert_passes(tighter, rng=f"{self.base}..{head}")
        later = self.commit({BASELINE: tighter + "fanout\tworker.Other\t9\n"}, "one more\n\nCoupling: no trailer admits it")
        self.assert_fails(tighter + "fanout\tworker.Other\t9\n", "fanout worker.Other added at 9", rng=f"{head}..{later}")

    def test_a_lowered_limit_without_a_trailer_fails(self):
        tighter = rows("limit\tfanout\t8", "fanout\tworker.Wiring\t9", drop=("limit\tfanout",))
        head = self.commit({BASELINE: tighter}, "tighten, no why")
        self.assert_fails(tighter, "fanout rows taken again under new limits", rng=f"{self.base}..{head}")

    def test_a_measurement_fix_takes_the_fan_out_rows_again_at_their_true_values_with_a_trailer(self):
        # same-file uses counted (Coupling.FanOutMeasure 2): MovieService's true 38, and a class newly past the limit
        measured = rows("limit\tfanout-measure\t2", "fanout\tservices.movies.MovieService\t38", "fanout\tworker.Wiring\t10",
                        drop=("fanout",))
        head = self.commit({BASELINE: measured}, "re-measure\n\nCoupling: fan-out counts same-file classes (measurement fix)")
        self.assert_passes(measured, rng=f"{self.base}..{head}")
        later = self.commit({BASELINE: measured + "fanout\tworker.Other\t10\n"}, "one more\n\nCoupling: no trailer admits it")
        self.assert_fails(measured + "fanout\tworker.Other\t10\n", "fanout worker.Other added at 10", rng=f"{head}..{later}")

    def test_a_measurement_fix_without_a_trailer_fails(self):
        measured = rows("limit\tfanout-measure\t2", "fanout\tservices.movies.MovieService\t38", drop=("fanout",))
        head = self.commit({BASELINE: measured}, "re-measure, no why")
        self.assert_fails(measured, "limit fanout-measure raised 1 -> 2", "fanout rows taken again under new limits",
                          rng=f"{self.base}..{head}")

    def test_a_baseline_keeping_its_measure_takes_no_rows_again(self):
        measured = rows("limit\tfanout-measure\t2")
        mid = self.commit({BASELINE: measured}, "re-measure\n\nCoupling: measurement fix")
        grown = measured.replace("MovieService\t35", "MovieService\t36")
        head = self.commit({BASELINE: grown}, "record it\n\nCoupling: no trailer admits it")
        self.assert_fails(grown, "fanout services.movies.MovieService raised 35 -> 36", rng=f"{mid}..{head}")

    def test_an_exemption_row_added_without_a_trailer_fails(self):
        head = self.commit({EXEMPTIONS: EXEMPTION_ROWS + "registry\tworker.Wiring\texempt\tworker\ta table\n"}, "exempt it")
        self.assert_fails(ROWS, "THE EXEMPTION TABLE LOOSENED", "registry worker.Wiring (exempt) added", rng=f"{self.base}..{head}")

    def test_an_exemption_row_added_with_a_trailer_passes(self):
        head = self.commit({EXEMPTIONS: EXEMPTION_ROWS + "registry\tworker.Wiring\texempt\tworker\ta table\n"},
                           "exempt it\n\nCoupling: a venue table, one row a venue")
        self.assert_passes(ROWS, rng=f"{self.base}..{head}")

    def test_an_exemption_limit_raised_without_a_trailer_fails(self):
        first = self.commit({EXEMPTIONS: EXEMPTION_ROWS + "config\tsettings.Config\t40\tcoupling\tthe root\n"},
                            "limit it\n\nCoupling: the typed-config root")
        head = self.commit({EXEMPTIONS: EXEMPTION_ROWS + "config\tsettings.Config\t60\tcoupling\tthe root\n"}, "raise it")
        self.assert_fails(ROWS, "config settings.Config changed 40 -> 60", rng=f"{first}..{head}")

    def test_an_exemption_row_dropped_needs_no_trailer(self):
        head = self.commit({EXEMPTIONS: "# empty\n"}, "no roots exempt")
        self.assert_passes(ROWS, rng=f"{self.base}..{head}")

    def contract(self, name, members, users="controllers.*"):
        return (f"contract\t{users}\t{name}\tweb\tthe users\n"
                + "".join(f"member\tmodels.M{i}\t{name}\tweb\tmember {i}\n" for i in range(members)))

    def test_a_contract_member_added_without_a_trailer_fails(self):
        first = self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("served", 3)}, "declare it\n\nCoupling: read together")
        head = self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("served", 4)}, "one more")
        self.assert_fails(ROWS, "THE EXEMPTION TABLE LOOSENED", "member:served models.M3 (served) added", rng=f"{first}..{head}")

    def test_a_contract_member_added_with_a_trailer_passes(self):
        first = self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("served", 3)}, "declare it\n\nCoupling: read together")
        head = self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("served", 4)}, "one more\n\nCoupling: read with the rest")
        self.assert_passes(ROWS, rng=f"{first}..{head}")

    def test_a_contract_member_dropped_without_a_trailer_fails(self):
        first = self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("served", 3)}, "declare it\n\nCoupling: read together")
        head = self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("served", 2)}, "one fewer")
        self.assert_fails(ROWS, "member:served models.M2 (served) dropped", rng=f"{first}..{head}")

    def test_a_type_joining_a_second_contract_without_a_trailer_fails(self):
        first = self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("scrape", 2, "services.cinemas.*") + self.contract("served", 1)},
                            "declare them\n\nCoupling: read together")
        head = self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("scrape", 2, "services.cinemas.*") + self.contract("served", 2)},
                           "M1 serves too")
        self.assert_fails(ROWS, "member:served models.M1 (served) added", rng=f"{first}..{head}")

    def test_a_contract_of_8_members_passes(self):
        self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("served", 8)}, "declare it")
        self.assert_passes(ROWS)

    def test_a_9th_contract_member_fails_whatever_the_trailer(self):
        self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("served", 9)}, "declare it\n\nCoupling: all of it")
        self.assert_fails(ROWS, "A DECLARED CONTRACT IS TOO LARGE", "contract served has 9 members, past the cap 8")

    def test_a_grandfathered_contract_holds_at_its_size(self):
        size = "contract-size\tscrape\t12\tscrapers\tdeclared before the cap\n"
        self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("scrape", 12, "services.cinemas.*") + size}, "grandfather it")
        self.assert_passes(ROWS)
        self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("scrape", 13, "services.cinemas.*") + size}, "grow it")
        self.assert_fails(ROWS, "contract scrape has 13 members, past its grandfathered size 12")

    def test_a_grandfathered_size_raised_without_a_trailer_fails(self):
        size = "contract-size\tscrape\t{}\tscrapers\tdeclared before the cap\n"
        first = self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("scrape", 12, "services.cinemas.*") + size.format(12)},
                            "grandfather it\n\nCoupling: declared before the cap")
        head = self.commit({EXEMPTIONS: EXEMPTION_ROWS + self.contract("scrape", 12, "services.cinemas.*") + size.format(13)}, "room")
        self.assert_fails(ROWS, "contract-size scrape changed 12 -> 13", rng=f"{first}..{head}")

    def test_an_upward_layer_edge_fails_whatever_the_baseline_says(self):
        violation = "layer-violation\tmodels->services.movies\t2"
        self.commit({BASELINE: rows(violation)}, "a baseline that recorded the violation")
        self.assert_fails(rows(violation), "a package edge into a higher layer: models->services.movies")

    def test_an_unlayered_package_fails(self):
        self.assert_fails(rows("layer-violation\tfresh->?\t0"), "package fresh has no layer")

    def test_an_upward_edge_inside_a_layer_fails_naming_the_direction_allowed(self):
        violation = "rank-violation\tservices.movies->services.tasks\t3"
        self.commit({BASELINE: rows(violation)}, "a baseline that recorded the violation\n\nCoupling: no")
        self.assert_fails(rows(violation), "an upward edge inside a layer: services.movies (rank 0) -> services.tasks (rank 1)",
                          "the allowed direction is services.tasks -> services.movies",
                          "services.movies.MovieService -> services.tasks.TaskQueue",
                          edges="services.movies->services.tasks\tservices.movies.MovieService->services.tasks.TaskQueue\n")

    def test_a_package_without_its_rank_fails(self):
        self.assert_fails(rows("rank-violation\tservices.fresh->?\t0"), "package services.fresh has no rank in its layer")

    def test_falls_pass_and_are_said(self):
        out = self.assert_passes(rows("ce\tmodels\t3", drop=("ce\tmodels", "fanout")))
        self.assertIn("coupling fell", out)
        self.assertIn("ce models 12 -> 3", out)
        self.assertIn("fanout services.movies.MovieService 35 -> below the limit", out)

    def test_a_grown_baseline_without_a_trailer_fails(self):
        grown = rows("module-edge\tcommon->worker\t1")
        head = self.commit({BASELINE: grown}, "use the worker from common")
        self.assert_fails(grown, "THE COUPLING BASELINE GREW", "module-edge common->worker added", rng=f"{self.base}..{head}")

    def test_a_coupling_line_outside_the_final_block_is_refused_saying_where_it_is(self):
        grown = rows("ce\tmodels\t14", drop=("ce\tmodels",))
        head = self.commit({BASELINE: grown}, "models reads the clock\n\nCoupling: the clock is the one seam\n\n"
                                              "Co-Authored-By: A <a@b.c>")
        self.assert_fails(grown, "THE COUPLING BASELINE GREW",
                          f"{head[:9]}: found `Coupling: the clock is the one seam` on line 3, but it is not in the final"
                          " trailer block", rng=f"{self.base}..{head}")

    def test_a_grown_baseline_with_a_trailer_passes(self):
        grown = rows("ce\tmodels\t14", drop=("ce\tmodels",))
        head = self.commit({BASELINE: grown}, "models reads the clock\n\nCoupling: the clock is the one seam models needs")
        self.assert_passes(grown, rng=f"{self.base}..{head}")

    def test_a_trailer_on_a_commit_not_changing_the_baseline_does_not_count(self):
        grown = rows("module-edge\tcommon->worker\t1")
        first = self.commit({BASELINE: grown}, "a new edge kind")
        head = self.commit({"README": "x\n"}, "docs\n\nCoupling: not this commit's")
        self.assert_fails(grown, "module-edge common->worker added", rng=f"{self.base}..{head}")
        self.assertTrue(first)

    def test_an_empty_trailer_does_not_count(self):
        grown = rows("ce\tmodels\t14", drop=("ce\tmodels",))
        head = self.commit({BASELINE: grown}, "models reads the clock\n\nCoupling: ")
        self.assert_fails(grown, "ce models raised 12 -> 14", rng=f"{self.base}..{head}")

    def test_a_raised_ce_row_without_a_trailer_fails(self):
        grown = rows("ce\tmodels\t14", drop=("ce\tmodels",))
        head = self.commit({BASELINE: grown}, "models reads the clock")
        self.assert_fails(grown, "ce models raised 12 -> 14", rng=f"{self.base}..{head}")

    def test_a_raised_limit_is_growth(self):
        grown = rows("limit\tfanout\t40", drop=("limit\tfanout",))
        head = self.commit({BASELINE: grown}, "looser")
        self.assert_fails(grown, "limit fanout raised 9 -> 40", rng=f"{self.base}..{head}")

    def test_a_new_package_row_within_the_limit_is_no_growth(self):
        grown = rows("ce\tservices.fresh\t4")
        head = self.commit({BASELINE: grown}, "a new package")
        self.assert_passes(grown, rng=f"{self.base}..{head}")

    def test_a_shrunk_baseline_needs_no_trailer(self):
        shrunk = rows("ce\tmodels\t5", drop=("ce\tmodels", "module-edge"))
        head = self.commit({BASELINE: shrunk}, "models uses less")
        self.assert_passes(shrunk, rng=f"{self.base}..{head}")

    def test_an_allowed_upward_edge_without_a_trailer_fails(self):
        head = self.commit({LAYERS: LAYER_ROWS + "allow\tservices.movies->controllers\tthe view reads it\n"}, "allow it")
        self.assert_fails(ROWS, "THE LAYER TABLE LOOSENED", "allow services.movies->controllers added",
                          rng=f"{self.base}..{head}")

    def test_an_allowed_upward_edge_with_a_trailer_passes(self):
        head = self.commit({LAYERS: LAYER_ROWS + "allow\tservices.movies->controllers\tthe view reads it\n"},
                           "allow it\n\nCoupling: the view model is the one seam")
        self.assert_passes(ROWS, rng=f"{self.base}..{head}")

    def test_a_package_moved_to_another_layer_without_a_trailer_fails(self):
        head = self.commit({LAYERS: LAYER_ROWS.replace("layer\t1\tmodels", "layer\t3\tmodels")}, "models up")
        self.assert_fails(ROWS, "THE LAYER TABLE LOOSENED", "models moved from layer 1 to 3", rng=f"{self.base}..{head}")

    def test_a_sub_package_lifted_by_a_new_row_without_a_trailer_fails(self):
        head = self.commit({LAYERS: LAYER_ROWS + "layer\t5\tservices.identity\tlifted\n"}, "lift identity")
        self.assert_fails(ROWS, "services.identity moved from layer 3 to 5", rng=f"{self.base}..{head}")

    def test_a_trailer_on_a_commit_not_changing_the_layer_table_does_not_count(self):
        self.commit({LAYERS: LAYER_ROWS + "allow\tmodels->services\tx\n"}, "allow")
        head = self.commit({BASELINE: ROWS + "# touched\n"}, "baseline\n\nCoupling: not the table's")
        self.assert_fails(ROWS, "allow models->services added", rng=f"{self.base}..{head}")

    def test_a_re_ranked_package_without_a_trailer_fails(self):
        head = self.commit({LAYERS: LAYER_ROWS.replace("rank\t0\tservices.movies", "rank\t2\tservices.movies")}, "movies up")
        self.assert_fails(ROWS, "THE LAYER TABLE LOOSENED", "services.movies re-ranked from 0 to 2", rng=f"{self.base}..{head}")

    def test_a_re_ranked_package_with_a_trailer_passes(self):
        head = self.commit({LAYERS: LAYER_ROWS.replace("rank\t0\tservices.movies", "rank\t2\tservices.movies")},
                           "movies up\n\nCoupling: the corpus reads the queue now, the queue no longer the corpus")
        self.assert_passes(ROWS, rng=f"{self.base}..{head}")

    def test_a_new_package_rank_row_needs_no_trailer(self):
        head = self.commit({LAYERS: LAYER_ROWS + "rank\t1\tservices.fresh\ta new package\n"}, "a new package")
        self.assert_passes(ROWS, rng=f"{self.base}..{head}")

    def test_a_new_top_level_package_row_needs_no_trailer(self):
        head = self.commit({LAYERS: LAYER_ROWS + "layer\t5\tdiagnostics\tcommand-line programs\n"}, "a new package")
        self.assert_passes(ROWS, rng=f"{self.base}..{head}")

    def test_a_dropped_allow_row_needs_no_trailer(self):
        self.commit({LAYERS: LAYER_ROWS + "allow\tmodels->services\tx\n"}, "allow\n\nCoupling: once")
        mid = self.scratch("scratch_git", "rev-parse", "HEAD")
        head = self.commit({LAYERS: LAYER_ROWS}, "cut it")
        self.assert_passes(ROWS, rng=f"{mid}..{head}")

    def test_a_malformed_row_is_an_error_not_a_pass(self):
        rc, out = self.check(ROWS + "ce\tmodels\n")
        self.assertEqual(2, rc, out)


# THE SPLIT (the owner, 2026-11-18): ReadModelSweeper (frozen at 36) split into SweepPlanner and SweepPass -- the
# 2026-11-18 split the ratchet refused and that was reverted, 17 + 11. Its 36 references are Dep0..Dep35, recorded at
# the base as its frozen reference set (test/resources/coupling-fanout-refs.tsv).
SWEEPER_PATH = "worker/src/main/scala/services/readmodel/ReadModelSweeper.scala"
PLANNER_PATH = "worker/src/main/scala/services/readmodel/SweepPlanner.scala"
PASS_PATH = "worker/src/main/scala/services/readmodel/SweepPass.scala"
SWEEPER = "services.readmodel.ReadModelSweeper"
PLANNER = "services.readmodel.SweepPlanner"
PASS = "services.readmodel.SweepPass"


def deps(lo: int, hi: int) -> list:
    """Dep<lo>..Dep<hi - 1>: references the sweeper had at the base."""
    return [f"services.movies.Dep{i}" for i in range(lo, hi)]


SWEEPER_REFS = "".join(f"{SWEEPER}\t{d}\n" for d in deps(0, 36))


def member(name: str) -> str:
    return (f"  def {name}(corpus: Corpus, clock: Clock): Seq[Film] = {{\n"
            f"    val seen = corpus.films.filter(_.id.nonEmpty).map(f => f.copy(seenAt = clock.now))\n"
            f"    seen.sortBy(_.title).take(corpus.size) // {name}\n  }}\n")


PLANNED = ("planOrphans", "planStale", "planDuplicates", "planRetired", "planCutover", "planBudget")
PASSED = ("passProjection", "passPrune", "passCensus")


def scala(cls: str, names, extra: str = "") -> str:
    return f"package services.readmodel\n\nfinal class {cls}(deps: Deps) {{\n" + "".join(member(n) for n in names) + extra + "}\n"


def split_rows(*pieces: str, sweeper: int = None, readmodel_distance: int = 30, movies_distance: int = 48) -> str:
    kept = [f"fanout\t{SWEEPER}\t{sweeper}"] if sweeper else []
    return rows(*kept, *pieces, f"distance\tservices.readmodel\t{readmodel_distance}", f"distance\tservices.movies\t{movies_distance}",
                drop=("distance\tservices.movies",))


class CouplingSplitTest(RatchetHarness):
    """A frozen class split honestly: `Coupling: split of <X>`, the pieces taking over X's code, each strictly below X's
    frozen value, the union of what they and X's remainder reference within X's frozen reference set at the base --
    each piece frozen at its own value from then on, and the distance rows of the packages holding X or a piece let rise
    by 0.02 at most in that range."""

    def setUp(self):
        super().setUp()
        self.base = self.commit({BASELINE: split_rows(sweeper=36), REFS: SWEEPER_REFS,
                                 SWEEPER_PATH: scala("ReadModelSweeper", PLANNED + PASSED)}, "the sweeper, frozen at 36")

    def split(self, recorded: str, message: str, pass_text: str = None, sweeper_text=None) -> str:
        return self.commit({BASELINE: recorded, SWEEPER_PATH: sweeper_text, PLANNER_PATH: scala("SweepPlanner", PLANNED),
                            PASS_PATH: pass_text or scala("SweepPass", PASSED)}, message)

    def test_a_valid_split_passes_and_freezes_each_piece(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11")
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        out = self.assert_passes(recorded, rng=f"{self.base}..{head}", fanouts={PLANNER: deps(0, 17), PASS: deps(17, 28)})
        self.assertIn(f"split of {SWEEPER} (36) into {PASS} 11, {PLANNER} 17: their references (28) within {SWEEPER}'s 36", out)

    def test_two_pieces_sharing_the_originals_types_pass_though_their_sum_is_past_it(self):
        # the union rule: 20 + 17 = 37 past 36, yet both use only what the sweeper used -- no dependency added
        recorded = split_rows(f"fanout\t{PLANNER}\t20", f"fanout\t{PASS}\t17")
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        out = self.assert_passes(recorded, rng=f"{self.base}..{head}", fanouts={PLANNER: deps(0, 20), PASS: deps(10, 27)})
        self.assertIn("their references (27) within", out)

    def test_a_piece_adding_a_dependency_the_original_did_not_have_fails(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11")
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_fails(recorded, "A `Coupling: split of <X>` CLAIM IS REFUSED",
                          f"add what {SWEEPER} did not reference at", "services.web.Fresh", f"fanout {PLANNER} added at 17",
                          rng=f"{self.base}..{head}", fanouts={PLANNER: deps(0, 16) + ["services.web.Fresh"], PASS: deps(17, 28)})

    def test_the_pieces_references_to_each_other_and_the_original_are_internal(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t12")
        head = self.split(recorded, "split the sweeper\n\nCoupling: split of ReadModelSweeper into SweepPlanner, SweepPass",
                          sweeper_text=scala("ReadModelSweeper", ("sweep",)))
        self.assert_passes(recorded, rng=f"{self.base}..{head}",
                           fanouts={PLANNER: deps(0, 16) + [PASS], PASS: deps(17, 28) + [PLANNER], SWEEPER: [PLANNER, PASS]})

    def test_a_piece_git_sees_as_a_copy_takes_over_code_though_the_original_keeps_forwarders(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17")
        forwarders = "".join(f"  def {n}(corpus: Corpus, clock: Clock): Seq[Film] = planner.{n}(corpus, clock)\n"
                             for n in PLANNED + PASSED)
        head = self.commit({BASELINE: recorded, PLANNER_PATH: scala("SweepPlanner", PLANNED + PASSED),
                            SWEEPER_PATH: "package services.readmodel\n\nfinal class ReadModelSweeper(planner: SweepPlanner) {\n"
                            + forwarders + "}\n"}, f"the sweeper forwards\n\nCoupling: split of {SWEEPER} into SweepPlanner")
        self.assert_passes(recorded, rng=f"{self.base}..{head}", fanouts={PLANNER: deps(0, 17), SWEEPER: [PLANNER] + deps(0, 2)})

    def test_a_piece_of_two_moved_members_and_new_code_takes_over_code(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17")
        fresh = "".join(f"  private def helper{i}(x: Int): Int = x * {i} + {i * i}\n" for i in range(40))
        head = self.commit({BASELINE: recorded, PLANNER_PATH: scala("SweepPlanner", PLANNED[:2], fresh),
                            SWEEPER_PATH: scala("ReadModelSweeper", PLANNED[2:] + PASSED)},
                           f"two plans move\n\nCoupling: split of {SWEEPER}")
        fanouts = {PLANNER: deps(0, 17), SWEEPER: deps(17, 36)}
        self.assert_passes(recorded, rng=f"{self.base}..{head}", fanouts=fanouts)
        head = self.commit({PLANNER_PATH: scala("SweepPlanner", PLANNED[:1], fresh),
                            SWEEPER_PATH: scala("ReadModelSweeper", PLANNED[1:] + PASSED)}, "only one moves after all")
        self.assert_fails(recorded, f"{PLANNER} takes over none of {SWEEPER}'s code", rng=f"{self.base}..{head}", fanouts=fanouts)

    def test_what_the_original_keeps_is_held_too(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11")
        head = self.split(recorded, "split the sweeper\n\nCoupling: split of ReadModelSweeper into SweepPlanner, SweepPass",
                          sweeper_text=scala("ReadModelSweeper", ("sweep",)))
        self.assert_passes(recorded, rng=f"{self.base}..{head}",
                           fanouts={PLANNER: deps(0, 17), PASS: deps(17, 28), SWEEPER: deps(28, 36)})
        self.assert_fails(recorded, f"add what {SWEEPER} did not reference at", "services.web.Fresh", rng=f"{self.base}..{head}",
                          fanouts={PLANNER: deps(0, 17), PASS: deps(17, 28), SWEEPER: deps(28, 35) + ["services.web.Fresh"]})

    def test_a_contract_the_original_used_covers_its_members(self):
        # the same folding: the sweeper counted the `sweep` contract as one; a piece outside the contract's users names
        # its members one by one, which are still what the sweeper used
        contract = (EXEMPTION_ROWS + f"contract\t{SWEEPER}\tsweep\tworker\tthe sweep contract\n"
                    "member\tmodels.Film\tsweep\tworker\tfilm\nmember\tmodels.Showtime\tsweep\tworker\tscreening\n")
        refs = "".join(f"{SWEEPER}\t{d}\n" for d in deps(0, 35) + ["contract:sweep"])
        base = self.commit({EXEMPTIONS: contract, REFS: refs}, "the sweeper uses the sweep contract\n\nCoupling: a contract")
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t12")
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_passes(recorded, rng=f"{base}..{head}",
                           fanouts={PLANNER: deps(0, 17), PASS: deps(17, 27) + ["models.Film", "models.Showtime"]})

    def test_a_reference_set_out_of_step_with_the_frozen_row_refuses_the_split(self):
        base = self.commit({REFS: "".join(f"{SWEEPER}\t{d}\n" for d in deps(0, 35))}, "a stale reference set")
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11")
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_fails(recorded, f"{SWEEPER}'s recorded reference set at", "has 35, its row 36", rng=f"{base}..{head}",
                          fanouts={PLANNER: deps(0, 17), PASS: deps(17, 28)})

    def test_a_piece_without_its_measured_references_refuses_the_split(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11")
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_fails(recorded, f"{PASS}'s references are not measured", rng=f"{self.base}..{head}",
                          fanouts={PLANNER: deps(0, 17), PASS: 11})

    def test_a_piece_at_or_above_the_original_fails(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t36")
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_fails(recorded, f"{PLANNER} 36 is not below {SWEEPER}'s 36", f"fanout {PLANNER} added at 36",
                          rng=f"{self.base}..{head}", fanouts={PLANNER: deps(0, 36), PASS: []})

    def test_an_unrelated_new_class_claiming_split_credit_fails(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11")
        unrelated = scala("SweepPass", ("renderPoster", "postBeacon", "tallyVisits"))
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}", pass_text=unrelated,
                          sweeper_text=scala("ReadModelSweeper", PASSED))
        self.assert_fails(recorded, f"{PASS} takes over none of {SWEEPER}'s code", f"fanout {PASS} added at 11",
                          rng=f"{self.base}..{head}", fanouts={PLANNER: deps(0, 17), PASS: deps(17, 28), SWEEPER: []})

    def test_a_split_without_the_trailer_fails(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11")
        head = self.split(recorded, "split the sweeper\n\nCoupling: the sweeper was too big")
        self.assert_fails(recorded, f"fanout {PLANNER} added at 17", f"fanout {PASS} added at 11",
                          rng=f"{self.base}..{head}", fanouts={PLANNER: deps(0, 17), PASS: deps(17, 28)})

    def test_a_piece_rising_in_a_later_commit_fails(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11")
        split = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        risen = split_rows(f"fanout\t{PLANNER}\t18", f"fanout\t{PASS}\t11")
        fanouts = {PLANNER: deps(0, 18), PASS: deps(17, 28)}
        self.assert_fails(risen, f"class {PLANNER}: fan-out 18 past its frozen row 17", fanouts=fanouts)
        head = self.commit({BASELINE: risen}, f"record it\n\nCoupling: split of {SWEEPER}")
        self.assert_fails(risen, f"fanout {PLANNER} raised 17 -> 18", rng=f"{split}..{head}", fanouts=fanouts)

    def test_a_split_of_a_class_that_was_not_frozen_fails(self):
        # no row is added, so only the claim itself can fail: a trailer claiming a split that is not one is refused
        recorded = split_rows(sweeper=36)
        head = self.commit({BASELINE: recorded + "# touched\n", PLANNER_PATH: scala("SweepPlanner", PLANNED)},
                           "a planner\n\nCoupling: split of services.readmodel.Nothing")
        self.assert_fails(recorded, "A `Coupling: split of <X>` CLAIM IS REFUSED",
                          "services.readmodel.Nothing has no frozen fanout row", rng=f"{self.base}..{head}",
                          fanouts={PLANNER: 7, SWEEPER: 36})

    # THE DISTANCE TOLERANCE: a split moves types between packages' counts, so a package holding X or a piece may see
    # its frozen distance rise by 0.02 at most, re-frozen there; nowhere else, and never outside a valid split.
    VALID = {PLANNER: deps(0, 17), PASS: deps(17, 28)}

    def test_a_valid_split_may_raise_its_packages_distance_by_two_hundredths(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", readmodel_distance=32)
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_passes(recorded, rng=f"{self.base}..{head}", fanouts=self.VALID)

    def test_a_valid_split_raising_its_packages_distance_by_three_hundredths_fails(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", readmodel_distance=33)
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_fails(recorded, "distance services.readmodel raised 30 -> 33", rng=f"{self.base}..{head}", fanouts=self.VALID)

    def test_a_commit_that_is_no_split_raising_a_distance_fails(self):
        recorded = split_rows(sweeper=36, readmodel_distance=31)
        head = self.commit({BASELINE: recorded}, "the read model grew abstract\n\nCoupling: the sweeper was too big")
        self.assert_fails(recorded, "distance services.readmodel raised 30 -> 31", rng=f"{self.base}..{head}",
                          fanouts={SWEEPER: deps(0, 36)})

    def test_a_split_cannot_raise_another_packages_distance(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", movies_distance=49)
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_fails(recorded, "distance services.movies raised 48 -> 49", rng=f"{self.base}..{head}", fanouts=self.VALID)

    def test_a_refused_split_raises_no_distance(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", readmodel_distance=31)
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_fails(recorded, "distance services.readmodel raised 30 -> 31", rng=f"{self.base}..{head}",
                          fanouts={PLANNER: deps(0, 16) + ["services.web.Fresh"], PASS: deps(17, 28)})


class CouplingRefsTest(unittest.TestCase):
    """`refs`: each frozen class's reference set -- what --update records beside the baseline, and a later split's
    claim is held to."""

    def test_refs_are_the_frozen_classes_targets_one_a_line(self):
        with tempfile.TemporaryDirectory() as d:
            rows_path, fanouts_path = os.path.join(d, "rows.tsv"), os.path.join(d, "fanouts.tsv")
            with open(rows_path, "w") as f:
                f.write(split_rows(sweeper=36))
            with open(fanouts_path, "w") as f:
                f.write(fanout_line(SWEEPER, deps(0, 2)) + fanout_line(PLANNER, deps(5, 7)))
            done = subprocess.run([sys.executable, "-I", TOOL, "refs", "--current", rows_path, "--fanouts", fanouts_path],
                                  capture_output=True, text=True)
        self.assertEqual(0, done.returncode, done.stderr)
        self.assertEqual(f"{SWEEPER}\tservices.movies.Dep0\n{SWEEPER}\tservices.movies.Dep1\n", done.stdout)


if __name__ == "__main__":
    unittest.main()
