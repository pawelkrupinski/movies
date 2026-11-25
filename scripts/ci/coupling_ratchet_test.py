#!/usr/bin/env python3
"""coupling_ratchet.py: any package cycle edge fails, measured or recorded in the baseline, whatever the trailer; so
does measured coupling past the baseline (a new cross-module edge kind, a package's Ce past its row or the new-package
limit), a class newly past the fan-out limit or a large package newly past the distance limit (only an exemption
row admits one, never --update), a frozen fan-out or distance row rising, any package edge into a higher layer or, inside an ordered
layer, a higher rank (naming the direction allowed), and a package without its layer or rank; a baseline grown in a
range passes only when a commit changing it carries `Coupling: <why>`, and so does a layer table loosened in a range (an
`allow` row added, a package given another layer or rank) and an exemption table loosened (a row added, a limit
changed); falls pass and are said; a frozen class split honestly (`Coupling: split of <X>`, the pieces taking over X's
code, each below X's row, the union of what they and X's remainder reference within X's frozen reference set) freezes
its pieces, and lets the distance row of a package holding X, a piece or a class they use rise by 0.02 at most;
nothing else does.
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
        args = [sys.executable, "-I", "-B", TOOL, "check", "--current", path, "--baseline", BASELINE]
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

    def test_a_frozen_class_that_moved_keeps_its_row_once_not_for_two_new_classes(self):
        self.assert_fails(rows("fanout\tservices.corpus.MovieService\t30", "fanout\tservices.titles.MovieService\t30",
                               drop=("fanout",)), "past the limit 9")

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

    def test_a_package_measure_change_takes_the_distance_rows_again_keeping_their_ceilings(self):
        # deeper packages measured on their own (Coupling.PackageMeasure 2): services.movies' consumers now count its
        # sub-package apart, its true D 55 -- taken again under the trailer, its ceiling (48 + 2) kept
        measured = rows("limit\tdistance-measure\t2", "distance\tservices.movies\t55", "distance-cap\tservices.movies\t50",
                        drop=("distance",))
        head = self.commit({BASELINE: measured}, "re-measure\n\nCoupling: deeper packages measured on their own")
        self.assert_passes(measured, rng=f"{self.base}..{head}")
        raised = measured.replace("distance-cap\tservices.movies\t50", "distance-cap\tservices.movies\t57")
        head = self.commit({BASELINE: raised}, "re-measure, a fresh ceiling\n\nCoupling: deeper packages measured on their own")
        self.assert_fails(raised, "distance-cap services.movies raised 50 -> 57 -- a ceiling only falls", rng=f"{self.base}..{head}")

    def test_a_package_measure_change_without_a_trailer_fails(self):
        measured = rows("limit\tdistance-measure\t2", "distance\tservices.movies\t55", drop=("distance",))
        head = self.commit({BASELINE: measured}, "re-measure, no why")
        self.assert_fails(measured, "limit distance-measure raised 1 -> 2", "distance rows taken again under new limits",
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
        why = "venue table: one row a venue, each naming the scraper class that reads it"
        head = self.commit({EXEMPTIONS: EXEMPTION_ROWS + f"registry\tworker.Wiring\texempt\tworker\t{why}\t2026-11-19\n"},
                           f"exempt it\n\nCoupling: exempt Wiring — {why}")
        self.assert_passes(ROWS, rng=f"{self.base}..{head}")

    def test_an_exemption_trailer_folded_onto_a_continuation_line_passes(self):
        # git reads an indented continuation line as part of the trailer's value; the ratchet must read it whole
        why = "venue table: one row a venue, each naming the scraper class that reads it"
        folded = why.replace(" each naming", "\n each naming")
        head = self.commit({EXEMPTIONS: EXEMPTION_ROWS + f"registry\tworker.Wiring\texempt\tworker\t{why}\t2026-11-19\n"},
                           f"exempt it\n\nCoupling: exempt Wiring — {folded}")
        self.assert_passes(ROWS, rng=f"{self.base}..{head}")

    def test_an_exemption_limit_raised_without_a_trailer_fails(self):
        first = self.commit({EXEMPTIONS: EXEMPTION_ROWS + "config\tsettings.Config\t40\tcoupling\tthe root\n"},
                            "limit it\n\nCoupling: the typed-config root")
        head = self.commit({EXEMPTIONS: EXEMPTION_ROWS + "config\tsettings.Config\t60\tcoupling\tthe root\n"}, "raise it")
        self.assert_fails(ROWS, "config settings.Config changed 40 -> 60", rng=f"{first}..{head}")

    def test_an_exemption_row_dropped_needs_no_trailer(self):
        head = self.commit({EXEMPTIONS: "# empty\n"}, "no roots exempt")
        self.assert_passes(ROWS, rng=f"{self.base}..{head}")

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

    def test_a_batch_where_only_the_other_commit_carries_the_trailer_fails_naming_the_raiser(self):
        own = rows("ce\tmodels\t14", drop=("ce\tmodels",))
        self.commit({BASELINE: own}, "change A: models reads the clock\n\nCoupling: the clock is the one seam")
        both = own + "module-edge\tcommon->worker\t1\n"
        head = self.commit({BASELINE: both}, "change B: use the worker from common")
        self.assert_fails(both, "THE COUPLING BASELINE GREW", "module-edge common->worker added",
                          f"{head[:9]} change B: use the worker from common: module-edge common->worker added",
                          rng=f"{self.base}..{head}")

    def test_a_batch_whose_commits_each_justify_their_own_rise_passes(self):
        own = rows("ce\tmodels\t14", drop=("ce\tmodels",))
        self.commit({BASELINE: own}, "change A\n\nCoupling: the clock is the one seam")
        both = own + "module-edge\tcommon->worker\t1\n"
        head = self.commit({BASELINE: both}, "change B\n\nCoupling: the worker's one entry point")
        self.assert_passes(both, rng=f"{self.base}..{head}")

    def test_a_batch_of_a_justified_rise_and_an_unrelated_trailerless_change_passes(self):
        own = rows("ce\tmodels\t14", drop=("ce\tmodels",))
        self.commit({BASELINE: own}, "change A\n\nCoupling: the clock is the one seam")
        head = self.commit({"README": "x\n"}, "change B: docs")
        self.assert_passes(own, rng=f"{self.base}..{head}")

    def test_a_batch_where_only_the_other_commit_carries_the_trailer_fails_for_the_layer_table(self):
        own = LAYER_ROWS + "allow\tservices.movies->controllers\tthe view reads it\n"
        self.commit({LAYERS: own}, "change A: allow it\n\nCoupling: the view model is the one seam")
        head = self.commit({LAYERS: own + "allow\tmodels->controllers\tthe models read it\n"}, "change B: allow more")
        self.assert_fails(ROWS, "THE LAYER TABLE LOOSENED",
                          f"{head[:9]} change B: allow more: allow models->controllers added", rng=f"{self.base}..{head}")

    def test_a_batch_where_only_the_other_commit_carries_the_trailer_fails_for_the_exemption_table(self):
        own = EXEMPTION_ROWS + "registry\tworker.Wiring\texempt\tworker\ta table\n"
        self.commit({EXEMPTIONS: own}, "change A: exempt it\n\nCoupling: a venue table, one row a venue")
        head = self.commit({EXEMPTIONS: own + "registry\tweb.Wiring\texempt\tweb\ta table\n"}, "change B: exempt more")
        self.assert_fails(ROWS, "THE EXEMPTION TABLE LOOSENED",
                          f"{head[:9]} change B: exempt more: registry web.Wiring (exempt) added", rng=f"{self.base}..{head}")

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


def split_rows(*pieces: str, sweeper: int = None, readmodel_distance: int = 30, movies_distance: int = 48,
               web_distance: int = 40) -> str:
    kept = [f"fanout\t{SWEEPER}\t{sweeper}"] if sweeper else []
    return rows(*kept, *pieces, f"distance\tservices.readmodel\t{readmodel_distance}", f"distance\tservices.movies\t{movies_distance}",
                f"distance\tservices.web\t{web_distance}", drop=("distance\tservices.movies",))


def cap(pkg: str, value: int) -> str:
    """A ceiling row (THE CEILING): what --update writes after each frozen distance row."""
    return f"distance-cap\t{pkg}\t{value}"


def frozen_sweeper(harness: RatchetHarness) -> str:
    """The split tests' base: the sweeper frozen at 36, its references recorded."""
    return harness.commit({BASELINE: split_rows(sweeper=36), REFS: SWEEPER_REFS,
                           SWEEPER_PATH: scala("ReadModelSweeper", PLANNED + PASSED)}, "the sweeper, frozen at 36")


# A RENAME (the owner, 2026-11-19, "rename credit"): the 2026-11-19 renames the ratchet refused -- seven frozen identity
# classes given names that say what each decides or makes (c72701244, c507af9ab and the FilmRecords lane), each a `git mv`.
IDENTITY = "identity/src/main/scala/services/identity"
REAL_RENAMES = (("ListingsApart", "ListingCannotLinks", 15), ("SourcesApart", "SourceCannotLinks", 13),
                ("MembersApart", "NoFilmMembers", 12), ("ClusterRead", "ClusterDecisionFacts", 16),
                ("ProjectionDrafting", "ScopeDrafting", 28), ("VenueSlotBuilds", "LeanVenueSlots", 19),
                ("FilmRecordDrafts", "FilmRecords", 20))


def identity_class(cls: str, names=None) -> str:
    """A frozen identity class, its members named after it unless given (a rename keeps the old class's)."""
    names = names or tuple(f"{cls[0].lower()}{cls[1:]}{n[0].upper()}{n[1:]}" for n in PLANNED + PASSED)
    return (f"package services.identity\n\nprivate[identity] final class {cls}(deps: Deps) {{\n"
            + "".join(member(n) for n in names) + "}\n")


def identity_path(cls: str) -> str:
    return f"{IDENTITY}/{cls}.scala"


def members_of(cls: str) -> tuple:
    return tuple(f"{cls[0].lower()}{cls[1:]}{n[0].upper()}{n[1:]}" for n in PLANNED + PASSED)


def frozen_identity(*values) -> str:
    return rows(*(f"fanout\tservices.identity.{cls}\t{v}" for cls, v in values))


class CouplingRenameTest(RatchetHarness):
    """A frozen class renamed -- git sees its source renamed, or scripts/pure-move-check calls the commit a PURE MOVE --
    keeps its row under the new name, at or below the old value; a rise, one name gone to two, or a split posing as a
    rename does not."""

    def setUp(self):
        super().setUp()
        self.base = self.commit({BASELINE: frozen_identity(*((old, v) for old, _, v in REAL_RENAMES)),
                                 **{identity_path(old): identity_class(old) for old, _, _ in REAL_RENAMES}}, "the frozen identity classes")

    def rename(self, pairs, message="rename", values=None, extra=None):
        values = values or {}
        kept = [(pairs.get(old, old), values.get(pairs.get(old, old), v)) for old, _, v in REAL_RENAMES]
        recorded = frozen_identity(*kept, *((extra or {}).items()))
        files = {BASELINE: recorded}
        for old, new in pairs.items():
            files[identity_path(old)] = None
            files[identity_path(new)] = identity_class(new, members_of(old))
        return recorded, self.commit(files, message)

    def test_a_pure_rename_keeps_the_frozen_row_under_the_new_name(self):
        recorded, head = self.rename({"ListingsApart": "ListingCannotLinks"})
        out = self.assert_passes(recorded, rng=f"{self.base}..{head}")
        self.assertIn("fanout services.identity.ListingsApart (15) renamed to services.identity.ListingCannotLinks"
                      f" (git renames {identity_path('ListingsApart')} to {identity_path('ListingCannotLinks')})", out)

    def test_a_rename_at_a_lower_value_keeps_the_lower_row(self):
        recorded, head = self.rename({"ListingsApart": "ListingCannotLinks"}, values={"ListingCannotLinks": 12})
        self.assert_passes(recorded, rng=f"{self.base}..{head}")

    def test_a_rename_whose_value_rises_fails(self):
        recorded, head = self.rename({"ListingsApart": "ListingCannotLinks"}, values={"ListingCannotLinks": 16})
        self.assert_fails(recorded, "fanout services.identity.ListingCannotLinks added at 16",
                          "but 16 is past services.identity.ListingsApart's frozen row 15", rng=f"{self.base}..{head}")

    def test_a_rename_not_yet_recorded_in_the_baseline_passes_against_the_measured_rows(self):
        _, head = self.rename({"ListingsApart": "ListingCannotLinks"})
        self.commit({BASELINE: frozen_identity(*((old, v) for old, _, v in REAL_RENAMES))}, "the baseline as it was")
        measured = frozen_identity(*((("ListingCannotLinks" if old == "ListingsApart" else old), v) for old, _, v in REAL_RENAMES))
        self.assert_passes(measured, rng=f"{self.base}..{head}")

    def test_a_new_class_without_a_rename_gets_no_credit(self):
        recorded = frozen_identity(*((old, v) for old, _, v in REAL_RENAMES if old != "ListingsApart"), ("ListingCannotLinks", 15))
        head = self.commit({BASELINE: recorded, identity_path("ListingCannotLinks"): identity_class("ListingCannotLinks", ("other", "unrelated"))},
                           "a new class, the old one dropped from the baseline")
        self.assert_fails(recorded, "fanout services.identity.ListingCannotLinks added at 15", rng=f"{self.base}..{head}")

    def test_a_split_posing_as_a_rename_gets_no_rename_credit(self):
        # the old file renamed (git: R) keeping most of the code, a second new class taking two of its members
        recorded = frozen_identity(*((old, v) for old, _, v in REAL_RENAMES if old != "ListingsApart"),
                                   ("ListingCannotLinks", 14), ("ListingPairs", 10))
        head = self.commit({BASELINE: recorded, identity_path("ListingsApart"): None,
                            identity_path("ListingCannotLinks"): identity_class("ListingCannotLinks", members_of("ListingsApart")[:7]),
                            identity_path("ListingPairs"): identity_class("ListingPairs", members_of("ListingsApart")[7:])}, "split it, call it a rename")
        self.assert_fails(recorded, "a split posing as a rename", "fanout services.identity.ListingCannotLinks added at 14",
                          rng=f"{self.base}..{head}")

    def test_one_name_gone_to_two_gets_no_rename_credit(self):
        recorded = frozen_identity(*((old, v) for old, _, v in REAL_RENAMES if old != "ListingsApart"),
                                   ("ListingCannotLinks", 14), ("ListingPairs", 10))
        both = identity_class("ListingCannotLinks", members_of("ListingsApart")) + "\nprivate[identity] final class ListingPairs(deps: Deps)\n"
        head = self.commit({BASELINE: recorded, identity_path("ListingsApart"): None, identity_path("ListingCannotLinks"): both},
                           "two classes in the renamed file")
        self.assert_fails(recorded, "a rename has one new name", rng=f"{self.base}..{head}")

    def test_a_split_claim_of_a_rename_reads_as_the_rename(self):
        # c507af9ab's trailers: `split of ProjectionDrafting into ScopeDrafting`, written to get a rename's row across
        recorded, head = self.rename({"ListingsApart": "ListingCannotLinks"},
                                     message="rename\n\nCoupling: split of ListingsApart into ListingCannotLinks")
        self.assertNotIn("CLAIM IS REFUSED", self.assert_passes(recorded, rng=f"{self.base}..{head}"))
        recorded, head = self.rename({"SourcesApart": "SourceCannotLinks"}, values={"SourceCannotLinks": 14},
                                     message="rename\n\nCoupling: split of SourcesApart into SourceCannotLinks")
        self.assert_fails(recorded, "A `Coupling: split of <X>` CLAIM IS REFUSED", rng=f"{head}^..{head}")

    def test_a_pure_move_commit_credits_a_rename_git_no_longer_sees_over_the_range(self):
        recorded, moved = self.rename({"ListingsApart": "ListingCannotLinks"}, "the pure rename")
        rewritten = recorded.replace("ListingCannotLinks\t15", "ListingCannotLinks\t11")
        head = self.commit({BASELINE: rewritten, identity_path("ListingCannotLinks"):
                            "package services.identity\n\nprivate[identity] final class ListingCannotLinks(a: A) {\n"
                            "  def decide(x: X): Boolean = a.cannotLink(x)\n}\n"}, "rewrite it smaller")
        out = self.assert_passes(rewritten, rng=f"{self.base}..{head}")
        self.assertIn(f"scripts/pure-move-check calls {moved[:9]} a PURE MOVE", out)

    def test_a_commit_that_is_no_pure_move_credits_no_rename_git_no_longer_sees(self):
        recorded = frozen_identity(*((("ListingCannotLinks" if old == "ListingsApart" else old), v) for old, _, v in REAL_RENAMES))
        changed = identity_class("ListingCannotLinks", members_of("ListingsApart")).replace("take(corpus.size)", "take(3)")
        self.commit({BASELINE: recorded, identity_path("ListingsApart"): None, identity_path("ListingCannotLinks"): changed},
                    "renamed, and its behaviour changed")
        rewritten = recorded.replace("ListingCannotLinks\t15", "ListingCannotLinks\t11")
        head = self.commit({BASELINE: rewritten, identity_path("ListingCannotLinks"):
                            "package services.identity\n\nprivate[identity] final class ListingCannotLinks(a: A) {\n"
                            "  def decide(x: X): Boolean = a.cannotLink(x)\n}\n"}, "rewrite it smaller")
        self.assert_fails(rewritten, "fanout services.identity.ListingCannotLinks added at 11", rng=f"{self.base}..{head}")

    def test_the_real_renames_of_2026_11_19_keep_their_rows(self):
        recorded, head = self.rename({old: new for old, new, _ in REAL_RENAMES},
                                     "Resolver pieces named for what they decide; ScopeDrafting, LeanVenueSlots, FilmRecords")
        out = self.assert_passes(recorded, rng=f"{self.base}..{head}")
        for old, new, value in REAL_RENAMES:
            self.assertIn(f"fanout services.identity.{old} ({value}) renamed to services.identity.{new}", out)

    def test_a_renamed_package_keeps_its_distance_row_and_ceiling(self):
        base = self.commit({BASELINE: frozen_identity() + "distance\tservices.identity\t30\n" + cap("services.identity", 30) + "\n"},
                           "frozen identity distance")
        files = {BASELINE: frozen_identity() + "distance\tservices.resolve\t30\n" + cap("services.resolve", 30) + "\n"}
        for old, _, _ in REAL_RENAMES:
            files[identity_path(old)] = None
            files[f"identity/src/main/scala/services/resolve/{old}.scala"] = identity_class(old).replace(
                "package services.identity", "package services.resolve").replace("private[identity]", "private[resolve]")
        head = self.commit(files, "the package renamed")
        self.assert_passes(files[BASELINE], rng=f"{base}..{head}")
        loose = files[BASELINE].replace(cap("services.resolve", 30), cap("services.resolve", 32))
        head = self.commit({BASELINE: loose}, "--update wrote a fresh ceiling")
        self.assert_fails(loose, "distance-cap services.resolve 32 is past services.identity's 30", rng=f"{base}..{head}")
        raised = files[BASELINE].replace("services.resolve\t30\n" + "distance-cap", "services.resolve\t31\n" + "distance-cap")
        head = self.commit({BASELINE: raised}, "it rose")
        self.assert_fails(raised, "distance services.resolve added at 31", "but 31 is past services.identity's frozen row 30",
                          rng=f"{base}..{head}")

    def test_a_renamed_deeper_package_with_a_row_of_its_own_keeps_its_distance_row(self):
        # services.identity.agreement is measured as a package of its own (its `rank` row), so its sources moving to
        # services.identity.voting (ranked in its place) rename IT -- not services.identity into services.identity
        def ranked(pkg):
            return LAYER_ROWS + f"rank\t2\t{pkg}\tover the model\n"
        def sources(pkg):
            return {f"identity/src/main/scala/{pkg.replace('.', '/')}/{old}.scala": identity_class(old).replace(
                "package services.identity", f"package {pkg}") for old, _, _ in REAL_RENAMES}
        before = frozen_identity() + "distance\tservices.identity.agreement\t30\n" + cap("services.identity.agreement", 30) + "\n"
        base = self.commit({BASELINE: before, LAYERS: ranked("services.identity.agreement"),
                            **sources("services.identity.agreement")}, "the agreement measured on its own")
        after = frozen_identity() + "distance\tservices.identity.voting\t30\n" + cap("services.identity.voting", 30) + "\n"
        files = {BASELINE: after, LAYERS: ranked("services.identity.voting"), **sources("services.identity.voting")}
        files.update({path: None for path in sources("services.identity.agreement")})
        head = self.commit(files, "the agreement renamed")
        out = self.assert_passes(after, rng=f"{base}..{head}")
        self.assertIn("distance services.identity.agreement (30) renamed to services.identity.voting", out)


# A FAN-OUT ROW ADDED (the owner, 2026-11-19: "3 a yes with specific reason"): an agent may exempt a genuinely new class
# past the fan-out limit -- with a specific why, its date, and the commit's own `Coupling: exempt <Class> — <reason>`.
STAGE = "services.identity.Clustering"
STAGE_WHY = "pipeline stage: clusters a family's members; needs the evidence, edges and solver it sequences"


def class_row(why: str = STAGE_WHY, added: str = "\t2026-11-19", cls: str = STAGE) -> str:
    return f"class\t{cls}\texempt\tidentity\t{why}{added}\n"


# A PACKAGE ROW (the owner, 2026-11-20): one package's distance exempted by its role, held like a class row.
BUNDLES_WHY = "metric bundles: each registers its own Prometheus series and no caller swaps one for another"


def package_row(why: str = BUNDLES_WHY, added: str = "\t2026-11-20") -> str:
    return f"package\tservices.metrics\texempt\tcoupling\t{why}{added}\n"


class ExemptionAdmissionTest(RatchetHarness):
    def exempt(self, row: str, message: str) -> str:
        return self.commit({EXEMPTIONS: EXEMPTION_ROWS + row}, message)

    def test_a_row_with_a_specific_reason_and_its_trailer_passes(self):
        head = self.exempt(class_row(), f"Clustering exempt\n\nCoupling: exempt Clustering — {STAGE_WHY}")
        self.assert_passes(ROWS, rng=f"{self.base}..{head}")

    def test_the_trailer_may_name_the_class_in_full_and_use_two_hyphens(self):
        head = self.exempt(class_row(), f"Clustering exempt\n\nCoupling: exempt {STAGE} -- {STAGE_WHY}")
        self.assert_passes(ROWS, rng=f"{self.base}..{head}")

    def test_a_generic_reason_fails(self):
        why = "resolver class: it is complex and all of its collaborators are needed here"
        head = self.exempt(class_row(why), f"Clustering exempt\n\nCoupling: exempt Clustering — {why}")
        self.assert_fails(ROWS, "AN EXEMPTION ROW IS NOT ADMITTED", "`complex` is generic", "`needed` is generic",
                          rng=f"{self.base}..{head}")

    def test_the_class_name_as_the_whole_reason_fails(self):
        head = self.exempt(class_row("Clustering"), f"Clustering exempt\n\nCoupling: exempt Clustering — {STAGE_WHY}")
        self.assert_fails(ROWS, "the class's name is the whole reason", "lead with the class's role", rng=f"{self.base}..{head}")

    def test_a_short_reason_fails(self):
        head = self.exempt(class_row("stage: runs the Clustering"), f"Clustering exempt\n\nCoupling: exempt Clustering — {STAGE_WHY}")
        self.assert_fails(ROWS, "too short: 40+ characters and 6+ words besides the class's name", rng=f"{self.base}..{head}")

    def test_a_missing_trailer_fails(self):
        head = self.exempt(class_row(), "Clustering exempt\n\nCoupling: a pipeline stage")
        self.assert_fails(ROWS, "no `Coupling: exempt Clustering — <reason>` trailer in the commit's final trailer block",
                          rng=f"{self.base}..{head}")

    def test_a_trailer_outside_the_final_block_fails(self):
        head = self.exempt(class_row(), f"Clustering exempt\n\nCoupling: exempt Clustering — {STAGE_WHY}\n\n"
                                        "Co-Authored-By: someone <someone@example.com>")
        self.assert_fails(ROWS, "no `Coupling: exempt Clustering", rng=f"{self.base}..{head}")

    def test_a_generic_trailer_reason_fails(self):
        head = self.exempt(class_row(), "Clustering exempt\n\nCoupling: exempt Clustering — needed")
        self.assert_fails(ROWS, "its trailer's reason ('needed')", rng=f"{self.base}..{head}")

    def test_a_package_row_with_a_specific_reason_and_its_trailer_passes(self):
        head = self.exempt(package_row(), f"metrics exempt\n\nCoupling: exempt services.metrics — {BUNDLES_WHY}")
        self.assert_passes(ROWS, rng=f"{self.base}..{head}")

    def test_a_package_row_is_held_like_a_class_row(self):
        head = self.exempt(package_row(added=""), "metrics exempt\n\nCoupling: a metrics package")
        self.assert_fails(ROWS, "AN EXEMPTION ROW IS NOT ADMITTED", "no `added` date", "no `Coupling: exempt metrics — <reason>` trailer",
                          rng=f"{self.base}..{head}")

    def test_a_row_without_its_date_fails(self):
        head = self.exempt(class_row(added=""), f"Clustering exempt\n\nCoupling: exempt Clustering — {STAGE_WHY}")
        self.assert_fails(ROWS, "no `added` date", rng=f"{self.base}..{head}")

    def test_the_weekly_report_lists_each_new_row_with_its_reason_and_commit(self):
        head = self.exempt(class_row(), f"Clustering exempt\n\nCoupling: exempt Clustering — {STAGE_WHY}")
        voting = class_row("pipeline stage: votes a cluster's film from the scored candidates it gathers", cls="services.identity.Voting")
        later = self.commit({EXEMPTIONS: EXEMPTION_ROWS + voting}, "Voting exempt, Clustering struck\n\nCoupling: exempt Voting — "
                            "pipeline stage: votes a cluster's film from the scored candidates it gathers")
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        done = subprocess.run([sys.executable, "-I", "-B", TOOL, "new-exemptions", "--since", "7d", "--rev", "HEAD"],
                              cwd=self.repo,
                              env=env, capture_output=True, text=True)
        self.assertEqual(0, done.returncode, done.stdout + done.stderr)
        self.assertIn(f"- 2026-11-19 {head[:9]} `class` **{STAGE}** (struck since) (exempt, identity): {STAGE_WHY}", done.stdout)
        self.assertIn(f"  trailer: Coupling: exempt Clustering — {STAGE_WHY}", done.stdout)
        self.assertIn(f"{later[:9]} `class` **services.identity.Voting** (exempt", done.stdout)


class CouplingSplitTest(RatchetHarness):
    """A frozen class split honestly: `Coupling: split of <X>`, the pieces taking over X's code, each strictly below X's
    frozen value, the union of what they and X's remainder reference within X's frozen reference set at the base --
    each piece frozen at its own value from then on, and the distance rows of the packages holding X, a piece or a class
    they use let rise by 0.02 at most in that range."""

    def setUp(self):
        super().setUp()
        self.base = frozen_sweeper(self)

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
    # its frozen distance rise by 0.02 at most, re-frozen there; so may a package the split's references reach (each
    # piece is one more outside class using it: its Ca rises, its instability falls); nowhere else, and never outside a
    # valid split.
    VALID = {PLANNER: deps(0, 17), PASS: deps(17, 28)}

    def test_a_valid_split_may_raise_its_packages_distance_by_two_hundredths(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", readmodel_distance=32) + cap("services.readmodel", 32) + "\n"
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

    def test_a_valid_split_may_raise_the_distance_of_a_package_it_uses_by_two_hundredths(self):
        # services.movies holds the sweeper's Dep*: two pieces using them are two outside classes where one was
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", movies_distance=50) + cap("services.movies", 50) + "\n"
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_passes(recorded, rng=f"{self.base}..{head}", fanouts=self.VALID)

    def test_a_valid_split_raising_a_used_packages_distance_by_three_hundredths_fails(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", movies_distance=51)
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_fails(recorded, "distance services.movies raised 48 -> 51", rng=f"{self.base}..{head}", fanouts=self.VALID)

    def test_a_split_cannot_raise_the_distance_of_a_package_it_does_not_reach(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", web_distance=41)
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_fails(recorded, "distance services.web raised 40 -> 41", rng=f"{self.base}..{head}", fanouts=self.VALID)

    def test_a_refused_split_raises_no_distance(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", readmodel_distance=31)
        head = self.split(recorded, f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_fails(recorded, "distance services.readmodel raised 30 -> 31", rng=f"{self.base}..{head}",
                          fanouts={PLANNER: deps(0, 16) + ["services.web.Fresh"], PASS: deps(17, 28)})


class SplitCheckTest(RatchetHarness):
    """`split-check` (scripts/coupling-report --check-split): the working tree's split judged by the ratchet's own
    split_credit and frozen_growth before any commit carries the trailer -- and the ratchet, once it is committed with
    the trailer the tool names, agrees, accepting what it accepted and refusing what it refused."""

    VALID = CouplingSplitTest.VALID

    def setUp(self):
        super().setUp()
        self.base = frozen_sweeper(self)

    def write(self, files):
        for path, text in files.items():
            full = os.path.join(self.repo, path)
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "w") as f:
                f.write(text)

    def split_check(self, current, fanouts, *pieces):
        current_path, fanouts_path = os.path.join(self.dir.name, "current.tsv"), os.path.join(self.dir.name, "fanouts.tsv")
        with open(current_path, "w") as f:
            f.write(current)
        with open(fanouts_path, "w") as f:
            f.write("".join(fanout_line(c, n) for c, n in fanouts.items()))
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1", GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@t",
                   GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@t")
        args = [sys.executable, "-I", "-B", TOOL, "split-check", "--current", current_path, "--fanouts", fanouts_path,
                "--split", "ReadModelSweeper", "--base", self.base]
        for p in pieces:
            args += ["--into", p]
        done = subprocess.run(args, cwd=self.repo, env=env, capture_output=True, text=True)
        return done.returncode, done.stdout + done.stderr

    def uncommitted_split(self):
        self.write({PLANNER_PATH: scala("SweepPlanner", PLANNED), PASS_PATH: scala("SweepPass", PASSED)})
        os.remove(os.path.join(self.repo, SWEEPER_PATH))

    def test_an_honest_split_in_the_working_tree_is_accepted_and_the_ratchet_agrees(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", readmodel_distance=32)
        self.uncommitted_split()
        rc, out = self.split_check(recorded, self.VALID, "SweepPlanner", "SweepPass")
        self.assertEqual(0, rc, out)
        for said in (f"piece {PLANNER}: fan-out 17 -- below 36", f"piece {PASS}: fan-out 11 -- below 36",
                     f"of them outside {SWEEPER}'s set: 0 (a subset)", "distance services.readmodel: 30 -> 32 -- rose within",
                     f"commit it with: Coupling: split of {SWEEPER} into {PLANNER}, {PASS}"):
            self.assertIn(said, out)
        recorded += cap("services.readmodel", 32) + "\n"  # as --update writes it
        head = self.commit({BASELINE: recorded}, f"split the sweeper\n\nCoupling: split of {SWEEPER} into {PLANNER}, {PASS}")
        self.assert_passes(recorded, rng=f"{self.base}..{head}", fanouts=self.VALID)

    def test_a_piece_adding_a_dependency_is_refused_and_the_ratchet_agrees(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11")
        fanouts = {PLANNER: deps(0, 16) + ["services.web.Fresh"], PASS: deps(17, 28)}
        self.uncommitted_split()
        rc, out = self.split_check(recorded, fanouts, "SweepPlanner", "SweepPass")
        self.assertEqual(1, rc, out)
        for said in ("REFUSED: split of", "outside", "services.web.Fresh", f"REFUSED: fanout {PLANNER} added at 17",
                     "the union rule", "scripts/coupling-report --check-split"):
            self.assertIn(said, out)
        head = self.commit({BASELINE: recorded}, f"split the sweeper\n\nCoupling: split of {SWEEPER} into {PLANNER}, {PASS}")
        self.assert_fails(recorded, "A `Coupling: split of <X>` CLAIM IS REFUSED", "services.web.Fresh",
                          "scripts/coupling-report --check-split", rng=f"{self.base}..{head}", fanouts=fanouts)

    def test_a_distance_rise_past_the_tolerance_is_refused_and_the_ratchet_agrees(self):
        recorded = split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", readmodel_distance=33)
        self.uncommitted_split()
        rc, out = self.split_check(recorded, self.VALID, "SweepPlanner", "SweepPass")
        self.assertEqual(1, rc, out)
        self.assertIn("distance services.readmodel: 30 -> 33 -- rose: refused", out)
        self.assertIn("REFUSED: distance services.readmodel raised 30 -> 33", out)
        head = self.commit({BASELINE: recorded}, f"split the sweeper\n\nCoupling: split of {SWEEPER} into {PLANNER}, {PASS}")
        self.assert_fails(recorded, "distance services.readmodel raised 30 -> 33", rng=f"{self.base}..{head}", fanouts=self.VALID)

    def test_the_check_leaves_the_index_and_refs_untouched(self):
        self.uncommitted_split()
        status = lambda: self.scratch("scratch_git", "status", "--porcelain") + self.scratch("scratch_git", "rev-parse", "HEAD")
        before = status()
        self.split_check(split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11"), self.VALID, "SweepPlanner", "SweepPass")
        self.assertEqual(before, status())


class DistanceCeilingTest(RatchetHarness):
    """THE CEILING (the owner, 2026-11-19): each frozen distance row has a fixed ceiling, its value when the cap came in
    + 0.02 (a `distance-cap` row; a row without one is read at its own value + 0.02). A split may raise D by its 0.02
    only while D stays at or under the ceiling; a fall lowers the ceiling with the row (min(ceiling, row + 2)), so it
    only tightens; `update` (what scripts/coupling-report --update writes) carries it, and over a conflicted baseline
    takes the minimum of every side's."""

    VALID = CouplingSplitTest.VALID

    def setUp(self):
        super().setUp()
        self.base = frozen_sweeper(self)

    def update(self, measured: str) -> str:
        """`update` over the measured rows in the scratch repository: the baseline it writes."""
        path = os.path.join(self.dir.name, "measured.tsv")
        with open(path, "w") as f:
            f.write(measured)
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        done = subprocess.run([sys.executable, "-I", "-B", TOOL, "update", "--current", path, "--baseline", BASELINE],
                              cwd=self.repo, env=env, capture_output=True, text=True)
        self.assertEqual(0, done.returncode, done.stdout + done.stderr)
        return self.baseline()

    def baseline(self) -> str:
        with open(os.path.join(self.repo, BASELINE)) as f:
            return f.read()

    def split(self, readmodel_distance: int) -> str:
        """The sweeper split honestly into its two pieces, the read model's D measured at `readmodel_distance`, the
        baseline `update` writes committed: the range's head."""
        recorded = self.update(split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", readmodel_distance=readmodel_distance))
        return self.commit({BASELINE: recorded, SWEEPER_PATH: None, PLANNER_PATH: scala("SweepPlanner", PLANNED),
                            PASS_PATH: scala("SweepPass", PASSED)}, f"split the sweeper\n\nCoupling: split of {SWEEPER}")

    def refreeze(self) -> str:
        """Another frozen sweeper on top of the recorded baseline: the next split's base."""
        kept = [line for line in self.baseline().splitlines()
                if not line.startswith(f"fanout\t{PLANNER}") and not line.startswith(f"fanout\t{PASS}")]
        return self.commit({BASELINE: "\n".join(kept + [f"fanout\t{SWEEPER}\t36"]) + "\n", PLANNER_PATH: None, PASS_PATH: None,
                            SWEEPER_PATH: scala("ReadModelSweeper", PLANNED + PASSED)}, "another frozen sweeper")

    def test_three_splits_in_a_row_cannot_raise_a_distance_past_its_ceiling(self):
        # the read model frozen at 30: its ceiling 32. Each split's +0.01 is within the split's 0.02, but the third
        # would take D past the ceiling.
        head = self.split(31)
        self.assert_passes(self.baseline(), rng=f"{self.base}..{head}", fanouts=self.VALID)
        self.assertIn(cap("services.readmodel", 32), self.baseline())
        base = self.refreeze()
        head = self.split(32)
        self.assert_passes(self.baseline(), rng=f"{base}..{head}", fanouts=self.VALID)
        self.assertIn(cap("services.readmodel", 32), self.baseline())
        base = self.refreeze()
        head = self.split(33)
        self.assert_fails(self.baseline(), "distance services.readmodel raised 32 -> 33, past its ceiling 32",
                          "free 0.01 of its D", "a leaf value type", rng=f"{base}..{head}", fanouts=self.VALID)

    def test_a_fall_tightens_the_ceiling(self):
        fell = self.update(split_rows(sweeper=36, readmodel_distance=27))
        self.assertIn(cap("services.readmodel", 29), fell)
        base = self.commit({BASELINE: fell}, "the read model fell")
        self.assert_passes(fell, rng=f"{self.base}..{base}")
        head = self.split(29)
        self.assert_passes(self.baseline(), rng=f"{base}..{head}", fanouts=self.VALID)
        base = self.refreeze()
        head = self.split(30)  # the old ceiling, 32, would have let it through
        self.assert_fails(self.baseline(), "distance services.readmodel raised 29 -> 30, past its ceiling 29",
                          rng=f"{base}..{head}", fanouts=self.VALID)

    def test_a_ceiling_far_above_its_row_still_lets_one_split_add_only_two_hundredths(self):
        base = self.commit({BASELINE: split_rows(sweeper=36) + cap("services.readmodel", 40) + "\n"}, "a loose ceiling")
        head = self.commit({BASELINE: split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", readmodel_distance=33)
                            + cap("services.readmodel", 35) + "\n", SWEEPER_PATH: None,
                            PLANNER_PATH: scala("SweepPlanner", PLANNED), PASS_PATH: scala("SweepPass", PASSED)},
                           f"split the sweeper\n\nCoupling: split of {SWEEPER}")
        self.assert_fails(self.baseline(), "distance services.readmodel raised 30 -> 33", rng=f"{base}..{head}",
                          fanouts=self.VALID)

    def test_update_keeps_the_ceiling(self):
        base = self.commit({BASELINE: split_rows(sweeper=36, readmodel_distance=31) + cap("services.readmodel", 32) + "\n"},
                           "a ceiling below the row's own + 2")
        written = self.update(split_rows(sweeper=36, readmodel_distance=31))
        self.assertIn(cap("services.readmodel", 32), written)
        self.assertNotIn(cap("services.readmodel", 33), written)
        self.assertIn(cap("services.movies", 50), written)  # a row without a ceiling is read at its own + 2
        self.assertTrue(written.startswith("# a comment\n"), written)
        head = self.commit({BASELINE: written}, "recorded")
        self.assert_passes(written, rng=f"{base}..{head}")

    def test_a_raised_or_dropped_ceiling_fails_whatever_the_trailer(self):
        base = self.commit({BASELINE: split_rows(sweeper=36) + cap("services.readmodel", 32) + "\n"}, "a ceiling")
        head = self.commit({BASELINE: split_rows(sweeper=36) + cap("services.readmodel", 34) + "\n"},
                           "loosen it\n\nCoupling: room for one more split")
        self.assert_fails(self.baseline(), "distance-cap services.readmodel raised 32 -> 34", rng=f"{base}..{head}")
        # a dropped row is read at the row's own + 2 again: past a ceiling that had fallen below it
        tightened = self.commit({BASELINE: split_rows(sweeper=36) + cap("services.readmodel", 31) + "\n"}, "tighten it")
        head = self.commit({BASELINE: split_rows(sweeper=36)}, "drop it\n\nCoupling: no ceiling")
        self.assert_fails(self.baseline(), "distance-cap services.readmodel raised 31 -> 32", rng=f"{tightened}..{head}")

    def test_the_merge_takes_the_min(self):
        # main and a change both lowered the read model's ceiling; the rebase conflicts in the baseline, agentctl's
        # regen checks main's side out and runs --update over the combined code: every side's ceiling is read
        self.commit({BASELINE: split_rows(sweeper=36, readmodel_distance=30) + cap("services.readmodel", 32) + "\n"},
                    "a ceiling")
        self.scratch("scratch_git", "checkout", "-qb", "change")
        change = self.commit({BASELINE: split_rows(sweeper=36, readmodel_distance=28) + cap("services.readmodel", 30) + "\n"},
                             "the change lowers it")
        self.scratch("scratch_git", "checkout", "-q", "main")
        self.commit({BASELINE: split_rows(sweeper=36, readmodel_distance=29) + cap("services.readmodel", 31) + "\n"},
                    "main lowers it")
        picked = subprocess.run(["bash", "-c", '. "$1"; scratch_git "$2" cherry-pick "$3"', "_", SCRATCH_GIT, self.repo, change],
                                capture_output=True, text=True)
        self.assertNotEqual(0, picked.returncode, "the pick must conflict in the baseline")
        self.scratch("scratch_git", "checkout", "--ours", "--", BASELINE)
        written = self.update(split_rows(sweeper=36, readmodel_distance=29))  # the combined code measures main's 29
        self.assertIn(cap("services.readmodel", 30), written)
        self.assertNotIn(cap("services.readmodel", 31), written)

    def test_split_check_reports_the_headroom_left(self):
        os.remove(os.path.join(self.repo, SWEEPER_PATH))
        for path, cls, names in ((PLANNER_PATH, "SweepPlanner", PLANNED), (PASS_PATH, "SweepPass", PASSED)):
            with open(os.path.join(self.repo, path), "w") as f:
                f.write(scala(cls, names))
        current, fanouts = os.path.join(self.dir.name, "current.tsv"), os.path.join(self.dir.name, "fanouts.tsv")
        with open(current, "w") as f:
            f.write(split_rows(f"fanout\t{PLANNER}\t17", f"fanout\t{PASS}\t11", readmodel_distance=31))
        with open(fanouts, "w") as f:
            f.write("".join(fanout_line(c, n) for c, n in self.VALID.items()))
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1", GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@t",
                   GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@t")
        done = subprocess.run([sys.executable, "-I", "-B", TOOL, "split-check", "--current", current, "--fanouts", fanouts,
                               "--split", "ReadModelSweeper", "--into", "SweepPlanner", "--into", "SweepPass",
                               "--base", self.base], cwd=self.repo, env=env, capture_output=True, text=True)
        out = done.stdout + done.stderr
        self.assertEqual(0, done.returncode, out)
        self.assertIn("distance services.readmodel: 30 -> 31 -- rose within the split's 2: tolerated; ceiling 32, "
                      "headroom 0.01 left", out)
        self.assertIn("distance services.movies: 48 -- ceiling 50, headroom 0.02 left", out)


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
            done = subprocess.run([sys.executable, "-I", "-B", TOOL, "refs", "--current", rows_path, "--fanouts", fanouts_path],
                                  capture_output=True, text=True)
        self.assertEqual(0, done.returncode, done.stderr)
        self.assertEqual(f"{SWEEPER}\tservices.movies.Dep0\n{SWEEPER}\tservices.movies.Dep1\n", done.stdout)


if __name__ == "__main__":
    unittest.main()
