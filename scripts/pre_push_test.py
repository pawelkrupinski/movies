#!/usr/bin/env python3
"""THE PRE-PUSH HOOK'S SELECTION, black-box (scripts/hooks/pre-push's plan modes): which checks a list of changed paths
selects (`--plan`), which test configs it compiles (`--compile-plan`), the ratchets' paths (`--coupling-plan`,
`--quality-plan`, `--js-quality-plan`, `--py-quality-plan`), what moves takes (`--identity-change`), the unit tests and
their universe (`--unit-plan`, `--unit-universe`, `--source-lints`), the lint registry's rows (`--lint-plan`,
`--per-file-lints`), the script self-tests (`--script-test-plan`) and a failed run's summary (`--failure-summary`).
agentctl's land queue and MINIMAL RECHECKS read these lines; the hook's own run reads the same selection. Hermetic: a
scratch checkout with the fixture's tables (pre_push_fixture.py). Passes unchanged on the shell hook and on the Python
one (the migration's equivalence proof, /Users/pawel/Documents/shell-to-python-migration.md section 10)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import List, Mapping, Optional

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
from pre_push_fixture import PlanCase, lines  # noqa: E402

ALL_COMPILES = ["common/Test/compile", "identity/Test/compile", "testkit/Test/compile", "scrapers/Test/compile",
                "repochecks/Test/compile", "worker/Test/compile", "worker/It/compile", "web/Test/compile",
                "web/It/compile", "web/Page/compile", "e2e/Test/compile"]
WORKER_REST = "worker/testOnly * -services.identity.* -scripts.* -tools.* -services.movies.HeavyRestSpec"


class PlanTest(PlanCase):
    """`--plan`: the checks, one a line, in run order."""

    def assertPlans(self, expected: List[str], paths: List[str], env: Optional[Mapping[str, Optional[str]]] = None) -> None:
        self.assertEqual(expected, self.plan("--plan", paths, env), paths)

    def test_nothing_changed_selects_nothing(self):
        self.assertPlans([], [])
        self.assertPlans([], ["docs/x.md", "README"])

    def test_the_browser_assets(self):
        self.assertPlans(["eslint", "jsquality", "sbt"], ["web/src/main/assets/js/a.js"])
        self.assertPlans(["eslint"], ["page-tests-playwright/tests/a.ts"])
        self.assertPlans(["eslint"], ["package.json"])
        self.assertPlans(["jsquality"], ["infra/version-dashboard/src/a.ts"])
        self.assertPlans(["sbt"], ["web/src/main/twirl/a.scala.html"])

    def test_the_shell_tools_by_shebang(self):
        self.assertPlans(["scripttests", "shellcheck", "sbt"], ["scripts/a-tool"])
        self.assertPlans(["pyquality", "scripttests", "sbt"], ["scripts/a-python-tool"])
        self.assertPlans(["scripttests", "sbt"], ["scripts/a-data-file"])
        self.assertPlans(["pyquality", "scripttests", "sbt"], ["scripts/x.py"])
        self.assertPlans(["scripttests", "sbt"], ["scripts/x_test.py"])  # a self-test is reported, never held

    def test_python_tools_outside_scripts(self):
        self.assertPlans(["pyquality", "scripttests"], ["infra/bin/x.py"])
        self.assertPlans(["pyquality"], ["pyrightconfig.json"])

    def test_the_workflows(self):
        self.assertPlans(["newchecks", "schedules", "actionlint", "sbt"], [".github/workflows/x.yml"])
        self.assertPlans(["scripttests", "actionlint", "sbt"], [".github/actions/a/action.yml"])
        self.assertPlans(["newchecks", "sbt"], [".github/gating-checks.tsv"])
        self.assertPlans(["schedules"], ["infra/nix/files/scheduled-workflows.json"])

    def test_the_hook_itself(self):
        self.assertPlans(["newchecks", "scripttests", "shellcheck", "sbt"], ["scripts/hooks/pre-push"])

    def test_a_registry_file_alone_runs_the_sbt_check(self):
        self.assertPlans(["sbt"], ["Dockerfile"])
        # a pattern row selects its spec only when the check runs anyway
        self.assertPlans([], ["docs/a.md"])

    def test_the_identity_area(self):
        self.assertPlans(["driftguards", "sbt", "identity"], ["identity/src/main/scala/A.scala"])
        self.assertPlans(["judgements", "driftguards", "sbt", "identity"],
                         ["test/resources/fixtures/identity-judgements.tsv"])
        # what the identity specs read by class alone: the area, no drift guards
        self.assertPlans(["sbt", "identity"], ["testkit/src/main/scala/A.scala"])
        self.assertPlans(["drift", "sbt", "identity"], ["common/src/main/scala/models/Film.scala"])
        self.assertPlans(["driftguards", "sbt"], ["worker/src/main/scala/A.scala"])

    def test_the_identity_fixtures(self):
        self.assertPlans(["driftguards", "sbt", "flipbacklog", "identity"], ["test/resources/fixtures/drift-slices/a/b.tsv"])
        self.assertPlans(["flipbacklog"], ["test/resources/fixtures/drift-gate-seeds.tsv"])
        self.assertPlans(["stampcarry"], ["test/resources/identity-stamp-carry.tsv"])

    def test_the_generated_rosters_and_the_snapshot(self):
        self.assertPlans(["drift"], ["android/app/src/main/java/pl/kinowo/model/City.kt"])
        self.assertPlans(["drift", "scripttests"], ["data/x.tsv"])
        self.assertPlans(["snapshot"], ["test/resources/fixtures/08-06-2026/a.json"])

    def test_the_monitoring(self):
        self.assertPlans(["dashboards", "sbt"], ["infra/nix/files/monitoring/grafana/dashboards/a.json"])
        self.assertPlans(["dashboards", "metricalerts"], ["infra/nix/files/monitoring/rules/a.rules"])
        self.assertPlans(["metricalerts", "shellcheck", "sbt"], ["infra/nix/files/exporter.sh"])
        self.assertPlans(["shellcheck", "sbt"], ["infra/nix/files/quiet.sh"])

    def test_ios(self):
        self.assertPlans(["ios"], ["ios/Kinowo/A.swift"])

    def test_the_allow_lists(self):
        self.assertPlans(["allowlists", "sbt"], [".github/allow-lists.txt"])

    def test_a_declared_read_runs_the_script_tests(self):
        self.assertPlans(["scripttests"], ["docs/read-by-a-test.md"])

    def test_several_paths_select_each_check_once_in_run_order(self):
        self.assertPlans(["judgements", "newchecks", "schedules", "driftguards", "eslint", "jsquality", "scripttests",
                          "actionlint", "shellcheck", "sbt", "identity", "ios"],
                         ["ios/A.swift", "web/src/main/assets/js/a.js", "scripts/a-tool", ".github/workflows/x.yml",
                          "test/resources/fixtures/identity-judgements.tsv"])

    def test_the_heavy_rows_only_in_the_landing_check(self):
        heavy = {"PREPUSH_HEAVY_ROWS": "1"}
        self.assertPlans(["sbt", "itrest"], ["web/src/main/scala/A.scala"], heavy)
        # e2e (corpus) rides `basic` here: build.sbt's e2eCorpus specs are all on a basic line
        self.assertPlans(["driftguards", "sbt", "identity", "itrest", "itidentity"],
                         ["identity/src/main/scala/A.scala"], heavy)
        self.assertPlans(["driftguards", "sbt", "itrest", "itidentity"], ["worker/src/main/scala/A.scala"], heavy)
        self.assertPlans(["newchecks", "schedules", "actionlint", "sbt"], [".github/workflows/x.yml"], heavy)
        self.assertPlans(["sbt"], ["web/src/main/scala/A.scala"])


class CorpusOutsideBasicTest(PlanCase):
    """With `basic` not running build.sbt's e2eCorpus specs, the identity row brings e2e (corpus)."""
    EXTRA = {"scripts/hooks/local-tests.txt": "basic sbt Test/compile\nidentity sbt worker/testOnly services.identity.*\n"}

    def test_the_identity_row_brings_the_corpus_replay(self):
        self.assertEqual(["driftguards", "sbt", "identity", "itrest", "itidentity", "e2ecorpus"],
                         self.plan("--plan", ["identity/src/main/scala/A.scala"], {"PREPUSH_HEAVY_ROWS": "1"}))


class NoIdentityTestsTest(PlanCase):
    """An area runs only when local-tests.txt names tests for it."""
    EXTRA = {"scripts/hooks/local-tests.txt": "basic sbt Test/compile\n"}

    def test_no_identity_lines_no_identity_area(self):
        self.assertEqual(["sbt"], self.plan("--plan", ["testkit/src/main/scala/A.scala"]))
        self.assertEqual(["driftguards", "sbt"], self.plan("--plan", ["identity/src/main/scala/A.scala"]))


class CompilePlanTest(PlanCase):
    """`--compile-plan`: the test configs a change compiles, its module's and every dependent's, in order."""

    def test_the_build_compiles_everything(self):
        self.assertEqual(ALL_COMPILES, self.plan("--compile-plan", ["build.sbt"]))
        self.assertEqual(ALL_COMPILES, self.plan("--compile-plan", ["common/src/main/scala/A.scala"]))

    def test_down_the_chain(self):
        self.assertEqual(ALL_COMPILES[2:], self.plan("--compile-plan", ["testkit/src/main/scala/A.scala"]))
        self.assertEqual(["scrapers/Test/compile", "worker/Test/compile", "worker/It/compile", "e2e/Test/compile"],
                         self.plan("--compile-plan", ["scrapers/src/main/scala/A.scala"]))
        self.assertEqual(["worker/Test/compile", "worker/It/compile", "e2e/Test/compile"],
                         self.plan("--compile-plan", ["worker/src/main/scala/A.scala"]))
        self.assertEqual(["web/Test/compile", "web/It/compile", "web/Page/compile", "e2e/Test/compile"],
                         self.plan("--compile-plan", ["web/src/main/scala/A.scala"]))
        self.assertEqual(["worker/Test/compile", "worker/It/compile", "web/Test/compile", "web/It/compile",
                          "web/Page/compile", "e2e/Test/compile"],
                         self.plan("--compile-plan", ["worker/src/fixtures/scala/A.scala"]))
        self.assertEqual(["repochecks/Test/compile", "worker/Test/compile", "worker/It/compile", "e2e/Test/compile"],
                         self.plan("--compile-plan", ["repochecks/src/fixtures/A.scala"]))

    def test_a_test_tree_is_its_modules_alone(self):
        self.assertEqual(["web/Test/compile", "web/It/compile", "web/Page/compile"],
                         self.plan("--compile-plan", ["web/src/test/scala/ASpec.scala"]))
        self.assertEqual(["web/It/compile"], self.plan("--compile-plan", ["web/src/it/scala/ASpec.scala"]))
        self.assertEqual(["web/Page/compile"], self.plan("--compile-plan", ["web/src/page/scala/ASpec.scala"]))
        self.assertEqual(["worker/It/compile"], self.plan("--compile-plan", ["worker/src/it/scala/ASpec.scala"]))
        self.assertEqual(["worker/It/compile", "worker/Test/compile"],
                         self.plan("--compile-plan", ["worker/src/test/scala/ASpec.scala"]))
        self.assertEqual(["repochecks/Test/compile", "worker/It/compile"],
                         self.plan("--compile-plan", ["repochecks/src/test/scala/ASpec.scala"]))
        self.assertEqual(["http/Test/compile"], self.plan("--compile-plan", ["http/src/test/scala/ASpec.scala"]))
        self.assertEqual(["identity/Test/compile"], self.plan("--compile-plan", ["identity/src/test/scala/ASpec.scala"]))

    def test_a_module_already_compiled_is_not_named_twice(self):
        self.assertEqual(ALL_COMPILES, self.plan("--compile-plan", ["build.sbt", "web/src/test/scala/ASpec.scala",
                                                                    "common/src/test/scala/ASpec.scala"]))

    def test_nothing_compiled_for_non_code(self):
        self.assertEqual([], self.plan("--compile-plan", ["docs/a.md", "scripts/a-tool"]))


class RatchetPathsTest(PlanCase):
    """The ratchets' path filters: each prints the paths that select its step."""

    def test_coupling(self):
        paths = ["web/src/main/scala/A.scala", "web/src/main/java/B.java", "web/src/test/scala/C.scala",
                 "coupling.sbt", "project/Coupling.scala", "test/resources/coupling-layers.tsv",
                 "scripts/ci/coupling_ratchet.py", "docs/a.md"]
        self.assertEqual(paths[:2] + paths[3:7], self.plan("--coupling-plan", paths))

    def test_quality(self):
        paths = ["worker/src/main/scala/A.scala", "worker/src/main/java/B.java", "quality/src/main/scala/Q.scala",
                 "project/Dependencies.scala", "test/resources/length-baseline.tsv", "scripts/design-rules/naming.py",
                 "scripts/lib/rules_table.py", "docs/a.md"]
        self.assertEqual([paths[0]] + paths[2:7], self.plan("--quality-plan", paths))

    def test_js_quality(self):
        paths = ["web/src/main/assets/js/a.js", "infra/version-dashboard/src/a.ts", "scripts/js-quality-report",
                 "scripts/design-rules/model.py", "page-tests-playwright/tests/a.ts", "web/src/page/resources/a.js"]
        self.assertEqual(paths[:4], self.plan("--js-quality-plan", paths))

    def test_py_quality(self):
        paths = ["scripts/x.py", "scripts/x_test.py", "scripts/a-python-tool", "scripts/a-tool", "infra/bin/y.py",
                 "test/resources/py-quality-baseline.tsv", "pyrightconfig.json", "docs/z.py"]
        self.assertEqual(["scripts/x.py", "scripts/a-python-tool", "infra/bin/y.py",
                          "test/resources/py-quality-baseline.tsv", "pyrightconfig.json"],
                         self.plan("--py-quality-plan", paths))


class IdentityChangeTest(PlanCase):
    """`--identity-change`: `identity` when a path can move takes, nothing otherwise."""

    def test_take_paths(self):
        for path in ("identity/src/main/scala/A.scala", "common/src/main/resources/title-rules.tsv",
                     "common/src/main/scala/services/titles/TitleNormalizer.scala",
                     "test/resources/fixtures/identity-judgements.tsv",
                     "worker/src/main/scala/services/identityadapters/A.scala"):
            self.assertEqual(["identity"], self.plan("--identity-change", ["docs/a.md", path]), path)

    def test_an_identity_spec_or_fixture_moves_no_take(self):
        self.assertEqual([], self.plan("--identity-change", ["identity/src/test/scala/ASpec.scala",
                                                             "test/resources/fixtures/corpus/a.tsv",
                                                             "common/src/main/scala/models/A.scala"]))


class UnitPlanTest(PlanCase):
    """`--unit-plan` and `--unit-universe`: the combined-tree verification's unit tests."""

    def test_a_module_whole(self):
        self.assertEqual(["web/Test/test"], self.plan("--unit-plan", ["web/src/main/scala/A.scala"]))
        self.assertEqual(["common/Test/test", "web/Test/test"],
                         self.plan("--unit-plan", ["web/src/test/scala/A.scala", "common/src/main/scala/B.scala"]))

    def test_outside_a_modules_sources_nothing(self):
        self.assertEqual([], self.plan("--unit-plan", ["web/README.md", "docs/a.md", "e2e/src/test/scala/A.scala"]))

    def test_the_workers_rest_row_read_from_the_build(self):
        self.assertEqual([WORKER_REST, "worker/testOnly scripts.RestShardSpec"],
                         self.plan("--unit-plan", ["worker/src/main/scala/A.scala"]))

    def test_an_identity_row_spec_changed_runs_by_name(self):
        self.assertEqual([WORKER_REST, "worker/testOnly scripts.RestShardSpec services.identity.ASpec tools.BSpec"],
                         self.plan("--unit-plan", ["worker/src/test/scala/services/identity/ASpec.scala",
                                                   "worker/src/test/scala/tools/BSpec.scala",
                                                   "worker/src/test/scala/scripts/RestShardSpec.scala"]))

    def test_the_universe(self):
        self.assertEqual(["base/Test/test", "http/Test/test", "common/Test/test", "identity/Test/test",
                          "testkit/Test/test", "scrapers/Test/test", "repochecks/Test/test", "web/Test/test",
                          "worker/testOnly * -services.identity.HeavyOneSpec -services.movies.HeavyRestSpec "
                          "-scripts.HeavyTwoSpec"],
                         self.plan("--unit-universe", []))

    def test_the_source_lints_not_already_run(self):
        self.assertEqual(["web/testOnly lint.WalkerSpec", "worker/testOnly tools.ScanSpec"],
                         self.plan("--source-lints", []))
        self.assertEqual(["worker/testOnly tools.ScanSpec"], self.plan("--source-lints", ["web/Test/test"]))
        self.assertEqual(["web/testOnly lint.WalkerSpec"], self.plan("--source-lints", [], None, "worker:tools.ScanSpec"))


class UnreadableBuildTest(PlanCase):
    """A build whose worker rows cannot be read: a command sbt refuses, never a guessed row."""
    EXTRA = {"build.sbt": "lazy val nothing = 1\n"}

    def test_fails_closed(self):
        self.assertEqual(["worker/build.sbt-WORKER_ROW-unreadable"],
                         self.plan("--unit-plan", ["worker/src/main/scala/A.scala"])[:1])
        self.assertEqual("worker/build.sbt-HeavyRows-unreadable", self.plan("--unit-universe", [])[-1])


class LintPlanTest(PlanCase):
    """`--lint-plan` and `--per-file-lints`: the registry's rows a change selects, `<module>:<spec>` a line."""

    def test_the_standing_lints_always(self):
        self.assertEqual(["web:lint.StandingSpec"], self.plan("--lint-plan", ["README"]))

    def test_a_pattern_row(self):
        self.assertEqual(["web:lint.StandingSpec", "web:lint.WorkflowSpec"],
                         self.plan("--lint-plan", [".github/workflows/a.yml"]))
        self.assertEqual(["web:lint.StandingSpec", "worker:lint.DockerSpec"], self.plan("--lint-plan", ["Dockerfile"]))

    def test_a_leading_star_slash_is_one_module(self):
        self.assertEqual(["web:lint.StandingSpec", "common:lint.MainSourcesSpec"],
                         self.plan("--lint-plan", ["web/src/main/scala/A.scala"]))
        self.assertEqual(["web:lint.StandingSpec"], self.plan("--lint-plan", ["a/b/src/main/scala/A.scala"]))

    def test_its_own_code_selects_it(self):
        self.assertEqual(["web:lint.StandingSpec", "scrapers:lint.CodeOnlySpec"],
                         self.plan("--lint-plan", ["scrapers/src/test/scala/A.scala"]))

    def test_a_dash_row_never(self):
        self.assertNotIn("worker:lint.ServerSpec", self.plan("--lint-plan", ["worker/src/test/scala/A.scala"]))

    def test_gitops_rows_only_with_the_manifests(self):
        self.assertNotIn("repochecks:deploy.GitopsSpec", self.plan("--lint-plan", ["infra/kubernetes/a.yaml"]))
        self.assertIn("repochecks:deploy.GitopsSpec",
                      self.plan("--lint-plan", ["infra/kubernetes/a.yaml"], {"PREPUSH_GITOPS": "1"}))

    def test_lintspecs_skipped_leaves_the_standing_rows(self):
        self.assertEqual(["web:lint.StandingSpec"],
                         self.plan("--lint-plan", [".github/workflows/a.yml"], {"PREPUSH_SKIP": "lintspecs"}))

    def test_per_file(self):
        self.assertEqual(["web:lint.PerFileSpec"], self.plan("--per-file-lints", ["docs/a.md"]))
        self.assertEqual([], self.plan("--per-file-lints", ["docs/a.md", "web/src/test/scala/A.scala"]))
        self.assertEqual([], self.plan("--per-file-lints", [".github/workflows/a.yml"]))


class ScriptTestPlanTest(PlanCase):
    """`--script-test-plan`: run-script-tests' own selection for the paths, a test a line (its first column)."""

    def test_the_runners_selection(self):
        self.assertEqual(["scripts/a-test.sh", "scripts/b-test.sh"],
                         self.plan("--script-test-plan", ["scripts/a.sh", "scripts/b.py"]))


class FailureSummaryTest(PlanCase):
    """`--failure-summary <log>`: every failing test with its message's first lines, then sbt's [error] lines."""

    def summary(self, text: str) -> List[str]:
        log = self.write("run.log", text)
        done = self.hook("--failure-summary", log)
        self.assertEqual(0, done.code, done.output)
        return lines(done.stdout)

    def test_failures_first_then_errors(self):
        text = "".join(f"[info] - passing {n}\n" for n in range(100)) + (
            "[info] - a case *** FAILED ***\n[info]   expected 1\n[info]   got 2\n[info]   at A.scala:3\n"
            "[info]   line four\n[info]   line five\n[info] - next\n[error] Failed tests:\n[error] \tASpec\n")
        self.assertEqual(["[info] - a case *** FAILED ***", "[info]   expected 1", "[info]   got 2",
                          "[info]   at A.scala:3", "[info]   line four", "[error] Failed tests:", "[error] \tASpec"],
                         self.summary(text))

    def test_no_failure_line_shows_the_logs_end(self):
        text = "".join(f"line {n}\n" for n in range(30)) + "population walk: hidden\n"
        self.assertEqual([f"line {n}" for n in range(10, 30)], self.summary(text))


class NotAPlanModeTest(PlanCase):
    """A plan mode prints only; outside the checkout's root it still answers for the hook's own tree."""

    def test_plan_from_a_subdirectory(self):
        done = self.run_tool("--plan", cwd=self.path("repo", "scripts"), stdin="ios/A.swift\n",
                             tool=self.path("repo", "scripts", "hooks", "pre-push"))
        self.assertEqual(["ios"], lines(done.stdout))


if __name__ == "__main__":
    unittest.main()
