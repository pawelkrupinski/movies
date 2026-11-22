#!/usr/bin/env python3
"""scripts/gitops-specs, black-box: the plan read off a fixture lint registry, and a --manifests run against a scratch
movies repository whose sbt-warm is a stub that passes or fails on the manifests it is shown. Ported from
gitops-specs-test.sh case for case, plus its usage, a failed fetch and an empty plan; the same cases hold the shell
tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/gitops_specs.py", "scripts/gitops-specs")
REGISTRY = "\n".join([
    "# header row naming @gitops in a comment",
    "repochecks:deploy.ASpec\t@gitops",
    "common:models.NotGitops\t@repo *.scala",
    "worker:deploy.BSpec\t@gitops infra/nix/files/x.rules",
    "repochecks:deploy.CSpec\t@repo @gitops",
    "repochecks:deploy.LookalikeSpec\t@gitopsx"]) + "\n"
PLAN = ["repochecks/testOnly deploy.ASpec deploy.CSpec", "worker/testOnly deploy.BSpec"]
# sbt-warm in the scratch movies: what it was asked noted beside the check worktree, the manifests it saw too; it fails
# when they carry a file named `breaks-main`
SBT_WARM = """#!/usr/bin/env bash
[ "${1:-}" = --stop ] && { echo stopped >> "$PWD/../sbt-calls"; exit 0; }
printf '%s\\n' "$@" > "$PWD/../sbt-calls"
ls -A infra/kubernetes > "$PWD/../seen"
[ -e infra/kubernetes/breaks-main ] && { echo "[info] - x *** FAILED ***"; echo "[error] Failed: Total 1"; exit 1; }
echo "[info] Tests: succeeded 3, failed 0"
"""


class GitopsSpecsTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.registry = self.write("registry.tsv", REGISTRY)
        self.out = self.err = ""

    def plan(self, registry: str) -> cli_harness.Done:
        return self.run_tool("--plan", env={"KINOWO_LINT_REGISTRY": registry})

    def scratch_movies(self, registry: str = REGISTRY) -> None:
        """origin (bare) with a main carrying the registry and the sbt-warm stub; `movies` its clone; `gitops` a tree."""
        origin = self.repo("origin.git", "--bare")
        movies = self.repo("movies")
        self.write("movies/scripts/hooks/lint-registry.tsv", registry)
        os.chmod(self.write("movies/scripts/sbt-warm", SBT_WARM), 0o755)
        self.git(movies, "add", "-A")
        self.git(movies, "commit", "-qm", "base")
        self.git(movies, "remote", "add", "origin", origin)
        self.git(movies, "push", "-q", "origin", "HEAD:main")
        gitops = self.repo("gitops")
        self.write("gitops/all.yaml", "manifest\n")
        self.git(gitops, "add", "-A")
        self.git(gitops, "commit", "-qm", "base")

    def run_manifests(self, manifests: str = "gitops", movies: str = "movies") -> int:
        done = self.run_tool("--manifests", self.path(manifests),
                             env={"KINOWO_MOVIES_REPO": self.path(movies),
                                  "KINOWO_GITOPS_CHECK_WORKTREE": self.path("check"), "TMPDIR": self.work})
        self.out, self.err = done.stdout, done.stderr
        return done.code

    def test_the_plan_is_one_testonly_line_per_module_gitops_rows_only_in_first_seen_order(self) -> None:
        done = self.plan(self.registry)
        self.assertEqual((0, "\n".join(PLAN) + "\n", ""), (done.code, done.stdout, done.stderr))

    def test_an_empty_plan_prints_nothing(self) -> None:
        done = self.plan(self.write("empty.tsv", "# nothing\ncommon:models.X\t@repo\n"))
        self.assertEqual((0, ""), (done.code, done.stdout))

    def test_a_tree_mains_specs_accept_is_green(self) -> None:
        self.scratch_movies()
        self.assertEqual(0, self.run_manifests(), self.out + self.err)
        self.assertEqual(PLAN + ["stopped"], self.read("sbt-calls").split("\n")[:-1],
                         "the specs ran in the check worktree, as main's registry plans them; the server stopped")
        self.assertEqual("all.yaml\n", self.read("seen"), "they saw the tree as infra/kubernetes, without its .git")
        main = self.git(self.path("movies"), "rev-parse", "origin/main")
        self.assertEqual(main, self.git(self.path("check"), "rev-parse", "HEAD"), "the check sits at origin/main")
        lines = self.out.split("\n")
        self.assertEqual(f"gitops-specs: movies {main[:len(lines[0].split()[2])]} (origin/main) in "
                         f"{self.path('check')}", lines[0])
        self.assertRegex(lines[1], r"^gitops-specs: 2 sbt line\(s\); log \S+$")
        self.assertEqual("gitops-specs: GREEN ([info] Tests: succeeded 3, failed 0 )", lines[2])
        self.assertEqual(["", ""], [lines[3], "\n".join(lines[4:])], "three lines")

    def test_a_tree_that_fails_a_spec_is_red_and_the_next_run_shows_only_its_tree(self) -> None:
        self.scratch_movies()
        self.write("gitops/breaks-main", "")
        self.assertEqual(1, self.run_manifests())
        self.assertEqual(["[info] - x *** FAILED ***", "[error] Failed: Total 1"], self.err.split("\n")[:2],
                         "the failures, on stderr")
        self.assertRegex(self.err.split("\n")[2],
                         r"^gitops-specs: RED -- this tree would turn movies main red; full log: \S+$")
        log = self.err.split("\n")[2].split("full log: ")[1]
        with open(log) as f:
            self.assertIn("*** FAILED ***", f.read(), "the log it names holds the run")
        os.remove(self.path("gitops/breaks-main"))
        self.assertEqual(0, self.run_manifests(), "a file the last tree had is gone from the next run's tree")
        self.assertEqual("all.yaml\n", self.read("seen"))

    def test_what_cannot_run(self) -> None:
        self.scratch_movies()
        self.assertEqual(2, self.run_manifests("nope"))
        self.assertEqual(f"gitops-specs: no directory {self.path('nope')}\n", self.err)
        self.assertEqual(2, self.run_manifests(movies="gitops-not-movies"))
        self.assertIn("gitops-specs: cannot fetch movies origin/main", self.err)

    def test_a_registry_naming_no_gitops_spec_cannot_run(self) -> None:
        self.scratch_movies("common:models.X\t@repo\n")
        self.assertEqual(2, self.run_manifests())
        self.assertEqual("gitops-specs: the lint registry names no @gitops spec", self.err.strip().split("\n")[-1])
        self.assertFalse(os.path.exists(self.path("sbt-calls")), "no sbt")

    def test_usage(self) -> None:
        for args in ([], ["--bogus"]):
            done = self.run_tool(*args)
            self.assertEqual(2, done.code, args)
            self.assertIn("scripts/gitops-specs --plan", done.stderr)
        done = self.run_tool("--manifests")
        self.assertNotEqual(0, done.code)
        self.assertIn("--manifests <dir>", done.stderr)


if __name__ == "__main__":
    unittest.main()
