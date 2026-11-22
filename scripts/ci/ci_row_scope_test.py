#!/usr/bin/env python3
"""scripts/ci/ci-row-scope.sh, black-box: which of ci.yml's page, mobile and e2e rows a push's paths can move, and the
`scope` job's outputs -- diffed from the newest green Main run, every row on anything unreadable. gh is a fake; git is
a real throwaway repository whose `origin` is itself. Ported from ci-row-scope-test.sh; the same cases hold the shell
tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import sys
import unittest
from typing import Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/ci_row_scope.py", "scripts/ci/ci-row-scope.sh")
KINDS = ("e2e", "pages", "browsers")
EVERY_ROW = "e2e=true pages=true browsers=true"
NO_ROW = "e2e=false pages=false browsers=false"


class ScopeCase(cli_harness.ToolCase):
    """A repository shaped like this one where it matters: the consumers name some scripts, one sources another."""
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.repo = self.path("repo")
        self.put(".github/workflows/ci.yml", "run: scripts/ci/tree-unchanged.sh")
        self.put(".github/actions/run-page-test/action.yml", "run: ../scripts/ci/install-playwright.sh")
        self.put("e2e/src/Spec.scala", 'Process(Seq("scripts/read-model-snapshot-inputs.sh", "write"))')
        self.put("scripts/ci/tree-unchanged.sh",
                 '#!/usr/bin/env bash\n. "$here/../tool-conventions.sh"\n# see scripts/identity-measure for the rest')
        for f in ("scripts/ci/install-playwright.sh", "scripts/read-model-snapshot-inputs.sh",
                  "scripts/tool-conventions.sh", "scripts/identity-measure", "scripts/identity_rule_ablation.py",
                  "scripts/ci/identity_takes.py"):
            self.put(f, f"# {f}")
        for d in ("web/src/page", "docs", "identity/src/main", "identity/src/test", "worker"):
            os.makedirs(os.path.join(self.repo, d), exist_ok=True)
        if not shutil.which("timeout"):
            self.fakes.script("timeout", 'shift\nexec "$@"')

    def put(self, relative: str, text: str, repo: Optional[str] = None) -> None:
        full = os.path.join(repo or self.repo, relative)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "w") as f:
            f.write(text + "\n")

    def verdict(self, kind: str, *paths: str, repo: Optional[str] = None) -> str:
        done = self.run_tool(kind, repo or self.repo, stdin="".join(p + "\n" for p in paths))
        self.assertEqual(0, done.code, done.output)
        return done.stdout.strip()


class ClassificationTest(ScopeCase):
    def test_what_moves_no_row(self) -> None:
        for kind in KINDS:
            for paths in (("docs/identity-experiments.md",), ("CLAUDE.md",), (".claude/skills/x/SKILL.md",),
                          ("test/resources/fixtures/identity-judgements.tsv",),
                          ("scripts/identity_rule_ablation.py", "scripts/ci/identity_takes.py"),
                          ("scripts/identity-measure",), ()):
                with self.subTest(kind=kind, paths=paths):
                    self.assertEqual("false", self.verdict(kind, *paths))

    def test_what_moves_every_row(self) -> None:
        for kind in KINDS:
            for paths in (("scripts/ci/tree-unchanged.sh",), ("scripts/ci/install-playwright.sh",),
                          ("scripts/read-model-snapshot-inputs.sh",), ("scripts/tool-conventions.sh",),
                          ("docs/a.md", "worker/src/main/scala/A.scala"), (".github/workflows/ci.yml",),
                          ("web/src/main/resources/a.md",)):
                with self.subTest(kind=kind, paths=paths):
                    self.assertEqual("true", self.verdict(kind, *paths))

    def test_paths_are_read_trimmed(self) -> None:
        done = self.run_tool("pages", self.repo, stdin="  docs/a.md  \n\n   \n")
        self.assertEqual("false\n", done.stdout, done.output)

    def test_a_live_script_is_live_though_the_live_list_outgrows_the_pipe_buffer(self) -> None:
        big = self.path("big")
        names = [f"zz-live-script-with-a-deliberately-long-name-to-fill-the-pipe-buffer-{i}.sh" for i in range(1, 1201)]
        for name in names + ["aa-first.sh"]:
            self.put(f"scripts/{name}", f"# {name}", repo=big)
        self.put(".github/workflows/ci.yml", "\n".join(f"run: scripts/{n}" for n in names + ["aa-first.sh"]), repo=big)
        self.assertEqual("true", self.verdict("e2e", "scripts/aa-first.sh", repo=big))

    def test_identity_by_kind(self) -> None:
        for kind, path, moves in (("e2e", "identity/src/test/scala/ASpec.scala", "true"),
                                  ("pages", "identity/src/test/scala/ASpec.scala", "false"),
                                  ("browsers", "identity/src/test/scala/ASpec.scala", "false"),
                                  ("e2e", "identity/src/main/scala/A.scala", "true"),
                                  ("pages", "identity/src/main/scala/A.scala", "true"),
                                  ("browsers", "identity/src/main/scala/A.scala", "false")):
            with self.subTest(kind=kind, path=path):
                self.assertEqual(moves, self.verdict(kind, path))

    def test_an_unknown_kind_is_refused(self) -> None:
        done = self.run_tool("deploy", self.repo, stdin="docs/a.md\n")
        self.assertEqual((2, ""), (done.code, done.stdout), done.output)
        self.assertIn("unknown kind 'deploy'", done.stderr)

    def test_no_kind_is_refused(self) -> None:
        done = self.run_tool()
        self.assertNotEqual(0, done.code, done.output)
        self.assertIn("usage: ci-row-scope.sh pages|browsers|e2e", done.stderr)


class OutputsTest(ScopeCase):
    """--outputs over a real git history, `origin` itself; gh answers the newest green Main run from a file."""

    def setUp(self) -> None:
        super().setUp()
        self.git(self.repo, "init", "-q")
        self.git(self.repo, "add", "-A")
        self.git(self.repo, "commit", "-qm", "base")
        self.green_sha = self.git(self.repo, "rev-parse", "HEAD")
        self.git(self.repo, "remote", "add", "origin", self.repo)
        self.fakes.script("gh", '[ -f "$STUB/gh-fails" ] && { echo "HTTP 502" >&2; exit 1; }\ncat "$STUB/green" 2>/dev/null')
        self.green(self.green_sha)
        self.log = ""

    def green(self, sha: str) -> None:
        self.write("green", sha + "\n" if sha else "")

    def commit(self, path: str) -> str:
        full = os.path.join(self.repo, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "a") as f:
            f.write("x\n")
        self.git(self.repo, "add", "-A")
        self.git(self.repo, "commit", "-qm", path)
        return self.git(self.repo, "rev-parse", "HEAD")

    def outputs(self, sha: str, **env: str) -> str:
        out = self.path("out")
        if os.path.exists(out):
            os.remove(out)
        done = self.run_tool("--outputs", cwd=self.repo, env={
            "STUB": self.work, "GITHUB_OUTPUT": out, "GITHUB_SHA": sha, "BEFORE": self.green_sha, "PULL_REQUEST": "",
            **env})
        self.log = done.output
        self.assertEqual(0, done.code, done.output)
        with open(out) as f:
            return " ".join(line for line in f.read().split("\n") if line)

    def test_a_docs_only_push_since_the_green_run_skips_every_row(self) -> None:
        self.assertEqual(NO_ROW, self.outputs(self.commit("docs/b.md")))
        self.assertIn(f"since the newest green Main run ({self.green_sha}): 1 paths\n  docs/b.md\n", self.log)
        self.assertIn("e2e=false\npages=false\nbrowsers=false\n", self.log)

    def test_a_change_stays_in_the_range_until_a_run_passes_with_it(self) -> None:
        identity = self.commit("identity/src/main/scala/A.scala")
        self.assertEqual("e2e=true pages=true browsers=false", self.outputs(identity))
        docs = self.commit("docs/c.md")
        self.assertEqual("e2e=true pages=true browsers=false", self.outputs(docs))
        self.green(identity)
        self.assertEqual(NO_ROW, self.outputs(docs))

    def test_a_tier_not_yet_deployed_keeps_the_e2e_row_which_builds_its_image(self) -> None:
        identity = self.commit("identity/src/main/scala/A.scala")
        docs = self.commit("docs/c.md")
        self.green(identity)
        self.git(self.repo, "tag", "deployed-web", self.green_sha)
        self.assertEqual("e2e=true pages=false browsers=false", self.outputs(docs))
        self.git(self.repo, "tag", "-d", "deployed-web")
        self.assertEqual(NO_ROW, self.outputs(docs))

    def test_whatever_it_cannot_judge_runs_every_row(self) -> None:
        docs = self.commit("docs/c.md")
        for why, env in (("a pull request", {"PULL_REQUEST": "7"}), ("not a push (a dispatch)", {"BEFORE": ""}),
                         ("not a push (a dispatch)", {"BEFORE": "0" * 40})):
            with self.subTest(env=env):
                self.assertEqual(EVERY_ROW, self.outputs(docs, **env))
                self.assertIn(f"{why}: every row runs", self.log)
        self.green("")
        self.assertEqual(EVERY_ROW, self.outputs(docs))
        self.assertIn("no green Main run to diff from: every row runs", self.log)
        self.write("gh-fails", "")
        self.assertEqual(EVERY_ROW, self.outputs(docs))
        self.assertRegex(self.log, r"could not read Main.s runs \(HTTP 502.*every row runs")
        os.remove(self.path("gh-fails"))
        self.green("0123456789abcdef0123456789abcdef01234567")
        self.assertEqual(EVERY_ROW, self.outputs(docs))
        self.assertIn("could not fetch the newest green Main run's commit 0123456789abcdef0123456789abcdef01234567",
                      self.log)

    def test_the_green_run_is_asked_of_main_yml_on_main(self) -> None:
        self.outputs(self.commit("docs/b.md"))
        self.assertEqual([["run", "list", "--workflow", "main.yml", "--branch", "main", "--status", "success", "--limit",
                           "1", "--json", "headSha", "--jq", ".[0].headSha // empty"]], self.fakes.calls("gh"))

    def test_a_push_of_thousands_of_paths_still_decides_every_row_listing_only_the_first_50(self) -> None:
        self.green(self.commit("docs/c.md"))
        for i in range(1, 3001):
            self.put(f"docs/many/page-{i}-with-a-long-enough-name.md", "x")
        self.git(self.repo, "add", "-A")
        self.git(self.repo, "commit", "-qm", "many")
        self.assertEqual(NO_ROW, self.outputs(self.git(self.repo, "rev-parse", "HEAD")))
        self.assertEqual(50, sum(1 for line in self.log.split("\n") if line.startswith("  docs/many/")))


if __name__ == "__main__":
    unittest.main()
