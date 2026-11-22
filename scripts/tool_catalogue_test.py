#!/usr/bin/env python3
# script-test-reads: *.sh *.py scripts/* .github/workflows/* .github/actions/* .github/scripts/* infra/bin/* infra/nix/bin/* android/scripts/* ios/scripts/* tools/* CLAUDE.md .claude/skills/* docs/tools.md
"""tool_catalogue.py: every tool states its purpose, the catalogue is rendered from those lines and the repository's
callers, and --check fails on a tool without one or a stale docs/tools.md -- here and on this repository."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tool_catalogue as tc  # noqa: E402

REPO = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))


class Scratch:
    """A throwaway git repository whose index holds the files written into it."""

    def __init__(self) -> None:
        self.dir = tempfile.TemporaryDirectory()
        self.root = self.dir.name
        subprocess.run(["git", "init", "-q", self.root], check=True)

    def write(self, path: str, text: str, executable: bool = False) -> None:
        full = os.path.join(self.root, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "w", encoding="utf-8") as handle:
            handle.write(text)
        if executable:
            os.chmod(full, 0o755)
        subprocess.run(["git", "-C", self.root, "add", path], check=True)

    def close(self) -> None:
        self.dir.cleanup()


class PurposeTest(unittest.TestCase):
    def setUp(self) -> None:
        self.scratch = Scratch()
        self.addCleanup(self.scratch.close)

    def purpose(self, path: str, text: str) -> object:
        self.scratch.write(path, text)
        return tc.purpose_of(self.scratch.root, path)

    def test_a_purpose_line_in_the_header_is_the_purpose(self) -> None:
        self.assertEqual(self.purpose("scripts/a.sh", "#!/bin/bash\n# Purpose: Do the one thing.\nset -e\n"),
                         "Do the one thing.")

    def test_a_python_docstring_s_first_line_is_the_purpose(self) -> None:
        self.assertEqual(self.purpose("scripts/b.py", '#!/usr/bin/env python3\n"""Count the rows.\n\nMore."""\n'),
                         "Count the rows.")

    def test_an_action_s_folded_description_is_read_to_its_first_sentence(self) -> None:
        text = "name: x\ndescription: >-\n  Restore the cache, from the runner's copy. Then more.\nruns:\n  using: composite\n"
        self.assertEqual(self.purpose(".github/actions/x/action.yml", text), "Restore the cache, from the runner's copy")

    def test_a_header_comment_without_a_purpose_line_is_no_purpose(self) -> None:
        self.assertIsNone(self.purpose("scripts/c.sh", "#!/bin/bash\n# Does a thing.\n"))


class CheckTest(unittest.TestCase):
    def setUp(self) -> None:
        self.scratch = Scratch()
        self.addCleanup(self.scratch.close)
        self.scratch.write("scripts/tool.sh", "#!/bin/bash\n# Purpose: The tool.\n", executable=True)
        self.scratch.write(".github/workflows/w.yml", "# Purpose: The workflow.\non:\n  push:\njobs:\n  a:\n"
                                                      "    steps:\n      - run: scripts/tool.sh\n")

    def test_a_tool_without_a_purpose_fails_the_check_naming_it(self) -> None:
        self.scratch.write("scripts/bare.sh", "#!/bin/bash\necho\n")
        tc.main(["--root", self.scratch.root])
        self.assertEqual([p.split(":")[0] for p in tc.problems(self.scratch.root)], ["scripts/bare.sh"])

    def test_a_stale_catalogue_fails_the_check_until_it_is_regenerated(self) -> None:
        self.assertEqual(tc.main(["--root", self.scratch.root, "--check"]), 1)
        self.assertEqual(tc.main(["--root", self.scratch.root]), 0)
        self.assertEqual(tc.main(["--root", self.scratch.root, "--check"]), 0)
        self.scratch.write("scripts/new.sh", "#!/bin/bash\n# Purpose: A new tool.\n")
        self.assertIn("docs/tools.md is stale", " ".join(tc.problems(self.scratch.root)))

    def test_the_catalogue_names_a_tool_s_workflow_caller_how_it_runs_and_its_class(self) -> None:
        self.scratch.write(tc.MIGRATION_CLASSES, "# path\tclass\nscripts/tool.sh\tKEEP\n")
        row = [t for t in tc.catalogue(self.scratch.root) if t.path == "scripts/tool.sh"][0]
        self.assertEqual((row.purpose, row.run_as, row.callers, row.migration),
                         ("The tool.", "scripts/tool.sh", "wf w", "KEEP"))

    def test_the_workflow_runs_on_its_triggers(self) -> None:
        row = [t for t in tc.catalogue(self.scratch.root) if t.path == ".github/workflows/w.yml"][0]
        self.assertEqual(row.run_as, "workflow: push")


class AreaAndAgentctlTest(unittest.TestCase):
    def test_each_tool_falls_in_its_area(self) -> None:
        self.assertEqual([tc.area_of(p) for p in ("scripts/hooks/pre-push", "scripts/identity-slice",
                                                  "scripts/local-mirror/mirror.sh", "infra/bin/check",
                                                  "android/scripts/devtest.sh", "tools/devpanel/build.sh")],
                         ["Landing and CI", "Identity", "Mirror and data", "Infra and deploy", "Mobile", "Dev"])

    def test_agentctl_names_a_tool_by_its_path_or_a_distinctive_basename_only(self) -> None:
        text = "run scripts/sbt-warm, then check the stall-watch.sh output"
        self.assertEqual([tc.named_in(p, text) for p in ("scripts/sbt-warm", "scripts/stall-watch.sh", "infra/bin/check")],
                         [True, True, False])


class ThisRepositoryTest(unittest.TestCase):
    def test_every_tool_here_has_a_purpose_and_docs_tools_md_is_current(self) -> None:
        self.assertEqual(tc.problems(REPO), [])


if __name__ == "__main__":
    unittest.main()
