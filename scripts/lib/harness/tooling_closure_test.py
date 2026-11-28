#!/usr/bin/env python3
"""tooling_closure.py: a tool's hashed files are its import closure within scripts/, so a module split out of it is covered
without a list edit; the shared lib modules and test files are not the tool's."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from harness import tooling_closure  # noqa: E402
from seams import tool_files  # noqa: E402

TREE = {
    "scripts/tool_cli.py": "import tool_cli_flow\nfrom tool_cli_context import Context\nimport tool_process\n",
    "scripts/tool_cli_flow.py": "import tool_cli_verdict\n",
    "scripts/tool_cli_verdict.py": "import os\n",
    "scripts/tool_cli_context.py": "",
    "scripts/tool_cli_unrelated.py": "",
    "scripts/lib/tool_process.py": "",
    "scripts/tool.sh": "#!/bin/sh\n",
}


def checkout() -> tool_files.Files:
    files = tool_files.Files.in_memory(base="/repo")
    for path, text in TREE.items():
        files.write_text(f"/repo/{path}", text)
    return files


class ToolingClosureTest(unittest.TestCase):
    def test_a_module_split_out_of_the_tool_is_covered_by_itself(self) -> None:
        own = tooling_closure.own_files(checkout(), "/repo", ["scripts/tool_cli.py"])
        self.assertEqual(["scripts/tool_cli.py", "scripts/tool_cli_context.py", "scripts/tool_cli_flow.py",
                          "scripts/tool_cli_verdict.py"], own)

    def test_the_lib_and_what_no_import_reaches_are_not_the_tools(self) -> None:
        own = tooling_closure.own_files(checkout(), "/repo", ["scripts/tool_cli.py"], extra=["scripts/tool.sh"])
        self.assertIn("scripts/tool.sh", own)
        self.assertNotIn("scripts/lib/tool_process.py", own)
        self.assertNotIn("scripts/tool_cli_unrelated.py", own)


if __name__ == "__main__":
    unittest.main()
