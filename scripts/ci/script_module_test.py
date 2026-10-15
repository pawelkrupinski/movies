#!/usr/bin/env python3
"""script_module.py: a tool whose file name is no module name -- hyphenated, or with no extension at all -- imports
for its test."""

import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import script_module  # noqa: E402


class ScriptModuleTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()

    def tool(self, name, body):
        path = os.path.join(self.dir, name)
        with open(path, "w", encoding="utf-8") as f:
            f.write(body)
        return path

    def test_a_hyphenated_tool_imports_under_its_underscored_name(self):
        module = script_module.load(self.tool("check-the-thing.py", "def answer():\n    return 42\n"))
        self.assertEqual(42, module.answer())
        self.assertEqual("check_the_thing", module.__name__)

    def test_an_extensionless_tool_imports_too_without_running_its_main(self):
        path = self.tool("identity-report", "#!/usr/bin/env python3\nRAN = []\nif __name__ == '__main__':\n    RAN.append(1)\n")
        self.assertEqual([], script_module.load(path).RAN)

    def test_a_name_can_be_given(self):
        self.assertEqual("findings", script_module.load(self.tool("convergence-findings.py", ""), "findings").__name__)


if __name__ == "__main__":
    unittest.main()
