#!/usr/bin/env python3
"""python_floor_lint.py: a Python file that would break on the floor fails, one within it passes, and the repository is
within it."""

import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import python_floor_lint as lint  # noqa: E402

REPO = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")
FLOOR = (3, 9)


class PythonFloorLintTest(unittest.TestCase):
    def test_a_runtime_union_annotation_fails_on_the_floor(self):
        # scripts/discover-cinemas/discover.py:142's shape, without its __future__ import
        self.assertEqual([2], [line for line, _ in lint.findings_in("import re\ndef f(x: str | None) -> int:\n    return 0\n", FLOOR)])
        self.assertEqual([1], [line for line, _ in lint.findings_in("def f() -> list[str] | None:\n    pass\n", FLOOR)])
        self.assertEqual([1], [line for line, _ in lint.findings_in("x: int | None = None\n", FLOOR)])

    def test_deferred_annotations_are_never_evaluated_so_pass(self):
        self.assertEqual([], lint.findings_in("from __future__ import annotations\ndef f(x: str | None) -> int:\n    return 0\n", FLOOR))

    def test_a_union_outside_an_annotation_is_no_annotation_finding(self):
        self.assertEqual([], lint.findings_in("flags = 1 | 2\n", FLOOR))

    def test_syntax_newer_than_the_floor_fails(self):
        found = lint.findings_in("match 1:\n    case 1:\n        pass\n", FLOOR)
        self.assertEqual(1, len(found))
        self.assertIn("syntax newer than Python 3.9", found[0][1])

    def test_syntax_feature_version_lets_through_fails_too(self):
        # ast.parse(feature_version=) is best-effort: on 3.12+ it parses PEP 701 f-strings whatever the floor
        cases = {
            "a nested f-string quote": 'd = {}\nx = f"{d["k"]}"\n',
            "a backslash in an f-string expression": "x = f\"{'\\n'.join([])}\"\n",
            "a comment in an f-string expression": 'x = f"""{1  # one\n}"""\n',
            "a single-quoted f-string expression across lines": 'x = f"{1 +\n2}"\n',
            "a nested f-string reusing its quote": 'x = f"{f"{1}"}"\n',
            "an except*": "try:\n    pass\nexcept* ValueError:\n    pass\n",
            "a type alias statement": "type X = int\n",
            "a type parameter list": "def f[T](x: T) -> T:\n    return x\n",
        }
        for what, source in cases.items():
            with self.subTest(what):
                found = lint.findings_in(source, FLOOR)
                self.assertEqual(1, len(found), found)
                self.assertIn("syntax newer than Python 3.9", found[0][1])

    def test_syntax_the_floor_has_passes(self):
        cases = {
            "a nested quote of the other kind": "d = {}\nx = f\"{d['k']}\"\n",
            "a triple-quoted f-string holding single quotes": 'd = {}\nx = f"""{d["k"]}"""\n',
            "a backslash outside the expression": 'x = f"\\n{1}"\n',
            "a format spec": 'x = f"{1:>{2}}"\n',
            # accepted by 3.9's own parser, though only documented from 3.10
            "parenthesised context managers": "with (open('a') as f, open('b') as g):\n    pass\n",
        }
        for what, source in cases.items():
            with self.subTest(what):
                self.assertEqual([], lint.findings_in(source, FLOOR))

    def test_an_interpreter_at_the_floor_parses_for_real(self):
        floor = lint.floor_interpreter(FLOOR)
        if floor is None:
            self.skipTest("no Python 3.9 here (macOS's /usr/bin/python3 is one)")
        root = tempfile.mkdtemp()
        with open(os.path.join(root, "new.py"), "w") as f:
            f.write('d = {}\nx = f"{d["k"]}"\n')
        with open(os.path.join(root, "old.py"), "w") as f:
            f.write("x = 1\n")
        found = lint.floor_syntax(floor, root, ["new.py", "old.py"])
        self.assertEqual(["new.py"], [path for path, _, _ in found])

    def test_an_interpreter_off_the_floor_is_not_taken_for_it(self):
        fake = os.path.join(tempfile.mkdtemp(), "python3")
        with open(fake, "w") as f:
            f.write("#!/bin/sh\necho 3.11\n")
        os.chmod(fake, 0o755)
        self.assertFalse(lint.is_floor(fake, FLOOR))

    def test_the_floor_is_the_runners(self):
        self.assertEqual(FLOOR, lint.floor_of(REPO))

    def test_it_reads_extensionless_python_scripts_and_fails_the_run(self):
        root = tempfile.mkdtemp()
        os.makedirs(os.path.join(root, "scripts"))
        with open(os.path.join(root, lint.RUNNER), "w") as f:
            f.write('PythonFloor="3.9"\n')
        with open(os.path.join(root, "scripts", "tool"), "w") as f:
            f.write("#!/usr/bin/env python3\ndef f(x: str | None):\n    pass\n")
        with open(os.path.join(root, "scripts", "shell"), "w") as f:
            f.write("#!/usr/bin/env bash\necho 'def f(x: str | None): pass'\n")
        lint.git(root, "init", "-q")
        lint.git(root, "add", "-A")
        self.assertEqual(["scripts/tool"], list(lint.python_files(root)))
        self.assertEqual(1, lint.main([root]))

    def test_the_repository_is_within_the_floor(self):
        self.assertEqual(0, lint.main([REPO]))


if __name__ == "__main__":
    unittest.main()
