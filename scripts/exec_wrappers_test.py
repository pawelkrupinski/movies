#!/usr/bin/env python3
"""The one-line `exec python3 <tool>.py` wrappers kept for the shell tools' callers (the migration's CLI-compat layer;
WRAPPERS) are all one shape: a header, then exactly `exec python3 [-I -B] "$(dirname "$0")/<sibling>.py" "$@"`, the sibling there.
No hand-edited variants (BASH_SOURCE cd-dances, a second statement)."""
from __future__ import annotations

import os
import re
import unittest
from typing import List

SCRIPTS = os.path.dirname(os.path.abspath(__file__))
WRAPPER = re.compile(r'^exec python3 (?:-I -B )?"\$\(dirname "\$0"\)/([a-z_]+\.py)" "\$@"$')


def body(path: str) -> List[str]:
    with open(path, encoding="utf-8") as source:
        return [line.rstrip("\n") for line in source if line.strip() and not line.startswith("#")]


WRAPPERS = ["local-mirror/mirror.sh", "local-mirror/service.sh", "local-mirror/start-local-mongo.sh", "reset-corpus.sh",
            "restore-country-corpus.sh", "rule-firing-prod"]


def wrappers() -> List[str]:
    return [os.path.join(SCRIPTS, name) for name in WRAPPERS]


class ExecWrapperTest(unittest.TestCase):
    def test_every_wrapper_is_the_one_exec_line_of_the_template(self) -> None:
        for path in wrappers():
            lines = body(path)
            with self.subTest(wrapper=os.path.relpath(path, SCRIPTS)):
                self.assertEqual(1, len(lines), f"a header and one line, not: {lines}")
                match = WRAPPER.match(lines[0])
                self.assertIsNotNone(match, f"not the template: {lines[0]}")
                if match:
                    self.assertTrue(os.path.isfile(os.path.join(os.path.dirname(path), match.group(1))),
                                    f"{match.group(1)} is not beside it")


if __name__ == "__main__":
    unittest.main()
