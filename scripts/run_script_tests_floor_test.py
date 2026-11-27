"""scripts/run-script-tests, black-box (ported from run-script-tests-test.sh): --bash runs each .sh test, and every tool it
starts through `#!/usr/bin/env bash`, by that bash; with --changed it is THE FLOOR'S OWN SCOPE -- the bash the change
touches parsed by the floor and its added lines held to the bash-4-only constructs (scripts/floor-constructs.tsv), the
self-tests of the bash tools it touches and of those sourcing them re-run under it; and --floor is both runs in ONE pool.
Run: python3 scripts/run_script_tests_floor_test.py"""
from __future__ import annotations

import os
import shutil
import unittest
from typing import List

from run_script_tests_fixture import ScriptTestsCase
import cli_harness

CONSTRUCTS = ["mapfile -t lines < f", 'echo "${name,,}"', "wait -n", 'echo "${arr[-1]}"', "exec {fd}> f",
              "cmd |& tee f", "echo x &>> f", 'printf "%(%s)T" -1', "[[ -v name ]]", 'echo "${name@Q}"',
              "local -n ref=x", "shopt -s globstar", 'echo "$EPOCHSECONDS"', "coproc cat", "declare -g x=1",
              "declare -u name", "local -l name=x", "echo {1..9..2}"]
HARMLESS = ["# mapfile and ${x,,} need bash 4", 'echo "${BASHPID:-$$}" # on 3.2: $$', "declare -a list",
            'echo "${x:-a}|${#y}"', "local -r name=x", "echo {1..9}"]


class BashTest(ScriptTestsCase):
    def test_bash_runs_the_test_and_the_tools_it_starts_through_env_bash(self) -> None:
        real = shutil.which("bash") or "/bin/bash"
        fake = self.write("fake-bash", '#!/bin/sh\necho "$*" >> "%s"\nexec %s "$@"\n' % (self.path("via-fake"), real))
        os.chmod(fake, 0o755)
        self.put("scripts/env/tool-test.sh", "scripts/env/tool")
        self.put("scripts/env/tool", "#!/usr/bin/env bash\necho tool ran", mode=0o755)
        done = self.runner("--bash", fake, "scripts/env")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["scripts/env/tool-test.sh", "scripts/env/tool"], self.read("via-fake").splitlines())
        self.assertEqual(64, self.runner("--bash", self.path("no-such-bash"), "scripts/env").code)


class FloorCase(ScriptTestsCase):
    """A committed tree of bash tools, their tests, and a floor bash that cannot parse a FLOOR_CANNOT_PARSE line."""

    def setUp(self) -> None:
        super().setUp()
        real = shutil.which("bash") or "/bin/bash"
        self.floor_bash = self.write("floor-bash", '#!/bin/sh\n[ "$1" = -n ] && grep -q FLOOR_CANNOT_PARSE "$2" && '
                                                   '{ echo "$2: syntax error" >&2; exit 2; }\nexec %s "$@"\n' % real)
        os.chmod(self.floor_bash, 0o755)
        for relative, text in (("scripts/fl/tool.sh", "echo tool"), ("scripts/fl/tool-test.sh", "bash scripts/fl/tool.sh"),
                               ("scripts/fl/mention-test.sh", 'echo "reads what scripts/fl/tool.sh prints"'),
                               ("scripts/fl/flib.sh", "echo lib"), ("scripts/fl/user.sh", '. "$(dirname "$0")/flib.sh"'),
                               ("scripts/fl/user-test.sh", "bash scripts/fl/user.sh"), ("scripts/fl/py.py", "print(1)"),
                               ("scripts/fl/py_test.py", "# script-test-reads: scripts/fl/*.py\nprint(1)"),
                               ("scripts/fl/bare", "#!/usr/bin/env bash\necho bare"),
                               ("scripts/fl/bare-test.sh", "bash scripts/fl/bare")):
            self.put(relative, text)
        self.floorbase = self.commit("floor")

    def floor(self, *args: str) -> cli_harness.Done:
        return self.runner("--bash", self.floor_bash, "--changed", self.floorbase, *args)

    def floor_listed(self) -> str:
        return self.listed("--bash", self.floor_bash, "--changed", self.floorbase)

    def findings(self, prefix: str) -> List[str]:
        return [line for line in self.floor().stdout.splitlines() if line.startswith("FLOOR") and prefix in line]


class FloorTest(FloorCase):
    def test_the_scope_is_a_changed_bash_files_own_self_tests(self) -> None:
        self.append("scripts/fl/tool.sh", "echo edited")
        self.assertEqual("scripts/fl/mention-test.sh scripts/fl/tool-test.sh", self.listed("--changed", self.floorbase))
        self.assertEqual("scripts/fl/tool-test.sh", self.floor_listed())
        done = self.floor()
        self.assertEqual((0, 1), (done.code, self.rows(done, "PASS", "fl/tool-test.sh")), done.output)
        self.restore("scripts/fl/tool.sh")
        for edited, selected in (("scripts/fl/flib.sh", "scripts/fl/user-test.sh"),
                                 ("scripts/fl/bare", "scripts/fl/bare-test.sh")):
            self.append(edited, "echo edited")
            self.assertEqual(selected, self.floor_listed())
            self.restore(edited)
        self.append("scripts/fl/py.py", "print(2)")
        done = self.floor()
        self.assertEqual(0, done.code, done.output)
        self.assertIn("no script tests selected", done.stdout)

    def test_an_added_bash_4_construct_fails_the_floor_run_naming_it(self) -> None:
        self.append("scripts/fl/tool.sh", "declare -A seen\nseen[x]=1")
        done = self.floor()
        self.assertEqual((1, 1), (done.code, self.rows(done, "PASS", "fl/tool-test.sh")), done.output)
        self.assertRegex(done.stdout, r"(?m)^FLOOR .*scripts/fl/tool\.sh:2: an associative array, declare -A \(bash "
                                      r"4\.0\)")
        self.assertEqual(0, self.runner("--changed", self.floorbase, "scripts/fl").code)
        self.restore("scripts/fl/tool.sh")
        self.append("scripts/fl/mention-test.sh", "readarray -t x < f")
        self.assertEqual(1, len(self.findings("scripts/fl/mention-test.sh:2:")))

    def test_each_bash_4_construct_is_held_and_nothing_harmless(self) -> None:
        for construct in CONSTRUCTS:
            self.append("scripts/fl/tool.sh", construct)
            self.assertEqual(1, len(self.findings("scripts/fl/tool.sh:2:")), construct)
            self.restore("scripts/fl/tool.sh")
        self.append("scripts/fl/tool.sh", "\n".join(HARMLESS))
        self.assertEqual(0, self.floor().code)
        self.restore("scripts/fl/tool.sh")
        self.put("scripts/fl/data.sh", '# bash-floor: a lint naming the constructs as data\necho "declare -A is 4.0"')
        self.assertEqual(0, self.floor().code)
        os.remove(os.path.join(self.tree, "scripts/fl/data.sh"))
        self.append("scripts/fl/py.py", "mapfile = 1")
        self.assertEqual(0, self.floor().code)

    def test_a_file_the_floor_cannot_parse_and_which_lines_count_as_added(self) -> None:
        self.append("scripts/fl/user.sh", "echo FLOOR_CANNOT_PARSE")
        done = self.floor()
        self.assertEqual(1, done.code, done.output)
        self.assertRegex(done.stdout, r"(?m)^FLOOR .*scripts/fl/user\.sh: bash 3\.2 cannot parse it")
        self.restore("scripts/fl/user.sh")
        self.append("scripts/fl/tool.sh", "readarray -t x < f")
        self.commit("a construct committed")
        self.assertEqual(1, len(self.findings("scripts/fl/tool.sh:2:")))
        self.put("scripts/fl/tool.sh", "echo tool\necho after")
        self.assertEqual([], self.findings(""))
        self.put("scripts/fl/tool.sh", "echo tool")
        self.commit("the construct gone")
        self.put("scripts/fl/new.sh", "readarray -t x < f")
        self.assertEqual(1, len(self.findings("scripts/fl/new.sh:1:")))


class OnePoolTest(FloorCase):
    def test_floor_runs_its_scope_under_the_floor_bash_and_the_rest_natively(self) -> None:
        self.put("scripts/fl/which-test.sh", 'echo "floor $(command -v bash)" >> "$WHICH_BASH"')
        self.put("scripts/fl/which.sh", "echo which")
        self.put("scripts/fl/names-which-test.sh", 'echo "reads what scripts/fl/which.sh prints"; '
                                                  'echo "native $(command -v bash)" >> "$WHICH_BASH"')
        which = self.commit("a tool whose test says which bash runs it")
        self.append("scripts/fl/which.sh", "echo edited")
        env = {"WHICH_BASH": self.write("which-bash", "")}
        done = self.runner("--floor", self.floor_bash, "--changed", which, env=env)
        seen = dict(line.split(" ", 1) for line in self.read("which-bash").splitlines())
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(2, len(seen))
        self.assertTrue(seen["floor"].endswith("floor-bin/bash"), seen)
        self.assertNotEqual(seen["floor"], seen["native"])
        self.assertRegex(done.stdout, r"(?m)^PASS .*fl/names-which-test\.sh$")
        self.assertRegex(done.stdout, r"(?m)^PASS .*fl/which-test\.sh \(floor\)$")
        self.append("scripts/fl/which.sh", "readarray -t x < f")
        done = self.runner("--floor", self.floor_bash, "--changed", which, env=env)
        self.assertEqual(1, done.code)
        self.assertRegex(done.stdout, r"(?m)^FLOOR .*scripts/fl/which\.sh:3:")
        self.assertEqual(64, self.runner("--floor", self.floor_bash).code)
        self.assertEqual(64, self.runner("--floor", self.floor_bash, "--bash", self.floor_bash, "--changed", which).code)

    def test_a_python_self_test_in_its_scope_runs_under_python3_the_floor_bash_first_on_its_path(self) -> None:
        self.put("scripts/fl/pylib.sh", "echo pylib")
        self.put("scripts/fl/pytool", '#!/usr/bin/env bash\n. "$(dirname "$0")/pylib.sh"')
        self.put("scripts/fl/pytool_test.py", 'import subprocess\nsubprocess.run(["bash", "-c", "echo py $(command -v '
                                              'bash) >> \\"$WHICH_BASH\\""], check=True)')
        base = self.commit("a bash tool with a Python self-test")
        self.append("scripts/fl/pylib.sh", "echo edited")
        done = self.runner("--floor", self.floor_bash, "--changed", base, env={"WHICH_BASH": self.write("which", "")})
        self.assertEqual(0, done.code, done.output)
        self.assertRegex(done.stdout, r"(?m)^PASS .*fl/pytool_test\.py \(floor\)$")
        self.assertRegex(self.read("which"), r"(?m)^py .*floor-bin/bash$")


if __name__ == "__main__":
    unittest.main()
