#!/usr/bin/env python3
"""scripts/py-quality-report and its ratchet (py_quality_ratchet.py), black-box over a scratch checkout with a fake
`node` standing in for pyright: Sonar's cognitive complexity and McCabe's cyclomatic as specified; a new function past a
limit, a clone across files, an import cycle, a fan-out past 9 and a new file's type errors each fail --check, while a
self-test's rows are reported, never held; --paths type-checks only the push's own files, the rest keeping their
baseline rows; --update records what --check holds; a raised `pyright` row lands only with a `Typing:` trailer; a
migrated tool's new file (migrate-tools.tsv) past a limit is refused even recorded under its trailer; no node blocks
(exit 2)."""
from __future__ import annotations

import ast
import os
import shutil
import sys
import unittest
from typing import Dict

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(os.path.dirname(HERE), "lib")]
from harness import cli_harness  # noqa: E402
import functions  # noqa: E402

REPORT = os.path.join(HERE, "report.py")
SIMPLE = "def f(x):\n    return x\n"
HAIRY = """def hairy(x):
    if x:
        for i in x:
            if i and x or i:
                pass
            elif i:
                pass
            else:
                pass
    try:
        pass
    except ValueError:
        pass
    return 1 if x else 2
"""
CLONE = "def table():\n    return [" + ", ".join(f"('row{n}', {n}, {n * 2})" for n in range(12)) + "]\n"
# A fake pyright run: one error per `# type-error` line of each file asked, in pyright's --outputjson shape
NODE = """
files = [a for a in argv[1:] if a.endswith('.py')]  # like pyright: .py only
diagnostics = [{'file': os.path.abspath(f), 'severity': 'error' if '# type-error' in line else 'warning'}
               for f in files for line in open(f) if '# type-error' in line or '# type-warning' in line]
out(json.dumps({'generalDiagnostics': diagnostics}))
exit(1 if diagnostics else 0)
"""


class MeasureTest(unittest.TestCase):
    def test_cognitive_and_cyclomatic_as_specified(self) -> None:
        [fn] = functions.functions(ast.parse(HAIRY))
        # if 1, for 2, if 3, its two boolean runs 2, elif 1, else 1, except 1, ternary 1
        self.assertEqual((12, 9, 14), (fn.cognitive, fn.cyclomatic, fn.lines))

    def test_a_nested_def_is_measured_by_itself_and_counted_by_its_holder(self) -> None:
        tree = ast.parse("def outer():\n    def a():\n        if x:\n            pass\n    def b():\n        pass\n")
        measured = {f.name: f for f in functions.functions(tree)}
        self.assertEqual((0, 2), (measured["outer"].cognitive, measured["outer"].inner_defs))
        self.assertEqual(1, measured["outer.a"].cognitive)


class ReportTest(cli_harness.ToolCase):
    TOOL = REPORT

    def setUp(self) -> None:
        super().setUp()
        self.root = self.repo("checkout")
        cache = self.path("cache")
        self.fakes.program("node", NODE)
        env = {"KINOWO_TOOL_CACHE": cache}
        installed = self.run_tool("--tools-dir", env=env).stdout.strip()
        self.write(os.path.join(os.path.relpath(installed, self.work), "node_modules/pyright/index.js"), "// fake\n")
        self.tool_env: Dict[str, str] = env
        self.files({"scripts/ci/plain.py": SIMPLE, "pyrightconfig.json": "{}\n"})
        self.assertEqual(0, self.report("--update").code)
        self.commit("base")

    def files(self, files: Dict[str, str]) -> None:
        for path, text in files.items():
            self.write(os.path.join("checkout", path), text)

    def commit(self, message: str) -> None:
        self.git(self.root, "add", "-A")
        self.git(self.root, "commit", "-qm", message)

    def report(self, *args: str) -> cli_harness.Done:
        return self.run_tool("--root", self.root, *args, env=self.tool_env, cwd=self.root)

    def baseline(self) -> str:
        return self.read("checkout/test/resources/py-quality-baseline.tsv")

    def assert_check_fails(self, *said: str) -> None:
        done = self.report("--check")
        self.assertEqual(1, done.code, done.output)
        for s in said:
            self.assertIn(s, done.output)

    def test_the_baseline_itself_passes(self) -> None:
        done = self.report("--check")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("py quality: within", done.output)

    def test_a_new_function_past_a_limit_fails_and_update_records_it(self) -> None:
        self.files({"scripts/ci/hairy.py": HAIRY})
        self.assert_check_fails("cognitive scripts/ci/hairy.py:hairy: 12, past the limit 10 (no baseline row)")
        updated = self.report("--update")
        self.assertEqual(1, updated.code)  # recorded, but a rise: said, and exit 1
        self.assertIn("cognitive scripts/ci/hairy.py:hairy raised <=10 -> 12", updated.output)
        self.assertIn("cognitive\tscripts/ci/hairy.py:hairy\t12\n", self.baseline())
        self.assertEqual(0, self.report("--check").code)

    def test_a_clone_and_a_cycle_fail_and_fan_out_is_the_design_rules(self) -> None:
        many = {f"scripts/ci/m{n}.py": SIMPLE for n in range(10)}
        self.files({"scripts/ci/a.py": CLONE + "import b\n", "scripts/ci/b.py": CLONE + "import a\n",
                    "scripts/ci/wide.py": "".join(f"import m{n}\n" for n in range(10)), **many})
        self.assert_check_fails("copies scripts/ci/a.py|scripts/ci/b.py: 2",
                                "cycle scripts/ci/a.py|scripts/ci/b.py: 2")
        self.assertNotIn("fanout scripts/ci/wide.py", self.report("--check").output)

    def test_a_cycle_through_a_library_package_is_found_by_its_dotted_imports(self) -> None:
        self.files({"scripts/lib/shared/c.py": "from shared import d\n", "scripts/lib/shared/d.py": "from shared.c import X\n",
                    "scripts/ci/user.py": "import shared.c\n"})
        self.assert_check_fails("cycle scripts/lib/shared/c.py|scripts/lib/shared/d.py: 2")

    def test_a_self_tests_rows_are_reported_never_held(self) -> None:
        self.files({"scripts/ci/hairy_test.py": HAIRY + "x = 1  # type-error\n"})
        done = self.report("--check")
        self.assertEqual(0, done.code, done.output)

    def test_a_new_files_type_errors_fail_and_paths_type_check_only_the_push(self) -> None:
        self.files({"scripts/ci/typed.py": "x = 1  # type-error\n", "scripts/ci/plain.py": SIMPLE + "y = 2  # type-error\n"})
        self.write("paths", "scripts/ci/typed.py\n")
        done = self.report("--check", "--paths", self.path("paths"))
        self.assertEqual(1, done.code, done.output)
        self.assertIn("pyright scripts/ci/typed.py: 1, past the limit 0", done.output)
        self.assertNotIn("plain.py", done.output)  # not asked: its baseline row (none) stands
        self.assertEqual([["scripts/ci/typed.py"]], [[a for a in c if a.endswith(".py")] for c in self.fakes.calls("node")][-1:])

    def test_a_file_with_only_pyright_warnings_has_no_row_and_an_error_still_counts(self) -> None:
        self.files({"scripts/ci/warned.py": "x = 1  # type-warning\n", "scripts/ci/typed.py": "y = 1  # type-warning\nz = 2  # type-error\n"})
        self.assert_check_fails("pyright scripts/ci/typed.py: 1, past the limit 0")
        done = self.report("--check")
        self.assertNotIn("warned.py", done.output)

    def test_an_extensionless_python_tools_type_errors_are_counted_under_its_own_name(self) -> None:
        self.files({"scripts/ci/tool": "#!/usr/bin/env python3\nx = 1  # type-error\n"})
        self.assert_check_fails("pyright scripts/ci/tool: 1, past the limit 0")
        self.assertFalse(os.path.exists(os.path.join(self.root, "scripts/ci/tool.extensionless.py")))

    def test_a_raised_pyright_row_lands_only_with_a_typing_trailer(self) -> None:
        self.files({"scripts/ci/plain.py": SIMPLE + "y = 2  # type-error\n"})
        self.report("--update")
        self.commit("an untyped line")
        base = self.git(self.root, "rev-parse", "HEAD~1")
        self.assert_check_fails_range(f"{base}..HEAD", "no `Typing: <why>` trailer")
        self.git(self.root, "commit", "-q", "--amend", "-m", "an untyped line\n\nTyping: the fake's one error")
        done = self.report("--check", "--range", f"{base}..HEAD")
        self.assertEqual(0, done.code, done.output)

    def test_a_migrated_tools_row_is_refused_even_recorded_under_its_trailer(self) -> None:
        # an old file with its frozen row, which the migrated tool calls: still admitted by its row
        self.files({"scripts/old.py": HAIRY})
        self.report("--update")
        self.commit("the old code, frozen")
        base = self.git(self.root, "rev-parse", "HEAD")
        self.files({
            "scripts/shell-py-progress/migrate-tools.tsv": f"# its base {base}\n1\tscripts/tool\t\n",
            "scripts/tool": '#!/bin/sh\nexec python3 "$(dirname "$0")/tool_cli.py" "$@"\n',
            "scripts/tool_cli.py": "import old\nimport tool_part\n",
            "scripts/tool_part.py": HAIRY.replace("hairy", "part"),
            "scripts/unlisted.py": HAIRY.replace("hairy", "other"),
        })
        self.report("--update")
        self.commit("a migrated tool past a limit\n\nComplexity: recorded, as an unlisted tool could be")
        done = self.report("--check", "--range", f"{base}..HEAD")
        self.assertEqual(1, done.code, done.output)
        refused = done.output.split("where no row is admitted:", 1)[1]
        self.assertIn("cognitive scripts/tool_part.py:part: 12, past the limit 10", refused)
        self.assertIn("migrated tools stay under the bar — split it", refused)
        self.assertNotIn("old.py", refused)       # not new: the tool's old code keeps its row
        self.assertNotIn("unlisted.py", refused)  # no listed tool reaches it: its trailer admits it
        self.files({"scripts/tool_part.py": SIMPLE})
        self.report("--update")
        self.commit("split")
        done = self.report("--check")
        self.assertEqual(0, done.code, done.output)

    def test_a_migration_base_the_clone_lacks_is_an_error(self) -> None:
        self.files({"scripts/shell-py-progress/migrate-tools.tsv": "# its base 0123456789abc\n1\tscripts/tool\t\n"})
        done = self.report("--check")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("the migration base 0123456789abc", done.stderr)

    def assert_check_fails_range(self, rng: str, said: str) -> None:
        done = self.report("--check", "--range", rng)
        self.assertEqual(1, done.code, done.output)
        self.assertIn(said, done.output)

    def test_no_node_blocks(self) -> None:
        # A PATH of git alone: a system PATH may hold a node (the --linux container's /usr/bin/node)
        git_only = self.path("git-only-bin")
        os.makedirs(git_only)
        for name in "git".split():
            os.symlink(shutil.which(name) or name, os.path.join(git_only, name))
        env = cli_harness.isolated_env(os.environ, dict(self.tool_env, PATH=git_only))
        done = cli_harness.run_bounded([sys.executable, REPORT, "--root", self.root, "--check"], env=env, cwd=self.root,
                                       stdin=None)
        self.assertEqual(2, done.code, done.output)
        self.assertIn("node is not installed", done.stderr)

if __name__ == "__main__":
    unittest.main()
