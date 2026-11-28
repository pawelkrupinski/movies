"""scripts/run-script-tests, black-box (ported from run-script-tests-test.sh): what it discovers under the script roots
(and what a `# script-test: <tag>` header keeps out of the default run), its table, failures and exit status, the path
filters, --changed against a base (a tool's edit, a file a test names, what a tool sources or imports however deep, the
globs a test declares it reads, --paths, --readers, --reads), the per-test timeout and the no-progress bound, the pool's
width, the longest-first order, the Python floor, and its usage.
Run: python3 scripts/run_script_tests_test.py"""
from __future__ import annotations

import os
import re
import sys
import unittest
from typing import Dict, Optional

from run_script_tests_fixture import ROOT, RUNNER, ScriptTestsCase
import cli_harness


class DiscoveryTest(ScriptTestsCase):
    def test_it_finds_the_tests_under_the_roots_sorted_untagged_only(self) -> None:
        self.assertEqual(".github/actions/x/e-test.sh android/scripts/f-test.sh scripts/a-test.sh scripts/c-test.sh "
                         "scripts/ci/b_test.py", self.listed())
        done = self.runner("--list", "--all")
        self.assertIn("scripts/d-test.sh\tmanual\tneeds a live server\n", done.stdout)

    def test_a_failing_test_fails_the_run_its_row_output_and_whole_log_kept(self) -> None:
        done = self.runner()
        self.assertEqual(1, done.code, done.output)
        self.assertEqual((4, 1), (self.rows(done, "PASS", ""), self.rows(done, "FAIL", "")))
        self.assertEqual(1, self.rows(done, "FAIL", "scripts/c-test.sh"))
        self.assertRegex(done.stdout, r"(?m)^ *boom$")
        self.assertRegex(done.stdout, r"(?m)^1 of 5 failed \(\d+s\)$")
        kept = re.search(r"(?m)^ *\(whole log: (.*)\)$", done.stdout)
        self.assertIsNotNone(kept, done.output)
        with open(kept.group(1) if kept else "") as f:
            lines = f.read().splitlines()
        self.assertEqual((True, "boom"), ("line 1" in lines, lines[-1]))
        self.assertEqual(2, len(re.findall(r"(?m)^ *(✗ an early case|expected: yes)$", done.stdout)))
        self.assertNotIn("d-test", done.stdout)

    def test_the_filters_pick_by_directory_substring_and_glob(self) -> None:
        done = self.runner("scripts/ci")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual((1, 0), (self.rows(done, "PASS", "b_test.py"), done.stdout.count("a-test")))
        self.assertEqual("scripts/a-test.sh", self.listed("a-test"))
        self.assertEqual("scripts/a-test.sh scripts/c-test.sh", self.listed("scripts/[ac]-*"))
        self.assertEqual(1, self.runner("no-such-test").code)
        done = self.runner("--tag", "manual")
        self.assertEqual((1, 1), (done.code, self.rows(done, "FAIL", "d-test")))

    def test_a_test_sees_none_of_the_ci_runs_variables(self) -> None:
        names = "GITHUB_REF GITHUB_EVENT_NAME GITHUB_SERVER_URL GITHUB_SHA GITHUB_REPOSITORY GITHUB_RUN_ID GITHUB_ENV"
        self.put("scripts/runctx-test.sh", 'for v in %s; do eval "[ -z \\"\\${$v+set}\\" ]" || { echo "$v leaked"; '
                                           'exit 1; }; done' % names)
        ci = {"GITHUB_REF": "refs/heads/feature", "GITHUB_EVENT_NAME": "push", "GITHUB_SERVER_URL": "https://github.com",
              "GITHUB_SHA": "abc", "GITHUB_REPOSITORY": "o/r", "GITHUB_RUN_ID": "1", "GITHUB_ENV": os.devnull}
        done = self.runner("runctx-test", env=ci)
        self.assertEqual(0, done.code, done.output)
        self.assertNotIn("leaked", done.stdout)


class ChangedTest(ScriptTestsCase):
    def test_a_tools_edit_a_named_files_edit_and_a_new_tool_select_their_tests(self) -> None:
        self.append("scripts/a.sh", "echo tool a, edited")
        self.commit("edit tool a")
        self.assertEqual("scripts/a-test.sh", self.listed("--changed", self.base))
        self.append("scripts/ci/lib.sh", "# edited")
        self.assertEqual("scripts/a-test.sh scripts/c-test.sh", self.listed("--changed", self.base))
        self.restore("scripts/ci/lib.sh")
        self.put("scripts/ci/b.py", "print(1)")
        self.assertEqual("scripts/a-test.sh scripts/ci/b_test.py", self.listed("--changed", self.base))

    def test_an_unrelated_change_selects_nothing_and_a_base_no_commit_fails(self) -> None:
        self.put("docs/readme.md", "unrelated")
        done = self.runner("--changed", self.base)
        self.assertEqual(0, done.code, done.output)
        self.assertIn("no script tests selected", done.stdout)
        self.assertEqual(2, self.runner("--changed", "no-such-ref").code)

    def libraries(self) -> str:
        self.put("scripts/k.sh", '# shellcheck source=scripts/lib/shared.sh\n. "$(dirname "${BASH_SOURCE[0]}")/lib/'
                                 'shared.sh"\nshared')
        self.put("scripts/k-test.sh", "bash scripts/k.sh")
        self.put("scripts/lib/shared.sh", 'source "$(dirname "${BASH_SOURCE[0]}")/deeper.sh"\nshared() { deeper; }')
        self.put("scripts/lib/deeper.sh", "deeper() { echo deep; }")
        self.put("scripts/ci/m.py", "import os\nfrom takes_lib import take\nprint(take())")
        self.put("scripts/ci/m_test.py", "import m")
        self.put("scripts/ci/n_test.py", "import sys\nimport takes_lib")
        self.put("scripts/ci/takes_lib.py", "def take(): return 1")
        self.put("scripts/ci/unrelated_test.py", "print(1)")
        return self.commit("libraries")

    def test_what_a_tool_sources_or_imports_however_deep_selects_its_test(self) -> None:
        libs = self.libraries()
        for library, selected in (("scripts/lib/shared.sh", "scripts/k-test.sh"),
                                  ("scripts/lib/deeper.sh", "scripts/k-test.sh"),
                                  ("scripts/ci/takes_lib.py", "scripts/ci/m_test.py scripts/ci/n_test.py")):
            self.append(library, "# edited")
            self.assertEqual(selected, self.listed("--changed", libs), library)
            self.restore(library)

    def test_paths_lists_what_the_paths_a_file_names_select(self) -> None:
        self.libraries()
        paths = self.write("paths", "scripts/lib/deeper.sh\nscripts/ci/takes_lib.py\n")
        self.assertEqual("scripts/ci/m_test.py scripts/ci/n_test.py scripts/k-test.sh", self.listed("--paths", paths))
        self.write("paths", "")
        self.assertEqual("", self.listed("--paths", paths))
        self.assertEqual(2, self.runner("--list", "--paths", self.path("no-such-file")).code)
        self.assertEqual(64, self.runner("--paths", paths).code)


class ReadsTest(ScriptTestsCase):
    def setUp(self) -> None:
        super().setUp()
        self.put("scripts/ci/layout_test.py", '"""Every workflow to a rule."""\n'
                                              "# script-test-reads: .github/workflows/* .github/actions/*\nprint(1)")
        self.put("scripts/ci/floor_test.py", "# script-test-reads: *.py\nprint(1)")
        self.put("scripts/ci/late_test.py", "\n".join(str(n) for n in range(1, 41))
                 + "\n# script-test-reads: .github/workflows/*\nprint(1)")
        self.put("scripts/hooks/gate.sh", "exit 0")
        self.put("scripts/hooks/gate-test.sh", "echo runs scripts/hooks/gate.sh")
        self.put("scripts/hooks/gatekeeper-test.sh", 'echo "the hooks/gatekeeper and a gate"')
        self.put("scripts/hooks/gate-plan-test.sh", 'echo "runs $REPO_ROOT/scripts/hooks/gate --plan"')
        self.put("scripts/hooks/gate", "exit 0")
        self.put(".github/workflows/main.yml", "on: push")
        self.readers = self.commit("readers")

    def test_a_declared_glob_selects_its_test_within_the_first_40_lines(self) -> None:
        self.append(".github/workflows/main.yml", "# edited")
        self.assertEqual("scripts/ci/layout_test.py", self.listed("--changed", self.readers))
        self.append("scripts/hooks/gate.sh", "# edited")
        self.assertEqual("scripts/ci/layout_test.py scripts/hooks/gate-test.sh", self.listed("--changed", self.readers))
        self.assertEqual("scripts/ci/layout_test.py", self.listed("--changed", self.readers, "--readers"))
        self.assertEqual(64, self.runner("--readers").code)

    def test_a_py_anywhere_and_an_extensionless_python_tool_select_the_test_reading_every_py(self) -> None:
        self.put("tools/x/deep.py", "print(1)")
        self.assertEqual("scripts/ci/floor_test.py", self.listed("--changed", self.readers))
        os.remove(os.path.join(self.tree, "tools/x/deep.py"))
        self.put("tools/x/deep", "#!/usr/bin/env python3")
        self.put("tools/x/shelly", "#!/usr/bin/env bash")
        self.assertEqual("scripts/ci/floor_test.py", self.listed("--changed", self.readers))
        os.remove(os.path.join(self.tree, "tools/x/deep"))
        self.assertEqual("", self.listed("--changed", self.readers))

    def test_reads_lists_every_declared_glob_once_sorted(self) -> None:
        self.assertEqual("*.py\n.github/actions/*\n.github/workflows/*\n", self.runner("--reads").stdout)

    def test_an_extensionless_tool_is_named_by_its_directory_and_name(self) -> None:
        self.append("scripts/hooks/gate", "# edited")
        self.assertEqual("scripts/hooks/gate-plan-test.sh scripts/hooks/gate-test.sh",
                         self.listed("--changed", self.readers))

    def test_this_checkouts_watchdog_selects_the_hooks_tools_test(self) -> None:
        """14eb62591d: stall-watch.sh changed, --changed never selected pre-push-tools-test.sh, which runs it through
        the hook, and its stalled-check case failed on main."""
        paths = self.write("stall-paths", "scripts/stall-watch.sh\nscripts/stall_watch.py\n")
        done = self.run_tool("--root", ROOT, "--list", "--paths", paths)
        selected = [line.split("\t")[0] for line in done.stdout.splitlines()]
        self.assertIn("scripts/hooks/pre-push-tools-test.sh", selected, done.output)
        self.assertIn("scripts/stall_watch_test.py", selected)


class DerivedReadsTest(ScriptTestsCase):
    """THE DERIVED READS (14eb62591d): a test is selected by a change to what it runs and what that runs, with no
    `# script-test-reads:` line -- the test runs the hook, the hook runs the watchdog."""

    def setUp(self) -> None:
        super().setUp()
        self.put("scripts/hooks/hook", '#!/usr/bin/env bash\n"$(dirname "$0")/../watch.sh" watch "$$"', mode=0o755)
        self.put("scripts/watch.sh", '#!/bin/sh\nexec python3 "$(dirname "$0")/watch_py.py" "$@"', mode=0o755)
        self.put("scripts/watch_py.py", "print('watching')")
        self.put("scripts/hooks/hook-tools-test.sh", 'out=$(bash scripts/hooks/hook) && [ "$out" = watching ]')
        self.put("scripts/hooks/prose-test.sh", "# what scripts/hooks/hook runs is not this test's\nexit 0")
        self.derived = self.commit("a hook running a watchdog")

    def test_what_a_test_runs_runs_selects_it_with_no_declaration(self) -> None:
        self.append("scripts/watch.sh", "# edited")
        self.assertEqual("scripts/hooks/hook-tools-test.sh", self.listed("--changed", self.derived))
        done = self.runner("--changed", self.derived)
        self.assertEqual((0, 1), (done.code, self.rows(done, "PASS", "hooks/hook-tools-test.sh")), done.output)

    def test_never_through_a_comment_nor_past_two_steps_nor_in_the_floors_scope(self) -> None:
        self.append("scripts/watch_py.py", "# edited")
        self.assertNotIn("hook-tools-test", self.listed("--changed", self.derived))
        self.restore("scripts/watch_py.py")
        self.append("scripts/watch.sh", "# edited")
        self.assertEqual("", self.listed("--changed", self.derived, "--bash", "/bin/bash"))


class HubReadsTest(ScriptTestsCase):
    """A helper that many tests source names scripts for its own purpose: the tests through it do not read them
    (2026-11-27: tool_python.py reached 215 tests through shell-spec.sh -> tool-conventions.sh -> gh_download.py);
    a helper only a few tests source still lends them what it runs."""

    def setUp(self) -> None:
        super().setUp()
        self.put("scripts/hub.sh", 'run_it() { bash scripts/hubbed.sh; }')
        self.put("scripts/hubbed.sh", "exit 0")
        self.put("scripts/lib2.sh", 'run_it() { bash scripts/libbed.sh; }')
        self.put("scripts/libbed.sh", "exit 0")
        for n in range(12):
            self.put(f"scripts/hubbing/h{n}-test.sh", ". scripts/hub.sh\nexit 0")
        self.put("scripts/hubbing/lone-test.sh", ". scripts/lib2.sh\nexit 0")
        self.hubbed = self.commit("twelve tests sourcing a hub, one a library")

    def test_a_script_a_hub_runs_selects_none_of_the_tests_sourcing_it(self) -> None:
        self.append("scripts/hubbed.sh", "# edited")
        self.assertEqual("", self.listed("--changed", self.hubbed))

    def test_the_hub_itself_still_selects_every_test_sourcing_it(self) -> None:
        self.append("scripts/hub.sh", "# edited")
        self.assertEqual(" ".join(f"scripts/hubbing/h{n}-test.sh" for n in sorted(range(12), key=str)),
                         self.listed("--changed", self.hubbed))

    def test_a_script_a_library_few_tests_source_runs_still_selects_them(self) -> None:
        self.append("scripts/libbed.sh", "# edited")
        self.assertEqual("scripts/hubbing/lone-test.sh", self.listed("--changed", self.hubbed))


class BoundsTest(ScriptTestsCase):
    def test_a_hung_test_is_cut_at_its_timeout_and_left_running_none(self) -> None:
        self.put("scripts/slow-test.sh", "echo $$ >> '%s'; exec sleep 86400" % self.path("hung"))
        done = self.runner("--timeout", "1", "slow-test")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, self.rows(done, "TIMEOUT", "slow-test"))
        self.assertEqual(0, cli_harness.still_running(self.path("hung")))

    def test_a_test_making_no_progress_is_ended_as_stalled(self) -> None:
        self.put("scripts/slow-test.sh", "echo $$ >> '%s'; exec sleep 86400" % self.path("hung"))
        done = self.runner("--stall", "2", "slow-test", env={"KINOWO_STALL_POLL_SECONDS": "1"})
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, self.rows(done, "STALLED", "slow-test"))
        self.assertIn("stall report: process group", done.stdout)
        self.assertEqual(0, cli_harness.still_running(self.path("hung")))

    def test_a_test_still_making_progress_past_its_stall_bound_passes(self) -> None:
        self.put("scripts/busy-test.sh", 'for n in $(seq 1 40); do echo "case $n"; sleep 0.2; done')
        done = self.runner("--stall", "4", "busy-test", env={"KINOWO_STALL_POLL_SECONDS": "1"})
        self.assertEqual((0, 1), (done.code, self.rows(done, "PASS", "busy-test")), done.output)

    def test_the_pool_runs_at_most_jobs_at_once(self) -> None:
        running = self.path("running")
        os.makedirs(running)
        for n in range(1, 5):
            self.put(f"scripts/par/p{n}-test.sh",
                     'touch "$PAR_DIR/$$"; sleep 1; ls "$PAR_DIR" | wc -l >> "$PAR_DIR.max"; rm "$PAR_DIR/$$"')
        for jobs in (1, 2):
            if os.path.exists(running + ".max"):
                os.remove(running + ".max")
            self.runner("--jobs", str(jobs), "scripts/par", env={"PAR_DIR": running})
            with open(running + ".max") as f:
                self.assertEqual(jobs, max(int(line) for line in f if line.strip()))


class OrderTest(ScriptTestsCase):
    def order(self) -> str:
        log = self.write("ord.log", "")
        self.runner("--jobs", "1", "scripts/ord", env={"ORD_LOG": log})
        return " ".join(self.read("ord.log").split())

    def test_the_longest_first_and_one_with_no_record_before_them_all(self) -> None:
        for n in "abc":
            self.put(f"scripts/ord/{n}-test.sh", 'echo "$0" | sed "s|.*/||" >> "$ORD_LOG"')
        self.put("scripts/ord/d-test.sh", 'echo "$0" | sed "s|.*/||" >> "$ORD_LOG"\n# padding: d the larger file')
        self.assertEqual("d-test.sh a-test.sh b-test.sh c-test.sh", self.order())
        rows = self.read("durations.tsv").splitlines()
        self.assertEqual((4, 4), (len(rows), len({r.split("\t")[0] for r in rows})))
        self.write("durations.tsv", "scripts/ord/a-test.sh\t5\nscripts/ord/b-test.sh\t90\nscripts/ord/c-test.sh\t40\n"
                                    "scripts/ord/gone-test.sh\t7\n")
        self.assertEqual("d-test.sh b-test.sh c-test.sh a-test.sh", self.order())
        text = self.read("durations.tsv")
        self.assertEqual((4, 1), (len(re.findall(r"scripts/ord/.-test\.sh", text)), text.count("gone-test.sh\t7")))
        self.assertRegex(text, r"(?m)^scripts/ord/b-test\.sh\t[0-9]$")
        self.assertEqual([], [f for f in os.listdir(self.work) if f.startswith("durations.tsv.")])


class UsageTest(ScriptTestsCase):
    def test_a_python3_below_the_floor_fails_a_run_with_python_tests_before_any(self) -> None:
        # its tests' python3 too old; a runner in Python itself started by the real one (`*`: a script)
        self.write("oldpy/python3", '#!/bin/sh\ncase "$1" in -V|--version) echo "Python 3.8.10" ;; -c) exit 1 ;; '
                                    '*) exec %s "$@" ;; esac\n' % sys.executable)
        os.chmod(self.path("oldpy/python3"), 0o755)
        old = {"PATH": self.path("oldpy") + os.pathsep + self.env()["PATH"]}
        done = self.runner("scripts/ci", env=old)
        self.assertEqual(1, done.code, done.output)
        self.assertRegex(done.stderr, r"Python 3\.8\.10.*3\.9 or newer")
        self.assertEqual(0, self.rows(done, "PASS", ""))
        self.assertEqual(0, self.runner("scripts/a-test.sh", env=old).code)

    def test_usage_errors_exit_64_and_help_prints_the_header(self) -> None:
        self.assertEqual(64, self.runner("--bogus").code)
        done = self.runner("--help")
        self.assertEqual(0, done.code)
        self.assertRegex(done.stdout, r"(?m)^Options: --jobs N")
        self.assertRegex(done.stdout, r"(?m)^  scripts/run-script-tests --changed origin/main")
        done = self.runner("--jobs")
        self.assertEqual(64, done.code)
        self.assertIn("run-script-tests: --jobs needs a value", done.stderr)
        self.assertRegex(done.stderr, r"(?m)^Options: --jobs N")

    def test_this_checkouts_own_listing_finds_its_own_tests(self) -> None:
        done = self.run_tool("--list", "--all", cwd=ROOT)
        self.assertEqual(0, done.code, done.output)
        self.assertIn("scripts/run_script_tests_test.py\t-\t\n", done.stdout)
        self.assertTrue(os.access(RUNNER, os.X_OK))


class RequiresTest(ScriptTestsCase):
    """`# script-test-requires: <requirements.txt> <module>`: the test runs under the python3 scripts/python-venv names, and
    a package that cannot be had refuses the run instead of the test skipping itself (poster_eval_test passed on a laptop
    without numpy and failed in CI)."""

    def needing_test(self) -> None:
        self.put("scripts/needs_test.py",
                 "# script-test-requires: scripts/needs-requirements.txt somemod\n"
                 "import os, sys\nfirst = os.environ['PATH'].split(os.pathsep)[0]\nsys.exit(0 if first == '/the/venv/bin' else 'first on PATH: ' + first)")
        self.put("scripts/needs-requirements.txt", "somemod==1.0")

    def venv_tool(self, body: str) -> Dict[str, Optional[str]]:
        path = self.write("venv-tool", "#!/bin/sh\n" + body)
        os.chmod(path, 0o755)
        return {"RUN_SCRIPT_TESTS_PYTHON_VENV": path}

    def test_a_test_runs_with_the_venv_the_tool_names_first_on_its_path(self) -> None:
        self.needing_test()
        done = self.runner("scripts/needs_test.py", env=self.venv_tool("echo /the/venv/bin"))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, self.rows(done, "PASS", "scripts/needs_test.py"), done.output)

    def test_a_package_that_cannot_be_had_refuses_the_run_naming_the_module_and_the_install_command(self) -> None:
        self.needing_test()
        done = self.runner("scripts/needs_test.py", env=self.venv_tool("exit 1"))
        self.assertEqual(1, done.code, done.output)
        self.assertIn("scripts/needs_test.py needs the python module somemod", done.output)
        self.assertRegex(done.output, r"install it with `\S+ scripts/needs-requirements\.txt somemod`")
        self.assertEqual(0, self.rows(done, "PASS", "needs_test"), done.output)


class CallersTest(ScriptTestsCase):
    """What the callers read of the runner itself: scripts/hooks/pre-push greps its text for the flags it knows
    (`--floor)`, `--stress)`, `--readers`, `--linux`, `--bash`) before passing them, ToolSelfTestsOffGateSpec its
    roots, and ScriptTests.scala (worker it) and agentctl run it by path and read its table's rows."""

    def test_its_text_names_the_flags_the_hook_greps_for_and_its_roots(self) -> None:
        with open(RUNNER) as f:
            text = f.read()
        for flag in ("--floor)", "--stress)", "--readers", "--linux", "--bash"):
            self.assertIn(flag, text)
        roots = re.search(r'(?m)^(?:Roots=|ROOTS = )"([^"]*)"', text)
        self.assertIsNotNone(roots)
        self.assertIn("ios/scripts", roots.group(1).split() if roots else [])

    def test_a_table_row_is_status_seconds_and_path(self) -> None:
        """agentctl's queuefailure reads `<STATUS> <secs>s  <path>[ (floor)]` rows."""
        done = self.runner("scripts/a-test.sh", "scripts/c-test.sh")
        self.assertRegex(done.stdout, r"(?m)^PASS +\d+s  scripts/a-test\.sh$")
        self.assertRegex(done.stdout, r"(?m)^FAIL +\d+s  scripts/c-test\.sh$")
        self.assertRegex(done.stdout, r"(?m)^── FAIL scripts/c-test\.sh \(exit 1\) ──$")
        self.assertTrue(done.stdout.startswith("\x1b[36m▸\x1b[0m 2 script test(s), 8 at a time\n"), done.stdout)


if __name__ == "__main__":
    unittest.main()
