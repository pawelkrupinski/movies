#!/usr/bin/env python3
"""scripts/js-quality-report, black-box (cli_harness): its own command line -- where the pinned tools live, their
install, which report.js run and which ratchet each mode makes, the usage errors -- against a fake node and npm; then,
as js-quality-report-test.sh did, the coupling and clone-grouping logic (node alone) and the whole report over a small
fixture checkout with the pinned tools: every measure's over-limit row is found (cognitive and cyclomatic complexity,
inner defs, function and file length, a clone group, fan-out, a module cycle, a directory cycle, a directory's
distance), what must not count does not (callbacks as inner defs, two files importing alike as a clone), and the
classic-script globals graph resolves `/* global */` names to their `/* exported */` files. That leg runs where the
tools are installed or npm can install them -- not in run-script-tests' --linux container, which has node but no npm.
Run: python3 scripts/js_quality_report_test.py"""
from __future__ import annotations

import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import unittest
from typing import Dict, List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402
import js_quality_tools  # noqa: E402
import tool_git  # noqa: E402

ROOT = cli_harness.ROOT
TOOLS = os.path.join(ROOT, "scripts", "js-quality")
TOOL = cli_harness.tool_path("scripts/js_quality_report.py", "scripts/js-quality-report")
# What the tool itself runs on, linked into a PATH of its own so a case can leave out node or npm
SANDBOX_TOOLS = "bash sh env cat git cut dirname mktemp rm mkdir cp mv sed python3".split()


def lock_key() -> str:
    """`cat package.json package-lock.json | git hash-object --stdin`, its first 16 hex digits."""
    data = b""
    for name in ("package.json", "package-lock.json"):
        with open(os.path.join(TOOLS, name), "rb") as f:
            data += f.read()
    return hashlib.sha1(b"blob %d\0" % len(data) + data).hexdigest()[:16]


LIMITS = "".join(f"limit\t{kind}\t{value}\n" for kind, value in (
    ("clone-tokens", 50), ("cognitive", 10), ("copies", 0), ("cycle", 0), ("cyclomatic", 10), ("distance", 0.2),
    ("fanout", 9), ("file-lines", 500), ("inner-defs", 2), ("method-lines", 50)))
PAST = "cognitive\tweb/src/main/assets/js/a.js:f\t14\n"

# A fake node: logs its argv, cwd and the two variables report.js reads, and plays report.js's part where asked
FAKE_NODE = r'''
import json, os
with open(os.environ["FAKE_NODE_LOG"], "a") as f:
    f.write(json.dumps({"argv": argv, "cwd": os.getcwd(), "modules": os.environ.get("JS_QUALITY_NODE_MODULES"),
                        "node_path": os.environ.get("NODE_PATH")}) + "\n")
if "--out" in argv:
    out_dir = argv[argv.index("--out") + 1]
    with open(os.path.join(out_dir, "over-limit.tsv"), "w") as f:
        f.write(os.environ.get("FAKE_OVER_LIMIT", ""))
print("report of " + " ".join(argv[1:]))
exit(int(os.environ.get("FAKE_NODE_EXIT", "0")))
'''


def node_argv(run: Dict[str, object]) -> List[str]:
    argv = run["argv"]
    assert isinstance(argv, list)
    return [str(arg) for arg in argv]  # pyright: ignore[reportUnknownVariableType, reportUnknownArgumentType]


class CommandLineTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.cache = self.path("cache")
        self.installed = os.path.join(self.cache, "js-quality", lock_key())
        self.fakes.program("node", FAKE_NODE)

    def install_tools(self) -> None:
        eslint = os.path.join(self.installed, "node_modules", ".bin", "eslint")
        os.makedirs(os.path.dirname(eslint))
        with open(eslint, "w") as f:
            f.write("#!/bin/sh\n")
        os.chmod(eslint, 0o755)

    def tool_env(self, **extra: str) -> Dict[str, str]:
        return {"KINOWO_TOOL_CACHE": self.cache, "FAKE_NODE_LOG": self.path("node.log"), **extra}

    def node_runs(self) -> List[Dict[str, object]]:
        if not os.path.exists(self.path("node.log")):
            return []
        with open(self.path("node.log")) as f:
            return [json.loads(line) for line in f]

    def sandbox_path(self) -> str:
        """A PATH holding only what the tool runs on, without node or npm (the fakes' directory comes first)."""
        sandbox = self.path("sandbox-bin")
        os.makedirs(sandbox)
        for name in SANDBOX_TOOLS:
            found = shutil.which(name)
            if found:
                os.symlink(found, os.path.join(sandbox, name))
        return sandbox

    def test_the_tools_live_outside_the_checkout_under_the_cache_keyed_by_the_lock(self) -> None:
        home = self.path("home")
        done = self.run_tool("--tools-dir", env={"HOME": home, "KINOWO_TOOL_CACHE": None, "XDG_CACHE_HOME": None})
        self.assertEqual((0, f"{home}/.cache/kinowo/js-quality/{lock_key()}\n"), (done.code, done.stdout))
        self.assertFalse(done.stdout.startswith(ROOT + "/"))
        self.assertRegex(lock_key(), r"^[0-9a-f]{16}$")
        self.assertEqual([], self.node_runs(), "--tools-dir runs nothing")

    def test_kinowo_tool_cache_moves_them_and_xdg_cache_home_moves_the_default(self) -> None:
        moved = self.run_tool("--tools-dir", env={"KINOWO_TOOL_CACHE": self.path("c")})
        self.assertEqual(f"{self.path('c')}/js-quality/{lock_key()}\n", moved.stdout)
        xdg = self.run_tool("--tools-dir", env={"KINOWO_TOOL_CACHE": None, "XDG_CACHE_HOME": self.path("x")})
        self.assertEqual(f"{self.path('x')}/kinowo/js-quality/{lock_key()}\n", xdg.stdout)

    def test_the_pre_push_hook_installs_no_js_quality_tools_in_the_tree(self) -> None:
        with open(os.path.join(ROOT, "scripts", "hooks", "pre-push")) as f:
            hook = f.read()
        body = re.search(r"^check_jsquality\(\) \{\n(.*?)^\}", hook, re.S | re.M)
        assert body is not None
        code = [line for line in body.group(1).splitlines() if not line.lstrip().startswith("#")]
        self.assertEqual([], [line for line in code if "npm ci" in line])

    def test_an_unknown_argument_is_refused_with_exit_2(self) -> None:
        done = self.run_tool("--bogus", env=self.tool_env())
        self.assertEqual((2, "js-quality-report: unknown argument --bogus\n"), (done.code, done.stderr))

    def test_help_prints_the_header_without_its_comment_marks(self) -> None:
        done = self.run_tool("--help")
        lines = done.stdout.splitlines()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("Purpose: The JS/TS quality report: complexity, length, clones and coupling of the production "
                         "JS/TS", lines[0])
        self.assertIn("  scripts/js-quality-report --tools-dir         where the pinned tools live (installed or not), "
                      "and nothing else", lines)
        self.assertTrue(lines[-1].endswith("fan-out counts every module used."), lines[-1])
        self.assertEqual([], [line for line in lines if line.startswith("#")])

    def test_the_report_runs_report_js_over_the_checkout_into_a_scratch_directory_it_removes(self) -> None:
        self.install_tools()
        done = self.run_tool(env=self.tool_env(NODE_PATH="/elsewhere"))
        self.assertEqual(0, done.code, done.output)
        [run] = self.node_runs()
        argv = node_argv(run)
        out_dir = argv[4]
        self.assertEqual([os.path.join(TOOLS, "report.js"), "--root", ROOT, "--out", out_dir], argv)
        self.assertEqual(f"report of --root {ROOT} --out {out_dir}\n", done.stdout)
        self.assertEqual(os.path.join(self.installed, "node_modules"), run["modules"])
        self.assertEqual(os.path.join(self.installed, "node_modules") + ":/elsewhere", run["node_path"])
        self.assertFalse(os.path.exists(out_dir), "the scratch --out is removed at the end")

    def test_out_root_and_areas_are_passed_on_in_order_and_the_out_directory_kept(self) -> None:
        self.install_tools()
        os.makedirs(self.path("checkout"))
        os.makedirs(self.path("kept"))
        done = self.run_tool("--root", "checkout", "--area", "app=app", "--out", self.path("kept"), "--areas",
                             "areas.tsv", "--area", "web:globals=web", env=self.tool_env())
        self.assertEqual(0, done.code, done.output)
        [run] = self.node_runs()
        self.assertEqual([os.path.join(TOOLS, "report.js"), "--root", self.path("checkout"), "--out",
                          self.path("kept"), "--area", "app=app", "--areas", "areas.tsv", "--area", "web:globals=web"],
                         node_argv(run))
        self.assertTrue(os.path.isfile(self.path("kept", "over-limit.tsv")))
        self.assertEqual(os.path.join(self.installed, "node_modules"), run["node_path"])

    def test_the_report_exits_as_report_js_does(self) -> None:
        self.install_tools()
        done = self.run_tool(env=self.tool_env(FAKE_NODE_EXIT="3"))
        self.assertEqual(3, done.code, done.output)

    def ratchet_root(self, baseline: str) -> str:
        root = self.path("ratchet-root")
        self.write("ratchet-root/test/resources/js-quality-baseline.tsv", baseline)
        return root

    def test_check_measures_the_ratchet_areas_and_holds_them_to_the_root_s_baseline(self) -> None:
        self.install_tools()
        root = self.ratchet_root(LIMITS)
        clean = self.run_tool("--check", "--root", root, "--area", "ignored=x",
                              env=self.tool_env(FAKE_OVER_LIMIT=LIMITS))
        self.assertEqual(0, clean.code, clean.output)
        [run] = self.node_runs()
        argv = node_argv(run)
        self.assertEqual([os.path.join(TOOLS, "report.js"), "--root", root, "--out", argv[4], "--areas",
                          os.path.join(TOOLS, "ratchet-areas.tsv")], argv)
        self.assertNotIn("report of", clean.stdout, "report.js's own output is not shown in a ratchet run")
        self.assertFalse(os.path.exists(argv[4]))
        past = self.run_tool("--check", "--root", root, env=self.tool_env(
            FAKE_OVER_LIMIT=LIMITS + PAST))
        self.assertEqual(1, past.code, past.output)
        self.assertIn("web/src/main/assets/js/a.js:f", past.output)

    def test_check_passes_every_range_to_the_ratchet(self) -> None:
        self.install_tools()
        root = self.ratchet_root(LIMITS)
        self.git(root, "init", "-q")
        self.git(root, "add", "-A")
        self.git(root, "commit", "-qm", "the baseline")
        self.write("ratchet-root/test/resources/js-quality-baseline.tsv", LIMITS + PAST)
        self.git(root, "commit", "-qam", "a rise without its trailer")
        within = self.run_tool("--check", "--root", root, env=self.tool_env(FAKE_OVER_LIMIT=LIMITS + PAST))
        self.assertEqual(0, within.code, within.output)
        done = self.run_tool("--check", "--root", root, "--range", "HEAD~1..HEAD", "--range", "HEAD..HEAD",
                             env=self.tool_env(FAKE_OVER_LIMIT=LIMITS + PAST))
        self.assertEqual(1, done.code, done.output)
        self.assertIn("BASELINE GREW in HEAD~1..HEAD (cognitive, cyclomatic, inner-defs) with no `Complexity: <why>`",
                      done.output)

    def test_update_rewrites_the_root_s_baseline_from_the_measure(self) -> None:
        self.install_tools()
        root = self.ratchet_root(LIMITS + PAST)
        done = self.run_tool("--update", "--root", root, env=self.tool_env(FAKE_OVER_LIMIT=LIMITS))
        self.assertEqual(0, done.code, done.output)
        self.assertNotIn("a.js:f", self.read("ratchet-root/test/resources/js-quality-baseline.tsv"))

    def fake_npm(self, body: str) -> None:
        self.fakes.program("npm", "import os, json\n"
                           "open(os.environ['FAKE_NPM_LOG'], 'w').write(json.dumps("
                           "{'cwd': os.getcwd(), 'files': sorted(os.listdir('.'))}))\n" + body)

    def test_missing_tools_are_installed_with_npm_ci_aside_and_renamed_into_place(self) -> None:
        self.fake_npm("os.makedirs('node_modules/.bin'); open('node_modules/.bin/eslint', 'w').close()\n"
                      "os.chmod('node_modules/.bin/eslint', 0o755)\n")
        done = self.run_tool(env=self.tool_env(FAKE_NPM_LOG=self.path("npm.log")))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["ci", "--no-audit", "--no-fund", "--silent"]], self.fakes.calls("npm"))
        self.assertIn(f"js-quality-report: installing the tools (npm ci in {self.installed})\n", done.stderr)
        with open(self.path("npm.log")) as f:
            npm = json.load(f)
        self.assertEqual(["package-lock.json", "package.json"], npm["files"])
        self.assertTrue(os.path.basename(npm["cwd"]).startswith(lock_key() + ".build."), npm["cwd"])
        self.assertTrue(os.access(os.path.join(self.installed, "node_modules", ".bin", "eslint"), os.X_OK))
        self.assertEqual([lock_key()], os.listdir(os.path.dirname(self.installed)), "no build directory left")
        self.assertEqual(1, len(self.node_runs()))

    def test_a_failed_npm_ci_exits_2_and_leaves_no_build_directory(self) -> None:
        self.fake_npm("exit(1)\n")
        done = self.run_tool(env=self.tool_env(FAKE_NPM_LOG=self.path("npm.log")))
        self.assertEqual(2, done.code, done.output)
        self.assertEqual([], os.listdir(os.path.dirname(self.installed)))
        self.assertEqual([], self.node_runs())

    def test_an_install_without_eslint_exits_2_saying_so(self) -> None:
        self.fake_npm("")
        done = self.run_tool(env=self.tool_env(FAKE_NPM_LOG=self.path("npm.log")))
        self.assertEqual(2, done.code, done.output)
        self.assertIn(f"js-quality-report: the tools did not install in {self.installed}\n", done.stderr)
        self.assertEqual([], self.node_runs())

    def test_without_node_the_report_exits_2(self) -> None:
        os.remove(os.path.join(self.fakes.bin, "node"))
        done = self.run_tool(env={**self.tool_env(), "PATH": self.sandbox_path()})
        self.assertEqual((2, "js-quality-report: node is not installed\n"), (done.code, done.stderr))

    def test_without_npm_missing_tools_exit_2_and_installed_ones_need_none(self) -> None:
        done = self.run_tool(env={**self.tool_env(), "PATH": self.sandbox_path()})
        self.assertEqual((2, "js-quality-report: npm is not installed (needed once, to install the tools)\n"),
                         (done.code, done.stderr))
        self.install_tools()
        installed = self.run_tool(env={**self.tool_env(), "PATH": self.path("sandbox-bin")})
        self.assertEqual(0, installed.code, installed.output)


NODE_LOGIC = r'''
const { cycles, globalEdges, measureCoupling } = require("./coupling");
const { groupPairs } = require("./clones");
const edges = new Map([["a", new Set(["b"])], ["b", new Set(["a"])], ["c", new Set(["a"])]]);
console.log("cycles " + JSON.stringify(cycles(["a", "b", "c"], edges)));
const texts = new Map([
  ["s/x.js", "/* global bar */\n/* exported foo */\nfunction foo() {}\n"],
  ["s/y.js", "/* global foo, PAGE_CONST */  // x.js\n/* exported bar,\n   baz */\n"],
  ["p/t.js", "/* global baz, foo:writable */\n"],
]);
const g = globalEdges([...texts.keys()], [...texts.keys()], texts);
console.log("globals " + [...g.edges].map(([f, s]) => f + ">" + [...s].join("+")).join(" ") + " templates " + g.templateNames.get("s/y.js"));
const m = measureCoupling(g.edges, texts, g.templateNames);
console.log("module cycles " + JSON.stringify(m.moduleCycles) + " dirs " + m.packages.map((p) => p.dir + ":" + p.ca + "/" + p.ce).join(" "));
const pairs = [{ tokens: 60, copies: [{ file: "a", start: 1, end: 9 }, { file: "b", start: 3, end: 11 }] },
               { tokens: 70, copies: [{ file: "b", start: 3, end: 11 }, { file: "c", start: 5, end: 13 }] }];
console.log("groups " + JSON.stringify(groupPairs(pairs).map((group) => [group.tokens, group.copies.map((copy) => copy.file).join("")])));
'''


@unittest.skipIf(shutil.which("node") is None, "node is not installed")
class CouplingLogicTest(unittest.TestCase):
    """report.js's coupling and clone-grouping modules, node alone."""

    def test_cycles_globals_directories_and_clone_groups(self) -> None:
        done = subprocess.run(["node", "-e", NODE_LOGIC], cwd=TOOLS, capture_output=True, text=True, timeout=120)
        self.assertEqual(0, done.returncode, done.stderr)
        lines = done.stdout.splitlines()
        self.assertEqual([
            'cycles [["a","b"]]',  # a cycle is its strongly connected component, a node outside it none
            # /* global */ names resolve to the /* exported */ (or top-level function) file; others are template globals
            "globals s/x.js>s/y.js s/y.js>s/x.js p/t.js>s/y.js+s/x.js templates 1",
            'module cycles [["s/x.js","s/y.js"]] dirs s:1/0 p:0/2',  # ...and form a module cycle; each directory's Ca/Ce
            'groups [[70,"abc"]]',  # pairs sharing a copy are one clone group, at its largest token count
        ], lines)


CLONED = """export function render(items) {
  const rows = [];
  for (const item of items) {
    const cell = { id: item.id, name: item.name, value: item.value * 2, label: `${item.name}: ${item.value}` };
    rows.push(cell);
  }
  return rows.filter((row) => row.value > 10).map((row) => row.label).join(", ");
}"""


def branchy(name: str) -> str:
    """A function of six `if`s each holding one: cognitive 6 x (1 + 2) = 18, cyclomatic 13."""
    body = "".join(f"  if (a > {i}) {{ if (b > {i}) {{ a += 1; }} }}\n" for i in range(1, 7))
    return f"export function {name}(a, b) {{\n{body}  return a;\n}}\n"


def fixture_files() -> Dict[str, str]:
    imports = "".join(f"import {{ name{i}, other{i} }} from './module-{i}.js';\n" for i in range(1, 13))
    core = ("import { helper } from '../feature/f.js';\n" + branchy("complicated")
            + "export function holder(list) {\n  function one() { return 1; }\n  const two = () => 2;\n"
            + "  const three = function () { return 3; };\n"
            + "  list.forEach((x) => x); list.map(function (y) { return y; });\n"
            + "  return one() + two() + three() + helper();\n}\n"
            + "export function long() {\n" + "".join(f"  let v{i} = {i};\n" for i in range(1, 56)) + "}\n")
    twins = "".join(f"export function {outer}() {{\n  function twin() {{\n"
                    + "".join(f"    // {outer} {i}\n" for i in range(1, 56)) + "  }\n  return twin;\n}\n"
                    for outer in ("one", "two"))
    files = {
        "app/core/c.js": core,
        "app/feature/f.js": "import { complicated } from '../core/c.js';\n"
                            "export function helper() { return complicated(1, 2); }\n",
        "app/core/big.js": "".join(f"// line {i}\n" for i in range(1, 601)),
        "app/core/clone-a.js": imports + CLONED + "\n",
        "app/feature/imports-only-a.js": imports + "export const x = 1;\n",
        "app/feature/imports-only-b.js": imports + "export const y = 2;\n",
        "app/feature/clone-b.js": CLONED + "\n",
        "app/types/shape.ts": "export interface Shape { side: number }\nexport type Id = string;\n",
        "hub/hub.mjs": "".join(f"import {{ leaf{i} }} from './leaf{i}.mjs';\n" for i in range(1, 11))
                       + "export const all = [leaf1];\n",
        "app/feature/twins.js": twins,
        "web/shared/a.js": "/* global second */  // b.js\n/* exported first */\nfunction first() { return second(); }\n",
        "web/shared/b.js": "/* global first, FROM_TEMPLATE */\n/* exported second */\n"
                           "function second() { return first() + FROM_TEMPLATE; }\n",
    }
    files.update({f"hub/leaf{i}.mjs": f"export const leaf{i} = {i};\n" for i in range(1, 11)})
    return files


@unittest.skipUnless(js_quality_tools.ready(TOOL, tool_git.shared_runner(), os.environ),
                 "end-to-end leg not run: the tools are not installed and npm cannot install them here")
class EndToEndTest(cli_harness.ToolCase):
    """The whole report over a fixture checkout, with the pinned tools (installed in the real cache, as callers do)."""
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        for relative, text in fixture_files().items():
            self.write("checkout/" + relative, text)

    def row(self, kind: str) -> str:
        rows = [line.split("\t")[1:3] for line in self.read("out/over-limit.tsv").splitlines()
                if line.startswith(kind + "\t")]
        return "".join(f"{key}\t{value};" for key, value in sorted(rows))

    def test_every_measure_s_over_limit_row(self) -> None:
        node_modules = os.path.join(TOOLS, "node_modules")
        before = os.path.exists(node_modules)
        done = self.run_tool("--root", self.path("checkout"), "--out", self.path("out"), "--area", "app=app",
                             "--area", "web:globals=web", "--area", "hub=hub")
        self.assertEqual(0, done.code, done.output)  # the report runs and prints its summary table
        self.assertEqual(1, len([line for line in done.output.splitlines() if line.startswith("| app (imports) |")]))
        self.assertEqual("app/core/c.js:complicated\t18;", self.row("cognitive"))
        self.assertEqual("app/core/c.js:complicated\t13;", self.row("cyclomatic"))
        self.assertEqual("app/core/c.js:holder\t3;", self.row("inner-defs"))  # the three named ones, not the callbacks
        # a function over 50 lines, keyed by file and name (no line: an edit above never re-keys it), a later
        # same-named one #2
        self.assertEqual("app/core/c.js:long\t57;app/feature/twins.js:twin\t57;app/feature/twins.js:twin#2\t57;",
                         re.sub(r"app.feature.twins.js:(one|two)\t[0-9]+;", "", self.row("method-lines")))
        self.assertEqual("clone-tokens\t50;cognitive\t10;copies\t0;cycle\t0;cyclomatic\t10;distance\t0.2;fanout\t9;"
                         "file-lines\t500;inner-defs\t2;method-lines\t50;", self.row("limit"))  # each limit a row
        self.assertEqual("app/core/big.js\t600;", self.row("file-lines"))
        # the one clone group (its files joined by |, its copies), the files importing alike none
        self.assertEqual("app/core/clone-a.js|app/feature/clone-b.js\t2;", self.row("copies"))
        # fan-out is the general design rules' (collaborators only, scripts/design-rules): no row here, its limit only
        self.assertEqual("", self.row("fanout"))
        # the module cycles (imports and globals) and the directory cycle
        self.assertEqual("app/core/c.js|app/feature/f.js\t2;app/core|app/feature\t2;web/shared/a.js|web/shared/b.js\t2;",
                         self.row("cycle"))
        coupling = self.read("out/coupling.tsv").splitlines()
        # the globals graph counts a template-only name apart
        self.assertEqual(["module\tweb\tweb/shared/b.js\tfan-out=1 template-globals=1"],
                         [line for line in coupling if "web/shared/b.js" in line])
        # a type-only TypeScript module is abstract (A = 1 in its directory)
        [types] = [line for line in coupling if "directory\tapp\tapp/types" in line]
        self.assertEqual(["A=1.00"], re.findall(r"A=[0-9.]*", types))
        # ...and the run made no node_modules in the checkout (one an old install left is neither made nor removed)
        self.assertEqual(before, os.path.exists(node_modules))

    def test_an_area_of_a_bad_shape_is_refused(self) -> None:
        done = self.run_tool("--root", self.path("checkout"), "--area", "bad shape")
        self.assertEqual(2, done.code, done.output)


if __name__ == "__main__":
    unittest.main()
