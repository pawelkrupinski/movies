#!/usr/bin/env python3
# script-test-width: 3 (node-heavy: its JS extractor / report.js bursts of short-lived node processes; run-script-tests counts the extra cores)
"""The general rule core (scripts/design-rules/): every rule over every language's modules -- naming (suffix,
abbreviation, idioms), data or collaborator (effects, state, interfaces, the data guard, the closure), collaborator-only
fan-out, distance with V2 abstractness, layers, the design lint (adapters, state, import-time IO, the main guard), the
exemption lint -- each through its extractor (Python's `ast`, JS/TS's facts.js, Scala's text), and the one rules table
every language's constants agree with, and the `mixed` rule (a function performing an effect and deciding) over each
extractor's functions. DESIGN_RULES_UNDER_TEST names another copy of the core (the mutation test's)."""
from __future__ import annotations

import ast
import os
import re
import subprocess
import sys
import tempfile
import unittest
from typing import Dict, List, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
CORE = os.environ.get("DESIGN_RULES_UNDER_TEST") or HERE
SCRIPTS = os.path.dirname(CORE)  # the core's own siblings: a mutated copy's lib and js-quality are read
sys.path[:0] = [CORE, os.path.join(SCRIPTS, "lib"), os.path.join(SCRIPTS, "ci"), os.path.join(SCRIPTS, "py-quality")]
import collaborators  # noqa: E402
import design_lint  # noqa: E402
import exemptions  # noqa: E402
import extractors  # noqa: E402
from code_tools import js_quality_tools  # noqa: E402
import tool_git  # noqa: E402
import naming  # noqa: E402
import packages  # noqa: E402
import python_facts  # noqa: E402
import rules_table  # noqa: E402
import scala_names  # noqa: E402
import splits  # noqa: E402
from model import DataType, Module, Name  # noqa: E402

RULES = rules_table.load(ROOT)
EXTRA = rules_table.parse("\n".join([
    "limit\tfanout\t2\tpy", "limit\tdistance\t0.2\tpy", "limit\tdistance-min-modules\t2\tpy",
    "limit\tdata-methods\t4\tpy", "limit\tdata-decisions\t9\tpy", "limit\tdatabag\t9\tpy", "limit\tdatafanout\t2\tpy",
    "limit\tmixed-cyclomatic\t3\tscala py js", "adapter\tpy\tlib/clock.py\tclock", "adapter\tpy\tlib/git_repo.py\tprocess", "adapter\tpy\tlib/io.py\tfiles storage network", "distance\tpy\tlib",
    "layer\tpy\tlib\t0\tthe library", "layer\tpy\ttools\t1\tthe tools"]))


def py(path: str, source: str, imports: Tuple[str, ...] = ()) -> Module:
    return python_facts.module(path, ast.parse(source), frozenset(imports))


def kinds(rows: List[Tuple[str, str, float]]) -> Dict[Tuple[str, str], float]:
    return {(k, key): v for k, key, v in rows}


PURE = "import dataclasses\n@dataclasses.dataclass(frozen=True)\nclass Point:\n    x: int\n\ndef twice(n):\n    return 2 * n\n"
RUNNER = "import subprocess\n\ndef run(argv):\n    return subprocess.run(argv)\n"


class NamingTest(unittest.TestCase):
    def test_a_forbidden_suffix_on_a_type_or_module_is_a_row(self) -> None:
        rows = kinds(naming.rows([py("tools/cache_utils.py", "class CacheManager:\n    pass\n")], RULES))
        self.assertIn(("suffix", "tools/cache_utils.py:CacheManager"), rows)
        self.assertIn(("suffix", "tools/cache_utils.py:cache_utils"), rows)

    def test_an_abbreviation_is_a_row_and_an_idiom_is_not(self) -> None:
        rows = kinds(naming.rows([py("tools/x.py", "def read_cfg():\n    pass\n\ndef parse_args():\n    pass\n")], RULES))
        self.assertIn(("abbreviation", "tools/x.py:read_cfg"), rows)
        self.assertNotIn(("abbreviation", "tools/x.py:parse_args"), rows)

    def test_words_split_every_case(self) -> None:
        self.assertEqual(("http", "error", "handler"), naming.words("HTTPErrorHandler"))
        self.assertEqual(("prune", "sweep"), naming.words("prune_sweep"))
        self.assertEqual(("day", "swipe"), naming.words("day-swipe"))

    def test_permuted_names_of_two_files_of_one_package_clash_and_of_one_file_do_not(self) -> None:
        clash = naming.permutations([py("tools/prune_sweep.py", ""), py("tools/sweep_prune.py", "")])
        self.assertEqual({("py", "tools"): {"prune_sweep": "tools/prune_sweep.py", "sweep_prune": "tools/sweep_prune.py"}}, clash)
        self.assertEqual({}, naming.permutations([py("tools/build_image.py", "class ImageBuild:\n    pass\n")]))

    def test_a_scala_sources_types_and_members_are_names(self) -> None:
        m = scala_names.module("worker/src/main/scala/a/A.scala", "package a\n\nfinal class FooImpl {\n  def readCfg(): Int = 1\n}\n")
        rows = kinds(naming.rows([m], RULES))
        self.assertIn(("suffix", "worker/src/main/scala/a/A.scala:FooImpl"), rows)
        self.assertIn(("abbreviation", "worker/src/main/scala/a/A.scala:readCfg"), rows)


class CollaboratorTest(unittest.TestCase):
    def test_effects_state_interfaces_and_logic_in_data_make_a_collaborator_and_pure_data_does_not(self) -> None:
        logic = "import dataclasses\n@dataclasses.dataclass\nclass Bag:\n" + "".join(
            f"    def m{n}(self):\n        if self:\n            return 1\n        return 2\n" for n in range(5))
        modules = {m.path: m for m in [
            py("a/pure.py", PURE), py("a/runner.py", RUNNER), py("a/state.py", "SEEN = []\n\ndef see(x):\n    SEEN.append(x)\n"),
            py("a/port.py", "from typing import Protocol\nclass Port(Protocol):\n    def go(self) -> None: ...\n"),
            py("a/bag.py", logic), py("a/user.py", "import runner\n", ("a/runner.py",))]}
        found = collaborators.collaborators(modules, RULES)
        self.assertEqual({"a/runner.py", "a/state.py", "a/port.py", "a/bag.py", "a/user.py"}, found)

    def test_a_reference_alone_is_no_effect_but_an_environment_read_is(self) -> None:
        self.assertEqual({}, py("a.py", "import subprocess\ntry:\n    pass\nexcept subprocess.CalledProcessError:\n    pass\n").effects)
        self.assertEqual({"env": 1}, py("a.py", "import os\nHOME = os.environ\n").effects)
        self.assertEqual({"clock": 1}, py("a.py", "from datetime import datetime\ndef now():\n    return datetime.now()\n").effects)

    def test_fan_out_counts_collaborators_only(self) -> None:
        data = [py(f"a/d{n}.py", PURE) for n in range(4)]
        users = [py(f"a/c{n}.py", RUNNER) for n in range(3)]
        wide_data = py("a/wide_data.py", "", tuple(m.path for m in data))
        wide = py("a/wide.py", "", tuple(m.path for m in users))
        modules = {m.path: m for m in [*data, *users, wide_data, wide]}
        found = collaborators.collaborators(modules, EXTRA)
        self.assertEqual(0, collaborators.fanout(wide_data, found))
        self.assertEqual(3, collaborators.fanout(wide, found))
        self.assertIn(("datafanout", "a/wide_data.py"), kinds(collaborators.rows(modules, EXTRA)))

    def test_a_data_type_naming_many_types_is_a_databag_row(self) -> None:
        fields = "".join(f"    f{n}: T{n}\n" for n in range(10))
        bag = py("a/bag.py", f"import dataclasses\n@dataclasses.dataclass\nclass Wide:\n{fields}")
        self.assertIn(("databag", "a/bag.py:Wide"), kinds(collaborators.rows({bag.path: bag}, EXTRA)))


class PackagesTest(unittest.TestCase):
    def test_distance_counts_collaborators_only_in_its_abstractness(self) -> None:
        port = py("lib/port.py", "from typing import Protocol\nclass Port(Protocol):\n    def go(self) -> None: ...\n")
        modules = {m.path: m for m in [port, py("lib/data.py", PURE), py("tools/t.py", "", ("lib/port.py", "lib/data.py"))]}
        found = collaborators.collaborators(modules, EXTRA)
        # lib: I = 0 (used, uses nothing); A = 1 abstract / 1 collaborator (the data module dilutes nothing): D = 0
        self.assertEqual(0.0, packages.distance("lib", modules, found))

    def test_an_upward_edge_and_an_unplaced_directory_are_rows(self) -> None:
        modules = {m.path: m for m in [py("lib/a.py", "", ("tools/t.py",)), py("tools/t.py", ""), py("new/n.py", "")]}
        rows = kinds(packages.rows("py", modules, set(), EXTRA))
        self.assertIn(("layer", "lib->tools"), rows)
        self.assertIn(("unplaced", "new"), rows)
        self.assertIn(("dircycle", "lib|tools"), kinds(packages.rows("py", {**modules, "tools/t.py": py("tools/t.py", "", ("lib/a.py",))}, set(), EXTRA)))


class DesignLintTest(unittest.TestCase):
    def test_a_direct_clock_outside_its_adapter_is_a_row_and_inside_it_is_not(self) -> None:
        source = "import time\n\ndef stamp():\n    return time.time() + time.time()\n"
        rows = kinds(design_lint.rows([py("tools/t.py", source), py("lib/clock.py", source)], EXTRA))
        self.assertEqual({("effect", "tools/t.py"): 1}, rows)  # one effect touched, however often

    def test_files_storage_and_network_outside_their_adapters_are_a_row_each_counted_by_use(self) -> None:
        source = ("import sqlite3\nimport time\nimport urllib.request\n\ndef keep(path, url):\n"
                  "    with open(path) as f:\n        f.read()\n    open(path + '.bak', 'w').close()\n"
                  "    sqlite3.connect(path)\n    return urllib.request.urlopen(url, timeout=time.time())\n")
        rows = kinds(design_lint.rows([py("tools/io.py", source), py("lib/io.py", source)], EXTRA))
        self.assertEqual({("effect", "tools/io.py"): 1, ("effect", "tools/io.py:files"): 2,
                          ("effect", "tools/io.py:storage"): 1, ("effect", "tools/io.py:network"): 1,
                          ("effect", "lib/io.py"): 1}, rows)  # the adapter's own clock is still a row: not its effect

    def test_module_state_written_by_a_function_is_a_row_and_a_constant_or_a_local_is_not(self) -> None:
        source = ("CACHE = {}\nNAMES = ('a',)\nCOUNT = 0\n\ndef put(k):\n    CACHE[k] = 1\n\ndef bump():\n    global COUNT\n"
                  "    COUNT += 1\n\nTOTAL = []\n\ndef local():\n    TOTAL = []\n    TOTAL.append(1)\n")
        self.assertEqual(("CACHE", "COUNT"), py("t.py", source).state)
        self.assertEqual(("t.py",), tuple(key for (k, key) in kinds(design_lint.rows([py("t.py", source)], RULES)) if k == "state"))

    def test_io_at_import_is_a_row_and_the_main_guard_is_the_root(self) -> None:
        at_import = py("t.py", "import os\nHOME = os.environ['HOME']\n")
        rooted = py("t.py", "import os\n\ndef main():\n    return os.environ\n\nif __name__ == '__main__':\n    print(os.environ)\n")
        self.assertEqual(1, at_import.import_io)
        self.assertEqual(0, rooted.import_io)


MIXED = """import subprocess
import git_repo


def decide_and_run(paths):
    for p in paths:
        if p.endswith(".py"):
            subprocess.run(["x", p])


def decide(paths, repo):
    for p in paths:
        if p and repo.exists(p):
            return p


def run_once(argv):
    return subprocess.run(argv) or None


def through_the_adapter(rev):
    if rev:
        return git_repo.Repo.here().show(rev) or ""


def outer(paths):
    def inner(p):
        return subprocess.run(p)
    return [inner(p) for p in paths if p]


def main(argv):
    if argv and argv[0] == "-h":
        return subprocess.run(argv)


class Tool:
    def check(self, argv):
        if argv:
            return subprocess.run(argv) if argv[0] else None
"""


class MixedTest(unittest.TestCase):
    def test_a_function_both_performing_an_effect_and_deciding_is_a_row_valued_its_cyclomatic(self) -> None:
        rows = kinds(design_lint.mixed_rows(py("tools/t.py", MIXED), EXTRA))
        self.assertEqual({("mixed", "tools/t.py:decide_and_run"): 3, ("mixed", "tools/t.py:through_the_adapter"): 3,
                          ("mixed", "tools/t.py:Tool.check"): 3}, rows)

    def test_a_pure_decision_over_an_injected_adapter_an_effect_alone_and_the_composition_root_are_no_row(self) -> None:
        rows = kinds(design_lint.mixed_rows(py("tools/t.py", MIXED), EXTRA))
        for name in ("decide", "run_once", "outer", "outer.inner", "main"):
            self.assertNotIn(("mixed", f"tools/t.py:{name}"), rows)

    def test_an_adapter_modules_own_functions_are_the_leaves(self) -> None:
        self.assertEqual([], design_lint.mixed_rows(py("lib/git_repo.py", MIXED), EXTRA))
        self.assertIn(("mixed", "tools/t.py:decide_and_run"), kinds(design_lint.rows([py("tools/t.py", MIXED)], EXTRA)))

    def test_a_functions_facts_are_its_own_body_and_count_each_boolean_operator(self) -> None:
        facts = {f.name: f for f in py("tools/t.py", MIXED).functions}
        self.assertEqual(({}, 3), (facts["outer"].effects, facts["outer"].cyclomatic))
        self.assertEqual({"process": 1}, facts["outer.inner"].effects)
        self.assertEqual(4, facts["decide"].cyclomatic)
        self.assertEqual(frozenset({"git_repo.Repo.here"}), facts["through_the_adapter"].calls)

    def test_a_scala_defs_own_lines_with_comments_and_strings_blanked(self) -> None:
        source = ("package a\n\nfinal class Store(clock: Clock) {\n  def save(\n    p: Path\n  ): Unit = {\n"
                  "    if (p.isAbsolute && clock.ok) Files.writeString(p, \"if || if\")\n"
                  "    def nested(n: Int) = if (n > 0 || n < 9) Files.size(p) else 0\n  }\n\n"
                  "  // if if Files.readString(p) if\n  def pure(n: Int): Int = if (n > 0 && n < 9) 1 else 0\n}\n")
        m = scala_names.module("common/src/main/scala/a/Store.scala", source)
        facts = {f.name: f for f in m.functions}
        self.assertEqual(({"files": 1}, 3), (facts["Store.save"].effects, facts["Store.save"].cyclomatic))
        self.assertEqual(({"files": 1}, 3), (facts["Store.save.nested"].effects, facts["Store.save.nested"].cyclomatic))
        self.assertEqual(({}, 3), (facts["Store.pure"].effects, facts["Store.pure"].cyclomatic))
        self.assertEqual({("mixed", "common/src/main/scala/a/Store.scala:Store.save"): 3,
                          ("mixed", "common/src/main/scala/a/Store.scala:Store.save.nested"): 3},
                         kinds(design_lint.mixed_rows(m, EXTRA)))


CORE = "import subprocess\n\ndef run_git(a):\n    return subprocess.run(a)\n\ndef keep(a):\n    return subprocess.run(a)\n"
GITREPO = "import subprocess\n\ndef run_git(a):\n    return subprocess.run(a)\n"
NET = "import urllib.request\n\ndef get(u):\n    return urllib.request.urlopen(u)\n"


class SplitTest(unittest.TestCase):
    """The split credit (splits.py): `split of lib/core.py into lib/gitrepo.py`, an importer's use of run_git moved."""

    def sides(self, head_user: str, head_imports: Tuple[str, ...], extra: str = ""):
        base = {m.path: m for m in [py("lib/core.py", CORE), py("lib/net.py", NET),
                                    py("tools/t.py", "import core\ncore.run_git(1)\ncore.keep(2)\n", ("lib/core.py",))]}
        head = {m.path: m for m in [py("lib/core.py", "import subprocess\n\ndef keep(a):\n    return subprocess.run(a)\n"),
                                    py("lib/gitrepo.py", GITREPO + extra), py("lib/net.py", NET),
                                    py("tools/t.py", head_user, head_imports)]}
        found = (collaborators.collaborators(base, EXTRA), collaborators.collaborators(head, EXTRA))
        fanouts = tuple({p: collaborators.fanout(m, c) for p, m in side.items()} for side, c in zip((base, head), found))
        return (base, head), found, fanouts

    def refusals(self, head_user: str, head_imports: Tuple[str, ...], extra: str = "",
                 grown=(("fanout", "tools/t.py", None, 2),)) -> List[str]:
        sides, found, fanouts = self.sides(head_user, head_imports, extra)
        return splits.refusals("split of lib/core.py into gitrepo", list(grown), sides, found, fanouts)

    def test_an_importer_using_the_moved_code_is_credited(self) -> None:
        user = "import core\nimport gitrepo\ngitrepo.run_git(1)\ncore.keep(2)\n"
        self.assertEqual([], self.refusals(user, ("lib/core.py", "lib/gitrepo.py")))

    def test_an_importer_gaining_an_unrelated_collaborator_is_refused(self) -> None:
        user = "import core\nimport gitrepo\nimport net\ngitrepo.run_git(1)\ncore.keep(2)\nnet.get(3)\n"
        found = self.refusals(user, ("lib/core.py", "lib/gitrepo.py", "lib/net.py"))
        self.assertIn("tools/t.py gained lib/net.py, no piece of lib/core.py", found)
        self.assertTrue(any("more than the 1 piece" in r for r in found), found)

    def test_a_name_read_from_the_piece_that_x_never_held_is_refused(self) -> None:
        user = "import core\nimport gitrepo\ngitrepo.fresh(1)\ncore.keep(2)\n"
        found = self.refusals(user, ("lib/core.py", "lib/gitrepo.py"), "\ndef fresh(a):\n    return a\n")
        self.assertEqual(["tools/t.py reads fresh from lib/gitrepo.py: not code it read from lib/core.py at the base"], found)

    def test_another_kinds_rise_and_a_malformed_split_are_refused(self) -> None:
        user = "import core\nimport gitrepo\ngitrepo.run_git(1)\ncore.keep(2)\n"
        found = self.refusals(user, ("lib/core.py", "lib/gitrepo.py"), grown=(("distance", "lib", 0.1, 0.3),))
        self.assertEqual(["distance lib: a split excuses importers' fan-out only"], found)
        sides, collab, fanouts = self.sides(user, ("lib/core.py", "lib/gitrepo.py"))
        self.assertEqual(1, len(splits.refusals("split of nothing.py into gitrepo", [], sides, collab, fanouts)))
        self.assertEqual(("x", ["a", "b"]), splits.parse("split of x into a, b"))
        self.assertIsNone(splits.parse("a reason"))


class ExemptionsTest(unittest.TestCase):
    def test_a_generic_or_undated_why_is_refused_and_a_specific_one_passes(self) -> None:
        bad = exemptions.parse("effect\ttools/t.py\tyesterday\tneeded for now\n")
        problems = exemptions.row_problems(bad)
        self.assertTrue(any("no YYYY-MM-DD date" in p for p in problems), problems)
        self.assertTrue(any("`for now` is generic" in p for p in problems), problems)
        good = exemptions.parse("effect\ttools/t.py\t2026-11-26\tprocess adapter: runs the one git binary for every"
                                " reader, which receive its runner\n")
        self.assertEqual([], exemptions.row_problems(good))

    def test_another_repositorys_table_path_is_read(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            os.makedirs(os.path.join(root, "rules"))
            with open(os.path.join(root, "rules", "own-exemptions.tsv"), "w") as f:
                f.write("effect\ttools/t.py\t2026-11-26\tprocess adapter: runs the one git binary for every reader\n")
            self.assertEqual({("effect", "tools/t.py")}, exemptions.keys(root, "rules/own-exemptions.tsv"))
            self.assertEqual({}, exemptions.read(root))
            self.assertEqual([], exemptions.problems(root, [], {}, "rules/own-exemptions.tsv"))

    def test_the_rules_of_another_repositorys_table_are_loaded_and_its_name_is_in_the_errors(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            with open(os.path.join(root, "own-rules.tsv"), "w") as f:
                f.write("limit\tfanout\t7\tscala py js\n")
            self.assertEqual(7, rules_table.load(root, "own-rules.tsv").limit("fanout"))
            with open(os.path.join(root, "bad.tsv"), "w") as f:
                f.write("limit\n")
            with self.assertRaisesRegex(ValueError, "bad.tsv:1"):
                rules_table.load(root, "bad.tsv")


@unittest.skipUnless(js_quality_tools.ready(os.path.join(SCRIPTS, "js-quality-report"), tool_git.shared_runner(), os.environ),
                     "the JS extractor's leg not run: node, and its tools or npm to install them, are not here")
class JsFactsTest(unittest.TestCase):
    def test_the_js_extractor_reads_effects_state_import_io_and_names(self) -> None:
        source = ("let count = 0;\nconst seen = [];\nconst LIMIT = 3;\nconst started = Date.now();\n"
                  "function readCfg() { count += 1; seen.push(new Date()); return LIMIT; }\n"
                  "function init() { const page = { n: 0 }; page.n = 1; return process.env.HOME; }\n"
                  "function load() { localStorage.getItem('k'); return fetch('/api'); }\n")
        with tempfile.TemporaryDirectory() as root:
            os.makedirs(os.path.join(root, "src"))
            with open(os.path.join(root, "src", "page.js"), "w") as f:
                f.write(source)
            out = os.path.join(root, "out")
            subprocess.run([os.path.join(SCRIPTS, "js-quality-report"), "--root", root, "--area", "app=src",
                            "--out", out], check=True, stdout=subprocess.DEVNULL)
            m = extractors.read_js(os.path.join(out, "modules.json"), set())["src/page.js"]
        self.assertEqual(("count", "seen"), m.state)
        self.assertEqual(1, m.import_io)
        self.assertEqual({"clock": 2, "env": 1, "network": 1, "storage": 1}, m.effects)
        self.assertEqual({("effect", "src/page.js"): 2, ("effect", "src/page.js:network"): 1,
                          ("effect", "src/page.js:storage"): 1, ("import-io", "src/page.js"): 1,
                          ("state", "src/page.js"): 2}, kinds(design_lint.rows([m], RULES)))
        self.assertIn(("abbreviation", "src/page.js:readCfg"), kinds(naming.rows([m], RULES)))

    def test_the_js_extractor_reads_each_functions_effects_adapter_calls_and_decisions(self) -> None:
        sources = {"src/exec.ts": "export function run(argv: string[]): string { return argv.join(' '); }\n",
                   "src/tool.ts": ("import { run } from './exec';\nimport fs from 'node:fs';\n"
                                   "export function decide(xs: string[], repo: { has(x: string): boolean }) {\n"
                                   "  for (const x of xs) { if (repo.has(x)) return x; }\n  return null;\n}\n"
                                   "export function mixed(p: string) {\n  if (p && fs.existsSync(p)) return 1;\n  return 0;\n}\n"
                                   "export function viaAdapter(xs: string[]) {\n  return xs.length > 1 && xs[0] ? run(xs) : '';\n}\n"
                                   "class Page { tick() { return Date.now() > 3 ? (Date.now() > 4 ? 1 : 2) : 0; } }\n")}
        with tempfile.TemporaryDirectory() as root:
            for path, text in sources.items():
                os.makedirs(os.path.join(root, os.path.dirname(path)), exist_ok=True)
                with open(os.path.join(root, path), "w") as f:
                    f.write(text)
            out = os.path.join(root, "out")
            subprocess.run([os.path.join(SCRIPTS, "js-quality-report"), "--root", root, "--area", "app=src",
                            "--out", out], check=True, stdout=subprocess.DEVNULL)
            m = extractors.read_js(os.path.join(out, "modules.json"), set())["src/tool.ts"]
        facts = {f.name: f for f in m.functions}
        self.assertEqual(({"files": 1}, 3), (facts["mixed"].effects, facts["mixed"].cyclomatic))
        self.assertEqual(frozenset({"src/exec.ts"}), facts["viaAdapter"].calls)
        self.assertEqual(frozenset({("src/exec.ts", "run")}), m.references)
        self.assertEqual(({"clock": 2}, 3), (facts["Page.tick"].effects, facts["Page.tick"].cyclomatic))
        adapters = rules_table.parse("limit\tmixed-cyclomatic\t3\tjs\nadapter\tjs\tsrc/exec.ts\tprocess\n")
        self.assertEqual({("mixed", "src/tool.ts:mixed"): 3, ("mixed", "src/tool.ts:viaAdapter"): 3,
                          ("mixed", "src/tool.ts:Page.tick"): 3}, kinds(design_lint.mixed_rows(m, adapters)))


class TableTest(unittest.TestCase):
    def scala_constant(self, path: str, pattern: str) -> str:
        with open(os.path.join(ROOT, path), encoding="utf-8") as f:
            found = re.search(pattern, f.read())
        self.assertIsNotNone(found, f"{pattern} in {path}")
        return found.group(1) if found else ""

    def test_every_languages_constants_agree_with_the_one_table(self) -> None:
        report = "quality/src/main/scala/quality/QualityReport.scala"
        for kind in ("cognitive", "cyclomatic", "inner-defs", "method-lines"):
            self.assertEqual(RULES.limit(kind), float(self.scala_constant(report, rf'"{kind}" -> (\d+)')), kind)
        self.assertEqual(RULES.limit("file-lines"), float(self.scala_constant(report, r'"file-lines" -> (\d+)')))
        self.assertEqual(RULES.limit("clone-tokens"), float(self.scala_constant("quality/src/main/scala/quality/Clones.scala", r"MinimumTokens = (\d+)")))
        self.assertEqual(RULES.limit("fanout"), float(self.scala_constant("project/Coupling.scala", r"DefaultFanOutLimit = (\d+)")))
        self.assertEqual(RULES.limit("distance") * 100, float(self.scala_constant("project/Coupling.scala", r"DefaultDistanceLimit = (\d+)")))
        # the data guard: the Scala one (project/Coupling.scala, landed 2026-11-26) and this core's share its thresholds
        for kind, constant in (("data-methods", "DataMethodCut"), ("data-decisions", "DataDecisionCut"),
                               ("datafanout", "DefaultDataFanOutLimit"), ("databag", "DefaultDataBagLimit")):
            self.assertEqual(RULES.limit(kind), float(self.scala_constant("project/Coupling.scala", rf"{constant} = (\d+)")), kind)
        import clones  # scripts/py-quality's
        self.assertEqual(RULES.limit("clone-tokens"), clones.MIN_TOKENS)
        self.assertEqual(RULES.limit("clone-tokens"), float(self.scala_constant("scripts/js-quality/clones.js", r"MinTokens = (\d+)")))


if __name__ == "__main__":
    unittest.main()
