#!/usr/bin/env python3
"""The general rule core (scripts/design-rules/): every rule over every language's modules -- naming (suffix,
abbreviation, idioms), data or collaborator (effects, state, interfaces, the data guard, the closure), collaborator-only
fan-out, distance with V2 abstractness, layers, the design lint (adapters, state, import-time IO, the main guard), the
exemption lint -- each through its extractor (Python's `ast`, JS/TS's facts.js, Scala's text), and the one rules table
every language's constants agree with. DESIGN_RULES_UNDER_TEST names another copy of the core (the mutation test's)."""
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
import naming  # noqa: E402
import packages  # noqa: E402
import python_facts  # noqa: E402
import rules_table  # noqa: E402
import scala_names  # noqa: E402
from model import DataType, Module, Name  # noqa: E402

RULES = rules_table.load(ROOT)
EXTRA = rules_table.parse("\n".join([
    "limit\tfanout\t2\tpy", "limit\tdistance\t0.2\tpy", "limit\tdistance-min-modules\t2\tpy",
    "limit\tdata-methods\t4\tpy", "limit\tdata-decisions\t9\tpy", "limit\tdatabag\t9\tpy", "limit\tdatafanout\t2\tpy",
    "adapter\tpy\tlib/clock.py\tclock", "distance\tpy\tlib",
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
        source = "import time\n\ndef stamp():\n    return time.time()\n"
        rows = kinds(design_lint.rows([py("tools/t.py", source), py("lib/clock.py", source)], EXTRA))
        self.assertEqual({("effect", "tools/t.py"): 1}, rows)

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


class ExemptionsTest(unittest.TestCase):
    def test_a_generic_or_undated_why_is_refused_and_a_specific_one_passes(self) -> None:
        bad = exemptions.parse("effect\ttools/t.py\tyesterday\tneeded for now\n")
        problems = exemptions.row_problems(bad)
        self.assertTrue(any("no YYYY-MM-DD date" in p for p in problems), problems)
        self.assertTrue(any("`for now` is generic" in p for p in problems), problems)
        good = exemptions.parse("effect\ttools/t.py\t2026-11-26\tprocess adapter: runs the one git binary for every"
                                " reader, which receive its runner\n")
        self.assertEqual([], exemptions.row_problems(good))


class JsFactsTest(unittest.TestCase):
    def test_the_js_extractor_reads_effects_state_import_io_and_names(self) -> None:
        source = ("let count = 0;\nconst seen = [];\nconst LIMIT = 3;\nconst started = Date.now();\n"
                  "function readCfg() { count += 1; seen.push(new Date()); return LIMIT; }\n"
                  "function init() { const page = { n: 0 }; page.n = 1; return process.env.HOME; }\n")
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
        self.assertEqual({"clock": 2, "env": 1}, m.effects)
        self.assertIn(("abbreviation", "src/page.js:readCfg"), kinds(naming.rows([m], RULES)))


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
