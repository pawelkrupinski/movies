"""WHAT THE PRE-PUSH HOOK COMPILES AND UNIT-TESTS (scripts/hooks/pre-push `--compile-plan`, `--unit-plan`,
`--unit-universe`, `--source-lints`): the test configs a change compiles -- its module's and every dependent's, by
build.sbt's dependsOn (geodata, base -> http -> common -> identity -> testkit -> scrapers -> worker -> web -> e2e) -- and
the combined-tree verification's unit tests, the worker's rows read from build.sbt's own definition (an unreadable one
prints a command sbt refuses: the verification fails closed)."""
from __future__ import annotations

import re
from typing import Callable, Iterable, List, Sequence, Set, Tuple

from pre_push_paths import matches

# a test tree compiles its module's configs alone: (patterns, configs, and the worker's It on top)
TEST_TREES: Tuple[Tuple[Tuple[str, ...], Tuple[str, ...], Tuple[str, ...]], ...] = (
    (("repochecks/src/test/*.scala", "repochecks/src/test/*.java"), (), ("repochecks", "worker_it")),
    (("worker/src/it/*.scala", "worker/src/it/*.java"), (), ("worker_it",)),
    (("web/src/it/*.scala", "web/src/it/*.java"), ("web/It/compile",), ()),
    (("web/src/page/*.scala", "web/src/page/*.java"), ("web/Page/compile",), ()),
    (("web/src/test/*.scala", "web/src/test/*.java"), ("web/Test/compile", "web/It/compile", "web/Page/compile"), ()),
    (("worker/src/test/*.scala", "worker/src/test/*.java"), ("worker/Test/compile",), ("worker_it",)),
    (("identity/src/test/*.scala", "identity/src/test/*.java"), ("identity/Test/compile",), ()),
    (("scrapers/src/test/*.scala", "scrapers/src/test/*.java"), ("scrapers/Test/compile",), ()),
    (("testkit/src/test/*.scala",), ("testkit/Test/compile",), ()),
)
OWN_TEST = ("base/src/test/*.scala", "http/src/test/*.scala", "geodata/src/test/*.scala", "common/src/test/*.scala",
            "common/src/test/*.java")
# a main path: the modules it touches (first match wins)
MAIN_TREES: Tuple[Tuple[Tuple[str, ...], Tuple[str, ...]], ...] = (
    (("build.sbt", "project/*"), ("common",)),
    (("common/*.scala", "common/*.java", "geodata/*.scala", "base/*.scala", "http/*.scala"), ("common",)),
    (("identity/*.scala", "identity/*.java"), ("identity",)),
    (("testkit/*.scala", "testkit/*.java"), ("testkit",)),
    (("scrapers/*.scala", "scrapers/*.java"), ("scrapers",)),
    (("repochecks/src/fixtures/*",), ("repochecks", "worker")),
    (("repochecks/*.scala", "repochecks/*.java"), ("repochecks", "worker_it")),
    (("worker/src/fixtures/*",), ("worker", "web")),
    (("worker/*.scala", "worker/*.java"), ("worker",)),
    (("web/*.scala", "web/*.java"), ("web",)),
    (("e2e/*.scala", "e2e/*.java"), ("e2e",)),
)
# what a module's tests being compiled brings: (module, implied)
IMPLIED = (("common", "identity"), ("identity", "testkit"), ("testkit", "scrapers"), ("scrapers", "worker"),
           ("worker", "e2e"), ("web", "e2e"), ("testkit", "web"), ("testkit", "repochecks"))
COMPILES = (("common", ("common/Test/compile",)), ("identity", ("identity/Test/compile",)),
            ("testkit", ("testkit/Test/compile",)), ("scrapers", ("scrapers/Test/compile",)),
            ("repochecks", ("repochecks/Test/compile",)), ("worker", ("worker/Test/compile", "worker/It/compile")))
WEB = ("web/Test/compile", "web/It/compile", "web/Page/compile")


def touched(path: str, modules: Set[str], tests: List[str]) -> None:
    """What one path compiles: a test tree's configs into `tests`, a main path's modules into `modules`."""
    for patterns, configs, marks in TEST_TREES:
        if matches(path, patterns):
            tests.extend(configs)
            modules.update(marks)
            return
    if matches(path, OWN_TEST):
        tests.append(path.split("/", 1)[0] + "/Test/compile")
        return
    for patterns, marks in MAIN_TREES:
        if matches(path, patterns):
            modules.update(marks)
            return


def already_compiled(config: str, modules: Set[str]) -> bool:
    """Whether a test tree's own config is one the modules' compiles name already."""
    module = config.split("/", 1)[0]
    if module in ("common", "base", "http", "geodata"):
        return "common" in modules and module == "common"
    return module in ("identity", "testkit", "scrapers", "worker", "web") and module in modules


def module_compiles(modules: Set[str]) -> List[str]:
    """The test configs the touched modules compile, in dependency order."""
    out = [c for module, configs in COMPILES if module in modules for c in configs]
    if "worker" not in modules and "worker_it" in modules:
        out.append("worker/It/compile")
    return out + [c for module, configs in (("web", WEB), ("e2e", ("e2e/Test/compile",))) if module in modules
                  for c in configs]


def compile_plan(changed: Iterable[str]) -> List[str]:
    modules: Set[str] = set()
    tests: List[str] = []
    for path in changed:
        touched(path, modules, tests)
    for module, implied in IMPLIED:
        if module in modules:
            modules.add(implied)
    out = module_compiles(modules) + [c for c in tests if not already_compiled(c, modules)]
    return list(dict.fromkeys(out))


OWN_CODE = re.compile(r"^(build\.sbt|project/.*|testkit/.*|[^/]+/src/(test|it|page|fixtures)/.*)$")


def own_code_modules(changed: Iterable[str]) -> List[str]:
    """The modules whose Test config compiles a pushed test-side path (a spec's own code)."""
    configs = compile_plan(p for p in changed if OWN_CODE.match(p))
    return [c[:-len("/Test/compile")] for c in configs if c.endswith("/Test/compile")]


QUOTED = re.compile(r'"([^"]*)"')
BLOCKS = (("^lazy val HeavyRows", "heavy"), ("^lazy val HeavySuites", ""), ("^lazy val WorkerRestSuites", "rest"),
          ("^lazy val workerRowSettings", "row"))


def next_block(line: str, block: str) -> str:
    for start, name in BLOCKS:
        if re.match(start, line):
            block = name
    return block


def quoted(kind: str, line: str) -> List[Tuple[str, str]]:
    """The quoted names of a build line, comments off: every one a prefix, else only those with a dot."""
    line = line.split("//", 1)[0]
    return [(kind, n) for n in QUOTED.findall(line) if kind == "prefix" or "." in n]


def line_rows(line: str, block: str) -> Tuple[List[Tuple[str, str]], str]:
    """One build line's rows and the block after it (the awk reading of worker_rows, line by line)."""
    block = next_block(line, block)
    rows: List[Tuple[str, str]] = []
    if block == "row" and "exists(suite.startsWith)" in line:
        rows += quoted("prefix", line)
    if block in ("heavy", "rest"):
        rows += quoted(block, line)
    if block == "rest" and re.search(r"\)\s*(//.*)?$", line):
        block = ""
    if block == "row" and re.match(r"^\S", line) and not line.startswith("lazy val workerRowSettings"):
        block = ""
    return rows, block


def worker_rows(build: str) -> List[Tuple[str, str]]:
    rows: List[Tuple[str, str]] = []
    block = ""
    for line in build.splitlines():
        found, block = line_rows(line, block)
        rows += found
    return rows


def names(rows: Sequence[Tuple[str, str]], kind: str) -> List[str]:
    return [name for row_kind, name in rows if row_kind == kind]


def rest_exclusions(rows: Sequence[Tuple[str, str]], prefixes: Sequence[str]) -> List[str]:
    """The identity row's prefixes and every heavy suite outside them, as `-` exclusions."""
    heavy = [name for name in names(rows, "heavy") if not any(name.startswith(p) for p in prefixes)]
    return [f"-{p}*" for p in prefixes] + [f"-{name}" for name in heavy]


def worker_rest_row(rows: Sequence[Tuple[str, str]]) -> Tuple[str, List[str]]:
    """The rest row's command and the suites it runs besides; an unreadable build: a command sbt refuses."""
    prefixes, rest = names(rows, "prefix"), names(rows, "rest")
    if not prefixes or not rest:
        return "worker/build.sbt-WORKER_ROW-unreadable", []
    heavy = set(names(rows, "heavy"))
    return (" ".join(["worker/testOnly *"] + rest_exclusions(rows, prefixes)),
            [name for name in rest if name not in heavy])


UNIT_MODULES = ("base", "http", "common", "identity", "testkit", "scrapers", "repochecks", "web", "worker")


def unit_universe(rows: Sequence[Tuple[str, str]]) -> List[str]:
    """Every module's unit tests, the worker's every suite but its heavy rows'."""
    out = [f"{module}/Test/test" for module in UNIT_MODULES[:-1]]
    heavy = [name for kind, name in rows if kind == "heavy"]
    if not heavy:
        return out + ["worker/build.sbt-HeavyRows-unreadable"]
    return out + [" ".join(["worker/testOnly *"] + [f"-{name}" for name in heavy])]


IDENTITY_ROW_SPEC = ("worker/src/test/scala/services/identity/*Spec.scala", "worker/src/test/scala/scripts/*Spec.scala",
                     "worker/src/test/scala/tools/*Spec.scala")


def unit_modules(changed: Iterable[str]) -> Tuple[Set[str], List[str]]:
    """The modules with a path under their src/, and the worker identity-row specs among the paths, by class."""
    modules: Set[str] = set()
    specs: List[str] = []
    for path in changed:
        module = path.split("/", 1)[0]
        if module in UNIT_MODULES and path.startswith(module + "/src/"):
            modules.add(module)
            if matches(path, IDENTITY_ROW_SPEC):
                specs.append(path[len("worker/src/test/scala/"):-len(".scala")].replace("/", "."))
    return modules, specs


def unit_plan(changed: Iterable[str], rows: Sequence[Tuple[str, str]]) -> List[str]:
    """`<module>/Test/test` for each module with a path under its src/, the worker's rest row, and the worker's
    identity-row specs the paths change, by name, in one run with the rest row's own suites."""
    modules, specs = unit_modules(changed)
    out = [f"{m}/Test/test" for m in UNIT_MODULES if m in modules and m != "worker"]
    suites: List[str] = []
    if "worker" in modules:
        command, suites = worker_rest_row(rows)
        out.append(command)
    suites = list(dict.fromkeys(suites + specs))
    return out + ([f"worker/testOnly {' '.join(suites)}"] if suites else [])


WALKERS = re.compile(r"\b(ScalaSourceScan|StatefulFieldScan|ModuleSources|SourceTrees)\b")
Specs = Callable[[str], List[str]]  # a module's test sources naming a source walker, sorted


def runs_class(plan: Sequence[str], module: str, cls: str) -> bool:
    pattern = re.compile(rf"^{module}/testOnly( .*)? {re.escape(cls)}( |$)")
    return any(pattern.match(line) for line in plan)


def module_source_lints(module: str, plan: Sequence[str], lints: Sequence[str], walker_specs: Specs) -> List[str]:
    """A module's walker specs as classes, less those the lint plan or the plan's lines run already."""
    classes: List[str] = []
    for spec in walker_specs(module):
        cls = spec[len(f"{module}/src/test/scala/"):-len(".scala")].replace("/", ".")
        if f"{module}:{cls}" not in lints and not runs_class(plan, module, cls):
            classes.append(cls)
    return classes


def source_lint_lines(plan: Sequence[str], lints: Sequence[str], walker_specs: Specs) -> List[str]:
    """THE LINTS WALKING EVERY MODULE'S SOURCES not already run: a `<module>/testOnly <classes>` line a module."""
    out: List[str] = []
    for module in UNIT_MODULES:
        if f"{module}/Test/test" in plan:
            continue
        classes = module_source_lints(module, plan, lints, walker_specs)
        if classes:
            out.append(f"{module}/testOnly " + " ".join(classes))
    return out


def walker_spec_finder(walk: Callable[[str], Iterable[Tuple[str, str]]]) -> Specs:
    """A module's `*Spec.scala` under src/test/scala naming a source walker, from `walk(dir)`'s (path, text) pairs."""
    def find(module: str) -> List[str]:
        found = [path for path, text in walk(f"{module}/src/test/scala")
                 if path.endswith("Spec.scala") and WALKERS.search(text)]
        return sorted(found)
    return find

