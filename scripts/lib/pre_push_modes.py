"""THE PRE-PUSH HOOK'S PLAN MODES (scripts/hooks/pre-push; pre_push_test.py holds every one): each reads changed paths
(or sbt lines) one a line and prints what the hook would run for them, and exits -- `--plan`, `--compile-plan`, the
ratchets' `--coupling-plan` / `--quality-plan` / `--js-quality-plan` / `--py-quality-plan`, `--identity-change`,
`--unit-plan`, `--unit-universe`, `--source-lints [<lints>]`, `--lint-plan`, `--per-file-lints`,
`--script-test-plan`, `--failure-summary <log>` -- and the hook's own questions of them (`--reads-gitops`,
`--without-unfetched-gitops`, `--shell-paths`, `--production-paths`). The checkout's files come through `Files` (its
root), the hook's tables from beside the hook, run-script-tests through `Run`."""
from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Callable, Dict, List, Mapping, Sequence, Tuple

import pre_push_lints as lints
import pre_push_paths as paths_of
import pre_push_plan as planning
import pre_push_units as units
from tool_files import Files

Run = Callable[[Sequence[str], str], Tuple[int, str]]  # (argv, stdin) -> (status, stdout)


@dataclass(frozen=True)
class Checkout:
    """The checkout the hook answers for: its files (cwd, its root), the hook's own directory, the environment."""
    files: Files
    hooks: str                 # scripts/hooks, beside the hook (its tables, ../run-script-tests, ../../build.sbt)
    env: Mapping[str, str]
    run: Run

    def table(self, name: str) -> str:
        return self.files.read_text_or(f"{self.hooks}/{name}", "")

    def first_line(self, path: str) -> str:
        if not self.files.is_file(path):
            return ""
        try:
            return self.files.read_text(path).split("\n", 1)[0]
        except (OSError, UnicodeDecodeError):
            return ""

    def says_metrics(self, path: str) -> bool:
        try:
            return "# TYPE kinowo_" in self.files.read_text_or(path, "")
        except (OSError, UnicodeDecodeError):
            return False

    def skipped(self, check: str) -> bool:
        return check in self.env.get("PREPUSH_SKIP", "").split()

    def push(self, changed: List[str]) -> lints.Push:
        return lints.Push(changed, set(units.own_code_modules(changed)), self.env.get("PREPUSH_GITOPS") == "1",
                          self.skipped("lintspecs"))

    def tree(self) -> planning.Tree:
        code, reads = self.run([f"{self.hooks}/../run-script-tests", "--reads"], "")
        allow = self.files.read_text_or(".github/allow-lists.txt", "")
        local_tests = self.table("local-tests.txt")
        return planning.Tree(
            script_test_reads=[g for g in reads.splitlines() if g] if code == 0 else [],
            registry_files=set(lints.named_files(self.table("lint-registry.tsv"))),
            allow_lists=set(allow.splitlines()),
            identity_tests=bool(re.search(r"(?m)^identity[ \t]+(sbt|sh|first)[ \t]", local_tests)),
            corpus_in_basic=planning.is_corpus_in_basic(self.table("../../build.sbt"), local_tests),
            heavy_rows=self.env.get("PREPUSH_HEAVY_ROWS") == "1",
            first_line=self.first_line, exports_metrics=self.says_metrics)

    def worker_rows(self) -> List[Tuple[str, str]]:
        return units.worker_rows(self.files.read_text_or("build.sbt", ""))

    def walk(self, directory: str) -> List[Tuple[str, str]]:
        if not self.files.is_dir(directory):
            return []
        return [(p, self.files.read_text_or(p, "")) for p in self.files.under(directory)]


def relative(checkout: Checkout, found: List[str]) -> List[str]:
    """The walker specs' paths, relative to the checkout's root."""
    base = checkout.files.path() + "/"
    return [p[len(base):] if p.startswith(base) else p for p in found]


Answer = Callable[[Checkout, List[str], Sequence[str]], Tuple[int, List[str]]]


def answer_plan(c: Checkout, changed: List[str], _args: Sequence[str]) -> Tuple[int, List[str]]:
    return 0, planning.plan(changed, c.tree())


def registry_answer(ask: Callable[[str, lints.Push], List[str]]) -> Answer:
    """A question of the lint registry about the push's paths."""
    return lambda c, changed, _args: (0, ask(c.table("lint-registry.tsv"), c.push(changed)))


def answer_reads_gitops(c: Checkout, changed: List[str], _args: Sequence[str]) -> Tuple[int, List[str]]:
    return (0 if lints.reads_gitops(c.table("lint-registry.tsv"), c.push(changed)) else 1), []


def answer_unfetched(c: Checkout, given: List[str], _args: Sequence[str]) -> Tuple[int, List[str]]:
    fetched = c.env.get("PREPUSH_GITOPS") == "1"
    return 0, lints.without_unfetched_gitops(c.table("lint-registry.tsv"), given, fetched)


def answer_source_lints(c: Checkout, plan: List[str], args: Sequence[str]) -> Tuple[int, List[str]]:
    finder = units.walker_spec_finder(c.walk)
    return 0, units.source_lint_lines(plan, (args[0] if args else "").split("\n"),
                                      lambda module: relative(c, finder(module)))


def answer_script_tests(c: Checkout, changed: List[str], _args: Sequence[str]) -> Tuple[int, List[str]]:
    """run-script-tests' own selection, its listing's first column; the paths handed over on its stdin."""
    code, out = c.run([f"{c.hooks}/../run-script-tests", "--paths", "/dev/stdin", "--list"],
                      "".join(p + "\n" for p in changed))
    return code, [line.split("\t", 1)[0] for line in out.splitlines()]


def pure(select: Callable[[List[str]], List[str]]) -> Answer:
    return lambda _c, changed, _args: (0, select(changed))


ANSWERS: Dict[str, Answer] = {
    "--plan": answer_plan,
    "--compile-plan": pure(units.compile_plan),
    "--coupling-plan": pure(paths_of.coupling_paths),
    "--quality-plan": pure(paths_of.quality_paths),
    "--js-quality-plan": pure(paths_of.js_quality_paths),
    "--py-quality-plan": lambda c, changed, _a: (0, paths_of.py_quality_paths(changed, c.first_line)),
    "--identity-change": pure(paths_of.identity_change),
    "--unit-plan": lambda c, changed, _a: (0, units.unit_plan(changed, c.worker_rows())),
    "--unit-universe": lambda c, _changed, _a: (0, units.unit_universe(c.worker_rows())),
    "--source-lints": answer_source_lints,
    "--lint-plan": registry_answer(lints.lint_plan),
    "--per-file-lints": registry_answer(lambda registry, push: lints.lint_plan(registry, push, per_file=True)),
    "--script-test-plan": answer_script_tests,
    "--reads-gitops": answer_reads_gitops,
    "--without-unfetched-gitops": answer_unfetched,
    "--shell-paths": lambda c, changed, _a: (0, [p for p in changed if c.files.is_file(p)
                                                 and paths_of.is_shell_path(p, c.first_line)]),
    "--production-paths": pure(lambda changed: [p for p in changed if paths_of.is_production_path(p)]),
}
# the modes a person or agentctl asks (the hook's own questions above are answered too, never listed as plan modes)
PLAN_MODES = ("--plan", "--compile-plan", "--coupling-plan", "--quality-plan", "--js-quality-plan",
              "--py-quality-plan", "--identity-change", "--unit-plan", "--unit-universe", "--source-lints",
              "--lint-plan", "--per-file-lints", "--script-test-plan", "--failure-summary")


def is_mode(word: str) -> bool:
    return word in ANSWERS or word == "--failure-summary"


def answer(checkout: Checkout, mode: str, args: Sequence[str], stdin: str) -> Tuple[int, str]:
    """A mode's exit status and stdout."""
    if mode == "--failure-summary":
        return 0, failure_summary(checkout.files.read_text_or(args[0], "") if args else "")
    given = [line for line in stdin.split("\n") if line] if mode != "--source-lints" else stdin.split("\n")
    code, out = ANSWERS[mode](checkout, given, args)
    return code, "".join(line + "\n" for line in out)


ESCAPES = r"(\x1b\[[0-9;]*m)*"
FAILED = re.compile(r"\*\*\* (FAILED|ABORTED) \*\*\*")
UNDER = re.compile(rf"^{ESCAPES}\[{ESCAPES}info{ESCAPES}\]{ESCAPES} {ESCAPES}  ")
ERRORS = re.compile(r"\[error\]|\*\*\* [0-9]+ TESTS? FAILED|✘|[0-9]+ failed|Error:")


def failing_tests(log: List[str]) -> List[str]:
    """Every failing test with its message's first lines (the lines indented under it, at most four)."""
    shown: List[str] = []
    under = 0
    for line in log:
        if FAILED.search(line):
            shown.append(line)
            under = 4
        elif under > 0 and UNDER.search(line):
            shown.append(line)
            under -= 1
        else:
            under = 0
    return shown[:80]


def failure_summary(text: str) -> str:
    """What a failed sbt (or area) run shows of its log: the failing tests first, then sbt's [error] lines; a run
    that failed before any (sbt-warm refused, a JVM that never started): the log's end, less the walk's lines."""
    log = text.split("\n")
    if log and log[-1] == "":
        log.pop()
    shown = failing_tests(log) + [line for line in log if ERRORS.search(line)][:40]
    if not shown:
        shown = [line for line in log if not line.startswith("population walk: ")][-20:]
    return "".join(line + "\n" for line in shown)
