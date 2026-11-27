"""THE LIGHT CHECKS (scripts/hooks/pre-push's header says what each guards): the checks that run a lint, a generator's
drift check or a commit rule over what the push changed, each in seconds -- eslint, jsquality, pyquality, drift,
snapshot, dashboards, metricalerts, actionlint, shellcheck, newchecks, schedules, allowlists, driftguards, puremove,
stampcarry, judgements, ios. Each runs in a process of its own (`scripts/pre_push.py <hook> --check <name> <state>`,
pre_push_checks.Python), at the checked tree, after the run's state (pre_push_checks.Shell's variables, as JSON); its
lines are the shell hook's: `▸ pre-push` on stdout, `! pre-push` on stderr, each tool's own output where it wrote it."""
from __future__ import annotations

import json
import re
from dataclasses import dataclass
from typing import Callable, Dict, List, Mapping, MutableMapping, Optional, Protocol, Sequence, Union

import machine
import pre_push_modes as modes
import tool_python
from pre_push_repo import Repo, Say, into

Value = Union[str, List[str]]
PYTHON_FIX = "install Python 3 (brew install python)"
NODE_FIX = "install Node.js (brew install node)"
STDERR = 2
GH_SECONDS = "15"
JUDGEMENTS = "test/resources/fixtures/identity-judgements.tsv"
SERVED_JQ = '[.[] | select(.conclusion == "success" or .conclusion == "failure")][0:3][].databaseId'
FAILURES = re.compile(r"^FAIL|^ERROR|AssertionError")


@dataclass
class Check:
    """One check's run: the checked tree (`repo`, its cwd), the hook's lines, the run's state, where the hook is."""
    repo: Repo
    say: Say
    state: Mapping[str, Value]
    hook: str

    # ── the run's state ──
    def text(self, name: str) -> str:
        value = self.state.get(name, "")
        return value if isinstance(value, str) else " ".join(value)

    def changed(self) -> List[str]:
        return [p for p in self.text("changed").split("\n") if p]

    def ranges(self) -> List[str]:
        value = self.state.get("ranges", [])
        return list(value) if isinstance(value, list) else value.split()

    def range_args(self) -> List[str]:
        return [w for r in self.ranges() for w in ("--range", r)]

    def tmp(self, name: str) -> str:
        return f"{self.text('tmp')}/{name}"

    def scripts(self) -> str:
        """The hook's own scripts/ (`$(dirname "${BASH_SOURCE[0]}")/..`)."""
        return f"{self.hook.rsplit('/', 1)[0]}/.."

    # ── tools ──
    def found(self, tool: str) -> bool:
        return machine.output(["sh", "-c", 'command -v "$1"', "sh", tool], env=self.repo.env)[0] == 0

    def missing(self, check: str, tool: str, fix: str) -> int:
        """A MISSING LOCAL TOOL: the check fails, saying how to fix it."""
        self.say.warn(f"{tool} not found: the {check} check cannot run, so the push is BLOCKED (never skipped). Fix: "
                      f"{fix} -- then push again (or PREPUSH_SKIP={check}, named in your report).")
        return 1

    def need(self, check: str, tool: str, fix: str) -> bool:
        return self.found(tool) or self.missing(check, tool, fix) == 0

    # ── commands ──
    def run(self, argv: Sequence[str], out: machine.Sink = None, env: Optional[Mapping[str, str]] = None) -> int:
        """A command's status, its stdout ours (or `out`: STDERR, `>&2`), its stderr ours."""
        self.say.flush()
        return machine.status(argv, self.repo.cwd, self.env_with(env), out=out)

    def logged(self, argv: Sequence[str], log: str, env: Optional[Mapping[str, str]] = None) -> int:
        """A command's status, its stdout and stderr into `log` (`> log 2>&1`)."""
        return machine.status(into(log, argv), self.repo.cwd, self.env_with(env))

    def env_with(self, extra: Optional[Mapping[str, str]]) -> MutableMapping[str, str]:
        return dict(self.repo.env, **(extra or {}))

    def log_text(self, log: str) -> List[str]:
        text = self.repo.files.read_text_or(log, "")
        return text[:-1].split("\n") if text.endswith("\n") else text.split("\n") if text else []

    def shown(self, lines: Sequence[str]) -> None:
        self.say.raw("".join(line + "\n" for line in lines))

    def printed(self, lines: Sequence[str]) -> None:
        """Lines on stdout (`shown`'s go to stderr)."""
        self.say.raw("".join(line + "\n" for line in lines), to_err=False)

    def both(self, argv: Sequence[str]) -> "tuple[int, str, str]":
        return machine.both(argv, self.repo.cwd, self.repo.env)

    def answer(self, argv: Sequence[str]) -> "tuple[int, str]":
        """(status, stdout) of a command, its stderr ours."""
        return machine.captured(argv, cwd=self.repo.cwd, env=self.repo.env)

    def holds(self, test: str, path: str) -> bool:
        """`test <test> <path>` at the tree (-x: executable, -f: a file)."""
        return machine.output(["test", test, path], cwd=self.repo.cwd)[0] == 0

    def quietly(self, argv: Sequence[str]) -> int:
        return machine.status(argv, out=machine.QUIET, err=machine.QUIET)

    def temporary_directory(self, template: str) -> str:
        """A new directory `mktemp -d` makes of `template`, "" when it cannot."""
        made, out = machine.output(["mktemp", "-d", template], env=self.repo.env)
        return out.strip() if made == 0 else ""

    def shell_paths(self) -> List[str]:
        checkout = modes.Checkout(self.repo.files, self.hook.rsplit("/", 1)[0], self.repo.env, self.selection_run)
        return modes.answer(checkout, "--shell-paths", [], self.text("changed") + "\n")[1].split("\n")

    def selection_run(self, argv: Sequence[str], text: str) -> "tuple[int, str]":
        return machine.output(argv, text, cwd=self.repo.cwd, env=self.repo.env)

    def warnings(self, path: str, cwd: str) -> int:
        out = machine.output(["shellcheck", "-S", "warning", "-f", "gcc", path], cwd=cwd, env=self.repo.env)[1]
        return out.count("\n")

    def bounded(self, seconds: str, argv: Sequence[str], out: machine.Sink = None) -> "tuple[int, str]":
        lib = f"{self.scripts()}/lib/tool_bounded.py"
        return machine.run(tool_python.isolated(lib, seconds, *argv),
                           machine.Ran(self.repo.cwd, self.repo.env, out, machine.QUIET))

    def common_dir(self) -> str:
        return self.repo.git_out("rev-parse", "--path-format=absolute", "--git-common-dir").strip()


def tail(lines: Sequence[str], n: int) -> List[str]:
    return list(lines[-n:]) if n else []


def from_to_end(lines: Sequence[str], start: "re.Pattern[str]") -> List[str]:
    """sed -n '/start/,$p'."""
    for i, line in enumerate(lines):
        if start.search(line):
            return list(lines[i:])
    return []


def with_following(lines: Sequence[str], start: "re.Pattern[str]", following: int) -> List[str]:
    """sed -n '/start/,+N p': each matching line and the N after it."""
    shown: List[str] = []
    left = -1
    for line in lines:
        if left >= 0:
            shown.append(line)
            left -= 1
        elif start.search(line):
            shown.append(line)
            left = following - 1
    return shown


# ── the checks ──
def eslint(c: Check) -> int:
    if not c.need("eslint", "npm", NODE_FIX):
        return 1
    if not c.holds("-x", "node_modules/.bin/eslint"):
        c.say.log("installing ESLint (npm ci, once per worktree)")
        if c.run(["npm", "ci", "--no-audit", "--no-fund", "--silent"], out=machine.QUIET) != 0:
            return 1
    return c.run(["npm", "run", "--silent", "lint"])


def quality_tools(c: Check, check: str, report: str, installed: str) -> bool:
    """node and python3, and npm unless `report --tools-dir` already holds the tools (`installed` under it)."""
    if not (c.need(check, "node", NODE_FIX) and c.need(check, "python3", PYTHON_FIX)):
        return False
    tools = c.answer([report, "--tools-dir"])[1].rstrip("\n")
    return (c.holds("-x" if installed.endswith("eslint") else "-f", f"{tools}/{installed}")
            or c.need(check, "npm", "install Node.js with npm (brew install node)"))


def design_rules(c: Check, language: str) -> int:
    """The general design rules over a language (scripts/design-rules: names, fan-out, distance, layers, the lint)."""
    for tool in ("naming_lint.py", "design_check.py"):
        code = c.run(["python3", f"scripts/design-rules/{tool}", "check", "--languages", language, *c.range_args()],
                     out=STDERR)
        if code != 0:
            return code
    return 0


def jsquality(c: Check) -> int:
    # the tools are js-quality-report's to install, once per lock and OUTSIDE the checkout (--tools-dir): an `npm ci` in
    # scripts/js-quality here left node_modules in every check's tree, for a self-test's `grep -r` to read (#99)
    if not quality_tools(c, "jsquality", "scripts/js-quality-report", "node_modules/.bin/eslint"):
        return 1
    if c.run(["scripts/js-quality-report", "--check", *c.range_args()], out=STDERR) != 0:
        return 1
    return design_rules(c, "js")


def pyquality(c: Check) -> int:
    # pyright is py-quality-report's to install, once per lock and OUTSIDE the checkout (--tools-dir), as js-quality's
    if not quality_tools(c, "pyquality", "scripts/py-quality-report", "node_modules/pyright/index.js"):
        return 1
    paths = c.tmp("pyquality-paths")  # strict pyright over the push's own files; the rest keep their rows
    c.repo.files.write_text(paths, c.text("changed") + "\n")
    if c.run(["scripts/py-quality-report", "--check", "--paths", paths, *c.range_args()], out=STDERR) != 0:
        return 1
    return design_rules(c, "py")


def drift(c: Check) -> int:
    if not c.need("drift", "python3", PYTHON_FIX):
        return 1
    # data/us/scripts/generate_roster.py needs timezonefinder: the PATH's python3 when it has it, else the pinned venv
    # scripts/timezonefinder-python builds once (2026-11-17: Homebrew's python refuses a pip install, PEP 668)
    code, out, err = c.both([f"{c.scripts()}/timezonefinder-python"])
    if code != 0:
        c.say.raw(err)
        return c.missing("drift", "python3's timezonefinder",
                         "scripts/timezonefinder-python (it builds the pinned venv; its error is above)")
    log = c.tmp("drift.log")
    # GeoNames' Polish gazetteer, kept in the shared git dir so every worktree downloads it once.
    env = {"PATH": f"{out.rstrip(chr(10))}:{c.repo.env.get('PATH', '')}",
           "GEONAMES_CACHE": f"{c.common_dir()}/kinowo-cache/geonames"}
    if c.logged(["scripts/ci/generated-artefacts-drift.sh"], log, env) == 0:
        return 0
    lines = c.log_text(log)
    c.shown(from_to_end(lines, re.compile("::error::"))[:80] if any("::error::" in line for line in lines)
            else tail(lines, 30))
    return 1


def snapshot(c: Check) -> int:
    log = c.tmp("snapshot.log")
    if c.logged(["scripts/read-model-snapshot-inputs.sh", "check", c.text("head_sha")], log) == 0:
        return 0
    c.shown(tail(c.log_text(log), 40))
    c.say.warn(f"full log: {log}")
    return 1


def dashboards(c: Check) -> int:
    if not c.need("dashboards", "python3", PYTHON_FIX):
        return 1
    log = c.tmp("dashboards.log")
    if c.logged(["python3", "infra/test/test_dashboards.py"], log) == 0:
        return 0
    c.shown(with_following(c.log_text(log), FAILURES, 3)[:40])
    return 1


def metricalerts(c: Check) -> int:
    if not c.need("metricalerts", "python3", PYTHON_FIX):
        return 1
    tools = c.scripts()
    code, yaml_bin = c.answer([f"{tools}/python-venv", f"{tools}/pyyaml-requirements.txt", "yaml"])
    if code != 0:
        return c.missing("metricalerts", "python3's PyYAML",
                         "scripts/python-venv (it builds the pinned venv; its error is above)")
    log = c.tmp("metricalerts.log")
    path = {"PATH": f"{yaml_bin.rstrip(chr(10))}:{c.repo.env.get('PATH', '')}"}
    if c.logged(["python3", "infra/test/test_metric_alert_coverage.py"], log, path) == 0:
        return 0
    c.shown(from_to_end(c.log_text(log), FAILURES)[:60])
    return 1


def actionlint(c: Check) -> int:
    """actionlint over the changed workflows; a composite action's `run:` blocks, which actionlint does not lint,
    through scripts/ci/shellcheck-action-runs.sh. CI's `Workflow lint` step runs both over the whole tree."""
    workflows = [p for p in c.changed() if re.search(r"^\.github/workflows/.*\.ya?ml$", p)]
    actions = [p for p in c.changed() if re.search(r"^\.github/actions/[^/]*/action\.ya?ml$", p)]
    code = 0
    if workflows:
        if c.found("actionlint"):
            code |= c.run(["actionlint", *workflows], env={"SHELLCHECK_OPTS": "-S warning"}) != 0
        else:
            code |= c.missing("actionlint", "actionlint", "brew install actionlint")
    if actions:
        if c.found("shellcheck"):
            code |= c.run(["scripts/ci/shellcheck-action-runs.sh", *actions]) != 0
        else:
            code |= c.missing("actionlint", "shellcheck", "brew install shellcheck")
    return int(code)


def shellcheck(c: Check) -> int:
    """Fail on a warning the push ADDS: each changed script's warnings counted at the base and at HEAD."""
    if not c.need("shellcheck", "shellcheck", "brew install shellcheck"):
        return 1
    shell = c.shell_paths()
    # local-tests.txt, lint-registry.tsv: the hook's tables, no shell
    return int(any([added_warnings(c, p) for p in shell if p and not p.endswith((".txt", ".tsv"))]))


def added_warnings(c: Check, path: str) -> bool:
    """Whether the push adds shellcheck warnings to `path`, said with them."""
    after, before, base = c.warnings(path, c.repo.cwd), 0, c.text("base_sha")
    if c.repo.git("cat-file", "-e", f"{base}:{path}")[0] == 0:
        copy = c.tmp("base.sh")
        c.repo.files.write_text(copy, c.repo.git_out("show", f"{base}:{path}"))
        before = c.warnings(copy, c.repo.files.path(path.rsplit("/", 1)[0] if "/" in path else "."))
    if after <= before:
        return False
    c.say.warn(f"{path}: shellcheck warnings {before} → {after}")
    c.run(["shellcheck", "-S", "warning", path], out=STDERR)
    return True


def python_tool(check: str, *argv: str) -> Callable[[Check], int]:
    def run(c: Check) -> int:
        return c.run(["python3", *argv]) if c.need(check, "python3", PYTHON_FIX) else 1
    return run


def commit_rules(rules: str) -> Callable[[Check], int]:
    """THE DRIFT GATE'S COMMIT RULES (the header's `allowlists` and `driftguards`): one walker, scripts/ci/commit_rules.py."""
    def run(c: Check) -> int:
        if not c.need(rules, "python3", PYTHON_FIX):
            return 1
        return c.run(["python3", "scripts/ci/commit_rules.py", rules, *c.range_args()])
    return run


def puremove(c: Check) -> int:
    """A PURE MOVE'S CLAIM HOLDS (the header's `puremove`): each commit saying `Pure-Move:` is one, by the check
    identity-what-if runs, over that commit alone."""
    if not c.need("puremove", "python3", PYTHON_FIX):
        return 1
    code = 0
    for r in c.ranges():
        for sha in c.repo.lines("rev-list", "--reverse", r):
            code |= pure_move(c, sha)
    return code


def pure_move(c: Check, sha: str) -> int:
    claim = "\n".join(c.repo.lines("log", "-1", "--format=%(trailers:key=Pure-Move,valueonly,unfold)", sha))
    if not claim:
        return 0
    if claim != "verified":
        c.say.warn(f"puremove: {sha[:9]} says 'Pure-Move: {claim}': the trailer is 'Pure-Move: verified', or none")
        return 1
    log = c.tmp(f"puremove.{sha}")
    if c.logged(["scripts/pure-move-check", f"{sha}^..{sha}"], log) == 0:
        c.say.log(f"puremove: {sha[:9]} is a pure move")
        return 0
    c.say.raw(c.repo.files.read_text_or(log, ""))
    c.say.warn(f"puremove: {sha[:9]} says 'Pure-Move: verified' but scripts/pure-move-check does not find a pure move: "
               "drop the trailer (and run scripts/identity-what-if) or make the commit one")
    return 1


def stampcarry(c: Check) -> int:
    """A CARRIED STAMP IS PROVEN (the header's `stampcarry`): every carry row the push adds or changes, proven again."""
    if not c.need("stampcarry", "python3", PYTHON_FIX):
        return 1
    return int(any([c.run(["scripts/identity-stamp-carry", "verify", r]) != 0 for r in c.ranges()]))


def served_takes(c: Check) -> List[str]:
    """The takes the newest decided Identity model convergence legs served, as `<dir>/<cc>-takes/<cc>.json.gz` under a
    cache kept per run in the shared git dir; none (warned) when gh cannot fetch them in time. A run that reused an
    earlier run's verdict (scripts/ci/convergence_reuse.py) ran no leg and served nothing: the next one back is read,
    up to three."""
    code, out = c.bounded(c.repo.env.get("PREPUSH_GH_SECONDS") or GH_SECONDS,
                        ["gh", "run", "list", "--workflow", "Identity model convergence", "--branch", "main", "--status",
                         "completed", "--limit", "30", "--json", "databaseId,conclusion", "--jq", SERVED_JQ],
                        machine.PIPE)
    runs = out.split() if code == 0 else []
    if not runs:
        c.say.warn("judgements: no Identity model convergence run to read served takes from — served check SKIPPED")
        return []
    for run in runs:
        found = served_of(c, run)
        if found:
            return found
    c.say.warn(f"judgements: could not fetch the served takes of runs {' '.join(runs)} — served check SKIPPED")
    return []


def served_of(c: Check, run: str) -> List[str]:
    files = c.repo.files
    cache = f"{c.common_dir()}/kinowo-cache/served-takes/{run}"
    if not files.is_dir(cache):
        # Agents push at once and share this cache: each downloads into a directory of its own, and only the first to
        # finish renames it into place (rename(2) onto a filled directory fails; `mv` would nest it inside).
        files.make_directories(cache.rsplit("/", 1)[0])
        part = c.temporary_directory(f"{cache}.part.XXXXXX")
        if not part:
            return []
        fetched = c.bounded("120", ["gh", "run", "download", run, "-p", "convergence-diagnostics-*-convergence",
                                     "-D", part], machine.QUIET)[0]
        if fetched != 0 or not files.rename(part, cache):
            c.quietly(["rm", "-rf", part])
            if fetched != 0:
                return []
    return [p for p in files.under(cache) if re.search(r"-takes/[^/]*\.json\.gz$", p)]


def judgements(c: Check) -> int:
    if not c.need("judgements", "python3", PYTHON_FIX):
        return 1
    served: List[str] = []
    before = ""
    # --fast-only (THE COMBINED FILES): the files' own consistency; each member's own check read the served takes.
    if not c.text("fast_only") and JUDGEMENTS in c.changed():
        if not c.need("judgements", "gh", "brew install gh"):
            return 1
        served = served_takes(c)
        code, text = c.repo.git("show", f"{c.text('base_sha')}:{JUDGEMENTS}")
        if code == 0:
            before = c.tmp("judgements-before.tsv")
            c.repo.files.write_text(before, text)
    # --replay-head: the takes the push newly judges wrong judged at the PUSHED code (identity-slice), not the last leg's
    argv = ["python3", "scripts/ci/identity_judgements_lint.py"]
    argv += ["--served", *served] if served else []
    argv += ["--before", before, "--replay-head"] if before else []
    return c.run(argv)


def ios(c: Check) -> int:
    return c.run(["scripts/ios-compile-check.sh"])


class Light(Protocol):
    """A light check: its status over the run's `Check` (0 passes), its lines said as it goes."""

    def __call__(self, c: Check, /) -> int: ...


CHECKS: Dict[str, Light] = {
    "eslint": eslint, "jsquality": jsquality, "pyquality": pyquality, "drift": drift, "snapshot": snapshot,
    "dashboards": dashboards, "metricalerts": metricalerts, "actionlint": actionlint, "shellcheck": shellcheck,
    # A NEW CHECK GATES ONLY ONCE IT HAS BEEN GREEN (the header's `newchecks`): scripts/ci/gating_checks.py lint
    "newchecks": python_tool("newchecks", "scripts/ci/gating_checks.py", "lint"),
    # THE EXTERNAL SCHEDULER'S SCHEDULE IS THE WORKFLOWS' OWN CRONS (the header's `schedules`)
    "schedules": python_tool("schedules", "infra/nix/files/scheduled_dispatch.py", "check"),
    "allowlists": commit_rules("allowlists"), "driftguards": commit_rules("driftguards"),
    "puremove": puremove, "stampcarry": stampcarry, "judgements": judgements, "ios": ios,
}


def state_of(text: str) -> Dict[str, Value]:
    """The run's state, as pre_push_checks.Python wrote it: a JSON object of strings and lists of strings."""
    loaded: Dict[str, Value] = json.loads(text)
    return loaded
