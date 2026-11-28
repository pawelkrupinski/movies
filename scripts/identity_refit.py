#!/usr/bin/env python3
"""The weekly identity rule refit, whole -- the one script `.github/workflows/identity-refit.yml` runs and a local run
runs the same way (scripts.IdentityRefit; docs/design/identity-resolver.md §14.10):

  1. unit-test the refit (scripts.IdentityRefitSpec);
  2. measure every bounded step of the existing rules' numbers on the unmatched-cluster ratchet and write the kept
     ones into identity/src/main/resources/identity-weights.json (scripts.IdentityRefit --apply), the report to
     --report;
  3. only when the weights changed, the checks a calibration change answers to: the identity specs and the ratchet,
     the hard clusters (the real pipeline over a throwaway Mongo), then the whole-corpus snapshots and the page
     snapshots regenerated (delete, write, re-run to prove them stable) -- the films that moved appended to the
     report (identity_refit_checks.py). A red check fails the tool (in CI: no PR).

Usage: scripts/identity-refit [--skip-checks] [--relearn <signal,…>] [--threads <n>] [--report <file.md>] [--merge <dir>] [--help]
       scripts/identity-refit --shard <i/n> --shard-out <file.json> [--relearn …] [--threads <n>]

  --skip-checks   measure (and write the kept steps) only; no step 3
  --relearn       listing-film signals whose inputs changed, refitted from the fresh units alone (year.delta,…)
  --threads       proposals measured at once (default: every core; each holds a re-resolve of the five captures)
  --report        where the report goes (default target/identity-refit/report.md); the measure's log beside it
  --shard i/n     measure only round 1's proposals i, i+n, … (no spec, no report, no weights written) into
                  --shard-out: one of n runners measuring round 1 side by side (.github/workflows/identity-refit.yml)
  --merge dir     round 1 answered by the shard files in dir, the rest as ever: the report and the weights byte for
                  byte an unsharded run's (scripts.IdentityRefit refuses shards that measured another refit)

TMDB: the captures' unanswered TMDB questions are asked live only with TMDB_API_KEY in the ENVIRONMENT -- never read
from .env.local. Locally: `secrets run movies TMDB_API_KEY -- scripts/identity-refit`. Without it a proposal that
asks TMDB something is unmeasured (never kept).

Locally sbt runs through scripts/sbt-warm, the measure in its own JVM (IDENTITY_REFIT_HEAP, default 12g) on the
classpath sbt exports, and the hard clusters on a mongod of the run's own (scripts/it-local.sh). In CI (CI set)
plain sbt, and the hard clusters on the Mongo the workflow started (MONGODB_URI). It refuses a dirty tree and the
root checkout: its results are changed files in THIS worktree -- the weights, the snapshots -- and the report.

Exit: 0 done; 1 refused or a step failed (a failed measure or check: its own status); 2 asked wrongly."""
from __future__ import annotations

import os
import re
import sys
import tempfile
from dataclasses import dataclass
from typing import Dict, List, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(HERE, "lib")]
from processes import machine  # noqa: E402
from identity_tooling.ci import identity_jvm  # noqa: E402
import identity_refit_checks  # noqa: E402
from cli import tool_actions  # noqa: E402
from cli import tool_cli  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402

ROOT = os.path.dirname(HERE)
TOOL = "identity-refit"
WEIGHTS = "identity/src/main/resources/identity-weights.json"
CLASSPATH_LINE = re.compile(r"^/[^ ]*(\.jar|classes|resources)(:|$)")
ANSI = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
VALUED = {"--relearn": "relearn", "--threads": "threads", "--report": "report", "--shard": "shard",
          "--shard-out": "shard_out", "--merge": "merge"}


@dataclass
class Options:
    skip_checks: bool = False
    relearn: str = ""
    threads: str = ""
    report: str = "target/identity-refit/report.md"
    shard: str = ""
    shard_out: str = ""
    merge: str = ""


def word(o: Options, argv: List[str], i: int) -> int:
    if argv[i] == "--skip-checks":
        o.skip_checks = True
        return i
    if argv[i] in VALUED:
        setattr(o, VALUED[argv[i]], tool_cli.value_at(argv, i))
        return i + 1
    raise tool_cli.Refused(f"{TOOL}: unknown argument '{argv[i]}'", with_usage=True)


def options(argv: List[str]) -> Optional[Options]:
    o = tool_cli.read_options(Options(), argv, usage(), word)
    if o is not None and o.shard and not o.shard_out:
        raise tool_cli.Refused(f"{TOOL}: --shard needs --shard-out <file.json>")
    return o


def usage() -> str:
    return (__doc__ or "") + "\n"


class Refit:
    def __init__(self, o: Options, env: Dict[str, str], runner: tool_process.Runner) -> None:
        self.o, self.env, self.runner = o, env, runner
        self.ci = bool(env.get("CI"))
        self.git = tool_git.Git(runner, env, ".")
        self.checks = identity_refit_checks.Checks(runner, env, self.git, self.sbt_run)

    def sbt_run(self, commands: Sequence[str], log_path: Optional[str] = None) -> int:
        """sbt: through scripts/sbt-warm locally, plain batch sbt in CI; its output into `log_path` when given."""
        argv = ["sbt", "-batch", *commands] if self.ci else ["scripts/sbt-warm", *commands]
        return self.runner.run(tool_process.Command(argv, self.env, log=log_path))

    def refused(self) -> Optional[str]:
        """Why this checkout cannot take the refit's results: a dirty tree, or the root checkout."""
        if self.ci:
            return None
        if (self.git.out("status", "--porcelain") or "").strip():
            return "the tree is dirty: commit or stash first — the refit's results are the changes it leaves"
        common = os.path.realpath((self.git.out("rev-parse", "--git-common-dir") or ".git").strip())
        if os.path.realpath(".") == os.path.dirname(common):
            return ("this is the root checkout: run it in a worktree of your own "
                    "(git worktree add -b <branch> <path> origin/main)")
        return None

    def classpath(self) -> Optional[str]:
        """Step 1: the refit's own spec, and the classpath the measure runs on -- one sbt call (a shard only the
        classpath: the run that merges it tests the refit, and a shard's proposals are checked against that run's)."""
        commands = ["export worker/Test/fullClasspath"]
        if self.o.shard:
            log(f"shard {self.o.shard}: the classpath")
        else:
            log("unit-testing the refit")
            commands.insert(0, "worker/testOnly scripts.IdentityRefitSpec")
        with tempfile.NamedTemporaryFile(prefix="identity-refit-sbt.", mode="r", errors="replace") as out:
            exported = self.sbt_run(commands, out.name)
            lines = out.read().splitlines()
        if exported != 0:
            return die_with(lines[-60:], "the refit's spec failed (above): its proposals are not trusted")
        for line in lines:
            if re.search(r"Tests:|All tests passed", line):
                print(line, file=sys.stderr)
        found = [line for line in (ANSI.sub("", line) for line in lines) if CLASSPATH_LINE.search(line)]
        return found[-1] if found else die_with(lines[-20:], "sbt exported no worker/Test classpath (above)")

    def java(self, classpath: str, args: Sequence[str], before: Sequence[str] = ()) -> List[str]:
        """The measure's JVM: its own heap, headless and en-US, on the exported classpath."""
        heap = f"-Xmx{self.env.get('IDENTITY_REFIT_HEAP') or '12g'}"
        relearn = ["--relearn", self.o.relearn] if self.o.relearn else []
        return ["java", heap, "-XX:AutoBoxCacheMax=16384", *identity_jvm.JAVA_OPTIONS, *before, "-cp", classpath,
                "scripts.IdentityRefit", *args, "--threads", self.o.threads, *relearn]

    def measure(self, classpath: str) -> int:
        """Step 2: the measure, in its own JVM, the processor named (hosted runners differ in it, and a run's time is
        read against it). A shard measures round 1's share and stops there."""
        on = f"on {self.o.threads} thread(s) of {os.cpu_count()} processors ({self.cpu() or 'cpu unknown'})"
        if self.o.shard:
            log(f"measuring round 1, shard {self.o.shard}, {on}")
            shard = ["--shard", self.o.shard, "--shard-out", self.o.shard_out]
            return self.runner.run(tool_process.Command(self.java(classpath, shard), self.env))
        report_dir = os.path.dirname(self.o.report)
        measure_log = os.path.join(report_dir, "refit.log")
        # every collection with its pause and CPU, to tell a thread count's GC cost
        gc = f"-Xlog:gc,gc+cpu:file={os.path.join(report_dir, 'refit-gc.log')}:uptime"
        merge = ["--merge", self.o.merge] if self.o.merge else []
        log(f"measuring every step {on} — the log in {measure_log}")
        argv = self.java(classpath, ["--apply", "--report", self.o.report], [gc])
        return self.runner.run(tool_process.Command([*argv, *merge], self.env, log=measure_log, tee=True))

    def cpu(self) -> str:
        lscpu = self.runner.capture(tool_process.Command(["lscpu"], self.env))
        for line in lscpu.stdout.splitlines() if lscpu.code == 0 else []:
            if line.startswith("Model name:"):
                return line[len("Model name:"):].strip()
        brand = self.runner.capture(tool_process.Command(["sysctl", "-n", "machdep.cpu.brand_string"], self.env))
        return brand.stdout.strip().splitlines()[0] if brand.code == 0 and brand.stdout.strip() else ""

    def main(self) -> int:
        why = self.refused()
        if why:
            print(f"{TOOL}: {why}", file=sys.stderr)
            return 1
        self.o.threads = self.o.threads or str(os.cpu_count() or 1)
        os.makedirs(os.path.dirname(self.o.report) or ".", exist_ok=True)
        self.say_tmdb()
        classpath = self.classpath()
        if classpath is None:
            return 1
        measured = self.measure(classpath)
        if measured != 0 or self.o.shard:
            return measured
        tool_actions.Actions(self.env, sys.stdout).summary(identity_refit_checks.text(self.o.report))
        if self.git.ok("diff", "--quiet", "--", WEIGHTS):
            log(f"nothing kept: identity-weights.json unchanged, no checks to run. Report: {self.o.report}")
            return 0
        if self.o.skip_checks:
            log(f"kept changes written to {WEIGHTS}; checks skipped (--skip-checks). Report: {self.o.report}")
            return 0
        return self.checks.run(self.o.report)

    def say_tmdb(self) -> None:
        if self.env.get("TMDB_API_KEY"):
            log("TMDB: asked live (TMDB_API_KEY from the environment)")
        else:
            log("TMDB: no TMDB_API_KEY — a proposal asking TMDB anything is unmeasured "
                f"(locally: secrets run movies TMDB_API_KEY -- {sys.argv[0]})")


def log(line: str) -> None:
    identity_refit_checks.log(line)


def die_with(lines: List[str], why: str) -> None:
    """sbt's last lines and why the refit stops, on stderr."""
    for line in lines:
        print(line, file=sys.stderr)
    print(f"{TOOL}: {why}", file=sys.stderr)


def main(argv: List[str]) -> int:
    code, o = tool_cli.parse_call(TOOL, usage(), lambda: options(argv))
    if o is None:
        return code or 0
    os.chdir(ROOT)
    return Refit(o, dict(machine.environment()), tool_process.SubprocessRunner()).main()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
