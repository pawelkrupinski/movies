#!/usr/bin/env python3
"""THE IDENTITY BOOT-COST CHECK: what the identity model allocates and spends a family -- a cold resolve, the worker's
take-up re-resolving every family after a rules change, and a take-up with nothing moved -- over the pinned identity
slices (scripts.IdentityBootCost, worker's test tree), held to test/resources/identity-boot-cost-baseline.tsv
(scripts/identity_boot_cost.py). Run it on any change to the resolver's hot paths or the boot re-resolve, BEFORE
landing: on 2026-11-20 ~17-20 MB a family of boot allocation was found only after deploy, in production's GC.

  scripts/identity-boot-cost [--update] [--runs N] [--out DIR]

The measure runs in a JVM of its own on worker's test classpath (from this checkout's kept sbt server), without
escape analysis (`-XX:-DoEscapeAnalysis`: the bytes are the code's, not the JIT's state's) and with the worker's
boxed-integer cache (`-XX:AutoBoxCacheMax=16384`, infra/jvm/worker.options), under one governor ticket
(scripts/gov-run --job identity-boot-cost). Not part of testUnit or the pre-push hook: ~2-4 minutes, on demand.

  --update    record the measure as the baseline (its tolerance row kept): after a deliberate change, with the
              figures in the commit message
  --runs N    measured passes a phase, the median kept (default 3)
  --out DIR   keep rows.tsv, the sbt and JVM logs in DIR (default: a temporary directory)

Exit 0 within the baseline (or updated); 1 a phase's bytes a family rose past the tolerance; 2 the measure failed."""
from __future__ import annotations

import os
import re
import shutil
import sys
import tempfile
from dataclasses import dataclass
from typing import List, Mapping, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import machine  # noqa: E402
import tool_cli  # noqa: E402
import tool_process  # noqa: E402
import tool_python  # noqa: E402

TOOL = "identity-boot-cost"
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
PURPOSE = "The identity boot-cost check: what the identity model allocates and spends a family"
JVM_FLAGS = ("-XX:-DoEscapeAnalysis", "-XX:AutoBoxCacheMax=16384", "-Xmx2g", "-Djava.awt.headless=true",
             "-Duser.language=en", "-Duser.country=US")


class NoVerdict(Exception):
    """The measure failed: its line said after the tool's name, the exit 2."""


@dataclass
class Options:
    update: bool = False
    runs: str = "3"
    out: str = ""


def usage() -> str:
    """The header as the shell tool's usage_from_header printed it: its Purpose line first."""
    return f"Purpose: {PURPOSE}\n{__doc__}\n"


def word(o: Options, argv: List[str], i: int) -> int:
    if argv[i] == "--update":
        o.update = True
        return i
    if argv[i] in ("--runs", "--out"):
        setattr(o, argv[i][2:], tool_cli.value_at(argv, i))
        return i + 1
    raise tool_cli.UsageError(f"unknown argument {argv[i]}")


def parsed(argv: List[str]) -> Optional[Options]:
    o = tool_cli.read_options(Options(), argv, usage(), word)
    if o is not None and (not re.fullmatch(r"[0-9]+", o.runs) or o.runs == "0"):
        raise tool_cli.UsageError(f"--runs wants a whole number above 0, not '{o.runs}'")
    return o


def read(path: str) -> str:
    with open(path, encoding="utf-8", errors="replace") as f:
        return f.read()


def tail(path: str, lines: int) -> str:
    """`tail -n <lines>`: the file's last lines, as they end."""
    return "".join(read(path).splitlines(keepends=True)[-lines:])


class BootCost:
    def __init__(self, env: Mapping[str, str], runner: tool_process.Runner, out: str) -> None:
        self.env, self.runner, self.out = env, runner, out

    def logged(self, argv: List[str], log: str, failed: str) -> None:
        """`argv` from the checkout, its output in `log`; NoVerdict, the log's tail shown, when it failed."""
        path = os.path.join(self.out, log)
        if self.runner.run(tool_process.Command(argv, self.env, log=path, cwd=ROOT)) != 0:
            sys.stderr.write(tail(path, 30))
            raise NoVerdict(f"{failed} (log: {path})")

    def classpath(self) -> str:
        """worker's test classpath, compiled and exported by the checkout's sbt."""
        sbt = self.env.get("KINOWO_BOOT_COST_SBT") or os.path.join(ROOT, "scripts", "sbt-warm")
        self.logged([sbt, "worker/Test/compile", "export worker/Test/fullClasspath"], "sbt.log",
                    "sbt worker/Test/compile failed")
        exported = [line for line in read(os.path.join(self.out, "sbt.log")).splitlines() if line.startswith("/")]
        if not exported:
            raise NoVerdict(f"no classpath in sbt's export (log: {self.out}/sbt.log)")
        return exported[-1]

    def measure(self, o: Options) -> int:
        """The rows measured in a JVM of their own, under one governor ticket, then held to the baseline."""
        java = self.env.get("KINOWO_BOOT_COST_JAVA") or "java"
        rows = os.path.join(self.out, "rows.tsv")
        self.logged([os.path.join(ROOT, "scripts", "gov-run"), "--job", TOOL, "--cores", "1", "--mem", "3", "--",
                     java, *JVM_FLAGS, "-cp", self.classpath(), "scripts.IdentityBootCost", rows, o.runs], "jvm.log",
                    "the measure failed")
        sys.stdout.write(tail(os.path.join(self.out, "jvm.log"), 1))
        sys.stdout.flush()
        baseline = (self.env.get("KINOWO_BOOT_COST_BASELINE")
                    or os.path.join(ROOT, "test", "resources", "identity-boot-cost-baseline.tsv"))
        ratchet = tool_python.isolated(os.path.join(HERE, "identity_boot_cost.py"), "update" if o.update else "check",
                                       "--current", rows, "--baseline", baseline)
        return self.runner.run(tool_process.Command(ratchet, self.env, cwd=ROOT))


def own_checkout(runner: tool_process.Runner, env: Mapping[str, str]) -> bool:
    """It measures its own checkout, never another one's (scripts/own-checkout, which says which copy to run)."""
    guard = [os.path.join(HERE, "own-checkout"), os.path.join(HERE, TOOL)]
    return runner.run(tool_process.Command(guard, env)) == 0


def checked(o: Options, env: Mapping[str, str], runner: tool_process.Runner) -> int:
    out = o.out or tempfile.mkdtemp(dir=env.get("TMPDIR") or None)
    try:
        os.makedirs(out, exist_ok=True)
        return BootCost(env, runner, os.path.abspath(out)).measure(o)
    except NoVerdict as e:
        print(f"{TOOL}: {e}", file=sys.stderr)
        return 2
    finally:
        if not o.out:
            shutil.rmtree(out, ignore_errors=True)


def main(argv: List[str], env: Mapping[str, str]) -> int:
    runner = tool_process.SubprocessRunner()
    if not own_checkout(runner, env):
        return 2
    code, o = tool_cli.parse_call(TOOL, "", lambda: parsed(argv))
    return checked(o, env, runner) if o is not None else code or 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment()))
