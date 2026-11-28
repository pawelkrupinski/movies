#!/usr/bin/env python3
"""Re-capture the unmatched-cluster fixture (test/resources/fixtures/identity-unmatched/<cc>.json.gz)
in one command — docs/design/identity-resolver.md §20.14 item 5.

  scripts/identity-capture.sh [--dry-run] [--capture|--fill] [--parallel 2] [--heap 12g] [cc...]

  cc...       countries to capture (default: us de uk pl es, run largest first)
  --capture   capture every country named, whatever its fixture's inputs
  --fill      fill every country named (its <cc>.json.gz must exist)
  --parallel  countries run side by side, one JVM each (default 2; fewer when memory holds fewer
              12g heaps beside 4 GB for the rest), sharing 4 live reads per host between them
  --heap      each country's JVM heap (default 12g)
  --dry-run   print the plan — what it would download, export and run, with every variable — and
              touch neither prod nor the network

CAPTURE OR FILL, per country, said and logged: a FILL (integration.UnmatchedClustersFill, minutes)
answers only the questions a change newly asks of the fixture's clusters; a CAPTURE resolves the
whole corpus again. It fills when <cc>.json.gz exists and its decisions are unchanged under the
current code — <cc>.inputs, stamped by each capture, names the same recording and the same hash of
the code deciding them (scripts.IdentityCapture.DecisionInputs) — and captures otherwise.

What a capture does (scripts.IdentityCapture, worker/src/test/scala/scripts, does the work):
  1. finds the newest successful "Record scrape fixtures" run (identity-tools.sh's identity_newest_recording, passed
     on as $KINOWO_IDENTITY_NEWEST_RECORDING) and fetches its corpora (artifact
     scrape-fixtures-<cc>) and the enrichment trees recorded with them (release convergence-fixtures,
     enrichment-<cc>-<run>.tar.*) into $KINOWO_IDENTITY_CAPTURE_WORK (default target/identity-capture),
     skipping a country whose download is already that run's;
  2. exports prod's identity_family_answers (kinowo, kinowo_<cc>) READ-ONLY over the prod tunnel
     (scripts/local-mirror/prod-tunnel.sh's prod_tunnel_run; the URI is the Touch ID vault's MONGODB_URI, unless
     exported, rewritten to the tunnel and never printed) — or over
     $KINOWO_IDENTITY_FAMILY_URI where the caller (CI) already holds a read-only route;
  3. runs each country's integration.UnmatchedClustersCapture in a JVM of its own, against the
     it/ Mongo (127.0.0.1:28017) in a database of its own, every directory defaulted (a variable you
     set is kept), the TMDB key read from the Touch ID vault (TMDB_API_KEY) unless
     KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY is set — both secrets in ONE `secrets run` (one Touch ID):
     the tool re-runs itself under it before compiling, naming only the keys still missing; a country whose capture
     read venue pages live ("capture again") is captured again to answer them, while each capture reads more (at most
     4 captures);
  4. prints each phase's time and throughput, and exits non-zero, naming them, when any country's JVM
     failed, was killed or printed no capture.

Then judge every new take into identity-judgements.tsv and re-baseline expected-matches.tsv (the ratchet,
worker/src/test/scala/services/identity/UnmatchedClustersRatchetSpec.scala)."""
from __future__ import annotations

import os
import re
import sys
from dataclasses import dataclass, field
from typing import Dict, List, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, os.path.join(HERE, "lib"))
sys.path.insert(0, HERE)
from processes import machine  # noqa: E402
import tool_process  # noqa: E402
from processes import tool_shell  # noqa: E402
import vault_env  # noqa: E402

TAG = "[identity-capture]"
FAMILY = "KINOWO_IDENTITY_FAMILY_URI"
PROD = "KINOWO_PROD_MONGODB_URI"
NEWEST = "KINOWO_IDENTITY_NEWEST_RECORDING"
TMDB_SPEC = "KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY=TMDB_API_KEY"
# Under its own name, NOT MONGODB_URI: the capture JVMs inherit this environment, and a MONGODB_URI there replaces
# each one's local throwaway database (IdentityCapture.environment).
PROD_SPEC = f"{PROD}=MONGODB_URI"
CAPTURE_CLASSES = re.compile(r"/worker/target/[^/]+/test-classes$")


@dataclass
class Options:
    dry: bool = False
    fill: bool = False
    help: bool = False
    java: List[str] = field(default_factory=lambda: [])


def options(argv: List[str]) -> Options:
    """Every argument is the JVM's, as given (a --heap / --parallel value too); the tool reads its own few."""
    o = Options(java=list(argv))
    o.dry = "--dry-run" in argv
    o.fill = "--fill" in argv
    o.help = any(arg in ("-h", "--help") for arg in argv if arg not in values_of(argv))
    return o


def values_of(argv: List[str]) -> List[str]:
    return [argv[i + 1] for i, arg in enumerate(argv[:-1]) if arg in ("--heap", "--parallel")]


class Capture:
    def __init__(self, o: Options, env: Dict[str, str], runner: tool_process.Runner) -> None:
        self.o, self.env, self.runner = o, env, runner
        self.reads_prod = not o.dry and not o.fill and not env.get(FAMILY) and not env.get("KINOWO_IDENTITY_FAMILY_SEED")
        self.work = ""

    def secrets(self, argv: List[str]) -> bool:
        """The secrets, up front and together, so a missing one costs one Touch ID before the compile rather than a
        failure after it; this tool run again under `secrets run` when one is missing (it does not return then)."""
        if self.o.dry:
            return True
        needed = [TMDB_SPEC, *([PROD_SPEC] if self.reads_prod else [])]
        if vault_env.exec_under_vault(needed, [sys.executable, os.path.abspath(__file__), *argv]):
            return True
        names = " ".join(spec.split("=", 1)[0] for spec in vault_env.missing_specs(needed))
        print(f"[vault] {names} not set, and no `secrets` CLI on PATH to read it from the vault", file=sys.stderr)
        return False

    def newest(self) -> None:
        """The newest recording, found by the shared helper and handed to scripts.IdentityCapture. A lookup that fails
        leaves it unset: the JVM keeps the downloads it has and fails a country with none."""
        if self.o.dry:
            return
        done = tool_shell.shell_function(self.runner, self.env, "scripts/identity-tools.sh",
                                         "identity_newest_recording", [])
        if done.code == 0:
            self.env[NEWEST] = done.stdout.strip()
        else:
            self.env.pop(NEWEST, None)
            print(f"{TAG} the newest recording could not be looked up (above)", file=sys.stderr)

    def classpath(self) -> Optional[str]:
        """The integration classpath, compiled once: every country's JVM runs on it. sbt's whole output goes to a log,
        and only a line that IS a classpath is kept (2026-10-12: `java -cp` handed whatever sbt printed last)."""
        print(f"{TAG} compiling the integration classpath", flush=True)
        log = os.path.join(self.work, "sbt.log")
        compiled = self.runner.run(tool_process.Command(
            ["sbt", "-batch", "worker/IntegrationTest/compile", "export worker/IntegrationTest/fullClasspath"],
            self.env, log=log))
        if compiled != 0:
            return self.failed(log, "sbt failed to compile the integration classpath (log above)")
        line = tool_shell.shell_function(self.runner, self.env, "scripts/identity-tools.sh", "identity_classpath_line",
                                         [log]).stdout.strip()
        with open(os.path.join(self.work, "classpath"), "w") as f:
            f.write(line + "\n" if line else "")
        classes = [entry for entry in line.split(":") if CAPTURE_CLASSES.search(entry)]
        if not classes or not os.path.isfile(os.path.join(classes[0], "scripts", "IdentityCapture.class")):
            return self.failed(log, "no classpath holding scripts.IdentityCapture in sbt's output (log above)")
        return line

    @staticmethod
    def failed(log: str, why: str) -> None:
        with open(log, errors="replace") as f:
            sys.stderr.write(f.read())
        print(f"{TAG} {why}", file=sys.stderr)

    def capture(self, classpath: str) -> int:
        """The capture JVM; prod reached by it only as the family export's route, through a tunnel of this run's."""
        java = ["java", "-Xmx2g", "-Djava.awt.headless=true", "-cp", classpath, "scripts.IdentityCapture", *self.o.java]
        if not self.reads_prod:
            return self.runner.run(tool_process.Command(java, self.env))
        uri = self.env.pop(PROD)
        return self.runner.run(tool_shell.shell_command(self.env, "scripts/local-mirror/prod-tunnel.sh",
                                                        "prod_tunnel_run", ["identity-capture", uri, FAMILY, *java]))

    def main(self, argv: List[str]) -> int:
        if not self.secrets(argv):
            return 2
        self.work = os.path.abspath(self.env.get("KINOWO_IDENTITY_CAPTURE_WORK") or "target/identity-capture")
        os.makedirs(self.work, exist_ok=True)
        self.newest()
        classpath = self.classpath()
        return 1 if classpath is None else self.capture(classpath)


def main(argv: List[str]) -> int:
    o = options(argv)
    if o.help:
        print(__doc__)
        return 0
    os.chdir(ROOT)
    return Capture(o, dict(machine.environment()), tool_process.SubprocessRunner()).main(argv)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
