#!/usr/bin/env python3
"""Run a country's convergence leg locally against the SAME recorded inputs CI uses.

  scripts/convergence-local.sh --i-need-a-local-leg <pl|de|uk|us|es> [sbt alias, default the full leg's]

REFUSED WITHOUT --i-need-a-local-leg. A leg runs tens of minutes and loads the whole Mac, and most agents ran one only
to see which takes a change moved: on 2026-10-15 agents spent ~31 of 219 agent-hours polling local legs. That
question has faster tools -- `scripts/identity-slice` (seconds, per listing) and `scripts/identity-measure` (the whole
corpus in CI, ~8 min). The flag is for what only the pipeline itself answers (scrape, enrichment, projection), said on
purpose.

CI's verdict legs are HERMETIC: they replay the corpus + enrichment-tree PAIR `Record scrape fixtures` pinned
(`hermetic-<code>.txt` in the `convergence-fixtures` release) and never touch the network. `restore-country-corpus.sh`
lays that pair into this checkout -- downloaded every time, deliberately: the pin moves nightly, and a stale local copy
would test different inputs from the ones CI tested while looking fine. Then the suite runs hermetically, so a local
run answers exactly the question the CI leg did, offline, failing by name on anything the pair lacks. There is no
fallback: no pinned pair, and this stops.

TMDB_API_KEY is the one input nothing replays around (TmdbClient short-circuits without it). It comes from the
environment if set, else from the Touch ID vault, INLINE into sbt's env only -- never the vault's MONGODB_URI, which is
production's. OMDB_API_KEY likewise, optional (CI has it; without it the OMDb rung of ImdbIdResolver is absent).

Every collection runs on a real MongoDB replica set (change streams and the staging fold's transaction both need one).
MONGODB_URI if given; else the itAll mongod on :28017 if it answers; else a throwaway mongo:8.3.17 container of this
run's own (convergence-local-mongo-<pid>, on a free port, or CONVERGENCE_LOCAL_MONGO_PORT -- whatever already answers
there is reused, never replaced), waited for at most CONVERGENCE_LOCAL_MONGO_WAIT seconds (120) a step and removed when
sbt ends or the leg is ended by INT, TERM or HUP (only a KILL leaves it: `docker rm -f convergence-local-mongo-<pid>`); the sbt of such a leg runs in a group of its own, ended with it, and at most
CONVERGENCE_LOCAL_SBT_TIMEOUT seconds (6 hours). Each run's
databases are created under a MONGODB_DB prefix unique to it and dropped by the suite (`ConvergenceStorage.close`).
CONVERGENCE_LOCAL_PAIR=<recording run | laid fixtures dir> lays a pair already on this machine instead (offline: GitHub
unreachable), said as not necessarily the pin CI holds. CONVERGENCE_LOCAL_OWN_MONGOD=1 runs the leg on a mongod of its
own (scripts/it-local.sh), removed however it ends. Tested by scripts/convergence_local_test.py against fake sbt,
docker, secrets and restore."""
from __future__ import annotations

import glob
import os
import shutil
import signal
import socket
import sys
import time
from types import FrameType
from typing import Callable, Dict, List, Mapping, NoReturn, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import machine  # noqa: E402
import mongo_local  # noqa: E402
from tool_process import Command, Runner, SubprocessRunner  # noqa: E402

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FLAG = "--i-need-a-local-leg"
SUITES = {"pl": "convergencePoland", "de": "convergenceGermany", "uk": "convergenceUk", "us": "convergenceUs",
          "es": "convergenceSpain"}
CONTAINER_PREFIX = "convergence-local-mongo"  # + "-<pid>": one container a run, never shared with another agent's leg
IMAGE = "mongo:8.3.17"  # the server release production runs (MongoServerVersionParitySpec; infra/bin/nix-update moves it)
FASTER_TOOLS = """  Which takes a change moves:  scripts/identity-slice (--ref A --ref B, --batch, --explain), seconds per listing
  The whole corpus, base vs head: scripts/identity-measure (CI, ~8 min, LANDABLE / NOT LANDABLE)
"""


class Stop(Exception):
    """The leg ends here, with `code` (its reason already said)."""

    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


def say(text: str) -> None:
    print(text, flush=True)


def warn(text: str) -> None:
    sys.stderr.write(text + "\n")
    sys.stderr.flush()


class Leg:
    def __init__(self, runner: Runner, env: Mapping[str, str], code: str, suite: str,
                 sleep: Callable[[float], None] = time.sleep) -> None:
        self.runner = runner
        self.env: Dict[str, str] = dict(env)
        self.code = code
        self.suite = suite
        self.sleep = sleep
        self.container = f"{CONTAINER_PREFIX}-{os.getpid()}"
        self.started_container = False

    def lay_pair(self) -> None:
        laid = self.env.get("CONVERGENCE_LOCAL_PAIR", "")
        if laid:
            self.lay_cached(laid)
        else:
            restored = self.runner.run(Command(["scripts/restore-country-corpus.sh", self.code], self.env))
            if restored != 0:
                raise Stop(restored)
        say(f"    (overwrites any committed corpus for {self.code} — "
            "'git checkout -- test/resources/fixtures/corpus' restores it)")

    def lay_cached(self, laid: str) -> None:
        """OFFLINE: a pair already laid on this machine -- a recording run in the identity cache
        ($KINOWO_IDENTITY_CACHE/recordings/<run>/pair, as identity-slice lays it) or a laid fixtures directory -- for
        when GitHub, which pins and serves the pair, cannot be reached. It is the pair of THAT recording, not
        necessarily the one CI pins now."""
        pair = laid
        if not os.path.isdir(pair):
            cache = self.env.get("KINOWO_IDENTITY_CACHE") or os.path.join(os.path.expanduser("~"), ".cache",
                                                                          "kinowo-identity")
            pair = os.path.join(cache, "recordings", laid, "pair", "test", "resources", "fixtures")
        code = self.code
        if not (os.path.isfile(f"{pair}/corpus/cinema-scrapes-{code}.json.gz") and os.path.isdir(f"{pair}/enrichment-{code}")):
            warn(f"convergence-local: no laid pair for {code} at {pair}")
            raise Stop(1)
        say(f"==> laying the cached pair {pair} for {code} (offline: not the pin CI holds now, unless that recording is it)")
        os.makedirs("test/resources/fixtures/corpus", exist_ok=True)
        for pattern in (f"*-{code}.json*", f"*-{code}-sample.json*"):
            for file in sorted(glob.glob(os.path.join(pair, "corpus", pattern))):
                if os.path.isfile(file):
                    shutil.copy(file, "test/resources/fixtures/corpus/")
        tree = f"test/resources/fixtures/enrichment-{code}"
        shutil.rmtree(tree, ignore_errors=True)
        shutil.copytree(f"{pair}/enrichment-{code}", tree, symlinks=True)

    def vault(self, key: str) -> str:
        return self.runner.capture(Command(["secrets", "get", "movies", key], self.env)).stdout.rstrip("\n")

    def keys(self) -> None:
        if not self.env.get("TMDB_API_KEY"):
            self.env["TMDB_API_KEY"] = self.vault("TMDB_API_KEY")
            if not self.env["TMDB_API_KEY"]:
                warn("no TMDB_API_KEY in the environment or the vault (secrets get movies TMDB_API_KEY) — "
                     "the leg would resolve nothing")
                raise Stop(1)
        self.env["OMDB_API_KEY"] = self.env.get("OMDB_API_KEY") or self.vault("OMDB_API_KEY")
        if not self.env["OMDB_API_KEY"]:
            warn("    no OMDB_API_KEY — the OMDb rung runs absent, as it does not in CI")

    def hermetic(self) -> Dict[str, str]:
        return {**self.env, "KINOWO_COUNTRY": self.code, "KINOWO_COUNTRIES": self.code,
                "KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES": f"enrichment-{self.code}",
                "KINOWO_CONVERGENCE_HERMETIC": "true"}

    def mongo_uri(self) -> str:
        fallback = self.env.get("CONVERGENCE_LOCAL_MONGO_PORT") or free_port()
        if self.env.get("MONGODB_URI"):
            say("==> MongoDB from MONGODB_URI")
            return self.env["MONGODB_URI"]
        if mongo_local.answers(mongo_local.local_mongo_port(self.env)):
            say(f"==> reusing the itAll MongoDB on :{mongo_local.local_mongo_port(self.env)}")
            return mongo_local.local_mongo_uri(self.env)
        if mongo_local.answers(fallback):
            # REUSE whatever already answers on the port: `docker run -p` fails outright when it is taken.
            say(f"==> reusing the MongoDB already listening on :{fallback}")
        else:
            self.throwaway(fallback)
        return f"mongodb://127.0.0.1:{fallback}/?directConnection=true"

    def docker_bound(self) -> float:
        """A docker call that stalls (a wedged Docker Desktop) ends at this many seconds, as a failure."""
        return float(self.env.get("CONVERGENCE_LOCAL_DOCKER_TIMEOUT") or "60")

    def docker(self, *args: str) -> int:
        done = self.runner.capture(Command(["docker", *args], self.env, timeout=self.docker_bound()))
        return done.code

    def throwaway(self, port: str) -> None:
        say(f"==> starting a throwaway {IMAGE} single-node replica set on :{port}")
        self.started_container = True
        started = self.runner.capture(Command(["docker", "run", "-d", "--name", self.container, "-p", f"{port}:27017",
                                               IMAGE, "--replSet", "rs0", "--bind_ip_all"], self.env,
                                              timeout=15 * 60))  # an image pull is in it
        sys.stderr.write(started.stderr)
        if started.code != 0:
            raise Stop(started.code)
        self.until_mongo("ping", lambda: self.mongosh("db.runCommand({ping:1})") is not None)
        initiate = mongo_local.throwaway_replica_set_initiate("127.0.0.1:27017")
        if self.mongosh(f"try {{ rs.status().ok }} catch (e) {{ {initiate} }}", quiet_errors=False) is None:
            raise Stop(1)
        self.until_mongo("a primary", lambda: "1" in (self.mongosh("rs.status().myState") or "").splitlines())

    def mongosh(self, expression: str, quiet_errors: bool = True) -> Optional[str]:
        """The container's mongosh evaluating `expression`: its stdout, or None when it failed."""
        done = self.runner.capture(Command(["docker", "exec", self.container, "mongosh", "--quiet", "--eval", expression],
                                           self.env, timeout=self.docker_bound()))
        if not quiet_errors:
            sys.stderr.write(done.stderr)
        return done.stdout if done.code == 0 else None

    def remove_container(self) -> None:
        if self.started_container:
            self.docker("rm", "-f", self.container)

    def until_mongo(self, what: str, ready: Callable[[], bool]) -> None:
        """Each wait has a deadline: a container that never comes up (an image pull refused, a crash loop) used to hold
        the leg forever, printing nothing."""
        bound = int(self.env.get("CONVERGENCE_LOCAL_MONGO_WAIT") or "120")
        deadline = machine.monotonic() + bound  # wall time: a call that stalls is part of the wait
        while not ready():
            if machine.monotonic() >= deadline:
                warn(f"the throwaway MongoDB never answered ({what}) in {bound}s — docker logs {self.container}")
                raise Stop(1)
            self.sleep(1)


def free_port() -> str:
    """A port nothing listens on (the kernel's pick), for this run's own container."""
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        return str(probe.getsockname()[1])


def run_leg(leg: Leg) -> NoReturn:
    leg.lay_pair()
    leg.keys()
    if leg.env.get("CONVERGENCE_LOCAL_OWN_MONGOD"):
        # A mongod of this leg's own (scripts/it-local.sh): its data in a temp directory, killed and removed however
        # the leg ends, where a shared server keeps a crashed leg's databases (the suite's close drops them).
        say(f"==> sbt {leg.suite} on a mongod of its own (scripts/it-local.sh: removed when the leg ends, however it ends)")
        execute(["scripts/it-local.sh", leg.suite], leg.hermetic())
    uri = leg.mongo_uri()
    database = leg.env.get("MONGODB_DB") or f"kinowo_conv_{leg.code}_{os.getpid()}"
    say(f"    databases under the prefix {database}")
    say(f"==> sbt {leg.suite}")
    say(f"    watch: '[{leg.code}] <phase> done in Ns', 'scraped N/M', 'staging round N: A → B rows', "
        "'coverage — N films'")
    environment = {**leg.hermetic(), "MONGODB_URI": uri, "MONGODB_DB": database}
    if not leg.started_container:
        execute(["sbt", leg.suite], environment)
    bound = float(leg.env.get("CONVERGENCE_LOCAL_SBT_TIMEOUT") or str(6 * 3600))
    # a container of ours: sbt must not replace us, and runs in a group of its own that a signal to us ends with us
    raise Stop(leg.runner.run(Command(["sbt", leg.suite], environment, timeout=bound)))


def execute(argv: List[str], env: Mapping[str, str]) -> NoReturn:
    sys.stdout.flush()
    sys.stderr.flush()
    try:
        os.execvpe(argv[0], argv, dict(env))
    except OSError as e:
        warn(f"convergence-local: {argv[0]}: {e.strerror}")
        sys.exit(127 if isinstance(e, FileNotFoundError) else 126)


def run(argv: Sequence[str], env: Mapping[str, str], runner: Runner) -> int:
    local_leg = FLAG in argv
    args = [a for a in argv if a != FLAG]
    code = args[0] if args else ""
    if code not in SUITES:
        warn(f"usage: scripts/convergence-local.sh {FLAG} <pl|de|uk|us|es> [sbt alias]")
        return 2
    if not local_leg:
        warn("convergence-local: refused -- a local leg runs tens of minutes and loads the whole machine.")
        sys.stderr.write(FASTER_TOOLS)
        warn(f"  Only the pipeline itself (scrape, enrichment, projection) needs a leg: add {FLAG}.")
        return 2
    warn("convergence-local: running a local leg (tens of minutes). If you only need take changes, stop and use:")
    sys.stderr.write(FASTER_TOOLS)
    os.chdir(ROOT)
    leg = Leg(runner, env, code, args[1] if len(args) > 1 and args[1] else SUITES[code])
    try:
        run_leg(leg)
    except Stop as stop:
        return stop.code
    finally:
        leg.remove_container()


def end_by_signal(signum: int, _frame: Optional[FrameType]) -> NoReturn:
    """INT, TERM and HUP end the leg as an exception, so its `finally` removes the container (a default TERM or HUP kills
    the process outright, leaving it)."""
    raise Stop(128 + signum)


def main() -> int:
    for sig in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):  # before the runner, which passes a signal on to this
        signal.signal(sig, end_by_signal)
    return run(sys.argv[1:], machine.environment(), SubprocessRunner())


if __name__ == "__main__":
    sys.exit(main())
