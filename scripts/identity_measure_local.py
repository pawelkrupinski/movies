#!/usr/bin/env python3
"""THE IDENTITY MEASURE, LOCALLY: scripts/identity-measure's whole-corpus base-vs-head verdict with no GitHub -- the
same resolve (integration.IdentityResolveDump, the model's takes), the same final takes (the unmatched-cluster
ratchet's --measure) and the same diff (scripts/ci/identity_takes_diff.py) the convergence suite's `takes` rows run,
over a recording already in the identity cache.

  scripts/identity-measure-local --base REF [--head REF] [--countries pl,de,uk,us,es] [--recording RUN] [--out DIR]

  --base       the commit to measure against (required: no commit status is read to find one)
  --head       the commit measured (default HEAD)
  --countries  default every country the recording holds
  --recording  a "Record scrape fixtures" run whose laid pair is cached
               ($KINOWO_IDENTITY_CACHE/recordings/<run>/pair, as identity-slice lays it); default the newest cached one
  --out        default target/identity-measure-local/<recording>/<base>..<head> (…-live with --live-gaps)
  --live-gaps  ask TMDB and IMDb live what the recording cannot answer, at both refs alike: the measure of a change that
               asks NEW questions (a search spelling, a query), which a hermetic replay answers with nothing. Needs
               TMDB_API_KEY (`secrets run movies TMDB_API_KEY -- scripts/identity-measure-local … --live-gaps`); CI's
               convergence fill records those answers once GitHub can run it

Each ref resolves in a detached worktree of its own, its compile seeded from the newest local build (sbt-warm --seed),
in ONE sbt session over a mongod of its own (scripts/it-local.sh): every country's dump, then the ratchet's final
takes. A ref's takes are a pure function of the commit and the recording, so they are SHARED across checkouts: kept in
$KINOWO_IDENTITY_CACHE/measure-local/<recording>/<sha>/whole/ (each country's dump on its own, the final takes per
country set) with the ref's worktree beside them, and <out>/<sha> links to them for reading. A ref another run is
resolving is waited for (<sha>.flock's lock, held over its worktree, its build and its writes), not resolved twice
(agents measuring against one base resolved it five times over on 2026-10-18), and each resolve
holds one of the machine's JVM slots with its sbt sized for KINOWO_JVM_CPUS cores (scripts/jvm-gate.sh). --live-gaps
answers change with time, so those resolves stay in <out>.

CONCURRENT RUNS ARE SAFE (2026-10-21: three heavy slots, one base..head measured on several recordings at once, and
runs sharing an out dir read each other's recording through its re-pointed links -- NOT LANDABLE over thousands of
listings "on one side only", and as easily a LANDABLE over the wrong recording). So: a resolve writes into a staging
directory of its own and moves each dump, and the final takes, into whole/ only once its sbt session exited 0 -- a
file in whole/ is finished, and never rewritten (one killed mid-write, a dump cut short, was taken for a finished one
by the next run); the verdict reads whole/'s files by their own paths, never through <out>'s links; and what is this
run's alone (the judgements, the classified layout) lives in a directory of this run's own. Takes cached before
whole/ (in <sha>/ itself) are not trusted: they are resolved again. Prints each country's model and final
verdicts as identity_takes_diff.py words them, and exits 1 when any fails (a right take lost or turned wrong, a new
wrong take, an unjudged move), 0 when none does, 2 on a usage or setup error.

The verdict is scripts/identity_measure.py's, as CI's measure classifies it (cure, right lost, right->wrong, new
wrong, gained...), after each country's identity_takes_diff.py report. BLIND SPOT, as in CI: the final stage replays
only the checked-in capture's clusters (the ones the model left unmatched when it was captured), so a listing the head
newly leaves unmatched at the model stage never reaches it -- a final 0 there is "not seen", not "the agreement refilled
it"; the model rows bound what is served.

What it does not do that CI does: read the base from the commit statuses, reuse a stored base, run the corpus-shape
specs. A FALLBACK ONLY (the user, 2026-10-21: heavy work runs on CI): measure with scripts/identity-measure
(--recording RUN for the same recording), and run this only for what that cannot do, under the machine's JVM
slots (scripts/jvm-gate.sh's jvm_slot_take, in the session's bash). scripts/identity-measure is the verdict of record.
AND ITS REPLAY IS NOT CI'S ON A MAC: a recording's tree holds paths that differ only by case (PL 528: 450+, e.g.
suggestion/l/Lalka.json beside lalka.json), and a case-insensitive APFS keeps one of each pair -- 528 PL resolved here
left 568 searches unanswered that CI answers, and took one listing to another film (2026-10-21)."""
from __future__ import annotations

import fcntl
import glob
import os
import re
import shutil
import sys
import tempfile
import time
from dataclasses import dataclass, field
from typing import IO, Dict, List, Mapping, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import tool_cli  # noqa: E402
import tool_countries  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402
import tool_python  # noqa: E402

TOOL = "identity-measure-local"
HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
JUDGEMENTS = "test/resources/fixtures/identity-judgements.tsv"
BLIND_SPOT = ("identity-measure-local: the final stage replays the checked-in capture's clusters only -- a listing the "
              "head newly\n  leaves unmatched at the model stage never reaches it, so its final 0 means not seen, not "
              "refilled: the model rows\n  bound what is served.")

DUMP = "worker/IntegrationTest/runMain integration.IdentityResolveDump"
RATCHET = "worker/IntegrationTest/runMain scripts.UnmatchedClustersRatchet"
# the session: a JVM slot taken, the compile seeded from the newest local build, the commands in one it-local.sh session
SESSION = '''. "$1" || exit 2; tree=$2; log=$3; shift 3
jvm_slot_take identity-measure-local
[ -x "$tree/scripts/sbt-warm" ] && ( cd "$tree" && scripts/sbt-warm --seed ) > "$log" 2>&1
( cd "$tree" && SBT_OPTS="${SBT_OPTS:-} $(jvm_cpu_options)" scripts/it-local.sh "$@" ) >> "$log" 2>&1
rc=$?; jvm_slot_release; exit "$rc"'''
EXCEPTION = re.compile(r"([A-Za-z_$][A-Za-z0-9_$]*\.)+[A-Za-z0-9_$]*(Exception|Error)(: |$)|\[ratchet\] no takes written")


class Failed(Exception):
    """A setup error: `<tool>: <message>` on stderr, exit 2 (the shell's DIE_STATUS=2)."""


def say(line: str, err: bool = False) -> None:
    print(line, file=sys.stderr if err else sys.stdout, flush=True)


def nonempty(path: str) -> bool:
    """`[ -s path ]`."""
    try:
        return os.path.getsize(path) > 0
    except OSError:
        return False


def link(target: str, name: str) -> None:
    """`ln -sfn target name`."""
    if os.path.islink(name) or os.path.isfile(name):
        os.remove(name)
    os.symlink(target, name)


class Measure:
    """What one run measures: the refs, the recording's laid pair, the countries, where its takes are kept and read."""

    def __init__(self, runner: tool_process.Runner, env: Dict[str, str], repo: str, cache: str) -> None:
        self.runner, self.env, self.repo, self.cache = runner, env, repo, cache
        self.base = self.head = self.recording = self.pair = self.out = self.store = ""
        self.countries: List[str] = []

    def git(self, *args: str) -> Optional[str]:
        return tool_git.Git(self.runner, self.env, self.repo).out(*args, quiet=True)

    def short(self, ref: str) -> str:
        sha = self.git("rev-parse", "--short=12", f"{ref}^{{commit}}")
        if sha is None:
            raise Failed(f"no commit {ref}")
        return sha.strip()

    def ratchet_key(self) -> str:
        return "-".join(sorted(self.countries))

    def whole(self, sha: str) -> str:
        """Where a ref's finished takes are: each file whole, moved in, never rewritten."""
        return os.path.join(self.store, sha, "whole")


class Resolver:
    def __init__(self, m: Measure) -> None:
        self.m = m
        self.held: Optional[IO[str]] = None
        self.held_path = ""
        self.staging = ""

    def close(self) -> None:
        if self.staging:
            shutil.rmtree(self.staging, ignore_errors=True)
            self.staging = ""
        self.unlock()

    def lock(self, path: str) -> None:
        """`until jvm_lock <path>; do …; done`: this process holds its kernel lock, its pid written in."""
        said = False
        while True:
            f = open(path, "a")
            try:
                fcntl.flock(f, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except OSError:
                f.close()
                if not said:
                    say(f"{TOOL}: {os.path.basename(path)[:-len('.flock')]} is being resolved by pid "
                        f"{read(path)}: waiting for it")
                    said = True
                time.sleep(float(self.m.env.get("KINOWO_MEASURE_LOCAL_POLL") or "5"))
                continue
            f.truncate(0)
            f.write(f"{os.getpid()}\n")
            f.flush()
            self.held, self.held_path = f, path
            return

    def unlock(self) -> None:
        if self.held is not None:
            self.held.truncate(0)
            self.held.close()
            self.held, self.held_path = None, ""

    def commands(self, sha: str) -> List[str]:
        """What is missing of the ref's takes: each country's dump, then this country set's final takes."""
        m, whole, key = self.m, self.m.whole(sha), self.m.ratchet_key()
        wanted = [f"{DUMP} --out {self.staging} --full {cc} --corpus-dir {m.pair}/corpus --fixtures {m.pair}"
                  for cc in m.countries if not nonempty(f"{whole}/decisions-{cc}.jsonl")]
        if not nonempty(f"{whole}/ratchet-{key}/ratchet-takes.tsv"):
            wanted.append(f"{RATCHET} --measure --out {self.staging}/ratchet-{key} --countries {','.join(m.countries)}")
        return wanted

    def resolve(self, sha: str) -> bool:
        """Every country's model takes and the final ones, in the ref's own worktree, shared across checkouts."""
        whole = self.m.whole(sha)
        os.makedirs(whole, exist_ok=True)
        self.lock(os.path.join(self.m.store, f"{sha}.flock"))
        # each country's dump and the final takes for this country set are a pure function of the commit and the
        # recording: what any run already resolved is kept, and only what is missing is resolved -- into a staging
        # directory, moved into whole/ once the session succeeded
        self.staging = tempfile.mkdtemp(prefix=".staging.", dir=os.path.join(self.m.store, sha))
        commands = self.commands(sha)
        if not commands:
            say(f"{TOOL}: {sha} already resolved in {whole}")
        elif not self.session(sha, commands):
            self.close()
            return False
        self.close()
        self.links(sha)
        return self.finished(sha)

    def session(self, sha: str, commands: List[str]) -> bool:
        """The commands in one sbt session in the ref's worktree; False only when its worktree could not be made."""
        m, tree, log = self.m, os.path.join(self.m.store, "trees", sha), self.log(sha)
        if not os.path.isdir(tree) and m.runner.run(tool_process.Command(
                ["git", "-C", m.repo, "worktree", "add", "--detach", "--quiet", tree, sha], m.env)) != 0:
            return False
        started = time.monotonic()
        status = m.runner.run(tool_process.Command(
            ["bash", "-c", SESSION, TOOL, os.path.join(m.repo, "scripts", "jvm-gate.sh"), tree, log, *commands], m.env))
        say(f"{TOOL}: {sha} resolved in {int(time.monotonic() - started)}s (exit {status}, log {log})")
        if status == 0:
            self.keep(sha)
        else:
            say(f"{TOOL}: {sha}'s session failed (exit {status}): nothing it wrote is kept", err=True)
        return True

    def keep(self, sha: str) -> None:
        """A rename within one directory tree: each file appears in whole/ at once, finished."""
        whole, key = self.m.whole(sha), self.m.ratchet_key()
        for cc in self.m.countries:
            if nonempty(f"{self.staging}/decisions-{cc}.jsonl"):
                for file in glob.glob(f"{glob.escape(self.staging)}/*-{cc}.*"):
                    os.replace(file, os.path.join(whole, os.path.basename(file)))
        ratchet = f"{self.staging}/ratchet-{key}"
        if nonempty(f"{ratchet}/ratchet-takes.tsv"):
            shutil.rmtree(f"{whole}/ratchet-{key}", ignore_errors=True)
            os.rename(ratchet, f"{whole}/ratchet-{key}")

    def log(self, sha: str) -> str:
        return os.path.join(self.m.store, f"{sha}.log")

    def links(self, sha: str) -> None:
        """<out>/<sha>: links to the shared takes for reading them. The verdict reads whole/ itself: a run sharing <out>
        re-points these."""
        m, whole, log = self.m, self.m.whole(sha), self.log(sha)
        os.makedirs(os.path.join(m.out, sha), exist_ok=True)
        for cc in m.countries:
            link(f"{whole}/decisions-{cc}.jsonl", os.path.join(m.out, sha, f"decisions-{cc}.jsonl"))
        link(f"{whole}/ratchet-{m.ratchet_key()}", os.path.join(m.out, sha, "ratchet"))
        if os.path.exists(log) and log != os.path.join(m.out, f"{sha}.log"):
            link(log, os.path.join(m.out, f"{sha}.log"))

    def finished(self, sha: str) -> bool:
        whole, key = self.m.whole(sha), self.m.ratchet_key()
        for cc in self.m.countries:
            if not nonempty(f"{whole}/decisions-{cc}.jsonl"):
                say(f"{TOOL}: {sha} wrote no decisions-{cc}.jsonl", err=True)
                return False
        # no final takes is no resolve: on 2026-10-19 the ratchet died on a NoClassDefFoundError, sbt's runMain still
        # said [success], and the measure went on to NOT LANDABLE with no reason given -- so say what it died of
        if nonempty(f"{whole}/ratchet-{key}/ratchet-takes.tsv"):
            return True
        say(f"{TOOL}: {sha} wrote no final takes ({whole}/ratchet-{key})", err=True)
        cause = died_of(self.log(sha))
        lines = (f"died: {cause}" if cause else "left no exception in its log").split("\n")
        say("\n".join([f"  the ratchet {lines[0]}", *(f"    {line}" for line in lines[1:])]), err=True)
        return False


def read(path: str) -> str:
    try:
        with open(path) as f:
            return f.read().strip()
    except OSError:
        return ""


def died_of(log: str) -> str:
    """`grep -m1 -A1 <exception> log`: the first line naming an exception, and the line after it."""
    try:
        with open(log, errors="replace") as f:
            lines = f.read().split("\n")
    except OSError:
        return ""
    for i, line in enumerate(lines):
        if EXCEPTION.search(line):
            return "\n".join(lines[i:i + 2])
    return ""


def usage() -> str:
    return (__doc__ or "") + "\n"


@dataclass
class Options:
    base: str = ""
    head: str = "HEAD"
    countries: List[str] = field(default_factory=lambda: [])
    recording: str = ""
    out: str = ""
    live: bool = False


VALUED = {"--base": "base", "--head": "head", "--recording": "recording", "--out": "out"}


def word(o: Options, argv: List[str], i: int) -> int:
    arg = argv[i]
    if arg in VALUED:
        setattr(o, VALUED[arg], tool_cli.value_at(argv, i))
        return i + 1
    if arg.split("=", 1)[0] in ("--country", "--countries"):
        o.countries, i = tool_countries.flag_at(argv, i, o.countries)
        return i
    if arg == "--live-gaps":
        o.live = True
        return i
    raise tool_cli.UsageError(f"unknown argument: {arg}")


def options(argv: List[str]) -> Optional[Options]:
    o = tool_cli.read_options(Options(), argv, usage(), word)
    if o is not None and not o.base:
        raise tool_cli.UsageError("--base REF is required")
    return o


def newest_recording(cache: str) -> str:
    runs = [int(run) for run in (laid.split(os.sep)[-6] for laid in
                                 glob.glob(os.path.join(cache, "recordings", "*", "pair", "test", "resources",
                                                        "fixtures", "corpus")))
            if re.fullmatch(r"[0-9]+", run)]
    if not runs:
        raise Failed(f"no laid recording in {cache}/recordings (lay one with scripts/identity-slice first)")
    return str(max(runs))


def countries_of(o: Options, pair: str) -> List[str]:
    if o.countries:
        return o.countries
    return [os.path.basename(d)[len("enrichment-"):] for d in sorted(glob.glob(os.path.join(pair, "enrichment-*")))
            if os.path.isdir(d)]


def live_env(o: Options, env: Dict[str, str]) -> None:
    if not o.live:
        return
    key = env.get("KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY") or env.get("TMDB_API_KEY") or ""
    if not key:
        raise Failed(f"--live-gaps needs TMDB_API_KEY (secrets run movies TMDB_API_KEY -- {sys.argv[0]} …)")
    env["KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY"] = key


def measure_of(o: Options, env: Dict[str, str], runner: tool_process.Runner) -> Measure:
    cache = env.get("KINOWO_IDENTITY_CACHE") or os.path.join(env.get("HOME", ""), ".cache", "kinowo-identity")
    env["KINOWO_IDENTITY_CACHE"] = cache
    m = Measure(runner, env, REPO, cache)
    m.base, m.head = m.short(o.base), m.short(o.head)
    m.recording = o.recording or newest_recording(cache)
    m.pair = os.path.join(cache, "recordings", m.recording, "pair", "test", "resources", "fixtures")
    if not os.path.isdir(os.path.join(m.pair, "corpus")):
        raise Failed(f"recording {m.recording} has no laid pair at {m.pair}")
    m.countries = countries_of(o, m.pair)
    live_env(o, env)
    out = o.out or os.path.join(REPO, "target", TOOL, m.recording, f"{m.base}..{m.head}" + ("-live" if o.live else ""))
    os.makedirs(out, exist_ok=True)
    m.out = os.path.abspath(out)  # each ref's commands run from its own worktree
    m.store = m.out if o.live else os.path.join(cache, "measure-local", m.recording)
    os.makedirs(os.path.join(m.store, "trees"), exist_ok=True)
    say(f"identity-measure-local: base {m.base}, head {m.head}, recording {m.recording}, countries "
        f"{' '.join(m.countries)}{', live gaps' if o.live else ''}, out {m.out}")
    return m


def diffs(m: Measure, judgements: str) -> bool:
    """Each country's model and final takes diffed (identity_takes_diff.py): whether every diff passed."""
    passed = True
    base, head = m.whole(m.base), m.whole(m.head)
    names = ["--judgements", judgements, "--base-name", m.base, "--head-name", m.head]
    diff = os.path.join(REPO, "scripts", "ci", "identity_takes_diff.py")
    for cc in m.countries:
        say(f"== {cc} model")
        passed &= m.runner.run(tool_process.Command(
            ["python3", diff, cc, f"{base}/decisions-{cc}.jsonl", f"{head}/decisions-{cc}.jsonl", *names], m.env)) == 0
        say(f"== {cc} final")  # both refs' final takes are there: a ref without them did not resolve
        ratchet = f"ratchet-{m.ratchet_key()}/ratchet-takes.tsv"
        passed &= m.runner.run(tool_process.Command(
            ["python3", diff, cc, f"{base}/{ratchet}", f"{head}/{ratchet}", "--final", *names], m.env)) == 0
    return passed


def classified(m: Measure, run: str, judgements: str) -> bool:
    """The verdict CI's measure prints: the moves split by the judgements (cure, right lost, new wrong, ...), from the
    two refs' takes laid out as each takes row's artifact is. Whether it found none that fails."""
    root = os.path.join(run, "classified")
    for cc in m.countries:
        for side, sha in (("base", m.base), ("head", m.head)):
            directory = os.path.join(root, f"identity-takes-{cc}", side)
            os.makedirs(directory, exist_ok=True)
            os.symlink(f"{m.whole(sha)}/decisions-{cc}.jsonl", os.path.join(directory, f"decisions-{cc}.jsonl"))
            os.symlink(f"{m.whole(sha)}/ratchet-{m.ratchet_key()}/ratchet-takes.tsv",
                       os.path.join(directory, "ratchet-takes.tsv"))
    say("")
    return m.runner.run(tool_process.Command(tool_python.isolated(
        os.path.join(REPO, "scripts", "identity_measure.py"), root, "--judgements", judgements, "--countries",
        ",".join(m.countries)), m.env)) == 0


def verdict(m: Measure, run: str) -> int:
    # the head's own judgements, read from the commit: a head another run resolved has no worktree made by this one
    judgements = os.path.join(run, f"judgements-{m.head}.tsv")
    shown = m.git("show", f"{m.head}:{JUDGEMENTS}")
    if shown is None:
        raise Failed(f"no judgements at {m.head}")
    with open(judgements, "w") as f:
        f.write(shown)
    passed = diffs(m, judgements)
    clean = classified(m, run, judgements)
    say(BLIND_SPOT)
    # the classification decides (a right take lost or turned wrong, a new wrong one); a move nobody judged fails the
    # takes diff alone, and must be judged before landing, as CI's measure says
    if not clean:
        say("NOT LANDABLE (local): see the classification above")
        return 1
    if not passed:
        say("LANDABLE ONCE JUDGED (local): judge every unjudged move listed above first")
        return 1
    say("LANDABLE (local)")
    return 0


def measured(m: Measure) -> int:
    # this run's own: its judgements and its classified layout, which a run sharing <out> must not rewrite under it
    run = tempfile.mkdtemp(prefix=".run.", dir=m.out)
    resolver = Resolver(m)
    try:
        for side, sha in (("base", m.base), ("head", m.head)):
            if not resolver.resolve(sha):
                raise Failed(f"the {side} did not resolve (log {m.out}/{sha}.log)")
        return verdict(m, run)
    finally:
        resolver.close()
        shutil.rmtree(run, ignore_errors=True)


def main(argv: List[str], env: Mapping[str, str]) -> int:
    tool_cli.raise_on_signals()
    try:
        o = options(argv)
    except tool_cli.UsageError as error:
        return tool_cli.usage_failed(TOOL, error)
    if o is None:
        return 0
    try:
        return measured(measure_of(o, dict(env), tool_process.SubprocessRunner()))
    except Failed as failure:
        print(f"{TOOL}: {failure}", file=sys.stderr)
        return 2
    except tool_cli.Interrupted as interrupted:
        return 128 + interrupted.signum


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
