#!/usr/bin/env python3
"""What each lint spec OPENS, observed: every tracked file (and directory) it reads while it runs, against what its
lint-registry row (scripts/hooks/lint-registry.tsv) selects it for -- the dynamic half of the registry's read proof
(lint-registry-test.sh holds the static half: scripts/ci/lint_registry_reads.py, the paths a spec's source names).

WHY (the owner, 2026-11-21, landing-speed proposal 1): agentctl keeps a change's verdict when main moved only outside
what its checks read, so a row must select its spec for everything the spec opens -- and for nothing more, or every two
changes share a lint (175 `@repo` rows did). A row narrower than its spec's reads is a lint that never runs before a
push; this finds it by running the spec.

HOW IT SEES A READ: the file's access time. APFS (like Linux's relatime) moves a file's atime on a read only while the
atime is not after its mtime, so before each run every tracked file, every directory holding one and every resource sbt
copied to a module's classes gets an atime long before its mtime (the mtime kept), and whatever moved afterwards was
opened -- a file read or a directory listed, by any API (Files.*, java.io, Source, a classpath resource, a shell
child). A copy under */target/scala-*/<config>-classes/ stands for the tracked resource it was copied from; a MAIN
resource read through the classpath is the production code's own reading, as its sources are (the module's unit tests
own it; a resource the spec names itself the static half holds). A stat (Files.exists) moves nothing: an existence
check is the static half's, by its literal. Each spec runs alone in its own JVM, as sbt forks it (its module's Test
classpath, the repo root as the working directory -- ForkedTests; web's in web/, as Play forks it), never in sbt, which reads every source to stamp it;
and twice, keeping only what both runs opened (an indexer or an editor moves atimes too).

  lint_reads_trace.py [--classpaths FILE] [--out TSV] (--all | SPEC ...)
      SPEC: `<module>:<package.Spec>`; --all: every registry row's spec but `-` rows. Prints `<spec>TAB<path>` for each
      file a spec opened that its row does not select (asked of `pre-push --lint-plan`, the manifests in place);
      nothing when every row covers its spec. --out keeps every observed read (`<spec>TAB<path>`, a listed directory
      with a trailing /). --classpaths: `<module>TAB<classpath>` lines (else `sbt-warm export <module>/Test/fullClasspath`).
      Exit 1 on a miss, 2 when a spec did not run green (its reads are then partial: named on stderr), or when this
      filesystem's atimes do not move on a read or a listing (probed first: a trace seeing nothing proves nothing).
  lint_reads_trace.py --suggest TSV   the reads each spec's row wants, from a kept --out: a listed directory as
      `<dir>/*` (the topmost listed), any other file by name, a module segment (`<m>/src/...`) as `*` once three or more
      modules share the rest, a source config (`*/src/<config>/scala`) likewise; `@code` for a spec that opened nothing.
Run from the root, after `scripts/sbt-warm "<module>/Test/compile"` for every module traced, and with the GitOps
manifests in place (KINOWO_GITOPS_DEST=$PWD/infra/kubernetes infra/bin/fetch-gitops) for the `@gitops` rows.
"""
import collections
import os
import re
import shutil
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from typing import Dict, Iterable, List, Optional, Set, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from governor.tool_pool import governed_workers  # noqa: E402  (THE GOVERNED FAN-OUT: the hook runs under a governor ticket)

OLD_NS = 946684800 * 10**9  # 2000-01-01: before every mtime, so the next read moves the atime
HOOK = "scripts/hooks/pre-push"
REGISTRY = "scripts/hooks/lint-registry.tsv"
JAVA_OPTIONS = ["-Djava.awt.headless=true", "-Duser.language=en", "-Duser.country=US", "-Xmx2g"]
GENERALISE_AT = 3
KIND_FAMILIES = {k: family for family in ((".yml", ".yaml"), (".scala", ".java"), (".js", ".mjs", ".ts"), (".sh", ".bash"))
                 for k in family}
COMPACT_AT = 5  # files a spec reads by name in one fixture directory: the directory read as `<dir>/*`
FIXTURES = re.compile(r"(^|/)(fixtures|resources)/")  # recorded data, never code
WHOLE_TREE_AT = 6  # top-level directories a spec walks whole for the same kinds of file: it reads those kinds anywhere
RESOLVING = re.compile(r"[^/]+(/src)?")  # `<module>`, `<module>/src`: what a `*/src/*/scala` glob lists to resolve  # modules (or configs) sharing the rest of a listed directory: the segment read as `*`


def tracked():
    """The tracked files on disk and every directory above one (a walk lists `worker/src/main/scala`, which holds only
    directories) -- but the root: ModuleSources' `*/src/*/scala` lists it to find the modules, and a row's
    `*/src/main/scala/*` already reads a new module's tree."""
    out = subprocess.run(["git", "ls-files", "-z"], capture_output=True, text=True, check=True).stdout
    files = [p for p in out.split("\0") if p and os.path.isfile(p)]
    return files, sorted({a for p in files for a in ancestors(p)})


def _resources_by_module(files: List[str]) -> Dict[str, List[Tuple[str, str, str]]]:
    """{module: [(config, path relative to its resources, tracked path)]} of every `<m>/src/<config>/resources/<rel>`."""
    by_module: Dict[str, List[Tuple[str, str, str]]] = collections.defaultdict(list)
    for p in files:
        parts = p.split("/")
        if len(parts) > 4 and parts[1] == "src" and parts[3] == "resources":
            by_module[parts[0]].append((parts[2], "/".join(parts[4:]), p))
    return by_module


def _scala_targets(module: str) -> List[str]:
    target = os.path.join(module, "target")
    return [d for d in (os.listdir(target) if os.path.isdir(target) else []) if d.startswith("scala-")]


def resource_copies(files: List[str]) -> Dict[str, str]:
    """{copy under target/: the tracked resource sbt copied there}: `<m>/src/<config>/resources/<rel>` is served from
    `<m>/target/scala-*/<classes>/<rel>` (main: classes, else <config>-classes)."""
    candidates = {os.path.join(module, "target", scala, "classes" if config == "main" else f"{config}-classes", rel): path
                  for module, rows in _resources_by_module(files).items()
                  for scala in _scala_targets(module)
                  for config, rel, path in rows}
    return {copy: path for copy, path in candidates.items() if os.path.isfile(copy)}


def is_main_copy(path):
    """Whether path is sbt's copy of a module's src/main/resources file: <m>/target/scala-*/classes/..."""
    parts = path.split("/")
    return len(parts) > 3 and parts[1] == "target" and parts[2].startswith("scala-") and parts[3] == "classes"


def atime_moves(root="."):
    """(a file read moves its atime, a directory listed moves its atime) on root's filesystem: a probe file and directory
    made there, aged, read and listed by a child process, then removed. A filesystem mounted noatime (or a read-only or
    bind mount that keeps atimes, CI's Linux container, 2026-11-21) moves neither, or only the file's."""
    probe = tempfile.mkdtemp(prefix=".atime-probe-", dir=root)
    try:
        inner, entry = os.path.join(probe, "d"), os.path.join(probe, "f")
        os.mkdir(inner)
        with open(os.path.join(inner, "x"), "w") as f:
            f.write("x\n")
        with open(entry, "w") as f:
            f.write("x\n")
        for p in (inner, entry):
            os.utime(p, ns=(OLD_NS, os.stat(p).st_mtime_ns))
        subprocess.run(["sh", "-c", 'cat "$1" > /dev/null; ls "$2" > /dev/null', "_", entry, inner], check=True)
        return tuple(moved(p) for p in (entry, inner))
    finally:
        shutil.rmtree(probe, ignore_errors=True)


def moved(path: str) -> bool:
    """Whether path's atime left OLD_NS, where it was aged: a read since. Never "at or after the clock read before the
    command": Linux stamps a file with its coarse clock, a tick behind time.time_ns(), so a read right after that
    reading can carry an earlier atime (ci-runner's ext4, 2026-11-22: both reading cases saw no read)."""
    return os.stat(path).st_atime_ns > OLD_NS


def blind(moves):
    """Why reads cannot be seen here, or None: a trace on a filesystem whose atimes do not move would see no read and
    report every row whole -- a false proof -- so the tools refuse instead."""
    files, dirs = moves
    if not files:
        return "a file read does not move its atime on this filesystem (noatime, or a mount that keeps atimes)"
    if not dirs:
        return "a directory listed does not move its atime on this filesystem: a walk's directories go unseen"
    return None


def observe(command, watched, copies, cwd=None):
    """(what command opened of watched, its completed process): every watched path aged, the command run, the moved
    ones read back -- class files and main resources served through the classpath left out."""
    for p in watched:
        try:
            os.utime(p, ns=(OLD_NS, os.stat(p).st_mtime_ns))
        except OSError:
            pass
    run = subprocess.run(command, cwd=cwd, capture_output=True, text=True)
    seen = []
    for p in watched:
        try:
            if moved(p):
                seen.append(p)
        except OSError:
            pass
    return sorted({copies.get(p, p) for p in seen if not p.endswith(".class") and not is_main_copy(p)}), run


# the working directory of a module's forked specs: the repo root (ForkedTests), but web's, which Play forks in web/
SPEC_CWD = {"web": "web"}


def spec_command(spec, classpath):
    return ["java", *JAVA_OPTIONS, "-cp", classpath, "org.scalatest.tools.Runner", "-oW", "-s", spec.split(":", 1)[1]]


def green(run):
    return run.returncode == 0 and "*** RUN ABORTED ***" not in run.stdout and "Suites: completed 0" not in run.stdout


def _given_classpaths(path: Optional[str]) -> Dict[str, str]:
    """The `<module>TAB<classpath>` lines of `--classpaths`."""
    if not path:
        return {}
    with open(path) as f:
        return dict(line.rstrip("\n").split("\t", 1) for line in f if "\t" in line)


def _exported_classpaths(missing: List[str]) -> Dict[str, str]:
    """The Test classpaths of `missing` modules, from one sbt-warm export; exits where it could not."""
    run = subprocess.run(["scripts/sbt-warm", *[f"export {m}/Test/fullClasspath" for m in missing]],
                         capture_output=True, text=True)
    cps = [line for line in (x.replace("\x1b[0J", "") for x in run.stdout.splitlines()) if line.startswith("/") and ":" in line]
    if run.returncode != 0 or len(cps) != len(missing):
        sys.exit(f"lint_reads_trace: could not export the classpaths of {' '.join(missing)}")
    return dict(zip(missing, cps))


def classpaths(modules: List[str], given: Optional[str]) -> Dict[str, str]:
    found = _given_classpaths(given)
    missing = [m for m in modules if m not in found]
    if missing:
        found.update(_exported_classpaths(missing))
    return found


def selections(paths, hook=HOOK):
    """{path: the specs `pre-push --lint-plan` selects for a push of it alone}, the manifests in place, 8 at a time."""
    def one(path):
        run = subprocess.run([hook, "--lint-plan"], input=path + "\n", capture_output=True, text=True,
                             env={**os.environ, "PREPUSH_GITOPS": "1"})
        return set(run.stdout.split())
    with governed_workers("lint-plan", 8) as width, ThreadPoolExecutor(max_workers=width) as pool:
        return dict(zip(paths, pool.map(one, paths)))


def misses(observed, hook=HOOK):
    """`<spec>TAB<file>` for every file a spec opened that its row does not select (directories: --suggest's)."""
    files = sorted({p for reads in observed.values() for p in reads if not p.endswith("/")})
    selection = selections(files, hook)
    return [f"{spec}\t{p}" for spec, reads in observed.items() for p in reads
            if not p.endswith("/") and spec not in selection[p]]


def registry_specs():
    with open(REGISTRY) as f:
        rows = [l.rstrip("\n").split("\t") for l in f if l.strip() and not l.startswith("#")]
    return [r[0] for r in rows if len(r) > 1 and r[1].split() != ["-"]]


def ancestors(path):
    parts = path.split("/")
    return ["/".join(parts[:i]) for i in range(1, len(parts))]


def _as_regex(pattern: str) -> "re.Pattern[str]":
    return re.compile(re.escape(pattern).replace(r"\*", "[^/]+") + "$")


def _shared_by_segment(out: Set[str], position: int) -> Dict[Tuple[str, ...], Set[str]]:
    """{the pattern with the segment at `position` as `*`: the segment values sharing the rest} of the `<m>/src/...` dirs."""
    shared: Dict[Tuple[str, ...], Set[str]] = collections.defaultdict(set)
    for d in out:
        parts = d.split("/")
        if len(parts) > 2 and parts[1] == "src":
            shared[tuple(parts[:position]) + ("*",) + tuple(parts[position + 1:])].add(parts[position])
    return shared


def _generalise_at(out: Set[str], position: int, tree: Iterable[str]) -> Set[str]:
    """`out` with the segment at `position` (0: the module, 2: the source config) read as `*` where GENERALISE_AT or more
    share the rest; a config only when every config of the tree the pattern names was walked."""
    def walked(d: str) -> bool:
        return any(_as_regex(o).match(x) for o in out for x in [d, *ancestors(d)])
    for key, values in _shared_by_segment(out, position).items():
        matches = _as_regex("/".join(key))
        if len(values) >= GENERALISE_AT and (position == 0 or all(walked(d) for d in tree if matches.match(d))):
            out = {d for d in out if not matches.match(d)} | {"/".join(key)}
    return out


def generalised(directories: Iterable[str], tree: Iterable[str]) -> Set[str]:
    """The walked directories with a module segment read as `*` once GENERALISE_AT or more modules share the rest
    (`<m>/src/main/scala` -> `*/src/main/scala`: a walk declared over a few modules' trees reads a little wider, never
    narrower -- but never `*/src`, every module's every tree), then a source config (`*/src/<config>/...`) read as `*`
    when every config of the tree the pattern names was walked (a test-tree scan stays one: no production change
    selects it)."""
    tree = list(tree)
    out = set(directories)
    for position in (0, 2):
        out = _generalise_at(out, position, tree)
    return out


def compacted(files):
    """Files read by name, a FIXTURE directory holding COMPACT_AT or more of them read as `<dir>/*`, then one holding
    GENERALISE_AT or more such as `<dir>/*` (a harness reading a venue's 117 recorded pages: its fixture tree, wider
    and never narrower). Code is never compacted: a spec reading six scripts by name reads no seventh, and a row
    widened to `scripts/*` would meet every other script change."""
    out = {p for p in files if FIXTURES.search(p)}
    return _compacted(out) | (set(files) - out)


def _compact_level(out: Set[str], at: int) -> Set[str]:
    """`out` with each directory holding `at` or more of its entries read as `<dir>/*`."""
    by_parent: Dict[str, Set[str]] = collections.defaultdict(set)
    for p in out:
        if "/" in p.rstrip("/*").rstrip("/"):
            by_parent[os.path.dirname(p[:-2] if p.endswith("/*") else p)].add(p)
    for parent, members in by_parent.items():
        if len(members) >= at and parent:
            out = (out - members) | {parent + "/*"}
    return out


def _compacted(files: Iterable[str]) -> Set[str]:
    out = _compact_level(_compact_level(set(files), COMPACT_AT), GENERALISE_AT)
    return {p for p in out if not any(p != q and q.endswith("/*") and p.startswith(q[:-1]) for q in out)}


def extension(path):
    return os.path.splitext(path)[1]


def _resolving(listed: Set[str]) -> Set[str]:
    """The directories listed only to resolve `*/src/*/scala`: a module's `<m>/src` and, when listed, its `<m>`."""
    resolving = {d for d in listed if re.fullmatch(r"[^/]+/src", d)}
    return resolving | {d.split("/")[0] for d in resolving if d.split("/")[0] in listed}


def _listed_roots(reads: List[str]) -> Set[str]:
    """The topmost directories a spec listed, less the module directories and `<m>/src` it listed only to resolve
    `*/src/*/scala` (SourceTrees) when it went deeper: what the walk read is, and a new module's tree is read by the `*`
    its row's module segment generalises to."""
    listed = {p.rstrip("/") for p in reads if p.endswith("/")}
    resolving = _resolving(listed)
    listed -= {d for d in resolving if any(q.startswith(d + "/") and q.rstrip("/") not in resolving for q in reads)}
    return {d for d in listed if not any(a in listed for a in ancestors(d))}


def _filters_by_kind(top: Set[str], read: Set[str], under: Dict[str, Set[str]], kinds: Set[str]) -> bool:
    """Whether a walk of `top` reading `kinds` left some file unread and read exactly those kinds' files under each
    directory it walked (a file filter seen), rather than every file or none by kind."""
    if not kinds or "" in kinds or all(read >= under[d] for d in top):
        return False
    return all(read & under[d] == {f for f in under[d] if extension(f) in kinds} for d in top)


def _kinds_read(top: Set[str], read: Set[str], under: Dict[str, Set[str]]) -> Set[str]:
    """The extensions a walk of `top` kept, or {""} when it read whole (no file left unread: no filter seen) or not by
    kind. A walk keeping some kinds of file reads, under every directory it walked, exactly those kinds' files there --
    `<dir>/*.<ext>`, a new file of another kind no read of it. A kind read is its family: a walk keeping `.yml` keeps
    `.yaml` (RepoFile.workflows), one keeping `.scala` may keep `.java`, though the tree holds none of the other today."""
    kinds = {extension(p) for d in top for p in read & under[d]}
    kinds = kinds if _filters_by_kind(top, read, under, kinds) else {""}
    return {k for kind in kinds for k in KIND_FAMILIES.get(kind, (kind,))}


def _globs(top: Set[str], kinds: Set[str], under: Dict[str, Set[str]]) -> List[str]:
    """`<dir>/*<kind>` for each walked directory and kind: a whole-tree walk (NoEnvLocalReadersSpec: every file of 13
    kinds, wherever it is) reads those kinds anywhere."""
    whole = kinds != {""} and len({d for d in top if "/" not in d}) >= WHOLE_TREE_AT
    dirs = {""} if whole else generalised(top, under)
    return sorted({(d + "/" if d else "") + "*" + kind for d in dirs for kind in kinds})


def _spec_reads(reads: List[str], under: Dict[str, Set[str]]) -> List[str]:
    """The reads one spec's row wants (`suggest`)."""
    top = _listed_roots(reads)
    read = {p for p in reads if not p.endswith("/")}
    by_name = compacted({p for p in read if not any(a in top for a in ancestors(p))})
    return _globs(top, _kinds_read(top, read, under), under) + sorted(by_name) or ["@code"]


def suggest(observed: Dict[str, List[str]], files: List[str]) -> Dict[str, List[str]]:
    """{spec: the reads its row wants} from {spec: observed reads} (a listed directory with a trailing /) over the
    tracked files. A listed directory is read as `<dir>/*` -- or as `<dir>/*.<ext>` for each extension the spec read
    anywhere, when what it read under every walked directory is exactly the files of those extensions there and it left
    some file unread (a walk keeping `.scala`; one reading every file it lists reads a new kind too)."""
    under: Dict[str, Set[str]] = collections.defaultdict(set)
    for f in files:
        for a in ancestors(f):
            under[a].add(f)
    return {spec: _spec_reads(reads, under) for spec, reads in observed.items()}


def read_observed(path):
    observed = collections.defaultdict(list)
    with open(path) as f:
        for line in f:
            spec, _, read = line.rstrip("\n").partition("\t")
            if read:
                observed[spec].append(read)
            else:
                observed.setdefault(spec, [])
    return observed


def _options(args: List[str]) -> Tuple[Optional[str], Optional[str], List[str]]:
    """(--classpaths file, --out file, the rest) of the arguments."""
    given = out_path = None
    while args[:1] in (["--classpaths"], ["--out"]) and len(args) > 1:
        given, out_path = (args[1], out_path) if args[0] == "--classpaths" else (given, args[1])
        args = args[2:]
    return given, out_path, args


def _trace_spec(spec: str, classpath: str, watched: List[str], copies: Dict[str, str]) -> Tuple[List[str], bool]:
    """(what both of two runs of `spec` opened, whether both ran green)."""
    runs = [observe(spec_command(spec, classpath), watched, copies, SPEC_CWD.get(spec.split(":", 1)[0])) for _ in range(2)]
    green_both = all(green(run) for _, run in runs)
    if not green_both:
        print(f"lint_reads_trace: {spec} did not run green; its reads are partial:\n{runs[0][1].stdout[-2000:]}",
              file=sys.stderr)
    both = set(runs[0][0]) & set(runs[1][0])
    return sorted(p + "/" if os.path.isdir(p) else p for p in both), green_both


def _write_observed(path: str, observed: Dict[str, List[str]]) -> None:
    with open(path, "w") as f:
        f.write("".join(f"{spec}\t{p}\n" for spec, reads in observed.items() for p in reads)
                + "".join(f"{spec}\t\n" for spec, reads in observed.items() if not reads))


def _trace_all(specs: List[str], cps: Dict[str, str]) -> Tuple[Dict[str, List[str]], List[str]]:
    """({spec: what it opened}, the specs that did not run green)."""
    files, dirs = tracked()
    copies = resource_copies(files)
    watched = files + dirs + list(copies)
    observed: Dict[str, List[str]] = {}
    broken: List[str] = []
    for spec in specs:
        observed[spec], green_both = _trace_spec(spec, cps[spec.split(":", 1)[0]], watched, copies)
        if not green_both:
            broken.append(spec)
        print(f"traced {spec}: {len(observed[spec])} reads", file=sys.stderr)
    return observed, broken


def _suggest_command(path: str) -> int:
    for spec, reads in sorted(suggest(read_observed(path), tracked()[0]).items()):
        print(f"{spec}\t{' '.join(reads)}")
    return 0


def _usable(specs: List[str]) -> int:
    """0 when `specs` can be traced here; else 2, having said why."""
    if not specs or any(":" not in s for s in specs):
        print(__doc__, file=sys.stderr)
        return 2
    why = blind(atime_moves())
    if why:
        print(f"lint_reads_trace: cannot trace here: {why}; run it on the laptop (APFS)", file=sys.stderr)
    return 2 if why else 0


def main(args: List[str]) -> int:
    if args[:1] == ["--suggest"] and len(args) == 2:
        return _suggest_command(args[1])
    given, out_path, args = _options(args)
    specs = registry_specs() if args == ["--all"] else args
    if _usable(specs):
        return 2
    observed, broken = _trace_all(specs, classpaths(sorted({s.split(":", 1)[0] for s in specs}), given))
    if out_path:
        _write_observed(out_path, observed)
    found = misses(observed)
    for line in found:
        print(line)
    return 2 if broken else 1 if found else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
