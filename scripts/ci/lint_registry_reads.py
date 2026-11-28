#!/usr/bin/env python3
"""What the specs READ, against the lint registry (scripts/hooks/lint-registry.tsv) that selects them.

Default: the registry rows that miss a path their spec reads. Each registered spec's source is scanned for what it
reads of the repository (`reads`, below: a spec reading tools/devpanel/scripts/devpanel.py with a row that never
selected on tools/ turned Main red, 2026-11-06; one reading the Dockerfile, a path with no slash, was never seen at
all, Main #2547/#2550). A push carrying that path is put to the hook itself (`pre-push --lint-plan`, the GitOps
manifests in place), so `*`, `@code`, `@gitops`, globs and a spec's own code are judged by the one implementation. A
directory is probed by one file of each extension under it (a spec scanning it reads the kinds it scans): one selecting
probe is enough. A path git ignores is never probed: no push carries it. What a spec opens through a name no literal
gives (a file a workflow names, a walk's files) the dynamic half sees: scripts/ci/lint_reads_trace.py runs the spec.
Prints `<spec>TAB<literal>` for each miss; nothing when every row covers what its spec reads. Run from the root.

WHAT A SPEC READS (`reads`), from its string literals:
  - a literal naming a path of the repository (`"infra/nix/files/host-memory.json"`, `"test/resources/fixtures/x"`);
  - a literal naming a tracked file at the root (`"Dockerfile"`, `"build.sbt"`): RepoFile.read("Dockerfile");
  - a classpath resource a resource opener names (`getResourceAsStream("/routes")`, `Source.fromResource("x.tsv")`),
    found under any module's `src/<config>/resources/`.
A spec opening a resource through a name no literal gives (`getResourceAsStream(ClassListResource)`, `s"/$file"`)
reads something this cannot name: its row declares it by hand, and `--missing` still asks for that row.

  lint_registry_reads.py [--registry FILE] [--only SPEC ...]   the misses (of SPEC's rows only, with --only)
  lint_registry_reads.py --missing    THE READER LINT: every spec that reads a checked-in artefact (above, or through a
                                      resource opener / RepoFile call) and has no registry row, `<module>:<class>` a line
  lint_registry_reads.py --suggest    each of those with the reads its row would declare (an author's aid)
  lint_registry_reads.py --reads SPEC ...   what each SPEC (`<module>:<class>`) reads: `<spec> TAB <literal> TAB <paths>`
"""
import bisect
import os
import re
import sys
from concurrent.futures import ThreadPoolExecutor
from typing import Callable, Dict, Iterable, Iterator, List, Mapping, Sequence, Set, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from processes import machine  # noqa: E402
from seams.tool_files import Files  # noqa: E402
from tool_git import Git, shared_runner  # noqa: E402
from governor.tool_pool import governed_workers  # noqa: E402  (THE GOVERNED FAN-OUT: the hook runs under a governor ticket)
from tool_process import Command, Runner  # noqa: E402

LITERAL = re.compile(r'"([A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)+/?)"')
ROOT_LITERAL = re.compile(r'"([A-Za-z0-9_.-]+)"')
# a classpath resource named where it is opened: the literal the opener's first argument is
RESOURCE_CALL = re.compile(r'\b(?:getResourceAsStream|getResource|fromResource)\(\s*"/?([A-Za-z0-9_./-]+)"')
# a spec that opens a checked-in file at all, by whatever name: it has a row, declared by hand where no literal says
OPENER = re.compile(r'\b(?:getResourceAsStream|getResource|fromResource)\(|\bRepoFile\.')
EXTENSION = re.compile(r"\.[A-Za-z]{1,6}")  # a file kind; a dated or numbered suffix is no kind
HOOK = "scripts/hooks/pre-push"
SPEC_PATH = re.compile(r"^([^/]+)/src/test/scala/(.+Spec)\.scala$")


class Repo:
    """The tracked files, listed once (`git ls-files`): the root's files and every module's resource trees; the files
    on disk are read through `files`."""

    def __init__(self, tracked: Iterable[str], files: Files):
        self.files = files
        self.tracked = sorted(tracked)
        self.tracked_set = set(self.tracked)
        self.root_files = {p for p in self.tracked if "/" not in p}
        self.resource_roots = sorted({m.group(1) for m in map(re.compile(r"^([^/]+/src/[^/]+/resources/)").match,
                                                              self.tracked) if m})

    def under(self, prefix: str) -> Iterator[str]:
        """The tracked paths under a directory prefix, in order."""
        start = bisect.bisect_left(self.tracked, prefix)
        for path in self.tracked[start:]:
            if not path.startswith(prefix):
                break
            yield path

    def on_disk_under(self, directory: str) -> List[str]:
        """Every file below a directory on disk (paths from the root), none when it is no directory."""
        if not self.files.is_dir(directory):
            return []
        return [os.path.relpath(f, self.files.base) for f in self.files.under(directory)]


def probes(literal: str, repo: Repo) -> List[str]:
    """The pushed paths standing for `literal`: itself, or one file per extension below a directory -- tracked files
    (the index, listed once) and, for a path the index lacks, the files on disk (a file about to be committed)."""
    path = os.path.normpath(literal)
    if path in repo.tracked_set or repo.files.is_file(path):
        return [path]
    found: Dict[str, str] = {}
    below = list(repo.under(path + "/")) or repo.on_disk_under(path)
    for file in below:
        extension = os.path.splitext(file)[1]
        found.setdefault(extension if EXTENSION.fullmatch(extension) else "", file)
    return list(found.values())


def pushable(paths: List[str], repo: Repo, ignored_of: Callable[[List[str]], Set[str]]) -> List[str]:
    """`paths` less those git ignores (`ignored_of` asks it): a push never carries an ignored path -- the GitOps manifests
    that infra/bin/fetch-gitops lays under infra/kubernetes/ are movies-gitops' files, and probing them failed this check
    on every clone that had fetched them (the landing checks' among them, Tool self-tests #158) and on none without. A
    tracked path is never ignored, so only the rest are asked (one git call a spec, and only when it has any)."""
    untracked = [p for p in paths if p not in repo.tracked_set]
    if not untracked:
        return paths
    ignored = ignored_of(untracked)
    return [p for p in paths if p not in ignored]


def reads(text: str, repo: Repo, ignored_of: Callable[[List[str]], Set[str]]) -> Dict[str, List[str]]:
    """{literal: [probe paths]}: what a spec's source reads of the repository (the module docstring's rules)."""
    candidates: Dict[str, List[str]] = {}
    for literal in set(LITERAL.findall(text)):
        candidates.setdefault(literal, []).extend(probes(literal, repo))
    for literal in set(ROOT_LITERAL.findall(text)) & repo.root_files:
        candidates.setdefault(literal, []).append(literal)
    # a resource named at run time (`getResource("/" + pkg)`: compiled classes, no checked-in file) names nothing here
    for literal in {r for r in RESOURCE_CALL.findall(text) if r.strip("/")}:
        candidates.setdefault(literal, []).extend(p for root in repo.resource_roots for p in probes(root + literal.rstrip("/"), repo))
    keep = set(pushable(sorted({p for paths in candidates.values() for p in paths
                                if not p.startswith(("target/", ".git/"))}), repo, ignored_of))
    found = {literal: sorted(set(paths) & keep) for literal, paths in sorted(candidates.items())}
    return {literal: paths for literal, paths in found.items() if paths}


def spec_source(spec: str) -> str:
    module, name = spec.split(":", 1)
    return f"{module}/src/test/scala/{name.replace('.', '/')}.scala"


def registry_rows(registry: str, files: Files) -> List[List[str]]:
    return [line.split("\t") for line in files.read_text(registry).splitlines() if line.strip() and not line.startswith("#")]


def git_ignored(git: Git) -> Callable[[List[str]], Set[str]]:
    """Which of the paths git ignores (`git check-ignore --stdin`)."""
    return lambda paths: set(git.call("check-ignore", "--stdin", quiet=True, input="\n".join(paths) + "\n").stdout.split("\n"))


def hook_selection(runner: Runner, env: Mapping[str, str]) -> Callable[[str], Set[str]]:
    """The specs the hook selects for a push of one path (`pre-push --lint-plan`, the GitOps manifests in place)."""
    def selected(path: str) -> Set[str]:
        done = runner.capture(Command([HOOK, "--lint-plan"], {**env, "PREPUSH_GITOPS": "1"}, input=path + "\n"))
        return set(done.stdout.split())
    return selected


def misses(registry: str, only: Set[str], repo: Repo, ignored_of: Callable[[List[str]], Set[str]],
           selected_specs: Callable[[str], Set[str]]) -> List[str]:
    """The `<spec>TAB<literal>` lines of the registry rows that miss a path their spec reads."""
    wanted: Dict[str, Dict[str, List[str]]] = {}  # spec -> {literal: [probe, ...]}
    for columns in registry_rows(registry, repo.files):
        spec, declared = columns[0], columns[1] if len(columns) > 1 else ""
        source = spec_source(spec)
        if declared.split() == ["-"] or not repo.files.is_file(source) or (only and spec not in only):
            continue
        per = reads(repo.files.read_text(source), repo, ignored_of)
        if per:
            wanted[spec] = per
    unique = sorted({p for per in wanted.values() for found in per.values() for p in found})
    with governed_workers("lint-plan", 8) as width, ThreadPoolExecutor(max_workers=width) as pool:
        selection = dict(zip(unique, pool.map(selected_specs, unique)))
    return [f"{spec}\t{literal}" for spec, per in wanted.items() for literal, found in per.items()
            if not any(spec in selection[p] for p in found)]


def rowless_readers(registry: str, repo: Repo, ignored_of: Callable[[List[str]], Set[str]]
                    ) -> List[Tuple[str, Dict[str, List[str]]]]:
    """[(spec, {literal: probes})]: the specs reading a checked-in artefact that have no row, in C order."""
    rows = {columns[0] for columns in registry_rows(registry, repo.files)}
    out: List[Tuple[str, Dict[str, List[str]]]] = []
    for path in repo.tracked:
        match = SPEC_PATH.match(path)
        if not match:
            continue
        spec = f"{match.group(1)}:{match.group(2).replace('/', '.')}"
        if spec in rows or not repo.files.is_file(path):
            continue
        source = repo.files.read_text(path)
        per = reads(source, repo, ignored_of)
        if per or OPENER.search(source):
            out.append((spec, per))
    return sorted(out, key=lambda row: row[0].encode())


def suggested(per: Dict[str, List[str]], files: Files) -> str:
    """The reads a row would declare: a directory read as everything under it; a resource as the files it resolved to."""
    globs = sorted({literal.rstrip("/") + "/*" if files.is_dir(literal) else literal if files.exists(literal) else p
                    for literal, found in per.items() for p in found})
    return " ".join(globs) or "?"


def report(args: Sequence[str], registry: str, repo: Repo, ignored_of: Callable[[List[str]], Set[str]],
           selected_specs: Callable[[str], Set[str]]) -> List[str]:
    """The lines a mode prints (the module docstring's modes); none for a usage error, said by the caller."""
    mode, rest = (args[0], list(args[1:])) if args else ("", [])
    if mode == "--missing":
        return [spec for spec, _ in rowless_readers(registry, repo, ignored_of)]
    if mode == "--suggest":
        return [f"{spec}\t{suggested(per, repo.files)}" for spec, per in rowless_readers(registry, repo, ignored_of)]
    if mode == "--reads":
        return [f"{spec}\t{literal}\t{' '.join(found)}" for spec in rest
                for literal, found in reads(repo.files.read_text(spec_source(spec)), repo, ignored_of).items()]
    return misses(registry, set(rest), repo, ignored_of, selected_specs)


MODES = ("", "--missing", "--suggest", "--reads", "--only")


def main(args: List[str]) -> int:
    files, git = Files.here(), Git.here()
    registry = "scripts/hooks/lint-registry.tsv"
    if args[:1] == ["--registry"]:
        registry, args = args[1], args[2:]
    elif args and not args[0].startswith("--"):  # the positional form
        registry, args = args[0], args[1:]
    if not files.is_file(registry):
        return 0
    if (args[0] if args else "") not in MODES:
        print(__doc__, file=sys.stderr)
        return 2
    selected = hook_selection(shared_runner(), machine.environment())
    repo = Repo(git.lines("ls-files"), files)
    print("".join(line + "\n" for line in report(args, registry, repo, git_ignored(git), selected)), end="")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
