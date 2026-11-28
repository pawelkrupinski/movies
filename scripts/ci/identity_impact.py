#!/usr/bin/env python3
"""IDENTITY TEST-IMPACT SELECTION: of the identity area's local tests (scripts/hooks/local-tests.txt's `identity` set),
only what a change can affect -- the specs it can reach, over the countries it can reach. Never a test that could fail.

Two halves, each sound on its own:

  THE SPECS (`plan`): sbt's `identityImpact` task (project/TestImpact.scala) reads the compiler's dependency record and
  writes the test classes any changed class reaches, or the changed data's readers reach (by the names their string
  literals give it), or `ALL <why>` (a build change, a deleted source, a resource no literal names). `plan` narrows each
  of the set's test lines (`<m>/test`, `testOnly <globs>`, `<m>/testOnly <globs>`) to those classes, one
  `<m>/testOnly` per module, each class once (the set ran identity's specs twice: `identity/test`, then root
  `testOnly services.identity.*`) -- under ALL too, over every test class (`--every-test`: identityImpact's `<out>.tests`).

  THE COUNTRIES (`countries`): what each changed path can reach, by country --
    - title-rules.tsv: per country, the rows its process loads (TitleRuleSet.forCountry: `countries` blank or naming
      it), a venue row (`cinema`) only where a venue of that country keys to it (TitleRuleKey: a chain's prefix or the
      display name's slug, matched against data/<country>/ and models/' hand-written Polish venues). A country whose
      loaded rows read alike, in the same order, is not reached. A venue key no roster holds reaches every country;
    - title-words.tsv: EVERY country (TitleWords.tokens folds every language's words into one set: DecorationWords,
      StageHouse, BillingPieces read a German word in a Polish title too), unless only comments changed;
    - a fixture of one country: identity-unmatched/<cc>.*, identity-slices/<cc>-*/, corpus/*-<cc>[-sample].*;
    - a row file keyed by country (identity-judgements.tsv, expected-matches.tsv, model-take-audit.tsv,
      review-candidates.tsv, hard-clusters-seeds.tsv): the countries of the rows added or removed; its header: all;
    - */services/cinemas/<cc>/ and data/<country>/: that country;
    - Markdown, docs/, .claude/: none;
    - http/ (the HTTP leaf module): none -- no identity digest lists an http/ source (IdentityRulesVersionSpec holds
      that), so no take can move by one; the HTTP fetchers left in common reach every country as any other path;
    - any other path: every country.
  The countries scope only the specs that mix in tools.CountryScope (each check of which decomposes by country); every
  other selected spec runs whole. `scripts/identity-measure`'s footprint reads the same answer.

Usage:
  identity_impact.py countries <base> <head> [< paths]   the countries, comma-separated (pl,uk,de,us,es order), or ""
  identity_impact.py plan --impact FILE [--countries CSV] < set's sbt lines   the sbt commands to run, one a line
      [--reads FILE]  and the READS classes (a test-side reader of a changed file reaches) any of FILE's lines runs
      [--except FILE] less the `<module>:<class>` classes another line of the same run runs
      [--every-test FILE]  every test class (`<module> <class>` lines): ALL runs each of them once, by name

Proof: scripts/identity-impact-check (replay of history, mutations). The revisions are read through a
`tool_git.GitRepo` (CLAUDE.md, "Side effects live at the leaves"): `main` hands the checkout's, the tests an
InMemoryGitRepo.
"""

import argparse
import fnmatch
import functools
import json
import os
import re
import sys
import typing
import unicodedata
from typing import AbstractSet, Callable, Dict, Iterable, List, Optional, Sequence, Set, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import tool_git  # noqa: E402

Classes = Dict[str, Set[str]]   # module -> test classes
COUNTRIES = ["pl", "uk", "de", "us", "es"]
DATA_DIRS = {"pl": "pl", "germany": "de", "uk": "uk", "us": "us", "spain": "es"}
ROW_FILES = {
    "test/resources/fixtures/identity-judgements.tsv",
    "test/resources/fixtures/identity-unmatched/expected-matches.tsv",
    "test/resources/fixtures/identity-unmatched/model-take-audit.tsv",
    "test/resources/fixtures/identity-unmatched/review-candidates.tsv",
    "test/resources/fixtures/corpus/hard-clusters-seeds.tsv",
}
TITLE_RULES = "common/src/main/resources/title-rules.tsv"
TITLE_WORDS = "common/src/main/resources/title-words.tsv"
MODELS_SCALA = "common/src/main/scala/models"
CHAIN_PREFIXES = {"cinema-city": "Cinema City", "helios": "Helios", "multikino": "Multikino"}
CC = "(pl|uk|de|us|es)"


def working_file(path: str) -> "str | None":
    """The working tree's file, or None where it is absent or no text."""
    try:
        with open(path, encoding="utf-8") as fh:
            return fh.read()
    except (OSError, UnicodeDecodeError):
        return None


def show(rev: "str | None", path: str, repo: tool_git.GitRepo) -> "str | None":
    """The file at a revision (None: the working tree), or None where it is absent."""
    return working_file(path) if rev is None else repo.show(rev, path)


# ── the venue keys (TitleRuleKey) ────────────────────────────────────────────────────────────────────────────────────

def skeleton(text: str) -> str:
    """Letters and digits only, accents off, the letters NFKD keeps whole folded by hand: what a slug of `text` can be
    told apart by. A slug (Slugify.stable) folds no more than this keeps, so a venue's key and its name agree here."""
    t = text.replace("ß", "ss").replace("ẞ", "ss").replace("ł", "l").replace("Ł", "l").replace("ø", "o").replace("Ø", "o") \
        .replace("æ", "ae").replace("Æ", "ae").replace("đ", "d").replace("Đ", "d").replace("œ", "oe").replace("ı", "i")
    t = unicodedata.normalize("NFKD", t)
    return re.sub(r"[^a-z0-9]", "", "".join(c for c in t if not unicodedata.combining(c)).lower())


def skeletons(text: str) -> Set[str]:
    """`skeleton`, and the same with a ß dropped (Slugify.stable drops it: "Große" keys as "groe")."""
    return {skeleton(text), skeleton(text.replace("ß", "").replace("ẞ", ""))}


def _json_strings(doc: object) -> List[str]:
    """Every string of a JSON document, keys included, shorter than a venue name can be."""
    out: List[str] = []
    stack: List[object] = [doc]
    while stack:
        v = stack.pop()
        if isinstance(v, dict):
            stack.extend(typing.cast("Dict[str, object]", v).values())
            stack.extend(typing.cast("Dict[str, object]", v).keys())
        elif isinstance(v, list):
            stack.extend(typing.cast("List[object]", v))
        elif isinstance(v, str) and len(v) < 120:
            out.append(v)
    return out


def _load_json(path: str) -> object:
    """The JSON document at `path`, None where it cannot be read or parsed."""
    try:
        with open(path, encoding="utf-8") as fh:
            return json.load(fh)
    except (OSError, ValueError):
        return None


def _data_roster_names(tree: str) -> List[Tuple[str, str]]:
    """(country, name) for every string of data/<country>/**.json that reads as JSON."""
    return [(cc, v)
            for directory, cc in DATA_DIRS.items()
            for root, _, files in os.walk(os.path.join(tree, "data", directory))
            for f in files if f.endswith(".json")
            for v in _json_strings(_load_json(os.path.join(root, f)))]


def _models_roster_names(tree: str) -> List[Tuple[str, str]]:
    """(country, name) of the Cinema(...) names and case objects of models/*.scala: every hand-written venue is Poland's
    or a chain network's; the rest are data-driven."""
    out: List[Tuple[str, str]] = []
    models = os.path.join(tree, MODELS_SCALA)
    for f in sorted(os.listdir(models)) if os.path.isdir(models) else []:
        if not f.endswith(".scala"):
            continue
        with open(os.path.join(models, f), encoding="utf-8") as fh:
            for line in fh:
                for name, display in re.findall(r"case object (\w+)\s+extends\s+Cinema\(\"([^\"]*)\"", line):
                    out.append(("pl", display))
                    out.append(("pl", re.sub(r"([a-z0-9])([A-Z])", r"\1-\2", name)))
    return out


def roster_names(tree: str) -> List[Tuple[str, str]]:
    """(country, name) for every venue name the rosters hold: every string of data/<country>/**.json, and the
    Cinema(...) names and case objects of models/*.scala (Poland's `*Cinemas.scala`, the networks')."""
    return _data_roster_names(tree) + _models_roster_names(tree)


class Rosters:
    """Every roster name by its skeletons (`roster_names`), indexed once: which countries hold a venue keyed `key`."""

    def __init__(self, tree: str) -> None:
        self.by_skeleton: Dict[str, Set[str]] = {}
        for cc, name in roster_names(tree):
            for sk in skeletons(name):
                self.by_skeleton.setdefault(sk, set()).add(cc)
        self.cache: Dict[str, Optional[Set[str]]] = {}

    def venue_countries(self, key: str) -> Optional[Set[str]]:
        """The countries a venue rule keyed `key` can fire in: every country holding a venue that keys to it. None when
        no roster holds one (its venue is unknown: every country)."""
        if key not in self.cache:
            found: Set[str]
            if key == "bok":
                found = {"pl"}
            elif key in CHAIN_PREFIXES:
                prefix = skeleton(CHAIN_PREFIXES[key])
                found = {cc for sk, ccs in self.by_skeleton.items() if sk.startswith(prefix) for cc in ccs}
            else:
                found = set(self.by_skeleton.get(skeleton(key), set()))
            self.cache[key] = found or None
        return self.cache[key]


# ── the countries ────────────────────────────────────────────────────────────────────────────────────────────────────

def rule_rows(text: Optional[str]) -> List[str]:
    return [line for line in (text or "").splitlines() if line.strip() and not line.startswith("#")]


def _row_reach(row: str, changed: AbstractSet[str], venues_of: Callable[[str], Optional[Set[str]]]) -> Set[str]:
    """The countries a title-rules row loads in: its `countries` column (blank: every), narrowed to those holding a
    venue its `cinema` keys to -- looked up only for a changed row."""
    cols = row.split("\t")
    scope = {c.strip() for c in re.split(r"[,\s]+", cols[3]) if c.strip()} if len(cols) > 3 else set()
    cinema = cols[2].strip() if len(cols) > 2 else ""
    reach = scope & set(COUNTRIES) if scope else set(COUNTRIES)
    venues = venues_of(cinema) if cinema and row in changed else None
    return reach & venues if venues is not None else reach


def _loads(rows: Iterable[str], changed: AbstractSet[str], venues_of: Callable[[str], Optional[Set[str]]]) -> Dict[str, List[str]]:
    """The rows each country's process loads, in order."""
    per: Dict[str, List[str]] = {cc: [] for cc in COUNTRIES}
    for row in rows:
        for cc in _row_reach(row, changed, venues_of):
            per[cc].append(row)
    return per


def title_rules_reach(base_text: Optional[str], head_text: Optional[str], tree: str) -> Set[str]:
    """The countries whose loaded title rules differ between the two tables (the module docstring). A row on both sides
    alike is read as loaded everywhere, its venue never looked up: a row both lists hold, added to both, hides no
    difference between them (taking it out of both again gives the lists back), so only the changed rows' venues are."""
    before_rows, after_rows = rule_rows(base_text), rule_rows(head_text)
    changed = set(before_rows) ^ set(after_rows)

    @functools.lru_cache(maxsize=None)
    def rosters() -> Rosters:
        return Rosters(tree)

    def venues_of(cinema: str) -> Optional[Set[str]]:
        return rosters().venue_countries(cinema)

    before, after = _loads(before_rows, changed, venues_of), _loads(after_rows, changed, venues_of)
    return {cc for cc in COUNTRIES if before[cc] != after[cc]}


def changed_rows(base_text: Optional[str], head_text: Optional[str]) -> List[str]:
    """The lines added or removed, comments and blanks aside."""
    a, b = rule_rows(base_text), rule_rows(head_text)
    from collections import Counter
    ca, cb = Counter(a), Counter(b)
    return list((ca - cb).elements()) + list((cb - ca).elements())


def row_file_reach(base_text: Optional[str], head_text: Optional[str]) -> Set[str]:
    """A country-keyed row file: the countries of its added and removed rows; a changed header: every country."""
    def header(text: Optional[str]) -> Optional[str]:
        rows = rule_rows(text)
        return rows[0] if rows else None
    if header(base_text) != header(head_text) and base_text is not None and head_text is not None:
        return set(COUNTRIES)
    out: Set[str] = set()
    for row in changed_rows(base_text, head_text):
        cc = row.split("\t", 1)[0].strip()
        if cc == "country":
            continue
        if cc not in COUNTRIES:
            return set(COUNTRIES)
        out.add(cc)
    return out


COUNTRY_PATHS = (rf"^test/resources/fixtures/identity-unmatched/{CC}\.[^/]+$",
                 rf"^test/resources/fixtures/identity-slices/{CC}-[^/]+/",
                 rf"^test/resources/fixtures/corpus/[^/]+-{CC}(-sample)?\.[^/]+$",
                 rf"/services/cinemas/{CC}/")


def _fixture_country(path: str) -> Optional[str]:
    """The one country a fixture, a venue's code or a data directory of `path` belongs to, if it names one."""
    for pattern in COUNTRY_PATHS:
        m = re.search(pattern, path)
        if m:
            return m.group(1)
    m = re.match(r"^data/([^/]+)/", path)
    return DATA_DIRS.get(m.group(1)) if m else None


def path_reach(path: str, base: str, head: "str | None", tree: str, repo: tool_git.GitRepo) -> "set[str]":
    """The countries a change to `path` can reach (the module docstring)."""
    if path.endswith(".md") or path.startswith(("docs/", ".claude/", "http/")):
        return set()
    if path == TITLE_RULES:
        return title_rules_reach(show(base, path, repo), show(head, path, repo), tree)
    if path == TITLE_WORDS:
        return set(COUNTRIES) if changed_rows(show(base, path, repo), show(head, path, repo)) else set()
    if path in ROW_FILES:
        return row_file_reach(show(base, path, repo), show(head, path, repo))
    country = _fixture_country(path)
    return {country} if country else set(COUNTRIES)


def countries(base: str, head: "str | None", paths: "list[str]", tree: str, repo: tool_git.GitRepo) -> "list[str]":
    """The countries the changed `paths` reach, base to head (None: the working tree), read through `repo`."""
    reach: "set[str]" = set()
    for path in paths:
        reach |= path_reach(path, base, head, tree, repo)
        if len(reach) == len(COUNTRIES):
            break
    return [cc for cc in COUNTRIES if cc in reach]


# ── the plan ─────────────────────────────────────────────────────────────────────────────────────────────────────────

def read_impact(path: str) -> Tuple[List[str], Classes]:
    """(reasons for ALL, {module: set of test classes}); no file -- identityImpact gave no answer -- exits 1, saying so
    in one line (the hook then runs every identity test)."""
    why: List[str] = []
    tests: Classes = {}
    if not os.path.isfile(path):
        raise SystemExit(f"identity_impact.py: no answer from identityImpact: {path} was not written")
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            line = line.strip()
            if not line:
                continue
            if line.startswith("ALL"):
                why.append(line[4:])
            elif line.startswith("READS "):
                continue  # read_reads
            else:
                module, cls = line.split(" ", 1)
                tests.setdefault(module, set()).add(cls)
    return why, tests


def read_reads(path: str) -> Classes:
    """{module: set of test classes} of the `READS` lines: what a test-side reader of a changed file reaches, in any
    module (TestImpact.select)."""
    reads: Classes = {}
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            words = line.split()
            if len(words) == 3 and words[0] == "READS":
                reads.setdefault(words[1], set()).add(words[2])
    return reads


def read_excluded(path: str) -> Set[Tuple[str, ...]]:
    """{(module, class)} of `<module>:<class>` lines (pre-push's lint plan): classes another line of the run runs."""
    with open(path, encoding="utf-8") as fh:
        return {tuple(l.strip().split(":", 1)) for l in fh if ":" in l}


def universe(line: str, modules: List[str]) -> Optional[List[Tuple[str, List[str]]]]:
    """(module, globs) pairs a set line runs, or None for a line that is no test run of classes. A glob may exclude
    (`-services.identity.*`, as sbt's testOnly reads it): see `runs`."""
    words = line.split()
    if not words or "--" in words:
        return None
    m = re.fullmatch(r"(?:(\w+)/)?(?:Test/)?(test|testOnly)", words[0])
    if not m:
        return None
    module, task = m.group(1), m.group(2)
    globs = ["*"] if task == "test" else words[1:]
    if task == "test" and len(words) > 1:
        return None
    return [(mod, globs) for mod in ([module] if module else modules)]


def _choose(selected: Classes, among: Iterable[str], modules: List[str], excluded: AbstractSet[Tuple[str, ...]],
            chosen: Classes) -> List[str]:
    """Adds to `chosen` the `selected` classes any of the `among` lines runs; the lines that run no classes."""
    rest: List[str] = []
    for line in among:
        pairs = universe(line, modules)
        if pairs is None:
            rest.append(line)
            continue
        for module, globs in pairs:
            chosen.setdefault(module, set()).update(
                cls for cls in selected.get(module, ()) if runs(cls, globs) and (module, cls) not in excluded)
    return rest


def _testonly_lines(chosen: Classes, scope: Optional[Sequence[str]], suffix: str) -> List[str]:
    """One `<m>/testOnly` per module and test JVM (`fork_group`), each class once."""
    out: List[str] = []
    for m in sorted(chosen):
        groups: Dict[str, List[str]] = {}
        for cls in sorted(chosen[m]):
            groups.setdefault(fork_group(m, cls, scope), []).append(cls)
        out += [f"{m}/testOnly {' '.join(groups[g])}{suffix}" for g in sorted(groups)]
    return out


def _countries_suffix(scope: Optional[Sequence[str]]) -> str:
    """The sbt argument narrowing a CountryScope spec to `scope`; none when it holds every country."""
    return f" -- -Didentity.countries={','.join(scope)}" if scope is not None and len(scope) < len(COUNTRIES) else ""


def _scoped(line: str, suffix: str) -> str:
    """A testOnly line with no arguments of its own after `--` takes the countries `suffix`; any other line stays."""
    words = line.split()
    return line + suffix if words[0].endswith("testOnly") and "--" not in words else line


def plan(lines: List[str], why: List[str], tests: Classes, scope: Optional[Sequence[str]], reads: Optional[Classes] = None,
         reads_lines: Sequence[str] = (), excluded: AbstractSet[Tuple[str, ...]] = frozenset(),
         every_test: Optional[Classes] = None) -> List[str]:
    """`lines` narrowed to the selected `tests`; plus the `reads` classes (read_reads) any of `reads_lines` runs, in
    whichever module (the combined-tree verification's: every module's unit lines); less the `excluded` (module, class)
    pairs, which another line of the same run runs. ALL (`why`) selects every test: narrowed to `every_test` class
    (identityImpact's `<out>.tests`), each class once and by name -- the lines as they are ran identity's specs twice
    (`identity/test`, then root `testOnly services.identity.*`) and left the landing check's shards
    (scripts/ci/shard_tests.py) two lines no class weight could split; without it, the lines as they are."""
    suffix = _countries_suffix(scope)
    if why and every_test:
        return plan(lines, [], every_test, scope, reads=reads, reads_lines=reads_lines, excluded=excluded)
    if why:
        return [_scoped(line, suffix) for line in lines]
    chosen: Classes = {}
    modules = sorted(set(tests) | set(reads or {}))
    rest = _choose(tests, lines, modules, excluded, chosen)
    _choose(reads or {}, reads_lines, modules, excluded, chosen)
    return rest + _testonly_lines({m: c for m, c in chosen.items() if c}, scope, suffix)


def runs(cls: str, globs: Sequence[str]) -> bool:
    """Whether a testOnly of `globs` runs `cls`: a glob it matches, and no excluding (`-`) one."""
    return any(fnmatch.fnmatchcase(cls, g) for g in globs if not g.startswith("-")) and \
        not any(fnmatch.fnmatchcase(cls, g[1:]) for g in globs if g.startswith("-"))


def fork_group(module: str, cls: str, scope: Optional[Sequence[str]]) -> str:
    """Which test JVM a selected class runs in: whenever it runs (Poland or Spain in scope, the captures it replays),
    the worker's refit measure apart, as Main's `refit` row runs it (build.sbt's RefitRowSuites) -- in one 2 GB fork
    with the other capture suites alone (the ratchet, the non-film events, the slices: what a capture change selects)
    over every country, three suites replaying the largest captures at once, they ran out of heap. Outside its
    countries it cancels itself (CountryScope), so it shares the one fork: a fork more is its JVM's start."""
    apart = module == "worker" and cls == "scripts.IdentityRefitMeasureAllSpec" and \
        (scope is None or "pl" in scope or "es" in scope)
    return "1-refit" if apart else "2"


def piped_paths(stdin: "typing.TextIO") -> "list[str]":
    """The paths a pipe gives, one a line; none from a terminal."""
    return [] if stdin.isatty() else [line.strip() for line in stdin if line.strip()]


def changed_paths(base: str, head: str, piped: "list[str]", repo: tool_git.GitRepo) -> "list[str]":
    """The changed paths: stdin's, else the diff's -- an empty or detached stdin is no "no change" (fail safe, never "none")."""
    return piped or repo.changed(base, head)


DOCSTRING = __doc__ or ""  # None under python -OO


def _parse(argv: List[str]) -> argparse.Namespace:
    ap = argparse.ArgumentParser(description=DOCSTRING.split("\n")[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    c = sub.add_parser("countries")
    c.add_argument("base")
    c.add_argument("head")
    c.add_argument("--tree", default=".", help="the checkout whose rosters key the venue rules (default: here)")
    p = sub.add_parser("plan")
    p.add_argument("--impact", required=True)
    p.add_argument("--countries", help="the countries in scope, comma-separated (default: every country)")
    p.add_argument("--reads", help="a file of every module's unit lines: the READS classes any of them runs join the plan")
    p.add_argument("--except", dest="excluded", help="a file of `<module>:<class>` lines another line of the run runs")
    p.add_argument("--every-test", help="identityImpact's `<out>.tests` (every test class): ALL runs each by name, once")
    return ap.parse_args(argv)


def _countries_command(a: argparse.Namespace) -> int:
    repo = tool_git.SubprocessGitRepo.here()
    paths = changed_paths(a.base, a.head, piped_paths(sys.stdin), repo)
    print(",".join(countries(a.base, a.head, paths, a.tree, repo)))
    return 0


def _read_lines(path: str) -> List[str]:
    with open(path, encoding="utf-8") as fh:
        return [line.strip() for line in fh if line.strip()]


def _scope(csv: Optional[str]) -> Optional[List[str]]:
    """The countries in scope: none given is every country (None)."""
    return None if csv is None else [cc for cc in csv.split(",") if cc]


def _plan_command(a: argparse.Namespace) -> int:
    why, tests = read_impact(a.impact)
    every_test = read_impact(a.every_test)[1] if why and a.every_test else None
    scope = _scope(a.countries)
    reads_lines = _read_lines(a.reads) if a.reads else []
    excluded = read_excluded(a.excluded) if a.excluded else frozenset()
    for line in plan([line.rstrip("\n") for line in sys.stdin if line.strip()], why, tests, scope,
                     reads=read_reads(a.impact), reads_lines=reads_lines, excluded=excluded, every_test=every_test):
        print(line)
    return 0


def main(argv: List[str]) -> int:
    a = _parse(argv)
    return _countries_command(a) if a.cmd == "countries" else _plan_command(a)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
