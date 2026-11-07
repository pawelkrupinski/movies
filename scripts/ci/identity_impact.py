#!/usr/bin/env python3
"""IDENTITY TEST-IMPACT SELECTION: of the identity area's local tests (scripts/hooks/local-tests.txt's `identity` set),
only what a change can affect -- the specs it can reach, over the countries it can reach. Never a test that could fail.

Two halves, each sound on its own:

  THE SPECS (`plan`): sbt's `identityImpact` task (project/TestImpact.scala) reads the compiler's dependency record and
  writes the test classes any changed class reaches, or the changed data's readers reach (by the names their string
  literals give it), or `ALL <why>` (a build change, a deleted source, a resource no literal names). `plan` narrows each
  of the set's test lines (`<m>/test`, `testOnly <globs>`, `<m>/testOnly <globs>`) to those classes, one
  `<m>/testOnly` per module, each class once (the set ran identity's specs twice: `identity/test`, then root
  `testOnly services.identity.*`).

  THE COUNTRIES (`countries`): what each changed path can reach, by country --
    - title-rules.tsv: per country, the rows its process loads (TitleRuleSet.forCountry: `countries` blank or naming
      it), a venue row (`cinema`) only where a venue of that country keys to it (TitleRuleKey: a chain's prefix or the
      display name's slug, matched against data/<country>/ and models/Cinema.scala's sections). A country whose loaded
      rows read alike, in the same order, is not reached. A venue key no roster holds reaches every country;
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

Proof: scripts/identity-impact-check (replay of history, mutations).
"""

import argparse
import fnmatch
import json
import os
import re
import subprocess
import sys
import unicodedata

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
CINEMA_SCALA = "common/src/main/scala/models/Cinema.scala"
CHAIN_PREFIXES = {"cinema-city": "Cinema City", "helios": "Helios", "multikino": "Multikino"}
CC = "(pl|uk|de|us|es)"


def git(*args, check=True):
    return subprocess.run(["git", *args], capture_output=True, text=True, check=check).stdout


def show(rev, path):
    """The file at a revision (None: the working tree), or None where it is absent."""
    if rev is None:
        try:
            with open(path, encoding="utf-8") as fh:
                return fh.read()
        except (OSError, UnicodeDecodeError):
            return None
    r = subprocess.run(["git", "show", f"{rev}:{path}"], capture_output=True, text=True)
    return r.stdout if r.returncode == 0 else None


# ── the venue keys (TitleRuleKey) ────────────────────────────────────────────────────────────────────────────────────

def skeleton(text):
    """Letters and digits only, accents off, the letters NFKD keeps whole folded by hand: what a slug of `text` can be
    told apart by. A slug (Slugify.stable) folds no more than this keeps, so a venue's key and its name agree here."""
    t = text.replace("ß", "ss").replace("ẞ", "ss").replace("ł", "l").replace("Ł", "l").replace("ø", "o").replace("Ø", "o") \
        .replace("æ", "ae").replace("Æ", "ae").replace("đ", "d").replace("Đ", "d").replace("œ", "oe").replace("ı", "i")
    t = unicodedata.normalize("NFKD", t)
    return re.sub(r"[^a-z0-9]", "", "".join(c for c in t if not unicodedata.combining(c)).lower())


def skeletons(text):
    """`skeleton`, and the same with a ß dropped (Slugify.stable drops it: "Große" keys as "groe")."""
    return {skeleton(text), skeleton(text.replace("ß", "").replace("ẞ", ""))}


def roster_names(tree):
    """(country, name) for every venue name the rosters hold: every string of data/<country>/**.json, and the
    Cinema(...) names and case objects of models/Cinema.scala, each by the country section it sits in."""
    out = []
    for d, cc in DATA_DIRS.items():
        base = os.path.join(tree, "data", d)
        for root, _, files in os.walk(base):
            for f in files:
                if not f.endswith(".json"):
                    continue
                try:
                    with open(os.path.join(root, f), encoding="utf-8") as fh:
                        doc = json.load(fh)
                except (OSError, ValueError):
                    continue
                stack = [doc]
                while stack:
                    v = stack.pop()
                    if isinstance(v, dict):
                        stack.extend(v.values()); stack.extend(v.keys())
                    elif isinstance(v, list):
                        stack.extend(v)
                    elif isinstance(v, str) and len(v) < 120:
                        out.append((cc, v))
    path = os.path.join(tree, CINEMA_SCALA)
    if os.path.exists(path):
        cc = "pl"  # the hand-written Polish sections come first; each data-driven country's section names it
        with open(path, encoding="utf-8") as fh:
            for line in fh:
                if line.startswith("// ──"):
                    for marker, code in (("United Kingdom", "uk"), ("Germany", "de"), ("United States", "us"), ("Spain", "es")):
                        if marker in line:
                            cc = code
                for name, display in re.findall(r"case object (\w+) extends Cinema\(\"([^\"]*)\"", line):
                    out.append((cc, display))
                    out.append((cc, re.sub(r"([a-z0-9])([A-Z])", r"\1-\2", name)))
    return out


class Rosters:
    """Every roster name by its skeletons (`roster_names`), indexed once: which countries hold a venue keyed `key`."""

    def __init__(self, tree):
        self.by_skeleton = {}
        for cc, name in roster_names(tree):
            for sk in skeletons(name):
                self.by_skeleton.setdefault(sk, set()).add(cc)
        self.cache = {}

    def venue_countries(self, key):
        """The countries a venue rule keyed `key` can fire in: every country holding a venue that keys to it. None when
        no roster holds one (its venue is unknown: every country)."""
        if key not in self.cache:
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

def rule_rows(text):
    return [line for line in (text or "").splitlines() if line.strip() and not line.startswith("#")]


def title_rules_reach(base_text, head_text, tree):
    """The countries whose loaded title rules differ between the two tables (the module docstring). A row on both sides
    alike is read as loaded everywhere, its venue never looked up: a row both lists hold, added to both, hides no
    difference between them (taking it out of both again gives the lists back), so only the changed rows' venues are."""
    every = set(COUNTRIES)
    before_rows, after_rows = rule_rows(base_text), rule_rows(head_text)
    changed = set(before_rows) ^ set(after_rows)
    rosters = None

    def loads(rows):
        nonlocal rosters
        per = {cc: [] for cc in COUNTRIES}
        for row in rows:
            cols = row.split("\t")
            scope = {c.strip() for c in re.split(r"[,\s]+", cols[3]) if c.strip()} if len(cols) > 3 else set()
            cinema = cols[2].strip() if len(cols) > 2 else ""
            reach = scope & every if scope else set(every)
            if cinema and row in changed:
                if rosters is None:
                    rosters = Rosters(tree)
                venues = rosters.venue_countries(cinema)
                if venues is not None:
                    reach &= venues
            for cc in reach:
                per[cc].append(row)
        return per

    before, after = loads(before_rows), loads(after_rows)
    return {cc for cc in COUNTRIES if before[cc] != after[cc]}


def changed_rows(base_text, head_text):
    """The lines added or removed, comments and blanks aside."""
    a, b = rule_rows(base_text), rule_rows(head_text)
    from collections import Counter
    ca, cb = Counter(a), Counter(b)
    return list((ca - cb).elements()) + list((cb - ca).elements())


def row_file_reach(base_text, head_text):
    """A country-keyed row file: the countries of its added and removed rows; a changed header: every country."""
    def header(text):
        rows = rule_rows(text)
        return rows[0] if rows else None
    if header(base_text) != header(head_text) and base_text is not None and head_text is not None:
        return set(COUNTRIES)
    out = set()
    for row in changed_rows(base_text, head_text):
        cc = row.split("\t", 1)[0].strip()
        if cc == "country":
            continue
        if cc not in COUNTRIES:
            return set(COUNTRIES)
        out.add(cc)
    return out


def path_reach(path, base, head, tree):
    """The countries a change to `path` can reach (the module docstring)."""
    every = set(COUNTRIES)
    if path.endswith(".md") or path.startswith(("docs/", ".claude/", "http/")):
        return set()
    if path == TITLE_RULES:
        return title_rules_reach(show(base, path), show(head, path), tree)
    if path == TITLE_WORDS:
        return every if changed_rows(show(base, path), show(head, path)) else set()
    if path in ROW_FILES:
        return row_file_reach(show(base, path), show(head, path))
    for pattern in (rf"^test/resources/fixtures/identity-unmatched/{CC}\.[^/]+$",
                    rf"^test/resources/fixtures/identity-slices/{CC}-[^/]+/",
                    rf"^test/resources/fixtures/corpus/[^/]+-{CC}(-sample)?\.[^/]+$",
                    rf"/services/cinemas/{CC}/"):
        m = re.search(pattern, path)
        if m:
            return {m.group(1)}
    m = re.match(r"^data/([^/]+)/", path)
    if m and m.group(1) in DATA_DIRS:
        return {DATA_DIRS[m.group(1)]}
    return every


def countries(base, head, paths, tree="."):
    reach = set()
    for path in paths:
        reach |= path_reach(path, base, head, tree)
        if len(reach) == len(COUNTRIES):
            break
    return [cc for cc in COUNTRIES if cc in reach]


# ── the plan ─────────────────────────────────────────────────────────────────────────────────────────────────────────

def read_impact(path):
    """(reasons for ALL, {module: set of test classes})."""
    why, tests = [], {}
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            line = line.strip()
            if not line:
                continue
            if line.startswith("ALL"):
                why.append(line[4:])
            else:
                module, cls = line.split(" ", 1)
                tests.setdefault(module, set()).add(cls)
    return why, tests


def universe(line, modules):
    """(module, globs) pairs a set line runs, or None for a line that is no test run of classes."""
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


def plan(lines, why, tests, scope):
    suffix = f" -- -Didentity.countries={','.join(scope)}" if scope is not None and len(scope) < len(COUNTRIES) else ""
    if why:
        return [line + suffix if line.split()[0].endswith("testOnly") and "--" not in line.split() else line for line in lines]
    chosen, rest = {}, []
    modules = sorted(tests)
    for line in lines:
        pairs = universe(line, modules)
        if pairs is None:
            rest.append(line)
            continue
        for module, globs in pairs:
            for cls in tests.get(module, ()):
                if any(fnmatch.fnmatchcase(cls, g) for g in globs):
                    chosen.setdefault(module, set()).add(cls)
    out = []
    for m in sorted(chosen):
        groups = {}
        for cls in sorted(chosen[m]):
            groups.setdefault(fork_group(m, cls, scope), []).append(cls)
        out += [f"{m}/testOnly {' '.join(groups[g])}{suffix}" for g in sorted(groups)]
    return rest + out


def fork_group(module, cls, scope):
    """Which test JVM a selected class runs in: whenever it runs (Poland or Spain in scope, the captures it replays),
    the worker's refit measure apart, as Main's `refit` row runs it (build.sbt's RefitRowSuites) -- in one 2 GB fork
    with the other capture suites alone (the ratchet, the non-film events, the slices: what a capture change selects)
    over every country, three suites replaying the largest captures at once, they ran out of heap. Outside its
    countries it cancels itself (CountryScope), so it shares the one fork: a fork more is its JVM's start."""
    apart = module == "worker" and cls == "scripts.IdentityRefitMeasureAllSpec" and \
        (scope is None or "pl" in scope or "es" in scope)
    return "1-refit" if apart else "2"


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    c = sub.add_parser("countries")
    c.add_argument("base"); c.add_argument("head")
    c.add_argument("--tree", default=".", help="the checkout whose rosters key the venue rules (default: here)")
    p = sub.add_parser("plan")
    p.add_argument("--impact", required=True)
    p.add_argument("--countries", help="the countries in scope, comma-separated (default: every country)")
    a = ap.parse_args(argv)
    if a.cmd == "countries":
        paths = [l.strip() for l in sys.stdin if l.strip()] if not sys.stdin.isatty() else \
            git("diff", "--name-only", "--no-renames", a.base, a.head).split()
        print(",".join(countries(a.base, a.head, paths, a.tree)))
        return 0
    why, tests = read_impact(a.impact)
    scope = None if a.countries is None else [cc for cc in a.countries.split(",") if cc]
    for line in plan([l.rstrip("\n") for l in sys.stdin if l.strip()], why, tests, scope):
        print(line)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
