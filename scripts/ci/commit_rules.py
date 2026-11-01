#!/usr/bin/env python3
# THE COMMIT RULES OF THE DRIFT GATE: one walker over a push's commits, two rule sets -- the pre-push hook's
# `allowlists` and `driftguards` checks. Each rule reads one commit (its changes against its FIRST parent, so a merge's
# resolution is judged too, and its trailers) and says why it is refused, or nothing.
#
# `allowlists` -- A NEW CHECK LANDS GREEN; ITS ALLOW-LIST ONLY SHRINKS (convergence-failures-2026-10-29 §4 G1,
# recommendation 2). 15 of 36 red convergence runs and ~43 of 67 red hours in the week to 2026-10-29 came from a check
# turned on while main's own pinned recording already failed it (the drift gate 9bb54c1a4, the flip gate a2c684f80):
#   - a commit giving birth to a check (or tightening one) carries `New-Check: <what>`, and everything the check finds
#     at birth goes onto its allow-list in that commit, each row `TEMPORARY owner: <who>; remove when <fix>; <why>`;
#   - any other commit only REMOVES rows from a registered allow-list (.github/allow-lists.txt);
#   - `New-Check:` alone is no birth: the same commit changes what decides what the gate finds (the drift gate's paths,
#     .github/identity-drift-gate-paths.txt, its lists and baseline aside, or the convergence workflows' rows);
#   - never in a commit carrying `Fixes-Red:`: a red is fixed, not listed;
#   - never, on the flips or drift list, a listing already known: one any earlier revision of either list or the
#     baseline named, or one a committed drift slice holds (test/resources/fixtures/drift-slices/).
#   THE KNOWN-FLIPS BASELINE ONLY SHRINKS (test/resources/fixtures/identity-flips-baseline.tsv; the owner, 2026-10-29:
#   "fail build only on new flips, focus in eliminating all old flips and then fail on all flips"). Rows are written only
#   by its creation, once (a file that existed before is never made again), and by a TIGHTENED CHECK's birth: a commit
#   carrying `New-Check:` that changes what decides what the gate finds, never `Fixes-Red:`, may baseline the flips the
#   tightened check finds at birth, each a listing no list or earlier baseline named (a drift slice's listing may be
#   baselined: the slice holds its past flip; the owner, 2026-10-30: a229501a7's precise `none` revealed 138 flips the
#   blind predicate hid, "baseline them, then burn them down"). Any other added row is refused: no override.
#   A registered list is a TSV whose last column is the row's reason; `#` lines and the header (its first other line)
#   are not rows. Every allow-list file in the repository is registered (AllowListRatchetSpec).
#
# `driftguards` -- the owner, 2026-10-29:
#   - A DRIFT OR FLIP RED IS NOT CLOSED UNTIL ITS SLICE IS COMMITTED. A commit naming a red (`Fixes-Red:`) whose message
#     speaks of drift or flips comes with a drift slice of the listing it fixes -- a change under SLICES
#     (scripts/identity-slice <cc> SELECTOR --emit-drift-slice) anywhere in the same push -- so IdentityDriftSlicesSpec
#     holds the fix from then on (convergence-failures 2026-10-29, G2: E3, E4, E8 and E9's re-breaks were listings that
#     had failed before). A fix whose red has no listing to slice (a CI script, a timeout) says `Drift-Slice: none -- <why>`.
#   - A RED FIX IS NO RESOLVER RULE. A flip or drift is fixed in the incremental state that diverges -- a lag, a budget,
#     the queue's order, a cache, a restart's key -- never by a new identity resolver rule: a rule is identity design,
#     measured on the full corpus in every country. A commit naming a red (`Fixes-Red:`) that changes RULE_FILES carries
#     `Identity-Design: <doc>`, a document that exists in the commit's tree. Mechanism files need none.
#   - A CHANGE TO THE DRIFT HARNESS CARRIES ITS SELF-TEST (G6): a commit changing HARNESS_FILES also changes one of
#     SELF_TEST_FILES -- a third of the week's drift reds were the judge's, not the product's (91cfbe7e8, 7b586af0f,
#     5df3295bd) -- or says `Drift-Harness-Refactor: <why>`.
#
# usage: commit_rules.py allowlists|driftguards --range A..B [--range C..D ...] [--registry .github/allow-lists.txt]
# Prints each refusal and why; exit 0 none, 1 some, 2 usage or an unreadable range. Tested by commit_rules_test.py.

from __future__ import annotations

import gzip
import json
import re
import subprocess
import sys
from dataclasses import dataclass, field
from typing import Callable

REGISTRY = ".github/allow-lists.txt"
DRIFT_LISTS = ("test/resources/fixtures/identity-flips-allow.tsv", "test/resources/fixtures/identity-drift-allow.tsv")
BASELINES = ("test/resources/fixtures/identity-flips-baseline.tsv",)
GATE_PATHS = ".github/identity-drift-gate-paths.txt"
WORKFLOW_ROWS = (".github/workflows/country-convergence-leg.yml", ".github/workflows/identity-model-convergence.yml")
SLICES = "test/resources/fixtures/drift-slices/"
TEMPORARY = re.compile(r"^TEMPORARY\b.*\bowner:\s*\S+.*\bremove when\b\s*\S+", re.I)

_ID = "identity/src/main/scala/services/identity"
RULE_FILES = re.compile(
    rf"^({_ID}/(IdentityResolver|FeedLink|FamilyScope|Acceptance[A-Za-z]*|CandidateScoring|CandidateGeneration|ClusterVoting"
    r"|ConstraintEdges|ConstraintSolver|Corroboration|EvidenceWeights|EvidenceRatingGate|ReleaseVeto|LearnedCannotLinks"
    r"|IdentityCalibration|VenueBacking|SourceReliability|FilmNess|MultiFilmBill|CatalogueLinks|TitleLinks|TitleCredit"
    rf"|CreditRelation|FactRelations)\.scala|{_ID}/agreement/(Agreement|AgreementCandidates|Broadcast|Catalogue|Correction"
    r"|FamilyVerdicts|NonFilmEvents|SynopsisEvidence)\.scala|common/src/main/resources/title-(rules|words)\.tsv"
    r"|common/src/main/scala/services/titlerules/.*)$")
HARNESS_FILES = re.compile(
    r"^(worker/src/test/scala/scripts/IdentityDrift\.scala|worker/src/test/scala/scripts/IdentityLiveDrift\.scala"
    rf"|{_ID}/TakeFlips\.scala|scripts/ci/identity-drift-check\.sh)$")
SELF_TEST_FILES = re.compile(
    r"^(worker/src/test/scala/scripts/IdentityDrift(Spec|SlicesSpec)\.scala|worker/src/test/scala/scripts/IdentityLiveDriftSpec\.scala"
    r"|identity/src/test/scala/services/identity/TakeFlipsSpec\.scala"
    r"|identity/src/test/scala/services/identity/agreement/CorrectionFlipReplaySpec\.scala"
    r"|scripts/ci/identity-drift-check-test\.sh|test/resources/fixtures/drift-slices/.*)$")


def git(*args: str) -> str:
    # core.quotePath=false: a path with a non-ASCII letter (drift-slices/pl/centrum-3d-przemyśl-...) is printed as
    # itself, not as a quoted octal escape that no prefix test matches (2026-11-01: a push's committed Zniknięcie slice
    # read as "no drift slice", landing check run 41275275554)
    return subprocess.run(["git", "-c", "core.quotePath=false", *args], check=True, capture_output=True, text=True).stdout


def at(rev: str, path: str) -> str:
    try:
        return git("show", f"{rev}:{path}")
    except subprocess.CalledProcessError:
        return ""  # absent there


@dataclass
class Commit:
    sha: str
    subject: str
    trailers: dict[str, list[str]]
    changed: set[str]  # against the first parent: a merge's resolution counts
    message: str = ""
    parent: str = field(init=False)

    def __post_init__(self):
        self.parent = f"{self.sha}^"

    @staticmethod
    def read(sha: str) -> "Commit":
        subject = git("log", "-1", "--format=%s", sha).strip()
        trailers: dict[str, list[str]] = {}
        for line in git("log", "-1", "--format=%(trailers:only,unfold)", sha).splitlines():
            key, sep, value = line.partition(":")
            if sep and value.strip():
                trailers.setdefault(key.strip().lower(), []).append(value.strip())
        has_parent = subprocess.run(["git", "rev-parse", "-q", "--verify", f"{sha}^"], capture_output=True).returncode == 0
        names = git("diff", "--name-only", "--no-renames", f"{sha}^", sha) if has_parent \
            else git("diff-tree", "--no-commit-id", "--name-only", "-r", "--root", sha)
        return Commit(sha, subject, trailers, {n for n in names.splitlines() if n}, git("log", "-1", "--format=%B", sha))

    def has(self, key: str) -> bool:
        return bool(self.trailers.get(key.lower()))

    def claims_fix(self) -> bool:
        """`Fixes-Red:` as a claim: a `Red-Unrelated:` commit names the red only to land on it under the owner's call
        and claims no fix (as the pre-push hook reads it), so the red-fix guards do not judge it."""
        return self.has("Fixes-Red") and not self.has("Red-Unrelated")

    def label(self) -> str:
        return f"{self.sha[:9]} {self.subject}"


Rule = Callable[[Commit, dict], list[str]]


# ── allowlists ──────────────────────────────────────────────────────────────────────────────────────────────────────
def rows(text: str) -> list[str]:
    """The data rows of an allow-list: no comment, no blank line, no header (the first other line)."""
    body = [line for line in text.splitlines() if line.strip() and not line.lstrip().startswith("#")]
    return body[1:]


def registered(registry: str) -> list[str]:
    try:
        with open(registry) as f:
            return [line.strip() for line in f if line.strip() and not line.lstrip().startswith("#")]
    except OSError:
        return []


def check_paths(rev: str) -> set[str]:
    """What decides what the drift gate finds, at `rev`: the gate's paths (its lists and baseline aside) and the rows."""
    listed = [line.strip() for line in at(rev, GATE_PATHS).splitlines() if line.strip() and not line.startswith("#")]
    return {p for p in listed if not p.endswith("-allow.tsv") and p not in BASELINES} | set(WORKFLOW_ROWS)


def listing(row: str) -> tuple[str, str, str]:
    """A drift or flips row's listing: country, venue, raw title (a baseline row's seed, its second column, aside)."""
    cols = row.split("\t")
    if len(cols) > 1 and cols[1].strip().isdigit():
        cols = [cols[0]] + cols[2:]
    return tuple(c.strip() for c in (cols + ["", "", ""])[:3])


def known(rev: str, slices_too: bool = True) -> set[tuple[str, str, str]]:
    """The listings known before `rev`: every row any earlier revision of the drift or flips list or the baseline held,
    and every listing a drift slice committed at `rev`'s parent holds -- `slices_too` False: the lists' and the
    baseline's alone (a tightened check's baseline birth: a slice holds a listing's past flip, and the tightened check
    sees a new one of it, baselined while the slice passes it, IdentityDriftSlicesSpec)."""
    out = set()
    for path in DRIFT_LISTS + BASELINES:
        try:
            history = git("log", "--format=%H", f"{rev}^", "--", path).split()
        except subprocess.CalledProcessError:
            history = []
        for old in history:
            out |= {listing(r) for r in rows(at(old, path))}
    if not slices_too:
        return out
    try:
        slices = [p for p in git("ls-tree", "-r", "--name-only", f"{rev}^", "--", SLICES).split("\n") if p.endswith("/slice.json.gz")]
    except subprocess.CalledProcessError:
        slices = []
    for path in slices:
        try:
            doc = json.loads(gzip.decompress(subprocess.run(["git", "show", f"{rev}^:{path}"], check=True, capture_output=True).stdout))
        except (subprocess.CalledProcessError, OSError, ValueError):
            continue
        for seed in doc.get("seeds", []):
            parts = seed.split("\x00")
            raw = parts[3] if parts[0] == "N" and len(parts) > 3 else (parts[2] if len(parts) > 2 else "")
            out.add((doc.get("country", ""), parts[1] if len(parts) > 1 else "", raw))
    return out


def is_known(row: tuple[str, str, str], before: set[tuple[str, str, str]]) -> bool:
    cc, venue, title = row
    return any(k[0] == cc and k[2] == title and (venue == "*" or k[1] == "*" or k[1] == venue) for k in before)


def existed_before(sha: str, path: str) -> bool:
    """Whether any ancestor of `sha` held `path`: a baseline is created once, never made again."""
    try:
        return bool(git("log", "--format=%H", "-1", f"{sha}^", "--", path).strip())
    except subprocess.CalledProcessError:
        return False


def judge(added: list[str], births: bool, fixes_red: bool = False, adds_check: bool = True,
          drift_list: bool = False, before: set | None = None) -> list[str]:
    """Why each row added to an allow-list is refused; empty when every one may land."""
    why = []
    for row in added:
        reason = row.split("\t")[-1].strip()
        if births and fixes_red:
            why.append(f"{row!r}: never in a commit carrying `Fixes-Red:` -- a red is fixed, not listed; a check's birth is a "
                       "commit of its own (the owner, 2026-10-29)")
        elif births and not adds_check:
            why.append(f"{row!r}: `New-Check:` but the commit adds no check -- a TEMPORARY row is a newborn check's only (a new "
                       "gate, row kind or predicate: a change to the drift gate's paths or the convergence rows)")
        elif births and drift_list and is_known(listing(row), before or set()):
            why.append(f"{row!r}: a known flip or divergence (an earlier revision of the drift or flips list or the baseline "
                       "named it, or a committed drift slice holds it) -- a known flip is on the known-flips baseline, to be "
                       "fixed, never a newborn check's TEMPORARY row (the owner, 2026-10-29)")
        elif not births:
            why.append(f"{row!r}: an allow-list only shrinks -- a finding after its check's birth is fixed, never listed "
                       "(a check's birth carries `New-Check: <what>`)")
        elif not TEMPORARY.search(reason):
            why.append(f"{row!r}: a row added at a check's birth is TEMPORARY with an owner and when it goes: "
                       "`TEMPORARY owner: <who>; remove when <fix>; <why>`")
    return why


def allow_lists(c: Commit, opts: dict) -> list[str]:
    refused, before_known = [], None
    births, fixes_red = c.has("New-Check"), c.has("Fixes-Red")
    for path in registered(opts["registry"]):
        if path not in c.changed:
            continue
        before = set(rows(at(c.parent, path)))
        added = [r for r in rows(at(c.sha, path)) if r not in before]
        if path in BASELINES:
            born = not before and not at(c.parent, path) and not existed_before(c.sha, path)
            if not added or born:
                continue
            if not (births and bool(c.changed & check_paths(c.sha)) and not fixes_red):
                refused += [f"{path}: {r!r}: the known-flips baseline only shrinks -- a new flip is fixed, never baselined; "
                            "only a tightened check's birth (`New-Check:`, a change to what decides the gate, no "
                            "`Fixes-Red:`) baselines what it finds (no override)" for r in added]
                continue
            listed_before = known(c.sha, slices_too=False)
            refused += [f"{path}: {r!r}: a known flip or divergence (an earlier revision of a list or the baseline named "
                        "it) -- fixed, never baselined again" for r in added if is_known(listing(r), listed_before)]
            continue
        drift_list = path in DRIFT_LISTS
        if added and drift_list and births and before_known is None:
            before_known = known(c.sha)
        adds_check = bool(c.changed & check_paths(c.sha))
        refused += [f"{path}: {why}" for why in judge(added, births, fixes_red, adds_check, drift_list, before_known)]
    return refused


# ── driftguards ─────────────────────────────────────────────────────────────────────────────────────────────────────
DRIFT_WORDS = re.compile(r"(^|[^a-z])(drift|flip)", re.I)
SLICE_NONE = re.compile(r"^none\s*(--|\u2014|-)\s*\S", re.I)


def drift_fix_carries_its_slice(c: Commit, opts: dict) -> list[str]:
    if not c.claims_fix() or not DRIFT_WORDS.search(c.message):
        return []
    if any(p.startswith(SLICES) for p in opts.get("pushed", ())) or any(SLICE_NONE.match(v) for v in c.trailers.get("drift-slice", [])):
        return []
    return ["fixes a drift/flip red but the push commits no drift slice.\n"
            "  A drift or flip red is not closed until its slice is committed: scripts/identity-slice <cc> --venue V --title T\n"
            "  --emit-drift-slice, then commit test/resources/fixtures/drift-slices/<cc>/<name>/ in this push. No listing to slice\n"
            "  (a CI script, a timeout): add the trailer 'Drift-Slice: none -- <why>'."]


def red_fix_is_no_rule(c: Commit, _opts: dict) -> list[str]:
    rules = sorted(p for p in c.changed if RULE_FILES.match(p))
    if not c.claims_fix() or not rules:
        return []
    docs = c.trailers.get("identity-design", [])
    if docs and all(at(c.sha, d.split()[0]) for d in docs):
        return []
    named = f" (Identity-Design: {', '.join(docs)} names no file of the commit)" if docs else ""
    return [f"fixes a red by changing identity RULE code: {' '.join(rules)}{named}\n"
            "  Red fixers do not add resolver rules (the owner, 2026-10-29): fix the incremental state that diverges (a lag, a\n"
            "  budget, the queue's order, a cache, a restart key). A rule change is identity design, measured on the full corpus\n"
            "  in every country: it lands outside red fixing, or with 'Identity-Design: <doc>' naming the design doc that measured it."]


def harness_carries_self_test(c: Commit, _opts: dict) -> list[str]:
    harness = sorted(p for p in c.changed if HARNESS_FILES.match(p))
    if not harness or any(SELF_TEST_FILES.match(p) for p in c.changed) or c.has("Drift-Harness-Refactor"):
        return []
    return [f"changes the drift harness ({' '.join(harness)}) without its self-test.\n"
            "  A harness change carries a capture-replay case proving the replay behaves as the worker does: extend\n"
            "  IdentityDriftSpec, CorrectionFlipReplaySpec, TakeFlipsSpec or identity-drift-check-test.sh in the same commit.\n"
            "  A pure refactor: the trailer 'Drift-Harness-Refactor: <why>'."]


RULE_SETS: dict[str, list[Rule]] = {
    "allowlists": [allow_lists],
    "driftguards": [drift_fix_carries_its_slice, red_fix_is_no_rule, harness_carries_self_test],
}


def check(rule_set: str, ranges: list[str], opts: dict) -> list[str]:
    refused = []
    for rng in ranges:
        # what the whole push changes: a drift fix's slice may come in a later commit of it
        opts = {**opts, "pushed": set(git("diff", "--name-only", "--no-renames", rng.replace("..", "...", 1)).split("\n")) - {""}}
        for sha in git("rev-list", "--reverse", rng).split():
            c = Commit.read(sha)
            for rule in RULE_SETS[rule_set]:
                refused += [f"{c.label()}: {why}" for why in rule(c, opts)]
    return refused


def main(argv: list[str]) -> int:
    usage = "usage: commit_rules.py allowlists|driftguards --range A..B [--range ...] [--registry FILE]"
    if not argv or argv[0] not in RULE_SETS:
        print(usage, file=sys.stderr)
        return 2
    rule_set, ranges, opts, rest = argv[0], [], {"registry": REGISTRY}, argv[1:]
    while rest:
        if rest[0] in ("--range", "--registry") and len(rest) > 1:
            (ranges.append(rest[1]) if rest[0] == "--range" else opts.update(registry=rest[1]))
            rest = rest[2:]
        else:
            print(f"commit-rules: unknown argument {rest[0]!r}\n{usage}", file=sys.stderr)
            return 2
    if not ranges:
        print(usage, file=sys.stderr)
        return 2
    if rule_set == "allowlists" and not registered(opts["registry"]):
        print(f"commit-rules: no registry {opts['registry']}", file=sys.stderr)
        return 2
    try:
        refused = check(rule_set, ranges, opts)
    except subprocess.CalledProcessError as e:
        print(f"commit-rules: could not read the range: {(e.stderr or '').strip() or e}", file=sys.stderr)
        return 2
    for line in refused:
        print(f"{rule_set}: {line}", file=sys.stderr)
    return 1 if refused else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
