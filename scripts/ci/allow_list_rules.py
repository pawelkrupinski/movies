#!/usr/bin/env python3
# Purpose: The drift gate's `allowlists` rule: an allow-list row lands only at a check's birth, a baseline only shrinks
"""THE `allowlists` RULE of commit_rules.py (split out of it): a new check lands green and its allow-lists only shrink;
the known-flips baseline only shrinks; the struck flips take rows by the owner's named decision only. The rule's story
is commit_rules.py's header."""

from __future__ import annotations

import gzip
import json
import re
import subprocess

import tool_git
from commit_facts import SLICES, Commit, at

REGISTRY = ".github/allow-lists.txt"
DRIFT_LISTS = ("test/resources/fixtures/identity-flips-allow.tsv", "test/resources/fixtures/identity-drift-allow.tsv")
BACKLOG = "test/resources/fixtures/identity-flip-backlog.tsv"
# the flips the owner struck from the backlog as an input artefact (scripts.IdentityDrift.struck): rows by his decision only
STRUCK = "test/resources/fixtures/identity-flips-struck.tsv"
SEEDS = "test/resources/fixtures/drift-gate-seeds.tsv"
# the old known-flips baseline (merged into the backlog 2026-11-04) stays for `known`'s history reads
BASELINES = ("test/resources/fixtures/identity-flips-baseline.tsv", BACKLOG)
GATE_PATHS = ".github/identity-drift-gate-paths.txt"
WORKFLOW_ROWS = (".github/workflows/country-convergence-leg.yml", ".github/workflows/identity-model-convergence.yml",
                 ".github/workflows/identity-takes.yml")
TEMPORARY = re.compile(r"^TEMPORARY\b.*\bowner:\s*\S+.*\bremove when\b\s*\S+", re.I)


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


BACKLOG_KINDS = ("drift", "population")


def explorer_filing(c: "Commit", added: list[str]) -> bool:
    """THE EXPLORER'S FILING (.github/workflows/identity-drift-explorer.yml, filed by agentctl): a commit changing nothing
    but the backlog and the gate's seeds, adding only `drift` rows -- a flip main already shows on a seed the gate did not
    replay. The differential drift gate passes a flip its base shows, so the row hides nothing a change adds."""
    return c.changed <= {BACKLOG, SEEDS} and all(r.split("\t")[0].strip() == "drift" for r in added)


def listing(row: str) -> tuple[str, str, str]:
    """A drift or flips row's listing: country, venue, raw title (a baseline row's seed, its second column, aside)."""
    cols = row.split("\t")
    if cols[0].strip() in BACKLOG_KINDS:  # a backlog row: kind, country, seed, slice, venue, rawTitle, ...
        return _first_three([cols[1]] + cols[4:6])
    if len(cols) > 1 and cols[1].strip().isdigit():
        cols = [cols[0]] + cols[2:]
    return _first_three(cols)


def _first_three(cols: list[str]) -> tuple[str, str, str]:
    country, venue, title = (cols + ["", "", ""])[:3]
    return country.strip(), venue.strip(), title.strip()


def known(rev: str, slices_too: bool = True) -> set[tuple[str, str, str]]:
    """The listings known before `rev`: every row any earlier revision of the drift or flips list or the baseline held,
    and every listing a drift slice committed at `rev`'s parent holds -- `slices_too` False: the lists' and the
    baseline's alone (a tightened check's baseline birth: a slice holds a listing's past flip, and the tightened check
    sees a new one of it, baselined while the slice passes it, IdentityDriftSlicesSpec)."""
    out = set()
    for path in DRIFT_LISTS + BASELINES:
        try:
            history = tool_git.Git.here().text("log", "--format=%H", f"{rev}^", "--", path).split()
        except subprocess.CalledProcessError:
            history = []
        for old in history:
            out |= {listing(r) for r in rows(at(old, path))}
    if not slices_too:
        return out
    try:
        slices = [p for p in tool_git.Git.here().text("ls-tree", "-r", "--name-only", f"{rev}^", "--", SLICES).split("\n") if p.endswith("/slice.json.gz")]
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
        return bool(tool_git.Git.here().text("log", "--format=%H", "-1", f"{sha}^", "--", path).strip())
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


OWNER_APPROVED = re.compile(r"^(\d{4}-\d{2}-\d{2})\s+\S")
ROW_APPROVED = re.compile(r"^OWNER-APPROVED (\d{4}-\d{2}-\d{2}): \S")


def owner_approved(added: list[str], trailers: list[str]) -> list[str]:
    """Why each row an `Owner-Approved:` commit adds to the known-flips baseline is refused; empty when every one may
    land. The owner's named exception (2026-11-02, CinemaxX Dammtor/Raschplatz "Titans": a flip inherent to which
    listings are present, no mechanism to fix, the rule to be designed later): the commit carries
    `Owner-Approved: <YYYY-MM-DD> <the owner's decision and why>`, and every row it adds says so in its note,
    `OWNER-APPROVED <the same date>: <why>` -- so each such row names its decision where the baseline is read."""
    dates = {m.group(1) for t in trailers if (m := OWNER_APPROVED.match(t))}
    if not dates:
        return [f"`Owner-Approved: {t}`: not `<YYYY-MM-DD> <the owner's decision and why>`" for t in trailers]
    why = []
    for row in added:
        m = ROW_APPROVED.match(row.split("\t")[-1].strip())
        if not m or m.group(1) not in dates:
            why.append(f"{row!r}: an `Owner-Approved:` baseline row says so in its note, `OWNER-APPROVED <the trailer's "
                       "date>: <why>` (the known-flips baseline only shrinks but by the owner's named decision)")
    return why


def allow_lists(c: Commit, opts: dict) -> list[str]:
    refused, before_known = [], None
    births, fixes_red = c.has("New-Check"), c.has("Fixes-Red")
    for path in registered(opts["registry"]):
        if path not in c.changed:
            continue
        before = set(rows(at(c.parent, path)))
        added = [r for r in rows(at(c.sha, path)) if r not in before]
        if path == STRUCK:
            if added and c.has("Owner-Approved"):
                refused += [f"{path}: {why}" for why in owner_approved(added, c.trailers["owner-approved"])]
            elif added:
                refused += [f"{path}: {r!r}: a flip is struck only by the owner's named decision -- `Owner-Approved: "
                            "<YYYY-MM-DD> <decision and why>`, the row's decision `OWNER-APPROVED <that date>: <why>` "
                            "(no override)" for r in added]
            continue
        if path in BASELINES:
            born = not before and not at(c.parent, path) and not existed_before(c.sha, path)
            if not added or born or (path == BACKLOG and explorer_filing(c, added)):
                continue
            if c.has("Owner-Approved"):
                refused += [f"{path}: {why}" for why in owner_approved(added, c.trailers["owner-approved"])]
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
