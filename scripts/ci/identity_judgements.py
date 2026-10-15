#!/usr/bin/env python3
"""THE IDENTITY JUDGEMENTS: the one file every gate reads a hand judgement of a take from, and its one reader in Python.

test/resources/fixtures/identity-judgements.tsv holds ONE row per (country, venue or "*", raw title, film) -- what a
person judged that film to be for that billing, and what the gates are to make of it. The Scala reader is
tools.IdentityJudgements; both read the same columns and key a billing the same way (`normalise`, checked against the
shared cases in test/resources/fixtures/identity-judgements-normalise.jsonl by both languages' tests).

Columns, tab-separated, a header line first (lines starting "#" are comments):
  country    pl, uk, de, es, us
  venue      the venue's display name, or "*" for every venue billing the title; a venue's row overrides "*"'s
  rawTitle   the billing, normalised (runs of whitespace one space, none at the ends)
  film       tmdb:<id>, imdb:tt<id>, filmweb:<id>, ...
  verdict    right | wrong | undecided (a suspect standing unjudged since the suspect gate was armed)
  state      open                  a plain judgement: a take of a wrong film fails every gate
             known-wrong           a WRONG take that is served and no safe fix removes yet; `scope` names the gates that
                                   let it stand, each failing once it is cured (shrink-only), `reason` says why
             accepted-loss         the identity takes check accepts the listing's take moving OFF this film to none (a
                                   move to another film is judged by both films' verdicts): a trade within the rate,
                                   `reason` says which
             accepted-right-loss   the same for a film judged RIGHT -- the lint's policy forbids accepting a right take's
                                   loss, so this state is that policy's explicit exception, named on every lint run
  scope      for known-wrong (and an undecided suspect), comma-separated: capture (the unmatched-cluster ratchet),
             served (a convergence leg's suspect gate), rows (MatchedTakesLabelSpec's matched rows), hard-clusters (an
             expected-hard-clusters-<cc>.txt expectation)
  filmTitle  the film's title and year, for a reader
  reason     why the row's state stands (required but for `open`)
  judged     the date (YYYY-MM-DD) the verdict was given
  evidence   what the verdict rests on

The lint (identity_judgements_lint.py) checks the file's shape with `problems`.
"""

import os
import re
from collections import defaultdict

PATH = "test/resources/fixtures/identity-judgements.tsv"
COLUMNS = ("country", "venue", "rawTitle", "film", "verdict", "state", "scope", "filmTitle", "reason", "judged", "evidence")
HEADER = "\t".join(COLUMNS)
COUNTRIES = ("pl", "uk", "de", "es", "us")
VERDICTS = ("right", "wrong", "undecided")
STATES = ("open", "known-wrong", "accepted-loss", "accepted-right-loss")
SCOPES = ("capture", "served", "rows", "hard-clusters")
ACCEPTED = ("accepted-loss", "accepted-right-loss")
# The suspect gate was armed on this day with the suspects then standing: an undecided row is one of those, never a new
# suspect (judge that right or wrong instead), so the undecided rows only shrink.
SUSPECTS_ARMED = "2026-10-13"

# Java's (?U)\s -- Unicode White_Space -- which UnmatchedClusters.singleSpaced folds; Python's str.split() would also
# fold \x1c-\x1f, so the set is spelled out.
_WHITESPACE = "\t\n\x0b\x0c\r \x85\xa0  -     　"
_RUNS = re.compile(f"[{_WHITESPACE}]+")


def normalise(text):
    """A billing (or venue) as every judgement keys it: runs of whitespace -- a tab, a line break, a no-break space, two
    spaces -- one space, none at the ends. A venue's re-spacing is not a new billing."""
    return _RUNS.sub(" ", text).strip(" ")


class Row:
    __slots__ = COLUMNS + ("line",)

    def __init__(self, cols, line=0):
        for name, value in zip(COLUMNS, cols):
            setattr(self, name, value)
        self.line = line

    @property
    def key(self):
        return (self.country, self.venue, normalise(self.rawTitle), self.film)

    @property
    def scopes(self):
        return {s for s in self.scope.split(",") if s}

    def cols(self):
        return [getattr(self, c) for c in COLUMNS]


def read(path):
    """Every row, in file order. Raises ValueError on a malformed line."""
    out = []
    with open(path, encoding="utf-8") as f:
        for n, line in enumerate(f, 1):
            text = line.rstrip("\n")
            if not text.strip() or text.startswith("#"):
                continue
            cols = text.split("\t")
            if cols[0] == "country":
                if tuple(cols) != COLUMNS:
                    raise ValueError(f"{path}:{n}: header is not {HEADER!r}")
                continue
            if len(cols) != len(COLUMNS):
                raise ValueError(f"{path}:{n}: {len(cols)} fields, want {len(COLUMNS)}: {text}")
            out.append(Row(cols, n))
    return out


def read_at(root, rel=PATH):
    return read(os.path.join(root, rel))


def write(path, rows, preamble=""):
    """The rows in their canonical order (country, raw title, film, venue), after `preamble`'s comment lines."""
    rows = sorted(rows, key=lambda r: (r.country, normalise(r.rawTitle), r.film, r.venue != "*", r.venue))
    with open(path, "w", encoding="utf-8") as f:
        f.write(preamble)
        f.write(HEADER + "\n")
        for r in rows:
            f.write("\t".join(r.cols()) + "\n")


# ── the views every gate reads ───────────────────────────────────────────────────────────────

class Labels(dict):
    """One country's {(venue, rawTitle, film): verdict}, indexed by listing for right_films."""

    def right_by_listing(self):
        if "_right" not in self.__dict__:
            self._right = defaultdict(set)
            for (venue, title, film), verdict in self.items():
                if verdict == "right":
                    self._right[(venue, title)].add(film)
        return self._right


def labels_by_country(rows):
    """{cc: Labels} of the rows judging right or wrong -- what label_of reads."""
    out = defaultdict(Labels)
    for r in rows:
        if r.verdict in ("right", "wrong"):
            out[r.country][(r.venue, normalise(r.rawTitle), r.film)] = r.verdict
    return out


def country_views(rows, cc):
    """(labels, accepted moves, tolerated suspects) for `cc`: what label_of reads; each accepted loss as the move
    (venue, rawTitle, film, "none") it accepts; and the {(rawTitle, film)} the judgements tolerate in scope `served`."""
    return (labels_by_country(rows)[cc], [(venue, title, film, "none") for venue, title, film in accepted(rows, cc)],
            {(title, film) for country, title, film in tolerated(rows, "served") if country == cc})


def judgements_of(path, cc):
    """country_views of the judgements at `path`; none when no path is given."""
    return country_views(read(path), cc) if path else ({}, [], set())


def own_or_every(table, venue, title, film):
    """`table`'s entry for the listing: the venue's own row's, else the row for every venue ("*")'s."""
    title = normalise(title)
    own = table.get((venue, title, film))
    return own if own is not None else table.get(("*", title, film))


def stated(labels, venue, title, film):
    """The verdict a row states for `film` at the listing: the venue's own row overrides "*"'s."""
    return own_or_every(labels, venue, title, film)


def right_films(labels, venue, title):
    """The films the judgements call right for the listing, at its venue or at every venue ("*")."""
    title = normalise(title)
    if isinstance(labels, Labels):
        index = labels.right_by_listing()
        return index.get((venue, title), set()) | index.get(("*", title), set())
    return {f for (v, t, f), verdict in labels.items() if verdict == "right" and t == title and v in ("*", venue)}


def label_of(labels, venue, title, film):
    """The verdict the judgements give `film` for the listing (one country's labels_by_country), as
    UnmatchedClusters.verdict reads it: the one a row states (`stated`: the venue's own row overrides "*"'s), else WRONG
    when they call another film of its database (tmdb:, imdb:, ...) right for the listing -- the listing is that film,
    so it is not this one -- else None, unjudged."""
    said = stated(labels, venue, title, film)
    if said is not None:
        return said
    database = film.split(":", 1)[0] + ":"
    return "wrong" if any(f != film and f.startswith(database) for f in right_films(labels, venue, title)) else None


def tolerated(rows, scope):
    """{(cc, rawTitle, film)}: the billings whose wrong takes the gate `scope` lets stand -- a known-wrong row at any of
    its venues covers every venue billing it (and, for `served`, the undecided suspects standing since it was armed)."""
    return {(r.country, normalise(r.rawTitle), r.film) for r in rows
            if scope in r.scopes and (r.state == "known-wrong" or (scope == "served" and r.verdict == "undecided"))}


def accepted(rows, cc):
    """[(venue, rawTitle, film)] whose loss the identity takes check accepts for `cc`."""
    return [(r.venue, normalise(r.rawTitle), r.film) for r in rows if r.country == cc and r.state in ACCEPTED]


# ── the file's own rules ─────────────────────────────────────────────────────────────────────

FILM = re.compile(r"^(tmdb:\d+|imdb:tt\d+|(?!tmdb:|imdb:)[a-z]+:\S+)$")
DATE = re.compile(r"^\d{4}-\d{2}-\d{2}$")


def problems(rows):
    """[(row or None, message)]: every row breaking the file's rules -- one row a key, normalised keys, the vocabulary,
    and what each state asks of the rest of the row."""
    out = []
    seen = {}
    for r in rows:
        def bad(message):
            out.append((r, f"{PATH}:{r.line}: {message}"))
        if r.country not in COUNTRIES:
            bad(f"country {r.country!r} is none of {', '.join(COUNTRIES)}")
        if not r.venue or r.venue != normalise(r.venue):
            bad(f"venue {r.venue!r} is not normalised (runs of whitespace one space, none at the ends)")
        if not r.rawTitle or r.rawTitle != normalise(r.rawTitle):
            bad(f"rawTitle {r.rawTitle!r} is not normalised (runs of whitespace one space, none at the ends)")
        if not FILM.match(r.film):
            bad(f"film {r.film!r} is no id (tmdb:N, imdb:ttN, ...)")
        if r.verdict not in VERDICTS:
            bad(f"verdict {r.verdict!r} is none of {', '.join(VERDICTS)}")
        if r.state not in STATES:
            bad(f"state {r.state!r} is none of {', '.join(STATES)}")
        unknown = r.scopes - set(SCOPES)
        if unknown:
            bad(f"scope {', '.join(sorted(unknown))} is none of {', '.join(SCOPES)}")
        if r.judged and not DATE.match(r.judged):
            bad(f"judged {r.judged!r} is no date (YYYY-MM-DD)")
        if r.state == "known-wrong":
            if r.verdict != "wrong":
                bad("known-wrong is a state of a WRONG take: its verdict must be wrong")
            if not r.scopes:
                bad("known-wrong names no scope: which gate lets it stand (capture, served, rows, hard-clusters)?")
        elif r.verdict == "undecided" and r.state == "open":
            if r.scopes != {"served"}:
                bad("an undecided row is a suspect the served gate lets stand: its scope is served")
            if not r.judged or r.judged > SUSPECTS_ARMED:
                bad(f"an undecided suspect stands only from when the suspect gate was armed ({SUSPECTS_ARMED}): judge a "
                    "new suspect right or wrong, never add it undecided")
        elif r.scopes:
            bad(f"scope is for a known-wrong row (or an undecided suspect), not {r.state} {r.verdict}")
        if r.state == "accepted-loss" and r.verdict == "right":
            bad("accepted-loss of a take judged RIGHT: fix the rule that loses it -- or, a trade the policy allows, say so "
                "with state accepted-right-loss")
        if r.state == "accepted-right-loss" and r.verdict != "right":
            bad("accepted-right-loss is for a take judged RIGHT")
        if r.state != "open" and not r.reason:
            bad(f"{r.state} gives no reason")
        k = r.key
        if k in seen:
            bad(f"a second row for {' | '.join(k)} (the first at line {seen[k]}): one row a listing and film -- merge them")
        else:
            seen[k] = r.line
    return out
