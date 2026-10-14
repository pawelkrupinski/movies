#!/usr/bin/env python3
"""THE IDENTITY JUDGEMENTS LINT: the hand judgements and the files that lean on them agree, before a push lands.

Every gate that judges a take reads the same few files. Over 2026-10-13/14 four kinds of disagreement between them each
landed and turned a gate red after the push, where nothing ran them before it:

  accepted   an accepted.tsv move off a take judged RIGHT -- by labels.tsv, or by the line's own note ("a right take
             lost"). 19890b7a5 accepted four Romeo & Juliet takes it called right, lost to a guard; the right fix is the
             guard, never the acceptance.
  conflict   one listing (country, venue, raw title, film) judged both right and wrong.
  scattered  a billing judged WRONG one venue at a time at five venues or more, with no line for every venue ("*"):
             911d8f486 labelled 81 of 82 Odeons' "We're Going on a Bear Hunt+Tiger Who Came to..." and 7 of 8 DE
             "Triple: Der Herr der Ringe" venues; the one left out (Peterborough, Schwerin) failed the takes check when
             e741882f6 dropped the wrong take everywhere.
  expected   a checked-in expectation encoding a take labels.tsv calls WRONG: an expected-matches.tsv line, or a film of
             expected-hard-clusters-<cc>.txt (c151636f1 regenerated PL's to the 2007 "Ktos calkiem obcy", which
             labels.tsv calls wrong at every venue).
  served     with --served (a convergence leg's `<cc>-takes/<cc>.json.gz` diagnostics): a SERVED take labels.tsv calls
             wrong that neither the suspect baseline nor known-wrong.tsv lists -- what the leg's suspect gate fails
             after the push (911d8f486's 48 wrong labels: DE, ES red), said before it. With --labels-before (the
             labels the push started from) only the takes the push newly calls wrong: the leg's takes are the last
             leg's, whose other wrong takes the code since may have cured.

The labels are read as identity_takes_diff.label_of reads them (a venue's own label overrules every venue's).

usage: identity_judgements_lint.py [--root DIR] [--served FILE.json.gz ...] [--labels-before FILE]
Exit 1 on any finding, 2 on unreadable input.
"""

import argparse
import gzip
import json
import os
import re
import sys
import unicodedata
from collections import defaultdict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from identity_takes_diff import label_of, single_spaced  # noqa: E402  (the one reading of a label, shared with the takes check)

LABELS = "test/resources/fixtures/identity-unmatched/labels.tsv"
ACCEPTED = "test/resources/fixtures/identity-takes/accepted.tsv"
EXPECTED_MATCHES = "test/resources/fixtures/identity-unmatched/expected-matches.tsv"
KNOWN_WRONG = "test/resources/fixtures/identity-unmatched/known-wrong.tsv"
BASELINE = "test/resources/fixtures/identity-suspects/baseline.tsv"
HARD_CLUSTERS = "test/resources/fixtures/corpus/expected-hard-clusters-{cc}.txt"
COUNTRIES = ("pl", "uk", "de", "es", "us")

# A billing judged wrong one venue at a time at this many venues is one "*" line.
SCATTERED_AT = 5

# Findings standing when the lint was armed, each with why it stands and when it goes. Only shrinks: a line no finding
# is any more fails as stale, to strike.
GRANDFATHERED = {
    ("accepted", "pl", "Kino Powiśle", "OPERA-OTELLO", "tmdb:1703634"):
        "7f939ee78's sole-result coverage trade (11 wrong cured for 2 right lost), landed before this lint: spent once "
        "identity takes (pl) passes past 7f939ee78 -- then strike the accepted.tsv line and this one",
    ("accepted", "uk", "Riverside Theatre Woodbridge", "Andre Rieu Christmas 2026", "tmdb:1707785"):
        "7f939ee78's sole-result coverage trade, as above: spent once identity takes (uk) passes past 7f939ee78",
    ("expected", "pl", "hard-clusters", "ktoscalkiemobcy", "tmdb:7183"):
        "c151636f1 regenerated the expectation to the take 25246b125 leaves at Kino Kryterium; the take is being fixed "
        "(the Ktos calkiem obcy follow-up of the 2026-10-14 destabilization report) -- regenerate, then strike",
    ("expected", "pl", "hard-clusters", "obcywdomu", "tmdb:923171"):
        "the hard-cluster replay takes Wortman's 1986 short for U-jazdowski's block of three 2025-26 shorts billed "
        "'Obcy w domu' (labels.tsv, judged 10-06); 257ad0afd regenerated it so -- fix the take, regenerate, then strike",
}

RIGHT_LOST_NOTE = re.compile(r"\bright take(s)? lost\b", re.IGNORECASE)


def rows(path, min_cols):
    with open(path, encoding="utf-8") as f:
        out = []
        for n, line in enumerate(f):
            if not line.strip() or line.startswith("#"):
                continue
            cols = line.rstrip("\n").split("\t")
            if cols[0] == "country" or len(cols) < min_cols:
                continue
            out.append(cols)
        return out


def labels_by_country(label_rows):
    """{cc: {(venue, rawTitle, film): verdict}} -- identity_takes_diff.labels_of's shape, every country at once."""
    out = defaultdict(dict)
    for c in label_rows:
        out[c[0]][(c[1], single_spaced(c[2]), c[3])] = c[4]
    return out


def key(title):
    """A title as the hard-cluster files key a film: accents folded, lowercased, letters and digits only."""
    folded = unicodedata.normalize("NFKD", title.replace("ł", "l").replace("Ł", "L"))
    return "".join(ch for ch in folded.lower() if ch.isalnum() and not unicodedata.combining(ch))


def accepted_findings(accepted_rows, labels):
    out = []
    for c in accepted_rows:
        cc, venue, title, before, after = c[:5]
        note = c[5] if len(c) > 5 else ""
        if not re.match(r"^(tmdb|imdb):", before) or after == before:
            continue
        judged = label_of(labels[cc], venue, title, before) if venue != "*" else labels[cc].get(("*", single_spaced(title), before))
        if judged == "right" or RIGHT_LOST_NOTE.search(note):
            why = "labels.tsv calls it right" if judged == "right" else "its own note calls it right"
            out.append((("accepted", cc, venue, title, before),
                        f"accepted.tsv accepts losing {before} at {venue} | {title} ({after}), a take {why}: fix the rule "
                        f"that loses it, never accept it"))
    return out


def conflict_findings(label_rows):
    verdicts = defaultdict(set)
    for c in label_rows:
        verdicts[(c[0], c[1], single_spaced(c[2]), c[3])].add(c[4])
    return [(("conflict",) + k, f"labels.tsv judges {k[3]} both right and wrong for {k[0]} {k[1]} | {k[2]}")
            for k, v in sorted(verdicts.items()) if len(v) > 1]


def scattered_findings(label_rows):
    every = {(c[0], single_spaced(c[2]), c[3]) for c in label_rows if c[1] == "*" and c[4] == "wrong"}
    venues = defaultdict(set)
    for c in label_rows:
        if c[1] != "*" and c[4] == "wrong":
            venues[(c[0], single_spaced(c[2]), c[3])].add(c[1])
    return [(("scattered",) + k, f"labels.tsv judges {k[2]} wrong for {k[0]} '{k[1]}' at {len(v)} venues one by one: "
             f"judge it once for every venue ('*'), or the venue left out fails the takes check")
            for k, v in sorted(venues.items()) if len(v) >= SCATTERED_AT and k not in every]


def expected_match_findings(expected_rows, labels):
    return [(("expected", c[0], c[1], c[2], c[3]), f"expected-matches.tsv expects {c[3]} for {c[0]} {c[1]} | {c[2]}, "
             f"which labels.tsv calls wrong")
            for c in expected_rows if label_of(labels[c[0]], c[1], c[2], c[3]) == "wrong"]


HARD_LINE = re.compile(r"^(?P<key>[^|\t]*)\|[^\t]*\t.*\btmdb=(?P<tmdb>\d+)\b.*\bcinemas=\[(?P<cinemas>[^\]]*)\]")


def hard_cluster_findings(root, label_rows):
    wrong = defaultdict(list)  # (cc, title key, film) -> venues judged wrong ("*" for every)
    for c in label_rows:
        if c[4] == "wrong":
            wrong[(c[0], key(c[2]), c[3])].append(c[1])
    out = []
    for cc in COUNTRIES:
        path = os.path.join(root, HARD_CLUSTERS.format(cc=cc))
        if not os.path.exists(path):
            continue
        with open(path, encoding="utf-8") as f:
            for line in f:
                m = HARD_LINE.match(line)
                if not m:
                    continue
                film, cinemas = f"tmdb:{m['tmdb']}", {v.strip() for v in m["cinemas"].split(",")}
                venues = wrong.get((cc, m["key"], film), [])
                if "*" in venues or cinemas & set(venues):
                    out.append((("expected", cc, "hard-clusters", m["key"], film),
                                f"expected-hard-clusters-{cc}.txt expects '{m['key']}' as {film}, which labels.tsv calls wrong"))
    return out


def served_findings(served_paths, labels, listed, before=None):
    out = {}
    for path in served_paths:
        with gzip.open(path, "rt", encoding="utf-8") as f:
            capture = json.load(f)
        cc = capture["country"]
        for decision in capture["decisions"]:
            if not decision.get("film"):
                continue
            film = f"tmdb:{decision['film']}"
            for m in decision["members"]:
                title = single_spaced(m["rawTitle"])
                if (cc, title, film) in listed or label_of(labels[cc], m["venue"], title, film) != "wrong":
                    continue
                if before is not None and label_of(before[cc], m["venue"], title, film) == "wrong":
                    continue
                k = ("served", cc, title, film)
                out.setdefault(k, (k, f"{cc} serves {film} for '{title}' (at {m['venue']}), which labels.tsv calls wrong and "
                                      f"neither the suspect baseline nor known-wrong.tsv lists: fix it, or list it with why"))
    return list(out.values())


def lint(root, served_paths=(), labels_before=None):
    """(findings, stale grandfathered keys): each finding (key, message), the grandfathered ones left out."""
    p = lambda rel: os.path.join(root, rel)  # noqa: E731
    label_rows = rows(p(LABELS), 5)
    labels = labels_by_country(label_rows)
    listed = {(c[0], single_spaced(c[1]), c[2]) for c in rows(p(BASELINE), 3) + rows(p(KNOWN_WRONG), 3)}
    found = (accepted_findings(rows(p(ACCEPTED), 5), labels) + conflict_findings(label_rows) + scattered_findings(label_rows)
             + expected_match_findings(rows(p(EXPECTED_MATCHES), 4), labels) + hard_cluster_findings(root, label_rows)
             + served_findings(served_paths, labels, listed,
                               labels_by_country(rows(labels_before, 5)) if labels_before else None))
    keys = {k for k, _ in found}
    return [f for f in found if f[0] not in GRANDFATHERED], sorted(k for k in GRANDFATHERED if k not in keys)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--root", default=".")
    ap.add_argument("--served", nargs="*", default=[])
    ap.add_argument("--labels-before")
    a = ap.parse_args(argv)
    try:
        findings, stale = lint(a.root, a.served, a.labels_before)
    except (OSError, ValueError, KeyError) as e:
        print(f"identity judgements: unreadable input: {e}", file=sys.stderr)
        return 2
    for _, message in findings:
        print(f"FAIL {message}")
    for k in stale:
        print(f"FAIL stale GRANDFATHERED entry in scripts/ci/identity_judgements_lint.py, no finding any more -- strike it: {k}")
    if findings or stale:
        print(f"identity judgements: {len(findings)} finding(s), {len(stale)} stale grandfathered line(s)")
        return 1
    print("identity judgements: consistent")
    return 0


if __name__ == "__main__":
    sys.exit(main())
