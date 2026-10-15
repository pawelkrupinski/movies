#!/usr/bin/env python3
"""THE IDENTITY JUDGEMENTS LINT: the one judgements file (identity_judgements.PATH) is well formed, and agrees with the
files that lean on it, before a push lands.

Every gate that judges a take reads identity-judgements.tsv. Over 2026-10-13/14 the disagreements below each landed and
turned a gate red after the push, where nothing ran them before it:

  store      a blank line (65884e94e's broke LabelsExportSpec's byte-for-byte round trip), or a row breaking the file's
             rules (identity_judgements.problems): a second row for one listing and film (a
             listing judged both right and wrong, or a union merge that doubled one), a key not normalised (double
             spaces), a word outside the vocabulary, a known-wrong row naming no scope or reason, an undecided suspect
             added after the suspect gate was armed, an accepted-loss of a take judged RIGHT -- 19890b7a5 accepted four
             Romeo & Juliet takes it called right, lost to a guard; the right fix is the guard, never the acceptance (a
             trade the policy allows says so: state accepted-right-loss, named on every run).
  accepted   an accepted loss whose listing the judgements call RIGHT at its venue (a "*" row) or whose own reason says
             so ("a right take lost"), but whose state is not accepted-right-loss.
  scattered  a billing judged WRONG one venue at a time at five venues or more, with no row for every venue ("*"):
             911d8f486 labelled 81 of 82 Odeons' "We're Going on a Bear Hunt+Tiger Who Came to..." and 7 of 8 DE
             "Triple: Der Herr der Ringe" venues; the one left out (Peterborough, Schwerin) failed the takes check when
             e741882f6 dropped the wrong take everywhere.
  tolerated  a known-wrong row whose film the judgements call right at every venue ("*").
  expected   a checked-in expectation encoding a take the judgements call WRONG: an expected-matches.tsv line, or a film
             of expected-hard-clusters-<cc>.txt (c151636f1 regenerated PL's to the 2007 "Ktos calkiem obcy", wrong at every
             venue) -- unless a known-wrong row of scope hard-clusters stands for it; such a row no expectation needs any
             more fails as stale.
  served     with --served (a convergence leg's `<cc>-takes/<cc>.json.gz` diagnostics): a SERVED take the judgements call
             wrong that no known-wrong row of scope served or capture tolerates -- what the leg's suspect gate fails after
             the push (911d8f486's 48 wrong labels: DE, ES red), said before it. With --before (the judgements the push
             started from) only the takes the push newly calls wrong: the leg's takes are the last leg's, whose other
             wrong takes the code since may have cured. With --replay-head too, those newly-wrong takes are replayed at
             the PUSHED code (scripts/identity-slice --batch, a handful of listings) and only the ones it still serves
             fail: a push that judges a take wrong and cures it lands (Der Diplomat, Blutgericht in Texas, 2026-10-14).
             Where the slice cannot run, the finding stands (refused) on the last leg's takes, said as "could not replay".

usage: identity_judgements_lint.py [--root DIR] [--served FILE.json.gz ...] [--before FILE] [--replay-head]
Exit 1 on any finding, 2 on unreadable input.
"""

import argparse
import csv
import gzip
import json
import os
import re
import subprocess
import sys
import tempfile
import unicodedata
from collections import defaultdict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_judgements as j  # noqa: E402  (the one reader of the judgements, shared with the takes check)
from identity_judgements import label_of, normalise  # noqa: E402

JUDGEMENTS = j.PATH
EXPECTED_MATCHES = "test/resources/fixtures/identity-unmatched/expected-matches.tsv"
HARD_CLUSTERS = "test/resources/fixtures/corpus/expected-hard-clusters-{cc}.txt"
COUNTRIES = j.COUNTRIES

# A billing judged wrong one venue at a time at this many venues is one "*" row.
SCATTERED_AT = 5

RIGHT_LOST_NOTE = re.compile(r"\bright take(s)? lost\b", re.IGNORECASE)


def rows(path, min_cols):
    with open(path, encoding="utf-8") as f:
        out = []
        for line in f:
            if not line.strip() or line.startswith("#"):
                continue
            cols = line.rstrip("\n").split("\t")
            if cols[0] == "country" or len(cols) < min_cols:
                continue
            out.append(cols)
        return out


def key(title):
    """A title as the hard-cluster files key a film: accents folded, lowercased, letters and digits only."""
    folded = unicodedata.normalize("NFKD", title.replace("ł", "l").replace("Ł", "L"))
    return "".join(ch for ch in folded.lower() if ch.isalnum() and not unicodedata.combining(ch))


def store_findings(root, judged):
    blank = [(("store", "blank", str(n)), f"{JUDGEMENTS}:{n}: a blank line -- drop it (the file is rewritten byte for byte)")
             for n in j.blank_lines(os.path.join(root, JUDGEMENTS))]
    return blank + [(("store", r.country, r.venue, normalise(r.rawTitle), r.film), m) for r, m in j.problems(judged)]


def accepted_findings(judged, labels):
    out = []
    for r in judged:
        if r.state != "accepted-loss" or r.verdict == "right":  # a right verdict is the store's own finding
            continue
        title = normalise(r.rawTitle)
        verdict = label_of(labels[r.country], r.venue, title, r.film)
        if verdict == "right" or RIGHT_LOST_NOTE.search(r.reason):
            why = "the judgements call it right" if verdict == "right" else "its own reason calls it right"
            out.append((("accepted", r.country, r.venue, title, r.film),
                        f"an accepted loss of {r.film} at {r.venue} | {title}, a take {why}: fix the rule that loses it, never "
                        f"accept it (or, a trade the policy allows, judge it right with state accepted-right-loss)"))
    return out


def scattered_findings(judged):
    every = {(r.country, normalise(r.rawTitle), r.film) for r in judged if r.venue == "*" and r.verdict == "wrong"}
    venues = defaultdict(set)
    for r in judged:
        if r.venue != "*" and r.verdict == "wrong":
            venues[(r.country, normalise(r.rawTitle), r.film)].add(r.venue)
    return [(("scattered",) + k, f"the judgements call {k[2]} wrong for {k[0]} '{k[1]}' at {len(v)} venues one by one: "
             f"judge it once for every venue ('*'), or the venue left out fails the takes check")
            for k, v in sorted(venues.items()) if len(v) >= SCATTERED_AT and k not in every]


def tolerated_right_findings(judged, labels):
    return [(("tolerated", r.country, r.venue, normalise(r.rawTitle), r.film),
             f"{r.country} {r.venue} | {r.rawTitle}: {r.film} is known-wrong here, but the judgements call it right at every venue")
            for r in judged if r.state == "known-wrong" and labels[r.country].get(("*", normalise(r.rawTitle), r.film)) == "right"]


def expected_match_findings(expected_rows, labels):
    return [(("expected", c[0], c[1], c[2], c[3]), f"expected-matches.tsv expects {c[3]} for {c[0]} {c[1]} | {c[2]}, "
             f"which the judgements call wrong")
            for c in expected_rows if label_of(labels[c[0]], c[1], c[2], c[3]) == "wrong"]


HARD_LINE = re.compile(r"^(?P<key>[^|\t]*)\|[^\t]*\t.*\btmdb=(?P<tmdb>\d+)\b.*\bcinemas=\[(?P<cinemas>[^\]]*)\]")


def hard_cluster_findings(root, judged):
    """The hard-cluster expectations the judgements call wrong that no known-wrong row of scope hard-clusters stands
    for, and each such row no expectation needs any more (stale)."""
    wrong = defaultdict(list)  # (cc, title key, film) -> venues judged wrong ("*" for every)
    for r in judged:
        if r.verdict == "wrong":
            wrong[(r.country, key(r.rawTitle), r.film)].append(r.venue)
    standing = {(r.country, key(r.rawTitle), r.film): r for r in judged if r.state == "known-wrong" and "hard-clusters" in r.scopes}
    out, needed = [], set()
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
                k = (cc, m["key"], film)
                venues = wrong.get(k, [])
                if "*" in venues or cinemas & set(venues):
                    if k in standing:
                        needed.add(k)
                    else:
                        out.append((("expected", cc, "hard-clusters", m["key"], film),
                                    f"expected-hard-clusters-{cc}.txt expects '{m['key']}' as {film}, which the judgements call wrong"))
    for k, r in sorted(standing.items()):
        if k not in needed:
            out.append((("stale", r.country, r.venue, r.rawTitle, r.film),
                        f"{r.country} {r.venue} | {r.rawTitle}: {r.film} is known-wrong in scope hard-clusters, but no hard-cluster "
                        f"expectation takes it any more -- drop the scope (and the state, if it was the last)"))
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
                title = normalise(m["rawTitle"])
                if (cc, title, film) in listed or label_of(labels[cc], m["venue"], title, film) != "wrong":
                    continue
                if before is not None and label_of(before[cc], m["venue"], title, film) == "wrong":
                    continue
                k = ("served", cc, title, film)
                out.setdefault(k, (k, f"{cc} serves {film} for '{title}' (at {m['venue']}), which the judgements call wrong and "
                                      f"no known-wrong row tolerates: fix it, or make the row known-wrong (scope served) with why", []))
                out[k][2].append(m["venue"])
    return list(out.values())


def at_head(found, replay):
    """The served findings `found` (key, message, venues) judged by the PUSHED code: each listing replayed by `replay`
    (cc, [(venue, title)]) -> {(venue, title): film}, a finding kept only where the head still serves its film; a
    country the replay cannot answer judged by the last leg's takes, said so."""
    out = []
    by_cc = defaultdict(list)
    for f in found:
        by_cc[f[0][1]].append(f)
    for cc, fs in sorted(by_cc.items()):
        listings = sorted({(v, f[0][2]) for f in fs for v in f[2][:3]})
        try:
            takes = replay(cc, listings)
        except Exception as e:  # noqa: BLE001 -- any failure to replay falls back, said
            out += [(k, f"{m} [COULD NOT REPLAY the pushed code ({e}); this stands on the last convergence leg's takes, which the code "
                       f"may since have cured: fix the replay (scripts/identity-slice {cc} ... --recording RUN) and push again]")
                    for k, m, _ in fs]
            continue
        for k, m, venues in fs:
            if any(takes.get((v, k[2])) == k[3] for v in venues[:3]):
                out.append((k, f"{m} [the pushed code still serves it]"))
    return out


def slice_replay(command):
    """A replay by scripts/identity-slice at the checked-out code: one --batch per country, its takes read off --out."""
    def replay(cc, listings):
        with tempfile.TemporaryDirectory() as d:
            batch, takes = os.path.join(d, "batch.tsv"), os.path.join(d, "takes.tsv")
            with open(batch, "w", encoding="utf-8") as f:
                f.writelines(f"{venue}\t{title}\n" for venue, title in listings)
            r = subprocess.run([command, cc, "--batch", batch, "--out", takes], capture_output=True, text=True, timeout=1800)
            if r.returncode not in (0, 3) or not os.path.exists(takes):
                raise RuntimeError(f"identity-slice exited {r.returncode}: {(r.stderr or '').strip().splitlines()[-1:] or ''}")
            with open(takes, encoding="utf-8") as f:
                return {(row["venue"], normalise(row["rawTitle"])): row["film"]
                        for row in csv.DictReader(f, delimiter="\t", quoting=csv.QUOTE_NONE)}
    return replay


def served(found, replay):
    return at_head(found, replay) if replay else [(k, m) for k, m, _ in found]


def notices(judged):
    """The policy's explicit exceptions, named on every run: each accepted-right-loss."""
    return [f"accepted-right-loss: {r.country} {r.venue} | {r.rawTitle}: losing {r.film} accepted -- {r.reason}"
            for r in judged if r.state == "accepted-right-loss"]


def lint(root, served_paths=(), before=None, replay=None):
    """Every finding (key, message). `before`: the judgements file the push started from; `replay` (with it): the newly
    wrong served takes judged at the pushed code ([[at_head]])."""
    judged = j.read_at(root)
    labels = j.labels_by_country(judged)
    listed = j.tolerated(judged, "served") | j.tolerated(judged, "capture")
    return (store_findings(root, judged) + accepted_findings(judged, labels) + scattered_findings(judged)
            + tolerated_right_findings(judged, labels)
            + expected_match_findings(rows(os.path.join(root, EXPECTED_MATCHES), 4), labels) + hard_cluster_findings(root, judged)
            + served(served_findings(served_paths, labels, listed, j.labels_by_country(j.read(before)) if before else None),
                     replay if before else None))


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--root", default=".")
    ap.add_argument("--served", nargs="*", default=[])
    ap.add_argument("--before", help="the judgements file the push started from")
    ap.add_argument("--replay-head", action="store_true",
                    help="replay the newly-wrong served takes at the checked-out code (scripts/identity-slice, or $IDENTITY_SLICE)")
    a = ap.parse_args(argv)
    replay = slice_replay(os.environ.get("IDENTITY_SLICE", os.path.join(a.root, "scripts/identity-slice"))) if a.replay_head else None
    try:
        findings = lint(a.root, a.served, a.before, replay)
        named = notices(j.read_at(a.root))
    except (OSError, ValueError, KeyError) as e:
        print(f"identity judgements: unreadable input: {e}", file=sys.stderr)
        return 2
    for message in named:
        print(f"NOTE {message}")
    for _, message in findings:
        print(f"FAIL {message}")
    if findings:
        print(f"identity judgements: {len(findings)} finding(s)")
        return 1
    print("identity judgements: consistent")
    return 0


if __name__ == "__main__":
    sys.exit(main())
