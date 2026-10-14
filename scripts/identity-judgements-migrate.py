#!/usr/bin/env python3
"""ONE-SHOT: merge the seven places an identity judgement lived into test/resources/fixtures/identity-judgements.tsv.

  scripts/identity-judgements-migrate.py [--ref REF] [--out FILE] [--check] [--old-views DIR] [--takes FILE]

Reads, at --ref (default origin/main: never the working tree, which the migration rewrites):
  labels.tsv, known-wrong.tsv        test/resources/fixtures/identity-unmatched/
  baseline.tsv                       test/resources/fixtures/identity-suspects/  (its "wrong by labels.tsv" lines too)
  accepted.tsv                       test/resources/fixtures/identity-takes/
  MatchedTakesLabelSpec's knownWrong / noLongerServed maps
  identity_judgements_lint.py's GRANDFATHERED set
each line's `judged` date its git blame's. Writes --out (default the judgements file), then proves the migration
lossless: every view a gate reads (the labels, each scope's tolerated wrong takes, the suspects baseline, the accepted
losses) re-derived from the new file equals the old files' -- printing every difference, each with why it is no change
of verdict, and exiting 1 on any unexplained one or any conflict. --check writes nothing: only the proof, on a
temporary file.
"""

import argparse
import os
import re
import subprocess
import sys
import tempfile
from collections import defaultdict
from datetime import datetime, timezone

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "ci"))
import identity_judgements as j  # noqa: E402

LABELS = "test/resources/fixtures/identity-unmatched/labels.tsv"
KNOWN_WRONG = "test/resources/fixtures/identity-unmatched/known-wrong.tsv"
BASELINE = "test/resources/fixtures/identity-suspects/baseline.tsv"
ACCEPTED = "test/resources/fixtures/identity-takes/accepted.tsv"
MATCHED_SPEC = "worker/src/test/scala/services/identity/MatchedTakesLabelSpec.scala"
LINT = "scripts/ci/identity_judgements_lint.py"
WRONG_BY_LABELS = "wrong by labels.tsv"
RIGHT_LOST_NOTE = re.compile(r"\bright take(s)? lost\b", re.IGNORECASE)
n = j.normalise

PREAMBLE = """# THE IDENTITY JUDGEMENTS: one row per (country, venue or *, raw title, film) -- every hand judgement of a take.
# The columns, the states and the scopes: scripts/ci/identity_judgements.py (Python) and tools.IdentityJudgements (Scala).
# Edit a row in place; never add a second row for a key (the lint fails it). Rows are independent lines: .gitattributes
# merges this file with merge=union, and the lint names any key a merge doubled.
"""


GIT_ENV = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}


def source(root, ref, rel):
    """`rel` as `ref` holds it (the old files are read from a commit, never the working tree the migration rewrites),
    or None when it holds none."""
    r = subprocess.run(["git", "-C", root, "show", f"{ref}:{rel}"], capture_output=True, env=GIT_ENV, timeout=120)
    return r.stdout.decode("utf-8") if r.returncode == 0 else None


def tsv(root, ref, rel, min_cols):
    out = []
    text = source(root, ref, rel)
    if text is None:
        return out
    if True:
        for no, line in enumerate(text.splitlines(keepends=True), 1):
            if not line.strip() or line.startswith("#"):
                continue
            cols = line.rstrip("\n").split("\t")
            if cols[0] == "country" or len(cols) < min_cols:
                continue
            out.append((no, cols))
    return out


def blame_dates(root, ref, rel):
    """{line number: YYYY-MM-DD} of the commit that last changed each line at `ref`."""
    try:
        out = subprocess.run(["git", "-C", root, "blame", "--line-porcelain", ref, "--", rel], capture_output=True, text=True,
                             check=True, env=GIT_ENV, timeout=300).stdout
    except (subprocess.SubprocessError, OSError):
        return {}
    dates, line = {}, None
    for text in out.split("\n"):
        m = re.match(r"^[0-9a-f]{40} \d+ (\d+)", text)
        if m:
            line = int(m.group(1))
        elif line and text.startswith("author-time "):
            dates[line] = datetime.fromtimestamp(int(text.split()[1]), timezone.utc).strftime("%Y-%m-%d")
    return dates


SCALA_ENTRY = re.compile(r'^\s*\("(?P<cc>[a-z]{2})",\s*"(?P<title>(?:[^"\\]|\\.)*)",\s*"(?P<film>[a-z]+:[^"]+)"\)\s*->(?P<rest>.*)$')
SCALA_STRING = re.compile(r'"((?:[^"\\]|\\.)*)"')


def scala_unescape(s):
    return s.encode().decode("unicode_escape").encode("latin-1").decode("utf-8")


def matched_spec(root, ref):
    """[(line, section, cc, title, film, why)] of MatchedTakesLabelSpec's two maps."""
    text = source(root, ref, MATCHED_SPEC)
    if text is None:
        return []
    lines = text.split("\n")
    named = {}
    for i, line in enumerate(lines):
        m = re.match(r"\s*private (?:lazy )?val (\w+)\s*=\s*$", line)
        if m:
            named[m.group(1)] = "".join(SCALA_STRING.findall(lines[i + 1]))
    out, section = [], None
    for i, line in enumerate(lines):
        m = re.match(r"\s*private val (noLongerServed|knownWrong)\b", line)
        if m:
            section = m.group(1)
        m = SCALA_ENTRY.match(line)
        if not (m and section):
            continue
        why, k, rest = [], i, m.group("rest")
        while True:
            why += SCALA_STRING.findall(rest)
            ident = re.fullmatch(r"\s*(\w+)\s*,?\s*\)?\s*", rest)
            if ident and ident.group(1) in named:
                why.append(named[ident.group(1)])
            k += 1
            if k >= len(lines) or SCALA_ENTRY.match(lines[k]) or not lines[k].strip() or \
                    lines[k].strip().startswith(("//", "/**", "private", "*")):
                break
            rest = lines[k]
        out.append((i + 1, section, m.group("cc"), scala_unescape(m.group("title")), m.group("film"),
                    scala_unescape("".join(why))))
    return out


def grandfathered(root, ref):
    """The lint's GRANDFATHERED keys and whys, read without importing the (rewritten) lint."""
    text = source(root, ref, LINT)
    if text is None:
        return {}
    m = re.search(r"^GRANDFATHERED = (\{.*?^\})", text, re.S | re.M)
    return eval(m.group(1)) if m else {}  # noqa: S307 -- our own checked-in literal


def hard_key(title):
    import unicodedata
    folded = unicodedata.normalize("NFKD", title.replace("ł", "l").replace("Ł", "L"))
    return "".join(ch for ch in folded.lower() if ch.isalnum() and not unicodedata.combining(ch))


class Old:
    """The seven places, read."""

    def __init__(self, root, ref):
        self.labels_text = source(root, ref, LABELS)
        if self.labels_text is None:
            raise SystemExit(f"{ref} holds no {LABELS}: nothing to migrate")
        self.labels = tsv(root, ref, LABELS, 5)
        self.known_wrong = tsv(root, ref, KNOWN_WRONG, 3)
        self.baseline = tsv(root, ref, BASELINE, 3)
        self.accepted = tsv(root, ref, ACCEPTED, 5)
        self.matched = matched_spec(root, ref)
        self.grandfathered = grandfathered(root, ref)
        self.dates = {rel: blame_dates(root, ref, rel) for rel in (LABELS, KNOWN_WRONG, BASELINE, ACCEPTED, MATCHED_SPEC)}

    # the views, as the old readers read them
    def label_view(self):
        return {(c[0], c[1], n(c[2]), c[3]): c[4] for _, c in self.labels}

    def capture_view(self):
        return {(c[0], n(c[1]), c[2]) for _, c in self.known_wrong}

    def served_view(self):
        return {(c[0], n(c[1]), c[2]) for _, c in self.baseline}

    def rows_view(self):
        return {(cc, n(t), f) for _, _, cc, t, f, _ in self.matched}

    def hard_view(self):
        return {(k[1], k[3], k[4]) for k in self.grandfathered if k[0] == "expected" and k[2] == "hard-clusters"}

    def accepted_view(self):
        return {(c[0], c[1], n(c[2]), c[3], c[4]) for _, c in self.accepted}


def migrate(old):
    """(rows, conflicts, notes)."""
    rows, conflicts, notes = {}, [], []

    def date(rel, line):
        return old.dates.get(rel, {}).get(line, "")

    for no, c in old.labels:
        cc, venue, raw, film, verdict = c[:5]
        note = c[5] if len(c) > 5 else ""
        k = (cc, venue, n(raw), film)
        if not j.FILM.match(film):
            notes.append(f"DROPPED labels.tsv:{no} {' | '.join(c[:5])}: its film is no id (its columns shifted), so it judged no "
                         f"take")
            continue
        if k in rows:
            had = rows[k]
            if had.verdict != verdict:
                conflicts.append(f"labels.tsv:{no} judges {' | '.join(k)} {verdict}, line {had.line} {had.verdict}")
                continue
            if note and note not in had.evidence:
                had.evidence = f"{had.evidence} | {note}" if had.evidence else note
            notes.append(f"labels.tsv:{no} repeats {' | '.join(k)} (spaced apart): merged")
            continue
        rows[k] = j.Row([cc, venue, n(raw), film, verdict, "open", "", "", "", date(LABELS, no), note], no)

    # every tolerance of a wrong take, keyed by billing: (cc, title, film) -> [(scope, title, reason, date)]
    tolerance = defaultdict(list)
    for no, c in old.known_wrong:
        tolerance[(c[0], n(c[1]), c[2])].append(("capture", c[3] if len(c) > 3 else "", c[4] if len(c) > 4 else "",
                                                 date(KNOWN_WRONG, no)))
    suspects = []
    for no, c in old.baseline:
        reason = c[4] if len(c) > 4 else ""
        if reason == WRONG_BY_LABELS:
            tolerance[(c[0], n(c[1]), c[2])].append(("served", c[3] if len(c) > 3 else "", "", date(BASELINE, no)))
        else:
            suspects.append((no, c))
    for no, section, cc, title, film, why in old.matched:
        tolerance[(cc, n(title), film)].append(("rows", "", ("no longer served: " if section == "noLongerServed" else "") + why,
                                                date(MATCHED_SPEC, no)))
    for k, why in old.grandfathered.items():
        if k[0] == "expected" and k[2] == "hard-clusters":
            titles = {key[2] for key in rows if key[0] == k[1] and hard_key(key[2]) == k[3] and key[3] == k[4]}
            if len(titles) != 1:
                conflicts.append(f"GRANDFATHERED {k}: {len(titles)} labelled titles key as '{k[3]}' -- place it by hand")
                continue
            tolerance[(k[1], titles.pop(), k[4])].append(("hard-clusters", "", why, ""))

    for (cc, title, film), found in sorted(tolerance.items()):
        same = sorted((key for key in rows if key[0] == cc and key[2] == title and key[3] == film), key=lambda key: (key[1] != "*", key[1]))
        wrong = [key for key in same if rows[key].verdict == "wrong"]
        if not wrong:
            right_star = [key for key in same if key[1] == "*" and rows[key].verdict == "right"]
            if right_star:
                conflicts.append(f"{cc} | {title} | {film} is tolerated as wrong ({', '.join(s for s, *_ in found)}) but "
                                 f"labelled right at every venue")
                continue
            # wrong by contradiction only: the labels name another film of its database right. A wrong row where that
            # right label stands is what the ratchet already reads -- no verdict moves
            db = film.split(":")[0]
            where = sorted({key[1] for key in rows if key[0] == cc and key[2] == title and key[3] != film
                            and key[3].split(":")[0] == db and rows[key].verdict == "right"}, key=lambda v: (v != "*", v))
            if not where:
                conflicts.append(f"{cc} | {title} | {film} is tolerated as wrong ({', '.join(s for s, *_ in found)}) but no "
                                 f"label calls it wrong, nor another film of its database right")
                continue
            named = [v for v in where if any(v in reason for _, _, reason, _ in found)]  # the venue the reason names
            where = named[:1] + [v for v in where if v not in named[:1]]
            for venue in where[:1]:  # one wrong row carries the tolerance, which covers every venue
                key = (cc, venue, title, film)
                rows[key] = j.Row([cc, venue, title, film, "wrong", "open", "", "", "", found[0][3],
                                   f"wrong by the right film the labels name at {venue}"], 0)
                notes.append(f"ADDED {' | '.join(key)} wrong: implied by the right label there (contradiction), carries "
                             f"its {', '.join(s for s, *_ in found)} tolerance")
            wrong = [(cc, where[0], title, film)]
        target = rows[wrong[0]]
        target.state = "known-wrong"
        target.scope = ",".join(s for s in j.SCOPES if any(f[0] == s for f in found))
        target.filmTitle = next((f[1] for f in found if f[1]), target.filmTitle)
        reasons = []
        for scope, _, reason, _ in found:
            reason = reason or "listed in the suspect baseline as wrong by labels when it was armed; no reason recorded"
            if reason not in reasons:
                reasons.append(reason)
        target.reason = " / ".join(reasons)

    for no, c in suspects:
        cc, raw, film = c[0], n(c[1]), c[2]
        k = (cc, "*", raw, film)
        if k in rows:
            conflicts.append(f"baseline.tsv:{no} holds the suspect {' | '.join(k)} undecided, judged {rows[k].verdict} in labels.tsv")
            continue
        rows[k] = j.Row([cc, "*", raw, film, "undecided", "open", "served", c[3] if len(c) > 3 else "",
                         c[4] if len(c) > 4 else "", j.SUSPECTS_ARMED, ""], 0)

    for no, c in old.accepted:
        cc, venue, raw, before, after = c[:5]
        note = c[5] if len(c) > 5 else ""
        if not j.FILM.match(before) or after != "none":
            conflicts.append(f"accepted.tsv:{no} accepts {before} -> {after}: only a loss (to none) of one film migrates -- "
                             f"label {after} right, or {before} wrong, instead")
            continue
        k = (cc, venue, n(raw), before)
        right = RIGHT_LOST_NOTE.search(note) is not None
        if k in rows:
            target = rows[k]
            if target.state != "open":
                conflicts.append(f"accepted.tsv:{no}: {' | '.join(k)} is {target.state} already")
                continue
            if right and target.verdict != "right":
                conflicts.append(f"accepted.tsv:{no}: its note calls {' | '.join(k)} right, labels.tsv {target.verdict}")
                continue
        else:
            target = rows[k] = j.Row([cc, venue, n(raw), before, "right" if right else "undecided", "open", "", "", "",
                                      date(ACCEPTED, no), ""], 0)
            if right:
                notes.append(f"ADDED {' | '.join(k)} right: accepted.tsv's own note calls it a right take lost")
        target.state = "accepted-right-loss" if target.verdict == "right" else "accepted-loss"
        target.reason = note
    return list(rows.values()), conflicts, notes


def prove(old, path):
    """[(kind, message)] every difference between the old views and the new file's, `explained` or not."""
    rows = j.read(path)
    out = []
    new_labels = {(cc, venue, title, film): v for cc, ls in j.labels_by_country(rows).items() for (venue, title, film), v in ls.items()}
    old_labels = {k: v for k, v in old.label_view().items() if j.FILM.match(k[3])}  # a shifted line judged no take
    implied = {r.key for r in rows if r.evidence.startswith("wrong by the right film the labels name at ")}
    accepted_right = {r.key for r in rows if r.state == "accepted-right-loss" and r.key not in old_labels}
    for k in sorted(set(old_labels) | set(new_labels)):
        a, b = old_labels.get(k), new_labels.get(k)
        if a == b:
            continue
        if a is None and k in implied:
            out.append(("explained", f"label {' | '.join(k)}: none -> {b} (implied by the contradiction rule: the ratchet "
                                     f"already called it wrong there)"))
        elif a is None and k in accepted_right:
            out.append(("explained", f"label {' | '.join(k)}: none -> {b} (accepted.tsv's own note judged it right)"))
        else:
            out.append(("UNEXPLAINED", f"label {' | '.join(k)}: {a} -> {b}"))
    for name, before, after in (("capture", old.capture_view(), j.tolerated(rows, "capture")),
                                ("served", old.served_view(), j.tolerated(rows, "served")),
                                ("rows", old.rows_view(), j.tolerated(rows, "rows")),
                                ("hard-clusters", old.hard_view(),
                                 {(cc, hard_key(t), f) for cc, t, f in j.tolerated(rows, "hard-clusters")})):
        for k in sorted(before ^ after):
            out.append(("UNEXPLAINED", f"{name} tolerance {' | '.join(k)}: {'lost' if k in before else 'gained'}"))
    new_accepted = {(r.country, r.venue, n(r.rawTitle), r.film, "none") for r in rows if r.state in j.ACCEPTED}
    for k in sorted(old.accepted_view() ^ new_accepted):
        out.append(("UNEXPLAINED", f"accepted {' | '.join(k)}: {'lost' if k in old.accepted_view() else 'gained'}"))
    out += [("UNEXPLAINED", m) for _, m in j.problems(rows)]
    return out


def write_old_views(old, out_dir):
    """The old views for scripts.JudgementsEquivalence: the old labels file, and each scope's tolerated billings."""
    os.makedirs(out_dir, exist_ok=True)
    with open(os.path.join(out_dir, "labels.tsv"), "w", encoding="utf-8") as g:
        g.write(old.labels_text)
    for name, keys in (("capture", old.capture_view()), ("served", old.served_view()), ("rows", old.rows_view())):
        with open(os.path.join(out_dir, f"{name}.tsv"), "w", encoding="utf-8") as g:
            g.writelines("\t".join(k) + "\n" for k in sorted(keys))


def prove_takes(old, path, takes_file):
    """[(kind, message)]: over every take of `takes_file` (cc, venue, raw title, film -- the ratchet's and a leg's, as
    scripts.JudgementsEquivalence writes them), the takes check's reading -- label_of, and whether a loss of the take is
    accepted -- old against new."""
    rows = j.read(path)
    new_labels = j.labels_by_country(rows)
    old_labels = defaultdict(dict)
    for (cc, venue, title, film), v in old.label_view().items():
        old_labels[cc][(venue, title, film)] = v
    old_accepted = old.accepted_view()
    new_accepted = {(r.country, r.venue, n(r.rawTitle), r.film) for r in rows if r.state in j.ACCEPTED}
    implied = {r.key for r in rows if r.evidence.startswith("wrong by the right film the labels name at ")}
    out, seen = [], 0
    with open(takes_file, encoding="utf-8") as f:
        for line in f:
            cc, venue, title, film = line.rstrip("\n").split("\t")[:4]
            seen += 1
            a, b = j.label_of(old_labels[cc], venue, title, film), j.label_of(new_labels[cc], venue, title, film)
            if a != b:
                why = "explained" if (cc, venue, n(title), film) in implied or (cc, "*", n(title), film) in implied else "UNEXPLAINED"
                out.append((why, f"takes check label_of {cc} | {venue} | {title} | {film}: {a} -> {b}"))
            acc_a = any((cc, v, n(title), film, "none") in old_accepted for v in (venue, "*"))
            acc_b = any((cc, v, n(title), film) in new_accepted for v in (venue, "*"))
            if acc_a != acc_b:
                out.append(("UNEXPLAINED", f"takes check accepts losing {cc} | {venue} | {title} | {film}: {acc_a} -> {acc_b}"))
    print(f"takes check over {seen} takes: {len(out)} difference(s)")
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--root", default=os.path.dirname(HERE))
    ap.add_argument("--ref", default="origin/main", help="the commit whose old files are migrated (default origin/main)")
    ap.add_argument("--out")
    ap.add_argument("--check", action="store_true")
    ap.add_argument("--old-views", help="write the old views here, for scripts.JudgementsEquivalence")
    ap.add_argument("--takes", help="prove the takes check's reading over these takes (cc, venue, raw title, film)")
    a = ap.parse_args(argv)
    old = Old(a.root, a.ref)
    if a.old_views:
        write_old_views(old, a.old_views)
    rows, conflicts, notes = migrate(old)
    out = a.out or os.path.join(a.root, j.PATH)
    if a.check:
        out = os.path.join(tempfile.mkdtemp(), "identity-judgements.tsv")
    j.write(out, rows, PREAMBLE)
    print(f"read: labels.tsv {len(old.labels)}, known-wrong.tsv {len(old.known_wrong)}, baseline.tsv {len(old.baseline)}, "
          f"accepted.tsv {len(old.accepted)}, MatchedTakesLabelSpec {len(old.matched)}, GRANDFATHERED {len(old.grandfathered)}")
    print(f"wrote {out}: {len(rows)} rows (" + ", ".join(f"{s} {sum(1 for r in rows if r.state == s)}" for s in j.STATES) +
          f"; undecided {sum(1 for r in rows if r.verdict == 'undecided')})")
    for m in notes:
        print(f"  {m}")
    for m in conflicts:
        print(f"CONFLICT {m}")
    diffs = prove(old, out) + (prove_takes(old, out, a.takes) if a.takes else [])
    for kind, m in diffs:
        print(f"{kind} {m}")
    bad = [d for d in diffs if d[0] == "UNEXPLAINED"]
    print(f"proof: {len(diffs) - len(bad)} explained difference(s), {len(bad)} unexplained; {len(conflicts)} conflict(s)")
    return 1 if bad or conflicts else 0


if __name__ == "__main__":
    sys.exit(main())
