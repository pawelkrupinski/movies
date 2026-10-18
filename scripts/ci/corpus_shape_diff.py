#!/usr/bin/env python3
"""THE CORPUS-SHAPE CHECK, scoped to what the code changed: the corpus-shape specs' findings over ONE recording, at the
base commit and at the head.

SearchQueryMarkersSpec and CinemaSlotInvariantsSpec (CorpusShapeSpec) write, per corpus they read, the findings no
allowlist entry explains and the allowlist entries the corpus no longer breaks, as
`corpus-shape-recorded-<cc>-<spec>.txt` (one `finding|stale <TAB> venue <TAB> listing <TAB> what` line each). The
identity takes check runs them at the base and at the head over the recording its row replays, and this diffs the two:

  new       at the head only -- a title rule, the marker vocabulary, a slot check or an allowlist the change made:
            FAILS;
  standing  at base and head alike -- the recording's own, which no change of this commit's moved: REPORTED, never
            failed (the newest recording's are the Identity findings report's, identity-findings-report.yml);
  cured     at the base only -- counted, never failed.

A finding is matched by spec, kind, venue and listing; its text (the query a listing searches) may differ between base
and head without making it new. A spec may find one listing more than once: those are matched as many to many, the same
texts first, so a second finding the head made for a listing already found is new.

usage: corpus_shape_diff.py <cc> <base dir> <head dir> [--summary F] [--base-name S] [--head-name S]
Exit 1 on a new finding, 2 when a side wrote no findings file for <cc>, or the head none for a spec the base ran (the
specs did not run): no verdict. A spec only the head has (one the change brings) judges its findings as new.
"""

import argparse
import glob
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from report_fields import cell  # noqa: E402


def specs_of(directory, cc):
    """{spec: path} of every corpus-shape file `directory` holds for `cc`'s recorded corpus -- each spec writes its own,
    findings or none, so a spec absent from it did not run there."""
    prefix = f"corpus-shape-recorded-{cc}-"
    return {os.path.basename(p)[len(prefix):-len(".txt")]: p for p in glob.glob(os.path.join(directory, prefix + "*.txt"))}


def findings_of(directory, cc):
    """{(spec, kind, venue, listing): [what, ...]} of every corpus-shape file `directory` holds for `cc`'s recorded corpus;
    None when it holds none."""
    specs = specs_of(directory, cc)
    if not specs:
        return None
    out = {}
    for spec, path in sorted(specs.items()):
        with open(path, encoding="utf-8") as f:
            for line in f:
                cols = line.rstrip("\n").split("\t")
                if len(cols) >= 3:
                    out.setdefault((spec, cols[0], cols[1], cols[2]), []).append(cols[3] if len(cols) > 3 else "")
    return out


def _matched(base, head):
    """(standing, new, cured) whats of one key's findings at base and head: the same texts paired first, then the rest in
    order (a finding whose text moved still stands); what is left over at the head is new, at the base cured."""
    left = list(base)
    same, rest = [], []
    for what in head:
        (same if what in left else rest).append(what)
        if what in left:
            left.remove(what)
    paired = min(len(rest), len(left))
    return same + rest[:paired], rest[paired:], left[paired:]


def classify(base, head):
    """{"new", "standing", "cured"}: (key, what) pairs, sorted."""
    out = {"new": [], "standing": [], "cured": []}
    for k in base.keys() | head.keys():
        standing, new, cured = _matched(base.get(k, []), head.get(k, []))
        out["standing"] += [(k, w) for w in standing]
        out["new"] += [(k, w) for w in new]
        out["cured"] += [(k, w) for w in cured]
    return {kind: sorted(pairs) for kind, pairs in out.items()}


def render(cc, found, base_name, head_name):
    new, standing, cured = found["new"], found["standing"], found["cured"]
    line = (f"corpus shape ({cc}): base {base_name} vs head {head_name} on one recording -- {len(new)} new (the code's: "
            f"fail), {len(standing)} standing (reported, never failed), {len(cured)} cured")
    text = [line] + [f"  FAIL {k[0]} {k[1]}: {k[2]} | {k[3]}: {what}" for k, what in new] + \
           [f"  cured {k[0]} {k[1]}: {k[2]} | {k[3]}" for k, _ in cured]
    md = [f"### Corpus shape ({cc})", "", line, ""]
    if new:
        md += ["| | spec | kind | venue | listing | finding |", "|---|---|---|---|---|---|"]
        md += [f"| ❌ | {k[0]} | {k[1]} | {cell(k[2])} | {cell(k[3])} | {cell(what)} |" for k, what in new[:300]]
        md += ["", "A new finding is the change's: fix the title rule / parser / check, or allowlist the listing with why "
                   "(the title-trace and venue-replay tools, identity-investigation skill)."]
    if standing:
        md += ["", f"<details><summary>{len(standing)} standing finding(s): the recording's, untouched by this change -- "
                   "triaged from the Identity findings report, never failed here</summary>", ""]
        md += [f"- {k[0]} {k[1]}: {cell(k[2])} \\| {cell(k[3])} -- {cell(what)}" for k, what in standing[:200]]
        md += ["", "</details>"]
    return "\n".join(text), "\n".join(md) + "\n"


def main(argv=None):
    p = argparse.ArgumentParser()
    p.add_argument("cc")
    p.add_argument("base")
    p.add_argument("head")
    p.add_argument("--summary")
    p.add_argument("--base-name", default="base")
    p.add_argument("--head-name", default="head")
    a = p.parse_args(argv)
    try:
        base, head = findings_of(a.base, a.cc), findings_of(a.head, a.cc)
    except (OSError, ValueError) as e:
        print(f"corpus shape ({a.cc}): could not read the findings: {type(e).__name__}: {e}", file=sys.stderr)
        return 2
    if base is None or head is None:
        print(f"corpus shape ({a.cc}): no findings file at the {'base' if base is None else 'head'} -- its specs did not run",
              file=sys.stderr)
        return 2
    unrun = sorted(set(specs_of(a.base, a.cc)) - set(specs_of(a.head, a.cc)))
    if unrun:
        # its findings would read as cured: a spec the head did not run is no verdict, never a pass
        print(f"corpus shape ({a.cc}): {', '.join(unrun)} wrote findings at the base and none at the head -- it did not run there",
              file=sys.stderr)
        return 2
    found = classify(base, head)
    text, md = render(a.cc, found, a.base_name, a.head_name)
    print(text)
    if a.summary:
        with open(a.summary, "a", encoding="utf-8") as f:
            f.write(md)
    return 1 if found["new"] else 0


if __name__ == "__main__":
    sys.exit(main())
