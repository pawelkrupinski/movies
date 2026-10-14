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
and head without making it new.

usage: corpus_shape_diff.py <cc> <base dir> <head dir> [--summary F] [--base-name S] [--head-name S]
Exit 1 on a new finding, 2 when a side wrote no findings file for <cc> (its specs did not run): no verdict.
"""

import argparse
import glob
import os
import sys


def findings_of(directory, cc):
    """{(spec, kind, venue, listing): what} of every corpus-shape file `directory` holds for `cc`'s recorded corpus;
    None when it holds none."""
    paths = sorted(glob.glob(os.path.join(directory, f"corpus-shape-recorded-{cc}-*.txt")))
    if not paths:
        return None
    out = {}
    for path in paths:
        spec = os.path.basename(path)[len(f"corpus-shape-recorded-{cc}-"):-len(".txt")]
        with open(path, encoding="utf-8") as f:
            for line in f:
                cols = line.rstrip("\n").split("\t")
                if len(cols) >= 3:
                    out[(spec, cols[0], cols[1], cols[2])] = cols[3] if len(cols) > 3 else ""
    return out


def classify(base, head):
    """{"new", "standing", "cured"}: (key, what) pairs, sorted."""
    return {"new": sorted((k, head[k]) for k in head.keys() - base.keys()),
            "standing": sorted((k, head[k]) for k in head.keys() & base.keys()),
            "cured": sorted((k, base[k]) for k in base.keys() - head.keys())}


def cell(text):
    return str(text).replace("|", "\\|").replace("\n", " ")


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
    found = classify(base, head)
    text, md = render(a.cc, found, a.base_name, a.head_name)
    print(text)
    if a.summary:
        with open(a.summary, "a", encoding="utf-8") as f:
            f.write(md)
    return 1 if found["new"] else 0


if __name__ == "__main__":
    sys.exit(main())
