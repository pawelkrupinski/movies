#!/usr/bin/env python3
"""THE DRIFT HARNESS'S A/A SELF-TEST (identity-and-convergence review 2026-11-06, P5; the owner approved it).

WHY. The differential drift gate compares head vs base under the same harness, so a harness that is itself not a function
of its inputs (the shared rng that changed what a change asked) reads as the resolver's defect: 13 convergence episodes,
~71 red hours, harness-bug. An A/A run -- the harness at one commit replayed twice against ITSELF -- has nothing a
resolver change could move, so any difference is the harness's own fault, and fails as such.

WHERE. In main's convergence (identity-model-convergence.yml), AFTER landing, never in pre-push or `agentctl land`: the
preflight runs `decide`, and when the push changed a path of .github/harness-aa-paths.txt each leg adds two rows,
`takes-drift-aa-pass0|1 (<country>)`, side by side, each running scripts/ci/identity-drift-check.sh once on the row-1
seeds with DRIFT_BASE=<head> (no base replay: base and head are one commit), then `judge`; its `order-compare` job then
compares the two passes' TSVs (scripts/ci/drift-replays-compare.sh) as `takes-drift-aa (<country>)` and `judge`s that;
and one row `takes-aa (<country>)`, the
takes check's own harness (scripts/ci/identity-takes-check.sh with TAKES_AA=true: the head resolved and its final takes
replayed twice, in two checkouts, nothing reused from the store), then `takes`. A push touching no harness path runs no
such row: it costs nothing elsewhere. The rows are registered `unproven` in .github/gating-checks.tsv until their first
green run on main, so a red there reports and holds no push until then.

usage: harness_aa.py decide --head SHA [--since SHA] [--paths FILE]
         prints `harness-aa=true|false` and `harness-aa-paths=<changed harness paths>` (GITHUB_OUTPUT lines): the range is
         since..head, `since` the previous convergence run's head on main; empty, unknown or not an ancestor: head's parent.
       harness_aa.py judge --cc CC --code N --verdict FILE
         the A/A row's drift-check exit N: 0 PASS; 1 with a `not deterministic` verdict: HARNESS FAULT (exit 1); 1
         otherwise (a divergence: the head's own, judged by takes-drift-1/2): a notice, exit 0; anything else: exit 2.
       harness_aa.py takes --cc CC --a DIR --b DIR
         THE TAKES HARNESS'S A/A (the `takes-aa` row: identity-takes-check.sh with TAKES_AA=true resolves and replays the
         head twice over one recording, against itself): DIR/decisions-CC.jsonl (the model's takes) and
         DIR/ratchet-takes.tsv (the final takes) of the two sides. Any listing whose take differs at all -- lost, changed or
         gained -- is HARNESS FAULT (exit 1), never a move to judge or accept; the same: PASS (0); a side unreadable: 2.
       harness_aa.py moved --cc CC --a DIR --b DIR
         prints `moved` when any take differs between the two sides, `unmoved` when none does, `unknown` when a side cannot
         be read (the takes check's skip-gate shadow); exit 0.
Tested by harness_aa_test.py.
"""
from __future__ import annotations

import csv
import json
import os
import subprocess
import sys

PATHS = ".github/harness-aa-paths.txt"


def git(*args: str) -> subprocess.CompletedProcess:
    return subprocess.run(["git", "-c", "core.quotePath=false", *args], capture_output=True, text=True)


def prefixes(path: str) -> list[str]:
    with open(path) as f:
        return [line.strip() for line in f if line.strip() and not line.lstrip().startswith("#")]


def decide(head: str, since: str, paths_file: str) -> int:
    base = f"{head}^"
    if since:
        if git("merge-base", "--is-ancestor", since, head).returncode == 0:
            base = since
        else:
            print(f"harness-aa: {since[:12]} is unknown here or no ancestor of {head[:12]}: judging the parent alone",
                  file=sys.stderr)
    diff = git("diff", "--name-only", "--no-renames", base, head)
    if diff.returncode != 0:
        print(f"harness-aa: could not diff {base}..{head}: {diff.stderr.strip()}", file=sys.stderr)
        return 2
    watched = prefixes(paths_file)
    hit = sorted(p for p in diff.stdout.split("\n") if p and any(p.startswith(w) for w in watched))
    print(f"harness-aa={'true' if hit else 'false'}")
    print(f"harness-aa-paths={' '.join(hit)}")
    return 0


def judge(cc: str, code: int, verdict_file: str) -> int:
    try:
        with open(verdict_file) as f:
            verdict = f.read().strip()
    except OSError:
        verdict = ""
    title = f"harness A/A ({cc})"
    if code == 0:
        print(f"{title}: PASS -- two replays of the head against itself wrote the same drift and flips TSVs")
        return 0
    if code == 1 and verdict.startswith("not deterministic"):
        print(f"::error title={title}::HARNESS FAULT, not the resolver's: {verdict} -- the head replayed twice against "
              "itself (base = head) must be identical; fix the harness (docs/design/identity-no-drift.md, \"The A/A self-test\")")
        return 1
    if code == 1:
        print(f"::notice title={title}::the A/A replays agree; the head's own verdict ({verdict or 'see the log'}) is "
              "takes-drift-1/2's to judge, not this row's")
        return 0
    print(f"::error title={title}::the drift check could not run (exit {code}): nothing was judged")
    return 2


def takes_of(side: str, cc: str) -> tuple[dict, dict]:
    """A side's takes: the model's {listing key: film} and the final {(country, venue, raw title): (status, film)}."""
    with open(os.path.join(side, f"decisions-{cc}.jsonl")) as f:
        model = {d["key"]: d.get("film") for d in map(json.loads, filter(str.strip, f))}
    with open(os.path.join(side, "ratchet-takes.tsv")) as f:
        final = {(r["country"], r["venue"], r["rawTitle"]): (r.get("status"), r.get("film"))
                 for r in csv.DictReader(f, delimiter="\t")}
    return model, final


def differing(a: dict, b: dict) -> list:
    return sorted((k for k in a.keys() | b.keys() if a.get(k, "absent") != b.get(k, "absent")), key=str)


def takes_differences(a: str, b: str, cc: str) -> tuple[list, list] | None:
    """The listings whose model take and whose final take differ between the sides; None when a side cannot be read."""
    try:
        (am, af), (bm, bf) = takes_of(a, cc), takes_of(b, cc)
    except (OSError, ValueError, KeyError):
        return None
    return differing(am, bm), differing(af, bf)


def takes(cc: str, a: str, b: str) -> int:
    title = f"harness A/A ({cc})"
    found = takes_differences(a, b, cc)
    if found is None:
        print(f"::error title={title}::a side's takes could not be read ({a}, {b}): nothing was judged")
        return 2
    model, final = found
    if not model and not final:
        print(f"{title}: PASS -- the head resolved and replayed twice against itself took every listing alike")
        return 0
    named = "; ".join(str(k) for k in (model + final)[:5])
    print(f"::error title={title}::HARNESS FAULT, not the resolver's: {len(model)} model take(s) and {len(final)} final "
          f"take(s) differ between two runs of the head against itself (first: {named}) -- the takes check must be a "
          "function of its commit and recording; fix the harness (docs/design/identity-no-drift.md, \"The A/A self-test\")")
    return 1


def moved(cc: str, a: str, b: str) -> int:
    found = takes_differences(a, b, cc)
    print("unknown" if found is None else "moved" if found[0] or found[1] else "unmoved")
    return 0


def main(argv: list[str]) -> int:
    usage = __doc__.split("usage: ", 1)[1].split("Tested by", 1)[0]
    if not argv or argv[0] not in ("decide", "judge", "takes", "moved"):
        print(f"usage: {usage}", file=sys.stderr)
        return 2
    opts, rest = {}, argv[1:]
    while rest:
        if not rest[0].startswith("--") or len(rest) < 2:
            print(f"harness-aa: unknown argument {rest[0]!r}", file=sys.stderr)
            return 2
        opts[rest[0][2:]] = rest[1]
        rest = rest[2:]
    try:
        if argv[0] == "decide":
            return decide(opts["head"], opts.get("since", ""), opts.get("paths", PATHS))
        if argv[0] == "takes":
            return takes(opts["cc"], opts["a"], opts["b"])
        if argv[0] == "moved":
            return moved(opts["cc"], opts["a"], opts["b"])
        return judge(opts["cc"], int(opts["code"]), opts["verdict"])
    except (KeyError, ValueError) as e:
        print(f"harness-aa: missing or bad argument {e}\nusage: {usage}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
