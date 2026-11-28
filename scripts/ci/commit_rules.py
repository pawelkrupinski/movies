#!/usr/bin/env python3
# Purpose: The commit rules of the drift gate: the pre-push hook's allowlists and driftguards checks
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
#   THE FLIP BACKLOG (test/resources/fixtures/identity-flip-backlog.tsv, 2026-11-04; it merged the known-flips baseline,
#   identity-flips-baseline.tsv, and the population walk's known flips) takes rows as the baseline did, below, and by
#   THE EXPLORER'S FILING: a commit changing only the backlog and drift-gate-seeds.tsv, adding only `drift` rows
#   (explorer_filing). The baseline's rules, as they were:
#   THE KNOWN-FLIPS BASELINE ONLY SHRINKS (test/resources/fixtures/identity-flips-baseline.tsv; the owner, 2026-10-29:
#   "fail build only on new flips, focus in eliminating all old flips and then fail on all flips"). Rows are written only
#   by its creation, once (a file that existed before is never made again), and by a TIGHTENED CHECK's birth: a commit
#   carrying `New-Check:` that changes what decides what the gate finds, never `Fixes-Red:`, may baseline the flips the
#   tightened check finds at birth, each a listing no list or earlier baseline named (a drift slice's listing may be
#   baselined: the slice holds its past flip; the owner, 2026-10-30: a229501a7's precise `none` revealed 138 flips the
#   blind predicate hid, "baseline them, then burn them down"). The one exception is the OWNER'S NAMED DECISION (the
#   owner, 2026-11-02: CinemaxX Dammtor/Raschplatz "Titans", a flip inherent to which listings are present): a commit
#   carrying `Owner-Approved: <YYYY-MM-DD> <decision and why>`, each row it adds noted `OWNER-APPROVED <that date>:
#   <why>` (owner_approved). Any other added row is refused: no override.
#   THE STRUCK FLIPS (test/resources/fixtures/identity-flips-struck.tsv; the owner, 2026-11-09, DE "Titans"): a listing
#   whose flips are an input artefact, struck from the backlog, its slices kept as the witness. A row lands ONLY by the
#   owner's named decision (`Owner-Approved:`, owner_approved), never by a birth or a filing; its creation is no exception.
#   A registered list is a TSV whose last column is the row's reason; `#` lines and the header (its first other line)
#   are not rows. Every allow-list file in the repository is registered (AllowListRatchetSpec).
#
# `driftguards` -- the owner, 2026-10-29:
#   - A DRIFT OR FLIP RED IS NOT CLOSED UNTIL ITS SLICE IS COMMITTED. A commit naming a red (`Fixes-Red:`) whose message
#     speaks of drift or flips comes with a drift slice of the listing it fixes -- a change under SLICES
#     (scripts/identity-slice <cc> SELECTOR --emit-drift-slice) anywhere in the same push -- so IdentityDriftSlicesSpec
#     holds the fix from then on (convergence-failures 2026-10-29, G2: E3, E4, E8 and E9's re-breaks were listings that
#     had failed before). A fix whose red has no listing to slice (a CI script, a timeout) says `Drift-Slice: none -- <why>`;
#     for one already on main (never amended), a later commit of the judged range says `Drift-Slice-For: <sha> none -- <why>`.
#   - A RED FIX IS NO RESOLVER RULE. A flip or drift is fixed in the incremental state that diverges -- a lag, a budget,
#     the queue's order, a cache, a restart's key -- never by a new identity resolver rule: a rule is identity design,
#     measured on the full corpus in every country. A commit naming a red (`Fixes-Red:`) that changes RULE_FILES carries
#     `Identity-Design: <doc>`, a document that exists in the commit's tree. Mechanism files need none, nor does a revert
#     (`This reverts commit <sha>`) that leaves every rule file exactly as it was before <sha>: it adds no rule.
#   - A CHANGE TO THE DRIFT HARNESS CARRIES ITS SELF-TEST (G6): a commit changing HARNESS_FILES also changes one of
#     SELF_TEST_FILES -- a third of the week's drift reds were the judge's, not the product's (91cfbe7e8, 7b586af0f,
#     5df3295bd) -- or says `Drift-Harness-Refactor: <why>`.
#   - GAUGES BEFORE BEHAVIOUR (identity-and-convergence review 2026-11-06, P4; the owner approved it): a commit changing
#     what the worker serves over time (SERVING_FILES: the model's input gate, holds, settle, asks) changes a gauge,
#     alert or dashboard (GAUGE_FILES) in the same commit, or names the EARLIER push's commit that did
#     (`Gauges-Shipped: <sha>`, an ancestor outside this push), or says `Gauges: none -- <why>`. On 11-06 the readiness
#     gate deployed at 07:50Z, its gauges ~12:20Z, and stuck listings went unseen ~4.5 h. Watch the roll with
#     scripts/deploy-watch. A serving file the commit changes only in its imports, comments and declared types
#     (`retypes_only`: a parameter narrowed to a role trait) moves no behaviour and needs none of these.
#
# usage: commit_rules.py allowlists|driftguards --range A..B [--range C..D ...] [--registry .github/allow-lists.txt]
# Prints each refusal and why; exit 0 none, 1 some, 2 usage or an unreadable range. Tested by commit_rules_test.py.

from __future__ import annotations

import os
import re
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # its modules beside it, under `python3 -I` too
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import tool_git  # noqa: E402
from allow_list_rules import REGISTRY, allow_lists, registered  # noqa: E402
from commit_facts import Commit, Rule  # noqa: E402
from drift_guard_rules import (drift_fix_carries_its_slice, gauges_before_behaviour,  # noqa: E402
                               harness_carries_self_test, red_fix_is_no_rule)

SLICE_FOR = re.compile(r"^([0-9a-f]{7,40})\s+(.+)$", re.I)

RULE_SETS: dict[str, list[Rule]] = {
    "allowlists": [allow_lists],
    "driftguards": [drift_fix_carries_its_slice, red_fix_is_no_rule, harness_carries_self_test, gauges_before_behaviour],
}


def check(rule_set: str, ranges: list[str], opts: dict) -> list[str]:
    refused = []
    for rng in ranges:
        # what the whole push changes: a drift fix's slice may come in a later commit of it
        opts = {**opts, "pushed": set(tool_git.Git.here().text("diff", "--name-only", "--no-renames", rng.replace("..", "...", 1)).split("\n")) - {""}}
        commits = [Commit.read(sha) for sha in tool_git.Git.here().text("rev-list", "--reverse", rng).split()]
        opts["in_push"] = {c.sha for c in commits}
        # a published commit's `Drift-Slice:` given by a later commit of the range, by its sha: main is never amended
        opts["declared"] = [(m.group(1).lower(), m.group(2)) for c in commits for v in c.trailers.get("drift-slice-for", [])
                            if (m := SLICE_FOR.match(v))]
        for c in commits:
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
