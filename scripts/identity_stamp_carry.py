#!/usr/bin/env python3
"""THE STAMP CARRY'S PROOFS (project/StampCarry.scala; the owner, 2026-11-20): a row of test/resources/identity-stamp-carry.tsv
lets a digest of the identity code carry an earlier digest's stamp, so a deploy of it re-decides no stored family. This
writes the rows (`propose`) and proves them again (`verify`, the pre-push hook's `stampcarry` check). A row holds only when

  1. ITS DIGESTS ARE THE RANGE'S: `digest` is <head>'s raw digest of that stamp and `equals` <base>'s, each made by the
     build itself over a tree of that commit (`scripts/identity-stamp-carry stamps <sha>`), never copied from a row;
  2. ITS PROOF HOLDS for exactly <base>..<head>, <base> an ancestor of <head>:
     - `pure-move`: scripts/pure-move-check calls the range PURE, run again here. Carries every stamp.
     - `what-if`: scripts/identity-what-if recorded, under exactly that head and base, a verdict for each of pl de uk es us
       -- read from this machine's record by `propose`/`auto`, which commit a copy beside the rows
       (test/resources/identity-what-if-verdicts/<head>-<base>.tsv), and from that copy by `verify`, so ci-runner proves
       the row as the laptop does --
       whose newest line is `done`, over EVERY family (`--all` or `--verify`, not its selection) and with no take moved,
       the model's or the final one's, AND that line is the one the what-if PUBLISHED (THE PUBLISHED VERDICT, below).
       Carries identity-rules; agreement-rules only when the final takes were compared (not `--model-only`); never
       venue-slot (a what-if builds no venue slot);
  3. THE RANGE CHANGES NO SEMANTICS A PROOF CANNOT SEE: no commit says `Identity-Design:` (a rule change by design); for
     a what-if, none touches the drift harness (scripts/ci/drift_guard_rules.py's HARNESS_FILES: a what-if never resolves
     through it, so it cannot vouch for it -- a pure move of it carries, as any pure move does, the owner 2026-11-21) and
     (it sees today's corpus only) none touches the title rules, a resource of a module's src/main (weights, tables,
     data: the model's inputs) or a store or codec (*Store*, *Codec*, *Bson* under src/main: what a stored family is
     read back by). A rename counts both its paths.

THE PUBLISHED VERDICT (the owner, 2026-11-21): a committed verdict is text its author writes, so a forged "0 moved" would
pass. Each identity-what-if run therefore publishes what it recorded -- its verdict lines and the takes diffs behind them
-- as ONE asset of the `identity-what-if-verdicts` release (KINOWO_WHAT_IF_RELEASE), `what-if-verdict-<key>.json`, the key
a hash of the head, base, the diff's patch-id, the run's countries, the what-if tooling's hash and the run's own id; each
line it records names its asset (field 7). A what-if row holds only when every country's committed line is, byte for
byte, a line of the asset it names, the asset's head, base and patch-id are the run's, and its takes diffs hold exactly
the rows the line says moved. A missing asset, or one gh cannot fetch, refuses. Assets are fetched once into the identity
cache (<cache>/what-if-assets/), a few KB each. Trust: the upload takes the repository's write credentials (the owner's
gh on the laptop; a workflow token with contents: write), and GitHub keeps when and by whom it was uploaded. So it proves
the committed verdict is the one published at the time by a holder of those credentials -- editing the committed file
is caught; it does not prove the what-if made the asset (a credential holder could upload one by hand), nor that the
corpus it read was prod's (the mirror is the laptop's own).

THE QUEUE REWRITES WHAT LAND WROTE: a row whose cited commits the queue's picks rewrote is proven at the pushed range
holding the same diff (scripts/identity_stamp_carry_ranges.py), its proof and digests made again there.

A row is never edited: a row a push changes is a new row, and proven as one. A row taken out only bumps its digest's
stamp, as an unproven change does.

usage:
  identity_stamp_carry.py propose <base>..<head> [--proof pure-move|what-if] [--table FILE]
  identity_stamp_carry.py verify <old>..<new>
  identity_stamp_carry.py auto <base>..<head> [--no-commit]   (agentctl land's, after its rebase: see `auto`)
  identity_stamp_carry.py asset-name|publish <head> <base> <countries> <tools hash> <run id>   (identity-what-if's)
Exit 0 written / every new row proven; 1 refused (each reason named); 2 usage, or a range git cannot read.
Tested by scripts/identity_stamp_carry_test.py.
"""

from __future__ import annotations

import os
import sys
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(HERE, "lib"), os.path.join(HERE, "ci")]  # its own modules, tool_python, drift_guard_rules
from identity_stamp_carry_published import asset_name, diff_id, publish  # noqa: E402
from identity_stamp_carry_proof import TABLE, Refused, covered, sha_of, stamps_of  # noqa: E402
from identity_stamp_carry_rows import DIGESTED_MODULES as DIGESTED_MODULES  # noqa: E402,F401  (the stamps' modules, its test's)
from identity_stamp_carry_rows import STAMP_BUILD_FILES as STAMP_BUILD_FILES  # noqa: E402,F401
from identity_stamp_carry_rows import auto, carry_rows, write_rows  # noqa: E402
from identity_stamp_carry_verify import verify  # noqa: E402
from pure_move_check import Undecidable  # noqa: E402


def propose(rng: str, proof: str | None, table: str) -> int:
    base, head = (sha_of(r) for r in rng.split("..", 1))
    kinds = [proof] if proof else ["pure-move", "what-if"]
    carried, refusals, proven = set(), [], ""
    for kind in kinds:
        carried, why = covered(kind, base, head)
        if carried:
            proven = kind
            break
        refusals += [f"{kind}: {w}" for w in why]
    if not carried:
        print("REFUSED: no proof holds for " + rng + "; its stamps bump, as any unproven change's do", file=sys.stderr)
        for w in refusals:
            print(f"  {w}", file=sys.stderr)
        return 1
    before, after = stamps_of(base), stamps_of(head)
    new = carry_rows(proven, base, head, carried, before, after, table)
    write_rows(table, new)
    return 0


def options(rest: list[str]) -> tuple[str | None, str] | None:
    """(--proof, --table) of propose's arguments, or None after naming an unknown one."""
    proof, table = None, TABLE
    while rest:
        if rest[0] == "--proof" and len(rest) > 1:
            proof, rest = rest[1], rest[2:]
        elif rest[0] == "--table" and len(rest) > 1:
            table, rest = rest[1], rest[2:]
        else:
            print(f"unknown argument {rest[0]}", file=sys.stderr)
            return None
    return proof, table


def asset_command(argv: list[str]) -> int:
    """identity-what-if's `asset-name` / `publish <head> <base> <countries> <tools hash> <run id>`."""
    head, base, *rest = argv[1:]
    if argv[0] == "publish":
        return publish(head, base, *rest)
    print(asset_name(head, base, diff_id(base, head), *rest))
    return 0


def command(argv: list[str]) -> int:
    if argv[0] == "verify":
        return verify(argv[1])
    if argv[0] == "auto":
        return auto(argv[1], TABLE, commit="--no-commit" not in argv[2:])
    parsed = options(argv[2:])
    return 2 if parsed is None else propose(argv[1], *parsed)


def main(argv: list[str]) -> int:
    if argv[:1] in (["asset-name"], ["publish"]) and len(argv) == 6:
        return asset_command(argv)
    if len(argv) < 2 or argv[0] not in ("propose", "verify", "auto") or ".." not in argv[1]:
        print(__doc__, file=sys.stderr)
        return 2
    try:
        return command(argv)
    except Refused as e:
        print(f"REFUSED: {e}", file=sys.stderr)
        return 1
    except Undecidable as e:
        print(f"undecidable: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
