"""THE PRE-PUSH `stampcarry` CHECK (scripts/identity_stamp_carry.py `verify`): every row a push adds proven again at the
range the push holds -- its stamp known, its proof holding, its digests the ones the build makes.

Imported by identity_stamp_carry.py, which has put scripts/, scripts/lib and scripts/ci on sys.path."""
from __future__ import annotations

import sys
from identity_stamp_carry_proof import STAMPS, Row, covered, stamps_of, table_at  # noqa: E402
from identity_stamp_carry_ranges import RANGES, cited_range  # noqa: E402  (THE QUEUE REWRITES)
from pure_move_check import Undecidable  # noqa: E402


def row_refusals(r: Row, old: str, new: str, proofs: dict[tuple[str, str, str], tuple[set[str], list[str]]],
                 stamps: dict[str, dict[str, str]]) -> list[str]:
    """Why the added row `r` does not hold in the push old..new, proven where the push holds its range: the cited one,
    or the one the queue's picks rewrote it into (THE QUEUE REWRITES). `proofs` and `stamps` are shared by its rows."""
    label = f"{r.stamp} {r.digest[:12]} ({r.proof} {r.base[:12]}..{r.head[:12]})"
    if r.stamp not in STAMPS:
        return [f"{label}: no stamp {r.stamp}"]
    found = cited_range(r.base, r.head, old, new)
    if found is None:
        return [f"{label}: it cites {r.head[:12]}, which is not in the pushed history, and no pushed range is its diff "
                f"({RANGES} has no row of it, or the rebase changed what the range changes)"]
    base, head = found
    try:
        why = proof_refusals(r, base, head, new, proofs)
        if why:
            return [f"{label}: {w}" for w in why]
        for sha in (base, head):
            stamps.setdefault(sha, stamps_of(sha))
    except Undecidable as e:
        return [f"{label}: {e}"]
    why = digest_refusals(r, base, head, stamps)
    if not why:
        print(f"stampcarry: {label} proven" + ("" if found == (r.base, r.head) else f" at {base[:12]}..{head[:12]}"))
    return [f"{label}: {w}" for w in why]


def proof_refusals(r: Row, base: str, head: str, new: str,
                   proofs: dict[tuple[str, str, str], tuple[set[str], list[str]]]) -> list[str]:
    """Why r's proof, run again over base..head (once a push), does not carry r's stamp."""
    key = (r.proof, base, head)
    if key not in proofs:
        proofs[key] = covered(*key, at=new, record=(r.head, r.base))
    carried, why = proofs[key]
    if not carried:
        return why
    return [] if r.stamp in carried else [f"a {r.proof} proof does not carry {r.stamp}"]


def digest_refusals(r: Row, base: str, head: str, stamps: dict[str, dict[str, str]]) -> list[str]:
    """Where r's digests are not the ones the build makes of head and base."""
    refusals: list[str] = []
    if r.digest != stamps[head][r.stamp]:
        refusals.append(f"digest is not {head[:12]}'s ({stamps[head][r.stamp][:12]})")
    if r.equals != stamps[base][r.stamp]:
        refusals.append(f"equals is not {base[:12]}'s ({stamps[base][r.stamp][:12]})")
    return refusals


def added_rows(old: str, new: str) -> tuple[list[Row], list[str]]:
    """The rows the push adds to the table, and the refusals of a digest the table carries twice."""
    before = {r.line() for r in table_at(old)}
    after = table_at(new)
    twice = sorted({f"{r.stamp} {r.digest[:12]}" for r in after if sum(1 for o in after if (o.stamp, o.digest) == (r.stamp, r.digest)) > 1})
    return [r for r in after if r.line() not in before], [f"one digest carried twice: {t}" for t in twice]


def verify(rng: str) -> int:
    old, new = rng.split("..", 1)
    added, refusals = added_rows(old, new)
    proofs: dict[tuple[str, str, str], tuple[set[str], list[str]]] = {}
    stamps: dict[str, dict[str, str]] = {}
    for r in added:
        refusals += row_refusals(r, old, new, proofs, stamps)
    if refusals:
        for w in refusals:
            print(f"stampcarry: REFUSED {w}", file=sys.stderr)
        print("stampcarry: a carry row is written by scripts/identity-stamp-carry propose <base>..<head>, never by hand; "
              "take the refused rows out (the stamp then bumps, as before)", file=sys.stderr)
        return 1
    if not added:
        print("stampcarry: no row added")
    return 0
