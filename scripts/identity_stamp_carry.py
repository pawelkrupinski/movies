#!/usr/bin/env python3
"""THE STAMP CARRY'S PROOFS (project/StampCarry.scala; the owner, 2026-11-20): a row of test/resources/identity-stamp-carry.tsv
lets a digest of the identity code carry an earlier digest's stamp, so a deploy of it re-decides no stored family. This
writes the rows (`propose`) and proves them again (`verify`, the pre-push hook's `stampcarry` check). A row holds only when

  1. ITS DIGESTS ARE THE RANGE'S: `digest` is <head>'s raw digest of that stamp and `equals` <base>'s, each made by the
     build itself over a tree of that commit (`scripts/identity-stamp-carry stamps <sha>`), never copied from a row;
  2. ITS PROOF HOLDS for exactly <base>..<head>, <base> an ancestor of <head>:
     - `pure-move`: scripts/pure-move-check calls the range PURE, run again here. Carries every stamp.
     - `what-if`: scripts/identity-what-if recorded, under exactly that head and base, a verdict for each of pl de uk es us
       whose newest line is `done`, over EVERY family (`--all` or `--verify`, not its selection) and with no take moved,
       the model's or the final one's. Carries identity-rules; agreement-rules only when the final takes were compared
       (not `--model-only`); never venue-slot (a what-if builds no venue slot);
  3. THE RANGE CHANGES NO SEMANTICS A PROOF CANNOT SEE: no commit says `Identity-Design:` (a rule change by design); none
     touches the drift harness (scripts/ci/commit_rules.py's HARNESS_FILES); and, for a what-if (which sees today's
     corpus only), none touches the title rules, a resource of a module's src/main (weights, tables, data: the model's
     inputs) or a store or codec (*Store*, *Codec*, *Bson* under src/main: what a stored family is read back by).

A row is never edited: a row a push changes is a new row, and proven as one. A row taken out only bumps its digest's
stamp, as an unproven change does.

usage:
  identity_stamp_carry.py propose <base>..<head> [--proof pure-move|what-if] [--table FILE]
  identity_stamp_carry.py verify <old>..<new>
Exit 0 written / every new row proven; 1 refused (each reason named); 2 usage, or a range git cannot read.
Tested by scripts/identity_stamp_carry_test.py.
"""

from __future__ import annotations

import os
import re
import subprocess
import sys
from dataclasses import dataclass

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "ci"))
from commit_rules import HARNESS_FILES  # noqa: E402  (the drift harness: one definition)

TABLE = "test/resources/identity-stamp-carry.tsv"
STAMPS = ("identity-rules", "venue-slot", "agreement-rules")
COUNTRIES = ("pl", "de", "uk", "es", "us")
# what a what-if cannot vouch for: the title rules (data a corpus covers thinly), a module's resources (the model's
# inputs), a store or codec (how a stored family is read back)
WHAT_IF_BLIND = re.compile(
    r"^(common/src/main/resources/title-(rules|words)\.tsv|common/src/main/scala/services/titles/rules/.*"
    r"|[a-z]+/src/main/resources/.*|[a-z]+/src/main/scala/.*(Store|Codec|Bson)[A-Za-z]*\.scala)$")


class Refused(Exception):
    pass


class Undecidable(Exception):
    pass


@dataclass(frozen=True)
class Row:
    stamp: str
    digest: str
    equals: str
    proof: str
    base: str
    head: str

    def line(self) -> str:
        return "\t".join((self.stamp, self.digest, self.equals, self.proof, self.base, self.head))


def git(*args: str, ok: tuple[int, ...] = (0,)) -> str:
    done = subprocess.run(["git", *args], capture_output=True, text=True)
    if done.returncode not in ok:
        raise Undecidable(f"git {' '.join(args)}: {done.stderr.strip() or done.returncode}")
    return done.stdout


def rows(text: str) -> list[Row]:
    out = []
    for line in text.splitlines():
        if not line.strip() or line.startswith("#"):
            continue
        fields = line.split("\t")
        if len(fields) != 6:
            raise Refused(f"{TABLE}: a row of {len(fields)} fields, not 6: {line}")
        out.append(Row(*fields))
    return out


def table_at(rev: str) -> list[Row]:
    return rows(git("show", f"{rev}:{TABLE}", ok=(0, 128)))


def sha_of(ref: str) -> str:
    return git("rev-parse", "--verify", f"{ref}^{{commit}}").strip()


def stamps_of(sha: str) -> dict[str, str]:
    """Each stamp's RAW digest at `sha`, made by the build over a tree of that commit (identity-stamp-carry stamps). A
    test names another maker in KINOWO_STAMP_CARRY_STAMPS: the compile is the infrastructure, the rules are this file's."""
    maker = os.environ.get("KINOWO_STAMP_CARRY_STAMPS") or os.path.join(HERE, "identity-stamp-carry")
    done = subprocess.run([maker, "stamps", sha], capture_output=True, text=True)
    if done.returncode != 0:
        raise Undecidable(f"the stamps of {sha[:12]} could not be made (exit {done.returncode}): {done.stderr.strip()[-600:]}")
    stamps = dict(line.split("\t", 1) for line in done.stdout.splitlines() if "\t" in line)
    missing = [s for s in STAMPS if not re.fullmatch(r"[0-9a-f]{64}", stamps.get(s, ""))]
    if missing:
        raise Undecidable(f"the stamps of {sha[:12]} lack {', '.join(missing)}: {done.stdout.strip()}")
    return stamps


def what_if_verdicts(head: str, base: str) -> dict[str, list[str]]:
    """Each country's newest line of identity-what-if's record under (head, base) (identity-tools.sh's what_if_record_verdict):
    cc, verdict, finished, takes diff, moved, mode."""
    cache = os.environ.get("KINOWO_IDENTITY_CACHE") or os.path.expanduser("~/.cache/kinowo-identity")
    path = os.path.join(cache, "what-if-verdicts", f"{head}-{base}.tsv")
    newest: dict[str, list[str]] = {}
    if os.path.isfile(path):
        with open(path) as f:
            for line in f:
                fields = line.rstrip("\n").split("\t")
                newest[fields[0]] = fields
    return newest


def covered(proof: str, base: str, head: str) -> tuple[set[str], list[str]]:
    """The stamps `proof` over base..head carries, and why it carries none, if it does not hold."""
    if base == head or subprocess.run(["git", "merge-base", "--is-ancestor", base, head], capture_output=True).returncode != 0:
        return set(), [f"{base[:12]} is not an ancestor of {head[:12]}"]
    why = []
    for sha in git("rev-list", f"{base}..{head}").split():
        design = git("log", "-1", "--format=%(trailers:key=Identity-Design,valueonly)", sha).strip()
        if design:
            why.append(f"{sha[:12]} says 'Identity-Design: {design}': a rule change by design is decided again, never carried")
    changed = git("diff", "--name-only", "-M", base, head).split()
    harness = [p for p in changed if HARNESS_FILES.match(p)]
    if harness:
        why.append(f"the range changes the drift harness ({' '.join(harness)}): it is never carried")
    if proof == "pure-move":
        done = subprocess.run([sys.executable, "-I", os.path.join(HERE, "pure_move_check.py"), f"{base}..{head}", "--quiet"],
                              capture_output=True, text=True)
        if done.returncode != 0:
            tail = (done.stdout + done.stderr).strip().splitlines()[-3:]
            why.append(f"scripts/pure-move-check does not call {base[:12]}..{head[:12]} PURE (exit {done.returncode}): {' / '.join(tail)}")
        return (set() if why else set(STAMPS)), why
    if proof != "what-if":
        return set(), [f"no proof {proof!r} (pure-move or what-if)"]
    blind = [p for p in changed if WHAT_IF_BLIND.match(p)]
    if blind:
        why.append(f"a what-if sees today's corpus only, and the range changes what it cannot vouch for: {' '.join(blind)}")
    verdicts = what_if_verdicts(head, base)
    final = True
    for cc in COUNTRIES:
        v = verdicts.get(cc)
        if v is None:
            why.append(f"{cc}: no identity-what-if verdict recorded for head {head[:12]} over base {base[:12]}")
            continue
        verdict, moved, mode = v[1], (v[4] if len(v) > 4 else ""), (v[5] if len(v) > 5 else "")
        if verdict != "done":
            why.append(f"{cc}: its newest verdict is {verdict!r}, not done")
        if moved != "0":
            why.append(f"{cc}: {moved or 'an unrecorded number of'} take(s) moved")
        if "whole" not in mode.split(","):
            why.append(f"{cc}: resolved its selection, not every family (run identity-what-if --all)")
        final = final and "final" in mode.split(",")
    if why:
        return set(), why
    return ({"identity-rules", "agreement-rules"} if final else {"identity-rules"}), []


def propose(rng: str, proof: str | None, table: str) -> int:
    base, head = (sha_of(r) for r in rng.split("..", 1))
    kinds = [proof] if proof else ["pure-move", "what-if"]
    carried, refusals = set(), []
    for kind in kinds:
        carried, why = covered(kind, base, head)
        if carried:
            proof = kind
            break
        refusals += [f"{kind}: {w}" for w in why]
    if not carried:
        print("REFUSED: no proof holds for " + rng + "; its stamps bump, as any unproven change's do", file=sys.stderr)
        for w in refusals:
            print(f"  {w}", file=sys.stderr)
        return 1
    before, after = stamps_of(base), stamps_of(head)
    with open(table) as f:
        held = {(r.stamp, r.digest): r for r in rows(f.read())}
    new = []
    for stamp in STAMPS:
        if before[stamp] == after[stamp]:
            print(f"{stamp}: unchanged by the range, nothing to carry")
        elif stamp not in carried:
            print(f"{stamp}: a {proof} proof does not carry it: it bumps")
        elif (stamp, after[stamp]) in held:
            print(f"{stamp}: {after[stamp][:12]} is carried already")
        else:
            new.append(Row(stamp, after[stamp], before[stamp], proof, base, head))
            print(f"{stamp}: {after[stamp][:12]} carries {before[stamp][:12]}'s stamp ({proof})")
    if new:
        with open(table, "a") as f:
            f.writelines(r.line() + "\n" for r in new)
        print(f"{len(new)} row(s) added to {table}: commit it (the hook's stampcarry check proves each again)")
    return 0


def verify(rng: str) -> int:
    old, new = rng.split("..", 1)
    before = {r.line() for r in table_at(old)}
    after = table_at(new)
    added = [r for r in after if r.line() not in before]
    twice = sorted({f"{r.stamp} {r.digest[:12]}" for r in after if sum(1 for o in after if (o.stamp, o.digest) == (r.stamp, r.digest)) > 1})
    refusals = [f"one digest carried twice: {t}" for t in twice]
    proofs: dict[tuple[str, str, str], tuple[set[str], list[str]]] = {}
    stamps: dict[str, dict[str, str]] = {}
    for r in added:
        label = f"{r.stamp} {r.digest[:12]} ({r.proof} {r.base[:12]}..{r.head[:12]})"
        if r.stamp not in STAMPS:
            refusals.append(f"{label}: no stamp {r.stamp}")
            continue
        try:
            key = (r.proof, r.base, r.head)
            if key not in proofs:
                proofs[key] = covered(*key)
            carried, why = proofs[key]
            if not carried:
                refusals += [f"{label}: {w}" for w in why]
                continue
            if r.stamp not in carried:
                refusals.append(f"{label}: a {r.proof} proof does not carry {r.stamp}")
                continue
            for sha in (r.base, r.head):
                stamps.setdefault(sha, stamps_of(sha))
        except Undecidable as e:
            refusals.append(f"{label}: {e}")
            continue
        if r.digest != stamps[r.head][r.stamp]:
            refusals.append(f"{label}: digest is not {r.head[:12]}'s ({stamps[r.head][r.stamp][:12]})")
        if r.equals != stamps[r.base][r.stamp]:
            refusals.append(f"{label}: equals is not {r.base[:12]}'s ({stamps[r.base][r.stamp][:12]})")
        if not any(w.startswith(label) for w in refusals):
            print(f"stampcarry: {label} proven")
    if refusals:
        for w in refusals:
            print(f"stampcarry: REFUSED {w}", file=sys.stderr)
        print("stampcarry: a carry row is written by scripts/identity-stamp-carry propose <base>..<head>, never by hand; "
              "take the refused rows out (the stamp then bumps, as before)", file=sys.stderr)
        return 1
    if not added:
        print("stampcarry: no row added")
    return 0


def main(argv: list[str]) -> int:
    if len(argv) < 2 or argv[0] not in ("propose", "verify") or ".." not in argv[1]:
        print(__doc__, file=sys.stderr)
        return 2
    try:
        if argv[0] == "verify":
            return verify(argv[1])
        proof, table, rest = None, TABLE, argv[2:]
        while rest:
            if rest[0] == "--proof" and len(rest) > 1:
                proof, rest = rest[1], rest[2:]
            elif rest[0] == "--table" and len(rest) > 1:
                table, rest = rest[1], rest[2:]
            else:
                print(f"unknown argument {rest[0]}", file=sys.stderr)
                return 2
        return propose(argv[1], proof, table)
    except Refused as e:
        print(f"REFUSED: {e}", file=sys.stderr)
        return 1
    except Undecidable as e:
        print(f"undecidable: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
