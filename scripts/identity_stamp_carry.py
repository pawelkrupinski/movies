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
  identity_stamp_carry.py auto <base>..<head> [--no-commit]   (agentctl land's, after its rebase: see `auto`)
Exit 0 written / every new row proven; 1 refused (each reason named); 2 usage, or a range git cannot read.
Tested by scripts/identity_stamp_carry_test.py.
"""

from __future__ import annotations

import glob
import os
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "ci"))
from commit_rules import HARNESS_FILES  # noqa: E402  (the drift harness: one definition)

TABLE = "test/resources/identity-stamp-carry.tsv"
# the what-if records a what-if row stands on, committed with it (<head>-<base>.tsv, identity-tools.sh's format)
VERDICTS = "test/resources/identity-what-if-verdicts"
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


def cached_verdict(head: str, base: str) -> str:
    """identity-what-if's record under (head, base) on THIS machine (identity-tools.sh's what_if_record_verdict), "" for
    none: what `propose` and `auto` read, and copy beside the rows they write (VERDICTS)."""
    cache = os.environ.get("KINOWO_IDENTITY_CACHE") or os.path.expanduser("~/.cache/kinowo-identity")
    path = os.path.join(cache, "what-if-verdicts", f"{head}-{base}.tsv")
    if not os.path.isfile(path):
        return ""
    with open(path) as f:
        return f.read()


def verdict_path(head: str, base: str) -> str:
    return f"{VERDICTS}/{head}-{base}.tsv"


def what_if_verdicts(head: str, base: str, at: str | None = None) -> dict[str, list[str]]:
    """Each country's newest line of the what-if record under (head, base) -- cc, verdict, finished, takes diff, moved,
    mode: this machine's own record, or, `at` a commit, the record committed there beside the rows (what `verify` reads,
    so the hook proves a row alike on the laptop and on ci-runner, where no what-if ever ran)."""
    text = cached_verdict(head, base) if at is None else git("show", f"{at}:{verdict_path(head, base)}", ok=(0, 128))
    newest: dict[str, list[str]] = {}
    for line in text.splitlines():
        fields = line.split("\t")
        if fields[0]:
            newest[fields[0]] = fields
    return newest


def covered(proof: str, base: str, head: str, at: str | None = None) -> tuple[set[str], list[str]]:
    """The stamps `proof` over base..head carries, and why it carries none, if it does not hold. A what-if's record is
    this machine's, or, `at` a commit, the one committed there (what_if_verdicts)."""
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
    verdicts = what_if_verdicts(head, base, at)
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
    new = carry_rows(proof, base, head, carried, before, after, table)
    write_rows(table, new)
    return 0


def carry_rows(proof: str, base: str, head: str, carried: set[str], before: dict[str, str], after: dict[str, str],
               table: str) -> list[Row]:
    """The rows a proven base..head adds to `table`: one a stamp the range moves and the proof carries, not held yet."""
    with open(table) as f:
        held = {(r.stamp, r.digest) for r in rows(f.read())}
    new = []
    for stamp in STAMPS:
        if before[stamp] == after[stamp]:
            print(f"{stamp}: unchanged by {base[:12]}..{head[:12]}, nothing to carry")
        elif stamp not in carried:
            print(f"{stamp}: a {proof} proof does not carry it: it bumps")
        elif (stamp, after[stamp]) in held:
            print(f"{stamp}: {after[stamp][:12]} is carried already")
        else:
            new.append(Row(stamp, after[stamp], before[stamp], proof, base, head))
            print(f"{stamp}: {after[stamp][:12]} carries {before[stamp][:12]}'s stamp ({proof} {base[:12]}..{head[:12]})")
    return new


def write_rows(table: str, new: list[Row]) -> list[str]:
    """Appends the rows to `table`, and copies each what-if row's record beside it (VERDICTS): the files to commit."""
    if not new:
        return []
    with open(table, "a") as f:
        f.writelines(r.line() + "\n" for r in new)
    written = [table]
    for base, head in sorted({(r.base, r.head) for r in new if r.proof == "what-if"}):
        path = verdict_path(head, base)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as f:
            f.write(cached_verdict(head, base))
        written.append(path)
    print(f"{len(new)} row(s) added to {table}: commit it with {', '.join(written[1:]) or 'nothing else'}"
          " (the hook's stampcarry check proves each again)")
    return written


# What may move a stamp: the main code or resources of a module the stamps' closures walk (build.sbt's
# `IdentityRulesSources.Module`s: the identity module and what it is built on -- never web, worker or scrapers, which it
# cannot reach), build.sbt, or a file of project/ the stamps are made by (build.sbt's stamp task: what to digest, the
# digest, the carry) -- not the rest of project/, which measures or tests the code. A line of code changed (not only a
# comment or whitespace) in one: read from the diff alone, so it errs toward yes -- a yes is proven or left to bump.
DIGESTED_MODULES = ("identity", "common", "geodata", "base", "http")
STAMP_BUILD_FILES = ("project/IdentityRulesSources.scala", "project/SourceDigest.scala", "project/StampCarry.scala")
MAY_MOVE = re.compile(rf"^(({'|'.join(DIGESTED_MODULES)})/src/main/|build\.sbt$|{'|'.join(map(re.escape, STAMP_BUILD_FILES))}$)")
COMMENT_ONLY = re.compile(r"^[+-]\s*(//|\*|/\*|\*/|$)")


def may_move(sha: str) -> bool:
    changes = [line.split("\t") for line in git("diff-tree", "--no-commit-id", "--name-status", "-M", "-r", "--root", sha).splitlines()]
    changes = [c for c in changes if any(MAY_MOVE.match(p) for p in c[1:])]
    if any(c[0] != "M" for c in changes):
        return True  # a file added, removed or moved: the digest keys each file by its path
    paths = [c[1] for c in changes]
    if not paths:
        return False
    diff = git("diff", "-w", f"{sha}^", sha, "--", *paths)
    return any(line[:1] in "+-" and not line.startswith(("+++", "---")) and not COMMENT_ONLY.match(line)
               for line in diff.splitlines())


def range_verdict(base: str, head: str) -> tuple[str, str] | None:
    """A recorded what-if (head0, base0) of the very change base..head -- the same diff, byte for byte -- taken before a
    rebase moved its shas (agentctl land rebases after the lane ran it). Its proof is of base0..head0's code; a row of it
    is live at `head` only while head's digest is head0's, which `auto` checks."""
    cache = os.environ.get("KINOWO_IDENTITY_CACHE") or os.path.expanduser("~/.cache/kinowo-identity")
    wanted = git("diff", "--binary", base, head)
    found = []
    for path in glob.glob(os.path.join(cache, "what-if-verdicts", "*-*.tsv")):
        head0, _, base0 = os.path.basename(path)[:-4].partition("-")
        if not (re.fullmatch(r"[0-9a-f]{40}", head0) and re.fullmatch(r"[0-9a-f]{40}", base0)):
            continue
        if subprocess.run(["git", "cat-file", "-e", f"{head0}^{{commit}}"], capture_output=True).returncode or \
                subprocess.run(["git", "cat-file", "-e", f"{base0}^{{commit}}"], capture_output=True).returncode:
            continue
        if git("diff", "--binary", base0, head0) == wanted:
            found.append((os.path.getmtime(path), base0, head0))
    return max(found)[1:] if found else None


def auto(rng: str, table: str, commit: bool) -> int:
    """agentctl land's step after its rebase (scripts/land-before-queue): carry what the pushed range proves, by itself.
    The whole range by a recorded whole what-if of its very diff, else commit by commit by pure-move-check. One commit
    that may move a stamp and proves nothing bumps the head's stamps whatever the rest prove -- the head is what deploys,
    and its stamp is its chain's end -- so then nothing is built and no row written."""
    base, head = (sha_of(r) for r in rng.split("..", 1))
    commits = git("rev-list", "--reverse", "--first-parent", f"{base}..{head}").split()
    moving = [c for c in commits if may_move(c)]
    if not moving:
        print(f"stamp carry: no commit of {base[:12]}..{head[:12]} may move an identity stamp")
        return 0
    steps: list[tuple[str, str, str, set[str]]] = []
    found = range_verdict(base, head)
    if found:
        carried, why = covered("what-if", *found)
        if carried:
            steps = [("what-if", found[0], found[1], carried)]
        else:
            print(f"stamp carry: the recorded what-if {found[0][:12]}..{found[1][:12]} of this change proves nothing: {'; '.join(why)}")
    if not steps:
        for c in moving:
            parent = git("rev-parse", f"{c}^").strip()
            carried, why = covered("pure-move", parent, c)
            if not carried:
                print(f"stamp carry: {c[:12]} {git('log', '-1', '--format=%s', c).strip()[:70]!r} may move a stamp and proves "
                      "nothing (no pure move, no whole what-if of the range): the push bumps its stamps")
                return 0
            steps.append(("pure-move", parent, c, carried))
    needed = sorted({sha for _, b, h, _ in steps for sha in (b, h)} | ({head} if steps[0][0] == "what-if" else set()))
    with ThreadPoolExecutor(max_workers=3) as pool:
        stamps = dict(zip(needed, pool.map(stamps_of, needed)))
    new: list[Row] = []
    for proof, b, h, carried in steps:
        if proof == "what-if":
            stale = {s for s in carried if stamps[head][s] != stamps[h][s]}
            for s in sorted(stale):
                print(f"{s}: the head's code is not the what-if's ({stamps[head][s][:12]}, not {stamps[h][s][:12]}): it bumps")
            carried = carried - stale
        new += carry_rows(proof, b, h, carried, stamps[b], stamps[h], table)
    written = write_rows(table, new)
    if new and commit:
        git("add", *written)
        git("-c", "commit.gpgsign=false", "commit", "-q", "-m", f"Carry the identity stamps over {base[:10]}..{head[:10]}: {len(new)} proven row(s)\n\n"
            f"Written by scripts/identity-stamp-carry auto ({', '.join(sorted({p for p, *_ in steps}))}); the pre-push "
            "stampcarry check proves each row again.")
        print(f"stamp carry: committed {git('rev-parse', 'HEAD').strip()[:12]}")
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
                proofs[key] = covered(*key, at=new)
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
    if len(argv) < 2 or argv[0] not in ("propose", "verify", "auto") or ".." not in argv[1]:
        print(__doc__, file=sys.stderr)
        return 2
    try:
        if argv[0] == "verify":
            return verify(argv[1])
        if argv[0] == "auto":
            return auto(argv[1], TABLE, commit="--no-commit" not in argv[2:])
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
