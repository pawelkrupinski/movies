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
     a what-if, none touches the drift harness (scripts/ci/commit_rules.py's HARNESS_FILES: a what-if never resolves
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

import glob
import os
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(HERE, "lib"), os.path.join(HERE, "ci")]  # its own modules, tool_python, commit_rules
from commit_rules import HARNESS_FILES  # noqa: E402  (the drift harness: one definition)
from identity_stamp_carry_published import (  # noqa: E402  (THE PUBLISHED VERDICT: the what-if's record)
    ASSET, COUNTRIES, asset_name, cached_verdict, country_refusals, diff_id, fetch_asset, identity_cache,
    moved_rows, publish, release, verdict_path, what_if_verdicts)
from pure_move_check import Undecidable, git  # noqa: E402  (the one git a proof reads, and its refusal)
from processes import machine  # noqa: E402
import tool_git  # noqa: E402
from processes import tool_python  # noqa: E402
from identity_stamp_carry_ranges import RANGES, cited_range, record_of, write_ranges  # noqa: E402  (THE QUEUE REWRITES)

TABLE = "test/resources/identity-stamp-carry.tsv"
STAMPS = ("identity-rules", "venue-slot", "agreement-rules")
# what a what-if cannot vouch for: the title rules (data a corpus covers thinly), a module's resources (the model's
# inputs), a store or codec (how a stored family is read back)
WHAT_IF_BLIND = re.compile(
    r"^(common/src/main/resources/title-(rules|words)\.tsv|common/src/main/scala/services/titles/rules/.*"
    r"|[a-z]+/src/main/resources/.*|[a-z]+/src/main/scala/.*(Store|Codec|Bson)[A-Za-z]*\.scala)$")


class Refused(Exception):
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
    maker = machine.environment().get("KINOWO_STAMP_CARRY_STAMPS") or os.path.join(HERE, "identity-stamp-carry")
    done = subprocess.run([maker, "stamps", sha], capture_output=True, text=True)
    if done.returncode != 0:
        raise Undecidable(f"the stamps of {sha[:12]} could not be made (exit {done.returncode}): {done.stderr.strip()[-600:]}")
    stamps = dict(line.split("\t", 1) for line in done.stdout.splitlines() if "\t" in line)
    missing = [s for s in STAMPS if not re.fullmatch(r"[0-9a-f]{64}", stamps.get(s, ""))]
    if missing:
        raise Undecidable(f"the stamps of {sha[:12]} lack {', '.join(missing)}: {done.stdout.strip()}")
    return stamps


def published_refusals(verdicts: dict[str, list[str]], head: str, base: str, patch_id: str) -> list[str]:
    """Why each country's verdict line is not the one the what-if published for head over base (THE PUBLISHED VERDICT)."""
    why = []
    for cc in COUNTRIES:
        line = verdicts[cc]
        name = line[6] if len(line) > 6 else ""
        if not ASSET.fullmatch(name):
            why.append(f"{cc}: its verdict names no published asset (recorded before the verdicts were published, or "
                       "written by hand): run scripts/identity-what-if again")
            continue
        body = fetch_asset(name, "\t".join(line))
        if body is None:
            why.append(f"{cc}: no asset {name} on release {release()} (never published, or gh could not fetch it)")
            continue
        fields = tuple(body.get(k, "") for k in ("head", "base", "patch_id", "countries", "tools", "run"))
        if asset_name(*fields) != name or fields[:3] != (head, base, patch_id):
            why.append(f"{cc}: the asset {name} is not of head {head[:12]} over base {base[:12]}, patch-id {patch_id[:12]}")
        elif "\t".join(line) not in body.get("lines", []):
            why.append(f"{cc}: the committed verdict is not the one published as {name}: {chr(9).join(line[1:6])!r}")
        elif line[4] != "-":
            diff = body.get("diffs", {}).get(cc, {})
            rows = moved_rows(diff.get("takes", "") + diff.get("final", ""))
            if str(rows) != line[4]:
                why.append(f"{cc}: the published takes diff holds {rows} row(s), not the {line[4]} the verdict says moved")
    return why


def design_refusals(base: str, head: str) -> list[str]:
    """Each commit of base..head saying `Identity-Design:`: a rule change by design, never carried."""
    designs = ((sha, git("log", "-1", "--format=%(trailers:key=Identity-Design,valueonly,unfold)", sha).strip())
               for sha in git("rev-list", f"{base}..{head}").split())
    return [f"{sha[:12]} says 'Identity-Design: {design}': a rule change by design is decided again, never carried"
            for sha, design in designs if design]


def pure_move_refusals(base: str, head: str) -> list[str]:
    """Why scripts/pure-move-check, run again, does not call base..head PURE; none when it does."""
    done = subprocess.run(tool_python.isolated(os.path.join(HERE, "pure_move_check.py"), f"{base}..{head}", "--quiet",
                                               python=sys.executable), capture_output=True, text=True)
    if done.returncode == 0:
        return []
    tail = (done.stdout + done.stderr).strip().splitlines()[-3:]
    return [f"scripts/pure-move-check does not call {base[:12]}..{head[:12]} PURE (exit {done.returncode}): {' / '.join(tail)}"]


def blind_refusals(base: str, head: str) -> list[str]:
    """What base..head changes that a what-if cannot vouch for: the drift harness, and WHAT_IF_BLIND."""
    # every path the range touches, both sides of a rename (a harness file moved away is still a harness change)
    changed = sorted({p for line in git("diff", "--name-status", "-M", base, head).splitlines() for p in line.split("\t")[1:]})
    why: list[str] = []
    harness = [p for p in changed if HARNESS_FILES.match(p)]
    if harness:
        why.append(f"the range changes the drift harness ({' '.join(harness)}): a what-if never carries it (a pure move does)")
    blind = [p for p in changed if WHAT_IF_BLIND.match(p)]
    if blind:
        why.append(f"a what-if sees today's corpus only, and the range changes what it cannot vouch for: {' '.join(blind)}")
    return why


def ran_on(verdicts: dict[str, list[str]], base: str, head: str, at: str | None) -> tuple[str, str, str, list[str]]:
    """The head, base and patch-id the what-if ran on -- these, or (a row written after a rebase) the committed record's
    patch-id line's -- and why that record is of another diff."""
    if at is None or not verdicts:
        return head, base, "", []
    recorded = verdicts.get("patch-id", ["", ""])[1]
    line = verdicts.get("patch-id", [])
    ran_head, ran_base = (line[2], line[3]) if len(line) >= 4 else (head, base)
    stale = [] if recorded == diff_id(base, head) else [
        f"the committed what-if verdict is of another diff (patch-id {recorded[:12] or 'none'}, not "
        f"{base[:12]}..{head[:12]}'s {diff_id(base, head)[:12]})"]
    return ran_head, ran_base, recorded, stale


def covered(proof: str, base: str, head: str, at: str | None = None,
            record: tuple[str, str] | None = None) -> tuple[set[str], list[str]]:
    """The stamps `proof` over base..head carries, and why it carries none, if it does not hold. A what-if's record is
    this machine's, or, `at` a commit, the one committed there (what_if_verdicts) under the range its row cites --
    `record`, (head, base), when the queue rewrote that range into base..head (THE QUEUE REWRITES)."""
    if base == head or not tool_git.Git.here().ok("merge-base", "--is-ancestor", base, head, quiet=True):
        return set(), [f"{base[:12]} is not an ancestor of {head[:12]}"]
    why = design_refusals(base, head)
    if proof == "pure-move":
        why += pure_move_refusals(base, head)
        return (set() if why else set(STAMPS)), why
    if proof != "what-if":
        return set(), [f"no proof {proof!r} (pure-move or what-if)"]
    why += blind_refusals(base, head)
    verdicts = what_if_verdicts(*record_of(record, base, head), at)
    ran_head, ran_base, patch_id, stale = ran_on(verdicts, base, head, at)
    countries, final = country_refusals(verdicts, base, head)
    why += stale + countries
    if not why:
        why = published_refusals(verdicts, ran_head, ran_base, patch_id or diff_id(base, head))
    if why:
        return set(), why
    return ({"identity-rules", "agreement-rules"} if final else {"identity-rules"}), []


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


def write_rows(table: str, new: list[Row], ran: dict[tuple[str, str], tuple[str, str]] | None = None) -> list[str]:
    """Appends the rows to `table`, and copies each what-if row's record beside it (VERDICTS), bound to the row's diff by
    its patch-id: the files to commit. `ran` maps a row's (base, head) to the (base, head) the what-if ran under, when a
    rebase moved them (auto)."""
    if not new:
        return []
    with open(table, "a") as f:
        f.writelines(r.line() + "\n" for r in new)
    written = [table, write_ranges(os.path.join(os.path.dirname(table), os.path.basename(RANGES)),
                                   {(r.base, r.head) for r in new})]
    for base, head in sorted({(r.base, r.head) for r in new if r.proof == "what-if"}):
        path = verdict_path(head, base)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        ran_base, ran_head = (ran or {}).get((base, head), (base, head))
        with open(path, "w") as f:
            f.write(cached_verdict(ran_head, ran_base))
            f.write(f"patch-id\t{diff_id(base, head)}\t{ran_head}\t{ran_base}\n")
        written.append(path)
    print(f"{len(new)} row(s) added to {table}: commit it with {', '.join(written[1:])}"
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
    """A recorded what-if (head0, base0) of the very change base..head -- the same diff by patch-id, whatever blobs and line
    numbers a main that moved elsewhere gave its context -- taken before a rebase moved its shas (agentctl land rebases
    after the lane ran it). Its proof is of base0..head0's code; a row of it is live at `head` only while head's digest is
    head0's, which `auto` checks."""
    wanted = diff_id(base, head)
    found = []
    for path in glob.glob(os.path.join(identity_cache(), "what-if-verdicts", "*-*.tsv")):
        head0, _, base0 = os.path.basename(path)[:-4].partition("-")
        if wanted and all(is_commit(c) for c in (head0, base0)) and diff_id(base0, head0) == wanted:
            found.append((os.path.getmtime(path), base0, head0))
    return max(found)[1:] if found else None


def is_commit(sha: str) -> bool:
    return bool(re.fullmatch(r"[0-9a-f]{40}", sha)) and \
        tool_git.Git.here().ok("cat-file", "-e", f"{sha}^{{commit}}", quiet=True)


def auto(rng: str, table: str, commit: bool) -> int:
    """agentctl land's step after its rebase (scripts/land-before-queue): carry what the pushed range proves, by itself.
    The whole range by a recorded whole what-if of its very diff, else commit by commit by pure-move-check. One commit
    that may move a stamp and proves nothing bumps the head's stamps whatever the rest prove -- the head is what deploys,
    and its stamp is its chain's end -- so then nothing is built and no row written. A what-if taken before the land's
    rebase is written for the PUSHED commits (the same diff, the stamps alike on both sides), its record bound to that
    diff's patch-id: `verify` refuses a row citing a commit the push does not hold, and a record of another diff."""
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
    needed = sorted({sha for _, b, h, _ in steps for sha in (b, h)} | ({base, head} if steps[0][0] == "what-if" else set()))
    with ThreadPoolExecutor(max_workers=3) as pool:
        stamps = dict(zip(needed, pool.map(stamps_of, needed)))
    new: list[Row] = []
    ran: dict[tuple[str, str], tuple[str, str]] = {}
    for proof, b, h, carried in steps:
        if proof == "what-if":
            # the what-if ran on the lane's commits (b..h), before the rebase; the rows cite the PUSHED ones (base..head,
            # the same diff: range_verdict), which every checker has -- for a stamp whose digests the rebase kept on both
            # sides, so the carry proven of b..h is the carry of base..head
            stale = {s for s in carried if stamps[head][s] != stamps[h][s] or stamps[base][s] != stamps[b][s]}
            for s in sorted(stale):
                print(f"{s}: the pushed code is not the what-if's (head {stamps[head][s][:12]} / {stamps[h][s][:12]}, base "
                      f"{stamps[base][s][:12]} / {stamps[b][s][:12]}): it bumps")
            ran[(base, head)] = (b, h)
            new += carry_rows(proof, base, head, carried - stale, stamps[base], stamps[head], table)
        else:
            new += carry_rows(proof, b, h, carried, stamps[b], stamps[h], table)
    written = write_rows(table, new, ran)
    if new and commit:
        git("add", *written)
        git("-c", "commit.gpgsign=false", "commit", "-q", "-m", f"Carry the identity stamps over {base[:10]}..{head[:10]}: {len(new)} proven row(s)\n\n"
            f"Written by scripts/identity-stamp-carry auto ({', '.join(sorted({p for p, *_ in steps}))}); the pre-push "
            "stampcarry check proves each row again.")
        print(f"stamp carry: committed {git('rev-parse', 'HEAD').strip()[:12]}")
    return 0


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


def main(argv: list[str]) -> int:
    if argv[:1] in (["asset-name"], ["publish"]) and len(argv) == 6:
        head, base, *rest = argv[1:]
        if argv[0] == "publish":
            return publish(head, base, *rest)
        print(asset_name(head, base, diff_id(base, head), *rest))
        return 0
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
