"""THE ROWS A PROVEN RANGE ADDS (scripts/identity_stamp_carry.py): `carry_rows` and `write_rows` (a row, and its what-if's
record copied beside it), and `auto`, agentctl land's step after its rebase, which proves what the pushed range carries by
itself.

Imported by identity_stamp_carry.py, which has put scripts/, scripts/lib and scripts/ci on sys.path."""
from __future__ import annotations

import os
import glob
import re
from concurrent.futures import ThreadPoolExecutor
from identity_stamp_carry_published import cached_verdict, diff_id, identity_cache, verdict_path  # noqa: E402
from identity_stamp_carry_proof import STAMPS, Row, covered, rows, sha_of, stamps_of  # noqa: E402
from identity_stamp_carry_ranges import RANGES, write_ranges  # noqa: E402  (THE QUEUE REWRITES)
from pure_move_check import git  # noqa: E402
import tool_git  # noqa: E402


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


def digested_changes(sha: str) -> list[list[str]]:
    """The name-status rows of `sha` that touch a path a stamp's digest reads."""
    changes = [line.split("\t") for line in git("diff-tree", "--no-commit-id", "--name-status", "-M", "-r", "--root", sha).splitlines()]
    return [c for c in changes if any(MAY_MOVE.match(p) for p in c[1:])]


def code_changed(sha: str, paths: list[str]) -> bool:
    """Whether `sha` changes a line of code (not only a comment or whitespace) in `paths`."""
    diff = git("diff", "-w", f"{sha}^", sha, "--", *paths)
    return any(line[:1] in "+-" and not line.startswith(("+++", "---")) and not COMMENT_ONLY.match(line)
               for line in diff.splitlines())


def may_move(sha: str) -> bool:
    changes = digested_changes(sha)
    if any(c[0] != "M" for c in changes):
        return True  # a file added, removed or moved: the digest keys each file by its path
    return bool(changes) and code_changed(sha, [c[1] for c in changes])


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


def what_if_steps(base: str, head: str) -> list[tuple[str, str, str, set[str]]]:
    """The one step a recorded what-if of the very diff proves, or none (saying why it proves nothing)."""
    found = range_verdict(base, head)
    if not found:
        return []
    carried, why = covered("what-if", *found)
    if carried:
        return [("what-if", found[0], found[1], carried)]
    print(f"stamp carry: the recorded what-if {found[0][:12]}..{found[1][:12]} of this change proves nothing: {'; '.join(why)}")
    return []


def pure_move_steps(moving: list[str]) -> list[tuple[str, str, str, set[str]]] | None:
    """A step per commit that may move a stamp, each a pure move; None when one proves nothing."""
    steps: list[tuple[str, str, str, set[str]]] = []
    for c in moving:
        parent = git("rev-parse", f"{c}^").strip()
        carried, _ = covered("pure-move", parent, c)
        if not carried:
            print(f"stamp carry: {c[:12]} {git('log', '-1', '--format=%s', c).strip()[:70]!r} may move a stamp and proves "
                  "nothing (no pure move, no whole what-if of the range): the push bumps its stamps")
            return None
        steps.append(("pure-move", parent, c, carried))
    return steps


def stamps_for(steps: list[tuple[str, str, str, set[str]]], base: str, head: str) -> dict[str, dict[str, str]]:
    """Each stamp set the steps (and, for a what-if, the pushed base and head) read, made three at a time."""
    needed = sorted({sha for _, b, h, _ in steps for sha in (b, h)} | ({base, head} if steps[0][0] == "what-if" else set()))
    with ThreadPoolExecutor(max_workers=3) as pool:
        return dict(zip(needed, pool.map(stamps_of, needed)))


def what_if_rows(base: str, head: str, ran: tuple[str, str], carried: set[str], stamps: dict[str, dict[str, str]],
                 table: str) -> list[Row]:
    """The rows a what-if taken on the lane's commits `ran` (b, h) proves for the PUSHED ones (base..head, the same diff:
    range_verdict), which every checker has -- for a stamp whose digests the rebase kept on both sides, so the carry proven
    of b..h is the carry of base..head."""
    b, h = ran
    stale = {s for s in carried if stamps[head][s] != stamps[h][s] or stamps[base][s] != stamps[b][s]}
    for s in sorted(stale):
        print(f"{s}: the pushed code is not the what-if's (head {stamps[head][s][:12]} / {stamps[h][s][:12]}, base "
              f"{stamps[base][s][:12]} / {stamps[b][s][:12]}): it bumps")
    return carry_rows("what-if", base, head, carried - stale, stamps[base], stamps[head], table)


def carried_rows(steps: list[tuple[str, str, str, set[str]]], base: str, head: str, stamps: dict[str, dict[str, str]],
                 table: str) -> tuple[list[Row], dict[tuple[str, str], tuple[str, str]]]:
    """The rows every step adds, and where the what-if ran when a rebase moved it."""
    new: list[Row] = []
    ran: dict[tuple[str, str], tuple[str, str]] = {}
    for proof, b, h, carried in steps:
        if proof == "what-if":
            ran[(base, head)] = (b, h)
            new += what_if_rows(base, head, (b, h), carried, stamps, table)
        else:
            new += carry_rows(proof, b, h, carried, stamps[b], stamps[h], table)
    return new, ran


def commit_rows(base: str, head: str, new: list[Row], steps: list[tuple[str, str, str, set[str]]], written: list[str]) -> None:
    git("add", *written)
    git("-c", "commit.gpgsign=false", "commit", "-q", "-m", f"Carry the identity stamps over {base[:10]}..{head[:10]}: {len(new)} proven row(s)\n\n"
        f"Written by scripts/identity-stamp-carry auto ({', '.join(sorted({p for p, *_ in steps}))}); the pre-push "
        "stampcarry check proves each row again.")
    print(f"stamp carry: committed {git('rev-parse', 'HEAD').strip()[:12]}")


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
    steps = what_if_steps(base, head) or pure_move_steps(moving)
    if not steps:
        return 0
    new, ran = carried_rows(steps, base, head, stamps_for(steps, base, head), table)
    written = write_rows(table, new, ran)
    if new and commit:
        commit_rows(base, head, new, steps, written)
    return 0
