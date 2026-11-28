"""WHAT A PROOF CARRIES (scripts/identity_stamp_carry.py, its header: the digests, the proof, what a range changes that a
proof cannot see): the table's rows, the stamps the build makes of a commit, and `covered`, the stamps `pure-move` or
`what-if` carries over base..head and the reasons it carries none.

Imported by identity_stamp_carry.py, which has put scripts/, scripts/lib and scripts/ci on sys.path."""
from __future__ import annotations

import os
import sys
import re
import subprocess
from dataclasses import dataclass
from typing import Any
from drift_guard_rules import HARNESS_FILES  # noqa: E402  (the drift harness: one definition)
from identity_stamp_carry_published import (  # noqa: E402  (THE PUBLISHED VERDICT: the what-if's record)
    ASSET, COUNTRIES, asset_name, country_refusals, diff_id, fetch_asset, moved_rows, release, what_if_verdicts)
from pure_move_check import Undecidable, git  # noqa: E402  (the one git a proof reads, and its refusal)
from processes import machine  # noqa: E402
import tool_git  # noqa: E402
from processes import tool_python  # noqa: E402
from identity_stamp_carry_ranges import record_of  # noqa: E402  (THE QUEUE REWRITES)

HERE = os.path.dirname(os.path.abspath(__file__))
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
    reasons = (country_refusal(cc, verdicts[cc], head, base, patch_id) for cc in COUNTRIES)
    return [why for why in reasons if why]


def country_refusal(cc: str, line: list[str], head: str, base: str, patch_id: str) -> str | None:
    """Why `cc`'s verdict line is not the one the what-if published for head over base, or None when it is."""
    name = line[6] if len(line) > 6 else ""
    if not ASSET.fullmatch(name):
        return (f"{cc}: its verdict names no published asset (recorded before the verdicts were published, or "
                "written by hand): run scripts/identity-what-if again")
    body = fetch_asset(name, "\t".join(line))
    if body is None:
        return f"{cc}: no asset {name} on release {release()} (never published, or gh could not fetch it)"
    fields = tuple(body.get(k, "") for k in ("head", "base", "patch_id", "countries", "tools", "run"))
    if asset_name(*fields) != name or fields[:3] != (head, base, patch_id):
        return f"{cc}: the asset {name} is not of head {head[:12]} over base {base[:12]}, patch-id {patch_id[:12]}"
    if "\t".join(line) not in body.get("lines", []):
        return f"{cc}: the committed verdict is not the one published as {name}: {chr(9).join(line[1:6])!r}"
    return moved_refusal(cc, line, body)


def moved_refusal(cc: str, line: list[str], body: dict[str, Any]) -> str | None:
    """Why the published takes diff does not hold the rows the verdict line says moved, or None."""
    if line[4] == "-":
        return None
    diff = body.get("diffs", {}).get(cc, {})
    rows = moved_rows(diff.get("takes", "") + diff.get("final", ""))
    return None if str(rows) == line[4] else f"{cc}: the published takes diff holds {rows} row(s), not the {line[4]} the verdict says moved"


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
