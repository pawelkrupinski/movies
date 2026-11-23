"""THE THREE PROOFS scripts/identity_impact_check.py makes (its header says what each proves): `mutations`, `hook` and
`replay`, each an Outcome -- its table's header, one row a mutation or commit, and the rows that are not sound.

  mutations(checkout, out, only)          every class that failed selected; a scoped spec passing outside its countries
  hook(checkout, out, only)               the pre-push hook's own selection, from a check tree, the same as this tool's
  replay(checkout, out, n, ref, start)    no test the selection left out changing outcome over history"""
from __future__ import annotations

import os
from dataclasses import dataclass, field
from typing import Callable, List, Optional, Set, Tuple

import identity_impact_check_mutations as mutation_list
import identity_impact_check_tree as tree_module
import tool_process
from identity_impact_check_tree import Checkout, Classes, flat, minus, restricted, say, within

Row = List[str]
SCOPED = {"services.identity.UnmatchedClustersRatchetSpec", "services.identity.NonFilmEventsFixtureSpec",
          "services.identity.IdentitySliceCasesSpec", "scripts.IdentityRefitMeasureAllSpec",
          "scripts.IdentityRefitCaptureReplaySpec"}
# the hook's identity paths (pre-push's is_identity_path), as git pathspecs; the worker's identity adapters:
# services.identityadapters, services/identity/adapters before 2026-11-18 (the replay walks both)
IDENTITY_PATHS = ["identity/", "worker/src/main/scala/services/identityadapters/", "worker/src/main/scala/services/identity/",
                  ":(glob)*/src/test/scala/services/identity/**",
                  "common/src/main/resources/title-rules.tsv", "common/src/main/resources/title-words.tsv",
                  "common/src/main/scala/services/titles/rules/", "common/src/main/scala/services/titles/TitleNormalizer.scala",
                  ":(glob)*/src/test/scala/services/titles/rules/**", "worker/src/test/scala/scripts/",
                  "worker/src/test/scala/tools/HardClusterRatchetSpec.scala", "worker/src/test/scala/tools/CountryScope.scala",
                  "worker/src/fixtures/", "testkit/src/main/scala/services/identity/",
                  "testkit/src/main/scala/services/movies/SingleCountryNormalizer.scala",
                  "testkit/src/main/scala/services/movies/NormalizerTrace.scala",
                  ":(glob)test/resources/fixtures/identity-*/**", ":(glob)test/resources/fixtures/identity-*",
                  "test/resources/fixtures/corpus/", "test/resources/fixtures/drift-slices/",
                  # what the identity specs read by class alone (pre-push's is_identity_reach_path, 2026-11-19)
                  "common/src/main/scala/", "common/src/main/java/", "testkit/src/main/scala/"]
NO_AREA = "the hook runs no identity area for this change"


@dataclass
class Outcome:
    header: Row
    rows: List[Row] = field(default_factory=lambda: [])
    unsound: List[str] = field(default_factory=lambda: [])

    def add(self, row: Row, unsound: bool) -> None:
        self.rows.append(row)
        if unsound:
            self.unsound.append(row[0])


def joined(names: Set[str]) -> str:
    return ",".join(sorted(names)) or "-"


def over_mutations(c: Checkout, only: Set[str], label: str, outcome: Outcome,
                   row_of: Callable[[str], Tuple[Row, bool]]) -> Outcome:
    """Each mutation `only` names (none: all) applied in the checkout at HEAD, its row made; the checkout left at HEAD."""
    head = c.head()
    for m in mutation_list.chosen(only):
        say(f"{label} {m.name}: {m.what}")
        c.prepare(head)
        m.apply(c.tree)
        outcome.add(*row_of(m.name))
    c.prepare(head)
    return outcome


# ── mutations ───────────────────────────────────────────────────────────────────────────────────────────────────────

def mutations(c: Checkout, out: str, only: Set[str]) -> Outcome:
    dirty = c.git_in(c.repo, "status", "--porcelain", "--", *tree_module.TOOLING).strip()
    if dirty:
        raise RuntimeError(f"commit the selection's tooling first: the mutations run on HEAD ({dirty})")
    head = c.head()
    header = ["mutation", "verdict", "selected", "of", "countries", "failed", "missed", "outOfScopeFailed"]
    # uncommitted: the selection reads the working tree against HEAD
    return over_mutations(c, only, "mutation", Outcome(header), lambda name: mutation_row(c, f"{out}/{name}", head))


def mutation_row(c: Checkout, prefix: str, head: str) -> Tuple[Row, bool]:
    s = c.select(head, None, prefix)
    area = c.area(s.every)
    selected = area if s.why else within(s.chosen, area)
    failed = c.run_classes(f"{prefix}.full.log", area)
    missed = failed - flat(selected)
    out_of_scope = out_of_scope_failed(c, prefix, selected, s.countries)
    verdict = "UNSOUND" if missed or out_of_scope else ("VACUOUS" if not failed else "SOUND")
    row = [os.path.basename(prefix), verdict, "ALL" if s.why else str(len(flat(selected))), str(len(flat(area))),
           ",".join(s.countries) or "-", joined(failed), joined(missed), out_of_scope or "-"]
    say(f"  {verdict}: selected {row[2]} of {row[3]} classes, countries {row[4]}; failed {row[5]}; "
        f"missed {row[6]}; out of scope failed {row[7]}")
    return row, verdict == "UNSOUND"


def out_of_scope_failed(c: Checkout, prefix: str, selected: Classes, countries: List[str]) -> str:
    """The selected country-scoped specs that fail over the countries the selection left out, said with them."""
    scoped = restricted(selected, SCOPED)
    rest = [cc for cc in tree_module.COUNTRIES if cc not in countries]
    failed: Set[str] = c.run_classes(f"{prefix}.outscope.log", scoped, rest) if scoped and rest else set()
    return f"{','.join(sorted(failed))} (over {','.join(rest)})" if failed else ""


# ── the hook's own selection, from a check tree ─────────────────────────────────────────────────────────────────────

def hook(c: Checkout, out: str, only: Set[str]) -> Outcome:
    header = ["mutation", "verdict", "hookClasses", "toolClasses", "hookSummary"]
    return over_mutations(c, only, "hook", Outcome(header), lambda name: hook_row(c, out, name))


def hook_row(c: Checkout, out: str, name: str) -> Tuple[Row, bool]:
    c.git("-c", "user.name=identity-impact-check", "-c", "user.email=identity-impact-check@localhost",
          "commit", "-qam", f"identity-impact-check: {name}")  # a detached commit: the hook's range, as a check's
    s = c.select("HEAD~1", "HEAD", f"{out}/{name}")
    mine = tree_module.plan_classes(tree_module.PLAN(c.set_lines(), s.why, s.chosen, s.countries,
                                                     every_test=s.every if s.why else None))
    summary, lines = hook_plan(c, f"{out}/{name}")
    theirs = tree_module.plan_classes(lines)
    verdict = "NO AREA" if summary is None else "NO SELECTION" if summary.startswith("no test-impact selection") \
        else ("AGREES" if theirs == mine else "DISAGREES")
    said = (NO_AREA if summary is None else summary)[:160]
    row = [name, verdict, str(len(flat(theirs))), str(len(flat(mine))), said]
    say(f"  {verdict}: the hook {row[2]} classes, this tool {row[3]}; {said}")
    return row, verdict != "AGREES"


def hook_plan(c: Checkout, prefix: str) -> Tuple[Optional[str], List[str]]:
    """(summary, sbt lines) of the hook's identity area at the tree's HEAD~1..HEAD (PREPUSH_IDENTITY_PLAN); no summary
    when the change is no identity path to the hook (is_identity_path): no area run."""
    log, plan_file, hook_tmp = f"{prefix}.hook.log", f"{prefix}.hook.plan", f"{prefix}.hook-tmp"
    os.makedirs(hook_tmp, exist_ok=True)
    env = {**c.env, "TMPDIR": hook_tmp, "PREPUSH_ONLY": "identity", "PREPUSH_IDENTITY_PLAN": plan_file,
           "PREPUSH_SKIP": "eslint lintspecs"}
    if os.path.exists(plan_file):
        os.remove(plan_file)
    rc = c.runner.run(tool_process.Command(
        ["bash", "scripts/hooks/pre-push", "--checks-only", "HEAD~1..HEAD"], env, log=log, cwd=c.tree,
        closed_stdin=True))
    if rc != 0:
        raise RuntimeError(f"the hook failed (exit {rc}): {log}")
    if not os.path.exists(plan_file):
        return None, []
    with open(plan_file, encoding="utf-8") as fh:
        lines = [line.rstrip("\n") for line in fh if line.strip()]
    return next((line[2:] for line in lines if line.startswith("# ")), ""), [l for l in lines if not l.startswith("#")]


# ── the replay ──────────────────────────────────────────────────────────────────────────────────────────────────────

def replay(c: Checkout, out: str, n: int, ref: str, start: int) -> Outcome:
    commits = c.git_in(c.repo, "log", "--format=%H", "--no-merges", f"-n{n}", ref, "--", *IDENTITY_PATHS).split()[::-1]
    outcome = Outcome(["commit", "verdict", "selected", "of", "countries", "leftOutFailed", "changedOutcome", "subject"])
    for i, sha in enumerate(commits):
        if i >= start:
            outcome.add(*replay_row(c, out, sha, f"[{i + 1}/{len(commits)}]"))
    return outcome


def at_commit(c: Checkout, sha: str) -> None:
    c.prepare(sha)
    c.lay_tooling()


def replay_row(c: Checkout, out: str, sha: str, counter: str) -> Tuple[Row, bool]:
    short = sha[:9]
    subject = c.git_in(c.repo, "log", "-1", "--format=%s", sha).strip()[:70]
    say(f"{counter} {short} {subject}")
    at_commit(c, sha)
    try:
        s = c.select(f"{sha}~1", sha, f"{out}/{short}")
    except RuntimeError as e:
        return [short, "NOT MADE", "-", "-", "-", "-", str(e)[:200], subject], False
    area = c.area(s.every)
    selected = area if s.why else within(s.chosen, area)
    left = minus(area, selected)
    failed: Set[str] = c.run_classes(f"{out}/{short}.left.log", left) if left else set()
    changed = changed_outcome(c, f"{out}/{short}", sha, left, failed)
    verdict = "UNSOUND" if changed else "SOUND"
    row = [short, verdict, "ALL" if s.why else str(len(flat(selected))), str(len(flat(area))),
           ",".join(s.countries) or "-", joined(failed), joined(changed), subject]
    say(f"  {verdict}: selected {row[2]} of {row[3]}, countries {row[4]}; left-out failed {row[5]}")
    return row, bool(changed)


def changed_outcome(c: Checkout, prefix: str, sha: str, left: Classes, failed: Set[str]) -> Set[str]:
    """The left-out tests that failed at the commit and pass at its parent."""
    if not failed:
        return set()
    say(f"  left-out tests failed at {sha[:9]}: {','.join(sorted(failed))} -- at its parent too?")
    at_commit(c, f"{sha}~1")
    return failed - c.run_classes(f"{prefix}.parent.log", restricted(left, failed))
