"""AN AREA'S LOCAL TESTS (scripts/hooks/pre-push's header: the `identity` area; scripts/hooks/local-tests.txt): its
`first` lines (each a fast check whose failure names everything at once: it fails the area there, nothing else run),
its `sbt` lines in ONE scripts/sbt-warm call (the checkout's kept server: warm, and the one compiler over its target/),
then its `sh` lines; a failure shows the failing specs, the full log kept. The identity area's lines are narrowed first
by TEST-IMPACT SELECTION (`test_impact`, also the sbt check's --verify-only narrowing), cut into the landing check's
shards (PREPUSH_IDENTITY_SHARD) and given the population walk's candidates and kept rows (`walk_rows`, shared with the
flipbacklog check). Run as a light check (pre_push_checks.Python); its lines are the shell hook's."""
from __future__ import annotations

import fnmatch
import re
from typing import List, Optional, Sequence, Tuple

import machine
import pre_push_modes as modes
import tool_python
from pre_push_light import Check, tail

LOCAL_TESTS = "scripts/hooks/local-tests.txt"
SBT_WARM = "scripts/sbt-warm"
SDKMAN_FIX = ('put sdkman\'s sbt and java on PATH: export PATH="$HOME/.sdkman/candidates/sbt/current/bin:'
              '$HOME/.sdkman/candidates/java/current/bin:$PATH" (or run: . "$HOME/.sdkman/bin/sdkman-init.sh")')
PASSED = re.compile(r"Tests: succeeded|All tests passed|[0-9]+ passed")
COUNTED = re.compile(r"^[a-z-]+: [0-9]+ ")
APPEND = 'exec "$@" >> "$0" 2>&1'
ERRORS_INTO = 'exec "$@" 2>> "$0"'
DIGEST = re.compile(r"[^0-9a-f]")


# ── shared by the sbt checks ──
def split_value(c: Check, name: str) -> str:
    """One of the run's own variables (THE JOB'S SPLIT: state, never exported), else the environment's."""
    return c.text(name) or c.repo.env.get(name, "")


def need_jvm(c: Check, check: str) -> bool:
    """sbt, and a java that RUNS (macOS's /usr/bin/java is a stub that only says no runtime is installed); sbt's
    launcher takes $JAVA_HOME's first."""
    if not c.need(check, "sbt", SDKMAN_FIX):
        return False
    home = c.repo.env.get("JAVA_HOME", "")
    runtime = f"{home}/bin/java" if home and c.holds("-x", f"{home}/bin/java") else "java"
    runs = c.bounded("30", [runtime, "-version"], machine.QUIET)[0] == 0  # a version asked, no JVM run: never AWT
    return runs or c.missing(check, "a java that runs ($JAVA_HOME or PATH)", SDKMAN_FIX) == 0


def nonempty(text: str) -> List[str]:
    return [line for line in text.split("\n") if line]


def answered(c: Check, mode: str, given: Sequence[str], args: Sequence[str] = ()) -> Tuple[int, str]:
    """A selection mode's (status, answer) -- scripts/pre_push.py's -- for `given` lines, at the checked tree, in this
    process, with the check's environment as it is now (PREPUSH_GITOPS once the manifests are fetched)."""
    checkout = modes.Checkout(c.repo.files, c.hook.rsplit("/", 1)[0], c.repo.env, c.selection_run)
    return modes.answer(checkout, mode, list(args), "".join(line + "\n" for line in given))


def selection(c: Check, mode: str, given: Sequence[str], args: Sequence[str] = ()) -> str:
    return answered(c, mode, given, args)[1]


def selected(c: Check, mode: str, given: Sequence[str]) -> int:
    return answered(c, mode, given)[0]


def summary(c: Check, log: str) -> None:
    """A failed run's summary (the failing tests, sbt's errors) and where its full log is."""
    c.say.raw(modes.failure_summary(c.repo.files.read_text_or(log, "")))
    c.say.warn(f"full log: {log}")


def local_test_lines(c: Check, name: str, kind: str) -> List[str]:
    """The commands of set `name` of `kind` (sbt|sh|first) in the CHECKED tree's local-tests.txt, one per line."""
    pattern = re.compile(rf"^{re.escape(name)}[ \t\r\f\v]+{re.escape(kind)}[ \t\r\f\v]+")
    found = (pattern.match(line) for line in c.repo.files.read_text_or(LOCAL_TESTS, "").split("\n"))
    return [m.string[m.end():] for m in found if m]


def fetched(c: Check, log: str) -> bool:
    """infra/bin/fetch-gitops into <cwd>/infra/kubernetes (the GitOps manifests, from GitHub), bounded by
    $PREPUSH_GITOPS_SECONDS (90), its output into `log`."""
    bounded = tool_python.isolated(f"{c.scripts()}/lib/tool_bounded.py", c.repo.env.get("PREPUSH_GITOPS_SECONDS", "90"),
                                   "infra/bin/fetch-gitops")
    return c.logged(bounded, log, {"KINOWO_GITOPS_DEST": f"{c.repo.cwd}/infra/kubernetes"}) == 0


def last_line(c: Check, log: str) -> str:
    return (c.log_text(log) or [""])[-1]


def appended(log: str, argv: Sequence[str]) -> List[str]:
    """`argv >> log 2>&1`."""
    return ["bash", "-c", APPEND, log, *argv]


def errors_into(log: str, argv: Sequence[str]) -> List[str]:
    """`argv 2>> log`: its stdout the caller's."""
    return ["bash", "-c", ERRORS_INTO, log, *argv]


def merge_base(c: Check) -> Optional[str]:
    code, out = c.repo.git("merge-base", c.text("base_sha"), c.text("head_sha"))
    return out.strip() if code == 0 else None


# ── TEST-IMPACT SELECTION (scripts/ci/identity_impact.py, project/TestImpact.scala) ──
def test_impact(c: Check, name: str) -> Tuple[bool, List[str]]:
    """The range's changed paths to <tmp>/<name>.changed and the classes they reach (identityImpact's `<module>
    <class>` lines, or `ALL <why>`) to <tmp>/<name>.impact, through the checkout's kept sbt server; (whether it
    answered, what it says why not). Every path the range changed counts, deletions too; one range only."""
    base, head = c.text("base_sha"), c.text("head_sha")
    if len(c.ranges()) != 1:
        return False, [f"the push holds {len(c.ranges())} ranges"]
    found = merge_base(c)
    if found is None:
        return False, [f"no merge base of {base[:9]} and {head[:9]}"]
    code, changed = c.repo.git("diff", "--name-only", "--no-renames", found, head)
    if code != 0:
        return False, []
    c.repo.files.replace_text(c.tmp(f"{name}.changed"), changed)
    log, impact = c.tmp(f"{name}.impact.log"), c.tmp(f"{name}.impact")
    if c.logged([SBT_WARM, f"identityImpact {c.tmp(f'{name}.changed')} {impact}"], log) != 0:
        return False, tail(c.log_text(log), 20) + ["sbt identityImpact failed"]
    # an exit 0 that wrote nothing (a kept server answering for a command it never ran) is no answer, said as one
    if not c.repo.files.is_file(impact):
        return False, tail(c.log_text(log), 20) + ["sbt identityImpact wrote no answer"]
    return True, []


def impact_all(c: Check, name: str) -> str:
    """The impact's first `ALL <why>` line, "" for none."""
    return next((line for line in c.log_text(c.tmp(f"{name}.impact")) if line.startswith("ALL")), "")


def said_into(c: Check, log: str, lines: Sequence[str]) -> None:
    c.repo.files.replace_text(log, c.repo.files.read_text_or(log, "") + "".join(line + "\n" for line in lines))


def select_identity_tests(c: Check, cmds: Sequence[str]) -> Optional[List[str]]:
    """Of the identity set's sbt lines, only the specs the pushed change can reach, each once, the country-scoped ones
    over the countries it can reach (<tmp>/identity.plan, the summary in <tmp>/identity.summary); None when there is
    no selection (every test runs), why in <tmp>/identity.select.log."""
    log = c.tmp("identity.select.log")
    c.repo.files.replace_text(log, "")
    answered, why = test_impact(c, "identity")
    found = merge_base(c) if answered else None
    if found is None:
        said_into(c, log, why)
        return None
    every = c.tmp("identity.impact.tests")
    every_test = ["--every-test", every] if c.repo.files.read_text_or(every, "") else []
    code, countries = c.repo.output(errors_into(log, ["python3", "scripts/ci/identity_impact.py", "countries", found,
                                                      c.text("head_sha")]), c.repo.files.read_text(c.tmp("identity.changed")))
    if code != 0:
        said_into(c, log, ["the countries could not be read"])
        return None
    countries = countries.rstrip("\n")
    code, plan = c.repo.output(errors_into(log, ["python3", "scripts/ci/identity_impact.py", "plan", "--impact",
                                                 c.tmp("identity.impact"), "--countries", countries, *every_test]),
                               "".join(line + "\n" for line in cmds))
    c.repo.files.replace_text(c.tmp("identity.plan"), plan)
    if code != 0:
        said_into(c, log, ["no plan"])
        return None
    c.repo.files.replace_text(c.tmp("identity.summary"), identity_summary(c, countries))
    return nonempty(plan)


def identity_summary(c: Check, countries: str) -> str:
    every = impact_all(c, "identity")
    reached = (f"every identity spec: {every[4:]}" if every else
               f"the specs the change reaches ({len(nonempty(c.repo.files.read_text(c.tmp('identity.impact'))))} test "
               "classes in every module)")
    return f"{reached}\ncountries in scope: {countries or 'none'}\n"


# ── THE WALK'S ROWS KEPT (PopulationMonotonicitySpec's CacheKey, sbt's identityWalkDigest) ──
def walk_rows(c: Check, check: str) -> str:
    """<the rows root>/<identityWalkDigest>, made and fresh; "" when sbt gives no digest (every slice walks). Asked
    once a run (the answer kept in tmp): the identity area and the flipbacklog check share it."""
    env = c.repo.env
    cache = env.get("KINOWO_IDENTITY_CACHE") or f"{env.get('HOME', '')}/.cache/kinowo-identity"
    root = env.get("KINOWO_POPULATION_WALK_ROWS") or f"{cache}/population-walk-rows"
    answer, log = c.tmp("identity.walk-digest"), c.tmp("identity.walk-digest.log")
    if not c.repo.files.is_file(c.tmp("walk-digest.asked")):
        if c.logged([SBT_WARM, f"identityWalkDigest {answer}"], log) != 0:
            c.repo.files.remove(answer)
        c.repo.files.replace_text(c.tmp("walk-digest.asked"), "")
    digest = DIGEST.sub("", c.repo.files.read_text_or(answer, ""))
    if len(digest) != 64:
        c.say.log(f"{check}: no population walk digest ({last_line(c, log)}): every slice walks", to_err=True)
        return ""
    kept = f"{root}/{digest}"
    c.quietly(["mkdir", "-p", kept])
    c.quietly(["touch", kept])
    c.quietly(["find", root, "-mindepth", "1", "-maxdepth", "1", "-type", "d", "-mtime", "+5", "-exec", "rm", "-rf",
               "{}", "+"])
    slices = sum(1 for p in c.repo.files.under(kept) if p.endswith(".tsv"))
    c.say.log(f"{check}: the population walk's rows kept under digest {digest[:12]} ({slices} slice(s) kept)",
              to_err=True)
    return kept


# ── the area ──
def area_selection(c: Check, name: str, cmds: List[str]) -> Optional[List[str]]:
    """The identity area's lines after its test-impact selection (said), or with PREPUSH_IDENTITY_PLAN the plan alone
    written (None: the area is done)."""
    selected = select_identity_tests(c, cmds)
    if selected is not None:
        said = c.repo.files.read_text(c.tmp("identity.summary")).replace("\n", ";")
        said = re.sub(";$", "", said).replace(";", "; ", 1)
        c.say.log(f"{name}: {said}")
        cmds = selected
    else:
        said = f"no test-impact selection ({last_line(c, c.tmp('identity.select.log'))}) -- every identity test"
        c.say.warn(f"{name}: {said}")
    plan_only = split_value(c, "PREPUSH_IDENTITY_PLAN")
    if plan_only:
        c.repo.files.replace_text(plan_only, f"# {said}\n" + "".join(line + "\n" for line in cmds))
        c.say.log(f"{name}: the plan alone, to {plan_only} (PREPUSH_IDENTITY_PLAN): nothing run")
        return None
    if not cmds:
        c.say.log(f"{name}: the change reaches no identity spec")
        return None
    return cmds


def first_lines(c: Check, name: str) -> bool:
    """The area's first lines, once, never sharded (shard 1 runs them); False when one failed (said)."""
    log = c.tmp(f"{name}.first.log")
    for line in [line for line in local_test_lines(c, name, "first") if line]:
        if c.logged(["bash", "-c", line], log) != 0:
            c.say.raw(c.repo.files.read_text_or(log, ""))
            c.say.warn(f"{name}: `{line}` failed (full log: {log})")
            return False
        c.printed([line for line in c.log_text(log) if COUNTED.search(line)][:1])
    return True


def first_shard(shard: str) -> bool:
    return shard.split("/", 1)[0] == "1"


def sharded(c: Check, cmds: List[str], shard: str) -> Optional[List[str]]:
    """THE IDENTITY SHARDS (precommit-check.yml's `check (identity k/n)` jobs): shard k of the selected lines
    (scripts/ci/shard_tests.py: every class in exactly one shard, balanced on scripts/ci/shard-weights-identity.tsv);
    None when it cannot be cut (said)."""
    code, out = c.repo.output(["python3", "scripts/ci/shard_tests.py", "--shard", shard, "--weights", "identity"],
                              "".join(line + "\n" for line in cmds))
    c.repo.files.replace_text(c.tmp("identity.shard"), out)
    if code != 0:
        c.say.warn(f"identity: shard {shard} could not be cut")
        return None
    c.say.log(f"identity: shard {shard}, {len(out.split())} words of {len(cmds)} line(s)")
    return nonempty(out)


def with_candidates(cmds: Sequence[str], extra: str) -> List[str]:
    """THE POPULATION WALK AGAINST THE BASE: every testOnly line names the candidates directory (and the kept rows)."""
    return [f"{cmd} {extra}" if fnmatch.fnmatchcase(cmd, "*testOnly * -- *")
            else f"{cmd} -- {extra}" if fnmatch.fnmatchcase(cmd, "*testOnly *") else cmd for cmd in cmds]


def walk_against_base(c: Check, popwalk: str, log: str) -> bool:
    """A slice whose flips differ from the known rows judged against the change's merge base (a flip main has too
    passes, one the change adds refuses, naming the listing, slice and chain); True when nothing was refused."""
    base = merge_base(c) or c.text("base_sha")
    passed = c.run(appended(log, ["scripts/ci/population-walk-diff", popwalk, base])) == 0
    c.shown(tail([line for line in c.log_text(log) if line.startswith("population walk: ")], 40))
    return passed


def sh_lines(c: Check, name: str, log: str) -> bool:
    return all(c.run(appended(log, ["bash", "-c", line])) == 0 for line in local_test_lines(c, name, "sh") if line)


def run_area(c: Check, name: str) -> int:
    """An area's local tests (the module's header)."""
    if not need_jvm(c, name):
        return 1
    cmds = area_lines(c, name)
    if cmds is None:
        return 0
    shard = split_value(c, "PREPUSH_IDENTITY_SHARD") if name == "identity" else ""
    if (not shard or first_shard(shard)) and not first_lines(c, name):
        return 1
    if shard and cmds:
        return run_shard(c, name, shard, cmds)
    return run_lines(c, name, cmds, True)


def area_lines(c: Check, name: str) -> Optional[List[str]]:
    """The area's sbt lines, the identity area's after its selection; None when the area is done."""
    cmds = [line for line in local_test_lines(c, name, "sbt") if line]
    return area_selection(c, name, cmds) if name == "identity" and cmds else cmds


def run_shard(c: Check, name: str, shard: str, cmds: List[str]) -> int:
    """Shard k of the lines; the area's sh lines are no classes to split: shard 1 runs them."""
    cut = sharded(c, cmds, shard)
    if cut is None:
        return 1
    if not cut and not first_shard(shard):
        c.say.log(f"identity: nothing in shard {shard}")
        return 0
    return run_lines(c, name, cut, first_shard(shard))


def candidate_lines(c: Check, cmds: List[str]) -> Tuple[List[str], str]:
    """The identity lines naming the walk's candidates and kept rows, and the candidates' directory."""
    popwalk = c.tmp("population-walk")
    kept = walk_rows(c, "identity")
    return with_candidates(cmds, f"-Didentity.population.candidates={popwalk}"
                           + (f" -Didentity.population.cache={kept}" if kept else "")), popwalk


def passed_lines(c: Check, log: str) -> int:
    shown = tail([line for line in c.log_text(log) if PASSED.search(line)], 3)
    c.printed(shown)
    return 0 if shown else 1  # the shell hook's `grep ... | tail -3` under pipefail: no such line fails the area


def run_lines(c: Check, name: str, cmds: List[str], run_sh: bool) -> int:
    log, popwalk = c.tmp(f"{name}.log"), ""
    if name == "identity" and cmds:
        cmds, popwalk = candidate_lines(c, cmds)
    passed = not cmds or c.logged([SBT_WARM, *cmds], log) == 0
    # judged even when another spec failed: the refusal names its listings beside the other failures
    if popwalk and c.repo.files.is_dir(popwalk):
        passed = walk_against_base(c, popwalk, log) and passed
    if passed and run_sh:
        passed = sh_lines(c, name, log)
    if not passed:
        summary(c, log)
        return 1
    return passed_lines(c, log)


def identity(c: Check) -> int:
    return run_area(c, "identity")
