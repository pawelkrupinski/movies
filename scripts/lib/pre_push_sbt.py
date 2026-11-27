"""THE SBT CHECK (scripts/hooks/pre-push's header: `sbt`): a new repo-reading spec needs its lint-registry row, and
its row selects it for every path it reads; then ONE sbt run -- the test configs the push compiles, the lint specs its
paths select (the GitOps-reading ones once infra/bin/fetch-gitops has put the manifests here: the fetch failing
BLOCKS), local-tests.txt's `basic` set (`fast` for --fast-only), --verify-only's unit tests narrowed to what the
combined change reaches, the coupling report and the quality report -- through the checkout's kept server
(scripts/sbt-warm) when there is one, else `sbt -batch`; then the coupling and quality ratchets and the design rules
over their rows. Run as a light check (pre_push_checks.Processes); its lines are the shell hook's."""
from __future__ import annotations

import re
from typing import List, Optional, Tuple

from pre_push_area import (fetched, impact_all, last_line, local_test_lines, need_jvm, nonempty, selected, selection, summary,
                           test_impact)
from pre_push_light import Check, PYTHON_FIX, STDERR, tail

SPEC_OR_REGISTRY = re.compile(r"(/src/test/scala/.*Spec\.scala|^scripts/hooks/lint-registry\.tsv)$")
EVERY_ROW = re.compile(r"^scripts/(hooks/lint-registry\.tsv|hooks/pre-push|ci/lint_registry_reads\.py)$")
SPEC = re.compile(r"^([^/]+)/src/test/scala/(.*Spec)\.scala$")
LINT_MODULES = ("http", "common", "scrapers", "repochecks", "worker", "web")
COUPLING_FILES = ("coupling.rows", "coupling.edges", "coupling.fanouts", "coupling.collaborators")


def unregistered(c: Check) -> bool:
    """A NEW REPO-READING SPEC NEEDS ITS LINT-REGISTRY ROW (Tool self-tests #158, #166); True when one has none (said)
    or the lookup failed."""
    code, out = c.answer([f"{c.hook.rsplit('/', 1)[0]}/lint-registry-missing"])
    if code != 0:
        return True
    for spec in out.split():
        c.say.warn(f"{spec} has no lint-registry row: add one to scripts/hooks/lint-registry.tsv (its header has the "
                   "format)")
    return bool(out.split())


def unselected_reads(c: Check) -> bool:
    """...AND ITS ROW SELECTS IT FOR EVERY PATH IT READS (Tool self-tests #194): over every row on a push of the
    registry or its readers (~8 s), over the pushed specs' rows else; True when a read is unselected (said) or the
    check failed."""
    changed = c.changed()
    if not any(SPEC_OR_REGISTRY.search(p) for p in changed) or not c.holds("-f", "scripts/ci/lint_registry_reads.py"):
        return False
    only = [] if any(EVERY_ROW.search(p) for p in changed) else \
        ["--only", *(f"{m.group(1)}:{m.group(2)}".replace("/", ".") for m in map(SPEC.match, changed) if m)]
    code, misses = c.answer(["python3", "scripts/ci/lint_registry_reads.py", *only])
    if code != 0:
        return True
    for miss in nonempty(misses.rstrip("\n")):
        spec, _, line = miss.partition("\t")
        c.say.warn(f"{spec} reads {line}, which its lint-registry row does not select: add the path's glob to its row")
    return bool(misses.strip("\n"))


def fetch_gitops(c: Check) -> bool:
    """The specs reading the GitOps manifests join the lints once infra/bin/fetch-gitops has put them in this checkout
    (PREPUSH_GITOPS=1 from here on); False when the fetch failed: the manifests are STALE and the push BLOCKED (said),
    the specs needing them left out, the rest still run for their verdicts."""
    if selected(c, "--reads-gitops", c.changed()) != 0 or skipped(c, "lintspecs") \
            or not c.holds("-x", "infra/bin/fetch-gitops"):
        return True
    if fetched(c, c.tmp("fetch-gitops.log")):
        c.repo.env["PREPUSH_GITOPS"] = "1"
        return True
    c.shown(tail(["    " + line for line in c.log_text(c.tmp("fetch-gitops.log"))], 8))
    c.say.warn("GitOps manifests STALE: infra/bin/fetch-gitops could not fetch movies-gitops main, so the push is "
               "BLOCKED. Fix: the network to github.com, then push again (or PREPUSH_SKIP=lintspecs, named in your "
               "report).")
    return False


def lint_lines(lints: List[str]) -> List[str]:
    """A module none of whose rows the push selects runs none: a bare `testOnly` would run its every test."""
    out: List[str] = []
    for module in LINT_MODULES:
        named = [line[len(module) + 1:] for line in lints if line.startswith(f"{module}:")]
        if named:
            out.append(f"{module}/Test/testOnly {' '.join(named)}")
    return out


def verify_units(c: Check, lints: List[str]) -> List[str]:
    """THE COMBINED TREE'S UNIT TESTS, NARROWED TO WHAT THE CHANGE REACHES (TEST-IMPACT SELECTION, as the identity
    area's): through the kept server only; a cold `sbt -batch` check, or no answer, runs every touched module's unit
    tests. The selection adds, in ANY module, the tests a test-side reader of a changed file reaches, and leaves out
    the lint specs this run already runs. No selection naming the change's readers (ALL, cold, no answer): every
    module's source-walking lints join."""
    plan = nonempty(selection(c, "--unit-plan", c.changed()))
    c.repo.files.replace_text(c.tmp("unit.universe"), selection(c, "--unit-universe", []))
    c.repo.files.replace_text(c.tmp("unit.lints"), "".join(line + "\n" for line in lints) or "\n")
    widen = bool(plan)
    if plan and c.text("sbt_client") == "1":
        narrowed = narrowed_units(c, plan)
        if narrowed is not None:
            plan, widen = narrowed, bool(impact_all(c, "unit"))
    if widen:
        joined = nonempty(selection(c, "--source-lints", plan, ["\n".join(lints)]))
        c.say.log(f"verify: no selection names the change's readers -- every module's source lints join "
                  f"({len(joined)} sbt line(s))", to_err=True)
        plan += joined
    return nonempty(selection(c, "--without-unfetched-gitops", plan))


def narrowed_units(c: Check, plan: List[str]) -> Optional[List[str]]:
    """The unit plan narrowed to the classes the combined change reaches (said); None when there is no selection."""
    answered, why = test_impact(c, "unit")
    c.repo.files.replace_text(c.tmp("unit.select.log"), "".join(line + "\n" for line in why))
    code, out = c.repo.output(["python3", "scripts/ci/identity_impact.py", "plan", "--impact", c.tmp("unit.impact"),
                               "--reads", c.tmp("unit.universe"), "--except", c.tmp("unit.lints")],
                              "".join(line + "\n" for line in plan)) if answered else (1, "")
    if code != 0:
        c.say.warn(f"verify: no test-impact selection ({last_line(c, c.tmp('unit.select.log'))}) -- every touched "
                   "module's unit tests")
        return None
    every = impact_all(c, "unit")
    c.say.log(f"verify: the unit tests the combined change reaches: {len(nonempty(out))} sbt line(s)"
              f"{', every one:' + every[3:] if every else ''}", to_err=True)
    return nonempty(out)


def skipped(c: Check, check: str) -> bool:
    return check in c.repo.env.get("PREPUSH_SKIP", "").split()


def ratchet_rows(c: Check, check: str, paths_mode: str, kind: str, path: str) -> bool:
    """Whether the push's paths select a ratchet's report (`paths_mode`) and the tree has it (`test <kind> <path>`);
    SKIPPED said."""
    if not nonempty(selection(c, paths_mode, c.changed())) or not c.holds(kind, path):
        return False
    if skipped(c, check):
        c.say.warn(f"{check} SKIPPED (PREPUSH_SKIP)")
        return False
    return True


def report_lines(c: Check) -> Tuple[List[str], bool, bool]:
    """The coupling and quality reports' sbt commands, and whether each runs (its old rows removed)."""
    lines: List[str] = []
    coupling = ratchet_rows(c, "coupling", "--coupling-plan", "-f", "coupling.sbt")
    if coupling:
        for name in COUPLING_FILES:
            c.repo.files.remove(c.tmp(name))
        lines.append(f"couplingReport {c.tmp('coupling.rows')} {c.tmp('coupling.md')} {c.tmp('coupling.edges')} "
                     f"{c.tmp('coupling.fanouts')} {c.tmp('coupling.collaborators')}")
    quality = ratchet_rows(c, "quality", "--quality-plan", "-d", "quality")
    if quality:
        c.repo.files.remove(c.tmp("quality.rows"))
        lines.append(f"quality/runMain quality.QualityReport --root {c.repo.cwd} --rows {c.tmp('quality.rows')}")
    return lines, coupling, quality


def sbt_lines(c: Check) -> List[str]:
    """The one sbt run's compiles, lint specs, basic (or fast) set and --verify-only's unit tests."""
    compiles = selection(c, "--compile-plan", c.changed()).split()
    lints = nonempty(selection(c, "--lint-plan", c.changed()))
    tests = lint_lines(lints)
    named = [line for line in local_test_lines(c, "fast" if c.text("fast_only") else "basic", "sbt") if line]
    tests += nonempty(selection(c, "--without-unfetched-gitops", named))
    return compiles + tests + (verify_units(c, lints) if c.text("verify_only") else [])


def ran_sbt(c: Check, lines: List[str]) -> bool:
    """The one run, through scripts/sbt-warm when the checkout keeps a server (a bare `sbt --client` held no busy
    lock, so the server's keeper could stop it mid-check, 2026-10-19), else `sbt -batch`; its summary when it failed."""
    run = ([f"{c.text('root')}/scripts/sbt-warm", "; ".join(lines)] if c.text("sbt_client") == "1"
           else ["sbt", "-batch", "-Dsbt.server.forcestart=false", *lines])
    if c.logged(run, c.tmp("sbt.log")) == 0:
        return True
    summary(c, c.tmp("sbt.log"))
    return False


def ratchet(c: Check, what: str, rows: str, argv: List[str]) -> bool:
    """A ratchet over the rows sbt wrote (said when it wrote none), its lines on stderr."""
    if not c.need("sbt", "python3", PYTHON_FIX):
        return False
    if not c.repo.files.read_text_or(rows, ""):
        c.say.warn(f"{what} wrote no rows (full log: {c.tmp('sbt.log')})")
        return False
    return c.run([*argv, *c.range_args()], out=STDERR) == 0


def coupling_ratchet(c: Check) -> bool:
    return ratchet(c, "coupling: sbt couplingReport", c.tmp("coupling.rows"), [
        "python3", "scripts/ci/coupling_ratchet.py", "check", "--current", c.tmp("coupling.rows"), "--edges",
        c.tmp("coupling.edges"), "--fanouts", c.tmp("coupling.fanouts"), "--collaborators",
        c.tmp("coupling.collaborators")])


def quality_ratchet(c: Check) -> bool:
    if not ratchet(c, "quality: quality/runMain quality.QualityReport", c.tmp("quality.rows"),
                   ["python3", "scripts/ci/quality_ratchet.py", "check", "--current", c.tmp("quality.rows")]):
        return False
    return all(c.run(["python3", f"scripts/design-rules/{tool}", "check", "--languages", "scala", *c.range_args()],
                     out=STDERR) == 0 for tool in ("naming_lint.py", "design_check.py"))


def sbt(c: Check) -> int:
    if unregistered(c) or unselected_reads(c) or not need_jvm(c, "sbt"):
        return 1
    fresh = fetch_gitops(c)
    lines = sbt_lines(c)
    reports, coupling, quality = report_lines(c)
    if not ran_sbt(c, lines + reports):
        return 1
    if (coupling and not coupling_ratchet(c)) or (quality and not quality_ratchet(c)):
        return 1
    return 0 if fresh else 1
