"""WHICH CHECKS A SET OF CHANGED PATHS SELECTS (scripts/hooks/pre-push --plan; the hook's header says what each check
guards): the check names in run order, and the heavy rows the landing check adds (PREPUSH_HEAVY_ROWS=1). What the
selection reads besides the paths -- the globs the self-tests declare they read, the lint registry's files, the
allow-lists, local-tests.txt, build.sbt, a file's first line -- comes in as a `Tree`."""
from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Callable, List, Sequence, Set, Tuple

from pre_push_hook.plan import pre_push_paths as paths_of
from pre_push_hook.plan.pre_push_paths import FirstLine, matches

ORDER = ("judgements", "allowlists", "newchecks", "schedules", "driftguards", "eslint", "jsquality", "pyquality",
         "mobilequality", "configquality", "shellgate", "drift", "snapshot", "dashboards", "metricalerts", "scripttests", "actionlint", "shellcheck", "sbt",
         "flipbacklog", "stampcarry", "identity", "ios")


@dataclass(frozen=True)
class Tree:
    """What the selection reads of the checkout besides the paths."""
    script_test_reads: Sequence[str]   # run-script-tests --reads
    registry_files: Set[str]           # the lint registry's named files
    allow_lists: Set[str]              # .github/allow-lists.txt's lines
    identity_tests: bool               # local-tests.txt names identity tests
    corpus_in_basic: bool              # `basic` already runs build.sbt's e2eCorpus specs
    heavy_rows: bool                   # PREPUSH_HEAVY_ROWS=1
    first_line: FirstLine
    exports_metrics: Callable[[str], bool]  # a fleet exporter: it echoes a `# TYPE kinowo_` header


# (check, patterns): a path matching one selects the check
BY_PATTERN = (
    ("jsquality", ("web/src/main/assets/js/*", "infra/version-dashboard/src/*", "test/resources/js-quality-baseline.tsv",
                   "scripts/ci/js_quality_ratchet.py", "scripts/ci/baseline_ratchet.py", "scripts/ci/trailers.py",
                   "scripts/js-quality/*", "scripts/js-quality-report", "scripts/js_quality_report.py",
                   "scripts/lib/code_tools/pinned_npm_tools.py", "scripts/lib/code_tools/twirl_scripts.py",
                   "web/src/main/twirl/*")),
    ("mobilequality", ("ios/*.swift", "android/*.kt", "android/*.kts", "test/resources/swift-quality-baseline.tsv",
                       "test/resources/kotlin-quality-baseline.tsv", "scripts/ci/mobile_quality_*",
                       "scripts/mobile-quality-report", "scripts/lizard-requirements.txt", "scripts/python-venv",
                       "scripts/lib/code_tools/clone_groups.py", "scripts/lib/code_tools/quality_report.py",
                       "scripts/ci/baseline_ratchet.py", "test/resources/design-rules.tsv")),
    ("configquality", ("*.nix", "*.tf", "web/src/main/twirl/*", "test/resources/nix-quality-baseline.tsv",
                       "test/resources/terraform-quality-baseline.tsv", "test/resources/twirl-quality-baseline.tsv",
                       "scripts/ci/config_quality_*", "scripts/config-quality-report",
                       "scripts/lib/code_tools/clone_groups.py", "scripts/lib/code_tools/quality_report.py",
                       "scripts/ci/baseline_ratchet.py", "test/resources/design-rules.tsv")),
    ("shellgate", ("test/resources/shell-baseline.tsv", "test/resources/shell-keeps.tsv", ".github/workflows/*",
                   ".github/actions/*", "scripts/design-rules/shell_gate.py", "scripts/design-rules/shell_measure.py",
                   "scripts/design-rules/workflow_runs.py",
                   "scripts/ci/baseline_ratchet.py", "test/resources/design-rules.tsv")),
    ("eslint", ("web/src/main/assets/js/*.js", "web/src/page/resources/*.js", "eslint.config.js", "eslint-rules/*.js",
                "package.json", "package-lock.json", "page-tests-playwright/tests/*.ts",
                "page-tests-playwright/lint-flake-patterns*.mjs")),
    ("drift", ("common/src/main/scala/models/*", "geodata/src/main/scala/models/*",
               "geodata/src/main/resources/generated/*", "data/*", "android/app/src/main/java/pl/kinowo/model/City.kt",
               "ios/Kinowo/Models/City.swift", "scripts/ci/generated-artefacts-drift.sh")),
    ("snapshot", ("test/resources/fixtures/08-06-2026/*", "scripts/read-model-snapshot-inputs.sh")),
    ("sbt", ("*.scala", "*.java", "build.sbt", "project/*", "infra/nix/files/monitoring/grafana/dashboards/*",
             "web/src/main/assets/*", "web/src/main/twirl/*", "web/src/page/*",
             "test/resources/coupling-baseline.tsv", "test/resources/coupling-layers.tsv",
             "test/resources/coupling-exemptions.tsv", "test/resources/coupling-fanout-refs.tsv",
             "test/resources/coupling-collaborators.tsv", "test/resources/complexity-baseline.tsv",
             "test/resources/length-baseline.tsv", "test/resources/duplication-baseline.tsv",
             "test/resources/quality-data-tables.tsv", "scripts/ci/quality_ratchet.py",
             "scripts/design-rules/naming_lint.py", "scripts/design-rules/naming.py",
             "scripts/design-rules/scala_names.py", "scripts/design-rules/extractors.py",
             "scripts/design-rules/design_check.py", "scripts/design-rules/model.py",
             "scripts/design-rules/exemptions.py", "scripts/lib/rules_table.py", "test/resources/design-rules.tsv",
             "test/resources/design-baseline.tsv", "test/resources/design-exemptions.tsv",
             "geodata/src/main/resources/generated/*", "scripts/*.sh", ".github/*.sh", ".github/*.yml",
             ".github/*.yaml", "infra/*.sh", "infra/*.yml", "infra/*.yaml")),
    ("dashboards", ("infra/nix/files/monitoring/*", "infra/test/test_dashboards.py")),
    ("metricalerts", ("infra/nix/files/monitoring/rules/*", "infra/nix/files/monitoring/grafana/alerting/*",
                      "infra/test/app-metrics-catalogue/*", "infra/test/test_metric_alert_coverage.py",
                      "infra/test/app_metrics_catalogue.py", "infra/test/app_metrics_absent.py",
                      "infra/test/promql_selectors.py", "test/resources/metrics-without-alerts.tsv",
                      "scripts/pyyaml-requirements.txt", "scripts/python-venv", "*/src/main/*etrics*")),
    ("scripttests", ("scripts/*", ".github/scripts/*", ".github/actions/*", "android/scripts/*", "ios/scripts/*",
                     "infra/bin/*", "data/*")),
    ("actionlint", (".github/workflows/*.yml", ".github/workflows/*.yaml", ".github/actions/*/action.yml",
                    ".github/actions/*/action.yaml")),
    ("ios", ("ios/*",)),
    ("judgements", ("test/resources/fixtures/identity-judgements.tsv", "test/resources/fixtures/identity-unmatched/*.tsv",
                    "test/resources/fixtures/corpus/expected-hard-clusters-*", "scripts/ci/identity_judgements*.py",
                    "scripts/ci/identity_takes*.py")),
    ("allowlists", (".github/allow-lists.txt", "scripts/ci/commit_rules*.py", "scripts/ci/commit_facts.py",
                   "scripts/ci/allow_list_rules.py", "scripts/ci/drift_guard_rules.py", "scripts/ci/retype_rules.py")),
    ("flipbacklog", ("test/resources/fixtures/identity-flip-backlog.tsv",
                     "test/resources/fixtures/identity-flips-struck.tsv", "test/resources/fixtures/drift-gate-seeds.tsv",
                     "test/resources/fixtures/drift-slices/*", "test/resources/fixtures/identity-slices/*",
                     "scripts/ci/population_walk_diff*.py")),
    ("newchecks", (".github/workflows/*", ".github/gating-checks.tsv", "scripts/ci/gating_checks*",
                   "scripts/hooks/pre-push")),
    ("stampcarry", ("test/resources/identity-stamp-carry.tsv",)),
    ("schedules", (".github/workflows/*", "infra/nix/files/scheduled-workflows.json",
                   "infra/nix/files/scheduled_dispatch.py")),
)
EXPORTERS = ("infra/nix/*.sh", "infra/nix/*.nix", "scripts/ci/*.py")


Rule = Callable[[str, Tree], bool]
# (check, rule): a path the rule holds for selects the check, beside BY_PATTERN
RULES: Sequence[Tuple[str, Rule]] = (
    ("sbt", lambda p, t: paths_of.read_by_lint_specs(p) or p in t.registry_files),
    ("metricalerts", lambda p, t: matches(p, EXPORTERS) and t.exports_metrics(p)),
    ("scripttests", lambda p, t: matches(p, t.script_test_reads)),
    ("allowlists", lambda p, t: p in t.allow_lists),
    ("shellcheck", lambda p, t: paths_of.is_shell_path(p, t.first_line)),
    # a shell tool, or an extension-less one that may have been (a migration drops its frozen rows: a fall to record)
    ("shellgate", lambda p, _t: p.endswith((".sh", ".bash")) or paths_of.is_tool_name(p)),
    ("identity", lambda p, _t: paths_of.is_identity_path(p) or paths_of.is_identity_reach_path(p)),
    ("driftguards", lambda p, _t: paths_of.is_drift_path(p)),
)


def path_checks(path: str, tree: Tree) -> Set[str]:
    """The checks one path selects, before the whole-push rules (the identity area's tests, the Python ratchet)."""
    found = {check for check, patterns in BY_PATTERN if matches(path, patterns)}
    return found | {check for check, rule in RULES if check not in found and rule(path, tree)}


def plan(changed: Sequence[str], tree: Tree) -> List[str]:
    """The checks the paths select, in run order, then the heavy rows (landing check only, with the sbt check)."""
    found: Set[str] = set()
    for path in changed:
        found |= path_checks(path, tree)
    if paths_of.py_quality_paths(changed, tree.first_line):
        found.add("pyquality")
    if not tree.identity_tests:
        found.discard("identity")
    if "identity" in found:
        found.add("sbt")
    selected = [check for check in ORDER if check in found]
    if tree.heavy_rows and "sbt" in found:
        selected += heavy_rows(changed, tree.corpus_in_basic)
    return selected


IT_REST = ("*/src/main/*", "*/src/it/*", "*/src/test/*", "*/src/fixtures/*", "build.sbt", "project/*", "conf/*",
           "test/resources/*")
IT_IDENTITY = ("build.sbt", "project/*", "test/resources/fixtures/*", "common/src/main/*", "base/src/main/*",
               "worker/src/main/*", "worker/src/it/scala/HardCluster*")


def reaches_identity_row(path: str) -> bool:
    return (paths_of.is_identity_path(path) or paths_of.is_identity_reach_path(path)
            or matches(path, IT_IDENTITY))


def heavy_rows(changed: Sequence[str], corpus_in_basic: bool) -> List[str]:
    """THE HEAVY ROWS the paths can move: itrest, itidentity and, with it, e2ecorpus unless `basic` runs it."""
    rows: List[str] = []
    if any(matches(p, IT_REST) for p in changed):
        rows.append("itrest")
    if any(reaches_identity_row(p) for p in changed):
        rows.append("itidentity")
        if not corpus_in_basic:
            rows.append("e2ecorpus")
    return rows


ALIAS = re.compile(r'^addCommandAlias\("e2eCorpus",.*"e2e/Test/testOnly ([^"]*)')
BASIC_E2E = re.compile(r"^basic.*sbt.* e2e/(?:Test/)?testOnly (.*)$")


def is_corpus_in_basic(build: str, local_tests: str) -> bool:
    """Whether every one of build.sbt's e2eCorpus specs is named on a `basic` e2e line (an alias it cannot read: no)."""
    specs: List[str] = []
    for line in build.splitlines():
        found = ALIAS.match(line)
        if found:
            specs = found.group(1).split()
    basic: Set[str] = set()
    for line in local_tests.splitlines():
        found = BASIC_E2E.match(line)
        if found:
            basic.update(found.group(1).split())
    return bool(specs) and all(not s.startswith("-") and s in basic for s in specs)
