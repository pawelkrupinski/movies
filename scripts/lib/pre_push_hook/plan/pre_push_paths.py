"""THE PRE-PUSH HOOK'S PATH CLASSES (scripts/hooks/pre-push; its header says what each check guards): which changed path
belongs to which area -- the identity paths, what moves takes, what the identity specs read by class alone, what a
drift fix touches, a production path, what the lint specs read -- and the ratchets' path filters (`--coupling-plan`,
`--quality-plan`, `--js-quality-plan`, `--py-quality-plan`). Pure: a pattern is a bash `case` pattern (`*` crosses
`/`), matched by fnmatchcase; what a file's first line says comes in as a function."""
from __future__ import annotations

import re
from fnmatch import fnmatchcase
from typing import Callable, Iterable, List, Sequence

FirstLine = Callable[[str], str]  # a path's first line, "" when it cannot be read


def matches(path: str, patterns: Sequence[str]) -> bool:
    """Whether `path` matches one of the bash `case` patterns."""
    return any(fnmatchcase(path, pattern) for pattern in patterns)


IDENTITY = (
    "identity/*", "worker/src/main/scala/services/identityadapters/*", "*/src/test/scala/services/identity/*",
    "common/src/main/resources/title-rules.tsv", "common/src/main/resources/title-words.tsv",
    "common/src/main/scala/services/titles/rules/*", "common/src/main/scala/services/titles/TitleNormalizer.scala",
    "*/src/test/scala/services/titles/rules/*", "worker/src/test/scala/scripts/*",
    "worker/src/test/scala/tools/HardClusterRatchetSpec.scala", "worker/src/test/scala/tools/CountryScope.scala",
    "worker/src/fixtures/*", "testkit/src/main/scala/services/identity/*",
    "testkit/src/main/scala/services/movies/SingleCountryNormalizer.scala",
    "testkit/src/main/scala/services/movies/NormalizerTrace.scala",
    "test/resources/fixtures/identity-*", "test/resources/fixtures/corpus/*", "test/resources/fixtures/drift-slices/*")
TAKES = (
    "identity/src/main/*", "worker/src/main/scala/services/identityadapters/*",
    "common/src/main/resources/title-rules.tsv", "common/src/main/resources/title-words.tsv",
    "common/src/main/scala/services/titles/rules/*", "common/src/main/scala/services/titles/TitleNormalizer.scala",
    "test/resources/fixtures/identity-judgements.tsv")
IDENTITY_REACH = ("common/src/main/scala/*", "common/src/main/java/*", "testkit/src/main/scala/*")
DRIFT = ("worker/src/main/*", "worker/src/test/scala/scripts/IdentityDrift*",
         "worker/src/test/scala/scripts/IdentityLiveDrift*", "scripts/ci/identity-drift-check*",
         "scripts/ci/identity_drift_check*", "scripts/ci/commit_rules*")
PRODUCTION = ("*/src/main/*", "conf/*", "*/conf/*", "build.sbt", "project/*", "infra/nix/*", "infra/kubernetes/*",
              "Dockerfile")
LINT_READ = ("scripts/*", ".github/*", ".claude/*", "CLAUDE.md", "build.sbt", "project/*", "*/src/test/*",
             "*/src/it/*", "*/src/page/*.scala", "worker/src/fixtures/*", "scrapers/src/fixtures/*",
             "http/src/fixtures/*", "repochecks/src/fixtures/*", "testkit/*")


def is_identity_path(path: str) -> bool:
    return matches(path, IDENTITY)


def is_take_path(path: str) -> bool:
    return matches(path, TAKES)


def is_identity_reach_path(path: str) -> bool:
    return matches(path, IDENTITY_REACH)


def is_drift_path(path: str) -> bool:
    return is_identity_path(path) or matches(path, DRIFT)


def is_production_path(path: str) -> bool:
    return matches(path, PRODUCTION)


def read_by_lint_specs(path: str) -> bool:
    """Whether a push of `path` runs the sbt check for the lint specs (the mobile apps' trees are no sbt module's)."""
    return not matches(path, ("android/*", "ios/*")) and matches(path, LINT_READ)


SHELL_SHEBANG = re.compile(r"^#!.*[/ ](ba|da|k)?sh( |$)")
PYTHON_SHEBANG = re.compile(r"^#!.*python")


def basename(path: str) -> str:
    return path.rsplit("/", 1)[-1]


def is_shell_path(path: str, first_line: FirstLine) -> bool:
    """What shellcheck reads: a .sh, the hooks, and a tool with no extension whose shebang names a shell."""
    if matches(path, ("*.sh", "scripts/hooks/*")):
        return True
    return "." not in basename(path) and bool(SHELL_SHEBANG.search(first_line(path)))


def identity_change(paths: Iterable[str]) -> List[str]:
    """`identity` when one of the paths can move takes."""
    return ["identity"] if any(is_take_path(p) for p in paths) else []


MODULES = "(geodata|base|http|common|identity|scrapers|worker|web)"
COUPLING = re.compile(
    rf"^({MODULES}/src/main/.*\.(scala|java)|build\.sbt|coupling\.sbt|project/[^/]*\.scala|"
    r"test/resources/coupling-(baseline|layers|exemptions|fanout-refs|collaborators)\.tsv|"
    r"scripts/ci/coupling_(ratchet|rows|violations|growth|exemption_table|exemption_history|reasons|renames|splits|"
    r"verdicts)\.py)$")
QUALITY = re.compile(
    rf"^({MODULES}/src/main/.*\.scala|quality/src/main/.*|project/Dependencies\.scala|"
    r"test/resources/(complexity|length|duplication)-baseline\.tsv|test/resources/quality-data-tables\.tsv|"
    r"scripts/ci/quality_ratchet\.py|"
    r"scripts/design-rules/(naming_lint|naming|scala_names|extractors|design_check|model|exemptions)\.py|"
    r"scripts/lib/rules_table\.py|test/resources/design-(rules|baseline|exemptions)\.tsv)$")
JS_QUALITY = re.compile(
    r"^(web/src/main/assets/js/.*|infra/version-dashboard/src/.*|test/resources/js-quality-baseline\.tsv|"
    r"scripts/ci/js_quality_ratchet\.py|scripts/ci/baseline_ratchet\.py|scripts/ci/trailers\.py|"
    r"scripts/js-quality/.*|scripts/js-quality-report|scripts/js_quality_report\.py|"
    r"scripts/lib/pinned_npm_tools\.py|scripts/design-rules/[a-z_]+\.py|scripts/lib/rules_table\.py|"
    r"test/resources/design-(rules|baseline|exemptions)\.tsv)$")
PYTHON_TREES = ("scripts/*", ".github/scripts/*", "infra/bin/*", "infra/nix/bin/*", "infra/nix/files/*",
                "android/scripts/*", "ios/scripts/*")
PY_QUALITY = tuple(tree + ".py" for tree in PYTHON_TREES) + (
    "test/resources/py-quality-baseline.tsv", "scripts/py-quality/*", "scripts/py-quality-report",
    "test/resources/design-rules.tsv", "test/resources/design-baseline.tsv", "test/resources/design-exemptions.tsv",
    "pyrightconfig.json")


def coupling_paths(paths: Iterable[str]) -> List[str]:
    return [p for p in paths if COUPLING.search(p)]


def quality_paths(paths: Iterable[str]) -> List[str]:
    return [p for p in paths if QUALITY.search(p)]


def js_quality_paths(paths: Iterable[str]) -> List[str]:
    return [p for p in paths if JS_QUALITY.search(p)]


def is_py_quality_path(path: str, first_line: FirstLine) -> bool:
    """A Python tool (a .py, or an extension-less file whose first line names python), its baseline, the measure, its
    engine or pyrightconfig.json; a self-test is reported, never held."""
    if path.endswith("_test.py"):
        return False
    if matches(path, PY_QUALITY):
        return True
    return (matches(path, PYTHON_TREES) and "." not in basename(path)
            and bool(PYTHON_SHEBANG.search(first_line(path))))


def py_quality_paths(paths: Iterable[str], first_line: FirstLine) -> List[str]:
    return [p for p in paths if is_py_quality_path(p, first_line)]
