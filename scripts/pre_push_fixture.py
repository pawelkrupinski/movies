"""The scratch checkout the pre-push hook's black-box tests drive (scripts/pre_push_test.py, pre_push_run_test.py): a git repository holding a copy
of this checkout's hook and everything it runs from its own tree (scripts/hooks/, the shell shims beside it,
scripts/lib/ and the hook's Python modules), with FIXTURE tables in place of the real ones -- a small lint registry,
local-tests.txt, build.sbt's worker rows -- and a stub scripts/run-script-tests, so every expectation reads off
this file, never off the repository's own tables. The tests hold the hook to its command line, its output and its
exit status only: they pass unchanged on the shell hook and on its Python rewrite (scripts/hooks/pre-push either
way; KINOWO_PRE_PUSH_LEGACY=1 runs the frozen shell copy, when there is one)."""
from __future__ import annotations

import os
import re
import sys
from fnmatch import fnmatchcase
from typing import Dict, List, Mapping, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402
from seams.tool_files import Files, LocalDisk  # noqa: E402

ROOT = cli_harness.ROOT
HOOK = "scripts/hooks/pre-push"
# What the hook runs from its own tree, copied into every scratch checkout (globs from the repository root)
COPIED = ["scripts/hooks/*", "scripts/hooks-path.sh", "scripts/stall-watch.sh", "scripts/stall_watch.py", "scripts/lib/**/*.py",
          "scripts/pre_push.py", "scripts/identity-tools.sh", "scripts/identity_tools*.py"]

REGISTRY = "\n".join([
    "# the fixture's lint registry",
    "web:lint.StandingSpec\t*\tthe standing lint",
    "web:lint.WorkflowSpec\t.github/workflows/*\treads the workflows",
    "worker:lint.DockerSpec\tDockerfile\treads one file",
    "common:lint.MainSourcesSpec\t*/src/main/scala/*\tscans the production sources",
    "worker:lint.ServerSpec\t-\tboots a server",
    "repochecks:deploy.GitopsSpec\tinfra/kubernetes/* @gitops\treads the manifests",
    "web:lint.PerFileSpec\tdocs/* @per-file\tjudges each doc alone",
    "scrapers:lint.CodeOnlySpec\t@code\topens nothing",
    ""])
LOCAL_TESTS = "\n".join([
    "# the fixture's local tests",
    "basic sbt Test/compile",
    "basic sbt e2e/testOnly services.movies.FilmScheduleEndToEndSpec",
    "basic sbt repochecks/testOnly deploy.*",
    "fast sbt Test/compile",
    "identity first echo first-line-ran",
    "identity sbt worker/testOnly services.identity.*",
    ""])
BUILD = "\n".join([
    "lazy val HeavyRows: Map[String, Set[String]] = Map(",
    '  "heavy1" -> Set(',
    '    "services.identity.HeavyOneSpec",   // a heavy identity spec',
    '    "services.movies.HeavyRestSpec"),   // a heavy spec outside the identity prefixes',
    '  "heavy2" -> Set("scripts.HeavyTwoSpec"))',
    "lazy val HeavySuites: Set[String] = HeavyRows.values.flatten.toSet",
    'lazy val WorkerRestSuites: Set[String] = Set("scripts.RestShardSpec")',
    "lazy val workerRowSettings = rowSettings(\"WORKER_ROW\", Test, Seq(",
    '  "identity"  -> (suite => !HeavySuites(suite) &&',
    '    Seq("services.identity.", "scripts.", "tools.").exists(suite.startsWith))))',
    "",
    'addCommandAlias("e2eCorpus",   "e2e/Test/testOnly services.movies.FilmScheduleEndToEndSpec")',
    ""])
# scripts/run-script-tests, stubbed: `--reads` prints $STUB_READS' globs (else one), `--paths F --list` a row per path,
# anything else logs its argv to $STUB_RUNNER_ARGS and says one line
RUNNER = r'''#!/usr/bin/env bash
# the stub accepts --bash --floor) --linux --stress) --readers, as the real runner does (the hook greps for them)
case "$1" in
  --reads) [ -z "${STUB_READS_EXIT:-}" ] || exit "$STUB_READS_EXIT"; printf '%s\n' "${STUB_READS:-docs/read-by-a-test.md}"; exit 0 ;;
  --paths) while IFS= read -r p; do printf '%s-test.sh\tuntagged\tchanged\n' "${p%.*}"; done < "$2"; exit 0 ;;
esac
[ -z "${STUB_RUNNER_ARGS:-}" ] || echo "$*" >> "$STUB_RUNNER_ARGS"
case "$*" in *--linux*) [ -z "${STUB_NO_DOCKER:-}" ] || { echo "run-script-tests: --linux needs a running Docker" >&2; exit 2; } ;; esac
echo "run-script-tests: ${STUB_RUNNER_SAYS:-1 passed}"
exit "${STUB_RUNNER_EXIT:-0}"
'''
SNAPSHOT = '''#!/usr/bin/env bash
[ -z "${STUB_SNAPSHOT_RAN:-}" ] || echo "$*" >> "$STUB_SNAPSHOT_RAN"
[ -z "${STUB_SNAPSHOT_SLEEP:-}" ] || python3 -c 'import sys, time; time.sleep(float(sys.argv[1]))' "$STUB_SNAPSHOT_SLEEP"
echo "${STUB_SNAPSHOT_LINE:-FAIL: the snapshot inputs moved at c0ffee}"
exit "${STUB_SNAPSHOT_EXIT:-0}"
'''
FIXTURE_FILES: Dict[str, str] = {
    "scripts/hooks/lint-registry.tsv": REGISTRY,
    "scripts/hooks/local-tests.txt": LOCAL_TESTS,
    "scripts/hooks/lint-registry-missing": "#!/usr/bin/env bash\nexit 0\n",
    "build.sbt": BUILD,
    "scripts/run-script-tests": RUNNER,
    "scripts/read-model-snapshot-inputs.sh": SNAPSHOT,
    "README": "the scratch checkout\n",
}
# a gh answering every gating workflow from $GH_<WORKFLOW> (JSON runs, default none) through the hook's own --jq;
# GH_FAILS: an API error. `run view` answers $STUB_JOBS (jobs JSON) through --jq.
FAKE_GH = r'''
import subprocess
if env.get("GH_FAILS"):
    err("HTTP 502: Bad Gateway"); exit(1)
words = dict(zip(argv, argv[1:]))
if argv[:2] == ["run", "view"]:
    data = env.get("STUB_JOBS", '{"jobs": []}')
else:
    key = {"Main": "GH_MAIN", "Identity model convergence": "GH_IMC", "Identity takes": "GH_IT"}.get(words.get("--workflow", ""))
    if key is None:
        err("unknown call"); exit(1)
    data = env.get(key, "[]")
done = subprocess.run(["jq", "-r", words["--jq"]], input=data, capture_output=True, text=True)
sys.stdout.write(done.stdout); sys.stderr.write(done.stderr); exit(done.returncode)
'''


def run_json(run_id: int, status: str, conclusion: str) -> str:
    return (f'{{"databaseId":{run_id},"number":null,"status":"{status}","conclusion":"{conclusion}",'
            f'"url":"https://x/{run_id}"}}')


RED = run_json(38520439037, "completed", "failure")
GREEN = run_json(38487614044, "completed", "success")


FILES = Files(LocalDisk(), "/")


def copy_file(source: str, target: str) -> None:
    text = FILES.read_text(source)
    FILES.replace_text(target, text, 0o755 if text.startswith("#!") else 0o644)


def copy_matching(dest: str, pattern: str) -> None:
    """This checkout's files matching `pattern` (a directory, then a name pattern; `<directory>/**/<name>` the
    directory and every one below it) into the same place under `dest`."""
    directory, name = pattern.rsplit("/", 1)
    below = directory.endswith("/**")
    directory = directory[:-3] if below else directory
    for found in FILES.list(f"{ROOT}/{directory}"):
        if below and FILES.is_dir(f"{ROOT}/{directory}/{found}"):
            copy_matching(dest, f"{directory}/{found}/**/{name}")
        elif fnmatchcase(found, name) and FILES.is_file(f"{ROOT}/{directory}/{found}"):
            copy_file(f"{ROOT}/{directory}/{found}", f"{dest}/{directory}/{found}")


def copy_hook(dest: str) -> None:
    """The hook and what it runs from its own tree, from this checkout into `dest`, then the fixture's files."""
    for pattern in COPIED:
        copy_matching(dest, pattern)
    for line in FILES.read_text(f"{ROOT}/scripts/tool-conventions-files.txt").splitlines():
        if line.strip() and not line.startswith("#"):
            copy_file(f"{ROOT}/{line.strip()}", f"{dest}/{line.strip()}")
    write_files(dest, FIXTURE_FILES)


def write_files(dest: str, files: Mapping[str, str]) -> None:
    for relative, text in files.items():
        FILES.replace_text(f"{dest}/{relative}", text, 0o755 if text.startswith("#!") else 0o644)


def lines(text: str) -> List[str]:
    return [line for line in text.splitlines() if line]


def plain(text: str) -> str:
    """`text` without its colour codes (the hook's `▸ pre-push` / `! pre-push` prefixes are coloured)."""
    return re.sub(r"\x1b\[[0-9;]*m", "", text)


class PlanCase(cli_harness.ToolCase):
    """A scratch checkout per case, read-only: the plan modes ask it which checks a list of paths selects."""
    EXTRA: Dict[str, str] = {}  # a class's own files over the fixture's

    def setUp(self) -> None:
        super().setUp()
        self.checkout = self.repo("repo", "-b", "main")
        copy_hook(self.checkout)
        write_files(self.checkout, {
            "scripts/a-tool": "#!/usr/bin/env bash\necho a shell tool\n",
            "scripts/a-python-tool": "#!/usr/bin/env python3\nprint(1)\n",
            "scripts/a-data-file": "not a tool\n",
            "infra/nix/files/exporter.sh": "#!/bin/sh\necho '# TYPE kinowo_up gauge'\n",
            "infra/nix/files/quiet.sh": "#!/bin/sh\necho nothing\n",
            "web/src/test/scala/lint/WalkerSpec.scala": "class WalkerSpec { SourceTrees.all }\n",
            "web/src/test/scala/lint/OtherSpec.scala": "class OtherSpec\n",
            "worker/src/test/scala/tools/ScanSpec.scala": "class ScanSpec { ScalaSourceScan.files }\n"})
        write_files(self.checkout, self.EXTRA)
        self.commit_tree(self.checkout, "the scratch checkout")

    def hook(self, *args: str, stdin: Optional[str] = None, env: Optional[Mapping[str, Optional[str]]] = None
             ) -> cli_harness.Done:
        return self.run_tool(*args, env=env, cwd=self.checkout, stdin=stdin if stdin is not None else "",
                             tool=f"{self.checkout}/{HOOK}")

    def plan(self, mode: str, paths: Sequence[str], env: Optional[Mapping[str, Optional[str]]] = None,
             *extra: str) -> List[str]:
        done = self.hook(mode, *extra, stdin="".join(p + "\n" for p in paths), env=env)
        self.assertEqual(0, done.code, done.output)
        return lines(done.stdout)


class PushCase(cli_harness.ToolCase):
    """A scratch checkout of its own per case, its base commit `self.base`: the hook run over ranges of it."""

    def setUp(self) -> None:
        super().setUp()
        self.checkout = self.repo("repo", "-b", "main")
        copy_hook(self.checkout)
        self.base = self.commit_tree(self.checkout, "the scratch checkout")
        self.fakes.program("gh", FAKE_GH)

    def change(self, message: str, files: Mapping[str, str]) -> str:
        """`files` written and committed with `message` (a whole message: trailers in its last paragraph)."""
        write_files(self.checkout, files)
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-q", "-m", message)
        return self.git(self.checkout, "rev-parse", "HEAD")

    def hook(self, *args: str, env: Optional[Mapping[str, Optional[str]]] = None, stdin: Optional[str] = None,
             bound: float = cli_harness.HANG_BOUND) -> cli_harness.Done:
        full: Dict[str, Optional[str]] = {"PREPUSH_NO_REUSE": "1", "PREPUSH_GATE_RETRY_SECONDS": "0",
                                          "HOME": self.path("home")}
        full.update(env or {})
        return self.run_tool(*args, env=full, cwd=self.checkout, stdin=stdin,
                             tool=f"{self.checkout}/{HOOK}", bound=bound)

    def listed(self, *args: str, env: Optional[Mapping[str, Optional[str]]] = None) -> List[str]:
        """The checks the hook would run (PREPUSH_LIST=1), after every range rule."""
        done = self.hook(*args, env={"PREPUSH_LIST": "1", **(env or {})})
        self.assertEqual(0, done.code, done.output)
        return lines(done.stdout)
