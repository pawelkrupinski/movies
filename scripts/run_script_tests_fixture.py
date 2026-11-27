"""The scratch checkout scripts/run-script-tests' black-box tests drive (run_script_tests_*_test.py): a git repository of
fake self-tests and tools under the script roots, the runner run over it with --root, and what its listing selects.
The tests hold the tool to its command line only (they passed unchanged on the shell tool it replaced)."""
from __future__ import annotations

import os
import sys
from typing import Dict, Mapping, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)), "lib"))
import cli_harness  # noqa: E402

ROOT = cli_harness.ROOT
RUNNER = cli_harness.tool_path("scripts/run-script-tests")
# the base tree: five untagged tests to find, one tagged, and what discovery must pass over
C_TEST = "\n".join([". scripts/ci/lib.sh",
                    'echo "  ✗ an early case"; echo "     expected: yes"; echo "     actual:   no"',
                    'for n in $(seq 1 100); do echo "line $n"; done', "echo boom; exit 1"])
BASE_TREE: Dict[str, str] = {
    "scripts/a.sh": "echo tool a",
    "scripts/a-test.sh": "echo a passes",
    "scripts/ci/b_test.py": 'print("b passes")',
    "scripts/ci/lib.sh": "echo a library",
    "scripts/c-test.sh": C_TEST,
    "scripts/d-test.sh": "#!/usr/bin/env bash\n# script-test: manual needs a live server\nexit 1",
    ".github/actions/x/e-test.sh": "exit 0",
    "android/scripts/f-test.sh": "exit 0",
    "scripts/helper.sh": "exit 1",
    "scripts/test_old.py": "raise SystemExit(1)",
    "scripts/node_modules/pkg/g-test.sh": "exit 1",
    "docs/h-test.sh": "exit 1",
}
# an agentctl whose governor charges cores (shell-spec.sh's spec_fake_governor): `gov admit` notes its arguments in
# $GOV_CALLS, waits while $GOV_HOLD names a file not there yet (a starved laptop), then takes the ticket's lock on fd 7
FAKE_GOVERNOR = r'''#!/usr/bin/env bash
[ "$1 $2" = "gov probe" ] && { echo "agentctl governor: ${GOV_FEATURES:-admit cores route width}"; exit 0; }
case "$1 $2" in
  "gov status") [ -z "${GOV_STATUS:-}" ] || sed "s/PID/$(cat "$GOV_HOLD.pid" 2>/dev/null)/" "$GOV_STATUS"; exit 0 ;;
  "gov stats") [ -z "${GOV_STATS:-}" ] || cat "$GOV_STATS"; exit 0 ;;
esac
printf '%s\n' "$*" >> "$GOV_CALLS"
if [ -n "${GOV_GRANT:-}" ] && case " $* " in *" --min-cores "*) true ;; *) false ;; esac; then
  ticket=$(printf '%s\n' "$@" | sed -n '/^--ticket$/{n;p;}')
  echo "{\"cores\": $GOV_GRANT}" > "${ticket%.lock}.json"
fi
if [ -n "${GOV_HOLD:-}" ]; then
  printf '%s\n' "$@" | sed -n '/^--pid$/{n;p;}' > "$GOV_HOLD.pid"
  echo waiting > "$GOV_HOLD.waiting"
  for _ in $(seq 1 2400); do [ -e "$GOV_HOLD" ] && break; sleep 0.05; done
fi
python3 -I -B -c 'import fcntl; fcntl.flock(7, fcntl.LOCK_EX | fcntl.LOCK_NB)'
'''


class ScriptTestsCase(cli_harness.ToolCase):
    """A scratch checkout (`self.tree`, its first commit `self.base`) and the runner over it."""
    TOOL = RUNNER

    def setUp(self) -> None:
        super().setUp()
        self.tree = self.repo("repo")
        for relative, text in BASE_TREE.items():
            self.put(relative, text)
        self.base = self.commit_tree(self.tree, "base")

    def put(self, relative: str, text: str, mode: int = 0o644) -> str:
        """A file in the scratch tree, its text and a newline."""
        full = os.path.join(self.tree, relative)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "w") as f:
            f.write(text + "\n")
        os.chmod(full, mode)
        return full

    def append(self, relative: str, text: str) -> None:
        with open(os.path.join(self.tree, relative), "a") as f:
            f.write(text + "\n")

    def restore(self, *relatives: str) -> None:
        self.git(self.tree, "checkout", "-q", "--", *relatives)

    def commit(self, message: str) -> str:
        return self.commit_tree(self.tree, message)

    def runner(self, *args: str, env: Optional[Mapping[str, Optional[str]]] = None,
               root: Optional[str] = None) -> cli_harness.Done:
        """The runner over the scratch tree (or `root`), from the scratch directory."""
        return self.run_tool("--root", root or self.tree, *args,
                             env={"RUN_SCRIPT_TESTS_LOG_DIR": self.path("logs"),
                                  "RUN_SCRIPT_TESTS_DURATIONS": self.path("durations.tsv"), **(env or {})})

    def listed(self, *args: str, env: Optional[Mapping[str, Optional[str]]] = None) -> str:
        """The paths `--list` prints, space-separated."""
        done = self.runner("--list", *args, env=env)
        return " ".join(line.split("\t")[0] for line in done.stdout.splitlines() if line)

    def rows(self, done: cli_harness.Done, status: str, name: str) -> int:
        """How many table rows of `status` name `name`."""
        return sum(1 for line in done.stdout.splitlines() if line.startswith(status) and name in line)

    def governor(self) -> str:
        """The fake agentctl (FAKE_GOVERNOR) beside its tickets' directory, its calls noted in gov-calls."""
        path = self.write("gov-bin/agentctl", FAKE_GOVERNOR)
        os.chmod(path, 0o755)
        self.write("gov-calls", "")
        return path

    def governed(self, agentctl: str, extra: Optional[Mapping[str, Optional[str]]] = None) -> Dict[str, Optional[str]]:
        """The environment of a run on "the Mac" with that governor governing (shell-spec.sh's spec_governed)."""
        env: Dict[str, Optional[str]] = {
            "KINOWO_GOV_CPU_HELD": None, "KINOWO_GOV_CLASS": None, "KINOWO_JVM_SLOT_HELD": None,
            "AGENTCTL_GOV_WIDTH": None, "KINOWO_GOVERNOR_FEATURES": None, "KINOWO_GOVERNOR": "1",
            "KINOWO_GOVERNOR_BIN": agentctl, "KINOWO_JVM_GATE_OS": "Darwin",
            "AGENTCTL_GOV_DIR": os.path.join(os.path.dirname(agentctl), "gov"), "GOV_CALLS": self.path("gov-calls")}
        env.update(extra or {})
        return env

    def gov_calls(self) -> str:
        return self.read("gov-calls")
