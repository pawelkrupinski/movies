#!/usr/bin/env python3
# script-test-reads: scripts/* .github/scripts/* .github/actions/* infra/bin/* android/scripts/* ios/scripts/*
"""fanout_lint.py (THE FAN-OUT LINT) over a scratch tree: it catches an ungoverned `xargs -P` and worker pool, passes
the governed ones and an allowed one, refuses a stale or unexplained allow row -- and finds this repository clean."""
from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import fanout_lint  # noqa: E402

REPO = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(REPO, "scripts/scratch-git.sh")
FILES = {
    "scripts/mutants-test.sh": 'seq 1 9 | xargs -P 4 -I{} bash -c "true"\n',
    "scripts/procs-test.sh": "ls | xargs --max-procs=8 -n1 true\n",
    "scripts/pool.py": "from concurrent.futures import ProcessPoolExecutor\n",
    "scripts/ci/threads.py": "import concurrent.futures\nconcurrent.futures.ThreadPoolExecutor(8)\n",
    "scripts/governed-test.sh": 'seq 1 9 | "$REPO/scripts/gov-parallel" --job m -P 4 -I{} true\n',
    "scripts/governed.py": "with governed_workers('m', 4) as w, ThreadPoolExecutor(w) as pool:\n    pass\n",
    "scripts/serial.sh": "seq 1 9 | xargs -n1 true  # one at a time\n",
    "scripts/commented.py": "# a ThreadPoolExecutor here would be wrong\n",
    "infra/bin/fetch": "import concurrent.futures\nconcurrent.futures.ThreadPoolExecutor(8)\n",
    "docs/elsewhere.sh": "seq 1 9 | xargs -P 9 true\n",
}


def scratch_git(*args: str) -> None:
    """git through scripts/scratch-git.sh's isolating helper (no inherited GIT_*, no ~/.gitconfig)."""
    subprocess.run(["bash", "-c", '. "$1"; shift; "$@"', "_", SCRATCH_GIT, *args], check=True, capture_output=True)


def lint(files: dict[str, str], allow: str = "") -> subprocess.CompletedProcess[str]:
    with tempfile.TemporaryDirectory() as root:
        for rel, body in {**files, **({fanout_lint.ALLOW: allow} if allow else {})}.items():
            os.makedirs(os.path.dirname(os.path.join(root, rel)), exist_ok=True)
            with open(os.path.join(root, rel), "w") as f:
                f.write(body)
        scratch_git("scratch_repo", root)
        scratch_git("scratch_git", root, "add", "-A")
        return subprocess.run([sys.executable, "-I", "-B", os.path.join(HERE, "fanout_lint.py"), "--root", root],
                              capture_output=True, text=True)


class FanoutLintTest(unittest.TestCase):
    def test_it_catches_every_ungoverned_fanout_and_only_those(self) -> None:
        r = lint(FILES)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)
        caught = sorted({line.split(":", 1)[0] for line in r.stdout.splitlines()})
        self.assertEqual(caught, ["infra/bin/fetch", "scripts/ci/threads.py", "scripts/mutants-test.sh",
                                  "scripts/pool.py", "scripts/procs-test.sh"])
        self.assertIn("scripts/gov-parallel", r.stdout, "it says how to fix an xargs -P")

    def test_an_allowed_pool_passes_and_a_stale_row_fails(self) -> None:
        ok = {"infra/bin/fetch": FILES["infra/bin/fetch"]}
        why = "network-bound: HTTP fetches, the laptop only waits"
        self.assertEqual(lint(ok, f"infra/bin/fetch\t{why}\n").returncode, 0)
        stale = lint(ok, f"infra/bin/fetch\t{why}\nscripts/gone.py\t{why}\n")
        self.assertEqual(stale.returncode, 1)
        self.assertIn("scripts/gone.py: stale", stale.stdout)

    def test_an_allow_row_without_a_real_why_is_malformed(self) -> None:
        r = lint({"infra/bin/fetch": FILES["infra/bin/fetch"]}, "infra/bin/fetch\tneeded\n")
        self.assertEqual(r.returncode, 2, r.stdout + r.stderr)

    def test_this_repository_is_clean(self) -> None:
        r = subprocess.run([sys.executable, "-I", "-B", os.path.join(HERE, "fanout_lint.py"), "--root", REPO],
                           capture_output=True, text=True)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)


if __name__ == "__main__":
    unittest.main()
