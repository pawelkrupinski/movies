"""The scratch repository the black-box tests of the ref-laying identity tools share (title_trace_test.py,
venue_replay_test.py): a copy of the tool and the scripts it runs (each name copied when the checkout holds it, so one
test runs unchanged against the shell tool and its Python rewrite), scripts/lib whole, a stand-in for every file the tool
drops into a ref, the trees identity_classpath looks at, and build.sbt at two commits -- `base` and `head` -- whose
contents tell the refs' code apart; and an `sbt` fake printing a classpath, as `export` does."""
from __future__ import annotations

import glob
import os
import shutil
import sys
from dataclasses import dataclass
from typing import Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

SBT = 'echo "$PWD/worker/target/scala-3.x/test-classes:/lib/a.jar"'


@dataclass(frozen=True)
class ToolRepo:
    checkout: str
    base: str
    head: str


def tool_repo(case: cli_harness.ToolCase, scripts: Sequence[str], tool_files: Sequence[str], base_build: str,
              head_build: str) -> ToolRepo:
    """The repository at `case.path("repo")`: `scripts` (paths under scripts/) copied from this checkout, `tool_files`
    each a one-line stand-in, build.sbt `base_build` at the first commit and `head_build` at the second."""
    checkout = case.path("repo")
    for name in scripts:
        source = os.path.join(cli_harness.ROOT, "scripts", name)
        if os.path.exists(source):
            os.makedirs(os.path.dirname(os.path.join(checkout, "scripts", name)), exist_ok=True)
            shutil.copy2(source, os.path.join(checkout, "scripts", name))
    os.makedirs(os.path.join(checkout, "scripts", "lib"), exist_ok=True)
    for module in glob.glob(os.path.join(cli_harness.ROOT, "scripts", "lib", "*.py")):
        shutil.copy2(module, os.path.join(checkout, "scripts", "lib"))
    for relative in tool_files:
        case.write(f"repo/{relative}", f"// {relative}\n")
    for tree in ("common/src", "identity/src", "testkit/src"):
        os.makedirs(os.path.join(checkout, tree), exist_ok=True)
    case.write("repo/build.sbt", base_build)
    case.repo("repo")
    case.git(checkout, "add", "-A")
    case.git(checkout, "commit", "-qm", "base")
    case.write("repo/build.sbt", head_build)
    case.git(checkout, "commit", "-qam", "head")
    case.fakes.script("sbt", SBT)
    return ToolRepo(checkout, case.git(checkout, "rev-parse", "HEAD~1").strip(),
                    case.git(checkout, "rev-parse", "HEAD").strip())
