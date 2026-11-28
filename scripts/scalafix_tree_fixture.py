"""What scalafix_move_test.py and scalafix_organize_test.py share: a scratch checkout of a few Scala sources, the kept
scalafix tree beside it, a fake sbt (KINOWO_SCALAFIX_SBT) noting each call's argv, directory, route and seed, and the
sbt commands every scalafix tool sends first. Black-box: nothing here reads the tools' own code."""
from __future__ import annotations

import json
import os
import sys
from typing import Any, Dict, List, Mapping, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

# every module's configurations, dependents first (as the shell tool spelled them)
CONFIGURATIONS = """quality/Test quality/Compile e2e/Test web/PageTest web/IntegrationTest web/Test web/Compile
worker/IntegrationTest worker/Test worker/Fixtures worker/Compile repochecks/Test repochecks/Fixtures scrapers/Test
scrapers/Fixtures scrapers/Compile itkit/Compile testkit/Test testkit/Compile identity/Test identity/Compile common/Test
common/Compile http/Test http/Fixtures http/Compile base/Test base/Compile geodata/Test geodata/Compile""".split()
SEMANTICDB = ["set ThisBuild / semanticdbEnabled := true", "set ThisBuild / scalafixCaching := false"]
# the head of every fake sbt: its route and seed noted, one JSON line a call
NOTE_ENV = '''
with open(env["SBT_ENV_LOG"], "a") as log:
    log.write(json.dumps({"route": env.get("KINOWO_SBT_WARM_ROUTE"), "seed": env.get("KINOWO_SBT_WARM_SEED")}) + "\\n")
'''


class ScalafixToolCase(cli_harness.ToolCase):
    """A case with `checkout` (a scratch repository of SOURCES, committed), `tree` (where the kept tree goes) and the
    fake sbt SBT (a Python body after NOTE_ENV)."""
    SOURCES: Mapping[str, str] = {}
    SBT = ""

    def setUp(self) -> None:
        super().setUp()
        self.checkout = self.repo("repo", "-b", "main")
        for path, text in self.SOURCES.items():
            self.write(f"repo/{path}", text)
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", "base")
        self.sbt = self.fakes.program("sbt", NOTE_ENV + self.SBT)
        self.tree = self.path("tree")

    def scalafix(self, *args: str, **extra: str) -> cli_harness.Done:
        """The tool from the checkout, its repository, tree and sbt the case's; KINOWO_SBT_WARM_SEED unset."""
        env: Dict[str, Optional[str]] = {
            "KINOWO_SCALAFIX_REPO": self.checkout, "KINOWO_SCALAFIX_TREE": self.tree, "KINOWO_SCALAFIX_SBT": self.sbt,
            "SBT_ENV_LOG": self.path("sbt-env.jsonl"), "KINOWO_SBT_WARM_SEED": None, **extra}
        return self.run_tool(*args, env=env, cwd=self.checkout)

    def sbt_calls(self) -> List[Dict[str, Any]]:
        """Each sbt call's argv and the directory it ran in."""
        with open(self.fakes.log) as f:
            return [e for e in map(json.loads, f) if e["name"] == "sbt"]

    def sbt_env(self) -> List[Dict[str, Optional[str]]]:
        """Each sbt call's KINOWO_SBT_WARM_ROUTE and KINOWO_SBT_WARM_SEED."""
        return [json.loads(line) for line in self.read("sbt-env.jsonl").splitlines()]

    def status(self) -> str:
        return self.git(self.checkout, "status", "--porcelain")

    def work_left(self, tool: str) -> List[str]:
        """The tool's work directories left in TMPDIR."""
        return [n for n in os.listdir(self.work) if n.startswith(f"{tool}.")]
