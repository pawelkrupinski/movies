#!/usr/bin/env python3
"""The tools' e2e `gh` (first on every tool's PATH as `gh`, scripts/e2e/e2e_world.py): each call goes to the scenario's
stub GitHub ($E2E_GH_URL, scripts/e2e/gh_stub.py, which keeps the calls), never to GitHub. The stub answers the data;
this applies `--jq` through the real jq, as gh does (`jq -r`), and prints it."""
import json
import os
import sys
from typing import Any, Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.realpath(__file__))), "lib"))
import cli_harness  # noqa: E402
import machine  # noqa: E402
import tool_http  # noqa: E402


def printed(data: Any, jq: Optional[str], env: Dict[str, str]) -> int:
    """`data` printed as gh prints it: JSON, or through `jq -r <filter>`."""
    text = json.dumps(data)
    if jq is None:
        sys.stdout.write(text + "\n")
        return 0
    done = cli_harness.run_bounded(["jq", "-r", jq], env=env, cwd=".", stdin=text, bound=30)
    sys.stdout.write(done.stdout)
    sys.stderr.write(done.stderr)
    return done.code


def main(argv: List[str]) -> int:
    env = dict(machine.environment())
    reply = tool_http.Http.here().post(env["E2E_GH_URL"] + "/cli", 20, json_body={"argv": argv})
    answer: Dict[str, Any] = reply.json()
    sys.stderr.write(str(answer.get("stderr", "")))
    if answer.get("rc", 0) != 0:
        return int(answer["rc"])
    return printed(answer.get("data"), argv[argv.index("--jq") + 1] if "--jq" in argv else None, env)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
