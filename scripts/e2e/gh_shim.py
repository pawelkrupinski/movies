#!/usr/bin/env python3
"""The tools' e2e `gh` (first on every tool's PATH as `gh`, scripts/e2e/e2e_world.py): each call goes to the scenario's
stub GitHub ($E2E_GH_URL, scripts/e2e/gh_stub.py), never to GitHub. The stub answers the data; this applies `--jq`
through the real jq, as gh does (`jq -r`), and prints it. Every call is logged to $E2E_CALLS as the other shims'."""
import json
import os
import subprocess
import sys
import urllib.request

argv = sys.argv[1:]
with open(os.environ["E2E_CALLS"], "a") as log:
    log.write(json.dumps({"name": "gh", "argv": argv, "cwd": os.getcwd()}) + "\n")
request = urllib.request.Request(os.environ["E2E_GH_URL"] + "/cli", data=json.dumps({"argv": argv}).encode(),
                                 method="POST", headers={"Content-Type": "application/json"})
with urllib.request.urlopen(request, timeout=20) as reply:
    answer = json.loads(reply.read())
sys.stderr.write(answer.get("stderr", ""))
if answer.get("rc", 0) != 0:
    sys.exit(answer["rc"])
text = json.dumps(answer.get("data"))
jq = argv[argv.index("--jq") + 1] if "--jq" in argv else None
if jq is None:
    sys.stdout.write(text + "\n")
    sys.exit(0)
done = subprocess.run(["jq", "-r", jq], input=text, text=True, capture_output=True)
sys.stdout.write(done.stdout)
sys.stderr.write(done.stderr)
sys.exit(done.returncode)
