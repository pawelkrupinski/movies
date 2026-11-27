"""The production snapshot identity-calibrate.sh --extract-prod reads, READ-ONLY, from every country database.

Each database's movies and movie_slots (scripts/identity-calibrate/extract-prod.js, find() only) into
<prod dir>/prod-<db>.jsonl, its row count said. Run by identity_calibrate.py under prod-tunnel.sh's prod_tunnel_run,
which holds the tunnel for its length and hands it the tunnel's URI as $KINOWO_CALIBRATE_URI (read by mongosh-at.sh,
never on mongosh's command line).

  KINOWO_CALIBRATE_URI=<tunnel uri> identity_calibrate_extract.py <prod dir>

Exit: 0, or the first failed read's mongosh status (the databases after it not read)."""
from __future__ import annotations

import os
import sys
from typing import Dict, List

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, os.path.join(HERE, "lib"))
import machine  # noqa: E402
import tool_process  # noqa: E402

URI = "KINOWO_CALIBRATE_URI"
DATABASES = ("kinowo", "kinowo_uk", "kinowo_de", "kinowo_es", "kinowo_us")
MONGOSH_AT = os.path.join(ROOT, "scripts", "local-mirror", "mongosh-at.sh")
EXTRACT = "scripts/identity-calibrate/extract-prod.js"


def extract(runner: tool_process.Runner, env: Dict[str, str], prod: str) -> int:
    os.makedirs(prod, exist_ok=True)
    for db in DATABASES:
        read = runner.capture(tool_process.Command([MONGOSH_AT, env[URI], "--quiet", "--file", EXTRACT],
                                                   {**env, "KINOWO_CALIBRATE_DB": db}, stderr_through=True))
        with open(os.path.join(prod, f"prod-{db}.jsonl"), "w") as f:
            f.write(read.stdout)
        if read.code != 0:
            return read.code
        print(f"prod snapshot {db}: {read.stdout.count(chr(10))} rows", flush=True)
    return 0


def main(argv: List[str]) -> int:
    if len(argv) != 1 or URI not in machine.environment():
        print(f"usage: {URI}=<tunnel uri> identity_calibrate_extract.py <prod dir>", file=sys.stderr)
        return 64
    os.chdir(ROOT)
    return extract(tool_process.SubprocessRunner(), dict(machine.environment()), argv[0])


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
