"""THE GOVERNED FAN-OUT FOR A PYTHON TOOL (scripts/lib; stdlib only): a worker pool sized by the governor.

  with governed_workers("mutation-tests", 4) as width:
      with ThreadPoolExecutor(width) as pool: ...

The owner, 2026-11-25 ("fix structurally"): load 103 on 18 cores while `agentctl gov status` said 0 admitted -- the
mutation tests' pools, run beside eight others by run-script-tests, held no ticket. The width comes from the ONE place
movies sizes a fan-out, scripts/jvm-gate.sh gov_width (scripts/gov-parallel is the same for `xargs -P`):

- under a ticket already ($AGENTCTL_GOV_WIDTH: a check's hook, run-script-tests' test, a gov-run with cores): none
  taken -- a nested pool never waits on its parent -- and at most that wide;
- else a ticket charged `want` cores, held by a helper shell (descriptor 7) until the pool ends, however it ends (its
  stdin closes with this process); the governor's own wait first, while the laptop is starved;
- no governor (Linux CI, KINOWO_GOVERNOR=0): `want` wide; the governor failing: one wide, warned.

THE TICKET'S MEASURED CPU: the CPU of this process's whole tree (every worker's children: node, npx, python) is metered
for the block (cpu_meter.py) and handed to the helper, which records it (`gov usage`) when it holds a ticket of its own:
the job's charge follows what the pool really used, not the width it asked for.
Inside, every process the pool starts runs with AGENTCTL_GOV_WIDTH=1 (os.environ, restored after): one core of it each.
scripts/ci/fanout_lint.py refuses a ThreadPoolExecutor / ProcessPoolExecutor / multiprocessing pool / `xargs -P` in a
file that does not size it here, unless its row in scripts/ci/fanout-allow.tsv says why (a network-bound pool)."""
from __future__ import annotations

import os
import subprocess
from typing import Optional

from processes import machine
from governor.cpu_meter import CpuMeter, own_tree_meter

WIDTH_ENV = "AGENTCTL_GOV_WIDTH"
GATE = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))), "jvm-gate.sh")
# the helper: gov_width, its width on stdout, then it holds the ticket (descriptor 7) until stdin ends -- after the pool's
# measured "<wall> <cpu>" line, which it records with the ticket's job (the tree's real CPU, `gov usage`)
HOLD = ('. "$1"; gov_width "$2" "$2" "$3" 1; echo "$GOV_WIDTH"; '
        'if read -r wall cpu && [ -n "$JVM_SLOT" ]; then jvm_usage "$2" "$wall" "$cpu" tree; fi; exec cat > /dev/null')


class governed_workers:  # noqa: N801  (a context manager read as a call: `with governed_workers(job, n) as width`)
    """The width a pool of up to `want` workers may run, under a governor ticket for as long as the block runs."""

    def __init__(self, job: str, want: int) -> None:
        self.job, self.want = job, max(1, want)
        self.saved: Optional[str] = None
        self.holder: Optional["subprocess.Popen[str]"] = None
        self.meter: Optional[CpuMeter] = None
        self.started = 0.0

    def __enter__(self) -> int:
        self.saved = os.environ.get(WIDTH_ENV)
        self.holder = subprocess.Popen(["bash", "-c", HOLD, "tool_pool", GATE, self.job, str(self.want)],
                                       stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
        line = self.holder.stdout.readline().strip() if self.holder.stdout else ""
        os.environ[WIDTH_ENV] = "1"
        self.meter = own_tree_meter(float(os.environ.get("KINOWO_CPU_METER_POLL") or 2))
        self.meter.start()
        self.started = machine.monotonic()
        return int(line) if line.isdigit() and int(line) >= 1 else 1

    def __exit__(self, *_exc: object) -> None:
        if self.saved is None:
            os.environ.pop(WIDTH_ENV, None)
        else:
            os.environ[WIDTH_ENV] = self.saved
        if self.holder:
            if self.holder.stdin:
                if self.meter:
                    self.holder.stdin.write(f"{int(machine.monotonic() - self.started)} {self.meter.stop()}\n")
                self.holder.stdin.close()
            if self.holder.stdout:
                self.holder.stdout.close()
            self.holder.wait()
