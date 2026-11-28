"""The fixture scripts/sbt_warm_stall_test.py, sbt_warm_progress_test.py and sbt_warm_serve_test.py share (on top of
sbt_warm_fixture.py): the stubs a call that hangs, burns CPU or winds down, a `ps` that holds the CPU measures, and
StallCase, a checkout seeded for them with a 1 s stall poll."""
from __future__ import annotations

import os

from sbt_warm_fixture import SbtWarmCase
from harness import cli_harness

HANG = '#!/usr/bin/env bash\nif [ -n "${STUB_HANG_ONCE:-}" ] && [ -f "$STUB_HANG_ONCE" ]; then echo served; exit 0; fi\n' \
       '[ -n "${STUB_HANG_ONCE:-}" ] && touch "$STUB_HANG_ONCE"\necho $$ >> "@S@/hung"; exec sleep 86400\n'
BUSY = '#!/usr/bin/env bash\nend=$((SECONDS + 4))\nwhile [ "$SECONDS" -lt "$end" ] ||\n' \
       '  { [ -n "${KINOWO_PROGRESS_FILE:-}" ] && [ ! -e "$KINOWO_PROGRESS_FILE" ] && [ "$SECONDS" -lt 60 ]; }; do :; done\n' \
       'echo done\n'
WINDING = '#!/usr/bin/env bash\nend=$((SECONDS + 2)); while [ "$SECONDS" -lt "$end" ]; do :; done\n' \
          'touch "@S@/winding-down"\nend=$((SECONDS + 3)); while [ "$SECONDS" -lt "$end" ]; do :; done; echo done\n'
# a `ps` that holds every CPU measure made once the stub winds down until the processes it names have ended
SLOW_PS = r'''#!/usr/bin/env bash
measure=0 pids="" previous=""
for argument in "$@"; do
  [ "$previous $argument" = "-o time=" ] && measure=1
  [ "$previous" = -p ] && pids=$argument
  previous=$argument
done
if [ "$measure" = 1 ] && [ -n "$pids" ] && [ -e "@S@/winding-down" ]; then
  for _ in $(seq 1 100); do @PS@ -o stat= -p "$pids" 2> /dev/null | grep -qv '^Z' || break; sleep 0.1; done
fi
exec @PS@ "$@"
'''


class StallCase(SbtWarmCase):
    def setUp(self) -> None:
        super().setUp()
        self.extra["KINOWO_STALL_POLL_SECONDS"] = "1"
        self.stall = self.worktree("stall")
        self.warm(self.stall, "--seed")
        self.ps = "/bin/ps"

    def shimmed(self, directory: str, body: str) -> str:
        """A PATH with `ps` shimmed by `body` (@PS@ the real one) first."""
        return self.shim(directory, "ps", body.replace("@PS@", self.ps)) + os.pathsep + self.env()["PATH"]

    def left_running(self) -> int:
        return cli_harness.still_running(self.path("hung"))
