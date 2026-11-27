"""THE HOOK EVENT the claude-hooks read: the JSON Claude Code writes on a hook's stdin, read for a bounded time.

  read_event(seconds)   the event as a dict; {} from a terminal (a hook run by hand never waits on the tty), from a
                        writer that sends nothing within `seconds`, and from JSON that is not an object

Stdlib only. A malformed document raises ValueError: a hook's own `except Exception` decides (every guard allows)."""
from __future__ import annotations

import json
import os
import select
import sys
import time
from typing import Any, Dict, Optional

DEFAULT_SECONDS = 3.0


def read_event(seconds: float = DEFAULT_SECONDS, fd: Optional[int] = None) -> Dict[str, Any]:
    if fd is None:
        if sys.stdin is None or sys.stdin.isatty():
            return {}
        fd = sys.stdin.fileno()
    data, end = b"", time.monotonic() + seconds
    while True:
        left = end - time.monotonic()
        if left <= 0 or not select.select([fd], [], [], left)[0]:
            return {}
        chunk = os.read(fd, 65536)
        if not chunk:
            event = json.loads(data or b"{}")
            return event if isinstance(event, dict) else {}
        data += chunk
