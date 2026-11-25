"""listing_history's helpers (scripts/listing_history.py: the runner, the mirror and log endpoints, the title folding) under
real annotations, for the flip_timeline modules: listing_history itself carries none."""
from __future__ import annotations

import os
from collections.abc import Callable, Mapping
from typing import cast

import listing_history as _history

REPO = _history.REPO
LOGS = cast(str, _history.LOGS)
PROMETHEUS = cast(str, _history.PROMETHEUS)
MIRROR = cast(str, _history.MIRROR)
LOG_LIMIT = cast(int, _history.LOG_LIMIT)

Run = Callable[[list[str]], tuple[int, str]]

fold = cast(Callable[[str], str], getattr(_history, "fold"))
seconds = cast(Callable[[str], int], getattr(_history, "seconds"))
re2_escape = cast(Callable[[str], str], getattr(_history, "re2_escape"))
timestamp = cast(Callable[[str], float], getattr(_history, "_ts"))
remote_curl = cast(Callable[[str, list[tuple[str, object]]], list[str]], getattr(_history, "_remote_curl"))
run_command: Run = cast(Run, getattr(_history, "run_command"))
local_mongo_port = cast(Callable[[Mapping[str, str]], str], _history.mongo_local.local_mongo_port)
mirror_database = cast(Callable[[str], str], _history.mongo_local.mirror_database)


def mirror_url(cc: str) -> str:
    """The local mirror's connection string for country `cc`."""
    return MIRROR.format(port=local_mongo_port(os.environ), db=mirror_database(cc))
