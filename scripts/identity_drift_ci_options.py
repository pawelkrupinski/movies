"""identity-drift-ci's command line (scripts/identity_drift_ci.py, whose header is the usage): the options, checked --
a usage error is the tool's exit 2 (tool-conventions.sh's usage_error)."""
from __future__ import annotations

import os
import sys
from dataclasses import dataclass, field
from typing import List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "ci"))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import drift_seeds  # noqa: E402
from cli import tool_cli  # noqa: E402
from cli import tool_countries  # noqa: E402
from cli.tool_cli import UsageError, usage_failed  # noqa: E402

__all__ = ["Options", "UsageError", "parse", "usage_failed"]


@dataclass
class Options:
    ref: str = "HEAD"
    seeds: str = "both"
    for_red: str = ""
    against: str = ""
    lane: str = ""
    timeout_min: int = 75
    timeout_set: bool = False
    out: str = ""
    no_wait: bool = False
    run: str = ""
    profile: bool = False
    countries: List[str] = field(default_factory=lambda: [])
    pins: str = ""


VALUED = {"--head": "ref", "--seeds": "seeds", "--for-red": "for_red", "--against": "against", "--timeout": "timeout",
          "--out": "out", "--lane": "lane", "--run": "run"}


def parse(argv: List[str], usage: str) -> Optional[Options]:
    """The options, checked; None for --help (`usage` said)."""
    o = tool_cli.read_options(Options(), argv, usage, word)
    return None if o is None else checked(o)


def word(o: Options, argv: List[str], i: int) -> int:
    """argv[i] read into the options: the index of the last word it took."""
    arg = argv[i]
    if arg.split("=", 1)[0] in ("--country", "--countries"):
        o.countries, i = tool_countries.flag_at(argv, i, o.countries)
        return i
    if arg in VALUED:
        valued(o, VALUED[arg], tool_cli.value_at(argv, i))
        return i + 1
    if arg in ("--no-wait", "--profile"):
        setattr(o, arg[2:].replace("-", "_"), True)
    elif arg.startswith("-"):
        raise UsageError(f"unknown option {arg}")
    else:
        o.ref = arg
    return i


def valued(o: Options, name: str, value: str) -> None:
    if name in ("for_red", "against"):
        value = value[1:] if value.startswith("#") else value
    if name == "timeout":
        o.timeout_min, o.timeout_set = tool_cli.minutes("--timeout", value), True
    else:
        setattr(o, name, value)


def checked(o: Options) -> Options:
    if o.seeds != "both":
        try:
            one, two = drift_seeds.parse_pins(o.seeds)
        except ValueError:
            raise UsageError(f"--seeds takes S1,S2, S1, ,S2 or both, not {o.seeds}") from None
        o.pins = f"{one or ''},{two or ''}"
    if o.for_red and o.against:
        raise UsageError("--for-red and --against each name the baseline: one of them")
    o.lane = o.lane or ("red-fix" if o.for_red else "landing")
    if o.lane not in ("red-fix", "landing", "measure"):
        raise UsageError(f"--lane takes red-fix, landing or measure, not {o.lane}")
    return o
