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
import tool_countries  # noqa: E402
from tool_cli import UsageError  # noqa: E402

__all__ = ["Options", "UsageError", "parse"]


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
    o, i = Options(), 0
    while i < len(argv):
        if argv[i] in ("-h", "--help"):
            print(usage, end="")
            return None
        i = word(o, argv, i) + 1
    return checked(o)


def word(o: Options, argv: List[str], i: int) -> int:
    """argv[i] read into the options: the index of the last word it took."""
    arg = argv[i]
    if arg.split("=", 1)[0] in ("--country", "--countries"):
        return countries(o, argv, i)
    if arg in VALUED:
        if i + 1 >= len(argv):
            raise UsageError(f"{arg} needs a value")
        valued(o, VALUED[arg], argv[i + 1])
        return i + 1
    if arg in ("--no-wait", "--profile"):
        setattr(o, arg[2:].replace("-", "_"), True)
    elif arg.startswith("-"):
        raise UsageError(f"unknown option {arg}")
    else:
        o.ref = arg
    return i


def countries(o: Options, argv: List[str], i: int) -> int:
    """`--country cc[,cc]` / `--countries a,b` (or `--flag=value`) at argv[i]: the index of its last word."""
    arg = argv[i]
    flag = arg.split("=", 1)[0]
    values, i = ([arg.split("=", 1)[1]], i) if "=" in arg else (argv[i + 1:i + 2], i + 1)
    o.countries = tool_countries.selected(flag, values or [""], o.countries)
    return i


def valued(o: Options, name: str, value: str) -> None:
    if name in ("for_red", "against"):
        value = value[1:] if value.startswith("#") else value
    if name == "timeout":
        if not value.isdigit():
            raise UsageError(f"--timeout takes minutes, not {value}")
        o.timeout_min, o.timeout_set = int(value), True
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
