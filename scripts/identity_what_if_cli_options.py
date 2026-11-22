"""identity-what-if's command line (scripts/identity_what_if_cli.py): every flag the shell tool took, its defaults, and
its usage errors -- `<flag> needs a value` (tool-conventions.sh's flag_value), `--bisect GOOD BAD`, an unknown
argument with the usage after it, no country named -- each exit 2."""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Mapping

TOOL = "identity-what-if"


class Usage(Exception):
    """Asked wrongly: the message on stderr (and the usage after it, with `usage`), exit 2."""

    def __init__(self, message: str, usage: bool = False) -> None:
        super().__init__(message)
        self.usage = usage


class Help(Exception):
    """-h / --help: the usage on stdout, exit 0."""


@dataclass
class Options:
    argv: List[str]
    base: str = "origin/main"
    head: str = "."
    at: str = ""
    max_age: str = "6"
    all: str = "auto"  # auto, 1 (--all) or 0 (--selection)
    verify: bool = False
    threads: str = "6"
    out: str = ""
    json: bool = False
    judgements: str = ""
    answers: str = "live"  # live, offline (--offline) or, without a TMDB key, cached
    answers_dir: str = ""
    final: bool = True
    serve: bool = True
    bisect_good: str = ""
    bisect_bad: str = ""
    listing: str = ""
    since: str = ""
    recording: str = ""
    ccs: List[str] = field(default_factory=lambda: [])


VALUED = {"--base": "base", "--head": "head", "--at": "at", "--max-age": "max_age", "--threads": "threads",
          "--out": "out", "--judgements": "judgements", "--answers-dir": "answers_dir", "--listing": "listing",
          "--since": "since", "--recording": "recording"}
SWITCHES: Dict[str, Dict[str, object]] = {
    "--all": {"all": "1"}, "--selection": {"all": "0"}, "--verify": {"verify": True}, "--json": {"json": True},
    "--offline": {"answers": "offline"}, "--model-only": {"final": False}, "--no-serve": {"serve": False}}


def parse(argv: List[str], env: Mapping[str, str]) -> Options:
    o = Options(argv=list(argv), serve=(env.get("KINOWO_IDENTITY_WHAT_IF_SERVE") or "1") == "1")
    i = 0
    while i < len(argv):
        i += take(o, argv, i)
    if not o.ccs:
        raise Usage("name a country (pl de uk es us)")
    return o


def take(o: Options, argv: List[str], i: int) -> int:
    """The argument at i read into `o`; how many it took."""
    arg = argv[i]
    if arg in VALUED:
        if i + 1 >= len(argv):
            raise Usage(f"{TOOL}: {arg} needs a value\n{TOOL}: see {TOOL} --help")
        setattr(o, VALUED[arg], argv[i + 1])
        return 2
    if arg in SWITCHES:
        o.__dict__.update(SWITCHES[arg])
        return 1
    return positional(o, argv, i)


def positional(o: Options, argv: List[str], i: int) -> int:
    arg = argv[i]
    if arg == "--bisect":
        if i + 2 >= len(argv):
            raise Usage("--bisect GOOD BAD")
        o.bisect_good, o.bisect_bad = argv[i + 1], argv[i + 2]
        return 3
    if arg in ("-h", "--help"):
        raise Help()
    if arg.startswith("-"):
        raise Usage(f"unknown argument {arg}", usage=True)
    o.ccs.append(arg.lower())
    return 1
