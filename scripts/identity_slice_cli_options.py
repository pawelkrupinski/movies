"""identity-slice's command line (scripts/identity_slice_cli.py): the options after the country, read as the shell tool
read them -- a flag's value checked there (tool-conventions.sh's flag_value), a path made absolute while the caller's
directory is the current one, a directory that is not there refused (path_flag) -- each mistake a usage error, exit 2."""
from __future__ import annotations

import os
import re
from dataclasses import dataclass, field
from typing import Callable, Dict, List, Mapping, Tuple

TOOL = "identity-slice"


class Usage(Exception):
    """Asked wrongly: the message on stderr, with the tool's header after it when `usage`, then exit 2."""

    def __init__(self, message: str, usage: bool = False) -> None:
        super().__init__(message)
        self.usage = usage


def usage_error(message: str) -> Usage:
    """tool-conventions.sh's usage_error: the message and a pointer to --help."""
    return Usage(f"{TOOL}: {message}\n{TOOL}: see {TOOL} --help")


@dataclass
class Options:
    cc: str = ""
    refs: List[str] = field(default_factory=lambda: [])
    run: str = ""
    passed: List[str] = field(default_factory=lambda: [])  # handed to the JVM as they are
    batch: str = ""
    bisect: str = ""
    log: str = ""
    faith: str = ""
    sample: str = "50"
    seed: str = "1"
    families: str = ""
    threads: str = ""
    fresh: bool = False
    serve: str = "0"
    explain: bool = False
    drift: bool = False
    fixture: str = ""
    keep: str = ""
    mirror: str = ""
    labels: str = ""
    emit_drift: bool = False
    fills: str = ""  # IDENTITY_FILLS, handed to identity-tools.sh's recording functions


def logical_cwd(env: Mapping[str, str]) -> str:
    """The shell's `pwd`: $PWD while it names the current directory, else the physical one."""
    pwd = env.get("PWD", "")
    try:
        if pwd and os.path.samefile(pwd, "."):
            return pwd
    except OSError:
        pass
    return os.getcwd()


def dirname(path: str) -> str:
    """POSIX `dirname`."""
    stripped = path.rstrip("/")
    if not stripped:
        return "/" if path else "."
    return os.path.dirname(stripped).rstrip("/") or ("/" if stripped.startswith("/") else ".")


def path_flag(flag: str, value: str, env: Mapping[str, str]) -> str:
    """A path a tool hands to a JVM, absolute: a directory that is not there is the caller's mistake."""
    directory = dirname(value)
    full = os.path.normpath(os.path.join(logical_cwd(env), directory))
    if not os.path.isdir(full):
        raise usage_error(f"{flag} {value}: no directory {directory}")
    return f"{full}/{os.path.basename(value.rstrip('/')) or '/'}"


Setter = Callable[[Options, str], None]
# the flags taking a value, each what it sets
VALUED: Dict[str, Setter] = {
    "--ref": lambda o, v: o.refs.append(v),
    "--recording": lambda o, v: setattr(o, "run", v),
    "--fills": lambda o, v: setattr(o, "fills", v),
    "--bisect": lambda o, v: setattr(o, "bisect", v),
    "--log": lambda o, v: setattr(o, "log", v),
    "--faithfulness": lambda o, v: setattr(o, "faith", v),
    "--sample": lambda o, v: setattr(o, "sample", v),
    "--seed": lambda o, v: setattr(o, "seed", v),
    "--threads": lambda o, v: setattr(o, "threads", v),
    "--fixture": lambda o, v: setattr(o, "fixture", v),
}
PASSED_VALUED = ("--venue", "--title", "--key", "--widen", "--orders", "--modes", "--limit")
PATHS = {"--batch": "batch", "--out": "keep", "--labels": "labels"}
SWITCHES: Dict[str, Setter] = {
    "--fresh": lambda o, _: setattr(o, "fresh", True),
    "--serve": lambda o, _: setattr(o, "serve", "1"),
    "--explain": lambda o, _: setattr(o, "explain", True),
    "--drift": lambda o, _: setattr(o, "drift", True),
    "--no-resolve": lambda o, _: o.passed.append("--no-resolve"),
}


class Parser:
    def __init__(self, env: Mapping[str, str], repo: str) -> None:
        self.env, self.repo = env, repo

    def parse(self, argv: List[str]) -> Options:
        o = Options()
        if argv[0] != "--log":
            o.cc, argv = argv[0].lower(), argv[1:]
        while argv:
            argv = self.one(o, argv)
        return o

    def one(self, o: Options, argv: List[str]) -> List[str]:
        """The first option of `argv` read into `o`: what is left."""
        flag = argv[0]
        if flag in SWITCHES:
            SWITCHES[flag](o, "")
            return argv[1:]
        if flag in ("--emit-case", "--emit-drift-slice", "--prod-mirror"):
            return self.special(o, argv)
        if flag not in VALUED and flag not in PASSED_VALUED and flag not in PATHS and flag != "--families":
            raise Usage(f"unknown argument {flag}", usage=True)
        if len(argv) < 2:
            raise usage_error(f"{flag} needs a value")
        self.valued(o, flag, argv[1])
        return argv[2:]

    def valued(self, o: Options, flag: str, value: str) -> None:
        if flag in VALUED:
            VALUED[flag](o, value)
        elif flag in PASSED_VALUED:
            o.passed += [flag, value]
        elif flag == "--families":
            o.families = "none" if value == "none" else path_flag(flag, value, self.env)
        else:
            setattr(o, PATHS[flag], path_flag(flag, value, self.env))

    def special(self, o: Options, argv: List[str]) -> List[str]:
        flag = argv[0]
        if flag == "--emit-case":
            # through a kept JVM: pinning cases one after another reads the recording and the index once
            # (KINOWO_IDENTITY_SLICE_EMIT_ROOT pins elsewhere: scripts/identity-slice-refresh's scratch root)
            root = self.env.get("KINOWO_IDENTITY_SLICE_EMIT_ROOT") or f"{self.repo}/test/resources/fixtures/identity-slices"
            o.passed += ["--emit-case", root]
            o.serve = self.env.get("KINOWO_IDENTITY_SLICE_EMIT_SERVE") or "1"
        elif flag == "--emit-drift-slice":
            o.passed += ["--emit-slice", f"{self.repo}/test/resources/fixtures/drift-slices"]
            o.emit_drift = True
        else:
            o.mirror, more = mirror_country(argv[1:])
            return more
        return argv[1:]


def mirror_country(rest: List[str]) -> Tuple[str, List[str]]:
    """--prod-mirror [cc]: the country when the next word is one, else `on`; what is left."""
    if rest and re.fullmatch(r"[A-Za-z]{2}", rest[0]):
        return rest[0].lower(), rest[1:]
    return "on", rest
