"""GITHUB THROUGH `gh`, the one way the Python tools ask it (scripts/lib): every call bounded (a hung API read is no
answer, never a step waiting out its job's timeout), its stderr passed on unless asked to be quiet, and a read that
fails told apart from one that answered -- `read` gives None for a failed call, never an empty answer.

  GitHub(runner, env).read(["api", "repos/o/r/commits/x", "--jq", ".sha"], timeout=120) -> stdout, or None
  .json(["api", ...], timeout, quiet=True)                                               -> the parsed answer, or None when gh failed or said no JSON
  GitHub.here()                                                                          -> over this process's one subprocess runner and its environment
  .succeeds([...], timeout, quiet=True)                                                  -> whether gh exited 0
  .download(release, asset, dest, timeout)  `gh release download` of one asset to `dest`: whether it is there now
  .asset_names(release, timeout)            the release's asset names, or None when they could not be listed
  .fetch_gzipped(release, asset, path, timeout)  a gzipped asset unpacked into `path`: whether it is there, non-empty

THE FAKE (CLAUDE.md, "Side effects live at the leaves"): `GitHub(InMemoryGh(api=..., releases=...), {})` -- every rule
above is GitHub's own, shared; only the boundary differs. InMemoryGh is a runner answering `gh` as GitHub would from
what it holds: `api <path> [--jq <path>]` (a JSON value per path; the jq paths the tools use: `.a.b`, `.[]`,
`.a[].b`), `release view <tag> --json assets`, `release download <tag> -p <asset> -O <dest>`; anything else exits 1
saying it is not held. tool_github_contract_test.py holds GitHub alike over it and over a real `gh` process."""
from __future__ import annotations

import gzip
import os
import shutil
import json
import sys
from typing import Any, Dict, List, Mapping, Optional, Sequence, Tuple, cast

import atomic
import tool_git
import tool_process


class GitHub:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], gh: str = "gh") -> None:
        self.runner = runner
        self.env = env
        self.gh = gh

    @classmethod
    def here(cls) -> "GitHub":
        return cls(tool_git.shared_runner(), os.environ, os.environ.get("KINOWO_GH") or "gh")

    def call(self, args: Sequence[str], timeout: float, quiet: bool = False, input: Optional[str] = None,
             closed_stdin: bool = False) -> tool_process.Captured:
        """`gh args`, bounded by `timeout`; `input` is what its stdin reads, `closed_stdin` a stdin that reads nothing."""
        done = self.runner.capture(tool_process.Command([self.gh, *args], self.env, timeout=timeout, input=input,
                                                        closed_stdin=closed_stdin))
        if done.stderr and not quiet:
            sys.stderr.write(done.stderr)
            sys.stderr.flush()
        return done

    def read(self, args: Sequence[str], timeout: float, quiet: bool = False) -> Optional[str]:
        done = self.call(args, timeout, quiet)
        return done.stdout if done.code == 0 else None

    def json(self, args: Sequence[str], timeout: float, quiet: bool = False, closed_stdin: bool = False) -> Any:
        """What gh answered, parsed: None when the call failed or its answer is no JSON."""
        done = self.call(args, timeout, quiet, closed_stdin=closed_stdin)
        if done.code != 0:
            return None
        try:
            return json.loads(done.stdout)
        except ValueError:
            return None

    def succeeds(self, args: Sequence[str], timeout: float, quiet: bool = True) -> bool:
        return self.call(args, timeout, quiet).code == 0

    def download(self, release: str, asset: str, dest: str, timeout: float) -> bool:
        """One release asset into `dest` (replaced): False when absent or the download failed, quietly."""
        os.makedirs(os.path.dirname(dest) or ".", exist_ok=True)
        return self.succeeds(["release", "download", release, "-p", asset, "-O", dest, "--clobber"], timeout) \
            and os.path.isfile(dest)

    def asset_names(self, release: str, timeout: float, quiet: bool = False) -> Optional[List[str]]:
        """Every asset name of `release`; None when the listing failed -- never an empty release."""
        listed = self.read(["release", "view", release, "--json", "assets", "--jq", ".assets[].name"], timeout, quiet)
        return None if listed is None else [name for name in listed.split("\n") if name]

    def fetch_gzipped(self, release: str, asset: str, path: str, timeout: float) -> bool:
        """The gzipped `asset` downloaded beside `path` (`path`.gz) and unpacked into it, quietly: False when absent,
        failed, or not a whole gzip (a truncated download is no result)."""
        packed = path + ".gz"
        if not self.download(release, asset, packed, timeout):
            return False
        try:
            with gzip.open(packed, "rb") as source, open(path, "wb") as plain:
                shutil.copyfileobj(source, plain)
        except (OSError, EOFError):
            return False
        os.remove(packed)
        return os.path.getsize(path) > 0


def jq_step(values: List[Any], step: str, path: str) -> List[Any]:
    if step == "[]":
        return [item for v in values for item in (cast(List[Any], v) if isinstance(v, list)
                                                  else list(cast(Dict[str, Any], v).values()) if isinstance(v, dict) else [])]
    if not step.replace("_", "").replace("-", "").isalnum():
        raise ValueError(f"jq path {path!r}: only .key and [] steps are held")
    return [cast(Dict[str, Any], v).get(step) if isinstance(v, dict) else None for v in values]


def jq_path(value: Any, path: str) -> List[Any]:
    """The values a jq path of `.key` and `[]` steps (`.assets[].name`) reads from `value`; ValueError for any other."""
    out: List[Any] = [value]
    for step in path.replace("[]", ".[]").split(".")[1:]:
        if step:
            out = jq_step(out, step, path)
    return out


def jq_text(values: List[Any]) -> str:
    """gh --jq's output: a string raw, anything else as JSON, one per line."""
    return "".join((v if isinstance(v, str) else json.dumps(v)) + "\n" for v in values)


class InMemoryGh:
    """A runner answering `gh` from memory: `api` paths -> JSON values, releases -> {asset name: bytes}."""

    def __init__(self, api: Optional[Mapping[str, Any]] = None,
                 releases: Optional[Mapping[str, Mapping[str, bytes]]] = None) -> None:
        self.api: Dict[str, Any] = dict(api or {})
        self.releases: Dict[str, Dict[str, bytes]] = {k: dict(v) for k, v in (releases or {}).items()}
        self.asked: List[List[str]] = []  # every gh argv it answered

    def capture(self, command: tool_process.Command) -> tool_process.Captured:
        self.asked.append(list(command.argv))
        return tool_process.Captured(*self.answer(list(command.argv)[1:]))

    def run(self, command: tool_process.Command) -> int:
        return self.capture(command).code

    def answer(self, args: List[str]) -> Tuple[int, str, str]:
        """(exit code, stdout, stderr) of `gh args`."""
        jq = args[args.index("--jq") + 1] if "--jq" in args else None
        if args[:1] == ["api"] and len(args) > 1:
            return self.api_answer(args[1], jq)
        if args[:2] in (["release", "view"], ["release", "download"]) and len(args) > 2:
            assets = self.releases.get(args[2])
            if assets is None:
                return 1, "", "release not found\n"
            return self.view(assets, jq) if args[1] == "view" else self.download(assets, args)
        return 1, "", f"InMemoryGh: `gh {' '.join(args)}` is not held\n"

    def api_answer(self, path: str, jq: Optional[str]) -> Tuple[int, str, str]:
        if path not in self.api:
            return 1, "", "gh: Not Found (HTTP 404)\n"
        value = self.api[path]
        return 0, jq_text(jq_path(value, jq)) if jq else json.dumps(value) + "\n", ""

    @staticmethod
    def view(assets: Mapping[str, bytes], jq: Optional[str]) -> Tuple[int, str, str]:
        value = {"assets": [{"name": name, "size": len(data)} for name, data in sorted(assets.items())]}
        return 0, jq_text(jq_path(value, jq)) if jq else json.dumps(value) + "\n", ""

    @staticmethod
    def download(assets: Mapping[str, bytes], args: List[str]) -> Tuple[int, str, str]:
        name, dest = args[args.index("-p") + 1], args[args.index("-O") + 1]
        if name not in assets:
            return 1, "", f"no assets match the file pattern {name}\n"
        atomic.write_bytes(dest, assets[name])  # gh writes the file: the boundary's one effect
        return 0, "", ""
