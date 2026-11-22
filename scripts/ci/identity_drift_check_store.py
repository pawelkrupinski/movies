"""THE DRIFT CHECK'S KEYS AND STORE (identity_drift_check.py, "THE STORE" and "THE HEAD'S REPLAY IS A FUNCTION OF ITS
INPUTS" in its header): what a replay reads at a commit (`replay_tree_of`), the recording's key, the asset names, and the
release a run reads results from and stores them on -- never failing the check."""
from __future__ import annotations

import gzip
import hashlib
import os
import shutil
from dataclasses import dataclass
from typing import List, Mapping, Optional, Sequence

import tool_process

# The top-level directories no build of worker/Test reads (infra and scripts each keep a part: below)
OUTSIDE = {"docs", ".claude", ".github", "web", "e2e", "ios", "android", "page-tests-playwright", "eslint-rules", "tools"}
CHECK = "scripts/ci/identity-drift-check.sh"


def sha24(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8", "surrogateescape")).hexdigest()[:24]


def lines_key(*lines: str) -> str:
    """`printf '%s\\n' <lines> | sha256sum | cut -c1-24`."""
    return sha24("".join(f"{line}\n" for line in lines))


def is_the_check(path: str) -> bool:
    """The check's own files: its entry and its Python modules (not their self-tests)."""
    if path == CHECK:
        return True
    name = os.path.basename(path)
    return path.startswith("scripts/ci/identity_drift_") and name.endswith(".py") and not name.endswith("_test.py")


def replay_input(path: str) -> bool:
    """Whether a tracked path is an input of the replay: the whole tree but what no build of worker/Test reads -- an
    exclusion list, never an inclusion list, so a path added anywhere else costs a replay, never a stale reading."""
    top = path.split("/", 1)[0]
    if path == top:
        return path.endswith(".sbt") or path in (".jvmopts", ".sbtopts")
    if top in OUTSIDE:
        return False
    if top == "infra":
        return path.startswith("infra/jvm/")  # build.sbt reads infra/jvm/
    if top == "scripts":
        return is_the_check(path)
    return True


@dataclass
class Keys:
    """The replay's inputs at a commit, and the recording's key; assets named by them."""
    root: str
    cc: str
    runner: tool_process.Runner
    env: Mapping[str, str]
    recording: str = ""

    def git(self, *args: str) -> tool_process.Captured:
        return self.runner.capture(tool_process.Command(["git", "-C", self.root, *args], self.env))

    def code_of(self, sha: str) -> str:
        listed = self.git("-c", "core.quotePath=false", "ls-tree", "-r", sha).stdout
        kept = [line for line in listed.split("\n") if line and replay_input(line.split("\t", 1)[-1])]
        return lines_key(*kept)

    def recording_key(self, fixtures: str) -> str:
        parts = "".join(f"{v}\n" for v in (self.cc, *(self.env.get(f"KINOWO_CONVERGENCE_{k}", "") for k in
                                                       ("PIN_CORPUS_RUN", "TREE_ASSET", "FILL_ASSETS"))))
        data = parts.encode()
        cc = self.cc
        for path in (f"{fixtures}/corpus/cinema-scrapes-{cc}.json.gz", f"{fixtures}/identity-unmatched/{cc}.json.gz",
                     f"{fixtures}/identity-unmatched/{cc}.inputs"):
            if os.path.isfile(path):
                with open(path, "rb") as f:
                    data += f.read()
        data += "".join(f"{line}\n" for line in enrichment_listing(fixtures, self.cc)).encode()
        self.recording = hashlib.sha256(data).hexdigest()[:24]
        return self.recording

    def asset(self, kind: str, code: str, seed: str) -> str:
        return f"drift-{kind}-{self.cc}-{seed}-{lines_key(code, self.recording, seed)}.tsv.gz"


def enrichment_listing(fixtures: str, cc: str) -> List[str]:
    """`find enrichment-<cc> -type f -printf '%P %s\\n' | LC_ALL=C sort`, from the fixtures directory."""
    top = os.path.join(fixtures, f"enrichment-{cc}")
    found: List[str] = []
    for directory, _, files in os.walk(top):
        for name in files:
            path = os.path.join(directory, name)
            found.append(f"{os.path.relpath(path, top)} {os.path.getsize(path)}")
    return sorted(found, key=lambda s: s.encode())


class Store:
    """The release results are read from and stored on (DRIFT_STORE; empty: nothing stored or read)."""

    def __init__(self, release: str, root: str, cc: str, runner: tool_process.Runner, env: Mapping[str, str]) -> None:
        self.release = release
        self.root = root
        self.cc = cc
        self.runner = runner
        self.env = env
        self.names: List[str] = []

    def gh(self, args: Sequence[str], timeout: float) -> tool_process.Captured:
        return self.runner.capture(tool_process.Command(["gh", *args], self.env, timeout=timeout))

    def list(self, assets_file: str) -> None:
        """The release's asset names into `assets_file`: an unreadable store is an empty one (every base replayed)."""
        self.names = []
        if self.release:
            done = self.gh(["release", "view", self.release, "--json", "assets", "--jq", ".assets[].name"], 120)
            self.names = done.stdout.split("\n") if done.code == 0 else []
        with open(assets_file, "w") as f:
            f.write("\n".join(n for n in self.names if n) + ("\n" if any(self.names) else ""))

    def stored(self, asset: str) -> bool:
        return asset in self.names

    def fetch(self, asset: str, path: str) -> bool:
        """Whether the stored asset was fetched and unpacked into `path` (non-empty)."""
        if not self.release or not self.stored(asset):
            return False
        done = self.gh(["release", "download", self.release, "-p", asset, "-O", path + ".gz", "--clobber"], 300)
        if done.code != 0:
            return False
        try:
            with gzip.open(path + ".gz", "rb") as packed, open(path, "wb") as plain:
                shutil.copyfileobj(packed, plain)
            os.remove(path + ".gz")
        except (OSError, EOFError):
            return False
        return os.path.getsize(path) > 0

    def script(self, name: str, *args: str, timeout: Optional[float] = None) -> int:
        command = [os.path.join(self.root, "scripts/ci", name), *args]
        return self.runner.capture(tool_process.Command(command, self.env, timeout=timeout)).code

    def keep(self, path: str, asset: str) -> None:
        """Store a result -- room made first (release-store-prune.sh: GitHub's 1000-asset cap) -- never failing."""
        if not self.release or not os.path.isfile(path) or os.path.getsize(path) == 0 or self.stored(asset):
            return
        if self.gh(["release", "view", self.release], 120).code != 0:
            self.script("gh-release.sh", "create", self.release, "--prerelease", "--title", "Identity drift store (rolling)",
                        "--notes", "The drift gate's flips per seed, keyed by identity code, recording and seed "
                        "(scripts/ci/identity-drift-check.sh). A cache, not a version of anything.")
        kind = asset[len("drift-"):].split("-", 1)[0]
        self.runner.run(tool_process.Command([os.path.join(self.root, "scripts/ci/release-store-prune.sh"), self.release,
                                              f"drift-{kind}-{self.cc}-", "48",
                                              self.env.get("DRIFT_STORE_ASSETS") or "900"], self.env))
        packed = os.path.join(self.env.get("RUNNER_TEMP") or "/tmp", asset)
        with open(path, "rb") as plain, gzip.open(packed, "wb") as out:
            shutil.copyfileobj(plain, out)
        if self.script("gh-release.sh", "upload", self.release, packed, "--clobber") != 0:
            print(f"::warning::identity drift ({self.cc}): could not store {asset} -- a later run resolves that base again",
                  flush=True)
