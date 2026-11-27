#!/usr/bin/env python3
"""THE FAMILY EXPLORER'S WALK ROW (.github/workflows/identity-family-explorer.yml; the owner, 2026-11-06, option A): one
shard of one country's families, walked over the pinned recording the row laid. services.identity.FamilyConvergence
walks every family's POPULATION LATTICE -- every subset of a family of 7 or fewer, the levels 0..L and n-L..n of one of
8..63, seeded arrival orders of a larger one -- through the model AND the final take, and names every listing some
arrival or add/remove sequence takes A -> B -> A (docs/design/identity-no-drift.md, "The family explorer").
The shard is a share of the families BALANCED BY RECORDED COST (family-costs-<cc>.tsv), not by count.
NOT A GATE: it reports flips and fails only when it could not run; no commit is judged by it.

THE CACHE (FAMILY_STORE, a release; empty: nothing stored or read) is one rolling asset a country and shard, under a
rules key that names no code. A night re-walks only the families the code changed since the cache's commit can move
(scripts/ci/family_reach.py: the what-if's rule attribution; anything it cannot attribute -- or no cache commit to
compare with -- walks every family), and FamilyConvergence serves the rest from the cache while their reads' digests
hold. The cache and its commit are stored only after a walk that finished, so a failed night leaves the last good pair.

  scripts/ci/identity-family-nightly.sh <cc> <out dir>
  env: FAMILY_SHARD (`i/N`, default 1/1), FAMILY_LEVELS (default 1), FAMILY_ORDERS (the sampled tier's arrival orders,
       default 8) and FAMILY_ORDER_SALT (default 0: the week's number), FAMILY_STORE, HEAP (sbt -Xmx, default 6g),
       GITHUB_SHA (the commit walked, default HEAD), GH_TOKEN.
Writes <out>/family-flips-<cc>.tsv and family-walks-<cc>.tsv (the walks' seconds a family), sbt.log, report.txt, reach.txt.
Exit: 0 walked, 2 it could not run. The workflow runs it by its shell name; tested by identity_family_nightly_test.py."""
from __future__ import annotations

import gzip
import os
import re
import shutil
import sys
import tempfile
from dataclasses import dataclass
from typing import List, Mapping, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
FIXTURES = os.path.join(ROOT, "test", "resources", "fixtures")
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "lib"))
import machine  # noqa: E402
import tool_git  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402

GH_RELEASE = os.path.join(HERE, "gh-release.sh")
REPORT_LINE = re.compile(r"^## |^  [a-z]+\t")
STORE_NOTES = ("The nightly family explorer's verdict caches (scripts/ci/identity-family-nightly.sh). "
               "A cache, not a version of anything.")


@dataclass(frozen=True)
class Night:
    cc: str
    out: str
    heap: str
    shard: str
    levels: str
    orders: str
    salt: str
    store: str
    head_sha: str

    @property
    def stem(self) -> str:
        index, count = self.shard.split("/")
        return f"family-cache-{self.cc}-{index}of{count}-L{self.levels}"

    def say(self, text: str) -> str:
        return f"family explorer ({self.cc}): {text}"


class Cache:
    """The verdict cache a release stores: an asset gunzipped to a file, or a file gzipped into the release."""

    def __init__(self, night: Night, gh: tool_github.GitHub, runner: tool_process.Runner, env: Mapping[str, str]) -> None:
        self.night = night
        self.gh = gh
        self.runner = runner
        self.env = env

    def fetch(self, asset: str, path: str) -> bool:
        """The asset gunzipped to `path`; False when the store holds none (no store, an unreadable one or a missing
        asset are one answer: nothing cached, every family walks)."""
        store = self.night.store
        if not store:
            return False
        listed = self.gh.read(["release", "view", store, "--json", "assets", "--jq", ".assets[].name"],
                              timeout=120, quiet=True)
        if listed is None or asset not in listed.split("\n"):
            return False
        if not self.gh.succeeds(["release", "download", store, "-p", asset, "-O", path + ".gz", "--clobber"], 300):
            return False
        try:
            with gzip.open(path + ".gz", "rb") as source, open(path, "wb") as target:
                shutil.copyfileobj(source, target)
            os.remove(path + ".gz")
        except OSError:
            return False
        return os.path.getsize(path) > 0

    def _quiet(self, argv: Sequence[str], timeout: float) -> bool:
        return self.runner.run(tool_process.Command(argv, self.env, log=os.devnull, timeout=timeout)) == 0

    def keep(self, path: str, asset: str) -> None:
        """The file stored as `asset`, never failing the walk."""
        store = self.night.store
        if not store or not os.path.isfile(path) or os.path.getsize(path) == 0:
            return
        if not self.gh.succeeds(["release", "view", store], 120):
            self._quiet([GH_RELEASE, "create", store, "--prerelease", "--title", "Identity family explorer store (rolling)",
                         "--notes", STORE_NOTES], 600)
        packed = os.path.join(self.env.get("RUNNER_TEMP") or tempfile.gettempdir(), asset)
        with open(path, "rb") as source, gzip.open(packed, "wb") as target:
            shutil.copyfileobj(source, target)
        if not self._quiet([GH_RELEASE, "upload", store, packed, "--clobber"], 600):
            print(self.night.say(f"could not store {asset} -- a later night walks it again"), flush=True)


class Walk:
    def __init__(self, night: Night, runner: tool_process.Runner, env: Mapping[str, str], cache: Cache) -> None:
        self.night = night
        self.runner = runner
        self.env = env
        self.cache = cache
        self.git = tool_git.Git(runner, env, ROOT)
        self.cache_file = os.path.join(night.out, "cache.json")
        self.sha_file = os.path.join(night.out, "cache.sha")

    def reach_argument(self) -> str:
        """` --reach <file>` when the families to walk again are known; else "" (every family walks)."""
        out, night = self.night.out, self.night
        if not (self.cache.fetch(night.stem + ".json.gz", self.cache_file)
                and self.cache.fetch(night.stem + ".sha.gz", self.sha_file)):
            print(night.say("no verdict cache stored -- every family walks"), flush=True)
            self._forget(self.cache_file, self.sha_file)
            return ""
        with open(self.sha_file) as f:
            cached = "".join(f.read().split())
        # a commit not fetched leaves the reach to say ALL (it cannot compare), so every family walks again
        if not self.git.ok("cat-file", "-e", f"{cached}^{{commit}}", quiet=True):
            self.git.ok("fetch", "-q", "--no-tags", "--depth=1", "origin", cached, timeout=300, quiet=True)
        why = os.path.join(out, "reach-why.txt")
        reached = self.runner.run(tool_process.Command(
            ["python3", os.path.join(HERE, "family_reach.py"), "reach", "--repo", ROOT, "--base", cached,
             "--head", night.head_sha, "--out", os.path.join(out, "reach.txt")], self.env, log=why)) == 0
        if not reached:
            print(night.say(f"the reach of {cached[:12]}..{night.head_sha[:12]} could not be read -- every family walks again"),
                  flush=True)
            self._forget(self.cache_file)
        self._cat(why)
        return f" --reach {os.path.join(out, 'reach.txt')}" if reached else ""

    @staticmethod
    def _forget(*paths: str) -> None:
        for path in paths:
            if os.path.exists(path):
                os.remove(path)

    @staticmethod
    def _cat(path: str) -> None:
        with open(path) as f:
            sys.stdout.write(f.read())
        sys.stdout.flush()

    def sbt(self, reach: str) -> int:
        n, fixtures = self.night, FIXTURES
        run_main = (f"worker/Test/runMain services.identity.FamilyConvergence --corpus {n.cc} --recording {fixtures} "
                    f"--families {fixtures}/identity-unmatched/{n.cc}.json.gz --shard {n.shard} --levels {n.levels} "
                    f"--orders {n.orders} --order-salt {n.salt} --costs {fixtures}/family-costs-{n.cc}.tsv "
                    f"--rules family-explorer-L{n.levels} --cache {self.cache_file}{reach} --out {n.out}")
        return self.runner.run(tool_process.Command(
            ["sbt", f"-J-Xmx{n.heap}", "set worker / Test / run / fork := false", run_main], self.env,
            log=os.path.join(n.out, "sbt.log"), cwd=ROOT))

    def report(self, walked: int) -> Optional[List[str]]:
        """The report's `## ` lines; None (said, the log's tail shown) when the walk wrote no verdict."""
        log = os.path.join(self.night.out, "sbt.log")
        with open(log, errors="replace") as f:
            lines = f.read().splitlines()
        kept = [line for line in lines if REPORT_LINE.match(line)]
        with open(os.path.join(self.night.out, "report.txt"), "w") as f:
            f.writelines(line + "\n" for line in kept)
        flips = os.path.join(self.night.out, f"family-flips-{self.night.cc}.tsv")
        if not os.path.isfile(flips) or not any(line.startswith(f"## {self.night.cc} family check: ") for line in kept):
            sys.stderr.write("\n".join(lines[-40:]) + "\n")
            print(f"family explorer ({self.night.cc}, shard {self.night.shard}): no verdict (sbt exit {walked})",
                  file=sys.stderr)
            return None
        return [line for line in kept if line.startswith("## ")]

    def run(self) -> int:
        verdict = self.report(self.sbt(self.reach_argument()))
        if verdict is None:
            return 2
        print("\n".join(verdict), flush=True)
        with open(self.sha_file, "w") as f:
            f.write(self.night.head_sha + "\n")
        self.cache.keep(self.cache_file, self.night.stem + ".json.gz")
        self.cache.keep(self.sha_file, self.night.stem + ".sha.gz")
        self._forget(self.cache_file)  # the cache is a store's, not an artifact
        return 0


def refusal(cc: str, shard: str) -> Optional[str]:
    """Why the night cannot run, or None."""
    if not re.fullmatch(r"[0-9]+/[0-9]+", shard):
        return f"family explorer ({cc}): FAMILY_SHARD '{shard}' is not i/N"
    if not os.path.isfile(os.path.join(FIXTURES, "corpus", f"cinema-scrapes-{cc}.json.gz")):
        return f"family explorer ({cc}): no recorded corpus in {FIXTURES}/corpus"
    return None


def night_of(cc: str, out: str, env: Mapping[str, str], head: str) -> Night:
    return Night(cc, out, env.get("HEAP") or "6g", env.get("FAMILY_SHARD") or "1/1", env.get("FAMILY_LEVELS") or "1",
                 env.get("FAMILY_ORDERS") or "8", env.get("FAMILY_ORDER_SALT") or "0", env.get("FAMILY_STORE") or "", head)


def main(argv: Sequence[str], env: Mapping[str, str]) -> int:
    if len(argv) < 2:
        print("usage: identity-family-nightly.sh <cc> <out dir>", file=sys.stderr)
        return 2
    cc, out = argv[0], argv[1]
    runner = tool_process.SubprocessRunner()
    os.makedirs(out, exist_ok=True)
    why = refusal(cc, env.get("FAMILY_SHARD") or "1/1")
    if why:
        print(why, file=sys.stderr)
        return 2
    head = env.get("GITHUB_SHA") or (tool_git.Git(runner, env, ROOT).out("rev-parse", "HEAD") or "").strip()
    night = night_of(cc, out, env, head)
    return Walk(night, runner, env, Cache(night, tool_github.GitHub(runner, env), runner, env)).run()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(machine.environment())))
