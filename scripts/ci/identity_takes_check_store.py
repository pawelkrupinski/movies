"""THE STORE of the identity takes check (identity_takes_check.py): a resolve is a pure function of its commit and its
inputs, so each side's decisions, final takes, corpus-shape findings and normalizer traces are stored on the TAKES_STORE
release, KEYED by the commit's identity code (not its sha: a commit touching none of it is then the base of the next run
under the key its last identity change stored) and every input it read -- the pinned corpus, its tree and fills, the fill
this row laid, and for the final takes the capture they replay. A stored key is downloaded, not compiled and resolved;
a fill laid but never published, or no recording pinned, names no key.

A head's results are stored only on main (the only commits a later base is); a BASE's on any ref (a measure/ branch's
base is main's commit). Room is made first (release-store-prune.sh: GitHub refuses uploads past 1000 assets), and an
upload refused because another run stored the asset meanwhile is the race lost to an equal result, no warning.
THE BASE ROW (TAKES_SIDE=head): a result the run's `takes-base` row stores meanwhile is waited for while that row runs."""
from __future__ import annotations

import gzip
import hashlib
import os
import shutil
from typing import Callable, Iterable, Optional, Tuple

import identity_takes_check_context as ctx_module
import identity_takes_check_paths as paths
import tool_process

Context = ctx_module.Context


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()[:24]


def lines(*items: str) -> bytes:
    return "".join(f"{i}\n" for i in items).encode()


class Keys:
    """The keys a side's results are stored under."""

    def __init__(self, ctx: Context, laid: str) -> None:
        self.ctx = ctx
        self.laid = laid
        self.pin = ctx.env.get("KINOWO_CONVERGENCE_PIN_CORPUS_RUN", "")
        self.named = ctx.env.get("KINOWO_CONVERGENCE_NAMED_RECORDING", "")

    def tree_hash(self, sha: str, under: Iterable[str]) -> str:
        listed = self.ctx.runner.capture(tool_process.Command(["git", "ls-tree", "-r", sha, "--", *under], self.ctx.env))
        return sha256(listed.stdout.encode())

    def code_of(self, sha: str) -> str:
        return self.tree_hash(sha, paths.IDENTITY_PATHS)

    def decisions(self, sha: str) -> str:
        env = self.ctx.env
        return sha256(lines(self.code_of(sha), self.ctx.cc, self.pin, env.get("KINOWO_CONVERGENCE_TREE_ASSET", ""),
                            env.get("KINOWO_CONVERGENCE_FILL_ASSETS", ""), self.laid))

    def capture(self, sha: str) -> str:
        data = lines(self.code_of(sha), self.ctx.cc)
        for suffix in (".json.gz", ".inputs"):
            path = self.ctx.fixtures("identity-unmatched", self.ctx.cc + suffix)
            if os.path.isfile(path):
                with open(path, "rb") as f:
                    data += f.read()
        return sha256(data)

    def shape(self, sha: str) -> str:
        return sha256(lines(self.tree_hash(sha, paths.SHAPE_PATHS), self.ctx.cc, self.pin))

    def asset(self, kind: str, key: str) -> str:
        """None ("") for a result over the recording when none is pinned: its key would name no data."""
        if kind not in ("final", "gaps") and not self.pin:
            return ""
        return f"takes-{kind}-{self.ctx.cc}-{key}.gz"

    def trace(self, sha: str) -> str:
        """A side's normalizer trace, named so the laptop finds it: its identity code and the recording."""
        if self.named:
            return f"takes-trace-{self.ctx.cc}-{self.code_of(sha)}-n{self.named}.gz"
        return f"takes-trace-{self.ctx.cc}-{self.code_of(sha)}-p{self.pin}.gz" if self.pin else ""

    def exact_trace(self, sha: str) -> str:
        """The trace under the side's EXACT inputs, for the skip gate; none without a pinned recording."""
        decisions = self.asset("decisions", self.decisions(sha))
        return f"takes-tracex-{self.ctx.cc}-{sha256(lines(decisions, self.capture(sha)))}.gz" if decisions else ""


class Store:
    def __init__(self, ctx: Context, store: str, keys: Keys, side: str) -> None:
        self.ctx = ctx
        self.store = store
        self.keys = keys
        self.side = side
        self.gh_read = ctx.seconds("IDENTITY_TAKES_GH_TIMEOUT", "120")
        self.gh_download = ctx.seconds("IDENTITY_TAKES_DOWNLOAD_TIMEOUT", "600")
        self.made = False
        self.how = "reused"

    def fetch(self, asset: str, path: str) -> bool:
        """Whether the stored result was fetched into `path`, quietly."""
        if not self.store or not asset:
            return False
        if not self.ctx.gh.download(self.store, asset, path + ".gz", self.gh_download):
            return False
        try:
            with gzip.open(path + ".gz", "rb") as packed, open(path, "wb") as plain:
                shutil.copyfileobj(packed, plain)
        except OSError:
            return False
        os.remove(path + ".gz")
        return ctx_module.nonempty(path)

    def reused(self, asset: str, path: str, what: str) -> bool:
        """Whether a stored result was fetched; says why not when it was not."""
        self.how = "reused"
        if self.fetch(asset, path):
            return True
        if not self.store:
            why = "no store (TAKES_STORE empty, or a fill laid unpublished)"
        elif not asset:
            why = "no key (no recording pinned)"
        else:
            why = f"{asset} not on release {self.store}"
        self.ctx.say(f"{self.ctx.label}: {what} not stored -- {why}: resolving it")
        return False

    def awaited(self, asset: str, path: str, what: str) -> bool:
        """`reused`, or -- beside a base row -- stored by it meanwhile, waited for while that row runs."""
        if self.side != "head" or not self.store or not asset:
            return self.reused(asset, path, what)
        return BaseRowWait(self, asset, path, what).wait()

    def release(self) -> None:
        """The store's release, made by the first result stored on it (rows racing all find it there after)."""
        if self.made:
            return
        if not self.ctx.gh.succeeds(["release", "view", self.store], self.gh_read):
            self.ctx.runner.capture(tool_process.Command(
                [os.path.join(self.ctx.here, "scripts/ci/gh-release.sh"), "create", self.store, "--prerelease", "--title",
                 "Identity takes store (rolling)", "--notes", "The identity takes check's resolves, keyed by code and "
                 "inputs (scripts/ci/identity-takes-check.sh). A cache, not a version of anything."], self.ctx.env))
        self.made = True

    def keep(self, path: str, asset: str, base: bool = False) -> None:
        """Store a result, never failing the check."""
        on_main = self.ctx.env.get("GITHUB_REF", "") == "refs/heads/main"
        if not (self.store and asset and (base or on_main) and ctx_module.nonempty(path)):
            return
        self.release()
        kind = asset[len("takes-"):].split("-", 1)[0]
        self.ctx.runner.run(tool_process.Command(
            [os.path.join(self.ctx.here, "scripts/ci/release-store-prune.sh"), self.store, f"takes-{kind}-{self.ctx.cc}-",
             "24", self.ctx.setting("TAKES_STORE_ASSETS", "900")],
            {**self.ctx.env, "RELEASE_PRUNE_TIMEOUT": f"{self.gh_read:g}"}))
        if not self.upload(path, asset):
            self.raced(asset)

    def upload(self, path: str, asset: str) -> bool:
        packed = os.path.join(self.ctx.temp, asset)
        try:
            with open(path, "rb") as plain, gzip.open(packed, "wb") as out:
                shutil.copyfileobj(plain, out)
        except OSError:
            return False
        done = self.ctx.runner.capture(tool_process.Command(
            [os.path.join(self.ctx.here, "scripts/ci/gh-release.sh"), "upload", self.store, packed, "--clobber"],
            self.ctx.env))
        if done.stderr:
            self.ctx.warn(done.stderr.rstrip("\n"))
        return done.code == 0

    def raced(self, asset: str) -> None:
        listed = self.ctx.gh.read(["release", "view", self.store, "--json", "assets", "--jq",
                                   f'.assets[] | select(.name == "{asset}") | .name'], self.gh_read, quiet=True)
        if listed is not None and asset in listed.splitlines():
            self.ctx.say(f"{self.ctx.label}: {asset} was stored by another run meanwhile")
        else:
            self.ctx.say(f"::warning::{self.ctx.label}: could not store {asset} -- the next run resolves its base again")

    def keep_final(self, side: str, sha: str, base: bool = False) -> None:
        """A side's final takes stored, its capture gaps FIRST: a run finding the takes stored finds their gaps too."""
        gaps = self.ctx.path(side, "capture-gaps.tsv")
        capture = self.keys.capture(sha)
        if os.path.isfile(gaps):
            self.keep(gaps, self.keys.asset("gaps", capture), base)
        self.keep(self.ctx.path(side, "ratchet-takes.tsv"), self.keys.asset("final", capture), base)

    def final_gaps(self, side: str, sha: str) -> None:
        """After a side's final takes were fetched: their capture gaps, when stored (information only)."""
        gaps = self.ctx.path(side, "capture-gaps.tsv")
        remove(gaps)
        if not self.fetch(self.keys.asset("gaps", self.keys.capture(sha)), gaps):
            remove(gaps)

    def keep_trace(self, traces: str, side: str, sha: str, base: bool = False) -> None:
        """Both of a side's traces, as one gzip stream, stored -- one alone proves nothing."""
        parts = [os.path.join(traces, f"{side}-{kind}.jsonl.gz") for kind in ("model", "final")]
        if not all(ctx_module.nonempty(p) for p in parts):
            return
        joined = os.path.join(traces, f"{side}.jsonl.gz")
        with open(joined, "wb") as out:
            for part in parts:
                with open(part, "rb") as f:
                    out.write(f.read())
        self.keep(joined, self.keys.trace(sha), base)
        self.keep(joined, self.keys.exact_trace(sha), base)


class BaseRowWait:
    """THE BASE ROW: a result the run's takes-base row stores, waited for as long as that row is RUNNING."""

    def __init__(self, store: Store, asset: str, path: str, what: str) -> None:
        self.store = store
        self.ctx = store.ctx
        self.asset, self.path, self.what = asset, path, what

    def row_state(self) -> Optional[str]:
        env = self.ctx.env
        jobs = f"repos/{env.get('GITHUB_REPOSITORY', '')}/actions/runs/{env.get('GITHUB_RUN_ID', '')}/jobs?per_page=100"
        answer = self.ctx.gh.read(["api", jobs, "--jq", ".jobs[] | select(.name | endswith(env.TAKES_BASE_JOB)) | .status"],
                                  self.store.gh_read)
        return None if answer is None else answer.strip()

    def wait(self) -> bool:
        started = self.ctx.clock()
        deadline = started + self.ctx.seconds("TAKES_BASE_WAIT", "1800")
        self.store.how = "reused"
        while True:
            if self.store.fetch(self.asset, self.path):
                self.say(f"{self.store.how} ({self.asset}, after {int(self.ctx.clock() - started)}s)")
                return True
            self.store.how = "awaited"
            verdict = self.after(self.row_state())
            if verdict is not None:
                return verdict
            if self.ctx.clock() >= deadline:
                self.say(f"not stored after {int(deadline - started)}s -- resolving it")
                return False
            self.ctx.sleep(self.ctx.seconds("TAKES_BASE_POLL", "15"))

    def after(self, state: Optional[str]) -> Optional[bool]:
        """What the row's state settles: True stored, False to resolve here, None wait on (a failed read too)."""
        if state is None or state == "in_progress":
            return None
        if state == "completed":  # done meanwhile: what it stored just before is there now, or never will be
            if self.store.fetch(self.asset, self.path):
                self.say(f"awaited ({self.asset})")
                return True
            self.say("not stored by the takes-base row (completed) -- resolving it")
            return False
        self.say(f"not stored, and the takes-base row is {state or 'absent'} -- resolving it")
        return False

    def say(self, rest: str) -> None:
        self.ctx.say(f"{self.ctx.label}: {self.what} {rest}")


def remove(path: str) -> None:
    if os.path.exists(path):
        os.remove(path)


Fetcher = Callable[[str, str, str], bool]


def unchanged(ctx: Context, base: str, under: Iterable[str]) -> bool:
    """Nothing under these paths differs between the base and the head."""
    return base == ctx.head or ctx.git.ok("diff", "--quiet", base, ctx.head, "--", *under)


def moves(ctx: Context, base: str, aa: bool) -> Tuple[bool, bool]:
    """(the takes can move, a corpus-shape finding can): the identity code, or the checks and allowlists, changed."""
    shape = bool(ctx.env.get("SHAPE_COMMAND")) and not unchanged(ctx, base, paths.SHAPE_PATHS)
    return aa or not unchanged(ctx, base, paths.IDENTITY_PATHS), shape


def store_named(ctx: Context, aa: bool) -> Tuple[str, str]:
    """(the store, the fill laid): a fill laid but never published names nothing a later run could match -- such a
    row neither reuses nor stores -- and THE A/A makes both sides here, neither fetched nor kept."""
    laid = ctx_module.read(os.path.join(ctx.temp, "laid-fill")).rstrip("\n")
    store = "" if laid == "unpublished" or aa else ctx.env.get("TAKES_STORE", "")
    return store, laid
