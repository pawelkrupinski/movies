"""THE BASE, per seed (identity_drift_check.py's header): the newest first-parent ancestor whose result for the seed main
STORED GREEN, else the parent (on main) or the merge-base with main (a branch); DRIFT_BASE names one outright. A base
whose identity code is the head's replays nothing (`same-code`); a stored one is read; the rest are resolved here, in a
worktree of their own, one replay a seed -- a base that does not compile stepping back to its parent (DRIFT_BASE_STEPS).
Each seed's base, and how it was had, is written to <out>/base/{base,how}-<seed>.txt."""
from __future__ import annotations

import os
import shutil
import sys
from typing import List, Mapping, Optional, Sequence, Tuple

import identity_drift_check_replay as replay_
import identity_drift_check_store as store_
import tool_process


class Bases:
    def __init__(self, keys: store_.Keys, store: store_.Store, replayer: replay_.Replayer, env: Mapping[str, str],
                 out: str) -> None:
        self.keys = keys
        self.store = store
        self.replayer = replayer
        self.env = env
        self.out = out
        self.base_dir = os.path.join(out, "base")
        self.cc = keys.cc

    def git(self, *args: str, timeout: Optional[float] = None) -> tool_process.Captured:
        return self.keys.runner.capture(tool_process.Command(["git", "-C", self.keys.root, *args], self.env,
                                                             timeout=timeout))

    def commit(self, ref: str) -> str:
        done = self.git("rev-parse", "--verify", "-q", f"{ref}^{{commit}}")
        return done.stdout.strip() if done.code == 0 else ""

    def stop(self, said: str) -> replay_.Stop:
        self.replayer.say_verdict(said)
        return replay_.Stop(said)

    def write(self, name: str, text: str) -> None:
        with open(os.path.join(self.base_dir, name), "w") as f:
            f.write(text + "\n")

    def default_base(self, head_sha: str, depth: int) -> str:
        named = self.env.get("DRIFT_BASE", "")
        if named:
            self.git("fetch", "-q", "--no-tags", "--depth=1", "origin", named, timeout=300)
            return self.commit(named) or self.fail(f"identity drift ({self.cc}): no base commit {named}")
        base = ""
        if self.env.get("GITHUB_REF", "") != "refs/heads/main" and self.git(
                "fetch", "-q", "--no-tags", "--filter=blob:none", f"--depth={depth}", "origin", "main", timeout=300).code == 0:
            base = self.git("merge-base", "HEAD", "FETCH_HEAD").stdout.strip()
        if not base or base == head_sha:
            base = self.commit("HEAD~1") or self.fail(
                f"identity drift ({self.cc}): could not name a base commit (no parent in the fetched history)")
        return base

    def fail(self, said: str) -> str:
        raise self.stop(said)

    def ancestors(self, depth: int) -> List[Tuple[str, str]]:
        if self.env.get("DRIFT_BASE"):
            return []
        listed = self.git("rev-list", "--first-parent", f"--max-count={depth}", "HEAD~1").stdout.split()
        return [(sha, self.keys.code_of(sha)) for sha in listed]

    def choose(self, seeds: Sequence[str], head_code: str) -> None:
        """Each seed's base and how it was had -- and the bases nothing stored, resolved here."""
        depth = int(self.env.get("DRIFT_BASE_DEPTH") or "60")
        head_sha = self.git("rev-parse", "HEAD").stdout.strip()
        self.git("fetch", "-q", "--no-tags", "--filter=blob:none", f"--depth={depth}", "origin", head_sha, timeout=300)
        default = self.default_base(head_sha, depth)
        ancestors = self.ancestors(depth)
        resolve = [s for s in seeds if self.choose_seed(s, default, ancestors, head_code)]
        if resolve:
            self.resolve(resolve, default)

    def choose_seed(self, seed: str, default: str, ancestors: List[Tuple[str, str]], head_code: str) -> bool:
        """Whether this seed's base is to be resolved (its base and how written either way)."""
        green = next(((sha, code) for sha, code in ancestors if self.store.stored(self.keys.asset("green", code, seed))), None)
        base, base_code = green if green else (default, self.keys.code_of(default))
        how = "stored" if green else ""
        self.write(f"base-{seed}.txt", base)
        resolve = False
        if base_code == head_code:
            how = "same-code"
        elif how == "stored" and self.fetched("green", base_code, seed):
            pass
        elif self.fetched("calc", base_code, seed):
            how = "stored"
        else:
            how = "resolved"
            self.write(f"base-{seed}.txt", default)
            if self.keys.code_of(default) == head_code:
                how = "same-code"
            else:
                resolve = True
        self.write(f"how-{seed}.txt", how)
        return resolve

    def fetched(self, kind: str, code: str, seed: str) -> bool:
        """A stored base's flips fetched, and its scenario with them where it has one."""
        if not self.store.fetch(self.keys.asset(kind, code, seed), os.path.join(self.base_dir, f"flips-{seed}.tsv")):
            return False
        self.store.fetch(self.keys.asset(f"{kind}scenario", code, seed), os.path.join(self.base_dir, f"scenario-{seed}.tsv"))
        return True

    def checkout_dir(self) -> str:
        return os.path.join(self.env.get("RUNNER_TEMP") or self.out, "drift-base-checkout")

    def resolve(self, seeds: List[str], default: str) -> None:
        """The bases not stored, replayed here: the first seed decides whether the base compiles."""
        base, steps, first = default, 0, seeds[0]
        while True:
            self.check_out(base, steps, seeds)
            self.replayer.replay(self.checkout_dir(), self.seed_dir(first), first)
            parent = self.broken_parent(base, first, steps)
            if not parent:
                break
            print(f"::warning title=identity drift ({self.cc})::the base {base[:12]} does not compile; judged against its "
                  f"parent {parent[:12]} instead", flush=True)
            base, steps = parent, steps + 1
        for seed in seeds:
            self.store_seed(base, seed, first)
        self.git("worktree", "remove", "--force", self.checkout_dir())

    def seed_dir(self, seed: str) -> str:
        return os.path.join(self.base_dir, f"seed-{seed}")

    def check_out(self, base: str, steps: int, seeds: List[str]) -> None:
        print(f"identity drift ({self.cc}): resolving the base {base[:12]} on seed(s) {' '.join(seeds)} (none stored)",
              flush=True)
        self.git("fetch", "-q", "--no-tags", f"--depth={steps + 2}", "origin", base, timeout=300)
        shutil.rmtree(self.checkout_dir(), ignore_errors=True)
        self.git("worktree", "prune")
        if self.git("worktree", "add", "-q", "--detach", self.checkout_dir(), base).code != 0:
            raise self.stop(f"identity drift ({self.cc}): could not check out the base {base}")

    def broken_parent(self, base: str, seed: str, steps: int) -> str:
        """The parent to step back to when the base did not compile (and steps are left), else ""."""
        seed_dir = self.seed_dir(seed)
        if os.path.isfile(os.path.join(seed_dir, f"flips-{self.cc}.tsv")) or steps >= int(self.env.get("DRIFT_BASE_STEPS") or "3"):
            return ""
        if "Compilation failed" not in replay_.tail(os.path.join(seed_dir, "sbt.log"), 10 ** 9):
            return ""
        return self.commit(f"{base}~1")

    def store_seed(self, base: str, seed: str, first: str) -> None:
        self.write(f"base-{seed}.txt", base)
        seed_dir = self.seed_dir(seed)
        if seed != first:  # one seed a replay: a base from before `--seed S1,S2` reads one seed alone
            self.replayer.replay(self.checkout_dir(), seed_dir, seed)
        flips = os.path.join(seed_dir, f"flips-{self.cc}.tsv")
        if not os.path.isfile(flips):
            sys.stderr.write(replay_.tail(os.path.join(seed_dir, "sbt.log")))
            raise self.stop(f"identity drift ({self.cc}): the base's replay of seed {seed} wrote nothing, so nothing was judged")
        code = self.keys.code_of(base)
        shutil.copy(flips, os.path.join(self.base_dir, f"flips-{seed}.tsv"))
        self.store.keep(os.path.join(self.base_dir, f"flips-{seed}.tsv"), self.keys.asset("calc", code, seed))
        scenario = os.path.join(seed_dir, f"scenario-{self.cc}.tsv")
        if os.path.isfile(scenario) and os.path.getsize(scenario) > 0:  # a base from before the scenario guard has none
            shutil.copy(scenario, os.path.join(self.base_dir, f"scenario-{seed}.tsv"))
            self.store.keep(os.path.join(self.base_dir, f"scenario-{seed}.tsv"), self.keys.asset("calcscenario", code, seed))
