"""--bisect GOOD BAD (scripts/identity_what_if_cli.py): the commit of GOOD..BAD (first parent) whose code moved the take
-- a listing's model and final take with --listing, or every take -- over ONE mirror snapshot, each step a whole side
kept as any base is. When GOOD and BAD take alike there, the code did not move it: the corpus did, and the take at
--since (24 h before the snapshot by default) and at the snapshot is shown."""
from __future__ import annotations

import datetime
import sys
from typing import List

from identity_what_if_cli_context import Context, Exit
from identity_what_if_cli_mirror import Mirror
from identity_what_if_cli_sides import SideKey, Sides


def minute_now() -> str:
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:00Z")


def day_before(snapshot: str) -> str:
    moment = datetime.datetime.fromisoformat(snapshot.replace("Z", "+00:00"))
    return (moment - datetime.timedelta(hours=24)).strftime("%Y-%m-%dT%H:%M:00Z")


class Bisect:
    def __init__(self, ctx: Context, mirror: Mirror, sides: Sides, key: SideKey) -> None:
        self.ctx, self.mirror, self.sides, self.key = ctx, mirror, sides, key
        self.cc = ""
        self.started = 0.0

    def commit(self, ref: str) -> str:
        sha = self.ctx.commit_of(ref)
        if sha is None:
            print(f"--bisect: no commit {ref}", file=sys.stderr)
            raise Exit(2)
        return sha

    def take_at(self, sha: str, snapshot: str) -> str:
        """The take (identity_what_if.py take) of `sha`'s whole side at `snapshot`."""
        side = self.sides.whole(sha, self.cc, snapshot)
        listing = ["--listing", self.ctx.o.listing] if self.ctx.o.listing else []
        done = self.ctx.capture(self.ctx.what_if_py(["take", *listing, "--side", side]))
        if done.code != 0:
            raise Exit(2)
        return done.stdout.rstrip("\n")

    def wall(self) -> str:
        return f"{self.ctx.clock() - self.started:.0f}"

    def run(self) -> int:
        o = self.ctx.o
        if len(o.ccs) != 1:
            print("--bisect: one country", file=sys.stderr)
            raise Exit(2)
        self.cc = self.ctx.cc = o.ccs[0]
        good, bad = self.commit(o.bisect_good), self.commit(o.bisect_bad)
        self.mirror.fence(self.cc)
        self.key.mirror_gen = self.mirror.ready(self.cc)
        snapshot = o.at or self.sides.cached_at(self.cc, bad) or minute_now()
        listed = self.ctx.git("rev-list", "--first-parent", "--reverse", f"{good}..{bad}").stdout
        commits = [good, *[c for c in listed.split("\n") if c]]
        self.ctx.log(f"{self.cc} bisect: {len(commits)} commit(s) from {good[:12]} to {bad[:12]} at {snapshot}"
                     + (f", listing {o.listing}" if o.listing else ""))
        self.started = float(f"{self.ctx.clock():.1f}")
        first, last = self.take_at(good, snapshot), self.take_at(bad, snapshot)
        print(f"  {good[:12]} (good): {first}")
        print(f"  {bad[:12]} (bad):  {last}")
        if first == last:
            return self.corpus(bad, snapshot, last)
        return self.search(commits, snapshot, first)

    def corpus(self, bad: str, snapshot: str, last: str) -> int:
        """No commit moves it at the snapshot: whether the corpus did, since --since."""
        since = self.ctx.o.since or day_before(snapshot)
        before = self.take_at(bad, since)
        print(f"  no commit of the range moves it at {snapshot}: the code takes alike. {bad[:12]} at {since}: {before}")
        if before == last:
            print(f"  CORPUS: the take stood alike at {since} too -- neither the code nor the corpus moved it in that "
                  "window")
        else:
            print(f"  CORPUS: the corpus moved it between {since} and {snapshot} (scripts/identity-replay / "
                  "identity-why name the input)")
        self.unchanged()
        print(f"  wall {self.wall()}s")
        return 0

    def unchanged(self) -> None:
        if not self.mirror.unchanged(self.cc, self.key.mirror_gen, "after the bisect"):
            raise Exit(2)

    def search(self, commits: List[str], snapshot: str, first: str) -> int:
        lo, hi, step = 0, len(commits) - 1, 0
        while hi - lo > 1:
            mid = (lo + hi) // 2
            step += 1
            t = self.take_at(commits[mid], snapshot)
            print(f"  step {step}: {commits[mid][:12]} {'good' if t == first else 'bad'}: {t}")
            if t == first:
                lo = mid
            else:
                hi = mid
        self.unchanged()
        moved = self.ctx.git("log", "-1", "--format=%h %s", commits[hi]).stdout.rstrip("\n")
        print(f"  FIRST MOVED BY {moved}")
        print(f"    before: {self.take_at(commits[lo], snapshot)}")
        print(f"    after:  {self.take_at(commits[hi], snapshot)}")
        print(f"  snapshot {snapshot}; {step} step(s); wall {self.wall()}s")
        return 0
