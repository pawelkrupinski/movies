"""THE RESULT CACHE of identity-measure (scripts/identity_measure_cli.py, its header): every verdict a measurement prints
stored as a release asset `measure-result-<key>.json`, the key a hash of the head, the base, the recording, the
countries, the measure mode and THE TOOLING HASH (the blobs of the measure's own tooling in this checkout), and a
repeat request with the same key answered from it in seconds."""
from __future__ import annotations

import glob
import hashlib
import json
import os
import time
from typing import Any, List, Optional

from identity_measure_cli_context import HERE, Context, say

ROOT = os.path.dirname(HERE)
TOOLING = ["scripts/identity-measure", "scripts/identity_measure.py", "scripts/identity-tools.sh",
           "scripts/tool-conventions.sh", "scripts/ci/identity_takes.py", "scripts/ci/identity_takes_diff.py",
           "scripts/lib/tool_process.py", "scripts/lib/tool_github.py", "scripts/lib/tool_git.py",
           "scripts/lib/tool_dispatch.py", "scripts/lib/identity_dispatch.py", "scripts/ci_wait.py",
           "scripts/lib/tool_shell.py", "scripts/ci/dispatch-takes.sh", ".github/workflows/identity-takes.yml",
           ".github/workflows/identity-model-convergence.yml", ".github/workflows/country-convergence-leg.yml"]
TOOLING_GLOBS = ["scripts/identity_measure_cli*.py", "scripts/ci/identity_takes_check*.py"]


def blob(path: str) -> str:
    """git hash-object's id of the file."""
    with open(path, "rb") as f:
        data = f.read()
    return hashlib.sha1(b"blob %d\0" % len(data) + data).hexdigest()


def tooling_hash() -> str:
    found = [f for f in TOOLING if os.path.isfile(os.path.join(ROOT, f))]
    found += sorted(os.path.relpath(p, ROOT) for g in TOOLING_GLOBS for p in glob.glob(os.path.join(ROOT, g))
                    if not p.endswith(("_test.py", "_fixture.py")))
    listing = "".join(f"{f} {blob(os.path.join(ROOT, f))}\n" for f in found)
    return hashlib.sha256(listing.encode()).hexdigest()[:16]


class Results:
    def __init__(self, c: Context) -> None:
        self.c = c
        self.release = c.env.get("FIXTURE_RELEASE_TAG") or "convergence-fixtures"
        self.cached_run = ""

    def key(self, mode: str) -> str:
        s = self.c.state
        text = (f"head={s.commit} base={s.base} recording={s.recording or 'pinned'} countries={s.countries} "
                f"mode={mode} tooling={tooling_hash()}")
        return f"measure-result-{hashlib.sha256(text.encode()).hexdigest()[:32]}.json"

    def lookup(self, mode: str) -> Optional[int]:
        """A stored verdict (its table printed), or None when none answers."""
        o = self.c.o
        if o.no_cache or o.fresh or o.json:
            return None
        name = self.key(mode)
        stored = self.download(name)
        if stored is None or stored.get("verdict") not in (0, 1):
            return None
        at = int(stored.get("stored_at") or 0)
        return self.answer(mode, stored, at) if self.fresh_enough(name, at) else None

    def fresh_enough(self, name: str, at: int) -> bool:
        """A pinned-pair verdict answers for IDENTITY_MEASURE_CACHE_HOURS (the pinned pair is renewed); a named
        recording's for good."""
        hours = int(self.c.env.get("IDENTITY_MEASURE_CACHE_HOURS") or 24)
        age = int(self.c.now()) - at
        if not self.c.state.recording and age > hours * 3600:
            say(f"cache: {name} is {int(age / 3600)}h old, older than the pinned pair's {hours}h -- measuring")
            return False
        return True

    def answer(self, mode: str, stored: Any, at: int) -> int:
        s, run = self.c.state, str(stored.get("run") or "?")
        when = time.strftime("%Y-%m-%dT%H:%MZ", time.gmtime(at))
        say(f"CACHED verdict of run {run} ({mode}, {s.recording or 'pinned'} {s.countries}, stored {when}) -- "
            "--no-cache to measure again")
        self.c.say_commit_and_base()
        print(stored.get("table") or "", end="", flush=True)
        # the cross-country follow-up reads the countries that moved beside the run, as after a measurement
        self.cached_run = f"cached-{run}"
        os.makedirs(self.c.measured(self.cached_run), exist_ok=True)
        with open(self.c.measured(self.cached_run, "moved"), "w") as f:
            f.write(f"{stored.get('moved') or ''}\n")
        return int(stored["verdict"])

    def download(self, name: str) -> Any:
        target = os.path.join(self.c.scratch_dir(), name)
        if not self.c.github.succeeds(["release", "download", self.release, "-p", name, "-O", target, "--clobber"],
                                      self.c.gh_timeout):
            return None
        try:
            with open(target) as f:
                return json.load(f)
        except (OSError, ValueError):
            return None

    def store(self, mode: str, verdict: int, run: str, table: str, moved: str = "") -> None:
        self.store_as(self.key(mode), verdict, run, table, moved)

    def store_as(self, name: str, verdict: int, run: str, table: str, moved: str = "") -> None:
        """Best effort, said when it fails."""
        o, s = self.c.o, self.c.state
        if o.no_cache or o.no_store or o.json or verdict > 1 or not table or not os.path.getsize(table):
            return
        with open(table) as f:
            text = f.read()
        record = {"key": name, "head": s.commit, "base": s.base, "recording": s.recording or "pinned",
                  "countries": s.countries, "verdict": verdict, "run": run, "stored_at": int(self.c.now()),
                  "moved": read_stripped(moved), "table": text}
        path = os.path.join(self.c.scratch_dir(), name)
        with open(path, "w") as f:
            json.dump(record, f)
        if self.c.github.succeeds(["release", "upload", self.release, path, "--clobber"], 300):
            say(f"cache: stored as {name} on release {self.release}")
        else:
            say(f"cache: could not store {name} on release {self.release} (the next request measures again)")

    def leave_key(self, run: str, mode: str) -> None:
        """A --no-wait dispatch's key beside its run: its --run stores the verdict under this request's key."""
        o, s = self.c.o, self.c.state
        if o.no_cache or o.no_store or o.json:
            return
        os.makedirs(self.c.measured(run), exist_ok=True)
        fields: List[str] = [self.key(mode), s.commit, s.base, s.recording or "pinned", s.countries]
        with open(self.c.measured(run, "result-key"), "w") as f:
            f.write("\t".join(fields) + "\n")


def read_stripped(path: str) -> str:
    try:
        with open(path) as f:
            return f.read().rstrip("\n")
    except OSError:
        return ""
