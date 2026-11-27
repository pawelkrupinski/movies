#!/usr/bin/env python3
"""THE WHAT-IF'S RECORD a stamp carry's what-if row stands on (identity_stamp_carry.py, its header's proof 2 and THE
PUBLISHED VERDICT): identity-what-if's verdicts under a (head, base) -- this machine's record, or the copy committed beside
the rows -- and the release asset each run publishes them as, which `verify` reads them back from. identity-what-if runs
its `asset-name` and `publish` through identity_stamp_carry.py. And what each country's verdict line must say for a
what-if row to hold (country_refusals)."""
from __future__ import annotations

import hashlib
import json
import os
import re
import sys
import tempfile
from typing import Any

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from pure_move_check import git  # noqa: E402  (the one git a proof reads)
import machine  # noqa: E402
import tool_git  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402

# the what-if records a what-if row stands on, committed with it (<head>-<base>.tsv, identity-tools.sh's format)
VERDICTS = "test/resources/identity-what-if-verdicts"
COUNTRIES = ("pl", "de", "uk", "es", "us")  # a what-if row needs a verdict of each


def identity_cache() -> str:
    """The cache every identity tool shares (tool-conventions.sh's KINOWO_IDENTITY_CACHE)."""
    return machine.environment().get("KINOWO_IDENTITY_CACHE") or os.path.expanduser("~/.cache/kinowo-identity")


def cached_verdict(head: str, base: str) -> str:
    """identity-what-if's record under (head, base) on THIS machine (identity-tools.sh's what_if_record_verdict), "" for
    none: what `propose` and `auto` read, and copy beside the rows they write (VERDICTS)."""
    path = os.path.join(identity_cache(), "what-if-verdicts", f"{head}-{base}.tsv")
    if not os.path.isfile(path):
        return ""
    with open(path) as f:
        return f.read()


def diff_id(base: str, head: str) -> str:
    """The stable patch-id of base..head's whole diff (`git patch-id --stable`): the same for a change rebased onto a main
    that moved elsewhere, another for any other diff; "" for an empty one."""
    diff = git("diff", "--binary", base, head)
    patch_id = tool_git.Git.here().call("patch-id", "--stable", input=diff, quiet=True).stdout
    return patch_id.split()[0] if patch_id.strip() else ""


def verdict_path(head: str, base: str) -> str:
    return f"{VERDICTS}/{head}-{base}.tsv"


# ── THE PUBLISHED VERDICT (identity_stamp_carry.py's header) ─────────────────────────────────────────────────────────────────────────────────────
ASSET = re.compile(r"what-if-verdict-[0-9a-f]{32}\.json")


def release() -> str:
    return machine.environment().get("KINOWO_WHAT_IF_RELEASE") or "identity-what-if-verdicts"


def gh(*args: str, timeout: int = 120) -> tool_process.Captured:
    """gh against the repository (KINOWO_GH: a test's fake, the seam), bounded; a hung or missing gh is a failed call."""
    repository = machine.environment().get("KINOWO_REPO") or "pawelkrupinski/movies"
    return tool_github.GitHub.here().call([*args, "-R", repository], timeout, quiet=True)


def asset_name(head: str, base: str, patch_id: str, countries: str, tools: str, run: str) -> str:
    key = f"head={head} base={base} patch-id={patch_id} countries={countries} tools={tools} run={run}"
    return f"what-if-verdict-{hashlib.sha256(key.encode()).hexdigest()[:32]}.json"


def moved_rows(text: str) -> int:
    """The rows of a takes diff, as identity-what-if counts them (`grep -cv '^country'`)."""
    return sum(1 for line in text.splitlines() if not line.startswith("country"))


def read_text(path: str) -> str:
    if not path or not os.path.isfile(path):
        return ""
    with open(path) as f:
        return f.read()


def publish(head: str, base: str, countries: str, tools: str, run: str) -> int:
    """identity-what-if's step after each country it records: its run's asset, re-uploaded whole (only this run writes
    it: the key holds the run's id) with every line it recorded so far and the takes diffs they name."""
    patch_id = diff_id(base, head)
    name = asset_name(head, base, patch_id, countries, tools, run)
    lines = [line for line in cached_verdict(head, base).splitlines() if line.split("\t")[6:7] == [name]]
    diffs: dict[str, dict[str, str]] = {}
    for line in lines:
        cc, takes, mode = line.split("\t")[0], line.split("\t")[3], line.split("\t")[5]
        paths = {"takes": takes, "final": takes.replace("takes-diff-", "final-diff-") if "final" in mode.split(",") else ""}
        diffs[cc] = {kind: read_text(path) for kind, path in paths.items()}
    body = {"name": name, "head": head, "base": base, "patch_id": patch_id, "countries": countries, "tools": tools,
            "run": run, "lines": lines, "diffs": diffs}
    with tempfile.TemporaryDirectory() as tmp:
        path = os.path.join(tmp, name)
        with open(path, "w") as f:
            json.dump(body, f, indent=1, sort_keys=True)
        done = gh("release", "upload", release(), path, "--clobber")
        if done.code != 0 and "not found" in (done.stderr + done.stdout).lower():
            gh("release", "create", release(), "--prerelease", "--title", "Identity what-if verdicts (rolling)", "--notes",
               "Each scripts/identity-what-if run's verdict and takes diffs: what scripts/identity-stamp-carry verify "
               "checks a committed what-if verdict against.")
            done = gh("release", "upload", release(), path, "--clobber")
    if done.code != 0:
        print(f"could not publish {name} on release {release()}: {(done.stderr or done.stdout).strip()[-300:]} -- a stamp "
              "carry refuses this verdict until it is published (run the what-if again)", file=sys.stderr)
        return 1
    print(f"published {name} on release {release()} ({len(lines)} verdict(s))")
    return 0


def fetch_asset(name: str, wanted: str) -> dict[str, Any] | None:
    """The published asset `name`: the identity cache's copy when it holds the line `wanted` (an asset grows while its run
    publishes), else gh's, kept in the cache; None when gh has none it can read."""
    cache = os.path.join(identity_cache(), "what-if-assets")
    kept = os.path.join(cache, name)

    def read() -> dict[str, Any] | None:
        try:
            with open(kept) as f:
                return json.load(f)
        except (OSError, ValueError):
            return None

    body = read()
    if body is not None and wanted in body.get("lines", []):
        return body
    os.makedirs(cache, exist_ok=True)
    if gh("release", "download", release(), "-p", name, "-O", kept + ".part", "--clobber").code != 0:
        return None
    os.replace(kept + ".part", kept)
    return read()


def what_if_verdicts(head: str, base: str, at: str | None = None) -> dict[str, list[str]]:
    """Each country's newest line of the what-if record under (head, base) -- cc, verdict, finished, takes diff, moved,
    mode: this machine's own record, or, `at` a commit, the record committed there beside the rows (what `verify` reads,
    so the hook proves a row alike on the laptop and on ci-runner, where no what-if ever ran). The committed record of a
    row is named by the range the row cites, whatever the queue's picks made of its shas."""
    text = cached_verdict(head, base) if at is None else git("show", f"{at}:{verdict_path(head, base)}", ok=(0, 128))
    newest: dict[str, list[str]] = {}
    for line in text.splitlines():
        fields = line.split("\t")
        if fields[0]:
            newest[fields[0]] = fields
    return newest


def verdict_refusals(cc: str, v: list[str]) -> list[str]:
    """Why one country's newest verdict line carries nothing: not done, takes moved, or only its selection resolved."""
    verdict, moved, mode = v[1], (v[4] if len(v) > 4 else ""), (v[5] if len(v) > 5 else "")
    why: list[str] = []
    if verdict != "done":
        why.append(f"{cc}: its newest verdict is {verdict!r}, not done")
    if moved != "0":
        why.append(f"{cc}: {moved or 'an unrecorded number of'} take(s) moved")
    if "whole" not in mode.split(","):
        why.append(f"{cc}: resolved its selection, not every family (run identity-what-if --all)")
    return why


def country_refusals(verdicts: dict[str, list[str]], base: str, head: str) -> tuple[list[str], bool]:
    """Why each country's newest verdict carries nothing, and whether every one compared the final takes."""
    why: list[str] = []
    for cc in COUNTRIES:
        v = verdicts.get(cc)
        why += verdict_refusals(cc, v) if v is not None else [
            f"{cc}: no identity-what-if verdict recorded for head {head[:12]} over base {base[:12]}"]
    final = all("final" in (verdicts[cc][5] if len(verdicts[cc]) > 5 else "").split(",") for cc in COUNTRIES if cc in verdicts)
    return why, final
