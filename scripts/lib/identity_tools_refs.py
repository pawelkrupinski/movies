"""THE COMMANDS OF scripts/lib/identity_tools.py ABOUT A CI MEASURE'S COMMITS AND A RUN'S RESULTS: the commits and branches a
measure runs on, a convergence run's takes, and the what-if verdict record."""
from __future__ import annotations

from typing import List

import identity_measure_refs
import identity_what_if_record
from identity_tools_cache import Tools


def takes_download(t: Tools, a: List[str]) -> int:
    identity_what_if_record.TakesDownloads(t.ctx).download(a[0], a[1], a[2])
    return 0


def main_ci_over(t: Tools, a: List[str]) -> int:
    commit = identity_measure_refs.main_ci_over(t.ctx.env, a[0], a[1], a[2])
    return t.say(commit) if commit else 1


def measure_branch(t: Tools, a: List[str]) -> int:
    return t.say(identity_measure_refs.measure_branch(a[0], a[1] if len(a) > 1 else ""))


def push_measure_branch(t: Tools, a: List[str]) -> int:
    code, why = identity_measure_refs.MeasurePush(t.ctx, t.locks).push(a[1], a[2], a[3])
    t.ctx.files.write_text(a[0], why)
    return code


def verdicts_file(t: Tools, a: List[str]) -> int:
    return t.say(identity_what_if_record.verdicts_file(t.ctx.cache, a[0], a[1]))


def record_verdict(t: Tools, a: List[str]) -> int:
    identity_what_if_record.record_verdict(t.ctx.files, t.ctx.cache, *a[:8])
    return 0


def verdicts(t: Tools, a: List[str]) -> int:
    found = identity_what_if_record.verdicts(t.ctx.files, t.ctx.cache, a[0], a[1])
    return 1 if found is None else t.say(found)
