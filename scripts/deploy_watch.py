#!/usr/bin/env python3
"""scripts/deploy-watch: wait until the workers run an image, then read PromQL checks over a window, PASS/FAIL each.
Usage and the checks format: the header of scripts/deploy-watch. Tests: scripts/deploy_watch_test.py."""
from __future__ import annotations

import argparse
import json
import math
import operator
import os
import re
import shlex
import subprocess
import sys
import time
from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Callable

HERE = os.path.dirname(os.path.abspath(__file__))
CHECKS_DIR = os.path.join(HERE, "deploy-watch-checks")
COUNTRIES = ["pl", "de", "uk", "us", "es"]
PROMETHEUS = "http://10.20.0.11:9090"
OPS = {"<=": operator.le, "<": operator.lt, ">=": operator.ge, ">": operator.gt, "==": operator.eq, "!=": operator.ne}
# a worker pod is worker-<cc>-<replicaset>-<pod>; its image is registry.kinowo.net/movies-worker:<tag>
POD = re.compile(r"^worker-([a-z]{2})-")
PODS = 'kube_pod_container_info{namespace="kinowo",container="worker"}'


class Usage(Exception):
    pass


@dataclass
class Check:
    name: str
    op: str
    threshold: float
    missing: str  # fail | zero
    promql: str

    def ok(self, value: float) -> bool:
        return OPS[self.op](value, self.threshold)


def parse_checks(text: str) -> list[Check]:
    checks = []
    for n, line in enumerate(text.splitlines(), 1):
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        cols = line.split("\t")
        if len(cols) != 5:
            raise Usage(f"checks line {n}: want 5 tab-separated columns (name op threshold missing promql), got {len(cols)}")
        name, op, threshold, missing, promql = (c.strip() for c in cols)
        if op not in OPS:
            raise Usage(f"checks line {n}: operator {op!r} is none of {' '.join(OPS)}")
        if missing not in ("fail", "zero"):
            raise Usage(f"checks line {n}: missing {missing!r} is neither fail nor zero")
        try:
            checks.append(Check(name, op, float(threshold), missing, promql))
        except ValueError:
            raise Usage(f"checks line {n}: threshold {threshold!r} is not a number")
    if not checks:
        raise Usage("no checks")
    return checks


def load_checks(name_or_path: str) -> list[Check]:
    path = name_or_path if os.path.exists(name_or_path) else os.path.join(CHECKS_DIR, f"{name_or_path}.tsv")
    try:
        with open(path) as f:
            return parse_checks(f.read())
    except FileNotFoundError:
        raise Usage(f"no checks file {name_or_path} (nor {path})")


def duration(text: str) -> float:
    m = re.fullmatch(r"(\d+)([smh]?)", text)
    if not m:
        raise argparse.ArgumentTypeError(f"{text!r}: want e.g. 30s, 5m, 2h")
    return int(m.group(1)) * {"s": 1, "": 60, "m": 60, "h": 3600}[m.group(2)]


def prometheus_query(promql: str) -> list[dict]:
    """One instant query, through DEPLOY_WATCH_QUERY (a command given the query, printing Prometheus' JSON) or by
    default `ssh monitoring-1 curl` against the fleet's Prometheus (reachable only from the monitoring host)."""
    override = os.environ.get("DEPLOY_WATCH_QUERY")
    if override:
        cmd = [*shlex.split(override), promql]
    else:
        url = os.environ.get("DEPLOY_WATCH_PROMETHEUS", PROMETHEUS)
        cmd = ["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=15", "monitoring-1",
               f"curl -sS --max-time 60 --data-urlencode query={shlex.quote(promql)} {url}/api/v1/query"]
    done = subprocess.run(cmd, capture_output=True, text=True, timeout=120)
    try:
        answer = json.loads(done.stdout)
    except json.JSONDecodeError:
        raise RuntimeError(f"unreadable answer (exit {done.returncode}): {(done.stderr or done.stdout).strip()[:300]}")
    if answer.get("status") != "success":
        raise RuntimeError(f"query failed: {answer.get('error', answer)}")
    return answer["data"]["result"]


def rollout(result: list[dict], tag: str, countries: list[str]) -> dict[str, tuple[int, int]]:
    """Per country: (pods on the tag, pods)."""
    state = {cc: (0, 0) for cc in countries}
    for s in result:
        m = POD.match(s["metric"].get("pod", ""))
        if m and m.group(1) in state:
            on, total = state[m.group(1)]
            state[m.group(1)] = (on + s["metric"].get("image", "").endswith(f":{tag}"), total + 1)
    return state


def judge(check: Check, result: list[dict], countries: list[str],
          reported: frozenset = frozenset()) -> tuple[bool, str, set[str]]:
    """This reading's verdict, its readings, and the countries NOT REPORTED YET, which judges nothing in this reading: a
    series reading NaN (the worker seeds its gauges NaN at boot until their first report -- the held gauges at the model's
    first batch, ~2 min after a pod starts), or, for a country with no value yet (not in `reported`), no series at all
    (a pod just started is not scraped yet: uk, 2026-11-07, the minute the roll ended). A series gone after its country
    reported fails; main() fails a country that never reports over the whole window."""
    values = {s["metric"].get("country"): float(s["value"][1]) for s in result}
    ok, parts, unreported = True, [], set()
    for cc in countries:
        if cc not in values and check.missing == "fail":
            if cc not in reported:
                unreported.add(cc)
                parts.append(f"{cc}=not reported yet")
                continue
            ok = False
            parts.append(f"{cc}=no series")
            continue
        value = values.get(cc, 0.0)
        if math.isnan(value):
            unreported.add(cc)
            parts.append(f"{cc}=not reported yet")
            continue
        good = check.ok(value)
        ok &= good
        parts.append(f"{cc}={value:g}" + ("" if good else f" (want {check.op} {check.threshold:g})"))
    return ok, " ".join(parts), unreported


def stamp(clock: Callable[[], float]) -> str:
    return datetime.fromtimestamp(clock(), timezone.utc).strftime("%H:%M:%SZ")


Query = Callable[[str], list]
Sleep = Callable[[float], object]


def parse(argv: list[str]) -> tuple[argparse.Namespace, list[Check]] | None:
    """The arguments and the checks they name; None (said on stderr) when they are no usage."""
    p = argparse.ArgumentParser(prog="deploy-watch", add_help=False)
    p.add_argument("--image", required=True)
    p.add_argument("--checks", default="identity-gate")
    p.add_argument("--countries", default=",".join(COUNTRIES))
    p.add_argument("--every", type=duration, default=duration("5m"))
    p.add_argument("--window", type=duration, default=duration("2h"))
    p.add_argument("--poll", type=duration, default=duration("1m"))
    p.add_argument("--rollout-timeout", type=duration, default=duration("90m"))
    p.add_argument("--no-rollout", action="store_true")
    try:
        args = p.parse_args(argv)
        return args, load_checks(args.checks)
    except Usage as e:
        print(f"deploy-watch: {e}", file=sys.stderr)
    except SystemExit:
        pass
    return None


def rollout_state(query: Query, clock: Callable[[], float], tag: str, countries: list[str]) -> dict[str, tuple[int, int]] | None:
    """One read of the rollout; None (said) when it was unreadable: asked again, never a verdict."""
    try:
        return rollout(query(PODS), tag, countries)
    except Exception as e:  # noqa: BLE001 -- one unreadable read is asked again, never a verdict
        print(f"{stamp(clock)} ROLLOUT: unreadable ({e}); asking again", file=sys.stderr)
        return None


def waiting_countries(state: dict[str, tuple[int, int]]) -> list[str]:
    return [cc for cc, (on, total) in state.items() if total == 0 or on < total]


def say_waiting(clock: Callable[[], float], state: dict[str, tuple[int, int]] | None, waiting: list[str]) -> None:
    if state is None:
        return
    pods = ", ".join(f"{cc} {state[cc][0]}/{state[cc][1]}" for cc in waiting)
    print(f"{stamp(clock)} ROLLOUT: waiting: {' '.join(waiting)} ({pods} pods on the tag)", file=sys.stderr)


def say_not_rolled(clock: Callable[[], float], minutes: float, state: object, waiting: list[str]) -> None:
    print(f"{stamp(clock)} NOT ROLLED after {minutes:g} min: {' '.join(waiting) if state else 'unreadable'}", file=sys.stderr)


def wait_for_rollout(args: argparse.Namespace, tag: str, countries: list[str], query: Query,
                     clock: Callable[[], float], sleep: Sleep) -> bool:
    """Waits until every worker of every country runs the tag; False when the rollout timeout passes first."""
    deadline = clock() + args.rollout_timeout
    print(f"{stamp(clock)} ROLLOUT: waiting for movies-worker:{tag} on {' '.join(countries)}", file=sys.stderr)
    while True:
        state = rollout_state(query, clock, tag, countries)
        waiting = [] if state is None else waiting_countries(state)
        if state is not None and not waiting:
            print(f"{stamp(clock)} ROLLED: every worker of {' '.join(countries)} runs {tag}", file=sys.stderr)
            return True
        say_waiting(clock, state, waiting)
        if clock() >= deadline:
            say_not_rolled(clock, args.rollout_timeout // 60, state, waiting)
            return False
        sleep(args.poll)


def judged(c: Check, query: Query, countries: list[str], reported: set[str]) -> tuple[bool, str, set[str]]:
    """A check's reading: (ok, detail, the countries not reported yet); an unreadable check fails the reading, named."""
    try:
        ok, detail, unreported = judge(c, query(c.promql), countries, frozenset(reported))
    except Exception as e:  # noqa: BLE001 -- an unreadable check fails this reading, named
        return False, f"unreadable: {e}", set()
    reported |= set(countries) - unreported
    return ok, detail, unreported


def read_once(checks: list[Check], reading: int, countries: list[str], query: Query, clock: Callable[[], float],
              failed: dict[str, list[str]], reported: dict[str, set[str]]) -> None:
    """One reading of every check: failures and reported countries are added to `failed` and `reported`."""
    for c in checks:
        ok, detail, unreported = judged(c, query, countries, reported[c.name])
        if ok and unreported:
            print(f"{stamp(clock)} {c.name}: {detail} (no verdict for those this reading)", file=sys.stderr)
        if not ok:
            failed[c.name].append(f"reading {reading} {stamp(clock)}: {detail}")
            print(f"{stamp(clock)} RED {c.name}: {detail}", file=sys.stderr)
    print(f"{stamp(clock)} reading {reading}: {sum(1 for f in failed.values() if f)} of {len(checks)} checks failed so far",
          file=sys.stderr)


def read_checks(checks: list[Check], args: argparse.Namespace, countries: list[str], query: Query,
                clock: Callable[[], float], sleep: Sleep) -> tuple[dict[str, list[str]], dict[str, set[str]], int]:
    """The checks read every `--every` over the window: (failures by check, countries each check reported, readings)."""
    failed: dict[str, list[str]] = {c.name: [] for c in checks}
    reported: dict[str, set[str]] = {c.name: set() for c in checks}  # the countries each check read a value of
    end, reading = clock() + args.window, 0
    while True:
        reading += 1
        read_once(checks, reading, countries, query, clock, failed, reported)
        if clock() + args.every > end:
            return failed, reported, reading
        sleep(args.every)


def verdict_line(c: Check, failures: list[str], reading: int) -> str:
    if failures:
        return f"FAIL\t{c.name}\t{c.op} {c.threshold:g}\t{len(failures)} of {reading} readings; first {failures[0]}"
    return f"PASS\t{c.name}\t{c.op} {c.threshold:g}\t{reading} readings"


def verdict_lines(checks: list[Check], countries: list[str], failed: dict[str, list[str]],
                  reported: dict[str, set[str]], reading: int) -> list[str]:
    """One PASS/FAIL line a check; a country that never reported over the whole window fails its check."""
    lines = []
    for c in checks:
        never = [cc for cc in countries if cc not in reported[c.name]]
        if never and not failed[c.name]:
            failed[c.name].append(f"{' '.join(never)} never reported a value (NaN or no series in all {reading} readings)")
        lines.append(verdict_line(c, failed[c.name], reading))
    return lines


def main(argv: list[str], query: Query = prometheus_query, clock: Callable[[], float] = time.time,
         sleep: Sleep = time.sleep) -> int:
    parsed = parse(argv)
    if parsed is None:
        return 2
    args, checks = parsed
    tag = args.image.rsplit(":", 1)[-1]
    countries = [c for c in args.countries.split(",") if c]
    if not args.no_rollout and not wait_for_rollout(args, tag, countries, query, clock, sleep):
        print("DEPLOY-WATCH: FAIL (not rolled)")
        return 1
    failed, reported, reading = read_checks(checks, args, countries, query, clock, sleep)
    for line in verdict_lines(checks, countries, failed, reported, reading):
        print(line)
    bad = sum(1 for f in failed.values() if f)
    print(f"DEPLOY-WATCH: {'FAIL' if bad else 'PASS'} ({bad} of {len(checks)} checks failed; {tag}; "
          f"{' '.join(countries)}; {reading} readings)")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
