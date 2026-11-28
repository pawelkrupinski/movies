#!/usr/bin/env python3
"""THE RECORDING'S PER-ORIGIN HEALTH: which origins refused most of what this recording asked them.

From 2026-10-07 to 10-13 every nightly `Record scrape fixtures` run was refused (HTTP 403) by Cineworld's
API on every detail read — ~350 a run — because the recorder lacked the residential-proxy fallback prod
has (fixed by f20a6b5dd). Every run went green; the first thing to fail was an identity capture six days
later, which found the UK's Cineworld listings holding no detail at all. This makes the recording itself
say so, the night it happens.

It reads a country's enrichment tree AFTER a recording leg has run over it, and keys everything by
origin (the URL's host) — no host lists, no per-site rules:

  * a response this recording got is a file under `<host>/...` with an mtime at or after `--since` (the
    recording run's start; a restored file keeps its archived, older mtime);
  * a failure it got is a `.enrichment-cache/**.entry` (FileEnrichmentCacheStore's encoding) whose
    fetchedAt is at or after `--since`. A 404 / 410 is an ANSWER — the URL does not exist, which the
    rating sites' slug probes are told most of the time by design — and counts as one. Anything else
    (403, 429, 5xx, a timeout, a circuit opened by them) is a REFUSAL.

An origin is flagged
  * `refusing` when it was asked at least MIN_REQUESTS things and refused more than half of them — the
    absolute floor, which needs no previous recording; and
  * `gone` when the previous green recording's manifest held at least MIN_REQUESTS of its responses and
    this tree holds none (every one expired and nothing was recorded in their place).

    recording_health.py <tree> --since <ISO-8601|epoch-seconds> [--previous previous.json]
                        [--write this.json] [--summary $GITHUB_STEP_SUMMARY] [--title <country>]

PROXY UNREACHABLE is no verdict on the origin: a refusal whose `proxy:` leg (FallbackHttpFetch's composite, the
recording's direct -> residential-proxy route) failed without reaching it -- `Tunnel failed`, a CONNECT that timed out
or was hung up on before any answer, a connect timeout or refusal -- says only that production's route was not there to
ask. An origin flagged `refusing` with most of its refusals so is `proxy-unreachable` instead: UNKNOWN, exit 3, never a
code red (Record scrape fixtures #2238: Decodo answered `Tunnel failed, got: 503` from 2026-10-27T17:00Z, and
Cineworld, which 403s CI's own address, was booked as refusing 66/66; in #2265 the same outage's silent ports hung up
on 53 of Cineworld's 156 asks, `HTTP/1.1 header parser received no bytes`).

AN ORIGIN DOWN FOR EVERYONE is no verdict on our route either (the owner, 2026-11-14, deciding Record scrape fixtures
#186): a refusal whose `direct:` AND `proxy:` legs both got an HTTP 5xx, or both timed out (Record scrape fixtures #207:
www.iluzjon.fn.org.pl let 25 of 28 asks time out on both legs, then answered in 0.3 s) -- the origin failing for CI and
for production's route alike, as www.kinomuza.pl answered 503 to all 22 of #186's asks and to every request for hours
after -- is the site's outage. An origin flagged `refusing` with most of its refusals so is `origin-down`: UNKNOWN, exit 3, a warning
and no red, and the leg keeps that origin's previous recording in the tree it publishes rather than an empty one
(keep-previous-origins.sh). A 403 / 429, or a 5xx on our direct path alone, is still `refusing`: we are being blocked.

BEST-EFFORT origins (BEST_EFFORT) are judged the same way but only WARN: OMDb is the one optional
source, whose free key runs out of its daily quota and then answers 401 to everything (run 469,
2026-10-20); the user chose to have recordings pass without it. Every other origin still fails the leg.

Exits 1 when any origin other than a best-effort one is flagged refusing or gone, after writing the manifest and the
table; 3 when the only such flags are proxy-unreachable or origin-down (UNKNOWN: the proxy, or the origin's own outage,
not the code; the manifest's `flags` says which); 2 when the tree (or the
`--previous` manifest) cannot be read, or holds no origin at all: no verdict. A missing `--previous`
file is the absolute floor alone (a country's first recording, or the manifest not yet published).
Tests: recording_health_test.py beside it.
"""
from __future__ import annotations

import argparse
import gzip
import json
import os
import re
import sys
import urllib.parse
import zlib
from dataclasses import dataclass
from typing import TypedDict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from report_fields import parse_instant  # noqa: E402

MIN_REQUESTS = 20       # below this an origin's share is noise: a handful of 503s from a site asked five things
REFUSING_SHARE = 0.5    # "fails for most requests"
NOT_FOUND = {"404", "410"}   # HttpStatusException.isDurable: a verdict about the URL, not a refusal
CACHE_DIRECTORY = ".enrichment-cache"
# Origins whose refusal or absence is reported but never fails a recording, with the name the warning uses.
BEST_EFFORT = {"www.omdbapi.com": "OMDb"}
# A composite failure's proxy leg that never reached the origin, in java.net.http's words: the proxy refused the tunnel
# (`Tunnel failed, got: 503`), let its CONNECT time out, or took the connection and hung up before a byte of any answer
# (`HTTP/1.1 header parser received no bytes`); or no connection was made at all.
PROXY_DOWN = re.compile(r"^\s*proxy: (?:[\w.]*\.)?(?:IOException: (?:Tunnel failed|HTTP/1\.1 header parser received no bytes)|"
                        r"HttpTimeoutException: proxy tunneling CONNECT request timed out|HttpConnectTimeoutException|"
                        r"ConnectException|NoRouteToHostException|UnknownHostException)", re.M)
# A composite failure whose every route reached the origin and got a server error, or whose request timed out on every
# route (a proxy CONNECT that timed out is PROXY_DOWN, above): the origin down for everyone.
_ORIGIN_FAILED = r"(?:[\w.]*\.)?(?:HttpStatusException: HTTP 5\d\d\b|HttpTimeoutException: request timed out)"
DIRECT_5XX = re.compile(r"^\s*direct: " + _ORIGIN_FAILED, re.M)
PROXY_5XX = re.compile(r"^\s*proxy: " + _ORIGIN_FAILED, re.M)
UNKNOWN_EXIT = 3
UNKNOWN_REASONS = ("proxy-unreachable", "origin-down")


class OriginCounts(TypedDict):
    """What one origin's tree and cache entries hold (the manifest's per-origin row)."""
    tree: int
    answered: int
    notFound: int
    refused: int
    refusals: dict[str, int]
    proxyDown: int
    originDown: int


class Manifest(TypedDict):
    since: float
    origins: dict[str, OriginCounts]


def _origin(key: str) -> str:
    """`BYTES https://host/...` / `GET https://...` (the cache's request-kind prefix) or a bare URL → host."""
    url = key.split(" ", 1)[1] if " " in key and not key.startswith("http") else key
    return urllib.parse.urlsplit(url).hostname or "?"


def _refusal(status: str, message: str) -> str:
    """A refusal's label: its HTTP status, else the exception's simple class name (`HttpTimeoutException`)."""
    if status:
        return status
    head = message.split(":", 1)[0].strip()
    return head.rsplit(".", 1)[-1] or "error"


def origin_counts() -> OriginCounts:
    return {"tree": 0, "answered": 0, "notFound": 0, "refused": 0, "refusals": {}, "proxyDown": 0, "originDown": 0}


def _count_responses(tree: str, since: float, origins: dict[str, OriginCounts]) -> None:
    """Count each origin's files under `tree`: all of them held, and those written at or after `since` answered."""
    for host in sorted(os.listdir(tree)):
        if host.startswith(".") or not os.path.isdir(os.path.join(tree, host)):
            continue
        counts = origins.setdefault(host, origin_counts())
        for directory, _, files in os.walk(os.path.join(tree, host)):
            counts["tree"] += len(files)
            counts["answered"] += sum(os.stat(os.path.join(directory, name)).st_mtime >= since for name in files)


def _read_failure(path: str, since: float) -> tuple[str, str, str] | None:
    """(key, status, message) of a `failed` cache entry fetched at or after `since`, else None (also undecodable)."""
    try:
        with gzip.open(path) as entry:
            fields = entry.read().decode("utf-8", "replace").split("\n", 5)
        key, kind, fetched_at, status, _method, message = fields
        fetched_at = int(fetched_at) / 1000
    except (OSError, ValueError, EOFError, zlib.error):
        return None   # undecodable: FileEnrichmentCacheStore skips it too
    return (key, status, message) if kind == "failed" and fetched_at >= since else None


def _count_failure(counts: OriginCounts, status: str, message: str) -> None:
    if status in NOT_FOUND:
        counts["notFound"] += 1
        return
    counts["refused"] += 1
    if PROXY_DOWN.search(message):
        counts["proxyDown"] += 1
    elif DIRECT_5XX.search(message) and PROXY_5XX.search(message):
        counts["originDown"] += 1
    label = _refusal(status, message)
    counts["refusals"][label] = counts["refusals"].get(label, 0) + 1


def manifest(tree: str, since: float) -> Manifest:
    """Per-origin counts for the recording that started at `since` (epoch seconds) over `tree`."""
    origins: dict[str, OriginCounts] = {}
    _count_responses(tree, since, origins)
    for directory, _, files in os.walk(os.path.join(tree, CACHE_DIRECTORY)):
        for name in files:
            failure = _read_failure(os.path.join(directory, name), since) if name.endswith(".entry") else None
            if failure:
                key, status, message = failure
                _count_failure(origins.setdefault(_origin(key), origin_counts()), status, message)
    return {"since": since, "origins": origins}


def asked(counts: OriginCounts) -> int:
    return counts["answered"] + counts["notFound"] + counts["refused"]


def _held(previous: Manifest | None) -> dict[str, OriginCounts]:
    """The previous green recording's per-origin counts (none for a country's first recording)."""
    return previous["origins"] if previous else {}


@dataclass
class Flag:
    origin: str
    reason: str   # `refusing` | `gone` | `proxy-unreachable` | `origin-down`

    @property
    def blocking(self):
        """False for a BEST_EFFORT origin: warned about, never failing the recording."""
        return self.origin not in BEST_EFFORT

    @property
    def unknown(self):
        """The residential proxy failed most of it, or the origin was down on every route: no verdict (exit 3)."""
        return self.reason in UNKNOWN_REASONS


def _refusing_flag(origin: str, counts: OriginCounts) -> 'Flag | None':
    """`refusing` / `proxy-unreachable` / `origin-down` for an origin that refused most of what it was asked, else None."""
    if asked(counts) < MIN_REQUESTS or counts["refused"] <= REFUSING_SHARE * asked(counts):
        return None
    if counts["proxyDown"] > REFUSING_SHARE * counts["refused"]:
        return Flag(origin, "proxy-unreachable")
    if counts["originDown"] > REFUSING_SHARE * counts["refused"]:
        return Flag(origin, "origin-down")
    return Flag(origin, "refusing")


def judge(this: Manifest, previous: Manifest | None) -> list['Flag']:
    """The flagged origins, in name order: see the module doc for the two rules."""
    flags: list[Flag] = []
    before = _held(previous)
    for origin in sorted(set(this["origins"]) | set(before)):
        counts = this["origins"].get(origin, origin_counts())
        flag = _refusing_flag(origin, counts)
        if flag:
            flags.append(flag)
        elif before.get(origin, {}).get("tree", 0) >= MIN_REQUESTS and counts["tree"] == 0:
            flags.append(Flag(origin, "gone"))
    return flags


def share(counts: OriginCounts) -> str:
    total = asked(counts)
    if total == 0:
        return "—"
    return f"{counts['refused']}/{total} ({round(100 * counts['refused'] / total)}%)"


def _verdict_cell(origin: str, flagged: dict[str, str]) -> str:
    if origin not in flagged:
        return "ok"
    reason = flagged[origin]
    return f"**{reason.upper()}{' (UNKNOWN)' if reason in UNKNOWN_REASONS else ''}{' (BEST EFFORT)' if origin in BEST_EFFORT else ''}**"


def _table_row(origin: str, counts: OriginCounts, was: OriginCounts | None, verdict: str) -> str:
    refusals = ", ".join(f"{label}×{n}" for label, n in sorted(counts["refusals"].items(), key=lambda kv: -kv[1]))
    return (f"| {origin} | {verdict} | {share(counts)} | {refusals or '—'} | {counts['tree']} | "
            f"{share(was) if was else 'n/a'} | {was['tree'] if was else 'n/a'} |")


def table(this: Manifest, previous: Manifest | None, flags: list[Flag]) -> str:
    """Markdown: every origin this recording asked or the previous one held, flagged ones first."""
    before = _held(previous)
    flagged = {flag.origin: flag.reason for flag in flags}
    origins = sorted(set(this["origins"]) | set(before), key=lambda o: (o not in flagged, o))
    lines = ["| origin | verdict | refused / asked | refusals | in tree | last green: refused / asked | last green: in tree |",
             "|---|---|---|---|---|---|---|"]
    lines += [_table_row(o, this["origins"].get(o, origin_counts()), before.get(o), _verdict_cell(o, flagged)) for o in origins]
    return "\n".join(lines)


def since(raw: str) -> float:
    try:
        return float(raw)
    except ValueError:
        return parse_instant(raw).timestamp()


DOCSTRING = __doc__ or ""  # None under python -OO


def _heading_title(title: str) -> str:
    return f" ({title})" if title else ""


def _load(args: argparse.Namespace) -> tuple[Manifest, Manifest | None]:
    """(this manifest, previous manifest or None); OSError / ValueError when the tree or `--previous` is unreadable."""
    this = manifest(args.tree, since(args.since))
    if not (args.previous and os.path.isfile(args.previous)):
        return this, None
    with open(args.previous) as handle:
        return this, json.load(handle)


def _unknown_notes(this: Manifest, unknown: list[Flag]) -> str:
    """The UNKNOWN paragraphs: origins whose refusals were the proxy's or the origin's own outage, not our block."""
    origins = this["origins"]
    unreachable = [f for f in unknown if f.reason == "proxy-unreachable"]
    down = [f for f in unknown if f.reason == "origin-down"]
    text = ""
    if unreachable:
        text += ("\n\n**UNKNOWN: the residential proxy was unreachable** for " +
                 ", ".join(f"{f.origin} ({origins[f.origin]['proxyDown']} of {origins[f.origin]['refused']} refusals)"
                           for f in unreachable) +
                 " -- production's route to them was not there to ask, so their health is not judged: fix the proxy, "
                 "not the code")
    if down:
        text += ("\n\n**UNKNOWN: down for everyone** -- " +
                 ", ".join(f"{f.origin} ({origins[f.origin]['originDown']} of {origins[f.origin]['refused']} "
                           "refusals a 5xx or a timeout both direct and through the proxy)" for f in down) +
                 ": the site's own outage, not a block on us; its previous recording is kept")
    return text


def _best_effort_notes(this: Manifest, flags: list[Flag]) -> str:
    return "".join(
        f"\n\n**{BEST_EFFORT[f.origin]} unavailable: {this['origins'].get(f.origin, origin_counts())['refused']} call(s) refused** "
        f"({f.origin} {f.reason}; best effort -- warned, not failing the recording)"
        for f in flags if not f.blocking)


def _verdict(this: Manifest, flags: list[Flag], blocking: list[Flag], unknown: list[Flag]) -> str:
    head = (f"**{len(blocking)} origin(s) flagged:** " + ", ".join(f"{f.origin} ({f.reason})" for f in blocking)
            if blocking else "No origin refused most of what it was asked, and none vanished.")
    return head + _unknown_notes(this, unknown) + _best_effort_notes(this, flags)


def _what(flag: Flag, counts: OriginCounts) -> str:
    """The annotation's account of why `flag` was raised."""
    if flag.reason == "refusing":
        return f"refused {share(counts)} of what this recording asked it"
    if flag.reason == "proxy-unreachable":
        return (f"refused {share(counts)} of what this recording asked it, {counts['proxyDown']} with the residential "
                "proxy unreachable -- proxy unreachable, the origin not judged")
    if flag.reason == "origin-down":
        return (f"answered {counts['originDown']} of {counts['refused']} refusals with a 5xx or a timeout both direct and "
                "through the proxy -- down for everyone, its previous recording kept")
    return "held responses in the last green recording and holds none now"


def _annotation(flag: Flag, counts: OriginCounts, title: str) -> str:
    level = "error" if flag.blocking and flag.reason != "origin-down" else "warning"
    return (f"::{level}::{'UNKNOWN: ' if flag.unknown and flag.blocking else ''}{title + ': ' if title else ''}"
            f"{flag.origin} {_what(flag, counts)}{'' if flag.blocking else ' (best effort: not failing the recording)'}")


def _report(args: argparse.Namespace, this: Manifest, previous: Manifest | None, flags: list[Flag],
            blocking: list[Flag], unknown: list[Flag]) -> str:
    basis = "against the last green recording" if previous else "absolute floor only — no previous manifest"
    return (f"### Recording health{_heading_title(args.title)}\n\n{_verdict(this, flags, blocking, unknown)} "
            f"({basis}; flagged at ≥{MIN_REQUESTS} asked and >{round(100 * REFUSING_SHARE)}% "
            f"refused, or ≥{MIN_REQUESTS} held last time and none now)\n\n{table(this, previous, flags)}\n")


def _parse(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=DOCSTRING.split("\n")[0])
    parser.add_argument("tree")
    parser.add_argument("--since", required=True, help="the recording's start: ISO-8601 or epoch seconds")
    parser.add_argument("--previous", help="the last green recording's manifest; a missing file is the floor alone")
    parser.add_argument("--write", help="where to write this recording's manifest")
    parser.add_argument("--summary", help="a markdown file to append the table to ($GITHUB_STEP_SUMMARY)")
    parser.add_argument("--title", default="", help="the country, for the table's heading")
    return parser.parse_args(argv)


def _no_verdict(args: argparse.Namespace, why: str) -> int:
    print(f"::error::recording health{_heading_title(args.title)}: {why}", file=sys.stderr)
    return 2


def _write_outputs(args: argparse.Namespace, this: Manifest, flags: list[Flag], report: str) -> None:
    if args.write:
        with open(args.write, "w") as handle:
            json.dump({**this, "flags": {f.origin: f.reason for f in flags}}, handle, indent=1, sort_keys=True)
    if args.summary:
        with open(args.summary, "a") as handle:
            handle.write(report + "\n")


def _exit_code(blocking: list[Flag], unknown: list[Flag]) -> int:
    if blocking:
        return 1
    return UNKNOWN_EXIT if unknown else 0


def main(argv: list[str]) -> int:
    args = _parse(argv)
    try:
        this, previous = _load(args)
    except (OSError, ValueError) as e:
        return _no_verdict(args, f"could not judge -- {type(e).__name__}: {e}")
    if not this["origins"]:
        return _no_verdict(args, f"{args.tree} holds no origin's responses -- no recording to judge")
    flags = judge(this, previous)
    blocking = [f for f in flags if f.blocking and not f.unknown]
    unknown = [f for f in flags if f.blocking and f.unknown]
    report = _report(args, this, previous, flags, blocking, unknown)
    print(report)
    _write_outputs(args, this, flags, report)
    for flag in flags:
        print(_annotation(flag, this["origins"].get(flag.origin, origin_counts()), args.title), file=sys.stderr)
    return _exit_code(blocking, unknown)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
