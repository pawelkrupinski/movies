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

Exits 1 when any origin is flagged, after writing the manifest and the table. A missing `--previous`
file is the absolute floor alone (a country's first recording, or the manifest not yet published).
Tests: recording_health_test.py beside it.
"""
import argparse
import datetime
import gzip
import json
import os
import sys
import urllib.parse
from dataclasses import dataclass

MIN_REQUESTS = 20       # below this an origin's share is noise: a handful of 503s from a site asked five things
REFUSING_SHARE = 0.5    # "fails for most requests"
NOT_FOUND = {"404", "410"}   # HttpStatusException.isDurable: a verdict about the URL, not a refusal
CACHE_DIRECTORY = ".enrichment-cache"


def _origin(key):
    """`BYTES https://host/...` / `GET https://...` (the cache's request-kind prefix) or a bare URL → host."""
    url = key.split(" ", 1)[1] if " " in key and not key.startswith("http") else key
    return urllib.parse.urlsplit(url).hostname or "?"


def _refusal(status, message):
    """A refusal's label: its HTTP status, else the exception's simple class name (`HttpTimeoutException`)."""
    if status:
        return status
    head = message.split(":", 1)[0].strip()
    return head.rsplit(".", 1)[-1] or "error"


def _origin_counts():
    return {"tree": 0, "answered": 0, "notFound": 0, "refused": 0, "refusals": {}}


def manifest(tree, since):
    """Per-origin counts for the recording that started at `since` (epoch seconds) over `tree`."""
    origins = {}
    tree = str(tree)
    for host in sorted(os.listdir(tree)):
        if host.startswith(".") or not os.path.isdir(os.path.join(tree, host)):
            continue
        counts = origins.setdefault(host, _origin_counts())
        for directory, _, files in os.walk(os.path.join(tree, host)):
            for name in files:
                counts["tree"] += 1
                if os.stat(os.path.join(directory, name)).st_mtime >= since:
                    counts["answered"] += 1
    for directory, _, files in os.walk(os.path.join(tree, CACHE_DIRECTORY)):
        for name in files:
            if not name.endswith(".entry"):
                continue
            try:
                with gzip.open(os.path.join(directory, name)) as entry:
                    fields = entry.read().decode("utf-8", "replace").split("\n", 5)
                key, kind, fetched_at, status, _method, message = fields
                fetched_at = int(fetched_at) / 1000
            except (OSError, ValueError, EOFError):
                continue   # undecodable: FileEnrichmentCacheStore skips it too
            if kind != "failed" or fetched_at < since:
                continue
            counts = origins.setdefault(_origin(key), _origin_counts())
            if status in NOT_FOUND:
                counts["notFound"] += 1
            else:
                counts["refused"] += 1
                label = _refusal(status, message)
                counts["refusals"][label] = counts["refusals"].get(label, 0) + 1
    return {"since": since, "origins": origins}


def asked(counts):
    return counts["answered"] + counts["notFound"] + counts["refused"]


@dataclass
class Flag:
    origin: str
    reason: str   # `refusing` | `gone`


def judge(this, previous):
    """The flagged origins, in name order: see the module doc for the two rules."""
    flags = []
    before = (previous or {}).get("origins", {})
    for origin in sorted(set(this["origins"]) | set(before)):
        counts = this["origins"].get(origin, _origin_counts())
        if asked(counts) >= MIN_REQUESTS and counts["refused"] > REFUSING_SHARE * asked(counts):
            flags.append(Flag(origin, "refusing"))
        elif before.get(origin, {}).get("tree", 0) >= MIN_REQUESTS and counts["tree"] == 0:
            flags.append(Flag(origin, "gone"))
    return flags


def _share(counts):
    total = asked(counts)
    if total == 0:
        return "—"
    return f"{counts['refused']}/{total} ({round(100 * counts['refused'] / total)}%)"


def table(this, previous, flags):
    """Markdown: every origin this recording asked or the previous one held, flagged ones first."""
    before = (previous or {}).get("origins", {})
    flagged = {flag.origin: flag.reason for flag in flags}
    origins = sorted(set(this["origins"]) | set(before), key=lambda o: (o not in flagged, o))
    lines = ["| origin | verdict | refused / asked | refusals | in tree | last green: refused / asked | last green: in tree |",
             "|---|---|---|---|---|---|---|"]
    for origin in origins:
        counts = this["origins"].get(origin, _origin_counts())
        was = before.get(origin)
        refusals = ", ".join(f"{label}×{n}" for label, n in sorted(counts["refusals"].items(), key=lambda kv: -kv[1]))
        verdict = f"**{flagged[origin].upper()}**" if origin in flagged else "ok"
        lines.append(f"| {origin} | {verdict} | {_share(counts)} | {refusals or '—'} | {counts['tree']} | "
                     f"{_share(was) if was else 'n/a'} | {was['tree'] if was else 'n/a'} |")
    return "\n".join(lines)


def _since(raw):
    try:
        return float(raw)
    except ValueError:
        return datetime.datetime.fromisoformat(raw.replace("Z", "+00:00")).timestamp()


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("tree")
    parser.add_argument("--since", required=True, help="the recording's start: ISO-8601 or epoch seconds")
    parser.add_argument("--previous", help="the last green recording's manifest; a missing file is the floor alone")
    parser.add_argument("--write", help="where to write this recording's manifest")
    parser.add_argument("--summary", help="a markdown file to append the table to ($GITHUB_STEP_SUMMARY)")
    parser.add_argument("--title", default="", help="the country, for the table's heading")
    args = parser.parse_args(argv)

    this = manifest(args.tree, _since(args.since))
    previous = None
    if args.previous and os.path.isfile(args.previous):
        with open(args.previous) as handle:
            previous = json.load(handle)
    flags = judge(this, previous)
    if args.write:
        with open(args.write, "w") as handle:
            json.dump(this, handle, indent=1, sort_keys=True)
    heading = f"### Recording health{f' ({args.title})' if args.title else ''}"
    basis = "against the last green recording" if previous else "absolute floor only — no previous manifest"
    verdict = (f"**{len(flags)} origin(s) flagged:** " + ", ".join(f"{f.origin} ({f.reason})" for f in flags)
               if flags else "No origin refused most of what it was asked, and none vanished.")
    report = (f"{heading}\n\n{verdict} ({basis}; flagged at ≥{MIN_REQUESTS} asked and >{round(100 * REFUSING_SHARE)}% "
              f"refused, or ≥{MIN_REQUESTS} held last time and none now)\n\n{table(this, previous, flags)}\n")
    print(report)
    if args.summary:
        with open(args.summary, "a") as handle:
            handle.write(report + "\n")
    for flag in flags:
        counts = this["origins"].get(flag.origin, _origin_counts())
        what = (f"refused {_share(counts)} of what this recording asked it" if flag.reason == "refusing"
                else "held responses in the last green recording and holds none now")
        print(f"::error::{args.title + ': ' if args.title else ''}{flag.origin} {what}", file=sys.stderr)
    return 1 if flags else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
