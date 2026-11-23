#!/usr/bin/env python3
"""RECORDING PREFLIGHT: before a `Record scrape fixtures` run spends 5 legs x 30-125 min, ask OMDb and TMDB, once each,
whether they still take the run's keys.

WHY. In the week to 2026-10-26, 14 of 24 recording runs failed or were cancelled (~18 runner-hours), and the largest
single cause was OMDb refusing the key: runs 335, 408, 528, 563 and 2507 died on "OMDb refused its key N times in a
row" (OMDbClient's KeyRefusedException) 22-125 min in, after every leg had booted, restored and replayed. A rotated or
bad key is already visible in one request at t=0.

A SPENT DAILY QUOTA ONLY WARNS. OMDb is the recording's one best-effort origin -- the owner chose recordings that pass
without it (recording_health.py, BEST_EFFORT) -- and a recording's suite is no verdict (capture only), so nothing else in
the run fails on it: #2388 (run 3571) met OMDb's 401 from its first ask, 553 refusals in Germany alone, and went green.
A refused key is never remembered (CachingEnrichmentFetch), so each gap stays a gap for the hermetic replay to name and
the next fill whose key works to close. Failing here on it was the one red the quota still caused, and it recorded
nothing at all: the nightly #2426 met a day the workers had spent, CI still holding their shared 1,000-a-day key (the
vault had no OMDB_API_KEY_CI yet, infra/k8s-secrets/github-secrets.map).

Usage: recording-preflight.sh       (reads OMDB_API_KEY and TMDB_API_KEY; prints no value, not even a length)

A REFUSAL fails (exit 1): an unset key; TMDB's HTTP 401/403; OMDb's "Invalid API key!" (in a 401 or a 200 document) or
a 401/403 that says nothing else, which no new day restores. OMDb's "Request limit reached!" (its spent daily quota,
likewise either way) is a warning: the legs record without OMDb until its day turns at midnight US Central. Anything
else that is not an answer -- a timeout, a 5xx, no network -- is only a warning (exit 0): the run's own clients retry
those, and a preflight that fails on a blip would cost more runs than it saves.

Stdlib only (the probes through curl, as the shell tool's). record-scrape-fixtures.yml runs it by the name
scripts/ci/recording-preflight.sh; tested by recording_preflight_test.py beside it against a fake curl."""
from __future__ import annotations

import os
import re
import subprocess
import sys
import tempfile
from typing import List, Mapping, Sequence, Tuple

OMDB_ERROR = re.compile(r'"Error" *: *"([^"]*)"')
QUOTA = ("::warning::OMDb's daily quota is spent (HTTP {status}: {error}) — recording without OMDb, its best-effort "
         "origin: every ask is refused until OMDb's day turns at midnight US Central (05:00 UTC in summer time, 06:00 "
         "in winter) and left a gap for the next fill whose key works. Spent by this OMDb day's earlier recordings, or "
         "by prod's workers while CI holds their key (OMDB_API_KEY_CI, infra/k8s-secrets/github-secrets.map)")


def say(line: str) -> None:
    print(line, flush=True)


def probe(url: str, *headers: str) -> Tuple[str, str]:
    """(http status, the WHOLE body without its newlines): status 000 when there was no answer. The whole body: OMDb
    puts "Response" last, after the plot and the ratings, and a record cut at 400 bytes read as no record."""
    with tempfile.TemporaryDirectory() as scratch:
        body = os.path.join(scratch, "body")
        argv = ["curl", "-sS", "--max-time", "20", "--retry", "2", "--retry-all-errors", "-o", body, "-w",
                "%{http_code}", *headers, url]
        try:
            status = subprocess.run(argv, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True).stdout.strip()
        except OSError:
            status = ""
        text = ""
        if os.path.exists(body):
            with open(body, errors="replace") as f:
                text = f.read().replace("\n", "")
    return status or "000", text


class Preflight:
    def __init__(self, env: Mapping[str, str]) -> None:
        self.env = env
        self.problems: List[str] = []

    def refused(self, why: str) -> None:
        say(f"::error::{why}")
        self.problems.append(why)

    def omdb(self) -> None:
        key = self.env.get("OMDB_API_KEY", "")
        if not key:
            self.refused("secret OMDB_API_KEY is unset or empty — the recording's OMDb lookups would all fail")
            return
        self.omdb_answered(*probe(f"https://www.omdbapi.com/?i=tt0113277&apikey={key}"))

    def omdb_answered(self, status: str, body: str) -> None:
        found = OMDB_ERROR.search(f"{status} {body}")
        error = found.group(1) if found else ""
        if status == "200" and '"Response":"True"' in body:
            say("OMDb: key accepted")
        elif "limit reached" in error.lower():
            say(QUOTA.format(status=status, error=error))
        elif "invalid api key" in error.lower() or status in ("401", "403"):
            self.refused(f"OMDb refuses the key (HTTP {status}: {error or 'no reason given'}) — a bad or rotated key, "
                         "which no new day restores")
        elif status == "200":
            say(f"::warning::OMDb answered 200 without a record for the probe title ({error or 'no reason given'}); "
                "not treated as a refusal")
        else:
            say(f"::warning::OMDb did not answer the probe (HTTP {status}); going ahead, its clients retry")

    def tmdb(self) -> None:
        key = self.env.get("TMDB_API_KEY", "")
        if not key:
            self.refused("secret TMDB_API_KEY is unset or empty")
            return
        # Both the `api_key` parameter and the bearer header, as TmdbApiKey.authorization sends them: a v3 key and a v4
        # read token each authenticate through one of the two.
        status, _ = probe(f"https://api.themoviedb.org/3/configuration?api_key={key}", "-H",
                          f"Authorization: Bearer {key}")
        if status == "200":
            say("TMDB: key accepted")
        elif status in ("401", "403"):
            self.refused(f"TMDB refuses the key (HTTP {status})")
        else:
            say(f"::warning::TMDB did not answer the probe (HTTP {status}); going ahead, its clients retry")


def main(_argv: Sequence[str], env: Mapping[str, str]) -> int:
    preflight = Preflight(env)
    preflight.omdb()
    preflight.tmdb()
    if preflight.problems:
        say(f"recording preflight: {len(preflight.problems)} refusal(s) — not starting the legs")
        return 1
    say("recording preflight: no key refused")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(os.environ)))
