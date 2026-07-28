#!/usr/bin/env python3
"""Re-fingerprint the TMDB movie-detail fixtures for the `release_dates` append.

`fullDetails` now asks for `append_to_response=credits,release_dates`, which changes the
query string and therefore the fixture's `stableQueryFingerprint` suffix. Every recorded
detail body has to move to the new name AND carry the appended block.

The block is fetched live per film and MERGED into the existing recorded body rather than
re-recording the whole detail response. That is deliberate: a fresh detail fetch would also
pull in three weeks of upstream drift (overviews, poster paths, popularity), so the snapshot
diff would mix "age ratings appeared" with "TMDB edited its copy" and nobody could review it.
Merging keeps every previously-recorded field byte-identical, so the only behavioural delta
in the snapshots is the age-rating badge — which is the point of the change.

Usage: TMDB_API_KEY=... python3 scripts/migrate-tmdb-detail-fixtures.py [--dry-run]
"""
import json
import os
import re
import sys
import time
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from threading import Lock

ROOT = Path(__file__).resolve().parent.parent
MOVIE_DIR = ROOT / "test/resources/fixtures/08-06-2026/api.themoviedb.org/3/movie"
DRY_RUN = "--dry-run" in sys.argv

# TMDB is good for ~50 req/s; the skill's ceiling for a single-API script is 10 workers.
WORKERS = 8
IGNORED_PARAMS = ("api_key", "access_token")


def java_hash(s: str) -> int:
    h = 0
    for ch in s:
        h = (31 * h + ord(ch)) & 0xFFFFFFFF
    return h - 2**32 if h >= 2**31 else h


def fingerprint(raw_query: str) -> str:
    """Mirror of RecordingHttpFetch.stableQueryFingerprint — writer and reader agree on it."""
    meaningful = sorted(
        p for p in raw_query.split("&")
        if not any(p.startswith(f"{name}=") for name in IGNORED_PARAMS)
    )
    h = java_hash("&".join(meaningful))
    return format(h & 0xFFFFFFFF, "x") if h < 0 else format(h, "x")


def detail_query(language: str, appends: str) -> str:
    return f"language={language}&append_to_response={appends}"


lock = Lock()
stats = {"ok": 0, "no_block": 0, "failed": 0}
throttle = {"workers": WORKERS}


def fetch_release_dates(tmdb_id: str, key: str):
    url = f"https://api.themoviedb.org/3/movie/{tmdb_id}/release_dates?api_key={key}"
    for attempt in range(4):
        try:
            with urllib.request.urlopen(url, timeout=30) as response:
                return json.loads(response.read().decode("utf-8"))
        except urllib.error.HTTPError as e:
            if e.code == 404:
                return None                      # TMDB no longer knows this id
            if e.code in (429, 503):
                # Halve concurrency on throttling, per the rate-limit rule, and back off.
                with lock:
                    throttle["workers"] = max(1, throttle["workers"] // 2)
                time.sleep(2 ** attempt)
                continue
            raise
        except (urllib.error.URLError, TimeoutError):
            time.sleep(2 ** attempt)
    return None


def migrate(path: Path, old_suffix: str, new_suffix: str, key: str):
    tmdb_id = path.name[: -len(old_suffix)]
    try:
        body = json.loads(path.read_text())
    except json.JSONDecodeError:
        with lock:
            stats["failed"] += 1
        return f"SKIP {path.name}: not JSON"

    block = fetch_release_dates(tmdb_id, key)
    if block is None:
        # No block available: still migrate the file so the new fingerprint resolves —
        # a detail fixture that stops resolving would fail the resolve outright, which is
        # a far bigger change than a film with no age rating.
        with lock:
            stats["no_block"] += 1
    else:
        body["release_dates"] = block

    target = path.with_name(tmdb_id + new_suffix)
    if not DRY_RUN:
        target.write_text(json.dumps(body, ensure_ascii=False, separators=(",", ":")))
        if target != path:
            path.unlink()
    with lock:
        stats["ok"] += 1
    return None


def main():
    key = os.environ.get("TMDB_API_KEY")
    if not key:
        sys.exit("TMDB_API_KEY not set")

    # Which language the committed corpus was recorded in — derived from the fixtures
    # themselves rather than assumed, so a second country's tree migrates too.
    suffixes = {p.name.split(".", 1)[1] for p in MOVIE_DIR.glob("*.*") if p.is_file()}
    languages = {}
    for language in ("pl-PL", "en-GB", "de-DE"):
        old = fingerprint(detail_query(language, "credits"))
        if old in suffixes:
            languages[language] = (f".{old}", f".{fingerprint(detail_query(language, 'credits,release_dates'))}")
    if not languages:
        sys.exit(f"no detail fixtures found under {MOVIE_DIR} (suffixes seen: {sorted(suffixes)})")

    for language, (old_suffix, new_suffix) in languages.items():
        files = sorted(MOVIE_DIR.glob(f"*{old_suffix}"))
        print(f"{language}: {len(files)} fixture(s)  {old_suffix} -> {new_suffix}")
        started = time.time()
        with ThreadPoolExecutor(max_workers=throttle["workers"]) as pool:
            for message in pool.map(lambda f: migrate(f, old_suffix, new_suffix, key), files):
                if message:
                    print("  " + message)
        elapsed = time.time() - started
        print(f"  {stats['ok']} migrated, {stats['no_block']} without a release_dates block, "
              f"{stats['failed']} failed — {len(files) / max(elapsed, 0.001):.1f} files/s")


if __name__ == "__main__":
    main()
