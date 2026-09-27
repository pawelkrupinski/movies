#!/usr/bin/env python3
"""Retire venues the worker's closure sweep confirmed closed: the body of
.github/workflows/retire-venues.yml.

The worker (ClosureSweep) has already judged each venue closed from weeks of
evidence; this is the second, independent check, made at PR time and from
outside the fleet: each venue's own page on its aggregator must answer 404 or
410 NOW. Anything else, including a 200, a 403 from a runner the site blocks,
or a timeout, keeps the venue, and the PR body says why.

A venue that passes is appended to data/<roster>/retired.json (which the
generator drops; see retired_venues.py), the roster is regenerated, and the
roster size that CountrySpec pins and the comments quote is lowered to match.

Usage:
  python3 data/scripts/retire_venues.py --roster germany \\
      --venues '[{"id": "A1451", "name": "...", "evidence": "..."}]' --body pr-body.md
Prints the number retired; writes the PR body to --body.
"""
import argparse
import datetime
import json
import pathlib
import re
import subprocess
import sys
import urllib.error
import urllib.request

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import retired_venues  # noqa: E402

ROOT = pathlib.Path(__file__).resolve().parents[2]

# The page each roster's scraper reads, which the sweep saw answering 404/410.
PAGE = {
    "germany": "https://www.filmstarts.de/kinoprogramm/kino/{id}/",
    "spain":   "https://www.sensacine.com/cines/cine/{id}/",
    "us":      "https://www.flicks.us/cinema/{id}/",
}
GENERATE = {
    "germany": ["python3", "data/germany/scripts/generate_roster.py"],
    "spain":   ["python3", "data/spain/scripts/generate_roster.py"],
    "us":      ["python3", "data/us/scripts/generate_roster.py", "data/us/venues.json",
                "common/src/main/scala/models/UsRosterData.scala"],
}
# The line in CountrySpec that pins each roster's size.
PINNED_SIZE = {
    "germany": r"(Country\.Germany\.cities\.flatMap\(_\.cinemas\)\.size shouldBe )(\d+)",
    "spain":   r"(Country\.Spain\.cities\.flatMap\(_\.cinemas\)\.size shouldBe )(\d+)",
    "us":      r"(Country\.UnitedStates\.cities\.flatMap\(_\.cinemas\)\.size shouldBe )(\d+)",
}
COUNTRY_SPEC = ROOT / "common/src/test/scala/models/CountrySpec.scala"
GONE = {404, 410}
UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0 Safari/537.36"


def probe(url: str) -> str:
    """The page's HTTP status as text, or why there is none."""
    request = urllib.request.Request(url, headers={"User-Agent": UA})
    try:
        with urllib.request.urlopen(request, timeout=30) as response:
            return str(response.status)
    except urllib.error.HTTPError as error:
        return str(error.code)
    except Exception as error:  # noqa: BLE001 — any failure to get a status keeps the venue
        return f"no answer ({type(error).__name__})"


def decide(retired: dict, venues: list, page: str, status_of, today: str):
    """Split `venues` into the ones to retire (with their retired.json entries) and
    the ones kept, each with the live status that decided it."""
    added, kept = {}, []
    for venue in venues:
        venue_id = venue["id"]
        if venue_id in retired:
            kept.append((venue, "already retired"))
            continue
        status = status_of(page.format(id=venue_id))
        if status.isdigit() and int(status) in GONE:
            added[venue_id] = {"name": venue["name"], "reason": "closed", "retiredOn": today,
                               "evidence": f"{venue['evidence']} Re-checked {today}: HTTP {status}."}
        else:
            kept.append((venue, f"page answered {status}, not 404/410"))
    return added, kept


def rewrite_count(text: str, old: int, new: int) -> str:
    """`old` as prose writes it ("1,517") becomes `new`, only as a whole number."""
    return re.sub(rf"(?<![\d,]){re.escape(f'{old:,}')}(?![\d,])", f"{new:,}", text)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--roster", required=True, choices=sorted(PAGE))
    parser.add_argument("--venues", required=True)
    parser.add_argument("--body", required=True)
    args = parser.parse_args()

    directory = ROOT / "data" / args.roster
    retired = retired_venues.load(directory)
    today = datetime.date.today().isoformat()
    added, kept = decide(retired, json.loads(args.venues), PAGE[args.roster], probe, today)

    if added:
        (directory / "retired.json").write_text(
            json.dumps({**retired, **added}, ensure_ascii=False, indent=2) + "\n")
        subprocess.run(GENERATE[args.roster], cwd=ROOT, check=True)
        spec = COUNTRY_SPEC.read_text()
        old = int(re.search(PINNED_SIZE[args.roster], spec).group(2))
        new = old - len(added)
        COUNTRY_SPEC.write_text(re.sub(PINNED_SIZE[args.roster], rf"\g<1>{new}", spec))
        quoted = subprocess.run(["git", "grep", "-l", f"{old:,}", "--", "common", "worker", "web", f"data/{args.roster}/README.md"],
                                cwd=ROOT, capture_output=True, text=True).stdout.split()
        for path in quoted:
            file = ROOT / path
            file.write_text(rewrite_count(file.read_text(), old, new))

    lines = [f"Retires {len(added)} {args.roster} venue(s) the worker's closure sweep confirmed closed "
             f"and this run re-checked live.", ""]
    lines += [f"- **{e['name']}** (`{i}`): {e['evidence']}" for i, e in added.items()]
    if kept:
        lines += ["", "Kept, because the live re-check did not confirm it:", ""]
        lines += [f"- {v['name']} (`{v['id']}`): {why}" for v, why in kept]
    lines += ["", "If one has reopened, delete its `retired.json` entry and regenerate."]
    pathlib.Path(args.body).write_text("\n".join(lines) + "\n")
    print(len(added))
    return 0


if __name__ == "__main__":
    sys.exit(main())
