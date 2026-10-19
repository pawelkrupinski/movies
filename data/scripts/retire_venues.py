#!/usr/bin/env python3
"""Retire venues the worker's closure sweep confirmed closed: the body of
.github/workflows/retire-venues.yml.

The worker (ClosureSweep) has already judged each venue closed: its sources
dropped it for weeks AND the venue itself showed it had closed. This is the
second, independent check, made at PR time and from outside the fleet, and it
asks both questions again NOW:

  - the page the venue's primary scrapes must answer 404 or 410: the "page" the
    sweep sends (a venue scraped off its own site), its aggregator page otherwise; and
  - a page about the venue ITSELF (the sweep's `checks`) must show the closure:
    its own website gone (404/410, no DNS record), parked or for sale, or
    announcing a permanent closure; or a second listing answering 404/410.
    The phrases are closure-notices.json's, which the worker reads too.

An aggregator dropping a venue only proves we lost its programme: ACME
Screening Room was open with an October festival when flicks.us 404'd it. So a
venue with no venue-level check, with none that re-confirms the closure, or with
one that shows it alive (its own site answering with no closure notice) is
kept, and the PR body says why. Anything that is not an answer (a 403 from a
runner the site blocks, a timeout) re-confirms nothing.

A venue that passes is appended to data/<roster>/retired.json (which the
generator drops; see retired_venues.py), the roster is regenerated, and the
roster size that CountrySpec pins and the comments quote is lowered to match.

Usage:
  python3 data/scripts/retire_venues.py --roster germany \\
      --venues '[{"id": "A1451", "name": "...", "evidence": "...", "page": "https://...",
                  "checks": [{"kind": "website", "url": "https://..."}]}]' --body pr-body.md
Prints the number retired; writes the PR body to --body.
"""
import argparse
import datetime
import html.parser
import json
import pathlib
import re
import socket
import subprocess
import sys
import time
import urllib.error
import urllib.request

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import retired_venues  # noqa: E402

ROOT = pathlib.Path(__file__).resolve().parents[2]

# The page each roster's aggregator scraper reads, which the sweep saw answering 404/410 — re-checked
# for a venue the sweep sends no "page" of its own for.
PAGE = {
    "germany": "https://www.filmstarts.de/kinoprogramm/kino/{id}/",
    "spain":   "https://www.sensacine.com/cines/cine/{id}/",
    "us":      "https://www.flicks.us/cinema/{id}/",
}
# The commands that rebuild each roster without the retired venues, in order. A
# country with town pages rebuilds them first: its generator refuses a page that
# still lists a venue the roster dropped.
GENERATE = {
    "germany": [["python3", "data/germany/scripts/build_pages.py"],
                ["python3", "data/germany/scripts/generate_roster.py"]],
    "spain":   [["python3", "data/spain/scripts/build_pages.py"],
                ["python3", "data/spain/scripts/generate_roster.py"]],
    "us":      [["python3", "data/us/scripts/build_pages.py"],
                ["python3", "data/us/scripts/generate_roster.py"]],
}
# The line in CountrySpec that pins each roster's size.
PINNED_SIZE = {
    "germany": r"(Country\.Germany\.cities\.flatMap\(_\.cinemas\)\.size shouldBe )(\d+)",
    "spain":   r"(Country\.Spain\.cities\.flatMap\(_\.cinemas\)\.size shouldBe )(\d+)",
    "us":      r"(Country\.UnitedStates\.cities\.flatMap\(_\.cinemas\)\.size shouldBe )(\d+)",
}
COUNTRY_SPEC = ROOT / "common/src/test/scala/models/CountrySpec.scala"
NOTICES = json.loads((ROOT / "worker/src/main/resources/closure-notices.json").read_text())
NO_SUCH_HOST = "no such host"
GONE = {404, 410}
UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0 Safari/537.36"


def status_of(url: str) -> str:
    """The page's HTTP status as text, or why there is none."""
    request = urllib.request.Request(url, headers={"User-Agent": UA})
    try:
        with urllib.request.urlopen(request, timeout=30) as response:
            return str(response.status)
    except urllib.error.HTTPError as error:
        return str(error.code)
    except Exception as error:  # noqa: BLE001 — any failure to get a status keeps the venue
        return f"no answer ({type(error).__name__})"


def page_of(url: str) -> tuple[str, str]:
    """The page's HTTP status as text and its body (empty unless 2xx), NO_SUCH_HOST when
    its host has no DNS record, or why there is no answer."""
    request = urllib.request.Request(url, headers={"User-Agent": UA})
    try:
        with urllib.request.urlopen(request, timeout=30) as response:
            return str(response.status), response.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as error:
        return str(error.code), ""
    except urllib.error.URLError as error:
        if isinstance(error.reason, socket.gaierror) and error.reason.errno == socket.EAI_NONAME:
            return NO_SUCH_HOST, ""
        return f"no answer ({type(error.reason).__name__})", ""
    except Exception as error:  # noqa: BLE001 — any failure to get a status says nothing either way
        return f"no answer ({type(error).__name__})", ""


class _VisibleText(html.parser.HTMLParser):
    """The text a reader sees: everything outside <script> and <style>."""
    def __init__(self):
        super().__init__()
        self.parts, self._hidden = [], 0

    def handle_starttag(self, tag, attrs):
        if tag in ("script", "style"):
            self._hidden += 1

    def handle_endtag(self, tag):
        if tag in ("script", "style") and self._hidden:
            self._hidden -= 1

    def handle_data(self, data):
        if not self._hidden:
            self.parts.append(data)


def visible_text(body: str) -> str:
    parser = _VisibleText()
    parser.feed(body)
    return re.sub(r"\s+", " ", " ".join(parser.parts)).lower()


def website_signal(url: str, answer: str, body: str) -> tuple[str, str]:
    """("closure" | "life" | "silent", what was observed) for a venue's own site, judged as
    the worker's OwnSiteEvidence judges it. A page that merely lists no showtimes is life."""
    if answer == NO_SUCH_HOST:
        return "closure", f"own site {url} does not resolve (no DNS record)"
    if answer.isdigit() and int(answer) in GONE:
        return "closure", f"own site {url} answered HTTP {answer}"
    if answer.isdigit() and 200 <= int(answer) < 300:
        raw = body.lower()
        marker = next((m for m in NOTICES["parked"] if m in raw), None)
        if marker:
            return "closure", f'own site {url} is a parked or for-sale domain ("{marker}")'
        text = visible_text(body)
        phrase = next((p for p in NOTICES["closure"] if p in text), None)
        if phrase:
            return "closure", f'own site {url} announces the closure ("{phrase}")'
        return "life", f"own site {url} answered with no closure notice"
    return "silent", f"own site {url} could not be read ({answer})"


def listing_signal(url: str, answer: str) -> tuple[str, str]:
    """A second listing that had dropped the venue: still gone is a closure; listing it again is life."""
    if answer.isdigit() and int(answer) in GONE:
        return "closure", f"second listing {url} answered HTTP {answer}"
    if answer.isdigit() and 200 <= int(answer) < 300:
        return "life", f"second listing {url} answers HTTP {answer} again"
    return "silent", f"second listing {url} could not be read ({answer})"


def check(venue_check: dict, read_page) -> tuple[str, str]:
    """One venue-level check the sweep handed over, read live."""
    answer, body = read_page(venue_check["url"])
    if venue_check["kind"] == "website":
        return website_signal(venue_check["url"], answer, body)
    return listing_signal(venue_check["url"], answer)


def probe(url: str, fetch=status_of, sleep=time.sleep, attempts: int = 3) -> str:
    """The page's status, asking again when it did not answer at all. A status is the
    page's answer and stands; a blip must not keep a venue, because the sweep asks for
    a confirmed venue only once."""
    answer = fetch(url)
    for attempt in range(1, attempts):
        if answer.isdigit():
            break
        sleep(5 * attempt)
        answer = fetch(url)
    return answer


def decide(retired: dict, venues: list, page: str, status_of, today: str, read_page=page_of):
    """Split `venues` into the ones to retire as closed (with their retired.json entries)
    and the ones kept, each with the live answers that decided it."""
    added, kept = {}, []
    for venue in venues:
        venue_id = venue["id"]
        if venue_id in retired:
            kept.append((venue, "already retired"))
            continue
        status = status_of(venue.get("page") or page.format(id=venue_id))
        if not (status.isdigit() and int(status) in GONE):
            kept.append((venue, f"primary page answered {status}, not 404/410"))
            continue
        signals = [check(c, read_page) for c in venue.get("checks", [])]
        alive = [seen for kind, seen in signals if kind == "life"]
        shut = [seen for kind, seen in signals if kind == "closure"]
        if alive:
            kept.append((venue, f"source dropped, but the venue shows life: {'; '.join(alive)}"))
        elif not shut:
            seen = "; ".join(s for _, s in signals) or "no own website or second listing to check"
            kept.append((venue, f"source dropped, but nothing re-confirms the venue closed ({seen})"))
        else:
            added[venue_id] = {"name": venue["name"], "reason": "closed", "retiredOn": today,
                               "evidence": f"{venue['evidence']} Re-checked {today}: primary page HTTP {status}; "
                                           f"{'; '.join(shut)}."}
    return added, kept


def whole_number(count: int) -> str:
    """`count` as prose writes it ("1,517"), matched only as a whole number."""
    return rf"(?<![\d,]){re.escape(f'{count:,}')}(?![\d,])"


def rewrite_count(text: str, old: int, new: int) -> str:
    """`old` as prose writes it becomes `new`, only as a whole number."""
    return re.sub(whole_number(old), f"{new:,}", text)


def files_quoting(count: int, roster: str) -> list[str]:
    """The prose that quotes the roster size: Scala sources (their comments and CountrySpec) and the
    roster's README. Only text files, and only the count as a whole number — a size under 1,000
    has no comma to tell it apart, and Spain's 602 also sits in fonts, images and test fixtures."""
    return subprocess.run(
        ["git", "grep", "-I", "-l", "-P", whole_number(count), "--",
         "common/*.scala", "worker/*.scala", "web/*.scala", f"data/{roster}/README.md"],
        cwd=ROOT, capture_output=True, text=True).stdout.split()


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
        for command in GENERATE[args.roster]:
            subprocess.run(command, cwd=ROOT, check=True)
        spec = COUNTRY_SPEC.read_text()
        old = int(re.search(PINNED_SIZE[args.roster], spec).group(2))
        new = old - len(added)
        COUNTRY_SPEC.write_text(re.sub(PINNED_SIZE[args.roster], rf"\g<1>{new}", spec))
        for path in files_quoting(old, args.roster):
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
