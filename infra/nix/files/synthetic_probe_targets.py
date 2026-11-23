#!/usr/bin/env python3
"""FINDS ONE LIVE FILM PAGE AND ITS SHARE CARD PER COUNTRY, FOR THE BLACKBOX PROBES TO FETCH.

A film page cannot be a static probe target: a film's page 404s the day its last screening passes, so any slug written
into the configuration becomes a permanent false ProbeFailing. This asks each country's city page which films it lists
RIGHT NOW, takes one, reads the share card its `og:image` names, and writes both as a Prometheus file_sd document that
the `blackbox` job re-reads on its own (no Prometheus restart). It runs every few minutes from
roles/synthetic-probes.nix (packaged with writePython3Bin: stdlib only, curl from the unit's path), so a film that
leaves the schedule is replaced long before ProbeFailing's hold could elapse on it.

STICKY. The film probed last time is kept for as long as its city page still lists it. The first film a city page
lists changes many times a day (Warsaw's changed five times in three hours on 2026-09-24), and every change is a new
`instance` -- a new series whose `for:` holds start from zero, so a ProbeSlow that needs twenty minutes on one URL
could never complete. A kept film whose film page and share card are both already targets is not fetched again: they
are carried over as they were. The film page was the costliest fetch a run made (a London film page is ~870 KB, read
from the origin every two minutes per country); a film with no card yet is still re-read, so a card rendered later is
picked up.

THE FILM PAGE IS PROBED THROUGH THE EDGE, 403 OR NOT. showtimes.cc's Cloudflare custom rule "Challenge non-verified-bot
traffic on catalog paths" answers `/movie/` with a managed challenge (a JavaScript page, `cf-mitigated: challenge`,
403) for any client that is not a verified bot -- a deliberate rule against scrapers, which blackbox_exporter (no
JavaScript) cannot pass. Since 2026-09-25 the rule exempts monitoring-1's address (`ip.src ne 128.140.49.167`), so its
film pages answer 200 here like kinowo.net's, and the film page is probed alongside its share card. Should the
exemption be lost (the rule edited, or monitoring-1's address changed), the probe reads 403 and ProbeBlockedByEdge
says so, naming the URL -- which is why a refused film page keeps its probe. Discovery must not go blind meanwhile, so
a page the edge answers 403 is then read from the ORIGIN (PROBE_ORIGIN_ADDRESS, over the private network, TLS pinned
to the origin certificates in PROBE_ORIGIN_CA): the share card it names is still found, and probed through the edge.

AND POSTERS PER POSTER HOST. The same city page names every poster its cards draw (`data-original-src`,
`data-fallbacks`), exactly as a browser is handed them -- through weserv (`images.weserv.nl/?url=...`) or direct. For
each country this takes posters per poster host (the origin's registrable domain, the same rule as the web's
`tools.PosterHost`: kinoheld.de, tmdb.org, savoysystems.co.uk ...), for the hosts that draw at least PROBE_POSTER_MIN of
the page's posters, the PROBE_POSTER_MAX biggest, and writes each as a `kind: poster` target with a `poster_host`
label, probed as an image (roles/synthetic-probes.nix `http_image`). That is the probe that would have caught
2026-10-21 within minutes: PosterProxy handed weserv every kinoheld poster scheme-less, weserv fetched it over http,
kinoheld answered 403, and weserv's 404 hid every kinoheld poster in Germany while the page, the pods and every counter
stayed green. The probe takes the URL off the page as served, so a fault in how the URL is BUILT is probed too.

A weserv poster is probed with an hourly `&kinowo_probe=<hour>` appended: weserv caches what it fetched, so a poster
probed under one fixed URL would keep answering from cache after its origin started refusing weserv (Multikino, Amazon
and AlloCine each did). A new key each hour makes weserv go to the origin once an hour, as it does for every poster a
visitor is the first to ask for. STICKY like the film: a poster probed for a host is kept while the page still lists
it. EVERY HOST IS PROBED, even one that refuses a datacentre address (this host is at Hetzner) and serves visitors
(Odeon, Multikino, biletyna): PosterProbeFailing holds while visitors' browsers report that host's posters loading
(posters.rules, `country_poster_host:browser_loads:served`), and pages once theirs fail too.

ALL OR NOTHING. If any country's discovery fails -- the city page did not answer, or listed no film -- the previous
document is left exactly as it was and the failure is logged. Replacing it with the countries that did answer would
silently drop a country's film probe on a transient error; keeping the old one means a discovery that stays broken
ends as a film page that 404s, which ProbeFailing reports, naming the URL.

Usage: synthetic-probe-targets <output.json> <country>=<city page URL> ...
Env:   PROBE_USER_AGENT     (the User-Agent every fetch sends; the probes send the same one)
       PROBE_TEXTFILE       (optional: a node_exporter textfile to publish
                             kinowo_synthetic_probe_discovery_last_success_timestamp_seconds into, written only when a
                             discovery succeeded, so ProbeDiscoveryStale can tell a discovery that keeps failing from
                             one that keeps working)
       PROBE_ORIGIN_ADDRESS (optional: the origin's address, for a page the edge answers 403)
       PROBE_ORIGIN_CA      (with it: the PEM file of origin certificates to trust)
       PROBE_POSTER_MIN     (posters a host must draw on a city page to be probed; default 10)
       PROBE_POSTER_MAX     (poster hosts probed per country, the biggest first; default 8)
       PROBE_HOUR           (the cache key's hour, YYYYMMDDHH; default the current UTC hour)"""
from __future__ import annotations

import collections
import datetime
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from typing import Any, Dict, List, Optional, Tuple, cast

NAME = "synthetic-probe-targets"
# The textfile the discovery's success writes, its `# TYPE` header spelled out whole (GrafanaMetricCoverageSpec reads
# the fleet's exported families from those headers)
DISCOVERY_METRIC = (
    "# HELP kinowo_synthetic_probe_discovery_last_success_timestamp_seconds When the synthetic probes last found a live"
    " film page for every country.\n"
    "# TYPE kinowo_synthetic_probe_discovery_last_success_timestamp_seconds gauge\n"
    "kinowo_synthetic_probe_discovery_last_success_timestamp_seconds %d\n")

# TWO PER HOST, so one dead image is not a host outage: a cinema that lists a poster it never uploaded (Cinema City,
# 2026-05-21) is one broken card whose fallback chain covers it, and PosterProbeFailing fires only when every poster
# probed for a host fails -- which a host-wide fault (2026-10-21: all of kinoheld's) makes them all do.
PER_HOST = 2
FILM_HREF = re.compile(r'href="([^"\n]*/movie/[^"/?#\n]+)"')
POSTER_ATTRIBUTE = re.compile(r'data-(?:original-src|fallbacks)="([^"\n]*)"')
# THE SHARE CARD IS OPTIONAL. A film whose card has not been rendered yet names its city's card instead
# (`/share-cards/<cc>/city-<slug>.jpg`), which is not the film's and not what this probe is for: a film card's name is
# `[a-z0-9]+`, a city card's always holds a `-`.
SHARE_CARD = re.compile(r'og:image"\s+content="([^"\n]*/share-cards/[a-z]{2}/[a-z0-9]+\.jpg[^"\n]*)"')
WESERV = "https://images.weserv.nl/"
SECOND_LEVELS = ("co", "com", "org", "net", "ac", "gov", "edu")
Target = Dict[str, Any]


def say(text: str) -> None:
    print("%s: %s" % (NAME, text), file=sys.stderr)


def poster_host(url: str) -> str:
    """The registrable domain of a poster's ORIGIN -- a weserv URL's `url=` target, decoded just enough to find its
    host -- by the rule tools.PosterHost applies on the web; "" when there is none."""
    if url.startswith(WESERV):
        target = re.search(r"[?&]url=([^&]*)", url)
        if not target:
            return ""
        url = re.sub("%2[Ff]", "/", re.sub("%3[Aa]", ":", target.group(1)))
    host = re.sub(r"[/:?#].*$", "", re.sub(r"^https?://", "", url, count=1).lower())
    labels = host.split(".") if host else []
    if len(labels) < 2:
        return ""
    keep = 3 if len(labels) >= 3 and len(labels[-1]) == 2 and labels[-2] in SECOND_LEVELS else 2
    return ".".join(labels[-keep:])


def poster_urls(page: str) -> List[Tuple[str, str]]:
    """Every poster URL a page hands a browser, as (poster host, url), in page order: each card's primary
    (`data-original-src`) and its fallback chain (`data-fallbacks`, `|`-separated), HTML-unescaped."""
    found: List[Tuple[str, str]] = []
    for value in POSTER_ATTRIBUTE.findall(page):
        for url in value.split("|"):
            url = url.replace("&amp;", "&")
            host = poster_host(url) if re.match(r"https?://", url) else ""
            if host:
                found.append((host, url))
    return found


def film_target(country: str, film: str) -> Target:
    return {"targets": [film], "labels": {"country": country, "kind": "film", "__param_module": "http_page",
                                          "__film": film}}


class Previous:
    """The document the last discovery wrote (none, or unreadable, is an empty one)."""

    def __init__(self, path: str) -> None:
        self.targets: List[Target] = []
        try:
            with open(path) as f:
                loaded: List[Any] = list(json.load(f))
            self.targets = [cast(Target, t) for t in loaded
                            if isinstance(t, dict) and isinstance(cast(Target, t).get("labels"), dict)]
        except (OSError, ValueError, TypeError):
            pass

    def of(self, country: str) -> List[Target]:
        return [t for t in self.targets if t["labels"].get("country") == country]

    def posters(self, country: str, host: str) -> List[str]:
        wanted = {"kind": "poster", "poster_host": host}
        return [t["labels"]["__poster"] for t in self.of(country)
                if all(t["labels"].get(k) == v for k, v in wanted.items()) and t["labels"].get("__poster")]

    def film(self, country: str) -> str:
        """The film this country's probes followed, carried in the hidden `__film` label (Prometheus drops `__` labels
        after relabelling, so it never reaches a series)."""
        return next((t["labels"]["__film"] for t in self.of(country) if t["labels"].get("__film")), "")

    def complete(self, country: str) -> Optional[List[Target]]:
        """This country's film targets when they are complete -- its film page AND a share card -- to carry over (its
        posters are found afresh every run). None otherwise: a film with no card yet is re-read so a card rendered later
        is found, and a card carried without its film page is rebuilt so the film page is probed again (the 2026-09-24
        discovery dropped a challenged film page and kept its card; carrying that left four countries' film pages
        unprobed for a week)."""
        kept = [t for t in self.of(country) if t["labels"].get("kind") != "poster"]
        kinds = {t["labels"].get("kind") for t in kept}
        return kept if {"share-card", "film"} <= kinds else None


class Fetcher:
    """curl, one page at a time, the body in a scratch file; a probe -- curl's failure IS the measurement, status
    000."""

    def __init__(self, work: str) -> None:
        self.body = os.path.join(work, "body")
        self.agent = os.environ.get("PROBE_USER_AGENT", "kinowo-synthetic-probe/1")
        self.origin_address = os.environ.get("PROBE_ORIGIN_ADDRESS", "")
        self.status = "000"
        self.edge_status = "000"

    def fetch(self, url: str, origin: bool = False) -> Optional[str]:
        """The page's body on a 2xx (its status in .status either way)."""
        args = ["curl", "-sS", "--compressed", "--max-time", "20", "-A", self.agent, "-o", self.body, "-w",
                "%{http_code}"]
        if origin:
            host = re.sub(r"^https?://([^/:]+).*", r"\1", url, flags=re.S)
            args += ["--resolve", "%s:443:%s" % (host, self.origin_address),
                     "--cacert", os.environ.get("PROBE_ORIGIN_CA", "")]
        open(self.body, "w").close()
        done = subprocess.run(args + [url], stdout=subprocess.PIPE)
        self.status = done.stdout.decode(errors="replace") or "000"
        if not self.status.startswith("2"):
            return None
        with open(self.body, "rb") as f:
            return f.read().decode(errors="replace")

    def page(self, url: str) -> Optional[str]:
        """Through the edge; on the edge's 403, from the origin when one is configured. .edge_status keeps the edge's
        answer, so the caller can tell a refused page from a missing one."""
        body = self.fetch(url)
        self.edge_status = self.status
        if body is not None or self.status != "403" or not self.origin_address:
            return body
        return self.fetch(url, origin=True)


class Discovery:
    def __init__(self, out: str, work: str) -> None:
        self.out = out
        self.previous = Previous(out)
        self.fetcher = Fetcher(work)
        self.poster_min = int(os.environ.get("PROBE_POSTER_MIN", "10"))
        self.poster_max = int(os.environ.get("PROBE_POSTER_MAX", "8"))
        self.hour = os.environ.get("PROBE_HOUR") or datetime.datetime.now(datetime.timezone.utc).strftime("%Y%m%d%H")
        self.targets: List[Target] = []
        self.failed = False

    def poster_targets(self, country: str, page: str) -> List[Target]:
        """This country's poster targets: PER_HOST posters for each host drawing at least the floor, the biggest
        hosts first; each probed poster kept while the page lists it."""
        posters = poster_urls(page)
        listed = set(posters)
        counts = collections.Counter(host for host, _ in posters)
        hosts = sorted(counts, key=lambda h: (-counts[h], h))
        targets: List[Target] = []
        for host in [h for h in hosts if counts[h] >= self.poster_min][:self.poster_max]:
            kept = [u for u in self.previous.posters(country, host) if (host, u) in listed]
            chosen = list(dict.fromkeys(kept + [u for h, u in posters if h == host]))[:PER_HOST]
            targets += [self.poster_target(country, host, url) for url in chosen]
        return targets

    def poster_target(self, country: str, host: str, url: str) -> Target:
        probe = url + "&kinowo_probe=" + self.hour if url.startswith(WESERV) else url
        return {"targets": [probe], "labels": {"country": country, "kind": "poster", "poster_host": host,
                                               "__param_module": "http_image", "__poster": url}}

    def film(self, country: str, film: str) -> None:
        page = self.fetcher.page(film)
        self.targets.append(film_target(country, film))
        if page is None:
            # Still probed: a film page that fails is exactly what ProbeFailing is for.
            say("%s: the film page %s did not answer (HTTP %s)" % (country, film, self.fetcher.status))
            return
        if self.fetcher.edge_status == "403":
            say("%s: the edge refuses %s (403), so monitoring-1's Cloudflare exemption is lost; read from the origin, "
                "still probed" % (country, film))
        card = SHARE_CARD.search(page)
        if card:
            self.targets.append({"targets": [card.group(1)], "labels": {
                "country": country, "kind": "share-card", "__param_module": "http_asset", "__film": film}})

    def country(self, country: str, city: str) -> None:
        origin = re.sub(r"^(https?://[^/]+).*", r"\1", city, flags=re.S)
        page = self.fetcher.page(city)
        if page is None:
            say("%s: could not fetch the city page %s (HTTP %s)" % (country, city, self.fetcher.status))
            self.failed = True
            return
        films = [h if re.match(r"https?://", h) else origin + h for h in FILM_HREF.findall(page)]
        self.targets += self.poster_targets(country, page)
        if not films:
            say("%s: %s lists no film" % (country, city))
            self.failed = True
            return
        film = self.previous.film(country)
        kept = self.previous.complete(country) if film in films else None
        if kept is not None:
            self.targets += kept
            return
        self.film(country, film if film in films else films[0])

    def write(self) -> int:
        if self.failed:
            say("keeping the previous %s (discovery failed above)" % self.out)
            return 0
        if not publish(self.out, json.dumps(self.targets, indent=2, ensure_ascii=False) + "\n"):
            say("could not write " + self.out)
            return 1
        print("%s: wrote %d target(s) to %s" % (NAME, len(self.targets), self.out))
        textfile = os.environ.get("PROBE_TEXTFILE")
        if textfile:
            publish(textfile, DISCOVERY_METRIC % int(time.time()), mode=0o644)
        return 0


def publish(path: str, text: str, mode: Optional[int] = None) -> bool:
    """`text` into `path` through a temporary beside it, renamed into place (file_sd reads it on write)."""
    temporary = "%s.tmp.%d" % (path, os.getpid())
    try:
        with open(temporary, "w") as f:
            f.write(text)
        if mode is not None:
            os.chmod(temporary, mode)
        os.replace(temporary, path)
    except OSError:
        if os.path.exists(temporary):
            os.remove(temporary)
        return False
    return True


def main(argv: List[str]) -> int:
    # EVERY TOOL IT RUNS, OR NONE OF IT. On 2026-10-21 the unit was given no awk, so every discovery for days wrote
    # film targets and no poster's, exited 0 and logged "wrote 10 target(s)" -- PosterProbesMissing paging for all
    # five countries while the unit was green. curl is the one tool left.
    if not shutil.which("curl"):
        say("curl is not on PATH")
        return 1
    if not argv:
        print("usage: synthetic-probe-targets <output.json> <country>=<city url> ...", file=sys.stderr)
        return 1
    with tempfile.TemporaryDirectory() as work:
        discovery = Discovery(argv[0], work)
        for pair in argv[1:]:
            country, _, city = pair.partition("=")
            discovery.country(country, city or pair)
        return discovery.write()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
