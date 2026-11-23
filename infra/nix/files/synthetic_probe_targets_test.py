#!/usr/bin/env python3
"""The synthetic probes' discovery (infra/nix/files/synthetic-probe-targets, roles/synthetic-probes.nix), black-box:
one live film page and its share card per country, sticky while the city page lists the film, read from the origin
when the edge refuses it, all or nothing, and two posters per poster host as the page hands them to a browser.
Wrong, it writes a target that 404s (a paging ProbeFailing for a site that is up) or throws away the last good
targets on a transient error. Driven against a fake `curl` serving pages from the case's directory, offline. The
other half -- the scrape job monitoring-1 installs, and the discovery run with only its unit's PATH -- is
infra/test/test_synthetic_probes.sh. The same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import shutil
import sys
import unittest
from typing import Any, Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__))))), "scripts", "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("infra/nix/files/synthetic_probe_targets.py", "infra/nix/files/synthetic-probe-targets.sh")

# Serves pages/<url with / and : as _> -- or, for a fetch pinned to the origin with --resolve, pages/origin/<same>. A
# page's status is 200, or what its .status file says; a missing page is a 404. Honours -o and -w '%{http_code}'.
CURL = """
url, body, write, pages = argv[-1], None, False, env["FAKE_PAGES"]
i = 0
while i < len(argv) - 1:
    if argv[i] == "-o":
        body = argv[i + 1]
    elif argv[i] == "-w":
        write = True
    elif argv[i] == "--resolve":
        pages = os.path.join(pages, "origin")
    i += 1
page = os.path.join(pages, url.replace("/", "_").replace(":", "_"))
code = "404"
if os.path.exists(page):
    code = open(page + ".status").read() if os.path.exists(page + ".status") else "200"
    with open(page, "rb") as source, open(body or "/dev/stdout", "wb") as target:
        target.write(source.read())
if write:
    sys.stdout.write(code)
"""
# systemd runs every service with SIGPIPE ignored (IgnoreSIGPIPE=yes): a closed pipe is then an EPIPE its writer
# reports on stderr ("grep: write error: Broken pipe") rather than a silent death -- so the tool runs so here too.
IGNORING_SIGPIPE = "trap '' PIPE\nexec \"$TOOL_UNDER_TEST\" \"$@\""
WESERV = "https://images.weserv.nl/?url="
HINTS = "&amp;w=480&amp;h=720&amp;fit=cover&amp;output=webp"
SERVED_HINTS = "&w=480&h=720&fit=cover&output=webp"
WARSAW, LONDON, NEW_YORK, BERLIN = ("https://kinowo.net/warszawa/", "https://showtimes.cc/uk/london/",
                                    "https://showtimes.cc/us/new-york/", "https://showtimes.cc/de/berlin/")
COYOTE = NEW_YORK + "movie/coyote-vs-acme"
COYOTE_CARD = "https://showtimes.cc/share-cards/us/h2dd4.jpg?v=01dd"
ORIGIN = {"PROBE_ORIGIN_ADDRESS": "10.20.0.12", "PROBE_ORIGIN_CA": "/etc/origin.pem"}


def og_image(url: str, spacing: str = " ") -> str:
    return '<meta property="og:image"%scontent="%s"></html>' % (spacing, url)


def berlin_page() -> str:
    """2026-10-21's page: kinoheld's posters handed to weserv SCHEME-LESS (the bug, 5a77a8867), TMDB's with https,
    an Amazon poster direct, a Multikino one direct (probed like any other) and a venue with a single poster (under
    the floor)."""
    cards = "".join(
        '<a href="/de/berlin/movie/f%d">x</a><img src="x" data-original-src="%sstatic.kinoheld.de%%2Fimages%%2Ffilm'
        '%%2Ff%d.jpg%s" data-fallbacks="%shttps%%3A%%2F%%2Fimage.tmdb.org%%2Ft%%2Fp%%2Fw500%%2Ff%d.jpg%s|'
        'https://m.media-amazon.com/images/f%d.jpg">' % (i, WESERV, i, HINTS, WESERV, i, HINTS, i) for i in (1, 2, 3))
    multikino = "".join('<img data-original-src="https://www.multikino.pl/-/media/%s.jpg">' % n for n in "abc")
    return cards + multikino + '<img data-original-src="%swww.kino-lumiere.de%%2Fa.jpg%s"></html>' % (WESERV, HINTS)


def kinoheld(*names: str, hour: str) -> List[str]:
    return ["%sstatic.kinoheld.de%%2Fimages%%2Ffilm%%2F%s.jpg%s&kinowo_probe=%s" % (WESERV, n, SERVED_HINTS, hour)
            for n in names]


class SyntheticProbeTargetsTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        os.makedirs(self.path("pages", "origin"))
        self.fakes.program("curl", CURL)
        self.wrapper = self.fakes.script("ignoring-sigpipe", IGNORING_SIGPIPE)
        self.targets = self.path("targets.json")
        self.serve(WARSAW, '<a href="/warszawa/movie/100-dni-misja-zeus">x</a><a href="/warszawa/movie/other">y</a>'
                           '</html>')
        self.serve(WARSAW + "movie/100-dni-misja-zeus",
                   og_image("https://kinowo.net/share-cards/pl/had62.jpg?v=b179", spacing="        "))
        self.serve(LONDON, '<a href="/uk/london/movie/bad-apples">x</a></html>')
        self.serve(LONDON + "movie/bad-apples", og_image("https://showtimes.cc/share-cards/uk/city-london.jpg"))
        self.serve(NEW_YORK, '<a href="/us/new-york/movie/coyote-vs-acme">x</a></html>')
        self.serve(COYOTE, og_image(COYOTE_CARD))

    def page(self, url: str, origin: bool = False) -> str:
        return self.path("pages", *(["origin"] if origin else []), url.replace("/", "_").replace(":", "_"))

    def serve(self, url: str, body: str, origin: bool = False, status: Optional[str] = None) -> None:
        with open(self.page(url, origin), "w") as f:
            f.write(body)
        if status:
            with open(self.page(url, origin) + ".status", "w") as f:
                f.write(status)

    def refuse(self, url: str) -> None:
        """The edge's managed challenge: a 403 with a JavaScript page."""
        self.serve(url, "<title>Just a moment...</title></html>", status="403")

    def discover(self, *pairs: str, env: Optional[Dict[str, Optional[str]]] = None) -> cli_harness.Done:
        self.fakes.clear()
        extra: Dict[str, Optional[str]] = {"FAKE_PAGES": self.path("pages"), "TOOL_UNDER_TEST": self.TOOL,
                                           "PROBE_TEXTFILE": self.path("probes.prom")}
        extra.update(env or {})
        return self.run_tool(self.targets, *pairs, env=extra, tool=self.wrapper)

    def found(self) -> List[Dict[str, Any]]:
        with open(self.targets) as f:
            return json.load(f)

    def kinds(self) -> List[str]:
        return ["%s %s" % (t["labels"]["kind"], t["targets"][0]) for t in self.found()]

    def of(self, kind: str, **labels: str) -> List[str]:
        return [t["targets"][0] for t in self.found() if t["labels"]["kind"] == kind
                and all(t["labels"].get(k) == v for k, v in labels.items())]

    def fetched(self) -> List[str]:
        return [call[-1] for call in self.fakes.calls("curl")]

    def test_a_film_per_country_and_its_share_card_when_it_has_one(self) -> None:
        done = self.discover("pl=" + WARSAW, "uk=" + LONDON)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([WARSAW + "movie/100-dni-misja-zeus", LONDON + "movie/bad-apples"], self.of("film"))
        self.assertEqual(["http_page", "http_page"],
                         [t["labels"]["__param_module"] for t in self.found() if t["labels"]["kind"] == "film"])
        self.assertEqual([{"targets": ["https://kinowo.net/share-cards/pl/had62.jpg?v=b179"],
                           "labels": {"country": "pl", "kind": "share-card", "__param_module": "http_asset",
                                      "__film": WARSAW + "movie/100-dni-misja-zeus"}}],
                         [t for t in self.found() if t["labels"]["kind"] == "share-card"])
        self.assertEqual("synthetic-probe-targets: wrote 3 target(s) to %s\n" % self.targets, done.stdout)
        prom = self.read("probes.prom").splitlines()
        self.assertEqual("# TYPE kinowo_synthetic_probe_discovery_last_success_timestamp_seconds gauge", prom[1])
        self.assertRegex(prom[2], r"^kinowo_synthetic_probe_discovery_last_success_timestamp_seconds \d{10}$")
        self.assertEqual(0o644, os.stat(self.path("probes.prom")).st_mode & 0o777)
        self.assertTrue(all("--compressed" in call for call in self.fakes.calls("curl")))
        self.assertTrue(all(["-A", "kinowo-synthetic-probe/1"] == call[call.index("-A"):call.index("-A") + 2]
                            for call in self.fakes.calls("curl")))

    def test_all_or_nothing_a_failed_country_leaves_the_previous_targets_untouched(self) -> None:
        self.discover("pl=" + WARSAW)
        before = self.read("targets.json")
        os.remove(self.path("probes.prom"))
        self.serve(BERLIN, "<p>no films today</p></html>")
        done = self.discover("pl=" + WARSAW, "de=" + BERLIN)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(before, self.read("targets.json"))
        self.assertIn("synthetic-probe-targets: de: %s lists no film\n" % BERLIN, done.stderr)
        self.assertIn("synthetic-probe-targets: keeping the previous %s (discovery failed above)\n" % self.targets,
                      done.stderr)
        self.assertFalse(os.path.exists(self.path("probes.prom")))
        done = self.discover("pl=" + WARSAW, "es=https://showtimes.cc/es/madrid/")
        self.assertEqual(before, self.read("targets.json"))
        self.assertIn("synthetic-probe-targets: es: could not fetch the city page https://showtimes.cc/es/madrid/ "
                      "(HTTP 404)\n", done.stderr)

    def test_a_film_that_left_the_schedule_is_replaced_and_the_one_probed_stays_while_listed(self) -> None:
        self.discover("pl=" + WARSAW)
        self.serve(WARSAW, '<a href="/warszawa/movie/next-film">x</a></html>')
        self.serve(WARSAW + "movie/next-film", og_image("https://kinowo.net/share-cards/pl/hnext.jpg?v=1"))
        self.discover("pl=" + WARSAW)
        self.assertEqual([WARSAW + "movie/next-film"], self.of("film"))
        # A real city page lists hundreds of films: big enough that a reader stopping at the first match would close
        # the pipe on a grep still writing.
        many = "".join('<a href="/warszawa/movie/film-%d">f</a>' % i for i in range(1, 20001))
        self.serve(WARSAW, '<a href="/warszawa/movie/newly-first">x</a><a href="/warszawa/movie/next-film">y</a>%s'
                           '</html>' % many)
        done = self.discover("pl=" + WARSAW)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["film " + WARSAW + "movie/next-film", "share-card https://kinowo.net/share-cards/pl/hnext.jpg?v=1"],
                         self.kinds())
        self.assertEqual({WARSAW + "movie/next-film"}, {t["labels"]["__film"] for t in self.found()})
        self.assertNotIn("Broken pipe", done.stderr)
        self.assertEqual([WARSAW], self.fetched(), "the kept film's page is not fetched again")

    def test_a_kept_film_with_no_share_card_yet_is_fetched_again(self) -> None:
        self.serve(WARSAW, '<a href="/warszawa/movie/cardless">x</a></html>')
        self.serve(WARSAW + "movie/cardless", og_image("https://kinowo.net/assets/img/og.jpg"))
        self.discover("pl=" + WARSAW)
        self.assertEqual(["film " + WARSAW + "movie/cardless"], self.kinds())
        self.serve(WARSAW + "movie/cardless", og_image("https://kinowo.net/share-cards/pl/hcard.jpg?v=2"))
        self.discover("pl=" + WARSAW)
        self.assertEqual(["film " + WARSAW + "movie/cardless", "share-card https://kinowo.net/share-cards/pl/hcard.jpg?v=2"],
                         self.kinds())

    def test_a_film_page_the_edge_answers_is_probed_with_its_card_and_the_origin_never_asked(self) -> None:
        self.discover("us=" + NEW_YORK, env=dict(ORIGIN))
        self.assertEqual(["film " + COYOTE, "share-card " + COYOTE_CARD], self.kinds())
        self.assertFalse(any("--resolve" in call for call in self.fakes.calls("curl")))

    def test_carried_targets_that_lack_their_film_page_get_it_back(self) -> None:
        # The 2026-09-24 discovery dropped a challenged film page and kept only its card; carried as-is, that left
        # four countries with no film probe for a week after the edge answered 200 again.
        self.write("targets.json", json.dumps([{"targets": [COYOTE_CARD], "labels": {
            "country": "us", "kind": "share-card", "__param_module": "http_asset", "__film": COYOTE}}]))
        self.discover("us=" + NEW_YORK)
        self.assertEqual(["film " + COYOTE, "share-card " + COYOTE_CARD], self.kinds())

    def test_a_film_page_the_edge_challenges_is_read_from_the_origin_and_still_probed(self) -> None:
        self.refuse(COYOTE)
        self.serve(COYOTE, og_image(COYOTE_CARD), origin=True)
        done = self.discover("us=" + NEW_YORK, env=dict(ORIGIN))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["film " + COYOTE, "share-card " + COYOTE_CARD], self.kinds())
        pinned = [c for c in self.fakes.calls("curl") if "--resolve" in c]
        self.assertEqual([["--resolve", "showtimes.cc:443:10.20.0.12", "--cacert", "/etc/origin.pem", COYOTE]],
                         [c[-5:] for c in pinned])
        self.assertIn("synthetic-probe-targets: us: the edge refuses %s (403), so monitoring-1's Cloudflare exemption "
                      "is lost; read from the origin, still probed\n" % COYOTE, done.stderr)
        self.assertEqual({COYOTE}, {t["labels"]["__film"] for t in self.found()})

    def test_with_no_origin_a_refused_film_page_is_still_probed(self) -> None:
        self.refuse(COYOTE)
        done = self.discover("us=" + NEW_YORK)
        self.assertEqual(["film " + COYOTE], self.kinds())
        self.assertIn("synthetic-probe-targets: us: the film page %s did not answer (HTTP 403)\n" % COYOTE, done.stderr)

    def test_two_posters_per_poster_host_as_the_city_page_hands_them_to_a_browser(self) -> None:
        self.serve(BERLIN, berlin_page())
        self.serve(BERLIN + "movie/f1", og_image("https://showtimes.cc/share-cards/de/hf1.jpg"))
        done = self.discover("de=" + BERLIN, env={"PROBE_POSTER_MIN": "3", "PROBE_HOUR": "2026102117"})
        self.assertEqual(0, done.code, done.output)
        posters = [t for t in self.found() if t["labels"]["kind"] == "poster"]
        self.assertEqual(["kinoheld.de"] * 2 + ["media-amazon.com"] * 2 + ["multikino.pl"] * 2 + ["tmdb.org"] * 2,
                         sorted(t["labels"]["poster_host"] for t in posters))
        self.assertEqual(kinoheld("f1", "f2", hour="2026102117"), self.of("poster", poster_host="kinoheld.de"))
        self.assertEqual("https://m.media-amazon.com/images/f1.jpg", self.of("poster", poster_host="media-amazon.com")[0])
        self.assertEqual({"http_image de"}, {"%s %s" % (t["labels"]["__param_module"], t["labels"]["country"])
                                             for t in posters})
        self.assertEqual(WESERV + "static.kinoheld.de%2Fimages%2Ffilm%2Ff1.jpg" + SERVED_HINTS,
                         posters[0]["labels"]["__poster"])
        self.assertEqual(["film", "share-card"], sorted(t["labels"]["kind"] for t in self.found() if t not in posters))

    def test_a_probed_poster_stays_while_the_page_lists_it(self) -> None:
        self.serve(BERLIN, berlin_page())
        self.serve(BERLIN + "movie/f1", og_image("https://showtimes.cc/share-cards/de/hf1.jpg"))
        self.discover("de=" + BERLIN, env={"PROBE_POSTER_MIN": "3", "PROBE_HOUR": "2026102117"})
        self.serve(BERLIN, '<img data-original-src="%sstatic.kinoheld.de%%2Fimages%%2Ffilm%%2Fnew.jpg%s">%s'
                   % (WESERV, HINTS, berlin_page()))
        self.discover("de=" + BERLIN, env={"PROBE_POSTER_MIN": "3", "PROBE_HOUR": "2026102118"})
        self.assertEqual(kinoheld("f1", "f2", hour="2026102118"), self.of("poster", poster_host="kinoheld.de"))
        self.serve(BERLIN, berlin_page().replace("f1.jpg", "gone.jpg"))
        self.discover("de=" + BERLIN, env={"PROBE_POSTER_MIN": "3", "PROBE_HOUR": "2026102118"})
        self.assertEqual(kinoheld("f2", "gone", hour="2026102118"), self.of("poster", poster_host="kinoheld.de"))

    def test_a_poster_host_is_its_registrable_domain_and_the_floor_holds(self) -> None:
        self.serve(LONDON, '<a href="/uk/london/movie/bad-apples">x</a>'
                   '<img data-original-src="%shttps%%3A%%2F%%2Farthousecrouchend.savoysystems.co.uk%%2Fa.png%s">'
                   '<img data-original-src="%shttps%%3A%%2F%%2Fimages.savoysystems.co.uk%%2Fb.png%s">'
                   '<img data-original-src="https://de.web.img2.acsta.net/a.jpg">'
                   '<img data-original-src="https://de.web.img3.acsta.net/b.jpg"></html>' % (WESERV, HINTS, WESERV, HINTS))
        self.discover("uk=" + LONDON, env={"PROBE_POSTER_MIN": "2"})
        self.assertEqual(["acsta.net", "savoysystems.co.uk"],
                         sorted({t["labels"]["poster_host"] for t in self.found() if t["labels"]["kind"] == "poster"}))
        os.remove(self.targets)
        self.discover("uk=" + LONDON, env={"PROBE_POSTER_MIN": "3"})
        self.assertEqual([], self.of("poster"))
        os.remove(self.targets)
        self.discover("uk=" + LONDON, env={"PROBE_POSTER_MIN": "2", "PROBE_POSTER_MAX": "1"})
        self.assertEqual(["acsta.net"], sorted({t["labels"]["poster_host"] for t in self.found()
                                                if t["labels"]["kind"] == "poster"}))

    def test_a_tool_it_runs_missing_from_path_fails_the_run_and_the_last_good_targets_stand(self) -> None:
        # 2026-10-21: the unit was given no awk, and every discovery for days wrote film targets and no poster's and
        # logged success. Here curl, the one tool every version of it runs, is the one missing.
        tools = self.path("no-curl")
        os.makedirs(tools)
        for tool in ("bash", "sh", "env", "cat", "rm", "jq", "grep", "sed", "awk", "sort", "uniq", "cut", "tr", "head",
                     "date", "mktemp", "mv", "chmod", "python3"):
            found = shutil.which(tool)
            if found:
                os.symlink(found, os.path.join(tools, tool))
        os.remove(os.path.join(self.fakes.bin, "curl"))
        self.write("targets.json", '[{"targets":["kept"]}]')
        done = self.run_tool(self.targets, "uk=" + LONDON, env={"PATH": tools, "FAKE_PAGES": self.path("pages")})
        self.assertEqual(1, done.code, done.output)
        self.assertEqual("synthetic-probe-targets: curl is not on PATH\n", done.stderr)
        self.assertEqual('[{"targets":["kept"]}]', self.read("targets.json"))

    def test_no_output_path_is_the_usage(self) -> None:
        done = self.run_tool()
        self.assertNotEqual(0, done.code, done.output)
        self.assertIn("usage: synthetic-probe-targets <output.json> <country>=<city url> ...", done.stderr)


if __name__ == "__main__":
    unittest.main()
