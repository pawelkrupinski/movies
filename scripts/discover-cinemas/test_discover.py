#!/usr/bin/env python3
"""Unit tests for the cinema-discovery tool (stdlib unittest — no deps, no network).

Run: python3 scripts/discover-cinemas/test_discover.py
"""
import unittest

import discover as d


# Minimal but structurally faithful fixtures ----------------------------------
#
# Where a venue is wired is read off the CATALOG (which `<region>Scrapers` val holds
# each venue) and data/uk/venues.json (where each venue is): a new venue joins the
# val of the wired venue nearest it. Which PAGE lists it is generated later, from
# its town (data/uk/scripts/build_pages.py), so nothing here touches a page.
VENUES = [
    {"cinemaObject": "BfiLondonSouthbank", "lat": 51.5069, "lon": -0.1150},
    {"cinemaObject": "ChapterCardiff", "lat": 51.4820, "lon": -3.2010},
    {"cinemaObject": "CineworldCardiff", "lat": 51.4780, "lon": -3.1760},
    {"cinemaObject": "HomeManchester", "lat": 53.4740, "lon": -2.2460},
]
LONDON = {"BfiLondonSouthbank"}

CINEMA_SCALA = '''
case object ChapterCardiff extends Cinema("Chapter Cardiff", "Chapter")
case object CineworldCardiff extends Cinema("Cineworld Cardiff", "Cineworld Cardiff")
case object HomeManchester extends Cinema("HOME Manchester", "HOME")
'''

CATALOG_SCALA = '''  private val londonScrapers: Seq[CinemaScraper] = Seq(
    flicks("bfi-london-southbank", BfiLondonSouthbank),
  )
  private val cardiffScrapers: Seq[CinemaScraper] = Seq(
    flicks("chapter-cardiff", ChapterCardiff),
    cineworldUk("cardiff", CineworldCardiff),
  )
  private val manchesterScrapers: Seq[CinemaScraper] = Seq(
    flicks("home-manchester", HomeManchester),
  )
  private val baseByCity: Map[String, Seq[CinemaScraper]] = Map(
    "cardiff" -> cardiffScrapers,
    "manchester" -> manchesterScrapers,
  )
'''

# Chain venues are catalogued under their own-site client (above), so their Flicks
# slug — the aggregator FALLBACK — lives only here.
CHAIN_FALLBACK_SCALA = '''object ChainFlicksFallback {
  val slugs: Map[Cinema, String] = Map(
    CineworldCardiff -> "cineworld-cardiff",
  )
}
'''

SITEMAP = """<?xml version="1.0"?><urlset>
<url><loc>https://www.flicks.co.uk/cinema/bfi-london-southbank/</loc></url>
<url><loc>https://www.flicks.co.uk/cinema/chapter-cardiff/</loc></url>
<url><loc>https://www.flicks.co.uk/cinema/cineworld-cardiff/</loc></url>
<url><loc>https://www.flicks.co.uk/cinema/home-manchester/</loc></url>
<url><loc>https://www.flicks.co.uk/cinema/premiere-cinema-cardiff/</loc></url>
<url><loc>https://www.flicks.co.uk/cinema/odeon-luxe-manchester/</loc></url>
</urlset>"""


class EnumerationTests(unittest.TestCase):
    def test_sitemap_slugs(self):
        self.assertEqual(
            d.sitemap_slugs(SITEMAP),
            {"bfi-london-southbank", "chapter-cardiff", "cineworld-cardiff", "home-manchester",
             "premiere-cinema-cardiff", "odeon-luxe-manchester"},
        )

    def test_wired_slugs_spans_flicks_and_chain_fallback(self):
        # cineworld-cardiff is wired own-site-primary, so it appears only in the
        # fallback map — it is still WIRED and must never read as a new candidate.
        self.assertEqual(
            d.wired_slugs(CATALOG_SCALA, CHAIN_FALLBACK_SCALA),
            {"bfi-london-southbank", "chapter-cardiff", "cineworld-cardiff", "home-manchester"},
        )

    def test_load_exclude_ignores_comments(self):
        txt = "# header\n\nfoo-bar   # closed\n  baz-qux\n"
        self.assertEqual(d.load_exclude(txt), {"foo-bar", "baz-qux"})

    def test_diff_new_and_gone(self):
        flicks = d.sitemap_slugs(SITEMAP)
        wired = d.wired_slugs(CATALOG_SCALA, CHAIN_FALLBACK_SCALA)
        exclude = {"odeon-luxe-manchester"}  # pretend this one is defunct
        self.assertEqual(sorted(flicks - wired - exclude), ["premiere-cinema-cardiff"])
        self.assertEqual(wired - flicks - exclude, set())


class GroupTests(unittest.TestCase):
    def test_reads_the_val_each_wired_venue_is_in(self):
        vals = d.scraper_vals(CATALOG_SCALA)
        self.assertEqual(vals["ChapterCardiff"], "cardiffScrapers")
        self.assertEqual(vals["CineworldCardiff"], "cardiffScrapers")   # own-site clients count too
        self.assertEqual(vals["HomeManchester"], "manchesterScrapers")

    def test_nearest_wired_venue_decides_the_val(self):
        vals = d.scraper_vals(CATALOG_SCALA)
        # a point in central Cardiff resolves to a Cardiff venue, not Manchester/London
        group = d.nearest_group(51.48, -3.18, VENUES, vals, LONDON)
        self.assertEqual(group.val, "cardiffScrapers")
        self.assertEqual(group.near, "CineworldCardiff")
        self.assertLess(group.km, 5)
        self.assertFalse(group.london)

    def test_london_venues_come_from_the_generated_pages(self):
        pages = '{"pages": [{"slug": "london", "cinemas": ["A", "B"]}, {"slug": "leeds", "cinemas": ["C"]}]}'
        self.assertEqual(d.london_venues(pages), {"A", "B"})


class PascalTests(unittest.TestCase):
    def test_pascal(self):
        self.assertEqual(d.pascal("Premiere Cinema Cardiff"), "PremiereCinemaCardiff")
        self.assertEqual(d.pascal("Vue Cinemas Croydon Grant's"), "VueCinemasCroydonGrantS")
        self.assertEqual(d.pascal("123 Cinema"), "N123Cinema")


class PlanTests(unittest.TestCase):
    def _meta(self, **kw):
        m = d.VenueMeta(kw.pop("slug"))
        for k, v in kw.items():
            setattr(m, k, v)
        return m

    def test_wire_good_candidate(self):
        metas = [self._meta(slug="premiere-cinema-cardiff", display="Premiere Cinema Cardiff",
                            lat=51.48, lon=-3.18, has_sessions=True)]
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        self.assertEqual(len(plan.wire), 1)
        w = plan.wire[0]
        self.assertEqual(w.obj, "PremiereCinemaCardiff")
        self.assertEqual(w.group.val, "cardiffScrapers")

    def test_park_london(self):
        metas = [self._meta(slug="x", display="New Picturehouse Soho", lat=51.51, lon=-0.13, has_sessions=True)]
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        self.assertEqual(plan.wire, [])
        self.assertIn("London", plan.park[0].reason)

    def test_park_no_sessions(self):
        metas = [self._meta(slug="x", display="Dark Hall Cardiff", lat=51.48, lon=-3.18, has_sessions=False)]
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        self.assertIn("no sessions", plan.park[0].reason)

    def test_park_no_coords(self):
        metas = [self._meta(slug="x", display="Somewhere", lat=None, has_sessions=True)]
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        self.assertIn("coordinates", plan.park[0].reason)

    def test_park_name_collision(self):
        metas = [self._meta(slug="chapter-cardiff-2", display="Chapter Cardiff",
                            lat=51.48, lon=-3.18, has_sessions=True)]
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        self.assertIn("collision", plan.park[0].reason)


class SpliceTests(unittest.TestCase):
    def test_apply_plan_edits_both_files(self):
        metas = [d.VenueMeta("premiere-cinema-cardiff", "Premiere Cinema Cardiff", 51.48, -3.18, True)]
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        new_cin, new_cat = d.apply_plan(CINEMA_SCALA, CATALOG_SCALA, plan)
        # case object added
        self.assertIn('case object PremiereCinemaCardiff extends Cinema("Premiere Cinema Cardiff"', new_cin)
        # catalog flicks line added inside cardiffScrapers, on ITS OWN line
        self.assertIn(
            '\n    flicks("premiere-cinema-cardiff", PremiereCinemaCardiff),\n', new_cat
        )
        # exactly one flicks(...) per line — the new entry didn't graft onto another
        for line in new_cat.splitlines():
            self.assertLessEqual(line.count("flicks("), 1, f"two flicks on one line: {line!r}")
        # it lands before the cardiffScrapers closing paren, above manchesterScrapers
        self.assertLess(new_cat.index("premiere-cinema-cardiff"), new_cat.index("manchesterScrapers"))

    def test_no_candidates_is_noop(self):
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, VENUES, LONDON, metas=[],
                            gone_dead=["gone-slug"], gone_live=[])
        new_cin, new_cat = d.apply_plan(CINEMA_SCALA, CATALOG_SCALA, plan)
        self.assertEqual(new_cin, CINEMA_SCALA)
        self.assertEqual(new_cat, CATALOG_SCALA)
        self.assertIn("gone-slug", d.render_report(plan, applied=False))


class GoneLivenessTests(unittest.TestCase):
    """A slug gone from the sitemap is a retirement candidate ONLY if it also has
    no sessions; a still-serving one is a benign index gap, never flagged."""

    def test_partition_splits_dead_from_live(self):
        gone = {"vue-cinemas-colchester", "palace-cinemas-isle-of-man", "really-closed-odeon"}
        live_set = {"vue-cinemas-colchester", "palace-cinemas-isle-of-man"}
        dead, live = d.partition_gone(gone, probe=lambda s: s in live_set)
        self.assertEqual(dead, ["really-closed-odeon"])
        self.assertEqual(live, ["palace-cinemas-isle-of-man", "vue-cinemas-colchester"])

    def test_live_index_gap_is_not_a_retirement_candidate(self):
        dead, live = d.partition_gone({"vue-cinemas-colchester"}, probe=lambda s: True)
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, VENUES, LONDON, metas=[],
                            gone_dead=dead, gone_live=live)
        report = d.render_report(plan, applied=False)
        # reported as an index gap, NOT under retirement candidates
        self.assertIn("index gap", report.lower())
        self.assertEqual(plan.gone_dead, [])
        retire_section = report.split("Retirement candidates")[1].split("###")[0]
        self.assertNotIn("vue-cinemas-colchester", retire_section)


class RealRepoInvariantTests(unittest.TestCase):
    """Guards against a known-defunct venue creeping back into the live wiring.
    Reads the real repo files, so it fails if any of the 8 closed/duplicate UK
    venues is re-wired or dropped from the exclude list."""

    DEFUNCT = {
        "odeon-cinema-camden-town", "vue-cinemas-cardiff", "vue-cinemas-grants-croydon",
        "cineworld-friars-walk-newport", "curzon-canterbury", "curzon-cinema-sheffield",
        "omniplex-armagh", "vue-cinemas-shepherd-s-bush",
    }

    def _wired(self):
        return d.wired_slugs(d.read_text(d.CATALOG_SCALA), d.read_text(d.CHAIN_FALLBACK_SCALA))

    def test_defunct_are_excluded_and_unwired(self):
        exclude = d.load_exclude(d.read_text(d.EXCLUDE_TXT))
        wired = self._wired()
        for slug in self.DEFUNCT:
            self.assertIn(slug, exclude, f"{slug} missing from exclude.txt")
            self.assertNotIn(slug, wired, f"{slug} is still wired in the catalog")

    def test_surviving_siblings_still_wired(self):
        # the real venues we kept must NOT have been swept up by the deletion —
        # the three chain venues among them are wired own-site-primary, so they
        # count as wired only via the Flicks-fallback map.
        wired = self._wired()
        for slug in ("vue-cinemas-westfield-shepherd-s-bush", "curzon-canterbury-riverside",
                     "vue-cinemas-purley-way-croydon", "everyman-cinema-durham"):
            self.assertIn(slug, wired, f"{slug} was removed by mistake")


if __name__ == "__main__":
    unittest.main(verbosity=2)
