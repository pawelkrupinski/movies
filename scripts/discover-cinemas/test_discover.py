#!/usr/bin/env python3
"""Unit tests for the cinema-discovery tool (stdlib unittest — no deps, no network).

Run: python3 scripts/discover-cinemas/test_discover.py
"""
import unittest

import discover as d


# Minimal but structurally faithful fixtures ----------------------------------
CITY_SCALA = '''
case object London extends City("london",
  CityLabels("London", "London", "London"), 51.5074, -0.1278, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.london
  override val areas: Seq[CinemaAreaGroup] = Cinema.londonAreas
}
case object Cardiff extends City("cardiff",
  CityLabels("Cardiff", "Cardiff", "Cardiff"), 51.4816, -3.1791, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.cardiff
}
case object Manchester extends City("manchester",
  CityLabels("Manchester", "Manchester", "Manchester"), 53.4808, -2.2426, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.manchester
}
case object Poznan extends City("poznan",
  CityLabels("Poznań", "Poznaniu", "Poznania"), 52.4064, 16.9252, ZoneId.of("Europe/Warsaw")) {
  val cinemas: Seq[Cinema] = Cinema.poznan
}
  private[models] val allUkCities: Seq[City] = Seq(
    London, Cardiff, Manchester,
  )
'''

CINEMA_SCALA = '''
case object ChapterCardiff extends Cinema("Chapter Cardiff", "Chapter")
case object CineworldCardiff extends Cinema("Cineworld Cardiff", "Cineworld Cardiff")
case object HomeManchester extends Cinema("HOME Manchester", "HOME")

  val london: Seq[Cinema] = Seq(BfiLondonSouthbank, PrinceCharlesLondon)
  val cardiff: Seq[Cinema] = Seq(ChapterCardiff, CineworldCardiff)
  val manchester: Seq[Cinema] = Seq(HomeManchester)
'''

CATALOG_SCALA = '''  private val cardiffScrapers: Seq[CinemaScraper] = Seq(
    flicks("chapter-cardiff", ChapterCardiff),
    flicks("cineworld-cardiff", CineworldCardiff),
  )
  private val manchesterScrapers: Seq[CinemaScraper] = Seq(
    flicks("home-manchester", HomeManchester),
  )
  private val baseByCity: Map[String, Seq[CinemaScraper]] = Map(
    "cardiff" -> cardiffScrapers,
    "manchester" -> manchesterScrapers,
  )
'''

SITEMAP = """<?xml version="1.0"?><urlset>
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
            {"chapter-cardiff", "cineworld-cardiff", "home-manchester",
             "premiere-cinema-cardiff", "odeon-luxe-manchester"},
        )

    def test_wired_slugs(self):
        self.assertEqual(
            d.wired_slugs(CATALOG_SCALA),
            {"chapter-cardiff", "cineworld-cardiff", "home-manchester"},
        )

    def test_load_exclude_ignores_comments(self):
        txt = "# header\n\nfoo-bar   # closed\n  baz-qux\n"
        self.assertEqual(d.load_exclude(txt), {"foo-bar", "baz-qux"})

    def test_diff_new_and_gone(self):
        flicks = d.sitemap_slugs(SITEMAP)
        wired = d.wired_slugs(CATALOG_SCALA)
        exclude = {"odeon-luxe-manchester"}  # pretend this one is defunct
        self.assertEqual(sorted(flicks - wired - exclude), ["premiere-cinema-cardiff"])
        self.assertEqual(wired - flicks - exclude, set())


class CityTests(unittest.TestCase):
    def test_parses_only_uk_cities_with_coords_and_group(self):
        cities = d.parse_uk_cities(CITY_SCALA)
        self.assertEqual({c.obj for c in cities}, {"London", "Cardiff", "Manchester"})
        self.assertNotIn("Poznan", {c.obj for c in cities})  # PL excluded
        cardiff = next(c for c in cities if c.obj == "Cardiff")
        self.assertEqual(cardiff.slug, "cardiff")
        self.assertEqual(cardiff.group_val, "cardiff")
        self.assertAlmostEqual(cardiff.lat, 51.4816)

    def test_nearest_city_by_distance(self):
        cities = d.parse_uk_cities(CITY_SCALA)
        # a point in central Cardiff resolves to Cardiff, not Manchester/London
        city, dist = d.nearest_city(51.48, -3.18, cities)
        self.assertEqual(city.obj, "Cardiff")
        self.assertLess(dist, 5)


class PascalTests(unittest.TestCase):
    def test_pascal(self):
        self.assertEqual(d.pascal("Premiere Cinema Cardiff"), "PremiereCinemaCardiff")
        self.assertEqual(d.pascal("Vue Cinemas Croydon Grant's"), "VueCinemasCroydonGrantS")
        self.assertEqual(d.pascal("123 Cinema"), "N123Cinema")


class PlanTests(unittest.TestCase):
    def setUp(self):
        self.cities = d.parse_uk_cities(CITY_SCALA)

    def _meta(self, **kw):
        m = d.VenueMeta(kw.pop("slug"))
        for k, v in kw.items():
            setattr(m, k, v)
        return m

    def test_wire_good_candidate(self):
        metas = [self._meta(slug="premiere-cinema-cardiff", display="Premiere Cinema Cardiff",
                            lat=51.48, lon=-3.18, has_sessions=True)]
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, self.cities, metas, gone_dead=[], gone_live=[])
        self.assertEqual(len(plan.wire), 1)
        w = plan.wire[0]
        self.assertEqual(w.obj, "PremiereCinemaCardiff")
        self.assertEqual(w.city.obj, "Cardiff")

    def test_park_london(self):
        metas = [self._meta(slug="x", display="New Picturehouse Soho", lat=51.51, lon=-0.13, has_sessions=True)]
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, self.cities, metas, gone_dead=[], gone_live=[])
        self.assertEqual(plan.wire, [])
        self.assertIn("London", plan.park[0].reason)

    def test_park_no_sessions(self):
        metas = [self._meta(slug="x", display="Dark Hall Cardiff", lat=51.48, lon=-3.18, has_sessions=False)]
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, self.cities, metas, gone_dead=[], gone_live=[])
        self.assertIn("no sessions", plan.park[0].reason)

    def test_park_no_coords(self):
        metas = [self._meta(slug="x", display="Somewhere", lat=None, has_sessions=True)]
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, self.cities, metas, gone_dead=[], gone_live=[])
        self.assertIn("coordinates", plan.park[0].reason)

    def test_park_name_collision(self):
        metas = [self._meta(slug="chapter-cardiff-2", display="Chapter Cardiff",
                            lat=51.48, lon=-3.18, has_sessions=True)]
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, self.cities, metas, gone_dead=[], gone_live=[])
        self.assertIn("collision", plan.park[0].reason)


class SpliceTests(unittest.TestCase):
    def test_apply_plan_edits_all_three_sites(self):
        cities = d.parse_uk_cities(CITY_SCALA)
        metas = [d.VenueMeta("premiere-cinema-cardiff", "Premiere Cinema Cardiff", 51.48, -3.18, True)]
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, cities, metas, gone_dead=[], gone_live=[])
        new_cin, new_cat = d.apply_plan(CINEMA_SCALA, CATALOG_SCALA, plan)
        # case object added
        self.assertIn('case object PremiereCinemaCardiff extends Cinema("Premiere Cinema Cardiff"', new_cin)
        # added to the cardiff grouping val, not london/manchester
        cardiff_line = next(l for l in new_cin.splitlines() if l.strip().startswith("val cardiff:"))
        self.assertIn("PremiereCinemaCardiff", cardiff_line)
        self.assertNotIn("PremiereCinemaCardiff", next(l for l in new_cin.splitlines() if "val manchester:" in l))
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
        cities = d.parse_uk_cities(CITY_SCALA)
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, cities, metas=[],
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
        cities = d.parse_uk_cities(CITY_SCALA)
        dead, live = d.partition_gone({"vue-cinemas-colchester"}, probe=lambda s: True)
        plan = d.build_plan(CINEMA_SCALA, CATALOG_SCALA, cities, metas=[],
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

    def test_defunct_are_excluded_and_unwired(self):
        exclude = d.load_exclude(open(d.EXCLUDE_TXT).read())
        wired = d.wired_slugs(open(d.CATALOG_SCALA).read())
        for slug in self.DEFUNCT:
            self.assertIn(slug, exclude, f"{slug} missing from exclude.txt")
            self.assertNotIn(slug, wired, f"{slug} is still wired in the catalog")

    def test_surviving_siblings_still_wired(self):
        # the real venues we kept must NOT have been swept up by the deletion
        wired = d.wired_slugs(open(d.CATALOG_SCALA).read())
        for slug in ("vue-cinemas-westfield-shepherd-s-bush", "curzon-canterbury-riverside",
                     "vue-cinemas-purley-way-croydon", "everyman-cinema-durham"):
            self.assertIn(slug, wired, f"{slug} was removed by mistake")


if __name__ == "__main__":
    unittest.main(verbosity=2)
