#!/usr/bin/env python3
"""Unit tests for the cinema-discovery tool (stdlib unittest — no deps, no network).

Run: python3 scripts/discover-cinemas/discover_test.py
"""
import unittest

import discover as d


# Minimal but structurally faithful fixtures ----------------------------------
#
# Where a venue is wired is read off data/uk/roster.json (which group holds each
# venue) and data/uk/venues.json (where each venue is): a new venue joins the group
# of the wired venue nearest it. Which PAGE lists it is generated later, from its
# town (data/uk/scripts/build_pages.py), so nothing here touches a page.
VENUES = [
    {"cinemaObject": "BfiLondonSouthbank", "lat": 51.5069, "lon": -0.1150},
    {"cinemaObject": "ChapterCardiff", "lat": 51.4820, "lon": -3.2010},
    {"cinemaObject": "CineworldCardiff", "lat": 51.4780, "lon": -3.1760},
    {"cinemaObject": "HomeManchester", "lat": 53.4740, "lon": -2.2460},
]
LONDON = {"BfiLondonSouthbank"}


def row(obj, display, slug, primary=None):
    v = {"object": obj, "displayName": display, "pillName": display, "flicks": slug}
    return dict(v, primary=primary) if primary else v


# A chain venue is scraped off its own site (`primary`), and keeps its Flicks slug
# — the aggregator FALLBACK — in `flicks` all the same.
ROSTER = [
    {"group": "london", "venues": [row("BfiLondonSouthbank", "BFI London Southbank", "bfi-london-southbank",
                                       ["bfiSouthbank"])]},
    {"group": "cardiff", "venues": [row("ChapterCardiff", "Chapter Cardiff", "chapter-cardiff"),
                                    row("CineworldCardiff", "Cineworld Cardiff", "cineworld-cardiff",
                                        ["cineworld", "x078m-cineworld-cinema-cardiff"])]},
    {"group": "manchester", "venues": [row("HomeManchester", "HOME Manchester", "home-manchester")]},
]

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
        # cineworld-cardiff is wired own-site-primary, Flicks only its fallback — it
        # is still WIRED and must never read as a new candidate.
        self.assertEqual(
            d.wired_slugs(ROSTER),
            {"bfi-london-southbank", "chapter-cardiff", "cineworld-cardiff", "home-manchester"},
        )

    def test_load_exclude_ignores_comments(self):
        txt = "# header\n\nfoo-bar   # closed\n  baz-qux\n"
        self.assertEqual(d.load_exclude(txt), {"foo-bar", "baz-qux"})

    def test_diff_new_and_gone(self):
        flicks = d.sitemap_slugs(SITEMAP)
        wired = d.wired_slugs(ROSTER)
        exclude = {"odeon-luxe-manchester"}  # pretend this one is defunct
        self.assertEqual(sorted(flicks - wired - exclude), ["premiere-cinema-cardiff"])
        self.assertEqual(wired - flicks - exclude, set())


class GroupTests(unittest.TestCase):
    def test_reads_the_group_each_wired_venue_is_in(self):
        vals = d.scraper_vals(ROSTER)
        self.assertEqual(vals["ChapterCardiff"], "cardiff")
        self.assertEqual(vals["CineworldCardiff"], "cardiff")   # own-site clients count too
        self.assertEqual(vals["HomeManchester"], "manchester")

    def test_nearest_wired_venue_decides_the_group(self):
        vals = d.scraper_vals(ROSTER)
        # a point in central Cardiff resolves to a Cardiff venue, not Manchester/London
        group = d.nearest_group(51.48, -3.18, VENUES, vals, LONDON)
        self.assertEqual(group.section, "cardiff")
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
        plan = d.build_plan(ROSTER, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        self.assertEqual(len(plan.wire), 1)
        w = plan.wire[0]
        self.assertEqual(w.obj, "PremiereCinemaCardiff")
        self.assertEqual(w.group.section, "cardiff")

    def test_park_london(self):
        metas = [self._meta(slug="x", display="New Picturehouse Soho", lat=51.51, lon=-0.13, has_sessions=True)]
        plan = d.build_plan(ROSTER, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        self.assertEqual(plan.wire, [])
        self.assertIn("London", plan.park[0].reason)

    def test_park_no_sessions(self):
        metas = [self._meta(slug="x", display="Dark Hall Cardiff", lat=51.48, lon=-3.18, has_sessions=False)]
        plan = d.build_plan(ROSTER, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        self.assertIn("no sessions", plan.park[0].reason)

    def test_park_no_coords(self):
        metas = [self._meta(slug="x", display="Somewhere", lat=None, has_sessions=True)]
        plan = d.build_plan(ROSTER, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        self.assertIn("coordinates", plan.park[0].reason)

    def test_park_name_collision(self):
        metas = [self._meta(slug="chapter-cardiff-2", display="Chapter Cardiff",
                            lat=51.48, lon=-3.18, has_sessions=True)]
        plan = d.build_plan(ROSTER, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        self.assertIn("collision", plan.park[0].reason)


class ApplyTests(unittest.TestCase):
    def _groups(self, roster):
        return {g["group"]: [v["object"] for v in g["venues"]] for g in roster}

    def test_display_name_collision_is_parked(self):
        # A new venue under a name another holds would read that venue's stored slots.
        metas = [d.VenueMeta("home-manchester-2", "HOME Manchester", 53.47, -2.24, True)]   # HOMEManchester
        plan = d.build_plan(ROSTER, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        self.assertEqual(plan.wire, [])
        self.assertIn("display-name collision", plan.park[0].reason)

    def test_apply_plan_appends_a_flicks_row_to_the_nearest_group(self):
        metas = [d.VenueMeta("premiere-cinema-cardiff", "Premiere Cinema Cardiff", 51.48, -3.18, True)]
        plan = d.build_plan(ROSTER, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        new = d.apply_plan(ROSTER, plan)
        self.assertEqual(self._groups(new)["cardiff"], ["ChapterCardiff", "CineworldCardiff", "PremiereCinemaCardiff"])
        self.assertEqual(new[1]["venues"][-1], {"object": "PremiereCinemaCardiff", "displayName": "Premiere Cinema Cardiff",
                                                "pillName": "Premiere Cinema Cardiff", "flicks": "premiere-cinema-cardiff"})
        self.assertEqual(d.uk_roster.problems(new), [])     # a row the generator takes
        self.assertEqual(len(ROSTER[1]["venues"]), 2)       # and the input is left alone

    def test_apply_plan_appends_to_the_last_group(self):
        metas = [d.VenueMeta("new-manchester", "New Manchester", 53.47, -2.24, True)]
        plan = d.build_plan(ROSTER, VENUES, LONDON, metas, gone_dead=[], gone_live=[])
        self.assertEqual(self._groups(d.apply_plan(ROSTER, plan))["manchester"], ["HomeManchester", "NewManchester"])

    def test_no_candidates_is_noop(self):
        plan = d.build_plan(ROSTER, VENUES, LONDON, metas=[], gone_dead=["gone-slug"], gone_live=[])
        self.assertEqual(d.apply_plan(ROSTER, plan), ROSTER)
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
        plan = d.build_plan(ROSTER, VENUES, LONDON, metas=[],
                            gone_dead=dead, gone_live=live)
        report = d.render_report(plan, applied=False)
        # reported as an index gap, NOT under retirement candidates
        self.assertIn("index gap", report.lower())
        self.assertEqual(plan.gone_dead, [])
        retire_section = report.split("Retirement candidates")[1].split("###")[0]
        self.assertNotIn("vue-cinemas-colchester", retire_section)


class RealRepoInvariantTests(unittest.TestCase):
    """Guards against a known-defunct venue creeping back into the live wiring.
    Reads the real roster, so it fails if any of the 8 closed/duplicate UK
    venues is re-wired or dropped from the exclude list."""

    DEFUNCT = {
        "odeon-cinema-camden-town", "vue-cinemas-cardiff", "vue-cinemas-grants-croydon",
        "cineworld-friars-walk-newport", "curzon-canterbury", "curzon-cinema-sheffield",
        "omniplex-armagh", "vue-cinemas-shepherd-s-bush",
    }

    def _wired(self):
        return d.wired_slugs(d.uk_roster.load())

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
