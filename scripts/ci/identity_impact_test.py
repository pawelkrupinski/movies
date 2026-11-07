#!/usr/bin/env python3
"""identity_impact.py: the countries a change reaches (a title rule by its row's countries and venue, a country's
fixture, a country-keyed row, title words everywhere) and the identity set's lines narrowed to the selected classes."""

import os
import re
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_impact as ii  # noqa: E402

RULES = "common/src/main/resources/title-rules.tsv"
HEADER = "# id\tkind\tcinema\tcountries\tflags\teffect\tseason\tpattern\treplacement\tnote\texamples\n"


def row(rid, countries="", cinema="", pattern="x"):
    kind = "venue" if cinema else "query"
    return f"{rid}\t{kind}\t{cinema}\t{countries}\t\tjunk\t\t{pattern}\t\t\t[]\n"


SCRATCH_GIT = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "scratch-git.sh")


class Repo:
    """A scratch repository made through scripts/scratch-git.sh -- never a raw git, which under the pre-push hook inherits
    its GIT_DIR and writes into the repository every worktree shares -- a base commit, then a head commit, of the given
    files. countries() reads it with every GIT_* variable of this process dropped, for the same reason."""

    def __init__(self, base, head):
        self.dir = tempfile.TemporaryDirectory()
        self.path = os.path.join(self.dir.name, "repo")
        self.scratch("scratch_repo", "-q")
        self.base = self.commit(base)
        self.head = self.commit(head)

    def scratch(self, helper, *args):
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.path, *args],
                              check=True, capture_output=True, text=True).stdout.strip()

    def git(self, *args):
        return self.scratch("scratch_git", *args)

    def commit(self, files):
        for name, text in files.items():
            p = os.path.join(self.path, name)
            if text is None:
                if os.path.exists(p):
                    os.remove(p)
                continue
            os.makedirs(os.path.dirname(p), exist_ok=True)
            with open(p, "w", encoding="utf-8") as fh:
                fh.write(text)
        self.git("add", "-A")
        self.git("commit", "-q", "--allow-empty", "-m", "c")
        return self.git("rev-parse", "HEAD")

    def countries(self):
        changed = self.git("diff", "--name-only", "--no-renames", self.base, self.head).split()
        cwd, saved = os.getcwd(), {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        try:
            os.chdir(self.path)
            return ii.countries(self.base, self.head, changed, self.path)
        finally:
            os.chdir(cwd)
            os.environ.update(saved)
            self.dir.cleanup()


ROSTERS = {
    "data/pl/venues.json": '[{"displayName": "Kino Muza", "cinemaObject": "KinoMuza"}]',
    "data/germany/venues.json": '[{"displayName": "Kino Muza Berlin"}, {"displayName": "Cinema City Mitte"}]',
}


class CountriesTest(unittest.TestCase):

    def test_a_row_scoped_to_a_country_reaches_it_alone(self):
        r = Repo({RULES: HEADER + row("a") + row("b", "de")}, {RULES: HEADER + row("a") + row("b", "de", pattern="y")})
        self.assertEqual(r.countries(), ["de"])

    def test_an_unscoped_row_reaches_every_country(self):
        r = Repo({RULES: HEADER + row("a")}, {RULES: HEADER + row("a", pattern="y")})
        self.assertEqual(r.countries(), ii.COUNTRIES)

    def test_a_reordered_pair_reaches_the_countries_loading_both(self):
        r = Repo({RULES: HEADER + row("a", "pl,de") + row("b", "de")}, {RULES: HEADER + row("b", "de") + row("a", "pl,de")})
        self.assertEqual(r.countries(), ["de"])

    def test_comments_reach_no_country(self):
        r = Repo({RULES: HEADER + row("a")}, {RULES: HEADER + "# a note\n" + row("a")})
        self.assertEqual(r.countries(), [])

    def test_a_venue_row_reaches_the_countries_holding_a_venue_of_its_key(self):
        r = Repo(dict(ROSTERS, **{RULES: HEADER + row("m", cinema="kino-muza")}),
                 {RULES: HEADER + row("m", cinema="kino-muza", pattern="y")})
        self.assertEqual(r.countries(), ["pl"])

    def test_a_chain_key_reaches_every_country_with_a_venue_of_the_chain(self):
        r = Repo(dict(ROSTERS, **{RULES: HEADER + row("c", cinema="cinema-city")}),
                 {RULES: HEADER + row("c", cinema="cinema-city", pattern="y")})
        self.assertEqual(r.countries(), ["de"])

    def test_a_venue_key_no_roster_holds_reaches_every_country(self):
        r = Repo(dict(ROSTERS, **{RULES: HEADER + row("u", cinema="kino-nowhere")}),
                 {RULES: HEADER + row("u", cinema="kino-nowhere", pattern="y")})
        self.assertEqual(r.countries(), ii.COUNTRIES)

    def test_a_venue_row_scoped_to_a_country_reaches_no_more_than_it(self):
        r = Repo(dict(ROSTERS, **{RULES: HEADER + row("u", "uk", cinema="kino-nowhere")}),
                 {RULES: HEADER + row("u", "uk", cinema="kino-nowhere", pattern="y")})
        self.assertEqual(r.countries(), ["uk"])

    def test_a_title_word_in_any_language_reaches_every_country(self):
        words = "common/src/main/resources/title-words.tsv"
        r = Repo({words: "# c\nanniversary\tde\tjubiläum\n"}, {words: "# c\nanniversary\tde\tjubiläum\nanniversary\tde\tfeier\n"})
        self.assertEqual(r.countries(), ii.COUNTRIES)

    def test_a_country_keyed_row_reaches_its_country(self):
        j = "test/resources/fixtures/identity-judgements.tsv"
        head = "country\tvenue\trawTitle\n"
        r = Repo({j: head + "de\tA\tB\n"}, {j: head + "de\tA\tB\nus\tC\tD\n"})
        self.assertEqual(r.countries(), ["us"])

    def test_a_country_keyed_files_header_reaches_every_country(self):
        j = "test/resources/fixtures/identity-unmatched/expected-matches.tsv"
        r = Repo({j: "country\tvenue\n"}, {j: "country\tvenue\tfilm\n"})
        self.assertEqual(r.countries(), ii.COUNTRIES)

    def test_one_countrys_fixtures_reach_it(self):
        r = Repo({}, {"test/resources/fixtures/identity-unmatched/uk.json.gz": "x",
                      "test/resources/fixtures/identity-slices/es-a-b-1/expected.tsv": "x",
                      "test/resources/fixtures/corpus/cinema-scrapes-pl-sample.json.gz": "x",
                      "scrapers/src/main/scala/services/cinemas/de/A.scala": "x",
                      "data/spain/venues.json": "[]"})
        self.assertEqual(r.countries(), ["pl", "uk", "de", "es"])

    def test_shared_code_reaches_every_country_and_docs_none(self):
        self.assertEqual(Repo({}, {"identity/src/main/scala/A.scala": "x"}).countries(), ii.COUNTRIES)
        self.assertEqual(Repo({}, {"docs/a.md": "x", ".claude/x": "y"}).countries(), [])

    def test_the_http_module_reaches_no_country_and_the_fetchers_left_in_common_every_one(self):
        self.assertEqual(Repo({}, {"http/src/main/scala/tools/RealHttpFetch.scala": "x"}).countries(), [])
        self.assertEqual(Repo({}, {"http/src/main/scala/tools/RealHttpFetch.scala": "x",
                                   "common/src/main/scala/tools/PacedHttpFetch.scala": "y"}).countries(), ii.COUNTRIES)


SET = ["identity/test", "testOnly services.identity.* scripts.*"]


class PlanTest(unittest.TestCase):

    def test_each_line_narrowed_to_the_selected_classes_each_run_once(self):
        tests = {"identity": {"services.identity.A", "services.identity.B"}, "worker": {"services.identity.C", "tools.D"}}
        self.assertEqual(ii.plan(SET, [], tests, None),
                         ["identity/testOnly services.identity.A services.identity.B", "worker/testOnly services.identity.C"])

    def test_the_countries_go_to_the_specs_as_their_config_map(self):
        self.assertEqual(ii.plan(SET, [], {"worker": {"scripts.X"}}, ["pl"]),
                         ["worker/testOnly scripts.X -- -Didentity.countries=pl"])

    def test_every_country_needs_no_scope(self):
        self.assertEqual(ii.plan(SET, [], {"worker": {"scripts.X"}}, list(ii.COUNTRIES)), ["worker/testOnly scripts.X"])

    def test_the_refit_measure_forks_apart_with_its_countries_in_scope(self):
        tests = {"worker": {"scripts.IdentityRefitMeasureAllSpec", "scripts.X"}}
        self.assertEqual(ii.plan(SET, [], tests, ["pl"]),
                         ["worker/testOnly scripts.IdentityRefitMeasureAllSpec -- -Didentity.countries=pl",
                          "worker/testOnly scripts.X -- -Didentity.countries=pl"])
        self.assertEqual(ii.plan(SET, [], tests, ["de"]),
                         ["worker/testOnly scripts.IdentityRefitMeasureAllSpec scripts.X -- -Didentity.countries=de"])

    def test_nothing_selected_runs_nothing(self):
        self.assertEqual(ii.plan(SET, [], {}, None), [])

    def test_all_runs_the_set_as_it_is_scoped(self):
        self.assertEqual(ii.plan(SET, ["the build changed"], {}, ["uk"]),
                         ["identity/test", "testOnly services.identity.* scripts.* -- -Didentity.countries=uk"])

    def test_a_line_that_runs_no_classes_is_kept(self):
        self.assertEqual(ii.plan(["Test/compile"] + SET, [], {}, None), ["Test/compile"])


def local_identity_lines():
    """The identity set's sbt lines of the checked-in scripts/hooks/local-tests.txt."""
    path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "hooks", "local-tests.txt")
    with open(path, encoding="utf-8") as fh:
        return [l.split(None, 2)[2].strip() for l in fh if re.match(r"identity\s+sbt\s", l)]


class LocalTestsTest(unittest.TestCase):

    def test_a_title_rule_change_runs_the_scraper_specs_it_reaches(self):
        # Main #2266: helios-suffix-9 dropped, and only HeliosClientFullRepertoireSpec (Helios's scraper cleans its
        # titles with its venue rows) saw "... - seanse z konkursami HDD" stop folding; the identity set ran no scraper spec
        tests = {"scrapers": {"clients.helios.HeliosClientFullRepertoireSpec"}, "worker": {"services.titlerules.X"}}
        plan = ii.plan(local_identity_lines(), [], tests, ["pl"])
        self.assertIn("scrapers/testOnly clients.helios.HeliosClientFullRepertoireSpec -- -Didentity.countries=pl", plan)


if __name__ == "__main__":
    unittest.main()
