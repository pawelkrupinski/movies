#!/usr/bin/env python3
"""corpus_shape_diff.py: a finding the head made fails; one standing at base and head is reported; a cure is counted."""

import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import corpus_shape_diff as d  # noqa: E402

# Recording 38625654132 (2026-10-14) turned UK and DE red on listings it brought itself, no commit touching them.
ARRIETTY = ("finding\tCineworld Barnsley\tArrietty (4K Restoration) Experience It In IMAX\t"
            "searches 'Arrietty (4K Restoration) Experience It' (4k → FormatTags)\n")
PREVIEW = ("finding\tFilmRaum\tLa bola negra , Preview am Mi. 18.11. um 20 Uhr\tsearches 'La bola negra , Preview am Mi. "
           "18.11. um 20 Uhr' (preview → the country's title rules (TitleRules / ExtraTitleRules))\n")


class CorpusShapeDiffTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()

    def side(self, name, files):
        path = os.path.join(self.dir, name)
        os.makedirs(path, exist_ok=True)
        for file, lines in files.items():
            with open(os.path.join(path, file), "w", encoding="utf-8") as f:
                f.write("".join(lines))
        return path

    def run_diff(self, cc, base, head):
        summary = os.path.join(self.dir, f"summary-{cc}.md")
        code = d.main([cc, base, head, "--summary", summary])
        with open(summary, encoding="utf-8") if os.path.exists(summary) else open(os.devnull) as f:
            return code, f.read()

    def test_findings_the_recording_brought_stand_at_base_and_head_and_pass_reported(self):
        files = {"corpus-shape-recorded-uk-SearchQueryMarkers.txt": [ARRIETTY],
                 "corpus-shape-recorded-uk-CinemaSlotInvariants.txt": [],
                 "corpus-shape-recorded-de-SearchQueryMarkers.txt": [PREVIEW]}
        base, head = self.side("base", files), self.side("head", files)
        code, md = self.run_diff("uk", base, head)
        self.assertEqual(0, code)
        self.assertIn("0 new (the code's: fail), 1 standing (reported, never failed), 0 cured", md)
        self.assertIn("- SearchQueryMarkers finding: Cineworld Barnsley \\| Arrietty (4K Restoration) Experience It In IMAX", md)
        self.assertEqual(0, self.run_diff("de", base, head)[0])

    def test_a_finding_the_head_made_fails_and_a_cure_is_counted(self):
        # a rule change that leaves "Preview" in a query it used to strip, and cures Arrietty
        base = self.side("base", {"corpus-shape-recorded-de-SearchQueryMarkers.txt": [ARRIETTY]})
        head = self.side("head", {"corpus-shape-recorded-de-SearchQueryMarkers.txt": [PREVIEW]})
        code, md = self.run_diff("de", base, head)
        self.assertEqual(1, code)
        self.assertIn("1 new (the code's: fail), 0 standing (reported, never failed), 1 cured", md)
        self.assertIn("| ❌ | SearchQueryMarkers | finding | FilmRaum | La bola negra , Preview am Mi. 18.11. um 20 Uhr |", md)

    def test_a_stale_allowlist_entry_the_head_made_fails_and_a_finding_whose_query_moved_stands(self):
        stale = "stale\t*\tSneak Preview\tallowlisted but no longer breaking the rule\n"
        base = self.side("base", {"corpus-shape-recorded-de-SearchQueryMarkers.txt": [PREVIEW]})
        head = self.side("head", {"corpus-shape-recorded-de-SearchQueryMarkers.txt": [PREVIEW.replace("searches 'La", "searches 'Lo"), stale]})
        code, md = self.run_diff("de", base, head)
        self.assertEqual(1, code)
        self.assertIn("1 new (the code's: fail), 1 standing", md)
        self.assertIn("| ❌ | SearchQueryMarkers | stale | * | Sneak Preview |", md)

    def test_a_second_finding_the_head_made_for_a_listing_already_found_fails(self):
        # a spec may find a listing more than once: the head's extra finding is new, not hidden behind the first
        second = ARRIETTY.replace("(4k → FormatTags)", "(imax → FormatTags)")
        base = self.side("base", {"corpus-shape-recorded-uk-SearchQueryMarkers.txt": [ARRIETTY]})
        head = self.side("head", {"corpus-shape-recorded-uk-SearchQueryMarkers.txt": [ARRIETTY, second]})
        code, md = self.run_diff("uk", base, head)
        self.assertEqual(1, code)
        self.assertIn("1 new (the code's: fail), 1 standing (reported, never failed), 0 cured", md)
        self.assertIn("imax → FormatTags", md.split("<details>")[0])
        self.assertEqual(0, self.run_diff("uk", head, base)[0])  # and one fewer is a cure

    def test_a_side_whose_specs_wrote_nothing_is_no_verdict(self):
        base = self.side("base", {"corpus-shape-recorded-uk-SearchQueryMarkers.txt": [ARRIETTY]})
        self.assertEqual(2, self.run_diff("uk", base, self.side("head", {}))[0])
        self.assertEqual(2, self.run_diff("uk", self.side("empty", {}), base)[0])

    def test_a_spec_the_base_ran_and_the_head_did_not_is_no_verdict_never_its_findings_cured(self):
        # every spec writes its file, findings or none: one missing at the head is a spec that did not run there
        base = self.side("base", {"corpus-shape-recorded-uk-SearchQueryMarkers.txt": [],
                                  "corpus-shape-recorded-uk-CinemaSlotInvariants.txt": [ARRIETTY]})
        head = self.side("head", {"corpus-shape-recorded-uk-SearchQueryMarkers.txt": []})
        self.assertEqual(2, self.run_diff("uk", base, head)[0])
        # a spec the head brings, the base predating it, judges its findings as the head's
        self.assertEqual(1, self.run_diff("uk", head, base)[0])


if __name__ == "__main__":
    unittest.main()
