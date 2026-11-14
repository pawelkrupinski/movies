#!/usr/bin/env python3
"""Unit tests for unmatched_census.py over a synthetic country dump: no mirror, no mongosh.

    python3 scripts/unmatched_census_test.py
"""
import io
import json
import os
import re
import sys
import tempfile
import unittest
from contextlib import redirect_stdout

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import unmatched_census  # noqa: E402


def listing(venue, title, film, tmdb=None, basis='NoCandidate', showtimes=1, trace=True):
    return {'listingKey': 'N\0%s\0https://x/%s\0%s' % (venue, film, title), 'filmId': film, 'tmdbId': tmdb,
            'searchTitle': title, 'showtimes': showtimes,
            'trace': {'rawTitle': title, 'year': None, 'basis': basis, 'candidates': ["1 3.0% rank 'X' (2001)"],
                      'refusals': [], 'vetoedBy': None} if trace and not tmdb else None}


DUMP = {'screenings': 9, 'listings': [
    listing('A', 'Matched Film', 'f-matched', tmdb=11),
    listing('A', 'NT Live: Hamlet', 'f-hamlet', basis='BelowThreshold'),
    listing('B', 'NT Live: Hamlet', 'f-hamlet', basis='BelowThreshold'),
    listing('A', 'Close', 'f-close', basis='Vetoed'),
    listing('B', 'Close', 'f-close', basis='Vetoed'),
    listing('C', 'Close', 'f-close', basis='NoCandidate'),
    listing('A', 'Heated Rivalry', 'f-heated'),
    listing('A', 'Untraced Thing', 'f-untraced', trace=False),
    {'listingKey': 'P\0D\0Pooled Title\x002024\0Someone', 'filmId': 'f-pooled', 'tmdbId': None, 'searchTitle': 'x',
     'showtimes': 2, 'trace': None},
]}
PATTERNS = [re.compile(r'\bnt live\b', re.I)]


def verdict(film, title, kind, cause=''):
    return {'country': 'DE', 'filmId': film, 'title': title, 'verdict': kind, 'cause': cause, 'tmdbId': '', 'note': ''}


class CensusTest(unittest.TestCase):
    def test_pooled_listing_key_carries_its_title_in_the_third_part(self):
        self.assertEqual(unmatched_census.listing_title('P\0venue\0The Title\x002024\0dir'), 'The Title')
        self.assertEqual(unmatched_census.listing_title('N\0venue\0https://u\0Other'), 'Other')

    def test_counts_unmatched_listings_and_clusters_by_basis(self):
        result = unmatched_census.census('DE', DUMP, PATTERNS, [], [])
        self.assertEqual((result['listings'], result['clusters'], result['all_listings']), (8, 5, 9))
        self.assertEqual(result['listings_by_basis'], {'BelowThreshold': 2, 'Vetoed': 2, 'NoCandidate': 2, 'NoTrace': 2})
        self.assertEqual(result['clusters_by_basis']['Vetoed'], 1, 'a cluster takes its commonest basis')

    def test_events_are_set_aside_and_counted_under_a_miss_basis(self):
        result = unmatched_census.census('DE', DUMP, PATTERNS, [], [])
        self.assertEqual(result['events'], {'listings': 2, 'clusters': 1, 'under_miss_listings': 2, 'under_miss_clusters': 1})
        self.assertEqual(result['other_clusters'], 4)
        self.assertEqual({cluster['filmId'] for cluster in result['unjudged']}, {'f-close', 'f-heated', 'f-untraced', 'f-pooled'})

    def test_exhaustive_verdicts_count_exact_listings(self):
        verdicts = [verdict('f-close', 'Close', 'real', 'learned-veto'), verdict('f-heated', '', 'not-film'),
                    verdict('f-untraced', '', 'no-candidate'), verdict('f-pooled', '', 'unclear')]
        result = unmatched_census.census('DE', DUMP, PATTERNS, verdicts, [])
        self.assertTrue(result['exhaustive'])
        self.assertEqual(result['buckets']['real/learned-veto']['listings'], 3)
        self.assertEqual(result['buckets']['real/learned-veto']['examples'], ['Close'])

    def test_a_sample_extrapolates_by_its_share_of_clusters(self):
        verdicts = [verdict('f-close', 'Close', 'real', 'learned-veto'), verdict('f-heated', '', 'not-film')]
        result = unmatched_census.census('DE', DUMP, PATTERNS, verdicts, [])
        self.assertFalse(result['exhaustive'])
        self.assertEqual(result['other_listings'], 6)
        self.assertEqual(result['buckets']['real/learned-veto']['listings'], 3, 'half the judged clusters of 6 listings')

    def test_a_verdict_follows_its_title_when_the_film_id_changes(self):
        result = unmatched_census.census('DE', DUMP, PATTERNS, [verdict('f-old-id', 'close', 'real', 'banner')], [])
        self.assertIn('real/banner', result['buckets'])

    def test_a_verdict_of_another_country_does_not_apply(self):
        other = dict(verdict('f-close', 'Close', 'real', 'banner'), country='UK')
        self.assertEqual(unmatched_census.census('DE', DUMP, PATTERNS, [other], [])['buckets'], {})

    def test_watch_list_says_matched_unmatched_or_not_listed(self):
        watch = [('DE', 'matched film'), ('DE', 'Close'), ('DE', 'Gone Film'), ('UK', 'Close')]
        entries = unmatched_census.census('DE', DUMP, PATTERNS, [], watch)['watch']
        self.assertEqual([entry['title'] for entry in entries], ['matched film', 'Close', 'Gone Film'])
        self.assertEqual(entries[0]['now'], [{'listed': 'Matched Film', 'state': 'tmdb 11'}])
        self.assertEqual({hit['state'] for hit in entries[1]['now']}, {'UNMATCHED Vetoed', 'UNMATCHED NoCandidate'})
        self.assertEqual(entries[2]['now'], 'not listed')

    def test_weighted_sample_is_reproducible_and_bounded(self):
        unjudged = unmatched_census.census('DE', DUMP, PATTERNS, [], [])['unjudged']
        first = [cluster['filmId'] for cluster in unmatched_census.weighted_sample(unjudged, 2, seed=7)]
        self.assertEqual(first, [cluster['filmId'] for cluster in unmatched_census.weighted_sample(unjudged, 2, seed=7)])
        self.assertEqual(len(first), 2)


class CommandLineTest(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.mkdtemp()
        with open(os.path.join(self.directory, 'DE.json'), 'w', encoding='utf-8') as handle:
            json.dump(DUMP, handle)
        self.patterns = os.path.join(self.directory, 'patterns.txt')
        with open(self.patterns, 'w', encoding='utf-8') as handle:
            handle.write('# comment\n\\bnt live\\b\n')
        self.verdicts = os.path.join(self.directory, 'verdicts.tsv')

    def write_verdicts(self, text):
        with open(self.verdicts, 'w', encoding='utf-8') as handle:
            handle.write('\t'.join(unmatched_census.VERDICT_COLUMNS) + '\n' + text)

    def run_census(self, *extra):
        output = io.StringIO()
        with redirect_stdout(output):
            unmatched_census.main(['--countries', 'DE', '--from-dump', self.directory, '--event-patterns', self.patterns,
                                   '--verdicts', self.verdicts, '--watch', os.path.join(self.directory, 'none.tsv')] + list(extra))
        return output.getvalue()

    def test_text_and_json_reports_from_a_dump(self):
        self.write_verdicts('DE\tf-close\tClose\treal\tlearned-veto\t901563\t\n')
        self.assertIn('real/learned-veto', self.run_census())
        report = json.loads(self.run_census('--json'))
        self.assertEqual(report['countries']['DE']['judged_clusters'], 1)
        self.assertNotIn('unjudged', report['countries']['DE'])

    def test_unjudged_clusters_are_written_biggest_first(self):
        self.write_verdicts('')
        path = os.path.join(self.directory, 'unjudged.tsv')
        self.run_census('--emit-unjudged', path)
        with open(path, encoding='utf-8') as handle:
            rows = handle.read().splitlines()
        self.assertEqual(rows[0].split('\t')[:3], ['country', 'filmId', 'title'])
        self.assertEqual(rows[1].split('\t')[1], 'f-close')
        self.assertEqual(len(rows), 5)

    def test_a_real_verdict_without_a_known_cause_is_refused(self):
        self.write_verdicts('DE\tf-close\tClose\treal\tbecause\t\t\n')
        with self.assertRaises(SystemExit) as refusal:
            self.run_census()
        self.assertIn('needs a cause', str(refusal.exception))

    def test_prod_port_is_refused(self):
        with self.assertRaises(SystemExit) as refusal:
            unmatched_census.main(['--mongo', 'mongodb://host:27017', '--from-dump', self.directory])
        self.assertIn('refusing :27017', str(refusal.exception))

    def test_checked_in_data_files_parse(self):
        unmatched_census.read_patterns(os.path.join(unmatched_census.DATA, 'event-patterns.txt'))
        self.assertTrue(unmatched_census.read_verdicts(os.path.join(unmatched_census.DATA, 'verdicts.tsv')))


if __name__ == '__main__':
    unittest.main()
