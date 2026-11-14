#!/usr/bin/env python3
"""Census of the listings the identity resolver leaves unmatched, read from the local prod mirror (READ-ONLY).

Re-runs docs/investigations/2026-11-01-unmatched-real-films.md on the day's data:

  step 1  unmatched listings and clusters (a cluster is one filmId) per country, by the identity trace's basis
  step 2  the clusters an event title pattern sets aside, and how many of them still sit under a miss basis
  step 3  the hand verdicts' buckets over the other clusters: exact when every cluster is judged, else
          extrapolated from a listing-weighted sample (draw one with --emit-sample)
  step 4  a watch list of titles: matched now (its tmdbId), still unmatched (its basis), or no longer listed

    scripts/unmatched_census.py                              # every country, today, the checked-in data files
    scripts/unmatched_census.py --countries DE,UK --json
    scripts/unmatched_census.py --emit-sample 60 --sample-out /path/sample.tsv   # clusters to judge next
    scripts/unmatched_census.py --dump DIR                   # also keep each country's raw listings (<cc>.json)
    scripts/unmatched_census.py --from-dump DIR              # read those instead of the mirror

Run `scripts/local-mirror/freshness <cc>` first. The data files live in scripts/unmatched-census/:
event-patterns.txt (one case-insensitive regex a line), watch.tsv (country, a listing's exact title, any case) and verdicts.tsv (country,
filmId, title, verdict, cause, tmdbId, note). A verdict is found by filmId, else by the cluster's title, so an
unmatched film's id changing between runs keeps its verdict. Judge new clusters into verdicts.tsv.

Verdicts: real (a TMDB film that should match; `cause` one of CAUSES), double-bill, relay, not-film, not-on-tmdb,
unclear, no-candidate (no TMDB candidate at all, not looked up). Every trace basis other than OwnMatch and
PooledMatch is a miss; the projection-only bases (Event, Withdrawn, ...) are never stored in a trace.
"""
import argparse
import collections
import csv
import datetime
import json
import os
import random
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.join(HERE, 'unmatched-census')
COUNTRIES = ['PL', 'DE', 'UK', 'US', 'ES']
MATCH_BASES = {'OwnMatch', 'PooledMatch'}
NO_TRACE = 'NoTrace'
VERDICTS = {'real', 'double-bill', 'relay', 'not-film', 'not-on-tmdb', 'unclear', 'no-candidate'}
CAUSES = {'rating-cut', 'banner', 'companion-event', 'learned-veto', 'imdb-only', 'spelling', 'namesake-runner-up',
          'venue-typo', 'local-title', 'festival-veto', 'numbered-instalment', 'other'}
VERDICT_COLUMNS = ['country', 'filmId', 'title', 'verdict', 'cause', 'tmdbId', 'note']


def database_name(country):
    return 'kinowo_prod_mirror' if country == 'PL' else 'kinowo_%s_prod_mirror' % country.lower()


def listing_title(listing_key):
    """A listingKey is `N<NUL>venue<NUL>url<NUL>title` or `P<NUL>venue<NUL>title<NUL>year<NUL>director`."""
    parts = listing_key.split('\0')
    return parts[2] if parts[0] == 'P' else parts[-1]


def query_mirror(mongo, country, since):
    environment = dict(os.environ, UNMATCHED_CENSUS_DATABASE=database_name(country), UNMATCHED_CENSUS_SINCE=since)
    command = [os.path.join(HERE, 'local-mirror', 'mongosh-at.sh'), mongo, '--quiet', '--file',
               os.path.join(DATA, 'query.js')]
    return json.loads(subprocess.run(command, env=environment, capture_output=True, text=True, check=True).stdout)


def read_rows(path):
    if not path or not os.path.exists(path):
        return []
    with open(path, encoding='utf-8', newline='') as handle:
        return [row for row in csv.reader(handle, delimiter='\t') if row and row[0].strip() and not row[0].startswith('#')]


def read_patterns(path):
    if not path:
        return []
    with open(path, encoding='utf-8') as handle:
        return [re.compile(line.rstrip('\n'), re.I) for line in handle if line.strip() and not line.startswith('#')]


def read_verdicts(path):
    verdicts = []
    for row in read_rows(path):
        verdict = dict(zip(VERDICT_COLUMNS, row + [''] * (len(VERDICT_COLUMNS) - len(row))))
        if verdict['country'] == 'country':
            continue
        if verdict['verdict'] not in VERDICTS:
            sys.exit('%s: unknown verdict %r for %s (one of %s)' % (path, verdict['verdict'], verdict['filmId'], sorted(VERDICTS)))
        if verdict['verdict'] == 'real' and verdict['cause'] not in CAUSES:
            sys.exit('%s: a real film needs a cause, %r for %s is not one of %s' % (path, verdict['cause'], verdict['filmId'], sorted(CAUSES)))
        verdicts.append(verdict)
    return verdicts


def clusters_of(dump):
    """The unmatched listings grouped by filmId; a cluster's basis is its listings' commonest."""
    clusters = {}
    for listing in dump['listings']:
        if listing['tmdbId']:
            continue
        cluster = clusters.setdefault(listing['filmId'], {'filmId': listing['filmId'], 'listings': 0, 'showtimes': 0,
                                                          'titles': collections.Counter(), 'bases': collections.Counter(),
                                                          'trace': None})
        trace = listing['trace']
        basis = trace['basis'] if trace else NO_TRACE
        cluster['listings'] += 1
        cluster['showtimes'] += listing['showtimes']
        cluster['titles'][listing_title(listing['listingKey'])] += 1
        cluster['bases'][basis] += 1
        if trace and (cluster['trace'] is None or basis in ('BelowThreshold', 'Vetoed')):
            cluster['trace'] = trace
    for cluster in clusters.values():
        cluster['basis'] = cluster['bases'].most_common(1)[0][0]
        cluster['title'] = cluster['titles'].most_common(1)[0][0]
    return clusters


def verdict_for(country, cluster, by_film, by_title):
    return by_film.get((country, cluster['filmId'])) or by_title.get((country, cluster['title'].casefold()))


def census(country, dump, patterns, verdicts, watch):
    by_film = {(verdict['country'], verdict['filmId']): verdict for verdict in verdicts}
    by_title = {(verdict['country'], verdict['title'].casefold()): verdict for verdict in verdicts if verdict['title']}
    clusters = clusters_of(dump)
    listings_by_basis = collections.Counter()
    clusters_by_basis = collections.Counter()
    events = {'listings': 0, 'clusters': 0, 'under_miss_listings': 0, 'under_miss_clusters': 0}
    others = []
    for cluster in clusters.values():
        listings_by_basis.update(cluster['bases'])
        clusters_by_basis[cluster['basis']] += 1
        if any(pattern.search(title) for title in cluster['titles'] for pattern in patterns):
            events['listings'] += cluster['listings']
            events['clusters'] += 1
            if cluster['basis'] not in MATCH_BASES:
                events['under_miss_listings'] += cluster['listings']
                events['under_miss_clusters'] += 1
        else:
            cluster['verdict'] = verdict_for(country, cluster, by_film, by_title)
            others.append(cluster)
    judged = [cluster for cluster in others if cluster['verdict']]
    exhaustive = len(judged) == len(others)
    other_listings = sum(cluster['listings'] for cluster in others)
    buckets = {}
    for cluster in judged:
        verdict = cluster['verdict']
        key = verdict['verdict'] + ('/' + verdict['cause'] if verdict['verdict'] == 'real' else '')
        bucket = buckets.setdefault(key, {'judged_listings': 0, 'judged_clusters': 0, 'examples': []})
        bucket['judged_listings'] += cluster['listings']
        bucket['judged_clusters'] += 1
        bucket['examples'].append((cluster['listings'], cluster['title']))
    for bucket in buckets.values():
        # A listing-weighted sample: a bucket's share of the sampled CLUSTERS estimates its share of the LISTINGS.
        bucket['listings'] = bucket['judged_listings'] if exhaustive else round(bucket['judged_clusters'] / len(judged) * other_listings)
        bucket['examples'] = [title for _, title in sorted(bucket['examples'], reverse=True)[:3]]
    return {
        'listings': sum(cluster['listings'] for cluster in clusters.values()),
        'clusters': len(clusters),
        'all_listings': len(dump['listings']),
        'listings_by_basis': dict(listings_by_basis),
        'clusters_by_basis': dict(clusters_by_basis),
        'events': events,
        'other_listings': other_listings,
        'other_clusters': len(others),
        'judged_clusters': len(judged),
        'judged_listings': sum(cluster['listings'] for cluster in judged),
        'exhaustive': exhaustive,
        'buckets': buckets,
        'watch': watched(country, dump, watch),
        'unjudged': [cluster for cluster in others if not cluster['verdict']],
    }


def watched(country, dump, watch):
    results = []
    for watch_country, title in watch:
        if watch_country != country:
            continue
        hits = {}
        for listing in dump['listings']:
            listed = listing_title(listing['listingKey'])
            if title.casefold() == listed.casefold():
                state = 'tmdb %s' % listing['tmdbId'] if listing['tmdbId'] else \
                    'UNMATCHED ' + (listing['trace']['basis'] if listing['trace'] else NO_TRACE)
                hits[(listed, state)] = None
        results.append({'title': title, 'now': [{'listed': listed, 'state': state} for listed, state in hits] or 'not listed'})
    return results


def weighted_sample(unjudged, size, seed):
    """Clusters drawn without replacement, each with probability proportional to its listings."""
    generator = random.Random(seed)
    keyed = sorted(unjudged, key=lambda cluster: generator.random() ** (1.0 / cluster['listings']), reverse=True)
    return keyed[:size]


def write_clusters(path, country_clusters):
    with open(path, 'w', encoding='utf-8', newline='') as handle:
        writer = csv.writer(handle, delimiter='\t', lineterminator='\n')
        writer.writerow(['country', 'filmId', 'title', 'listings', 'basis', 'year', 'candidates', 'refusals', 'titles'])
        for country, cluster in country_clusters:
            trace = cluster['trace'] or {}
            writer.writerow([country, cluster['filmId'], cluster['title'], cluster['listings'], cluster['basis'],
                             trace.get('year') or '', ' ; '.join(trace.get('candidates') or []),
                             ' | '.join(trace.get('refusals') or []), ' || '.join(list(cluster['titles'])[:3])])


def render(since, results):
    lines = ['unmatched census, listings showing on or after %s' % since, '',
             'STEP 1  unmatched listings (clusters) by trace basis']
    for country, result in results.items():
        bases = ', '.join('%s %d' % item for item in sorted(result['listings_by_basis'].items(), key=lambda item: -item[1]))
        lines.append('  %-3s %5d of %6d listings (%4d clusters): %s' % (country, result['listings'], result['all_listings'],
                                                                       result['clusters'], bases))
    lines += ['', 'STEP 2  events by title pattern (under a miss basis = what the Event basis has yet to take)']
    for country, result in results.items():
        events = result['events']
        lines.append('  %-3s %5d listings (%4d clusters), %d (%d) under a miss basis; %d clusters left to judge'
                     % (country, events['listings'], events['clusters'], events['under_miss_listings'],
                        events['under_miss_clusters'], result['other_clusters']))
    lines += ['', 'STEP 3  verdicts over the clusters left: listings (judged listings / judged clusters), examples']
    for country, result in results.items():
        lines.append('  %-3s judged %d of %d clusters (%d of %d listings): %s'
                     % (country, result['judged_clusters'], result['other_clusters'], result['judged_listings'],
                        result['other_listings'], 'exact' if result['exhaustive'] else 'a sample, extrapolated'))
        for key, bucket in sorted(result['buckets'].items(), key=lambda item: -item[1]['listings']):
            lines.append('      %-28s %s%5d (%d / %d)  %s' % (key, '' if result['exhaustive'] else '~', bucket['listings'],
                                                             bucket['judged_listings'], bucket['judged_clusters'],
                                                             '; '.join(bucket['examples'])))
    lines += ['', 'STEP 4  watch list']
    for country, result in results.items():
        for entry in result['watch']:
            if entry['now'] == 'not listed':
                lines.append('  %-3s %-45s not listed' % (country, entry['title']))
            for hit in entry['now'] if entry['now'] != 'not listed' else []:
                lines.append('  %-3s %-45.45s %s' % (country, hit['listed'], hit['state']))
    return '\n'.join(lines)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--countries', default=','.join(COUNTRIES))
    parser.add_argument('--since', default=datetime.date.today().isoformat())
    parser.add_argument('--mongo', default='mongodb://127.0.0.1:28017', help='the local prod mirror; :27017 is refused')
    parser.add_argument('--event-patterns', default=os.path.join(DATA, 'event-patterns.txt'))
    parser.add_argument('--watch', default=os.path.join(DATA, 'watch.tsv'))
    parser.add_argument('--verdicts', default=os.path.join(DATA, 'verdicts.tsv'))
    parser.add_argument('--dump', metavar='DIR', help='also write each country\'s raw listings to DIR/<cc>.json')
    parser.add_argument('--from-dump', metavar='DIR', help='read DIR/<cc>.json instead of the mirror')
    parser.add_argument('--emit-unjudged', metavar='FILE', help='every cluster still needing a verdict, as TSV')
    parser.add_argument('--emit-sample', type=int, metavar='N', help='a listing-weighted sample of N unjudged clusters per country')
    parser.add_argument('--sample-out', metavar='FILE', help='where --emit-sample writes (default: stdout)')
    parser.add_argument('--seed', type=int, default=0)
    parser.add_argument('--json', action='store_true')
    arguments = parser.parse_args(argv)
    if re.search(r':27017\b', arguments.mongo):
        sys.exit('refusing :27017 (prod); the census reads the local mirror on :28017')
    countries = [country.strip().upper() for country in arguments.countries.split(',') if country.strip()]
    patterns = read_patterns(arguments.event_patterns)
    verdicts = read_verdicts(arguments.verdicts)
    watch = [(row[0].upper(), row[1]) for row in read_rows(arguments.watch) if len(row) > 1 and row[0] != 'country']
    results = {}
    for country in countries:
        if arguments.from_dump:
            with open(os.path.join(arguments.from_dump, country + '.json'), encoding='utf-8') as handle:
                dump = json.load(handle)
        else:
            dump = query_mirror(arguments.mongo, country, arguments.since)
        if arguments.dump:
            os.makedirs(arguments.dump, exist_ok=True)
            with open(os.path.join(arguments.dump, country + '.json'), 'w', encoding='utf-8') as handle:
                json.dump(dump, handle, ensure_ascii=False)
        results[country] = census(country, dump, patterns, verdicts, watch)
    if arguments.emit_unjudged:
        write_clusters(arguments.emit_unjudged, [(country, cluster) for country, result in results.items()
                                                 for cluster in sorted(result['unjudged'], key=lambda c: -c['listings'])])
    if arguments.emit_sample:
        write_clusters(arguments.sample_out or '/dev/stdout',
                       [(country, cluster) for country, result in results.items()
                        for cluster in weighted_sample(result['unjudged'], arguments.emit_sample, arguments.seed)])
        if not arguments.sample_out:
            return 0
    for result in results.values():
        result.pop('unjudged')
    print(json.dumps({'since': arguments.since, 'countries': results}, ensure_ascii=False, indent=1) if arguments.json
          else render(arguments.since, results))
    return 0


if __name__ == '__main__':
    sys.exit(main())
