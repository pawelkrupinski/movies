import sys, json, gzip, collections
sys.argv, args = sys.argv[:2], sys.argv[2:]
exec(open(__file__.replace('detail.py','slots.py')).read().split('for cc in countries:')[0])
cc, want = args[0], args[1:]
venues = json.load(gzip.open(f'{corpus}/cinema-scrapes-{cc}.json.gz'))
films = json.load(gzip.open(f'{corpus}/prod-films-{cc}.json.gz'))
take = {}
for f in films:
    for s in f.get('slots', []):
        v, k = s.split('␟', 1); take[(v, k)] = f.get('tmdbId')
for slot in want:
    by_val = collections.defaultdict(list)
    for v in venues:
        for f in v['films']:
            urls = [s.get('bookingUrl') for s in f['showtimes'] if s.get('bookingUrl')]
            vals = set()
            for u in urls:
                vals |= {val for k, val in tokens(u, 'book') if k == slot}
            if f.get('filmUrl'):
                vals |= {val for k, val in tokens(f['filmUrl'], 'film') if k == slot}
            if len(vals) == 1:
                by_val[vals.pop()].append((v['_id'], f['movie']['title'], take.get((v['_id'], norm(f['movie']['title'])), 'nojoin')))
    print('==', slot)
    for val, g in by_val.items():
        keys = {norm(t) for _, t, _ in g}; tk = {x for _, _, x in g if x not in ('nojoin', None)}
        if len(keys) > 1 or len(tk) > 1:
            print(' ', val, 'takes', sorted(map(str, {x for *_, x in g})))
            for t, n in collections.Counter((t, str(x)) for _, t, x in g).most_common(6): print('     ', n, t)
