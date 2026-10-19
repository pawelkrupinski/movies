import gzip, json, re, pickle, sys
root = sys.argv[1]; out = sys.argv[2]
def norm(s): return re.sub(r'\s+', ' ', s or '').strip()
idx = {}
for cc in ['pl','de','uk','us','es']:
    for v in json.load(gzip.open(f'{root}/corpus/cinema-scrapes-{cc}.json.gz')):
        venue = v['_id']
        for f in v['films']:
            m = f['movie']
            raw = m.get('rawTitle') or m['title']
            fmts = sorted({x for s in f.get('showtimes', []) for x in s.get('format', [])})
            rec = {'formats': fmts, 'genres': m.get('genres', []), 'title': m['title'], 'raw': raw}
            for t in {norm(raw), norm(m['title'])}:
                e = idx.setdefault((cc, venue, t), rec)
                if e is not rec:
                    e['formats'] = sorted(set(e['formats']) | set(fmts)); e['genres'] = e['genres'] or rec['genres']
pickle.dump(idx, open(out, 'wb')); print(len(idx))
