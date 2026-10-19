import json, os, sys, pickle
root = sys.argv[1]; out = sys.argv[2]
idx = {}  # id -> {'lang':..., 'genres':set(ids), 'src':'movie'|'search'}
def put(i, lang, genres, src):
    e = idx.setdefault(i, {'lang': None, 'genres': set(), 'src': src})
    if lang and not e['lang']: e['lang'] = lang
    if genres: e['genres'] |= set(genres)
    if src == 'movie': e['src'] = 'movie'
for cc in ['pl','de','uk','us','es']:
    base = f'{root}/enrichment-{cc}/api.themoviedb.org/3'
    for dirpath, _, files in os.walk(base):
        for f in files:
            p = os.path.join(dirpath, f)
            try: js = json.load(open(p))
            except Exception: continue
            if not isinstance(js, dict): continue
            if 'results' in js or 'movie_results' in js:
                for r in js.get('results', []) + js.get('movie_results', []):
                    if isinstance(r, dict) and 'id' in r and ('title' in r or 'original_title' in r):
                        put(r['id'], r.get('original_language'), r.get('genre_ids', []), 'search')
            elif 'id' in js and 'title' in js:
                put(js['id'], js.get('original_language'), [g['id'] for g in js.get('genres', []) if 'id' in g], 'movie')
print(len(idx), sum(1 for e in idx.values() if e['lang']), sum(1 for e in idx.values() if e['genres']))
pickle.dump(idx, open(out, 'wb'))
