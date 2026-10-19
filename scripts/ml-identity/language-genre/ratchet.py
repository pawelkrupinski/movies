import pickle, sys, re, collections
sys.path.insert(0, sys.argv[1]); from features import *  # run.sh copies features.py into the out dir
S=sys.argv[1]
L = pickle.load(open(f'{S}/listings.pkl','rb')); T = pickle.load(open(f'{S}/tmdb.pkl','rb'))
def norm(s): return re.sub(r'\s+', ' ', s or '').strip()
c = collections.Counter()
for line in open(sys.argv[2]):
    p = line.rstrip('\n').split('\t')
    if p[0] not in ('WRONG','UNJUDGED','RIGHT','NEW') or not p[4].startswith('tmdb:'): continue
    st, cc, venue, raw, film, title = p[:6]
    rec = L.get((cc, venue, norm(raw))); t = T.get(int(film[5:]), {})
    if not rec: c[(st,'nolisting')] += 1; continue
    con = language_constraint(cc, rec['formats'], raw, rec['genres'])
    ll = language_level(con, t.get('lang')); gl = genre_level(listing_genres(rec['genres']), t.get('genres'))
    c[(st,'L:'+ll)] += 1; c[(st,'G:'+gl)] += 1
    if st in ('WRONG','UNJUDGED') or ll=='contradicts' or gl=='contradicts':
        print(st, cc, venue[:22], '|', raw[:40], '|', title[:28], '| fl', t.get('lang'), 'L', ll, sorted(con[2]), 'G', gl, rec['genres'][:3], sorted(t.get('genres',[])))
for k in sorted(c): print(k, c[k])
