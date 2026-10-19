import pickle, sys, re, collections
sys.path.insert(0, sys.argv[1]); from features import *  # run.sh copies features.py into the out dir
S=sys.argv[1]; repo=sys.argv[2]
L = pickle.load(open(f'{S}/listings.pkl','rb')); T = pickle.load(open(f'{S}/tmdb.pkl','rb'))
def norm(s): return re.sub(r'\s+', ' ', s or '').strip()
# coverage
cov = collections.defaultdict(collections.Counter)
seen=set()
for (cc,v,t),r in L.items():
    if id(r) in seen: continue
    seen.add(id(r)); c=cov[cc]; c['listings']+=1
    eq,forb,src = language_constraint(cc, r['formats'], r['raw'], r['genres'])
    if eq or forb: c['lang']+=1
    if eq: c['lang.equals']+=1
    lg = listing_genres(r['genres'])
    if lg is not None: c['genres']+=1
    if lg and (lg & KEY): c['genres.docAnim']+=1
for cc in cov: print('COVERAGE', cc, dict(cov[cc]))
# by venue name lookup
byvt = collections.defaultdict(list)
for (cc,v,t),r in L.items(): byvt[(cc,t)].append((v,r))
rows=[]
for line in open(f'{repo}/test/resources/fixtures/identity-judgements.tsv'):
    if line.startswith('#') or line.startswith('country\t'): continue
    c=line.rstrip('\n').split('\t')
    if len(c)<5 or c[4] not in ('right','wrong') or not c[3].startswith('tmdb:'): continue
    cc,venue,title,film,verdict = c[:5]
    recs = [L[(cc,venue,norm(title))]] if (cc,venue,norm(title)) in L else ([r for v,r in byvt[(cc,norm(title))]] if venue=='*' else [])
    if not recs: rows.append((cc,venue,title,film,verdict,None,None,None)); continue
    fmts = sorted({f for r in recs for f in r['formats']}); gen = next((r['genres'] for r in recs if r['genres']), [])
    t = T.get(int(film[5:]), {})
    con = language_constraint(cc, fmts, title, gen)
    rows.append((cc,venue,title,film,verdict, language_level(con, t.get('lang')), genre_level(listing_genres(gen), t.get('genres')), (con, t.get('lang'), gen, sorted(t.get('genres',[])))))
found=[r for r in rows if r[5]]
print('judged rows', len(rows), 'found in recording', len(found))
for lvl_i,name in ((5,'LANG'),(6,'GENRE')):
    tab=collections.Counter((r[4], r[lvl_i]) for r in found)
    levels=sorted({k[1] for k in tab})
    print(name); [print(f'  {l:20s} right {tab[("right",l)]:5d}  wrong {tab[("wrong",l)]:5d}') for l in levels]
# per-token contradiction
tok=collections.Counter()
for r in found:
    if r[5] in ('match','contradicts'):
        for s in r[7][0][2]: tok[(s, r[4], r[5])]+=1
print('TOKENS (token, verdict, level)')
for s in sorted({k[0] for k in tok}):
    print(f'  {s:28s} R-match {tok[(s,"right","match")]:4d} R-contra {tok[(s,"right","contradicts")]:3d} W-match {tok[(s,"wrong","match")]:3d} W-contra {tok[(s,"wrong","contradicts")]:3d}')
print('CONTRADICTS examples')
for r in found:
    if r[5]=='contradicts' or r[6] in ('contradicts','contradicts:film'):
        print(' ', r[4], r[0], r[1][:28], '|', r[2][:50], '|', r[3], 'L', r[5], 'G', r[6], '| fl', r[7][1], 'src', sorted(r[7][0][2]), 'lg', r[7][2][:4], 'fg', r[7][3])
pickle.dump(rows, open(f'{S}/judged.pkl','wb'))
