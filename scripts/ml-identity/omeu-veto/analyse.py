import json, pickle, collections, re, sys, unicodedata
T = pickle.load(open(sys.argv[1],'rb'))  # language-genre/index_tmdb.py output
d = json.load(open(sys.argv[2]))  # omeu-takes.js output
def norm(s):
    s=unicodedata.normalize('NFC', re.sub(r'\s+',' ',s or '').strip()).lower(); return s
J = {}
for line in open(sys.argv[3] + '/test/resources/fixtures/identity-judgements.tsv'):
    c=line.rstrip('\n').split('\t')
    if len(c)<5 or c[0]!='de' or not c[3].startswith('tmdb:'): continue
    J[(c[1], norm(c[2]), int(c[3][5:]))] = c[4]
def verdict(x):
    for v in (x['venue'], '*'):
        r = J.get((v, norm(x['title']), x['tmdb']))
        if r: return r
    return 'unjudged'
for x in d:
    x['shape'] = 'all' if x['k']==x['n'] else 'maj' if 2*x['k']>x['n'] else 'min'
    e = T.get(x['tmdb']) if x['tmdb'] else None
    x['lang'] = e['lang'] if e else None
    x['genres'] = sorted(e['genres']) if e else []
    x['verdict'] = verdict(x) if x['tmdb'] else '-'
c = collections.Counter((x['shape'], x['lang']=='en', x['lang'] is None, x['verdict']) for x in d if x['tmdb'])
for k,v in sorted(c.items()): print(v, k)
print('--- en takes')
seen=set()
for x in sorted(d, key=lambda x:(x['shape'],x['verdict'])):
    if x['lang']=='en':
        key=(x['title'],x['tmdb'])
        print(x['shape'], x['verdict'], x['k'],'/',x['n'], '|', x['venue'][:26], '|', x['title'][:45], '|', x['year'], x['director'], '| tmdb', x['tmdb'], x['tmdbBasis'], 'model', x['model'], x['modelBasis'], 'g', x['genres'])
