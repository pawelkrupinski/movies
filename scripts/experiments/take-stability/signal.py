import json, sys, collections
sys.path.insert(0, __import__('os').path.join(__import__('os').path.dirname(__file__),'../../ci'))
import identity_judgements as J
rows = J.read(__import__('os').path.join(__import__('os').path.dirname(__file__),'../../..')+'/test/resources/fixtures/identity-judgements.tsv')
labels = J.labels_by_country(rows)
kw={(r.country,J.normalise(r.rawTitle),r.film) for r in rows if r.state=='known-wrong'}
ch=json.load(open('changes_judged.json'))
by=collections.defaultdict(list)
for c in ch: by[(c['country'],c['venue'],c['listing'])].append(c)
def cls(v):
    if not v: return 'stable'
    films={c.get('tmdb_before') for c in v}|{c.get('tmdb_after') for c in v}
    steady=[c for c in v if not c['deploy']]
    rev=any(v[j].get('tmdb_after')==v[i].get('tmdb_before') for i in range(len(v)) for j in range(i+1,len(v)))
    if rev: return 'reversed'
    if len(v)>=2: return 'multi'
    return 'once-steady' if steady else 'once-deploy'
tab=collections.Counter(); ex=collections.defaultdict(list)
for line in open('traces.tsv'):
    cc,j=line.split('\t',1); d=json.loads(j)
    if d['f'] is None: continue
    film='tmdb:%d'%d['f']; verdict=J.label_of(labels[cc],d['v'],d['t'],film) or 'unjudged'
    k=cls(by.get((cc,d['v'],d['t'])))
    tab[(k,verdict)]+=1
    if verdict=='wrong': ex[k].append((cc,d['v'],d['t'],film,(cc,J.normalise(d['t']),film) in kw))
print('current matched takes: flip class x verdict')
for k in ['stable','once-deploy','once-steady','multi','reversed']:
    r,w,u=tab[(k,'right')],tab[(k,'wrong')],tab[(k,'unjudged')]
    print(f"{k:12} right {r:6} wrong {w:4} unjudged {u:6}  wrong share of judged {w/max(r+w,1):.3f}")
for k in ex: print(k, ex[k][:4])
print('--- distinct billings (cc, normalised title, film)')
bill=collections.defaultdict(set)
for line in open('traces.tsv'):
    cc,j=line.split('\t',1); d=json.loads(j)
    if d['f'] is None: continue
    film='tmdb:%d'%d['f']; verdict=J.label_of(labels[cc],d['v'],d['t'],film) or 'unjudged'
    bill[(cls(by.get((cc,d['v'],d['t']))),verdict)].add((cc,J.normalise(d['t']),film))
for k in ['stable','once-deploy','once-steady','multi','reversed']:
    r,w=len(bill[(k,'right')]),len(bill[(k,'wrong')])
    print(f"{k:12} right {r:5} wrong {w:4} unjudged {len(bill[(k,'unjudged')]):6} wrong share {w/max(r+w,1):.3f}")
print('reversed wrong billings:', sorted(bill[('reversed','wrong')])[:30])
print('reversed right billings sample:', sorted(bill[('reversed','right')])[:15])
