import json, sys, collections, datetime as dt
sys.path.insert(0, __import__('os').path.join(__import__('os').path.dirname(__file__),'../../ci'))
import identity_judgements as J
rows = J.read(__import__('os').path.join(__import__('os').path.dirname(__file__),'../../..')+'/test/resources/fixtures/identity-judgements.tsv')
labels = J.labels_by_country(rows)
def t(s): return dt.datetime.fromisoformat(s[:26].rstrip('Z'))
proj=[json.loads(l) for l in open('proj.jsonl')]
den=collections.defaultdict(int); first=collections.defaultdict(lambda: None); modes=collections.Counter()
for p in proj:
    den[p.get('country')]=max(den[p.get('country')], int(p.get('listings') or 0))
    k=(p.get('country'),p.get('build')); tt=t(p['_time'])
    if first[k] is None or tt<first[k]: first[k]=tt
    modes[(p['country'],p.get('projection_mode'))]+=1
ch=[json.loads(l) for l in open('changes.jsonl')]
ch.sort(key=lambda c:c['_time'])
for c in ch:
    c['t']=t(c['_time']); f=first[(c['country'],c.get('build'))]; c['deploy']= f is not None and (c['t']-f).total_seconds()<1800
    L=labels[c['country']]
    def v(f): return J.label_of(L,c['venue'],c['listing'],'tmdb:'+f) if f else None
    c['vb']=v(c.get('tmdb_before')); c['va']=v(c.get('tmdb_after'))
by=collections.defaultdict(list)
for c in ch: by[(c['country'],c['venue'],c['listing'])].append(c)
print('denominator listings', dict(den))
print('changes', len(ch), 'listings changed', len(by), 'deploy-window share', sum(c['deploy'] for c in ch)/len(ch))
cnt=collections.Counter(len(v) for v in by.values()); print('changes per listing', sorted(cnt.items())[:12])
# per country listings with >=2 changes, reversals
for cc in sorted(den):
    ls=[v for k,v in by.items() if k[0]==cc]
    multi=[v for v in ls if len(v)>=2]
    rev=[v for v in ls if any(v[j].get('tmdb_after')==v[i].get('tmdb_before') for i in range(len(v)) for j in range(i+1,len(v)))] if ls else []
    nodeploy=[v for v in ls if any(not c['deploy'] for c in v)]
    print(cc, 'listings',den[cc],'changed',len(ls),f"({100*len(ls)/max(den[cc],1):.1f}%)",'>=2 changes',len(multi),'reversal(A..B..A)',len(rev),'changed outside deploy window',len(nodeploy))
# judged flips
jc=collections.Counter()
for c in ch:
    if c['vb'] or c['va']: jc[(c['kind'],c['vb'],c['va'],'deploy' if c['deploy'] else 'steady')]+=1
print('judged changes (kind, before, after, window):')
for k,n in sorted(jc.items(), key=lambda x:-x[1]): print(' ',k,n)
json.dump([{k:(str(v) if k=='t' else v) for k,v in c.items()} for c in ch], open('changes_judged.json','w'))
