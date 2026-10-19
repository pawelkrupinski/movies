import json, sys, collections, datetime as dt
sys.path.insert(0, __import__('os').path.join(__import__('os').path.dirname(__file__),'../../ci'))
import identity_judgements as J
ch=json.load(open('changes_judged.json'))
for c in ch: c['t']=dt.datetime.fromisoformat(c['t'])
start=min(c['t'] for c in ch)
by=collections.defaultdict(list)
for c in ch: by[(c['country'],c['venue'],c['listing'])].append(c)
def run(H, reset_on_deploy, hold_none):
    out=collections.Counter(); bills=collections.defaultdict(set); ex=collections.defaultdict(list)
    for k,v in by.items():
        prev=start  # held since at least window start
        for c in v:
            held=(c['t']-prev).total_seconds()/3600
            frozen = held>=H and not (reset_on_deploy and c['deploy']) and (hold_none or c.get('tmdb_before'))
            if frozen:
                cat=(c['kind'],c['vb'] or '?',c['va'] or '?')
                good = c['vb']=='right' and c['va']!='right' or c['va']=='wrong' and c['vb']!='wrong'
                bad = c['vb']=='wrong' and c['va']!='wrong' or c['va']=='right' and c['vb']!='right'
                tag='blocks-harm' if good else 'blocks-cure' if bad else 'unjudged'
                out[tag]+=1; bills[tag].add((c['country'],J.normalise(c['listing']),c.get('tmdb_before'),c.get('tmdb_after')))
                if len(ex[tag])<4: ex[tag].append((c['country'],c['venue'],c['listing'],c.get('tmdb_before'),c.get('tmdb_after'),c['vb'],c['va']))
            prev=c['t']
    return out,{k:len(s) for k,s in bills.items()},ex
for H in (6,24,48):
  for reset in (False,True):
    for hn in (False,True):
      o,b,ex=run(H,reset,hn)
      print(f"H={H}h reset={reset} holdNone={hn}: changes {dict(o)}  billings {b}")
o,b,ex=run(24,True,False)
for k in ex: print(k, ex[k])
# which side right in reversals (A->B->A): side held at the end vs flipped-to
side=collections.Counter()
for k,v in by.items():
    for i in range(len(v)):
        for j in range(i+1,len(v)):
            if v[j].get('tmdb_after')==v[i].get('tmdb_before'):
                side[(v[i]['vb'] or '?', v[i]['va'] or '?')]+=1; break
        else: continue
        break
print('reversal first flip (orig verdict, flipped-to verdict):', side)
