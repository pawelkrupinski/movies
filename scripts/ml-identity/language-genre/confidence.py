import gzip, pickle, sys, re, collections, numpy as np
sys.path.insert(0, sys.argv[1]); from features import *  # run.sh copies features.py into the out dir
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GroupKFold
from sklearn.metrics import roc_auc_score, log_loss, brier_score_loss
S, repo, which = sys.argv[1], sys.argv[2], sys.argv[3]
L = pickle.load(open(f'{S}/listings.pkl','rb')); T = pickle.load(open(f'{S}/tmdb.pkl','rb'))
def norm(s): return re.sub(r'\s+', ' ', s or '').strip()
J = {}
for line in open(f'{repo}/test/resources/fixtures/identity-judgements.tsv'):
    c = line.rstrip('\n').split('\t')
    if line.startswith('#') or len(c) < 5 or c[4] not in ('right','wrong'): continue
    J[(c[0], c[1], norm(c[2]).lower(), c[3])] = 1 if c[4]=='right' else 0
def judged(cc, venue, title, film):
    k = norm(title).lower()
    return J.get((cc, venue, k, film), J.get((cc, '*', k, film)))
f = gzip.open(f'{repo}/test/resources/fixtures/identity-unified/{which}.tsv.gz', 'rt')
H = f.readline().rstrip('\n').split('\t'); rows = [dict(zip(H, l.rstrip('\n').split('\t'))) for l in f]
num = [h for h in H[13:]]
X0=[]; XL=[]; XG=[]; y=[]; grp=[]; hand=[]; meta=[]
LANG_LV = ['contradicts','match:equals','match:not']; GEN_LV = ['agree','contradicts','contradicts:film','neutral']
cnt = collections.Counter()
for r in rows:
    cc = r['country']; j = judged(cc, r['venue'], r['rawTitle'], r['film'])
    lab = j if j is not None else ({'1':1,'0':0}.get(r['label']))
    if lab is None or not r['film'].startswith('tmdb:'): continue
    rec = L.get((cc, r['venue'], norm(r['rawTitle'])))
    t = T.get(int(r['film'][5:]), {})
    if rec:
        con = language_constraint(cc, rec['formats'], r['rawTitle'], rec['genres'])
        ll = language_level(con, t.get('lang'))
        if ll == 'match': ll = 'match:equals' if con[0] else 'match:not'
        gl = genre_level(listing_genres(rec['genres']), t.get('genres'))
    else: ll = gl = 'nolisting'; con=None
    cnt[('found' if rec else 'missing')] += 1
    X0.append([float(r[h] or 0) for h in num]); XL.append([ll == v for v in LANG_LV]); XG.append([gl == v for v in GEN_LV])
    y.append(lab); grp.append(r['cluster']); hand.append(j is not None or r['source']=='hand')
    meta.append((r, ll, gl, lab, j is not None, con, t.get('lang')))
X0=np.array(X0); XL=np.array(XL,float); XG=np.array(XG,float); y=np.array(y); hand=np.array(hand)
print(which, dict(cnt), 'rows', len(y), 'right', int(y.sum()), 'hand', int(hand.sum()), 'hand-wrong', int(((1-y)*hand).sum()))
print('coverage: lang known', int((XL.sum(1)>0).sum()), 'genre known', int((XG.sum(1)>0).sum()))
def cv(X, label, cols):
    p = np.zeros(len(y)); w = np.where(hand, 5.0, 1.0)
    mu = X.mean(0); sd = X.std(0) + 1e-9; Xs = (X - mu)/sd
    for tr, te in GroupKFold(5).split(Xs, y, grp):
        m = LogisticRegression(C=1.0, max_iter=3000).fit(Xs[tr], y[tr], sample_weight=w[tr]); p[te] = m.predict_proba(Xs[te])[:,1]
    m = LogisticRegression(C=1.0, max_iter=3000).fit(Xs, y, sample_weight=w)
    out = [label]
    for mask, nm in ((np.ones(len(y),bool),'all'), (hand,'hand')):
        out.append(f'{nm}: AUC {roc_auc_score(y[mask],p[mask]):.4f} LL {log_loss(y[mask],p[mask]):.4f} Brier {brier_score_loss(y[mask],p[mask]):.4f}')
    coefs = {c: round(float(k/sd[i]),2) for i,(c,k) in enumerate(zip(cols, m.coef_[0])) if c.startswith(('lang','genre'))}
    print(' | '.join(out), coefs); return p
LC = ['lang.'+v for v in LANG_LV]; GC = ['genre.'+v for v in GEN_LV]
base = cv(X0, 'base', num)
cv(np.hstack([X0[:, :1], X0[:, 1:2]]), 'logit-only', ['model.logit','model.unscored'])
cv(np.hstack([X0[:, :2], XL]), 'logit+lang', ['model.logit','model.unscored']+LC)
cv(np.hstack([X0[:, :2], XG]), 'logit+genre', ['model.logit','model.unscored']+GC)
pl = cv(np.hstack([X0, XL]), 'base+lang', num+LC)
pg = cv(np.hstack([X0, XG]), 'base+genre', num+GC)
pb = cv(np.hstack([X0, XL, XG]), 'base+both', num+LC+GC)
# today's takes flagged
print('TODAY TAKES by level (label: right/wrong)')
for idx, name in ((1,'lang'),(2,'genre')):
    c = collections.Counter((m[idx], m[3]) for m in meta if m[0]['today']=='1')
    print(' ', name, sorted(c.items()))
print('TODAY TAKES contradicted (lang):')
for m in meta:
    if m[0]['today']=='1' and m[1]=='contradicts':
        print('  ', 'RIGHT' if m[3] else 'WRONG', 'hand' if m[4] else 'weak', m[0]['country'], m[0]['venue'][:25], '|', m[0]['rawTitle'][:45], '|', m[0]['film'], m[0]['filmTitle'][:35], 'fl', m[6], sorted(m[5][2]))
print('TODAY TAKES contradicted (genre):')
for m in meta:
    if m[0]['today']=='1' and m[2].startswith('contradicts'):
        print('  ', 'RIGHT' if m[3] else 'WRONG', 'hand' if m[4] else 'weak', m[2], m[0]['country'], m[0]['venue'][:25], '|', m[0]['rawTitle'][:45], '|', m[0]['film'], m[0]['filmTitle'][:35])
print('CALIBRATION-STYLE COUNTS per level: (positives, negatives) over all contenders')
for idx, name in ((1,'lang'),(2,'genre')):
    c = collections.Counter((m[idx], m[3]) for m in meta)
    for lv in sorted({k[0] for k in c}): print(f'  {name} {lv:18s} pos {c[(lv,1)]:6d} neg {c[(lv,0)]:6d}')
print('MARGINS: contenders whose level moves score; model.logit of today takes contradicted / of non-takes matched-equals')
for m in meta:
    if m[1]=='contradicts' and m[0]['today']=='1': pass
lc=[float(m[0]['model.logit']) for m in meta if m[1]=='contradicts' and m[0]['today']=='1']
print('  lang-contradicted takes logit', sorted(lc))
ge=[(float(m[0]['model.logit']), m[3], m[0]['rawTitle'][:30]) for m in meta if m[1]=='match:equals' and m[0]['today']!='1']
print('  lang-equals non-takes', sorted(ge)[-8:])
gc=sorted(float(m[0]['model.logit']) for m in meta if m[2]=='contradicts' and m[0]['today']=='1')
print('  genre-contradicted takes logit min..', gc[:10])
ga=[(float(m[0]['model.logit']), m[3], m[0]['rawTitle'][:30]) for m in meta if m[2]=='agree' and m[0]['today']!='1']
print('  genre-agree non-takes top', sorted(ga)[-8:])
import math
print('TAKE SIMULATION (model stage, best contender per cluster, accept if logit+w >= CUT)')
CUT = 0.27
prior = math.log(y.sum()/(len(y)-y.sum()))
def calib_w(idx):
    c = collections.Counter((m[idx], m[3]) for m in meta)
    return {lv: math.log((c[(lv,1)]+0.5)/(c[(lv,0)]+0.5)) - prior for lv in {k[0] for k in c} if not lv.startswith(('unknown','nolisting'))}
variants = {
  'lang (in-logit fit)': (1, {'contradicts': -0.75, 'match:equals': 0.55, 'match:not': 0.09}),
  'lang (calibration-style)': (1, calib_w(1)),
  'genre (in-logit fit)': (2, {'agree': 1.12, 'contradicts': -1.9, 'contradicts:film': -0.67, 'neutral': -0.6}),
  'genre (calibration-style)': (2, calib_w(2)),
}
byc = collections.defaultdict(list)
for m in meta: byc[m[0]['cluster']].append(m)
def take(ms, wfun):
    best = max(ms, key=lambda m: float(m[0]['model.logit']) + wfun(m))
    return best if float(best[0]['model.logit']) + wfun(best) >= CUT else None
for name, (idx, w) in variants.items():
    print(' ', name, {k: round(v,2) for k,v in w.items()})
    moves = collections.Counter(); ex = []
    for cl, ms in byc.items():
        a = take(ms, lambda m: 0.0); b = take(ms, lambda m: w.get(m[idx], 0.0))
        if a is b: continue
        kind = ('lost-right' if a and a[3] else 'cured-wrong' if a else '') + ('/' if a and b else '') + ('gained-right' if b and b[3] else 'new-wrong' if b else '')
        moves[kind] += 1; ex.append((kind, ms[0][0]['country'], ms[0][0]['rawTitle'][:40], (a or b)[0]['filmTitle'][:30], 'hand' if (a or b)[4] else 'weak'))
    print('    ', dict(moves)); [print('      ', e) for e in ex[:8]]
