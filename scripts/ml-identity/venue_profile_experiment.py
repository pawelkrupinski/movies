"""Venue-history prior, OFFLINE experiment (2026-10-19, branch experiment-venue-history; docs/identity-experiments.md).

Data: the LOCAL prod mirror (read-only, after scripts/local-mirror/freshness), exported per country to <data dir>:
  traces-<cc>.jsonl  db.identity_traces -> {v: listing.venue, r: listing.rawTitle, f: film, b: basis, c: candidates, e: evidence}
  films-<cc>.jsonl   db.tmdb_films      -> {id: _id, y: record.year}
Run: python3 -I scripts/ml-identity/venue_profile_experiment.py <repo root>   (from <data dir>; needs numpy, scikit-learn)
"""
import json, math, re, sys, os, collections
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score, log_loss, brier_score_loss
from sklearn.model_selection import GroupKFold

ROOT = sys.argv[1]
HERE = os.getcwd()  # the data dir
sys.path.insert(0, os.path.join(ROOT, "scripts/ci"))
import identity_judgements as J

NOW = 2026
BUCKETS = [(0, 1), (2, 4), (5, 14), (15, 34), (35, 999)]
CAND = re.compile(r"^(\d+) ([\d.]+)% rank (\S+) (.*?)'(.*)'(?: \((\d{4})\))?\s*$")


def bucket(age):
    for i, (lo, hi) in enumerate(BUCKETS):
        if lo <= age <= hi:
            return i
    return 0


def parse_cands(cs):
    out = []
    for c in cs or []:
        m = CAND.match(c)
        if not m:
            continue
        out.append(dict(id=int(m.group(1)), p=float(m.group(2)) / 100, rank=m.group(3), denied="DENIED" in m.group(4),
                        year=int(m.group(6)) if m.group(6) else None, title=m.group(5)))
    return out


rows = J.read(os.path.join(ROOT, "test/resources/fixtures/identity-judgements.tsv"))
labels = J.labels_by_country(rows)
expected = collections.defaultdict(dict)
for n, cols in J.lines(os.path.join(ROOT, "test/resources/fixtures/identity-unmatched/expected-matches.tsv")):
    if cols[0] != "country":
        expected[cols[0]][(cols[1], J.normalise(cols[2]))] = cols[3]

data = []  # candidate rows
takes_report = []
same_title_prior = collections.Counter()
for cc in ["pl", "uk", "de", "us", "es"]:
    path = os.path.join(HERE, f"traces-{cc}.jsonl")
    if not os.path.exists(path):
        continue
    years = {}
    genres = {}
    for line in open(os.path.join(HERE, f"films-{cc}.jsonl")):
        f = json.loads(line)
        try:
            years[int(f["id"])] = f.get("y")
            genres[int(f["id"])] = f.get("g") or []
        except ValueError:
            pass
    traces = [json.loads(l) for l in open(path)]
    # venue profile: per venue, per normalised title, the taken film's age bucket (and documentary flag)
    by_venue = collections.defaultdict(lambda: collections.defaultdict(list))
    country_counts = np.zeros(len(BUCKETS))
    for t in traces:
        if t["f"] is None:
            continue
        y = years.get(t["f"])
        if y is None:
            for c in parse_cands(t["c"]):
                if c["id"] == t["f"]:
                    y = c["year"]
        if y is None:
            continue
        b = bucket(NOW - y)
        by_venue[t["v"]][J.normalise(t["r"]).lower()].append(b)
        country_counts[b] += 1
    country_p = (country_counts + 1) / (country_counts.sum() + len(BUCKETS))
    venue_tot = {v: np.bincount([b for bs in d.values() for b in bs], minlength=len(BUCKETS)) for v, d in by_venue.items()}
    ALPHA = 10.0

    def llr(venue, title, year):
        if year is None:
            return 0.0, 0
        d = by_venue.get(venue)
        counts = venue_tot.get(venue, np.zeros(len(BUCKETS))).astype(float).copy()
        if d is not None:
            own = d.get(J.normalise(title).lower(), [])
            counts -= np.bincount(own, minlength=len(BUCKETS)) if own else 0
        n = counts.sum()
        pv = (counts + ALPHA * country_p) / (n + ALPHA)
        b = bucket(NOW - year)
        return math.log(pv[b] / country_p[b]), int(n)

    lab = labels.get(cc, {})
    for t in traces:
        cands = parse_cands(t["c"])
        if not cands:
            continue
        venue, raw = t["v"], t["r"]
        verdicts = [J.label_of(lab, venue, raw, f"tmdb:{c['id']}") for c in cands]
        if not any(v is not None for v in verdicts):
            continue
        group = f"{cc}|{J.normalise(raw).lower()}"
        prev = expected[cc].get((venue, J.normalise(raw)))
        for c, v in zip(cands, verdicts):
            if v is None:
                continue
            x, n = llr(venue, raw, c["year"])
            p = min(max(c["p"], 1e-4), 1 - 1e-4)
            data.append(dict(cc=cc, venue=venue, raw=raw, film=c["id"], title=c["title"], year=c["year"], p=p,
                             logit=math.log(p / (1 - p)), llr=x, n=n, y=1 if v == "right" else 0, group=group,
                             taken=(t["f"] == c["id"]), denied=c["denied"], prior=(prev == f"tmdb:{c['id']}") if prev else None,
                             listing=f"{cc}|{venue}|{raw}"))

print(f"candidate rows {len(data)}, listings {len({d['listing'] for d in data})}, right {sum(d['y'] for d in data)}")
X_logit = np.array([[d["logit"]] for d in data])
X_both = np.array([[d["logit"], d["llr"]] for d in data])
y = np.array([d["y"] for d in data])
groups = np.array([d["group"] for d in data])


def cv(X):
    pred = np.zeros(len(y))
    for tr, te in GroupKFold(5).split(X, y, groups):
        m = LogisticRegression(C=1.0).fit(X[tr], y[tr])
        pred[te] = m.predict_proba(X[te])[:, 1]
    return pred


for name, X in [("logit only", X_logit), ("logit + venue-age LLR", X_both), ("LLR only", np.array([[d['llr']] for d in data]))]:
    pr = cv(X)
    print(f"{name:24s} AUC {roc_auc_score(y, pr):.4f}  logloss {log_loss(y, pr):.4f}  brier {brier_score_loss(y, pr):.4f}")
m = LogisticRegression(C=1.0).fit(X_both, y)
print("full fit coef logit, llr:", m.coef_, m.intercept_)
print("raw LLR AUC (right vs wrong):", round(roc_auc_score(y, [d["llr"] for d in data]), 4))
with_year = [d for d in data if d["year"] and d["n"] >= 10]
print("rows with year and venue n>=10:", len(with_year), "AUC LLR", round(roc_auc_score([d['y'] for d in with_year], [d['llr'] for d in with_year]), 4))

# current takes judged: does LLR flag the wrong ones?
taken = [d for d in data if d["taken"]]
tw = [d for d in taken if d["y"] == 0]
tr_ = [d for d in taken if d["y"] == 1]
print(f"current takes judged: {len(tr_)} right, {len(tw)} wrong; AUC(LLR) on takes {roc_auc_score([d['y'] for d in taken], [d['llr'] for d in taken]):.4f}")
for cut in (-0.5, -1.0, -1.5):
    print(f"  LLR < {cut}: wrong flagged {sum(d['llr'] < cut for d in tw)}, right flagged {sum(d['llr'] < cut for d in tr_)}")
for d in sorted(tw, key=lambda d: d["llr"])[:12]:
    print(f"  WRONG take llr {d['llr']:+.2f} n={d['n']:4d} {d['cc']} {d['venue'][:30]:30s} «{d['raw'][:40]}» → {d['title'][:30]} ({d['year']})")

# within-listing: pairs (right, wrong) of different age buckets; does the prior order them right?
by_listing = collections.defaultdict(list)
for d in data:
    by_listing[d["listing"]].append(d)
agree = disagree = tie = 0
flips = []
for l, ds in by_listing.items():
    rights = [d for d in ds if d["y"] == 1]
    wrongs = [d for d in ds if d["y"] == 0 and d["year"]]
    for r in rights:
        for w in wrongs:
            if r["year"] is None or bucket(NOW - r["year"]) == bucket(NOW - w["year"]):
                continue
            if r["llr"] > w["llr"]:
                agree += 1
            elif r["llr"] < w["llr"]:
                disagree += 1
            else:
                tie += 1
    # re-rank with the fitted weight: does the top candidate change?
    score = lambda d: m.coef_[0][0] * d["logit"] + m.coef_[0][1] * d["llr"]
    top0 = max(ds, key=lambda d: d["logit"])
    top1 = max(ds, key=score)
    if top0 is not top1:
        flips.append((top0, top1))
print(f"remake pairs (right vs wrong of a different age bucket): prior orders right {agree}, wrong {disagree}, tie {tie}")
print(f"top-candidate flips under the fitted weight: {len(flips)}; to right {sum(b['y'] for a, b in flips)}, to wrong {sum(1 - b['y'] for a, b in flips)}")
for a, b in flips[:10]:
    print(f"  {a['cc']} {a['venue'][:28]} «{a['raw'][:36]}»: {a['title'][:22]} ({a['year']}) y={a['y']} → {b['title'][:22]} ({b['year']}) y={b['y']}")

# prior take at the same venue + raw title (expected-matches.tsv, the ratchet's earlier takes)
pr = [d for d in data if d["prior"] is not None]
print("same venue+raw earlier take available for", len({d['listing'] for d in pr}), "judged listings")
c = collections.Counter((d["prior"], d["y"]) for d in pr)
print("  prior==cand & right", c[(True, 1)], " prior==cand & wrong", c[(True, 0)], " prior!=cand & right", c[(False, 1)], " prior!=cand & wrong", c[(False, 0)])
tw_prior = [d for d in tw if d["prior"] is not None]
print("  current WRONG takes with an earlier take:", len(tw_prior), "earlier take = same wrong film:", sum(d["prior"] for d in tw_prior))

# ---- decision proxy and subsets ----
w = m.coef_[0][1] / m.coef_[0][0]
print(f"\nLLR weight in calibrated-logit units: {w:.2f}")
def norm(s): return re.sub(r"[^\w]+", " ", s.lower()).strip()
moved = collections.Counter(); examples = []
for l, ds in by_listing.items():
    live = [d for d in ds if not d["denied"]]
    if len(live) < 2: continue
    a = max(live, key=lambda d: d["logit"]); b = max(live, key=lambda d: d["logit"] + w * d["llr"])
    if a is b: continue
    contested = norm(a["title"]) == norm(b["title"])
    moved[(contested, a["y"], b["y"])] += 1
    if contested and len(examples) < 12: examples.append((a, b))
print("argmax moves among non-denied (same-title?, from y, to y):", dict(moved))
for a, b in examples:
    print(f"  {a['cc']} {a['venue'][:28]} «{a['raw'][:34]}»: ({a['year']}) y={a['y']} → ({b['year']}) y={b['y']}  llr {a['llr']:+.2f}/{b['llr']:+.2f}")
cont = [d for l, ds in by_listing.items() if len({norm(x['title']) for x in ds}) < len(ds) for d in ds]
yy = [d['y'] for d in cont]
if len(set(yy)) == 2:
    print(f"same-title (remake/bare) listings: rows {len(cont)}; AUC logit {roc_auc_score(yy,[d['logit'] for d in cont]):.4f}, logit+w*llr {roc_auc_score(yy,[d['logit']+w*d['llr'] for d in cont]):.4f}, llr {roc_auc_score(yy,[d['llr'] for d in cont]):.4f}")
for cc in ["pl","uk","de","us","es"]:
    s = [d for d in data if d["cc"] == cc]; ys = [d["y"] for d in s]
    if len(set(ys)) == 2:
        print(f"  {cc}: rows {len(s)} AUC logit {roc_auc_score(ys,[d['logit'] for d in s]):.4f} → +llr {roc_auc_score(ys,[d['logit']+w*d['llr'] for d in s]):.4f}")
# takes: right takes the prior would most penalise
print("RIGHT takes with the lowest LLR:")
for d in sorted(tr_, key=lambda d: d["llr"])[:6]:
    print(f"  llr {d['llr']:+.2f} n={d['n']:4d} {d['cc']} {d['venue'][:30]:30s} «{d['raw'][:40]}» → {d['title'][:30]} ({d['year']})")
print("WRONG takes with the highest LLR:")
for d in sorted(tw, key=lambda d: -d["llr"])[:6]:
    print(f"  llr {d['llr']:+.2f} n={d['n']:4d} {d['cc']} {d['venue'][:30]:30s} «{d['raw'][:40]}» → {d['title'][:30]} ({d['year']})")

print("\n-- CV within subsets (5-fold grouped by title), logit vs logit+llr --")
def cv_sub(rows, feats):
    X = np.array([[d[f] for f in feats] for d in rows]); yv = np.array([d['y'] for d in rows]); g = np.array([d['group'] for d in rows])
    pred = np.zeros(len(yv))
    for tr, te in GroupKFold(5).split(X, yv, g):
        pred[te] = LogisticRegression().fit(X[tr], yv[tr]).predict_proba(X[te])[:, 1]
    return roc_auc_score(yv, pred), log_loss(yv, pred), brier_score_loss(yv, pred)
for name, rows_ in [("all, with country dummies", None), ("same-title", cont)] + [(cc, [d for d in data if d['cc']==cc]) for cc in ["pl","uk","de","us"]]:
    if rows_ is None:
        for d in data:
            for cc in ["pl","uk","de","us"]: d['c_'+cc] = 1.0 if d['cc']==cc else 0.0
        a = cv_sub(data, ['logit','c_pl','c_uk','c_de','c_us']); b = cv_sub(data, ['logit','llr','c_pl','c_uk','c_de','c_us'])
    else:
        a = cv_sub(rows_, ['logit']); b = cv_sub(rows_, ['logit','llr'])
    print(f"  {name:28s} AUC {a[0]:.4f}→{b[0]:.4f}  logloss {a[1]:.4f}→{b[1]:.4f}  brier {a[2]:.4f}→{b[2]:.4f}")

print("\n-- weight sweep: argmax moves among non-denied candidates of judged listings --")
for ww in (0.1, 0.25, 0.5, 1.0, 1.49):
    c = collections.Counter(); wrongs = []
    for l, ds in by_listing.items():
        live = [d for d in ds if not d["denied"]]
        if len(live) < 2: continue
        a = max(live, key=lambda d: d["logit"]); b = max(live, key=lambda d: d["logit"] + ww * d["llr"])
        if a is b: continue
        c[(a['y'], b['y'])] += 1
        if a['y'] == 1: wrongs.append(f"{b['cc']}:{b['raw'][:28]}")
    print(f"  w={ww}: wrong→right {c[(0,1)]}, right→wrong {c[(1,0)]}, wrong→wrong {c[(0,0)]}  e.g. {wrongs[:4]}")
    if ww in (0.1,0.25):
        for l, ds in by_listing.items():
            live = [d for d in ds if not d["denied"]]
            if len(live) < 2: continue
            a = max(live, key=lambda d: d["logit"]); b = max(live, key=lambda d: d["logit"] + ww * d["llr"])
            if a is not b and (a['y'],b['y'])!=(0,0): print(f"     {a['cc']} {a['venue'][:26]} «{a['raw'][:34]}» {a['title'][:24]} ({a['year']}) p={a['p']:.2f} y={a['y']} → {b['title'][:24]} ({b['year']}) p={b['p']:.2f} y={b['y']}")
