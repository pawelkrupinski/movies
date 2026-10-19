"""Venue-KIND profile, offline (experiment venue-kinds). Run from the data dir: python3 -I analyse.py <repo root>"""
import json, os, re, sys, collections, unicodedata
ROOT = sys.argv[1]
sys.path.insert(0, os.path.join(ROOT, "scripts/ci"))
import identity_judgements as J

CCS = ["pl", "uk", "de", "us", "es"]
# --- 1. the classifiers' reading of each billing (scripts/title-trace --batch) ---
flags = {}
cur = None
import itertools
for line in itertools.chain(*(open(f, encoding="utf-8", errors="replace") for f in ["all.out", "rest.out"] if os.path.exists(f))):
    if line.startswith("### "):
        parts = line[4:].rstrip("\n").split("\t")
        cur = tuple(parts) if len(parts) == 3 else None
        if cur: flags[cur] = set()
    elif cur and "NonMovieEventClassifier:" in line:
        for name, key in [("mystery screening", "mystery"), ("non-screening", "nonscreening"), ("live event", "live"), ("screened broadcast", "broadcast")]:
            if re.search(re.escape(name) + r"[^,]* YES", line): flags[cur].add(key)
    elif cur and "ListingShape:" in line:
        for key in ["billsSeveral", "stagesAWork", "billsAHouse", "relays"]:
            if f"{key} YES" in line: flags[cur].add(key)
        if "programme slot none" not in line: flags[cur].add("programme")
print(f"classified billings {len(flags)}")

WHOLE = {"live", "tour", "show", "band", "gala", "slam", "quiz", "night", "nacht", "stage", "disco", "open", "experience", "abend", "noc", "wieczor"}
STEMS = ["konzert", "concert", "koncert", "festival", "festiwal", "party", "comedy", "musical", "tribute", "karaoke", "lesung", "poetry",
         "signierstunde", "kabarett", "kabaret", "cabaret", "soulnight", "tournee", "stand-up", "standup"]
def fold(s): return "".join(c for c in unicodedata.normalize("NFD", s.lower()) if unicodedata.category(c) != "Mn").replace("ł", "l")
def words(s): return re.findall(r"[\w-]+", fold(s))
def event_words(raw): return [w for w in words(raw) if w in WHOLE or any(st in w for st in STEMS)]

RELAY = {"broadcast", "billsAHouse"}
EVENT = {"nonscreening", "live"} | ({"eventword"} if os.environ.get("EVENTWORDS") else set())
def kinds(cc, v, r):
    f = set(flags.get((cc, v, r), ()))
    if event_words(r): f.add("eventword")
    return f

# --- 2. data ---
rows = J.read(os.path.join(ROOT, "test/resources/fixtures/identity-judgements.tsv"))
labels = J.labels_by_country(rows)
stage_names = set()
for fn in ["identity-stage-works.tsv", "identity-stage-works-extra.tsv"]:
    for l in open(os.path.join(ROOT, "identity/src/main/resources", fn), encoding="utf-8"):
        if l.startswith("#"): continue
        cols = l.rstrip("\n").split("\t")
        for n in "|".join(cols[1:]).split("|"):
            k = " ".join(words(n))
            if len(k) > 3: stage_names.add(k)

out = {}
for cc in CCS:
    traces = [json.loads(l) for l in open(f"t-{cc}.jsonl")]
    facts = {}
    for l in open(f"facts-{cc}.jsonl"):
        d = json.loads(l); facts[(d["v"], d["r"])] = d
    films = {}
    for l in open(f"films-{cc}.jsonl"):
        d = json.loads(l)
        try: films[int(d["id"])] = d
        except (ValueError, TypeError): pass
    # venue → normalised title → kind set (union over spellings)
    venue = collections.defaultdict(dict)
    for t in traces:
        k = kinds(cc, t["v"], t["r"])
        venue[t["v"]].setdefault(J.normalise(t["r"]).lower(), set()).update(k)
    out[cc] = (traces, facts, films, venue)

def profile(venue, v, raw):
    d = venue[v]; own = J.normalise(raw).lower()
    others = [k for t, k in d.items() if t != own]
    n = len(others)
    rel = sum(1 for k in others if k & RELAY); ev = sum(1 for k in others if k & EVENT)
    nf = sum(1 for k in others if k & (RELAY | EVENT))
    return n, rel / n if n else 0.0, ev / n if n else 0.0, nf / n if n else 0.0

# --- 3. coverage ---
print("\n== coverage: venues with >=10 other titles; share of titles relay / event / non-film")
for cc in CCS:
    traces, facts, films, venue = out[cc]
    big = {v: d for v, d in venue.items() if len(d) >= 10}
    def share(d, s): return sum(1 for k in d.values() if k & s) / len(d)
    r10 = sum(1 for d in big.values() if share(d, RELAY) >= .10); r20 = sum(1 for d in big.values() if share(d, RELAY) >= .20)
    e10 = sum(1 for d in big.values() if share(d, EVENT) >= .10); e20 = sum(1 for d in big.values() if share(d, EVENT) >= .20)
    n20 = sum(1 for d in big.values() if share(d, RELAY | EVENT) >= .20)
    tot = sum(len(d) for d in venue.values()); trel = sum(1 for d in venue.values() for k in d.values() if k & RELAY)
    tev = sum(1 for d in venue.values() for k in d.values() if k & EVENT)
    print(f"{cc}: venues {len(venue)} (>=10 titles {len(big)}); titles {tot}, relay {trel} ({trel/tot:.1%}), event {tev} ({tev/tot:.1%}); "
          f"relay-heavy >=10% {r10}, >=20% {r20}; event-heavy >=10% {e10}, >=20% {e20}; non-film >=20% {n20}")

# --- 4. (B) separation on judged current takes ---
def auc(pos, neg):
    if not pos or not neg: return float("nan")
    s = 0.0
    for p in pos:
        for q in neg: s += 1 if p > q else .5 if p == q else 0
    return s / (len(pos) * len(neg))

def ordinary_feature(rec):
    if rec is None: return True
    t = fold(rec.get("t") or "")
    if any(w in t for w in ["live", "national theatre", "opera", "ballet", "concert", "tour", "royal", "nt "]): return False
    rt = rec.get("rt")
    return rt is None or rt >= 60

def own_fact(f): return bool(f and (f.get("y") or f.get("rt") or f.get("d")))

def piece_only(raw, rec):
    """The billing says words beyond every title of the record (the record named by a piece)."""
    if rec is None or not rec.get("t"): return False
    bw = [w for w in words(raw) if not w.isdigit() and len(w) > 2]
    rw = set(words(rec["t"]))
    extra = [w for w in bw if w not in rw]
    return len(extra) >= 2 and any(w in rw for w in bw)

def names_stage_work(raw):
    ws = words(raw); k = " ".join(ws)
    return k in stage_names or any(" ".join(ws[i:j]) in stage_names for i in range(len(ws)) for j in range(i + 2, len(ws) + 1))

judged = []
for cc in CCS:
    traces, facts, films, venue = out[cc]
    lab = labels.get(cc, {})
    for t in traces:
        if t["f"] is None: continue
        verdict = J.label_of(lab, t["v"], t["r"], f"tmdb:{t['f']}")
        if verdict not in ("right", "wrong"): continue
        n, rs, es, ns = profile(venue, t["v"], t["r"])
        own = kinds(cc, t["v"], t["r"])
        rec = films.get(t["f"])
        judged.append(dict(cc=cc, v=t["v"], r=t["r"], f=t["f"], b=t["b"], y=verdict == "right", n=n, rs=rs, es=es, ns=ns, own=own,
                           fact=own_fact(facts.get((t["v"], t["r"]))), feature=ordinary_feature(rec), piece=piece_only(t["r"], rec),
                           stage=names_stage_work(t["r"]), rec=(rec or {}).get("t"), ry=(rec or {}).get("y")))
print(f"\n== (B) judged current takes: {len(judged)} (right {sum(j['y'] for j in judged)}, wrong {sum(not j['y'] for j in judged)})")
for name, key in [("relay share", "rs"), ("event share", "es"), ("non-film share", "ns")]:
    pos = [j[key] for j in judged if not j["y"]]; neg = [j[key] for j in judged if j["y"]]
    print(f"  {name:15s} AUC(wrong over right) {auc(pos, neg):.3f}   mean wrong {sum(pos)/len(pos):.3f}  right {sum(neg)/len(neg):.3f}")
sub = [j for j in judged if j["n"] >= 10]
for name, key in [("non-film share", "ns")]:
    for cut in [.05, .10, .20, .30]:
        hi = [j for j in sub if j[key] >= cut]
        print(f"  venues >=10 titles, {name} >= {cut:.2f}: takes {len(hi)}, wrong {sum(not j['y'] for j in hi)} ({sum(not j['y'] for j in hi)/max(1,len(hi)):.0%})"
              f" vs below {sum(not j['y'] for j in sub if j[key] < cut)}/{sum(1 for j in sub if j[key] < cut)}")
looks = [j for j in judged if j["own"] & (RELAY | EVENT) or j["piece"] or j["stage"]]
print(f"  takes whose billing looks event/relay-ish (own kind, piece-only or stage work): {len(looks)}, wrong {sum(not j['y'] for j in looks)}")
pos = [j["ns"] for j in looks if not j["y"]]; neg = [j["ns"] for j in looks if j["y"]]
print(f"  …non-film share AUC among them {auc(pos, neg):.3f}")

# --- 5. proxy vetoes on every current take (judged or not) ---
print("\n== proxy veto: a take of an ordinary feature, no own fact, at a venue (>=N other titles) whose non-film share >= S")
VARIANTS = {
    "A own-kind": lambda j: bool(j["own"] & (RELAY | EVENT)),
    "B weak-cue": lambda j: bool(j["own"] & (RELAY | EVENT)) or j["piece"] or j["stage"],
    "C piece/stage only": lambda j: j["piece"] or j["stage"],
}
alltakes = []
for cc in CCS:
    traces, facts, films, venue = out[cc]
    lab = labels.get(cc, {})
    for t in traces:
        if t["f"] is None: continue
        n, rs, es, ns = profile(venue, t["v"], t["r"])
        rec = films.get(t["f"])
        alltakes.append(dict(cc=cc, v=t["v"], r=t["r"], f=t["f"], b=t["b"], n=n, ns=ns, own=kinds(cc, t["v"], t["r"]), fact=own_fact(facts.get((t["v"], t["r"]))),
                             feature=ordinary_feature(rec), piece=piece_only(t["r"], rec), stage=names_stage_work(t["r"]), rec=(rec or {}).get("t"), ry=(rec or {}).get("y"),
                             verdict=J.label_of(lab, t["v"], t["r"], f"tmdb:{t['f']}")))
examples = collections.defaultdict(list)
for name, cue in VARIANTS.items():
    for S in [0.0, .05, .10, .20, .30]:
        hit = [j for j in alltakes if (S == 0 or j["n"] >= 10) and j["ns"] >= S and j["feature"] and not j["fact"] and cue(j)]
        c = collections.Counter(j["verdict"] for j in hit)
        print(f"  {name:18s} S>={S:.2f}: vetoed {len(hit):5d}  wrong {c['wrong']:3d}  right {c['right']:3d}  unjudged {c[None]:5d}")
        if S == .20: examples[name] = hit
for name, hit in examples.items():
    print(f"\n-- {name} S>=0.20 judged:")
    for j in [j for j in hit if j["verdict"]][:25]:
        print(f"   {j['verdict']:5s} {j['cc']} {j['v'][:28]:28s} «{j['r'][:50]}» → {j['rec']} ({j['ry']}) ns={j['ns']:.2f} own={sorted(j['own'])} piece={j['piece']} stage={j['stage']}")
    print(f"   unjudged sample:")
    for j in [j for j in hit if not j["verdict"]][:15]:
        print(f"   ?     {j['cc']} {j['v'][:28]:28s} «{j['r'][:50]}» → {j['rec']} ({j['ry']}) ns={j['ns']:.2f} own={sorted(j['own'])}")
json.dump([{k: (sorted(v) if isinstance(v, set) else v) for k, v in j.items()} for j in alltakes], open("alltakes.json", "w"))
