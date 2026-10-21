"""Offline prototype: billing-kind classifier, hard plausibility constraints, cross-venue/catalogue agreement,
review-queue concentration -- measured on the five prod-mirror exports against identity-judgements.tsv + review answers.
python3 -I analyse.py <repo> <datadir>"""
import json, re, sys, collections, os, datetime

REPO, DATA = sys.argv[1], sys.argv[2]
CCS = ["pl", "de", "uk", "us", "es"]
norm = lambda s: re.sub(r"\s+", " ", s or "").strip(" ")

# ---------- labels ----------
J = collections.defaultdict(lambda: {"right": set(), "wrong": set(), "event": False, "user": False})
for line in open(os.path.join(REPO, "test/resources/fixtures/identity-judgements.tsv")):
    if line.startswith("#") or line.startswith("country\t"):
        continue
    c = line.rstrip("\n").split("\t")
    if len(c) < 5 or c[4] not in ("right", "wrong"):
        continue
    k = (c[0], c[1], norm(c[2]))
    J[k][c[4]].add(c[3])
    if (c[10] if len(c) > 10 else "").startswith(("review page:", "hand label:")):
        J[k]["user"] = True
for l in open(os.path.join(DATA, "reviews.jsonl")):
    a = json.loads(l)
    for v, t in a["members"]:
        k = (a["cc"], v, norm(t))
        if a["verdict"] in ("event", "bill", "none"):
            J[k]["event"] = True
        elif a["verdict"] in ("right", "film") and (a["ref"] if a["verdict"] == "film" else a["shown"]):
            J[k]["right"].add(a["ref"] if a["verdict"] == "film" else a["shown"])
        elif a["verdict"] == "wrong" and a["shown"]:
            J[k]["wrong"].add(a["shown"])
        J[k]["user"] = True


def label(cc, r):
    a = J.get((cc, r["v"], norm(r["rt"])))
    b = J.get((cc, "*", norm(r["rt"])))
    if a and b:  # venue overrides *, per film
        m = {"right": set(b["right"]) - a["wrong"] | a["right"], "wrong": set(b["wrong"]) - a["right"] | a["wrong"],
             "event": a["event"] or b["event"], "user": a["user"] or b["user"]}
        return m
    return a or b


def has(r, ref):
    k, v = ref.split(":", 1)
    if k == "tmdb": return str(r["tmdb"]) == v
    if k == "imdb": return r["imdb"] == v
    if k == "filmweb": return bool(r["fw"]) and v in r["fw"]
    return None


def verdict(cc, r):
    """right / wrong / lost / event_ok / event_served / None"""
    L = label(cc, r)
    if not L: return None
    served = bool(r["tmdb"] or r["imdb"])
    rights = [x for x in L["right"] if x.split(":")[0] in ("tmdb", "imdb", "filmweb")]
    if any(has(r, x) for x in rights): return "right"
    if served and any(has(r, x) for x in L["wrong"]): return "wrong"
    if L["event"] and not rights: return "event_served" if served else "event_ok"
    if rights and not served: return "lost"
    if rights and served: return "other"   # another film than the right one (incl. alt-id artefacts)
    if not served and L["wrong"]: return "none_ok"
    return None


# ---------- the billing parse (prototype) ----------
W = lambda *ws: re.compile(r"(?i)(?<![\w])(?:" + "|".join(ws) + r")(?![\w])")
RELAY = W(r"met\s*opera", r"the\s+metropolitan\s+opera", r"metropolitan\s+opera", r"live\s+from\s+the\s+met", r"\(met\)", r"met:",
          r"nt\s+live", r"national\s+theatre\s+live", r"royal\s+opera(\s+house)?", r"royal\s+ballet", r"rbo", r"roh",
          r"bolshoi", r"teatr\s+bolszoj", r"op[ée]ra\s+national\s+de\s+paris", r"opera\s+de\s+paris", r"ballet\s+de\s+l'op[ée]ra",
          r"exhibition\s+on\s+screen", r"great\s+art\s+on\s+screen", r"wielka\s+sztuka", r"art\s+on\s+screen",
          r"retransmisj\w*", r"transmisj\w*", r"live\s+viewing", r"encore", r"opera\s+live", r"ballet\s+live", r"ópera", r"opera",
          r"ballett?", r"balet\w*", r"andr[ée]\s+rieu\w*", r"hauser", r"live\s+in\s+cinema\w*", r"in\s+concert", r"konzertfilm",
          r"world\s+tour", r"live\s+aus", r"kinokonzert", r"the\s+musical", r"musical", r"pathé\s+live")
LIVE = W(r"league\s+of\s+legends", r"worlds\s+\d{2,4}", r"finals?\s+in\s+cinema", r"jeopardy", r"quiz\w*", r"karaoke", r"sing[- ]?a[- ]?long",
         r"panto\w*", r"pantomime", r"lesung", r"vortrag", r"live[- ]reportage", r"multivision", r"wyk[łl]ad\w*", r"spotkanie", r"prelekcj\w*",
         r"lecture", r"talk", r"in\s+conversation", r"comedy", r"stand[- ]?up", r"kabarett", r"kabaret\w*", r"konzert\w*", r"koncert\w*",
         r"concert", r"party", r"disco", r"gala", r"workshop", r"warsztat\w*", r"kneipe", r"stammtisch", r"podcast", r"live\s+show",
         r"brunch", r"dinner", r"quizabend", r"vernissage", r"lesebühne", r"slam", r"impro\w*", r"theater", r"teatr\w*", r"spektakl\w*",
         r"theatre", r"a\s+play", r"performance", r"live\s+music", r"cabaret", r"gig", r"esports?", r"gaming")
MYSTERY = W(r"secret\s+screen\w*", r"mystery\s+(screen\w*|voyage|transmission|movie|film)", r"screen\s+unseen", r"sneak(\s+preview)?",
            r"[üu]berraschungs\w*", r"film\s+niespodzian\w*", r"seans\s+niespodzian\w*", r"niespodzianka", r"surprise\s+(screen\w*|film|movie)",
            r"secret\s+(movie|film|cinema)", r"preview\s+screening", r"blind\s+date", r"überraschungsfilm", r"sneak\s*peek", r"mystery")
MULTI = W(r"marat[oh]n\w*", r"minimaraton", r"double\s+(feature|bill)", r"triple\s*(feature|bill)?", r"doppel(vorstellung|feature|pack)",
          r"zestaw\s*\d*", r"trilog\w*", r"all[- ]nighter", r"night\s+of", r"nacht\s+der", r"noc\s+\w+", r"kurzfilm\w*", r"short\s+films?",
          r"shorts", r"kr[óo]tk\w*\s+metra\w*", r"krótkometra\w*", r"programm\b", r"blok", r"block", r"hexenkessel", r"kurzfilmprogramm",
          r"film\s+programme", r"compilation", r"anthology", r"double", r"triple", r"back\s+to\s+back")
FEST = re.compile(r"(?i)^(.*?\b(festival|festiwal|filmfest\w*|film\s*fest|filmtage|filmwoche|days|dni|tage)\b[^:|–-]*)(?:\s*[:|–-]\s*(.*))?$")
VERSION = W(r"extended(\s+(cut|edition|version))?", r"director'?s\s+cut", r"directors\s+cut", r"remastered", r"restored", r"4k", r"re[- ]?release",
            r"\d+(th|\.)?\s+(anniversary|jubil\w*|jahre)", r"anniversary", r"jubiläum\w*", r"rocznica", r"wersja\s+\w+", r"fassung", r"uncut",
            r"ov", r"omu", r"omeu", r"omdu", r"napisy", r"dubbing", r"subtitled", r"2d", r"3d", r"imax", r"4dx", r"screenx", r"dolby\s*\w*",
            r"\(\d+a?\)", r"\(u\)", r"\(pg\)", r"\(12a?\)", r"\(15\)", r"\(18\)", r"pyjama\s+party", r"relaxed", r"autism\s+friendly",
            r"parent\s+and\s+baby", r"carers?\s+and\s+babies", r"subtitled", r"hoh", r"\(ad\)")
SEP = re.compile(r"\s+(?:\+|&|//|/|and|und|i|oraz)\s+", re.I)


def parse(title):
    """returns (kind, banner, work) -- the structured reading the resolver would get once per listing"""
    t = norm(title)
    kinds = []
    if MYSTERY.search(t): kinds.append("mystery")
    if MULTI.search(t): kinds.append("multi")
    if RELAY.search(t): kinds.append("relay")
    if LIVE.search(t): kinds.append("live")
    banner, work = None, t
    m = re.match(r"^([^:|]{2,60}?)\s*[:|]\s+(.+)$", t)
    if m:
        banner, work = m.group(1), m.group(2)
    fm = FEST.match(t)
    if fm and (not fm.group(3) or len(fm.group(3).split()) == 0):
        kinds.append("programme")
    kind = kinds[0] if kinds else "film"
    return kind, banner, work, kinds


# ---------- run ----------
def load(cc):
    rows = [json.loads(l) for l in open(os.path.join(DATA, f"ex_{cc}.jsonl"))]
    for r in rows:
        r["rt"] = r.get("rt") or r.get("ti") or ""
        for k in ("tmdb", "imdb", "fw", "f", "li", "basis"): r.setdefault(k, None)
    return rows


out = {}
for cc in CCS:
    rows = load(cc)
    V = collections.Counter()
    for r in rows:
        r["verdict"] = verdict(cc, r)
        V[r["verdict"]] += 1
        r["kind"], r["banner"], r["work"], r["kinds"] = parse(r["rt"])
    out[cc] = rows

print("== judged-listing outcome (current prod served film) ==")
for cc in CCS:
    print(cc, len(out[cc]), dict(collections.Counter(r["verdict"] for r in out[cc] if r["verdict"])))

print("\n== billing-kind classifier vs labels ==")
NOREC = {"mystery", "multi", "live", "programme"}
for cc in CCS:
    rows = out[cc]
    c = collections.Counter()
    ex = collections.defaultdict(collections.Counter)
    for r in rows:
        v = r["verdict"]; k = r["kind"]
        served = bool(r["tmdb"])
        c[(k, "served" if served else "none")] += 1
        if v:
            c[("J", k, v)] += 1
            if k in NOREC and v == "right": ex["norec_right"][r["rt"]] += 1
            if k in NOREC and v in ("wrong", "event_served"): ex["norec_catch"][r["rt"]] += 1
            if k == "film" and v in ("wrong", "event_served"): ex["film_wrong"][r["rt"]] += 1
            if k in NOREC and v == "lost": ex["norec_lost"][r["rt"]] += 1
        if k in NOREC and served and not v: ex["norec_served_unjudged"][r["rt"] + " -> " + str((r["f"] or {}).get("t"))] += 1
        if k == "film" and not served: ex["film_none"][r["rt"]] += 1
    print(cc, {f"{k}/{s}": n for (k, *rest), n in [(x, y) for x, y in c.items() if x[0] != "J"] for s in [rest[0]]})
    print("   judged:", {f"{x[1]}:{x[2]}": n for x, n in c.items() if x[0] == "J"})
    for key in ("norec_right", "norec_catch", "film_wrong", "norec_lost", "norec_served_unjudged", "film_none"):
        tot = sum(ex[key].values())
        print(f"   {key} {tot} listings / {len(ex[key])} titles:", "; ".join(f"{t[:60]}×{n}" for t, n in ex[key].most_common(12 if key != 'film_none' else 25)))
json.dump({cc: [{k: r[k] for k in ("v", "rt", "tmdb", "basis", "kind", "kinds", "verdict", "n")} for r in out[cc]] for cc in CCS},
          open(os.path.join(DATA, "kinds.json"), "w"))

# ---------- hard plausibility constraints on served takes ----------
print("\n== plausibility constraints on served takes (judged right / wrong / unjudged violating) ==")


def first_release(f):
    ds = []
    if f.get("rel"):
        for m in re.finditer(r"([A-Z]{2})(\d{4})(?:-(\d{2})-(\d{2}))?", f["rel"]):
            pass
    if f.get("rd"):
        try:
            ds.append(datetime.date.fromisoformat(f["rd"][:10]) if len(f["rd"]) >= 10 else datetime.date(int(f["rd"][:4]), 1, 1))
        except Exception:
            pass
    return min(ds) if ds else None


for cc in CCS:
    C = collections.Counter(); EX = collections.defaultdict(list)
    for r in out[cc]:
        f = r["f"]
        if not (r["tmdb"] and f): continue
        v = r["verdict"] or "unjudged"
        li = r["li"] or {}
        checks = {}
        if li.get("yr") and f.get("y"):
            checks["year>=3"] = abs(li["yr"] - f["y"]) >= 3
        if li.get("rt") and f.get("rt"):
            d = abs(li["rt"] - f["rt"])
            checks["runtime>30%&>20m"] = d > 20 and d > 0.3 * f["rt"]
        fr = first_release(f)
        if fr:
            last = datetime.datetime.utcfromtimestamp(r["d1"] / 1000).date()
            checks["released>90d after last show"] = (fr - last).days > 90
        if f.get("y") and li.get("yub"):
            checks["after upper bound+1"] = f["y"] > li["yub"] + 1
        for k, bad in checks.items():
            C[(k, v, bad)] += 1
            if bad and v in ("right", "wrong"): EX[(k, v)].append(f"{r['rt'][:40]}→{f.get('t')} {f.get('y')}")
    for k in ("year>=3", "runtime>30%&>20m", "released>90d after last show", "after upper bound+1"):
        print(f"  {cc} {k}: right {C[(k,'right',True)]}/{C[(k,'right',True)]+C[(k,'right',False)]}, "
              f"wrong {C[(k,'wrong',True)]}/{C[(k,'wrong',True)]+C[(k,'wrong',False)]}, "
              f"unjudged {C[(k,'unjudged',True)]}/{C[(k,'unjudged',True)]+C[(k,'unjudged',False)]}")
        for vv in ("right", "wrong"):
            if EX[(k, vv)]:
                print(f"      {vv}: " + "; ".join(sorted(collections.Counter(EX[(k, vv)]))[:8]))

# ---------- catalogue ids and cross-venue agreement ----------
print("\n== catalogue-id agreement ==")
for cc in CCS:
    by = collections.defaultdict(list)
    for r in out[cc]:
        for src, i in ((r["li"] or {}).get("ex") or {}).items():
            by[(src, str(i))].append(r)
    fill = collections.Counter(); split = collections.Counter(); fillv = collections.Counter(); splitv = collections.Counter(); exf = collections.Counter(); exs = collections.Counter()
    for key, rs in by.items():
        films = collections.Counter(r["tmdb"] for r in rs if r["tmdb"])
        if not films: continue
        top, n = films.most_common(1)[0]
        for r in rs:
            if not r["tmdb"] and n >= 2 and len(films) == 1:
                fill[key[0]] += 1; fillv[r["verdict"]] += 1; exf[f"{r['rt'][:40]} ⇒ {top}"] += 1
            elif r["tmdb"] and len(films) > 1:
                split[key[0]] += 1; splitv[r["verdict"]] += 1; exs[f"{key[0]}:{r['rt'][:30]}→{r['tmdb']}"] += 1
    print(f"  {cc} untaken listing whose catalogue id ≥2 other listings take as ONE film: {sum(fill.values())} {dict(fill)} verdicts {dict(fillv)}")
    print("     e.g. " + "; ".join(f"{t}×{n}" for t, n in exf.most_common(10)))
    print(f"  {cc} taken listing whose catalogue id other listings take as ANOTHER film: {sum(split.values())} {dict(split)} verdicts {dict(splitv)}")
    print("     e.g. " + "; ".join(f"{t}×{n}" for t, n in exs.most_common(8)))

print("\n== cross-venue: same billing key, different served films ==")
key = lambda t: re.sub(r"[^\w]+", " ", (t or "").lower()).strip()
for cc in CCS:
    g = collections.defaultdict(list)
    for r in out[cc]: g[key(r["rt"])].append(r)
    multi = {k: rs for k, rs in g.items() if len({r["v"] for r in rs}) >= 2}
    dis = {k: rs for k, rs in multi.items() if len({r["tmdb"] for r in rs if r["tmdb"]}) >= 2}
    mixed = {k: rs for k, rs in multi.items() if any(r["tmdb"] for r in rs) and any(not r["tmdb"] for r in rs)}
    wr = [r for r in out[cc] if r["verdict"] in ("wrong", "event_served")]
    single = sum(1 for r in wr if len({x["v"] for x in g[key(r["rt"])]}) == 1)
    lostmixed = sum(1 for k, rs in mixed.items() for r in rs if r["verdict"] == "lost")
    print(f"  {cc}: billing keys at ≥2 venues {len(multi)}; with ≥2 films served {len(dis)} ({sum(len(v) for v in dis.values())} listings); "
          f"half-taken {len(mixed)} ({sum(1 for v in mixed.values() for r in v if not r['tmdb'])} untaken listings, {lostmixed} judged lost); "
          f"wrong-served listings {len(wr)}, of them billed at one venue only {single}")

# ---------- review-queue concentration ----------
print("\n== review queue: uncertain, unjudged billings ranked by listings ==")
for cc in CCS:
    g = collections.Counter()
    for r in out[cc]:
        if r["verdict"]: continue
        unc = (not r["tmdb"] and r["kind"] == "film" and r["basis"] in ("BelowThreshold", "Vetoed")) or \
              (r["tmdb"] and r["basis"] in ("PooledMatch",))
        if unc: g[key(r["rt"])] += 1
    tot = sum(g.values())
    top = sum(n for _, n in g.most_common(50))
    print(f"  {cc}: uncertain unjudged listings {tot} over {len(g)} billings; top 50 billings cover {top} ({100*top/max(tot,1):.0f}%)")
