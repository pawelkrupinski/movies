"""Cross-country agreement, OFFLINE experiment (2026-10-19, branch experiment-cross-country; docs/identity-experiments.md).

Would one country's fact-backed take settle a bare-title listing of the same original title in another country?

Data: the LOCAL prod mirror (read-only, after scripts/local-mirror/freshness), exported once per country to <data dir>
by scripts/ml-identity/cross_country_export.js:
  traces-<cc>.jsonl    db.identity_traces   -> {v, r, f (taken TMDB id), b (basis), ru (rules), e (evidence), c (candidates)}
  listings-<cc>.jsonl  db.identity_listings -> {v, t, o (originalTitle), y (releaseYear), rt, d (director), s0, s1, n}
  films-<cc>.jsonl     db.tmdb_films        -> {id, t, o, y, rt, d}
Run: python3 -I scripts/ml-identity/cross_country_experiment.py <repo root> <data dir>
"""
import collections
import json
import os
import re
import sys
import unicodedata

ROOT, DATA = sys.argv[1], sys.argv[2]
sys.path.insert(0, os.path.join(ROOT, "scripts/ci"))
import identity_judgements as J  # noqa: E402

COUNTRIES = ["pl", "uk", "de", "es", "us"]
DAY = 86400000
SLACK = 3 * DAY  # "the same week": screening windows overlapping within three days
CAND = re.compile(r"^(\d+) ([\d.]+)% rank (\S+) (.*?)'(.*)'(?: \((\d{4})\))?\s*$")
YEAR_IN_TITLE = re.compile(r"\((1[89]\d\d|20\d\d)\)")


def fold(s):
    s = unicodedata.normalize("NFD", s or "").replace("ł", "l").replace("Ł", "L")
    return "".join(ch for ch in s if not unicodedata.combining(ch)).lower()


def title_key(s):
    """Normalised original title: brackets, format tokens, punctuation and a leading article dropped."""
    s = fold(s)
    s = re.sub(r"\([^)]*\)|\[[^\]]*\]", " ", s)
    s = re.sub(r"\b(omu|omeu|ov|ome|2d|3d|imax|4dx|dolby|atmos|vose|vo|napisy|dubbing)\b", " ", s)
    s = s.replace("&", " and ")
    s = re.sub(r"[^\w]+", " ", s).strip()
    s = re.sub(r"^(the|a|an|der|die|das|el|la|los|las|le|les) ", "", s)
    return s


def person(s):
    return re.sub(r"[^a-z]", "", fold(s))


rows = J.read(os.path.join(ROOT, "test/resources/fixtures/identity-judgements.tsv"))
labels = J.labels_by_country(rows)

films = {}
for cc in COUNTRIES:
    for line in open(os.path.join(DATA, f"films-{cc}.jsonl")):
        f = json.loads(line)
        try:
            fid = int(f["id"])
        except (TypeError, ValueError):
            continue
        y = f.get("y")
        films[fid] = dict(t=f.get("t"), o=f.get("o"), y=int(y) if y and str(y).isdigit() else None,
                          d={person(x) for x in (f.get("d") or [])} - {""})


def cands(trace):
    out = {}
    for c in trace.get("c") or []:
        m = CAND.match(c)
        if m:
            out[int(m.group(1))] = dict(p=float(m.group(2)) / 100, denied="DENIED" in m.group(4))
    return out


listings = []  # one per trace, with the facts its billing states
for cc in COUNTRIES:
    facts = collections.defaultdict(list)
    for line in open(os.path.join(DATA, f"listings-{cc}.jsonl")):
        l = json.loads(line)
        facts[(l["v"], l["t"])].append(l)
    for line in open(os.path.join(DATA, f"traces-{cc}.jsonl")):
        t = json.loads(line)
        fs = facts.get((t["v"], t["r"]), [])
        own_year = next((f["y"] for f in fs if f.get("y")), None)
        m = YEAR_IN_TITLE.search(t["r"] or "")
        if own_year is None and m:
            own_year = int(m.group(1))
        directors = {person(d) for f in fs for d in f.get("d") or []} - {""}
        runtime = next((f["rt"] for f in fs if f.get("rt")), None)
        original = next((f["o"] for f in fs if f.get("o")), None)
        s0 = min((f["s0"] for f in fs if f.get("s0")), default=None)
        s1 = max((f["s1"] for f in fs if f.get("s1")), default=None)
        film = t.get("f")
        rec = films.get(film) if film else None
        backed = None  # does a fact of the listing's own agree with the taken film's record?
        if rec:
            if directors and rec["d"] and directors & rec["d"]:
                backed = "director"
            elif own_year and rec["y"] and abs(own_year - rec["y"]) <= 1:
                backed = "year"
        listings.append(dict(cc=cc, v=t["v"], r=t["r"], f=film, b=t.get("b"), year=own_year, dirs=directors,
                             rt=runtime, o=original, s0=s0, s1=s1, backed=backed,
                             bare=own_year is None and not directors, strict_bare=own_year is None and not directors and not runtime,
                             keys={k for k in {title_key(original), title_key(t["r"])} if k}, cands=cands(t)))


def verdict(l, film):
    if film is None:
        return None
    return J.label_of(labels.get(l["cc"], {}), l["v"], l["r"], f"tmdb:{film}")


print("== listings per country (traces joined to identity_listings)")
for cc in COUNTRIES:
    ls = [l for l in listings if l["cc"] == cc]
    taken = [l for l in ls if l["f"]]
    print(f"{cc}: listings {len(ls)}, taken {len(taken)}, bare (no year/director) {sum(l['bare'] for l in ls)}, "
          f"strict bare (no runtime either) {sum(l['strict_bare'] for l in ls)}, take fact-backed {sum(1 for l in taken if l['backed'])}, "
          f"own original title {sum(1 for l in ls if l['o'])}, window known {sum(1 for l in ls if l['s0'])}")

# ---- 2. coverage
by_film = collections.defaultdict(lambda: collections.defaultdict(list))
for l in listings:
    if l["f"]:
        by_film[l["f"]][l["cc"]].append(l)
multi = {f: d for f, d in by_film.items() if len(d) >= 2}
print(f"\n== coverage: films taken {len(by_film)}, in >=2 countries {len(multi)}, >=3 {sum(len(d) >= 3 for d in multi.values())}")
print("   per country pair:", dict(collections.Counter(
    "-".join(sorted(p)) for d in multi.values() for p in [(a, b) for a in d for b in d if a < b])))
bare_taken = [l for l in listings if l["f"] and l["bare"]]
bare_backed_elsewhere = [l for l in bare_taken if any(m["backed"] for c, ms in by_film[l["f"]].items() if c != l["cc"] for m in ms)]
print(f"   bare listings with a take: {len(bare_taken)}; their film fact-backed in another country: {len(bare_backed_elsewhere)} "
      f"({len({(l['cc'], l['f']) for l in bare_backed_elsewhere})} country-films)")
for cc in COUNTRIES:
    a = sum(1 for l in bare_taken if l["cc"] == cc)
    b = sum(1 for l in bare_backed_elsewhere if l["cc"] == cc)
    print(f"   {cc}: bare taken {a}, film backed elsewhere {b}")
vb = collections.Counter(verdict(l, l["f"]) for l in bare_backed_elsewhere)
print("   judged verdicts of those bare takes:", dict(vb))

# ---- 3. disagreement
index = collections.defaultdict(list)
for l in listings:
    for k in l["keys"]:
        index[k].append(l)


def overlap(a, b):
    if a["s0"] is None or b["s0"] is None:
        return False
    return a["s0"] <= b["s1"] + SLACK and b["s0"] <= a["s1"] + SLACK


groups = 0
disagree_keys = []
pairs = collections.Counter()
for k, ls in index.items():
    ccs = {l["cc"] for l in ls}
    if len(ccs) < 2:
        continue
    by_cc = collections.defaultdict(set)
    linked = [l for l in ls if any(m["cc"] != l["cc"] and overlap(l, m) for m in ls)]
    if not linked:
        continue
    groups += 1
    for l in linked:
        by_cc[l["cc"]].add(l["f"])
    taken_sets = {cc: fs - {None} for cc, fs in by_cc.items()}
    films_all = set().union(*taken_sets.values())
    if len(films_all) >= 2 and sum(1 for fs in taken_sets.values() if fs) >= 2:
        disagree_keys.append(k)
print(f"\n== disagreement: title keys billed in >=2 countries in the same week: {groups}; "
      f"countries take different films: {len(disagree_keys)}")

# judge each cross-country pair of different takes on each side, in its own country
side = collections.Counter()
pair_examples = []
seen = set()
for k in disagree_keys:
    ls = index[k]
    for a in ls:
        for b in ls:
            if a["cc"] >= b["cc"] or not a["f"] or not b["f"] or a["f"] == b["f"] or not overlap(a, b):
                continue
            key = (a["cc"], a["v"], a["r"], b["cc"], b["f"])
            if key in seen:
                continue
            seen.add(key)
            va, vb2 = verdict(a, a["f"]), verdict(b, b["f"])
            side[(va or "unjudged", vb2 or "unjudged")] += 1
            if va and vb2 and va != vb2:
                pair_examples.append((k, a, b, va, vb2))
print("   pairs (verdict A, verdict B), each listing side judged in its own country:")
for (x, y), n in sorted(side.items(), key=lambda kv: -kv[1]):
    print(f"     {x:9s} {y:9s} {n}")
print("   one right, one wrong (the pool's potential cure or risk):", len(pair_examples))
for k, a, b, va, vb2 in pair_examples[:12]:
    print(f"     «{k}»  {a['cc']} {a['v']} «{a['r']}» tmdb:{a['f']} {va} [{a['backed'] or 'bare' if a['bare'] else a['backed'] or 'facts'}]"
          f"  |  {b['cc']} {b['v']} «{b['r']}» tmdb:{b['f']} {vb2} [{b['backed'] or ('bare' if b['bare'] else 'facts')}]")

# ---- 4. the rule
#  A bare listing whose take disagrees with a fact-backed take of the same title in another country, in the same week,
#  adopts it or is denied by it. Donors are looked up by the listing's raw-title key first, and only when that finds
#  none by its own original-title key (a translated billing). Variants:
#    ADOPT       take the donor's film;
#    ADOPT-CAND  take it only when it is among the listing's own non-denied candidates, else drop the take (none);
#    DENY        only drop a take the donor disagrees with (never fills an untaken listing);
#    *-DIR       the same, donors backed by a shared DIRECTOR only (a year agreeing alone is not enough).
#    *-RAW       the same, donors found by the raw-title key only (no original-title tier: a bill or a copied original).
VARIANTS = ("ADOPT", "ADOPT-CAND", "DENY", "ADOPT-DIR", "ADOPT-CAND-DIR", "ADOPT-DIR-RAW", "DENY-DIR-RAW")
outcomes = {v: collections.Counter() for v in VARIANTS}
examples = {v: collections.defaultdict(list) for v in VARIANTS}
fired = collections.Counter()
ambiguous = collections.Counter()


def donors_of(l, director_only, raw_only=False):
    found = {}
    for keys in ([title_key(l["r"])], [title_key(l["o"])] if l["o"] and not raw_only else []):
        for k in keys:
            for m in index.get(k, []):
                if m["cc"] != l["cc"] and m["f"] and m["backed"] and (m["backed"] == "director" or not director_only) and overlap(l, m):
                    found.setdefault(m["f"], []).append(m)
        if found:
            return found
    return found


def bucket(old, new, ov, nv):
    kind = "fill" if old is None else ("drop" if new is None else "switch")
    if ov == "wrong":
        return kind, ("cure" if nv == "right" or new is None else f"wrong->{nv or 'unjudged'}")
    if ov == "right":
        return kind, "right lost"
    if nv == "wrong":
        return kind, "new wrong"
    if nv == "right":
        return kind, "gained right"
    return kind, "unjudged moved"


for l in listings:
    if not l["bare"]:
        continue
    for variant in VARIANTS:
        donors = donors_of(l, "-DIR" in variant, variant.endswith("-RAW"))
        if not donors or set(donors) == {l["f"]}:
            continue
        if len(donors) > 1:
            ambiguous[variant] += 1
            continue
        donor = next(iter(donors))
        if variant.startswith("DENY") and l["f"] is None:
            continue
        fired[variant] += 1
        if variant.startswith("ADOPT-CAND"):
            c = l["cands"].get(donor)
            new = donor if c and not c["denied"] else None
        elif variant.startswith("DENY"):
            new = None
        else:
            new = donor
        if new == l["f"]:
            continue
        ov, nv = verdict(l, l["f"]), verdict(l, new)
        kind, b = bucket(l["f"], new, ov, nv)
        outcomes[variant][(b, kind)] += 1
        examples[variant][b].append((l, new, donors[donor][0], ov, nv))
print("\n== rule (bare listing adopts / is denied by another country's fact-backed take, same week)")
for variant in VARIANTS:
    print(f"   {variant:15s} fired {fired[variant]}, skipped (donors disagree) {ambiguous[variant]}")
    agg = collections.Counter()
    for (b, kind), n in outcomes[variant].items():
        agg[b] += n
    for b, n in sorted(agg.items(), key=lambda kv: -kv[1]):
        split = {k: m for (bb, k), m in outcomes[variant].items() if bb == b}
        print(f"       {b:16s} {n:5d}  {split}")
for variant in VARIANTS:
    for b, exs in examples[variant].items():
        if b == "unjudged moved":
            continue
        for l, new, m, ov, nv in exs[:5]:
            print(f"     {variant} {b}: {l['cc']} {l['v']} «{l['r']}» tmdb:{l['f']} ({ov}) -> {new} ({nv})"
                  f"  donor {m['cc']} {m['v']} «{m['r']}» [{m['backed']}]")
for variant in ("ADOPT", "ADOPT-DIR"):
    unjudged = examples[variant]["unjudged moved"]
    print(f"\n   {variant} unjudged moved, by country:", dict(collections.Counter(l['cc'] for l, *_ in unjudged)),
          "distinct (country, title):", len({(l['cc'], title_key(l['r'])) for l, *_ in unjudged}))
    seen_t = set()
    for l, new, m, ov, nv in sorted(unjudged, key=lambda e: e[0]["r"]):
        if (l["cc"], title_key(l["r"]), new) in seen_t:
            continue
        seen_t.add((l["cc"], title_key(l["r"]), new))
        n = sum(1 for e in unjudged if e[0]["cc"] == l["cc"] and title_key(e[0]["r"]) == title_key(l["r"]) and e[1] == new)
        old_rec, new_rec = films.get(l["f"]) or {}, films.get(new) or {}
        print(f"     x{n:<3d} {l['cc']} «{l['r']}» tmdb:{l['f']} {old_rec.get('t')} {old_rec.get('y')} -> tmdb:{new} "
              f"{new_rec.get('t')} {new_rec.get('y')}  donor {m['cc']} «{m['r']}» [{m['backed']}]")

# ---- the ceiling: every judged-wrong current take, and whether any other country holds a fact-backed take of its title
wrong = [l for l in listings if l["f"] and verdict(l, l["f"]) == "wrong"]
reach = collections.Counter()
for l in wrong:
    d = donors_of(l, False)
    other = {f for f in d if f != l["f"]}
    reach[("bare" if l["bare"] else "has facts", "donor disagrees" if other else ("donor agrees" if d else "no donor"))] += 1
print(f"\n== ceiling: judged-wrong current takes {len(wrong)}:", dict(reach))
for l in wrong:
    d = donors_of(l, False)
    if any(f != l["f"] for f in d):
        print(f"     {l['cc']} {l['v']} «{l['r']}» tmdb:{l['f']} bare={l['bare']} donors={ {f: [verdict(l, f)] + [m['cc'] for m in ms][:3] for f, ms in d.items()} }")
