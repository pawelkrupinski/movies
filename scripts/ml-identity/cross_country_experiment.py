"""Cross-country agreement, OFFLINE experiment (2026-10-19, branch experiment-cross-country; docs/identity-experiments.md).

Would one country's fact-backed take settle a bare-title listing of the same original title in another country?

Data: the LOCAL prod mirror (read-only, after scripts/local-mirror/freshness), exported once per country to <data dir>
by scripts/ml-identity/cross_country_export.js (full, not LIGHT: the rule reads candidates); read by
scripts/ci/cross_country.py, which documents the files.
Run: python3 -I scripts/ml-identity/cross_country_experiment.py <repo root> <data dir>
"""
import collections
import os
import sys

ROOT, DATA = sys.argv[1], sys.argv[2]
sys.path.insert(0, os.path.join(ROOT, "scripts/ci"))
import cross_country as X  # noqa: E402  (the one reading of the countries' takes side by side)
import identity_judgements as J  # noqa: E402

COUNTRIES = ["pl", "uk", "de", "es", "us"]
title_key, overlap = X.title_key, X.overlap

rows = J.read(os.path.join(ROOT, "test/resources/fixtures/identity-judgements.tsv"))
labels = J.labels_by_country(rows)
films = X.read_films(DATA, COUNTRIES)
listings = X.read_listings(DATA, COUNTRIES, films)  # one per trace, with the facts its billing states


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
index = X.index_by_title(listings)
groups = sum(1 for ls in index.values() if len({l["cc"] for l in ls}) >= 2 and X.same_week_across_border(ls))
disagree_keys = [k for k, _ in X.disagreeing(index)]
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
