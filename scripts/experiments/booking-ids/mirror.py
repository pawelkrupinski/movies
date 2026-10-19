"""Chain/external film ids in booking + detail URLs, against prod's current takes (mirror export) and the judgements.
usage: python3 -I mirror.py <scratch dir> <judgements.tsv> [cc...]"""
import sys, json, collections, csv
src = open(__file__.replace('mirror.py', 'slots.py')).read().split('for cc in countries:')[0]
sys.argv = [sys.argv[0], 'x'] + sys.argv[1:]
exec(src)
scratch, judg = sys.argv[2], sys.argv[3]
ccs = sys.argv[4:] or ['pl', 'uk', 'de', 'es', 'us']
J = collections.defaultdict(dict)  # (cc, venue, norm raw) -> {film: verdict}
for row in csv.reader((l for l in open(judg) if not l.startswith('#')), delimiter='\t'):
    if len(row) > 4 and row[0] != 'country':
        J[(row[0], row[1], norm(row[2]))][row[3]] = row[4] + ('/' + row[5] if row[5] not in ('open', '') else '')
def verdict(cc, venue, raw, film):
    f = f'tmdb:{film}' if film else None
    for v in (venue, '*'):
        d = J.get((cc, v, norm(raw)), {})
        if f in d: return d[f]
    return 'unjudged'
for cc in ccs:
    L, T = [], {}
    for line in open(f'{scratch}/mirror-{cc}.jsonl'):
        r = json.loads(line)
        if r['t'] == 'L': L.append(r)
        else: T[(r['venue'], r['raw'])] = r['film']
    n = len(L); withurl = sum(1 for l in L if l['urls']); withfilm = sum(1 for l in L if l['filmUrl'])
    traced = sum(1 for l in L if (l['venue'], l['title']) in T)
    ext = collections.Counter(); extnew = []
    slots = collections.defaultdict(lambda: collections.defaultdict(list))
    for l in L:
        blob = ' '.join(l['urls'] + ([l['filmUrl']] if l['filmUrl'] else []))
        for kind, rx in EXTERNAL.items():
            m = rx.search(blob)
            if m:
                ext[kind] += 1
                if kind not in l['ext']: extnew.append((kind, l['venue'], l['title'], m.group(0), T.get((l['venue'], l['title']), 'untraced')))
        per = collections.defaultdict(set)
        for u in l['urls']:
            for k, v in tokens(u, 'book'): per[k].add(v)
        if l['filmUrl']:
            for k, v in tokens(l['filmUrl'], 'film'): per[k].add(v)
        for k, vs in per.items():
            if len(vs) == 1: slots[k][next(iter(vs))].append(l)
    print(f'\n=== {cc.upper()} listings {n}  bookingUrl {withurl/n:.0%}  filmUrl {withfilm/n:.0%}  traced {traced/n:.0%}  '
          f'external ids in URLs {dict(ext)}, not in externalIds {len(extnew)}')
    for e in extnew[:12]: print('    ext', e, verdict(cc, e[1], e[2], e[4]) if e[4] not in ('untraced',) else '')
    tot = collections.Counter()
    for k, groups in sorted(slots.items(), key=lambda kv: -sum(len(g) for g in kv[1].values())):
        ls = [l for g in groups.values() for l in g]
        venues = {l['venue'] for l in ls}
        if len(ls) < 40 or len(venues) < 2: continue
        # film-level: a venue's listings carry distinct values
        pv = collections.defaultdict(set)
        for v, g in groups.items():
            for l in g: pv[l['venue']].add(v)
        if sum(len(s) for s in pv.values()) / len(ls) < 0.8: continue
        captured = sum(1 for l in ls if any(str(x).upper() == v for x in l['ext'].values()) for v in [next(val for val, g in groups.items() if l in g)]) if False else None
        shared = {v: g for v, g in groups.items() if len({l['venue'] for l in g}) >= 2}
        retitled = {v: g for v, g in shared.items() if len({norm(l['title']) for l in g}) > 1}
        multi, fill, fill_l = [], [], 0
        for v, g in shared.items():
            films = collections.Counter(T.get((l['venue'], l['title']), 'untraced') for l in g)
            real = {f for f in films if f not in (None, 'untraced')}
            if len(real) > 1: multi.append((v, g))
            elif len(real) == 1 and films.get(None):
                fill.append((v, g)); fill_l += films[None]
        # already captured? value equals one of the listing's externalIds
        cap = sum(1 for v, g in groups.items() for l in g if v in {str(x).upper() for x in l['ext'].values()}) / len(ls)
        print(f'  {k[:50]:<50} lst {len(ls):>6} ven {len(venues):>4} ids {len(groups):>5} shared {len(shared):>4} '
              f'retitled {len(retitled):>3} multiFilm {len(multi):>3} fill {len(fill):>3}({fill_l}) captured {cap:.0%}')
        if cap < 0.5:
            tot['slots'] += 1; tot['listings'] += len(ls); tot['retitled'] += len(retitled); tot['multi'] += len(multi); tot['fill'] += fill_l
            for v, g in multi[:6] + fill[:4]:
                c = collections.Counter((l['title'][:50], l['venue'] if len(g) < 4 else '', T.get((l['venue'], l['title']), 'untraced')) for l in g)
                print('      ', v, [(t, vn, f, verdict(cc, l0['venue'], t, f) if f not in (None,'untraced') else '-', n) for (t, vn, f), n in c.most_common(4) for l0 in [next(l for l in g if l['title'][:50] == t)]])
    print('  UNCAPTURED totals', dict(tot))
