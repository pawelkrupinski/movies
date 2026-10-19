import sys, json, gzip, collections
a, b = sys.argv[1], sys.argv[2]
sys.argv = [sys.argv[0], 'x']
exec(open(__file__.replace('stable.py', 'slots.py')).read().split('for cc in countries:')[0])
W = {'pl': ['film:helios.pl:p3:slugnum', 'book:www.multikino.pl:p3:vista', 'film:biletyna.pl:slug'],
     'de': ['book:www.cinemaxx.de:p3:vista', 'film:www.kinoheld.de:slug'],
     'us': ['book:ticketing.marcustheatres.com:q:featurecode', 'book:www.cinemark.com:q:m', 'book:harkins.com:p4:vista']}
def load(rec, cc):
    m = collections.defaultdict(dict)
    for v in json.load(gzip.open(f'{rec}/cinema-scrapes-{cc}.json.gz')):
        for f in v['films']:
            per = collections.defaultdict(set)
            for s in f['showtimes']:
                if s.get('bookingUrl'):
                    for k, x in tokens(s['bookingUrl'], 'book'): per[k].add(x)
            if f.get('filmUrl'):
                for k, x in tokens(f['filmUrl'], 'film'): per[k].add(x)
            for k in W[cc]:
                if len(per.get(k, ())) == 1: m[k][(v['_id'], norm(f['movie']['title']))] = next(iter(per[k]))
    return m
for cc in W:
    A, B = load(a, cc), load(b, cc)
    for k in W[cc]:
        common = set(A[k]) & set(B[k]); same = sum(1 for x in common if A[k][x] == B[k][x])
        # across venues within one recording: titles with 1 id vs several
        byt = collections.defaultdict(set)
        for (v, t), x in B[k].items(): byt[t].add(x)
        multi = sum(1 for s in byt.values() if len(s) > 1)
        print(f'{cc} {k:<48} listings in both {len(common):>5}  same id {same/max(1,len(common)):.1%}  titles {len(byt)} with >1 id across venues {multi}')
