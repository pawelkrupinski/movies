"""Booking/detail URL film-id coverage per country and host, from a recording's corpus.

usage: python3 -I slots.py <corpus dir> [cc...]
"""
import collections, gzip, json, re, sys, unicodedata, urllib.parse as up

corpus = sys.argv[1]
countries = sys.argv[2:] or ['pl', 'uk', 'de', 'es', 'us']

EXTERNAL = {
    'imdb': re.compile(r'\b(tt\d{7,9})\b'),
    'tmdb': re.compile(r'themoviedb\.org/movie/(\d+)|[?&]tmdb(?:_?id)?=(\d+)', re.I),
    'filmweb': re.compile(r'filmweb\.pl/(?:film|serial)/[^/?#]*-(\d+)', re.I),
    'eidr': re.compile(r'10\.5240/[0-9A-F-]{20,}', re.I),
}
VISTA = re.compile(r'^(HO\d{8})$', re.I)
SLUGNUM = re.compile(r'^(?:\d+-)?[a-z0-9%.-]*?[a-z][a-z0-9%.-]*-(\d{3,})$', re.I)
LEADNUM = re.compile(r'^(\d{3,})-[a-z]', re.I)
UUID = re.compile(r'^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$', re.I)
DIGITS = re.compile(r'^\d{3,}$')
ALNUM = re.compile(r'^(?=.*\d)[A-Z0-9]{8,}$')


def norm(t):
    t = unicodedata.normalize('NFD', t.lower())
    t = ''.join(c for c in t if not unicodedata.combining(c)).replace('ł', 'l')
    return re.sub(r'[^a-z0-9]', '', t)


def tokens(url, tag):
    """(slot name, value) for every id-shaped piece of a URL; slot names are host+position+shape."""
    out = []
    try:
        p = up.urlparse(url)
    except ValueError:
        return out
    host = p.netloc.lower()
    host = re.sub(r'^[^.]+\.(bilety24\.pl)$', r'*.\1', host)
    host = re.sub(r'^tickets\.ocine[a-z]*\.es$', 'tickets.ocine*.es', host)
    host = re.sub(r'^[a-z]+\.artesiete\.es$', '*.artesiete.es', host)
    parts = [x for x in p.path.split('/') if x]
    frag = [x for x in re.split(r'[/?&=#]', p.fragment) if x]
    for i, seg in enumerate(parts + ['#' + f for f in frag]):
        seg = up.unquote(seg).lstrip('#')
        for shape, rx in (('vista', VISTA), ('slugnum', SLUGNUM), ('leadnum', LEADNUM), ('uuid', UUID),
                          ('digits', DIGITS), ('alnum', ALNUM)):
            m = rx.match(seg)
            if m:
                out.append((f'{tag}:{host}:p{i}:{shape}', (m.group(1) if m.groups() else seg).upper()))
                break
    for k, vs in up.parse_qs(p.query).items():
        for v in vs:
            if VISTA.match(v) or DIGITS.match(v) or UUID.match(v) or ALNUM.match(v):
                out.append((f'{tag}:{host}:q:{k.lower()}', v.upper()))
    if tag == 'film' and parts:  # a detail page's slug names its film even without a number
        last = parts[-1].lower()
        if re.search('[a-z]', last) and not last.endswith(('.s', '.html', '.php')):
            out.append((f'film:{host}:slug', last))
    return out


def chain(venue):
    return venue.split(' ')[0] + (' ' + venue.split(' ')[1] if venue.startswith(('Cinema City', 'Kino ')) and len(venue.split(' ')) > 1 else '')


for cc in countries:
    venues = json.load(gzip.open(f'{corpus}/cinema-scrapes-{cc}.json.gz'))
    films = json.load(gzip.open(f'{corpus}/prod-films-{cc}.json.gz'))
    take = {}
    for f in films:
        for s in f.get('slots', []):
            v, k = s.split('␟', 1)
            take[(v, k)] = f.get('tmdbId')
    listings = []
    ext = collections.Counter(); ext_new = collections.Counter(); ext_examples = collections.defaultdict(list)
    for v in venues:
        for f in v['films']:
            urls = [s.get('bookingUrl') for s in f['showtimes'] if s.get('bookingUrl')]
            fu = f.get('filmUrl')
            blob = ' '.join(urls + ([fu] if fu else []))
            for kind, rx in EXTERNAL.items():
                m = rx.search(blob)
                if m:
                    ext[kind] += 1
                    have = {k.lower() for k in f.get('externalIds', {})}
                    if kind not in have:
                        ext_new[kind] += 1
                        if len(ext_examples[kind]) < 3:
                            ext_examples[kind].append((v['_id'], f['movie']['title'], m.group(0)))
            per_show = [dict(tokens(u, 'book')) for u in urls]
            slots = collections.defaultdict(set)
            for d in per_show:
                for k, val in d.items():
                    slots[k].add(val)
            if fu:
                for k, val in tokens(fu, 'film'):
                    slots[k].add(val)
            key = norm(f['movie']['title'])
            listings.append(dict(venue=v['_id'], title=f['movie']['title'], key=key, n=len(urls), slots=slots,
                                 tmdb=take.get((v['_id'], key), 'nojoin'), ext=f.get('externalIds', {})))
    n = len(listings)
    joined = sum(1 for l in listings if l['tmdb'] != 'nojoin')
    print(f'\n=== {cc.upper()}  listings {n}  joined-to-take {joined / n:.0%}  external ids in URLs: '
          f'{dict(ext)}  not yet in externalIds: {dict(ext_new)}')
    for kind, exs in ext_examples.items():
        print('   e.g.', kind, exs)
    # film-level slots: constant within a listing, different between a venue's listings
    by_slot = collections.defaultdict(list)
    for l in listings:
        for k, vals in l['slots'].items():
            by_slot[k].append(l)
    rows = []
    for k, ls in by_slot.items():
        if len(ls) < 40:
            continue
        multi = [l for l in ls if l['n'] >= 2 or k.startswith('film:')]
        const = sum(1 for l in multi if len(l['slots'][k]) == 1) / max(1, len(multi))
        if const < 0.9:
            continue
        per_venue = collections.defaultdict(list)
        for l in ls:
            per_venue[l['venue']].append(l)
        distinct = sum(len({next(iter(l['slots'][k])) for l in g if len(l['slots'][k]) == 1}) for g in per_venue.values()) / len(ls)
        if distinct < 0.8:
            continue
        # cross-venue: values on 2+ venues
        by_val = collections.defaultdict(list)
        for l in ls:
            if len(l['slots'][k]) == 1:
                by_val[next(iter(l['slots'][k]))].append(l)
        shared = [g for g in by_val.values() if len({l['venue'] for l in g}) >= 2]
        shared_listings = sum(len(g) for g in shared)
        title_agree = sum(1 for g in shared if len({l['key'] for l in g}) == 1)
        # takes: same id -> one film?
        ids_multi_film = 0; ids_fill = 0; fill_listings = 0; agree_take = 0
        for val, g in by_val.items():
            tk = {l['tmdb'] for l in g if l['tmdb'] not in ('nojoin', None)}
            if len(tk) >= 2:
                ids_multi_film += 1
            elif len(tk) == 1 and len(g) >= 2:
                agree_take += 1
                unmatched = [l for l in g if l['tmdb'] is None]
                if unmatched:
                    ids_fill += 1; fill_listings += len(unmatched)
        # a film (take) carried under 2+ ids of this slot within the chain
        by_film = collections.defaultdict(set)
        for val, g in by_val.items():
            for l in g:
                if l['tmdb'] not in ('nojoin', None):
                    by_film[l['tmdb']].add(val)
        film_split = sum(1 for s in by_film.values() if len(s) >= 2)
        captured = any(str(next(iter(ls[0]['slots'][k]))).lower() == str(x).lower() for x in ls[0]['ext'].values())
        rows.append((len(ls), k, const, distinct, len({l['venue'] for l in ls}), len(by_val), len(shared), shared_listings,
                     title_agree, agree_take, ids_multi_film, film_split, ids_fill, fill_listings, captured))
    rows.sort(reverse=True)
    print(f'  {"listings":>8} {"slot":<52} const dist venues ids shared(lst) titleAgree takeAgree idsMultiFilm filmsSplit fill(lst) captured')
    for r in rows[:25]:
        print(f'  {r[0]:>8} {r[1][:52]:<52} {r[2]:.2f} {r[3]:.2f} {r[4]:>5} {r[5]:>5} {r[6]:>5}({r[7]}) {r[8]:>5} {r[9]:>5} {r[10]:>5} {r[11]:>5} {r[12]:>4}({r[13]}) {r[14]}')
