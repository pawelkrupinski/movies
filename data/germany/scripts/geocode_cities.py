import json, re, unicodedata, time, sys
from collections import defaultdict

def strip_accents_lower(s):
    s = s.replace('ä','ae').replace('ö','oe').replace('ü','ue').replace('ß','ss')
    s = s.replace('Ä','Ae').replace('Ö','Oe').replace('Ü','Ue')
    s = unicodedata.normalize('NFKD', s)
    s = ''.join(c for c in s if not unicodedata.combining(c))
    return s.lower().strip()

def normalize_key(s):
    # aggressive normalization for matching: strip accents, lowercase, remove
    # parenthetical suffixes, "am X"/"an der X"/"i." suffixes, punctuation
    s = strip_accents_lower(s)
    s = re.sub(r'\(.*?\)', '', s)  # remove parentheticals
    s = re.sub(r'[^a-z0-9\s/-]', ' ', s)
    s = re.sub(r'\s+', ' ', s).strip()
    return s

SUFFIX_RE = re.compile(r'\b(am|an der|an|im|in|auf|unter|bei|ob der)\b.*$')

def base_name_candidates(city_name):
    """Yield progressively looser normalized keys to try against the index."""
    key = normalize_key(city_name)
    yield key
    # strip " - district" / "/district" suffixes (first token only)
    if '/' in key:
        yield normalize_key(key.split('/')[0])
    if ' - ' in city_name:
        yield normalize_key(city_name.split(' - ')[0])
    stripped = SUFFIX_RE.sub('', key).strip()
    if stripped and stripped != key:
        yield stripped
    # hyphen: try first component (e.g. "Schloss Holte-Stukenbrock" -> "Schloss Holte")
    if '-' in key:
        yield normalize_key(key.split('-')[0])
    # some GeoNames entries hyphenate compound town names that the source data
    # writes with a plain space (e.g. "Enkenbach Alsenborn" -> "Enkenbach-Alsenborn")
    if ' ' in key and '-' not in key:
        yield key.replace(' ', '-')

# ---- Load GeoNames DE.txt ----
GEONAMES_COLS = ['geonameid','name','asciiname','alternatenames','latitude','longitude',
                  'feature_class','feature_code','country_code','cc2','admin1','admin2',
                  'admin3','admin4','population','elevation','dem','timezone','moddate']

populated_codes = {'PPL','PPLA','PPLA2','PPLA3','PPLA4','PPLA5','PPLC','PPLF','PPLG',
                    'PPLL','PPLR','PPLS','PPLX','PPLQ'}
# administrative-municipality fallback: some real towns (Weinstadt, Argenbuehl,
# Feldberger Seenlandschaft) only exist in GeoNames as ADM4 admin boundaries,
# not as P-class populated places. Index them too but rank below P-class.
admin_codes = {'ADM4', 'ADM3'}

print("Loading GeoNames DE.txt...", file=sys.stderr)
name_index = defaultdict(list)  # normalized name -> list of (population, lat, lon, canonical_name, feature_code, is_admin)

with open('geonames/DE.txt', encoding='utf-8') as f:
    for line in f:
        parts = line.rstrip('\n').split('\t')
        if len(parts) < 15:
            continue
        row = dict(zip(GEONAMES_COLS, parts))
        is_admin = False
        if row['feature_class'] == 'P' and row['feature_code'] in populated_codes:
            pass
        elif row['feature_class'] == 'A' and row['feature_code'] in admin_codes:
            is_admin = True
        else:
            continue
        try:
            pop = int(row['population']) if row['population'] else 0
        except ValueError:
            pop = 0
        lat = float(row['latitude'])
        lon = float(row['longitude'])
        names = set()
        names.add(row['name'])
        names.add(row['asciiname'])
        if row['alternatenames']:
            for alt in row['alternatenames'].split(','):
                # skip alternate names that look like language-tagged codes (rare in this dump) - keep as is
                names.add(alt)
        entry = (pop, lat, lon, row['name'], row['feature_code'], is_admin, row['admin1'])
        for n in names:
            if not n:
                continue
            key = normalize_key(n)
            if key:
                name_index[key].append(entry)

print(f"Indexed {len(name_index)} normalized name keys", file=sys.stderr)

def best_geonames_match(city_name):
    # try progressively looser candidate keys; stop at first that yields a hit
    for key in base_name_candidates(city_name):
        candidates = name_index.get(key)
        if candidates:
            break
    else:
        return None
    # prefer non-admin (real populated place) over admin boundary; then highest population
    code_rank = {'PPLC':5,'PPLA':4,'PPLA2':3,'PPLA3':2,'PPLA4':1}
    def score(e):
        pop, lat, lon, name, code, is_admin, admin1 = e
        return (0 if is_admin else 1, pop, code_rank.get(code, 0))
    best = max(candidates, key=score)
    return best

if __name__ == '__main__':
    data = json.load(open('de-theaters.json'))
    cities = sorted(set(d['city'] for d in data if d['city']))
    print(f"Distinct cities: {len(cities)}", file=sys.stderr)

    results = {}
    unmatched = []
    for c in cities:
        m = best_geonames_match(c)
        if m:
            pop, lat, lon, name, code, is_admin, admin1 = m
            results[c] = {'lat': lat, 'lon': lon, 'population': pop, 'source': 'geonames-adm' if is_admin else 'geonames', 'matched_name': name, 'feature_code': code, 'admin1': admin1}
        else:
            unmatched.append(c)

    print(f"GeoNames matched: {len(results)}/{len(cities)}", file=sys.stderr)
    print(f"Unmatched (need Nominatim): {len(unmatched)}", file=sys.stderr)
    for u in unmatched:
        print("  UNMATCHED:", u, file=sys.stderr)

    json.dump({'matched': results, 'unmatched': unmatched}, open('geocode_stage1.json', 'w', encoding='utf-8'), ensure_ascii=False, indent=1)
