import json, math, re, unicodedata, sys
from collections import defaultdict, Counter

def slugify(name):
    s = name.replace('ä','ae').replace('ö','oe').replace('ü','ue').replace('ß','ss')
    s = s.replace('Ä','Ae').replace('Ö','Oe').replace('Ü','Ue')
    s = unicodedata.normalize('NFKD', s)
    s = ''.join(c for c in s if not unicodedata.combining(c))
    s = s.lower()
    s = re.sub(r'[^a-z0-9]+', '-', s).strip('-')
    return s

def haversine_km(lat1, lon1, lat2, lon2):
    R = 6371.0
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dphi = math.radians(lat2 - lat1)
    dlambda = math.radians(lon2 - lon1)
    a = math.sin(dphi/2)**2 + math.cos(p1)*math.cos(p2)*math.sin(dlambda/2)**2
    return 2*R*math.asin(math.sqrt(a))

theaters = json.load(open('de-theaters.json'))
coords = json.load(open('de-city-coords.json'))
admin1_to_bundesland = json.load(open('admin1_to_bundesland.json'))

theaters = [t for t in theaters if t['city']]
assert len(theaters) == 1533, len(theaters)

# group cinemas + bundesland by city
city_cinemas = defaultdict(list)
city_bundesland = Counter()
for t in theaters:
    c = t['city']
    city_cinemas[c].append(t)
    if t['bundesland'] and t['bundesland'] != '?':
        city_bundesland[(c, t['bundesland'])] += 1

def bundesland_for_city(city):
    candidates = {bl: cnt for (c, bl), cnt in city_bundesland.items() if c == city}
    if candidates:
        return max(candidates.items(), key=lambda kv: kv[1])[0]
    # source data has no bundesland for this city ('?' on every cinema row) -
    # fall back to the GeoNames admin1 region of its matched coordinates
    info = coords.get(city)
    if info and info.get('admin1') in admin1_to_bundesland:
        return admin1_to_bundesland[info['admin1']]
    return '?'

cities = sorted(city_cinemas.keys())
for c in cities:
    if c not in coords:
        print("MISSING COORD:", c, file=sys.stderr)

# City record: name, count, population, lat, lon
CityInfo = {}
for c in cities:
    info = coords.get(c)
    CityInfo[c] = {
        'name': c,
        'count': len(city_cinemas[c]),
        'population': info['population'] if info else 0,
        'lat': info['lat'] if info else None,
        'lon': info['lon'] if info else None,
        'bundesland': bundesland_for_city(c),
    }

# cities with no coords: best-effort - assign to nearest hub by matching bundesland later,
# or standalone region with lat/lon = None -> we'll flag them
no_coord_cities = [c for c in cities if CityInfo[c]['lat'] is None]
print(f"Cities without coords: {len(no_coord_cities)}: {no_coord_cities}", file=sys.stderr)

RADIUS_KM = 35.0

def build_regions(radius_km):
    """Rank cities by cinema count desc (tiebreak population desc). Cities with
    count >= min_hub_count are candidate hubs; greedily assign hubs from the
    biggest down, then assign every remaining city to nearest qualifying hub
    within radius_km, else the city becomes its own standalone region."""
    ranked = sorted(cities, key=lambda c: (-CityInfo[c]['count'], -CityInfo[c]['population']))

    hubs = []  # list of city names, in selection order (biggest first)
    hub_set = set()
    assigned = {}  # city -> hub city name

    for c in ranked:
        if CityInfo[c]['lat'] is None:
            continue  # handle separately
        if not hubs:
            hubs.append(c); hub_set.add(c); assigned[c] = c
            continue
        # find nearest existing hub
        best_hub, best_dist = None, None
        for h in hubs:
            dist = haversine_km(CityInfo[c]['lat'], CityInfo[c]['lon'], CityInfo[h]['lat'], CityInfo[h]['lon'])
            if best_dist is None or dist < best_dist:
                best_dist = dist; best_hub = h
        if best_dist is not None and best_dist <= radius_km:
            assigned[c] = best_hub
        else:
            # becomes a new hub (own region) -- always true for the biggest
            # unassigned city not within radius of any existing hub
            hubs.append(c); hub_set.add(c); assigned[c] = c

    # cities with no coords -> standalone region on their own (best-effort, flagged)
    for c in no_coord_cities:
        hubs.append(c); assigned[c] = c

    return hubs, assigned

# iterate radius upward until region count <= 200
radius = RADIUS_KM
hubs, assigned = build_regions(radius)
while len(hubs) > 200:
    radius += 5
    hubs, assigned = build_regions(radius)

print(f"Final radius used: {radius} km -> {len(hubs)} regions", file=sys.stderr)

# Build region objects
regions_by_hub = defaultdict(list)  # hub -> list of member cities
for c, h in assigned.items():
    regions_by_hub[h].append(c)

regions = []
for hub, members in regions_by_hub.items():
    members_sorted = sorted(members, key=lambda c: (-CityInfo[c]['count'], -CityInfo[c]['population']))
    # region name = largest constituent city by cinemas, tiebreak population
    name_city = members_sorted[0]
    all_cinemas = []
    for m in members_sorted:
        for t in city_cinemas[m]:
            all_cinemas.append({'theaterId': t['theaterId'], 'name': t['name'], 'city': t['city']})
    region = {
        'slug': slugify(name_city),
        'name': name_city,
        'lat': CityInfo[hub]['lat'],
        'lon': CityInfo[hub]['lon'],
        'bundesland': CityInfo[hub]['bundesland'],
        'cities': members_sorted,
        'cinemas': all_cinemas,
    }
    regions.append(region)

regions.sort(key=lambda r: -len(r['cinemas']))

# dedupe slugs (in case two hubs collapse to same name/slug - shouldn't happen since hub names are distinct cities)
slug_counts = Counter(r['slug'] for r in regions)
dupes = {s: n for s, n in slug_counts.items() if n > 1}
if dupes:
    print("DUPLICATE SLUGS:", dupes, file=sys.stderr)
    seen = Counter()
    for r in regions:
        seen[r['slug']] += 1
        if slug_counts[r['slug']] > 1:
            r['slug'] = f"{r['slug']}-{seen[r['slug']]}"

total_cinemas = sum(len(r['cinemas']) for r in regions)
print(f"Total cinemas across regions: {total_cinemas}", file=sys.stderr)
assert total_cinemas == 1533, total_cinemas

single_city = sum(1 for r in regions if len(r['cities']) == 1)
merged = sum(1 for r in regions if len(r['cities']) > 1)
print(f"Single-city regions: {single_city}, merged regions: {merged}", file=sys.stderr)

json.dump(regions, open('de-regions.json', 'w', encoding='utf-8'), ensure_ascii=False, indent=1)
print(f"Wrote {len(regions)} regions to de-regions.json", file=sys.stderr)

print("\nTop 25 regions by cinema count:", file=sys.stderr)
for r in regions[:25]:
    print(f"  {r['name']:30s} slug={r['slug']:30s} cinemas={len(r['cinemas']):4d} cities={len(r['cities']):3d} bundesland={r['bundesland']}", file=sys.stderr)

if no_coord_cities:
    print("\nCities with NO geocode (best-effort standalone region, flag these):", file=sys.stderr)
    for c in no_coord_cities:
        print(f"  {c}: {len(city_cinemas[c])} cinemas", file=sys.stderr)
