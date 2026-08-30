#!/usr/bin/env python3
"""Generate common/src/main/scala/models/UsRosterData.scala from the harvested
flicks.us venue dataset.

The addressable place is the METRO, not the state: a visitor wants "films in Los
Angeles", never "films in California". So each venue carries the metro its
coordinates put it in (cluster_metros.py, labelled by metros.py) plus that
metro's own centroid, and `UsRoster` turns each metro into a `City` of its own.

The STATE survives as the grouping the picker reads — "California → Los
Angeles" is how a visitor finds a metro — and as the `City` itself for the nine
states and territories too small to have metros (Alaska, Hawaii, DC, …), whose
listing IS their page.

Usage:  python3 data/us/scripts/generate_roster.py <venues.json> <out.scala>
"""
import json, re, sys, unicodedata
from collections import defaultdict
sys.path.insert(0, __file__.rsplit('/', 1)[0])
from states import STATES
from metros import labels_by_slug
from cluster_metros import metros_for_state, sub_areas_for_metro


def scala_str(s: str) -> str:
    """Escape a Python string for a Scala double-quoted literal."""
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('$', '$$')


def clean(s):
    return re.sub(r'\s+', ' ', (s or '')).strip()


def main(src, out):
    venues = json.load(open(src))
    by_state = defaultdict(list)
    skipped_no_state, skipped_no_coords, skipped_unknown_state = [], [], []

    for v in venues:
        state, slug, title = v.get('state'), v.get('slug'), clean(v.get('title'))
        if not state:
            skipped_no_state.append(slug); continue
        if state not in STATES:
            skipped_unknown_state.append((slug, state)); continue
        try:
            lat, lon = float(v['lat']), float(v['lon'])
        except (TypeError, ValueError, KeyError):
            skipped_no_coords.append(slug); continue
        if not (slug and title):
            continue
        by_state[state].append({
            'slug': slug, 'title': title, 'city': clean(v.get('city')),
            'lat': lat, 'lon': lon, 'metro': v.get('region_slug') or '',
        })

    # Display names are the WIRE KEY every per-cinema slot is stored under
    # (Source.byDisplayName is a plain toMap), so a duplicate silently rebinds one
    # venue's showtimes to another. Qualify collisions with the town, then the
    # state, and fail loudly if that still is not enough.
    seen = defaultdict(list)
    for state, vs in by_state.items():
        for v in vs:
            seen[v['title']].append((state, v))
    for title, entries in seen.items():
        if len(entries) == 1:
            continue
        for state, v in entries:
            v['title'] = f"{title} ({v['city']})" if v['city'] else f"{title} ({state})"
    final = defaultdict(list)
    for state, vs in by_state.items():
        for v in vs:
            final[v['title']].append((state, v))
    for title, entries in final.items():
        if len(entries) > 1:
            for state, v in entries:
                v['title'] = f"{title} ({state})"
    still = defaultdict(int)
    for state, vs in by_state.items():
        for v in vs:
            still[v['title']] += 1
    dupes = {t: n for t, n in still.items() if n > 1}
    if dupes:
        raise SystemExit(f"UNRESOLVED display-name collisions (wire keys): {dupes}")

    # The metro each venue is filed under is DISTANCE-CLUSTERED, not its raw
    # Flicks `region_slug`: adjacent metros inside one travel-shed merge, and the
    # 788 venues recovered from their own pages (no `region_slug` at all) join
    # the metro nearest them instead of a per-state catch-all. Flicks' metros
    # survive as the LABELS the clusters are named after.
    metro_labels = labels_by_slug({state: {v['metro'] for v in vs if v['metro']}
                                   for state, vs in by_state.items()})
    metro_of = {state: metros_for_state(state, vs, metro_labels[state])
                for state, vs in by_state.items()}

    # A metro too big to browse as one list is clustered AGAIN, at a twelfth of
    # the radius, into the districts a local names — `UsMetroSubAreas` groups by
    # this second label. Empty for every venue in a metro under
    # `MIN_VENUES_TO_SUBDIVIDE`, which is all but five of the 470.
    by_metro = defaultdict(list)
    for state, vs in by_state.items():
        for v in vs:
            by_metro[(state, metro_of[state][v['slug']])].append(v)
    sub_of = {}
    for (state, metro), members in by_metro.items():
        sub_of.update(sub_areas_for_metro(metro, members))

    def centroid(vs):
        """Centroid of a group's actual venues — centres the map where the
        cinemas are rather than on a geographic midpoint nobody goes to."""
        return (round(sum(v['lat'] for v in vs) / len(vs), 5),
                round(sum(v['lon'] for v in vs) / len(vs), 5))

    regions = []
    for state in sorted(by_state, key=lambda s: STATES[s][0]):
        vs = sorted(by_state[state], key=lambda v: v['title'].lower())
        slug, zone = STATES[state]
        lat, lon = centroid(vs)
        # A metro is a PLACE of its own — `/los-angeles/`, a `City` — so it needs
        # its own coordinates for the landing's nearest-place geolocation. The
        # state's centroid is no answer for one: every metro in California would
        # sit on the same point, 300 km from most of them.
        grouped = defaultdict(list)
        for v in vs:
            grouped[metro_of[state][v['slug']]].append(v)
        metros = sorted((label, *centroid(members)) for label, members in grouped.items())
        regions.append((slug, state, lat, lon, zone, vs, metros))

    total = sum(len(r[5]) for r in regions)
    lines = [
        "// GENERATED from data/us/venues.json by data/us/scripts/generate_roster.py",
        "// — do NOT edit by hand. Full US cinema roster: "
        f"{len(regions)} states/territories / {total:,} cinemas (Flicks, www.flicks.us).",
        "// Regenerate after re-harvesting; see data/us/README.md.",
        "package models",
        "",
        "private[models] object UsRosterData {",
        "  // (displayName, pillName, flicks cinema slug, metro label, district label)",
        "  type C = (String, String, String, String, String)",
        "  // (metro label, lat, lon) — the centroid of that metro's own venues",
        "  type M = (String, Double, Double)",
        "  // (slug, name, lat, lon, zoneId, cinemas, metros)",
        "  type R = (String, String, Double, Double, String, Seq[C], Seq[M])",
        "",
    ]
    for slug, state, lat, lon, zone, vs, metros in regions:
        ident = 'r_' + slug.replace('-', '_')
        lines.append(f'  private def {ident}: R = ("{slug}", "{scala_str(state)}", '
                     f'{lat}, {lon}, "{zone}", Seq(')
        for v in vs:
            t = scala_str(v['title'])
            lines.append(f'    ("{t}", "{t}", "{scala_str(v["slug"])}", '
                         f'"{scala_str(metro_of[state][v["slug"]])}", '
                         f'"{scala_str(sub_of.get(v["slug"], ""))}"),')
        lines.append('  ), Seq(')
        for label, mlat, mlon in metros:
            lines.append(f'    ("{scala_str(label)}", {mlat}, {mlon}),')
        lines.append('  ))')
        lines.append('')
    lines.append('  val regions: Seq[R] = Seq(')
    for slug, *_ in regions:
        lines.append(f'    r_{slug.replace("-", "_")},')
    lines.append('  )')
    lines.append('}')
    open(out, 'w').write('\n'.join(lines) + '\n')

    print(f"regions={len(regions)} cinemas={total}")
    print(f"skipped: no state={len(skipped_no_state)} unknown state={len(skipped_unknown_state)} "
          f"no coords={len(skipped_no_coords)}")
    if skipped_unknown_state:
        print("  unknown states:", sorted({s for _, s in skipped_unknown_state}))
    qualified = sum(1 for _, vs in by_state.items() for v in vs if v['title'].endswith(')'))
    print(f"display names qualified to break collisions: ~{qualified}")
    raw = {v['metro'] for vs in by_state.values() for v in vs if v['metro']}
    clustered = {(state, label) for state, m in metro_of.items() for label in m.values()}
    print(f"metros: {len(raw)} raw Flicks -> {len(clustered)} clustered "
          f"(python3 data/us/scripts/cluster_metros.py reports the distribution)")
    sub_metros = {m for (_, m), vs in by_metro.items() if any(v['slug'] in sub_of for v in vs)}
    print(f"sub-divided metros: {sorted(sub_metros)} -> "
          f"{len(set(sub_of.values()))} sub-areas over {len(sub_of)} venues")


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
