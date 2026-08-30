#!/usr/bin/env python3
"""Generate common/src/main/scala/models/UsRosterData.scala from the harvested
flicks.us venue dataset.

Grouping: ONE region per US state/territory (55), not one per Flicks metro (577).
The metros are far too many for a city picker — the roster caps the dropdown at
~200 (Germany ships 158) — and a state is the unit a US visitor actually
recognises. Metro granularity is not lost: each venue carries the metro its
coordinates put it in (cluster_metros.py, labelled by metros.py), which
`UsRoster` turns into the CinemaAreaGroups a big state's picker is grouped by,
the way London's are.

Usage:  python3 data/us/scripts/generate_roster.py <venues.json> <out.scala>
"""
import json, re, sys, unicodedata
from collections import defaultdict
sys.path.insert(0, __file__.rsplit('/', 1)[0])
from states import STATES
from metros import labels_by_slug
from cluster_metros import metros_for_state


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

    regions = []
    for state in sorted(by_state, key=lambda s: STATES[s][0]):
        vs = sorted(by_state[state], key=lambda v: v['title'].lower())
        slug, zone = STATES[state]
        # Centroid of the state's actual venues — centres the map where the
        # cinemas are rather than on a geographic midpoint nobody goes to.
        lat = round(sum(v['lat'] for v in vs) / len(vs), 5)
        lon = round(sum(v['lon'] for v in vs) / len(vs), 5)
        regions.append((slug, state, lat, lon, zone, vs))

    total = sum(len(r[5]) for r in regions)
    lines = [
        "// GENERATED from data/us/venues.json by data/us/scripts/generate_roster.py",
        "// — do NOT edit by hand. Full US cinema roster: "
        f"{len(regions)} states/territories / {total:,} cinemas (Flicks, www.flicks.us).",
        "// Regenerate after re-harvesting; see data/us/README.md.",
        "package models",
        "",
        "private[models] object UsRosterData {",
        "  // (displayName, pillName, flicks cinema slug, metro label, lat, lon)",
        "  type C = (String, String, String, String, Double, Double)",
        "  // (slug, name, lat, lon, zoneId, cinemas)",
        "  type R = (String, String, Double, Double, String, Seq[C])",
        "",
    ]
    for slug, state, lat, lon, zone, vs in regions:
        ident = 'r_' + slug.replace('-', '_')
        lines.append(f'  private def {ident}: R = ("{slug}", "{scala_str(state)}", '
                     f'{lat}, {lon}, "{zone}", Seq(')
        for v in vs:
            t = scala_str(v['title'])
            # Coordinates ride along per venue: `UsMetroSubAreas` splits a metro
            # too big to browse into compass sub-areas from them, exactly as the
            # metros themselves are clustered from them. 5 decimals is ~1 m,
            # far past what a compass bearing can notice, and keeps the literal
            # short.
            lines.append(f'    ("{t}", "{t}", "{scala_str(v["slug"])}", '
                         f'"{scala_str(metro_of[state][v["slug"]])}", '
                         f'{round(v["lat"], 5)}, {round(v["lon"], 5)}),')
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


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
