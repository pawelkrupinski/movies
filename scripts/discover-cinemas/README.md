# UK cinema discovery

Weekly job that finds **new** UK cinemas on [flicks.co.uk](https://www.flicks.co.uk)
and opens a PR wiring them into the catalog. Run by
[`.github/workflows/discover-cinemas.yml`](../../.github/workflows/discover-cinemas.yml)
every Monday 06:00 UTC (and on `workflow_dispatch`).

## How it works

Every UK venue is declared once, as a row of [`data/uk/roster.json`](../../data/uk/roster.json):
its Scala name, display and pill names, flicks.co.uk slug and primary source (none: Flicks).
`data/uk/scripts/generate_roster.py` generates the Scala from it — `UkRosterData` (the venues)
and `UkSourcesData` (their scrapers and Flicks fallbacks).

Which page LISTS a venue is not wired by hand: it is generated from the venue's
town (`data/uk/scripts/build_pages.py` → `UkPages.scala`), which the workflow
reruns after harvesting and geocoding the new venues.

`discover.py`:

1. Fetches Flicks' `sitemap-cinemas.xml` — every cinema slug Flicks knows.
2. Diffs it against the slugs we already wire, minus [`exclude.txt`](exclude.txt).
3. For each genuinely new slug, fetches the cinema page for its display name + coordinates and probes the sessions endpoint (today … +6d) for whether it currently lists showtimes.
4. Appends each venue's row to the roster group of the **nearest already-wired venue** by great-circle distance (coordinates in `data/uk/venues.json`), scraped off Flicks.
5. Reports venues wired by us but **dropped from Flicks' sitemap index**, split by session-liveness: a slug that also has **no sessions** this week is a genuine **retirement candidate**; one that **still serves sessions** is a benign Flicks index gap (the sitemap-cinemas.xml index omits some live venues — e.g. Vue Colchester, the Isle of Man cinemas — while their per-venue sessions endpoint keeps working), reported as "no action" and left wired. It never auto-deletes.

### Parking (never write a row the generator would refuse)

A venue is **parked** (reported, not wired) when it can't be placed safely:

- nearest venue is on the **London** page — London needs a manual area assignment in `models.LondonAreas`;
- **object-name or display-name collision** with an existing cinema (the display name is the key its slots are stored under);
- **no coordinates** on the page — city can't be resolved;
- **no sessions** this week — verify it is actually live before wiring.

The workflow compile-gates the generated Scala (`sbt worker/compile`) before opening the PR, so a bad row fails the job instead of shipping a broken PR.

## exclude.txt

Flicks keeps a `/cinema/<slug>/` page (and sitemap entry) for venues long after
they close, so a closed cinema still looks discoverable. List its Flicks slug —
a roster row's `flicks` — with a one-line reason.
Blank lines and `#`-comments are ignored.

## Run locally

```bash
# report only (no edits) — hits Flicks, prints new/parked/gone
python3 scripts/discover-cinemas/discover.py

# actually wire new venues into data/uk/roster.json (then regenerate: data/uk/scripts/generate_roster.py)
python3 scripts/discover-cinemas/discover.py --apply --report-path discovery-report.md

# unit tests (stdlib only, no network)
python3 scripts/discover-cinemas/discover_test.py
```
