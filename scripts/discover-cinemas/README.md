# UK cinema discovery

Weekly job that finds **new** UK cinemas on [flicks.co.uk](https://www.flicks.co.uk)
and opens a PR wiring them into the catalog. Run by
[`.github/workflows/discover-cinemas.yml`](../../.github/workflows/discover-cinemas.yml)
every Monday 06:00 UTC (and on `workflow_dispatch`).

## How it works

The UK roster is one hand-wired `FlicksClient` per venue, spread across two files:

- `common/src/main/scala/models/Cinema.scala` — a `case object … extends Cinema(display, pill)` plus membership in a per-city grouping `val` (e.g. `val cardiff`).
- `worker/src/main/scala/services/cinemas/CinemaScraperCatalog.scala` — a `flicks("<slug>", Obj)` line inside the city's `<city>Scrapers` Seq.

`discover.py`:

1. Fetches Flicks' `sitemap-cinemas.xml` — every cinema slug Flicks knows.
2. Diffs it against the slugs we already wire, minus [`exclude.txt`](exclude.txt).
3. For each genuinely new slug, fetches the cinema page for its display name + coordinates and probes the sessions endpoint (today … +6d) for whether it currently lists showtimes.
4. Assigns each venue to the **nearest wired UK city** by great-circle distance (cities carry lat/lon in `City.scala`), then splices compilable Scala into both files.
5. Reports **GONE** venues — wired by us but dropped from Flicks' sitemap — as retirement candidates. It never auto-deletes.

### Parking (never emit broken Scala)

A venue is **parked** (reported, not wired) when it can't be placed safely:

- nearest city is **London** — London needs a manual `londonAreas` area-group assignment;
- **object-name collision** with an existing cinema;
- **no coordinates** on the page — city can't be resolved;
- **no sessions** this week — verify it is actually live before wiring.

The workflow compile-gates the generated Scala (`sbt worker/compile`) before opening the PR, so a bad splice fails the job instead of shipping a broken PR.

## exclude.txt

Flicks keeps a `/cinema/<slug>/` page (and sitemap entry) for venues long after
they close, so a closed cinema still looks discoverable. List its Flicks slug —
the first arg of the `flicks("<slug>", …)` catalog line — with a one-line reason.
Blank lines and `#`-comments are ignored.

## Run locally

```bash
# report only (no edits) — hits Flicks, prints new/parked/gone
python3 scripts/discover-cinemas/discover.py

# actually wire new venues into the two source files
python3 scripts/discover-cinemas/discover.py --apply --report-path discovery-report.md

# unit tests (stdlib only, no network)
python3 scripts/discover-cinemas/test_discover.py
```
