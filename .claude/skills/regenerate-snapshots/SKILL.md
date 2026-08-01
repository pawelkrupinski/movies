---
name: regenerate-snapshots
description: How to regenerate the three checked-in snapshot layers — the whole-corpus assertion (expected-schedules.txt), the projected read-model snapshot (read-model-snapshot.json), and the rendered-HTML page snapshots (expected-*.html). Use whenever a change alters the HTML a Twirl template emits, or alters the pipeline's output (scrapers, enrichment, TitleNormalizer, staging fold, ReadModelProjector, model fields, raw fixtures).
---

# Regenerating the snapshots

There are THREE snapshot layers, regenerated for DIFFERENT changes:

- **`expected-schedules.txt`** — the whole-corpus ASSERTION. One block per
  film, rendered from the controller's `FilmSchedule` output: `displayTitle`
  beside every raw `cinemaTitles` spelling, runtime, year, countries, poster,
  synopsis length, cast, director, tmdbId, imdbId, all four ratings, the
  MC/RT/Filmweb URLs, per-cinema slot provenance, and every showtime with its
  room and format. Regenerate for the same changes as the read model below.
  Read its diff FIRST when several snapshots move: it is the only layer that
  names the films that appeared or vanished, and a pipeline change that
  silently drops films shows up here as deleted blocks.
- **`read-model-snapshot.json`** — the pipeline's OUTPUT (what `web_movies` /
  `web_screenings` hold). Regenerate when you change anything that alters that
  output: a cinema scraper, the enrichment pipeline, `TitleNormalizer` rules,
  the staging fold, `ReadModelProjector`/`ReadModelProjection`, model fields, or
  the raw fixture files under `08-06-2026/`. A render-only Twirl/CSS change does
  NOT touch it.
- **`expected-*.html`** — the RENDERED output. A pipeline change usually
  shifts ALL THREE (the new read model renders differently), so regenerate the
  HTML too; a render-only change shifts only the HTML.

## Page snapshots (`expected-*.html`)

`PageSnapshotSpec` (`web/src/page/scala/views/PageSnapshotSpec.scala`)
diffs the rendered HTML for `/` (per city: Poznań, Wrocław, Warszawa)
and `/plan` against checked-in expected files under
`test/resources/fixtures/08-06-2026/`. Any change that alters the HTML a
Twirl template emits — new or changed attributes on an element,
added/removed markup, reordered output, changed inline JS — will break
the snapshot comparison. (Comments inside inline `<style>`/`<script>`
blocks are stripped by `tools.Minify` at render time, so editing those
does NOT shift the snapshot; HTML `<!-- -->` comments do survive.)

When your change intentionally alters the rendered HTML:

1. Delete the stale expected file(s):
   ```
   rm test/resources/fixtures/08-06-2026/expected-index.html
   rm test/resources/fixtures/08-06-2026/expected-wroclaw-index.html
   rm test/resources/fixtures/08-06-2026/expected-warszawa-index.html
   rm test/resources/fixtures/08-06-2026/expected-plan.html
   ```
   Delete only the pages your change affects. When in doubt, delete
   all four — they regenerate in seconds.

2. Run the snapshot spec:
   ```
   sbt 'web/PageTest/testOnly views.PageSnapshotSpec'
   ```
   The spec writes the missing file(s) and fails with
   "Snapshot didn't exist — wrote …". This is expected.

3. Re-run to confirm the new snapshot is stable:
   ```
   sbt 'web/PageTest/testOnly views.PageSnapshotSpec'
   ```
   All tests should pass. If they don't, the rendering is
   non-deterministic — investigate before committing.

4. Commit the regenerated snapshot(s) alongside the production change
   that caused them. Don't commit snapshots in a separate commit —
   they're part of the same logical change.

Changes that typically require regeneration: Twirl template edits
(`web/src/main/twirl/views/*.scala.html`), `PosterProxy` output changes, model
fields that surface in the view, CSS class or `data-*` attribute
changes on rendered elements, inline `onerror`/`onclick` handler
changes.

## Whole-corpus snapshot (`expected-schedules.txt`)

`FilmScheduleEndToEndSpec`'s first test asserts ONE anchor film field-by-field
inline, and gives every other film in the fixture corpus the same depth through
this file — ~11,000 lines, one block per film, rendered deterministically
(films by lowercased title, cinemas by display name, showtimes by clock).

It is the layer that tells you WHICH films moved. The read-model snapshot is a
blob; this one deletes or adds a named block, so a pipeline change that silently
drops films reads straight off the diff:

```
diff <old> test/resources/fixtures/08-06-2026/expected-schedules.txt \
  | grep '^[<>].*==='
```

Regenerate exactly like the read model, and read its diff before you trust
either:

```
rm test/resources/fixtures/08-06-2026/expected-schedules.txt
sbt 'e2e/testOnly services.movies.FilmScheduleEndToEndSpec'   # writes it, fails "didn't exist"
sbt 'e2e/testOnly services.movies.FilmScheduleEndToEndSpec'   # re-run: must pass (stable)
```

## Read-model snapshot (`read-model-snapshot.json`)

The page-test servers no longer recompute the ~110s fixture corpus pipeline on
every boot. `FixtureServerMain` (the Playwright + mobile fixture server) and the
in-JVM `PageSnapshotSpec` / `PageJsBehaviourSpec` LOAD a checked-in projected
read model — `test/resources/fixtures/08-06-2026/read-model-snapshot.json` — via
`FixtureTestWiring.bootFromSnapshotOrPipeline` (see
`worker/src/fixtures/scala/tools/ReadModelSnapshot.scala`). The snapshot is the
deterministic output of `bootStartup` (scrape → enrich → stage → fold →
project), captured once instead of ~15× across the page-test runners.

The guard is `FilmScheduleEndToEndSpec` ("...match the checked-in read-model
snapshot..."), which boots the REAL pipeline and diffs it against the file —
so a stale snapshot fails CI loudly (in the `e2e (rest)` shard) with the exact
regenerate command. To regenerate after an intentional change:

```
rm test/resources/fixtures/08-06-2026/read-model-snapshot.json
sbt 'e2e/testOnly services.movies.FilmScheduleEndToEndSpec'   # writes it, fails "didn't exist"
sbt 'e2e/testOnly services.movies.FilmScheduleEndToEndSpec'   # re-run: must pass (stable)
```

Then regenerate the `expected-*.html` per the section above if rendering shifted,
and commit all of them together with the production change. Consumers fall back
to the full pipeline boot when the file is absent, so a forgotten regen is slow,
never wrong — but the guard still fails until you commit the fresh snapshot.
