# The prune is a no-op

**Rule (Paweł, 2026-09-07):** staging, enrichment and every other update path must leave
the read model in exactly the state a settle would produce, so the scheduled prune finds
nothing. A card the prune removes is a defect in the path that wrote it, not accepted
churn. The read-model twin of "the settle is self-heal only" (`docs/stable-film-id.md`).

## Why it was not true

The projector subscribed to UPSERTS only, and wrote what a row projects to without ever
asking what it had projected to before. So two shapes leaked:

- **A vanished variant.** A row fans out into one card per display-title variant — the
  plain listing plus each decorated one ("Kino Kobiet: Foo", "Tani Poniedziałek: Foo",
  a festival strand). When that week's decorated listing stopped being scraped, the row
  re-projected to fewer cards and the old variant card stayed in `web_movies`.
- **A row that went away.** A delete or a merge dropped the `movies` document, and
  nothing told the projector: `watchUpserts` discards deletes by construction.

Both were removed up to 30 minutes later by the prune. In between, the worker's
served-films census counted the film and the web could not serve it — the same
discrepancy the drift alert reads, at a smaller amplitude. Measured on production
2026-09-07: about 80 cards pruned every six hours, still climbing after the morning's
heal fixes, because the heals repaired missing cards while the prune kept creating them.

## What holds it now

- `ReadModelProjector` remembers, per source row, the card ids its last projection
  produced (`lastCardsByRow`). A re-projection retires the ids it no longer produces
  (`variant-gone`); a row that loses `readyToProject` retires all of them
  (`row-unready`); a delete event retires them and the cards under the row's own id
  (`row-deleted`). It subscribes with `watchChanges`, so deletes arrive.
- `kinowo_worker_readmodel_cards_retired_total{country,reason}` counts what the stream
  took back. `kinowo_worker_readmodel_films_pruned_total{country,reason}` counts what was
  LEFT to the prune — `row-gone` (no row projects to that id) or `variant-gone` (the row
  lives but no longer produces that variant). **The prune counter must sit at zero**;
  both are drawn on the read-model removals panel of the application-health dashboard.
- `ReadModelConvergenceSpec` (worker) drives the real projector through every one of
  those shapes, plus sixty shuffled steps, and after each asserts that the read model
  already equals the settle's answer AND that `pruneOrphans()` removes nothing.

## When a prune shows up in production

Read its `reason` label, reproduce the shape as a case in `ReadModelConvergenceSpec`
(it will fail), and fix the path that wrote the card. Never widen the prune, and never
schedule it more often — that hides the defect behind a shorter window.

## When `ReadModelServingDiffersFromCorpus` fires but the prune counter is zero

Check `process_start_time_seconds{job=~".*worker.*|.*web.*",country="<country>"}` for a
restart in the ~30 minutes before the divergence started, before chasing a projection
bug. Three US episodes (2026-09-09, 2026-09-14, 2026-09-15) all self-resolved within
20-30 minutes with `kinowo_worker_readmodel_films_pruned_total{country="us"}` flat at
zero for every `reason` throughout — not the prune failing to retire a card, just the
freshly-booted worker/web pair's census and change-stream state re-syncing after a
rolling restart. Two of the three had a worker-us restart within 1-8 minutes of the
divergence starting (and a web-us restart within the same window); the third had no
worker-us restart but a web-us restart 35 minutes earlier. This is a boot-lag shape, not
a data-loss one — no card is permanently missing, it just takes longer than the usual
~5-10 minute census skew to reappear after a restart. Confirm with the same per-city
diff query the alert annotation already gives, and `sum by (reason)
(increase(kinowo_worker_readmodel_films_pruned_total{country="<country>"}[10m]))` staying
at zero rules out the prune-side defect this doc otherwise describes.
