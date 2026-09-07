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
