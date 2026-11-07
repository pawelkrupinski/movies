# A departed listing stays an identity input for a configured window (the owner, 2026-11-07)

## The decision

The owner, 2026-11-07, in order:

- "Keep and use listings data (for everything) even after showtimes are gone as long as any rules reference it."
- Corrected: "it doesn't mean keeping past state of an identity, we're just keeping listing details longer". So no prior
  take and no identity history become an input. The 2026-10-31 rule stands: a take is decided on the inputs as they
  stand.
- "don't make it more stateful than it was". No new collection, cache or reference-tracking store.
- "(listing having showtimes or being refernced by identity match) + 30 days (actually)", then "make it 6 months". The
  window was then delegated to the main session ("the biggest retention that doesn't degrade anything too much"), which
  chose 7 days (2026-11-07) on the measurements below.
- Then: "also write and update all listings forever to mongo db (but not memory) ... marked as deleted when they would be
  deleted", "only if it doesn't degrade service / queries", "still wipe showtimes", "keep updated time so we could wipe by
  time in the future" (the archive, below).

## The rule, as built

`ListingRetention` already keeps a departed listing's facts: `identity_retained` holds its last-billed listing and
`departedAt`. A departed listing is an input while a billed listing relates to it by title (`TitleRelation.related`,
today's test and the relation the model groups and backs by) and it departed no more than the RETENTION WINDOW ago.

- "Referenced by an identity match" is read as that title relation, not from stored takes. A stored take naming the
  listing would make an output an input: a decision with the listing among its members keeps it referenced, so it never
  leaves.
- The window is typed config, `settings.IdentityRetentionWindow` (`KINOWO_IDENTITY_RETENTION_DAYS`), read in one place
  (`ProcessConfiguration`), defaulting to `ListingRetention.DefaultMaxAge`: 7 days. The old constant was 60 days, but
  departures were dated only from 2026-11-03, so the window in effect was about 3-4 days, and 7 is a modest step up. It
  is counted from `departedAt` against the model's clock, never the wall clock, so incremental == whole still holds.
- **Not built: "while referenced, then + grace".** The accepted wording keeps a listing for as long as it is referenced
  and only then starts the grace. That would drop the cap for evergreen titles. The main session chose a fixed cap
  instead (2026-11-07), on the measurements below.

## What it cures, and what it does not

- **Departures: already cured by today's rule.** The family explorer walked a departure as an absence. Walked as the
  worker keeps it (`ListingRetention.retained`; run 42358278155, recording 573), every add/remove-only flip goes: DE 78 -> 0,
  UK 66 -> 0, and no new flip appears. The explorer now walks it so (`FamilyConvergence.flipOf`). A longer window adds
  nothing here.
- **Arrivals: not cured.** A new listing arriving is a new input. The 51 arrival flips stand: DE 18, UK 33, among them
  "Titans", "Momo", "The Killer", "Fallen Angels", "Grinch", "Romeo and Juliet", PFF "Hen" and "Miracle on 34th Street".

## Costs, measured

Dated departures exist only from 2026-11-03, and recordings keep their corpora for 14 days. So the history is
SYNTHETIC (`scripts.RetentionWindows`, kept as the tag `experiment/retention-windows`): every listing an older
recording billed that the newest does not is departed, at the last older recording that billed it. The newest
recording's corpus (2026-11-07; DE 32.5k listings, UK 33k) is resolved whole with each window's kept listings, on a CI
runner (run 42361106859). Model takes.

| country | window (days) | kept | resolve s | film moves | right -> wrong | new wrong | right lost |
|---|---|---|---|---|---|---|---|
| DE | 0 | 0 | 53.6 | - | - | - | - |
| DE | 9 | 6,596 | 51.7 | 31 | 0 | 0 | 1 |
| DE | 18 | 12,255 | 83.6 (+56%) | 42 | 0 | 0 | 2 |
| UK | 0 | 0 | 55.0 | - | - | - | - |
| UK | 9 | 6,160 | 62.6 (+14%) | 72 | 0 | 0 | 4 |
| UK | 18 | 14,537 | 67.3 (+22%) | 81 | 0 | 1 | 4 |

- **Kept grows linearly with no plateau by 18 days**: DE ~680 a day of history, UK ~810. Extrapolated, a 60-day window
  would keep more listings than are billed (DE at ~45-50 days).
- **Resolve time**: within noise at DE 9 days (one run per point; DE's 2-day point read 77.8 s), +14% at UK 9 days,
  +22% to +56% at 18. Seven days is the largest window the ~10% budget supports. It keeps about 5k listings in DE and
  6k in UK, well under the billed sets.
- **Takes**: no right -> wrong. The right lost (DE "KARLA" at Kommunales Kino Pforzheim; UK "Candyman (1992)" at four
  Curzons, Pending) and UK's one new wrong (Beehive "Nostalgic Cinema: The Secret Garden", Pending) come with windows
  longer than 7 days.
- **A measurement limit**: thousands of "same film, now Pending" moves. A kept listing from an older recording asks
  questions the newest recording's tree never answered, and the replay reads each gap as awaited. In prod the worker
  asks them.
- **Re-measure on real history**: the archive (below) keeps every departed listing's row and its dates, so a later
  window change can be measured on real dated departures instead of a synthetic history. Due once prod has about three
  weeks of them (around 2026-11-25): if `kinowo_worker_identity_projection_seconds` stayed flat as the kept set grew,
  raise the default to 14 days.

## The archive: rows are expired, never deleted (milestone c)

`identity_retained` keeps every row. One document per listing key (`_id`): a listing that departs again replaces its own
row, never adds one. Where the store used to delete a row (the listing billed again, or past the window, or left out of a
take-up's `replace`), it now EXPIRES it instead. `expiredAt` and `updatedAt` are set from the injected clock, and the
row's screening days (`listing.screenings`) are stripped ("still wipe showtimes"). The row keeps the listing's details:
title, facts, venue, links and timestamps. `updatedAt` is written on every insert, replace and expiry.

- **Readers.** The store's `read` (the worker's take-up; `tools.ProdRetained`, the nightly recording's capture) asks for
  `expiredAt: null`, the live rows and the marker. So neither memory nor the capture grows with the archive. The prod
  mirror copies the whole collection, and no mirror tool reads it except through the store.
- **Index.** One index, (`expiredAt`, `updatedAt`). The reader's query uses its prefix. A future wipe
  (`MongoRetainedListingStore.expiredBefore`: `expiredAt > 0`, `updatedAt` < T; not `$exists`, which reads every index key) is a range on it. `MongoRetainedListingStoreIntegrationSpec` holds both to IXSCAN
  over ten expired rows for every live one, and holds that the reader examines only the live rows and the marker.
- **Disk.** Unique keys departing a day, measured on the mirror's `identity_history` from 10-29 to 11-06: DE ~410, UK ~530,
  US ~1,730, ES ~180 (PL keeps no history). Only ~1% of keys depart twice in that week, so unique rows grow at roughly
  the departure rate, ~2.9k a day across countries. At 310-900 B a row (the mirror's average object size per country)
  that is about 0.5-1 GB a year. Mongo has no per-collection size metric (the exporter deliberately leaves collstats off);
  the volume's `filesystem-capacity` alert covers it.

## Metrics

The worker exports `kinowo_worker_identity_model_{present,kept,departed}_listings{country}` beside
`kinowo_worker_identity_projection_seconds`, drawn on the worker diagnostics dashboard's "Identity retention" panel.
Two alerts watch them: `IdentityKeptListingsHigh` fires when kept outnumbers billed for an hour, and
`IdentityProjectionSlower` fires when the 6-hour mean projection is 1.25x the same window a week before.

Risks of a longer window, should one be chosen later: the resolve, the solver and the explorer cost more as families
grow (evergreen titles most); an old namesake's kept listing can contest a later release (the Titans shape); and kept
venues count toward `venues.current`.
