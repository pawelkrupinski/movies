# A departed listing stays an identity input for a configured window (the owner, 2026-11-07)

## The decision

The owner, 2026-11-07, in order:

- "Keep and use listings data (for everything) even after showtimes are gone as long as any rules reference it."
- Corrected: "it doesn't mean keeping past state of an identity, we're just keeping listing details longer". So no prior
  take and no identity history become an input. The 2026-10-31 rule stands: a take is decided on the inputs as they
  stand.
- "don't make it more stateful than it was". No new collection, cache or reference-tracking store.
- "(listing having showtimes or being refernced by identity match) + 30 days (actually)", then "make it 6 months". The
  window was then delegated to the main session, which kept 60 days (2026-11-07) on the measurements below.
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
  (`ProcessConfiguration`), defaulting to `ListingRetention.DefaultMaxAge` (60 days, the old constant). It is counted from
  `departedAt` against the model's clock, never the wall clock, so incremental == whole still holds. With the default
  this is no behaviour change.
- **Not built: "while referenced, then + grace".** The accepted wording keeps a listing for as long as it is referenced
  and only then starts the grace. That would drop the cap for evergreen titles. The main session chose the 60-day cap
  instead (2026-11-07), on the measurements below, until the kept set's growth is seen to level off.

## What it cures, and what it does not

- **Departures: already cured by today's rule.** The family explorer walked a departure as an absence. Walked as the
  worker keeps it (`ListingRetention.retained`; run 42358278155, recording 573), every add/remove-only flip goes: DE 78 -> 0,
  UK 66 -> 0, and no new flip appears. The explorer now walks it so (`FamilyConvergence.flipOf`). A longer window adds
  nothing here.
- **Arrivals: not cured.** A new listing arriving is a new input. The 51 arrival flips stand: DE 18, UK 33, among them
  "Titans", "Momo", "The Killer", "Fallen Angels", "Grinch", "Romeo and Juliet", PFF "Hen" and "Miracle on 34th Street".

## Costs, measured

Dated departures exist only from 2026-11-03, and recordings keep their corpora for 14 days. So the history is
SYNTHETIC (`scripts.RetentionWindows`, on the experiment branch `identity-retained-history`): every listing an older
recording billed that the newest does not is departed, at the last older recording that billed it. The newest
recording's corpus (DE, 2026-11-07, 32.5k listings) is resolved whole with each window's kept listings.

| window (days of history) | departed | kept | take moves | of them to Pending |
|---|---|---|---|---|
| 2 | 634 | 543 | 10 | 7 |
| 4 | 2,862 | 2,600 | 26 | 17 |
| 6 | 4,910 | 4,461 | 77 | 68 |
| 9 | 8,896 | 7,478 | 428 | 420 |

- **Kept grows about 830 listings a day of history** under today's rule: +23% of the billed set after 9 days.
  `identity_retained` was first dated on 2026-11-04, so the kept set will keep growing under the 60-day cap until about
  January. Watch it (Metrics).
- **Takes.** Almost every move is a family going Pending. That is a MEASUREMENT LIMIT, not a cost: a kept listing from
  an older recording asks questions the newest recording's tree never answered, and the replay reads each gap as
  awaited. In prod the worker asks them. One real film move by 9 days; no right -> wrong among the judged.
- **Resolve time** was measured on a loaded laptop (load average 80-200). Those numbers are not usable. The CI sweep
  (run 42361106859) is meant to measure it.

## Metrics

The worker exports `kinowo_worker_identity_model_{present,kept,departed}_listings{country}` beside
`kinowo_worker_identity_projection_seconds`, drawn on the worker diagnostics dashboard's "Identity retention" panel.
Two alerts watch them: `IdentityKeptListingsHigh` fires when kept outnumbers billed for an hour, and
`IdentityProjectionSlower` fires when the 6-hour mean projection is 1.25x the same window a week before.

Risks of a longer window, should one be chosen later: the resolve, the solver and the explorer cost more as families
grow (evergreen titles most); an old namesake's kept listing can contest a later release (the Titans shape); and kept
venues count toward `venues.current`.
