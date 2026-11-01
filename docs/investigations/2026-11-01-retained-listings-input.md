# The kept listings in every convergence row's input (2026-11-01)

The owner, 2026-10-30 (`ListingRetention`): "Drop showings, keep movie details from all cinemas data until there are no
showtimes in any cinema in the country". The rule ran in the worker's drain, the drift replay and the equivalence spec. The
other convergence rows did not see it.

## What each row fed the model (main, 5b2e13bd38a)

| row | input | kept listings |
| --- | --- | --- |
| `takes` (identity takes check, hard-cluster ratchet) | `IdentityShadow.listingsOf`, `ModelRead.corpus(archivedListings)`: the recording's corpus | none |
| `convergence`, `order-independence-pNN` | the production `IdentityModelService` over `ArchiveReplayWiring`; take-up from the corpus | none at take-up; `retaining` only for what leaves during the run |
| `next-day` | the same, day 2 cut from day 1 (`NextDayListings`) | only the day-1 to day-2 leavers |
| `takes-drift-N` | `IdentityDrift.sequence`, random leaves from the snapshot | its own leavers only |

A recording is one snapshot: `cinema_scrapes`' last content-bearing scrape per venue. A listing that dropped off a venue's
board while the venue kept scraping is in no recording. Production kept it, but only in memory. `IdentityModelService.retained`
was "lost on a restart", so every deploy forgot the set too.

## The source chosen: the worker's own kept set, persisted and captured

Three sources were considered.

- **A recording's own history.** There is none. Each nightly is one snapshot, and next-day's "day 2" is cut from day 1.
- **Replaying the nightly recordings through `ListingRetention`.** This misses every leave between two nights. It also
  never matched production, which lost the set at each restart.
- **The worker's retained state, captured at record time.** This one is chosen, because it is the only one that is
  production's input by construction. The set had no persistent form, so this change gives it one:
  - `identity_retained` (`MongoRetainedListingStore`) is written by the drain as the set moves.
  - It is resumed by every take-up through `ListingRetention.restored`, the same `leave` applied to the stored set
    against the listings billed now. A restart no longer forgets what the rule keeps.
  - The recorder captures it beside the corpus as `prod-retained-<cc>.json.gz` (`ProdRetained`).
  - Every replay reads it by the same function: `ArchiveReplayWiring.recordedRetained` seeds the take-up for the
    convergence, order-independence and next-day rows; `IdentityShadow.listingsOf` serves the takes check;
    `IdentityLiveDrift.modelInput` serves the live drift check.

Before the store is ever written (the first take-up after this lands), the take-up and the recorder start the set from
`identity_history`. Each listing's newest entry is taken when it still holds the listing. The history keeps a month, and
listing entries go back to 2026-10-29. The rule (`restored`) then keeps those a present listing bills.

## Size

The capture holds every listing the history last saw held, before the rule. For 2026-11-01:

| country | listings | gzipped |
| --- | --- | --- |
| PL | 2,213 | 105 KB |
| DE | 2,241 | 101 KB |
| UK | 4,075 | 238 KB |
| US | 12,265 | 615 KB |
| ES | 953 | 45 KB |

Once the store is written it holds only the kept set, which should be smaller.

## Measurement

See below (identity-measure over prod-latest 41265286306, the capture made after the fact by `scripts.RecordProdRetained`
from the local mirror at each country's recording time).
