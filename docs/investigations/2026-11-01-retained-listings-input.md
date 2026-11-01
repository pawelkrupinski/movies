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

## Measurement (2026-11-01)

The run was `scripts/identity-measure 1a25d0c97a5 --recording 41265286306 --all-countries --one-recording`, run 41308398650.
The capture came from the local mirror at each country's recording time (`scripts.RecordProdRetained`). It went onto a
measure-only commit and was never landed. Base is 5b2e13bd38a.

| country | kept added to the input (of recorded) | moved among the billed | new takes of kept listings |
| --- | --- | --- | --- |
| PL | 415 | none | 1: Kino Sokół "PIESEK" -> tmdb 1319522 *Gohan* (2026) |
| DE | 72 (of 2,241) | 18 gained: "Eternity" -> tmdb 1259102 | 16: UCI "Brute-1986" -> tmdb 1433854 *Brute 1986* (2026) |
| UK | 60 | none | none flagged |
| US | 66 | none | none flagged |
| ES | 5 | none | none flagged |

No take is lost and no take changes film.

Each take was judged against TMDB:

- **PL "PIESEK" -> Gohan: right.** It is the Thai *โกฮัง..หัวใจโกโฮม*, released in Poland as "Piesek". Multikino's
  "Piesek" lists its directors, Atta Hemwadee and Nattawut Poonpiriya. The suspect check fired on the Thai title alone.
- **DE "Brute-1986" -> Brute 1986 (2026): right.** This is Marcel Walz's slasher, released in Germany on 26.11.2026.
- **DE "Eternity" x18 -> tmdb 1259102: WRONG.** That id is David Freyne's 2025 romcom. The 18 venues bill Susanne
  Regina Meures' 2026 *Eternity*, Florentina Holzinger's glacier film, which plays across Germany on 25 and 29 November
  2026. It is not on TMDB, so "none" was the right take.
  - The cause is the rule itself. Retention reads a title alone, so Caligari Ludwigsburg's kinoheld listing of Freyne's
    film (`/film/eternity`, director David Freyne) is kept while the Meures screenings bill "Eternity". It then
    corroborates the bare "Eternity (2025)" listings.
  - Production holds the same 18 wrong takes. `identity_history` has them gained at 2026-10-31T10:49Z, six minutes
    after that listing left Caligari's board (10:43Z).
  - Before this change a restart cleared them. With the kept set persisted, they outlast restarts until no venue bills
    "Eternity".

The full suite on the same commit and recording ran as run 41308412546. The convergence, order-independence and next-day
rows read the kept listings. The drift rows read none: their path, `IdentityDrift` and `IdentitySlice`, is untouched.

## Re-measure over the kept-apart rule (2026-11-01)

`docs/investigations/2026-11-01-kept-overruled-apart.md` (the owner's "B") landed first. This branch is rebased on top
of it.

The re-measure ran as run 41324825465. Its base was the rule alone; its head was this branch with the kept input,
prod-latest 41265286306. Nothing moved among the billed listings in any country, and the 18 DE "Eternity" stay none.
The kept listings' own takes are all right: PL "PIESEK" -> Gohan, and DE UCI "Brute-1986" x16 -> Brute 1986, judged in
identity-judgements.tsv. No wrong take is left for the persisted set to preserve. LANDABLE.
