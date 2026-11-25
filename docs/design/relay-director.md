# A relay and its credited director name the production (identity design, 2026-11-25)

The owner approved it 2026-11-25 ("the NT Live rule"): a relay (NT Live, the Met, the Royal Ballet & Opera and the like)
plus the director it credits names the PRODUCTION, the filmed staging, not a film of the play.

## What was wrong

PL Multikino bills the National Theatre's 2026 "The Misanthrope" as "NT Live: Mizantrop" {Indhu Rubasingham} [115'] at
15 venues, screening 2 December 2026 (prod mirror 2026-11-25: none, BelowThreshold; judged right as tmdb 1693710 since
2026-10-13). TMDB holds the record only in English ("National Theatre Live: The Misanthrope", alternative "The
Misanthrope"), and its Polish search for "Mizantrop" ranks Szifron's "To Catch a Killer" (Polish title "Mizantrop")
first. Her director search reaches 1693710 beside her other National Theatre relays ("The Jungle Book", April 2027;
"Bacchae", September 2025; "The Father and the Assassin", 2024), but every rule reads the WORK: house-production needs the
record to bill the listing's work under its house, directors-work the work's words, and the stage-work table holds
operas and ballets, not plays. So no rule could tell which of her productions the relay is.

## The rule (`relay-director`, the last alone rule)

The one eligible record that:

- opens with the banner the listing opens with: spelt alike, abbreviated or initialled by it ("NT Live" of "National
  Theatre Live"), numbers alike, no edition's words (`StageRelays.opensUnderItsBanner`); a word merely sharing letters
  ("Met Live") names no house;
- credits the director the listing credits (`Scored.ownCredit`), no published fact contradicting it;
- is a broadcast of the season the listing's screenings fall in (`RelayStage.seasonsOf`), aired (released) by the year
  its first screening falls in: a record of a later year has not aired yet (`ListingDating.relayScreeningYear`).

Required: the listing credits a director, bills a relay's banner (`RelayBanner.billed`), bills one work, and no record
bills its work under its house (that is house-production's, taken or refused). Two such records are no answer.

`relayScreeningYear` is held only for a listing billing a relay's banner (`RelayBanner.billed`) and crediting a director, and is part of its
evidence key (`Evidence.key`), so no other listing's key moves; it reads the listing's own screenings, no other listing
and no take: a whole resolve and the incremental model decide alike.

## Its cost, by design

- In January-June of a season both of a director's relays of that season may have aired: two, no answer (a miss, never
  a wrong take).
- An encore of an OLDER production of the same director, billed under a title no rule links to its record, in a season
  where she also has a new relay, would read as the new one. Judged rare; the what-if over five countries is the check.

## Test

`AcceptanceSpec` "a relay crediting its director": the Mizantrop candidates as TMDB holds them take 1693710 by
`relay-director`; January 2027 (both aired), another season, no credit, another director, a bare billing and another
banner's record of her take nothing.
