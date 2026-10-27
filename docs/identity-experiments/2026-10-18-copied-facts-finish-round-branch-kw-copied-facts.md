# 2026-10-18 — copied facts, finish round (branch `kw-copied-facts-2`, recording 38713274617, base `4713c2ea0`)

Whole-corpus local measure of the three landed changes together, every country: DE model 3 cured (The Killer ×2,
Filmladen Kassel "Space Cadet") and 4 right gained (The Killer at four bare Cineamo venues); DE final 2 cured (ATMOS:
Michael, Ganes); 0 right lost, 0 new wrong; ES, PL, UK, US nothing moved.

## A feed-linked fill against a current first hit its copied year denied ("ATMOS: Michael") — LANDED
- **Tried:** `family.feedLink` (a database family's take + the TMDB-keyed feed's link) does not fire where every listing's
  year is a feed's copy and that year denied the CURRENT film TMDB ranked first for a title naming it, for an older film:
  the family took the copy's film by the copied year, no second source.
- **Result:** first form (any first hit) also lost DE Gröben-Lichtspiele's "Camouflage OmU" (Cineamo and Filmweb on the
  2026 Ukrainian film, the 2016 namesake ranked first); narrowed to a current first hit and an older linked film, it
  cures ATMOS alone. The format-banner angle (Atmos beside a silent film) was not needed.

## A near-spelling copied original names no film for the agreement ("Ganes") — LANDED
- **Tried:** the agreement reads a listing whose original title is a feed's copy one edit from its billing
  (`Agreement.copiesItsSearchHit`) without that title when it asks whether the billing names the families' film. The
  brief's "more than one edit" angle could not reach Ganes ("Ganes"→"Ganges" is one edit); `IdentityMeasures.Listing.
  originalStandsApart` already reads such a copy as the feed's search hit at the model stage, the agreement did not.
- **Result:** Ganes cured, nothing else moved (no typo-rescued agreement in the corpus).

## A venue's own subtitle holds its billing off a film titled without it ("Auf Augenhöhe") — not landed
- **Tried:** two forms. (1) No title-segment must-link from a factless "Work - Subtitle" billing (3+ words beyond the
  work, no event or edition word, in none of the film's titles nor its tagline) to the cluster of the film titled "Work":
  the node still joined by the title family's vote (`same-film`). (2) The same test as a node-level denial in
  `FamilyScope` — cures Ostpassagen's "Auf Augenhöhe - Jugend, Macht und Kinderschutz".
- **Result (form 2, base `9a28edbd2`, every country):** DE 41 + PL 14 + UK 6 judged right lost and ~100 unjudged:
  real subtitles TMDB lacks ("Akte Grüninger - Die Geschichte eines stillen Helden", "Hurtigruten und Norwegen - Eine
  epische Reise") and venue decorations the event words miss ("LIEBE BRAUCHT KEINE FERIEN (Best of Cinema)", PL
  "Vincent. Legenda Oceanu", "/ napisy - Nasze Kino", UK "Fallen Angels by Noël Coward", "GIANT - The Play (15)").
- **Why stopped:** far past 100:1. The known-wrong row's reason is confirmed by measure.
- **Code:** `experiment/identity-own-subtitle-apart` (form 2 on top of form 1).
- **Next angle:** only the venue's other programme can tell the circus show apart (Ostpassagen bills theatre); a venue
  whose other listings are mostly non-film events, read as a prior, is the signal — no listing field carries it today.
