# 2026-10-18 — a venue's synopsis against its namesakes' TMDB overviews (branch `kw-synopsis-signal`, recording 38713274617)

## Refuse an agreed or filled film whose namesake's description the synopsis names over its own — LANDED
- **Data:** the recordings hold every TMDB `/movie` body raw (both overviews: the localized one is often empty, the
  English one rarely); the store's minimal record and the captures held none, so the store now keeps an overview's
  capitalised-word stems (`overview_words`, hashes, never text), the captures an `overviews` section and each
  listing's `words`, filled from the recordings (`UnmatchedClustersFill --recording [--listing-words]`).
- **Signal:** `ProseWords` — the words a text capitalises, each as its first four letters folded and hashed, a venue's
  sentence-first word left out: names and places survive translation and inflection ("Belgii" is "Belgium", "Eliasa"
  "Elias"), so a Polish synopsis reads against an English overview; no stopword list, no language rule.
  `SynopsisEvidence.veto`: a namesake (the cluster's TMDB candidates, else its answered title searches) whose
  overview shares 2+ words with the synopsis that the taken film's overview, cast and directors do not, the taken film
  sharing none of its own; titles' words out. Read at take time like the poster veto, never waited on.
- **Offline first** (Python over the ratchet's 1,386 agreement takes): 105 have a synopsis and an overview; the veto
  fired on 3 — Binti (cure), NADZIEJA at Gołdap (judged right; its page is Sødahl's 2019 "Hope", tmdb 616561, word for
  word: re-judged wrong — a second cure) and CK Lublin's "Lalka" (right; Prus named only by the 2010 musical's
  overview) — which the taken film's cast in its own words spares (the capture's casts filled from the recording).
- **Measure** (identity-measure-local, every country): PL final 2 cured (Binti, NADZIEJA), 0 right lost, 0 new
  wrong; DE, ES, UK, US and every model stage unmoved (DE/ES/US captures got no overview: their agreement takes hold
  almost no venue synopsis, 2 DE and 1 US of 1,386). Memory: a listing holds ~44 bytes of words; intake +600 B per
  listing with a synopsis; a veto reads two overviews' hashes off the store, ~3 KB, nothing held.
- **Not cured, Into The Shadows (UK Beehive):** the Shadows tribute's synopsis shares nothing with the 2009 doc's
  overview, but no namesake's names it either. The absolute form ("names 5+ words, none of the taken film's") also
  refuses Wallace & Gromit (Nerve Centre), George Michael (The Arzner) and Rialto's SYMFONIA ZMYSŁÓW: 1 cure, 3 right
  lost on a threshold fitted to one row — not built.
- **Next angle:** the same veto on a MODEL take (`Correction`): Binti and NADZIEJA are model takes in the re-emitted rows
  (known-wrong `rows`); an event's synopsis with no film database word for Into The Shadows.
