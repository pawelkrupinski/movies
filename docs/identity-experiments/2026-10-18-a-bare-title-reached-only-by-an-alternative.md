# 2026-10-18 — a bare title reached only by an alternative title (branch `kw-livegaps`, recording 38713274617, base `dd84b11f3`)

## Refuse an IMDb-suggested film a bare billing names only by an alternative title ("Patagonien") — narrowed form LANDED
- **Tried:** DE Kinopolis Freiberg's "Patagonien" (a travel lecture) took Bozzelli's 2023 "Patagonia" by `imdb-suggested`,
  `title=alternative` (TMDB files the film under "Patagonien" beside its own title), with no fact of the listing's own.
  Not class 2's "a take needs a fact of its own" (variant H, ~191 right + ~625 unjudged lost): only the IMDb rule, only a
  bare billing, only a film named by an alternative title. Three forms, `identity-measure-local`:

  | Variant | Refuses | Cured | Right lost | Unjudged lost |
  |---|---|---|---|---|
  | A | `suggestedOnly` (IMDb alone reached it, title not the film's own or original) | 0 | 0 | 8 (DE: Salò, A Soul Haunted By Painting, _UA_Stop-Zemlia…) |
  | B | `title=alternative`, TMDB's search does not rank the film | 1 | 8 | 8 (DE: Eine Seite des Wahnsinns, Tempel zur goldenen Halle, Wir, das Orchester…) |
  | C | B, and TMDB's search of the billing finds OTHER films | 1 | 0 | 0 (every country) |

  A missed Patagonien: the billing carries the film's alternative title, so it is not `suggestedOnly`. B's losses are
  arthouse German titles of foreign films, the rule's very purpose; each had a TMDB search finding nothing at all,
  where "Patagonien" ranks "Abenteuer Patagonien" and a dozen more.
- **Landed:** C, `Acceptance.alternativeAgainstTmdbsSearch` (AcceptanceSpec). Code of A and B:
  `experiment/identity-alternative-title-suggestion`.
- **Next angle:** the final stage was not seen (caveat above): a capture re-emitted after it shows whether the agreement
  refills "Patagonien" with the 2023 film.
