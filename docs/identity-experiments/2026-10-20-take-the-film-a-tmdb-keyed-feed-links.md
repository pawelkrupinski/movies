# 2026-10-20 — take the film a TMDB-keyed feed links when nothing speaks against it ("Vorspiel", "Life Support") — NOT LANDED (local tag `experiment/feed-linked-take` in ~/projects/movies, recordings 528 and 38713274617, base `bf2c1e404`)

- **Target:** DE BelowThreshold listings whose Cineamo id names the film: Union Friedrichshagen's "Vorspiel" (286393m,
  Kahane 1987) and Babylon Mitte's "Life Support" (1643143m), from `docs/investigations/2026-10-20-de-*.md`. Prod serves
  both as no film (movie_slots, 10-20).
- **Rule:** an alone acceptance rule `feed-linked` (`Scored.feedLinked`, `AcceptanceFactRules.feedLinkedWhy`): the one
  eligible linked candidate, the title naming it whole (exact / original / alternative), not `contradicted`, no takeable
  whole-named namesake ranked above it.
- **Measured (first form):** 528 DE model 18 gained right, 28 unjudged, **1 right→wrong** (Traum-Kino "TOGETHER APART":
  the alone take split the node off the cluster taking 1557399, to the namesake the operator linked) and **2 new-wrong**
  (Kino Boizenburg "SINGING IN THE RAIN" → a 2025 namesake of Donen's film; Tübingen "Dernier été" → a 2026 namesake of
  the 1981 "Der letzte Sommer"). Each wrong link is the search's first hit, so rank cannot tell them apart.
- **Second form:** a yearless listing's link must name a film two seasons older than its screenings (a Cineamo listing
  states its year only of a film opening now, f5784855d, so a yearless one linked to a current record is an old entry
  linked to a new namesake). 528: 12 right, 21 unjudged, 0 wrong; 38713274617: 12 right, 4 unjudged, 0 wrong — LANDABLE
  by the tool, but the unjudged gains include Odeon's "Fetch" → an 18-minute 2007 short (OmU evening) and DFF's "CLOSE
  YOUR EYES" → a 2019 namesake (Erice's 2023 "Cerrar los ojos" is the likely film), and Yorck Kant's bare "Lola" → Demy's
  1961 film (Fassbinder's 1981 "Lola" equally likely). Life Support (2026) is lost to the season gate.
- **Why stopped:** the gains stand on the operator's pick alone, which 49a827dd2 measured wrong often; the unjudged moves
  could not be evidenced beyond the link, and at least two look wrong.
- **Next angle:** exclude shorts (`ShortFilms.isShort`) and require a second, independent source to name the linked
  film (another venue's billing, a family's take) — the agreement stage's `family.feedLink` shape, which needs a database
  family to have taken it.
