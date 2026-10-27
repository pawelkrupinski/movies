# 2026-10-19 — where in the billing the candidate matched (branch `experiment-title-head`, recording 38713274617)

## A `title.piece` measure: head, lead, trail, inner or decoration vocabulary — not landed
- **Tried:** a listing-film measure, read only for `segment`/`decorated` titles (`TitleRelationMeasures.titlePiece`, built
  on `namingPieces`). It places the piece that names the film within the billing:
  - `decoration`: every word of the piece is an event, edition or format word (`DecorationWords`);
  - `head`: no word beside the piece bills a work;
  - `lead` / `trail` / `inner`: words of a work follow it, precede it, or stand on both sides.
  The weights came from a residual logistic on the refit's labelled units (`scripts.TitlePieceUnits`).
- **State of the target wrongs on main (measured 10-19):** six of the seven "title piece or banner taken as the title"
  wrongs were already cured (Tea Alchemist, Lumière, Le Moulin and FBF take none; Turksib takes the right film). The one
  left, "Pornfilmfestival: The Dating Game", matches the HEAD (a German alternative title), so this signal cannot reach it.
- **Offline (B):** over 3,546 capture units, AUC is flat (0.928). Near the cut (log-odds −8..10), on segment/decorated
  units, log-loss goes 0.468 → 0.425 and AUC 0.815 → 0.833. The fitted offsets: decoration −4.7, head +4.7, inner −6,
  lead/trail ≈ 0.
- **Whole corpus (`identity-measure-local`):**
  - V1 (decoration −4.7, inner −6): 0 cured, 20 judged-right and ~96 unjudged lost. "inner" fired on
    "Banner: Work - Cert PG", and "decoration" on "Pillow Talk".
  - V2 (+ head +4.7): 26 judged-right gained, but 3 new wrong (Ausverkauft!, Człowiek znikąd 2010, Manon 1981) and
    right→other moves (the Twilight Rifftraxes).
  - V3 (placement on the words that bill a work, certificates peeled; decoration −4.7 only, base `bf2a47648`): 1 cured
    (DE "ES (ULRICH SCHAMONI)"), 2 new wrong ("Amazing Grace + Q&A" → 2007, "Dracula + Q&A" → 2025), 4× "Fallen Angels"
    and "I.K.U" right lost, 4 unjudged "+ Q&A" gains that look right (Inside Gaza, Journey There, My Daughter Is a
    Zombie, The Assassin).
- **Why stopped:** every variant makes new wrong takes. Weighing the "Q & A" record down takes away the one rival that
  held a bare, factless head off its namesakes. A decoration piece's real effect is relational: the head is then the
  billing, and a factless head should stay unmatched, not win by default.
- **Code:** `experiment/title-head`.
- **Next angle:** use the placement as a guard, not a weight: a `decoration` piece names no film at all, so the
  candidate drops out without lifting the head's namesakes. The word table also lacks English "premiere" ("World
  Premiere"); add it only with a whole-corpus measure, since every reader of the table moves with it.
