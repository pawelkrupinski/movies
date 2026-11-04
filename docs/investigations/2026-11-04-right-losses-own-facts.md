# 2026-11-04 — the two right takes 84cba51ce03 lost, recovered from their own facts or left unmatched

84cba51ce03 keeps a node ambiguous in a stronger solver tier alone in the weaker ones (it cured Torrington "Dracula"'s
arrival-order flip). Two listings judged right lost their takes with it and were recorded `accepted-right-loss` by
5d6413406bf. The owner (2026-11-04): recover them properly — from POPULATION-INDEPENDENT inputs only (the listing's own
facts and TMDB), else unmatched; no stickiness, no flips.

## UK Keswick Alhambra "KFC Presents: Hen" {György Pálfi}, 118′ — recovered

**Own facts:** the billing (a banner, "KFC Presents", and the piece "Hen" after its colon), the director field crediting
György Pálfi, 118 minutes. No year, original title or id. **Namesakes:** Pálfi's 2026 "Hen" (1382513, 96′, TMDB rank 2)
and the Afrikaans 2025 "Hen" (1534155, 108′, rank 1). The runtime fits both (a longer slot says nothing). The credit
tells them apart.

**Why it was lost.** Before 84cba51ce03 the node rode Mwldan's "Hen" cluster. Alone, the rules a node is taken by on its
own facts all refused it. `directors-title` needs the title to name the film EXACTLY, and "KFC Presents: Hen" names it
by a piece (`segment`). `directors-work` needs a work of two words or a year. `segment-top-hit` needs a piece of two
words and the first hit. The model put it at 20.0%, below the cut. 84cba51ce03 did not suppress an own-fact rule: no
own-fact rule covered a banner's one-word piece.

**The rule.** `directors-title` reads a whole delimited piece of the listing's OWN billing as its bare title
(`byPiece`), as `segment-top-hit` does. The piece is the rest after a banner's colon, dash or pipe
(`SearchTitles.candidates` of the title alone). One word is enough, because the credited director tells the word's
films apart, as he does for an exact one-word title. The rule takes nothing when:
- the billing is a double bill or joins another work (`billsTwoWorks`, `joinsAnotherWork`);
- the rest numbers an instalment or dates another year (`pieceStandsForTheWhole`);
- the shape comes only from the published original title or a search title. DE kinoheld's "DAS PACK" as Pack {Peiqi
  Peng} copies the hit record's facts.

The credit must be billed (`ownCredit`). Pinned: AcceptanceSpec ("a whole delimited piece of the billing … one word
too", fails before) and `identity-slices/uk-keswick-hen-4247dc45` (IdentitySliceCasesSpec). The slice takes the same
film in 12 shuffled arrival orders.

## UK Plough Arts Centre Torrington "Dracula", 87′, 12A — stays unmatched (no own-fact rule; owner's call)

**Own facts:** the bare title, 87 minutes, certificate 12A and a Spektrix catalogue id. No year, director, original
title, cast or feed link; the venue page adds nothing. **Namesakes by runtime.delta:**

| Film | runtime.delta | Fits 87′? |
|---|---|---|
| 1958 Hammer (11868) | +5 | yes |
| 1931 (138) | +13 | yes: a longer slot fits (`Corroboration.fits`) |
| 2002 (1655673) | −17 | no |
| 1979 (33521) | −22 | no |
| 1992 (6114) | −41 | no |
| 2025 (1246049) | −43 | no |

So the runtime cannot single out 1958. Of the two films the runtime leaves, only the certificate tells 1958 (TMDB GB
12A) from 1931 (PG). The certificate is a GUARD measure: it weighs in the probability and is no fact a rule counts
(`IdentityMeasures.GuardMeasures`). The other 12A in the list is the 2002 film, which the runtime excludes.

Recovering it needs a NEW fact class, the certificate deciding between namesakes, plus a rule taking a node on runtime
and certificate together. That would still use only population-independent inputs. It is not an existing rule that
fails to fire, and it carries its own risk: re-ratings TMDB may not carry, and coarse ratings that many films share. So
it is not implemented here. It stays `accepted-right-loss`, the namesake-ambiguous class staying unmatched by design,
unless the owner wants a certificate-decides rule designed and measured.
