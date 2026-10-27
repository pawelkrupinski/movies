# 2026-10-22 — the billing parse's recall (WS2 of `docs/proposals/2026-10-21-matching-fundamentals.md`, phase 2)

## "~600 strand-prefixed and edition real films" — sized again, parked
- **Tried:** before building a behaviour change on `BillingParse` (strand and banner stripping, edition tags), size the
  proposal's ~600 untaken listings again on its own export (`scratchpad/fund/ex_*.jsonl`, the five prod mirrors of
  10-21). Untaken "film"-kind listings with a banner or strand piece, an edition or version tag, a bracketed credit,
  certificate or runtime noise, or quotes, whose work (the piece left) matches a TAKEN listing's clean title in the
  same country.
- **Result:** 1,378 untaken listings show such a shape, but only **50** have a work a taken title matches. Most
  banner-shaped ones are strands or events of their own ("Toddler Club" ×76, "Kurz Film Tour 2026", "Pornfilmfestival",
  "Spatzenkino"). Edition tags matched 5 ("LA HAINE (30TH ANNIVERSARY)"). Several of the 50 are relays or live events
  the "film" kind missed ("NT Recorded Live: Golden Boy", "Opéra de Paris: Notre-Dame De Paris"), whose taken id
  may be the wrong target. The biggest clusters: UK "Home Alone (with Santa)" ×6, DE "Dune: Teil 1 + 2 - Vision" ×2.
- **Why parked:** no cluster is worth a resolver change on its own. What landed instead are the parse bugs below, which
  move no take on 528 (pl, de) or 573 (uk).
- **Landed with it ("BillingParse: no instalment from a date, …"):** numbers a slash joins (dates, short seasons such as "RB&O 26/27") and a
  series that opens and closes on a number ("13 GOING ON 30", "9 to 5") are no instalment. A subtitle drops its
  closing version tag and reads the version, edition and anniversary vocabulary (PL "wersja reżyserska", ES
  "aniversario"). The golden file moved on exactly those 79 billings.
- **Next angle:** the relay matcher (WS1) holds the larger recall. The parse's remaining value is deleting readers.
