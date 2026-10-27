# 2026-10-20 — an author's credit refuses its work's older namesake ("Fallen Angels by Noël Coward") — LANDED (branch `stage-play-by`, recording 38713274617, base `78d0f8215`)

- **Target:** DE Cineplex ×5 «Fallen Angels by Noël Coward» — the 2026 capture of Coward's play — which prod takes as
  Wong Kar-wai's 1995 «Fallen Angels» (`accept:favoured-calibrated`, prod mirror 2026-10-19). On recording 38713274617
  the slice already takes the capture (`imdb-suggested`, 11220 runner-up at 59 %): prod's state tips it, so the cure is
  pinned by `IdentityResolverCasesSpec` ("A billing crediting its work to an author"), not by the measure. The
  cross-country, venue-history, venue-kinds and copied-share entries all named this credit as the missing in-country fact.
- **Rule:** `ListingShape.authorCredited` reads "<work> by|von|de|autorstwa <Whole Name>" closing a title (a name of two
  capitalised words or more; a lone surname credits nobody, "Vertigo by Hitchcock"); `ListingShape.creditDenies` rules
  out a film whose title is the WORK word for word, made ≥2 seasons before the billing's screenings, the credited person
  not among its directors, no title of its carrying the person's name ("Mary Shelley's Frankenstein").
  `Acceptance.notAnAuthorsNamesake` refuses such a take in every rule's chain (a refusal, not a candidate denial).
  Allocation-free on billings without an authorship word (an index scan before the pattern).
- **V1 — a node-level DENIAL in `FamilyScope.denialByNode`, work unchecked — NOT LANDABLE:** UK model 22 lost
  (13 right-lost, 3 right→wrong: «Fallen Angels by Noël Coward» → Dhont's «Coward» 2026), US 1, DE 1. Two causes:
  (1) the denial's `denies-film` cannot-links split UK's 92-venue cluster of the capture from the 1995 film's 13-venue
  repertory cluster's bare-title siblings, leaving fragments below the cut; (2) hosts and scores read as authors:
  «Linda Linda Linda 4K Restoration + Intro by Japanese Film Club», «Nosferatu (1922) with Live Score by Hugo Max»,
  «Sexy Beast - Presented by Jackie Treehorn», «Revenge with Introduction by Dylan Mayer», «THE GENERAL - Stummfilm mit
  Klavierbegleitung von Andreas Benz», all-caps «NEUES VON PETTERSON UND FINDUS».
- **V2 (landed) — an acceptance refusal, the work naming the film whole:** `scripts/identity-measure-local --base
  78d0f8215 --head 0c86932e4 --recording 38713274617` (0c86932e4: the code as landed in `76a4ac679`)

  | cc | model lost | changed | gained | right lost | right→wrong | new wrong | unjudged |
  |---|---|---|---|---|---|---|---|
  | de, es, pl, us | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
  | uk | 1 | 0 | 0 | 0 | 0 | 0 | 1 → judged |

  The one move: UK The Poly Falmouth «A Christmas Carol by Patrick Barlow» 17979 (Zemeckis 2009) → none, judged WRONG
  (Barlow's stage adaptation). Final stage moved nothing. Beside the capture's record the tiny-table case takes no film
  rather than the capture (the refused namesake still outranks it on the billing's facts): "no film, never the namesake".
- **Not reached:** PL Multikino ×16 «Fallen Angels by Noël Coward» → Dhont's «Coward» (2026, judged wrong): current,
  so no older namesake. Next angle: the work naming none of the record's titles while the person's surname is the
  record's whole title — a surname echo, not the work.
