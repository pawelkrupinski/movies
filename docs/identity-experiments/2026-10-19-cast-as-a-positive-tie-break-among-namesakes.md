# 2026-10-19 — cast as a positive tie-break among namesakes (branch `experiment-cast-tiebreak`, tag `experiment/cast-tiebreak`, recording 38713274617, base `f48ab8001`)

Follows `experiment/cast-overlap` (coverage, not the rule, was the limit).

## Coverage first — LANDED in f48ab8001 (languages only)
- **Where cast was missing.** `TmdbNormalizer.minimal` has kept the top 10 cast since f1b4f51c6 (2026-10-06); prod's records
  filed before it hold none (lacking `credits.cast`: PL 17,483, UK 31,783, DE 42,064, ES 7,577, US 44,413 of the Local
  partials, mirror 10-19). The captures held a cast for 5–7 % of the films they asked because they were taken
  (10-13) through a `MemoLookups` that passed no cast on (fixed 5faaac6c8): every cast asked was noted unknown.
- `UnmatchedClustersFill --recording` now answers every cast a capture asked (and any its replay newly asks) and each
  record's `original_language` (now kept by `minimal` and on `IdentityMeasures.Film.originalLanguage`) off the
  recording's TMDB tree: PL 1,299, UK 3,478, ES 91, US 296 casts; DE asks none (its venue casts are kinoheld copies).
- **The refilled casts alone, under today's `cast.venue`** (`7e20550f2`, measured fc2288144 → 7e20550f2): UK final
  +6 — 5 gained unjudged (all right, judged below) and **1 NEW WRONG**: Electric Palace Hastings' "Straight Talk: A
  Tribute to Dolly Parton" → the 1992 film (the venue page credits Barnet Kellman and the film's runtime/year: a feed copy
  of the film the tribute is named after). So only the languages landed; the casts stay on this branch. The next
  re-capture (casts now pass through `MemoLookups`) and the prod cast backfill will bring that wrong take back unless
  the cast rule changes first.

## The tie-break — LANDED 2026-10-19 (with the refilled casts)
- `CastEvidence.tieBreak`: of two or more candidates (contenders and denied ones) the billing names alike
  (`Agreement.namesIt`), the one whose top-billed cast the venue's OWN text (`SourceReliability` Names, as before) names
  two whole names of, no other doing so; never a lone candidate; a rival named once or not at all is no evidence against
  it; nothing where a rival's cast is unknown, where any listing bills a director, or bills several works
  (`ListingShape.billsSeveral`). Still the pinned `cast.venue` fill rule (agreement stage only), never a fitted or pooled
  signal; the fill's guards (`bill.piece`, `stage.work`, …) apply as before.
- **Measure** (identity-measure-local, f48ab8001's tree fc2288144 → 1009a8c83, every country): model stage unmoved
  everywhere; UK final +6 gained, 0 lost, 0 right lost, 0 new wrong; DE, ES, PL, US unmoved. The Dolly Parton tribute is
  not taken (a director is billed). Gained, all judged right 10-19: Chapter Cardiff "Out of the Woods" ×2 (2025, Sal
  Bardo: Fisher, Kennedy, Davison named), Phoenix Leicester "Amazing Grace + Q&A" (Walters, Lewis) and "Premature +
  Writer/Director Q&A" (Rottger, Watson), Riverside Studios "Man Baby (… Preview + Q&A + Props Table)" (Ladwa,
  Fitzgerald), Riverside Theatre Woodbridge "Dracula + Q&A" → Hammer's 1958 film (Lee, Cushing; new against the old
  rule, which a remake's one shared name blocked).
- **(B) confidence/separation for listings billing no director:** none at the model stage — cast never enters the model;
  in the agreement stage it decides 6 no-director ties right, 0 wrong. Too few to refit anything on.
- **Not tried:** cast for a lone candidate (no tie); prod's served takes (needs the backfill); cast in the model's
  logistic (experiment A of `cast-overlap`: −22 right).
- **Next angle:** land the tie-break before (or with) the prod cast backfill and the next re-capture, which otherwise
  surface the Dolly Parton wrong take under today's rule.
