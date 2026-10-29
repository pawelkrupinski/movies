# 2026-10-28 — the drift replay's stage hearing TMDB's filings as the worker's does — PARKED, it surfaces a correction lag (pinned 573, base `3f34bbab5`)

- **What:** the worker makes its family store follow the live agreement stage by hand (`IdentityProjectionWiring`:
  `awaitedBy(stage.wanted.contains)`, `readBy(stage.reads)`), so the TMDB store's filing of a find, record or search the
  stage waits on or read moves the stage's answers' version. The drift replay (`scripts.IdentityDrift`) does neither: its
  stage is built through `AgreementStageComposition` without that subscription, and its fills release TMDB answers to the
  model alone. The experiment moves the subscription into the composition (`AnswerChanges.follow`, called for every
  stage it builds; the worker's hand-made copy goes) and has the replay's fills announce what they file under the TMDB
  store's own keys (`IdentityDrift.announce`). Found while fixing Main's red convergence #2335 (run 3509), whose
  takes-drift rows failed on flips alone.
- **Measured:** `scripts/identity-ci-run --recording 573` (the bare pair, no fills: the leg's flips differ a little),
  `scripts.IdentityDrift <cc> --seeds 2`, base `3f34bbab5` against the experiment with the flip-predicate change
  (`5924def25`, which only removes flags) on top: runs 3547 / 3545 (de), 3553 / 3548 (pl), 3554 / 3546 (uk),
  3555 / 3549 (us). No divergence on either side.
- **Result:** flips base -> experiment: de 4 -> 4 (Werkstattkino "CAMOUFLAGE"; CinemaxX "Titans" at Dammtor,
  Raschplatz and Regensburg — model takes, the same both sides); us 0 -> 0 (the leg's US "NT Live: Hamlet", withdrawn
  at a deploy, does not reproduce on the bare pair either side); uk 8 -> 6, the two gone (CBeebies "Treasure Island",
  GFT "Candyman (2021)") the predicate change's, Ties and Rocky Horror flipping on both sides; pl 1 -> 2, Kino Eva's
  "LALKA" gone (the predicate change's) and two NEW: Kino Oaza and Przedwiośnie "Ktoś całkiem obcy", 7183 at boot
  (OwnMatch) -> 1193128 at step 5, a re-scrape republishing them ("inputs unchanged": Corrected by families and poster).
- **Why parked:** hearing the filings makes the replay's stage the worker's (a take of the title prod flipped on
  2026-10-25, PL "Ktoś całkiem obcy", then moves as prod's did: the model's 2007 film first, the correction later), but
  landed alone it trades flips: PL's seed-1 row goes from red on a false flag
  (LALKA, which the predicate change removes) to red on a real one. Those takes' correction (poster and families) is
  applied only when a re-scrape republishes their listings, not at the apply after the answers it reads were filed. UK
  Chichester's "The Ties That Bind Us" (seed 1) is corrected the same way, at the re-scrape, without the experiment.
  That correction lag is the fix to make first; land this with it, the replay then showing whether it holds.
- **Code:** local tag `experiment/drift-stage-hears-tmdb-filings` (`fd8509094`): the composition, the worker's wiring,
  the replay, and two tests — `CorrectionArrivalOrderSpec` "its answers' changes should follow the stage the composition
  built" (fails before: "the composition built a stage its answers' changes do not follow") and `IdentityDriftSpec` "a
  fill's TMDB answers should move the stage's answers' version".
