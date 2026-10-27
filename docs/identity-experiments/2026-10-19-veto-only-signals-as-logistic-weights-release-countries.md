# 2026-10-19 — veto-only signals as logistic weights: release countries, certificate, poster, synopsis — certificate LANDED (branch `experiment-weighted-guards`, tag `experiment/weighted-guards`, recording 38713274617)

**Tried.** Promote four signals that today only veto or pin a take to weighted levels of the per-candidate model,
with every guard left as it is:

- `release.country`: released / unreleased in the venue's country ([[ReleaseVeto]]'s record).
- `certificate`: match / near / below / above, on the venue country's scale ([[Certificates.below]]'s ladder).
- Poster pHash: match ≤8 bits / near / far / contradicts (another candidate matches).
- Synopsis proper names against the TMDB overview: match ≥2 / near 1 / none / contradicts.

Release and certificate became `ListingFilmMeasures` slots (`IdentityMeasures.GuardMeasures`). They sit outside
`FactMeasures` and the own-evidence sums, so they weigh in the probability only, and the venue's certificate is
carried into `Evidence`. With no table, the control commit moved 0 takes over all five corpora. Each signal's table
was fitted the calibration's way (log-likelihood ratio, α 0.5, missing neutral) on every candidate of the five
recorded corpora: weak labels from today's takes, judgements overriding (`integration.IdentityGuardUnits`,
`scripts/ml-identity/weighted_guards.py`).

**Poster and synopsis cannot be per-candidate measures.**

- *Poster:* hashes exist only for the posters the agreement stage asks about. A node averages ~20 candidates × 8 TMDB
  posters, so hashing for every candidate means millions of image fetches. A hash costs 8 bytes to hold.
- *Synopsis:* overviews come from the whole TMDB document, one Mongo read per candidate. The recordings answer none (0%).
- Both were measured for confidence (B) on the captures only.

**Coverage.**

- Captures (3,547 hand-labelled units, 189 right): release 100%, certificate 5.2% (UK only), poster 17.7% (UK, PL),
  synopsis 8.1% (UK, PL).
- Recordings (403,854 candidate rows): release 100%, certificate 7.8% (match 2,917/11,741, below 0/584).

**(B) Confidence**, grouped 5-fold cross-validation over today's log-odds. The joint row is the levels refit by a
logistic beside today's score; the additive row is the calibration's own fit added to it.

| units | variant | AUC | log-loss |
|---|---|---|---|
| captures | today | 0.928 | 0.1341 |
| captures | + poster (joint) | 0.938 | 0.1223 |
| captures | + synopsis (joint) | 0.932 | 0.1255 |
| captures | + all four (joint) | 0.942 | 0.1148 |
| captures | + release / + certificate (joint) | 0.928 / 0.926 | 0.1340 / 0.1349 |
| recordings | today | 0.9975 | 0.01939 |
| recordings | + release, additive | 0.9968 | 0.02074 |
| recordings | + both, joint | 0.9976 | 0.01906 |

- *Recordings:* release added the calibration's way hurts, and pushes down 347 of 759 judged-right takes by more than
  0.3 log-odds. The joint refit gains next to nothing.
- *Captures:* poster and synopsis separate right from wrong where known. Poster contradicts 0/155 right, match 24/0;
  synopsis contradicts 0/126, match 25/4.

**(A) Takes against base** (identity-measure-local; model stage, final 0 throughout).

| variant | gained right | gained unjudged | right lost | new wrong | other |
|---|---|---|---|---|---|
| release (base b79c71a78) | 12 | 9 | 16 | 2 | 3 cures, 65 unjudged moves |
| certificate (base b79c71a78) | 10 (5 judged in 916c9835d) | 0 | 1 | 1 | 3 unjudged lost, 1 switch |
| both (base b79c71a78) | 17 | 15 | 17 | 1 | 3 cures |
| certificate-agrees: match +1.79, below −3.88 only (base 0c1a0ae3b) | 11 | 1 (Almost Famous, right) | 0 | 1 | 1 switch not accepted |

- *Release losses:* new and festival films TMDB dates no release of in the venue's country: UK "ODEON Pride Nights -
  We Will Find Happiness" ×14, DE "Rodychi", PL "Lalka" 2026, UK "Short Cuts (35mm)". Its gains are DE "The Killer" ×3
  (1989) and Notre-Dame ×2 cured.
- *Certificate-agrees gains:* Moana 2026, E.T., Doctor Zhivago, LOTR, Caravaggio, The Enforcer 1951.
- *Certificate-agrees new wrong:* Hyde Park's double bill "The Tiger Who Came for Tea and We're Going on a Bear Hunt" →
  The Tiger Who Came to Tea, 36.6% → 49%.
- *Certificate-agrees switch:* Ciné Lumière's "De Gaulle 1: Resistance" moves from 2026 to Resistance (2020).
- *Full certificate table only:* "near" and "above" cost "Home Alone with Wine" (18 at the venue against PG) and
  "Kissing Jessica Stein (25th Anniversary)".

**Why it stopped.**

- Release as a weight trades right for right, and double-counts what the veto and the country measure already read.
- Certificate agreement is the one real gain, 11 right with nothing lost. It stays blocked by one double-bill take
  and one switch.

**Retried with the bill guards (base 75a28ea41).** A certificate of a listing that `MultiFilmBill.billsTwoWorks` (the
`notADoubleBill` gate) or `MultiFilmBill.marker` names now reads `missing:bill`, pinned by `CertificatesSpec`. The
measure did not move:

- 11 right gained, 0 right lost.
- 1 unjudged gain: Kino KILN's "Almost Famous", which looks right.
- The same new wrong and the same switch.

None of the existing bill reads catches Hyde Park's title, which joins its two works with "and". title-trace shows
marker none and billsSeveral no, so the guard cannot reach that take. The switch comes from a learned listing-listing
cannot-link: it now keeps "De Gaulle 1: Resistance" apart from "De Gaulle Part 1: Résistance" [2026] {Baudry}, and the
listing pools to Resistance (2020). That reads as wrong, and it is not judged.

**The switch was the pooled reading, not a listing-listing cannot-link.** `listingListing` holds no certificate. The
switch came from `FamilyScope.pooled`, which reads a cluster as its lead listing and so carried the lead's certificate
into the pooled scores. That lifted Resistance (2020) to a tie with the 2026 film. Now the pooled reading states no
certificate. The fix is pinned by an identity-slice case, "De Gaulle 1: Resistance" judged right for 1165366, which
fails before the fix and passes after.

Re-measured (base 75a28ea41):

- 9 right gained, 0 right lost.
- 1 unjudged gain, Almost Famous, now judged right.
- The switch is gone.
- The one new wrong is still Hyde Park's "The Tiger Who Came for Tea and We're Going on a Bear Hunt".

**Blocked.** An "and"/"&" bill read the way `Acceptance.slashesTwoWorks` reads a slash (each half exactly titles a
distinct eligible candidate, and no candidate is titled as the whole) cannot catch that billing. Its first half
misspells the film ("…Came **for** Tea" against "…Came **to** Tea"), so no candidate is named exactly. Catching it needs a
near-spelling half, which is a fuzzier rule than the exact one asked for.

**LANDED, narrowed.** The certificate counts only when the title names the film as a WHOLE title (exact, original or
alternative relation: `IdentityMeasures.CertifiedRelations`). Beside a film named by a piece of the billing
(segment, fragment, decorated, overlap) it reads `missing:piece`, because the certificate rates the programme there.
No "and"-bill rule, and MultiFilmBill is untouched. Also landed:

- The pooled reading carries no certificate.
- `match` +1.79 and `below` −3.88; `near` and `above` stay neutral.
- The listing's certificate is inside its equality and hash, so a moved certificate re-resolves.
- Release, poster and synopsis stay on the tag.

Measured against main 102dca355 (identity-measure-local): 2 right gained (DE "The Enforcer" → 1951, UK "Ghosts - The
Possesion of Button House (PG)"), 0 lost, 0 new wrong, 0 unjudged, final stage 0. LANDABLE. Most of the 11 earlier
gains named their film by a piece ("Moana (Live Action) - Films For A Fiver", "Spooked: E.T. …") and are given up with
the Tiger bill.

**Next angle:** poster and synopsis as WEIGHTS in the unified stage (`UnifiedEvidence`), where their answers already
exist, rather than per candidate.
