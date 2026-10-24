# A stable cluster identity (design, 2026-10-24; NOT implemented, one decision open)

## The problem

Everything the identity system keeps per CLUSTER is keyed by the cluster's alphabetically first listing,
`StoredFamily.idOf(members)` (`ListingKey.serialised(members.min)`):

| Kept per cluster | Where | Keyed by |
|---|---|---|
| the agreement's verdict, its correction (`correction|<id>`) | `AgreementStage.held`, `identity_agreements` (`MongoAgreementVerdicts`, `_id`) | `idOf` |
| the stage's waits, checks, settle memory, inline asks | `waiting`, `checked`, `unreached`, `askedNow`, `candidateFilms`, `correctionWaits` | `idOf` |
| what the drift check forgets on adopt | `IdentityDriftCheck.Adopt` → `AgreementStage.forget` | `idOf` |
| staging rows | `IdentityStagingPublisher.keyOf` (`identity_staging`) | hash of `idOf` |
| the model's stored families | `IncrementalResolver` (`storeId`) | `idOf` (out of scope here) |

The key MOVES when a listing that sorts first arrives (UK's "A…" venues, PL's Bajka), when the first listing leaves the
schedule, or when clusters merge — none of which need change the cluster's take. What a re-key costs today:

- **The verdict itself**: little. A verdict stands only while its listings' digest (`StoredVerdict.listings`) is
  unchanged, and every re-key above changes the membership, so the verdict is decided again either way. Re-keying
  discards nothing a digest check would have kept.
- **The settle memory** (`unreached`): a settled cluster that is re-keyed loses it, so "a cluster that settled before
  settles again rather than wait anew" (`resolved`, `settling`) no longer holds — it WAITS ANEW on questions it had
  already settled past, and its take falls back to the model's while it waits.
- **`askedNow`**: a re-keyed cluster that also passes `newCluster` (nothing held under the new key) is asked inline a
  second time.
- **Staging rows**: the old key's row resolves as `regrouped` and a new row starts with `since` reset — the screen
  shows a regroup that is not one, and loses how long the cluster has waited.
- **Stored correction fallback**: `held.get(key).filter(_.listings == digest)` cannot match after a re-key (the digest
  moved too), so this is the verdict case again: nothing extra is lost.

So the real losses are the per-cluster MEMORIES (settle, inline ask, wait start), not the verdicts.

## The design

### The key: a cluster counter, assigned by overlap, kept by the stage

A cluster's identity is an opaque `Long` from a counter that is never reused, assigned by OVERLAP with the clusters
the last apply keyed — exactly `IdAssigner`'s rule, which the projection's FilmIds already live by:

- a MERGE keeps the OLDER key (the smaller counter);
- a SPLIT keeps the key on the LARGER half (equal halves: the half holding the smallest listing); the other half gets a
  fresh key;
- a cluster overlapping none gets a fresh key, in order of its smallest listing;
- a key no cluster overlaps is retired, never handed out again.

Why the stage's own counter, not the projection's FilmId:

1. **Order.** The stage runs inside the projection BEFORE the projection assigns ids (`IdentityProjection.planned`:
   agreement, then `live.update`, then `IdAssigner` in `ProjectionDrafting`). A FilmId is known only for a cluster the
   previous projection wrote; a new cluster, and the smaller half of a split, would key provisionally until the next
   written projection gave it a counter — a second re-key, one tick later, of exactly the clusters that matter most
   (new ones, asked inline).
2. **One film is not one cluster.** The projection joins every cluster of one TMDB film into one film
   (`ClusterId.Matched(film)`); the stage reads each model decision on its own (a correction per model take). Two
   decisions of one TMDB film would share a FilmId, and their corrections one key.
3. **Unmatched clusters are 1:1 with films** (`ClusterId.Unmatched`), so for the agreement's own clusters the two
   counters would agree in all but numbering; nothing is gained by sharing one.

The assignment runs in `AgreementStage.applied` (never on a quiet apply, which hands back the last result), over the
decisions it is handed, against the last apply's `(key, members)`; each decision's key is kept by identity
(`IdentityHashMap`, as `digested` is), so `waitingOn`, the publisher and the drift check read a decision's key with no
allocation. The memberships it overlaps against are the decisions' own `members`, held already.

### Persistence

The counter and each live cluster's key must outlive a restart, or the first apply after a boot re-keys everything:

- `identity_agreement_keys` (per country): one document per live cluster, `{_id: <counter>, first: <idOf>}`, plus the
  counter's high-water mark. `first` is what lets a restart re-attach: on the first apply after a boot, a decision is
  given the stored key whose `first` it holds (its first listing); every other is assigned by overlap as above. This
  is a small collection (one row per cluster, ~10k in the US), written only for keys that moved.
- Membership is NOT stored: after a restart the first apply re-attaches by first listing, and overlap resumes from
  that apply's decisions. A cluster re-keyed by a first listing that arrived while the worker was down is the one case
  a restart still re-keys; it is no worse than today.

### Merge and split

- **Merge** (clusters A < B join): the merged cluster keeps A. A's verdict is decided again (its digest moved). A's
  settle memory and inline-ask mark carry over; B's are RETIRED with B (open question 1 below). Staging: B's row
  resolves as `regrouped`; A's row keeps its `since`.
- **Split** (A splits into A₁ ⊇ larger half, A₂): A₁ keeps A, with A's memories; A₂ is fresh — asked inline as a new
  cluster (`newCluster` holds: nothing is held under its key), its own staging row.

### Verdict migration

`identity_agreements` documents are keyed `_id = idOf` today. Proposed: MIGRATE LAZILY, deciding nothing again.

- The first apply under the new code keys every cluster (by overlap against nothing: every cluster fresh, numbered in
  order of its smallest listing). For each, it looks the verdict up under the new key, then, failing that, under its
  legacy `idOf` (and `correction|<idOf>`); a legacy verdict whose digest still matches is ADOPTED: written under the
  new key, the legacy document deleted in the same `replace`. A digest that does not match is decided again, as today.
- Every verdict the first pass does not adopt belongs to a cluster that no longer exists and is deleted by
  `forgetGone`, as today.
- Cost: no resolve beyond what today's boot does (a verdict with an unchanged digest stands); one bulk write of every
  live verdict under its new id on the first pass after the deploy. The alternative — drop every verdict and decide
  them all again — is the 40 s / 514 MB boot pass of 2026-10-05 (PL) at deploy, and far more on worker-us, which was
  OOM-killed twice this week by a comparable whole-corpus working set.
- The stored `unreached` memory is rebuilt from each verdict's `reads` (`AgreementStage.Unanswered`) at load, as now,
  so it migrates with the verdict.

### What reads the key

- `AgreementStage`: `held`, `checked`, `waiting`, `unreached`, `askedNow`, `candidateFilms`, `correctionWaits`, the
  `correction|<key>` ids, `forget`, and the staging events (`StagingEvent.Waiting/Settled.cluster`).
- `IdentityDriftCheck.Adopt`: forgets the live decisions' keys, read from the live stage (`stage.keyOf(decision)`).
  The drift check's WHOLE resolve keys its fresh stage from nothing; it compares takes by listing, so its keys never
  meet the live ones.
- `IdentityStagingPublisher.keyOf`: the stage's key (hashed short, as now). Rows already in `identity_staging` under
  `idOf` hashes resolve once as `regrouped` at the deploy; resolutions expire in 7 days as ever.
- Model families (`IncrementalResolver.storeId`) keep `idOf`: they are the model's, read by the model alone.

### `scripts/identity-slice` and `OlderRefs`

identity-slice compiles THIS checkout's tool files into every ref it decides under (`scripts/identity-slice-old-ref-test.sh`
holds identity-slice at c2974e3aa, title-trace at e90d2b498). The tool files touched:

- `scripts.IdentitySlice` and `tools.UnmatchedClusters` construct `AgreementStage` and `InMemoryAgreementVerdicts`, and
  `IdentitySlice.storedOf` loads prod verdicts by their stored `_id`. If keying stays INSIDE the stage (a constructor
  parameter with a default, the key store an `AgreementVerdicts`-like seam with an in-memory default), the tool files
  need no new member and compile into old refs unchanged.
- `storedOf` reads a prod mirror's `identity_agreements`: after the deploy its `_id`s are counters. The slice must
  hand the stage both the verdicts and the key store (`identity_agreement_keys`) read from the mirror; under an old
  ref (no key store) the verdicts are keyed by counter its stage cannot look up, so a mirror slice under a pre-change
  ref decides every verdict afresh. That is correct (a slice decides as that build would with nothing kept) but slower;
  `OlderRefs` needs a by-name pass of the key store only where the ref's `AgreementStage` has the parameter
  (`OlderRefs.passing`, as `NewerLookups` is passed).
- The old-ref test gains no new floor: nothing the tool files read is newer than c2974e3aa unless the key store is
  passed, and that goes through `OlderRefs`.

### Landing

The riskiest of the three steps: it touches what every verdict is looked up by. It lands only on
`scripts/identity-landing-check --full` LANDABLE with 0 moved — keying alone moves no take (a verdict stands exactly
while its digest does, under any key), so any moved take is a bug in the adoption.

## Open questions (the owner's)

1. **What a MERGE does with the younger cluster's memories.** Proposed: retired with its key (the merged cluster's
   verdict is decided again anyway; its settle memory is the older cluster's). Alternative: union both clusters'
   `unreached` and inline-ask marks into the survivor, so a merge never makes a settled half wait anew.
2. **Verdict migration: adopt lazily, or decide every verdict again at the deploy?** Proposed: adopt (above): no
   resolve, no take can move, one bulk rewrite. Deciding again is simpler code but is the boot pass that OOM-kills
   worker-us.
3. **The stage's own counter, or the projection's FilmId?** Proposed: the stage's own (above, "Why"). Keying by FilmId
   means a provisional key for new clusters and split halves until the next written projection, and a shared key for
   two decisions of one TMDB film.
