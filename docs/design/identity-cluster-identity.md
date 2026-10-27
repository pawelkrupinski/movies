# A stable cluster identity (design, 2026-10-24; IMPLEMENTED 2026-10-25, the owner's decisions below)

> **Superseded in part, 2026-10-25 ([identity-no-drift.md](identity-no-drift.md)):** under the owner's no-drift
> principle a key only NAMES a cluster. Decision 1 (merge unions memories) and decision 2 (adopt verdicts under the
> key) are withdrawn: a verdict is found by its listings' digest under whichever key filed it, a merge carries no
> memory, and a cluster settles on the question memory alone. The counter, the overlap rule, the key store and the
> restart re-attach stay, for naming (the staging screen, the drift check).

## The problem

Everything the identity system keeps per CLUSTER is keyed by the cluster's alphabetically first listing,
`StoredFamily.idOf(members)` (`ListingKey.serialised(members.min)`):

| Kept per cluster | Where | Keyed by |
|---|---|---|
| the agreement's verdict, its correction (`correction|<id>`) | `AgreementStage.held`, `identity_agreements` (`MongoAgreementVerdicts`, `_id`) | `idOf` |
| the stage's waits, checks, settle memory, inline asks | `waiting`, `checked`, `unreached`, `askedNow`, `candidateFilms`, `correctionWaits` | `idOf` |
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

## Status

Implemented 2026-10-25 as decided below: `StableNames` (the overlap rule, the projection's film ids' too), `AgreementKeys` (`identity_agreement_keys`,
in-memory default), `AgreementStage.keying`/`adopt`/`keyOf`, `StagingEvent.Merged`, and `ProdMirror` reading the mirror's
verdicts through the key store. Specs: `StableNamesSpec`, `AgreementKeyingSpec`. (The staging screen's half,
`StagingEvent.Merged` and `IdentityStagingPublisher`, was retired 2026-10-27.)

The owner's decisions (2026-10-25) on the three open questions:

1. **Merge: UNION** the younger cluster's memories into the survivor (the "Alternative" below, not the "Proposed"), so a
   merge never makes a settled half wait anew: its `unreached`, its inline-ask mark (`askedNow`), and its wait's start
   (the earlier `since` is kept).
2. **Verdict migration: adopt lazily**, no re-resolve at the deploy, as proposed.
3. **Key: the stage's own counter**, not the projection's FilmId, as proposed.

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
  settle memory and inline-ask mark carry over, and B's are UNIONED into them (the owner's decision 1): B's key is
  retired but ABSORBED (`StableNames.Named.absorbed`, by the next cluster holding most of its listings — which
  always kept an older key), its `unreached` added to A's and its `askedNow` mark set on A. Staging: B's row resolves
  as `regrouped`; the stage tells the screen `StagingEvent.Merged(A, B)`, and A's row is staged since the EARLIER of
  its own wait's start and B's (the publisher reads B's from its published row, so this holds across a restart too).
- **Split** (A splits into A₁ ⊇ larger half, A₂): A₁ keeps A, with A's memories; A₂ is fresh — asked inline as a new
  cluster (`newCluster` holds: nothing is held under its key), its own staging row.

### Verdict migration

`identity_agreements` documents were keyed `_id = idOf`. Decided: MIGRATE LAZILY, deciding nothing again.

- The first apply under the new code keys every cluster (by overlap against nothing: every cluster fresh, numbered in
  order of its smallest listing). For each, it looks the verdict up under the new key, then, failing that, under its
  legacy `idOf` (and `correction|<idOf>`); a legacy verdict is ADOPTED: written under the new key, the legacy document
  deleted in the same `replace` (`AgreementStage.adopt`). One whose digest still matches stands, unread; one whose
  digest does not is decided again under its key, as today. The legacy lookup runs on every first apply after a boot,
  so a crash between the key write and the verdict write adopts on the next boot.
- Note: the deploy that brings this in changes `AgreementStage.scala`, so `AgreementStage.RulesVersion` moves and every
  verdict is decided again on its first pass anyway, as on any deploy touching the stage's sources. Adoption keeps
  the verdicts' keyed memory for every later deploy and restart.
- Every verdict the first pass does not adopt belongs to a cluster that no longer exists and is deleted by
  `forgetGone`, as today.
- Cost: no resolve beyond what today's boot does (a verdict with an unchanged digest stands); one bulk write of every
  live verdict under its new id on the first pass after the deploy. The alternative — drop every verdict and decide
  them all again — is the 40 s / 514 MB boot pass of 2026-10-05 (PL) at deploy, and far more on worker-us, which was
  OOM-killed twice this week by a comparable whole-corpus working set.
- The stored `unreached` memory is rebuilt from each verdict's `reads` (`Dependency.Unanswered`) at load, as now,
  so it migrates with the verdict.

### What reads the key

- `AgreementStage`: `held`, `checked`, `waiting`, `unreached`, `askedNow`, `candidateFilms`, `correctionWaits`, the
  `correction|<key>` ids, and the staging events (`StagingEvent.Waiting/Settled.cluster`).
- The worker's drift check (`IdentityDriftCheck`, with its `Adopt` and `AgreementStage.forget`) was deleted. The
  off-worker check, `scripts.IdentityLiveDrift`, keys its fresh stage from nothing and compares takes by listing, so its
  keys never meet the live ones.
- Model families (`IncrementalResolver.storeId`) keep `idOf`: they are the model's, read by the model alone.

### `scripts/identity-slice` and `OlderRefs`

identity-slice compiles THIS checkout's tool files into every ref it decides under (`scripts/identity-slice-old-ref-test.sh`
holds identity-slice at c2974e3aa, title-trace at e90d2b498). The tool files touched:

- `scripts.IdentitySlice` and `tools.UnmatchedClusters` construct `AgreementStage` and `InMemoryAgreementVerdicts`, and
  `IdentitySlice.storedOf` loads prod verdicts by their stored `_id`. If keying stays INSIDE the stage (a constructor
  parameter with a default, the key store an `AgreementVerdicts`-like seam with an in-memory default), the tool files
  need no new member and compile into old refs unchanged.
- `storedOf` reads a prod mirror's `identity_agreements`: after the deploy its `_id`s are counters. IMPLEMENTED more
  simply than proposed: `tools.ProdMirror` (a tool file) reads the mirror's `identity_agreement_keys` raw, by collection
  name, and hands every verdict back under its cluster's FIRST LISTING (`first`), as a pre-change build stored it. A
  pre-change ref's stage reads them as it always did; this build's stage adopts them on its first apply (above). No
  `OlderRefs` member and no key store is passed; `ProdMirror.sides` finds a listing's verdict through the same
  `first` → key lookup. `identity_agreement_keys` is marked mirrored in `services.MongoCollections` so the mirror holds it.
- The old-ref test gains no new floor: the tool files name nothing newer than c2974e3aa.

### Landing

The riskiest of the three steps: it touches what every verdict is looked up by. It lands only on
`scripts/identity-landing-check --full` LANDABLE with 0 moved — keying alone moves no take (a verdict stands exactly
while its digest does, under any key), so any moved take is a bug in the adoption.

## Open questions (the owner's) — DECIDED 2026-10-25 (see Status)

1. **What a MERGE does with the younger cluster's memories.** DECIDED: union (the alternative). Proposed: retired with its key (the merged cluster's
   verdict is decided again anyway; its settle memory is the older cluster's). Alternative: union both clusters'
   `unreached` and inline-ask marks into the survivor, so a merge never makes a settled half wait anew.
2. **Verdict migration: adopt lazily, or decide every verdict again at the deploy?** DECIDED: adopt. Proposed: adopt (above): no
   resolve, no take can move, one bulk rewrite. Deciding again is simpler code but is the boot pass that OOM-kills
   worker-us.
3. **The stage's own counter, or the projection's FilmId?** DECIDED: the stage's own. Proposed: the stage's own (above, "Why"). Keying by FilmId
   means a provisional key for new clusters and split halves until the next written projection, and a shared key for
   two decisions of one TMDB film.
