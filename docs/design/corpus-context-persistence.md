# Persisting the worker's corpus context: a design, measured

Status: **proposal, waiting on the owner's approval** (2026-11-21). Nothing here has landed. The prototype and the probe
are on branch `design/corpus-persist` (commit `48d86d1ff`, worktree `../movies-corpus-persist`).

## The problem

When a worker boots with nothing changed, the take-up (`IncrementalResolver.restore`) re-resolves no family, but it
still rebuilds the corpus context, `LiveCorpus.seen` of every held listing. The partial re-resolve (5ea0c4f29) and the
stamp carry (a7fb46042) removed the re-resolves. They did not touch this rebuild, which is now almost the whole cost of
an unchanged boot. The brief put it at about 11.8 of 11.9 GB, and said that about half of that was decoding every
candidate film's record.

Three options were put forward:

1. persist a digest per film, so unchanged records skip decoding;
2. persist the built context itself, as a snapshot keyed by the stamps and the corpus version;
3. anything better, such as decoding lazily or decoding fewer fields.

## How it was measured

- **Countries:** `services.identity.CorpusBootProbe` (prototype branch, worker test tree) runs each country's take-up
  over the **local prod mirror** (read-only).
- **Lookups:** it uses production's lookup stack, `ProdLookupInputs.lookups`: `StoredFirstLookups` over
  `StoredTmdbLookups` over a `CachedTmdbDocuments` over the mirror's `tmdb_*` collections. Every write is dropped.
- **Cold cache per pass:** each pass gets a fresh document cache, as a booting process starts with an empty one.
- **Passes:** first a cold take-up fills an in-memory model store. Then come two unchanged take-ups under the same
  rules, and `LiveCorpus.seen` measured alone.
- **Bytes** are the whole JVM's (`getTotalThreadAllocatedBytes`), because the Mongo driver decodes on its own threads.
  The JVM ran with ParallelGC, `-XX:AutoBoxCacheMax=16384` and `-Xmx7-9g`.
- **Retained heap** is the heap after a GC with one built context held.
- **Allocation sites** come from JFR `profile` (`jdk.ObjectAllocationSample`) over three `seen` passes on DE, before
  and after the prototype.
- **Slices:** `scripts/identity-boot-cost` was run as well, over the 90 pinned slices.
- **Noise:** the laptop's load average was 26-40 throughout, so wall times are noisy (±30%). Bytes repeat to within 1%
  pass to pass.

## What the rebuild actually spends

Before any change, an unchanged take-up of each country cost the following. The `seen` column is `LiveCorpus.seen`
alone.

| country | listings | context nodes | unchanged take-up | `seen` | share | context retained |
|---|---:|---:|---:|---:|---:|---:|
| PL | 10,256 | 3,471 | 3.55 GB | 3.49 GB | 98% | 74-86 MB |
| UK | 29,623 | 4,713 | 6.40 GB | 6.25 GB | 98% | 121 MB |
| DE | 29,846 | 6,958 | 8.90 GB | 8.71 GB | 98% | 155 MB |
| US | 79,791 | 1,995 | 7.38 GB | 7.14 GB | 97% | 140 MB |
| ES | 5,821 | 437 | 1.53 GB | 1.50 GB | 98% | 31 MB |
| **total** | | | **27.8 GB** | **27.1 GB** | | **~525 MB** |

The brief's ratio holds: the context build is 97-98% of an unchanged boot. Its explanation does not hold.

DE's `seen`, by allocation site:

| what | share of `seen` | per pass |
|---|---:|---:|
| `NonMovieEventClassifier.isBroadcastLabel` | **60%** | 4.95 GB |
| the rest of the billings (`StageRelays.billings` over each node × film) | 4% | 0.30 GB |
| documents read by the prefetch (`StoredTmdbLookups.prefetch`: Mongo, cache encode and decode, hits) | 10% | 0.86 GB |
| node queries, node keys, walks (`CandidateQueries.of`, `CandidateGeneration.nodeKey`, `ownWalk`) | 10% | 0.9 GB |
| **film record decoding** (`IdentityAnswerBson.filmOf`, `withRuntimes`) | **4-5%** | 0.4 GB |
| the rest (evidence, refile, candidates, houses, venue titles) | ~11% | 0.9 GB |

Decoding the candidate records is about 5%, not half. The cost is one line:
`isBroadcastLabel(piece)` runs once for every billed work of every candidate film
(`Film.billedWorksBesideLabels`), and each call splits **every** broadcast marker into words again with a fresh
`String.split` regex. That is 98% of `billingsByFilm` and 57% of everything the context allocates.

The built context is small: 31-155 MB retained, against 1.5-8.7 GB allocated to build it. It is 40-60 times as
much garbage as result. **The cost is wasted work, not unavoidable decoding.**

## Option 3a: split the markers once (prototyped and measured)

The change is in `common/src/main/scala/services/titles/NonMovieEventClassifier.scala`. The markers' word sequences
are computed once, as a lazy `Set[Seq[String]]`. The regex is compiled once. `isBroadcastLabel` becomes
`own.nonEmpty && BroadcastLabels.contains(own)`. The answer is the same for every input, because
`exists(_ == own)` over a list is `contains` over its set.

### Measured: unchanged take-up, before and after

| country | before (GB / s) | after (GB / s) | change |
|---|---:|---:|---:|
| PL | 3.55 / 2.6-10 | **1.65** / 2.6 | -54% |
| UK | 6.40 / 7-10 | **2.85** / 3.7 | -55% |
| DE | 8.90 / 8.5-9 | **4.13** / 5.0 | -54% |
| US | 7.38 / 5.3 | **3.29** / 4.8 | -55% |
| ES | 1.53 / 1.2 | **0.51** / 1.0 | -67% |
| **total** | **27.8 GB** | **12.4 GB** | **-55%** |

Rules-changed boots and cold resolves fall with it, because every resolve bills films too:

| country | cold take-up before | cold take-up after |
|---|---:|---:|
| PL | 17.9 GB | 12.5 GB |
| UK | 31.5 GB | 21.1 GB |
| DE | 35.6 GB | 22.4 GB |
| US | 37.5 GB | 26.0 GB |
| ES | 4.6 GB | 2.4 GB |

On the 90 pinned slices, `scripts/identity-boot-cost` measured the following (bytes per family):

| phase | before | after | change |
|---|---:|---:|---:|
| resolve | 53.26 MB | 45.90 MB | -13.8% |
| boot | 52.28 MB | 45.05 MB | -13.8% |
| boot-steady | 17.70 MB | 14.95 MB | -15.5% |

The slices are small corpora with few relay films, so the label check weighs less there.

### Its profile

| | |
|---|---|
| **Memory** | Steady-state heap added: one set of about 30 short word lists, a few KB. The retained context is unchanged. |
| **Storage** | 0 bytes. |
| **Write cost** | None. Every scrape's and settle's refresh that bills a film gets cheaper by the same mechanism. |
| **Invalidation and correctness** | Nothing is persisted, so nothing can go stale. It survives any stamp change, codec change or partial write. |
| **Drift risk** | None by construction: the function's value is identical for every input. |
| **Complexity** | +8 / -3 lines, no new collaborator, no coupling edge. `common` already owns the classifier. |

The drift proof:

- `NonMovieEventClassifierSpec` (28) and `LiveCorpusSpec` (3) pass.
- The landing commit adds an equivalence case: the old and new `isBroadcastLabel` agree over every marker and every
  billed work of the pinned slices.
- It adds the `identity-boot-cost --update` baseline fall.
- It runs `identity-what-if` on all five countries. The stamp carry then carries the identity stamps, so a deploy
  causes no re-resolve.

### What is left after it (DE, 3.4 GB a pass)

| what | per pass |
|---|---:|
| the prefetch's document reads | 0.94 GB |
| the remaining billings | 0.65 GB |
| node queries and node keys | 0.69 GB |
| walks | 0.32 GB |
| record decoding (`filmOf`) | 0.27 GB |
| refile | 0.26 GB |

The two cheapest follow-ups are of the same "no new state" kind:

1. On a cold `CachedTmdbDocuments`, every answer read is encoded to bytes to be kept, and every hit is then decoded
   again (`filmHits`, `heldAnswers`). That is 0.78 GB on DE. A read could hand back the document it just read, rather
   than a decode of the bytes it just kept.
2. `StageRelays.billings` intersects each node's billed works with each film's. A node bills against the films it
   reached, and most share no work. An index of the films by billed work would skip the empty pairs.

Each is its own measured lane.

## Option 1: per-film digests

| | |
|---|---|
| **Measured basis** | Record decoding is 0.27-0.4 GB of DE's `seen` (4-8%, before or after 3a). That is the most this option could ever save. In a fresh JVM a digest saves less than that, because the context still needs every candidate's `Film`. `refile` builds `Candidate.of` from it, and `ownTitleKeys`, the season works, the billings and the venue titles all read it at build time. So an unchanged digest still means the film must be decoded. Skipping the decode needs the derived facts persisted too, and that is option 2. |
| **Storage** | About 40 B per film. PL 21k, UK 38k, DE 54k, US 45k and ES 13k films come to about 7 MB in total, written whenever a record is filed. |
| **Memory** | A `LongMap` of digests, about 2-4 MB per country. |
| **Invalidation** | A digest per document, rewritten on each TMDB filing. A partial write leaves the digest and the record out of step, so it needs the two in one document. |
| **Drift risk** | Low, but it pays for almost nothing. |
| **Complexity** | About 150 lines across the TMDB store and the corpus, plus a store field. |

**Verdict: not worth building.** It saves at most about 5% and cannot save the decode in a cold JVM.

## Option 2: a snapshot of the built context

| | |
|---|---|
| **Measured basis** | The context retains 31-155 MB of heap (about 525 MB over the five countries). After 3a its build costs 0.5-4.1 GB per country (12.4 GB in total). |
| **Storage** (estimate) | Encoded as BSON at about 0.4 × the retained heap: PL ~30 MB, UK ~50 MB, DE ~62 MB, US ~56 MB, ES ~12 MB. That is **~210 MB in total**, against the 9.5-208 MB per country `identity_model_families` already holds (477 MB in total). The candidate records alone are 8-30 MB per country (`$bsonSize` of `record`, 585 B each). |
| **Boot** (estimate) | A decode of the snapshot allocates roughly 3-5 × its bytes: PL ~0.1-0.15 GB, DE ~0.2-0.3 GB. Against post-3a DE that saves about 3.8 of 4.1 GB. |
| **Memory** | None at rest, if it is read once at boot and dropped. While it is written, a second encoded copy (up to 62 MB on DE) is held. |
| **Write cost** | This is what decides against it. The context moves on almost every scrape, because a node or a candidate changes. Rewriting the whole snapshot on each change is 12-62 MB per write, at a scrape cadence of minutes: gigabytes of Mongo writes an hour, per country. The alternative is to write only at shutdown (lost on a kill or OOM, which are exactly the boots that matter) or on a timer. A snapshot written on a timer is stale by construction, so a boot must replay what moved since it was taken, and that is the incremental machinery done a second time. Per-key documents (a Mongo mirror of every map in `LiveCorpus`, about 25 of them) avoid the rewrite but become a second persistent model, with every write path doubled. |
| **Invalidation and correctness** | The snapshot is valid only if it equals `CorpusContext.of` over the listings held, as they are answered now. Its key would have to cover the identity stamps, a codec version, a digest of the held listing set, and a version of every TMDB document and venue page it read: a corpus version bumped on every `TmdbStore` and `VenuePageIndex` filing. A stamp or codec change makes it stand for nothing, and the boot rebuilds. A partial write needs a generation id with a manifest written last, and a boot that finds no complete manifest rebuilds. A missed bump does not fail loudly. It boots a context that disagrees with its inputs, and every family's slice digest then reads differently, or worse, the same. |
| **Drift risk** | Real. A snapshot that is stale or wrongly decoded changes the facts families are checked against (`stands`: `corpus.slice(reads).digest`), and so which families re-resolve and what they read. Proving it safe needs: <br>(a) a `LiveCorpusSpec`-style equivalence after every event, comparing the restored context with the built one; <br>(b) `IdentityDrift` replays with a restart taken from a snapshot at every seed; <br>(c) a what-if over all countries; <br>(d) a permanent shadow check in production that rebuilds and compares, which gives the saving back. |
| **Complexity** | About 800-1,200 lines: an encoder and decoder for every map and the node, evidence and candidate types, a corpus-version counter threaded through two stores, the manifest, and the shadow check. It adds a new collaborator, a `CorpusSnapshotStore`, wired into `LiveCorpus` (whose fan-out is at the limit) and `IncrementalResolver`. It adds edges from the identity core to the store layer, whose coupling the ratchet would make us justify. It also lands in `services/corpus`'s neighbourhood, which the `review/worker-core` lane is reviewing. |

**Verdict: not now.** After 3a the most it can still save is 0.5-3.8 GB per boot. It would cost 210 MB of storage,
a write path at scrape cadence and a new class of stale-state drift. It would also rule out the next simple
allocation cuts measuring cleanly.

## Option 3b: lazy or narrower decoding

| | |
|---|---|
| **Lazy records** | Every film is read during the build (`refile`, the billings, `retitle`), so laziness defers nothing at boot. |
| **Fewer fields** | The record decode is 0.27 GB of DE's 3.4 GB. Dropping fields the context never reads (`castPairs`, `certificates`, `releases`) would save a fraction of that. The resolve reads those fields later, so they would have to be decoded on demand, with a cache: complexity for about 3%. |

**Verdict: not worth it** on its own.

## Recommendation

1. **Land option 3a, with its test**, as an ordinary landing after the owner's approval. It cuts every country's
   unchanged boot by 54-67% (27.8 → 12.4 GB in total) and every cold or rules-changed take-up by 30-48%, with no
   state, no storage and no drift risk. It needs an `identity-boot-cost --update` and an all-countries what-if, and
   the stamp carry spares the deploy a re-resolve.
2. **Then take the two allocation follow-ups**, each its own measured lane under `identity-boot-cost` and the probe:
   the cold-cache encode and decode round trip, and the indexed billings. Together they are about 1.4 GB of DE's
   remaining 3.4 GB.
3. **Do not build options 1 or 2.** Option 1 cannot save the decode it targets. Option 2's write cost and
   stale-state drift risk outweigh the 0.5-3.8 GB per boot it could still save after 3a. Revisit option 2 only if the
   boot stays above budget once the follow-ups land. If it is revisited, start from a snapshot written only at
   graceful shutdown with a full-rebuild fallback, and put a shadow comparison before any trust.

The prototype, `CorpusBootProbe` and the classifier change, stays unlanded on `design/corpus-persist` until approval.
