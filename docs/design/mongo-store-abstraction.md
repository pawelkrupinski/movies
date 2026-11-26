# One keyed store over Mongo and memory

Owner decision, 2026-11-26 (Scala consolidation item E): "Design a store abstraction covering bulk writes, TTL and
counters, then migrate the 8 stores ... needs `itAll` plus careful tests on each store".

`tools.KeyedDocStore` (2026-10-19) already holds five small stores: string key, whole-document replace, scan, retention
sweep. Eight more Mongo stores and their hand-written in-memory twins did not fit it. Each of them repeats the same
`Await`/`Filters.eq("_id")`/upsert boilerplate in its Mongo class and copies the store's rules into its fake.

## The eight pairs, and what each does

| store (trait) | module | key | read | write | bulk | TTL / expiry | counters | other |
|---|---|---|---|---|---|---|---|---|
| `ScheduledRunStore` | worker | occurrence id (text) | -- | insert-once (dup key = lost claim) | -- | 48 h on `claimedAt` | -- | in-memory twin in `src/test` |
| `AuthExchangeCodeStore` | web | code (text) | take (`findOneAndDelete`) | insert | -- | 2 min + 60 s on `issuedAt` | -- | `Option[db]`, failures logged, 5 s timeout |
| `ReviewAnswerStore` | web | `<epoch ms>-<seq>-<rand>` (text, ordered) | ordered keyset scan | insert | -- | -- | -- | local mirror DB only |
| `PendingListingStore` | worker | serialised `ListingKey` | read all | upsert | upsert many + `deleteMany` | -- | -- | |
| `PinStore` | identity | pin content id | read all | upsert | -- | -- | -- | delete says whether held |
| `ScrapeCostStore` | worker | dedup key | keyset scan | `$push` + `$slice` (capped array) | -- | -- | -- | bookkeeping write concern |
| `RetainedListingStore` | worker | serialised `ListingKey` | read live rows (`expiredAt: null`) | upsert + marker doc | bulk upsert, `updateMany` soft expiry | soft (marked, never deleted) | -- | compound index, clock |
| `FilmIdCounterStore` | identity | film id | read all, undecodable rows reserved | `insertMany` unordered | yes | -- | the counter: unique index | refuses, never overwrites |

Near the edge, catalogued but not in the eight: `OmdbBudget` (a day's conditional `$inc` below a ceiling -- the shape
`StoreCounters` is cut for, migrated after the eight), `VenuePageStore` (field-wise `$set`/`$unset` and a `landed`
sub-document kept across a re-read: a partial-update store, not keyed values), `FacebookRescrapeStore` (a queue:
claim by `findOneAndUpdate` sort, pipelines), `IdentityModelStore` / `IdentityTraceStore` / `TmdbDocuments`
(digest-skipping writes through `DocumentDigest.writeMoved`, projections, a coalescing writer thread).

## The interfaces

All in `tools` (common), beside `KeyedDocStore`:

```scala
trait StoreReader[K, V]   { get(key); getAll(keys); scan(each: Seq[(K, V)] => Unit): ScanOutcome; final all() }
trait StoreWriter[K, V]   { put(key, value); delete(key): Boolean }
trait StoreOnce[K, V]     { insert(key, value): Boolean; take(key): Option[V] }      // claims, single-use codes
trait StoreBulk[K, V]     { putAll(values); deleteAll(keys): Int }
trait StoreCounters[K]    { increment(key, field, by = 1, below = None): Option[Long]; counted(key, field): Long }

trait KeyCodec[K]         { id(key): BsonValue; key(id): Option[K] }      // KeyCodec.Text, KeyCodec.Number (Long)
trait DocCodec[V]         // the existing value codec: fields beside `_id`, and back

final class KeyedStore[K, V](documents: StoreDocuments, keys: KeyCodec[K], values: DocCodec[V])
  extends StoreReader[K, V] with StoreWriter[K, V] with StoreOnce[K, V] with StoreBulk[K, V] with StoreCounters[K]
```

A store above it takes only the interfaces it uses as its constructor parameter (`KeyedScheduledRunStore(claims:
StoreOnce[String, Instant], clock)`); its `Mongo*` and `InMemory*` classes are constructors that hand it a `KeyedStore`
over Mongo's or memory's documents. The store's public trait does not change, so no caller does.

**Expiring** is not an interface a caller calls: it is a property of the documents, `DocumentExpiry(field, after)`,
given to either implementation at construction.

## One rule set, two storage boundaries

`StoreDocuments` is the seam (`writing-fakes`): whole BSON documents by `_id` -- `find`, `scan`, `replace` (bulk upsert),
`insert` (false on a held `_id`), `take` (find-and-delete), `delete`, `increment`. Two implementations, and `Absent`
(no database: holds nothing):

- `MongoStoreDocuments`: `_id in` lookups and bulk writes in batches of 500, `_id`-keyset scan in pages of 2,000 with
  retries (`KeysetScan`; a non-text `_id` carried between pages as its extended JSON), every write counted by
  `MongoWriteErrors` (a duplicate key alone is not a failure), the TTL index made off-thread.
- `InMemoryStoreDocuments(clock, expiry)`: each document kept as its BSON bytes through `BsonRoundTrip` (a document
  Mongo would refuse throws here too), in Mongo's `_id` order (numbers by value before strings, strings by UTF-8 bytes).

Everything else is `KeyedStore`'s, once: `_id` from the key codec, the value's fields from the value codec, a document
whose `_id` or value does not decode is no value (one bad row never fails a read), a counter is a number field of the
key's document, 0 until raised.

### TTL semantics

A document lives `after` past the date in `field`, then is DELETED -- by Mongo's TTL monitor (about once a minute, so up
to a minute late), in memory at the first call whose injected clock reads past it. Neither hides a document before it
is deleted: a caller needing an exact cutoff reads the stamp itself (`AuthExchangeCodes.redeem` refuses a code past 2
minutes; its TTL is the sweep). A document without a date in the field never expires, as Mongo's monitor skips it.
Specs pin `MongoTtlSpecClock.Pinned` (`NoTtlExpiredRowsInSpecsSpec`). Soft expiry (`RetainedListingStore`: marked,
screenings stripped, never deleted) is that store's rule, not a TTL.

### Counter semantics

`increment(key, field, by, below)` raises the field in one server step and returns its new value; with a ceiling it
returns None and raises nothing when the field already stands at `below` or more. With a ceiling the document is made
first (`$setOnInsert` 0) and raised by a conditional update that never upserts -- an upsert whose filter reads the
field meets a document at the ceiling as a duplicate key, indistinguishable from a failure (`MongoOmdbBudget`'s
lesson). Racing callers each count; none passes the ceiling (contract-tested with 20 racers).

## Tests

- `worker/src/it/scala/contracts/KeyedStoreContractSpec.scala`: ONE suite over `MongoStoreDocuments` (a real Mongo, an
  isolated database dropped after) and `InMemoryStoreDocuments` -- get, last put wins, lookups, multi-page `_id`-order
  scan with number keys, undecodable rows skipped, delete and bulk delete counts, insert-once and take-once (and 8
  racers each), counters with and without a ceiling (20 racers), the TTL index.
- `worker/src/test/scala/tools/InMemoryStoreDocumentsSpec.scala`: expiry on the injected clock, an expired `_id` freed
  for a new insert, Mongo's `_id` order.
- Per store, BEFORE its migration: a characterisation of its current behaviour on both implementations (an existing
  `it` spec extended, or a new one), kept after it, so the migration is proved to change nothing a caller sees.

## Migration order (lowest risk first, one store per commit)

1. `ScheduledRunStore` -- insert-once + TTL, no reads, worker only. DONE first: the fake now models the 48 h TTL.
2. `AuthExchangeCodeStore` -- DONE 2026-11-27: insert + take + TTL, `Absent` for no database; web only, failures logged as now. Its
   in-memory store is built without a clock by ~10 specs: each must hand it the `AuthExchangeCodes` clock, or a code
   minted on a pinned past instant expires on arrival.
3. `ReviewAnswerStore` -- DONE 2026-11-27 (the fake now reads in answer-time order, as Mongo did): insert + ordered scan, local-mirror only (no prod data).
4. `PendingListingStore` -- `putAll` + `deleteAll` + read all. DONE: Mongo now reads in key order, as the fake did.
5. `ScrapeCostStore` -- DONE 2026-11-27 (`StoreDocuments.append`, `StoreCappedLists`): needed one more boundary operation, a capped append (`$push` + `$slice`), added with its
   contract cases; or a read-modify-`put`, which loses the server-side atomicity two workers rely on. Prefer the former.
6. `PinStore` -- DONE 2026-11-27, stamps unchanged: identity module: check the identity stamps (`scripts/pure-move-check`; `MongoPinStore` is outside the
   rules closure if the stamp digests do not move) before landing.
7. `RetainedListingStore` -- DONE 2026-11-27 (`StoreMarks`: `scanAbsent` / `editAbsent`; Mongo now reads in key order): bulk + its soft-expiry rule kept in the store above (an `updateMany` on the live filter
   is a further boundary operation, `markWhere`), the marker document, its index. Read-model input: what-if if the
   identity closure is reached.
8. `FilmIdCounterStore` -- DONE 2026-11-27 (`StoreOnce.insertAll`, a unique field): `insert` per entry (or an unordered `insertAll` returning the count landed) and a unique
   secondary index; identity module, stamps as for 6.

`OmdbBudget` is on `StoreCounters` (2026-11-27; `markSpent` is an increment to the limit, not `$max`). Last: `KeyedDocStore` itself folded in (`KeyedDocStore[T]` is a
`KeyedStore[String, T]` plus `putDetached` and `retention`): its five users move, and the two abstractions become one.

## Coupling

`scripts/coupling-report --simulate` (2026-11-26) of a separate `tools.store` package, with `KeyedDocStore` and its codec
moved in: 9 types, A 0.38, I 0.30, **D 0.33 -- past the 0.20 limit for a package of 8+ types**, and the new classes
alone (no users yet) at I 1.00. So the abstraction lives in `tools` itself, the foundation package (concrete and stable
on purpose, an exemption category): there it adds abstract collaborators, which lowers `tools`' D (0.74, A 0.26), adds no
package edge (each migrated store's package already uses `tools`) and no cycle. Every class's fan-out stays far under
9 (`KeyedStore` 1: its documents; `MongoStoreDocuments` the collection and `tools` objects).

## Risks

- **TTL drift**: the Mongo TTL index is made with `createIndex`, which never alters an existing one. Every migrated
  store keeps its field and seconds exactly (checked by its it spec); a future change of expiry goes through
  `MongoTtlIndex.reconcile`.
- **Write-error metrics**: every write now goes through `MongoWriteErrors.counted`, which seeds the collection's series
  at 0 on its first write -- new series for stores that did not count before (`scheduled_runs`), never a new alert.
- **Ordering**: a store reading "all" now reads in `_id` order through a keyset scan where it read one unbounded
  `find()` (natural order); a store whose caller depends on that order must say so in its characterisation test.
- **Identity stamps**: the identity-module stores (`PinStore`, `FilmIdCounterStore`) may sit in the identity rules'
  source closure; a migration there is checked with `scripts/pure-move-check` and, if a stamp moves, carried or
  accepted as a re-resolve, as `agentctl land` does.
- **Timeouts**: each store keeps its own round-trip bound (`MongoStoreDocuments` `timeout`; the auth codes' 5 s).
