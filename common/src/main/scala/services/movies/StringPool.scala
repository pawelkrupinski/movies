package services.movies

import com.github.benmanes.caffeine.cache.Caffeine

/** Interns short, heavily-repeated `SourceData` strings — synopses, cast/director names,
 *  countries, genres, and the poster / film-page / trailer URLs — so a film shown at N
 *  cinemas doesn't hold N byte-identical copies
 *  of the same value across its per-cinema slots. Low-cardinality tokens especially win:
 *  a country or genre recurs in thousands of slots corpus-wide yet collapses to ONE
 *  instance. Interning happens at the single write boundary (`MovieCache.buildCinemaSlot`);
 *  the prior-slot carry-forward already holds interned instances, so only fresh values
 *  need it.
 *
 *  Bounded (a plain `ConcurrentHashMap` would retain every string a film ever had,
 *  forever — the unbounded-growth trap that caused the original heap creep) so strings
 *  from films that left the listings are evicted. Not `String.intern()` — that pins text
 *  in native memory with no eviction. Sized well above the corpus's distinct working set
 *  (~6-7k synopses + the cast/director/country/genre token vocabulary + ~4k distinct
 *  poster/film-page URLs). Only LOW-CARDINALITY values belong here — pooling a
 *  per-screening value such as `Showtime.bookingUrl` (116k distinct in the UK corpus
 *  alone) would evict the whole vocabulary and save almost nothing. */
object StringPool {

  /** The pool's ceiling, named so a deployment spec can assert it and
   *  `kinowo_worker_string_pool_max_entries` can publish it.
   *
   *  THE BOUND FAILS SILENTLY, which is why there is a gauge beside it: past the
   *  maximum Caffeine evicts, the next lookup of an evicted value allocates a fresh
   *  String, and interning degrades into a no-op that still costs a hash per call.
   *  Nothing logs, nothing errors -- the heap just grows.
   *
   *  MEASURED 2026-09-03, DO NOT RAISE IT: the pooled vocabulary is 28,695 distinct
   *  values on the US corpus (121,236 slots) and 26,415 on the UK's (38,666) -- 22%
   *  and 20% of this cap. A 2026-08-30 note called the cap "a US-scale precaution";
   *  the US has now been counted and the precaution was unnecessary, because THE
   *  VOCABULARY SATURATES. US carries 3x the UK's slots and barely more distinct
   *  strings, since extra slots are extra showings of films already pooled.
   *  Duplication factors bear it out: genres 4,395x (53 distinct across 232,953
   *  elements), ageRating 8,532x (7 distinct), director 69x, cast 60x.
   *
   *  So heap duplication is NOT this cap overflowing. It is the paths that never
   *  reach the pool -- interning happens only at `MovieCache.buildCinemaSlot`, so
   *  anything rehydrated through `MovieCodecs` decode, plus `Showtime.format` and
   *  `CinemaShowing.titleKey`, arrives as fresh instances. Raising this number would
   *  cost memory and change nothing. */
  val MaxEntries: Long = 131072L

  private val pool = Caffeine.newBuilder()
    .maximumSize(MaxEntries)
    // Occupancy and evictions are the only way to tell a working pool from a
    // thrashing one; Caffeine keeps these on LongAdders, so the cost is a counter
    // bump per lookup against an allocation saved.
    .recordStats()
    .build[String, String]()

  /** The canonical instance for a string: the first equal value interned wins, so all
   *  byte-identical values across the corpus share one object. */
  def canonical(s: String): String = pool.get(s, (k: String) => k)

  /** Intern every element of a list (cast, genres, …), preserving order. */
  def canonicalAll(xs: Seq[String]): Seq[String] = if (xs.isEmpty) xs else xs.map(canonical)

  /** Distinct strings held right now. Caffeine's estimate, which is what a gauge
   *  wants — forcing `cleanUp()` for exactness would make a scrape do the pool's
   *  maintenance work. */
  def heldEntries: Long = pool.estimatedSize()

  /** Strings evicted since boot. Zero is a pool that fits its corpus. */
  def evictions: Long = pool.stats().evictionCount()

  /** Share of lookups served an instance already held. An idle pool has missed
   *  nothing, so it reads 1.0 rather than Caffeine's NaN for 0/0 — a gauge that
   *  goes NaN at boot reads as a broken exporter. */
  def hitRate: Double = {
    val stats = pool.stats()
    if (stats.requestCount() == 0L) 1.0 else stats.hitRate()
  }
}
