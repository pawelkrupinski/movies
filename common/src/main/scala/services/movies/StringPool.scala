package services.movies


/** Interns short, heavily-repeated `SourceData` strings — synopses, cast/director names,
 *  countries, genres, and the poster / film-page / trailer URLs — so a film shown at N
 *  cinemas doesn't hold N byte-identical copies
 *  of the same value across its per-cinema slots. Low-cardinality tokens especially win:
 *  a country or genre recurs in thousands of slots corpus-wide yet collapses to ONE
 *  instance. Interning happens wherever a slot enters memory: a scrape's
 *  (`ScrapeLanding.buildCinemaSlot`) and the cache's read from the store ([[slot]]).
 *
 *  Bounded (a plain `ConcurrentHashMap` would retain every string a film ever had,
 *  forever — the unbounded-growth trap that caused the original heap creep) so strings
 *  from films that left the listings are evicted. Not `String.intern()` — that pins text
 *  in native memory with no eviction. Sized well above the corpus's distinct working set
 *  (~6-7k synopses + the cast/director/country/genre token vocabulary + ~4k distinct
 *  poster/film-page URLs). Only LOW-CARDINALITY values belong here — pooling a
 *  per-screening value such as `Showtime.bookingUrl` (116k distinct in the UK corpus
 *  alone) would evict the whole vocabulary and save almost nothing.
 *
 *  An instance, never a global: the worker's metrics bundle owns ONE for the process and
 *  its composition root hands that one to every country's cache, so a string Poland
 *  interned is the instance Germany gets back and the gauges read the pool the scrapes
 *  actually fill. Anything else (a spec, a lone cache) gets a pool of its own. */
final class StringPool {

  private val pool = tools.BoundedCache.ofSize(StringPool.MaxEntries)
    // Occupancy and evictions are the only way to tell a working pool from a
    // thrashing one; Caffeine keeps these on LongAdders, so the cost is a counter
    // bump per lookup against an allocation saved.
    .recordStats()
    .build[String, Some[String]]()

  // A film's cast, genres, directors and countries are one list at every venue that shows it, but
  // each venue's slot held its own copy: ~985k list cells on the US worker (~24 MB of `::`, live
  // heap 2026-10-01). Equal lists of interned strings share one instance through this.
  private val lists = tools.BoundedCache.ofSize(StringPool.MaxEntries)
    .build[Seq[String], Seq[String]]()

  /** The canonical instance for a string: the first equal value interned wins, so all
   *  byte-identical values across the corpus share one object. */
  def canonical(s: String): String = canonicalSome(s).value

  /** The canonical `Some` of a string: a slot's optional text shares its WRAPPER as well as its
   *  text. Every present Option field is its own `Some` otherwise — ~25 MB of them on the US
   *  worker's live heap (dump 2026-09-29), though most wrap text other slots of the film share. */
  def canonicalSome(s: String): Some[String] = pool.get(s, (k: String) => Some(k))
  def canonical(o: Option[String]): Option[String] = o match {
    case Some(s) => canonicalSome(s)
    case None    => None
  }

  /** Intern every element of a list (cast, genres, …), preserving order. */
  def canonicalAll(xs: Seq[String]): Seq[String] =
    if (xs.isEmpty) xs else { val canon = xs.map(canonical); lists.get(canon, (k: Seq[String]) => k) }

  /** A slot with its low-cardinality text interned — how a slot read back from the store (boot
   *  hydrate, the change stream, a rehydrate) joins the scrape path's instances. Without it nearly
   *  every cached slot kept its own copies: worker-uk held 1.23M duplicate String objects (37 MB)
   *  while this pool held 3,916 strings. Not the film-page URL — one per cinema slot, so it shares
   *  nothing and would crowd the vocabulary out (US holds ~100k slots) — nor the showtimes and title
   *  searches, per-screening values. */
  def slot(sd: models.SourceData): models.SourceData = sd.copy(
    title          = canonical(sd.title),
    rawTitle       = canonical(sd.rawTitle),
    originalTitle  = canonical(sd.originalTitle),
    englishTitle   = canonical(sd.englishTitle),
    synopsis       = canonical(sd.synopsis),
    cast           = canonicalAll(sd.cast),
    director       = canonicalAll(sd.director),
    countries      = canonicalAll(sd.countries),
    genres         = canonicalAll(sd.genres),
    posterUrl      = canonical(sd.posterUrl),
    trailerUrl     = canonical(sd.trailerUrl),
    language       = canonical(sd.language),
    ageRating      = canonical(sd.ageRating),
    runtimeMinutes = StringPool.small(sd.runtimeMinutes),
    releaseYear    = StringPool.small(sd.releaseYear))

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

object StringPool {

  /** One shared `Some` for each Int a runtime or a year takes, 0 to 2,999 — a constant table, not
   *  a cache: `Some(97)` per slot boxed its Int too (the JDK caches Integers only up to 127). */
  private val SmallSomes: IArray[Some[Int]] = IArray.tabulate(3000)(Some(_))
  def small(o: Option[Int]): Option[Int] = o match {
    case Some(i) if i >= 0 && i < SmallSomes.length => SmallSomes(i)
    case other                                      => other
  }

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
   *  reach the pool -- `Showtime.format` and `CinemaShowing.titleKey` arrive as fresh
   *  instances, and so does anything a reader other than the cache decodes through `MovieCodecs`. Raising this number would
   *  cost memory and change nothing. */
  val MaxEntries: Long = 131072L
}
