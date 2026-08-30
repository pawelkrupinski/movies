package services.movies

import com.github.benmanes.caffeine.cache.Caffeine

/** Interns short, heavily-repeated `SourceData` strings — titles (display, raw and
 *  original), synopses, cast/director names, countries, genres, the poster /
 *  film-page / trailer URLs, and a cinema slot key's `titleKey` — so a film shown at N
 *  cinemas doesn't hold N byte-identical copies
 *  of the same value across its per-cinema slots. Low-cardinality tokens especially win:
 *  a country or genre recurs in thousands of slots corpus-wide yet collapses to ONE
 *  instance. Interning happens at the slot write boundary (`MovieCache.buildCinemaSlot`)
 *  and, for the slot KEY's `titleKey`, in `Source.byWireKey` — the Mongo decode path
 *  `rehydrate` rebuilds the whole corpus through. It is deliberately NOT called on the
 *  scrape path (`CinemaShowing.keyFor`), where `TitleNormalizer.sanitize` already
 *  memoises to one instance per title; the prior-slot carry-forward likewise already
 *  holds interned instances, so only fresh values need it.
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

  private val pool = Caffeine.newBuilder()
    .maximumSize(131072L)
    .build[String, String]()

  /** The canonical instance for a string: the first equal value interned wins, so all
   *  byte-identical values across the corpus share one object. */
  def canonical(s: String): String = pool.get(s, (k: String) => k)

  /** Intern every element of a list (cast, genres, …), preserving order. */
  def canonicalAll(xs: Seq[String]): Seq[String] = if (xs.isEmpty) xs else xs.map(canonical)
}
