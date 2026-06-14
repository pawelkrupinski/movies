package models

/**
 * Tag for "where this piece of data came from". Every per-source field on
 * `MovieRecord` (synopsis, cast, director, year, countries, …) is keyed by
 * a `Source`: either a `Cinema` instance (one of the scrape sites) or one
 * of the external enrichment sources (`Tmdb`, `Imdb`). The merged accessors
 * on `MovieRecord` iterate the per-source slots in priority order — Multikino
 * first, then the rest of `Cinema.all`, then `Tmdb`, then `Imdb`.
 *
 * Cinema extends Source so existing per-cinema slots fit the model without
 * a wrapper, and `data: Map[Source, SourceData]` accepts a Cinema instance
 * as a key directly.
 *
 * Sealed types must live in a single source file in Scala 2.13; `Cinema`
 * (in `Cinema.scala`) is the second leaf, so the rule is bent by exposing
 * `Source` as a non-sealed trait. The concrete `case object` cases here plus
 * `Cinema.all` keep `Source.all` fully enumerable. */
trait Source { def displayName: String }

case object Tmdb    extends Source { val displayName: String = "TMDB" }
case object Imdb    extends Source { val displayName: String = "IMDB" }
case object Filmweb extends Source { val displayName: String = "Filmweb" }

object Source {
  /** All known sources, ordered for derived-accessor priority: Multikino
   *  first (preserves the old `prioritizedShowings` behaviour), then the rest
   *  of `Cinema.all`, then the network-level chain detail sources (synthetic
   *  cinemas that aren't in `Cinema.all`/`byCity` — see `CinemaCityChain`),
   *  then the external enrichment sources. The chain sources rank after the
   *  physical venues so a venue's own value still wins, and must appear here so
   *  `priority`/`byDisplayName` cover them (the latter resolves Mongo slot keys
   *  on read). */
  val all: Seq[Source] = {
    val cinemasPrioritized = Multikino +: Cinema.all.filterNot(_ == Multikino)
    cinemasPrioritized ++ Seq(CinemaCityChain) ++ Seq(Tmdb, Imdb, Filmweb)
  }

  /** Stable priority index for ordering source slots. Lower = preferred. */
  val priority: Map[Source, Int] = all.zipWithIndex.toMap

  /** Look a source up by its `displayName` — the wire form used as a sub-document
   *  key in Mongo. Unknown names return None (legacy/dropped cinemas). */
  val byDisplayName: Map[String, Source] = all.map(s => s.displayName -> s).toMap
}
