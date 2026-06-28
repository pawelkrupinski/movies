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

/** A cinema's slot for ONE shown title. A venue usually lists a film under a
 *  single title, but can list it under several at the same time — the original
 *  plus a dubbed / decorated edition — so each shown title is its own slot,
 *  keeping its own showtimes, synopsis, and scrape divert-recognition. Without
 *  this, folding the same-tmdbId editions onto one record would collapse them
 *  into a single per-cinema slot and lose every title but one (and re-divert the
 *  rest every tick). `titleKey` is `sanitize(title)`; the read-model split keys a
 *  card on it. Wire form (Mongo sub-document key): `"<cinema.displayName>␟<titleKey>"`.
 *  A bare `Cinema` key remains valid (a venue with a single title, and most test
 *  fixtures) — accessors treat the two uniformly via [[Source.cinemaOf]]. */
case class CinemaShowing(cinema: Cinema, titleKey: String) extends Source {
  val displayName: String = s"${cinema.displayName}${CinemaShowing.Separator}$titleKey"
}
object CinemaShowing {
  // ␟ SYMBOL FOR UNIT SEPARATOR — a printable char that never appears in a real
  // cinema displayName, so it round-trips the wire key unambiguously.
  final val Separator: Char = '␟'

  /** The slot key for a cinema's report of a film under `title`. The ONE rule for
   *  deriving a cinema slot key, shared by the scrape ingest, the staging write,
   *  and detail enrichment, so the same (cinema, title) always lands on one slot. */
  def keyFor(cinema: Cinema, title: String): CinemaShowing =
    CinemaShowing(cinema, services.movies.TitleNormalizer.sanitize(title))
}

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

  /** The Cinema behind a slot key — a bare [[Cinema]] or a per-title
   *  [[CinemaShowing]]. `None` for the external enrichment sources (Tmdb/Imdb/
   *  Filmweb), which are never cinema slots. */
  def cinemaOf(source: Source): Option[Cinema] = source match {
    case cinema: Cinema       => Some(cinema)
    case CinemaShowing(c, _)  => Some(c)
    case _                    => None
  }

  /** Priority index treating a per-title cinema slot exactly like its cinema, so
   *  the merged accessors order a `CinemaShowing` slot where its venue ranks
   *  (it isn't in [[all]], so a bare `priority` lookup would sink it to last). */
  def priorityOf(source: Source): Int = source match {
    case CinemaShowing(c, _) => priority.getOrElse(c, Int.MaxValue)
    case s                   => priority.getOrElse(s, Int.MaxValue)
  }

  /** Drop legacy bare-[[Cinema]] slots that a per-title [[CinemaShowing]] slot
   *  for the SAME cinema now supersedes. Rows written before the per-title split
   *  (commit 847f555f) keyed a cinema's slot by the bare `Cinema`; once a
   *  `CinemaShowing(cinema, titleKey)` slot for that film exists they DUPLICATE it
   *  — identical showtimes under two keys, surfaced as twin slots on `/debug` and
   *  double-counted by `cinemaSlots`. The per-title key is canonical, so the bare
   *  one is the redundant copy: drop it (its content is already in the per-title
   *  slot). A LONE bare slot (no per-title sibling — a dormant legacy row not yet
   *  re-scraped) is left as-is; it isn't duplicated and gets re-keyed on its next
   *  scrape. Applied at the storage boundary ([[services.movies.MovieCodecs]]
   *  decode) so no hydrated record carries a duplicate cinema slot. */
  def dropSupersededCinemaSlots[A](data: Map[Source, A]): Map[Source, A] = {
    val perTitleCinemas: Set[Cinema] =
      data.keysIterator.collect { case CinemaShowing(cinema, _) => cinema }.toSet
    if (perTitleCinemas.isEmpty) data
    else data.filter {
      case (cinema: Cinema, _) => !perTitleCinemas.contains(cinema)
      case _                   => true
    }
  }

  /** Resolve a Mongo wire key back to a Source: a known `displayName`, else a
   *  `"<cinema>␟<titleKey>"` per-title cinema slot. None for legacy/dropped
   *  cinemas (the cinema part no longer maps to a known venue). */
  def byWireKey(key: String): Option[Source] = byDisplayName.get(key).orElse {
    val sep = key.indexOf(CinemaShowing.Separator)
    if (sep < 0) None
    else byDisplayName.get(key.substring(0, sep)).collect {
      case cinema: Cinema => CinemaShowing(cinema, key.substring(sep + 1))
    }
  }
}
