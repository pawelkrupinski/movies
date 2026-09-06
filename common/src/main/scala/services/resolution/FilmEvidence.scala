package services.resolution

import models.MovieRecord
import services.movies.TitleNormalizer

/**
 * What the CINEMAS published about a film — the only evidence a TMDB resolution may
 * be searched from or judged against.
 *
 * Built from cinema slots ALONE, never from the `Tmdb` / `Imdb` / `Filmweb` slots a
 * previous resolution derived. That rule was re-learned one field at a time: a
 * resolver reading the merged `director` let the wrong film's own credit verify the
 * wrong film ("Dreams" on Haugerud's "Drømmer" while Kino Malta said Michel Franco);
 * reading the merged year let a title-only guess hand its own year back as the
 * search year (`homosapiens|1960` against twelve venues publishing 2025). Each fix
 * added a cinema-only accessor beside the merged one. This is the one value that
 * holds them all, so a caller cannot reach for the derived field by mistake.
 *
 * Every collection is de-duplicated and sorted, so the evidence — and everything
 * concluded from it — is a pure function of the row's state rather than of the
 * order the cinemas arrived in (`StagingOrderDeterminismSpec`). `originalTitles`
 * is the exception: it keeps source-priority order because [[originalTitle]], the
 * search hint, prefers the higher-priority venue's spelling.
 */
final case class FilmEvidence(
  /** Every title form a cinema slot published, one entry per SLOT — so a title
   *  38 venues list under one name outweighs one venue's other name. Sorted, like
   *  everything else here, because the slots come out of a map. */
  slotTitles:     Seq[String],
  /** Cinema-published original (international) titles, in source-priority order. */
  originalTitles: Seq[String],
  /** Every director credit the cinemas published, as printed — a comma-packed
   *  crew stays one string, split by the caller that walks it. */
  directors:      Seq[String],
  cast:           Seq[String],
  runtimes:       Seq[Int],
  years:          Seq[Int]
) {
  /** The distinct raw titles the cinemas report this film under. */
  def titles: Set[String] = slotTitles.toSet

  /** The cinema-reported original title used as a TMDB search hint — the
   *  highest-priority venue's, when any published one. */
  def originalTitle: Option[String] = originalTitles.headOption

  /** The directors as the one comma-joined hint string the dispatch carries. */
  def directorHint: Option[String] = Option.when(directors.nonEmpty)(directors.mkString(", "))

  /** This evidence plus director names an EVENT carried (the cinema whose detail
   *  just landed), folded in the same way so the result is still order-free. */
  def withDirectors(extra: Iterable[String]): FilmEvidence =
    copy(directors = (directors ++ extra.map(_.trim).filter(_.nonEmpty)).distinct.sorted)

  /** How many cinema SLOTS published each search form of the title. A venue that
   *  lists the film under two names votes once per slot, and the two query forms
   *  (`apiQuery`, `searchQuery`) of one title count as one vote when they
   *  sanitize alike. The director walk ranks a credit the MOST venues name above
   *  one a single venue names — which is what stopped Kino Klaps's lone "Maryja.
   *  Matka Papieża" listing taking the "Mistyczka" row from 38 venues. */
  def titleVotes(normalizer: TitleNormalizer): Map[String, Int] =
    slotTitles.iterator
      .flatMap { t =>
        SearchTitles.candidates(t, None)
          .flatMap(f => Seq(normalizer.apiQuery(f), normalizer.searchQuery(f)))
          .map(normalizer.sanitize).filter(_.nonEmpty).distinct
      }
      .toSeq.groupBy(identity).view.mapValues(_.size).toMap

  def isEmpty: Boolean =
    slotTitles.isEmpty && originalTitles.isEmpty && directors.isEmpty && cast.isEmpty && runtimes.isEmpty && years.isEmpty
}

object FilmEvidence {
  val empty: FilmEvidence = FilmEvidence(Nil, Nil, Nil, Nil, Nil, Nil)

  /** The evidence a record's cinema slots carry. Titles come from EVERY slot (a
   *  venue listing the film under two names contributes both); the scalar facts —
   *  original title, director, cast, runtime, year — come from one representative
   *  slot per venue, in source-priority order, exactly as the accessors this
   *  replaces read them. */
  def of(record: MovieRecord): FilmEvidence = {
    val perVenue = record.cinemaData.toSeq
      .sortBy { case (cinema, _) => models.Source.priority.getOrElse(cinema, Int.MaxValue) }
      .map(_._2)
    FilmEvidence(
      slotTitles     = record.cinemaShowings.flatMap(_._2.title).sorted,
      originalTitles = perVenue.flatMap(_.originalTitle).map(_.trim).filter(_.nonEmpty).distinct,
      directors      = perVenue.flatMap(_.director).map(_.trim).filter(_.nonEmpty).distinct.sorted,
      cast           = perVenue.flatMap(_.cast).map(_.trim).filter(_.nonEmpty).distinct.sorted,
      runtimes       = perVenue.flatMap(_.runtimeMinutes).distinct.sorted,
      years          = perVenue.flatMap(_.releaseYear).distinct.sorted)
  }
}
