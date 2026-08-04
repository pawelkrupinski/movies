package services.movies

import models.{MovieRecord, Source, SourceData}

/**
 * Decides whether one `movies` row is actually holding TWO DIFFERENT FILMS, and
 * which of its cinema slots belong to which.
 *
 * A row is keyed by its title, so two unrelated films released under the same
 * Polish title land on it together — and once merged, nothing separates them
 * again. `FilmCanonicalizer.clusterByFilm` splits by tmdbId, but that operates on
 * ROWS: a single row holding both films has one tmdbId and nothing to cluster.
 * Whatever id it carries, the cinemas showing the other film are mis-served.
 *
 * Two live examples:
 *   - "Joanna d'Arc" — Kino Muranów screens Besson's 1999 film (160 min, original
 *     title "Joan of Arc"); Kino Nowe Horyzonty screens Pálmason's 2025
 *     "Jóhanna af Örk". One row, and it resolves to neither.
 *   - "Obcy" — 36 cinemas screen Ozon's "L'étranger", one screens Brandt
 *     Andersen's "I Was A Stranger" (2024, 103 min). The row is right for 36 and
 *     wrong for the odd one out.
 *
 * The evidence is the cinemas' OWN published data — director and original title —
 * never anything derived from the resolution, which would just be the answer this
 * is checking. Slots that publish neither are uninformative: they are not evidence
 * of a second film and always stay with the main group.
 */
object MixedFilmDetector {

  /** One film's worth of a row: the identity its cinemas published, and the slots
   *  that published it. */
  case class Group(directors: Set[String], originalTitle: Option[String], slots: Seq[(Source, SourceData)])

  /** The cinema slots partitioned by the film they describe, largest first, then
   *  by identity so the order is a pure function of the row.
   *
   *  Empty when the row describes ONE film (the normal case) — including when it
   *  describes one film inconsistently, since disagreement is only evidence of a
   *  second film if the two identities are mutually exclusive. See [[conflicting]].
   */
  def split(record: MovieRecord, normalizer: TitleNormalizer): Seq[Group] = {
    val informative = record.cinemaSlots.filter { case (_, sd) =>
      sd.director.exists(_.trim.nonEmpty) || sd.originalTitle.exists(_.trim.nonEmpty)
    }
    val groups = informative
      .groupBy { case (_, sd) => (normalise(sd.director, normalizer), sd.originalTitle.map(normalizer.sanitize).filter(_.nonEmpty)) }
      .toSeq
      .map { case ((directors, original), slots) => Group(directors, original, slots) }
      .sortBy(g => (-g.slots.size, g.directors.toSeq.sorted.mkString(","), g.originalTitle.getOrElse("")))
    if (groups.sizeIs < 2 || !groups.exists(g => conflicting(groups.head, g))) Seq.empty else groups
  }

  /** The slots that belong to a film OTHER than the row's main one — what has to
   *  leave the row for each film to get a record of its own. */
  def strays(record: MovieRecord, normalizer: TitleNormalizer): Seq[(Source, SourceData)] =
    split(record, normalizer) match {
      case Seq()               => Seq.empty
      case main +: others      => others.filter(conflicting(main, _)).flatMap(_.slots)
    }

  /** Do these two identities describe DIFFERENT films?
   *
   *  Only a positive contradiction counts, never a gap. Two named directors with
   *  nobody in common is one; so are two original titles that share no word.
   *  A slot that merely omits what another publishes tells us nothing, and
   *  treating that as a second film would shred every row whose cinemas describe
   *  it to different depths. */
  def conflicting(a: Group, b: Group): Boolean = {
    val directorsDisagree = a.directors.nonEmpty && b.directors.nonEmpty && a.directors.intersect(b.directors).isEmpty
    val titlesDisagree = (for {
      x <- a.originalTitle
      y <- b.originalTitle
    } yield !shareAWord(x, y)).getOrElse(false)
    directorsDisagree || titlesDisagree
  }

  private def normalise(directors: Seq[String], normalizer: TitleNormalizer): Set[String] =
    directors.iterator.map(normalizer.sanitize).filter(_.nonEmpty).toSet

  /** Sanitized titles are one token, so compare them whole; "L'étranger" and
   *  "L’Étranger" fold together, "I Was A Stranger" does not fold onto either. */
  private def shareAWord(a: String, b: String): Boolean = a == b || a.contains(b) || b.contains(a)
}
