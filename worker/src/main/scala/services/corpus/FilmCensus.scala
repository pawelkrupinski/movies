package services.corpus

import services.movies.LatestTitleYear
import models.{MovieRecord, Source, SourceData}
import services.movies.StoredMovieRecord
import services.titles.TitleNormalizer
import services.readmodel.ReadModelProjection

/** One film's part in the [[CorpusCensus]], from the film as the cache holds it.
 *
 *  `subsets` has a bit per [[CensusSubsets.Static]] the film belongs to. A film ready to project
 *  (`readyToProject`, the projector's own gate) files its screening venue slots under `cards` — one card per
 *  display-title group, as [[ReadModelProjection.partition]] splits it — and one that is not keeps them in
 *  `screenable`, for `unresolved_with_showtimes`. A venue slot with no showtime start is in neither: it screens nothing. */
final class FilmCensus private (val subsets: Int, val ready: Boolean, val cinemaSlots: Int,
                                private val bySource: Map[Source, SlotCensus], val cards: Seq[Seq[SlotCensus]],
                                val screenable: Seq[SlotCensus], private val anchor: Option[String]) {
  /** The part of the slot at `source`, if the film holds one there. */
  private[corpus] def partAt(source: Source): Option[SlotCensus] = bySource.get(source)
  /** Whether this part holds `other`'s very map of slot parts. */
  private[corpus] def sameSlots(other: FilmCensus): Boolean = bySource eq other.bySource
}

object FilmCensus {
  /** `stored`'s part. A slot the cache holds as the very object it held for `prior` keeps its derived part: a scrape
   *  landing moves one slot of a film, and a film can hold thousands. */
  def of(stored: StoredMovieRecord, normalizer: TitleNormalizer, prior: Option[FilmCensus])(using LatestTitleYear): FilmCensus = {
    val record   = stored.record
    val bySource = slotParts(record, prior, normalizer)
    val ready    = record.readyToProject
    val anchor   = Option.when(ready)(ReadModelProjection.anchorKeyOf(stored.asReadBack(normalizer), normalizer))
    prior.filter(p => (p.bySource eq bySource) && p.ready == ready && p.anchor == anchor) match {
      case Some(p) => new FilmCensus(subsetsOf(record), ready, bySource.size, bySource, p.cards, p.screenable, anchor)
      case None    => fresh(subsetsOf(record), ready, bySource, anchor)
    }
  }

  private def subsetsOf(record: MovieRecord)(using LatestTitleYear): Int =
    CensusSubsets.Static.indices.foldLeft(0)((bits, i) =>
      if (CensusSubsets.Static(i)._2(record)) bits | (1 << i) else bits)

  /** The film's venue slots' parts, over the prior part's own map: only a slot that is not the object it held is
   *  derived again and put in, and a film whose slots all are keeps the map, its title groups and its screenable
   *  slots — derived anew on every change of a film, they were kept until its next one, so promoted and left to die
   *  old. */
  private def slotParts(record: MovieRecord, prior: Option[FilmCensus], normalizer: TitleNormalizer): Map[Source, SlotCensus] = {
    val held    = prior.fold(Map.empty[Source, SlotCensus])(_.bySource)
    val cinemas = record.data.iterator.collect { case (source, slot) if Source.cinemaOf(source).isDefined => source -> slot }.toSeq
    val gone    = held.keysIterator.filterNot(record.data.contains).toSeq
    cinemas.foldLeft(if (gone.isEmpty) held else held -- gone) { case (map, (source, slot)) =>
      if (map.get(source).exists(_.slot eq slot)) map else map.updated(source, SlotCensus(slot, Source.cinemaOf(source).get, normalizer))
    }
  }

  private def fresh(subsets: Int, ready: Boolean, bySource: Map[Source, SlotCensus], anchor: Option[String]): FilmCensus = {
    // Held as arrays, not lists: a wide film's part held a list cell per screening slot (24 bytes each), rebuilt on
    // every change of the film — the largest named holder of dead list cells in worker-us's old generation (heap
    // dump 2026-10-05: 9.1 MB).
    val screening = bySource.valuesIterator.filter(_.starts.nonEmpty).to(scala.collection.immutable.ArraySeq)
    val cards     = anchor.fold(Seq.empty[Seq[SlotCensus]])(a => screening.filter(_.city.isDefined).groupBy(_.titleKey.getOrElse(a))
      .valuesIterator.to(scala.collection.immutable.ArraySeq))
    new FilmCensus(subsets, ready, bySource.size, bySource, cards, if (ready) Nil else screening, anchor)
  }
}
