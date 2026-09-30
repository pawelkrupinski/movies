package services.identity

import models.{MovieRecord, SourceData}
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.freshness.FreshnessStore
import services.movies.MovieCacheReader
import services.observations.LookupQuery
import services.staging.StagingRepository
import services.tasks.{EnrichDetailsTasks, StagingTaskKeys}

/**
 * The venue detail pages the PIPELINE's own enrichment read, answered from where it keeps them: the
 * per-cinema slots (`SourceData`) of the stored films and the staged newcomers. The identity model
 * asks these instead of fetching a page itself, so a page is fetched once, by the enrichment, and
 * the model waits for it: a page not asked yet is a gap (`None`), re-asked when the enrichment
 * announces it (`VenueDetailRead` → [[keyOf]]).
 *
 * A slot answers only once its page was ASKED — stamped by the enrichment itself: a film row's detail
 * read marker (`EnrichDetailsTasks.readMarker`, set when the page merged) or its dedup stamp alone
 * (the page was gone: asked, no detail), a staged row's `StagingTaskKeys.detailKey`. Until then the
 * slot holds only the listing's own values, which are no answer about the page.
 *
 * The slot answers with the listing's values merged with the page's (listing values win, the page
 * fills gaps) — exactly the evidence the model merges from a listing and its page, so the two agree.
 * Indexed by (enricher group, page) over every row, rebuilt when the cache or an announced page moved.
 */
final class VenueDetailSlots(cache: MovieCacheReader, staging: StagingRepository, freshness: FreshnessStore,
                             enrichers: Seq[DetailEnricher]) {
  import VenueDetailSlots._

  private val enricherOf = enrichers.map(e => e.cinema -> e).toMap

  /** What the enrichment said about `page` for `enricher`'s group: `None` while it has not asked;
   *  `Some(None)` when it asked and the page had nothing (gone); `Some(Some(facts))` otherwise. */
  def answer(enricher: DetailEnricher, page: String): Option[Option[FilmDetail]] =
    index.getOrElse((enricher.detailGroup, page), Nil).iterator.map(_.answer(freshness)).collectFirst { case Some(a) => a }
      .orElse(index.getOrElse((enricher.detailGroup, page), Nil).iterator.map(_.gone(freshness)).collectFirst { case true => None })

  /** The page moved (the enrichment announced it): rebuild on the next ask. */
  def changed(): Unit = { dirty = true }

  @volatile private var dirty                     = true
  @volatile private var builtAt: Option[java.time.Instant] = None
  @volatile private var built: Map[(String, String), Seq[Entry]] = Map.empty

  private def index: Map[(String, String), Seq[Entry]] = synchronized {
    val version = cache.lastModified
    if (dirty || !builtAt.contains(version)) {
      dirty   = false
      builtAt = Some(version)
      built   = build()
    }
    built
  }

  private def build(): Map[(String, String), Seq[Entry]] = {
    val films = cache.entries.flatMap { case (key, record) =>
      entriesOf(record).map { case (e, page, slot) =>
        val asked = EnrichDetailsTasks.dedupKey(e.detailGroup, key)
        (e.detailGroup, page) -> Entry(slot, read = Some(EnrichDetailsTasks.readMarker(asked)), gone = Some(asked))
      }
    }
    val staged = staging.findAll().flatMap { row =>
      entriesOf(row.record).map { case (e, page, slot) =>
        (e.detailGroup, page) -> Entry(slot, read = Some(StagingTaskKeys.detailKey(staging.normalizer.sanitize(row.title), e.cinema.displayName)),
          gone = None)
      }
    }
    (films ++ staged).groupMap(_._1)(_._2)
  }

  /** Each enricher of a cinema `record` shows, the page it would fetch for it, and the slot that page merged into. */
  private def entriesOf(record: MovieRecord): Seq[(DetailEnricher, String, SourceData)] =
    record.cinemaData.keys.toSeq.flatMap(enricherOf.get).flatMap { e =>
      for {
        page <- e.nativeDetailRef(record)
        slot <- if (e.detailTarget != e.cinema) record.data.get(e.detailTarget) else record.cinemaData.get(e.cinema)
      } yield (e, page, slot)
    }
}

object VenueDetailSlots {

  /** The key a read of `page` for `detailGroup` files with the model's reads, and a `VenueDetailRead`
   *  re-asks: the same string on both sides is all the model needs. */
  def keyOf(detailGroup: String, page: String): String = LookupQuery.venueDetail(detailGroup, page).key

  /** One slot a page merged into, and the stamps saying whether (and how) the page was asked. */
  private final case class Entry(slot: SourceData, read: Option[String], gone: Option[String]) {
    def answer(freshness: FreshnessStore): Option[Option[FilmDetail]] =
      read.filter(freshness.lastFetchedAt(_).isDefined).map(_ => Some(factsOf(slot)))
    def gone(freshness: FreshnessStore): Boolean = gone.exists(freshness.lastFetchedAt(_).isDefined)
  }

  /** The page-level facts the model reads, as the enrichment left them in the slot. */
  def factsOf(slot: SourceData): FilmDetail =
    FilmDetail(director = slot.director, runtimeMinutes = slot.runtimeMinutes, releaseYear = slot.releaseYear,
      originalTitle = slot.originalTitle, countries = slot.countries)
}

/**
 * A venue's detail as the identity model reads it: from the pipeline's own enrichment
 * ([[VenueDetailSlots]]), never fetched here. Everything but the fetch is the wrapped enricher's. A
 * page the enrichment has not asked yet is a gap — `Answer.Unknown` to the model, which re-asks it
 * when the enrichment announces the page — never a failure and never a live fetch.
 */
final class SourceDataDetailEnricher(underlying: DetailEnricher, slots: VenueDetailSlots, gaps: ObservationGaps,
                                     reads: ObservationReads = ObservationReads.Untracked) extends DetailEnricher {

  override def cinema: models.Cinema                      = underlying.cinema
  override def detailGroup: String                        = underlying.detailGroup
  override def detailTarget: models.Source                = underlying.detailTarget
  override def enrichmentServiceOverride: Option[String] = underlying.enrichmentServiceOverride
  override def defersTmdbResolution: Boolean              = underlying.defersTmdbResolution

  override def fetchFilmDetail(ref: String): Option[FilmDetail] = {
    val key = VenueDetailSlots.keyOf(detailGroup, ref)
    reads.read(key)
    slots.answer(underlying, ref).getOrElse {
      gaps.record(LookupQuery(key)); throw new ObservationGap(key)
    }
  }
}

