package services.identity

import models.{MovieRecord, SourceData}
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.freshness.FreshnessStore
import services.movies.MovieCacheReader
import services.lookups.LookupQuery
import services.staging.StagingRepository
import services.tasks.{EnrichDetailsTasks, StagingTaskKeys}

/**
 * The venue detail pages the PIPELINE's own enrichment read, answered from where it keeps them: the
 * per-cinema slots (`SourceData`) of the stored films and the staged newcomers. The identity model
 * asks these instead of fetching a page itself, so a page is fetched once, by the enrichment, and
 * the model waits for it: a page not asked yet is a gap (`None`), re-asked when the enrichment
 * announces it (`VenueDetailRead` → [[keyOf]]).
 *
 * A slot answers only once its page was ASKED — stamped by the enrichment itself, on the page
 * (`EnrichDetailsTasks.pageRead` / `pageGone`, so the answer moves with the listing when rows
 * regroup), or — for what was enriched before pages had stamps — on the film row it was on (its read
 * marker, its dedup stamp alone for a gone page) or a staged row's `StagingTaskKeys.detailKey`. Until then the
 * slot holds only the listing's own values, which are no answer about the page.
 *
 * The slot answers with the listing's values merged with the page's (listing values win, the page
 * fills gaps) — exactly the evidence the model merges from a listing and its page, so the two agree.
 * Indexed by (enricher group, page) over every row, rebuilt when the cache or an announced page moved.
 */
final class VenueDetailSlots(cache: MovieCacheReader, staging: StagingRepository, freshness: FreshnessStore,
                             enrichers: Seq[DetailEnricher], changed: String => Unit = _ => ()) {
  import VenueDetailSlots._

  private val enricherOf = enrichers.map(e => e.cinema -> e).toMap

  /** What the enrichment said about `page` for `enricher`'s group: `None` while it has not asked;
   *  `Some(None)` when it asked and the page had nothing (gone); `Some(Some(facts))` otherwise. */
  def answer(enricher: DetailEnricher, page: String): Option[Option[FilmDetail]] =
    answerOf(index.getOrElse((enricher.detailGroup, page), Nil))

  /** Rebuild now, and report (`changed`, the page's [[keyOf]]) every page whose answer differs from the
   *  last build's — fetched, found gone, or put on a row the slots read by a projection: the model
   *  re-asks exactly those. The enrichment's announcements, each projection's writes and each shadow
   *  tick call it. The first build is the baseline and reports nothing. */
  def refresh(): Unit = synchronized { dirty = true; index; () }

  @volatile private var dirty                     = true
  @volatile private var builtAt: Option[java.time.Instant] = None
  @volatile private var built: Map[(String, String), Seq[Entry]] = Map.empty
  private var answered: Option[Map[(String, String), Option[Option[FilmDetail]]]] = None

  private def index: Map[(String, String), Seq[Entry]] = synchronized {
    val version = cache.lastModified
    if (dirty || !builtAt.contains(version)) {
      dirty   = false
      builtAt = Some(version)
      built   = build()
      val now = built.view.mapValues(answerOf).toMap
      answered.foreach(before => (before.keySet ++ now.keySet).filter(k => before.get(k).flatten != now.get(k).flatten)
        .foreach { case (group, page) => changed(keyOf(group, page)) })
      answered = Some(now)
    }
    built
  }

  private def answerOf(entries: Seq[Entry]): Option[Option[FilmDetail]] =
    entries.iterator.map(_.answer(freshness)).collectFirst { case Some(a) => a }
      .orElse(entries.iterator.map(_.gone(freshness)).collectFirst { case true => None })

  private def build(): Map[(String, String), Seq[Entry]] = {
    // The page's own stamps first; the film row's are what enrichment wrote before pages had their
    // own, and name the page only while it is still on the row that was enriched.
    val films = cache.entries.flatMap { case (key, record) =>
      entriesOf(record).map { case (e, page, slot) =>
        val asked = EnrichDetailsTasks.dedupKey(e.detailGroup, key)
        (e.detailGroup, page) -> Entry(slot,
          read = Seq(EnrichDetailsTasks.pageRead(e.detailGroup, page), EnrichDetailsTasks.readMarker(asked)),
          gone = Seq(EnrichDetailsTasks.pageGone(e.detailGroup, page), asked))
      }
    }
    val staged = staging.findAll().flatMap { row =>
      entriesOf(row.record).map { case (e, page, slot) =>
        (e.detailGroup, page) -> Entry(slot,
          read = Seq(EnrichDetailsTasks.pageRead(e.detailGroup, page),
            StagingTaskKeys.detailKey(staging.normalizer.sanitize(row.title), e.cinema.displayName)),
          gone = Seq(EnrichDetailsTasks.pageGone(e.detailGroup, page)))
      }
    }
    (films ++ staged).groupMap(_._1)(_._2)
  }

  /** Each venue slot of `record` whose cinema has an enricher, by the page that slot names, and the
   *  slot that page merged into: the venue slot itself — one listing's, never the cinema's slots merged,
   *  which would lend one listing's page facts to another's — or, for a chain, its shared target. */
  private def entriesOf(record: MovieRecord): Seq[(DetailEnricher, String, SourceData)] = {
    val paged = record.data.toSeq.flatMap { case (source, venueSlot) =>
      for {
        cinema <- models.Source.cinemaOf(source).toSeq
        e      <- enricherOf.get(cinema).toSeq
        page   <- DetailEnricher.nativeRefOf(venueSlot).toSeq
      } yield (e, page, venueSlot)
    }
    // A chain lands every page of a row on ONE shared target slot, so it holds whichever was written
    // last: it answers for a page only when the row names that chain no other page (Cinema City's
    // "Lalka" and "Ladies Night - Lalka" on one row carried the other film's director).
    val pagesOf = paged.filter { case (e, _, _) => e.detailTarget != e.cinema }.groupMap(_._1.detailGroup)(_._2).view.mapValues(_.toSet).toMap
    paged.flatMap { case (e, page, venueSlot) =>
      if (e.detailTarget == e.cinema) Seq((e, page, venueSlot))
      else if (pagesOf.get(e.detailGroup).exists(_.sizeIs == 1)) record.data.get(e.detailTarget).map(slot => (e, page, slot)).toSeq
      else Nil
    }
  }
}

object VenueDetailSlots {

  /** The key a read of `page` for `detailGroup` files with the model's reads, and a `VenueDetailRead`
   *  re-asks: the same string on both sides is all the model needs. */
  def keyOf(detailGroup: String, page: String): String = LookupQuery.venueDetail(detailGroup, page).key

  /** One slot a page merged into, and the stamps saying whether (and how) the page was asked. */
  private final case class Entry(slot: SourceData, read: Seq[String], gone: Seq[String]) {
    def answer(freshness: FreshnessStore): Option[Option[FilmDetail]] =
      Option.when(read.exists(freshness.lastFetchedAt(_).isDefined))(Some(factsOf(slot)))
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
final class SourceDataDetailEnricher(underlying: DetailEnricher, slots: VenueDetailSlots, gaps: LookupGaps,
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
      gaps.record(LookupQuery(key)); throw new LookupGap(key)
    }
  }
}

