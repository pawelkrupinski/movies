package services.identity

import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.lookups.LookupQuery
import services.venuepages.{VenuePage, VenuePageKey, VenuePageStore}

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/**
 * The venue detail pages as the identity model reads them: `venue_pages`, the one place every page read
 * is written (`VenuePageReader`), by page — never fetched here. A page not read yet is a gap (`None`),
 * re-asked when its read is announced (`VenueDetailRead` → [[pageRead]] → [[keyOf]]).
 *
 * Answers between settles stay the ones the model was last told of: an announcement only notes the page,
 * and [[settle]] — which the model calls before each drain — re-reads the noted pages and reports every
 * one whose answer changed. The whole store is read at the first settle (the baseline, which reports
 * nothing), and again at each settle until a scan reaches every page.
 */
final class VenuePageIndex(pages: VenuePageStore, changed: String => Unit = _ => ()) {
  import VenuePageIndex._

  /** What `page` of `enricher`'s group said: `None` while unread; `Some(None)` when gone; `Some(Some(detail))` otherwise. */
  def answer(enricher: DetailEnricher, page: String): Option[Option[FilmDetail]] = {
    if (!scanned) settle()
    answers.get((enricher.detailGroup, page))
  }

  /** `page` of `detailGroup` was written to venue_pages: re-read it at the next [[settle]]. */
  def pageRead(detailGroup: String, page: String): Unit = { pending.add((detailGroup, page)); () }

  /** Take in what was announced since the last settle, telling the model of every page whose answer moved. */
  def settle(): Unit = synchronized {
    if (!complete) {
      // Announcements made before the scan are covered by it; one made while it runs may be of a page
      // it already passed, so it stays noted for the next settle to read again.
      pending.clear()
      val all = Map.newBuilder[(String, String), Option[FilmDetail]]
      val reachedEnd = pages.foreach(p => all += ((p.key.detailGroup, p.key.page) -> answerOf(p)))
      val read = all.result()
      // The first scan is the baseline and reports nothing. A scan a failed read stopped short is not
      // the whole store: the pages past it stay gaps only until the next settle scans again, which
      // reports every answer it moved — a page the model found a gap is re-asked then.
      if (scanned) (read.keySet ++ answers.keySet).foreach { key =>
        if (read.get(key) != answers.get(key)) changed(keyOf(key._1, key._2))
      }
      answers  = read
      scanned  = true
      complete = reachedEnd.isComplete
    } else if (!pending.isEmpty) {
      // Each page is un-noted only as it is read: a read that throws (a Mongo timeout) leaves it and
      // every page after it noted for the next settle, instead of dropping their announcements. One
      // announced again while it is read stays noted, to be read once more.
      pending.asScala.toSeq.foreach { case key @ (group, page) =>
        pending.remove(key)
        val now = try pages.get(VenuePageKey(group, page)).map(answerOf)
                  catch { case scala.util.control.NonFatal(e) => pending.add(key); throw e }
        if (now != answers.get(key)) {
          answers = now.fold(answers - key)(a => answers + (key -> a))
          changed(keyOf(group, page))
        }
      }
    }
  }

  @volatile private var answers: Map[(String, String), Option[FilmDetail]] = Map.empty
  /** Whether a scan of the whole store has run at all, and whether one reached every page. */
  @volatile private var scanned  = false
  @volatile private var complete = false
  private val pending = ConcurrentHashMap.newKeySet[(String, String)]()
}

object VenuePageIndex {

  /** The key a read of `page` for `detailGroup` files with the model's reads, and a `VenueDetailRead`
   *  re-asks: the same string on both sides is all the model needs. */
  def keyOf(detailGroup: String, page: String): String = LookupQuery.venueDetail(detailGroup, page).key

  private def answerOf(page: VenuePage): Option[FilmDetail] = page.outcome match {
    case VenuePage.Read(detail) => Some(detail)
    case VenuePage.Gone(_)      => None
  }
}

/**
 * A venue's detail as the identity model reads it: from venue_pages ([[VenuePageIndex]]), never fetched
 * here. Everything but the fetch is the wrapped enricher's. A page not read yet is a gap — `Answer.Unknown`
 * to the model, which re-asks it when the read is announced — never a failure and never a live fetch.
 */
final class VenuePageDetailEnricher(underlying: DetailEnricher, index: VenuePageIndex, gaps: LookupGaps,
                                    reads: ObservationReads = ObservationReads.Untracked) extends DetailEnricher {

  override def cinema: models.Cinema                      = underlying.cinema
  override def detailGroup: String                        = underlying.detailGroup
  override def detailTarget: models.Source                = underlying.detailTarget
  override def enrichmentServiceOverride: Option[String] = underlying.enrichmentServiceOverride
  override def defersTmdbResolution: Boolean              = underlying.defersTmdbResolution

  override def fetchFilmDetail(ref: String): Option[FilmDetail] = {
    val key = VenuePageIndex.keyOf(detailGroup, ref)
    reads.read(key)
    index.answer(underlying, ref).getOrElse {
      gaps.record(LookupQuery(key)); throw new LookupGap(key)
    }
  }
}

/**
 * venue_pages as a venue slot is built from it ([[services.movies.VenuePageFacts]]): the index's answer for the listing's
 * page, landed as the detail enrichment lands it on the slot (`FilmDetail.landed`) — never fetched, and only for a venue
 * whose detail lands on its own slot (a chain's lands on its network source, which no listing builds).
 */
final class IndexedVenuePageFacts(enrichers: Seq[DetailEnricher], index: VenuePageIndex, enrichmentLanguage: java.util.Locale)
    extends services.movies.VenuePageFacts {
  private val ownSlot: Map[models.Cinema, DetailEnricher] =
    enrichers.iterator.filter(e => e.detailTarget == e.cinema).map(e => e.cinema -> e).toMap

  def of(cinema: models.Cinema, page: String): Option[models.SourceData] =
    ownSlot.get(cinema).flatMap(index.answer(_, page)).flatten.map(_.landed(enrichmentLanguage).pageFields)
}
