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
 * one whose answer changed. The whole store is read once, at the first settle (the baseline, which
 * reports nothing).
 */
final class VenuePageIndex(pages: VenuePageStore, changed: String => Unit = _ => ()) {
  import VenuePageIndex._

  /** What `page` of `enricher`'s group said: `None` while unread; `Some(None)` when gone; `Some(Some(detail))` otherwise. */
  def answer(enricher: DetailEnricher, page: String): Option[Option[FilmDetail]] = {
    if (!loaded) settle()
    answers.get((enricher.detailGroup, page))
  }

  /** `page` of `detailGroup` was written to venue_pages: re-read it at the next [[settle]]. */
  def pageRead(detailGroup: String, page: String): Unit = { pending.add((detailGroup, page)); () }

  /** Take in what was announced since the last settle, telling the model of every page whose answer moved. */
  def settle(): Unit = synchronized {
    if (!loaded) {
      val all = Map.newBuilder[(String, String), Option[FilmDetail]]
      pages.foreach(p => all += ((p.key.detailGroup, p.key.page) -> answerOf(p)))
      pending.clear()
      answers = all.result()
      loaded  = true
    } else if (!pending.isEmpty) {
      val noted = pending.asScala.toSeq
      noted.foreach(pending.remove)
      noted.foreach { case key @ (group, page) =>
        val now = pages.get(VenuePageKey(group, page)).map(answerOf)
        if (now != answers.get(key)) {
          answers = now.fold(answers - key)(a => answers + (key -> a))
          changed(keyOf(group, page))
        }
      }
    }
  }

  @volatile private var answers: Map[(String, String), Option[FilmDetail]] = Map.empty
  @volatile private var loaded = false
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
