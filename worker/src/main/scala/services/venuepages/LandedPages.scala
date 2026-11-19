package services.venuepages

import services.cinemas.common.{DetailEnricher, FilmDetail}

import java.util.Locale

/**
 * What the film rows took of each venue page ([[StoredVenuePage.landed]]), against which a fresh read is measured: the
 * fields a page now states DIFFERENTLY from what the rows took are authoritative ([[authoritative]]), everything else
 * only fills gaps. Both reads are compared as a slot holds them (`FilmDetail.landed`, its countries in the corpus's
 * `language`): compared raw, a page that spells a country or lists its genres the way it always has would differ from
 * what it landed as.
 */
final class LandedPages(pages: VenuePageStore, language: Locale) {

  /** What venue_pages held of `ref` of `enricher`'s group before a read replaces it: the last read and what the rows
   *  took of it. Read BEFORE the page is read again. */
  def before(enricher: DetailEnricher, ref: String): LandedPages.Before = {
    val key = VenuePageKey(enricher.detailGroup, ref)
    LandedPages.Before(key, pages.stored(key))
  }

  /** A page's fields as a cinema slot holds them. */
  def asSlotHolds(detail: FilmDetail): FilmDetail = detail.landed(language)

  /** What did this page say when the rows last took it? The fields `detail` (as a slot holds it) now states
   *  DIFFERENTLY are authoritative (`FilmDetail.refreshInto`): a venue that reuses a URL for a different film —
   *  Kino Pionier's `/event/lalka`, Has's 1968 picture then the 2026 one — otherwise keeps the first film's year and
   *  runtime for ever, because the fill-only merge has nothing to fill. Everything else only fills gaps (`mergeInto`),
   *  so the listing keeps out-ranking the page wherever both speak — on a first landing (a page never read, or read
   *  only as gone: a 404 is not a read), and on every re-read that says what the rows took before. Not the page as
   *  last READ: the reader stores that before this merge, and a merge that missed its row (re-keyed between enqueue and
   *  pickup) left the changed read stored and never landed — every later read then matched it, and the row kept the
   *  old year and runtime for ever. Not a freshness marker: the reader stamps the page read before the handler merges,
   *  so a marker read here made every read — the first included — authoritative, and a re-read every refresh window
   *  rewrote the listing's own countries and genres with the page's.
   *
   *  A page last read before `landed` was kept has no record of what the rows took. Its last read is what every read
   *  then landed — except on a row whose merge had already missed, which holds an older read. Its year and runtime
   *  (what such a stuck row shows: the row is keyed by the year) are therefore landed authoritatively ONCE, and the page
   *  is then landed, so every later read is measured against what the rows took. Only those two: making the whole read
   *  authoritative would rewrite the listing's own countries and genres (above). The cost is that a listing whose own
   *  year or runtime differs from its page's is overruled once, on a field the page states. */
  def authoritative(before: LandedPages.Before, detail: FilmDetail): FilmDetail =
    before.stored.landed match {
      case Some(took) => detail.changedSince(asSlotHolds(took))
      case None       => before.stored.page.map(_.outcome).collect { case VenuePage.Read(last) => last }.fold(FilmDetail())(last =>
        detail.changedSince(asSlotHolds(last)).copy(releaseYear = detail.releaseYear, runtimeMinutes = detail.runtimeMinutes))
    }

  /** The rows took `read` of the page: recorded, unless it is what they last took. Compared with the last read, a page
   *  read before `landed` was kept and unchanged since was never landed, and paid this check's find on every task for
   *  ever. */
  def taken(before: LandedPages.Before, read: FilmDetail): Unit =
    if (!before.stored.landed.contains(read)) { pages.land(before.key, read); () }
}

object LandedPages {
  /** What venue_pages held of the page at `key` before it was read again. */
  final case class Before(key: VenuePageKey, stored: StoredVenuePage)
}
