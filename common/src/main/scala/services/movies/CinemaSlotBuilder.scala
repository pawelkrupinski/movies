package services.movies

import models.{CinemaMovie, SourceData}
import tools.{PersonName, TextNormalization}

/**
 * How one venue's scraped row becomes that venue's slot on a film: the landing's `movies` write,
 * its staging divert, and the identity projection (phase 5) all build slots here, so a film reads
 * the same whichever path made it.
 *
 * `enrichmentLanguage` is the deployment's language, which cinema-reported production countries
 * are canonicalised into (`CountryNames.canonical`); `stringPool` is where the strings a slot
 * repeats across venues are interned.
 */
final class CinemaSlotBuilder(enrichmentLanguage: java.util.Locale, stringPool: StringPool,
                              val pages: VenuePageFacts = VenuePageFacts.none) {

  /** Build one cinema's `SourceData` slot for a scraped film, by the same rules for every
   *  path that builds one:
   *    - two-stage detail preservation: a deferred cinema (e.g. Kino Muza) ships
   *      `posterUrl`/`synopsis`/`trailerUrl` AND the detail fields
   *      (cast/director/runtime/originalTitle/countries/genres) as None/empty on
   *      the listing tick — keep whatever the detail refresher already wrote
   *      (`priorSlot` carry-forward); else a listing tick WIPES the enrichment. Where the listing's page has
   *      been read (`pages`), a field the page states is carried from that read, not from the slot built over;
   *    - year fallback: keep the prior slot's year when the listing carries none — a tick that
   *      drops it (Helios' REST year flakes), or a venue page's year the detail enrichment wrote
   *      — treating a missing year as loss, not a change; a listing with no page keeps neither its
   *      prior year nor director, which key it (see `carryKeyed`);
   *    - cast/director cased for display (`displayNames`: ALL CAPS down for
   *      Cinema City, all-lowercase up for Flicks), a runtime no screened film has (zero, or Filmtheater
   *      Bleicherode's 6000-minute "flüstern & SCHREIEN") squashed to None, and country names canonicalised. */
  def build(
    cm:            CinemaMovie,
    displayTitle:  String,
    builtOver:     Option[SourceData]
  ): SourceData = {
    // A slot another page of the venue's wrote is another listing's: Kino Iluzjon's two "Lalka" pages (Has 1968,
    // Kawalski 2026) share one slot key, and the 2026 film's slot, built over the 1968 one, took its year and
    // director (2026-10-05). Nothing of it is this listing's to carry.
    val priorSlot = builtOver.filterNot(prior => otherPage(prior.filmUrl, cm.filmUrl))
    // The venue's page for the film: what a link it printed relative to its own site resolves against.
    val filmPage = SlotFields.url(cm.filmUrl, None)
    // A listing with no page is keyed by its year and directors (`ListingKey.Published`): a year or director carried
    // from the prior slot is ANOTHER listing's (this one's own, missing, would be another key), and keys the slot as
    // that listing — whose film the next projection then puts this one's slot on, a fresh film every projection
    // (2026-10-04). Carried only where a page keys the listing, as the detail enrichment that writes them needs one.
    val carryKeyed = cm.filmUrl.exists(_.trim.nonEmpty)
    // What the listing's own page states, as venue_pages last read it: what the detail enrichment wrote onto the slot,
    // so it, not the slot built over, is what a field the listing leaves out is carried from. A slot that once took
    // another page's year, director, cast and runtime under this page's url (Kino Iluzjon's 1968 "Lalka", written
    // before the guard above) carried them as its own until a field the page states disagreed — now never past a read.
    val page    = cm.filmUrl.map(_.trim).filter(_.nonEmpty).flatMap(pages.of(cm.cinema, _))
    def carriedOpt[A](field: SourceData => Option[A]): Option[A] = page.flatMap(field).orElse(priorSlot.flatMap(field))
    def carriedSeq[A](field: SourceData => Seq[A]): Seq[A] =
      page.map(field).filter(_.nonEmpty).orElse(priorSlot.map(field)).getOrElse(Seq.empty)
    SourceData(
      title          = stringPool.canonicalSome(displayTitle),
      // Verbatim upstream title, kept so the merge key is re-derivable when the
      // per-cinema rules change. A rule-driven client carries the pre-strip
      // string in `movie.rawTitle`; others leave it None and `title` is raw.
      rawTitle       = stringPool.canonical(cm.movie.rawTitle.orElse(Some(cm.movie.title))),
      originalTitle  = cm.movie.originalTitle.orElse(carriedOpt(_.originalTitle)),
      // Collapse a blurb the cinema CMS pasted N× into one description field
      // (Bilety24's Kino Piast shipped the "Ojczyzna" synopsis 9× glued together)
      // at the ingestion boundary, so we never store the duplicate — not just hide
      // it at read time. See tools.SynopsisMarkdown.collapseRepeats.
      // Intern so a film's N cinema slots carrying the same chain-wide blurb share ONE
      // String instead of N byte-identical copies (see `stringPool`). Same applies to the
      // cast/director/country/genre fields below — only the FRESH branch needs interning;
      // the prior-slot carry-forward already holds interned instances.
      synopsis       = stringPool.canonical(cm.synopsis.map(tools.SynopsisMarkdown.collapseRepeats)).orElse(carriedOpt(_.synopsis)),
      // Detail fields (cast/director/runtime/originalTitle/countries/genres) are
      // filled by the deferred EnrichDetails merge; a listing-only cinema's re-scrape
      // carries none of them. Carry the prior slot's values forward when the fresh
      // listing lacks them — exactly as synopsis/poster/trailer above — so a listing
      // tick doesn't WIPE the enrichment (which EnrichDetails then re-adds, flapping
      // the row + doubling its change-stream writes). A listing that DOES carry the
      // field still wins, matching FilmDetail.mergeInto's "fill only if empty" rule.
      cast           = if (cm.cast.nonEmpty) displayNames(cm.cast)
                       else carriedSeq(_.cast),
      director       = if (cm.director.nonEmpty) displayNames(cm.director)
                       else if (carryKeyed) carriedSeq(_.director) else Seq.empty,
      runtimeMinutes = StringPool.small(cm.movie.runtimeMinutes.filter(FilmRuntime.plausible)).orElse(carriedOpt(_.runtimeMinutes)),
      releaseYear    = StringPool.small(cm.movie.releaseYear.orElse(if (carryKeyed) carriedOpt(_.releaseYear) else None)),
      countries      = { val cs = stringPool.canonicalAll(SlotFields.countries(cm.movie.countries, enrichmentLanguage))
                         if (cs.nonEmpty) cs else carriedSeq(_.countries) },
      genres         = { val gs = stringPool.canonicalAll(SlotFields.genres(cm.movie.genres))
                         if (gs.nonEmpty) gs else carriedSeq(_.genres) },
      // Interned like the fields above, and for the same reason: a film's poster,
      // film page and trailer are ONE url repeated across every cinema showing it.
      // Highest-yield strings in the corpus by some margin — the 2026-07-27 UK heap
      // dump held 136,064 poster-url instances for 1,896 distinct values (71.8x) and
      // 138,199 film-page instances for 2,004 (69.0x). `Showtime.bookingUrl` is
      // deliberately NOT interned: it is per-screening, only 1.6x repeated
      // (182,719 -> 116,571 distinct), so pooling it would evict this whole
      // low-cardinality vocabulary for almost no saving.
      // Each through `SlotFields.url`, so no client serves a link a reader cannot follow.
      posterUrl      = stringPool.canonical(SlotFields.url(cm.posterUrl, filmPage)).orElse(priorSlot.flatMap(_.posterUrl)),
      // As published: it is also the venue's detail ref (`DetailEnricher.nativeRefOf`).
      filmUrl        = stringPool.canonical(cm.filmUrl),
      trailerUrl     = stringPool.canonical(SlotFields.url(cm.trailerUrl, filmPage)).orElse(priorSlot.flatMap(_.trailerUrl)),
      // Canonical order so a reorder-only re-scrape stores a byte-identical slot and
      // the write-through guard skips it. Past showings the fresh scrape drops are NOT
      // retained: under the index-only cache the resident `priorSlot` is stripped (Nil
      // showtimes + a digest), so there's nothing to retain FROM, and re-stitching a
      // film's screenings from Mongo per scrape would cost far more read I/O than the
      // one deferred write it would save. Dropping a just-passed showtime is
      // display-neutral (the web filters past showtimes at render). See
      // MovieRecordMerge.sortShowtimes.
      // Booking links made followable and a screening the listing printed twice kept once
      // (`SlotFields.showtimes`) before the sort.
      showtimes      = MovieRecordMerge.sortShowtimes(SlotFields.showtimes(cm.showtimes, filmPage)),
      // Carry the certificate forward on a listing-only re-scrape, like the detail
      // fields above, so a tick that lacks it doesn't wipe a value the detail merge added.
      ageRating      = stringPool.canonical(cm.ageRating).orElse(carriedOpt(_.ageRating))
    )
  }

  /** Whether a slot of page `prior` is another listing's than one of page `page`: both pages published, and not one. */
  private def otherPage(prior: Option[String], page: Option[String]): Boolean = {
    def published(url: Option[String]) = url.map(_.trim).filter(_.nonEmpty)
    (published(prior), published(page)) match {
      case (Some(a), Some(b)) => a != b
      case _                  => false
    }
  }

  /** Cast/crew names as the display layer needs them, for the two casings a
   *  cinema source invents: SHOUTED credits are title-cased
   *  ([[TextNormalization.titleCaseIfAllCaps]] — Cinema City's "KARL URBAN") and
   *  all-lowercase ones are capitalised ([[PersonName]] — Flicks' Anglophone
   *  venues emit `content_cast` as "christoph waltz"). The two rules are
   *  disjoint by construction — each returns its input untouched unless the
   *  string is entirely in the other's case — so a properly-cased name from
   *  TMDB, IMDb or any of the Polish scrapers passes through both unchanged, and
   *  the order they compose in doesn't matter.
   *
   *  Interned last, so the pool holds the canonical DISPLAY spelling rather than
   *  a separate instance per source casing. */
  private def displayNames(names: Seq[String]): Seq[String] =
    stringPool.canonicalAll(names.map(TextNormalization.titleCaseIfAllCaps).map(PersonName.capitalized))
}
