package services.movies

import models.{CinemaMovie, SourceData}
import services.cinemas.CountryNames
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
final class CinemaSlotBuilder(enrichmentLanguage: java.util.Locale, stringPool: StringPool) {

  /** Build one cinema's `SourceData` slot for a scraped film, by the same rules for every
   *  path that builds one:
   *    - two-stage detail preservation: a deferred cinema (e.g. Kino Muza) ships
   *      `posterUrl`/`synopsis`/`trailerUrl` AND the detail fields
   *      (cast/director/runtime/originalTitle/countries/genres) as None/empty on
   *      the listing tick — keep whatever the detail refresher already wrote
   *      (`priorSlot` carry-forward); else a listing tick WIPES the enrichment;
   *    - year fallback (`effectiveYear`): keep the prior year when a tick drops it
   *      (Helios' REST year flakes), treating a dropped year as loss not a change;
   *    - cast/director cased for display (`displayNames`: ALL CAPS down for
   *      Cinema City, all-lowercase up for Flicks), runtime-zero squashed to
   *      None, and country names canonicalised. */
  def build(
    cm:            CinemaMovie,
    displayTitle:  String,
    priorSlot:     Option[SourceData],
    effectiveYear: Option[Int]
  ): SourceData =
    SourceData(
      title          = Some(displayTitle),
      // Verbatim upstream title, kept so the merge key is re-derivable when the
      // per-cinema rules change. A rule-driven client carries the pre-strip
      // string in `movie.rawTitle`; others leave it None and `title` is raw.
      rawTitle       = cm.movie.rawTitle.orElse(Some(cm.movie.title)),
      originalTitle  = cm.movie.originalTitle.orElse(priorSlot.flatMap(_.originalTitle)),
      // Collapse a blurb the cinema CMS pasted N× into one description field
      // (Bilety24's Kino Piast shipped the "Ojczyzna" synopsis 9× glued together)
      // at the ingestion boundary, so we never store the duplicate — not just hide
      // it at read time. See tools.SynopsisMarkdown.collapseRepeats.
      // Intern so a film's N cinema slots carrying the same chain-wide blurb share ONE
      // String instead of N byte-identical copies (see `stringPool`). Same applies to the
      // cast/director/country/genre fields below — only the FRESH branch needs interning;
      // the prior-slot carry-forward already holds interned instances.
      synopsis       = cm.synopsis.map(tools.SynopsisMarkdown.collapseRepeats).map(stringPool.canonical).orElse(priorSlot.flatMap(_.synopsis)),
      // Detail fields (cast/director/runtime/originalTitle/countries/genres) are
      // filled by the deferred EnrichDetails merge; a listing-only cinema's re-scrape
      // carries none of them. Carry the prior slot's values forward when the fresh
      // listing lacks them — exactly as synopsis/poster/trailer above — so a listing
      // tick doesn't WIPE the enrichment (which EnrichDetails then re-adds, flapping
      // the row + doubling its change-stream writes). A listing that DOES carry the
      // field still wins, matching FilmDetail.mergeInto's "fill only if empty" rule.
      cast           = if (cm.cast.nonEmpty) displayNames(cm.cast)
                       else priorSlot.map(_.cast).getOrElse(Seq.empty),
      director       = if (cm.director.nonEmpty) displayNames(cm.director)
                       else priorSlot.map(_.director).getOrElse(Seq.empty),
      runtimeMinutes = cm.movie.runtimeMinutes.filter(_ > 0).orElse(priorSlot.flatMap(_.runtimeMinutes)),
      releaseYear    = effectiveYear,
      countries      = { val cs = stringPool.canonicalAll(cm.movie.countries.map(c => CountryNames.canonical(c, enrichmentLanguage)).distinct)
                         if (cs.nonEmpty) cs else priorSlot.map(_.countries).getOrElse(Seq.empty) },
      genres         = if (cm.movie.genres.nonEmpty) stringPool.canonicalAll(cm.movie.genres)
                       else priorSlot.map(_.genres).getOrElse(Seq.empty),
      // Interned like the fields above, and for the same reason: a film's poster,
      // film page and trailer are ONE url repeated across every cinema showing it.
      // Highest-yield strings in the corpus by some margin — the 2026-07-27 UK heap
      // dump held 136,064 poster-url instances for 1,896 distinct values (71.8x) and
      // 138,199 film-page instances for 2,004 (69.0x). `Showtime.bookingUrl` is
      // deliberately NOT interned: it is per-screening, only 1.6x repeated
      // (182,719 -> 116,571 distinct), so pooling it would evict this whole
      // low-cardinality vocabulary for almost no saving.
      posterUrl      = cm.posterUrl.map(stringPool.canonical).orElse(priorSlot.flatMap(_.posterUrl)),
      filmUrl        = cm.filmUrl.map(stringPool.canonical),
      trailerUrl     = cm.trailerUrl.map(stringPool.canonical).orElse(priorSlot.flatMap(_.trailerUrl)),
      // Canonical order so a reorder-only re-scrape stores a byte-identical slot and
      // the write-through guard skips it. Past showings the fresh scrape drops are NOT
      // retained: under the index-only cache the resident `priorSlot` is stripped (Nil
      // showtimes + a digest), so there's nothing to retain FROM, and re-stitching a
      // film's screenings from Mongo per scrape would cost far more read I/O than the
      // one deferred write it would save. Dropping a just-passed showtime is
      // display-neutral (the web filters past showtimes at render). See
      // MovieRecordMerge.sortShowtimes.
      showtimes      = MovieRecordMerge.sortShowtimes(cm.showtimes),
      // Carry the certificate forward on a listing-only re-scrape, like the detail
      // fields above, so a tick that lacks it doesn't wipe a value the detail merge added.
      ageRating      = cm.ageRating.map(stringPool.canonical).orElse(priorSlot.flatMap(_.ageRating))
    )

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
