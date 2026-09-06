package services.movies

import models.{Cinema, CinemaMovie}
import services.titlerules.TitleRuleKey

/**
 * A venue's listing as the cache will record it: each title cleaned by the
 * venue's own rules, every screening badged with its film's format tokens through
 * the shared vocabulary, and the venue's several rows for one film folded onto
 * the one slot they share. A pure function of the listing and the country's
 * rules, so the scrape→record step is reproducible whatever order the scraper
 * emitted its rows in.
 */
object ScrapeListing {

  final case class Prepared(movies: Seq[CinemaMovie], cleaned: CinemaMovie => String)

  def prepare(cinema: Cinema, movies: Seq[CinemaMovie], normalizer: TitleNormalizer,
              screeningTokens: ScreeningTokens): Prepared = {
    // Per-cinema title cleanup, rule-driven and keyed by the cinema. A migrated
    // client already applies these rules to `title` (carrying the pre-strip string in
    // `rawTitle`), so this re-application is idempotent insurance; a client that
    // emits a raw title with no inline cleanup gets cleaned here. Display CASING is
    // NOT applied — the raw spelling is kept as provenance so the `displayTitle`
    // picker can rank on it; casing is applied to the chosen title there.
    val ruleKey = TitleRuleKey.of(cinema)
    // Central format strip: peel a screen-format/language tag ("(Napisy PL)",
    // "- 2D dubbing", "[2D DUB]") off EVERY cinema's title into the showings'
    // `format`, so a film's dub/subtitle/2D editions fold onto ONE clean slot for
    // every cinema with no per-client code. `FormatTags` strips only format words,
    // so a programme prefix, a "+ event" suffix, or a Ukrainian screening keep their
    // title and stay their own card.
    def cleanAndFormat(cm: CinemaMovie): (String, List[String]) =
      FormatTags.extractFormatTags(normalizer.cinemaClean(ruleKey, cm.movie.title))
    val cleaned: CinemaMovie => String = cm => cleanAndFormat(cm)._1
    // Badge each screening with its film's format tokens (unless the client already
    // set one), BEFORE the same-title fold below unions them — then put EVERY token
    // through `ScreeningTokens`, the one gate a source's own words pass to become a
    // badge: it maps each spelling onto the shared vocabulary and drops what is not
    // a screening attribute at all.
    val formatted: Seq[CinemaMovie] = movies.map { cm =>
      val tokens = cleanAndFormat(cm)._2
      cm.copy(showtimes = cm.showtimes.map { st =>
        st.copy(format = screeningTokens.normalize(if (st.format.isEmpty) tokens else st.format))
      })
    }
    // A single cinema can report one film as several rows — one per screening page,
    // or under two spellings that share the slot but differ by year or by a canonical
    // unification the cleaned title keeps apart. They all land on the SAME cinema
    // slot, `CinemaShowing(cinema, sanitize(title))` with no year. Recording them one
    // by one let the LAST win and dropped every other screening, and which row won
    // depended on the scraper's emit order (ReScrapeIdempotencySpec). Fold each
    // cinema's same-slot rows into one at exactly the slot-key granularity: union
    // every screening's showtimes, deduped by physical identity, and keep a
    // deterministic representative for the scalar film fields.
    val deduped: Seq[CinemaMovie] =
      formatted.groupBy(cm => normalizer.sanitize(cleaned(cm))).toSeq
        .sortBy { case (k, _) => k }
        .map { case (_, group) =>
          if (group.lengthCompare(1) == 0) group.head
          else MovieRecordMerge.slotRepresentative(group)
            .copy(showtimes = MovieRecordMerge.dedupShowtimes(group.flatMap(_.showtimes)))
        }
    Prepared(deduped, cleaned)
  }
}
