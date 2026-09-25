package services.movies

import models.{Cinema, CinemaMovie, SourceData}
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
    // Central format strip: peel a screen-format/language tag ("(Napisy PL)",
    // "- 2D dubbing", "[2D DUB]") off EVERY cinema's title into the showings'
    // `format`, so a film's dub/subtitle/2D editions fold onto ONE clean slot for
    // every cinema with no per-client code. `FormatTags` strips only format words,
    // so a programme prefix, a "+ event" suffix, or a Ukrainian screening keep their
    // title and stay their own card.
    def cleanAndFormat(cm: CinemaMovie): (String, List[String]) = cleanTitle(cinema, cm.movie.title, normalizer)
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
    //
    // …but only rows of ONE film. Arc Cinema Blackpool lists "Belle (2013)" and "Belle
    // (2021)" side by side, Marion Theatre Ocala "Planet of the Apes" (Schaffner, no year)
    // beside "Planet of the Apes (2001)" (Burton): each pair cleans to one title, and unioned
    // into one slot one film was served the other's showtimes. `filmsOf` keeps them apart.
    val deduped: Seq[CinemaMovie] =
      formatted.groupBy(cm => normalizer.sanitize(cleaned(cm))).toSeq
        .sortBy { case (k, _) => k }
        .flatMap { case (_, group) => filmsOf(group, normalizer) }
        .map { group =>
          if (group.lengthCompare(1) == 0) group.head
          else MovieRecordMerge.slotRepresentative(group)
            .copy(showtimes = MovieRecordMerge.dedupShowtimes(group.flatMap(_.showtimes)))
        }
    Prepared(deduped, cleaned)
  }

  /** The films a venue's same-title rows name, told apart only by what the venue itself
   *  published — the discriminator `ListingKey.Published` keys a page-less listing by, never a
   *  year or film the pipeline derived:
   *
   *   - years a production-to-release gap apart (each row's own, else its title's bracket).
   *     Each part carries its year, so the landing puts it on its own film; a row naming no
   *     year is a part of its own;
   *   - then, within a year, directors crediting no common person
   *     (`ListingConstraints.venueCreditsApart`, which folds spelling and name order). A row
   *     crediting nobody joins the credited rows when they are one film, else is a part of
   *     its own.
   *
   *  Rows of one film stay one group, as before: a dub beside a subtitled print. A pure
   *  function of the rows as a set; the parts come out in a fixed order. */
  private def filmsOf(group: Seq[CinemaMovie], normalizer: TitleNormalizer): Seq[Seq[CinemaMovie]] = {
    val years = group.flatMap(yearOf).distinct
    val byYear =
      // Years a production-vs-release gap apart are one film printed two ways.
      if (years.sizeIs < 2 || years.max - years.min <= services.resolution.YearWindow.ProductionToRelease) Seq(group)
      else group.groupBy(yearOf).toSeq.sortBy(_._1.getOrElse(0)).map { case (year, films) =>
        films.map(cm => cm.copy(movie = cm.movie.copy(releaseYear = cm.movie.releaseYear.orElse(year))))
      }
    byYear.flatMap(byDirector(_, normalizer))
  }

  private def byDirector(group: Seq[CinemaMovie], normalizer: TitleNormalizer): Seq[Seq[CinemaMovie]] = {
    val (credited, uncredited) = group.partition(_.director.exists(_.trim.nonEmpty))
    // Connected components of "credits the same person": whatever order they are joined in.
    val people = credited.foldLeft(List.empty[List[CinemaMovie]]) { (parts, cm) =>
      val (same, other) = parts.partition(_.exists(p => ListingConstraints.venueCreditsApart(p.director, cm.director, normalizer).isEmpty))
      (cm :: same.flatten) :: other
    }
    if (people.sizeIs < 2) Seq(group)
    else (people.sortBy(_.flatMap(_.director).sorted.mkString("\u0000")) ++ Option.when(uncredited.nonEmpty)(uncredited))
      .map(part => group.filter(cm => part.exists(_ eq cm)))
  }

  /** The year a listing names: its own, else the one its title brackets. What tells two films
   *  a venue lists under one title apart. */
  def yearOf(cm: CinemaMovie): Option[Int] =
    cm.movie.releaseYear.orElse(EmbeddedYear.ofAll(cm.movie.rawTitle.toSeq :+ cm.movie.title))
  /** The same reading off a stored slot, so a slot and the listing that wrote it agree. */
  def yearOf(sd: SourceData): Option[Int] = sd.releaseYear.orElse(EmbeddedYear.ofAll(sd.rawTitle ++ sd.title))

  /** A listed title as the venue's slot will carry it — cleaned by the venue's rules, its
   *  format tags peeled off — with those tags. The one definition [[prepare]] folds on. */
  def cleanTitle(cinema: Cinema, title: String, normalizer: TitleNormalizer): (String, List[String]) =
    FormatTags.extractFormatTags(normalizer.cinemaClean(TitleRuleKey.of(cinema), title))

  /** The cinema slot a listing under `title` lands in: `CinemaShowing(cinema, slotKey)`. */
  def slotKey(cinema: Cinema, title: String, normalizer: TitleNormalizer): String =
    normalizer.sanitize(cleanTitle(cinema, title, normalizer)._1)
}
