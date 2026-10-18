package services.movies

import models.{Cinema, CinemaMovie, SourceData}

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
              screeningTokens: ScreeningTokens, pageApart: CinemaMovie => Option[String] = _ => None)(using LatestTitleYear): Prepared = {
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
    //
    // Asked of a title again by the fold below and by every caller of `cleaned`: the normalizer
    // memoises the cleaning (`TitleNormalizer.listingTitle`).
    def cleanAndFormat(cm: CinemaMovie): (String, List[String]) = cleanTitle(cinema, cm.movie.title, normalizer)
    val cleaned: CinemaMovie => String = cm => cleanAndFormat(cm)._1
    // Badge each screening with its film's format tokens (unless the client already
    // set one), BEFORE the same-title fold below unions them — then put EVERY token
    // through `ScreeningTokens`, the one gate a source's own words pass to become a
    // badge: it maps each spelling onto the shared vocabulary and drops what is not
    // a screening attribute at all.
    // …and give the listing the facts its title bills ([[withTitleFacts]]), before the fold below tells rows apart by them.
    val formatted: Seq[CinemaMovie] = movies.map { cm =>
      val tokens = cleanAndFormat(cm)._2
      withTitleFacts(cm.copy(showtimes = cm.showtimes.map { st =>
        st.copy(format = screeningTokens.normalize(if (st.format.isEmpty) tokens else st.format))
      }), normalizer.titleFacts(cinema, cm.movie.title))
    }
    // A single cinema can report one film as several rows — one per screening page,
    // or under two spellings that share the slot but differ by year or by a canonical
    // unification the cleaned title keeps apart. They all land on the SAME cinema
    // slot, `CinemaShowing(cinema, sanitize(title))` with no year. Recording them one
    // by one let the LAST win and dropped every other screening, and which row won
    // depended on the scraper's emit order. Fold each
    // cinema's same-slot rows into one at exactly the slot-key granularity: union
    // every screening's showtimes, deduped by physical identity, and keep a
    // deterministic representative for the scalar film fields.
    //
    // …but only rows of ONE film. Arc Cinema Blackpool lists "Belle (2013)" and "Belle
    // (2021)" side by side, Marion Theatre Ocala "Planet of the Apes" (Schaffner, no year)
    // beside "Planet of the Apes (2001)" (Burton): each pair cleans to one title, and unioned
    // into one slot one film was served the other's showtimes. `filmsOf` keeps them apart.
    val deduped: Seq[CinemaMovie] =
      // …nor rows of two pages the venue's reads say are two films (`pageApart`: `VenueSlots.filmPages`).
      formatted.groupBy(cm => (normalizer.sanitize(cleaned(cm)), pageApart(cm))).toSeq
        .sortBy { case ((k, page), _) => (k, page.getOrElse("")) }
        .flatMap { case (_, group) => filmsOf(group, normalizer) }
        .map(folded)
    Prepared(deduped, cleaned)
  }

  /** `cm` with the facts its title bills ([[TitleFacts]]) in its own fields. A director the title credits is the venue's
   *  own word and replaces what the client carried — a feed's linked film's credit, as kinoheld copied Shiv Rawail's onto
   *  DE ScharfrichterKino's "Alpha (dir. Ducournau)"; a running time, an age rating or an original title only fills a
   *  field the client left empty, a structured field being the venue's word too. */
  def withTitleFacts(cm: CinemaMovie, facts: TitleFacts.Facts): CinemaMovie =
    if (facts == TitleFacts.Facts.empty) cm
    else cm.copy(
      director  = if (facts.directors.nonEmpty) facts.directors else cm.director,
      ageRating = cm.ageRating.orElse(facts.ageRating),
      movie     = cm.movie.copy(runtimeMinutes = cm.movie.runtimeMinutes.orElse(facts.runtimeMinutes),
                                originalTitle  = cm.movie.originalTitle.orElse(facts.originalTitle)))

  /** One venue's rows of one film on one slot, as one: the representative's fields
   *  ([[MovieRecordMerge.slotRepresentative]]) over every row's showtimes — and, where the
   *  representative names no year, the one year its siblings name. DE Schauburg Karlsruhe's
   *  yearless "Sinn und Sinnlichkeit (OV)" represented its dated "(OmU)" print of Oakley's 2026
   *  film, and a slot naming no year could not be told from Ang Lee's film beside it. */
  def folded(group: Seq[CinemaMovie])(using LatestTitleYear): CinemaMovie =
    if (group.lengthCompare(1) == 0) group.head
    else {
      val representative = MovieRecordMerge.slotRepresentative(group)
      val dated = if (yearOf(representative).isDefined) representative else group.flatMap(yearOf).distinct match {
        case Seq(year) => representative.copy(movie = representative.movie.copy(releaseYear = Some(year)))
        case _         => representative
      }
      dated.copy(showtimes = MovieRecordMerge.dedupShowtimes(group.flatMap(_.showtimes)))
    }

  /** The films a venue's same-title rows name, told apart only by what the venue itself
   *  published — the discriminator `ListingKey.Published` keys a page-less listing by, never a
   *  year or film the pipeline derived:
   *
   *   - the seasons their titles name ([[TitleSeason]]): a house's two seasons' productions of
   *     one work are two broadcasts — UK The Ashford Cinema's "RBO Cinema Season 2026-27: Swan
   *     Lake" beside "Royal Ballet & Opera 2024-25: Swan Lake" took the 2024/25 record, folded.
   *     A row naming no season is a part of its own when the others name two; a season a title names
   *     beside no stage house ([[StageHouse]]) is a programme's cycle, and splits nothing;
   *   - then years a production-to-release gap apart (each row's own, else its title's bracket).
   *     Each part carries its year, so the landing puts it on its own film; a row naming no
   *     year is a part of its own;
   *   - then, within a year, directors crediting no common person
   *     (`ListingConstraints.venueCreditsApart`, which folds spelling and name order). A row
   *     crediting nobody joins the credited rows when they are one film, else is a part of
   *     its own.
   *
   *  Rows of one film stay one group, as before: a dub beside a subtitled print. A pure
   *  function of the rows as a set; the parts come out in a fixed order. */
  private def filmsOf(group: Seq[CinemaMovie], normalizer: TitleNormalizer)(using LatestTitleYear): Seq[Seq[CinemaMovie]] = {
    // a stage house's season names its production; a programme's ("Kino Seniora 2025/2026: Lalka") only its cycle
    def seasonOf(cm: CinemaMovie) = {
      val titles = cm.movie.rawTitle.toSeq :+ cm.movie.title
      TitleSeason.of(titles).filter(_ => StageHouse.billed(titles))
    }
    val seasons = group.flatMap(seasonOf).distinct
    if (seasons.sizeIs < 2) byYear(group, normalizer)
    else group.groupBy(seasonOf).toSeq.sortBy(_._1.getOrElse(0)).flatMap { case (_, season) => byYear(season, normalizer) }
  }

  private def byYear(group: Seq[CinemaMovie], normalizer: TitleNormalizer)(using LatestTitleYear): Seq[Seq[CinemaMovie]] = {
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
  def yearOf(cm: CinemaMovie)(using LatestTitleYear): Option[Int] =
    cm.movie.releaseYear.orElse(EmbeddedYear.ofAll(cm.movie.rawTitle.toSeq :+ cm.movie.title, LatestTitleYear.current))
  /** The same reading off a stored slot, so a slot and the listing that wrote it agree. */
  def yearOf(sd: SourceData)(using LatestTitleYear): Option[Int] = sd.releaseYear.orElse(EmbeddedYear.ofAll(sd.rawTitle ++ sd.title, LatestTitleYear.current))

  /** A listed title as the venue's slot will carry it — cleaned by the venue's rules, its
   *  format tags peeled off — with those tags. The one definition [[prepare]] folds on. */
  def cleanTitle(cinema: Cinema, title: String, normalizer: TitleNormalizer): (String, List[String]) =
    normalizer.listingTitle(cinema, title)

  /** The cinema slot a listing under `title` lands in: `CinemaShowing(cinema, slotKey)`. */
  def slotKey(cinema: Cinema, title: String, normalizer: TitleNormalizer): String =
    normalizer.sanitize(cleanTitle(cinema, title, normalizer)._1)
}
