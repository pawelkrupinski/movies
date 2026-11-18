package services.movies

import services.titles.TitleContainment
import services.titles.TitleNormalizer

import models.{Cinema, Filmweb, Imdb, MovieRecord, Source, SourceData, Tmdb}

/**
 * A record's readings built on its ONE representative slot per venue ([[cinemaData]]): the evidence a resolution
 * searches from, the display title, genres, age rating and original titles. They live here, not on
 * [[models.MovieRecord]], because picking that slot is title logic (a decorated listing gives way to the bare one,
 * [[TitleContainment]]; equal spellings merge, [[MovieRecordMerge]]) and models sit below it.
 *
 * Extension methods, members of [[LatestTitleYear]] (which mixes this in): every reading needs that year to tell a
 * sequel's number from a year, so wherever one is given -- `record.cinemaData` already asked for it -- the readings are
 * in scope with no import. Where the year is a plain value (a Twirl template's parameter), import
 * `services.movies.CinemaSlotReadings.*`.
 */
trait CinemaSlotReadings {

  extension (record: MovieRecord) {

    /** One representative slot per cinema — the keyed view detail enrichment, the
     *  published [[evidence]] and the [[displayTitle]] vote read. A venue listing the film
     *  several ways (plain, a senior-club screening, a cheap-Tuesday promo) is represented
     *  by its LEAST-decorated listing — see [[leastDecorated]]; it used to be whichever
     *  title sorted last, which named rows after promotions. Consumers that need EVERY
     *  slot (the display split, divert) use [[cinemaShowings]]; consumers that only ask
     *  WHICH venues are present use [[slotCinemas]]. `latestYear` is what tells a sequel's
     *  number from a year in that decoration test ("Odyssey 2027"), so every accessor built on
     *  this view takes it from its caller's clock. */
    def cinemaData(using latestYear: LatestTitleYear): Map[Cinema, SourceData] = {
      val byVenue = record.cinemaShowings.groupMap(_._1)(_._2)
      if (byVenue.valuesIterator.forall(_.lengthIs == 1)) byVenue.view.mapValues(_.head).toMap
      else {
        val venuesPerSpelling = byVenue.valuesIterator
          .flatMap(_.flatMap(_.title).map(TitleContainment.tokens).distinct)
          .toSeq.groupMapReduce(identity)(_ => 1)(_ + _)
        byVenue.view.mapValues(CinemaSlotReadings.leastDecorated(_, venuesPerSpelling, latestYear.value)).toMap
      }
    }

    /** What the cinemas published about this film — the only evidence a TMDB
     *  resolution is searched from or judged against. Cinema slots only; see
     *  [[FilmEvidence]] for why the derived slots are excluded. */
    def evidence(using LatestTitleYear): FilmEvidence = FilmEvidence.of(record)

    /** Cinema-only iteration in the same priority order — used by accessors
     *  that should *not* fall back to TMDB/IMDb (e.g. `cinemaOriginalTitle`,
     *  which is specifically the cinema-reported English title used as a
     *  TMDB-search hint). */
    private def prioritizedCinema(using LatestTitleYear): Seq[(Cinema, SourceData)] =
      cinemaData.toSeq.sortBy { case (c, _) => (Source.priority.getOrElse(c, Int.MaxValue), c.displayName) }

    /** Display title for the row, derived deterministically (no scrape-order
     *  dependence) by the shared `chooseDisplay` ladder: dominant clean form
     *  across the per-cinema reported titles → the `Tmdb` slot's Polish title
     *  (when it shares that identity and is well-formed) → the cinema
     *  `preferredDisplay` ladder → `recase`. A trailing bracketed year equal to
     *  [[resolvedYear]] — the year the card prints beside the title — is dropped from
     *  every spelling first.
     *
     *  Caller supplies cleanTitle (the post-decoration-strip anchor the cache keys
     *  the row by) because the record itself doesn't carry it; it's the fallback
     *  when no cinema is scraping yet (TMDB-only row).
     *
     *  `extraCinemaTitles` are cinema-reported titles this record does not itself carry —
     *  the slots a MIGRATED film keeps in `movie_slots` while its `movies` document reports
     *  none. They join the VOTE and nothing else, so a caller can make the ladder see the
     *  whole film without putting foreign slots into a record something is about to persist.
     *
     *  `normalizer` is an ordinary parameter, not a context one: the display
     *  ladder is country-specific, and the Twirl templates that render a row have
     *  no given in scope — making it implicit silently broke `web`. */
    def displayTitle(cleanTitle: String, normalizer: TitleNormalizer,
                     extraCinemaTitles: Seq[String] = Nil)(using LatestTitleYear): String =
      normalizer.chooseDisplay(
        perCinemaTitles = cinemaData.values.flatMap(_.title).toSeq ++ extraCinemaTitles,
        fallback        = cleanTitle,
        tmdbTitle       = record.data.get(Tmdb).flatMap(_.title),
        shownYear       = record.resolvedYear)

    /** Genre names — first non-empty list in genre-priority
     *  order: TMDB → Filmweb → cinema slots (in their normal priority order).
     *  Unlike `countries`, we don't union across sources because each source
     *  has its own taxonomy and spelling (TMDB's "Sci-Fi" vs Helios'
     *  "science fiction"); unioning would surface inconsistent labels for the
     *  same concept. Each list is spelled as the genre master set names it in the screening country's
     *  language ([[SlotFields.genres]]): a slot keeps the spelling it was stored with
     *  until its source is read again (a TMDB slot fetched before the set printed "film TV"), and a
     *  placeholder ("Unknown") is no genre, so the next source's list is taken instead. */
    def genres(using LatestTitleYear): Seq[String] = {
      val language = record.screeningLanguage
      def served(genres: Seq[String]): Seq[String] = language.fold(genres)(SlotFields.genres(genres, _))
      def slotGenres(s: Source): Seq[String] = record.data.get(s).map(slot => served(slot.genres)).getOrElse(Seq.empty)
      val tmdb    = slotGenres(Tmdb)
      val filmweb = slotGenres(Filmweb)
      if (tmdb.nonEmpty)    tmdb
      else if (filmweb.nonEmpty) filmweb
      else prioritizedCinema.iterator.map(slot => served(slot._2.genres)).find(_.nonEmpty).getOrElse(Seq.empty)
    }

    /** Age rating / certificate, CINEMA-first: the local screening venue's
     *  certificate (BBFC in the UK) is the authoritative one for that country,
     *  unlike TMDB's multi-country certification. Falls back to a `Tmdb` slot's
     *  value only if no cinema carries one. Verbatim per source — no cross-scheme
     *  normalisation (a UK "15" and a German "FSK 16" are different labels for
     *  different audiences). */
    def ageRating(using LatestTitleYear): Option[String] =
      prioritizedCinema.iterator.flatMap(_._2.ageRating).find(_.nonEmpty)
        .orElse(record.data.get(Tmdb).flatMap(_.ageRating))

    /** Best available original (production-language) title across sources —
     *  the TMDB-resolved one first, then IMDb's `originalTitleText`, then
     *  whatever a cinema's own API exposed (only Multikino does), then Filmweb's
     *  `/info` originalTitle as a last-resort gap-filler (surfaces on a
     *  Filmweb-only row TMDB never resolved). None when no source supplied one.
     *  Distinct from `originalTitle`, which is TMDB-only and stays that way for
     *  the search/URL fallbacks that rely on it. */
    def anyOriginalTitle(using LatestTitleYear): Option[String] =
      record.originalTitle
        .orElse(record.data.get(Imdb).flatMap(_.originalTitle))
        .orElse(evidence.originalTitle)
        .orElse(record.data.get(Filmweb).flatMap(_.originalTitle))

    /** The original title worth showing alongside `displayed` — present, and
     *  not merely a case/whitespace re-spelling of the title already on screen.
     *  An English-language film carries the same string as both its cinema
     *  title and its original title, so showing it would just repeat the line.
     *  Centralising the "is it redundant?" test here lets every frontend render
     *  the result unconditionally. */
    def distinctOriginalTitle(displayed: String)(using LatestTitleYear): Option[String] =
      anyOriginalTitle.map(_.trim).filter(_.nonEmpty)
        .filterNot(_.equalsIgnoreCase(displayed.trim))
  }
}

object CinemaSlotReadings extends CinemaSlotReadings {

  /** Of one venue's slots, the least-decorated listing: a spelling that does not
   *  decorate another of the venue's titles (`TitleContainment` — a banner on either
   *  edge), then the spelling most venues share (catches a banner on BOTH edges, and
   *  picks between equally bare translations), then the fewest words, then the words
   *  themselves. Structural rather than rule-driven, so it needs no country's
   *  normalizer. Several slots carrying that one spelling are one listing and settle
   *  field by field (`MovieRecordMerge.mergeSlots`), so no order picks among them. */
  private[movies] def leastDecorated(slots: Seq[SourceData], venuesPerSpelling: Map[Seq[String], Int], latestYear: Int): SourceData = {
    val spelled = slots.map(sd => sd -> TitleContainment.tokens(sd.title.getOrElse("")))
    val best = spelled.map(_._2).distinct.minBy { tokens =>
      (spelled.exists { case (_, other) => TitleContainment.decorates(other, tokens, latestYear) },
       -venuesPerSpelling.getOrElse(tokens, 0), tokens.length, tokens.mkString(" "))
    }
    spelled.collect { case (sd, tokens) if tokens == best => sd } match {
      case Seq(only) => only
      case several   => MovieRecordMerge.mergeSlots(several)
    }
  }
}
