package services.scrapes

import models.{Cinema, CinemaMovie, Movie, Showtime}
import org.bson.codecs.configuration.CodecRegistry
import org.bson.codecs.configuration.CodecRegistries.{fromCodecs, fromProviders, fromRegistries}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import services.PersistedCodecs
import services.movies.{JavaTimeCodecs, ShowtimeCodec}

import java.time.Instant

/** Storage mirror of one `CinemaMovie`. Identical to the domain type minus
 *  `cinema` — every film in a row belongs to the row's cinema, so repeating it
 *  10-30 times per document would be pure waste, and `Cinema` is a sealed
 *  hierarchy the driver could not encode anyway. Re-attached on read from
 *  `Cinema.byDisplayName`. */
case class ArchivedFilmDto(
  movie:       Movie,
  posterUrl:   Option[String],
  filmUrl:     Option[String],
  synopsis:    Option[String],
  cast:        Seq[String],
  director:    Seq[String],
  showtimes:   Seq[Showtime],
  externalIds: Map[String, String],
  trailerUrl:  Option[String],
  ageRating:   Option[String],
  // The listing's matching-only cut synopsis (`CinemaMovie.synopsisExcerpt`); absent on a film archived before it.
  synopsisExcerpt: Option[String] = None,
  // `showtimes.##`, written with them: what a reader that wants the film without its showtimes — the identity
  // model's take-up, the projection's listing read — tells a change in them by, leaving them on the server. Absent
  // on a film written before it existed, whose reader reads the showtimes instead.
  showtimesDigest: Option[Int] = None
)

/** The newest attempt that produced nothing, stored beside the listing it failed
 *  to refresh. `outcome` is the wire label of a [[ScrapeOutcome]]. */
case class BarrenAttemptDto(
  at:      Instant,
  outcome: String,
  error:   Option[String],
  // When the current unbroken barren run began; absent on rows written before the
  // field existed, which decode as "since `at`".
  since:   Option[Instant] = None,
  // Consecutive failed runs; absent on rows written before the count existed,
  // which decode as an uncounted run.
  failedRuns: Option[Int] = None,
  // Written only when true, so a row from before the field existed — and every
  // unvouched empty or failure — decodes as false.
  noScheduleListed: Option[Boolean] = None,
  // The source the attempt read; absent on rows written before the field existed (an unknown source).
  sourceKey: Option[String] = None
) {
  def toDomain: Option[BarrenAttempt] =
    ScrapeOutcome.byLabel(outcome).map(o => BarrenAttempt(at, o, error, since, failedRuns, noScheduleListed.contains(true), sourceKey))
}

object BarrenAttemptDto {
  def from(b: BarrenAttempt): BarrenAttemptDto =
    BarrenAttemptDto(b.at, b.outcome.label, b.error, b.since, b.failedRuns, Option.when(b.noScheduleListed)(true), b.sourceKey)
}

/** Storage DTO for one cinema's archive row — the macro codec target for the
 *  `cinema_scrapes` collection. `_id` is the cinema's `displayName`, the same
 *  wire key every per-cinema row is stored under elsewhere.
 *
 *  `scrapedAt` / `listingComplete` / `films` describe the last scrape WITH
 *  content and are absent only on a row that has never had any — hence optional,
 *  so a barren-only row (a cinema failing since before the archive began) still
 *  decodes. */
case class StoredScrapeDto(
  _id:             String,
  city:            Option[String],
  scrapedAt:       Option[Instant],
  listingComplete: Option[Boolean],
  films:           Option[Seq[ArchivedFilmDto]],
  lastBarren:      Option[BarrenAttemptDto]
)

/** The projection the content census reads: a cinema, when it last produced films,
 *  and its barren marker's no-schedule flag. Its own DTO because decoding
 *  `StoredScrapeDto` would pull every archived film along with the one timestamp
 *  wanted. */
case class ContentStampDto(_id: String, scrapedAt: Option[Instant], lastBarren: Option[NoScheduleFlagDto]) {
  def toDomain: ContentStamp = ContentStamp(scrapedAt, lastBarren.exists(_.noScheduleListed.contains(true)))
}

/** `lastBarren` projected down to its one flag. */
case class NoScheduleFlagDto(noScheduleListed: Option[Boolean])

object StoredScrapeDto {

  def toFilmDto(f: CinemaMovie): ArchivedFilmDto =
    ArchivedFilmDto(f.movie, f.posterUrl, f.filmUrl, f.synopsis, f.cast, f.director,
      f.showtimes, f.externalIds, f.trailerUrl, f.ageRating, f.synopsisExcerpt, Some(f.showtimes.##))

  def fromSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): StoredScrapeDto =
    StoredScrapeDto(
      _id             = cinema.displayName,
      city            = city,
      scrapedAt       = Some(scrape.at),
      listingComplete = Some(scrape.listingComplete),
      films           = Some(scrape.films.map(toFilmDto)),
      // A scrape that just succeeded is by definition the newest thing that has
      // happened to this cinema, so nothing barren can still apply.
      lastBarren      = None
    )

  /** `None` for a row whose cinema no longer exists in the catalog — a venue that
   *  was renamed or dropped. Its films can't be attributed to anything, so the
   *  row is skipped rather than guessed at. */
  def toDomain(dto: StoredScrapeDto): Option[ArchivedScrape] = Cinema.byDisplayName.get(dto._id).map(toDomain(_, dto))

  /** `cinema`'s row as `dto` stores it. */
  def toDomain(cinema: Cinema, dto: StoredScrapeDto): ArchivedScrape =
    ArchivedScrape(
      cinema      = cinema,
      city        = dto.city,
      lastSuccess = dto.scrapedAt.map(at => SuccessfulScrape(
        at              = at,
        listingComplete = dto.listingComplete.getOrElse(true),
        films           = dto.films.getOrElse(Seq.empty).map(f => CinemaMovie(
          f.movie, cinema, f.posterUrl, f.filmUrl, f.synopsis, f.cast, f.director,
          f.showtimes, f.externalIds, f.trailerUrl, f.ageRating, f.synopsisExcerpt))
      )),
      lastBarren  = dto.lastBarren.flatMap(_.toDomain)
    )

  /** A whole row as stored: its last listing, if any, and the barren attempt since. */
  def of(row: ArchivedScrape): StoredScrapeDto =
    row.lastSuccess.fold(StoredScrapeDto(row.cinema.displayName, row.city, None, None, None, None))(fromSuccess(row.cinema, row.city, _))
      .copy(lastBarren = row.lastBarren.map(BarrenAttemptDto.from))
}

/** BSON wiring for `cinema_scrapes`. `IgnoreNone` throughout so an absent
 *  `synopsis`/`room`/`ageRating` costs nothing on the wire and decodes back to
 *  `None` — the same trade `MovieCodecs` makes for `Showtime`. */
object ScrapeArchiveCodecs extends PersistedCodecs {
  type OmittingNone = (ContentStampDto, NoScheduleFlagDto, Movie, ArchivedFilmDto, BarrenAttemptDto, StoredScrapeDto)
  type WritingNone  = EmptyTuple

  /** Every showtime through the movies' hand-written [[ShowtimeCodec]] (`Showtime` has no macro
   *  codec), and every film through [[StreamingArchivedFilmCodec]], which can store its booking URLs
   *  split at their shared prefix: a venue's listing carries every showtime it scraped, the identity
   *  intake reads a venue's back as each scrape lands, and the projection the whole archive — on the
   *  US corpus the macro's per-showtime machinery was most of the archive's decode, ~10% of a
   *  convergence leg's CPU (JFR). `ScrapeArchiveCodecsSpec` pins both to the macro's reading. */
  val registry: CodecRegistry = {
    val macros = fromRegistries(
      fromCodecs(JavaTimeCodecs.localDateTime, ShowtimeCodec),
      fromProviders(PersistedCodecs.omittingNone[OmittingNone]*),
      DEFAULT_CODEC_REGISTRY)
    fromRegistries(fromCodecs(new StreamingArchivedFilmCodec(macros.get(classOf[Movie]))), macros)
  }

  /** [[registry]] reading every film WITHOUT its showtimes (`Nil`, their digest kept): for a read that projects
   *  `films.showtimes` out on the server, and wants a venue's listing, not its screenings. */
  val leanRegistry: CodecRegistry =
    fromRegistries(fromCodecs(new StreamingArchivedFilmCodec(registry.get(classOf[Movie]), withShowtimes = false)), registry)
}
