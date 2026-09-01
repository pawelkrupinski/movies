package services.staging

import models.{Cinema, CinemaShowing, Country, MovieRecord, Showtime, Source, SourceData}
import org.bson.BsonBinaryWriter
import org.bson.codecs.{Codec, EncoderContext}
import org.bson.io.BasicOutputBuffer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{InMemoryMovieRepository, InMemoryScreeningsRepository, MovieCodecs, MovieRepository, StoredMovieDto}

import java.time.{Instant, LocalDateTime}

/**
 * The `movies` document `MongoStagingFolder` writes, MEASURED — because the thing that
 * broke was a size, and no assertion about shape says how close to the ceiling a corpus
 * is.
 *
 * On 2026-09-01 the United States' convergence leg logged, once per staging round:
 *
 * {{{
 * Staging fold 'Avengers: Doomsday' aborted after 1 attempt(s):
 *   Payload document size is larger than maximum of 16793600.
 * }}}
 *
 * The fold writes `movies` directly inside its transaction — its upserts and its staging
 * deletes have to commit together — and it was writing the record whole: every venue's
 * `SourceData` with its board of showtimes inline. That is linear in the number of venues
 * screening the film, and a wide release plays most of a country.
 * `BsonMaximumSizeExceededException` carries no transient label, so `foldWithRetry`
 * abandoned after one attempt and rethrew; the staging rows were therefore never
 * consumed, so `StagingReaper` enqueued the same fold again on the next tick, and the
 * next, with no backoff and no give-up.
 *
 * The venues are the REAL catalogue rather than a number typed here, so the guard tracks
 * the corpus: a country that doubles its venues moves this test, which is the whole point
 * of measuring rather than asserting a shape.
 *
 * Both assertions are load-bearing and neither works alone. The first is the fix. The
 * second pins the reason it is needed: on its own the first would still pass on a corpus
 * that had merely got smaller, and the guard would quietly stop guarding.
 *
 * No Mongo — this is the encoder's own byte count, which is what the server compares
 * against its limit. That the FOLDER writes this shape is `FoldWritesStorageShapeSpec`,
 * which needs a replica set and lives in the it layer.
 */
class StagingFoldDocumentSizeSpec extends AnyFlatSpec with Matchers {

  /** Mongo's ceiling as the driver reports it: 16 MiB plus the command overhead the
   *  server allows on top. The number from the exception, verbatim. */
  private val BsonDocumentLimit = 16793600

  /** A fortnight of a wide release's day, per venue. Deliberately modest: the horizon is
   *  `ScrapeHorizon.MaxDays` (730), not a fortnight, so a real board is much larger and
   *  the measurement below is a floor rather than a worst case. */
  private val ShowtimesPerVenue = 28

  /** The largest country's real venue list — the corpus that found this. */
  private val venues: Seq[Cinema] =
    Country.all.map(c => c.cities.flatMap(_.cinemas).distinct).maxBy(_.size)

  private val codec: Codec[StoredMovieDto] = MovieCodecs.registry.get(classOf[StoredMovieDto])

  /** What the server weighs: the encoded document, in bytes. */
  private def encodedBytes(dto: StoredMovieDto): Int = {
    val buffer = new BasicOutputBuffer()
    try {
      codec.encode(new BsonBinaryWriter(buffer), dto, EncoderContext.builder().build())
      buffer.getPosition
    } finally buffer.close()
  }

  private def slot(venue: Cinema): (Source, SourceData) =
    CinemaShowing(venue, "avengersdoomsday") -> SourceData(
      title     = Some("Avengers: Doomsday"),
      synopsis  = Some("The Avengers face their gravest threat yet. " * 12),
      cast      = Seq("Robert Downey Jr.", "Chris Hemsworth", "Anthony Mackie", "Vanessa Kirby"),
      genres    = Seq("Action", "Adventure", "Science Fiction"),
      showtimes = (1 to ShowtimesPerVenue).map(n => Showtime(
        dateTime   = LocalDateTime.of(2026, 12, 1, 10, 0).plusHours(n * 3L),
        bookingUrl = Some(s"https://tickets.example.com/${venue.displayName.replace(' ', '-')}/$n"),
        room       = Some(s"Auditorium ${n % 16 + 1}"),
        format     = List("IMAX", "3D"))))

  /** One wide release, playing the whole country. */
  private val record: MovieRecord =
    MovieRecord(tmdbId = Some(1061474), data = venues.map(slot).toMap)

  private def documentFor(data: Map[Source, SourceData]): StoredMovieDto =
    StoredMovieDto.fromDomain("avengersdoomsday|2026", record.copy(data = data), Instant.now())

  /** The repository as production wires it under the read-split, asked the way
   *  `MongoStagingFolder` asks it — so the rule under test is the one that ships, not a
   *  second copy of it written here. */
  private val splitAware: MovieRepository =
    new InMemoryMovieRepository(screenings = Some(new InMemoryScreeningsRepository),
                                normalizer = titleNormalizer)

  "the document the staging fold writes" should
    "stay inside Mongo's limit for a film playing every venue in the largest country" in {
    // A catalogue this test could not read would make both assertions meaningless in
    // opposite directions — this one trivially true, the next one trivially false.
    venues.size should be > 1000
    val bytes = encodedBytes(documentFor(splitAware.slotsForStorage(record.data)))
    withClue(s"${venues.size} venues encoded to $bytes bytes: ") {
      bytes should be < BsonDocumentLimit
    }
  }

  it should "be over that limit with the boards left inline, which is what it used to write" in {
    val bytes = encodedBytes(documentFor(record.data))
    withClue(s"${venues.size} venues x $ShowtimesPerVenue showtimes encoded to $bytes bytes: ") {
      bytes should be > BsonDocumentLimit
    }
  }
}
