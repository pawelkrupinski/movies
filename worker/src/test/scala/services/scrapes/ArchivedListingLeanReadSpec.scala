package services.scrapes

import models.{Cinema, CinemaMovie, Helios, Movie, Showtime}
import org.bson.codecs.{DecoderContext, EncoderContext}
import org.bson.{BsonBinaryReader, BsonDocument, BsonDocumentReader, BsonDocumentWriter, RawBsonDocument}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDateTime}
import scala.jdk.CollectionConverters._

/** The identity model's take-up and the projection's listing read want each venue's listing without its showtimes —
 *  the model reads no showtime, the projection only their digest — yet a US boot decoded every showtime of the
 *  archive to get there: ~11 CPU-s of `ShowtimeCodec.read` at take-up (JFR). A film is written with its showtimes'
 *  digest, and a lean read leaves the showtimes on the server and in the reader. */
class ArchivedListingLeanReadSpec extends AnyFlatSpec with Matchers {

  private val at    = Instant.parse("2026-10-04T10:00:00Z")
  private val start = LocalDateTime.of(2026, 10, 5, 12, 0)

  private def film(cinema: Cinema, n: Int, showtimes: Int): CinemaMovie =
    CinemaMovie(Movie(s"Film $n", releaseYear = Some(2026)), cinema, Some(s"https://poster/$n"), Some(s"https://venue/film/$n"),
      Some("A synopsis " * 10), Seq("Actor One", "Actor Two"), Seq("Director"),
      (0 until showtimes).map(i => Showtime(start.plusMinutes(i * 95L), Some(s"https://venue/book/$n/$i?seats=1"), Some(s"Screen ${i % 9}"),
        List("2D"))), Map("tmdb" -> n.toString))

  private def encoded(cinema: Cinema, films: Seq[CinemaMovie]): BsonDocument = {
    val doc = new BsonDocument()
    ScrapeArchiveCodecs.registry.get(classOf[StoredScrapeDto]).encode(new BsonDocumentWriter(doc),
      StoredScrapeDto.fromSuccess(cinema, None, SuccessfulScrape(at, listingComplete = true, films)), EncoderContext.builder().build())
    doc
  }

  private def decode(registry: org.bson.codecs.configuration.CodecRegistry, doc: BsonDocument): StoredScrapeDto =
    registry.get(classOf[StoredScrapeDto]).decode(new BsonDocumentReader(doc), DecoderContext.builder().build())

  /** `doc` as the server hands it over with `films.showtimes` projected out. */
  private def withoutShowtimes(doc: BsonDocument): BsonDocument = {
    val copy = doc.clone()
    copy.getArray("films").asScala.foreach(_.asDocument().remove("showtimes"))
    copy
  }

  "a film written to the archive" should "carry its showtimes' digest" in {
    val films = Seq(film(Helios, 1, 3), film(Helios, 2, 0))
    val doc   = encoded(Helios, films)
    doc.getArray("films").asScala.map(_.asDocument().getInt32("showtimesDigest").getValue) shouldBe films.map(_.showtimes.##)
  }

  "a lean read of an archived listing" should "give every film without its showtimes, and their digest" in {
    val films = Seq(film(Helios, 1, 3), film(Helios, 2, 0))
    val lean  = decode(ScrapeArchiveCodecs.leanRegistry, withoutShowtimes(encoded(Helios, films)))
    lean.films.get.map(_.showtimes) shouldBe Seq(Nil, Nil)
    lean.films.get.map(_.showtimesDigest) shouldBe films.map(f => Some(f.showtimes.##))
    lean.films.get.map(_.copy(showtimes = Nil, showtimesDigest = None)) shouldBe
      decode(ScrapeArchiveCodecs.registry, encoded(Helios, films)).films.get.map(_.copy(showtimes = Nil, showtimesDigest = None))
  }

  it should "give a film stored before the digest existed none, for its reader to read it whole" in {
    val doc = withoutShowtimes(encoded(Helios, Seq(film(Helios, 1, 3))))
    doc.getArray("films").asScala.foreach(_.asDocument().remove("showtimesDigest"))
    decode(ScrapeArchiveCodecs.leanRegistry, doc).films.get.map(_.showtimesDigest) shouldBe Seq(None)
  }

  // A US archive: ~5,000 venues, ~60 KB each. A tenth of it, decoded from its bytes as a reply's documents are.
  it should "cost a fraction of the whole read on a US-sized archive" in {
    val venues = 500
    val rows   = (0 until venues).map(v => new RawBsonDocument(encoded(Helios, (0 until 20).map(n => film(Helios, v * 100 + n, 30))),
      ScrapeArchiveCodecs.registry.get(classOf[BsonDocument])))
    val projected = rows.map(r => new RawBsonDocument(withoutShowtimes(r.decode(ScrapeArchiveCodecs.registry.get(classOf[BsonDocument]))),
      ScrapeArchiveCodecs.registry.get(classOf[BsonDocument])))
    def measure(registry: org.bson.codecs.configuration.CodecRegistry, docs: Seq[RawBsonDocument]): (Double, Long, Long) = {
      val codec = registry.get(classOf[StoredScrapeDto])
      def once(): Int = docs.iterator.map(d => codec.decode(new BsonBinaryReader(d.getByteBuffer.asNIO()), DecoderContext.builder().build())
        .films.fold(0)(_.size)).sum
      (0 until 3).foreach(_ => once())   // warm
      val cpu   = tools.ThreadCpuClock.threadMxBean.nanos()
      val alloc = tools.ThreadAllocation.of(once())._2
      ((tools.ThreadCpuClock.threadMxBean.nanos() - cpu) / 1e9, alloc, docs.map(_.getByteBuffer.remaining.toLong).sum)
    }
    val (fullCpu, fullAlloc, fullBytes) = measure(ScrapeArchiveCodecs.registry, rows)
    val (leanCpu, leanAlloc, leanBytes) = measure(ScrapeArchiveCodecs.leanRegistry, projected)
    info(f"$venues venues: whole ${fullBytes / 1e6}%.1f MB read in ${fullCpu}%.3f CPU-s, ${fullAlloc / 1e6}%.0f MB allocated; " +
      f"lean ${leanBytes / 1e6}%.1f MB in ${leanCpu}%.3f CPU-s, ${leanAlloc / 1e6}%.0f MB")
    leanBytes should be < fullBytes / 3
    leanAlloc should be < fullAlloc / 3
  }
}
