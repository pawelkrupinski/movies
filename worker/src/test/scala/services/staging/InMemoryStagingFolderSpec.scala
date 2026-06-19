package services.staging

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger => LogbackLogger}
import ch.qos.logback.core.read.ListAppender
import models.{Helios, Multikino, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory
import services.movies.{CacheKey, InMemoryMovieRepository}

import scala.jdk.CollectionConverters._

class InMemoryStagingFolderSpec extends AnyFlatSpec with Matchers {

  private def resolved(cinema: Source, year: Int): MovieRecord =
    MovieRecord(tmdbId = Some(1454157), data = Map[Source, SourceData](
      cinema -> SourceData(title = Some("Kumotry"), releaseYear = Some(year)),
      Tmdb   -> SourceData(title = Some("Kumotry"), releaseYear = Some(year))))

  "foldGroup" should "move a film's staging rows into movies and delete them" in {
    val staging  = new InMemoryStagingRepository
    val movies   = new InMemoryMovieRepository
    staging.upsert(Helios, "Kumotry", Some(2026), resolved(Helios, 2026))
    staging.upsert(Multikino, "Kumotry", Some(2026), resolved(Multikino, 2026))
    val folder = new InMemoryStagingFolder(staging, movies)

    folder.foldGroup("Kumotry")

    staging.findAll() shouldBe empty
    val rows = movies.findAll()
    rows should have size 1
    rows.head.record.tmdbId shouldBe Some(1454157)
    rows.head.record.data.keySet shouldBe Set(Helios, Multikino, Tmdb)
  }

  it should "collapse a film's ±1-year variants into one movies row (settle absorbed into the fold)" in {
    // Cinema City reports 'Kumotry' at the production year 2025, the rest at the
    // release year 2026 (tmdbYear 2026). The group-scoped fold settles them into
    // ONE row keyed to 2026 — no separate settle pass.
    val staging = new InMemoryStagingRepository
    val movies  = new InMemoryMovieRepository
    staging.upsert(Multikino, "Kumotry", Some(2025), resolved(Multikino, 2025).copy(
      data = resolved(Multikino, 2025).data + (Tmdb -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))
    staging.upsert(Helios, "Kumotry", Some(2026), resolved(Helios, 2026))

    new InMemoryStagingFolder(staging, movies).foldGroup("Kumotry")

    val rows = movies.findAll()
    rows should have size 1
    rows.head.year shouldBe Some(2026)
    rows.head.record.data.keySet shouldBe Set(Multikino, Helios, Tmdb)
  }

  it should "be a no-op when no staging rows match (already folded)" in {
    val staging = new InMemoryStagingRepository
    val movies  = new InMemoryMovieRepository
    new InMemoryStagingFolder(staging, movies).foldGroup("Ghost") shouldBe empty
    movies.findAll() shouldBe empty
  }

  it should "return the brand-new film as a promotion so its ratings can be scheduled" in {
    val staging = new InMemoryStagingRepository
    val movies  = new InMemoryMovieRepository
    staging.upsert(Helios, "Kumotry", Some(2026), resolved(Helios, 2026))

    val promotions = new InMemoryStagingFolder(staging, movies).foldGroup("Kumotry")

    promotions.map(_._1) shouldBe Seq(CacheKey("Kumotry", Some(2026)))
    promotions.head._2.tmdbId shouldBe Some(1454157)
  }

  it should "NOT return a promotion when the staging row merges into an existing movie" in {
    val staging = new InMemoryStagingRepository
    val movies  = new InMemoryMovieRepository
    movies.upsert("Kumotry", Some(2026), resolved(Helios, 2026))   // already in movies
    staging.upsert(Multikino, "Kumotry", Some(2026), resolved(Multikino, 2026))

    val promotions = new InMemoryStagingFolder(staging, movies).foldGroup("Kumotry")

    promotions shouldBe empty                                       // merged, not promoted
    movies.findAll().head.record.data.keySet shouldBe Set(Helios, Multikino, Tmdb)  // merge still happened
  }

  it should "WARN when a known film (tmdbId already in movies) re-incubates and folds as a new row" in {
    // 'Tangled' is already in `movies` as the Polish 'Zaplątani' (tmdbId 38757). A
    // staging row of the English title sanitizes to a DIFFERENT key, so the divert
    // gate missed it; it folds as a brand-new row carrying the SAME tmdbId — a
    // re-entry the folder must flag (the duplicate the settle then merges).
    val staging = new InMemoryStagingRepository
    val movies  = new InMemoryMovieRepository
    movies.upsert("Zaplątani", Some(2010), MovieRecord(tmdbId = Some(38757),
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Zaplątani"), releaseYear = Some(2010)))))
    staging.upsert(Multikino, "Tangled", Some(2010), MovieRecord(tmdbId = Some(38757),
      data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Tangled"), releaseYear = Some(2010)),
        Tmdb      -> SourceData(title = Some("Tangled"), releaseYear = Some(2010)))))

    val logger   = LoggerFactory.getLogger(classOf[InMemoryStagingFolder]).asInstanceOf[LogbackLogger]
    val appender = new ListAppender[ILoggingEvent]
    appender.start()
    logger.addAppender(appender)
    try new InMemoryStagingFolder(staging, movies).foldGroup("Tangled")
    finally logger.detachAppender(appender)

    val warnings = appender.list.asScala.filter(_.getLevel == Level.WARN).map(_.getFormattedMessage)
    val reentry  = warnings.find(_.contains("Staging RE-ENTRY"))
    reentry shouldBe defined
    reentry.get should (include("tmdbId=38757") and include("zaplatani|2010"))
  }
}
