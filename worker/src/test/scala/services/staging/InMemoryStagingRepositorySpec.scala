package services.staging

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger => LogbackLogger}
import ch.qos.logback.core.read.ListAppender
import models.{CinemaCityKinepolis, Helios, Multikino, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._

class InMemoryStagingRepositorySpec extends AnyFlatSpec with Matchers {

  private def slot(cinema: Source, title: String, year: Option[Int]): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](
      cinema -> SourceData(title = Some(title), rawTitle = Some(title), releaseYear = year)))

  /** Capture the WARN lines `body` emits from `InMemoryStagingRepository`'s logger. */
  private def warnsDuring(body: => Unit): Seq[String] = {
    val logger   = LoggerFactory.getLogger(classOf[InMemoryStagingRepository]).asInstanceOf[LogbackLogger]
    val appender = new ListAppender[ILoggingEvent]
    appender.start()
    logger.addAppender(appender)
    try body finally logger.detachAppender(appender)
    appender.list.asScala.toSeq.filter(_.getLevel == Level.WARN).map(_.getFormattedMessage)
  }

  "InMemoryStagingRepository" should "keep one row per (cinema, title, year) and read them back" in {
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Kumotry", Some(2026), slot(Helios, "Kumotry", Some(2026)))
    repository.upsert(Multikino, "Kumotry", Some(2026), slot(Multikino, "Kumotry", Some(2026)))

    val rows = repository.findAll()
    rows.map(r => (r.cinema, r.title, r.year)).toSet shouldBe
      Set((Helios, "Kumotry", Some(2026)), (Multikino, "Kumotry", Some(2026)))
  }

  it should "preserve enrichment when a re-scrape re-upserts an already-resolved newcomer row" in {
    // The "stuck in staging" bug: MovieCache.recordCinemaScrape re-diverts a
    // newcomer via `upsert` on EVERY scrape tick until it folds. A blind replace
    // nulled the tmdbId/imdbId/tmdbNoMatch the resolve step stamped between
    // resolution and fold — so the film folded UN-ENRICHED and the reaper
    // re-resolved it forever. The re-scrape must REFRESH the cinema slot without
    // clobbering enrichment.
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Denʹ istyny - UA", Some(2026), slot(Helios, "Denʹ istyny - UA", Some(2026)))

    // Resolve step stamps the row in place (under its persisted id).
    val resolved = repository.findAll().head
    repository.upsertRow(resolved.copy(record = resolved.record.copy(
      tmdbId = Some(1275779), imdbId = Some("tt15047880"))))

    // A later scrape tick re-diverts the still-incubating newcomer with a fresh blank slot.
    repository.upsert(Helios, "Denʹ istyny - UA", Some(2026), slot(Helios, "Denʹ istyny - UA", Some(2026)))

    val row = repository.findAll().head
    row.record.tmdbId shouldBe Some(1275779)
    row.record.imdbId shouldBe Some("tt15047880")
  }

  it should "preserve tmdbNoMatch=true when a re-scrape re-upserts a concluded newcomer row" in {
    // The decorated-title half of the same bug ("Kino bez barier: Ministranci"):
    // TMDB returns no match → the row is concluded with tmdbNoMatch=true. A
    // re-scrape must not reset it to false, or the reaper never folds it.
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Kino bez barier: Ministranci", None, slot(Helios, "Kino bez barier: Ministranci", None))
    val concluded = repository.findAll().head
    repository.upsertRow(concluded.copy(record = concluded.record.copy(tmdbNoMatch = true)))

    repository.upsert(Helios, "Kino bez barier: Ministranci", None, slot(Helios, "Kino bez barier: Ministranci", None))

    repository.findAll().head.record.tmdbNoMatch shouldBe true
  }

  it should "collapse case/diacritic variants of one cinema's title into a single row" in {
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Dzień objawienia", Some(2026), slot(Helios, "Dzień objawienia", Some(2026)))
    repository.upsert(Helios, "DZIEN OBJAWIENIA", Some(2026), slot(Helios, "DZIEN OBJAWIENIA", Some(2026)))
    repository.findAll() should have size 1
  }

  it should "delete a single cinema's row without touching the others" in {
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Kumotry", Some(2026), slot(Helios, "Kumotry", Some(2026)))
    repository.upsert(Multikino, "Kumotry", Some(2026), slot(Multikino, "Kumotry", Some(2026)))
    repository.delete(Helios, "Kumotry", Some(2026))
    repository.findAll().map(_.cinema) shouldBe Seq(Multikino)
  }

  it should "fire the upsert watcher with the decoded row" in {
    val repository = new InMemoryStagingRepository
    val seen = scala.collection.mutable.ListBuffer.empty[StagingRecord]
    repository.watchUpserts(seen += _)
    repository.upsert(CinemaCityKinepolis, "Kumotry", Some(2026), slot(CinemaCityKinepolis, "Kumotry", Some(2026)))
    seen.map(r => (r.cinema, r.title, r.year)) shouldBe Seq((CinemaCityKinepolis, "Kumotry", Some(2026)))
  }

  it should "WARN when the same (cinema, sanitized title) enters staging under a second year-key" in {
    val repository = new InMemoryStagingRepository
    val warnings = warnsDuring {
      repository.upsert(Helios, "Kumotry", Some(2025), slot(Helios, "Kumotry", Some(2025)))  // insert, no sibling
      repository.upsert(Helios, "Kumotry", Some(2026), slot(Helios, "Kumotry", Some(2026)))  // insert, sibling 2025 → WARN
    }
    val reentry = warnings.find(_.contains("RE-ENTRY (same film + cinema)"))
    reentry shouldBe defined
    reentry.get should (include("kumotry|2026") and include("kumotry|2025"))
  }

  it should "NOT warn on a per-tick re-divert of an already-staged row (insert vs update)" in {
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Kumotry", Some(2026), slot(Helios, "Kumotry", Some(2026)))  // first insert
    val warnings = warnsDuring {
      repository.upsert(Helios, "Kumotry", Some(2026), slot(Helios, "Kumotry", Some(2026)))  // same id → update
    }
    warnings.filter(_.contains("RE-ENTRY")) shouldBe empty
  }

  it should "NOT warn when a different cinema stages the same title (different _id prefix)" in {
    val repository = new InMemoryStagingRepository
    val warnings = warnsDuring {
      repository.upsert(Helios, "Kumotry", Some(2026), slot(Helios, "Kumotry", Some(2026)))
      repository.upsert(Multikino, "Kumotry", Some(2026), slot(Multikino, "Kumotry", Some(2026)))
    }
    warnings.filter(_.contains("RE-ENTRY")) shouldBe empty
  }

  // --- pure same-film-entry helpers (shared by both repository impls) -----------

  "StagingRepository.cinemaTitlePrefix" should "return the cinema|sanitize| prefix shared by year-variants" in {
    StagingRepository.cinemaTitlePrefix("Helios|kumotry|2026") shouldBe "Helios|kumotry|"
    StagingRepository.cinemaTitlePrefix("Helios|kumotry|")     shouldBe "Helios|kumotry|"  // yearless
  }

  "StagingRepository.sameFilmSiblings" should "match only same (cinema, sanitized title), other years" in {
    val ids = Seq("Helios|kumotry|2025", "Helios|kumotry|2026", "Helios|kumotrze|2026", "Multikino|kumotry|2026")
    StagingRepository.sameFilmSiblings("Helios|kumotry|2026", ids) shouldBe Seq("Helios|kumotry|2025")
  }

  "StagingRepository.duplicateEntryWarning" should "fire only on an INSERT that has same-film siblings" in {
    val id = "Helios|kumotry|2026"
    StagingRepository.duplicateEntryWarning(id, alreadyPresent = true,  Seq("Helios|kumotry|2025")) shouldBe None  // update
    StagingRepository.duplicateEntryWarning(id, alreadyPresent = false, Seq.empty)                   shouldBe None  // no sibling
    val warned = StagingRepository.duplicateEntryWarning(id, alreadyPresent = false, Seq("Helios|kumotry|2025"))
    warned shouldBe defined
    warned.get should (include(id) and include("Helios|kumotry|2025"))
  }
}
