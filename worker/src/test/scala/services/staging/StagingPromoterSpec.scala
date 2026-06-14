package services.staging

import ch.qos.logback.classic.{Level, Logger => LogbackLogger}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import models.{Cinema, Helios, Multikino, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory
import services.cinemas.{DetailEnricher, FilmDetail}
import services.movies.MovieService

import scala.jdk.CollectionConverters._

class StagingPromoterSpec extends AnyFlatSpec with Matchers {

  /** Capture the INFO+ messages `StagingPromoter` logs while `body` runs, by
   *  attaching a logback list appender to its logger. */
  private def stagingLogs[T](body: => T): Seq[String] = {
    val lb       = LoggerFactory.getLogger("services.staging.StagingPromoter").asInstanceOf[LogbackLogger]
    val appender = new ListAppender[ILoggingEvent]
    appender.start()
    lb.addAppender(appender)
    val prevLevel = lb.getLevel
    lb.setLevel(Level.INFO)
    try { body; appender.list.asScala.toSeq.map(_.getFormattedMessage) }
    finally { lb.detachAppender(appender); lb.setLevel(prevLevel) }
  }

  private class FakeEnricher(val cinema: Cinema, detail: Option[FilmDetail], defer: Boolean = true)
    extends DetailEnricher {
    def detailGroup = "fake"
    override def defersTmdbResolution: Boolean = defer
    def fetchFilmDetail(ref: String): Option[FilmDetail] = detail
  }

  private def listingRow(cinema: Cinema, title: String, year: Option[Int]): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](
      cinema -> SourceData(title = Some(title), releaseYear = year, filmUrl = Some(s"https://x/$title"))))

  private def seeded(cinema: Cinema, title: String, year: Option[Int]): (InMemoryStagingRepo, StagingRecord) = {
    val repo = new InMemoryStagingRepo
    repo.upsert(cinema, title, year, listingRow(cinema, title, year))
    (repo, repo.findAll().head)
  }

  "promote" should "fetch detail FIRST, then resolve with the detail hints, and conclude" in {
    val (repo, row) = seeded(Helios, "Newcomer", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("A plot"), director = Seq("Jane Doe"))))
    var resolveSawDirector = false
    val concluded = scala.collection.mutable.ListBuffer.empty[StagingRecord]
    val promoter = new StagingPromoter(repo, Seq(enricher),
      resolveStaging = (_, _, rec) => {
        resolveSawDirector = rec.data.get(Helios).exists(_.director.contains("Jane Doe"))
        Some(rec.copy(tmdbId = Some(1275779)))
      },
      recoverImdbId = (_, _) => None,
      onConcluded = concluded += _)

    promoter.promote(row) shouldBe true
    resolveSawDirector shouldBe true                                   // detail ran before resolve
    repo.findAll().head.record.data(Helios).synopsis shouldBe Some("A plot")
    repo.findAll().head.record.tmdbId shouldBe Some(1275779)
    concluded.map(_.title) shouldBe Seq("Newcomer")
  }

  it should "conclude a definitive no-match (tmdbNoMatch) and fire onConcluded" in {
    val (repo, row) = seeded(Helios, "Obscure", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("x"))))
    val concluded = scala.collection.mutable.ListBuffer.empty[StagingRecord]
    val promoter = new StagingPromoter(repo, Seq(enricher),
      resolveStaging = (_, _, rec) => Some(rec.copy(tmdbNoMatch = true)),
      recoverImdbId = (_, _) => None,
      onConcluded = concluded += _)

    promoter.promote(row) shouldBe true
    repo.findAll().head.record.tmdbNoMatch shouldBe true
    concluded should have size 1
  }

  it should "NOT resolve a deferred cinema until its detail lands" in {
    val (repo, row) = seeded(Helios, "Pending", Some(2026))
    val enricher = new FakeEnricher(Helios, detail = None) // detail fetch keeps failing
    var resolveCalled = false
    val promoter = new StagingPromoter(repo, Seq(enricher),
      resolveStaging = (_, _, _) => { resolveCalled = true; None },
      recoverImdbId = (_, _) => None,
      onConcluded = _ => ())

    promoter.promote(row) shouldBe false
    resolveCalled shouldBe false
    repo.findAll().head.record.tmdbConcluded shouldBe false
  }

  it should "leave a row unconcluded on a transient TMDB failure (retry next pass)" in {
    val (repo, row) = seeded(Helios, "Flaky", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("x"))))
    val concluded = scala.collection.mutable.ListBuffer.empty[StagingRecord]
    val promoter = new StagingPromoter(repo, Seq(enricher),
      resolveStaging = (_, _, _) => None, // transient
      recoverImdbId = (_, _) => None,
      onConcluded = concluded += _)

    promoter.promote(row) shouldBe false
    concluded shouldBe empty
  }

  it should "resolve a non-deferred cinema immediately (no detail enricher)" in {
    val (repo, row) = seeded(Multikino, "Inline", Some(2026))
    val concluded = scala.collection.mutable.ListBuffer.empty[StagingRecord]
    val promoter = new StagingPromoter(repo, Seq.empty, // Multikino isn't a DetailEnricher
      resolveStaging = (_, _, rec) => Some(rec.copy(tmdbId = Some(42))),
      recoverImdbId = (_, _) => None,
      onConcluded = concluded += _)

    promoter.promote(row) shouldBe true
    concluded should have size 1
  }

  it should "recover a missing imdbId inline when TMDB resolved but shipped no cross-reference" in {
    // A recent Polish film: TMDB hit (tmdbId set) but no imdb cross-reference, so
    // the resolved record has imdbId empty. The direct path recovers it via the
    // async `ImdbIdMissing` chain; staging rows never enter the cache, so the
    // promoter must recover the id INLINE before folding (else the merged movies
    // row — and its IMDb/RT/Metacritic ratings — would be permanently id-less).
    val (repo, row) = seeded(Helios, "Pucio", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("x"))))
    var searchedFor = Option.empty[String]
    val promoter = new StagingPromoter(repo, Seq(enricher),
      resolveStaging = (_, _, rec) => Some(rec.copy(tmdbId = Some(1645035))), // hit, but imdbId empty
      recoverImdbId = (search, _) => { searchedFor = Some(search); Some("tt42003604") },
      onConcluded = _ => ())

    promoter.promote(row) shouldBe true
    searchedFor shouldBe Some(MovieService.apiQuery("Pucio"))   // searched the (api-query'd) title
    repo.findAll().head.record.imdbId shouldBe Some("tt42003604")
  }

  it should "not call the imdbId recovery when TMDB already shipped a cross-reference" in {
    val (repo, row) = seeded(Helios, "HasImdb", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("x"))))
    var recoverCalled = false
    val promoter = new StagingPromoter(repo, Seq(enricher),
      resolveStaging = (_, _, rec) => Some(rec.copy(tmdbId = Some(7), imdbId = Some("tt0000007"))),
      recoverImdbId = (_, _) => { recoverCalled = true; None },
      onConcluded = _ => ())

    promoter.promote(row) shouldBe true
    recoverCalled shouldBe false
    repo.findAll().head.record.imdbId shouldBe Some("tt0000007")
  }

  it should "log each promotion step at INFO (the formerly-silent path is now traceable)" in {
    val (repo, row) = seeded(Helios, "Loggable", Some(2026))
    val enricher = new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("x"), director = Seq("Dir"))))
    val promoter = new StagingPromoter(repo, Seq(enricher),
      resolveStaging = (_, _, rec) => Some(rec.copy(tmdbId = Some(99))),
      recoverImdbId = (_, _) => None,
      onConcluded = _ => ())

    val logs = stagingLogs(promoter.promote(row))

    logs.exists(_.contains(s"Staging: promoting 'Loggable' (cinemas: ${Helios.displayName})")) shouldBe true
    logs.exists(m => m.contains("Loggable") && m.contains(s"detail from ${Helios.displayName}")) shouldBe true
    logs.exists(m => m.contains("Loggable") && m.contains("concluded") && m.contains("tmdbId=99")) shouldBe true
  }
}
