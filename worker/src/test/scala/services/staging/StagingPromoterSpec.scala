package services.staging

import models.{Cinema, Helios, Multikino, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{DetailEnricher, FilmDetail}

class StagingPromoterSpec extends AnyFlatSpec with Matchers {

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
      onConcluded = concluded += _)

    promoter.promote(row) shouldBe false
    concluded shouldBe empty
  }

  it should "resolve a non-deferred cinema immediately (no detail enricher)" in {
    val (repo, row) = seeded(Multikino, "Inline", Some(2026))
    val concluded = scala.collection.mutable.ListBuffer.empty[StagingRecord]
    val promoter = new StagingPromoter(repo, Seq.empty, // Multikino isn't a DetailEnricher
      resolveStaging = (_, _, rec) => Some(rec.copy(tmdbId = Some(42))),
      onConcluded = concluded += _)

    promoter.promote(row) shouldBe true
    concluded should have size 1
  }
}
