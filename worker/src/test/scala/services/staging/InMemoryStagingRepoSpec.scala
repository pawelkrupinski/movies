package services.staging

import models.{CinemaCityKinepolis, Helios, Multikino, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InMemoryStagingRepoSpec extends AnyFlatSpec with Matchers {

  private def slot(cinema: Source, title: String, year: Option[Int]): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](
      cinema -> SourceData(title = Some(title), rawTitle = Some(title), releaseYear = year)))

  "InMemoryStagingRepo" should "keep one row per (cinema, title, year) and read them back" in {
    val repo = new InMemoryStagingRepo
    repo.upsert(Helios, "Kumotry", Some(2026), slot(Helios, "Kumotry", Some(2026)))
    repo.upsert(Multikino, "Kumotry", Some(2026), slot(Multikino, "Kumotry", Some(2026)))

    val rows = repo.findAll()
    rows.map(r => (r.cinema, r.title, r.year)).toSet shouldBe
      Set((Helios, "Kumotry", Some(2026)), (Multikino, "Kumotry", Some(2026)))
  }

  it should "collapse case/diacritic variants of one cinema's title into a single row" in {
    val repo = new InMemoryStagingRepo
    repo.upsert(Helios, "Dzień objawienia", Some(2026), slot(Helios, "Dzień objawienia", Some(2026)))
    repo.upsert(Helios, "DZIEN OBJAWIENIA", Some(2026), slot(Helios, "DZIEN OBJAWIENIA", Some(2026)))
    repo.findAll() should have size 1
  }

  it should "delete a single cinema's row without touching the others" in {
    val repo = new InMemoryStagingRepo
    repo.upsert(Helios, "Kumotry", Some(2026), slot(Helios, "Kumotry", Some(2026)))
    repo.upsert(Multikino, "Kumotry", Some(2026), slot(Multikino, "Kumotry", Some(2026)))
    repo.delete(Helios, "Kumotry", Some(2026))
    repo.findAll().map(_.cinema) shouldBe Seq(Multikino)
  }

  it should "fire the upsert watcher with the decoded row" in {
    val repo = new InMemoryStagingRepo
    val seen = scala.collection.mutable.ListBuffer.empty[StagingRecord]
    repo.watchUpserts(seen += _)
    repo.upsert(CinemaCityKinepolis, "Kumotry", Some(2026), slot(CinemaCityKinepolis, "Kumotry", Some(2026)))
    seen.map(r => (r.cinema, r.title, r.year)) shouldBe Seq((CinemaCityKinepolis, "Kumotry", Some(2026)))
  }
}
