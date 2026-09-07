package services.staging

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{StagingFilmEnriched, TaskFinished}
import services.movies.{CacheKey, InMemoryMovieRepository}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.tasks.TaskType

import scala.collection.mutable.ListBuffer

/**
 * The fold-on-conclusion policy that used to be an inline lambda in the worker's
 * composition root: which rows a `StagingFilmEnriched` fold reads, and which of
 * its results are announced as brand-new films.
 */
class FoldOnStagingEnrichedSpec extends AnyFlatSpec with Matchers {

  private def staged(title: String, tmdbId: Option[Int] = Some(1275779)): MovieRecord =
    MovieRecord(tmdbId = tmdbId,
      data = Map[Source, SourceData](Helios -> SourceData(title = Some(title), filmUrl = Some("u"))))

  /** A folder that only records what it was asked to fold. */
  private class RecordingFolder(answer: Seq[(CacheKey, MovieRecord)] = Seq.empty) extends StagingFolder {
    val asked = ListBuffer.empty[(String, Option[Set[String]])]
    def foldGroup(cleanTitle: String, candidateIds: Option[Set[String]]): Seq[(CacheKey, MovieRecord)] = {
      asked += cleanTitle -> candidateIds
      answer
    }
  }

  "on StagingFilmEnriched" should "fold the film's group into movies and announce the newcomer" in {
    val staging   = new InMemoryStagingRepository
    val movies    = new InMemoryMovieRepository
    val announced = ListBuffer.empty[CacheKey]
    val subscriber = new FoldOnStagingEnriched(
      new InMemoryStagingFolder(staging, movies), staging, (key, _) => announced += key)
    staging.upsert(Helios, "Newcomer", Some(2026), staged("Newcomer"))

    subscriber.onStagingFilmEnriched(StagingFilmEnriched("Newcomer"))

    movies.findAll().map(_.title) shouldBe Seq("Newcomer")
    staging.findAll() shouldBe empty
    announced shouldBe Seq(CacheKey("Newcomer", Some(2026), titleNormalizer))
  }

  it should "hand the fold only the rows sharing the event's anchor" in {
    val staging = new InMemoryStagingRepository
    val folder  = new RecordingFolder
    val subscriber = new FoldOnStagingEnriched(folder, staging, (_, _) => fail("nothing was folded"))
    staging.upsert(Helios, "Dune",  Some(2026), staged("Dune"))
    staging.upsert(Helios, "Dune",  Some(2025), staged("Dune"))
    staging.upsert(Helios, "Other", Some(2026), staged("Other"))
    val duneIds = staging.findAll().filter(_.title == "Dune").map(_.id).toSet
    duneIds should have size 2

    subscriber.onStagingFilmEnriched(StagingFilmEnriched("Dune"))

    folder.asked shouldBe Seq("Dune" -> Some(duneIds))
  }

  it should "announce exactly the brand-new films the folder reports, and nothing on a merge" in {
    val staging   = new InMemoryStagingRepository
    val announced = ListBuffer.empty[CacheKey]
    val newcomer  = CacheKey("Newcomer", Some(2026), titleNormalizer)
    // A merge into an existing `movies` row reports no promotion: the row keeps its
    // ratings, so the subscriber must leave it alone.
    val merged = new FoldOnStagingEnriched(new RecordingFolder(Seq.empty), staging, (key, _) => announced += key)
    merged.onStagingFilmEnriched(StagingFilmEnriched("Newcomer"))
    announced shouldBe empty

    val fresh = new FoldOnStagingEnriched(
      new RecordingFolder(Seq(newcomer -> staged("Newcomer"))), staging, (key, _) => announced += key)
    fresh.onStagingFilmEnriched(StagingFilmEnriched("Newcomer"))
    announced shouldBe Seq(newcomer)
  }

  it should "ignore every other event" in {
    val subscriber = new FoldOnStagingEnriched(
      new RecordingFolder, new InMemoryStagingRepository, (_, _) => fail("nothing was folded"))
    subscriber.onStagingFilmEnriched.isDefinedAt(TaskFinished(TaskType.StagingFold, "k", Map.empty)) shouldBe false
  }
}
