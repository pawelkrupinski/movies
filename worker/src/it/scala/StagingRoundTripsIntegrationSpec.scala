package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{Helios, HeliosMagnolia, Multikino, MultikinoPasazGrunwaldzki, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.{MongoStagingRepository, StagingRepository}
import tools.ConcurrentInstances

/**
 * The staging drain's per-ROW round trips, made per-GROUP — with the rows each lands unchanged.
 *
 * A convergence replay's staging drain runs serially (one claimant, so its seeded order is the
 * only nondeterminism), which makes it a file of Mongo round trips; wall-clock samples of the UK
 * order-independence passes (2026-10-03) put ~32% of it in the resolve step re-stamping a group
 * one `replaceOne` per row, and ~27% in the fold deleting the group's staging rows one
 * `deleteOne` per row inside its transaction. Production pays the same, four claimants wide.
 *
 * Counted on the wire, through the command log a `ConcurrentInstances` pod keeps: a regression
 * to per-row writes is a test failure, not a slower nightly. Requires MONGODB_URI; skips otherwise.
 */
class StagingRoundTripsIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  /** The commands `pod` sent while `body` ran, as `command -> collection`. */
  private def sentDuring(pod: ConcurrentInstances.Instance)(body: => Unit): Seq[(String, String)] = {
    val before = pod.commands.size
    body
    pod.commands.drop(before).map(c => c.name -> c.collection.getOrElse(""))
  }

  private val cinemas = Seq(Multikino, Helios, HeliosMagnolia, MultikinoPasazGrunwaldzki)

  "upsertRows" should "re-stamp a resolved group in one round trip, landing exactly what upsertRow would" in {
    ConcurrentInstances.withInstances(mongoTarget, "staging-round-trips-upsert", count = 1) { case Seq(pod) =>
      val repository = new MongoStagingRepository(Some(pod.database), normalizer = titleNormalizer)
      val title = "Round Trip Stamp"
      cinemas.foreach(c => repository.upsert(c, title, Some(2026), MovieRecord(data = Map(c -> SourceData(title = Some(title))))))
      val group = repository.findAll()
      group should have size cinemas.size
      val stamped = group.map(r => r.copy(record = r.record.copy(tmdbId = Some(5150), imdbId = Some("tt5150515"))))

      val sent = sentDuring(pod)(repository.upsertRows(stamped))

      withClue("one bulk write for the group, not a replace per row: ") {
        sent.count(_ == ("update" -> StagingRepository.Collection)) shouldBe 1
      }
      repository.findAll().map(r => r.id -> (r.record.tmdbId, r.record.imdbId, r.record.data)).toMap shouldBe
        stamped.map(r => r.id -> (Some(5150), Some("tt5150515"), r.record.data)).toMap
      withClue("the rows stay findable under their anchor, as upsertRow's index keeps them: ") {
        repository.findByAnchor(titleNormalizer.sanitize(title)).map(_.id).toSet shouldBe stamped.map(_.id).toSet
      }
    }
  }

  "a staging fold" should "delete the group's staging rows in one round trip inside its transaction" in {
    ConcurrentInstances.withInstances(mongoTarget, "staging-round-trips-fold", count = 1) { case Seq(pod) =>
      val fold = FoldFixture.on(mongoTarget)(pod)
      val title = "Round Trip Fold"
      val rows  = cinemas.map(c => fold.seedStagingRow(c.displayName, title, Some(2026), tmdbId = 6160))
      val folder = fold.folder()

      val sent = sentDuring(pod)(folder.foldGroup(title))

      rows.filter(fold.stagingRowExists) shouldBe empty
      fold.filmIds(titleNormalizer.sanitize(title)) should have size 1
      withClue("one delete for the group's staging rows, not a deleteOne per row: ") {
        sent.count(_ == ("delete" -> StagingRepository.Collection)) shouldBe 1
      }
    }
  }
}
