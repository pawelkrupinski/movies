package services.contracts

import services.movies.CountingNormalizer
import models.{Helios, KinoMuza, Multikino, MovieRecord, Source, SourceData}
import org.mongodb.scala.SingleObservableFuture
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.{InMemoryStagingRepository, StagingRepository}
import tools.contracts.Implementations
import tools.costs.CostScaling
import tools.{Env, IsolatedMongoDatabase}

import scala.concurrent.Await
import scala.concurrent.duration.*

/**
 * ONE behaviour suite for [[StagingRepository]], run against every implementation found on
 * the class path — `MongoStagingRepository` and the `InMemoryStagingRepository` the
 * convergence harness and every unit spec run.
 *
 * Two kinds of drift, both seen: the ANSWER (every narrow read — `findByAnchor`,
 * `holdsAnchor`, `cinemasUnder`, `findByCinema`, `findByCinemaAndAnchor` — must say exactly
 * what filtering `findAll` says), and the COST. The in-memory fake answered `findByAnchor` by
 * walking the whole backlog (42d2ecc6f) while Mongo read an index, so the harness paid a
 * quadratic production does not — and, the other way round, would have hidden one production
 * did pay. Cost is counted as `sanitize` calls, one per row a read touches, never timed.
 */
class StagingRepositoryContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")

  private lazy val database = IsolatedMongoDatabase.open(Env.get("MONGODB_URI").get, "staging-contract")
  override protected def afterAll(): Unit = try IsolatedMongoDatabase.drop(database) finally super.afterAll()

  /** Counts `sanitize`, which every per-row walk calls once per row. */

  /** A fresh, empty repository and the normalizer it counts with. */
  private def fresh(cls: Class[? <: StagingRepository]): (StagingRepository, CountingNormalizer) = {
    Await.result(database.getCollection(StagingRepository.Collection).drop().toFuture(), 30.seconds)
    val normalizer = new CountingNormalizer
    val repository = Implementations.construct(cls, _.getTypeName match {
      case "scala.Option<org.mongodb.scala.MongoDatabase>" => Some(Some(database))
      case "services.movies.TitleNormalizer"               => Some(normalizer)
      case _                                               => None
    }).fold(missing => fail(missing), identity)
    (repository, normalizer)
  }

  private def slot(cinema: Source, title: String, year: Option[Int]): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](cinema -> SourceData(title = Some(title), rawTitle = Some(title), releaseYear = year)))

  private def stage(repository: StagingRepository, cinema: Source, title: String, year: Option[Int]): Unit =
    repository.upsert(cinema, title, year, slot(cinema, title, year))

  private val implementations =
    Implementations.of(classOf[StagingRepository], classOf[StagingRepository], classOf[InMemoryStagingRepository])

  "the StagingRepository implementations" should "include the in-memory fake and the Mongo repository" in {
    implementations.map(_.getSimpleName) should contain allOf ("InMemoryStagingRepository", "MongoStagingRepository")
  }

  implementations.foreach { cls =>
    val name = cls.getSimpleName

    it should s"[$name] answer every narrow read exactly as filtering findAll does" in {
      val (repository, normalizer) = fresh(cls)
      stage(repository, Helios,    "Kumotry",   Some(2026))
      stage(repository, Multikino, "KUMOTRY",   Some(2025))
      stage(repository, KinoMuza,  "Kumotry",   Some(2026))
      stage(repository, Helios,    "Other Film", Some(2026))
      repository.delete(KinoMuza, "Kumotry", Some(2026))

      val all     = repository.findAll()
      val anchors = all.map(row => normalizer.sanitize(row.title)).distinct :+ normalizer.sanitize("Never Staged")
      all.map(_.cinema).toSet shouldBe Set(Helios, Multikino)
      anchors.foreach { anchor =>
        val group = all.filter(row => normalizer.sanitize(row.title) == anchor)
        withClue(s"anchor '$anchor': ") {
          repository.findByAnchor(anchor).map(_.id)  shouldBe group.map(_.id)
          repository.holdsAnchor(anchor)             shouldBe group.nonEmpty
          repository.cinemasUnder(anchor)            shouldBe group.map(_.cinema).toSet
          Seq(Helios, Multikino, KinoMuza).foreach { cinema =>
            repository.findByCinemaAndAnchor(cinema, anchor).map(_.id) shouldBe group.filter(_.cinema == cinema).map(_.id)
          }
        }
      }
      Seq(Helios, Multikino, KinoMuza).foreach { cinema =>
        repository.findByCinema(cinema).map(_.id) shouldBe all.filter(_.cinema == cinema).map(_.id)
      }
    }

    it should s"[$name] read one film's group without walking the rest of the backlog" in {
      /** `sanitize` calls ONE group read makes against a backlog of `backlog` other films. */
      def groupReadCost(backlog: Int): Long = {
        val (repository, normalizer) = fresh(cls)
        (1 to backlog).foreach(n => stage(repository, Helios, s"Backlog Film $n", Some(2026)))
        stage(repository, Helios,    "Kumotry", Some(2026))
        stage(repository, Multikino, "KUMOTRY", Some(2025))
        val anchor = normalizer.sanitize("Kumotry")
        repository.holdsAnchor("warm-up")   // any one-off index build is not the per-read cost
        normalizer.reset()
        repository.findByAnchor(anchor) should have size 2
        repository.holdsAnchor(anchor)  shouldBe true
        repository.cinemasUnder(anchor) shouldBe Set(Helios, Multikino)
        repository.findByCinemaAndAnchor(Helios, anchor) should have size 1
        normalizer.calls.toLong
      }
      CostScaling.assertIndependent("sanitize calls reading a 2-row group beside a backlog of other films", n = 20, factor = 10)(groupReadCost)
    }
  }
}
