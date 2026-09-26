package integration

import models.{Cinema, CinemaCityKinepolis, Helios, Multikino}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.ObservableFuture
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{MovieRepository, ScreeningsRepository, SlotsRepository}
import tools.ConcurrentInstances
import tools.ConcurrentInstances.{race, rounds, successes}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Two workers of ONE country over one corpus — the old pod and the new one of a deploy that
 * overlaps them, each with the roster its build knows — landing their scrapes into staging and
 * folding them at the same moment. Each worker is its own Mongo client, so its fold transactions
 * and retries are its own (`FoldFixture.on`), unlike `StagingFoldConcurrentTmdbRaceIntegrationSpec`,
 * whose racing folds share one folder.
 *
 * The rosters overlap on one cinema (both builds scrape Helios) and differ on one each (Multikino
 * was dropped, Cinema City added), and each lands a decorated spelling the other does not — the
 * Lalka shape (a42086081), where separate fold groups converge on one tmdbId.
 *
 * The worker Deployment is `Recreate` with one replica (pinned by `WebRolloutAvailabilitySpec`),
 * so two workers of a country overlap only when that pin breaks or a `worker/Test/runMain
 * scripts.*` tool writes the corpus beside the running worker. What this holds is that the
 * Mongo-level guards (the fold transaction, its retry, the unique indexes) keep one film per
 * identity across processes — the in-process `withIdLock` cannot help there.
 */
class RollingWorkersFoldIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private final case class Landing(cinema: Cinema, title: String)

  /** A title of this round's own: the fold pulls siblings by tmdbId over the WHOLE collection. */
  private def film(round: Int): String = s"Wydmy ${(('a' + round - 1).toChar.toString) * 3}"

  "two workers of one country landing and folding one film at once" should
    "leave one film, carrying every cinema either roster scraped, and consume all of staging" in
    ConcurrentInstances.withInstances(mongoTarget, "rolling-workers-fold") { instances =>
      val workers = instances.map(FoldFixture.on(mongoTarget))
      rounds(8, tools.ConcurrentInstances.baseSeed(configuration)) { round =>
        val title  = film(round.number)
        val tmdbId = 612000 + round.number
        val oldRoster = Seq(Landing(Multikino, title), Landing(Helios, title), Landing(Helios, s"Ladies Night - $title"))
        val newRoster = Seq(Landing(Helios, title), Landing(CinemaCityKinepolis, title), Landing(CinemaCityKinepolis, s"Kino kobiet: $title"))
        val landed = Seq(oldRoster, newRoster).zip(workers).map { case (roster, worker) =>
          () => {
            val ids = roster.map(l => worker.seedStagingRow(l.cinema.displayName, l.title, Some(2026), tmdbId))
            val folder = worker.folder(maxRetries = 8)
            roster.map(_.title).distinct.foreach(folder.foldGroup(_))
            ids
          }
        }
        val stagingIds = successes(race(landed, Some(round))).flatten.distinct

        val films = Await.result(workers.head.movies.find(Filters.eq("tmdbId", tmdbId)).toFuture(), 10.seconds)
          .flatMap(_.get("_id").map(_.asString().getValue))
        withClue(s"films for tmdbId $tmdbId: $films — two workers must not mint one each: ") {
          films should have size 1
        }
        withClue("a worker's fold must not drop a cinema the other worker's fold wrote: ") {
          workers.head.slots.findForFilm(films.head).keySet shouldBe (oldRoster ++ newRoster).map(_.cinema.displayName).toSet
        }
        withClue("every landed staging row is consumed by one fold or the other: ") {
          stagingIds.filter(workers.head.stagingRowExists) shouldBe empty
        }
      }
    }

  // The new pod boots while the old one serves, and every repository builds its indexes on boot.
  // Once the corpus carries them, a boot must find them and leave them alone: a drop-and-rebuild,
  // even of an identical index, leaves a window with no uniqueness in which the OTHER pod's writes
  // are unguarded (the userStates boot did exactly this until e098b3b62).
  "a worker booting while another serves" should "rebuild no index the corpus already carries" in
    ConcurrentInstances.withInstances(mongoTarget, "rolling-workers-boot") { instances =>
      def boot(worker: FoldFixture.Handles): Unit = {
        worker.splitAwareRepository.enabled shouldBe true
        worker.slots.findForFilm("warm-up")
        worker.screenings.findForFilm("warm-up")
        ()
      }
      val Seq(old, fresh) = instances.map(FoldFixture.on(mongoTarget))
      boot(old)
      rounds(3, tools.ConcurrentInstances.baseSeed(configuration)) { round =>
        val serving = () => { old.seedStagingRow(Helios.displayName, film(round.number), Some(2026), 613000 + round.number); old.folder().foldGroup(film(round.number)); () }
        successes(race(Seq(() => boot(fresh), serving), Some(round)))
        val drops = for {
          instance   <- instances
          collection <- Seq(MovieRepository.Collection, SlotsRepository.Collection, ScreeningsRepository.Collection)
          command    <- instance.indexCommands(collection) if command.name == "dropIndexes"
        } yield s"${instance.name}: $command"
        drops shouldBe empty
      }
    }
}
