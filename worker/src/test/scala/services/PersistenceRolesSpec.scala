package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.MovieRepository
import services.scrapes.ScrapeArchiveRepository
import services.tasks.TaskQueue
import tools.contracts.ReadOnlyCollaborators.writerTypedIn

/**
 * The worker's read-only collaborators keep the narrow persistence role they were split to
 * (CLAUDE.md, "Interface segregation"): each reads through a reader, a change feed, a producer, a
 * consumer or a monitor, and none takes back the writer — which would compile, since the writer
 * extends every role. A component that has come to need the writes moves off this list in the
 * same commit, saying why.
 */
class PersistenceRolesSpec extends AnyFlatSpec with Matchers {

  private def module(name: String): Class[?] = Class.forName(s"$name$$")

  "the read-only corpus components" should "take MovieRepositoryReader (and MovieChangeFeed), never MovieRepository" in {
    Seq(
      writerTypedIn(classOf[services.readmodel.ReadModelProjector], classOf[MovieRepository]),
      writerTypedIn(classOf[services.readmodel.ReadModelSweeper.Projection], classOf[MovieRepository], "movieRepository"),
      writerTypedIn(Class.forName("services.movies.MirroredCorpus"), classOf[MovieRepository], "repository"),
      writerTypedIn(classOf[services.movies.UnscreenedCleanup], classOf[MovieRepository]),
      writerTypedIn(module("services.movies.OrphanFilmStateSweep"), classOf[MovieRepository], "liveTmdbIds"),
      writerTypedIn(module("services.readmodel.ReadModelContentAudit"), classOf[MovieRepository], "differences")
    ).flatten shouldBe empty
  }

  "the scrape archive's readers" should "take ScrapeArchiveReader, never ScrapeArchiveRepository" in {
    Seq(
      writerTypedIn(classOf[services.closure.ClosureSweep], classOf[ScrapeArchiveRepository]),
      writerTypedIn(classOf[services.tasks.CinemaContentCensus], classOf[ScrapeArchiveRepository]),
      writerTypedIn(classOf[services.tasks.VenuePageHistory], classOf[ScrapeArchiveRepository])
    ).flatten shouldBe empty
    // The intake writes the listings it accepts, and only reads the archive's copy.
    writerTypedIn(classOf[services.identity.IdentityListingIntake], classOf[ScrapeArchiveRepository]) should have size 1
  }

  "the task queue's users" should "take the role they play — producer, consumer or monitor — never TaskQueue" in {
    Seq(
      classOf[services.tasks.TaskWorker], classOf[services.tasks.WorkerHeartbeat],
      classOf[services.venuepages.DetailReaper], classOf[services.tasks.EnrichmentReaper], classOf[services.tasks.RatingEnqueuer],
      classOf[services.tasks.RecheckedAudit], classOf[services.tasks.ScrapeReaper], classOf[services.tasks.QueueEnrichmentRetrigger],
      classOf[services.tasks.ChunkScrapePlanner], classOf[services.tasks.ChunkScrapeReaper], classOf[services.tasks.ChunkScrapeCoordinator],
      classOf[services.sharecards.ShareCardService], classOf[services.sharecards.CityCardService],
      classOf[services.sharecards.ShareCardBackfill], classOf[services.identityadapters.VenuePageWait]
    ).flatMap(writerTypedIn(_, classOf[TaskQueue])) ++
      writerTypedIn(module("services.venuepages.ReadVenuePageTasks"), classOf[TaskQueue], "enqueue") ++
      writerTypedIn(classOf[services.venuepages.DetailSchedule], classOf[TaskQueue]) shouldBe empty
  }

  "the guard" should "see a writer-typed parameter, so the checks above are not vacuous" in {
    writerTypedIn(classOf[services.corpus.StrandedSideRowsCleanup], classOf[MovieRepository]) should not be empty
    writerTypedIn(classOf[services.tasks.MeteredTaskQueue], classOf[TaskQueue]) should not be empty
  }
}
