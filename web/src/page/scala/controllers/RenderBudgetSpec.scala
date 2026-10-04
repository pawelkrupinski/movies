package controllers

import models.{City, Country}
import org.apache.pekko.util.ByteString
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.http.HttpEntity
import play.api.mvc.{Action, AnyContent}
import play.api.test.FakeRequest
import tools.FixtureTestWiring
import tools.costs.{AllocationMeter, PerformanceBudget, PerformanceBudgets}

import java.time.{Clock, LocalDateTime, ZoneOffset}

/**
 * What each page and payload of the fixture corpus allocates to render, held to [[PerformanceBudgets]].
 *
 * Every regression these budgets name was found by luck, in production or a review: the schedule
 * cache rebuilt on every render (1 MB → 40 KB a render), every film's API JSON rebuilt for the handful
 * that moved (26.5 → 2.8 MB a Warsaw `/api/repertoire`), the film page's 3.7 → 1.7 MB. A test that only
 * checks the bytes served passes all of them. These measure the production actions over the checked-in
 * read-model snapshot, on the thread that serves them, three ways:
 *
 *  - COLD: a controller built for the run, every cache of its own empty — a city's first render after a
 *    boot, or after its stamp moved everything.
 *  - WARM: the controller's caches (schedules, film cards, film JSON, JSON-LD) full, the response blob
 *    missed — `?diag=` in production, and every render after a city's stamp moved one film.
 *  - BLOB HIT: the gzipped body served as kept.
 *
 * A render must complete on the calling thread with a strict body for its allocation to be all counted
 * here; the first case holds every measured action to that.
 */
class RenderBudgetSpec extends AnyFlatSpec with Matchers {

  // `FixtureServerMain`'s instant: midnight of the corpus's day, so no showing has started.
  private val now   = LocalDateTime.of(2026, 6, 8, 0, 0)
  private val clock = Clock.fixed(now.atZone(Country.default.zone).toInstant, ZoneOffset.UTC)

  private lazy val wiring = {
    val w = new FixtureTestWiring("08-06-2026")
    w.bootFromSnapshotOrPipeline()
    w
  }

  /** A controller wired as production wires it, over the snapshot — in `Mode.Prod`, whose memoising minifier
   *  renders the shared stylesheets once rather than per page (a test-mode controller passes them through,
   *  and its film page cost 3.6 MB against production's 1.7); `blobs`: the response cache, by
   *  default one too small to hold any body, so every request renders. */
  private def controller(blobs: EncodedResponseCache = TestResponseCache(maxBytes = 1)): MovieController =
    TestMovieController.build(Nil, mode = play.api.Mode.Prod, readModel = Some(wiring.webReadModel), clock = clock, responseCache = blobs,
      filmCards = new CaffeineFilmCardFragments(CaffeineFilmCardFragments.DefaultMaxBytes))._1

  private val city: City = models.Poznan
  private lazy val schedules = new MovieControllerService(wiring.webReadModel, clock).toSchedules(city)
  /** The film with the most showtimes — the heaviest film page. */
  private lazy val film = schedules.maxBy(s => (s.showings.map(_._2.map(_.showtimes.size).sum).sum, s.movie.title))
  /** The country the most films list — the heaviest facet page. */
  private lazy val country = schedules.flatMap(_.movie.countries).groupBy(identity).toSeq.map { case (c, n) => (n.size, c) }.max._2

  /** What each budgeted request asks of a controller. */
  private def listing(c: MovieController)  = served(c.index(city.slug), s"/${city.slug}/")
  private def filmPage(c: MovieController) = served(c.filmBySlug(city.slug, film.slug.get), s"/${city.slug}/movie/${film.slug.get}")
  private def browse(c: MovieController)   = served(c.browse(city.slug, Some(country), None, None, None), s"/${city.slug}/movies?country=$country")
  private def repertoire(c: MovieController) = served(c.apiRepertoire(city.slug), s"/${city.slug}/api/repertoire")
  private def details(c: MovieController)  = served(c.apiDetails(city.slug), s"/${city.slug}/api/details")

  /** The body `action` serves a gzip-accepting client at `uri` — on this thread, or the spec fails. */
  private def served(action: Action[AnyContent], uri: String): ByteString = {
    val result = action(FakeRequest("GET", uri).withHeaders("Accept-Encoding" -> "gzip", "Host" -> "kinowo.pl"))
    withClue(s"$uri must be answered on the calling thread: ")(result.isCompleted shouldBe true)
    val answer = result.value.get.get
    withClue(s"$uri: ")(answer.header.status shouldBe 200)
    answer.body match {
      case HttpEntity.Strict(bytes, _) => bytes
      case other                       => fail(s"$uri: a ${other.getClass.getSimpleName} body is produced off this thread")
    }
  }

  private def holds(budget: PerformanceBudget, actual: Long) = {
    info(budget.render(actual))
    budget.check(actual)
  }

  "every budgeted render" should "be answered on the calling thread with a strict body" in {
    val c = controller()
    Seq(listing(c), filmPage(c), browse(c), repertoire(c), details(c)).foreach(_.size should be > 1000)
  }

  "a city listing" should "stay within its cold, warm and blob-hit allocation budgets" in {
    holds(PerformanceBudgets.ListingCold, AllocationMeter.medianFrom(warmups = 2, runs = 5)(controller())(listing))
    val warm = controller()
    holds(PerformanceBudgets.ListingWarm, AllocationMeter.median()(listing(warm)))
    val kept = controller(TestResponseCache())
    holds(PerformanceBudgets.ListingBlobHit, AllocationMeter.median()(listing(kept)))
  }

  "a film page" should "stay within its cold and warm allocation budgets" in {
    holds(PerformanceBudgets.FilmPageCold, AllocationMeter.medianFrom(warmups = 2, runs = 5)(controller())(filmPage))
    val warm = controller()
    holds(PerformanceBudgets.FilmPageWarm, AllocationMeter.median()(filmPage(warm)))
  }

  "a facet page" should "stay within its warm allocation budget" in {
    val warm = controller()
    holds(PerformanceBudgets.BrowseWarm, AllocationMeter.median()(browse(warm)))
  }

  "the apps' JSON payloads" should "stay within their cold and warm allocation budgets" in {
    holds(PerformanceBudgets.ApiRepertoireCold, AllocationMeter.medianFrom(warmups = 2, runs = 5)(controller())(repertoire))
    holds(PerformanceBudgets.ApiDetailsCold, AllocationMeter.medianFrom(warmups = 2, runs = 5)(controller())(details))
    val warm = controller()
    holds(PerformanceBudgets.ApiRepertoireWarm, AllocationMeter.median()(repertoire(warm)))
    holds(PerformanceBudgets.ApiDetailsWarm, AllocationMeter.median()(details(warm)))
  }

  "every fixture city's schedules" should "be reused, not rebuilt, by a second render over an unchanged read model" in {
    val service = new MovieControllerService(wiring.webReadModel, clock)
    val cities  = City.all.filter(c => c.country == Country.default)
    val first   = cities.map(c => c -> service.toSchedules(c, now)).filter(_._2.nonEmpty)
    first.map(_._2.size).sum should be > 100
    val rebuilt = first.map { case (c, before) => ScheduleRebuilds.between(before, service.toSchedules(c, now)) }
    holds(PerformanceBudgets.ScheduleRebuildsOnRepeatRender, rebuilt.sum)
  }
}
