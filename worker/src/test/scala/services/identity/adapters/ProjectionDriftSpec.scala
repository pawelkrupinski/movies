package services.identity
package adapters

import models.{FilmId, ListingKey, MovieRecord, StoredMovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identityadapters.{IdentityProjectionMetrics, ProjectionDrift, ProjectionTick}

/** A reconciling projection's drift: the films it wrote or retired outside the scope its changes gave it, counted and
 *  named — a film new to the store, one whose fields moved, one that moved its key alone, one retired. */
class ProjectionDriftSpec extends AnyFlatSpec with Matchers {
  private val normalizer = ProjectionWorld.normalizer
  private val loggerName = "test.projection-drift"

  private def film(id: String, title: String, record: MovieRecord, members: Seq[ListingKey] = Nil) =
    ProjectedFilm(FilmId(id), 1L, title, Some(2020), s"${title.toLowerCase}|2020", record, members)

  private final class Counted extends IdentityProjectionMetrics {
    val drifts = scala.collection.mutable.ListBuffer.empty[Int]
    def projected(films: Int, listings: Int, regroupings: Regroupings, seconds: Double): Unit = ()
    def refused(reason: IdentityProjectionMetrics.Refusal): Unit = ()
    def drifted(films: Int): Unit = { drifts += films; () }
    def reconciled(measured: Boolean): Unit = ()
    def takesChanged(changes: Seq[TakeChange]): Unit = ()
    def agreementPhase(mode: String, phase: services.identityadapters.ProjectionPhase): Unit = ()
  }

  "a reconcile's drift" should "count and name each film written or retired outside its scope, and none inside it" in {
    val inScope  = ListingKey.Published("Kino Muza", "Inside", None, Nil)
    val stored   = Seq(
      StoredMovieRecord("Moved", Some(2020), MovieRecord(imdbId = Some("tt1")), FilmId("f-moved")),
      StoredMovieRecord("Old Key", Some(2019), MovieRecord(), FilmId("f-key")),
      StoredMovieRecord("Scoped", Some(2020), MovieRecord(), FilmId("f-scoped")))
    val changed  = Seq(film("f-new", "New", MovieRecord()), film("f-moved", "Moved", MovieRecord(imdbId = Some("tt2"))),
      film("f-key", "New Key", MovieRecord()), film("f-scoped", "Scoped", MovieRecord(imdbId = Some("tt3"))),
      film("f-listed", "Listed", MovieRecord(imdbId = Some("tt4")), members = Seq(inScope)),
      // stored under the plain key a film inside the scope is drafted under: the closed scope takes it in
      film("f-collide", "Scoped Again", MovieRecord(imdbId = Some("tt5"))))
    val collider = StoredMovieRecord("Scoped", Some(2020), MovieRecord(), FilmId("f-collide"))
    val plan     = ProjectionPlan(changed, Seq(FilmId("f-gone"), FilmId("f-scoped-gone")), Nil, Regroupings(0, 0, 0, 0, 0))
    val tick     = ProjectionTick(None, Some(plan), 0, 0, 0, 0, None, changed = changed)
    val gone     = StoredMovieRecord("Scoped Gone", Some(2020), MovieRecord(), FilmId("f-scoped-gone"))
    // only stored films are films of a scope: the scope's own, and those its keys reach
    val index    = ProjectionIndex(Map.empty, (stored ++ Seq(collider, gone)).map(r => r.id.value -> r).toMap, Map.empty, Map.empty,
      FilmIdCounters.empty, Nil, Map.empty, Map.empty)(normalizer)
    val scope    = ProjectionScope(whole = false, Set("f-scoped", "f-scoped-gone"), Set.empty, Set(inScope))
    val metrics  = new Counted
    val drift    = new ProjectionDrift(normalizer, metrics, play.api.Logger(loggerName))
    val logged   = tools.LogCapture.capture(loggerName)(drift(scope, Set.empty, index, tick, stored ++ Seq(collider, gone)))
    metrics.drifts.toSeq shouldBe Seq(4)
    logged.map(_.getFormattedMessage) shouldBe Seq("identity projection drift: the whole projection changed 4 film(s) the " +
      "scoped one would have left: f-gone (retired), f-key (key oldkey|2019 -> new key|2020, titled 'Old Key' -> 'New Key'), " +
      "f-moved (imdbId), f-new (new)")
  }

  it should "count none and log nothing when every change was inside its scope" in {
    val changed = Seq(film("f-scoped", "Scoped", MovieRecord()))
    val plan    = ProjectionPlan(changed, Nil, Nil, Regroupings(0, 0, 0, 0, 0))
    val index   = ProjectionIndex(Map.empty, Map.empty, Map.empty, Map.empty, FilmIdCounters.empty, Nil, Map.empty, Map.empty)(normalizer)
    val metrics = new Counted
    val logged  = tools.LogCapture.capture(loggerName)(new ProjectionDrift(normalizer, metrics, play.api.Logger(loggerName))(
      ProjectionScope(whole = false, Set("f-scoped"), Set.empty, Set.empty), Set.empty, index,
      ProjectionTick(None, Some(plan), 0, 0, 0, 0, None, changed = changed), Nil))
    metrics.drifts.toSeq shouldBe Seq(0)
    logged shouldBe empty
  }
}
