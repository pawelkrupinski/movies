package services.readmodel

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{InMemoryMovieRepository, StoredMovieRecord}

import java.time.LocalDateTime

/** THE RULE (Paweł, 2026-09-07): the prune is a no-op. Staging, enrichment and every
 *  update path must leave the read model in exactly the state the settle would produce,
 *  so the scheduled prune finds nothing — a pruned card is a projection defect, never
 *  accepted churn. This spec drives the real projector through the change stream over
 *  every shape that used to leave a card behind, and after each step asserts that the
 *  read model already equals the reconcile's answer and the prune removes nothing.
 *  Extend it with the shape of any prune that shows up in production. */
class ReadModelConvergenceSpec extends AnyFlatSpec with Matchers {
  private def at(d: String) = Showtime(LocalDateTime.parse(d), bookingUrl = None)

  private class RecordingMetrics extends ReadModelProjectionMetrics {
    val pruned  = scala.collection.mutable.Buffer.empty[String]
    val retired = scala.collection.mutable.Buffer.empty[String]
    def recordWrite(target: String, op: String, count: Int): Unit = ()
    def recordFilmPruned(reason: String, count: Int): Unit        = (1 to count).foreach(_ => pruned += reason)
    def recordCardRetired(reason: String): Unit                   = retired += reason
    def recordProject(wallSeconds: Double, cpuSeconds: Double): Unit = ()
    def recordMetadataProjection(reused: Boolean): Unit           = ()
    def recordReconcileSweep(kind: String, didWork: Boolean): Unit = ()
    def recordCatchUp(rows: Int): Unit                              = ()
    def recordDriftWrites(documents: Int): Unit                     = ()
    def recordCardWrite(changed: Set[String]): Unit                 = ()
  }

  private class Stage {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm         = new InMemoryReadModelRepository()
    val metrics    = new RecordingMetrics()
    val projector  = new ReadModelProjector(repository, rm, rm, metrics)
    projector.start()

    def row(title: String): StoredMovieRecord = repository.findAll().find(_.title == title).get

    /** What the settle would produce: every card and screening of every ready row. */
    def expected(): (Set[String], Set[String]) = {
      val rows = repository.findAll().filter(_.record.readyToProject)
      (rows.flatMap(ReadModelProjection.filmIds(_, titleNormalizer)).toSet,
       rows.flatMap(ReadModelProjection.screeningsAll(_, titleNormalizer).flatten.map(_._id)).toSet)
    }

    /** The invariant, asked after every step. */
    def convergedAfter(step: String): Unit = {
      val (cards, screenings) = expected()
      withClue(s"after '$step' the read model must already be the settle's answer: ") {
        rm.findAllMovieIds().toSet shouldBe cards
        rm.findAllScreenings().map(_._id).toSet shouldBe screenings
      }
      val prunedBefore = metrics.pruned.size
      projector.pruneOrphans()
      withClue(s"after '$step' the prune must find nothing: ") {
        metrics.pruned.drop(prunedBefore) shouldBe empty
        rm.findAllMovieIds().toSet shouldBe cards
        rm.findAllScreenings().map(_._id).toSet shouldBe screenings
      }
    }
    def stop(): Unit = projector.stop()
  }

  private def plain(showtime: String, rating: Option[Double] = Some(7.0)) =
    Multikino -> SourceData(title = Some("Foo"), showtimes = Seq(at(showtime)))
  private def decorated(showtime: String) =
    KinoMuranow -> SourceData(title = Some("Kino Kobiet: Foo"), showtimes = Seq(at(showtime)))
  private def film(slots: (Source, SourceData)*): MovieRecord =
    MovieRecord(tmdbId = Some(1), imdbRating = Some(7.0), data = Map[Source, SourceData](slots*))

  "the read model" should "already hold the settle's answer after every incremental step, so the prune is a no-op" in {
    val stage = new Stage
    import stage._

    repository.upsert("Foo", Some(2024), film(plain("2026-06-12T20:00"), decorated("2026-06-13T20:00")))
    convergedAfter("a film lands under a plain and a decorated listing")

    repository.upsert("Foo", Some(2024), film(plain("2026-06-12T20:00")))
    convergedAfter("the decorated listing vanishes on the next scrape")
    metrics.retired should contain ("variant-gone")

    repository.upsert("Foo", Some(2024), film(plain("2026-06-12T20:00"), decorated("2026-06-20T20:00")))
    convergedAfter("the decorated listing comes back")

    repository.upsert("Bar", Some(2024), film(KinoPalacowe -> SourceData(title = Some("Bar"), showtimes = Seq(at("2026-06-14T20:00")))))
    val bar = row("Bar").id
    convergedAfter("a second film lands")

    // A merge: Bar turns out to be Foo — its side rows move and its document goes.
    repository.moveFilm(bar, row("Foo").id) shouldBe true
    repository.delete(bar)
    convergedAfter("a film is merged away")
    metrics.retired should contain ("row-deleted")

    repository.upsert("Foo", Some(2024), film(plain("2026-06-12T20:00")).copy(tmdbId = None))
    convergedAfter("a film loses its readiness")
    metrics.retired should contain ("row-unready")

    repository.upsert("Foo", Some(2024), film(plain("2026-06-12T20:00")))
    convergedAfter("the film becomes ready again")

    repository.delete(row("Foo").id)
    convergedAfter("a film is deleted outright")

    metrics.pruned shouldBe empty
    stop()
  }

  it should "stay the settle's answer under a shuffled sequence of the same steps" in {
    val stage = new Stage
    import stage._
    val random = new scala.util.Random(20260907L)
    val titles = Seq("Foo", "Bar", "Baz")
    (1 to 60).foreach { step =>
      val title = titles(random.nextInt(titles.size))
      random.nextInt(6) match {
        case 0 | 1 => repository.upsert(title, Some(2024), film(plain(s"2026-06-${10 + random.nextInt(19)}T20:00")))
        case 2     => repository.upsert(title, Some(2024), film(plain("2026-06-12T20:00"), decorated(s"2026-06-${10 + random.nextInt(19)}T20:00")))
        case 3     => repository.upsert(title, Some(2024), film(plain("2026-06-12T20:00")).copy(tmdbId = None))
        case 4     => repository.findAll().find(_.title == title).foreach(r => repository.delete(r.id))
        case 5     =>
          val rows = repository.findAll()
          if (rows.sizeIs >= 2) { val Seq(a, b) = random.shuffle(rows).take(2); repository.moveFilm(a.id, b.id); repository.delete(a.id) }
      }
      convergedAfter(s"random step $step")
    }
    metrics.pruned shouldBe empty
    stop()
  }
}
