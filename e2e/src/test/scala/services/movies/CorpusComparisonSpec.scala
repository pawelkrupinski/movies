package services.movies

import tools.SpecPool
import tools.SpecTimeouts

import controllers.FilmSchedule
import models.{Movie, MovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.TestReadModel

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * The two properties the order-independence comparison is load-bearing for.
 *
 * The first is a HEAP property, and it has no other test layer: the leg that
 * exercises it takes five hours on a CI runner and reports nothing at all when it
 * runs out of memory (the JVM exits on `-XX:+ExitOnOutOfMemoryError` before
 * ScalaTest writes a report, which is exactly how the 2026-09-05 US failure
 * presented). So the invariant is asserted where it can be: at most one pass
 * materialises its corpus at a time, which is what bounds the resident set to the
 * baseline plus one.
 *
 * The second is that bounding the heap did not cost the diagnostics — the reason
 * to serialise the tail rather than reduce later passes to digests.
 */
class CorpusComparisonSpec extends AnyFlatSpec with Matchers {

  private def row(title: String): FilmSchedule =
    FilmSchedule(Movie(title), posterUrl = None, synopsis = None, cast = Nil, director = Nil,
      cinemaFilmUrls = Nil, showings = Nil,
      resolved = TestReadModel.resolved(title, None, MovieRecord()), slug = Some(title),
      asOf = java.time.LocalDate.of(2026, 6, 8))

  private def corpus(titles: String*): ReplayCorpus =
    ReplayCorpus(records = Nil, screenings = Map.empty, rows = titles.map(row))

  "the corpus comparison" should "count every pass it folds in, so a pass that never arrived is not read as agreement" in {
    val comparison = new CorpusComparison
    comparison.folded shouldBe 0
    comparison.submit(0)(() => corpus("Ghost in the Shell"))
    comparison.submit(1)(() => corpus("Ghost in the Shell"))
    // two agreeing passes: no divergence, and the count says only two of (say) three were compared
    comparison.divergences shouldBe empty
    comparison.folded shouldBe 2
  }

  it should
    "materialise one pass's corpus at a time, so the heap never holds more than the baseline and one other" in {
    val comparison = new CorpusComparison
    val live = new AtomicInteger()
    val peak = new AtomicInteger()

    SpecPool.usingContext(4) { context =>
      implicit val ec: ExecutionContext = context
      Await.result(Future.sequence((0 until 4).map(pass => Future {
        comparison.submit(pass) { () =>
          peak.updateAndGet(_ max live.incrementAndGet())
          // Wide enough that four passes arriving together would overlap if the
          // materialisation were not the thing being serialised.
          Thread.sleep(100)
          live.decrementAndGet()
          corpus("Ghost in the Shell")
        }
      })), SpecTimeouts.Io)
    }

    peak.get() shouldBe 1
    comparison.divergences shouldBe empty
    comparison.reference.rows.map(_.movie.title) shouldBe Seq("Ghost in the Shell")
  }

  it should "print BOTH sides of a divergence, not merely that one happened" in {
    val comparison = new CorpusComparison
    comparison.submit(0)(() => corpus("Perfect Blue"))
    comparison.submit(2)(() => corpus("Paprika"))

    val report = comparison.divergences.mkString("\n")
    report should include ("RENDERED ROWS differ on pass2")
    report should include ("Perfect Blue")
    report should include ("Paprika")
  }

  it should "say nothing when every pass agrees" in {
    val comparison = new CorpusComparison
    (0 until 3).foreach(pass => comparison.submit(pass)(() => corpus("Akira")))

    comparison.divergences shouldBe empty
  }

  it should "refuse to hand out a reference corpus no pass produced" in {
    a [IllegalStateException] should be thrownBy new CorpusComparison().reference
  }
}
