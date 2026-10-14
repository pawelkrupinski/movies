package services.identity

import models.{CinemaShowing, Helios, MovieRecord, Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{FilmId, SingleCountryNormalizer, StoredMovieRecord}

import java.time.LocalDateTime

/** The projection's shrink guard: a draft that would take more than its share of cards or upcoming showtimes off the
 *  site is refused — the shares always of the whole corpus, whether the projection drafted all of it or a scope. */
class ProjectionGuardSpec extends AnyFlatSpec with Matchers {
  private val now = LocalDateTime.of(2026, 10, 14, 12, 0)

  /** A film at one venue with `upcoming` showtimes still to come and `past` already shown. */
  private def record(title: String, upcoming: Int, past: Int = 0): MovieRecord = {
    val times = (1 to upcoming).map(h => now.plusHours(h)) ++ (1 to past).map(h => now.minusHours(h))
    MovieRecord(data = Map(CinemaShowing.keyFor(Helios, title, SingleCountryNormalizer.titleNormalizer) ->
      SourceData(title = Some(title), showtimes = times.map(Showtime(_, None)))))
  }
  private def stored(i: Int, upcoming: Int = 10, past: Int = 0) =
    StoredMovieRecord(s"Film $i", Some(2026), record(s"Film $i", upcoming, past), FilmId(s"film-$i|2026"))
  private def draftOf(kept: Seq[StoredMovieRecord], vanished: Seq[StoredMovieRecord] = Nil, upcoming: Int = 10) =
    ProjectionDraft(kept.zipWithIndex.map { case (s, i) => FilmDraft(i + 1L, Some(s.id), Nil, record(s.title, upcoming), s.title) },
      vanished.map(_.id), vanished.map(_.id), FilmIdCounters.empty, Nil, Regroupings(0, 0, 0, 0, 0))

  private val corpus = (1 to 100).map(stored(_))

  "the shrink guard" should "pass a draft that keeps every film and showtime" in {
    ProjectionGuard.refusal(draftOf(corpus), corpus, now, corpus = corpus) shouldBe None
  }

  it should "refuse a draft whose vanished cards pass their share of the corpus, naming them" in {
    val (kept, gone) = corpus.splitAt(97)
    ProjectionGuard.refusal(draftOf(kept, gone), corpus, now, corpus = corpus).get should startWith("3 of 100 films would vanish")
  }

  it should "not count a film whose screenings are all past as a card lost" in {
    val finished = (101 to 110).map(stored(_, upcoming = 0, past = 3))
    ProjectionGuard.refusal(draftOf(corpus, finished), corpus ++ finished, now, corpus = corpus ++ finished) shouldBe None
  }

  it should "refuse a draft losing more than its share of upcoming showtimes" in {
    ProjectionGuard.refusal(draftOf(corpus, upcoming = 9), corpus, now, corpus = corpus).get shouldBe
      "upcoming showtimes would fall from 1000 to 900 (more than 0.5%)"
  }

  it should "weigh a scoped draft's losses against the whole corpus, not its scope" in {
    val wide  = (1 to 2000).map(stored(_))
    val scope = wide.take(10)
    // The scope loses 40 of its 100 showtimes: 2% of the 20,000 the corpus holds, but 40% of the scope's own.
    val draft = draftOf(scope, upcoming = 6)
    ProjectionGuard.refusal(draft, scope, now, corpus = wide) shouldBe None
    ProjectionGuard.refusal(draft, scope, now, corpus = scope) shouldBe defined
  }

  it should "pass anything over an empty corpus" in {
    ProjectionGuard.refusal(draftOf(Nil), Nil, now, corpus = Nil) shouldBe None
  }
}
