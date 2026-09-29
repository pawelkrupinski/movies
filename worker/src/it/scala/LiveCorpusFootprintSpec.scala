package services.identity

import integration.Reachable
import models.{CinemaMovie, Helios, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import java.time.LocalDateTime

/** What the live corpus keeps per film. Its film-keyed maps each boxed every TMDB id — above the
 *  JVM's small-integer cache, so one `Integer` per map per film — and spent a hash-map node on each:
 *  0.81 MB of `Integer` per map on worker-us's 40k films (live dump, 2026-09-29). */
class LiveCorpusFootprintSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val Films      = 3000
  private val FirstId    = 500000                        // TMDB-sized ids, never the cached -128..127
  private val show       = Showtime(LocalDateTime.of(2099, 3, 1, 18, 0), None)

  /** Each listing's own title, whose search names one film of its own. No years anywhere: a year
   *  such as 2020 boxes too, and the count below is of what the corpus's structures box for ids. */
  private object Lookups extends IdentityLookups {
    def hasDetail(listing: Listing) = false
    def detail(listing: Listing)    = Answer.Known(None)
    def candidates(query: CandidateQuery): Answer[Seq[Hit]] = query match {
      case CandidateQuery.Title(text) => Answer.Known(idOf(text).toSeq.map(id => Hit(id, text, None, None, 5.0)))
      case _                          => Answer.Known(Nil)
    }
    def film(id: Int) = Answer.Known(Some(IdentityMeasures.Film(s"Film ${id - FirstId}")))
    private def idOf(text: String) = "(\\d+)".r.findFirstIn(text).map(_.toInt + FirstId)
  }

  "the live corpus" should "not box a TMDB id per film-keyed map" in {
    val listings = (0 until Films).map(n => Listing.of(Helios, CinemaMovie(Movie(s"Film $n"), Helios, None,
      Some(s"https://helios.pl/film/$n"), None, Nil, Nil, Seq(show)), normalizer))
    val live   = new LiveCorpus(Lookups, normalizer, PinConstraints(Nil), TitleDecorations.None)
    live.seen(listings)
    live.candidateCount shouldBe Films
    // Counted from the corpus itself, not as a heap-wide delta: `itAll` runs suites in parallel in
    // one JVM, and their Integers landed in a heap-wide count (21,242 for this 12,000 once).
    val kept  = Reachable.count(live, outside = Seq(Lookups, normalizer))
    val boxed = kept.getOrElse("java.lang.Integer", 0L)
    // Ten per film before (six film-keyed maps, the nodes' reached films, the queries' named films,
    // the small per-key Sets); four now — only the per-key Sets, which need set semantics, box still.
    withClue(s"boxed Integers the corpus reaches for $Films films: ")(boxed should be < 5L * Films)
  }
}
