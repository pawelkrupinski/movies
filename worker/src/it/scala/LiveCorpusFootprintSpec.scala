package services.identity

import integration.LiveHeap
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

  /** Each listing's own title, whose search names one film of its own. */
  private object Lookups extends IdentityLookups {
    def hasDetail(listing: Listing) = false
    def detail(listing: Listing)    = Answer.Known(None)
    def candidates(query: CandidateQuery): Answer[Seq[Hit]] = query match {
      case CandidateQuery.Title(text) => Answer.Known(idOf(text).toSeq.map(id => Hit(id, text, None, Some(2020), 5.0)))
      case _                          => Answer.Known(Nil)
    }
    def film(id: Int) = Answer.Known(Some(IdentityMeasures.Film(s"Film ${id - FirstId}", year = Some(2020))))
    private def idOf(text: String) = "(\\d+)".r.findFirstIn(text).map(_.toInt + FirstId)
  }

  "the live corpus" should "not box a TMDB id per film-keyed map" in {
    val listings = (0 until Films).map(n => Listing.of(Helios, CinemaMovie(Movie(s"Film $n", releaseYear = Some(2020)), Helios, None,
      Some(s"https://helios.pl/film/$n"), None, Nil, Nil, Seq(show)), normalizer))
    val before = LiveHeap.classes()
    val live   = new LiveCorpus(Lookups, normalizer, PinConstraints(Nil), TitleDecorations.None)
    live.seen(listings)
    val grown  = LiveHeap.grown(before, LiveHeap.classes()).map(c => c.name -> c).toMap
    live.candidateCount shouldBe Films
    val boxed = grown.get("java.lang.Integer").fold(0L)(_.instances)
    // Ten per film before (six film-keyed maps, the nodes' reached films, the queries' named films,
    // the small per-key Sets); four now — only the per-key Sets, which need set semantics, box still.
    withClue(s"boxed Integers kept for $Films films (${LiveHeap.render(grown.values.toSeq.sortBy(-_.bytes))}): ")(boxed should be < 5L * Films)
  }
}
