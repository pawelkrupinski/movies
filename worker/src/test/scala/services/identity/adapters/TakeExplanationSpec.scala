package services.identity
package adapters

import models.{Country, KinoMuza, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey
import tools.{HttpStatusException, MutableClock, TestWiring}

import scala.util.{Failure, Success}

/** "Why did this listing change film then?" answered from the history alone ([[TakeExplanation]]): a take moved by the
 *  projection is kept with its line's fields, its decision's members and its listings' reads ([[TakeHistory]]), and the
 *  inputs that moved between it and the take before are named — a record that failed to come, an answer that changed,
 *  a listing that joined the decision. */
class TakeExplanationSpec extends AnyFlatSpec with Matchers {
  private val language = "pl-PL"
  private val titles   = services.movies.SingleCountryNormalizer.titleNormalizer
  private def listing(cinema: models.Cinema, title: String): Listing =
    Listing.all(Seq(cinema -> Seq(models.CinemaMovie(models.Movie(title), cinema, None, Some(s"${cinema.displayName}/$title"), None, Nil, Nil,
      Seq(models.Showtime(java.time.LocalDateTime.of(2026, 10, 1, 18, 0), None))))), titles).head

  private final class World {
    val clock   = new MutableClock(TestWiring.FixedInstant)
    val history = new InMemoryIdentityHistory
    val docs    = new HistoryTmdbDocuments(new InMemoryTmdbDocuments, history, clock, "b1")
    val store   = new TmdbStore(docs, clock, history = history, build = "b1")
    val normalizer = new TmdbNormalizer(store, failures = new KeptFailures(history, clock, "b1"))
    val lines   = new TakeChangeLines(Country.Poland, settings.CommitSha("b1"), reads = TakeReads.of(language))
    val takes   = new TakeHistory(history, TakeReads.of(language), clock, "b1")
    def later(): Unit = clock.advance(java.time.Duration.ofMinutes(1))
    /** The projection moving `key` from `before` to `after`, its decision holding `members`. */
    def moved(key: ListingKey, before: Int, after: Int, basis: ResolverDecision.Basis, was: ResolverDecision.Basis, members: Seq[Listing]): Unit = {
      val decision = ResolverDecision(members.map(_.key), Some(after), 0.4, basis, Seq(s"pooled evidence of ${members.size} node(s) → $after"))()
      val take     = TakeChange(key, Some(s"film-$before"), Some(before), Some(after), Some(was), Some(basis), cause = Some(TakeChange.Cause.Evidence))
      val line     = lines.of(take, Some("Lalka"), Some(s"film-$after" -> "Lalka"), Some(decision), members.find(_.key == key),
        Some(TakeChange.Prior(was, 0.9, Some("before"))), Some("scrape"))
      takes.kept(Seq((take, line, Some(decision))), k => members.find(_.key == k))
    }
  }

  "a take that flipped" should "be explained by the inputs that moved since the take before: the listing that joined, the read that failed" in {
    val w        = new World
    val multikino = listing(Multikino, "Lalka")
    val muza      = listing(KinoMuza, "Lalka")
    w.moved(multikino.key, 2, 4, ResolverDecision.Basis.PooledMatch, ResolverDecision.Basis.OwnMatch, Seq(multikino))
    w.later()
    // between the two takes: Muza's "Lalka" joins, TMDB's search moves, and Kawalski's record's read is answered 429
    w.history.append(Seq(HistoryEntry(HistoryEntry.Listing, ListingKey.serialised(muza.key), w.clock.millis(), None,
      Some(ListingBson.doc(muza)), "b1")))
    w.normalizer.filed("GET", s"https://api.themoviedb.org/3/search/movie?language=$language&include_adult=false&query=Lalka",
      Success("""{"results":[{"id":2,"title":"Lalka","original_title":"Lalka","release_date":"2025-01-01","popularity":12.4}]}"""))
    w.normalizer.filed("GET", s"https://api.themoviedb.org/3/movie/2?language=$language&append_to_response=credits,release_dates",
      Failure(new HttpStatusException(429, "GET", "https://api.themoviedb.org/3/movie/2", None)))
    w.later()
    w.moved(multikino.key, 4, 2, ResolverDecision.Basis.OwnMatch, ResolverDecision.Basis.PooledMatch, Seq(multikino, muza))

    val explained = TakeExplanation.explain(w.history, ListingKey.serialised(multikino.key), w.clock.millis()).get
    explained.joined shouldBe Seq(ListingKey.serialised(muza.key))
    explained.left shouldBe empty
    explained.inputs.map(e => (e.kind, e.failure)) should contain allOf (
      (HistoryEntry.Listing, None), (HistoryEntry.answer(TmdbKind.Query), None), (HistoryEntry.Failure, Some("http 429")))
    val rendered = explained.render
    info(rendered.mkString("\n"))
    rendered.head should startWith (s"${multikino.venue} | Lalka: tmdb 4 -> tmdb 2")
    rendered.mkString("\n") should (include ("basis PooledMatch -> OwnMatch") and include ("failed: http 429") and include ("joined: "))
  }

  it should "say the code decided otherwise when no input it read moved" in {
    val w = new World
    val multikino = listing(Multikino, "Lalka")
    w.moved(multikino.key, 2, 4, ResolverDecision.Basis.PooledMatch, ResolverDecision.Basis.OwnMatch, Seq(multikino))
    w.later()
    w.moved(multikino.key, 4, 2, ResolverDecision.Basis.OwnMatch, ResolverDecision.Basis.PooledMatch, Seq(multikino))
    TakeExplanation.explain(w.history, ListingKey.serialised(multikino.key), w.clock.millis()).get.render.last should include ("the code decided otherwise")
  }

  "a listing the history holds no take of" should "be explained by nothing" in {
    TakeExplanation.explain(new InMemoryIdentityHistory, "nowhere|nothing", 0L) shouldBe None
  }

  "a take's line" should "name what it read and how many listings its decision holds" in {
    val w = new World
    val l = listing(Multikino, "Lalka")
    val fields = w.lines.of(TakeChange(l.key, Some("f"), Some(1), Some(2)), None, None,
      Some(ResolverDecision(Seq(l.key), Some(2), 0.9, ResolverDecision.Basis.OwnMatch, Seq("own"))()), Some(l), None).fields.toMap
    fields("members") shouldBe Some(1)
    fields("reads").toString should include (s"${HistoryEntry.answer(TmdbKind.Query)} movie|$language|Lalka")
  }
}
