package services.identity

import models.{CinemaMovie, KinoMuza, Movie}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer}

import java.time.{Clock, Instant, ZoneOffset}
import scala.collection.mutable

/** The model's proposals for unresolved listings: filed by title, read by the model (which re-resolves the listings of
 *  a title whose proposal changed), and asked for only about unresolved titles that hold none yet, within a budget. */
class ProposalsSpec extends AnyFlatSpec with Matchers {
  private val clock = Clock.fixed(Instant.parse("2026-10-03T12:00:00Z"), ZoneOffset.UTC)
  private def listing(title: String) =
    Listing.of(KinoMuza, CinemaMovie(Movie(title), KinoMuza, None, None, None, Nil, Nil, Nil), SingleCountryNormalizer.titleNormalizer)

  "the proposal index" should "serve a title's proposal to every venue's listing of it, and tell the model when one is filed" in {
    val changed = mutable.ArrayBuffer.empty[String]
    val index = new ProposalIndex(new InMemoryProposalStore, changed += _)
    val swede = Proposal("film", Some("Confessions of a Swedish Man"), Some(2025))
    index.put(StoredProposal(ProposalIndex.keyOf("Wyznania szwedzkiego mężczyzny"), "Wyznania szwedzkiego mężczyzny", swede, "m", clock.instant()))
    changed.toSeq shouldBe Seq("proposal:wyznaniaszwedzkiegomezczyzny")
    index.proposal(listing("WYZNANIA SZWEDZKIEGO MĘŻCZYZNY"), ObservationReads.Untracked) shouldBe Some(swede)
    index.proposal(listing("Lalka"), ObservationReads.Untracked) shouldBe None
  }

  "the proposal fill" should "ask about the unresolved titles holding no proposal, once per title, within its budget" in {
    def trace(venue: String, title: String, blocker: Option[String]) =
      ListingTrace(ListingKey.Published(venue, title, Some(2025), Seq("Jonas Selberg Augustsén")), "f", None, "BelowThreshold", Nil, None, blocker = blocker)
    val traces = new InMemoryIdentityTraceStore
    traces.replace(Set.empty, () => Seq(
      trace("Kino Muza", "Wyznania szwedzkiego mężczyzny", Some("search:found-nothing")),
      trace("Kino Kosmos", "WYZNANIA SZWEDZKIEGO MĘŻCZYZNY", Some("search:found-nothing")),  // one title: asked once
      trace("Kino Muza", "Dyrygent", Some("rule:below-the-rating-cut")),
      trace("Kino Muza", "Warsztaty ceramiczne", Some("not-a-film:event")),                  // judged already
      trace("Kino Muza", "Lalka", None),                                                     // resolved
      trace("Kino Muza", "Pianista - Kino Konesera", Some("search:found-nothing"))))
    val index = new ProposalIndex(new InMemoryProposalStore)
    index.put(StoredProposal(ProposalIndex.keyOf("Pianista - Kino Konesera"), "Pianista - Kino Konesera", Proposal("film", Some("The Pianist"), Some(2002)), "m", clock.instant()))
    val asked = mutable.ArrayBuffer.empty[ProposalAsk]
    val proposer = new Proposer {
      val model = "test"
      def propose(asks: Seq[ProposalAsk]) = { asked ++= asks; asks.map(a => a.key -> Proposal("unclear")).toMap }
    }
    new ProposalFill(traces, index, proposer, clock, budget = 10, batch = 1).round() shouldBe 2
    asked.map(_.title).toSet shouldBe Set("Wyznania szwedzkiego mężczyzny", "Dyrygent")
    asked.find(_.title == "Dyrygent").map(a => (a.year, a.directors)) shouldBe Some((Some(2025), Seq("Jonas Selberg Augustsén")))
    index.has(ProposalIndex.keyOf("Dyrygent")) shouldBe true
    // a second round asks nothing: every unresolved title holds a proposal now
    asked.clear()
    new ProposalFill(traces, index, proposer, clock).round() shouldBe 0
    asked shouldBe empty
  }

  "an Anthropic answer" should "propose a film only when the model is sure of it" in {
    // The Messages API's response shape; not yet a recorded response — record one (and replace this) once
    // ANTHROPIC_API_KEY is configured.
    val asks = Seq(ProposalAsk("a", "Dyrygent", "Kino Muza", None, Nil), ProposalAsk("b", "Dobry chłopiec", "Kino Muza", None, Nil),
      ProposalAsk("c", "Warsztaty ceramiczne", "Kino Muza", None, Nil))
    val text = """[{"i":1,"category":"film","original_title":"Dyrygent","year":1980,"directors":["Andrzej Wajda"],"confidence":0.9},""" +
      """{"i":2,"category":"film","original_title":"Good Boy","year":2026,"directors":[],"confidence":0.5},""" +
      """{"i":3,"category":"event","original_title":null,"year":null,"directors":[],"confidence":0.95}]"""
    val response = play.api.libs.json.Json.obj("content" -> play.api.libs.json.Json.arr(play.api.libs.json.Json.obj("type" -> "text", "text" -> text))).toString
    AnthropicProposer.parse(response, asks) shouldBe Map(
      "a" -> Proposal("film", Some("Dyrygent"), Some(1980), Seq("Andrzej Wajda")),
      "b" -> Proposal("unclear"),
      "c" -> Proposal("event"))
  }
}
