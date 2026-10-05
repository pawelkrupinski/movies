package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

/** A decision reads back from its stored family as it was decided — its fallback film and unanswered questions too: a
 *  restart's model projects the stored decisions, and a fallback lost on the way would drop every IMDb id it gave. */
class ResolverDecisionBsonSpec extends AnyFlatSpec with Matchers {

  private val snowLeopard: ListingKey = ListingKey.Published("Kinozentrum Frauentor", "Snow Leopard", Some(2020), Nil)

  "a stored decision" should "read back with its fallback film, best candidate and unanswered questions, and without them when it had none" in {
    val fallen = ResolverDecision(Seq(snowLeopard), None, 0.8, ResolverDecision.Basis.BelowThreshold, Seq("falls back to imdb tt13920372"),
      fallback = Some(ResolverDecision.Fallback("imdb", "tt13920372", 0.31)), leaning = Some(ResolverDecision.Leaning(996584, 21223152)), unanswered = 2,
      candidate = Some(ResolverDecision.Leaning(996584, 21223152)))()
    ResolverDecisionBson.decode(ResolverDecisionBson.encode(fallen)) shouldBe fallen
    val matched = ResolverDecision(Seq(snowLeopard), Some(996584), 0.9, ResolverDecision.Basis.OwnMatch, Nil)()
    ResolverDecisionBson.decode(ResolverDecisionBson.encode(matched)) shouldBe matched
  }

  it should "read back with each agreeing family's own id of its film" in {
    val agreed = ResolverDecision(Seq(snowLeopard), Some(996584), 1.0, ResolverDecision.Basis.Agreed, Seq("imdb, metacritic, rt agree on 'Snow Leopard'"),
      agreed = Map("imdb" -> "tt13920372", "metacritic" -> "snow-leopard", "rt" -> "snow_leopard_2023"))()
    ResolverDecisionBson.decode(ResolverDecisionBson.encode(agreed)) shouldBe agreed
  }
}
