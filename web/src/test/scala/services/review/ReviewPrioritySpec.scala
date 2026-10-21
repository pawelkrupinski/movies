package services.review

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.DecisionTrace
import services.identity.ResolverDecision
import services.identity.ResolverDecision.Basis
import services.movies.ListingKey

class ReviewPrioritySpec extends AnyFlatSpec with Matchers {
  import ReviewFixtures._

  private def cluster(d: ResolverDecision) = ReviewCluster.of(Country.Poland, d)
  private def queue(ds: Seq[ResolverDecision], served: Map[ListingKey, Option[Int]] = Map.empty, labels: Seq[LabelRow] = Nil) =
    ReviewSelection.queue(ds.map(cluster), served.map { case (k, f) => ListingKey.serialised(k) -> f }, new JudgedListings(labels))
  private def at(n: Int, raw: String) = (1 to n).map(i => ListingKey.Published(s"Kino $i", raw, None, Nil))

  /** "Kandydaci śmierci" at six cinemas, its best candidate held back at 52.3%. */
  private val crowded = ResolverDecision(at(6, "Kandydaci śmierci"), None, 0.48, Basis.BelowThreshold,
    Seq("best rejected candidate 1703629 at 52.3% (title overlap)"))()

  "the queue" should "list first the cluster an answer settles the most listings of, weighted by how likely the resolver is wrong" in {
    val ranked = queue(Seq(heldDecision, vetoedDecision, nothingDecision, crowded))
    ranked.map(_._1.title) shouldBe Seq("Kandydaci śmierci", "FRANZ KAFKA", "Macbeth", "Filmowe popołudnie dla dzieci: Robaczki")
    ranked.map { case (_, p) => (p.listings, p.uncertainty) } shouldBe Seq((6, 0.523), (1, 0.864), (1, 0.625), (1, 0.0))
    ranked.head._2.score shouldBe 6 * 0.523 +- 1e-9
  }

  it should "count only the listings no label settles yet: one judged right for a film, or wrong for the film the card shows" in {
    val labels = Seq(
      LabelRow("pl", "Kino 1", "Kandydaci  śmierci", "tmdb:1", "right", "review page: right"),   // its film known
      LabelRow("pl", "Kino 2", "Kandydaci śmierci", "tmdb:1703629", "wrong", "review page: wrong"), // the shown film ruled out
      LabelRow("pl", "Kino 3", "Kandydaci śmierci", "tmdb:99", "wrong", "review page: wrong"),      // another film: still open
      LabelRow("de", "*", "Kandydaci śmierci", "tmdb:1", "right", "review page: right"))           // another country's
    val (_, p) = queue(Seq(crowded), labels = labels).head
    (p.listings, p.judged) shouldBe ((4, 2))
    p.reasons should contain("2 of 6 listings already labelled")
    // every venue billing the title, by a `*` row: nothing left to settle
    queue(Seq(crowded), labels = Seq(LabelRow("pl", "*", "Kandydaci śmierci", "tmdb:1", "right", "x"))).head._2.score shouldBe 0.0
  }

  it should "list a pooled take, unsure by 1 − its confidence, but not a member's own confident take" in {
    val pooled = ResolverDecision(at(3, "Dolly"), Some(500), 0.7, Basis.PooledMatch, Nil)()
    val ranked = queue(Seq(pooled, matchedDecision))
    ranked.map(_._1.title) shouldBe Seq("Dolly")
    ranked.head._2.uncertainty shouldBe 0.3 +- 1e-9
    ranked.head._2.reasons shouldBe Seq("PooledMatch take at 70%")
  }

  it should "list a take prod serves otherwise, at least half unsure, the served film a candidate to pick" in {
    val sure = matchedDecision.copy(confidence = 0.99)(DecisionTrace.Empty)
    val ranked = queue(Seq(sure), served = Map(Matched -> Some(77)))
    ranked.map(_._1.title) shouldBe Seq("Klondike")
    val (c, p) = ranked.head
    p.uncertainty shouldBe 0.5
    p.served shouldBe Seq(Some(77))
    p.reasons should contain("served as tmdb:77, the model decided tmdb:913760")
    c.candidates.map(_.film) shouldBe Seq(77)
    // served as it was decided: no disagreement, and an own take stays off the queue
    queue(Seq(sure), served = Map(Matched -> Some(913760))) shouldBe empty
  }

  it should "read a no-match prod serves a film as the stages disagreeing, and put that film forward when the model weighed none" in {
    val ranked = queue(Seq(nothingDecision), served = Map(Nothing -> Some(42)))
    val (c, p) = ranked.head
    p.uncertainty shouldBe 0.5
    c.shown shouldBe Some(42)
    // a listing served with no film, as the model decided: they agree
    queue(Seq(nothingDecision), served = Map(Nothing -> None)).head._2.stagesDisagree shouldBe false
  }
}
