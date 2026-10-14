package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

/** The shadow diff's reference ([[ShadowDiffOracle]]): each resolver cluster's relation to the pipeline's films — what
 *  the projection's canary, read off its index, is held to (`LiveProjectionIndexSpec`). */
class ShadowDiffSpec extends AnyFlatSpec with Matchers {

  private def key(venue: String, title: String): ListingKey = ListingKey.Published(venue, title, None, Nil)
  private val (a1, a2, b1, c1, c2, d1, e1, e2) =
    (key("A", "Belle"), key("B", "Belle"), key("A", "Lalka"), key("A", "It"), key("B", "It"), key("A", "Nowy"),
      key("A", "Opętanie"), key("B", "Opętanie"))

  private def decision(members: Seq[ListingKey], film: Option[Int], confidence: Double = 0.9) =
    ResolverDecision(members, film, confidence, if (film.isDefined) ResolverDecision.Basis.OwnMatch else ResolverDecision.Basis.NoCandidate, Nil)()

  private val belle   = PipelineFilmRef("belle", Some(1))
  private val lalka   = PipelineFilmRef("lalka", Some(2))
  private val it1990  = PipelineFilmRef("it-1990", Some(3))
  private val it2017  = PipelineFilmRef("it-2017", Some(4))
  private val opetanie = PipelineFilmRef("opetanie", Some(5))

  // The resolver: Belle as the pipeline has it; Lalka on another film; It's two pipeline films
  // joined; Opętanie's one film split in two; a listing the pipeline has not placed yet.
  private val decisions = Seq(
    decision(Seq(a1, a2), Some(1)),
    decision(Seq(b1), Some(20), confidence = 0.4),
    decision(Seq(c1, c2), Some(3)),
    decision(Seq(e1), Some(5)),
    decision(Seq(e2), Some(6)),
    decision(Seq(d1), None))
  private val pipelineOf = Map(a1 -> belle, a2 -> belle, b1 -> lalka, c1 -> it1990, c2 -> it2017, e1 -> opetanie, e2 -> opetanie)

  private val clusters = ShadowDiffOracle.relationsOf(decisions, pipelineOf)
  private def relationOf(k: ListingKey) = clusters.find(_._1.listings(k)).flatMap(_._2)

  "the shadow diff" should "relate each cluster to the pipeline's films over the listings the pipeline has placed" in {
    relationOf(a1) shouldBe Some(ShadowRelation.Identical)
    relationOf(b1) shouldBe Some(ShadowRelation.Moved)
    relationOf(c1) shouldBe Some(ShadowRelation.Merged)
    relationOf(e1) shouldBe Some(ShadowRelation.Split)
    relationOf(e2) shouldBe Some(ShadowRelation.Split)
    relationOf(d1) shouldBe None
    clusters.find(_._1.listings(c1)).get._3 shouldBe Seq(it1990, it2017)
  }

  it should "count every relation, zero included" in {
    ShadowDiffOracle.counts(decisions, pipelineOf) shouldBe Map(ShadowRelation.Identical -> 1, ShadowRelation.Moved -> 1,
      ShadowRelation.Merged -> 1, ShadowRelation.Split -> 2)
    ShadowDiffOracle.counts(Nil, pipelineOf).values.toSet shouldBe Set(0)
  }
}
