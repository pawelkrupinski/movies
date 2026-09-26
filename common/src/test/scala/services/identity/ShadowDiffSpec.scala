package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

/** The shadow diff: each resolver cluster's relation to the pipeline's films, which clusters are a
 *  verdict, and which families are reported. */
class ShadowDiffSpec extends AnyFlatSpec with Matchers {

  private def key(venue: String, title: String): ListingKey = ListingKey.Published(venue, title, None, Nil)
  private val (a1, a2, b1, c1, c2, d1, e1, e2) =
    (key("A", "Belle"), key("B", "Belle"), key("A", "Lalka"), key("A", "It"), key("B", "It"), key("A", "Nowy"),
      key("A", "Opętanie"), key("B", "Opętanie"))

  private def decision(members: Seq[ListingKey], film: Option[Int], confidence: Double = 0.9) =
    ResolverDecision(members, film, confidence, if (film.isDefined) ResolverDecision.Basis.OwnMatch else ResolverDecision.Basis.NoCandidate, Nil)

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
  private val families = Map(a1 -> 0, a2 -> 0, b1 -> 1, c1 -> 2, c2 -> 2, e1 -> 3, e2 -> 3, d1 -> 4)
  private val resolution = Resolution(decisions, nodes = 8, familyOf = families, edges = Nil, queries = Nil, filmLookups = 0,
    unknownQueries = 0, unknownDetails = 0, unknownFilms = 0, violations = 0, films = Map.empty)
  private val pipelineOf = Map(a1 -> belle, a2 -> belle, b1 -> lalka, c1 -> it1990, c2 -> it2017, e1 -> opetanie, e2 -> opetanie)

  private val (clusters, diff) = ShadowDiff.of(resolution, pipelineOf)
  private def relationOf(k: ListingKey) = clusters.find(_.decision.listings(k)).flatMap(_.relation)

  "the shadow diff" should "relate each cluster to the pipeline's films over the listings the pipeline has placed" in {
    relationOf(a1) shouldBe Some(ShadowRelation.Identical)
    relationOf(b1) shouldBe Some(ShadowRelation.Moved)
    relationOf(c1) shouldBe Some(ShadowRelation.Merged)
    relationOf(e1) shouldBe Some(ShadowRelation.Split)
    relationOf(e2) shouldBe Some(ShadowRelation.Split)
    relationOf(d1) shouldBe None
    clusters.find(_.decision.listings(c1)).get.pipelineFilms shouldBe Seq(it1990, it2017)
    clusters.map(_.family) shouldBe Seq(0, 1, 2, 3, 3, 4)
  }

  it should "count every relation, zero included" in {
    ShadowDiff.counts(clusters) shouldBe Map(ShadowRelation.Identical -> 1, ShadowRelation.Moved -> 1,
      ShadowRelation.Merged -> 1, ShadowRelation.Split -> 2)
    ShadowDiff.counts(Nil).values.toSet shouldBe Set(0)
  }

  it should "report only the families where the two part ways, with both sides' listings" in {
    diff.map(_.family) shouldBe Seq(1, 2, 3, 4)
    val opetanieFamily = diff.find(_.family == 3).get
    opetanieFamily.resolver shouldBe Seq(Some(5) -> Seq(e1), Some(6) -> Seq(e2))
    opetanieFamily.pipeline shouldBe Seq(opetanie -> Seq(e1, e2).sorted)
    opetanieFamily.relations shouldBe Map(ShadowRelation.Split -> 2)
    diff.find(_.family == 4).get.unplaced shouldBe Seq(d1)
  }

  it should "count as a verdict only an identical cluster (right) and another film for the same listings (wrong)" in {
    clusters.map(c => c.decision.members.head -> c.agrees).toMap shouldBe
      Map(a1 -> Some(true), b1 -> Some(false), c1 -> None, e1 -> None, e2 -> None, d1 -> None)
    // No film against the pipeline's film is coverage, not a wrong film.
    ShadowCluster(decision(Seq(b1), None), 1, Some(ShadowRelation.Moved), Seq(lalka)).agrees shouldBe None
  }
}
