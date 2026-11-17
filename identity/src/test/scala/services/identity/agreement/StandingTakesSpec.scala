package services.identity.agreement

import models.{KinoMuza, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{FilmTable, Listing, PosterAnswers, Question, Resolution, ResolverDecision}

/** A cluster's last settled take stands while its take waits ([[StandingTakes]]): served in the waiting take's place,
 *  typed by the hold, while that take takes no film over the same listings and for at most
 *  [[AgreementStage.HoldAtMost]] — and the hold ends once nothing is waited on. */
class StandingTakesSpec extends AnyFlatSpec with Matchers {
  import FilmTable.listing

  private val (muza, multikino) = (listing(KinoMuza, "Klondike", year = Some(2022)), listing(Multikino, "Klondike", year = Some(2022)))
  private val record            = Question.Record(913760)

  private def took(film: Option[Int], listings: Listing*) =
    ResolverDecision(listings.map(_.key), film, 0.9, ResolverDecision.Basis.Agreed, Nil)()
  private def resolution(decisions: ResolverDecision*) = {
    val keys = decisions.flatMap(_.members)
    Resolution(decisions, keys.size, keys.zipWithIndex.toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
  }

  /** One apply: `model` keyed "7", taken as `take`, served by `standing` while the cluster waits on `waits`. */
  private def served(standing: StandingTakes, model: ResolverDecision, take: ResolverDecision, waits: Set[Question] = Set.empty) = {
    val pass = new AgreementStage.Pass(1L, 1, new AgreementStage.Read(PosterAnswers.Silent, None))
    pass.keys.put(model, "7")
    val got = standing.served(resolution(model), Seq(take), pass, _ => Seq(waits).filter(_.nonEmpty)).head
    (got, pass)
  }

  "A cluster whose take waits and takes no film" should "be served its last settled take over the same listings, keyed as its cluster" in {
    val clock    = new _root_.tools.MutableClock(_root_.tools.SpecClock.Pinned.instant())
    val standing = new StandingTakes(clock)
    val model    = took(None, muza, multikino)
    val agreed   = took(Some(913760), muza, multikino)
    served(standing, model, agreed)._1 shouldBe agreed
    val waiting  = took(None, muza, multikino)
    val (got, pass) = served(standing, waiting, waiting, Set(record))
    got shouldBe theSameInstanceAs (agreed)
    pass.keys.get(agreed) shouldBe "7"
    standing.holdOf("7", agreed.members).map(_.waits) shouldBe Some(Set(record))
    standing.held().listings shouldBe 2
    clock.advanceSeconds(3)
    val (settled, ended) = served(standing, waiting, waiting)
    settled shouldBe theSameInstanceAs (waiting)
    ended.holdsEnded.toSeq shouldBe Seq(3.0)
    standing.held().listings shouldBe 0
  }

  it should "stand for at most the bound, then be served as it stands" in {
    val clock    = new _root_.tools.MutableClock(_root_.tools.SpecClock.Pinned.instant())
    val standing = new StandingTakes(clock)
    val agreed   = took(Some(913760), muza, multikino)
    served(standing, agreed, agreed)
    val waiting  = took(None, muza, multikino)
    served(standing, waiting, waiting, Set(record))._1 shouldBe agreed
    standing.due shouldBe false
    clock.advance(AgreementStage.HoldAtMost)
    standing.due shouldBe true
    val (got, pass) = served(standing, waiting, waiting, Set(record))
    got shouldBe theSameInstanceAs (waiting)
    pass.holdsBounded.toSeq shouldBe Seq(AgreementStage.HoldAtMost.toSeconds.toDouble)
    standing.due shouldBe false
  }

  "A cluster whose listings moved" should "be served its take as it stands, nothing held" in {
    val standing = new StandingTakes(_root_.tools.SpecClock.Pinned)
    val agreed   = took(Some(913760), muza, multikino)
    served(standing, agreed, agreed)
    val reshaped = took(None, muza)
    served(standing, reshaped, reshaped, Set(record))._1 shouldBe theSameInstanceAs (reshaped)
    standing.held().listings shouldBe 0
  }
}
