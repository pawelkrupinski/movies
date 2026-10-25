package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

/** The causes no listing's leaving decides (the projection's spec holds those): what set the projection off, whether the
 *  store holds this process's takes, and whether a decision took another film. */
class TakeCausesSpec extends AnyFlatSpec with Matchers {

  private val pionier: ListingKey =
    ListingKey.Published("Kino Pionier 1907", "Kupiłem motocykl, który jest wampirem", Some(1990), Seq("Dirk Campbell"))
  private val lost  = TakeChange(pionier, Some("film-41153"), Some(41153), None)
  private val prior = Some(TakeChange.Prior(ResolverDecision.Basis.OwnMatch, 0.9, None))
  private def reading(trigger: String = "periodic", ownTakes: Boolean = true,
                      republished: Option[ListingKey => Boolean] = Some(_ => false)) =
    TakeCauses.Reading(_ => None, _ => true, _ => true, republished, _ => Seq(pionier), trigger, ownTakes)

  "a take's cause" should "be the code's while the store holds another build's takes, whatever set the projection off" in {
    TakeCauses.of(lost, reading("deploy", ownTakes = false)) shouldBe Some(TakeChange.Cause.Code)
    TakeCauses.of(lost, reading("answer", ownTakes = false, republished = None)) shouldBe Some(TakeChange.Cause.Code)
  }

  it should "be the evidence's when an answer filed or a stored film moved set the projection off, or a listing it stood on was re-published" in {
    TakeCauses.of(lost.copy(prior = prior), reading("answer+scrape")) shouldBe Some(TakeChange.Cause.Evidence)
    TakeCauses.of(lost.copy(prior = prior), reading("film-change")) shouldBe Some(TakeChange.Cause.Evidence)
    TakeCauses.of(lost.copy(prior = prior), reading(ownTakes = false, republished = Some(Set(pionier)))) shouldBe Some(TakeChange.Cause.Evidence)
  }

  it should "be the code's for this build's decision replaced on the same inputs, regrouped when no decision took another film" in {
    TakeCauses.of(lost.copy(prior = prior), reading("scrape")) shouldBe Some(TakeChange.Cause.Code)
    TakeCauses.of(lost, reading("scrape")) shouldBe Some(TakeChange.Cause.Regrouped)
  }

  it should "name no cause for a new listing's first take" in {
    TakeCauses.of(TakeChange(pionier, None, None, Some(41153)), reading()) shouldBe None
  }
}
