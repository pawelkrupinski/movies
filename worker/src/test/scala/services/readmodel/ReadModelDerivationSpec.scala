package services.readmodel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Which pass a store owes, from the derivation it recorded and the history this code carries. */
class ReadModelDerivationSpec extends AnyFlatSpec with Matchers {
  import DerivationScope.{Cards, Full}

  private def v(name: String) = DerivationVersion(name)
  private val history = Seq(Derivation(v("a"), Full), Derivation(v("b"), Cards), Derivation(v("c"), Cards), Derivation(v("d"), Full))

  "a store on the current derivation" should "owe nothing" in {
    ReadModelDerivation.owedSince(Some(v("d")), history) shouldBe None
  }

  "a store one or more cards-only derivations behind" should "owe only the cards" in {
    ReadModelDerivation.owedSince(Some(v("b")), history.take(3)) shouldBe Some(Cards)
    ReadModelDerivation.owedSince(Some(v("a")), history.take(3)) shouldBe Some(Cards)
  }

  "a store behind a derivation that moved more than cards" should "owe everything, whatever came before it" in {
    ReadModelDerivation.owedSince(Some(v("b")), history) shouldBe Some(Full)
    ReadModelDerivation.owedSince(Some(v("c")), history) shouldBe Some(Full)
  }

  "a store with no derivation recorded, or one this code has never heard of" should "owe everything" in {
    ReadModelDerivation.owedSince(None, history) shouldBe Some(Full)
    ReadModelDerivation.owedSince(Some(v("from-another-branch")), history) shouldBe Some(Full)
  }

  "the history" should "name each version once, so a recorded one resolves to a single place in it" in {
    ReadModelDerivation.History.map(_.version).distinct shouldBe ReadModelDerivation.History.map(_.version)
  }
}
