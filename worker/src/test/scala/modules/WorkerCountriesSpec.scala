package modules

import models.Country
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The boot guard that keeps a worker to one country per process.
 *
 * `WorkerMain` builds one wiring per `KINOWO_COUNTRIES` entry and shares a
 * budget, a Mongo client and a metrics registry between them, so running
 * `pl,de,uk` in one JVM looks like a config flip. The guard was added while the
 * title normalizer still resolved one rule set per process; that is gone (every
 * component takes its wiring's normalizer), so what it pins now is the deploy
 * shape itself until running several countries per JVM is chosen deliberately.
 */
class WorkerCountriesSpec extends AnyFlatSpec with Matchers with OptionValues {

  "unsupportedCountries" should "allow the single-country deploys we actually run" in {
    Seq(Country.Poland, Country.Germany, Country.UnitedKingdom).foreach { c =>
      withClue(s"${c.code}: ")(WorkerMain.unsupportedCountries(Seq(c)) shouldBe None)
    }
  }

  it should "refuse a worker asked to run several countries at once" in {
    WorkerMain.unsupportedCountries(Seq(Country.Poland, Country.Germany, Country.UnitedKingdom)) shouldBe defined
  }

  it should "refuse even a two-country pairing" in {
    WorkerMain.unsupportedCountries(Seq(Country.Poland, Country.Germany)) shouldBe defined
  }

  it should "name the offending countries so the log says which config was rejected" in {
    val why = WorkerMain.unsupportedCountries(Seq(Country.Poland, Country.Germany)).value
    why should include("KINOWO_COUNTRIES")
    why should include(Country.Poland.code)
    why should include(Country.Germany.code)
  }
}
