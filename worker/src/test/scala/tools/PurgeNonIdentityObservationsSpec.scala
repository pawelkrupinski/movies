package tools

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.PurgeNonIdentityObservations.{Mode, Request, parse}

/** The cleanup's command line: a dry run of every country unless told otherwise. The purge
 *  itself runs against Mongo in `MongoObservationStoreIntegrationSpec`. */
class PurgeNonIdentityObservationsSpec extends AnyFlatSpec with Matchers {

  "the purge" should "be a dry run over every country by default" in {
    parse(Nil) shouldBe Right(Request(Mode.DryRun, Country.all))
  }

  it should "delete only under --apply, narrowed to the countries named" in {
    parse(Seq("--apply", "de", "UK")) shouldBe Right(Request(Mode.Apply, Seq(Country.Germany, Country.UnitedKingdom)))
  }

  it should "refuse an unknown flag or country rather than guess" in {
    parse(Seq("--aply")).isLeft shouldBe true
    parse(Seq("xx")).isLeft shouldBe true
  }
}
