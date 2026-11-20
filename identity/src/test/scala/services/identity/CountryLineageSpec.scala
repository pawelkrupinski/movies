package services.identity

import services.identity.measures.{CountryLineage, PinnedGateMeasures}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The checked-in table of dissolved states ([[CountryLineage]]); how the country measure reads it is
 *  `IdentityMeasuresSpec`'s "a dissolved state". */
class CountryLineageSpec extends AnyFlatSpec with Matchers {
  "the checked-in table" should "name only codes and spellings the JDK's table leaves to it" in {
    CountryLineage.table.map(_.code) should contain allOf ("XC", "SU", "YU", "XG", "CS")
    CountryLineage.table.foreach { state =>
      withClue(state.code)(java.util.Locale.getISOCountries should not contain state.code)
      state.successors.foreach(code => withClue(s"${state.code} → $code")(code should fullyMatch regex "[A-Z]{2}"))
      // a spelling the JDK maps is the JDK's, never shadowed by the table
      state.spellings.foreach(name => withClue(name)(PinnedGateMeasures.countryCode(name) shouldBe None))
    }
  }
}
