package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InstalledBuildSlugsSpec extends AnyFlatSpec with Matchers {

  "InstalledBuildSlugs.qualifiedSlugs" should "keep a page's own slug when the table has no qualified slug for it, not throw" in {
    // An old owner whose slug now answers only through an alias: the keeper names
    // ANOTHER country for the slug, but the generated table lists only pages whose
    // slug another country has as a page or a retired page, so this page has none.
    val york = Country.UnitedStates.bySlug("york")
    val keeper = Map("york" -> Country.UnitedKingdom)
    val table = Map("us" -> Map.empty[String, String])
    InstalledBuildSlugs.qualifiedSlugs(Seq(Country.UnitedStates), keeper, table).get((Country.UnitedStates, york.slug)) shouldBe None
    InstalledBuildSlugs.qualifiedSlugs(Seq(Country.UnitedStates), keeper, Map.empty) shouldBe empty
  }

  it should "qualify a page another country keeps bare where the table names its qualified slug" in {
    val keeper = Map("york" -> Country.UnitedKingdom)
    val table = Map("us" -> Map("york" -> "york-pennsylvania"))
    InstalledBuildSlugs.qualifiedSlugs(Seq(Country.UnitedStates), keeper, table) shouldBe
      Map((Country.UnitedStates, "york") -> "york-pennsylvania")
  }

}
