package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, Json}

import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** The UK roster's two GEOGRAPHY rules, held over `data/uk/venues.json` (each
 *  venue's harvested fix) and the hand-written roster (each page's hub and the
 *  venues filed under it).
 *
 *  The UK keeps its counties as the page unit — unlike Poland's, which are
 *  clustered from scratch by `data/pl/scripts/build_pages.py` — so these do not
 *  re-cluster anything. They catch the two ways a hand-filed county roster goes
 *  wrong, with the same straight-line distances Poland's builder uses; see
 *  `data/uk/README.md`. */
class UkPageGeographySpec extends AnyFlatSpec with Matchers {
  import UkPageGeographySpec._

  private val pages: Seq[City] = City.allUkCities

  /** Every UK venue's fix, by the display name it is stored under. */
  private val fixes: Map[String, GeoPoint] = {
    val raw = new String(Files.readAllBytes(testsupport.RepoRoot.file("data/uk/venues.json").toPath),
                         StandardCharsets.UTF_8)
    Json.parse(raw).as[JsArray].value.toSeq.flatMap { v =>
      for {
        name <- (v \ "displayName").asOpt[String]
        lat  <- (v \ "lat").asOpt[Double]
        lon  <- (v \ "lon").asOpt[Double]
      } yield name -> GeoPoint(lat, lon)
    }.toMap
  }

  "Every UK venue" should "have a harvested fix, bar the one Flicks does not list" in {
    // The rules below can only judge a venue they can place. The Old Court in
    // Windsor is wired to no Flicks slug, so the harvest never saw it; naming it
    // here means the NEXT venue to go missing fails instead of being skipped.
    pages.flatMap(_.cinemas).map(_.displayName).filterNot(fixes.contains) shouldBe Seq("The Old Court Windsor")
  }

  it should "sit on its own page's hub unless it is far from it AND close to another" in {
    // A venue more than `FarFromHubKm` from its page's hub that is within
    // `NearAnotherHubKm` of a DIFFERENT page's hub was filed under the wrong
    // county. A remote venue with no hub near it at all — Lerwick, 285 km up the
    // Highlands and Islands — is far from everything and stays where it is.
    val misfiled = for {
      page   <- pages
      cinema <- page.cinemas
      fix    <- fixes.get(cinema.displayName).toSeq
      if fix.kmTo(page.centre) > FarFromHubKm
      other  <- pages.filterNot(_ == page)
      if fix.kmTo(other.centre) <= NearAnotherHubKm
    } yield f"${cinema.displayName}: ${fix.kmTo(page.centre)}%.1f km from ${page.slug}'s hub, " +
            f"${fix.kmTo(other.centre)}%.1f km from ${other.slug}'s"
    misfiled shouldBe empty
  }

  "No two UK pages" should "have hubs close enough to be one urban area" in {
    val tooClose = for {
      (a, i) <- pages.zipWithIndex
      b      <- pages.drop(i + 1)
      if a.centre.kmTo(b.centre) < SameUrbanAreaKm
    } yield f"${a.slug}/${b.slug}: ${a.centre.kmTo(b.centre)}%.1f km"
    tooClose shouldBe empty
  }

  "A UK page folded into its neighbour" should "redirect onto the page that absorbed it" in {
    // Dudley and Sandwell were boroughs of the Birmingham conurbation 7-13 km
    // from its hub; Lanarkshire's hub is East Kilbride/Hamilton, 12 km from
    // Glasgow's. Old links land on the page holding their venues now.
    City.renamedSlugs.get("dudley")      shouldBe Some("birmingham")
    City.renamedSlugs.get("sandwell")    shouldBe Some("birmingham")
    City.renamedSlugs.get("lanarkshire") shouldBe Some("glasgow")
    Country.UnitedKingdom.bySlug.get("dudley") shouldBe None
    Country.UnitedKingdom.bySlug("birmingham").cinemas.map(_.displayName) should contain allOf (
      "Odeon Cinema Dudley", "Showcase Cinema Dudley", "Odeon Cinema West Bromwich")
    Country.UnitedKingdom.bySlug("glasgow").cinemas.map(_.displayName) should contain allOf (
      "Odeon Luxe East Kilbride", "Showcase Glasgow Coatbridge", "Vue Cinemas Hamilton")
  }

  it should "stay findable in the picker by the name it used to have" in {
    val rows = Country.UnitedKingdom.cityGroups.flatMap(_.groups)
    def aliasesOf(slug: String): Seq[String] =
      rows.find(_.soleCity.exists(_.slug == slug)).fold(Seq.empty[String])(_.searchAliases)
    aliasesOf("birmingham") should contain allOf ("West Midlands", "Dudley", "Sandwell")
    aliasesOf("glasgow")    should contain ("Lanarkshire")
  }
}

object UkPageGeographySpec {
  /** Further than this from its own hub, a venue is a candidate for re-filing. */
  val FarFromHubKm = 40.0
  /** …and it IS re-filed when another page's hub is at most this far away — the
   *  outer town-cluster radius Poland's builder uses. */
  val NearAnotherHubKm = 25.0
  /** Two hubs closer than this are one town's two halves, not two places. */
  val SameUrbanAreaKm = 15.0
}
