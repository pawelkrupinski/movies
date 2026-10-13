package models

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, Json}

import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** The UK's pages after the move from 76 hand-filed Flicks regions to Poland's
 *  shape: a major city lists only its own venues, every other venue is on a town
 *  page or a cluster of nearby small towns, and no venue is more than 40 km from
 *  the town its page is named after — see `data/uk/scripts/build_pages.py` and
 *  [[UkPages]]. The UK counterpart of [[SpanishPagesSpec]]. */
class UkPagesSpec extends AnyFlatSpec with Matchers with OptionValues {
  import UkPagesSpec._

  private def page(slug: String): City = Country.UnitedKingdom.bySlug.get(slug).value
  private def venue(name: String): Cinema = Cinema.byDisplayName.get(name).value
  private def pageOf(name: String): String = City.forCinema(venue(name)).value.slug

  /** Every UK venue's coordinates, by the display name it is stored under. */
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

  "Every UK venue" should "have coordinates in data/uk/venues.json" in {
    // The cap below can only judge a venue it can place, so the next venue to go
    // missing fails here instead of being skipped there.
    Country.UnitedKingdom.cities.flatMap(_.cinemas).map(_.displayName).filterNot(fixes.contains) shouldBe empty
  }

  it should "be within 40 km of the town its page is named after" in {
    val far = for {
      city   <- Country.UnitedKingdom.cities
      cinema <- city.cinemas
      fix    <- fixes.get(cinema.displayName).toSeq
      if fix.kmTo(city.centre) > MaxReachKm
    } yield f"${cinema.displayName}: ${fix.kmTo(city.centre)}%.1f km from ${city.slug}"
    far shouldBe empty
  }

  "The UK's pages" should "leave no venue off a page, or on two" in {
    val onPages = Country.UnitedKingdom.cities.flatMap(_.cinemas)
    onPages.distinct should have size onPages.size
    onPages.toSet shouldBe fixes.keySet.map(venue)
  }

  "a major city" should "list only the venues inside it" in {
    page("leeds").cinemas should contain (venue("Hyde Park Picture House"))
    page("leeds").cinemas should not contain venue("Ilkley Cinema")   // was on /west-yorkshire/ with it
    page("bradford").cinemas should contain (venue("Pictureville (Science and Media Museum Bradford)"))
  }

  "London" should "be every venue in Greater London, and nothing outside it, in its compass areas" in {
    London.cinemas should contain allOf (venue("David Lean Cinema Croydon"), venue("Lumiere Romford"))
    // Surrey's, which the old London page listed, are Surrey's pages now.
    London.cinemas should not contain venue("Nova Cinema Woking")
    pageOf("Nova Cinema Woking") shouldBe "woking"
    pageOf("Leatherhead Cinema") shouldBe "epsom"
    London.areas.flatMap(_.cinemas) should contain theSameElementsAs London.cinemas
  }

  "a cluster page" should "be named after its biggest town, and speak of it as an area" in {
    val epsom = page("epsom")
    epsom.labels.nominative shouldBe "Epsom area"
    epsom.locativePhrase shouldBe "in the Epsom area"
    epsom.coveredPlaces should contain allOf ("Epsom", "Esher", "Leatherhead")
  }

  "an island" should "never share a page with the mainland" in {
    page("newport-isle-of-wight").coveredPlaces should contain allOf ("Newport", "Ryde")
    pageOf("Southsea Cinema & Arts Centre") should not be "newport-isle-of-wight"
    page("lerwick").cinemas.map(_.displayName) shouldBe Seq("Mareel Lerwick, Shetland Islands")
  }

  "two UK towns of one name" should "be two pages, the more populous with the bare slug" in {
    // Each country owns its slugs, so only another UK Newport qualifies one.
    page("newport").coveredPlaces should contain ("Newport")
    page("newport").lat should be > 51.5            // Gwent's, not the Isle of Wight's
    page("newport-isle-of-wight").coveredPlaces should contain ("Ryde")
    page("bangor-gwynedd").labels.nominative should startWith ("Bangor")
    page("bangor").lat should be > 54.0             // County Down's
  }

  "every retired UK slug" should "redirect to a UK page" in {
    val gone = HandFiled.filterNot(slug => Country.UnitedKingdom.bySlug.contains(slug))
    gone.filterNot(Country.UnitedKingdom.renamedSlugs.contains) shouldBe empty
    gone.map(Country.UnitedKingdom.renamedSlugs).filterNot(Country.UnitedKingdom.bySlug.contains) shouldBe empty
    // A region named after a town goes to the page holding it; a county, to the
    // page holding most of its venues.
    Country.UnitedKingdom.renamedSlugs("antrim") shouldBe pageOf("Omniplex Antrim")
    Country.UnitedKingdom.renamedSlugs("west-yorkshire") shouldBe "leeds"
    // A page folded into a neighbour before the re-cut follows its venues too,
    // not the neighbour it was folded into.
    Country.UnitedKingdom.renamedSlugs("sandwell") shouldBe pageOf("Odeon Cinema West Bromwich")
    Country.UnitedKingdom.renamedSlugs("lanarkshire") shouldBe pageOf("Vue Cinemas Hamilton")
    Country.UnitedKingdom.renamedSlugs("highlands-and-islands") shouldBe pageOf("Eden Court Theatre Inverness")
  }

  "The UK picker" should "file every page under its nation and county, nations in their fixed order" in {
    Country.UnitedKingdom.cityGroups.map(_.label) shouldBe UkPages.nations
    Country.UnitedKingdom.cityGroups.flatMap(_.allCities) should contain theSameElementsAs Country.UnitedKingdom.cities
    val england = Country.UnitedKingdom.cityGroups.head
    england.groups.find(_.label == "West Yorkshire").value.cities should contain allOf (page("leeds"), page("bradford"))
    // A county of one page collapses onto it, its name kept as a search alias.
    val shetland = Country.UnitedKingdom.cityGroups.flatMap(_.groups).find(_.label == "Shetland").value
    shetland.soleCity shouldBe Some(page("lerwick"))
    shetland.searchAliases shouldBe Seq("Shetland")
  }
}

object UkPagesSpec {
  /** No venue further than this from the town its page is named after. */
  val MaxReachKm = 40.0

  /** The UK's slugs before it was rebuilt from towns (2026-10): the 76 Flicks
   *  regions and the three already folded into a neighbour. */
  val HandFiled: Seq[String] = """
    aberdeenshire antrim armagh ayrshire-and-arran bedfordshire belfast berkshire birmingham bristol buckinghamshire
    cambridgeshire cardiff central-scotland cheshire clwyd cornwall county-durham cumbria derbyshire devon dorset down
    dudley dumfries-and-galloway dunbartonshire-argyll-bute dyfed east-sussex east-yorkshire edinburgh-and-lothians
    essex fermanagh fife glamorgan glasgow gloucestershire guernsey gwent gwynedd hampshire herefordshire hertfordshire
    highlands-and-islands isle-of-man isle-of-wight jersey kent lanarkshire lancashire leicestershire lincolnshire
    liverpool london londonderry manchester north-yorkshire northamptonshire northumberland norwich nottinghamshire
    oxfordshire powys renfrewshire roxburgh-ettrick-and-lauderdale sandwell shropshire somerset south-yorkshire
    staffordshire suffolk surrey tayside tyne-and-wear tyrone warwickshire west-sussex west-yorkshire wiltshire
    worcestershire yorkshire""".split("\\s+").toSeq.filter(_.nonEmpty)
}
