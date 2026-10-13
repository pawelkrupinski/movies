package models

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Germany's pages after the move from 35 km regions to Poland's shape: a major
 *  city lists only its own venues, and every venue outside one is on a town page
 *  or a cluster of nearby small towns — see `data/germany/scripts/build_pages.py`
 *  and [[GermanRoster]]. The German counterpart of [[SpanishPagesSpec]]. */
class GermanPagesSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def page(slug: String): City = Country.Germany.bySlug.get(slug).value
  private def venue(name: String): Cinema = Cinema.byDisplayName.get(name).value
  private def pageOf(name: String): String = City.forCinema(venue(name)).value.slug

  "a major city" should "list only the venues inside it" in {
    page("koeln").cinemas should contain (venue("Cinedom"))
    page("koeln").cinemas should not contain venue("Kino in der Brotfabrik")   // Bonn — once on /koeln/
    page("berlin").cinemas should not contain venue("Filmmuseum Potsdam")      // once on /berlin/
    page("berlin").coveredPlaces shouldBe Seq("Berlin")
  }

  it should "take the venues Filmstarts files under one of its districts" in {
    pageOf("Lichtspiele Kalk") shouldBe "koeln"
  }

  "a town with three or more venues" should "be a page of its own" in {
    val ansbach = page("ansbach")
    ansbach.labels.nominative shouldBe "Ansbach"
    ansbach.cinemas.map(_.displayName) should contain allOf ("Capitol Kinocenter", "Kammerspiele Ansbach", "Kultur am Schloss")
  }

  "a cluster page" should "be named after its biggest town, and speak of it 'und Umgebung'" in {
    val bayreuth = page("bayreuth")
    bayreuth.labels.nominative shouldBe "Bayreuth und Umgebung"
    bayreuth.locativePhrase shouldBe "in Bayreuth und Umgebung"
    bayreuth.coveredPlaces shouldBe Seq("Bayreuth und Umgebung", "Bayreuth", "Kulmbach", "Pegnitz", "Hollfeld")
    pageOf("Cineplex Kulmbach") shouldBe "bayreuth"
  }

  "a one-town page" should "just name its town" in {
    page("ilmenau").labels.nominative shouldBe "Ilmenau"
    page("ilmenau").locativePhrase shouldBe "in Ilmenau"
  }

  "a venue whose town GeoNames matched to a namesake" should "be on the page of the town it is really in" in {
    pageOf("Capitol-Kinocenter Lohne (Oldenburg)") shouldBe "cloppenburg"   // not Löhne, Westphalia
    pageOf("Sommernachtskino (Löhne)") shouldBe "herford"
    pageOf("Kinopolis Main-Taunus") shouldBe "hofheim-am-taunus"            // Sulzbach (Taunus)
    pageOf("KuKi - Das Kultkino") shouldBe "dingolfing"                     // Landau an der Isar
    pageOf("Park-Theater Buckow") shouldBe "fuerstenwalde-an-der-spree"     // Buckow (Märkische Schweiz)
  }

  "a venue sharing a name with another country's" should "be qualified with its own town" in {
    // A re-cluster moves a venue between pages but never between towns, so its wire key holds still.
    pageOf("Studio Sankt Augustin") shouldBe "siegburg"
    pageOf("Cineworld Recklinghausen") shouldBe "recklinghausen"
  }

  "Germany's pages" should "leave no venue off a page, or on two" in {
    val onPages = Country.Germany.cities.flatMap(_.cinemas)
    onPages.distinct should have size onPages.size
    onPages.toSet shouldBe GermanRoster.byCity.flatMap(_._2).toSet
    onPages should have size GermanRoster.venueKeyByCinema.size   // CountrySpec pins the count
  }

  "Germany's picker" should "group every page by Bundesland, major cities beside their clusters" in {
    val groups = Country.Germany.cityGroups
    groups should have size 16
    groups.flatMap(_.allCities) should contain theSameElementsAs Country.Germany.cities
    val nrw = groups.find(_.label == "Nordrhein-Westfalen").value.cities
    nrw should contain allOf (page("koeln"), page("bonn"), page("marl"), page("minden"))
    groups.find(_.label == "Niedersachsen").value.cities should contain (page("cloppenburg"))
  }

  "a German page" should "keep its own name, whatever another country calls a town" in {
    // Each country owns its slugs; a page's Land-qualified slug 301s to it.
    Country.Germany.bySlug("potsdam").country shouldBe Country.Germany
    Country.Germany.redirectFor("koeln-nordrhein-westfalen") shouldBe Some("koeln")
  }

  "a region that is no longer a page" should "redirect to the page holding the town it was named after" in {
    // Vechta clusters with Cloppenburg; most of the old region's venues are on
    // Wildeshausen's page, but a visitor typing /vechta/ wants Vechta.
    Country.Germany.renamedSlugs.get("vechta").value shouldBe "cloppenburg"
    Country.Germany.renamedSlugs.get("korbach").value shouldBe "meschede"
    Country.Germany.renamedSlugs.get("itzehoe").value shouldBe "elmshorn"
    Country.Germany.renamedSlugs.get("dorsten").value shouldBe "marl"
    Country.Germany.renamedSlugs.get("sassnitz").value shouldBe "stralsund"
    Country.Germany.bySlug.get("vechta") shouldBe None
  }

  it should "fall back to the page holding most of its venues when its town has none left" in {
    // Sigmaringen's own cinema closed; most of its region's venues are on Tuttlingen's page.
    Country.Germany.renamedSlugs.get("sigmaringen").value shouldBe "tuttlingen"
  }

  it should "leave no old region slug unanswered" in {
    val regions = Seq("altenburg", "altensteig", "amberg", "anklam", "ansbach", "arnsberg", "aschaffenburg",
      "augsburg", "bad-aibling", "bad-berleburg", "bad-hersfeld", "bad-kreuznach", "bad-lobenstein",
      "bad-neuenahr-ahrweiler", "bad-schwalbach", "bad-toelz", "bad-urach", "bamberg", "bautzen", "bayreuth",
      "berchtesgaden", "berlin", "bernburg", "bernkastel-kues", "bielefeld", "brandenburg", "braunschweig", "bremen",
      "bremerhaven", "burghausen", "butzbach", "celle", "cham", "chemnitz", "coburg", "cottbus", "crailsheim",
      "cuxhaven", "daun", "demmin", "dessau-rosslau", "dorsten", "dortmund", "dresden", "eberswalde-finow",
      "eisenach", "emden", "erbach-im-odenwald", "erfurt", "fehmarn", "finsterwalde", "fischbach-bei-dahn",
      "flensburg", "frankfurt-am-main", "frankfurt-an-der-oder", "freiburg", "fulda", "garmisch-partenkirchen",
      "goerlitz", "goettingen", "goslar", "gummersbach", "hachenburg", "halberstadt", "hamburg", "hameln",
      "hannover", "hechingen", "heilbronn", "helgoland", "husum", "ingolstadt", "itzehoe", "jena", "juelich",
      "kaiserslautern", "karlsruhe", "kassel", "kiel", "kleve", "kloeden", "koblenz", "koeln", "konstanz", "korbach",
      "krefeld", "landsberg-am-lech", "landshut", "langeoog", "leipzig", "letschin", "loerrach", "luckenwalde",
      "luebben", "luebeck", "lueneburg", "lychen", "magdeburg", "mannheim", "marburg", "marktoberdorf",
      "marktredwitz", "memmingen", "meppen", "minden", "muenchen", "muenster", "neubrandenburg", "neuruppin",
      "niebuell", "nienburg", "noerdlingen", "nordhausen", "nuernberg", "oberstdorf", "offenburg", "oldenburg",
      "osnabrueck", "paderborn", "parchim", "passau", "plauen", "prenzlau", "prerow", "ravensburg", "regen",
      "regensburg", "rheine", "riesa", "rostock", "rotenburg-an-der-wuemme", "saarbruecken", "salzwedel", "sassnitz",
      "schwaebisch-gmuend", "schwedt", "schweinfurt", "schwerin", "sigmaringen", "sosa", "stendal", "stralsund",
      "straubing", "stuttgart", "suhl", "tauberbischofsheim", "traunstein", "trier", "uffenheim", "ulm", "vechta",
      "villingen-schwenningen", "waldshut-tiengen", "waren-mueritz", "weissenburg", "westerland", "wittenberge",
      "wuppertal")
    regions should have size 158
    forAll(regions) { slug =>
      val landing = Country.Germany.renamedSlugs.getOrElse(slug, slug)
      Country.Germany.bySlug.get(landing).value.country shouldBe Country.Germany
    }
    regions.count(Country.Germany.bySlug.get(_).isDefined) shouldBe 138
  }

  it should "never redirect a slug another country serves" in {
    val germanFormer = Country.Germany.renamedSlugs.filter { case (_, to) => Country.Germany.bySlug.get(to).exists(_.country == Country.Germany) }.keySet
    forAll(germanFormer)(former => Country.Germany.bySlug.get(former) shouldBe None)
  }

  // Filmstarts' directory never linked these cities; its id sweep and Cineamo/kinoheld found
  // their cinemas (data/germany/venue-sources.json, add_found_venues.py, 2026-10-11).
  "a city Filmstarts' directory never reached" should "be a page holding its cinemas" in {
    page("bochum").cinemas.map(_.displayName) should contain theSameElementsAs Seq(
      "Capitol Bochum", "Casablanca Bochum", "Endstation Kino", "Metropolis Bochum", "UCI Bochum", "Union Bochum")
    page("aachen").cinemas.map(_.displayName) should contain theSameElementsAs Seq(
      "Apollo Aachen", "Capitol Aachen", "Cineplex Aachen", "Eden Palast Aachen", "Filmstudio an der RWTH", "Raststätte")
    page("solingen").cinemas.map(_.displayName) should contain theSameElementsAs Seq("Das Lumen Solingen", "Kino im Cobra")
    page("siegen").cinemas.map(_.displayName) should contain theSameElementsAs Seq("CineStar Siegen", "Kurbelkiste")
    page("hamm").cinemas.map(_.displayName) shouldBe Seq("Cineplex Hamm")
  }

  "a venue Filmstarts does not list" should "be keyed by the feed that found it, and have no theaterId" in {
    GermanRoster.venueKeyByCinema(venue("Raststätte")) shouldBe "kinoheld:2417"
    GermanRoster.theaterIdByCinema.get(venue("Raststätte")) shouldBe None
    GermanRoster.theaterIdByCinema(venue("UCI Bochum")) shouldBe "A1216"   // the id sweep's: Filmstarts lists it
  }

  // Filmstarts still lists the Central at its old Maxstrasse address beside its present
  // one in the Bürgerbräu; only the second has a programme (data/germany/retired.json).
  "a cinema that moved" should "be on its page once, at its present address" in {
    page("wuerzburg").cinemas.map(_.displayName) should contain theSameElementsAs Seq(
      "Central im Bürgerbräu", "CinemaxX Würzburg")
  }

  private def forAll[A](xs: Iterable[A])(f: A => Unit): Unit = org.scalatest.Inspectors.forAll(xs)(f)
}
