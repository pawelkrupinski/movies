package models

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Spain's pages after the move from provinces to Poland's shape: a major city
 *  lists only its own venues, and every venue outside one is on a town page or
 *  a cluster of nearby small towns — see `data/spain/scripts/build_pages.py`
 *  and [[SpanishRoster]]. The Spanish counterpart of [[PolishPagesSpec]]. */
class SpanishPagesSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def page(slug: String): City = City.bySlug(slug).value
  private def venue(name: String): Cinema = Cinema.byDisplayName.get(name).value
  private def pageOf(name: String): String = City.forCinema(venue(name)).value.slug

  "a major city" should "list only the venues inside it" in {
    page("barcelona").cinemas should contain (venue("Arenas Multicines 3D"))
    page("barcelona").cinemas should not contain venue("Multicines Catalunya")      // Berga, 100 km out
    page("barcelona").cinemas should not contain venue("Cinesa Llobregat Centre")   // Cornellà
    page("madrid").cinemas should not contain venue("Yelmo Cines Planetocio")       // Collado Villalba
  }

  "a Barcelona-province town with three or more venues" should "be a page of its own" in {
    val cornella = page("cornella-de-llobregat")
    cornella.labels.nominative shouldBe "Cornellà de Llobregat"
    cornella.cinemas.map(_.displayName) should contain allOf (
      "Cinesa Llobregat Centre", "Kinépolis Barcelona Full Splau", "Odeon Multicines Llobregat")
  }

  "a cluster page" should "be named after its biggest town, and speak of it 'y alrededores'" in {
    val sitges = page("sitges")
    sitges.labels.nominative shouldBe "Sitges y alrededores"
    sitges.locativePhrase shouldBe "en Sitges y alrededores"
    sitges.coveredPlaces should contain allOf ("Sitges", "Vilanova i la Geltrú")
    pageOf("Cinema Ribes") shouldBe "sitges"   // Sant Pere de Ribes
  }

  "a one-town page" should "just name its town" in {
    page("berga").labels.nominative shouldBe "Berga"
    page("berga").locativePhrase shouldBe "en Berga"
  }

  "a venue SensaCine files under the wrong town" should "be on the page of the town its address is in" in {
    // Filed under "Estacion De Espiel", a Córdoba hamlet; postal code 04600 is Huércal-Overa,
    // in Almería — which clusters with Águilas, 30 km away over the Murcia line.
    pageOf("Cine Municipal Huércal-Overa") shouldBe "aguilas"
    page("aguilas").coveredPlaces should contain ("Huércal-Overa")
  }

  "Ceuta" should "keep its own page rather than join Algeciras', 28 km across the Strait" in {
    page("ceuta").coveredPlaces shouldBe Seq("Ceuta")
    page("algeciras").coveredPlaces should not contain "Ceuta"
  }

  "Spain's pages" should "leave no venue off a page, or on two" in {
    val onPages = Country.Spain.cities.flatMap(_.cinemas)
    onPages.distinct should have size onPages.size
    onPages.toSet shouldBe SpanishRoster.byCity.flatMap(_._2).toSet
    onPages should have size 602
  }

  "Spain's picker" should "group every page by province, major cities beside their clusters" in {
    val groups = Country.Spain.cityGroups
    groups should have size 52
    groups.flatMap(_.allCities) should contain theSameElementsAs Country.Spain.cities
    val barcelona = groups.find(_.label == "Barcelona").value.cities
    barcelona should contain allOf (page("barcelona"), page("sitges"), page("cornella-de-llobregat"))
  }

  "a province that is no longer a page" should "redirect to the page holding most of its venues" in {
    City.renamedSlugs.get("asturias").value shouldBe "gijon"
    City.renamedSlugs.get("islas-baleares").value shouldBe "palma-de-mallorca"
    City.renamedSlugs.get("las-palmas").value shouldBe "las-palmas-de-gran-canaria"
    City.renamedSlugs.get("vizcaya").value shouldBe "barakaldo"
    City.bySlug("asturias") shouldBe None
  }

  it should "leave no old province slug unanswered" in {
    val provinces = Seq("a-coruna", "alava", "albacete", "alicante", "almeria", "asturias", "avila", "badajoz",
      "barcelona", "burgos", "caceres", "cadiz", "cantabria", "castellon", "ceuta", "ciudad-real", "cordoba",
      "cuenca", "girona", "granada", "guadalajara", "guipuzcoa", "huelva", "huesca", "islas-baleares", "jaen",
      "la-rioja", "las-palmas", "leon", "lerida", "lugo", "madrid", "malaga", "melilla", "murcia", "navarra",
      "ourense", "palencia", "pontevedra", "salamanca", "santa-cruz-de-tenerife", "segovia", "sevilla", "soria",
      "tarragona", "teruel", "toledo-castilla-la-mancha", "valencia", "valladolid", "vizcaya", "zamora", "zaragoza")
    forAll(provinces) { slug =>
      val landing = City.renamedSlugs.getOrElse(slug, slug)
      City.bySlug(landing).value.country shouldBe Country.Spain
    }
  }

  it should "never redirect a slug another country serves" in {
    val spanishFormer = City.slugSuccession.filter { case (_, to) => to.exists(City.bySlug(_).exists(_.country == Country.Spain)) }.keySet
    forAll(spanishFormer)(former => City.bySlug(former) shouldBe None)
  }

  private def forAll[A](xs: Iterable[A])(f: A => Unit): Unit = org.scalatest.Inspectors.forAll(xs)(f)
}
