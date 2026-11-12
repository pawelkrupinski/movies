package models

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Spain's pages after the move from provinces to Poland's shape: a major city
 *  lists only its own venues, and every venue outside one is on a town page or
 *  a cluster of nearby small towns — see `data/spain/scripts/build_pages.py`
 *  and [[SpanishRoster]]. The Spanish counterpart of [[PolishPagesSpec]]. */
class SpanishPagesSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def page(slug: String): City = Country.Spain.bySlug.get(slug).value
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
    onPages should have size 598
  }

  "Spain's picker" should "group every page by province, major cities beside their clusters" in {
    val groups = Country.Spain.cityGroups
    groups should have size 52
    groups.flatMap(_.allCities) should contain theSameElementsAs Country.Spain.cities
    val barcelona = groups.find(_.label == "Barcelona").value.cities
    barcelona should contain allOf (page("barcelona"), page("sitges"), page("cornella-de-llobregat"))
  }

  "a province that is no longer a page" should "redirect to the page holding most of its venues" in {
    Country.Spain.renamedSlugs.get("asturias").value shouldBe "gijon"
    Country.Spain.renamedSlugs.get("islas-baleares").value shouldBe "palma-de-mallorca"
    Country.Spain.renamedSlugs.get("las-palmas").value shouldBe "las-palmas-de-gran-canaria"
    Country.Spain.renamedSlugs.get("vizcaya").value shouldBe "barakaldo"
    Country.Spain.bySlug.get("asturias") shouldBe None
  }

  it should "leave no old province slug unanswered" in {
    val provinces = Seq("a-coruna", "alava", "albacete", "alicante", "almeria", "asturias", "avila", "badajoz",
      "barcelona", "burgos", "caceres", "cadiz", "cantabria", "castellon", "ceuta", "ciudad-real", "cordoba",
      "cuenca", "girona", "granada", "guadalajara", "guipuzcoa", "huelva", "huesca", "islas-baleares", "jaen",
      "la-rioja", "las-palmas", "leon", "lerida", "lugo", "madrid", "malaga", "melilla", "murcia", "navarra",
      "ourense", "palencia", "pontevedra", "salamanca", "santa-cruz-de-tenerife", "segovia", "sevilla", "soria",
      "tarragona", "teruel", "toledo-castilla-la-mancha", "valencia", "valladolid", "vizcaya", "zamora", "zaragoza")
    forAll(provinces) { slug =>
      val landing = Country.Spain.renamedSlugs.getOrElse(slug, slug)
      Country.Spain.bySlug.get(landing).value.country shouldBe Country.Spain
    }
  }

  it should "never redirect a slug another country serves" in {
    val spanishFormer = Country.Spain.renamedSlugs.filter { case (_, to) => Country.Spain.bySlug.get(to).exists(_.country == Country.Spain) }.keySet
    forAll(spanishFormer)(former => Country.Spain.bySlug.get(former) shouldBe None)
  }

  "the Spanish roster data" should "be the generated tables, row for row" in {
    val pageRows = GeneratedTable.rows("generated/es-roster/pages.tsv")
    val cinemaRows = GeneratedTable.rows("generated/es-roster/cinemas.tsv")
    SpanishRosterData.pages.map(_._1) shouldBe pageRows.map(_(0))
    SpanishRosterData.pages.map(_._10.size).sum shouldBe cinemaRows.size
    SpanishRosterData.retired.size shouldBe GeneratedTable.rows("generated/es-roster/retired.tsv").size
    SpanishRosterData.seasonal.size shouldBe GeneratedTable.rows("generated/es-roster/seasonal.tsv").size
  }

  it should "keep a page's fields, a venue's optional ids and an exact Double" in {
    SpanishRosterData.pages.size shouldBe 240
    SpanishRosterData.pages.map(_._10.size).sum shouldBe 598
    SpanishRosterData.pages.head shouldBe (
      "madrid", "madrid-comunidad-de-madrid", "Madrid", "Madrid", 40.4165, -3.7026, "Europe/Madrid", false,
      Seq("Madrid"), SpanishRosterData.pages.head._10)
    val madrid = SpanishRosterData.pages.head._10
    madrid.head shouldBe (("Cines Princesa", "Cines Princesa", Some("E0364"), None))
    madrid.last shouldBe (("Ocine Urban Caleido", "Ocine Urban Caleido", None, Some("tickets.ocineurbancaleido.es")))
    SpanishRosterData.retired should contain (("alava", "alava-pais-vasco", "vitoria-gasteiz"))
    SpanishRosterData.seasonal should contain ("Autocine Getxo")
  }

  private def forAll[A](xs: Iterable[A])(f: A => Unit): Unit = org.scalatest.Inspectors.forAll(xs)(f)
}
