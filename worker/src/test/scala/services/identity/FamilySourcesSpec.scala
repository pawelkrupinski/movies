package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.{FilmwebClient, ImdbClient, MetacriticClient, RottenTomatoesClient, WikidataClient}
import tools.{HttpFetch, HttpStatusException, RoutingHttpFetch}

/** Each film database family read live, from recorded answers: its title search's films (films only), and its record
 *  of a film with the facts the identity measures read and the ids that link it to other families. */
class FamilySourcesSpec extends AnyFlatSpec with Matchers {
  private def fixture(path: String) = scala.io.Source.fromResource(path)(using scala.io.Codec.UTF8).mkString

  /** IMDb's GraphQL answers, routed by the query a body asks: one URL serves them all. */
  private final class ImdbFetch extends HttpFetch {
    override def get(url: String): String = throw new HttpStatusException(404, "GET", url, None)
    override def post(url: String, body: String, contentType: String): String =
      if (body == ImdbClient.titleSearchBody("Diuna")) fixture("fixtures/imdb/title_search_diuna_films_and_series.json")
      else if (ImdbClient.identityRecordId(body).contains("tt43338336")) fixture("fixtures/imdb/identity_record_kuzma.json")
      else throw new HttpStatusException(404, "POST", url, None)
  }

  "IMDb as a family" should "find films by any language's title, never a series, and link its record by IMDb id" in {
    val imdb = new ImdbFamily(new ImdbClient(new ImdbFetch))
    imdb.titled("Diuna").map(_.id) shouldBe Seq("tt1160419", "tt15239678", "tt31378509", "tt0087182", "tt15331462", "tt22084616")
    val kuzma = imdb.record("tt43338336")
    kuzma.map(r => (r.film.title, r.film.year, r.film.directors, r.crossIds)) shouldBe
      Some(("Kuzma", Some(2026), Some(Seq("Artem Hryhorian")), Map("imdb" -> "tt43338336")))
  }

  "Filmweb as a family" should "read a film's record off its info and preview, running time and countries included" in {
    val base = "www.filmweb.pl/api/v1/film/804771"
    val filmweb = new FilmwebFamily(new FilmwebClient(new RoutingHttpFetch(Seq(
      s"$base/info"    -> fixture(s"fixtures/08-06-2026/$base/info"),
      s"$base/preview" -> fixture(s"fixtures/08-06-2026/$base/preview")))))
    filmweb.record("804771").map(r => (r.film.title, r.film.year, r.film.runtime, r.film.directors, r.film.countries, r.crossIds)) shouldBe
      Some(("Basia", Some(2016), Some(11), Some(Seq("Marcin Wasilewski", "Łukasz Kacprowicz")), Some(Seq("PL")), Map("filmweb" -> "804771")))
  }

  "Rotten Tomatoes and Metacritic as families" should "read a film's name, year, running time and directors off its page" in {
    val rt = new RottenTomatoesFamily(new RottenTomatoesClient(new RoutingHttpFetch(Seq(
      "/m/dune_2021" -> fixture("fixtures/rottentomatoes/movie_dune_2021_runtime_beside_trailer.html")))))
    rt.record("dune_2021").map(r => (r.film.title, r.film.year, r.film.runtime, r.crossIds)) shouldBe
      Some(("Dune", Some(2021), Some(155), Map("rt" -> "dune_2021")))
    val mc = new MetacriticFamily(new MetacriticClient(new RoutingHttpFetch(Seq(
      "/movie/dune/" -> fixture("fixtures/metacritic/movie_dune_1984_duration_hours_as_minutes.html")))))
    mc.record("dune").map(r => (r.film.title, r.film.year, r.film.runtime, r.film.directors, r.crossIds)) shouldBe
      Some(("Dune", Some(1984), Some(137), Some(Seq("David Lynch")), Map("metacritic" -> "dune")))
  }

  /** Wikidata's recorded answers by their exact URL — each built as a live request is (`URI.create`), so a list
   *  separator a live read could not carry fails here too. */
  private final class WikidataFetch(answers: Map[String, String]) extends HttpFetch {
    private val api = "https://www.wikidata.org/w/api.php?"
    override def get(url: String): String = {
      java.net.URI.create(url)
      answers.get(url.stripPrefix(api)).map(name => fixture(s"fixtures/wikidata/$name.json")).getOrElse(throw new HttpStatusException(404, "GET", url, None))
    }
    override def post(url: String, body: String, contentType: String): String = throw new HttpStatusException(404, "POST", url, None)
  }

  "Wikidata as a family" should "find only film items, and read one's record with its directors, countries and other databases' ids" in {
    val hits = "Q286340|Q1191033|Q3886923|Q602965|Q150948|Q7393932|Q110492062|Q141615534|Q15618295|Q6421255|Q6421261|Q6421263|Q3197896|Q1774838|Q14710913|Q6421264"
    val wiki = new WikiFamily(new WikidataClient(new WikidataFetch(Map(
      "action=wbsearchentities&search=Klondike&language=pl&uselang=pl&type=item&limit=15&format=json" -> "search_klondike_pl",
      "action=wbsearchentities&search=Klondike&language=en&uselang=en&type=item&limit=15&format=json" -> "search_klondike_en",
      s"action=wbgetentities&ids=${hits.replace("|", "%7C")}&props=claims&format=json"                 -> "claims_klondike_hits",
      "action=wbgetentities&ids=Q110492062&props=claims%7Clabels%7Caliases%7Csitelinks&format=json"    -> "item_Q110492062",
      "action=wbgetentities&ids=Q110811441&props=labels&format=json"                                   -> "labels_klondike_directors",
      "action=wbgetentities&ids=Q212&props=claims&format=json"                                         -> "claims_country_Q212",
      "action=wbgetentities&ids=Q43&props=claims&format=json"                                          -> "claims_country_Q43"))), "pl")
    // the 2022 and 1932 films; a TV miniseries with an IMDb id, the gold rush, the river and a person do not pass
    wiki.titled("Klondike").map(_.id) shouldBe Seq("Q110492062", "Q6421261")
    wiki.record("Q110492062").map(r => (r.film.originalTitle, r.film.year, r.film.runtime, r.film.directors, r.film.countries, r.crossIds)) shouldBe
      Some((Some("Клондайк"), Some(2022), Some(100), Some(Seq("Maryna Er Gorbach")), Some(Seq("UA", "TR")),
        Map("wikidata" -> "Q110492062", "imdb" -> "tt16315948", "tmdb" -> "913760")))
  }
}
