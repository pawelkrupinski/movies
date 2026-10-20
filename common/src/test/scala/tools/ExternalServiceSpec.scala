package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExternalServiceSpec extends AnyFlatSpec with Matchers {

  "ExternalService.ofUrl" should "name the third-party service every client's hosts belong to" in {
    val expected = Seq(
      "https://www.omdbapi.com/?apikey=k&i=tt1"                       -> ExternalService.Omdb,
      "https://api.themoviedb.org/3/movie/1"                          -> ExternalService.Tmdb,
      "https://image.tmdb.org/t/p/w500/x.jpg"                         -> ExternalService.Tmdb,
      "https://caching.graphql.imdb.com/"                             -> ExternalService.Imdb,
      "https://v3.sg.media-imdb.com/suggestion/x/xyz.json"            -> ExternalService.Imdb,
      "https://www.imdb.com/title/tt1/"                               -> ExternalService.Imdb,
      "https://www.rottentomatoes.com/m/dune"                         -> ExternalService.RottenTomatoes,
      "https://www.metacritic.com/movie/dune/"                        -> ExternalService.Metacritic,
      "https://www.filmweb.pl/film/Diuna-2021-1"                      -> ExternalService.Filmweb,
      "https://letterboxd.com/imdb/tt1/"                              -> ExternalService.Letterboxd,
      "https://www.wikidata.org/w/api.php?action=wbgetentities"       -> ExternalService.Wikidata,
      "https://query.wikidata.org/sparql?query=x"                     -> ExternalService.Wikidata,
      "https://v3-cinemeta.strem.io/catalog/movie/top/search=dune.json" -> ExternalService.Cinemeta,
      "https://api.telegram.org/bot123/sendMessage"                   -> ExternalService.Telegram)
    expected.foreach { case (url, service) => withClue(url) { ExternalService.ofUrl(url) shouldBe Some(service) } }
  }

  it should "name nothing for a host that is not one of them, nor for a lookalike suffix" in {
    ExternalService.ofUrl("https://www.cinema-city.pl/repertoire") shouldBe None
    ExternalService.ofUrl("https://notimdb.com/title/tt1") shouldBe None
    ExternalService.ofUrl("not a url") shouldBe None
  }

  it should "list only the services it can name, so a label grid seeded from it is the whole set" in {
    ExternalService.apis.toSet should contain allOf (ExternalService.Omdb, ExternalService.Tmdb, ExternalService.Imdb,
      ExternalService.RottenTomatoes, ExternalService.Metacritic, ExternalService.Filmweb, ExternalService.Letterboxd,
      ExternalService.Wikidata, ExternalService.Cinemeta)
    ExternalService.apis should not contain ExternalService.Telegram
  }
}
