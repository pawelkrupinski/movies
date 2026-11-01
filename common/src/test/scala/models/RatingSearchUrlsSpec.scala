package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The English-language rating sites are searched by a Latin-script title: PL "Killhouse"
 *  (TMDB 1493123) has the Ukrainian original "Кіллхаус", which finds nothing on Metacritic. */
class RatingSearchUrlsSpec extends AnyFlatSpec with Matchers {

  "isLatinScript" should "tell Latin-script titles from Cyrillic and CJK ones" in {
    RatingSearchUrls.isLatinScript("Killhouse") shouldBe true
    RatingSearchUrls.isLatinScript("Część druga: Diuna!") shouldBe true
    RatingSearchUrls.isLatinScript("1917") shouldBe true
    RatingSearchUrls.isLatinScript("Кіллхаус") shouldBe false
    RatingSearchUrls.isLatinScript("千と千尋の神隠し") shouldBe false
  }

  "a Cyrillic original" should "search Metacritic and Rotten Tomatoes by TMDB's English title" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Killhouse"), originalTitle = Some("Кіллхаус"), englishTitle = Some("Killhouse")),
      Imdb -> SourceData(originalTitle = Some("Кіллхаус"))))
    record.metacriticHref("Killhouse PL") shouldBe "https://www.metacritic.com/search/Killhouse/?category=2"
    record.rottenTomatoesHref("Killhouse PL") shouldBe "https://www.rottentomatoes.com/search?search=Killhouse"
  }

  it should "fall back to a romanised original, then the shown title, when TMDB has no English title" in {
    val romanised = MovieRecord(data = Map[Source, SourceData](
      Tmdb -> SourceData(originalTitle = Some("Кіллхаус")), Imdb -> SourceData(originalTitle = Some("Killkhaus"))))
    romanised.metacriticHref("Zabójczy dom") shouldBe "https://www.metacritic.com/search/Killkhaus/?category=2"
    val shown = MovieRecord(data = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Кіллхаус"))))
    shown.metacriticHref("Killhouse") shouldBe "https://www.metacritic.com/search/Killhouse/?category=2"
  }

  "a Latin-script original" should "stay the search title, as before" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Tmdb -> SourceData(originalTitle = Some("Le Comte de Monte-Cristo"), englishTitle = Some("The Count of Monte Cristo"))))
    record.metacriticHref("Hrabia Monte Christo") shouldBe
      RatingSearchUrls.metacritic("Le Comte de Monte-Cristo")
  }

  "Filmweb" should "keep searching by the original, whatever its script" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Tmdb -> SourceData(originalTitle = Some("Кіллхаус"), englishTitle = Some("Killhouse"))))
    record.filmwebHref("Killhouse") shouldBe RatingSearchUrls.filmweb("Кіллхаус")
  }
}
