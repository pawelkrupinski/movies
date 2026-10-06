package services.review

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FilmRefSpec extends AnyFlatSpec with Matchers {

  "a pasted film link" should "parse into the ref labels.tsv files it under" in {
    val cases = Seq(
      "https://www.themoviedb.org/movie/1703622-rbo-cinema-season-2026-27-macbeth?language=pl" -> "tmdb:1703622",
      "themoviedb.org/movie/291289"                                                        -> "tmdb:291289",
      "https://www.imdb.com/title/tt4020156/?ref_=fn_al_tt_1"                             -> "imdb:tt4020156",
      "https://m.imdb.com/pl/title/tt0058385/"                                              -> "imdb:tt0058385",
      "https://www.filmweb.pl/film/Franz+Kafka-2025-10008278"                               -> "filmweb:10008278",
      "https://www.filmweb.pl/film/Franz+Kafka-2025-10008278/discussion"                    -> "filmweb:10008278",
      "https://www.wikidata.org/wiki/Q101245192"                                            -> "wikidata:Q101245192",
      "https://www.rottentomatoes.com/m/1018413-scrooge"                                    -> "rt:1018413-scrooge",
      "https://letterboxd.com/film/13-souls/"                                               -> "letterboxd:13-souls",
      "https://www.metacritic.com/movie/a-prayer-for-the-dying/"                            -> "metacritic:a-prayer-for-the-dying",
      "tt0381487"                                                                           -> "imdb:tt0381487",
      "Q141180912"                                                                          -> "wikidata:Q141180912",
      "  71183 "                                                                            -> "tmdb:71183",
      "tmdb:122"                                                                            -> "tmdb:122",
      "filmweb:10006285"                                                                    -> "filmweb:10006285",
    )
    cases.foreach { case (input, ref) => withClue(input) { FilmRef.parse(input).map(_.render) shouldBe Some(ref) } }
  }

  it should "refuse what names no film rather than guess" in {
    Seq("", "hello", "https://www.themoviedb.org/tv/1399-game-of-thrones", "https://www.imdb.com/name/nm0000233/",
      "https://example.com/film/123", "tmdb:abc", "imdb:123", "netflix:123")
      .foreach(input => withClue(input) { FilmRef.parse(input) shouldBe None })
  }

  "a ref" should "link to its film's page" in {
    FilmRef("tmdb", "122").url shouldBe Some("https://www.themoviedb.org/movie/122")
    FilmRef("imdb", "tt0167260").url shouldBe Some("https://www.imdb.com/title/tt0167260/")
    FilmRef.tmdb(122).tmdb shouldBe Some(122)
    FilmRef("imdb", "tt0167260").tmdb shouldBe None
  }
}
