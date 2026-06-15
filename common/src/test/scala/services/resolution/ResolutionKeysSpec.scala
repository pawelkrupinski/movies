package services.resolution

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResolutionKeysSpec extends AnyFlatSpec with Matchers {

  "ResolutionKeys.tmdb" should "be independent of director order and duplication" in {
    val a = ResolutionKeys.tmdb("Dune", Some(2021), Seq("Denis Villeneuve", "Greig Fraser"), None)
    val b = ResolutionKeys.tmdb("Dune", Some(2021), Seq("Greig Fraser", "Denis Villeneuve", "Denis Villeneuve"), None)
    a shouldBe b
  }

  it should "be independent of title casing/diacritics/punctuation (sanitize)" in {
    ResolutionKeys.tmdb("Diabeł!", Some(2020), Nil, None) shouldBe
      ResolutionKeys.tmdb("diabel", Some(2020), Nil, None)
  }

  it should "differ when a hint differs (year, director, original title)" in {
    val base = ResolutionKeys.tmdb("Dune", Some(2021), Seq("Villeneuve"), None)
    base should not be ResolutionKeys.tmdb("Dune", Some(1984), Seq("Villeneuve"), None)
    base should not be ResolutionKeys.tmdb("Dune", Some(2021), Seq("Lynch"), None)
    base should not be ResolutionKeys.tmdb("Dune", Some(2021), Seq("Villeneuve"), Some("Dune Part One"))
  }

  it should "not collide across sources for the same title+year" in {
    val title = "Dune"; val year = Some(2021)
    val keys = Set(
      ResolutionKeys.imdb(title, year),
      ResolutionKeys.filmweb(title, year, None, Nil),
      ResolutionKeys.rt(title, None, year),
      ResolutionKeys.mc(title, None, year)
    )
    keys.size shouldBe 4
  }
}
