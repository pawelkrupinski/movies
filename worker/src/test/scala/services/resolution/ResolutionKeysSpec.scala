package services.resolution

import services.movies.SingleCountryNormalizer.titleNormalizer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResolutionKeysSpec extends AnyFlatSpec with Matchers {

  "ResolutionKeys.tmdb" should "be independent of director order and duplication" in {
    val a = ResolutionKeys.tmdb("Dune", Some(2021), Seq("Denis Villeneuve", "Greig Fraser"), None, titleNormalizer)
    val b = ResolutionKeys.tmdb("Dune", Some(2021), Seq("Greig Fraser", "Denis Villeneuve", "Denis Villeneuve"), None, titleNormalizer)
    a shouldBe b
  }

  it should "be independent of title casing/diacritics/punctuation (sanitize)" in {
    ResolutionKeys.tmdb("Diabeł!", Some(2020), Nil, None, titleNormalizer) shouldBe
      ResolutionKeys.tmdb("diabel", Some(2020), Nil, None, titleNormalizer)
  }

  it should "differ when a hint differs (year, director, original title)" in {
    val base = ResolutionKeys.tmdb("Dune", Some(2021), Seq("Villeneuve"), None, titleNormalizer)
    base should not be ResolutionKeys.tmdb("Dune", Some(1984), Seq("Villeneuve"), None, titleNormalizer)
    base should not be ResolutionKeys.tmdb("Dune", Some(2021), Seq("Lynch"), None, titleNormalizer)
    base should not be ResolutionKeys.tmdb("Dune", Some(2021), Seq("Villeneuve"), Some("Dune Part One"), titleNormalizer)
  }

  it should "not collide across sources for the same title+year" in {
    val title = "Dune"; val year = Some(2021)
    val keys = Set(
      ResolutionKeys.imdb(title, year, titleNormalizer),
      ResolutionKeys.filmweb(title, year, None, Nil, titleNormalizer),
      ResolutionKeys.rt(title, None, year, titleNormalizer),
      ResolutionKeys.mc(title, None, year, titleNormalizer)
    )
    keys.size shouldBe 4
  }

  // A forced re-enrich must forget a film's memoised resolutions, or it replays
  // them instead of re-probing. The film's own title sits in a DIFFERENT field
  // per source — mc/rt lead with the ORIGINAL title and carry it in the fallback
  // field — so the match is on whole fields, not position. These are the exact
  // key shapes observed in prod for "Odyseja" (2026).
  "belongsTo" should "match every source's key for the film, wherever its title sits" in {
    val keys = Seq(
      "tmdb|odyseja|2026|christophernolan|theodyssey",
      "imdb|odyseja|2026",
      "filmweb|odyseja|2026|theodyssey|christophernolan",
      "rt|theodyssey|odyseja|2026",
      "mc|theodyssey|odyseja|2026"
    )
    all(keys.map(ResolutionKeys.belongsTo(_, "Odyseja", titleNormalizer))) shouldBe true
  }

  it should "match regardless of the year in the key — the resolver's year can differ from the row's" in {
    ResolutionKeys.belongsTo("mc|theodyssey|odyseja|2025", "Odyseja", titleNormalizer) shouldBe true
    ResolutionKeys.belongsTo("imdb|odyseja|",             "Odyseja", titleNormalizer) shouldBe true
  }

  it should "not match another film" in {
    ResolutionKeys.belongsTo("mc|thenorth|polnoc|2026", "Odyseja", titleNormalizer) shouldBe false
    ResolutionKeys.belongsTo("imdb|odysejakosmiczna|1968", "Odyseja", titleNormalizer) shouldBe false
  }

  // Whole-field, never substring: "odyseja" must not match inside a longer
  // field, or a re-enrich of one film would wipe an unrelated one's entry.
  it should "match whole fields only, not substrings" in {
    ResolutionKeys.belongsTo("mc|2001odysejakosmiczna||1968", "Odyseja", titleNormalizer) shouldBe false
  }

  it should "never match on an empty title" in {
    ResolutionKeys.belongsTo("mc|theodyssey||2026", "", titleNormalizer) shouldBe false
  }
}
