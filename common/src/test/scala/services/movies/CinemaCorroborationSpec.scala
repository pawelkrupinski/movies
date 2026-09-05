package services.movies

import models.{KinoApollo, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.resolution.TmdbBasis

/** Prod, on the day the contradiction sweep went live: 191 of 202 flagged rows were
 *  correctly resolved, and the "disagreement" was a naming convention. TMDB writes
 *  Hungarian and Japanese credits surname-first — "Enyedi Ildikó", "Szabó István",
 *  "Pálfi György" — where the cinemas write them given-name-first, and a venue may
 *  print a middle name TMDB omits. Comparing the folded string made every one of
 *  those a contradiction, so the sweep would have force-re-resolved ~190 correct
 *  films — churn, and a re-resolution can land somewhere worse than where it started.
 */
class CinemaCorroborationSpec extends AnyFlatSpec with Matchers {

  private def row(filmDirectors: Seq[String], cinemaDirectors: Seq[String],
                  filmRuntime: Option[Int] = Some(100), cinemaRuntime: Option[Int] = Some(100)): MovieRecord =
    MovieRecord(tmdbId = Some(1), tmdbBasis = Some(TmdbBasis.DirectorWalk.toString),
      data = Map[Source, SourceData](
        Tmdb      -> SourceData(title = Some("F"), runtimeMinutes = filmRuntime, director = filmDirectors),
        KinoApollo -> SourceData(title = Some("F"), runtimeMinutes = cinemaRuntime, director = cinemaDirectors)))

  "contradicts" should "not read a surname-first credit as a different director" in {
    CinemaCorroboration.contradicts(row(Seq("Enyedi Ildikó"), Seq("Ildikó Enyedi"))) shouldBe false
    CinemaCorroboration.contradicts(row(Seq("Szabó István"),  Seq("István Szabó")))  shouldBe false
    CinemaCorroboration.contradicts(row(Seq("Pálfi György"),  Seq("György Pálfi")))  shouldBe false
  }

  it should "not read an extra middle name as a different director" in {
    CinemaCorroboration.contradicts(row(Seq("Neele Vollmar"), Seq("Neele Leana Vollmar"))) shouldBe false
  }

  it should "accept a match against ANY of the credited directors" in {
    CinemaCorroboration.contradicts(row(Seq("Jason Hand", "Dana Ledoux Miller"), Seq("Jason Hand"))) shouldBe false
  }

  // An initial standing in for the full name is the same person: TMDB credits
  // "Alejandro G. Iñárritu" where the venue writes "Alejandro González Iñárritu".
  it should "match an initial against the name it abbreviates" in {
    CinemaCorroboration.contradicts(
      row(Seq("Alejandro G. Iñárritu"), Seq("Alejandro González Iñárritu"))) shouldBe false
  }

  it should "not let a bare initial match an unrelated name" in {
    CinemaCorroboration.contradicts(row(Seq("A. Wajda"), Seq("Louisa Proske"))) shouldBe true
  }

  // Upstream feeds mangle names, and not per-venue: "Paul Verhoven" arrives
  // identically from six unrelated UK cinemas, "Michael Gottli" from five Arc
  // venues, "Pedro Almod" truncated at the accent. None is evidence of a different
  // film, and none can be fixed at source. A contradiction has to mean the names
  // are actually DIFFERENT, not merely differently mangled.
  it should "not read a truncated credit as a different director" in {
    CinemaCorroboration.contradicts(row(Seq("Michael Gottlieb"), Seq("Michael Gottli"))) shouldBe false
    CinemaCorroboration.contradicts(row(Seq("Pedro Almodóvar"),  Seq("Pedro Almod")))    shouldBe false
  }

  it should "not read a one-letter misspelling as a different director" in {
    CinemaCorroboration.contradicts(row(Seq("Paul Verhoeven"), Seq("Paul Verhoven"))) shouldBe false
  }

  it should "keep short names strict, where one letter is a different person" in {
    // "Lee" and "Loe", or "Kim" and "Kam", are not near-misses to forgive.
    CinemaCorroboration.contradicts(row(Seq("Bong Joon Ho"), Seq("Bong Joon Il"))) shouldBe true
  }

  it should "still catch two genuinely different directors" in {
    CinemaCorroboration.contradicts(row(Seq("Andrzej Wajda"), Seq("Louisa Proske"))) shouldBe true
  }

  it should "abstain when a credit folds away to nothing, as a CJK name does" in {
    // "王家衛" and "Wong Kar Wai" are the same person; nothing here can know that,
    // so the comparison must not fire either way.
    CinemaCorroboration.contradicts(row(Seq("王家衛"), Seq("Wong Kar Wai"))) shouldBe false
  }

  it should "still catch a runtime a category apart" in {
    CinemaCorroboration.contradicts(
      row(Seq("Same Person"), Seq("Same Person"), filmRuntime = Some(15), cinemaRuntime = Some(180))) shouldBe true
  }
}
