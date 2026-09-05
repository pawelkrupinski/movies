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

  // Letters NFD leaves alone because they are distinct letters, not accented
  // bases: an ASCII filter then simply deletes them. "Fatih Akın" folded to
  // "fatih ak" and read as a different person from "Fatih Akin".
  it should "fold letters NFD does not decompose" in {
    CinemaCorroboration.contradicts(row(Seq("Fatih Akin"),      Seq("Fatih Akın")))      shouldBe false
    CinemaCorroboration.contradicts(row(Seq("Joachim Trier"),   Seq("Joachim Trıer")))   shouldBe false
  }

  // Transliteration: one long surname spelled two ways ("Tarkowski" / "Tarkovsky")
  // is the same director, and two long surnames two edits apart are hardly ever
  // different people. Short tokens keep the tighter bound.
  it should "not read a transliterated surname as a different director" in {
    CinemaCorroboration.contradicts(row(Seq("Andrei Tarkowski"), Seq("Andreï Tarkovsky"))) shouldBe false
  }

  // A familiar form with the same surname and initial is the same credit:
  // "Tom Donnelly" for "Thomas Michael Donnelly", "Dave" for "David G.".
  it should "not read a familiar first name as a different director" in {
    CinemaCorroboration.contradicts(row(Seq("Thomas Michael Donnelly"), Seq("Tom Donnelly"))) shouldBe false
    CinemaCorroboration.contradicts(row(Seq("David G. Derrick Jr."),    Seq("Dave Derrick Jr."))) shouldBe false
  }

  // A hyphenated surname split one way by TMDB and another by the venue —
  // "Amrou Al-Kadhi" against "Amrou Alkadhi" — is one name written two ways, and
  // token-wise it looks like an extra word. Compared whole, it is identical.
  it should "not read a differently-hyphenated surname as a different director" in {
    CinemaCorroboration.contradicts(row(Seq("Amrou Al-Kadhi"), Seq("Amrou Alkadhi"))) shouldBe false
  }

  // Two more shapes the thresholds were just too tight for: a five-letter surname
  // one edit apart ("Mitić" / "Mitik") and a seven-letter one transliterated
  // through two different languages ("Sokurow" from German, "Sokourov" from
  // French). Both keep their first name, which is what makes the looser bound safe.
  it should "not read a short surname's single-letter variant as a different director" in {
    CinemaCorroboration.contradicts(row(Seq("Kosara Mitik"), Seq("Kosara Mitić"))) shouldBe false
  }

  it should "not read a doubly-transliterated surname as a different director" in {
    CinemaCorroboration.contradicts(
      row(Seq("Alexander Nikolajewitsch Sokurow"), Seq("Alexandre Sokourov"))) shouldBe false
  }

  it should "not read a four-letter surname's single-letter variant as different" in {
    CinemaCorroboration.contradicts(row(Seq("Christian Nyby"), Seq("Christian Niby"))) shouldBe false
  }

  // The venue spells a Tamil name as one word and adds a given name TMDB omits:
  // "Mathi Maran" against "Pugazhendhi Mathimaran". Written out, one name contains
  // the other.
  it should "not read a joined name carrying an extra given name as different" in {
    CinemaCorroboration.contradicts(row(Seq("Mathi Maran"), Seq("Pugazhendhi Mathimaran"))) shouldBe false
  }

  it should "keep a shared first name from merging two directors" in {
    CinemaCorroboration.contradicts(row(Seq("Andrzej Wajda"), Seq("Andrzej Żuławski"))) shouldBe true
  }

  // Names that are the same person and share no letters: a pseudonym, or one name
  // romanised from two different dialects. No comparison of the strings reaches
  // these, so they are listed. Small and closed by nature — each entry is a fact
  // about one director, not a rule.
  it should "treat a known pseudonym as the person behind it" in {
    CinemaCorroboration.contradicts(row(Seq("Loriot"), Seq("Vicco von Bülow"))) shouldBe false
    CinemaCorroboration.contradicts(row(Seq("Anthony M. Dawson"), Seq("Antonio Margheriti"))) shouldBe false
  }

  it should "treat two romanisations of one name as one director" in {
    // Cantonese and Mandarin readings of the same characters.
    CinemaCorroboration.contradicts(row(Seq("Lau Kar-leung"), Seq("Liu Chia-Liang"))) shouldBe false
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
      row(Seq("Someone Else"), Seq("Another Person"), filmRuntime = Some(15), cinemaRuntime = Some(180))) shouldBe true
  }

  it should "not read a short film in a longer slot as the wrong film" in {
    // Prod, 2026-09-05: seven of the nine runtime contradictions had the SAME
    // director on both sides. Almodovar's 30-minute "The Human Voice" advertised
    // at 90 with a Q&A; a Chaplin/Keaton shorts programme against one Keaton
    // short. The film is right and the slot is longer than it.
    CinemaCorroboration.contradicts(
      row(Seq("Pedro Almodovar"), Seq("Pedro Almodovar"), filmRuntime = Some(30), cinemaRuntime = Some(90))) shouldBe false
  }

  it should "keep the runtime signal when only one side names a director" in {
    // An agreement needs two names. A film no venue credits has not agreed with
    // anything, so the runtime still speaks.
    CinemaCorroboration.contradicts(
      row(Seq("Tadeusz Makarczynski"), Seq.empty, filmRuntime = Some(15), cinemaRuntime = Some(180))) shouldBe true
  }
}
