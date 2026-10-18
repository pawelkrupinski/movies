package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProseWordsSpec extends AnyFlatSpec with Matchers {

  "a venue's synopsis" should "read the words it capitalises past a sentence's first, by their stems, across languages and inflection" in {
    // PL Janosik's "edukacja MH: Binti" (bilety24) against Migom's 2019 film's English overview
    val synopsis = ProseWords.ofProse(Seq("Binti, nastoletnia imigrantka nielegalnie mieszkająca w Belgii, nie pamięta Konga. Gdy poznaje Eliasa, " +
      "młodego obrońcę praw zwierząt, dostrzega szansę."))
    val overview = ProseWords.ofDescription(Seq("Binti is a 12-year-old girl of Congolese origin. Binti meets Elias and befriends her, " +
      "so they can stay in Belgium."))
    (synopsis & overview) shouldBe ProseWords.ofWords(Seq("Belgium Elias"))
    // a sentence's first word is capitalised whatever it is: the synopsis reads neither "Binti" nor "Gdy"
    (synopsis & ProseWords.ofWords(Seq("Binti Gdy"))) shouldBe ProseWords.None
    // the description reads them
    (overview & ProseWords.ofWords(Seq("Binti"))).size shouldBe 1
  }

  it should "fold a stem's accents and case, and read no word of fewer than three letters" in {
    ProseWords.ofProse(Seq("Spotkanie z Łucją i Józefem.")) shouldBe ProseWords.ofWords(Seq("lucja jozef"))
    ProseWords.ofProse(Seq("We met Al and I there.")) shouldBe ProseWords.None
  }

  it should "round-trip through its hashes" in {
    val words = ProseWords.ofDescription(Seq("Four Tanzanian women in Dar-es-Salaam."))
    ProseWords.ofHashes(words.hashes) shouldBe words
    words.size shouldBe 4
    (words -- ProseWords.ofWords(Seq("Four"))).size shouldBe 3
  }

  // Held per listing, as its venue names are (CastEvidenceSpec): a handful of hashes, read off its row once at intake.
  "a listing's synopsis words" should "be read off its row once, held as a few hashes, outside its equality, and none of a feed catalogue's" in {
    val synopsis = "Binti, nastoletnia imigrantka nielegalnie mieszkająca w Belgii, nie pamięta Konga, z którego pochodzi. Dziewczynka marzy " +
      "o karierze vlogerki, bo chce „istnieć, a nie tylko żyć”. Gdy poznaje Eliasa, młodego obrońcę praw zwierząt, dostrzega dla siebie szansę."
    val cm = models.CinemaMovie(models.Movie("Binti"), models.KinoMuza, None, Some("https://kinomuza.pl/film/binti"), Some(synopsis), Nil, Nil, Nil)
    val normalizer = services.movies.SingleCountryNormalizer.titleNormalizer
    val listing = Listing.of(models.KinoMuza, cm, normalizer)
    listing.words shouldBe ProseWords.ofWords(Seq("Belgii Konga Eliasa"))
    listing shouldBe Listing.of(models.KinoMuza, cm.copy(synopsis = None), normalizer)
    Listing.movedOutsideEquality(Listing.of(models.KinoMuza, cm.copy(synopsis = None), normalizer), listing) shouldBe true
    Listing.of(models.KinoMuza, cm.copy(externalIds = Map("webedia" -> "1")), normalizer).words shouldBe ProseWords.None
    def perListing(row: models.CinemaMovie) = {
      Listing.of(models.KinoMuza, row, normalizer)
      tools.ThreadAllocation.of { var i = 0; while (i < 200) { Listing.of(models.KinoMuza, row, normalizer); i += 1 } }._2 / 200
    }
    val without = perListing(cm.copy(synopsis = None))
    val withText = perListing(cm)
    info(s"Listing.of: $without bytes without its synopsis, $withText with (+${withText - without}, names and words); " +
      s"words held: ${16 + 4 * listing.words.size + 16} bytes")
  }
}
