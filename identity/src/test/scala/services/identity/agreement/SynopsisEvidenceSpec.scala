package services.identity.agreement

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.ProseWords

class SynopsisEvidenceSpec extends AnyFlatSpec with Matchers {
  import ProseWords.{ofDescription, ofProse, ofWords}

  // PL Janosik "edukacja MH: Binti" (identity-unmatched capture; its bilety24 page)
  private val janosik = ofProse(Seq("Binti, nastoletnia imigrantka nielegalnie mieszkająca w Belgii, nie pamięta Konga, z którego pochodzi. " +
    "Dziewczynka marzy o karierze vlogerki. Gdy poznaje Eliasa, młodego obrońcę praw zwierząt, dostrzega szansę."))
  private val shamte = ofDescription(Seq("Four Tanzanian women are unknowingly connected through their ability to persevere extreme hardships " +
    "in the city of Dar-es-Salaam."))
  private val migom = ofDescription(Seq("Binti is a 12-year-old girl of Congolese origin who dreams of becoming a famous vlogger like her idol " +
    "Tatyana. Binti meets Elias and befriends her. She hatches the perfect plan: her dad has to marry Elias’s mom, so they can stay in Belgium."))
  private val bintiTitles = ofWords(Seq("edukacja MH: Binti", "Binti"))

  "a venue's synopsis" should "refuse the agreed film when it names a namesake's description and nothing of its own" in {
    SynopsisEvidence.veto(janosik, shamte, ofWords(Seq("Seko Shamte")), Seq(577285 -> migom), bintiTitles) shouldBe Some(577285 -> 2)
  }

  it should "refuse nothing it names of the agreed film's own description or people, nor one word of a namesake's" in {
    // the agreed film is Migom's: the synopsis names its description
    SynopsisEvidence.veto(janosik, migom, ProseWords.None, Seq(855156 -> shamte), bintiTitles) shouldBe None
    // one shared word is no namesake named
    val oneWord = ofProse(Seq("Dziewczynka mieszka w Belgii z ojcem."))
    SynopsisEvidence.veto(oneWord, shamte, ProseWords.None, Seq(577285 -> migom), bintiTitles) shouldBe None
    // PL Kino CK Lublin "Lalka": the synopsis names Kawalski's 2026 cast, and Prus, whom only the 2010 musical's overview names
    val ckLublin = ofProse(Seq("„Lalka” powraca jako wielka filmowa opowieść. Marcin Dorociński, Kamila Urzędowska i Marek Kondrat " +
      "spotykają się w ekranizacji kultowej powieści Bolesława Prusa."))
    val kawalski = ofDescription(Seq("Warszawa końca XIX wieku. Serce Stanisława Wokulskiego owładnięte jest szaloną miłością do Izabeli Łęckiej."))
    val musical  = ofDescription(Seq("„Lalka” w reżyserii Wojciecha Kościelniaka to pierwsze musicalowe odczytanie powieści Bolesława Prusa. " +
      "Premiera odbyła się w Teatrze Muzycznym w Gdyni."))
    val lalka = ofWords(Seq("Lalka"))
    SynopsisEvidence.veto(ckLublin, kawalski, ProseWords.None, Seq(1607358 -> musical), lalka) shouldBe Some(1607358 -> 2)
    SynopsisEvidence.veto(ckLublin, kawalski, ofWords(Seq("Marcin Dorociński", "Kamila Urzędowska", "Marek Kondrat", "Maciej Kawalski")),
      Seq(1607358 -> musical), lalka) shouldBe None
  }

  it should "refuse no film without a description, nor on words every title shares" in {
    SynopsisEvidence.veto(janosik, ProseWords.None, ofWords(Seq("Seko Shamte")), Seq(577285 -> migom), bintiTitles) shouldBe None
    val titled = ofProse(Seq("Film o Belgii. Opowieść o Belgii i Eliasie."))
    SynopsisEvidence.veto(titled, shamte, ProseWords.None, Seq(577285 -> migom), ofWords(Seq("Belgium Elias"))) shouldBe None
  }

  // Read per agreed take, never held: the overviews' words come off the store's document when the veto asks
  // (`IdentityLookups.overview`), and a store keeps each film's as a few hashes (`TmdbFilmRecord.OverviewWords`).
  "the veto" should "cost a few kilobytes a take, reading two descriptions' words" in {
    def once() = SynopsisEvidence.veto(janosik, ofDescription(Seq("Four Tanzanian women in Dar-es-Salaam.")), ofWords(Seq("Seko Shamte")),
      Seq(577285 -> ofDescription(Seq("Binti meets Elias and befriends her, so they can stay in Belgium."))), bintiTitles)
    once() shouldBe Some(577285 -> 2)
    val perTake = _root_.tools.ThreadAllocation.of { var i = 0; while (i < 200) { once(); i += 1 } }._2 / 200
    info(s"one veto, its two descriptions' words read: $perTake bytes; a film's words held in its store document: " +
      s"${4 * migom.size} bytes of hashes (${migom.size} words)")
    perTake should be < 16384L
  }
}
