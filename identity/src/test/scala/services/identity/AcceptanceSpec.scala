package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures.{Film, Listing, ListingFilm, Qualifiers}
import services.identity.StageRelays.Houses

/** The acceptance rules read one listing's RANKED candidates ([[Scored]]) — no resolve needed. */
class AcceptanceSpec extends AnyFlatSpec with Matchers {
  private val calibration = IdentityCalibration.resolver
  private val acceptance  = new Acceptance(calibration)
  private val houses      = Houses(Map("ntlive" -> "nationaltheatrelive"))

  /** `films` scored for `listing` as `FamilyScope.score` scores them: each at its search rank, against
   *  the rivals its title names alike, best first. */
  private def ranked(listing: Listing, films: (Int, Film, Option[Int])*): Seq[Scored] = {
    def rivalling(film: Film) = IdentityMeasures.Rivalling(IdentityMeasures.titleRelation(listing, film, houses, Qualifiers.Unknown).value)
    val close = films.count { case (_, film, _) => rivalling(film) }
    films.map { case (tmdbId, film, rank) =>
      val measures = IdentityMeasures.listingFilm(listing, film, rank, close - (if (rivalling(film)) 1 else 0), 0, houses)
      Scored(Candidate(tmdbId, film), calibration.probability(ListingFilm, measures), measures, denial = None, listing, rank,
        houseProduction = StageRelays.billsUnderItsHouse(listing, film, houses))
    }.sortBy(scored => (-scored.probability, scored.candidate.tmdbId))
  }
  private def alone(candidates: Seq[Scored]): Option[Scored.Accepted] = acceptance.aloneNamed(candidates).map(_._1)
  private def taken(candidates: Seq[Scored]): Option[Int] = alone(candidates).map(_._1.candidate.tmdbId)
  /** The rule a node's alone acceptance took `film` by, as the decision's explanation names it. */
  private def acceptedBy(ranked: Seq[Scored], film: Int): Option[String] = Acceptance.ruleTaking(acceptance.aloneNamed(ranked), film)

  // Flicks' UK and US "NT Live: The Misanthrope" (155 listings): TMDB's search for the play ranks
  // three bare records of Molière's work above the National Theatre's 2026 broadcast.
  private val misanthrope = Listing("NT Live: The Misanthrope", runtime = Some(180))
  private val bare = Seq(
    (511684, Film("The Misanthrope", year = Some(1966), runtime = Some(171)), Some(1)),
    (700001, Film("The Misanthrope", year = Some(1994), runtime = Some(97)), Some(2)),
    (700002, Film("The Misanthrope", year = Some(2011), runtime = Some(125)), Some(4)))
  private val national = (1693710, Film("National Theatre Live: The Misanthrope", year = Some(2026), runtime = Some(181)), Some(3))

  "directors-title" should "not count TMDB's hollow duplicate of a film as its director's second film" in {
    // DE Anker-Filmtheater's "Solo" [2024] {Sophie Dupuis}: her 2023 "Solo" (102′), and 1450958, an empty "Solo" record
    // of hers with no date or running time
    val solo  = Listing("Solo", year = Some(2024), runtime = Some(102), directors = Seq("Sophie Dupuis"))
    val full  = (952200, Film("Solo", year = Some(2023), runtime = Some(102), directors = Some(Seq("Sophie Dupuis"))), Some(5))
    val empty = (1450958, Film("Solo", directors = Some(Seq("Sophie Dupuis"))), None)
    acceptance.byRule("directors-title")(ranked(solo, full, empty)).map(_._1.candidate.tmdbId) shouldBe Some(952200)
    // two full records of hers stay two films
    val remake = (999, Film("Solo", year = Some(2025), runtime = Some(95), directors = Some(Seq("Sophie Dupuis"))), None)
    acceptance.byRule("directors-title")(ranked(solo, full, remake)) shouldBe None
  }

  "season-record" should "take the one season record carrying the listing's title, and leave two seasons to the season" in {
    // UK "Royal Ballet and Opera: Romeo and Juliet" ×127 is the RBO's 2026/27 record; "…: Tosca" has 2025/26 and 2026/27
    val rj = Listing("Royal Ballet and Opera: Romeo and Juliet", runtime = Some(170))
    acceptance.byRule("season-record")(ranked(rj, (1702789, Film("Royal Ballet & Opera 2026/27: Romeo and Juliet", year = Some(2027)), Some(1))))
      .map(_._1.candidate.tmdbId) shouldBe Some(1702789)
    val tosca = Listing("Royal Ballet and Opera: Tosca", runtime = Some(195))
    acceptance.byRule("season-record")(ranked(tosca, (1482356, Film("Royal Ballet & Opera 2025/26: Tosca", year = Some(2025)), None),
      (1702784, Film("Royal Ballet & Opera 2026/27: Tosca", year = Some(2027)), None))) shouldBe None
  }

  it should "not take a season record of another season than the year the listing states" in {
    // UK Aldeburgh's "The Royal Ballet: Romeo & Juliet" [2025] {Kenneth MacMillan} [170′], recording 38363428661, screening
    // the RBO's 25 May 2027 relay: TMDB's "The Royal Ballet: Romeo and Juliet (2021/22)" carries its title once the season
    // is out, and is four seasons off its year
    val rj = Listing("The Royal Ballet: Romeo & Juliet", year = Some(2025), runtime = Some(170))
    acceptance.byRule("season-record")(ranked(rj, (875143, Film("The Royal Ballet: Romeo and Juliet (2021/22)", runtime = Some(170)), Some(5)))) shouldBe None
    acceptance.byRule("season-record")(ranked(rj.copy(year = Some(2022)), (875143, Film("The Royal Ballet: Romeo and Juliet (2021/22)", runtime = Some(170)), Some(5))))
      .map(_._1.candidate.tmdbId) shouldBe Some(875143)
  }

  it should "take the record of the season its screenings broadcast it in, whatever earlier year dates its staging" in {
    // ...and screening on 25 May 2027, the day "Royal Ballet & Opera 2026/27: Romeo and Juliet" airs: its 2025 dates the
    // staging's premiere, not the broadcast; the 2021/22 record stays another season's
    val rj = Listing("The Royal Ballet: Romeo & Juliet", year = Some(2025), runtime = Some(170), screenedSeason = Some(2026))
    val season = (1702789, Film("Royal Ballet & Opera 2026/27: Romeo and Juliet", year = Some(2027)), Some(7))
    acceptance.byRule("season-record")(ranked(rj, season)).map(_._1.candidate.tmdbId) shouldBe Some(1702789)
    acceptance.byRule("season-record")(ranked(rj, (875143, Film("The Royal Ballet: Romeo and Juliet (2021/22)", runtime = Some(170)), Some(5)))) shouldBe None
  }

  it should "take the season its screenings broadcast it in when two seasons' records carry its title" in {
    // UK recording 38342304766: ~30 venues bill the RBO's Swan Lake relay of 22 April 2027 with no season or year
    // ("Royal Ballet & Opera: Swan Lake", Pocklington Arts Centre); TMDB holds the house's 2024/25 and 2026/27
    // records of it, so each venue's listing was left unidentified as a film of its own.
    val swanLake = Listing("Royal Ballet & Opera: Swan Lake", broadcastSeason = Some(2026), screenedSeason = Some(2026))
    val seasons  = Seq((1300034, Film("Royal Ballet & Opera 2024/25: Swan Lake", year = Some(2025)), Some(1)),
      (1702782, Film("Royal Ballet & Opera 2026/27: Swan Lake", year = Some(2027)), Some(2)))
    acceptance.byRule("season-record")(ranked(swanLake, seasons *)).map(_._1.candidate.tmdbId) shouldBe Some(1702782)
    // screening in neither season leaves the two to nobody
    acceptance.byRule("season-record")(ranked(swanLake.copy(broadcastSeason = Some(2028), screenedSeason = Some(2028)), seasons *)) shouldBe None
  }

  "a relay billing its house and no season" should "take its house's record of the season its screenings broadcast it in" in {
    // Prod UK 2026-10-13: Filmhouse Newlyn's "MET: Macbeth" screening 17 October 2026 took the Met's 2014 staging by
    // house-production, beside its 2026/27 record; Kendal's "Romeo & Juliet - The Royal Ballet" (May 2027) took 2013's.
    val macbeth     = Listing("MET: Macbeth", broadcastSeason = Some(2026), screenedSeason = Some(2026))
    val staging2014 = (295526, Film("The Metropolitan Opera - Verdi: Macbeth", year = Some(2014), runtime = Some(169)), Some(1))
    val season2026  = (1703622, Film("The Metropolitan Opera 2026/27: Macbeth", year = Some(2026)), Some(2))
    taken(ranked(macbeth, staging2014, season2026)) shouldBe Some(1703622)
    // a record of another season than its screenings' is no staging it screens, however its house bills the work
    acceptance.byRule("season-record")(ranked(macbeth.copy(broadcastSeason = Some(2027), screenedSeason = Some(2027)), staging2014, season2026)) shouldBe None
    // nor is another house's: "MET" names no Royal Ballet & Opera
    acceptance.byRule("season-record")(ranked(macbeth, (1, Film("Royal Ballet & Opera 2026/27: Macbeth", year = Some(2027)), Some(1)))) shouldBe None

    val romeo = Listing("Romeo & Juliet - The Royal Ballet", broadcastSeason = Some(2026), screenedSeason = Some(2026))
    taken(ranked(romeo, (476204, Film("Romeo and Juliet (Royal Ballet)", year = Some(2013), runtime = Some(168)), Some(1)),
      (1451897, Film("The Royal Ballet: Romeo and Juliet", year = Some(2025), runtime = Some(175)), Some(3)),
      (1702789, Film("Royal Ballet & Opera 2026/27: Romeo and Juliet", year = Some(2027)), Some(2)))) shouldBe Some(1702789)
  }

  it should "take the season record of its work when its title dates it by a bare year" in {
    // Prod UK 2026-10-13: The Beehive Honiton's "RBO: Tosca 27" and "RBO: Swan Lake 27", and Chichester's "Macbeth (Met
    // Opera)", each left its season's record, TMDB's first hit, below the threshold.
    taken(ranked(Listing("RBO: Tosca 27", broadcastSeason = Some(2026), screenedSeason = Some(2026)),
      (1702784, Film("Royal Ballet & Opera 2026/27: Tosca", year = Some(2027)), Some(1)),
      (1482356, Film("Royal Ballet & Opera 2025/26: Tosca", year = Some(2025), runtime = Some(195)), Some(2)))) shouldBe Some(1702784)
    taken(ranked(Listing("RBO: Swan Lake 27", broadcastSeason = Some(2026), screenedSeason = Some(2026)),
      (1702782, Film("Royal Ballet & Opera 2026/27: Swan Lake", year = Some(2027)), Some(1)),
      (1300034, Film("Royal Ballet & Opera 2024/25: Swan Lake", year = Some(2025), runtime = Some(210)), Some(2)))) shouldBe Some(1702782)
    taken(ranked(Listing("Macbeth (Met Opera)", broadcastSeason = Some(2026), screenedSeason = Some(2026)),
      (1703622, Film("The Metropolitan Opera 2026/27: Macbeth", year = Some(2026)), Some(1)))) shouldBe Some(1703622)
  }

  it should "never take another house's season record, nor one of another work its title also names" in {
    // Identity measure on recording 38363428661: DE "PARIS OPERA: ROMEO UND JULIA" ×12 took the Royal Ballet's 2026/27
    // record by the word "opera", UK "La Fanciulla Del West - Royal Opera" the Met's, and ES "Manon de Jules Massenet -
    // Ópera (Cine)" ×14 MacMillan's ballet — "Manon" names Massenet's opera too, and "Ópera" no house.
    taken(ranked(Listing("PARIS OPERA: ROMEO UND JULIA", broadcastSeason = Some(2026), screenedSeason = Some(2026)),
      (1702789, Film("RBO 2026/27: Romeo und Julia (The Royal Ballet)", year = Some(2027)), Some(1)))) shouldBe None
    taken(ranked(Listing("La Fanciulla Del West - Royal Opera", broadcastSeason = Some(2026), screenedSeason = Some(2026)),
      (1703629, Film("The Metropolitan Opera 2026/27: La Fanciulla del West", year = Some(2027)), Some(1)))) shouldBe None
    taken(ranked(Listing("Manon de Jules Massenet - Ópera (Cine)", broadcastSeason = Some(2026), screenedSeason = Some(2026)),
      (1702757, Film("Royal Ballet & Opera 2026/27: Manon", year = Some(2026)), Some(1)))) shouldBe None
    // a banner naming no house, over the one work it names, still takes its season's record
    taken(ranked(Listing("OPERA-PARSIFAL", broadcastSeason = Some(2026), screenedSeason = Some(2026)),
      (1703636, Film("The Metropolitan Opera 2026/27: Parsifal", year = Some(2027)), Some(1)))) shouldBe Some(1703636)
  }

  it should "take its season's record over another staging of the work, by the house's word or the year its title dates" in {
    // Prod PL 2026-10-13: Kino Powiśle's "OPERA-SAMSON I DALILA" left the Met's 2026/27 record, its first hit, below the
    // threshold; DE recording 38351832400: "RBO live 2027: Tosca" took the ROH's 2011 Tosca by IMDb's suggestion, and
    // Cinema Stuttgart's "Metropolitan Opera: Macbeth (Verdi)" the Met's 2014 staging.
    val samson1949 = (29993, Film("Samson and Delilah", year = Some(1949), runtime = Some(134)), Some(2))
    taken(ranked(Listing("OPERA-SAMSON I DALILA", broadcastSeason = Some(2026), screenedSeason = Some(2026)),
      (1703624, Film("The Metropolitan Opera 2026/27: Samson et Dalila", year = Some(2026)), Some(1)), samson1949)) shouldBe Some(1703624)
    taken(ranked(Listing("RBO live 2027: Tosca"), (181442, Film("Royal Opera House: Tosca", year = Some(2011), runtime = Some(128)), Some(1)),
      (1702784, Film("Royal Ballet & Opera 2026/27: Tosca", year = Some(2027)), Some(2)))) shouldBe Some(1702784)
    taken(ranked(Listing("Metropolitan Opera: Macbeth (Verdi)", broadcastSeason = Some(2026), screenedSeason = Some(2026)),
      (295526, Film("The Metropolitan Opera - Verdi: Macbeth", year = Some(2014), runtime = Some(169)), Some(1)),
      (1703622, Film("The Metropolitan Opera 2026/27: Macbeth", year = Some(2026)), Some(2)))) shouldBe Some(1703622)
  }

  it should "read its work and its house however a box office spells them" in {
    // Prod UK 2026-10-13: each venue's spelling, unread, named no work to search with its broadcast season ("NoCandidate"),
    // or no house the season's record bills
    val romeo   = (1702789, Film("Royal Ballet & Opera 2026/27: Romeo and Juliet", year = Some(2027)), Some(1))
    val macbeth = (1703622, Film("The Metropolitan Opera 2026/27: Macbeth", year = Some(2026)), Some(1))
    val billed  = Seq(
      "RB&O Romeo & Juliet (12 TBC) 170 mins" -> romeo, // Electric Palace Bridport
      "RB&O 26/27: Romeo & Juliet"            -> romeo, // Lonsdale Alhambra Penrith
      "RBO present Romeo and Juliet 27"        -> romeo, // The Flavel
      "ROYAL BALLET & OPERA LIVE_ ROMEO AND JULIET" -> romeo,
      "RBO/MET presents MACBETH"               -> macbeth, // The Flavel
      "Live from the Met: Macbeth"             -> macbeth, // Dundee Contemporary Arts
      "The Metropolitan Opera: Macbeth [New Production" -> macbeth, // The Maltings Berwick
      "The Met: Macbeth 26"                    -> macbeth, // The Beehive Honiton
      "Met Opera in Association with RBO: Macbeth (12A)" -> macbeth) // Theatr Mwldan Cardigan
    billed.filterNot { case (title, record) =>
      StageRelays.stageWorksBilled(Seq(title), seasonNamed = false).nonEmpty &&
        taken(ranked(Listing(title, broadcastSeason = Some(2026), screenedSeason = Some(2026)), record)).contains(record._1)
    }.map(_._1) shouldBe empty
  }

  "directors-work" should "leave two seasons of one staging, both billing the work, to the season" in {
    // UK "Royal Ballet and Opera: Tosca" {Oliver Mears} [195′] ×123 screens in May 2027 — the 2026/27 record, which
    // credits nobody yet; the 2025/26 one is his with the running time. Billed alike, the rule takes neither.
    val tosca   = Listing("Royal Ballet and Opera: Tosca", runtime = Some(195), directors = Seq("Oliver Mears"))
    val season1 = (1482356, Film("Royal Ballet & Opera 2025/26: Tosca", year = Some(2025), runtime = Some(195), directors = Some(Seq("Oliver Mears"))), None)
    val season2 = (1702784, Film("Royal Ballet & Opera 2026/27: Tosca", year = Some(2027)), None)
    acceptance.byRule("directors-work")(ranked(tosca, season1, season2)) shouldBe None
    // the one record billing the work alone is his staging, and taken
    acceptance.byRule("directors-work")(ranked(tosca, season1)).map(_._1.candidate.tmdbId) shouldBe Some(1482356)
  }

  "a listing crediting a director" should "take the one record its title names exactly by that director, however the database ranks it" in {
    // US "Man of Iron" {Andrzej Wajda}: TMDB's search ranks Wajda's 1981 film seventh, behind "Iron Man".
    val listing = Listing("Man of Iron", directors = Seq("Andrzej Wajda"), runtime = Some(153))
    val wajda   = (225, Film("Man of Iron", year = Some(1981), runtime = Some(144), directors = Some(Seq("Andrzej Wajda"))), Some(7))
    val favreau = (1726, Film("Iron Man", year = Some(2008), runtime = Some(126), directors = Some(Seq("Jon Favreau")), popularity = Some(40.0)), Some(1))
    taken(ranked(listing, favreau, wajda)) shouldBe Some(225)
  }

  it should "take the one record by that director its published ORIGINAL title names exactly, when that title stands apart from the billing" in {
    // PL Kinoteka's "Podróżnik", published as Kiarostami's "Mossafer": the work's own title in another language
    val listing = Listing("Podróżnik", originalTitle = Some("Mossafer"), directors = Seq("Abbas Kiarostami"))
    val his     = (54001, Film("The Traveler", originalTitle = Some("Mossafer"), year = Some(1974), runtime = Some(73),
      directors = Some(Seq("Abbas Kiarostami"))), Some(9))
    val first   = (68543, Film("Podróż na Księżyc", year = Some(2011), runtime = Some(100), directors = Some(Seq("Someone Else")), popularity = Some(3.0)), Some(1))
    acceptance.byRule("directors-title")(ranked(listing, first, his)).map(_._1.candidate.tmdbId) shouldBe Some(54001)
    // US Landmark's "Legend (1986)", published as "Legend": the billing itself, its year aside
    val legend = Listing("Legend (1986)", originalTitle = Some("Legend"), directors = Seq("Ridley Scott"))
    val scott  = (11976, Film("Legend", year = Some(1985), runtime = Some(94), directors = Some(Seq("Ridley Scott"))), Some(3))
    acceptance.byRule("directors-title")(ranked(legend, scott)).map(_._1.candidate.tmdbId) shouldBe Some(11976)
    // DE Goli's "The Piano Tuner (Dokumentarfilm)", published as "The Piano Tuner" {David Doucette}: a decorated piece
    val goli     = Listing("The Piano Tuner (Dokumentarfilm)", originalTitle = Some("The Piano Tuner"), directors = Seq("David Doucette"))
    val doucette = (1571662, Film("The Piano Tuner", year = Some(2025), runtime = Some(128), directors = Some(Seq("David Doucette"))), Some(5))
    acceptance.byRule("directors-title")(ranked(goli, doucette)).map(_._1.candidate.tmdbId) shouldBe Some(1571662)
    // the original title naming only the director's longer title is no exact naming
    val longer = (54001, Film("The Traveler's Return", originalTitle = Some("Mossafer 2"), directors = Some(Seq("Abbas Kiarostami"))), Some(9))
    acceptance.byRule("directors-title")(ranked(listing, first, longer)) shouldBe None
  }

  it should "take nothing by a published original title that is a part or a near-spelling of the billing, or beside a record the billing names" in {
    // DE kinoheld's event tickets (run 38578038277), each published with the original title and director of the record a
    // search for the billing hit — the "facts" are that record's, so they cannot confirm it: a word of the billing
    // ("Tino Bomelino", a comedian, as Tino {Anna Lucía Alcalá Pérez}; "Ping Pong Session" as Ping Pong {Yu Tianji}),
    // a near-spelling of it ("Ganes", the band, as Ganges {Talía Carolina Osorio}) …
    val tino = (1020336, Film("Tino", year = Some(2022), runtime = Some(4), directors = Some(Seq("Anna Lucía Alcalá Pérez"))), Some(1))
    acceptance.byRule("directors-title")(ranked(Listing("Tino Bomelino", originalTitle = Some("Tino"), directors = Seq("Anna Lucía Alcalá Pérez")), tino)) shouldBe None
    val yu = (1559695, Film("Ping Pong", year = Some(2025), runtime = Some(16), directors = Some(Seq("Yu Tianji"))), Some(18))
    acceptance.byRule("directors-title")(ranked(Listing("Ping Pong Session", originalTitle = Some("Ping Pong"), directors = Seq("Yu Tianji")), yu)) shouldBe None
    def hit(billed: String, original: String, director: String) = acceptance.byRule("directors-title")(ranked(
      Listing(billed, originalTitle = Some(original), directors = Seq(director)), (7, Film(original, year = Some(2025), directors = Some(Seq(director))), Some(5))))
    hit("Punkabend #93", "'93", "Mike Wood") shouldBe None
    hit("Christmas Time Machine", "Time Machine", "Zihan Gao") shouldBe None
    hit("DAS PACK", "Pack", "Peiqi Peng") shouldBe None
    hit("Dancing Queens & Band „live and kicking“ – A Tribute to ABBA", "Live Forever", "Connor Withall") shouldBe None
    val ganges = (608716, Film("Ganges", year = Some(2019), runtime = Some(75), directors = Some(Seq("Talía Carolina Osorio"))), Some(1))
    acceptance.byRule("directors-title")(ranked(Listing("Ganes", originalTitle = Some("Ganges"), directors = Seq("Talía Carolina Osorio")), ganges)) shouldBe None
    // … or beside the record the billed title names exactly: Fontane Klub's "Ich bin Dein" runs Iram Haq's 100 minutes,
    // published with Claudia Bille Stræde's credit from another "Jeg er din"
    val haq     = (215028, Film("Ich bin Dein", originalTitle = Some("Jeg er din"), year = Some(2013), runtime = Some(100),
      directors = Some(Seq("Iram Haq"))), Some(1))
    val straede = (1366306, Film("Jeg er din", year = Some(2014), directors = Some(Seq("Claudia Bille Stræde"))), Some(2))
    acceptance.byRule("directors-title")(ranked(Listing("Ich bin Dein", originalTitle = Some("Jeg er din"), runtime = Some(100),
      directors = Seq("Claudia Bille Stræde")), haq, straede)) shouldBe None
  }

  it should "take nothing when its runtime contradicts that record, or two such records are candidates" in {
    val listing = Listing("Man of Iron", directors = Seq("Andrzej Wajda"), runtime = Some(200))
    val wajda   = (225, Film("Man of Iron", year = Some(1981), runtime = Some(144), directors = Some(Seq("Andrzej Wajda"))), Some(7))
    taken(ranked(listing, wajda)) shouldBe None
    val cut     = (226, Film("Man of Iron", year = Some(1982), runtime = Some(195), directors = Some(Seq("Andrzej Wajda"))), Some(8))
    taken(ranked(listing.copy(runtime = Some(170)), wajda, cut)) shouldBe None
  }

  "a programme listing whose title carries a film's whole title" should "take that film when it is the piece's top hit and nothing else is named" in {
    // PL "DZIEŃ KINA POLSKIEGO: Przepraszam, czy tu biją": the banner turned the exact top hit into a segment,
    // which no rule took, and one venue's evidence stayed at 28.9%.
    val day   = Listing("DZIEŃ KINA POLSKIEGO: Przepraszam, czy tu biją")
    val film  = (318545, Film("Przepraszam, czy tu biją?", year = Some(1976)), Some(1))
    def segment(l: Listing, films: (Int, Film, Option[Int])*) = acceptance.byRule("segment-top-hit")(ranked(l, films *)).map(_._1.candidate.tmdbId)
    segment(day, film) shouldBe Some(318545)
    // credited as the exact top hit it is once the banner is off, never less than its own probability
    acceptance.byRule("segment-top-hit")(ranked(day, film)).map(_._2).get should be >= 0.99
    // not its search's FIRST hit: no answer
    segment(day, film.copy(_3 = Some(2)), (9, Film("Inny film"), Some(1))) shouldBe None
    // a programme naming two films: neither ("Akademia Polskiego Filmu: Ostatni etap | Majdanek - cmentarz Europy"; a
    // one-word piece names no film here — see "Inna Mamusia - maraton" below)
    segment(Listing("Akademia: Ostatni etap | Majdanek - cmentarz Europy"), (121141, Film("Ostatni etap", year = Some(1948)), Some(1)),
      (121142, Film("Majdanek - cmentarz Europy", year = Some(1944)), Some(2))) shouldBe None
    // a double bill: neither
    segment(Listing("Basia. Humor w paski mam + Kocia Szajka"), (1747514, Film("Basia. Humor w paski mam"), Some(1))) shouldBe None
    // the guards: a one-word piece, a title year another than the record's, a numbered set
    segment(Listing("Bhutan - Trails of Happiness"), (5, Film("Bhutan", year = Some(1928)), Some(1))) shouldBe None
    segment(Listing("Toddler Club: Disney Junior Cinema Club 2026"), (6, Film("Disney Junior Cinema Club", year = Some(2024)), Some(1))) shouldBe None
    segment(Listing("Bolek i Lolek - zestaw IV"), (7, Film("Bolek i Lolek", year = Some(1936)), Some(1))) shouldBe None
    // a one-word piece names no rival: "Inna Mamusia - maraton" is "Inna mamusia", not one of TMDB's "Maraton"s
    segment(Listing("Inna Mamusia - maraton"), (1400837, Film("Inna mamusia", year = Some(2026)), Some(1)),
      (493305, Film("Maraton", year = Some(2006)), Some(1))) shouldBe Some(1400837)
    // a whole title is the exact top hit's, not this rule's
    segment(Listing("Przepraszam, czy tu biją?"), film) shouldBe None
  }

  /** `scored` as IMDb's suggestions for the listing's own title placed them: `(tmdbId, place)` among `of`. A film
   *  only IMDb reached, under a title the listing does not carry, is one only the IMDb rule may take. */
  private def suggested(scored: Seq[Scored], of: Int, places: (Int, Int)*): Seq[Scored] = scored.map { sc =>
    places.toMap.get(sc.candidate.tmdbId).fold(sc)(place => sc.copy(imdb = Some(Scored.ImdbPlace(place, of)),
      suggestedOnly = sc.rank.isEmpty && !sc.category("title").exists(IdentityMeasures.Rivalling)))
  }
  private def byImdb(scored: Seq[Scored]): Option[Int] = acceptance.byRule("imdb-suggested")(scored).map(_._1.candidate.tmdbId)

  "a film IMDb suggests for the listing's title" should "be taken on the old pipeline's rungs: its director, its year, or the only suggestion the title names" in {
    // PL "Superfutrzak i złośliwa wiewiórka" {Joona Tena}: TMDB holds the film only under its Finnish title; IMDb
    // suggests just it (tt35166699, test/resources/fixtures/imdb/suggestion_superfutrzak.json).
    val futrzak = Listing("Superfutrzak i złośliwa wiewiórka", directors = Seq("Joona Tena"))
    val finnish = (1273554, Film("Supermarsu ja suuri huijaus", year = Some(2025), directors = Some(Seq("Joona Tena"))), None)
    taken(ranked(futrzak, finnish)) shouldBe None
    val placed  = suggested(ranked(futrzak, finnish), 1, 1273554 -> 1)
    acceptedBy(placed, 1273554) shouldBe Some("imdb-suggested")
    // the director rung, among several suggestions
    val other = (9, Film("Inny", year = Some(2020), directors = Some(Seq("Ktoś Inny"))), None)
    byImdb(suggested(ranked(futrzak, finnish, other), 2, 9 -> 1, 1273554 -> 2)) shouldBe Some(1273554)
    // the year rung: IMDb's first suggestion, in the year the listing states — "Camino dla opornych" is Compostelle
    val camino      = Listing("Filmowe Rekolekcje: Camino dla opornych", year = Some(2026))
    val compostelle = (1404604, Film("Compostelle", year = Some(2026)), None)
    byImdb(suggested(ranked(camino, compostelle), 3, 1404604 -> 1)) shouldBe Some(1404604)
    byImdb(suggested(ranked(camino.copy(year = Some(2024)), compostelle), 3, 1404604 -> 1)) shouldBe None
    byImdb(suggested(ranked(camino, compostelle), 3, 1404604 -> 2)) shouldBe None
    // the sole rung: the one film IMDb suggests, when the title names it
    val hope = Listing("Witajcie w Hope PREMIERA 2D napisy")
    val film = (1058424, Film("Witajcie w Hope", year = Some(2026)), None)
    byImdb(suggested(ranked(hope, film), 1, 1058424 -> 1)) shouldBe Some(1058424)
    byImdb(suggested(ranked(hope, film), 2, 1058424 -> 1)) shouldBe None
  }

  it should "take IMDb's first suggestion, the only suggested film the title is, when TMDB ranks no film it names above it" in {
    // PL "Kroll": TMDB ranks Machulski's 1991 film first and Krõll (1972) second; IMDb's only movie suggestion is the
    // 1991 film, beside other suggestions — the old pipeline's yearless rung.
    val kroll  = Listing("Kroll")
    val y1991  = (36394, Film("Kroll", year = Some(1991)), Some(1))
    val y1972  = (563252, Film("Krõll", year = Some(1972)), Some(2))
    val others = (58221, Film("Nick Kroll: Thank You Very Cool", year = Some(2011)), Some(4))
    byImdb(suggested(ranked(kroll, y1991, y1972, others), 3, 36394 -> 1, 58221 -> 2)) shouldBe Some(36394)
    // ranked below a film the title names, it is no fallback
    byImdb(suggested(ranked(kroll, y1991.copy(_3 = Some(2)), y1972.copy(_3 = Some(1)), others), 3, 36394 -> 1, 58221 -> 2)) shouldBe None
    // two suggested films the title is: no answer
    byImdb(suggested(ranked(kroll, y1991, y1972, others), 3, 36394 -> 1, 563252 -> 2)) shouldBe None
    // a title only an ALTERNATIVE of the suggested film carries is not the film's: "Lumière" is not "Café Lumière"
    val cafe = (52512, Film("Café Lumière", year = Some(2004), alternativeTitles = Seq("Lumière")), Some(1))
    byImdb(suggested(ranked(Listing("Lumière"), cafe, others), 3, 52512 -> 1, 58221 -> 2)) shouldBe None
  }

  it should "be a fallback: not past a film TMDB's own search names, another instalment, or a double bill" in {
    // UK "BTS 'ARIRANG' IN SÃO PAULO: LIVE VIEWING" [2026]: IMDb's first 2026 suggestion is the Busan concert.
    val bts     = Listing("BTS 'ARIRANG' IN SÃO PAULO: LIVE VIEWING", year = Some(2026))
    val saoPaulo = (1700001, Film("BTS 'Arirang' in São Paulo: Live Viewing", year = Some(2026)), Some(1))
    val busan    = (1700002, Film("BTS WORLD TOUR [ARIRANG] in Busan", year = Some(2026)), None)
    byImdb(suggested(ranked(bts, saoPaulo, busan), 2, 1700002 -> 1)) shouldBe None
    // "Recepta na szczęście 2" is not the first film
    byImdb(suggested(ranked(Listing("Recepta na szczęście 2"), (1, Film("Recepta na szczęście", year = Some(2008)), None)), 1, 1 -> 1)) shouldBe None
    // a double bill is neither film
    byImdb(suggested(ranked(Listing("Basia. Humor w paski mam + Kocia Szajka"), (2, Film("Basia. Humor w paski mam"), None)), 1, 2 -> 1)) shouldBe None
    // and a film only IMDb's other-language match reached is the IMDb rule's alone, however it scores
    acceptedBy(suggested(ranked(Listing("Kuźma", year = Some(2026)), (3, Film("Кузьма: Страшно веселий", year = Some(2026)), None)), 2, 3 -> 1)
      .map(_.copy(probability = 0.99)), 3) shouldBe Some("imdb-suggested")
  }

  "the only film a listing's own title search returns" should "be taken unless a fact contradicts it, as the old pipeline took it" in {
    // PL Kino Bajka's "Loving Karma" [78′]: TMDB's search returns one film, its 85-minute record; seven minutes
    // weighed against it, so neither the exact top hit nor any other rule took it.
    val karma  = Listing("Loving Karma", runtime = Some(78))
    val record = (1563699, Film("Loving Karma", year = Some(2026), runtime = Some(85)), Some(1))
    def sole(scored: Seq[Scored], id: Int) = scored.map(sc => if (sc.candidate.tmdbId == id) sc.copy(soleResult = true) else sc)
    def bySole(scored: Seq[Scored]) = acceptance.byRule("sole-result")(scored).map(_._1.candidate.tmdbId)
    bySole(ranked(karma, record)) shouldBe None
    bySole(sole(ranked(karma, record), 1563699)) shouldBe Some(1563699)
    // a runtime that contradicts it, a one-word piece, another film the title names, a double bill: no
    bySole(sole(ranked(karma.copy(runtime = Some(150)), record), 1563699)) shouldBe None
    bySole(sole(ranked(Listing("Bhutan - Trails of Happiness"), (5, Film("Bhutan", year = Some(1928)), Some(1))), 5)) shouldBe None
    bySole(sole(ranked(karma, record, (7, Film("Loving Karma", year = Some(1990)), None)), 1563699)) shouldBe None
    bySole(sole(ranked(Listing("Loving Karma + Kocia Szajka"), record), 1563699)) shouldBe None
    // a whole title every word of which the film's title carries — two words at least, or its main title — as the old
    // pipeline took a whole title's only result: PL "Dzień Dziecka księdza Kaczkowskiego", "TAFITI"
    bySole(sole(ranked(Listing("Dzień Dziecka księdza Kaczkowskiego"), (1707378, Film("Dzień Dziecka księdza Jana Kaczkowskiego", year = Some(2026)), Some(1))), 1707378)) shouldBe Some(1707378)
    bySole(sole(ranked(Listing("TAFITI"), (1437198, Film("Tafiti - Ab durch die Wüste", year = Some(2025)), Some(1))), 1437198)) shouldBe Some(1437198)
    // a word that is no main title of the film's is not it
    bySole(sole(ranked(Listing("Wüste"), (1437198, Film("Tafiti - Ab durch die Wüste", year = Some(2025)), Some(1))), 1437198)) shouldBe None
  }

  it should "not take it on words scattered over a longer title, covering half its words or fewer" in {
    // the wrong-match hunt, 10-14: an event a venue bills in two or three words, its search's only result a longer title
    // that happens to hold them — DE "Simply Christmas" (a band's Advent concert), "80ER PARTY", "Anime Nights" (six
    // nights of different films), "DAS GEHEIME FILMARCHIV" (a secret-classic series), "Bodies in Translation" (a stage show)
    def sole(l: String, film: String) =
      acceptance.byRule("sole-result")(ranked(Listing(l), (9, Film(film, year = Some(2022)), Some(1))).map(_.copy(soleResult = true))).map(_._1.candidate.tmdbId)
    sole("Simply Christmas", "Steve's Simply Happy Christmas Cook-A-Long") shouldBe None
    sole("80ER PARTY", "Die Stärksten 80er Clips – Saturn-Party Edition") shouldBe None
    sole("Anime Nights", "Crunchyroll Anime Nights - Mob Psycho 100") shouldBe None
    sole("DAS GEHEIME FILMARCHIV", "Das geheime Filmarchiv der Eva Braun") shouldBe None
    sole("Bodies in Translation", "Purple, Bodies in Translation – Part II of A Yellow Memory from the Yellow Age") shouldBe None
    // more than half its words, or every part of it the billing touches whole: still the film
    sole("Dzień Dziecka księdza Kaczkowskiego", "Dzień Dziecka księdza Jana Kaczkowskiego") shouldBe Some(9)
    sole("Manon (BALLET LIVE)", "BALLET LIVE. MANON. ROYAL ÓPERA HOUSE") shouldBe Some(9)
  }

  "a programme dating a one-word film's title" should "take that film by its year" in {
    // PL Kino NCKF EC1's "Akademia Kina Polskiego: Drogówka (2012)": "Drogówka" alone is many films' title, but not
    // beside the year that dates it — Smarzowski's 2013 film, a year off.
    val dated = Listing("Akademia Kina Polskiego: Drogówka (2012)")
    val film  = (167179, Film("Drogówka", year = Some(2013)), Some(1))
    val other = (900001, Film("Drogówka", year = Some(1978)), Some(2))
    acceptedBy(ranked(dated, film, other), 167179) shouldBe Some("dated-title")
    // undated, the one word names no film on its own
    acceptedBy(ranked(Listing("Akademia Kina Polskiego: Drogówka"), film, other), 167179) should not be Some("dated-title")
  }

  "a double programme of two whole works" should "take neither film, whatever its facts back" in {
    // UK "We're Going on a Bear Hunt + The Tiger Who Came to Tea" {Joanna Harrison, Robin Shaw} ×133: each piece found
    // its own film first and both directors were credited — the two scored 91–93%, and popularity picked one.
    val bill  = Listing("We're Going on a Bear Hunt + The Tiger Who Came to Tea", directors = Seq("Joanna Harrison", "Robin Shaw"))
    val bear  = (431591, Film("We're Going on a Bear Hunt", year = Some(2016), directors = Some(Seq("Joanna Harrison"))), Some(1))
    val tiger = (644120, Film("The Tiger Who Came to Tea", year = Some(2019), directors = Some(Seq("Robin Shaw"))), Some(1))
    // as the corpus scored them: 101–134 venues corroborating each lifted both to ~92%
    def corroborated(scored: Seq[Scored]) = scored.map { sc =>
      val tiger = sc.candidate.tmdbId == 644120
      sc.copy(probability = if (tiger) 0.925 else 0.913,
        measures = sc.measures + ("venues.corroborating" -> IdentityMeasures.Number(if (tiger) 101 else 134)))
    }
      .sortBy(sc => (-sc.probability, sc.candidate.tmdbId))
    acceptance.billsSeveralWorks(corroborated(ranked(bill, bear, tiger))) shouldBe true
    taken(corroborated(ranked(bill, bear, tiger))) shouldBe None
    acceptance.pooled(corroborated(ranked(bill, bear, tiger))) shouldBe None
    // its facts picking ONE of the two leave it a double programme: neither film (user rule)
    acceptance.billsSeveralWorks(ranked(bill.copy(directors = Seq("Joanna Harrison")), bear, tiger)) shouldBe true
    // a single film is no bill
    acceptance.billsSeveralWorks(ranked(Listing("We're Going on a Bear Hunt"), bear)) shouldBe false
    // a talk after the plus is no second work: "La Perra | BEST FILM on Tour | POKAZ FILMU + SPOTKANIE"
    val perra = (1550622, Film("La Perra", year = Some(2026)), Some(1))
    val talk  = (1195735, Film("Spotkanie", year = Some(1949)), Some(1))
    acceptance.billsSeveralWorks(ranked(Listing("La Perra | BEST FILM on Tour | POKAZ FILMU + SPOTKANIE"), perra, talk)) shouldBe false
  }

  "a listing dating its title" should "take the one record its title names exactly from that year, however the database ranks it" in {
    // US "Troll (1986)": TMDB ranks "Troll 2" and the 2022 "Troll" above the 1986 film.
    val troll = Listing("Troll (1986)")
    val films = Seq((1180831, Film("Troll 2", year = Some(2025), popularity = Some(20.0)), Some(1)),
      (736526, Film("Troll", year = Some(2022), popularity = Some(10.0)), Some(2)), (33061, Film("Troll", year = Some(1986)), Some(3)))
    taken(ranked(troll, films *)) shouldBe Some(33061)
    // Two records of that title from that year are no answer.
    taken(ranked(troll, films :+ (33062, Film("Troll", year = Some(1986)), Some(4)) *)) shouldBe None
    // Nor is the record whose director the listing's credit contradicts: two UK venues credit
    // "Dracula (1931)" to Karl Freund, its cinematographer, not Tod Browning.
    val dracula = Listing("Dracula (1931)", directors = Seq("Karl Freund"))
    taken(ranked(dracula, (138, Film("Dracula", year = Some(1931), directors = Some(Seq("Tod Browning"))), Some(3)))) shouldBe None
  }

  it should "take the one record its title only decorates, years aside, from the year it dates it" in {
    // US "The Metropolitan Opera: La Fanciulla del West Encore (2027)" is the Met's 2026/27 broadcast;
    // its 2018 staging is nine years off.
    val encore = Listing("The Metropolitan Opera: La Fanciulla del West Encore (2027)")
    val staging2018 = (543704, Film("The Metropolitan Opera: La Fanciulla del West", year = Some(2018)), Some(1))
    val season      = (1703629, Film("The Metropolitan Opera 2026/27: La Fanciulla del West", year = Some(2027)), Some(2))
    taken(ranked(encore, staging2018, season)) shouldBe Some(1703629)
    // A title that is only a word of the listing's is not decorated by it.
    taken(ranked(Listing("Fanciulla Encore (2027)"), (1, Film("Encore", year = Some(2027)), Some(1)))) shouldBe None
  }

  "a listing billing its work under a house" should "take the one record billing the work under that house over bare records of the work" in {
    taken(ranked(misanthrope, bare :+ national *)) shouldBe Some(1693710)
  }

  it should "take nothing when two records bill the work under its house" in {
    val encore = (1693711, Film("National Theatre Live: The Misanthrope", year = Some(2027), runtime = Some(181)), Some(5))
    taken(ranked(misanthrope, bare :+ national :+ encore *)) shouldBe None
  }

  it should "take nothing when another record carries the house record's very title, whatever its flags" in {
    // US "NT Live: Hamlet" (204 minutes): TMDB holds the National Theatre's 2010 and 2015 Hamlets
    // under one title; only the 2010 record read as billed under the house.
    val listing = Listing("NT Live: Hamlet", runtime = Some(204))
    val older   = (436484, Film("National Theatre Live: Hamlet", year = Some(2010)), Some(1))
    val later   = (396227, Film("National Theatre Live: Hamlet", year = Some(2015), runtime = Some(217)), Some(2))
    val candidates = ranked(listing, older, later).map(scored =>
      if (scored.candidate.tmdbId == 396227) scored.copy(houseProduction = false) else scored)
    acceptance.byRule("house-production")(candidates) shouldBe None
  }

  it should "take nothing when its runtime contradicts the house's record" in {
    val cut = national.copy(_2 = national._2.copy(runtime = Some(120)))
    taken(ranked(misanthrope, bare :+ cut *)) shouldBe None
  }

  it should "not take its house's record over a candidate the title names that the listing's facts fit at least as well" in {
    // Everyman's "Cellar Door x ThoughtBubble presents: Terminator 2: Judgment Day" publishes nothing but
    // its title; its banner, learned as TMDB's "The Making of" house, took the making-of documentary
    // at 3.4% over Cameron's film at 54% and split it from the film's other listings.
    val listing = Listing("Cellar Door x ThoughtBubble presents: Terminator 2: Judgment Day")
    val makingOf = Film("The Making of 'Terminator 2: Judgment Day'", year = Some(1991))
    val learned = StageRelays.billing(listing, makingOf).map(billed => Houses(Map(billed.listingHouse -> billed.filmHouse)))
    learned should not be empty
    val film = Film("Terminator 2: Judgment Day", year = Some(1991), runtime = Some(137))
    def scored(tmdbId: Int, candidate: Film, rank: Int) = {
      val measures = IdentityMeasures.listingFilm(listing, candidate, Some(rank), 0, 0, learned.get)
      Scored(Candidate(tmdbId, candidate), calibration.probability(ListingFilm, measures), measures, denial = None, listing, Some(rank),
        houseProduction = StageRelays.billsUnderItsHouse(listing, candidate, learned.get))
    }
    val candidates = Seq(scored(280, film, 1), scored(473793, makingOf, 2)).sortBy(candidate => -candidate.probability)
    candidates.find(_.candidate.tmdbId == 473793).map(_.houseProduction) shouldBe Some(true)
    acceptance.byRule("house-production")(candidates) shouldBe None
  }

  it should "not take another house's record of the work" in {
    val rsc = (1693712, Film("RSC Live: The Misanthrope", year = Some(2026), runtime = Some(181)), Some(3))
    taken(ranked(misanthrope, bare :+ rsc *)) shouldBe None
  }

  "every own match" should "name the rule that took it" in {
    // PL Kinoteka's "Basia. Humor w paski mam + Kocia Szajka…" was an own match at 38.2%, far under
    // the calibrated cut, and nothing said which rule took it.
    val manOfIron = Listing("Man of Iron", directors = Seq("Andrzej Wajda"), runtime = Some(153))
    val wajda     = (225, Film("Man of Iron", year = Some(1981), runtime = Some(144), directors = Some(Seq("Andrzej Wajda"))), Some(7))
    val favreau   = (1726, Film("Iron Man", year = Some(2008), runtime = Some(126), directors = Some(Seq("Jon Favreau")), popularity = Some(40.0)), Some(1))
    acceptedBy(ranked(manOfIron, favreau, wajda), 225) shouldBe Some("directors-title")
    val troll = Seq((1180831, Film("Troll 2", year = Some(2025), popularity = Some(20.0)), Some(1)),
      (736526, Film("Troll", year = Some(2022), popularity = Some(10.0)), Some(2)), (33061, Film("Troll", year = Some(1986)), Some(3)))
    acceptedBy(ranked(Listing("Troll (1986)"), troll *), 33061) shouldBe Some("dated-title")
    acceptedBy(ranked(misanthrope, bare :+ national *), 1693710) shouldBe Some("house-production")
    // never a record from another year than the title dates: US "MetOpera: Carmen (2009)" ×513 took the Met's 2024
    // Carmen this way, though Eyre's 2009 staging is another record
    val dated = Listing("NT Live: The Misanthrope (2009)", runtime = Some(180))
    acceptance.byRule("house-production")(ranked(dated, bare :+ national *)) shouldBe None
    acceptance.refusals(ranked(dated, bare :+ national *)).find(_.rule == "house-production").map(_.why) shouldBe
      Some("its house's record is from another year than its title dates")
    // A film no rule takes names none.
    acceptedBy(ranked(misanthrope, bare :+ national *), 511684) shouldBe None
  }

  "a listing whose title a record contains" should "not take a series sibling its title only overlaps, on facts that record leaves missing" in {
    // ES "BTS World Tour 'ARIRANG' In Buenos Aires: Live" ×82 took the Busan concert: TMDB's
    // Buenos Aires record states no runtime and credits no director, so the Busan record's matching
    // credit and 195 minutes out-weighed a record that merely lacks them.
    val listing     = Listing("BTS World Tour 'ARIRANG' In Buenos Aires: Live", year = Some(2026), runtime = Some(195), directors = Seq("Jungjae Ha"))
    val buenosAires = (1770237, Film("BTS World Tour 'Arirang'  in Buenos Aires: Live Viewing", year = Some(2026)), Some(1))
    val busan       = (1701849, Film("BTS WORLD TOUR [ARIRANG] in Busan", year = Some(2026), runtime = Some(195), directors = Some(Seq("Ha Jung-jae"))), Some(2))
    taken(ranked(listing, buenosAires, busan)) should not be Some(1701849)
    // Nor pooled: the 82 listings are one node, which then took Busan by its pooled vote.
    acceptance.pooled(ranked(listing, buenosAires, busan)).map(_._1.candidate.tmdbId) should not be Some(1701849)
  }

  it should "take the one record, TMDB's first, that bills the listing's work under another subtitle" in {
    // DE ×140 and ES ×82 "BTS World Tour 'ARIRANG' In Buenos Aires: Live" are TMDB's "…: Live Viewing",
    // its own search's first hit, which the title only measures as a fragment of ("Live" / "Live Viewing").
    val listing     = Listing("BTS World Tour 'ARIRANG' In Buenos Aires: Live", year = Some(2026), runtime = Some(195), directors = Seq("Jungjae Ha"))
    val buenosAires = (1770237, Film("BTS World Tour 'Arirang'  in Buenos Aires: Live Viewing", year = Some(2026)), Some(1))
    val busan       = (1701849, Film("BTS WORLD TOUR [ARIRANG] in Busan", year = Some(2026), runtime = Some(195), directors = Some(Seq("Ha Jung-jae"))), None)
    acceptedBy(ranked(listing, buenosAires, busan), 1770237) shouldBe Some("sole-work")
    // Two records billing that work are no answer, whichever TMDB ranks first.
    val encore = (1770238, Film("BTS World Tour 'Arirang' in Buenos Aires: Encore", year = Some(2026)), Some(2))
    taken(ranked(listing, buenosAires, encore, busan)) shouldBe None
  }

  it should "still take a record its title names by its original title" in {
    // PL "Following" at Kino Amondo is Nolan's "Śledząc" (original title "Following"), not a
    // same-titled record whose title merely contains it.
    val listing = Listing("Following", directors = Seq("Christopher Nolan"))
    val nolan   = (11660, Film("Śledząc", originalTitle = Some("Following"), year = Some(1999), runtime = Some(69), directors = Some(Seq("Christopher Nolan")), popularity = Some(8.0)), Some(1))
    val other   = (900001, Film("Following", year = Some(2024)), Some(2))
    taken(ranked(listing, nolan, other)) shouldBe Some(11660)
  }

  it should "still take the record naming it all over one a piece of its title names" in {
    // UK "English National Ballet: The Sleeping Beauty" ×22 is ENB's production, not the record a
    // segment of its title names.
    val listing = Listing("English National Ballet: The Sleeping Beauty", year = Some(2026), runtime = Some(150), directors = Seq("Kenneth MacMillan"))
    val enb     = (1500001, Film("English National Ballet presents The Sleeping Beauty", year = Some(2026), runtime = Some(150),
      directors = Some(Seq("Kenneth MacMillan"))), Some(2))
    val segment = (1500002, Film("The Sleeping Beauty"), Some(3))
    taken(ranked(listing, enb, segment)) shouldBe Some(1500001)
  }

  it should "not take a series sibling over the record one of its title's works names whole" in {
    // PL Kinoteka "Basia. Humor w paski mam + Kocia Szajka. Tajemnica zniknięcia śledzi": the double
    // bill names TMDB's "Basia. Humor w paski mam" whole, a record stating no director or runtime,
    // and took "Basia. Radzę sobie!" on the series director and a 53-minute runtime alone.
    val listing = Listing("Basia. Humor w paski mam + Kocia Szajka. Tajemnica zniknięcia śledzi", runtime = Some(53),
      directors = Seq("Marcin Wasilewski", "Marek Lachowicz", "Piotr Szczepanowicz"))
    val humor   = (1747514, Film("Basia. Humor w paski mam"), Some(1))
    val radze   = (1370603, Film("Basia. Radzę sobie!", year = Some(2025), runtime = Some(53),
      directors = Some(Seq("Marcin Wasilewski", "Łukasz Kacprowicz", "Ignas Meilūnas"))), None)
    taken(ranked(listing, humor, radze)) should not be Some(1370603)
  }

  "a no-match's lean" should "name its best film at 1.25 times the runner-up's probability, and none nearer than that" in {
    // the search sweep (measure/identity-film-search): a lean at 1.25× completes agreements 2× left without one
    val listing = Listing("Bohater")
    def scored(id: Int, p: Double) = Scored(Candidate(id, Film("Bohater", year = Some(2000 + id % 100))), p, Map.empty, denial = None, listing, Some(1))
    acceptance.leaning(Seq(scored(672208, 0.30), scored(425751, 0.20))).map(_.candidate.tmdbId) shouldBe Some(672208)
    acceptance.leaning(Seq(scored(672208, 0.30), scored(425751, 0.25))) shouldBe None
  }

  "the release veto's rivals" should "count no fewer than none, however many namesakes it took away" in {
    // the `rivals` measure counts the namesakes ranked against the film; the veto may take away more than it counts
    val listing = Listing("Diabły")
    val scored  = Scored(Candidate(1, Film("Diabły", year = Some(1971))), 0.5, Map("rivals" -> IdentityMeasures.Number(1)), denial = None,
      listing, Some(1), vetoedRivals = 3)
    ReleaseVeto.unvetoed(scored, scored.measures)("rivals") shouldBe IdentityMeasures.Number(0)
    ReleaseVeto.unvetoed(scored.copy(vetoedRivals = 1), scored.measures)("rivals") shouldBe IdentityMeasures.Number(0)
    ReleaseVeto.unvetoed(scored.copy(measures = Map("rivals" -> IdentityMeasures.Number(4))), Map("rivals" -> IdentityMeasures.Number(4)))("rivals") shouldBe
      IdentityMeasures.Number(1)
  }

  "a calibrated take" should "not stand beside a film only IMDb reached that the evidence weighs higher" in {
    // PL Kino Kryterium's "DKF: Ktoś całkiem obcy", recording 38363428661: TMDB's Polish title of Foley's 2007 "Perfect
    // Stranger" (7183) took it at 46.3% while Andersen's "I Was a Stranger" (1193128), which TMDB holds under its Arabic
    // title and IMDb's suggestion alone reaches, scored 47.8% on another venue's listing of it — the film Kryterium shows.
    val listing  = Listing("Ktoś całkiem obcy")
    val stranger = Film("Ktoś całkiem obcy", year = Some(2007), runtime = Some(109))
    val measures = IdentityMeasures.listingFilm(listing, stranger, Some(1), 0, 0, Houses.Unknown)
    val foley    = Scored(Candidate(7183, stranger), 0.463, measures, denial = None, listing, Some(1))
    val andersen = Scored(Candidate(1193128, Film("قضية الغرباء", year = Some(2025), runtime = Some(103))), 0.478,
      measures + ("venues.corroborating" -> IdentityMeasures.Number(1)), denial = None, listing, None, suggestedOnly = true)
    alone(Seq(andersen, foley)) shouldBe None
    // one it outscores, or one no other venue's listing corroborates, is no reason to decline
    alone(Seq(foley, andersen.copy(probability = 0.2))).map(_._1.candidate.tmdbId) shouldBe Some(7183)
    alone(Seq(andersen.copy(measures = measures), foley)).map(_._1.candidate.tmdbId) shouldBe Some(7183)
  }

  "no rule" should "take a film the title numbers as another instalment, alone or pooled" in {
    // PL Cinema City ×6 and UK Odeon ×4, convergence 2026-10-13: the 2026 final took TMDB's record of the 2025 one, its
    // only search hit, by its pooled vote.
    val worlds26 = Listing("League of Legends Worlds 26 | Finals in Cinema", year = Some(2026), runtime = Some(420))
    val worlds25 = (1552059, Film("League of Legends Worlds25 - Finals in Cinema", year = Some(2025)), Some(1))
    // the pooled scoring rated it 40.5%, past the calibration's cut, on the venues billing it
    def pooledAt(scored: Seq[Scored]) = scored.map(_.copy(probability = 0.405))
    taken(pooledAt(ranked(worlds26, worlds25))) shouldBe None
    acceptance.pooled(pooledAt(ranked(worlds26, worlds25))) shouldBe None
    // its own instalment is taken
    val worlds26Record = (1, Film("League of Legends Worlds 26 - Finals in Cinema", year = Some(2026)), Some(1))
    acceptance.pooled(pooledAt(ranked(worlds26, worlds26Record))).map(_._1.candidate.tmdbId) shouldBe Some(1)
  }
}
