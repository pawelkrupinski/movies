package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The cast tie-break on the real cases its offline measure met (2026-10-06, 244 labelled clusters: 24 decided, 24 right):
 * of namesakes, the venue's own text naming two whole names or more of ONE candidate's top-billed TMDB cast, no other's twice.
 * Casts are TMDB's own, as recorded for the measure.
 */
class CastEvidenceSpec extends AnyFlatSpec with Matchers {

  // TMDB 1321666, Kawalski's 2026 "Lalka", and 81315, Has's 1968 one — both "Lalka" to a Polish venue
  private val doll2026 = Seq("Marcin Dorociński", "Kamila Urzędowska", "Marek Kondrat", "Andrzej Seweryn", "Krystyna Janda", "Maria Dębska",
    "Mateusz Damięcki", "Agata Kulesza", "Karolina Gruszka", "Maja Ostaszewska")
  private val doll1968 = Seq("Mariusz Dmochowski", "Beata Tyszkiewicz", "Tadeusz Fijewski", "Jadwiga Halina Gallowa", "Wiesław Gołas",
    "Kalina Jędrusik", "Jan Koecher", "Jan Kreczmar", "Tadeusz Kondrat", "Halina Kwiatkowska")
  // Kino CK Lublin's own page of its "Lalka" (ck-lublin.bilety24.pl/wydarzenie/?id=162455)
  private val ckLublin = "Miłość, która nie zna granic i ambicja, która nie zna ceny. „Lalka” powraca jako wielka filmowa opowieść o " +
    "pragnieniach, rozczarowaniach i wyborach, które definiują ludzkie życie. Marcin Dorociński, Kamila Urzędowska i Marek Kondrat spotykają " +
    "się na wielkim ekranie w najbardziej wyczekiwanej premierze roku – epickiej ekranizacji kultowej powieści Bolesława Prusa."

  "a venue's text" should "name the 2026 'Lalka' by three of its cast, and not Has's — whose Tadeusz Kondrat shares only a surname" in {
    val venue = VenueNames.of(Seq(ckLublin))
    CastEvidence.tieBreak(venue, Seq(1321666 -> Some(doll2026), 81315 -> Some(doll1968))) shouldBe
      Some(1321666 -> Seq("Marcin Dorociński", "Kamila Urzędowska", "Marek Kondrat"))
  }

  it should "match whole names across case and accents, never a surname alone" in {
    val venue = VenueNames.of(Seq("W rolach głównych: MARCIN DOROCINSKI oraz kamila urzędowska, a także Kondrat."))
    venue.names("Marcin Dorociński") shouldBe true
    // a run of lower-case words is prose, not a name, in a synopsis
    venue.names("Kamila Urzędowska") shouldBe false
    venue.names("Marek Kondrat") shouldBe false
    venue.names("Kondrat") shouldBe false
    // a venue's cast field holds names, whatever their case
    VenueNames.of(Nil, cast = Seq("kamila urzędowska")).names("Kamila Urzędowska") shouldBe true
  }

  it should "not join two names a comma or a bracket keeps apart into a third" in {
    val venue = VenueNames.of(Seq("Kalina Jędrusik (Maria Dębska) zachwyca"))
    venue.names("Kalina Jędrusik") shouldBe true
    venue.names("Jędrusik Maria") shouldBe false
    VenueNames.of(Seq("Kondrat, Marcin Dorociński")).names("Kondrat Marcin") shouldBe false
  }

  it should "carry a name's hyphened part in lower case: 'Na Hong-jin', 'Bong Joon-ho'" in {
    // ES "Hope" (2026-11-02): a synopsis naming Na Hong-jin read the run as "Na Hong" and named nobody
    val venue = VenueNames.of(Seq("Lo nuevo de Na Hong-jin, tras Bong Joon-ho y otros."))
    venue.names("Na Hong-jin") shouldBe true
    venue.names("Bong Joon-ho") shouldBe true
    // prose after a hyphen outside a run of capitals stays prose
    VenueNames.of(Seq("una historia anti-bélica de amor")).isEmpty shouldBe true
  }

  "one name of a candidate's cast" should "break no tie: US 'MetOpera: Medea (2022–23)' names Radvanovsky, who sings another production's Medea" in {
    // flicks.us's text of the Met's 2022–23 Medea (TMDB 1737985, no cast on TMDB); TMDB 1601989 is the 2025 Naples production
    val venue = VenueNames.of(Seq("Having triumphed at the Met in some of the repertory’s fiercest soprano roles, Sondra Radvanovsky stars as the " +
      "mythic sorceress who will stop at nothing in her quest for vengeance. Joining Radvanovsky in the Met-premiere production of Cherubini’s " +
      "rarely performed masterpiece is tenor Matthew Polenzani as Medea’s Argonaut husband, Giasone; soprano Janai Brugger as her rival."),
      cast = Seq("Matthew Polenzani", "Sondra Radvanovsky", "Ekaterina Gubanova", "Michele Pertusi", "Janai Brugger"))
    val naples = Seq("Sondra Radvanovsky", "Francesco Demuro", "Giorgi Manoshvili", "Désirée Giove", "Anita Rachvelishvili")
    CastEvidence.tieBreak(venue, Seq(1601989 -> Some(naples), 1737985 -> Some(Nil))) shouldBe None
  }

  "a double bill naming two films' casts" should "take neither" in {
    val venue = VenueNames.of(Seq("Dwie „Lalki” jednego wieczoru: Mariusz Dmochowski i Beata Tyszkiewicz u Hasa, " +
      "Marcin Dorociński i Kamila Urzędowska u Kawalskiego."))
    CastEvidence.tieBreak(venue, Seq(1321666 -> Some(doll2026), 81315 -> Some(doll1968))) shouldBe None
  }

  "the tie-break" should "prefer the namesake whose cast the text names twice, a rival named once standing in no way" in {
    val venue = VenueNames.of(Seq(ckLublin + " Jak niegdyś Tadeusz Fijewski."))
    CastEvidence.tieBreak(venue, Seq(1321666 -> Some(doll2026), 81315 -> Some(doll1968))) shouldBe
      Some(1321666 -> Seq("Marcin Dorociński", "Kamila Urzędowska", "Marek Kondrat"))
  }

  it should "take no lone candidate: there is no tie to break" in {
    CastEvidence.tieBreak(VenueNames.of(Seq(ckLublin)), Seq(1321666 -> Some(doll2026))) shouldBe None
  }

  it should "break no tie a rival of unknown cast is in, nor one two namesakes' casts are both named in" in {
    CastEvidence.tieBreak(VenueNames.of(Seq(ckLublin)), Seq(1321666 -> Some(doll2026), 81315 -> None)) shouldBe None
    val both = VenueNames.of(Seq("Mariusz Dmochowski i Beata Tyszkiewicz u Hasa, Marcin Dorociński i Kamila Urzędowska u Kawalskiego."))
    CastEvidence.tieBreak(both, Seq(1321666 -> Some(doll2026), 81315 -> Some(doll1968))) shouldBe None
  }

  "no venue text" should "name nothing" in {
    VenueNames.of(Nil).isEmpty shouldBe true
    CastEvidence.tieBreak(VenueNames.None, Seq(1321666 -> Some(doll2026))) shouldBe None
  }

  // A corpus holds a listing per film and venue (worker-us: ~100k): what each holds of its text is a handful of hashes,
  // and reading them off its row at intake a few allocations per word — never on the resolver's path, which reads none.
  "a listing's venue names" should "be read off its row once, held as a few hashes, and none of a feed catalogue's" in {
    val cm = models.CinemaMovie(models.Movie("Lalka"), models.KinoMuza, None, Some("https://kinomuza.pl/film/lalka"), Some(ckLublin),
      Seq("Marcin Dorociński", "Kamila Urzędowska"), Nil, Nil)
    val normalizer = services.movies.SingleCountryNormalizer.titleNormalizer
    val listing = Listing.of(models.KinoMuza, cm, normalizer)
    listing.names.names("Marek Kondrat") shouldBe true
    listing.names.size should be <= 8
    // outside the listing's equality: a venue rewording its synopsis re-resolves nothing
    listing shouldBe Listing.of(models.KinoMuza, cm.copy(synopsis = None), normalizer)
    // its cast FIELD is inside it: the model reads it ([[CastEvidence.mismatch]])
    listing should not be Listing.of(models.KinoMuza, cm.copy(cast = Nil), normalizer)
    Listing.of(models.KinoMuza, cm.copy(externalIds = Map("webedia" -> "1")), normalizer).names shouldBe VenueNames.None
    def perListing(row: models.CinemaMovie) = {
      Listing.of(models.KinoMuza, row, normalizer)
      tools.ThreadAllocation.of { var i = 0; while (i < 200) { Listing.of(models.KinoMuza, row, normalizer); i += 1 } }._2 / 200
    }
    val without = perListing(cm.copy(synopsis = None, cast = Nil))
    val withText = perListing(cm)
    info(s"Listing.of: $without bytes without its text, $withText with (+${withText - without}); held: ${16 + 4 * listing.names.size + 16} bytes")
    (withText - without) should be < 2_000L
  }

  "venue names" should "be held as a few hashes, the same for the same text" in {
    val venue = VenueNames.of(Seq(ckLublin))
    venue shouldBe VenueNames.of(Seq(ckLublin))
    venue.## shouldBe VenueNames.of(Seq(ckLublin)).##
    (venue ++ VenueNames.of(Nil, cast = Seq("Maria Dębska"))).names("Maria Dębska") shouldBe true
    // only the runs of capitalised words: the names, "Lalka", "Bolesława Prusa" — not every pair of the prose
    venue.size should be <= 8
  }

  // MSI cuts a long description at ~300 characters and the page keeps none of it: the cut text still names the cast.
  it should "be read from a venue's matching-only synopsis excerpt when it shows no synopsis" in {
    val cm = models.CinemaMovie(models.Movie("Lalka"), models.KinoMuza, None, None, None, Nil, Nil, Nil,
      synopsisExcerpt = Some("Marcin Dorociński, Kamila Urzędowska i Marek Kondrat w ekranizacji powieści Bolesława Prusa"))
    val listing = Listing.of(models.KinoMuza, cm, services.movies.SingleCountryNormalizer.titleNormalizer)
    listing.names.names("Kamila Urzędowska") shouldBe true
  }

  // ── the MISMATCH: the venue's own cast FIELD naming none of a film's cast ──

  private def lalkaAt(cast: Seq[String], director: Option[String] = None, title: String = "Lalka") =
    FilmTable.listing(models.Multikino, title, director = director).copy(listedCast = ListedCast.of(cast))
  private val Drama = IdentityMeasures.Film.genreBitsOf(Seq(18))
  private def film(cast: Seq[String], year: Int, director: String, language: String = "pl", genres: Int = Drama) =
    IdentityMeasures.Film("Lalka", year = Some(year), directors = Some(Seq(director)), originalLanguage = Some(language), genreBits = genres,
      cast = FilmCast.of(cast))
  // Helios' cast field of its "Lalka", as recorded 2026-10-15 (recording 38713274617)
  private val helios = Seq("Andrzej Seweryn, Krystyna Janda, Maria Dębska, Mateusz Damięcki, Marcin Dorociński, Kamila Urzędowska")
  private val has1968      = film(doll1968, 1968, "Wojciech Has")
  private val kawalski2026 = film(doll2026, 2026, "Maciej Kawalski")

  "the mismatch" should "rule Has's 1968 'Lalka' out where the venue's cast field lists the 2026 film's, and not the 2026 film" in {
    CastEvidence.mismatch(Seq(lalkaAt(helios)), has1968, Some("pl"), castIsTheFilms = true) shouldBe defined
    CastEvidence.mismatch(Seq(lalkaAt(helios)), kawalski2026, Some("pl"), castIsTheFilms = true) shouldBe None
  }

  it should "stand down where a fact the venue states agrees with the film (its director, its year)" in {
    CastEvidence.mismatch(Seq(lalkaAt(helios, director = Some("Wojciech Has"))), has1968, Some("pl"), castIsTheFilms = true) shouldBe None
    CastEvidence.mismatch(Seq(lalkaAt(helios).copy(year = Some(1968))), has1968, Some("pl"), castIsTheFilms = true) shouldBe None
  }

  it should "read no film of another language than the venue's: a dub's field lists its voices (UK 'Arrietty': Saoirse Ronan, Tom Holland)" in {
    val arrietty = IdentityMeasures.Film("Arrietty", year = Some(2010), originalLanguage = Some("ja"),
      cast = FilmCast.of(Seq("Mirai Shida", "Ryunosuke Kamiki", "Tomokazu Miura")))
    CastEvidence.mismatch(Seq(lalkaAt(Seq("Saoirse Ronan, Tom Holland, Olivia Colman"), title = "Arrietty")), arrietty, Some("en"), castIsTheFilms = true) shouldBe None
  }

  it should "read nothing beside a stage relay, nor where no listing lists a cast field" in {
    val relay = lalkaAt(Seq("Paul Marque, Sae Eun Park, Francesco Mura"), title = "Balet z Opery Paryskiej 2026-2027: Romeo i Julia")
    CastEvidence.castIsTheFilms(Seq(relay)) shouldBe false
    CastEvidence.castIsTheFilms(Seq(lalkaAt(helios))) shouldBe true
    CastEvidence.mismatch(Seq(FilmTable.listing(models.Multikino, "Lalka")), has1968, Some("pl"), castIsTheFilms = true) shouldBe None
  }

  it should "be told once per evidence node, as every scope and cluster holding the node asks it: the node's listings' own verdict" in {
    val relay = lalkaAt(Seq("Paul Marque, Sae Eun Park, Francesco Mura"), title = "Balet z Opery Paryskiej 2026-2027: Romeo i Julia")
    def node(listings: Listing*) = new EvidenceNode(Evidence.of(listings.head, None, TitleDecorations.None), listings)
    node(lalkaAt(helios)).castIsTheFilms shouldBe true
    node(lalkaAt(helios), relay).castIsTheFilms shouldBe false
    node(relay).castIsTheFilms shouldBe CastEvidence.castIsTheFilms(Seq(relay))
  }

  "a listed cast" should "name a name whatever order its words come in (UK 'Raise the Red Lantern': 'Li Gong' is TMDB's 'Gong Li')" in {
    FilmCast.of(Seq("Gong Li", "He Saifei")).get.namedBy(ListedCast.of(Seq("Li Gong | Saifei He"))) shouldBe 2
  }

  it should "be no evidence with fewer than two names, or beside a one-word entry: a mononymous star or a role (UK 'Jailer': Rajinikanth)" in {
    ListedCast.of(Seq("Marcin Dorociński")) shouldBe ListedCast.None
    ListedCast.of(Seq("Jackie Shroff | Vinayakan | Rajinikanth | Mohanlal")) shouldBe ListedCast.None
    ListedCast.of(Seq("Paul Marque (Romeo), Sae Eun Park (Julia)")) shouldBe ListedCast.None
  }

  "a film's cast" should "be no evidence in other letters than a venue's, nor for an animated film or a documentary" in {
    FilmCast.of(Seq("Γιάννης Κοκιασμένος", "Μαρία Διακοπαναγιώτου", "Αργύρης Πανταζάρας")) shouldBe None
    FilmCast.of(doll2026) shouldBe defined
  }

  "the mismatch" should "stand down for an animated film or a documentary (TMDB genres 16, 99): a dub's voices, a documentary's contributors" in {
    for (genre <- CastEvidence.VoiceOrContributorGenres)
      CastEvidence.mismatch(Seq(lalkaAt(helios)), film(doll1968, 1968, "Wojciech Has", genres = Drama | IdentityMeasures.Film.genreBitsOf(Seq(genre))),
        Some("pl"), castIsTheFilms = true) shouldBe None
  }

  it should "stand down for a record whose genres are not known: one filed before records kept them may be a dub's" in {
    CastEvidence.mismatch(Seq(lalkaAt(helios)), film(doll1968, 1968, "Wojciech Has", genres = 0), Some("pl"), castIsTheFilms = true) shouldBe None
  }
}
