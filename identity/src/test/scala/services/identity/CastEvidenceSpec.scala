package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The cast evidence on the real cases its offline measure met (2026-10-06, 244 labelled clusters: 24 decided, 24 right):
 * the venue's own text naming two whole names or more of ONE candidate's top-billed TMDB cast, and none of any other's.
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
    CastEvidence.take(venue, Seq(1321666 -> Some(doll2026), 81315 -> Some(doll1968))) shouldBe
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

  "one name of a candidate's cast" should "take nothing: US 'MetOpera: Medea (2022–23)' names Radvanovsky, who sings another production's Medea" in {
    // flicks.us's text of the Met's 2022–23 Medea (TMDB 1737985, no cast on TMDB); TMDB 1601989 is the 2025 Naples production
    val venue = VenueNames.of(Seq("Having triumphed at the Met in some of the repertory’s fiercest soprano roles, Sondra Radvanovsky stars as the " +
      "mythic sorceress who will stop at nothing in her quest for vengeance. Joining Radvanovsky in the Met-premiere production of Cherubini’s " +
      "rarely performed masterpiece is tenor Matthew Polenzani as Medea’s Argonaut husband, Giasone; soprano Janai Brugger as her rival."),
      cast = Seq("Matthew Polenzani", "Sondra Radvanovsky", "Ekaterina Gubanova", "Michele Pertusi", "Janai Brugger"))
    val naples = Seq("Sondra Radvanovsky", "Francesco Demuro", "Giorgi Manoshvili", "Désirée Giove", "Anita Rachvelishvili")
    CastEvidence.take(venue, Seq(1601989 -> Some(naples), 1737985 -> Some(Nil))) shouldBe None
  }

  "a double bill naming two films' casts" should "take neither" in {
    val venue = VenueNames.of(Seq("Dwie „Lalki” jednego wieczoru: Mariusz Dmochowski i Beata Tyszkiewicz u Hasa, " +
      "Marcin Dorociński i Kamila Urzędowska u Kawalskiego."))
    CastEvidence.take(venue, Seq(1321666 -> Some(doll2026), 81315 -> Some(doll1968))) shouldBe None
  }

  "a candidate whose cast is not known" should "leave the evidence unread: it might be the one the text names" in {
    CastEvidence.take(VenueNames.of(Seq(ckLublin)), Seq(1321666 -> Some(doll2026), 81315 -> None)) shouldBe None
  }

  "no venue text" should "name nothing" in {
    VenueNames.of(Nil).isEmpty shouldBe true
    CastEvidence.take(VenueNames.None, Seq(1321666 -> Some(doll2026))) shouldBe None
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
    listing shouldBe Listing.of(models.KinoMuza, cm.copy(synopsis = None, cast = Nil), normalizer)
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
}
