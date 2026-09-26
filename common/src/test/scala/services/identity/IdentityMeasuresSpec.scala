package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures.{Category, Film, Listing, ListingListing, Missing, Number}

/** What the calibrated score reads must separate facts that mean different things: a venue's
 *  published year, a year the venue put in its title, a season a broadcast names, and whether a
 *  title DECORATES a film's title or is a FRAGMENT of a longer one. */
class IdentityMeasuresSpec extends AnyFlatSpec with Matchers {

  private def measures(l: Listing, f: Film) = IdentityMeasures.listingFilm(l, f, None, 0, 0)

  "a bracketed year in a title" should "be its own measure, never the listing's published year" in {
    // A re-release bracket: the 2015 film shown in 2026.
    val m = measures(Listing("The Hunger Games: Mockingjay - Part 2 (2026)"), Film("The Hunger Games: Mockingjay - Part 2", year = Some(2015)))
    m("year.delta") shouldBe Missing("listing")
    m("titleYear.delta") shouldBe Number(-11)
    measures(Listing("Belle", year = Some(2013)), Film("Belle", year = Some(2013)))("titleYear.delta") shouldBe Missing("listing")
  }

  "a season a broadcast names" should "be read as its own measure, in every spelling, and never as a bracketed year" in {
    val film = Film("Samson et Dalila", year = Some(1949))
    Seq("Samson i dalila | metropolitan opera: live in hd 2026/27", "OPERA 2026/2027 - SAMSON I DALILA- RETRANSMISJA",
        "Met Opera 2026-27: Samson et Dalila", "Samson i Dalila (sezon 2026/27)", "Sezon 2026-2027 - Samson i Dalila").foreach { t =>
      withClue(t) {
        val m = measures(Listing(t), film)
        m("season.delta") shouldBe Number(-77)
        m("titleYear.delta") shouldBe Missing("listing")
      }
    }
    // Not a season: consecutive digits of one number, a year range that is not one season apart.
    measures(Listing("Blade Runner 2049"), film)("season.delta") shouldBe Missing("listing")
    measures(Listing("Retrospektywa 1990-1999"), film)("season.delta") shouldBe Missing("listing")
  }

  "a film record of a broadcast's season production" should "be a segment of the listing's title, and only for a film" in {
    val met = Listing("Met Opera 2026-27: Samson et Dalila")
    val record = Film("The Metropolitan Opera 2026/27: Samson et Dalila", year = Some(2026))
    IdentityMeasures.namesSeasonProduction(met, record) shouldBe true
    IdentityMeasures.titleRelation(met, record) shouldBe Category("segment")
    IdentityMeasures.namesSeasonProduction(Listing("Samson i dalila | metropolitan opera: live in hd 2026/27"),
      record.copy(alternativeTitles = Seq("The Metropolitan Opera 2026/27: Samson i Dalila"))) shouldBe true
    // Another season, another work of the season, or no season on one side: not its production.
    IdentityMeasures.titleRelation(met, record.copy(title = "The Metropolitan Opera 2025/26: Samson et Dalila")) shouldBe Category("overlap")
    IdentityMeasures.namesSeasonProduction(met, Film("The Metropolitan Opera 2026/27: Macbeth")) shouldBe false
    IdentityMeasures.namesSeasonProduction(Listing("Samson et Dalila"), record) shouldBe false
    // Two listings' banners do not say which house staged the work: the Met's and the RBO's
    // "Carmen" of one season are not one production by title.
    IdentityMeasures.listingListing(Listing("Met Opera 2026-27: Carmen"), Listing("RBO Cinema Season 2026-27: Carmen"),
      sameVenue = false, sharedChainId = None)("title") shouldBe Category("overlap")
  }

  "a title containing another" should "say which way: the listing decorates the film, or is a fragment of a longer title" in {
    IdentityMeasures.titleRelation(Listing("Ken Russell's The Devils"), Film("The Devils")) shouldBe Category("decorated")
    IdentityMeasures.titleRelation(Listing("It"), Film("It Ends with Us")) shouldBe Category("fragment")
  }

  "own agreement" should "count a bracketed year that matches, and deny only on a published year" in {
    IdentityMeasures.ownAgreement(measures(Listing("It (1990)"), Film("It", year = Some(1990))))._1 should contain ("year")
    IdentityMeasures.ownAgreement(measures(Listing("Toy Story (2026)"), Film("Toy Story", year = Some(1995))))._2 should not contain ("year")
    IdentityMeasures.ownAgreement(measures(Listing("Toy Story", year = Some(2026)), Film("Toy Story", year = Some(1995))))._2 should contain ("year")
  }

  "venues corroborating a film" should "count only venues whose own title names it, not every venue crediting its director" in {
    // 237 Regal venues list "Candyman (1992)" by Bernard Rose: they back his Candyman, but not
    // every other film the director walk turns up — a director alone does not pick his film.
    val group = (1 to 5).map(i => s"Venue $i" -> Listing("Candyman (1992)", directors = Seq("Bernard Rose")))
    val candyman = Film("Candyman", year = Some(1992), directors = Some(Seq("Bernard Rose")))
    val other    = Film("Paperhouse", year = Some(1988), directors = Some(Seq("Bernard Rose")))
    IdentityMeasures.corroboratingVenues(candyman, group, "Venue 1") shouldBe 4
    IdentityMeasures.corroboratingVenues(other, group, "Venue 1") shouldBe 0
  }

  "a group's venue backing" should "count, for every member and film, exactly what corroboratingVenues counts" in {
    val films = Seq(Film("Candyman", year = Some(2021), directors = Some(Seq("Nia DaCosta"))),
      Film("Candyman", year = Some(1992), directors = Some(Seq("Bernard Rose"))), Film("Candy", year = Some(2006)))
    val group = (1 to 12).map { i =>
      s"Venue ${i % 5}" -> Listing(if (i % 4 == 0) "Candyman (1992)" else "Candyman", year = Option.when(i % 3 == 0)(if (i % 2 == 0) 2021 else 1992),
        directors = if (i % 5 == 1) Seq("Nia DaCosta") else Nil)
    }
    val backing = new IdentityMeasures.VenueBacking(Map("candyman" -> group))
    for (f <- films; (venue, _) <- group :+ ("Elsewhere" -> Listing("Candyman")))
      withClue(s"$venue ${f.year}")(backing.corroborating("candyman", f, venue) shouldBe IdentityMeasures.corroboratingVenues(f, group, venue))
  }

  "a season title's banner" should "be how it spells its house, and a banner's house the one most of its works name" in {
    IdentityMeasures.listingBanner(Listing("RBO Cinema Season 2026-27: Manon")) shouldBe Some("rbocinemaseason")
    IdentityMeasures.filmBanner(Film("The Metropolitan Opera 2026/27: Manon")) shouldBe Some("themetropolitanopera")
    IdentityMeasures.listingBanner(Listing("Manon")) shouldBe None
    val named = Seq(("rbo", "royal", "swanlake"), ("rbo", "royal", "alice"), ("rbo", "met", "manon"), ("rbo", "royal", "swanlake"),
      ("met", "met", "manon"), ("tie", "royal", "a"), ("tie", "met", "b"))
    IdentityMeasures.housesOf(named) shouldBe Map("rbo" -> "royal", "met" -> "met")
  }

  "title shapes" should "de-decorate the parts a banner leaves, as well as the whole title" in {
    // "Throwback: Donnie Darko (25th Anniversary)": the banner split leaves "Donnie Darko (25th
    // Anniversary)", whose trailing bracket is itself a decoration.
    IdentityMeasures.titleShapes(Listing("Throwback: Donnie Darko (25th Anniversary)")) should contain ("Donnie Darko")
    IdentityMeasures.searchQueries(Listing("Throwback: Donnie Darko (25th Anniversary)")) should contain ("Donnie Darko")
  }

  "a listing's comparable facts" should "be every measure but the title relation, the ranking priors and the pooled count" in {
    IdentityMeasures.FactMeasures shouldBe Set("originalTitle", "year.delta", "year.distance", "titleYear.delta", "season.delta",
      "director", "runtime.delta", "country")
    val film = Film("The Last Whale Singer", year = Some(2025), directors = Some(Seq("Reza Memari")))
    // A title and nothing else: no fact to compare, whatever the titles' relation.
    IdentityMeasures.comparesAFact(IdentityMeasures.ListingFilm, measures(Listing("Vincent. Legenda oceanu"), film)) shouldBe false
    IdentityMeasures.comparesAFact(IdentityMeasures.ListingFilm, measures(Listing("Donnie Darko 25th Anniversary"), Film("Donnie Darko"))) shouldBe false
    // A published fact the film cannot be compared on (no year in its record) is not a comparison.
    IdentityMeasures.comparesAFact(IdentityMeasures.ListingFilm, measures(Listing("Vincent", year = Some(2025)), Film("Vincent"))) shouldBe false
    IdentityMeasures.comparesAFact(IdentityMeasures.ListingFilm, measures(Listing("Vincent", year = Some(1975)), film)) shouldBe true
    IdentityMeasures.comparesAFact(IdentityMeasures.ListingFilm, measures(Listing("Vincent (1975)"), film)) shouldBe true
    IdentityMeasures.comparesAFact(IdentityMeasures.ListingFilm, measures(Listing("Vincent", originalTitle = Some("Vincent")), film)) shouldBe true
  }

  private def pair(a: Listing, b: Listing) = IdentityMeasures.listingListing(a, b, sameVenue = false, sharedChainId = None)

  "two listings' title relation" should "read one title as a whole delimited piece of the other's in either order" in {
    def title(a: String, b: String) = pair(Listing(a), Listing(b))("title")
    title("Astra Seniora - Lalka", "Lalka") shouldBe Category("segment")
    title("Lalka", "Astra Seniora - Lalka") shouldBe Category("segment")
    title("Tony", "Kino bez barier: Tony (AD + CC)") shouldBe Category("segment")
    // A token run that is no delimited piece stays what it was, and two banners' shared words are
    // no segment of either title.
    title("It", "It Ends with Us") shouldBe Category("fragment")
    title("Kino bez barier: Tony", "Kino bez barier: Lalka") shouldBe Category("overlap")
  }

  "two listings' comparable facts" should "be every listing-listing measure but the title relation and the venue" in {
    IdentityMeasures.ListingFactMeasures shouldBe Set("originalTitle", "year.delta", "titleYear.delta", "season.delta",
      "director", "runtime.delta", "chainId")
    // A fact one side publishes and the other does not — a year in one's bracket, the other's field —
    // is no comparison.
    IdentityMeasures.comparesAFact(ListingListing, pair(Listing("Lalka (2026)"),
      Listing("Filmowy Klub Seniora: LALKA", year = Some(2026), directors = Seq("Maciej Kawalski")))) shouldBe false
    IdentityMeasures.comparesAFact(ListingListing, pair(Listing("Lalka", year = Some(2026)), Listing("Lalka | PREMIERA", year = Some(2026)))) shouldBe true
    IdentityMeasures.comparesAFact(ListingListing, pair(Listing("Lalka", directors = Seq("Maciej Kawalski")),
      Listing("Lalka", directors = Seq("Wojciech Has")))) shouldBe true
  }

  "a runtime a title brackets with a minute mark" should "be the listing's runtime when it publishes none" in {
    // KinoPort prints the running time into the title: "DZIADKU WIEJEMY (97’)".
    val film = Film("Dziadku, wiejemy!", year = Some(2025), runtime = Some(97))
    Seq("DZIADKU WIEJEMY (97’)", "Dziadku wiejemy (97')", "Dziadku wiejemy (97′)", "Dziadku wiejemy [97 min]").foreach { t =>
      withClue(t)(measures(Listing(t), film)("runtime.delta") shouldBe Number(0))
    }
    // A published runtime wins; a year, a sequel number or a bare number is not a runtime.
    measures(Listing("Dziadku wiejemy (97’)", runtime = Some(99)), film)("runtime.delta") shouldBe Number(2)
    Seq("Dziadku wiejemy (2025)", "Ocean's 11", "Kino 60 Krzeseł", "Dziadku wiejemy (97)").foreach { t =>
      withClue(t)(measures(Listing(t), film)("runtime.delta") shouldBe Missing("listing"))
    }
    // Between two listings too: the bracketed runtime is the listing's runtime.
    IdentityMeasures.listingListing(Listing("DZIADKU WIEJEMY (97’)"), Listing("Dziadku wiejemy", runtime = Some(97)),
      sameVenue = false, sharedChainId = None)("runtime.delta") shouldBe Number(0)
  }

  "a director credited in another script" should "be compared in Latin letters, not read as incomparable" in {
    // TMDB credits a film's director in the deployment language's name for them, which is often
    // the native one ("毕赣", "Яков Протазанов"); the venue prints the Latin spelling.
    IdentityMeasures.directorRelation(Seq("Bi Gan"), Seq("毕赣")) shouldBe Category("same_person")
    IdentityMeasures.directorRelation(Seq("Giorgos Lanthimos"), Seq("Γιώργος Λάνθιμος")) shouldBe Category("same_person")
    IdentityMeasures.directorRelation(Seq("Kira Muratova"), Seq("Кира Муратова")) shouldBe Category("same_person")
    // A transliteration convention apart (Yakov / Akov): the surname is shared.
    IdentityMeasures.directorRelation(Seq("Yakov Protazanov"), Seq("Яков Протазанов")) shouldBe Category("shared_name")
    IdentityMeasures.directorRelation(Seq("Ljubomir Stefanov"), Seq("Љубомир Стефанов")) shouldBe Category("shared_name")
    IdentityMeasures.directorRelation(Seq("Ljubomir Stefanov", "Tamara Kotevska"), Seq("Љубомир Стефанов", "Тамара Котевска")) shouldBe
      Category("same_person")
  }

  it should "still tell different people apart, as a disagreement across scripts of its own" in {
    // A mismatch after transliteration is weaker than a Latin-to-Latin one (a Japanese reading of
    // Kanji is not its pinyin), so the calibration weighs it apart from `different`.
    IdentityMeasures.directorRelation(Seq("Bi Gan"), Seq("张艺谋")) shouldBe Category("different_script")
    IdentityMeasures.directorRelation(Seq("Kira Muratova"), Seq("Андрей Тарковский")) shouldBe Category("different_script")
    // Both in one script: compared as ever.
    IdentityMeasures.directorRelation(Seq("Wong Kar Wai"), Seq("Kim Jeong-hwan")) shouldBe Category("different")
    IdentityMeasures.directorRelation(Seq("Кира Муратова"), Seq("Кира Муратова")) shouldBe Category("same_person")
  }

  "a decorated original title" should "read as naming the film, as a decorated title does, not as a word overlap" in {
    val yourName = Seq("Twoje imię", "君の名は。", "Your Name.")
    IdentityMeasures.originalTitleRelation(Some("Your Name (re-release)"), yourName) shouldBe Category("segment")
    IdentityMeasures.originalTitleRelation(Some("Your Name"), yourName) shouldBe Category("match")
    IdentityMeasures.originalTitleRelation(Some("Ken Russell's The Devils"), Seq("The Devils")) shouldBe Category("decorated")
    IdentityMeasures.originalTitleRelation(Some("It"), Seq("It Ends with Us")) shouldBe Category("fragment")
    IdentityMeasures.originalTitleRelation(Some("Stalker"), Seq("Solaris")) shouldBe Category("disjoint")
    // The listing-film measure reads it the same way.
    measures(Listing("Twoje imię", originalTitle = Some("Your Name (re-release)")),
      Film("Twoje imię", Some("君の名は。"), alternativeTitles = Seq("Your Name.")))("originalTitle") shouldBe Category("segment")
  }

  "a listing's exact top hits" should "be the films its title names exactly that a title search returned FIRST" in {
    val listing = Listing("Godzilla vs. Megalon")
    val megalon = Film("Godzilla vs. Megalon", year = Some(1973))
    val remake  = Film("Godzilla vs. Megalon", year = Some(2031))
    val other   = Film("Godzilla vs. Mothra", year = Some(1964))
    IdentityMeasures.exactTopHits(listing, Seq((1, megalon, Some(1)), (2, remake, Some(2)), (3, other, Some(1)))) shouldBe Seq(1)
    // First only under a banner segment's search, or not first at all: no exact top hit.
    IdentityMeasures.exactTopHits(Listing("Kino Nocne: Godzilla vs. Megalon"), Seq((1, megalon, Some(1)))) shouldBe Nil
    IdentityMeasures.exactTopHits(listing, Seq((1, megalon, Some(2)), (3, other, Some(1)), (4, megalon, None))) shouldBe Nil
  }
}
