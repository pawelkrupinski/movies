package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures.{Billing, Category, Film, Houses, Listing, ListingListing, Missing, Number}

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

  "a house's record of a work" should "be a segment of a listing spelling it with a bracket year instead of the season" in {
    // US venues list the Met's 2026/27 season as "The Metropolitan Opera: Manon (2027)"; TMDB files
    // it as "The Metropolitan Opera 2026/27: Manon". Neither year is part of how the house bills it.
    IdentityMeasures.titleRelation(Listing("The Metropolitan Opera: Manon (2027)"), Film("The Metropolitan Opera 2026/27: Manon")) shouldBe
      Category("segment")
    IdentityMeasures.titleRelation(Listing("The Metropolitan Opera: Così fan tutte (2026)"),
      Film("The Metropolitan Opera 2026/27: Così fan tutte")) shouldBe Category("segment")
    // Another work of the house is not.
    IdentityMeasures.titleRelation(Listing("The Metropolitan Opera: Manon (2027)"), Film("The Metropolitan Opera 2026/27: Otello")) shouldBe
      Category("overlap")
  }

  "a season production" should "be searched by its work and its season, which is how the database finds a house it spells otherwise" in {
    IdentityMeasures.searchQueries(Listing("RBO Cinema Season 2026-27: Manon")) should contain ("Manon 2026")
    IdentityMeasures.searchQueries(Listing("Samson i dalila | metropolitan opera: live in hd 2026/27")) should contain ("Samson i dalila 2026")
    IdentityMeasures.searchQueries(Listing("NT Live: Manon")).exists(_.contains("20")) shouldBe false
    // Only the title naming the season asks, and a bracketed year is no part of the work.
    val cleaned = Listing("The Metropolitan Opera: Manon (2027)", rawTitle = Some("Met Opera 2026-27: Manon (2027)"))
    IdentityMeasures.searchQueries(cleaned).filter(_.endsWith(" 2026")) shouldBe Seq("Manon 2026")
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

  "a published year beside the same director" should "date a screening, not the film, when it denies the film" in {
    val devils = Film("Diabły", year = Some(1971), directors = Some(Seq("Ken Russell")))
    val screened = measures(Listing("Diabły", year = Some(2026), directors = Seq("Ken Russell")), devils)
    IdentityMeasures.PublishedYear.foreach(m => screened(m) shouldBe IdentityMeasures.MissingListing)
    // A title that does not name the film leaves the year a fact: it tells the director's films
    // apart (KINOMUZEUM's 2026 "Błotem w twarz" is Jaak Kilmi's 2026 film, not his 2017 "Sangarid").
    measures(Listing("Błotem w twarz", year = Some(2026), directors = Seq("Jaak Kilmi")),
      Film("Sangarid", year = Some(2017), directors = Some(Seq("Jaak Kilmi"))))("year.delta") shouldBe Number(9)
    // Nor does a title that only starts with the film's ("Basia. Humor w paski mam + Kocia Szajka…"
    // beside Wasilewski's 2018 "Basia"): it is not the film's title, so the year is not its screening.
    measures(Listing("Basia. Humor w paski mam + Kocia Szajka. Tajemnica zniknięcia śledzi", year = Some(2026),
      directors = Seq("Marcin Wasilewski")), Film("Basia", year = Some(2018), directors = Some(Seq("Marcin Wasilewski"))))("year.delta") shouldBe Number(8)
    // Agreeing, it stays the fact it is; beside another director, a year decades off denies.
    measures(Listing("Diabły", year = Some(1971), directors = Seq("Ken Russell")), devils)("year.delta") shouldBe Number(0)
    measures(Listing("Diabły", year = Some(2026), directors = Seq("Someone Else")), devils)("year.delta") shouldBe Number(55)
    measures(Listing("Diabły", year = Some(2026)), devils)("year.delta") shouldBe Number(55)
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
      withClue(s"$venue ${f.year}")(backing.corroborating(Seq("candyman"), f, venue) shouldBe IdentityMeasures.corroboratingVenues(f, group, venue))
  }

  it should "count nothing for a title group no venue lists" in {
    // PL, a decorated "Pucio ... 2D DUB" whose undecorated title no other venue lists bare: its
    // undecorated group is empty, not missing (the calibration's refit threw on it).
    val decorated = Listing("Pucio 2D DUB", decorations = TitleDecorations(Set.empty, Set(Seq("2d", "dub"))))
    val groups    = IdentityMeasures.titleGroups(decorated)
    groups should contain (IdentityMeasures.key("Pucio"))
    val backing = new IdentityMeasures.VenueBacking(Map(IdentityMeasures.key(decorated.title) -> Seq("Kino 1" -> decorated)))
    backing.corroborating(groups, Film("Pucio", year = Some(2026)), "Kino 2") shouldBe 0
  }

  "a title's billing" should "be the work two titles share and how each spells its house, and a banner's house the one most of its works name" in {
    IdentityMeasures.billing(Listing("RBO Cinema Season 2026-27: Manon"), Film("The Metropolitan Opera 2026/27: Manon")) shouldBe
      Some(Billing(Seq("rbo", "cinema", "season"), Seq("the", "metropolitan", "opera"), "manon"))
    IdentityMeasures.billing(Listing("NT Live: Les Liaisons Dangereuses (2025)"), Film("National Theatre Live: Les Liaisons Dangereuses")) shouldBe
      Some(Billing(Seq("nt", "live"), Seq("national", "theatre", "live"), "lesliaisonsdangereuses"))
    // The work alone on either side bills no house.
    IdentityMeasures.billing(Listing("Manon"), Film("The Metropolitan Opera 2026/27: Manon")) shouldBe None
    IdentityMeasures.billing(Listing("Throwback: Manon"), Film("Manon")) shouldBe None
    def b(banner: String, house: String, work: String) = Billing(Seq(banner), Seq(house), work)
    val houses = Houses.learn(Seq(b("rbo", "royal", "swanlake"), b("rbo", "royal", "alice"), b("rbo", "met", "manon"), b("rbo", "royal", "swanlake"),
      b("met", "met", "manon"), b("met", "met", "otello"), b("tie", "royal", "a"), b("tie", "met", "b"), b("tie", "royal", "c"), b("tie", "met", "d"),
      b("once", "nt", "hamlet")))
    houses shouldBe Houses(Map("rbo" -> "royal", "met" -> "met"))
    houses.other(b("rbo", "met", "manon")) shouldBe true
    houses.same(b("rbo", "royal", "manon")) shouldBe true
    houses.same(b("once", "once", "x")) shouldBe true
    houses.other(b("once", "nt", "hamlet")) shouldBe false
    // A banner's own words name its house before any count of works does.
    def w(banner: String, house: String, work: String) = Billing(banner.split(" ").toSeq, house.split(" ").toSeq, work)
    Houses.learn(Seq(w("metropolitan opera live in hd", "royal ballet opera", "carmen"), w("metropolitan opera live in hd", "royal ballet opera", "cosi"),
      w("metropolitan opera live in hd", "the metropolitan opera", "cosi"))) shouldBe Houses(Map("metropolitanoperaliveinhd" -> "themetropolitanopera"))
    // Words both rivals share decide nothing, and a house outside the rivalry does not make them decide.
    Houses.learn(Seq(w("met opera", "the metropolitan opera", "cosi"), w("met opera", "the metropolitan opera", "manon"),
      w("met opera", "the metropolitan opera", "otello"), w("met opera", "royal ballet opera", "cosi"), w("met opera", "royal ballet opera", "manon"),
      w("met opera", "salzburger festspiele", "carmen"))) shouldBe Houses(Map("metopera" -> "themetropolitanopera"))
    Houses.learn(Seq(w("nt live", "national theatre live", "earnest"), w("nt live", "national theatre live", "playboy"),
      w("nt live", "rsc live", "macbeth"))) shouldBe Houses(Map("ntlive" -> "nationaltheatrelive"))
  }

  "a record billing the listing's work under its learned house" should "be a segment of its title, and only under that house" in {
    val nt = Listing("NT Live: The Importance of Being Earnest")
    val record = Film("National Theatre Live: The Importance of Being Earnest")
    val houses = Houses(Map("ntlive" -> "nationaltheatrelive"))
    IdentityMeasures.titleRelation(nt, record) shouldBe Category("overlap")
    IdentityMeasures.titleRelation(nt, record, houses) shouldBe Category("segment")
    IdentityMeasures.titleRelation(nt, Film("RSC Live: The Importance of Being Earnest"), houses) shouldBe Category("overlap")
    IdentityMeasures.listingFilm(nt, record, None, 0, 0, houses)("title") shouldBe Category("segment")
  }

  "qualifiers" should "be the pieces records bill beside more works than the rest of the listing's title" in {
    val q = IdentityMeasures.Qualifiers.learn(Seq("Chocolate - Director's Cut",
      "The Great War: Director's Cut", "The Promise (Director's Cut)", "Director's Cut", "Dark City", "Manhunter").map(Film(_)))
    q.of(Listing("Dark City: Director's Cut")) shouldBe Set("directorscut")
    // A bare title, and a title whose only piece is its bracketed year, have no qualifier.
    q.of(Listing("Director's Cut")) shouldBe empty
    q.of(Listing("Manhunter (1986)")) shouldBe empty
    // A record titled only the qualifier names nothing; the work's record still names the listing.
    val cut = Listing("Dark City: Director's Cut")
    IdentityMeasures.titleRelation(cut, Film("Director's Cut")) shouldBe Category("segment")
    IdentityMeasures.titleRelation(cut, Film("Director's Cut"), Houses.Unknown, q) shouldBe Category("overlap")
    IdentityMeasures.titleRelation(cut, Film("Dark City"), Houses.Unknown, q) shouldBe Category("decorated")
    IdentityMeasures.titleRelation(cut, Film("Dark City: Director's Cut"), Houses.Unknown, q) shouldBe Category("exact")
  }

  it should "tie to nothing when both pieces are billed alike" in {
    val q = IdentityMeasures.Qualifiers.learn(Seq("Heat: Director's Cut", "Heat - Extended", "Alien: Director's Cut", "Heat").map(Film(_)))
    // "directorscut" trailing two works, "heat" leading two pieces: neither is the listing's qualifier.
    q.of(Listing("Heat: Director's Cut")) shouldBe empty
  }

  it should "leave a work its record under a banner when the records bill it on the other side" in {
    // PL "Tani wtorek: OBCY": the records bill the work BEFORE its sequels, the listing after its banner.
    val q = IdentityMeasures.Qualifiers.learn(Seq("Obcy", "Obcy: Przymierze", "Obcy: Romulus", "Obcy - 8. pasażer Nostromo").map(Film(_)))
    q.of(Listing("Tani wtorek: Obcy")) shouldBe empty
    IdentityMeasures.titleRelation(Listing("Tani wtorek: Obcy"), Film("Obcy"), Houses.Unknown, q) shouldBe Category("segment")
  }

  it should "never be the piece a listing publishes as its original title" in {
    // UK Cineworld's "Cineworld 30: The Dark Knight" (x87), originally "The Dark Knight": TMDB bills
    // the work after two banners too, but the venue names it as the film.
    val q = IdentityMeasures.Qualifiers.learn(Seq("The Dark Knight", "Enter the World of Hans Zimmer: The Dark Knight",
      "GARO - Kiba: The Dark Knight").map(Film(_)))
    q.of(Listing("Cineworld 30: The Dark Knight")) shouldBe Set("thedarkknight")
    val cineworld = Listing("Cineworld 30: The Dark Knight", originalTitle = Some("The Dark Knight"))
    q.of(cineworld) shouldBe empty
    IdentityMeasures.titleRelation(cineworld, Film("The Dark Knight"), Houses.Unknown, q) shouldBe Category("segment")
  }

  "an edition" should "be a later record carrying the work's title under a qualifier, never a namesake" in {
    val murnau = Film("Nosferatu", alternativeTitles = Seq("Nosferatu: A Symphony of Horror"), year = Some(1922))
    IdentityMeasures.editionOf(Film("Radiohead X Nosferatu: A Symphony of Horror", year = Some(2025)), murnau) shouldBe true
    IdentityMeasures.editionOf(Film("Nosferatu: A Symphony of Horror", year = Some(2023)), murnau) shouldBe false
    IdentityMeasures.editionOf(Film("Nosferatu", year = Some(2024)), murnau) shouldBe false
    IdentityMeasures.editionOf(murnau, Film("Radiohead X Nosferatu: A Symphony of Horror", year = Some(2025))) shouldBe false
    // DE "Die Puppe" (1975, originally "The Doll") is a namesake of Has's "Lalka", which TMDB also
    // calls "The Doll": an original title is a whole title, never a piece of the record's own.
    val has = Film("Lalka", alternativeTitles = Seq("The Doll"), year = Some(1968))
    IdentityMeasures.editionOf(Film("Die Puppe", originalTitle = Some("The Doll"), year = Some(1975)), has) shouldBe false
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

  "a director's name" should "be the same person whatever its order, case and splitting" in {
    // BTS São Paulo: the venue writes the given name joined and the surname last in capitals.
    IdentityMeasures.directorRelation(Seq("Jungjae HA"), Seq("Ha Jung-jae")) shouldBe Category("same_person")
    IdentityMeasures.directorRelation(Seq("Makoto Shinkai"), Seq("Shinkai Makoto")) shouldBe Category("same_person")
    // Every letter must be accounted for: a name word missing is not the same written name.
    IdentityMeasures.directorRelation(Seq("Jung Ha"), Seq("Ha Jung-jae")) should not be Category("same_person")
    IdentityMeasures.directorRelation(Seq("Jungjae Kim"), Seq("Ha Jung-jae")) should not be Category("same_person")
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

  "an original title copying the listing's own title" should "repeat it whether whole or a truncation" in {
    // UK BTS relays: the venue's original-title field is its own title cut short ("…: Live").
    val bts = "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES: LIVE VIEWING"
    IdentityMeasures.repeatsItsTitle(Listing(bts, originalTitle = Some("BTS World Tour 'ARIRANG' In Buenos Aires: Live"))) shouldBe true
    IdentityMeasures.repeatsItsTitle(Listing(bts, originalTitle = Some(bts))) shouldBe true
    IdentityMeasures.repeatsItsTitle(Listing("Diabły | Splat!FilmFest", originalTitle = Some("Diabły"))) shouldBe true
    // Anything the title does not already carry is a second fact: another language's title, a
    // shared word, or a LONGER title the venue shortened for display.
    IdentityMeasures.repeatsItsTitle(Listing("Diabły | Splat!FilmFest", originalTitle = Some("The Devils"))) shouldBe false
    IdentityMeasures.repeatsItsTitle(Listing("De Gaulle: Part 2 - Liberte", originalTitle = Some("La Bataille de Gaulle - Partie 2 : J’écris ton nom"))) shouldBe false
    IdentityMeasures.repeatsItsTitle(Listing("Alien", originalTitle = Some("Alien: Romulus"))) shouldBe false
    IdentityMeasures.repeatsItsTitle(Listing("Alien")) shouldBe false
    // UK Everyman: an original title that holds the title as one piece of more is not a copy. Read as
    // one, it stopped counting against Zeffirelli's 1968 film and the Royal Ballet's staging took it.
    IdentityMeasures.repeatsItsTitle(Listing("Royal Ballet and Opera: Romeo and Juliet",
      originalTitle = Some("Royal Ballet and Opera: Romeo and Juliet (INACTIVE)"))) shouldBe false
  }

  it should "be absent against a film the title names, and stay what it measures against one it does not" in {
    val bts = Listing("BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES: LIVE VIEWING", originalTitle = Some("BTS World Tour 'ARIRANG' In Buenos Aires: Live"))
    measures(bts, Film("BTS World Tour 'Arirang' in Buenos Aires: Live Viewing"))("originalTitle") shouldBe IdentityMeasures.MissingListing
    // UK Everyman: the repeat against The Mummy, which its title does not name, is still a disjoint title.
    val dracula = Listing("Dracula (4K Restoration)", originalTitle = Some("Dracula 4k Restoration"))
    measures(dracula, Film("The Mummy"))("originalTitle") shouldBe Category("disjoint")
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

  "a sequel numeral" should "be compared by the NUMBER each title carries, however it is written" in {
    def numeral(listing: String, film: Film) = IdentityMeasures.numeralRelation(Listing(listing), film)
    // US, the recorded "The Texas Chainsaw Massacre 2" ×4 (labelled 16337): TMDB's 1974 original
    // (30497, alternative title "The Texas Chainsaw Massacre") is the series without the number.
    val original = Film("The Texas Chain Saw Massacre", alternativeTitles = Seq("Leatherface", "The Texas Chainsaw Massacre"))
    val sequel   = Film("The Texas Chainsaw Massacre Part 2", alternativeTitles = Seq("TCM 2", "The Texas Chain Saw Massacre Part 2"))
    numeral("The Texas Chainsaw Massacre 2", original) shouldBe Category("listing_only")
    numeral("The Texas Chainsaw Massacre 2", sequel) shouldBe Category("same")
    numeral("The Texas Chainsaw Massacre", sequel) shouldBe Category("film_only")
    numeral("Mortal Kombat 2", Film("Mortal Kombat II")) shouldBe Category("same")
    numeral("Rocky II", Film("Rocky")) shouldBe Category("listing_only")
    numeral("Toy Story 2", Film("Toy Story 3")) shouldBe Category("different")
    numeral("Kill Bill: Vol. 2 (2026)", Film("Kill Bill: Vol. 1")) shouldBe Category("different")
    numeral("Star Wars: Episode IV - A New Hope", Film("Star Wars", alternativeTitles = Seq("Star Wars: Episode IV - A New Hope"))) shouldBe Category("same")
  }

  it should "have nothing to compare in a remake, a title whose number is its name, or a decoration's number" in {
    def numeral(listing: String, film: String) = IdentityMeasures.numeralRelation(Listing(listing), Film(film))
    numeral("Suspiria", "Suspiria") shouldBe Missing("none")
    numeral("2001: A Space Odyssey", "2001: A Space Odyssey") shouldBe Missing("none")
    numeral("1917", "1917") shouldBe Missing("none")
    numeral("Se7en", "Se7en") shouldBe Missing("none")
    numeral("Ocean's Eleven", "Ocean's Eleven") shouldBe Missing("none")
    numeral("2046", "2046") shouldBe Missing("none")
    numeral("9 to 5", "9 to 5") shouldBe Category("same")
    numeral("Cineworld 30: The Matrix", "The Matrix") shouldBe Missing("none")
    numeral("Sense and Sensibility (2026)", "Sense and Sensibility") shouldBe Missing("none")
    numeral("The Metropolitan Opera 2026/27: Manon", "Manon") shouldBe Missing("none")
    // A lone "i" inside a title is a word (Polish "and"), and L, C, D, M spell words, not instalments.
    numeral("Vivaldi i ja", "Vivaldi") shouldBe Missing("none")
    numeral("Listy do M. 5", "Listy do M. 5") shouldBe Category("same")
    numeral("Listy do M 5", "Listy do M. 5") shouldBe Category("same")
    numeral("Zodiac", "Fight Club") shouldBe Missing("unrelated")
  }

  it should "keep a title that numbers another instalment from NAMING the film its words decorate" in {
    val original = Film("The Texas Chain Saw Massacre", alternativeTitles = Seq("The Texas Chainsaw Massacre"))
    val listing  = Listing("The Texas Chainsaw Massacre 2")
    IdentityMeasures.titleRelation(listing, original) shouldBe Category("decorated")
    IdentityMeasures.namesFilm(listing, original) shouldBe false
    IdentityMeasures.namesFilm(Listing("Throwback: The Texas Chainsaw Massacre"), original) shouldBe true
    IdentityMeasures.namesFilm(Listing("Cineworld 30: The Matrix"), Film("The Matrix")) shouldBe true
    // So no venue's own facts back the original on a sequel's title.
    val group = Seq("a", "b").map(_ -> listing.copy(directors = Seq("Tobe Hooper")))
    IdentityMeasures.backingVenues(original.copy(directors = Some(Seq("Tobe Hooper"))), group) shouldBe empty
  }

  "a listing that publishes nothing but its title" should "be told from one that publishes a fact" in {
    Listing("Sense and Sensibility").publishesAFact shouldBe false
    Listing("Relaxed Screening: Sense and Sensibility").publishesAFact shouldBe false
    Listing("Sense and Sensibility", originalTitle = Some("Sense and Sensibility")).publishesAFact shouldBe false
    Listing("Sense and Sensibility (2026)").publishesAFact shouldBe true
    Listing("Sense and Sensibility", directors = Seq("Georgia Oakley")).publishesAFact shouldBe true
    Listing("Sense and Sensibility", runtime = Some(132)).publishesAFact shouldBe true
  }

  "a title naming two films" should "name them apart when their pieces sit at spans of it that do not overlap" in {
    val gruffalo = Film("The Gruffalo"); val child = Film("The Gruffalo's Child")
    // A double bill: the two titles share words, but each has its own place in the title.
    IdentityMeasures.namedApart(Listing("The Gruffalo + The Gruffalo's Child"), gruffalo, child) shouldBe true
    // Disjoint words, as before.
    IdentityMeasures.namedApart(Listing("Lalka (Dolly)"), Film("Lalka"), Film("Dolly")) shouldBe true
    // Nested: one film's title is part of the other's, which the whole title names.
    IdentityMeasures.namedApart(Listing("Joker: Folie à deux"), Film("Joker"), Film("Joker: Folie à deux")) shouldBe false
    IdentityMeasures.namedApart(Listing("The Gruffalo's Child"), gruffalo, child) shouldBe false
    // A film named by both of its titles leaves no place of its own for a third title's word.
    IdentityMeasures.namedApart(Listing("Tokyo Story (Tôkyô monogatari)"), Film("Tokyo Story", Some("Tôkyô monogatari")), Film("Tokyo")) shouldBe false
    // A film whose own title joins two others' is named by the whole title, which overlaps both.
    IdentityMeasures.namedApart(Listing("Romeo + Juliet"), Film("Romeo + Juliet"), Film("Romeo")) shouldBe false
    IdentityMeasures.namedApart(Listing("Fast & Furious"), Film("Fast & Furious"), Film("Furious")) shouldBe false
  }

  "the titles venues publish for a record" should "be a title published beside one of its titles as the original, when unanimous" in {
    val concert = Film("André Rieu's 2026 Summer Concert: Viva Maastricht!")
    val listings = Seq(
      Listing("Andre Rieu. Niech żyje Maastricht!", originalTitle = Some("Andre Rieu's 2026 Summer Concert: Viva Maastricht!")),
      Listing("André Rieu. Niech żyje Maastricht!"),
      // An original title repeating the listing's own title translates nothing.
      Listing("Nosferatu", originalTitle = Some("Nosferatu")))
    IdentityMeasures.venueTitles(listings, Seq(1 -> concert, 2 -> Film("Nosferatu"))) shouldBe
      Map(1 -> Seq("Andre Rieu. Niech żyje Maastricht!"))
    IdentityMeasures.titleRelation(Listing("André Rieu. Niech żyje Maastricht!"),
      IdentityMeasures.withVenueTitles(concert, Seq("Andre Rieu. Niech żyje Maastricht!"))) shouldBe Category("alternative")
    // Two venues giving one title two originals name two films by it: it translates neither.
    val invitation = Seq(Listing("La invitación", originalTitle = Some("The Invitation")), Listing("La invitación", originalTitle = Some("The Invite")))
    IdentityMeasures.venueTitles(invitation, Seq(1 -> Film("The Invitation"), 2 -> Film("The Invite"))) shouldBe Map.empty
    // A record already titled so gains nothing, nor one the title names already.
    IdentityMeasures.venueTitles(Seq(Listing("Diuna", originalTitle = Some("Dune"))), Seq(1 -> Film("Diuna", Some("Dune")))) shouldBe Map.empty
    IdentityMeasures.venueTitles(Seq(Listing("Coraline (2009)", originalTitle = Some("Coraline"))), Seq(1 -> Film("Coraline"))) shouldBe Map.empty
    // An original naming several records names none of them: the facts pick among namesakes.
    IdentityMeasures.venueTitles(Seq(Listing("Niebo nad Normandią", originalTitle = Some("Pressure"))),
      Seq(1 -> Film("Pressure"), 2 -> Film("Pressure"))) shouldBe Map.empty
    IdentityMeasures.venueTitles(Seq(Listing("Diabły", originalTitle = Some("The Devils"))),
      Seq(31767 -> Film("Diabły", Some("The Devils")), 1491681 -> Film("The Devils"))) shouldBe Map.empty
  }

  "a venue's one-letter typo in a long word" should "still name the film's title exactly" in {
    // US: "Shaun the Sheep: The Beast of Mossy Botton" (×6) and "Pradhama Drishtiya Kuttakkar" (×16)
    // found the film but measured the title as unrelated; the pipeline's fuzzy TMDB search took them.
    IdentityMeasures.titleRelation(Listing("Shaun the Sheep: The Beast of Mossy Botton"), Film("Shaun the Sheep: The Beast of Mossy Bottom")) shouldBe IdentityMeasures.Category("exact")
    IdentityMeasures.titleRelation(Listing("Pradhama Drishtiya Kuttakkar"), Film("Pradhama Drishtya Kuttakkar")) shouldBe IdentityMeasures.Category("exact")
  }

  it should "not reach a sequel's number, a short word, or two words" in {
    IdentityMeasures.titleRelation(Listing("Scary Movie 3"), Film("Scary Movie 4")) should not be IdentityMeasures.Category("exact")
    IdentityMeasures.titleRelation(Listing("Mission: Impossible II"), Film("Mission: Impossible III")) should not be IdentityMeasures.Category("exact")
    IdentityMeasures.titleRelation(Listing("The Hunt"), Film("The Hurt")) should not be IdentityMeasures.Category("exact")
    IdentityMeasures.titleRelation(Listing("The Beast of Mossy Botton"), Film("The Feast of Mossy Bottom")) should not be IdentityMeasures.Category("exact")
  }

  it should "also name the film from a shape of the title, and from the original title" in {
    // UK, Vue's "Pradhama Drishtiya Kuttakkar (Malayalam)" (×13), originally "Pradhama Drishtiya
    // Kuttakkar": the language tag is a shape away, and the original title carries the same typo.
    IdentityMeasures.titleRelation(Listing("Pradhama Drishtiya Kuttakkar (Malayalam)"), Film("Pradhama Drishtya Kuttakkar")).value should
      (be("exact") or be("segment"))
    IdentityMeasures.originalTitleRelation(Some("Pradhama Drishtiya Kuttakkar"), Seq("Pradhama Drishtya Kuttakkar")) shouldBe IdentityMeasures.Category("match")
    IdentityMeasures.originalTitleRelation(Some("Scary Movie 3"), Seq("Scary Movie 4")) should not be IdentityMeasures.Category("match")
  }

  it should "not reach a one-word title: a letter apart there is another film" in {
    // UK, 64 "Lalka" / "Lalka (The Doll)" listings, originally "Lalka", were taken for "Lalkar" (1972).
    IdentityMeasures.originalTitleRelation(Some("Lalka"), Seq("Lalkar")) should not be IdentityMeasures.Category("match")
    IdentityMeasures.titleRelation(Listing("Lalka"), Film("Lalkar")) should not be IdentityMeasures.Category("exact")
  }

  "an original title" should "match a film's title that differs only by a year or a season" in {
    // DE, 153 "MET Opera Live im Kino: Così Fan Tutte" listings, originally "The Metropolitan Opera: Così
    // fan tutte (2026)", against the record "The Metropolitan Opera 2026/27: Così fan tutte".
    IdentityMeasures.originalTitleRelation(Some("The Metropolitan Opera: Così fan tutte (2026)"),
      Seq("The Metropolitan Opera 2026/27: Così fan tutte"), filmYear = Some(2026)) shouldBe IdentityMeasures.Category("match")
    IdentityMeasures.originalTitleRelation(Some("Dune (2021)"), Seq("Dune"), filmYear = Some(2021)) shouldBe IdentityMeasures.Category("match")
    IdentityMeasures.originalTitleRelation(Some("The Metropolitan Opera: Carmen (2026)"),
      Seq("Royal Ballet & Opera 2026/27: Carmen")) should not be IdentityMeasures.Category("match")
  }

  it should "not match when the year it drops disagrees with the film's" in {
    // UK, Cineworld's "The Royal Ballet: The Nutcracker", originally "... (2024)", went to the 2015 recording
    // "The Royal Ballet: The Nutcracker" instead of the 2024/25 production.
    IdentityMeasures.originalTitleRelation(Some("The Royal Ballet: The Nutcracker (2024)"), Seq("The Royal Ballet: The Nutcracker"),
      filmYear = Some(2015)) should not be IdentityMeasures.Category("match")
    IdentityMeasures.originalTitleRelation(Some("The Metropolitan Opera: Così fan tutte (2026)"),
      Seq("The Metropolitan Opera 2026/27: Così fan tutte"), filmYear = Some(2026)) shouldBe IdentityMeasures.Category("match")
  }
}
