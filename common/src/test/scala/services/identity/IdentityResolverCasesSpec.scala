package services.identity

import models.{CharlieMonroe, Cinema, CinemaCityKinepolis, CinemaCityKorona, CinemaCityPoznanPlaza, CinemaCityWroclavia, Helios, HeliosAlejaBielany, HeliosMagnolia, KinoApollo, KinoBulgarska, KinoCytadela, KinoDKFRumcajs, KinoMikro, KinoMuza, KinoOaza, KinoPalacowe, Kinoteka, Multikino, MultikinoPasazGrunwaldzki, Rialto, StacjaFalenica}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingConstraints, ListingKey, SingleCountryNormalizer}

/**
 * The resolver on hand-built families shaped like historical incidents. The film database is a
 * small table; each case asserts only what the evidence decides. The cases are TEST LABELS: no
 * title, venue or film below reaches the resolver as a rule.
 */
class IdentityResolverCasesSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val weights    = IdentityCalibration.fromResource("services/identity/test-calibration.json").get
  import FilmTable.{F, listing}

  private def resolve(listings: Seq[Listing], films: Seq[F]): Resolution =
    IdentityResolver.resolve(listings, new FilmTable(films, normalizer), normalizer, weights)

  private def together(r: Resolution, a: Listing, b: Listing) = r.decisionOf(a.key) eq r.decisionOf(b.key)

  "A listing whose work matches a film's, under a translated subtitle" should "take that film when its director is the film's" in {
    // PL, Multikino's "Cirque du Soleil: Kurios - Gabinet osobliwości" (×38): the record is "Cirque du
    // Soleil: KURIOS - Cabinet des curiosités", same director. The pipeline searched the work and
    // took the hit its director verified — no wrong match among the 3,247 listings it decided that way.
    val films = Seq(F(558130, "Cirque du Soleil: KURIOS - Cabinet des curiosités", 2018, "Michel Laprise", 85, 3))
    val ls    = Seq(Multikino, Helios).map(v => listing(v, "Cirque du Soleil: Kurios - Gabinet osobliwości", director = Some("Michel Laprise"),
      runtime = Some(89)).copy(originalTitle = Some("Cirque du Soleil: KURIOS – Cabinet of Curiosities")))
    val r = shipped(ls, films)
    withClue(ls.map(l => r.decisionOf(l.key).render).distinct.mkString("\n"))(ls.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(558130)))
  }

  "A listing that publishes only a film's work, without its subtitle" should "take the film when its director and year are the film's" in {
    // ES, three venues' "Leonas" (2026, Juan Manuel Cotelo, 94 min) is "Leonas, el instinto más
    // salvaje": the calibration read the bare title as a fragment of the record's.
    // TMDB ranks it fifth for "Leonas", below four other films of the word.
    val films = Seq(F(9101, "Leonas", 2019, "Ana Ruiz", 88, 40), F(9102, "Las leonas", 2021, "Pedro Gil", 101, 35),
      F(9103, "Leonas de Atlas", 2012, "Eva Sanz", 76, 30), F(9104, "Leonas del mar", 2008, "Luis Mora", 90, 25),
      F(1651192, "Leonas, el instinto más salvaje", 2026, "Juan Manuel Cotelo", 94, 2))
    val ls    = Seq(Multikino, Helios).map(listing(_, "Leonas", Some(2026), Some("Juan Manuel Cotelo"), Some(94)))
    val r = shipped(ls, films)
    withClue(ls.map(l => r.decisionOf(l.key).render).distinct.mkString("\n"))(ls.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(1651192)))
  }

  it should "take the film whose work is the original title the venue publishes, by its director and year" in {
    // DE, Goli Theater's "Ein Hund namens Quill" (2004, Yōichi Sai, 100 min), published as "Quill": TMDB's German
    // record is "Quill - Ein Freund für´s Leben", which TMDB's search does not return for either title (only the
    // director's filmography reaches it), so the title and original title read as overlap and fragment.
    val films = Seq(F(49258, "Quill - Ein Freund für´s Leben", 2004, "Yōichi Sai", 100, 0.6, searched = false),
      F(9201, "Quill", 2015, "Anna Berg", 12, 40))
    val l = listing(Multikino, "Ein Hund namens Quill", Some(2004), Some("Yōichi Sai"), Some(100)).copy(originalTitle = Some("Quill"))
    val d = shipped(Seq(l), films).decisionOf(l.key)
    withClue(d.render)(d.film shouldBe Some(49258))
  }

  it should "take its director's staging both titles bill under a banner, when it is the only record billing the work" in {
    // UK Flicks' "Royal Shakespeare Company: Macbeth" {Polly Findlay} [133′] ×9: TMDB's "RSC Live: Macbeth" (2018,
    // 123′) is reached only by her filmography, the one-word work names it, and the listing publishes no year.
    val films = Seq(F(512470, "RSC Live: Macbeth", 2018, "Polly Findlay", 123, 2, searched = false),
      F(27883, "Macbeth", 1948, "Orson Welles", 107, 20), F(225728, "Macbeth", 2015, "Justin Kurzel", 113, 30))
    val l = listing(Multikino, "Royal Shakespeare Company: Macbeth", None, Some("Polly Findlay"), Some(133))
    val d = shipped(Seq(l), films).decisionOf(l.key)
    withClue(d.render)(d.film shouldBe Some(512470))
  }

  "A listing whose own whole-title search returns one film" should "take it though the title's shorter form names others, when the film carries every word" in {
    // ES Ocine's "Manon (BALLET LIVE)" [165′] ×7, published as "Manon": its whole-title search returns only TMDB's
    // "BALLET LIVE. MANON. ROYAL ÓPERA HOUSE", denied at 2.4% by the cut alone (the original title reads as a
    // fragment of it); "Manon" names twenty films, none of them a ballet.
    val films = Seq(F(471328, "BALLET LIVE. MANON. ROYAL ÓPERA HOUSE", 2018, "", 0, 0.3),
      F(132332, "Manon", 1949, "Henri-Georges Clouzot", 101, 8), F(1071117, "Manon", 2001, "Someone", 163, 2))
    val l = listing(Multikino, "Manon (BALLET LIVE)", None, None, Some(165)).copy(originalTitle = Some("Manon"))
    val d = shipped(Seq(l), films).decisionOf(l.key)
    withClue(d.render)(d.film shouldBe Some(471328))
  }

  it should "not take a one-word work's film when no year says which" in {
    val films = Seq(F(1651192, "Leonas, el instinto más salvaje", 2026, "Juan Manuel Cotelo", 94, 2))
    val l = listing(Multikino, "Leonas", None, Some("Juan Manuel Cotelo"))
    val d = shipped(Seq(l), films).decisionOf(l.key)
    withClue(d.render)(d.film shouldBe None)
  }

  "A listing that publishes a film's title without its colon subtitle" should "take that film when TMDB ranks it first and no other fits" in {
    // US, 1,915 listings of "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES" / "… IN SÃO PAULO": TMDB's first
    // hit is "BTS World Tour 'Arirang' in Buenos Aires: Live Viewing", measured as a fragment of it; the
    // director walk's 2022 Seoul concert film was the best candidate left. Neither pipeline matched them.
    val films = Seq(F(1770237, "BTS World Tour 'Arirang' in Buenos Aires: Live Viewing", 2026, "", 0, 5),
      F(939984, "BTS: Permission to Dance on Stage - Seoul", 2022, "Jungjae HA", 195, 20))
    val ls = Seq(Multikino, Helios).map(listing(_, "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES", director = Some("Jungjae HA"), runtime = Some(195)))
    val r = shipped(ls, films)
    withClue(ls.map(l => r.decisionOf(l.key).render).distinct.mkString("\n"))(ls.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(1770237)))
  }

  it should "take it though its runtime is a quarter hour off, the listing billing the work under a shorter subtitle" in {
    // DE ×140 "BTS World Tour 'ARIRANG' In Buenos Aires: Live" (195 min): TMDB's German record of
    // "…: Live Viewing" states 180. The title only measured a fragment of it, and that title's weight
    // with the runtime gap vetoed the one record the title names, as if it were another film.
    val films = Seq(F(1770237, "BTS World Tour 'Arirang'  in Buenos Aires: Live Viewing", 2026, "", 180, 5),
      F(1701849, "BTS WORLD TOUR [ARIRANG] in Busan", 2026, "Ha Jung-jae", 180, 5))
    val ls = Seq(Multikino, Helios).map(listing(_, "BTS World Tour 'ARIRANG' In Buenos Aires: Live", Some(2026), Some("Jungjae Ha"), Some(195)))
    val r = shipped(ls, films)
    withClue(ls.map(l => r.decisionOf(l.key).render).distinct.mkString("\n"))(ls.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(1770237)))
  }

  "Listings carrying one chain's catalogue id" should "be one film, whatever each venue calls it" in {
    // US Showcase lists ONE "It" (boxoffice 144685) at every venue, a bare listing beside its credited
    // siblings; a chain's id is global to it (0 ids reused for another film across the five recorded
    // corpora). Here one venue bills the film under a title no search reaches.
    val films = Seq(F(1417, "Pan's Labyrinth", 2006, "Guillermo del Toro", 119), F(399055, "The Shape of Water", 2017, "Guillermo del Toro", 123))
    val showcase = Seq(CatalogueId("boxoffice", "1000045847"))
    val credited = listing(Multikino, "Pan's Labyrinth", director = Some("Guillermo del Toro"), runtime = Some(119)).copy(catalogueIds = showcase)
    val spanish  = listing(Helios, "El laberinto del fauno").copy(catalogueIds = showcase)
    val r = shipped(Seq(credited, spanish), films)
    withClue(Seq(credited, spanish).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      r.decisionOf(spanish.key).film shouldBe Some(1417)
    }
    // Another id is another film of that chain: the title alone reaches nothing.
    val other = spanish.copy(catalogueIds = Seq(CatalogueId("boxoffice", "1000045848")))
    shipped(Seq(credited, other), films).decisionOf(other.key).film shouldBe None
  }

  it should "keep a re-release's bare listings on the film their credited siblings name, not a namesake of the re-release year" in {
    // US Showcase "9 to 5 (2026)" (boxoffice 1000052877): two venues credit Colin Higgins, two publish
    // only the title, whose "(2026)" dated them onto a two-minute 2026 short about a dachshund.
    val films = Seq(F(19494, "Nine to Five", 1980, "Colin Higgins", 110, 12.0, alternatives = Seq("9 to 5")),
      F(1683690, "9 to 5", 2026, "Castiel Collyer", 2, 0.2))
    val showcase = Seq(CatalogueId("boxoffice", "1000052877"))
    val credited = Seq(Multikino, Helios).map(v => listing(v, "9 to 5 (2026)", Some(2026), Some("Colin Higgins"), Some(110)).copy(catalogueIds = showcase))
    val bare     = Seq(KinoApollo, Rialto).map(v => listing(v, "9 to 5 (2026)", Some(2026)).copy(catalogueIds = showcase))
    val r = shipped(credited ++ bare, films)
    withClue((credited ++ bare).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      (credited ++ bare).map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(19494))
    }
    r.violations shouldBe 0
  }

  "A season listing's banner" should "name its house from every season production of its work, not only those its own search found" in {
    // The Met's record reached the family through another venue's search only; as evidence it names
    // the banner's house by its words (`MetSeasonHouseCase`).
    import MetSeasonHouseCase.*
    val r = IdentityResolver.resolve(Seq(cosi, carmen, other), lookups(normalizer), normalizer, IdentityCalibration.resolver)
    withClue(Seq(cosi, carmen, other).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      r.decisionOf(carmen.key).film should not be Some(RboCarmen)
      r.decisionOf(cosi.key).film should not be Some(RboCosi)
    }
  }

  it should "take its house's record of the work dated in its season, when TMDB's title of it names no season" in {
    // US Flicks' "Met Opera 2026-27: Così fan tutte" {The Metropolitan Opera} ×356 (recording 37216285654): TMDB's
    // US title of the Met's 2026 Così is "The Metropolitan Opera: Così fan tutte" (1703620, twelfth in the search),
    // naming no season, so no season production was found; Royal Ballet & Opera's 2026/27 Così ranks first and is
    // another house's. The banner is the Met's by the season records of its other works.
    val films = Seq(F(1703620, "The Metropolitan Opera: Così fan tutte", 2026, "Phelim McDermott", 0, 1),
      F(1702775, "Royal Ballet & Opera 2026/27: Così fan tutte", 2027, "", 0, 20),
      F(455831, "The Metropolitan Opera: Così fan tutte", 2018, "Phelim McDermott", 200, 3),
      F(551689, "Così fan tutte", 1989, "Peter Sellars", 240, 8),
      F(1703629, "The Metropolitan Opera 2026/27: La Fanciulla del West", 2027, "", 0, 2),
      F(1703622, "The Metropolitan Opera 2026/27: Macbeth", 2026, "", 0, 2))
    val cosi   = listing(Multikino, "Met Opera 2026-27: Così fan tutte", None, Some("The Metropolitan Opera"))
    val others = Seq(listing(Multikino, "Met Opera 2026-27: La Fanciulla del West"), listing(Multikino, "Met Opera 2026-27: Macbeth"))
    val r = shipped(cosi +: others, films)
    withClue((cosi +: others).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      r.decisionOf(cosi.key).film shouldBe Some(1703620)
      others.map(l => r.decisionOf(l.key).film) shouldBe Seq(Some(1703629), Some(1703622))
    }
  }

  it should "not take a sequel whose title runs on from a film the listing names exactly" in {
    val films = Seq(F(346364, "It", 2017, "Andy Muschietti", 135, 60), F(474350, "It: Chapter Two", 2019, "Andy Muschietti", 169, 50))
    val l = listing(Multikino, "It")
    val d = shipped(Seq(l), films).decisionOf(l.key)
    withClue(d.render)(d.film should not be Some(474350))
  }

  "A decorated spelling" should "take the film its plain siblings' own evidence matched" in {
    val films = Seq(F(1, "Lalka", 2026, "Maciej Kawalski", 150, 5), F(2, "Lalka", 1968, "Wojciech Has", 159, 8))
    val plain = Seq(Multikino, Helios, KinoApollo).map(listing(_, "Lalka", Some(2026), Some("Maciej Kawalski")))
    val decorated = listing(KinoMuza, "Oficjalna premiera: Lalka")
    val r = resolve(plain :+ decorated, films)
    r.decisionOf(plain.head.key).film shouldBe Some(1)
    r.decisionOf(decorated.key).film shouldBe Some(1)
    together(r, plain.head, decorated) shouldBe true
  }

  "A listing whose own title search is empty" should
    "be offered the film its identical title's siblings reached, and take it on its own facts" in {
    // PL, "Vincent. Legenda oceanu": TMDB titles the film "The Last Whale Singer", so the Polish
    // title's own search is empty; the credited venues reach it through their director. One venue
    // publishes a year and a credit the record contradicts, so its own evidence denies the film
    // and it is kept apart from them. A venue publishing the film's year and running time but no
    // credit is title-linked to both — ambiguous, so it stays alone (A2) — and, with no candidate
    // of its own, was NoCandidate. Its identical title's siblings' candidates are its candidates
    // too, scored on its own facts.
    val films    = Seq(F(677558, "The Last Whale Singer", 2025, "Reza Memari", 91, 5))
    val credited = Seq(Multikino, Helios, Rialto).map(listing(_, "Vincent. Legenda oceanu", Some(2025), Some("Reza Memari")))
    val denier   = listing(KinoMuza, "Vincent. Legenda oceanu", Some(1998), Some("Pavel Hrubos"))
    val dated    = listing(KinoApollo, "VINCENT. LEGENDA OCEANU", Some(2025), runtime = Some(91))
    val r = resolve(credited ++ Seq(denier, dated), films)
    withClue((credited ++ Seq(denier, dated)).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      r.decisionOf(credited.head.key).film shouldBe Some(677558)
      r.decisionOf(denier.key).film shouldBe None
      r.decisionOf(dated.key).film shouldBe Some(677558)
    }
    r.violations shouldBe 0
  }

  "A spelling a learned venue decoration wraps" should "take the film its plain siblings matched" in {
    // A strand no title rule strips (Cineworld's "(4DX Rewind) Shrek" was one until its rule):
    // the whole title names no film, so its own search is
    // empty and the plain "Shrek" the other venues list never reaches it. "(Retro Rewind)" is a
    // LEARNED decoration (it recurs around other films' titles and no record carries it), so
    // "Shrek" is one of its title shapes: searched and read by the title relation, and the film
    // it then takes on its own joins it to its plain siblings.
    val films     = Seq(F(808, "Shrek", 2001, "Andrew Adamson", 90, 60), F(809, "Shrek 2", 2004, "Andrew Adamson", 93, 50))
    val plain     = Seq(Multikino, Helios).map(listing(_, "Shrek", Some(2001), Some("Andrew Adamson")))
    val decorated = Seq(KinoApollo, KinoMuza).map(listing(_, "(Retro Rewind) Shrek"))
    val learned   = TitleDecorations(Set(Seq("retro", "rewind")), Set.empty)
    def run(d: TitleDecorations) = IdentityResolver.resolve(plain ++ decorated, new FilmTable(films, normalizer), normalizer, weights, decorations = d)
    val r = run(learned)
    withClue((plain ++ decorated).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      (plain ++ decorated).map(l => r.decisionOf(l.key).film) shouldBe Seq.fill(4)(Some(808))
      together(r, plain.head, decorated.head) shouldBe true
    }
    r.violations shouldBe 0
    // Without the learned decoration the spelling names nothing, as before.
    decorated.map(l => run(TitleDecorations.None).decisionOf(l.key).film) shouldBe Seq(None, None)
  }

  it should "be corroborated by the venues listing the title it wraps, as a bare listing of that title is" in {
    // PL, Ekobilet's "Mistyczka 2D PL": nothing but the title. Undecorated it is a bare
    // "Mistyczka", which the other venues list with the film's year and director; they corroborate
    // it as they corroborate each other's bare listings. It is not title-linked to them (a learned
    // decoration relates it to a FILM, never to a listing), so without their corroboration it
    // stays below the cut.
    val films     = Seq(F(1731866, "Mistyczka", 2026, "Jan Sobierajski", 100, 1), F(2, "Mistyczka", 1994, "Someone Else", 90, 2))
    val plain     = Seq(Multikino, Helios, Rialto).map(listing(_, "Mistyczka", Some(2026), Some("Jan Sobierajski")))
    val decorated = listing(KinoApollo, "Mistyczka 2D PL")
    val learned   = TitleDecorations(Set.empty, Set(Seq("2d", "pl")))
    val r = IdentityResolver.resolve(plain :+ decorated, new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver, decorations = learned)
    withClue((plain :+ decorated).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      r.decisionOf(plain.head.key).film shouldBe Some(1731866)
      r.decisionOf(decorated.key).film shouldBe Some(1731866)
    }
    r.violations shouldBe 0
  }

  it should "not be title-linked by it to another venue's bare namesake" in {
    // UK, Cineworld's "Horror Season 2026 Dracula" crediting Terence Fisher: stripped of its learned
    // season banner it is searched as "Dracula", but it is not a segment sibling of every venue's
    // bare "Dracula". A must-link to one listing Besson's film, whose facts deny Fisher's, would
    // withdraw its own match (a title-linked sibling denies it) and leave it unmatched.
    val films   = Seq(F(11868, "Dracula", 1958, "Terence Fisher", 82, 8), F(1246049, "Dracula", 2025, "Luc Besson", 130, 60))
    val season  = Seq(Multikino, Helios).map(listing(_, "Horror Season 2026 Dracula", director = Some("Terence Fisher"), runtime = Some(82)))
    val besson  = listing(KinoApollo, "Dracula", Some(2025), Some("Luc Besson"), Some(130))
    val learned = TitleDecorations(Set(Seq("horror", "season", "2026")), Set.empty)
    val r = IdentityResolver.resolve(season :+ besson, new FilmTable(films, normalizer), normalizer, weights, decorations = learned)
    withClue((season :+ besson).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      season.map(l => r.decisionOf(l.key).film) shouldBe Seq(Some(11868), Some(11868))
      r.decisionOf(besson.key).film shouldBe Some(1246049)
      r.edges.exists(e => e.must && e.reason == "title-segment") shouldBe false
    }
    r.violations shouldBe 0
  }

  "A listing that publishes only a title" should
    "not be denied its credited siblings' film by the title relation alone" in {
    // PL, the Polish title of a foreign film: TMDB's record carries only the original title, so a
    // bare listing's title shares no word with it (`title=none`). The credited siblings reach it
    // through their director; the bare same-titled one and the bannered one must follow them —
    // nothing they publish contradicts the film, and a title relation is a score, not a veto.
    val films     = Seq(F(677558, "The Last Whale Singer", 2025, "Reza Memari", 91, 5))
    val credited  = Seq(Multikino, Helios).map(listing(_, "Vincent. Legenda oceanu", Some(2025), Some("Reza Memari")))
    val bare      = listing(KinoApollo, "VINCENT. LEGENDA OCEANU")
    val bannered  = listing(KinoMuza, "Dyskusyjny Klub Bajkowy: Vincent. Legenda oceanu")
    val r = resolve(credited ++ Seq(bare, bannered), films)
    val shown = (credited ++ Seq(bare, bannered)).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")
    withClue(shown) {
      (credited ++ Seq(bare, bannered)).map(l => r.decisionOf(l.key).film) shouldBe Seq.fill(4)(Some(677558))
      together(r, credited.head, bare) shouldBe true
      together(r, credited.head, bannered) shouldBe true
    }
    r.violations shouldBe 0
  }

  it should "still be denied a film one published fact contradicts" in {
    // The same shape, but the bare listing states a year decades from the film's: a fact, so the
    // veto stands and the listing stays off the credited cluster.
    val films    = Seq(F(677558, "The Last Whale Singer", 2025, "Reza Memari", 91, 5))
    val credited = Seq(Multikino, Helios).map(listing(_, "Vincent. Legenda oceanu", Some(2025), Some("Reza Memari")))
    val dated    = listing(KinoApollo, "VINCENT. LEGENDA OCEANU", Some(1975))
    val r = resolve(credited :+ dated, films)
    r.decisionOf(credited.head.key).film shouldBe Some(677558)
    r.decisionOf(dated.key).film shouldBe None
    together(r, credited.head, dated) shouldBe false
    r.violations shouldBe 0
  }

  /** The fixture calibration with a listing-listing cannot-link cut strict enough that two
   *  listings whose titles only overlap fall below it, as the production cut does. */
  private val strictPairCut = weights.copy(scopes = weights.scopes.updatedWith(IdentityMeasures.ListingListing)(_.map(s =>
    s.copy(thresholds = s.thresholds.updated("cannotLink", IdentityCalibration.Threshold(0.5))))))

  "A bare listing beside its credited decorated siblings" should
    "take their film when the two publish no fact to compare" in {
    // PL, ADA Kino Studyjne's bare "Tony": every other venue lists it under a programme banner
    // with an access-format suffix. The pair's titles only overlap one way round, but nothing the
    // bare one publishes can contradict its siblings, so the pair's score is no veto and the
    // title-segment must-link joins them. (The bare listing's venue sorts first, so the pair is
    // measured from its side: the order the veto used to depend on.)
    val films     = Seq(F(1329016, "Tony", 2026, "Matt Johnson", 106, 5), F(2, "Tony", 2003, "Someone Else", 90, 8))
    val decorated = Seq(Multikino, KinoMuza).map(listing(_, "Kino bez barier: Tony (AD + CC)", Some(2026), Some("Matt Johnson")))
    val bare      = listing(Helios, "Tony")
    val r = IdentityResolver.resolve(decorated :+ bare, new FilmTable(films, normalizer), normalizer, strictPairCut)
    withClue((decorated :+ bare).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      r.decisionOf(decorated.head.key).film shouldBe Some(1329016)
      r.decisionOf(bare.key).film shouldBe Some(1329016)
      together(r, decorated.head, bare) shouldBe true
    }
    r.violations shouldBe 0
  }

  it should "stay apart from them when a year it publishes contradicts theirs" in {
    val films     = Seq(F(1329016, "Tony", 2026, "Matt Johnson", 106, 5), F(2, "Tony", 2003, "Someone Else", 90, 8))
    val decorated = Seq(Multikino, KinoMuza).map(listing(_, "Kino bez barier: Tony (AD + CC)", Some(2026), Some("Matt Johnson")))
    val dated     = listing(Helios, "Tony", Some(2003))
    val r = IdentityResolver.resolve(decorated :+ dated, new FilmTable(films, normalizer), normalizer, strictPairCut)
    r.decisionOf(decorated.head.key).film shouldBe Some(1329016)
    together(r, decorated.head, dated) shouldBe false
    r.decisionOf(dated.key).film should not be Some(1329016)
    r.violations shouldBe 0
  }

  "Two listings no film database answers for" should
    "stay apart when the years and directors they publish contradict, with no film to tell them apart" in {
    // DE production shadow, lookups still sparse: Sheri Hagen's "Billie" (2025) and James Erskine's
    // "Billie – Legende des Jazz" (2020). The shorter title is a whole segment of the longer, which
    // must-links them; their own facts are what keep them apart, film or no film.
    val hagen   = Seq(Multikino, Helios).map(listing(_, "Billie", Some(2025), Some("Sheri Hagen"), Some(101)))
    val erskine = Seq(KinoMuza, Rialto).map(listing(_, "Billie – Legende des Jazz", Some(2020), Some("James Erskine"), Some(98)))
    for (cut <- Seq(weights, strictPairCut)) {
      val r = IdentityResolver.resolve(hagen ++ erskine, new FilmTable(Nil, normalizer), normalizer, cut)
      together(r, hagen.head, hagen(1)) shouldBe true
      together(r, erskine.head, erskine(1)) shouldBe true
      together(r, hagen.head, erskine.head) shouldBe false
      r.violations shouldBe 0
    }
  }

  it should "join a decorated spelling to its plain sibling when neither publishes a fact to compare" in {
    val plain     = listing(Helios, "Lalka")
    val decorated = listing(KinoMuza, "Astra Seniora - Lalka")
    for (cut <- Seq(weights, strictPairCut)) {
      val r = IdentityResolver.resolve(Seq(plain, decorated), new FilmTable(Nil, normalizer), normalizer, cut)
      together(r, plain, decorated) shouldBe true
      r.violations shouldBe 0
    }
  }

  "A bare listing its own evidence cannot separate between two films" should
    "follow its title's credited siblings, not the database's popularity ranking" in {
    // Four 2026 films TMDB titles "Lalka"; the one the venue credits ranks LAST in the search, so
    // on ranking priors alone a bare listing scores it below the cannot-link cut — which is
    // ambiguity, not evidence of another film, and must not veto it.
    val films = Seq(F(1, "Lalka", 2026, "Maciej Kawalski", 150, 5), F(2, "Lalka", 2026, "Someone Else", 95, 80),
      F(3, "Lalka", 2026, "A Third", 100, 60), F(4, "Lalka", 2026, "A Fourth", 90, 40))
    val credited = Seq(Multikino).map(listing(_, "Lalka", Some(2026), Some("Maciej Kawalski")))
    val bare     = Seq(KinoApollo, KinoMuza, Rialto).map(listing(_, "Lalka"))
    val r = resolve(credited ++ bare, films)
    r.decisionOf(credited.head.key).film shouldBe Some(1)
    bare.map(l => r.decisionOf(l.key).film) shouldBe Seq.fill(3)(Some(1))
    r.violations shouldBe 0
  }

  "A broadcast's bare listing" should "not take an old film its title-linked sibling's season denies" in {
    // The Met's 2026/27 "Samson et Dalila": one venue names the season, another lists the bare
    // title. TMDB's search for the Polish title returns only DeMille's 1949 film.
    val films  = Seq(F(29993, "Samson i Dalila", 1949, "Cecil B. DeMille", 131, 20))
    val season = listing(Multikino, "Samson i dalila | metropolitan opera: live in hd 2026/27")
    val bare   = Seq(Helios, KinoApollo).map(listing(_, "Samson i Dalila"))
    val r = resolve(season +: bare, films)
    (season +: bare).foreach(l => withClue(l.title + "\n" + (season +: bare).map(x => r.decisionOf(x.key).render).distinct.mkString("\n"))(r.decisionOf(l.key).film should not be Some(29993)))
    r.violations shouldBe 0
  }

  "A broadcast naming its season" should "take its season's production record, never a namesake film of another year" in {
    // The Met's 2026/27 "Silent Night": TMDB files the season's production as a film of its own,
    // crediting nobody, beside John Woo's 2023 action film — which ranks first and matches the
    // work's name exactly. A venue listing Woo's film keeps it.
    val films = Seq(F(891699, "Silent Night", 2023, "John Woo", 104, 60),
      F(1707867, "The Metropolitan Opera 2026/27: Silent Night", 2027, "", 0, 1))
    val met  = Seq(Multikino, Helios).map(listing(_, "Met Opera 2026-27: Silent Night")) :+
      listing(KinoMuza, "OPERA 2026/2027 - SILENT NIGHT - RETRANSMISJA")
    val woo  = listing(Rialto, "Silent Night", Some(2023), Some("John Woo"), Some(104))
    val r = resolve(met :+ woo, films)
    met.foreach(l => withClue(l.title + ": " + r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe Some(1707867)))
    r.decisionOf(woo.key).film shouldBe Some(891699)
    together(r, met.head, woo) shouldBe false
    r.violations shouldBe 0
  }

  it should "take its season's record when the venue names the work in its own language" in {
    // PL "Royal Ballet and Opera Sezon Kinowy 2026-27: Dziadek do orzechów": TMDB holds the season's record only as
    // "Royal Ballet & Opera 2026/27: The Nutcracker", which no search of the Polish title returns
    val films = Seq(F(1702790, "Royal Ballet & Opera 2026/27: The Nutcracker", 2026, "", 0, 3),
      F(495198, "Dziadek do orzechów", 1967, "Someone Else", 80, 8))
    val pl = Seq(Helios, KinoApollo).map(listing(_, "Royal Ballet and Opera Sezon Kinowy 2026-27: Dziadek do orzechów"))
    val r = resolve(pl, films)
    pl.foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe Some(1702790)))
  }

  it should "take its house's season record of its work from its year, when the title names no season" in {
    // DE "Royal Ballet & Opera im Kino: Manon" [2026] {Kenneth MacMillan} ×59 and "MET Opera Live im Kino: Manon" [2027]
    // {Laurent Pelly} ×4: two houses' 2026/27 Manon, told apart by the year each venue publishes; neither title names
    // the season, and "Manon" alone ranks a dozen namesake films first
    val films = Seq(F(1702757, "Royal Ballet & Opera 2026/27: Manon", 2026, "", 0, 2), F(1703631, "The Metropolitan Opera 2026/27: Manon", 2027, "", 0, 2),
      F(132332, "Manon", 1949, "Henri-Georges Clouzot", 100, 9), F(370009, "Manon", 1986, "Someone", 135, 4))
    val rbo = Seq(Multikino, Helios).map(listing(_, "Royal Ballet & Opera im Kino: Manon", Some(2026), Some("Kenneth MacMillan"), Some(190)))
    val met = listing(KinoApollo, "MET Opera Live im Kino: Manon", Some(2027), Some("Laurent Pelly"), Some(264))
    val r = resolve(rbo :+ met, films)
    rbo.foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe Some(1702757)))
    withClue(r.decisionOf(met.key).render)(r.decisionOf(met.key).film shouldBe Some(1703631))
  }

  it should "take the one season record whose title is its own but for the season, and not choose between two seasons" in {
    // UK "Royal Ballet and Opera: Romeo and Juliet" {Kenneth MacMillan} [170′] ×127 screens May–June 2027: TMDB's
    // "Royal Ballet & Opera 2026/27: Romeo and Juliet", which the cut alone denied for its season marker
    val films = Seq(F(1702789, "Royal Ballet & Opera 2026/27: Romeo and Juliet", 2027, "", 0, 1),
      F(6003, "Romeo and Juliet", 1968, "Franco Zeffirelli", 138, 30))
    val ls = Seq(Multikino, Helios).map(listing(_, "Royal Ballet and Opera: Romeo and Juliet", None, Some("Kenneth MacMillan"), Some(170)))
    val r = resolve(ls, films)
    ls.foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe Some(1702789)))
  }

  it should "stay apart from another season's broadcast of the same work, and not take its record" in {
    // The Royal Ballet's 2024/25 "The Nutcracker" has a record; its 2026/27 one does not yet. A
    // bare "The Nutcracker" is a segment of both seasons' titles.
    val films = Seq(F(1300037, "Royal Ballet & Opera 2024/25: The Nutcracker", 2024, "", 0, 3),
      F(149385, "The Nutcracker", 1985, "Carroll Ballard", 89, 20))
    val old  = listing(Multikino, "Royal Ballet & Opera 2024/25: The Nutcracker")
    val next = Seq(Helios, KinoApollo).map(listing(_, "RBO Cinema Season 2026-27: The Nutcracker"))
    val bare = listing(Rialto, "The Nutcracker")
    val r = resolve(Seq(old, bare) ++ next, films)
    r.decisionOf(old.key).film shouldBe Some(1300037)
    next.foreach { l =>
      withClue(r.decisionOf(l.key).render) { Seq(Some(1300037), Some(149385)) should not contain r.decisionOf(l.key).film }
      together(r, old, l) shouldBe false
    }
    r.violations shouldBe 0
  }

  it should "take neither of two houses' records of its work in its season when nothing it publishes tells them apart" in {
    val films = Seq(F(1, "The Metropolitan Opera 2026/27: Carmen", 2027, "", 0, 5),
      F(2, "Royal Ballet & Opera 2026/27: Carmen", 2027, "", 0, 3))
    val opera = listing(Multikino, "OPERA 2026/2027 - CARMEN")
    val r = resolve(Seq(opera), films)
    withClue(r.decisionOf(opera.key).render)(r.decisionOf(opera.key).film shouldBe None)
  }

  it should "not take another house's record of its work, when its banner's other works name its own house" in {
    // UK venues list the Royal Ballet & Opera season as "RBO Cinema Season 2026-27: …"; TMDB files
    // RBO's Swan Lake and Alice, but its Manon only under the Met. Which house a banner is, is what
    // its other works' records say — never a list of houses.
    val films = Seq(F(1702782, "Royal Ballet & Opera 2026/27: Swan Lake", 2027, "", 0, 3),
      F(1702778, "Royal Ballet & Opera 2026/27: Alice's Adventures in Wonderland", 2027, "", 0, 3),
      F(1703631, "The Metropolitan Opera 2026/27: Manon", 2027, "", 0, 5),
      F(1703622, "The Metropolitan Opera 2026/27: Macbeth", 2026, "", 0, 5))
    def rbo(work: String) = Seq(Helios, KinoApollo).map(listing(_, s"RBO Cinema Season 2026-27: $work"))
    def met(work: String) = Seq(Multikino, Rialto).map(listing(_, s"Met Opera 2026-27: $work"))
    val (swan, alice, rboManon) = (rbo("Swan Lake"), rbo("Alice's Adventures in Wonderland"), rbo("Manon"))
    val (metManon, metMacbeth)  = (met("Manon"), met("Macbeth"))
    val r = resolve(swan ++ alice ++ rboManon ++ metManon ++ metMacbeth, films)
    swan.map(l => r.decisionOf(l.key).film) shouldBe Seq.fill(2)(Some(1702782))
    alice.map(l => r.decisionOf(l.key).film) shouldBe Seq.fill(2)(Some(1702778))
    metManon.map(l => r.decisionOf(l.key).film) shouldBe Seq.fill(2)(Some(1703631))
    rboManon.foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film should not be Some(1703631)))
    together(r, rboManon.head, metManon.head) shouldBe false
    r.violations shouldBe 0
  }

  it should "take its own house's record of the work once the database returns it" in {
    // TMDB does file RBO's 2026/27 Manon; a search for the work and its season returns both houses'.
    val films = Seq(F(1702782, "Royal Ballet & Opera 2026/27: Swan Lake", 2027, "", 0, 3),
      F(1702778, "Royal Ballet & Opera 2026/27: Alice's Adventures in Wonderland", 2027, "", 0, 3),
      F(1702757, "Royal Ballet & Opera 2026/27: Manon", 2026, "", 0, 1),
      F(1703631, "The Metropolitan Opera 2026/27: Manon", 2027, "", 0, 5))
    def rbo(work: String) = Seq(Helios, KinoApollo).map(listing(_, s"RBO Cinema Season 2026-27: $work"))
    val rboManon = rbo("Manon")
    val r = resolve(rbo("Swan Lake") ++ rbo("Alice's Adventures in Wonderland") ++ rboManon, films)
    rboManon.foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe Some(1702757)))
    r.violations shouldBe 0
  }

  "A house spelled unlike its records" should "take the house's record of the work, as its banner's other works do" in {
    // US and UK venues bill the National Theatre's broadcasts "NT Live: …"; TMDB files them as
    // "National Theatre Live: …". Which house "NT Live" is, is what its works' records say — the
    // same house bills every one of them — never a list of houses or of abbreviations.
    val films = Seq(F(1352026, "National Theatre Live: The Importance of Being Earnest", 2025, "", 0, 2),
      F(1401957, "National Theatre Live: Dr. Strangelove", 2025, "", 0, 2),
      F(1598661, "National Theatre Live: The Playboy of the Western World", 2025, "", 170, 2),
      F(36019, "The Playboy of the Western World", 1962, "Brian Desmond Hurst", 100, 5))
    def nt(work: String, runtime: Option[Int] = None) = Seq(Helios, KinoApollo).map(listing(_, s"NT Live: $work", runtime = runtime))
    val (earnest, strangelove, playboy) = (nt("The Importance of Being Earnest"), nt("Dr. Strangelove"),
      nt("The Playboy of the Western World", Some(172)))
    val hurst = listing(Rialto, "The Playboy of the Western World", Some(1962), Some("Brian Desmond Hurst"))
    val r = resolve(earnest ++ strangelove ++ playboy :+ hurst, films)
    val shown = (earnest ++ strangelove ++ playboy).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")
    withClue(shown) {
      earnest.map(l => r.decisionOf(l.key).film) shouldBe Seq.fill(2)(Some(1352026))
      strangelove.map(l => r.decisionOf(l.key).film) shouldBe Seq.fill(2)(Some(1401957))
      playboy.map(l => r.decisionOf(l.key).film) shouldBe Seq.fill(2)(Some(1598661))
    }
    r.decisionOf(hurst.key).film shouldBe Some(36019)
    r.violations shouldBe 0
  }

  it should "not take a house's record for a banner only one of whose works that house bills" in {
    // A programme banner is not a house because one of its films has a house's record: "Throwback"
    // shows Hurst's Playboy, and the National Theatre's record of the play is another film.
    val films = Seq(F(1598661, "National Theatre Live: The Playboy of the Western World", 2025, "", 170, 2),
      F(36019, "The Playboy of the Western World", 1962, "Brian Desmond Hurst", 100, 5))
    val throwback = Seq(Helios, KinoApollo).map(listing(_, "Throwback: The Playboy of the Western World"))
    val r = resolve(throwback, films)
    throwback.foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film should not be Some(1598661)))
  }

  "A banner spelling its house" should "be that house, though another house's records bill more of its works" in {
    // PL: "Carmen | metropolitan opera: live in hd 2026/27". TMDB files no Met Carmen this season,
    // only RBO's, and both houses' Così: by co-occurrence the banner would be RBO's. Its own words
    // name the Metropolitan Opera — the house whose banner shares words no rival's does.
    val films = Seq(F(1703620, "The Metropolitan Opera 2026/27: Così fan tutte", 2026, "", 0, 5),
      F(1702775, "Royal Ballet & Opera 2026/27: Così fan tutte", 2026, "", 0, 3),
      F(1702759, "Royal Ballet & Opera 2026/27: Carmen", 2026, "", 0, 3))
    val cosi   = listing(Multikino, "Cosi fan tutte | metropolitan opera: live in hd 2026/27")
    val carmen = listing(Multikino, "Carmen | metropolitan opera: live in hd 2026/27")
    val r = resolve(Seq(cosi, carmen), films)
    withClue(r.decisionOf(cosi.key).render)(r.decisionOf(cosi.key).film shouldBe Some(1703620))
    withClue(r.decisionOf(carmen.key).render)(r.decisionOf(carmen.key).film should not be Some(1702759))
    r.violations shouldBe 0
  }

  "Three films under one title" should "stay three, and a bare listing joins neither of the dated ones by title alone" in {
    val films = Seq(F(1954, "A Star Is Born", 1954, "George Cukor", 176), F(1976, "A Star Is Born", 1976, "Frank Pierson", 139),
      F(2018, "A Star Is Born", 2018, "Bradley Cooper", 136, 60))
    val old  = listing(Multikino, "A Star Is Born", Some(1954), Some("George Cukor"))
    val mid  = listing(Rialto, "A Star Is Born", Some(1976), Some("Frank Pierson"))
    val cur  = Seq(Helios, KinoApollo).map(listing(_, "A Star Is Born", Some(2018), Some("Bradley Cooper")))
    val bare = listing(KinoMuza, "A Star Is Born")
    val r = resolve(Seq(old, mid, bare) ++ cur, films)
    Seq(old, mid, cur.head).map(l => r.decisionOf(l.key).film) shouldBe Seq(Some(1954), Some(1976), Some(2018))
    together(r, old, cur.head) shouldBe false
    together(r, bare, old) shouldBe false
    together(r, bare, mid) shouldBe false
    r.violations shouldBe 0
  }

  "A sequel's listing" should "take the sequel, not the original its title's words decorate" in {
    // US, the recorded "The Texas Chainsaw Massacre 2" ×4 (labelled 16337; old pipeline and
    // resolver both filed it on the 1974 original): every venue credits Tobe Hooper, who directed
    // both, so the venues' own facts "backed" the original its title decorates.
    // TMDB's records, with the alternative titles that decide the title relations: the original's
    // "The Texas Chainsaw Massacre" (which the sequel's title decorates) and the sequel's "The
    // Texas Chainsaw Massacre 2 - UC" (of which the listing's title is a fragment).
    val films = Seq(F(30497, "The Texas Chain Saw Massacre", 1974, "Tobe Hooper", 83, 12, Seq("Leatherface", "The Texas Chainsaw Massacre")),
      F(16337, "The Texas Chainsaw Massacre Part 2", 1986, "Tobe Hooper", 100, 12.75, Seq("The Texas Chainsaw Massacre 2 - UC", "TCM 2")),
      F(609, "Poltergeist", 1982, "Tobe Hooper", 114, 30))
    val sequel = Seq(Multikino, Helios, KinoApollo, KinoMuza).map(listing(_, "The Texas Chainsaw Massacre 2", None, Some("Tobe Hooper"), Some(101)))
    // The title search finds the sequel; the director's filmography reaches the original.
    val r = IdentityResolver.resolve(sequel, new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    withClue(r.decisionOf(sequel.head.key).render) {
      sequel.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(16337))
    }
  }

  it should "take the instalment its number names over an earlier one whose crew it credits" in {
    // PL, Kinoteka's recorded "Niesamowite przygody skarpetek 4. Do roboty! – zestaw | Młode
    // Horyzonty" (original title the part-4 record's, 55 minutes, crediting animators): TMDB credits
    // them on the 2025 first set, which the resolver took on that credit; part 4's record (2026, 55
    // minutes) credits nobody. The title numbers part 4 as its record does.
    val films = Seq(F(1447108, "Niesamowite przygody skarpetek", 2025, "Mateusz Kmieć", 55, 1.2),
      F(1735319, "Niesamowite przygody skarpetek 4. Do roboty!", 2026, "", 55, 0.8))
    val set = listing(Multikino, "Niesamowite przygody skarpetek 4. Do roboty! – zestaw | Młode Horyzonty", None, Some("Mateusz Kmieć"), Some(55))
      .copy(originalTitle = Some("Niesamowite przygody skarpetek 4. Do roboty!"))
    val d = shipped(Seq(set), films).decisionOf(set.key)
    withClue(d.render)(d.film shouldBe Some(1735319))
    // The first set's own listing keeps it: no record numbers an instalment its title does not.
    val first = listing(Helios, "Niesamowite przygody skarpetek", None, Some("Mateusz Kmieć"), Some(55))
    shipped(Seq(first), films).decisionOf(first.key).film shouldBe Some(1447108)
  }

  it should "leave a title whose number is its name, and a remake, on their films" in {
    val films = Seq(F(844, "2046", 2004, "Wong Kar-wai", 129), F(9, "9 to 5", 1980, "Colin Higgins", 109),
      F(1977, "Suspiria", 1977, "Dario Argento", 99, 20), F(2018, "Suspiria", 2018, "Luca Guadagnino", 152, 15))
    val listings = Seq(listing(Multikino, "2046", Some(2004), Some("Wong Kar-wai")), listing(Helios, "9 to 5", Some(1980), Some("Colin Higgins")),
      listing(KinoApollo, "Suspiria", Some(2018), Some("Luca Guadagnino")), listing(KinoMuza, "Suspiria", Some(1977), Some("Dario Argento")))
    val r = resolve(listings, films)
    listings.map(l => r.decisionOf(l.key).film) shouldBe Seq(Some(844), Some(9), Some(2018), Some(1977))
  }

  "A listing that publishes only its title" should "follow its title family's clear majority film, not the database's ranking" in {
    // US Landmark Ritz Five's bare "Sense and Sensibility" (and UK Everyman's "Relaxed Screening:
    // …"): the credited siblings are split between the current release and Ang Lee's 1995 film,
    // which TMDB ranks first. The bare listing's own vote took the 1995 film on that ranking.
    val films = Seq(F(4584, "Sense and Sensibility", 1995, "Ang Lee", 136, 20), F(1503762, "Sense and Sensibility", 2026, "Georgia Oakley", 132, 10))
    val current = Seq(Helios, KinoApollo, Rialto, CharlieMonroe, CinemaCityKinepolis, KinoPalacowe, CinemaCityPoznanPlaza,
      CinemaCityWroclavia, CinemaCityKorona).map(listing(_, "Sense and Sensibility", Some(2026), Some("Georgia Oakley")))
    val lee     = listing(Multikino, "Sense and Sensibility", Some(1995), Some("Ang Lee"))
    val bare    = listing(KinoMuza, "Sense and Sensibility")
    val relaxed = listing(KinoBulgarska, "Relaxed Screening: Sense and Sensibility")
    val r = resolve(current ++ Seq(lee, bare, relaxed), films)
    withClue((current.head +: Seq(lee, bare, relaxed)).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      r.decisionOf(current.head.key).film shouldBe Some(1503762)
      r.decisionOf(lee.key).film shouldBe Some(4584)
      r.decisionOf(bare.key).film shouldBe Some(1503762)
      r.decisionOf(relaxed.key).film shouldBe Some(1503762)
    }
    r.violations shouldBe 0
  }

  it should "keep its own vote when the family is too thin to outweigh it" in {
    // UK, 78 Cineworld venues' bare "The Omen" (the 1976 film's 50th-anniversary re-release)
    // beside 2 venues crediting Donner's 1976 film and 4 crediting the 2006 remake: 4 of the
    // family's 84 venues is no majority, and the bare listings vote on their own evidence.
    val films = Seq(F(794, "The Omen", 1976, "Richard Donner", 111, 20), F(806, "The Omen", 2006, "John Moore", 110, 10))
    val donner   = Seq(Multikino, Rialto).map(listing(_, "The Omen", Some(1976), Some("Richard Donner")))
    val moore    = Seq(Helios, KinoApollo, CinemaCityKinepolis, KinoPalacowe).map(listing(_, "The Omen", Some(2006), Some("John Moore")))
    val chain    = Seq(KinoMuza, KinoBulgarska, CharlieMonroe, CinemaCityPoznanPlaza, CinemaCityWroclavia, CinemaCityKorona,
      MultikinoPasazGrunwaldzki, HeliosMagnolia, HeliosAlejaBielany).map(listing(_, "The Omen"))
    val r = resolve(donner ++ moore ++ chain, films)
    withClue(r.decisionOf(chain.head.key).render) {
      r.decisionOf(donner.head.key).film shouldBe Some(794)
      r.decisionOf(moore.head.key).film shouldBe Some(806)
      chain.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(794))
    }
    r.violations shouldBe 0
  }

  it should "not take a house's season production from siblings whose titles name the season it does not" in {
    // PL, Kino Amok's bare "Manon" (it lists the Met's broadcasts bare) beside 16 Multikino
    // venues' "Royal Ballet and Opera Sezon Kinowy 2026-27: Manon" and Kino Kijów's "OPERA
    // 2026/2027 - MANON" (the Met's): a season names a house's production, and a bare title
    // names no house, so the siblings' venues say nothing about which one it is.
    val films = Seq(F(1702757, "Royal Ballet & Opera 2026/27: Manon", 2026, "", 0, 1), F(1703631, "The Metropolitan Opera 2026/27: Manon", 2027, "", 0, 1),
      F(132332, "Manon", 1949, "Henri-Georges Clouzot", 100, 3), F(2, "Manon", 2013, "Someone Else", 90, 2), F(3, "Manon", 1974, "A Third", 95, 1))
    val rbo  = Seq(Multikino, Helios, KinoApollo, Rialto, CharlieMonroe, CinemaCityKinepolis, KinoPalacowe, CinemaCityPoznanPlaza, CinemaCityWroclavia)
      .map(listing(_, "Royal Ballet and Opera Sezon Kinowy 2026-27: Manon"))
    val met  = listing(CinemaCityKorona, "The Metropolitan Opera 2026/27: Manon")
    val bare = listing(KinoMuza, "Manon")
    val r = IdentityResolver.resolve(rbo ++ Seq(met, bare), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    withClue((Seq(rbo.head, met, bare)).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      r.decisionOf(rbo.head.key).film shouldBe Some(1702757)
      r.decisionOf(met.key).film shouldBe Some(1703631)
      r.decisionOf(bare.key).film should not be Some(1702757)
    }
  }

  "Two listings stating years decades apart" should "stay apart when no film ties them: 'It (1990)' is not 'IT (2017)'" in {
    val films = Seq(F(346364, "It", 2017, "Andy Muschietti", 135, 90))
    val old = listing(Multikino, "It (1990)", Some(1990))
    val cur = listing(Helios, "IT (2017)", Some(2017))
    val r = resolve(Seq(old, cur), films)
    r.decisionOf(cur.key).film shouldBe Some(346364)
    together(r, old, cur) shouldBe false
  }

  "A title contained in another" should "not join it: 'Zärtlich kreist die Faust' is not Murnau's 'Faust'" in {
    val films = Seq(F(10728, "Faust", 1926, "F.W. Murnau", 116), F(5, "Zärtlich kreist die Faust", 1990, "Christoph Böll", 90))
    val faust = listing(Multikino, "Faust", Some(1926), Some("F.W. Murnau"))
    val other = listing(Helios, "Zärtlich kreist die Faust", Some(1990))
    val r = resolve(Seq(faust, other), films)
    together(r, faust, other) shouldBe false
    r.decisionOf(faust.key).film shouldBe Some(10728)
  }

  "'It Ends with Us'" should "not land on 'It Ends'" in {
    val films = Seq(F(1079091, "It Ends with Us", 2024, "Justin Baldoni", 130, 80), F(1422011, "It Ends", 2025, "Alexander Ullom", 89, 2))
    val us   = listing(Multikino, "It Ends with Us", Some(2024), Some("Justin Baldoni"), Some(130))
    val ends = listing(Helios, "It Ends", Some(2025), Some("Alexander Ullom"), Some(89))
    val r = resolve(Seq(us, ends), films)
    together(r, us, ends) shouldBe false
    r.decisionOf(us.key).film shouldBe Some(1079091)
    r.decisionOf(ends.key).film shouldBe Some(1422011)
  }

  "Group-level voting" should "match a cluster by its members' POOLED evidence when no member's own evidence can" in {
    // One member publishes only the year (four films that year), the other only the runtime
    // (two films that length, decades apart). Together they name one film.
    val films = Seq(F(1, "Pressure", 2025, "Anthony Maras", 120, 20), F(2, "Pressure", 2025, "Someone Else", 90, 40),
      F(5, "Pressure", 2025, "A Third", 100, 20), F(6, "Pressure", 2025, "A Fourth", 95, 20),
      F(3, "Pressure", 1990, "Third Person", 120, 30))
    val byYear    = listing(Multikino, "Pressure", Some(2025))
    val byRuntime = listing(Helios, "Pressure", runtime = Some(120))
    val r = resolve(Seq(byYear, byRuntime), films)
    together(r, byYear, byRuntime) shouldBe true
    r.decisionOf(byYear.key).film shouldBe Some(1)
    r.decisionOf(byYear.key).basis shouldBe ResolverDecision.Basis.PooledMatch
    val off = IdentityResolver.resolveWith(Seq(byYear, byRuntime), new FilmTable(films, normalizer), normalizer, weights, IdentityResolver.Mutation.NoVoting)
    off.decisionOf(byYear.key).film shouldBe None
  }

  /** Candyman, which TMDB's search does not return: a credited listing and two bare siblings, and
   *  the two other films walking Bernard Rose's filmography reaches. */
  private object Candyman {
    val walked   = Seq(F(353927, "Inside Out 4", 1992, "Bernard Rose", 99, 3), F(2, "Paperhouse", 1988, "Bernard Rose", 92, 5))
    val credited = listing(Multikino, "Candyman (1992)", director = Some("Bernard Rose"))
    val bare     = Seq(Helios, KinoApollo).map(listing(_, "Candyman"))
  }

  "The pooled vote" should "not choose a walked film whose rival the listing's own facts fit better" in {
    // An event whose title only mentions the film it screens, crediting the director and a
    // runtime: TMDB's search names nothing, the director's filmography reaches two films. The
    // listing's title shares a word with the less popular one; popularity alone must not hand it
    // the other.
    val walked = Seq(F(108, "Blue", 1993, "Krzysztof Kieślowski", 100, 3), F(110, "Red", 1994, "Krzysztof Kieślowski", 99, 40))
    val event  = listing(KinoMuza, "Cinematographers: a Blue evening", director = Some("Krzysztof Kieślowski"), runtime = Some(100))
    val r = resolve(Seq(event), walked)
    withClue(r.decisionOf(event.key).render)(r.decisionOf(event.key).film should not be Some(110))
  }

  it should "still take a walked film its facts fit as well as any rival's, when the calibration rates it higher" in {
    // PL, the Polish title of a documentary TMDB's search does not return: year and director fit
    // both of the director's films that year; the calibration (their standing) prefers one.
    val walked = Seq(F(431444, "The Curious World of Hieronymus Bosch", 2016, "David Bickerstaff", 90, 8),
      F(381710, "Goya: Visions of Flesh and Blood", 2016, "David Bickerstaff", 90, 3))
    val credited = listing(KinoMuza, "Osobliwy świat Hieronymusa Boscha", Some(2016), Some("David Bickerstaff"), Some(87))
    val r = resolve(Seq(credited), walked)
    withClue(r.decisionOf(credited.key).render)(r.decisionOf(credited.key).film shouldBe Some(431444))
  }

  it should "still take the one walked film the pooled facts single out, though no title names it" in {
    // PL, the Polish title of a foreign film TMDB's search does not return: one listing credits the
    // director (whose two films its facts alone cannot tell apart), a sibling publishes the year.
    // Pooled, only one of the director's films fits — the walk reached it, the facts chose it.
    val films    = Seq(F(677558, "The Last Whale Singer", 2025, "Reza Memari", 91, 5), F(2, "Jonah's Voyage", 2012, "Reza Memari", 91, 8))
    val credited = listing(Multikino, "Vincent. Legenda oceanu", director = Some("Reza Memari"))
    val dated    = listing(Helios, "Vincent. Legenda oceanu", Some(2025))
    val r = resolve(Seq(credited, dated), films)
    withClue(r.decisionOf(credited.key).render) {
      Seq(credited, dated).map(l => r.decisionOf(l.key).film) shouldBe Seq.fill(2)(Some(677558))
      r.decisionOf(credited.key).basis shouldBe ResolverDecision.Basis.PooledMatch
    }
  }

  it should "not choose a film only a director's filmography reached (known issue)" in {
    // TMDB's search has no "Candyman"; walking Bernard Rose's filmography turns up two other films,
    // whose director agrees with the credited listing and whose titles name nothing. The listing's
    // facts fit both equally (a title's bracket year only agrees, never denies), so the calibration's
    // preference decides — the same shape that rightly picks a documentary among its director's
    // films of one year. Telling them apart needs the bracket year to deny, which it does not yet.
    import Candyman.*
    val r = resolve(credited +: bare, walked)
    pendingUntilFixed {
      (credited +: bare).foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe None))
    }
  }

  it should "find the film the title names once the database has it, whatever the walk reached" in {
    import Candyman.*
    val named = resolve(credited +: bare, walked :+ F(9529, "Candyman", 1992, "Bernard Rose", 99, 20))
    (credited +: bare).map(l => named.decisionOf(l.key).film) shouldBe Seq.fill(3)(Some(9529))
    named.violations shouldBe 0
  }

  "The database's ranking priors" should "lend confidence to a film the listing's own facts pick, never withdraw it" in {
    // The venue credits the director and nothing else; the film is the least popular of
    // ten same-titled ones, ranks last in TMDB's search and no other venue lists it. Every signal
    // against it is TMDB's ranking or the family's count — none is a fact — and the facts rule the
    // nine namesakes out, so the ranking is no reason to leave the listing unmatched.
    val films = F(1, "Solo", 2019, "Hugo Stuven", 98, 0.2) +:
      (2 to 10).map(i => F(i, "Solo", 1960 + 6 * i, s"Director $i", 80 + 5 * i, 10.0 * i))
    val credited = listing(Rialto, "Solo", director = Some("Hugo Stuven"))
    val d = IdentityResolver.resolve(Seq(credited), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver).decisionOf(credited.key)
    withClue(d.render) {
      d.film shouldBe Some(1)
      d.basis shouldBe ResolverDecision.Basis.OwnMatch
      IdentityCalibration.resolver.showsRatings(d.confidence) shouldBe true
      d.explanation.head should include ("ranking priors lending")
    }
  }

  it should "still leave a listing unmatched when they are all that separates two namesakes its facts fit alike" in {
    // Both films are 2019 and the listing publishes only the year: its facts cannot tell them
    // apart, so TMDB's ranking is the only separator, and it keeps its full weight.
    val films = Seq(F(1, "Solo", 2019, "Hugo Stuven", 98, 0.2), F(2, "Solo", 2019, "Someone Else", 90, 0.3),
      F(3, "Solo", 1996, "Norberto Barba", 94, 30), F(4, "Solo", 1972, "A Third", 88, 20))
    val dated = listing(Rialto, "Solo", year = Some(2019))
    val d = IdentityResolver.resolve(Seq(dated), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver).decisionOf(dated.key)
    withClue(d.render)(d.film shouldBe None)
  }

  /** A film TMDB holds no record of — PL "Kuźma", "Superfutrzak" — is taken from the FALLBACK source when no TMDB film
   *  was, and only on a fact the listing publishes: its director, or its very year. A bare title, a year against the
   *  record, or a TMDB film of that title its facts leave standing falls back to nothing; of two IMDb namesakes, the one
   *  its year names. */
  "A listing no TMDB film was taken for" should "fall back to the IMDb film its facts corroborate, and to none otherwise" in {
    val kuzma = F(9900001, "Kuźma", 2025, "Anna Nowak", 88, imdbOnly = true)
    def fallbackOf(l: Listing, films: Seq[F]) = { val d = resolve(Seq(l), films).decisionOf(l.key); withClue(d.render)(d.film shouldBe None); d.fallback.map(_.id) }
    fallbackOf(listing(KinoMuza, "Kuźma", director = Some("Anna Nowak")), Seq(kuzma)) shouldBe Some("tt9900001")
    fallbackOf(listing(KinoMuza, "Kuźma", year = Some(2025)), Seq(kuzma)) shouldBe Some("tt9900001")
    fallbackOf(listing(KinoMuza, "Kuźma"), Seq(kuzma)) shouldBe None
    fallbackOf(listing(KinoMuza, "Kuźma", year = Some(1990)), Seq(kuzma)) shouldBe None
    fallbackOf(listing(KinoMuza, "Kuźma", director = Some("Anna Nowak")), Seq(kuzma, F(41, "Kuźma", 1970, "", 0), F(42, "Kuźma", 1981, "", 0))) shouldBe None
    val older = F(9800002, "Kuźma", 1999, "Jan Kowalski", 95, imdbOnly = true)
    fallbackOf(listing(KinoMuza, "Kuźma", year = Some(1999)), Seq(kuzma, older)) shouldBe Some("tt9800002")
  }

  /** A fallback film whose IMDb record is not answered yet is only its suggestion's title and year — no credit, no
   *  running time to rule it out — so nothing falls back to it until its record is filed (the fill asks it; its filing
   *  re-resolves the family). Prod's first ticks after the deploy held every IMDb record unasked. */
  it should "fall back to no film whose IMDb record is not answered yet" in {
    val kuzma = F(9900001, "Kuźma", 2025, "Anna Nowak", 88, imdbOnly = true)
    val table = new FilmTable(Seq(kuzma), normalizer)
    val unanswered = new IdentityLookups {
      def hasDetail(l: Listing): Boolean                  = table.hasDetail(l)
      def detail(l: Listing): Answer[Option[DetailFacts]] = table.detail(l)
      def candidates(q: CandidateQuery): Answer[Seq[Hit]] = table.candidates(q)
      def film(id: Int): Answer[Option[IdentityMeasures.Film]] = if (FallbackIds.isFallback(id)) Answer.Unknown else table.film(id)
    }
    val bare = listing(KinoMuza, "Kuźma", year = Some(2025))
    val d = IdentityResolver.resolve(Seq(bare), unanswered, normalizer, weights).decisionOf(bare.key)
    withClue(d.render)((d.film, d.fallback) shouldBe ((None, None)))
  }

  /** UK Cineworld's "Royal Ballet and Opera: Tosca" ×123 (Oliver Mears) fell back to IMDb's record of the 2025/26
   *  production — which TMDB holds too, as "Royal Ballet & Opera 2025/26: Tosca", unlinked to IMDb's id — while its own
   *  runtime left that TMDB film below the cut. A TMDB film the title names, or crediting its director, that no fact rules
   *  out is one TMDB knows the listing may be: its rules not taking it is no licence to take a fallback in its place. */
  it should "fall back to nothing while a TMDB film its title names or its director credits stands undenied" in {
    val imdbs = F(9810381, "Royal Ballet and Opera: Tosca", 2025, "Oliver Mears", 210, imdbOnly = true)
    val seasons = Seq(F(1482356, "Royal Ballet & Opera 2025/26: Tosca", 2025, "Oliver Mears", 195, 0.3, searched = false),
      F(1702784, "Royal Ballet & Opera 2026/27: Tosca", 2027, "Oliver Mears", 205, 0.3, searched = false))
    val d = IdentityResolver.resolve(Seq(listing(KinoMuza, "Royal Ballet and Opera: Tosca", director = Some("Oliver Mears"), runtime = Some(210))),
      new FilmTable(imdbs +: seasons, normalizer), normalizer, IdentityCalibration.resolver).decisions.head
    withClue(d.render)((d.film, d.fallback) shouldBe ((None, None)))
  }

  /** A no-match whose own evidence still prefers one film — below the rating cut, its facts favouring it over every
   *  namesake — names it as the film it LEANS to, the one a card may keep an earlier answer's ratings of: PL "Tatarak"
   *  read 18.4% for Wajda's 2009 film beside the 1965 one. Two namesakes only an IMDb lookup found, alike in all but
   *  their credit, or a double bill, lean to none. */
  "A no-match" should "name the one film its evidence leans to, and none on a tie or a double bill" in {
    val films = Seq(F(1, "Tatarak", 2009, "Andrzej Wajda", 85, 1), F(2, "Tatarak", 1965, "Someone Else", 20, 0.3),
      F(3, "Lalka", 1968, "Wojciech Has", 159, 1, searched = false), F(4, "Lalka", 1968, "Another Has", 150, 1, searched = false))
    val lean   = listing(KinoMuza, "Tatarak")
    val tie    = listing(KinoMuza, "Lalka")
    val bill   = listing(KinoMuza, "Tatarak + Lalka")
    val r      = IdentityResolver.resolve(Seq(lean, tie, bill), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    def of(l: Listing) = { val d = r.decisionOf(l.key); withClue(d.render)(d.film shouldBe None); d }
    of(lean).leaning shouldBe Some(ResolverDecision.Leaning(1, 1))
    of(tie).leaning shouldBe None
    of(bill).leaning shouldBe None
  }

  /** PL Kino 1410 bills the Met's relays "Carmen | metropolitan opera: live in hd 2026/27": TMDB holds no Met 2026/27
   *  Carmen, but Royal Ballet & Opera's of that season, and the banner — learned only from the RBO records its works
   *  reached — was RBO's, so its season production was taken. Its "Makbet" bills the Met's "Macbeth" in another language
   *  ([[IdentityMeasures.stageBilling]]): the banner is the Met, and RBO's Carmen another house's production. */
  "A season relay's banner" should "be the house it bills a stage work under in another language, not the other house of its works" in {
    val films = Seq(F(1702759, "Royal Ballet & Opera 2026/27: Carmen", 2026, "Damiano Michieletto", 200, 0.5),
      F(1703622, "The Metropolitan Opera 2026/27: Macbeth", 2026, "", 0, 0.5))
    val carmen = listing(KinoMuza, "Carmen | metropolitan opera: live in hd 2026/27")
    val makbet = listing(KinoMuza, "Makbet | metropolitan opera: live in hd 2026/27")
    val r = IdentityResolver.resolve(Seq(carmen, makbet), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    withClue(r.decisionOf(makbet.key).render)(r.decisionOf(makbet.key).film shouldBe Some(1703622))
    withClue(r.decisionOf(carmen.key).render)(r.decisionOf(carmen.key).film shouldBe None)
  }

  "A listing the evidence cannot place" should "stay unmatched, and say which candidate it refused" in {
    val films = Seq(F(1, "Opętanie", 1981, "Andrzej Żuławski", 124), F(2, "Opętanie", 1973, "Someone Else", 90))
    val bare = listing(Multikino, "Opętanie")
    val d = resolve(Seq(bare), films).decisionOf(bare.key)
    d.film shouldBe None
    d.basis shouldBe ResolverDecision.Basis.BelowThreshold
    d.explanation.exists(_.startsWith("best rejected candidate")) shouldBe true
  }

  private def shipped(listings: Seq[Listing], films: Seq[F]): Resolution =
    IdentityResolver.resolve(listings, new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)

  "A director credited in another script" should "be the listing's when its person search, by the listing's own spelling, reaches the film" in {
    // PL "Spragnieni miłości" [2000] {Wong Kar Wai}: TMDB's Polish credits write him 王家衛 (film 843, recording
    // 36807940234); transliterated, that reads "Wang Jiawei", a different person, and vetoed the film. TMDB's person
    // search for "Wong Kar Wai" finds him and his filmography holds 843. PL, DE and ES held 41 listings so.
    val mood   = F(843, "Spragnieni miłości", 2000, "王家衛", 98, directorAliases = Seq("Wong Kar Wai"))
    val rival  = F(9001, "Spragnieni miłości", 1987, "Someone Else", 90, 3)
    val credited = listing(Rialto, "Spragnieni miłości", Some(2000), Some("Wong Kar Wai"), Some(98))
    val d = shipped(Seq(credited), Seq(mood, rival)).decisionOf(credited.key)
    withClue(d.render)(d.film shouldBe Some(843))
    // only a script gap the person search bridges moves: a film its search does not reach, or two Latin names
    // that differ, stay what they were
    import IdentityMeasures.Category
    IdentityMeasures.creditedBySearch(Map("director" -> Category("different_script")), directedBySpelling = false) shouldBe
      Map("director" -> Category("different_script"))
    IdentityMeasures.creditedBySearch(Map("director" -> Category("different")), directedBySpelling = true) shouldBe
      Map("director" -> Category("different"))
    IdentityMeasures.creditedBySearch(Map("director" -> Category("incomparable")), directedBySpelling = true) shouldBe
      Map("director" -> Category("same_person"))
  }

  "A double bill crediting both its works' directors" should "be neither film, as is the same bill crediting one" in {
    // UK, 2026-10-02: "We're Going on a Bear Hunt + The Tiger Who Came to Tea" {Joanna Harrison, Robin Shaw} ×133 was
    // joined by its title to the same bill crediting Harrison alone — which IS Bear Hunt — and took that film.
    val films = Seq(F(431591, "We're Going on a Bear Hunt", 2016, "Joanna Harrison", 32), F(644120, "The Tiger Who Came to Tea", 2019, "Robin Shaw", 24))
    val both  = Seq(Rialto, KinoApollo).map(v => listing(v, "We're Going on a Bear Hunt + The Tiger Who Came to Tea").copy(directors = Seq("Joanna Harrison", "Robin Shaw")))
    val one   = Seq(Multikino, KinoMikro).map(v => listing(v, "We're Going on a Bear Hunt + The Tiger Who Came to Tea", director = Some("Joanna Harrison")))
    val r = shipped(both ++ one, films)
    withClue((both ++ one).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      // crediting one work's director leaves it a double programme: neither film either (user rule)
      one.map(l => r.decisionOf(l.key).film) shouldBe Seq(None, None)
      both.map(l => r.decisionOf(l.key).film) shouldBe Seq(None, None)
    }
  }

  "A double bill one of whose works no database holds" should "be neither film, while a film billed with a talk stays that film" in {
    // PL Kino Pałacowe, 2026-10-05: "Akademia Polskiego Filmu: (Wyobrażone) początki polskiego kina | Historia kina w
    // Popielawach + Pruska kultura" took the 1998 "Historia kina w Popielawach": TMDB holds no "Pruska kultura" (a
    // 1908 short), so the bill's second work was never a candidate and nothing vetoed the first. User rule: neither.
    val films = Seq(F(157320, "Historia kina w Popielawach", 1998, "Jan Jakub Kolski", 98, 2),
      F(42199, "Bez znieczulenia", 1978, "Andrzej Wajda", 131, 3))
    // its page credits the first work's director and year; Kinematograf bills that film alone, by the same facts
    val bill  = listing(Rialto, "Akademia Polskiego Filmu: (Wyobrażone) początki polskiego kina | Historia kina w Popielawach + Pruska kultura",
      Some(1998), Some("Jan Jakub Kolski"))
    val alone = listing(KinoMikro, "Akademia Polskiego Filmu: „Historia kina w Popielawach”", Some(1998), Some("Jan Jakub Kolski"))
    val talks = Seq(alone, listing(Multikino, "Wajda. Bez znieczulenia + prelekcja"), listing(KinoApollo, "Bez znieczulenia + spotkanie z Andrzejem Wajdą"),
      listing(Helios, "Kino bez barier: Bez znieczulenia (AD + CC + PJM)"))
    val r = shipped(bill +: talks, films)
    withClue((bill +: talks).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      r.decisionOf(bill.key).film shouldBe None
      talks.map(l => r.decisionOf(l.key).film) shouldBe Some(157320) +: Seq.fill(3)(Some(42199))
    }
    // a second work titled in numbers is a work too: Pałacowe's "… | Zakazane piosenki + 2 x 2 = 4"
    IdentityMeasures.billsTwoWholeWorks(IdentityMeasures.Listing("Od nowa | Zakazane piosenki + 2 x 2 = 4")) shouldBe true
    IdentityMeasures.billsTwoWholeWorks(IdentityMeasures.Listing("Bez znieczulenia + Q&A")) shouldBe false
  }

  "A double bill sharing its first work's search form" should "not join that work's bare listing when its second work is another listing's" in {
    // UK, 2026-10-01: "Toddler Club: Tabby McTat + Room on the Broom" {Various Directors} searches as "Tabby McTat";
    // joined to the bare "Tabby McTat" ×23, its directors vetoed the film for all of them. A bill whose second
    // part is a talk ("Wajda. Bez znieczulenia + prelekcja") still joins the film's own listings.
    val films = Seq(F(1205520, "Tabby McTat", 2024, "Jac Hamman", 27, 2), F(82390, "Room on the Broom", 2012, "Jan Lachauer", 27, 6),
      F(42199, "Bez znieczulenia", 1978, "Andrzej Wajda", 131, 3))
    val tabby = Seq(listing(Rialto, "Tabby McTat"), listing(KinoApollo, "Tabby McTat"))
    val bill  = listing(Multikino, "Toddler Club: Tabby McTat + Room on the Broom", director = Some("Various Directors"))
    val broom = listing(Rialto, "Room on the Broom")
    val wajda = Seq(listing(Rialto, "Bez znieczulenia", director = Some("Andrzej Wajda")), listing(KinoApollo, "Bez znieczulenia"),
      listing(Multikino, "Wajda. Bez znieczulenia + prelekcja"))
    val all = tabby ++ Seq(bill, broom) ++ wajda
    val r = shipped(all, films)
    withClue(all.map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      tabby.map(l => r.decisionOf(l.key).film) shouldBe Seq(Some(1205520), Some(1205520))
      wajda.map(l => r.decisionOf(l.key).film) shouldBe Seq.fill(3)(Some(42199))
    }
  }

  "The pooled vote" should "not take the film TMDB's ranking favours when the cluster's own facts favour another" in {
    // PL, "Camino dla opornych" at four venues: one publishes the original title "Santiago" and
    // 113 minutes, one only the runtime, two nothing. "Santiago" returns Gordon Douglas's 93-minute
    // 1956 film first and a 113-minute "Santiago!" fourth. Pooled, every fact the cluster publishes
    // fits the fourth better; only the ranking carried the first past the cut.
    val films = Seq(F(197704, "Santiago", 1956, "Gordon Douglas", 93, 3), F(801, "Santiago Calatrava", 2001, "A", 50, 2.8),
      F(802, "Santiago Bernabéu", 2010, "B", 45, 2.5), F(373623, "Santiago!", 1970, "Someone", 113, 1))
    val credited = listing(Helios, "Camino dla opornych - KNT", runtime = Some(113)).copy(originalTitle = Some("Santiago"))
    val timed    = listing(KinoApollo, "Camino dla opornych | CHKF", runtime = Some(113))
    val bare     = Seq(Multikino, Rialto).map(listing(_, "Camino dla opornych"))
    val r = shipped(credited +: timed +: bare, films)
    (credited +: timed +: bare).foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film should not be Some(197704)))
  }

  "A title piece naming the listing's own venue" should "not name a film, whatever TMDB's ranking says" in {
    // PL, Kino Twierdza bills its screenings "TWIERDZA - VINCENT. LEGENDA OCEANU": the venue's
    // name is a segment, and "Twierdza" is The Rock's Polish title. US, the Alamo Drafthouse
    // circuit's "Dismember the Alamo 2026 - Chicago" at its Chicago venue names the city, not the
    // musical. With nothing else to go on, the ranking must not pick either.
    val rock    = Seq(F(9802, "Twierdza", 1996, "Michael Bay", 137, 10), F(26198, "Twierdza", 1983, "Michael Mann", 96, 4))
    val billed  = listing(models.KinoTwierdza, "TWIERDZA - VINCENT. LEGENDA OCEANU")
    val bd      = shipped(Seq(billed), rock).decisionOf(billed.key)
    withClue(bd.render)(bd.film shouldBe None)
    val chicago = Seq(F(1574, "Chicago", 2002, "Rob Marshall", 113, 6), F(128298, "Chicago", 1927, "Frank Urson", 103, 1))
    val venue   = models.Cinema.all.find(c => models.City.forCinema(c).exists(_.labels.nominative == "Chicago")).get
    val circuit = listing(venue, "Dismember the Alamo 2026 - Chicago")
    val cd      = shipped(Seq(circuit), chicago).decisionOf(circuit.key)
    withClue(cd.render)(cd.film shouldBe None)
  }

  it should "still name the film elsewhere, and as the whole title at the venue itself" in {
    val chicago = Seq(F(1574, "Chicago", 2002, "Rob Marshall", 113, 6), F(128298, "Chicago", 1927, "Frank Urson", 103, 1))
    val here    = models.Cinema.all.find(c => models.City.forCinema(c).exists(_.labels.nominative == "Chicago")).get
    val banner  = listing(KinoMuza, "Dismember the Alamo 2026 - Chicago")
    val whole   = listing(here, "Chicago")
    shipped(Seq(banner), chicago).decisionOf(banner.key).film shouldBe Some(1574)
    shipped(Seq(whole), chicago).decisionOf(whole.key).film shouldBe Some(1574)
    // US, "Charlotte (2021)" at a Charlotte venue: the city and a year are the whole title.
    val charlotte = Seq(F(844135, "Charlotte", 2021, "Tahir Rana", 92, 6), F(2, "Charlotte", 1974, "Someone Else", 90, 1))
    val there     = models.Cinema.all.find(c => models.City.forCinema(c).exists(_.labels.nominative == "Charlotte")).get
    val dated     = listing(there, "Charlotte (2021)")
    withClue(shipped(Seq(dated), charlotte).decisionOf(dated.key).render)(shipped(Seq(dated), charlotte).decisionOf(dated.key).film shouldBe Some(844135))
  }

  "A title naming two films by disjoint pieces" should "take neither on the title alone, nor its plain piece's film" in {
    // PL, Blackhurst's horror "Dolly" is distributed as "Lalka", as Kawalski's "Lalka" is.
    // "Lalka (Dolly)" names Kawalski's film by one word and Blackhurst's by the other; it publishes
    // nothing else, so neither its own score nor the "Lalka" segment's must-link to Kawalski's
    // credited listings may decide between them.
    val films    = Seq(F(1321666, "Lalka", 2026, "Maciej Kawalski", 162, 3), F(1309083, "Dolly", 2026, "Rod Blackhurst", 83, 10),
      F(81315, "Lalka", 1968, "Wojciech Has", 152, 2.5))
    val credited = Seq(Multikino, Helios, KinoApollo).map(listing(_, "Lalka", Some(2026), Some("Maciej Kawalski"), Some(162)))
    val horror   = listing(Rialto, "Lalka (ale to horror)", Some(2025), Some("Rod Blackhurst"), Some(82))
    val both     = listing(KinoMuza, "Lalka (Dolly)")
    val r = shipped(credited ++ Seq(horror, both), films)
    withClue((credited ++ Seq(horror, both)).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      credited.foreach(l => r.decisionOf(l.key).film shouldBe Some(1321666))
      r.decisionOf(horror.key).film shouldBe Some(1309083)
      r.decisionOf(both.key).film should not be Some(1321666)
    }
    r.violations shouldBe 0
  }

  it should "take the gloss's horror when the listing credits nobody but its facts are the horror's" in {
    // PL, Cinema City's "Lalka (ale to horror)" ("Lalka (but it's the horror)"): 2025, 82 minutes,
    // no director. Only its year and runtime tell Blackhurst's 83-minute "Dolly" from Kawalski's
    // 162-minute "Lalka" credited everywhere else.
    val films    = Seq(F(1321666, "Lalka", 2026, "Maciej Kawalski", 162, 3), F(1309083, "Dolly", 2026, "Rod Blackhurst", 83, 10,
      alternatives = Seq("Lalka")), F(81315, "Lalka", 1968, "Wojciech Has", 152, 2.5))
    val credited = Seq(Multikino, Helios, KinoApollo).map(listing(_, "Lalka", Some(2026), Some("Maciej Kawalski"), Some(162)))
    val horror   = Seq(CinemaCityPoznanPlaza, CinemaCityKinepolis, CinemaCityWroclavia).map(listing(_, "Lalka (ale to horror)", Some(2025), None, Some(82)))
    val r = shipped(credited ++ horror, films)
    withClue((credited ++ horror).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      credited.foreach(l => r.decisionOf(l.key).film shouldBe Some(1321666))
      horror.foreach(l => r.decisionOf(l.key).film shouldBe Some(1309083))
    }
  }

  it should "take neither for a programme whose two films' titles share words but not their place in it" in {
    // UK, Odeon's "The Gruffalo + The Gruffalo's Child" (×39): 53 minutes, both films' directors
    // credited. Its title starts with one film's title and ends with the other's; the two share
    // "The Gruffalo", so read as word sets they looked nested and the pooled vote took one film.
    // They sit at spans of the title that do not overlap: the title names both alike, its facts fit
    // both alike (both directors, the runtime of neither), so it takes neither.
    val films    = Seq(F(28118, "The Gruffalo", 2009, "Max Lang", 27, 8), F(81684, "The Gruffalo's Child", 2011, "Johannes Weiland", 27, 6))
    val plain    = Seq(Multikino, Helios).map(listing(_, "The Gruffalo", director = Some("Max Lang"), runtime = Some(27)))
    val bill     = Seq(KinoApollo, Rialto).map(listing(_, "The Gruffalo + The Gruffalo's Child",
      director = Some("Jakob Schuh, Max Lang, Johannes Weiland"), runtime = Some(53)))
    val r = shipped(plain ++ bill, films)
    withClue((plain ++ bill).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      plain.foreach(l => r.decisionOf(l.key).film shouldBe Some(28118))
      bill.foreach(l => r.decisionOf(l.key).film shouldBe None)
    }
    r.violations shouldBe 0
  }

  it should "take neither film though the facts of such a title fit one" in {
    // The same title credited to one film's director at its runtime: still a double programme, so neither film (user rule).
    val films = Seq(F(28118, "The Gruffalo", 2009, "Max Lang", 27, 8), F(81684, "The Gruffalo's Child", 2011, "Johannes Weiland", 27, 6))
    val one   = listing(KinoApollo, "The Gruffalo + The Gruffalo's Child", Some(2011), Some("Johannes Weiland"), Some(27))
    val d = shipped(Seq(one), films).decisionOf(one.key)
    withClue(d.render)(d.film shouldBe None)
  }

  it should "still take a film whose own title joins two others' ('Romeo + Juliet', 'Fast & Furious')" in {
    val romeo  = Seq(F(454, "Romeo + Juliet", 1996, "Baz Luhrmann", 120, 30), F(6003, "Romeo", 2011, "Someone Else", 90, 2),
      F(6004, "Juliet", 2015, "Another One", 95, 2), F(6005, "Romeo and Juliet", 1968, "Franco Zeffirelli", 138, 15))
    val furious = Seq(F(13804, "Fast & Furious", 2009, "Justin Lin", 107, 40), F(6006, "Fast", 2010, "Someone Else", 80, 2),
      F(6007, "Furious", 2017, "Another One", 110, 3))
    val bareRomeo   = listing(KinoApollo, "Romeo + Juliet")
    val bareFurious = listing(KinoApollo, "Fast & Furious")
    withClue(shipped(Seq(bareRomeo), romeo).decisionOf(bareRomeo.key).render)(shipped(Seq(bareRomeo), romeo).decisionOf(bareRomeo.key).film shouldBe Some(454))
    withClue(shipped(Seq(bareFurious), furious).decisionOf(bareFurious.key).render)(
      shipped(Seq(bareFurious), furious).decisionOf(bareFurious.key).film shouldBe Some(13804))
  }

  it should "still join a decorated spelling whose other pieces name no film to its plain siblings" in {
    val films    = Seq(F(1321666, "Lalka", 2026, "Maciej Kawalski", 162, 3), F(81315, "Lalka", 1968, "Wojciech Has", 152, 2.5))
    val credited = Seq(Multikino, Helios).map(listing(_, "Lalka", Some(2026), Some("Maciej Kawalski"), Some(162)))
    val premiere = listing(KinoMuza, "Oficjalna premiera: Lalka")
    val r = shipped(credited :+ premiere, films)
    withClue(r.decisionOf(premiere.key).render)(r.decisionOf(premiere.key).film shouldBe Some(1321666))
  }

  "A title a venue publishes beside a record's own title as its original" should
    "name that record wherever the title is listed" in {
    // PL, André Rieu's 2026 Maastricht concert: TMDB titles it in English only, so "Andre Rieu. Niech
    // żyje Maastricht!" relates to it by a few shared words (`overlap`, 5.1%). Multikino's detail
    // page publishes the English title as the original: the Polish title is that record's title in
    // Polish at Cinema City too, which credits the production company and the country the concert
    // is staged in (the Netherlands; TMDB's origin is Belgium): read by those shared words, its own
    // facts denied the record, and the bare listings, title-linked to both, followed neither.
    val films = Seq(F(1702801, "André Rieu's 2026 Summer Concert: Viva Maastricht!", 2026, "", 0, 0.5, countries = Seq("BE")))
    val multikino = Seq(Multikino, Helios).map(listing(_, "Andre Rieu. Niech żyje Maastricht!", runtime = Some(185))
      .copy(originalTitle = Some("Andre Rieu's 2026 Summer Concert: Viva Maastricht!")))
    val cinemaCity = listing(KinoApollo, "Andre Rieu. Niech żyje Maastricht!", Some(2026), Some("André Rieu Productions"), Some(185))
      .copy(countries = Seq("Holandia"))
    val bare       = Seq(KinoMuza, Rialto).map(listing(_, "André Rieu. Niech żyje Maastricht!"))
    val all = multikino ++ Seq(cinemaCity) ++ bare
    val r = shipped(all, films)
    withClue(all.map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      all.foreach(l => r.decisionOf(l.key).film shouldBe Some(1702801))
    }
    r.violations shouldBe 0
  }

  "A listing whose title and credited director name one film" should "not take another film of that director its title does not name" in {
    // US, Syndicated's "Zodiac", credited to David Fincher at 139 minutes — Fight Club's runtime,
    // not Zodiac's 157. The walk of Fincher's filmography reached Fight Club; the runtime fit made
    // it the best score, six same-titled namesakes weighing on Zodiac. The title and the director
    // name Zodiac together.
    val films  = Seq(F(1949, "Zodiac", 2007, "David Fincher", 157, 20), F(550, "Fight Club", 1999, "David Fincher", 139, 40)) ++
      (1 to 6).map(i => F(2000 + i, "Zodiac", 1970 + 5 * i, s"Director $i", 90, 1))
    val zodiac = listing(Rialto, "Zodiac", director = Some("David Fincher"), runtime = Some(139))
    val d = shipped(Seq(zodiac), films).decisionOf(zodiac.key)
    withClue(d.render)(d.film shouldBe Some(1949))
  }

  it should "still take a walked film when its title names none of that director's films" in {
    // The Polish title of a documentary TMDB's search does not return: the walk is the only path.
    val films    = Seq(F(431444, "The Curious World of Hieronymus Bosch", 2016, "David Bickerstaff", 90, 8))
    val credited = listing(KinoMuza, "Osobliwy świat Hieronymusa Boscha", Some(2016), Some("David Bickerstaff"), Some(90))
    shipped(Seq(credited), films).decisionOf(credited.key).film shouldBe Some(431444)
  }

  "Nowe Horyzonty's 83-minute Your Name re-release" should "take Shinkai's film, whose record carries the English title it publishes" in {
    // §15.8 listed it as a known regression: the r5 artefact VETOED it (0.06 < the certified cut)
    // against a hand-built record titled only "Twoje imię", so its original title "Your Name
    // (re-release)" read as disjoint (−2.94) beside 83 minutes against 106 (−3.26). TMDB's record
    // (recording 36224654409, the PL tree's 372058) carries "Your Name" among its titles: the
    // original title is the film's, decorated, and the shipped artefact takes it on its director.
    val films = Seq(F(372058, "Twoje imię.", 2016, "Makoto Shinkai", 106, 30, Seq("Twoje imię", "Your Name", "Kimi no Na wa.")))
    val nh = Listing(KinoMuza, ListingKey.Published(KinoMuza.displayName, "Twoje imię", None, Seq("Makoto Shinkai")), "Twoje imię",
      "Twoje imię", "Twoje imię", None, Seq("Makoto Shinkai"), Some(83), None, Some("Your Name (re-release)"))
    val d = IdentityResolver.resolve(Seq(nh), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver).decisionOf(nh.key)
    withClue(d.render)(d.film shouldBe Some(372058))
  }

  "A re-release whose listing publishes its screening year" should "not veto the film its same director made" in {
    // Helios RePlay and Splat!FilmFest list Ken Russell's 1971 "Diabły" with the screening's year,
    // 2026, in the year field. The director is the same person: the year dates the screening, not
    // another film, so it scores against the film but never vetoes it.
    val films = Seq(F(31767, "Diabły", 1971, "Ken Russell", 111, 5))
    val dated = Seq(Multikino, Helios).map(listing(_, "Diabły", Some(2026), Some("Ken Russell"), Some(114)))
    val r = IdentityResolver.resolve(dated, new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    dated.foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe Some(31767)))
  }

  it should "take that director's film even though the screening year counts against it in the score" in {
    // PL Kinoteka publishes its screening year, 2026, for retrospective titles: Veit Helmer's 1999
    // "Tuvalu" with its director and runtime, and Ken Russell's 1971 "Diabły" in a festival banner.
    // The year no longer vetoed them, but its −7.77 still sank them below the cut (5.8%, 27.3%).
    val films = Seq(F(8076, "Tuvalu", 1999, "Veit Helmer", 101, 5), F(31767, "Diabły", 1971, "Ken Russell", 111, 5))
    val tuvalu = listing(Multikino, "Tuvalu", Some(2026), Some("Veit Helmer"), Some(101))
    val devils = listing(Helios, "Diabły | Splat!FilmFest", Some(2026), Some("Ken Russell"), Some(111)).copy(originalTitle = Some("The Devils"))
    val r = IdentityResolver.resolve(Seq(tuvalu, devils), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    withClue(r.decisionOf(tuvalu.key).render)(r.decisionOf(tuvalu.key).film shouldBe Some(8076))
    withClue(r.decisionOf(devils.key).render)(r.decisionOf(devils.key).film shouldBe Some(31767))
  }

  it should "not take that director's film for a programme whose title only starts with it" in {
    // PL, Kino Nowe Horyzonty's 2026 double bill "Basia. Humor w paski mam + Kocia Szajka. Tajemnica
    // zniknięcia śledzi", credited to Marcin Wasilewski, whose 2018 "Basia" its title merely starts
    // with (`decorated`). The title is not that film's title, so its 2026 is not a screening year
    // of it: the year stays a fact, and the programme is not the 2018 film.
    val films = Seq(F(634598, "Basia", 2018, "Marcin Wasilewski", 0, 1))
    val bill  = listing(Multikino, "Basia. Humor w paski mam + Kocia Szajka. Tajemnica zniknięcia śledzi", Some(2026),
      Some("Marcin Wasilewski"), Some(53))
    val d = IdentityResolver.resolve(Seq(bill), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver).decisionOf(bill.key)
    withClue(d.render)(d.film shouldBe None)
  }

  it should "still take the same director's film of the listing's year when both are there" in {
    val films = Seq(F(10234, "Funny Games", 1997, "Michael Haneke", 108, 8), F(8461, "Funny Games", 2007, "Michael Haneke", 111, 9))
    val dated = listing(Multikino, "Funny Games", Some(2007), Some("Michael Haneke"), Some(111))
    IdentityResolver.resolve(Seq(dated), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
      .decisionOf(dated.key).film shouldBe Some(8461)
  }

  it should "keep the year of a revival whose runtime is not the film's" in {
    // DE Gloria Palast and Roxy Kinos: "MET Opera Live im Kino: Manon", 2027, Laurent Pelly, 264
    // minutes — the Met's 2026/27 revival of Pelly's staging. His 2019 recording of it runs 232: a
    // re-release is the same cut, so a runtime half an hour off makes the 2027 another production's
    // year, not the 2019 film's screening year.
    val films = Seq(F(616147, "The Metropolitan Opera: Manon", 2019, "Laurent Pelly", 232, 1))
    val revival = Seq(Multikino, Helios).map(listing(_, "MET Opera Live im Kino: Manon", Some(2027), Some("Laurent Pelly"), Some(264)))
    val r = IdentityResolver.resolve(revival, new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    revival.foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe None))
  }

  it should "still veto a film of that title another director made decades before" in {
    val films = Seq(F(31767, "Diabły", 1971, "Ken Russell", 111, 5))
    val other = listing(Multikino, "Diabły", Some(2026), Some("Someone Else"), Some(95))
    IdentityResolver.resolve(Seq(other), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
      .decisionOf(other.key).film shouldBe None
  }

  "A listing whose original title only repeats its own title" should "not deny its credited siblings' film" in {
    // UK: five credited "Terminator 2: Judgment Day" listings and Everyman's "Cellar Door x
    // ThoughtBubble presents: Terminator 2: Judgment Day", whose original-title field repeats that
    // title. The repeat is the title again, not a second fact: read as one, it scored as a segment
    // against 280, denied it for the whole cluster, and the vote took The Terminator (1984).
    val films    = Seq(F(280, "Terminator 2: Judgment Day", 1991, "James Cameron", 137, 40), F(218, "The Terminator", 1984, "James Cameron", 108, 35))
    val credited = Seq(Multikino, Helios, KinoApollo, Rialto).map(listing(_, "Terminator 2: Judgment Day", director = Some("James Cameron"), runtime = Some(137)))
    val title    = "Cellar Door x ThoughtBubble presents: Terminator 2: Judgment Day"
    val echo     = listing(KinoMuza, title).copy(originalTitle = Some("Cellar Door x ThoughtBubble Presents: Terminator 2: Judgment Day"))
    val r = IdentityResolver.resolve(credited :+ echo, new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    (credited :+ echo).foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film should not be Some(218)))
    credited.foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe Some(280)))
    r.violations shouldBe 0
  }

  it should "never have that repeat veto the film on its own" in {
    // Alone, the repeat was the one fact the listing compared, and it scored against 280
    // (originalTitle=segment): a veto that reached every sibling. A repeat is the title again,
    // and a title relation alone never vetoes.
    val films = Seq(F(280, "Terminator 2: Judgment Day", 1991, "James Cameron", 137, 40), F(218, "The Terminator", 1984, "James Cameron", 108, 35))
    val echo  = listing(KinoMuza, "Cellar Door x ThoughtBubble presents: Terminator 2: Judgment Day")
      .copy(originalTitle = Some("Cellar Door x ThoughtBubble Presents: Terminator 2: Judgment Day"))
    val d = IdentityResolver.resolve(Seq(echo), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver).decisionOf(echo.key)
    withClue(d.render)(d.basis should not be ResolverDecision.Basis.Vetoed)
  }

  it should "not veto the film when it is the listing's title cut short" in {
    // UK, 271 BTS "… IN BUENOS AIRES / SÃO PAULO: LIVE VIEWING" listings whose original-title field
    // is the same title truncated to "…: Live": scored as a fragment of the record's full title, it
    // vetoed the exact, rank-1 record. A truncated copy is the title again, not a second fact.
    val films = Seq(F(1770237, "BTS World Tour 'Arirang' in Buenos Aires: Live Viewing", 2026, "", 0, 20))
    val relay = listing(KinoMuza, "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES: LIVE VIEWING", director = Some("Jungjae HA"), runtime = Some(195))
      .copy(originalTitle = Some("BTS World Tour 'ARIRANG' In Buenos Aires: Live"))
    val d = IdentityResolver.resolve(Seq(relay), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver).decisionOf(relay.key)
    withClue(d.render)(d.film shouldBe Some(1770237))
  }

  "An exact top hit whose record credits nobody and states no runtime" should
    "not be out-ranked on those facts by a record its title does not name" in {
    // ES, Ocine's "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES: LIVE VIEWING" ×8, crediting "Jungjae HA"
    // and 195 minutes: TMDB's Buenos Aires record, the exact rank-1 hit, credits no director and
    // states no runtime. The credit's filmography walk reaches that director's OTHER concerts ("…
    // in Busan", "Permission to Dance on Stage - Seoul"), each crediting him at 195 minutes under a
    // title the listing only overlaps; once "Jungjae HA" and "Ha Jung-jae" read as one person, they
    // out-weighed the exact record on facts it cannot answer, and the top-hit acceptance was
    // withheld. A fact the record does not carry is missing evidence, not evidence against it. The
    // São Paulo listings, whose record credits him, still take theirs.
    val films = Seq(F(1770237, "BTS World Tour 'Arirang'  in Buenos Aires: Live Viewing", 2026, "", 0, 0.953),
                    F(1770234, "BTS World Tour 'Arirang' In São Paulo: Live Viewing", 2026, "Jungjae HA", 0, 0.9572),
                    F(1701849, "BTS WORLD TOUR [ARIRANG] in Busan", 2026, "Jungjae HA", 195, 3.5784),
                    F(939984, "BTS: Permission to Dance on Stage - Seoul", 2022, "Jungjae HA", 195, 3.6096))
    val venues = Seq(Multikino, Helios, KinoApollo, KinoMuza)
    val buenosAires = venues.map(listing(_, "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES: LIVE VIEWING", director = Some("Jungjae HA"), runtime = Some(195)))
    val saoPaulo    = venues.map(listing(_, "BTS WORLD TOUR 'ARIRANG' IN SAO PAULO: LIVE VIEWING", director = Some("Jungjae HA"), runtime = Some(195)))
    val r = IdentityResolver.resolve(buenosAires ++ saoPaulo, new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    withClue((buenosAires ++ saoPaulo).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      buenosAires.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(1770237))
      saoPaulo.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(1770234))
    }
    r.violations shouldBe 0
  }

  "A re-release titled with its screening year" should "not veto the film its credited siblings name" in {
    // 951 US venues list the 1939 film; some spell it "Gone With The Wind (2026)" — the re-release's
    // year, which a bracket year is as often as the film's. A bracket year agrees; it never denies
    // (the label rule's own asymmetry, `IdentityMeasures.ownAgreement`).
    val films = Seq(F(770, "Gone with the Wind", 1939, "Victor Fleming", 238, 20))
    val credited = Seq(Multikino, Helios).map(listing(_, "Gone with the Wind", director = Some("Victor Fleming"), runtime = Some(238)))
    val dated    = Seq(KinoApollo, Rialto).map(listing(_, "Gone With The Wind (2026)"))
    val r = IdentityResolver.resolve(credited ++ dated, new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    (credited ++ dated).foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe Some(770)))
    r.violations shouldBe 0
  }

  "One member's own denial" should "split that member off, not veto the film for its credited siblings" in {
    // UK, "Ozzy & Black Sabbath: Back to the Beginning": most venues credit the film's director and
    // runtime; one credits another director and a runtime the record contradicts — not apart from
    // its siblings on what they publish, but denying the film for itself — and pooled, that denial
    // used to veto the film for every sibling. The denier stays off it.
    val films    = Seq(F(1515139, "Back to the Beginning", 2026, "Paul Dugdale", 100, 20))
    val credited = Seq(Multikino, Helios, KinoApollo).map(listing(_, "Back to the Beginning", Some(2026), Some("Paul Dugdale"), Some(100)))
    val denier   = listing(Rialto, "Back to the Beginning", None, Some("Tim Van Someren"), Some(140))
    val r = resolve(credited :+ denier, films)
    val shown = (credited :+ denier).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")
    withClue(shown) {
      credited.foreach(l => r.decisionOf(l.key).film shouldBe Some(1515139))
      r.decisionOf(denier.key).film should not be Some(1515139)
      together(r, credited.head, denier) shouldBe false
    }
    r.violations shouldBe 0
  }

  it should "still veto the film when the whole cluster's pooled facts deny it" in {
    // PL, a 2026 double bill beside the same director's older "Basia": the members that publish a
    // year deny the old film on it; the rest publish none. Pooled — the modal year, the median
    // runtime — the cluster itself denies it, so the yearless rest must not take it on the director.
    val films   = Seq(F(1, "Back to the Beginning", 2026, "Paul Dugdale", 100, 20))
    val rest    = listing(Multikino, "Back to the Beginning", None, Some("Paul Dugdale"), Some(100))
    val deniers = Seq(Helios, KinoApollo).map(listing(_, "Back to the Beginning", Some(1990), None, Some(140)))
    val r = resolve(rest +: deniers, films)
    (rest +: deniers).foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe None))
  }

  it should "still veto the film for bare siblings whose own facts cannot carry it" in {
    // The same denial beside listings that publish only the title: nothing of their own names the
    // film, so the database's ranking alone never outvotes the credited member.
    val films  = Seq(F(1515139, "Back to the Beginning", 2026, "Paul Dugdale", 100, 20))
    val bare   = Seq(Multikino, Helios, KinoApollo).map(listing(_, "Back to the Beginning"))
    val denier = listing(Rialto, "Back to the Beginning", None, Some("Tim Van Someren"), Some(140))
    val r = resolve(bare :+ denier, films)
    (bare :+ denier).foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe None))
  }

  /** The shipped artefact without its evidence classes: what a bare title scores signal by signal. */
  private val withoutClasses = IdentityCalibration.resolver.copy(evidenceClasses = Nil)
  /** The shipped artefact with one evidence class: an exact top hit with at most `rivals` same-titled rivals. */
  private def withTopHitClass(rivals: Double, base: IdentityCalibration = withoutClasses): IdentityCalibration = {
    import IdentityCalibration.{Condition, EvidenceClass}
    base.copy(evidenceClasses = Seq(EvidenceClass("exact top hit", IdentityMeasures.ListingFilm,
      Seq(Condition("title", in = Seq("exact")), Condition("search.rank", atMost = Some(1)), Condition("rivals", atMost = Some(rivals))),
      probability = 0.99)))
  }

  "A bare exact title TMDB returns first" should "take that film on its evidence class's measured probability" in {
    val films = Seq(F(39264, "Godzilla vs. Megalon", 1973, "Jun Fukuda", 82, 0.6), F(2, "Godzilla vs. Mothra", 1964, "Ishirō Honda", 89, 30))
    val bare  = listing(Rialto, "Godzilla vs. Megalon")
    val before = IdentityResolver.resolve(Seq(bare), new FilmTable(films, normalizer), normalizer, withoutClasses).decisionOf(bare.key)
    before.film shouldBe None
    before.basis shouldBe ResolverDecision.Basis.BelowThreshold
    val after = IdentityResolver.resolve(Seq(bare), new FilmTable(films, normalizer), normalizer, withTopHitClass(0.5)).decisionOf(bare.key)
    after.film shouldBe Some(39264)
    after.basis shouldBe ResolverDecision.Basis.OwnMatch
    after.confidence shouldBe 0.99
    after.explanation.head should include ("exact top hit")
  }

  it should "not take it past its class: a same-titled rival, a banner, a published fact against it, a rival that fits better" in {
    val megalon = F(39264, "Godzilla vs. Megalon", 1973, "Jun Fukuda", 82, 0.6)
    val remake  = F(7, "Godzilla vs. Megalon", 2031, "Someone Else", 120, 0.5)
    def film(l: Listing, films: Seq[F], rivals: Double) =
      IdentityResolver.resolve(Seq(l), new FilmTable(films, normalizer), normalizer, withTopHitClass(rivals)).decisionOf(l.key).film
    // A rival the class was not measured with.
    film(listing(Rialto, "Godzilla vs. Megalon"), Seq(megalon, remake), rivals = 0.5) shouldBe None
    // A banner segment's first hit IS the bare title's exact top hit when the banner names no film
    // (segment-top-hit, 2026-10-02) — but not past the class's other limits, below.
    film(listing(Rialto, "Kino Nocne: Godzilla vs. Megalon"), Seq(megalon), rivals = 0.5) shouldBe Some(39264)
    film(listing(Rialto, "Kino Nocne: Godzilla vs. Megalon"), Seq(megalon, remake), rivals = 0.5) shouldBe None
    // The venue's own runtime weighs against the top hit.
    film(listing(Rialto, "Godzilla vs. Megalon", runtime = Some(150)), Seq(megalon), rivals = 0.5) shouldBe None
    // The rival's facts fit the listing better than the top hit's: the top hit is not taken.
    film(listing(Rialto, "Godzilla vs. Megalon", runtime = Some(120)), Seq(megalon, remake), rivals = 2.5) should not be Some(39264)
  }

  it should "still leave a broadcast's bare listing off the old film its title-linked sibling's season denies" in {
    val films  = Seq(F(29993, "Samson i Dalila", 1949, "Cecil B. DeMille", 131, 20))
    val season = listing(Multikino, "Samson i dalila | metropolitan opera: live in hd 2026/27")
    val bare   = Seq(Helios, KinoApollo).map(listing(_, "Samson i Dalila"))
    // On the case's own weights, where the sibling's season denies the film: the class must not undo that.
    val r = IdentityResolver.resolve(season +: bare, new FilmTable(films, normalizer), normalizer, withTopHitClass(0.5, weights))
    (season +: bare).foreach(l => withClue(l.title)(r.decisionOf(l.key).film should not be Some(29993)))
    r.violations shouldBe 0
  }

  "A listing's own match" should "outweigh a title-linked sibling that denies the film only by the probability cut" in {
    // UK, "Fallen Angels by Noel Coward" (×95 UK, ×360 US, 91%): the 2026 recording by its screen director, Annette
    // Jolles, and its runtime, withdrawn because the "Fallen Angels by Noël Coward" spelling (×194), crediting only the
    // stage director Scott Ellis and billing 15 minutes more, denies the film — by the cannot-link probability cut alone,
    // no rule. A denial by a rule ("Samson i Dalila"'s season, above) still withdraws it.
    val films  = Seq(F(11220, "Fallen Angels", 1995, "Wong Kar Wai", 99, popularity = 20), F(1702350, "Fallen Angels", 2026, "Annette Jolles", 95, popularity = 1),
      F(1437981, "Coward", 2026, "Lukas Dhont", 117, popularity = 15))
    val noel   = Seq(Multikino, Helios, KinoApollo).map(v => listing(v, "Fallen Angels by Noel Coward", director = Some("Scott Ellis"), runtime = Some(95))
      .copy(directors = Seq("Scott Ellis", "Annette Jolles"), originalTitle = Some("Fallen Angels By Noël Coward")))
    val accent = Seq(KinoMuza, Rialto, KinoPalacowe).map(listing(_, "Fallen Angels by Noël Coward", director = Some("Scott Ellis"), runtime = Some(110)))
    val bare   = Seq(KinoMikro, KinoOaza).map(listing(_, "Fallen Angels", director = Some("Wong Kar Wai"), runtime = Some(98)))
    val r      = shipped(noel ++ accent ++ bare, films)
    noel.foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe Some(1702350)))
    accent.foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film should not be Some(11220)))
  }

  "A local title TMDB carries no translation of" should "take the film IMDb lists under that title" in {
    // PL "Camino dla opornych" (×14 venues, ~20 listings) is IMDb's Polish title of tt39814688, TMDB's "Compostelle"
    // (2026), which TMDB knows only in French and English: its search for the Polish title finds nothing, and IMDb's
    // suggestion reached the record under a title the listing does not carry, so no rule took it.
    val compostelle = F(1404604, "Compostelle", 2026, "Ben Eyrich", 98, popularity = 2, imdbTitles = Seq("Santiago: The Camino Therapy", "Camino dla opornych"))
    val ls = Seq(listing(KinoMuza, "Camino dla opornych"), listing(Rialto, "Camino dla opornych | CHKF"), listing(Helios, "Camino dla opornych", runtime = Some(97)))
    val r  = shipped(ls, Seq(compostelle, F(197704, "Santiago", 1956, "Gordon Douglas", 93, popularity = 5)))
    ls.foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe Some(1404604)))
  }

  it should "not take it beside a record carrying the title, a year the title dates, a numbered set, or a stage work" in {
    // PL "Renoir": IMDb lists Bourdos's 2012 film as "Renoir", and TMDB carries Hayakawa's 2025 "Renoir" too — the
    // listing's own namesake stands beside it. "Miłość 2024" dates another year than Haneke's 2012 "Miłość";
    // "Bolek i Lolek – zestaw IV" numbers a set of cartoons; "… – Così fan tutte" names the opera the Met relays.
    def taken(l: Listing, films: Seq[F]) = shipped(Seq(l), films).decisionOf(l.key).film
    val bourdos  = F(76180, "Renoir", 2012, "Gilles Bourdos", 111, popularity = 3, searched = false, imdbTitles = Seq("Renoir"))
    taken(listing(KinoMuza, "Renoir"), Seq(bourdos, F(1286773, "Renoir", 2025, "Chie Hayakawa", 122, popularity = 2))) should not be Some(76180)
    val amour    = F(86837, "Amour", 2012, "Michael Haneke", 127, popularity = 8, searched = false, imdbTitles = Seq("Miłość"))
    taken(listing(KinoMuza, "Miłość 2024"), Seq(amour)) shouldBe None
    val bolek    = F(1167324, "Bolek and Lolek", 1936, "Henryk Szaro", 80, popularity = 1, searched = false, imdbTitles = Seq("Bolek i Lolek"))
    taken(listing(KinoMuza, "Bolek i Lolek - zestaw IV"), Seq(bolek)) shouldBe None
    // PL "Afrykanska Przygoda 3D IMAX" [2007] {Ben Stassen} stays Stassen's film, not the 1954 one IMDb also calls so.
    val stassen  = F(435263, "African Adventure: Safari in the Okavango 3D", 2007, "Ben Stassen", 40, popularity = 1)
    val old1954  = F(250001, "African Adventure", 1954, "Robert Ruark", 70, popularity = 1, searched = false, imdbTitles = Seq("Afrykanska Przygoda"))
    taken(listing(KinoMuza, "Afrykanska Przygoda 3D IMAX", Some(2007), Some("Ben Stassen")), Seq(stassen, old1954)) should not be Some(250001)
    val brass    = F(29380, "Così fan tutte", 1992, "Tinto Brass", 95, popularity = 4, searched = false, imdbTitles = Seq("Così fan tutte"))
    taken(listing(KinoMuza, "ReTransmisje Met: Na żywo w HD - Così fan tutte"), Seq(brass)) shouldBe None
  }

  it should "take it through another title the listing is searched by names several films on IMDb" in {
    // PL Helios's "Camino dla opornych - KNT" publishes the original title "Santiago", which IMDb lists a dozen films
    // under: that title names none of them, and is no namesake of the listing's own title. The plain listing beside it
    // takes Compostelle; Helios's is not Gordon Douglas's 1956 "Santiago" its original names.
    val compostelle = F(1404604, "Compostelle", 2026, "Ben Eyrich", 98, popularity = 2, alternatives = Seq("Santiago: The Camino Therapy"),
      imdbTitles = Seq("Camino dla opornych"))
    val santiagos   = Seq(F(197704, "Santiago", 1956, "Gordon Douglas", 118, popularity = 5, imdbTitles = Seq("Santiago")),
      F(86324, "Santiago", 2007, "Somebody", 131, popularity = 1, imdbTitles = Seq("Santiago")))
    // (its learned "KNT" decoration off, which this case's listing does not carry)
    val helios = listing(Helios, "Camino dla opornych", runtime = Some(98)).copy(originalTitle = Some("Santiago"))
    val plain  = listing(KinoMuza, "Camino dla opornych")
    def film(l: Listing) = shipped(Seq(l), compostelle +: santiagos).decisionOf(l.key).film
    film(plain) shouldBe Some(1404604)
    film(helios) should not be Some(197704)
  }

  it should "leave a title IMDb lists two films under to what else the listing publishes" in {
    // PL "Lalka": IMDb lists Has's 1968 film and Kawalski's 2026 one under it; a listing publishing nothing else is
    // neither by the title alone.
    val films = Seq(F(81315, "The Doll", 1968, "Wojciech Has", 159, popularity = 3, searched = false, imdbTitles = Seq("Lalka")),
      F(1321666, "The Doll", 2026, "Maciej Kawalski", 162, popularity = 3, searched = false, imdbTitles = Seq("Lalka")))
    val bare  = listing(KinoMuza, "Lalka")
    shipped(Seq(bare), films).decisionOf(bare.key).film shouldBe None
    val dated = listing(Rialto, "Lalka", Some(2026), Some("Maciej Kawalski"))
    shipped(Seq(dated), films).decisionOf(dated.key).film shouldBe Some(1321666)
  }

  "A runtime no film runs" should "be read as unpublished, not as a fact against the film" in {
    // DE Filmtheater Bleicherode: "flüstern & SCHREIEN" [1989] {Dieter Schumann}, 6000 minutes — the 1988 film by its
    // director and year, vetoed by the learned `runtime.delta >= 81`.
    val film = F(149841, "flüstern & SCHREIEN", 1988, "Dieter Schumann", 117, popularity = 1)
    val l    = listing(KinoMuza, "flüstern & SCHREIEN", Some(1989), Some("Dieter Schumann"), runtime = Some(6000))
    val r    = shipped(Seq(l), Seq(film))
    withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe Some(149841))
  }

  "Curation pins" should "override the evidence: a pinned film, a denied one, and a pinned group" in {
    def pin(ls: Seq[Listing], claim: PinClaim) = Pin(ls.map(_.key), claim, "spec", "test", java.time.Instant.EPOCH)
    val films = Seq(F(1, "Opętanie", 1981, "Andrzej Żuławski", 124), F(2, "Opętanie", 1973, "Someone Else", 90))
    val bare  = listing(Multikino, "Opętanie | klasyka w 4k")
    val dated = listing(Helios, "Opętanie", Some(1981), Some("Andrzej Żuławski"))
    val other = listing(KinoMuza, "Possession")
    def withPins(ps: Pin*) = IdentityResolver.resolve(Seq(bare, dated, other), new FilmTable(films, normalizer), normalizer, weights,
      pins = ListingConstraints.pinned(ps))

    val none = withPins()
    none.decisionOf(dated.key).film shouldBe Some(1)
    together(none, dated, other) shouldBe false

    // Pinned AGAINST what its siblings' evidence would give it.
    val isFilm = withPins(pin(Seq(bare), PinClaim.IsFilm(2)))
    isFilm.decisionOf(bare.key).film shouldBe Some(2)
    isFilm.decisionOf(bare.key).basis shouldBe ResolverDecision.Basis.Pinned
    isFilm.decisionOf(bare.key).confidence shouldBe 1.0
    together(isFilm, bare, dated) shouldBe false

    val never = withPins(pin(Seq(dated), PinClaim.NeverFilm(1)))
    never.decisionOf(dated.key).film should not be Some(1)

    val same = withPins(pin(Seq(dated, other), PinClaim.SameFilm))
    together(same, dated, other) shouldBe true
    same.violations shouldBe 0
  }

  "Learned cannot-links" should "veto by the artefact's rules, and never on missing evidence" in {
    val rule = IdentityCalibration.CannotLinkRule("director in {different} AND year.distance >= 6", "listing-film",
      Seq(IdentityCalibration.Condition("director", in = Seq("different")), IdentityCalibration.Condition("year.distance", atLeast = Some(6))),
      falseVetoRate = 0.001, support = 100)
    val withRule = weights.copy(cannotLinks = Seq(rule))
    val films = Seq(F(1, "Samson i Dalila", 1949, "Cecil B. DeMille", 131, 50))
    val met   = listing(Multikino, "Samson i Dalila", Some(2026), Some("Darko Tresnjak"))
    val bare  = listing(Helios, "Samson i Dalila")
    val r = IdentityResolver.resolve(Seq(met, bare), new FilmTable(films, normalizer), normalizer, withRule)
    r.decisionOf(met.key).film should not be Some(1)
    r.decisionOf(met.key).basis shouldBe ResolverDecision.Basis.Vetoed
    // The bare listing publishes neither a year nor a director: nothing to veto on.
    val alone = IdentityResolver.resolve(Seq(bare), new FilmTable(films, normalizer), normalizer, withRule)
    alone.edges.filterNot(_.must) shouldBe empty
    val bareMeasures = IdentityMeasures.listingFilm(IdentityMeasures.Listing("Samson i Dalila"),
      IdentityMeasures.Film("Samson i Dalila", year = Some(1949), directors = Some(Seq("Cecil B. DeMille"))), None, 0, 0)
    ListingConstraints.learned(withRule, "listing-film", bareMeasures, probability = 0.5) shouldBe None
  }

  // ── editions and cuts: a title plus a qualifier ────────────────────────────────────────

  /** What a title search for a qualifier returns: records billing it beside other works, as
   *  TMDB's "Director's Cut" and "The Final Cut" searches did (recording 36224654409). */
  private val directorsCuts = Seq(F(355536, "Director's Cut", 2016, "Adam Rifkin", 91, 2.3), F(1291247, "Director's Cut", 2024, "Someone", 88, 1.6),
    F(1255761, "Chocolate - Director's Cut", 2008, "Prachya Pinkaew", 110, 1.0), F(526535, "The Great War: Director's Cut", 2013, "Other", 100, 0.7),
    F(572544, "The Promise (Director's Cut)", 2016, "Terry George", 133, 0.7))
  private val finalCuts = Seq(F(11099, "The Final Cut", 2004, "Omar Naim", 95, 5.2), F(2442, "The Final Cut", 1995, "Roger Christian", 99, 2.8),
    F(92476, "Pink Floyd: The Final Cut", 1983, "Willie Christie", 19, 1.7), F(469940, "Mantrap – Straw Dogs: The Final Cut", 2003, "Other", 30, 1.8))

  "A title plus a cut" should "never take a record titled only its cut" in {
    // US, Landmark Nuart's "Dark City: Director's Cut", no fact published: the new resolver took
    // "Director's Cut" (2016). Records bill "Director's Cut" after many works, as the
    // listing does — the cut is its qualifier, and a record of the qualifier alone names nothing.
    val films = directorsCuts ++ Seq(F(2666, "Dark City", 1998, "Alex Proyas", 100, 13.3), F(36331, "Dark City", 1950, "William Dieterle", 98, 2.7))
    val cut   = listing(Rialto, "Dark City: Director's Cut")
    val r = resolve(Seq(cut), films)
    withClue(r.decisionOf(cut.key).render) {
      r.decisionOf(cut.key).film should not be Some(355536)
      r.decisionOf(cut.key).film should not be Some(1291247)
    }
    // "Michael Mann's Manhunter: The Final Cut" (US, Alamo) took Omar Naim's "The Final Cut" (2004).
    val manhunter = Seq(F(11454, "Manhunter", 1986, "Michael Mann", 120, 0.4), F(100279, "Manhunter", 1974, "Walter Grauman", 74, 2.3))
    val mann = listing(Rialto, "Michael Mann's Manhunter: The Final Cut")
    val m = resolve(Seq(mann), finalCuts ++ manhunter)
    withClue(m.decisionOf(mann.key).render) {
      Set(Some(11099), Some(2442)) should not contain m.decisionOf(mann.key).film
    }
  }

  it should "leave a listing of the qualifier's own title its film" in {
    // The same records: a listing titled "Director's Cut" alone, with its year, is the 2016 film.
    val films = directorsCuts :+ F(2666, "Dark City", 1998, "Alex Proyas", 100, 13.3)
    val bare  = listing(Rialto, "Director's Cut", Some(2016), Some("Adam Rifkin"))
    resolve(Seq(bare), films).decisionOf(bare.key).film shouldBe Some(355536)
  }

  "A title plus a qualifier" should "take the edition's record when TMDB files one, and the work beside it keeps the work" in {
    // UK, 42 Picturehouse listings of "Radiohead X Nosferatu: A Symphony of Horror", crediting
    // Murnau and his 94 minutes: the new resolver took Murnau's 1922 record, which the title names
    // only through its alternative title "Nosferatu: A Symphony of Horror". TMDB files the event as
    // its own record, crediting its maker — the listing's facts are the work's, which the edition
    // carries; the whole title names the edition.
    val films = Seq(
      F(653, "Nosferatu", 1922, "F. W. Murnau", 94, 9.2, Seq("Nosferatu: A Symphony of Horror", "Nosferatu the Vampire")),
      F(1489665, "Radiohead X Nosferatu: A Symphony of Horror", 2025, "Josh Frank", 90, 1.3),
      F(394151, "Nosferatu: A Symphony of Horror", 2023, "David Lee Fisher", 92, 2.4),
      F(426063, "Nosferatu", 2024, "Robert Eggers", 132, 40))
    val events = Seq(Multikino, Helios, KinoApollo).map(listing(_, "Radiohead X Nosferatu: A Symphony of Horror", None, Some("F.W. Murnau"), Some(94)))
    val work   = listing(KinoMuza, "Nosferatu (1922)", None, Some("F.W. Murnau"))
    // The production calibration: its director veto is what denied the edition's record.
    val r = IdentityResolver.resolve(events :+ work, new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    withClue((events :+ work).map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      events.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(1489665))
      r.decisionOf(work.key).film shouldBe Some(653)
    }
    r.violations shouldBe 0
  }

  it should "take an edition only IMDb's id reaches, over a work that names it only as an alternative title" in {
    // US, three Flicks venues' "Caligula: The Ultimate Cut", crediting Tinto Brass and 157 minutes
    // (recording 36224654409). TMDB's search for the title returns only the 1979 "Caligula", whose
    // record files "Caligula: The Ultimate Cut" among its alternative titles; the re-cut's own
    // record (2024, 178 minutes, crediting nobody) is reached only by the IMDb id IMDb lists under
    // the title. The whole title is the re-cut's own; the work's facts, which the edition carries,
    // chose the work.
    val films = Seq(
      F(9453, "Caligula", 1979, "Tinto Brass", 156, 10.2, Seq("Io, Caligola", "Caligula: The Ultimate Cut", "Calígula")),
      F(1774981, "Caligula: The Ultimate Cut", 2024, "", 178, 0.23, searched = false))
    val cut = Seq(Multikino, Helios, KinoApollo).map(listing(_, "Caligula: The Ultimate Cut", None, Some("Tinto Brass"), Some(157)))
    val r = shipped(cut, films)
    withClue(cut.map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      cut.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(1774981))
    }
    // The work's own title keeps the work: "Caligula" names the 1979 record as its own.
    val work = listing(KinoMuza, "Caligula", None, Some("Tinto Brass"), Some(156))
    shipped(Seq(work), films).decisionOf(work.key).film shouldBe Some(9453)
    // A record carrying the work's alternative title that credits ANOTHER person is a remake, not
    // an edition: Murnau's listing under his film's alternative title is not David Lee Fisher's.
    val nosferatu = Seq(F(653, "Nosferatu", 1922, "F. W. Murnau", 94, 9.2, Seq("Nosferatu: A Symphony of Horror")),
      F(394151, "Nosferatu: A Symphony of Horror", 2023, "David Lee Fisher", 92, 2.4))
    val murnau = listing(KinoMuza, "Nosferatu: A Symphony of Horror", None, Some("F.W. Murnau"), Some(94))
    val m = shipped(Seq(murnau), nosferatu).decisionOf(murnau.key)
    withClue(m.render)(m.film shouldBe Some(653))
  }

  it should "leave a sequel, a subtitle and a banner their own films" in {
    // A later record carrying the work's title is an edition only to a listing that names IT whole
    // and the work only by a piece: "Dune" names Dune whole; "Dune: Part Two" names its own record.
    val dune = Seq(F(438631, "Dune", 2021, "Denis Villeneuve", 155, 60), F(693134, "Dune: Part Two", 2024, "Denis Villeneuve", 166, 80),
      F(841, "Dune", 1984, "David Lynch", 137, 20))
    val first  = listing(Multikino, "Dune", Some(2021), Some("Denis Villeneuve"))
    val second = listing(Helios, "Dune: Part Two", Some(2024), Some("Denis Villeneuve"))
    val r = resolve(Seq(first, second), dune)
    r.decisionOf(first.key).film shouldBe Some(438631)
    r.decisionOf(second.key).film shouldBe Some(693134)
    // A programme banner beside a work TMDB files one cut of: records bill the work beside one
    // piece, a coincidence, never a qualifier — the work's record still names it.
    val darko = Seq(F(141, "Donnie Darko", 2001, "Richard Kelly", 113, 20), F(9999, "Donnie Darko: Director's Cut", 2004, "Richard Kelly", 133, 2))
    val banners = Seq(listing(KinoMuza, "Throwback: Donnie Darko", None, Some("Richard Kelly"), Some(113)),
      listing(KinoMuza, "Throwback: Dark City"), listing(KinoMuza, "Throwback: Heat"))
    val t = resolve(banners, darko ++ directorsCuts :+ F(2666, "Dark City", 1998, "Alex Proyas", 100, 13.3))
    t.decisionOf(banners.head.key).film shouldBe Some(141)
  }

  it should "leave a work its record under a banner when the records bill it on the other side" in {
    // PL, four venues' "Tani wtorek: OBCY" and UK Cineworld's "Cineworld 30: The Dark Knight" lost
    // their film when records billing the work beside its sequels made the work read as a qualifier.
    // The records bill it BEFORE its sequels; the listing bills it after its banner.
    val films = Seq(F(1429348, "Obcy", 2025, "Zuzanna Grajcewska", 90, 3), F(126889, "Obcy: Przymierze", 2017, "Ridley Scott", 122, 30),
      F(945961, "Obcy: Romulus", 2024, "Fede Alvarez", 119, 60), F(348, "Obcy - 8. pasażer Nostromo", 1979, "Ridley Scott", 117, 50))
    val bannered = listing(KinoMuza, "Tani wtorek: Obcy", Some(2025), Some("Zuzanna Grajcewska"))
    val r = IdentityResolver.resolve(Seq(bannered), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    withClue(r.decisionOf(bannered.key).render) { r.decisionOf(bannered.key).film shouldBe Some(1429348) }
  }

  "A spelling whose original title reaches its OWN film" should
    "still join the plain listings that share its search form" in {
    // PL, main 5424e7874 lost 40 "Pieśni lasu" listings: the plain ones carry no fact and their
    // Polish title is not the record's, so they took the film only through Kinoteka's decorated
    // "Pieśni lasu | Pokaz z kompozycją zapachową od Alba1913", which publishes the original title
    // "Whispers in the Woods". The festival rule dropped their same-search-form link: it read the
    // spelling's OWN segment "Pieśni lasu" — sanitised, without spaces — as another listing's title
    // sharing no word with the form, and its original title's own film as a film named beside it.
    val films = Seq(F(1309373, "Le Chant des forêts", 2025, "Vincent Munier", 95, 3.0, alternatives = Seq("Whispers in the Woods")))
    def scraped(venue: Cinema, title: String) =
      listing(venue, title).copy(cleanTitle = services.movies.ScrapeListing.cleanTitle(venue, title, normalizer)._1)
    // Its clean title keeps the banner; only its SEARCH form is "Pieśni lasu", as the plain ones'.
    val decorated = listing(Kinoteka, "Pieśni lasu | Pokaz z kompozycją zapachową od Alba1913", None, Some("Vincent Munier"), Some(94))
      .copy(originalTitle = Some("Whispers in the Woods"))
    // One plain venue credits the director, as Kino IKM does; the others publish only the title.
    val plain = scraped(StacjaFalenica, "Pieśni lasu").copy(directors = Seq("Vincent Munier")) +:
      Seq(KinoMikro, KinoCytadela).map(scraped(_, "Pieśni lasu"))
    val all = decorated +: plain
    val r = IdentityResolver.resolve(all, new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    withClue(all.map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      r.decisionOf(decorated.key).film shouldBe Some(1309373)
      plain.foreach(l => r.decisionOf(l.key).film shouldBe Some(1309373))
    }
  }

  "A festival's spellings chained through one another's segments" should
    "never put two films in one cluster, though no key relates the films' own listings" in {
    // PL recording run 36321731194: Kino Oaza lists a festival's films as "\"<title>\" - film,
    // V FESTIWAL WAPI 2026". Its title rules leave "film, V FESTIWAL WAPI 2026" as the clean title
    // of two of them, which is a segment of every other spelling (tier 4), while each spelling's
    // quoted segment is the plain listings' title of its own film (tier 4). The plain "Ścieżki
    // życia" and the plain "Kumotry" share no block key, so no cannot-link was drawn between them:
    // through the festival's spellings the solver joined three films' listings into one cluster
    // and the resolve failed its own invariant.
    val films = Seq(F(1127625, "The Salt Path", 2025, "Marianne Elliott", 116, 8),
      F(1454157, "Kumotry", 2025, "Emilia Śniegoska", 70, 2),
      F(1646671, "Niesamowite przygody skarpetek 3. Ale kosmos!", 2026, "Elżbieta Wąsik", 55, 1))
    def scraped(venue: Cinema, title: String, director: Option[String] = None, runtime: Option[Int] = None, year: Option[Int] = None) =
      listing(venue, title, year, director, runtime).copy(cleanTitle = services.movies.ScrapeListing.cleanTitle(venue, title, normalizer)._1)
    val plain = Seq(
      scraped(StacjaFalenica, "Ścieżki życia", Some("Marianne Elliot"), Some(116)),
      scraped(KinoDKFRumcajs, "Ścieżki życia", Some("Marianne Elliott")),
      scraped(StacjaFalenica, "Kumotry", Some("Emilia Śniegoska"), Some(70)),
      scraped(KinoMikro, "Kumotry"),
      scraped(KinoCytadela, "Niesamowite przygody skarpetek 3. Ale kosmos!", Some("Elżbieta Wąsik"), Some(55), Some(2026)))
    val festival = Seq("\" Kicia Kocia w podróży \" - film, V FESTIWAL WAPI 2026", "\" ŚCIEŻKI ŻYCIA\" - film, V FESTIWAL WAPI 2026",
      "\"Bałtyk\"- film, V FESTIWAL WAPI 2026", "\"CIEMNA STRONA MOUNT EVEREST\" - film. V FESTIWAL WAPI 2026",
      "\"Kumotry\" - film, V FESTIWAL WAPI 2026", "\"Niesamowite przygody skarpetek 3. Ale kosmos!\" - film, V FESTIWAL WAPI 2026")
      .map(scraped(KinoOaza, _))
    val all = plain ++ festival
    val r = IdentityResolver.resolve(all, new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    withClue(all.map(l => r.decisionOf(l.key).render).distinct.mkString("\n")) {
      r.decisionOf(plain(0).key).film shouldBe Some(1127625)
      r.decisionOf(plain(2).key).film shouldBe Some(1454157)
      r.decisionOf(plain(4).key).film shouldBe Some(1646671)
      // Each spelling takes its own film or none: the festival's shared suffix names no film.
      festival.map(l => r.decisionOf(l.key).film).zip(Seq(None, Some(1127625), None, None, Some(1454157), Some(1646671)))
        .foreach { case (took, own) => Seq(None, own) should contain(took) }
    }
    r.violations shouldBe 0
  }

  "A focused listing's candidates" should "be reported as its family scored them, the one its facts deny marked" in {
    val films = Seq(F(1, "Lalka", 2026, "Maciej Kawalski", 120), F(2, "Lalka", 1968, "Wojciech Jerzy Has", 159))
    val focused = listing(Multikino, "Lalka", year = Some(2026), director = Some("Maciej Kawalski"))
    val other   = listing(Helios, "Kurier")
    val report  = IdentityResolver.candidatesOf(Seq(focused, other), new FilmTable(films, normalizer), normalizer, weights)(_ == focused)
    report.map(_.label) shouldBe Seq("'Lalka' [2026] {Maciej Kawalski} ×1")
    val candidates = report.head.candidates
    candidates.map(_.tmdbId) shouldBe Seq(1, 2)
    candidates.head.denied shouldBe false
    candidates.last.denied shouldBe true
    candidates.last.render should include ("DENIED")
    // …and why: which of the listing's evidence rules it out.
    candidates.last.denial shouldBe Some("Learned(listing-film probability below the cannot-link cut)")
    candidates.last.render should include ("DENIED (Learned(listing-film probability below the cannot-link cut))")
  }

  "A programme's banner" should "not chain its films into one family, while a film's own title still joins its spellings" in {
    // UK "Coraline - Sensory Friendly Screening" and "Verity - Sensory Friendly Screening" were one
    // family by their shared banner; PL's biggest family (~75% of the corpus) was glued the same way.
    val films     = Seq(F(14836, "Coraline", 2009, "Henry Selick", 100), F(1283515, "Verity", 2026, "Michael Showalter", 110))
    val coraline  = listing(Helios, "Coraline - Sensory Friendly Screening")
    val verity    = listing(Helios, "Verity - Sensory Friendly Screening")
    val plain     = listing(Multikino, "Coraline", Some(2009), Some("Henry Selick"))
    val r = resolve(Seq(coraline, verity, plain), films)
    r.familyOf(coraline.key) should not be r.familyOf(verity.key)
    r.familyOf(coraline.key) shouldBe r.familyOf(plain.key)
  }

  "A title's segments" should "be family keys when they are a work, and not when they are a banner beside a work" in {
    // UK "Coraline - Sensory Friendly Screening": its other piece is a listing's whole title, so the
    // banner is no key. "RBO Cinema Season 2026-27: Così fan tutte": no piece is anyone's whole title,
    // so the work stays a key — the RBO's season-free "Royal Ballet and Opera: Così fan tutte" reaches
    // its record, and the Met's same-work listings stay beside it to be kept apart, only through it.
    def keys(title: String, wholes: Set[String], banners: Set[String] = Set.empty) = {
      val l = listing(Helios, title)
      TitleLinks.titleKeys(new EvidenceNode(Evidence.of(l, None, TitleDecorations.None), Seq(l)), normalizer, PinConstraints(Nil),
        wholes.map(normalizer.sanitize), banners.map(normalizer.sanitize))
    }
    val sensory = keys("Coraline - Sensory Friendly Screening", Set("Coraline"))
    sensory should contain ("t:" + normalizer.sanitize("Coraline"))
    sensory should not contain ("t:" + normalizer.sanitize("Sensory Friendly Screening"))
    keys("RBO Cinema Season 2026-27: Così fan tutte", Set.empty) should contain ("t:" + normalizer.sanitize("Così fan tutte"))
    // A piece carried by many titles and nobody's whole title is a banner, even with no work beside it.
    keys("Młode Horyzonty: Pieśni lasu", Set.empty, banners = Set("Młode Horyzonty")) should not contain ("t:" + normalizer.sanitize("Młode Horyzonty"))
    // Format and label pieces beside the listing's OWN title are no keys: PL's largest family meshed
    // ~4k listings through "2D DUB", "AF", "KNT", "napisy", "15+" beside each film's own title.
    // Cleaned, the listing's own title is the work beside those labels, as the normaliser strips them.
    val raw   = listing(Helios, "Spider-Man. Całkiem nowy dzień - 2D DUB - KNT")
    val clean = raw.copy(cleanTitle = "Spider-Man. Całkiem nowy dzień", title = "Spider-Man. Całkiem nowy dzień")
    val labelled = TitleLinks.titleKeys(new EvidenceNode(Evidence.of(clean, None, TitleDecorations.None), Seq(clean)), normalizer,
      PinConstraints(Nil), _ => false, _ => false)
    labelled should not contain ("t:" + normalizer.sanitize("2D DUB"))
    labelled should not contain ("t:" + normalizer.sanitize("KNT"))
    // ...also when the normaliser drops the label itself, so the published title normalises to the
    // cleaned one: Kino na Boku's "Lalka | PREMIERA" and "Luna i rozgadana świnka | PREMIERA" welded
    // Lalka's and Luna's families through "PREMIERA" (carried by 7 titles, under the banner spread).
    val premiere = listing(Helios, "Lalka | PREMIERA").copy(cleanTitle = "Lalka", title = "Lalka")
    TitleLinks.titleKeys(new EvidenceNode(Evidence.of(premiere, None, TitleDecorations.None), Seq(premiere)), normalizer,
      PinConstraints(Nil), _ => false, _ => false) should not contain ("t:" + normalizer.sanitize("PREMIERA"))
    // ...and beside the title's SEARCH FORM, when a label sticks to the film's piece so that neither
    // piece is anyone's whole title: Nasze Kino's "Gorzkie święta / napisy - Nasze Kino" kept its
    // venue as the work, and the venue welded Obcy, Róża, Vincent and Almodóvar into one PL family.
    val venueSuffixed = keys("Gorzkie święta / napisy - Nasze Kino", Set.empty)
    venueSuffixed should not contain ("t:" + normalizer.sanitize("Nasze Kino"))
    venueSuffixed should contain ("q:" + normalizer.searchQuery("Gorzkie święta / napisy - Nasze Kino"))
    // An event's banner beside its film is dropped, the film kept: the screening IS the film.
    val event = keys("Dzień Dziecka Księdza Jana Kaczkowskiego: Luna i rozgadana świnka",
      Set("Dzień Dziecka Księdza Jana Kaczkowskiego", "Luna i rozgadana świnka"))
    event should contain ("t:" + normalizer.sanitize("Luna i rozgadana świnka"))
    // A title naming two WORKS other listings carry whole — a double bill — welds their families: it
    // is neither one film, so neither work is its key (PL's largest family meshed ~4k listings so).
    val double = keys("Tedi i magiczna lampa + Luna i rozgadana świnka", Set("Tedi i magiczna lampa", "Luna i rozgadana świnka"))
    double should not contain ("t:" + normalizer.sanitize("Tedi i magiczna lampa"))
    double should not contain ("t:" + normalizer.sanitize("Luna i rozgadana świnka"))
  }

  "A listing's explanation" should "say why it has each family key, why a piece is no key, how it joined its family and what it decided" in {
    val films    = Seq(F(14836, "Coraline", 2009, "Henry Selick", 100), F(1283515, "Verity", 2026, "Michael Showalter", 110))
    val sensory  = listing(Helios, "Coraline - Sensory Friendly Screening")
    val verity   = listing(Helios, "Verity - Sensory Friendly Screening")
    val plain    = listing(Multikino, "Coraline", Some(2009), Some("Henry Selick"))
    val plainToo = listing(KinoMuza, "Coraline", Some(2009), Some("Henry Selick"))
    val told = IdentityResolver.explain(Seq(sensory, verity, plain, plainToo), new FilmTable(films, normalizer), normalizer, weights)(Set(sensory.key)).head
    val coraline = "t:" + normalizer.sanitize("Coraline")
    told.keys.toMap.get(coraline) shouldBe Some("a piece of its title that a listing carries whole")
    told.dropped.map(_._1) should contain ("'Sensory Friendly Screening'")
    told.dropped.toMap.apply("'Sensory Friendly Screening'") should include ("banner beside the work")
    told.chain should not be empty
    (told.keys.map(_._1) should contain (told.chain.head._1)) // the first link is one of its own keys
    told.render.mkString("\n") should include ("in this family by:")
  }

  "A family's anatomy" should "name the key that glues it and the piece it leaves without that key" in {
    val films   = Seq(F(1321666, "Lalka", 2026, "Maciej Kawalski", 162))
    val plain   = listing(Helios, "Lalka", Some(2026), Some("Maciej Kawalski"))
    val premier = listing(Multikino, "Oficjalna premiera: Lalka")
    val anatomy = IdentityResolver.familyAnatomy(Seq(plain, premier), new FilmTable(films, normalizer), normalizer, weights)(1).head
    anatomy.listings shouldBe 2
    anatomy.nodes shouldBe 2
    val glue = "t:" + normalizer.sanitize("Lalka")
    anatomy.keys should contain (glue -> 2)
    anatomy.withoutKey.toMap.get(glue) shouldBe Some(2) // the film's id key still holds them together
  }

  // Scoring a listing against its family's pool is the resolve's dominant cost (PL's 127-node
  // "Lalka" family): a family a round of `Families.grow` left unchanged keeps its scores, and a
  // cluster voting and the decisions both pool is pooled once.
  "A resolve" should "score each node once when the next round leaves its family as it was" in {
    val films = Seq(F(1651192, "Leonas, el instinto más salvaje", 2026, "Juan Manuel Cotelo", 94, 2))
    val ls    = Seq(Multikino, Helios).map(listing(_, "Leonas", Some(2026), Some("Juan Manuel Cotelo"), Some(94)))
    val r     = shipped(ls, films)
    ls.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(1651192))
    r.scorings shouldBe r.nodes + r.decisions.size
  }

  it should "pool a cluster once for its vote and its decision" in {
    val films = Seq(F(1651192, "Leonas, el instinto más salvaje", 2026, "Juan Manuel Cotelo", 94, 2))
    val l = listing(Multikino, "Leonas", None, Some("Juan Manuel Cotelo"))
    val r = shipped(Seq(l), films)
    r.decisionOf(l.key).film shouldBe None
    r.scorings shouldBe r.nodes + r.decisions.size
  }

  // A title relation reads only the listing's titles, so the venues billing one title — and a cluster read
  // under its lead's titles — share each pool film's relation instead of reading it per node.
  it should "relate a title the family's venues share to each pool film once" in {
    val films = Seq(F(1651192, "Leonas, el instinto más salvaje", 2026, "Juan Manuel Cotelo", 94, 2))
    val ls    = Seq(Multikino, Helios).map(listing(_, "Leonas", Some(2026), Some("Juan Manuel Cotelo"), Some(94)))
    val r     = shipped(ls, films)
    ls.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(1651192))
    r.scorings should be > 1
    r.titleRelations shouldBe 1
  }

  "The calibration" should "load from an artefact in its own format, the fixture as the real one" in {
    weights.version shouldBe "test-fixture-2"
    IdentityCalibration.resolver.scopes.keySet shouldBe weights.scopes.keySet
  }
}
