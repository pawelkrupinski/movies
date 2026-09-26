package services.identity

import models.{Cinema, Helios, KinoApollo, KinoMuza, Multikino, Rialto}
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

  private final case class F(id: Int, title: String, year: Int, director: String, runtime: Int, popularity: Double = 10.0)

  /** A film database of `films`: search by all-words containment (year-scoped when asked), the
   *  directors' filmographies, and each film's record. */
  private final class Table(films: Seq[F]) extends IdentityLookups {
    private def words(s: String) = services.movies.TitleContainment.tokens(normalizer.searchQuery(s)).toSet
    private def hit(f: F) = Hit(f.id, f.title, None, Some(f.year), f.popularity)
    override def hasDetail(l: Listing): Boolean = false
    override def detail(l: Listing): Answer[Option[DetailFacts]] = Answer.Known(None)
    override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = Answer.Known(q match {
      case CandidateQuery.Title(text) =>
        val want = words(text)
        films.filter(f => want.nonEmpty && want.subsetOf(words(f.title))).sortBy(-_.popularity).map(hit)
      case CandidateQuery.Director(name) => films.filter(_.director == name).map(hit)
    })
    // A record crediting nobody (an empty director) and with no runtime (0), as a broadcast's is.
    override def film(id: Int): Answer[Option[IdentityMeasures.Film]] =
      Answer.Known(films.find(_.id == id).map(f =>
        IdentityMeasures.Film(f.title, None, Nil, Some(f.year), Some(f.runtime).filter(_ > 0), Some(Seq(f.director).filter(_.nonEmpty)),
          None, Some(f.popularity))))
  }

  private def listing(venue: Cinema, title: String, year: Option[Int] = None, director: Option[String] = None,
                      runtime: Option[Int] = None): Listing =
    Listing(venue, ListingKey.Published(venue.displayName, title, year, director.toSeq), title, title, title, year,
      director.toSeq, runtime, None, None)

  private def resolve(listings: Seq[Listing], films: Seq[F]): Resolution =
    IdentityResolver.resolve(listings, new Table(films), normalizer, weights)

  private def together(r: Resolution, a: Listing, b: Listing) = r.decisionOf(a.key) eq r.decisionOf(b.key)

  "A decorated spelling" should "take the film its plain siblings' own evidence matched" in {
    val films = Seq(F(1, "Lalka", 2026, "Maciej Kawalski", 150, 5), F(2, "Lalka", 1968, "Wojciech Has", 159, 8))
    val plain = Seq(Multikino, Helios, KinoApollo).map(listing(_, "Lalka", Some(2026), Some("Maciej Kawalski")))
    val decorated = listing(KinoMuza, "Oficjalna premiera: Lalka")
    val r = resolve(plain :+ decorated, films)
    r.decisionOf(plain.head.key).film shouldBe Some(1)
    r.decisionOf(decorated.key).film shouldBe Some(1)
    together(r, plain.head, decorated) shouldBe true
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
    val off = IdentityResolver.resolveWith(Seq(byYear, byRuntime), new Table(films), normalizer, weights, IdentityResolver.Mutation.NoVoting)
    off.decisionOf(byYear.key).film shouldBe None
  }

  "The pooled vote" should "never choose a film only a director's filmography reached, for a credited or a bare member" in {
    // TMDB's search has no "Candyman"; walking Bernard Rose's filmography turns up two other films,
    // whose director agrees with the credited listing and whose titles name nothing. Its own facts
    // cannot choose between them, so the listing goes to the vote with its bare siblings.
    val walked    = Seq(F(353927, "Inside Out 4", 1992, "Bernard Rose", 99, 3), F(2, "Paperhouse", 1988, "Bernard Rose", 92, 5))
    val credited  = listing(Multikino, "Candyman (1992)", director = Some("Bernard Rose"))
    val bare      = Seq(Helios, KinoApollo).map(listing(_, "Candyman"))
    val r = resolve(credited +: bare, walked)
    (credited +: bare).foreach(l => withClue(r.decisionOf(l.key).render)(r.decisionOf(l.key).film shouldBe None))
    r.decisionOf(credited.key).basis should not be ResolverDecision.Basis.BelowThreshold

    // The film the title names, once the database has it, is still found — the walk is not what
    // decides it.
    val named = resolve(credited +: bare, walked :+ F(9529, "Candyman", 1992, "Bernard Rose", 99, 20))
    (credited +: bare).map(l => named.decisionOf(l.key).film) shouldBe Seq.fill(3)(Some(9529))
    named.violations shouldBe 0
  }

  "A listing the evidence cannot place" should "stay unmatched, and say which candidate it refused" in {
    val films = Seq(F(1, "Opętanie", 1981, "Andrzej Żuławski", 124), F(2, "Opętanie", 1973, "Someone Else", 90))
    val bare = listing(Multikino, "Opętanie")
    val d = resolve(Seq(bare), films).decisionOf(bare.key)
    d.film shouldBe None
    d.basis shouldBe ResolverDecision.Basis.BelowThreshold
    d.explanation.exists(_.startsWith("best rejected candidate")) shouldBe true
  }

  "Nowe Horyzonty's 83-minute Your Name re-release" should "take Shinkai's film under the shipped artefact (known regression, pending)" in {
    // §15.8: the r5 artefact VETOES it on its own facts (0.06 < the certified cut) and takes a bare
    // sibling down with it. Pending, so the refit that decides it right flips this red.
    val films = Seq(F(372058, "Twoje imię", 2016, "Makoto Shinkai", 106, 30))
    val nh = Listing(KinoMuza, ListingKey.Published(KinoMuza.displayName, "Twoje imię", None, Seq("Makoto Shinkai")), "Twoje imię",
      "Twoje imię", "Twoje imię", None, Seq("Makoto Shinkai"), Some(83), None, Some("Your Name (re-release)"))
    pendingUntilFixed {
      IdentityResolver.resolve(Seq(nh), new Table(films), normalizer, IdentityCalibration.resolver).decisionOf(nh.key).film shouldBe Some(372058)
    }
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
    val before = IdentityResolver.resolve(Seq(bare), new Table(films), normalizer, withoutClasses).decisionOf(bare.key)
    before.film shouldBe None
    before.basis shouldBe ResolverDecision.Basis.BelowThreshold
    val after = IdentityResolver.resolve(Seq(bare), new Table(films), normalizer, withTopHitClass(0.5)).decisionOf(bare.key)
    after.film shouldBe Some(39264)
    after.basis shouldBe ResolverDecision.Basis.OwnMatch
    after.confidence shouldBe 0.99
    after.explanation.head should include ("exact top hit")
  }

  it should "not take it past its class: a same-titled rival, a banner, a published fact against it, a rival that fits better" in {
    val megalon = F(39264, "Godzilla vs. Megalon", 1973, "Jun Fukuda", 82, 0.6)
    val remake  = F(7, "Godzilla vs. Megalon", 2031, "Someone Else", 120, 0.5)
    def film(l: Listing, films: Seq[F], rivals: Double) =
      IdentityResolver.resolve(Seq(l), new Table(films), normalizer, withTopHitClass(rivals)).decisionOf(l.key).film
    // A rival the class was not measured with.
    film(listing(Rialto, "Godzilla vs. Megalon"), Seq(megalon, remake), rivals = 0.5) shouldBe None
    // A banner segment's first hit is not the listing's exact title.
    film(listing(Rialto, "Kino Nocne: Godzilla vs. Megalon"), Seq(megalon), rivals = 0.5) shouldBe None
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
    val r = IdentityResolver.resolve(season +: bare, new Table(films), normalizer, withTopHitClass(0.5, weights))
    (season +: bare).foreach(l => withClue(l.title)(r.decisionOf(l.key).film should not be Some(29993)))
    r.violations shouldBe 0
  }

  "Curation pins" should "override the evidence: a pinned film, a denied one, and a pinned group" in {
    def pin(ls: Seq[Listing], claim: PinClaim) = Pin(ls.map(_.key), claim, "spec", "test", java.time.Instant.EPOCH)
    val films = Seq(F(1, "Opętanie", 1981, "Andrzej Żuławski", 124), F(2, "Opętanie", 1973, "Someone Else", 90))
    val bare  = listing(Multikino, "Opętanie | klasyka w 4k")
    val dated = listing(Helios, "Opętanie", Some(1981), Some("Andrzej Żuławski"))
    val other = listing(KinoMuza, "Possession")
    def withPins(ps: Pin*) = IdentityResolver.resolve(Seq(bare, dated, other), new Table(films), normalizer, weights,
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
    val r = IdentityResolver.resolve(Seq(met, bare), new Table(films), normalizer, withRule)
    r.decisionOf(met.key).film should not be Some(1)
    r.decisionOf(met.key).basis shouldBe ResolverDecision.Basis.Vetoed
    // The bare listing publishes neither a year nor a director: nothing to veto on.
    val alone = IdentityResolver.resolve(Seq(bare), new Table(films), normalizer, withRule)
    alone.edges.filterNot(_.must) shouldBe empty
    val bareMeasures = IdentityMeasures.listingFilm(IdentityMeasures.Listing("Samson i Dalila"),
      IdentityMeasures.Film("Samson i Dalila", year = Some(1949), directors = Some(Seq("Cecil B. DeMille"))), None, 0, 0)
    ListingConstraints.learned(withRule, "listing-film", bareMeasures, probability = 0.5) shouldBe None
  }

  "The calibration" should "load from an artefact in its own format, the fixture as the real one" in {
    weights.version shouldBe "test-fixture-2"
    IdentityCalibration.resolver.scopes.keySet shouldBe weights.scopes.keySet
  }
}
