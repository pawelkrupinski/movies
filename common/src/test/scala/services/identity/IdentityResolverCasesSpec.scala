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
    override def film(id: Int): Answer[Option[IdentityMeasures.Film]] =
      Answer.Known(films.find(_.id == id).map(f =>
        IdentityMeasures.Film(f.title, None, Nil, Some(f.year), Some(f.runtime), Some(Seq(f.director)), None, Some(f.popularity))))
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

  "A bare listing its own evidence cannot separate between two films" should
    "follow its title's credited siblings, not the database's popularity ranking" in {
    // Two 2026 films TMDB titles "Lalka"; the more popular one is not the one the venues credit.
    val films = Seq(F(1, "Lalka", 2026, "Maciej Kawalski", 150, 5), F(2, "Lalka", 2026, "Someone Else", 95, 80))
    val credited = Seq(Multikino, Helios).map(listing(_, "Lalka", Some(2026), Some("Maciej Kawalski")))
    val bare     = Seq(KinoApollo, KinoMuza, Rialto).map(listing(_, "Lalka"))
    val r = resolve(credited ++ bare, films)
    r.decisionOf(credited.head.key).film shouldBe Some(1)
    bare.map(l => r.decisionOf(l.key).film) shouldBe Seq.fill(3)(Some(1))
    r.violations shouldBe 0
  }

  "Three films under one title" should "stay three, and a bare listing joins neither of the dated ones by title alone" in {
    val films = Seq(F(1954, "A Star Is Born", 1954, "George Cukor", 176), F(1976, "A Star Is Born", 1976, "Frank Pierson", 139),
      F(2018, "A Star Is Born", 2018, "Bradley Cooper", 136, 60))
    val old  = listing(Multikino, "A Star Is Born", Some(1954), Some("George Cukor"))
    val mid  = listing(Rialto, "A Star Is Born", Some(1976))
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

  "A listing the evidence cannot place" should "stay unmatched, and say which candidate it refused" in {
    val films = Seq(F(1, "Opętanie", 1981, "Andrzej Żuławski", 124), F(2, "Opętanie", 1973, "Someone Else", 90))
    val bare = listing(Multikino, "Opętanie")
    val d = resolve(Seq(bare), films).decisionOf(bare.key)
    d.film shouldBe None
    d.basis shouldBe ResolverDecision.Basis.BelowThreshold
    d.explanation.exists(_.startsWith("best rejected candidate")) shouldBe true
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
    IdentityCalibration.default.scopes.keySet shouldBe weights.scopes.keySet
  }
}
