package scripts

import tools.ScratchDirectories
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{ResolverDecision, TakeChange}
import services.movies.ListingKey
import tools.ProdFilm

/** The off-worker drift check joins each recorded listing to the film prod screened it as and names every one whose TMDB
 *  film a whole resolve takes otherwise — but not a listing prod screens in no slot, nor one the whole resolve left on a
 *  model gap, nor the listings of a slot the whole resolve splits. */
class IdentityLiveDriftSpec extends AnyFlatSpec with Matchers with ScratchDirectories {
  private def key(venue: String, title: String): ListingKey = ListingKey.Published(venue, title, None, Nil)
  private def decision(k: ListingKey, film: Option[Int], unanswered: Int = 0) =
    ResolverDecision(Seq(k), film, 0.5, ResolverDecision.Basis.OwnMatch, Seq("why"), unanswered = unanswered)()

  private val muza   = key("Kino Muza", "Ojciec chrzestny")
  private val helios = key("Helios", "Rose")
  private val raices = key("Museum-Lichtspiele", "Raíces")
  private val gone   = key("Rialto", "Klondike")
  private val waits  = key("Kino Kijów", "Nowy film")
  private val unseen = key("Kino Pod Baranami", "Kos")
  private val split2 = key("Kino Muza", "Ojciec chrzestny 2D")
  private val live = Map("muza|oc" -> Some(238), "helios|rose" -> Some(1176962), "lichtspiele|raices" -> None,
    "rialto|klondike" -> Some(7), "kijow|nowy" -> Some(5), "muza|oc2" -> Some(240))
  private val whole = Map(
    muza -> decision(muza, Some(238)), helios -> decision(helios, Some(1712584)), raices -> decision(raices, Some(122769)),
    gone -> decision(gone, None), waits -> decision(waits, None, unanswered = 2), unseen -> decision(unseen, Some(9)),
    split2 -> decision(split2, Some(241)))
  private val listings = Seq(muza -> Seq("muza|oc"), helios -> Seq("helios|rose"), raices -> Seq("lichtspiele|raices"),
    gone -> Seq("rialto|klondike"), waits -> Seq("kijow|nowy"), unseen -> Seq("pod-baranami|kos"), split2 -> Seq("muza|oc2"))

  "compare" should "name each listing whose TMDB film the whole resolve takes otherwise, by what adopting it would do" in {
    val report = IdentityLiveDrift.compare(listings, live, whole)
    report.drifted.map(d => d.listing -> d.kind) should contain theSameElementsAs Seq(
      helios -> TakeChange.Kind.Changed.label, raices -> TakeChange.Kind.Gained.label, gone -> TakeChange.Kind.Lost.label,
      split2 -> TakeChange.Kind.Changed.label)
    report.unsettled shouldBe 1
    report.compared shouldBe 5
    report.listings shouldBe 7
  }

  it should "leave out a slot whose listings the whole resolve splits between films" in {
    val both = Seq(muza -> Seq("muza|oc"), split2 -> Seq("muza|oc"))
    val report = IdentityLiveDrift.compare(both, live, whole)
    report.compared shouldBe 0
    report.drifted shouldBe empty
  }

  it should "join a listing through its page's own slot first, else the venue's slot for its title" in {
    IdentityLiveDrift.compare(Seq(muza -> Seq("muza|oc#1f", "muza|oc")), live, whole).compared shouldBe 1
    IdentityLiveDrift.compare(Seq(helios -> Seq("helios|rose#2a", "helios|rose")), live + ("helios|rose#2a" -> Some(1712584)), whole)
      .drifted shouldBe empty
  }

  "liveBySlot" should "keep a slot only while one of prod's films names it" in {
    val films = Seq(ProdFilm("a", "rose|", Some(1176962), None, None, Set.empty, Seq("helios|rose", "muza|rose")),
      ProdFilm("b", "rose|2025", None, None, None, Set.empty, Seq("muza|rose")))
    IdentityLiveDrift.liveBySlot(films) shouldBe Map("helios|rose" -> Some(1176962))
  }

  "filedOf" should "refuse a --families capture that is not there, never read it as no answers filed" in {
    val missing = tempDirectory("live-drift").resolve("de.json.gz")
    IdentityLiveDrift.filedOf(Some(missing), models.Country.Germany) shouldBe Left(s"no --families capture at $missing")
    IdentityLiveDrift.filedOf(None, models.Country.Germany).map(_.isEmpty) shouldBe Right(true)
  }

  "parse" should "tell --families none apart from no --families at all" in {
    IdentityLiveDrift.parse(List("de", "--families", "none")).families shouldBe Some(None)
    IdentityLiveDrift.parse(List("de")).families shouldBe None
  }

  "Differences" should "name in the TSV's header what prod's inputs were and every input the whole side reads otherwise" in {
    val differences = IdentityLiveDrift.Differences(pins = 2, familyAnswers = 77132, acceptedListings = 1237,
      lookups = "9 TMDB answer(s), 2 whole document(s), 84 venue page(s)", capture = "de.json.gz")
    differences.header.head shouldBe "# inputs: prod's 2 pin(s), 77132 family answer(s), 1237 accepted listing(s) and 9 TMDB answer(s), " +
      "2 whole document(s), 84 venue page(s) as recorded, the capture de.json.gz beneath"
    differences.header.tail.map(_.takeWhile(_ != '—').stripPrefix("# differs: ").trim) shouldBe
      IdentityLiveDrift.Differences.Otherwise.map(_._1)
    (differences.json \ "differs" \ "stored verdicts").as[String] should include("keeps nothing")
  }

  "A drifted listing" should "wait on asks when prod's filed answers alone take it as prod does, and only then" in {
    val drift = IdentityLiveDrift.Drift(raices, "lichtspiele|raices", None, decision(raices, Some(122769)))
    drift.waitsOnAsks shouldBe false
    drift.copy(filedOnly = Some(decision(raices, None))).waitsOnAsks shouldBe true
    drift.copy(filedOnly = Some(decision(raices, Some(122769)))).waitsOnAsks shouldBe false
  }

  // The model's input beyond the listings billed (ListingRetention): what prod's worker kept, captured beside the corpus
  // (ProdRetained) and read back as its take-up resumes it -- every replay of a recording reads it so.
  "the model's input" should "hold a recorded listing whose showtimes ended while another venue bills its title, and not once none does" in {
    import models.{CinemaMovie, Helios, Movie, Multikino, Showtime}
    val normalizer = services.movies.SingleCountryNormalizer.titleNormalizer
    def listings(byVenue: (models.Cinema, String)*) = services.identity.ModelRead.corpus(byVenue.map { case (cinema, title) =>
      cinema -> Seq(CinemaMovie(Movie(title), cinema, None, None, None, Nil, Nil, Seq(Showtime(java.time.LocalDateTime.of(2026, 10, 1, 18, 0), None))))
    }, normalizer)
    val root  = tempDirectory("prod-retained")
    val ended = listings(Multikino -> "Lalka")                                // Multikino's run ended: prod's worker kept it
    tools.ProdRetained.write(root, "pl", ended)
    val recorded = tools.ProdRetained.readFrom(root, "pl")
    recorded shouldBe ended                                                   // read back as written, its facts whole
    IdentityLiveDrift.modelInput(listings(Helios -> "Lalka"), recorded, normalizer).map(_.key) should contain(ended.head.key)
    IdentityLiveDrift.modelInput(listings(Helios -> "Matilda"), recorded, normalizer).map(_.key) should not contain ended.head.key
  }
}
