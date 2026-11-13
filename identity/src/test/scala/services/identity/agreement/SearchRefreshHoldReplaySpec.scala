package services.identity.agreement

import models.{KinoMuza, KinoPalacowe, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, Dependency, FilmTable, IdentityCalibration, IdentityLookups, IdentityMeasures, Listing, PosterAnswers,
  PosterHash, Resolution, ResolverDecision, TakeOutcome, UnifiedEvidence, UnifiedRules}
import services.movies.SingleCountryNormalizer

import scala.collection.mutable

/** THE SEARCH REFRESH's lost take, replayed step by step as the worker met it: US Alamo Drafthouse "Game" x12 (prod
 *  2026-11-09 15:41, build 1cdb8ec; again 11-12 15:45, build 511987c). Its listings unchanged since 11-04, the cluster
 *  stood on tmdb 1547524 by `fill:venues.current`; a TMDB search refreshed with a NEW candidate ("I'm Game" 1250502,
 *  then "Blind Game" 1755818), the fill waited on that candidate's poster, and the projection served the model's
 *  `pooled:no-film` for 4.9 s / 6.9 s until the poster was filed and the fill took 1547524 again. The owner (2026-11-13,
 *  extending 11-12's "they should stay the same until all is resolved for the new one"): the previous take stands, typed
 *  Pending, until the new candidate's answers are in -- for at most the input gate's 10 minutes
 *  ([[AgreementStage.HoldAtMost]]). Never a film the cluster was not served before (the dropped Option B). */
class SearchRefreshHoldReplaySpec extends AnyFlatSpec with Matchers {
  import SearchRefreshHoldReplaySpec._

  "a cluster whose refreshed search names a new candidate" should "keep its previous take, Pending, until the candidate's poster is filed" in {
    val world = new World
    val boot  = world.applied()
    (boot.film, world.outcome(boot)) shouldBe ((Some(Game), TakeOutcome.Taken(s"tmdb:$Game")))
    boot.explanation.last should startWith ("filled by venues.current")

    // 15:43:41: TMDB's search for "Game" filed again, "Blind Game" among its hits; its poster not hashed yet
    world.refreshSearch()
    val waiting = world.applied()
    waiting.film shouldBe Some(Game)                       // was None: the model's pooled:no-film served
    world.outcome(waiting) shouldBe a[TakeOutcome.Pending]
    world.stage.heldNow().listings shouldBe 3

    // a re-scrape billing the same listings before the poster comes: still standing
    world.applied().film shouldBe Some(Game)

    // 15:45:40: the new candidate's poster filed -- another film's: the fill takes Game again, decided
    world.filePoster()
    val decided = world.applied()
    (decided.film, world.outcome(decided)) shouldBe ((Some(Game), TakeOutcome.Taken(s"tmdb:$Game")))
    decided.explanation.last should startWith ("filled by venues.current")
    world.stage.heldNow().listings shouldBe 0
  }

  it should "stand at most the input gate's bound, then be served as it stands, Pending" in {
    val world = new World
    world.applied().film shouldBe Some(Game)
    world.refreshSearch()
    world.applied().film shouldBe Some(Game)
    world.clock.advance(AgreementStage.HoldAtMost.minusSeconds(1))
    world.applied().film shouldBe Some(Game)
    world.clock.advanceSeconds(1)
    // nothing filed, the same decisions: the bound alone reads the cluster again
    val bounded = world.applied(sameVersion = true)
    bounded.film shouldBe None
    world.outcome(bounded) shouldBe a[TakeOutcome.Pending]
    world.stage.heldNow().listings shouldBe 0
    world.filePoster()
    world.applied().film shouldBe Some(Game)
  }

  it should "hold nothing across a change of its listings, nor after a restart" in {
    val world = new World
    world.applied().film shouldBe Some(Game)
    world.refreshSearch()
    // a venue stops billing it: another cluster, decided on what it has
    world.applied(listings = world.listings.take(2)).film shouldBe None
    // a restart keeps no previous take: the refreshed search's wait is served as the model left it
    new World(searchRefreshed = true).applied().film shouldBe None
  }
}

object SearchRefreshHoldReplaySpec {
  val Game      = 1547524
  val BlindGame = 1755818
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val venuePoster = "https://drafthouse.example/game.jpg"
  private val shown       = PosterHash(0x5a5a5a5a5a5aL)

  /** The cluster's world: three venues billing "Game" (the fill's wide billing), IMDb holding the 2026 film TMDB finds as
   *  1547524, TMDB's search -- refreshed or not with "Blind Game" -- and the posters hashed so far. */
  final class World(searchRefreshed: Boolean = false) {
    val clock = new tools.MutableClock(java.time.Instant.parse("2026-11-12T15:40:00Z"))
    private val game      = FilmTable.F(Game, "Game", 2026, "Jane Doe", 100)
    private val blindGame = FilmTable.F(BlindGame, "Blind Game", 2025, "John Roe", 90)
    private var table     = new FilmTable(if (searchRefreshed) Seq(game, blindGame) else Seq(game), normalizer)
    private val filed     = mutable.Set.empty[String]
    private val films     = mutable.Map[Int, Seq[PosterHash]](Game -> Seq(PosterHash(0L)))   // the venue's poster near no candidate: no vote, no veto
    var version = 1L
    private val lookups = new IdentityLookups {
      def hasDetail(l: Listing): Boolean = false
      def detail(l: Listing) = table.detail(l)
      def candidates(q: services.identity.CandidateQuery) = table.candidates(q)
      def film(id: Int) = table.film(id)
    }
    private val posters: PosterAnswers = new PosterAnswers {
      def venue(url: String) = Answer.Known(Option.when(url == venuePoster)(shown))
      def film(id: Int)      = films.get(id).fold[Answer[Seq[PosterHash]]](Answer.Unknown)(Answer.Known(_))
    }
    private val record   = IdentityMeasures.Film("Game", None, Nil, Some(2026), Some(100), Some(Seq("Jane Doe")), None, None)
    private val families = Map[VoterFamily, FamilyAnswers](
      VoterFamily.Imdb -> new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt39000001" -> SourceRecord(record, Map("imdb" -> "tt39000001"))))) ++
      Seq(VoterFamily.Wiki, VoterFamily.Filmweb, VoterFamily.RottenTomatoes).map(family => family -> new HeldFamilyAnswers(family, Map.empty))
    val listings: Seq[Listing] = Seq(KinoMuza, Multikino, KinoPalacowe).map(venue => FilmTable.listing(venue, "Game").copy(poster = Some(venuePoster)))
    private def fresh(): AgreementStage = new AgreementStage(families, NoVenueDetails, normalizer, IdentityCalibration.resolver,
      tmdbOf = _ => Answer.Known(Some(Game)), new InMemoryAgreementVerdicts, clock = clock, posters = posters, tmdb = Some(lookups),
      changes = _ => Some(filed.toSet), rules = UnifiedRules("t", UnifiedEvidence.Guards, Seq("venues.current")))
    val stage: AgreementStage = fresh()
    /** The model's decision: none, as prod's `pooled:no-film` -- a new object each projection, as a refreshed search hands. */
    def model(over: Seq[Listing]): Resolution = Resolution(Seq(ResolverDecision(over.map(_.key), None, 0.2, ResolverDecision.Basis.BelowThreshold, Nil)()),
      over.size, over.map(_.key -> 0).toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    private var lastModel = model(listings)
    /** One projection's apply: a new version, unless `sameVersion` (a tick with nothing filed). */
    def applied(sameVersion: Boolean = false, listings: Seq[Listing] = this.listings): ResolverDecision = {
      if (!sameVersion) { version += 1; lastModel = model(listings) }
      stage.apply(lastModel, key => listings.find(_.key == key), version).decisions.head
    }
    def outcome(decision: ResolverDecision): TakeOutcome = stage.outcomeOf(decision)
    def refreshSearch(): Unit = { table = new FilmTable(Seq(game, blindGame), normalizer); filed += Dependency.QueryFiled }
    def filePoster(): Unit = { films(BlindGame) = Seq(PosterHash(-1L)); filed += s"poster:$BlindGame" }
  }
}
