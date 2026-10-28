package services.identity.agreement

import models.{KinoMuza, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, CatalogueAnswers, FilmTable, IdentityMeasures, Listing, PosterAnswers, PosterHash, Question, Resolution,
  ResolverDecision, ScreeningDays, TakeFlips}
import services.movies.{ListingKey, SingleCountryNormalizer}

import scala.collection.mutable

/** THE SEEDED FLIP the convergence leg's flip check must catch (scripts.IdentityDrift, [[TakeFlips]]): "own match first,
 *  correction later", replayed step by step as the leg replays a corpus — a boot, a listing joining a take the evidence
 *  corrects, applies with nothing new, its answers filed, a re-scrape, a deploy — every listing's served take recorded at
 *  each apply. PL "Ktoś całkiem obcy" (2026-10-25): Bałtyk's "Klub Konesera" listing joined Na Starówce's corrected take
 *  and both were served the model's film until its answers came in, then the correction again (UK "The Stranger" at
 *  Riverside Woodbridge flips the same way, 1429348 <-> 848791). Built through the worker's composition, its inline asks
 *  answering what they ask, as the worker's do. */
class CorrectionFlipReplaySpec extends AnyFlatSpec with Matchers {
  import CorrectionFlipReplaySpec._

  "the leg's flip check" should "find no flip in a replay of a listing joining a corrected take" in {
    val replay = Replay()
    replay.flips.flips.map(_.line) shouldBe empty
    replay.flips.recorded shouldBe 7
    replay.last shouldBe Map(first.key -> Some(Corrected), joined.key -> Some(Corrected))
  }
}

object CorrectionFlipReplaySpec {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  val ModelFilm = 1001
  val Corrected = 1002

  private val table = new FilmTable(Seq(FilmTable.F(ModelFilm, "Lalka", 2026, "Maciej Kawalski", 162), FilmTable.F(Corrected, "Lalka", 1968, "Wojciech Has", 159)), normalizer)
  private val shown        = PosterHash(0x5a5a5a5a5a5aL)
  private def near(bits: Int) = PosterHash(shown.bits ^ ((1L << bits) - 1))
  private val far          = PosterHash(-1L)
  private val day          = java.time.LocalDate.parse("2026-10-25")
  private val firstPoster  = "https://multikino.example/lalka.jpg"
  private val joinedPoster = "https://muza.example/klub-konesera-lalka.jpg"
  val first: Listing  = FilmTable.listing(Multikino, "Lalka").copy(poster = Some(firstPoster), screenings = ScreeningDays.of(Seq(day)))
  val joined: Listing = FilmTable.listing(KinoMuza, "Klub Konesera: Lalka").copy(poster = Some(joinedPoster), screenings = ScreeningDays.of(Seq(day.plusDays(1))))

  private val has = SourceRecord(IdentityMeasures.Film("Lalka", None, Seq("The Doll", "Klub Konesera: Lalka"), Some(1968), Some(159), Some(Seq("Wojciech Has")), None, None),
    Map("tmdb" -> Corrected.toString))

  /** The model's take: its own match, the 2026 film, over every listing it holds. */
  private def model(listings: Seq[Listing]) =
    Resolution(Seq(ResolverDecision(listings.map(_.key), Some(ModelFilm), 0.46, ResolverDecision.Basis.OwnMatch, Seq(s"own match $ModelFilm"))()),
      1, listings.map(_.key -> 0).toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)

  /** One replay, as the leg's worker runs one: each step's events, then an apply whose takes [[flips]] records. */
  final case class Replay() {
    private var posterFiled = false
    private var familyFiled = false
    private var version     = 1L
    private val filings     = mutable.ArrayBuffer.empty[(Long, String)]
    private def file(ids: String*): Unit = ids.foreach { id => version += 1; filings += version -> id }

    private def joinedQuestion(question: String) = question.toLowerCase(java.util.Locale.ROOT).contains("klub")
    private def family(of: VoterFamily, inner: FamilyAnswers): FamilyAnswers = new FamilyAnswers {
      val family: VoterFamily = of
      private def gated[A](question: String)(answer: => Answer[A]): Answer[A] = if (joinedQuestion(question) && !familyFiled) Answer.Unknown else answer
      def titled(text: String): Answer[Seq[SourceHit]]     = gated(text)(inner.titled(text))
      def directedBy(name: String): Answer[Seq[SourceHit]] = inner.directedBy(name)
      def record(id: String): Answer[Option[SourceRecord]] = inner.record(id)
      override def fresh(question: String): Boolean        = !(joinedQuestion(question) && !familyFiled) && inner.fresh(question)
    }
    private val families: Map[VoterFamily, FamilyAnswers] = VoterFamily.values.map { f =>
      val held = if (f == VoterFamily.Imdb || f == VoterFamily.Wiki) new HeldFamilyAnswers(f, Map("x" -> has)) else new HeldFamilyAnswers(f, Map.empty)
      f -> family(f, held)
    }.toMap
    private val posters: PosterAnswers = new PosterAnswers {
      def venue(url: String): Answer[Option[PosterHash]] =
        if (url == firstPoster) Answer.Known(Some(shown)) else if (posterFiled) Answer.Known(Some(near(2))) else Answer.Unknown
      def film(tmdbId: Int): Answer[Seq[PosterHash]] = Answer.Known(if (tmdbId == Corrected) Seq(near(3)) else Seq(far))
    }
    private def tried(question: Question): Boolean = question match {
      case Question.Poster(AgreementStage.PosterQuestion.Venue(url)) => url != joinedPoster || posterFiled
      case Question.Family(_, asked)                                 => !joinedQuestion(asked) || familyFiled
      case _                                                         => true
    }
    /** Kept across a deploy, as the worker's stores keep them. */
    private val verdicts = new InMemoryAgreementVerdicts
    private val keys     = new InMemoryAgreementKeys
    private val sources  = AgreementStageComposition.Sources(families, NoVenueDetails, table, _ => Answer.Known(None), verdicts,
      _root_.tools.SpecClock.Pinned, normalizer, identities = Seq(VoterFamily.Wiki), listedOn = None,
      changes = since => Some(filings.iterator.collect { case (at, id) if at > since => id }.toSet), posters = posters,
      catalogue = CatalogueAnswers.Silent, attempted = tried, attemptsMarked = None, keys = keys)
    // the worker's inline asks: the family questions asked are answered — filed — before the apply reads them again
    private val askNow: AgreementStage.AskNow = (questions, _) =>
      if (!familyFiled && questions.exists { case Question.Family(_, asked) => joinedQuestion(asked); case _ => false }) {
        familyFiled = true; file("imdb|title|Klub Konesera: Lalka", "wiki|title|Klub Konesera: Lalka")
      }
    private def started(): AgreementStage = AgreementStageComposition(sources, AgreementStageComposition.Effects(askNow = askNow))
    private var stage    = started()

    val flips = new TakeFlips
    var last  = Map.empty[ListingKey, Option[Int]]
    private var listings = Seq(first)
    private var cluster  = Set.empty[ListingKey]
    private var steps    = 0
    /** One apply after `events`: its takes recorded, a listing's inputs moved when answers were filed or its cluster changed. */
    private def applied(events: String*): Unit = {
      val resolution = stage.apply(model(listings), key => listings.find(_.key == key), version)
      steps += 1
      val now = listings.map(_.key).toSet
      flips.record(s"$steps ${if (events.isEmpty) "re-apply" else events.mkString("+")}",
        resolution.decisions.iterator.flatMap(d => d.members.map(_ -> TakeFlips.Served(d.film.fold("none")(id => s"tmdb:$id"), s"${d.basis}"))),
        _ => events.contains("answer") || now != cluster)
      cluster = now
      last = resolution.decisions.flatMap(d => d.members.map(_ -> d.film)).toMap
    }

    applied("boot")
    file("filed|elsewhere") // past the boot's pass: what the worker's projection meets afresh is asked inline
    applied("answer")
    listings = Seq(first, joined)
    applied("scrape")
    applied()
    posterFiled = true; familyFiled = true; file(s"poster|venue|$joinedPoster", "imdb|title|Klub Konesera: Lalka")
    applied("answer")
    applied("scrape")
    stage = started()
    applied("deploy")
  }
}
