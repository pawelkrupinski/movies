package services.identity.agreement

import models.{KinoMuza, Multikino}
import services.identity.{Answer, CatalogueAnswers, FilmTable, IdentityMeasures, Listing, PosterAnswers, PosterHash, Question,
  Resolution, ResolverDecision, ScreeningDays}
import services.movies.SingleCountryNormalizer

import scala.collection.mutable

/** One history of a listing joining a cluster whose model take the evidence corrects (PL "Ktoś całkiem obcy",
 *  2026-10-25: Bałtyk's "Klub Konesera" listing joined Na Starówce's corrected "Lalka" take), as the worker's stores
 *  hold it: the answers filed so far and their versions, the family and poster answers gated on them, the agreement
 *  verdicts and keys kept across a deploy. [[started]] composes the worker's stage over it
 *  ([[AgreementStageComposition]]), its inline asks answering what they ask, as the worker's do.
 *  [[CorrectionArrivalOrderSpec]] and [[CorrectionFlipReplaySpec]] replay it. */
final class CorrectionWorld {
  import CorrectionWorld._

  private var posterFiled = false
  private var familyFiled = false
  /** The version the stores are at: one more per filing. */
  var version: Long       = 1L
  private val filings     = mutable.ArrayBuffer.empty[(Long, String)]
  def file(ids: String*): Unit = ids.foreach { id => version += 1; filings += version -> id }
  /** The joining listing's own answers come in: its poster hashed, its title's family answered. */
  def answers(): Unit = { posterFiled = true; familyFiled = true; file(s"poster|venue|$joinedPoster", "imdb|title|Klub Konesera: Lalka") }

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
    catalogue = CatalogueAnswers.Silent, attempted = tried, givenUp = _ => true, attemptsMarked = None, keys = keys)
  // the worker's inline asks: the family questions asked are answered — filed — before the apply reads them again
  private val askNow: AgreementStage.AskNow = (questions, _) =>
    if (!familyFiled && questions.exists { case Question.Family(_, asked) => joinedQuestion(asked); case _ => false }) {
      familyFiled = true; file("imdb|title|Klub Konesera: Lalka", "wiki|title|Klub Konesera: Lalka")
    }
  /** A stage as the worker composes one at boot, over this world's stores. */
  def started(): AgreementStage = AgreementStageComposition(sources, AgreementStageComposition.Effects(askNow = askNow))
}

object CorrectionWorld {
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
  def model(listings: Seq[Listing]): Resolution =
    Resolution(Seq(ResolverDecision(listings.map(_.key), Some(ModelFilm), 0.46, ResolverDecision.Basis.OwnMatch, Seq(s"own match $ModelFilm"))()),
      1, listings.map(_.key -> 0).toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
}
