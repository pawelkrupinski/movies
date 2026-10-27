package services.identity.agreement

import models.{KinoMuza, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, CatalogueAnswers, FilmTable, IdentityMeasures, Listing, PosterAnswers, PosterHash, Question,
  Resolution, ResolverDecision, ScreeningDays}
import services.movies.{ListingKey, SingleCountryNormalizer}

import scala.collection.mutable

/** A corrected take must not depend on the order its inputs arrive in: a listing joining a cluster whose model take the
 *  evidence corrects, its own poster and its title's family answers filed before it joins or after, is taken as the
 *  corrected film at every apply — never as the model's film for the seconds its answers take (PL "Ktoś całkiem obcy",
 *  2026-10-25: Bałtyk's "Klub Konesrea: Ktoś całkiem obcy" joined Na Starówce's corrected cluster, and both listings showed
 *  the model's 2007 film until the families and the poster were in, 21 s later). Built through the worker's one
 *  composition ([[AgreementStageComposition]]), its inline asks answering what they ask, as the worker's do. */
class CorrectionArrivalOrderSpec extends AnyFlatSpec with Matchers {
  import CorrectionArrivalOrderSpec._

  "a listing joining a corrected cluster" should "be taken as the corrected film at every apply, whichever of it and its answers comes first" in {
    val joinFirst    = Run(answersFirst = false)
    val answersFirst = Run(answersFirst = true)
    joinFirst.seen.flatMap(_.values).flatten should not contain ModelFilm
    answersFirst.seen.flatMap(_.values).flatten should not contain ModelFilm
    joinFirst.seen.last shouldBe answersFirst.seen.last
    joinFirst.seen.last shouldBe Map(joined.key -> Some(Corrected), first.key -> Some(Corrected))
  }

  it should "still be decided again once its own poster is in: a partial read of the posters is no final one" in {
    val run = Run(answersFirst = false)
    run.stage.wanted.collect { case Question.Poster(q) => q } shouldBe empty
    run.seen.last shouldBe Map(joined.key -> Some(Corrected), first.key -> Some(Corrected))
  }
}

object CorrectionArrivalOrderSpec {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  val ModelFilm = 1001
  val Corrected = 1002

  private val table = new FilmTable(Seq(FilmTable.F(ModelFilm, "Lalka", 2026, "Maciej Kawalski", 162), FilmTable.F(Corrected, "Lalka", 1968, "Wojciech Has", 159)), normalizer)
  private val shown = PosterHash(0x5a5a5a5a5a5aL)
  private def near(bits: Int) = PosterHash(shown.bits ^ ((1L << bits) - 1))
  private val far   = PosterHash(-1L)
  private val day   = java.time.LocalDate.parse("2026-10-25")
  private val firstPoster  = "https://multikino.example/lalka.jpg"
  private val joinedPoster = "https://muza.example/klub-konesera-lalka.jpg"
  val first: Listing  = FilmTable.listing(Multikino, "Lalka").copy(poster = Some(firstPoster), screenings = ScreeningDays.of(Seq(day)))
  val joined: Listing = FilmTable.listing(KinoMuza, "Klub Konesera: Lalka").copy(poster = Some(joinedPoster), screenings = ScreeningDays.of(Seq(day.plusDays(1))))

  private val has = SourceRecord(IdentityMeasures.Film("Lalka", None, Seq("The Doll", "Klub Konesera: Lalka"), Some(1968), Some(159), Some(Seq("Wojciech Has")), None, None),
    Map("tmdb" -> Corrected.toString))

  private def model(listings: Listing*) =
    Resolution(Seq(ResolverDecision(listings.map(_.key), Some(ModelFilm), 0.46, ResolverDecision.Basis.OwnMatch, Seq(s"own match $ModelFilm"))()),
      1, listings.map(_.key -> 0).toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)

  /** One history: the first listing alone, corrected; then the joining listing and its answers — its poster hashed and
   *  its title's family questions answered — in the order given. `seen`: every listing's take at each apply. */
  final case class Run(answersFirst: Boolean) {
    /** Filed: the joining listing's poster and its family answers. Asked inline: the family questions alone. */
    private var posterFiled  = false
    private var familyFiled  = false
    private var version      = 1L
    private val filings      = mutable.ArrayBuffer.empty[(Long, String)]
    private def file(ids: String*): Unit = ids.foreach { id => version += 1; filings += version -> id }

    private def joinedQuestion(question: String) = question.toLowerCase.contains("klub")
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
    private val sources = AgreementStageComposition.Sources(families, NoVenueDetails, table, _ => Answer.Known(None), new InMemoryAgreementVerdicts,
      _root_.tools.SpecClock.Pinned, normalizer, identities = Seq(VoterFamily.Wiki), listedOn = None,
      changes = since => Some(filings.iterator.collect { case (at, id) if at > since => id }.toSet), posters = posters,
      catalogue = CatalogueAnswers.Silent, attempted = tried, attemptsMarked = None, keys = new InMemoryAgreementKeys)
    // the worker's inline asks: the family questions asked are answered — filed — before the apply reads them again
    private val askNow: AgreementStage.AskNow = (questions, _) =>
      if (!familyFiled && questions.exists { case Question.Family(_, asked) => joinedQuestion(asked); case _ => false }) {
        familyFiled = true; file("imdb|title|Klub Konesera: Lalka", "wiki|title|Klub Konesera: Lalka")
      }
    val stage: AgreementStage = AgreementStageComposition(sources, AgreementStageComposition.Effects(askNow = askNow))

    private var listings = Seq(first)
    private def listingOf: ListingKey => Option[Listing] = key => listings.find(_.key == key)
    val seen = mutable.ArrayBuffer.empty[Map[ListingKey, Option[Int]]]
    private def applied(): Unit = {
      val resolution = stage.apply(model(listings*), listingOf, version)
      seen += resolution.decisions.flatMap(d => d.members.map(_ -> d.film)).toMap
    }

    applied()
    file("filed|elsewhere")   // past the boot's pass: what the worker's projection meets afresh is asked inline
    applied()
    private def answers(): Unit = { posterFiled = true; familyFiled = true; file(s"poster|venue|$joinedPoster", "imdb|title|Klub Konesera: Lalka") }
    if (answersFirst) answers()
    listings = Seq(first, joined)
    applied()
    if (!answersFirst) { applied(); answers() }
    applied()
    applied()
  }
}
