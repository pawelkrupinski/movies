package services.identity.agreement

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, CandidateQuery, Dependency, DetailFacts, FilmTable, Hit, IdentityCalibration, IdentityLookups,
  IdentityMeasures, Listing, ProseWords, Question}
import services.movies.{ListingKey, SingleCountryNormalizer}

import scala.collection.mutable

/** One digest, one reader (docs/design/identity-no-drift.md, "One dependency index"): every kind of dependency a receipt
 *  records is recorded in the form a re-read compares it in, so a receipt read again over unchanged inputs stands — and
 *  one whose input moved stands no more, for that dependency alone. The year once recorded as its bare text and compared
 *  as `Known(<year>)` never stood a re-read (9627359ec): this holds every kind to it at once. */
class DependencyReadSpec extends AnyFlatSpec with Matchers {
  private val film    = 913760
  private val query   = CandidateQuery.Title("lalka")
  private val listing = FilmTable.listing(KinoMuza, "Lalka", year = Some(2025))

  /** Every input a dependency reads, each one movable on its own. */
  private final class World {
    var record   = Option(IdentityMeasures.Film("Lalka", year = Some(2025)))
    var release  = Option(java.time.LocalDate.of(2025, 11, 7))
    var cast     = Option(Seq("Kamila Urzędowska"))
    var overview = Option(ProseWords.ofDescription(Seq("Warsaw, 1878.")))
    var hits     = Seq(Hit(film, "Lalka", None, Some(2025), 1.0))
    var page     = Option(DetailFacts(Some(2025), Seq("Maciej Kawulski"), Some(150), None))
    var family   = Map("tt1" -> SourceRecord(IdentityMeasures.Film("Lalka", year = Some(2025)), Map("imdb" -> "tt1")))
    var titled   = Seq(SourceHit("tt1", "Lalka", None, Some(2025)))
    var tried    = Set.empty[Question]
    val tmdb: IdentityLookups = new IdentityLookups {
      def hasDetail(l: Listing): Boolean                                       = false
      def detail(l: Listing): Answer[Option[DetailFacts]]                      = Answer.Known(None)
      def candidates(q: CandidateQuery): Answer[Seq[Hit]]                      = Answer.Known(hits)
      def film(id: Int): Answer[Option[IdentityMeasures.Film]]                 = Answer.Known(record)
      override def releaseDay(id: Int): Answer[Option[java.time.LocalDate]]    = Answer.Known(release)
      override def cast(id: Int): Answer[Option[Seq[String]]]                  = Answer.Known(World.this.cast)
      override def overview(id: Int): Answer[Option[ProseWords]]               = Answer.Known(World.this.overview)
    }
    val venues: IdentityLookups = new IdentityLookups {
      def hasDetail(l: Listing): Boolean                       = true
      def detail(l: Listing): Answer[Option[DetailFacts]]      = page.fold[Answer[Option[DetailFacts]]](Answer.Unknown)(p => Answer.Known(Some(p)))
      def candidates(q: CandidateQuery): Answer[Seq[Hit]]      = Answer.Known(Nil)
      def film(id: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Known(None)
    }
    val answers: FamilyAnswers = new FamilyAnswers {
      val family: VoterFamily = VoterFamily.Imdb
      def titled(text: String): Answer[Seq[SourceHit]]     = Answer.Known(World.this.titled)
      def directedBy(name: String): Answer[Seq[SourceHit]] = Answer.Known(Nil)
      def record(id: String): Answer[Option[SourceRecord]] = World.this.family.get(id).fold[Answer[Option[SourceRecord]]](Answer.Unknown)(r => Answer.Known(Some(r)))
    }
    val stage = new AgreementStage(Map(VoterFamily.Imdb -> answers), venues, SingleCountryNormalizer.titleNormalizer, IdentityCalibration.resolver,
      tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned, tmdb = Some(tmdb),
      attempted = q => tried(q))
    stage.yearNow = 2026
    val listingOf: ListingKey => Option[Listing] = Map(listing.key -> listing).get

    /** A receipt of every kind, recorded as the stage records each: its lookups through [[AgreementStage.Noting]], a
     *  family's answers through [[AgreementStage.Recording]], the year and an attempt through the one reader. */
    def recorded(): Map[Dependency, Long] = {
      val reads = mutable.Map.empty[Dependency, Long]
      stage.receipting(reads) {
        val looked = stage.looked.get
        looked.film(film); looked.releaseDay(film); looked.cast(film); looked.overview(film); looked.candidates(query)
        stage.venuesLooked.detail(listing)
      }
      val recording = new AgreementStage.Recording(answers, mutable.Set.empty, reads)
      recording.titled("Lalka"); recording.record("tt1"); recording.record("tt2")
      Seq[Dependency](Dependency.Year, Dependency.Attempted(Question.Record(film))).foreach(read => stage.readNow(read).foreach(reads(read) = _))
      reads.toMap
    }
    def moved(receipt: Map[Dependency, Long]): Set[Dependency] =
      receipt.collect { case (read, digest) if !stage.digestNow(read, listingOf).contains(digest) => read }.toSet
  }

  private val kinds: Seq[(String, World => Unit, Dependency)] = Seq(
    ("a TMDB record", w => w.record = w.record.map(_.copy(year = Some(2024))), Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Answer, Question.FilmRecord(film))),
    ("its release day", w => w.release = None, Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Release, Question.FilmRecord(film))),
    ("its cast", w => w.cast = Some(Seq("Someone Else")), Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Cast, Question.FilmRecord(film))),
    ("its descriptions", w => w.overview = None, Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Overview, Question.FilmRecord(film))),
    ("a TMDB search", w => w.hits = Nil, Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Answer, Question.Candidate(query))),
    ("a venue page", w => w.page = None, Dependency.Lookup(Dependency.Via.Venues, Dependency.Aspect.Answer, Question.VenuePage(listing.key))),
    ("a family's search", w => w.titled = Nil, Dependency.Answer(Question.Family(VoterFamily.Imdb, "title|Lalka"))),
    ("a family's record", w => w.family = w.family.map { case (id, r) => id -> r.copy(crossIds = Map.empty) }, Dependency.Answer(Question.Family(VoterFamily.Imdb, "record|tt1"))),
    ("a family's record answered later", w => w.family += "tt2" -> w.family("tt1"), Dependency.Answer(Question.Family(VoterFamily.Imdb, "record|tt2"))),
    ("an attempt", w => w.tried += Question.Record(film), Dependency.Attempted(Question.Record(film))),
    ("the year", w => w.stage.yearNow = 2027, Dependency.Year))

  "a receipt of every kind" should "stand when read again over the inputs it was recorded over" in {
    val w       = new World
    val receipt = w.recorded()
    receipt.keySet shouldBe kinds.map(_._3).toSet
    w.moved(receipt) shouldBe empty
  }

  kinds.foreach { case (what, move, read) =>
    it should s"stand no more for $what alone once it moves" in {
      val w       = new World
      val receipt = w.recorded()
      move(w)
      w.moved(receipt) shouldBe Set(read)
      // and recorded afresh, it stands again: the form recorded is the form compared
      w.moved(w.recorded()) shouldBe empty
    }
  }
}
