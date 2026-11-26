package services.identity.agreement

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{FamilyIndex, FilmTable, IdentityLookups, LookupLedger, ForwardingIdentityLookups, ReadEvent, RecordingLookups}
import services.identity.measures.{Film, Listing}
import services.identity.vocabulary.{Answer, CandidateQuery, Dependency, DetailFacts, Hit, Question, VoterFamily}
import tools.Equivalence

import scala.collection.mutable

/** The one [[RecordingAnswers]] and [[RecordingLookups]] feed the same receipt, trace tape and family-index digests as the
 *  four wrappers they replaced (frozen below, as they were): over scripted reads of every kind, every line the old
 *  wrappers recorded is the one the new ones do, byte for byte. */
class RecordingWrappersEquivalenceSpec extends AnyFlatSpec with Matchers {

  private sealed trait Op
  private final case class Title(text: String) extends Op
  private final case class Director(name: String) extends Op
  private final case class Rec(id: String) extends Op
  private final case class Showing(venue: String) extends Op
  private final case class Detail(i: Int) extends Op
  private final case class Has(i: Int) extends Op
  private final case class Cand(i: Int) extends Op
  private final case class FilmOf(i: Int) extends Op
  private final case class Release(i: Int) extends Op
  private final case class Cast(i: Int) extends Op
  private final case class Overview(i: Int) extends Op

  private def listingOf(i: Int): Listing = FilmTable.listing(KinoMuza, s"Film $i", year = Some(2000 + i))

  private final class Answers extends FamilyAnswers {
    val family: VoterFamily = VoterFamily.Imdb
    private def known(text: String): Boolean = text.length % 2 == 0
    def titled(text: String): Answer[Seq[SourceHit]]     = if (known(text)) Answer.Known(Seq(SourceHit(text, text, None, None))) else Answer.Unknown
    def directedBy(name: String): Answer[Seq[SourceHit]] = if (known(name)) Answer.Known(Nil) else Answer.Unknown
    def record(id: String): Answer[Option[SourceRecord]] = if (known(id)) Answer.Known(None) else Answer.Unknown
    override def showing(venue: String): Answer[Seq[services.identity.agreement.Showing]] = if (known(venue)) Answer.Known(Nil) else Answer.Unknown
    override def fresh(question: String): Boolean = !question.contains("stale")
  }

  private final class Lookups extends IdentityLookups {
    private def known(i: Int): Boolean = i % 3 != 0
    def hasDetail(listing: Listing): Boolean = listing.year.exists(_ % 2 == 0)
    def detail(listing: Listing): Answer[Option[DetailFacts]] = if (known(listing.year.getOrElse(0))) Answer.Known(None) else Answer.Unknown
    def candidates(query: CandidateQuery): Answer[Seq[Hit]] = if (known(query.toString.length)) Answer.Known(Nil) else Answer.Unknown
    def film(tmdbId: Int): Answer[Option[Film]] = if (known(tmdbId)) Answer.Known(None) else Answer.Unknown
    override def cast(tmdbId: Int): Answer[Option[Seq[String]]] = if (known(tmdbId)) Answer.Known(Some(Seq(s"Actor $tmdbId"))) else Answer.Unknown
  }

  // ---- the old wrappers, frozen -------------------------------------------------------------------------------------
  private final class FrozenTaping(inner: FamilyAnswers, tape: mutable.ArrayBuffer[String]) extends FamilyAnswers {
    val family: VoterFamily = inner.family
    private def taped[A](question: String, answer: Answer[A]): Answer[A] = {
      tape += s"family|$question|${Dependency.digest(answer)}|${answer == Answer.Unknown}"
      answer
    }
    def titled(text: String): Answer[Seq[SourceHit]]     = taped(s"title|$text", inner.titled(text))
    def directedBy(name: String): Answer[Seq[SourceHit]] = taped(s"director|$name", inner.directedBy(name))
    def record(id: String): Answer[Option[SourceRecord]] = taped(s"record|$id", inner.record(id))
    override def showing(venue: String): Answer[Seq[services.identity.agreement.Showing]] = taped(s"showing|$venue", inner.showing(venue))
    override def fresh(question: String): Boolean        = inner.fresh(question)
  }
  private final class FrozenTapingVenues(inner: IdentityLookups, tape: mutable.ArrayBuffer[String]) extends ForwardingIdentityLookups(inner) {
    override def hasDetail(listing: Listing): Boolean = { val has = inner.hasDetail(listing); tape += s"has|${listing.key}|$has"; has }
    override def detail(listing: Listing): Answer[Option[DetailFacts]] = {
      val answer = inner.detail(listing); tape += s"detail|${listing.key}|${Dependency.digest(answer)}"; answer
    }
  }
  private final class FrozenRecording(answers: FamilyAnswers, asked: mutable.Set[Question], reads: mutable.Map[Dependency, Long]) extends FamilyAnswers {
    val family: VoterFamily = answers.family
    private def noted[A](question: String, answer: Answer[A]): Answer[A] = {
      AgreementEvidence.noted(answers, question, Dependency.digest(answer), answer == Answer.Unknown, asked, reads)
      answer
    }
    def titled(text: String): Answer[Seq[SourceHit]]     = noted(s"title|$text", answers.titled(text))
    def directedBy(name: String): Answer[Seq[SourceHit]] = noted(s"director|$name", answers.directedBy(name))
    def record(id: String): Answer[Option[SourceRecord]] = noted(s"record|$id", answers.record(id))
    override def showing(venue: String): Answer[Seq[services.identity.agreement.Showing]] = noted(s"showing|$venue", answers.showing(venue))
  }
  private final class FrozenRecordingLookups(underlying: IdentityLookups) extends FamilyIndex.ReadingLookups {
    val answers = scala.collection.concurrent.TrieMap.empty[Dependency, Answer[?]]
    val detailed = scala.collection.concurrent.TrieMap.empty[models.ListingKey, Boolean]
    protected def read[A](dependency: Dependency, ask: => Answer[A]): Answer[A] = { val answer = ask; answers.update(dependency, answer); answer }
    def reads: Map[Dependency, Long] = answers.toMap.map { case (dependency, answer) => dependency -> Dependency.digest(answer) }
    def hasDetail(listing: Listing): Boolean = { val has = underlying.hasDetail(listing); detailed.update(listing.key, has); has }
    protected def detailOf(listing: Listing) = underlying.detail(listing)
    protected def searchOf(query: CandidateQuery) = underlying.candidates(query)
    protected def recordOf(tmdbId: Int) = underlying.film(tmdbId)
    protected def releaseOf(tmdbId: Int) = underlying.releaseDay(tmdbId)
    protected def castOf(tmdbId: Int) = underlying.cast(tmdbId)
    protected def overviewOf(tmdbId: Int) = underlying.overview(tmdbId)
  }

  // ---- what each side records over a script ---------------------------------------------------------------------------
  private def play(script: Seq[Op], answers: FamilyAnswers, lookups: IdentityLookups): Unit = script.foreach {
    case Title(t)    => answers.titled(t)
    case Director(n) => answers.directedBy(n)
    case Rec(id)     => answers.record(id)
    case Showing(v)  => answers.showing(v)
    case Detail(i)   => lookups.detail(listingOf(i))
    case Has(i)      => lookups.hasDetail(listingOf(i))
    case Cand(i)     => lookups.candidates(CandidateQuery.Title(s"Film $i"))
    case FilmOf(i)   => lookups.film(i)
    case Release(i)  => lookups.releaseDay(i)
    case Cast(i)     => lookups.cast(i)
    case Overview(i) => lookups.overview(i)
  }

  private def lines(tape: Seq[String], asked: collection.Set[Question], receipt: collection.Map[Dependency, Long],
                    indexReads: Map[Dependency, Long], detailed: Map[models.ListingKey, Boolean], indexAnswers: Map[Dependency, Answer[?]]): Seq[String] =
    tape.map("tape " + _) ++ asked.toSeq.map(q => "asked " + q).sorted ++ receipt.toSeq.map { case (d, h) => s"receipt ${Dependency.encode(d)} $h" }.sorted ++
      indexReads.toSeq.map { case (d, h) => s"index ${Dependency.encode(d)} $h" }.sorted ++ detailed.toSeq.map { case (k, h) => s"detailed $k $h" }.sorted ++
      indexAnswers.toSeq.map { case (d, a) => s"answer ${Dependency.encode(d)} $a" }.sorted

  private def frozenReading(script: Seq[Op]): Seq[String] = {
    val (tape, asked, receipt) = (mutable.ArrayBuffer.empty[String], mutable.Set.empty[Question], mutable.Map.empty[Dependency, Long])
    val answers = new FrozenRecording(new FrozenTaping(new Answers, tape), asked, receipt)
    val venues  = new FrozenTapingVenues(new Lookups, tape)
    val index   = new FrozenRecordingLookups(new Lookups)
    play(script, answers, venues); play(script.filter(isLookup), answers, index)
    lines(tape.toSeq, asked, receipt, index.reads, index.detailed.toMap, index.answers.toMap)
  }

  private def currentReading(script: Seq[Op]): Seq[String] = {
    val (tape, asked, receipt) = (mutable.ArrayBuffer.empty[String], mutable.Set.empty[Question], mutable.Map.empty[Dependency, Long])
    val taping: ReadEvent => Unit = {
      case ReadEvent.FamilyAnswered(q, a)    => tape += s"family|$q|${Dependency.digest(a)}|${a == Answer.Unknown}"
      case ReadEvent.DetailPresence(l, has)  => tape += s"has|${l.key}|$has"
      case ReadEvent.DetailAnswered(l, a)    => tape += s"detail|${l.key}|${Dependency.digest(a)}"
      case _: ReadEvent.LookupAnswered       => ()
    }
    val base    = new Answers
    val answers = AgreementEvidence.recording(base, asked, receipt, taping)
    val venues  = new RecordingLookups(new Lookups, taping)
    val ledger  = new LookupLedger
    val index   = new RecordingLookups(new Lookups, ledger.record)
    play(script, answers, venues); play(script.filter(isLookup), answers, index)
    lines(tape.toSeq, asked, receipt, ledger.reads, ledger.detailed.toMap, ledger.answers.toMap)
  }

  private def isLookup(op: Op): Boolean = !op.isInstanceOf[Title] && !op.isInstanceOf[Director] && !op.isInstanceOf[Rec] && !op.isInstanceOf[Showing]

  private val scripts: Seq[Seq[Op]] = Seq(
    Seq(Title("ab"), Title("abc"), Director("dir"), Rec("tt1"), Rec("tt22"), Showing("muza"), Showing("stale")),
    Seq(Title("stale title"), Rec("stale"), Title("stale title")),
    Seq(Detail(1), Detail(3), Detail(1), Has(1), Has(2), Has(3)),
    Seq(Cand(1), Cand(2), FilmOf(1), FilmOf(3), Release(2), Release(3), Cast(1), Cast(3), Overview(1)),
    Seq(Title("xy"), Detail(2), Has(2), Cand(4), FilmOf(4), Cast(4), Rec("r"), Overview(6)),
    Seq(Showing("ab"), Director("n"), Detail(6), Has(5), Release(9), Release(9)),
    Seq(Title("t"), Title("t"), Rec("tt1"), Rec("tt1"), Detail(1), Detail(1)),
    Seq(Cast(2), Cast(6), Overview(2), Overview(3), FilmOf(5), Title("four"), Director("four"))
  )

  "The shared recording wrappers" should "record, over every kind of read, exactly what the four they replaced did" in {
    val verdict = Equivalence.assertEquivalent(scripts, frozenReading, currentReading, minimum = scripts.size, show = _.toString)
    verdict.compared shouldBe scripts.size
    verdict.mismatched shouldBe 0
  }
}
