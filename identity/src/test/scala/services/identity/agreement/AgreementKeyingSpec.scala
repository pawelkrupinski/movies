package services.identity.agreement

import models.{KinoMuza, KinoPalacowe, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, FilmTable, IdentityCalibration, IdentityMeasures, Listing, Question, Resolution, ResolverDecision, StoredFamily}
import services.identity.staging.StagingEvent
import services.movies.{ListingKey, SingleCountryNormalizer}

/** What the stage keeps per cluster is kept under the cluster's key (docs/design/identity-cluster-identity.md): a listing
 *  sorting first, a merge or a split moves no memory a cluster keeps, a restart re-attaches each key by its first listing,
 *  and the verdicts stored under first listings before keys are taken over under the keys, none decided again for it. */
class AgreementKeyingSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  import FilmTable.listing

  private val klondike = IdentityMeasures.Film("Klondike", None, Nil, Some(2022), Some(100), Some(Seq("Maryna Er Gorbach")), None, None)
  // RT never answers: once its question is attempted, a cluster settles on the other three, which agree — their reads
  // `counted` (RT's is read on every pass, to tell whether it was attempted)
  private def families(counted: () => Unit): Map[VoterFamily, FamilyAnswers] = Map(
    VoterFamily.Imdb           -> new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt16315948" -> SourceRecord(klondike, Map("imdb" -> "tt16315948")))),
    VoterFamily.Wiki           -> new HeldFamilyAnswers(VoterFamily.Wiki, Map("Q1" -> SourceRecord(klondike, Map("imdb" -> "tt16315948")))),
    VoterFamily.Filmweb        -> new HeldFamilyAnswers(VoterFamily.Filmweb, Map("880000" -> SourceRecord(klondike))),
    VoterFamily.RottenTomatoes -> new HeldFamilyAnswers(VoterFamily.RottenTomatoes, Map.empty, unanswered = true)).map { case (family, answers) =>
    val count: () => Unit = if (family == VoterFamily.RottenTomatoes) () => () else counted
    family -> new FamilyAnswers {
      val family: VoterFamily = answers.family
      def titled(text: String)     = { count(); answers.titled(text) }
      def directedBy(name: String) = { count(); answers.directedBy(name) }
      def record(id: String)       = { count(); answers.record(id) }
      override def fresh(question: String) = answers.fresh(question)
    }
  }
  // three venues' "Klondike", in the order their listings sort
  private val Seq(first, second, third) = Seq(KinoMuza, Multikino, KinoPalacowe).map(listing(_, "Klondike", year = Some(2022))).sortBy(_.key)(using ListingKey.ordering)
  private val listingOf: ListingKey => Option[Listing] = Seq(first, second, third).map(l => l.key -> l).toMap.get

  private def unmatched(listings: Listing*) = ResolverDecision(listings.map(_.key), None, 0.7, ResolverDecision.Basis.BelowThreshold, Nil)()
  private def modelTook(listings: Listing*) = ResolverDecision(listings.map(_.key), Some(438631), 0.99, ResolverDecision.Basis.OwnMatch, Nil)()
  private def model(decisions: ResolverDecision*) = {
    val keys = decisions.flatMap(_.members)
    Resolution(decisions, keys.size, keys.zipWithIndex.toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
  }

  /** A stage over [[families]], RT's question attempted while `tried` holds: once it lapses, a cluster that settled has
   *  nothing but its own settle memory ([[AgreementStage.unreached]]) to settle again by. */
  private final class Stage(verdicts: AgreementVerdicts = new InMemoryAgreementVerdicts, keys: AgreementKeys = new InMemoryAgreementKeys,
                            counted: () => Unit = () => (), askNow: AgreementStage.AskNow = AgreementStage.AskNow.Never) {
    var tried = true
    val told  = scala.collection.mutable.ArrayBuffer.empty[StagingEvent]
    val stage = new AgreementStage(families(counted), NoVenueDetails, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(Some(913760)),
      verdicts, clock = _root_.tools.SpecClock.Pinned, askNow = askNow, staging = Some(told ++= _), keyStore = keys,
      attempted = { case Question.Family(VoterFamily.RottenTomatoes, _) => tried; case _ => false })
    def apply(resolution: Resolution, version: Long): Seq[ResolverDecision] = stage.apply(resolution, listingOf, version).decisions
  }

  "a settled cluster a listing sorting first joins" should "keep its key, and settle again rather than wait anew" in {
    val s       = new Stage
    val settled = s.apply(model(unmatched(second)), version = 1).head
    settled.film shouldBe Some(913760)
    s.tried = false
    val joined = s.apply(model(unmatched(first, second)), version = 2).head
    s.stage.keyOf(joined) shouldBe s.stage.keyOf(settled)
    joined.film shouldBe Some(913760)   // keyed by its first listing, it waited anew on RT and fell back to the model's take
    s.stage.waitingOn(joined).settled shouldBe true
  }

  "a merge" should "keep the older key and take on the younger's settle memory, its wait's start told to the staging screen" in {
    val s     = new Stage
    // the older cluster the model took (nothing settled), the younger one settled on the families without RT
    val Seq(took, settled) = s.apply(model(modelTook(first), unmatched(second)), version = 1)
    settled.film shouldBe Some(913760)
    val (older, younger) = (s.stage.keyOf(took), s.stage.keyOf(settled))
    older.toLong should be < younger.toLong
    s.tried = false
    s.told.clear()
    val merged = s.apply(model(unmatched(first, second)), version = 2).head
    s.stage.keyOf(merged) shouldBe older
    merged.film shouldBe Some(913760)   // retired with its key, the younger's memory left the merged cluster waiting anew
    s.stage.waitingOn(merged).settled shouldBe true
    s.told.collect { case m: StagingEvent.Merged => (m.cluster, m.absorbed) } shouldBe Seq((older, younger))
  }

  it should "take on the younger's inline ask: the merged cluster is not asked inline again" in {
    val asked = scala.collection.mutable.ArrayBuffer.empty[Set[Question]]
    val s     = new Stage(askNow = (questions, _) => asked += questions)
    s.tried = false
    s.apply(model(modelTook(first)), version = 1)                               // the first pass: nothing asked inline
    s.apply(model(modelTook(first), unmatched(second)), version = 2)            // a new cluster: asked inline once
    asked should have size 1
    s.apply(model(unmatched(first, second)), version = 3)                       // merged into the older key, nothing held under it
    asked should have size 1
  }

  "a split" should "keep the key on the larger half, the other a new cluster asked inline" in {
    val asked = scala.collection.mutable.ArrayBuffer.empty[Set[Question]]
    val s     = new Stage(askNow = (questions, _) => asked += questions)
    s.tried = false
    val whole = s.apply(model(unmatched(first, second, third)), version = 1).head
    val Seq(small, large) = s.apply(model(unmatched(first), unmatched(second, third)), version = 2)
    s.stage.keyOf(large) shouldBe s.stage.keyOf(whole)
    s.stage.keyOf(small) should not be s.stage.keyOf(whole)
    asked shouldBe Seq(Set(Question.Family(VoterFamily.RottenTomatoes, "title|Klondike")))
  }

  "a restart" should "re-attach each cluster's key by its first listing, its verdict standing unread, and retire the keys of clusters gone" in {
    val (verdicts, keys) = (new InMemoryAgreementVerdicts, new InMemoryAgreementKeys)
    val before  = new Stage(verdicts, keys)
    val Seq(kept, gone) = before.apply(model(unmatched(second), unmatched(third)), version = 1)
    keys.all().keys.map(k => k.key.toString -> k.first).toSet shouldBe Set(before.stage.keyOf(kept) -> StoredFamily.idOf(Seq(second.key)),
      before.stage.keyOf(gone) -> StoredFamily.idOf(Seq(third.key)))
    var read = 0
    val after = new Stage(verdicts, keys, counted = () => read += 1)
    // a cluster new to it, its listing sorting first: numbered past every key the counter handed out
    val Seq(arrived, again) = after.apply(model(unmatched(first), unmatched(second)), version = 1)
    after.stage.keyOf(again) shouldBe before.stage.keyOf(kept)
    again.film shouldBe Some(913760)
    after.stage.keyOf(arrived).toLong should be > before.stage.keyOf(gone).toLong
    keys.all().keys.map(_.key.toString) should not contain before.stage.keyOf(gone)
    val restartedRead = read
    read = 0
    new Stage(new InMemoryAgreementVerdicts, new InMemoryAgreementKeys, counted = () => read += 1).apply(model(unmatched(first), unmatched(second)), version = 1)
    restartedRead should be < read   // the re-attached cluster's verdict stood unread: only the new cluster was resolved
  }

  "a verdict stored under its cluster's first listing" should "be taken over under the cluster's key, unread, and its old document deleted" in {
    val verdicts = new InMemoryAgreementVerdicts
    val decided  = new Stage(verdicts).apply(model(unmatched(second)), version = 1).head
    decided.film shouldBe Some(913760)
    // as a build before keys stored it
    val legacy = StoredFamily.idOf(Seq(second.key))
    verdicts.replace(verdicts.all().map(_.id).toSet, verdicts.all().map(_.copy(id = legacy)))
    var read = 0
    val s       = new Stage(verdicts, counted = () => read += 1)
    val adopted = s.apply(model(unmatched(second)), version = 1).head
    adopted.film shouldBe Some(913760)
    read shouldBe 0
    verdicts.all().map(_.id) shouldBe Seq(s.stage.keyOf(adopted))
  }

  it should "be decided again under the cluster's key where its listings moved, the old document deleted" in {
    val verdicts = new InMemoryAgreementVerdicts
    new Stage(verdicts).apply(model(unmatched(second)), version = 1)
    val legacy = StoredFamily.idOf(Seq(second.key))
    val stored = verdicts.all()
    verdicts.replace(stored.map(_.id).toSet, stored.map(_.copy(id = legacy, listings = 42L)))
    var read = 0
    val s       = new Stage(verdicts, counted = () => read += 1)
    val decided = s.apply(model(unmatched(second)), version = 1).head
    decided.film shouldBe Some(913760)
    read should be > 0
    verdicts.all().map(v => (v.id, v.listings)) shouldBe stored.map(v => (s.stage.keyOf(decided), v.listings))
  }
}
