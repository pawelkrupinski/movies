package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

/** A decision still waiting on answers moves no listing off an ESTABLISHED take it holds; an answered one, one over
 *  listings with no take before or a take itself made on gaps, decides as it decides — and a hold ends once its answers
 *  are in, or the fill has asked all it can. */
class GapHoldSpec extends AnyFlatSpec with Matchers with org.scalatest.LoneElement {
  private def key(venue: String): ListingKey = ListingKey.Published(venue, "Rose", None, Nil)
  private val (metropol, kikubi, fresh) = (key("Metropol"), key("KiKuBi"), key("Atlantis"))
  private def decided(members: Seq[ListingKey], film: Option[Int], basis: ResolverDecision.Basis, unanswered: Int) =
    ResolverDecision(members, film, 0.6, basis, Seq(s"$basis why"), unanswered = unanswered)()
  private def resolution(ds: ResolverDecision*) =
    Resolution(ds, ds.map(_.members.size).sum, ds.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
  // DE "Rose": Schleinzer's film, taken on answered evidence at both venues
  private val schleinzer = decided(Seq(metropol, kikubi), Some(1176962), ResolverDecision.Basis.OwnMatch, unanswered = 0)
  private val babayan    = Some(1712584)

  private val clock = new tools.MutableClock(java.time.Instant.parse("2026-10-22T11:09:00Z"))
  private val none: ListingKey => Boolean = _ => false

  /** A projection's GapHold: each call holds against the takes the last one made, as `IdentityProjection` does. */
  private final class Projection(var taken: Resolution = resolution(schleinzer), var rounds: Long = 0L) {
    val hold = new GapHold(clock)
    def apply(model: Resolution, blind: ListingKey => Boolean = none): Resolution = {
      val of = taken.decisionOf
      taken = hold(model, (k, d) => hold.priorOf(k, of.getOrElse(k, null), d), (k, d) => hold.confirms(k, of.getOrElse(k, null), d), blind, rounds)
      taken
    }
  }

  "A decision waiting on answers" should "keep the established take its listings hold, film and basis, and say why" in {
    val p    = new Projection
    val kept = p(resolution(decided(Seq(metropol, kikubi), babayan, ResolverDecision.Basis.OwnMatch, unanswered = 3))).decisions.loneElement
    (kept.film, kept.basis, kept.members) shouldBe ((Some(1176962), ResolverDecision.Basis.OwnMatch, Seq(metropol, kikubi)))
    kept.explanation.last shouldBe "held: tmdb 1176962 kept while 3 question(s) it reads are unanswered"
    // a take lost to a gap is held too
    new Projection().apply(resolution(decided(Seq(metropol), None, ResolverDecision.Basis.BelowThreshold, unanswered = 1)))
      .decisions.head.film shouldBe Some(1176962)
    // a new listing beside held ones takes the cluster's decision, the held ones keep theirs
    new Projection().apply(resolution(decided(Seq(metropol, fresh), babayan, ResolverDecision.Basis.OwnMatch, unanswered = 1)))
      .decisions.map(d => d.members -> d.film) should contain theSameElementsAs Seq(Seq(metropol) -> Some(1176962), Seq(fresh) -> babayan)
  }

  it should "pass the model on as it is where nothing is held: answered, the same take, a pin" in {
    val p        = new Projection
    val answered = resolution(decided(Seq(metropol), babayan, ResolverDecision.Basis.OwnMatch, unanswered = 0))
    p(answered) should be theSameInstanceAs answered
    val same = resolution(decided(Seq(metropol, kikubi), Some(1176962), ResolverDecision.Basis.PooledMatch, unanswered = 4))
    new Projection()(same) should be theSameInstanceAs same
    val pinned = resolution(decided(Seq(metropol), babayan, ResolverDecision.Basis.Pinned, unanswered = 2))
    new Projection()(pinned) should be theSameInstanceAs pinned
  }

  it should "hold no initial take: a listing with no take before, or a boot's first projection" in {
    val alone = resolution(decided(Seq(fresh), babayan, ResolverDecision.Basis.OwnMatch, unanswered = 2))
    new Projection()(alone) should be theSameInstanceAs alone
    val boot  = new Projection(taken = resolution())
    val first = resolution(decided(Seq(metropol, kikubi), babayan, ResolverDecision.Basis.OwnMatch, unanswered = 5))
    boot(first) should be theSameInstanceAs first
  }

  it should "hold no take itself made on gaps: what an outage took while answers were missing moves as they arrive" in {
    // TMDB down: the cluster lost its take, decided on gaps — then TMDB answers half, and the take it moves to is not held
    val p = new Projection(taken = resolution(decided(Seq(metropol, kikubi), None, ResolverDecision.Basis.BelowThreshold, unanswered = 40)))
    val recovering = resolution(decided(Seq(metropol, kikubi), Some(1176962), ResolverDecision.Basis.OwnMatch, unanswered = 2))
    p(recovering) should be theSameInstanceAs recovering
    // and one decided blind (a record or page not known yet) neither
    // a boot's first take, decided blind at Metropol (a venue page not read yet), answered at KiKuBi
    val q = new Projection(taken = resolution())
    q(resolution(decided(Seq(metropol, kikubi), Some(1176962), ResolverDecision.Basis.OwnMatch, unanswered = 0)), blind = Set(metropol))
    val moved = resolution(decided(Seq(metropol, kikubi), babayan, ResolverDecision.Basis.OwnMatch, unanswered = 1))
    q(moved).decisions.map(d => d.members -> d.film) should contain theSameElementsAs Seq(Seq(kikubi) -> Some(1176962), Seq(metropol) -> babayan)
  }

  it should "split a cluster the gaps merged, each listing kept on the take it held, a new one decided as the cluster is" in {
    val p = new Projection(taken = resolution(decided(Seq(metropol), Some(1), ResolverDecision.Basis.OwnMatch, 0),
      decided(Seq(kikubi), Some(2), ResolverDecision.Basis.OwnMatch, 0)))
    p(resolution(decided(Seq(metropol, kikubi, fresh), Some(3), ResolverDecision.Basis.PooledMatch, unanswered = 1)))
      .decisions.map(d => d.members -> d.film) should contain theSameElementsAs Seq(Seq(metropol) -> Some(1), Seq(kikubi) -> Some(2), Seq(fresh) -> Some(3))
  }

  it should "hold a cluster its family decided on a film record or venue page not known yet, though its searches answered" in {
    val blindly = resolution(decided(Seq(metropol, kikubi), babayan, ResolverDecision.Basis.OwnMatch, unanswered = 0))
    new Projection()(blindly, blind = Set(metropol)).decisions.head.film shouldBe Some(1176962)
    new Projection()(blindly, blind = Set.empty) should be theSameInstanceAs blindly
  }

  "A held take" should "move once its answers are in, and stay held across ticks until then" in {
    val p      = new Projection
    val gapped = resolution(decided(Seq(metropol, kikubi), babayan, ResolverDecision.Basis.OwnMatch, unanswered = 3))
    p(gapped).decisions.head.film shouldBe Some(1176962)
    p(gapped).decisions.head.film shouldBe Some(1176962)   // a held take is established: held again
    val answered = resolution(decided(Seq(metropol, kikubi), babayan, ResolverDecision.Basis.OwnMatch, unanswered = 0))
    p(answered) should be theSameInstanceAs answered
  }

  it should "stay established through a gap state that made it again, and hold it when the next one moves it" in {
    // PL "Cicha powódź": no film, on answered evidence; a deploy's gaps make it again, then half filled take a film
    val p = new Projection(taken = resolution(decided(Seq(metropol), None, ResolverDecision.Basis.BelowThreshold, unanswered = 0)))
    val again = resolution(decided(Seq(metropol), None, ResolverDecision.Basis.BelowThreshold, unanswered = 6))
    p(again) should be theSameInstanceAs again
    p(resolution(decided(Seq(metropol), babayan, ResolverDecision.Basis.OwnMatch, unanswered = 3))).decisions.head.film shouldBe None
  }

  it should "end once the fill has ended two complete rounds since it began: a gap left then is one no fill closes" in {
    val p      = new Projection(rounds = 7)
    val gapped = resolution(decided(Seq(metropol, kikubi), babayan, ResolverDecision.Basis.OwnMatch, unanswered = 1))
    p(gapped).decisions.head.film shouldBe Some(1176962)
    p.rounds = 8
    p(gapped).decisions.head.film shouldBe Some(1176962)
    p.rounds = 9
    p(gapped).decisions.head.film shouldBe babayan
    // and the take it moved to, made on gaps, is not held in its turn
    p(resolution(decided(Seq(metropol, kikubi), Some(1176962), ResolverDecision.Basis.OwnMatch, unanswered = 1))).decisions.head.film shouldBe
      Some(1176962)
  }

  it should "hold no longer than the limit, the fill or not" in {
    val p      = new Projection
    val gapped = resolution(decided(Seq(metropol, kikubi), babayan, ResolverDecision.Basis.OwnMatch, unanswered = 1))
    p(gapped).decisions.head.film shouldBe Some(1176962)
    clock.advance(GapHold.Limit)
    p(gapped).decisions.head.film shouldBe Some(1176962)
    clock.advance(java.time.Duration.ofMinutes(1))
    p(gapped).decisions.head.film shouldBe babayan
  }

  "A worker just up" should "hold the take its stored film shows: a TMDB film, a fallback film its record names, or none" in {
    def row(record: models.MovieRecord) = services.movies.StoredMovieRecord("Dora", Some(2026), record, services.movies.FilmId("f-1"))
    val filmweb = GapHold.Prior.stored(None, Some(row(models.MovieRecord(filmwebUrl = Some("https://www.filmweb.pl/film/Dora-2026-10114519")))))
    filmweb.fallback.map(f => (f.source, f.id)) shouldBe Some(("filmweb", "10114519"))
    // DE "19. FGA: Dora" at Janosik: taken by its families as Filmweb's film; a deploy's gaps leave the model nothing
    val lost = decided(Seq(metropol), None, ResolverDecision.Basis.BelowThreshold, unanswered = 2)
    val hold = new GapHold(clock)
    val kept = hold(resolution(lost), (k, d) => Option.when(k == metropol)(filmweb).filterNot(_.same(d)).orNull, (_, _) => false, none, 0L)
      .decisions.head
    (kept.film, kept.fallback.map(_.id), kept.basis) shouldBe ((None, Some("10114519"), ResolverDecision.Basis.Agreed))
    GapHold.Prior.stored(Some(42), None).film shouldBe Some(42)
    GapHold.Prior.stored(None, Some(row(models.MovieRecord()))).same(lost) shouldBe true
  }
}
