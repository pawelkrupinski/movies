package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

/** A decision still waiting on answers moves no listing off the take it holds; an answered one, or one with nothing
 *  agreed before it, decides as it decides. */
class GapHoldSpec extends AnyFlatSpec with Matchers {
  private def key(venue: String): ListingKey = ListingKey.Published(venue, "Rose", None, Nil)
  private val (metropol, kikubi, fresh) = (key("Metropol"), key("KiKuBi"), key("Atlantis"))
  private def decided(members: Seq[ListingKey], film: Option[Int], basis: ResolverDecision.Basis, unanswered: Int) =
    ResolverDecision(members, film, 0.6, basis, Seq(s"$basis why"), unanswered = unanswered)()
  private def resolution(ds: ResolverDecision*) =
    Resolution(ds, ds.map(_.members.size).sum, ds.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
  // DE "Rose": Schleinzer's film, held at both venues
  private val prior = Map(metropol -> GapHold.Prior(Some(1176962), None, ResolverDecision.Basis.OwnMatch, 0.98),
    kikubi -> GapHold.Prior(Some(1176962), None, ResolverDecision.Basis.OwnMatch, 0.98)).get

  private val clock = new tools.MutableClock(java.time.Instant.parse("2026-10-22T11:09:00Z"))
  private def hold = new GapHold(clock)

  "A decision waiting on answers" should "keep the take its listings hold, film and basis, and say why" in {
    val babayan = hold(resolution(decided(Seq(metropol, kikubi), Some(1712584), ResolverDecision.Basis.OwnMatch, unanswered = 3)), prior).decisions.head
    (babayan.film, babayan.basis, babayan.members) shouldBe ((Some(1176962), ResolverDecision.Basis.OwnMatch, Seq(metropol, kikubi)))
    babayan.explanation.last shouldBe "held: tmdb 1176962 kept while 3 question(s) it reads are unanswered"
    // a take lost to a gap is held too
    hold(resolution(decided(Seq(metropol), None, ResolverDecision.Basis.BelowThreshold, unanswered = 1)), prior).decisions.head.film shouldBe Some(1176962)
    // a new listing beside held ones holds the cluster's take with them
    hold(resolution(decided(Seq(metropol, fresh), Some(1712584), ResolverDecision.Basis.OwnMatch, unanswered = 1)), prior).decisions.head.film shouldBe
      Some(1176962)
  }

  it should "decide as it decides once answered, for listings with no take before, or where the takes it merged disagree" in {
    val answered = decided(Seq(metropol), Some(1712584), ResolverDecision.Basis.OwnMatch, unanswered = 0)
    hold(resolution(answered), prior).decisions.head should be theSameInstanceAs answered
    val alone = decided(Seq(fresh), Some(1712584), ResolverDecision.Basis.OwnMatch, unanswered = 2)
    hold(resolution(alone), prior).decisions.head should be theSameInstanceAs alone
  }

  it should "split a cluster the gaps merged, each listing kept on the take it held, a new one decided as the cluster is" in {
    val disagreeing = Map(metropol -> GapHold.Prior(Some(1), None, ResolverDecision.Basis.OwnMatch, 1.0),
      kikubi -> GapHold.Prior(Some(2), None, ResolverDecision.Basis.OwnMatch, 1.0)).get
    val merged = decided(Seq(metropol, kikubi, fresh), Some(3), ResolverDecision.Basis.PooledMatch, unanswered = 1)
    hold(resolution(merged), disagreeing).decisions.map(d => d.members -> d.film) should contain theSameElementsAs Seq(
      Seq(metropol) -> Some(1), Seq(kikubi) -> Some(2), Seq(fresh) -> Some(3))
  }

  it should "hold a cluster its family decided on a film record or venue page not known yet, though its searches answered" in {
    val blindly = decided(Seq(metropol), Some(1712584), ResolverDecision.Basis.OwnMatch, unanswered = 0)
    hold(resolution(blindly), prior, Set(metropol)).decisions.head.film shouldBe Some(1176962)
    hold(resolution(blindly), prior, Set.empty).decisions.head should be theSameInstanceAs blindly
  }

  it should "hold no take longer than the limit: a gap that never closes decides as a whole resolve would" in {
    val holds = new GapHold(clock)
    val gapped = resolution(decided(Seq(metropol), Some(1712584), ResolverDecision.Basis.OwnMatch, unanswered = 1))
    holds(gapped, prior).decisions.head.film shouldBe Some(1176962)
    clock.advance(GapHold.Limit)
    holds(gapped, prior).decisions.head.film shouldBe Some(1176962)
    clock.advance(java.time.Duration.ofMinutes(1))
    holds(gapped, prior).decisions.head.film shouldBe Some(1712584)
  }

  "A worker just up" should "hold the take its stored film shows: a TMDB film, a fallback film its record names, or none" in {
    def row(record: models.MovieRecord) = services.movies.StoredMovieRecord("Dora", Some(2026), record, services.movies.FilmId("f-1"))
    val filmweb = GapHold.Prior.stored(None, Some(row(models.MovieRecord(filmwebUrl = Some("https://www.filmweb.pl/film/Dora-2026-10114519")))))
    filmweb.fallback.map(f => (f.source, f.id)) shouldBe Some(("filmweb", "10114519"))
    // DE "19. FGA: Dora" at Janosik: taken by its families as Filmweb's film; a deploy's gaps leave the model nothing
    val lost = decided(Seq(metropol), None, ResolverDecision.Basis.BelowThreshold, unanswered = 2)
    val kept = hold(resolution(lost), Map(metropol -> filmweb).get).decisions.head
    (kept.film, kept.fallback.map(_.id), kept.basis) shouldBe ((None, Some("10114519"), ResolverDecision.Basis.Agreed))
    GapHold.Prior.stored(Some(42), None).film shouldBe Some(42)
    GapHold.Prior.stored(None, Some(row(models.MovieRecord()))).same(lost) shouldBe true
  }
}
