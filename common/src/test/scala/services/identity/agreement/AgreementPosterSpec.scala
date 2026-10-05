package services.identity.agreement

import models.{KinoMuza, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, FilmTable, IdentityCalibration, IdentityMeasures, Listing, PosterAnswers, PosterHash, Resolution, ResolverDecision}
import services.movies.SingleCountryNormalizer

/** The venue posters on the agreement's way to the projection: a cluster nothing took takes the one candidate its poster
 *  matches, a film the families agree on is not taken when the poster matches another candidate, and a poster not
 *  hashed yet holds the cluster as the model left it and is asked for. */
class AgreementPosterSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer

  private val venuePoster = "https://kino.example/lalka.jpg"
  private val shown       = PosterHash(0x5a5a5a5a5a5aL)
  private def near(bits: Int) = PosterHash(shown.bits ^ ((1L << bits) - 1))

  /** TMDB's two "Lalka"s, and the venue's poster of the 2026 one. */
  private val table = new FilmTable(Seq(FilmTable.F(1001, "Lalka", 2026, "Maciej Kawalski", 120), FilmTable.F(1002, "Lalka", 1968, "Wojciech Has", 159)), normalizer)
  private def lalka(title: String = "Lalka", poster: Option[String] = Some(venuePoster)): Listing =
    FilmTable.listing(Multikino, title).copy(poster = poster)
  private def silentFamilies = VoterFamily.values.map(family => family -> new HeldFamilyAnswers(family, Map.empty)).toMap[VoterFamily, FamilyAnswers]

  private def resolutionOf(listing: Listing) = Resolution(Seq(ResolverDecision(Seq(listing.key), None, 0.4, ResolverDecision.Basis.BelowThreshold, Nil)()),
    1, Map(listing.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)

  private final class HeldPosters(venues: Map[String, Option[PosterHash]], films: Map[Int, Seq[PosterHash]]) extends PosterAnswers {
    def venue(url: String): Answer[Option[PosterHash]] = venues.get(url).fold[Answer[Option[PosterHash]]](Answer.Unknown)(Answer.Known(_))
    def film(tmdbId: Int): Answer[Seq[PosterHash]]     = films.get(tmdbId).fold[Answer[Seq[PosterHash]]](Answer.Unknown)(Answer.Known(_))
  }

  private def stage(posters: PosterAnswers, families: Map[VoterFamily, FamilyAnswers] = silentFamilies, ask: AgreementStage.Open => Unit = _ => ()) =
    new AgreementStage(families, table, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts,
      ask = ask, clock = _root_.tools.SpecClock.Pinned, posters = posters, tmdb = Some(table))

  "an unmatched cluster's venue poster" should "take the one candidate it matches, no other as near" in {
    val listing = lalka()
    val decided = stage(new HeldPosters(Map(venuePoster -> Some(shown)), Map(1001 -> Seq(PosterHash(0L), near(3)), 1002 -> Seq(PosterHash(-1L)))))
      .apply(resolutionOf(listing), Map(listing.key -> listing).get, version = 1).decisions.head
    (decided.film, decided.basis) shouldBe ((Some(1001), ResolverDecision.Basis.Poster))
    decided.explanation.last should include ("matches 'Lalka' (2026) (3 bits)")
  }

  it should "take none when two candidates' posters are as near, or none is near enough" in {
    val listing = lalka()
    def decidedWith(films: Map[Int, Seq[PosterHash]]) =
      stage(new HeldPosters(Map(venuePoster -> Some(shown)), films)).apply(resolutionOf(listing), Map(listing.key -> listing).get, version = 1).decisions.head.film
    decidedWith(Map(1001 -> Seq(near(2)), 1002 -> Seq(near(4)))) shouldBe None
    decidedWith(Map(1001 -> Seq(near(6)), 1002 -> Seq(PosterHash(-1L)))) shouldBe None
  }

  it should "speak for no film of a stage relay's listing, nor where the venue shows none" in {
    val relay = FilmTable.listing(KinoMuza, "The Royal Ballet: The Nutcracker").copy(poster = Some(venuePoster))
    val nutcracker = new FilmTable(Seq(FilmTable.F(2001, "The Royal Ballet: The Nutcracker", 2024, "", 0)), normalizer)
    val relayStage = new AgreementStage(silentFamilies, nutcracker, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None),
      new InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned, posters = new HeldPosters(Map(venuePoster -> Some(shown)), Map(2001 -> Seq(shown))),
      tmdb = Some(nutcracker))
    relayStage.apply(resolutionOf(relay), Map(relay.key -> relay).get, version = 1).decisions.head.film shouldBe None
    val bare = lalka(poster = None)
    stage(new HeldPosters(Map.empty, Map(1001 -> Seq(shown)))).apply(resolutionOf(bare), Map(bare.key -> bare).get, version = 1).decisions.head.film shouldBe None
  }

  it should "hold the cluster while a poster is not hashed yet, and hand the poster to the queue" in {
    val listing = lalka()
    val handed  = scala.collection.mutable.ArrayBuffer.empty[AgreementStage.Open]
    val waiting = stage(new HeldPosters(Map.empty, Map.empty), ask = handed += _)
    waiting.apply(resolutionOf(listing), Map(listing.key -> listing).get, version = 1).decisions.head.film shouldBe None
    waiting.wantedPosters shouldBe Set(AgreementStage.PosterQuestion.Venue(venuePoster))
    val films = stage(new HeldPosters(Map(venuePoster -> Some(shown)), Map(1001 -> Seq(shown))), ask = handed += _)
    films.apply(resolutionOf(listing), Map(listing.key -> listing).get, version = 1).decisions.head.film shouldBe None
    films.wantedPosters shouldBe Set(AgreementStage.PosterQuestion.Film(1002))
    handed.flatMap(_.posters).toSet shouldBe Set(AgreementStage.PosterQuestion.Venue(venuePoster), AgreementStage.PosterQuestion.Film(1002))
  }

  "a film the families agree on" should "not be taken when the venue's poster matches another candidate and not it" in {
    val listing = lalka()
    // the families take Has's 1968 film; the venue's poster is the 2026 film's
    val has = SourceRecord(IdentityMeasures.Film("Lalka", None, Nil, Some(1968), Some(159), Some(Seq("Wojciech Has")), None, None), Map("tmdb" -> "1002"))
    val agreeing = Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb).map(family => family -> new HeldFamilyAnswers(family, Map("x" -> has))).toMap
    def decidedWith(films: Map[Int, Seq[PosterHash]]) =
      stage(new HeldPosters(Map(venuePoster -> Some(shown)), films), silentFamilies ++ agreeing)
        .apply(resolutionOf(listing), Map(listing.key -> listing).get, version = 1).decisions.head
    val vetoed = decidedWith(Map(1001 -> Seq(near(2)), 1002 -> Seq(PosterHash(-1L))))
    (vetoed.film, vetoed.basis) shouldBe ((Some(1001), ResolverDecision.Basis.Poster))   // vetoed, and the poster's own vote takes the 2026 film
    val agreed = decidedWith(Map(1001 -> Seq(PosterHash(0L)), 1002 -> Seq(PosterHash(-1L))))
    (agreed.film, agreed.basis) shouldBe ((Some(1002), ResolverDecision.Basis.Agreed))   // a poster matching no candidate vetoes nothing
  }
}
