package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.identity.FilmTable.F
import services.movies.SingleCountryNormalizer
import services.identity.vocabulary.{PinConstraints, TitleDecorations}
import services.identity.measures.Listing

/** The group vote ([[ClusterVoting.vote]]) over a cluster the test names, so the solver's split — which no recording
 *  reproduces — is given rather than hoped for. */
class ClusterVotingSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val uk = models.Cinema.all.filter(c => models.City.forCinema(c).exists(_.country.language.getCountry == "GB")).take(12)
  private val day = java.time.LocalDate.of(2027, 3, 14)

  private def scraped(venue: models.Cinema, raw: String, runtime: Option[Int] = None, ids: Map[String, String] = Map.empty): Listing =
    Listing.of(venue, models.CinemaMovie(models.Movie(raw, runtimeMinutes = runtime), venue, None, None, None, Nil, Nil,
      Seq(models.Showtime(day.atTime(18, 0), None)), externalIds = ids), normalizer)

  /** UK "Notre-Dame de Paris", prod mirror 2026-11-07: Magic Lantern Tywyn's billing [107′] took the 1956 Hunchback alone
   *  and Leiston's "Ballet - Notre-Dame de Paris" withdrew it (a screen namesake of the house's work). The solver put it
   *  in one cluster with Riverside's bare billing and Plough Torrington's [90′] and Quad Derby's [125′], which took
   *  nothing alone. The films the cluster votes, over a film table numbered by TMDB's ids or not (`tmdbNumbered`: the
   *  agreement's `FamilyLookups`), Magic Lantern's billing carrying `lanternIds`. */
  private def notreDameVotes(tmdbNumbered: Boolean = true, lanternIds: Map[String, String] = Map.empty): Seq[Int] = {
    val records = Seq(
      F(11594, "The Hunchback of Notre Dame", 1956, "Jean Delannoy", 115, 6.0, original = Some("Notre-Dame de Paris")),
      F(56759, "Notre-Dame de Paris", 1998, "Gilles Amado", 175, 1.2),
      F(484261, "Notre Dame de Paris", 2017, "Someone Else", 127, 1.1),
      F(18987, "The Hunchback of Notre Dame", 1923, "Wallace Worsley", 120, 1.6),
      F(1523791, "Notre-Dame de Paris", 1997, "Another Director", 160, 0.5),
      F(28892, "Etoile", 1989, "Peter Del Monte", 98, 0.8, alternatives = Seq("Notre-Dame de Paris")),
      F(790330, "La Scala Ballet: Notre-Dame de Paris", 2013, "Roland Petit", 110, 0.6))
    val ballet   = scraped(uk(0), "Ballet - Notre-Dame de Paris") +:
      uk.slice(1, 4).map(scraped(_, "Opéra De Paris Ballet - Notre-Dame de Paris"))
    val lantern   = scraped(uk(4), "Notre-Dame De Paris", runtime = Some(107), ids = lanternIds)
    val riverside = scraped(uk(5), "Notre-Dame de Paris")
    val plough    = scraped(uk(6), "Notre-Dame de Paris", runtime = Some(90))
    val quad      = scraped(uk(7), "Notre-Dame De Paris (Cert. TBC)", runtime = Some(125))
    val listings  = ballet ++ Seq(lantern, riverside, plough, quad)
    val stages = new ResolveStages(listings, new FilmTable(records, normalizer, tmdbNumbered), normalizer, IdentityCalibration.resolver,
      IdentityResolver.Mutation.None, PinConstraints(Nil), TitleDecorations.None)
    import stages.{acceptance, families, generation, scoring}
    def node(l: Listing) = generation.nodes.find(_.listings.exists(_.key == l.key)).get
    val withdrawn = node(lantern)
    val scope     = families.scopeOf(withdrawn)
    // the premise: Magic Lantern took the Hunchback alone and a sibling withdrew it; Plough and Quad took nothing alone
    withClue((scope.of(withdrawn).take(3).map(NodeCandidates.CandidateScore.of(_, IdentityCalibration.resolver).render) ++ acceptance.refusals(scope.of(withdrawn)).map(_.toString)).mkString("\n"))(scope.takenAlone(withdrawn).map(_._1._1.candidate.tmdbId) shouldBe Some(11594))
    families.bestOf.get(withdrawn.id) shouldBe empty
    Seq(plough, quad).map(node).flatMap(n => scope.takenAlone(n).map(taken => n.id -> (taken._1._1.candidate.tmdbId, taken._2))) shouldBe empty
    val cluster = Seq(lantern, riverside, plough, quad).map(node).distinct
    new ClusterVoting(scoring, families, acceptance).vote(cluster, scope).map(_._2._1).distinct
  }

  "A cluster voting a film one member was withdrawn from" should "not vote it back on that member's evidence" in {
    notreDameVotes() should not contain 11594
  }

  it should "not read a TMDB-keyed feed's link as keeping it where the films are a family's own numbers, which no TMDB id names" in {
    // the agreement's family resolves number their films themselves (FamilyLookups): a Cineamo id that happens to be
    // the Hunchback's number links nothing, and the withdrawal stands
    notreDameVotes(tmdbNumbered = false, lanternIds = Map("cineamo" -> "11594m")) should not contain 11594
  }
}
