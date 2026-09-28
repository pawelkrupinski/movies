package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures.{Film, Houses, Listing, ListingFilm, Qualifiers}

/** The acceptance rules read one listing's RANKED candidates ([[Scored]]) — no resolve needed. */
class AcceptanceSpec extends AnyFlatSpec with Matchers {
  private val calibration = IdentityCalibration.resolver
  private val acceptance  = new Acceptance(calibration)
  private val houses      = Houses(Map("ntlive" -> "nationaltheatrelive"))

  /** `films` scored for `listing` as `FamilyScope.score` scores them: each at its search rank, against
   *  the rivals its title names alike, best first. */
  private def ranked(listing: Listing, films: (Int, Film, Option[Int])*): Seq[Scored] = {
    def rivalling(film: Film) = IdentityMeasures.Rivalling(IdentityMeasures.titleRelation(listing, film, houses, Qualifiers.Unknown).value)
    val close = films.count { case (_, film, _) => rivalling(film) }
    films.map { case (tmdbId, film, rank) =>
      val measures = IdentityMeasures.listingFilm(listing, film, rank, close - (if (rivalling(film)) 1 else 0), 0, houses)
      Scored(Candidate(tmdbId, film), calibration.probability(ListingFilm, measures), measures, denied = false, listing, rank,
        houseProduction = IdentityMeasures.billsUnderItsHouse(listing, film, houses))
    }.sortBy(scored => (-scored.probability, scored.candidate.tmdbId))
  }
  private def taken(candidates: Seq[Scored]): Option[Int] = acceptance.alone(candidates).map(_._1.candidate.tmdbId)

  // Flicks' UK and US "NT Live: The Misanthrope" (155 listings): TMDB's search for the play ranks
  // three bare records of Molière's work above the National Theatre's 2026 broadcast.
  private val misanthrope = Listing("NT Live: The Misanthrope", runtime = Some(180))
  private val bare = Seq(
    (511684, Film("The Misanthrope", year = Some(1966), runtime = Some(171)), Some(1)),
    (700001, Film("The Misanthrope", year = Some(1994), runtime = Some(97)), Some(2)),
    (700002, Film("The Misanthrope", year = Some(2011), runtime = Some(125)), Some(4)))
  private val national = (1693710, Film("National Theatre Live: The Misanthrope", year = Some(2026), runtime = Some(181)), Some(3))

  "a listing crediting a director" should "take the one record its title names exactly by that director, however the database ranks it" in {
    // US "Man of Iron" {Andrzej Wajda}: TMDB's search ranks Wajda's 1981 film seventh, behind "Iron Man".
    val listing = Listing("Man of Iron", directors = Seq("Andrzej Wajda"), runtime = Some(153))
    val wajda   = (225, Film("Man of Iron", year = Some(1981), runtime = Some(144), directors = Some(Seq("Andrzej Wajda"))), Some(7))
    val favreau = (1726, Film("Iron Man", year = Some(2008), runtime = Some(126), directors = Some(Seq("Jon Favreau")), popularity = Some(40.0)), Some(1))
    taken(ranked(listing, favreau, wajda)) shouldBe Some(225)
  }

  it should "take nothing when its runtime contradicts that record, or two such records are candidates" in {
    val listing = Listing("Man of Iron", directors = Seq("Andrzej Wajda"), runtime = Some(200))
    val wajda   = (225, Film("Man of Iron", year = Some(1981), runtime = Some(144), directors = Some(Seq("Andrzej Wajda"))), Some(7))
    taken(ranked(listing, wajda)) shouldBe None
    val cut     = (226, Film("Man of Iron", year = Some(1982), runtime = Some(195), directors = Some(Seq("Andrzej Wajda"))), Some(8))
    taken(ranked(listing.copy(runtime = Some(170)), wajda, cut)) shouldBe None
  }

  "a listing dating its title" should "take the one record its title names exactly from that year, however the database ranks it" in {
    // US "Troll (1986)": TMDB ranks "Troll 2" and the 2022 "Troll" above the 1986 film.
    val troll = Listing("Troll (1986)")
    val films = Seq((1180831, Film("Troll 2", year = Some(2025), popularity = Some(20.0)), Some(1)),
      (736526, Film("Troll", year = Some(2022), popularity = Some(10.0)), Some(2)), (33061, Film("Troll", year = Some(1986)), Some(3)))
    taken(ranked(troll, films *)) shouldBe Some(33061)
    // Two records of that title from that year are no answer.
    taken(ranked(troll, films :+ (33062, Film("Troll", year = Some(1986)), Some(4)) *)) shouldBe None
  }

  "a listing billing its work under a house" should "take the one record billing the work under that house over bare records of the work" in {
    taken(ranked(misanthrope, bare :+ national *)) shouldBe Some(1693710)
  }

  it should "take nothing when two records bill the work under its house" in {
    val encore = (1693711, Film("National Theatre Live: The Misanthrope", year = Some(2027), runtime = Some(181)), Some(5))
    taken(ranked(misanthrope, bare :+ national :+ encore *)) shouldBe None
  }

  it should "take nothing when another record carries the house record's very title, whatever its flags" in {
    // US "NT Live: Hamlet" (204 minutes): TMDB holds the National Theatre's 2010 and 2015 Hamlets
    // under one title; only the 2010 record read as billed under the house.
    val listing = Listing("NT Live: Hamlet", runtime = Some(204))
    val older   = (436484, Film("National Theatre Live: Hamlet", year = Some(2010)), Some(1))
    val later   = (396227, Film("National Theatre Live: Hamlet", year = Some(2015), runtime = Some(217)), Some(2))
    val candidates = ranked(listing, older, later).map(scored =>
      if (scored.candidate.tmdbId == 396227) scored.copy(houseProduction = false) else scored)
    acceptance.houseProduction(candidates) shouldBe None
  }

  it should "take nothing when its runtime contradicts the house's record" in {
    val cut = national.copy(_2 = national._2.copy(runtime = Some(120)))
    taken(ranked(misanthrope, bare :+ cut *)) shouldBe None
  }

  it should "not take its house's record over a candidate the title names that the listing's facts fit at least as well" in {
    // Everyman's "Cellar Door x ThoughtBubble presents: Terminator 2: Judgment Day" publishes nothing but
    // its title; its banner, learned as TMDB's "The Making of" house, took the making-of documentary
    // at 3.4% over Cameron's film at 54% and split it from the film's other listings.
    val listing = Listing("Cellar Door x ThoughtBubble presents: Terminator 2: Judgment Day")
    val makingOf = Film("The Making of 'Terminator 2: Judgment Day'", year = Some(1991))
    val learned = IdentityMeasures.billing(listing, makingOf).map(billed => Houses(Map(billed.listingHouse -> billed.filmHouse)))
    learned should not be empty
    val film = Film("Terminator 2: Judgment Day", year = Some(1991), runtime = Some(137))
    def scored(tmdbId: Int, candidate: Film, rank: Int) = {
      val measures = IdentityMeasures.listingFilm(listing, candidate, Some(rank), 0, 0, learned.get)
      Scored(Candidate(tmdbId, candidate), calibration.probability(ListingFilm, measures), measures, denied = false, listing, Some(rank),
        houseProduction = IdentityMeasures.billsUnderItsHouse(listing, candidate, learned.get))
    }
    val candidates = Seq(scored(280, film, 1), scored(473793, makingOf, 2)).sortBy(candidate => -candidate.probability)
    candidates.find(_.candidate.tmdbId == 473793).map(_.houseProduction) shouldBe Some(true)
    acceptance.houseProduction(candidates) shouldBe None
  }

  it should "not take another house's record of the work" in {
    val rsc = (1693712, Film("RSC Live: The Misanthrope", year = Some(2026), runtime = Some(181)), Some(3))
    taken(ranked(misanthrope, bare :+ rsc *)) shouldBe None
  }
}
