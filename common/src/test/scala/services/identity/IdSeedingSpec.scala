package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdSeeding.Film
import services.movies.ListingKey

import scala.util.Random

/**
 * The migration's first assignment: today's films as the previous one, the resolver's clusters as
 * the next, and every film that cannot simply hand its id to one cluster on the review list.
 */
class IdSeedingSpec extends AnyFlatSpec with Matchers {

  private def l(venue: String, title: String): ListingKey = ListingKey.Native(venue, s"https://$venue/$title", title)
  private def listings(title: String, venues: String*): Set[ListingKey] = venues.map(l(_, title)).toSet

  private val belle  = listings("belle", "a", "b", "c")
  private val it1990 = listings("it", "a")
  private val it2017 = listings("it", "b", "c", "d")
  private val big    = listings("dune", "a", "b", "c", "d")
  private val small  = listings("dune (imax)", "a", "b")
  private val gone   = listings("closed", "z")
  private val unseen = listings("new", "n")

  private val films = Seq(
    Film("belle|2013", belle),                 // one film, one cluster: keeps its id
    Film("it|", it1990 ++ it2017),              // the resolver splits it: 1990 apart from 2017
    Film("dune|2021", big),                     // the resolver merges these two …
    Film("f00000000000abc", small),             // … and the larger film keeps its id
    Film("closed|2020", gone))                  // its listings are not in the corpus
  private val clusters = Seq(belle, it1990, it2017, big ++ small, unseen)

  "review" should "hand each film's id to the cluster holding most of its listings" in {
    val r = IdSeeding.review(films, clusters)
    r.keeps shouldBe Map("belle|2013" -> belle, "it|" -> it2017, "dune|2021" -> (big ++ small))
  }

  it should "list what a person has to look at: no cluster, merged away, split, and the fresh clusters" in {
    val r = IdSeeding.review(films, clusters)
    r.unmatched.map(_.id) shouldBe Seq("closed|2020")
    r.mergedAway.map { case (f, winner) => f.id -> winner } shouldBe Seq("f00000000000abc" -> "dune|2021")
    r.split.map { case (f, cs) => f.id -> cs } shouldBe Seq("it|" -> Seq(it2017, it1990))
    r.fresh shouldBe Seq(it1990 -> Seq("it|"), unseen -> Nil)
  }

  it should "not depend on the order either side is given in" in {
    val expected = IdSeeding.review(films, clusters)
    (1 to 20).foreach { seed =>
      val random = new Random(seed)
      IdSeeding.review(random.shuffle(films), random.shuffle(clusters)) shouldBe expected
    }
  }

  it should "honour the persisted FilmId map: a film mapped earlier keeps its counter, and with it a contested cluster" in {
    // `f00000000000abc` was seeded first (counter 1) when it was the larger film; today `dune|2021`
    // is larger, but the stored order stands — the map is append-only, so seeding never re-ranks.
    val stored = FilmIdCounters.of(Seq(FilmIdCounter("f00000000000abc", 1))).toOption.get
    val r = IdSeeding.review(films, clusters, stored)
    r.keeps("f00000000000abc") shouldBe (big ++ small)
    r.mergedAway.map { case (f, winner) => f.id -> winner } shouldBe Seq("dune|2021" -> "f00000000000abc")
    IdSeeding.review(films, clusters, FilmIdCounters.empty) shouldBe IdSeeding.review(films, clusters)
  }
}
