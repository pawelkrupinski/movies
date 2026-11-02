package services.identity

import models.{Helios, HeliosMagnolia, KinoApollo, KinoBulgarska, KinoMikro, KinoMuza, KinoOaza, KinoPalacowe, Kinoteka, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

/**
 * A BARE listing -- no fact of its own, no own match, nothing but its title joining it to a film -- whose title family is
 * CONTESTED takes no film (docs/investigations/2026-11-01-flip-unification.md, stage 5; the owner: "unmatched beats
 * wrong"): two works, each backed by more than one venue's own statements, and nothing of the bare listing's to tell them
 * apart by. It followed the family's majority, whose bound a venue arriving crossed either way: UK Beehive Honiton's
 * "Nostalgic Cinema: The Addams Family" went 2907 -> 481084 -> 2907 as venues crediting Sonnenfeld's 1991 film and
 * Vernon's 2019 one arrived (drift seed 58). As listings arrive, a bare listing's take can now only go to none.
 */
class ContestedFamilySpec extends AnyFlatSpec with Matchers {
  import FilmTable.{F, listing}
  private val normalizer = SingleCountryNormalizer.titleNormalizer

  private val films = Seq(F(2907, "The Addams Family", 1991, "Barry Sonnenfeld", 99, 30.0), F(481084, "The Addams Family", 2019, "Conrad Vernon", 87, 40.0))
  private def sonnenfeld(venues: Seq[models.Cinema]) = venues.map(listing(_, "The Addams Family", Some(1991), Some("Barry Sonnenfeld"), Some(99)))
  private def vernon(venues: Seq[models.Cinema]) = venues.map(listing(_, "The Addams Family", Some(2019), Some("Conrad Vernon"), Some(87)))
  private val bare = listing(Kinoteka, "Nostalgic Cinema: The Addams Family")

  private def whole(listings: Seq[Listing]) = IdentityResolver.resolve(listings, new FilmTable(films, normalizer), normalizer, ResolveInputs.Unpinned)
  private def rendered(r: Resolution, ls: Seq[Listing]) = ls.map(l => r.decisionOf(l.key).render).distinct.mkString("\n")

  private val contested = sonnenfeld(Seq(KinoMuza, Helios)) ++ vernon(Seq(Multikino, KinoPalacowe, KinoOaza, KinoMikro, KinoBulgarska))

  "A bare listing in a contested title family" should "take no film, its siblings their own" in {
    val r = whole(contested :+ bare)
    withClue(rendered(r, contested :+ bare)) {
      r.decisionOf(bare.key).film shouldBe None
      r.decisionOf(bare.key).explanation.exists(_.startsWith("contested title family")) shouldBe true
      contested.take(2).map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(2907))
      contested.drop(2).map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(Some(481084))
    }
  }

  // Contested is absorbing: each work's venues only grow as listings arrive, so once both are well supported the bare
  // listing stays on none. Before that it follows its siblings as before (one venue each side contests nothing).
  it should "stay on no film once its family is contested, whatever order the family's listings arrive in" in {
    val lookups = new FilmTable(films, normalizer)
    def contestedAt(prefix: Seq[Listing]) =
      Seq(1991, 2019).forall(year => prefix.count(l => l.year.contains(year)) >= ClusterVoting.WellSupported)
    val left = (1L to 8L).flatMap { seed =>
      PopulationMonotonicity.chains(contested :+ bare, seed, 8, 8).flatMap { chain =>
        chain.filter(_.contains(bare)).dropWhile(prefix => !contestedAt(prefix)).flatMap { prefix =>
          val r = IdentityResolver.resolve(prefix, lookups, normalizer, ResolveInputs.Unpinned)
          r.decisionOf(bare.key).film.map(film => s"seed $seed, ${prefix.size} listings: $film")
        }
      }
    }
    left shouldBe empty
  }

  // the guards: a family one venue's statement contests is not contested; a family backing one film lends it
  "A bare listing beside one venue's other film" should "still follow its family: one venue's credit may be its own slip" in {
    val siblings = sonnenfeld(Seq(KinoMuza)) ++ vernon(Seq(Multikino, KinoPalacowe, KinoOaza, KinoMikro, KinoBulgarska, HeliosMagnolia, KinoApollo))
    val r = whole(siblings :+ bare)
    withClue(rendered(r, siblings :+ bare))(r.decisionOf(bare.key).explanation.exists(_.startsWith("contested title family")) shouldBe false)
  }

  "A bare listing beside one film's venues alone" should "take that film" in {
    val siblings = vernon(Seq(Multikino, KinoPalacowe, KinoOaza, KinoMikro, KinoBulgarska))
    val r = whole(siblings :+ bare)
    withClue(rendered(r, siblings :+ bare))(r.decisionOf(bare.key).film shouldBe Some(481084))
  }
}
