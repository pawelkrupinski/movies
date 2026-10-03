package services.identity

import models.{KinoApollo, Multikino, Rialto}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer}

import scala.collection.mutable

/** The identity trace: which rules decided each listing — recorded on the decision, filed per listing by the
 *  incremental model, and never kept in its memory. */
class IdentityTraceSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  import FilmTable.{F, listing}

  private val lynch = Seq(F(1018, "Mulholland Drive", 2001, "David Lynch", 146), F(2, "Mulholland Falls", 1996, "Lee Tamahori", 107, 4))
  private val credited = listing(Rialto, "Mulholland Drive", Some(2001), Some("David Lynch"), Some(146))
  private val bare     = listing(KinoApollo, "Mulholland Drive")

  "a decision" should "name the rule each member was accepted by and what joined it" in {
    val r = IdentityResolver.resolve(Seq(credited, bare), new FilmTable(lynch, normalizer), normalizer, IdentityCalibration.resolver)
    val decision = r.decisionOf(credited.key)
    decision.film shouldBe Some(1018)
    val rules = decision.trace.rulesOf(credited.key)
    rules.filter(_.startsWith("accept:")) should not be empty
    // joined to it by the strongest link the pair has: here both took the film alone
    decision.trace.rulesOf(bare.key) should contain ("join:same-film")
  }

  it should "name the member whose own evidence vetoed the cluster's film" in {
    val wrongDirector = listing(Multikino, "Mulholland Drive", Some(1961), Some("Lee Tamahori"), Some(62))
    val r = IdentityResolver.resolve(Seq(wrongDirector), new FilmTable(lynch.take(1), normalizer), normalizer, IdentityCalibration.resolver)
    val decision = r.decisionOf(wrongDirector.key)
    withClue(decision.render) {
      decision.basis shouldBe ResolverDecision.Basis.Vetoed
      decision.trace.vetoed.flatMap(_.by) shouldBe defined
      decision.trace.rulesOf(wrongDirector.key).exists(_.startsWith("veto:")) shouldBe true
    }
  }

  it should "say, for a listing no rule took, which condition stopped each rule" in {
    // two films of one title and nothing published: no rule can tell them apart
    val films = Seq(F(1, "Tatarak", 2009, "Andrzej Wajda", 85), F(2, "Tatarak", 1965, "Someone Else", 90))
    val bare  = listing(Rialto, "Tatarak")
    val r = IdentityResolver.resolve(Seq(bare), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    val rules = r.decisionOf(bare.key).trace.rulesOf(bare.key)
    withClue(rules.mkString("\n")) {
      // taken, if at all, only by the cluster's pooled vote — each rule that could take it alone says why it did not
      rules.filter(_.startsWith("accept:")) shouldBe empty
      rules should contain ("refused:dated-title:no-film-of-its-title-from-the-year-its-title-dates")
      rules should contain ("refused:exact-top-hit:no-evidence-class-measured-for-it")
      rules.count(_.startsWith("refused:")) shouldBe 10
    }
    // and what stopped it, about which candidate: the rival it lost to, the two films its title names, the facts it lacks
    val refusals = r.decisionOf(bare.key).trace.nodes(bare.key).refusals.map(refusal => refusal.rule -> refusal).toMap
    withClue(refusals.values.mkString("\n")) {
      refusals("favoured-calibrated").film shouldBe Some(1)
      refusals("favoured-calibrated").detail shouldBe "2 Tatarak (1965)"
      refusals("segment-top-hit").detail shouldBe "1 Tatarak (2009); 2 Tatarak (1965)"
      refusals("directors-work").detail shouldBe "the listing credits no director"
      refusals("dated-title").detail shouldBe "the title dates no year"
      refusals("sole-work").detail should startWith ("first hit ")
    }
    // a listing a rule took names no refusal
    val credited2 = listing(Rialto, "Tatarak", Some(2009), Some("Andrzej Wajda"), Some(85))
    val taken = IdentityResolver.resolve(Seq(credited2), new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)
    taken.decisionOf(credited2.key).trace.rulesOf(credited2.key).filter(_.startsWith("refused:")) shouldBe empty
  }

  "a title" should "name the title rules it took and the formats peeled off it" in {
    // `xtra-pokaz-filmu`: "Klub Filmowy: pokaz filmu „Mira”" searches as "Mira"
    normalizer.firedRules(Rialto, "Klub Filmowy: pokaz filmu \"Mira\"") should contain ("title:xtra-pokaz-filmu")
    normalizer.firedRules(Rialto, "Mira") shouldBe empty
    normalizer.firedRules(Rialto, "Mistyczka 2D napisy").filter(_.startsWith("format:")) should not be empty
  }

  "the incremental model" should "file each re-resolved family's traces, drop a removed family's, and keep none in memory" in {
    val filed   = mutable.LinkedHashMap.empty[ListingKey, ListingTrace]
    val dropped = mutable.ArrayBuffer.empty[String]
    val sink = new IdentityTraceStore {
      def replace(removed: Set[String], added: () => Seq[ListingTrace]): Unit = {
        dropped ++= removed
        filed.filterInPlace((_, trace) => !removed(trace.family))
        added().foreach(trace => filed(trace.listing) = trace)
      }
    }
    val model = new IncrementalResolver(new FilmTable(lynch, normalizer), normalizer, IdentityCalibration.resolver, traces = sink)
    model.listingsSeen(Seq(credited, bare))
    filed.keySet shouldBe Set(credited.key, bare.key)
    filed(credited.key).film shouldBe Some(1018)
    filed(credited.key).rules.exists(_.startsWith("accept:")) shouldBe true
    // why, with the numbers: each measure against the film it was decided on, and its weight
    filed(credited.key).weighedFilm shouldBe Some(1018)
    filed(credited.key).evidence.exists(_.startsWith("director=same_person +")) shouldBe true
    filed(credited.key).evidence.exists(_.startsWith("year.delta=")) shouldBe true
    model.decisions.map(_.trace) shouldBe Seq(DecisionTrace.Empty)
    model.listingsGone(Seq(bare.key))
    dropped should not be empty
    filed.keySet shouldBe Set(credited.key)
  }
}
