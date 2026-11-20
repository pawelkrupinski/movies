package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titles.TitleNormalizer
import tools.UnmatchedClusters

/**
 * The acceptance rules' equivalence phase: every ranking the resolve hands [[Acceptance]] over the five checked-in
 * unmatched-cluster captures and every drift slice -- each node's own candidates and each family's pooled ones -- read by today's
 * Acceptance and by its copy from before its rules became a table ([[LegacyAcceptance]]), every answer and every
 * refusal (the trace's rule, condition, candidate and detail) rendered alike. The rewrite moves no take and no trace
 * line while this holds; the copy and this spec go once it has landed.
 */
class AcceptanceEquivalenceSpec extends AnyFlatSpec with Matchers {
  import AcceptanceEquivalenceSpec.*

  "every ranking of the captures" should "be read alike by the table and by its copy from before" in {
    val captures = models.Country.all.map(UnmatchedClusters.fixturePath).filter(java.nio.file.Files.exists(_)).map(UnmatchedClusters.read)
    captures.size shouldBe 5
    val corpora = captures.map(capture => (capture.country.code, () => new ResolveStages(capture.listings, new UnmatchedClusters.Replay(capture),
        TitleNormalizer.forCountry(capture.country), IdentityCalibration.resolver, IdentityResolver.Mutation.None, PinConstraints(Nil), TitleDecorations.resolver))) ++
      scripts.IdentityDriftSlicesSpec.AllSlices.map { dir =>
        (s"drift-slices/${dir.getParent.getFileName}/${dir.getFileName}", () => {
          val corpus = FamilyConvergence.sliceCorpus(dir, "")
          new ResolveStages(corpus.listings, corpus.lookups, corpus.reads.normalizer, IdentityCalibration.resolver, IdentityResolver.Mutation.None,
            PinConstraints(Nil), TitleDecorations.resolver)
        })
      }
    val legacy = Legacy(new LegacyAcceptance(IdentityCalibration.resolver))
    val compared = corpora.map { case (name, build) =>
      val stages   = build()
      val nodes    = stages.generation.nodes
      val own      = nodes.map(stages.families.scoredOf)
      val pooled   = nodes.groupBy(node => stages.families.scopeOf(node)).toSeq.map { case (scope, members) => scope.pooled(members) }
      val rankings = (own ++ pooled).filter(_.nonEmpty) :+ Nil
      val current  = Current(stages.acceptance)
      val mismatch = rankings.iterator.map(ranked => (reading(current)(ranked), reading(legacy)(ranked)))
        .filter { case (now, was) => now != was }.take(5).toSeq
      withClue(s"$name: " + mismatch.map { case (now, was) =>
        s"\n  was ${was.filterNot(now.contains).mkString("\n      ")}\n  now ${now.filterNot(was.contains).mkString("\n      ")}" }.mkString) {
        mismatch shouldBe empty
      }
      name -> rankings.size
    }
    info(s"${compared.size} corpora, ${compared.map(_._2).sum} rankings: " + compared.take(5).map { case (name, n) => s"$name $n" }.mkString(", "))
    compared.map(_._2).sum should be > 3000
  }
}

object AcceptanceEquivalenceSpec {
  /** What one Acceptance answers, rendered: the two sides' `Refused` are distinct types. */
  trait Reads {
    def aloneNamed(ranked: Seq[Scored]): Option[(Scored.Accepted, String)]
    def pooledNamed(ranked: Seq[Scored]): Option[(Scored.Accepted, String)]
    def refusals(ranked: Seq[Scored]): Seq[DecisionTrace.Refusal]
    def ruleNames: Seq[String]
    def verdict(rule: String, ranked: Seq[Scored]): String
    def topHit(ranked: Seq[Scored]): Option[Scored.Accepted]
    def calibrated(ranked: Seq[Scored]): String
    def seasonProduction(ranked: Seq[Scored]): Option[Option[Scored.Accepted]]
    def fallback(ranked: Seq[Scored]): Option[Scored]
    def leaning(ranked: Seq[Scored]): Option[Scored]
    def billsSeveralWorks(ranked: Seq[Scored]): Boolean
    def confidenceOf(ranked: Seq[Scored], film: Int): Double
    def liftedBy(ranked: Seq[Scored], scored: Scored, confidence: Double): String
    def copiedBeyondItsBilling(scored: Scored): Boolean
    def editionNamed(ranked: Seq[Scored])(accepted: Scored.Accepted): Scored.Accepted
  }

  final case class Current(a: Acceptance) extends Reads {
    def aloneNamed(ranked: Seq[Scored])  = a.aloneNamed(ranked)
    def pooledNamed(ranked: Seq[Scored]) = a.pooledNamed(ranked)
    def refusals(ranked: Seq[Scored])    = a.refusals(ranked)
    def ruleNames                        = (a.aloneRules ++ a.pooledRules).map(_.name).distinct
    def verdict(rule: String, ranked: Seq[Scored]) =
      rendered((a.aloneRules ++ a.pooledRules).find(_.name == rule).get.accepts(ranked).left.map(r => (r.why, r.film, r.detail)))
    def topHit(ranked: Seq[Scored])      = a.topHit(ranked)
    def calibrated(ranked: Seq[Scored])  = rendered(a.calibratedWhy(ranked).left.map(r => (r.why, r.film, r.detail)))
    def seasonProduction(ranked: Seq[Scored]) = a.seasonProduction(ranked)
    def fallback(ranked: Seq[Scored])    = a.fallback(ranked)
    def leaning(ranked: Seq[Scored])     = a.leaning(ranked)
    def billsSeveralWorks(ranked: Seq[Scored]) = a.billsSeveralWorks(ranked)
    def confidenceOf(ranked: Seq[Scored], film: Int) = a.confidenceOf(ranked, film)
    def liftedBy(ranked: Seq[Scored], scored: Scored, confidence: Double) = a.liftedBy(ranked, scored, confidence)
    def copiedBeyondItsBilling(scored: Scored) = a.copiedBeyondItsBilling(scored)
    def editionNamed(ranked: Seq[Scored])(accepted: Scored.Accepted) = a.editionNamed(ranked)(accepted)
  }

  final case class Legacy(a: LegacyAcceptance) extends Reads {
    def aloneNamed(ranked: Seq[Scored])  = a.aloneNamed(ranked)
    def pooledNamed(ranked: Seq[Scored]) = a.pooledNamed(ranked)
    def refusals(ranked: Seq[Scored])    = a.refusals(ranked)
    def ruleNames                        = (a.aloneRules ++ a.pooledRules).map(_.name).distinct
    def verdict(rule: String, ranked: Seq[Scored]) =
      rendered((a.aloneRules ++ a.pooledRules).find(_.name == rule).get.accepts(ranked).left.map(r => (r.why, r.film, r.detail)))
    def topHit(ranked: Seq[Scored])      = a.topHit(ranked)
    def calibrated(ranked: Seq[Scored])  = rendered(a.calibratedWhy(ranked).left.map(r => (r.why, r.film, r.detail)))
    def seasonProduction(ranked: Seq[Scored]) = a.seasonProduction(ranked)
    def fallback(ranked: Seq[Scored])    = a.fallback(ranked)
    def leaning(ranked: Seq[Scored])     = a.leaning(ranked)
    def billsSeveralWorks(ranked: Seq[Scored]) = a.billsSeveralWorks(ranked)
    def confidenceOf(ranked: Seq[Scored], film: Int) = a.confidenceOf(ranked, film)
    def liftedBy(ranked: Seq[Scored], scored: Scored, confidence: Double) = a.liftedBy(ranked, scored, confidence)
    def copiedBeyondItsBilling(scored: Scored) = a.copiedBeyondItsBilling(scored)
    def editionNamed(ranked: Seq[Scored])(accepted: Scored.Accepted) = a.editionNamed(ranked)(accepted)
  }

  private def taken(accepted: Scored.Accepted): String = s"${accepted._1.candidate.tmdbId}@${accepted._2}"
  private def rendered(verdict: Either[(String, Option[Int], String), Scored.Accepted]): String =
    verdict.fold({ case (why, film, detail) => s"refused [$why] ${film.getOrElse("-")} [$detail]" }, taken)

  /** Every answer `reads` gives of `ranked`, one line each, in a fixed order. */
  def reading(reads: Reads)(ranked: Seq[Scored]): Seq[String] =
    Seq(
      "alone "    + reads.aloneNamed(ranked).map { case (accepted, rule) => s"${taken(accepted)} $rule" },
      "pooled "   + reads.pooledNamed(ranked).map { case (accepted, rule) => s"${taken(accepted)} $rule" },
      "top "      + reads.topHit(ranked).map(taken),
      "calibrated " + reads.calibrated(ranked),
      "season "   + reads.seasonProduction(ranked).map(_.map(taken)),
      "fallback " + reads.fallback(ranked).map(_.candidate.tmdbId),
      "leaning "  + reads.leaning(ranked).map(_.candidate.tmdbId),
      "bills "    + reads.billsSeveralWorks(ranked)) ++
      reads.refusals(ranked).map(r => s"refusal ${r.rule} [${r.why}] ${r.film} [${r.detail}]") ++
      reads.ruleNames.map(rule => s"rule $rule ${reads.verdict(rule, ranked)}") ++
      ranked.map { scored =>
        val id         = scored.candidate.tmdbId
        val confidence = reads.confidenceOf(ranked, id)
        s"candidate $id confidence $confidence lifted [${reads.liftedBy(ranked, scored, confidence)}] copied ${reads.copiedBeyondItsBilling(scored)}" +
          s" edition ${taken(reads.editionNamed(ranked)(scored -> scored.probability))}"
      }
}
