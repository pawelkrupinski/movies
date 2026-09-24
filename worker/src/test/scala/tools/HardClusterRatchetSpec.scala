package tools

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

/**
 * The hard-cluster fixture is a RATCHET: it only grows, and its exemptions only shrink. The
 * integration spec that replays it runs under itAll; this holds the ratchet's own shape on
 * every push, so a hand-pruned seed or a quietly added exemption fails here, in seconds.
 */
class HardClusterRatchetSpec extends AnyFlatSpec with Matchers {

  private val seeds = HardClusters.readSeeds()

  "The hard-cluster seeds" should "never shrink below the floor, nor repeat" in {
    withClue(s"${seeds.size} seeds — the fixture only grows (scripts/hard-clusters.sh appends); a seed removed by " +
             "hand drops a cluster that has already gone wrong: ") {
      seeds.size should be >= HardClusterExemptions.MinSeeds
    }
    seeds.groupBy(s => (s.country, s.title)).collect { case (k, dup) if dup.size > 1 => k } shouldBe empty
  }

  it should "each still have its cluster in the country's recorded corpus" in {
    val missing = seeds.groupBy(_.country).toSeq.flatMap { case (code, countrySeeds) =>
      val country = Country.byCode(code).getOrElse(fail(s"seed for an unknown country '$code'"))
      val corpus  = CorpusFixture.read(HardClusters.corpusKey(country))
      countrySeeds.filter(seed => HardClusters.select(country, corpus, Seq(seed), budget = 0)._1.isEmpty)
        .map(seed => s"$code: ${seed.title}")
    }
    withClue("seeds whose listings the corpus does not hold — re-record with scripts/hard-clusters.sh: ") {
      missing shouldBe empty
    }
  }

  "The exemptions" should "stay within their ceiling" in {
    withClue(s"exempted: ${HardClusterExemptions.all.mkString(", ")} — ") {
      HardClusterExemptions.all.size should be <= HardClusterExemptions.MaxEntries
    }
  }

  it should "each name one film of a seeded cluster, by its full stored key" in {
    val bad = HardClusterExemptions.all.filterNot { case (code, key) =>
      val normalizer = Country.byCode(code).map(TitleNormalizer.forCountry)
      key.count(_ == '|') == 1 && normalizer.exists { n =>
        seeds.exists(s => s.country == code && key.takeWhile(_ != '|').contains(n.sanitize(s.title)))
      }
    }
    bad shouldBe empty
  }
}
