package scripts

import models.Country
import tools.{CorpusFixture, HardClusters}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/**
 * Builds and grows the hard-cluster fixture `HardClusterConvergenceIntegrationSpec`
 * replays. See `tools.HardClusters`, and `scripts/hard-clusters.sh`, which wraps this
 * with the downloads and the response re-recording.
 *
 * {{{
 *   # From scratch: every seed in hard-clusters-seeds.tsv, then `budget` listings of
 *   # automatic picks, split evenly across the countries whose corpus is given.
 *   sbt 'worker/Test/runMain scripts.BuildHardClusters select <dir-with-cinema-scrapes-<cc>.json.gz> [budget]'
 *
 *   # The ratchet: add the clusters of the titles in <titles.tsv> (`code<TAB>title` per
 *   # line, as scripts/convergence-findings.py writes them) to the existing fixture.
 *   sbt 'worker/Test/runMain scripts.BuildHardClusters extend <dir-with-cinema-scrapes-<cc>.json.gz> <titles.tsv>'
 * }}}
 */
object BuildHardClusters {

  def main(args: Array[String]): Unit = args.toList match {
    case "select" :: dir :: rest => select(dir, rest.headOption.map(_.toInt).getOrElse(HardClusters.DefaultBudget))
    case "extend" :: dir :: titles :: Nil => extend(dir, titles)
    case _ =>
      System.err.println("usage: BuildHardClusters select <corpus-dir> [budget] | extend <corpus-dir> <titles.tsv>")
      sys.exit(2)
  }

  private def corpusIn(dir: String, country: Country) =
    Some(Paths.get(dir, s"cinema-scrapes-${country.code}.json.gz")).filter(Files.exists(_))

  private def select(dir: String, budget: Int): Unit = {
    val present = Country.all.flatMap(c => corpusIn(dir, c).map(c -> _))
    val seeds   = HardClusters.readSeeds()
    val share   = budget / present.size.max(1)
    present.foreach { case (country, path) =>
      val (rows, picked) = HardClusters.select(country, CorpusFixture.readFrom(path), seeds, share)
      val out = CorpusFixture.write(HardClusters.corpusKey(country), rows)
      HardClusters.appendSeeds(picked)
      println(s"[hard-clusters] ${country.code}: ${rows.size} venues, ${rows.map(_.films.size).sum} listings " +
              s"(${picked.size} automatic clusters) -> $out")
    }
  }

  private def extend(dir: String, titlesFile: String): Unit = {
    val titles = Files.readAllLines(Paths.get(titlesFile), StandardCharsets.UTF_8).toArray(Array.empty[String]).toSeq
      .map(_.split("\t", -1)).collect { case Array(code, title, _*) if title.trim.nonEmpty =>
        HardClusters.Seed(code.trim.toLowerCase, title.trim, HardClusters.Reason.Finding.label) }
    val known = HardClusters.readSeeds().map(s => (s.country, s.title)).toSet
    val fresh = titles.filterNot(s => known.contains((s.country, s.title))).distinct
    if (fresh.isEmpty) println("[hard-clusters] every finding is already in the fixture")
    fresh.groupBy(_.country).toSeq.sortBy(_._1).foreach { case (code, seeds) =>
      Country.all.find(_.code == code).zip(Country.all.find(_.code == code).flatMap(corpusIn(dir, _))) match {
        case None => System.err.println(s"[hard-clusters] $code: no corpus in $dir — skipped ${seeds.size} finding(s)")
        case Some((country, path)) =>
          val key      = HardClusters.corpusKey(country)
          val existing = if (CorpusFixture.exists(key)) CorpusFixture.read(key) else Nil
          val before   = existing.map(_.films.size).sum
          val grown    = HardClusters.extend(country, existing, CorpusFixture.readFrom(path), seeds)
          CorpusFixture.write(key, grown)
          HardClusters.appendSeeds(seeds)
          println(s"[hard-clusters] $code: +${grown.map(_.films.size).sum - before} listings for " +
                  s"${seeds.map(_.title).mkString(", ")}")
      }
    }
  }
}
