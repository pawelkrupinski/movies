package scripts

import models.Country
import services.movies.LatestTitleYear
import tools.{CommandLine, CorpusFixture, HardClusters}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

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
  // The latest year a title may name (`LatestTitleYear`), read off this clock at each ask.
  private given LatestTitleYear = LatestTitleYear(java.time.Clock.systemUTC())

  val commandLine: CommandLine.Commands = CommandLine.Commands("BuildHardClusters", "Builds and grows the hard-cluster corpus fixture.",
    CommandLine("select", "every seed, then the budget's automatic picks split across the countries whose corpus is given")
      .withPositionals(CommandLine.Positional("corpus-dir", "the cinema-scrapes-<cc>.json.gz corpora (required)"),
        CommandLine.Positional("budget", s"listings of automatic picks (default: ${HardClusters.DefaultBudget})")),
    CommandLine("extend", "adds the clusters of the titles found to the existing fixture")
      .withPositionals(CommandLine.Positional("corpus-dir", "the cinema-scrapes-<cc>.json.gz corpora (required)"),
        CommandLine.Positional("titles", "the findings, code<TAB>title per line (required)")))

  def main(args: Array[String]): Unit = commandLine.parseOrExit(args) match {
    case ("select", arguments) =>
      select(arguments.requiredPositional("corpus-dir"), arguments.positionalInt("budget").getOrElse(HardClusters.DefaultBudget))
    case (_, arguments) => extend(arguments.requiredPositional("corpus-dir"), arguments.requiredPositional("titles"))
  }

  /** The countries whose corpus `dir` holds, each with its corpus. */
  def corporaIn(dir: String): Seq[(Country, Path)] = Country.all.flatMap(c => corpusIn(dir, c).map(c -> _))

  private def corpusIn(dir: String, country: Country) =
    Some(Paths.get(dir, s"cinema-scrapes-${country.code}.json.gz")).filter(Files.exists(_))

  /** The findings of a titles file (`code<TAB>title`, as scripts/convergence-findings.py writes them; columns after
   *  the title ignored), as seeds — a line with no title is none. */
  def findings(lines: Seq[String]): Seq[HardClusters.Seed] = lines.map(_.split("\t", -1)).collect {
    case Array(code, title, _*) if title.trim.nonEmpty => HardClusters.Seed(code.trim.toLowerCase(java.util.Locale.ROOT), title.trim, HardClusters.Reason.Finding.label)
  }

  /** The findings no seed holds yet, each once. */
  def fresh(found: Seq[HardClusters.Seed], known: Seq[HardClusters.Seed]): Seq[HardClusters.Seed] = {
    val held = known.map(s => (s.country, s.title)).toSet
    found.filterNot(s => held((s.country, s.title))).distinct
  }

  /** The fresh findings by country code, in code order: each with the country and its corpus in `dir`, or none —
   *  a code no country has, or a country `dir` holds no corpus of — when they are skipped. */
  def extensions(fresh: Seq[HardClusters.Seed], dir: String): Seq[(String, Option[(Country, Path)], Seq[HardClusters.Seed])] =
    fresh.groupBy(_.country).toSeq.sortBy(_._1).map { case (code, seeds) =>
      (code, Country.byCode(code).flatMap(country => corpusIn(dir, country).map(country -> _)), seeds)
    }

  private def select(dir: String, budget: Int)(using LatestTitleYear): Unit = {
    val present = corporaIn(dir)
    val seeds   = HardClusters.readSeeds()
    val share   = budget / present.size.max(1)
    present.foreach { case (country, path) =>
      val (rows, picked) = HardClusters.select(country, CorpusFixture.readFrom(path), seeds, share)
      val out = CorpusFixture.write(HardClusters.corpusKey(country), rows).path
      HardClusters.appendSeeds(picked)
      println(s"[hard-clusters] ${country.code}: ${rows.size} venues, ${rows.map(_.films.size).sum} listings " +
              s"(${picked.size} automatic clusters) -> $out")
    }
  }

  private def extend(dir: String, titlesFile: String)(using LatestTitleYear): Unit = {
    val found = fresh(findings(Files.readAllLines(Paths.get(titlesFile), StandardCharsets.UTF_8).asScala.toSeq), HardClusters.readSeeds())
    if (found.isEmpty) println("[hard-clusters] every finding is already in the fixture")
    extensions(found, dir).foreach {
      case (code, None, seeds) => System.err.println(s"[hard-clusters] $code: no corpus in $dir — skipped ${seeds.size} finding(s)")
      case (code, Some((country, path)), seeds) =>
        val key      = HardClusters.corpusKey(country)
        val existing = if (CorpusFixture.exists(key)) CorpusFixture.read(key) else Nil
        val before   = existing.map(_.films.size).sum
        val grown    = HardClusters.extend(country, existing, CorpusFixture.readFrom(path), seeds)
        val _ = CorpusFixture.write(key, grown)
        HardClusters.appendSeeds(seeds)
        println(s"[hard-clusters] $code: +${grown.map(_.films.size).sum - before} listings for " +
                s"${seeds.map(_.title).mkString(", ")}")
    }
  }
}
