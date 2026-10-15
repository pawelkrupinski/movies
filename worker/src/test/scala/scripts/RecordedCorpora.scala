package scripts

import models.Country
import scripts.IdentityCalibrationData.{TmdbAnswers, languageOf, responsesFile}
import tools.CommandLine

import java.nio.file.{Files, Path}

/** The recorded inputs a relearn reads ([[IdentityCalibrate]], [[IdentityDecorationsLearn]]): the full corpora, the
 *  enrichment trees their searches were answered from, and the hard-cluster corpora with their responses. */
final case class RecordedCorpora(corpora: Path, fixtures: Path, hardClusters: Path) {

  /** A country's corpora that exist, by where they come from (`full`, `hard-clusters`). */
  def sources(country: Country): Seq[(String, Path)] = Seq(
    "full" -> corpora.resolve(s"cinema-scrapes-${country.code}.json.gz"),
    "hard-clusters" -> hardClusters.resolve(s"cinema-scrapes-hard-clusters-${country.code}.json.gz")).filter(p => Files.exists(p._2))

  /** The recorded TMDB answers of a country: its enrichment tree's and its hard clusters' responses. */
  def answers(country: Country): TmdbAnswers =
    new TmdbAnswers(Seq(fixtures.resolve(s"enrichment-${country.code}")).filter(Files.isDirectory(_)),
      responsesFile(hardClusters.resolve(s"hard-clusters-responses-${country.code}.json.gz")), languageOf(country))
}

object RecordedCorpora {

  val options: Seq[CommandLine.Value] = Seq(
    CommandLine.Value("corpora", "dir", "the cinema-scrapes-<cc>.json.gz corpora (required)"),
    CommandLine.Value("fixtures", "dir", "the enrichment-<cc>/ trees (required)"),
    CommandLine.Value("hard-clusters", "dir", "the hard-cluster corpora and their responses", default = Some("test/resources/fixtures/corpus")))

  def of(arguments: CommandLine.Arguments): RecordedCorpora =
    RecordedCorpora(arguments.requiredPath("corpora"), arguments.requiredPath("fixtures"), arguments.requiredPath("hard-clusters"))
}
