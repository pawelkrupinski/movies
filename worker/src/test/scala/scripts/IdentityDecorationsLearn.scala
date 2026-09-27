package scripts

import models.Country
import play.api.libs.json.Json
import services.identity.TitleDecorations
import scripts.IdentityCalibrationData.{ProdSnapshot, TmdbAnswers, languageOf, listings, responsesFile}

import java.nio.file.{Files, Path, Paths}

/**
 * Learns the identity resolver's VENUE DECORATIONS (`TitleDecorations.learn`) from the recorded
 * corpora and the film records their answers hold, and writes them with their provenance to
 * `common/src/main/resources/identity-decorations.json` — the refit path of the decorations the
 * resolver strips, run by `scripts/identity-calibrate.sh` before the weights are fitted (whose
 * title shapes read them). ONE rule for every country: the listings and records of every corpus
 * are pooled, and the artefact is a function of those sets (sorted, no clock).
 *
 *   worker/Test/runMain scripts.IdentityDecorationsLearn --corpora <dir> --fixtures <dir> [--out <json>] [--version <v>]
 */
object IdentityDecorationsLearn {

  val Artefact: Path = Paths.get("common/src/main/resources", TitleDecorations.ResourcePath)

  def main(args: Array[String]): Unit = {
    val opts = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    def path(k: String) = opts.get(k).map(Paths.get(_))
    val corpora  = path("corpora").getOrElse(sys.error("--corpora <dir of cinema-scrapes-<cc>.json.gz>"))
    val fixtures = path("fixtures").getOrElse(sys.error("--fixtures <dir of enrichment-<cc>/ trees>"))
    val hard     = path("hard-clusters").getOrElse(Paths.get("test/resources/fixtures/corpus"))
    val out      = path("out").getOrElse(Artefact)
    write(learn(corpora, fixtures, hard, opts.getOrElse("version", "unversioned")), out)
    println(s"wrote $out")
  }

  /** Every corpus listing's venue and titles, and every recorded film record's titles, of every
   *  country, learned from as one pool. */
  def learn(corpora: Path, fixtures: Path, hardClusters: Path, version: String): TitleDecorations.Artefact = {
    val perCountry = Country.all.map { country =>
      val cc = country.code
      val sources = Seq(
        "full" -> corpora.resolve(s"cinema-scrapes-$cc.json.gz"),
        "hard-clusters" -> hardClusters.resolve(s"cinema-scrapes-hard-clusters-$cc.json.gz")).filter(p => Files.exists(p._2))
      val titled = listings(country, sources, ProdSnapshot(Map.empty, Nil), 0)
        .flatMap(o => (Seq(o.listing.title) ++ o.listing.rawTitle).distinct.map(o.venue -> _))
      val answers = new TmdbAnswers(Seq(fixtures.resolve(s"enrichment-$cc")).filter(Files.isDirectory(_)),
        responsesFile(hardClusters.resolve(s"hard-clusters-responses-$cc.json.gz")), languageOf(country))
      val films = answers.films
      println(s"[$cc] ${titled.size} listing titles, ${films.size} film records")
      (titled, films.flatMap(f => Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles))
    }
    val titled  = perCountry.flatMap(_._1)
    val records = perCountry.flatMap(_._2)
    TitleDecorations.Artefact(version,
      s"an edge token run of listing titles whose remainder is another listing's whole title, for >= ${TitleDecorations.MinFilms} " +
        "different remainders, that no recorded film record's title, original title or alternative title carries",
      Map("listingTitles" -> titled.size, "venues" -> titled.map(_._1).distinct.size, "recordTitles" -> records.distinct.size),
      TitleDecorations.learn(titled, records))
  }

  def write(artefact: TitleDecorations.Artefact, out: Path): Unit = {
    Files.createDirectories(out.getParent)
    Files.writeString(out, Json.prettyPrint(Json.toJson(artefact)) + "\n")
  }
}
