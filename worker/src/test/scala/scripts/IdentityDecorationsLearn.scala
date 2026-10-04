package scripts

import models.Country
import play.api.libs.json.Json
import services.identity.TitleDecorations
import scripts.IdentityCalibrationData.{ProdSnapshot, TmdbAnswers, languageOf, listings, queries, responsesFile}

import java.nio.file.{Files, Path, Paths}

/**
 * Learns the identity resolver's VENUE DECORATIONS (`TitleDecorations.learn`) from the recorded
 * corpora and the film records their answers hold, and writes them with their provenance to
 * `common/src/main/resources/identity-decorations.json` — the refit path of the decorations the
 * resolver strips, run by `scripts/identity-calibrate.sh` before the weights are fitted (whose
 * title shapes read them). ONE rule for every country: the listings and records of every corpus
 * are pooled, and the artefact is a function of those sets and of the decorations `--out` already
 * holds, which a relearn keeps (`TitleDecorations.accumulate`; sorted, no clock).
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
    // Relearned on top of what `out` holds: a recording sees only that week's programmes (`TitleDecorations.accumulate`).
    val earlier = Option.when(Files.exists(out))(Json.parse(Files.readString(out)).as[TitleDecorations.Artefact])
    write(learn(corpora, fixtures, hard, opts.getOrElse("version", "unversioned"), earlier.fold(Seq.empty[TitleDecorations.Learned])(_.decorations)), out)
    println(s"wrote $out")
  }

  /** Every corpus listing's venue and titles, and every recorded film record's titles, of every
   *  country, learned from as one pool. */
  def learn(corpora: Path, fixtures: Path, hardClusters: Path, version: String,
            earlier: Seq[TitleDecorations.Learned] = Nil): TitleDecorations.Artefact = {
    val perCountry = Country.all.map { country =>
      val cc = country.code
      val sources = Seq(
        "full" -> corpora.resolve(s"cinema-scrapes-$cc.json.gz"),
        "hard-clusters" -> hardClusters.resolve(s"cinema-scrapes-hard-clusters-$cc.json.gz")).filter(p => Files.exists(p._2))
      val observed = listings(country, sources, ProdSnapshot(Map.empty, Nil), 0)
      val titled = observed.flatMap(o => (Seq(o.listing.title) ++ o.listing.rawTitle).distinct.map(o.venue -> _))
      val answers = new TmdbAnswers(Seq(fixtures.resolve(s"enrichment-$cc")).filter(Files.isDirectory(_)),
        responsesFile(hardClusters.resolve(s"hard-clusters-responses-$cc.json.gz")), languageOf(country))
      // Whether each listing title's own searches were all recorded and all found nothing.
      val searches = observed.flatMap { o =>
        val qs = queries(o.listing)
        val empty = qs.nonEmpty && qs.forall(q => answers.search(q).exists(_.isEmpty))
        (Seq(o.listing.title) ++ o.listing.rawTitle).distinct.map(_ -> empty)
      }
      val films = answers.films
      println(s"[$cc] ${titled.size} listing titles, ${films.size} film records")
      (titled, films.flatMap(f => Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles), searches)
    }
    val titled  = perCountry.flatMap(_._1)
    val records = perCountry.flatMap(_._2)
    TitleDecorations.Artefact(version,
      s"an edge token run of listing titles whose remainder is another listing's whole title, for >= ${TitleDecorations.MinFilms} " +
        s"different remainders — or for one remainder that is a recorded film record's title exactly, at >= ${TitleDecorations.MinVenues} venues, " +
        "where the decorated title's own recorded searches found nothing and no longer record title runs on from the remainder into " +
        "the run — that no recorded film record's title, original title or " +
        "alternative title carries",
      Map("listingTitles" -> titled.size, "venues" -> titled.map(_._1).distinct.size, "recordTitles" -> records.distinct.size),
      TitleDecorations.accumulate(earlier, TitleDecorations.learn(titled, records, perCountry.flatMap(_._3)), records))
  }

  def write(artefact: TitleDecorations.Artefact, out: Path): Unit = {
    Files.createDirectories(out.getParent)
    Files.writeString(out, Json.prettyPrint(Json.toJson(artefact)) + "\n")
  }
}
