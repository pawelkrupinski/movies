package scripts

import models.Country
import play.api.libs.json.Json
import services.identity.{TitleDecorations, TitleDecorationsLearner}
import scripts.IdentityCalibrationData.{ProdSnapshot, listings, queries}
import tools.CommandLine

import java.nio.file.{Files, Path, Paths}

/**
 * Learns the identity resolver's VENUE DECORATIONS (`TitleDecorationsLearner.learn`) from the recorded
 * corpora and the film records their answers hold, and writes them with their provenance to
 * `identity/src/main/resources/identity-decorations.json` — the refit path of the decorations the
 * resolver strips, run by `scripts/identity-calibrate.sh` before the weights are fitted (whose
 * title shapes read them). ONE rule for every country: the listings and records of every corpus
 * are pooled, and the artefact is a function of those sets and of the decorations `--out` already
 * holds, which a relearn keeps (`TitleDecorationsLearner.accumulate`; sorted, no clock).
 *
 *   worker/Test/runMain scripts.IdentityDecorationsLearn --corpora <dir> --fixtures <dir> [--out <json>] [--version <v>]
 */
object IdentityDecorationsLearn {

  val Artefact: Path = Paths.get("identity/src/main/resources", TitleDecorations.ResourcePath)

  val commandLine: CommandLine = CommandLine("IdentityDecorationsLearn", "Learns the title decorations from every country's recorded corpora.",
    RecordedCorpora.options ++ Seq(
      CommandLine.Value("out", "json", "the artefact relearned on top of and written", default = Some(Artefact.toString)),
      CommandLine.Value("version", "v", "the version the artefact is filed under", default = Some("unversioned")))*)

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val out       = arguments.requiredPath("out")
    val _ = relearn(RecordedCorpora.of(arguments), arguments.required("version"), out)
    println(s"wrote $out")
  }

  /** The decorations learned from `inputs` on top of those `out` holds — a recording sees only that week's programmes
   *  (`TitleDecorationsLearner.accumulate`) — written to `out` under `version`. */
  def relearn(inputs: RecordedCorpora, version: String, out: Path): TitleDecorations.Artefact = {
    val earlier  = Option.when(Files.exists(out))(Json.parse(Files.readString(out)).as[TitleDecorations.Artefact])
    val artefact = learn(inputs, version, earlier.fold(Seq.empty[TitleDecorations.Learned])(_.decorations))
    write(artefact, out)
    artefact
  }

  /** Every corpus listing's venue and titles, and every recorded film record's titles, of every
   *  country, learned from as one pool. */
  def learn(inputs: RecordedCorpora, version: String, earlier: Seq[TitleDecorations.Learned] = Nil): TitleDecorations.Artefact = {
    val perCountry = Country.all.map { country =>
      val cc = country.code
      val observed = listings(country, inputs.sources(country), ProdSnapshot(Map.empty, Nil), 0)
      val titled = observed.flatMap(o => (Seq(o.listing.title) ++ o.listing.rawTitle).distinct.map(o.venue -> _))
      val answers = inputs.answers(country)
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
      s"an edge token run of listing titles whose remainder is another listing's whole title, for >= ${TitleDecorationsLearner.MinFilms} " +
        s"different remainders — or for one remainder that is a recorded film record's title exactly, at >= ${TitleDecorationsLearner.MinVenues} venues, " +
        "where the decorated title's own recorded searches found nothing and no longer record title runs on from the remainder into " +
        "the run, and the remainder is two words or five letters and the run ends in no number range — that no recorded film " +
        "record's title, original title or alternative title carries, and that the resolver cuts off some title billing it",
      Map("listingTitles" -> titled.size, "venues" -> titled.map(_._1).distinct.size, "recordTitles" -> records.distinct.size),
      TitleDecorationsLearner.accumulate(earlier, TitleDecorationsLearner.learn(titled, records, perCountry.flatMap(_._3)), records, titled))
  }

  def write(artefact: TitleDecorations.Artefact, out: Path): Unit = {
    Files.createDirectories(out.getParent)
    Files.writeString(out, Json.prettyPrint(Json.toJson(artefact)) + "\n")
  }
}
