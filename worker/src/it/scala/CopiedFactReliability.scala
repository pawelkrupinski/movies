package integration

import models.Country
import play.api.libs.json.{JsArray, JsNull, JsNumber, JsObject, JsString, JsValue, Json}
import services.identity._
import services.identity.adapters.TmdbIdentityLookups
import tools._

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.Try

/**
 * How often each feed's copied facts point at the right film: every listing whose year, directors, runtime or original
 * title a feed copied ([[SourceReliability.factsOf]] other than the venue), and every judged listing, with its copied facts
 * read against the film the resolver takes and every film `identity-judgements.tsv` judges for it — each fact's measure
 * and its calibrated contribution. `scripts/experiments/copied-fact-reliability.py` counts them per feed.
 *
 * {{{
 *   scripts/identity-ci-run --recording 528 --countries pl,de -- \
 *     'worker/IntegrationTest/runMain integration.CopiedFactReliability --out {out} --full {cc} --corpus-dir {corpus} --fixtures {fixtures}'
 * }}}
 */
object CopiedFactReliability {

  import IdentityShadow._

  val commandLine: CommandLine = CommandLine("CopiedFactReliability", "Writes each feed-sourced listing's copied facts against its films.",
    CommandLine.Value("out", "dir", "where copied-<cc>.jsonl is written (required)"),
    CommandLine.Value("full", "codes", "the countries resolved, comma-separated (required)"),
    CommandLine.Value("corpus-dir", "dir", "the directory holding cinema-scrapes-<cc>.json.gz (required)"),
    CommandLine.Value("fixtures", "dir", "the directory holding enrichment-<cc>/ (default: KINOWO_FIXTURE_ROOT, else the repository's)"),
    CommandLine.Value("judgements", "file", "identity-judgements.tsv (default test/resources/fixtures/identity-judgements.tsv)"))

  private final case class Judged(country: String, venue: String, rawTitle: String, film: Int, verdict: String, evidence: String)

  private def judgements(path: Path): Seq[Judged] =
    scala.io.Source.fromFile(path.toFile, "UTF-8").getLines().filterNot(l => l.startsWith("#") || l.startsWith("country\t")).flatMap { line =>
      val f = line.split("\t", -1)
      if (f.length >= 11 && f(3).startsWith("tmdb:")) f(3).stripPrefix("tmdb:").toIntOption.map(Judged(f(0), f(1), f(2), _, f(4), f(10)))
      else None
    }.toSeq

  private val Signals = Seq("year.delta", "director", "runtime.delta", "originalTitle")

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val countries = arguments.required("full").split(",").toSeq.map(_.trim).filter(_.nonEmpty).map(code =>
      Country.byCode(code).getOrElse(arguments.fail(s"--full: no country $code")))
    val out       = arguments.requiredPath("out")
    val corpusDir = arguments.requiredPath("corpus-dir")
    val judged    = judgements(arguments.path("judgements").getOrElse(Path.of("test/resources/fixtures/identity-judgements.tsv")))
    val configuration = settings.ProcessConfiguration.resolve()
    val root      = arguments.path("fixtures").map(settings.FixtureRoot(_)).getOrElse(configuration.fixtureRoot)
    val target    = IntegrationMongoTarget.from(configuration).getOrElse(arguments.fail("MONGODB_URI names no throwaway Mongo"))
    target.requireThrowaway()
    val corpora   = IdentityShadow.full(countries.toSet, corpusDir, root, configuration.identityLiveGaps)
    val storages  = mutable.ListBuffer.empty[ConvergenceStorage]
    val failed    = try corpora.filterNot { c =>
      Try(dump(configuration, target, root, c, out, storages, judged.filter(_.country == c.country.code))).fold({ e =>
        System.err.println(s"[${c.label}] FAILED: $e"); e.printStackTrace(); false }, _ => true)
    } finally storages.foreach(s => Try(s.close()))
    if (failed.nonEmpty || corpora.size != countries.size) sys.exit(1)
  }

  private def dump(configuration: settings.ProcessConfiguration, target: IntegrationMongoTarget, root: settings.FixtureRoot, c: Corpus, dir: Path,
                   storages: mutable.ListBuffer[ConvergenceStorage], judged: Seq[Judged]): Unit = {
    val w        = wiring(target, c, storages, root, configuration.env)
    val listings = listingsOf(w, c.normalizer)
    val lookups  = new Memo(new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)),
      language = c.country.language, retrySleep = (_: Long) => ()), new services.enrichment.ImdbClient(c.fetch), w.detailEnrichers,
      c.gaps))
    val resolution = IdentityResolver.resolve(listings, lookups, c.normalizer, IdentityCalibration.resolver)
    val byTitle    = judged.groupBy(_.rawTitle)
    val calibration = IdentityCalibration.resolver
    def opt[A](a: Option[A])(f: A => JsValue): JsValue = a.fold[JsValue](JsNull)(f)
    def against(evidence: Evidence, id: Int, verdict: Option[Judged]): Option[JsObject] =
      lookups.film(id).toOption.flatten.map { film =>
        val measures = IdentityMeasures.listingFilm(evidence.measured, film, None, 0, 0, country = Some(c.country.code.toUpperCase))
        val weights  = calibration.contributions(IdentityMeasures.ListingFilm, measures).toMap
        JsObject(Seq(
          "id" -> JsNumber(id), "title" -> JsString(film.title), "year" -> opt(film.year)(JsNumber(_)),
          "runtime" -> opt(film.runtime)(JsNumber(_)), "directors" -> JsArray(film.directors.getOrElse(Nil).map(JsString(_))),
          "verdict" -> opt(verdict)(v => JsString(v.verdict)), "evidence" -> opt(verdict)(v => JsString(v.evidence.take(40))),
          "measures" -> JsObject(Signals.flatMap(s => measures.get(s).map(m => s -> JsString(m.toString)))),
          "weights" -> JsObject(Signals.flatMap(s => weights.get(s).map(x => s -> JsNumber(BigDecimal(x).setScale(3, BigDecimal.RoundingMode.HALF_UP)))))))
      }
    Files.createDirectories(dir)
    // every judged listing's node, its candidates as the resolve scores them: the full log-odds beside the copied facts' share
    val judgedKeys = listings.filter(l => byTitle.getOrElse(l.key.rawTitle, Nil).exists(j => j.venue == "*" || j.venue == l.key.venue)).map(_.key).toSet
    val byKey      = listings.map(l => l.key -> l).toMap
    val nodes      = IdentityResolver.evidenceOf(listings, lookups, c.normalizer, calibration)(l => judgedKeys(l.key))
    val scored = nodes.flatMap { node =>
      val members = node.keys.flatMap(byKey.get)
      val sources = members.map(l => SourceReliability.factsOf(l.catalogueIds, l.page, l.pageFacts || lookups.detail(l).toOption.flatten.isDefined).name).distinct
      val verdicts = members.flatMap(l => byTitle.getOrElse(l.key.rawTitle, Nil).filter(j => j.venue == "*" || j.venue == l.key.venue).map(l.key -> _))
      val takenBy  = members.map(l => resolution.decisionOf(l.key).film).distinct
      val keep     = verdicts.map(_._2.film).toSet ++ takenBy.flatten
      node.candidates.filter(cand => keep(cand.tmdbId)).map { cand =>
        val weights = calibration.contributions(IdentityMeasures.ListingFilm, cand.measures).toMap
        Json.stringify(JsObject(Seq(
          "keys" -> JsArray(members.map(l => JsString(s"${l.key.venue}|${l.key.rawTitle}"))), "sources" -> JsArray(sources.map(JsString(_))),
          "id" -> JsNumber(cand.tmdbId), "title" -> JsString(cand.film.title), "year" -> opt(cand.film.year)(JsNumber(_)),
          "verdicts" -> JsArray(verdicts.filter(_._2.film == cand.tmdbId).map(_._2).distinct.map(v => JsString(v.verdict + "\t" + v.evidence.take(20)))),
          "taken" -> JsNumber(takenBy.count(_.contains(cand.tmdbId))), "denied" -> JsString(cand.denied.toString),
          "probability" -> JsNumber(BigDecimal(cand.probability).setScale(4, BigDecimal.RoundingMode.HALF_UP)),
          "logOdds" -> JsNumber(BigDecimal(calibration.logOdds(IdentityMeasures.ListingFilm, cand.measures)).setScale(3, BigDecimal.RoundingMode.HALF_UP)),
          "weights" -> JsObject(Signals.flatMap(s => weights.get(s).map(x => s -> JsNumber(BigDecimal(x).setScale(3, BigDecimal.RoundingMode.HALF_UP))))))))
      }
    }
    Files.writeString(dir.resolve(s"scored-${c.country.code}.jsonl"), scored.mkString("", "\n", "\n"))
    val lines = listings.flatMap { l =>
      val detail   = lookups.detail(l).toOption.flatten
      val source   = SourceReliability.factsOf(l.catalogueIds, l.page, l.pageFacts || detail.isDefined)
      val verdicts = byTitle.getOrElse(l.key.rawTitle, Nil).filter(j => j.venue == "*" || j.venue == l.key.venue)
      if ((source eq SourceReliability.Venue) && verdicts.isEmpty) None
      else {
        val evidence = Evidence.of(l, detail)
        val taken    = resolution.decisionOf(l.key).film
        val films    = (taken.toSeq ++ verdicts.map(_.film)).distinct
        Some(Json.stringify(JsObject(Seq(
          "venue" -> JsString(l.key.venue), "rawTitle" -> JsString(l.key.rawTitle), "source" -> JsString(source.name),
          "claim" -> JsString(SourceReliability.claimOf(l)),
          "copied" -> JsArray(SourceReliability.Fact.values.toSeq.filter(f => source.trust(f) == SourceReliability.Trust.Support).map(f => JsString(f.toString))),
          "own" -> JsObject(Seq("year" -> opt(l.year)(JsNumber(_)), "runtime" -> opt(l.runtime)(JsNumber(_)), "directors" -> JsArray(l.directors.map(JsString(_))))),
          "detail" -> JsString(detail.fold("")(_ => "yes")),
          "year" -> opt(evidence.year)(JsNumber(_)), "runtime" -> opt(evidence.runtime)(JsNumber(_)),
          "directors" -> JsArray(evidence.directors.map(JsString(_))), "originalTitle" -> opt(evidence.originalTitle)(JsString(_)),
          "taken" -> opt(taken)(JsNumber(_)),
          "films" -> JsArray(films.flatMap(id => against(evidence, id, verdicts.find(_.film == id))))))))
      }
    }
    Files.writeString(dir.resolve(s"copied-${c.country.code}.jsonl"), lines.mkString("", "\n", "\n"))
    println(s"[${c.label}] ${lines.size} feed-sourced or judged listings of ${listings.size}")
  }
}
