package integration

import models.Country
import services.identity._
import services.identity.IdentityMeasures.{Category, Measure, Missing}
import services.identity.adapters.TmdbIdentityLookups
import tools._

import java.nio.file.Files
import scala.collection.mutable

/**
 * Experiment `weighted-guards` (docs/identity-experiments.md): every candidate of every node of a recorded full corpus, as
 * the shipped model scores it — its log-odds and probability, the film its node's cluster takes, and the levels of the two
 * guard signals the per-candidate model now measures (`release.country`, `certificate`) — written to
 * `<out>/units-<cc>.tsv` for the confidence study (`scripts/ml-identity/weighted_guards.py`). The corpus and lookups are
 * [[IdentityResolveDump]]'s:
 *
 * {{{
 *   scripts/it-local.sh "worker/IntegrationTest/runMain integration.IdentityGuardUnits --out target/weighted-guards --full uk \
 *     --corpus-dir <pair>/corpus --fixtures <pair>"
 * }}}
 */
object IdentityGuardUnits {
  import IdentityShadow._

  val commandLine: CommandLine = CommandLine("IdentityGuardUnits", "Writes every node's scored candidates over recorded full corpora.",
    CommandLine.Value("out", "dir", "where units-<cc>.tsv is written (required)"),
    CommandLine.Value("full", "codes", "the countries resolved, comma-separated (required)"),
    CommandLine.Value("corpus-dir", "dir", "the directory holding cinema-scrapes-<cc>.json.gz (required)"),
    CommandLine.Value("fixtures", "dir", "the directory holding enrichment-<cc>/"))

  private def level(m: Option[Measure]): String = m match {
    case Some(Category(v)) => v
    case Some(Missing(s))  => s"missing:$s"
    case _                 => "-"
  }

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val countries = arguments.required("full").split(",").toSeq.map(_.trim).filter(_.nonEmpty).map(code =>
      Country.byCode(code).getOrElse(arguments.fail(s"--full: no country $code")))
    val out       = arguments.requiredPath("out")
    val configuration = settings.ProcessConfiguration.resolve()
    val root      = arguments.path("fixtures").map(settings.FixtureRoot(_)).getOrElse(configuration.fixtureRoot)
    val target    = IntegrationMongoTarget.from(configuration).getOrElse(arguments.fail("MONGODB_URI names no throwaway Mongo"))
    target.requireThrowaway()
    val storages  = mutable.ListBuffer.empty[ConvergenceStorage]
    try IdentityShadow.full(countries.toSet, arguments.requiredPath("corpus-dir"), root, configuration.identityLiveGaps).foreach { c =>
      val w        = wiring(target, c, storages, root, configuration.env)
      val listings = listingsOf(w, c.normalizer)
      val lookups  = new Memo(new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)),
        language = c.country.language, retrySleep = (_: Long) => ()), new services.enrichment.ImdbClient(c.fetch), w.detailEnrichers, c.gaps))
      val calibration = IdentityCalibration.resolver
      val scored   = IdentityResolver.resolveScored(listings, lookups, c.normalizer, calibration)
      val byKey    = listings.map(l => l.key -> l).toMap
      val lines = scored.evidence.flatMap { node =>
        val first = node.keys.min(using Ordering.by(services.movies.ListingKey.serialised))
        val take  = node.keys.iterator.flatMap(k => scored.resolution.decisionOf(k).film).nextOption()
        val certificates = node.keys.flatMap(byKey.get).flatMap(_.certificate).distinct.mkString("/")
        node.candidates.map { cand =>
          Seq(c.country.code, first.venue, first.rawTitle.replace('\t', ' '), node.keys.size.toString, services.movies.ListingKey.serialised(first).hashCode.toString,
            cand.tmdbId.toString, take.fold("")(_.toString), cand.denied.toString,
            f"${calibration.logOdds(IdentityMeasures.ListingFilm, cand.measures)}%.5f", f"${cand.probability}%.6f",
            level(cand.measures.get("release.country")), level(cand.measures.get("certificate")), certificates,
            cand.film.certificates.getOrElse("")).mkString("\t")
        }
      }
      Files.createDirectories(out)
      val header = "country\tvenue\trawTitle\tlistings\tnode\ttmdbId\ttake\tdenied\tlogit\tprobability\trelease\tcertificate\tvenueCertificate\tfilmCertificate"
      Files.writeString(out.resolve(s"units-${c.country.code}.tsv"), (header +: lines).mkString("", "\n", "\n"))
      println(s"[${c.label}] ${lines.size} candidate rows over ${scored.evidence.size} nodes")
    } finally storages.foreach(s => scala.util.Try(s.close()))
  }
}
