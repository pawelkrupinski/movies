package integration

import services.identity._
import services.movies.ListingKey
import tools.{ConvergenceStorage, UnmatchedClusters}

import java.nio.file.{Files, Paths}
import scala.collection.mutable

/**
 * Every take of the recorded full corpora with the evidence [[SuspectTakes]] reads — what `scripts.SuspectTakesMeasure`
 * grades the suspect-take signals on against `labels.tsv`, in seconds, as often as a signal is tuned:
 *
 *   KINOWO_IDENTITY_FULL=pl,uk,de,es,us KINOWO_IDENTITY_CORPUS_DIR=test/resources/fixtures/corpus
 *   scripts/it-local.sh "worker/IntegrationTest/runMain integration.SuspectTakesDump --out target/suspect-takes"
 *
 * Today's takes as [[IdentityUnifiedDataset]] reads them: the unmatched clusters as the agreement stage takes them (the
 * fixture's replay, [[UnmatchedClusters]]), the rest of the corpus as the model decides it. Writes one
 * [[UnmatchedClusters.Capture]] per country (`<out>/<cc>.json.gz`): every listing, today's decisions, the venue pages'
 * facts of every listing, and the record of every film a node scored — the films a label names among them.
 */
object SuspectTakesDump {

  def main(args: Array[String]): Unit = {
    val opts = args.grouped(2).collect { case Array(k, v) => k.stripPrefix("--") -> v }.toMap
    val out  = Paths.get(opts.getOrElse("out", sys.error("--out <dir>")))
    val configuration = settings.ProcessConfiguration.resolve()
    val storages = mutable.ListBuffer.empty[ConvergenceStorage]
    Files.createDirectories(out)
    try new DecorationCorpora(configuration, storages).loaded.foreach { l =>
      val started  = System.nanoTime()
      val c        = l.c
      val scored   = IdentityResolver.resolveScored(l.listings, l.lookups, c.normalizer, IdentityCalibration.resolver)
      val model    = scored.resolution
      val capture  = Some(UnmatchedClusters.fixturePath(c.country)).filter(Files.exists(_)).map(UnmatchedClusters.read)
      val outcome  = capture.map(UnmatchedClusters.replay(_))
      val agreed   = outcome.toSeq.flatMap(_.agreed.decisions)
      val covered  = agreed.flatMap(_.members).toSet
      val decisions = agreed ++ model.decisions.filter(_.basis != ResolverDecision.Basis.Pinned).flatMap { d =>
        val kept = d.members.filterNot(covered)
        Option.when(kept.nonEmpty)(d.copy(members = kept)(d.trace))
      }
      val candidates = scored.evidence.flatMap(_.candidates).map(k => k.tmdbId -> k.film)
      val films = (candidates ++ capture.toSeq.flatMap(_.films.collect { case (id, Some(f)) => id -> f }) ++
        outcome.toSeq.flatMap(_.agreed.films) ++ model.films).toMap
      val taken = decisions.filter(d => d.film.isDefined || d.fallback.isDefined).flatMap(_.members).toSet
      val details = l.listings.filter(x => taken(x.key) && l.lookups.hasDetail(x)).flatMap(x =>
        l.lookups.detail(x).toOption.map(ListingKey.serialised(x.key) -> _))
      val dump = UnmatchedClusters.Capture(c.country, l.listings, decisions, Map.empty, films.view.mapValues(Option(_)).toMap, details.toMap,
        details.map(_._1).toSet, Map.empty, Map.empty)
      UnmatchedClusters.write(out.resolve(s"${c.country.code}.json.gz"), dump)
      println(f"[${c.label}] ${l.listings.size} listings, ${decisions.size} decisions (${agreed.size} from the fixture), ${films.size} films " +
        f"in ${(System.nanoTime() - started) / 1e9}%.0fs")
    } finally storages.foreach(s => scala.util.Try(s.close()))
  }
}
