package integration

import models.Country
import services.identity._
import services.identity.adapters.TmdbIdentityLookups
import services.movies.ListingKey
import tools._

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.Try

/**
 * Does a recording decide the same read LEAN, as prod's identity model reads it ([[ModelRead]]), as read WHOLE? The
 * guard against a rule that measures cured on a recording and is dead in prod because it reads what the lean read drops
 * (every screening day of a film billing no stage work): four were, 10-15 to 10-21 (17da3656b), invisible to measurement.
 *
 * {{{
 *   scripts/identity-ci-run --recording 528 --countries pl,de -- \
 *     'worker/IntegrationTest/runMain integration.LeanWholeGuard --out {out} --full {cc} --corpus-dir {corpus} --fixtures {fixtures}'
 * }}}
 *
 * The resolver runs over both reads of each country's recording, one memo of the recorded answers between them, and
 * writes `lean-whole-<cc>.tsv` (each listing whose take or rules differ: its film each way, the rules firing on whole
 * data only and on lean only) and `lean-whole-<cc>.txt` (the summary: per rule, on how many listings it fires on whole
 * data only — dead in prod — and how many of those change their take). Measurements read lean; this alone reads whole.
 *
 * Exit: 0 written (a difference is a finding, not a failure — `--strict` makes a differing take exit 1); 1 a corpus
 * missing or failed; 2 usage.
 */
object LeanWholeGuard {

  import IdentityShadow._

  val commandLine: CommandLine = CommandLine("LeanWholeGuard", "Names the rules a recording fires read whole but not lean, as prod reads it.",
    CommandLine.Value("out", "dir", "where lean-whole-<cc>.tsv and .txt are written (required)"),
    CommandLine.Value("full", "codes", "the countries whose recorded corpora are compared, comma-separated (required)"),
    CommandLine.Value("corpus-dir", "dir", "the directory holding cinema-scrapes-<cc>.json.gz (required)"),
    CommandLine.Value("fixtures", "dir", "the directory holding enrichment-<cc>/ (default: KINOWO_FIXTURE_ROOT, else the repository's)"),
    CommandLine.Flag("strict", "exit 1 when a listing's take differs between the reads"))

  /** One listing's decision as the guard compares it: its film, and the rules that fired on it or refused a film. */
  final case class Read(film: Option[Int], rules: Set[String])

  /** A listing whose two reads differ. */
  final case class Difference(key: ListingKey, lean: Read, whole: Read) {
    def takeMoved: Boolean = lean.film != whole.film
    def wholeOnly: Set[String] = whole.rules -- lean.rules
    def leanOnly: Set[String] = lean.rules -- whole.rules
  }

  /** The decisions of `resolution`, by listing. */
  def reads(resolution: Resolution, listings: Seq[Listing]): Map[ListingKey, Read] = listings.map { l =>
    val d = resolution.decisionOf(l.key)
    val refused = d.trace.nodes.get(l.key).toSeq.flatMap(_.refusals).map(r => s"refused:${r.rule}")
    l.key -> Read(d.film, (d.trace.rulesOf(l.key) ++ refused).toSet)
  }.toMap

  /** Every listing whose two reads differ, in key order. */
  def differences(lean: Map[ListingKey, Read], whole: Map[ListingKey, Read]): Seq[Difference] =
    (lean.keySet ++ whole.keySet).toSeq.sortBy(ListingKey.serialised).flatMap { key =>
      val (l, w) = (lean.getOrElse(key, Read(None, Set.empty)), whole.getOrElse(key, Read(None, Set.empty)))
      Option.when(l != w)(Difference(key, l, w))
    }

  /** The summary: per rule, the listings it fires on whole data only (and of those, whose take moves), most first. */
  def summary(label: String, listings: Int, found: Seq[Difference]): Seq[String] = {
    def tally(of: Difference => Set[String]) = found.flatMap(d => of(d).map(_ -> d)).groupMap(_._1)(_._2).toSeq
      .map { case (rule, ds) => (rule, ds.size, ds.count(_.takeMoved)) }.sortBy { case (rule, n, moved) => (-moved, -n, rule) }
    val moved = found.filter(_.takeMoved)
    Seq(s"[$label] $listings listings: ${moved.size} take(s) differ lean vs whole, ${found.size - moved.size} more differ in rules alone") ++
      (if (moved.isEmpty) Nil else "takes differing (lean → whole):" +: moved.take(40).map(d =>
        s"  ${d.key.venue}: '${d.key.rawTitle}' ${d.lean.film.getOrElse("-")} → ${d.whole.film.getOrElse("-")}  whole only: ${d.wholeOnly.toSeq.sorted.mkString(", ")}")) ++
      (tally(_.wholeOnly) match {
        case Nil  => Seq("no rule fires on whole data only")
        case some => "rules firing on WHOLE data only (dead in prod): rule, listings, of them take moved" +: some.map { case (r, n, m) => s"  $r\t$n\t$m" }
      }) ++
      (tally(_.leanOnly) match {
        case Nil  => Nil
        case some => "rules firing on LEAN data only: rule, listings, of them take moved" +: some.map { case (r, n, m) => s"  $r\t$n\t$m" }
      })
  }

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val countries = arguments.required("full").split(",").toSeq.map(_.trim).filter(_.nonEmpty).map(code =>
      Country.byCode(code).getOrElse(arguments.fail(s"--full: no country $code (one of ${Country.all.map(_.code).mkString(", ")})")))
    val out       = arguments.requiredPath("out")
    val corpusDir = arguments.requiredPath("corpus-dir")
    val configuration = settings.ProcessConfiguration.resolve()
    val root      = arguments.path("fixtures").map(settings.FixtureRoot(_)).getOrElse(configuration.fixtureRoot)
    val target    = IntegrationMongoTarget.from(configuration).getOrElse(arguments.fail("MONGODB_URI names no throwaway Mongo to seed the corpora into"))
    target.requireThrowaway()
    val corpora   = IdentityShadow.full(countries.toSet, corpusDir, root, configuration.identityLiveGaps)
    val missing   = countries.filterNot(c => corpora.exists(_.country == c))
    missing.foreach(c => System.err.println(s"[LeanWholeGuard] no cinema-scrapes-${c.code}.json.gz in $corpusDir"))
    val storages  = mutable.ListBuffer.empty[ConvergenceStorage]
    var moved     = 0
    val failed    = try corpora.filterNot { c =>
      Try(moved += compare(configuration, target, root, c, out, storages)).fold({ e =>
        System.err.println(s"[${c.label}] FAILED: $e"); e.printStackTrace(); false }, _ => true)
    } finally storages.foreach(s => Try(s.close()))
    if (missing.nonEmpty || failed.nonEmpty) sys.exit(1)
    if (arguments.flag("strict") && moved > 0) sys.exit(1)
  }

  /** `c` resolved read lean and read whole, the differences written; how many takes differ. */
  private def compare(configuration: settings.ProcessConfiguration, target: IntegrationMongoTarget, root: settings.FixtureRoot, c: Corpus, dir: Path,
                      storages: mutable.ListBuffer[ConvergenceStorage]): Int = {
    val w       = wiring(target, c, storages, root, configuration.env)
    val lookups = new Memo(new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)),
      language = c.country.language), new services.enrichment.ImdbClient(c.fetch), w.detailEnrichers, c.gaps))
    def resolved(listings: Seq[Listing]) = reads(IdentityResolver.resolve(listings, lookups, c.normalizer, IdentityCalibration.resolver), listings)
    val lean  = ModelRead.corpus(w.archivedListings, c.normalizer)
    val found = differences(resolved(lean), resolved(ModelRead.whole(w.archivedListings, c.normalizer)))
    Files.createDirectories(dir)
    def cell(s: String) = s.replace('\t', ' ').replace('\n', ' ')
    Files.writeString(dir.resolve(s"lean-whole-${c.country.code}.tsv"), (
      "key\tvenue\trawTitle\tleanFilm\twholeFilm\twholeOnly\tleanOnly" +: found.map(d => Seq(ListingKey.serialised(d.key), d.key.venue, d.key.rawTitle,
        d.lean.film.fold("")(_.toString), d.whole.film.fold("")(_.toString), d.wholeOnly.toSeq.sorted.mkString(","), d.leanOnly.toSeq.sorted.mkString(","))
        .map(cell).mkString("\t"))).mkString("", "\n", "\n"))
    val lines = summary(c.label, lean.size, found)
    Files.writeString(dir.resolve(s"lean-whole-${c.country.code}.txt"), lines.mkString("", "\n", "\n"))
    lines.foreach(println)
    found.count(_.takeMoved)
  }
}
