package scripts

import services.identity._
import services.movies.{ListingKey, TitleNormalizer}
import tools.UnmatchedClusters

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/**
 * The resolver's and the agreement stage's every DECISION over the five checked-in unmatched-cluster captures
 * ([[UnmatchedClusters]]), written one line per cluster — the equivalence proof of a refactor of the rules: run it
 * before and after, and `diff -r` the two directories. Two replays per country:
 *
 *  - `agreed`: the agreement stage over the captured model decisions — what the ratchet grades;
 *  - `resolved`: the resolver over the capture's listings (its answers replayed, a question it never held `Unknown`),
 *    the agreement stage over that — every acceptance rule, denial, veto and join the resolver fires on them.
 *
 * Each line holds the cluster's members, film, basis, fallback, agreeing families, TMDB's lean and best candidate, the
 * explanation in full and every rule the trace names (`accept:`, `pooled:`, `veto:`, `join:`, `apart:`, `refused:`) with
 * the denials on its candidates. `allocation.tsv` holds each replay's bytes on this thread and its wall time.
 *
 * `sbt "worker/Test/runMain scripts.IdentityEquivalence <out dir>"`.
 */
object IdentityEquivalence {

  def main(args: Array[String]): Unit = {
    val out = Path.of(args.headOption.getOrElse(sys.error("usage: IdentityEquivalence <out dir>")))
    Files.createDirectories(out)
    val captures = models.Country.all.map(UnmatchedClusters.fixturePath).filter(Files.exists(_)).map(UnmatchedClusters.read)
    val costs = captures.flatMap { capture =>
      val cc = capture.country.code
      val normalizer = TitleNormalizer.forCountry(capture.country)
      val ((agreed, _), agreedBytes, agreedSeconds) = measured(UnmatchedClusters.replay(capture) -> ())
      write(out.resolve(s"agreed-$cc.tsv"), lines(agreed.agreed.decisions))
      val (resolution, resolveBytes, resolveSeconds) =
        measured(IdentityResolver.resolve(capture.listings, new UnmatchedClusters.Replay(capture), normalizer))
      val (reagreed, reagreeBytes, reagreeSeconds) = measured(UnmatchedClusters.replay(capture, resolution.decisions))
      write(out.resolve(s"resolved-$cc.tsv"), lines(resolution.decisions))
      write(out.resolve(s"resolved-agreed-$cc.tsv"), lines(reagreed.agreed.decisions))
      println(f"[$cc] agreed ${agreedBytes / 1e6}%.1f MB ${agreedSeconds}%.1fs; resolve ${resolveBytes / 1e6}%.1f MB ${resolveSeconds}%.1fs; " +
        f"re-agree ${reagreeBytes / 1e6}%.1f MB ${reagreeSeconds}%.1fs")
      Seq(s"$cc\tagreed\t$agreedBytes\t$agreedSeconds", s"$cc\tresolve\t$resolveBytes\t$resolveSeconds", s"$cc\tre-agree\t$reagreeBytes\t$reagreeSeconds")
    }
    write(out.resolve("allocation.tsv"), costs)
  }

  private def measured[A](block: => A): (A, Long, Double) = {
    val watch = tools.Stopwatch.start()
    val (value, bytes) = tools.ThreadAllocation.of(block)
    (value, bytes, watch.seconds)
  }

  private def write(path: Path, lines: Seq[String]): Unit = Files.writeString(path, lines.mkString("", "\n", "\n"), StandardCharsets.UTF_8)

  /** One line per decision, sorted by its first member: every field an outcome is made of, and what the trace fired. */
  def lines(decisions: Seq[ResolverDecision]): Seq[String] = decisions.map { d =>
    val members = d.members.map(ListingKey.serialised).sorted
    val rules   = d.members.flatMap(d.trace.rulesOf).distinct.sorted
    val denials = d.members.flatMap(key => d.trace.nodes.get(key).toSeq.flatMap(_.candidates)).filter(_.contains("DENIED")).distinct.sorted
    Seq(members.mkString(" | "), d.film.fold("-")(_.toString), d.basis.toString,
      d.fallback.fold("-")(f => s"${f.source}:${f.id}"), d.agreed.toSeq.sorted.map { case (k, v) => s"$k=$v" }.mkString(","),
      d.leaning.fold("-")(_.film.toString), d.candidate.fold("-")(_.film.toString), f"${d.confidence}%.4f", d.unanswered.toString,
      d.explanation.mkString(" ⟂ "), d.contradictions.mkString(" ⟂ "), rules.mkString(" "), denials.mkString(" ⟂ ")).mkString("\t")
  }.sorted
}
