package services.identity
package adapters

import org.bson.BsonDocument

import scala.jdk.CollectionConverters._

/**
 * Why a listing changed film at a moment, from the identity history alone — nothing resolved again
 * (`scripts/identity-why`): its take there ([[HistoryEntry.Take]], the last one at or before the moment, else the first
 * after it) beside the take before it, and every input that moved between the two — the DECIDING INPUTS: a listing of the
 * decision that joined or left it, a listing of it re-published otherwise, an answer one of its listings' questions or
 * one of the films they name are filed under that changed, was deleted or failed to come ([[HistoryEntry.Failure]]).
 */
object TakeExplanation {
  final case class Explained(listing: String, take: HistoryEntry, previous: Option[HistoryEntry], joined: Seq[String], left: Seq[String],
                             inputs: Seq[HistoryEntry]) {
    def render: Seq[String] = {
      val now = take.after.getOrElse(new BsonDocument())
      def field(d: BsonDocument, name: String) = Option(d.get(name)).map(shown)
      def f(name: String) = field(now, name).getOrElse("-")
      Seq(s"${named(listing)}: tmdb ${f("tmdb_before")} -> tmdb ${f("tmdb_after")} at ${java.time.Instant.ofEpochMilli(take.at)} (build ${take.build.take(9)})",
        s"  basis ${f("previous_basis")} -> ${f("basis")}; stage ${f("stage")}, decided by ${f("decided_by")}; cause ${f("cause")}; trigger ${f("trigger")}",
        s"  reason ${f("reason_first")}",
        previous.fold("  the take before: none kept (the first moved in the history)")(p =>
          s"  the take before: tmdb ${p.after.flatMap(field(_, "tmdb_before")).getOrElse("-")} -> tmdb ${p.after.flatMap(field(_, "tmdb_after")).getOrElse("-")} " +
            s"at ${java.time.Instant.ofEpochMilli(p.at)}"),
        s"  members ${membersOf(take).size}" + (if (joined.nonEmpty) s"; joined: ${joined.map(named).mkString(", ")}" else "") +
          (if (left.nonEmpty) s"; left: ${left.map(named).mkString(", ")}" else "")) ++
        (if (inputs.isEmpty) Seq("  no input it read moved since the take before: the code decided otherwise")
         else s"  deciding inputs since the take before (${inputs.size}):" +: inputs.map(describe))
    }
  }

  /** The take of `listing` (a serialised listing key) at `at`, and what moved since the one before it — None while the
   *  history keeps no take of it. */
  def explain(reader: IdentityHistoryReader, listing: String, at: Long): Option[Explained] = {
    val takes = reader.of(HistoryEntry.Take, Seq(listing)).sortBy(_.at)
    takes.filter(_.at <= at).lastOption.orElse(takes.find(_.at > at)).map { take =>
      val previous = takes.filter(_.at < take.at).lastOption
      val now      = membersOf(take)
      val before   = previous.fold(now)(membersOf)
      val films    = (Seq(take) ++ previous).flatMap(_.after).flatMap(d => Seq("tmdb_before", "tmdb_after", "runner_up_tmdb", "previous_runner_up_tmdb")
        .flatMap(name => Option(d.get(name)).map(shown))).distinct
      val reads    = (Seq(take) ++ previous).flatMap(readsOf).distinct
      val from     = previous.fold(take.at - Window)(_.at)
      val answers  = reads.groupMap(_._1)(_._2).toSeq.flatMap { case (kind, ids) => reader.of(kind, ids) } ++
        reader.of(HistoryEntry.answer(TmdbKind.Film), films)
      // the films the questions' answers named, in the window: a candidate's record moving is a deciding input too
      val named    = answers.flatMap(e => e.before.toSeq ++ e.after).flatMap(d => Option(d.get("ids")).toSeq)
        .flatMap(_.asArray.getValues.asScala.map(_.asNumber.intValue.toString)).distinct.filterNot(films.contains)
      val listings = reader.of(HistoryEntry.Listing, (now ++ before).distinct)
      val titles   = (now ++ before).flatMap(k => services.movies.ListingKey.parse(k).map(_.rawTitle)).distinct
      val failures = reader.since(Seq(HistoryEntry.Failure), from).filter(e => (films ++ named).exists(id => e.id.contains(s"/movie/$id")) ||
        titles.exists(t => e.id.contains(java.net.URLEncoder.encode(t, "UTF-8"))))
      val inputs   = (answers ++ reader.of(HistoryEntry.answer(TmdbKind.Film), named) ++ listings ++ failures)
        .filter(e => e.at > from && e.at <= take.at).distinctBy(e => (e.kind, e.id, e.at)).sortBy(_.at)
      Explained(listing, take, previous, now.diff(before), before.diff(now), inputs)
    }
  }

  /** How far back a take with none before it is explained from. */
  val Window: Long = 24L * 3600 * 1000

  /** A value as a line shows it: a string as written, a number as a number. */
  private def shown(v: org.bson.BsonValue): String =
    if (v.isString) v.asString.getValue else if (v.isNumber) v.asNumber match {
      case d: org.bson.BsonDouble => d.getValue.toString
      case n                      => n.longValue.toString
    } else v.toString
  /** A serialised listing key as `venue | billed title`. */
  def named(key: String): String = services.movies.ListingKey.parse(key).fold(key)(k => s"${k.venue} | ${k.rawTitle}")

  private def membersOf(take: HistoryEntry): Seq[String] =
    take.after.flatMap(d => Option(d.get("members"))).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asString.getValue))
  private def readsOf(take: HistoryEntry): Seq[(String, String)] =
    take.after.flatMap(d => Option(d.get("reads"))).toSeq.flatMap(_.asArray.getValues.asScala.map(v => TakeReads.parse(v.asString.getValue)))

  /** One input's move, in a line: what it was and became — the fields that moved, an answer's value that left or came. */
  def describe(e: HistoryEntry): String = {
    val when = java.time.Instant.ofEpochMilli(e.at)
    val what = (e.before, e.after, e.failure) match {
      case (_, _, Some(failure)) => s"failed: $failure"
      case (None, Some(_), _)    => if (e.kind == HistoryEntry.Listing) "taken in (new)" else "came (none before)"
      case (Some(_), None, _)    => if (e.kind == HistoryEntry.Listing) "let go" else "deleted"
      case (Some(b), Some(a), _) => s"changed: ${moved(b, a).mkString(", ")}"
      case (None, None, _)       => "?"
    }
    s"    $when ${e.kind} ${if (e.kind == HistoryEntry.Listing || e.kind == HistoryEntry.Take) named(e.id) else e.id}: $what"
  }

  /** The fields of `b` and `a` that differ, each `name: before -> after` when short. */
  private def moved(b: BsonDocument, a: BsonDocument): Seq[String] =
    (b.keySet.asScala ++ a.keySet.asScala).toSeq.sorted.filter(k => b.get(k) != a.get(k)).map { k =>
      def short(v: org.bson.BsonValue) = Option(v).fold("none")(x => { val s = shown(x); if (s.length > 60) s.take(57) + "..." else s })
      s"$k: ${short(b.get(k))} -> ${short(a.get(k))}"
    }
}
