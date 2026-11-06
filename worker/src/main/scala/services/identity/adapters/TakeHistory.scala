package services.identity
package adapters

import org.bson.{BsonArray, BsonBoolean, BsonDocument, BsonDouble, BsonInt32, BsonInt64, BsonString, BsonValue}
import services.identity.agreement.VoterFamily
import services.movies.ListingKey

import scala.jdk.CollectionConverters._

/**
 * Each take the projection moves, kept in the history ([[HistoryEntry.Take]], by the listing's key) with what decided
 * it: its take-change line's fields ([[TakeChangeLines]]: the film and basis before and after, the stage and rule, the
 * cause, the trigger, the build), the decision's MEMBERS, and the answers its listings' questions are filed under
 * ([[TakeReads]]). What [[TakeExplanation]] answers "why did this listing change film then" from, the history alone.
 */
final class TakeHistory(history: IdentityHistory, reads: Listing => Seq[String], clock: java.time.Clock, build: String) {
  /** `takes`, each the take change's line, the decision now holding the listing and the listings by key. */
  def kept(takes: Seq[(TakeChange, TakeChangeLines.Line, Option[ResolverDecision])], listingOf: ListingKey => Option[Listing]): Unit =
    if (takes.nonEmpty) {
      val at = clock.millis()
      history.append(takes.map { case (take, line, decision) =>
        val members = decision.fold(Seq(take.listing))(_.members)
        val d = TakeHistory.fieldsDoc(line.fields)
          .append("members", new BsonArray(members.map(k => new BsonString(ListingKey.serialised(k))).asJava))
          .append("reads", new BsonArray(members.flatMap(listingOf).flatMap(reads).distinct.sorted.map(new BsonString(_)).asJava))
        HistoryEntry(HistoryEntry.Take, ListingKey.serialised(take.listing), at, None, Some(d), build)
      })
    }
}

object TakeHistory {
  val Discard: TakeHistory = new TakeHistory(IdentityHistory.Discard, _ => Nil, java.time.Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC), "")

  /** What [[TakeReversals]] held at `now` (epoch millis) of each listing's last film change, read back off the take
   *  history (`history`, at boot) over [[TakeReversals.Horizon]]: the latest entry of each listing whose TMDB film moved,
   *  its films, stage, members and when, and whether a deploy set it off (its line's `trigger`, `review` for a pin's) --
   *  exactly what [[TakeReversals.note]] held of it, so a reversal straddling a restart is counted as one within a
   *  process is. A few hundred entries a day a country (DE 609 in the 3 days to 2026-11-06), read once. */
  def reversalsHeld(history: IdentityHistoryReader, now: Long): Seq[(ListingKey, TakeReversals.Held)] =
    reversalsHeld(history.since(Seq(HistoryEntry.Take), now - TakeReversals.Horizon.toMillis))

  private def reversalsHeld(entries: Seq[HistoryEntry]): Seq[(ListingKey, TakeReversals.Held)] = {
    def tmdb(d: BsonDocument, field: String): Option[Int] = Option(d.get(field)).filter(_.isNumber).map(_.asNumber.intValue)
    def text(d: BsonDocument, field: String): Option[String] = Option(d.get(field)).filter(_.isString).map(_.asString.getValue)
    val moved = entries.iterator.filter(_.kind == HistoryEntry.Take).flatMap(e => e.after.map(e -> _))
      .filter { case (_, d) => tmdb(d, "tmdb_before") != tmdb(d, "tmdb_after") }
    moved.toSeq.groupBy(_._1.id).toSeq.flatMap { case (id, changes) =>
      val (entry, d) = changes.sortBy(_._1.at).last   // stable: of two at one millisecond, the one written last
      ListingKey.parse(id).map { listing =>
        val stage   = text(d, "stage")
        // the decision's listings as TakeReversals hashes them: none when no decision held the listing (no stage)
        val members = if (stage.isEmpty) Set.empty[ListingKey] else Option(d.get("members")).filter(_.isArray).toSeq
          .flatMap(_.asArray.getValues.asScala).filter(_.isString).flatMap(v => ListingKey.parse(v.asString.getValue)).toSet
        listing -> TakeReversals.Held(tmdb(d, "tmdb_before"), tmdb(d, "tmdb_after"), stage, text(d, "trigger").contains("deploy"),
          members.hashCode, entry.at, 0L)
      }
    }
  }

  /** A line's fields as BSON: each a string, a number or a flag; a `None` field left out, as the log line leaves it. */
  def fieldsDoc(fields: Seq[(String, Any)]): BsonDocument = {
    val d = new BsonDocument()
    def value(v: Any): Option[BsonValue] = v match {
      case None | null    => None
      case Some(inner)    => value(inner)
      case s: String      => Some(new BsonString(s))
      case i: Int         => Some(new BsonInt32(i))
      case l: Long        => Some(new BsonInt64(l))
      case x: Double      => Some(new BsonDouble(x))
      case b: Boolean     => Some(BsonBoolean.valueOf(b))
      case other          => Some(new BsonString(other.toString))
    }
    fields.foreach { case (name, v) => value(v).foreach(d.append(name, _)) }
    d
  }
}

/** The answers a listing's own questions are filed under, each as `<history kind> <document id>`: its TMDB and IMDb
 *  searches (`tmdb_queries`) and the families' searches of its title (`identity_family_answers`) — what a take of it
 *  read, besides the films those name, which the explanation reads off the searches' answers. */
object TakeReads {
  def of(language: String)(listing: Listing): Seq[String] =
    (CandidateQueries.of(Evidence.of(listing, None)).map(q => s"${HistoryEntry.answer(TmdbKind.Query)} ${TmdbStore.questionId(language, q)}") ++
      VoterFamily.values.toSeq.map(f => s"${HistoryEntry.answer(TmdbKind.Family)} ${FamilyAnswerStore.titleId(f, listing.title)}")).distinct
  /** A read as [[of]] names it: its history kind and its document's id. */
  def parse(read: String): (String, String) = read.indexOf(' ') match {
    case -1 => (read, "")
    case i  => (read.substring(0, i), read.substring(i + 1))
  }
}
