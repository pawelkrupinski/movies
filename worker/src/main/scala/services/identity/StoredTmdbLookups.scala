package services.identity

import org.bson.BsonDocument
import services.enrichment.ImdbClient

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/**
 * The identity model's TMDB and IMDb questions answered from [[TmdbStore]]'s normalized documents
 * — a title search as its films, a director walk through its person search and each person's
 * credits, an IMDb title through its suggestions and each suggestion's find, a film by its record —
 * exactly as `TmdbIdentityLookups` answers them from the responses those documents were parsed from.
 * A document the store does not hold yet is `Unknown` (the fill's question), never "no film".
 * Venue detail pages are `details`' (the venue page index's).
 *
 * Every document a question reads is filed with `reads` under its [[TmdbStore.keyOf]] key, so a
 * document that changes re-asks exactly the questions that read it. A [[prefetch]] loads a slice's
 * documents in a few batched reads — questions, then the people and finds they name, then their films.
 */
final class StoredTmdbLookups(store: TmdbStore, language: String, details: IdentityLookups, reads: ObservationReads,
                              proposals: Option[ProposalIndex] = None)
    extends IdentityLookups {
  /** A model's proposal for the listing's title, the read filed so a new one re-resolves it. */
  override def proposal(listing: Listing): Option[Proposal] = proposals.flatMap(_.proposal(listing, reads))
  import StoredTmdbLookups._

  private val held = TmdbKind.values.map(_ -> new ConcurrentHashMap[String, Held]()).toMap

  override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], pages: Iterable[Listing]): Unit = {
    held.values.foreach(_.clear())
    val asked = queries.toSeq
    load(TmdbKind.Query, asked.flatMap(questionIds))
    // What the questions name: each person search's people, each IMDb title's finds.
    load(TmdbKind.Person, asked.collect { case CandidateQuery.Director(name) => personSearchOf(name) }
      .flatMap(heldDocument(TmdbKind.Query, _)).flatMap(d => intsOf(d).take(MaxPersonCandidates)).map(_.toString))
    load(TmdbKind.Query, asked.collect { case CandidateQuery.Imdb(title) => title }.flatMap { title =>
      heldDocument(TmdbKind.Query, TmdbStore.suggestionsId(ImdbClient.suggestionUrl(title))).toSeq
        .flatMap(suggestionIds(_, title)).map(TmdbStore.findId)
    })
    // And every film any of them names.
    val named = held(TmdbKind.Query).values.asScala.flatMap(_.document).filter(_.containsKey("ids")).flatMap(intsOf(_)) ++
      held(TmdbKind.Person).values.asScala.flatMap(_.document).flatMap(d => intsOf(d, "directed") ++ intsOf(d, "wrote"))
    load(TmdbKind.Film, (named ++ films).map(_.toString).toSeq)
    details.prefetch(Nil, Nil, pages)
  }

  // The documents a prefetch read stay only while its own asks are answered: held until the next
  // prefetch, a slice's searches and every film they name sat in the heap through the slice's build
  // and the resolves after it, and UK's take-up spent 72% of its time in full GCs.
  override def prefetchAnswered(): Unit = { held.values.foreach(_.clear()); details.prefetchAnswered() }

  /** How many documents the last prefetch still holds. */
  private[identity] def heldDocuments: Int = held.values.map(_.size).sum

  def hasDetail(listing: Listing): Boolean                  = details.hasDetail(listing)
  def detail(listing: Listing): Answer[Option[DetailFacts]] = details.detail(listing)

  def candidates(query: CandidateQuery): Answer[Seq[Hit]] = query match {
    case CandidateQuery.Title(text) =>
      document(TmdbKind.Query, TmdbStore.titleSearchId(language, text)).fold[Answer[Seq[Hit]]](Answer.Unknown)(d => hitsOf(intsOf(d)))
    case CandidateQuery.Director(name) =>
      document(TmdbKind.Query, personSearchOf(name)).fold[Answer[Seq[Hit]]](Answer.Unknown) { search =>
        val people = intsOf(search).take(MaxPersonCandidates).map(id => document(TmdbKind.Person, id.toString))
        if (people.exists(_.isEmpty)) Answer.Unknown
        else sequence(people.flatten.map { p =>
          val directed = intsOf(p, "directed")
          hitsOf(if (directed.nonEmpty) directed else intsOf(p, "wrote"))
        }).mapKnown(_.flatten.distinctBy(_.tmdbId))
      }
    case CandidateQuery.Imdb(title) =>
      val id = TmdbStore.suggestionsId(ImdbClient.suggestionUrl(title))
      document(TmdbKind.Query, id).fold[Answer[Seq[Hit]]](Answer.Unknown) { d =>
        val finds = suggestionIds(d, title).map(tt => document(TmdbKind.Query, TmdbStore.findId(tt)))
        if (finds.exists(_.isEmpty)) Answer.Unknown
        else sequence(finds.flatten.map(f => hitsOf(intsOf(f).take(1)))).mapKnown(_.flatten.distinctBy(_.tmdbId))
      }
  }

  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] =
    document(TmdbKind.Film, tmdbId.toString).flatMap(d => Option(d.get("record"))) match {
      case Some(record) => Answer.Known(IdentityAnswerBson.filmOf(record))
      case None         => Answer.Unknown
    }

  // ── documents, read once per prefetch and filed with `reads` ──────────────────────

  private def load(kind: TmdbKind, ids: Seq[String]): Map[String, Option[BsonDocument]] = {
    val wanted = ids.distinct.filterNot(held(kind).containsKey)
    val got    = if (wanted.isEmpty) Map.empty[String, BsonDocument] else store.answers(kind, wanted)
    wanted.foreach(id => held(kind).put(id, Held(got.get(id))))
    ids.map(id => id -> held(kind).get(id).document).toMap
  }

  private def document(kind: TmdbKind, id: String): Option[BsonDocument] = {
    reads.read(TmdbStore.keyOf(kind, id)) // tracked whatever it holds: its change re-asks the question
    Option(held(kind).get(id)).getOrElse(Held(store.answers(kind, Seq(id)).get(id))).document
  }

  private def heldDocument(kind: TmdbKind, id: String): Option[BsonDocument] = Option(held(kind).get(id)).flatMap(_.document)

  /** The films `ids` name, each as its record or hit reads — `Unknown` while any is not held. */
  private def hitsOf(ids: Seq[Int]): Answer[Seq[Hit]] = {
    val hits = ids.map(id => document(TmdbKind.Film, id.toString).flatMap(TmdbStore.filmHit(id, _)))
    if (hits.exists(_.isEmpty)) Answer.Unknown else Answer.Known(hits.flatten)
  }

  private def questionIds(query: CandidateQuery): Seq[String] = Seq(TmdbStore.questionId(language, query))
}

object StoredTmdbLookups {
  /** How many of a name's people the walk tries: `TmdbClient`'s own bound. */
  val MaxPersonCandidates = 4

  private final case class Held(document: Option[BsonDocument])

  private def personSearchOf(name: String): String =
    TmdbStore.personSearchId(CandidateQuery.personName(name))

  private def intsOf(d: BsonDocument, field: String = "ids"): Seq[Int] = TmdbStore.intsOf(d.get(field))

  /** The `tt` ids IMDb suggests for `title` — `ImdbClient.suggestedIds`' own reading. */
  private def suggestionIds(d: BsonDocument, title: String): Seq[String] = {
    val entries = d.getArray("suggestions").getValues.asScala.toSeq.map(_.asDocument).map { s =>
      ImdbClient.Suggestion(s.getString("id").getValue, Option(s.get("title")).map(_.asString.getValue),
        Option(s.get("year")).map(_.asInt32.getValue), s.getInt32("rank").getValue)
    }
    if (title.trim.isEmpty) Nil else ImdbClient.suggested(entries)
  }

  extension [A](answer: Answer[A]) private def mapKnown[B](f: A => B): Answer[B] = answer match {
    case Answer.Known(value) => Answer.Known(f(value))
    case Answer.Unknown      => Answer.Unknown
  }

  private def sequence[A](answers: Seq[Answer[A]]): Answer[Seq[A]] =
    if (answers.forall(_.toOption.isDefined)) Answer.Known(answers.flatMap(_.toOption)) else Answer.Unknown
}
