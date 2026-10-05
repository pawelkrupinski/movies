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
final class StoredTmdbLookups(store: TmdbStore, language: String, details: IdentityLookups, reads: ObservationReads)
    extends IdentityLookups {
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
    // Each IMDb-titled question's suggestions: their titles, then the finds of those IMDb lists under the title.
    val titled = asked.collect { case CandidateQuery.ImdbTitled(title) => title }.flatMap { title =>
      heldDocument(TmdbKind.Query, TmdbStore.suggestionsId(ImdbClient.suggestionUrl(title))).toSeq.flatMap(suggestionsOf).take(ImdbClient.SuggestedMovies).map(_.id)
    }.distinct
    load(TmdbKind.Query, titled.map(TmdbStore.imdbTitlesId) ++ titled.map(TmdbStore.findId))
    // And every film any of them names: TMDB's records, and a fallback source's for the ids TMDB holds none of.
    val named = held(TmdbKind.Query).values.asScala.flatMap(_.document).filter(_.containsKey("ids")).flatMap(intsOf(_)) ++
      held(TmdbKind.Person).values.asScala.flatMap(_.document).flatMap(d => intsOf(d, "directed") ++ intsOf(d, "wrote"))
    val (fallbacks, tmdbFilms) = films.toSeq.partition(FallbackIds.isFallback)
    load(TmdbKind.Film, (named ++ tmdbFilms).map(_.toString).toSeq)
    load(TmdbKind.Query, fallbacks.flatMap(FallbackIds.imdbId).map(TmdbStore.imdbRecordId))
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
    case CandidateQuery.ImdbTitled(title) =>
      document(TmdbKind.Query, TmdbStore.suggestionsId(ImdbClient.suggestionUrl(title))).fold[Answer[Seq[Hit]]](Answer.Unknown) { d =>
        val movies = suggestionsOf(d)
        ImdbClient.titled(title, movies, tt => document(TmdbKind.Query, TmdbStore.imdbTitlesId(tt)).map(stringsOf(_, "titles")))
          .fold[Answer[Seq[Hit]]](Answer.Unknown) { ids =>
            val finds = ids.map(tt => tt -> document(TmdbKind.Query, TmdbStore.findId(tt)))
            if (finds.exists(_._2.isEmpty)) Answer.Unknown
            else sequence(finds.flatMap(_._2).map(f => hitsOf(intsOf(f).take(1)))).mapKnown(found =>
              TmdbIdentityLookups.titledHits(found, TmdbIdentityLookups.fallbackHits(finds.collect { case (tt, Some(f)) if intsOf(f).isEmpty => tt }, movies)))
          }
      }
  }

  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] =
    if (FallbackIds.isFallback(tmdbId)) fallbackFilm(tmdbId)
    else document(TmdbKind.Film, tmdbId.toString).flatMap(d => Option(d.get("record")).map(d -> _)) match {
      case Some((d, record)) => Answer.Known(IdentityAnswerBson.filmOf(record).map(withImdbNumber(_, d)))
      case None              => Answer.Unknown
    }

  /** The record's release day — `Unknown` while the localized response it was parsed from holds only a year: filed before
   *  records kept the whole day (`TmdbNormalizer.minimal` cut it to the year), the record states no day though TMDB does,
   *  and reading it again (`agreement.AgreementStage.wantedRecords`) files the day. */
  override def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] = film(tmdbId) match {
    // the whole document, read only for a record stating no day: an answer reads no partial's date, and the answer cache
    // holding one per film would cost every film its bytes for the few stage relays that read it
    case Answer.Known(Some(record)) if record.released.isEmpty && store.get(TmdbKind.Film, Seq(tmdbId.toString)).get(tmdbId.toString).exists(yearOnly) =>
      Answer.Unknown
    case Answer.Known(record) => Answer.Known(record.flatMap(_.released))
    case Answer.Unknown       => Answer.Unknown
  }

  /** `film` with the IMDb number its responses name: a record filed before the record carried one holds none, while
   *  the responses it was parsed from ([[TmdbStore.Partial]]) still hold TMDB's `imdb_id`. */
  private def withImdbNumber(film: IdentityMeasures.Film, d: BsonDocument): IdentityMeasures.Film =
    if (film.imdbNumber > 0) film
    else TmdbStore.Partial.values.iterator.flatMap(partial => Option(d.get(partial.field)).filter(_.isDocument))
      .flatMap(response => Option(response.asDocument.get("imdb_id")).filter(_.isString).map(_.asString.getValue))
      .map(IdentityMeasures.imdbNumber).find(_ > 0).fold(film)(number => film.copy(imdbNumber = number))

  /** A fallback source's film: IMDb's record of the title, as `ImdbClient.identityRecord` read it. */
  private def fallbackFilm(id: Int): Answer[Option[IdentityMeasures.Film]] =
    FallbackIds.imdbId(id).fold[Answer[Option[IdentityMeasures.Film]]](Answer.Known(None)) { tt =>
      document(TmdbKind.Query, TmdbStore.imdbRecordId(tt)).flatMap(d => Option(d.get("record")))
        .fold[Answer[Option[IdentityMeasures.Film]]](Answer.Unknown)(record => Answer.Known(IdentityAnswerBson.filmOf(record)))
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

  /** Does the film's localized response date it by its year alone ("2026")? TMDB never answers so — it states a whole
   *  day or none — so only the store's cut before records kept the day does. */
  private def yearOnly(film: BsonDocument): Boolean =
    Option(film.get(TmdbStore.Partial.Local.field)).filter(_.isDocument).flatMap(local => Option(local.asDocument.get("release_date")))
      .exists(date => date.isString && date.asString.getValue.length == 4)

  /** The `tt` ids IMDb suggests for `title` — `ImdbClient.suggestedIds`' own reading. */
  private def suggestionIds(d: BsonDocument, title: String): Seq[String] =
    if (title.trim.isEmpty) Nil else ImdbClient.suggested(suggestionsOf(d))

  /** A suggestions document's movies, as `ImdbClient` read them. */
  private def suggestionsOf(d: BsonDocument): Seq[ImdbClient.Suggestion] =
    d.getArray("suggestions").getValues.asScala.toSeq.map(_.asDocument).map { s =>
      ImdbClient.Suggestion(s.getString("id").getValue, Option(s.get("title")).map(_.asString.getValue),
        Option(s.get("year")).map(_.asInt32.getValue), s.getInt32("rank").getValue)
    }

  private def stringsOf(d: BsonDocument, field: String): Seq[String] =
    Option(d.get(field)).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asString.getValue))

  extension [A](answer: Answer[A]) private def mapKnown[B](f: A => B): Answer[B] = answer match {
    case Answer.Known(value) => Answer.Known(f(value))
    case Answer.Unknown      => Answer.Unknown
  }

  private def sequence[A](answers: Seq[Answer[A]]): Answer[Seq[A]] =
    if (answers.forall(_.toOption.isDefined)) Answer.Known(answers.flatMap(_.toOption)) else Answer.Unknown
}
