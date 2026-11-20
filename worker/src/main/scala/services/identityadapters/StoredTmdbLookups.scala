package services.identityadapters

import services.identity.*
import org.bson.{BsonDocument, BsonValue}
import services.enrichment.ImdbClient

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._
import services.identity.Film

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

  // What the latest prefetch holds: each prefetch fills a holding of its OWN and puts it in place of the last, and a
  // release puts an empty one in place -- never clears one a prefetch may be filling. One instance is asked from two
  // threads (the live agreement stage's: its apply on the projection's, `questionsOf` on the model's input gate), and
  // the release one made cleared the maps the other's prefetch was loading: an id it had just held was gone by the
  // time it read it back (worker-uk 2026-11-16, Sentry KINOWO-35, the drain failing and the model rebuilt). A holding
  // replaced under an ask only sends that ask to the store, as any document not held does.
  @volatile private var holding = new Holding

  override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], pages: Iterable[Listing]): Unit = {
    val mine = new Holding
    holding = mine
    def load(kind: TmdbKind, ids: Seq[String], readThrough: Boolean = false): Unit = mine.load(store, kind, ids, readThrough)
    def heldDocument(kind: TmdbKind, id: String): Option[BsonDocument] = mine.document(kind, id)
    val asked = queries.toSeq
    load(TmdbKind.Query, asked.map(TmdbStore.questionId(language, _)))
    // Each title credit's director, as its suggestions name him ([[TitleCredit.named]]): his person search, walked as a
    // director question's is.
    val credited = asked.collect { case CandidateQuery.TitleCredit(title) => title }.flatMap(title =>
      heldDocument(TmdbKind.Query, TmdbStore.imdbSuggestionsId(title)).filter(_.containsKey("directors"))
        .flatMap(d => TitleCredit.named(title, BsonFields.stringsOf(d, "directors"))))
    load(TmdbKind.Query, credited.map(personSearchOf))
    // What the questions name: each person search's people, each IMDb title's finds.
    load(TmdbKind.Person, (asked.collect { case CandidateQuery.Director(name) => name } ++ credited).map(personSearchOf)
      .flatMap(heldDocument(TmdbKind.Query, _)).flatMap(d => intsOf(d).take(MaxPersonCandidates)).map(_.toString))
    load(TmdbKind.Query, asked.collect { case CandidateQuery.Imdb(title) => title }.flatMap { title =>
      heldDocument(TmdbKind.Query, TmdbStore.imdbSuggestionsId(title)).toSeq
        .flatMap(suggestionIds(_, title)).map(TmdbStore.findId)
    })
    // Each IMDb-titled question's suggestions: their titles, then the finds of those IMDb lists under the title.
    val titled = asked.collect { case CandidateQuery.ImdbTitled(title) => title }.flatMap { title =>
      heldDocument(TmdbKind.Query, TmdbStore.imdbSuggestionsId(title)).toSeq.flatMap(suggestionsOf).take(ImdbClient.SuggestedMovies).map(_.id)
    }.distinct
    load(TmdbKind.Query, titled.map(TmdbStore.imdbTitlesId) ++ titled.map(TmdbStore.findId))
    // And every film any of them names, for its hit; the records asked for, read through — whoever asks for a
    // record keeps it decoded (the corpus its records) — and a fallback source's for the ids TMDB holds none of.
    val named = mine.held(TmdbKind.Query).values.asScala.flatMap(_.document).filter(_.containsKey("ids")).flatMap(intsOf(_)) ++
      mine.held(TmdbKind.Person).values.asScala.flatMap(_.document).flatMap(d => intsOf(d, "directed") ++ intsOf(d, "wrote"))
    val (fallbacks, tmdbFilms) = films.toSeq.partition(FallbackIds.isFallback)
    mine.loadHits(store, named.map(_.toString).toSeq)
    load(TmdbKind.Film, tmdbFilms.map(_.toString), readThrough = true)
    load(TmdbKind.Query, fallbacks.flatMap(FallbackIds.imdbId).map(TmdbStore.imdbRecordId))
    details.prefetch(Nil, Nil, pages)
  }

  // The documents a prefetch read stay only while its own asks are answered: held until the next
  // prefetch, a slice's searches and every film they name sat in the heap through the slice's build
  // and the resolves after it, and UK's take-up spent 72% of its time in full GCs.
  override def prefetchAnswered(): Unit = { release(); details.prefetchAnswered() }

  /** Let go of every document, film and hit the last prefetch holds. */
  private def release(): Unit = holding = new Holding

  def hasDetail(listing: Listing): Boolean                  = details.hasDetail(listing)
  def detail(listing: Listing): Answer[Option[DetailFacts]] = details.detail(listing)

  def candidates(query: CandidateQuery): Answer[Seq[Hit]] = query match {
    case CandidateQuery.Title(text) =>
      document(TmdbKind.Query, TmdbStore.titleSearchId(language, text)).toAnswer.flatMap(d => hitsOf(intsOf(d)))
    case CandidateQuery.Director(name) => directed(name)
    case CandidateQuery.TitleCredit(title) =>
      // a suggestions document filed before it kept the directors is no answer: the live ask files them
      document(TmdbKind.Query, TmdbStore.imdbSuggestionsId(title)).filter(_.containsKey("directors")).toAnswer.flatMap(d =>
        TitleCredit.named(title, BsonFields.stringsOf(d, "directors")).fold[Answer[Seq[Hit]]](Answer.Known(Nil))(directed))
    case CandidateQuery.Imdb(title) =>
      document(TmdbKind.Query, TmdbStore.imdbSuggestionsId(title)).toAnswer.flatMap { d =>
        firstFilms(suggestionIds(d, title).map(tt => document(TmdbKind.Query, TmdbStore.findId(tt)))).map(_.flatten.distinctBy(_.tmdbId))
      }
    case CandidateQuery.ImdbTitled(title) =>
      document(TmdbKind.Query, TmdbStore.imdbSuggestionsId(title)).toAnswer.flatMap { d =>
        val movies = suggestionsOf(d)
        ImdbClient.titled(title, movies, tt => document(TmdbKind.Query, TmdbStore.imdbTitlesId(tt)).map(BsonFields.stringsOf(_, "titles")))
          .toAnswer.flatMap { ids =>
            val finds = ids.map(tt => tt -> document(TmdbKind.Query, TmdbStore.findId(tt)))
            firstFilms(finds.map(_._2)).map(found =>
              TmdbIdentityLookups.titledHits(found, TmdbIdentityLookups.fallbackHits(finds.collect { case (tt, Some(f)) if intsOf(f).isEmpty => tt }, movies)))
          }
      }
  }

  /** The films a person search for `name` finds directed — or, with no directing credit, written — as its people's
   *  documents hold them: `Unknown` while the search or any of its people is not held. */
  private def directed(name: String): Answer[Seq[Hit]] =
    document(TmdbKind.Query, personSearchOf(name)).toAnswer.flatMap { search =>
      val people = intsOf(search).take(MaxPersonCandidates).map(id => document(TmdbKind.Person, id.toString))
      if (people.exists(_.isEmpty)) Answer.Unknown
      else Answer.sequence(people.flatten.map { p =>
        val directedIds = intsOf(p, "directed")
        hitsOf(if (directedIds.nonEmpty) directedIds else intsOf(p, "wrote"))
      }).map(_.flatten.distinctBy(_.tmdbId))
    }

  def film(tmdbId: Int): Answer[Option[Film]] =
    if (FallbackIds.isFallback(tmdbId)) fallbackFilm(tmdbId)
    else { val now = holding; Option(now.held(TmdbKind.Film).get(tmdbId.toString)) match {
      // One record object per film a prefetch holds: decoded again for every listing a family weighs against it, each
      // copy worked its titles, tokens and credits out afresh (worker-pl's identity model, JFR 2026-10-05). The document is
      // the prefetch's, unchanged until the next one, so its record is too.
      case Some(prefetched) if prefetched.document.exists(_.containsKey("record")) =>
        reads.read(TmdbStore.keyOf(TmdbKind.Film, tmdbId.toString))
        now.films.computeIfAbsent(tmdbId, _ => filmOf(prefetched.document))
      // Held with no record -- one response of its two filed, or a search's hit only -- is read again from the store, as
      // a document held absent is: a drain falling between the two responses' filings prefetched it so, the gap's live ask
      // files the other, and its filing wakes no one, the asker's own (Main #2712, #2716, #2717).
      case _ => filmOf(storedDocument(TmdbKind.Film, tmdbId.toString))
    } }

  private def filmOf(doc: Option[BsonDocument]): Answer[Option[Film]] =
    doc.flatMap(d => Option(d.get("record")).map(d -> _)) match {
      case Some((d, record)) => Answer.Known(IdentityAnswerBson.filmOf(record).map(film => withRuntimes(withImdbNumber(film, d), d)))
      case None              => Answer.Unknown
    }

  /** The record's release day — `Unknown` while the localized response it was parsed from holds only a year: filed before
   *  records kept the whole day (`TmdbNormalizer.minimal` cut it to the year), the record states no day though TMDB does,
   *  and reading it again (`agreement.AgreementStage.wanted`, its `Question.Record`s) files the day. */
  override def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] = film(tmdbId) match {
    // the whole document, read only for a record stating no day: an answer reads no partial's date, and the answer cache
    // holding one per film would cost every film its bytes for the few stage relays that read it
    case Answer.Known(Some(record)) if record.released.isEmpty && wholeFilm(tmdbId).exists(yearOnly) =>
      Answer.Unknown
    case Answer.Known(record) => Answer.Known(record.flatMap(_.released))
    case Answer.Failed        => Answer.Failed
    case Answer.Unknown       => Answer.Unknown
  }

  /** The film's top-billed cast, off its localized response's `credits` block as the store filed it — read from the
   *  whole document (an answer reads no partial), never kept on the record ([[IdentityLookups.cast]]): `Known(None)` for a
   *  response filed before the store kept the cast, and `Unknown` while the store holds no document of the film. Read
   *  only by the agreement stage's fill, for an unmatched cluster's candidates. */
  override def cast(tmdbId: Int): Answer[Option[Seq[String]]] =
    if (FallbackIds.isFallback(tmdbId)) Answer.Known(None)
    else wholeFilm(tmdbId) match {
      case Some(d) => Answer.Known(Option(d.get(TmdbStore.Partial.Local.field)).filter(_.isDocument)
        .flatMap(local => TmdbFilmRecord.cast(Seq(TmdbStore.jsonOf(local)))))
      case None    => Answer.Unknown
    }

  /** The words the film's overviews capitalise, off both its responses as the store filed them
   *  ([[TmdbFilmRecord.overview]]) — read from the whole document, never kept on the record, as the cast is: `Known(None)`
   *  for responses filed before the store kept them, `Unknown` while the store holds no document of the film. Read only by
   *  the agreement stage's synopsis evidence, for an unmatched cluster's candidates. */
  override def overview(tmdbId: Int): Answer[Option[ProseWords]] =
    if (FallbackIds.isFallback(tmdbId)) Answer.Known(None)
    else wholeFilm(tmdbId) match {
      case Some(d) => Answer.Known(TmdbFilmRecord.overview(TmdbStore.Partial.values.toSeq.flatMap(partial =>
        Option(d.get(partial.field)).filter(_.isDocument).map(TmdbStore.jsonOf))))
      case None    => Answer.Unknown
    }

  /** `film` with the IMDb number its responses name: a record filed before the record carried one holds none, while
   *  the responses it was parsed from ([[TmdbStore.Partial]]) still hold TMDB's `imdb_id`. */
  private def withImdbNumber(film: Film, d: BsonDocument): Film =
    if (film.imdbNumber > 0) film
    else partialFields(d, "imdb_id").filter(_.isString).map(value => IdentityMeasures.imdbNumber(value.asString.getValue)).find(_ > 0)
      .fold(film)(number => film.copy(imdbNumber = number))

  /** The film with every runtime its partial responses state ([[TmdbFilmRecord.runtimes]]), the record's own first: a record
   *  filed before records carried the other translation's runtime has only the localized one. */
  private def withRuntimes(film: Film, d: BsonDocument): Film = {
    val stated = partialFields(d, "runtime").filter(_.isInt32).map(_.asInt32.getValue).toSeq
    val runtimes = TmdbFilmRecord.runtimes(film.runtimes ++ stated)
    if (runtimes == film.runtimes) film else film.copy(runtime = runtimes.headOption, alternativeRuntimes = runtimes.drop(1))
  }

  /** A fallback source's film: IMDb's record of the title, as `ImdbClient.identityRecord` read it. */
  private def fallbackFilm(id: Int): Answer[Option[Film]] =
    FallbackIds.imdbId(id).fold[Answer[Option[Film]]](Answer.Known(None)) { tt =>
      document(TmdbKind.Query, TmdbStore.imdbRecordId(tt)).flatMap(d => Option(d.get("record")))
        .map(IdentityAnswerBson.filmOf).toAnswer
    }

  // ── documents, read once per prefetch and filed with `reads` ──────────────────────

  private def document(kind: TmdbKind, id: String): Option[BsonDocument] = {
    reads.read(TmdbStore.keyOf(kind, id)) // tracked whatever it holds: its change re-asks the question
    // A document the prefetch found absent is read again: the gap's live ask files it moments later, and an absence
    // held would answer the question that filed it from before its own filing. So no negative memo, not even per
    // instance: `StoredFirstLookups.storedFirst` asks here, on a gap asks live (filing into this store), then asks
    // here AGAIN to answer as the store holds it — a remembered absence would hand it the unbucketed live answer.
    holding.document(kind, id).orElse(store.answers(kind, Seq(id)).get(id))
  }

  /** The document as the store holds it now, never as a prefetch held it -- filed with `reads` as [[document]]'s is. */
  private def storedDocument(kind: TmdbKind, id: String): Option[BsonDocument] = {
    reads.read(TmdbStore.keyOf(kind, id))
    store.answers(kind, Seq(id)).get(id)
  }

  /** Each of `finds`' first film, as its hit reads — `Unknown` while any find, or any of those films, is not held. */
  private def firstFilms(finds: Seq[Option[BsonDocument]]): Answer[Seq[Seq[Hit]]] =
    if (finds.exists(_.isEmpty)) Answer.Unknown else Answer.sequence(finds.flatten.map(f => hitsOf(intsOf(f).take(1))))

  /** The store's whole document of TMDB film `id` — every partial response, not only what an answer reads. */
  private def wholeFilm(id: Int): Option[BsonDocument] = store.get(TmdbKind.Film, Seq(id.toString)).get(id.toString)

  /** The films `ids` name, each as its record or hit reads — `Unknown` while any is not held. */
  private def hitsOf(ids: Seq[Int]): Answer[Seq[Hit]] = {
    val found = ids.map(hit)
    if (found.exists(_.isEmpty)) Answer.Unknown else Answer.Known(found.flatten)
  }

  /** A film's hit, filed with `reads` under its document's key: its change re-asks the question. */
  private def hit(id: Int): Option[Hit] = {
    val key = id.toString
    reads.read(TmdbStore.keyOf(TmdbKind.Film, key))
    Option(holding.hits.get(key)).flatten.orElse(store.filmHits(Seq(key)).get(key)) // an absent one read again, as `document`'s
  }
}

object StoredTmdbLookups {
  /** How many of a name's people the walk tries: [[clients.TmdbClient]]'s own bound. */
  val MaxPersonCandidates: Int = clients.TmdbClient.MaxPersonCandidates

  private final case class Held(document: Option[BsonDocument])

  /** What one prefetch read: its documents by kind (`None` for one the store holds none of), the films decoded from them
   *  and the hits of the films they name, by film id (`None` for a film holding none). Filled only by the prefetch that
   *  made it, and let go whole when the next one, or a release, takes its place. */
  private final class Holding {
    val held  = TmdbKind.values.map(_ -> new ConcurrentHashMap[String, Held]()).toMap
    val films = new ConcurrentHashMap[Int, Answer[Option[Film]]]()
    val hits  = new ConcurrentHashMap[String, Option[Hit]]()

    def document(kind: TmdbKind, id: String): Option[BsonDocument] = Option(held(kind).get(id)).flatMap(_.document)

    def load(store: TmdbStore, kind: TmdbKind, ids: Seq[String], readThrough: Boolean): Unit = {
      val wanted = ids.distinct.filterNot(held(kind).containsKey)
      val got    = if (wanted.isEmpty) Map.empty[String, BsonDocument]
                   else if (readThrough) store.answersReadThrough(kind, wanted) else store.answers(kind, wanted)
      wanted.foreach(id => held(kind).put(id, Held(got.get(id))))
    }

    def loadHits(store: TmdbStore, ids: Seq[String]): Unit = {
      val wanted = ids.distinct.filterNot(hits.containsKey)
      if (wanted.nonEmpty) { val got = store.filmHits(wanted); wanted.foreach(id => hits.put(id, got.get(id))) }
    }
  }

  private def personSearchOf(name: String): String =
    TmdbStore.personSearchId(CandidateQuery.personName(name))

  /** What each of a film's partial responses ([[TmdbStore.Partial]]) holds under `field`, in their order. */
  private def partialFields(d: BsonDocument, field: String): Iterator[BsonValue] =
    TmdbStore.Partial.values.iterator.flatMap(partial => Option(d.get(partial.field)).filter(_.isDocument))
      .flatMap(response => Option(response.asDocument.get(field)))

  private def intsOf(d: BsonDocument, field: String = "ids"): Seq[Int] = BsonFields.intsOf(d.get(field))

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
}
