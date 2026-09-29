package integration

import services.identity._
import tools.HttpFetch

/**
 * The normalized TMDB store against the recorded answers: every question a resolve asked is
 * answered again from documents the normalizer parsed as those same responses were fetched through
 * it, and must be the answer the recorded responses gave — popularity reduced to its bucket, and a
 * hit's own title/year only where its film holds no record (the resolver reads the record over it).
 */
object StoredLookupsEquivalence {
  final case class Result(questions: Int, films: Int, mismatches: Seq[String])

  /** `fetch` whose recording MISSES — answered 404 by the replay and counted in `misses` — fail as
   *  what they are, a request never recorded: a transient failure, which normalizes to nothing (a
   *  gap), where a real 404 is an answer. The recorded side reads them as `Unknown` by the same count. */
  private final class MissAsGap(fetch: HttpFetch, misses: () => Long) extends HttpFetch {
    private def read[A](call: => A): A = {
      val before = misses()
      try call catch { case e: tools.HttpStatusException if e.code == 404 && misses() != before => throw new java.io.IOException("not recorded", e) }
    }
    override def get(url: String): String                              = read(fetch.get(url))
    override def get(url: String, headers: Map[String, String]): String = read(fetch.get(url, headers))
    override def getBytes(url: String): Array[Byte]                   = read(fetch.getBytes(url))
    override def post(url: String, body: String, contentType: String): String = read(fetch.post(url, body, contentType))
  }

  def check(fetch: HttpFetch, misses: () => Long, language: java.util.Locale,
            asked: (Map[CandidateQuery, Answer[Seq[Hit]]], Map[Int, Answer[Option[IdentityMeasures.Film]]])): Result = {
    val (queries, films) = asked
    val store   = new TmdbStore(new InMemoryTmdbDocuments, java.time.Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC))
    val through = new NormalizingHttpFetch(new MissAsGap(fetch, misses), new TmdbNormalizer(store))
    val filling = new TmdbIdentityLookups(new clients.TmdbClient(through, apiKey = Some(settings.TmdbApiKey(IdentityShadow.StubTmdbKey)),
      language = language, retrySleep = (_: Long) => ()), new services.enrichment.ImdbClient(through), Nil)
    queries.keys.toSeq.sorted.foreach(filling.candidates)
    films.keys.toSeq.sorted.foreach(filling.film)

    val stored  = new StoredTmdbLookups(store, language.toLanguageTag, filling, new ObservationReads)
    stored.prefetch(queries.keys, films.keys, Nil)
    val recorded = films.collect { case (id, Answer.Known(Some(_))) => id }.toSet
    def bucketed(p: Double) = PopularityBucket.representative(PopularityBucket.of(p))
    def hitView(h: Hit) = if (recorded(h.tmdbId)) (h.tmdbId, None) else (h.tmdbId, Some((h.title, h.originalTitle, h.year, bucketed(h.popularity))))
    val queryMismatches = queries.toSeq.sortBy(_._1).flatMap { case (q, expected) =>
      (expected, stored.candidates(q)) match {
        case (Answer.Known(a), Answer.Known(b)) if a.map(hitView) == b.map(hitView) => None
        case (Answer.Unknown, Answer.Unknown)                                       => None
        case (a, b) => Some(s"${q.sortKey}: recorded ${render(a)} vs stored ${render(b)}")
      }
    }
    val filmMismatches = films.toSeq.sortBy(_._1).flatMap { case (id, expected) =>
      val want = expected match { case Answer.Known(f) => Answer.Known(f.map(r => r.copy(popularity = r.popularity.map(bucketed)))); case u => u }
      Option.when(want != stored.film(id))(s"film $id: recorded $want vs stored ${stored.film(id)}")
    }
    Result(queries.size, films.size, queryMismatches ++ filmMismatches)
  }

  private def render(a: Answer[Seq[Hit]]): String = a match {
    case Answer.Known(hits) => hits.take(4).map(h => s"${h.tmdbId}:${h.title}:${h.year.getOrElse("")}").mkString("[", ", ", if (hits.size > 4) ", …]" else "]")
    case Answer.Unknown     => "Unknown"
  }
}
