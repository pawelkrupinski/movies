package clients.tools

import tools.{FixtureTestWiring, HttpFetch, RealHttpFetch}

/**
 * Additively record the TMDB PERSON endpoints the director walk needs, into the
 * existing corpus — without re-recording a single byte of cinema data.
 *
 * Why this exists: a director-bearing row now resolves ONLY by walking its
 * director's filmography, so `/3/search/person` and `/person/{id}/movie_credits`
 * became load-bearing for roughly half the corpus. The recorded corpus carries
 * only ~96 person searches, because the OLD resolver reached the walk rarely —
 * everything else resolved off a title search. `FakeHttpFetch` answers an
 * unrecorded TMDB query with an empty result set (deliberately — that is what
 * production sees for an unknown query), so on replay those walks silently found
 * nothing and 55 films came out unresolved. That was a fixture gap, not a
 * regression: probed against live TMDB, all 55 resolve.
 *
 * A full `RecordCorpusFixture` run would fix it but re-captures every cinema too,
 * moving the corpus to today's listings and away from the date hardcoded across
 * seven files. So: replay EVERYTHING from the fixtures as usual, and route only
 * the two person endpoints to a recording pass-through. The result is the same
 * corpus plus the person responses it was always missing.
 *
 * NOT needed when the corpus is re-rolled. `RecordAllDataToFixture` wraps ONE
 * `RecordingHttpFetch` around everything it drives, the `TmdbClient` included, so
 * a fresh capture records whatever endpoints the resolver of the day calls —
 * person lookups among them — with no separate step. This exists for the other
 * case: an EXISTING corpus that predates a new call, which is exactly how the
 * 08-06-2026 tree ended up without them.
 *
 * Run (needs TMDB_API_KEY from `.env.local`, and the network):
 *   sbt "worker/Test/runMain clients.tools.RecordDirectorWalkFixtures"
 *   sbt "worker/Test/runMain clients.tools.RecordDirectorWalkFixtures 350,42238"   // named films
 * then regenerate the snapshots per the `regenerate-snapshots` skill.
 */
object RecordDirectorWalkFixtures {

  private val Fixture = "08-06-2026"

  /** Replay from the corpus; fill in what the walk needs and the corpus lacks.
   *
   *  Two kinds of gap, both TMDB-only — a missing CINEMA fixture must still fail
   *  loudly rather than quietly re-scrape a live cinema site:
   *
   *   - The person endpoints, always recorded. They are the walk's inputs and the
   *     corpus barely has them, and `FakeHttpFetch` answers an unrecorded TMDB
   *     query with an empty result set rather than an error, so a stale one would
   *     silently read as "this director has no films".
   *   - Any other TMDB URL with NO fixture, recorded on the miss. Resolving by
   *     walk reaches films the old title search never did, and those films' own
   *     `/movie/{id}` and `/movie/{id}/external_ids` were therefore never captured
   *     ("Nagi instynkt" → 402). Unlike a search, those throw when unrecorded, so
   *     the miss is detectable — and the throw is what made the row's conclusion
   *     depend on retry order.
   *
   *  `foldYear = false` because these are ENRICHMENT fixtures — the year-scoped
   *  and yearless TMDB queries return materially different bodies and must not
   *  collapse onto one file (see `RecordingHttpFetch`). */
  private final class PersonRecordingFetch(fixture: String) extends HttpFetch {
    private val replay = new FakeHttpFetch(fixture)
    private val record = new RecordingHttpFetch(fixture, new RealHttpFetch, foldYear = false)

    private def isPersonEndpoint(url: String): Boolean =
      url.contains("/3/search/person") || url.contains("/movie_credits")

    private def isTmdb(url: String): Boolean = url.contains("api.themoviedb.org")

    private def viaReplayElseRecord[A](url: String)(f: HttpFetch => A): A =
      if (isPersonEndpoint(url)) f(record)
      else if (!isTmdb(url)) f(replay)
      else
        try f(replay)
        catch { case _: Exception => f(record) }

    override def get(url: String): String           = viaReplayElseRecord(url)(_.get(url))
    override def getBytes(url: String): Array[Byte] = viaReplayElseRecord(url)(_.getBytes(url))
    override def get(url: String, headers: Map[String, String]): String =
      viaReplayElseRecord(url)(_.get(url, headers))
    override def post(url: String, body: String, contentType: String): String =
      replay.post(url, body, contentType)
  }

  /** Record `/movie/{id}` + `/movie/{id}/external_ids` for specific films.
   *
   *  A single boot only ever reaches the candidates ITS arrival order produces,
   *  but `StagingOrderDeterminismSpec` replays the corpus in shuffled orders, and a
   *  row's key year moves with the order — so a different, entirely legitimate
   *  credit gets picked ("Diabeł ubiera się u Prady 2" keying to 2006 resolves to
   *  the FIRST Prada film, 350). Those films' fixtures are missing, the fetch
   *  throws, and the throw is what makes the outcome order-dependent.
   *
   *  Recording every credit of every director would be ~4.4k films — almost all of
   *  them never screened here — so instead take the ids a run actually asked for.
   *  Run the spec, collect the "No fixture file for …/movie/{id}/…" ids, pass them
   *  here. */
  private def recordFilms(ids: Seq[Int]): Unit = {
    val record = new RecordingHttpFetch(Fixture, new RealHttpFetch, foldYear = false)
    val key    = tools.Env.get("TMDB_API_KEY").getOrElse(sys.error("TMDB_API_KEY not set"))
    ids.foreach { id =>
      Seq(
        s"https://api.themoviedb.org/3/movie/$id?language=pl-PL&append_to_response=credits,release_dates&api_key=$key",
        s"https://api.themoviedb.org/3/movie/$id?language=en-US&append_to_response=alternative_titles&api_key=$key",
        s"https://api.themoviedb.org/3/movie/$id/external_ids?api_key=$key",
        s"https://api.themoviedb.org/3/movie/$id/credits?api_key=$key"
      ).foreach(u => scala.util.Try(record.get(u)))
      println(s"  recorded film $id")
    }
  }

  def main(args: Array[String]): Unit = {
    val explicit = args.flatMap(a => a.split(",")).flatMap(s => scala.util.Try(s.trim.toInt).toOption).toSeq
    if (explicit.nonEmpty) {
      println(s"Recording ${explicit.size} specific film(s) into $Fixture …")
      recordFilms(explicit)
      println("Done.")
      return
    }
    // `TestWiring` injects a STUB api key so replay never depends on a real one.
    // The person legs here DO go to live TMDB, so they need the real key or every
    // one 401s and is swallowed into "no candidates" — recording nothing at all.
    // Safe for the corpus: `FakeHttpFetch`/`RecordingHttpFetch` strip `api_key`
    // out of the fixture fingerprint, so a file recorded under the real key is
    // the same file replay looks up under the stub one.
    val realKey = tools.Env.get("TMDB_API_KEY").filter(_.nonEmpty).getOrElse(
      sys.error("TMDB_API_KEY not set — add it to .env.local (this script records against live TMDB)."))

    val wiring = new FixtureTestWiring(Fixture) {
      override lazy val httoFetch: HttpFetch       = new PersonRecordingFetch(Fixture)
      override lazy val enrichmentFetch: HttpFetch = httoFetch
      override lazy val tmdbClient: clients.TmdbClient =
        new clients.TmdbClient(enrichmentFetch, apiKey = Some(realKey))
    }
    println(s"Recording director-walk person fixtures into $Fixture … (full pipeline, ~2 minutes)")
    wiring.bootStartup()
    println("Done. Regenerate expected-schedules.txt / read-model-snapshot.json / expected-*.html next.")
  }
}
