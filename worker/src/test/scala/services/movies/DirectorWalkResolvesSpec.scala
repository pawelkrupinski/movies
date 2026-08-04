package services.movies

import clients.TmdbClient
import models.{Helios, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{InProcessEventBus, MovieDetailsComplete}
import tools.GetOnlyHttpFetch
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * The director is how a row RESOLVES, not a veto applied to a title search.
 *
 * Two failures this pins, both of which let a wrong film stick:
 *
 *  1. **A contradicted resolution must re-resolve.** `needsTmdbResolution`
 *     already re-verifies a resolved row against its reported directors — but
 *     `reportedDirectors` read EVERY slot, including the `Tmdb` slot the
 *     previous resolution stamped on. So the wrong film's own director verified
 *     the wrong film, the contradiction never fired, and the row was stuck for
 *     good. Reading cinema slots only makes the check mean something, and the
 *     whole `Dreams`/`Drømmer` class self-heals instead of needing an operator.
 *
 *  2. **A title search verified by director is not good enough.** Verification
 *     only asks "did this candidate's credits include the reported director" —
 *     which CANNOT separate two films by the SAME director. Yann Gozlan's
 *     "Gourou" and "Dalloway" both pass it, so whichever the title search
 *     happened to return won. Walking the director's filmography and matching
 *     the title is what actually picks between them; when the walk can't find
 *     the film, the honest answer is no match, not a verified guess.
 */
class DirectorWalkResolvesSpec extends AnyFlatSpec with Matchers {

  private class StubFetch(routes: Map[String, String]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
  }

  // ── 1. A resolution its own cinema contradicts must re-resolve ─────────────

  private val DreamsTmdb = 1134463   // Michel Franco, "Dreams: Sueños"
  private val DrommerTmdb = 1228682  // Dag Johan Haugerud, "Drømmer"

  private def dreamsTmdb(): TmdbClient = new TmdbClient(
    http = new StubFetch(Map(
      "/search/movie"                     -> """{"results":[]}""",
      "query=Michel+Franco"               -> """{"results":[{"id":5000,"name":"Michel Franco","known_for_department":"Directing"}]}""",
      "/person/5000/movie_credits"        -> s"""{"crew":[
        |{"id":$DreamsTmdb,"title":"Dreams","original_title":"Dreams: Sueños",
        | "release_date":"2025-07-10","department":"Directing","popularity":6.2}
        |]}""".stripMargin,
      // The stale resolution's credits — Haugerud, NOT Michel Franco.
      s"/movie/$DrommerTmdb/credits"      -> """{"crew":[{"job":"Director","name":"Dag Johan Haugerud","original_name":"Dag Johan Haugerud"}]}""",
      s"/movie/$DreamsTmdb/credits"       -> """{"crew":[{"job":"Director","name":"Michel Franco","original_name":"Michel Franco"}]}""",
      s"/movie/$DreamsTmdb/external_ids"  -> s"""{"id":$DreamsTmdb,"imdb_id":"tt31710990"}""",
      s"/movie/$DreamsTmdb"               -> s"""{"id":$DreamsTmdb,"title":"Dreams","original_title":"Dreams: Sueños","release_date":"2025-07-10","runtime":98}"""
    )),
    apiKey = Some("stub"))

  "a resolved row whose cinema reports a different director" should "re-resolve instead of verifying against its own Tmdb slot" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val bus   = new InProcessEventBus()
    val service = new MovieService(cache, bus, dreamsTmdb())
    bus.subscribe(service.onMovieDetailsComplete)

    val key = cache.keyOf("Dreams", Some(2024))
    // Production's stuck shape: resolved to Drømmer, one cinema slot naming
    // Michel Franco, and a Tmdb slot carrying the wrong film's own director.
    cache.put(key, MovieRecord(
      tmdbId = Some(DrommerTmdb),
      data = Map[Source, SourceData](
        Helios -> SourceData(title = Some("Dreams"), director = Seq("Michel Franco")),
        Tmdb   -> SourceData(director = Seq("Dag Johan Haugerud"), releaseYear = Some(2024)))))

    // A re-scrape carrying the cinema's director. With the bug `reportedDirectors`
    // also sees the Tmdb slot's "Dag Johan Haugerud", which verifies against the
    // current id, so the contradiction never fires and the row keeps Drømmer.
    bus.publish(MovieDetailsComplete("Dreams", Some(2024), originalTitle = None, director = Some("Michel Franco")))
    service.stop()

    val row = cache.get(cache.canonicalKeyFor(key).getOrElse(key))
    row.flatMap(_.tmdbId) shouldBe Some(DreamsTmdb)
  }

  // ── 1b. A re-resolve onto a DIFFERENT film must drop that film's ratings ───

  /** `buildResolvedRecord` already refuses to let a stale `imdbId` leak across a
   *  change of film ("a DIFFERENT tmdbId accepts the new film's ids"), but the
   *  rating URLs and scores were carried over unconditionally. So a corrected row
   *  kept the WRONG film's Metacritic/RT/Filmweb links and numbers until each
   *  source's own refresh cadence came round — prod served Michel Franco's
   *  "Dreams" with `/movie/dreams-drommer` and the Norwegian film's metascore 81
   *  hours after the tmdbId was fixed. Identity-derived fields follow the identity. */
  it should "drop the previous film's rating urls and scores" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val bus   = new InProcessEventBus()
    val service = new MovieService(cache, bus, dreamsTmdb())
    bus.subscribe(service.onMovieDetailsComplete)

    val key = cache.keyOf("Dreams", Some(2024))
    cache.put(key, MovieRecord(
      tmdbId            = Some(DrommerTmdb),
      imdbRating        = Some(7.2),
      metascore         = Some(81),
      metacriticUrl     = Some("https://www.metacritic.com/movie/dreams-drommer"),
      rottenTomatoes    = Some(90),
      rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/dreams_2024"),
      filmwebRating     = Some(7.2),
      filmwebUrl        = Some("https://www.filmweb.pl/film/Sny+o+milosci-2024-10060046"),
      data = Map[Source, SourceData](
        Helios -> SourceData(title = Some("Dreams"), director = Seq("Michel Franco")),
        Tmdb   -> SourceData(director = Seq("Dag Johan Haugerud"), releaseYear = Some(2024)))))

    bus.publish(MovieDetailsComplete("Dreams", Some(2024), originalTitle = None, director = Some("Michel Franco")))
    service.stop()

    val row = cache.get(cache.canonicalKeyFor(key).getOrElse(key)).getOrElse(fail("row vanished"))
    row.tmdbId shouldBe Some(DreamsTmdb)     // it really did move to the other film
    row.metacriticUrl     shouldBe None
    row.metascore         shouldBe None
    row.rottenTomatoesUrl shouldBe None
    row.rottenTomatoes    shouldBe None
    row.filmwebUrl        shouldBe None
    row.filmwebRating     shouldBe None
    row.imdbRating        shouldBe None
  }

  // ── 2. A same-director sibling must not resolve via the verified search ────

  private val Gourou   = 1259983   // Yann Gozlan, "Gourou" — the film the cinema shows
  private val Dalloway = 1315702   // Yann Gozlan, "Dalloway" — a DIFFERENT film, same director

  "a director-bearing row whose walk finds nothing" should "refuse rather than accept a same-director title-search hit" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    // findPerson misses (TMDB can't place the name), so the walk yields nothing.
    // The title search DOES return a hit — Gozlan's OTHER film — and verifying it
    // by director PASSES, because it really is a Yann Gozlan picture. Only the
    // walk could have told the two apart, so the verified search must not stand in.
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/person"              -> """{"results":[]}""",
      "/search/movie"               -> s"""{"results":[{"id":$Dalloway,"title":"Dalloway","original_title":"Dalloway","release_date":"2025-09-17","popularity":7.0}]}""",
      s"/movie/$Dalloway/credits"   -> """{"crew":[{"job":"Director","name":"Yann Gozlan","original_name":"Yann Gozlan"}]}""",
      s"/movie/$Dalloway/external_ids" -> s"""{"id":$Dalloway,"imdb_id":"tt00000000"}""",
      s"/movie/$Dalloway"           -> s"""{"id":$Dalloway,"title":"Dalloway","original_title":"Dalloway","release_date":"2025-09-17","runtime":100}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Guru"), director = Seq("Yann Gozlan"))))
    val resolved = service.resolveStagingRecord("Guru", Some(2025), existing)

    // Refused: no tmdbId, and definitively concluded as a no-match.
    resolved.flatMap(_.tmdbId) shouldBe None
    resolved.map(_.tmdbNoMatch) shouldBe Some(true)
    // Specifically NOT the same-director sibling the verified search would have taken.
    resolved.flatMap(_.tmdbId) should not be Some(Dalloway)
  }

  // ── 3. The WALK picks the person, not the person-search ranking ───────────

  /** `findPerson` took TMDB's first hit, and that hit is routinely the wrong
   *  person: a duplicate stub with zero credits ("Chan-wook Park" → a "Park Chan
   *  Wook" entry with none, while the real one is 5646), or an ALIAS collision
   *  ("Andrew Stanton" → Jim Wynorski, who lists it among his pseudonyms). Once
   *  the walk is the only resolver, one bad person pick is the whole resolution.
   *
   *  Which person is right is decided by the same evidence the walk already
   *  uses — whose filmography actually contains the film — so try the candidates
   *  in turn rather than trusting the ranking. Measured against live TMDB this
   *  is 3 of the 5 remaining corpus failures.
   */
  "the walk" should "try further person candidates when the first has no matching credit" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie" -> """{"results":[]}""",
      // Two people answer the name. The first is the credit-less stub TMDB ranks
      // top; the real director is second.
      "/search/person" -> """{"results":[
        |{"id":3311495,"name":"Park Chan Wook","known_for_department":"Directing"},
        |{"id":5646,"name":"Park Chan-wook","known_for_department":"Directing"}
        |]}""".stripMargin,
      "/person/3311495/movie_credits" -> """{"crew":[]}""",
      "/person/5646/movie_credits"    -> """{"crew":[
        |{"id":1429318,"title":"Bez wyjścia","original_title":"어쩔수가없다",
        | "release_date":"2025-09-24","department":"Directing","job":"Director","popularity":9.0}
        |]}""".stripMargin,
      "/movie/1429318/external_ids"   -> """{"id":1429318,"imdb_id":"tt27577097"}""",
      "/movie/1429318"                -> """{"id":1429318,"title":"Bez wyjścia","original_title":"어쩔수가없다","release_date":"2025-09-24","runtime":139}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Bez wyjścia"), director = Seq("Chan-wook Park"))))
    val resolved = service.resolveStagingRecord("Bez wyjścia", Some(2025), existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(1429318)
  }

  // ── 3b. A credit must match the TITLE. Year alone is never enough ─────────

  /** The walk used to have a year-only branch: when the director had exactly ONE
   *  credit in the row's year, take it — with no title check whatsoever. That is a
   *  resolution by director alone, and it is the one thing the walk must not do.
   *
   *  It resolved a film to whatever that person happened to make that year:
   *  "Głos Hind Rajab" became Łukasz Kowalski's "Lombard" because ONE cinema of
   *  four published his name, and "Zawieście czerwone latarnie" became a different
   *  Zhang Yimou film because one cinema published 1989 instead of 1991. Nothing
   *  inside a row can tell those from a correct (director, year) pair — the title
   *  is the only evidence that the credit IS the film the cinema is showing.
   *
   *  The cost is real and accepted: films whose Polish title has no TMDB entry
   *  (TMDB's pl-PL credits fall back to the original — "Giulietta degli spiriti"
   *  for "Giulietta i duchy") no longer resolve off the director. They keep the
   *  cinema's own title, synopsis and poster; they lose TMDB metadata and ratings.
   */
  it should "refuse a credit that matches only on year, never on title" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"  -> """{"results":[]}""",
      // The ONLY person the name resolves to — i.e. the candidate the walk trusts
      // most — with exactly ONE credit in the row's year, under a completely
      // unrelated title. Year-only would have taken it.
      "/search/person" -> """{"results":[{"id":700002,"name":"Łukasz Kowalski","known_for_department":"Directing"}]}""",
      "/person/700002/movie_credits" -> """{"crew":[
        |{"id":949623,"title":"Lombard","original_title":"Lombard",
        | "release_date":"2025-11-04","department":"Directing","job":"Director","popularity":3.0}
        |]}""".stripMargin
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Głos Hind Rajab"), director = Seq("Łukasz Kowalski"))))
    val resolved = service.resolveStagingRecord("Głos Hind Rajab", Some(2025), existing)

    resolved.flatMap(_.tmdbId) shouldBe None
  }

  /** The other half of the same guard: it must NOT cost the films it exists for.
   *
   *  TMDB's pl-PL credits fall back to the ORIGINAL title when no Polish one is
   *  registered, so "Giulietta i duchy" faces "Giulietta degli spiriti" —
   *  `titleClose`'s edit distance can't bridge that, and only the year-pinned
   *  branch resolves it. A shared distinctive word is what tells this apart from
   *  the Lombard case: translations keep the proper noun, unrelated films share
   *  nothing. */
  it should "still resolve a year-pinned credit whose title is merely translated" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"  -> """{"results":[]}""",
      "/search/person" -> """{"results":[{"id":4415,"name":"Federico Fellini","known_for_department":"Directing"}]}""",
      // TMDB has no Polish title, so the credit reads Italian — far outside
      // `titleClose` — but "Giulietta" survives the translation.
      "/person/4415/movie_credits" -> """{"crew":[
        |{"id":19120,"title":"Giulietta degli spiriti","original_title":"Giulietta degli spiriti",
        | "release_date":"1965-10-22","department":"Directing","job":"Director","popularity":8.0}
        |]}""".stripMargin,
      "/movie/19120/external_ids" -> """{"id":19120,"imdb_id":"tt0059229"}""",
      "/movie/19120"              -> """{"id":19120,"title":"Giulietta degli spiriti","original_title":"Giulietta degli spiriti","release_date":"1965-10-22","runtime":137}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Giulietta i duchy"), director = Seq("Federico Fellini"), releaseYear = Some(1965))))
    val resolved = service.resolveStagingRecord("Giulietta i duchy", Some(1965), existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(19120)
  }

  // ── 3d. RUNTIME corroborates where no shared word survives translation ────

  /** A shared word rescues a translation that keeps a proper noun ("Giulietta"),
   *  but a fully translated title keeps nothing: "Trener Tenisa" against TMDB's
   *  "Il Maestro", "Kochanie" against "Gioia mia", "Miasta na równinie" against
   *  "Le città di pianura". There is no title evidence to find, so the year-pinned
   *  branch is the only way in — and on its own it is what resolved a film to a
   *  stranger.
   *
   *  RUNTIME is evidence the title cannot give and language cannot touch. Measured
   *  across the corpus it separates the two cases cleanly: the translated films
   *  agree to within a minute or two (125/125, 98/97, 93/95), while the bogus
   *  matches are nowhere near (89 vs Lombard's 78, 142 vs Codename Cougar's 76).
   *  Cinemas publish no cast for these rows, so runtime is the signal that exists.
   */
  it should "accept a year-pinned credit whose RUNTIME matches, when the title is fully translated" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"  -> """{"results":[]}""",
      "/search/person" -> """{"results":[{"id":600001,"name":"Andrea Di Stefano","known_for_department":"Directing"}]}""",
      // Nothing in "Il Maestro" shares a word with "Trener Tenisa".
      "/person/600001/movie_credits" -> """{"crew":[
        |{"id":1143973,"title":"Il Maestro","original_title":"Il Maestro",
        | "release_date":"2025-09-01","department":"Directing","job":"Director","popularity":5.0}
        |]}""".stripMargin,
      "/movie/1143973?"            -> """{"id":1143973,"title":"Il Maestro","original_title":"Il Maestro","release_date":"2025-09-01","runtime":125}""",
      "/movie/1143973/external_ids" -> """{"id":1143973,"imdb_id":"tt30000001"}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Trener Tenisa"), director = Seq("Andrea Di Stefano"),
                           releaseYear = Some(2025), runtimeMinutes = Some(125))))
    val resolved = service.resolveStagingRecord("Trener Tenisa", Some(2025), existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(1143973)
  }

  /** …and the runtime corroborator must not become a new way in for the very
   *  match it was added beside. The cinema reporting Hind Rajab's 89 minutes
   *  against Lombard's 78 is the real corpus pair. */
  it should "still refuse a year-only match when the runtime disagrees too" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"  -> """{"results":[]}""",
      "/search/person" -> """{"results":[{"id":700002,"name":"Łukasz Kowalski","known_for_department":"Directing"}]}""",
      "/person/700002/movie_credits" -> """{"crew":[
        |{"id":949623,"title":"Lombard","original_title":"Lombard",
        | "release_date":"2025-11-04","department":"Directing","job":"Director","popularity":3.0}
        |]}""".stripMargin,
      "/movie/949623?" -> """{"id":949623,"title":"Lombard","original_title":"Lombard","release_date":"2025-11-04","runtime":78}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Głos Hind Rajab"), director = Seq("Łukasz Kowalski"),
                           releaseYear = Some(2025), runtimeMinutes = Some(89))))
    val resolved = service.resolveStagingRecord("Głos Hind Rajab", Some(2025), existing)

    resolved.flatMap(_.tmdbId) shouldBe None
  }

  // ── 3e. The shared word may be written in another ALPHABET ───────────────

  /** "Mavka. Prawdziwy mit" against TMDB's "Мавка. Справжній міф" shares its
   *  proper noun — the SAME word, in Cyrillic. `sanitize` keeps only a-z0-9, so
   *  the Cyrillic side reduces to nothing and the titles look unrelated. Ukrainian
   *  releases and dubs are a standing part of the Polish corpus, so the token
   *  comparison transliterates before folding. Runtime can't save this one: the
   *  cinema publishes 100 minutes against TMDB's 90. */
  it should "match a shared title word written in Cyrillic" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"  -> """{"results":[]}""",
      "/search/person" -> """{"results":[{"id":3086908,"name":"Katya Tsarik","known_for_department":"Directing"}]}""",
      "/person/3086908/movie_credits" -> """{"crew":[
        |{"id":1459612,"title":"Мавка. Справжній міф","original_title":"Мавка. Справжній міф",
        | "release_date":"2026-01-01","department":"Directing","job":"Director","popularity":4.0}
        |]}""".stripMargin,
      "/movie/1459612?"             -> """{"id":1459612,"title":"Мавка. Справжній міф","original_title":"Мавка. Справжній міф","release_date":"2026-01-01","runtime":90}""",
      "/movie/1459612/external_ids" -> """{"id":1459612,"imdb_id":"tt30000002"}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    // Runtime deliberately disagrees (100 vs 90) so only the transliterated
    // title can carry it.
    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Mavka. Prawdziwy mit"), director = Seq("Katya Tsarik"),
                           releaseYear = Some(2026), runtimeMinutes = Some(100))))
    val resolved = service.resolveStagingRecord("Mavka. Prawdziwy mit", Some(2026), existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(1459612)
  }

  // ── 3f. A sequel must not collapse onto the film it follows ──────────────

  /** `titleClose` is deliberately fuzzy so a cinema's spelling of a foreign title
   *  still finds the credit ("Guru" → "Gourou"). A SEQUEL is one character away
   *  from the film it follows, so both match — and `minByOption(_.id)`, there to
   *  collapse a TMDB duplicate of ONE film, then picks the older one.
   *
   *  The row's year normally hides this: a 2026 key year filters the 2006 original
   *  out. But the year arrives with whichever cinema publishes it, so a row scraped
   *  before that lands resolves year-less — and "Diabeł ubiera się u Prady 2" took
   *  tmdbId 350, the FIRST film, in exactly those arrival orders. Two orders, two
   *  identities for one film, which is what `StagingOrderDeterminismSpec` catches.
   *
   *  An exact title match is not a guess, so it outranks a fuzzy one; the lowest-id
   *  tie-break still applies among equally exact credits, where it is doing the job
   *  it was written for.
   */
  it should "prefer an exact title match over a one-character-off sibling, with no year to separate them" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"  -> """{"results":[]}""",
      "/search/person" -> """{"results":[{"id":5065,"name":"David Frankel","known_for_department":"Directing"}]}""",
      // The original has the LOWER id, so lowest-id would take it.
      "/person/5065/movie_credits" -> """{"crew":[
        |{"id":350,"title":"Diabeł ubiera się u Prady","original_title":"The Devil Wears Prada",
        | "release_date":"2006-06-30","department":"Directing","job":"Director","popularity":30.0},
        |{"id":1314481,"title":"Diabeł ubiera się u Prady 2","original_title":"The Devil Wears Prada 2",
        | "release_date":"2026-05-01","department":"Directing","job":"Director","popularity":120.0}
        |]}""".stripMargin,
      "/movie/1314481/external_ids" -> """{"id":1314481,"imdb_id":"tt33612209"}""",
      "/movie/1314481?"             -> """{"id":1314481,"title":"Diabeł ubiera się u Prady 2","original_title":"The Devil Wears Prada 2","release_date":"2026-05-01","runtime":110}""",
      // Stubbed so the pre-fix behaviour surfaces as the WRONG film rather than a
      // fetch failure — the point is that it resolved, confidently, to the original.
      "/movie/350/external_ids"     -> """{"id":350,"imdb_id":"tt0458352"}""",
      "/movie/350?"                 -> """{"id":350,"title":"Diabeł ubiera się u Prady","original_title":"The Devil Wears Prada","release_date":"2006-06-30","runtime":109}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    // No year anywhere — the state a row is in before the cinema that publishes
    // one has been scraped.
    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Diabeł ubiera się u Prady 2"), director = Seq("David Frankel"))))
    val resolved = service.resolveStagingRecord("Diabeł ubiera się u Prady 2", None, existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(1314481)
  }

  // ── 3g. The name a cinema prints may be the WRITER ───────────────────────

  /** Cinemas do not reliably print the director. "Drzewo magii" is directed by Ben
   *  Gregor and WRITTEN by Simon Farnaby, and cinemas print one or the other — a
   *  fact that already cost this repo a false "two different films" split, because
   *  the two names never overlap on what is plainly one film.
   *
   *  So the walk should follow the writer too. Its guards are unchanged: the credit
   *  still has to match the title (or a corroborated year), so widening WHERE the
   *  film may be found doesn't widen what counts as a match. Directing credits are
   *  tried first, so nothing about the common case changes; writing is the fallback
   *  for a row that would otherwise resolve to nothing at all.
   */
  it should "follow the writer's filmography when the cinema credits the writer" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"  -> """{"results":[]}""",
      "/search/person" -> """{"results":[{"id":1215930,"name":"Simon Farnaby","known_for_department":"Writing"}]}""",
      // Farnaby DIRECTED none of these; the film is among what he WROTE.
      "/person/1215930/movie_credits" -> """{"crew":[
        |{"id":1140521,"title":"Drzewo magii","original_title":"The Magic Faraway Tree",
        | "release_date":"2025-12-12","department":"Writing","job":"Writer","popularity":8.0}
        |]}""".stripMargin,
      "/movie/1140521/external_ids" -> """{"id":1140521,"imdb_id":"tt21276604"}""",
      "/movie/1140521?"             -> """{"id":1140521,"title":"Drzewo magii","original_title":"The Magic Faraway Tree","release_date":"2025-12-12","runtime":96}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Drzewo magii"), director = Seq("Simon Farnaby"), releaseYear = Some(2025))))
    val resolved = service.resolveStagingRecord("Drzewo magii", Some(2025), existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(1140521)
  }

  /** Widening to writers must not widen what counts as a MATCH. A writer's
   *  filmography is long, and a year-pinned credit from it needs the same
   *  corroboration as any other — an unrelated title with nothing to back it stays
   *  refused. */
  it should "still refuse an uncorroborated year-only match from a writer's filmography" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"  -> """{"results":[]}""",
      "/search/person" -> """{"results":[{"id":1215930,"name":"Simon Farnaby","known_for_department":"Writing"}]}""",
      "/person/1215930/movie_credits" -> """{"crew":[
        |{"id":555001,"title":"Coś zupełnie innego","original_title":"Something Else Entirely",
        | "release_date":"2025-03-03","department":"Writing","job":"Writer","popularity":4.0}
        |]}""".stripMargin,
      "/movie/555001?" -> """{"id":555001,"title":"Coś zupełnie innego","original_title":"Something Else Entirely","release_date":"2025-03-03","runtime":140}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    // Shares no word with the credit, and the runtime disagrees (96 vs 140).
    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Drzewo magii"), director = Seq("Simon Farnaby"),
                           releaseYear = Some(2025), runtimeMinutes = Some(96))))
    val resolved = service.resolveStagingRecord("Drzewo magii", Some(2025), existing)

    resolved.flatMap(_.tmdbId) shouldBe None
  }

  // ── 4. A " - " programme banner must not hide the film's own title ────────

  /** "Ladies Night - Narodziny gwiazdy" is a programme banner joined with a
   *  DASH rather than a colon or pipe. The de-decoration split only the latter
   *  two, so the walk compared Bradley Cooper's credits against the whole
   *  decorated string and missed "Narodziny gwiazdy" sitting right there. Extra
   *  candidates are safe under a walk — it still only accepts a title that
   *  matches a credit — so splitting the dash costs nothing and recovers the film. */
  it should "see the film's own title through a dash-joined programme banner" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"  -> """{"results":[]}""",
      "/search/person" -> """{"results":[{"id":51329,"name":"Bradley Cooper","known_for_department":"Acting"}]}""",
      "/person/51329/movie_credits" -> """{"crew":[
        |{"id":332562,"title":"Narodziny gwiazdy","original_title":"A Star Is Born",
        | "release_date":"2018-10-03","department":"Directing","job":"Director","popularity":20.0}
        |]}""".stripMargin,
      "/movie/332562/external_ids" -> """{"id":332562,"imdb_id":"tt1517451"}""",
      "/movie/332562"              -> """{"id":332562,"title":"Narodziny gwiazdy","original_title":"A Star Is Born","release_date":"2018-10-03","runtime":136}"""
    )), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), tmdb)

    val existing = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Ladies Night - Narodziny gwiazdy"), director = Seq("Bradley Cooper"))))
    val resolved = service.resolveStagingRecord("Ladies Night - Narodziny gwiazdy", Some(2018), existing)

    resolved.flatMap(_.tmdbId) shouldBe Some(332562)
  }
}
