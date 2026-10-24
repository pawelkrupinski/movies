package integration

import services.identity._
import services.identity.adapters.{FamilyAnswerStore, InMemoryTmdbDocuments, PosterAnswerStore, TmdbIdentityLookups, TmdbKind}
import tools._

import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Fills what one country's [[UnmatchedClusters]] capture lacks after a change that asks the agreement stage new questions
 * of the SAME clusters — a family's question, an agreed IMDb id's TMDB find, a poster's hash — answered as the capture
 * answers them (the experiment's cache, else live; posters as [[CachedPosters]] keeps them), without resolving the whole
 * corpus again — and, with `KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY`, the TMDB searches and records the stage's own resolves
 * ask beyond the capture (a fill rule resolving a cluster alone searches what the whole corpus's resolve never did),
 * asked live. The model's decisions and the answers captured stay as they are: a change to those is a re-capture
 * ([[UnmatchedClustersCapture]]). `scripts.IdentityCapture` runs it when a capture's decisions are current:
 *
 * {{{
 *   java -cp <it classpath> integration.UnmatchedClustersFill de --capture test/resources/fixtures/identity-unmatched \
 *     --agreement-cache <dir> [--poster-cache <dir>] [--recording <dir> [--listing-words]]
 * }}}
 *
 * `--recording` (a recording's fixtures directory: `corpus/cinema-scrapes-<cc>.json.gz` and `enrichment-<cc>/`) answers
 * from its TMDB tree the film overviews the synopsis evidence asks ([[IdentityLookups.overview]]), the casts the cast
 * and synopsis evidence ask ([[IdentityLookups.cast]]) and the records' original languages and genres that the capture
 * holds none of ([[UnmatchedClusters.withRecorded]]); with `--listing-words`, which only the recording the capture was taken from may be given for, the
 * captured listings also gain the words their own synopses capitalise ([[Listing.words]]), read off its corpus as a
 * capture reads them.
 *
 * Exit: 0 filled in place, and its replay asks nothing it lacks; 1 not (no `<cc>.json.gz` to fill, a question left
 * open); 2 usage.
 */
object UnmatchedClustersFill {

  import UnmatchedClustersCapture.check

  val commandLine: CommandLine = CommandLine("UnmatchedClustersFill",
    "Answers in place what one country's unmatched-cluster capture lacks, its decisions kept.",
    CommandLine.Value("capture", "dir", "the directory holding <cc>.json.gz, filled in place (required)"),
    CommandLine.Value("agreement-cache", "dir", "the families' answer cache, <host>/<sha256 of \"METHOD url body\"> (required)"),
    CommandLine.Value("poster-cache", "dir", "the posters kept between runs (default: KINOWO_IDENTITY_POSTER_CACHE, else target/identity-posters)"),
    CommandLine.Value("recording", "dir", "a recording's fixtures directory (corpus/, enrichment-<cc>/): the TMDB overviews the capture lacks, answered from it"),
    CommandLine.Flag("listing-words", "the listings' synopsis words read off --recording's corpus: only the recording the capture was taken from"))
    .withPositionals(CommandLine.Positional("country", "the country filled (required)"))

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val country   = UnmatchedClustersCapture.countryOf(arguments)
    val path      = arguments.requiredPath("capture").resolve(s"${country.code}.json.gz")
    val cache     = arguments.requiredPath("agreement-cache")
    if (!java.nio.file.Files.exists(path)) { System.err.println(s"[UnmatchedClustersFill] no $path to fill — capture it first"); sys.exit(1) }
    val recording = arguments.path("recording")
    if (arguments.flag("listing-words") && recording.isEmpty) arguments.fail("--listing-words reads --recording's corpus")
    Try(fill(settings.ProcessConfiguration.resolve(), country, path, cache, arguments.path("poster-cache"), recording,
      arguments.flag("listing-words"))).failed.foreach { e =>
      System.err.println(s"[${country.code}] fill FAILED: ${e.getMessage}")
      if (!e.isInstanceOf[UnmatchedClustersCapture.Refused]) e.printStackTrace()
      sys.exit(1)
    }
  }

  private def fill(configuration: settings.ProcessConfiguration, country: models.Country, path: java.nio.file.Path, agreementCache: java.nio.file.Path,
                   posterCache: Option[java.nio.file.Path], recording: Option[java.nio.file.Path], listingWords: Boolean): Unit = {
    var capture  = UnmatchedClusters.read(path)
    // what the capture's replay already left unanswered: only a live fill answers those, and only it is held to
    val unansweredBefore = if (configuration.identityLiveGaps.isDefined) Set.empty[String] else UnmatchedClusters.replay(capture).unanswered
    val recorded = recording.map(dir => RecordedCorpus.open(country, dir.resolve("corpus").resolve(s"cinema-scrapes-${country.code}.json.gz"),
      settings.FixtureRoot(dir)))
    if (listingWords) recorded.foreach { opened =>
      val normalizer = services.movies.TitleNormalizer.forCountry(country)
      val words = opened.rows.flatMap(row => row.films.map(Listing.of(row.cinema, _, normalizer))).map(l => l.key -> l.words).toMap
      val read  = capture.listings.count(l => words.get(l.key).exists(_.nonEmpty))
      capture = capture.copy(listings = capture.listings.map(l => words.get(l.key).fold(l)(w => l.copy(words = w))))
      println(s"[${country.code}] fill: $read of ${capture.listings.size} listing(s) given their synopsis words from ${recording.get}")
    }
    // the overviews the synopsis evidence asks, off the recording's TMDB tree
    val overviewsOf = recorded.map { opened =>
      val client = new clients.TmdbClient(opened.fetch, apiKey = Some(settings.TmdbApiKey(IdentityShadow.StubTmdbKey)),
        language = country.language)
      new TmdbIdentityLookups(client, new services.enrichment.ImdbClient(opened.fetch), Nil, opened.gaps)
    }
    // the casts, original languages and genres a capture taken without them lacks, off the same tree
    def recordedOf(tree: TmdbIdentityLookups, asked: Iterable[Int]): Unit = {
      val before = capture
      capture = UnmatchedClusters.withRecorded(capture, asked, id => tree.cast(id).toOption.flatten, id => tree.film(id).toOption.flatten)
      def held(c: UnmatchedClusters.Capture)(has: IdentityMeasures.Film => Boolean) = c.films.count(_._2.exists(has))
      println(s"[${country.code}] fill: ${capture.casts.count(_._2.isDefined) - before.casts.count(_._2.isDefined)} cast(s), " +
        s"${held(capture)(_.originalLanguage.isDefined) - held(before)(_.originalLanguage.isDefined)} original language(s) and " +
        s"${held(capture)(_.genreBits != 0) - held(before)(_.genreBits != 0)} record(s)' genres read from the recording")
    }
    overviewsOf.foreach(recordedOf(_, Nil))
    val clock    = new MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z"))
    val docs     = new InMemoryTmdbDocuments
    docs.put(TmdbKind.Family, capture.families.toSeq)
    val store    = new FamilyAnswerStore(docs, clock)
    val cache    = new ExperimentCacheFetch(agreementCache)
    val families = UnmatchedClusters.familiesOf(country)
    val sources  = LiveFamilySources.of(cache, country, families)
    val tmdb  = configuration.identityLiveGaps.map(key => new clients.TmdbClient(new RealHttpFetch(), apiKey = Some(settings.TmdbApiKey(key.tmdbKey)),
      language = country.language))
    val finds = new java.util.concurrent.ConcurrentHashMap[String, Option[Int]](capture.finds.asJava)
    val tmdbOf: String => Answer[Option[Int]] = imdb => Option(finds.get(imdb)).map(Answer.Known(_)).getOrElse {
      tmdb.flatMap(client => Try(client.findByImdbId(imdb).map(_.id)).toOption)
        .fold[Answer[Option[Int]]](Answer.Unknown) { found => finds.put(imdb, found); Answer.Known(found) }
    }
    val posterStore = new PosterAnswerStore(store, clock)
    val posters     = CachedPosters.of(configuration, country, posterCache)
    // TMDB's own answers to what the stage's resolves ask beyond the capture (a fill rule's resolve of a cluster alone)
    val live        = tmdb.map(client => new TmdbIdentityLookups(client, new services.enrichment.ImdbClient(new RealHttpFetch()), Nil))
    var lookups     = new UnmatchedClusters.Replay(capture)
    val catalogue   = new CatalogueFill(store, clock, cache)
    def agree() = UnmatchedClusters.agree(capture.decisions, capture.listings, { lookups = new UnmatchedClusters.Replay(capture); lookups }, families.map(f => f -> store.answers(f)).toMap,
      store.version, tmdbOf, services.movies.TitleNormalizer.forCountry(country), posterStore,
      modules.wiring.IdentityProjectionWiring.identities(country.code), catalogue.answers,
        modules.wiring.IdentityProjectionWiring.listedOn(country.code))
    // a question its host never answers is given up on, kept in the fixture as unreachable, and asked again next time
    val asked = new AgreementQuestionRounds(store, sources, s"${country.code} fill").run(() => agree())(o => Question.families(o.stage.wanted),
      pending = o => o.stage.wanted.exists { case Question.Poster(_) | Question.Catalogue(_) | Question.Find(_) => true; case _ => false } ||
        (live.isDefined && (!lookups.queries.isEmpty || !lookups.films.isEmpty)) ||
        (overviewsOf.isDefined && lookups.overviews.asScala.exists(id => !capture.overviews.contains(id))) ||
        (overviewsOf.isDefined && lookups.casts.asScala.exists(id => !capture.casts.contains(id))),
      beside = { o =>
        val (wantedPosters, wantedCatalogue, wantedFinds) = (o.stage.wanted.toSeq.collect { case Question.Poster(q) => q },
          o.stage.wanted.collect { case Question.Catalogue(q) => q }, o.stage.wanted.collect { case Question.Find(imdb) => imdb })
        println(s"[${country.code}] fill: ${wantedPosters.size} poster(s), ${wantedCatalogue.size} catalogue question(s), ${wantedFinds.size} find(s)")
        posters.file(posterStore, wantedPosters)
        catalogue.file(wantedCatalogue)
        wantedFinds.foreach(tmdbOf)
        live.foreach { tmdbLookups =>
          val queries = lookups.queries.asScala.toSeq.flatMap(query => tmdbLookups.candidates(query).toOption.map(query -> _))
          val films   = lookups.films.asScala.toSeq.flatMap(id => tmdbLookups.film(id).toOption.map(id -> _))
          println(s"[${country.code}] fill: ${queries.size} TMDB search(es), ${films.size} TMDB record(s) answered live")
          capture = capture.copy(queries = capture.queries ++ queries, films = capture.films ++ films)
        }
        overviewsOf.foreach { tree =>
          // an overview the tree does not hold is none: asked again, it would be no more known
          val read = lookups.overviews.asScala.toSeq.filterNot(capture.overviews.contains).map(id => id -> tree.overview(id).toOption.flatten)
          println(s"[${country.code}] fill: ${read.size} TMDB overview(s) read from the recording, ${read.count(_._2.isDefined)} held")
          capture = capture.copy(overviews = capture.overviews ++ read)
          // and the cast of each film the evidence reads, where the capture holds none: a synopsis naming a film's cast
          // names it ([[agreement.SynopsisEvidence]]), as the cast evidence reads it; a cast the tree holds none of is
          // filed unknown, asked again it would be no more known
          val asked = lookups.casts.asScala.toSeq ++ read.map(_._1)
          recordedOf(tree, asked)
          capture = capture.copy(casts = asked.filterNot(capture.casts.contains).map(_ -> Option.empty[Seq[String]]).toMap ++ capture.casts)
        }
      })
    val (outcome, rounds) = (asked.outcome, asked.rounds)
    check(outcome.model.unknownQueries == 0, s"the model's replay left ${outcome.model.unknownQueries} search(es) unanswered")
    val filed = docs.get(TmdbKind.Family, docs.fetchedBefore(TmdbKind.Family, Long.MaxValue).map(_._1))
    UnmatchedClusters.write(path, capture.copy(families = filed, finds = finds.asScala.toMap, unreachable = asked.unreachable))
    val reread   = UnmatchedClusters.read(path)
    val replayed = UnmatchedClusters.replay(reread)
    val unasked  = UnmatchedClusters.unasked(reread, Question.families(replayed.stage.wanted))
    check(unasked.isEmpty, s"the filled capture replays asking what it never answered: ${unasked.take(5).mkString("; ")}")
    def wanting(what: String)(kind: PartialFunction[Question, Any]) = {
      val open = replayed.stage.wanted.collect(kind)
      check(open.isEmpty, s"the filled capture replays wanting $what: ${open.take(5).mkString("; ")}")
    }
    wanting("finds") { case Question.Find(imdb) => imdb }
    wanting("posters") { case Question.Poster(q) => q }
    wanting("catalogue answers") { case Question.Catalogue(q) => q }
    val unanswered = replayed.unanswered -- unansweredBefore
    check(unanswered.isEmpty, s"the filled capture replays leaving TMDB questions unanswered: ${unanswered.take(5).mkString("; ")}")
    println(s"[${country.code}] filled after $rounds round(s): ${filed.size} family answers, ${finds.size} finds, ${asked.unreachable.size} unreachable")
  }
}
