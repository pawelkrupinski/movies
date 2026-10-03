package integration

import models.Country
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{CinemaScraper, PreScrapedCinemaScraper}
import services.movies.{StoredMovieRecord, TitleNormalizer}
import services.readmodel.FilmSlugs
import tools._

import java.util.concurrent.Executors
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Random, Try}

/**
 * The convergence legs' identity claims, over the HARD CLUSTERS only, in minutes rather than hours.
 *
 * Every identity regression the country convergence legs caught lived in a few dozen films —
 * franchise siblings joined by arrival order, a contested film address decided by which listing came
 * first, a decorated re-release year read as a second film, a programme-prefixed listing moving on an
 * identical rescrape. `tools.HardClusters` extracts those clusters (and the shapes around them) from
 * the recorded corpora into a few hundred listings, and this spec runs the REAL pipeline over them —
 * real Mongo, the listing intake, the identity projection and the read-model projection — with every
 * HTTP answer the clusters need replayed from one checked-in file (`tools.RecordedResponses`).
 *
 * Per country it asserts:
 *
 *   1. ORDER-INDEPENDENCE — three seeded arrival orders (cinemas shuffled, each cinema's films
 *      shuffled) and a SPLIT arrival (half the cinemas, projected, then the rest) come out as the
 *      same films: identity, title, year, tmdbId, imdbId, cinemas and the film's public address.
 *   2. THE RIGHT ANSWER — the films checked in for the clusters (`expected-hard-clusters-<cc>.txt`).
 *   3. NO CHURN — a further projection, and two identical rescrapes each projected, write nothing.
 *   4. AN OUTAGE IS NOT AN ANSWER — with TMDB failing half its requests, then back, the films come
 *      out as the undisturbed boot's (their IMDb ids reported, not asserted).
 *
 * Each pass runs in its own uniquely-named database (`IsolatedMongoDatabase`), dropped
 * in `afterAll`.
 *
 * RECORD MODE re-captures the responses file after the fixture grows (see
 * `scripts/hard-clusters.sh`): with `KINOWO_HARD_CLUSTERS_RECORD=1` and
 * `KINOWO_FIXTURE_ROOT` naming a directory holding the countries' `enrichment-<cc>`
 * trees, every request the file does not already answer is asked of the tree, and the
 * file is written back with the old answers AND the new (`RecordedResponses.recording`).
 */
class HardClusterConvergenceIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  /** Where the countries' `enrichment-<cc>` trees live: KINOWO_FIXTURE_ROOT when
   *  `scripts/hard-clusters.sh` names one, else the repository's own. */
  private val FixtureRoot = configuration.fixtureRoot
  private val Recording = configuration.hardClusterRecording.value

  /** Fixed, so an order dependence fails the same way on every run. */
  private val OrderSeed = 0x2026_09_24L
  private val Permutations = 3

  private val storages = mutable.ListBuffer.empty[ConvergenceStorage]

  override def afterAll(): Unit = {
    if (Recording) { recordEachSpellingAlone(); recordIdentityLookups() }
    storages.synchronized(storages.foreach(s => Try(s.close())))
    if (Recording) responses.foreach { case (country, r) =>
      val path = r.write(RecordedResponses.pathFor(country.code))
      println(s"[${country.code}] recorded ${r.size} responses to $path")
    }
    super.afterAll()
  }

  /** `KINOWO_HARD_CLUSTERS_COUNTRIES=uk,us` narrows a local run to those countries. */
  private val countries: Seq[Country] = {
    val only = configuration.hardClusterCountries.map(_.value)
    Country.all.filter(c => CorpusFixture.exists(HardClusters.corpusKey(c)) && only.forall(_.contains(c)))
  }

  private lazy val responses: Map[Country, RecordedResponses] = countries.map { country =>
    country -> (
      if (Recording) RecordedResponses.recording(new FallbackHttpFetch(Seq(
        "tree"       -> new clients.tools.FakeHttpFetch(s"enrichment-${country.code}", strict = true, foldYear = false, root = FixtureRoot),
        "unrecorded" -> new clients.tools.FailingHttpFetch(404))), prior = Some(RecordedResponses.pathFor(country.code)))
      else RecordedResponses.replaying(RecordedResponses.pathFor(country.code)))
  }.toMap

  /** One film as the public sees it, keyed by what does NOT follow arrival order: the
   *  stored key, never the opaque id (which is minted from whichever key a row was first
   *  created under — see `FilmId`). */
  private final case class Film(key: String, title: String, year: Option[Int], tmdbId: Option[Int],
                                imdbId: Option[String], cinemas: Seq[String], address: Option[String]) {
    override def toString: String =
      s"$title (${year.getOrElse("—")}) tmdb=${tmdbId.getOrElse("—")} imdb=${imdbId.getOrElse("—")} " +
      s"address=${address.getOrElse("unprojected")} cinemas=${cinemas.mkString("[", ", ", "]")}"
  }

  private final class Pass(val label: String, val wiring: ArchiveReplayWiring, val scrapers: Seq[CinemaScraper])

  private def wiringFor(country: Country, label: String, wrap: HttpFetch => HttpFetch = identity,
                        movableClock: Option[MutableClock] = None): (ArchiveReplayWiring, ConvergenceStorage) = {
    val storage = ConvergenceStorage.mongo(mongoTarget, s"hc-${country.code}-$label", TitleNormalizer.forCountry(country),
      services.movies.MovieChangeStream.Debounce.forCountry(country))
    storages.synchronized(storages += storage)
    val w = FetchReplayWiring(country, storage, CorpusFixture.read(HardClusters.corpusKey(country)), wrap(responses(country)),
      FixtureRoot, clock = movableClock.getOrElse(java.time.Clock.fixed(TestWiring.FixedInstant, java.time.ZoneOffset.UTC)),
      // The outage pass refuses on purpose; its retries need not sleep through it.
      retrySleep = if (movableClock.isDefined) (_: Long) => () else Thread.sleep, environment = configuration.env)
    (w, storage)
  }

  /** The cinemas in a seeded order, each serving its films in a seeded order — the two
   *  orders production varies (the reaper's due-time queue, a site reordering its list). */
  private def arrivals(w: ArchiveReplayWiring, rnd: Random): Seq[CinemaScraper] =
    rnd.shuffle(w.archivedListings.toSeq.sortBy(_._1.displayName)).map { case (cinema, films) =>
      PreScrapedCinemaScraper.replaying(cinema, rnd.shuffle(films.toList))
    }

  /** Production's arrival: each cinema's scrape lands in the identity model's intake through the
   *  production runner. */
  private def arrive(w: ArchiveReplayWiring, scrapers: Seq[CinemaScraper]): Unit =
    scrapers.foreach(scraper => Try(w.cinemaScrapeRunner.run(scraper)))

  /** Projections until one writes nothing — the rest production's projection interval reaches, as the
   *  venue pages and ids a projection's enrichment fetched are taken in by the next — then the read model. */
  private def settle(w: ArchiveReplayWiring): Unit = {
    Iterator.continually(w.projectIdentity()).take(SettleProjections).find(_.wroteNothing)
    project(w)
  }
  private val SettleProjections = 5

  /** The read model every claim reads, from a COMPLETE scan. Passes boot side by side against
   *  one local Mongo, and under a loaded `itAll` a page's side read can time out: the scan then
   *  skips that page, so its films are simply not projected yet — which production's next sweep
   *  repairs and nothing here would. A read-model write can time out too, leaving a card's
   *  document without its screenings. Reconciling again until a scan completes with every row
   *  projected is that next sweep; one that never does fails as what it is, not as a missing
   *  showtime. */
  private def project(w: ArchiveReplayWiring): Unit = {
    val attempts = Iterator.continually(w.readModelProjector.reconcile()).take(ReconcileAttempts)
    if (!attempts.contains(true))
      throw new IllegalStateException(s"read-model reconcile: no complete, wholly projected sweep in $ReconcileAttempts attempts — " +
        "Mongo reads or read-model writes kept failing, so the projected read model is not the corpus's")
  }
  private val ReconcileAttempts = 3

  private def films(w: ArchiveReplayWiring, normalizer: TitleNormalizer): Seq[Film] = {
    val records   = w.movieRepository.findAll()
    val projected = w.readModelRepository.findAllMovies()
    val slugs     = FilmSlugs(projected)
    records.map { r =>
      Film(r.key(normalizer), r.title, r.year, r.record.tmdbId, r.record.imdbId,
        r.record.cinemaData.keySet.map(_.displayName).toSeq.sorted, slugs.slugFor(r.id.value))
    }.sortBy(f => (f.key, f.title))
  }

  private def boot(country: Country, label: String, seed: Long, split: Boolean): (Pass, Seq[Film]) = {
    val (w, _) = wiringFor(country, label)
    val rnd = new Random(seed)
    val scrapers = arrivals(w, rnd)
    if (split) {
      // Half the cinemas land, settle and project as a finished state; the rest arrive
      // on top of it — the corpus a worker holds after a partial tick, then the next.
      val (first, rest) = scrapers.splitAt(scrapers.size / 2)
      arrive(w, first); settle(w)
      arrive(w, rest);  settle(w)
    } else {
      arrive(w, scrapers); settle(w)
    }
    (new Pass(label, w, scrapers), films(w, TitleNormalizer.forCountry(country)))
  }

  /** Every pass of every country, concurrently — each in its own database and wiring. */
  private lazy val booted: Map[Country, Seq[(Pass, Seq[Film])]] = {
    val pool = Executors.newFixedThreadPool(8)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)
    try {
      val jobs = countries.flatMap { c =>
        (0 until Permutations).map(i => c -> (() => boot(c, s"p$i", OrderSeed + i, split = false))) :+
          (c -> (() => boot(c, "split", OrderSeed, split = true)))
      }
      val done = Await.result(Future.traverse(jobs) { case (c, job) => Future(c -> job()) }, 10.minutes)
      done.groupMap(_._1)(_._2)
    } finally pool.shutdown()
  }

  /** RECORD MODE: also ask the identity resolver's query set (`IdentityLookupSweep`) of each
   *  country's clusters, so the responses file answers the phase-1 gate
   *  (`IdentityQueryCoverageIntegrationSpec`) as well as this spec — whatever the tree holds. */
  private def recordIdentityLookups(): Unit = countries.foreach { country =>
    val storage = ConvergenceStorage.mongo(IntegrationMongoTarget.from(_root_.settings.ProcessConfiguration.resolve()).get,
      s"hc-${country.code}-identity", TitleNormalizer.forCountry(country))
    storages.synchronized(storages += storage)
    val w = FetchReplayWiring(country, storage, CorpusFixture.read(HardClusters.corpusKey(country)), responses(country))
    println(s"[${country.code}] identity resolver lookups: ${IdentityLookupSweep.over(w)}")
  }

  /**
   * RECORD MODE ONLY: boot every distinct spelling in the fixture ON ITS OWN, so the file
   * also holds the answers a spelling needs when nothing folds it onto its siblings.
   *
   * The passes above record only what the CURRENT code asks, and a regression is exactly
   * a change in what gets asked: revert the fix that folds "Mockingjay - Part 1 (2026)"
   * onto "Part 1" and the decorated row resolves on its own, through searches the fixed
   * code never made — which a replay would answer 404, leaving the row unresolved and the
   * bug invisible. Resolving each spelling alone records those searches too.
   */
  private def recordEachSpellingAlone(): Unit = {
    val pool = Executors.newFixedThreadPool(8)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)
    try {
      val jobs = countries.flatMap { country =>
        CorpusFixture.read(HardClusters.corpusKey(country))
          .flatMap(row => row.films.map(row.cinema -> _))
          .distinctBy { case (_, film) => (film.movie.title.trim.toLowerCase, film.movie.releaseYear) }
          .zipWithIndex.map { case ((cinema, film), i) => () =>
            val (w, storage) = wiringFor(country, s"s$i")
            try {
              arrive(w, Seq(PreScrapedCinemaScraper.replaying(cinema, Seq(film))))
              settle(w)
            } finally storage.close()
          }
      }
      Await.result(Future.traverse(jobs)(job => Future(Try(job()))), 20.minutes)
      println(s"[hard-clusters] recorded ${jobs.size} spellings on their own")
    } finally pool.shutdown()
  }

  /**
   * TMDB DOWN for part of a boot: half its URLs answer 503 while every venue lands and is projected,
   * and then it comes back. A 5xx is "not now", never "no such film": the model holds the question
   * open, and the lookup fill asks it again — so once TMDB answers and a fill round has run, the
   * corpus must come out as the undisturbed reference did.
   *
   * Returns the films matched in the reference that the outage left unmatched (reported), and the
   * films after the recovery.
   */
  private def outage(country: Country): (Seq[String], Seq[Film]) = {
    val down  = new java.util.concurrent.atomic.AtomicBoolean(true)
    val clock = new MutableClock(TestWiring.FixedInstant)
    def flaky(inner: HttpFetch): HttpFetch = new HttpFetch {
      private def check(url: String): Unit =
        if (down.get && url.contains("themoviedb.org") && (url.## & 1) == 0)
          throw new HttpStatusException(503, "GET", url, retryAfter = None)
      override def get(url: String): String = { check(url); inner.get(url) }
      override def get(url: String, headers: Map[String, String]): String = { check(url); inner.get(url, headers) }
      override def getBytes(url: String): Array[Byte] = { check(url); inner.getBytes(url) }
      override def post(url: String, body: String, contentType: String): String = { check(url); inner.post(url, body, contentType) }
    }
    val normalizer = TitleNormalizer.forCountry(country)
    val reference  = booted(country).head._2.filter(_.tmdbId.isDefined).map(_.key).toSet
    val (w, _)     = wiringFor(country, "outage", flaky, Some(clock))
    arrive(w, arrivals(w, new Random(OrderSeed)))
    settle(w)
    val unmatched = films(w, normalizer).collect { case f if f.tmdbId.isEmpty && reference.contains(f.key) => f.toString }
    // TMDB answers again; the fill's next round asks what the outage left open, as production's
    // refresh schedule does, and the projection takes the answers in.
    down.set(false)
    clock.advance(java.time.Duration.ofHours(1))
    w.shadowLookupFill.round()
    settle(w)
    (unmatched, films(w, normalizer))
  }

  private lazy val outages: Map[Country, (Seq[String], Seq[Film])] = countries.map(c => c -> outage(c)).toMap

  /** What moved between two passes, by film key — named so a failure says WHICH cluster. */
  private def diff(a: Seq[Film], b: Seq[Film], la: String, lb: String): Seq[(String, String)] = {
    val byA = a.groupBy(_.key); val byB = b.groupBy(_.key)
    (byA.keySet ++ byB.keySet).toSeq.sorted.flatMap { key =>
      ((byA.getOrElse(key, Nil), byB.getOrElse(key, Nil)) match {
        case (x, y) if x == y => None
        case (Nil, y)         => Some(s"  only in $lb [$key]: ${y.mkString("; ")}")
        case (x, Nil)         => Some(s"  only in $la [$key]: ${x.mkString("; ")}")
        case (x, y)           => Some(s"  $key\n    $la: ${x.mkString("; ")}\n    $lb: ${y.mkString("; ")}")
      }).map(key -> _)
    }
  }

  // The known, unfixed divergences — see `tools.HardClusterExemptions`, where a unit guard
  // holds them on every push.
  private val KnownSplitArrivalDivergences = HardClusterExemptions.SplitArrivalDivergences
  private val KnownRescrapeChurn           = HardClusterExemptions.RescrapeChurn

  countries.foreach { country =>
    val name = country.displayName

    s"the $name hard clusters" should "come out as the same films whatever order, and however split, they arrive in" in {
      val passes = booted(country)
      val (reference, refFilms) = passes.head
      info(s"$name: ${refFilms.size} films from ${reference.scrapers.size} cinemas, " +
           s"${refFilms.count(_.tmdbId.isDefined)} resolved, ${refFilms.count(_.address.isDefined)} projected; " +
           s"${responses(country).size} recorded responses, ${responses(country).misses} unrecorded request(s)")
      refFilms should not be empty
      // `KINOWO_HARD_CLUSTERS_DUMP=1` prints every pass's films — the first thing to read
      // when a cluster moves.
      if (configuration.hardClusterDump.value)
        passes.foreach { case (p, fs) => println(s"[${country.code}] ${p.label}:\n  ${fs.mkString("\n  ")}") }
      val known = KnownSplitArrivalDivergences.getOrElse(country.code, Set.empty)
      def isKnown(pass: Pass, key: String) = pass.label == "split" && known.contains(key)
      val divergences = passes.tail.flatMap { case (pass, fs) =>
        val d = diff(refFilms, fs, reference.label, pass.label).filterNot { case (key, _) => isKnown(pass, key) }
        if (d.isEmpty) None
        else Some(s"${reference.label} vs ${pass.label} (seed base 0x${OrderSeed.toHexString}):\n${d.map(_._2).take(15).mkString("\n")}")
      }
      val stillDiverging = passes.tail.flatMap { case (pass, fs) =>
        diff(refFilms, fs, reference.label, pass.label).collect { case (key, _) if isKnown(pass, key) => key }
      }.toSet
      val fixed = known -- stillDiverging
      withClue(s"$name: ${divergences.size} pass(es) diverged:\n${divergences.mkString("\n")}\n") {
        divergences shouldBe empty
      }
      withClue(s"$name: ${fixed.mkString(", ")} no longer diverge(s) on the split arrival — delete the entry from " +
               "HardClusterExemptions.SplitArrivalDivergences so the exemption cannot hide the next regression: ") {
        fixed shouldBe empty
      }
    }

    /* The claims above are all RELATIVE — every pass agrees with every other, a settle agrees
     * with itself — and a relative claim holds just as well over a wrong answer. A franchise
     * sibling folded onto the first film, a re-release given the wrong year, two films under
     * one title collapsed into one: each is deterministic, so every pass makes it identically
     * and every one of those claims stays green. These clusters are exactly the films where
     * those mistakes have been made before, and their inputs are all checked in, so their
     * RIGHT answer can be too. */
    it should "come out as the films checked in for them — the clusters' answer, not merely a consistent one" in {
      val (_, films) = booted(country).head
      val path     = HardClusters.expectedFilmsPath(country)
      val actual   = films.map(f => s"${f.key}\t$f").mkString("", "\n", "\n")
      // RECORD mode is how the clusters GROW (`scripts/hard-clusters.sh`, and the ratchet a red
      // convergence leg runs): new films are the point there, so the file is rewritten with
      // the responses and ships beside them for review, never compared against.
      if (Recording) {
        java.nio.file.Files.writeString(path, actual)
        info(s"$name: recording — rewrote $path")
      } else if (!java.nio.file.Files.exists(path)) {
        java.nio.file.Files.writeString(path, actual)
        fail(s"no expected films for $name — wrote $path. Review it, commit it, and re-run.")
      }
      if (Recording) succeed else {
      val expected = java.nio.file.Files.readString(path)
      val byKey    = (text: String) => text.linesIterator.filter(_.nonEmpty).map(l => l.takeWhile(_ != '\t') -> l).toSeq.groupMap(_._1)(_._2)
      val (want, got) = (byKey(expected), byKey(actual))
      val moved = (want.keySet ++ got.keySet).toSeq.sorted.flatMap { key =>
        (want.getOrElse(key, Nil), got.getOrElse(key, Nil)) match {
          case (w, g) if w == g => None
          case (Nil, g)         => Some(s"  + ${g.mkString("; ")}")
          case (w, Nil)         => Some(s"  - ${w.mkString("; ")}")
          case (w, g)           => Some(s"  ~ was ${w.mkString("; ")}\n    now ${g.mkString("; ")}")
        }
      }
      withClue(s"$name's hard clusters no longer come out as $path says (${moved.size} film(s)). If the change is " +
               s"intended, delete the file, re-run to regenerate it, and commit it with the change:\n" +
               s"${moved.take(20).mkString("\n")}\n") {
        moved shouldBe empty
      }
      }
    }

    it should "serve every listing of every pass under the one film that holds it" in {
      val listings = ServedCorpusInvariants.listings(CorpusFixture.read(HardClusters.corpusKey(country)))
      val problems = booted(country).flatMap { case (pass, _) =>
        val w = pass.wiring
        ServedCorpusInvariants.violations(listings, w.movieRepository.findAll(), w.readModelRepository.findAllMovies(),
          w.readModelRepository.findAllScreenings(), TitleNormalizer.forCountry(country), country = Some(country))
          .map(p => s"pass ${pass.label}: $p")
      }
      withClue(s"$name's served hard clusters do not match their listings:\n${problems.mkString("\n")}\n") {
        problems shouldBe empty
      }
    }

    it should "come out as the undisturbed boot's films once TMDB is back after an outage" in {
      val (unmatched, recovered) = outages(country)
      val (reference, refFilms)  = booted(country).head
      if (unmatched.nonEmpty)
        info(s"$name: ${unmatched.size} film(s) unmatched while TMDB failed (held open, not concluded):\n  " +
             unmatched.take(12).mkString("\n  "))
      // The films themselves — identity, title, year, TMDB match, cinemas — must come back. An IMDb id the
      // id recovery looked up during the outage is REPORTED, not asserted: that recovery runs once per
      // match and is not re-asked when TMDB failed under it (as before the cut-over, 2026-09-25).
      def identity(fs: Seq[Film]) = fs.map(_.copy(imdbId = None, address = None))
      val idDrift = diff(refFilms, recovered, reference.label, "outage-recovered").map(_._2)
      if (idDrift.nonEmpty) info(s"$name: ${idDrift.size} film(s) differ after the recovery (IMDb ids included):\n${idDrift.take(12).mkString("\n")}")
      val drift = diff(identity(refFilms), identity(recovered), reference.label, "outage-recovered").map(_._2)
      withClue(s"$name: ${drift.size} film(s) not back to the undisturbed boot after TMDB recovered:\n${drift.take(12).mkString("\n")}\n") {
        drift shouldBe empty
      }
    }

    it should "not rewrite a settled film on a further projection or an identical rescrape" in {
      val (pass, _) = booted(country).head
      val w          = pass.wiring
      val normalizer = TitleNormalizer.forCountry(country)
      // The known churners are read apart from the rest: left out of every check below,
      // and required to still churn (see the end), so fixing one retires its entry.
      val knownChurn = KnownRescrapeChurn.getOrElse(country.code, Set.empty)
      def isKnown(key: String) = knownChurn.contains(key)
      def allRecords = w.movieRepository.findAll().sortBy(r => (r.key(normalizer), r.title))
      def records: Seq[StoredMovieRecord] = allRecords.filterNot(r => isKnown(r.key(normalizer)))
      def knownRecords: Seq[StoredMovieRecord] = allRecords.filter(r => isKnown(r.key(normalizer)))
      def keys = w.movieCache.snapshot().map(r => (r.title, r.year))
        .filterNot { case (title, year) => isKnown(StoredMovieRecord.keyFor(title, year, normalizer)) }.toSet
      val knownBefore = knownRecords

      val problems = mutable.ListBuffer.empty[String]
      val settledRecords = records
      val settledKeys    = keys

      val again = w.projectIdentity()
      if (!again.wroteNothing) problems += s"a projection over the settled corpus wrote ${again.written}, retired ${again.retired}"
      val afterProjection = records
      if (afterProjection != settledRecords)
        problems += s"a projection over the settled corpus changed records:\n${changed(settledRecords, afterProjection, normalizer)}"

      (1 to 2).foreach { tick =>
        val before = records
        arrive(w, new Random(OrderSeed + 100 + tick).shuffle(pass.scrapers))
        val projected = w.projectIdentity()
        if (!projected.wroteNothing) problems += s"rescrape $tick's projection wrote ${projected.written}, retired ${projected.retired}"
        val after = records
        if (after != before) problems += s"rescrape $tick changed records:\n${changed(before, after, normalizer)}"
        val drift = (keys -- settledKeys).map(k => s"+$k") ++ (settledKeys -- keys).map(k => s"-$k")
        if (drift.nonEmpty) problems += s"rescrape $tick moved keys: ${drift.toSeq.sorted.take(10).mkString(", ")}"
      }
      if (knownChurn.nonEmpty && knownRecords == knownBefore)
        problems += s"${knownChurn.mkString(", ")} no longer churn(s) — delete the entry from HardClusterExemptions.RescrapeChurn " +
                    "so the exemption cannot hide the next regression"
      withClue(s"$name did not stay settled:\n${problems.mkString("\n")}\n") {
        problems shouldBe empty
      }
    }
  }

  /** Records that appeared, vanished or changed between two reads, by stored key. */
  private def changed(a: Seq[StoredMovieRecord], b: Seq[StoredMovieRecord], normalizer: TitleNormalizer): String = {
    val byA = a.map(r => r.key(normalizer) -> r).toMap
    val byB = b.map(r => r.key(normalizer) -> r).toMap
    (byA.keySet ++ byB.keySet).toSeq.sorted.flatMap { k =>
      (byA.get(k), byB.get(k)) match {
        case (Some(x), Some(y)) if x == y => None
        case (Some(x), Some(y)) =>
          val fields = x.record.productElementNames.zip(x.record.productIterator.zip(y.record.productIterator))
            .collect { case (n, (p, q)) if p != q => s"$n: $p -> $q" }.toSeq
          val head = if (x.title != y.title || x.year != y.year) Seq(s"key ${x.title}/${x.year} -> ${y.title}/${y.year}") else Nil
          Some(s"  ~ $k ${(head ++ fields).take(6).mkString("; ")}")
        case (Some(x), None) => Some(s"  - ${x.title} (${x.year.getOrElse("—")})")
        case (None, Some(y)) => Some(s"  + ${y.title} (${y.year.getOrElse("—")})")
        case _               => None
      }
    }.take(12).mkString("\n")
  }
}
