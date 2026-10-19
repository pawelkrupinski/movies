package integration

import tools.SpecClock.given

import tools.SpecTimeouts

import models.Country
import services.cinemas.common.{CinemaScraper, PreScrapedCinemaScraper}
import services.movies.{StoredMovieRecord, TitleNormalizer}
import services.readmodel.FilmSlugs
import tools._

import java.util.concurrent.Executors
import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Random, Try}

/**
 * The REAL pipeline over the hard clusters ([[HardClusters]]' few hundred listings) — real Mongo, the listing intake, the
 * identity projection and the read-model projection — with every HTTP answer the clusters need replayed from one
 * checked-in file ([[RecordedResponses]]), or, `recording`, asked of the countries' `enrichment-<cc>` trees under
 * `fixtureRoot` and kept: what [[HardClusterConvergenceIntegrationSpec]] asserts over, and [[HardClusterRecord]]
 * re-records the file with. Each pass runs in its own uniquely-named database (`IsolatedMongoDatabase`); [[close]]
 * closes them.
 */
final class HardClusterPasses(target: IntegrationMongoTarget, fixtureRoot: settings.FixtureRoot, environment: Env,
                              val countries: Seq[Country], val recording: Boolean) extends AutoCloseable {

  import HardClusterPasses._

  private val storages = mutable.ListBuffer.empty[ConvergenceStorage]

  def close(): Unit = storages.synchronized(storages.foreach(s => Try(s.close())))

  lazy val responses: Map[Country, RecordedResponses] = countries.map { country =>
    country -> (
      if (recording) RecordedResponses.recording(new FallbackHttpFetch(Seq(
        "tree"       -> new clients.tools.FakeHttpFetch(s"enrichment-${country.code}", strict = true, foldYear = false, root = fixtureRoot),
        "unrecorded" -> new clients.tools.FailingHttpFetch(404))), prior = Some(RecordedResponses.pathFor(country.code)))
      else RecordedResponses.replaying(RecordedResponses.pathFor(country.code)))
  }.toMap

  /** One film as the public sees it, keyed by what does NOT follow arrival order: the
   *  stored key, never the opaque id (which is minted from whichever key a row was first
   *  created under — see `FilmId`). */
  final case class Film(key: String, title: String, year: Option[Int], tmdbId: Option[Int],
                        imdbId: Option[String], cinemas: Seq[String], address: Option[String]) {
    override def toString: String =
      s"$title (${year.getOrElse("—")}) tmdb=${tmdbId.getOrElse("—")} imdb=${imdbId.getOrElse("—")} " +
      s"address=${address.getOrElse("unprojected")} cinemas=${cinemas.mkString("[", ", ", "]")}"
  }

  final class Pass(val label: String, val wiring: ArchiveReplayWiring, val scrapers: Seq[CinemaScraper])

  private def wiringFor(country: Country, label: String, wrap: HttpFetch => HttpFetch = identity,
                        movableClock: Option[MutableClock] = None,
                        venuePages: services.venuepages.VenuePageStore => services.venuepages.VenuePageStore = identity)
      : (ArchiveReplayWiring, ConvergenceStorage) = {
    val storage = ConvergenceStorage.mongo(target, s"hc-${country.code}-$label", TitleNormalizer.forCountry(country),
      services.movies.MovieChangeStream.Debounce.forCountry(country))
    storages.synchronized(storages += storage)
    val w = FetchReplayWiring(country, storage, CorpusFixture.read(HardClusters.corpusKey(country)), wrap(responses(country)),
      fixtureRoot, clock = movableClock.getOrElse(java.time.Clock.fixed(TestWiring.FixedInstant, java.time.ZoneOffset.UTC)),
      // The outage pass refuses on purpose; its retries need not sleep through it.
      retrySleep = if (movableClock.isDefined) (_: Long) => () else Thread.sleep, environment = environment, venuePages = venuePages)
    (w, storage)
  }

  /** The cinemas in a seeded order, each serving its films in a seeded order — the two
   *  orders production varies (the reaper's due-time queue, a site reordering its list). */
  private def arrivals(w: ArchiveReplayWiring, rnd: Random): Seq[CinemaScraper] =
    rnd.shuffle(w.archivedListings.toSeq.sortBy(_._1.displayName)).map { case (cinema, films) =>
      PreScrapedCinemaScraper.replaying(cinema, rnd.shuffle(films.toList))
    }

  /** Production's arrival: each cinema's scrape lands in the identity model's intake through the
   *  production runner. How many landed without throwing. */
  private def arrive(w: ArchiveReplayWiring, scrapers: Seq[CinemaScraper]): Int =
    scrapers.count(scraper => Try(w.cinemaScrapeRunner.run(scraper)).isSuccess)

  /** Projections until one writes nothing — the rest production's projection interval reaches, as the
   *  venue pages and ids a projection's enrichment fetched are taken in by the next — then the read model. */
  private def settle(w: ArchiveReplayWiring): Unit = {
    Iterator.continually(w.projectIdentity()).take(SettleProjections).find(_.atRest)
    // The read model every claim reads, from a whole sweep: passes boot side by side against one
    // local Mongo, where a side read or a read-model write can time out.
    WholeReconcile(w.readModelProjector)
  }
  private val SettleProjections = 5

  def films(w: ArchiveReplayWiring, normalizer: TitleNormalizer): Seq[Film] = {
    val records   = w.movieRepository.findAll()
    val projected = w.readModelRepository.findAllMovies()
    val slugs     = FilmSlugs(projected)
    records.map { r =>
      Film(r.key(normalizer), r.title, r.year, r.record.tmdbId, r.record.imdbId,
        r.record.cinemaData.keySet.map(_.displayName).toSeq.sorted, slugs.slugFor(r.id.value))
    }.sortBy(f => (f.key, f.title))
  }

  private def boot(country: Country, label: String, seed: Long, split: Boolean,
                   venuePages: services.venuepages.VenuePageStore => services.venuepages.VenuePageStore = identity): (Pass, Seq[Film]) = {
    val (w, _) = wiringFor(country, label, venuePages = venuePages)
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
  // Held as a Try: a lazy val whose initialiser throws is re-run on the next access, so one failed
  // boot re-ran every pass of every country for each test after it.
  private lazy val bootAttempt: Try[Map[Country, Seq[(Pass, Seq[Film])]]] = Try {
    val pool = Executors.newFixedThreadPool(8)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)
    try {
      val jobs = countries.flatMap { c =>
        (0 until Permutations).map(i => c -> (() => boot(c, s"p$i", OrderSeed + i, split = false))) :+
          (c -> (() => boot(c, "split", OrderSeed, split = true))) :+
          // Every venue page's first write lands unacknowledged, as a loaded Mongo times one out: the
          // page is read again and taken in, never decided without.
          (c -> (() => boot(c, "unacked", OrderSeed, split = false, venuePages = new UnacknowledgedVenuePageWrites(_))))
      }
      val done = Await.result(Future.traverse(jobs) { case (c, job) => Future(c -> job()) }, SpecTimeouts.Run)
      done.groupMap(_._1)(_._2)
    } finally pool.shutdown()
  }
  def booted: Map[Country, Seq[(Pass, Seq[Film])]] = bootAttempt.get

  /** RECORD MODE: also ask the identity resolver's query set (`IdentityLookupSweep`) of each
   *  country's clusters, so the responses file answers the phase-1 gate
   *  (`IdentityQueryCoverageIntegrationSpec`) as well as this spec — whatever the tree holds. */
  private def recordIdentityLookups(): Unit = countries.foreach { country =>
    val storage = ConvergenceStorage.mongo(target,
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
          .distinctBy { case (_, film) => (film.movie.title.trim.toLowerCase(java.util.Locale.ROOT), film.movie.releaseYear) }
          .zipWithIndex.map { case ((cinema, film), i) => () =>
            val (w, storage) = wiringFor(country, s"s$i")
            try {
              arrive(w, Seq(PreScrapedCinemaScraper.replaying(cinema, Seq(film))))
              settle(w)
            } finally storage.close()
          }
      }
      Await.result(Future.traverse(jobs)(job => Future(Try(job()))), SpecTimeouts.Run)
      println(s"[hard-clusters] recorded ${jobs.size} spellings on their own")
    } finally pool.shutdown()
  }

  /**
   * TMDB DOWN for part of a boot: half its URLs answer 503 while every venue lands and is projected,
   * and then it comes back. A 5xx is "not now", never "no such film": the model holds the question
   * open, and the lookup fill asks it again — so once TMDB answers and a fill round has run, the
   * corpus must come out as the undisturbed reference did.
   *
   * Returns the films matched in the reference that the outage left unmatched (reported), the films
   * after the recovery, and how many TMDB requests the outage refused.
   */
  private def outage(country: Country): Outage = {
    val down  = new java.util.concurrent.atomic.AtomicBoolean(true)
    val refused = new java.util.concurrent.atomic.AtomicLong()
    val clock = new MutableClock(TestWiring.FixedInstant)
    def flaky(inner: HttpFetch): HttpFetch = new HttpFetch {
      private def check(url: String): Unit =
        if (down.get && url.contains("themoviedb.org") && (url.## & 1) == 0)
          { refused.incrementAndGet(); throw new HttpStatusException(503, "GET", url, retryAfter = None) }
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
    w.lookupFill.round()
    settle(w)
    Outage(unmatched, films(w, normalizer), refused.get)
  }

  /** The films concluded unmatched during the outage, the films after it recovered, and how many
   *  TMDB requests the outage refused — zero would make "nothing concluded unmatched" vacuous. */
  final case class Outage(unmatched: Seq[String], recovered: Seq[Film], refused: Long)

  lazy val outages: Map[Country, Outage] = countries.map(c => c -> outage(c)).toMap

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

  /** The passes that came out other than the reference, but the split-arrival divergences [[HardClusterExemptions]]
   *  knows; and those known divergences no pass shows any more (`fixed`: each is struck, so the exemption cannot hide the
   *  next regression). */
  def divergences(country: Country): (Seq[String], Set[String]) = {
    val passes = booted(country)
    val (reference, refFilms) = passes.head
    val known = HardClusterExemptions.SplitArrivalDivergences.getOrElse(country.code, Set.empty)
    def isKnown(pass: Pass, key: String) = pass.label == "split" && known.contains(key)
    val diverging = passes.tail.flatMap { case (pass, fs) =>
      val d = diff(refFilms, fs, reference.label, pass.label).filterNot { case (key, _) => isKnown(pass, key) }
      if (d.isEmpty) None
      else Some(s"${reference.label} vs ${pass.label} (seed base 0x${OrderSeed.toHexString}):\n${d.map(_._2).take(15).mkString("\n")}")
    }
    val stillDiverging = passes.tail.flatMap { case (pass, fs) =>
      diff(refFilms, fs, reference.label, pass.label).collect { case (key, _) if isKnown(pass, key) => key }
    }.toSet
    (diverging, known -- stillDiverging)
  }

  /** The reference pass's films as `expected-hard-clusters-<cc>.txt` holds them. */
  def actualFilms(country: Country): String = booted(country).head._2.map(f => s"${f.key}\t$f").mkString("", "\n", "\n")

  /** The films checked in for the clusters that the reference pass no longer comes out as. */
  def movedFilms(country: Country, expected: String): Seq[String] = {
    val byKey    = (text: String) => text.linesIterator.filter(_.nonEmpty).map(l => l.takeWhile(_ != '\t') -> l).toSeq.groupMap(_._1)(_._2)
    val (want, got) = (byKey(expected), byKey(actualFilms(country)))
    (want.keySet ++ got.keySet).toSeq.sorted.flatMap { key =>
      (want.getOrElse(key, Nil), got.getOrElse(key, Nil)) match {
        case (w, g) if w == g => None
        case (Nil, g)         => Some(s"  + ${g.mkString("; ")}")
        case (w, Nil)         => Some(s"  - ${w.mkString("; ")}")
        case (w, g)           => Some(s"  ~ was ${w.mkString("; ")}\n    now ${g.mkString("; ")}")
      }
    }
  }

  /** Every pass's served listings that do not match the films holding them. */
  def servedProblems(country: Country): Seq[String] = {
    val listings = ServedCorpusInvariants.listings(CorpusFixture.read(HardClusters.corpusKey(country)))
    booted(country).flatMap { case (pass, _) =>
      val w = pass.wiring
      ServedCorpusInvariants.violations(listings, w.movieRepository.findAll(), w.readModelRepository.findAllMovies(),
        w.readModelRepository.findAllScreenings(), TitleNormalizer.forCountry(country), country = Some(country))
        .map(p => s"pass ${pass.label}: $p")
    }
  }

  /** The films not back to the undisturbed boot's once TMDB recovered from its outage (address aside). */
  def outageDrift(country: Country): Seq[String] = {
    val (reference, refFilms) = booted(country).head
    // The films themselves — identity, title, year, TMDB match, IMDb id, cinemas — must come back. The IMDb id
    // included: a film matched while TMDB failed is announced to the id recovery only once its details are in,
    // so the recovery searches with what TMDB says of it, as the undisturbed boot's did.
    def identity(fs: Seq[Film]) = fs.map(_.copy(address = None))
    diff(identity(refFilms), identity(outages(country).recovered), reference.label, "outage-recovered").map(_._2)
  }

  /** What a further projection and two identical rescrapes of the reference pass rewrote — the known churners
   *  ([[HardClusterExemptions]]) read apart, and named when they no longer churn. Rescrapes the pass: run it once. */
  def churnProblems(country: Country): Seq[String] = {
    val (pass, _) = booted(country).head
    val w          = pass.wiring
    val normalizer = TitleNormalizer.forCountry(country)
    // The known churners are read apart from the rest: left out of every check below,
    // and required to still churn (see the end), so fixing one retires its entry.
    val knownChurn = HardClusterExemptions.RescrapeChurn.getOrElse(country.code, Set.empty)
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
      val before    = records
      val rescraped = arrive(w, new Random(OrderSeed + 100 + tick).shuffle(pass.scrapers))
      // A rescrape that landed nothing would change nothing, and pass every check below.
      if (rescraped != pass.scrapers.size)
        problems += s"rescrape $tick landed only $rescraped of ${pass.scrapers.size} venues — the churn checks cover only those"
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
    problems.toSeq
  }

  /** RECORDING: what the passes do not ask — each spelling alone, the identity resolver's query set — asked too, and
   *  every country's responses written back, the old answers and the new. */
  def writeRecording(): Unit = {
    require(recording, "only a recording writes its responses")
    recordEachSpellingAlone(); recordIdentityLookups()
    responses.foreach { case (country, r) =>
      val path = r.write(RecordedResponses.pathFor(country.code))
      println(s"[${country.code}] recorded ${r.size} responses to $path")
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

object HardClusterPasses {
  /** Fixed, so an order dependence fails the same way on every run. */
  val OrderSeed: Long = 0x2026_09_24L
  val Permutations = 3

  /** Every country whose hard clusters are checked in, of `only` when it names some. */
  def countries(only: Option[Set[Country]]): Seq[Country] =
    Country.all.filter(c => CorpusFixture.exists(HardClusters.corpusKey(c)) && only.forall(_.contains(c)))
}
