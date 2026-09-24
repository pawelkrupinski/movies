package integration

import clients.TmdbClient
import models.Country
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{CinemaScraper, PreScrapedCinemaScraper}
import services.events.MovieDetailsComplete
import services.movies.{StoredMovieRecord, TitleNormalizer}
import services.readmodel.FilmSlugs
import tools._

import java.util.concurrent.Executors
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Random, Try}

/**
 * The convergence legs' fold/settle claims, over the HARD CLUSTERS only, in minutes
 * rather than hours.
 *
 * Every fold/settle regression the country convergence legs caught in their first
 * months lived in a few dozen films — franchise siblings folding by arrival order, a
 * contested film address decided by which row came first, a decorated re-release year
 * read as a second film by the settle's split detector, a programme-prefixed listing
 * re-folding on an identical rescrape. The legs found each of them 1.5-5 hours after
 * the commit, and a red streak ran up to two and a half days. `tools.HardClusters`
 * extracts those clusters (and the shapes around them) from the recorded corpora into a
 * few hundred listings, and this spec runs the REAL pipeline over them — real Mongo,
 * the real staging fold, settle and read-model projection — with every HTTP answer the
 * clusters need replayed from one checked-in file (`tools.RecordedResponses`).
 *
 * Per country it asserts:
 *
 *   1. ORDER-INDEPENDENCE — three seeded arrival orders (cinemas shuffled, each
 *      cinema's films shuffled, the staging reaper advanced between arrivals as
 *      production's does) and a SPLIT arrival (half the cinemas, settled and projected,
 *      then the rest) come out as the same films: identity, title, year, tmdbId,
 *      imdbId, cinemas and the film's public address.
 *   2. NO SETTLE-SPLIT — no settle, in any pass or on the settled corpus, splits a slot
 *      off a row as a second film. The fixture holds no genuinely mixed row, so a split
 *      is the detector reading ordinary data as two films (the "Mockingjay - Part 1
 *      (2026)" beside "Part 1" split of 14 UK venues). A further settle over the settled
 *      corpus also changes no record.
 *   3. NO CHURN — two identical rescrapes re-divert no known film to staging, move no
 *      key and change no stored record.
 *
 * Each pass runs in its own uniquely-named database (`IsolatedMongoDatabase`), dropped
 * in `afterAll`.
 *
 * RECORD MODE re-captures the responses file after the fixture grows (see
 * `scripts/hard-clusters.sh`): with `KINOWO_HARD_CLUSTERS_RECORD=1` and
 * `KINOWO_FIXTURE_ROOT` naming a directory holding the countries' `enrichment-<cc>`
 * trees, every request is answered from the tree and the answers written back.
 */
class HardClusterConvergenceIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val uri = Env.get("MONGODB_URI")
  assume(uri.isDefined, "MONGODB_URI not set")
  IntegrationMongo.requireThrowaway()

  private val Recording = Env.get("KINOWO_HARD_CLUSTERS_RECORD").exists(v => v == "1" || v.equalsIgnoreCase("true"))

  /** Fixed, so an order dependence fails the same way on every run. */
  private val OrderSeed = 0x2026_09_24L
  private val Permutations = 3

  private val storages = mutable.ListBuffer.empty[ConvergenceStorage]

  override def afterAll(): Unit = {
    if (Recording) recordEachSpellingAlone()
    storages.synchronized(storages.foreach(s => Try(s.close())))
    if (Recording) responses.foreach { case (country, r) =>
      val path = r.write(RecordedResponses.pathFor(country.code))
      println(s"[${country.code}] recorded ${r.size} responses to $path")
    }
    super.afterAll()
  }

  /** `KINOWO_HARD_CLUSTERS_COUNTRIES=uk,us` narrows a local run to those countries. */
  private val countries: Seq[Country] = {
    val only = Env.get("KINOWO_HARD_CLUSTERS_COUNTRIES").map(_.split(",").map(_.trim.toLowerCase).toSet)
    Country.all.filter(c => CorpusFixture.exists(HardClusters.corpusKey(c)) && only.forall(_.contains(c.code)))
  }

  private lazy val responses: Map[Country, RecordedResponses] = countries.map { country =>
    country -> (
      if (Recording) RecordedResponses.recording(new FallbackHttpFetch(Seq(
        "tree"       -> new clients.tools.FakeHttpFetch(s"enrichment-${country.code}", strict = true, foldYear = false),
        "unrecorded" -> new clients.tools.FailingHttpFetch(404))))
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

  private final class Pass(val label: String, val wiring: ArchiveReplayWiring, val scrapers: Seq[CinemaScraper]) {
    /** Slots the settle split off a row as a second film while this pass booted. */
    val bootSplits: Int = wiring.movieService.mixedFilmSplits
  }

  private def wiringFor(country: Country, label: String): (ArchiveReplayWiring, ConvergenceStorage) = {
    val normalizer = TitleNormalizer.forCountry(country)
    val storage    = ConvergenceStorage.mongo(uri.get, s"hc-${country.code}-$label", normalizer)
    storages.synchronized(storages += storage)
    val rows = CorpusFixture.read(HardClusters.corpusKey(country))
    CorpusFixture.seedInto(storage.archive, rows)
    val fetch    = responses(country)
    val language = country.language
    val w = new ArchiveReplayWiring(country, storage.archive, None, storage) {
      // Ordering, not timing: the whole cascade on the calling thread, so the only
      // nondeterminism left is the seeded arrival order.
      override lazy val backgroundBudget: ExecutionBudget = new SameThreadExecutionBudget
      override lazy val httoFetch: HttpFetch       = fetch
      override lazy val enrichmentFetch: HttpFetch = fetch
      // A stub key: the answers are replayed, and a keyless client short-circuits
      // before it reaches the fetch at all.
      // Held in memory: its daemon flusher outlives the pass and would re-create the
      // pass's database after `afterAll` dropped it. Uptime has no part in the claims.
      override lazy val uptimeMonitor = new services.UptimeMonitor(None, clock = clock)
      override lazy val tmdbClient: TmdbClient =
        new TmdbClient(fetch, apiKey = Some("hard-clusters"), language = language)
    }
    (w, storage)
  }

  /** The cinemas in a seeded order, each serving its films in a seeded order — the two
   *  orders production varies (the reaper's due-time queue, a site reordering its list). */
  private def arrivals(w: ArchiveReplayWiring, rnd: Random): Seq[CinemaScraper] =
    rnd.shuffle(w.archivedListings.toSeq.sortBy(_._1.displayName)).map { case (cinema, films) =>
      PreScrapedCinemaScraper.replaying(cinema, rnd.shuffle(films.toList))
    }

  /** Production's arrival: each cinema's scrape lands and publishes inline, and the
   *  staging reaper advances between cinemas — so a film's group can resolve and fold
   *  against a PARTIAL set of its cinemas. */
  private def arrive(w: ArchiveReplayWiring, scrapers: Seq[CinemaScraper]): Unit = {
    scrapers.foreach { scraper =>
      try w.cinemaScrapeRunner.run(scraper) catch { case _: Exception => () }
      w.advanceStagingOnce()
    }
    w.enrichDetailsSync()
    w.drainServices()
  }

  /** The periodic settle production runs once staging has drained, then projection. */
  private def settle(w: ArchiveReplayWiring): Unit = {
    w.drainStaging()
    w.movieService.settle()
    w.movieCache.canonicalizeBySanitize()
    w.drainStaging()
    w.movieService.settle()
    w.concludeEnrichment()
    w.readModelProjector.reconcile()
  }

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

  /** What moved between two passes, by film key — named so a failure says WHICH cluster. */
  private def diff(a: Seq[Film], b: Seq[Film], la: String, lb: String): Seq[(String, String)] = {
    val byA = a.groupBy(_.key); val byB = b.groupBy(_.key)
    (byA.keySet ++ byB.keySet).toSeq.sorted.flatMap { key =>
      ((byA.getOrElse(key, Nil), byB.getOrElse(key, Nil)) match {
        case (x, y) if x == y => None
        case (Nil, y)         => Some(s"  only in $lb: ${y.mkString("; ")}")
        case (x, Nil)         => Some(s"  only in $la: ${x.mkString("; ")}")
        case (x, y)           => Some(s"  $key\n    $la: ${x.mkString("; ")}\n    $lb: ${y.mkString("; ")}")
      }).map(key -> _)
    }
  }

  /**
   * Divergences this spec found on its first run that are REAL and NOT YET FIXED, each
   * confined to the SPLIT arrival (the permutations agree) — so the rest of the claim can
   * guard everything else meanwhile. Keyed by country code, then the film's stored title
   * key; a film listed here is left out of the split-vs-reference comparison only.
   *
   * A ratchet, not an allowlist: an entry that no longer diverges FAILS the spec, so a
   * fix has to delete its entry and cannot leave the exemption behind to hide the next
   * regression. Never add to it to get a build green — add the fix.
   */
  /** The rescrape twin of [[KnownSplitArrivalDivergences]], keyed by sanitized title — the
   *  same ratchet: a listed film that stops churning fails the spec. */
  private val KnownRescrapeChurn: Map[String, Set[String]] = Map(
    // "IT (2017)" and "It (1990)" sanitize to one anchor, incubate yearless together and
    // fold into ONE unresolved row; the next identical rescrape re-keys it to 1990 (the
    // landing reads the bracketed year, the staging divert does not). Filing staging rows
    // under the printed year fixed this and was reverted (829eb309d): some venues print an
    // EVENT year on an old film, and 5,235 Polish screenings were lost. Needs corroboration
    // beyond the title text; the full US leg has carried this loop since 2026-09-07.
    "us" -> Set("it")
  )

  private val KnownSplitArrivalDivergences: Map[String, Set[String]] = Map(
    // Odeon's rerelease pages bracket the SEASON's year onto a 2013 film ("The Hunger
    // Games: Catching Fire (2026)", "... Mockingjay - Part 2 (2026)"). Arriving beside the bare listing it folds into the
    // group that resolves to 2013; arriving after the 2013 row has settled it resolves
    // alone, TMDB has no 2026 film of that name, and the year-window reclaim refuses it
    // because its own bracketed year disagrees — the guard that keeps "It (1990)" apart
    // from "It" (2017), and the reason 829eb309d/3e4cbb3c5 could not simply trust the
    // printed year. An unresolved duplicate card.
    "uk" -> Set("thehungergamescatchingfire", "thehungergamesmockingjaypart2"),
    // "Opętanie | klasyka w 4k" (Kino 1410, no director or year): TMDB rightly refuses
    // the bare "Opętanie" as ambiguous, and the staging IMDb recovery then takes the
    // suggestion endpoint's first hit — a 1973 film — for it. Arriving after Żuławski's
    // 1981 row has settled, the landing puts it on that row instead.
    "pl" -> Set("opetanie", "opetanieklasykaw4k")
  )

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
      if (Env.get("KINOWO_HARD_CLUSTERS_DUMP").isDefined)
        passes.foreach { case (p, fs) => println(s"[${country.code}] ${p.label}:\n  ${fs.mkString("\n  ")}") }
      val known = KnownSplitArrivalDivergences.getOrElse(country.code, Set.empty)
      def isKnown(pass: Pass, key: String) = pass.label == "split" && known.contains(key.takeWhile(_ != '|'))
      val divergences = passes.tail.flatMap { case (pass, fs) =>
        val d = diff(refFilms, fs, reference.label, pass.label).filterNot { case (key, _) => isKnown(pass, key) }
        if (d.isEmpty) None
        else Some(s"${reference.label} vs ${pass.label} (seed base 0x${OrderSeed.toHexString}):\n${d.map(_._2).take(15).mkString("\n")}")
      }
      val stillDiverging = passes.tail.flatMap { case (pass, fs) =>
        diff(refFilms, fs, reference.label, pass.label).collect { case (key, _) if isKnown(pass, key) => key.takeWhile(_ != '|') }
      }.toSet
      val fixed = known -- stillDiverging
      withClue(s"$name: ${divergences.size} pass(es) diverged:\n${divergences.mkString("\n")}\n") {
        divergences shouldBe empty
      }
      withClue(s"$name: ${fixed.mkString(", ")} no longer diverge(s) on the split arrival — delete the entry from " +
               "KnownSplitArrivalDivergences so the exemption cannot hide the next regression: ") {
        fixed shouldBe empty
      }
    }

    it should "not split, re-fold or rewrite a settled row on a further settle or an identical rescrape" in {
      val (pass, _) = booted(country).head
      val w          = pass.wiring
      val normalizer = TitleNormalizer.forCountry(country)
      // The known churners are read apart from the rest: left out of every check below,
      // and required to still churn (see the end), so fixing one retires its entry.
      val knownChurn = KnownRescrapeChurn.getOrElse(country.code, Set.empty)
      def isKnown(title: String) = knownChurn.contains(normalizer.sanitize(title))
      def allRecords = w.movieRepository.findAll().sortBy(r => (r.key(normalizer), r.title))
      def records: Seq[StoredMovieRecord] = allRecords.filterNot(r => isKnown(r.title))
      def knownRecords: Seq[StoredMovieRecord] = allRecords.filter(r => isKnown(r.title))
      def keys = w.movieCache.snapshot().map(r => (r.title, r.year)).filterNot(k => isKnown(k._1)).toSet
      val knownBefore = knownRecords

      val problems = mutable.ListBuffer.empty[String]
      booted(country).collect { case (p, _) if p.bootSplits != 0 =>
        problems += s"pass ${p.label}'s settles split ${p.bootSplits} cinema slot(s) off rows as a second film" }
      val settledRecords = records
      val settledKeys    = keys
      val splitsBefore   = w.movieService.mixedFilmSplits

      w.movieService.settle()
      w.movieCache.canonicalizeBySanitize()
      val splitBySettle = w.movieService.mixedFilmSplits - splitsBefore
      if (splitBySettle != 0)
        problems += s"a settle on the settled corpus split $splitBySettle cinema slot(s) off rows as a second film"
      val afterSettle = records
      if (afterSettle != settledRecords)
        problems += s"a settle on the settled corpus changed records:\n${changed(settledRecords, afterSettle, normalizer)}"

      (1 to 2).foreach { tick =>
        val before     = records
        val staged     = w.stagingRepository.findAll().map(r => (r.cinema.displayName, r.title)).toSet
        val ready      = mutable.ListBuffer.empty[MovieDetailsComplete]
        val rnd        = new Random(OrderSeed + 100 + tick)
        rnd.shuffle(pass.scrapers).foreach { scraper =>
          Try(scraper.fetch()).toOption.foreach { listed =>
            val touched = w.movieCache.recordCinemaScrape(scraper.cinema, listed)
            ready ++= w.cinemaScrapeRunner.classify(scraper.cinema, touched)
          }
        }
        val diverted = w.stagingRepository.findAll().map(r => (r.cinema.displayName, r.title)).toSet -- staged
        ready.foreach(w.eventBus.publish)
        w.drainServices()
        w.drainStaging()
        val splitsBeforeSettle = w.movieService.mixedFilmSplits
        w.movieService.settle()
        val splitsInTick = w.movieService.mixedFilmSplits - splitsBeforeSettle
        val divertedUnknown = diverted.filterNot { case (_, title) => isKnown(title) }
        if (divertedUnknown.nonEmpty)
          problems += s"rescrape $tick re-diverted ${divertedUnknown.size} known film(s) to staging: ${divertedUnknown.toSeq.sorted.take(10).mkString(", ")}"
        if (splitsInTick != 0)
          problems += s"rescrape $tick's settle split $splitsInTick cinema slot(s) off rows"
        val after = records
        if (after != before) problems += s"rescrape $tick changed records:\n${changed(before, after, normalizer)}"
        val drift = (keys -- settledKeys).map(k => s"+$k") ++ (settledKeys -- keys).map(k => s"-$k")
        if (drift.nonEmpty) problems += s"rescrape $tick moved keys: ${drift.toSeq.sorted.take(10).mkString(", ")}"
      }
      if (knownChurn.nonEmpty && knownRecords == knownBefore)
        problems += s"${knownChurn.mkString(", ")} no longer churn(s) — delete the entry from KnownRescrapeChurn " +
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
