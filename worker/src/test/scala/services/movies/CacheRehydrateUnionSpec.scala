package services.movies

import tools.SpecClock.given

import scala.concurrent.duration.DurationInt
import models.{CinemaCityKinepolis, MovieRecord, Multikino, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.{RuleScope, TitleRule, TitleRuleSet}
import services.movies.SingleCountryNormalizer.titleNormalizer

/** Pins the fix for the late-added-merge-rule data-loss bug: when a merge-key
 *  rule (a Canonical-tier unification — NOT a GlobalStructural decoration strip,
 *  which no longer feeds the key) is added AFTER two documents were written under
 *  distinct keys, they now collide on `CacheKey` (equality is `sanitize`, which
 *  runs the canonical tier). `rehydrate` must UNION the colliding rows, not
 *  last-write-wins drop one — otherwise a hydration silently loses one document's
 *  showtimes until the next scrape (and on the read-only web app, the row
 *  briefly serves half its cinemas). Reproduces prod: seed two distinct rows
 *  under the defaults, install a `/Kino Cafe` canonical unification so they
 *  collide, then hydrate. */
class CacheRehydrateUnionSpec extends AnyFlatSpec with Matchers {

  private def repositoryOf(rows: StoredMovieRecord*): MovieRepository =
    new StoredRowsRepository(rows.toSeq, titleNormalizer)

  private def row(title: String, cinema: Source): StoredMovieRecord =
    StoredMovieRecord.synthesised(title, Some(2025),
      MovieRecord(data = Map[Source, SourceData](
        cinema -> SourceData(title = Some(title), rawTitle = Some(title), releaseYear = Some(2025)))), services.movies.SingleCountryNormalizer.titleNormalizer)

  private val decorated = row("Takie jest życie/Kino Cafe", CinemaCityKinepolis)
  private val base      = row("Takie jest życie",           Multikino)

  // Canonical-tier unification that didn't exist when the rows were written;
  // under it both titles sanitise to the same key. (A GlobalStructural strip
  // would NOT collide them — that tier feeds external lookups, not the key.)
  private val kinoCafeRule = TitleRule("test-kino-cafe", RuleScope.Canonical, None,
    """(?i)\s*/\s*Kino\s+Cafe\s*$""", "", applyAll = false, order = 100)

  // A boot that finds Mongo not ready yet must retry rather than start empty: the cache starts empty —
  // the row only ever arrives if it's later re-written (via the change stream), so a quiescent row
  // stays Mongo-only and invisible. With retry enabled, boot waits Mongo out.
  private def flakeyRepository(row: StoredMovieRecord): MovieRepository = {
    val calls = new java.util.concurrent.atomic.AtomicInteger(0)
    new StoredRowsRepository(if (calls.getAndIncrement() == 0) Seq.empty else Seq(row), titleNormalizer)
  }

  "boot hydrate" should "retry an empty findAll (Mongo not ready) so quiescent rows still load" in {
    val cache = new CaffeineMovieCache(
      flakeyRepository(base), bootHydrateMaxAttempts = settings.BootHydrateMaxAttempts(5), bootHydrateRetry = settings.BootHydrateRetryInterval(20.millis), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.entries should have size 1   // boot retried past the empty first findAll
  }

  it should "give up after the configured attempts on a genuinely empty repository" in {
    val cache = new CaffeineMovieCache(repositoryOf(), bootHydrateMaxAttempts = settings.BootHydrateMaxAttempts(3), bootHydrateRetry = settings.BootHydrateRetryInterval(5.millis), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.entries should have size 0  // no rows, and it didn't hang
  }

  /** A store whose whole-corpus read fails while `failing` holds — Mongo slow past every page retry. */
  private final class FailingReadRepository(row: StoredMovieRecord) extends StoredRowsRepository(Seq(row), titleNormalizer) {
    @volatile var failing = true
    val reads = new java.util.concurrent.atomic.AtomicInteger(0)
    override def findAllChecked(): tools.ReadOutcome[Seq[StoredMovieRecord]] = {
      reads.incrementAndGet()
      if (failing) tools.ReadOutcome.Failed(tools.ReadFailure.Thrown(new com.mongodb.MongoTimeoutException("slow")))
      else super.findAllChecked()
    }
  }

  // A boot read that failed past its attempts left the worker's cache EMPTY, and only the change
  // stream (rows written after boot) and the 6-hour backstop ever filled it: every quiescent row
  // was invisible to the fold and the settle for hours after a Mongo blip at boot.
  "the cold-retry tick" should "re-read a cache whose boot read failed, until a read completes" in {
    val repository = new FailingReadRepository(base)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.entries should have size 0

    cache.coldRetryTick()          // still failing: still cold
    cache.entries should have size 0
    repository.failing = false
    cache.coldRetryTick()
    cache.entries should have size 1
  }

  it should "cost nothing once a read has completed" in {
    val repository = new FailingReadRepository(base)
    repository.failing = false
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val readsAtBoot = repository.reads.get()
    cache.coldRetryTick()
    repository.reads.get() shouldBe readsAtBoot
  }

  /** Records what the boot hydrate hands the boot's other corpus readers: each read it ended, as the
   *  pages it offered (`None` for a hydrate that gave up), and every call in order. */
  private final class RecordingBootReader(log: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer.empty) extends BootCorpusReader {
    val offers = scala.collection.mutable.ListBuffer.empty[Option[Seq[StoredMovieRecord]]]
    private val pages = scala.collection.mutable.ListBuffer.empty[StoredMovieRecord]
    def bootPage(rows: Seq[StoredMovieRecord]): Unit = { log += s"page of ${rows.map(_.title).mkString(", ")}"; pages ++= rows }
    def bootReadEnded(end: BootReadEnd): Unit = {
      log += s"read ended: $end"
      end match {
        case BootReadEnd.Whole    => offers += Some(pages.toList)
        case BootReadEnd.Retrying => ()
        case BootReadEnd.GaveUp   => offers += None
      }
      pages.clear()
    }
  }

  /** A store with showtimes in their own collection — so the cache keeps films without them, as the
   *  worker's does — whose corpus read arrives a page per film, each page built fresh as the scan reaches
   *  it (nothing here holds a page once it is handed over), with `between` run before each later page.
   *  Collecting the whole read goes through the same pages, as `MongoMovieRepository.findAllChecked` does. */
  private final class PagedRepository(page: Int => StoredMovieRecord, pages: Int, between: Int => Unit = _ => (),
                                      normalizer: TitleNormalizer = titleNormalizer)
      extends StoredRowsRepository((0 until pages).map(page), normalizer) {
    override def hasScreenings: Boolean = true
    override def foreachPage(onPage: Seq[StoredMovieRecord] => Unit): tools.ScanOutcome = {
      (0 until pages).foreach { i => if (i > 0) between(i); onPage(Seq(page(i))) }
      tools.ScanOutcome.complete
    }
    override def findAllChecked(): tools.ReadOutcome[Seq[StoredMovieRecord]] = {
      val rows = Vector.newBuilder[StoredMovieRecord]
      foreachPage(rows ++= _).collected(rows.result())
    }
  }

  private def showingAt(title: String, cinema: Source, showtimes: Seq[models.Showtime]): StoredMovieRecord =
    StoredMovieRecord.synthesised(title, Some(2025), MovieRecord(data = Map[Source, SourceData](
      cinema -> SourceData(title = Some(title), rawTitle = Some(title), releaseYear = Some(2025), showtimes = showtimes))), titleNormalizer)

  private def evening(day: Int, url: String): models.Showtime =
    models.Showtime(java.time.LocalDateTime.of(2026, 10, day, 20, 0), Some(url))

  // A boot held every film WITH its showtimes for the whole hydrate read and the boot readers' derivation
  // after it — on worker-us 1.4M showtimes, long enough at boot that the old generation took them in, only
  // to collect them again in the boot's first full collections (2026-10-05).
  "the hydrate" should "let a page's showtimes go once the page is read, keeping its films without them" in {
    val firstShowtimes = scala.collection.mutable.ListBuffer.empty[java.lang.ref.WeakReference[models.Showtime]]
    var heldAtSecondPage = -1
    def page(i: Int): StoredMovieRecord = {
      val showtimes = (1 to 3).map(day => evening(day, s"https://book/$i/$day"))
      if (i == 0) firstShowtimes ++= showtimes.map(new java.lang.ref.WeakReference(_))
      showingAt(s"Film $i", Multikino, showtimes)
    }
    def collected(): Int = firstShowtimes.count(_.get() != null)
    val repository = new PagedRepository(page, pages = 2, between = _ => {
      Iterator.continually { System.gc(); collected() }.take(20).find(_ == 0)
      heldAtSecondPage = collected()
    })
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    heldAtSecondPage shouldBe 0
    cache.entries should have size 2
    val first = cache.entries.collectFirst { case (_, record) if record.cinemaData.values.exists(_.title.contains("Film 0")) => record }.get
    first.cinemaData.values.map(ShowtimesDigest.slotShowtimeCount).toSeq shouldBe Seq(3)   // kept by count and digest
    first.cinemaData.values.flatMap(_.showtimes) shouldBe empty
  }

  it should "hand each page to the boot readers as it is read, not once the read is over" in {
    val log    = scala.collection.mutable.ListBuffer.empty[String]
    val reader = new RecordingBootReader(log)
    new CaffeineMovieCache(new PagedRepository(i => showingAt(s"Film $i", Multikino, Nil), pages = 2, between = i => log += s"reading page $i"),
      normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned, bootReaders = Seq(reader))
    log.toList shouldBe List("page of Film 0", "reading page 1", "page of Film 1", "read ended: Whole")
  }

  // Two documents a late merge-key rule collides are one film: the hydrate unions them, showtimes
  // included, wherever the read put them. Streamed, it keeps each row without its showtimes, so it reads
  // the two again whole to union them.
  it should "union the showtimes of two documents a late rule collides, on different pages" in {
    val rows = Vector(showingAt("Takie jest życie/Kino Cafe", Multikino, Seq(evening(1, "https://book/a"))),
                      showingAt("Takie jest życie", Multikino, Seq(evening(2, "https://book/b"))))
    val cache = new CaffeineMovieCache(new PagedRepository(rows, pages = 2), clock = _root_.tools.SpecClock.Pinned,
      normalizer = new TitleNormalizer(TitleRuleSet(services.titlerules.TitleRules.all :+ kinoCafeRule)))
    cache.entries should have size 1
    cache.entries.head._2.cinemaData.values.map(ShowtimesDigest.slotShowtimeCount).toSeq shouldBe Seq(2)
  }

  // A boot read the whole corpus three times before (the hydrate, the projector's missing-card check,
  // the census's first pass). The hydrate's read is handed to the other two instead — once, whole.
  "the boot hydrate" should "hand its complete read to the boot readers, once" in {
    val reader = new RecordingBootReader
    val cache  = new CaffeineMovieCache(repositoryOf(base, decorated), normalizer = titleNormalizer,
      clock = _root_.tools.SpecClock.Pinned, bootReaders = Seq(reader))
    reader.offers.map(_.map(_.map(_.title).toSet)) shouldBe Seq(Some(Set(base.title, decorated.title)))
    cache.rehydrate()                                      // a backstop read is not the boot's
    reader.offers should have size 1
  }

  // A failed read is not an empty corpus: a reader handed `Some(Nil)` would publish zero films or heal nothing.
  it should "hand over nothing but a None when its read failed, so each reader reads for itself" in {
    val reader = new RecordingBootReader
    new CaffeineMovieCache(new FailingReadRepository(base), normalizer = titleNormalizer,
      clock = _root_.tools.SpecClock.Pinned, bootReaders = Seq(reader))
    reader.offers shouldBe Seq(None)
  }

  // At boot an empty answer is as likely a Mongo not ready yet as an empty store (the retry above).
  it should "not hand over an empty answer as the corpus, but the first whole read a retry makes" in {
    val reader = new RecordingBootReader
    new CaffeineMovieCache(flakeyRepository(base), bootHydrateMaxAttempts = settings.BootHydrateMaxAttempts(5),
      bootHydrateRetry = settings.BootHydrateRetryInterval(5.millis), normalizer = titleNormalizer,
      clock = _root_.tools.SpecClock.Pinned, bootReaders = Seq(reader))
    reader.offers.map(_.map(_.map(_.title))) shouldBe Seq(Some(Seq(base.title)))

    val empty = new RecordingBootReader
    new CaffeineMovieCache(repositoryOf(), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned, bootReaders = Seq(empty))
    empty.offers shouldBe Seq(None)
  }
}
