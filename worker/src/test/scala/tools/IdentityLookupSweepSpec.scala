package tools

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * The recording sweep issues EXACTLY the identity resolver's query set — every listing's detail
 * page, every `CandidateQueries` question and the identity record of every film any answer names —
 * because it IS a resolve; and the set is a function of the listing SET, never of arrival order.
 */
class IdentityLookupSweepSpec extends AnyFlatSpec with Matchers {

  private def listing(cinema: Cinema, title: String, year: Option[Int] = None, director: Seq[String] = Nil,
                      page: Option[String] = None): CinemaMovie =
    CinemaMovie(Movie(title = title, releaseYear = year), cinema, None, page, None, Nil, director, Nil)

  private val corpus: Map[Cinema, Seq[CinemaMovie]] = Map(
    // One venue listing two films under one title — the rows `ScrapeListing.prepare` folds.
    Multikino -> Seq(listing(Multikino, "Sinn und Sinnlichkeit", Some(1995), Seq("Ang Lee")),
                     listing(Multikino, "Sinn und Sinnlichkeit", Some(2026), Seq("Georgia Oakley"))),
    Kinoteka  -> Seq(listing(Kinoteka, "Oficjalna premiera: Rozważna i romantyczna", page = Some("https://kinoteka.pl/film/1")),
                     listing(Kinoteka, "Coś", page = Some("https://kinoteka.pl/boom"))),
    Helios    -> Seq(listing(Helios, "Sinn und Sinnlichkeit", Some(1995), Seq("Ang Lee"))))

  /** A source answering every question with a film per distinct string, recording what it was asked
   *  — from any thread, each ask taking `latency` (a live fetch's wait), the most it held in flight
   *  at once counted in `mostInFlight`. */
  private final class Recording(latency: Long = 0) extends IdentityLookups {
    private val asks     = java.util.concurrent.ConcurrentLinkedQueue[String]()
    private val inFlight = java.util.concurrent.atomic.AtomicInteger()
    val mostInFlight     = java.util.concurrent.atomic.AtomicInteger()
    def asked: Seq[String] = scala.jdk.CollectionConverters.IterableHasAsScala(asks).asScala.toSeq
    private def ask(name: String): Unit = {
      mostInFlight.accumulateAndGet(inFlight.incrementAndGet(), math.max)
      try { if (latency > 0) Thread.sleep(latency); asks.add(name); () } finally { inFlight.decrementAndGet(); () }
    }
    override def hasDetail(l: Listing): Boolean = l.page.isDefined
    override def detail(l: Listing): Answer[Option[DetailFacts]] = {
      ask(s"detail ${l.page.get}")
      if (l.page.exists(_.endsWith("/boom"))) Answer.Unknown else Answer.Known(Some(DetailFacts(Some(1995), Seq("Ang Lee"), Some(136), None)))
    }
    override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = {
      ask(q.sortKey)
      Answer.Known(Seq(Hit(math.abs(q.sortKey.hashCode % 50), q.sortKey.drop(2), None, Some(1995), 1.0)))
    }
    override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = {
      ask(s"film $id")
      Answer.Known(Some(IdentityMeasures.Film(s"film $id", year = Some(1995))))
    }
  }

  private def sweep(archived: Map[Cinema, Seq[CinemaMovie]]) = {
    val source = new Recording
    val names  = scala.collection.mutable.ArrayBuffer.empty[String]
    val summary = IdentityLookupSweep.run(services.identity.Listing.corpus(archived, titleNormalizer), source, titleNormalizer, names += _)
    (summary, source.asked, names.toSeq)
  }

  "the sweep" should "ask exactly the resolver's questions: every listing's CandidateQueries, its detail page, and every named film's record" in {
    val (summary, asked, names) = sweep(corpus)
    val listings = services.identity.Listing.corpus(corpus, titleNormalizer)
    // What the resolver's own definitions say it must ask, derived from the SAME functions.
    val source = new Recording
    val evidences = listings.map(l => Evidence.of(l, if (source.hasDetail(l)) source.detail(l).toOption.flatten else None))
    val queries   = evidences.flatMap(CandidateQueries.of).distinct
    val films     = queries.flatMap(q => source.candidates(q).toOption.getOrElse(Nil)).map(_.tmdbId).distinct

    asked.filter(_.startsWith("detail")).sorted shouldBe Seq("detail https://kinoteka.pl/boom", "detail https://kinoteka.pl/film/1")
    asked.filterNot(a => a.startsWith("detail") || a.startsWith("film")).toSet shouldBe queries.map(_.sortKey).toSet
    asked.filterNot(a => a.startsWith("detail") || a.startsWith("film")).size shouldBe queries.size
    asked.filter(_.startsWith("film")).toSet shouldBe films.map(id => s"film $id").toSet
    // The title searches are the calibration's own shapes, yearless: the banner segment too.
    queries should contain (CandidateQuery.Title("Rozważna i romantyczna"))
    // …and a director's filmography, from the detail page merged under the listing.
    queries should contain (CandidateQuery.Director("Ang Lee"))
    // …and the films IMDb lists under each listing's whole title, a path TMDB's search can miss.
    evidences.map(_.title).distinct.foreach(t => queries should contain (CandidateQuery.Imdb(t)))
    summary.detailsUnanswered shouldBe 1
    names.size shouldBe asked.size
  }

  it should "ask the same questions whatever order the corpus comes in" in {
    val shuffled = scala.collection.immutable.ListMap(corpus.toSeq.reverse.map { case (c, ls) => c -> ls.reverse }*)
    sweep(shuffled)._2.sorted shouldBe sweep(corpus)._2.sorted
    sweep(shuffled)._1 shouldBe sweep(corpus)._1
  }

  private def withTree[A](body: java.nio.file.Path => A): A = {
    val root = java.nio.file.Files.createTempDirectory("identity-sweep-tree")
    try body(root)
    finally {
      Seq(IdentityLookupSweep.RecordedMarker, ".identity-lookups-v2", ".identity-lookups").foreach(m => java.nio.file.Files.deleteIfExists(root.resolve(m)))
      java.nio.file.Files.deleteIfExists(root)
    }
  }

  "a recording" should "keep every lookup each recording leg over the same tree asked" in withTree { root =>
    IdentityLookupSweep.markRecorded(root, Seq("film 1", "query b"))
    IdentityLookupSweep.markRecorded(root, Seq("film 2", "query b"))
    IdentityLookupSweep.recordedIn(root) shouldBe Some(Set("film 1", "film 2", "query b"))
  }

  // A recording leg's sweep met every lookup its tree lacked live and ONE AT A TIME: 274 s of the US
  // leg against 41 s for the same sweep replayed hermetically (runs 36974178044, 37029415020).
  private def pooled[A](body: java.util.concurrent.ExecutorService => A): A = {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(4)
    try body(pool) finally { pool.shutdownNow(); () }
  }

  "a sweep with a pool" should "ask its lookups side by side, and ask and answer exactly what it does one at a time" in pooled { pool =>
    val listed  = services.identity.Listing.corpus(corpus, titleNormalizer)
    val serial  = new Recording(latency = 5)
    val serialNames = scala.collection.mutable.ArrayBuffer.empty[String]
    val serialSummary = IdentityLookupSweep.run(listed, serial, titleNormalizer, serialNames += _)
    val side    = new Recording(latency = 5)
    val sideNames = scala.collection.mutable.ArrayBuffer.empty[String]
    val sideSummary = IdentityLookupSweep.run(listed, side, titleNormalizer, sideNames += _, pool = Some(pool))
    serial.mostInFlight.get shouldBe 1
    side.mostInFlight.get should be > 1
    side.asked.sorted shouldBe serial.asked.sorted
    sideNames.sorted shouldBe serialNames.sorted
    sideSummary shouldBe serialSummary
  }

  it should "fetch a detail page several listings share once" in pooled { pool =>
    val page   = Some("https://kinoteka.pl/film/1")
    val shared = corpus.updated(Kinoteka, corpus(Kinoteka) :+ listing(Kinoteka, "Rozważna i romantyczna", page = page))
    val side   = new Recording
    IdentityLookupSweep.run(services.identity.Listing.corpus(shared, titleNormalizer), side, titleNormalizer, pool = Some(pool))
    side.asked.count(_ == s"detail ${page.get}") shouldBe 1
  }
}
