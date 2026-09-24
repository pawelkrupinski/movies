package services.cinemas.common

import models.{Cinema, CinemaMovie, KinoMuranow, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.fallback.InMemoryFallbackStore
import tools.{HostScrapeStats, TraitDecorators}

import java.lang.reflect.Method
import java.util.concurrent.Executors

/**
 * Every scrape DECORATOR must answer for its delegate on everything except the listing.
 *
 * They did not, and it cost the UK its advance-booking programme.
 * `ScrapeChunkReduceHandler` publishes a partial reduce as
 * `PreScrapedCinemaScraper(listingComplete = false)` so `MovieCache` skips its prune —
 * but `WorkerWiring.publishScrape` wraps that in a recording/fallback decorator before
 * `CinemaScrapeRunner` reads the flag, and every decorator hand-copied `cinema` +
 * `scrapeHosts` while silently inheriting `CinemaScraper`'s default for the rest. All
 * three answered `listingIsComplete = true`, so the signal never reached the cache and
 * every film that only screened on a missing date was pruned as "stopped screening".
 * `chainVenueId` later shipped dropped the same way.
 *
 * So neither list here is hand-written. The MEMBERS are every zero-argument member of
 * `CinemaScraper`, read by reflection, and the delegate answers a value unlike the
 * trait's default for each; the DECORATORS are every class beside `CinemaScraper` that
 * implements it by wrapping one (or several, like `MultiListingScraper`). A member added to the trait, or a decorator added to
 * the codebase, is covered without touching this file — or fails it until it is.
 */
class DecoratorTransparencySpec extends AnyFlatSpec with Matchers {

  /** Members that ARE the decorator's own business: the listing it produces. */
  private val listingMembers = Set("fetch", "fetchWithSource")

  private val members: Seq[Method] =
    TraitDecorators.members(classOf[CinemaScraper])
      .filter(_.getParameterCount == 0)
      .filterNot(m => listingMembers(m.getName))

  /** What an undecorated scraper answers when it overrides nothing it need not. */
  private val bare: CinemaScraper = new CinemaScraper {
    val cinema: Cinema            = KinoMuranow
    def scrapeHosts: Set[String]  = Set.empty
    def fetch(): Seq[CinemaMovie] = Seq.empty
  }

  /** A value for `m` that differs from [[bare]]'s — so inheriting the default shows. */
  private def unlikeTheDefault(m: Method): Any = (m.invoke(bare), m.getName) match {
    case (n: Integer, _)            => n + 4
    case (b: java.lang.Boolean, _)  => !b
    case (_: Option[?], name)       => Some(s"sentinel-$name")
    case (_: Set[?], name)          => Set(s"$name.sentinel.test")
    case (_: String, name)          => s"sentinel-$name"
    case (_: Cinema, _)             => Multikino
    case (other, name) =>
      fail(s"CinemaScraper.$name answers a ${other.getClass.getName}; teach unlikeTheDefault a value of that type")
  }

  private val (delegate, _) = TraitDecorators.recording(classOf[CinemaScraper], m =>
    if (listingMembers(m.getName)) TraitDecorators.sampleAnswer(m.getReturnType) else unlikeTheDefault(m))

  private val executor = Executors.newSingleThreadExecutor()

  /** How to build each decorator around a delegate, and the members that are its own. */
  private val decorators: Map[Class[?], (CinemaScraper => CinemaScraper, Set[String])] = Map(
    classOf[RetryingCinemaScraper]  -> ((d: CinemaScraper) => new RetryingCinemaScraper(d), Set.empty[String]),
    classOf[AdaptiveTimeoutScraper] -> ((d: CinemaScraper) => new AdaptiveTimeoutScraper(d, new HostScrapeStats(), executor), Set.empty[String]),
    classOf[UptimeRecordingScraper] -> ((d: CinemaScraper) => new UptimeRecordingScraper(d, new UptimeMonitor()), Set.empty[String]),
    classOf[SourceFallbackScraper]  -> ((d: CinemaScraper) => new SourceFallbackScraper(d,
      fallback = () => None, fallbackName = "Flicks", fallbackRef = () => None,
      new UptimeMonitor(), new InMemoryFallbackStore()), Set.empty[String]),
    // The chunked reduce publishes its listing as a stand-in for the live scraper; only
    // whether that listing is whole is its own (checked on its own below).
    classOf[PreScrapedCinemaScraper] -> ((d: CinemaScraper) => PreScrapedCinemaScraper.of(d, () => Seq.empty), Set("listingIsComplete")),
    // A composite rather than a decorator, but over ONE listing it is that listing.
    classOf[MultiListingScraper] -> ((d: CinemaScraper) => new MultiListingScraper(d.cinema, Seq(d)), Set.empty[String])
  )

  "The CinemaScraper members under test" should "include every member a decorator has to carry" in {
    members.map(_.getName) should contain allOf ("cinema", "scrapeHosts", "chain", "listingIsComplete",
      "maxFetchAttempts", "sourceUrl", "sourceKey", "chainVenueId")
    members.foreach(m => withClue(s"${m.getName}: ")(m.invoke(delegate) should not equal m.invoke(bare)))
  }

  "Every CinemaScraper decorator" should "be listed here, so it is held to the check below" in {
    TraitDecorators.discover(classOf[CinemaScraper], classOf[CinemaScraper]) shouldBe decorators.keySet
  }

  decorators.toSeq.sortBy(_._1.getSimpleName).foreach { case (decoratorClass, (decorate, ownMembers)) =>
    it should s"carry every delegate answer through ${decoratorClass.getSimpleName}" in {
      val decorated = decorate(delegate)
      val dropped = members.filterNot(m => ownMembers(m.getName)).collect {
        case m if m.invoke(decorated) != m.invoke(delegate) =>
          s"${m.getName}: delegate ${m.invoke(delegate)}, decorated ${m.invoke(decorated)}"
      }
      withClue(s"${decoratorClass.getSimpleName} answers these itself instead of carrying the delegate's — forward them (for a decorator, in DelegatingCinemaScraper):\n") {
        dropped shouldBe empty
      }
    }
  }

  "PreScrapedCinemaScraper" should "answer for its own listing's completeness" in {
    PreScrapedCinemaScraper.of(delegate, () => Seq.empty, listingComplete = false).listingIsComplete shouldBe false
    PreScrapedCinemaScraper.of(delegate, () => Seq.empty).listingIsComplete shouldBe true
  }

  it should "reach MovieCache as a short listing once the reduce says so" in {
    // End to end over the seam that broke: the reduce's PreScraped listing, wrapped the way
    // `publishScrape` wraps it, is what the runner reads the flag off.
    val partial = PreScrapedCinemaScraper.of(delegate, () => Seq.empty[CinemaMovie], listingComplete = false)
    new UptimeRecordingScraper(partial, new UptimeMonitor()).listingIsComplete shouldBe false
  }
}
