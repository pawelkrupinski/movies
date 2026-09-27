package services.identity

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{CinemaShowing, Country, KinoMuranow, Kinoteka, MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepository, InMemoryScreeningsRepository, InMemorySlotsRepository, ListedShowtimes, ListingKey,
  ShowtimesDigest, SingleCountryNormalizer, SlotKeyed, UnreadableScreeningsRepository}
import settings.ListingKeyShadowSample

import java.time.LocalDateTime
import scala.util.Random

/**
 * The shadow read: each sampled listing read by today's slot key and by its `listingKey`, and the
 * two answers compared, per collection.
 */
class ListingKeyShadowReadSpec extends AnyFlatSpec with Matchers {

  private val show     = Showtime(LocalDateTime.of(2099, 3, 1, 18, 0), None)
  private val muranow  = CinemaShowing(KinoMuranow, "belle").displayName
  private val kinoteka = CinemaShowing(Kinoteka, "belle").displayName
  private val paged    = SourceData(title = Some("Belle"), filmUrl = Some("https://muranow.pl/belle"))
  private val pageless = SourceData(title = Some("Belle"), releaseYear = Some(2013))

  private def stamped(slotKey: String, slot: SourceData) = ListedShowtimes(Seq(show), ListingKey.ofSlotRow(slotKey, slot))

  private def shadow(slots: InMemorySlotsRepository, screenings: services.movies.ScreeningsRepository, sample: Int = 100) = {
    val gauge = ListingKeyShadowRead.gauge(new PrometheusRegistry())
    (new ListingKeyShadowRead(slots, screenings, ListingKeyShadowSample(sample), gauge, Country.Poland, new Random(7)), gauge)
  }

  "ListingKeyShadowRead" should "find every stamped listing's rows by listingKey exactly as by slot key" in {
    val slots = new InMemorySlotsRepository; val screenings = new InMemoryScreeningsRepository
    slots.upsertSlot("belle|2013", muranow, paged);    screenings.upsertSlot("belle|2013", muranow, stamped(muranow, paged))
    slots.upsertSlot("belle|2013", kinoteka, pageless) // a slot with no showtimes row: both reads find none
    slots.upsertSlot("belle|2013", Tmdb.displayName, SourceData(title = Some("Belle")))   // no listing: never sampled

    val (read, gauge) = shadow(slots, screenings)
    val report = read.compare().get
    report.compared.map(_.rowId).toSet shouldBe Set(SlotKeyed.idOf("belle|2013", muranow), SlotKeyed.idOf("belle|2013", kinoteka))
    report.disagreements shouldBe empty
    read.sample()
    gauge.labelValues("pl", ListingKeyShadowRead.Agree).get() shouldBe 2.0
    gauge.labelValues("pl", ListingKeyShadowRead.SlotsDisagree).get() shouldBe 0.0
  }

  it should "report a screenings row keyed apart from its slot, and one listing filed on two films" in {
    val slots = new InMemorySlotsRepository; val screenings = new InMemoryScreeningsRepository
    slots.upsertSlot("belle|2013", muranow, paged)
    screenings.upsertSlot("belle|2013", muranow, ListedShowtimes(Seq(show), None))      // unstamped showtimes
    slots.upsertSlot("belle|2013", kinoteka, pageless)
    slots.upsertSlot("belle|2021", kinoteka, pageless)                                  // the same listing, twice

    val (read, gauge) = shadow(slots, screenings)
    val report = read.compare().get
    report.disagreements.map(c => c.rowId -> (c.slotsAgree, c.screeningsAgree)).toMap shouldBe Map(
      SlotKeyed.idOf("belle|2013", muranow)  -> (true, false),
      SlotKeyed.idOf("belle|2013", kinoteka) -> (false, true),
      SlotKeyed.idOf("belle|2021", kinoteka) -> (false, true))
    read.sample()
    gauge.labelValues("pl", ListingKeyShadowRead.Agree).get() shouldBe 0.0
    gauge.labelValues("pl", ListingKeyShadowRead.SlotsDisagree).get() shouldBe 2.0
    gauge.labelValues("pl", ListingKeyShadowRead.ScreeningsDisagree).get() shouldBe 1.0
  }

  it should "compare at most the sample size per tick" in {
    val slots = new InMemorySlotsRepository
    (1 to 20).foreach(i => slots.upsertSlot(s"f$i|2020", muranow, paged.copy(filmUrl = Some(s"https://muranow.pl/$i"))))
    shadow(slots, new InMemoryScreeningsRepository, sample = 5)._1.compare().get.compared should have size 5
  }

  it should "publish nothing when a read by slot key failed" in {
    val slots = new InMemorySlotsRepository
    slots.upsertSlot("belle|2013", muranow, paged)
    shadow(slots, new UnreadableScreeningsRepository)._1.compare() shouldBe None
  }

  // A venue moved from Filmweb to its own-site scraper: its raw title lost " - KNT", so its listing
  // key moved, while its showtimes did not. The re-scrape reaches the store as a PATCH of the
  // cache's stripped records (`MovieCache.putIfPresent` -> `updateIfPresent`), never the
  // whole-record upsert, so the patch itself must carry the new key onto the screenings row.
  it should "still agree after a re-scrape changes only a venue's raw title" in {
    val slots = new InMemorySlotsRepository; val screenings = new InMemoryScreeningsRepository
    val repository = new InMemoryMovieRepository(screenings = Some(screenings), slots = Some(slots),
                                                 normalizer = SingleCountryNormalizer.titleNormalizer)
    val venue   = CinemaShowing(Kinoteka, "belle")
    val scraped = MovieRecord(data = Map[Source, SourceData](venue -> pageless.copy(rawTitle = Some("Belle - KNT"), showtimes = Seq(show))))
    val renamed = scraped.copy(data = Map[Source, SourceData](venue -> pageless.copy(rawTitle = Some("Belle"), showtimes = Seq(show))))
    repository.upsert("Belle", Some(2013), scraped)
    val filmId = screenings.filmIdsChecked()._1.head

    repository.updateIfPresent("Belle", Some(2013), ShowtimesDigest.stripForCache(scraped), ShowtimesDigest.stripForCache(renamed)) shouldBe true

    withClue("the screenings row keeps its showtimes and takes the listing's new key: ")(
      screenings.findListedForFilmChecked(filmId)._1 shouldBe Map(kinoteka -> ListedShowtimes(Seq(show), ListingKey.ofSource(venue, renamed.data(venue)))))
    shadow(slots, screenings)._1.compare().get.disagreements shouldBe empty
  }
}
