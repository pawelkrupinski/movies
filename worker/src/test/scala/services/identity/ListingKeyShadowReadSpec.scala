package services.identity

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{CinemaShowing, Country, KinoMuranow, Kinoteka, Showtime, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryScreeningsRepository, InMemorySlotsRepository, ListedShowtimes, ListingKey, SlotKeyed, UnreadableScreeningsRepository}
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
}
