package services.readmodel

import tools.SpecClock.given

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CaffeineMovieCache, FilmId, FilmWriteFence, InMemoryMovieRepository, InMemoryScreeningsRepository, StoredMovieRecord, VenueSlots}
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime
import scala.util.Random

/**
 * A change confined to some venues' showtimes, applied from those venues alone
 * ([[ReadModelProjector.onVenueSlots]]), must leave the read model EXACTLY as projecting the whole
 * film again leaves it — or decline and leave it to the whole-film projection. Random films (several
 * cinemas, display-title variants, venues holding two slots of the film) take random showtime changes
 * (some venues gaining their first showtimes or losing their last) one way on one projector and the
 * other way on another, and the two read models must agree after every change.
 */
class VenueSlotsEquivalenceSpec extends AnyFlatSpec with Matchers {

  private val clock   = java.time.Clock.fixed(java.time.Instant.parse("2026-06-01T10:00:00Z"), java.time.ZoneOffset.UTC)
  private val cinemas = Cinema.all.filter(City.forCinema(_).isDefined).take(8)
  private val titles  = Seq(Some("Foo"), Some("Foo"), Some("Foo"), None, Some("Foo (napisy)"), Some("Фу"))
  private val times   = (10 to 22 by 2).map(h => Showtime(LocalDateTime.of(2026, 6, 7, h, 0), Some(s"https://book/$h")))

  private def showtimes(rng: Random): Seq[Showtime] = if (rng.nextInt(5) == 0) Nil else rng.shuffle(times).take(1 + rng.nextInt(4))

  private def film(rng: Random): MovieRecord = {
    val slots = cinemas.flatMap { cinema =>
      (0 until rng.nextInt(3)).map { i =>
        val title = titles(rng.nextInt(titles.size))
        CinemaShowing(cinema, s"foo$i") -> SourceData(title = title, filmUrl = Some(s"https://${cinema.displayName}/foo$i"),
          showtimes = showtimes(rng))
      }
    }
    MovieRecord(tmdbId = Some(1), imdbRating = Some(7.5), data = (slots :+ (Tmdb -> SourceData(title = Some("Foo")))).toMap[Source, SourceData])
  }

  // `SourceData`'s equality is showtime-blind by design (and blind to its cache-only digests), so a
  // comparison of records proves nothing here: every field of every slot, arrays by content.
  private def everyField(record: MovieRecord) =
    (record, record.data.toSeq.map { case (source, slot) => source.displayName -> slot.productIterator.map {
      case array: Array[?]       => array.toSeq
      case Some(array: Array[?]) => Some(array.toSeq)
      case other                 => other
    }.toList }.sortBy(_._1))

  /** Every field of every case class reached, arrays by content and maps in a stable order — the
   *  index's records and slots compare with the showtime-blind equalities otherwise. */
  private def deep(value: Any): Any = value match {
    case array: Array[?]                  => array.toSeq.map(deep)
    case map: scala.collection.Map[?, ?]  => map.toSeq.map { case (k, v) => (deep(k), deep(v)) }.sortBy(_._1.toString)
    case set: scala.collection.Set[?]     => set.toSeq.map(deep).sortBy(_.toString)
    case seq: Iterable[?]                 => seq.toSeq.map(deep)
    case product: Product                 => (product.productPrefix, product.productIterator.map(deep).toList)
    case other                            => other
  }

  private def stored(record: MovieRecord) = StoredMovieRecord.synthesised("Foo", Some(2024), record, titleNormalizer)

  private def projector(rm: InMemoryReadModelRepository, bootStudy: Option[BootCorpusStudy] = None,
                        source: InMemoryMovieRepository = new InMemoryMovieRepository(normalizer = titleNormalizer)) =
    new ReadModelProjector(source, rm, rm, clock = clock, bootStudy = bootStudy)

  "A change at some venues" should "write from those venues alone exactly what projecting the whole film writes" in {
    var accepted, declined = 0
    val reasons = scala.collection.mutable.Set.empty[String]
    (0 until 300).foreach { round =>
      val rng = new Random(round)
      val (wholeRm, venueRm) = (new InMemoryReadModelRepository(), new InMemoryReadModelRepository())
      val (whole, venue)     = (projector(wholeRm), projector(venueRm))
      var record = film(rng)
      whole.onMovieUpsert(stored(record)); venue.onMovieUpsert(stored(record))
      (0 until 6).foreach { step =>
        val touched = rng.shuffle(cinemas).take(1 + rng.nextInt(2)).toSet
        record = record.copy(data = record.data.map {
          case (s @ CinemaShowing(cinema, _), slot) if touched(cinema) => s -> slot.copy(showtimes = showtimes(rng))
          case other => other
        })
        val now = stored(record)
        whole.onMovieUpsert(now)
        val atCinemas = touched.map(cinema => cinema -> record.data.toSeq.collect {
          case (s @ CinemaShowing(`cinema`, _), slot) => s -> slot }).toMap
        venue.onVenueSlots(VenueSlots(FilmId(now.id.value), atCinemas)) match {
          case services.movies.VenueVerdict.Applied          => accepted += 1
          case services.movies.VenueVerdict.Declined(reason) => declined += 1; reasons += reason; venue.onMovieUpsert(now)
        }
        withClue(s"round $round step $step: ") {
          venueRm.findAllMovies().toSet shouldBe wholeRm.findAllMovies().toSet
          venueRm.findAllScreenings().toSet shouldBe wholeRm.findAllScreenings().toSet
        }
      }
      whole.stop(); venue.stop()
    }
    // Both paths must actually have run, or the agreement above proves nothing.
    accepted should be > 300
    declined should be > 100
    // Every decline names why, and the random changes reach the reasons that are about presence.
    import services.movies.ChangeStreamMetrics.VenueDecline as Why
    reasons.toSet should contain allOf (Why.ProjectorVenueAppears, Why.ProjectorVenueVanishes)
    reasons.toSet.subsetOf(Why.All.toSet) shouldBe true
  }

  "The cache" should "hold after a change at some venues, applied from them alone, exactly what the whole film's re-read leaves" in {
    var accepted, declined = 0
    (0 until 200).foreach { round =>
      val rng = new Random(10000 + round)
      // Showtimes in their own collection, as in production: what the cache strips its slots for.
      def cache() = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer,
        screenings = Some(new InMemoryScreeningsRepository)), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
      val (whole, venue) = (cache(), cache())
      var record = film(rng)
      whole.applyUpsert(stored(record), FilmWriteFence.Unfenced); venue.applyUpsert(stored(record), FilmWriteFence.Unfenced)
      (0 until 6).foreach { step =>
        val touched = rng.shuffle(cinemas).take(1 + rng.nextInt(2)).toSet
        record = record.copy(data = record.data.map {
          case (s @ CinemaShowing(cinema, _), slot) if touched(cinema) => s -> slot.copy(showtimes = showtimes(rng))
          case other => other
        })
        val now = stored(record)
        whole.applyUpsert(now, FilmWriteFence.Unfenced)
        val atCinemas = touched.map(cinema => cinema -> record.data.toSeq.collect {
          case (s @ CinemaShowing(`cinema`, _), slot) => s -> slot }).toMap
        if (venue.applyVenueSlots(VenueSlots(FilmId(now.id.value), atCinemas), FilmWriteFence.Unfenced) == services.movies.VenueVerdict.Applied) accepted += 1
        else { declined += 1; venue.applyUpsert(now, FilmWriteFence.Unfenced) }
        val key = now.cacheKey(titleNormalizer)
        withClue(s"round $round step $step: ") {
          venue.get(key).map(everyField) shouldBe whole.get(key).map(everyField)
          // …and its index is the whole-row store's.
          deep(venue.indexSnapshot) shouldBe deep(whole.indexSnapshot)
        }
      }
    }
    accepted shouldBe 1200
    declined shouldBe 0
  }

  // After a boot the projector knows only the read model's contents, not the rows they came from: it
  // learns them from the boot hydrate's read, and a film it never learned is re-read whole.
  private final class Restart(seed: Long) {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val (beforeRm, afterRm) = (new InMemoryReadModelRepository(), new InMemoryReadModelRepository())
    val rng    = new Random(seed)
    var record = film(rng)
    while (!record.data.exists { case (CinemaShowing(_, _), slot) => slot.showtimes.nonEmpty; case _ => false }) record = film(rng)
    repository.upsert("Foo", Some(2024), record)
    def row = repository.findAll().head
    // the process before the restart projected it, into both read models
    Seq(beforeRm, afterRm).foreach { rm => val p = projector(rm); p.onMovieUpsert(row); p.stop() }
    val whole = projector(beforeRm)                          // a projector that keeps projecting whole
    whole.onMovieUpsert(row)
    val study  = new BootCorpusStudy(titleNormalizer)
    study.bootPage(Seq(row))                                 // the restarted worker's hydrate read, before any change
    study.bootReadEnded(services.movies.BootReadEnd.Whole)
    val booted = projector(afterRm, Some(study))             // the restarted worker's
    def cinema = record.data.collectFirst { case (CinemaShowing(c, _), slot) if slot.showtimes.nonEmpty => c }.get
    def change(at: models.Cinema): VenueSlots = {
      record = record.copy(data = record.data.map {
        case (s @ CinemaShowing(`at`, _), slot) => s -> slot.copy(showtimes = slot.showtimes.take(1) :+ times.last)
        case other => other
      })
      repository.upsert("Foo", Some(2024), record)
      whole.onMovieUpsert(row)
      VenueSlots(row.id, Map(at -> record.data.toSeq.collect { case (s @ CinemaShowing(`at`, _), slot) => s -> slot }))
    }
  }

  "The projector" should "learn a row from the boot hydrate's read, and apply its venues alone after" in {
    val world = new Restart(7)
    import world.*
    booted.prepare()
    val venues = change(cinema)
    booted.onVenueSlots(venues) shouldBe services.movies.VenueVerdict.Applied
    afterRm.findAllMovies().toSet shouldBe beforeRm.findAllMovies().toSet
    afterRm.findAllScreenings().toSet shouldBe beforeRm.findAllScreenings().toSet
    booted.onVenueSlots(VenueSlots(FilmId("absent|2024"), venues.atCinemas)) shouldBe
      services.movies.VenueVerdict.Declined(services.movies.ChangeStreamMetrics.VenueDecline.ProjectorRowUnprojected)
    whole.stop(); booted.stop()
  }

  it should "apply a change at one venue of a card whose other row drifted, leaving that row to the content check" in {
    val world = new Restart(11)
    import world.*
    val drifted = afterRm.findAllScreenings().head
    val stale   = drifted.copy(showtimes = Seq(times.head))
    afterRm.upsertScreening(stale)                                                       // the read model drifted
    booted.prepare()
    record.data.collectFirst { case (CinemaShowing(c, _), slot) if slot.showtimes.nonEmpty && c.displayName != drifted.cinema => c }
      .foreach { at =>
        booted.onVenueSlots(change(at)) shouldBe services.movies.VenueVerdict.Applied
        afterRm.findAllScreenings().find(_._id == drifted._id) shouldBe Some(stale)        // untouched: the content check's
        afterRm.findAllScreenings().filter(_.cinema == at.displayName).toSet shouldBe
          beforeRm.findAllScreenings().filter(_.cinema == at.displayName).toSet
      }
    whole.stop(); booted.stop()
  }

  // A boot whose hydrate read was not handed over (it failed, or its derivation ran past its wait) used to leave every
  // film's first venue change re-read whole for the process's life: 78 of the US's 78 venue applies, 2026-10-01.
  it should "learn the rows from a corpus read of its own when the boot had none to hand over" in {
    // A film every venue of which lists showtimes (the boot's heal projects any other whole, which teaches it too),
    // changing at a venue that lists it once (one listing twice at a venue is always re-read whole).
    val world = Iterator.from(17).map(seed => new Restart(seed.toLong)).find { w =>
      w.record.data.forall { case (_: CinemaShowing, slot) => slot.showtimes.nonEmpty; case _ => true } &&
        w.record.data.keys.count { case CinemaShowing(c, _) => c == w.cinema; case _ => false } == 1
    }.get
    import world.*
    val missing = new BootCorpusStudy(titleNormalizer)
    missing.bootReadEnded(services.movies.BootReadEnd.GaveUp)
    // Its own copy of the store as it stood at the boot, so the change below reaches it only through the venue path.
    val source = new InMemoryMovieRepository(Seq(("Foo", Some(2024), record)), normalizer = titleNormalizer)
    source.findAll().map(_.id) shouldBe Seq(row.id)
    val learning = projector(afterRm, Some(missing), source = source)
    learning.prepare()
    val venues = change(cinema)
    org.scalatest.concurrent.Eventually.eventually(org.scalatest.concurrent.Eventually.timeout(_root_.tools.SpecTimeouts.Settle)) {
      learning.onVenueSlots(venues) shouldBe services.movies.VenueVerdict.Applied
    }
    afterRm.findAllMovies().toSet shouldBe beforeRm.findAllMovies().toSet
    afterRm.findAllScreenings().toSet shouldBe beforeRm.findAllScreenings().toSet
    whole.stop(); booted.stop(); learning.stop()
  }

  // A row no read taught it — written since, or never ready — is re-read whole on its first change, at once.
  it should "decline a change at a row it never learned, at once" in {
    val world = new Restart(13)
    import world.*
    val unlearned = projector(afterRm)
    unlearned.prepare()
    unlearned.onVenueSlots(change(cinema)) shouldBe
      services.movies.VenueVerdict.Declined(services.movies.ChangeStreamMetrics.VenueDecline.ProjectorRowUnprojected)
    whole.stop(); booted.stop(); unlearned.stop()
  }
}
