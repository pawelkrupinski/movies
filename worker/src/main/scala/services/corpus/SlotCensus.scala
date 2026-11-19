package services.corpus

import models.{City, Source, SourceData, VenueClock, ShowtimesDigest}
import services.titles.TitleNormalizer
import services.readmodel.ReadModelProjection
import java.time.{ZoneId, ZoneOffset}

/** One venue slot's part: its title group ([[ReadModelProjection.titleKeyOf]]), its city, the zone its venue keeps
 *  time in, and its showtime starts — the lean slot's own array. */
final class SlotCensus private (val slot: SourceData, val titleKey: Option[String], val city: Option[String],
                                val zone: ZoneId, val starts: IArray[Int]) {
  /** How many starts fall after `minute`. */
  def startsAfter(minute: Int): Int = starts.length - firstAbove(minute)

  /** Whether a start falls in `[from, until)`. */
  def startsWithin(from: Int, until: Int): Boolean = {
    val i = firstAbove(from - 1)
    i < starts.length && starts(i) < until
  }

  /** The index of the first start above `minute`; the starts are ascending. */
  private def firstAbove(minute: Int): Int = {
    var low  = 0
    var high = starts.length
    while (low < high) { val mid = (low + high) >>> 1; if (starts(mid) <= minute) low = mid + 1 else high = mid }
    low
  }
}

object SlotCensus {
  /** The part of the slot at `source`, or `None` when `source` is no venue's: only venue slots take part. */
  def at(source: Source, slot: SourceData, normalizer: TitleNormalizer): Option[SlotCensus] =
    Source.cinemaOf(source).map(cinema =>
      new SlotCensus(slot, ReadModelProjection.titleKeyOf(slot, normalizer), City.forCinema(cinema).map(_.slug),
        VenueClock.zoneOf(cinema, ZoneOffset.UTC), ShowtimesDigest.startMinutes(slot)))
}
