package services.identity

import models.{Cinema, Source, SourceData, ListingKey, ShowtimesDigest}
import services.titles.TitleNormalizer

import IdentityProjectionPlan.{RowBatch, VenueGroup}

/** The LEAN venue slots of one draft ([[ScopeDrafting]]): each venue's from the memo when nothing it is built from
 *  moved, else built from the venue's rows, read a batch of venues at a time and let go. Built per draft: what it holds
 *  is that draft's. `previousOf`: the films the scope's listings were on, whose stored slots a restarted worker's memo
 *  reuses. */
private[identity] final class LeanVenueSlots(index: ProjectionIndex, previousOf: Map[ListingKey, StoredFilmRef],
                                              normalizer: TitleNormalizer, slotSource: VenueSlotSource, memo: VenueSlotMemo) {
  import index.storedById

  // Each stored film's venue slots by venue, worked out once per film, not once per venue of every film it lends a
  // listing to — a US film showing at hundreds of venues was scanned whole for each of them.
  private val slotsByFilm = scala.collection.mutable.HashMap.empty[String, Map[Cinema, Seq[(Source, SourceData)]]]
  private def slotsByVenueOf(id: String): Map[Cinema, Seq[(Source, SourceData)]] = slotsByFilm.getOrElseUpdate(id,
    storedById.get(id).fold(Map.empty[Cinema, Seq[(Source, SourceData)]])(stored => VenueSlots.byVenue(stored.record)))

  private val venueSlots = scala.collection.mutable.HashMap.empty[VenueSlotMemo.Key, Seq[(Source, SourceData)]]

  /** The lean slots of the venue group under `key`, once [[buildAll]] has put them in. */
  def apply(key: VenueSlotMemo.Key): Seq[(Source, SourceData)] = venueSlots(key)

  // The slots the stored film of a group's listings holds at its venue: what a restarted worker's memo reuses
  // when they are recorded as built from the group's key.
  private def storedAt(group: VenueGroup): Option[Seq[(Source, SourceData)]] =
    group.rows.flatMap(r => previousOf.get(r.listing.key)).map(_.id).distinct match {
      case Seq(id) => storedById.get(id).map(_ => slotsByVenueOf(id).getOrElse(group.cinema, Nil))
      case _ => None
    }

  /** Every planned venue's lean slots: a kept venue's as kept (`kept`), a worked-out one's (`worked`) from the memo,
   *  else built from its venue's rows, read a batch of venues at a time. */
  def buildAll(kept: Seq[VenueShape], worked: Seq[VenueGroup]): Unit = {
    kept.foreach(venue => venueSlots(venue.memoKey) = venue.lean)
    val pending = worked.filter(group =>
      memo.lookup(group.key, storedAt(group)).fold(true) { lean => venueSlots(group.key) = lean; false })
    pending.groupBy(_.cinema).toSeq.sortBy(_._1.displayName).grouped(RowBatch).foreach(buildBatch)
  }

  private def buildBatch(batch: Seq[(Cinema, Seq[VenueGroup])]): Unit = {
    val fetched = slotSource.rowsOf(batch.map(_._1).toSet)
    batch.foreach { case (cinema, groups) =>
      val rows = fetched.getOrElse(cinema, Map.empty)
      groups.foreach { group =>
        // Over the whole index: a slot's prior is its representative listing's film's, whichever of the venue's listings that is.
        val lean = slotSource.build(index, cinema, group.rows.map(_.listing.key), rows).map { case (source, slot) => source -> ShowtimesDigest.stripSlot(slot) }
        venueSlots(group.key) = lean
        memo.store(group.key, lean, keep = VenueSlots.readAsListed(cinema, group.rows, rows, normalizer), writtenAs = Some(group.counter))
      }
    }
  }
}

