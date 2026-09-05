package services.movies

import models.{Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * The re-key / fold MOVE of a film's side-collection rows.
 *
 * `replaceFilm` is upsert PLUS a delete of every key the payload does not name, so the
 * merge base — what is already filed at the destination — has to be READ before the write.
 * That read used the unchecked `findForFilm`, which reports a failure as an empty map: a
 * transient blip therefore read as "the destination is empty", the `$nin` deleted the rows
 * that were actually there, and the verification passed anyway because it only checked that
 * the MOVED keys had arrived. The old id was then deleted.
 *
 * The rule lives here rather than in either repository because a fake that re-stated it
 * (or, as the in-memory one did, skipped the verification entirely) lets every re-key spec
 * pass against a move production performs differently.
 */
class SideCollectionMoveSpec extends AnyFlatSpec with Matchers {

  private val times = Seq(Showtime(LocalDateTime.of(2027, 1, 1, 18, 0), None))

  /** A store whose reads can be told to fail for one specific film id. */
  private class Store(unreadable: Set[String] = Set.empty) {
    val rows = scala.collection.mutable.Map.empty[String, Map[String, Seq[Showtime]]]
    var deleted = List.empty[String]
    def read(id: String): (Map[String, Seq[Showtime]], Boolean) =
      if (unreadable(id)) (Map.empty, false) else (rows.getOrElse(id, Map.empty), true)
    /** Mirrors `replaceFilm`: upsert what is named, drop every key that is not. */
    def replace(id: String, payload: Map[String, Seq[Showtime]]): Boolean = {
      if (payload.isEmpty) rows.remove(id) else rows.update(id, payload); true
    }
    def deleteFilm(id: String): Unit = { rows.remove(id); deleted ::= id }
    def move(oldId: String, newId: String): Boolean =
      SideCollectionMove.move[Seq[Showtime]](oldId, newId, read, replace, deleteFilm)
  }

  "move" should "carry the source rows across and drop the old id" in {
    val store = new Store()
    store.rows("old|")      = Map("Kino A" -> times)
    store.rows("new|2026")  = Map("Kino B" -> times)

    store.move("old|", "new|2026") shouldBe true

    store.rows("new|2026").keySet should contain allOf ("Kino A", "Kino B")
    store.rows                    should not contain key ("old|")
    store.deleted                 shouldBe List("old|")
  }

  it should "KEEP the rows already at the destination when reading the destination fails" in {
    // The bug: an unreadable destination reads as empty, so the merge base is empty and
    // `replaceFilm`'s `$nin` deletes "Kino B" — a live cinema's showtimes, gone on a blip.
    val store = new Store(unreadable = Set("new|2026"))
    store.rows("old|")     = Map("Kino A" -> times)
    store.rows("new|2026") = Map("Kino B" -> times)

    store.move("old|", "new|2026") shouldBe false

    withClue("the destination's own rows must survive an unreadable merge base: ")(
      store.rows("new|2026").keySet shouldBe Set("Kino B"))
    withClue("and the source must not be deleted on a move that did not happen: ")(
      store.rows("old|").keySet shouldBe Set("Kino A"))
    store.deleted shouldBe empty
  }

  it should "refuse the move when the SOURCE read fails" in {
    // A failed source read is not "nothing to move" — and the caller deletes `oldId` next.
    val store = new Store(unreadable = Set("old|"))
    store.rows("old|") = Map("Kino A" -> times)

    store.move("old|", "new|2026") shouldBe false
    store.deleted                  shouldBe empty
  }

  it should "allow the rename when there is genuinely nothing filed under the old id" in {
    val store = new Store()
    store.move("old|", "new|2026") shouldBe true
  }

  it should "refuse when the write did not land" in {
    val store = new Store() {
      override def replace(id: String, payload: Map[String, Seq[Showtime]]): Boolean = false
    }
    store.rows("old|") = Map("Kino A" -> times)

    store.move("old|", "new|2026") shouldBe false
    store.rows("old|").keySet      shouldBe Set("Kino A")
    store.deleted                  shouldBe empty
  }

  it should "type-check against the slot map too, not just showtimes" in {
    // Both side collections share this rule; the slot store is the other caller.
    val slots = new InMemorySlotsRepository
    slots.upsertSlot("old|", "Kino A", SourceData(title = Some("A")))

    SideCollectionMove.move[SourceData]("old|", "new|2026",
      slots.findForFilmChecked, (id, rows) => slots.replaceFilm(id, rows), slots.deleteFilm) shouldBe true

    slots.findForFilm("new|2026").keySet shouldBe Set("Kino A")
    slots.findForFilm("old|")            shouldBe empty
  }
}
