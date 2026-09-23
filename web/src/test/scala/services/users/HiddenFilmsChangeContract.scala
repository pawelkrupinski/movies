package services.users

import models.UserState
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/** What `UserStateRepository.changeHiddenFilms` must do, stated once and run
 *  against every store — the in-memory one here in `UserStateRepositorySpec`,
 *  Mongo's update pipeline in `UserRepositoryIntegrationSpec`. The two
 *  implement the same meaning in two languages (Scala vs. an update pipeline),
 *  so only running one table of cases against both keeps them from drifting. */
trait HiddenFilmsChangeContract { this: AnyFlatSpec & Matchers =>

  /** The store under test, and the user ids its rows may use (a shared
   *  database's specs must stay out of each other's rows). */
  protected def hiddenFilmsStore: UserStateRepository
  protected def userIdPrefix: String

  /** A user with no row yet, whatever an earlier run left behind. */
  private def userId(suffix: String): String = {
    val id = s"$userIdPrefix$suffix"
    hiddenFilmsStore.delete(id)
    id
  }

  private val Stamp = Instant.parse("2026-05-19T12:00:00.123Z")

  private def change(id: String, c: HiddenFilmsChange, now: Instant = Stamp, country: String = "pl"): UserState =
    hiddenFilmsStore.changeHiddenFilms(id, country, c, now).value

  def hiddenFilmsChangeBehaviour(storeName: String): Unit = {

    s"$storeName.changeHiddenFilms" should "create the row on a user's first hide, with every field a read expects" in {
      val id    = userId("first-hide")
      val after = change(id, HiddenFilmsChange.Hide("Sing", 10))
      after shouldBe UserState(id, Set.empty, Set.empty, Stamp, Map("pl" -> Set("Sing")))
      hiddenFilmsStore.find(id).value shouldBe after
    }

    it should "add, re-add idempotently, and remove titles — touching only that country" in {
      val id = userId("add-remove")
      hiddenFilmsStore.upsert(UserState(id, Set("Legacy"), Set("Kino"), Stamp, Map("us" -> Set("Sing")), Some("en")))
      change(id, HiddenFilmsChange.Hide("Madagaskar", 10), Stamp.plusSeconds(1))
      change(id, HiddenFilmsChange.Hide("Sing", 10), Stamp.plusSeconds(2))
      change(id, HiddenFilmsChange.Hide("Sing", 10), Stamp.plusSeconds(3)).hiddenFilmsByCountry("pl") shouldBe Set("Madagaskar", "Sing")
      val after = change(id, HiddenFilmsChange.Unhide("Madagaskar"), Stamp.plusSeconds(4))
      after shouldBe UserState(id, Set("Legacy"), Set("Kino"), Stamp.plusSeconds(4), Map("us" -> Set("Sing"), "pl" -> Set("Sing")), Some("en"))
    }

    it should "clear only that country's bucket, and be harmless on a user with no row" in {
      val id = userId("clear")
      hiddenFilmsStore.upsert(UserState(id, Set.empty, Set.empty, Stamp, Map("pl" -> Set("A", "B"), "us" -> Set("C"))))
      change(id, HiddenFilmsChange.Clear, Stamp.plusSeconds(1)).hiddenFilmsByCountry shouldBe Map("pl" -> Set.empty, "us" -> Set("C"))
      change(userId("clear-no-row"), HiddenFilmsChange.Clear).hiddenFilmsByCountry shouldBe Map("pl" -> Set.empty)
    }

    it should "decline a NEW title into a full bucket — writing nothing, updatedAt included — but re-accept one already there" in {
      val id   = userId("full")
      val full = UserState(id, Set.empty, Set.empty, Stamp, Map("pl" -> Set("A", "B")))
      hiddenFilmsStore.upsert(full)
      change(id, HiddenFilmsChange.Hide("C", 2), Stamp.plusSeconds(1)) shouldBe full
      hiddenFilmsStore.find(id).value shouldBe full
      change(id, HiddenFilmsChange.Hide("A", 2), Stamp.plusSeconds(1)).updatedAt shouldBe Stamp.plusSeconds(1)
    }

    it should "move updatedAt one millisecond past the stored one when the clock hasn't passed it" in {
      val id = userId("same-ms")
      change(id, HiddenFilmsChange.Hide("A", 10)).updatedAt shouldBe Stamp
      change(id, HiddenFilmsChange.Hide("B", 10)).updatedAt shouldBe Stamp.plusMillis(1)
      change(id, HiddenFilmsChange.Unhide("B"), Stamp.minusSeconds(60)).updatedAt shouldBe Stamp.plusMillis(2)
    }

    it should "store a title verbatim even when it looks like an operator or a field path" in {
      val id     = userId("literal")
      val titles = Set("$size", "$hiddenFilms", "a.b", "$$ROOT")
      titles.foreach(t => change(id, HiddenFilmsChange.Hide(t, 10)))
      hiddenFilmsStore.find(id).value.hiddenFilmsByCountry("pl") shouldBe titles
      change(id, HiddenFilmsChange.Unhide("$size")).hiddenFilmsByCountry("pl") shouldBe titles - "$size"
    }
  }
}
