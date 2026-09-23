package services.users

import models.UserState
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/** What the atomic writes — `UserStateRepository.changeHiddenFilms` and
 *  `patchLegacyState` — must do, stated once and run
 *  against every store — the in-memory one here in `UserStateRepositorySpec`,
 *  Mongo's update pipeline in `UserRepositoryIntegrationSpec`. The two
 *  implement the same meaning in two languages (Scala vs. an update pipeline),
 *  so only running one table of cases against both keeps them from drifting. */
trait UserStateWritesContract { this: AnyFlatSpec & Matchers =>

  /** The store under test, and the user ids its rows may use (a shared
   *  database's specs must stay out of each other's rows). */
  protected def writesStore: UserStateRepository
  protected def userIdPrefix: String

  /** A user with no row yet, whatever an earlier run left behind. */
  private def userId(suffix: String): String = {
    val id = s"$userIdPrefix$suffix"
    writesStore.delete(id)
    id
  }

  private val Stamp = Instant.parse("2026-05-19T12:00:00.123Z")

  private def change(id: String, c: HiddenFilmsChange, now: Instant = Stamp, country: String = "pl"): UserState =
    writesStore.changeHiddenFilms(id, country, c, now).value

  def atomicWritesBehaviour(storeName: String): Unit = {

    s"$storeName.changeHiddenFilms" should "create the row on a user's first hide, with every field a read expects" in {
      val id    = userId("first-hide")
      val after = change(id, HiddenFilmsChange.Hide("Sing", 10))
      after shouldBe UserState(id, Set.empty, Set.empty, Stamp, Map("pl" -> Set("Sing")))
      writesStore.find(id).value shouldBe after
    }

    it should "add, re-add idempotently, and remove titles — touching only that country" in {
      val id = userId("add-remove")
      writesStore.upsert(UserState(id, Set("Legacy"), Set("Kino"), Stamp, Map("us" -> Set("Sing")), Some("en")))
      change(id, HiddenFilmsChange.Hide("Madagaskar", 10), Stamp.plusSeconds(1))
      change(id, HiddenFilmsChange.Hide("Sing", 10), Stamp.plusSeconds(2))
      change(id, HiddenFilmsChange.Hide("Sing", 10), Stamp.plusSeconds(3)).hiddenFilmsByCountry("pl") shouldBe Set("Madagaskar", "Sing")
      val after = change(id, HiddenFilmsChange.Unhide("Madagaskar"), Stamp.plusSeconds(4))
      after shouldBe UserState(id, Set("Legacy"), Set("Kino"), Stamp.plusSeconds(4), Map("us" -> Set("Sing"), "pl" -> Set("Sing")), Some("en"))
    }

    it should "clear only that country's bucket, and be harmless on a user with no row" in {
      val id = userId("clear")
      writesStore.upsert(UserState(id, Set.empty, Set.empty, Stamp, Map("pl" -> Set("A", "B"), "us" -> Set("C"))))
      change(id, HiddenFilmsChange.Clear, Stamp.plusSeconds(1)).hiddenFilmsByCountry shouldBe Map("pl" -> Set.empty, "us" -> Set("C"))
      change(userId("clear-no-row"), HiddenFilmsChange.Clear).hiddenFilmsByCountry shouldBe Map("pl" -> Set.empty)
    }

    it should "decline a NEW title into a full bucket — writing nothing, updatedAt included — but re-accept one already there" in {
      val id   = userId("full")
      val full = UserState(id, Set.empty, Set.empty, Stamp, Map("pl" -> Set("A", "B")))
      writesStore.upsert(full)
      change(id, HiddenFilmsChange.Hide("C", 2), Stamp.plusSeconds(1)) shouldBe full
      writesStore.find(id).value shouldBe full
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
      writesStore.find(id).value.hiddenFilmsByCountry("pl") shouldBe titles
      change(id, HiddenFilmsChange.Unhide("$size")).hiddenFilmsByCountry("pl") shouldBe titles - "$size"
    }

    s"$storeName.patchLegacyState" should "set only the fields the patch carries, keeping the rest and every country's hides" in {
      val id     = userId("patch")
      val stored = UserState(id, Set("H"), Set("D"), Stamp, Map("pl" -> Set("Kept")), Some("pl"))
      writesStore.upsert(stored)
      val after  = writesStore.patchLegacyState(id, LegacyStatePatch(disabledCinemas = Some(Set("$Kino", "a.b")), language = Some(Some("en"))), Stamp.plusSeconds(1)).value
      after shouldBe stored.copy(disabledCinemas = Set("$Kino", "a.b"), language = Some("en"), updatedAt = Stamp.plusSeconds(1))
      writesStore.find(id).value shouldBe after
    }

    it should "clear the language on an explicit null, and empty a set on an empty one" in {
      val id = userId("patch-clear")
      writesStore.upsert(UserState(id, Set("H"), Set("D"), Stamp, language = Some("pl")))
      writesStore.patchLegacyState(id, LegacyStatePatch(hiddenFilms = Some(Set.empty), language = Some(None)), Stamp.plusSeconds(1)).value shouldBe
        UserState(id, Set.empty, Set("D"), Stamp.plusSeconds(1))
    }

    it should "create the row when there is none, and move updatedAt even within one millisecond" in {
      val id = userId("patch-new")
      writesStore.patchLegacyState(id, LegacyStatePatch(language = Some(Some("de"))), Stamp).value shouldBe
        UserState(id, Set.empty, Set.empty, Stamp, language = Some("de"))
      writesStore.patchLegacyState(id, LegacyStatePatch(), Stamp).value.updatedAt shouldBe Stamp.plusMillis(1)
    }
  }
}
