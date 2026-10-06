package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.{Showing, SourceHit, SourceRecord, VoterFamily}
import tools.MutableClock

import java.time.Instant
import scala.concurrent.duration._

/** What another family answered is read back as filed — and a question never filed is a gap, never "no film"; a search
 *  is asked again after months, a record after a year. */
class FamilyAnswerStoreSpec extends AnyFlatSpec with Matchers {

  private def world() = { val clock = new MutableClock(Instant.parse("2026-10-04T00:00:00Z")); (new FamilyAnswerStore(new InMemoryTmdbDocuments, clock), clock) }

  "a family's answers" should "be a gap until filed, then read back as filed" in {
    val (store, _) = world()
    val imdb = store.answers(VoterFamily.Imdb)
    imdb.titled("Snow Leopard") shouldBe Answer.Unknown
    imdb.record("tt13920372") shouldBe Answer.Unknown
    val hits = Seq(SourceHit("tt13920372", "Snow Leopard", None, Some(2020)), SourceHit("tt21223152", "Snow Leopard", Some("Xue bao"), None))
    store.fileTitled(VoterFamily.Imdb, "Snow Leopard", hits)
    val record = SourceRecord(IdentityMeasures.Film("Snow Leopard", None, Nil, Some(2020), Some(98), Some(Seq("Lixing Wang")), Some(Seq("CN"))),
      Map("imdb" -> "tt13920372"))
    store.fileRecord(VoterFamily.Imdb, "tt13920372", Some(record))
    store.fileRecord(VoterFamily.Imdb, "tt0000001", None)
    imdb.titled("Snow Leopard") shouldBe Answer.Known(hits)
    imdb.record("tt13920372") shouldBe Answer.Known(Some(record))
    imdb.record("tt0000001") shouldBe Answer.Known(None)
    store.answers(VoterFamily.RottenTomatoes).titled("Snow Leopard") shouldBe Answer.Unknown
  }

  it should "be wanted again once older than its kind keeps it, and read meanwhile" in {
    val (store, clock) = world()
    store.fileTitled(VoterFamily.Filmweb, "Klondike", Nil)
    store.fileRecord(VoterFamily.Filmweb, "880000", None)
    store.wanted(FamilyAnswerStore.titleId(VoterFamily.Filmweb, "Klondike")) shouldBe false
    clock.advanceMillis((FamilyAnswerStore.SearchAge + 1.day).toMillis)
    store.wanted(FamilyAnswerStore.titleId(VoterFamily.Filmweb, "Klondike")) shouldBe true
    store.answers(VoterFamily.Filmweb).titled("Klondike") shouldBe Answer.Known(Nil)
    store.wanted(FamilyAnswerStore.recordId(VoterFamily.Filmweb, "880000")) shouldBe false
    store.wanted(FamilyAnswerStore.titleId(VoterFamily.Filmweb, "never asked")) shouldBe true
  }

  "a venue's programme" should "be a gap until filed, then read back as filed, and be asked again the next day" in {
    val (store, clock) = world()
    val filmweb = store.answers(VoterFamily.Filmweb)
    filmweb.showing("Kozienicki Dom Kultury") shouldBe Answer.Unknown
    val programme = Seq(Showing("10085635", ScreeningDays.of(Seq(java.time.LocalDate.parse("2026-10-07")))),
      Showing("10057628", ScreeningDays.of(Seq("2026-10-05", "2026-10-07").map(java.time.LocalDate.parse))))
    store.fileShowing(VoterFamily.Filmweb, "Kozienicki Dom Kultury", programme)
    store.fileShowing(VoterFamily.Filmweb, "Kino Nowe", Nil)
    filmweb.showing("Kozienicki Dom Kultury") shouldBe Answer.Known(programme)
    filmweb.showing("Kino Nowe") shouldBe Answer.Known(Nil)
    val id = FamilyAnswerStore.showingId(VoterFamily.Filmweb, "Kozienicki Dom Kultury")
    store.wanted(id) shouldBe false
    clock.advanceMillis((FamilyAnswerStore.ShowingAge + 1.minute).toMillis)
    store.wanted(id) shouldBe true
    filmweb.showing("Kozienicki Dom Kultury") shouldBe Answer.Known(programme)
  }
}
