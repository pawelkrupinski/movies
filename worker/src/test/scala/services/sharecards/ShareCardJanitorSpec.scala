package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.InMemoryReadModelRepository
import ShareCardTestKit.*

import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime
import java.time.Instant

class ShareCardJanitorSpec extends AnyFlatSpec with Matchers {
  import ShareCardJanitorSpec.*

  "The daily prune" should "never delete the files of a film on screen, however old" in new Setup {
    onScreen("flive")
    putFilm("flive", old)
    janitor().prune().deleted shouldBe empty
    filmFiles("flive") shouldBe 3
  }

  it should "retire every file of a film gone from the corpus or off the screens, and re-project one still pointing at its card" in new Setup {
    readModel.upsertMovie(film(id = "fgone2").copy(shareCard = Some(ShareCardFile.url("fgone2", "0" * 16))))   // no screenings
    putFilm("fgone1", old); putFilm("fgone2", old)
    val refreshed = collection.mutable.Buffer.empty[String]
    janitor(refresh = refreshed += _).prune().deleted shouldBe Map("retired" -> 6)
    filmFiles("fgone1") shouldBe 0
    filmFiles("fgone2") shouldBe 0
    refreshed shouldBe Seq("fgone2")
  }

  it should "leave any file younger than the grace period alone, referenced or not" in new Setup {
    putFilm("fgone", young)
    put(s"fgone.jpg.${ShareCardStore.writerId}-deadbeef.tmp", young)
    janitor().prune().deleted shouldBe empty
    store.list() should have size 4
  }

  it should "delete temp files older than the grace period — abandoned writes" in new Setup {
    put("fx.jpg.otherhost-7-cafebabe.tmp", old)
    janitor().enforceBudget().deleted shouldBe Map("temp" -> 1)
    store.list() shouldBe empty
  }

  it should "delete nothing but abandoned temps when web_movies could not be read whole" in new Setup {
    putFilm("fgone", old)
    val blind = new InMemoryReadModelRepository {
      override def findAllShareCardRefsChecked() = (Seq.empty, false)
    }
    new ShareCardJanitor(store, blind, budgetBytes = 1, metrics, clock, _ => ()).prune().deleted shouldBe empty
    filmFiles("fgone") shouldBe 3
  }

  "The budget" should "count cards, bases and posters together and evict the oldest unreferenced files first" in new Setup {
    withCard("flive")
    putFilm("flive", old, size = 300)                                       // current: 900 bytes
    putFilm("fstale", old.minusSeconds(600), size = 200)                    // oldest, unreferenced
    putFilm("fnewer", old, size = 100)
    val report = janitor(budget = 1300).enforceBudget()
    report.deleted shouldBe Map("budget" -> 3)
    filmFiles("fstale") shouldBe 0                                           // the oldest went first
    report.bytes shouldBe 1200
    report.currentBytes shouldBe 900
  }

  it should "never evict current files, and report when they alone exceed the budget" in new Setup {
    withCard("flive")
    putFilm("flive", old, size = 400)
    val report = janitor(budget = 500).enforceBudget()
    report.deleted shouldBe empty
    report.currentBytes shouldBe 1200
    filmFiles("flive") shouldBe 3
  }
}

object ShareCardJanitorSpec {
  class Setup {
    val store     = tempStore()
    val readModel = new InMemoryReadModelRepository
    val metrics   = ShareCardMetrics.noop
    val clock     = clockAt(T0)
    val old: Instant   = T0.minusSeconds(2 * 3600)
    val young: Instant = T0.minusSeconds(600)

    def janitor(budget: Long = 1L << 30, refresh: String => Unit = _ => ()) =
      new ShareCardJanitor(store, readModel, budget, metrics, clock, refresh)

    def path(name: String): Path = store.root.resolve(name)
    def put(name: String, modified: Instant, size: Int = 10): Unit = {
      Files.write(path(name), new Array[Byte](size))
      Files.setLastModifiedTime(path(name), FileTime.from(modified))
    }
    /** A film's card, base and poster. */
    def putFilm(filmId: String, modified: Instant, size: Int = 10): Unit =
      Seq(store.cardPath(filmId), store.basePath(filmId), store.posterPath(filmId)).foreach { p =>
        Files.write(p, new Array[Byte](size)); Files.setLastModifiedTime(p, FileTime.from(modified))
      }
    def filmFiles(filmId: String): Int =
      Seq(store.cardPath(filmId), store.basePath(filmId), store.posterPath(filmId)).count(Files.exists(_))
    def withCard(filmId: String): Unit =
      readModel.upsertMovie(film(id = filmId).copy(shareCard = Some(ShareCardFile.url(filmId, "0" * 16))))
    def onScreen(filmId: String): Unit = { withCard(filmId); readModel.upsertScreening(screening(filmId)) }
  }
}
