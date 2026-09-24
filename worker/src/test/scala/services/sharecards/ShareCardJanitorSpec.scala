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

  "The daily prune" should "never delete a card web_movies points at, however old" in new Setup {
    val live = movie("flive", card("flive", "a"))
    readModel.upsertMovie(live); readModel.upsertScreening(screening("flive"))
    put(card("flive", "a"), old)
    janitor().prune()
    exists(card("flive", "a")) shouldBe true
  }

  it should "delete a superseded card once its film points at the new one" in new Setup {
    readModel.upsertMovie(movie("flive", card("flive", "b"))); readModel.upsertScreening(screening("flive"))
    put(card("flive", "a"), old); put(card("flive", "b"), old)
    janitor().prune().deleted shouldBe Map("superseded" -> 1)
    exists(card("flive", "a")) shouldBe false
    exists(card("flive", "b")) shouldBe true
  }

  it should "retire the card of a film gone from the corpus or off the screens, and re-project a film still pointing at it" in new Setup {
    readModel.upsertMovie(movie("fgone2", card("fgone2", "a")))     // in web_movies, no screenings
    put(card("fgone1", "a"), old); put(card("fgone2", "a"), old)
    val refreshed = collection.mutable.Buffer.empty[String]
    janitor(refresh = refreshed += _).prune().deleted shouldBe Map("retired" -> 2)
    exists(card("fgone1", "a")) shouldBe false
    exists(card("fgone2", "a")) shouldBe false
    refreshed shouldBe Seq("fgone2")
  }

  it should "leave any file younger than the grace period alone, referenced or not" in new Setup {
    put(card("fgone", "a"), young)
    put(poster("https://gone.example/p.jpg"), young)
    put(card("fgone", "b") + s".${ShareCardStore.writerId}-deadbeef.tmp", young)
    janitor().prune().deleted shouldBe empty
    storeFiles should have size 3
  }

  it should "delete temp files older than the grace period — abandoned writes" in new Setup {
    put(card("fx", "a") + ".otherhost-7-cafebabe.tmp", old)
    janitor().enforceBudget().deleted shouldBe Map("temp" -> 1)
    storeFiles shouldBe empty
  }

  it should "delete cached posters no film on screen uses, and keep the ones it does" in new Setup {
    readModel.upsertMovie(movie("flive", card("flive", "a"), posterUrl = "https://cdn.example/live.jpg"))
    readModel.upsertScreening(screening("flive"))
    readModel.upsertMovie(movie("foff", card("foff", "a"), posterUrl = "https://cdn.example/off.jpg"))
    put(card("flive", "a"), old)
    put(poster("https://cdn.example/live.jpg"), old)
    put(poster("https://cdn.example/off.jpg"), old)
    put(poster("https://cdn.example/superseded.jpg"), old)
    janitor().prune().deleted shouldBe Map("unreferenced" -> 2)
    exists(poster("https://cdn.example/live.jpg")) shouldBe true
  }

  it should "delete nothing but abandoned temps when web_movies could not be read whole" in new Setup {
    put(card("fgone", "a"), old)
    val blind = new InMemoryReadModelRepository {
      override def findAllShareCardRefsChecked() = (Seq.empty, false)
    }
    new ShareCardJanitor(store, blind, budgetBytes = 1, metrics, clock, _ => ()).prune().deleted shouldBe empty
    exists(card("fgone", "a")) shouldBe true
  }

  it should "drop the first-publish marker of a film gone from the screens, and keep a live film's" in new Setup {
    readModel.upsertMovie(movie("flive", card("flive", "a"))); readModel.upsertScreening(screening("flive"))
    Seq("flive", "fgone").foreach { token =>
      store.markPublished(token)
      Files.setLastModifiedTime(store.publishedMarker(token), FileTime.from(old))
    }
    janitor().prune()
    store.publishedMarkers().map(_._1) shouldBe Seq("flive")
  }

  "The budget" should "count cards and posters together and evict the oldest unreferenced files first" in new Setup {
    readModel.upsertMovie(movie("flive", card("flive", "a"), posterUrl = "https://cdn.example/live.jpg"))
    readModel.upsertScreening(screening("flive"))
    put(card("flive", "a"), old, size = 400)                              // current
    put(poster("https://cdn.example/live.jpg"), old, size = 400)          // current
    put(poster("https://cdn.example/oldest.jpg"), old.minusSeconds(600), size = 300)
    put(card("fstale", "x"), old.minusSeconds(300), size = 300)
    put(card("fstale", "y"), old, size = 300)
    val report = janitor(budget = 1500).enforceBudget()
    report.deleted shouldBe Map("budget" -> 1)
    exists(poster("https://cdn.example/oldest.jpg")) shouldBe false      // the oldest went first
    report.bytes shouldBe 1400
    report.currentBytes shouldBe 800
  }

  it should "never evict current files, and report when they alone exceed the budget" in new Setup {
    readModel.upsertMovie(movie("flive", card("flive", "a"), posterUrl = "https://cdn.example/live.jpg"))
    readModel.upsertScreening(screening("flive"))
    put(card("flive", "a"), old, size = 400)
    put(poster("https://cdn.example/live.jpg"), old, size = 400)
    val report = janitor(budget = 500).enforceBudget()
    report.deleted shouldBe empty
    report.currentBytes shouldBe 800
    storeFiles should have size 2
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

    def card(filmId: String, variant: String): String = ShareCardFile(filmId, variant * 16).name
    def poster(url: String): String = s"${ShareCardStore.PosterDir}/${ShareCardPosters.key(url)}.${ShareCardStore.PosterExtension}"
    def path(name: String): Path = store.root.resolve(name)
    def put(name: String, modified: Instant, size: Int = 10): Unit = {
      Files.write(path(name), new Array[Byte](size))
      Files.setLastModifiedTime(path(name), FileTime.from(modified))
    }
    def exists(name: String): Boolean = Files.exists(path(name))
    def storeFiles: Seq[StoredFile] = store.list()

    def movie(id: String, card: String): models.ResolvedMovie = film(id = id).copy(shareCard = Some(card))
    def movie(id: String, card: String, posterUrl: String): models.ResolvedMovie =
      film(id = id, poster = posterUrl).copy(shareCard = Some(card))
  }
}
