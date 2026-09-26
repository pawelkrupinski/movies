package services.sharecards

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ShareCardTestKit.*

/** The pages a re-scrape asks Facebook to fetch again: the film's page in each city it screens in. */
class FilmPageUrlsSpec extends AnyFlatSpec with Matchers {

  "A film's pages" should "be its page in each city it screens in" in {
    val rig = new Rig
    val movie = film()
    rig.readModel.upsertMovie(movie)
    rig.readModel.upsertScreening(screening(movie._id, "poznan"))
    rig.readModel.upsertScreening(screening(movie._id, "wroclaw"))
    new FilmPageUrls(rig.readModel, Country.default, rig.clock)(movie._id) shouldBe
      Seq("https://kinowo.net/poznan/movie/diuna", "https://kinowo.net/wroclaw/movie/diuna")
  }

  // The page list comes from the read model. A read that failed produced no pages, so "no
  // request failed" held and a re-scrape that never ran was reported done.
  it should "throw, not come back empty, when the read model could not be read" in {
    Seq(false, true).foreach { screeningsReadable =>
      val readModel = new services.readmodel.UnreadableReadModelRepository
      readModel.failingReads = false
      val rig   = new Rig(readModel = readModel)
      val movie = film()
      readModel.upsertMovie(movie); readModel.upsertScreening(screening(movie._id))
      val pages = new FilmPageUrls(readModel, Country.default, rig.clock)
      readModel.failingReads = true
      readModel.screeningsReadable = screeningsReadable   // true: only web_movies (the slugs) is unreadable
      withClue(s"screeningsReadable=$screeningsReadable: ") {
        an[IllegalStateException] should be thrownBy pages(movie._id)
      }
    }
  }

  // A burst of re-scrapes (a template change re-draws every recent film) expands one film after
  // another. Each read the page list off a scan of the WHOLE of web_screenings — hundreds of
  // thousands of ids in the US — to keep the few rows filed under one film.
  it should "come from the film's own screenings, never a scan of the whole of web_screenings" in {
    val readModel = new services.readmodel.InMemoryReadModelRepository {
      override def findAllScreeningRefsChecked(): (Seq[services.readmodel.ScreeningRef], Boolean) =
        fail("a re-scrape scanned every screening in the read model for one film's pages")
    }
    val rig   = new Rig(readModel = readModel)
    val movie = film()
    readModel.upsertMovie(movie)
    readModel.upsertScreening(screening(movie._id, "poznan"))
    readModel.upsertScreening(screening(movie._id, "wroclaw"))
    new FilmPageUrls(readModel, Country.default, rig.clock)(movie._id) shouldBe
      Seq("https://kinowo.net/poznan/movie/diuna", "https://kinowo.net/wroclaw/movie/diuna")
  }

  it should "share one read of the film slugs across a burst, and read again for a film they lack" in {
    val rig = new Rig
    val first  = film()
    val second = film(id = "fsecond", title = "Oppenheimer")
    rig.readModel.upsertMovie(first); rig.readModel.upsertScreening(screening(first._id))
    val pages  = new FilmPageUrls(rig.readModel, Country.default, rig.clock)
    val before = rig.readModel.findAllMoviesCalls.get
    pages(first._id); pages(first._id)
    rig.readModel.findAllMoviesCalls.get - before shouldBe 1

    rig.readModel.upsertMovie(second); rig.readModel.upsertScreening(screening(second._id))
    pages(second._id) shouldBe Seq("https://kinowo.net/poznan/movie/oppenheimer")   // new since: read again
    rig.readModel.findAllMoviesCalls.get - before shouldBe 2
  }
}
