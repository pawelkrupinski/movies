package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.InMemoryReadModelRepository
import ShareCardTestKit.*

import java.nio.file.Files
import java.nio.file.attribute.FileTime

/**
 * `web_movies.shareCard` is the `og:image` URL verbatim, so the invariant is concrete: the file
 * it names is on disk, is that film's, and carries the version the URL names.
 */
class ShareCardAuditSpec extends AnyFlatSpec with Matchers with org.scalatest.LoneElement {

  private val v1 = "1" * 16
  private val v2 = "2" * 16

  private class Setup {
    val store     = tempStore()
    val readModel = new InMemoryReadModelRepository
    def card(filmId: String, version: String): Unit =
      store.writeAtomically(store.cardPath(filmId), posterJpeg, version)
    def pointAt(filmId: String, version: String): Unit =
      readModel.upsertMovie(film(id = filmId).copy(shareCard = Some(ShareCardFile.url(filmId, version))))
    def check(filmId: String): Option[Seq[String]] = ShareCardAudit.check(filmId, readModel, store)
  }

  "a pointer at the film's own card, at the version on disk" should "hold" in new Setup {
    card("fa", v1); pointAt("fa", v1)
    check("fa") shouldBe Some(Nil)
  }

  "a pointer at a file that is not there" should "be a violation" in new Setup {
    pointAt("fa", v1)
    check("fa").get.loneElement should include ("no such file")
  }

  "a pointer whose version the file on disk has moved on from" should "be a violation" in new Setup {
    card("fa", v2); pointAt("fa", v1)
    check("fa").get.loneElement should include (s"version $v2")
  }

  "a pointer at another film's file" should "be a violation even when that file exists" in new Setup {
    card("fb", v1)
    readModel.upsertMovie(film(id = "fa").copy(shareCard = Some(ShareCardFile.url("fb", v1))))
    check("fa").get.loneElement should include ("another film's file")
  }

  "a film with no card, or no document" should "not be judged" in new Setup {
    readModel.upsertMovie(film(id = "fa"))
    check("fa") shouldBe None
    check("fnone") shouldBe None
  }

  "the ids sampled" should "be the films whose document names a card, and none when web_movies could not be read whole" in new Setup {
    pointAt("fa", v1); readModel.upsertMovie(film(id = "fb"))
    ShareCardAudit.ids(readModel) shouldBe Some(Seq("fa"))
    val blind = new InMemoryReadModelRepository { override def findAllShareCardRefsChecked() = (Seq.empty, false) }
    ShareCardAudit.ids(blind) shouldBe None
  }

  // THE JANITOR'S HALF OF THE INVARIANT. The daily prune retires the files of a film off the
  // screens; one whose document still points at its card is re-projected at once so it points at
  // nothing rather than at a missing file (`refresh`, which in production is the projector's
  // `refreshShareCard`: the document's `shareCard` becomes what is on disk — here, nothing).
  // Without that call the document is left naming a deleted file, and this audit is what says so.
  private class Retired extends Setup {
    card("fgone", v1); pointAt("fgone", v1)                                  // a film off the screens
    Files.setLastModifiedTime(store.cardPath("fgone"), FileTime.from(T0.minusSeconds(2 * 3600)))
    def prune(refresh: String => Unit): Unit =
      new ShareCardJanitor(store, readModel, 1L << 30, ShareCardMetrics.noop, clockAt(T0), refresh).prune()
  }

  "a card the daily prune retired" should "leave no document pointing at it" in new Retired {
    val service = new Rig(store).service
    prune(filmId => readModel.findAllMovies().find(_._id == filmId).foreach(doc => readModel.upsertMovie(doc.copy(shareCard = service.current(doc)))))
    Files.exists(store.cardPath("fgone")) shouldBe false
    check("fgone") shouldBe None                                             // it names no card any more
  }

  it should "be caught by the audit when its film was never re-projected" in new Retired {
    prune(_ => ())                                                           // the re-projection that did not happen
    check("fgone").get.loneElement should include ("no such file")
  }
}
