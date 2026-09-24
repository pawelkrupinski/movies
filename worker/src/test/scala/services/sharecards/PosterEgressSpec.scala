package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{GetOnlyHttpFetch, HttpStatusException}

import java.nio.file.{Files, Path}

/** Posters on a host that refuses our datacenter IP go through the egress its scrapes already use.
 *  Multikino's Cloudflare 403s the k3s worker on every poster (checked 2026-09-24) while its
 *  scrapes read the same site through the residential proxy. */
class PosterEgressSpec extends AnyFlatSpec with Matchers {

  private val refused = new PosterDownload { def fetch(url: String): Either[String, Path] = Left(PosterFailure.Http4xx) }

  private final class Egress(answer: String => Array[Byte]) extends GetOnlyHttpFetch {
    val asked = scala.collection.mutable.ListBuffer.empty[String]
    def get(url: String): String = new String(getBytes(url), "ISO-8859-1")
    override def getBytes(url: String): Array[Byte] = { asked += url; answer(url) }
  }

  private val poster = Array[Byte](-1, -40, -1, 0, 1, 2, 3)

  "A poster on a host with its own egress" should "be fetched through that egress, and any other directly" in {
    val egress   = new Egress(_ => poster)
    val download = PosterDownload.routed(refused, Map("www.multikino.pl" -> new EgressPosterDownload(egress)))
    val file     = download.fetch("https://www.multikino.pl/-/media/multikino/images/x.jpg")
    file.map(Files.readAllBytes(_).toSeq) shouldBe Right(poster.toSeq)
    file.foreach(Files.deleteIfExists)
    egress.asked.toSeq shouldBe Seq("https://www.multikino.pl/-/media/multikino/images/x.jpg")
    download.fetch("https://cdn.example/poster.jpg") shouldBe Left(PosterFailure.Http4xx)
    egress.asked should have size 1
  }

  it should "report the egress's failures with the reasons a direct download gives" in {
    def via(answer: String => Array[Byte]) = new EgressPosterDownload(new Egress(answer), maxBytes = 4).fetch("https://www.multikino.pl/x.jpg")
    via(_ => throw new HttpStatusException(403, "GET", "u", None)) shouldBe Left(PosterFailure.Http4xx)
    via(_ => throw new HttpStatusException(503, "GET", "u", None)) shouldBe Left(PosterFailure.Http5xx)
    via(_ => throw new java.net.http.HttpTimeoutException("slow")) shouldBe Left(PosterFailure.Timeout)
    via(_ => throw new java.io.IOException("reset")) shouldBe Left(PosterFailure.Network)
    via(_ => Array.emptyByteArray) shouldBe Left(PosterFailure.EmptyBody)
    via(_ => poster) shouldBe Left(PosterFailure.TooLarge)
  }
}
