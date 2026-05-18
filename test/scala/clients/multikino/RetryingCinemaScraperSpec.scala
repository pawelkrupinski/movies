package clients.multikino

import models.{Cinema, CinemaMovie, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{CinemaScraper, RetryingCinemaScraper}

import java.time.LocalDateTime
import scala.concurrent.duration._

class RetryingCinemaScraperSpec extends AnyFlatSpec with Matchers {

  private val OneMovie: Seq[CinemaMovie] = Seq(
    CinemaMovie(
      movie     = Movie("X"),
      cinema    = Multikino,
      posterUrl = None,
      filmUrl   = None,
      synopsis  = None,
      cast      = None,
      director  = None,
      showtimes = Seq(Showtime(LocalDateTime.now(), Some("https://book")))
    )
  )

  private def scriptedScraper(plan: List[Either[Throwable, Seq[CinemaMovie]]]): CinemaScraper = new CinemaScraper {
    private var remaining = plan
    val cinema: Cinema    = Multikino
    def fetch(): Seq[CinemaMovie] = remaining match {
      case Right(v) :: rest => remaining = rest; v
      case Left(t)  :: rest => remaining = rest; throw t
      case Nil              => throw new IllegalStateException("scripted scraper exhausted")
    }
  }

  "RetryingCinemaScraper" should "delegate to the underlying scraper on the happy path" in {
    val s = new RetryingCinemaScraper(scriptedScraper(List(Right(OneMovie))), initialBackoff = 1.millis)
    s.fetch() shouldBe OneMovie
  }

  it should "preserve the wrapped scraper's cinema identity (so list lookups still work)" in {
    val s = new RetryingCinemaScraper(scriptedScraper(List(Right(OneMovie))), initialBackoff = 1.millis)
    s.cinema shouldBe Multikino
  }

  it should "retry on a transient failure and return the next success" in {
    val s = new RetryingCinemaScraper(
      scriptedScraper(List(Left(new RuntimeException("blip")), Right(OneMovie))),
      initialBackoff = 1.millis
    )
    s.fetch() shouldBe OneMovie
  }

  it should "rethrow when every retry fails" in {
    val s = new RetryingCinemaScraper(
      scriptedScraper(List.fill(3)(Left(new RuntimeException("down")))),
      maxAttempts    = 3,
      initialBackoff = 1.millis
    )
    intercept[RuntimeException] { s.fetch() }.getMessage shouldBe "down"
  }
}
