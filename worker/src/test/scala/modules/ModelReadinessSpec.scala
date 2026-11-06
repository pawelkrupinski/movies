package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Listing, ListingReadiness, Question}

/** The model's input gate is on (2026-11-06, with the gate's asks of what answers led to): a listing waits on its venue
 *  page and its families' questions alike -- the composition the worker and the drift replay both read. */
class ModelReadinessSpec extends AnyFlatSpec with Matchers {
  private final class Waits(question: Question) extends ListingReadiness {
    def awaiting(listing: Listing): Set[Question]               = Set(question)
    def ask(listing: Listing, questions: Set[Question]): Unit = ()
  }

  "the model's input gate" should "be on: a listing waits on its page and its families' questions" in {
    val listing = services.identity.ModelRead.corpus(Seq(models.KinoMuza -> Seq(models.CinemaMovie(models.Movie("Geister"), models.KinoMuza,
      None, None, None, Nil, Nil, Nil))), services.movies.SingleCountryNormalizer.titleNormalizer).head
    val title = Question.Family(services.identity.agreement.VoterFamily.Imdb, "title|Geister")
    WorkerWiring.modelReadiness(new Waits(Question.VenuePage(listing.key)), new Waits(title)).awaiting(listing) shouldBe
      Set(Question.VenuePage(listing.key), title)
  }
}
