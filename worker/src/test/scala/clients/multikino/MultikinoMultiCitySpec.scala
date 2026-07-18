package clients.multikino

import clients.tools.FakeHttpFetch
import models.{Multikino, MultikinoZloteTarasy}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.MultikinoClient

/** The client used to be hard-wired to Poznań's Stary Browar (`0011` →
 *  `Multikino`). These pin the parameterisation that lets one client serve
 *  every Multikino venue: the api URL is keyed by the cinema id, and the
 *  parsed rows carry the cinema passed in. */
class MultikinoMultiCitySpec extends AnyFlatSpec with Matchers {

  "MultikinoClient.apiUrl" should "key the showings endpoint by the cinema id" in {
    MultikinoClient.apiUrl("0013") should include ("/cinemas/0013/films")
    MultikinoClient.apiUrl("0040") should include ("/cinemas/0040/films")
    // The Poznań constant stays a stable alias for id 0011.
    MultikinoClient.ApiUrl shouldBe MultikinoClient.apiUrl("0011")
  }

  "A client constructed for a non-Poznań venue" should "tag every row with that cinema" in {
    // Reuse the Poznań fixture (id 0011) but assign a Warszawa venue — proves
    // the cinema flows from the constructor through the parser to the rows.
    val client  = new MultikinoClient(new FakeHttpFetch("multikino"), "0011", MultikinoZloteTarasy)
    val results = client.fetch()
    results should not be empty
    results.map(_.cinema).toSet shouldBe Set(MultikinoZloteTarasy)
  }

  "The default constructor" should "still be Poznań's Stary Browar" in {
    new MultikinoClient(new FakeHttpFetch("multikino")).cinema shouldBe Multikino
  }
}
