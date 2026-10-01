package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The normalizing fetch parses a TMDB body to file it and the client then parses the very same
 *  `String`: shared, the second parse is the first one's tree. */
class JsonBodiesSpec extends AnyFlatSpec with Matchers {

  "JsonBodies" should "hand the body this thread parsed last back to the next parse of that same body, once" in {
    val bodies = new JsonBodies
    val body   = """{"id": 1018, "credits": {"crew": [{"job": "Director", "name": "Wojciech Has"}]}}"""
    val first  = bodies.parse(body)
    bodies.parse(body) should be theSameInstanceAs first
    val third = bodies.parse(body)
    third should not be theSameInstanceAs(first)
    third shouldBe first
  }

  it should "parse an equal body that is not the same string anew, and share nothing across threads" in {
    val bodies = new JsonBodies
    val body   = """{"id": 1}"""
    val first  = bodies.parse(body)
    bodies.parse(new String(body.toCharArray)) should not be theSameInstanceAs(first)
    val again  = bodies.parse(body)
    var other: Option[play.api.libs.json.JsValue] = None
    val thread = new Thread(() => other = Some(bodies.parse(body)))
    thread.start(); thread.join()
    other.get should not be theSameInstanceAs(again)
    other.get shouldBe again
  }
}
