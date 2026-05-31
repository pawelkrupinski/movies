package services.auth

import org.scalatest.EitherValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class FacebookSignedRequestSpec extends AnyFlatSpec with Matchers {

  private val Secret = "s3cr3t-app-secret"

  "FacebookSignedRequest.parse" should "decode a correctly signed payload" in {
    val signed = FacebookSignedRequestFixture.forUser(Secret, "fb-12345")
    val parsed = FacebookSignedRequest.parse(signed, Secret).value
    parsed.userId   shouldBe "fb-12345"
    parsed.issuedAt shouldBe Some(1700000000L)
  }

  it should "reject a signature made with a different secret" in {
    val signed = FacebookSignedRequestFixture.forUser("attacker-secret", "fb-12345")
    FacebookSignedRequest.parse(signed, Secret).left.value should include ("signature mismatch")
  }

  it should "reject a tampered payload (signature no longer matches)" in {
    val signed         = FacebookSignedRequestFixture.forUser(Secret, "fb-12345")
    val Array(sig, _)  = signed.split("\\.", 2)
    val forgedPayload  = java.util.Base64.getUrlEncoder.withoutPadding()
      .encodeToString(Json.obj("user_id" -> "fb-99999", "algorithm" -> "HMAC-SHA256").toString.getBytes("UTF-8"))
    FacebookSignedRequest.parse(s"$sig.$forgedPayload", Secret).left.value should include ("signature mismatch")
  }

  it should "reject a payload declaring an unexpected algorithm" in {
    val signed = FacebookSignedRequestFixture.make(Secret, Json.obj("algorithm" -> "PLAINTEXT", "user_id" -> "fb-1"))
    FacebookSignedRequest.parse(signed, Secret).left.value should include ("unexpected algorithm")
  }

  it should "reject a payload with no user_id" in {
    val signed = FacebookSignedRequestFixture.make(Secret, Json.obj("algorithm" -> "HMAC-SHA256"))
    FacebookSignedRequest.parse(signed, Secret).left.value should include ("missing user_id")
  }

  it should "reject a blob that isn't '<signature>.<payload>'" in {
    FacebookSignedRequest.parse("not-a-signed-request", Secret).left.value should include ("'<signature>.<payload>'")
  }
}
