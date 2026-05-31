package services.auth

import play.api.libs.json.{JsValue, Json}

import java.nio.charset.StandardCharsets
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/**
 * Builds a valid Facebook `signed_request` blob the way Facebook does —
 * `base64url(HMAC-SHA256(payload, secret)) + "." + base64url(payload)`,
 * both segments unpadded. Shared by `FacebookSignedRequestSpec` (the
 * parser) and `FacebookDataDeletionControllerSpec` (the callback) so the
 * sign/verify round-trip is exercised end to end from one source.
 */
object FacebookSignedRequestFixture {

  private val encoder = Base64.getUrlEncoder.withoutPadding()

  /** Sign an arbitrary payload — lets specs craft odd shapes (wrong
   *  algorithm, missing user_id) to drive the parser's failure paths. */
  def make(secret: String, payload: JsValue): String = {
    val encodedPayload = encoder.encodeToString(Json.toBytes(payload))
    s"${encoder.encodeToString(hmac(secret, encodedPayload))}.$encodedPayload"
  }

  /** The happy-path data-deletion payload for a given Facebook user id. */
  def forUser(secret: String, userId: String): String =
    make(secret, Json.obj(
      "algorithm" -> "HMAC-SHA256",
      "issued_at" -> 1700000000L,
      "user_id"   -> userId
    ))

  private def hmac(secret: String, payloadSegment: String): Array[Byte] = {
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(secret.getBytes(StandardCharsets.UTF_8), "HmacSHA256"))
    mac.doFinal(payloadSegment.getBytes(StandardCharsets.US_ASCII))
  }
}
