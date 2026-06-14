package services.auth

import play.api.libs.json.Json

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.util.Try

/**
 * A verified Facebook `signed_request` payload. Today we only read
 * `userId` (the subject of a data-deletion callback); `issuedAt` is
 * carried for logging / future freshness checks.
 */
case class FacebookSignedRequest(userId: String, issuedAt: Option[Long])

object FacebookSignedRequest {

  private val Algorithm = "HMAC-SHA256"
  private val HmacName  = "HmacSHA256"

  /**
   * Parse and cryptographically verify a Facebook `signed_request` —
   * the `<base64url-signature>.<base64url-payload>` blob Facebook POSTimestamp
   * to the data-deletion and deauthorize callbacks. The payload is
   * returned only when an HMAC-SHA256 over the raw payload segment,
   * keyed by `appSecret`, matches the provided signature; otherwise
   * `Left(reason)`.
   *
   * Never throws — malformed input maps to a `Left`, so the caller can
   * turn any failure straight into a 400. The signature is checked
   * *before* the payload is parsed as JSON, so we never act on
   * untrusted bytes.
   */
  def parse(signedRequest: String, appSecret: String): Either[String, FacebookSignedRequest] =
    signedRequest.split("\\.", 2) match {
      case Array(encodedSig, encodedPayload) =>
        for {
          expectedSig  <- decode(encodedSig).toRight("signed_request signature is not base64url")
          actualSig     = hmac(encodedPayload, appSecret)
          _            <- Either.cond(MessageDigest.isEqual(expectedSig, actualSig), (),
                            "signed_request signature mismatch")
          payloadBytes <- decode(encodedPayload).toRight("signed_request payload is not base64url")
          json         <- Try(Json.parse(payloadBytes)).toOption.toRight("signed_request payload is not JSON")
          algo          = (json \ "algorithm").asOpt[String].getOrElse("")
          _            <- Either.cond(algo.equalsIgnoreCase(Algorithm), (),
                            s"signed_request has unexpected algorithm: $algo")
          userId       <- (json \ "user_id").asOpt[String].toRight("signed_request missing user_id")
        } yield FacebookSignedRequest(userId, (json \ "issued_at").asOpt[Long])
      case _ =>
        Left("signed_request is not in '<signature>.<payload>' form")
    }

  // Facebook base64url-encodes both segments without padding; Java's
  // URL decoder treats trailing padding as optional, so this handles
  // either form.
  private def decode(segment: String): Option[Array[Byte]] =
    Try(Base64.getUrlDecoder.decode(segment)).toOption

  private def hmac(payloadSegment: String, appSecret: String): Array[Byte] = {
    val mac = Mac.getInstance(HmacName)
    mac.init(new SecretKeySpec(appSecret.getBytes(StandardCharsets.UTF_8), HmacName))
    mac.doFinal(payloadSegment.getBytes(StandardCharsets.US_ASCII))
  }
}
