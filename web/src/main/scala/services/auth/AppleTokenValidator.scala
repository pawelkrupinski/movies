package services.auth

import play.api.libs.json.{JsArray, Json}
import tools.HttpFetch

import java.math.BigInteger
import java.security.KeyFactory
import java.security.spec.RSAPublicKeySpec
import java.util.Base64

class AppleTokenValidator(http: HttpFetch, bundleId: String) {

  @volatile private var cachedKeys: Map[String, java.security.PublicKey] = Map.empty
  @volatile private var keysLoadedAt: Long = 0
  private val keysCacheTtl = 3600_000L // 1 hour

  def validate(identityToken: String, fullName: Option[String]): OauthProfile = {
    val parts = identityToken.split('.')
    if (parts.length < 3) throw new RuntimeException("Apple identity token is not a valid JWT")

    val headerJson = Json.parse(Base64.getUrlDecoder.decode(parts(0)))
    val kid = (headerJson \ "kid").asOpt[String]
      .getOrElse(throw new RuntimeException("Apple JWT missing kid"))
    val alg = (headerJson \ "alg").asOpt[String].getOrElse("RS256")
    if (alg != "RS256") throw new RuntimeException(s"Unsupported Apple JWT algorithm: $alg")

    val publicKey = getKey(kid)
    val signedContent = s"${parts(0)}.${parts(1)}".getBytes("UTF-8")
    val signature = Base64.getUrlDecoder.decode(parts(2))
    val sig = java.security.Signature.getInstance("SHA256withRSA")
    sig.initVerify(publicKey)
    sig.update(signedContent)
    if (!sig.verify(signature))
      throw new RuntimeException("Apple JWT signature verification failed")

    val claimsJson = Json.parse(Base64.getUrlDecoder.decode(parts(1)))
    val iss = (claimsJson \ "iss").asOpt[String].getOrElse("")
    if (iss != "https://appleid.apple.com")
      throw new RuntimeException(s"Apple JWT iss mismatch: $iss")
    val aud = (claimsJson \ "aud").asOpt[String].getOrElse("")
    if (aud != bundleId)
      throw new RuntimeException(s"Apple JWT aud mismatch: expected $bundleId, got $aud")
    val exp = (claimsJson \ "exp").asOpt[Long].getOrElse(0L)
    if (exp * 1000 < System.currentTimeMillis())
      throw new RuntimeException("Apple JWT expired")

    OauthProfile(
      sub         = (claimsJson \ "sub").asOpt[String]
                      .getOrElse(throw new RuntimeException("Apple JWT missing sub")),
      email       = (claimsJson \ "email").asOpt[String],
      displayName = fullName,
      avatarUrl   = None
    )
  }

  private def getKey(kid: String): java.security.PublicKey = {
    if (System.currentTimeMillis() - keysLoadedAt > keysCacheTtl || !cachedKeys.contains(kid)) {
      refreshKeys()
    }
    cachedKeys.getOrElse(kid, throw new RuntimeException(s"Apple JWKS key not found for kid=$kid"))
  }

  private def refreshKeys(): Unit = {
    val body = http.get("https://appleid.apple.com/auth/keys")
    val js = Json.parse(body)
    val keys = (js \ "keys").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
    cachedKeys = keys.flatMap { k =>
      for {
        kid <- (k \ "kid").asOpt[String]
        n   <- (k \ "n").asOpt[String]
        e   <- (k \ "e").asOpt[String]
      } yield {
        val modulus  = new BigInteger(1, Base64.getUrlDecoder.decode(n))
        val exponent = new BigInteger(1, Base64.getUrlDecoder.decode(e))
        val spec = new RSAPublicKeySpec(modulus, exponent)
        kid -> KeyFactory.getInstance("RSA").generatePublic(spec)
      }
    }.toMap
    keysLoadedAt = System.currentTimeMillis()
  }
}
