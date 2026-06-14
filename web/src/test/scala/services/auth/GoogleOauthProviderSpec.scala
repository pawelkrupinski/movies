package services.auth

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.RoutingHttpFetch

class GoogleOauthProviderSpec extends AnyFlatSpec with Matchers {

  private val Client = "TEST_CLIENT_ID.apps.googleusercontent.com"
  private val Secret = "TEST_SECRET"

  private def scripted(replies: Map[String, String]) = new RoutingHttpFetch(replies)

  "Google.authUrl" should "hit Google's authorization endpoint with the OIDC scope" in {
    val p   = new GoogleOauthProvider(scripted(Map.empty), Client, Secret)
    val url = p.authUrl(state = "abc123", redirectUri = "https://k/auth/google/callback")
    url should startWith ("https://accounts.google.com/o/oauth2/v2/auth?")
    url should include ("response_type=code")
    url should include ("scope=openid+email+profile")
    url should include ("state=abc123")
    url should include ("client_id=TEST_CLIENT_ID.apps.googleusercontent.com")
  }

  it should "URL-encode the redirect_uri (colons and slashes)" in {
    val p   = new GoogleOauthProvider(scripted(Map.empty), Client, Secret)
    val url = p.authUrl(state = "s", redirectUri = "https://kinowo.fly.dev/auth/google/callback")
    url should include ("redirect_uri=https%3A%2F%2Fkinowo.fly.dev%2Fauth%2Fgoogle%2Fcallback")
  }

  it should "ask for prompt=select_account so multi-account browsers re-prompt" in {
    val p   = new GoogleOauthProvider(scripted(Map.empty), Client, Secret)
    p.authUrl("s", "https://x") should include ("prompt=select_account")
  }

  it should "include device_id and device_name when the redirect URI uses a private IP" in {
    val p   = new GoogleOauthProvider(scripted(Map.empty), Client, Secret)
    val url = p.authUrl("s", "http://172.20.10.3:9000/auth/google/callback")
    url should include ("device_id=")
    url should include ("device_name=")
  }

  it should "omit device_id and device_name for public hosts" in {
    val p   = new GoogleOauthProvider(scripted(Map.empty), Client, Secret)
    val url = p.authUrl("s", "https://kinowo.fly.dev/auth/google/callback")
    url should not include "device_id"
    url should not include "device_name"
  }

  "isPrivateIp" should "match all RFC 1918 ranges and localhost" in {
    import GoogleOauthProvider.isPrivateIp
    isPrivateIp("http://10.0.0.1:9000/cb")      shouldBe true
    isPrivateIp("http://172.16.0.1:9000/cb")     shouldBe true
    isPrivateIp("http://172.31.255.1:9000/cb")   shouldBe true
    isPrivateIp("http://192.168.1.100:9000/cb")  shouldBe true
    isPrivateIp("http://127.0.0.1:9000/cb")      shouldBe true
    isPrivateIp("https://kinowo.fly.dev/cb")     shouldBe false
    isPrivateIp("http://172.32.0.1:9000/cb")     shouldBe false
  }

  "Google.exchangeCode" should "POST form-encoded credentials to /token then GET /userinfo with the token" in {
    val fake = scripted(Map(
      "oauth2.googleapis.com/token"           -> """{"access_token":"ya29.tok","scope":"openid email profile","token_type":"Bearer"}""",
      "googleapis.com/oauth2/v3/userinfo"     -> """{"sub":"118811881188","email":"u@example.com","name":"Test User","picture":"https://lh3/avatar"}"""
    ))
    val p = new GoogleOauthProvider(fake, Client, Secret)
    val profile = p.exchangeCode("AUTH_CODE_xyz", "https://k/auth/google/callback")

    profile shouldBe OauthProfile(
      sub         = "118811881188",
      email       = Some("u@example.com"),
      displayName = Some("Test User"),
      avatarUrl   = Some("https://lh3/avatar")
    )

    val Seq(post, get) = fake.calls.toSeq
    post._1 shouldBe "POST"
    post._2 should include ("oauth2.googleapis.com/token")
    get._1  shouldBe "GET"
    get._2  should include ("/userinfo?access_token=ya29.tok")

    val Seq((postUrl, postBody, postCt)) = fake.postBodies.toSeq
    postUrl should include ("oauth2.googleapis.com/token")
    postCt  shouldBe "application/x-www-form-urlencoded"
    postBody should include ("grant_type=authorization_code")
    postBody should include ("code=AUTH_CODE_xyz")
    postBody should include ("client_secret=TEST_SECRET")
  }

  it should "throw with diagnostic context when /token doesn't return access_token" in {
    val fake = scripted(Map(
      "oauth2.googleapis.com/token" -> """{"error":"invalid_grant","error_description":"code expired"}"""
    ))
    val p  = new GoogleOauthProvider(fake, Client, Secret)
    val exception = intercept[RuntimeException](p.exchangeCode("X", "https://k/cb"))
    exception.getMessage should include ("missing access_token")
    exception.getMessage should include ("invalid_grant")
  }

  it should "throw when /userinfo omits sub — the User upsert key would be empty" in {
    val fake = scripted(Map(
      "oauth2.googleapis.com/token"       -> """{"access_token":"t"}""",
      "googleapis.com/oauth2/v3/userinfo" -> """{"email":"u@x.com"}"""
    ))
    val p  = new GoogleOauthProvider(fake, Client, Secret)
    val exception = intercept[RuntimeException](p.exchangeCode("X", "https://k/cb"))
    exception.getMessage should include ("missing sub")
  }

  it should "treat email / name / picture as optional — a sub-only response yields a valid profile" in {
    val fake = scripted(Map(
      "oauth2.googleapis.com/token"       -> """{"access_token":"t"}""",
      "googleapis.com/oauth2/v3/userinfo" -> """{"sub":"S"}"""
    ))
    new GoogleOauthProvider(fake, Client, Secret).exchangeCode("X", "https://k/cb") shouldBe
      OauthProfile(sub = "S", email = None, displayName = None, avatarUrl = None)
  }

  "Google.name" should "be 'google' (matches the route :provider segment)" in {
    new GoogleOauthProvider(scripted(Map.empty), Client, Secret).name shouldBe "google"
  }
}
