package services.auth

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.HttpFetch

import scala.collection.mutable

class GoogleOauthProviderSpec extends AnyFlatSpec with Matchers {

  // Minimal scripted fake — records every call and returns canned
  // responses keyed by URL fragment. The two providers each make at
  // most one GET and one POST per `exchangeCode`, so a dictionary
  // keyed by "match-any-substring" is plenty without invoking the
  // fixture-replay machinery in `clients.tools.FakeHttpFetch`.
  private class ScriptedFetch(replies: Map[String, String]) extends HttpFetch {
    val calls: mutable.ListBuffer[(String, String)] = mutable.ListBuffer.empty
    override def get(url: String): String = {
      calls += (("GET", url))
      replies.collectFirst { case (k, v) if url.contains(k) => v }
        .getOrElse(throw new RuntimeException(s"No scripted reply for GET $url"))
    }
    override def post(url: String, body: String, contentType: String): String = {
      calls += (("POST", url + "|body=" + body + "|ct=" + contentType))
      replies.collectFirst { case (k, v) if url.contains(k) => v }
        .getOrElse(throw new RuntimeException(s"No scripted reply for POST $url"))
    }
  }

  private val Client = "TEST_CLIENT_ID.apps.googleusercontent.com"
  private val Secret = "TEST_SECRET"

  "Google.authUrl" should "hit Google's authorization endpoint with the OIDC scope" in {
    val p   = new GoogleOauthProvider(new ScriptedFetch(Map.empty), Client, Secret)
    val url = p.authUrl(state = "abc123", redirectUri = "https://k/auth/google/callback")
    url should startWith ("https://accounts.google.com/o/oauth2/v2/auth?")
    url should include ("response_type=code")
    url should include ("scope=openid+email+profile")
    url should include ("state=abc123")
    url should include ("client_id=TEST_CLIENT_ID.apps.googleusercontent.com")
  }

  it should "URL-encode the redirect_uri (colons and slashes)" in {
    val p   = new GoogleOauthProvider(new ScriptedFetch(Map.empty), Client, Secret)
    val url = p.authUrl(state = "s", redirectUri = "https://kinowo.fly.dev/auth/google/callback")
    url should include ("redirect_uri=https%3A%2F%2Fkinowo.fly.dev%2Fauth%2Fgoogle%2Fcallback")
  }

  it should "ask for prompt=select_account so multi-account browsers re-prompt" in {
    val p   = new GoogleOauthProvider(new ScriptedFetch(Map.empty), Client, Secret)
    p.authUrl("s", "https://x") should include ("prompt=select_account")
  }

  "Google.exchangeCode" should "POST form-encoded credentials to /token then GET /userinfo with the token" in {
    val fake = new ScriptedFetch(Map(
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
    post._2 should include ("ct=application/x-www-form-urlencoded")
    post._2 should include ("grant_type=authorization_code")
    post._2 should include ("code=AUTH_CODE_xyz")
    post._2 should include ("client_secret=TEST_SECRET")
    get._1  shouldBe "GET"
    get._2  should include ("/userinfo?access_token=ya29.tok")
  }

  it should "throw with diagnostic context when /token doesn't return access_token" in {
    val fake = new ScriptedFetch(Map(
      "oauth2.googleapis.com/token" -> """{"error":"invalid_grant","error_description":"code expired"}"""
    ))
    val p  = new GoogleOauthProvider(fake, Client, Secret)
    val ex = intercept[RuntimeException](p.exchangeCode("X", "https://k/cb"))
    ex.getMessage should include ("missing access_token")
    ex.getMessage should include ("invalid_grant")
  }

  it should "throw when /userinfo omits sub — the User upsert key would be empty" in {
    val fake = new ScriptedFetch(Map(
      "oauth2.googleapis.com/token"       -> """{"access_token":"t"}""",
      "googleapis.com/oauth2/v3/userinfo" -> """{"email":"u@x.com"}"""
    ))
    val p  = new GoogleOauthProvider(fake, Client, Secret)
    val ex = intercept[RuntimeException](p.exchangeCode("X", "https://k/cb"))
    ex.getMessage should include ("missing sub")
  }

  it should "treat email / name / picture as optional — a sub-only response yields a valid profile" in {
    val fake = new ScriptedFetch(Map(
      "oauth2.googleapis.com/token"       -> """{"access_token":"t"}""",
      "googleapis.com/oauth2/v3/userinfo" -> """{"sub":"S"}"""
    ))
    new GoogleOauthProvider(fake, Client, Secret).exchangeCode("X", "https://k/cb") shouldBe
      OauthProfile(sub = "S", email = None, displayName = None, avatarUrl = None)
  }

  "Google.name" should "be 'google' (matches the route :provider segment)" in {
    new GoogleOauthProvider(new ScriptedFetch(Map.empty), Client, Secret).name shouldBe "google"
  }
}
