package services.auth

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.HttpFetch

import scala.collection.mutable

class FacebookOauthProviderSpec extends AnyFlatSpec with Matchers {

  private class ScriptedFetch(replies: Map[String, String]) extends HttpFetch {
    val calls: mutable.ListBuffer[(String, String)] = mutable.ListBuffer.empty
    override def get(url: String): String = {
      calls += (("GET", url))
      replies.collectFirst { case (k, v) if url.contains(k) => v }
        .getOrElse(throw new RuntimeException(s"No scripted reply for GET $url"))
    }
    override def post(url: String, body: String, contentType: String): String = {
      calls += (("POST", url))
      throw new RuntimeException("Facebook flow should never POST — it's GET-only")
    }
  }

  private val AppId  = "1234567890"
  private val Secret = "FB_APP_SECRET"

  "Facebook.authUrl" should "hit Facebook's dialog/oauth endpoint with email + public_profile scopes" in {
    val p   = new FacebookOauthProvider(new ScriptedFetch(Map.empty), AppId, Secret)
    val url = p.authUrl("xyz", "https://k/auth/facebook/callback")
    url should startWith ("https://www.facebook.com/v18.0/dialog/oauth?")
    url should include ("scope=email%2Cpublic_profile")
    url should include ("state=xyz")
    url should include ("client_id=1234567890")
  }

  "Facebook.exchangeCode" should "hit GET /token then GET /me?fields=… and parse the nested picture.data.url" in {
    val fake = new ScriptedFetch(Map(
      "graph.facebook.com/v18.0/oauth/access_token" -> """{"access_token":"EAAtoken","token_type":"bearer","expires_in":5183999}""",
      "graph.facebook.com/v18.0/me"                 ->
        """{"id":"100012345","name":"Test FB","email":"u@example.com","picture":{"data":{"url":"https://platform-lookaside.fbsbx.com/avatar.jpg","width":50,"height":50,"is_silhouette":false}}}"""
    ))
    val p = new FacebookOauthProvider(fake, AppId, Secret)
    val profile = p.exchangeCode("CODE_abc", "https://k/auth/facebook/callback")

    profile shouldBe OauthProfile(
      sub         = "100012345",
      email       = Some("u@example.com"),
      displayName = Some("Test FB"),
      avatarUrl   = Some("https://platform-lookaside.fbsbx.com/avatar.jpg")
    )

    fake.calls.toSeq.map(_._1) shouldBe Seq("GET", "GET")
    fake.calls.toSeq(0)._2 should include ("oauth/access_token")
    fake.calls.toSeq(0)._2 should include ("code=CODE_abc")
    fake.calls.toSeq(1)._2 should include ("/me?fields=id,name,email,picture")
    fake.calls.toSeq(1)._2 should include ("access_token=EAAtoken")
  }

  it should "accept an email-less profile — Facebook lets users decline email scope" in {
    // About a third of Facebook users decline the email permission. We
    // shouldn't reject those logins; just store email = None and carry on.
    val fake = new ScriptedFetch(Map(
      "graph.facebook.com/v18.0/oauth/access_token" -> """{"access_token":"t"}""",
      "graph.facebook.com/v18.0/me"                 -> """{"id":"S","name":"FB No-Email"}"""
    ))
    val p = new FacebookOauthProvider(fake, AppId, Secret)
    p.exchangeCode("X", "https://k/cb") shouldBe
      OauthProfile(sub = "S", email = None, displayName = Some("FB No-Email"), avatarUrl = None)
  }

  it should "throw when /me omits id" in {
    val fake = new ScriptedFetch(Map(
      "graph.facebook.com/v18.0/oauth/access_token" -> """{"access_token":"t"}""",
      "graph.facebook.com/v18.0/me"                 -> """{"name":"Idless"}"""
    ))
    val ex = intercept[RuntimeException] {
      new FacebookOauthProvider(fake, AppId, Secret).exchangeCode("X", "https://k/cb")
    }
    ex.getMessage should include ("missing id")
  }

  "Facebook.name" should "be 'facebook' (matches the route :provider segment)" in {
    new FacebookOauthProvider(new ScriptedFetch(Map.empty), AppId, Secret).name shouldBe "facebook"
  }
}
