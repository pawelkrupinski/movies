package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.users.InMemoryUserRepository

import java.time.{Instant, LocalDateTime}

/**
 * A page rendered for somebody must not be re-usable.
 *
 * Signed-OUT pages are byte-identical for every visitor and are cached hard on
 * purpose. A signed-in page carries an avatar, a display name and that person's
 * hidden films, and took the other branch — which set NO cache headers at all.
 * A response with no headers is heuristically cacheable, so the browser was free
 * to keep showing a signed-in page to somebody who had just signed out, which is
 * exactly what it did: "it seems to log out but the avatar stays".
 */
class PersonalisedPageCacheSpec extends AnyFlatSpec with Matchers {

  private val Now = LocalDateTime.now()

  private def controller(users: InMemoryUserRepository) = TestMovieController.build(
    Seq(("Test Film", Some(2024), MovieRecord(
      imdbId = Some("tt999"),
      data = Map[Source, SourceData](Helios -> SourceData(
        title = Some("Test Film"), releaseYear = Some(2024),
        showtimes = Seq(models.Showtime(Now.plusHours(2), None, None, Nil))))))),
    userRepository = users)._1

  private def signedIn(): InMemoryUserRepository = {
    val users = new InMemoryUserRepository
    users.upsert(models.User(
      id = "alice@example.com", provider = "google", providerSub = "sub-1",
      email = Some("alice@example.com"), displayName = Some("Alice"), avatarUrl = None,
      createdAt = Instant.EPOCH, lastSeenAt = Instant.EPOCH))
    users
  }

  "A page rendered for a signed-in visitor" should "forbid the browser from storing it" in {
    val result = controller(signedIn()).index("poznan")(
      FakeRequest("GET", "/poznan/").withSession("userId" -> "alice@example.com"))

    header("Cache-Control", result).value shouldBe PersonalisedPage.CacheControl
  }

  // `no-store`, not merely `no-cache`. `no-cache` still permits STORING and only
  // forces revalidation, which the back/forward cache ignores entirely — so
  // "log out, press Back, signed in again" would survive it.
  it should "say no-store, which is the only thing bfcache honours" in {
    val result = controller(signedIn()).index("poznan")(
      FakeRequest("GET", "/poznan/").withSession("userId" -> "alice@example.com"))

    header("Cache-Control", result).value should include ("no-store")
  }

  // The caching that matters on this site is the anonymous page, which is
  // byte-identical for everyone and must stay as cacheable as it was.
  "An anonymous page" should "keep its revalidating cache headers" in {
    val result = controller(new InMemoryUserRepository).index("poznan")(
      FakeRequest("GET", "/poznan/").withHeaders("Accept-Encoding" -> "gzip"))

    header("Cache-Control", result).value shouldBe "private, no-cache"
  }

  // A signed-in visitor whose account has since been deleted resolves to nobody,
  // so the render is anonymous and cacheable — the header follows the RENDER,
  // not the presence of a cookie.
  "A session naming a user who no longer exists" should "render as anonymous" in {
    val result = controller(new InMemoryUserRepository).index("poznan")(
      FakeRequest("GET", "/poznan/").withSession("userId" -> "deleted@example.com")
        .withHeaders("Accept-Encoding" -> "gzip"))

    header("Cache-Control", result).value shouldBe "private, no-cache"
  }
}
