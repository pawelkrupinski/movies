package controllers

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.UserState
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsNull, Json}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.metrics.LegacyUserStateMetrics
import services.users.{AccountDeletion, HiddenFilmsChange, InMemoryUserRepository, LegacyStatePatch, InMemoryUserStateRepository, NoUserChangeTimeCache, UserChangeTimeCache}

import java.time.Instant

class UserStateControllerSpec extends AnyFlatSpec with Matchers {

  // The controller stamps changes on this clock; the rows a test seeds are stamped from it too.
  private val specClock = java.time.Clock.fixed(Instant.parse("2026-06-01T10:00:00Z"), java.time.ZoneOffset.UTC)

  // Every action now routes through `SignedInUser` (see `signedInUserId`),
  // not just a bare session `userId` claim — so a test session names a real
  // row here, or the request 401s regardless of what the session carries.
  // Fields beyond `id` are arbitrary; nothing here asserts on them.
  private def testUser(id: String): models.User = models.User(
    id = id, provider = "google", providerSub = s"G-$id",
    email = Some(s"$id@example.com"), displayName = Some(id), avatarUrl = None,
    createdAt = specClock.instant(), lastSeenAt = specClock.instant()
  )

  private def fixture(
    prefilled:       Option[UserState] = None,
    changeTimeCache: UserChangeTimeCache = NoUserChangeTimeCache,
    stateRepository: InMemoryUserStateRepository = new InMemoryUserStateRepository,
    legacyMetrics:   LegacyUserStateMetrics = new LegacyUserStateMetrics(new PrometheusRegistry(), "pl", specClock)
  ): (UserStateController, InMemoryUserStateRepository, InMemoryUserRepository) = {
    val userRepository  = new InMemoryUserRepository
    userRepository.upsert(testUser("u1")) // the suite's default signed-in identity
    prefilled.foreach(stateRepository.upsert)
    val accountDeletion = new AccountDeletion(userRepository, stateRepository)
    (new UserStateController(Helpers.stubControllerComponents(), stateRepository, accountDeletion, changeTimeCache, legacyMetrics, userRepository, specClock), stateRepository, userRepository)
  }

  /** Counts `find` calls so a fast-path test can prove storage was never
   *  touched, not just that the response happened to be correct. */
  private class CountingUserStateRepository extends InMemoryUserStateRepository {
    var findCalls: Int = 0
    override def find(userId: String): Option[UserState] = { findCalls += 1; super.find(userId) }
  }

  /** A `UserChangeTimeCache` whose answers are fixed in the test, not derived
   *  from a real change stream — `UserChangeTimeCacheSpec` covers the real
   *  `CaffeineUserChangeTimeCache`'s own behaviour (eviction, invalidate-on-
   *  disconnect); this controller only needs to prove it CONSULTS whatever
   *  the trait says. */
  private class StubUserChangeTimeCache(times: Map[String, Instant] = Map.empty) extends UserChangeTimeCache {
    def lastChangeAt(userId: String): Option[Instant] = times.get(userId)
  }

  // ── GET /api/me/state ─────────────────────────────────────────────────────

  "GET /api/me/state" should "401 anonymous requests" in {
    val (ctl, _, _) = fixture()
    val result   = ctl.get()(FakeRequest("GET", "/api/me/state"))
    status(result) shouldBe UNAUTHORIZED
  }

  // Regression: before 2026-09-20 this controller trusted the session
  // cookie's bare `userId` claim directly, so a session `/api/me` had
  // already rejected — user row deleted, or session revoked — could still
  // read and write this endpoint's state indefinitely. Routing through
  // `SignedInUser` (`signedInUserId`) closes that gap; these two prove it.
  it should "401 a session whose user row no longer exists" in {
    val (ctl, _, _) = fixture()
    val result = ctl.get()(FakeRequest("GET", "/api/me/state").withSession("userId" -> "ghost"))
    status(result) shouldBe UNAUTHORIZED
  }

  it should "401 a session revoked since it was issued (sessionVersion mismatch)" in {
    val (ctl, _, userRepository) = fixture()
    userRepository.upsert(userRepository.findById("u1").value.copy(sessionVersion = 1))
    // The session itself still names sessionVersion 0 — as if issued before
    // the revoking "sign out everywhere" call bumped the row to 1.
    val request = FakeRequest("GET", "/api/me/state").withSession("userId" -> "u1", "sessionVersion" -> "0")
    status(ctl.get()(request)) shouldBe UNAUTHORIZED
  }

  // This endpoint holds one person's hidden films and disabled cinemas, and it
  // used to carry no `Cache-Control` at all — a response with none is
  // HEURISTICALLY cacheable, so nothing but luck kept a copy out of a browser or
  // a proxy. It matters more now than it did: the HTML pages stopped rendering
  // anybody, which makes these endpoints the entire per-user surface.
  it should "forbid anything keeping a copy of one person's state" in {
    val (ctl, _, userRepository) = fixture()
    userRepository.upsert(testUser("alice"))
    val result = ctl.get()(FakeRequest("GET", "/api/me/state").withSession("userId" -> "alice"))

    status(result) shouldBe OK
    header("Cache-Control", result).value shouldBe PerUserResponse.CacheControl
  }

  it should "return an empty state for a user with no stored row" in {
    val (ctl, _, userRepository) = fixture()
    userRepository.upsert(testUser("newbie"))
    val request  = FakeRequest("GET", "/api/me/state").withSession("userId" -> "newbie")
    val result   = ctl.get()(request)
    status(result)              shouldBe OK
    contentAsJson(result)       shouldBe Json.obj(
      "hiddenFilms"         -> Json.arr(),
      "disabledCinemas"     -> Json.arr(),
      "language"            -> JsNull
    )
  }

  it should "return the stored state sorted (deterministic wire format)" in {
    val stored = UserState(
      userId          = "u1",
      hiddenFilms     = Set("Madagaskar", "ABC"),
      disabledCinemas = Set("Kino Apollo"),
      updatedAt       = Instant.parse("2026-05-19T12:00:00Z")
    )
    val (ctl, _, _) = fixture(Some(stored))
    val request  = FakeRequest("GET", "/api/me/state").withSession("userId" -> "u1")
    val result   = ctl.get()(request)

    status(result) shouldBe OK
    val js = contentAsJson(result)
    (js \ "hiddenFilms").as[Seq[String]]     shouldBe Seq("ABC", "Madagaskar")
  }

  it should "return a stored language pick" in {
    val stored = UserState("u1", Set.empty, Set.empty, specClock.instant(), language = Some("de"))
    val (ctl, _, _) = fixture(Some(stored))
    val request  = FakeRequest("GET", "/api/me/state").withSession("userId" -> "u1")
    val result   = ctl.get()(request)

    status(result) shouldBe OK
    (contentAsJson(result) \ "language").as[String] shouldBe "de"
  }

  // ── GET /api/me/:country/hidden-films ─────────────────────────────────────
  // Per-country: `hiddenFilmsByCountry`, NOT the legacy global `hiddenFilms`
  // field `get()`/`put()` still serve — a title is not globally unique across
  // countries the way a cinema display name is.

  private def storedFor(country: String, films: String*): UserState =
    UserState("u1", Set.empty, Set.empty, Instant.parse("2026-05-19T12:00:00Z"), Map(country -> films.toSet))

  /** Calls `hiddenFilms(country)` against a request built for that same
   *  country's path, so a test only ever names the country once. */
  private def callHiddenFilms(
    ctl:     UserStateController,
    userId:  String = "u1",
    country: String = "pl",
    headers: (String, String)*
  ) =
    ctl.hiddenFilms(country)(
      FakeRequest("GET", s"/api/me/$country/hidden-films").withSession("userId" -> userId).withHeaders(headers*)
    )

  // A store that could not be READ used to answer as though the user had no state: 200,
  // an empty list, and a validator the client caches — so a Mongo blip told the app
  // "nothing is hidden".
  "GET /api/me/state and /api/me/:country/hidden-films" should "503 when the state cannot be read, never serve it as empty" in {
    val (ctl, _, _) = fixture(stateRepository = new services.users.FailingReadUserStateRepository)
    status(ctl.get()(FakeRequest("GET", "/api/me/state").withSession("userId" -> "u1"))) shouldBe SERVICE_UNAVAILABLE
    val hidden = callHiddenFilms(ctl)
    status(hidden) shouldBe SERVICE_UNAVAILABLE
    header("ETag", hidden) shouldBe None
  }

  "GET /api/me/:country/hidden-films" should "401 anonymous requests" in {
    val (ctl, _, _) = fixture()
    val result = ctl.hiddenFilms("pl")(FakeRequest("GET", "/api/me/pl/hidden-films"))
    status(result) shouldBe UNAUTHORIZED
  }

  it should "400 an unrecognised country code" in {
    val (ctl, _, _) = fixture()
    val result = callHiddenFilms(ctl, country = "xx")
    status(result) shouldBe BAD_REQUEST
  }

  it should "forbid anything keeping a copy of one person's state" in {
    val (ctl, _, userRepository) = fixture()
    userRepository.upsert(testUser("alice"))
    val result = callHiddenFilms(ctl, userId = "alice")
    header("Cache-Control", result).value shouldBe PerUserResponse.CacheControl
  }

  it should "return only THIS country's hidden films — no disabledCinemas key, no other country's titles" in {
    val stored = UserState("u1", Set.empty, Set("Kino Apollo"), Instant.parse("2026-05-19T12:00:00Z"),
      Map("pl" -> Set("Madagaskar"), "us" -> Set("Sing")))
    val (ctl, _, _) = fixture(Some(stored))
    val result = callHiddenFilms(ctl, country = "pl")

    status(result) shouldBe OK
    val js = contentAsJson(result)
    (js \ "hiddenFilms").as[Seq[String]] shouldBe Seq("Madagaskar")
    (js \ "disabledCinemas").toOption    shouldBe empty
  }

  it should "return an empty list for a country the user has no bucket for yet" in {
    val (ctl, _, _) = fixture(Some(storedFor("pl", "Madagaskar")))
    val result = callHiddenFilms(ctl, country = "us")
    status(result) shouldBe OK
    (contentAsJson(result) \ "hiddenFilms").as[Seq[String]] shouldBe empty
  }

  it should "carry an ETag and a Last-Modified header on a 200" in {
    val (ctl, _, _) = fixture(Some(storedFor("pl", "Madagaskar")))
    val result = callHiddenFilms(ctl)

    status(result) shouldBe OK
    header("ETag", result)         shouldBe defined
    header("Last-Modified", result) shouldBe Some("Tue, 19 May 2026 12:00:00 GMT")
  }

  it should "304 when If-None-Match already holds the current ETag" in {
    val (ctl, _, _) = fixture(Some(storedFor("pl", "Madagaskar")))
    val first = callHiddenFilms(ctl)
    val etag  = header("ETag", first).value

    val second = callHiddenFilms(ctl, headers = Seq("If-None-Match" -> etag)*)
    status(second) shouldBe NOT_MODIFIED
    // A 304 still carries the validators — a client refreshing its cached
    // copy's freshness needs the (unchanged) Last-Modified back too.
    header("ETag", second).value shouldBe etag
  }

  it should "200 with a fresh body when THIS country's hiddenFilms changed since the client's ETag" in {
    val stored = storedFor("pl", "Madagaskar")
    val (ctl, repository, _) = fixture(Some(stored))
    val first = callHiddenFilms(ctl)
    val staleEtag = header("ETag", first).value

    repository.upsert(stored.copy(
      hiddenFilmsByCountry = Map("pl" -> Set("Madagaskar", "Sing")),
      updatedAt             = Instant.parse("2026-05-20T09:00:00Z")))
    val second = callHiddenFilms(ctl, headers = Seq("If-None-Match" -> staleEtag)*)
    status(second) shouldBe OK
    (contentAsJson(second) \ "hiddenFilms").as[Seq[String]] shouldBe Seq("Madagaskar", "Sing")
  }

  it should "304 on a still-current If-Modified-Since when the client sent no If-None-Match" in {
    val (ctl, _, _) = fixture(Some(storedFor("pl", "Madagaskar")))
    val result = callHiddenFilms(ctl, headers = Seq("If-Modified-Since" -> "Tue, 19 May 2026 12:00:00 GMT")*)
    status(result) shouldBe NOT_MODIFIED
  }

  // If-None-Match takes precedence over If-Modified-Since per RFC 7232 §3.3 —
  // load-bearing here because `updatedAt` is the whole DOCUMENT's timestamp
  // (shared across every country's bucket plus the legacy fields), so a
  // stale If-Modified-Since alone is not proof THIS country's list is
  // unchanged.
  it should "ignore a stale If-Modified-Since when If-None-Match already proves the content is current" in {
    val (ctl, _, _) = fixture(Some(storedFor("pl", "Madagaskar")))
    val first = callHiddenFilms(ctl)
    val etag  = header("ETag", first).value

    val result = callHiddenFilms(ctl, headers = Seq(
      "If-None-Match"     -> etag,
      "If-Modified-Since" -> "Mon, 01 Jan 2001 00:00:00 GMT"
    )*)
    status(result) shouldBe NOT_MODIFIED
  }

  // ── concurrent writes for one user ───────────────────────────────────────
  //
  // A per-title hidden-films write is ONE atomic store operation, not a
  // read-modify-write of the row — so overlapping requests (another tab, the
  // app) each land, and each answers with what the store holds right after it.
  // The Mongo side of the same guarantee: HiddenFilmsConcurrentWritesIntegrationSpec.

  "a hidden-films write" should "not lose any of many parallel hides for one user" in {
    val (ctl, repository, _) = fixture(Some(storedFor("de", "Der Film")))
    val pool   = java.util.concurrent.Executors.newFixedThreadPool(16)
    val titles = (1 to 64).map(i => s"Film $i")
    try {
      val results = titles.map(t => pool.submit(() =>
        status(ctl.hideFilm("pl", t)(FakeRequest("PUT", "/api/me/pl/hidden-films/x").withSession("userId" -> "u1")))))
      results.map(_.get).distinct shouldBe Seq(OK)
    } finally pool.shutdown()

    val stored = repository.find("u1").value.hiddenFilmsByCountry
    stored("pl") shouldBe titles.toSet
    stored("de") shouldBe Set("Der Film") // another country's bucket, untouched
  }

  it should "answer with the bucket as actually stored, including a write that landed alongside it" in {
    val alongside = new InMemoryUserStateRepository {
      override def changeHiddenFilms(userId: String, country: String, change: HiddenFilmsChange, now: Instant): Option[UserState] = {
        super.changeHiddenFilms(userId, country, HiddenFilmsChange.Hide("Other Tab", UserStateController.MaxHiddenPerCountry), now)
        super.changeHiddenFilms(userId, country, change, now)
      }
    }
    val (ctl, _, _) = fixture(Some(storedFor("pl", "Madagaskar")), stateRepository = alongside)

    val result = ctl.hideFilm("pl", "Sing")(FakeRequest("PUT", "/api/me/pl/hidden-films/Sing").withSession("userId" -> "u1"))
    (contentAsJson(result) \ "hiddenFilms").as[Seq[String]] shouldBe Seq("Madagaskar", "Other Tab", "Sing")
  }

  it should "bump updatedAt past the stored one even within the same millisecond" in {
    val stamp = specClock.instant().plusSeconds(3600).truncatedTo(java.time.temporal.ChronoUnit.MILLIS) // "now" is behind the row: only the +1ms rule can move it
    val (ctl, repository, _) = fixture(Some(storedFor("pl", "Madagaskar").copy(updatedAt = stamp)))
    val result = ctl.unhideFilm("pl", "Never Hidden")(FakeRequest("DELETE", "/api/me/pl/hidden-films/x").withSession("userId" -> "u1"))

    repository.find("u1").value.updatedAt shouldBe stamp.plusMillis(1)
    header("Last-Modified", result).value shouldBe UserStateController.httpDate(stamp.plusMillis(1))
  }

  it should "503 when the store could not write at all" in {
    val failing = new InMemoryUserStateRepository {
      override def changeHiddenFilms(userId: String, country: String, change: HiddenFilmsChange, now: Instant): Option[UserState] = None
    }
    val (ctl, _, _) = fixture(Some(storedFor("pl", "Madagaskar")), stateRepository = failing)
    status(ctl.hideFilm("pl", "Sing")(FakeRequest("PUT", "/api/me/pl/hidden-films/Sing").withSession("userId" -> "u1"))) shouldBe SERVICE_UNAVAILABLE
  }

  "UserState.nextUpdatedAt" should "move the version even when two writes share a millisecond" in {
    val read = Instant.parse("2026-05-19T12:00:00.123Z")
    UserState.nextUpdatedAt(Some(read), now = Instant.parse("2026-05-19T12:00:00.123456Z")) shouldBe
      Instant.parse("2026-05-19T12:00:00.124Z")
    UserState.nextUpdatedAt(Some(read), now = Instant.parse("2026-05-19T12:00:05.5Z")) shouldBe
      Instant.parse("2026-05-19T12:00:05.500Z")
    UserState.nextUpdatedAt(None, now = Instant.parse("2026-05-19T12:00:00.123456Z")) shouldBe
      Instant.parse("2026-05-19T12:00:00.123Z")
  }

  "a legacy PUT /api/me/state" should "not erase, nor be erased by, per-title hides racing it" in {
    val (ctl, repository, _) = fixture()
    val pool   = java.util.concurrent.Executors.newFixedThreadPool(16)
    val titles = (1 to 32).map(i => s"Film $i")
    try {
      val writes = titles.flatMap(t => Seq(
        pool.submit(() => status(ctl.hideFilm("pl", t)(FakeRequest("PUT", "/api/me/pl/hidden-films/x").withSession("userId" -> "u1")))),
        pool.submit(() => status(ctl.put()(FakeRequest("PUT", "/api/me/state").withSession("userId" -> "u1")
          .withBody(Json.obj("language" -> "en", "disabledCinemas" -> Json.arr("Kino"))))))))
      writes.map(_.get).distinct shouldBe Seq(OK)
    } finally pool.shutdown()

    val stored = repository.find("u1").value
    stored.hiddenFilmsByCountry("pl") shouldBe titles.toSet
    stored.language shouldBe Some("en")
    stored.disabledCinemas shouldBe Set("Kino")
  }

  it should "503 when the store could not write at all" in {
    val failing = new InMemoryUserStateRepository {
      override def patchLegacyState(userId: String, patch: LegacyStatePatch, now: Instant): Option[UserState] = None
    }
    val (ctl, _, _) = fixture(Some(storedFor("pl", "Madagaskar")), stateRepository = failing)
    val result = ctl.put()(FakeRequest("PUT", "/api/me/state").withSession("userId" -> "u1").withBody(Json.obj("language" -> "en")))
    status(result) shouldBe SERVICE_UNAVAILABLE
  }

  it should "400 an entry longer than any real title, and 413 a set past the per-set bound — storing nothing" in {
    val (ctl, repository, _) = fixture(Some(storedFor("pl", "Madagaskar")))
    def put(body: play.api.libs.json.JsObject) =
      status(ctl.put()(FakeRequest("PUT", "/api/me/state").withSession("userId" -> "u1").withBody(body)))

    put(Json.obj("hiddenFilms" -> Json.arr("x" * (UserStateController.MaxTitleLength + 1)))) shouldBe BAD_REQUEST
    put(Json.obj("disabledCinemas" -> (0 to UserStateController.MaxHiddenPerCountry).map(i => s"Kino $i"))) shouldBe REQUEST_ENTITY_TOO_LARGE
    repository.find("u1").value shouldBe storedFor("pl", "Madagaskar")
  }

  // ── GET /api/me/:country/hidden-films — userChangeTimeCache fast path ────

  "GET /api/me/:country/hidden-films with a change-time cache" should "304 straight from the change-time cache, without reading storage, on a proven-unchanged If-Modified-Since" in {
    val countingRepo = new CountingUserStateRepository
    val cache = new StubUserChangeTimeCache(Map("u1" -> Instant.parse("2026-05-19T12:00:00Z")))
    val (ctl, _, _) = fixture(Some(storedFor("pl", "Madagaskar")), changeTimeCache = cache, stateRepository = countingRepo)
    countingRepo.findCalls = 0 // the prefill's own upsert doesn't call find; reset defensively anyway

    val result = callHiddenFilms(ctl, headers = Seq("If-Modified-Since" -> "Wed, 20 May 2026 00:00:00 GMT")*)

    status(result)             shouldBe NOT_MODIFIED
    countingRepo.findCalls     shouldBe 0
  }

  it should "fall through to storage when the change-time cache has no answer for this user" in {
    val countingRepo = new CountingUserStateRepository
    val (ctl, _, _) = fixture(Some(storedFor("pl", "Madagaskar")), changeTimeCache = NoUserChangeTimeCache, stateRepository = countingRepo)
    countingRepo.findCalls = 0

    val result = callHiddenFilms(ctl, headers = Seq("If-Modified-Since" -> "Wed, 20 May 2026 00:00:00 GMT")*)

    status(result)         shouldBe NOT_MODIFIED // still correct — just computed from storage, not the cache
    countingRepo.findCalls shouldBe 1
  }

  it should "never take the cache fast path when If-None-Match is present, even if the cache would answer" in {
    val countingRepo = new CountingUserStateRepository
    val cache = new StubUserChangeTimeCache(Map("u1" -> Instant.parse("2026-05-19T12:00:00Z")))
    val (ctl, _, _) = fixture(Some(storedFor("pl", "Madagaskar")), changeTimeCache = cache, stateRepository = countingRepo)
    countingRepo.findCalls = 0

    val result = callHiddenFilms(ctl, headers = Seq(
      "If-None-Match"     -> "\"whatever\"",
      "If-Modified-Since" -> "Wed, 20 May 2026 00:00:00 GMT"
    )*)

    countingRepo.findCalls shouldBe 1 // ETag comparison needs real content — the cache can't answer it
  }

  // ── PUT/DELETE /api/me/:country/hidden-films(/:title) ────────────────────

  "PUT /api/me/:country/hidden-films/:title" should "401 anonymous requests" in {
    val (ctl, _, _) = fixture()
    status(ctl.hideFilm("pl", "Sing")(FakeRequest("PUT", "/api/me/pl/hidden-films/Sing"))) shouldBe UNAUTHORIZED
  }

  it should "400 an unrecognised country code" in {
    val (ctl, _, _) = fixture()
    val result = ctl.hideFilm("xx", "Sing")(FakeRequest("PUT", "/api/me/xx/hidden-films/Sing").withSession("userId" -> "u1"))
    status(result) shouldBe BAD_REQUEST
  }

  it should "add the title to that country's bucket, leaving other countries untouched" in {
    val stored = UserState("u1", Set.empty, Set.empty, specClock.instant(), Map("us" -> Set("Sing")))
    val (ctl, repository, _) = fixture(Some(stored))
    val result = ctl.hideFilm("pl", "Madagaskar")(FakeRequest("PUT", "/api/me/pl/hidden-films/Madagaskar").withSession("userId" -> "u1"))

    status(result) shouldBe OK
    (contentAsJson(result) \ "hiddenFilms").as[Seq[String]] shouldBe Seq("Madagaskar")
    val stateAfter = repository.find("u1").value
    stateAfter.hiddenFilmsByCountry("pl") shouldBe Set("Madagaskar")
    stateAfter.hiddenFilmsByCountry("us") shouldBe Set("Sing") // untouched
  }

  it should "be idempotent — hiding an already-hidden title changes nothing" in {
    val (ctl, repository, _) = fixture(Some(storedFor("pl", "Madagaskar")))
    val result = ctl.hideFilm("pl", "Madagaskar")(FakeRequest("PUT", "/api/me/pl/hidden-films/Madagaskar").withSession("userId" -> "u1"))

    status(result) shouldBe OK
    repository.find("u1").value.hiddenFilmsByCountry("pl") shouldBe Set("Madagaskar")
  }

  it should "carry fresh ETag/Last-Modified so the client needn't re-GET" in {
    val (ctl, _, _) = fixture(Some(storedFor("pl", "Madagaskar")))
    val result = ctl.hideFilm("pl", "Sing")(FakeRequest("PUT", "/api/me/pl/hidden-films/Sing").withSession("userId" -> "u1"))
    header("ETag", result)          shouldBe defined
    header("Last-Modified", result) shouldBe defined
  }

  "DELETE /api/me/:country/hidden-films/:title" should "401 anonymous requests" in {
    val (ctl, _, _) = fixture()
    status(ctl.unhideFilm("pl", "Sing")(FakeRequest("DELETE", "/api/me/pl/hidden-films/Sing"))) shouldBe UNAUTHORIZED
  }

  it should "400 an unrecognised country code" in {
    val (ctl, _, _) = fixture()
    val result = ctl.unhideFilm("xx", "Sing")(FakeRequest("DELETE", "/api/me/xx/hidden-films/Sing").withSession("userId" -> "u1"))
    status(result) shouldBe BAD_REQUEST
  }

  it should "remove the title from that country's bucket, leaving other countries untouched" in {
    val stored = UserState("u1", Set.empty, Set.empty, specClock.instant(), Map("pl" -> Set("Madagaskar", "Sing"), "us" -> Set("Sing")))
    val (ctl, repository, _) = fixture(Some(stored))
    val result = ctl.unhideFilm("pl", "Sing")(FakeRequest("DELETE", "/api/me/pl/hidden-films/Sing").withSession("userId" -> "u1"))

    status(result) shouldBe OK
    (contentAsJson(result) \ "hiddenFilms").as[Seq[String]] shouldBe Seq("Madagaskar")
    val stateAfter = repository.find("u1").value
    stateAfter.hiddenFilmsByCountry("pl") shouldBe Set("Madagaskar")
    stateAfter.hiddenFilmsByCountry("us") shouldBe Set("Sing") // untouched
  }

  it should "be idempotent — unhiding a title that isn't hidden changes nothing" in {
    val (ctl, repository, _) = fixture(Some(storedFor("pl", "Madagaskar")))
    val result = ctl.unhideFilm("pl", "Never Hidden")(FakeRequest("DELETE", "/api/me/pl/hidden-films/Never%20Hidden").withSession("userId" -> "u1"))

    status(result) shouldBe OK
    repository.find("u1").value.hiddenFilmsByCountry("pl") shouldBe Set("Madagaskar")
  }

  "DELETE /api/me/:country/hidden-films" should "401 anonymous requests" in {
    val (ctl, _, _) = fixture()
    status(ctl.clearHiddenFilms("pl")(FakeRequest("DELETE", "/api/me/pl/hidden-films"))) shouldBe UNAUTHORIZED
  }

  it should "400 an unrecognised country code" in {
    val (ctl, _, _) = fixture()
    val result = ctl.clearHiddenFilms("xx")(FakeRequest("DELETE", "/api/me/xx/hidden-films").withSession("userId" -> "u1"))
    status(result) shouldBe BAD_REQUEST
  }

  it should "empty only THAT country's bucket, leaving other countries untouched" in {
    val stored = UserState("u1", Set.empty, Set.empty, specClock.instant(), Map("pl" -> Set("Madagaskar", "Sing"), "us" -> Set("Sing")))
    val (ctl, repository, _) = fixture(Some(stored))
    val result = ctl.clearHiddenFilms("pl")(FakeRequest("DELETE", "/api/me/pl/hidden-films").withSession("userId" -> "u1"))

    status(result) shouldBe OK
    (contentAsJson(result) \ "hiddenFilms").as[Seq[String]] shouldBe empty
    val stateAfter = repository.find("u1").value
    stateAfter.hiddenFilmsByCountry("pl") shouldBe empty
    stateAfter.hiddenFilmsByCountry("us") shouldBe Set("Sing") // untouched
  }

  it should "be harmless for a user with no row at all yet" in {
    val (ctl, repository, _) = fixture()
    val result = ctl.clearHiddenFilms("pl")(FakeRequest("DELETE", "/api/me/pl/hidden-films").withSession("userId" -> "u1"))
    status(result) shouldBe OK
    repository.find("u1").value.hiddenFilmsByCountry.getOrElse("pl", Set.empty) shouldBe empty
  }

  // ── PUT /api/me/state ─────────────────────────────────────────────────────

  "PUT /api/me/state" should "401 anonymous requests without writing anything" in {
    val (ctl, repository, _) = fixture()
    val request = FakeRequest("PUT", "/api/me/state")
      .withBody(Json.obj("hiddenFilms" -> Json.arr("X")))
      .withHeaders("Content-Type" -> "application/json")
    val result = ctl.put()(request)
    status(result)              shouldBe UNAUTHORIZED
    repository.find("anyone")         shouldBe empty
  }

  // The usage-metric records EVERY call, even a 401 — see LegacyUserStateMetrics:
  // "something still calls this URL at all" is the retirement question, and an
  // anonymous/malformed call is still evidence of that.
  it should "record the call on the legacy-usage gauge even when anonymous" in {
    val registry = new PrometheusRegistry()
    val metrics  = new LegacyUserStateMetrics(registry, "pl", specClock)
    val (ctl, _, _) = fixture(legacyMetrics = metrics)
    ctl.put()(FakeRequest("PUT", "/api/me/state").withBody(Json.obj("hiddenFilms" -> Json.arr("X"))))

    services.metrics.PrometheusExposition.render(registry) should include ("kinowo_web_legacy_userstate_put_last_called_seconds")
  }

  // `language` has no granular successor and the web client itself PUTs it
  // here on every explicit pick (shared.js's `pushStateToServer`) — that is
  // the endpoint's intended, ongoing use, not a legacy client. Counting it
  // would pin the retirement gauge at "called just now" forever and hide
  // whether any client still sends the sets the granular API replaced.
  it should "leave the legacy-usage gauge alone for a language-only body" in {
    val registry = new PrometheusRegistry()
    val metrics  = new LegacyUserStateMetrics(registry, "pl", specClock)
    val (ctl, _, _) = fixture(legacyMetrics = metrics)
    status(ctl.put()(FakeRequest("PUT", "/api/me/state").withSession("userId" -> "u1")
      .withBody(Json.obj("language" -> "en")))) shouldBe OK

    services.metrics.PrometheusExposition.render(registry) should not include ("kinowo_web_legacy_userstate_put_last_called_seconds{")
  }

  it should "replace the user's state with the request body" in {
    val initial = UserState("u1", Set("OLD"), Set.empty, specClock.instant())
    val (ctl, repository, _) = fixture(Some(initial))
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj(
        "hiddenFilms"     -> Json.arr("Hidden A")
      ))

    val result = ctl.put()(request)
    status(result) shouldBe OK

    val stored = repository.find("u1").value
    stored.hiddenFilms     shouldBe Set("Hidden A")
    stored.disabledCinemas shouldBe empty
  }

  // THE RULE OUTLIVES THE FIELDS IT WAS WRITTEN FOR. This covered a client that sent only the
  // sets it modelled while the web carried two more (`selectedMovies` / `favouriteRooms`, retired
  // with the plan page). Both remaining fields are now modelled by every client, so the omission
  // is constructed rather than incidental — but the rule is the contract, and the next field added
  // on one platform before the other depends on it.
  it should "preserve a field the body omits, rather than clearing it" in {
    val initial = UserState(
      userId          = "u1",
      hiddenFilms     = Set("OLD HIDE"),
      disabledCinemas = Set("OLD CINEMA"),
      updatedAt       = specClock.instant()
    )
    val (ctl, repository, _) = fixture(Some(initial))
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("hiddenFilms" -> Json.arr("New Hide")))

    status(ctl.put()(request)) shouldBe OK
    val stored = repository.find("u1").value
    stored.hiddenFilms     shouldBe Set("New Hide")    // present → replaced
    stored.disabledCinemas shouldBe Set("OLD CINEMA")  // absent  → preserved
  }

  it should "still clear a field when the body sends it as an explicit empty array" in {
    val initial = UserState("u1", Set("H"), Set("C"), specClock.instant())
    val (ctl, repository, _) = fixture(Some(initial))
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("disabledCinemas" -> Json.arr()))
    status(ctl.put()(request)) shouldBe OK
    repository.find("u1").value.disabledCinemas shouldBe empty    // present-but-empty → cleared
    repository.find("u1").value.hiddenFilms     shouldBe Set("H") // absent → preserved
  }

  it should "set a language pick" in {
    val (ctl, repository, _) = fixture()
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("language" -> "es"))
    status(ctl.put()(request)) shouldBe OK
    repository.find("u1").value.language shouldBe Some("es")
  }

  it should "preserve a stored language the body omits, same as the sets" in {
    val initial = UserState("u1", Set.empty, Set.empty, specClock.instant(), language = Some("de"))
    val (ctl, repository, _) = fixture(Some(initial))
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("hiddenFilms" -> Json.arr("X")))
    status(ctl.put()(request)) shouldBe OK
    repository.find("u1").value.language shouldBe Some("de")
  }

  it should "clear a stored language when the body sends it as explicit null" in {
    val initial = UserState("u1", Set.empty, Set.empty, specClock.instant(), language = Some("de"))
    val (ctl, repository, _) = fixture(Some(initial))
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("language" -> JsNull))
    status(ctl.put()(request)) shouldBe OK
    repository.find("u1").value.language shouldBe None
  }

  it should "400 an unsupported language code and not touch storage" in {
    val (ctl, repository, _) = fixture()
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("language" -> "fr"))
    val result = ctl.put()(request)
    status(result)                                     shouldBe BAD_REQUEST
    (contentAsJson(result) \ "error").as[String] should include ("language")
    repository.find("u1")                              shouldBe empty
  }

  it should "echo the saved state in the response so the client confirms what landed" in {
    val (ctl, _, _) = fixture()
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("hiddenFilms" -> Json.arr("A")))
    val result = ctl.put()(request)

    (contentAsJson(result) \ "hiddenFilms").as[Seq[String]] shouldBe Seq("A")
  }

  it should "400 a malformed payload (wrong type) and not touch storage" in {
    val (ctl, repository, _) = fixture()
    val request = FakeRequest("PUT", "/api/me/state")
      .withSession("userId" -> "u1")
      .withBody(Json.obj("hiddenFilms" -> "not-an-array"))
    val result = ctl.put()(request)
    status(result)               shouldBe BAD_REQUEST
    (contentAsJson(result) \ "error").as[String] should include ("hiddenFilms")
    repository.find("u1")              shouldBe empty
  }

  // ── DELETE /api/me ──────────────────────────────────────────────────────

  "DELETE /api/me" should "401 anonymous requests" in {
    val (ctl, _, _) = fixture()
    status(ctl.deleteAccount()(FakeRequest("DELETE", "/api/me"))) shouldBe UNAUTHORIZED
  }

  it should "remove the user + state rows AND clear the session" in {
    val initialState = UserState("u1", Set("Conclave"), Set.empty, specClock.instant())
    // `fixture` already seeds "u1" (needed for the request itself to pass
    // `SignedInUser`) — this just confirms deletion actually removes it.
    val (ctl, stateRepository, userRepository) = fixture(Some(initialState))

    val request = FakeRequest("DELETE", "/api/me").withSession("userId" -> "u1", "extra" -> "leftover")
    val result  = ctl.deleteAccount()(request)

    status(result)               shouldBe NO_CONTENT
    stateRepository.find("u1")         shouldBe empty
    userRepository.findById("u1")      shouldBe empty
    val sess = session(result)
    sess.get("userId")           shouldBe empty
    sess.get("extra")            shouldBe empty
  }

  // ── Pure helpers (also covered indirectly by the action specs above) ────

  "UserStateController.fromJson" should "keep base fields the body omits and overwrite the ones it sends" in {
    val base = UserState("u1", Set("H"), Set("D"), specClock.instant())
    UserStateController.fromJson(Json.obj("hiddenFilms" -> Json.arr("H2"))).map(_.applyTo(base)) match {
      case Right(s) =>
        s.hiddenFilms     shouldBe Set("H2")  // present → overwritten
        s.disabledCinemas shouldBe Set("D")   // absent  → preserved
      case Left(reason) => fail(s"expected Right, got Left($reason)")
    }
  }

  // `fromJson` has no field for hiddenFilmsByCountry at all — this legacy body can't touch it in
  // either direction. Were the patch ever to build a fresh `UserState` instead of copying the
  // stored one, the constructor's `Map.empty` default would silently wipe every per-country hide.
  it should "never wipe hiddenFilmsByCountry — a field this legacy body can't even express" in {
    val base = UserState("u1", Set("H"), Set("D"), specClock.instant(), Map("pl" -> Set("Kept Across A Legacy PUT")))
    UserStateController.fromJson(Json.obj("hiddenFilms" -> Json.arr("H2"))).map(_.applyTo(base)) match {
      case Right(s)      => s.hiddenFilmsByCountry shouldBe base.hiddenFilmsByCountry
      case Left(reason)  => fail(s"expected Right, got Left($reason)")
    }
  }
}
