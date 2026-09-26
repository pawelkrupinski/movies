package tools

import settings.*
import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoAddress
import testsupport.RepoRoot

import java.nio.file.{Files, Path}
import scala.concurrent.duration.*

/** The typed values a `main` hands down, resolved from an `Env` — driven here over maps, so
 *  nothing in this spec reads (or writes) the real process. */
class ProcessConfigurationSpec extends AnyFlatSpec with Matchers {

  private def resolvedFrom(vars: (String, String)*) = new ProcessConfiguration(Env.of(vars*))

  "ProcessConfiguration" should "resolve the serving country and the Mongo address" in {
    val resolved = resolvedFrom("KINOWO_COUNTRY" -> "uk", "MONGODB_URI" -> "mongodb://probe:1", "MONGODB_DB" -> "kinowo_probe")
    resolved.country      shouldBe Country.UnitedKingdom
    resolved.mongoAddress shouldBe MongoAddress(Some(MongoUri("mongodb://probe:1")), Some(MongoDatabaseName("kinowo_probe")))
    resolvedFrom().country shouldBe Country.default
    resolvedFrom().mongoAddress shouldBe MongoAddress.Disabled
  }

  it should "resolve a worker's countries, skipping and naming unknown codes" in {
    val resolved = resolvedFrom("KINOWO_COUNTRIES" -> "uk, xx ,de")
    resolved.workerCountries shouldBe WorkerCountries(Seq(Country.UnitedKingdom, Country.Germany))
    resolved.unknownCountryCodes shouldBe Seq(UnknownCountryCode("xx"))
    resolvedFrom("KINOWO_COUNTRIES" -> "xx").workerCountries shouldBe WorkerCountries(Seq(Country.default))
  }

  it should "resolve the identity cutover countries, empty unless named" in {
    resolvedFrom().identityCutover shouldBe IdentityCutoverCountries(Set.empty)
    resolvedFrom().identityCutover.covers(Country.Spain) shouldBe false
    val resolved = resolvedFrom("KINOWO_IDENTITY_CUTOVER" -> "es, xx ,de")
    resolved.identityCutover shouldBe IdentityCutoverCountries(Set(Country.Spain, Country.Germany))
    resolved.identityCutover.covers(Country.Spain) shouldBe true
    resolved.identityCutover.covers(Country.Poland) shouldBe false
    resolvedFrom("KINOWO_IDENTITY_PROJECTION_SECONDS" -> "90").identityProjectionInterval(IdentityProjectionInterval(5.minutes)) shouldBe
      IdentityProjectionInterval(90.seconds)
  }

  it should "resolve the commit and the port, with their defaults" in {
    resolvedFrom("COMMIT_SHA" -> "abc123").commit shouldBe CommitSha("abc123")
    resolvedFrom("PORT" -> "9123").healthPort(HealthPort(9000)) shouldBe HealthPort(9123)
    resolvedFrom().commit shouldBe CommitSha("unknown")
    resolvedFrom("PORT" -> "not-a-port").healthPort(HealthPort(9000)) shouldBe HealthPort(9000)
  }

  it should "map APP_MODE onto the application modes, refusing one it does not know" in {
    resolvedFrom("APP_MODE" -> "prod").applicationMode shouldBe Some(ApplicationMode.Production)
    resolvedFrom("APP_MODE" -> "Development").applicationMode shouldBe Some(ApplicationMode.Development)
    resolvedFrom().applicationMode shouldBe None
    an[IllegalArgumentException] should be thrownBy resolvedFrom("APP_MODE" -> "staging").applicationMode
  }

  it should "split PATH into the directories an executable is looked for in" in {
    val separator = java.io.File.pathSeparator
    resolvedFrom("PATH" -> s"/opt/homebrew/bin$separator$separator/usr/bin").executableSearchPath shouldBe
      ExecutableSearchPath(Seq(Path.of("/opt/homebrew/bin"), Path.of("/usr/bin")))
    resolvedFrom().executableSearchPath shouldBe ExecutableSearchPath(Nil)
  }

  it should "give each credential its own type, absent when unset" in {
    val resolved = resolvedFrom("TMDB_API_KEY" -> "tmdb", "OMDB_API_KEY" -> "omdb", "GOOGLE_CLIENT_ID" -> "google")
    resolved.tmdbApiKey shouldBe Some(TmdbApiKey("tmdb"))
    resolved.omdbApiKey shouldBe Some(OmdbApiKey("omdb"))
    resolved.googleClientId shouldBe Some(GoogleClientId("google"))
    resolved.zyteApiKey shouldBe None
    resolvedFrom("ZYTE_API_KEY" -> "  ").zyteApiKey shouldBe None
  }

  it should "parse the admin allowlist and the scrape cities" in {
    resolvedFrom("ADMIN_ALLOWLIST" -> "a@x.pl, b@x.pl,").adminAllowlist shouldBe AdminAllowlist(Set("a@x.pl", "b@x.pl"))
    resolvedFrom().adminAllowlist shouldBe AdminAllowlist(Set.empty)
    resolvedFrom("KINOWO_SCRAPE_CITIES" -> " Poznan , wroclaw ,").scrapeCitySlugs shouldBe Some(ScrapeCitySlugs(Set("poznan", "wroclaw")))
    resolvedFrom("KINOWO_SCRAPE_CITIES" -> " , ").scrapeCitySlugs shouldBe None
  }

  it should "route an alerter only when its token and a numeric chat are both set, naming what is missing" in {
    resolvedFrom("TELEGRAM_BOT_TOKEN" -> "bot", "KINOWO_FALLBACK_TG_CHAT_ID" -> "-100", "KINOWO_STAGING_STUCK_TG_TOPIC_ID" -> "7")
      .telegramRoute(AlertRoute.StagingStuck) shouldBe Right(TelegramRoute(TelegramBotToken("bot"), TelegramChatId(-100L), Some(TelegramTopicId(7L))))
    resolvedFrom("KINOWO_FILMWEB_DROP_TG_CHAT_ID" -> "chat").telegramRoute(AlertRoute.FilmwebDrop) shouldBe
      Left(Seq(MissingSetting("TELEGRAM_BOT_TOKEN"), MissingSetting("KINOWO_FILMWEB_DROP_TG_CHAT_ID (not a number)")))
  }

  it should "read a tuning knob, falling back to the caller's default for an unusable value" in {
    resolvedFrom("KINOWO_SCRAPE_TASKS_PER_VENUE" -> "3").scrapeTasksPerVenue(ScrapeTasksPerVenue(1)) shouldBe ScrapeTasksPerVenue(3)
    resolvedFrom("KINOWO_SCRAPE_TASKS_PER_VENUE" -> "0").scrapeTasksPerVenue(ScrapeTasksPerVenue(1)) shouldBe ScrapeTasksPerVenue(1)
    resolvedFrom("KINOWO_SETTLE_INTERVAL_SECONDS" -> "90").settleInterval(SettleInterval(5.minutes)) shouldBe SettleInterval(90.seconds)
    resolvedFrom().scrapeFreshness(ScrapeFreshness(3.hours)) shouldBe ScrapeFreshness(3.hours)
    resolvedFrom("KINOWO_OCINE_PACE_MS" -> "900").hostPace(PaceKnob.Ocine, HostPace(java.time.Duration.ofMillis(500))) shouldBe
      HostPace(java.time.Duration.ofMillis(900))
  }

  // The commit, the port and PATH are facts about the process, not knobs an admin flips —
  // they must not appear on /admin/config as if they were.
  it should "not register process facts as admin-page knobs" in {
    val resolved = resolvedFrom("COMMIT_SHA" -> "abc", "PORT" -> "1", "PATH" -> "/bin", "APP_MODE" -> "dev")
    resolved.commit; resolved.healthPort(HealthPort(9000)); resolved.executableSearchPath; resolved.applicationMode
    resolved.env.knobs shouldBe empty
  }

  /**
   * Every configuration value comes out as a type of its own, so the compiler decides what
   * may be passed where. Checked on the SOURCE, since a value class erases to its underlying
   * type in the bytecode a reflective check would read: no accessor of the resolver may be
   * declared as a bare String, number, Boolean, duration, URI or Path — alone or inside an
   * Option / Seq / Set / Either.
   */
  "Every ProcessConfiguration accessor" should "return a dedicated type, never a bare String, number, duration, URI or Path" in {
    val source    = Files.readString(RepoRoot.dir.toPath.resolve("common/src/main/scala/settings/ProcessConfiguration.scala"))
    val body      = source.substring(source.indexOf("final class ProcessConfiguration"), source.indexOf("object ProcessConfiguration"))
    val accessors = """(?m)^  def (\w+)(?:\([^)]*\))?\s*:\s*([^=]+?)\s*=""".r.findAllMatchIn(body).map(m => m.group(1) -> m.group(2).trim).toSeq
    val Bare      = """\b(String|Int|Long|Double|Float|Boolean|Duration|FiniteDuration|URI|Path)\b""".r
    withClue("accessors seen: " + accessors.map(_._1).mkString(", ") + "\n")(accessors.size should be > 60)
    val untyped   = accessors.collect { case (name, declared) if Bare.findFirstIn(declared).isDefined => s"$name: $declared" }
    withClue("give each its own `final case class X(value: …) extends AnyVal` in `configuration`: ")(untyped shouldBe empty)
  }
}
