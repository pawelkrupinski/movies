package views

import models.Poznan
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.FixtureTestWiring

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime

/**
 * Whole-page HTML snapshot regression. Renders the two user-facing pages
 * (`/`, `/kina`) against the recorded `17-05-2026` fixture corpus and
 * diffs each rendered body against a checked-in expected file.
 */
class PageSnapshotSpec extends AnyFlatSpec with Matchers {

  private val now = LocalDateTime.of(2026, 5, 17, 0, 0)

  private implicit val city: models.City = Poznan

  private val anonymousUser    = Option.empty[models.User]
  private val noOauthProviders = Set.empty[String]

  private val snapshotDir = Paths.get("test/resources/fixtures/17-05-2026")

  "the / page (repertoire view)" should "render the same HTML as the checked-in snapshot" in
    new FixtureTestWiring("17-05-2026") {
      bootStartup()
      val html: String = views.html.repertoire(
        movieControllerService.toSchedules(city, now),
        city.cinemaDisplayNames,
        city.cinemaPillMap,
        devMode = false,
        currentUser = anonymousUser,
        oauthProviders = noOauthProviders
      ).body
      assertSnapshot(snapshotDir.resolve("expected-index.html"), html)
    }

  "the /kina page" should "render the same HTML as the checked-in snapshot" in
    new FixtureTestWiring("17-05-2026") {
      bootStartup()
      val html: String = views.html.kina(
        movieControllerService.toCinemaSchedules(city, now),
        city.cinemaDisplayNames,
        city.cinemaPillMap,
        devMode = false,
        currentUser = anonymousUser,
        oauthProviders = noOauthProviders
      ).body
      assertSnapshot(snapshotDir.resolve("expected-kina.html"), html)
    }

  "the /kina/:cinema page" should "seed _kinaPinned from the URL-path cinema label" in
    new FixtureTestWiring("17-05-2026") {
      bootStartup()
      val html: String = views.html.kina(
        movieControllerService.toCinemaSchedules(city, now),
        city.cinemaDisplayNames,
        city.cinemaPillMap,
        devMode = false,
        currentUser = anonymousUser,
        oauthProviders = noOauthProviders,
        pinnedCinema = Some("Kino Apollo")
      ).body
      html should include ("""window._kinaPinned = "Kino Apollo";""")
    }

  // Multi-city coverage: the same corpus now carries Wrocław + Warszawa
  // cinemas, so each city's index renders its own scoped repertoire. Pins that
  // city scoping produces a stable, city-specific page (not Poznań's).
  "the Wrocław index" should "render the same HTML as the checked-in snapshot" in
    new FixtureTestWiring("17-05-2026") {
      bootStartup()
      val html: String = views.html.repertoire(
        movieControllerService.toSchedules(models.Wroclaw, now),
        models.Wroclaw.cinemaDisplayNames,
        models.Wroclaw.cinemaPillMap,
        devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders
      )(models.Wroclaw).body
      assertSnapshot(snapshotDir.resolve("expected-wroclaw-index.html"), html)
    }

  "the Warszawa index" should "render the same HTML as the checked-in snapshot" in
    new FixtureTestWiring("17-05-2026") {
      bootStartup()
      val html: String = views.html.repertoire(
        movieControllerService.toSchedules(models.Warszawa, now),
        models.Warszawa.cinemaDisplayNames,
        models.Warszawa.cinemaPillMap,
        devMode = false, currentUser = anonymousUser, oauthProviders = noOauthProviders
      )(models.Warszawa).body
      assertSnapshot(snapshotDir.resolve("expected-warszawa-index.html"), html)
    }

  "the /plan page" should "render the same HTML as the checked-in snapshot" in
    new FixtureTestWiring("17-05-2026") {
      bootStartup()
      val data = controllers.PlanController.viewData(city, movieControllerService.toSchedules(city, now))
      val html: String = views.html.plan(
        data,
        city.cinemaDisplayNames,
        city.cinemaPillMap,
        devMode = false,
        currentUser = anonymousUser,
        oauthProviders = noOauthProviders
      ).body
      assertSnapshot(snapshotDir.resolve("expected-plan.html"), html)
    }

  private def assertSnapshot(expectedPath: Path, actual: String): Unit = {
    if (!Files.exists(expectedPath)) {
      Files.write(expectedPath, actual.getBytes(StandardCharsets.UTF_8))
      fail(s"Snapshot didn't exist — wrote $expectedPath. Review the contents, commit, and re-run.")
    }
    val expected = new String(Files.readAllBytes(expectedPath), StandardCharsets.UTF_8)
    withClue(
      s"Page HTML snapshot mismatch at $expectedPath. To regenerate after an intentional change:\n" +
        s"  rm $expectedPath && sbt 'testOnly views.PageSnapshotSpec'\n"
    ) {
      actual shouldBe expected
    }
  }
}
