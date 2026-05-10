package clients

import org.openqa.selenium.{By, WebDriver}
import org.openqa.selenium.chrome.{ChromeDriver, ChromeOptions}
import org.openqa.selenium.support.ui.{ExpectedConditions, WebDriverWait}
import play.api.libs.json._

import java.nio.file.{Files, Paths}
import java.time.Duration
import scala.jdk.CollectionConverters._
import scala.util.Try

final case class Session(time: String, bookingUrl: Option[String])

final case class MultikinoShowtime(
  date: String,
  movieTitle: String,
  posterUrl: Option[String],
  filmUrl: Option[String],
  times: Seq[Session]
)

object MultikinoScraper {
  private val PageUrl   = "https://www.multikino.pl/repertuar/poznan-stary-browar/teraz-gramy"
  private val ApiPath   = "/api/microservice/showings/cinemas/0011/films?minEmbargoLevel=2&includesSession=true&includeSessionAttributes=true"
  private val DumpFile  = "/tmp/multikino_api.json"

  private def dbg(msg: String): Unit = println(s"[DBG] $msg")

  def scrape(): Seq[MultikinoShowtime] = {
    val options = new ChromeOptions()
    options.addArguments(
      "--headless=new",
      "--disable-gpu",
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--window-size=1920,1080",
      "--lang=pl-PL"
    )

    dbg("Launching ChromeDriver...")
    val driver = new ChromeDriver(options)

    try {
      val wait = new WebDriverWait(driver, Duration.ofSeconds(20))

      dbg(s"Navigating to $PageUrl")
      driver.get(PageUrl)

      dbg("Looking for cookie consent...")
      acceptCookiesIfPresent(driver)

      dbg("Waiting for page to initialise (auth cookies)...")
      wait.until(ExpectedConditions.presenceOfElementLocated(By.cssSelector("body")))
      waitForRepertoire(driver)
      dbg("Page ready")

      dbg("Fetching sessions JSON via browser (authenticated)...")
      val json = fetchJsonFromBrowser(driver, ApiPath)

      Files.writeString(Paths.get(DumpFile), json)
      dbg(s"Raw JSON written to $DumpFile")

      val results = parseJson(json)
      dbg(s"Parsed ${results.size} showtime entries")
      results
    } finally {
      dbg("Quitting driver")
      driver.quit()
    }
  }

  // ── Browser fetch ──────────────────────────────────────────────────────────

  /** Uses the browser's own fetch (with session cookies) to call the API. */
  private def fetchJsonFromBrowser(driver: WebDriver, path: String): String = {
    val js = driver.asInstanceOf[org.openqa.selenium.JavascriptExecutor]
    val result = js.executeAsyncScript(
      s"""
        const callback = arguments[arguments.length - 1];
        fetch('$path', { credentials: 'include' })
          .then(r => r.text())
          .then(text => callback(text))
          .catch(e  => callback(JSON.stringify({ __error: e.toString() })));
      """
    )
    result.toString
  }

  // ── JSON parsing ───────────────────────────────────────────────────────────

  def parseJson(json: String): Seq[MultikinoShowtime] = {
    val films = (Json.parse(json) \ "result").as[JsArray].value

    dbg(s"Found ${films.size} film entries in JSON")

    films.flatMap { film =>
      val title     = (film \ "filmTitle").as[String]
      val posterUrl = (film \ "posterImageSrc").asOpt[String].filter(_.nonEmpty)
      val filmUrl   = (film \ "filmUrl").asOpt[String].filter(_.nonEmpty)
                        .map(url => if (url.startsWith("http")) url else s"https://www.multikino.pl$url")
      val groups    = (film \ "showingGroups").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)

      groups.map { group =>
        // "2026-05-11T00:00:00" → "2026-05-11"
        val date = (group \ "date").as[String].take(10)

        val sessions = (group \ "sessions").as[JsArray].value.flatMap { s =>
          // "2026-05-11T10:10:00" → "10:10"
          (s \ "startTime").asOpt[String].map { startTime =>
            val bookingUrl = (s \ "bookingUrl").asOpt[String]
                              .map(url => if (url.startsWith("http")) url else s"https://www.multikino.pl$url")
            Session(time = startTime.substring(11, 16), bookingUrl = bookingUrl)
          }
        }.toSeq

        MultikinoShowtime(date = date, movieTitle = title, posterUrl = posterUrl, filmUrl = filmUrl, times = sessions)
      }
    }.toSeq
  }

  // ── Page helpers ───────────────────────────────────────────────────────────

  private def acceptCookiesIfPresent(driver: WebDriver): Unit = {
    val xpaths = Seq(
      "//button[contains(., 'Akceptuję')]",
      "//button[contains(., 'Akceptuj')]",
      "//button[contains(., 'Zgadzam')]",
      "//button[contains(., 'Accept')]",
      "//button[contains(., 'OK')]"
    )
    xpaths.foreach { xpath =>
      Try {
        driver.findElements(By.xpath(xpath)).asScala.headOption.foreach { btn =>
          if (btn.isDisplayed && btn.isEnabled) { btn.click(); Thread.sleep(500) }
        }
      }
    }
  }

  private def waitForRepertoire(driver: WebDriver): Unit = {
    val wait = new WebDriverWait(driver, Duration.ofSeconds(20))
    wait.until { d =>
      val text = d.findElement(By.tagName("body")).getText
      text.contains("Więcej dni") || text.contains("Kup bilet") || text.contains("Repertuar")
    }
  }
}
