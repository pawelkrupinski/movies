package views

import com.sun.net.httpserver.HttpExchange
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, JsString, JsValue, Json}
import testsupport.TestMessages.given
import tools.{CdpPage, Chrome, TestHttpServer}

import java.nio.charset.StandardCharsets.UTF_8
import java.time.LocalDateTime
import scala.collection.mutable
import scala.util.Random

/**
 * Model-based test of the web client's hidden-films + language sync
 * (`shared.js`'s `bootMergeFromServer` / `_writeHiddenFilms` /
 * `reconcileLanguage`), in real Chrome: seeded random sequences of what a
 * visitor, the network and the account's other devices do, checked against
 * the sync's invariants. The same alphabet and invariants run against Android
 * (`StateSyncModelTest`) and iOS (`StateSyncModelTests`).
 *
 * THE ALPHABET: switch country (navigate to the other country's page), hide,
 * unhide, clear, login, logout, resume (a page load — the reconcile; the
 * server answers 304 or 200 by its own content validator), another device
 * hiding / unhiding a title, the network going down, the network coming back
 * (reconnect + reload), a local language pick, another device's language pick.
 *
 * THE INVARIANTS:
 *  1. No title crosses countries: a title hidden in one country never reaches
 *     another country's server bucket (checked after EVERY event), nor — once a
 *     signed-in page has reconciled — the local list shown for another country.
 *  2. Convergence: once the network is up, signed in and every country's page
 *     has reconciled, each country's local list IS its server list, and the
 *     local language IS the account's.
 *  3. No op lost: an edit made while signed in reaches the server unless
 *     something later legitimately overrides it. Edits still owed at a logout
 *     are forgotten with the session, so their titles are unconstrained; the
 *     same holds for the last language pick made while signed in.
 *
 * The two countries are `uk` and `de` because they share ONE origin in
 * production (showtimes.cc/uk, /de), and so one `localStorage` list — the case
 * the per-country marker exists for. Each sequence runs on a fresh server
 * port, so a fresh origin and an empty `localStorage`.
 *
 * A failure prints the seed, the minimised sequence and the violation; pin the
 * sequence as a regression below.
 */
class HiddenFilmsSyncModelSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  import HiddenFilmsSyncModelSpec._

  private var chrome: Option[Chrome] = None

  override def beforeAll(): Unit = chrome = Chrome.tryStart()
  override def afterAll(): Unit = chrome.foreach(_.close())

  private def withChrome(body: Chrome => Unit): Unit =
    chrome match {
      case Some(c) => body(c)
      case None    => cancel("Chrome not installed — skipping the sync model")
    }

  "the web hidden-films sync" should "keep its invariants over random event sequences" in withChrome { c =>
    Seeds.foreach { seed =>
      val events = SyncModel.generate(seed)
      SyncModel.violationOf(c, events).foreach { violation =>
        val minimal = SyncModel.minimise(c, events)
        fail(s"seed=$seed violates the sync model: ${SyncModel.violationOf(c, minimal).getOrElse("?")}\n" +
          s"minimised sequence (${minimal.size} of ${events.size} events):\n  ${minimal.mkString(",\n  ")}\n" +
          s"full-sequence violation: $violation")
      }
    }
  }

  // Pinned shapes of the historical bugs, so they run whatever the seeds
  // generate. A 304 answering for a list that mirrors ANOTHER country
  // (c37ad26e0, the per-country marker): the de list stayed on the uk page.
  it should "not keep another country's list on screen after a 304" in withChrome { c =>
    SyncModel.violationOf(c, Seq(Login, Hide, SwitchCountry("de"), Hide, SwitchCountry("uk"))) shouldBe None
  }

  // Found by this model: a page loaded offline could not ask `/api/me`, took
  // itself for signed out, dropped every write the device still owed the
  // account and queued none of the edits made on it — so the next signed-in
  // load's union resurrected what had been cleared.
  it should "keep what a page that could not confirm the session owes the account" in withChrome { c =>
    SyncModel.violationOf(c, Seq(Login, Hide, NetworkDown, Resume, Clear)) shouldBe None
  }

  // Found by this model: a first sync's union ignored the writes still owed —
  // here, made offline on a country never synced this session — so the union
  // brought back what a queued clear removed, and the clear's late replay then
  // wiped the hide queued after it.
  it should "play owed writes over a first sync's union, in order" in withChrome { c =>
    SyncModel.violationOf(c, Seq(Login, RemoteHide("de"), NetworkDown, SwitchCountry("de"), Clear, Hide, Reconnect)) shouldBe None
  }

  // Found by this model: a language pick whose push failed was never retried,
  // and the next load adopted the account's older pick over it.
  it should "push a language pick made offline instead of adopting the account's older one" in withChrome { c =>
    SyncModel.violationOf(c, Seq(Login, NetworkDown, RemoteLanguage("en"), PickLanguage("es"))) shouldBe None
  }

  // Found by this model: an anonymous visitor's /uk hide, still in the one
  // per-origin list on /de, was unioned into the /de account at a sign-in there.
  it should "not union one country's anonymous hides into another country's account" in withChrome { c =>
    SyncModel.violationOf(c, Seq(Hide, SwitchCountry("de"))) shouldBe None
  }

  // Found by this model while fixing the one above: a pick made offline by a
  // visitor who was NOT signed in was owed to the account they signed into
  // next, and pushed over its newer pick.
  it should "not push an offline signed-out visitor's pick into the account they sign into" in withChrome { c =>
    SyncModel.violationOf(c, Seq(NetworkDown, SwitchCountry("de"), PickLanguage("de"), RemoteLanguage("es"))) shouldBe None
  }

  it should "resend a write that failed offline" in withChrome { c =>
    SyncModel.violationOf(c, Seq(Login, Hide, NetworkDown, Unhide(0), Reconnect)) shouldBe None
  }
}

object HiddenFilmsSyncModelSpec {

  /** Each sequence costs a few seconds of real page loads. */
  private val Seeds  = 1L to 12L
  private val Length = 20

  /** Two countries that share one origin in production. */
  private val Countries = Seq("uk", "de")
  private val Languages = Seq("en", "de", "pl", "es")

  sealed trait SyncEvent
  final case class SwitchCountry(country: String)           extends SyncEvent
  case object Hide                                           extends SyncEvent
  final case class Unhide(pick: Int)                         extends SyncEvent
  case object Clear                                          extends SyncEvent
  case object Login                                          extends SyncEvent
  case object Logout                                         extends SyncEvent
  case object Resume                                         extends SyncEvent
  final case class RemoteHide(country: String)               extends SyncEvent
  final case class RemoteUnhide(country: String, pick: Int)  extends SyncEvent
  case object NetworkDown                                    extends SyncEvent
  case object Reconnect                                      extends SyncEvent
  final case class PickLanguage(language: String)            extends SyncEvent
  final case class RemoteLanguage(language: String)          extends SyncEvent

  object SyncModel {
    def generate(seed: Long): Seq[SyncEvent] = {
      val random = new Random(seed)
      def country() = Countries(random.nextInt(Countries.size))
      Seq.fill(Length) {
        random.nextInt(100) match {
          case n if n < 20 => Hide
          case n if n < 32 => Unhide(random.nextInt(8))
          case n if n < 36 => Clear
          case n if n < 46 => SwitchCountry(country())
          case n if n < 53 => Login
          case n if n < 58 => Logout
          case n if n < 66 => Resume
          case n if n < 71 => RemoteHide(country())
          case n if n < 74 => RemoteUnhide(country(), random.nextInt(8))
          case n if n < 81 => NetworkDown
          case n if n < 89 => Reconnect
          case n if n < 96 => PickLanguage(Languages(random.nextInt(Languages.size)))
          case _           => RemoteLanguage(Languages(random.nextInt(Languages.size)))
        }
      }
    }

    /** Greedily drop events while the sequence still violates the model. */
    def minimise(chrome: Chrome, events: Seq[SyncEvent]): Seq[SyncEvent] = {
      var current = events
      var shrunk  = true
      while (shrunk) {
        shrunk = false
        current.indices.reverseIterator.find { i =>
          violationOf(chrome, current.patch(i, Nil, 1)).isDefined
        }.foreach { i => current = current.patch(i, Nil, 1); shrunk = true }
      }
      current
    }

    /** Run `events` then settle, in a fresh tab on a fresh origin; the first
     *  invariant broken, or None. */
    def violationOf(chrome: Chrome, events: Seq[SyncEvent]): Option[String] = {
      val account = new ModelAccountServer
      val server  = new TestHttpServer(Pages.routes, dynamicRoute = account.route)
      try chrome.openPage(server.baseUrl + Pages.path(Countries.head)) { page =>
        new Run(page, account, server.baseUrl).play(events)
      } finally server.close()
    }
  }

  /** The two countries' listing pages, empty (the sync needs no films), with a
   *  sign-in provider so the page asks `/api/me` who is looking. */
  private object Pages {
    private val cities = Map(
      "uk" -> models.Country.UnitedKingdom.cities.head,
      "de" -> models.Country.Germany.cities.head)

    def path(country: String): String = s"/${cities(country).slug}/"

    private lazy val html: Map[String, String] = cities.map { case (country, city) =>
      country -> views.html.repertoire(Seq.empty, Seq.empty, Map.empty, devMode = false,
        oauthProviders = Set("google"), renderedAt = LocalDateTime.of(2026, 6, 8, 0, 0))(using city, summon[play.api.i18n.Messages]).body
    }

    val routes: PartialFunction[String, String] = {
      case p if cities.exists { case (_, city) => p == s"/${city.slug}/" } =>
        html(cities.collectFirst { case (country, city) if p == s"/${city.slug}/" => country }.get)
    }
  }

  /** The account behind `/api/me`: per-country hidden-films buckets answering
   *  like `UserStateController` (content ETag, 304 on a match, every write
   *  echoing the set), the legacy `/api/me/state` language, a session that is
   *  signed in or not, and a network that is up or not. */
  final class ModelAccountServer {
    private val buckets = mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty)
    private var language: Option[String] = None
    @volatile var signedIn = false
    @volatile var offline  = false

    def bucket(country: String): Set[String] = synchronized(buckets(country))
    def update(country: String)(f: Set[String] => Set[String]): Unit = synchronized(buckets(country) = f(buckets(country)))
    def accountLanguage: Option[String] = synchronized(language)
    def setAccountLanguage(l: String): Unit = synchronized { language = Some(l) }

    private def etag(country: String): String =
      "\"" + Integer.toHexString(buckets(country).toSeq.sorted.mkString("|").hashCode) + "\""

    def route(exchange: HttpExchange): Boolean = synchronized {
      val path   = exchange.getRequestURI.getPath
      val method = exchange.getRequestMethod
      val Bucket = "/api/me/([a-z]+)/hidden-films(?:/(.+))?".r
      def reply(status: Int, body: String = "", headers: Seq[(String, String)] = Nil): Boolean = {
        headers.foreach { case (k, v) => exchange.getResponseHeaders.add(k, v) }
        exchange.getResponseHeaders.add("Content-Type", "application/json; charset=UTF-8")
        exchange.getResponseHeaders.add("Cache-Control", "no-store")
        val bytes = body.getBytes(UTF_8)
        if (status == 304 || bytes.isEmpty) exchange.sendResponseHeaders(status, -1)
        else {
          exchange.sendResponseHeaders(status, bytes.length.toLong)
          val os = exchange.getResponseBody
          try os.write(bytes) finally os.close()
        }
        true
      }
      def films(country: String): Boolean =
        reply(200, Json.stringify(Json.obj("hiddenFilms" -> buckets(country).toSeq.sorted)),
          Seq("ETag" -> etag(country)))

      if (!path.startsWith("/api/me")) false
      else if (offline) reply(503)
      else if (!signedIn) reply(401, """{"error":"not logged in"}""")
      else (method, path) match {
        case ("GET", "/api/me") =>
          reply(200, """{"displayName":"Model","email":"model@example.com","avatarUrl":null,"provider":"google"}""")
        case ("GET", "/api/me/state") =>
          reply(200, Json.stringify(Json.obj("hiddenFilms" -> JsArray(), "disabledCinemas" -> JsArray(),
            "language" -> language)))
        case ("PUT", "/api/me/state") =>
          val body = Json.parse(new String(exchange.getRequestBody.readAllBytes(), UTF_8))
          (body \ "language").asOpt[String].foreach(l => language = Some(l))
          reply(200, Json.stringify(Json.obj("language" -> language)))
        case ("GET", Bucket(country, null)) =>
          if (Option(exchange.getRequestHeaders.getFirst("If-None-Match")).contains(etag(country)))
            reply(304, headers = Seq("ETag" -> etag(country)))
          else films(country)
        case ("PUT", Bucket(country, title)) if title != null    => buckets(country) = buckets(country) + title; films(country)
        case ("DELETE", Bucket(country, title)) if title != null => buckets(country) = buckets(country) - title; films(country)
        case ("DELETE", Bucket(country, null))                   => buckets(country) = Set.empty; films(country)
        case _                                                   => reply(404)
      }
    }
  }

  /** One sequence against one tab. */
  private final class Run(page: CdpPage, account: ModelAccountServer, base: String) {
    private var country  = Countries.head
    private var signedIn = false
    private var minted   = 0
    private val mustHave    = mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty)
    private val mustNotHave = mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty)
    private var expectedLanguage: Option[String] = None

    // Count this page's own requests from the moment each document starts, so
    // "nothing in flight" can be asked of the page itself.
    page.send("Page.addScriptToEvaluateOnNewDocument", Json.obj("source" ->
      """(function () {
        |  window.__inflight = 0;
        |  var original = window.fetch.bind(window);
        |  var done = function () { setTimeout(function () { window.__inflight--; }, 0); };
        |  window.fetch = function () {
        |    window.__inflight++;
        |    var p;
        |    try { p = original.apply(null, arguments); } catch (e) { done(); throw e; }
        |    p.then(done, done);
        |    return p;
        |  };
        |})();""".stripMargin))

    private def load(): Unit = { page.navigate(base + Pages.path(country)); quiesce() }

    /** Nothing in flight and no debounced language push armed, held for a
     *  while — the page's reconcile chains its requests. */
    private def quiesce(): Unit = {
      val idle = "document.readyState === 'complete' && window.__inflight === 0 && " +
        "(typeof _serverSyncTimer === 'undefined' || _serverSyncTimer === 0)"
      val deadline = System.currentTimeMillis() + 10000
      var stable = 0
      while (stable < 6) {
        if (System.currentTimeMillis() > deadline) throw new RuntimeException("the page never went quiet")
        stable = if (page.evalBool(s"!!($idle)")) stable + 1 else 0
        Thread.sleep(25)
      }
    }

    private def local: Seq[String] = Json.parse(page.evalString("JSON.stringify(getHidden())")).as[Seq[String]].sorted

    private def pending(c: String): Seq[(String, Option[String])] =
      Json.parse(page.evalString(s"JSON.stringify(_pendingHiddenFilms('$c'))")).as[Seq[Seq[JsValue]]]
        .map(op => op.head.as[String] -> op(1).asOpt[String])

    def play(events: Seq[SyncEvent]): Option[String] = {
      load()
      events.zipWithIndex.foreach { case (event, index) =>
        apply(event)
        quiesce()
        isolationViolation(event).foreach(v => return Some(s"after event #$index $event: $v"))
      }
      settle()
    }

    private def apply(event: SyncEvent): Unit = event match {
      case SwitchCountry(c) => country = c; load()
      case Hide =>
        minted += 1
        val title = s"$country-$minted"
        // The card button's own handler, on a card carrying the title.
        page.eval(s"""(function (t) { var card = document.createElement('div'); card.setAttribute('data-title', t);
                     |  var b = document.createElement('button'); card.appendChild(b); document.body.appendChild(card);
                     |  hideFilm(b); })(${JsString(title)})""".stripMargin)
        if (signedIn) expect(country, title, hidden = true)
      case Unhide(pick) =>
        val list = local
        if (list.nonEmpty) {
          val title = list(pick % list.size)
          page.eval(s"restoreFilm(${JsString(title)})")
          if (signedIn) expect(country, title, hidden = false)
        }
      case Clear =>
        val list = local
        page.eval("showAllFilms()")
        if (signedIn) {
          mustNotHave(country) = mustNotHave(country) ++ list ++ mustHave(country)
          mustHave(country) = Set.empty
        }
      // Signing in or out is a round trip to the server on the web (the OAuth
      // redirect chain, the sign-out POST): offline, neither happens.
      case Login | Logout if account.offline => ()
      case Login =>
        account.signedIn = true; signedIn = true; load()
      case Logout =>
        // What the account is still owed is forgotten with the session.
        if (signedIn) {
          Countries.foreach { c =>
            pending(c).foreach {
              case (_, Some(title)) => unconstrain(c, title)
              case (_, None)        => mustHave(c) = Set.empty; mustNotHave(c) = Set.empty
            }
          }
          if (expectedLanguage.exists(l => !account.accountLanguage.contains(l))) expectedLanguage = None
        }
        account.signedIn = false; signedIn = false; load()
      case Resume => load()
      case RemoteHide(c) =>
        minted += 1
        val title = s"$c-r$minted"
        account.update(c)(_ + title)
        // A clear this device still owes the account lands after it.
        if (!pending(c).exists(_._2.isEmpty)) expect(c, title, hidden = true)
      case RemoteUnhide(c, pick) =>
        val remote = account.bucket(c).toSeq.sorted
        if (remote.nonEmpty) {
          val title = remote(pick % remote.size)
          account.update(c)(_ - title)
          // A first sync's union may legitimately bring it back.
          unconstrain(c, title)
        }
      case NetworkDown => account.offline = true
      case Reconnect   => account.offline = false; load()
      case PickLanguage(l) =>
        // Picking the language already on screen changes nothing.
        if (page.evalString("localStorage.getItem('kinowo_lang') || document.documentElement.lang") != l) {
          page.eval(s"onLanguageChange(${JsString(l)})")
          // Signed out, the account's own pick wins at the next login.
          expectedLanguage = if (signedIn) Some(l) else None
        }
      case RemoteLanguage(l) =>
        // A pick this device still owes the account is newer.
        val owed = expectedLanguage.exists(p => !account.accountLanguage.contains(p))
        account.setAccountLanguage(l)
        expectedLanguage = if (owed) None else Some(l)
    }

    private def expect(c: String, title: String, hidden: Boolean): Unit =
      if (hidden) { mustHave(c) = mustHave(c) + title; mustNotHave(c) = mustNotHave(c) - title }
      else { mustNotHave(c) = mustNotHave(c) + title; mustHave(c) = mustHave(c) - title }

    private def unconstrain(c: String, title: String): Unit = {
      mustHave(c) = mustHave(c) - title
      mustNotHave(c) = mustNotHave(c) - title
    }

    private def foreign(c: String, titles: Iterable[String]): Seq[String] =
      titles.filterNot(_.startsWith(s"$c-")).toSeq.sorted

    /** Every event: no server bucket holds another country's title. After a page
     *  load that reconciled signed in, the list on screen is this country's. */
    private def isolationViolation(event: SyncEvent): Option[String] = {
      Countries.collectFirst {
        case c if foreign(c, account.bucket(c)).nonEmpty =>
          s"server bucket '$c' holds another country's titles ${foreign(c, account.bucket(c))}"
      }.orElse {
        val reconciled = event match {
          case SwitchCountry(_) | Login | Resume | Reconnect => signedIn && !account.offline
          case _                                             => false
        }
        Option.when(reconciled && foreign(country, local).nonEmpty)(
          s"the signed-in '$country' page shows another country's titles ${foreign(country, local)}")
      }
    }

    /** Network up, signed in, every country's page reconciled — twice, so a
     *  first sync's union pushes have landed — then compare, page by page. */
    private def settle(): Option[String] = {
      account.offline = false
      load()
      if (!signedIn) apply(Login)
      (1 to 2).foreach(_ => Countries.foreach(c => apply(SwitchCountry(c))))
      Countries.foreach { c =>
        apply(SwitchCountry(c))
        val list   = local.toSet
        val remote = account.bucket(c)
        val v =
          if (foreign(c, list).nonEmpty) Some(s"after settling, the '$c' page shows another country's titles ${foreign(c, list)}")
          else if (foreign(c, remote).nonEmpty) Some(s"after settling, server bucket '$c' holds ${foreign(c, remote)}")
          else if (list != remote) Some(s"after settling, '$c' local ${list.toSeq.sorted} != server ${remote.toSeq.sorted}")
          else if ((mustHave(c) -- remote).nonEmpty) Some(s"after settling, '$c' lost hides ${(mustHave(c) -- remote).toSeq.sorted}")
          else if ((mustNotHave(c) intersect remote).nonEmpty)
            Some(s"after settling, '$c' resurrected unhidden ${(mustNotHave(c) intersect remote).toSeq.sorted}")
          else None
        if (v.isDefined) return v
      }
      val localLanguage = page.eval("localStorage.getItem('kinowo_lang')").asOpt[String]
      account.accountLanguage.filterNot(localLanguage.contains).map(a =>
        s"after settling, local language $localLanguage != account's $a")
        .orElse(expectedLanguage.filterNot(account.accountLanguage.contains).map(e =>
          s"after settling, the account's language is ${account.accountLanguage}, expected the last pick $e"))
    }
  }
}
