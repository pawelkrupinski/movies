package tools

import models.Country
import play.api.libs.json.Json

import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.nio.file.{Files, Paths}
import java.util.Base64
import javax.imageio.ImageIO

/**
 * Regenerates the per-city Open Graph share cards under
 * `web/src/main/assets/img/og-{slug}.png` — the 1200×630 previews
 * Facebook / Messenger / X / Slack render when a `/{slug}/` link is shared.
 *
 * Each card is the REAL repertoire page (a live desktop screenshot, with all
 * dates shown so the poster grid is full even at night) under a left-side dark
 * gradient carrying the deployment's wordmark (Kinowo / Showtimes) and the
 * IMDb·Metacritic·RT(·Filmweb) pills — the same look as the `/` landing card.
 * Everything language- and brand-specific is read from [[Country.fromEnv]]
 * (`KINOWO_COUNTRY`): the brand, the display host, the tagline language, and
 * whether the Filmweb pill shows (Poland only).
 *
 * Pipeline per card (one headless Chrome over CDP, a fresh tab each step):
 *   1. open the live `/{slug}/`, run `pickDay('anytime')`, screenshot the
 *      desktop viewport at 2× → the background.
 *   2. open a card HTML (the screenshot embedded as a data: URI + the
 *      overlay) at 3× → the composed card.
 *   3. downscale 3×→1× with bicubic supersampling (smooth text) → write PNG.
 *
 * Run (from the repo root):
 *   sbt 'web/PageTest/runMain tools.OgCardGenerator'                  # every city card
 *   sbt 'web/PageTest/runMain tools.OgCardGenerator poznan wroclaw'   # a subset of cities
 *   sbt 'web/PageTest/runMain tools.OgCardGenerator home'             # the `/` landing card
 *
 * The `home` target renders this country's landing montage — `og-home.png` for
 * Poland, `og-home-{code}.png` elsewhere (e.g. an English `og-home-uk.png`
 * screenshot off showtimes-uk.fly.dev). It screenshots the country's primary
 * city (its first [[Country.cities]], or `KINOWO_OG_HOME_CITY`).
 *
 * Env: `KINOWO_COUNTRY` (default `pl`) — which country's brand/language/host to
 * render; `KINOWO_OG_BASE` (default: that country's own host) — the site to
 * screenshot; `KINOWO_OG_OUT` (default `web/src/main/assets/img`) — output dir;
 * `KINOWO_OG_HOME_CITY` — override the city screenshotted for the `home` card.
 * Chrome is located via `Chrome.findExecutable()` (same `CDP_BROWSER_BIN`
 * override the page tests use). Each card takes well under a minute.
 */
object OgCardGenerator {

  private val Width  = 1200
  private val Height = 630
  private val Scale  = 3 // render the card at 3× then supersample down for smooth text

  def main(args: Array[String]): Unit = {
    val country = Country.fromEnv
    val baseUrl = sys.env.getOrElse("KINOWO_OG_BASE", country.ogOrigin).stripSuffix("/")
    val outDir  = Paths.get(sys.env.getOrElse("KINOWO_OG_OUT", "web/src/main/assets/img"))
    Files.createDirectories(outDir)

    // `home` renders the `/` landing montage; any other args are city slugs.
    val homeMode = args.sameElements(Array("home"))
    val only     = args.toSet
    val cities   = if (homeMode) Nil else country.cities.filter(c => only.isEmpty || only(c.slug))
    if (!homeMode && cities.isEmpty) { System.err.println(s"No ${country.code} cities matched ${only.mkString(", ")}"); sys.exit(1) }

    val chrome = Chrome.tryStart().getOrElse {
      System.err.println("No Chrome/Chromium found (set CDP_BROWSER_BIN). Aborting.")
      sys.exit(1)
    }

    val startedAt = System.currentTimeMillis()
    var ok = 0
    try {
      if (homeMode) {
        val city = sys.env.get("KINOWO_OG_HOME_CITY").flatMap(s => country.bySlug.get(s))
          .orElse(country.cities.headOption)
          .getOrElse { System.err.println(s"Country ${country.code} has no cities to screenshot for the home card."); sys.exit(1) }
        if (writeCard(chrome, country, s"$baseUrl/${city.slug}/", homeTagline(country), outDir.resolve(country.homeOgImage), "home")) ok += 1
      } else {
        cities.foreach { city =>
          if (writeCard(chrome, country, s"$baseUrl/${city.slug}/", s"Repertuar kin ${city.locativePhrase}", outDir.resolve(s"og-${city.slug}.png"), city.slug)) ok += 1
        }
      }
    } finally chrome.close()

    val total = if (homeMode) 1 else cities.size
    val secs  = (System.currentTimeMillis() - startedAt) / 1000.0
    println(f"done: $ok/$total cards in $secs%.1fs")
  }

  /** Screenshot `screenshotUrl`, compose the card for `country` with `tagline`,
   *  and write it to `out`. Retries transient load failures (the offline dino
   *  page, a momentary 5xx / rate-limit) before giving up; a genuine failure
   *  after `Attempts` tries leaves any prior card on disk untouched — never
   *  overwritten with a broken render. Returns whether a card was written. */
  private def writeCard(chrome: Chrome, country: Country, screenshotUrl: String, tagline: String, out: java.nio.file.Path, label: String): Boolean = {
    val t0 = System.currentTimeMillis()
    val Attempts = 3
    var attempt  = 0
    while (attempt < Attempts) {
      attempt += 1
      try {
        val bg   = screenshotCity(chrome, screenshotUrl)
        val card = renderCard(chrome, bg, tagline, country)
        Files.write(out, downscale(card))
        println(f"✓ $label%-20s ${(System.currentTimeMillis() - t0) / 1000.0}%4.1fs  $out")
        return true
      } catch {
        case e: Throwable =>
          if (attempt < Attempts) {
            System.err.println(s"… $label: ${e.getMessage} (attempt $attempt/$Attempts, retrying)")
            Thread.sleep(1500)
          } else System.err.println(s"✗ $label: ${e.getMessage} (gave up after $Attempts attempts)")
      }
    }
    false
  }

  /** The `/` home-card tagline for a deployment's language — the line under the
   *  wordmark on `og-home{-code}.png`. Mirrors each `messages` bundle's
   *  `landing.title` suffix; kept here because this dev tool has no Play i18n
   *  wired. */
  private def homeTagline(country: Country): String = country.language.getLanguage match {
    case "pl" => "Repertuar kin w Twoim mieście"
    case "de" => "Kinoprogramm in deiner Stadt"
    case _    => "Cinema listings in your city"
  }

  /** JS predicate: the repertoire page's inline JS ran. `pickDay` is defined
   *  on every successful render (empty repertoire or not), so it doubles as a
   *  "the site actually loaded" probe — false on Chrome's offline error page,
   *  a 5xx body, or any non-repertoire response that still reaches readyState
   *  `complete`. */
  private[tools] val RepertoireLoadedJs: String = "typeof pickDay==='function'"

  /** JS predicate: every poster `<img>` currently intersecting the viewport
   *  has either decoded (`complete` with a non-zero natural size) or been
   *  hidden by its `onerror` fallback chain (the "Brak plakatu" placeholder
   *  took over). The screenshot waits on this instead of a fixed sleep: the
   *  proxied poster `<img>`s are `loading="lazy"` and decode at variable
   *  speed, so a fixed wait raced the decode and left whichever cities were
   *  slow with blank posters on their card. Only viewport-intersecting
   *  posters matter — that's all the screenshot captures — so lazy posters
   *  below the fold are excluded (they never load and would hang the poll). */
  private[tools] val PostersReadyJs: String =
    "[].slice.call(document.querySelectorAll('img[data-original-src]'))" +
      ".filter(function(i){var r=i.getBoundingClientRect();" +
      "return r.bottom>0&&r.top<window.innerHeight&&r.right>0&&r.left<window.innerWidth;})" +
      ".every(function(i){return (i.complete&&i.naturalWidth>0)||i.offsetParent===null;})"

  /** Screenshot the live city page at desktop 2×, with every date shown so the
   *  grid is populated regardless of the hour. Returns Base64 PNG bytes.
   *
   *  Throws when the page didn't actually load (Chrome's offline error page, a
   *  5xx, prod rate-limiting): such a navigation still reaches readyState
   *  `complete`, so without this guard we'd screenshot the dino error page and
   *  emit a blank card. `pickDay` is defined inline on every repertoire render
   *  — empty repertoire or not — so its absence means the site never loaded.
   *  The caller treats the throw as a skip + retry rather than overwriting a
   *  previously-good card with garbage. */
  private def screenshotCity(chrome: Chrome, url: String): String =
    chrome.openPage(url) { page =>
      setMetrics(page, 1180, 760, 2)
      try page.waitFor(RepertoireLoadedJs, timeoutMs = 4000, pollMs = 100)
      catch { case _: Throwable => throw new RuntimeException("repertoire page did not load (no pickDay)") }
      // The date tabs are JS-only (`pickDay`); "anytime" shows all upcoming
      // days so a late-night "Dziś" empty view doesn't yield an empty card.
      try page.eval("pickDay('anytime')")
      catch { case _: Throwable => () }
      // Wait for the in-viewport posters to actually decode (see PostersReadyJs).
      // Capped so a genuinely broken poster can't hang the run — we screenshot
      // whatever decoded once the cap elapses.
      try page.waitFor(PostersReadyJs, timeoutMs = 8000, pollMs = 150)
      catch { case _: Throwable => () }
      Thread.sleep(400) // final layout + paint settle
      page.screenshot()
    }

  /** Compose the card HTML (the screenshot as a full-bleed background + the
   *  left gradient, wordmark, city line and rating pills) and screenshot it at
   *  `Scale`×. Returns Base64 PNG bytes. */
  private def renderCard(chrome: Chrome, backgroundPng: String, cityLine: String, country: Country): String = {
    val html = cardHtml(backgroundPng, cityLine, country)
    val tmp  = Files.createTempFile("og-card-", ".html")
    Files.writeString(tmp, html)
    try chrome.openPage(tmp.toUri.toString) { page =>
      setMetrics(page, Width, Height, Scale)
      Thread.sleep(250) // fonts + layout settle
      page.screenshot()
    } finally Files.deleteIfExists(tmp)
  }

  /** Bicubic supersample the `Scale`× render down to the canonical 1200×630
   *  OG size — text stays smooth (the earlier 1× direct render pixelated). */
  private def downscale(pngBase64: String): Array[Byte] = {
    val src = ImageIO.read(new ByteArrayInputStream(Base64.getDecoder.decode(pngBase64)))
    val dst = new BufferedImage(Width, Height, BufferedImage.TYPE_INT_RGB)
    val g   = dst.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
    g.setRenderingHint(RenderingHints.KEY_RENDERING,     RenderingHints.VALUE_RENDER_QUALITY)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,  RenderingHints.VALUE_ANTIALIAS_ON)
    g.drawImage(src, 0, 0, Width, Height, null)
    g.dispose()
    val out = new java.io.ByteArrayOutputStream()
    ImageIO.write(dst, "png", out)
    out.toByteArray
  }

  private def setMetrics(page: CdpPage, w: Int, h: Int, dpr: Int): Unit =
    page.send("Emulation.setDeviceMetricsOverride",
      Json.obj("width" -> w, "height" -> h, "deviceScaleFactor" -> dpr, "mobile" -> false))

  /** The card layout — kept byte-for-byte in sync with the `/` landing card:
   *  same gradient, wordmark, single white line and the page-accurate IMDb
   *  (yellow) · Metacritic (green) · RT (red) · Filmweb (orange) pills. The
   *  brand wordmark, display host and whether the Filmweb pill shows come from
   *  `country`; only the city line and background screenshot vary otherwise. */
  private def cardHtml(backgroundPng: String, cityLine: String, country: Country): String = {
    val urlText     = country.ogOrigin.stripPrefix("https://").stripPrefix("http://")
    val filmwebPill = if (country.filmwebEnabled) """<span class="r fw"><span class="lab">FW</span><span class="val">7.4</span></span>""" else ""
    s"""<!DOCTYPE html><html><head><meta charset="utf-8"><style>
       |*{margin:0;padding:0;box-sizing:border-box}
       |html,body{width:1200px;height:630px}
       |body{font-family:"Helvetica Neue",Arial,sans-serif;color:#fff;position:relative;overflow:hidden;background:#0d0d22;
       | -webkit-font-smoothing:antialiased;-moz-osx-font-smoothing:grayscale;text-rendering:geometricPrecision}
       |.hero{position:absolute;inset:0;background-image:url('data:image/png;base64,$backgroundPng');
       | background-repeat:no-repeat;background-size:cover;background-position:center top;filter:saturate(1.06)}
       |.shade{position:absolute;inset:0;background:linear-gradient(90deg,
       | rgba(13,13,34,.97) 0%,rgba(13,13,34,.92) 30%,rgba(13,13,34,.3) 64%,rgba(13,13,34,0) 100%)}
       |.left{position:absolute;left:84px;top:50%;transform:translateY(-50%);width:560px;z-index:3}
       |.clap{font-size:38px}
       |.brand{font-weight:800;font-size:86px;letter-spacing:-2px;line-height:1;color:#fff}
       |.tag{font-size:33px;line-height:1.3;color:#fff;font-weight:500;max-width:540px;margin-top:18px}
       |.tag b{color:#fff;font-weight:700}
       |.url{font-size:23px;color:#fff;font-weight:700;margin-top:28px;opacity:.95}
       |.ratings{display:flex;gap:9px;align-items:center;margin-top:30px;flex-wrap:wrap}
       |.r{display:inline-flex;align-items:stretch;border-radius:5px;overflow:hidden;font-size:25px;line-height:1.15;font-weight:600}
       |.r .lab{font-weight:700;padding:8px 10px;letter-spacing:.02em}
       |.r .val{background:#2a2a3e;padding:8px 11px}
       |.imdb .lab{background:#f5c518;color:#000}.imdb .val{color:#f5c518}
       |.fw .lab{background:#ff6c00;color:#fff}.fw .val{color:#ff9c4a}
       |.rt .lab{background:#fa320a;color:#fff}.rt .val{color:#ff7c5a}
       |.mc{background:#66cc66;color:#002200;font-weight:700;padding:8px 11px;border-radius:5px;font-size:25px;line-height:1.15}
       |</style></head><body>
       |<div class="hero"></div><div class="shade"></div>
       |<div class="left">
       |  <div class="clap">🎬</div>
       |  <div class="brand">${country.brandName}</div>
       |  <div class="tag">$cityLine</div>
       |  <div class="ratings">
       |    <span class="r imdb"><span class="lab">IMDb</span><span class="val">7.8</span></span>
       |    <span class="mc">81</span>
       |    <span class="r rt"><span class="lab">RT</span><span class="val">91%</span></span>
       |    $filmwebPill
       |  </div>
       |  <div class="url">$urlText</div>
       |</div>
       |</body></html>""".stripMargin
  }
}
