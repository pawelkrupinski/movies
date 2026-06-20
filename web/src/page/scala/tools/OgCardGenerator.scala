package tools

import models.City
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
 * Each card is the city's REAL repertoire page (a live desktop screenshot,
 * with all dates shown so the poster grid is full even at night) under a
 * left-side dark gradient carrying the "Kinowo / Repertuar kin w {locative}"
 * wordmark and the IMDb·Metacritic·RT·Filmweb pills — the same look as the
 * `/` landing card (`og-home.png`).
 *
 * Pipeline per city (one headless Chrome over CDP, a fresh tab each step):
 *   1. open the live `/{slug}/`, run `pickDay('anytime')`, screenshot the
 *      desktop viewport at 2× → the background.
 *   2. open a card HTML (the screenshot embedded as a data: URI + the
 *      overlay) at 3× → the composed card.
 *   3. downscale 3×→1× with bicubic supersampling (smooth text) → write PNG.
 *
 * Run (from the repo root):
 *   sbt 'web/PageTest/runMain tools.OgCardGenerator'            # all cities
 *   sbt 'web/PageTest/runMain tools.OgCardGenerator poznan wroclaw'   # a subset
 *
 * Env: `KINOWO_OG_BASE` (default `https://kinowo.fly.dev`) — the site to
 * screenshot; `KINOWO_OG_OUT` (default `web/src/main/assets/img`) — output dir.
 * Chrome is located via `Chrome.findExecutable()` (same `CDP_BROWSER_BIN`
 * override the page tests use). Each city takes well under a minute.
 */
object OgCardGenerator {

  private val Width  = 1200
  private val Height = 630
  private val Scale  = 3 // render the card at 3× then supersample down for smooth text

  def main(args: Array[String]): Unit = {
    val baseUrl = sys.env.getOrElse("KINOWO_OG_BASE", "https://kinowo.fly.dev").stripSuffix("/")
    val outDir  = Paths.get(sys.env.getOrElse("KINOWO_OG_OUT", "web/src/main/assets/img"))
    Files.createDirectories(outDir)

    val only   = args.toSet
    val cities = City.all.filter(c => only.isEmpty || only(c.slug))
    if (cities.isEmpty) { System.err.println(s"No cities matched ${only.mkString(", ")}"); sys.exit(1) }

    val chrome = Chrome.tryStart().getOrElse {
      System.err.println("No Chrome/Chromium found (set CDP_BROWSER_BIN). Aborting.")
      sys.exit(1)
    }

    val startedAt = System.currentTimeMillis()
    var ok = 0
    try {
      cities.foreach { city =>
        val t0 = System.currentTimeMillis()
        try {
          val bg   = screenshotCity(chrome, s"$baseUrl/${city.slug}/")
          val card = renderCard(chrome, bg, s"Repertuar kin ${city.locativePhrase}")
          val out  = outDir.resolve(s"og-${city.slug}.png")
          Files.write(out, downscale(card))
          ok += 1
          println(f"✓ ${city.slug}%-20s ${(System.currentTimeMillis() - t0) / 1000.0}%4.1fs  $out")
        } catch {
          case e: Throwable =>
            System.err.println(s"✗ ${city.slug}: ${e.getMessage}")
        }
      }
    } finally chrome.close()

    val secs = (System.currentTimeMillis() - startedAt) / 1000.0
    println(f"done: $ok/${cities.size} cards in $secs%.1fs (~${secs / cities.size}%.1fs/city)")
  }

  /** Screenshot the live city page at desktop 2×, with every date shown so the
   *  grid is populated regardless of the hour. Returns Base64 PNG bytes. */
  private def screenshotCity(chrome: Chrome, url: String): String =
    chrome.openPage(url) { page =>
      setMetrics(page, 1180, 760, 2)
      // The date tabs are JS-only (`pickDay`); "anytime" shows all upcoming
      // days so a late-night "Dziś" empty view doesn't yield an empty card.
      try page.eval("(typeof pickDay==='function') ? (pickDay('anytime'), true) : false")
      catch { case _: Throwable => () }
      Thread.sleep(900) // let the filter re-layout + the top posters decode
      page.screenshot()
    }

  /** Compose the card HTML (the screenshot as a full-bleed background + the
   *  left gradient, wordmark, city line and rating pills) and screenshot it at
   *  `Scale`×. Returns Base64 PNG bytes. */
  private def renderCard(chrome: Chrome, backgroundPng: String, cityLine: String): String = {
    val html = cardHtml(backgroundPng, cityLine)
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

  /** The card layout — kept byte-for-byte in sync with the `/` landing card
   *  (`og-home.png`): same gradient, wordmark, single white line and the
   *  page-accurate IMDb (yellow) · Metacritic (green) · RT (red) · Filmweb
   *  (orange) pills. Only the city line and background screenshot vary. */
  private def cardHtml(backgroundPng: String, cityLine: String): String =
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
       |  <div class="brand">Kinowo</div>
       |  <div class="tag">$cityLine</div>
       |  <div class="ratings">
       |    <span class="r imdb"><span class="lab">IMDb</span><span class="val">7.8</span></span>
       |    <span class="mc">81</span>
       |    <span class="r rt"><span class="lab">RT</span><span class="val">91%</span></span>
       |    <span class="r fw"><span class="lab">FW</span><span class="val">7.4</span></span>
       |  </div>
       |  <div class="url">kinowo.fly.dev</div>
       |</div>
       |</body></html>""".stripMargin
}
