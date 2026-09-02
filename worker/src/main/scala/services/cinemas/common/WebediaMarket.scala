package services.cinemas.common

import java.time.ZoneId

/**
 * One national deployment of the Webedia (AlloCiné) listings platform.
 *
 * Webedia runs the SAME site — same `/_/showtimes/theater-<id>/d-<date>/p-<n>/`
 * website-JSON, same `results[].movie` + bucketed `showtimes` envelope — on a
 * separate host per country (`www.filmstarts.de`, `www.sensacine.com`,
 * `www.allocine.fr`, `www.beyazperde.com`, …). Only the host, the theater-id
 * letter prefix, the localized VENUE-PAGE path and the language-shaped strings
 * differ, so [[WebediaShowtimesClient]] is parameterised by this rather than
 * duplicated per country — the same split [[FlicksMarket]] makes for Flicks.
 *
 * Each market is its own hostname, which is what makes the markets independent
 * downstream: the per-host pace gate ([[tools.RateLimitedHttpFetch]]) and the
 * 429 back-off ([[tools.ThrottledHttpFetch]]) both bucket by full lowercased
 * hostname, so a `Retry-After` earned on `www.filmstarts.de` never stalls
 * `www.sensacine.com` and neither market spends the other's request budget. The
 * corollary is the one that bites: `RealHttpFetch.HostPolicies` matches by host
 * SUFFIX, so a new market does NOT inherit a sibling's row and **a host with no
 * row of its own is not paced at all**. Every market here needs its own row.
 */
sealed abstract class WebediaMarket(
  /** The hostname this market is served from — the key the pace gate and the
   *  429 back-off bucket by, and the value a `HostPolicy` row must name. */
  val host: String,
  /** The market's reference time zone, used only to resolve "today" when a
   *  client is built without an explicit date. A per-market DEFAULT, not a claim
   *  that the country has one zone (Spain has two); getting it wrong costs at
   *  most a day-boundary, never a wrong showtime — Webedia prints each session in
   *  the venue's own local time and we store it as a `LocalDateTime`. */
  val zoneId: ZoneId,
  /** Hour + minute markers in this market's `runtime` string ("1 Std. 56 Min."
   *  in German, "1h 56min" in Spanish). Matched case-insensitively against the
   *  digits that precede them. */
  val hourMarker:   String,
  val minuteMarker: String,
  /** The tokens this market's cinemagoers read for the four language versions a
   *  screening can be in — the abbreviation the showtime badge and the Filtry
   *  "version" radio both show, so each has to be the spelling that country
   *  actually prints:
   *
   *    - `originalVersionToken`  — original audio, no subtitles (`OV` / `VO`)
   *    - `subtitledToken`        — original audio, subtitles in THIS market's
   *                                language (`OmU` / `VOSE`)
   *    - `englishSubtitledToken` — original audio, ENGLISH subtitles, which both
   *                                markets spell differently from their own
   *                                (`OmeU` / `VOSI`); a real and frequently
   *                                sought-out version in expat-heavy cities
   *    - `dubbedToken`           — dubbed into this market's language
   *                                (`DF` / `DOB`)
   *
   *  Dubbing is the DEFAULT in both markets, so the dubbed token is the one a
   *  visitor rarely needs — but it is still emitted, because a cinema that mixes
   *  versions is exactly where it earns its keep, and `_filmShowings` strips the
   *  tokens a cinema's every slot shares, so an all-dubbed venue shows none of
   *  them. */
  val originalVersionToken:  String,
  val subtitledToken:        String,
  val englishSubtitledToken: String,
  val dubbedToken:           String,
  /** Audio languages OTHER than this market's own that a dubbed screening is
   *  worth naming for, `Localization.Language.<X>` suffix (lower-case) → token.
   *  A Catalan-dubbed screening is a different audience from a Castilian one, and
   *  Catalan is the only such language a probe of 172 Spanish venues
   *  (2026-09-02) actually saw; Germany has no such split, so its map is empty.
   *  Add a row here — not a rule elsewhere — when another one shows up. */
  val dubbedLanguageTokens: Map[String, String] = Map.empty,
) {
  /** The public, browser-renderable venue page for `theaterId` — the ONE path
   *  that is localized per market (the JSON endpoint is uniform). Its
   *  `data-showtimes-dates` attribute is what tells the client which days to
   *  fetch. */
  def venuePageUrl(theaterId: String): String = s"https://$host$venuePath$theaterId/"

  protected def venuePath: String

  /** The stored spelling of a `releases[].certificate.code`. Germany's codes
   *  arrive already labelled ("FSK 6") and are kept verbatim; Spain's arrive as
   *  a BARE NUMBER ("16"), which reads as an age only with the "+" its cinemas
   *  print in front of it. Identity by default, so a market that already spells
   *  its certificates out needs nothing here. */
  def certificateLabel(code: String): String = code
}

object WebediaMarket {

  /** Germany — Filmstarts. Theater ids are `A####`. */
  case object Germany extends WebediaMarket(
    host                 = "www.filmstarts.de",
    zoneId               = ZoneId.of("Europe/Berlin"),
    hourMarker           = "Std",
    minuteMarker         = "Min",
    originalVersionToken  = "OV",
    subtitledToken        = "OmU",
    englishSubtitledToken = "OmeU",
    dubbedToken           = "DF",
  ) {
    protected def venuePath: String = "/kinoprogramm/kino/"
  }

  /** Spain — SensaCine. Theater ids are `E####`, and three things differ from
   *  Germany beyond the host: the venue page sits at `/cines/cine/<id>/`, the
   *  `runtime` string is the compact "1h 46min" rather than "1 Std. 46 Min.",
   *  and certificates come through as a bare number.
   *
   *  A THIRD the size of Germany's roster — 594 venues over 52 provinces
   *  (measured 2026-09-01 by sweeping `/cines/provincias-<id>/`; SensaCine
   *  publishes no cinema sitemap, so the province index is the only enumeration
   *  there is). `Europe/Madrid` is the "today" default only: the Canary
   *  provinces run an hour behind on `Atlantic/Canary` and each carries its own
   *  zone, so a client for a Canary venue is handed that city's date. */
  case object Spain extends WebediaMarket(
    host                 = "www.sensacine.com",
    zoneId               = ZoneId.of("Europe/Madrid"),
    hourMarker           = "h",
    minuteMarker         = "min",
    originalVersionToken  = "VO",
    subtitledToken        = "VOSE",
    englishSubtitledToken = "VOSI",
    dubbedToken           = "DOB",
    dubbedLanguageTokens  = Map("catalan" -> "CAT"),
  ) {
    protected def venuePath: String = "/cines/cine/"

    /** "16" -> "+16", the way a Spanish listing prints it; "APTA" and "TP" (all
     *  ages) are already words and are left alone. */
    override def certificateLabel(code: String): String =
      if (code.forall(_.isDigit) && code.nonEmpty) s"+$code" else code
  }

  val all: Seq[WebediaMarket] = Seq(Germany, Spain)
}
