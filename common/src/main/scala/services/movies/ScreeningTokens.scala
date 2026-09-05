package services.movies

import play.api.Logging

import java.util.Locale
import java.util.concurrent.ConcurrentHashMap

/**
 * The ONE vocabulary of tokens a showtime badge may carry, and the mapping from
 * every source's own spelling into it.
 *
 * A cinema source labels its screenings in its own words, and roughly a hundred
 * clients pass those words straight through to [[models.Showtime.format]]. Left
 * alone that produces neither a vocabulary nor a badge — measured across the
 * five production databases on 2026-09-02, the UK alone had **59 distinct
 * tokens**, of which the three commonest were `Wheelchair Accessible` (43,225
 * screenings), `Audio Described` (31,563) and `AD` (23,319) — the last two being
 * the same thing spelled twice. `Subtitled`/`SUB`/`Subbed`, `70mm`/`70mm
 * Screening`/`70MM`, `4K`/`4K Screening` and six spellings of a parent-and-baby
 * club all coexisted, and Poland shipped `2d` beside `2D`, `napisy` beside `NAP`
 * and `LEKT` beside `LEK`.
 *
 * So every token passes through here at the ingest choke point
 * ([[MovieCache.recordCinemaScrape]], plus the detail-page merge in
 * `DetailEnricher`), for the same reason [[FormatTags]] lives beside it: one
 * implementation every client shares, rather than a hundred clients each
 * deciding what a badge says.
 *
 * WHAT EARNS A BADGE — three things a visitor is choosing between, and nothing
 * else:
 *
 *   1. **Screen format** — what the projector and the room do (2D/3D, IMAX,
 *      4DX, ScreenX, Laser, Atmos, 70mm…).
 *   2. **Language version** — what you will hear and read (NAP/DUB/LEK/ORG in
 *      Poland, VO/VOSE/VOSI/DOB in Spain, OV/OmU/OmeU/DF in Germany, SUB/DUB
 *      elsewhere, plus the audio language itself where a source names one).
 *   3. **Per-screening accessibility** — AD (audio description) and OC (open
 *      captions), which a visitor who needs them seeks out by name.
 *
 * What does NOT: `Wheelchair Accessible`, which is a property of the VENUE and
 * true of nearly every screening in it — 43,225 badges saying nothing; seat
 * comfort (`Recliner`); and the audience/pricing labels a cinema hangs on a
 * screening (`Parent & Baby Club`, `Toddler Time`, `Relaxed`, `Silver Cinema`,
 * `£5 Tickets`, `Q&A`, `Free`). Those last are the same class of thing the
 * [[FormatTags]] title strip already refuses to treat as a format: a
 * special-audience screening is an EVENT, not a way of showing the film.
 *
 * An UNRECOGNISED label is dropped, not passed through — passing through is
 * precisely how `Glasgow Film Club` came to be a badge. It is logged once per
 * distinct label so a genuinely new format shows up in the logs instead of on
 * 40,000 screenings.
 *
 * One token is the COUNTRY's to spell, which is why this is a class with a
 * per-country instance rather than an object: see [[voiceover]].
 */
class ScreeningTokens(
  /** The token a VOICE-OVER screening carries — see [[models.Country.voiceoverToken]].
   *  Every OTHER token in the vocabulary is the same in every country (a source
   *  either says IMAX or it doesn't), so this one parameter is the whole reason
   *  the vocabulary is instantiated per country instead of being a shared table. */
  val voiceover: String
) extends Logging {
  import ScreeningTokens._

  /** The token(s) `raw` means — empty when it is not a screening attribute. */
  def canonical(raw: String): List[String] = {
    val k = key(raw)
    if (k.isEmpty) Nil
    else if (VoiceoverLabels.contains(k)) List(voiceover)
    else Canonical.get(k).orElse(LanguageNames.get(k)).getOrElse {
      if (!NotAScreeningAttribute.contains(k) && reported.add(k))
        logger.info(s"ScreeningTokens: dropping unrecognised screening label '$raw' — " +
          "add it to Canonical if it names a format, version or accessibility feature")
      Nil
    }
  }

  /** Normalise one screening's tokens: each mapped to the shared vocabulary,
   *  unrecognised ones dropped, duplicates collapsed, source order kept. */
  def normalize(tokens: Seq[String]): List[String] =
    tokens.iterator.flatMap(canonical).distinct.toList
}

object ScreeningTokens extends Logging {

  /** This vocabulary as `country` spells it. */
  def of(country: models.Country): ScreeningTokens = new ScreeningTokens(country.voiceoverToken)

  /** The default country's spelling, for the single-country constructions that
   *  predate the country split — the same default `MovieCache`'s
   *  `enrichmentLanguage` takes, and for the same reason. */
  val Default: ScreeningTokens = of(models.Country.default)

  /** The labels that name a voice-over, whose TOKEN is the country's own. Helios
   *  spells it `LEC`, and does so 42 times to `LEK`'s one: its `speakingType`
   *  vocabulary is exactly {Napisy, DUB, ORG, LEC}, and lektor is the only one of
   *  Poland's four versions the other three leave unnamed. */
  private val VoiceoverLabels: Set[String] = Set("lek", "lekt", "lektor", "lec")

  /** Source spelling (see [[key]]) → the token(s) it means. The keys are the
   *  real labels measured in the five production databases, plus the ones the
   *  chain clients emit that no country is currently scraping (AMC's, read off
   *  its recorded fixture); the values are the vocabulary.
   *
   *  A LIST because one label can name two things — AMC sells "IMAX with Laser
   *  at AMC" as a single attribute, and SensaCine spells a 4D screening in 3D
   *  `Format.Projection.4DE3D`. */
  private val Canonical: Map[String, List[String]] = Map(
    // ── Screen format ──────────────────────────────────────────────────────
    "2d" -> List("2D"), "3d" -> List("3D"),
    "imax" -> List("IMAX"), "imaxexperience" -> List("IMAX"),
    "4dx" -> List("4DX"), "4de" -> List("4DE"), "4de3d" -> List("4DE", "3D"),
    "screenx" -> List("SCREENX"), "isense" -> List("ISENSE"), "plf" -> List("PLF"),
    "epic" -> List("EPIC"), "infinity" -> List("INFINITY"), "dbox" -> List("DBOX"),
    "laser" -> List("LASER"), "hdr" -> List("HDR"),
    "atmos" -> List("ATMOS"), "dolbyatmos" -> List("ATMOS"), "dolby" -> List("DOLBY"),
    "4k" -> List("4K"), "4kscreening" -> List("4K"),
    "70mm" -> List("70MM"), "70mmscreening" -> List("70MM"),
    "35mm" -> List("35MM"), "35mmscreening" -> List("35MM"),
    "16mm" -> List("16MM"), "16mmscreening" -> List("16MM"),
    "vip" -> List("VIP"), "premium" -> List("PREMIUM"),
    // AMC brands every attribute with its own name, and sells two at once.
    "imaxatamc" -> List("IMAX"), "imaxwithlaseratamc" -> List("IMAX", "LASER"),
    "laseratamc" -> List("LASER"), "dolbycinemaatamc" -> List("DOLBY"),
    "reald3d" -> List("3D"),

    // ── Language version ───────────────────────────────────────────────────
    // Poland's own words arrive both spelled out and abbreviated.
    "nap" -> List("NAP"), "napisy" -> List("NAP"), "napisypl" -> List("NAP"),
    "dub" -> List("DUB"), "dubb" -> List("DUB"), "dubbing" -> List("DUB"), "dubbingpl" -> List("DUB"), "dubbed" -> List("DUB"),
    // (the voice-over labels are NOT here — their token is the country's own,
    // see `VoiceoverLabels` above)
    "org" -> List("ORG"), "oryginalny" -> List("ORG"),
    "sub" -> List("SUB"), "subbed" -> List("SUB"), "subtitled" -> List("SUB"), "subtitles" -> List("SUB"),
    // The market abbreviations the Webedia clients emit, kept verbatim: each is
    // the spelling that country's cinemagoers read.
    "vo" -> List("VO"), "vose" -> List("VOSE"), "vosi" -> List("VOSI"), "dob" -> List("DOB"), "cat" -> List("CAT"),
    "ov" -> List("OV"), "omu" -> List("OmU"), "omeu" -> List("OmeU"), "df" -> List("DF"),

    // ── Per-screening accessibility ────────────────────────────────────────
    "ad" -> List("AD"), "audiodescribed" -> List("AD"), "audiodescription" -> List("AD"),
    "oc" -> List("OC"), "opencaps" -> List("OC"), "opencaptions" -> List("OC"), "opencaptioned" -> List("OC"),
  )

  /** Labels seen in production that are deliberately NOT badges — a venue
   *  property, a seat, or a special-audience/pricing event. Listed rather than
   *  left to the unknown-label branch so they drop SILENTLY: they are not news,
   *  and logging 43,000 of them every scrape would bury the labels that are. */
  private val NotAScreeningAttribute: Set[String] = Set(
    "wheelchairaccessible", "wheelchair", "recliner", "recliners",
    "kids", "kidsclub", "kidsscreening",
    "relaxed", "relaxedscreening", "sensoryscreening", "dementiafriendly",
    "parentbaby", "parentbabyclub", "parentbabyonly", "babyme", "babyfriendly",
    "carersbabies", "toddlertime", "toddlerclub",
    "silvercinema", "silverscreen", "seniorscreening",
    "5tickets", "free", "qa", "glasgowfilmclub", "filmclub",
    "privatetheatrerental", "privatetheatrerentals",
  )

  /** Every ISO language spelled the way English names it, lower-cased and
   *  punctuation-free — so a source that labels a screening by its AUDIO
   *  LANGUAGE ("Hindi", "Telugu", "Japanese") yields that language as a token
   *  without a hand-kept list to fall behind. That IS a language version: at a
   *  UK multiplex "Hindi" is the whole difference between two screenings of the
   *  same film. */
  private val LanguageNames: Map[String, List[String]] =
    Locale.getISOLanguages.iterator.flatMap { code =>
      val name = Locale.forLanguageTag(code).getDisplayLanguage(Locale.ENGLISH)
      Option.when(name.nonEmpty && name != code)(key(name) -> List(name.toUpperCase(Locale.ROOT)))
    }.toMap

  /** Distinct labels already reported, so an unrecognised one is logged once per
   *  process rather than once per screening. */
  private val reported = ConcurrentHashMap.newKeySet[String]()

  /** The comparison form of a source label: lower-cased, stripped of everything
   *  that isn't a letter or a digit. Collapses `Audio Described` / `audio-described`
   *  / `AUDIO DESCRIBED`, and `70mm Screening` / `70mm screening`, onto one key. */
  private def key(raw: String): String =
    raw.toLowerCase(Locale.ROOT).filter(c => c.isLetterOrDigit)

}
