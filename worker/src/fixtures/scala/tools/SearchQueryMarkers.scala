package tools

import models.Country
import services.identity.MultiFilmBill
import services.movies.FormatTags

import java.util.regex.Pattern
import scala.util.matching.Regex

/** The screening markers a listing's title search must not carry, per country, and where the strip each escaped belongs:
 *  what `SearchQueryMarkersSpec` holds every corpus to, and what `scripts/title-trace` says one raw title would raise. */
object SearchQueryMarkers {

  /** A marker word or phrase, matched case-insensitively as whole words. */
  private def phrase(text: String): (String, Regex) =
    text -> ("(?iu)(?<![\\p{L}\\p{N}])" + text.split(" ").map(Pattern.quote).mkString("\\s+") + "(?![\\p{L}\\p{N}])").r

  /** The screening's own words, per country — what the title rules strip or a client peels off:
   *  `FormatTags`' format and version words everywhere; then each language's version and event words —
   *  the premiere / special-screening words of `ExtraTitleRules` in Poland, the version abbreviations
   *  `ScreeningTokens` maps (OV/OmU/OmeU in Germany, VO/VOS/VOSE/VOSI in Spain) and the special-audience
   *  labels it refuses as badges (parent & baby, autism friendly, relaxed, open caption…), the premium
   *  formats the chain clients badge (Dolby, RPX, ScreenX, 4DX), and the preview / sneak / estreno words
   *  `NonMovieEventClassifier` and the Webedia clients know. */
  def vocabulary(country: Country): Seq[(String, Regex)] = {
    val english = Seq("parent & baby", "parent and baby", "parents & babies", "parent & toddler", "autism friendly",
      "relaxed screening", "dementia friendly", "open caption", "open captioned", "audio described", "toddler time",
      "silver screen", "q&a", "sneak preview", "dolby", "rpx", "screenx", "4dx", "imax", "35mm", "70mm")
    val byLanguage: Map[String, Seq[String]] = Map(
      "pl" -> Seq("premiera", "przedpremiera", "przedpremierowo", "przedpremierowy", "pokaz", "pokazy", "seans"),
      "en" -> english,
      "de" -> Seq("ov", "omu", "omeu", "original mit untertiteln", "preview", "sneak", "vorpremiere"),
      "es" -> Seq("vo", "vos", "vose", "vosi", "v.o.s.e.", "doblada", "estreno", "preestreno", "versión original"))
    (FormatTags.FormatToken.keySet.toSeq ++ byLanguage.getOrElse(country.language.getLanguage, Nil))
      .distinct.sorted.map(phrase)
  }

  /** A listing billing several films as one programme ([[MultiFilmBill]]: a double bill, a trilogy, a marathon) — the
   *  identity resolver matches it to none of them, so what its search carries finds nothing by design. A programme word
   *  annotating ONE title ("Inna Mamusia - maraton") is that film's billing, and is still held to the rule. */
  def billsSeveralFilms(listing: String): Boolean =
    MultiFilmBill.marker(Seq(listing)).isDefined && MultiFilmBill.billedOne(Seq(listing)).isEmpty

  private val Credit = """(?iu)(?:^|\s)reż\.""".r
  private val PlTail = """\s+PL\s*$""".r

  /** Where the strip a marker escaped belongs: a format token is `FormatTags`' to strip, everything else a
   *  country's title rules' — what a finding names, so the fix starts in the right file. */
  def strippedBy(marker: String): String =
    if (FormatTags.FormatToken.contains(marker)) "FormatTags" else "the country's title rules (TitleRules / ExtraTitleRules)"

  /** What `SearchQueryMarkersSpec` finds in the listing billed `raw` that searches `query`: the markers left in it and
   *  where each belongs stripped — None when it carries none, or bills several films. */
  def finding(country: Country, raw: String, query: String): Option[String] =
    Option(raw).filter(_.nonEmpty).filterNot(billsSeveralFilms).flatMap { _ =>
      val markers = markersIn(country, query)
      Option.when(markers.nonEmpty)(s"searches '$query' (${markers.map(marker => s"$marker → ${strippedBy(marker)}").mkString(", ")})")
    }

  /** The markers `query` still carries, in a stable order. */
  def markersIn(country: Country, query: String): Seq[String] =
    vocabularies(country).collect { case (word, pattern) if pattern.findFirstIn(query).isDefined => word } ++
      (if (country == Country.Poland)
        Credit.findFirstIn(query).map(_ => "reż.") ++ PlTail.findFirstIn(query).map(_ => "trailing PL")
      else Nil)

  private val vocabularies: Map[Country, Seq[(String, Regex)]] = Country.all.map(c => c -> vocabulary(c)).toMap
}
