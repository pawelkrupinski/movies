package services.cinemas.es

import play.api.libs.json.{JsNumber, JsString, JsValue}
import tools.TextNormalization

import java.util.Locale

/** How the Spanish box-office feeds (taquilla.com's JSON, Artesiete's session
 *  props, Ocine's ticketing API) spell a field and a certificate, read the one way every Spanish own-source
 *  parser shares. */
private[es] object SpanishFeedText {

  /** Lower-cased, accents off: "Catalán" and "CATALÁN", "AÑOS" and "años" read alike. */
  def fold(s: String): String = TextNormalization.deburr(s.toLowerCase(Locale.ROOT))

  /** A field's text, trimmed — a number the feed sends unquoted read as its digits,
   *  a blank one as none. */
  def field(js: JsValue, key: String): Option[String] =
    (js \ key).toOption.collect {
      case JsString(s) => s.trim
      case JsNumber(n) => n.toString
    }.filter(_.nonEmpty)

  /** The certificate the way SensaCine prints it: "+12" for "M-12", "+16" and "NO
   *  RECOMENDADA PARA MENORES DE 16 AÑOS", "+7" for "Mayores de 7 años" and Catalan "7 anys"; "APTA" for
   *  "TP" and "APTA PARA TODOS LOS PÚBLICOS"; none while it is pending ("PDC",
   *  "Pendiente de Calificacion") or a placeholder ("-"). Anything else upper-cased. */
  def certificate(raw: String): Option[String] = {
    val folded = fold(raw.trim)
    if (folded == "pdc" || folded.contains("pendiente") || !folded.exists(_.isLetterOrDigit)) None
    else if (folded == "tp" || folded.contains("todos los publicos")) Some("APTA")
    else Years.findFirstMatchIn(folded).map(m => s"+${Option(m.group(1)).getOrElse(m.group(2))}")
      .orElse(Some(raw.trim.toUpperCase(Locale.ROOT)))
  }
  private val Years = """(?:^m-|^\+|menores de |^(?=\d{1,2}$))(\d{1,2})|(\d{1,2})\s*an[oy]s""".r
}
