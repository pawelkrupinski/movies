package controllers

/** Native-language names for the navbar's language picker
 *  (`_navbar.scala.html`'s `#language-select`) — a language names itself in
 *  its own tongue in a language switcher, never in the page's current
 *  language, so a Polish speaker looking at a German page still recognises
 *  "Polski". */
object LanguageNames {

  /** Display order for the picker — matches `WebLangResolver.Supported`. */
  val Codes: Seq[String] = Seq("pl", "en", "de", "es")

  def native(code: String): String = code match {
    case "pl" => "Polski"
    case "en" => "English"
    case "de" => "Deutsch"
    case "es" => "Español"
    case _    => code
  }
}
