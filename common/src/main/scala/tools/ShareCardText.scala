package tools

/** The words a film share card draws that are not the film's own data — per UI language, since
 *  the card is rendered by the worker, which has no Play `Messages`. Each value is the web's own
 *  message for the same thing (`detail.director`), and a web spec holds the two together. */
object ShareCardText {

  /** The director line's label: "Reżyseria" in Polish, "Regie" in German, "Director" otherwise. */
  def directorLabel(lang: String): String = lang match {
    case "pl" => "Reżyseria"
    case "de" => "Regie"
    case _    => "Director"
  }

  /** The language a country's share cards are drawn in: the deployment's own. The in-page
   *  language switch swaps copy client-side, and a crawler fetching `og:image` sees the page in the
   *  deployment's language, so there is one card per film. */
  def language(country: models.Country): String = country.language.getLanguage
}
