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
}
