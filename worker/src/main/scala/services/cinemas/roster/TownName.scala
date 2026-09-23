package services.cinemas.roster

import tools.Slugify

/**
 * Whether two spellings of a Polish town name the same town — the comparison
 * both roster audits make between the town we annotate a venue with and the town
 * its source publishes (in a URL segment offline, on the page online).
 *
 * The two sides spell one town differently in three ways, and only those three
 * are forgiven:
 *   - case, diacritics and separators: `Jastrzębie Zdrój` / `jastrzebie-zdroj`;
 *   - the qualifier ABBREVIATED: `Ostrów Wlkp.` for Ostrów Wielkopolski, and
 *     `Środa Wlkp.` for Środa Wielkopolska. Expanded, not dropped, because
 *     Ostrów Mazowiecka is a different town;
 *   - the qualifier LEFT OFF by one side: `Połczyn` for Połczyn-Zdrój;
 *   - the qualifier's adjective ENDING: Filmweb files a venue under
 *     "Wysokie Mazowiecki", which is Wysokie Mazowieckie. Only the ending
 *     (`-i`/`-ie`/`-a`/`-e` after `-sk`/`-ck`) is loosened, so Środa Śląska and
 *     Środa Wielkopolska stay apart.
 */
object TownName {

  /** Abbreviated qualifier → the stem every spelled-out form starts with. */
  private val Abbreviations: Map[String, String] = Map(
    "wlkp" -> "wielkopolsk",
    "maz"  -> "mazowieck",
    "sl"   -> "slask",
    "gd"   -> "gdansk",
  )

  private val AdjectiveEnding = """^(.+(?:sk|ck))(?:ie|i|a|e)$""".r

  private def tokens(town: String): Seq[String] =
    Slugify.stable(town).split('-').toSeq.filter(_.nonEmpty).map {
      case AdjectiveEnding(stem) => stem
      case token                 => token
    }

  private def sameToken(a: String, b: String): Boolean =
    a == b ||
      Abbreviations.get(a).exists(b.startsWith) ||
      Abbreviations.get(b).exists(a.startsWith)

  def same(a: String, b: String): Boolean = {
    val (x, y) = (tokens(a), tokens(b))
    x.nonEmpty && y.nonEmpty && x.zip(y).forall(sameToken.tupled)
  }
}
