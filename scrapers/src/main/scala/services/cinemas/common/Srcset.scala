package services.cinemas.common

/** An `<img srcset>`: the candidate URLs a page offers for one image, each with its width (`640w`) or pixel density
 *  (`2x`). */
object Srcset {

  /** A `srcset`'s candidates are split at a comma after a descriptor ("…-300x450.jpg 300w, …") or before a space,
    * never inside a URL ("…/w_300,h_450/…"). */
  private val Separator = """(?<=\d[wx])\s*,\s*|,\s+""".r
  private val Candidate = """(\S+)(?:\s+(\d+(?:\.\d+)?)([wx]))?""".r

  /** The widest candidate's URL as written (by `w`, else by `x` density; one naming neither is `1x`); `None` for a
    * blank srcset. A lazy loader's placeholder (Avada's `data:image/svg+xml,…` on Kino Remus and SCK Stargard) is no
    * candidate. */
  def widest(srcset: String): Option[String] = {
    val candidates = Separator.split(srcset.trim).toSeq.map(_.trim).flatMap(candidate)
    val widths     = candidates.filter(_._2 == "w")
    (if (widths.nonEmpty) widths else candidates).maxByOption(_._3).map(_._1)
  }

  /** One candidate's URL, descriptor unit (`w` or `x`, `x` when it names none) and size. */
  private def candidate(text: String): Option[(String, String, Double)] = text match {
    case Candidate(url, size, unit) if !url.startsWith("data:") =>
      Some((url, Option(unit).getOrElse("x"), Option(size).fold(1.0)(_.toDouble)))
    case _ => None
  }
}
