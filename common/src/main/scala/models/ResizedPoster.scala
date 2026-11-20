package models

/**
 * A poster URL naming a resized copy of an image by the size it was cut to: WordPress keeps each upload's original at
 * `<name>.<ext>` and writes every size a theme asks for beside it as `<name>-<W>x<H>.<ext>` (Kino Bułgarska 19's
 * listing shows the 120x120 `attachment-thumbnail`; Kino Malta's a 200x288 copy). Upscaled into the card's 480x720
 * slot, a copy that small is a blur, so the record ranks it below every full-size poster
 * ([[MovieRecord.posterUrl]]) and a scraper reads the original instead ([[fullSize]]).
 *
 * Only the last `-WxH` before the extension is a size, and each side has two digits at least: `-4x5` / `-16x9` are
 * aspect ratios some venues name their artwork by, never a resize.
 */
object ResizedPoster {

  private val Suffix = """-(\d{2,5})x(\d{2,5})(\.(?i:jpe?g|png|webp|gif))$""".r.unanchored

  /** Half the card's 480x720 request: a copy narrower or shorter than this is scaled up more than twice. */
  private val MinWidth  = 240
  private val MinHeight = 360

  /** The width and height `url`'s name says it was resized to, if it names one. */
  private def size(url: String): Option[(Int, Int)] = path(url) match {
    case Suffix(w, h, _) => Some((w.toInt, h.toInt))
    case _               => None
  }

  /** True when `url` names a resized copy smaller than half the card on either side. */
  def isSmall(url: String): Boolean = size(url).exists { case (w, h) => w < MinWidth || h < MinHeight }

  /** The original a small copy ([[isSmall]]) was resized from — its last `-WxH` dropped, where WordPress keeps the
   *  upload (an original named `...-834x1200.jpg` cut to `...-834x1200-120x120.jpg` gives back the former) — and
   *  `url` itself otherwise: a copy at size, or an image whose name merely ends in a size, is left as the venue
   *  links it. */
  def fullSize(url: String): String = {
    val p = path(url)
    p match {
      case Suffix(w, h, ext) if isSmall(url) => p.stripSuffix(s"-${w}x$h$ext") + ext + url.drop(p.length)
      case _                                 => url
    }
  }

  private def path(url: String): String = url.takeWhile(c => c != '?' && c != '#')
}
