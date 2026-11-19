package models

import java.util.Locale

/** How a [[MovieRecord]]'s display fields merge across its source slots, each rule given the slots' values in source
 *  priority order: its posters ([[postersByPreference]]), its credits ([[credits]]) and its production countries
 *  ([[countries]]). The record picks which slots feed each; the rule is here. */
private[models] object SourceMerge {

  /** Every poster URL, de-duplicated, in the order given — except that known "coming soon" placeholders (see
   *  [[PlaceholderPoster]]) are demoted to the very end, so a placeholder is only ever picked when no real poster
   *  exists. A URL that is no poster at all ([[PlaceholderPoster.isAbsent]]) is left out. */
  def postersByPreference(urls: Iterator[String]): Seq[String] = {
    val all = urls.filterNot(PlaceholderPoster.isAbsent).distinct.toSeq
    val (placeholders, real) = all.partition(PlaceholderPoster.matches)
    real ++ placeholders
  }

  /** `names`, spelled the way TMDB's slot (`tmdb`) spells every name it knows: see [[tools.CreditSpelling]]. */
  def credits(names: Seq[String], tmdb: Seq[String]): Seq[String] = tools.CreditSpelling.alignedTo(names, tmdb)

  /** The production countries of every slot, in the order given, deduplicated case-insensitively (see
   *  [[MovieRecord.countries]]). */
  def countries(names: Iterator[String]): Seq[String] = {
    val seen = scala.collection.mutable.LinkedHashSet.empty[String]
    names
      .filter(models.CountryNames.isPolish)
      .foreach { c =>
        val key = c.toLowerCase(Locale.ROOT)
        if (!seen.exists(_.toLowerCase(Locale.ROOT) == key)) seen += c
      }
    seen.toSeq
  }
}
