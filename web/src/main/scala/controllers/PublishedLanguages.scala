package controllers

/** Picks the language of a standalone document page from its `?lang=`.
 *
 *  The store listings (App Store and Play, one registration per locale) all
 *  point at whichever deployment serves that country, so the LINK carries the
 *  language and the deployment must be able to answer in one it does not itself
 *  run in. An unrecognised or absent `lang` falls back to the deployment's own
 *  language rather than 404ing — a bare URL is what an old link, or someone
 *  typing it, will hit.
 *
 *  Shared by `/privacy-policy` and `/support`, which publish different language
 *  sets (the policy has no Spanish translation yet, the support page does) but
 *  resolve them identically.
 */
object PublishedLanguages {
  def resolve(requested: Option[String], published: Set[String]): String =
    requested
      .map(_.trim.toLowerCase)
      .filter(published.contains)
      .getOrElse(models.Country.fromEnv.language.getLanguage)
}
