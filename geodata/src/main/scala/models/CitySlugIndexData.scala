package models

/** Every slug a country answers to besides its pages' — the tables `data/scripts/generate_slug_index.py`
 *  writes under `generated/slug-index/` (see that script for what makes a slug an alias). */
private[models] object CitySlugIndexData {
  private val Directory = "generated/slug-index"

  /** `country`'s alias slug → the page it 301s to. */
  def aliases(country: String): Map[String, String] = GeneratedTable.lookup(s"$Directory/aliases-$country.tsv")

  /** Per country: page slug → its qualified slug, for every page whose slug a page of
   *  another country has too. TEMPORARY, with `LegacySlugOwners`. */
  def sharedPageQualifiedSlugs: Map[String, Map[String, String]] =
    GeneratedTable.rows(s"$Directory/shared-qualified-slugs.tsv")
      .groupMap(_(0))(row => row(1) -> row(2)).view.mapValues(_.toMap).toMap
}
