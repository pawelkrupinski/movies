package services.cinemas.uk

import models.{GeneratedTable, UkRoster}

/** Every UK venue's sources, in scrape order: its primary, and the flicks.co.uk slug an own-site
 *  primary falls back to. Read from the generated table `generated/uk-sources/sources.tsv`
 *  (`data/uk/scripts/generate_roster.py`, from `data/uk/roster.json`, in its order): a row is the
 *  venue's [[UkRoster]] key, its primary's kind and arguments ([[UkFeed.parse]]), and the fallback. */
private[cinemas] object UkSourcesData {
  private val Table = "generated/uk-sources/sources.tsv"

  val venues: Seq[UkVenueSources] = GeneratedTable.rows(Table).map {
    case Seq(key, kind, args, fallback) =>
      UkVenueSources(UkRoster(key), UkFeed.parse(kind, GeneratedTable.list(args)), GeneratedTable.option(fallback))
    case row => throw new IllegalStateException(s"$Table: a row of ${row.size} cells, not 4: ${row.mkString(" | ")}")
  }
}
