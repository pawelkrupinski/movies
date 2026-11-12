package services.cinemas.de

import models.GeneratedTable
import services.cinemas.common.{CineamoClient, KinoheldVenue}

/** The generated table `data/germany/scripts/generate_sources.py` writes to `generated/de-sources/feeds.tsv`
 *  (a tab-separated classpath resource, read through [[GeneratedTable]]): one row a feed — venue key,
 *  the feed's place in the venue's order, its kind, its arguments. Regenerate after re-running
 *  harvest_sources.py; see data/germany/README.md. */
private[de] object GermanSourcesData {
  private def feed(kind: String, args: Seq[String]): GermanFeed = (kind, args) match {
    case ("filmstarts", Seq(theaterId))       => GermanFeed.Filmstarts(theaterId)
    case ("cineamo", Seq(id))                 => GermanFeed.Cineamo(CineamoClient.CinemaId(id))
    case ("kinoheld", Seq(id, path))          => GermanFeed.Kinoheld(KinoheldVenue(id, path))
    case ("kinoprogramm", Seq(path))          => GermanFeed.Kinoprogramm(path)
    case ("wix-events", Seq(page, postcode))  => GermanFeed.WixEvents(page, postcode)
    case ("rex-schifferstadt", Seq(page))     => GermanFeed.RexSchifferstadt(page)
    case ("arsenal-berlin", Seq())            => GermanFeed.ArsenalBerlin
    case _ => throw new IllegalStateException(s"generated/de-sources/feeds.tsv: no German feed of kind '$kind' with arguments $args")
  }

  /** Each venue's feeds, primary first, by its venue key. */
  val byVenueKey: Map[String, Seq[GermanFeed]] =
    GeneratedTable.rows("generated/de-sources/feeds.tsv")
      .groupBy(_(0))
      .map { case (key, rows) =>
        key -> rows.sortBy(_(1).toInt).map(row => feed(row(2), GeneratedTable.list(row(3)))).toSeq
      }
}
