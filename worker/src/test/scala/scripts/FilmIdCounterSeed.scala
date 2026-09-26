package scripts

import models.Country
import services.identity.{FilmIdMapping, IdSeeding, MongoFilmIdCounterStore}
import services.movies.{ListingKey, MovieRepository}

/**
 * Seeds the persisted FilmId map (`identity_film_ids`, docs/design/identity-resolver.md §16) from
 * today's films: every film id in `movies` or `movie_slots` gets the `Long` counter `IdAssigner`
 * will know it by, by [[services.identity.FilmIdCounters]]' rule — films already mapped keep their
 * counter, new ones follow the largest, largest film (most venue listings) first. Append-only: a run
 * never changes or removes an entry, and a second run over the same films adds nothing.
 *
 * Nothing serving reads the map; no FilmId changes. DRY RUN BY DEFAULT; `--apply` writes.
 *
 * {{{
 *   . scripts/local-mirror/prod-tunnel.sh && ensure_prod_tunnel
 *   sbt "worker/Test/runMain scripts.FilmIdCounterSeed"            # dry run, all countries
 *   sbt "worker/Test/runMain scripts.FilmIdCounterSeed --apply es"  # WRITE, Spain
 * }}}
 *
 * Mongo only; the reads are the backfill's keyset-paged scans.
 */
object FilmIdCounterSeed {

  /** Today's films: every id `movies` or `movie_slots` holds, each with its venue listings. */
  def films(slots: Seq[ListingKeyBackfill.SlotRow], movieIds: Seq[String]): Seq[IdSeeding.Film] = {
    val listings = slots.flatMap(r => ListingKey.ofSlotRow(r.slotKey, r.slot).map(r.filmId -> _)).groupMap(_._1)(_._2)
    (movieIds ++ slots.map(_.filmId)).distinct.sorted.map(id => IdSeeding.Film(id, listings.getOrElse(id, Nil).toSet))
  }

  def main(args: Array[String]): Unit = {
    val apply     = args.contains("--apply")
    val countries = args.filterNot(_.startsWith("--")).toSeq match {
      case Seq()     => Country.all
      case requested => requested.map(code => Country.byCode(code).getOrElse {
        println(s"Unknown country code '$code' — expected one of ${Country.all.map(_.code).mkString(", ")}."); sys.exit(1)
      })
    }
    println(if (apply) "APPLY — missing FilmId map entries will be WRITTEN." else "DRY RUN — nothing is written. Pass --apply to write.")
    countries.foreach(seed(_, apply))
    sys.exit(0)
  }

  private def seed(country: Country, apply: Boolean): Unit = {
    val (connection, database) = ListingKeyBackfill.openCountry(country)
    val started = System.nanoTime()
    val today   = films(ListingKeyBackfill.slotRows(database), ListingKeyBackfill.ids(database, MovieRepository.Collection))
    val mapping = new FilmIdMapping(new MongoFilmIdCounterStore(database))
    mapping.plan(today) match {
      case Left(why) => println(s"${country.displayName}: cannot seed — $why")
      case Right((current, additions)) =>
        println(f"${country.displayName}%-15s ${today.size} films (${today.count(_.listings.nonEmpty)} with venue listings) · " +
          s"map holds ${current.size} · ${additions.size} to add, counters ${additions.headOption.fold("-")(_.counter.toString)}" +
          s"..${additions.lastOption.fold("-")(_.counter.toString)}")
        additions.take(5).foreach(a => println(s"    ${a.counter} ← ${a.filmId}"))
        if (apply) println(s"    wrote ${mapping.append(today).fold(why => s"nothing: $why", _.toString)} entr(ies)")
    }
    println(f"    ${(System.nanoTime() - started) / 1e9}%.1fs")
    connection.close()
  }
}
