package services.identity

import scala.io.Source

/** The cuts of a film cinemas bill at their own running time (`identity-film-cuts.tsv`, hand-kept, each row citing
 *  its source): TMDB keeps ONE record per film at its theatrical runtime — "The Return of the King" at 201 minutes,
 *  none for the 263-minute extended edition; "Apocalypse Now" at 147, none for the Final Cut (183) or Redux (202) —
 *  and no source we read names a cut's runtime machine-readably for every film. A listing stating a cut's runtime
 *  runs as the film: the runtime comparison reads the film's runtime NEAREST the listing's
 *  ([[nearestRuntime]]), its theatrical one or a cut's.
 *
 *  Keyed by the film's IMDb number ([[IdentityMeasures.Film.imdbNumber]]), which a TMDB record and an IMDb or Wikidata
 *  record of the film all carry; the table's tmdbId column is for the reader. */
object FilmCuts {

  /** One cut of a film: its label ("Extended Edition", "Final Cut"), its running time in minutes, where it is stated. */
  final case class Cut(tmdbId: Int, imdbId: String, label: String, runtime: Int, source: String)

  val Resource = "identity-film-cuts.tsv"

  /** Lines `<tmdbId>\t<imdbId>\t<label>\t<minutes>\t<source URL>`; `#` starts a comment. */
  def parse(lines: Iterator[String]): Map[Int, Seq[Cut]] =
    lines.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#")).map(_.split("\t").map(_.trim)).collect {
      case Array(tmdbId, imdbId, label, minutes, source) if IdentityMeasures.imdbNumber(imdbId) > 0 =>
        Cut(tmdbId.toInt, imdbId, label, minutes.toInt, source)
    }.toSeq.groupBy(cut => IdentityMeasures.imdbNumber(cut.imdbId))

  /** The table on the classpath, by IMDb number. */
  lazy val table: Map[Int, Seq[Cut]] =
    Option(getClass.getClassLoader.getResourceAsStream(Resource)).fold(Map.empty[Int, Seq[Cut]]) { in =>
      try parse(Source.fromInputStream(in, "UTF-8").getLines()) finally in.close()
    }

  /** The cuts the table names of the film IMDb numbers `imdbNumber` (0: none). */
  def of(imdbNumber: Int): Seq[Cut] = if (imdbNumber == 0) Nil else table.getOrElse(imdbNumber, Nil)

  /** The film's running time nearest a listing's `stated` one: its own (theatrical) runtime or a cut's. `None` when the
   *  film states none. */
  def nearestRuntime(stated: Option[Int], film: IdentityMeasures.Film): Option[Int] =
    nearestRuntime(stated, film.runtime, film.imdbNumber)

  /** [[nearestRuntime]] of a record whose IMDb number is a cross-id beside it (a family's `SourceRecord`), not its own. */
  def nearestRuntime(stated: Option[Int], runtime: Option[Int], imdbNumber: Int): Option[Int] = {
    val own = runtime.filter(_ > 0)
    stated.fold(own) { minutes =>
      val cuts = of(imdbNumber)
      if (cuts.isEmpty) own else (own.toSeq ++ cuts.map(_.runtime)).minByOption(candidate => math.abs(candidate - minutes))
    }
  }

  /** The cut a listing running `stated` minutes bills: one the table names nearer its runtime than the film's own. */
  def billedCut(stated: Int, film: IdentityMeasures.Film): Option[Cut] = {
    val ownGap = film.runtime.filter(_ > 0).fold(Int.MaxValue)(runtime => math.abs(runtime - stated))
    of(film.imdbNumber).filter(cut => math.abs(cut.runtime - stated) < ownGap).minByOption(cut => math.abs(cut.runtime - stated))
  }
}
