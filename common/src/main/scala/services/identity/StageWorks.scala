package services.identity

import scala.io.Source

/** The stage works — operas and ballets — a season broadcast is billed by, each under every name it goes by
 *  (`identity-stage-works.tsv`, built from Wikidata by `scripts/identity-stage-works.py`, and the hand-kept
 *  `identity-stage-works-extra.tsv`): cinemas show one season's "Royal Ballet & Opera 2026/27: The Nutcracker" as
 *  "Dziadek do orzechów", "Der Nussknacker" or "El cascanueces", so a season production is read by its WORK, whatever
 *  language names it. One table for every country. A name several works share ("Armida") names them all.
 *  @param works   each name's key ([[IdentityMeasures.key]]) → the works it names
 *  @param english each work → the name a title search asks for it by (its English label, else another) */
final class StageWorks(works: Map[String, Set[String]], english: Map[String, String], names: Map[String, Seq[String]] = Map.empty) {
  /** The works a title piece names, by its key. */
  def named(key: String): Set[String] = works.getOrElse(key, Set.empty)
  /** The name a search asks for `work` by. */
  def searchName(work: String): Option[String] = english.get(work)
  /** Every name a search asks for `work` by: its English one first, then each other in the Latin script, one per
   *  spelling ([[IdentityMeasures.key]]) — a house files its record under the work's original name ("The Metropolitan
   *  Opera 2026/27: Samson et Dalila"), which neither the venue's "Samson i Dalila" nor "Samson and Delilah" finds. Only
   *  a name sharing a word with the English one: Wikidata files a work's arias and characters among its names too
   *  ("Escamillo", "Gypsy Song" of "Carmen"), which name other films. */
  def searchNames(work: String): Seq[String] = english.get(work).toSeq.flatMap { name =>
    val words = StageWorks.words(name)
    (name +: names.getOrElse(work, Nil).filter(other => StageWorks.latin(other) && StageWorks.words(other).exists(words)))
      .distinctBy(IdentityMeasures.key).filter(n => IdentityMeasures.key(n).nonEmpty)
  }
  def size: Int = english.size
}

object StageWorks {
  val Resource = "identity-stage-works.tsv"
  val Extra    = "identity-stage-works-extra.tsv"

  /** Lines `<work>\t<English name>\t<name>|<name>…` (the generated table) or `<work>\t<name>|<name>…` (the extra). */
  def parse(lines: Iterator[String]): StageWorks = {
    val rows = lines.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#")).map(_.split("\t")).collect {
      case Array(work, englishName, names) => (work, Some(englishName), names.split('|').toSeq :+ englishName)
      case Array(work, names)              => (work, None, names.split('|').toSeq)
    }.toSeq
    val byName = rows.flatMap { case (work, _, names) => names.map(IdentityMeasures.key).filter(_.nonEmpty).map(_ -> work) }
      .groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    new StageWorks(byName, rows.collect { case (work, Some(englishName), _) => work -> englishName }.toMap,
      rows.groupMapReduce(_._1)(_._3)(_ ++ _).view.mapValues(_.map(_.trim).filter(_.nonEmpty).distinct.sorted).toMap)
  }

  /** Is every letter of `name` a Latin one: a name a Latin-script title search can find? */
  private def words(name: String): Set[String] = services.movies.TitleContainment.tokens(name).filter(_.length >= 4).toSet

  private def latin(name: String): Boolean =
    name.exists(_.isLetter) && name.filter(_.isLetter).forall(c => Character.UnicodeScript.of(c.toInt) == Character.UnicodeScript.LATIN)

  private def lines(path: String): Iterator[String] =
    Option(getClass.getClassLoader.getResourceAsStream(path)).fold(Iterator.empty[String]) { in =>
      try Source.fromInputStream(in, "UTF-8").getLines().toVector.iterator finally in.close()
    }

  /** The table on the classpath, the extra names beside the generated ones. */
  lazy val resolver: StageWorks = parse(lines(Resource) ++ lines(Extra))
}
