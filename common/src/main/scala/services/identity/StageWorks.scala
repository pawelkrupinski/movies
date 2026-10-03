package services.identity

import scala.io.Source

/** The stage works — operas and ballets — a season broadcast is billed by, each under every name it goes by
 *  (`identity-stage-works.tsv`, built from Wikidata by `scripts/identity-stage-works.py`, and the hand-kept
 *  `identity-stage-works-extra.tsv`): cinemas show one season's "Royal Ballet & Opera 2026/27: The Nutcracker" as
 *  "Dziadek do orzechów", "Der Nussknacker" or "El cascanueces", so a season production is read by its WORK, whatever
 *  language names it. One table for every country. A name several works share ("Armida") names them all.
 *  @param works   each name's key ([[IdentityMeasures.key]]) → the works it names
 *  @param english each work → the name a title search asks for it by (its English label, else another) */
final class StageWorks(works: Map[String, Set[String]], english: Map[String, String]) {
  /** The works a title piece names, by its key. */
  def named(key: String): Set[String] = works.getOrElse(key, Set.empty)
  /** The name a search asks for `work` by. */
  def searchName(work: String): Option[String] = english.get(work)
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
    new StageWorks(byName, rows.collect { case (work, Some(englishName), _) => work -> englishName }.toMap)
  }

  private def lines(path: String): Iterator[String] =
    Option(getClass.getClassLoader.getResourceAsStream(path)).fold(Iterator.empty[String]) { in =>
      try Source.fromInputStream(in, "UTF-8").getLines().toVector.iterator finally in.close()
    }

  /** The table on the classpath, the extra names beside the generated ones. */
  lazy val resolver: StageWorks = parse(lines(Resource) ++ lines(Extra))
}
