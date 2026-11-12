package models

import scala.io.{Codec, Source}
import scala.util.Using

/** A generated table checked in as a tab-separated classpath resource under `generated/` — what the
 *  `data/**/scripts/generate_*.py` generators write (`data/scripts/generated_tables.py`) instead of Scala
 *  source, so no table is split into numbered chunks to stay under the JVM's 64 KB method limit or the
 *  compiler's nesting depth. One row a line, cells split on tabs; a list cell joins its items with
 *  [[ListSeparator]], and an absent optional cell is empty. The generators refuse a cell holding a tab,
 *  a newline or (in a list) the separator, so the split is exact. */
object GeneratedTable {
  val ListSeparator: Char = '|'

  /** Every row of resource `path`, as its cells. A missing resource fails loudly: it is a build fault. */
  def rows(path: String): IndexedSeq[IndexedSeq[String]] = {
    val stream = Option(getClass.getClassLoader.getResourceAsStream(path))
      .getOrElse(throw new IllegalStateException(s"generated table $path is not on the classpath"))
    Using.resource(Source.fromInputStream(stream)(using Codec.UTF8))(
      _.getLines().filter(_.nonEmpty).map(_.split("\t", -1).toIndexedSeq).toIndexedSeq)
  }

  /** A two-column table: first column → second. */
  def lookup(path: String): Map[String, String] =
    rows(path).map(row => row(0) -> row(1)).toMap

  /** A list cell's items; an empty cell is no items. */
  def list(cell: String): Seq[String] =
    if (cell.isEmpty) Seq.empty else cell.split(ListSeparator).toSeq

  /** An optional cell: empty is `None`. */
  def option(cell: String): Option[String] = Option(cell).filter(_.nonEmpty)
}
