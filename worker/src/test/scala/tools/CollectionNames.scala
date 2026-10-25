package tools

import java.nio.file.Path

import ScalaSourceScan.{MainRoots, codeOf, scalaFiles}

/**
 * The Mongo collection names sources open, read from the shapes they spell them in — for the registry's lints
 * (`CollectionRetentionSpec`, `RegisteredCollectionListsSpec`): a literal handed to `getCollection`, a `…Collection…`
 * constant, a `collection`/`collectionName` parameter default, an index helper's collection argument, the resolution
 * caches' and the TMDB store's names; and, for what a given program opens ([[openedFrom]]), a qualified constant
 * (`MongoVenuePageStore.Collection`, `TmdbKind.Family.collection`) and a store's constructor (`new MongoPinStore(…)`:
 * every name the store's own file opens).
 */
object CollectionNames {

  private val Spellings = Seq(
    """getCollection(?:\[[^(]*?\])?\(\s*"([^"]+)"""",
    """\bval\s+\w*Collection\w*\s*(?::\s*String)?\s*=\s*"([^"]+)"""",
    """\b(?:collection|collectionName)\s*:\s*String\s*=\s*"([^"]+)"""",
    """\bMongoIndex\s*\.\s*\w+\(\s*\w+\s*,\s*"([^"]+)"""",
    """\bresolutionCache\(\s*"([^"]+)"""",
    """\bextends\s+TmdbKind\(\s*"([^"]+)"""").map(_.r)

  /** Every name `source` spells literally. */
  def namesIn(source: String): Set[String] = Spellings.flatMap(_.findAllMatchIn(source).map(_.group(1))).toSet

  private val Fixtures = Seq("worker/src/fixtures")

  private lazy val sources: Map[Path, String] = scalaFiles(MainRoots ++ Fixtures).map(p => p -> codeOf(p)).toMap

  /** Every name main sources (and the recording's fixtures programs) spell. */
  lazy val named: Set[String] = sources.values.flatMap(namesIn).toSet

  private val ObjectHeader = """\b(?:object|enum)\s+(\w+)""".r
  private val Constant     = """\bval\s+(\w*Collection\w*)\s*(?::\s*String)?\s*=\s*"([^"]+)"""".r
  private val KindCase     = """\bcase\s+(\w+)\s+extends\s+(\w+)\(\s*"([^"]+)"""".r

  /** `Owner.Constant` -> the name it holds, for every collection constant an object or enum defines (an object nested
   *  in its owner's body counts as neither's: the constant is filed under every object opened before it, the nearest
   *  first). */
  private lazy val constants: Map[String, String] = sources.values.flatMap { source =>
    val owners = ObjectHeader.findAllMatchIn(source).map(m => m.start -> m.group(1)).toSeq
    Constant.findAllMatchIn(source).flatMap { m =>
      owners.takeWhile(_._1 < m.start).reverse.zipWithIndex.map { case ((_, owner), nearness) => (s"$owner.${m.group(1)}", nearness, m.group(2)) }
    } ++ KindCase.findAllMatchIn(source).map(m => (s"${m.group(2)}.${m.group(1)}.collection", 0, m.group(3)))
  }.toSeq.groupBy(_._1).map { case (key, filed) => key -> filed.minBy(_._2)._3 }

  private val Qualified = """\b([A-Z]\w*(?:\.[A-Z]\w*)?\.(?:\w*Collection\w*|collection))\b""".r
  private val Store     = """\bnew\s+(Mongo\w+)\s*[(\[]""".r

  /** The names `source` opens itself: what it spells and the constants it qualifies. */
  private def ownNames(source: String): Set[String] =
    namesIn(source) ++ Qualified.findAllMatchIn(source).flatMap(m => constants.get(m.group(1).split('.').takeRight(3).mkString(".")).orElse(
      constants.get(m.group(1).split('.').takeRight(2).mkString(".")))).toSet

  /** The names the program in `files` opens: its own, and for each store it constructs the collection its arguments name,
   *  else every name the file defining the store opens. */
  def openedFrom(files: Seq[Path]): Set[String] = files.map(codeOf).flatMap { source =>
    val stores = Store.findAllMatchIn(source).map { m =>
      val open = source.indexOf('(', m.start)
      m.group(1) -> ScalaSourceScan.argumentsAt(source, open)
    }.toSeq
    ownNames(source) ++ stores.flatMap { case (store, arguments) =>
      val named = ownNames(arguments)
      if (named.nonEmpty) named else sources.values.filter(s => classOf(store).findFirstIn(s).isDefined).flatMap(ownNames)
    }
  }.toSet

  private def classOf(name: String) = s"""\\bclass\\s+$name\\b""".r
}
