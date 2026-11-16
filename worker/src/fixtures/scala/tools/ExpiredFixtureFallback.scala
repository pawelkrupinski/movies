package tools

import settings.FixtureRoot

import java.nio.file.{Files, Path, StandardCopyOption}
import scala.util.control.NonFatal

/**
 * A recording's live leg that keeps a site's previous answer when the site fails it.
 *
 * [[EnrichmentFreshness.prune]] shelves the responses past their lifetime before a recording reads the tree, so the
 * live leg refetches them. When that refetch fails (the site down, as Kino Muza was for everyone in Record scrape
 * fixtures #186), the shelved response moves back into the tree, where the recording publishes it and every replay
 * reads it, and it is served. The file keeps its old mtime, so it is still expired and the next recording asks the site
 * again. A failure with nothing shelved fails the way the site did.
 *
 * Recording runs only: a hermetic replay asks no site, and serving a shelf there would hide a gap in the tree.
 */
final class ExpiredFixtureFallback(live: HttpFetch, shelf: Path, tree: Path) extends HttpFetch {
  private val shelved = new clients.tools.FakeHttpFetch(shelf.getFileName.toString, strict = true, foldYear = false,
    root = FixtureRoot(shelf.toAbsolutePath.getParent))
  private val inTree  = new clients.tools.FakeHttpFetch(tree.getFileName.toString, strict = true, foldYear = false,
    root = FixtureRoot(tree.toAbsolutePath.getParent))
  /** The tree files this fallback moved back, so a later failure of the same request reads the one already restored. */
  private val restored = scala.collection.mutable.Set.empty[Path]

  override def get(url: String): String = new String(getBytes(url), "UTF-8")

  override def getBytes(url: String): Array[Byte] = orShelved(url, None)(live.getBytes(url))

  override def get(url: String, headers: Map[String, String]): String =
    new String(orShelved(url, None)(live.get(url, headers).getBytes("UTF-8")), "UTF-8")

  override def post(url: String, body: String, contentType: String): String =
    new String(orShelved(url, Some(body))(live.post(url, body, contentType).getBytes("UTF-8")), "UTF-8")

  private def orShelved(url: String, body: Option[String])(fetch: => Array[Byte]): Array[Byte] =
    try fetch
    catch {
      case NonFatal(failure) => kept(url, body).map(Files.readAllBytes).getOrElse(throw failure)
    }

  /** The request's previous answer, in the tree: the shelved file moved back (a move keeps its mtime), or the one an
   *  earlier failure of it already moved back. Two passes failing the same request -- at once, or one after the other
   *  before the recorder above has written the tree -- both read it: the shelf no longer holds it for the second. */
  private def kept(url: String, body: Option[String]): Option[Path] = synchronized {
    shelved.fileFor(url, body).map { file =>
      val target = tree.resolve(shelf.toAbsolutePath.toRealPath().relativize(file.toAbsolutePath))
      Files.createDirectories(target.getParent)
      Files.move(file, target, StandardCopyOption.ATOMIC_MOVE)
      restored += canonical(target)
      target
    }.orElse(inTree.fileFor(url, body).filter(file => restored(canonical(file))))
  }

  private def canonical(file: Path): Path = file.toRealPath()
}
