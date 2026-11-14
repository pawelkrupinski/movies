package tools

import settings.FixtureRoot

import java.nio.file.{Files, NoSuchFileException, Path, StandardCopyOption}
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

  override def get(url: String): String = new String(getBytes(url), "UTF-8")

  override def getBytes(url: String): Array[Byte] = orShelved(url, None)(live.getBytes(url))

  override def get(url: String, headers: Map[String, String]): String =
    new String(orShelved(url, None)(live.get(url, headers).getBytes("UTF-8")), "UTF-8")

  override def post(url: String, body: String, contentType: String): String =
    new String(orShelved(url, Some(body))(live.post(url, body, contentType).getBytes("UTF-8")), "UTF-8")

  private def orShelved(url: String, body: Option[String])(fetch: => Array[Byte]): Array[Byte] =
    try fetch
    catch {
      case NonFatal(failure) =>
        shelved.fileFor(url, body).map(restore).getOrElse(throw failure)
    }

  /** Moves the shelved file back to its place in the tree (a move keeps its mtime) and reads it. Two passes asking at
   *  once: the second finds it already moved and reads it where the first put it. */
  private def restore(file: Path): Array[Byte] = {
    val target = tree.resolve(shelf.toAbsolutePath.toRealPath().relativize(file.toAbsolutePath))
    try {
      Files.createDirectories(target.getParent)
      Files.move(file, target, StandardCopyOption.ATOMIC_MOVE)
    } catch { case _: NoSuchFileException => () }
    Files.readAllBytes(target)
  }
}
