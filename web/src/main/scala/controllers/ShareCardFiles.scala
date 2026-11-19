package controllers

import models.Country
import settings.ShareCardDirectoryTemplate

import java.nio.file.{Files, Path}

/** The share cards this process can read off its own disk: a dev server's copy of what the worker draws, under
 *  `KINOWO_SHARE_CARD_DIR` ([[ShareCardDirectoryTemplate]]; `scripts/dev-server.sh` points it at
 *  `~/.cache/kinowo/share-cards/{cc}`). Production never reads them here — Caddy serves `/share-cards/` itself;
 *  only the dev-only gallery ([[ShareCardGalleryController]]) does. */
final class ShareCardFiles(directory: ShareCardDirectoryTemplate) {

  /** Where `country`'s cards are looked for. */
  def directoryOf(country: Country): Path = directory.forCountry(country)

  /** The card `file` of `country` when it is on disk. Only a bare `<name>.jpg` is looked up, so no request reaches
   *  a path outside the country's directory. */
  def find(country: Country, file: String): Option[Path] =
    Option.when(ShareCardFiles.CardFileName.matches(file))(directoryOf(country).resolve(file)).filter(Files.isRegularFile(_))
}

object ShareCardFiles {
  private val CardFileName = "[A-Za-z0-9_-]+\\.jpg".r

  /** A film's `shareCard` (`<film>.jpg?v=<version>`) as its file name. */
  def fileOf(shareCard: String): String = shareCard.takeWhile(_ != '?')
}
