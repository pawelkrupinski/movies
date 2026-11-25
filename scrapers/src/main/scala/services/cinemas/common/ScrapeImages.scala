package services.cinemas.common

import scala.jdk.CollectionConverters.*

import org.jsoup.nodes.{Document, Element}

/** Image and poster URLs read off a cinema page. */
private[cinemas] object ScrapeImages {

  private val CssUrl     = """url\((?:'|"|&quot;)?(.+?)(?:'|"|&quot;)?\)""".r

  /** The URL inside a CSS `url(...)` value, unwrapping `'`, `"` or `&quot;`
    * quoting. `None` when `s` holds no `url(...)`. */
  def cssUrl(s: String): Option[String] =
    CssUrl.findFirstMatchIn(s).map(_.group(1))

  /** The absolute URL an image element's `attribute` names, or `None` when the attribute is missing
    * or blank. jsoup's `abs:` resolves a BLANK value to the page's own URL, so `<img src="">` (every
    * Savoy box office's placeholder for a film without artwork) read as a "poster" that is the
    * venue's HTML page: never an image, and the poster download retried it until it gave up. A value
    * naming a directory rather than a file ([[namesAFile]]) is no image either. */
  def imageUrl(img: Element, attribute: String = "src"): Option[String] =
    Option(img.attr(attribute).trim).filter(_.nonEmpty).map(_ => img.absUrl(attribute)).filter(namesAFile)

  /** The largest image `img` offers, as its markup spells it: its `srcset`'s widest candidate (by `w`, else by `x`
    * density), else its `src`; `None` when both are blank. A theme's `src` is often the smallest copy it cut
    * (WordPress's `-300x450`, a `/thumbs/` one) while `srcset` names the full upload beside it. Relative as written:
    * the caller resolves it as it resolves `src`. */
  def largestImageSource(img: Element): Option[String] =
    Srcset.widest(img.attr("srcset")).orElse(Option(img.attr("src").trim).filter(_.nonEmpty))

  /** [[largestImageSource]] as an absolute URL naming a file, as [[imageUrl]] reads `src`. */
  def largestImageUrl(img: Element): Option[String] =
    Srcset.widest(img.attr("srcset")).map(org.jsoup.internal.StringUtil.resolve(img.baseUri, _)).filter(namesAFile)
      .orElse(imageUrl(img))

  /** Whether an absolute URL's path names a file: not empty and not ending in `/`. A CMS that
    * leaves a film's image name blank prints its upload directory (`…/tmdbimage/images/` on Nova
    * Cinema), which serves a page or a 404, never artwork. The path is read off the URL's shape, not
    * through `java.net.URI`, which refuses the unencoded space of a file name like Kino Kika's
    * `/uploads/event/3567/FMSB plakat PL_HQ.jpg` (as browsers fetch it, so do we). */
  def namesAFile(url: String): Boolean = url match {
    case AbsoluteUrlPath(path) => path.nonEmpty && !path.endsWith("/")
    case _                     => false
  }

  /** An absolute URL's path: after `scheme://authority`, up to its query or fragment. */
  private val AbsoluteUrlPath = """(?s)[A-Za-z][A-Za-z0-9+.-]*://[^/?#]*([^?#]*)(?:[?#].*)?""".r

  /** The page's first `og:image` without the shape of a SITE-WIDE default rather than this film's
    * poster ([[tools.SiteDefaultImage]]). A page may carry several: every bilety24 venue site prints
    * its default svg first and the event's poster after it. */
  def ogImage(document: Document): Option[String] =
    document.select("meta[property=og:image]").asScala.iterator.map(_.attr("content").trim)
      .find(url => url.nonEmpty && !tools.SiteDefaultImage(url))
}
