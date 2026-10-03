package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/**
 * Untrusted data reaches the web's pages only through the two vetted doors.
 *
 *  - Inline JSON goes through `controllers.ScriptJson`, which escapes `<` so a scraped
 *    cinema name or synopsis holding `</script>` cannot close the element. A template's
 *    `Html(…)` is a raw splice that skips Twirl's escaping; every one is either fed by
 *    `ScriptJson` or listed below with why its text is ours.
 *  - A data-driven `href`/`src` is a `controllers.WebHref`, whose only constructor
 *    rejects anything but http(s) — a scraped `javascript:` or `data:` URL in an `href`
 *    runs as our origin when clicked. An attribute VALUE that starts with a Scala
 *    expression is either an app route (`routes.*`, `FilmHref`, `BrowseHref`) or listed
 *    below, where the entry says which `WebHref`-typed value it is or why it is ours.
 *  - HTML the web builds in Scala (`ShowingsMarkup`, the streamed card fragments)
 *    writes its links by hand; each such file is listed with what vets its URLs.
 *
 * Every list is checked for stale entries, so a fixed site has to leave it.
 */
class RenderSafetyLintSpec extends AnyFlatSpec with Matchers {

  import RenderSafetyLintSpec._
  import ScalaSourceScan._

  "the template scanner" should "find raw splices and JSON serialisation, but not inside a Twirl comment" in {
    rawSplices("""<p>@Html(film.synopsis)</p>""") shouldBe Seq("film.synopsis")
    rawSplices("""@HtmlFormat.raw(x)""") shouldBe Seq("x")
    rawSplices("""@controllers.ScriptJson.embedSerialized(json)""") shouldBe empty
    rawSplices("""@* an old `@Html(x)` *@<p>@x</p>""") shouldBe empty
    serialisations("""const A = @Html(Json.stringify(a));""") shouldBe Seq("Json.stringify")
    serialisations("""const A = @controllers.ScriptJson.embed(Json.toJson(a));""") shouldBe empty
  }

  it should "find attribute values that start with an expression, skipping app routes and literal prefixes" in {
    expressionUrls("""<a href="@slot.bookingUrl">""") shouldBe Seq("slot.bookingUrl")
    expressionUrls("""<img src="@{u + "x"}">""") shouldBe Seq("{u + \"x\"}")
    expressionUrls("""<a href="@e.metacriticUrl(1).trim" class="x">""") shouldBe Seq("e.metacriticUrl(1).trim")
    expressionUrls("""<a href="@routes.MovieController.index(city.slug)">""") shouldBe empty
    expressionUrls("""<a href="@FilmHref.forSlug(slug, movie.title)">""") shouldBe empty
    expressionUrls("""<a href="?film=@id">""") shouldBe empty
    expressionUrls("""<a@for(href <- WebHref.of(u)){ href="@href"}>""") shouldBe Seq("href")
  }

  "Twirl templates" should "splice raw HTML only from ScriptJson or a listed source" in {
    val found = sites(templates)(rawSplices)
    withClue("Feed inline JSON through controllers.ScriptJson; render other data through Twirl's escaping: ") {
      unlisted(found, AllowedRawSplices) shouldBe empty
    }
    withClue("No longer in the source — drop from AllowedRawSplices: ")(stale(found, AllowedRawSplices) shouldBe empty)
  }

  they should "never serialise JSON except through ScriptJson" in {
    sites(templates)(serialisations) shouldBe empty
  }

  they should "take every data-driven href and src from a WebHref or an app route" in {
    val found = sites(templates)(expressionUrls)
    withClue("Pass these as controllers.WebHref (WebHref.of rejects javascript:/data: URLs): ") {
      unlisted(found, AllowedExpressionUrls) shouldBe empty
    }
    withClue("No longer in the source — drop from AllowedExpressionUrls: ")(stale(found, AllowedExpressionUrls) shouldBe empty)
  }

  "Web Scala sources" should "build raw HTML and hand-written links only where listed" in {
    val scala = scalaFiles(Seq("web/src/main/scala"))
    val found = sites(scala)(source => rawSplices(source) ++ handWrittenLinks(source))
    unlisted(found, AllowedScalaMarkup) shouldBe empty
    withClue("No longer in the source — drop from AllowedScalaMarkup: ")(stale(found, AllowedScalaMarkup) shouldBe empty)
  }

  "the dev-only exemption" should "name only templates that exist" in {
    val names = twirlFiles(Seq(Views)).map(_.getFileName.toString).toSet
    DevOnlyTemplates.keySet.diff(names) shouldBe empty
  }

  private def templates: Seq[Path] =
    twirlFiles(Seq(Views)).filterNot(path => DevOnlyTemplates.contains(path.getFileName.toString))
}

object RenderSafetyLintSpec {

  private val Views = "web/src/main/twirl"

  /** A site: the file it is in (by name) and the expression it splices. */
  type Site = (String, String)

  /** Templates served only behind `DevMode.gate` (a production deployment 404s them), so
   *  the data they show never reaches a visitor. Name → why. */
  val DevOnlyTemplates: Map[String, String] = Map(
    "debug.scala.html"                    -> "/debug — DebugController.devOnly",
    "debugDetails.scala.html"             -> "/debug/:id — DebugController.devOnly",
    "debugReadModel.scala.html"           -> "/debug/readmodel — DebugController.devOnly",
    "debugReadModelScreenings.scala.html" -> "/debug/readmodel/:id — DebugController.devOnly",
    "_debugRow.scala.html"                -> "rendered only into debug.scala.html",
  )

  /** Raw `Html(…)` splices in (non-dev) templates whose text is ours. (file, argument) → why. */
  val AllowedRawSplices: Map[Site, String] = Map(
    ("_minified.scala.html", "minifier.process(content.body)") ->
      "re-emits a template's own already-rendered (already-escaped) body after minifying it",
    ("_loginModal.scala.html", "messages(\"login.nag\")") ->
      "an i18n message we author (it carries a <strong>); no data is interpolated into it",
    ("_filmDetailContent.scala.html", "tools.SynopsisMarkdown.toInlineHtml(text)") ->
      "SynopsisMarkdown HTML-escapes the prose before it adds any tag (SynopsisMarkdownSpec)",
  )

  /** Attribute values starting with an expression that is not an app route. (file, expression) → why. */
  val AllowedExpressionUrls: Map[Site, String] = Map(
    ("_ratingBadges.scala.html", "href")             -> "a WebHref (`for (href <- WebHref.of(…))`)",
    ("_serviceNameLink.scala.html", "u")             -> "a WebHref (the template's `url: Option[WebHref]`)",
    ("_filmDetailContent.scala.html", "url")         -> "a WebHref (FilmSchedule.linkableCinemaFilmUrls)",
    ("_moviePoster.scala.html", "proxied")           -> "a WebHref (PosterProxy.proxyPoster of the template's `poster: Option[WebHref]`)",
    ("_filmDetailContent.scala.html", "href")        -> "an app route: FilmHref of a sibling city (MovieController.otherCityLinks)",
    ("film.scala.html", "canonicalUrl")              -> "an app URL: this page's own address (MovieController.film)",
    ("_ogTagsApp.scala.html", "{if(canonicalUrl.nonEmpty) canonicalUrl else pageUrl}") ->
      "an app URL: the page's own address, passed by the controller that renders it",
    ("_errorTracking.scala.html", "url.value")       -> "configuration: the error-tracker loader URL from the deployment's settings",
    ("landing.scala.html", "{c.webUrl.get}")         -> "configuration: a country's own web origin (models.Country)",
    ("_appBanner.scala.html", "models.ClientSupport.ios.storeUrl.getOrElse(\"#\")") ->
      "configuration: the App Store URL, validated as https in models.ClientSupport",
    ("_appBanner.scala.html", "models.ClientSupport.android.storeUrl.getOrElse(\"#\")") ->
      "configuration: the Play Store URL, validated as https in models.ClientSupport",
    ("ssoLogoutConfirm.scala.html", "signOutHref")   -> "an app route plus a `next` AuthController.ssoLogoutNext validated",
    ("ssoLogoutConfirm.scala.html", "stayHref")      -> "AuthController.ssoLogoutOnward: a validated `next` or our own root",
  )

  /** Web Scala that builds raw HTML or writes an `href`/`src` by hand. (file, site) → why. */
  val AllowedScalaMarkup: Map[Site, String] = Map(
    ("ScriptJson.scala", "stringify(value)")  -> "the vetted door itself",
    ("ScriptJson.scala", "escape(json)")      -> "the vetted door itself",
    ("Minifier.scala", "process(content.body)") -> "re-emits an already-rendered template body, minified",
    ("ResponseBody.scala", "Nil")             -> "StreamedHtml/PrewrittenHtml extend an empty Html; their text is their producer's",
    ("ResponseBody.scala", "List(streamed)")  -> "wraps a StreamedHtml so a template passes it through; the text is its producer's",
    ("FilmCardFragments.scala", "List(new PrewrittenHtml(text))") ->
      "a card's own rendered (escaped) template markup, cached and re-emitted whole",
    ("ShowingsMarkup.scala", "List(new StreamedHtml((out, flush) =>") ->
      "the showings tree ShowingsMarkup writes itself, every value through escapeInto",
    ("ShowingsMarkup.scala", "href")          ->
      "cinema links are WebHrefs (linkableCinemaFilmUrls); booking pills keep only WebHref.accepts URLs (linkableOnly); every value goes through escapeInto",
  )

  private val TwirlComment = """(?s)@\*.*?\*@""".r
  private val RawSplice    = """(?<![\w.])(?:play\.twirl\.api\.)?(?:Html|HtmlFormat\.raw)\(""".r
  private val ScriptJsonCall = """ScriptJson\.\w+\(""".r
  private val Serialise    = """Json\.(?:stringify|toJson|prettyPrint|asciiStringify)""".r
  private val UrlAttribute = """\b(?:href|src|action|formaction|poster)="@""".r
  private val AppRoute     = """^(?:controllers\.)?(?:routes\.|FilmHref\b|BrowseHref\b)""".r
  private val HandLink     = """(?:href|src)=\\"""".r

  private def uncommented(template: String): String = TwirlComment.replaceAllIn(template, "")

  /** The argument of every raw `Html(…)` / `HtmlFormat.raw(…)` in a template or source — its
   *  first line, which names it well enough to list. */
  def rawSplices(source: String): Seq[String] = {
    val text = uncommented(source)
    RawSplice.findAllMatchIn(text).map(m => ScalaSourceScan.argumentsAt(text, m.end - 1).trim.linesIterator.next().trim).toSeq
  }

  /** Every JSON serialisation in a template that is not an argument of `ScriptJson`. */
  def serialisations(template: String): Seq[String] = {
    val text = uncommented(template)
    val vetted = ScriptJsonCall.findAllMatchIn(text).map(m => m.end until ScalaSourceScan.closingParen(text, m.end - 1)).toSeq
    Serialise.findAllMatchIn(text).filterNot(m => vetted.exists(_.contains(m.start))).map(_.matched).toSeq
  }

  /** The expression of every URL attribute whose value starts with one, app routes excepted. */
  def expressionUrls(template: String): Seq[String] = {
    val text = uncommented(template)
    UrlAttribute.findAllMatchIn(text).map(m => expressionAt(text, m.end)).filter(AppRoute.findFirstIn(_).isEmpty).toSeq
  }

  /** Each `href=\"` / `src=\"` written into a Scala string — a hand-built link. */
  def handWrittenLinks(source: String): Seq[String] =
    HandLink.findAllMatchIn(source).map(_ => "href").toSeq

  /** The Twirl expression starting at `from`: a `{…}` block, or a dotted chain whose
   *  segments may carry `(…)` arguments. */
  private def expressionAt(text: String, from: Int): String =
    if (text(from) == '{') text.substring(from, closing(text, from, '{', '}') + 1)
    else {
      var i = from
      var continue = true
      while (continue && i < text.length) {
        val c = text(i)
        if (c.isLetterOrDigit || c == '_' || c == '.') i += 1
        else if (c == '(') i = closing(text, i, '(', ')') + 1
        else continue = false
      }
      text.substring(from, i).stripSuffix(".")
    }

  private def closing(text: String, open: Int, opening: Char, closer: Char): Int = {
    var depth = 0
    var i     = open
    while (i < text.length) {
      val c = text(i)
      if (c == opening) depth += 1
      else if (c == closer) { depth -= 1; if (depth == 0) return i }
      i += 1
    }
    text.length - 1
  }

  private def sites(files: Seq[Path])(find: String => Seq[String]): Seq[Site] =
    files.flatMap(path => find(ScalaSourceScan.read(path)).map(path.getFileName.toString -> _)).distinct

  private def unlisted(found: Seq[Site], allowed: Map[Site, String]): Seq[Site] = found.filterNot(allowed.contains)

  private def stale(found: Seq[Site], allowed: Map[Site, String]): Set[Site] = allowed.keySet.diff(found.toSet)
}
