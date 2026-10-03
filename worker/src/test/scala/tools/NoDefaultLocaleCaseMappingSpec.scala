package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Production code never case-maps a `String` in the JVM's default locale.
 *
 * A bare `toLowerCase` / `toUpperCase` reads `Locale.getDefault`: under a Turkish default
 * `"TITLE".toLowerCase` is `"tıtle"` (dotless ı) and `"title".toUpperCase` is `"TİTLE"`, so a
 * title key, a host name or a slug stops matching its own copy. Production is safe only because
 * its image happens to export `LANG=en_US`; a dev Mac, a CI runner or a future base image need
 * not. `Locale.ROOT` is what every such call means — the casing of identifiers, not of prose.
 * A call that does mean a language's rules names that locale explicitly, which passes too.
 *
 * `Character.toLowerCase(c)` takes an argument and is locale-independent, so it is not matched.
 */
class NoDefaultLocaleCaseMappingSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan._

  /** `.toLowerCase` / `.toUpperCase` with no argument (bare or `()`): the default-locale overloads. */
  private val DefaultLocaleCaseMapping = """\.to(?:Lower|Upper)Case\b(?!\s*\((?!\s*\)))""".r

  private def bareCalls(source: String): Seq[Int] =
    source.linesIterator.zipWithIndex.collect {
      case (line, index) if DefaultLocaleCaseMapping.findFirstIn(code(line)).isDefined => index + 1
    }.toSeq

  "the detector" should "flag a bare or empty-parenthesised call and pass an explicit locale" in {
    bareCalls("""val a = title.toLowerCase""") shouldBe Seq(1)
    bareCalls("""val a = names.map(_.toUpperCase)""") shouldBe Seq(1)
    bareCalls("""val a = title.toLowerCase().trim""") shouldBe Seq(1)
    bareCalls("""val a = title.toLowerCase(Locale.ROOT)""") shouldBe empty
    bareCalls("""val a = title.toUpperCase(polish)""") shouldBe empty
    bareCalls("""val a = Character.toUpperCase(c)""") shouldBe empty
    bareCalls("""// title.toLowerCase in a comment""") shouldBe empty
  }

  /** In a Twirl template only the paren-less form: an inline script's JS `s.toLowerCase()`
   *  needs its parentheses and is locale-independent, while a Scala `@(…toLowerCase)` drops
   *  them — so a template's `toLowerCase()` is left alone rather than misread as Scala. */
  private val TemplateDefaultLocaleCaseMapping = """\.to(?:Lower|Upper)Case\b(?!\s*\()""".r

  private def bareTemplateCalls(source: String): Seq[Int] =
    source.linesIterator.zipWithIndex.collect {
      case (line, index) if TemplateDefaultLocaleCaseMapping.findFirstIn(line).isDefined => index + 1
    }.toSeq

  "the template detector" should "flag a paren-less Scala call and pass JS calls and explicit locales" in {
    bareTemplateCalls("""data-haystack="@{(title + orig).toLowerCase}"""") shouldBe Seq(1)
    bareTemplateCalls("""var q = input.value.trim().toLowerCase();""") shouldBe empty
    bareTemplateCalls("""@title.toLowerCase(java.util.Locale.ROOT)""") shouldBe empty
  }

  "Production templates" should "never case-map a String in the JVM's default locale" in {
    val offenders = twirlFiles(MainRoots).flatMap(path => bareTemplateCalls(read(path)).map(line => s"$path:$line"))
    withClue("Pass java.util.Locale.ROOT to these template toLowerCase/toUpperCase calls:\n" +
      offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }

  "Production sources" should "never case-map a String in the JVM's default locale" in {
    val offenders = scalaFiles(MainRoots).flatMap(path => bareCalls(read(path)).map(line => s"$path:$line"))
    withClue("Pass Locale.ROOT (or the language the text is in) to these toLowerCase/toUpperCase calls:\n" +
      offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }
}
