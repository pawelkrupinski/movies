package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The case of [[TextNormalizationSpec]] that runs a child JVM under a Turkish default locale (its `TurkishSentenceCase`). */
class TextNormalizationIntegrationSpec extends AnyFlatSpec with Matchers {

  private lazy val harness = settings.HarnessConfiguration.resolve()

  "sentenceCase" should "be independent of the JVM default locale (no Turkish dotless-i)" in {
    // Regression: `sentenceCase` used the no-arg `String.toLowerCase`, which folds
    // case against the DEFAULT locale. Under a Turkish default the ASCII 'I' maps
    // to dotless 'ı', so "FILMOWE SPOTKANIA" came out "Fılmowe spotkanıa" — a
    // platform-dependent title that diverged between dev (en) and any tr-locale
    // JVM. Every sibling helper here pins `Locale.ROOT`; this one must too.
    //
    // In a Turkish JVM of its own: switching THIS JVM's default would turn every
    // default-locale case fold in the suites running beside it Turkish as well.
    val (exit, output) = ChildJvm(harness).run("tools.TurkishSentenceCase", Seq("FILMOWE SPOTKANIA"),
                                      jvmArgs = Seq("-Duser.language=tr", "-Duser.country=TR"))
    (exit, output.trim) shouldBe ((0, "tr: Filmowe spotkania"))
  }
}

/** `sentenceCase` of its argument, prefixed with the JVM's default language -- proof the child
 *  really ran under the locale it was started with. */
object TurkishSentenceCase {
  def main(args: Array[String]): Unit =
    println(s"${java.util.Locale.getDefault.getLanguage}: ${TextNormalization.sentenceCase(args.head)}")

}
