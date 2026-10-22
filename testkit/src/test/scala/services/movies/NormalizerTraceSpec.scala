package services.movies

import models.{Country, Helios}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{BufferedReader, InputStreamReader}
import java.io.{BufferedWriter, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

/** The footprint measure's proof (`scripts/identity-measure --footprint`): a trace hears every normaliser built while it
 *  is set, and the check reads every reading again under the rules it runs on, naming each one that reads otherwise. */
class NormalizerTraceSpec extends AnyFlatSpec with Matchers {

  private def traced(body: => Unit): Path = {
    val path = Files.createTempFile("normalizer-trace", ".jsonl.gz")
    NormalizerTrace.recording(Some(path))(body)
    path
  }
  private def lines(path: Path): Seq[String] = {
    val in = new BufferedReader(new InputStreamReader(new GZIPInputStream(Files.newInputStream(path)), UTF_8))
    try Iterator.continually(in.readLine()).takeWhile(_ != null).toSeq finally in.close()
  }
  private def rewritten(path: Path)(edit: String => String): Path = {
    val to  = Files.createTempFile("normalizer-trace-edited", ".jsonl.gz")
    val out = new BufferedWriter(new OutputStreamWriter(new GZIPOutputStream(Files.newOutputStream(to)), UTF_8))
    try lines(path).foreach(l => { out.write(edit(l)); out.write('\n') }) finally out.close()
    to
  }
  private def check(trace: Path) = NormalizerTraceCheck.check(Seq(trace), Files.createTempFile("differ", ".tsv"))

  "a trace" should "hear a normaliser built inside it, and every reading it makes, nested ones too" in {
    val trace = traced {
      val pl = TitleNormalizer.forCountry(Country.Poland)
      pl.sanitize("Gwiezdne Wojny: Nowa Nadzieja")
      pl.listingTitle(Helios, "Diuna 2D napisy")
      pl.searchQuery("KINO BEZ BARIER: Freak Show (AD + CC + PJM)")
    }
    val read = lines(trace)
    read.exists(l => l.contains("\"m\":\"sanitize\"") && l.contains("Gwiezdne Wojny: Nowa Nadzieja")) shouldBe true
    read.exists(l => l.contains("\"m\":\"listingTitle\"") && l.contains("Helios Posnania")) shouldBe true
    // searchQuery re-cases and strips through apiQuery: each nested reading is one more the rules could read otherwise
    read.exists(_.contains("\"m\":\"apiQuery\"")) shouldBe true
    read.forall(_.contains("\"c\":\"pl\"")) shouldBe true
  }

  it should "hear nothing once it ends" in {
    val pl    = TitleNormalizer.forCountry(Country.Poland)
    val trace = traced(pl.sanitize("Lalka"))
    pl.sanitize("Chłopi")
    lines(trace).exists(_.contains("Chłopi")) shouldBe false
  }

  "the check" should "read every reading alike under the same rules" in {
    val trace = traced {
      val uk = TitleNormalizer.forCountry(Country.UnitedKingdom)
      uk.sanitize("NT Live: Hamlet")
      uk.properNamed("NT Live: Hamlet")
      uk.titleFacts(Helios, "Digger (15)")
      uk.firedRules(Helios, "Digger (15)")
      uk.mergeKeyLookup(Seq("Top Gun 40th Anniversary", "Top gun | 40 rocznica"))("Top Gun 40th Anniversary")
      uk.chooseDisplay(Seq("ARNIE & BARNEY"), "arnie & barney", Some("Arnie & Barney"), Some(2024))
    }
    val outcome = check(trace)
    outcome.readings should be > 6L
    outcome.differ shouldBe 0
    outcome.unreplayable shouldBe 0
  }

  it should "name a reading the rules read otherwise" in {
    val trace  = traced(TitleNormalizer.forCountry(Country.Poland).sanitize("Lalka"))
    val edited = rewritten(trace)(_.replace("\"o\":\"lalka\"", "\"o\":\"lalka2\""))
    check(edited).differ shouldBe 1
  }

  it should "list a rule renamed in the decision trace apart: no take reads which rule fired" in {
    val trace  = traced(TitleNormalizer.forCountry(Country.Poland).firedRules(Helios, "Diuna 2D napisy"))
    val edited = rewritten(trace)(line => if (line.contains("\"m\":\"firedRules\"")) line.replace("format:", "format-renamed:") else line)
    val outcome = check(edited)
    (outcome.differ, outcome.traceOnly) shouldBe (0, 1)
  }

  it should "prove nothing of a reading it cannot make again: a normaliser no country built, or an unstable one" in {
    val trace = traced(new TitleNormalizer(services.titlerules.TitleRuleSet.forCountry(Country.Poland)).sanitize("Lalka"))
    check(trace).unreplayable shouldBe 1
    val unstable = rewritten(traced(TitleNormalizer.forCountry(Country.Poland).sanitize("Lalka")))(_.replace("}", ",\"unstable\":true}"))
    check(unstable).unreplayable shouldBe 1
  }
}
