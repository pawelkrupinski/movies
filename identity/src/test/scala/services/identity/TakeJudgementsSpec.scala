package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.TakeJudgements.Judgement

/** The hand judgements as the worker reads them ([[TakeJudgements]]): the file's rows, and the verdict they give a take. */
class TakeJudgementsSpec extends AnyFlatSpec with Matchers {

  "the judgements file" should "read the rows judging a film right or wrong, never an undecided one, a comment or the header" in {
    val lines = Seq(
      "# a comment",
      "country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\tjudged\tevidence",
      "pl\t*\tLalka\ttmdb:2\tright\topen\t\tLalka\t\t2026-10-01\t",
      "pl\tKino Iluzjon\tLalka\ttmdb:9\twrong\tknown-wrong\tserved\tLalka\t\t2026-10-01\t",
      "de\t*\tMichael\ttmdb:3\tundecided\topen\tserved\tMichael\t\t2026-10-01\t")
    TakeJudgements.parse(lines.iterator) shouldBe Seq(Judgement("pl", "*", "Lalka", "tmdb:2", right = true),
      Judgement("pl", "Kino Iluzjon", "Lalka", "tmdb:9", right = false))
  }

  "a take's verdict" should "be its venue's or every venue's judgement of a film it is known by, a billing single-spaced" in {
    val index = TakeJudgements.Index(Seq(Judgement("pl", "*", "Lalka", "tmdb:2", right = true),
      Judgement("pl", "Kino Iluzjon", "Lalka", "tmdb:9", right = true), Judgement("pl", "*", "Wersal", "tmdb:5", right = false)))
    index.verdict("Kino Muza", "Lalka", Set("tmdb:2")) shouldBe Some(true)
    index.verdict("Kino Muza", " Lalka ", Set("tmdb:2")) shouldBe Some(true)
    // another film in a database a judgement names the right film in: wrong
    index.verdict("Kino Muza", "Lalka", Set("tmdb:7")) shouldBe Some(false)
    index.verdict("Kino Iluzjon", "Lalka", Set("tmdb:9")) shouldBe Some(true)
    index.verdict("Kino Muza", "Wersal", Set("tmdb:5")) shouldBe Some(false)
    index.verdict("Kino Muza", "Michael", Set("tmdb:3")) shouldBe None
  }

  "the films denied a billing" should "be the TMDB films judged wrong at its venue or every venue, unless its venue's judges one right" in {
    val index = TakeJudgements.Index(Seq(Judgement("pl", "*", "Lalka", "tmdb:9", right = false), Judgement("pl", "*", "Lalka", "imdb:tt1", right = false),
      Judgement("pl", "Kino Iluzjon", "Lalka", "tmdb:9", right = true), Judgement("pl", "Kino Muza", "Wersal", "tmdb:5", right = false)))
    index.denied("Kino Muza", "Lalka") shouldBe Set(9)
    index.denied("Kino Iluzjon", "Lalka") shouldBe Set.empty
    index.denied("Kino Muza", "Wersal") shouldBe Set(5)
    index.denied("Helios", "Wersal") shouldBe Set.empty
  }

  "a billing" should "be normalised as the judgements key it, a no-break space and a run of spaces one space" in {
    TakeJudgements.normalise("Lalka") shouldBe "Lalka"
    TakeJudgements.normalise(" Lalka\u00a0 1968 ") shouldBe "Lalka 1968"
    TakeJudgements.normalise("ES  (ULRICH SCHAMONI)") shouldBe "ES (ULRICH SCHAMONI)"
  }

  "the bundled judgements" should "be none for a classpath without them" in {
    TakeJudgements.Index.bundled("pl").size shouldBe 0
  }
}
