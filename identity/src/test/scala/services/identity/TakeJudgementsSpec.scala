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
    val index = new TakeJudgements.Index(Seq(Judgement("pl", "*", "Lalka", "tmdb:2", right = true),
      Judgement("pl", "Kino Iluzjon", "Lalka", "tmdb:9", right = true), Judgement("pl", "*", "Wersal", "tmdb:5", right = false)))
    index.verdict("Kino Muza", "Lalka", Set("tmdb:2")) shouldBe Some(true)
    index.verdict("Kino Muza", " Lalka ", Set("tmdb:2")) shouldBe Some(true)
    // another film in a database a judgement names the right film in: wrong
    index.verdict("Kino Muza", "Lalka", Set("tmdb:7")) shouldBe Some(false)
    index.verdict("Kino Iluzjon", "Lalka", Set("tmdb:9")) shouldBe Some(true)
    index.verdict("Kino Muza", "Wersal", Set("tmdb:5")) shouldBe Some(false)
    index.verdict("Kino Muza", "Michael", Set("tmdb:3")) shouldBe None
  }

  "a take judged wrong" should "be withdrawn from its own listing alone, the rest of its decision taking the film still" in {
    def key(venue: String, title: String) = services.movies.ListingKey.Published(venue, title, None, Nil)
    val (tywyn, ilkley, other) = (key("Magic Lantern Tywyn", "Fallen Angels"), key("Ilkley Cinema", "Fallen Angels"), key("Kino Muza", "Lalka"))
    val pooled = ResolverDecision(Seq(tywyn, ilkley), Some(11220), 0.9, ResolverDecision.Basis.PooledMatch, Nil)()
    val lalka  = ResolverDecision(Seq(other), Some(2), 0.9, ResolverDecision.Basis.OwnMatch, Nil)()
    val resolution = Resolution(Seq(pooled, lalka), 2, Map.empty, Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    val index = new TakeJudgements.Index(Seq(Judgement("uk", "Magic Lantern Tywyn", "Fallen Angels", "tmdb:11220", right = false)))
    val after = TakeJudgements.withdraw(resolution, index).decisions
    after.find(_.members.contains(ilkley)).map(d => (d.members, d.film, d.basis)) shouldBe Some((Seq(ilkley), Some(11220), ResolverDecision.Basis.PooledMatch))
    after.find(_.members.contains(tywyn)).map(d => (d.members, d.film, d.basis)) shouldBe Some((Seq(tywyn), None, ResolverDecision.Basis.Withdrawn))
    after.find(_.members.contains(other)) shouldBe Some(lalka)
    TakeJudgements.withdraw(resolution, TakeJudgements.Index.Empty) shouldBe theSameInstanceAs(resolution)
  }

  "a billing" should "be normalised as the judgements key it, a no-break space and a run of spaces one space" in {
    TakeJudgements.normalise("Lalka") shouldBe "Lalka"
    TakeJudgements.normalise(" Lalka\u00a0 1968 ") shouldBe "Lalka 1968"
    TakeJudgements.normalise("ES  (ULRICH SCHAMONI)") shouldBe "ES (ULRICH SCHAMONI)"
  }
}
