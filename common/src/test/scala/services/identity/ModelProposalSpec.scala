package services.identity

import models.{Helios, KinoApollo, Rialto}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

/** A language model's proposal for a listing no rule took ([[Proposal]]): one more title search, and a film the
 *  `model-proposed` rule takes only when TMDB's own search finds it exactly, in its year, and the venue's facts do not
 *  contradict it — or, for a package of shorts or a live event, the listing's blocker. */
class ModelProposalSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  import FilmTable.{F, listing}

  /** `films`, with the proposals a model made for listings' titles. */
  private def withProposals(films: Seq[F], proposals: Map[String, Proposal]): IdentityLookups = new IdentityLookups {
    private val table = new FilmTable(films, normalizer)
    def hasDetail(l: Listing) = false
    def detail(l: Listing)    = table.detail(l)
    def candidates(q: CandidateQuery) = table.candidates(q)
    def film(id: Int) = table.film(id)
    override def proposal(l: Listing) = proposals.get(l.rawTitle)
  }
  private def resolve(ls: Seq[Listing], films: Seq[F], proposals: Map[String, Proposal]) =
    IdentityResolver.resolve(ls, withProposals(films, proposals), normalizer, IdentityCalibration.resolver)

  // PL Kino Kosmos's "Dokumentalna Kreska: Wyznania szwedzkiego mężczyzny": TMDB holds no Polish title for it
  private val swede  = F(1494878, "Confessions of a Swedish Man", 2025, "Jonas Selberg Augustsén", 90, 2)
  private val namesake = F(9001, "Confessions", 2010, "Tetsuya Nakashima", 106, 20)
  private val title  = "Dokumentalna Kreska: Wyznania szwedzkiego mężczyzny"

  "a listing a model proposes a film for" should "take that film when its search finds it exactly, in its year" in {
    val l = listing(Rialto, title)
    val d = resolve(Seq(l), Seq(swede, namesake), Map(title -> Proposal("film", Some("Confessions of a Swedish Man"), Some(2025)))).decisionOf(l.key)
    withClue(d.render) {
      d.film shouldBe Some(1494878)
      d.trace.rulesOf(l.key) should contain ("accept:model-proposed")
    }
  }

  it should "take nothing when the proposed year is another, or the venue's own facts contradict the film" in {
    val wrongYear = listing(Rialto, title)
    resolve(Seq(wrongYear), Seq(swede), Map(title -> Proposal("film", Some("Confessions of a Swedish Man"), Some(2019))))
      .decisionOf(wrongYear.key).film shouldBe None
    // the venue publishes a running time 60 minutes off the record's: its own fact outweighs any proposal
    val contradicted = listing(KinoApollo, title, runtime = Some(150))
    resolve(Seq(contradicted), Seq(swede), Map(title -> Proposal("film", Some("Confessions of a Swedish Man"), Some(2025))))
      .decisionOf(contradicted.key).film shouldBe None
  }

  it should "take nothing without a proposal: the Polish title alone finds no film" in {
    val l = listing(Rialto, title)
    resolve(Seq(l), Seq(swede, namesake), Map.empty).decisionOf(l.key).film shouldBe None
  }

  "a listing a model judged no film" should "be blocked as not a film, not by a rule" in {
    val workshop = "Warsztaty ceramiczne dla dzieci"
    val l = listing(Helios, workshop)
    val d = resolve(Seq(l), Seq(namesake), Map(workshop -> Proposal("event"))).decisionOf(l.key)
    d.film shouldBe None
    d.trace.nodes(l.key).blocker shouldBe Some("not-a-film:event")
  }
}
