package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Where two copies of one concept DISAGREED, found while consolidating the rules (identity-resolver-rules-inventory.md
 * §3, §5): F1, F3 and F4 unified into one rule each, each graded on the unmatched-cluster ratchet; F5 kept, its reason
 * stated (F2 is two concepts on purpose, inventory §5).
 */
class IdentityRuleFindingsSpec extends AnyFlatSpec with Matchers {

  "F1: a director credited in another script" should "contradict the film alike in the agreement's listing check and in a correction" in {
    // one rule for both: a venue's Latin credit against a record's Cyrillic one, transliterated and sharing no name's
    // stem, is another person — a guard that rules a film out reads the weaker evidence as against it (wrong beats
    // missing), as a correction always did
    val tarkovsky = IdentityMeasures.Film("Zerkalo", None, Nil, Some(1975), Some(107), Some(Seq("Андрей Тарковский")), None, None)
    val muratova  = IdentityMeasures.Film("Zerkalo", None, Nil, Some(1975), Some(107), Some(Seq("Kira Muratova")), None, None)
    val billed    = FilmTable.listing(models.KinoMuza, "Zerkalo", director = Some("Kira Muratova"))
    agreement.Agreement.contradictedByAnyListing(Seq(billed), agreement.SourceRecord(tarkovsky)) shouldBe true
    agreement.Correction.contradicts(muratova, tarkovsky) shouldBe true
    // the same person in two scripts contradicts neither
    val tarkovskyBilled = FilmTable.listing(models.KinoMuza, "Zerkalo", director = Some("Andrei Tarkovsky"))
    agreement.Agreement.contradictedByAnyListing(Seq(tarkovskyBilled), agreement.SourceRecord(tarkovsky)) shouldBe false
  }

  "F3: a title with a leading article the venue drops" should "be named alike by the measures and by the agreement" in {
    // one article list for both: DE "Camp der Verlorenen" is TMDB's "Das Camp der Verlorenen" to the measures too
    val listing = IdentityMeasures.Listing("Camp der Verlorenen")
    IdentityMeasures.titleRelation(listing, IdentityMeasures.Film("Das Camp der Verlorenen")) shouldBe IdentityMeasures.Category("exact")
    // still three words after it at least, and never the listing's own article dropped
    IdentityMeasures.titleRelation(IdentityMeasures.Listing("Grande Bellezza"), IdentityMeasures.Film("La Grande Bellezza")) should not be IdentityMeasures.Category("exact")
    IdentityMeasures.titleRelation(IdentityMeasures.Listing("Das Camp der Verlorenen"), IdentityMeasures.Film("Camp der Verlorenen")) should not be IdentityMeasures.Category("exact")
  }

  "F4: the broadcast join" should "take the same record in the fill's signal as in the stage's take" in {
    // one join: the fill's `broadcast.take` signal reads the productions the stage credits and waits as it waits. UK
    // Flicks' "RBO Cinema Season 2026-27: Macbeth" crediting Louisa Proske is the Met's 2026/27 Macbeth only by the film
    // database's record of the Met's production crediting her
    val metMacbeth = IdentityMeasures.Film("The Metropolitan Opera 2026/27: Macbeth", year = Some(2026), runtime = Some(209),
      released = Some(java.time.LocalDate.of(2026, 10, 17)))
    val proske     = Seq(IdentityMeasures.Film("The Metropolitan Opera: Macbeth", runtime = Some(209), directors = Some(Seq("Louisa Proske"))))
    val relay      = FilmTable.listing(models.Multikino, "RBO Cinema Season 2026-27: Macbeth", director = Some("Louisa Proske"))
      .copy(screenings = ScreeningDays.of(Seq(java.time.LocalDate.of(2026, 10, 20))))
    val measured   = (listing: Listing) => Evidence.of(listing, None).measured
    agreement.Broadcast.take(Seq(relay), measured, Seq(1703622 -> metMacbeth), () => Answer.Known(proske))().toOption.flatten.map(_.film) shouldBe Some(1703622)
    val decision = ResolverDecision(Seq(relay.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil)()
    val node     = IdentityResolver.NodeEvidence(Seq(relay.key), Seq(IdentityResolver.CandidateEvidence(1703622, metMacbeth, 0.1, Some(1), false,
      titleNamesIt = true, seasonProduction = true, houseProduction = false)))
    def signal(evidence: UnifiedEvidence.ClusterEvidence) =
      UnifiedEvidence.contenders(evidence).find(_.tmdb.contains(1703622)).flatMap(_.signals.get("broadcast.take"))
    val evidence = UnifiedEvidence.ClusterEvidence(Seq(relay), decision, Seq(node), Nil, Nil, _ => None, thisYear = 2026,
      measured = Some(measured), productions = () => Answer.Known(proske))
    signal(evidence) shouldBe Some(1.0)
    // …and none while the stage would wait on a record's day
    val undated = IdentityMeasures.Film("The Metropolitan Opera 2026/27: Macbeth", year = Some(2026), runtime = Some(209))
    val waiting = evidence.copy(nodes = Seq(node.copy(candidates = node.candidates :+ IdentityResolver.CandidateEvidence(1703699, undated, 0.1,
      Some(2), false, titleNamesIt = true, seasonProduction = true, houseProduction = false))), undated = _ == 1703699)
    signal(waiting) shouldBe None
  }

  "F5: the unified fill's poster guard" should "stay read after the fill picks: a vetoed pick takes nothing, never the next contender" in {
    // KEPT, not unified (inventory §5 F5). The offline fit read `poster.otherMatches` as a guard BEFORE a rule picks, so a
    // rule firing on two contenders, one a venue poster rules out, takes the other; the stage builds the fill's evidence
    // with no posters (they are hashed only once the fall-through asks, after the verdict and its fill are stored) and
    // vetoes the pick after ([[agreement.AgreementStage]]'s filledTake). Wiring them in changed no take on the ratchet's
    // captures (the fill re-read with every poster-bearing cluster's posters: 40 clusters, 0 fills moved), and needs the
    // stored verdict re-resolved when its posters land — so the stage keeps the read that only ever takes less.
    val rules  = UnifiedRules("t", UnifiedEvidence.Guards, Seq("venues.current"))
    def contender(tmdb: Int, signals: (String, Double)*) =
      UnifiedEvidence.Contender(s"tmdb:$tmdb", Some(tmdb), None, s"film $tmdb", Map.empty, signals.toMap)
    val other  = contender(1002, "venues.current" -> 1.0)
    // as the fit read it: the poster guards 1001 out, the rule fires on 1002 alone
    rules.filled(Seq(contender(1001, "venues.current" -> 1.0, "poster.otherMatches" -> 1.0), other)).map(_._1.tmdb) shouldBe Some(Some(1002))
    // as the stage reads it, no poster known: the rule fires on two, and takes neither
    rules.filled(Seq(contender(1001, "venues.current" -> 1.0), other)) shouldBe None
    // …and a pick a venue poster matches another candidate than is vetoed after, the stage taking nothing for it
    PosterEvidence.veto(Some(1001), Seq(Map(1001 -> Some(30), 1002 -> Some(2)))) shouldBe Some(1002 -> 2)
  }
}
