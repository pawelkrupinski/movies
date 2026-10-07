package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Where two copies of one concept DISAGREE, found while consolidating the rules (identity-resolver-rules-inventory.md
 * §3): behaviour is kept as it was, each written up here as a pending test until someone decides which copy is right.
 * Each states what the two copies do today; the pending body is the decision to take.
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
    // today: IdentityMeasures' article-less exact title knows English articles only ("the", "a", "an"); Agreement.namesIt
    // knows fifteen in five languages (DE "Camp der Verlorenen" names TMDB's "Das Camp der Verlorenen" only there)
    val listing = IdentityMeasures.Listing("Camp der Verlorenen")
    IdentityMeasures.titleRelation(listing, IdentityMeasures.Film("Das Camp der Verlorenen")) should not be IdentityMeasures.Category("exact")
    pending // decide: one article list for both (a resolver change: measure it on the ratchet and the full corpora)
  }

  "F4: the broadcast join" should "take the same record in the fill's signal as in the stage's take" in {
    // today: the stage takes by Broadcast.takeOrWait (a credited production, waiting on undated records), the unified
    // fill's `broadcast.take` signal by Broadcast.take (neither) — where a production's credit decides, they disagree
    pending
  }

  "F5: the unified fill's poster guard" should "rule a contender out before a fill rule picks, as the offline fit measured it" in {
    // today: AgreementStage.filledOf builds the fill's evidence with no posters, so `poster.otherMatches` never guards a
    // contender there; the veto is applied to the picked film after (filledTake), which then takes NOTHING where the fit
    // would have picked the next contender. Fewer takes than measured, never a wrong one.
    pending
  }
}
