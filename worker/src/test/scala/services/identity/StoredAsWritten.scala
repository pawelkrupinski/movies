package services.identity

import services.identity.agreement.{AgreementVerdicts, StoredVerdict}

/**
 * The agreement's verdicts as a deploy leaves them, for `scripts.IdentityDrift`: kept as the worker's Mongo store keeps
 * them (the in-memory store writes each one to BSON and reads it back, [[services.BsonRoundTrip]]), and able to outrule
 * every one of them as a deploy changing the agreement's code does.
 */
object StoredAsWritten {

  final class Verdicts extends AgreementVerdicts {
    private val inner = new agreement.InMemoryAgreementVerdicts
    def all(): Seq[StoredVerdict] = inner.all()
    def replace(removed: Set[String], added: Seq[StoredVerdict]): Unit = inner.replace(removed, added)
    /** Every verdict as decided under other rules: what a deploy changing the agreement's code leaves behind. */
    def outrule(): Unit = { val all = inner.all(); inner.replace(all.map(_.id).toSet, all.map(_.copy(rules = "outruled"))) }
  }
}
