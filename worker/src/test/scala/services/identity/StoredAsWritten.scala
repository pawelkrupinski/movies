package services.identity

import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.bson.codecs.{DecoderContext, EncoderContext}
import services.identity.agreement.{AgreementVerdicts, StoredVerdict}

/**
 * The model's families and the agreement's verdicts kept as the worker's Mongo stores keep them: each one written as
 * its BSON document and read back from it, so what a restart takes up is what the store could hold — a field the
 * codec drops is dropped here too. What `scripts.IdentityDrift` deploys from (package `services.identity` for the
 * model store's codec).
 */
object StoredAsWritten {

  /** `family` through [[MongoIdentityModelStore.StoredFamilyCodec]] and back. */
  def family(family: StoredFamily): StoredFamily = {
    val doc = new BsonDocument()
    MongoIdentityModelStore.StoredFamilyCodec.encode(new BsonDocumentWriter(doc), family, EncoderContext.builder().build())
    MongoIdentityModelStore.StoredFamilyCodec.decode(new BsonDocumentReader(doc), DecoderContext.builder().build())
  }

  /** `verdict` through [[AgreementVerdicts.encode]] and back. */
  def verdict(verdict: StoredVerdict): StoredVerdict = AgreementVerdicts.decode(AgreementVerdicts.encode(verdict))

  final class Families extends IdentityModelStore {
    private val inner = new InMemoryIdentityModelStore
    def families(): Seq[StoredFamily] = inner.families()
    def replace(removed: Set[String], added: Seq[StoredFamily]): Unit = inner.replace(removed, added.map(family))
    def rulesVersion: Option[String] = inner.rulesVersion
    def recordRulesVersion(version: String): Unit = inner.recordRulesVersion(version)
  }

  final class Verdicts extends AgreementVerdicts {
    private val inner = new agreement.InMemoryAgreementVerdicts
    def all(): Seq[StoredVerdict] = inner.all()
    def replace(removed: Set[String], added: Seq[StoredVerdict]): Unit = inner.replace(removed, added.map(verdict))
    /** Every verdict as decided under other rules: what a deploy changing the agreement's code leaves behind. */
    def outrule(): Unit = { val all = inner.all(); inner.replace(all.map(_.id).toSet, all.map(_.copy(rules = "outruled"))) }
  }
}
