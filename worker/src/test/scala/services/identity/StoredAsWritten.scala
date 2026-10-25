package services.identity

import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.bson.codecs.{DecoderContext, EncoderContext}
import services.identity.agreement.{AgreementKeys, AgreementVerdicts, StoredKey, StoredVerdict}

/**
 * The model's families and the agreement's verdicts and cluster keys kept as the worker's Mongo stores keep them: each one written as
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

  /** `key` through [[AgreementKeys.encode]] and back. */
  def key(key: StoredKey): StoredKey = AgreementKeys.decode(AgreementKeys.encode(key))

  /** The cluster keys a deploy's stage re-attaches its clusters by: without them, every stored verdict is under a key the
   *  restarted stage numbers otherwise, and a model take's stored correction stands no more. */
  final class Keys extends AgreementKeys {
    private val inner = new agreement.InMemoryAgreementKeys
    def all(): AgreementKeys.Held = inner.all()
    def replace(retired: Set[Long], moved: Seq[StoredKey], next: Long): Unit = inner.replace(retired, moved.map(key), next)
  }

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
