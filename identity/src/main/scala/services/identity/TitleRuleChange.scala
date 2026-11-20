package services.identity

import services.titles.{TitleForms, TitleNormalizer}

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** The titles a family's resolve read through its [[TitleForms]] -- the one way a region reads the country's title
 *  rules: those whose key form it took, and those whose search form. Kept with the family ([[StoredFamily]]), so a
 *  restore under other title rules can read them again ([[TitleRuleChange]]). */
final case class TitleFormInputs(keys: Set[String], searches: Set[String])

/** `forms`, each title a region asks of it noted: what the region's families read of the title rules
 *  ([[TitleFormInputs]]). One a region; safe on any thread. */
private[identity] final class RecordingTitleForms(forms: TitleForms) extends TitleForms {
  private val keyed    = ConcurrentHashMap.newKeySet[String]()
  private val searched = ConcurrentHashMap.newKeySet[String]()
  def sanitize(title: String): String    = { keyed.add(title); forms.sanitize(title) }
  def searchQuery(title: String): String = { searched.add(title); forms.searchQuery(title) }
  def inputs: TitleFormInputs = TitleFormInputs(keyed.asScala.toSet, searched.asScala.toSet)
}

/**
 * The title rules moved between a stored model and its restore (`was`, rebuilt from the store's [[TitleRuleRecord]], and
 * `now`), every other input of the rules version unchanged: what tells which stored families still stand.
 *
 * A family's resolve reads the title rules three ways, and nothing else of them (IdentityRulesVersionSpec holds the
 * resolver to the country's normaliser and the relay rows, which the rules version digests): its listings' fields, built
 * by the normaliser as the archive is read ([[Listing.of]]); the corpus context, built by it as the model takes the
 * listings in and compared by each family's slice digest as at every restore; and the titles its region asks the
 * [[TitleForms]] for ([[TitleFormInputs]]). A family whose members read as they did, and every one of whose titles has
 * the same forms under both rules, resolves as it did -- the same reads, in the same order, to the same decisions -- and
 * so stands; and its decision traces stand with it when each member's title fired the same rules
 * (`TitleNormalizer.firedRules`, what its trace names). Any other family is re-resolved, as every one was before.
 */
private[identity] final class TitleRuleChange(was: TitleNormalizer, now: TitleNormalizer) {
  // One answer a title, across every family that read it: most titles are read by one family, the corpus's common
  // words by many.
  private val keyAlike    = new ConcurrentHashMap[String, java.lang.Boolean]()
  private val searchAlike = new ConcurrentHashMap[String, java.lang.Boolean]()
  private val firedWas, firedNow = scala.collection.mutable.HashMap.empty[String, Seq[String]]

  /** Does a family that read `inputs` over `members` (as the model holds them now) read the title rules as it did? */
  def readsAlike(inputs: TitleFormInputs, members: Iterable[Listing]): Boolean =
    inputs.keys.forall(alike(keyAlike, _)(forms => forms.sanitize)) &&
      inputs.searches.forall(alike(searchAlike, _)(forms => forms.searchQuery)) && firesAlike(members)

  private def alike(memo: ConcurrentHashMap[String, java.lang.Boolean], title: String)(form: TitleNormalizer => String => String): Boolean =
    memo.computeIfAbsent(title, t => java.lang.Boolean.valueOf(form(was)(t) == form(now)(t))).booleanValue

  /** Does each of `members` fire the same rules under both, as its decision trace names them? */
  private def firesAlike(members: Iterable[Listing]): Boolean = synchronized {
    members.forall(l => was.firedRules(l.cinema, l.rawTitle, firedWas) == now.firedRules(l.cinema, l.rawTitle, firedNow))
  }
}
