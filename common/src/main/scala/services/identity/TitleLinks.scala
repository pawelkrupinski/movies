package services.identity

import services.movies.TitleNormalizer

/** How two nodes' TITLES relate: the keys a title blocks under, the delimited segments it carries,
 *  and whether two titles must-link them. */
private[identity] final class TitleLinks(nodes: Seq[EvidenceNode], normalizer: TitleNormalizer, pins: PinConstraints) {

  def sanitized(title: String): String  = normalizer.sanitize(title)
  def searchForm(title: String): String = normalizer.searchQuery(title)

  def titleKeys(node: EvidenceNode): Set[String] =
    FamilyClosure.blockKeys(node.evidence.cleanTitle, node.evidence.originalTitle, None, normalizer,
      segments = IdentityMeasures.titleShapes(node.evidence.published) :+ node.evidence.cleanTitle) ++
      pins.blockKeys(node.listings.head.key)

  val segmentsOf: Map[String, Set[String]] = nodes.map(node => node.id ->
    (IdentityMeasures.titleShapes(node.evidence.published).map(sanitized).toSet - sanitized(node.evidence.cleanTitle)).filter(_.nonEmpty)).toMap
  def segmentOf(whole: EvidenceNode, decorated: EvidenceNode): Boolean =
    segmentsOf(decorated.id).contains(sanitized(whole.evidence.cleanTitle))

  /** Do two nodes' titles must-link them (tiers 2–4: same sanitised title, same search form,
   *  an original title naming the other, or one a whole segment of the other)? */
  def titleLinked(node: EvidenceNode, other: EvidenceNode): Boolean = {
    val (evidence, otherEvidence) = (node.evidence, other.evidence)
    val originals = (evidence.originalTitle ++ otherEvidence.originalTitle).map(sanitized).filter(_.nonEmpty).toSet
    (sanitized(evidence.cleanTitle).nonEmpty && sanitized(evidence.cleanTitle) == sanitized(otherEvidence.cleanTitle)) ||
      (searchForm(evidence.cleanTitle).nonEmpty && searchForm(evidence.cleanTitle) == searchForm(otherEvidence.cleanTitle)) ||
      originals.contains(sanitized(evidence.cleanTitle)) || originals.contains(sanitized(otherEvidence.cleanTitle)) ||
      segmentOf(node, other) || segmentOf(other, node)
  }

  private val wholeTitles: Set[String] = nodes.map(node => sanitized(node.evidence.cleanTitle)).filter(_.nonEmpty).toSet

  /** Does `n`'s title carry, beside its search form `form`, a delimited segment that is ANOTHER
   *  listing's whole title sharing no word with the form — so that a search form it shares with
   *  another title is no evidence the two are one film? Kino Oaza's "\"Kumotry\" - film, V
   *  FESTIWAL WAPI 2026" searches as its festival's suffix, which every film of the festival shares,
   *  while its quoted segment is the title other venues list the film by: the form is then the
   *  festival's, not the film's, and says nothing about which film the spelling is. Not "names a
   *  film beside it": a spelling whose original title reaches its OWN film ("Pieśni lasu | Pokaz
   *  …", "Whispers in the Woods") would then lose the plain listings it is the only bridge for. */
  def titlesBeside(node: EvidenceNode, form: String): Boolean = {
    val words = services.movies.TitleContainment.tokens(form).toSet
    // Segments are SANITISED (no spaces), so compare with the form sanitised too: the title's own
    // segment ("Pieśni lasu" in "Pieśni lasu | Pokaz …") is never another listing's title beside it.
    val formKey = sanitized(form)
    segmentsOf(node.id).exists(seg => wholeTitles(seg) && seg != formKey && !formKey.contains(seg) && !seg.contains(formKey) &&
      (services.movies.TitleContainment.tokens(seg).toSet intersect words).isEmpty)
  }
}
