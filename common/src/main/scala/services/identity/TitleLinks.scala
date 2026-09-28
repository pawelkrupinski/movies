package services.identity

import services.movies.TitleNormalizer

/** How two nodes' TITLES relate: the keys a title blocks under, the delimited segments it carries,
 *  and whether two titles must-link them. */
private[identity] final class TitleLinks(nodes: Seq[EvidenceNode], normalizer: TitleNormalizer, pins: PinConstraints) {

  def sanitized(s: String): String  = normalizer.sanitize(s)
  def searchForm(s: String): String = normalizer.searchQuery(s)

  def titleKeys(n: EvidenceNode): Set[String] =
    FamilyClosure.blockKeys(n.evidence.cleanTitle, n.evidence.originalTitle, None, normalizer,
      segments = IdentityMeasures.titleShapes(n.evidence.published) :+ n.evidence.cleanTitle) ++
      pins.blockKeys(n.listings.head.key)

  val segmentsOf: Map[String, Set[String]] = nodes.map(n => n.id ->
    (IdentityMeasures.titleShapes(n.evidence.published).map(sanitized).toSet - sanitized(n.evidence.cleanTitle)).filter(_.nonEmpty)).toMap
  def segmentOf(whole: EvidenceNode, decorated: EvidenceNode): Boolean =
    segmentsOf(decorated.id).contains(sanitized(whole.evidence.cleanTitle))

  /** Do two nodes' titles must-link them (tiers 2–4: same sanitised title, same search form,
   *  an original title naming the other, or one a whole segment of the other)? */
  def titleLinked(x: EvidenceNode, y: EvidenceNode): Boolean = {
    val (ex, ey) = (x.evidence, y.evidence)
    val originals = (ex.originalTitle ++ ey.originalTitle).map(sanitized).filter(_.nonEmpty).toSet
    (sanitized(ex.cleanTitle).nonEmpty && sanitized(ex.cleanTitle) == sanitized(ey.cleanTitle)) ||
      (searchForm(ex.cleanTitle).nonEmpty && searchForm(ex.cleanTitle) == searchForm(ey.cleanTitle)) ||
      originals.contains(sanitized(ex.cleanTitle)) || originals.contains(sanitized(ey.cleanTitle)) ||
      segmentOf(x, y) || segmentOf(y, x)
  }

  private val wholeTitles: Set[String] = nodes.map(n => sanitized(n.evidence.cleanTitle)).filter(_.nonEmpty).toSet

  /** Does `n`'s title carry, beside its search form `form`, a delimited segment that is ANOTHER
   *  listing's whole title sharing no word with the form — so that a search form it shares with
   *  another title is no evidence the two are one film? Kino Oaza's "\"Kumotry\" - film, V
   *  FESTIWAL WAPI 2026" searches as its festival's suffix, which every film of the festival shares,
   *  while its quoted segment is the title other venues list the film by: the form is then the
   *  festival's, not the film's, and says nothing about which film the spelling is. Not "names a
   *  film beside it": a spelling whose original title reaches its OWN film ("Pieśni lasu | Pokaz
   *  …", "Whispers in the Woods") would then lose the plain listings it is the only bridge for. */
  def titlesBeside(n: EvidenceNode, form: String): Boolean = {
    val words = services.movies.TitleContainment.tokens(form).toSet
    // Segments are SANITISED (no spaces), so compare with the form sanitised too: the title's own
    // segment ("Pieśni lasu" in "Pieśni lasu | Pokaz …") is never another listing's title beside it.
    val formKey = sanitized(form)
    segmentsOf(n.id).exists(seg => wholeTitles(seg) && seg != formKey && !formKey.contains(seg) && !seg.contains(formKey) &&
      (services.movies.TitleContainment.tokens(seg).toSet intersect words).isEmpty)
  }
}
