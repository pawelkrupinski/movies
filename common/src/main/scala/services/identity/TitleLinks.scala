package services.identity

import services.movies.TitleNormalizer

/** How two nodes' TITLES relate: the keys a title blocks under, the delimited segments it carries,
 *  and whether two titles must-link them. */
private[identity] final class TitleLinks(nodes: Seq[EvidenceNode], normalizer: TitleNormalizer, pins: PinConstraints,
                                         wholeTitle: String => Boolean, bannerSegment: String => Boolean) {

  def sanitized(title: String): String  = normalizer.sanitize(title)
  def searchForm(title: String): String = normalizer.searchQuery(title)

  def titleKeys(node: EvidenceNode): Set[String] = TitleLinks.titleKeys(node, normalizer, pins, wholeTitle, bannerSegment)

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
    segmentsOf(node.id).exists(seg => wholeTitle(seg) && seg != formKey && !formKey.contains(seg) && !seg.contains(formKey) &&
      (services.movies.TitleContainment.tokens(seg).toSet intersect words).isEmpty)
  }
}

private[identity] object TitleLinks {
  /** The keys a node's title blocks under: its sanitised title, search form, original title, its
   *  segments except BANNERS, and its pins' keys — a family's seed before any match grows it. A
   *  segment is a banner when it is no listing's whole title (`wholeTitle`, sanitised) while another
   *  piece of the same title is one: in "Coraline - Sensory Friendly Screening" the work is
   *  "Coraline", and "Sensory Friendly Screening", "Part 2", "Młode Horyzonty" as keys chained PL's
   *  programmes and festivals into one family of ~75% of its listings. A title no piece of which is
   *  anyone's whole title keeps every piece: "RBO Cinema Season 2026-27: Così fan tutte" joins the
   *  RBO's and the Met's other spellings of the work only through "Così fan tutte" — unless the piece
   *  is a banner by how many titles carry it (`bannerSegment`: a festival's films rarely have a
   *  listing of their own, so "Młode Horyzonty: …" has no whole-title piece beside the banner). */
  def titleKeys(node: EvidenceNode, normalizer: TitleNormalizer, pins: PinConstraints, wholeTitle: String => Boolean,
                bannerSegment: String => Boolean): Set[String] =
    keyed(node, normalizer, pins, wholeTitle, bannerSegment).keys

  /** A node's family keys, each with WHY it has it, and the title pieces it does NOT block under,
   *  each with why not — what [[titleKeys]] decides, told (`IdentityResolver.explain`). */
  final case class Keyed(kept: Seq[(String, String)], dropped: Seq[(String, String)]) {
    def keys: Set[String] = kept.map(_._1).toSet
  }

  def keyed(node: EvidenceNode, normalizer: TitleNormalizer, pins: PinConstraints, wholeTitle: String => Boolean,
            bannerSegment: String => Boolean): Keyed = {
    val whole   = normalizer.sanitize(node.evidence.cleanTitle)
    val pieces  = IdentityMeasures.titleShapes(node.evidence.published).map(segment => segment -> normalizer.sanitize(segment))
    // The listing's own cleaned title is a work too, when another piece of what the venue published
    // lies OUTSIDE it: "Spider-Man. Całkiem nowy dzień" beside "2D DUB", "KNT"; "Lalka" beside
    // "PREMIERA", though the normaliser drops "premiera" from the whole. Pieces inside it ("Così fan
    // tutte" in "RBO Cinema Season 2026-27: Così fan tutte") say nothing of what is beside it.
    val besideIt = pieces.exists { case (_, key) => key.nonEmpty && key != whole && !whole.contains(key) }
    val isWhole = pieces.collect { case (_, key) if (key != whole && wholeTitle(key)) || (key == whole && besideIt) => key }.toSet
    def reasonToDrop(key: String): Option[String] =
      if (isWhole(key) || key == whole) None
      else isWhole.find(_ != key).map(work => s"banner beside the work '$work', which a listing carries whole")
        .orElse(Option.when(bannerSegment(key))("banner: no listing's whole title, and carried by many titles"))
    val (keptPieces, droppedPieces) = pieces.partition { case (_, key) => reasonToDrop(key).isEmpty }
    val blocked = FamilyClosure.blockKeys(node.evidence.cleanTitle, node.evidence.originalTitle, None, normalizer,
      segments = keptPieces.map(_._1) :+ node.evidence.cleanTitle)
    def why(key: String): String =
      if (key == "t:" + whole) "its title"
      else if (key == "q:" + normalizer.searchQuery(node.evidence.cleanTitle)) "its title's search form"
      else if (node.evidence.originalTitle.exists(original => key == "t:" + normalizer.sanitize(original) || key == "q:" + normalizer.searchQuery(original)))
        "its original title"
      else if (isWhole(key.drop(2))) "a piece of its title that a listing carries whole"
      else "a piece of its title beside no work (kept as the work)"
    val pinned = pins.blockKeys(node.listings.head.key).toSeq.sorted.map(_ -> "a curation pin")
    Keyed(blocked.toSeq.sorted.map(key => key -> why(key)) ++ pinned,
      droppedPieces.flatMap { case (segment, key) => reasonToDrop(key).map(reason => s"'$segment'" -> reason) }.distinctBy(_._1))
  }
}
