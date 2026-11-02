package services.identity

import services.movies.TitleNormalizer

import scala.collection.mutable

/** How two nodes' TITLES relate: the keys a title blocks under, the delimited segments it carries,
 *  and whether two titles must-link them. */
private[identity] final class TitleLinks(normalizer: TitleNormalizer, pins: PinConstraints,
                                         wholeTitle: String => Boolean, bannerSegment: String => Boolean) {

  // Each once per title, and each node's keys once per resolve: every pair of a family's nodes
  // compares them (`titleLinked`, the constraint edges), and every round of `Families.grow` keys them.
  private val sanitizedOf  = mutable.HashMap.empty[String, String]
  private val searchFormOf = mutable.HashMap.empty[String, String]
  private val keysOf       = mutable.HashMap.empty[String, Set[String]]
  def sanitized(title: String): String  = sanitizedOf.getOrElseUpdate(title, normalizer.sanitize(title))
  def searchForm(title: String): String = searchFormOf.getOrElseUpdate(title, normalizer.searchQuery(title))

  private val formsOf = mutable.HashMap.empty[String, TitleRelation.Forms]
  /** A node's title as [[TitleRelation]] reads it, once per resolve. */
  def forms(node: EvidenceNode): TitleRelation.Forms = formsOf.getOrElseUpdate(node.id, TitleRelation.of(node.evidence, sanitized, searchForm))

  def titleKeys(node: EvidenceNode): Set[String] =
    keysOf.getOrElseUpdate(node.id, TitleLinks.titleKeys(node, normalizer, pins, wholeTitle, bannerSegment))

  /** Each node's delimited segments, its own title aside. A segment with no letter names no work: a spaced dash splits
   *  the year range of "B-MOVIE – LUST & SOUND IN WESTBERLIN 1979 – 1989", and its "1989" is not the bare "(1989)"
   *  kinoheld titles 帝都大戦 by (DE recording 3650). */
  def segmentsOf(node: EvidenceNode): Set[String] = forms(node).segments
  def segmentOf(whole: EvidenceNode, decorated: EvidenceNode): Boolean = TitleRelation.segmentOf(forms(whole), forms(decorated))

  /** The title must-link two nodes' titles give, and why ([[TitleRelation.tier]]): what [[ConstraintEdges]] joins them by,
   *  the segment's own exception aside (it reads the candidates). */
  def tier(node: EvidenceNode, other: EvidenceNode): Option[(Int, String)] =
    TitleRelation.tier(forms(node), forms(other), titlesBeside(node, _), titlesBeside(other, _))

  /** Do two nodes' titles must-link them ([[tier]]: same sanitised title, same search form unless either bills another
   *  listing's title beside it, an original title naming the other or both, or one a whole segment of the other)? The
   *  relation the constraint edges join by, read for a sibling's denial too ([[Families.siblingDenying]]). */
  def titleLinked(node: EvidenceNode, other: EvidenceNode): Boolean = tier(node, other).isDefined

  /** Does `n`'s title carry, beside its search form `form`, a delimited segment that is ANOTHER
   *  listing's whole title sharing no word with the form — so that a search form it shares with
   *  another title is no evidence the two are one film? Kino Oaza's "\"Kumotry\" - film, V
   *  FESTIWAL WAPI 2026" searches as its festival's suffix, which every film of the festival shares,
   *  while its quoted segment is the title other venues list the film by: the form is then the
   *  festival's, not the film's, and says nothing about which film the spelling is. Not "names a
   *  film beside it": a spelling whose original title reaches its OWN film ("Pieśni lasu | Pokaz
   *  …", "Whispers in the Woods") would then lose the plain listings it is the only bridge for.
   *  A double bill's second work counts too when it is another listing's whole title: "Toddler Club:
   *  Tabby McTat + Room on the Broom" searches as "Tabby McTat", and joined to the bare "Tabby McTat"
   *  its "Various Directors" vetoed that film; "Wajda. Bez znieczulenia + prelekcja" bills a talk. */
  def titlesBeside(node: EvidenceNode, form: String): Boolean = {
    val words = services.movies.TitleContainment.tokens(form).toSet
    // Segments are SANITISED (no spaces), so compare with the form sanitised too: the title's own
    // segment ("Pieśni lasu" in "Pieśni lasu | Pokaz …") is never another listing's title beside it.
    val formKey = sanitized(form)
    val billed = MultiFilmBill.billedSecondTitle(node.evidence.published).map(sanitized).filter(_.nonEmpty)
    (segmentsOf(node) ++ billed).exists(seg => wholeTitle(seg) && seg != formKey && !formKey.contains(seg) && !seg.contains(formKey) &&
      (services.movies.TitleContainment.tokens(seg).toSet intersect words).isEmpty)
  }
}

private[identity] object TitleLinks {
  /** The keys a node's title blocks under: its title's ([[TitleRelation.Cut]]: sanitised title, search form, original
   *  title, its segments except BANNERS) and its pins' and catalogue ids' — a family's seed before any match grows it. */
  def titleKeys(node: EvidenceNode, normalizer: TitleNormalizer, pins: PinConstraints, wholeTitle: String => Boolean,
                bannerSegment: String => Boolean): Set[String] =
    new Cut(node, normalizer, pins, wholeTitle, bannerSegment).keys

  /** A node's family keys, each with WHY it has it, and the title pieces it does NOT block under,
   *  each with why not — what [[titleKeys]] decides, told (`IdentityResolver.explain`). */
  final case class Keyed(kept: Seq[(String, String)], dropped: Seq[(String, String)]) {
    def keys: Set[String] = kept.map(_._1).toSet
  }

  def keyed(node: EvidenceNode, normalizer: TitleNormalizer, pins: PinConstraints, wholeTitle: String => Boolean,
            bannerSegment: String => Boolean): Keyed = {
    val cut = new Cut(node, normalizer, pins, wholeTitle, bannerSegment)
    import cut.{blocked, catalogued, droppedPieces, isWhole, pinned, reasonToDrop, whole}
    val searchForm = normalizer.searchQuery(node.evidence.cleanTitle)
    def why(key: String): String =
      if (key == "t:" + whole) "its title"
      else if (key == "q:" + searchForm) "its title's search form"
      else if (node.evidence.originalTitle.exists(original => key == "t:" + normalizer.sanitize(original) || key == "q:" + normalizer.searchQuery(original)))
        "its original title"
      else if (isWhole(key.drop(2))) "a piece of its title that a listing carries whole"
      else "a piece of its title beside no work (kept as the work)"
    Keyed(blocked.toSeq.sorted.map(key => key -> why(key)) ++ pinned.map(_ -> "a curation pin") ++ catalogued.map(_ -> "its chain's catalogue id"),
      droppedPieces.flatMap { case (segment, key) => reasonToDrop(key).map(reason => s"'$segment'" -> reason) }.distinctBy(_._1))
  }

  /** What [[titleKeys]] and [[keyed]] both decide — the title's cut ([[TitleRelation.Cut]]: the pieces kept and dropped,
   *  and its block keys) and the node's pins and catalogue ids — with no reason told: a resolve and every event's title
   *  components key every node, and read no reason. */
  private final class Cut(node: EvidenceNode, normalizer: TitleNormalizer, pins: PinConstraints, wholeTitle: String => Boolean,
                          bannerSegment: String => Boolean) {
    private val title = new TitleRelation.Cut(TitleRelation.of(node.evidence, normalizer), normalizer, wholeTitle, bannerSegment)
    val whole: String = title.whole
    def isWhole(key: String): Boolean = title.isWhole(key)
    def reasonToDrop(key: String): Option[String] = title.reasonToDrop(key)
    def droppedPieces: Seq[(String, String)] = title.droppedPieces
    val blocked: Set[String] = title.blocked
    val pinned: Seq[String] = pins.blockKeys(node.listings.head.key).toSeq.sorted
    val catalogued: Seq[String] = node.listings.flatMap(_.catalogueIds).distinct.sorted.map(_.key)
    def keys: Set[String] = blocked ++ pinned ++ catalogued
  }
}
