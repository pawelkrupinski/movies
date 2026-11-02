package services.identity

import services.movies.TitleNormalizer

/**
 * THE TITLE RELATION (docs/investigations/2026-11-01-flip-unification.md, stage 2): how two listings' TITLES relate,
 * defined once and read by every use that relates listings by title --
 *
 *  - the model's GROUPING: a title must-link between two nodes ([[tier]]: [[ConstraintEdges]]' title tiers, and
 *    [[TitleLinks.titleLinked]], the sibling a denial is read from in [[Families]]);
 *  - a film's BACKING: the title groups a listing's venue backing reads ([[groups]], [[VenueBacking]], [[CorpusContext]]);
 *  - RETENTION: a listing that leaves keeps its facts while a present listing relates to it by either ([[Showing]],
 *    [[ListingRetention]]).
 *
 * Each read its own key until now: retention its search form without a trailing parenthesis and its delimited pieces,
 * the model its sanitised titles, search forms and shapes, the backing its letters-and-digits keys. Where they parted, a
 * listing the model joined left the model's input while a present listing still needed it (UK "The Ladykillers", DE
 * "Black Water (GLOBALE)"). The property (`TitleRelationSpec`): every pair the model links by title, or one backs the
 * other's film by, retention keeps.
 */
object TitleRelation {

  /** A title as the relation reads it, from a listing's own evidence:
   *  @param whole      its cleaned title, sanitised (tier 2, same title)
   *  @param searchForm its cleaned title's search form (tier 3, same search form)
   *  @param originals  its original titles, sanitised (tier 3, original title)
   *  @param segments   its delimited pieces with a letter, sanitised, its whole aside (tier 4, a whole segment of the other)
   *  @param groupKey   the title group it is filed under for backing ([[IdentityMeasures.key]] of its title)
   *  @param groups     the title groups its backing reads ([[groups]])
   *  @param cleanTitle its cleaned title, its `originalTitle` and its delimited `shapes` as published: what its family
   *                    block keys are cut from ([[Cut]]) */
  final case class Forms(whole: String, searchForm: String, originals: Set[String], segments: Set[String], groupKey: String,
                         groups: Set[String], cleanTitle: String, originalTitle: Option[String], shapes: Seq[String]) {
    /** The title block keys its family is grown by ([[Cut]]), against a corpus's whole titles and banners. */
    def keys(normalizer: TitleNormalizer, wholeTitle: String => Boolean, bannerSegment: String => Boolean): Set[String] =
      new Cut(this, normalizer, wholeTitle, bannerSegment).blocked
    /** The keys a PRESENT listing shows its title under ([[Showing]]). */
    def shown: Iterator[String] =
      Iterator(s"w:$whole", s"q:$searchForm", s"k:$groupKey").filter(_.length > 2) ++ originals.iterator.map("o:" + _) ++
        segments.iterator.map("s:" + _) ++ groups.iterator.map("g:" + _)
    /** The keys under which a present listing relating to this one shows ([[Showing]]): the same whole title, search form
     *  or original title; this whole a segment of its title, or its whole one of these segments; one title filed in a group
     *  the other's backing reads. */
    def probes: Iterator[String] =
      Iterator(s"w:$whole", s"q:$searchForm", s"o:$whole", s"g:$groupKey").filter(_.length > 2) ++ (if (whole.isEmpty) Iterator.empty else Iterator(s"s:$whole")) ++
        originals.iterator.flatMap(o => Iterator(s"w:$o", s"o:$o")) ++ segments.iterator.map("w:" + _) ++ groups.iterator.map("k:" + _)
  }

  /** `evidence`'s forms, sanitised and searched by `normalizer`. */
  def of(evidence: Evidence, normalizer: TitleNormalizer): Forms = of(evidence, normalizer.sanitize, normalizer.searchQuery)

  def of(evidence: Evidence, sanitize: String => String, searchQuery: String => String): Forms = {
    val whole = sanitize(evidence.cleanTitle)
    val shapes = evidence.published.shapes
    Forms(whole, searchQuery(evidence.cleanTitle), evidence.originalTitle.map(sanitize).filter(_.nonEmpty).toSet,
      (shapes.map(sanitize).toSet - whole).filter(_.exists(_.isLetter)),
      IdentityMeasures.key(evidence.title), groups(evidence.measured).toSet, evidence.cleanTitle, evidence.originalTitle, shapes)
  }

  /** A listing's forms read off the listing alone -- no venue page, the resolver's decorations: what retention relates
   *  listings by before any resolve, so the rule needs no take to decide what a take reads. */
  def of(listing: Listing, normalizer: TitleNormalizer): Forms = of(Evidence.of(listing, None, TitleDecorations.resolver), normalizer)

  /** Every title group a listing's backing can read, whatever the film ([[backingGroups]]): its title's and its
   *  decorated-only shapes' keys, every title its search asks for, and every shape of its title. What a family's resolve
   *  reads of the corpus's groups ([[CorpusContext.Reads]]) and what retention relates a listing by. */
  def groups(listing: IdentityMeasures.Listing): Seq[String] =
    (IdentityMeasures.titleGroups(listing) ++ IdentityMeasures.searchGroupsAny(listing) ++ listing.shapeKeys).filter(_.nonEmpty).distinct

  /** The title groups whose venues back `film` for `listing` ([[FamilyScope]]): its own title's, the searches naming the
   *  film, and a film the title carries as one of its delimited pieces is backed by the venues billing that piece alone —
   *  DE Astor's "ATMOS: Michael", which Cineamo links to Dreyer's 1924 film, is the "Michael" sixteen venues credit to
   *  Antoine Fuqua. Every one is among [[groups]]. */
  def backingGroups(listing: IdentityMeasures.Listing, film: IdentityMeasures.Film): Seq[String] =
    (IdentityMeasures.titleGroups(listing) ++ IdentityMeasures.searchGroups(listing, film) ++ listing.shapeKeys.filter(film.titleKeys)).distinct

  /** The title group a listing is filed under for backing: its title's key. */
  def groupKey(evidence: Evidence): String = IdentityMeasures.key(evidence.title)

  /** The title must-link the model joins two nodes by, and why ([[ConstraintEdges]]: 2 the same sanitised title, 3 the same
   *  search form -- not one either title only bills BESIDE another listing's title (`beside`) -- or an original title
   *  naming the other or both, 4 one's whole title a delimited segment of the other's). The segment's own exception, the
   *  rest of a title naming a film of its own, reads the candidates and stays with [[ConstraintEdges]]. */
  def tier(a: Forms, b: Forms, besideA: String => Boolean, besideB: String => Boolean): Option[(Int, String)] =
    if (a.whole.nonEmpty && a.whole == b.whole) Some((2, ConstraintEdges.SameTitle))
    else if (a.searchForm.nonEmpty && a.searchForm == b.searchForm && !besideA(a.searchForm) && !besideB(a.searchForm))
      Some((3, ConstraintEdges.SameSearchForm))
    else if ((a.originals ++ b.originals).exists(o => o == a.whole || o == b.whole) || (a.originals.nonEmpty && a.originals == b.originals))
      Some((3, ConstraintEdges.OriginalTitle))
    else if (segmentOf(a, b) || segmentOf(b, a)) Some((4, ConstraintEdges.TitleSegment))
    else None

  /** Is `whole`'s title a delimited segment of `decorated`'s? */
  def segmentOf(whole: Forms, decorated: Forms): Boolean = decorated.segments.contains(whole.whole)

  /** Does `b` back `a`'s film by title: `b`'s title is filed in a group `a`'s backing reads? */
  def backs(b: Forms, a: Forms): Boolean = b.groupKey.nonEmpty && a.groups.contains(b.groupKey)

  /** Do two titles relate by any use: linked by title (no exception), or one backing the other's film. What retention
   *  keeps a listing by -- a superset of each use, so whatever the model joins or the backing reads, retention keeps. */
  def related(a: Forms, b: Forms): Boolean = tier(a, b, _ => false, _ => false).isDefined || backs(a, b) || backs(b, a)

  /** The titles the PRESENT listings show, indexed: whether a leaving listing relates to any of them is one look-up per
   *  form, not a pass over every present listing. A leaving listing relates to a present one when [[related]] does, or when
   *  the two share a family block key ([[Cut]], cut against the whole titles and banners of every listing named here):
   *  what puts two listings in one family puts them in one another's input. `others` (the leaving and the kept ones)
   *  count toward those whole titles and banners too, as the model's corpus holds them. */
  final class Showing(present: Seq[Forms], others: Seq[Forms], normalizer: TitleNormalizer) {
    private val all    = present ++ others
    private val wholes = all.iterator.map(_.whole).filter(_.nonEmpty).toSet
    private val spread = all.flatMap(forms => forms.shapes.map(normalizer.sanitize).filter(piece => piece.nonEmpty && piece != forms.whole).distinct
      .map(_ -> forms.whole)).groupMap(_._1)(_._2).map { case (piece, titles) => piece -> titles.distinct.size }
    private def bannerSegment(piece: String) = !wholes(piece) && spread.getOrElse(piece, 0) >= CorpusContext.BannerSpread
    private def keysOf(forms: Forms): Iterator[String] = forms.keys(normalizer, wholes, bannerSegment).iterator.map("b:" + _)
    private val shown: Set[String] = present.iterator.flatMap(forms => forms.shown ++ keysOf(forms)).toSet
    def relates(gone: Forms): Boolean = gone.probes.exists(shown) || keysOf(gone).exists(shown)
  }

  /** A title's family block keys ([[TitleLinks.titleKeys]], pins and catalogue ids aside): its sanitised title, search form,
   *  original title, and its delimited pieces except BANNERS. A piece is a banner when it is no listing's whole title
   *  (`wholeTitle`, sanitised) while another piece of the same title is one: in "Coraline - Sensory Friendly Screening"
   *  the work is "Coraline", and "Sensory Friendly Screening", "Part 2", "Młode Horyzonty" as keys chained PL's
   *  programmes and festivals into one family of ~75% of its listings. A title no piece of which is anyone's whole title
   *  keeps every piece: "RBO Cinema Season 2026-27: Così fan tutte" joins the RBO's and the Met's other spellings of the
   *  work only through "Così fan tutte" — unless the piece is a banner by how many titles carry it (`bannerSegment`: a
   *  festival's films rarely have a listing of their own, so "Młode Horyzonty: …" has no whole-title piece beside the
   *  banner). */
  final class Cut(forms: Forms, normalizer: TitleNormalizer, wholeTitle: String => Boolean, bannerSegment: String => Boolean) {
    val whole   = normalizer.sanitize(forms.cleanTitle)
    val pieces  = forms.shapes.map(segment => segment -> normalizer.sanitize(segment))
    // The listing's own cleaned title is a work too, when another piece of what the venue published
    // lies OUTSIDE it: "Spider-Man. Całkiem nowy dzień" beside "2D DUB", "KNT"; "Lalka" beside
    // "PREMIERA", though the normaliser drops "premiera" from the whole. Pieces inside it ("Così fan
    // tutte" in "RBO Cinema Season 2026-27: Così fan tutte") say nothing of what is beside it.
    private val besideIt = pieces.exists { case (_, key) => key.nonEmpty && key != whole && !whole.contains(key) }
    val isWhole = pieces.collect { case (_, key) if (key != whole && wholeTitle(key)) || (key == whole && besideIt) => key }.toSet
    // The search form is a work too, for the pieces that lie outside it: in "Gorzkie święta / napisy
    // - Nasze Kino" the label sticks to the film's piece, so no piece is anyone's whole title, but the
    // normaliser's form "Gorzkie święta" still says the venue's name beside it is no work.
    private val form = normalizer.sanitize(normalizer.searchQuery(forms.cleanTitle))
    private def besideTheForm(key: String): Boolean = form.nonEmpty && form != whole && !key.contains(form) && !form.contains(key)
    def reasonToDrop(key: String): Option[String] =
      if (isWhole(key) || key == whole) None
      else isWhole.find(_ != key).map(work => s"banner beside the work '$work', which a listing carries whole")
        .orElse(Option.when(besideTheForm(key))(s"banner beside the work '$form', its title's search form"))
        .orElse(Option.when(bannerSegment(key))("banner: no listing's whole title, and carried by many titles"))
    val (keptPieces, droppedPieces) = pieces.partition { case (_, key) => reasonToDrop(key).isEmpty }
    val blocked: Set[String] = FamilyClosure.blockKeys(forms.cleanTitle, forms.originalTitle, normalizer,
      segments = keptPieces.map(_._1) :+ forms.cleanTitle)
  }
}
