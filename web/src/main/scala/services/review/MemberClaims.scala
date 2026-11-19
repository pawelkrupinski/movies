package services.review

/** What a card's members say of the film: the venues' own facts -- the first stated value per fact wins -- apart from
 *  a feed catalogue's copied claims, and the posters and synopsis, a venue's own before a catalogue's copy. */
final case class MemberClaims(members: Seq[MemberView]) {

  /** The venues' posters, each once — a venue's own before a feed catalogue's copy: the card shows the first that loads. */
  lazy val posters: Seq[String]     = members.sortBy(_.factsFromCatalogue).flatMap(_.poster).distinct
  /** The venue's synopsis: a venue's own before a feed catalogue's copy. */
  lazy val synopsis: Option[String] = members.sortBy(_.factsFromCatalogue).flatMap(_.synopsis).headOption

  /** What the venues themselves say, merged across members — the first stated value per fact wins — with a
   *  feed catalogue's copied claims left to [[catalogueSays]]. Screenings are every member's: no catalogue claims those. */
  lazy val venueSays: Seq[(String, String)] = {
    val own    = members.filterNot(_.factsFromCatalogue)
    val stated = own.map(_.member)
    val facts  = own.flatMap(m => m.slot.map(_.facts).toSeq ++ m.page.toSeq)
    def list(f: VenueFacts => Seq[String]) = facts.map(f).find(_.nonEmpty)
    val feeds  = members.flatMap(_.feed)
    rows(
      "Original title" -> facts.flatMap(_.originalTitle).headOption,
      "Year"           -> stated.flatMap(_.year).headOption.map(_.toString),
      "Director"       -> stated.map(_.directors).find(_.nonEmpty).map(_.mkString(", ")),
      "Cast"           -> Option(venueCast.mkString(", ")),
      "Runtime"        -> facts.flatMap(_.runtime).headOption.map(r => s"$r min"),
      "Country"        -> list(_.countries).map(_.mkString(", ")),
      "Catalogue ids"  -> Some(own.flatMap(_.feed).filter(_.catalogueIds.nonEmpty).map(_.catalogue).distinct.mkString(", ")),
      "Screenings"     -> Option.when(feeds.nonEmpty) {
        val span = (feeds.flatMap(_.first).minOption, feeds.flatMap(_.last).maxOption) match {
          case (Some(a), Some(b)) if a != b => s" · $a → $b"
          case (a, b)                       => a.orElse(b).fold("")(" · " + _)
        }
        s"${feeds.map(_.screenings).sum}$span"
      })
  }

  /** The cast the venues themselves list — the first member's that lists one; the page cuts it short ([[ReviewCards.CastShown]]). */
  lazy val venueCast: Seq[String] =
    members.filterNot(_.factsFromCatalogue).flatMap(m => m.slot.map(_.facts).toSeq ++ m.page.toSeq).map(_.cast).find(_.nonEmpty).getOrElse(Nil)
  /** The cast an aggregator's catalogue entry lists, copied onto its listings by the feed. */
  lazy val catalogueCast: Seq[String] =
    members.filter(_.factsFromCatalogue).flatMap(m => m.slot.map(_.facts).toSeq ++ m.page.toSeq).map(_.cast).find(_.nonEmpty).getOrElse(Nil)

  /** What an aggregator's catalogue entry claims, copied onto its listings by the feed — never the venue's word. */
  lazy val catalogueSays: Seq[(String, String)] = {
    val fed   = members.filter(_.factsFromCatalogue)
    val facts = fed.flatMap(m => m.slot.map(_.facts).toSeq ++ m.page.toSeq)
    rows(
      "Year"          -> facts.flatMap(_.year).headOption.map(_.toString),
      "Director"      -> facts.map(_.directors).find(_.nonEmpty).map(_.mkString(", ")),
      "Cast"          -> Option(catalogueCast.mkString(", ")),
      "Runtime"       -> facts.flatMap(_.runtime).headOption.map(r => s"$r min"),
      "Catalogue ids" -> Some(fed.map(m => m.feed.map(_.catalogue).filter(_.nonEmpty).orElse(m.pageUrl).getOrElse("")).distinct.mkString(", ")))
  }

  private def rows(stated: (String, Option[String])*): Seq[(String, String)] =
    stated.collect { case (name, Some(value)) if value.nonEmpty => name -> value }
}
