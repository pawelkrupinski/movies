package services.identity

import services.titles.billing.BillingPieces
import services.identity.Scored.Accepted

/** What makes a listing a BILL of several works, which no acceptance rule takes a film of: two works a conjunction, a "+",
 *  a slash or an "and" joins, or a programme a word bills as several films — and the one film such a word may still name.
 *  Part of [[Acceptance]], which mixes it in. */
private[identity] trait AcceptanceBillRules { this: Acceptance =>

  /** A DOUBLE PROGRAMME: two quoted works a conjunction joins ([[services.titles.billing.BillingParse.quotesTwoJoined]], PL "„Mieszkanie” i „Owoce
   *  rajskich drzew spożywamy”"), or a "+" joining two whole works ([[MultiFilmBill.billsTwoWholeWorks]]), whatever is held
   *  of them, is neither film (user rule) — UK "We're Going on a Bear Hunt + The Tiger Who Came to Tea" {Joanna
   *  Harrison, Robin Shaw} ×133, its two films scored 91–93% and popularity's coin picking one; PL Kino Pałacowe's
   *  "Historia kina w Popielawach + Pruska kultura", a 1908 short TMDB does not hold. A talk joined to one film
   *  ("+ spotkanie z reżyserem") is no second work, and a film whose own whole title it is ("Romeo + Juliet") is no
   *  bill. So is a programme a word bills as several films ([[MultiFilmBill]]: "Triple Feature: Lord of the Rings",
   *  "The Dark Knight Trilogy", "Maraton Horrorów") — unless an eligible film's own title carries that word ("Marathon
   *  Man"), and then only that film may be taken ([[takesNoBill]]). */
  def billsSeveralWorks(ranked: Seq[Scored]): Boolean = ranked.headOption.exists { any =>
    any.listing.rawTitle.exists(BillingPieces.quotesSeveralDated) || (any.listing.title +: any.listing.rawTitle.toSeq).exists(BillingPieces.quotesTwoJoined) ||
    slashesTwoWorks(any.listing, ranked) || andJoinsTwoWorks(any.listing, ranked) ||
    (MultiFilmBill.billsTwoWorks(any.listing) && !eligibleOf(ranked).exists(_.category("title").contains("exact")) &&
      MultiFilmBill.billsTwoWholeWorks(any.listing)) ||
      markerOf(ranked).exists(marker => !eligibleOf(ranked).exists(scored => MultiFilmBill.namedBy(marker, scored.candidate.film)) &&
        billedAlone(ranked).isEmpty)
  }

  /** Does a SLASH join two works — every piece of the title a slash parts the whole title of one candidate, a leading
   *  article aside, and no candidate titled as two pieces? UK Savoy Cinema Corby's "We're Going on a Bear Hunt / Tiger
   *  Who Came to Tea" bills the two shorts as one programme, as the "+" bills do; one film's titles either side of the
   *  slash ("Le Fabuleux Destin d'Amélie Poulain / Die fabelhafte Welt der Amélie") are its own, and a piece naming no
   *  candidate ("/ Baby Friendly") no second work. */
  private[identity] def slashesTwoWorks(listing: IdentityMeasures.Listing, ranked: Seq[Scored]): Boolean = {
    def slashed(title: String) = {
      val parts  = SlashBreak.split(title).toSeq
      val pieces = parts.map(pieceKey).filter(_.nonEmpty)
      // a format, a screening note or an event after the slash is no second work, whatever film carries its name
      // ("Baranek Shaun i kudłata bestia / dubbing", '"LALKA" / Premiera')
      pieces.sizeIs >= 2 && parts.forall(MultiFilmBill.namesAWork) && {
        val named = pieces.map(piece => eligibleOf(ranked).filter(_.candidate.film.titles.exists(pieceKey(_) == piece)).map(_.candidate.tmdbId).toSet)
        named.forall(_.nonEmpty) && named.combinations(2).forall { case Seq(a, b) => (a intersect b).isEmpty }
      }
    }
    // read for every rule's every ranking: a title with no slash is split for nothing
    def read(title: String) = title.indexOf('/') >= 0 && slashed(title)
    read(listing.title) || listing.rawTitle.exists(raw => raw != listing.title && read(raw))
  }
  private[identity] val SlashBreak = """\s+/\s+""".r
  /** A title's words, a leading article aside: what a piece of a bill is matched to a film's whole title by. */
  private def pieceKey(text: String): Seq[String] = {
    val words = services.titles.TitleContainment.tokens(text)
    if (words.sizeIs >= 2 && IdentityMeasures.LeadingArticles(words.head)) words.tail else words
  }

  /** Does an "AND" join two works — the title's one "and" parting two pieces of two words or more, each the title of
   *  another candidate (denied or not: a denial says the film is not the BILLING's, not that no film bears the piece's
   *  title), a word of four or more apart at most, and no candidate titled as the whole billing? UK Hyde Park Picture
   *  House's "The Tiger Who Came for Tea and We're Going on a Bear Hunt" bills the two shorts as one programme, as the
   *  "+" and the slash bills do: it took The Tiger Who Came to Tea at 49.6%. A film's own title joining two phrases
   *  ("Willy Wonka and the Chocolate Factory") is its candidate's whole title; a one-word piece ("The Boy and the Heron"
   *  beside a film titled "The Boy") is too short to name a work of its own. */
  private[identity] def andJoinsTwoWorks(listing: IdentityMeasures.Listing, ranked: Seq[Scored]): Boolean = {
    // read for every rule's every ranking: a title with no "and" is split for nothing
    def read(title: String) = AndBreak.findFirstIn(title).isDefined && joinsTwoWorks(title, ranked)
    read(listing.title) || listing.rawTitle.exists(raw => raw != listing.title && read(raw))
  }
  /** Does `title`'s "and" join two pieces naming a work each, no record titled the whole, each named by records the
   *  other is not? */
  private def joinsTwoWorks(title: String, ranked: Seq[Scored]): Boolean = AndBreak.split(title).toSeq match {
    case Seq(first, second) if Seq(first, second).forall(piece => pieceKey(piece).sizeIs >= 2 && MultiFilmBill.namesAWork(piece)) =>
      val whole = unjoinedKey(title)
      !ranked.exists(_.candidate.film.titles.exists(unjoinedKey(_) == whole)) && {
        val named = Seq(first, second).map(piece => ranked.filter(_.candidate.film.titles.exists(t => nearlyKey(pieceKey(t), pieceKey(piece)))).map(_.candidate.tmdbId).toSet)
        named.forall(_.nonEmpty) && (named.head intersect named.last).isEmpty
      }
    case _ => false
  }
  private def unjoinedKey(text: String): Seq[String] = pieceKey(text).filterNot(_ == "and")
  /** The same words, or as many, four at least, all but one alike. */
  private def nearlyKey(title: Seq[String], piece: Seq[String]): Boolean =
    title == piece || (title.sizeIs == piece.size && piece.sizeIs >= 4 && title.zip(piece).count((a, b) => a != b) == 1)
  private val AndBreak = """(?i)\s+and\s+""".r

  /** The one film a marathon billing one title names ([[MultiFilmBill.billedOne]]): the one eligible candidate titled
   *  exactly so, when no eligible candidate's title extends it — PL "Inna Mamusia - maraton" is Inna mamusia (2026);
   *  "Władca Pierścieni - maraton" bills Jackson's films, which extend Bakshi's "Władca Pierścieni". */
  private[identity] def billedAlone(ranked: Seq[Scored]): Option[Int] =
    ranked.headOption.flatMap(_.listing.billedOne).flatMap { title =>
      val named    = IdentityMeasures.key(title)
      val eligible = eligibleOf(ranked)
      def keys(scored: Scored) = scored.candidate.film.titles.map(IdentityMeasures.key)
      val extended = eligible.exists(scored => keys(scored).exists(key => key != named && key.startsWith(named)))
      eligible.filter(scored => keys(scored).contains(named)).map(_.candidate.tmdbId).distinct match {
        case Seq(one) if !extended && named.nonEmpty => Some(one)
        case _                                       => None
      }
    }

  /** The word the ranked listing bills several films by, if any ([[MultiFilmBill.marker]]). */
  private[identity] def markerOf(ranked: Seq[Scored]): Option[String] =
    ranked.headOption.flatMap(_.listing.billMarker)

  /** Is `taken` no film of a programme the listing bills by a word — the word its own title's, if the listing has one? */
  private[identity] def takesNoBill(ranked: Seq[Scored])(taken: (Accepted, String)): Boolean =
    markerOf(ranked).forall(marker => MultiFilmBill.namedBy(marker, taken._1._1.candidate.film) ||
      billedAlone(ranked).contains(taken._1._1.candidate.tmdbId))}
