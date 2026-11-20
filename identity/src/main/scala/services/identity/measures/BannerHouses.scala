package services.identity.measures

import services.identity.measures.StageRelays.{Billing, billing, stageBilling, namesSeasonProduction}
import services.titles.StageHouse.{abbreviates, namesOneHouse, particularWords}

/** Which house each listing banner is, LEARNED from how the film records of its works bill them
 *  (`learn`): no house, banner or abbreviation is known in advance. */
final case class BannerHouses(of: Map[String, String]) {
  /** Do the listing and the film bill the work under one house — the same spelling, or the house
   *  the listing's banner was learned to be? */
  def same(b: Billing): Boolean =
    b.listingHouse == b.filmHouse || of.get(b.listingHouse).contains(b.filmHouse) ||
      // an abbreviation, unless the banner was learned to be another house: "NT Live" learned as National Theatre Live
      // is no "New Theatre", however the initials meet
      (abbreviates(b.listingWords, b.filmWords) && of.get(b.listingHouse).forall(_ == b.listingHouse))
  /** Is the listing's banner known to be ANOTHER house than the film's — and the two names not one house's
   *  ([[namesOneHouse]])? DE's "RBO Live" banner was learned as the house records spelling it so bill, and then denied
   *  "RBO Live 2026/27: Schwanensee (Royal Ballet)" ×80 the season's record billing it "RBO 26/27": one house, two
   *  spellings. Nor is a banner of common words alone ([[particularWords]]) another house: it names none. A record
   *  learning a venue's "Götterdämmerung ROYAL OPERA HOUSE" bills "Götterdämmerung Royal Opera" under "House", and once
   *  "26/27" was a season that billing denied DE "RBO 26/27: Götterdämmerung (Royal Opera)" ×26 its own record. */
  def other(b: Billing): Boolean =
    !same(b) && of.contains(b.listingHouse) && particularWords(b.filmWords).nonEmpty && !namesOneHouse(b.listingWords, b.filmWords)
}

object BannerHouses {
  val Unknown: BannerHouses = BannerHouses(Map.empty)

  /** A banner's house, among the houses whose records bill its works: the one whose name shares
   *  the most of the banner's words ("metropolitan opera: live in hd" is the Metropolitan Opera,
   *  though Royal Ballet & Opera's records bill more of its works), else — on a tie in words —
   *  the one billing the most of its DISTINCT works, at least two: "RBO Cinema Season" is Royal
   *  Ballet & Opera because its Swan Lake and Alice are, though TMDB files its Manon only under
   *  the Met; "NT Live" is National Theatre Live because every play it bills is. One work is a
   *  coincidence — a programme banner showing a film a house also staged — not a house, unless the
   *  banner's words name it. A tie on both is no house. */
  def learn(billings: Iterable[Billing]): BannerHouses =
    BannerHouses(ranking(billings).flatMap { case (banner, ranked) => chosen(ranked).map(banner -> _.house) })

  /** One house a banner's billings name, with how many of the banner's words its name shares
   *  (of the `named` words its name has) and how many of the banner's DISTINCT works it bills. */
  final case class Contender(house: String, spelt: Int, works: Int, named: Int = 1) {
    /** Does the banner NAME the house — two of its words, or a one-word house's one? One word of more
     *  ("maastricht" of "Love in Maastricht") is a coincidence of vocabulary, as one work is of billing. */
    def spells: Boolean = spelt >= 2 || (spelt >= 1 && spelt == named)
    def render: String = s"$house (words $spelt, works $works)"
  }

  /** Each banner's contending houses, best first — what [[learn]] chooses among. */
  def ranking(billings: Iterable[Billing]): Map[String, Seq[Contender]] =
    billings.toSeq.distinct.groupBy(_.listingHouse).map { case (banner, bannerBillings) =>
      val words = bannerBillings.flatMap(_.listingWords).toSet
      banner -> bannerBillings.groupBy(_.filmHouse).toSeq.map { case (house, houseBillings) =>
        val houseWords = houseBillings.flatMap(_.filmWords).toSet
        Contender(house, (houseWords intersect words).size, houseBillings.map(_.work).distinct.size, houseWords.size)
      }.sortBy(contender => (-contender.spelt, -contender.works, contender.house))
    }

  /** The house [[learn]] takes from a banner's ranked contenders, if any. */
  def chosen(ranked: Seq[Contender]): Option[Contender] = {
    val best = ranked.head
    val next = ranked.lift(1)
    val spelt  = best.spells && next.forall(_.spelt < best.spelt)
    val billed = best.works >= 2 && next.forall(runnerUp => runnerUp.spelt < best.spelt || runnerUp.works < best.works)
    Option.when(spelt || billed)(best)
  }

  /** What a listing's candidates say about its banner: how each record of its work bills it — of
   *  its season, when the listing names one (another season's record says nothing about which
   *  house this season's broadcast is). */
  def evidence(l: MeasuredListing, films: Iterable[Film]): Iterable[Billing] =
    films.filter(f => l.seasonYear.isEmpty || namesSeasonProduction(l, f))
      .flatMap(f => billing(l, f).orElse(Option.when(l.seasonYear.isDefined)(stageBilling(l, f)).flatten))
}
