package services.review

import services.identity.ResolverDecision
import services.movies.ListingKey

import java.time.Instant

/** A [[ReviewSource]] over plain values: the reads only, filtered by the keys they are given, as the
 *  Mongo source's queries filter them. */
final class InMemoryReviewSource(
  decisionsHeld: Seq[ResolverDecision],
  slotsHeld:     Map[ListingKey, SlotFacts] = Map.empty,
  pagesHeld:     Map[String, VenueFacts] = Map.empty,
  feedsHeld:     Map[(String, String), ListingFeed] = Map.empty,
  filmsHeld:     Map[Int, FilmCard] = Map.empty,
) extends ReviewSource {
  def decisions(unmatchedOnly: Boolean): Seq[ResolverDecision] =
    if (unmatchedOnly) decisionsHeld.filter(_.film.isEmpty) else decisionsHeld
  def slots(listingKeys: Seq[String]): Map[String, SlotFacts] =
    slotsHeld.map { case (k, v) => ListingKey.serialised(k) -> v }.filter(e => listingKeys.contains(e._1))
  def updatedSince(since: Instant): Map[String, Instant] =
    slotsHeld.collect { case (k, s) if !s.updatedAt.isBefore(since) => ListingKey.serialised(k) -> s.updatedAt }
  def venuePages(urls: Seq[String]): Map[String, VenueFacts] = pagesHeld.filter(e => urls.contains(e._1))
  def feeds(listings: Seq[(String, String)]): Map[(String, String), ListingFeed] = feedsHeld.filter(e => listings.contains(e._1))
  def films(tmdbIds: Seq[Int]): Map[Int, FilmCard] = filmsHeld.filter(e => tmdbIds.contains(e._1))
}

/** A small corpus the review specs share: an unmatched PL cluster held just below the line, a vetoed one,
 *  one with no candidate, and a matched one. */
object ReviewFixtures {
  val Held    = ListingKey.Native("Kino Opalenica", "https://www.bilety24.pl/kino/967-franz-kafka-165208", "FRANZ KAFKA")
  val Vetoed  = ListingKey.Published("Kino Muza", "Macbeth", Some(1971), Seq("Roman Polański"))
  val Nothing = ListingKey.Native("Kino Kosmos", "https://kosmos/robaczki", "Filmowe popołudnie dla dzieci: Robaczki")
  val Matched = ListingKey.Native("Kino Bajka", "https://bajka/klondike", "Klondike")

  val heldDecision = ResolverDecision(Seq(Held), None, 0.14, ResolverDecision.Basis.BelowThreshold, Seq(
    "Kino Opalenica \"FRANZ KAFKA\": own match 1157322 at 41.0% (title exact)",
    "best rejected candidate 1157322 at 86.4% (title exact, year none)",
    "node N Kino Opalenica"), candidate = Some(ResolverDecision.Leaning(1157322, 22963134)))()
  val vetoedDecision = ResolverDecision(Seq(Vetoed), None, 0.37, ResolverDecision.Basis.Vetoed, Seq(
    "best vetoed candidate 1703622 at 62.5%, denied: another house's season production, (title overlap)"))()
  val nothingDecision = ResolverDecision(Seq(Nothing), None, 1.0, ResolverDecision.Basis.NoCandidate, Seq("node N Kino Kosmos"))()
  val matchedDecision = ResolverDecision(Seq(Matched), Some(913760), 0.62, ResolverDecision.Basis.OwnMatch, Seq(
    "Kino Bajka \"Klondike\": own match 913760 at 62.0% by favoured-calibrated (title exact)"))()

  val kafka = FilmCard(1157322, Some("tt22963134"), Some("Franz"), Some("Franz"), Some(2025), Seq("Agnieszka Holland"), Some(127),
    Some("https://image.tmdb.org/t/p/w185/kafka.jpg"), Some("Kafka's life."))
  val macbeth = FilmCard(1703622, None, Some("Royal Ballet and Opera: Macbeth"), None, Some(2026), Seq("Phyllida Lloyd"), Some(180), None, None)
  val klondike = FilmCard(913760, Some("tt15349040"), Some("Klondike"), None, Some(2022), Seq("Maryna Er Gorbach"), Some(100), None, None)

  def source(now: Instant): InMemoryReviewSource = new InMemoryReviewSource(
    Seq(heldDecision, vetoedDecision, nothingDecision, matchedDecision),
    slotsHeld = Map(
      Held    -> SlotFacts(VenueFacts(title = Some("Franz Kafka"), year = Some(2025), directors = Seq("Agnieszka Holland"),
        poster = Some("https://bilety24/kafka.jpg"), synopsis = Some("Biografia Kafki.")), now.minusSeconds(7200)),
      Matched -> SlotFacts(VenueFacts(title = Some("Klondike"), year = Some(2022)), now.minusSeconds(3600))),
    pagesHeld = Map(Held.nativeId -> VenueFacts(runtime = Some(127), cast = Seq("Idan Weiss"))),
    feedsHeld = Map(("Kino Opalenica", "FRANZ KAFKA") -> ListingFeed(Seq("bilety24" -> "165208"), 1, Some("2026-10-10 18:00"), Some("2026-10-10 18:00"))),
    filmsHeld = Map(kafka.tmdb -> kafka, macbeth.tmdb -> macbeth, klondike.tmdb -> klondike))
}
