package services.identity

import models.{Helios, KinoMuza}
import services.movies.TitleNormalizer
import FilmTable.{F, listing}

/** PL Kino 1410's Met broadcasts, as the recorded TMDB answered them (recording 36664654811 on):
 *  its own searches found only Royal Ballet & Opera's 2026/27 records, and the Met's 2026/27 Così
 *  record came up only for another venue's "OPERA 2026/2027 - COSÌ FAN TUTTE". Its banner then
 *  learned RBO — one word, two works — and both broadcasts took RBO's productions. */
private[identity] object MetSeasonHouseCase {
  val RboCosi   = 1702775
  val RboCarmen = 1702759
  val MetCosi   = 1703620

  val films: Seq[F] = Seq(F(RboCosi, "Royal Ballet & Opera 2026/27: Cosi fan tutte", 2027, "", 0, 1),
    F(RboCarmen, "Royal Ballet & Opera 2026/27: Carmen", 2026, "", 0, 1),
    F(MetCosi, "The Metropolitan Opera 2026/27: Così fan tutte", 2026, "", 0, 1))

  val cosi: Listing   = listing(KinoMuza, "Cosi fan tutte | metropolitan opera: live in hd 2026/27")
  val carmen: Listing = listing(KinoMuza, "Carmen | metropolitan opera: live in hd 2026/27")
  val other: Listing  = listing(Helios, "OPERA 2026/2027 - COSÌ FAN TUTTE")

  /** The film table, answering as TMDB did: only the accented query reaches the Met's record. */
  def lookups(normalizer: TitleNormalizer): IdentityLookups = {
    val table = new FilmTable(films, normalizer)
    new IdentityLookups {
      def hasDetail(l: Listing): Boolean = table.hasDetail(l)
      def detail(l: Listing): Answer[Option[DetailFacts]] = table.detail(l)
      def film(id: Int): Answer[Option[IdentityMeasures.Film]] = table.film(id)
      def candidates(q: CandidateQuery): Answer[Seq[Hit]] = table.candidates(q) match {
        case Answer.Known(hits) if !q.toString.contains("COSÌ") => Answer.Known(hits.filterNot(_.tmdbId == MetCosi))
        case other => other
      }
    }
  }
}
