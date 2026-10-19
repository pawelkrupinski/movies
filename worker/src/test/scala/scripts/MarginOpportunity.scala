package scripts

import models.Country
import services.identity.{IdentityCalibration, IdentityResolver}
import services.movies.TitleNormalizer
import tools.UnmatchedClusters

/**
 * Experiment `experiment/margin-take`: how many of the unmatched-cluster capture's listings left WITHOUT a film (after
 * the agreement stage) have a best undenied candidate below the rating cut that dominates its runner-up. One TSV row
 * per node: country, venue, raw title, listings, cut, best film, its probability, runner-up film, its probability
 * (empty when none), the captured model basis. The grid over (floor, ratio) is counted from the rows.
 * {{{ MarginOpportunity <out.tsv> [cc...] }}}
 */
object MarginOpportunity {
  def main(args: Array[String]): Unit = {
    val out       = java.nio.file.Path.of(args.head)
    val countries = if (args.length > 1) args.toSeq.tail else Seq("pl", "uk", "de", "es", "us")
    val cut       = IdentityCalibration.resolver.ratingCut
    val rows = countries.flatMap { cc =>
      val country = Country.all.find(_.code == cc).get
      val capture = UnmatchedClusters.read(UnmatchedClusters.fixturePath(country))
      val outcome = UnmatchedClusters.replay(capture)
      val taken   = UnmatchedClusters.takes(capture, outcome).map(t => (t.venue, t.rawTitle)).toSet
      val byKey   = capture.listings.map(l => l.key -> l).toMap
      val modelOf = outcome.model.decisionOf
      IdentityResolver.evidenceOf(capture.listings, new UnmatchedClusters.Replay(capture), TitleNormalizer.forCountry(country))(_ => true)
        .flatMap { node =>
          val listings = node.keys.flatMap(byKey.get)
          val untaken  = listings.filterNot(l => taken((l.venue, l.rawTitle)))
          val eligible = node.candidates.filterNot(_.denied)
          Option.when(untaken.nonEmpty && eligible.nonEmpty) {
            val first = untaken.head
            def named(c: IdentityResolver.CandidateEvidence) = s"${c.tmdbId} ${c.film.title} (${c.film.year.getOrElse("?")})"
            Seq(cc, first.venue, first.rawTitle, untaken.size.toString, f"$cut%.4f", named(eligible.head), f"${eligible.head.probability}%.4f",
              eligible.lift(1).fold("")(named), eligible.lift(1).fold("")(c => f"${c.probability}%.4f"),
              modelOf.get(first.key).fold("-")(_.basis.toString)).map(_.replace('\t', ' ')).mkString("\t")
          }
        }
    }
    java.nio.file.Files.writeString(out, rows.mkString("", "\n", "\n"))
    println(s"${rows.size} rows → $out")
  }
}
