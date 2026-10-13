package tools

import models.Source
import services.identity.ResolverDecision
import services.movies.{ListingKey, StoredMovieRecord}

import scala.collection.mutable

/** One venue's listing of a replayed film: its slot key (`"<cinema>␟<titleKey>"`, what production's
 *  [[ProdFilm.slots]] holds too), the venue, and the title the venue billed. */
final case class RunListing(slotKey: String, venue: String, raw: String) {
  def render: String = s"$venue: '$raw'"
}

/** A film the replay produced, as the per-film diff reads it: `covered` names the axes it holds a value on,
 *  under [[ProdFilms.Axes]]' names. */
final case class RunFilm(id: String, title: String, tmdbId: Option[Int], imdbId: Option[String], covered: Set[String],
                         listings: Seq[RunListing])

object RunFilm {
  /** A stored film as the diff reads it: each cinema slot a listing, billed under the venue's raw title. */
  def of(stored: StoredMovieRecord): RunFilm = {
    val record = stored.record
    RunFilm(stored.id.value, s"${stored.title}${stored.year.fold("")(year => s" ($year)")}", record.tmdbId, record.imdbId,
      CorpusCoverage.covered(record),
      record.cinemaSlots.flatMap { case (source, slot) =>
        Source.cinemaOf(source).map(cinema => RunListing(source.displayName, cinema.displayName, slot.rawTitle.orElse(slot.title).getOrElse("")))
      }.sortBy(_.slotKey))
  }
}

/** The identity model's decision on a run film, as the diff shows it: `cause`, why it took no film in a few
 *  words the gap is grouped by; `rendered`, the decision with its whole explanation. */
final case class IdentityVerdict(cause: String, rendered: String)

object IdentityVerdict {
  import ResolverDecision.Basis

  def of(decision: ResolverDecision): IdentityVerdict = {
    val cause = decision.basis match {
      case Basis.NoCandidate    => "no TMDB search hit"
      case Basis.NoEvidence     => "lookups unanswered"
      case Basis.Vetoed         => "vetoed"
      case Basis.BelowThreshold => "below the acceptance cut"
      case Basis.Withdrawn      => "withdrawn by the correction stage"
      case Basis.Event          => "billed as an event"
      case matched              => s"decided $matched, but the record holds no id"
    }
    IdentityVerdict(cause, decision.render)
  }

  /** Each of `films`' verdicts, read off the decision on the first of its listings the model decided. */
  def lookup(films: Seq[StoredMovieRecord], decisionOf: Map[ListingKey, ResolverDecision]): RunFilm => Option[IdentityVerdict] = {
    val byId = films.iterator.map(stored => stored.id.value -> stored).toMap
    film => byId.get(film.id).flatMap(_.record.data.iterator
      .flatMap { case (source, slot) => ListingKey.ofSource(source, slot) }.flatMap(decisionOf.get).nextOption())
      .map(of)
  }
}

/** One film's difference from production on one axis. `run` is the replayed film (absent for a film only
 *  production holds); `prod`, the production films sharing a slot with it. */
final case class FilmGap(axis: String, side: FilmGap.Side, cause: String, run: Option[RunFilm], prod: Seq[ProdFilm],
                         verdict: Option[IdentityVerdict])

object FilmGap {
  enum Side(val label: String) {
    case RunLacks extends Side("prod has, run lacks")
    case RunOnly  extends Side("run has, prod lacks")
    case Differs  extends Side("different film")
  }
}

/**
 * Which films a convergence leg's coverage differs from production's on, and why — the per-film account of
 * what [[ProdCoverageBaseline.report]] gives only as counts.
 *
 * A run film and a production film are the same film when they share a SCREENING SLOT
 * (`"<cinema>␟<titleKey>"`): the one identity both sides derive from the same listing, whatever either
 * folded it into. The films linked that way fall into components, and a component's shape says how the
 * two sides grouped its listings — one run film to one prod film is agreement, more run films than prod
 * films a SPLIT, fewer a MERGE, a run film alone a listing production does not screen, a prod film alone
 * one the corpus does not carry. Those account for the `films` axis exactly: run − prod is the sum over
 * components of their run films less their prod films.
 *
 * Every other axis is compared film by film over the links: production holding a value the run lacks
 * (grouped by the run's identity decision — no search hit, vetoed, below the cut — or by the split that
 * gave it to a sibling), the run holding one production lacks, and for `tmdbId` the run taking a
 * different film. Bounded: it holds the two film lists the leg already reads and their slot keys.
 */
final case class FilmCoverageDiff(runFilms: Int, prodFilms: Int, sharedSlots: Int, gaps: Seq[FilmGap],
                                  splits: Seq[(Seq[ProdFilm], Seq[RunFilm])], merges: Seq[(Seq[ProdFilm], Seq[RunFilm])],
                                  regrouped: Seq[(Seq[ProdFilm], Seq[RunFilm])], runOnly: Seq[RunFilm], prodOnly: Seq[ProdFilm]) {
  import FilmCoverageDiff._

  /** The causes behind each side of `axis`' gap, largest first. */
  def causes(axis: String): Seq[(FilmGap.Side, Seq[(String, Int)])] =
    FilmGap.Side.values.toSeq.flatMap { side =>
      val ofSide = gaps.filter(gap => gap.axis == axis && gap.side == side)
      Option.when(ofSide.nonEmpty)(side -> ofSide.groupMapReduce(_.cause)(_ => 1)(_ + _).toSeq.sortBy { case (cause, n) => (-n, cause) })
    }

  /** The lines a log leads with: where the film count goes, then each axis' gap by cause. */
  def summary: Seq[String] = {
    def surplus(components: Seq[(Seq[ProdFilm], Seq[RunFilm])]) = components.map { case (p, r) => r.size - p.size }.sum
    val films =
      s"films run=$runFilms prod=$prodFilms: +${runOnly.size} run-only, -${prodOnly.size} prod-only, " +
        s"+${surplus(splits)} from ${splits.size} split(s), ${surplus(merges)} from ${merges.size} merge(s)" +
        (if (regrouped.isEmpty) "" else s", ${regrouped.size} regrouped") + s" — joined on $sharedSlots shared slot key(s)"
    films +: ProdFilms.Axes.flatMap { axis =>
      val sides = causes(axis)
      Option.when(sides.nonEmpty)(s"$axis: " + sides.map { case (side, byCause) =>
        s"${side.label} ${byCause.map(_._2).sum} (${byCause.map { case (cause, n) => s"$cause $n" }.mkString(", ")})"
      }.mkString("; "))
    }
  }

  /** One line per film for `axes` (`films` for the components behind the count), at most `limit` of them and
   *  a tail naming how many more the full report holds. Largest cause first, so the first lines are where to look. */
  def details(axes: Set[String], limit: Int): Seq[String] = {
    val lines = entries(axes).map(_.head)
    lines.take(limit) ++ Option.when(lines.size > limit)(s"…and ${lines.size - limit} more in the full report")
  }

  /** Everything: every film on every axis, each with its listings and its whole identity decision. */
  def full: Seq[String] = summary ++ entries(ProdFilms.Axes.toSet + "films").flatMap("" +: _)

  private def entries(axes: Set[String]): Seq[Seq[String]] = {
    val byAxis = ProdFilms.Axes.filter(axes).flatMap { axis =>
      val order = causes(axis).flatMap { case (side, byCause) => byCause.map { case (cause, _) => (side, cause) } }.zipWithIndex.toMap
      gaps.filter(_.axis == axis).sortBy(gap => (order((gap.side, gap.cause)), gap.run.fold(gap.prod.map(_.key).mkString)(_.title)))
        .map(entry)
    }
    val films = if (!axes("films")) Nil else
      splits.map(component("split", _)) ++ merges.map(component("merge", _)) ++ regrouped.map(component("regrouped", _)) ++
        runOnly.map(film => Seq(s"films  run-only  '${film.title}' ← ${listingsOf(film)}")) ++
        prodOnly.map(film => Seq(s"films  prod-only '${film.key}' ← ${slotsOf(film)}${tmdbOf(film)}"))
    byAxis ++ films
  }

  private def entry(gap: FilmGap): Seq[String] = {
    val runSide  = gap.run.fold("(no run film)")(film => s"'${film.title}'${film.tmdbId.fold("")(id => s" tmdb $id")} ← ${listingsOf(film)}")
    val prodSide = if (gap.prod.isEmpty) "" else " | " + gap.prod.map(film => s"prod${tmdbOf(film)} '${film.key}'").mkString(", ") +
      (if (gap.run.isEmpty) s" ← ${gap.prod.map(film => slotsOf(film)).mkString("; ")}" else "")
    val decision = gap.verdict.fold("")(verdict => s" | decision: ${verdict.rendered.linesIterator.take(2).map(_.trim).mkString(" — ")}")
    clip(s"${gap.axis}  ${gap.side.label} [${gap.cause}] $runSide$prodSide$decision") +:
      gap.verdict.toSeq.flatMap(_.rendered.linesIterator.map("    " + _))
  }

  private def component(kind: String, linked: (Seq[ProdFilm], Seq[RunFilm])): Seq[String] = {
    val (prod, run) = linked
    clip(s"films  $kind  ${run.size} run film(s) / ${prod.size} prod film(s): " +
      run.map(film => s"'${film.title}'${film.tmdbId.fold("")(id => s" tmdb $id")}").mkString(", ") + " | " +
      prod.map(film => s"prod${tmdbOf(film)} '${film.key}'").mkString(", ")) +:
      (run.map(film => s"    run '${film.title}' ← ${listingsOf(film, Int.MaxValue)}") ++
        prod.map(film => s"    prod '${film.key}' ← ${slotsOf(film, Int.MaxValue)}"))
  }
}

object FilmCoverageDiff {

  /** A replay's screening films against production's: what a leg calls. `decisionOf` is the identity model's
   *  decisions, when it could be read. */
  def ofReplay(screening: Seq[StoredMovieRecord], prod: Seq[ProdFilm],
               decisionOf: Option[Map[ListingKey, ResolverDecision]]): FilmCoverageDiff =
    FilmCoverageDiff(screening.map(RunFilm.of), prod, decisionOf.fold((_: RunFilm) => Option.empty[IdentityVerdict])(IdentityVerdict.lookup(screening, _)))

  def apply(run: Seq[RunFilm], prod: Seq[ProdFilm], verdictOf: RunFilm => Option[IdentityVerdict]): FilmCoverageDiff = {
    val prodBySlot = prod.flatMap(film => film.slots.map(_ -> film)).groupMap(_._1)(_._2)
    val linksOf: Map[String, Seq[ProdFilm]] =
      run.map(film => film.id -> film.listings.flatMap(listing => prodBySlot.getOrElse(listing.slotKey, Nil)).distinctBy(_.id)).toMap
    val sharedSlots = run.iterator.flatMap(_.listings.map(_.slotKey)).count(prodBySlot.contains)

    // Components of the run–prod link graph, by union-find over "r:<id>" / "p:<id>".
    val parent = mutable.HashMap.empty[String, String]
    def root(node: String): String = {
      val up = parent.getOrElseUpdate(node, node)
      if (up == node) node else { val top = root(up); parent(node) = top; top }
    }
    run.foreach(film => linksOf(film.id).foreach(p => parent(root(s"r:${film.id}")) = root(s"p:${p.id}")))
    val componentOf = (run.map(film => root(s"r:${film.id}") -> Left(film)) ++ prod.map(film => root(s"p:${film.id}") -> Right(film)))
      .groupMap(_._1)(_._2).values.toSeq
      .map(members => (members.collect { case Right(p) => p }.sortBy(_.id), members.collect { case Left(r) => r }.sortBy(_.id)))
      .sortBy { case (p, r) => (r.headOption.map(_.id), p.headOption.map(_.id)) }
    val splits    = componentOf.filter { case (p, r) => p.nonEmpty && r.size > p.size }
    val merges    = componentOf.filter { case (p, r) => r.nonEmpty && p.size > r.size }
    val regrouped = componentOf.filter { case (p, r) => r.size == p.size && r.size > 1 }
    val runOnly   = componentOf.collect { case (p, r) if p.isEmpty => r }.flatten
    val prodOnly  = componentOf.collect { case (p, r) if r.isEmpty => p }.flatten
    val splitSiblings: Map[String, Seq[RunFilm]] = splits.flatMap { case (_, r) => r.map(film => film.id -> r.filterNot(_.id == film.id)) }.toMap

    val verdicts = mutable.HashMap.empty[String, Option[IdentityVerdict]]
    def verdict(film: RunFilm) = verdicts.getOrElseUpdate(film.id, verdictOf(film))

    val gaps = ProdFilms.Axes.flatMap { axis =>
      val linked = run.flatMap { film =>
        val prods    = linksOf(film.id)
        val prodHas  = prods.exists(_.covered(axis))
        val runHas   = film.covered(axis)
        def gap(side: FilmGap.Side, cause: String, withVerdict: Boolean) =
          FilmGap(axis, side, cause, Some(film), prods, if (withVerdict) verdict(film) else None)
        if (prods.isEmpty) Option.when(runHas)(gap(FilmGap.Side.RunOnly, "listing absent in prod", withVerdict = false))
        else if (prodHas && !runHas) Some {
          if (splitSiblings.getOrElse(film.id, Nil).exists(_.covered(axis))) gap(FilmGap.Side.RunLacks, "split — a sibling run film holds it", withVerdict = false)
          else if (axis == "tmdbId") gap(FilmGap.Side.RunLacks, verdict(film).fold("no identity decision")(_.cause), withVerdict = true)
          else if (!film.covered("tmdbId")) gap(FilmGap.Side.RunLacks, "run did not identify the film", withVerdict = true)
          else gap(FilmGap.Side.RunLacks, if (axis == "imdbId") "identified, no IMDb id" else "identified, not rated", withVerdict = false)
        }
        else if (runHas && !prodHas) Some(gap(FilmGap.Side.RunOnly, "prod lacks it", withVerdict = false))
        else Option.when(axis == "tmdbId" && runHas && film.tmdbId.exists(id => !prods.flatMap(_.tmdbId).contains(id)))(
          gap(FilmGap.Side.Differs, "a different film", withVerdict = true))
      }
      val unreplayed = prodOnly.filter(_.covered(axis)).map(film =>
        FilmGap(axis, FilmGap.Side.RunLacks, "prod-only listing — no run film carries its slots", None, Seq(film), None))
      linked ++ unreplayed
    }
    FilmCoverageDiff(run.size, prod.size, sharedSlots, gaps, splits, merges, regrouped, runOnly, prodOnly)
  }

  /** How long one log line may get — the listings of a chain release run to hundreds of venues. */
  private val LineLimit = 400

  private def clip(line: String): String = if (line.length <= LineLimit) line else line.take(LineLimit - 1) + "…"

  private def listingsOf(film: RunFilm, limit: Int = 3): String =
    film.listings.take(limit).map(_.render).mkString("; ") + (if (film.listings.sizeIs > limit) s" (+${film.listings.size - limit} more)" else "")

  private def slotsOf(film: ProdFilm, limit: Int = 3): String =
    film.slots.take(limit).map(slot => slot.split('␟') match {
      case Array(venue, title) => s"$venue: '$title'"
      case _                   => slot
    }).mkString("; ") + (if (film.slots.sizeIs > limit) s" (+${film.slots.size - limit} more)" else "")

  private def tmdbOf(film: ProdFilm): String =
    film.tmdbId.fold("")(id => s" tmdb $id${film.tmdbBasis.fold("")(basis => s" ($basis)")}")
}
