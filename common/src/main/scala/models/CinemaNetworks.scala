package models

/** The chains' NETWORK pseudo-cinemas ([[CinemaCityChain]], [[CineworldChain]], [[RegalChain]]) and
 *  which venues each one stands for. A chain's venues publish their listings under their own cinema,
 *  and its detail enricher lands every film page on the one shared network slot (`detailTarget`).
 *  No listing is ever published at one, and none is in `Cinema.all`. */
object CinemaNetworks {
  val all: Seq[Cinema] = Seq(CinemaCityChain, CineworldChain, RegalChain)

  /** Synthetic chain-detail sources ([[all]]) → the venue cinemas whose per-film
   *  detail they hold. A chain fetches a film's synopsis/cast/… ONCE network-wide into
   *  its network slot — Cinema City into [[CinemaCityChain]], Cineworld UK into
   *  [[CineworldChain]], Regal (unwired today) into [[RegalChain]] — rather than each
   *  venue's own slot, so that slot belongs to no single city. A city-scoped synopsis
   *  merge ([[MovieRecord.synopsisForCity]]) consults this map to decide which cities a
   *  chain blurb applies to — namely those where a member venue screens the film; a
   *  network missing here applies NOWHERE (Cineworld's did, until 2026-10-18).
   *
   *  Derived from the chain's own country's venues by the stable "<chain> <branch>"
   *  naming every one of its venues follows (the network itself is the bare chain name,
   *  and isn't in `Cinema.all`), so a newly-added branch is picked up automatically. Its own
   *  country only: another country's roster may hold an unrelated venue of the name,
   *  qualified with its town (Germany's "Cineworld Recklinghausen"). */
  lazy val detailVenues: Map[Source, Set[Cinema]] =
    Seq(CinemaCityChain -> Country.Poland, CineworldChain -> Country.UnitedKingdom, RegalChain -> Country.UnitedStates)
      .map { case (network, country) =>
        (network: Source) -> country.cities.flatMap(_.cinemas).filter(_.displayName.startsWith(s"${network.displayName} ")).toSet
      }.toMap
}
