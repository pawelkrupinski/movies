package models

// ── Network-level detail sources ──────────────────────────────────────────────
// Not a physical venue: a synthetic source that holds the per-film detail
// (synopsis/cast/director/genres/countries/trailer) a chain fetches ONCE per
// network and shares across all its locations, instead of each venue enriching
// its own slot. The venue slots keep their showtimes + filmUrl; the chain slot
// supplies the detail, picked up by `MovieRecord`'s film-level merged accessors.
// It extends `Cinema` so the genre fallback (`cinemaData`, a type-match) sees it,
// and is registered in `Source.all` (for `Source.priority` + Mongo-key lookup) —
// but deliberately NOT in `Cinema.all` / `byCity`, so the per-city/venue
// invariants hold and it never renders as a venue row. See `CinemaCityScraper` /
// `EnrichDetailsHandler`.
case object CinemaCityChain extends Cinema("Cinema City", "Cinema City")
// Network-level detail source for Cineworld UK, mirroring CinemaCityChain: the
// chain's 87 venues share one `detailGroup` ("cineworld"), so their deferred
// detail (synopsis/cast/director/certificate, fetched from the site's
// `movies?ids=` endpoint since the 2026-09-17 relaunch — see
// `services.cinemas.uk.CineworldClient`) lands in this ONE synthetic slot
// instead of an arbitrary venue's. Not in `Cinema.all`, so no scraper/coverage
// obligation; added to `Source.all` for priority + slot-key resolution.
case object CineworldChain extends Cinema("Cineworld", "Cineworld")
// Network-level detail source for Regal US, mirroring CineworldChain: the chain's
// ~400 venues share one `detailGroup` ("regal"), so the per-film detail their
// listing does NOT carry (runtime/rating/synopsis/cast/director, fetched from
// `/api/Movies?hoCode=`) lands in this ONE synthetic slot instead of ~400 copies.
// Not in `Cinema.all`, so no scraper/coverage obligation; added to `Source.all`
// for priority + slot-key resolution.
case object RegalChain extends Cinema("Regal", "Regal")

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
