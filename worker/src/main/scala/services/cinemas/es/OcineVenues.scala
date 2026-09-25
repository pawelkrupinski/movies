package services.cinemas.es

/**
 * The Ocine venues whose OWN ticketing server is the scrape primary, keyed by
 * the venue's SensaCine theaterId (`SpanishRoster.theaterIdByCinema`) and
 * mapped to the `<slug>` of its ticketing host `tickets.ocine<slug>.es`.
 *
 * Keyed by theaterId rather than display name because Spanish venues are built
 * at runtime from `SpanishRosterData`, and `SpanishRoster` QUALIFIES a display
 * name that collides with another country's — the id is the one handle that
 * cannot be renamed out from under this map.
 *
 * Built 2026-09-25 from the chain's own venue list (the "otros cines" links on
 * cinesocine.es, each venue site's `SiteSettings.ticketing_url`) and matched by
 * hand against the 19 "Ocine …" venues in the Spanish roster. Why these went
 * off SensaCine: eleven of them (Arenys, Blanes, Granollers, Màgic, Mendibil,
 * Platja d'Aro, Premium Aqua, Premium Bahía Real, Premium Estepark, Urban
 * X-Madrid, Vila-seca) had NO programme there — SensaCine's venue page
 * advertised no days at all — and Gavarres only two stray days, while the
 * chain's own server listed 10-24 films each. For the five SensaCine did cover
 * (Girona, Plaza Éboli, Rio Shopping, Roquetes, Serrallo) the own server reaches
 * further: measured at Girona, Serrallo and Rio Shopping, SensaCine advertised
 * 7-10 days ending 2026-10-01..10-17, the chain's own feed 13-17 days reaching
 * 2026-12-15 — with the room on every session, which SensaCine never carries.
 *
 * TWO roster Ocine venues are NOT here and stay on SensaCine:
 *   - Ocine Tudela (E0317) — `tickets.ocinetudela.es` answers on port 80 with a
 *     redirect to https, and port 443 then never completes a TCP connect
 *     (probed 2026-09-25). SensaCine lists its full week, so it is better off
 *     where it is; worth re-probing, since it is a wiring change, not a client.
 *   - Ocine Sant Celoni Altrium (E0745) — the chain links no site or ticketing
 *     server for it at all.
 *
 * The chain runs ten more venues (Copo, 7 Palmas, Gran Vía de Vigo, Lleida, Los
 * Fresnos, Porto Pi, Quadernillos, Tormes, Urban Caleido, El Vendrell) that our
 * roster does not carry — the roster is SensaCine's venue list, and SensaCine
 * does not list them. Adding them is a roster change, not a wiring one.
 */
object OcineVenues {

  val ticketingSlugByTheaterId: Map[String, String] = Map(
    "E0651" -> "arenys",           // Ocine Arenys
    "E0462" -> "blanes",           // Ocine Blanes
    "E0509" -> "gavarres",         // Ocine Gavarres
    "E0362" -> "girona",           // Ocine Girona
    "E0507" -> "granollers",       // Ocine Granollers
    "E0713" -> "magic",            // Ocine Màgic
    "E0537" -> "mendibil",         // Ocine Mendibil
    "E0554" -> "platjadaro",       // Ocine Platja d'Aro
    "E2900" -> "plazaeboli",       // Ocine Plaza Éboli
    "E0474" -> "premiumaqua",      // Ocine Premium Aqua
    "E1045" -> "premiumbahiareal", // Ocine Premium Bahía Real
    "E0925" -> "premiumestepark",  // Ocine Premium Estepark
    "E0796" -> "rioshopping",      // Ocine Rio Shopping
    "E0556" -> "roquetes",         // Ocine Roquetes
    "E0787" -> "serrallo",         // Ocine Serrallo
    "E1004" -> "urbanxmadrid",     // Ocine Urban X-Madrid
    "E0727" -> "vilaseca",         // Ocine Vila-seca
  )
}
