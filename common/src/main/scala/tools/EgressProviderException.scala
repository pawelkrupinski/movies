package tools

/**
 * A paid egress PROVIDER (Zyte) failing on its own account — its API answering
 * non-200, or answering without the origin's status — carrying the PROVIDER's
 * status, not the origin's.
 *
 * Deliberately NOT an [[HttpStatusException]]: every caller that branches on one
 * reads its code as the origin's verdict — a 404/410 stamps a detail page gone
 * ([[HttpStatusException.isDurable]]), a 429 opens the origin host's circuit
 * breaker and throttles it, a 403 counts as the origin blocking us. Zyte's 429 is
 * Zyte's throttle and its 403/520 a ban of Zyte's egress, none of them the
 * origin's. Only [[HttpOutcome.classify]] reads [[providerStatus]], for the
 * paid-egress counter that meters the provider itself.
 */
class EgressProviderException(val providerStatus: Int, message: String) extends RuntimeException(message)
