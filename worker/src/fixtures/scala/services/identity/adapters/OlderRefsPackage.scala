package services.identity.adapters

/** Makes `services.identity.adapters` a package in every ref identity-slice drops its tool files into. The worker's
 *  identity adapters moved there from `services.identity` on 2026-10-19, so a tool file reaches them by importing both
 *  packages, in a scope that is `@nowarn("msg=unused import")` (the one a ref does not keep them in is unused there) —
 *  and in a ref from before the move, without this file, the adapters package would not exist to import. */
private[adapters] object OlderRefsPackage
