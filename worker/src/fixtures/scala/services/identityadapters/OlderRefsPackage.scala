package services.identityadapters

/** Makes `services.identityadapters` a package in every ref the identity tools drop their tool files into. The worker's
 *  identity adapters moved there from `services.identity.adapters` on 2026-11-18 (the worker cycle's cut: the adapters
 *  sit above the stores, clients and task queue they adapt, the identity model below them), so a tool file reaches them
 *  by importing both packages, in a scope that is `@nowarn("msg=unused import")` (the one a ref does not keep them in is
 *  unused there) -- and in a ref from before the move, without this file, the package would not exist to import. */
private[identityadapters] object OlderRefsPackage
