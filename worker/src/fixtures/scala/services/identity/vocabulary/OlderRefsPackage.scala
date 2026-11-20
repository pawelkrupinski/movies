package services.identity.vocabulary

/** Makes `services.identity.vocabulary` a package in every ref the identity tools drop their tool files into. The
 *  identity model's value types (answers, questions, pins, hashes, decorations) moved there from `services.identity` on
 *  2026-11-20 (the knot's cut: the stable types every stage reads, below the measures and the resolver), so a tool file
 *  reaches them by importing both packages, in a scope that is `@nowarn("msg=unused import")` (the one a ref does not
 *  keep them in is unused there) -- and in a ref from before the move, without this file, the package would not exist
 *  to import. */
private[vocabulary] object OlderRefsPackage
