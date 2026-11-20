package services.identity.vocabulary

/** One constraint edge between two nodes (named by their smallest listing's sort key). */
final case class ResolverEdge(a: String, b: String, must: Boolean, tier: Int, reason: String)
