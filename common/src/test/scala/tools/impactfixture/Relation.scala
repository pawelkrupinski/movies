package tools.impactfixture

// SameFileEdgesSpec's fixture: the class a change touches (TitleRelation, 2026-11-22), in a file of its own.

object Relation { def same(a: String, b: String): Boolean = a == b }
