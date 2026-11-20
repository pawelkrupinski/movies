package services.identity.measures

/** Makes `services.identity.measures` a package in every ref the identity tools drop their tool files into. The identity
 *  measures (IdentityMeasures, the *Measures objects, MeasuredListing, Film, Measure and what they read) moved there from
 *  `services.identity` on 2026-11-20 (the measures below the resolver's rules and scoring), and Listing, its evidence and
 *  the decision values (Candidate, Scored, ResolverDecision, Resolution, DecisionTrace) followed later that day (lane P7),
 *  so a tool file reaches them by importing both packages, in a scope that is `@nowarn("msg=unused import")` (the one a ref does not keep them in is
 *  unused there) -- and in a ref from before the move, without this file, the package would not exist to import. */
private[measures] object OlderRefsPackage
