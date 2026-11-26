package services.tmdb

/** Makes `services.tmdb` a package in every ref the identity tools drop their tool files into. It holds what
 *  `services.identityadapters` held until 2026-11-26 (the split of that package by concern), so a tool file reaches it
 *  by importing both packages, in a scope that is `@nowarn("msg=unused import")` (the one a ref does not keep it in is
 *  unused there) -- and in a ref from before the split, without this file, the package would not exist to import. */
private[tmdb] object OlderRefsPackage
