package services.identity.agreement

/**
 * The film database families a cluster TMDB matched to nothing may be identified by when several of them, each on its
 * own, identify the same film (signal-combination experiment, 2026-10-04: "≥3 independent families agree, none names a
 * different film, and the listing's title names the film" — 17 right, 0 wrong titles; every family alone added wrongs,
 * 15–45% of its gains). A family's sources mirror each other and count once. Each family's search is weighed with
 * TMDB's calibration, its search priors' spread scaled by `priorSpread` (chosen per source by cross-validation, REPORT
 * §10: 19 right, 0 wrong, against 17 / 0 with TMDB's own spread).
 */
enum VoterFamily(val label: String, val database: String, val priorSpread: Double, val searchesDirectors: Boolean, val latinTitlesOnly: Boolean,
                 val namesFilms: Boolean = true) {
  /** IMDb's own title and name search, and its records (Cinemeta and OMDb mirror it). */
  case Imdb extends VoterFamily("imdb", "imdb", 1.5, searchesDirectors = true, latinTitlesOnly = false)
  /** Wikidata's film items, and the Wikipedia articles that name them. */
  case Wiki extends VoterFamily("wiki", "wikidata", 0.5, searchesDirectors = true, latinTitlesOnly = false)
  /** Filmweb's search and film records — a voter where it indexes the country's titles (PL, DE, ES). */
  case Filmweb extends VoterFamily("filmweb", "filmweb", 0.5, searchesDirectors = false, latinTitlesOnly = false)
  case Metacritic extends VoterFamily("metacritic", "metacritic", 1.0, searchesDirectors = false, latinTitlesOnly = true, namesFilms = false)
  case RottenTomatoes extends VoterFamily("rt", "rt", 1.5, searchesDirectors = false, latinTitlesOnly = true, namesFilms = false)
}
// `database`: the name a record's cross-ids file the family's own ids under (`SourceRecord.crossIds`), and a fallback
// film's source when the family's id is the one a film stands on (`AgreementStage.identities`). `namesFilms`: its record is
// a film database's, not a review site's page — a film only review sites take is never taken (`AgreementStage`).
// What each family is asked, measured on prod's 20,310 answers (2026-10-05, agreement-question-value.md): a director
// search decided an agreed film on IMDb (10) and Wikidata (8), never on Filmweb, Metacritic or Rotten Tomatoes
// (`searchesDirectors`); Rotten Tomatoes' and Metacritic's English searches answer nothing useful for a title with no
// Latin-script word (`latinTitlesOnly`).
