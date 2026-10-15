package services.identity

import services.movies.{TitleContainment, TitleWords}

/** A SHORT film — one TMDB states runs [[MaxMinutes]] or less in every language and cut it holds ([[FilmCuts]]), the
 *  Academy's line — and the screenings that may show one alone.
 *
 *  A venue showing a short says so: by the running time it states, the words of its billing ("Kurzfilm", "short film",
 *  "krótkometrażowy", "court métrage", "cortometraje" — [[TitleWords.Concept.ShortFilm]]), or a TMDB-keyed feed linking
 *  that very short. A listing saying none of it, beside a FEATURE its title names as its own, screens that feature, and a
 *  short is its namesake ([[untaken]]): DE Central im Bürgerbräu's "GINA - mit der Domschule Würzburg"
 *  took the 7-minute Indonesian "Gina", Kommunales Kino Freiburg's "THE COMMUNITY" the 4-minute 2022 "The Community"
 *  (Cineamo links the 64-minute 2025 documentary), Kinocenter Giessen's "Imagine Peace (GLOBALE)" the 40-minute 2010
 *  short (Cineamo links Chiquet's 2026 documentary) — each pooled at under 42%, nothing the venue published for it — and
 *  "L`Estive" ×3 a 3-minute "Summer Meadow" by the English title Cineamo copied from the 2026 documentary it links.
 *  [[SuspectTakes]]' runtime signal reads the same line. */
private[identity] object ShortFilms {

  /** The longest running time of a short, in minutes: the Academy's "40 minutes or less". */
  val MaxMinutes = 40

  /** Every running time TMDB states for `film`, and those of the cuts [[FilmCuts]] names for it. */
  def runtimesOf(film: IdentityMeasures.Film): Seq[Int] = (film.runtimes ++ FilmCuts.of(film.imdbNumber).map(_.runtime)).distinct

  /** Does every running time stated for `film` — at least one — make it a short? */
  def isShort(film: IdentityMeasures.Film): Boolean = {
    val runs = runtimesOf(film)
    runs.nonEmpty && runs.max <= MaxMinutes
  }

  private val ShortWords = TitleWords.foldedPattern(TitleWords.Concept.ShortFilm).r

  /** Does the listing bill a short by a word saying so in one of its titles? */
  def billsAShort(listing: IdentityMeasures.Listing): Boolean =
    (listing.ownTitles ++ listing.originalTitle).exists(title => ShortWords.findFirstIn(TitleContainment.tokens(title).mkString(" ")).isDefined)

  /** Does a title of the billing open on one of `film`'s titles and go on past it with words of its own — none a word an
   *  event, an edition, a print or a date is billed by ([[IdentityMeasures.datesAScreening]])? Then the billing names more
   *  than the short: DE Caligari's "UNLESS WE DANCE - QUEERE FILME ZUR LÄNGSTEN NACHT" is an evening of queer films named
   *  after the 14-minute "Unless We Dance". A short screened alone is billed by its own title — with a talk ("After Eight -
   *  The Story of Satpal Ram + Q&A"), a print ("Warsztat Świętego Mikołaja / dubbing"), or under a series' banner before
   *  it ("WIEDERBEGEGNUNGEN MIT EINEM FILM: NUIT ET BROUILLARD"), or signed with the venue's name ("Warsztat Świętego
   *  Mikołaja / dubbing - Nasze Kino" at Nasze Kino), each still the short's. */
  def billedBeyond(listing: IdentityMeasures.Listing, film: IdentityMeasures.Film, venue: String): Boolean = {
    val filmTitles = film.forms.map(_.words).filter(_.nonEmpty)
    val venueWords = TitleContainment.tokens(venue).toSet
    listing.ownForms.map(_.words).exists { billed =>
      // past the LONGEST of its titles the billing opens on: "After Eight - The Story of Satpal Ram + Q&A" goes on past
      // the record's "After Eight" only by its own full title
      val opening = filmTitles.filter(title => billed.sizeIs >= title.size && billed.startsWith(title)).maxByOption(_.size)
      opening.exists(title => billed.drop(title.size).exists(word => word.lengthIs > 1 && !word.forall(_.isDigit) &&
        !venueWords(word) && !IdentityMeasures.datesAScreening(word)))
    }
  }

  /** Why no rule may take `film` (`tmdbId`) for `listing` — a short it still weighs as a rival ([[Scored.untaken]]), so
   *  that leaving it out lifts no namesake past the cut (DE Filmklubb's "UNSPOKEN": its 3-minute short DENIED, a 2026 film
   *  TMDB titles "Unspoken" only among its others took it at 38.3%) — when it states no running time, bills no short, no TMDB-keyed
   *  feed links it that short (`linked`: the films its feeds link), and `namesake`: its title names a feature-length
   *  film of its pool as its own, whose namesake the short is — or the billing goes on past the short's title
   *  ([[billedBeyond]]). A short no feature shares a title with, billed whole, stays the listing's to take (the half-hour
   *  "Tabby McTat" a venue bills alone), as does one whose
   *  director the listing credits (DE Filmmuseum München's shorts programmes) — [[FamilyScope]] asks that — and a running
   *  time the venue states is its own fact, which its evidence weighs: UK ARC's "Screwball (15)" bills a 20-minute
   *  short's premiere and its Q&A in 60 minutes, DE Lichtblick's "Buddies" [81′] {Rahul Hansraj} is the 2026 film TMDB
   *  still states 5 minutes for. */
  def untaken(listing: IdentityMeasures.Listing, tmdbId: Int, film: IdentityMeasures.Film, linked: Set[Int],
              namesake: Boolean): Option[String] =
    Option.when(isShort(film) && namesake && listing.statedRuntime.isEmpty && !linked(tmdbId) && !billsAShort(listing))(
      s"a ${runtimesOf(film).max}-minute short, its title naming a feature-length film too, or its billing more than the short")
}
