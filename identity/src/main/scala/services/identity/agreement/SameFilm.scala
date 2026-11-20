package services.identity.agreement

import services.identity.measures.{FactRelations, Film, IdentityMeasures}
import services.titles.TitleContainment

/** Whether two film databases' records are ONE film: by a cross-id they share, else by their facts (year, directors,
 *  titles) -- what the agreement stage joins the families' picks by ([[Agreement]]). */
private[identity] object SameFilm {
  /** Is `record` of the film `records` are: the [[sameFilm]] as one of them, and held [[apart]] from none? A record that
   *  dates and credits nothing is the same film as every namesake, so through it alone two films its title shares would
   *  join into one (PL "Homo sapiens?": TMDB's undated Swedish "Homo Sapiens" linking Geyrhalter's 2016 film, which IMDb,
   *  Metacritic and RT take, to Cohn and Duprat's 2025 one, which Filmweb takes — read as Geyrhalter's takers taking a
   *  current film). */
  def joins(records: Seq[SourceRecord], record: SourceRecord): Boolean =
    records.exists(sameFilm(_, record)) && !records.exists(apart(_, record))

  /** The same film by a shared cross-id, else by [[equivalent]] facts. */
  def sameFilm(a: SourceRecord, b: SourceRecord): Boolean = sharesAnId(a, b) || equivalent(a.film, b.film)

  /** Two films by their facts, no cross-id linking them ([[factsApart]]). */
  def apart(a: SourceRecord, b: SourceRecord): Boolean = !sharesAnId(a, b) && factsApart(a.film, b.film)

  private def sharesAnId(a: SourceRecord, b: SourceRecord): Boolean =
    a.crossIds.exists { case (database, id) => b.crossIds.get(database).contains(id) }

  /** Does the model's `vote` name another IMDb or TMDB id than takers whose `records` all date and credit nothing? Such a
   *  record is the same film as every namesake by its facts, so only its ids tell which entry it is (UK "Romeo and
   *  Juliet": Wikidata's undated, uncredited record of Shakespeare's play, IMDb tt2428892, joined the model's vote for
   *  Zeffirelli's 1968 film, tt0063518, by the title alone). Only the takers': a dated taker holds the film by its facts,
   *  and a bare pick joining a dated one is left to their facts (UK "Thanks for coming FFFL": Wikidata's bare
   *  tt36481185 beside IMDb's and RT's 2026 Cavalier). IMDb and TMDB ids only, which every family spells alike —
   *  Wikidata files some sites' ids in a space of its own (UK "Imamura: Black Rain": RT "22294" there, RT's own page
   *  "1035681-black_rain"). */
  private[agreement] def votesAnotherEntry(records: Seq[SourceRecord], vote: SourceRecord): Boolean =
    records.nonEmpty && records.forall(record => bare(record.film)) && records.exists(record =>
      CanonicalIds.exists(database => record.crossIds.get(database).exists(id => vote.crossIds.get(database).exists(_ != id))))
  private val CanonicalIds = Seq("imdb", "tmdb")

  /** A record that dates and credits nothing. */
  private def bare(film: Film): Boolean = film.year.isEmpty && film.directors.forall(_.isEmpty)

  /** The same film by its facts, no cross-id linking them: years within one, no clash of Latin-script directors (one
   *  respelled in the same year and running time is none), and a
   *  shared title — or, titled in two languages, the same director the same year and a shared word of four letters. */
  def equivalent(a: Film, b: Film): Boolean = {
    def titles(f: Film) = f.titles.map(IdentityMeasures.key).filter(_.nonEmpty).toSet
    def words(f: Film)  = f.titles.flatMap(TitleContainment.tokens).filter(_.length >= 4).toSet
    !factsApart(a, b) && ((titles(a) intersect titles(b)).nonEmpty ||
      (sameDirector(a, b) && a.year.isDefined && a.year == b.year && (words(a) intersect words(b)).nonEmpty))
  }

  /** Do the films' facts hold them apart: years more than one apart, or Latin-script directors who are other people (one
   *  respelled in the same year and running time is none)? */
  private def factsApart(a: Film, b: Film): Boolean = {
    val (da, db) = (a.directors.getOrElse(Nil), b.directors.getOrElse(Nil))
    // one person in two transliterations ("Andriej Konczałowski", "Andrei Konchalovsky") is no clash where the films
    // share their year and running time
    val respelled = a.year.isDefined && a.year == b.year && a.runtime.zip(b.runtime).exists { case (x, y) => math.abs(x - y) <= 2 } &&
      FactRelations.namePrefixes(da).intersect(FactRelations.namePrefixes(db)).nonEmpty
    FactRelations.yearsApart(a.year, b.year) || (da.nonEmpty && db.nonEmpty && (da ++ db).forall(tools.LatinScript.lettersOnly) && !sameDirector(a, b) && !respelled)
  }

  private def sameDirector(a: Film, b: Film): Boolean =
    FactRelations.samePerson(a.directors.getOrElse(Nil), b.directors.getOrElse(Nil))
}
