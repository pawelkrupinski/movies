package services.movies

import tools.SpecClock.given

import models.{Helios, KinoMuranow, Multikino, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Both cases here are production rows, with the cinemas' own published fields.
 *
 * The detector's whole job is to distinguish "two cinemas describe one film to
 * different depths" — overwhelmingly the normal case, and one that must never be
 * told apart — from "two cinemas are describing different films", which no single
 * tmdbId can serve. Asked of two rows' cinemas ([[MixedFilmDetector.describeDifferentFilms]]).
 */
class MixedFilmDetectorSpec extends AnyFlatSpec with Matchers {

  private def slot(director: Seq[String], original: Option[String], runtime: Option[Int] = None) =
    SourceData(title = Some("x"), director = director, originalTitle = original, runtimeMinutes = runtime)

  /** Do the cinemas of row `a` and those of row `b` describe different films? */
  private def different(a: (Source, SourceData)*)(b: (Source, SourceData)*): Boolean =
    MixedFilmDetector.describeDifferentFilms(MovieRecord(data = a.toMap), MovieRecord(data = b.toMap), titleNormalizer)

  // The two real cases are corroborated by runtime or year, so the cases that
  // MUST tell two films apart carry them; the cases that must NOT carry agreeing ones.

  "rows whose cinemas publish different original titles" should "describe different films" in {
    // "Obcy": 2 cinemas on Ozon's L'étranger, 1 on Brandt Andersen's film.
    different(
      Multikino -> slot(Seq("François Ozon"), Some("L'étranger"), Some(120)),
      Helios    -> slot(Seq("François Ozon"), Some("L’Étranger"), Some(122)))(
      KinoMuranow -> slot(Seq("Brandt Andersen"), Some("I Was A Stranger"), Some(103))) shouldBe true
  }

  // ("Joanna d'Arc" — the other production row — is covered below, with the years
  // its cinemas actually publish; a differing title needs corroborating.)

  // `kungfupanda4|2008` (kinowo_us): sixteen venues screening Kung Fu Panda 4 beside
  // one screening the 2008 film. `titleWords` drops anything under four characters,
  // so the "4" that is the entire difference is read apart from the words.
  "a film beside its own numbered sequel" should "describe different films" in {
    def us(title: String, runtime: Int, director: String) =
      SourceData(title = Some(title), originalTitle = Some(title), runtimeMinutes = Some(runtime), director = Seq(director))
    different(Multikino -> us("Kung Fu Panda 4", 94, "Joel Crawford"))(Helios -> us("Kung Fu Panda", 82, "Mark Osborne")) shouldBe true
  }

  // UK prod, 2026-09-16: `hungergamesballadofsongbirdssnakes|2026` held Catching
  // Fire, both Mockingjay parts, and Ballad of Songbirds and Snakes — each
  // independently mis-resolved to one wrong tmdbId, with no runtime or year to
  // corroborate them. Only the curated franchise-siblings list can.
  "curated same-franchise siblings, with no runtime or year at all" should "describe different films" in {
    def odeon(title: String) = SourceData(title = Some(title), originalTitle = Some(title))
    different(Multikino -> odeon("The Hunger Games: Catching Fire"))(
      Helios -> odeon("The Hunger Games: Mockingjay - Part 2")) shouldBe true
  }

  // UK convergence run 35948292875 (2026-09-24), the real Flicks listings: 64 venues
  // calling it "The Hunger Games: Mockingjay - Part 1 (2026)" and 14 calling it plain
  // "... - Part 1", all 123 minutes, all Francis Lawrence. The curated-sibling path
  // read the rerelease year as a different franchise entry.
  "one curated franchise entry listed with and without a rerelease year" should "describe one film" in {
    def flicks(title: String) =
      SourceData(title = Some(title), originalTitle = Some(title), runtimeMinutes = Some(123), director = Seq("Francis Lawrence"))
    different(Multikino -> flicks("The Hunger Games: Mockingjay - Part 1 (2026)"))(
      Helios -> flicks("The Hunger Games: Mockingjay - Part 1")) shouldBe false
  }

  // ── What must NOT tell two films apart ────────────────────────────────────

  // The other side of the sequel arm: a number appearing on one venue's title and not
  // the other's is NOT enough on its own. "Ocean's 8" beside "Ocean's Eight" is one
  // film, and the agreeing runtime is what says so.
  "a number on one title only" should "describe one film when the runtimes agree" in {
    def us(title: String, runtime: Int, director: String) =
      SourceData(title = Some(title), originalTitle = Some(title), runtimeMinutes = Some(runtime), director = Seq(director))
    different(Multikino -> us("Ocean's 8", 110, "Gary Ross"))(Helios -> us("Ocean's Eight", 110, "Gary Ross")) shouldBe false
  }

  "cinemas describing ONE film to different depths" should "describe one film" in {
    // The overwhelmingly common shape: one publishes a director, another doesn't.
    different(Multikino -> slot(Seq("Michel Franco"), Some("Dreams: Sueños")))(Helios -> slot(Seq.empty, None)) shouldBe false
  }

  it should "not tell films apart on punctuation or case in the original title" in {
    different(Multikino -> slot(Seq("François Ozon"), Some("L'étranger")))(Helios -> slot(Seq("François Ozon"), Some("L’Étranger"))) shouldBe false
  }

  it should "not tell films apart when one cinema omits the director the other publishes" in {
    different(Multikino -> slot(Seq("Luc Besson"), Some("Joan of Arc")))(Helios -> slot(Seq.empty, Some("Joan of Arc"))) shouldBe false
  }

  /** Cinemas credit different ROLES for the same film. "Drzewo magii" is directed
   *  by Ben Gregor and written by Simon Farnaby, and cinemas print one or the
   *  other, so the names never overlap while the film is plainly the same one —
   *  which is why the original title, not the director, is what decides. */
  it should "not tell films apart when cinemas credit different people for the same film" in {
    different(Multikino -> slot(Seq("Ben Gregor"), Some("The Magic Faraway Tree")))(
      Helios -> slot(Seq("Simon Farnaby"), Some("The Magic Faraway Tree"))) shouldBe false
  }

  // ── The scrape boundary: don't create the mixed row at all ────────────────

  "an incoming listing for a different film" should "be recognised before it is merged" in {
    val row = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("François Ozon"), Some("L'étranger"), Some(120))))

    MixedFilmDetector.wouldAddASecondFilm(
      row, Some("I Was A Stranger"), Some(103), Some(2024), Seq("Brandt Andersen"), titleNormalizer) shouldBe true
  }

  /** The listing field the smaller cinemas fill with the POLISH title. It differs
   *  from the row's real original title, but agrees on runtime and year — so
   *  corroboration waves it through, where an uncorroborated gate re-diverted nine
   *  known films on every tick. */
  "an incoming listing that merely echoes the Polish title" should "still merge" in {
    val row = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("François Ozon"), Some("L'étranger"), Some(120))))

    MixedFilmDetector.wouldAddASecondFilm(
      row, Some("Obcy"), Some(120), None, Seq.empty, titleNormalizer) shouldBe false
  }

  "an incoming listing with nothing to corroborate it" should "still merge" in {
    val row = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("François Ozon"), Some("L'étranger"), Some(120))))

    MixedFilmDetector.wouldAddASecondFilm(
      row, Some("I Was A Stranger"), None, None, Seq("Brandt Andersen"), titleNormalizer) shouldBe false
  }

  "two rows' cinemas" should "not tell films apart on a differing director when neither publishes an original title" in {
    different(Multikino -> slot(Seq("Ben Gregor"), None))(Helios -> slot(Seq("Simon Farnaby"), None)) shouldBe false
  }

  /** What cinemas actually publish, measured against TMDB across the corpus: a
   *  fifth of original titles "disagree", and almost all of that is spelling
   *  variants and decorations rather than a different film — "Terminator 2:
   *  Judgement Day (re-release)" beside another cinema's "Terminator 2: Judgment
   *  Day". Comparing WORDS does not call those two films. */
  it should "not tell films apart on a spelling variant or a decoration in the original title" in {
    different(Multikino -> slot(Seq.empty, Some("Terminator 2: Judgement Day (re-release)")))(
      Helios -> slot(Seq.empty, Some("Terminator 2: Judgment Day"))) shouldBe false
  }

  it should "not tell films apart when one cinema publishes the English title and another the original" in {
    // A cinema prints the international release title where TMDB (and another cinema)
    // carry the original. They share the distinctive word.
    different(Multikino -> slot(Seq.empty, Some("Ghost in the Shell")))(Helios -> slot(Seq.empty, Some("GHOST IN THE SHELL"))) shouldBe false
  }

  /** A third of the corpus's original-title disagreements are the SAME film named
   *  in two languages — "Candidates of Death" beside "Kandydaci śmierci", "Otto e
   *  mezzo" beside "8½". No word bridges those; runtime and year are language-proof
   *  and agree when it really is one film. */
  it should "not tell two languages of one title apart when runtime and year agree" in {
    different(Multikino -> slot(Seq.empty, Some("Candidates of Death"), Some(94)).copy(releaseYear = Some(2026)))(
      Helios -> slot(Seq.empty, Some("Kandydaci śmierci"), Some(94)).copy(releaseYear = Some(2026))) shouldBe false
  }

  it should "tell films apart when the runtimes disagree as well as the titles" in {
    // "Obcy": Ozon's 120 minutes against Brandt Andersen's 103.
    different(Multikino -> slot(Seq("François Ozon"), Some("L'étranger"), Some(120)).copy(releaseYear = Some(2025)))(
      KinoMuranow -> slot(Seq("Brandt Andersen"), Some("I Was A Stranger"), Some(103)).copy(releaseYear = Some(2025))) shouldBe true
  }

  it should "tell films apart when the years disagree as well as the titles" in {
    // "Joanna d'Arc": Besson's 1999 film against the Icelandic 2025 one.
    different(KinoMuranow -> slot(Seq("Luc Besson"), Some("Joan of Arc"), Some(160)).copy(releaseYear = Some(1999)))(
      Helios -> slot(Seq.empty, Some("Jóhanna af Örk"), None).copy(releaseYear = Some(2025))) shouldBe true
  }

  // The year arm's EDGE: two cinemas' years for ONE film sit at most a year apart
  // (`YearWindow.PublishedAdjacency` — a festival year beside a release year), so
  // differing titles a year apart are one film and two years apart are two.
  it should "read differing titles a year apart as one film, and two years apart as two" in {
    def apart(icelandicYear: Int) =
      different(KinoMuranow -> slot(Seq.empty, Some("Joan of Arc"), None).copy(releaseYear = Some(1999)))(
        Helios -> slot(Seq.empty, Some("Jóhanna af Örk"), None).copy(releaseYear = Some(icelandicYear)))

    apart(2000) shouldBe false
    apart(1998) shouldBe false
    apart(2001) shouldBe true
    apart(1997) shouldBe true
  }

  /** A cinema can publish a runtime that is simply WRONG, and a wrong runtime beside
   *  a translated title is corroboration that looks impeccable. Production, 2026-08-29:
   *  forty cinemas list "Twoje imię" as "Kimi no na wa" at 110 minutes, Kino Nowe
   *  Horyzonty as "Your Name (re-release)" at 83 — its own page says `czas: 83'` for a
   *  106-minute film. The director both of them credit says these are one film. */
  it should "not tell films apart when both sides credit the same director, however far the runtimes drift" in {
    different(
      Helios      -> slot(Seq.empty, Some("Kimi no na wa"), Some(110)),
      KinoMuranow -> slot(Seq("Makoto Shinkai"), Some("Kimi no Na wa."), Some(106)))(
      Multikino   -> slot(Seq("Makoto Shinkai"), Some("Your Name (re-release)"), Some(83))) shouldBe false
  }

  it should "read a director the same however the cinema orders the name" in {
    different(Helios -> slot(Seq("Makoto Shinkai"), Some("Kimi no na wa"), Some(110)))(
      Multikino -> slot(Seq("Shinkai Makoto"), Some("Your Name (re-release)"), Some(83))) shouldBe false
  }

  /** Verify scrape recording #31, kinowo_de: Capitol Lichtspiele Kornwestheim lists "2 Aile
   *  Arasinda" as 2017 by "Ozan Aciktan"; TMDB 1301248 is the 2026 sequel by "Ozan Açıktan".
   *  The year alone is a venue's slip — the director is one man, so the venue does NOT deny the
   *  film. The dotless "ı" is its own letter, not an accented "i": NFD leaves it alone, and a key
   *  that folds by decomposition deleted it ("acktan"), reading one director as two. */
  it should "read a director spelt with a dotless i as the one spelt without it" in {
    val venue = SourceData(title = Some("2 Aile Arasinda"), releaseYear = Some(2017), director = Seq("Ozan Aciktan"))
    val tmdb  = SourceData(title = Some("2 Aile Arasında"), releaseYear = Some(2026), director = Seq("Ozan Açıktan"))

    MixedFilmDetector.creditSamePerson(Seq("Ozan Aciktan"), Seq("Ozan Açıktan"), titleNormalizer) shouldBe true
    MixedFilmDetector.deniesFilm(venue, tmdb, titleNormalizer) shouldBe false
  }

  /** The veto is whole-name, not per-word: two directors sharing a given name are
   *  still two directors, and matching the way `titleWords` does would fuse them. */
  it should "tell films apart when the two directors merely share a given name" in {
    different(Multikino -> slot(Seq("Michael Bay"), Some("L'étranger"), Some(120)))(
      KinoMuranow -> slot(Seq("Michael Mann"), Some("I Was A Stranger"), Some(103))) shouldBe true
  }

  "an incoming listing crediting the row's own director" should "still merge" in {
    // The scrape boundary asks the same question, so it owes the same answer.
    val row = MovieRecord(data = Map[Source, SourceData](
      Helios -> slot(Seq("Makoto Shinkai"), Some("Kimi no na wa"), Some(110))))

    MixedFilmDetector.wouldAddASecondFilm(
      row, Some("Your Name (re-release)"), Some(83), Some(2016), Seq("Makoto Shinkai"), titleNormalizer) shouldBe false
  }

  "two rows' cinemas" should "not tell films apart on a differing title alone, with nothing to corroborate it" in {
    different(Multikino -> slot(Seq.empty, Some("Candidates of Death"), None))(
      Helios -> slot(Seq.empty, Some("Kandydaci śmierci"), None)) shouldBe false
  }

  /** The detector answers from the record's CINEMA SLOTS, so a record that carries none
   *  cannot contradict anything and `describeDifferentFilms` says "not different films".
   *
   *  That default is right — no evidence is not evidence of difference, and refusing on it
   *  would block every adoption of an enrichment-only row — but it makes the answer depend
   *  on how the record was READ. Under the storage split a migrated film's `movies` document
   *  carries no `sourceData` at all (its cinemas are rows in `movie_slots`), so a caller
   *  planning on RAW documents gets `false` for a pair the same caller would get `true` for
   *  on the stitched view. `MongoStagingFolder` reads raw and is the caller to watch.
   *
   *  Pinned rather than fixed, deliberately. The one caller — `FilmCanonicalizer`'s
   *  token-run edge — was probed with a resolved base and an unresolved token-run extension,
   *  slot-less and stitched, at matching and differing years: `StagingFold.planGroup` returns
   *  the SAME rows either way, because `clusterByFilm` separates them regardless. And the
   *  only way a resolved base joins a fold group at all is `reconcileTmdbIds`, i.e. by
   *  SHARING a tmdbId, which the unguarded tmdbId edge unions on before this guard is
   *  consulted. So the blindness is real and currently costs nothing. This test exists so
   *  that stops being true LOUDLY: anything that starts routing a raw-read record here has
   *  to stitch it first. */
  "a record with no cinema slots" should "be unable to contradict, whatever the other side says" in {
    val cinemas = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("François Ozon"), Some("L'étranger"), Some(122))))
    val other   = MovieRecord(data = Map[Source, SourceData](
      KinoMuranow -> slot(Seq("Brandt Andersen"), Some("I Was A Stranger"), Some(103))))
    // The same film pair, read the two ways the split makes possible.
    val migrated = MovieRecord(data = Map.empty)

    withClue("premise: with its cinemas present the pair IS a contradiction: ")(
      MixedFilmDetector.describeDifferentFilms(cinemas, other, titleNormalizer) shouldBe true)
    withClue("a slot-less record reporting a contradiction would block legitimate adoptions " +
             "of enrichment-only rows: ")(
      MixedFilmDetector.describeDifferentFilms(migrated, other, titleNormalizer) shouldBe false)
  }
}
