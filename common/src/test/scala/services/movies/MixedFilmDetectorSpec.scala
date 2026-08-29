package services.movies

import models.{Helios, KinoMuranow, Multikino, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Both cases here are production rows, with the cinemas' own published fields.
 *
 * The detector's whole job is to distinguish "two cinemas describe one film to
 * different depths" — overwhelmingly the normal case, and one that must never be
 * split — from "two cinemas are describing different films", which no single
 * tmdbId can serve.
 */
class MixedFilmDetectorSpec extends AnyFlatSpec with Matchers {

  private def slot(director: Seq[String], original: Option[String], runtime: Option[Int] = None) =
    SourceData(title = Some("x"), director = director, originalTitle = original, runtimeMinutes = runtime)

  // The two real splits are corroborated by runtime or year, so the cases that
  // MUST split carry them; the cases that must NOT split carry agreeing ones.

  "a row whose cinemas publish different original titles" should "split, with the odd one out as the stray" in {
    // "Obcy": 2 cinemas on Ozon's L'étranger, 1 on Brandt Andersen's film.
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino    -> slot(Seq("François Ozon"), Some("L'étranger"), Some(120)),
      Helios       -> slot(Seq("François Ozon"), Some("L’Étranger"), Some(122)),
      KinoMuranow  -> slot(Seq("Brandt Andersen"), Some("I Was A Stranger"), Some(103))))

    val strays = MixedFilmDetector.strays(record, titleNormalizer)
    strays.map(_._1) shouldBe Seq(KinoMuranow: Source)
  }

  // ("Joanna d'Arc" — the other production row — is covered below, with the years
  // its cinemas actually publish; a differing title needs corroborating.)

  // ── What must NOT split ───────────────────────────────────────────────────

  "cinemas describing ONE film to different depths" should "not split" in {
    // The overwhelmingly common shape: one publishes a director, another doesn't.
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("Michel Franco"), Some("Dreams: Sueños")),
      Helios    -> slot(Seq.empty, None)))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  it should "not split on punctuation or case in the original title" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("François Ozon"), Some("L'étranger")),
      Helios    -> slot(Seq("François Ozon"), Some("L’Étranger"))))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  it should "not split when one cinema omits the director the other publishes" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("Luc Besson"), Some("Joan of Arc")),
      Helios    -> slot(Seq.empty, Some("Joan of Arc"))))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  /** Cinemas credit different ROLES for the same film. "Drzewo magii" is directed
   *  by Ben Gregor and written by Simon Farnaby, and cinemas print one or the
   *  other, so the names never overlap while the film is plainly the same one.
   *  Treating a differing director as evidence of a second film churned this and
   *  two others on every scrape tick (`ReScrapeIdempotencySpec`) — which is why
   *  the original title, not the director, is what decides. */
  it should "not split when cinemas credit different people for the same film" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("Ben Gregor"), Some("The Magic Faraway Tree")),
      Helios    -> slot(Seq("Simon Farnaby"), Some("The Magic Faraway Tree"))))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
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

  it should "not split on a differing director when neither publishes an original title" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("Ben Gregor"), None),
      Helios    -> slot(Seq("Simon Farnaby"), None)))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  /** What cinemas actually publish, measured against TMDB across the corpus: a
   *  fifth of original titles "disagree", and almost all of that is spelling
   *  variants and decorations rather than a different film — "Terminator 2:
   *  Judgement Day (re-release)" beside another cinema's "Terminator 2: Judgment
   *  Day". Folding each title to one string and testing containment calls those
   *  two different films; comparing WORDS does not. */
  it should "not split on a spelling variant or a decoration in the original title" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq.empty, Some("Terminator 2: Judgement Day (re-release)")),
      Helios    -> slot(Seq.empty, Some("Terminator 2: Judgment Day"))))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  it should "not split when one cinema publishes the English title and another the original" in {
    // Also real: a cinema prints the international release title where TMDB (and
    // another cinema) carry the original. They share the distinctive word.
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq.empty, Some("Ghost in the Shell")),
      Helios    -> slot(Seq.empty, Some("GHOST IN THE SHELL"))))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  /** A third of the corpus's original-title disagreements are the SAME film named
   *  in two languages — "Candidates of Death" beside "Kandydaci śmierci", "Otto e
   *  mezzo" beside "8½". No word bridges those, so on the title alone two cinemas
   *  listing one film that way would split it. Runtime and year are language-proof
   *  and agree when it really is one film. */
  it should "not split two languages of one title when runtime and year agree" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq.empty, Some("Candidates of Death"), Some(94)).copy(releaseYear = Some(2026)),
      Helios    -> slot(Seq.empty, Some("Kandydaci śmierci"),   Some(94)).copy(releaseYear = Some(2026))))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  it should "still split when the runtimes disagree as well as the titles" in {
    // "Obcy": Ozon's 120 minutes against Brandt Andersen's 103.
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino   -> slot(Seq("François Ozon"), Some("L'étranger"), Some(120)).copy(releaseYear = Some(2025)),
      KinoMuranow -> slot(Seq("Brandt Andersen"), Some("I Was A Stranger"), Some(103)).copy(releaseYear = Some(2025))))

    MixedFilmDetector.strays(record, titleNormalizer) should have size 1
  }

  it should "still split when the years disagree as well as the titles" in {
    // "Joanna d'Arc": Besson's 1999 film against the Icelandic 2025 one.
    val record = MovieRecord(data = Map[Source, SourceData](
      KinoMuranow -> slot(Seq("Luc Besson"), Some("Joan of Arc"), Some(160)).copy(releaseYear = Some(1999)),
      Helios      -> slot(Seq.empty, Some("Jóhanna af Örk"), None).copy(releaseYear = Some(2025))))

    MixedFilmDetector.strays(record, titleNormalizer) should have size 1
  }

  /** A cinema can publish a runtime that is simply WRONG, and a wrong runtime beside
   *  a translated title is corroboration that looks impeccable. Production, 2026-08-29:
   *  forty cinemas list "Twoje imię" as "Kimi no na wa" at 110 minutes, Kino Nowe
   *  Horyzonty as "Your Name (re-release)" at 83 — its own page says `czas: 83'` for a
   *  106-minute film. Nothing in the title or the runtime says these are one film; the
   *  director both of them credit does. Left unvetoed the row split on every settle,
   *  the stray resolved to the same tmdbId and folded straight back, and the two chased
   *  each other for the life of the corpus. */
  it should "not split when both sides credit the same director, however far the runtimes drift" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Helios      -> slot(Seq.empty, Some("Kimi no na wa"), Some(110)),
      KinoMuranow -> slot(Seq("Makoto Shinkai"), Some("Kimi no Na wa."), Some(106)),
      Multikino   -> slot(Seq("Makoto Shinkai"), Some("Your Name (re-release)"), Some(83))))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  it should "read a director the same however the cinema orders the name" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Helios    -> slot(Seq("Makoto Shinkai"), Some("Kimi no na wa"), Some(110)),
      Multikino -> slot(Seq("Shinkai Makoto"), Some("Your Name (re-release)"), Some(83))))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  /** The veto is whole-name, not per-word: two directors sharing a given name are
   *  still two directors, and matching the way `titleWords` does would fuse them. */
  it should "still split when the two directors merely share a given name" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino   -> slot(Seq("Michael Bay"), Some("L'étranger"), Some(120)),
      KinoMuranow -> slot(Seq("Michael Mann"), Some("I Was A Stranger"), Some(103))))

    MixedFilmDetector.strays(record, titleNormalizer) should have size 1
  }

  "an incoming listing crediting the row's own director" should "still merge" in {
    // The scrape boundary asks the same question, so it owes the same answer.
    val row = MovieRecord(data = Map[Source, SourceData](
      Helios -> slot(Seq("Makoto Shinkai"), Some("Kimi no na wa"), Some(110))))

    MixedFilmDetector.wouldAddASecondFilm(
      row, Some("Your Name (re-release)"), Some(83), Some(2016), Seq("Makoto Shinkai"), titleNormalizer) shouldBe false
  }

  it should "not split on a differing title alone, with nothing to corroborate it" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq.empty, Some("Candidates of Death"), None),
      Helios    -> slot(Seq.empty, Some("Kandydaci śmierci"),   None)))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  it should "not split a row with a single cinema" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Seq("Michel Franco"), Some("Dreams: Sueños"))))

    MixedFilmDetector.strays(record, titleNormalizer) shouldBe empty
  }

  "the split" should "be a pure function of the row, not of slot order" in {
    val a = MovieRecord(data = Map[Source, SourceData](
      Multikino   -> slot(Seq("François Ozon"), Some("L'étranger")),
      Helios      -> slot(Seq("François Ozon"), Some("L'étranger")),
      KinoMuranow -> slot(Seq("Brandt Andersen"), Some("I Was A Stranger"))))
    val b = MovieRecord(data = Map[Source, SourceData](
      KinoMuranow -> slot(Seq("Brandt Andersen"), Some("I Was A Stranger")),
      Helios      -> slot(Seq("François Ozon"), Some("L'étranger")),
      Multikino   -> slot(Seq("François Ozon"), Some("L'étranger"))))

    MixedFilmDetector.strays(a, titleNormalizer).map(_._1) shouldBe
      MixedFilmDetector.strays(b, titleNormalizer).map(_._1)
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
