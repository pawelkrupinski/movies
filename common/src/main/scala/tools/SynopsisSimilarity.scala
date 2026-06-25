package tools

/**
 * Text-closeness scorer for film synopses, used as a TIE-BREAK when a resolver
 * (TMDB, Filmweb) has several candidates that all survive its title/year/
 * director filters. The same film's plot blurbs across sources share their
 * distinctive nouns and character names but are paraphrased — different
 * sentences, different word order, inflected differently — so a good scorer has
 * to reward shared vocabulary without demanding identical strings.
 *
 * == Why character trigrams, not words ==
 *
 * The synopses we compare are Polish (TMDB `overview` vs Filmweb `plot`), and
 * Polish is heavily inflected: the same lemma surfaces as "chłopiec",
 * "chłopca", "chłopcem" across two blurbs of the same film. WORD-level overlap
 * (Jaccard / TF-IDF cosine) undercounts those as three different tokens.
 * CHARACTER-trigram overlap counts them as near-identical (they share
 * "chl"/"hlo"/"lop"/"opc"…), so it is robust to inflection AND to word reorder,
 * with no stemmer, stopword list, or external IDF corpus. Edit-distance /
 * Jaro-Winkler operate on whole strings and collapse under paraphrase + length
 * differences, so they're unfit for paragraph-length text. Trigram cosine is
 * the standard fuzzy-document choice for morphologically rich languages.
 *
 * Pure + deterministic (no I/O, no model) so it slots into the hermetic
 * fixture-replay test layers. CONTRACT: callers must pass SAME-LANGUAGE texts —
 * comparing a Polish reference to an English blurb is meaningless here. Phase 1
 * only compares Polish-to-Polish; an English path (TMDB en-US overview vs
 * IMDb/MC/RT) would obey the same contract.
 */
object SynopsisSimilarity {

  /** Trigrams shorter than this many normalized characters can't form one, so
   *  the text carries no signal — score 0 rather than invent a match. */
  private val MinChars = 3

  /**
   * Character-trigram cosine similarity of two texts, in [0,1]. 1.0 = identical
   * trigram profiles, 0.0 = no shared trigram (or either side too short). Each
   * text is normalized via [[normalize]] (strip URLs, deburr, lowercase,
   * collapse whitespace) before trigrams are taken, so casing, diacritics, and
   * spacing differences don't depress the score.
   */
  def similarity(a: String, b: String): Double = {
    val (na, nb) = (normalize(a), normalize(b))
    if (na.length < MinChars || nb.length < MinChars) 0.0
    else cosine(trigramCounts(na), trigramCounts(nb))
  }

  /**
   * Index of the candidate whose synopsis is the confident closest match to
   * `reference`, or None when there's no clear winner. A winner must BOTH clear
   * `min` (so a field of equally-poor matches doesn't get promoted) AND beat the
   * runner-up by at least `margin` (so two near-equal candidates stay
   * unresolved). Returning None is the safety valve: the caller then keeps its
   * existing deterministic ordering, so a weak/ambiguous synopsis signal never
   * overrides a sound title/year/director pick.
   *
   * The returned index addresses `candidates` positionally — callers must pass
   * one entry per candidate (use `_.getOrElse("")` for missing blurbs) so the
   * index maps straight back to the candidate list.
   */
  def confidentTieBreak(
    reference:  String,
    candidates: Seq[String],
    min:        Double = 0.35,
    margin:     Double = 0.05
  ): Option[Int] = {
    if (normalize(reference).length < MinChars || candidates.lengthCompare(1) <= 0) None
    else {
      val scored = candidates.iterator.map(c => similarity(reference, c)).toVector
      val bestIdx = scored.indices.maxBy(scored)        // first index wins ties → deterministic
      val best    = scored(bestIdx)
      val runnerUp = scored.indices.filter(_ != bestIdx).map(scored).maxOption.getOrElse(0.0)
      Option.when(best >= min && best - runnerUp >= margin)(bestIdx)
    }
  }

  /** stripUrls → deburr → lowercase → single-space → trim. Reuses the shared
   *  [[TextNormalization]] primitives so the fold matches the rest of the
   *  pipeline (same `ł→l`, same URL stripping the synopses are stored with). */
  private def normalize(s: String): String =
    TextNormalization.deburr(TextNormalization.stripUrls(s))
      .toLowerCase
      .replaceAll("\\s+", " ")
      .trim

  /** Frequency map of the length-3 character windows of `s` (already
   *  normalized). Frequencies, not a set, so a repeated distinctive phrase
   *  weighs more than a one-off — closer to TF cosine than plain Jaccard. */
  private def trigramCounts(s: String): Map[String, Int] =
    s.sliding(3).toSeq.groupMapReduce(identity)(_ => 1)(_ + _)

  private def cosine(a: Map[String, Int], b: Map[String, Int]): Double = {
    val dot = a.iterator.collect { case (g, ca) if b.contains(g) => ca.toDouble * b(g) }.sum
    if (dot == 0.0) 0.0
    else dot / (magnitude(a) * magnitude(b))
  }

  private def magnitude(counts: Map[String, Int]): Double =
    math.sqrt(counts.valuesIterator.map(c => c.toDouble * c).sum)
}
