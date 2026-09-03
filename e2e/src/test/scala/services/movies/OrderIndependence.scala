package services.movies

import org.scalatest.Tag

/**
 * The whole-corpus order-independence replay, so a country whose corpus has outgrown
 * one CI job can run it in a job of its own.
 *
 * It is the one assertion in [[CountryConvergenceBehaviour]] that does not share the
 * suite's booted corpus: it seeds its own archive and runs `Passes` independent
 * whole-corpus replays concurrently. That makes it separable, and for the United
 * States it has to be. A cold pass over 4,304 venues / 121,544 listings costs ~147
 * minutes (the leg's own `scrapeTick`, and `replay` walks the same serial loop), the
 * three concurrent passes run ~1.5x one boot — measured on the UK, 2,586s against a
 * 1,676s boot — and the boot itself is 167 minutes. Both in one job is ~5.5 hours
 * against a 315-minute suite ceiling and GitHub's hard 360-minute cancellation, which
 * is why no US leg had ever reached the end of this test.
 *
 * ONLY the full US leg excludes it (`convergenceUs`), and only because a second job
 * (`convergenceUsOrder`) runs it. The warm countries keep it inline — Poland's costs
 * 287s, Germany's 439s — and every SAMPLE leg keeps it too, so the ~100-film version
 * of this claim is checked on every country on every run regardless.
 *
 * A ScalaTest `Tag` rather than a `@TagAnnotation` like `@CorpusReplay`: those tag a
 * whole SPEC, and this has to tag one test inside a spec whose other tests stay where
 * they are.
 */
object OrderIndependence extends Tag("services.movies.OrderIndependence")
