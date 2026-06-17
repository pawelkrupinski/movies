package services.movies;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.scalatest.TagAnnotation;

/**
 * Tags the heavy whole-corpus determinism specs (ScrapeOrderDeterminismSpec,
 * StagingOrderDeterminismSpec) so CI can fan the {@code e2e} module out across
 * parallel runners: one shard per tagged heavy spec (run by name), and a "rest"
 * shard that runs {@code e2e/Test/test -- -l services.movies.CorpusReplay} —
 * i.e. EVERYTHING NOT tagged here. A newly-added e2e spec is untagged, so it
 * lands in the "rest" shard automatically and can never be silently dropped.
 *
 * Class-level tag → every test in the annotated spec carries it. See the
 * {@code e2eScrape} / {@code e2eStaging} / {@code e2eRest} aliases in build.sbt
 * and the {@code e2e} matrix job in .github/workflows/ci.yml.
 */
@TagAnnotation
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface CorpusReplay {}
