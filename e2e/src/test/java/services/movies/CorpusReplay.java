package services.movies;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.scalatest.TagAnnotation;

/**
 * Tags the heavy whole-corpus specs — each of which boots the whole fixture
 * corpus through the identity projection — so CI can fan the {@code e2e}
 * module out across parallel runners: ONE shard per tagged heavy spec (run by
 * name), and a "rest" shard that runs
 * {@code e2e/Test/test -- -l services.movies.CorpusReplay} — i.e. EVERYTHING NOT
 * tagged here. A newly-added e2e spec is untagged, so it lands in the "rest"
 * shard automatically and can never be silently dropped.
 *
 * Currently tagged (one shard, both run by name in {@code e2eCorpus}): the whole-corpus snapshot spec
 * FilmScheduleEndToEndSpec, whose shard also stages the worker distribution the image build reads, and
 * IdentityCutoverEndToEndSpec, the rest shard's longest suite (199-607 s against FilmSchedule's 153-319 s on
 * the 4-CPU measure tier, 2026-10-30): beside FilmSchedule it evens the two rows instead of costing a runner.
 *
 * ReScrapeIdempotencySpec — the heaviest single spec, which boots the settled
 * corpus once and runs identical re-scrape ticks against it — was tagged too,
 * and is not any more. Its shard finished ~2 min inside the build's long pole
 * with a whole runner to itself, and ci.yml is at GitHub's 20-runner cap, so the
 * slot went to another WebKit page-test shard instead. Untagging is the whole
 * move: it rejoins the {@code e2eRest} run automatically.
 *
 * Class-level tag → every test in the annotated spec carries it. See the
 * {@code e2eCorpus} / {@code e2eRest} aliases in build.sbt
 * and the {@code e2e} matrix job in .github/workflows/ci.yml.
 */
@TagAnnotation
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface CorpusReplay {}
