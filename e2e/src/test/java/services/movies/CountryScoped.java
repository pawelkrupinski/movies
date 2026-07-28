package services.movies;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.scalatest.TagAnnotation;

/**
 * Tags a spec that installs a country's {@code TitleRuleSet} into
 * {@code TitleNormalizer} — a PROCESS-GLOBAL swap.
 *
 * Such a spec cannot share a JVM with anything else. ScalaTest runs suites in
 * parallel, so a German rule set installed mid-run is visible to every other
 * suite in the process, including the Polish whole-corpus specs
 * ({@code ScrapeOrderDeterminismSpec}, {@code ReScrapeIdempotencySpec},
 * {@code FilmScheduleEndToEndSpec}) — which would then normalise Polish titles
 * under German rules and fail, or worse, pass for the wrong reason. Two
 * country-scoped specs in one JVM corrupt each other the same way.
 *
 * So these are EXCLUDED from every shared run — {@code testUnit} locally and the
 * {@code e2eRest} shard in CI — and run only one-per-JVM by name, via the
 * {@code convergencePoland} / {@code convergenceGermany} / {@code convergenceUk}
 * aliases. Those are dispatched as three parallel JOBS by
 * .github/workflows/country-convergence.yml, one country each.
 *
 * If you add a country-scoped spec, tag it and give it its own alias + matrix
 * leg. Do NOT fold it into an existing shard.
 *
 * Class-level tag → every test in the annotated spec carries it.
 */
@TagAnnotation
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface CountryScoped {}
