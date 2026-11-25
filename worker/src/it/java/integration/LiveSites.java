package integration;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.scalatest.TagAnnotation;

/**
 * Tags an integration spec that talks to a LIVE third-party site (Metacritic, Rotten Tomatoes, IMDb, TMDB, Filmweb).
 * Such a spec is a CANARY for the site's contract, not a check of our code: a third party answering differently (on
 * 2026-11-25 Metacritic began answering 200 for any slug) must never block an unrelated landing.
 *
 * build.sbt's {@code IntegrationTest} excludes this tag ({@code -l integration.LiveSites}) from every run, so
 * {@code itAll} -- Main's integration rows, the pre-push hook's {@code itrest}, the landing check -- never selects a
 * tagged spec. With {@code LIVE_SITES} set, the same config runs ONLY the tagged specs ({@code -n}): the nightly
 * live-sites.yml, which opens an issue on failure and gates nothing.
 *
 * A spec naming {@code LiveUpstream.Probes} reaches a real site and must carry this tag
 * (worker's LiveSitesTaggedSpec fails a spec that does not). Class-level: every test of the spec carries it.
 */
@TagAnnotation
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface LiveSites {}
