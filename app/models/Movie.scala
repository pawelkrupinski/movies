package models

import java.time.LocalDate

case class Movie(
  title:          String,
  runtimeMinutes: Option[Int]       = None,
  releaseYear:    Option[Int]       = None,
  premierePl:     Option[LocalDate] = None,
  premiereWorld:  Option[LocalDate] = None,
  // Production country (or comma-separated list when several co-produce).
  country:        Option[String]    = None,
  // English/international release title when the cinema's API exposes it
  // (Multikino does for niche international shows — Cirque du Soleil, opera,
  // English-language docs). Used as a TMDB-search fallback for titles whose
  // Polish translation doesn't index well; absent for most films, where the
  // Polish title is the canonical entry point.
  originalTitle:  Option[String]    = None
)
