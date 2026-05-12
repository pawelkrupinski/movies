package models

import java.time.LocalDate

case class Movie(
  title:          String,
  runtimeMinutes: Option[Int]       = None,
  releaseYear:    Option[Int]       = None,
  premierePl:     Option[LocalDate] = None,
  premiereWorld:  Option[LocalDate] = None
)
