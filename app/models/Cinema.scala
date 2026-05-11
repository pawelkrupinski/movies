package models

sealed abstract class Cinema(val displayName: String)
case object Multikino     extends Cinema("Multikino Stary Browar")
case object CharlieMonroe extends Cinema("Kino Malta Charlie Monroe")
case object KinoPalacowe  extends Cinema("Kino Pałacowe")