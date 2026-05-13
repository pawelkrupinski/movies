package models

sealed abstract class Cinema(val displayName: String)

case object CinemaCityKinepolis extends Cinema("Cinema City Kinepolis")

case object CinemaCityPoznanPlaza extends Cinema("Cinema City Poznań Plaza")

case object Helios extends Cinema("Helios Posnania")

case object KinoApollo extends Cinema("Kino Apollo")

case object KinoBulgarska extends Cinema("Kino Bułgarska 19")

case object CharlieMonroe extends Cinema("Kino Malta Charlie Monroe")

case object KinoMuza extends Cinema("Kino Muza")

case object KinoPalacowe extends Cinema("Kino Pałacowe")

case object Rialto extends Cinema("Kino Rialto")

case object Multikino extends Cinema("Multikino Stary Browar")

object Cinema {
  val all: Seq[Cinema] = Seq(
    CinemaCityKinepolis,
    CinemaCityPoznanPlaza,
    Helios,
    KinoApollo,
    KinoBulgarska,
    CharlieMonroe,
    KinoMuza,
    KinoPalacowe,
    Rialto,
    Multikino,
  )
}