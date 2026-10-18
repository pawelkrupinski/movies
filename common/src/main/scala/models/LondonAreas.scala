package models

/** London's compass areas — which of [[CinemaArea.compass]] each of its venues is filed under.
 *  [[London]] is the one city split into areas; its venues are every one in Greater London
 *  (see `UkPages`), and the areas partition them (`CinemaAreaSpec`). */
private[models] object LondonAreas {
  import CinemaArea.*

  private val venuesOf: Map[CinemaArea, Set[Cinema]] = Map(
    Central -> Set(
      BfiLondonImax, BfiLondonSouthbank, CineworldLeicesterSquare, CineLumiereLondon, CurzonCinemaBloomsbury,
      CurzonCinemaCamden, CurzonCinemaSeaContainersMondrian, CurzonCinemaVictoria, CurzonSoho,
      EverymanCinemaBakerStreet, EverymanCinemaBroadgate, EverymanCinemaChelsea, EverymanCinemaKingSCross,
      EverymanCinemaIslington, InstituteOfContemporaryArts, OdeonCinemaTottenhamCourtRoad, OdeonCinemaLuxeHaymarket,
      OdeonLuxeIslington, OdeonCinemaLuxeLeicesterSquare, OdeonLuxeWestEnd, PrinceCharlesLondon,
      RegentStreetCinemaLondon, ScienceMuseumLondonImax, TheCinemaAtSelfridges, TheGardenCinema, TheNickelLondon,
      VueCinemasIslington, VueCinemasPiccadillyCircus, VueCinemasWestEnd,
    ),
    North -> Set(
      ArthouseCrouchEnd, CineworldEnfield, CineworldIlford, CineworldWoodGreen, CrouchEndPicturehouse,
      EverymanCinemaBarnet, EverymanCinemaBelsizeParkHampstead, EverymanCinemaHampstead, EverymanCinemaMuswellHill,
      FinsburyParkPicturehouse, Jw3Hampstead, KilnKilburn, LexiKensalRise, LumiereRomford, OdeonCinemaHolloway,
      OdeonLuxeLeeValley, OdeonLuxeSwissCottage, PhoenixCinemaEastFinchley, VueCinemasDagenham,
      VueCinemasFinchleyRoadSwissCottage, VueCinemasFinchley, VueCinemasRomford, VueCinemasWoodGreen,
    ),
    East -> Set(
      BarbicanLondonCinema1, CastleCinemaHackney, CineworldWestIndiaQuay, CloseUpFilmCentreShoreditch,
      CurzonCinemaAldgate, CurzonCinemaHoxton, EverymanCinemaCanaryWharf, EverymanCinemaStratfordInternational,
      ForestCinemasWalthamstow, GenesisTowerHamlets, OdeonCinemaSouthWoodford, PicturehouseHackney,
      RichMixBethnalGreen, RioDalston, RooftopFilmClubStratfordRoofEast, VueCinemasStratford,
    ),
    South -> Set(
      SidcupStoryteller, CineworldGreenwich, CineworldBexleyheath, CineworldFeltham, CineworldLondonHounslow,
      CineworldWandsworth, CurzonCinemaKingston, CurzonCinemaRichmond, CurzonWimbledon, DavidLeanCinemaCroydon,
      EverymanCinemaBoroughYards, EverymanCinemaCrystalPalace, OdeonCinemaBeckenham, OdeonCinemaGreenwich,
      OdeonCinemaKingston, OdeonCinemaOrpington, OdeonCinemaRichmond, OdeonCinemaStreatham, OdeonCinemaWimbledon,
      OdeonCinemaLuxePutney, OlympicCinemaBarnes, EmpireCinemaSutton, Peckhamplex, PicturehouseClapham,
      PicturehouseEastDulwich, PicturehouseGreenwich, PicturehouseWestNorwood, RooftopFilmClubPeckhamBusseyBuilding,
      ArchlightCinemas, TheArzner, TheCinemaInThePowerStation, TheRitzyPicturehouseBrixton, VueCinemasBromley,
      VueCinemasEltham, VueCinemasFulham, VueCinemasPurleyWayCroydon,
    ),
    West -> Set(
      ActOneActon, ChiswickCinema, CineworldSouthRuislip, CineworldWembley, CurzonCinemaMayfair,
      ElectricCinemaLondon, ElectricCinemaWhiteCity, EverymanAtTheWhiteleyLondon, EverymanBrentford,
      EverymanCinemaMaidaVale, OdeonCinemaActon, OdeonCinemaUxbridge, PicturehouseCentralLondon,
      PicturehouseEalingFilmworks, RiversideStudiosHammersmith, TheGatePicturehouseLondon, VueCinemasHarrow,
      VueCinemasWestfieldShepherdSBush,
    ),
  )

  /** London's venues by compass area, areas in display order, each area's venues in page order. */
  val groups: Seq[CinemaAreaGroup] =
    CinemaArea.compass
      .map(area => CinemaAreaGroup(area, UkPages.london.cinemas.filter(venuesOf.getOrElse(area, Set.empty).contains)))
      .filter(_.cinemas.nonEmpty)
}
