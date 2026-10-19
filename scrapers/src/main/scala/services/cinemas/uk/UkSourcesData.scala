// GENERATED from data/uk/roster.json by data/uk/scripts/generate_roster.py
// — do NOT edit by hand. Re-source a UK venue in data/uk/roster.json and regenerate; see data/uk/README.md.
package services.cinemas.uk

import models._

/** Every UK venue's sources, in scrape order: its primary, and the flicks.co.uk slug an own-site
 *  primary falls back to. Grouped as roster.json groups them, by the town each venue was first
 *  wired near — the order the groups stand in is each page's scrape order. */
private[cinemas] object UkSourcesData {
  private def f(cinema: Cinema, slug: String): UkVenueSources = UkVenueSources(cinema, UkFeed.Flicks(slug), None)
  private def o(cinema: Cinema, primary: UkFeed, fallback: String): UkVenueSources =
    UkVenueSources(cinema, primary, Some(fallback))
  private def own(cinema: Cinema, primary: UkFeed): UkVenueSources = UkVenueSources(cinema, primary, None)

  // ── down
  private def group0: Seq[UkVenueSources] = Seq(
    f(IMCNewtownardsMovieland, "movieland-newtownards"),
    f(IveaghMovieStudioIMCBanbridge, "iveagh-movie-studio-banbridge"),
    f(OmniplexBanbridge, "omniplex-banbridge"),
    f(OmniplexBangor, "omniplex-d-luxx-bangor"),
    f(OmniplexDownpatrick, "omniplex-downpatrick"),
    f(OmniplexDundonald, "omniplex-dundonald"),
    f(OmniplexNewry, "omniplex-newry"),
  )
  // ── lincolnshire
  private def group1: Seq[UkVenueSources] = Seq(
    f(ArtsCentreStamford, "arts-centre-stamford"),
    o(EverymanCinemaLincoln, UkFeed.Everyman("G004G"), "everyman-cinema-lincoln"),
    o(JunctionGoole, UkFeed.Spektrix("junctiongoole", None), "junction-goole"),
    f(KinemaInTheWoods, "kinema-in-the-woods-woodhall-spa"),
    o(LoewenCinema, UkFeed.Savoy("https://loewencinema.com/LoewenCinema.dll/"), "loewen-mablethorpe"),
    o(OdeonCinemaLincoln, UkFeed.Odeon("568"), "odeon-cinema-lincoln"),
    f(ParkwayCinemaLouth, "playhouse-louth"),
    f(ParkwayCleethorpes, "parkway-cleethorpes"),
    o(SavoyBoston, UkFeed.Savoy("https://savoyboston.co.uk/SavoyBoston.dll/"), "savoy-boston"),
    o(SavoyGrantham, UkFeed.Savoy("https://savoygrantham.co.uk/SavoyGrantham.dll/"), "savoy-grantham"),
    f(SleafordPlayhouse, "sleaford-playhouse"),
    o(TowerCinemaSkegness, UkFeed.Savoy("https://towercinema.co.uk/TowerSkegness.dll/"), "tower-skegness"),
    o(VueCinemasScunthorpe, UkFeed.Vue("10050"), "vue-cinemas-scunthorpe"),
  )
  // ── cardiff
  private def group2: Seq[UkVenueSources] = Seq(
    o(ChapterCardiff, UkFeed.Ticketsolve("chapter"), "chapter-cardiff"),
    o(CineworldCardiff, UkFeed.Cineworld("x078m-cineworld-cinema-cardiff"), "cineworld-cardiff"),
    o(EverymanCinemaCardiff, UkFeed.Everyman("X11NU"), "everyman-cinema-cardiff"),
    o(OdeonCinemaCardiff, UkFeed.Odeon("954"), "odeon-cinema-cardiff"),
    o(ShowcaseCinemaCardiff, UkFeed.Showcase("X06JS"), "showcase-cardiff"),
  )
  // ── cumbria
  private def group3: Seq[UkVenueSources] = Seq(
    o(BreweryArtsCentreKendal, UkFeed.Spektrix("breweryarts", None), "brewery-arts-centre-kendal"),
    f(GaietyCinemaWhitehaven, "gaiety-cinema-whitehaven"),
    o(KeswickAlhambra, UkFeed.Savoy("https://keswickalhambra.co.uk/KeswickAlhambra.dll/"), "alhambra-keswick"),
    o(LonsdaleAlhambraPenrith, UkFeed.Savoy("https://penrithcinema.co.uk/LonsdalePenrith.dll/"), "lonsdale-alhambra-penrith"),
    f(ParkwayWorkington, "plaza-workington"),
    f(ReelCinemaMorecambe, "reel-cinema-morecambe"),
    f(RoxyUlverston, "roxy-ulverston"),
    f(RoyaltyBownessOnWindemere, "royalty-bowness-on-windemere"),
    o(TheRitzCinemaWorkington, UkFeed.Savoy("https://theritzcinema.co.uk/TheRitzWorkington.dll/"), "the-ritz-cinema-workington"),
    o(VueCinemasBarrow, UkFeed.Vue("10078"), "vue-cinemas-barrow"),
    o(VueCinemasCarlisle, UkFeed.Vue("10056"), "vue-cinemas-carlisle"),
    f(ZeffirellisCinemaAmbleside, "zeffirellis-by-the-park-ambleside"),
  )
  // ── tyne-and-wear
  private def group4: Seq[UkVenueSources] = Seq(
    o(CineworldBoldonTyneWear, UkFeed.Cineworld("x079v-cineworld-cinema-boldon-tyne-and-wear"), "cineworld-boldon-tyne-wear"),
    o(CineworldNewcastle, UkFeed.Cineworld("x06th-cineworld-cinema-newcastle-upon-tyne"), "cineworld-newcastle"),
    f(CustomsHouseCinemaSouthShields, "customs-house-south-shields"),
    o(EverymanCinemaNewcastle, UkFeed.Everyman("X11KB"), "everyman-cinema-newcastle"),
    f(JamJarCinema, "jam-jar-cinema"),
    o(OdeonCinemaMetrocentre, UkFeed.Odeon("980"), "odeon-cinema-tyne-wear-gateshead"),
    o(OdeonCinemaSilverlink, UkFeed.Odeon("961"), "odeon-cinema-tyne-wear-wallsend"),
    f(OmniplexSunderlandFormerlyEmpire, "empire-cinema-sunderland-tyne-wear"),
    f(StarAndShadowCinemaNewcastle, "star-and-shadow-cinema-newcastle"),
    f(TynesideNewcastle, "tyneside-newcastle"),
    o(VueCinemasGateshead, UkFeed.Vue("10088"), "vue-cinemas-gateshead-tyne-wear"),
  )
  // ── east-yorkshire
  private def group5: Seq[UkVenueSources] = Seq(
    o(CineworldHull, UkFeed.Cineworld("x079w-cineworld-cinema-hull"), "cineworld-hull"),
    f(ForumBridlington, "forum-bridlington"),
    o(OdeonLuxeHull, UkFeed.Odeon("440"), "odeon-luxe-hull"),
    f(PalaceCinemaMalton, "palace-cinema-malton"),
    f(ParkwayBeverley, "parkway-beverley"),
    f(ReelCinemaHull, "reel-cinema-hull"),
    o(VueCinemasHull, UkFeed.Vue("10065"), "vue-cinemas-hull"),
  )
  // ── staffordshire
  private def group6: Seq[UkVenueSources] = Seq(
    f(CannockCinema, "electric-palace-picture-house-cannock"),
    o(CinebowlUttoxeter, UkFeed.SavoyBoxOffice("https://cinebowlonline.org/CinebowlUttoxeter.dll/"), "cinebowl-uttoxeter"),
    o(CineworldBurtonOnTrent, UkFeed.Cineworld("x06x5-cineworld-cinema-burton-upon-trent"), "cineworld-burton-on-trent"),
    o(CineworldStokeOnTrent, UkFeed.Cineworld("x0weg-cineworld-cinema-stoke-on-trent"), "cineworld-stoke-on-trent"),
    o(CineworldWolverhampton, UkFeed.Cineworld("x06xr-cineworld-cinema-wolverhampton"), "cineworld-wolverhampton"),
    f(FilmTheatreStokeOnTrent, "film-theatre-stoke-on-trent"),
    o(LichfieldGarrickTheatreStudio, UkFeed.Spektrix("lichfieldgarrick", Some("Studio")), "lichfield-garrick-theatre-and-studio"),
    f(LockworksCinemaWolverhampton, "lockworks-cinema"),
    o(OdeonCinemaStokeOnTrent, UkFeed.Odeon("430"), "odeon-cinema-stoke-on-trent"),
    o(OdeonCinemaTamworth, UkFeed.Odeon("930"), "odeon-cinema-tamworth"),
    o(OdeonLuxeStafford, UkFeed.Odeon("857"), "odeon-luxe-stafford"),
    f(RedCarpetBartonMarina, "red-carpet-barton-under-needwood"),
    f(TheLightWalsall, "the-light-cinemas-walsall"),
    o(VueCinemasNewcastleUnderLyme, UkFeed.Vue("10053"), "vue-cinemas-newcastle-under-lyme"),
  )
  // ── clwyd
  private def group7: Seq[UkVenueSources] = Seq(
    o(CineworldLlandudno, UkFeed.Cineworld("x06xe-cineworld-cinema-llandudno"), "cineworld-llandudno"),
    f(MerlinScalaPrestatyn, "scala-prestatyn"),
    f(StrandCinemaRhyl, "vue-cinemas-rhyl"),
    o(TheatrColwyn, UkFeed.Ticketsolve("theatrcolwyn"), "theatr-colwyn-colwyn-bay"),
  )
  // ── renfrewshire
  private def group8: Seq[UkVenueSources] = Seq(
    o(OdeonCinemaBraehead, UkFeed.Odeon("615"), "odeon-cinema-braehead"),
    o(ShowcaseDeLuxPaisley, UkFeed.Showcase("X06KC"), "showcase-de-lux-paisley"),
    f(TheTowerDigitalArtsCenterHelensburgh, "the-tower-digital-arts-center-scottish-submarine-centre"),
    f(WaterfrontGreenock, "waterfront-greenock"),
  )
  // ── fermanagh
  private def group9: Seq[UkVenueSources] = Seq(
    f(IMCCinemaEnniskillen, "imc-cinema-enniskillen"),
  )
  // ── tayside
  private def group10: Seq[UkVenueSources] = Seq(
    f(BirksAberfeldy, "birks-aberfeldy"),
    f(ChalmersFilmhouseArbroath, "chalmers-arbroath-cinema"),
    o(CineworldDundee, UkFeed.Cineworld("x07a0-cineworld-cinema-dundee"), "cineworld-dundee"),
    o(DundeeContemporaryArtsDCA, UkFeed.Spektrix("dundeeca", None), "dundee-contemporary-arts-dca-dundee"),
    f(NewPictureHouseStAndrews, "new-picture-house-st-andrews"),
    o(OdeonLuxeDundee, UkFeed.Odeon("044"), "odeon-luxe-dundee"),
    f(PlayhouseCinemaPerth, "playhouse-perth"),
    f(TheMontrosePlayhouse, "the-montrose-playhouse"),
  )
  // ── west-sussex
  private def group11: Seq[UkVenueSources] = Seq(
    f(AtriumEastGrinstead, "atrium-east-grinstead"),
    f(CapitolHorsham, "capitol-horsham"),
    o(CineworldCrawley, UkFeed.Cineworld("x079c-cineworld-cinema-crawley"), "cineworld-crawley"),
    f(ConnaughtTheatreStudioWorthing, "connaught-theatre-studio-worthing"),
    f(DomeWorthing, "dome-worthing"),
    o(EverymanCinemaHorsham, UkFeed.Everyman("X11LP"), "everyman-cinema-horsham"),
    f(OrionBurgessHill, "orion-burgess-hill"),
    f(PicturedromeBognorRegis, "picturedrome-bognor-regis"),
    o(WindmillLittlehampton, UkFeed.Savoy("https://windmillcinema.co.uk/WindmillCinema.dll/"), "windmill-littlehampton"),
  )
  // ── hertfordshire
  private def group12: Seq[UkVenueSources] = Seq(
    f(BaldockArtsHeritageCentre, "baldock-arts-and-heritage-centre"),
    o(BEAMHertfordTheatre, UkFeed.Spektrix("beamhertford", None), "hertford-theatre"),
    f(BroadwayLetchworth, "broadway-letchworth"),
    o(CineworldHemelHempstead, UkFeed.Cineworld("x06sl-cineworld-cinema-hemel-hempstead"), "cineworld-hemel-hempstead"),
    o(CineworldStevenage, UkFeed.Cineworld("x06xn-cineworld-cinema-stevenage"), "cineworld-stevenage"),
    o(CineworldWatford, UkFeed.Cineworld("x11db-cineworld-cinema-watford"), "cineworld-watford"),
    o(OdeonCinemaHatfield, UkFeed.Odeon("929"), "odeon-cinema-hatfield"),
    f(ReelCinemaBorehamwood, "reel-cinema-borehamwood"),
    o(TheCinemaCampusWest, UkFeed.Spektrix("campuswest", None), "garden-city-cinema-welwyn"),
    o(TheOdysseyStAlbans, UkFeed.Savoy("https://odysseypictures.co.uk/OdysseyPictures.dll/"), "odyssey-st-albans"),
    o(TheRexCinemaBerkhamsted, UkFeed.SavoyBoxOffice("http://therexberkhamsted.savoysystems.co.uk/TheRexBerkhamsted.dll/"), "rex-berkhamsted"),
    o(VueCinemasWatford, UkFeed.Vue("10023"), "vue-cinemas-watford"),
    f(Watersmeet, "watersmeet"),
  )
  // ── isle-of-man
  private def group13: Seq[UkVenueSources] = Seq(
    f(BroadwayCinemaVillaMarina, "broadway-cinema-villa-marina"),
    f(PalaceCinemasIsleOfMan, "palace-cinemas-isle-of-man"),
  )
  // ── armagh
  private def group14: Seq[UkVenueSources] = Seq(
    f(OmniplexCraigavon, "omniplex-craigavon"),
  )
  // ── birmingham
  private def group15: Seq[UkVenueSources] = Seq(
    f(ArtrixBromsgrove, "artrix-bromsgrove"),
    o(CineworldBroadStreetBirmingham, UkFeed.Cineworld("x079j-cineworld-cinema-birmingham-broad-street"), "cineworld-broad-street-birmingham"),
    o(CineworldNECBirmingham, UkFeed.Cineworld("x0vho-cineworld-cinema-birmingham-nec"), "cineworld-nec-birmingham"),
    o(CineworldSolihull, UkFeed.Cineworld("x06xl-cineworld-cinema-solihull"), "cineworld-solihull"),
    o(EverymanCinemaBirmingham, UkFeed.Everyman("X0VHE"), "everyman-cinema-birmingham"),
    o(MidlandsArtsCentreBirmingham, UkFeed.Spektrix("macbirmingham", None), "midlands-arts-centre-birmingham"),
    o(MockingbirdCinemaKitchenBirmingham, UkFeed.Savoy("https://mockingbirdcinema.com/MockingbirdCinema.dll/"), "mockingbird-cinema-kitchen-birmingham"),
    o(OdeonBirminghamNewStreet, UkFeed.Odeon("017"), "odeon-cinema-birmingham-new-street"),
    o(OdeonCinemaDudley, UkFeed.Odeon("915"), "odeon-cinema-dudley"),
    o(OdeonCinemaWestBromwich, UkFeed.Odeon("761"), "odeon-cinema-west-bromwich"),
    o(OdeonLuxeBirminghamBroadwayPlaza, UkFeed.Odeon("846"), "odeon-luxe-birmingham"),
    f(OmniplexBirmingham, "empire-cinema-birmingham"),
    f(ReelCinemaQuinton, "reel-cinema-quinton"),
    f(RoyalCinemasSuttonColdfield, "royal-cinema-sutton-coldfield"),
    o(ShowcaseCinemaDudley, UkFeed.Showcase("X06JJ"), "showcase-cinema-dudley"),
    o(VueCinemasBirmingham, UkFeed.Vue("10015"), "vue-cinemas-birmingham"),
  )
  // ── roxburgh-ettrick-and-lauderdale
  private def group16: Seq[UkVenueSources] = Seq(
    f(PavilionCinemaGalashiels, "pavilion-galashiels"),
    f(TowerMillHeartOfHawick, "tower-mill-cinema-hawick"),
  )
  // ── essex
  private def group17: Seq[UkVenueSources] = Seq(
    f(CenturyCinemaClacton, "century-clacton"),
    o(CineworldBasildon, UkFeed.Cineworld("x06ur-cineworld-cinema-basildon"), "cineworld-basildon"),
    o(CineworldBraintree, UkFeed.Cineworld("x06x3-cineworld-cinema-braintree"), "cineworld-braintree"),
    o(CineworldHarlowHarveyCentre, UkFeed.Cineworld("x0x9g-cineworld-cinema-harlow-harvey-centre"), "cineworld-harlow-harvey-centre"),
    o(CineworldHarlowQueensgate, UkFeed.Cineworld("x079n-cineworld-cinema-harlow-queensgate"), "cineworld-harlow-queensgate"),
    f(CurzonCinemaColchester, "curzon-cinema-colchester"),
    o(ElectricPalaceHarwich, UkFeed.Savoy("https://electricpalace.com/ElectricPalaceHarwich.dll/"), "electric-palace-harwich"),
    o(EmpireTheatreHalstead, UkFeed.Savoy("https://empire-theatre.co.uk/EmpireTheatre.dll/"), "empire-theatre-halstead-park"),
    o(EverymanCinemaChelmsford, UkFeed.Everyman("X0XMY"), "everyman-cinema-chelmsford"),
    f(MovieStarrCanveyIsland, "movie-starr-canvey-island"),
    o(OdeonCinemaChelmsford, UkFeed.Odeon("498"), "odeon-cinema-chelmsford"),
    o(OdeonCinemaColchester, UkFeed.Odeon("515"), "odeon-cinema-colchester"),
    o(OdeonCinemaSouthendOnSea, UkFeed.Odeon("500"), "odeon-cinema-southend-on-sea"),
    f(RioBurnhamOnCrouch, "rio-burnham-on-crouch"),
    f(RoxyMoviesBishopSStortford, "empire-cinema-bishops"),
    f(SaffronScreen, "saffron-screen"),
    o(VueCinemasBasildon, UkFeed.Vue("10099"), "vue-cinemas-basildon"),
    o(VueCinemasColchester, UkFeed.Vue("20004"), "vue-cinemas-colchester"),
    o(VueCinemasWestThurrock, UkFeed.Vue("10045"), "vue-cinemas-west-thurrock"),
  )
  // ── glasgow
  private def group18: Seq[UkVenueSources] = Seq(
    o(CineworldSilverburnGlasgow, UkFeed.Cineworld("x0vq1-cineworld-cinema-glasgow-silverburn"), "cineworld-silverburn-glasgow"),
    o(EverymanCinemaGlasgow, UkFeed.Everyman("X11DQ"), "everyman-cinema-glasgow"),
    f(GlasgowFilmTheatre, "glasgow-film-theatre-glasgow"),
    f(GrosvenorCinemaGlasgow, "grosvenor-cinema-glasgow"),
    f(IMAXAtGlasgowScienceCentre, "imax-glasgow"),
    o(LanternhouseCinema, UkFeed.Spektrix("cumbernauldtheatretrust", None), "cumbernauld-theatre-at-lanternhouse"),
    o(OdeonLuxeEastKilbride, UkFeed.Odeon("923"), "odeon-luxe-east-kilbride"),
    o(OdeonLuxeGlasgow, UkFeed.Odeon("530"), "odeon-luxe-glasgow"),
    o(ShowcaseGlasgowCoatbridge, UkFeed.Showcase("X06KE"), "showcase-de-lux-coatbridge"),
    o(VueCinemasGlasgowFort, UkFeed.Vue("10086"), "vue-cinemas-glasgow"),
    o(VueCinemasGlasgowStEnoch, UkFeed.Vue("10097"), "vue-cinemas-glasgow-st-enoch"),
    o(VueCinemasHamilton, UkFeed.Vue("10003"), "vue-cinemas-hamilton"),
  )
  // ── somerset
  private def group19: Seq[UkVenueSources] = Seq(
    o(CineworldWestonSuperMare, UkFeed.Cineworld("x10kq-cineworld-cinema-weston-super-mare"), "cineworld-weston-super-mare"),
    o(CineworldYeovil, UkFeed.Cineworld("x06xt-cineworld-cinema-yeovil"), "cineworld-yeovil"),
    f(CurzonCinemaClevedon, "curzon-cinema-clevedon"),
    o(EverymanBath, UkFeed.Everyman("G01VT"), "tivoli-bath"),
    f(LittleTheatrePicturehouse, "little-theatre-picturehouse"),
    f(MerlinWellesleyWellington, "wellesley-wellington"),
    o(OdeonCinemaBath, UkFeed.Odeon("355"), "odeon-cinema-bath"),
    o(OdeonCinemaTaunton, UkFeed.Odeon("453"), "odeon-cinema-taunton"),
    f(PlazaCinemaWestonSuperMare, "plaza-cinema-weston-super-mare"),
    f(RitzBurnhamOnSea, "ritz-burnham-on-sea"),
    f(ScottCinemasBridgwater, "scott-bridgwater"),
    o(TauntonBrewhouse, UkFeed.Spektrix("brewhousetaunton", None), "taunton-brewhouse"),
    f(TheAvenueCinemaMinehead, "the-avenue-cinema-minehead"),
    f(TheWellsFilmCentre, "the-wells-film-centre"),
    f(WestwayCinemaFrome, "westway-frome"),
  )
  // ── isle-of-wight
  private def group20: Seq[UkVenueSources] = Seq(
    o(CineworldNewportIsleOfWight, UkFeed.Cineworld("x07ad-cineworld-cinema-isle-of-wight"), "cineworld-newport-isle-of-wight"),
    f(CommodoreRydeIsleOfWight, "commodore-ryde-isle-of-wight"),
  )
  // ── derbyshire
  private def group21: Seq[UkVenueSources] = Seq(
    o(CineworldChesterfield, UkFeed.Cineworld("x06x8-cineworld-cinema-chesterfield"), "cineworld-chesterfield"),
    f(EliteCinemaAndTheatreAshbourne, "elite-cinema-and-theatre-ashbourne"),
    f(NorthernLightWirksworth, "northern-light-wirksworth"),
    o(OdeonCinemaSwadlincote, UkFeed.Odeon("759"), "odeon-cinema-swadlincote"),
    o(OdeonLuxeDerby, UkFeed.Odeon("917"), "odeon-luxe-derby"),
    o(QuadDerby, UkFeed.QuadDerby, "quad-derby"),
    f(RitzBelper, "ritz-belper"),
    o(ShowcaseCinemaDeLuxDerby, UkFeed.Showcase("X08EM"), "showcase-cinema-de-lux-derby"),
  )
  // ── ayrshire-and-arran
  private def group22: Seq[UkVenueSources] = Seq(
    f(AstoriaCinemaAyr, "astoria-cinema-ayr"),
    o(CinemaSaltcoatsPremierLeisure, UkFeed.Savoy("https://cinema-saltcoats.co.uk/SaltcoatsCinema.dll/"), "premier-saltcoats"),
    o(OdeonCinemaKilmarnock, UkFeed.Odeon("019"), "odeon-cinema-kilmarnock"),
  )
  // ── hampshire
  private def group23: Seq[UkVenueSources] = Seq(
    o(ChichesterCinemaAtNewPark, UkFeed.SavoyBoxOffice("https://bookings.chichestercinema.org/ChichesterCinema.dll/"), "chichester-cinema-at-new-park"),
    o(CineworldChichester, UkFeed.Cineworld("x06x9-cineworld-cinema-chichester-gate"), "cineworld-chichester"),
    o(CineworldWhiteley, UkFeed.Cineworld("x0wee-cineworld-cinema-whiteley"), "cineworld-fareham"),
    o(EverymanCinemaWinchester, UkFeed.Everyman("X0711"), "everyman-cinema-winchester"),
    f(HarbourLightsPicturehouse, "harbour-lights-picturehouse-southampton"),
    f(HytheMoviolaCinema, "hythe-moviola-cinema"),
    f(No6CinemaPortsmouth, "no-6-cinema-portsmouth"),
    o(OdeonCinemaPortSolent, UkFeed.Odeon("948"), "odeon-cinema-portsmouth"),
    f(ReelCinemaFareham, "reel-cinema-fareham"),
    o(ShowcaseDeLuxSouthampton, UkFeed.Showcase("X0XV2"), "showcase-cinema-de-lux-southampton"),
    o(SouthseaCinemaArtsCentre, UkFeed.SavoyBoxOffice("https://southseacinema.savoysystems.co.uk/SouthseaCinema.dll/"), "southsea-cinema-and-arts-centre"),
    f(TheLivingRoomCinemaLiphook, "the-living-room-cinema"),
    f(TheMaltLymington, "the-malt-lymington"),
    o(VueCinemasBasingstoke, UkFeed.Vue("10029"), "vue-cinemas-basingstoke"),
    o(VueCinemasEastleigh, UkFeed.Vue("10069"), "vue-cinemas-eastleigh"),
    o(VueCinemasPortsmouth, UkFeed.Vue("10021"), "vue-cinemas-portsmouth"),
  )
  // ── guernsey
  private def group24: Seq[UkVenueSources] = Seq(
    f(BeauSejourLeisureCentreGuernsey, "beau-sejour-cinema-st-peter-port-guernsey"),
    f(TheMallardCinemaGuernsey, "the-mallard-cinema-guernsey"),
  )
  // ── leicestershire
  private def group25: Seq[UkVenueSources] = Seq(
    o(CineworldHinckley, UkFeed.Cineworld("x0wef-cineworld-cinema-hinckley"), "cineworld-hinckley"),
    f(FlixStudentRunCinemaLoughborough, "flix-student-run-cinema-loughborough"),
    o(OdeonCinemaLoughborough, UkFeed.Odeon("758"), "odeon-cinema-loughborough"),
    o(OdeonLuxeLeicester, UkFeed.Odeon("505"), "odeon-luxe-leicester"),
    o(PhoenixCinemaAndArtCentreLeicester, UkFeed.SavoyBoxOffice("https://tickets.phoenix.org.uk/PhoenixCinemaLeicester.dll/"), "phoenix-square-leicester"),
    o(PiccadillyCinemaLeicester, UkFeed.Savoy("https://www.piccadillycinemas.co.uk/PiccadillyCinemas.dll/"), "piccadilly-leicester"),
    f(RegalMeltonMowbray, "regal-melton-mowbray"),
    o(ShowcaseDeLuxLeicester, UkFeed.Showcase("X08NG"), "showcase-cinema-de-lux-leicester"),
    o(VueCinemasLeicester, UkFeed.Vue("10026"), "vue-cinemas-leicester"),
  )
  // ── tyrone
  private def group26: Seq[UkVenueSources] = Seq(
    f(OmniplexDungannon, "omniplex-dungannon"),
    f(OmniplexOmagh, "omniplex-omagh"),
    f(RitzMultiplexCookstown, "ritz-multiplex-cookstown"),
  )
  // ── dyfed
  private def group27: Seq[UkVenueSources] = Seq(
    f(AberystwythArtsCentre, "aberystwyth-arts-centre"),
    f(CommodoreCinemaAberystwyth, "commodore-aberystwyth"),
    f(CrossHandsHallCinema, "public-hall-cinema-cross-hands"),
    f(Libanus1877, "libanus-1877-borth"),
    o(OdeonCinemaLlanelli, UkFeed.Odeon("760"), "odeon-cinema-llanelli"),
    f(PalaceCinemaHaverfordwest, "palace-haverfordwest"),
    o(PublicHallBrynamman, UkFeed.Savoy("https://brynammancinema.org/BrynammanCinema.dll/"), "public-hall-brynamman"),
    o(TheatrGwaunFishguard, UkFeed.SavoyBoxOffice("https://theatrgwaun.savoysystems.co.uk/TheatrGwaun.dll/"), "theatr-gwaun-fishguard"),
    o(TheatrMwldanCardigan, UkFeed.Ticketsolve("mwldan"), "theatr-mwldan-cardigan"),
    f(TorchTheatreMilfordHaven, "torch-theatre-milford-haven"),
    o(VueCinemasCarmarthen, UkFeed.Vue("10075"), "vue-cinemas-carmarthen"),
  )
  // ── south-yorkshire
  private def group28: Seq[UkVenueSources] = Seq(
    f(ArcCinemaRotherham, "arc-cinema-rotherham"),
    o(CineworldBarnsley, UkFeed.Cineworld("g01hn-cineworld-cinema-barnsley"), "cineworld-barnsley"),
    f(ParkwayBarnsley, "parkway-barnsley"),
    o(SavoyDoncaster, UkFeed.Savoy("https://savoydoncaster.uk/SavoyDoncaster.dll/"), "savoy-doncaster"),
    o(VueCinemasDoncaster, UkFeed.Vue("10041"), "vue-cinemas-doncaster"),
  )
  // ── cambridgeshire
  private def group29: Seq[UkVenueSources] = Seq(
    f(ArtsCinemaJohnClareTheatrePeterborough, "arts-cinema-john-clare-theatre-peterborough"),
    f(ArtsPicturehouseCambridge, "arts-picturehouse-cambridge"),
    o(CineworldEly, UkFeed.Cineworld("x0y4c-cineworld-cinema-ely"), "cineworld-ely"),
    o(CineworldHuntingdon, UkFeed.Cineworld("x06xc-cineworld-cinema-huntingdon"), "cineworld-huntingdon"),
    o(CineworldStNeots, UkFeed.Cineworld("x0tvc-cineworld-cinema-st-neots"), "cineworld-st-neots"),
    o(ElyCommunityCinema, UkFeed.Savoy("https://elycinema.org.uk/ElyCommunityCinema.dll/"), "ely-community-cinema"),
    o(EverymanCinemaCambridge, UkFeed.Everyman("G02AM"), "everyman-cinema-cambridge"),
    f(KeyTheatrePeterborough, "key-theatre-peterborough"),
    f(LuxeWisbech, "luxe-wisbech"),
    o(OdeonLuxePeterborough, UkFeed.Odeon("998"), "odeon-luxe-peterborough"),
    o(ShowcaseDeLuxPeterborough, UkFeed.Showcase("X06JO"), "cinema-de-lux-peterborough"),
    f(TheLightCambridge, "the-light-cinemas-cambridge"),
    f(TheLightWisbech, "the-light-cinemas-wisbech"),
  )
  // ── county-durham
  private def group30: Seq[UkVenueSources] = Seq(
    o(ARCStocktonOnTees, UkFeed.Spektrix("arcstockton", None), "arc-stockton-on-tees"),
    o(CineworldDaltonParkMurtonCounty, UkFeed.Cineworld("x0x9f-cineworld-cinema-dalton-park"), "cineworld-murton-county"),
    o(EmpireTheatreConsett, UkFeed.Spektrix("galadurham", Some("Consett Empire")), "empire-cinema-consett-county"),
    o(EverymanCinemaDurham, UkFeed.Everyman("G01IW"), "everyman-cinema-durham"),
    f(FuseCommunityCinemaPrudhoe, "fuse-community-cinema-prudhoe"),
    o(GalaCinemaDurham, UkFeed.Spektrix("galadurham", Some("Gala Theatre & Cinema")), "gala-theatre-and-cinema-durham-county"),
    o(OdeonLuxeDurham, UkFeed.Odeon("859"), "odeon-luxe-durham-county"),
    o(ShowcaseCinemaDeLuxTeesside, UkFeed.Showcase("X06JP"), "showcase-cinema-de-lux-stockton-on-tees"),
    o(VueCinemasDarlington, UkFeed.Vue("10090"), "vue-cinemas-darlington"),
    o(VueCinemasHartlepool, UkFeed.Vue("10035"), "vue-cinemas-hartlepool-county"),
  )
  // ── dumfries-and-galloway
  private def group31: Seq[UkVenueSources] = Seq(
    f(LonsdaleCityCinemaAnnan, "lonsdale-city-annan"),
    f(RobertBurnsCentreFilmTheatre, "robert-burns-centre-film-theatre-dumfries"),
    o(TheCinemaNewtonStewart, UkFeed.SavoyBoxOffice("https://newtonstewartcinema.savoysystems.co.uk/NewtonStewartCinema.dll/"), "the-cinema-newton-stewart"),
    f(TheFullartonCastleDouglas, "fullarton-castle-douglas"),
  )
  // ── kent
  private def group32: Seq[UkVenueSources] = Seq(
    f(CarltonCinemaWestgateOnSea, "carlton-westgate-on-sea"),
    f(CinemarshTheMarshAcademy, "cinemarsh-the-marsh-academy"),
    o(CineworldAshford, UkFeed.Cineworld("x06wy-cineworld-cinema-ashford"), "cineworld-ashford"),
    o(CineworldDover, UkFeed.Cineworld("x10r9-cineworld-cinema-dover"), "cineworld-dover"),
    o(CineworldRochester, UkFeed.Cineworld("x079e-cineworld-cinema-rochester"), "cineworld-rochester"),
    f(CurzonCanterburyRiverside, "curzon-canterbury-riverside"),
    f(EmpireCinemaSandwich, "empire-cinema-sandwich"),
    f(GulbenkianTheatre, "gulbenkian-theatre-canterbury"),
    o(KavanaghCinemaHerneBay, UkFeed.Savoy("https://kavanaghcinema.com/KavanaghCinema.dll/"), "kavanagh-herne-bay"),
    f(KinoHawkhurst, "kino-hawkhurst"),
    o(OdeonCinemaChatham, UkFeed.Odeon("050"), "odeon-cinema-chatham"),
    o(OdeonCinemaMaidstone, UkFeed.Odeon("159"), "odeon-cinema-maidstone"),
    o(OdeonCinemaTunbridgeWells, UkFeed.Odeon("512"), "odeon-cinema-tunbridge-wells"),
    f(PalaceCinemaKent, "palace-cinema-broadstairs"),
    f(RoyalCinemaFaversham, "royal-faversham"),
    o(ShowcaseDeLuxBluewater, UkFeed.Showcase("X06JR"), "showcase-cinema-de-lux-dartford"),
    o(SilverScreenFolkestone, UkFeed.SavoyBoxOffice("https://silverscreenfolkestone.savoysystems.co.uk/SilverScreenFolkestone.dll/"), "silver-screen-folkestone"),
    f(StagSevenoaks, "stag-sevenoaks"),
    f(TheAshfordCinemaFormerlyPicturehouse, "picturehouse-ashford"),
    f(TheLightSittingbourne, "the-light-cinemas-sittingbourne"),
    f(TheWoodvilleGravesend, "the-woodville-gravesend"),
    o(VueCinemasThanetWestwoodCross, UkFeed.Vue("10063"), "vue-cinemas-westwood-cross-thanet"),
  )
  // ── london
  private def group33: Seq[UkVenueSources] = Seq(
    o(ActOneActon, UkFeed.Savoy("https://actonecinema.co.uk/ActOneCinema.dll/"), "act-one-acton"),
    o(ArthouseCrouchEnd, UkFeed.SavoyBoxOffice("http://arthousecrouchend.savoysystems.co.uk/ArthouseCrouchEnd.dll/"), "arthouse-crouch-end"),
    f(BarbicanLondonCinema1, "barbican-london-cinema-1"),
    f(BfiLondonImax, "bfi-london-imax"),
    o(BfiLondonSouthbank, UkFeed.BfiSouthbank, "bfi-london-southbank"),
    f(CastleCinemaHackney, "castle-cinema-hackney"),
    f(SidcupStoryteller, "sidcup-storyteller"),
    f(ChiswickCinema, "chiswick-cinema"),
    o(CineworldGreenwich, UkFeed.Cineworld("x087g-cineworld-cinema-london-the-o2-greenwich"), "cineworld-greenwich"),
    o(CineworldBexleyheath, UkFeed.Cineworld("x06x0-cineworld-cinema-london-bexleyheath"), "cineworld-bexleyheath"),
    o(CineworldEnfield, UkFeed.Cineworld("x078z-cineworld-cinema-london-enfield"), "cineworld-enfield"),
    o(CineworldFeltham, UkFeed.Cineworld("x06xb-cineworld-cinema-london-feltham"), "cineworld-feltham"),
    o(CineworldIlford, UkFeed.Cineworld("x06xd-cineworld-cinema-london-ilford"), "cineworld-ilford"),
    o(CineworldLeicesterSquare, UkFeed.Cineworld("x06v1-cineworld-cinema-london-leicester-square"), "cineworld-leicester-square"),
    o(CineworldLondonHounslow, UkFeed.Cineworld("g01a4-cineworld-cinema-london-hounslow"), "cineworld-london-hounslow"),
    o(CineworldSouthRuislip, UkFeed.Cineworld("x0y86-cineworld-cinema-london-south-ruislip"), "cineworld-south-ruislip"),
    o(CineworldWandsworth, UkFeed.Cineworld("x076w-cineworld-cinema-london-wandsworth"), "cineworld-wandsworth"),
    o(CineworldWembley, UkFeed.Cineworld("x0sra-cineworld-cinema-london-wembley"), "cineworld-wembley"),
    o(CineworldWestIndiaQuay, UkFeed.Cineworld("x0795-cineworld-cinema-london-west-india-quay"), "cineworld-west-india-quay"),
    o(CineworldWoodGreen, UkFeed.Cineworld("x06xs-cineworld-cinema-london-wood-green"), "cineworld-wood-green"),
    o(CineLumiereLondon, UkFeed.SavoyBoxOffice("https://cinelumiere.savoysystems.co.uk/CineLumiere.dll/"), "cine-lumiere-london"),
    f(CloseUpFilmCentreShoreditch, "close-up-film-centre-shoreditch"),
    f(CrouchEndPicturehouse, "crouch-end-picturehouse"),
    f(CurzonCinemaAldgate, "curzon-cinema-aldgate"),
    f(CurzonCinemaBloomsbury, "curzon-cinema-bloomsbury"),
    f(CurzonCinemaCamden, "curzon-cinema-camden"),
    f(CurzonCinemaHoxton, "curzon-cinema-hoxton"),
    f(CurzonCinemaKingston, "curzon-cinema-kingston"),
    f(CurzonCinemaMayfair, "curzon-cinema-mayfair"),
    f(CurzonCinemaRichmond, "curzon-cinema-richmond"),
    f(CurzonCinemaSeaContainersMondrian, "curzon-cinema-sea-containers-mondrian"),
    f(CurzonCinemaVictoria, "curzon-cinema-victoria"),
    f(CurzonSoho, "curzon-soho"),
    f(CurzonWimbledon, "curzon-wimbledon"),
    f(DavidLeanCinemaCroydon, "david-lean-cinema-croydon"),
    f(ElectricCinemaLondon, "electric-cinema-london"),
    f(ElectricCinemaWhiteCity, "electric-cinema-white-city"),
    o(EverymanAtTheWhiteleyLondon, UkFeed.Everyman("G05D7"), "everyman-at-the-whiteley-london"),
    o(EverymanBrentford, UkFeed.Everyman("G049A"), "everyman-brentford"),
    o(EverymanCinemaBakerStreet, UkFeed.Everyman("X0712"), "everyman-cinema-baker-street"),
    o(EverymanCinemaBarnet, UkFeed.Everyman("X06SI"), "everyman-cinema-barnet"),
    o(EverymanCinemaBelsizeParkHampstead, UkFeed.Everyman("X077P"), "everyman-cinema-belsize-park-hampstead"),
    o(EverymanCinemaBoroughYards, UkFeed.Everyman("G011I"), "everyman-cinema-borough-yards"),
    o(EverymanCinemaBroadgate, UkFeed.Everyman("X11NT"), "everyman-cinema-broadgate"),
    o(EverymanCinemaCanaryWharf, UkFeed.Everyman("X0VPB"), "everyman-cinema-canary-wharf"),
    o(EverymanCinemaChelsea, UkFeed.Everyman("X078X"), "everyman-cinema-chelsea"),
    o(EverymanCinemaCrystalPalace, UkFeed.Everyman("X11DR"), "everyman-cinema-crystal-palace"),
    o(EverymanCinemaEgham, UkFeed.Everyman("G01HI"), "everyman-cinema-egham"),
    o(EverymanCinemaEsher, UkFeed.Everyman("X06UF"), "everyman-cinema-esher"),
    o(EverymanCinemaHampstead, UkFeed.Everyman("X06ZW"), "everyman-cinema-hampstead"),
    o(EverymanCinemaKingSCross, UkFeed.Everyman("X0X5P"), "everyman-cinema-king-s-cross"),
    o(EverymanCinemaMaidaVale, UkFeed.Everyman("X0LWI"), "everyman-cinema-maida-vale"),
    o(EverymanCinemaMuswellHill, UkFeed.Everyman("X06SN"), "everyman-cinema-muswell-hill"),
    o(EverymanCinemaStratfordInternational, UkFeed.Everyman("G029X"), "everyman-cinema-stratford-international"),
    o(EverymanCinemaWaltonOnThames, UkFeed.Everyman("X0710"), "everyman-cinema-walton-on-thames"),
    o(EverymanCinemaIslington, UkFeed.Everyman("X077O"), "everyman-cinema-islington"),
    f(FinsburyParkPicturehouse, "finsbury-park-picturehouse"),
    f(ForestCinemasWalthamstow, "forest-cinemas-walthamstow"),
    f(GenesisTowerHamlets, "genesis-tower-hamlets"),
    o(InstituteOfContemporaryArts, UkFeed.Spektrix("ica", None), "institute-of-contemporary-arts"),
    o(Jw3Hampstead, UkFeed.Spektrix("jw3", None), "jw3-hampstead"),
    o(KilnKilburn, UkFeed.Spektrix("tricycle", None), "kiln-kilburn"),
    f(LeatherheadTheatreCinemaLeatherhead, "leatherhead-theatre-cinema-leatherhead"),
    o(LexiKensalRise, UkFeed.Savoy("https://thelexicinema.co.uk/TheLexiCinema.dll/"), "lexi-kensal-rise"),
    f(LumiereRomford, "lumiere-romford"),
    o(NovaCinemaWoking, UkFeed.NovaCinema, "nova-cinema-ambassadors-woking"),
    o(OdeonCinemaActon, UkFeed.Odeon("995"), "odeon-cinema-acton"),
    o(OdeonCinemaBeckenham, UkFeed.Odeon("695"), "odeon-cinema-beckenham"),
    o(OdeonCinemaEpsom, UkFeed.Odeon("570"), "odeon-cinema-epsom"),
    o(OdeonCinemaGreenwich, UkFeed.Odeon("963"), "odeon-cinema-greenwich"),
    o(OdeonCinemaHolloway, UkFeed.Odeon("125"), "odeon-cinema-holloway"),
    o(OdeonCinemaKingston, UkFeed.Odeon("536"), "odeon-cinema-kingston"),
    o(OdeonCinemaOrpington, UkFeed.Odeon("852"), "odeon-cinema-orpington"),
    o(OdeonCinemaRichmond, UkFeed.Odeon("888"), "odeon-cinema-richmond"),
    o(OdeonCinemaSouthWoodford, UkFeed.Odeon("090"), "odeon-cinema-south-woodford"),
    o(OdeonCinemaStreatham, UkFeed.Odeon("694"), "odeon-cinema-streatham"),
    o(OdeonCinemaTottenhamCourtRoad, UkFeed.Odeon("200"), "odeon-cinema-tottenham-court-road"),
    o(OdeonCinemaUxbridge, UkFeed.Odeon("593"), "odeon-cinema-uxbridge"),
    o(OdeonCinemaWimbledon, UkFeed.Odeon("555"), "odeon-cinema-wimbledon"),
    o(OdeonCinemaLuxeHaymarket, UkFeed.Odeon("158"), "odeon-cinema-luxe-haymarket"),
    o(OdeonLuxeIslington, UkFeed.Odeon("858"), "odeon-luxe-islington"),
    o(OdeonLuxeLeeValley, UkFeed.Odeon("949"), "odeon-luxe-lee-valley"),
    o(OdeonCinemaLuxeLeicesterSquare, UkFeed.Odeon("153"), "odeon-cinema-luxe-leicester-square"),
    o(OdeonCinemaLuxePutney, UkFeed.Odeon("486"), "odeon-cinema-luxe-putney"),
    o(OdeonLuxeSwissCottage, UkFeed.Odeon("838"), "odeon-luxe-swiss-cottage"),
    o(OdeonLuxeWestEnd, UkFeed.Odeon("155"), "odeon-luxe-west-end"),
    f(OlympicCinemaBarnes, "olympic-cinema-barnes"),
    f(EmpireCinemaSutton, "empire-cinema-sutton"),
    f(Peckhamplex, "peckhamplex"),
    o(PhoenixCinemaEastFinchley, UkFeed.Savoy("https://www.phoenixcinema.co.uk/PhoenixCinemaLondon.dll/"), "phoenix-cinema-east-finchley"),
    f(PicturehouseCentralLondon, "picturehouse-central-london"),
    f(PicturehouseClapham, "picturehouse-clapham"),
    f(PicturehouseEalingFilmworks, "picturehouse-ealing-filmworks"),
    f(PicturehouseEastDulwich, "picturehouse-east-dulwich"),
    f(PicturehouseEpsomSquare, "picturehouse-epsom-square"),
    f(PicturehouseGreenwich, "picturehouse-greenwich"),
    f(PicturehouseHackney, "picturehouse-hackney"),
    f(PicturehouseWestNorwood, "picturehouse-west-norwood"),
    f(PrinceCharlesLondon, "prince-charles-london"),
    f(RegentStreetCinemaLondon, "regent-street-cinema-london"),
    o(RichMixBethnalGreen, UkFeed.Spektrix("richmix", None), "rich-mix-bethnal-green"),
    o(RioDalston, UkFeed.Savoy("https://riocinema.org.uk/Rio.dll/"), "rio-dalston"),
    o(RiversideStudiosHammersmith, UkFeed.Spektrix("riversidestudios", None), "riverside-studios-hammersmith"),
    f(RooftopFilmClubPeckhamBusseyBuilding, "rooftop-film-club-peckham-bussey-building"),
    f(RooftopFilmClubStratfordRoofEast, "rooftop-film-club-stratford-roof-east"),
    f(ScienceMuseumLondonImax, "science-museum-london-imax"),
    f(ArchlightCinemas, "archlight-cinemas"),
    o(TheArzner, UkFeed.Savoy("https://thearzner.com/TheArzner.dll/"), "the-arzner"),
    f(TheCinemaAtSelfridges, "the-cinema-at-selfridges"),
    f(TheCinemaInThePowerStation, "the-cinema-in-the-power-station"),
    o(TheGardenCinema, UkFeed.SavoyBoxOffice("https://bookings.thegardencinema.co.uk/TheGardenCinema.dll/"), "the-garden-cinema"),
    f(TheGatePicturehouseLondon, "the-gate-picturehouse-london"),
    f(TheLightCinemasAddlestone, "the-light-cinemas-addlestone"),
    f(TheNickelLondon, "the-nickel-london"),
    f(TheRitzyPicturehouseBrixton, "the-ritzy-picturehouse-brixton"),
    o(VueCinemasBromley, UkFeed.Vue("10093"), "vue-cinemas-bromley"),
    o(VueCinemasDagenham, UkFeed.Vue("10044"), "vue-cinemas-dagenham"),
    o(VueCinemasEltham, UkFeed.Vue("10096"), "vue-cinemas-eltham"),
    o(VueCinemasFinchleyRoadSwissCottage, UkFeed.Vue("10031"), "vue-cinemas-finchley-road-swiss-cottage"),
    o(VueCinemasFulham, UkFeed.Vue("10046"), "vue-cinemas-fulham"),
    o(VueCinemasHarrow, UkFeed.Vue("10006"), "vue-cinemas-harrow"),
    o(VueCinemasIslington, UkFeed.Vue("10032"), "vue-cinemas-islington"),
    o(VueCinemasFinchley, UkFeed.Vue("10022"), "vue-cinemas-finchley"),
    o(VueCinemasPiccadillyCircus, UkFeed.Vue("10080"), "vue-cinemas-piccadilly-circus"),
    o(VueCinemasPurleyWayCroydon, UkFeed.Vue("10025"), "vue-cinemas-purley-way-croydon"),
    o(VueCinemasRomford, UkFeed.Vue("10013"), "vue-cinemas-romford"),
    o(VueCinemasStainesUponThames, UkFeed.Vue("10024"), "vue-cinemas-staines-upon-thames"),
    o(VueCinemasStratford, UkFeed.Vue("10074"), "vue-cinemas-stratford"),
    o(VueCinemasWestEnd, UkFeed.Vue("10030"), "vue-cinemas-west-end"),
    o(VueCinemasWestfieldShepherdSBush, UkFeed.Vue("10072"), "vue-cinemas-westfield-shepherd-s-bush"),
    o(VueCinemasWoodGreen, UkFeed.Vue("10071"), "vue-cinemas-wood-green"),
    f(WyllyottsTheatrePottersBar, "wyllyotts-theatre-potters-bar"),
  )
  // ── wiltshire
  private def group34: Seq[UkVenueSources] = Seq(
    o(CineworldShawRidgeSwindon, UkFeed.Cineworld("x079h-cineworld-cinema-swindon-shaw-ridge"), "cineworld-shaw-ridge-swindon"),
    o(EverymanCinemaSalisbury, UkFeed.Everyman("G01QM"), "everyman-cinema-salisbury"),
    o(OdeonCinemaAndover, UkFeed.Odeon("755"), "odeon-cinema-andover"),
    o(OdeonCinemaSalisbury, UkFeed.Odeon("220"), "odeon-cinema-salisbury"),
    o(OdeonCinemaTrowbridge, UkFeed.Odeon("847"), "odeon-cinema-trowbridge"),
    f(ReelCinemaChippenhamAstoria, "astoria-chippenham"),
    o(RegalCinemaFordingbridge, UkFeed.Savoy("https://theregalcinema.org/RegalFordingbridge.dll/"), "regal-fordingbridge"),
    f(TheParadeCinemaMarlborough, "the-parade-cinema-marlborough"),
    o(VueCinemasSwindon, UkFeed.Vue("10098"), "vue-cinemas-swindon"),
  )
  // ── yorkshire
  private def group35: Seq[UkVenueSources] = Seq(
    o(CineworldSheffield, UkFeed.Cineworld("x079y-cineworld-cinema-sheffield"), "cineworld-sheffield"),
    o(OdeonLuxeSheffield, UkFeed.Odeon("209"), "odeon-luxe-sheffield"),
    f(ParamountPenistone, "paramount-penistone"),
    f(ShowroomSheffield, "showroom-sheffield"),
    f(TheLightSheffield, "the-light-cinemas-sheffield"),
    o(VueCinemasSheffield, UkFeed.Vue("10042"), "vue-cinemas-sheffield"),
  )
  // ── antrim
  private def group36: Seq[UkVenueSources] = Seq(
    f(IMCCinemaBallymena, "imc-cinema-ballymena"),
    f(MovieHouseGlengormley, "movie-house-glengormley"),
    f(OmniplexAntrim, "omniplex-antrim"),
    f(OmniplexCarrickfergus, "omniplex-carrickfergus"),
    f(OmniplexLarne, "omniplex-larne"),
  )
  // ── norwich
  private def group37: Seq[UkVenueSources] = Seq(
    f(ArcCinemaGreatYarmouth, "arc-cinema-at-the-royalty-great-yarmouth"),
    f(CentralCinemaFakenham, "central-fakenham"),
    f(CornExchangeCinemaKingSLynn, "corn-exchange-cinema-king-s-lynn"),
    f(EastCoastCinemaLowestoft, "east-coast-cinema-lowestoft"),
    f(LittleTheatreSheringham, "little-theatre-sheringham"),
    f(MajesticKingSLynn, "majestic-king-s-lynn"),
    f(MarinaTheatreLowestoft, "marina-theatre-lowestoft"),
    o(OdeonNorwich, UkFeed.Odeon("957"), "odeon-cinema-norwich"),
    f(OrionDereham, "orion-dereham"),
    o(PalaceCinemaGorlestonOnSea, UkFeed.SavoyBoxOffice("https://palacecinemagorleston.savoysystems.co.uk/PalaceCinemaGorleston.dll/"), "palace-cinema-gorleston-on-sea"),
    f(CinemaCityPicturehouseNorwich, "cinema-city-picturehouse-norwich"),
    f(RegalMovieplexCromer, "regal-movieplex-cromer"),
    f(TheLightThetford, "the-light-cinemas-thetford"),
    o(VueCinemasNorwich, UkFeed.Vue("10014"), "vue-cinemas-norwich"),
  )
  // ── east-sussex
  private def group38: Seq[UkVenueSources] = Seq(
    o(CineworldBrighton, UkFeed.Cineworld("x079b-cineworld-cinema-brighton"), "cineworld-brighton"),
    o(CineworldEastbourne, UkFeed.Cineworld("x129j-cineworld-cinema-eastbourne-at-the-beacon"), "cineworld-eastbourne"),
    f(DepotLewes, "depot-lewes"),
    f(DukeOfYorkSPicturehouseBrighton, "duke-of-york-s-picturehouse"),
    f(DukeSAtKomediaPicturehouse, "duke-s-at-komedia-picturehouse"),
    o(ElectricPalaceHastings, UkFeed.Savoy("https://electricpalacecinema.com/ElectricPalaceHastings.dll/"), "electric-palace-hastings"),
    f(KinoRye, "kino-rye"),
    f(KinoTeatr, "kino-teatr-st-leonards-on-sea"),
    o(OdeonCinemaBrighton, UkFeed.Odeon("824"), "odeon-cinema-brighton"),
    o(OdeonCinemaHastings, UkFeed.Odeon("124"), "odeon-cinema-hastings"),
    f(PavilionHailsham, "pavilion-hailsham"),
    o(PictureHouseUckfield, UkFeed.Spektrix("uckfieldpicturehouse", None), "picturehouse-uckfield"),
    o(TownerEastbourneCinema, UkFeed.Spektrix("townereastbourne", None), "towner-eastbourne-cinema"),
  )
  // ── cheshire
  private def group39: Seq[UkVenueSources] = Seq(
    o(BuxtonCinemaPavilionArtsCentre, UkFeed.Spektrix("buxtonoperahouse", Some("Pavilion Arts Centre")), "buxton-cinema"),
    f(CinemacMacclesfield, "cinemac-macclesfield"),
    o(CineworldWarrington, UkFeed.Cineworld("x12iv-cineworld-cinema-warrington"), "cineworld-warrington"),
    f(CurzonCinemaKnutsford, "curzon-cinema-knutsford"),
    o(EverymanCinemaAltrincham, UkFeed.Everyman("X11DP"), "everyman-cinema-altrincham"),
    o(OdeonCinemaCrewe, UkFeed.Odeon("756"), "odeon-cinema-crewe"),
    o(OdeonCinemaNorthwichBaronsQuay, UkFeed.Odeon("853"), "odeon-cinema-northwich"),
    o(OdeonLuxeWarrington, UkFeed.Odeon("912"), "odeon-luxe-warrington"),
    f(PicturehouseChester, "picturehouse-chester"),
    f(ReelCinemaWidnes, "reel-cinema-widnes"),
    f(RexWilmslow, "rex-wilmslow"),
    f(StoryhouseChester, "storyhouse-chester"),
    o(VueCinemasAltrincham, UkFeed.Vue("10077"), "vue-cinemas-altrincham"),
    o(VueCinemasCheshireOaks, UkFeed.Vue("10034"), "vue-cinemas-ellesmere-port"),
  )
  // ── gwynedd
  private def group40: Seq[UkVenueSources] = Seq(
    f(CellBBlaenauFfestiniog, "cellb-blaenau-ffestiniog"),
    f(EmpireCinemaHolyhead, "empire-cinema-holyhead"),
    f(GaleriCaenarfon, "galeri-caenarfon"),
    o(MagicLanternTywyn, UkFeed.Savoy("https://tywyncinema.co.uk/MagicLantern.dll/"), "magic-lantern-llansern-hud-tywyn"),
    o(NeuaddDwyforPwllheli, UkFeed.Ticketsolve("neuadddwyfor"), "neuadd-dwyfor-pwllheli"),
    f(PontioArtsInnovationCentreBangor, "pontio-arts-innovation-centre-bangor"),
    f(TheatrDerekWilliams, "theatr-derek-williams"),
  )
  // ── edinburgh-and-lothians
  private def group41: Seq[UkVenueSources] = Seq(
    o(CineworldEdinburgh, UkFeed.Cineworld("x07a1-cineworld-cinema-edinburgh"), "cineworld-edinburgh"),
    f(DominionCinemaEdinburgh, "dominion-edinburgh"),
    o(EverymanCinemaEdinburgh, UkFeed.Everyman("G018L"), "everyman-cinema-edinburgh"),
    f(FilmhouseEdinburgh, "filmhouse-edinburgh"),
    o(OdeonEdinburghFortKinnaird, UkFeed.Odeon("850"), "odeon-cinema-edinburgh-fort-kinnaird"),
    o(OdeonEdinburghLothianRoad, UkFeed.Odeon("750"), "odeon-cinema-edinburgh-lothian-road"),
    o(OdeonLuxeEdinburghEdinburghWest, UkFeed.Odeon("841"), "odeon-luxe-edinburgh-edinburgh-west"),
    f(ScotsmanPicturehouseEdinburgh, "scotsman-picturehouse-edinburgh"),
    f(TheCameoPicturehouse, "the-cameo-picturehouse"),
    f(TheFraserCentreTranent, "the-fraser-centre-tranent"),
    o(VueCinemasLivingston, UkFeed.Vue("10001"), "vue-cinemas-livingston"),
    o(VueEdinburghOceanTerminal, UkFeed.Vue("10010"), "vue-ocean-terminal-edinburgh"),
    o(VueEdinburghOmniCentre, UkFeed.Vue("10038"), "vue-omni-centre-edinburgh"),
  )
  // ── worcestershire
  private def group42: Seq[UkVenueSources] = Seq(
    f(CastlemortonCinemaMortonMajestic, "castlemorton-cinema-morton-majestic"),
    o(FuturistCinema, UkFeed.Savoy("https://futuristcinema.uk/FuturistCinema.dll/"), "lume-cinema-and-cafe"),
    f(MalvernTheatres, "cinema-malvern"),
    o(Number8Pershore, UkFeed.Ticketsolve("number8"), "number-8-pershore"),
    o(OdeonCinemaWorcester, UkFeed.Odeon("282"), "odeon-cinema-worcester"),
    f(RegalCinemaEvesham, "regal-evesham"),
    f(RegalCinemaTenburyWells, "regal-tenbury-wells"),
    o(VueCinemasRedditch, UkFeed.Vue("10081"), "vue-cinemas-redditch"),
    o(VueCinemasWorcester, UkFeed.Vue("10017"), "vue-cinemas-worcester"),
  )
  // ── herefordshire
  private def group43: Seq[UkVenueSources] = Seq(
    f(CourtyardHereford, "courtyard-hereford"),
    f(GatewayCinemaRossOnWye, "gateway-ross-on-wye"),
    o(OdeonCinemaHereford, UkFeed.Odeon("848"), "odeon-cinema-hereford"),
    f(RichardBoothSBookshopHayOnWye, "richard-booth-s-cinema-hay-on-wye"),
  )
  // ── highlands-and-islands
  private def group44: Seq[UkVenueSources] = Seq(
    f(AnLanntairArtsCentreStornowayIsleOfLewis, "an-lanntair-arts-centre-stornoway-isle-of-lewis"),
    f(CromartyCinema, "cromarty-cinema-cromarty"),
    f(EdenCourtTheatreInverness, "eden-court-theatre-inverness"),
    f(HighlandCinemaFortWilliam, "highland-cinema-fort-william"),
    f(LASPortRighArosCinemaPortree, "aros-portree"),
    f(MareelLerwickShetlandIslands, "mareel-lerwick-shetland-islands"),
    f(MerlinCinemaThurso, "merlin-thurso"),
    f(PhoenixKirkwallOkneyIslands, "phoenix-kirkwall-okney-islands"),
    f(SpeyValleyCinemaAviemore, "spey-valley-aviemore"),
    o(VueCinemasInverness, UkFeed.Vue("10049"), "vue-cinemas-inverness"),
    f(WestSideCinemaStromness, "west-side-cinema-stromness"),
  )
  // ── lancashire
  private def group45: Seq[UkVenueSources] = Seq(
    f(ArcCinemaBlackpool, "the-backlot-cinema-and-diner"),
    f(ArcCinemaPreston, "arc-cinema-preston"),
    o(CineworldBolton, UkFeed.Cineworld("x079p-cineworld-cinema-bolton"), "cineworld-bolton"),
    o(CineworldBroughton, UkFeed.Cineworld("x0vhp-cineworld-cinema-broughton"), "cineworld-broughton"),
    o(EverymanCinemaClitheroe, UkFeed.Everyman("X11NR"), "everyman-cinema-clitheroe"),
    f(FlowerBowlEntertainmentCentrePreston, "flower-bowl-entertainment-centre-preston"),
    o(LowtherPavilionLytham, UkFeed.Spektrix("lowtherpavilion", None), "lowther-pavilion-lytham"),
    o(OdeonCinemaPreston, UkFeed.Odeon("925"), "odeon-cinema-preston"),
    o(OdeonCinemaRochdale, UkFeed.Odeon("843"), "odeon-cinema-rochdale"),
    f(ReelCinemaBlackburn, "reel-cinema-blackburn"),
    f(ReelCinemaChorley, "reel-cinema-chorley"),
    f(ReelCinemasBurnley, "reel-cinema-burnley"),
    f(RegentBlackpool, "regent-blackpool"),
    o(TheDukesLancaster, UkFeed.Spektrix("dukeslancaster", None), "the-dukes-lancaster"),
    f(TheIslandLythamStAnnes, "island-cinemas-lytham-st-annes"),
    f(TheLightBolton, "the-light-cinemas-bolton"),
    o(VueCinemasAccrington, UkFeed.Vue("10051"), "vue-cinemas-accrington"),
    o(VueCinemasBlackburn, UkFeed.Vue("10007"), "vue-cinemas-blackburn"),
    o(VueCinemasBolton, UkFeed.Vue("10040"), "vue-cinemas-bolton"),
    o(VueCinemasBury, UkFeed.Vue("10073"), "vue-cinemas-bury"),
    o(VueCinemasCleveleys, UkFeed.Vue("10060"), "vue-cinemas-cleveleys"),
    o(VueCinemasLancaster, UkFeed.Vue("10061"), "vue-cinemas-lancaster"),
    o(VueCinemasPreston, UkFeed.Vue("10043"), "vue-cinemas-preston"),
  )
  // ── suffolk
  private def group46: Seq[UkVenueSources] = Seq(
    f(AbbeygateBuryStEdmunds, "abbeygate-bury-st-edmunds"),
    o(AldeburghCinema, UkFeed.Savoy("https://aldeburghcinema.co.uk/AldeburghCinema.dll/"), "aldeburgh-cinema"),
    o(CineworldBuryStEdmunds, UkFeed.Cineworld("x07mh-cineworld-cinema-bury-st-edmunds"), "cineworld-bury-st-edmunds"),
    o(CineworldHaverhill, UkFeed.Cineworld("x08wo-cineworld-cinema-haverhill"), "cineworld-haverhill"),
    o(CineworldIpswich, UkFeed.Cineworld("x078j-cineworld-cinema-ipswich"), "cineworld-ipswich"),
    f(ElectricPicturePalaceSouthwold, "electric-picture-palace-southwold"),
    o(EverymanBuryStEdmunds, UkFeed.Everyman("G0210"), "everyman-bury-st-edmunds"),
    o(FilmTheatreLeiston, UkFeed.SavoyBoxOffice("https://leistonfilmtheatre.savoysystems.co.uk/LeistonFilmTheatre.dll/"), "film-theatre-leiston"),
    o(HaverhillArtsCentre, UkFeed.Spektrix("haverhillartscentre", None), "haverhill-arts-centre-haverhill"),
    f(KingStreetCinema, "ipswich-film-theatre-ipswich"),
    o(KingsCinemaNewmarket, UkFeed.Savoy("https://kingscinema.co.uk/KingsCinema.dll/"), "kings-cinema-newmarket"),
    f(OmniplexIpswichFormerlyEmpire, "empire-cinema-ipswich"),
    f(PalaceCinemaFelixstowe, "palace-cinema-felixstowe"),
    o(RegalStowmarket, UkFeed.Savoy("https://regalstowmarket.co.uk/RegalStowmarket.dll/"), "regal-stowmarket"),
    o(RiversideTheatreWoodbridge, UkFeed.SavoyBoxOffice("https://riversidewoodbridge.savoysystems.co.uk/RiversideWoodbridge.dll/"), "riverside-theatre-woodbridge"),
  )
  // ── bedfordshire
  private def group47: Seq[UkVenueSources] = Seq(
    o(CineworldLuton, UkFeed.Cineworld("x06xf-cineworld-cinema-luton"), "cineworld-luton"),
    o(VueCinemasBedford, UkFeed.Vue("10092"), "vue-cinemas-bedford"),
  )
  // ── powys
  private def group48: Seq[UkVenueSources] = Seq(
    f(ColiseumCinemaBrecon, "coliseum-brecon"),
    o(OdeonCinemaWrexham, UkFeed.Odeon("598"), "odeon-cinema-wrexham"),
    f(WyesideArtsCentreBuilthWells, "wyeside-arts-centre-builth-wells"),
  )
  // ── northumberland
  private def group49: Seq[UkVenueSources] = Seq(
    o(ForumCinemaHexham, UkFeed.Savoy("https://forumhexham.com/ForumHexham.dll/"), "forum-hexham"),
    f(MarketPavilionCinemaBlyth, "market-pavillion-cinema-blyth"),
    o(TheMaltingsBerwickUponTweed, UkFeed.Spektrix("maltingsberwick", None), "the-maltings-berwick-upon-tweed"),
    o(VueCinemasCramlington, UkFeed.Vue("10085"), "vue-cinemas-cramlington"),
  )
  // ── shropshire
  private def group50: Seq[UkVenueSources] = Seq(
    f(AssemblyRoomsLudlow, "assembly-rooms-ludlow"),
    o(CineworldShrewsbury, UkFeed.Cineworld("x06xk-cineworld-cinema-shrewsbury"), "cineworld-shrewsbury"),
    o(CineworldTelford, UkFeed.Cineworld("x0u5o-cineworld-cinema-telford"), "cineworld-telford"),
    f(FestivalDraytonCentre, "festival-drayton-centre-market-drayton"),
    o(MaonaCinemaOswestry, UkFeed.SavoyBoxOffice("https://maonacinema.savoysystems.co.uk/MaonaCinema.dll/"), "maona-cinema-oswestry"),
    o(OdeonLuxeTelford, UkFeed.Odeon("916"), "odeon-luxe-telford"),
    o(OldMarketHallShrewsbury, UkFeed.Spektrix("theatresevern", Some("Old Market Hall")), "old-market-hall-shrewsbury"),
    f(ReelCinemaBridgnorthMajestic, "majestic-bridgnorth"),
    f(WellingtonOrbit, "wellington-orbit-wellington"),
  )
  // ── belfast
  private def group51: Seq[UkVenueSources] = Seq(
    o(CineworldBelfast, UkFeed.Cineworld("x0787-cineworld-cinema-belfast"), "cineworld-belfast"),
    f(MovieHouseCitySideBelfast, "movie-house-city-side-belfast"),
    o(OdeonCinemaBelfast, UkFeed.Odeon("600"), "odeon-cinema-belfast"),
    f(OmniplexBelfast, "omniplex-belfast"),
    f(OmniplexLisburn, "omniplex-lisburn"),
    f(QueenSFilmTheatreBelfast, "queen-s-film-theatre-belfast"),
    f(StrandArtsCentreBelfast, "strand-arts-centre-belfast"),
    f(TheAvenueCinemaBelfast, "the-avenue-cinema-belfast"),
  )
  // ── cornwall
  private def group52: Seq[UkVenueSources] = Seq(
    o(CineworldPlymouth, UkFeed.Cineworld("x12iu-cineworld-cinema-plymouth"), "cineworld-plymouth"),
    o(FilmhouseNewlyn, UkFeed.Savoy("https://newlynfilmhouse.com/NewlynFilmhouse.dll/"), "filmhouse-newlyn"),
    f(FloraCinemaHelston, "flora-helston"),
    f(MerlinCapitolBodmin, "capitol-bodmin"),
    f(MerlinRegalCinemaRedruth, "regal-redruth"),
    f(MerlinSavoyPenzance, "savoy-penzance"),
    f(PhoenixCinemaFalmouth, "phoenix-falmouth"),
    o(PlymouthArtsCinema, UkFeed.Spektrix("plymouthartscentre", None), "plymouth-arts-cinema-plymouth"),
    f(RebelCinema, "rebel-bude"),
    f(RoyalStIvesCinema, "royal-st-ives"),
    f(TheAstraCinemaStMawgan, "the-astra-cinema-st-mawgan"),
    o(ThePolyFalmouth, UkFeed.SavoyBoxOffice("http://thepoly.savoysystems.co.uk/ThePoly.dll/"), "the-poly-falmouth"),
    o(VueCinemasPlymouth, UkFeed.Vue("10027"), "vue-cinemas-plymouth"),
    f(WTWLighthouseNewquay, "lighthouse-newquay"),
    f(WTWPlazaTruro, "plaza-truro"),
    f(WTWRegalWadebridge, "regal-wadebridge"),
    f(WTWWhiteRiverCinema, "white-river-st-austell"),
  )
  // ── northamptonshire
  private def group53: Seq[UkVenueSources] = Seq(
    f(ArcCinemaDaventry, "arc-cinema-daventry"),
    o(CineworldRushdenLakes, UkFeed.Cineworld("x125z-cineworld-cinema-rushden-lakes"), "cineworld-rushden"),
    f(ForumCinemaNorthampton, "forum-northampton"),
    f(NorthamptonFilmhouse, "northampton-filmhouse-northampton"),
    o(OdeonNorthampton, UkFeed.Odeon("380"), "odeon-northampton"),
    o(SavoyCinemaCorby, UkFeed.Savoy("https://savoycorby.co.uk/SavoyCorby.dll/"), "savoy-corby"),
    o(VueCinemasNorthampton, UkFeed.Vue("10005"), "vue-cinemas-northampton"),
  )
  // ── central-scotland
  private def group54: Seq[UkVenueSources] = Seq(
    f(ChalmersAlloaCinema, "chalmers-alloa-cinema"),
    o(CineworldFalkirk, UkFeed.Cineworld("x06xa-cineworld-cinema-falkirk"), "cineworld-falkirk"),
    o(HippodromeBoNess, UkFeed.Spektrix("falkirkcommunitytrust", Some("Hippodrome")), "hippodrome-bo-ness"),
    f(MacrobertArtCentreStirling, "macrobert-art-centre-stirling"),
    o(VueCinemasStirling, UkFeed.Vue("10064"), "vue-cinemas-stirling"),
  )
  // ── devon
  private def group55: Seq[UkVenueSources] = Seq(
    f(AlexandraNewtonAbbot, "alexandra-newton-abbot"),
    o(BarnCinemaDartingtonArtCentre, UkFeed.Spektrix("dartington", None), "dartington-art-centre-totnes"),
    f(CentralCinemaBarnstaple, "central-barnstaple"),
    f(EmbassyCinemaIlfracombe, "embassy-ilfracombe"),
    o(EverymanCinemaPlymouth, UkFeed.Everyman("G01RJ"), "everyman-cinema-plymouth"),
    f(KingsCinemaKingsbridge, "kings-kingsbridge"),
    f(LyntonCinema, "lynton-cinema-lynton-lynmouth"),
    f(NewCarltonOkehampton, "new-carlton-okehampton"),
    f(NewCentralCinemaTorquay, "new-central-cinema-torquay"),
    o(OdeonCinemaExeter, UkFeed.Odeon("086"), "odeon-cinema-exeter"),
    o(PavilionsTeignmouth, UkFeed.SavoyBoxOffice("http://pavilionsteignmouth.savoysystems.co.uk/PavilionsTeignmouth.dll/"), "pavilions-teignmouth"),
    f(PicturehouseExeter, "picturehouse-exeter"),
    o(PloughArtsCentreTorrington, UkFeed.Spektrix("theploughartscentre", None), "plough-arts-centre-torrington"),
    f(RadwaySidmouth, "radway-sidmouth"),
    f(SavoyScottCinemasExmouth, "savoy-exmouth"),
    o(TheBeehiveHoniton, UkFeed.SavoyBoxOffice("http://thebeehive.savoysystems.co.uk/TheBeehive.dll/"), "the-beehive-honiton"),
    o(TheFlavel, UkFeed.Savoy("https://theflavel.org.uk/TheFlavel.dll/"), "flavel-dartmouth"),
    f(TheWatermarkIvybridge, "the-watermark-ivybridge"),
    f(TivoliTiverton, "tivoli-tiverton"),
    f(TotnesCinema, "totnes-cinema"),
    o(VueCinemasExeter, UkFeed.Vue("10059"), "vue-cinemas-exeter"),
    o(VueCinemasTorbayPaignton, UkFeed.Vue("10084"), "vue-cinemas-paignton"),
  )
  // ── nottinghamshire
  private def group56: Seq[UkVenueSources] = Seq(
    f(ArcCinemaAtTheByronHucknall, "arc-cinema-at-the-byron-hucknall"),
    f(ArcCinemaBeeston, "arc-cinema-beeston"),
    o(BroadwayCinemaNottingham, UkFeed.Spektrix("broadwaynottingham", None), "broadway-nottingham"),
    o(OdeonCinemaMansfield, UkFeed.Odeon("842"), "odeon-cinema-mansfield"),
    o(OdeonCinemaNewark, UkFeed.Odeon("757"), "odeon-cinema-newark"),
    f(ReelCinemaScalaIlkeston, "scala-ilkeston"),
    f(SavoyCinemaNottingham, "savoy-nottingham"),
    o(SavoyWorksop, UkFeed.Savoy("https://savoyworksop.co.uk/SavoyWorksop.dll/"), "savoy-worksop"),
    o(ShowcaseDeLuxNottingham, UkFeed.Showcase("X06JN"), "showcase-cinema-nottingham"),
    o(VueCinemasNottingham, UkFeed.Vue("20003"), "cineworld-nottingham"),
  )
  // ── gloucestershire
  private def group57: Seq[UkVenueSources] = Seq(
    o(CineworldCheltenham, UkFeed.Cineworld("x07p5-cineworld-cinema-cheltenham"), "cineworld-cheltenham"),
    o(CineworldGloucesterQuays, UkFeed.Cineworld("x079m-cineworld-cinema-gloucester-quays"), "cineworld-gloucester"),
    f(ElectricPictureHouseWottonUnderEdge, "electric-picture-house-wotton-under-edge"),
    o(EverymanCheltenham, UkFeed.Everyman("G01VU"), "tivoli-cheltenham"),
    f(GuildhallCinemaGloucester, "guildhall-cinema-gloucester"),
    f(MerlinStudioColeford, "studio-coleford"),
    f(PalaceCinemaCinderford, "palace-cinema-cinderford"),
    o(RosesTheatreTewkesbury, UkFeed.Spektrix("rosestheatretewkesbury", None), "roses-theatre-tewksbury"),
    f(SherborneCinemaGloucester, "sherborne-gloucester"),
    o(VueCinemasStroud, UkFeed.Vue("10083"), "vue-cinemas-stroud"),
  )
  // ── manchester
  private def group58: Seq[UkVenueSources] = Seq(
    o(CineworldAshtonUnderLyne, UkFeed.Cineworld("x06wz-cineworld-cinema-ashton-under-lyne"), "cineworld-ashton-under-lyne"),
    o(CineworldManchester, UkFeed.Cineworld("x079r-cineworld-cinema-didsbury"), "cineworld-manchester"),
    o(CultplexManchester, UkFeed.Savoy("https://cultplex.co.uk/Cultplex.dll/"), "cultplex-manchester"),
    o(EverymanManchesterStJohns, UkFeed.Everyman("X11NP"), "everyman-manchester-st-johns"),
    f(FlixTreehouseManchester, "flix-treehouse-manchester"),
    f(HomeManchester, "home-manchester"),
    f(LeighFilmFactory, "leigh-film-factory"),
    f(NorthernLightSale, "northern-light-sale"),
    o(OdeonCinemaManchesterGreatNorthern, UkFeed.Odeon("856"), "odeon-cinema-manchester-great-northern"),
    o(OdeonCinemaManchesterTraffordCentre, UkFeed.Odeon("955"), "odeon-cinema-manchester-trafford-centre"),
    o(OdeonCinemaOldham, UkFeed.Odeon("851"), "odeon-cinema-oldham"),
    f(EmpireCinemaWigan, "empire-cinema-wigan"),
    f(PlazaStockport, "plaza-stockport"),
    f(ReelCinemaRochdale, "reel-cinema-rochdale"),
    o(RegentMarple, UkFeed.Savoy("https://regentmarple.co.uk/RegentCinemaMarple.dll/"), "regent-marple"),
    f(SavoyHeatonMoor, "savoy-heaton-moor"),
    f(TheLightCinemasStockport, "the-light-cinemas-stockport"),
    o(VueCinemasManchesterPrintworks, UkFeed.Vue("10091"), "vue-cinemas-manchester-printworks"),
    o(VueCinemasManchesterQuayside, UkFeed.Vue("10057"), "vue-cinemas-manchester-quayside"),
  )
  // ── buckinghamshire
  private def group59: Seq[UkVenueSources] = Seq(
    o(CineworldHighWycombe, UkFeed.Cineworld("x08c8-cineworld-cinema-high-wycombe"), "cineworld-high"),
    o(CineworldMiltonKeynes, UkFeed.Cineworld("x06xg-cineworld-cinema-milton-keynes"), "cineworld-milton-keynes"),
    o(EverymanCinemaGerrardsCross, UkFeed.Everyman("X06SK"), "everyman-cinema-gerrards-cross"),
    o(EverymanCinemaMarlow, UkFeed.Everyman("G01RL"), "everyman-cinema-marlow"),
    o(OdeonCinemaAylesbury, UkFeed.Odeon("845"), "odeon-cinema-aylesbury"),
    o(OdeonCinemaMiltonKeynes, UkFeed.Odeon("849"), "odeon-cinema-milton-keynes"),
    f(OmniplexHighWycombeFormerlyEmpire, "empire-cinema-high-wycombe"),
    f(VillagePictureHouseCuddington, "village-picture-house-cuddington"),
  )
  // ── west-yorkshire
  private def group60: Seq[UkVenueSources] = Seq(
    o(CineworldBradford, UkFeed.Cineworld("x06x2-cineworld-cinema-bradford"), "cineworld-bradford"),
    o(CineworldLeeds, UkFeed.Cineworld("x106z-cineworld-cinema-leeds-white-rose"), "cineworld-leeds"),
    o(CineworldWakefield, UkFeed.Cineworld("x06xp-cineworld-cinema-wakefield"), "cineworld-wakefield"),
    f(CottageRoadCinemaLeeds, "cottage-road-cinema-leeds"),
    o(EverymanCinemaLeeds, UkFeed.Everyman("X0QM5"), "everyman-cinema-leeds"),
    f(HeartCentreHeadingley, "heart-centre-headingley"),
    o(HebdenBridgePicturehouse, UkFeed.Savoy("https://hebdenbridgepicturehouse.co.uk/HebdenBridgePictureHouse.dll/"), "picturehouse-hebden-bridge"),
    o(HydeParkPictureHouse, UkFeed.Spektrix("leedsheritagetheatres", Some("Hyde Park Picture House")), "hyde-park-picture-house-leeds"),
    o(IlkleyCinema, UkFeed.SavoyBoxOffice("https://book.ilkleycinema.co.uk/IlkleyCinema.dll/"), "ilkley-cinema-ilkley"),
    o(OdeonCinemaHuddersfield, UkFeed.Odeon("953"), "odeon-cinema-huddersfield"),
    o(OdeonLuxeLeedsThorpePark, UkFeed.Odeon("860"), "odeon-luxe-leeds"),
    o(OdeonLuxeLeedsBradford, UkFeed.Odeon("484"), "odeon-luxe-bradford"),
    f(PictureHouseKeighley, "picturehouse-keighley"),
    f(PicturevilleScienceAndMediaMuseumBradford, "national-science-and-media-museum-bradford"),
    f(PlazaSkipton, "plaza-skipton"),
    f(ReelCinemaWakefield, "reel-cinema-wakefield"),
    f(RexElland, "rex-elland"),
    o(ShowcaseDeLuxLeeds, UkFeed.Showcase("X06JK"), "showcase-cinema-leeds"),
    f(TheLightBradford, "the-light-cinemas-bradford"),
    o(VueCinemasCastleford, UkFeed.Vue("20002"), "cineworld-castleford"),
    o(VueCinemasHalifax, UkFeed.Vue("10076"), "vue-cinemas-halifax"),
    o(VueCinemasLeedsKirkstallRoad, UkFeed.Vue("10037"), "vue-cinemas-leeds-kirkstall-road"),
    o(VueCinemasLeedsTheLight, UkFeed.Vue("10012"), "vue-cinemas-leeds-the-light"),
    o(WetherbyFilmTheatre, UkFeed.SavoyBoxOffice("https://book.wetherbycinema.com/WetherbyFilmTheatre.dll/"), "wetherby-film-theatre-wetherby"),
  )
  // ── dorset
  private def group61: Seq[UkVenueSources] = Seq(
    f(ColosseumBournemouth, "colosseum-bournemouth"),
    o(ElectricPalaceBridport, UkFeed.Spektrix("electricpalace", None), "electric-palace-bridport"),
    o(HilltopCinemaShaftesburyArtsCentre, UkFeed.SavoyBoxOffice("http://shaftesburyartscentre.savoysystems.co.uk/ShaftesburyArtsCentre.dll/"), "hilltop-cinema-shaftesbury-arts-centre"),
    o(LighthousePoole, UkFeed.Spektrix("lighthousepoole", None), "lighthouse-poole"),
    o(MowlemTheatre, UkFeed.Spektrix("mowlemswanage", None), "mowlem-swanage"),
    o(OdeonCinemaBournemouthBH2, UkFeed.Odeon("854"), "odeon-cinema-bournemouth"),
    o(OdeonCinemaDorchester, UkFeed.Odeon("985"), "odeon-cinema-dorchester"),
    f(PlazaCinemaDorchester, "plaza-cinema-dorchester"),
    o(RegentChristchurch, UkFeed.Spektrix("theregentchristchurch", None), "regent-christchurch"),
    f(TheNewVicTisburyVillageHall, "the-new-vic-tisbury-village-hall"),
    f(TheRexCinemaWareham, "rex-wareham"),
    f(TivoliTheatreWimborne, "tivoli-wimborne-minster"),
    o(VueCinemasPoole, UkFeed.Vue("20000"), "vue-poole"),
  )
  // ── gwent
  private def group62: Seq[UkVenueSources] = Seq(
    f(BakerStreetCinemaAbergavenny, "baker-street-cinema-abergavenny"),
    o(CineworldSpyttyParkNewport, UkFeed.Cineworld("x078t-cineworld-cinema-newport-wales-spytty-park"), "cineworld-spytty-park-newport"),
    f(MarketHallCinemaBrynmawr, "market-hall-cinema-brynmawr"),
    f(MaximeCinemaBlackwood, "maxime-blackwood"),
    f(RiverfrontNewport, "riverfront-newport"),
    f(SavoyTheatreMonmouth, "savoy-monmouth"),
    o(VueCinemasCwmbran, UkFeed.Vue("10067"), "vue-cinemas-cwmbran"),
  )
  // ── fife
  private def group63: Seq[UkVenueSources] = Seq(
    f(AdamSmithTheatreKirkcaldy, "adam-smith-theatre-kirkcaldy"),
    f(KinoGlenrothes, "kino-glenrothes"),
    o(OdeonCinemaDunfermline, UkFeed.Odeon("508"), "odeon-cinema-dunfermline"),
  )
  // ── north-yorkshire
  private def group64: Seq[UkVenueSources] = Seq(
    o(CineworldYork, UkFeed.Cineworld("x12iw-cineworld-cinema-york"), "cineworld-york"),
    f(CityScreenPicturehouseYork, "city-screen-picturehouse-york"),
    o(EverymanCinemaHarrogate, UkFeed.Everyman("X0X9K"), "everyman-cinema-harrogate"),
    o(EverymanCinemaNorthallerton, UkFeed.Everyman("G01OJ"), "everyman-cinema-northallerton"),
    o(EverymanCinemaYork, UkFeed.Everyman("X06TJ"), "everyman-cinema-york"),
    f(HollywoodPlazaScarborough, "hollywood-plaza-scarborough"),
    o(OdeonCinemaHarrogate, UkFeed.Odeon("806"), "odeon-cinema-harrogate"),
    o(OdeonMiddlesbrough, UkFeed.Odeon("395"), "odeon-middlesbrough"),
    f(PavilionCinemaWhitby, "pavilion-whitby"),
    o(PocklingtonArtsCentre, UkFeed.Ticketsolve("pocklingtonartscentre"), "pocklington-arts-centre"),
    f(RegentRedcar, "regent-redcar"),
    o(RitzCinemaThirsk, UkFeed.Savoy("https://ritzcinema.co.uk/RitzCinemaThirsk.dll/"), "ritz-thirsk"),
    f(RoxyMoviesMiddlesbrough, "roxy-movies-middlesbrough"),
    o(SavoyCinemaCatterickGarrison, UkFeed.Savoy("https://savoycatterick.co.uk/SavoyCatterick.dll/"), "savoy-cinema-catterick-garrison"),
    f(StationCinemaRichmond, "station-richmond"),
    f(StephenJosephTheatreScarborough, "stephen-joseph-theatre-scarborough"),
    o(TheForumNorthallerton, UkFeed.Savoy("https://forumnorthallerton.org.uk/ForumNorthallerton.dll/"), "the-forum-northallerton"),
    o(VueCinemasYork, UkFeed.Vue("10048"), "vue-cinemas-york"),
  )
  // ── aberdeenshire
  private def group65: Seq[UkVenueSources] = Seq(
    f(ArcCinemaPeterhead, "arc-peterhead"),
    o(CineworldQueensLinkAberdeen, UkFeed.Cineworld("x079z-cineworld-cinema-aberdeen-queens-links"), "cineworld-queens-link-aberdeen"),
    o(CineworldUnionSquareAberdeen, UkFeed.Cineworld("x0fr5-cineworld-cinema-aberdeen-union-square"), "cineworld-union-square-aberdeen"),
    o(MorayPlayhouse, UkFeed.Savoy("https://caledoniancinemas.co.uk/MorayPlayhouse.dll/"), "moray-playhouse-elgin"),
    f(Number30Huntly, "number-30-huntly"),
    f(TheBarnBanchory, "the-barn-banchory"),
    f(VictoriaHallEllon, "victoria-hall-ellon"),
  )
  // ── liverpool
  private def group66: Seq[UkVenueSources] = Seq(
    o(CineworldSpeke, UkFeed.Cineworld("x11bh-cineworld-cinema-speke"), "cineworld-speke"),
    o(CineworldStHelens, UkFeed.Cineworld("x06xm-cineworld-cinema-st-helens"), "cineworld-st-helens"),
    o(EverymanCinemaLiverpool, UkFeed.Everyman("X11I7"), "everyman-cinema-liverpool"),
    o(OdeonLiverpoolONE, UkFeed.Odeon("560"), "odeon-cinema-liverpool-one"),
    o(OdeonLiverpoolSwitchIsland, UkFeed.Odeon("522"), "odeon-luxe-liverpool"),
    o(OdeonLuxeBromborough, UkFeed.Odeon("475"), "odeon-luxe-bromborough"),
    f(PicturehouseAtFACTLiverpool, "picturehouse-at-fact-liverpool"),
    o(PlazaCommunityCinemaLiverpool, UkFeed.Savoy("https://plazacinema.org.uk/PlazaCinemaLiverpool.dll/"), "plaza-crosby"),
    o(ShowcaseDeLuxLiverpool, UkFeed.Showcase("X06JU"), "showcase-de-lux-liverpool"),
    f(SouthportBijouCinema, "bijou-southport"),
    f(TheLightNewBrighton, "the-light-cinemas-new-brighton"),
    o(VueCinemasBirkenhead, UkFeed.Vue("10055"), "vue-cinemas-birkenhead"),
    o(VueCinemasSouthport, UkFeed.Vue("10004"), "vue-cinemas-southport"),
    f(WooltonPictureHouse, "woolton-picture-house"),
  )
  // ── londonderry
  private def group67: Seq[UkVenueSources] = Seq(
    f(BrunswickMoviebowlLondonderry, "brunswick-moviebowl-londonderry"),
    f(MovieHouseColeraine, "movie-house-coleraine"),
    f(MovieHouseMaghera, "movie-house-maghera"),
    o(NerveCentreLondonderry, UkFeed.Ticketsolve("nervecentre"), "nerve-centre-londonderry"),
    f(OmniplexLondonderry, "omniplex-londonderry"),
  )
  // ── bristol
  private def group68: Seq[UkVenueSources] = Seq(
    f(CubeCinemaBristol, "cube-bristol"),
    o(EverymanCinemaBristol, UkFeed.Everyman("X0X3Q"), "everyman-cinema-bristol"),
    o(OdeonCabotCircus, UkFeed.Odeon("315"), "odeon-cabot-circus"),
    f(ScottCinemasBristolWestburyPark, "orpheus-bristol"),
    o(ShowcaseBristolAvonmeads, UkFeed.Showcase("X06JH"), "showcase-cinema-bristol"),
    o(VueCinemasBristolCribbsCauseway, UkFeed.Vue("10018"), "vue-cinemas-cribbs-causeway-bristol"),
    o(VueCinemasBristolLongwellGreen, UkFeed.Vue("10019"), "vue-cinemas-longwell-green-bristol"),
    o(WatershedBristol, UkFeed.Ticketsolve("watershed"), "watershed-bristol"),
  )
  // ── oxfordshire
  private def group69: Seq[UkVenueSources] = Seq(
    f(AbbeyCinemaAbingdon, "abbey-cinema-abingdon"),
    o(CineworldDidcot, UkFeed.Cineworld("x083o-cineworld-cinema-didcot"), "cineworld-didcot"),
    o(CineworldWitney, UkFeed.Cineworld("x0fr6-cineworld-cinema-witney"), "cineworld-witney"),
    f(CornExchangeCinemaWallingford, "corn-exchange-cinema-wallingford"),
    f(CurzonCinemaOxford, "curzon-cinema-oxford"),
    f(PhoenixPicturehouseOxford, "phoenix-picturehouse-oxford"),
    f(RegalPicturehouseHenley, "regal-picturehouse-henley"),
    f(TheLightBanbury, "the-light-cinemas-banbury"),
    f(TheLivingRoomCinemaChippingNorton, "the-living-room-cinema-chipping-norton"),
    f(TheOxfordCinemaCafe, "cinema-oxford"),
    f(UltimatePicturePalaceOxford, "ultimate-picture-palace-oxford"),
    o(VueCinemasBicester, UkFeed.Vue("10087"), "vue-cinemas-bicester"),
    o(VueCinemasOxford, UkFeed.Vue("10008"), "vue-cinemas-oxford"),
  )
  // ── jersey
  private def group70: Seq[UkVenueSources] = Seq(
    o(CineworldStHelierJersey, UkFeed.Cineworld("x06xu-cineworld-cinema-jersey"), "cineworld-st-helier-jersey"),
  )
  // ── berkshire
  private def group71: Seq[UkVenueSources] = Seq(
    o(CineworldBracknell, UkFeed.Cineworld("x104v-cineworld-cinema-bracknell"), "cineworld-bracknell"),
    f(CornExchangeNewburyScreenOne, "screen-one-newbury"),
    o(EverymanCinemaWokingham, UkFeed.Everyman("X11NV"), "everyman-cinema-wokingham"),
    o(OdeonLuxeMaidenhead, UkFeed.Odeon("962"), "odeon-cinema-luxe-maidenhead"),
    f(ReadingBiscuitFactory, "reading-biscuit-factory"),
    o(ShowcaseDeLuxReading, UkFeed.Showcase("X06KD"), "showcase-cinema-de-lux-reading"),
    f(SouthHillParkArtsCentreBracknell, "south-hill-park-arts-centre-bracknell"),
    own(TheOldCourtWindsor, UkFeed.TheOldCourt),
    o(VueCinemasNewbury, UkFeed.Vue("10070"), "vue-cinemas-newbury"),
    o(VueCinemasReading, UkFeed.Vue("10020"), "vue-cinemas-reading"),
  )
  // ── surrey
  private def group72: Seq[UkVenueSources] = Seq(
    f(ChiddingfoldVillageHallCinema, "chiddingfold-village-hall-cinema"),
    o(CineworldAldershot, UkFeed.Cineworld("x0njx-cineworld-cinema-aldershot"), "cineworld-aldershot"),
    o(EverymanCinemaOxted, UkFeed.Everyman("X070Y"), "everyman-cinema-oxted"),
    o(EverymanCinemaReigate, UkFeed.Everyman("X070X"), "everyman-cinema-reigate"),
    f(HaslemereHallCinema, "haslemere-hall-cinema"),
    o(OdeonCinemaGuildford, UkFeed.Odeon("510"), "odeon-cinema-guildford"),
    f(ReelCinemasFarnham, "reel-cinemas-farnham"),
    f(TheLightRedhill, "the-light-redhill"),
    o(VueCinemasCamberley, UkFeed.Vue("10068"), "vue-cinemas-camberley"),
    o(VueCinemasFarnborough, UkFeed.Vue("10089"), "vue-cinemas-farnborough"),
  )
  // ── warwickshire
  private def group73: Seq[UkVenueSources] = Seq(
    o(CineworldRugby, UkFeed.Cineworld("x06xi-cineworld-cinema-rugby"), "cineworld-rugby"),
    o(EverymanCinemaStratfordUponAvon, UkFeed.Everyman("X0YZU"), "everyman-cinema-stratford-upon-avon"),
    o(OdeonCinemaCoventry, UkFeed.Odeon("040"), "odeon-cinema-coventry"),
    o(OdeonLuxeNuneaton, UkFeed.Odeon("244"), "odeon-luxe-nuneaton"),
    f(RoyalSpaCentre, "royal-spa-centre-cinema-leamington"),
    o(ShowcaseDeLuxCoventry, UkFeed.Showcase("X06JI"), "cinema-de-lux-coventry"),
    o(VueCinemasLeamingtonSpa, UkFeed.Vue("10079"), "vue-cinemas-leamington-spa"),
    f(WarwickArtsCentreCoventry, "warwick-arts-centre-coventry"),
  )
  // ── glamorgan
  private def group74: Seq[UkVenueSources] = Seq(
    f(ColiseumTheatreAberdare, "coliseum-theatre-aberdare"),
    f(GwynHallNeath, "gwyn-hall-neath"),
    f(MinersWelfareAndCommunityHallYstradgynlais, "miners-welfare-and-community-hall-ystradgynlais"),
    o(OdeonCinemaBridgend, UkFeed.Odeon("546"), "odeon-cinema-bridgend"),
    o(OdeonCinemaSwansea, UkFeed.Odeon("920"), "odeon-cinema-swansea"),
    o(PontardaweArtsCentre, UkFeed.Ticketsolve("pontardaweartscentre"), "pontardawe-arts-centre"),
    f(ReelCinemaPortTalbot, "reel-port-talbot"),
    f(TaliesinArtsCentreSwansea, "taliesin-arts-centre-swansea"),
    o(VueCinemasMerthyrTydfil, UkFeed.Vue("10066"), "vue-cinemas-merthyr-tydfil"),
    o(VueCinemasSwansea, UkFeed.Vue("10058"), "vue-cinemas-swansea"),
  )
  // ── dunbartonshire-argyll-bute
  private def group75: Seq[UkVenueSources] = Seq(
    f(CampbeltownPictureHouse, "campbeltown-picture-house"),
    f(DiscoveryCentreCinemaRothesay, "discovery-theatre-rothesay"),
    f(OmniplexClydebankFormerlyEmpire, "empire-cinema-clydebank"),
    f(StudioCinemaDunoon, "studio-dunoon"),
  )

  val venues: Seq[UkVenueSources] =
    group0 ++ group1 ++ group2 ++ group3 ++ group4 ++ group5 ++ group6 ++ group7 ++ group8 ++ group9 ++ group10 ++ group11 ++
    group12 ++ group13 ++ group14 ++ group15 ++ group16 ++ group17 ++ group18 ++ group19 ++ group20 ++ group21 ++ group22 ++ group23 ++
    group24 ++ group25 ++ group26 ++ group27 ++ group28 ++ group29 ++ group30 ++ group31 ++ group32 ++ group33 ++ group34 ++ group35 ++
    group36 ++ group37 ++ group38 ++ group39 ++ group40 ++ group41 ++ group42 ++ group43 ++ group44 ++ group45 ++ group46 ++ group47 ++
    group48 ++ group49 ++ group50 ++ group51 ++ group52 ++ group53 ++ group54 ++ group55 ++ group56 ++ group57 ++ group58 ++ group59 ++
    group60 ++ group61 ++ group62 ++ group63 ++ group64 ++ group65 ++ group66 ++ group67 ++ group68 ++ group69 ++ group70 ++ group71 ++
    group72 ++ group73 ++ group74 ++ group75
}
