package models

// ── Filmweb catchment cinemas (nearby towns Filmweb groups under each city) ──
// Each is served by FilmwebShowtimesClient via its Filmweb internal cinema id
// (verified non-empty seances 2026-06). Wired in its town's registry (`services.cinemas.pl.venues`), Filmweb catchment tier.
// wroclaw
case object KinoAstra extends Cinema("Kino Astra", "Astra")   // Oborniki Śląskie — kulturalne-oborniki.bilety24.pl
case object KinoDyskusyjnyKlubFilmowyPolitechnika extends Cinema("Dyskusyjny Klub Filmowy Politechnika", "Wrocław")   // Wrocław — filmweb 1645
// warszawa
case object KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha extends Cinema("Kino Praha", "Praha")   // Warszawa — filmweb 2180
// lodz
case object KinoSpojnia extends Cinema("Kino Spójnia", "Spójnia")   // Aleksandrów Łódzki — filmweb 2403
case object KinoStaryMlyn extends Cinema("Kino Stary Młyn", "Stary Młyn")   // Zgierz — filmweb 2443
// katowice
case object CinemaCity extends Cinema("Cinema City Ruda Śląska", "Cinema City")   // Ruda Śląska — filmweb 388
case object KinoPatria extends Cinema("Patria", "Patria")   // Ruda Śląska — filmweb 352
// szczecin
case object KinoKawiarnia extends Cinema("Kino Kawiarnia", "Kawiarnia")   // Goleniów — filmweb 117
case object KinoPDK extends Cinema("Kino PDK", "PDK")   // Pyrzyce — filmweb 2363
case object KinoSCK extends Cinema("Kino SCK", "SCK")   // Stargard — filmweb 1941
// bialystok
case object KinoSokolSokolka extends Cinema("Kino Sokół Sokółka", "Sokół")   // Sokółka — filmweb 1659
// trojmiasto
case object KinoNaSzekspirowskim extends Cinema("Kino na Szekspirowskim", "Gdańsk")   // Gdańsk — filmweb 2100
case object MultikinoRumia extends Cinema("Multikino Rumia", "Rumia")   // Rumia — filmweb 1464
// bydgoszcz
case object KinoKinomax extends Cinema("Kinomax", "Kinomax")   // Inowrocław — filmweb 1124
case object KinoRondo extends Cinema("Kinoteatr Rondo", "Rondo")   // Chełmno — filmweb 3121
// lublin
case object KinoLewart extends Cinema("Kino Lewart", "Lewart")   // Lubartów — filmweb 1697
case object KinoMetalowiec extends Cinema("Metalowiec", "Metalowiec")   // Kraśnik — filmweb 285
// czestochowa
case object KinoDKFRumcajs extends Cinema("DKF Rumcajs", "DKF Rumcajs")   // Częstochowa — rumcajs.czest.pl
case object KinoKarolinka extends Cinema("Kino Karolinka", "Karolinka")   // Lubliniec — filmweb 1719
case object KinoMDK extends Cinema("Kino MDK", "MDK")   // Radomsko — filmweb 1732
case object KinoMOKCentrum extends Cinema("Kino MOK Centrum", "MOK Centrum")   // Zawiercie — filmweb 1525
case object KinoZacisze extends Cinema("Zacisze", "Zacisze")   // Piekary Śląskie — filmweb 2350
// radom
case object HeliosStarachowice extends Cinema("Helios Starachowice", "Starachowice")   // Starachowice — filmweb 1845
case object KinoCentrumSkarzyskoKamienna extends Cinema("Kino Centrum Skarżysko-Kamienna", "Centrum")   // Skarżysko-Kamienna — filmweb 1705
case object KinoGornik extends Cinema("Kino Górnik", "Górnik")   // Szydłowiec — filmweb 3064
case object KinoKozienickiDomKultury extends Cinema("Kozienicki Dom Kultury", "Kozienice")   // Kozienice — filmweb 1913
case object KinoKuznica extends Cinema("Kino Kuźnica", "Kuźnica")   // Suchedniów — filmweb 1713
case object KinoSwitZwolen extends Cinema("Świt Zwoleń", "Świt")   // Zwoleń — filmweb 2342
// torun
case object KinoMiejskieCentrumKultury extends Cinema("Miejskie Centrum Kultury", "Aleksandrów")   // Aleksandrów Kujawski — filmweb 3119
case object KinoZdroj extends Cinema("Kino Zdrój", "Zdrój")   // Ciechocinek — filmweb 1771
// kielce
case object KinoCK extends Cinema("Kino CK", "CK")   // Jędrzejów — filmweb 2351
case object KinoKoneckieCentrumKultury extends Cinema("Koneckie Centrum Kultury", "Końskie")   // Końskie — filmweb 3128
// rzeszow
case object HeliosKrosno extends Cinema("Helios Krosno", "Krosno")   // Krosno — filmweb 2014
case object KinoArtKino extends Cinema("artKino", "artKino")   // Krosno — filmweb 1122
case object KinoJednosc extends Cinema("Kino Jedność", "Jedność")   // Sędziszów Małopolski — filmweb 2335
case object KinoMCK extends Cinema("Kino MCK", "MCK")   // Leżajsk — filmweb 2344
case object KinoSniezka extends Cinema("Kino Śnieżka", "Śnieżka")   // Dębica — filmweb 1500
case object KinoSokolBrzozow extends Cinema("Kino Sokół Brzozów", "Sokół")   // Brzozów — filmweb 1477
case object KinoWarszawa extends Cinema("Kino Warszawa", "Warszawa")   // Przeworsk — filmweb 2346
// gliwice
case object KinoScenaKultura extends Cinema("Kino Scena Kultura", "Scena Kultura")   // Knurów — kinoscenakultura.pl
// olsztyn
case object KinoCinemaLumiere extends Cinema("Cinema Lumiere", "Cinema Lumiere")   // Szczytno — filmweb 2357
case object KinoIgnacy extends Cinema("Kino Ignacy", "Ignacy")   // Lidzbark Warmiński — filmweb 2354
case object KinoNarie extends Cinema("Kino Narie", "Narie")   // Morąg — filmweb 2355
// bielsko-biala
case object KinoJanosik extends Cinema("Janosik", "Janosik")   // Żywiec — filmweb 157
case object KinoPckulKino extends Cinema("Kino PCKul", "Pszczyna")   // Pszczyna — filmweb 3248
case object KinoSwitCzechowiceDziedzice extends Cinema("Świt", "Świt")   // Czechowice-Dziedzice — filmweb 135
case object KinoTeatrElektryczny extends Cinema("Teatr Elektryczny", "Skoczów")   // Skoczów — filmweb 3141
case object KinoWislaBrzeszcze extends Cinema("Kino Wisła Brzeszcze", "Wisła")   // Brzeszcze — filmweb 1490
case object MultikinoCzechowiceDziedzice extends Cinema("Multikino Czechowice-Dziedzice", "Multikino")   // Czechowice-Dziedzice — filmweb 1776
// opole
case object HeliosKedzierzynKozle extends Cinema("Helios Kędzierzyn-Koźle", "Helios")   // Kędzierzyn-Koźle — filmweb 1703
case object KinoBajkaKluczbork extends Cinema("Kino Bajka Kluczbork", "Bajka")   // Kluczbork — filmweb 2320
case object KinoChemik extends Cinema("Chemik", "Chemik")   // Kędzierzyn-Koźle — filmweb 619
case object KinoDiana extends Cinema("Kino Diana", "Diana")   // Prudnik — filmweb 2343
case object KinoKrapkowice extends Cinema("Kino Krapkowice", "Krapkowice")   // Krapkowice — filmweb 1681
case object KinoStudio extends Cinema("Studio", "Studio")   // Opole — filmweb 431
case object KinoTwierdza extends Cinema("Kino Twierdza", "Twierdza")   // Kędzierzyn-Koźle — filmweb 1672
// rybnik
case object HeliosZory extends Cinema("Helios Żory", "Helios")   // Żory — filmweb 2326
case object KinoBaltyk extends Cinema("Bałtyk", "Bałtyk")   // Racibórz — filmweb 168
case object KinoCentrum extends Cinema("Centrum", "Centrum")   // Jastrzębie Zdrój — filmweb 514
case object KinoNaStarowce extends Cinema("Na Starówce", "Na Starówce")   // Żory — filmweb 588
case object KinoPegaz extends Cinema("Pegaz", "Pegaz")   // Wodzisław Śląski — filmweb 3148
case object KinoTeatrZiemiRybnickiej extends Cinema("Teatr Ziemi Rybnickiej", "Rybnik")   // Rybnik — filmweb 3140
// elblag
case object HeliosTczew extends Cinema("Helios Tczew", "Helios")   // Tczew — filmweb 1673
case object KinoBaszta extends Cinema("Baszta", "Baszta")   // Braniewo — filmweb 2352
case object KinoPowisle extends Cinema("Kino Powiśle", "Powiśle")   // Sztum — filmweb 1798
case object KinoZulawskiOsrodekKultury extends Cinema("Żuławski Ośrodek Kultury", "Nowy")   // Nowy Dwór Gdański — filmweb 3131
// koszalin
case object KinoBajkaDarlowo extends Cinema("Bajka", "Bajka")   // Darłowo — filmweb 255
case object KinoCentrumBialogard extends Cinema("Kino Centrum", "Centrum")   // Białogard — filmweb 1675
case object KinoDK extends Cinema("Kino DK", "DK")   // Sławno — filmweb 2365
case object KinoGOK extends Cinema("Kino GOK", "GOK")   // Tychowo — filmweb 2414
case object KinoGoplana extends Cinema("Kino Goplana", "Goplana")   // Połczyn — filmweb 1864
case object KinoWybrzeze extends Cinema("Wybrzeże", "Wybrzeże")   // Kołobrzeg — filmweb 276
// kalisz
case object HeliosOstrowWlkp extends Cinema("Helios Ostrów Wlkp.", "Ostrów Wlkp.")   // Ostrów Wlkp. — filmweb 2372
case object KinoCentrum3D extends Cinema("Centrum 3D", "Centrum 3D")   // Kalisz — filmweb 1513
case object KinoEcho extends Cinema("Kino Echo", "Echo")   // Jarocin — filmweb 1484
case object KinoPiastOstrzeszow extends Cinema("Piast", "Piast")   // Ostrzeszów — filmweb 2359
case object KinoPrzedwiosnieKrotoszyn extends Cinema("Przedwiośnie", "Przedwiośnie")   // Krotoszyn — filmweb 1121
// zielona-gora
case object KinoEuropa extends Cinema("Europa", "Europa")   // Nowa Sól — filmweb 1955
case object KinoMaxKino extends Cinema("Max Kino", "Max Kino")   // Świebodzin — filmweb 2171
case object KinoPionierZary extends Cinema("Pionier", "Pionier")   // Żary — filmweb 1430
case object KinoSDKSwiebodzin extends Cinema("Kino ŚDK", "ŚDK")   // Świebodzin — filmweb 2331
// tychy
case object KinoNaszeKino extends Cinema("Nasze Kino", "Nasze Kino")   // Oświęcim — filmweb 1480
case object KinoPlanetCinema extends Cinema("Planet Cinema", "Planet Cinema")   // Oświęcim — MSI
// walbrzych
case object KinoMOKNowaRuda extends Cinema("Kino MOK Nowa Ruda", "MOK")   // Nowa Ruda — filmweb 1493
case object KinoMOKiS extends Cinema("Kino MOKiS", "MOKiS")   // Bielawa — filmweb 197
case object KinoSleza extends Cinema("Kino Ślęża", "Ślęża")   // Sobótka — filmweb 2410
case object KinoZbyszek extends Cinema("Kino Zbyszek", "Zbyszek")   // Dzierżoniów — filmweb 1530
case object MultikinoKlodzko extends Cinema("Multikino Kłodzko", "Multikino")   // Kłodzko — filmweb 2990
case object MultikinoSwidnica extends Cinema("Multikino Świdnica", "Świdnica")   // Świdnica — filmweb 2993
// tarnow
case object KinoFarys extends Cinema("Farys", "Farys")   // Biecz — filmweb 2315
case object KinoGCK extends Cinema("Kino GCK", "GCK")   // Solec-Zdrój — filmweb 2411
case object KinoKolory extends Cinema("Kino Kolory", "Kolory")   // Gorlice — filmweb 2404
case object KinoPlaneta extends Cinema("Kino Planeta", "Planeta")   // Brzesko — filmweb 1481
case object KinoPromien extends Cinema("Kino Promień", "Promień")   // Tuchów — kinotuchow.pl
case object KinoRegis extends Cinema("Regis", "Regis")   // Bochnia — filmweb 1294
case object KinoSokolDabrowaTarnowska extends Cinema("Kino Sokół Dąbrowa Tarnowska", "Sokół")   // Dąbrowa Tarnowska — filmweb 1488
// wloclawek
case object KinoJutrzenka extends Cinema("Jutrzenka", "Jutrzenka")   // Sierpc — filmweb 2341
case object KinoNawojka extends Cinema("Kino Nawojka", "Nawojka")   // Lipno — filmweb 3130
case object KinoNoweKinoWarszawa extends Cinema("Nowe Kino Warszawa", "Gostynin")   // Gostynin — filmweb 3246
// legnica
case object HeliosLubin extends Cinema("Helios Lubin", "Lubin")   // Lubin — filmweb 1420
case object KinoAurum extends Cinema("Kino Aurum", "Aurum")   // Złotoryja — filmweb 1718
case object KinoCyfroweKino extends Cinema("Cyfrowe Kino", "Cyfrowe Kino")   // Środa Śląska — filmweb 2313
case object KinoForumBoleslawiec extends Cinema("Forum", "Forum")   // Bolesławiec — filmweb 373
case object KinoMuzaLubin extends Cinema("Muza", "Muza")   // Lubin — filmweb 288
case object KinoPCA extends Cinema("Kino PCA", "PCA")   // Polkowice — filmweb 1139
// plock
case object KinoZaRogiem extends Cinema("Kino za Rogiem", "za Rogiem")   // Płock — filmweb 1949
case object KinoKDK extends Cinema("Kino KDK", "KDK")   // Kutno — filmweb 1134
case object KinoKalejdoskop extends Cinema("Kino Kalejdoskop", "Kalejdoskop")   // Płońsk — filmweb 1702
case object KinoODEON extends Cinema("Kino ODEON", "ODEON")   // Sochaczew — filmweb 2128
// nowy-sacz
case object KinoJaworzyna extends Cinema("Jaworzyna", "Jaworzyna")   // Krynica Zdrój — filmweb 561
case object KinoKlaps extends Cinema("Klaps", "Klaps")   // Limanowa — filmweb 1137
// slupsk
case object KinoFregata extends Cinema("Fregata", "Fregata")   // Lębork — filmweb 2123
// jelenia-gora
case object KinoWawel extends Cinema("Kino Wawel", "Wawel")   // Lubań — filmweb 1721
// przemysl
case object KinoCentrum3DPrzemysl extends Cinema("Centrum 3D Przemyśl", "Centrum 3D")   // Przemyśl — ck.przemysl.pl (was filmweb 1786, went empty)
case object KinoIkar extends Cinema("Kino Ikar", "Ikar")   // Jarosław — filmweb 1707
case object KinoNaBiegunach extends Cinema("Kino Na Biegunach", "Na Biegunach")   // Jarosław — filmweb 2172
case object KinoSDK extends Cinema("Kino SDK", "SDK")   // Sanok — bilety.sdksanok.pl
