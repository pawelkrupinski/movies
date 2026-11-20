package models

// ── Nearby-town cinemas and new regional hubs (2026-09 sweep) ─────────────────
// Every venue Filmweb, bilety24 or a chain lists as currently screening in a town
// no page covered yet. A town within ~45 km of an existing city joins that city;
// the rest are grouped into regions around their largest town, every venue about
// an hour's drive from it. Wired in its town's registry (`services.cinemas.pl.venues`), nearby-towns tier; the page
// that lists each is generated into `PolishPages`.
// szczecin
case object KinoMOKPolice extends Cinema("Kino MOK Police", "MOK Police")   // Police — biletyna Miejski-Osrodek-Kultury
case object KinoEva extends Cinema("Kino Eva", "Eva")   // Międzyzdroje — own site kino-eva.com.pl
// bielsko-biala
case object KinoCentrumWadowice extends Cinema("Kino Centrum Wadowice", "Centrum Wadowice")   // Wadowice — biletyna Wadowickie-Centrum-Kultury
case object CinemaCityCieszyn extends Cinema("Cinema City Cieszyn", "Cieszyn")   // Cieszyn — Cinema City API
case object KinoCKiF extends Cinema("Kino CKiF", "CKiF")   // Sucha Beskidzka — systembiletowy bilety.ckifsucha.pl
// warszawa
case object KinoGrodziskieCentrumKultury extends Cinema("Kino Grodziskiego Centrum Kultury", "GCK Grodzisk")   // Grodzisk Mazowiecki — bilety24
case object KinoBajkaBlonie extends Cinema("Kino Bajka Błonie", "Bajka")   // Błonie — kino.blonie.pl
case object KinoGrojeckiOsrodekKultury extends Cinema("Grójecki Ośrodek Kultury", "Grójec")   // Grójec — biletyna
case object KinoUciecha extends Cinema("Kino Uciecha", "Uciecha")   // Góra Kalwaria — biletyna Kino-Uciecha
case object HeliosWolomin extends Cinema("Helios Wołomin", "Helios Wołomin")   // Wołomin — Helios REST
case object KinoKulturaWolomin extends Cinema("Kino Kultura Wołomin", "Kultura Wołomin")   // Wołomin — biletyna Kino-Kultura
case object KinoTeatrKurtyna extends Cinema("Kino-Teatr Kurtyna", "Kurtyna")   // Sulejówek — filmweb 2079
case object MultikinoPruszkow extends Cinema("Multikino Pruszków", "Multikino Pruszków")   // Pruszków — Multikino API
case object KinoCKiSPruszkow extends Cinema("Kino CKiS Pruszków", "CKiS Pruszków")   // Pruszków — biletyna CDK Sala widowiskowa
case object HeliosLegionowo extends Cinema("Helios Legionowo", "Helios Legionowo")   // Legionowo — Helios REST
case object KinoOaza extends Cinema("Kino Oaza", "Oaza")   // Otwock — biletyna Kino-Oaza-Otwockie-Centrum-Kultury
// lodz
case object KinoTomi extends Cinema("Kino Tomi", "Tomi")   // Pabianice — own site kinotomi.pl
case object HeliosPabianice extends Cinema("Helios Pabianice", "Helios Pabianice")   // Pabianice — Helios REST
case object KinoGornikLeczyca extends Cinema("Kino Górnik Łęczyca", "Górnik")   // Łęczyca — MSI kinogornik.eurobilet.pl
// dabrowa-gornicza
case object KinoZbyszekOlkusz extends Cinema("Kino Zbyszek Olkusz", "Zbyszek")   // Olkusz — biletyna Kino-Zbyszek
case object MultikinoJaworzno extends Cinema("Multikino Jaworzno", "Multikino")   // Jaworzno — Multikino API
// krakow
case object KinoMuzaMyslenice extends Cinema("Kino Muza Myślenice", "Muza Myślenice")   // Myślenice — systembiletowy bilety.kino.myslenice.pl
case object KinoRadosc extends Cinema("Kino Radość", "Radość")   // Wolbrom — ekobilet dk-wolbrom
case object KinoGryf extends Cinema("Kino Gryf", "Gryf")   // Miechów — systembiletowy bilety.ckis.miechow.eu
case object KinoWazka extends Cinema("Kino Ważka", "Ważka")   // Niepołomice — bilety24
case object KinoWielickaMediateka extends Cinema("Kino Wielicka Mediateka", "Wielicka Mediateka")   // Wieliczka — ekobilet kino-wielicka-mediateka
case object KinoLubon extends Cinema("Kino Luboń 2.0", "Luboń")   // Mszana Dolna — bilety24
// poznan
case object KinoBasztaSroda extends Cinema("Kino Baszta Środa", "Baszta Środa")   // Środa Wielkopolska — bilety24
case object KinoHalszka extends Cinema("Kino Halszka", "Halszka")   // Szamotuły — bilety24
case object KinoteatrSlonko extends Cinema("Kinoteatr Słonko", "Słonko")   // Śrem — bilety24
case object KinoWielkopolanin extends Cinema("Kino Wielkopolanin", "Wielkopolanin")   // Buk — bilety24
case object KinoTrojka extends Cinema("Kino Trójka", "Trójka")   // Września — bilety24
case object KinoOpalenica extends Cinema("Kino Opalenica", "Opalenica")   // Opalenica — bilety24
case object KinoteatrGwiazda extends Cinema("Kinoteatr Gwiazda", "Gwiazda")   // Wronki — bilety24
// opole
case object KinoCentrumBrzeg extends Cinema("Kino Centrum Brzeg", "Centrum Brzeg")   // Brzeg — bilety24
case object KinoCinemaN extends Cinema("Cinema N 3D", "Cinema N")   // Nysa — systembiletowy bilety.kino.nysa.pl
// wroclaw
case object KinoOdraBrzegDolny extends Cinema("Kino Odra Brzeg Dolny", "Odra Brzeg Dolny")   // Brzeg Dolny — bilety24
case object KinoOdraOlawa extends Cinema("Kino Odra Oława", "Odra Oława")   // Oława — bilety24
case object GoKinoOlawa extends Cinema("GO KINO Oława", "GO KINO")   // Oława — MSI olawa.gokino.pl
case object KinoGrazyna extends Cinema("Kino Grażyna", "Grażyna")   // Strzelin — MSI bilety.kinostrzelin.pl
case object KinoPoloniaTrzebnica extends Cinema("Kino Polonia Trzebnica", "Polonia")   // Trzebnica — bilety24
case object KinoMOKiSOlesnica extends Cinema("Kino MOKiS Oleśnica", "MOKiS Oleśnica")   // Oleśnica — bilety24
// kielce
case object KinoZdrojBusko extends Cinema("Kino Zdrój Busko", "Zdrój")   // Busko-Zdrój — biletyna BSCK
case object KinoMuzaWloszczowa extends Cinema("Kino Muza Włoszczowa", "Muza Włoszczowa")   // Włoszczowa — biletyna Dom-Kultury-we-Wloszczowie
// trojmiasto
case object KinoWCK extends Cinema("Wejherowskie Centrum Kultury", "WCK Wejherowo")   // Wejherowo — iKsoris bilety.wck.org.pl
case object KinoZaRogiemChmielno extends Cinema("Kino za Rogiem Chmielno", "Chmielno")   // Chmielno — biletyna Kino-za-Rogiem-w-Chmielnie
case object KinoZeglarz extends Cinema("Kino Żeglarz", "Żeglarz")   // Jastarnia — bilety24
case object KinoKCK extends Cinema("Kino Kartuskie Centrum Kultury", "KCK Kartuzy")   // Kartuzy — bilety24
case object KinoCKiSPruszczGdanski extends Cinema("Kino CKiS Pruszcz Gdański", "Pruszcz Gdański")   // Pruszcz Gdański — bilety24
// bydgoszcz
case object KinoRelaks extends Cinema("Kino Relaks", "Relaks")   // Nakło nad Notecią — biletyna Nakielski-Osrodek-Kultury
// tarnow
case object KinoMCKRadlow extends Cinema("Kino MCK Radłów", "MCK Radłów")   // Radłów — filmweb 2408
case object KinoCentrumLisiaGora extends Cinema("Kino Centrum Lisia Góra", "Lisia Góra")   // Lisia Góra — bilety24
case object KinoJDK extends Cinema("Kino JDK", "JDK Jasło")   // Jasło — systembiletowy jdk.systembiletowy.pl
case object MultikinoMielec extends Cinema("Multikino Mielec", "Multikino Mielec")   // Mielec — Multikino API
// gorzow-wielkopolski
case object KinoPanorama extends Cinema("Kino Panorama", "Panorama")   // Barlinek — bilety24
// nowy-sacz
case object KinoMCKGrybow extends Cinema("Kino MCKCziE Grybów", "Grybów")   // Grybów — bilety24
// lublin
case object KinoCzyn extends Cinema("Kino-Teatr Czyn", "Czyn")   // Poniatowa — bilety24
// torun
case object HeliosGrudziadz extends Cinema("Helios Grudziądz", "Grudziądz")   // Grudziądz — Helios REST
// wloclawek
case object KinoBaltykRypin extends Cinema("Kino Bałtyk Rypin", "Bałtyk Rypin")   // Rypin — bilety24
// slupsk
case object KinoLeba extends Cinema("Kino MBP Łeba", "Łeba")   // Łeba — bilety24
// legnica
case object MultikinoGlogow extends Cinema("Multikino Głogów", "Multikino Głogów")   // Głogów — Multikino API
// piotrkow-trybunalski
case object HeliosPiotrkow extends Cinema("Helios Piotrków Trybunalski", "Helios Piotrków")   // Piotrków Trybunalski — Helios REST
case object HeliosBelchatow extends Cinema("Helios Bełchatów", "Helios Bełchatów")   // Bełchatów — Helios REST
case object KinoKulturaBelchatow extends Cinema("Kino Kultura Bełchatów", "Kultura")   // Bełchatów — iKsoris bilety.mckbelchatow.pl
case object HeliosTomaszow extends Cinema("Helios Tomaszów Mazowiecki", "Helios Tomaszów")   // Tomaszów Mazowiecki — Helios REST
// siedlce
case object HeliosSiedlce extends Cinema("Helios Siedlce", "Helios")   // Siedlce — Helios REST
case object NoveKinoSiedlce extends Cinema("Nove Kino Siedlce", "Nove Kino")   // Siedlce — NoveKino siedlce
case object KinoSokolSokolowPodlaski extends Cinema("Kino Sokół Sokołów Podlaski", "Sokół")   // Sokołów Podlaski — filmweb 2073
case object KinoSlawa extends Cinema("Kino Sława", "Sława")   // Międzyrzec Podlaski — biletyna Kino-Slawa
case object KinoKongres extends Cinema("Kino Kongres", "Kongres")   // Węgrów — biletyna Wegrowski-Osrodek-Kultury
case object KinoLukow extends Cinema("Kino Łuków", "Łuków")   // Łuków — own site kino.lukow.pl
case object CinemaCityBialaPodlaska extends Cinema("Cinema City Biała Podlaska", "Cinema City")   // Biała Podlaska — Cinema City API
case object KinoWilga extends Cinema("Kino Wilga", "Wilga")   // Garwolin — systembiletowy grl.systembiletowy.pl
// pila
case object HeliosPila extends Cinema("Helios Piła", "Helios")   // Piła — Helios REST
case object KinoWCKWalcz extends Cinema("Kino Wałeckie Centrum Kultury", "Wałcz")   // Wałcz — bilety24
case object KinoOsiedlowe extends Cinema("Kino Osiedlowe", "Osiedlowe")   // Trzcianka — bilety24
case object KinoRodlo extends Cinema("Kino Rodło", "Rodło")   // Złotów — biletyna Zlotowskie-Centrum-Aktywnosci-Spolecznej
case object KinoMewaBudzyn extends Cinema("Kino Mewa Budzyń", "Mewa")   // Budzyń — bilety24
case object KinoNotec extends Cinema("Kino Noteć", "Noteć")   // Chodzież — bilety24
case object KinoZaRogiemWielen extends Cinema("Kino za Rogiem Wieleń", "Wieleń")   // Wieleń — bilety24
case object KinoMDKWagrowiec extends Cinema("Kino MDK Wągrowiec", "MDK Wągrowiec")   // Wągrowiec — bilety24
case object KinoSwiatowidCzarnkow extends Cinema("Kino Światowid Czarnków", "Światowid")   // Czarnków — bilety24
// ostrowiec-swietokrzyski
case object KinoEtiuda extends Cinema("Kino Etiuda", "Etiuda")   // Ostrowiec Świętokrzyski — bilety24
case object KinoOOK extends Cinema("Opatowski Ośrodek Kultury", "Opatów")   // Opatów — biletyna Opatowski-Osrodek-Kultury
case object KinoStarowka extends Cinema("Kino Starówka", "Starówka")   // Sandomierz — ekobilet kinostarowka
// gniezno
case object HeliosGniezno extends Cinema("Helios Gniezno", "Helios")   // Gniezno — Helios REST
case object KinoWawrzyn extends Cinema("Kino Wawrzyn", "Wawrzyn")   // Mogilno — bilety24
case object KinoMiescisko extends Cinema("Kino Mieścisko", "Mieścisko")   // Mieścisko — bilety24
// suwalki
case object KinoCinemaLumiereSuwalki extends Cinema("Cinema Lumiere Suwałki", "Cinema Lumiere")   // Suwałki — MSI suwalki.cinema-lumiere.pl
case object KinoIskra extends Cinema("Kino Iskra", "Iskra")   // Augustów — own site kino-iskra.pl
case object KinoKulturaGoldap extends Cinema("Kino Kultura Gołdap", "Gołdap")   // Gołdap — bilety24
case object KinoMazur extends Cinema("Kino Mazur", "Mazur")   // Olecko — ekobilet mazury-olecko
case object KinoECK extends Cinema("Kino ECK", "ECK")   // Ełk — biletyna Elckie-Centrum-Kultury
case object KinoPlanetCinemaElk extends Cinema("Planet Cinema Ełk", "Planet Cinema")   // Ełk — MSI
// stalowa-wola
case object HeliosStalowaWola extends Cinema("Helios Stalowa Wola", "Helios")   // Stalowa Wola — Helios REST
case object KinoWrzos extends Cinema("Kino Wrzos", "Wrzos")   // Stalowa Wola — bilety24
case object KinoSokolNisko extends Cinema("Kino Sokół Nisko", "Sokół")   // Nisko — own site nck.nisko.pl
case object KinoMetalowiecNowaDeba extends Cinema("Kino Metalowiec Nowa Dęba", "Metalowiec")   // Nowa Dęba — own site soknowadeba.pl
case object KinoBCK extends Cinema("Kino BCK", "BCK")   // Biłgoraj — iKsoris bilet.bck.lbl.pl
// zamosc
case object KinoStylowy extends Cinema("CKF Stylowy", "Stylowy")   // Zamość — own site stylowy.net
case object KinoZorzaChelm extends Cinema("Kino Zorza Chełm", "Zorza")   // Chełm — bilety24
case object KinoMorskieOko extends Cinema("Kino Morskie Oko", "Morskie Oko")   // Krasnystaw — biletyna Kino-Morskie-Oko
case object KinoPlon extends Cinema("Kino Plon", "Plon")   // Hrubieszów — iKsoris kinoplon.pl
// leszno
case object MultikinoLeszno extends Cinema("Multikino Leszno", "Multikino")   // Leszno — Multikino API
case object KinoTeatrMiejskiLeszno extends Cinema("Kino Teatru Miejskiego w Lesznie", "Teatr Miejski")   // Leszno — bilety24
case object KinoPromienRawicz extends Cinema("Kino Promień Rawicz", "Promień")   // Rawicz — biletyna Kino-Promien-w-Rawiczu
case object KinoPodKopula extends Cinema("Kino Pod Kopułą", "Pod Kopułą")   // Gostyń — bilety24
case object KinoKrobia extends Cinema("Kino Krobia", "Krobia")   // Krobia — bilety24
// lomza
case object HeliosLomza extends Cinema("Helios Łomża", "Helios")   // Łomża — Helios REST
case object KinoJantar extends Cinema("Kino Jantar", "Jantar")   // Ostrołęka — bilety24
case object KinoCKZambrow extends Cinema("Kino Centrum Kultury Zambrów", "Zambrów")   // Zambrów — own site kino.mokzambrow.pl
case object KinoOstrovia extends Cinema("Kino Ostrovia", "Ostrovia")   // Ostrów Mazowiecka — MSI bilety.kinoostrovia.pl
case object KinoWars extends Cinema("Kino Wars", "Wars")   // Wysokie Mazowieckie — own site kino.wysokiemazowieckie.pl
case object KinoDomKulturyGrajewo extends Cinema("Kino Dom Kultury Grajewo", "Grajewo")   // Grajewo — biletyna Grajewskie Centrum Kultury
// pulawy
case object KinoSybilla extends Cinema("Kino Sybilla", "Sybilla")   // Puławy — NoveKino sybilla
case object KinoRenesans extends Cinema("Kino Renesans", "Renesans")   // Ryki — ekobilet kino-renesans
// skierniewice
case object KinoPolonez extends Cinema("Kino Polonez", "Polonez")   // Skierniewice — biletyna Kinoteatr-Polonez
case object KinoCKiSSkierniewice extends Cinema("Kino CKiS Skierniewice", "CKiS")   // Skierniewice — biletyna CKiS Sala koncertowa
case object KinoLen extends Cinema("Kino Len", "Len")   // Żyrardów — biletyna Kino-Len
case object KinoRomaRawa extends Cinema("Kino Roma Rawa Mazowiecka", "Roma")   // Rawa Mazowiecka — own site mdkrawa.pl
case object KinoFenix extends Cinema("Kino Fenix", "Fenix")   // Łowicz — systembiletowy lok.systembiletowy.pl
// starogard-gdanski
case object CinemaCityStarogard extends Cinema("Cinema City Starogard Gdański", "Cinema City")   // Starogard Gdański — Cinema City API
case object KinoRemus extends Cinema("Kino Remus", "Remus")   // Kościerzyna — own site remus.hostingasp.pl
// ciechanow
case object KinoLydynia extends Cinema("Kino Łydynia", "Łydynia")   // Ciechanów — bilety24
case object KinoKosmosMlawa extends Cinema("Kino Kosmos Mława", "Kosmos")   // Mława — biletyna Miejski-Dom-Kultury-kino-Kosmos
case object KinoSwiatowidPrzasnysz extends Cinema("Kino Światowid Przasnysz", "Światowid")   // Przasnysz — bilety24
case object KinoMDKMakow extends Cinema("Kino MDK Maków Mazowiecki", "MDK Maków")   // Maków Mazowiecki — bilety24
case object KinoApolloDzialdowo extends Cinema("Kino Apollo Działdowo", "Apollo")   // Działdowo — biletyna Miejski-Dom-Kultury
// wielun
case object KinoSyrena extends Cinema("Kino-Teatr Syrena", "Syrena")   // Wieluń — filmweb 2034
case object KinoTeatrSieradz extends Cinema("Kino Teatr Sieradz", "Sieradz")   // Sieradz — MSI bilety.scksieradz.pl
case object KinoSlonce extends Cinema("Kino Słońce", "Słońce")   // Wieruszów — MSI kinoslonce.pl
case object KinoSokolniaKepno extends Cinema("Kino Sokolnia Kępno", "Sokolnia")   // Kępno — iKsoris kinosokolnia.org
// chojnice
case object KinoChDK extends Cinema("Kino ChDK", "ChDK")   // Chojnice — MSI bilety.ckchojnice.pl
case object KinoUciechaCzluchow extends Cinema("Kino Uciecha Człuchów", "Uciecha")   // Człuchów — biletyna Kino-Uciecha
case object KinoZaRogiemBrusy extends Cinema("Kino za Rogiem Brusy", "Brusy")   // Brusy — bilety24
// zgorzelec
case object MultikinoZgorzelec extends Cinema("Multikino Zgorzelec", "Multikino")   // Zgorzelec — Multikino API
case object KinoKadrBogatynia extends Cinema("Kino Kadr Bogatynia", "Kadr")   // Bogatynia — bilety24
// ilawa
case object KinoteatrPasja extends Cinema("Kinoteatr Pasja", "Pasja")   // Iława — MSI biletyick.miastoilawa.pl
case object KinoPokoj extends Cinema("Kino Pokój", "Pokój")   // Lubawa — ekobilet mok-lubawa
case object KinoteatrHarmonia extends Cinema("Kinoteatr Harmonia", "Harmonia")   // Nowe Miasto Lubawskie — bilety24
// ketrzyn
case object KinoGwiazdaKetrzyn extends Cinema("Kino Gwiazda Kętrzyn", "Gwiazda")   // Kętrzyn — own site kino.ketrzyn.pl
case object KinoZodiak extends Cinema("Kino Zodiak", "Zodiak")   // Mrągowo — ekobilet mck-mragowo
// zakopane
case object KinoSokolZakopane extends Cinema("Kino Sokół Zakopane", "Sokół")   // Zakopane — biletyna Kino-Sokol
case object KinoGiewont extends Cinema("Kino Giewont", "Giewont")   // Zakopane — biletyna Kino-Giewont
case object KinoMiejsce extends Cinema("Kino Miejsce", "Miejsce")   // Zakopane — biletyna Kino-Miejsce
case object KinoSniezkaRabka extends Cinema("Kino Śnieżka Rabka", "Śnieżka")   // Rabka-Zdrój — bilety24
// wyszkow
case object KinoDobrychFilmow extends Cinema("Kino Dobrych Filmów", "Dobrych Filmów")   // Wyszków — biletyna Wyszkowski-Osrodek-Kultury-Hutnik
case object KinoMiGOK extends Cinema("Kino MiGOK", "MiGOK")   // Łochów — biletyna Miejski-i-Gminny-Osrodek-Kultury
// zlocieniec
case object KinoMewaZlocieniec extends Cinema("Kino Mewa Złocieniec", "Mewa")   // Złocieniec — bilety24
case object KinoWKadrze extends Cinema("Kino w Kadrze", "w Kadrze")   // Warszawa — biletyna Dom-Kultury-Kadr
// slubice
case object KinoSmok extends Cinema("Kino Smok", "Smok")   // Słubice — bilety24
// wlodawa
case object KinoWDK extends Cinema("Kino WDK Włodawa", "WDK")   // Włodawa — filmweb 2312
case object KinoOKDrawsko extends Cinema("Kino OK Drawsko Pomorskie", "Drawsko")   // Drawsko Pomorskie — bilety24

// biletyna.pl-only venues (2026-09-23 sweep of its hall sitemap): screening now,
// listed nowhere else we read.
case object KinoMCKTkacz extends Cinema("MCK Tkacz", "Tkacz")   // Tomaszów Mazowiecki — biletyna
case object KinoMDKOpoczno extends Cinema("MDK Opoczno", "Opoczno")   // Opoczno — biletyna
case object KinoDomSztukiUrsynow extends Cinema("Kino Domu Sztuki", "Dom Sztuki")   // Warszawa — biletyna
case object KinoTerminalKultury extends Cinema("Terminal Kultury Gocław", "Terminal Kultury")   // Warszawa — biletyna
case object KinoBielanskiOsrodekKultury extends Cinema("Bielański Ośrodek Kultury", "BOK Bielany")   // Warszawa — biletyna
case object KinoWawerskaStrefaKultury extends Cinema("Wawerska Strefa Kultury", "Wawer")   // Warszawa — biletyna
case object KinoBasn extends Cinema("Kino Baśń", "Baśń")   // Piastów — biletyna
case object KinoKasynoOficerskie extends Cinema("Kasyno Oficerskie", "Kasyno Oficerskie")   // Nowy Dwór Mazowiecki — biletyna
case object KinoNowyPafawag extends Cinema("Centrum Kultury Nowy Pafawag", "Nowy Pafawag")   // Wrocław — biletyna
case object TeatrBoto extends Cinema("Teatr Boto", "Boto")   // Sopot — biletyna
case object TeatrAtelier extends Cinema("Teatr Atelier", "Atelier")   // Sopot — biletyna
case object KinoCKGniewino extends Cinema("Centrum Kultury Gniewino", "Gniewino")   // Gniewino — biletyna
case object KinoMOKGlogow extends Cinema("MOK Głogów", "MOK Głogów")   // Głogów — biletyna
case object TeatrGryphius extends Cinema("Teatr im. Andreasa Gryphiusa", "Gryphius")   // Głogów — biletyna
case object KinoCKSwiecie extends Cinema("Centrum Kultury w Świeciu", "Świecie")   // Świecie — biletyna
case object KinoMOKGlowno extends Cinema("MOK Głowno", "Głowno")   // Głowno — biletyna
case object KinoPromienWiecbork extends Cinema("Kino Promień Więcbork", "Promień")   // Więcbork — biletyna
case object KinoCKiSSepolno extends Cinema("CKiS Sępólno Krajeńskie", "Sępólno")   // Sępólno Krajeńskie — biletyna
case object KinoGOKLipka extends Cinema("GOK Lipka", "Lipka")   // Lipka — biletyna
case object KinoNOKNasielsk extends Cinema("Nasielski Ośrodek Kultury", "Nasielsk")   // Nasielsk — biletyna
case object KinoFeniks extends Cinema("RCK Feniks", "Feniks")   // Rydułtowy — biletyna
case object KinoCKPrzezmierowo extends Cinema("Centrum Kultury Przeźmierowo", "Przeźmierowo")   // Przeźmierowo — biletyna
case object KinoNOKNowyTomysl extends Cinema("Nowotomyski Ośrodek Kultury", "Nowy Tomyśl")   // Nowy Tomyśl — biletyna
case object KinoMewaMiedzychod extends Cinema("Kino Mewa Międzychód", "Mewa")   // Międzychód — biletyna
case object KinoPromyk extends Cinema("GOK Promyk", "Promyk")   // Bystra — biletyna
case object KinoKsiazka extends Cinema("Kino Książka", "Książka")   // Stara Błotnica — biletyna
case object KinoChDKChoszczno extends Cinema("Choszczeński Dom Kultury", "Choszczno")   // Choszczno — biletyna
case object KinoDOKDrawno extends Cinema("Drawieński Ośrodek Kultury", "Drawno")   // Drawno — biletyna
case object KinoMGOKRecz extends Cinema("MGOK Recz", "Recz")   // Recz — biletyna
case object KinoWRatuszu extends Cinema("Kino w Ratuszu", "Ratusz")   // Zduńska Wola — MSI
case object KinoOKJastrowie extends Cinema("Ośrodek Kultury Jastrowie", "Jastrowie")   // Jastrowie — biletyna
case object KinoMarzenie extends Cinema("Kino Marzenie", "Marzenie")   // Tarnów — own site
case object KinoPlanetCinemaZabrze extends Cinema("Planet Cinema Zabrze", "Planet Cinema")   // Zabrze — MSI
case object KinoPlanetCinemaZawiercie extends Cinema("Planet Cinema Zawiercie", "Planet Cinema")   // Zawiercie — MSI
case object KinoSwiatowidElblag extends Cinema("Kino Światowid Elbląg", "Światowid Elbląg")   // Elbląg — own site

// 2026-09-23 scraper sweep: own-site / systembiletowy venues found screening now.
case object KinoBCKBytom extends Cinema("BCKino Bytom", "BCKino")   // Bytom — systembiletowy
case object KinoNowaFalaGizycko extends Cinema("Kino Nowa Fala", "Nowa Fala")   // Giżycko — own site
case object KinoOrzelUstrzyki extends Cinema("Kino Orzeł Ustrzyki Dolne", "Orzeł")   // Ustrzyki Dolne — systembiletowy
case object KinoGrajfka extends Cinema("Kino Grajfka", "Grajfka")   // Chorzów — own site

// Venues verified screening from a 2026-09-23 nearby-towns sweep. Every other
// candidate town in that sweep (Brodnica, Barcin, Białobrzegi, Kościan,
// Koluszki, Piekoszów, Kwidzyn, Solec Kujawski, and the "Kino za Rogiem"
// screens in Sompolno/Stare Miasto/Strzałkowo) turned out to have no real
// dated programme — either no scrapable source at all, or a source (biletyna
// events feed, a bilety24 organiser page, an ekobilet venue page, a coigdzie.pl
// listing) that carried zero ScreeningEvents / was dark / turned out to be a
// generic national catalog reused verbatim across unrelated towns — so none of
// them are modelled.
case object KinoKadrStaszow extends Cinema("Kino Kadr Staszów", "Kadr")   // Staszów — systemBiletowy
case object KinoSCKStrzegom extends Cinema("Kino SCK Strzegom", "SCK")   // Strzegom — biletyna
case object KinoSztuka extends Cinema("Kino Sztuka", "Sztuka")   // Chrzanów — biletyna

// small independent venues (2026-09-23 sweep, no shared ticketing platform)
case object DomKulturyLapy extends Cinema("Dom Kultury w Łapach", "Łapy")   // Łapy — own site
case object KinoBielsk extends Cinema("Kino Bielsk", "Bielsk")   // Bielsk Podlaski — own site
case object KinoNarew extends Cinema("Kino Narew", "Narew")   // Pułtusk — own site
case object KinoGrunwald extends Cinema("Kino Grunwald", "Grunwald")   // Olsztynek — own site
case object KinoParczew extends Cinema("Kino Parczew", "Parczew")   // Parczew — own site

// 2026-09-23 nearby-towns sweep, batch 2.
case object KinoMOKMiedzyrzecz extends Cinema("Kino MOK Międzyrzecz", "MOK Międzyrzecz")   // Międzyrzecz — MSI
case object KinoZaRogiemSiedlec extends Cinema("Kino za Rogiem w Siedlcu", "Siedlec")   // Siedlec — own site
// Verified screening from a 2026-09-23 nearby-towns sweep (ekobilet/systembiletowy
// venues). Iłża (CKiT, ekobilet slug centrum-kultury-i-turystyki-w-ilzy-8211) was
// checked in the same sweep and rejected: its earliest real screening is
// 2026-10-08, one day past the ~2-week verification window, so it is deliberately
// NOT modelled here.
case object KinoDKGora extends Cinema("Dom Kultury w Górze", "DK Góra")   // Góra — ekobilet
case object KinoZaciszeWasosz extends Cinema("Kino Zacisze (Wąsosz)", "Zacisze Wąsosz")   // Wąsosz — ekobilet
case object KinoMilenium extends Cinema("Kino Milenium", "Milenium")   // Milejów — ekobilet
case object KinoOpolanka extends Cinema("Kino Opolanka", "Opolanka")   // Opole Lubelskie — ekobilet
case object KinoTon extends Cinema("Kino Ton", "Ton")   // Żuromin — ekobilet
case object KinoCKiTIlza extends Cinema("Kino CKiT Iłża", "CKiT")   // Iłża — ekobilet
case object KinoBieszczadzkiDK extends Cinema("Bieszczadzki Dom Kultury", "BDK")   // Lesko — systembiletowy
// Venues verified from a 2026-09-23 lead sweep of 8 named towns. Tarnowo
// Podgórne's lead (GOK Sezam) turned out to be the same physical venue as
// KinoCKPrzezmierowo, already modelled above (Przeźmierowo is the village
// within Tarnowo Podgórne gmina where its hall sits) — not re-added. Kęty's
// biletyna place page carries only theatre/cabaret events, no films; Piaseczno's
// carries exactly one isolated film screening ever, not a recurring programme —
// neither is modelled.
case object KinoGryfinskiDomKultury extends Cinema("Gryfiński Dom Kultury", "Gryfiński DK")   // Gryfino — biletyna
case object KinoTucholskiOsrodekKultury extends Cinema("Tucholski Ośrodek Kultury", "TOK Tuchola")   // Tuchola — biletyna
case object KinoManhattanJanikowo extends Cinema("Kino Manhattan", "Manhattan")   // Janikowo — MSI
case object KinoAleKinoLibiaz extends Cinema("Ale! Kino", "Ale! Kino")   // Libiąż — MSI
case object KinoWolnoscSzczecinek extends Cinema("Kino Wolność", "Wolność")   // Szczecinek — MSI
// 2026-09-23 nearby-towns sweep, second pass. Each own site's booking link led
// to a ticketing platform we already have a client for, so all four reuse an
// existing client rather than a new one; only Kino Kultura Chojnów (own ASP.NET
// booking subsite, no browsable listing) needed a bespoke parser. Kino Café
// (Konin, kinooskard.pl/kino-cafe) was checked and deliberately NOT modelled —
// its "Kup bilet" links resolve to event ids already present in the existing
// Kino Oskard bilety24 feed, just suffixed "/Kino Cafe"; adding it would double
// the room's screenings.
case object Kino1410 extends Cinema("Kino 1410", "1410")   // Toruń — systembiletowy
case object KinoFrajda extends Cinema("Kino Frajda", "Frajda")   // Chorzów — systembiletowy
case object KinoStopiak extends Cinema("Stopiak Cinema", "Stopiak")   // Nowy Targ — MSI
case object KinoKulturaChojnow extends Cinema("Kino Kultura Chojnów", "Kultura Chojnów")   // Chojnów — own site
case object KinoLotSwidnik extends Cinema("Kino Lot Świdnik", "Lot Świdnik")   // Świdnik — biletyna
// second 2026-09-23 nearby-towns sweep: verified screening now.
case object KinoKadrTomaszowLubelski extends Cinema("Kino Kadr (Tomaszowski Dom Kultury)", "Kadr TL")   // Tomaszów Lubelski — own site
case object KinoJOK extends Cinema("Kino JOK (Janowski Ośrodek Kultury)", "JOK")   // Janów Lubelski — own site
case object KinoCKiBNowaSarzyna extends Cinema("Kino CKiB Nowa Sarzyna", "CKiB")   // Nowa Sarzyna — systembiletowy
case object KinoSokolStrzyzow extends Cinema("Kino Sokół Strzyżów", "Sokół")   // Strzyżów — own site
case object KinoRCKDrzewica extends Cinema("Kino RCK Drzewica", "RCK Drzewica")   // Drzewica — own site
case object KinoMDKMyszkow extends Cinema("Kino MDK Myszków", "MDK Myszków")   // Myszków — own site
