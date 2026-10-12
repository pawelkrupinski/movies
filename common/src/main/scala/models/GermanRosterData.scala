// GENERATED from data/germany/pages.json by data/germany/scripts/generate_roster.py
// — do NOT edit by hand. Full German cinema roster: 446 pages over 16 Bundesländer /
// 1,545 cinemas (Filmstarts, and the ones only Cineamo or kinoheld list). Regenerate after adding venues or re-clustering;
// see data/germany/README.md.
package models

private[models] object GermanRosterData {
  // (displayName, pillName, venue key -- its Filmstarts theaterId, or the feed that found a venue
  //  Filmstarts does not list ("cineamo:997") --, kinoprogramm.com fallback path, town)
  type C = (String, String, String, Option[String], String)
  // (slug, slug qualified with its Bundesland, name, Bundesland, lat, lon, multiTown, towns, cinemas)
  type R = (String, String, String, String, Double, Double, Boolean, Seq[String], Seq[C])

  private def p_berlin: R = ("berlin", "berlin-berlin", "Berlin", "Berlin", 52.5244, 13.4105, false, Seq("Berlin"), Seq(
    ("Acud Kino", "Acud Kino", "A0006", Some("/kino/berlin/acud-kino-31396"), "Berlin"),
    ("Adria Filmtheater Steglitz", "Adria Filmtheater Steglitz", "A0008", Some("/kino/berlin/adria-filmtheater-31359"), "Berlin"),
    ("Akademie der Künste", "Akademie der Künste", "A2830", None, "Berlin"),
    ("Arsenal Berlin", "Arsenal Berlin", "A0050", Some("/kino/berlin/arsenal-institut-fuer-film-und-videokunst-e.v.-31387"), "Berlin"),
    ("ARTE Freiluftkino Kulturforum", "ARTE Freiluftkino Kulturforum", "A1807", None, "Berlin"),
    ("Astor Film Lounge", "Astor Film Lounge", "A0514", Some("/kino/berlin/astor-film-lounge-31385"), "Berlin"),
    ("Astra-Filmpalast", "Astra-Filmpalast", "A0056", Some("/kino/berlin/astrafilmpalast-31400"), "Berlin"),
    ("B-Ware Ladenkino", "B-Ware Ladenkino", "A2620", Some("/kino/berlin/bware-ladenkino-80720"), "Berlin"),
    ("Babylon (Kreuzberg)", "Babylon (Kreuzberg)", "A0075", Some("/kino/berlin/babylon-kreuzberg-31447"), "Berlin"),
    ("Babylon (Mitte)", "Babylon (Mitte)", "A1489", Some("/kino/berlin/babylon-64256"), "Berlin"),
    ("Bali-Kino", "Bali-Kino", "A0079", Some("/kino/berlin/balikino-31391"), "Berlin"),
    ("Blauer Stern", "Blauer Stern", "A0099", Some("/kino/berlin/blauer-stern-31389"), "Berlin"),
    ("Brotfabrik im Kunst- &amp; Kulturzentrum", "Brotfabrik im Kunst- &amp; Kulturzentrum", "A0111", Some("/kino/berlin/brotfabrikkino-31392"), "Berlin"),
    ("Bundesplatz- Studio", "Bundesplatz- Studio", "A0114", Some("/kino/berlin/bundesplatzkino-31366"), "Berlin"),
    ("Capitol Dahlem", "Capitol Dahlem", "A0151", Some("/kino/berlin/capitol-dahlem-31438"), "Berlin"),
    ("Casablanca Berlin", "Casablanca Berlin", "A0171", Some("/kino/berlin/casablanca-34710"), "Berlin"),
    ("Cinema Paris", "Cinema Paris", "A0252", Some("/kino/berlin/cinema-paris-31368"), "Berlin"),
    ("CinemaxX Potsdamer Platz", "CinemaxX Potsdamer Platz", "A0275", Some("/kino/berlin/cinemaxx-36643"), "Berlin"),
    ("CineMotion Berlin Hohenschönhausen", "CineMotion Berlin Hohenschönhausen", "A0290", Some("/kino/berlin/cinemotion-berlin-hohenschoenhausen-37070"), "Berlin"),
    ("Cineplex Alhambra", "Cineplex Alhambra", "A0015", None, "Berlin"),
    ("Cineplex Neukölln", "Cineplex Neukölln", "A0647", Some("/kino/berlin/cineplex-neukoelln-arcaden-52857"), "Berlin"),
    ("Cineplex Spandau", "Cineplex Spandau", "A0316", Some("/kino/berlin/cineplex-spandau-52876"), "Berlin"),
    ("Cineplex Titania", "Cineplex Titania", "A1184", Some("/kino/berlin/cineplex-titania-72776"), "Berlin"),
    ("CineStar Berlin - Cubix am Alexanderplatz", "CineStar Berlin - Cubix am Alexanderplatz", "A0332", Some("/kino/berlin/cinestar-cubix-am-alexanderplatz-46796"), "Berlin"),
    ("CineStar Berlin - Hellersdorf", "CineStar Berlin - Hellersdorf", "A0373", Some("/kino/berlin/cinestar-hellersdorf-34961"), "Berlin"),
    ("CineStar Berlin - Kino in der Kulturbrauerei", "CineStar Berlin - Kino in der Kulturbrauerei", "A0738", Some("/kino/berlin/cinestar-kino-in-der-kulturbrauerei-39607"), "Berlin"),
    ("CineStar Berlin - Tegel", "CineStar Berlin - Tegel", "A0369", Some("/kino/berlin/cinestar-berlin-tegel-37654"), "Berlin"),
    ("City Kino Wedding", "City Kino Wedding", "A2672", Some("/kino/berlin/city-kino-wedding-85657"), "Berlin"),
    ("Colosseum Filmtheater Berlin", "Colosseum Filmtheater Berlin", "A1207", Some("/kino/berlin/colosseum-kino-35184"), "Berlin"),
    ("Cosima-Filmtheater", "Cosima-Filmtheater", "A0427", Some("/kino/berlin/cosima-31367"), "Berlin"),
    ("Czech Centre / Tschechische Zentrum Berlin", "Czech Centre / Tschechische Zentrum Berlin", "A2834", None, "Berlin"),
    ("Delphi Filmpalast am Zoo", "Delphi Filmpalast am Zoo", "A0435", Some("/kino/berlin/delphi-filmpalast-am-zoo-31370"), "Berlin"),
    ("Delphi LUX", "Delphi LUX", "A2870", Some("/kino/berlin/delphi-lux-90397"), "Berlin"),
    ("Eva-Lichtspiele", "Eva-Lichtspiele", "A0476", Some("/kino/berlin/evalichtspiele-31410"), "Berlin"),
    ("Filmkunst 66", "Filmkunst 66", "A0503", Some("/kino/berlin/filmkunst-66-31357"), "Berlin"),
    ("Filmrauschpalast im Kulturfabrik Moabit", "Filmrauschpalast im Kulturfabrik Moabit", "A0523", Some("/kino/berlin/filmrauschpalast-im-kulturfabrik-moabit-31382"), "Berlin"),
    ("Filmtheater am Friedrichshain", "Filmtheater am Friedrichshain", "A0479", Some("/kino/berlin/filmtheater-am-friedrichshain-31436"), "Berlin"),
    ("Freiheit fünfzehn - kino im liegestuh", "Freiheit fünfzehn - kino im liegestuh", "A2840", None, "Berlin"),
    ("Freiluftkino Friedrichshain", "Freiluftkino Friedrichshain", "A1805", None, "Berlin"),
    ("Freiluftkino Hasenheide", "Freiluftkino Hasenheide", "A1784", Some("/kino/berlin/freiluftkino-hasenheide-61899"), "Berlin"),
    ("Freiluftkino Kreuzberg", "Freiluftkino Kreuzberg", "A2198", Some("/kino/berlin/freiluftkino-kreuzberg-31380"), "Berlin"),
    ("Freiluftkino Naturtheater Friedrichshagen", "Freiluftkino Naturtheater Friedrichshagen", "A1808", None, "Berlin"),
    ("Freiluftkino Neue Zukunft", "Freiluftkino Neue Zukunft", "G02A8", Some("/kino/berlin/freiluftkino-neue-zukunft-93119"), "Berlin"),
    ("Freiluftkino Parkbühne Biesdorf", "Freiluftkino Parkbühne Biesdorf", "A1809", None, "Berlin"),
    ("Freiluftkino Rehberge", "Freiluftkino Rehberge", "A2739", Some("/kino/berlin/freiluftkino-rehberge-76002"), "Berlin"),
    ("fsk am Oranienplatz", "fsk am Oranienplatz", "A0564", Some("/kino/berlin/fsk-am-oranienplatz-31388"), "Berlin"),
    ("Hackesche Höfe Kino", "Hackesche Höfe Kino", "A0597", Some("/kino/berlin/hackesche-hoefe-kino-34705"), "Berlin"),
    ("IL KINO", "IL KINO", "A2674", Some("/kino/berlin/il-kino-85697"), "Berlin"),
    ("Kant Kino Charlottenburg", "Kant Kino Charlottenburg", "A0645", None, "Berlin"),
    ("Kino Central Berlin", "Kino Central Berlin", "A0185", Some("/kino/berlin/centralkino-31390"), "Berlin"),
    ("Kino Central Open Air (Berlin)", "Kino Central Open Air (Berlin)", "A2760", Some("/kino/berlin/centralkino-open-air-berlin-90497"), "Berlin"),
    ("Kino im Kulturhaus Spandau", "Kino im Kulturhaus Spandau", "A1734", Some("/kino/berlin/kino-im-kulturhaus-spandau-60817"), "Berlin"),
    ("Kino International Berlin", "Kino International Berlin", "A0625", Some("/kino/berlin/international-31440"), "Berlin"),
    ("Kino Intimes", "Kino Intimes", "A0627", Some("/kino/berlin/kino-intimes-34709"), "Berlin"),
    ("Kino Krokodil", "Kino Krokodil", "A0745", Some("/kino/berlin/kino-krokodil-66296"), "Berlin"),
    ("Kino Spreehöfe Berlin", "Kino Spreehöfe Berlin", "A0753", None, "Berlin"),
    ("Kino Zukunft", "Kino Zukunft", "A2660", Some("/kino/berlin/kino-zukunft-92976"), "Berlin"),
    ("Klick Kino", "Klick Kino", "A2751", Some("/kino/berlin/klick-kino-89117"), "Berlin"),
    ("Lichtblick-Kino", "Lichtblick-Kino", "A0901", Some("/kino/berlin/lichtblickkino-37315"), "Berlin"),
    ("Moviemento", "Moviemento", "A0970", Some("/kino/berlin/moviemento-31412"), "Berlin"),
    ("Neues Off", "Neues Off", "A0990", Some("/kino/berlin/neues-off-31449"), "Berlin"),
    ("Odeon", "Odeon", "A1577", Some("/kino/berlin/odeon-31445"), "Berlin"),
    ("Open Air Kino Spandau Innenhof Stadtbibliothek", "Open Air Kino Spandau Innenhof Stadtbibliothek", "A2276", None, "Berlin"),
    ("Passage Kino Neukölln", "Passage Kino Neukölln", "A1035", Some("/kino/berlin/passage-kinos-31448"), "Berlin"),
    ("Regenbogen Kino", "Regenbogen Kino", "A1049", Some("/kino/berlin/regenbogen-kino-36313"), "Berlin"),
    ("Rollberg Kino", "Rollberg Kino", "A1083", Some("/kino/berlin/rollberg-kino-31437"), "Berlin"),
    ("Sputnik Südstern", "Sputnik Südstern", "A1145", Some("/kino/berlin/sputnik-suedstern-73680"), "Berlin"),
    ("Thalia Berlin", "Thalia Berlin", "A1177", Some("/kino/berlin/thalia-53676"), "Berlin"),
    ("Tilsiter-Lichtspiele", "Tilsiter-Lichtspiele", "A1183", Some("/kino/berlin/tilsiterlichtspiele-31421"), "Berlin"),
    ("Toni und Tonino", "Toni und Tonino", "A1187", Some("/kino/berlin/toni-und-tonino-31431"), "Berlin"),
    ("UCI am Eastgate", "UCI am Eastgate", "A0667", Some("/kino/berlin/uci-kinowelt-am-eastgate-37709"), "Berlin"),
    ("UCI East Side Gallery", "UCI East Side Gallery", "A2906", Some("/kino/berlin/uci-kinowelt-berlin-east-side-gallery-%7C-luxe-91577"), "Berlin"),
    ("UCI Gropius Passagen", "UCI Gropius Passagen", "A1210", Some("/kino/berlin/uci-kinowelt-gropius-passagen-31427"), "Berlin"),
    ("Union Filmtheater Friedrichshagen", "Union Filmtheater Friedrichshagen", "A1230", Some("/kino/berlin/union-filmtheater-friedrichshagen-34712"), "Berlin"),
    ("Urania-Filmbühne", "Urania-Filmbühne", "A1248", Some("/kino/berlin/uraniafilmbuehne-39646"), "Berlin"),
    ("Wolf Kino", "Wolf Kino", "A2749", Some("/kino/berlin/wolf-kino-berlin-89057"), "Berlin"),
    ("Xenon", "Xenon", "A1284", Some("/kino/berlin/xenon-31435"), "Berlin"),
    ("Yorck Kino (Kreuzberg)", "Yorck Kino (Kreuzberg)", "A0995", Some("/kino/berlin/yorck-kinos-31446"), "Berlin"),
    ("Z-inema im Z-Bar", "Z-inema im Z-Bar", "A2374", Some("/kino/berlin/zinema-im-zbar-75057"), "Berlin"),
    ("Zeughauskino - Deutsches Historisches Museum", "Zeughauskino - Deutsches Historisches Museum", "A1290", Some("/kino/berlin/zeughauskino-deutsches-historisches-museum-31375"), "Berlin"),
    ("ZOO PALAST Berlin", "ZOO PALAST Berlin", "A1215", Some("/kino/berlin/zoopalast-31428"), "Berlin"),
    ("SINEMA TRANSTOPIA - Berlin", "SINEMA TRANSTOPIA - Berlin", "cineamo:1729", None, "Berlin")
  ))
  private def p_hamburg: R = ("hamburg", "hamburg-hamburg", "Hamburg", "Hamburg", 53.5507, 9.993, false, Seq("Hamburg"), Seq(
    ("3001 Kino", "3001 Kino", "A0002", Some("/kino/hamburg/3001-kino-32436"), "Hamburg"),
    ("Abaton", "Abaton", "A0003", Some("/kino/hamburg/abaton-32437"), "Hamburg"),
    ("Alabama-Kino", "Alabama-Kino", "A0012", Some("/kino/hamburg/alabamakino-32438"), "Hamburg"),
    ("Astor Film Lounge HafenCity Hamburg", "Astor Film Lounge HafenCity Hamburg", "A2900", Some("/kino/hamburg/astor-film-lounge-hafencity-91617"), "Hamburg"),
    ("B-Movie", "B-Movie", "A1907", Some("/kino/hamburg/bmovie-66237"), "Hamburg"),
    ("Blankeneser Kino", "Blankeneser Kino", "A0677", Some("/kino/hamburg/blankeneser-kino-32464"), "Hamburg"),
    ("CinemaxX Dammtor", "CinemaxX Dammtor", "A1467", Some("/kino/hamburg/cinemaxx-dammtor-33089"), "Hamburg"),
    ("CinemaxX Harburg", "CinemaxX Harburg", "A0287", Some("/kino/hamburg/cinemaxx-harburg-38443"), "Hamburg"),
    ("CinemaxX Wandsbek", "CinemaxX Wandsbek", "A0282", Some("/kino/hamburg/cinemaxx-wandsbek-42017"), "Hamburg"),
    ("Elbe-Kino", "Elbe-Kino", "A1400", Some("/kino/hamburg/elbekino-32453"), "Hamburg"),
    ("FilmRaum", "FilmRaum", "A2730", Some("/kino/hamburg/filmraum-88038"), "Hamburg"),
    ("Hansa Studio Bergedorf", "Hansa Studio Bergedorf", "A1449", Some("/kino/hamburg/hansafilmstudio-bergedorf-32462"), "Hamburg"),
    ("Holi", "Holi", "A0614", Some("/kino/hamburg/holi-32439"), "Hamburg"),
    ("Kinopolis HafenCity", "Kinopolis HafenCity", "G02MI", Some("/kino/hamburg/kinopolis-hafencity-93333"), "Hamburg"),
    ("Koralle", "Koralle", "A0869", Some("/kino/hamburg/koralle-49596"), "Hamburg"),
    ("Lichtmess", "Lichtmess", "A2604", Some("/kino/hamburg/lichtmess-77077"), "Hamburg"),
    ("LOTTO Hamburg SchanzenKino Open Air", "LOTTO Hamburg SchanzenKino Open Air", "A2296", Some("/kino/hamburg/lotto-hamburg-schanzenkino-open-air-59037"), "Hamburg"),
    ("Magazin", "Magazin", "A1553", Some("/kino/hamburg/magazin-32445"), "Hamburg"),
    ("Metropolis Kino", "Metropolis Kino", "A2712", Some("/kino/hamburg/metropolis-kino-32449"), "Hamburg"),
    ("Outdoor Cine - Das Open Air Kino im Völkerkunde-Museum", "Outdoor Cine - Das Open Air Kino im Völkerkunde-Museum", "A1913", None, "Hamburg"),
    ("Passage Hamburg", "Passage Hamburg", "A1034", Some("/kino/hamburg/passage-kino-hamburg-32443"), "Hamburg"),
    ("Savoy (Hamburg)", "Savoy (Hamburg)", "A0958", Some("/kino/hamburg/savoy-82236"), "Hamburg"),
    ("SchanzenKino 73 (zweisprachig)", "SchanzenKino 73 (zweisprachig)", "A2747", None, "Hamburg"),
    ("Sommerkino auf dem Alsterdorfer Markt", "Sommerkino auf dem Alsterdorfer Markt", "A2089", Some("/kino/hamburg/sommerkino-auf-dem-alsterdorfer-markt-32451"), "Hamburg"),
    ("UCI Mundsburg", "UCI Mundsburg", "A1203", Some("/kino/hamburg/uci-kinowelt-mundsburg-37115"), "Hamburg"),
    ("UCI Wandsbek", "UCI Wandsbek", "A1214", Some("/kino/hamburg/uci-kinowelt-wandsbek-43194"), "Hamburg"),
    ("Zeise Kinos", "Zeise Kinos", "A1464", Some("/kino/hamburg/zeise-kinos-32465"), "Hamburg"),
    ("Studio Kino", "Studio Kino", "A1166", None, "Hamburg")
  ))
  private def p_muenchen: R = ("muenchen", "muenchen-bayern", "München", "Bayern", 48.1374, 11.5755, false, Seq("München"), Seq(
    ("ABC-Kino", "ABC-Kino", "A1476", Some("/kino/muenchen/abckino-32873"), "München"),
    ("Astor Cinema Lounge", "Astor Cinema Lounge", "A2636", None, "München"),
    ("Astor Film Lounge im Arri", "Astor Film Lounge im Arri", "A1298", Some("/kino/muenchen/astor-film-lounge-im-arri-32867"), "München"),
    ("Cadillac Filmtheater", "Cadillac Filmtheater", "A0131", Some("/kino/muenchen/cadillac-und-veranda-kino-32857"), "München"),
    ("Cincinnati", "Cincinnati", "A0216", Some("/kino/muenchen/cincinnati-32870"), "München"),
    ("Cinema München", "Cinema München", "A2186", Some("/kino/muenchen/cinema-32869"), "München"),
    ("CinemaxX München", "CinemaxX München", "A0943", Some("/kino/muenchen/cinemaxx-33802"), "München"),
    ("City-Kinos - München", "City-Kinos - München", "A0397", None, "München"),
    ("Filmmuseum München", "Filmmuseum München", "A1419", Some("/kino/muenchen/filmmuseum-32872"), "München"),
    ("Forum 2 - Kulturverein Olympiadorf e.V.", "Forum 2 - Kulturverein Olympiadorf e.V.", "A0654", Some("/kino/muenchen/forum-2-kulturverein-olympiadorf-e.v.-32868"), "München"),
    ("Gloria Palast", "Gloria Palast", "A1443", Some("/kino/muenchen/gloriapalast-32885"), "München"),
    ("Instituto Cervantes - München", "Instituto Cervantes - München", "A2832", None, "München"),
    ("Istituto Italiano di Cultura - München", "Istituto Italiano di Cultura - München", "A2847", None, "München"),
    ("Kino in der Hochschule für Fernsehen und Film München", "Kino in der Hochschule für Fernsehen und Film München", "A2743", None, "München"),
    ("Kino Solln", "Kino Solln", "A1499", Some("/kino/muenchen/kino-solln-32880"), "München"),
    ("Leopold", "Leopold", "A0895", Some("/kino/muenchen/leopold-32875"), "München"),
    ("Mathäser Filmpalast", "Mathäser Filmpalast", "A0025", Some("/kino/muenchen/mathaeser-filmpalast-54158"), "München"),
    ("Monopol", "Monopol", "A0963", Some("/kino/muenchen/monopol-65618"), "München"),
    ("Museum-Lichtspiele München", "Museum-Lichtspiele München", "A1570", Some("/kino/muenchen/museumlichtspiele-32866"), "München"),
    ("Neues Arena", "Neues Arena", "A1573", Some("/kino/muenchen/arena-filmtheater-32856"), "München"),
    ("Neues Maxim Kino München", "Neues Maxim Kino München", "A0941", Some("/kino/muenchen/neues-maxim-32859"), "München"),
    ("Neues Rex - Filmtheater", "Neues Rex - Filmtheater", "A0991", Some("/kino/muenchen/neues-rex-filmtheater-32879"), "München"),
    ("Neues Rottmann", "Neues Rottmann", "A1575", Some("/kino/muenchen/neues-rottmann-32863"), "München"),
    ("Open Air Kino am Olympiasee", "Open Air Kino am Olympiasee", "A2425", Some("/kino/muenchen/open-air-kino-am-olympiasee-75479"), "München"),
    ("Open Air Kino, Mond &amp; Sterne", "Open Air Kino, Mond &amp; Sterne", "A1815", None, "München"),
    ("Programm-Kino im Viehhof-Zelt", "Programm-Kino im Viehhof-Zelt", "A2721", None, "München"),
    ("Rio Filmpalast", "Rio Filmpalast", "A1603", Some("/kino/muenchen/rio-filmpalast-32884"), "München"),
    ("Royal Filmpalast", "Royal Filmpalast", "A1610", Some("/kino/muenchen/royal-filmpalast-32892"), "München"),
    ("Studio Isabella", "Studio Isabella", "A1642", Some("/kino/muenchen/studio-isabella-32864"), "München"),
    ("Technische Universität", "Technische Universität", "A2600", None, "München"),
    ("Theatiner Film", "Theatiner Film", "A1179", Some("/kino/muenchen/theatiner-film-32871"), "München"),
    ("Werkstattkino e.V.", "Werkstattkino e.V.", "A1276", Some("/kino/muenchen/werkstattkino-e.v.-39386"), "München")
  ))
  private def p_koeln: R = ("koeln", "koeln-nordrhein-westfalen", "Köln", "Nordrhein-Westfalen", 50.9333, 6.95, false, Seq("Köln"), Seq(
    ("Cinedom", "Cinedom", "A0027", Some("/kino/koeln/cinedom-31189"), "Köln"),
    ("Cinenova", "Cinenova", "A0298", Some("/kino/koeln/cinenova-33237"), "Köln"),
    ("Cineplex Filmpalast Köln", "Cineplex Filmpalast Köln", "A0031", Some("/kino/koeln/cineplex-koeln-88897"), "Köln"),
    ("Filmclub 813", "Filmclub 813", "A0737", Some("/kino/koeln/filmclub-813-61197"), "Köln"),
    ("Filmforum im Museum Ludwig", "Filmforum im Museum Ludwig", "A2139", Some("/kino/koeln/filmforum-im-museum-ludwig-73057"), "Köln"),
    ("Filmhaus Kino Köln", "Filmhaus Kino Köln", "A0499", Some("/kino/koeln/filmhaus-kino-30858"), "Köln"),
    ("Filmpalette", "Filmpalette", "A0522", Some("/kino/koeln/filmpalette-31837"), "Köln"),
    ("Japanisches Kulturinstitut", "Japanisches Kulturinstitut", "A0629", Some("/kino/koeln/japanisches-kulturinstitut-65837"), "Köln"),
    ("Kino Weißhaus", "Kino Weißhaus", "A0756", Some("/kino/koeln/kino-weisshaus-31843"), "Köln"),
    ("Lichtspiele Kalk", "Lichtspiele Kalk", "A2915", None, "Köln"),
    ("Metropolis Köln", "Metropolis Köln", "A0959", Some("/kino/koeln/metropolis-31845"), "Köln"),
    ("Odeon Köln", "Odeon Köln", "A1004", Some("/kino/koeln/odeon-31847"), "Köln"),
    ("Odonisches Sommerkino", "Odonisches Sommerkino", "A2175", None, "Köln"),
    ("Off Broadway", "Off Broadway", "A1009", Some("/kino/koeln/off-broadway-31839"), "Köln"),
    ("Open Air Kino Im Rheinauenhafen", "Open Air Kino Im Rheinauenhafen", "A2436", None, "Köln"),
    ("Residenz - Astor Filmlounge", "Residenz - Astor Filmlounge", "A2616", Some("/kino/koeln/residenz-34498"), "Köln"),
    ("Rex am Ring", "Rex am Ring", "A1704", None, "Köln"),
    ("Turistarama", "Turistarama", "A2687", Some("/kino/koeln/turistarama-86197"), "Köln")
  ))
  private def p_frankfurt_am_main: R = ("frankfurt-am-main", "frankfurt-am-main-hessen", "Frankfurt am Main", "Hessen", 50.1155, 8.6842, false, Seq("Frankfurt am Main"), Seq(
    ("ASTOR Film Lounge MyZeil", "ASTOR Film Lounge MyZeil", "A2913", Some("/kino/frankfurt-am-main/astor-film-lounge-myzeill-81761"), "Frankfurt am Main"),
    ("Cinema am Rossmarkt", "Cinema am Rossmarkt", "A0261", None, "Frankfurt am Main"),
    ("CineStar Frankfurt am Main - Metropolis", "CineStar Frankfurt am Main - Metropolis", "A0378", Some("/kino/frankfurt-am-main/cinestar-metropolis-43378"), "Frankfurt am Main"),
    ("Eldorado Frankfurt am Main", "Eldorado Frankfurt am Main", "A0460", Some("/kino/frankfurt-am-main/eldorado-32028"), "Frankfurt am Main"),
    ("Filmforum Höchst", "Filmforum Höchst", "A1416", Some("/kino/frankfurt-am-main/filmforum-hoechst-32038"), "Frankfurt am Main"),
    ("Harmonie in Sachsenhausen", "Harmonie in Sachsenhausen", "A1450", Some("/kino/frankfurt-am-main/harmonie-32026"), "Frankfurt am Main"),
    ("Instituto Cervantes - Frankfurt", "Instituto Cervantes - Frankfurt", "A2908", None, "Frankfurt am Main"),
    ("Kino im DFF (Filmmuseum)", "Kino im DFF (Filmmuseum)", "A0718", Some("/kino/frankfurt-am-main/kino-im-dff-deutsches-filminstitut-und-filmmuseum-32025"), "Frankfurt am Main"),
    ("Mal seh&#039;n Kino", "Mal seh&#039;n Kino", "A1469", None, "Frankfurt am Main"),
    ("naxos.Kino im Theater Willy Praml", "naxos.Kino im Theater Willy Praml", "A2632", Some("/kino/frankfurt-am-main/naxos.kino-theater-willy-praml-80757"), "Frankfurt am Main"),
    ("Open-Air-Kino Brentanobad", "Open-Air-Kino Brentanobad", "A1853", None, "Frankfurt am Main"),
    ("Orfeos Erben", "Orfeos Erben", "A1019", Some("/kino/frankfurt-am-main/orfeos-erben-39664"), "Frankfurt am Main"),
    ("Pupille e.V. - Kino in der Uni", "Pupille e.V. - Kino in der Uni", "A1048", Some("/kino/frankfurt-am-main/pupille-kino-in-der-uni-36896"), "Frankfurt am Main")
  ))
  private def p_duesseldorf: R = ("duesseldorf", "duesseldorf-nordrhein-westfalen", "Düsseldorf", "Nordrhein-Westfalen", 51.2232, 6.7793, false, Seq("Düsseldorf"), Seq(
    ("Atelier Kino im Savoy-Theater", "Atelier Kino im Savoy-Theater", "A0064", Some("/kino/duesseldorf/atelier-kino-im-savoytheater-365"), "Düsseldorf"),
    ("Bambi", "Bambi", "A0082", Some("/kino/duesseldorf/bambi-343"), "Düsseldorf"),
    ("Black Box im Filmmuseum", "Black Box im Filmmuseum", "A0098", Some("/kino/duesseldorf/black-box-im-filmmuseum-1244"), "Düsseldorf"),
    ("Cinema Düsseldorf", "Cinema Düsseldorf", "A0231", Some("/kino/duesseldorf/cinema-36408"), "Düsseldorf"),
    ("Cinestar Düsseldorf", "Cinestar Düsseldorf", "A0379", Some("/kino/duesseldorf/cinestar-37176"), "Düsseldorf"),
    ("Frankenheim Kino", "Frankenheim Kino", "A1884", None, "Düsseldorf"),
    ("Kino Süd", "Kino Süd", "A0755", None, "Düsseldorf"),
    ("Metropol Düsseldorf", "Metropol Düsseldorf", "A0950", Some("/kino/duesseldorf/metropol-30040"), "Düsseldorf"),
    ("Open-Air-Kino Vier Linden", "Open-Air-Kino Vier Linden", "A1764", Some("/kino/duesseldorf/openairkino-vier-linden-36050"), "Düsseldorf"),
    ("UCI Düsseldorf", "UCI Düsseldorf", "A1200", Some("/kino/duesseldorf/uci-kinowelt-duesseldorf-37143"), "Düsseldorf"),
    ("UFA Palast Düsseldorf", "UFA Palast Düsseldorf", "A1221", Some("/kino/duesseldorf/ufapalast-35116"), "Düsseldorf")
  ))
  private def p_stuttgart: R = ("stuttgart", "stuttgart-baden-wuerttemberg", "Stuttgart", "Baden-Württemberg", 48.7823, 9.177, false, Seq("Stuttgart"), Seq(
    ("Atelier am Bollwerk", "Atelier am Bollwerk", "A0063", Some("/kino/stuttgart/atelier-am-bollwerk-34850"), "Stuttgart"),
    ("Cinema Stuttgart", "Cinema Stuttgart", "A0233", Some("/kino/stuttgart/cinema-62218"), "Stuttgart"),
    ("Cinemaxx Liederhalle Stuttgart", "Cinemaxx Liederhalle Stuttgart", "A0297", Some("/kino/stuttgart/cinemaxx-stuttgart-an-der-liederhalle-42697"), "Stuttgart"),
    ("CinemaxX SI-Centrum Stuttgart", "CinemaxX SI-Centrum Stuttgart", "A0942", Some("/kino/stuttgart/cinemaxx-im-sicentrum-35145"), "Stuttgart"),
    ("Corso Stuttgart", "Corso Stuttgart", "A1386", Some("/kino/stuttgart/corso-33017"), "Stuttgart"),
    ("Delphi 1+2", "Delphi 1+2", "A1389", None, "Stuttgart"),
    ("EM", "EM", "A1402", Some("/kino/stuttgart/em-33452"), "Stuttgart"),
    ("Gloria Stuttgart", "Gloria Stuttgart", "A1437", Some("/kino/stuttgart/gloria-33011"), "Stuttgart"),
    ("Kinothek", "Kinothek", "A0840", Some("/kino/stuttgart/kinothek-72656"), "Stuttgart"),
    ("Metropol Stuttgart", "Metropol Stuttgart", "A0948", Some("/kino/stuttgart/metropol-39257"), "Stuttgart"),
    ("Open Air Kino am Mercedes-Benz Museum", "Open Air Kino am Mercedes-Benz Museum", "A2811", Some("/kino/stuttgart/open-air-kino-im-mercedesbenz-museum-90417"), "Stuttgart")
  ))
  private def p_essen: R = ("essen", "essen-nordrhein-westfalen", "Essen", "Nordrhein-Westfalen", 51.4566, 7.0123, false, Seq("Essen"), Seq(
    ("Astra-Theater &amp; Luna", "Astra-Theater &amp; Luna", "A0054", Some("/kino/essen/astratheater-und-luna-374"), "Essen"),
    ("CinemaxX Essen", "CinemaxX Essen", "A0273", Some("/kino/essen/cinemaxx-383"), "Essen"),
    ("Drive In Autokino Essen", "Drive In Autokino Essen", "A0451", Some("/kino/essen/drive-in-autokino-30051"), "Essen"),
    ("Eulenspiegel Filmtheater", "Eulenspiegel Filmtheater", "A0470", Some("/kino/essen/eulenspiegel-filmtheater-400"), "Essen"),
    ("Filmstudio Glückauf", "Filmstudio Glückauf", "A0531", Some("/kino/essen/filmstudio-glueckauf-1319"), "Essen"),
    ("Galerie Cinema", "Galerie Cinema", "A0567", Some("/kino/essen/galerie-cinema-1326"), "Essen"),
    ("Lichtburg und Sabu", "Lichtburg und Sabu", "A0902", Some("/kino/essen/lichtburg-und-sabu-41636"), "Essen")
  ))
  private def p_dortmund: R = ("dortmund", "dortmund-nordrhein-westfalen", "Dortmund", "Nordrhein-Westfalen", 51.5149, 7.466, false, Seq("Dortmund"), Seq(
    ("CineStar Dortmund", "CineStar Dortmund", "A0029", Some("/kino/dortmund/cinestar-35057"), "Dortmund"),
    ("Filmbühne Zur Postkutsche", "Filmbühne Zur Postkutsche", "A0490", Some("/kino/dortmund/filmbuehne-zur-postkutsche-30028"), "Dortmund"),
    ("Kino im U", "Kino im U", "A2615", Some("/kino/dortmund/kino-im-u-80817"), "Dortmund"),
    ("Open Air Kino im Stadion", "Open Air Kino im Stadion", "A1898", None, "Dortmund"),
    ("PSD Bank Autokino Dortmund", "PSD Bank Autokino Dortmund", "A2947", None, "Dortmund"),
    ("Roxy Dortmund", "Roxy Dortmund", "A1088", None, "Dortmund"),
    ("Schauburg Dortmund", "Schauburg Dortmund", "A1122", Some("/kino/dortmund/lichtspiel-und-kunsttheater-schauburg-252"), "Dortmund"),
    ("sweetSixteen-Kino", "sweetSixteen-Kino", "A2590", Some("/kino/dortmund/sweetsixteen-76817"), "Dortmund")
  ))
  private def p_dresden: R = ("dresden", "dresden-sachsen", "Dresden", "Sachsen", 51.0509, 13.7383, false, Seq("Dresden"), Seq(
    ("CinemaxX Dresden", "CinemaxX Dresden", "A0285", Some("/kino/dresden/cinemaxx-42197"), "Dresden"),
    ("Cineplex Kristallpalast Dresden", "Cineplex Kristallpalast Dresden", "A1222", Some("/kino/dresden/cineplex-kristallpalast-31503"), "Dresden"),
    ("CLUB PASSAGE", "CLUB PASSAGE", "A0410", Some("/kino/dresden/club-passage-31498"), "Dresden"),
    ("Filmnächte am Elbufer", "Filmnächte am Elbufer", "A1801", Some("/kino/dresden/filmnaechte-am-elbufer-31486"), "Dresden"),
    ("Filmtheater Schauburg", "Filmtheater Schauburg", "A1477", Some("/kino/dresden/filmtheater-schauburg-31491"), "Dresden"),
    ("Kino im Kasten - Das Studentenkino", "Kino im Kasten - Das Studentenkino", "A0723", Some("/kino/dresden/kino-im-kasten-das-studentenkino-31501"), "Dresden"),
    ("Kino in der Scheune Dresden", "Kino in der Scheune Dresden", "A1700", Some("/kino/dresden/kino-in-der-scheune-39501"), "Dresden"),
    ("Programmkino Ost", "Programmkino Ost", "A1045", Some("/kino/dresden/programmkino-ost-31492"), "Dresden"),
    ("Rundkino Dresden", "Rundkino Dresden", "A2018", Some("/kino/dresden/rundkino-dresden-71841"), "Dresden"),
    ("Thalia Dresden", "Thalia Dresden", "A1173", Some("/kino/dresden/thalia-69577"), "Dresden"),
    ("UCI Elbe Park", "UCI Elbe Park", "A1208", Some("/kino/dresden/uci-kinowelt-elbe-park-40872"), "Dresden"),
    ("Zentralkino Dresden", "Zentralkino Dresden", "G011F", Some("/kino/dresden/zentralkino-92541"), "Dresden")
  ))
  private def p_bremen: R = ("bremen", "bremen-bremen", "Bremen", "Bremen", 53.0758, 8.8072, false, Seq("Bremen"), Seq(
    ("Cinema Ostertor", "Cinema Ostertor", "A1366", Some("/kino/bremen/cinema-im-ostertor-32358"), "Bremen"),
    ("CinemaxX Bremen", "CinemaxX Bremen", "A0274", Some("/kino/bremen/cinemaxx-35447"), "Bremen"),
    ("Cineplex Cinespace Bremen", "Cineplex Cinespace Bremen", "A0326", Some("/kino/bremen/cineplex-cinespace-61677"), "Bremen"),
    ("CineStar Bremen - Kristall-Palast", "CineStar Bremen - Kristall-Palast", "A0376", Some("/kino/bremen/cinestar-kristallpalast-38717"), "Bremen"),
    ("CITY 46 / Kommunalkino Bremen e.V.", "CITY 46 / Kommunalkino Bremen e.V.", "A0696", Some("/kino/bremen/city-46kommunalkino-bremen-e.v.-32359"), "Bremen"),
    ("City-Filmtheater", "City-Filmtheater", "A1378", None, "Bremen"),
    ("Filmkunsttheater Atlantis", "Filmkunsttheater Atlantis", "A0066", Some("/kino/bremen/atlantis-filmttheater-32364"), "Bremen"),
    ("Filmkunsttheater Gondel", "Filmkunsttheater Gondel", "A0588", Some("/kino/bremen/gondel-32366"), "Bremen"),
    ("Instituto Cervantes - Bremen", "Instituto Cervantes - Bremen", "A2845", None, "Bremen"),
    ("Schauburg Bremen", "Schauburg Bremen", "A1625", Some("/kino/bremen/schauburg-32361"), "Bremen")
  ))
  private def p_nuernberg: R = ("nuernberg", "nuernberg-bayern", "Nürnberg", "Bayern", 49.4542, 11.0775, false, Seq("Nürnberg"), Seq(
    ("Admiral Filmpalast Nürnberg", "Admiral Filmpalast Nürnberg", "A0661", Some("/kino/nuernberg-mittelfranken/admiral-filmpalast-32906"), "Nürnberg"),
    ("Casablanca Nürnberg", "Casablanca Nürnberg", "A0172", Some("/kino/nuernberg-mittelfranken/casablanca-32914"), "Nürnberg"),
    ("Cinecitta", "Cinecitta", "A0049", Some("/kino/nuernberg-mittelfranken/cinecitta-34569"), "Nürnberg"),
    ("Cinecittá Open Air", "Cinecittá Open Air", "A2779", None, "Nürnberg"),
    ("Filmfabrik KommKino", "Filmfabrik KommKino", "A0854", None, "Nürnberg"),
    ("Filmhauskino im Künstlerhaus", "Filmhauskino im Künstlerhaus", "A1418", Some("/kino/nuernberg-mittelfranken/filmhauskino-67658"), "Nürnberg"),
    ("Meisengeige", "Meisengeige", "A1557", Some("/kino/nuernberg-mittelfranken/meisengeige-32912"), "Nürnberg"),
    ("Metropolis Nürnberg", "Metropolis Nürnberg", "A0956", Some("/kino/nuernberg-mittelfranken/metropolis-32915"), "Nürnberg"),
    ("Open Air Fränkisches Museumseisenbahn", "Open Air Fränkisches Museumseisenbahn", "A2489", None, "Nürnberg"),
    ("Open Air Kino Freilichtbühne Desi", "Open Air Kino Freilichtbühne Desi", "A1981", None, "Nürnberg"),
    ("Open Air Kino Katharinenruine", "Open Air Kino Katharinenruine", "A2179", Some("/kino/nuernberg-mittelfranken/open-air-kino-katharinenruine-73363"), "Nürnberg"),
    ("Open Air Kino Naturgarten Bad", "Open Air Kino Naturgarten Bad", "A2498", Some("/kino/nuernberg-mittelfranken/open-air-naturgartenbad-86881"), "Nürnberg"),
    ("Open Air Kino Pellerhaus", "Open Air Kino Pellerhaus", "A2180", Some("/kino/nuernberg-mittelfranken/open-air-pellerhaus-91281"), "Nürnberg"),
    ("Open Air Krafftscher Hof", "Open Air Krafftscher Hof", "A2500", Some("/kino/nuernberg-mittelfranken/open-air-krafftscher-hof-75946"), "Nürnberg"),
    ("Open Air Radrennbahn am Reichelsdorfer Keller", "Open Air Radrennbahn am Reichelsdorfer Keller", "A1956", None, "Nürnberg"),
    ("Rio Palast", "Rio Palast", "A1604", Some("/kino/nuernberg-mittelfranken/rio-palast-32905"), "Nürnberg"),
    ("Roxy-Fremdsprachenkino", "Roxy-Fremdsprachenkino", "A1092", None, "Nürnberg"),
    ("SommerNachtFilmFestival - Marienbergpark", "SommerNachtFilmFestival - Marienbergpark", "A2822", None, "Nürnberg")
  ))
  private def p_hannover: R = ("hannover", "hannover-niedersachsen", "Hannover", "Niedersachsen", 52.3705, 9.7332, false, Seq("Hannover"), Seq(
    ("Apollo Hannover", "Apollo Hannover", "A0035", Some("/kino/hannover/apollo-32473"), "Hannover"),
    ("Astor Grand Cinema", "Astor Grand Cinema", "A1370", Some("/kino/hannover/astor-grand-cinema-85797"), "Hannover"),
    ("CinemaxX Raschplatz", "CinemaxX Raschplatz", "A0296", Some("/kino/hannover/cinemaxx-raschplatz-42357"), "Hannover"),
    ("Hochhaus Lichtspiele", "Hochhaus Lichtspiele", "A0608", Some("/kino/hannover/hochhaus-lichtspiele-32470"), "Hannover"),
    ("Kino am Raschplatz", "Kino am Raschplatz", "A0590", Some("/kino/hannover/kino-am-raschplatz-60676"), "Hannover"),
    ("Kino im Sprengel", "Kino im Sprengel", "A1015", Some("/kino/hannover/kino-im-sprengel-32469"), "Hannover"),
    ("Kommunales Kino im Künstlerhaus", "Kommunales Kino im Künstlerhaus", "A1495", Some("/kino/hannover/kommunales-kino-im-kuenstlerhaus-32475"), "Hannover"),
    ("Open Air im Theaterhof", "Open Air im Theaterhof", "A2868", None, "Hannover"),
    ("Puschenkino Puki", "Puschenkino Puki", "A2818", None, "Hannover"),
    ("Seh-Fest Gilde Parkbühne", "Seh-Fest Gilde Parkbühne", "A2472", None, "Hannover")
  ))
  private def p_leipzig: R = ("leipzig", "leipzig-sachsen", "Leipzig", "Sachsen", 51.3396, 12.3713, false, Seq("Leipzig"), Seq(
    ("Cineding", "Cineding", "A0225", Some("/kino/leipzig/cineding-56717"), "Leipzig"),
    ("CineStar Leipzig", "CineStar Leipzig", "A0345", Some("/kino/leipzig/cinestar-der-filmpalast-43338"), "Leipzig"),
    ("Cinémathèque in der naTo Leipzig e.V.", "Cinémathèque in der naTo Leipzig e.V.", "A0445", Some("/kino/leipzig/cin%C3%A9math%C3%A8que-in-der-nato-leipzig-e.v.-65357"), "Leipzig"),
    ("Filmclub KassaBlanka", "Filmclub KassaBlanka", "A2361", None, "Leipzig"),
    ("Freilichtkino auf der Pferderennbahn", "Freilichtkino auf der Pferderennbahn", "A0561", None, "Leipzig"),
    ("Kinobar &quot;Prager Frühling&quot;", "Kinobar &quot;Prager Frühling&quot;", "A1500", Some("/kino/leipzig/kinobar-%22prager-fruehling%22-33663"), "Leipzig"),
    ("Luru Kino in der Spinnerei", "Luru Kino in der Spinnerei", "A2591", Some("/kino/leipzig/lurukino-in-der-spinnerei-76897"), "Leipzig"),
    ("Passage Kinos Leipzig", "Passage Kinos Leipzig", "A1037", Some("/kino/leipzig/passage-kinos-36692"), "Leipzig"),
    ("Regina-Palast", "Regina-Palast", "A1056", Some("/kino/leipzig/reginapalast-31586"), "Leipzig"),
    ("Schauburg Leipzig", "Schauburg Leipzig", "A2586", Some("/kino/leipzig/schauburg-31585"), "Leipzig"),
    ("Schaubühne Lindenfels", "Schaubühne Lindenfels", "A1116", Some("/kino/leipzig/schaubuehne-lindenfels-31581"), "Leipzig"),
    ("Sommerkino Schauportal", "Sommerkino Schauportal", "A1858", None, "Leipzig"),
    ("Cineplex Leipzig", "Cineplex Leipzig", "A0516", None, "Leipzig"),
    ("Ostpassagen Theater", "Ostpassagen Theater", "cineamo:1235", None, "Leipzig")
  ))
  private def p_duisburg: R = ("duisburg", "duisburg-nordrhein-westfalen", "Duisburg", "Nordrhein-Westfalen", 51.4325, 6.7652, false, Seq("Duisburg"), Seq(
    ("Filmforum", "Filmforum", "A2750", Some("/kino/duisburg/filmforum-298"), "Duisburg"),
    ("Stadtwerke Sommerkino", "Stadtwerke Sommerkino", "A1862", Some("/kino/duisburg/stadtwerke-sommerkino-71280"), "Duisburg"),
    ("UCI Duisburg", "UCI Duisburg", "A0663", Some("/kino/duisburg/uci-kinowelt-duisburg-35240"), "Duisburg")
  ))
  private def p_bochum: R = ("bochum", "bochum-nordrhein-westfalen", "Bochum", "Nordrhein-Westfalen", 51.4817, 7.2165, false, Seq("Bochum"), Seq(
    ("Capitol Bochum", "Capitol Bochum", "A0104", None, "Bochum"),
    ("Casablanca Bochum", "Casablanca Bochum", "A0175", None, "Bochum"),
    ("Endstation Kino", "Endstation Kino", "A0467", None, "Bochum"),
    ("Metropolis Bochum", "Metropolis Bochum", "A0957", None, "Bochum"),
    ("UCI Bochum", "UCI Bochum", "A1216", None, "Bochum"),
    ("Union Bochum", "Union Bochum", "A1231", None, "Bochum")
  ))
  private def p_wuppertal: R = ("wuppertal", "wuppertal-nordrhein-westfalen", "Wuppertal", "Nordrhein-Westfalen", 51.2563, 7.1482, false, Seq("Wuppertal"), Seq(
    ("Cinema Wuppertal", "Cinema Wuppertal", "A0381", Some("/kino/wuppertal/cinema-13463"), "Wuppertal"),
    ("CinemaxX Wuppertal", "CinemaxX Wuppertal", "A0292", Some("/kino/wuppertal/cinemaxx-35200"), "Wuppertal"),
    ("Rex Wuppertal", "Rex Wuppertal", "A2680", Some("/kino/wuppertal/rex-86017"), "Wuppertal"),
    ("Talflimmern Open-Air-Kino", "Talflimmern Open-Air-Kino", "A1918", Some("/kino/wuppertal/talflimmern-openairkino-alte-feuerwache-58797"), "Wuppertal")
  ))
  private def p_bielefeld: R = ("bielefeld", "bielefeld-nordrhein-westfalen", "Bielefeld", "Nordrhein-Westfalen", 52.0333, 8.5333, false, Seq("Bielefeld"), Seq(
    ("AJZ Kino", "AJZ Kino", "A0010", Some("/kino/bielefeld/ajz-kino-61137"), "Bielefeld"),
    ("CinemaxX Bielefeld", "CinemaxX Bielefeld", "A0271", Some("/kino/bielefeld/cinemaxx-37117"), "Bielefeld"),
    ("Kamera Bielefeld", "Kamera Bielefeld", "A0634", Some("/kino/bielefeld/kamera-31762"), "Bielefeld"),
    ("Lichtwerk im Ravensberger Park", "Lichtwerk im Ravensberger Park", "A0915", Some("/kino/bielefeld/lichtwerk-im-ravensberger-park-31756"), "Bielefeld"),
    ("Melodie Filmtheater", "Melodie Filmtheater", "A2363", Some("/kino/bielefeld/melodie-filmtheater-75017"), "Bielefeld"),
    ("Offkino Im Filmhaus Bielefeld", "Offkino Im Filmhaus Bielefeld", "A2838", Some("/kino/bielefeld/offkino-im-filmhaus-78518"), "Bielefeld"),
    ("Open Air Kino Luna im Ravensberger Park", "Open Air Kino Luna im Ravensberger Park", "A2278", Some("/kino/bielefeld/open-air-kino-luna-im-ravensberger-park-36308"), "Bielefeld")
  ))
  private def p_bonn: R = ("bonn", "bonn-nordrhein-westfalen", "Bonn", "Nordrhein-Westfalen", 50.7344, 7.0955, false, Seq("Bonn"), Seq(
    ("Arkadenhof der Bonner Universität - Am Hof", "Arkadenhof der Bonner Universität - Am Hof", "A2067", Some("/kino/bonn/arkadenhof-der-bonner-universitaet-am-hof-55937"), "Bonn"),
    ("Bundeskunsthalle", "Bundeskunsthalle", "A2638", Some("/kino/bonn/bundeskunsthalle-81737"), "Bonn"),
    ("CineStar Bonn - Sternlichtspiele", "CineStar Bonn - Sternlichtspiele", "A0417", Some("/kino/bonn/cinestar-bonn-sternenlichtspiele-61716"), "Bonn"),
    ("Kino in der Brotfabrik", "Kino in der Brotfabrik", "A0736", Some("/kino/bonn/kino-in-der-brotfabrik-31768"), "Bonn"),
    ("Neue Filmbühne Bonn", "Neue Filmbühne Bonn", "A0982", Some("/kino/bonn/neue-filmbuehne-31770"), "Bonn"),
    ("Rex-Lichtspieltheater", "Rex-Lichtspieltheater", "A1073", Some("/kino/bonn/rexlichtspieltheater-31777"), "Bonn"),
    ("Woki", "Woki", "A1282", Some("/kino/bonn/woki-31772"), "Bonn"),
    ("Arthaus-Kino im Rheinischen Landesmuseum LVR Bonn", "Arthaus-Kino im Rheinischen Landesmuseum LVR Bonn", "kinoheld:1461", None, "Bonn"),
    ("Kinopolis Bonn-Bad Godesberg", "Kinopolis Bonn-Bad Godesberg", "A0030", None, "Bonn")
  ))
  private def p_muenster: R = ("muenster", "muenster-nordrhein-westfalen", "Münster", "Nordrhein-Westfalen", 51.9624, 7.6257, false, Seq("Münster"), Seq(
    ("Cinema &amp; Kurbelkiste Münster", "Cinema &amp; Kurbelkiste Münster", "A0243", Some("/kino/muenster/cinema-und-kurbelkiste-31879"), "Münster"),
    ("Cineplex Münster", "Cineplex Münster", "A0305", Some("/kino/muenster/cineplex-49196"), "Münster"),
    ("Kaisersaal-Lichtspiele", "Kaisersaal-Lichtspiele", "A0633", None, "Münster"),
    ("Schlosstheater Münster", "Schlosstheater Münster", "A1130", Some("/kino/muenster/schlosstheater-31880"), "Münster")
  ))
  private def p_mannheim: R = ("mannheim", "mannheim-baden-wuerttemberg", "Mannheim", "Baden-Württemberg", 49.4891, 8.4669, false, Seq("Mannheim"), Seq(
    ("Atlantis Mannheim", "Atlantis Mannheim", "A1305", Some("/kino/mannheim-universitaetsstadt/atlantis-32168"), "Mannheim"),
    ("Cinema Quadrat e.V.", "Cinema Quadrat e.V.", "A1368", Some("/kino/mannheim-universitaetsstadt/cinema-quadrat-e.v-32170"), "Mannheim"),
    ("Cineplex Mannheim", "Cineplex Mannheim", "A0270", Some("/kino/mannheim-universitaetsstadt/cineplex-37050"), "Mannheim"),
    ("Odeon Mannheim", "Odeon Mannheim", "A1002", Some("/kino/mannheim-universitaetsstadt/odeon-32169"), "Mannheim"),
    ("Planken Lichtspiele Mannheim", "Planken Lichtspiele Mannheim", "A1376", Some("/kino/mannheim-universitaetsstadt/planken-lichtspiele-32172"), "Mannheim"),
    ("Kino im Stadthaus", "Kino im Stadthaus", "kinoheld:3501", None, "Mannheim")
  ))
  private def p_augsburg: R = ("augsburg", "augsburg-bayern", "Augsburg", "Bayern", 48.3715, 10.8985, false, Seq("Augsburg"), Seq(
    ("CinemaxX Augsburg", "CinemaxX Augsburg", "A0276", Some("/kino/augsburg-bayern/cinemaxx-39119"), "Augsburg"),
    ("CineStar Augsburg", "CineStar Augsburg", "A0350", Some("/kino/augsburg-bayern/cinestar-37095"), "Augsburg"),
    ("Liliom", "Liliom", "A1541", Some("/kino/augsburg-bayern/liliom-32675"), "Augsburg"),
    ("Open Air Kino Lechflimmern Familienbad am Plärrer", "Open Air Kino Lechflimmern Familienbad am Plärrer", "A1800", Some("/kino/augsburg-bayern/open-air-kino-lechflimmern-familienbad-am-plaerrer-78016"), "Augsburg"),
    ("Savoy Kino", "Savoy Kino", "A1105", Some("/kino/augsburg-bayern/savoy-kino-32677"), "Augsburg"),
    ("Thalia Augsburg", "Thalia Augsburg", "A1645", Some("/kino/augsburg-bayern/thalia-32673"), "Augsburg")
  ))
  private def p_wiesbaden: R = ("wiesbaden", "wiesbaden-hessen", "Wiesbaden", "Hessen", 50.086, 8.2444, false, Seq("Wiesbaden"), Seq(
    ("Caligari FilmBühne Wiesbaden", "Caligari FilmBühne Wiesbaden", "A0134", Some("/kino/wiesbaden/caligari-filmbuehne-40177"), "Wiesbaden"),
    ("Cineplex Apollo-Center Wiesbaden", "Cineplex Apollo-Center Wiesbaden", "A0019", None, "Wiesbaden"),
    ("Cineplex Arkaden Wiesbaden", "Cineplex Arkaden Wiesbaden", "A0007", None, "Wiesbaden"),
    ("Filme im Schloss", "Filme im Schloss", "A2320", Some("/kino/wiesbaden/filme-im-schloss-74457"), "Wiesbaden"),
    ("Murnau-Filmtheater", "Murnau-Filmtheater", "A2631", Some("/kino/wiesbaden/murnaufilmtheater-77605"), "Wiesbaden"),
    ("Open Air Kino Reisinger Anlage", "Open Air Kino Reisinger Anlage", "A2424", Some("/kino/wiesbaden/open-air-kino-in-den-reisingeranlagen-75457"), "Wiesbaden")
  ))
  private def p_karlsruhe: R = ("karlsruhe", "karlsruhe-baden-wuerttemberg", "Karlsruhe", "Baden-Württemberg", 49.0094, 8.4044, false, Seq("Karlsruhe"), Seq(
    ("Die Kurbel Karlsruhe", "Die Kurbel Karlsruhe", "A2614", Some("/kino/karlsruhe-baden/die-kurbel-78682"), "Karlsruhe"),
    ("Filmpalast am ZKM - Karlsruhe", "Filmpalast am ZKM - Karlsruhe", "A0513", Some("/kino/karlsruhe-baden/filmpalast-am-zkm-39797"), "Karlsruhe"),
    ("Kinemathek Karlsruhe", "Kinemathek Karlsruhe", "A1711", Some("/kino/karlsruhe-baden/kinemathek-48097"), "Karlsruhe"),
    ("Open Air Kino am Schloß Gottesaue", "Open Air Kino am Schloß Gottesaue", "A1976", Some("/kino/karlsruhe-baden/open-air-kino-am-schloss-gottesaue-34538"), "Karlsruhe"),
    ("Schauburg Karlsruhe", "Schauburg Karlsruhe", "A1626", Some("/kino/karlsruhe-baden/schauburg-32107"), "Karlsruhe"),
    ("Universum-City Karlsruhe", "Universum-City Karlsruhe", "A1246", Some("/kino/karlsruhe-baden/universumcitykinocenter-34235"), "Karlsruhe")
  ))
  private def p_gelsenkirchen: R = ("gelsenkirchen", "gelsenkirchen-nordrhein-westfalen", "Gelsenkirchen", "Nordrhein-Westfalen", 51.5051, 7.0965, false, Seq("Gelsenkirchen"), Seq(
    ("Multiplex Gelsenkirchen", "Multiplex Gelsenkirchen", "A1260", Some("/kino/gelsenkirchen/apollo-cinemas-multiplex-gelsenkirchen-496"), "Gelsenkirchen"),
    ("Schauburg Filmpalast", "Schauburg Filmpalast", "A1123", Some("/kino/gelsenkirchen/schauburg-filmpalast-489"), "Gelsenkirchen")
  ))
  private def p_aachen: R = ("aachen", "aachen-nordrhein-westfalen", "Aachen", "Nordrhein-Westfalen", 50.7766, 6.0834, false, Seq("Aachen"), Seq(
    ("Apollo Aachen", "Apollo Aachen", "A0036", None, "Aachen"),
    ("Capitol Aachen", "Capitol Aachen", "A0150", None, "Aachen"),
    ("Cineplex Aachen", "Cineplex Aachen", "A0227", None, "Aachen"),
    ("Eden Palast Aachen", "Eden Palast Aachen", "A0456", None, "Aachen"),
    ("Filmstudio an der RWTH", "Filmstudio an der RWTH", "A0530", None, "Aachen"),
    ("Raststätte", "Raststätte", "kinoheld:2417", None, "Aachen")
  ))
  private def p_moenchengladbach: R = ("moenchengladbach", "moenchengladbach-nordrhein-westfalen", "Mönchengladbach", "Nordrhein-Westfalen", 51.1854, 6.4417, false, Seq("Mönchengladbach"), Seq(
    ("Cinefactory im Haus Zoar", "Cinefactory im Haus Zoar", "A2651", Some("/kino/moenchengladbach/cinefactory-im-haus-zoar-84518"), "Mönchengladbach"),
    ("Comet-Cine-Center", "Comet-Cine-Center", "A0419", Some("/kino/moenchengladbach/comet-cine-center-36603"), "Mönchengladbach")
  ))
  private def p_kiel: R = ("kiel", "kiel-schleswig-holstein", "Kiel", "Schleswig-Holstein", 54.3213, 10.1349, false, Seq("Kiel"), Seq(
    ("CinemaxX Kiel", "CinemaxX Kiel", "A0279", Some("/kino/kiel/cinemaxx-32509"), "Kiel"),
    ("Hansafilmpalast", "Hansafilmpalast", "A2842", None, "Kiel"),
    ("Kommunales Kino in der Pumpe", "Kommunales Kino in der Pumpe", "A1522", Some("/kino/kiel/kommunales-kino-in-der-pumpe-32510"), "Kiel"),
    ("Metro-Kino im Schloßhof", "Metro-Kino im Schloßhof", "A1750", Some("/kino/kiel/metrokino-im-schlosshof-72536"), "Kiel"),
    ("Studio - Filmtheater am Dreiecksplatz", "Studio - Filmtheater am Dreiecksplatz", "A0993", Some("/kino/kiel/studio-filmtheater-am-dreiecksplatz-44339"), "Kiel"),
    ("Traum-Kino", "Traum-Kino", "A0694", Some("/kino/kiel/traumkino-32511"), "Kiel")
  ))
  private def p_chemnitz: R = ("chemnitz", "chemnitz-sachsen", "Chemnitz", "Sachsen", 50.8357, 12.9292, false, Seq("Chemnitz"), Seq(
    ("Campusfilmnächte auf dem Sportplatz", "Campusfilmnächte auf dem Sportplatz", "A2831", None, "Chemnitz"),
    ("CineStar Chemnitz - am Roten Turm", "CineStar Chemnitz - am Roten Turm", "A0365", Some("/kino/chemnitz-sachsen/cinestar-der-filmpalast-am-roten-turm-47696"), "Chemnitz"),
    ("Clubkino Siegmar", "Clubkino Siegmar", "A0415", Some("/kino/chemnitz-sachsen/clubkino-siegmar-57856"), "Chemnitz"),
    ("Filmclub Mittendrin", "Filmclub Mittendrin", "A2356", Some("/kino/chemnitz-sachsen/filmclub-mittendrin-74977"), "Chemnitz"),
    ("Filmnächte auf dem Theaterplatz", "Filmnächte auf dem Theaterplatz", "A2645", Some("/kino/chemnitz-sachsen/filmnaechte-auf-dem-theaterplatz-79319"), "Chemnitz"),
    ("Metropol Chemnitz", "Metropol Chemnitz", "A0951", Some("/kino/chemnitz-sachsen/metropol-31469"), "Chemnitz"),
    ("Weltecho", "Weltecho", "A2756", Some("/kino/chemnitz-sachsen/weltecho-93080"), "Chemnitz")
  ))
  private def p_braunschweig: R = ("braunschweig", "braunschweig-niedersachsen", "Braunschweig", "Niedersachsen", 52.2659, 10.5267, false, Seq("Braunschweig"), Seq(
    ("C1 Cinema Braunschweig", "C1 Cinema Braunschweig", "A0284", None, "Braunschweig"),
    ("Roter Saal", "Roter Saal", "A2389", Some("/kino/braunschweig/roter-saal-75108"), "Braunschweig"),
    ("SchunterKino", "SchunterKino", "A2745", Some("/kino/braunschweig/schunterkino-88539"), "Braunschweig"),
    ("Universum Filmtheater", "Universum Filmtheater", "A1240", Some("/kino/braunschweig/kino-universum-53716"), "Braunschweig")
  ))
  private def p_magdeburg: R = ("magdeburg", "magdeburg-sachsen-anhalt", "Magdeburg", "Sachsen-Anhalt", 52.1313, 11.6319, false, Seq("Magdeburg"), Seq(
    ("CinemaxX Magdeburg", "CinemaxX Magdeburg", "A0293", Some("/kino/magdeburg/cinemaxx-31600"), "Magdeburg"),
    ("CineStar Magdeburg", "CineStar Magdeburg", "A1259", Some("/kino/magdeburg/cinestar-36260"), "Magdeburg"),
    ("Kulturzentrum auf dem Moritzhof", "Kulturzentrum auf dem Moritzhof", "A1709", Some("/kino/magdeburg/kulturzentrum-moritzhof-71957"), "Magdeburg"),
    ("Oli Lichtspiele", "Oli Lichtspiele", "A1708", None, "Magdeburg"),
    ("Studio-Kino", "Studio-Kino", "A1165", Some("/kino/magdeburg/studiokino-43159"), "Magdeburg")
  ))
  private def p_krefeld: R = ("krefeld", "krefeld-nordrhein-westfalen", "Krefeld", "Nordrhein-Westfalen", 51.3364, 6.5538, false, Seq("Krefeld"), Seq(
    ("CinemaxX Krefeld", "CinemaxX Krefeld", "A0278", Some("/kino/krefeld/cinemaxx-34929"), "Krefeld"),
    ("Fabrik Heeder", "Fabrik Heeder", "A0594", Some("/kino/krefeld/fabrik-heeder-927"), "Krefeld"),
    ("Open Air Kino Krefelder Rennbahn", "Open Air Kino Krefelder Rennbahn", "A2812", None, "Krefeld"),
    ("Primus-Palast", "Primus-Palast", "A0242", Some("/kino/krefeld/primus-palast-43576"), "Krefeld")
  ))
  private def p_halle: R = ("halle", "halle-sachsen-anhalt", "Halle", "Sachsen-Anhalt", 51.4816, 11.9795, false, Seq("Halle"), Seq(
    ("CinemaxX Halle-Charlottencenter", "CinemaxX Halle-Charlottencenter", "A0280", Some("/kino/halle-saale/cinemaxx-hallecharlottencenter-31556"), "Halle"),
    ("Luchs Kino am Zoo", "Luchs Kino am Zoo", "A0931", Some("/kino/halle-saale/luchs.kino-am-zoo-43039"), "Halle"),
    ("Prisma Cinema", "Prisma Cinema", "A0295", Some("/kino/halle-saale/prisma-cinema-53096"), "Halle"),
    ("PUSCHKINO", "PUSCHKINO", "A0932", Some("/kino/halle-saale/puschkino-62017"), "Halle"),
    ("Zazie", "Zazie", "A1286", Some("/kino/halle-saale/zazie-62577"), "Halle")
  ))
  private def p_freiburg: R = ("freiburg", "freiburg-baden-wuerttemberg", "Freiburg", "Baden-Württemberg", 47.9959, 7.8522, false, Seq("Freiburg"), Seq(
    ("aka-Filmclub", "aka-Filmclub", "A1474", None, "Freiburg"),
    ("CinemaxX Freiburg", "CinemaxX Freiburg", "A0291", Some("/kino/freiburg-im-breisgau/cinemaxx-35079"), "Freiburg"),
    ("Friedrichsbau-Apollo", "Friedrichsbau-Apollo", "A0040", Some("/kino/freiburg-im-breisgau/friedrichsbauapollo-65198"), "Freiburg"),
    ("Harmonie Freiburg", "Harmonie Freiburg", "A1218", Some("/kino/freiburg-im-breisgau/harmonie-34481"), "Freiburg"),
    ("Kandelhof", "Kandelhof", "A0644", Some("/kino/freiburg-im-breisgau/kandelhof-32046"), "Freiburg"),
    ("Kommunales Kino - Im Alten Wiehrebahnhof", "Kommunales Kino - Im Alten Wiehrebahnhof", "A0862", None, "Freiburg"),
    ("Sommernachts-Kino", "Sommernachts-Kino", "A2036", None, "Freiburg")
  ))
  private def p_mainz: R = ("mainz", "mainz-rheinland-pfalz", "Mainz", "Rheinland-Pfalz", 49.9819, 8.2801, false, Seq("Mainz"), Seq(
    ("Capitol Arthouse Mainz", "Capitol Arthouse Mainz", "A1331", Some("/kino/mainz-am-rhein/capitol-32160"), "Mainz"),
    ("CineStar Mainz", "CineStar Mainz", "A0352", None, "Mainz"),
    ("Cinémayence", "Cinémayence", "A0018", Some("/kino/mainz-am-rhein/cin%C3%A9mayence-32158"), "Mainz"),
    ("KlubKino", "KlubKino", "A2853", None, "Mainz"),
    ("Open Air im Kurfürstlichen Schloss", "Open Air im Kurfürstlichen Schloss", "A2866", None, "Mainz")
  ))
  private def p_oberhausen: R = ("oberhausen", "oberhausen-nordrhein-westfalen", "Oberhausen", "Nordrhein-Westfalen", 51.4781, 6.8625, false, Seq("Oberhausen"), Seq(
    ("CineStar Oberhausen - Filmpalast im Centro", "CineStar Oberhausen - Filmpalast im Centro", "A1261", Some("/kino/oberhausen/cinestar-der-filmpalast-oberhausen-im-centro-30754"), "Oberhausen"),
    ("Kino im Druckluft", "Kino im Druckluft", "A0719", Some("/kino/oberhausen/kino-im-druckluft-45421"), "Oberhausen"),
    ("Lichtburg-Filmpalast", "Lichtburg-Filmpalast", "A0905", Some("/kino/oberhausen/lichtburg-filmpalast-31021"), "Oberhausen"),
    ("Walzenlager-Zentrum Altenberg", "Walzenlager-Zentrum Altenberg", "A0733", Some("/kino/oberhausen/kino-im-walzenlager-zentrum-altenberg-31106"), "Oberhausen")
  ))
  private def p_erfurt: R = ("erfurt", "erfurt-thueringen", "Erfurt", "Thüringen", 50.9773, 11.0354, false, Seq("Erfurt"), Seq(
    ("CineStar Erfurt", "CineStar Erfurt", "A0344", Some("/kino/erfurt/cinestar-der-filmpalast-43336"), "Erfurt"),
    ("Kinoklub am Hirschlachufer", "Kinoklub am Hirschlachufer", "A0778", Some("/kino/erfurt/kinoklub-am-hirschlachufer-31514"), "Erfurt"),
    ("Open Air im Kulturhof Krönbacken", "Open Air im Kulturhof Krönbacken", "A1829", None, "Erfurt")
  ))
  private def p_luebeck: R = ("luebeck", "luebeck-schleswig-holstein", "Lübeck", "Schleswig-Holstein", 53.8689, 10.6873, false, Seq("Lübeck"), Seq(
    ("CineStar Lübeck - Filmhaus", "CineStar Lübeck - Filmhaus", "A0498", Some("/kino/luebeck/cinestar-filmhaus-32535"), "Lübeck"),
    ("CineStar Lübeck - Stadthalle", "CineStar Lübeck - Stadthalle", "A0367", Some("/kino/luebeck/cinestar-stadthalle-32534"), "Lübeck"),
    ("Kino Koki - Kommunales Kino Lübeck", "Kino Koki - Kommunales Kino Lübeck", "A2241", Some("/kino/luebeck/kino-koki-kommunales-kino-32532"), "Lübeck")
  ))
  private def p_hagen: R = ("hagen", "hagen-nordrhein-westfalen", "Hagen", "Nordrhein-Westfalen", 51.3608, 7.4717, false, Seq("Hagen"), Seq(
    ("Babylon Hagen", "Babylon Hagen", "A0705", Some("/kino/hagen/kino-babylon-im-kulturzentrum-pelmke-38405"), "Hagen"),
    ("CineStar Hagen", "CineStar Hagen", "A0341", Some("/kino/hagen/cinestar-35181"), "Hagen")
  ))
  private def p_rostock: R = ("rostock", "rostock-mecklenburg-vorpommern", "Rostock", "Mecklenburg-Vorpommern", 54.0887, 12.1405, false, Seq("Rostock"), Seq(
    ("CineStar Rostock - Capitol", "CineStar Rostock - Capitol", "A0370", Some("/kino/rostock/cinestar-capitol-31653"), "Rostock"),
    ("CineStar Rostock - Lütten Klein", "CineStar Rostock - Lütten Klein", "A0358", None, "Rostock"),
    ("Lichtspieltheater Wundervoll (Frieda 23)", "Lichtspieltheater Wundervoll (Frieda 23)", "A2649", None, "Rostock"),
    ("Lichtspieltheater Wundervoll (Metropol)", "Lichtspieltheater Wundervoll (Metropol)", "A0914", None, "Rostock")
  ))
  private def p_kassel: R = ("kassel", "kassel-hessen", "Kassel", "Hessen", 51.3167, 9.5, false, Seq("Kassel"), Seq(
    ("Bali-Kinos im KulturBahnhof Kassel", "Bali-Kinos im KulturBahnhof Kassel", "A0078", None, "Kassel"),
    ("Cineplex Capitol Kassel", "Cineplex Capitol Kassel", "A1375", Some("/kino/kassel-hessen/cineplex-capitol-32116"), "Kassel"),
    ("Filmladen Kassel e.V.", "Filmladen Kassel e.V.", "A0505", Some("/kino/kassel-hessen/filmladen-32114"), "Kassel"),
    ("Filmpalast Kassel", "Filmpalast Kassel", "A0354", Some("/kino/kassel-hessen/filmpalast-39197"), "Kassel"),
    ("GLORIA-Kino am Ständeplatz", "GLORIA-Kino am Ständeplatz", "A0577", Some("/kino/kassel-hessen/gloria-32118"), "Kassel"),
    ("Kasseler Open Air Sommerfilm", "Kasseler Open Air Sommerfilm", "A2063", None, "Kassel")
  ))
  private def p_potsdam: R = ("potsdam", "potsdam-brandenburg", "Potsdam", "Brandenburg", 52.3989, 13.0657, false, Seq("Potsdam"), Seq(
    ("Filmmuseum Potsdam", "Filmmuseum Potsdam", "A0506", Some("/kino/potsdam/filmmuseum-31635"), "Potsdam"),
    ("Freilichtkino in der Russischen Kolonie Alexandrowka", "Freilichtkino in der Russischen Kolonie Alexandrowka", "A2152", None, "Potsdam"),
    ("Freiluftkino Waschhaus", "Freiluftkino Waschhaus", "A1811", Some("/kino/potsdam/freiluftkino-waschhaus-67017"), "Potsdam"),
    ("Inselkino auf der Freundschaftsinsel", "Inselkino auf der Freundschaftsinsel", "A1932", None, "Potsdam"),
    ("Thalia Arthouse-Kino", "Thalia Arthouse-Kino", "A1175", Some("/kino/potsdam/thalia-arthousekino-babelsberg-52736"), "Potsdam"),
    ("UCI Potsdam", "UCI Potsdam", "A1206", Some("/kino/potsdam/uci-kinowelt-potsdam-43185"), "Potsdam")
  ))
  private def p_saarbruecken: R = ("saarbruecken", "saarbruecken-saarland", "Saarbrücken", "Saarland", 49.2326, 7.0098, false, Seq("Saarbrücken"), Seq(
    ("camera zwo - das arthouse kino", "camera zwo - das arthouse kino", "A0137", Some("/kino/saarbruecken/camera-zwo-das-arthouse-kino-51537"), "Saarbrücken"),
    ("CineStar Saarbrücken", "CineStar Saarbrücken", "A0340", Some("/kino/saarbruecken/cinestar-saarbruecken-42137"), "Saarbrücken"),
    ("kino achteinhalb", "kino achteinhalb", "A0698", Some("/kino/saarbruecken/kino-achteinhalb-34564"), "Saarbrücken"),
    ("Kino im Filmhaus", "Kino im Filmhaus", "A1125", Some("/kino/saarbruecken/filmhaus-32232"), "Saarbrücken"),
    ("Passage Saarbrücken", "Passage Saarbrücken", "A1586", Some("/kino/saarbruecken/passage-32234"), "Saarbrücken"),
    ("Unifilm im AudiMax", "Unifilm im AudiMax", "A2323", Some("/kino/saarbruecken/unifilm-im-audimax-74517"), "Saarbrücken"),
    ("UT-Kino-Center", "UT-Kino-Center", "A1249", Some("/kino/saarbruecken/utkinocenter-32236"), "Saarbrücken")
  ))
  private def p_hamm: R = ("hamm", "hamm-nordrhein-westfalen", "Hamm", "Nordrhein-Westfalen", 51.6803, 7.8209, false, Seq("Hamm"), Seq(
    ("Cineplex Hamm", "Cineplex Hamm", "cineamo:167", None, "Hamm")
  ))
  private def p_muelheim_an_der_ruhr: R = ("muelheim-an-der-ruhr", "muelheim-an-der-ruhr-nordrhein-westfalen", "Mülheim an der Ruhr", "Nordrhein-Westfalen", 51.4322, 6.8797, false, Seq("Mülheim an der Ruhr"), Seq(
    ("CinemaxX Mülheim", "CinemaxX Mülheim", "A0277", Some("/kino/muelheim-an-der-ruhr/cinemaxx-37082"), "Mülheim an der Ruhr"),
    ("Ringlokschuppen Open-Air-Kino auf der Drehscheibe", "Ringlokschuppen Open-Air-Kino auf der Drehscheibe", "A1870", Some("/kino/muelheim-an-der-ruhr/open-air-kino-ringlokschuppendrehscheibe-63077"), "Mülheim an der Ruhr"),
    ("Rio im Medienhaus", "Rio im Medienhaus", "A1082", Some("/kino/muelheim-an-der-ruhr/rio-filmtheater-im-medienhaus-27"), "Mülheim an der Ruhr")
  ))
  private def p_herne: R = ("herne", "herne-nordrhein-westfalen", "Herne", "Nordrhein-Westfalen", 51.5388, 7.2257, false, Seq("Herne"), Seq(
    ("Filmwelt Herne", "Filmwelt Herne", "A0547", Some("/kino/herne-westfalen/filmwelt-60717"), "Herne"),
    ("Open-Air Kino Schloss Strünkede", "Open-Air Kino Schloss Strünkede", "A2008", None, "Herne")
  ))
  private def p_darmstadt: R = ("darmstadt", "darmstadt-hessen", "Darmstadt", "Hessen", 49.8717, 8.6503, false, Seq("Darmstadt"), Seq(
    ("Citydome Darmstadt", "Citydome Darmstadt", "A0483", Some("/kino/darmstadt/citydome-32006"), "Darmstadt"),
    ("Kinopolis Darmstadt", "Kinopolis Darmstadt", "A0268", Some("/kino/darmstadt/kinopolis-39358"), "Darmstadt"),
    ("programmkino rex", "programmkino rex", "A1597", None, "Darmstadt"),
    ("Studentischer Filmkreis im Audimaxx der TU Darmstadt", "Studentischer Filmkreis im Audimaxx der TU Darmstadt", "A2665", None, "Darmstadt")
  ))
  private def p_osnabrueck: R = ("osnabrueck", "osnabrueck-niedersachsen", "Osnabrück", "Niedersachsen", 52.2726, 8.0498, false, Seq("Osnabrück"), Seq(
    ("Cinema-Arthouse", "Cinema-Arthouse", "A0257", Some("/kino/osnabrueck/cinemaarthouse-46624"), "Osnabrück"),
    ("Filmtheater Hasetor", "Filmtheater Hasetor", "A1431", Some("/kino/osnabrueck/filmtheater-hasetor-32581"), "Osnabrück"),
    ("HALL OF FAME - Kino de Luxe", "HALL OF FAME - Kino de Luxe", "A0348", Some("/kino/osnabrueck/hall-of-fame-92002"), "Osnabrück"),
    ("Kino in der Lagerhalle", "Kino in der Lagerhalle", "A0739", Some("/kino/osnabrueck/kino-in-der-lagerhalle-32578"), "Osnabrück"),
    ("Open Air Kino im Innenhof der Domschule Giro Live", "Open Air Kino im Innenhof der Domschule Giro Live", "A1961", Some("/kino/osnabrueck/open-air-kino-im-innenhof-der-domschule-giro-live-67997"), "Osnabrück"),
    ("Open-Air-Kino Schloss-Innenhof", "Open-Air-Kino Schloss-Innenhof", "A2504", None, "Osnabrück")
  ))
  private def p_solingen: R = ("solingen", "solingen-nordrhein-westfalen", "Solingen", "Nordrhein-Westfalen", 51.1734, 7.0845, false, Seq("Solingen"), Seq(
    ("Das Lumen Solingen", "Das Lumen Solingen", "A0267", None, "Solingen"),
    ("Kino im Cobra", "Kino im Cobra", "A0717", None, "Solingen")
  ))
  private def p_leverkusen: R = ("leverkusen", "leverkusen-nordrhein-westfalen", "Leverkusen", "Nordrhein-Westfalen", 51.0303, 6.9843, false, Seq("Leverkusen"), Seq(
    ("Cineplex Kinopolis Leverkusen", "Cineplex Kinopolis Leverkusen", "A0826", None, "Leverkusen"),
    ("Kommunales Kino Leverkusen", "Kommunales Kino Leverkusen", "A0859", Some("/kino/leverkusen/kommunales-kino-69959"), "Leverkusen"),
    ("Scala Leverkusen", "Scala Leverkusen", "A2358", Some("/kino/leverkusen/scala-31187"), "Leverkusen")
  ))
  private def p_oldenburg: R = ("oldenburg", "oldenburg-niedersachsen", "Oldenburg", "Niedersachsen", 53.1404, 8.2148, false, Seq("Oldenburg"), Seq(
    ("Casablanca-Programmkino", "Casablanca-Programmkino", "A0097", Some("/kino/oldenburg-in-oldenburg/casablancaprogrammkino-58136"), "Oldenburg"),
    ("Cine K Oldenburg", "Cine K Oldenburg", "A0220", Some("/kino/oldenburg-in-oldenburg/cine-k-69957"), "Oldenburg"),
    ("CinemaxX Oldenburg", "CinemaxX Oldenburg", "A0281", Some("/kino/oldenburg-in-oldenburg/cinemaxx-37682"), "Oldenburg"),
    ("Studentisches Kino Gegenlicht", "Studentisches Kino Gegenlicht", "A0569", Some("/kino/oldenburg-in-oldenburg/gegenlicht-35074"), "Oldenburg")
  ))
  private def p_neuss: R = ("neuss", "neuss-nordrhein-westfalen", "Neuss", "Nordrhein-Westfalen", 51.1981, 6.685, false, Seq("Neuss"), Seq(
    ("Hitch", "Hitch", "A0607", Some("/kino/neuss/hitch-30126"), "Neuss"),
    ("UCI Neuss", "UCI Neuss", "A1201", Some("/kino/neuss/uci-kinowelt-neuss-38582"), "Neuss")
  ))
  private def p_regensburg: R = ("regensburg", "regensburg-bayern", "Regensburg", "Bayern", 49.0151, 12.1016, false, Seq("Regensburg"), Seq(
    ("CinemaxX Regensburg", "CinemaxX Regensburg", "A0289", Some("/kino/regensburg/cinemaxx-37053"), "Regensburg"),
    ("Filmgalerie im Leeren Beutel", "Filmgalerie im Leeren Beutel", "A0497", Some("/kino/regensburg/filmgalerie-im-leeren-beutel-60781"), "Regensburg"),
    ("Garbo", "Garbo", "A1436", Some("/kino/regensburg/garbo-32949"), "Regensburg"),
    ("Kinos im Andreasstadel", "Kinos im Andreasstadel", "A1280", Some("/kino/regensburg/kinos-im-andreasstadl-66757"), "Regensburg"),
    ("Ostentor", "Ostentor", "A1581", Some("/kino/regensburg/ostentor-32948"), "Regensburg"),
    ("Regina Filmtheater Regensburg", "Regina Filmtheater Regensburg", "A1052", Some("/kino/regensburg/regina-filmtheater-32950"), "Regensburg"),
    ("Turm-Theater Regensburg", "Turm-Theater Regensburg", "A1195", None, "Regensburg")
  ))
  private def p_heidelberg: R = ("heidelberg", "heidelberg-baden-wuerttemberg", "Heidelberg", "Baden-Württemberg", 49.4077, 8.6908, false, Seq("Heidelberg"), Seq(
    ("Gloria &amp; Gloriette", "Gloria &amp; Gloriette", "A0578", Some("/kino/heidelberg-neckar/gloria-und-gloriette-32077"), "Heidelberg"),
    ("Kamera Heidelberg", "Kamera Heidelberg", "A0635", Some("/kino/heidelberg-neckar/kamera-54296"), "Heidelberg"),
    ("Karlstorkino", "Karlstorkino", "A0648", Some("/kino/heidelberg-neckar/karlstorkino-54216"), "Heidelberg"),
    ("Luxor Filmpalast Heidelberg", "Luxor Filmpalast Heidelberg", "A2874", Some("/kino/heidelberg-neckar/luxorfilmpalast-89317"), "Heidelberg")
  ))
  private def p_paderborn: R = ("paderborn", "paderborn-nordrhein-westfalen", "Paderborn", "Nordrhein-Westfalen", 51.7191, 8.7544, false, Seq("Paderborn"), Seq(
    ("Pollux by Cineplex Paderborn", "Pollux by Cineplex Paderborn", "A0315", None, "Paderborn"),
    ("Programmkino Lichtblick e.V.", "Programmkino Lichtblick e.V.", "A1044", Some("/kino/paderborn/programmkino-lichtblick-e.v.-66276"), "Paderborn"),
    ("UCI Paderborn", "UCI Paderborn", "A0832", Some("/kino/paderborn/uci-kinowelt-paderborn-74676"), "Paderborn")
  ))
  private def p_wuerzburg: R = ("wuerzburg", "wuerzburg-bayern", "Würzburg", "Bayern", 49.7939, 9.9512, false, Seq("Würzburg"), Seq(
    ("Central im Bürgerbräu", "Central im Bürgerbräu", "A2742", Some("/kino/wuerzburg/central-im-buergerbraeu-88817"), "Würzburg"),
    ("Central Programmkino", "Central Programmkino", "A2621", Some("/kino/wuerzburg/central-78717"), "Würzburg"),
    ("CinemaxX Würzburg", "CinemaxX Würzburg", "A0263", Some("/kino/wuerzburg/cinemaxx-38998"), "Würzburg")
  ))
  private def p_fuerth: R = ("fuerth", "fuerth-bayern", "Fürth", "Bayern", 49.4759, 10.9886, false, Seq("Fürth"), Seq(
    ("Babylon Kino am Stadtpark", "Babylon Kino am Stadtpark", "A0657", Some("/kino/fuerth/babylon-kino-am-stadtpark-32762"), "Fürth"),
    ("Cineplex Fürth", "Cineplex Fürth", "A2727", Some("/kino/fuerth/cineplex-87698"), "Fürth"),
    ("Freilichtbühne im Fürther Stadtpark", "Freilichtbühne im Fürther Stadtpark", "A1963", None, "Fürth"),
    ("Open Air Kino Mauerflimmern", "Open Air Kino Mauerflimmern", "A2780", Some("/kino/fuerth/open-air-kino-im-kulturforum-fuerth-%22mauerflimmern%22-88137"), "Fürth"),
    ("Uferpalast Führt", "Uferpalast Führt", "A1223", Some("/kino/fuerth/uferpalast-34559"), "Fürth")
  ))
  private def p_wolfsburg: R = ("wolfsburg", "wolfsburg-niedersachsen", "Wolfsburg", "Niedersachsen", 52.4245, 10.7815, false, Seq("Wolfsburg"), Seq(
    ("CinemaxX Wolfsburg", "CinemaxX Wolfsburg", "A0283", Some("/kino/wolfsburg/cinemaxx-42023"), "Wolfsburg"),
    ("Delphin Palast", "Delphin Palast", "A0437", Some("/kino/wolfsburg/delphin-palast-65736"), "Wolfsburg"),
    ("Kino im Hallenbad - Wolfsburg", "Kino im Hallenbad - Wolfsburg", "A2385", Some("/kino/wolfsburg/kino-im-hallenbad-wolfsburg-75112"), "Wolfsburg"),
    ("Metropol Theater Fallersleben", "Metropol Theater Fallersleben", "A0949", Some("/kino/wolfsburg/metropol-theater-fallersleben-32647"), "Wolfsburg")
  ))
  private def p_recklinghausen: R = ("recklinghausen", "recklinghausen-nordrhein-westfalen", "Recklinghausen", "Nordrhein-Westfalen", 51.6138, 7.1974, false, Seq("Recklinghausen"), Seq(
    ("Cineworld", "Cineworld", "A0388", Some("/kino/recklinghausen/cineworld-37763"), "Recklinghausen")
  ))
  private def p_goettingen: R = ("goettingen", "goettingen-niedersachsen", "Göttingen", "Niedersachsen", 51.5344, 9.9323, false, Seq("Göttingen"), Seq(
    ("CinemaxX Göttingen", "CinemaxX Göttingen", "A1293", Some("/kino/goettingen/cinemaxx-32426"), "Göttingen"),
    ("Lumière", "Lumière", "A1548", Some("/kino/goettingen/lumi%C3%A8re-32427"), "Göttingen"),
    ("Open Air Kino im Freibad", "Open Air Kino im Freibad", "A1788", Some("/kino/goettingen/open-air-kino-im-freibad-37803"), "Göttingen"),
    ("Méliès Göttingen", "Méliès Göttingen", "cineamo:2975", None, "Göttingen")
  ))
  private def p_heilbronn: R = ("heilbronn", "heilbronn-baden-wuerttemberg", "Heilbronn", "Baden-Württemberg", 49.1399, 9.2205, false, Seq("Heilbronn"), Seq(
    ("CinemaxX Heilbronn", "CinemaxX Heilbronn", "A2894", Some("/kino/heilbronn/cinemaxx-42206"), "Heilbronn"),
    ("Kinostar Arthaus Heilbronn", "Kinostar Arthaus Heilbronn", "A2748", Some("/kino/heilbronn/kinostar-arthaus-32085"), "Heilbronn"),
    ("Open-Air-Kino-Heilbronn", "Open-Air-Kino-Heilbronn", "A2814", Some("/kino/heilbronn/openairkino-92805"), "Heilbronn")
  ))
  private def p_ingolstadt: R = ("ingolstadt", "ingolstadt-bayern", "Ingolstadt", "Bayern", 48.7651, 11.4237, false, Seq("Ingolstadt"), Seq(
    ("Altstadtkinos Ingolstadt - Cinema", "Altstadtkinos Ingolstadt - Cinema", "A1369", Some("/kino/ingolstadt/altstadtkinos-cinema-32798"), "Ingolstadt"),
    ("Altstadtkinos Ingolstadt - Union", "Altstadtkinos Ingolstadt - Union", "A1225", Some("/kino/ingolstadt/altstadtkinos-union-32800"), "Ingolstadt"),
    ("Audi Programmkino", "Audi Programmkino", "A2784", Some("/kino/ingolstadt/audi-programmkino-im-audi-forum-ingolstadt-66437"), "Ingolstadt"),
    ("CineStar Ingolstadt", "CineStar Ingolstadt", "A0339", Some("/kino/ingolstadt/cinestar-der-filmpalast-61036"), "Ingolstadt"),
    ("Kino Open Air Ingolstadt", "Kino Open Air Ingolstadt", "A2024", None, "Ingolstadt")
  ))
  private def p_ulm: R = ("ulm", "ulm-baden-wuerttemberg", "Ulm", "Baden-Württemberg", 48.3984, 9.9916, false, Seq("Ulm"), Seq(
    ("Mephisto Ulm", "Mephisto Ulm", "A1559", Some("/kino/ulm/mephisto-33040"), "Ulm"),
    ("Obscura Ulm", "Obscura Ulm", "A0692", Some("/kino/ulm/obscura-33039"), "Ulm"),
    ("Xinedome", "Xinedome", "A1285", Some("/kino/ulm/xinedome-57656"), "Ulm")
  ))
  private def p_bottrop: R = ("bottrop", "bottrop-nordrhein-westfalen", "Bottrop", "Nordrhein-Westfalen", 51.5239, 6.9285, false, Seq("Bottrop"), Seq(
    ("Filmforum der VHS", "Filmforum der VHS", "A1475", Some("/kino/bottrop/filmforum-der-vhs-143"), "Bottrop")
  ))
  private def p_pforzheim: R = ("pforzheim", "pforzheim-baden-wuerttemberg", "Pforzheim", "Baden-Württemberg", 48.8844, 8.6989, false, Seq("Pforzheim"), Seq(
    ("Cinemoon Pforzheim", "Cinemoon Pforzheim", "A0309", Some("/kino/pforzheim/cinemoon-47956"), "Pforzheim"),
    ("Kommunales Kino Pforzheim", "Kommunales Kino Pforzheim", "A0857", Some("/kino/pforzheim/kommunales-kino-36754"), "Pforzheim"),
    ("Open-Air-Kino im Kulturhaus Osterfeld", "Open-Air-Kino im Kulturhaus Osterfeld", "A1911", Some("/kino/pforzheim/openairkino-im-kulturhaus-osterfeld-44536"), "Pforzheim"),
    ("rex Filmpalast Pforzheim", "rex Filmpalast Pforzheim", "A1068", Some("/kino/pforzheim/rex-filmpalast-56736"), "Pforzheim")
  ))
  private def p_offenbach_am_main: R = ("offenbach-am-main", "offenbach-am-main-hessen", "Offenbach am Main", "Hessen", 50.1006, 8.7665, false, Seq("Offenbach am Main"), Seq(
    ("CinemaxX Offenbach", "CinemaxX Offenbach", "A0264", Some("/kino/offenbach-am-main/cinemaxx-38829"), "Offenbach am Main"),
    ("Hafenkino - Programmkino im Kulturzentrum Hafen 2", "Hafenkino - Programmkino im Kulturzentrum Hafen 2", "A2691", Some("/kino/offenbach-am-main/hafenkino-programmkino-im-kulturzentrum-hafen-2-86217"), "Offenbach am Main"),
    ("Filmklubb", "Filmklubb", "cineamo:2997", None, "Offenbach am Main")
  ))
  private def p_bremerhaven: R = ("bremerhaven", "bremerhaven-bremen", "Bremerhaven", "Bremen", 53.5536, 8.5755, false, Seq("Bremerhaven"), Seq(
    ("CineMotion Bremerhaven", "CineMotion Bremerhaven", "A1773", Some("/kino/bremerhaven/cinemotion-im-havenhaus-bremerhaven-70856"), "Bremerhaven")
  ))
  private def p_remscheid: R = ("remscheid", "remscheid-nordrhein-westfalen", "Remscheid", "Nordrhein-Westfalen", 51.1798, 7.1925, false, Seq("Remscheid"), Seq(
    ("CineStar Remscheid", "CineStar Remscheid", "A2901", Some("/kino/remscheid/cinestar-91597"), "Remscheid"),
    ("Open Air an der Gelben Villa", "Open Air an der Gelben Villa", "A2810", None, "Remscheid"),
    ("Open Air Kino Remscheid", "Open Air Kino Remscheid", "A2826", None, "Remscheid")
  ))
  private def p_reutlingen: R = ("reutlingen", "reutlingen-baden-wuerttemberg", "Reutlingen", "Baden-Württemberg", 48.4914, 9.2043, false, Seq("Reutlingen"), Seq(
    ("Cineplex Planie Reutlingen", "Cineplex Planie Reutlingen", "A1038", Some("/kino/reutlingen/cineplex-planie-32958"), "Reutlingen"),
    ("Kamino", "Kamino", "A2717", Some("/kino/reutlingen/kamino-87097"), "Reutlingen"),
    ("Open Air Kino Spitalhof Reutlingen", "Open Air Kino Spitalhof Reutlingen", "A2518", None, "Reutlingen")
  ))
  private def p_koblenz: R = ("koblenz", "koblenz-rheinland-pfalz", "Koblenz", "Rheinland-Pfalz", 50.3536, 7.5788, false, Seq("Koblenz"), Seq(
    ("Kinopolis Koblenz", "Kinopolis Koblenz", "A0830", Some("/kino/koblenz-am-rhein/kinopolis-57476"), "Koblenz"),
    ("Odeon-Kinocenter", "Odeon-Kinocenter", "A0062", Some("/kino/koblenz-am-rhein/odeon-kinocenter-31835"), "Koblenz")
  ))
  private def p_siegen: R = ("siegen", "siegen-nordrhein-westfalen", "Siegen", "Nordrhein-Westfalen", 50.8748, 8.0243, false, Seq("Siegen"), Seq(
    ("CineStar Siegen", "CineStar Siegen", "A0351", None, "Siegen"),
    ("Kurbelkiste", "Kurbelkiste", "A0882", None, "Siegen")
  ))
  private def p_bergisch_gladbach: R = ("bergisch-gladbach", "bergisch-gladbach-nordrhein-westfalen", "Bergisch Gladbach", "Nordrhein-Westfalen", 50.9856, 7.133, false, Seq("Bergisch Gladbach"), Seq(
    ("Cineplex Bensberg", "Cineplex Bensberg", "A2715", Some("/kino/bergisch-gladbach/cineplex-bensberg-31750"), "Bergisch Gladbach")
  ))
  private def p_salzgitter: R = ("salzgitter", "salzgitter-niedersachsen", "Salzgitter", "Niedersachsen", 52.157, 10.4154, false, Seq("Salzgitter"), Seq(
    ("Cinema in der Angerpassage", "Cinema in der Angerpassage", "A0253", None, "Salzgitter"),
    ("City-Theater Kultiplex", "City-Theater Kultiplex", "A1377", Some("/kino/salzgitter/kultiplex-32602"), "Salzgitter")
  ))
  private def p_jena: R = ("jena", "jena-thueringen", "Jena", "Thüringen", 50.9288, 11.5899, false, Seq("Jena"), Seq(
    ("CineStar Jena", "CineStar Jena", "A0328", Some("/kino/jena/cinestar-37109"), "Jena"),
    ("Filmarena Open-Air Kino Theatervorplatz", "Filmarena Open-Air Kino Theatervorplatz", "A1844", Some("/kino/jena/filmarena-openair-kino-theatervorplatz-40658"), "Jena"),
    ("Kino am Markt", "Kino am Markt", "A2732", Some("/kino/jena/kino-am-markt-87777"), "Jena"),
    ("Kino im Schillerhof", "Kino im Schillerhof", "A0729", Some("/kino/jena/kino-im-schillerhof-71316"), "Jena")
  ))
  private def p_gera: R = ("gera", "gera-thueringen", "Gera", "Thüringen", 50.8803, 12.0819, false, Seq("Gera"), Seq(
    ("Kino im COMMA", "Kino im COMMA", "A0416", None, "Gera"),
    ("Metropol", "Metropol", "A2677", Some("/kino/gera/metropol-kino-31532"), "Gera"),
    ("Open Air im Comma-Garten", "Open Air im Comma-Garten", "A2759", None, "Gera")
  ))
  private def p_moers: R = ("moers", "moers-nordrhein-westfalen", "Moers", "Nordrhein-Westfalen", 51.4534, 6.6326, false, Seq("Moers"), Seq(
    ("Atlantic Kinocenter", "Atlantic Kinocenter", "A2328", None, "Moers"),
    ("Movie Center Moers", "Movie Center Moers", "A2789", Some("/kino/moers/movie-center-74636"), "Moers")
  ))
  private def p_hildesheim: R = ("hildesheim", "hildesheim-niedersachsen", "Hildesheim", "Niedersachsen", 52.1508, 9.9511, false, Seq("Hildesheim"), Seq(
    ("Hochschulkino im Audimax", "Hochschulkino im Audimax", "A2876", None, "Hildesheim"),
    ("Thega Filmpalast Hildesheim", "Thega Filmpalast Hildesheim", "A1181", Some("/kino/hildesheim/thegafilmpalast-32491"), "Hildesheim")
  ))
  private def p_erlangen: R = ("erlangen", "erlangen-bayern", "Erlangen", "Bayern", 49.591, 11.0078, false, Seq("Erlangen"), Seq(
    ("CineStar Erlangen", "CineStar Erlangen", "A0355", Some("/kino/erlangen/cinestar-erlangen-35135"), "Erlangen"),
    ("Kino im E-Werk", "Kino im E-Werk", "A0720", Some("/kino/erlangen/kino-im-ewerk-37549"), "Erlangen"),
    ("Lamm-Lichtspiele", "Lamm-Lichtspiele", "A1532", Some("/kino/erlangen/lammlichtspiele-32739"), "Erlangen"),
    ("Manhattan Deluxe – Premiumkino", "Manhattan Deluxe – Premiumkino", "A1554", Some("/kino/erlangen/manhattan-deluxe-32737"), "Erlangen"),
    ("Open Air an der Bleiche", "Open Air an der Bleiche", "A2864", None, "Erlangen"),
    ("Open Air Erlangen", "Open Air Erlangen", "A1943", None, "Erlangen")
  ))
  private def p_trier: R = ("trier", "trier-rheinland-pfalz", "Trier", "Rheinland-Pfalz", 49.7557, 6.6394, false, Seq("Trier"), Seq(
    ("Broadway Trier", "Broadway Trier", "A0109", Some("/kino/trier/broadway-31923"), "Trier"),
    ("CineAStA", "CineAStA", "A2728", Some("/kino/trier/cineasta-der-uni-trier-87857"), "Trier"),
    ("CinemaxX Trier", "CinemaxX Trier", "A0288", Some("/kino/trier/cinemaxx-39458"), "Trier"),
    ("Open-Air-Kino im TuFa-Hof", "Open-Air-Kino im TuFa-Hof", "A2829", None, "Trier")
  ))
  private def p_aalen: R = ("aalen", "aalen-baden-wuerttemberg", "Aalen", "Baden-Württemberg", 48.8378, 10.0933, false, Seq("Aalen"), Seq(
    ("Kino am Kocher", "Kino am Kocher", "A2023", Some("/kino/aalen/kino-am-kocher-71741"), "Aalen"),
    ("Kinopark Aalen", "Kinopark Aalen", "A0823", Some("/kino/aalen/kinopark-39656"), "Aalen")
  ))
  private def p_achim: R = ("achim", "achim-niedersachsen", "Achim", "Niedersachsen", 53.0142, 9.0263, true, Seq("Achim", "Verden an der Aller", "Rotenburg an der Wümme"), Seq(
    ("Cine City", "Cine City", "A0219", None, "Verden an der Aller"),
    ("Koki Kommunales Kino", "Koki Kommunales Kino", "A0853", None, "Achim"),
    ("Stadtkino Rotenburg", "Stadtkino Rotenburg", "A2372", None, "Rotenburg an der Wümme")
  ))
  private def p_ahaus: R = ("ahaus", "ahaus-nordrhein-westfalen", "Ahaus", "Nordrhein-Westfalen", 52.0794, 7.0134, false, Seq("Ahaus"), Seq(
    ("Cinema Borken", "Cinema Borken", "A0258", None, "Ahaus"),
    ("Cinetech das Erlebniskino Ahaus", "Cinetech das Erlebniskino Ahaus", "A0384", None, "Ahaus")
  ))
  private def p_ahlen: R = ("ahlen", "ahlen-nordrhein-westfalen", "Ahlen", "Nordrhein-Westfalen", 51.7634, 7.8887, true, Seq("Ahlen", "Oelde", "Ennigerloh"), Seq(
    ("CinemAhlen", "CinemAhlen", "A2879", None, "Ahlen"),
    ("Alte Brennerei", "Alte Brennerei", "A0021", None, "Ennigerloh"),
    ("Filmzentrum am Rathaus", "Filmzentrum am Rathaus", "A0549", None, "Oelde")
  ))
  private def p_aichach: R = ("aichach", "aichach-bayern", "Aichach", "Bayern", 48.4575, 11.1341, true, Seq("Aichach", "Meitingen", "Wertingen"), Seq(
    ("Cineplex Aichach", "Cineplex Aichach", "A1374", Some("/kino/aichach/cineplex-32655"), "Aichach"),
    ("Cineplex Meitingen", "Cineplex Meitingen", "A0217", Some("/kino/meitingen/cineplex-32844"), "Meitingen"),
    ("Filmtheater Wertingen", "Filmtheater Wertingen", "A2120", Some("/kino/wertingen/filmtheater-33064"), "Wertingen")
  ))
  private def p_albstadt: R = ("albstadt", "albstadt-baden-wuerttemberg", "Albstadt", "Baden-Württemberg", 48.2164, 9.026, true, Seq("Albstadt", "Balingen", "Gammertingen"), Seq(
    ("Bali Kino-Palast", "Bali Kino-Palast", "A1308", Some("/kino/balingen/bali-kinopalast-32691"), "Balingen"),
    ("Capitol Filmpalast - Albstadt", "Capitol Filmpalast - Albstadt", "A2878", Some("/kino/albstadt/capitol-filmpalast-32657"), "Albstadt"),
    ("Open Air", "Open Air", "A2772", Some("/kino/gammertingen/openair-kino-gammertingen-85497"), "Gammertingen")
  ))
  private def p_alfeld: R = ("alfeld", "alfeld-niedersachsen", "Alfeld", "Niedersachsen", 51.9838, 9.8199, true, Seq("Alfeld", "Gronau"), Seq(
    ("Gronauer-Lichtspiele", "Gronauer-Lichtspiele", "A1446", None, "Gronau"),
    ("Kinowelt Alfeld", "Kinowelt Alfeld", "A1515", None, "Alfeld")
  ))
  private def p_alpirsbach: R = ("alpirsbach", "alpirsbach-baden-wuerttemberg", "Alpirsbach", "Baden-Württemberg", 48.3451, 8.402, false, Seq("Alpirsbach"), Seq(
    ("Kino unterm Sternenhimmel Alpirsbach", "Kino unterm Sternenhimmel Alpirsbach", "A2486", None, "Alpirsbach"),
    ("Open Air Kino im Kreuzgarten", "Open Air Kino im Kreuzgarten", "A1818", Some("/kino/alpirsbach/open-air-kino-im-kreuzgarten-des-klosters-63816"), "Alpirsbach"),
    ("Subiaco Galerie", "Subiaco Galerie", "A0566", None, "Alpirsbach")
  ))
  private def p_alsdorf: R = ("alsdorf", "alsdorf-nordrhein-westfalen", "Alsdorf", "Nordrhein-Westfalen", 50.8767, 6.164, true, Seq("Alsdorf", "Würselen"), Seq(
    ("Cinetower Alsdorf", "Cinetower Alsdorf", "A0069", Some("/kino/alsdorf/cinetower-kinopark-alsdorf-58018"), "Alsdorf"),
    ("Metropolis Würselen", "Metropolis Würselen", "A1563", Some("/kino/wuerselen/metropolis-31943"), "Würselen")
  ))
  private def p_altenburg: R = ("altenburg", "altenburg-thueringen", "Altenburg", "Thüringen", 50.9876, 12.4368, true, Seq("Altenburg", "Borna", "Geithain"), Seq(
    ("Capitol Altenburg", "Capitol Altenburg", "A0143", Some("/kino/altenburg/capitol-49436"), "Altenburg"),
    ("Kino im Bürgerhaus Geithain", "Kino im Bürgerhaus Geithain", "A2944", None, "Geithain"),
    ("Volksplatz Borna Open Air Kino", "Volksplatz Borna Open Air Kino", "A1804", None, "Borna")
  ))
  private def p_amberg: R = ("amberg", "amberg-bayern", "Amberg", "Bayern", 49.4429, 11.8627, true, Seq("Amberg", "Burglengenfeld", "Nittenau", "Wald"), Seq(
    ("Cineplex Amberg", "Cineplex Amberg", "A2686", Some("/kino/amberg/cineplex-86179"), "Amberg"),
    ("Kino-Center Nittenau", "Kino-Center Nittenau", "A1461", Some("/kino/nittenau/kinocenter-32903"), "Nittenau"),
    ("Kinomobil Stuttgart - Feuerwehrhaus Wald", "Kinomobil Stuttgart - Feuerwehrhaus Wald", "A1937", None, "Wald"),
    ("Starmexx - Erlebniskino", "Starmexx - Erlebniskino", "A2664", Some("/kino/burglengenfeld/starmexx-erlebniskino-80257"), "Burglengenfeld")
  ))
  private def p_anklam: R = ("anklam", "anklam-mecklenburg-vorpommern", "Anklam", "Mecklenburg-Vorpommern", 53.8564, 13.6897, true, Seq("Anklam", "Zinnowitz", "Heringsdorf", "Koserow", "Zempin"), Seq(
    ("Autokino Usedom", "Autokino Usedom", "A0074", Some("/kino/koserow/autokino-usedom-36212"), "Koserow"),
    ("Club-Kino", "Club-Kino", "A0413", Some("/kino/zinnowitz-ostseebad/clubkino-31719"), "Zinnowitz"),
    ("Kino-Center Anklam", "Kino-Center Anklam", "A0703", Some("/kino/anklam/kinocenter-39957"), "Anklam"),
    ("Sommerkino Heringsdorf", "Sommerkino Heringsdorf", "A0751", Some("/kino/heringsdorf/sommerkino-heringsdorf-89417"), "Heringsdorf"),
    ("Sommerkino Zempin (SAK)", "Sommerkino Zempin (SAK)", "A2092", Some("/kino/zempin/sommerkino-zempin-sak-31715"), "Zempin")
  ))
  private def p_annaberg_buchholz: R = ("annaberg-buchholz", "annaberg-buchholz-sachsen", "Annaberg-Buchholz", "Sachsen", 50.5795, 13.0063, true, Seq("Annaberg-Buchholz", "Aue", "Marienberg", "Schwarzenberg/Erzgebirge", "Gelenau", "Geyer"), Seq(
    ("Autokino Greifensteine", "Autokino Greifensteine", "A2184", Some("/kino/geyer/autokino-greifensteine-73917"), "Geyer"),
    ("Clubkino", "Clubkino", "A0412", Some("/kino/gelenau-erzgebirge/clubkino-31529"), "Gelenau"),
    ("Filmtheater Movie", "Filmtheater Movie", "A0544", Some("/kino/marienberg/kinocenter-movie-31604"), "Marienberg"),
    ("Gloria Filmpalast Annaberg-Buchholz", "Gloria Filmpalast Annaberg-Buchholz", "A0580", Some("/kino/annabergbuchholz/gloria-filmpalast-35370"), "Annaberg-Buchholz"),
    ("Kino-Center Nickel-Odeon", "Kino-Center Nickel-Odeon", "A0996", Some("/kino/aue/nickelodeonfilmtheater-37556"), "Aue"),
    ("Olympia Schwarzenberg", "Olympia Schwarzenberg", "A1011", None, "Schwarzenberg/Erzgebirge")
  ))
  private def p_ansbach: R = ("ansbach", "ansbach-bayern", "Ansbach", "Bayern", 49.3048, 10.5931, false, Seq("Ansbach"), Seq(
    ("Capitol Kinocenter", "Capitol Kinocenter", "A1338", Some("/kino/ansbach/capitol-kinocenter-32665"), "Ansbach"),
    ("Kammerspiele Ansbach", "Kammerspiele Ansbach", "A1733", Some("/kino/ansbach/kammerspiele-32664"), "Ansbach"),
    ("Kultur am Schloss", "Kultur am Schloss", "A1628", Some("/kino/ansbach/kultur-am-schloss-32666"), "Ansbach")
  ))
  private def p_arnsberg: R = ("arnsberg", "arnsberg-nordrhein-westfalen", "Arnsberg", "Nordrhein-Westfalen", 51.3833, 8.0833, false, Seq("Arnsberg"), Seq(
    ("Apollo Arnsberg-Neheim", "Apollo Arnsberg-Neheim", "A0038", None, "Arnsberg"),
    ("Centra-Theater Arnsberg", "Centra-Theater Arnsberg", "A0184", Some("/kino/arnsberg/centraltheater-31738"), "Arnsberg"),
    ("Residenz Kino-Center", "Residenz Kino-Center", "A1062", Some("/kino/arnsberg/residenz-kinocenter-30866"), "Arnsberg")
  ))
  private def p_asbach: R = ("asbach", "asbach-rheinland-pfalz", "Asbach", "Rheinland-Pfalz", 50.6667, 7.4167, true, Seq("Asbach", "Hachenburg", "Neitersen"), Seq(
    ("Cine 5", "Cine 5", "A2640", Some("/kino/asbach/cine-5-78858"), "Asbach"),
    ("Cinexx", "Cinexx", "A0392", Some("/kino/hachenburg/cinexx-31812"), "Hachenburg"),
    ("Wied Scala", "Wied Scala", "A1278", Some("/kino/neitersen-westerwald/wied-scala-31882"), "Neitersen")
  ))
  private def p_aschaffenburg: R = ("aschaffenburg", "aschaffenburg-bayern", "Aschaffenburg", "Bayern", 49.977, 9.1521, false, Seq("Aschaffenburg"), Seq(
    ("Casino Aschaffenburg", "Casino Aschaffenburg", "A1342", Some("/kino/aschaffenburg/casino-31952"), "Aschaffenburg"),
    ("Kinopolis Aschaffenburg", "Kinopolis Aschaffenburg", "A0827", Some("/kino/aschaffenburg/kinopolis-aschaffenburg-35109"), "Aschaffenburg"),
    ("OPEN AIR auf dem Campus FH", "OPEN AIR auf dem Campus FH", "A1959", Some("/kino/aschaffenburg/open-air-auf-dem-campus-fh-71531"), "Aschaffenburg"),
    ("Open-Air-Kino im Nilkheimer Park", "Open-Air-Kino im Nilkheimer Park", "A2813", Some("/kino/aschaffenburg/openairkino-im-nilkheimer-park-40457"), "Aschaffenburg")
  ))
  private def p_auerbach: R = ("auerbach", "auerbach-sachsen", "Auerbach", "Sachsen", 50.5115, 12.4008, true, Seq("Auerbach", "Schneeberg", "Markneukirchen", "Sosa"), Seq(
    ("Freilichtbühne Sosa", "Freilichtbühne Sosa", "A2191", None, "Sosa"),
    ("Harmonie Lichtspiele Markneukirchen", "Harmonie Lichtspiele Markneukirchen", "A0600", Some("/kino/markneukirchen/harmonie-lichtspiele-31605"), "Markneukirchen"),
    ("Rekord-Lichtspiele", "Rekord-Lichtspiele", "A1058", Some("/kino/auerbach/rekord-lichtspiele-37051"), "Auerbach"),
    ("Union Filmtheater Schneeberg", "Union Filmtheater Schneeberg", "A1228", Some("/kino/schneeberg/union-filmtheater-31660"), "Schneeberg")
  ))
  private def p_aurich: R = ("aurich", "aurich-niedersachsen", "Aurich", "Niedersachsen", 53.4696, 7.4824, false, Seq("Aurich"), Seq(
    ("Kino Aurich", "Kino Aurich", "A2911", Some("/kino/aurich/kino-aurich-57096"), "Aurich")
  ))
  private def p_backnang: R = ("backnang", "backnang-baden-wuerttemberg", "Backnang", "Baden-Württemberg", 48.9474, 9.4372, true, Seq("Backnang", "Allmersbach im Tal"), Seq(
    ("Kino Universum Backnang", "Kino Universum Backnang", "A1656", Some("/kino/backnang/kino-universum-32680"), "Backnang"),
    ("Traumpalast Backnang", "Traumpalast Backnang", "A1430", Some("/kino/backnang/traumpalast-backnang-32679"), "Backnang"),
    ("Kinomobil Stuttgart - Bürgersaal", "Kinomobil Stuttgart - Bürgersaal", "A2557", None, "Allmersbach im Tal")
  ))
  private def p_bad_aibling: R = ("bad-aibling", "bad-aibling-bayern", "Bad Aibling", "Bayern", 47.8638, 12.0106, true, Seq("Bad Aibling", "Rosenheim"), Seq(
    ("Aibvision Filmtheater und Lindenkino", "Aibvision Filmtheater und Lindenkino", "A1543", Some("/kino/bad-aibling/aibvision-filmtheater-32681"), "Bad Aibling"),
    ("Open Air am B&amp;O Parkhotel", "Open Air am B&amp;O Parkhotel", "A2862", Some("/kino/bad-aibling/open-air-am-bundo-parkhotel-82462"), "Bad Aibling"),
    ("Kinopolis Rosenheim", "Kinopolis Rosenheim", "A0401", None, "Rosenheim")
  ))
  private def p_bad_belzig: R = ("bad-belzig", "bad-belzig-brandenburg", "Bad Belzig", "Brandenburg", 52.1418, 12.5927, false, Seq("Bad Belzig"), Seq(
    ("Hofgarten Kino", "Hofgarten Kino", "A0611", None, "Bad Belzig")
  ))
  private def p_bad_berleburg: R = ("bad-berleburg", "bad-berleburg-nordrhein-westfalen", "Bad Berleburg", "Nordrhein-Westfalen", 51.0522, 8.3923, true, Seq("Bad Berleburg", "Bad Laasphe", "Winterberg"), Seq(
    ("Capitol Bad Berleburg", "Capitol Bad Berleburg", "A0165", Some("/kino/bad-berleburg/capitol-31740"), "Bad Berleburg"),
    ("Filmtheater Winterberg", "Filmtheater Winterberg", "A0543", Some("/kino/winterberg/filmtheater-31940"), "Winterberg"),
    ("Residenztheater", "Residenztheater", "A1063", Some("/kino/bad-laasphe/residenztheater-41902"), "Bad Laasphe")
  ))
  private def p_bad_driburg: R = ("bad-driburg", "bad-driburg-nordrhein-westfalen", "Bad Driburg", "Nordrhein-Westfalen", 51.733, 9.0197, true, Seq("Bad Driburg", "Brakel"), Seq(
    ("Kino Bad Driburg", "Kino Bad Driburg", "A0706", Some("/kino/bad-driburg/kino-bad-driburg-31741"), "Bad Driburg"),
    ("Kino Brakel", "Kino Brakel", "A0711", Some("/kino/brakel/kino-brakel-66537"), "Brakel")
  ))
  private def p_bad_fuessing: R = ("bad-fuessing", "bad-fuessing-bayern", "Bad Füssing", "Bayern", 48.351, 13.312, false, Seq("Bad Füssing"), Seq(
    ("Filmgalerie Bad Füssing", "Filmgalerie Bad Füssing", "A1417", Some("/kino/bad-fuessing/filmgalerie-33684"), "Bad Füssing"),
    ("Kino im großen Kurhaus", "Kino im großen Kurhaus", "A1482", Some("/kino/bad-fuessing/kino-im-grossen-kurhaus-32684"), "Bad Füssing")
  ))
  private def p_bad_hersfeld: R = ("bad-hersfeld", "bad-hersfeld-hessen", "Bad Hersfeld", "Hessen", 50.872, 9.7089, true, Seq("Bad Hersfeld", "Bebra"), Seq(
    ("Biber-Kino-Center", "Biber-Kino-Center", "A0094", None, "Bebra"),
    ("Cineplex Bad Hersfeld", "Cineplex Bad Hersfeld", "A0765", Some("/kino/bad-hersfeld/cineplex-31958"), "Bad Hersfeld")
  ))
  private def p_bad_kissingen: R = ("bad-kissingen", "bad-kissingen-bayern", "Bad Kissingen", "Bayern", 50.2023, 10.0778, true, Seq("Bad Kissingen", "Hammelburg", "Bad Brückenau", "Bad Königshofen im Grabfeld"), Seq(
    ("Kino in der Stadtbibliothek Hammelburg", "Kino in der Stadtbibliothek Hammelburg", "A2348", None, "Hammelburg"),
    ("Rhön-Lichtspiele", "Rhön-Lichtspiele", "A2314", Some("/kino/bad-brueckenau/rhoenlichtspiele-31955"), "Bad Brückenau"),
    ("Stadtsaal-Lichtspiele", "Stadtsaal-Lichtspiele", "A1463", None, "Bad Königshofen im Grabfeld"),
    ("Universum Kino Palast", "Universum Kino Palast", "A1657", Some("/kino/bad-kissingen/universum-kino-palast-31962"), "Bad Kissingen")
  ))
  private def p_bad_kreuznach: R = ("bad-kreuznach", "bad-kreuznach-rheinland-pfalz", "Bad Kreuznach", "Rheinland-Pfalz", 49.8414, 7.8671, true, Seq("Bad Kreuznach", "Bingen am Rhein", "Ingelheim am Rhein", "Alzey", "Geisenheim", "Simmern", "Bad Sobernheim"), Seq(
    ("Bali", "Bali", "A1309", Some("/kino/alzey/bali-31948"), "Alzey"),
    ("Casablanca Ingelheim am Rhein", "Casablanca Ingelheim am Rhein", "A0024", None, "Ingelheim am Rhein"),
    ("Cineplex Bad Kreuznach", "Cineplex Bad Kreuznach", "A0308", Some("/kino/bad-kreuznach/cineplex-47137"), "Bad Kreuznach"),
    ("KiKuBi", "KiKuBi", "A0251", None, "Bingen am Rhein"),
    ("Linden Theater Geisenheim", "Linden Theater Geisenheim", "A1542", Some("/kino/geisenheim/lindentheater-32057"), "Geisenheim"),
    ("Pro-Winzkino", "Pro-Winzkino", "A1589", Some("/kino/simmern-hunsrueck/prowinzkino-32262"), "Simmern"),
    ("Rex Bad Sobernheim", "Rex Bad Sobernheim", "A1071", None, "Bad Sobernheim")
  ))
  private def p_bad_mergentheim: R = ("bad-mergentheim", "bad-mergentheim-baden-wuerttemberg", "Bad Mergentheim", "Baden-Württemberg", 49.4925, 9.7736, true, Seq("Bad Mergentheim", "Weikersheim", "Boxberg", "Creglingen", "Dörzbach", "Assamstadt"), Seq(
    ("Kinomobil Stuttgart - club-w-71", "Kinomobil Stuttgart - club-w-71", "A0798", Some("/kino/weikersheim/kinomobil-stuttgart-clubw71-41779"), "Weikersheim"),
    ("Kinomobil Stuttgart - Grundschule Assamstadt", "Kinomobil Stuttgart - Grundschule Assamstadt", "A1936", None, "Assamstadt"),
    ("Kinomobil Stuttgart - Kommunales Kino Creglingen", "Kinomobil Stuttgart - Kommunales Kino Creglingen", "A0800", Some("/kino/creglingen/kinomobil-stuttgart-41787"), "Creglingen"),
    ("Kinomobil Stuttgart - Medien und Kulturzentrum", "Kinomobil Stuttgart - Medien und Kulturzentrum", "A0783", None, "Boxberg"),
    ("Kinomobil Stuttgart - Schlosshof Schloss Eyb", "Kinomobil Stuttgart - Schlosshof Schloss Eyb", "A2227", None, "Dörzbach"),
    ("Movies Bad Mergentheim", "Movies Bad Mergentheim", "A2629", Some("/kino/bad-mergentheim/movies-81656"), "Bad Mergentheim")
  ))
  private def p_bad_neuenahr_ahrweiler: R = ("bad-neuenahr-ahrweiler", "bad-neuenahr-ahrweiler-rheinland-pfalz", "Bad Neuenahr-Ahrweiler", "Rheinland-Pfalz", 50.5432, 7.1113, true, Seq("Bad Neuenahr-Ahrweiler", "Wachtberg"), Seq(
    ("Drehwerk 1719", "Drehwerk 1719", "A2117", Some("/kino/wachtberg/drehwerk-1719-72977"), "Wachtberg"),
    ("Kino-Center Rhein-Ahr", "Kino-Center Rhein-Ahr", "A1115", None, "Bad Neuenahr-Ahrweiler")
  ))
  private def p_bad_neustadt_an_der_saale: R = ("bad-neustadt-an-der-saale", "bad-neustadt-an-der-saale-bayern", "Bad Neustadt an der Saale", "Bayern", 50.3217, 10.2067, false, Seq("Bad Neustadt an der Saale"), Seq(
    ("Rex Kinos", "Rex Kinos", "A1065", Some("/kino/bad-neustadt-an-der-saale/rex-kinos-31969"), "Bad Neustadt an der Saale"),
    ("Starlight Kinos", "Starlight Kinos", "A1149", Some("/kino/bad-neustadt-an-der-saale/starlight-kinos-31970"), "Bad Neustadt an der Saale")
  ))
  private def p_bad_oldesloe: R = ("bad-oldesloe", "bad-oldesloe-schleswig-holstein", "Bad Oldesloe", "Schleswig-Holstein", 53.8117, 10.3742, true, Seq("Bad Oldesloe", "Bad Schwartau", "Bad Segeberg", "Bargteheide", "Schönberg"), Seq(
    ("Blitz-Lichtspiele Schönberg", "Blitz-Lichtspiele Schönberg", "A1318", None, "Schönberg"),
    ("Cine Planet 5", "Cine Planet 5", "A1353", Some("/kino/bad-segeberg/cineplanet-5-32341"), "Bad Segeberg"),
    ("Kleines Theater", "Kleines Theater", "A0850", Some("/kino/bargteheide/kleines-theater-32343"), "Bargteheide"),
    ("Movie Star Bad Schwartau", "Movie Star Bad Schwartau", "A2326", Some("/kino/bad-schwartau/movie-star-74557"), "Bad Schwartau"),
    ("OHO-Kinocenter", "OHO-Kinocenter", "A1010", Some("/kino/bad-oldesloe/ohokinocenter-32336"), "Bad Oldesloe")
  ))
  private def p_bad_reichenhall: R = ("bad-reichenhall", "bad-reichenhall-bayern", "Bad Reichenhall", "Bayern", 47.7295, 12.8782, true, Seq("Bad Reichenhall", "Berchtesgaden", "Schönau am Königssee"), Seq(
    ("Kurkino im Kurhaus", "Kurkino im Kurhaus", "A0886", None, "Berchtesgaden"),
    ("Open Air Kino an der Sportanlage Schneewinkl am Königssee", "Open Air Kino an der Sportanlage Schneewinkl am Königssee", "A1977", Some("/kino/schoenau-am-koenigssee/open-air-kino-an-der-sportanlage-schneewinkl-am-koenigssee-68958"), "Schönau am Königssee"),
    ("Park-Kino", "Park-Kino", "A1585", Some("/kino/bad-reichenhall/parkkino-32686"), "Bad Reichenhall")
  ))
  private def p_bad_saulgau: R = ("bad-saulgau", "bad-saulgau-baden-wuerttemberg", "Bad Saulgau", "Baden-Württemberg", 48.0168, 9.5006, false, Seq("Bad Saulgau"), Seq(
    ("Autokino Bad Saulgau am Fesplatz", "Autokino Bad Saulgau am Fesplatz", "A2922", None, "Bad Saulgau"),
    ("Kino Saulgau", "Kino Saulgau", "A0750", Some("/kino/bad-saulgau/kino-saulgau-32973"), "Bad Saulgau")
  ))
  private def p_bad_schwalbach: R = ("bad-schwalbach", "bad-schwalbach-hessen", "Bad Schwalbach", "Hessen", 50.142, 8.0696, true, Seq("Bad Schwalbach", "Nastätten"), Seq(
    ("Bambi &amp; Camera", "Bambi &amp; Camera", "A1311", Some("/kino/bad-schwalbach/bambi-und-camera-88337"), "Bad Schwalbach"),
    ("Kinocenter Nastätten", "Kinocenter Nastätten", "A0774", None, "Nastätten")
  ))
  private def p_bad_toelz: R = ("bad-toelz", "bad-toelz-bayern", "Bad Tölz", "Bayern", 47.7611, 11.5589, false, Seq("Bad Tölz"), Seq(
    ("Capitol Filmtheater Bad Tölz", "Capitol Filmtheater Bad Tölz", "A0142", None, "Bad Tölz"),
    ("Isar-Kinocenter", "Isar-Kinocenter", "A0628", Some("/kino/bad-toelz/isarkinocenter-37943"), "Bad Tölz")
  ))
  private def p_bad_urach: R = ("bad-urach", "bad-urach-baden-wuerttemberg", "Bad Urach", "Baden-Württemberg", 48.4911, 9.4001, false, Seq("Bad Urach"), Seq(
    ("Bad Uracher Sommer Open Air", "Bad Uracher Sommer Open Air", "A2534", None, "Bad Urach"),
    ("Forum 22", "Forum 22", "A1731", Some("/kino/bad-urach/forum-22-32688"), "Bad Urach")
  ))
  private def p_bad_vilbel: R = ("bad-vilbel", "bad-vilbel-hessen", "Bad Vilbel", "Hessen", 50.1787, 8.7376, true, Seq("Bad Vilbel", "Karben"), Seq(
    ("Cinepark Karben", "Cinepark Karben", "A0303", Some("/kino/karben/cinepark-38103"), "Karben"),
    ("Kino Alte Mühle", "Kino Alte Mühle", "A0700", Some("/kino/bad-vilbel/kino-alte-muehle-44496"), "Bad Vilbel"),
    ("Open Air Kino im Bad Vilbeler Freibad", "Open Air Kino im Bad Vilbeler Freibad", "A1933", Some("/kino/bad-vilbel/open-air-kino-im-bad-vilbeler-freibad-36307"), "Bad Vilbel")
  ))
  private def p_bad_wildbad: R = ("bad-wildbad", "bad-wildbad-baden-wuerttemberg", "Bad Wildbad", "Baden-Württemberg", 48.7507, 8.5504, true, Seq("Bad Wildbad", "Schömberg"), Seq(
    ("KiWi-Kino", "KiWi-Kino", "A0847", Some("/kino/bad-wildbad/kiwi-kino-86258"), "Bad Wildbad"),
    ("Kurtheater Schömberg", "Kurtheater Schömberg", "A0888", Some("/kino/schoemberg/kurtheater-32249"), "Schömberg")
  ))
  private def p_bad_woerishofen: R = ("bad-woerishofen", "bad-woerishofen-bayern", "Bad Wörishofen", "Bayern", 48.0067, 10.5967, true, Seq("Bad Wörishofen", "Türkheim"), Seq(
    ("Filmhaus", "Filmhaus", "A1524", Some("/kino/tuerkheim/filmhaushuber-in-tuerkheim-33034"), "Türkheim"),
    ("Filmhaus Bad Wörishofen - Lichtspiele am Bahnhof", "Filmhaus Bad Wörishofen - Lichtspiele am Bahnhof", "A1537", None, "Bad Wörishofen")
  ))
  private def p_baden_baden: R = ("baden-baden", "baden-baden-baden-wuerttemberg", "Baden-Baden", "Baden-Württemberg", 48.7606, 8.2398, true, Seq("Baden-Baden", "Gaggenau", "Achern", "Gernsbach"), Seq(
    ("Cineplex Baden-Baden", "Cineplex Baden-Baden", "A2689", Some("/kino/badenbaden/cineplex-86277"), "Baden-Baden"),
    ("moviac - Kino im Kaiserhof", "moviac - Kino im Kaiserhof", "A2628", Some("/kino/badenbaden/moviac-kino-im-kaiserhof-82097"), "Baden-Baden"),
    ("Kinocenter Gernsbach", "Kinocenter Gernsbach", "A0771", Some("/kino/gernsbach/kinocenter-32061"), "Gernsbach"),
    ("Merkur-Kino-Center", "Merkur-Kino-Center", "A0944", Some("/kino/gaggenau/merkurkinocenter-32055"), "Gaggenau"),
    ("Tivoli-Filmtheater", "Tivoli-Filmtheater", "A1185", Some("/kino/achern/tivoli-kommunales-kino-31945"), "Achern")
  ))
  private def p_bamberg: R = ("bamberg", "bamberg-bayern", "Bamberg", "Bayern", 49.8987, 10.9007, false, Seq("Bamberg"), Seq(
    ("Lichtspiel, Kino &amp; Café", "Lichtspiel, Kino &amp; Café", "A1535", Some("/kino/bamberg/lichtspiel-kino-und-caf%C3%A9-32694"), "Bamberg"),
    ("Odeon Lichtspiel, Kino &amp; Café", "Odeon Lichtspiel, Kino &amp; Café", "A1005", Some("/kino/bamberg/odeon-lichtspiel-kino-und-caf%C3%A9-48482"), "Bamberg"),
    ("Open Air Kino Hainbad", "Open Air Kino Hainbad", "A1983", None, "Bamberg"),
    ("Sommerkino im Schloss Geyerswörth", "Sommerkino im Schloss Geyerswörth", "A2821", Some("/kino/bamberg/sommerkino-im-schloss-geyerswoerth-81417"), "Bamberg")
  ))
  private def p_baunatal: R = ("baunatal", "baunatal-hessen", "Baunatal", "Hessen", 51.2518, 9.4075, true, Seq("Baunatal", "Bad Wildungen", "Fritzlar", "Melsungen", "Wolfhagen"), Seq(
    ("Cinema Wolfhagen", "Cinema Wolfhagen", "A2790", Some("/kino/wolfhagen/cinema-32310"), "Wolfhagen"),
    ("Cineplex Baunatal", "Cineplex Baunatal", "A2704", Some("/kino/baunatal/cineplex-87219"), "Baunatal"),
    ("Cineplex Royal Fritzlar", "Cineplex Royal Fritzlar", "A0221", Some("/kino/fritzlar/cineplex-royal-38939"), "Fritzlar"),
    ("Freilichtkino Melsungen", "Freilichtkino Melsungen", "A2193", Some("/kino/melsungen/freilichtkino-melsungen-73674"), "Melsungen"),
    ("Wandelhalle Reinhardshausen", "Wandelhalle Reinhardshausen", "A2726", Some("/kino/bad-wildungen/wandelhalle-reinhardshausen-87357"), "Bad Wildungen")
  ))
  private def p_bautzen: R = ("bautzen", "bautzen-sachsen", "Bautzen", "Sachsen", 51.1803, 14.4349, false, Seq("Bautzen"), Seq(
    ("Filmpalast Bautzen", "Filmpalast Bautzen", "A0507", Some("/kino/bautzen/filmpalast-31354"), "Bautzen"),
    ("Freiluftkino am Spreebogen Bautzen", "Freiluftkino am Spreebogen Bautzen", "A2697", Some("/kino/bautzen/freiluftkino-am-spreebogen-86597"), "Bautzen"),
    ("Open Air Kino im Hof der Ortenburg", "Open Air Kino im Hof der Ortenburg", "A2237", None, "Bautzen"),
    ("Open-Air Kino im Freihof Gedenkstätte", "Open-Air Kino im Freihof Gedenkstätte", "A2827", Some("/kino/bautzen/openair-kino-im-freihof-gedenkstaette-79317"), "Bautzen"),
    ("Steinhaus", "Steinhaus", "A2231", Some("/kino/bautzen/steinhaus-73647"), "Bautzen")
  ))
  private def p_bayreuth: R = ("bayreuth", "bayreuth-bayern", "Bayreuth", "Bayern", 49.9478, 11.5789, true, Seq("Bayreuth", "Kulmbach", "Pegnitz", "Hollfeld"), Seq(
    ("Cineplex Bayreuth", "Cineplex Bayreuth", "A0314", Some("/kino/bayreuth/cineplex-53296"), "Bayreuth"),
    ("Cineplex Kulmbach", "Cineplex Kulmbach", "A0764", Some("/kino/kulmbach/cineplex-32819"), "Kulmbach"),
    ("Kintopp", "Kintopp", "A1517", Some("/kino/hollfeld/kintopp-32795"), "Hollfeld"),
    ("Regina-Filmtheater", "Regina-Filmtheater", "cineamo:1232", None, "Pegnitz")
  ))
  private def p_bernau: R = ("bernau", "bernau-brandenburg", "Bernau", "Brandenburg", 52.6798, 13.5871, false, Seq("Bernau"), Seq(
    ("Filmpalast Bernau bei Berlin", "Filmpalast Bernau bei Berlin", "A0512", Some("/kino/bernau/filmpalast-31452"), "Bernau")
  ))
  private def p_bernburg: R = ("bernburg", "bernburg-sachsen-anhalt", "Bernburg", "Sachsen-Anhalt", 51.7946, 11.7401, true, Seq("Bernburg", "Köthen", "Aschersleben"), Seq(
    ("Cine Circus", "Cine Circus", "A0218", None, "Köthen"),
    ("Filmpalast Aschersleben", "Filmpalast Aschersleben", "A0356", Some("/kino/aschersleben/filmpalast-31339"), "Aschersleben"),
    ("Filmtheater Capitol Bernburg", "Filmtheater Capitol Bernburg", "A0542", Some("/kino/bernburg-saale/filmtheater-capitol-31453"), "Bernburg")
  ))
  private def p_besigheim: R = ("besigheim", "besigheim-baden-wuerttemberg", "Besigheim", "Baden-Württemberg", 48.998, 9.1427, true, Seq("Besigheim", "Lauffen", "Löchgau", "Kirchheim am Neckar", "Walheim", "Erligheim"), Seq(
    ("Kinomobil Stuttgart - Alte Kelter", "Kinomobil Stuttgart - Alte Kelter", "A0793", Some("/kino/besigheim/kinomobil-stuttgart-stadthalle-alte-kelter-41595"), "Besigheim"),
    ("Kinomobil Stuttgart - Bürgerhaus Vordere Kelter", "Kinomobil Stuttgart - Bürgerhaus Vordere Kelter", "A0807", Some("/kino/erligheim/kinomobil-stuttgart-buergerhaus-vordere-kelter-42549"), "Erligheim"),
    ("Kinomobil Stuttgart - Gemeindehalle Löchgau", "Kinomobil Stuttgart - Gemeindehalle Löchgau", "A0797", Some("/kino/loechgau/kinomobil-stuttgart-gemeindehalle-41601"), "Löchgau"),
    ("Kinomobil Stuttgart - Kulturzentrum Alte Schule", "Kinomobil Stuttgart - Kulturzentrum Alte Schule", "A0785", None, "Kirchheim am Neckar"),
    ("Kinomobil Stuttgart - Römerhaus", "Kinomobil Stuttgart - Römerhaus", "A1954", None, "Walheim"),
    ("Kinomobil Stuttgart - Stadthalle Lauffen", "Kinomobil Stuttgart - Stadthalle Lauffen", "A0788", None, "Lauffen")
  ))
  private def p_biberach_an_der_riss: R = ("biberach-an-der-riss", "biberach-an-der-riss-baden-wuerttemberg", "Biberach an der Riß", "Baden-Württemberg", 48.0934, 9.7905, true, Seq("Biberach an der Riß", "Ehingen", "Bad Waldsee"), Seq(
    ("Central Kino Center", "Central Kino Center", "A0187", Some("/kino/ehingen/central-kino-center-64336"), "Ehingen"),
    ("seenema - Stadtkino Bad Waldsee eG", "seenema - Stadtkino Bad Waldsee eG", "A2722", Some("/kino/bad-waldsee/seenema-stadtkino-bad-waldsee-eg-87297"), "Bad Waldsee"),
    ("Cineplex Biberach", "Cineplex Biberach", "cineamo:668", None, "Biberach an der Riß")
  ))
  private def p_bocholt: R = ("bocholt", "bocholt-nordrhein-westfalen", "Bocholt", "Nordrhein-Westfalen", 51.8388, 6.6153, true, Seq("Bocholt", "Wesel", "Borken"), Seq(
    ("Comet Kinos Wesel", "Comet Kinos Wesel", "A0420", Some("/kino/wesel/comet-cine-center-49916"), "Wesel"),
    ("Kinodrom Multiplex", "Kinodrom Multiplex", "A0777", None, "Bocholt"),
    ("Kino-Center Borken", "Kino-Center Borken", "A0766", None, "Borken")
  ))
  private def p_boeblingen: R = ("boeblingen", "boeblingen-baden-wuerttemberg", "Böblingen", "Baden-Württemberg", 48.6821, 9.0117, false, Seq("Böblingen"), Seq(
    ("Filmzentrum Bären", "Filmzentrum Bären", "A0953", Some("/kino/boeblingen/filmzentrum-baeren-32704"), "Böblingen"),
    ("Metropol am Postplatz", "Metropol am Postplatz", "A2741", Some("/kino/boeblingen/metropol-am-postplatz-88557"), "Böblingen"),
    ("Open Air Böblinger See", "Open Air Böblinger See", "A2035", None, "Böblingen")
  ))
  private def p_bramsche: R = ("bramsche", "bramsche-niedersachsen", "Bramsche", "Niedersachsen", 52.4084, 7.9833, true, Seq("Bramsche", "Damme", "Ankum"), Seq(
    ("Dersa Kino-Center", "Dersa Kino-Center", "A0438", Some("/kino/damme-duemmer/dersa-kinocenter-32392"), "Damme"),
    ("Gloria Kinocenter", "Gloria Kinocenter", "A0576", Some("/kino/ankum/gloria-kinocenter-32326"), "Ankum"),
    ("Universum e.V.", "Universum e.V.", "A1655", Some("/kino/bramsche/filmtheater-universum-33140"), "Bramsche")
  ))
  private def p_brandenburg: R = ("brandenburg", "brandenburg-brandenburg", "Brandenburg", "Brandenburg", 52.4167, 12.55, false, Seq("Brandenburg"), Seq(
    ("Concerthaus Kino", "Concerthaus Kino", "A0421", Some("/kino/brandenburg-an-der-havel/concerthaus-kino-31464"), "Brandenburg"),
    ("Fontane Klub", "Fontane Klub", "A1930", Some("/kino/brandenburg-an-der-havel/fontane-klub-71552"), "Brandenburg"),
    ("Movietown Brandenburg", "Movietown Brandenburg", "cineamo:330", None, "Brandenburg")
  ))
  private def p_bruchsal: R = ("bruchsal", "bruchsal-baden-wuerttemberg", "Bruchsal", "Baden-Württemberg", 49.1243, 8.598, true, Seq("Bruchsal", "Bretten"), Seq(
    ("Cineplex Bruchsal", "Cineplex Bruchsal", "A1373", Some("/kino/bruchsal/cineplex-bruchsal-31996"), "Bruchsal"),
    ("Kinostar Filmwelt Bretten", "Kinostar Filmwelt Bretten", "A0508", Some("/kino/bretten/kinostar-filmwelt-34431"), "Bretten")
  ))
  private def p_brunsbuettel: R = ("brunsbuettel", "brunsbuettel-schleswig-holstein", "Brunsbüttel", "Schleswig-Holstein", 53.8962, 9.1046, false, Seq("Brunsbüttel"), Seq(
    ("Metropol Brunsbüttel 1", "Metropol Brunsbüttel 1", "A1565", None, "Brunsbüttel")
  ))
  private def p_buchholz_in_der_nordheide: R = ("buchholz-in-der-nordheide", "buchholz-in-der-nordheide-niedersachsen", "Buchholz in der Nordheide", "Niedersachsen", 53.3304, 9.866, true, Seq("Buchholz in der Nordheide", "Neu Wulmstorf"), Seq(
    ("Das Kino - Neu Wulmstorf", "Das Kino - Neu Wulmstorf", "A1334", Some("/kino/neu-wulmstorf/das-kino-32550"), "Neu Wulmstorf"),
    ("Movieplexx Delhi - Center", "Movieplexx Delhi - Center", "A0971", None, "Buchholz in der Nordheide")
  ))
  private def p_buedingen: R = ("buedingen", "buedingen-hessen", "Büdingen", "Hessen", 50.2901, 9.1114, true, Seq("Büdingen", "Gelnhausen", "Nidda"), Seq(
    ("Kino Gelnhausen (Pali und Casino)", "Kino Gelnhausen (Pali und Casino)", "A1028", Some("/kino/gelnhausen/casino-32058"), "Gelnhausen"),
    ("Lumos Lichtspiele &amp; Lounge", "Lumos Lichtspiele &amp; Lounge", "A2683", Some("/kino/nidda/lumos-lichtspiel-und-lounge-86137"), "Nidda"),
    ("Novum Kino", "Novum Kino", "A0112", Some("/kino/buedingen/novum-kino-buedingen-31999"), "Büdingen")
  ))
  private def p_buende: R = ("buende", "buende-nordrhein-westfalen", "Bünde", "Nordrhein-Westfalen", 52.1984, 8.5864, false, Seq("Bünde"), Seq(
    ("Autokino Bünder Lichtspiele am Festplatz", "Autokino Bünder Lichtspiele am Festplatz", "A2923", None, "Bünde"),
    ("Bünder Lichtspiele", "Bünder Lichtspiele", "A0113", Some("/kino/buende/buender-lichtspiele-31782"), "Bünde"),
    ("Universum Bünde", "Universum Bünde", "A1242", Some("/kino/buende/universum-45090"), "Bünde")
  ))
  private def p_burg: R = ("burg", "burg-sachsen-anhalt", "Burg", "Sachsen-Anhalt", 52.2715, 11.8549, false, Seq("Burg"), Seq(
    ("Burg-Theater Burg", "Burg-Theater Burg", "A0124", None, "Burg")
  ))
  private def p_burghausen: R = ("burghausen", "burghausen-bayern", "Burghausen", "Bayern", 48.1692, 12.8314, false, Seq("Burghausen"), Seq(
    ("Anker-Filmtheater", "Anker-Filmtheater", "A1490", None, "Burghausen"),
    ("Quadroscope", "Quadroscope", "A1147", Some("/kino/burghausen/quadroscope-45538"), "Burghausen")
  ))
  private def p_butzbach: R = ("butzbach", "butzbach-hessen", "Butzbach", "Hessen", 50.434, 8.6712, true, Seq("Butzbach", "Bad Nauheim", "Lich", "Weilmünster"), Seq(
    ("Butzbacher Filmtheater", "Butzbacher Filmtheater", "A0154", Some("/kino/butzbach/butzbacher-filmtheater-32003"), "Butzbach"),
    ("Open-Air-Kino im Landgrafenschloss", "Open-Air-Kino im Landgrafenschloss", "A2013", Some("/kino/butzbach/openairkino-im-landgrafenschloss-51458"), "Butzbach"),
    ("Filmbühne Bad Nauheim", "Filmbühne Bad Nauheim", "G01HV", Some("/kino/bad-nauheim/filmbuehne-31968"), "Bad Nauheim"),
    ("Saalbau-Lichtspiele Weilmünster", "Saalbau-Lichtspiele Weilmünster", "A1611", None, "Weilmünster"),
    ("Traumstern", "Traumstern", "A1190", Some("/kino/lich/traumstern-32148"), "Lich")
  ))
  private def p_celle: R = ("celle", "celle-niedersachsen", "Celle", "Niedersachsen", 52.6226, 10.0805, false, Seq("Celle"), Seq(
    ("Achteinhalb", "Achteinhalb", "A0005", Some("/kino/celle/kino-achteinhalb-46417"), "Celle"),
    ("Kammer-Lichtspiele / Filmpalast Celle", "Kammer-Lichtspiele / Filmpalast Celle", "A0638", Some("/kino/celle/kammerlichtspiele-37219"), "Celle"),
    ("Open Air Kino", "Open Air Kino", "A2869", None, "Celle")
  ))
  private def p_cham: R = ("cham", "cham-bayern", "Cham", "Bayern", 49.2257, 12.655, false, Seq("Cham"), Seq(
    ("Cine-World", "Cine-World", "A0389", Some("/kino/cham/cineworld-e.k.-35162"), "Cham")
  ))
  private def p_cloppenburg: R = ("cloppenburg", "cloppenburg-niedersachsen", "Cloppenburg", "Niedersachsen", 52.8475, 8.0474, true, Seq("Cloppenburg", "Vechta", "Lohne", "Diepholz"), Seq(
    ("Capitol-Kinocenter Lohne (Oldenburg)", "Capitol-Kinocenter Lohne (Oldenburg)", "A0162", None, "Lohne"),
    ("Central Diepholz", "Central Diepholz", "A1344", None, "Diepholz"),
    ("CineCenter Cloppenburg", "CineCenter Cloppenburg", "A1354", Some("/kino/cloppenburg/cinecenter-33130"), "Cloppenburg"),
    ("Schauburg Cineworld Vechta", "Schauburg Cineworld Vechta", "A1118", Some("/kino/vechta/schauburg-cineworld-33163"), "Vechta")
  ))
  private def p_coburg: R = ("coburg", "coburg-bayern", "Coburg", "Bayern", 50.2594, 10.9638, true, Seq("Coburg", "Sonneberg", "Lichtenfels", "Kronach"), Seq(
    ("Filmburg Kronach", "Filmburg Kronach", "A1410", Some("/kino/kronach/filmburg-32817"), "Kronach"),
    ("Kammer Lichtspiele", "Kammer Lichtspiele", "A0640", Some("/kino/sonneberg-thueringen/kammer-lichtspiele-31673"), "Sonneberg"),
    ("Neue Filmbühne Lichtenfels", "Neue Filmbühne Lichtenfels", "A0981", Some("/kino/lichtenfels/neue-filmbuehne-38563"), "Lichtenfels"),
    ("Utopolis", "Utopolis", "A1252", Some("/kino/coburg/utopolis-43397"), "Coburg")
  ))
  private def p_cochem: R = ("cochem", "cochem-rheinland-pfalz", "Cochem", "Rheinland-Pfalz", 50.1451, 7.1638, true, Seq("Cochem", "Zell"), Seq(
    ("Apollo Cochern", "Apollo Cochern", "A0041", Some("/kino/cochem/apollo-31783"), "Cochem"),
    ("KulturKino Kaimt", "KulturKino Kaimt", "kinoheld:3617", None, "Zell")
  ))
  private def p_cottbus: R = ("cottbus", "cottbus-brandenburg", "Cottbus", "Brandenburg", 51.7577, 14.3289, false, Seq("Cottbus"), Seq(
    ("Kino in der Stadhalle Cottbus", "Kino in der Stadhalle Cottbus", "A2370", Some("/kino/cottbus/kino-in-der-stadthalle-75063"), "Cottbus"),
    ("Obenkino im Glad-House", "Obenkino im Glad-House", "A1000", Some("/kino/cottbus/obenkino-im-gladhouse-43956"), "Cottbus"),
    ("UCI am Lausitz Park", "UCI am Lausitz Park", "A1204", Some("/kino/cottbus/uci-kinowelt-am-lausitz-park-43176"), "Cottbus"),
    ("Weltspiegel Cottbus", "Weltspiegel Cottbus", "A1663", Some("/kino/cottbus/weltspiegel-31475"), "Cottbus")
  ))
  private def p_crailsheim: R = ("crailsheim", "crailsheim-baden-wuerttemberg", "Crailsheim", "Baden-Württemberg", 49.1344, 10.0719, true, Seq("Crailsheim", "Ellwangen", "Feuchtwangen", "Ilshofen", "Kirchberg an der Jagst"), Seq(
    ("Cinecity", "Cinecity", "A0224", Some("/kino/crailsheim/cinecity-und-kammer-filmtheater-42476"), "Crailsheim"),
    ("Kammer-Filmtheater Premium-Kino Crailsheim", "Kammer-Filmtheater Premium-Kino Crailsheim", "A0641", Some("/kino/crailsheim/kammerfilmtheater-premiumkino-88777"), "Crailsheim"),
    ("Kino Klappe", "Kino Klappe", "A0744", None, "Kirchberg an der Jagst"),
    ("Kino Regina Ellwangen", "Kino Regina Ellwangen", "A1590", Some("/kino/ellwangen/kino-regina-32734"), "Ellwangen"),
    ("Kinomobil Stuttgart - Roland-Wurmthaler-Halle", "Kinomobil Stuttgart - Roland-Wurmthaler-Halle", "A0789", None, "Ilshofen"),
    ("KulturKino Feuchtwangen", "KulturKino Feuchtwangen", "A1054", None, "Feuchtwangen")
  ))
  private def p_cuxhaven: R = ("cuxhaven", "cuxhaven-niedersachsen", "Cuxhaven", "Niedersachsen", 53.8683, 8.699, false, Seq("Cuxhaven"), Seq(
    ("Bali-Service-Kino", "Bali-Service-Kino", "A0009", None, "Cuxhaven")
  ))
  private def p_dahme: R = ("dahme", "dahme-brandenburg", "Dahme", "Brandenburg", 51.8701, 13.4274, false, Seq("Dahme"), Seq(
    ("Kino-Cafe Dahme", "Kino-Cafe Dahme", "A0836", Some("/kino/dahme-holstein/kino-cafe-31477"), "Dahme")
  ))
  private def p_daun: R = ("daun", "daun-rheinland-pfalz", "Daun", "Rheinland-Pfalz", 50.1972, 6.8294, true, Seq("Daun", "Prüm", "Hillesheim"), Seq(
    ("Eifel-Film-Bühne", "Eifel-Film-Bühne", "A0458", Some("/kino/hillesheim/eifelfilmbuehne-31822"), "Hillesheim"),
    ("Eifel-Kinocenter", "Eifel-Kinocenter", "A0407", Some("/kino/pruem/eifelkinocenter-31896"), "Prüm"),
    ("Kinopalast Vulkaneifel", "Kinopalast Vulkaneifel", "A0822", Some("/kino/daun/kinopalast-vulkaneifel-31786"), "Daun")
  ))
  private def p_deggendorf: R = ("deggendorf", "deggendorf-bayern", "Deggendorf", "Bayern", 48.8409, 12.9607, true, Seq("Deggendorf", "Plattling", "Regen", "Zwiesel", "Viechtach"), Seq(
    ("CinePalast Regen", "CinePalast Regen", "A1460", Some("/kino/regen/cinepalast-32946"), "Regen"),
    ("Filmtheater Zwiesel", "Filmtheater Zwiesel", "A0704", Some("/kino/zwiesel/filmtheater-57576"), "Zwiesel"),
    ("Focus Cinemas Plattling", "Focus Cinemas Plattling", "A0552", Some("/kino/plattling/focus-cinemas-37107"), "Plattling"),
    ("Lichtspielhaus Deggendorf", "Lichtspielhaus Deggendorf", "A0910", Some("/kino/deggendorf/lichtspielhaus-32720"), "Deggendorf"),
    ("Neue Post Lichtspiele", "Neue Post Lichtspiele", "A0983", None, "Viechtach")
  ))
  private def p_delmenhorst: R = ("delmenhorst", "delmenhorst-niedersachsen", "Delmenhorst", "Niedersachsen", 53.0511, 8.6309, true, Seq("Delmenhorst", "Syke", "Schwanewede"), Seq(
    ("Film Palast", "Film Palast", "A1407", Some("/kino/schwanewede/filmpalast-32612"), "Schwanewede"),
    ("Hansa Kino", "Hansa Kino", "A1447", Some("/kino/syke/hansa-kino-32621"), "Syke"),
    ("MaxX", "MaxX", "A0266", Some("/kino/delmenhorst/maxx-57896"), "Delmenhorst")
  ))
  private def p_demmin: R = ("demmin", "demmin-mecklenburg-vorpommern", "Demmin", "Mecklenburg-Vorpommern", 53.9076, 13.0314, false, Seq("Demmin"), Seq(
    ("Filmeck Demmin", "Filmeck Demmin", "A2382", Some("/kino/demmin/filmeck-31479"), "Demmin")
  ))
  private def p_dessau_rosslau: R = ("dessau-rosslau", "dessau-rosslau-sachsen-anhalt", "Dessau-Roßlau", "Sachsen-Anhalt", 51.8386, 12.2455, false, Seq("Dessau-Roßlau"), Seq(
    ("Kiez-Kino", "Kiez-Kino", "A2849", None, "Dessau-Roßlau"),
    ("Open Air am Landhaus Dessau", "Open Air am Landhaus Dessau", "A2702", None, "Dessau-Roßlau"),
    ("UCI Dessau", "UCI Dessau", "A2890", Some("/kino/dessau-anhalt/uci-kinowelt-dessau-39078"), "Dessau-Roßlau")
  ))
  private def p_detmold: R = ("detmold", "detmold-nordrhein-westfalen", "Detmold", "Nordrhein-Westfalen", 51.9385, 8.8732, true, Seq("Detmold", "Lage", "Bad Pyrmont"), Seq(
    ("Filmwelt Lippe", "Filmwelt Lippe", "A2647", Some("/kino/lage/filmwelt-lippe-83457"), "Lage"),
    ("Kaiserhof+ Einfach Gutes Kino", "Kaiserhof+ Einfach Gutes Kino", "A0632", None, "Detmold"),
    ("Open-Air Mondscheinkino Waldbühne", "Open-Air Mondscheinkino Waldbühne", "A2010", Some("/kino/detmold/openair-mondscheinkino-waldbuehne-am-hermannsdenkmal-71538"), "Detmold"),
    ("Kronenlichtspiele", "Kronenlichtspiele", "A1525", Some("/kino/bad-pyrmont/kronenlichtspiele-32337"), "Bad Pyrmont")
  ))
  private def p_diessen_am_ammersee: R = ("diessen-am-ammersee", "diessen-am-ammersee-bayern", "Diessen am Ammersee", "Bayern", 47.9509, 11.1031, false, Seq("Diessen am Ammersee"), Seq(
    ("Cinema Augustinum Diessen am Ammersee", "Cinema Augustinum Diessen am Ammersee", "A2714", None, "Diessen am Ammersee"),
    ("Kinowelt am Ammersee", "Kinowelt am Ammersee", "A0841", None, "Diessen am Ammersee")
  ))
  private def p_dillenburg: R = ("dillenburg", "dillenburg-hessen", "Dillenburg", "Hessen", 50.7411, 8.287, false, Seq("Dillenburg"), Seq(
    ("Movie Star Dillenburg", "Movie Star Dillenburg", "A0579", Some("/kino/dillenburg/movie-star-77878"), "Dillenburg")
  ))
  private def p_dillingen_an_der_donau: R = ("dillingen-an-der-donau", "dillingen-an-der-donau-bayern", "Dillingen an der Donau", "Bayern", 48.5815, 10.4953, false, Seq("Dillingen an der Donau"), Seq(
    ("Autokino Dillingen", "Autokino Dillingen", "A2926", None, "Dillingen an der Donau"),
    ("Filmcenter Dillingen", "Filmcenter Dillingen", "A0492", Some("/kino/dillingen-an-der-donau/filmcenter-dillingen-32722"), "Dillingen an der Donau")
  ))
  private def p_dingolfing: R = ("dingolfing", "dingolfing-bayern", "Dingolfing", "Bayern", 48.6424, 12.4928, true, Seq("Dingolfing", "Landau"), Seq(
    ("Cinema Filmpalais", "Cinema Filmpalais", "A1363", Some("/kino/dingolfing/cinema-filmpalais-32723"), "Dingolfing"),
    ("KuKi - Das Kultkino", "KuKi - Das Kultkino", "A2623", None, "Landau")
  ))
  private def p_dinslaken: R = ("dinslaken", "dinslaken-nordrhein-westfalen", "Dinslaken", "Nordrhein-Westfalen", 51.5623, 6.7434, true, Seq("Dinslaken", "Kamp-Lintfort", "Geldern"), Seq(
    ("Hall of Fame Kamp-Lintfort", "Hall of Fame Kamp-Lintfort", "A2899", Some("/kino/kamplintfort/hall-of-fame-91817"), "Kamp-Lintfort"),
    ("Herzog-Theater", "Herzog-Theater", "A0687", Some("/kino/geldern/herzog-theater-48156"), "Geldern"),
    ("Lichtburg Dinslaken", "Lichtburg Dinslaken", "A2882", Some("/kino/dinslaken/lichtburg-center-177"), "Dinslaken")
  ))
  private def p_donaueschingen: R = ("donaueschingen", "donaueschingen-baden-wuerttemberg", "Donaueschingen", "Baden-Württemberg", 47.9551, 8.4971, true, Seq("Donaueschingen", "Titisee-Neustadt", "Lenzkirch", "Sankt Blasien"), Seq(
    ("Kino im Kursaal", "Kino im Kursaal", "A2802", None, "Sankt Blasien"),
    ("Kommunales Kino Guckloch Donaueschingen", "Kommunales Kino Guckloch Donaueschingen", "A0864", Some("/kino/donaueschingen/kommunales-kino-guckloch-93095"), "Donaueschingen"),
    ("Krone-Theater", "Krone-Theater", "A0873", Some("/kino/titiseeneustadt/kronetheater-32277"), "Titisee-Neustadt"),
    ("Kultur im Kino", "Kultur im Kino", "A0889", None, "Lenzkirch")
  ))
  private def p_dorfen: R = ("dorfen", "dorfen-bayern", "Dorfen", "Bayern", 48.2704, 12.1606, true, Seq("Dorfen", "Taufkirchen (Vils)"), Seq(
    ("Kinocafe", "Kinocafe", "A0393", Some("/kino/taufkirchen-vils/kinocafe-33021"), "Taufkirchen (Vils)"),
    ("sKino im Jakobmayer", "sKino im Jakobmayer", "A2671", Some("/kino/dorfen/skino-im-jakobmayer-80078"), "Dorfen"),
    ("Kino in Dorfen", "Kino in Dorfen", "kinoheld:2323", None, "Dorfen")
  ))
  private def p_dreieich: R = ("dreieich", "dreieich-hessen", "Dreieich", "Hessen", 50.02, 8.6961, true, Seq("Dreieich", "Mörfelden-Walldorf", "Langen", "Neu-Isenburg"), Seq(
    ("Drive In Autokino Gravenbruch", "Drive In Autokino Gravenbruch", "A1398", Some("/kino/neuisenburg/drive-in-autokino-frankfurt-gravenbruch-32193"), "Neu-Isenburg"),
    ("LichtBlick Mörfelden-Walldorf", "LichtBlick Mörfelden-Walldorf", "A0900", Some("/kino/moerfeldenwalldorf/lichtblick-32185"), "Mörfelden-Walldorf"),
    ("Open Air Kino am Rathaus Walldorf", "Open Air Kino am Rathaus Walldorf", "A2256", None, "Mörfelden-Walldorf"),
    ("Lichtburg Kinos", "Lichtburg Kinos", "A1534", None, "Langen"),
    ("Rex-Palast", "Rex-Palast", "A1074", Some("/kino/dreieich/rex-palast-32013"), "Dreieich"),
    ("Viktoria-Theater", "Viktoria-Theater", "A1257", Some("/kino/dreieich/viktoriatheater-53758"), "Dreieich")
  ))
  private def p_duderstadt: R = ("duderstadt", "duderstadt-niedersachsen", "Duderstadt", "Niedersachsen", 51.5131, 10.2595, true, Seq("Duderstadt", "Herzberg am Harz"), Seq(
    ("Central-Lichtspiele Herzberg am Harz", "Central-Lichtspiele Herzberg am Harz", "A1351", Some("/kino/herzberg-am-harz/centrallichtspiele-32488"), "Herzberg am Harz"),
    ("Movietown Eichsfeld", "Movietown Eichsfeld", "A1624", Some("/kino/duderstadt/movietown-eichsfeld-32403"), "Duderstadt")
  ))
  private def p_duelmen: R = ("duelmen", "duelmen-nordrhein-westfalen", "Dülmen", "Nordrhein-Westfalen", 51.8315, 7.2808, true, Seq("Dülmen", "Coesfeld"), Seq(
    ("Cinema Coesfeld (Coesfeld)", "Cinema Coesfeld (Coesfeld)", "A0229", Some("/kino/coesfeld/cinema-64898"), "Coesfeld"),
    ("Cinema Coesfeld (Dülmen)", "Cinema Coesfeld (Dülmen)", "A0249", None, "Dülmen")
  ))
  private def p_dueren: R = ("dueren", "dueren-nordrhein-westfalen", "Düren", "Nordrhein-Westfalen", 50.8043, 6.493, true, Seq("Düren", "Eschweiler"), Seq(
    ("Das Lumen Filmtheater", "Das Lumen Filmtheater", "A0418", Some("/kino/dueren-rheinland/das-lumen-filmtheater-35439"), "Düren"),
    ("Primus-Kinocenter", "Primus-Kinocenter", "A1041", Some("/kino/eschweiler/primuskinocenter-31796"), "Eschweiler")
  ))
  private def p_ebersbach: R = ("ebersbach", "ebersbach-sachsen", "Ebersbach", "Sachsen", 50.9944, 14.6, true, Seq("Ebersbach", "Neustadt in Sachsen", "Mittelherwigsdorf", "Großhennersdorf"), Seq(
    ("Film Theater Ebersbach", "Film Theater Ebersbach", "A0539", None, "Ebersbach"),
    ("Grenzland Lichtspiele", "Grenzland Lichtspiele", "A0592", None, "Neustadt in Sachsen"),
    ("Kulturfabrik Meda", "Kulturfabrik Meda", "A2710", Some("/kino/mittelherwigsdorf/kulturfabrik-meda-87268"), "Mittelherwigsdorf"),
    ("Kunst-Bauer-Kino", "Kunst-Bauer-Kino", "A2122", None, "Großhennersdorf")
  ))
  private def p_eberswalde_finow: R = ("eberswalde-finow", "eberswalde-finow-brandenburg", "Eberswalde-Finow", "Brandenburg", 52.8349, 13.8195, false, Seq("Eberswalde-Finow"), Seq(
    ("Movie Magic", "Movie Magic", "A2891", None, "Eberswalde-Finow")
  ))
  private def p_eckental: R = ("eckental", "eckental-bayern", "Eckental", "Bayern", 49.5841, 11.2175, false, Seq("Eckental"), Seq(
    ("Casino Lichtspiele Eckental", "Casino Lichtspiele Eckental", "A1343", Some("/kino/eckental/casino-lichtspiele-32728"), "Eckental")
  ))
  private def p_eggenfelden: R = ("eggenfelden", "eggenfelden-bayern", "Eggenfelden", "Bayern", 48.4051, 12.7575, false, Seq("Eggenfelden"), Seq(
    ("Gerniale Kino Open-Air", "Gerniale Kino Open-Air", "A2200", Some("/kino/eggenfelden/gerniale-kino-openair-73783"), "Eggenfelden"),
    ("Kinocenter Eggenfelden", "Kinocenter Eggenfelden", "A1312", None, "Eggenfelden")
  ))
  private def p_einbeck: R = ("einbeck", "einbeck-niedersachsen", "Einbeck", "Niedersachsen", 51.8202, 9.8696, false, Seq("Einbeck"), Seq(
    ("Deli Kino", "Deli Kino", "A1388", Some("/kino/einbeck/delikino-32405"), "Einbeck"),
    ("Welt -Theater", "Welt -Theater", "A1271", Some("/kino/einbeck/welttheater-32406"), "Einbeck")
  ))
  private def p_eisenach: R = ("eisenach", "eisenach-thueringen", "Eisenach", "Thüringen", 50.9807, 10.3152, true, Seq("Eisenach", "Mühlhausen", "Eschwege", "Bad Salzungen"), Seq(
    ("Capitol Eisenach", "Capitol Eisenach", "A0161", Some("/kino/eisenach-bei-trier/capitol-eisenach-31511"), "Eisenach"),
    ("Cinemagic Eschwege", "Cinemagic Eschwege", "A0260", Some("/kino/eschwege/cinemagic-eschwege-32020"), "Eschwege"),
    ("Filmpalast Central Mühlhausen", "Filmpalast Central Mühlhausen", "A0198", Some("/kino/muehlhausen/filmpalast-31612"), "Mühlhausen"),
    ("PAB Kinocenter", "PAB Kinocenter", "A0001", Some("/kino/bad-salzungen/%22pab%22-kinocenter-37280"), "Bad Salzungen")
  ))
  private def p_elmshorn: R = ("elmshorn", "elmshorn-schleswig-holstein", "Elmshorn", "Schleswig-Holstein", 53.7491, 9.6618, true, Seq("Elmshorn", "Wedel", "Itzehoe", "Uetersen"), Seq(
    ("Burg-Theater Uetersen", "Burg-Theater Uetersen", "A1322", Some("/kino/uetersen/burgtheater-32625"), "Uetersen"),
    ("CineMotion Itzehoe", "CineMotion Itzehoe", "A0125", Some("/kino/itzehoe/cinemotion-54256"), "Itzehoe"),
    ("Cineplex Elmshorn", "Cineplex Elmshorn", "A0317", Some("/kino/elmshorn/cineplex-elmshorn-42557"), "Elmshorn"),
    ("Open Air auf dem Theaterschiff", "Open Air auf dem Theaterschiff", "A2723", None, "Wedel")
  ))
  private def p_emden: R = ("emden", "emden-niedersachsen", "Emden", "Niedersachsen", 53.3659, 7.2085, false, Seq("Emden"), Seq(
    ("CineStar Emden", "CineStar Emden", "A0349", Some("/kino/emden-ostfriesland/cinestar-48578"), "Emden"),
    ("Sommernachtskino im Van-Ameren Bad", "Sommernachtskino im Van-Ameren Bad", "A1859", Some("/kino/emden-ostfriesland/sommernachtskino-im-vanameren-bad-67897"), "Emden")
  ))
  private def p_emmendingen: R = ("emmendingen", "emmendingen-baden-wuerttemberg", "Emmendingen", "Baden-Württemberg", 48.121, 7.8536, true, Seq("Emmendingen", "Waldkirch", "Breisach am Rhein", "Schallstadt"), Seq(
    ("Engel-Lichtspiele", "Engel-Lichtspiele", "A1403", None, "Breisach am Rhein"),
    ("Kinomobil Stuttgart - Johann-Philipp-Glock-Schule", "Kinomobil Stuttgart - Johann-Philipp-Glock-Schule", "A2100", None, "Schallstadt"),
    ("Kommunales Kino Klappe 11", "Kommunales Kino Klappe 11", "A2805", None, "Waldkirch"),
    ("Maja", "Maja", "A2808", None, "Emmendingen")
  ))
  private def p_enkenbach_alsenborn: R = ("enkenbach-alsenborn", "enkenbach-alsenborn-rheinland-pfalz", "Enkenbach Alsenborn", "Rheinland-Pfalz", 49.4833, 7.9, false, Seq("Enkenbach Alsenborn"), Seq(
    ("Provinzkino", "Provinzkino", "A1588", None, "Enkenbach Alsenborn")
  ))
  private def p_erbach_im_odenwald: R = ("erbach-im-odenwald", "erbach-im-odenwald-hessen", "Erbach im Odenwald", "Hessen", 49.6615, 8.994, true, Seq("Erbach im Odenwald", "Höchst im Odenwald", "Erlenbach am Main"), Seq(
    ("Erbacher Lichtspiele", "Erbacher Lichtspiele", "A0690", None, "Erbach im Odenwald"),
    ("Höchster Lichtspiele", "Höchster Lichtspiele", "A0240", None, "Höchst im Odenwald"),
    ("Passage Erlenbach am Main", "Passage Erlenbach am Main", "A1032", Some("/kino/erlenbach-am-main/kino-passage-32019"), "Erlenbach am Main")
  ))
  private def p_erding: R = ("erding", "erding-bayern", "Erding", "Bayern", 48.306, 11.9069, true, Seq("Erding", "Neufahrn", "Moosburg"), Seq(
    ("Cineplex Erding", "Cineplex Erding", "A0897", Some("/kino/erding/cineplex-erding-41159"), "Erding"),
    ("Cineplex Neufahrn", "Cineplex Neufahrn", "A2112", Some("/kino/neufahrn/cineplex-neufahrn-72758"), "Neufahrn"),
    ("Rosenhof-Lichtspiele", "Rosenhof-Lichtspiele", "A1606", Some("/kino/moosburg/rosenhoflichtspiele-32852"), "Moosburg")
  ))
  private def p_erdmannhausen: R = ("erdmannhausen", "erdmannhausen-baden-wuerttemberg", "Erdmannhausen", "Baden-Württemberg", 48.9426, 9.2962, true, Seq("Erdmannhausen", "Marbach am Neckar", "Grossbottwar"), Seq(
    ("Kinomobil Stuttgart - Astrid-Lindgren-Schule", "Kinomobil Stuttgart - Astrid-Lindgren-Schule", "A0813", None, "Erdmannhausen"),
    ("Kinomobil Stuttgart - Schulhof Astrid-Lindgren-Schule", "Kinomobil Stuttgart - Schulhof Astrid-Lindgren-Schule", "A1952", None, "Erdmannhausen"),
    ("Kinomobil Stuttgart - Jugend-Kultur-Haus planet x", "Kinomobil Stuttgart - Jugend-Kultur-Haus planet x", "A0784", None, "Marbach am Neckar"),
    ("Kinomobil Stuttgart - Kelter Winzerhausen", "Kinomobil Stuttgart - Kelter Winzerhausen", "A2132", None, "Grossbottwar")
  ))
  private def p_eschborn: R = ("eschborn", "eschborn-hessen", "Eschborn", "Hessen", 50.1433, 8.5711, true, Seq("Eschborn", "Kronberg im Taunus"), Seq(
    ("Eschborn K", "Eschborn K", "A2898", Some("/kino/eschborn/eschborn-k-75177"), "Eschborn"),
    ("Kronberger Lichtspiele", "Kronberger Lichtspiele", "A0872", Some("/kino/kronberg-im-taunus/kronberger-lichtspiele-81117"), "Kronberg im Taunus")
  ))
  private def p_espelkamp: R = ("espelkamp", "espelkamp-nordrhein-westfalen", "Espelkamp", "Nordrhein-Westfalen", 52.3814, 8.623, true, Seq("Espelkamp", "Rahden", "Lemförde", "Quernheim"), Seq(
    ("Elite", "Elite", "A0462", Some("/kino/espelkamp/elite-31797"), "Espelkamp"),
    ("Kinocenter Rahden", "Kinocenter Rahden", "A1730", None, "Rahden"),
    ("Lichtburg Lemförde", "Lichtburg Lemförde", "A0689", Some("/kino/quernheim-bei-diepholz/lichtburg-34805"), "Quernheim"),
    ("Lichtburg Open Air Kino", "Lichtburg Open Air Kino", "A1865", None, "Lemförde")
  ))
  private def p_esslingen_am_neckar: R = ("esslingen-am-neckar", "esslingen-am-neckar-baden-wuerttemberg", "Esslingen am Neckar", "Baden-Württemberg", 48.7396, 9.3047, false, Seq("Esslingen am Neckar"), Seq(
    ("Autokino Esslingen Neckar Center", "Autokino Esslingen Neckar Center", "A2928", Some("/kino/esslingen-am-neckar/autokino-im-neckar-center-92153"), "Esslingen am Neckar"),
    ("Kommunales Kino Esslingen", "Kommunales Kino Esslingen", "A1520", Some("/kino/esslingen-am-neckar/kommunales-kino-32745"), "Esslingen am Neckar"),
    ("Open-Air-Kino auf der Burg", "Open-Air-Kino auf der Burg", "A1847", Some("/kino/esslingen-am-neckar/openairkino-auf-der-burg-36365"), "Esslingen am Neckar"),
    ("Traumpalast Esslingen", "Traumpalast Esslingen", "A0076", Some("/kino/esslingen-am-neckar/traumpalast-66136"), "Esslingen am Neckar")
  ))
  private def p_fehmarn: R = ("fehmarn", "fehmarn-schleswig-holstein", "Fehmarn", "Schleswig-Holstein", 54.4378, 11.1935, false, Seq("Fehmarn"), Seq(
    ("Burg-Film-Theater", "Burg-Film-Theater", "A2729", None, "Fehmarn")
  ))
  private def p_fellbach: R = ("fellbach", "fellbach-baden-wuerttemberg", "Fellbach", "Baden-Württemberg", 48.8091, 9.277, true, Seq("Fellbach", "Kornwestheim", "Waiblingen", "Weinstadt", "Korb"), Seq(
    ("Capitol Lichtspiele Kornwestheim", "Capitol Lichtspiele Kornwestheim", "A0145", Some("/kino/kornwestheim/capitol-lichtspiele-33126"), "Kornwestheim"),
    ("Drive In Autokino Kornwestheim", "Drive In Autokino Kornwestheim", "A1921", Some("/kino/kornwestheim/drive-in-autokino-32814"), "Kornwestheim"),
    ("Fellbacher Sommernachts-Open-Air-Kino", "Fellbacher Sommernachts-Open-Air-Kino", "A1925", None, "Fellbach"),
    ("Kinokult Orfeo-Kino", "Kinokult Orfeo-Kino", "A1021", Some("/kino/fellbach/orfeo-36550"), "Fellbach"),
    ("Kinomobil Stuttgart - Alte Kelter Korb", "Kinomobil Stuttgart - Alte Kelter Korb", "A0796", Some("/kino/korb/kinomobil-stuttgart-alte-kelter-40623"), "Korb"),
    ("Kommunales Kino Weinstadt", "Kommunales Kino Weinstadt", "A2856", None, "Weinstadt"),
    ("Traumpalast Waiblingen", "Traumpalast Waiblingen", "A0480", Some("/kino/waiblingen/traumpalast-66138"), "Waiblingen")
  ))
  private def p_flensburg: R = ("flensburg", "flensburg-schleswig-holstein", "Flensburg", "Schleswig-Holstein", 54.788, 9.4372, false, Seq("Flensburg"), Seq(
    ("Kino 51 Stufen", "Kino 51 Stufen", "A1481", Some("/kino/flensburg/kino-51-stufen-32417"), "Flensburg"),
    ("UCI Flensburg", "UCI Flensburg", "A0829", Some("/kino/flensburg/uci-kinowelt-flensburg-74576"), "Flensburg")
  ))
  private def p_forchheim: R = ("forchheim", "forchheim-bayern", "Forchheim", "Bayern", 49.7175, 11.0588, false, Seq("Forchheim"), Seq(
    ("Kino-Center Forchheim", "Kino-Center Forchheim", "A0034", Some("/kino/forchheim/kinocenter-54076"), "Forchheim"),
    ("Open Air Kino In der Kaiserpfalz Forchheim", "Open Air Kino In der Kaiserpfalz Forchheim", "A1968", None, "Forchheim")
  ))
  private def p_frankenthal: R = ("frankenthal", "frankenthal-rheinland-pfalz", "Frankenthal", "Rheinland-Pfalz", 49.5341, 8.3536, false, Seq("Frankenthal"), Seq(
    ("Lux im Dathenushaus", "Lux im Dathenushaus", "A0930", None, "Frankenthal"),
    ("OPEN AIR KINO in der Erkenbert Ruine", "OPEN AIR KINO in der Erkenbert Ruine", "A1990", Some("/kino/frankenthal-pfalz/open-air-kino-in-der-erkenbert-ruine-71540"), "Frankenthal")
  ))
  private def p_frankfurt_an_der_oder: R = ("frankfurt-an-der-oder", "frankfurt-an-der-oder-brandenburg", "Frankfurt an der Oder", "Brandenburg", 52.3471, 14.5506, false, Seq("Frankfurt an der Oder"), Seq(
    ("CineStar Frankfurt (Oder)", "CineStar Frankfurt (Oder)", "A0343", Some("/kino/frankfurt-oder/cinestar-36056"), "Frankfurt an der Oder"),
    ("Kleines Kino", "Kleines Kino", "A2371", None, "Frankfurt an der Oder")
  ))
  private def p_freiberg: R = ("freiberg", "freiberg-sachsen", "Freiberg", "Sachsen", 50.9109, 13.3388, false, Seq("Freiberg"), Seq(
    ("Kinopolis Freiberg", "Kinopolis Freiberg", "A0825", Some("/kino/freiberg/kinopolis-35167"), "Freiberg"),
    ("Open Air auf Schloss Freudenstein", "Open Air auf Schloss Freudenstein", "A2567", None, "Freiberg")
  ))
  private def p_freudenstadt: R = ("freudenstadt", "freudenstadt-baden-wuerttemberg", "Freudenstadt", "Baden-Württemberg", 48.4669, 8.4137, false, Seq("Freudenstadt"), Seq(
    ("Central Freudenstadt", "Central Freudenstadt", "A0186", Some("/kino/freudenstadt/central-70397"), "Freudenstadt"),
    ("Subiaco im Kurhaus", "Subiaco im Kurhaus", "A1170", Some("/kino/freudenstadt/subiaco-im-kurhaus-58436"), "Freudenstadt")
  ))
  private def p_freyung: R = ("freyung", "freyung-bayern", "Freyung", "Bayern", 48.8095, 13.5477, false, Seq("Freyung"), Seq(
    ("Cineplex Freyung", "Cineplex Freyung", "A2642", Some("/kino/freyung/cineplex-82999"), "Freyung")
  ))
  private def p_friedrichsdorf: R = ("friedrichsdorf", "friedrichsdorf-hessen", "Friedrichsdorf", "Hessen", 50.2496, 8.6428, true, Seq("Friedrichsdorf", "Bad Homburg vor der Höhe"), Seq(
    ("Filmtheater Friedrichsdorf", "Filmtheater Friedrichsdorf", "A1427", Some("/kino/friedrichsdorf/filmtheater-32051"), "Friedrichsdorf"),
    ("Open Air im Freibad Friedrichsdorf", "Open Air im Freibad Friedrichsdorf", "A2281", Some("/kino/friedrichsdorf/open-air-im-freibad-friedrichsdorf-55257"), "Friedrichsdorf"),
    ("Kinopolis Bad Homburg", "Kinopolis Bad Homburg", "cineamo:1811", None, "Bad Homburg vor der Höhe")
  ))
  private def p_friedrichshafen: R = ("friedrichshafen", "friedrichshafen-baden-wuerttemberg", "Friedrichshafen", "Baden-Württemberg", 47.6569, 9.4755, true, Seq("Friedrichshafen", "Tettnang", "Markdorf", "Immenstaad am Bodensee"), Seq(
    ("Cineplex Friedrichshafen", "Cineplex Friedrichshafen", "A0313", Some("/kino/friedrichshafen/cineplex-friedrichshafen-65397"), "Friedrichshafen"),
    ("Kulturhaus Caserne Kino Studio 17", "Kulturhaus Caserne Kino Studio 17", "A0754", Some("/kino/friedrichshafen/kulturhaus-caserne-kino-66317"), "Friedrichshafen"),
    ("Kinomobil Stuttgart - Strandbad Aquastaad", "Kinomobil Stuttgart - Strandbad Aquastaad", "A1951", None, "Immenstaad am Bodensee"),
    ("KiTT - Kino und Kleinkunst Tettnang e.V.", "KiTT - Kino und Kleinkunst Tettnang e.V.", "A1518", None, "Tettnang"),
    ("Theaterstadel", "Theaterstadel", "A1178", Some("/kino/markdorf/theaterstadel-32177"), "Markdorf")
  ))
  private def p_fuerstenfeldbruck: R = ("fuerstenfeldbruck", "fuerstenfeldbruck-bayern", "Fürstenfeldbruck", "Bayern", 48.179, 11.2547, false, Seq("Fürstenfeldbruck"), Seq(
    ("Kino-Open-Air", "Kino-Open-Air", "A2210", Some("/kino/fuerstenfeldbruck/open-air-kino-33568"), "Fürstenfeldbruck"),
    ("Lichtspielhaus Fürstenfeldbruck", "Lichtspielhaus Fürstenfeldbruck", "A1539", Some("/kino/fuerstenfeldbruck/lichtspielhaus-32760"), "Fürstenfeldbruck"),
    ("Scala Kino Fürstenfeldbruck", "Scala Kino Fürstenfeldbruck", "A1110", Some("/kino/fuerstenfeldbruck/scala-64657"), "Fürstenfeldbruck")
  ))
  private def p_fuerstenwalde_an_der_spree: R = ("fuerstenwalde-an-der-spree", "fuerstenwalde-an-der-spree-brandenburg", "Fürstenwalde an der Spree", "Brandenburg", 52.3607, 14.0618, true, Seq("Fürstenwalde an der Spree", "Bad Saarow", "Beeskow", "Buckow"), Seq(
    ("Cinema by Velotel", "Cinema by Velotel", "G0119", None, "Bad Saarow"),
    ("Cinema Bad Saarow", "Cinema Bad Saarow", "cineamo:1404", None, "Bad Saarow"),
    ("Filmtheater Union", "Filmtheater Union", "A2383", Some("/kino/fuerstenwalde-spree/filmtheater-union-31527"), "Fürstenwalde an der Spree"),
    ("Open Air Kino Parkbühne", "Open Air Kino Parkbühne", "A2449", None, "Fürstenwalde an der Spree"),
    ("Park-Theater Buckow", "Park-Theater Buckow", "A1491", None, "Buckow"),
    ("Schukurama", "Schukurama", "A2366", Some("/kino/beeskow/schukurama-75023"), "Beeskow")
  ))
  private def p_fuessen: R = ("fuessen", "fuessen-bayern", "Füssen", "Bayern", 47.5714, 10.7017, false, Seq("Füssen"), Seq(
    ("Alpenfilmtheater Füssen", "Alpenfilmtheater Füssen", "A1472", Some("/kino/fuessen/alpenfilmtheater-32763"), "Füssen")
  ))
  private def p_fulda: R = ("fulda", "fulda-hessen", "Fulda", "Hessen", 50.5516, 9.6752, false, Seq("Fulda"), Seq(
    ("CineStar Fulda", "CineStar Fulda", "A0366", Some("/kino/fulda/cinestar-fulda-38822"), "Fulda"),
    ("Kinoinitiative 35", "Kinoinitiative 35", "A2661", Some("/kino/fulda/kinoinitiative-35-79917"), "Fulda")
  ))
  private def p_furtwangen_im_schwarzwald: R = ("furtwangen-im-schwarzwald", "furtwangen-im-schwarzwald-baden-wuerttemberg", "Furtwangen im Schwarzwald", "Baden-Württemberg", 48.0516, 8.2072, true, Seq("Furtwangen im Schwarzwald", "Triberg im Schwarzwald"), Seq(
    ("Kommunales Guckloch-Kino Furtwangen e.V.", "Kommunales Guckloch-Kino Furtwangen e.V.", "A0855", Some("/kino/furtwangen-im-schwarzwald/kommunales-gucklochkino-furtwangen-e.v.-69579"), "Furtwangen im Schwarzwald"),
    ("Open Air Kino Furtwangen", "Open Air Kino Furtwangen", "A1774", Some("/kino/furtwangen-im-schwarzwald/open-air-kino-furtwangen-70859"), "Furtwangen im Schwarzwald"),
    ("Kronen-Lichtspiele Triberg im Schwarzwald", "Kronen-Lichtspiele Triberg im Schwarzwald", "A1526", None, "Triberg im Schwarzwald")
  ))
  private def p_garbsen: R = ("garbsen", "garbsen-niedersachsen", "Garbsen", "Niedersachsen", 52.4196, 9.6033, true, Seq("Garbsen", "Langenhagen", "Neustadt am Rübenberge", "Bad Nenndorf"), Seq(
    ("Cinema im Leinepark", "Cinema im Leinepark", "A2873", Some("/kino/neustadt-am-ruebenberge/cinema-im-leinepark-93073"), "Neustadt am Rübenberge"),
    ("CineMotion Langenhagen", "CineMotion Langenhagen", "A1251", Some("/kino/langenhagen-hannover/cinemotion-37927"), "Langenhagen"),
    ("Phoenix Kurlichtspiele Bad Nenndorf", "Phoenix Kurlichtspiele Bad Nenndorf", "A0707", Some("/kino/bad-nenndorf/phoenix-kurlichtspiele-56637"), "Bad Nenndorf"),
    ("CineStar Garbsen", "CineStar Garbsen", "A2793", None, "Garbsen")
  ))
  private def p_garmisch_partenkirchen: R = ("garmisch-partenkirchen", "garmisch-partenkirchen-bayern", "Garmisch-Partenkirchen", "Bayern", 47.4921, 11.0958, false, Seq("Garmisch-Partenkirchen"), Seq(
    ("Hochland-Kino", "Hochland-Kino", "A0609", Some("/kino/garmischpartenkirchen/hochlandkino-32766"), "Garmisch-Partenkirchen"),
    ("Kinocenter Garmisch &amp; Aspen im Lamm", "Kinocenter Garmisch &amp; Aspen im Lamm", "A1508", Some("/kino/garmischpartenkirchen/kinocenter-garmisch-und-aspen-im-lamm-32767"), "Garmisch-Partenkirchen")
  ))
  private def p_genthin: R = ("genthin", "genthin-sachsen-anhalt", "Genthin", "Sachsen-Anhalt", 52.4067, 12.1592, false, Seq("Genthin"), Seq(
    ("Union Kino Genthin", "Union Kino Genthin", "A1239", Some("/kino/genthin/union-kino-31530"), "Genthin")
  ))
  private def p_germering: R = ("germering", "germering-bayern", "Germering", "Bayern", 48.1339, 11.3765, true, Seq("Germering", "Dachau", "Starnberg", "Gauting", "Gröbenzell", "Gilching", "Gräfelfing", "Seefeld", "Weßling", "Inning-Stegen"), Seq(
    ("Cinema Dachau", "Cinema Dachau", "A1357", Some("/kino/dachau/cinema-dachau-32719"), "Dachau"),
    ("Cineplex Germering", "Cineplex Germering", "A2705", Some("/kino/germering/cineplex-87221"), "Germering"),
    ("Filmeck im Bürgerhaus", "Filmeck im Bürgerhaus", "A1415", Some("/kino/graefelfing/filmeck-im-buergerhaus-32777"), "Gräfelfing"),
    ("Filmstation", "Filmstation", "A0526", Some("/kino/gilching/filmstation-37733"), "Gilching"),
    ("Gröben-Lichtspiele", "Gröben-Lichtspiele", "A1445", Some("/kino/groebenzell/groebenlichtspiele-32780"), "Gröbenzell"),
    ("Kino Breitwand Gauting", "Kino Breitwand Gauting", "A1412", Some("/kino/gauting/kino-breitwand-gauting-88757"), "Gauting"),
    ("Kino Breitwand im Schloß Seefeld", "Kino Breitwand im Schloß Seefeld", "A0713", Some("/kino/seefeld/kino-breitwand-im-schloss-seefeld-37701"), "Seefeld"),
    ("Kino Breitwand Starnberg", "Kino Breitwand Starnberg", "A0712", Some("/kino/starnberg/kino-breitwand-50617"), "Starnberg"),
    ("Kino in der Alten Brauerei", "Kino in der Alten Brauerei", "A0735", None, "Inning-Stegen"),
    ("Kino Pfarrstadel", "Kino Pfarrstadel", "A2659", Some("/kino/wessling/kino-pfarrstadel-84537"), "Weßling")
  ))
  private def p_giessen: R = ("giessen", "giessen-hessen", "Gießen", "Hessen", 50.5873, 8.6755, false, Seq("Gießen"), Seq(
    ("Kinocenter Giessen", "Kinocenter Giessen", "A0772", None, "Gießen"),
    ("Kinopolis Giessen", "Kinopolis Giessen", "A2662", None, "Gießen")
  ))
  private def p_gifhorn: R = ("gifhorn", "gifhorn-niedersachsen", "Gifhorn", "Niedersachsen", 52.4777, 10.5511, false, Seq("Gifhorn"), Seq(
    ("Kinocenter am Steinweg", "Kinocenter am Steinweg", "A0574", Some("/kino/gifhorn/kinocenter-am-steinweg-71836"), "Gifhorn")
  ))
  private def p_goeppingen: R = ("goeppingen", "goeppingen-baden-wuerttemberg", "Göppingen", "Baden-Württemberg", 48.7035, 9.6521, true, Seq("Göppingen", "Geislingen an der Steige", "Plochingen", "Donzdorf", "Deggingen", "Schlat"), Seq(
    ("Gloria Kino Center Geislingen an der Steige", "Gloria Kino Center Geislingen an der Steige", "A0582", Some("/kino/geislingen-an-der-steige/gloria-kino-center-32769"), "Geislingen an der Steige"),
    ("Kinomobil Stuttgart - Bürgerhaus", "Kinomobil Stuttgart - Bürgerhaus", "A0808", None, "Schlat"),
    ("Kinomobil Stuttgart - Feuerwehrhaus Deggingen", "Kinomobil Stuttgart - Feuerwehrhaus Deggingen", "A2128", None, "Deggingen"),
    ("Sommerkino Donzdorf", "Sommerkino Donzdorf", "A2544", None, "Donzdorf"),
    ("Staufen-Movieplex", "Staufen-Movieplex", "A1151", None, "Göppingen"),
    ("Union-Theater Plochingen", "Union-Theater Plochingen", "A1654", Some("/kino/plochingen/uniontheater-32941"), "Plochingen")
  ))
  private def p_goerlitz: R = ("goerlitz", "goerlitz-sachsen", "Görlitz", "Sachsen", 51.1552, 14.9885, false, Seq("Görlitz"), Seq(
    ("Camillo-Sommerkino im Rathaushof", "Camillo-Sommerkino im Rathaushof", "A2090", Some("/kino/goerlitz-neisse/camillosommerkino-im-rathaushof-56457"), "Görlitz"),
    ("CamilloKino", "CamilloKino", "A0138", Some("/kino/goerlitz-neisse/camillokino-53777"), "Görlitz"),
    ("Filmpalast Görlitz", "Filmpalast Görlitz", "A0510", Some("/kino/goerlitz-neisse/filmpalast-31536"), "Görlitz"),
    ("Offkino Klappe die Zweite", "Offkino Klappe die Zweite", "A2142", None, "Görlitz")
  ))
  private def p_goslar: R = ("goslar", "goslar-niedersachsen", "Goslar", "Niedersachsen", 51.9042, 10.4277, false, Seq("Goslar"), Seq(
    ("Cineplex Goslar", "Cineplex Goslar", "A0318", Some("/kino/goslar/cineplex-goslar-50417"), "Goslar")
  ))
  private def p_gotha: R = ("gotha", "gotha-thueringen", "Gotha", "Thüringen", 50.9482, 10.7019, true, Seq("Gotha", "Bad Langensalza"), Seq(
    ("Burgtheater Bad Langensalza", "Burgtheater Bad Langensalza", "A0121", Some("/kino/bad-langensalza/burgtheater-31347"), "Bad Langensalza"),
    ("Cineplex Gotha", "Cineplex Gotha", "A2675", Some("/kino/gotha-thueringen/cineplex-85777"), "Gotha")
  ))
  private def p_grafing_bei_muenchen: R = ("grafing-bei-muenchen", "grafing-bei-muenchen-bayern", "Grafing bei München", "Bayern", 48.046, 11.968, true, Seq("Grafing bei München", "Ebersberg"), Seq(
    ("Capitol Theater Grafing bei München", "Capitol Theater Grafing bei München", "A0159", None, "Grafing bei München"),
    ("Kino im Alten Kino", "Kino im Alten Kino", "A2850", None, "Ebersberg")
  ))
  private def p_greifswald: R = ("greifswald", "greifswald-mecklenburg-vorpommern", "Greifswald", "Mecklenburg-Vorpommern", 54.0891, 13.4024, false, Seq("Greifswald"), Seq(
    ("CineStar Greifswald", "CineStar Greifswald", "A0362", Some("/kino/greifswald-hansestadt/cinestar-der-filmpalast-31541"), "Greifswald")
  ))
  private def p_grevenbroich: R = ("grevenbroich", "grevenbroich-nordrhein-westfalen", "Grevenbroich", "Nordrhein-Westfalen", 51.091, 6.5827, true, Seq("Grevenbroich", "Erkelenz"), Seq(
    ("Gloria Filmpalast Erkelenz", "Gloria Filmpalast Erkelenz", "A0581", Some("/kino/erkelenz/gloria-filmpalast-31794"), "Erkelenz"),
    ("Grefi Kino Grevenbroich", "Grefi Kino Grevenbroich", "A0591", Some("/kino/grevenbroich/grefikinocenter-33182"), "Grevenbroich")
  ))
  private def p_grimma: R = ("grimma", "grimma-sachsen", "Grimma", "Sachsen", 51.2337, 12.7196, true, Seq("Grimma", "Döbeln", "Mittweida"), Seq(
    ("Central Theater Grimma", "Central Theater Grimma", "A0194", Some("/kino/grimma/central-theater-31545"), "Grimma"),
    ("Cinema Döbeln", "Cinema Döbeln", "A0215", Some("/kino/doebeln/cinema-37643"), "Döbeln"),
    ("Filmbühne Mittweida", "Filmbühne Mittweida", "A0487", Some("/kino/mittweida/filmbuehne-31611"), "Mittweida")
  ))
  private def p_gronau_westf: R = ("gronau-westf", "gronau-westf-nordrhein-westfalen", "Gronau (Westf.)", "Nordrhein-Westfalen", 52.211, 7.0224, false, Seq("Gronau (Westf.)"), Seq(
    ("Cinetech das Erlebniskino Gronau", "Cinetech das Erlebniskino Gronau", "A0382", None, "Gronau (Westf.)")
  ))
  private def p_gruenberg: R = ("gruenberg", "gruenberg-hessen", "Grünberg", "Hessen", 50.594, 8.9587, false, Seq("Grünberg"), Seq(
    ("Lichtspiele Grünberg", "Lichtspiele Grünberg", "A0044", Some("/kino/gruenberg/lichtspiele-gruenberg-57696"), "Grünberg"),
    ("Open Air Kino auf dem Marktplatz", "Open Air Kino auf dem Marktplatz", "A2565", None, "Grünberg")
  ))
  private def p_gruenstadt: R = ("gruenstadt", "gruenstadt-rheinland-pfalz", "Grünstadt", "Rheinland-Pfalz", 49.563, 8.1628, false, Seq("Grünstadt"), Seq(
    ("Europa-Theater Grünstadt", "Europa-Theater Grünstadt", "A1404", Some("/kino/gruenstadt/europa-kinos-32070"), "Grünstadt"),
    ("Filmwelt Grünstadt", "Filmwelt Grünstadt", "A2700", Some("/kino/gruenstadt/filmwelt-gruenstadt-86677"), "Grünstadt")
  ))
  private def p_guestrow: R = ("guestrow", "guestrow-mecklenburg-vorpommern", "Güstrow", "Mecklenburg-Vorpommern", 53.7972, 12.1734, false, Seq("Güstrow"), Seq(
    ("Movie Star Gustrow", "Movie Star Gustrow", "A0972", None, "Güstrow"),
    ("Sommerkino Güstrow", "Sommerkino Güstrow", "A2933", None, "Güstrow")
  ))
  private def p_guetersloh: R = ("guetersloh", "guetersloh-nordrhein-westfalen", "Gütersloh", "Nordrhein-Westfalen", 51.9069, 8.3785, false, Seq("Gütersloh"), Seq(
    ("Bambi &amp; Löwenherz", "Bambi &amp; Löwenherz", "A0081", Some("/kino/guetersloh/bambi-und-loewenherz-76656"), "Gütersloh"),
    ("Filmwerk Gutersloh", "Filmwerk Gutersloh", "G01IT", None, "Gütersloh"),
    ("Open Air Kino auf dem Dreiecksplatz", "Open Air Kino auf dem Dreiecksplatz", "A2434", Some("/kino/guetersloh/open-air-kino-auf-dem-dreiecksplatz-75548"), "Gütersloh")
  ))
  private def p_gummersbach: R = ("gummersbach", "gummersbach-nordrhein-westfalen", "Gummersbach", "Nordrhein-Westfalen", 51.0261, 7.5647, true, Seq("Gummersbach", "Olpe", "Hilchenbach"), Seq(
    ("Cineplex Olpe", "Cineplex Olpe", "A2115", Some("/kino/olpe/cineplex-72377"), "Olpe"),
    ("SEVEN Kinocenter Gummersbach", "SEVEN Kinocenter Gummersbach", "A2912", Some("/kino/gummersbach/seven-kinocenter-91926"), "Gummersbach"),
    ("Viktoria Filmtheater", "Viktoria Filmtheater", "A1256", Some("/kino/hilchenbach/viktoria-filmtheater-31821"), "Hilchenbach")
  ))
  private def p_halberstadt: R = ("halberstadt", "halberstadt-sachsen-anhalt", "Halberstadt", "Sachsen-Anhalt", 51.8956, 11.0562, true, Seq("Halberstadt", "Wernigerode", "Quedlinburg", "Thale"), Seq(
    ("Central Theater Thale", "Central Theater Thale", "A0208", Some("/kino/thale/central-theater-31687"), "Thale"),
    ("Studiokino Eisenstein im Kulturzentrum", "Studiokino Eisenstein im Kulturzentrum", "A1167", Some("/kino/quedlinburg/studiokino-eisenstein-im-kulturzentrum-65577"), "Quedlinburg"),
    ("Volkslichtspiele Wernigerode", "Volkslichtspiele Wernigerode", "A1265", None, "Wernigerode"),
    ("Zuckerfabrik Kinopark", "Zuckerfabrik Kinopark", "A1013", Some("/kino/halberstadt/zuckerfabrik-kinopark-35338"), "Halberstadt")
  ))
  private def p_hameln: R = ("hameln", "hameln-niedersachsen", "Hameln", "Niedersachsen", 52.104, 9.3562, false, Seq("Hameln"), Seq(
    ("Maxx Hameln", "Maxx Hameln", "A1715", Some("/kino/hameln/maxx-hameln-55117"), "Hameln"),
    ("Sumpfblume", "Sumpfblume", "A2825", Some("/kino/hameln/sumpfblume-32466"), "Hameln")
  ))
  private def p_hanau: R = ("hanau", "hanau-hessen", "Hanau", "Hessen", 50.1342, 8.9142, true, Seq("Hanau", "Nidderau", "Alzenau"), Seq(
    ("Kinopolis Hanau", "Kinopolis Hanau", "A2613", Some("/kino/hanau/kinopolis-80297"), "Hanau"),
    ("Luxor Filmpalast Nidderau", "Luxor Filmpalast Nidderau", "A0934", Some("/kino/nidderau/luxor-filmpalast-65497"), "Nidderau"),
    ("Kino Alzenau", "Kino Alzenau", "cineamo:1239", None, "Alzenau")
  ))
  private def p_hechingen: R = ("hechingen", "hechingen-baden-wuerttemberg", "Hechingen", "Baden-Württemberg", 48.3515, 8.9632, false, Seq("Hechingen"), Seq(
    ("Burgtheater-Kinos", "Burgtheater-Kinos", "A2787", Some("/kino/hechingen/burgtheater-32786"), "Hechingen"),
    ("Open Air Kino Burg Hohenzollern", "Open Air Kino Burg Hohenzollern", "A2053", None, "Hechingen"),
    ("Schwanen-Kinos", "Schwanen-Kinos", "A2819", Some("/kino/hechingen/schwanenkino-32787"), "Hechingen")
  ))
  private def p_heide: R = ("heide", "heide-schleswig-holstein", "Heide", "Schleswig-Holstein", 54.1956, 9.0974, true, Seq("Heide", "Meldorf", "Büsum"), Seq(
    ("Deutsches Haus-Lichtspiele", "Deutsches Haus-Lichtspiele", "A1390", Some("/kino/meldorf-holstein/deutsches-hauslichtspiele-32543"), "Meldorf"),
    ("Lichtblick Büsum", "Lichtblick Büsum", "A2909", Some("/kino/buesum/lichtblick-filmtheater-91985"), "Büsum"),
    ("LichtBlick Heide", "LichtBlick Heide", "A0265", Some("/kino/heide-holstein/lichtblick-32484"), "Heide")
  ))
  private def p_heidenheim: R = ("heidenheim", "heidenheim-baden-wuerttemberg", "Heidenheim", "Baden-Württemberg", 48.678, 10.1516, true, Seq("Heidenheim", "Königsbronn"), Seq(
    ("Capitol Heidenheim", "Capitol Heidenheim", "A0140", Some("/kino/heidenheim/capitol-32788"), "Heidenheim"),
    ("Kino-Center Heidenheim", "Kino-Center Heidenheim", "A0761", Some("/kino/heidenheim/kinocenter-32789"), "Heidenheim"),
    ("Kinomobil Stuttgart - Ketteler-Haus", "Kinomobil Stuttgart - Ketteler-Haus", "A0809", Some("/kino/koenigsbronn/kinomobil-stuttgart-ketteler-haus-53339"), "Königsbronn")
  ))
  private def p_heinsberg: R = ("heinsberg", "heinsberg-nordrhein-westfalen", "Heinsberg", "Nordrhein-Westfalen", 51.0636, 6.0998, true, Seq("Heinsberg", "Hückelhoven"), Seq(
    ("Corso-Filmpalast Hilfarth", "Corso-Filmpalast Hilfarth", "A0426", Some("/kino/hueckelhoven/corsofilmpalast-hilfarth-33152"), "Hückelhoven"),
    ("Roxy Filmtheater Heinsberg", "Roxy Filmtheater Heinsberg", "A0248", Some("/kino/heinsberg/roxy-31814"), "Heinsberg")
  ))
  private def p_helgoland: R = ("helgoland", "helgoland-schleswig-holstein", "Helgoland", "Schleswig-Holstein", 54.1814, 7.8863, false, Seq("Helgoland"), Seq(
    ("Hochseekino Helgoland", "Hochseekino Helgoland", "A2652", Some("/kino/helgoland/hochseekino-84577"), "Helgoland")
  ))
  private def p_helmstedt: R = ("helmstedt", "helmstedt-niedersachsen", "Helmstedt", "Niedersachsen", 52.2279, 11.0099, true, Seq("Helmstedt", "Königslutter am Elm", "Grasleben"), Seq(
    ("Autokino in Grasleben", "Autokino in Grasleben", "A2937", None, "Grasleben"),
    ("Camera am Holzberg", "Camera am Holzberg", "A1327", Some("/kino/helmstedt/camera-am-holzberg-32485"), "Helmstedt"),
    ("Roxy- Theater -Lichtspiele", "Roxy- Theater -Lichtspiele", "A1093", Some("/kino/helmstedt/roxy-theater-lichtspiele-32486"), "Helmstedt"),
    ("Kammerlichtspiele Könnigslutter am Elm", "Kammerlichtspiele Könnigslutter am Elm", "A0637", None, "Königslutter am Elm")
  ))
  private def p_herford: R = ("herford", "herford-nordrhein-westfalen", "Herford", "Nordrhein-Westfalen", 52.1146, 8.6734, true, Seq("Herford", "Bad Salzuflen", "Bad Oeynhausen", "Lemgo", "Löhne", "Oerlinghausen", "Kirchlengern", "Spenge", "Rödinghausen"), Seq(
    ("Capitol Herford", "Capitol Herford", "A0148", Some("/kino/herford/capitol-3d-kino-herford-31817"), "Herford"),
    ("Else-Lichtspiele", "Else-Lichtspiele", "A0463", Some("/kino/roedinghausen/elselichtspiele-31905"), "Rödinghausen"),
    ("Filmbühne Kinocenter", "Filmbühne Kinocenter", "A0488", Some("/kino/bad-salzuflen/filmbuehne-kinocenter-32339"), "Bad Salzuflen"),
    ("Hansa-Kino", "Hansa-Kino", "A0599", Some("/kino/lemgo/hansakino-35213"), "Lemgo"),
    ("LichtBlick Kirchlengern", "LichtBlick Kirchlengern", "A0898", Some("/kino/kirchlengern/lichtblick-65581"), "Kirchlengern"),
    ("Sommernachtskino (Löhne)", "Sommernachtskino (Löhne)", "A2824", None, "Löhne"),
    ("UCI Bad Oeynhausen", "UCI Bad Oeynhausen", "A2666", Some("/kino/bad-oeynhausen/uci-kinowelt-bad-oeynhausen-74656"), "Bad Oeynhausen"),
    ("Zentral-Theater Spenge", "Zentral-Theater Spenge", "A1288", Some("/kino/spenge/zentraltheater-31918"), "Spenge"),
    ("KNUP – Soziokulturelles Zentrum Oerlinghausen", "KNUP – Soziokulturelles Zentrum Oerlinghausen", "kinoheld:3871", None, "Oerlinghausen")
  ))
  private def p_herrenberg: R = ("herrenberg", "herrenberg-baden-wuerttemberg", "Herrenberg", "Baden-Württemberg", 48.5952, 8.8665, false, Seq("Herrenberg"), Seq(
    ("Kommunales Kino Herrenberg", "Kommunales Kino Herrenberg", "A0860", Some("/kino/herrenberg/kommunales-kino-69497"), "Herrenberg"),
    ("Open Air Kino Herrenberg", "Open Air Kino Herrenberg", "A2770", None, "Herrenberg")
  ))
  private def p_hof: R = ("hof", "hof-bayern", "Hof", "Bayern", 50.313, 11.9126, false, Seq("Hof"), Seq(
    ("Central-Kino Hof", "Central-Kino Hof", "A1346", Some("/kino/hof/centralkino-32794"), "Hof"),
    ("Scala Filmtheater Hof", "Scala Filmtheater Hof", "A1051", Some("/kino/hof/scala-filmtheater-32793"), "Hof")
  ))
  private def p_hofheim_am_taunus: R = ("hofheim-am-taunus", "hofheim-am-taunus-hessen", "Hofheim am Taunus", "Hessen", 50.0902, 8.4493, true, Seq("Hofheim am Taunus", "Kelkheim", "Bad Soden am Taunus", "Eppstein", "Sulzbach"), Seq(
    ("CasaBlanca Art House", "CasaBlanca Art House", "A1531", Some("/kino/bad-soden-am-taunus/kino-casablanca-art-house-33116"), "Bad Soden am Taunus"),
    ("Filmpalast Hofheim", "Filmpalast Hofheim", "A2641", Some("/kino/hofheim-am-taunus/filmpalast-78837"), "Hofheim am Taunus"),
    ("Open-Air Spielwiese neben dem Rathaus", "Open-Air Spielwiese neben dem Rathaus", "A2458", None, "Hofheim am Taunus"),
    ("Kino-Kelkheim", "Kino-Kelkheim", "A1510", Some("/kino/kelkheim/kinokelkheim-32123"), "Kelkheim"),
    ("Open-Air Marktplatz Neue Stadtmitte", "Open-Air Marktplatz Neue Stadtmitte", "A2204", None, "Kelkheim"),
    ("Kinopolis Main-Taunus", "Kinopolis Main-Taunus", "A0831", None, "Sulzbach"),
    ("Open-Air auf der Burg", "Open-Air auf der Burg", "A2283", None, "Eppstein")
  ))
  private def p_holzkirchen: R = ("holzkirchen", "holzkirchen-bayern", "Holzkirchen", "Bayern", 47.8766, 11.7018, true, Seq("Holzkirchen", "Hausham", "Rottach-Egern"), Seq(
    ("Kino am Tegernsee", "Kino am Tegernsee", "A0093", None, "Rottach-Egern"),
    ("KULTUR im Oberbräu", "KULTUR im Oberbräu", "A2357", None, "Holzkirchen"),
    ("Oberland Kinocenter", "Oberland Kinocenter", "A1001", Some("/kino/hausham/oberland-kinocenter-32785"), "Hausham")
  ))
  private def p_holzminden: R = ("holzminden", "holzminden-niedersachsen", "Holzminden", "Niedersachsen", 51.828, 9.4455, false, Seq("Holzminden"), Seq(
    ("Roxy Filmcentrum", "Roxy Filmcentrum", "A0697", None, "Holzminden")
  ))
  private def p_homburg: R = ("homburg", "homburg-saarland", "Homburg", "Saarland", 49.3264, 7.3387, true, Seq("Homburg", "Zweibrücken", "Sankt Wendel", "Landstuhl"), Seq(
    ("Broadway Ramstein-Miesenbach", "Broadway Ramstein-Miesenbach", "A1319", None, "Landstuhl"),
    ("Cinema Europa", "Cinema Europa", "A0250", Some("/kino/zweibruecken/cinema-europa-37628"), "Zweibrücken"),
    ("Eden Cinehouse", "Eden Cinehouse", "A1399", Some("/kino/homburg-saar/eden-cinehouse-32096"), "Homburg"),
    ("Neues Theater Sankt Wendel", "Neues Theater Sankt Wendel", "A0999", Some("/kino/sankt-wendel/neues-theater-32244"), "Sankt Wendel")
  ))
  private def p_hoyerswerda: R = ("hoyerswerda", "hoyerswerda-sachsen", "Hoyerswerda", "Sachsen", 51.4379, 14.2355, false, Seq("Hoyerswerda"), Seq(
    ("CineMotion Hoyerswerda", "CineMotion Hoyerswerda", "A0363", Some("/kino/hoyerswerda/cinemotion-hoyerswerda-31566"), "Hoyerswerda"),
    ("Kulturfabrik Hoyerswerda e. V.", "Kulturfabrik Hoyerswerda e. V.", "A2624", Some("/kino/hoyerswerda/blow-up-kino-in-der-kulturfabrik-hoyerswerda-81959"), "Hoyerswerda"),
    ("Open Air an der schwarzen Mühle", "Open Air an der schwarzen Mühle", "A2526", None, "Hoyerswerda")
  ))
  private def p_huerth: R = ("huerth", "huerth-nordrhein-westfalen", "Hürth", "Nordrhein-Westfalen", 50.8708, 6.8676, true, Seq("Hürth", "Euskirchen", "Brühl"), Seq(
    ("Cineplex Euskirchen", "Cineplex Euskirchen", "A2792", Some("/kino/euskirchen/cineplex-in-der-galleria-31798"), "Euskirchen"),
    ("CP Lichtspielfreunde Hürth von 2016 e.V.", "CP Lichtspielfreunde Hürth von 2016 e.V.", "A2754", None, "Hürth"),
    ("UCI Hürth Park", "UCI Hürth Park", "A1198", Some("/kino/huerth/uci-kinowelt-huerth-park-40814"), "Hürth"),
    ("Zoom-Kino", "Zoom-Kino", "A1291", Some("/kino/bruehl/zoomkino-31781"), "Brühl")
  ))
  private def p_husum: R = ("husum", "husum-schleswig-holstein", "Husum", "Schleswig-Holstein", 54.4858, 9.0524, false, Seq("Husum"), Seq(
    ("Kino-Center Husum", "Kino-Center Husum", "A0762", Some("/kino/husum/kinocenter-32498"), "Husum")
  ))
  private def p_ibbenbueren: R = ("ibbenbueren", "ibbenbueren-nordrhein-westfalen", "Ibbenbüren", "Nordrhein-Westfalen", 52.2796, 7.7146, true, Seq("Ibbenbüren", "Emsdetten", "Steinfurt"), Seq(
    ("Apollo Kino Center", "Apollo Kino Center", "A0045", Some("/kino/ibbenbueren/apollo-kino-31827"), "Ibbenbüren"),
    ("Kino Steinfurt", "Kino Steinfurt", "A0551", None, "Steinfurt"),
    ("Metropolis Kino Emsdetten", "Metropolis Kino Emsdetten", "A2809", None, "Emsdetten")
  ))
  private def p_ilmenau: R = ("ilmenau", "ilmenau-thueringen", "Ilmenau", "Thüringen", 50.6832, 10.9186, false, Seq("Ilmenau"), Seq(
    ("Hochschulfilmclub TU Ilmenau", "Hochschulfilmclub TU Ilmenau", "A0610", Some("/kino/ilmenau-thueringen/hochschulfilmclub-43299"), "Ilmenau"),
    ("Linden Lichtspiele", "Linden Lichtspiele", "A0917", Some("/kino/ilmenau-thueringen/linden-lichtspiele-31567"), "Ilmenau")
  ))
  private def p_iserlohn: R = ("iserlohn", "iserlohn-nordrhein-westfalen", "Iserlohn", "Nordrhein-Westfalen", 51.3755, 7.7028, true, Seq("Iserlohn", "Unna", "Menden", "Herdecke"), Seq(
    ("Filmpalast Iserlohn", "Filmpalast Iserlohn", "A2796", Some("/kino/iserlohn/filmpalast-39711"), "Iserlohn"),
    ("Kinorama Unna", "Kinorama Unna", "A0491", Some("/kino/unna/kinorama-13450"), "Unna"),
    ("Onikon", "Onikon", "A1014", Some("/kino/herdecke/onikon-605"), "Herdecke"),
    ("Corso-Kino Menden", "Corso-Kino Menden", "A2816", None, "Menden")
  ))
  private def p_juelich: R = ("juelich", "juelich-nordrhein-westfalen", "Jülich", "Nordrhein-Westfalen", 50.9215, 6.3627, false, Seq("Jülich"), Seq(
    ("KuBa", "KuBa", "A1680", Some("/kino/juelich/kuba-70097"), "Jülich"),
    ("Open Air Kino Jülich", "Open Air Kino Jülich", "A2765", None, "Jülich")
  ))
  private def p_kaiserslautern: R = ("kaiserslautern", "kaiserslautern-rheinland-pfalz", "Kaiserslautern", "Rheinland-Pfalz", 49.443, 7.7716, false, Seq("Kaiserslautern"), Seq(
    ("UCI Kaiserslautern", "UCI Kaiserslautern", "A0669", Some("/kino/kaiserslautern/uci-kinowelt-kaiserslautern-37896"), "Kaiserslautern"),
    ("Union - Studio für Filmkunst", "Union - Studio für Filmkunst", "A1226", Some("/kino/kaiserslautern/union-studio-fuer-filmkunst-32105"), "Kaiserslautern")
  ))
  private def p_kaufbeuren: R = ("kaufbeuren", "kaufbeuren-bayern", "Kaufbeuren", "Bayern", 47.8824, 10.6219, false, Seq("Kaufbeuren"), Seq(
    ("CICO Kaufbeuren", "CICO Kaufbeuren", "A0422", None, "Kaufbeuren"),
    ("Corona KinoPlex - Open Air", "Corona KinoPlex - Open Air", "A2893", Some("/kino/kaufbeuren/corona-kinoplex-open-air-38671"), "Kaufbeuren")
  ))
  private def p_kaufering: R = ("kaufering", "kaufering-bayern", "Kaufering", "Bayern", 48.0912, 10.8791, true, Seq("Kaufering", "Penzing"), Seq(
    ("Cineplex Penzing", "Cineplex Penzing", "A2643", Some("/kino/penzing/cineplex-82997"), "Penzing"),
    ("Filmpalast Kaufering", "Filmpalast Kaufering", "A2617", Some("/kino/kaufering/filmpalast-kaufering-78957"), "Kaufering")
  ))
  private def p_kehl: R = ("kehl", "kehl-baden-wuerttemberg", "Kehl", "Baden-Württemberg", 48.573, 7.8152, true, Seq("Kehl", "Oberkirch"), Seq(
    ("Kino-Center Kehl", "Kino-Center Kehl", "A1507", Some("/kino/kehl/kinocenter-32122"), "Kehl"),
    ("Open-Air Oberkirch", "Open-Air Oberkirch", "A2525", Some("/kino/oberkirch/open-air-kino-76097"), "Oberkirch")
  ))
  private def p_kelheim: R = ("kelheim", "kelheim-bayern", "Kelheim", "Bayern", 48.9173, 11.8862, true, Seq("Kelheim", "Abensberg"), Seq(
    ("Kelheimer Lichtspiele", "Kelheimer Lichtspiele", "A1462", None, "Kelheim"),
    ("Roxy Kino", "Roxy Kino", "A1086", Some("/kino/abensberg/roxy-kino-32654"), "Abensberg")
  ))
  private def p_kempten_im_allgaeu: R = ("kempten-im-allgaeu", "kempten-im-allgaeu-bayern", "Kempten im Allgäu", "Bayern", 47.7267, 10.3139, true, Seq("Kempten im Allgäu", "Marktoberdorf", "Immenstadt im Allgäu"), Seq(
    ("Colosseum-Center", "Colosseum-Center", "A1382", None, "Kempten im Allgäu"),
    ("Filmburg - Das Theaterkino", "Filmburg - Das Theaterkino", "A1409", Some("/kino/marktoberdorf/filmburg-das-theaterkino-32841"), "Marktoberdorf"),
    ("Union Filmtheater Immenstadt im Allgäu", "Union Filmtheater Immenstadt im Allgäu", "A1227", None, "Immenstadt im Allgäu")
  ))
  private def p_kerpen: R = ("kerpen", "kerpen-nordrhein-westfalen", "Kerpen", "Nordrhein-Westfalen", 50.8699, 6.6969, true, Seq("Kerpen", "Frechen"), Seq(
    ("Capitol-Theater Kerpen", "Capitol-Theater Kerpen", "A1340", Some("/kino/kerpen/capitoltheater-33457"), "Kerpen"),
    ("Euromax - Cinemas", "Euromax - Cinemas", "A0471", Some("/kino/kerpen/euromax-cinemas-31830"), "Kerpen"),
    ("Linden Theater Frechen", "Linden Theater Frechen", "A0918", Some("/kino/frechen/lindentheater-31800"), "Frechen")
  ))
  private def p_kirchheim_unter_teck: R = ("kirchheim-unter-teck", "kirchheim-unter-teck-baden-wuerttemberg", "Kirchheim unter Teck", "Baden-Württemberg", 48.6468, 9.4538, false, Seq("Kirchheim unter Teck"), Seq(
    ("Kommunales Kino Kirchheim unter Teck", "Kommunales Kino Kirchheim unter Teck", "A1692", None, "Kirchheim unter Teck"),
    ("Sommernachtskino (Kirchheim unter Teck)", "Sommernachtskino (Kirchheim unter Teck)", "A2823", None, "Kirchheim unter Teck"),
    ("Tyroler-Lichtspiele", "Tyroler-Lichtspiele", "A1197", Some("/kino/kirchheim-unter-teck/tyrolerlichtspiele-32813"), "Kirchheim unter Teck")
  ))
  private def p_kitzingen: R = ("kitzingen", "kitzingen-bayern", "Kitzingen", "Bayern", 49.7397, 10.1507, true, Seq("Kitzingen", "Ochsenfurt", "Dettelbach", "Sommerach"), Seq(
    ("Casablanca Ochsenfurt", "Casablanca Ochsenfurt", "A0173", Some("/kino/ochsenfurt/casablanca-32206"), "Ochsenfurt"),
    ("Cineworld Mainfrankenpark Dettelbach", "Cineworld Mainfrankenpark Dettelbach", "A0387", Some("/kino/dettelbach/cineworld-erlebniskino-mainfrankenpark-39999"), "Dettelbach"),
    ("Open Air Kino Winzerkeller Sommerach", "Open Air Kino Winzerkeller Sommerach", "A2277", Some("/kino/sommerach/open-air-kino-im-winzerkeller-73921"), "Sommerach"),
    ("Roxy Kino Kitzingen", "Roxy Kino Kitzingen", "A1738", Some("/kino/kitzingen/roxy-91837"), "Kitzingen")
  ))
  private def p_kleve: R = ("kleve", "kleve-nordrhein-westfalen", "Kleve", "Nordrhein-Westfalen", 51.7883, 6.1387, true, Seq("Kleve", "Goch"), Seq(
    ("Goli Theater Goch", "Goli Theater Goch", "A2597", Some("/kino/goch/goli-theater-goch-76617"), "Goch"),
    ("Tichelpark Kleve", "Tichelpark Kleve", "A1182", Some("/kino/kleve/tichelpark-kleve-30475"), "Kleve")
  ))
  private def p_kloeden: R = ("kloeden", "kloeden-sachsen-anhalt", "Klöden", "Sachsen-Anhalt", 51.7618, 12.8317, false, Seq("Klöden"), Seq(
    ("Sommerkino Klöden", "Sommerkino Klöden", "A2508", None, "Klöden")
  ))
  private def p_koenigs_wusterhausen: R = ("koenigs-wusterhausen", "koenigs-wusterhausen-brandenburg", "Königs Wusterhausen", "Brandenburg", 52.3014, 13.633, true, Seq("Königs Wusterhausen", "Erkner", "Wildau"), Seq(
    ("Capitol Königs Wusterhausen", "Capitol Königs Wusterhausen", "A0144", Some("/kino/koenigs-wusterhausen/capitol-das-kulturkino-61765"), "Königs Wusterhausen"),
    ("CineStar Wildau", "CineStar Wildau", "A0330", Some("/kino/wildau-bei-koenigs-wusterhausen/cinestar-38777"), "Wildau"),
    ("Movieland", "Movieland", "A0671", Some("/kino/erkner/movieland-76362"), "Erkner")
  ))
  private def p_koenigsbrunn: R = ("koenigsbrunn", "koenigsbrunn-bayern", "Königsbrunn", "Bayern", 48.2751, 10.8918, true, Seq("Königsbrunn", "Gersthofen", "Langerringen"), Seq(
    ("Cineplex Königsbrunn", "Cineplex Königsbrunn", "A0312", Some("/kino/koenigsbrunn-bei-augsburg/cineplex-35123"), "Königsbrunn"),
    ("Open Air Seebühne", "Open Air Seebühne", "A2539", None, "Gersthofen"),
    ("Filmhaus (Langerringen)", "Filmhaus (Langerringen)", "cineamo:3463", None, "Langerringen")
  ))
  private def p_konstanz: R = ("konstanz", "konstanz-baden-wuerttemberg", "Konstanz", "Baden-Württemberg", 47.6603, 9.1758, false, Seq("Konstanz"), Seq(
    ("CineStar Konstanz", "CineStar Konstanz", "A0338", Some("/kino/konstanz-universitaetsstadt/cinestar-konstanz-60996"), "Konstanz"),
    ("Open Air am Neuwerk", "Open Air am Neuwerk", "A2774", None, "Konstanz"),
    ("Zebra", "Zebra", "A1287", Some("/kino/konstanz-universitaetsstadt/zebra-kommunales-kino-konstanz-e.v.-32128"), "Konstanz")
  ))
  private def p_kuehlungsborn: R = ("kuehlungsborn", "kuehlungsborn-mecklenburg-vorpommern", "Kühlungsborn", "Mecklenburg-Vorpommern", 54.1476, 11.7432, true, Seq("Kühlungsborn", "Bad Doberan", "Rerik"), Seq(
    ("Kino- und Kulturverein", "Kino- und Kulturverein", "A0643", Some("/kino/bad-doberan/kino-und-kulturverein-31342"), "Bad Doberan"),
    ("Open Air Strandkorbkino am Bootshafen", "Open Air Strandkorbkino am Bootshafen", "A2059", None, "Kühlungsborn"),
    ("Ostseekino Kühlungsborn", "Ostseekino Kühlungsborn", "A1484", Some("/kino/kuehlungsborn-ostseebad/ostseekino-60856"), "Kühlungsborn"),
    ("Kino am Campingpark Rerik", "Kino am Campingpark Rerik", "cineamo:1386", None, "Rerik")
  ))
  private def p_lahnstein: R = ("lahnstein", "lahnstein-rheinland-pfalz", "Lahnstein", "Rheinland-Pfalz", 50.3, 7.6167, true, Seq("Lahnstein", "Boppard"), Seq(
    ("Cinema Boppard", "Cinema Boppard", "A1360", None, "Boppard"),
    ("Kino Lahnstein", "Kino Lahnstein", "A1196", Some("/kino/lahnstein/kino-lahnstein-32136"), "Lahnstein")
  ))
  private def p_lahr_am_schwarzwald: R = ("lahr-am-schwarzwald", "lahr-am-schwarzwald-baden-wuerttemberg", "Lahr am Schwarzwald", "Baden-Württemberg", 48.3404, 7.8689, true, Seq("Lahr am Schwarzwald", "Haslach im Kinzigtal", "Rust"), Seq(
    ("FORUM Lahr", "FORUM Lahr", "A2648", Some("/kino/lahr/forum-84797"), "Lahr am Schwarzwald"),
    ("Magic Cinema im Europa Park", "Magic Cinema im Europa Park", "A0936", None, "Rust"),
    ("Rio + Scala", "Rio + Scala", "A1081", None, "Haslach im Kinzigtal")
  ))
  private def p_landau_in_der_pfalz: R = ("landau-in-der-pfalz", "landau-in-der-pfalz-rheinland-pfalz", "Landau in der Pfalz", "Rheinland-Pfalz", 49.1984, 8.1169, false, Seq("Landau in der Pfalz"), Seq(
    ("Filmwelt Landau", "Filmwelt Landau", "A0548", None, "Landau in der Pfalz"),
    ("Universum Kinocenter", "Universum Kinocenter", "A1245", None, "Landau in der Pfalz")
  ))
  private def p_landsberg_am_lech: R = ("landsberg-am-lech", "landsberg-am-lech-bayern", "Landsberg am Lech", "Bayern", 48.0482, 10.8828, false, Seq("Landsberg am Lech"), Seq(
    ("Filmforum im Stadttheater Landsberg", "Filmforum im Stadttheater Landsberg", "A2795", Some("/kino/landsberg-am-lech/filmforum-im-stadttheater-landsberg-93063"), "Landsberg am Lech"),
    ("Olympia Filmtheater Landsberg", "Olympia Filmtheater Landsberg", "A1579", Some("/kino/landsberg-am-lech/olympia-filmtheater-32822"), "Landsberg am Lech"),
    ("Open Air Kino Landsberg", "Open Air Kino Landsberg", "A2207", None, "Landsberg am Lech")
  ))
  private def p_landshut: R = ("landshut", "landshut-bayern", "Landshut", "Bayern", 48.5296, 12.1618, false, Seq("Landshut"), Seq(
    ("Filmzentrum e.V. Kinoptikum", "Filmzentrum e.V. Kinoptikum", "A0834", Some("/kino/landshut-isar/filmzentrum-e.v.-kinoptikum-34563"), "Landshut"),
    ("Kinopolis Landshut", "Kinopolis Landshut", "A0026", Some("/kino/landshut-isar/kinopolis-57317"), "Landshut"),
    ("Kleines Theater Landshut", "Kleines Theater Landshut", "A2852", None, "Landshut")
  ))
  private def p_langenfeld: R = ("langenfeld", "langenfeld-nordrhein-westfalen", "Langenfeld", "Nordrhein-Westfalen", 51.1082, 6.9483, true, Seq("Langenfeld", "Hilden", "Monheim am Rhein"), Seq(
    ("Emotion Kino", "Emotion Kino", "G02P3", Some("/kino/monheim-am-rhein/emotion-kino-93182"), "Monheim am Rhein"),
    ("Lux Lichtspiele", "Lux Lichtspiele", "A0933", Some("/kino/hilden/lux-lichtspiele-946"), "Hilden"),
    ("Programmkino im Schaustall", "Programmkino im Schaustall", "A1043", Some("/kino/langenfeld/programmkino-im-schaustall-54937"), "Langenfeld"),
    ("Rex Langenfeld", "Rex Langenfeld", "A1067", Some("/kino/langenfeld/rex-31853"), "Langenfeld")
  ))
  private def p_lehrte: R = ("lehrte", "lehrte-niedersachsen", "Lehrte", "Niedersachsen", 52.3719, 9.9792, true, Seq("Lehrte", "Peine", "Burgdorf"), Seq(
    ("Astoria-Filmtheater", "Astoria-Filmtheater", "A0020", Some("/kino/peine/astoriafilmtheater-72276"), "Peine"),
    ("Das Andere Kino", "Das Andere Kino", "A0431", Some("/kino/lehrte/das-andere-kino-32524"), "Lehrte"),
    ("Open Air im Stadtpark", "Open Air im Stadtpark", "A2867", None, "Lehrte"),
    ("Neue Schauburg Burgdorf", "Neue Schauburg Burgdorf", "A1572", Some("/kino/burgdorf/neue-schauburg-32381"), "Burgdorf")
  ))
  private def p_letschin: R = ("letschin", "letschin-brandenburg", "Letschin", "Brandenburg", 52.6438, 14.3601, false, Seq("Letschin"), Seq(
    ("Haus Lichtblick", "Haus Lichtblick", "A2362", Some("/kino/letschin/lichtblick-75021"), "Letschin")
  ))
  private def p_leutkirch_im_allgaeu: R = ("leutkirch-im-allgaeu", "leutkirch-im-allgaeu-baden-wuerttemberg", "Leutkirch im Allgäu", "Baden-Württemberg", 47.8267, 10.0205, false, Seq("Leutkirch im Allgäu"), Seq(
    ("Centraltheater", "Centraltheater", "A0205", None, "Leutkirch im Allgäu"),
    ("Open-Air-Kino im Museumshof", "Open-Air-Kino im Museumshof", "A2548", None, "Leutkirch im Allgäu")
  ))
  private def p_limbach_oberfrohna: R = ("limbach-oberfrohna", "limbach-oberfrohna-sachsen", "Limbach-Oberfrohna", "Sachsen", 50.8588, 12.7616, true, Seq("Limbach-Oberfrohna", "Glauchau", "Hohenstein-Ernstthal", "Lichtenstein"), Seq(
    ("Capitol Hohenstein-Ernstthal", "Capitol Hohenstein-Ernstthal", "A0153", Some("/kino/hohensteinernstthal/filmtheater-capitol-31565"), "Hohenstein-Ernstthal"),
    ("Clubkino &quot;Capitol&quot;", "Clubkino &quot;Capitol&quot;", "A0414", None, "Lichtenstein"),
    ("Clubkino Glauchau e.V.", "Clubkino Glauchau e.V.", "A2315", Some("/kino/glauchau/clubkino-glauchau-e.v.-74299"), "Glauchau"),
    ("Filmtheater &quot;Apollo&quot;", "Filmtheater &quot;Apollo&quot;", "A0540", Some("/kino/limbachoberfrohna/apollo-filmtheater-31589"), "Limbach-Oberfrohna")
  ))
  private def p_limburg: R = ("limburg", "limburg-hessen", "Limburg", "Hessen", 50.3836, 8.0503, true, Seq("Limburg", "Weilburg", "Montabaur", "Hahnstätten"), Seq(
    ("Capitol-Kinocenter Montabaur", "Capitol-Kinocenter Montabaur", "A0163", Some("/kino/montabaur/capitolkinocenter-32184"), "Montabaur"),
    ("Cineplex Limburg", "Cineplex Limburg", "A0319", Some("/kino/limburg-an-der-lahn/cineplex-32149"), "Limburg"),
    ("Delphi - Filmtheater", "Delphi - Filmtheater", "A0434", Some("/kino/weilburg/delphi-filmtheater-42237"), "Weilburg"),
    ("Kreml Kulturhaus", "Kreml Kulturhaus", "A2755", Some("/kino/hahnstaetten/kreml-kulturhaus-66339"), "Hahnstätten")
  ))
  private def p_lindau: R = ("lindau", "lindau-bayern", "Lindau", "Bayern", 47.5461, 9.6843, true, Seq("Lindau", "Kressbronn"), Seq(
    ("Club Vaudeville", "Club Vaudeville", "A0411", Some("/kino/lindau/club-vaudeville-lindau-e.v.-69737"), "Lindau"),
    ("Parktheater und Studio", "Parktheater und Studio", "A1031", Some("/kino/lindau/parktheater-und-studio-32834"), "Lindau"),
    ("Kinomobil Stuttgart - Lände-Cafe", "Kinomobil Stuttgart - Lände-Cafe", "A1761", Some("/kino/kressbronn/kinomobil-stuttgart-cafe-laende-41859"), "Kressbronn")
  ))
  private def p_lingen: R = ("lingen", "lingen-niedersachsen", "Lingen", "Niedersachsen", 52.5227, 7.3255, false, Seq("Lingen"), Seq(
    ("Central-Kino Emsland (Lingen)", "Central-Kino Emsland (Lingen)", "A0200", Some("/kino/lingen/centralkino-32529"), "Lingen"),
    ("Filmpalast Cine-World", "Filmpalast Cine-World", "G02Q9", Some("/kino/lingen/filmpalast-cineworld-37227"), "Lingen")
  ))
  private def p_lippstadt: R = ("lippstadt", "lippstadt-nordrhein-westfalen", "Lippstadt", "Nordrhein-Westfalen", 51.6737, 8.3448, false, Seq("Lippstadt"), Seq(
    ("Cinema+Studio Lippstadt", "Cinema+Studio Lippstadt", "A2897", None, "Lippstadt"),
    ("Cineplex Lippstadt", "Cineplex Lippstadt", "A0310", None, "Lippstadt")
  ))
  private def p_loeningen: R = ("loeningen", "loeningen-niedersachsen", "Löningen", "Niedersachsen", 52.7364, 7.7574, true, Seq("Löningen", "Quakenbrück"), Seq(
    ("Li-Lo-Lichtspiele", "Li-Lo-Lichtspiele", "A0916", Some("/kino/loeningen/lilolichtspiele-32531"), "Löningen"),
    ("Schauburg Filmtheater", "Schauburg Filmtheater", "A1120", Some("/kino/quakenbrueck/schauburg-filmtheater-33162"), "Quakenbrück")
  ))
  private def p_loerrach: R = ("loerrach", "loerrach-baden-wuerttemberg", "Lörrach", "Baden-Württemberg", 47.615, 7.6646, false, Seq("Lörrach"), Seq(
    ("Cineplex Lörrach", "Cineplex Lörrach", "A0321", Some("/kino/loerrach/cineplex-32153"), "Lörrach"),
    ("Kino Free Cinema", "Kino Free Cinema", "A0560", Some("/kino/loerrach/kino-free-cinema-69696"), "Lörrach"),
    ("Open Air im Hof", "Open Air im Hof", "A2865", None, "Lörrach")
  ))
  private def p_lohr_am_main: R = ("lohr-am-main", "lohr-am-main-bayern", "Lohr am Main", "Bayern", 49.9892, 9.5722, true, Seq("Lohr am Main", "Karlstadt am Main"), Seq(
    ("Burg-Lichtspiele (Karlstadt am Main)", "Burg-Lichtspiele (Karlstadt am Main)", "A0118", None, "Karlstadt am Main"),
    ("Stattkino", "Stattkino", "kinoheld:3609", None, "Lohr am Main")
  ))
  private def p_luckenwalde: R = ("luckenwalde", "luckenwalde-brandenburg", "Luckenwalde", "Brandenburg", 52.0903, 13.1677, false, Seq("Luckenwalde"), Seq(
    ("Union Kino-Center", "Union Kino-Center", "A1233", Some("/kino/luckenwalde/union-kinocenter-54237"), "Luckenwalde")
  ))
  private def p_ludwigsburg: R = ("ludwigsburg", "ludwigsburg-baden-wuerttemberg", "Ludwigsburg", "Baden-Württemberg", 48.8973, 9.1916, false, Seq("Ludwigsburg"), Seq(
    ("Caligari Ludwigsburg", "Caligari Ludwigsburg", "A1324", Some("/kino/ludwigsburg-wuerttemberg/caligari-32837"), "Ludwigsburg"),
    ("Central Theater Ludwigsburg", "Central Theater Ludwigsburg", "A0195", Some("/kino/ludwigsburg-wuerttemberg/central-theater-32839"), "Ludwigsburg"),
    ("Ludwigsburger Sommernachts Open Air Kino", "Ludwigsburger Sommernachts Open Air Kino", "A1944", Some("/kino/ludwigsburg-wuerttemberg/ludwigsburger-sommernachts-open-air-kino-76083"), "Ludwigsburg"),
    ("Luna Lichtspieltheater", "Luna Lichtspieltheater", "A0928", Some("/kino/ludwigsburg-wuerttemberg/luna-lichtspieltheater-39661"), "Ludwigsburg"),
    ("Scala-Kino Ludwigsburg", "Scala-Kino Ludwigsburg", "A1108", Some("/kino/ludwigsburg-wuerttemberg/scalakino-32838"), "Ludwigsburg"),
    ("Union-Theater Ludwigsburg", "Union-Theater Ludwigsburg", "A1238", Some("/kino/ludwigsburg-wuerttemberg/uniontheater-32840"), "Ludwigsburg")
  ))
  private def p_ludwigslust: R = ("ludwigslust", "ludwigslust-mecklenburg-vorpommern", "Ludwigslust", "Mecklenburg-Vorpommern", 53.3245, 11.4971, false, Seq("Ludwigslust"), Seq(
    ("Luna Filmtheater", "Luna Filmtheater", "A1732", Some("/kino/ludwigslust-mecklenburg/luna-filmtheater-70497"), "Ludwigslust")
  ))
  private def p_luebben: R = ("luebben", "luebben-brandenburg", "Lübben", "Brandenburg", 51.9381, 13.8883, false, Seq("Lübben"), Seq(
    ("Spreewald Lichtspiele Lübben", "Spreewald Lichtspiele Lübben", "A1144", None, "Lübben")
  ))
  private def p_luedenscheid: R = ("luedenscheid", "luedenscheid-nordrhein-westfalen", "Lüdenscheid", "Nordrhein-Westfalen", 51.2198, 7.6273, true, Seq("Lüdenscheid", "Altena"), Seq(
    ("Apollo Service Kino", "Apollo Service Kino", "A1297", Some("/kino/altena/apollo-service-kino-30"), "Altena"),
    ("Filmpalast Lüdenscheid", "Filmpalast Lüdenscheid", "A0519", Some("/kino/luedenscheid/filmpalast-5201"), "Lüdenscheid"),
    ("Park-Theater Lüdenscheid", "Park-Theater Lüdenscheid", "A1030", Some("/kino/luedenscheid/parktheater-30120"), "Lüdenscheid")
  ))
  private def p_lueneburg: R = ("lueneburg", "lueneburg-niedersachsen", "Lüneburg", "Niedersachsen", 53.2512, 10.4155, false, Seq("Lüneburg"), Seq(
    ("Filmpalast Lüneburg", "Filmpalast Lüneburg", "A0327", Some("/kino/lueneburg/filmpalast-41917"), "Lüneburg"),
    ("Scala Kinocenter Lüneburg", "Scala Kinocenter Lüneburg", "A1616", Some("/kino/lueneburg/scala-kinocenter-32539"), "Lüneburg")
  ))
  private def p_luenen: R = ("luenen", "luenen-nordrhein-westfalen", "Lünen", "Nordrhein-Westfalen", 51.6163, 7.5287, true, Seq("Lünen", "Werne", "Lüdinghausen"), Seq(
    ("Capitol-Cinema-Center", "Capitol-Cinema-Center", "A0155", Some("/kino/werne/capitolcinemacenter-31939"), "Werne"),
    ("Cineworld Lünen", "Cineworld Lünen", "A0391", Some("/kino/luenen/cineworld-47639"), "Lünen"),
    ("CineMotion Lüdinghausen", "CineMotion Lüdinghausen", "cineamo:1724", None, "Lüdinghausen")
  ))
  private def p_lutherstadt_wittenberg: R = ("lutherstadt-wittenberg", "lutherstadt-wittenberg-sachsen-anhalt", "Lutherstadt Wittenberg", "Sachsen-Anhalt", 51.8661, 12.6497, false, Seq("Lutherstadt Wittenberg"), Seq(
    ("Centralkino Wittenberg", "Centralkino Wittenberg", "A0209", Some("/kino/wittenberg/central-kino-31703"), "Lutherstadt Wittenberg"),
    ("Sommerkino auf dem Cranach-Hof", "Sommerkino auf dem Cranach-Hof", "A2507", Some("/kino/lutherstadt-wittenberg/sommerkino-auf-dem-cranachhof-75996"), "Lutherstadt Wittenberg")
  ))
  private def p_marburg: R = ("marburg", "marburg-hessen", "Marburg", "Hessen", 50.809, 8.7707, false, Seq("Marburg"), Seq(
    ("Capitol Marburg", "Capitol Marburg", "A1332", Some("/kino/marburg/capitol-32175"), "Marburg"),
    ("Cineplex Marburg", "Cineplex Marburg", "A0320", Some("/kino/marburg/cineplex-42577"), "Marburg"),
    ("Kino Cafe Trauma", "Kino Cafe Trauma", "A0714", Some("/kino/marburg/kino-cafe-trauma-69717"), "Marburg"),
    ("Open Air Kino Marburg", "Open Air Kino Marburg", "A1855", None, "Marburg")
  ))
  private def p_marktredwitz: R = ("marktredwitz", "marktredwitz-bayern", "Marktredwitz", "Bayern", 50.0044, 12.0859, true, Seq("Marktredwitz", "Selb", "Tirschenreuth", "Mitterteich"), Seq(
    ("Angerlichtspiele Mitterteich", "Angerlichtspiele Mitterteich", "A2635", Some("/kino/mitterteich/angerlichtspiele-32851"), "Mitterteich"),
    ("Cineplanet Marktredwitz", "Cineplanet Marktredwitz", "A0870", Some("/kino/marktredwitz/cineplanet-45477"), "Marktredwitz"),
    ("Cineplanet Tirschenreuth", "Cineplanet Tirschenreuth", "A0302", Some("/kino/tirschenreuth/cineplanet-38303"), "Tirschenreuth"),
    ("Cineplex Kinocenter Selb", "Cineplex Kinocenter Selb", "A0763", None, "Selb")
  ))
  private def p_marl: R = ("marl", "marl-nordrhein-westfalen", "Marl", "Nordrhein-Westfalen", 51.6567, 7.0904, true, Seq("Marl", "Dorsten", "Gladbeck"), Seq(
    ("Central Kinocenter Dorsten", "Central Kinocenter Dorsten", "A2788", Some("/kino/dorsten/central-kinocenter-186"), "Dorsten"),
    ("Kommunales Kino im Studio der Stadtbücherei", "Kommunales Kino im Studio der Stadtbücherei", "A0861", None, "Gladbeck"),
    ("Loe Studios Marl", "Loe Studios Marl", "A2910", Some("/kino/marl/loe-studios-91739"), "Marl")
  ))
  private def p_mayen: R = ("mayen", "mayen-rheinland-pfalz", "Mayen", "Rheinland-Pfalz", 50.328, 7.2228, true, Seq("Mayen", "Nürburg"), Seq(
    ("Corso Kino", "Corso Kino", "A0424", Some("/kino/mayen/corso-kino-58538"), "Mayen"),
    ("ring°kino", "ring°kino", "A2670", Some("/kino/nuerburg/ring%C2%B0kino-78917"), "Nürburg")
  ))
  private def p_meiningen: R = ("meiningen", "meiningen-thueringen", "Meiningen", "Thüringen", 50.5679, 10.4152, true, Seq("Meiningen", "Schmalkalden"), Seq(
    ("Casino Lichtspiele Meiningen", "Casino Lichtspiele Meiningen", "A0181", Some("/kino/meiningen/casino-lichtspiele-31607"), "Meiningen"),
    ("Open Air Kino Schloss Wilhelmsburg", "Open Air Kino Schloss Wilhelmsburg", "A2246", None, "Schmalkalden")
  ))
  private def p_meissen: R = ("meissen", "meissen-sachsen", "Meissen", "Sachsen", 51.1616, 13.4737, true, Seq("Meissen", "Weinböhla", "Radeburg"), Seq(
    ("Filmpalast Meißen", "Filmpalast Meißen", "A0371", Some("/kino/meissen-sachsen/filmpalast-38490"), "Meissen"),
    ("Kinobar", "Kinobar", "A1759", None, "Radeburg"),
    ("Openair Zentralgasthof Weinböhla GmbH", "Openair Zentralgasthof Weinböhla GmbH", "A2202", None, "Weinböhla")
  ))
  private def p_memmingen: R = ("memmingen", "memmingen-bayern", "Memmingen", "Bayern", 47.9837, 10.1853, false, Seq("Memmingen"), Seq(
    ("Cineplex Memmingen", "Cineplex Memmingen", "A2113", Some("/kino/memmingen/cineplex-memmingen-72457"), "Memmingen"),
    ("Kaminwerk", "Kaminwerk", "A2848", Some("/kino/memmingen/kaminwerk-93104"), "Memmingen"),
    ("Rex Palast Memmingen", "Rex Palast Memmingen", "G0GK4", None, "Memmingen")
  ))
  private def p_meppen: R = ("meppen", "meppen-niedersachsen", "Meppen", "Niedersachsen", 52.6906, 7.291, true, Seq("Meppen", "Haren", "Haselünne"), Seq(
    ("Ferienzentrum Schloss Dankern", "Ferienzentrum Schloss Dankern", "A1387", Some("/kino/haren-ems/schloss-dankern-kino-33248"), "Haren"),
    ("Hasetor-Lichtspiele", "Hasetor-Lichtspiele", "A2650", None, "Haselünne"),
    ("Kino Meppen", "Kino Meppen", "A0573", Some("/kino/meppen/kino-meppen-34288"), "Meppen")
  ))
  private def p_merseburg: R = ("merseburg", "merseburg-sachsen-anhalt", "Merseburg", "Sachsen-Anhalt", 51.3548, 11.9892, true, Seq("Merseburg", "Leuna"), Seq(
    ("Domstadt Kino", "Domstadt Kino", "A0450", Some("/kino/merseburg-saale/domstadt-kino-65317"), "Merseburg"),
    ("UCI Nova Eventis", "UCI Nova Eventis", "A1212", None, "Leuna")
  ))
  private def p_merzig: R = ("merzig", "merzig-saarland", "Merzig", "Saarland", 49.4433, 6.6387, true, Seq("Merzig", "Schmelz"), Seq(
    ("Odeon Kino Merzig", "Odeon Kino Merzig", "A0679", Some("/kino/merzig-saar/odeon-kinocenter-65016"), "Merzig"),
    ("Schmelzer Lichtspiele", "Schmelzer Lichtspiele", "A0909", Some("/kino/schmelz-saar/schmelzer-lichtspiele-69857"), "Schmelz")
  ))
  private def p_meschede: R = ("meschede", "meschede-nordrhein-westfalen", "Meschede", "Nordrhein-Westfalen", 51.3502, 8.2833, true, Seq("Meschede", "Brilon", "Korbach", "Willingen"), Seq(
    ("Cine K Korbach", "Cine K Korbach", "A2902", Some("/kino/korbach/cinek-91980"), "Korbach"),
    ("Cineplex Brilon", "Cineplex Brilon", "G00YX", Some("/kino/brilon/cineplex-brilon-92576"), "Brilon"),
    ("Kino Studio Willingen", "Kino Studio Willingen", "A0096", Some("/kino/willingen/kino-studio-60757"), "Willingen"),
    ("Linden-Theater", "Linden-Theater", "A1701", None, "Meschede")
  ))
  private def p_minden: R = ("minden", "minden-nordrhein-westfalen", "Minden", "Nordrhein-Westfalen", 52.2895, 8.9146, true, Seq("Minden", "Stadthagen", "Bückeburg"), Seq(
    ("Filmtheater &quot;Die Birke&quot;", "Filmtheater &quot;Die Birke&quot;", "A2798", Some("/kino/minden-westfalen/die-birke-31866"), "Minden"),
    ("Kleines Theater Am Weingarten", "Kleines Theater Am Weingarten", "kinoheld:3905", None, "Minden"),
    ("Kinocenter Stadthagen", "Kinocenter Stadthagen", "A1632", Some("/kino/stadthagen/kinocenter-32618"), "Stadthagen"),
    ("Residenz-Kino-Center", "Residenz-Kino-Center", "A1061", Some("/kino/bueckeburg/residenzkinocenter-32379"), "Bückeburg")
  ))
  private def p_moelln: R = ("moelln", "moelln-schleswig-holstein", "Mölln", "Schleswig-Holstein", 53.6207, 10.6875, true, Seq("Mölln", "Schwarzenbek", "Ratzeburg", "Boizenburg"), Seq(
    ("Burgtheater Ratzeburg", "Burgtheater Ratzeburg", "A0120", Some("/kino/ratzeburg/burgtheater-32593"), "Ratzeburg"),
    ("Eulenspiegelkino Mölln", "Eulenspiegelkino Mölln", "A0741", None, "Mölln"),
    ("Kino Boizenburg", "Kino Boizenburg", "A0710", Some("/kino/boizenburg/kino-boizenburg-65421"), "Boizenburg"),
    ("Kino Grimm", "Kino Grimm", "A0695", Some("/kino/schwarzenbek/kino-grimm-32613"), "Schwarzenbek")
  ))
  private def p_mosbach: R = ("mosbach", "mosbach-baden-wuerttemberg", "Mosbach", "Baden-Württemberg", 49.3536, 9.1511, true, Seq("Mosbach", "Hardthausen am Kocher"), Seq(
    ("Kinomobil Stuttgart - Gewölbekeller im Rathaus", "Kinomobil Stuttgart - Gewölbekeller im Rathaus", "A0786", None, "Hardthausen am Kocher"),
    ("Kinostar Filmwelt Mosbach", "Kinostar Filmwelt Mosbach", "A1513", None, "Mosbach")
  ))
  private def p_muehlacker: R = ("muehlacker", "muehlacker-baden-wuerttemberg", "Mühlacker", "Baden-Württemberg", 48.9475, 8.8368, true, Seq("Mühlacker", "Oberriexingen"), Seq(
    ("Kinomobil Stuttgart - Festhalle Oberriexingen", "Kinomobil Stuttgart - Festhalle Oberriexingen", "A1938", None, "Oberriexingen"),
    ("Scala Filmtheater Mühlacker", "Scala Filmtheater Mühlacker", "A1617", Some("/kino/muehlacker/scalafilmtheater-32854"), "Mühlacker")
  ))
  private def p_muellheim: R = ("muellheim", "muellheim-baden-wuerttemberg", "Müllheim", "Baden-Württemberg", 47.8082, 7.6303, true, Seq("Müllheim", "Bad Krozingen", "Neuenburg am Rhein", "Buggingen"), Seq(
    ("Centra-Theater Müllheim", "Centra-Theater Müllheim", "A0207", Some("/kino/muellheim/centraltheater-32188"), "Müllheim"),
    ("Joki Kino am Bahnhof", "Joki Kino am Bahnhof", "A0722", None, "Bad Krozingen"),
    ("Kino im Rathaus", "Kino im Rathaus", "A0728", Some("/kino/buggingen/kino-im-rathaus-32000"), "Buggingen"),
    ("Kino Im Stadthaus", "Kino Im Stadthaus", "A1486", None, "Neuenburg am Rhein")
  ))
  private def p_murrhardt: R = ("murrhardt", "murrhardt-baden-wuerttemberg", "Murrhardt", "Baden-Württemberg", 48.9819, 9.5705, true, Seq("Murrhardt", "Gaildorf"), Seq(
    ("Kommunales Kino Murrhardt e.V.", "Kommunales Kino Murrhardt e.V.", "A1459", None, "Murrhardt"),
    ("Sonnen-Lichtspiele", "Sonnen-Lichtspiele", "A1140", Some("/kino/gaildorf/sonnenlichtspiele-32764"), "Gaildorf")
  ))
  private def p_nagold: R = ("nagold", "nagold-baden-wuerttemberg", "Nagold", "Baden-Württemberg", 48.5498, 8.7237, true, Seq("Nagold", "Altensteig", "Haiterbach"), Seq(
    ("Kinomobil Stuttgart - Festhalle Haiterbach", "Kinomobil Stuttgart - Festhalle Haiterbach", "A0804", Some("/kino/haiterbach/kinomobil-stuttgart-festhalle-41867"), "Haiterbach"),
    ("Krone-Lichtspiele", "Krone-Lichtspiele", "A0693", Some("/kino/nagold/kronelichtspiele-32896"), "Nagold"),
    ("Open Air im Badepark Nagold", "Open Air im Badepark Nagold", "A2235", None, "Nagold"),
    ("Open-Air Kino im Schlossgarten", "Open-Air Kino im Schlossgarten", "A1934", None, "Altensteig")
  ))
  private def p_naumburg: R = ("naumburg", "naumburg-sachsen-anhalt", "Naumburg", "Sachsen-Anhalt", 51.1499, 11.8098, true, Seq("Naumburg", "Zeitz", "Zwenkau", "Groitzsch", "Bad Klosterlausnitz"), Seq(
    ("Brühl Cinema Zeitz", "Brühl Cinema Zeitz", "A2637", Some("/kino/zeitz-elster/bruehl-cinema-81897"), "Zeitz"),
    ("Bürgerhaus Kino", "Bürgerhaus Kino", "A1320", Some("/kino/groitzsch-bei-pegau/buergerhaus-kino-31546"), "Groitzsch"),
    ("Holzlandkino", "Holzlandkino", "A0618", Some("/kino/bad-klosterlausnitz/holzlandkino-31345"), "Bad Klosterlausnitz"),
    ("Cineplex Naumburg", "Cineplex Naumburg", "A0306", None, "Naumburg"),
    ("KulturKino zwenkau", "KulturKino zwenkau", "kinoheld:3621", None, "Zwenkau")
  ))
  private def p_neckarsulm: R = ("neckarsulm", "neckarsulm-baden-wuerttemberg", "Neckarsulm", "Baden-Württemberg", 49.1891, 9.2253, false, Seq("Neckarsulm"), Seq(
    ("Cineplex Neckarsulm", "Cineplex Neckarsulm", "A0475", Some("/kino/neckarsulm/cineplex-35134"), "Neckarsulm"),
    ("Kinostar Scala + Scala-Keller", "Kinostar Scala + Scala-Keller", "A0839", Some("/kino/neckarsulm/kinostar-scala-58176"), "Neckarsulm")
  ))
  private def p_neresheim: R = ("neresheim", "neresheim-baden-wuerttemberg", "Neresheim", "Baden-Württemberg", 48.7551, 10.3304, true, Seq("Neresheim", "Dischingen"), Seq(
    ("Kinomobil Stuttgart - Härtsfeldhalle", "Kinomobil Stuttgart - Härtsfeldhalle", "A0817", None, "Neresheim"),
    ("Open-Air Kino Ballmertshofer Filmfest", "Open-Air Kino Ballmertshofer Filmfest", "A2285", Some("/kino/dischingen/openair-kino-ballmertshofer-filmfest-73657"), "Dischingen")
  ))
  private def p_neu_anspach: R = ("neu-anspach", "neu-anspach-hessen", "Neu-Anspach", "Hessen", 50.3167, 8.5, false, Seq("Neu-Anspach"), Seq(
    ("Kino Neu Anspach", "Kino Neu Anspach", "A1612", Some("/kino/neuanspach/kino-32194"), "Neu-Anspach"),
    ("Open-Air Kino Schwimmbad Neu-Ansbach", "Open-Air Kino Schwimmbad Neu-Ansbach", "A2107", Some("/kino/neuanspach/openair-kino-schwimmbad-neuansbach-37864"), "Neu-Anspach")
  ))
  private def p_neu_ulm: R = ("neu-ulm", "neu-ulm-bayern", "Neu-Ulm", "Bayern", 48.3928, 10.0111, true, Seq("Neu-Ulm", "Günzburg", "Krumbach", "Sontheim", "Offingen"), Seq(
    ("CinePark Krumbach (Schwaben)", "CinePark Krumbach (Schwaben)", "A0299", Some("/kino/krumbach/cinepark-34801"), "Krumbach"),
    ("Dietrich Theater Neu-Ulm", "Dietrich Theater Neu-Ulm", "A1741", Some("/kino/neuulm/dietrichtheater-70577"), "Neu-Ulm"),
    ("Donaulichtspiele", "Donaulichtspiele", "A1395", Some("/kino/offingen/donaulichtspiele-32925"), "Offingen"),
    ("Kino Biigz", "Kino Biigz", "A2653", Some("/kino/guenzburg/biigz-78877"), "Günzburg"),
    ("Kino in der Dampfsäg", "Kino in der Dampfsäg", "A2588", None, "Sontheim")
  ))
  private def p_neubrandenburg: R = ("neubrandenburg", "neubrandenburg-mecklenburg-vorpommern", "Neubrandenburg", "Mecklenburg-Vorpommern", 53.5573, 13.261, false, Seq("Neubrandenburg"), Seq(
    ("CineStar Neubrandenburg", "CineStar Neubrandenburg", "A0331", Some("/kino/neubrandenburg-mecklenburg/cinestar-31615"), "Neubrandenburg"),
    ("Latücht - Kommunales Kino", "Latücht - Kommunales Kino", "A0894", Some("/kino/neubrandenburg-mecklenburg/kino-latuecht-46637"), "Neubrandenburg")
  ))
  private def p_neuburg_an_der_donau: R = ("neuburg-an-der-donau", "neuburg-an-der-donau-bayern", "Neuburg an der Donau", "Bayern", 48.7322, 11.1871, true, Seq("Neuburg an der Donau", "Schrobenhausen", "Eichstätt", "Rennertshofen"), Seq(
    ("Cinema Kulturtreff Rennertshofen", "Cinema Kulturtreff Rennertshofen", "A0232", None, "Rennertshofen"),
    ("Filmstudio im alten Stadttheater", "Filmstudio im alten Stadttheater", "A2119", Some("/kino/eichstaett/filmstudio-im-alten-stadttheater-32732"), "Eichstätt"),
    ("Herzog-Filmtheater", "Herzog-Filmtheater", "A0605", Some("/kino/schrobenhausen/herzogfilmtheater-32978"), "Schrobenhausen"),
    ("Kinopalast Neuburg an der Donau", "Kinopalast Neuburg an der Donau", "A0820", Some("/kino/neuburg-an-der-donau/kinopalast-60557"), "Neuburg an der Donau")
  ))
  private def p_neumarkt_in_der_oberpfalz: R = ("neumarkt-in-der-oberpfalz", "neumarkt-in-der-oberpfalz-bayern", "Neumarkt in der Oberpfalz", "Bayern", 49.2803, 11.4628, false, Seq("Neumarkt in der Oberpfalz"), Seq(
    ("Cineplex Neumarkt", "Cineplex Neumarkt", "A2716", None, "Neumarkt in der Oberpfalz"),
    ("Rialto Palast", "Rialto Palast", "A1077", Some("/kino/neumarkt/rialto-palast-32899"), "Neumarkt in der Oberpfalz")
  ))
  private def p_neumuenster: R = ("neumuenster", "neumuenster-schleswig-holstein", "Neumünster", "Schleswig-Holstein", 54.074, 9.9846, false, Seq("Neumünster"), Seq(
    ("CineStar Neumünster", "CineStar Neumünster", "A1372", Some("/kino/neumuenster/cinestar-neumuenster-90577"), "Neumünster"),
    ("KDW Neumünster", "KDW Neumünster", "A2655", Some("/kino/neumuenster/kdwneumuenster-81837"), "Neumünster")
  ))
  private def p_neunkirchen: R = ("neunkirchen", "neunkirchen-saarland", "Neunkirchen", "Saarland", 49.3445, 7.1805, true, Seq("Neunkirchen", "Sankt Ingbert"), Seq(
    ("Cinetower", "Cinetower", "A0385", None, "Neunkirchen"),
    ("Neues Eden", "Neues Eden", "A0987", None, "Neunkirchen"),
    ("Kinowerkstatt", "Kinowerkstatt", "A0844", Some("/kino/sankt-ingbert/kinowerkstatt-32242"), "Sankt Ingbert"),
    ("Neues Regina", "Neues Regina", "A1057", Some("/kino/sankt-ingbert/regina-kino-32241"), "Sankt Ingbert")
  ))
  private def p_neuruppin: R = ("neuruppin", "neuruppin-brandenburg", "Neuruppin", "Brandenburg", 52.9282, 12.8031, false, Seq("Neuruppin"), Seq(
    ("Union Filmtheater Neuruppin", "Union Filmtheater Neuruppin", "A0685", Some("/kino/neuruppin/union-filmtheater-31621"), "Neuruppin")
  ))
  private def p_neustadt_an_der_aisch: R = ("neustadt-an-der-aisch", "neustadt-an-der-aisch-bayern", "Neustadt an der Aisch", "Bayern", 49.5795, 10.6113, true, Seq("Neustadt an der Aisch", "Bad Windsheim"), Seq(
    ("Kino NEA", "Kino NEA", "A2657", Some("/kino/neustadt-an-der-aisch/kino-nea-34902"), "Neustadt an der Aisch"),
    ("Open Air im Freilandmuseum", "Open Air im Freilandmuseum", "A1828", Some("/kino/bad-windsheim/open-air-kino-im-fraenkischen-freilandmuseum-43701"), "Bad Windsheim")
  ))
  private def p_neustadt_an_der_weinstrasse: R = ("neustadt-an-der-weinstrasse", "neustadt-an-der-weinstrasse-rheinland-pfalz", "Neustadt an der Weinstraße", "Rheinland-Pfalz", 49.3501, 8.1389, false, Seq("Neustadt an der Weinstraße"), Seq(
    ("Cineplex Neustadt", "Cineplex Neustadt", "A2752", Some("/kino/neustadt-an-der-weinstrasse/cineplex-89157"), "Neustadt an der Weinstraße"),
    ("Open Air Kino in der Hetzel-Galerie", "Open Air Kino in der Hetzel-Galerie", "A2768", Some("/kino/neustadt-an-der-weinstrasse/open-air-kino-der-roxy-kinos-und-der-hetzel-galerie-63758"), "Neustadt an der Weinstraße"),
    ("Roxy Kinos", "Roxy Kinos", "A1085", Some("/kino/neustadt-an-der-weinstrasse/roxy-kinos-44479"), "Neustadt an der Weinstraße")
  ))
  private def p_neustadt_in_holstein: R = ("neustadt-in-holstein", "neustadt-in-holstein-schleswig-holstein", "Neustadt in Holstein", "Schleswig-Holstein", 54.1071, 10.8145, false, Seq("Neustadt in Holstein"), Seq(
    ("Kino-Center Kremper Tor", "Kino-Center Kremper Tor", "A0060", None, "Neustadt in Holstein")
  ))
  private def p_neustrelitz: R = ("neustrelitz", "neustrelitz-mecklenburg-vorpommern", "Neustrelitz", "Mecklenburg-Vorpommern", 53.3602, 13.0726, false, Seq("Neustrelitz"), Seq(
    ("Basiskulturfabrik", "Basiskulturfabrik", "A0084", None, "Neustrelitz"),
    ("Movie Star Neustrelitz", "Movie Star Neustrelitz", "A2333", Some("/kino/neustrelitz/movie-star-74697"), "Neustrelitz")
  ))
  private def p_neuwied: R = ("neuwied", "neuwied-rheinland-pfalz", "Neuwied", "Rheinland-Pfalz", 50.4336, 7.4706, false, Seq("Neuwied"), Seq(
    ("Metropol-Kino-Center", "Metropol-Kino-Center", "A1564", Some("/kino/neuwied/metropolkinocenter-33987"), "Neuwied"),
    ("Schauburg-Theater", "Schauburg-Theater", "A1124", Some("/kino/neuwied/schauburgtheater-31887"), "Neuwied")
  ))
  private def p_niebuell: R = ("niebuell", "niebuell-schleswig-holstein", "Niebüll", "Schleswig-Holstein", 54.7866, 8.8285, true, Seq("Niebüll", "Leck", "Wyk auf Föhr"), Seq(
    ("Deli-Kino", "Deli-Kino", "A2368", Some("/kino/leck/delikino-32522"), "Leck"),
    ("Eck&#039; s Kino", "Eck&#039; s Kino", "A1697", None, "Niebüll"),
    ("Filmtheater am Sandwall", "Filmtheater am Sandwall", "G011C", Some("/kino/wyk-auf-foehr/filmtheater-am-sandwall-32650"), "Wyk auf Föhr")
  ))
  private def p_nienburg: R = ("nienburg", "nienburg-niedersachsen", "Nienburg", "Niedersachsen", 52.6444, 9.2166, true, Seq("Nienburg", "Sulingen", "Hoya"), Seq(
    ("Filmhof Hoya", "Filmhof Hoya", "A0500", Some("/kino/hoya-weser/filmhof-hoya-32496"), "Hoya"),
    ("Filmpalast Nienburg", "Filmpalast Nienburg", "A1468", Some("/kino/nienburg-weser/filmpalast-am-hafen-58736"), "Nienburg"),
    ("Filmpalast Sulingen", "Filmpalast Sulingen", "A1423", Some("/kino/sulingen/filmpalast-32620"), "Sulingen")
  ))
  private def p_noerdlingen: R = ("noerdlingen", "noerdlingen-bayern", "Nördlingen", "Bayern", 48.8512, 10.4887, false, Seq("Nördlingen"), Seq(
    ("Movieworld Kino Nördlingen", "Movieworld Kino Nördlingen", "A2630", Some("/kino/noerdlingen/movieworld-77678"), "Nördlingen"),
    ("Open Air Kino Ochsenzwinger Nördlingen", "Open Air Kino Ochsenzwinger Nördlingen", "A2484", None, "Nördlingen"),
    ("Ries Theater", "Ries Theater", "A1078", Some("/kino/noerdlingen/ries-theater-32904"), "Nördlingen")
  ))
  private def p_norddorf_auf_amrum: R = ("norddorf-auf-amrum", "norddorf-auf-amrum-schleswig-holstein", "Norddorf auf Amrum", "Schleswig-Holstein", 54.6795, 8.3341, false, Seq("Norddorf auf Amrum"), Seq(
    ("LichtBlick InselKino", "LichtBlick InselKino", "A2807", None, "Norddorf auf Amrum")
  ))
  private def p_norderney: R = ("norderney", "norderney-niedersachsen", "Norderney", "Niedersachsen", 53.708, 7.1572, true, Seq("Norderney", "Langeoog", "Spiekeroog"), Seq(
    ("Inselkino Spiekeroog", "Inselkino Spiekeroog", "A2884", Some("/kino/spiekeroog/inselkino-32615"), "Spiekeroog"),
    ("Windlicht", "Windlicht", "A2757", Some("/kino/langeoog/kino-windlicht-32518"), "Langeoog"),
    ("Kino im Kurtheater", "Kino im Kurtheater", "cineamo:1240", None, "Norderney")
  ))
  private def p_nordhausen: R = ("nordhausen", "nordhausen-thueringen", "Nordhausen", "Thüringen", 51.5018, 10.7957, true, Seq("Nordhausen", "Sondershausen", "Bleicherode"), Seq(
    ("Cinema 64", "Cinema 64", "A2367", Some("/kino/sondershausen-thueringen/cinema-64-31672"), "Sondershausen"),
    ("Filmpalast Nordhausen", "Filmpalast Nordhausen", "A0517", Some("/kino/nordhausen/filmpalast-31624"), "Nordhausen"),
    ("Filmtheater Bleicherode", "Filmtheater Bleicherode", "A0534", Some("/kino/bleicherode/filmtheater-35157"), "Bleicherode")
  ))
  private def p_nordhorn: R = ("nordhorn", "nordhorn-niedersachsen", "Nordhorn", "Niedersachsen", 52.4308, 7.0683, false, Seq("Nordhorn"), Seq(
    ("UCI LUXE Nordhorn", "UCI LUXE Nordhorn", "A2895", None, "Nordhorn")
  ))
  private def p_northeim: R = ("northeim", "northeim-niedersachsen", "Northeim", "Niedersachsen", 51.7066, 10.0, true, Seq("Northeim", "Bad Gandersheim"), Seq(
    ("Kino Gandeon", "Kino Gandeon", "A2707", Some("/kino/bad-gandersheim/kino-gandeon-87077"), "Bad Gandersheim"),
    ("Neue Schauburg Northeim", "Neue Schauburg Northeim", "A0984", Some("/kino/northeim/neue-schauburg-32568"), "Northeim")
  ))
  private def p_nuertingen: R = ("nuertingen", "nuertingen-baden-wuerttemberg", "Nürtingen", "Baden-Württemberg", 48.6257, 9.342, true, Seq("Nürtingen", "Metzingen", "Ensingen", "Grafenberg"), Seq(
    ("Kinomobil Stuttgart - BIO-Café Zimt und Zunder", "Kinomobil Stuttgart - BIO-Café Zimt und Zunder", "A0814", None, "Ensingen"),
    ("Kinomobil Stuttgart - Historische Kelter", "Kinomobil Stuttgart - Historische Kelter", "A2134", None, "Grafenberg"),
    ("Luna-Filmtheater", "Luna-Filmtheater", "A1549", Some("/kino/metzingen/lunafilmtheater-32849"), "Metzingen"),
    ("Traumpalast Nürtingen", "Traumpalast Nürtingen", "A1511", Some("/kino/nuertingen/traumpalast-nuertingen-32917"), "Nürtingen")
  ))
  private def p_oberndorf_am_neckar: R = ("oberndorf-am-neckar", "oberndorf-am-neckar-baden-wuerttemberg", "Oberndorf am Neckar", "Baden-Württemberg", 48.2906, 8.5722, false, Seq("Oberndorf am Neckar"), Seq(
    ("KKK-Filmtheater", "KKK-Filmtheater", "A0848", None, "Oberndorf am Neckar"),
    ("Sommernachtskino im Klosterhof in Oberndorf", "Sommernachtskino im Klosterhof in Oberndorf", "A1987", None, "Oberndorf am Neckar")
  ))
  private def p_oberstdorf: R = ("oberstdorf", "oberstdorf-bayern", "Oberstdorf", "Bayern", 47.4072, 10.2794, false, Seq("Oberstdorf"), Seq(
    ("Kurfilmtheater Oberstdorf", "Kurfilmtheater Oberstdorf", "A0883", Some("/kino/oberstdorf/kurfilmtheater-66638"), "Oberstdorf"),
    ("LOFT Oberstdorf", "LOFT Oberstdorf", "A0222", Some("/kino/oberstdorf/loft-65776"), "Oberstdorf"),
    ("Open Air Kino zwischen den Schanzen/Erdinger Arena", "Open Air Kino zwischen den Schanzen/Erdinger Arena", "A2551", Some("/kino/oberstdorf/open-air-kino-zwischen-den-schanzenerdinger-arena-76344"), "Oberstdorf")
  ))
  private def p_oberviechtach: R = ("oberviechtach", "oberviechtach-bayern", "Oberviechtach", "Bayern", 49.4581, 12.4167, false, Seq("Oberviechtach"), Seq(
    ("Kino in OVI", "Kino in OVI", "A1538", Some("/kino/oberviechtach/kino-in-ovi-32921"), "Oberviechtach")
  ))
  private def p_oehringen: R = ("oehringen", "oehringen-baden-wuerttemberg", "Öhringen", "Baden-Württemberg", 49.1988, 9.5072, true, Seq("Öhringen", "Künzelsau", "Forchtenberg"), Seq(
    ("Open-Air-Kino Forchtenberg", "Open-Air-Kino Forchtenberg", "A2440", None, "Forchtenberg"),
    ("Prestige", "Prestige", "A1040", Some("/kino/kuenzelsau/prestige-53416"), "Künzelsau"),
    ("Scala Filmtheater Öhringen", "Scala Filmtheater Öhringen", "A0837", Some("/kino/oehringen/scala-filmtheater-32212"), "Öhringen")
  ))
  private def p_offenburg: R = ("offenburg", "offenburg-baden-wuerttemberg", "Offenburg", "Baden-Württemberg", 48.4738, 7.945, false, Seq("Offenburg"), Seq(
    ("FORUM Offenburg", "FORUM Offenburg", "A2334", Some("/kino/offenburg/forum-74717"), "Offenburg"),
    ("Kommunales Kino im KiK", "Kommunales Kino im KiK", "A2854", None, "Offenburg"),
    ("Sommer Kino Nächte Ortenau", "Sommer Kino Nächte Ortenau", "A2002", None, "Offenburg")
  ))
  private def p_oldenburg_in_holstein: R = ("oldenburg-in-holstein", "oldenburg-in-holstein-schleswig-holstein", "Oldenburg in Holstein", "Schleswig-Holstein", 54.295, 10.8904, false, Seq("Oldenburg in Holstein"), Seq(
    ("Lichtblick Filmtheater", "Lichtblick Filmtheater", "A0899", Some("/kino/oldenburg-in-holstein/lichtblick-filmtheater-65499"), "Oldenburg in Holstein")
  ))
  private def p_oranienburg: R = ("oranienburg", "oranienburg-brandenburg", "Oranienburg", "Brandenburg", 52.748, 13.2519, true, Seq("Oranienburg", "Falkensee", "Kleinmachnow"), Seq(
    ("ALA Kino am Falkensee", "ALA Kino am Falkensee", "A0013", None, "Falkensee"),
    ("Filmpalast Oranienburg", "Filmpalast Oranienburg", "A0518", Some("/kino/oranienburg/filmpalast-31627"), "Oranienburg"),
    ("Kammerspiele Kleinmachnow", "Kammerspiele Kleinmachnow", "A0642", Some("/kino/kleinmachnow/neue-kammerspiele-31570"), "Kleinmachnow")
  ))
  private def p_osterholz_scharmbeck: R = ("osterholz-scharmbeck", "osterholz-scharmbeck-niedersachsen", "Osterholz-Scharmbeck", "Niedersachsen", 53.2266, 8.7923, true, Seq("Osterholz-Scharmbeck", "Brake", "Ritterhude"), Seq(
    ("Central Theater Osterholz-Scharmbeck", "Central Theater Osterholz-Scharmbeck", "A1348", Some("/kino/osterholzscharmbeck/central-theater-32583"), "Osterholz-Scharmbeck"),
    ("Oscar Kulturspielhaus", "Oscar Kulturspielhaus", "A2746", Some("/kino/osterholzscharmbeck/kino-im-oscar-kulturspielhaus-88877"), "Osterholz-Scharmbeck"),
    ("Centraltheater Brake", "Centraltheater Brake", "A2694", Some("/kino/brake/centraltheater-brake-86458"), "Brake"),
    ("Ritterhuder-Lichtspiele", "Ritterhuder-Lichtspiele", "A1605", Some("/kino/ritterhude/ritterhuder-lichtspiele-32599"), "Ritterhude")
  ))
  private def p_ottobrunn: R = ("ottobrunn", "ottobrunn-bayern", "Ottobrunn", "Bayern", 48.0649, 11.6633, true, Seq("Ottobrunn", "Haar"), Seq(
    ("Filmstudio Ottobrunn", "Filmstudio Ottobrunn", "A0528", Some("/kino/ottobrunn/filmstudio-32927"), "Ottobrunn"),
    ("Ottobrunner Kinos", "Ottobrunner Kinos", "A0969", Some("/kino/ottobrunn/ottobrunner-kinos-32928"), "Ottobrunn"),
    ("Haarer Kinos", "Haarer Kinos", "A1669", Some("/kino/haar/haarer-kinos-70017"), "Haar")
  ))
  private def p_papenburg: R = ("papenburg", "papenburg-niedersachsen", "Papenburg", "Niedersachsen", 53.0777, 7.4152, true, Seq("Papenburg", "Leer"), Seq(
    ("Kino Leer (Ostfriesland)", "Kino Leer (Ostfriesland)", "A0675", None, "Leer"),
    ("Kino Papenburg", "Kino Papenburg", "A0466", Some("/kino/papenburg/kino-papenburg-57136"), "Papenburg")
  ))
  private def p_parchim: R = ("parchim", "parchim-mecklenburg-vorpommern", "Parchim", "Mecklenburg-Vorpommern", 53.4263, 11.8488, false, Seq("Parchim"), Seq(
    ("Movie Star Parchim", "Movie Star Parchim", "A0967", Some("/kino/parchim/movie-star-43517"), "Parchim")
  ))
  private def p_passau: R = ("passau", "passau-bayern", "Passau", "Bayern", 48.5665, 13.4312, false, Seq("Passau"), Seq(
    ("Cineplex Passau", "Cineplex Passau", "A0322", Some("/kino/passau/cineplex-69823"), "Passau"),
    ("ProLi Cinema", "ProLi Cinema", "G011E", None, "Passau"),
    ("ScharfrichterKino", "ScharfrichterKino", "A1622", Some("/kino/passau/scharfrichterkino-32933"), "Passau")
  ))
  private def p_pfaffenhofen_an_der_ilm: R = ("pfaffenhofen-an-der-ilm", "pfaffenhofen-an-der-ilm-bayern", "Pfaffenhofen an der Ilm", "Bayern", 48.5305, 11.505, true, Seq("Pfaffenhofen an der Ilm", "Wolnzach"), Seq(
    ("Amper-Lichtspiele", "Amper-Lichtspiele", "A1466", Some("/kino/wolnzach/amperlichtspiele-33069"), "Wolnzach"),
    ("Cineplex Pfaffenhofen", "Cineplex Pfaffenhofen", "A0325", Some("/kino/pfaffenhofen-an-der-ilm/cineplex-32938"), "Pfaffenhofen an der Ilm")
  ))
  private def p_pirmasens: R = ("pirmasens", "pirmasens-rheinland-pfalz", "Pirmasens", "Rheinland-Pfalz", 49.2015, 7.6053, true, Seq("Pirmasens", "Annweiler am Trifels", "Fischbach bei Dahn"), Seq(
    ("Kino Digital im Hohenstaufensaal", "Kino Digital im Hohenstaufensaal", "A2619", Some("/kino/annweiler-am-trifels/kino-im-hohenstaufensaal-32335"), "Annweiler am Trifels"),
    ("Walhalla Kinocenter", "Walhalla Kinocenter", "A1659", Some("/kino/pirmasens/walhalla-kinocenter-32220"), "Pirmasens"),
    ("Wasgau-Theater", "Wasgau-Theater", "A1269", None, "Fischbach bei Dahn")
  ))
  private def p_pirna: R = ("pirna", "pirna-sachsen", "Pirna", "Sachsen", 50.9584, 13.937, false, Seq("Pirna"), Seq(
    ("Filmpalast Pirna", "Filmpalast Pirna", "A0511", Some("/kino/pirna/filmpalast-31630"), "Pirna")
  ))
  private def p_plauen: R = ("plauen", "plauen-sachsen", "Plauen", "Sachsen", 50.4973, 12.1378, true, Seq("Plauen", "Greiz", "Oelsnitz"), Seq(
    ("Capitol-Kino Plauen", "Capitol-Kino Plauen", "A0156", Some("/kino/plauen-vogtland/capitolkino-31631"), "Plauen"),
    ("Malzhaus", "Malzhaus", "A2375", Some("/kino/plauen-vogtland/malzhaus-75077"), "Plauen"),
    ("Open-Air-Kino - Waldbühne Neuwürschnitz", "Open-Air-Kino - Waldbühne Neuwürschnitz", "A1846", None, "Oelsnitz"),
    ("UT99 Kinocenter", "UT99 Kinocenter", "A1250", None, "Greiz")
  ))
  private def p_plettenberg: R = ("plettenberg", "plettenberg-nordrhein-westfalen", "Plettenberg", "Nordrhein-Westfalen", 51.2095, 7.8726, true, Seq("Plettenberg", "Lennestadt", "Attendorn"), Seq(
    ("JAC Kino Attendorn", "JAC Kino Attendorn", "A2914", Some("/kino/attendorn/jac-kino-attendorn-91983"), "Attendorn"),
    ("Lichtspielhaus Lennestadt", "Lichtspielhaus Lennestadt", "A0912", Some("/kino/lennestadt/lichtspielhaus-31856"), "Lennestadt"),
    ("Weidenhof Kino", "Weidenhof Kino", "A1270", Some("/kino/plettenberg/weidenhof-kino-31895"), "Plettenberg")
  ))
  private def p_preetz: R = ("preetz", "preetz-schleswig-holstein", "Preetz", "Schleswig-Holstein", 54.2358, 10.2793, true, Seq("Preetz", "Plön", "Bordesholm"), Seq(
    ("Astra-Filmtheater", "Astra-Filmtheater", "A1300", Some("/kino/ploen-holstein/astrafilmtheater-32590"), "Plön"),
    ("Captiol Cine Center", "Captiol Cine Center", "A1747", Some("/kino/preetz-holstein/capitol-cine-center-32591"), "Preetz"),
    ("Savoy (Bordesholm)", "Savoy (Bordesholm)", "A1614", Some("/kino/bordesholm/savoy-32346"), "Bordesholm")
  ))
  private def p_prenzlau: R = ("prenzlau", "prenzlau-brandenburg", "Prenzlau", "Brandenburg", 53.317, 13.864, false, Seq("Prenzlau"), Seq(
    ("Union Filmtheater Prenzlau", "Union Filmtheater Prenzlau", "A0666", Some("/kino/prenzlau/union-filmtheater-prenzlau-64357"), "Prenzlau")
  ))
  private def p_prerow: R = ("prerow", "prerow-mecklenburg-vorpommern", "Prerow", "Mecklenburg-Vorpommern", 54.4447, 12.5768, false, Seq("Prerow"), Seq(
    ("Cinema Ostseebad Prerow", "Cinema Ostseebad Prerow", "A0241", None, "Prerow")
  ))
  private def p_prien_am_chiemsee: R = ("prien-am-chiemsee", "prien-am-chiemsee-bayern", "Prien am Chiemsee", "Bayern", 47.856, 12.3462, true, Seq("Prien am Chiemsee", "Bad Endorf"), Seq(
    ("Marias Kino", "Marias Kino", "A1555", Some("/kino/bad-endorf/marias-kino-32682"), "Bad Endorf"),
    ("Mikes Kino", "Mikes Kino", "A0961", Some("/kino/prien-am-chiemsee/mikes-kino-65516"), "Prien am Chiemsee")
  ))
  private def p_quickborn: R = ("quickborn", "quickborn-schleswig-holstein", "Quickborn", "Schleswig-Holstein", 53.7282, 9.9108, true, Seq("Quickborn", "Norderstedt"), Seq(
    ("Beluga Kino", "Beluga Kino", "A0088", Some("/kino/quickborn/beluga-kino-61337"), "Quickborn"),
    ("Openair Kino", "Openair Kino", "A2182", Some("/kino/quickborn/openair-kino-73586"), "Quickborn"),
    ("Spectrum Kino Norderstedt", "Spectrum Kino Norderstedt", "A1142", Some("/kino/norderstedt/spectrum-34888"), "Norderstedt")
  ))
  private def p_rastatt: R = ("rastatt", "rastatt-baden-wuerttemberg", "Rastatt", "Baden-Württemberg", 48.8585, 8.2096, true, Seq("Rastatt", "Ettlingen", "Rheinmünster"), Seq(
    ("FORUM Rastatt", "FORUM Rastatt", "A2731", Some("/kino/rastatt/forum-87757"), "Rastatt"),
    ("Kinomobil Stuttgart - Festhalle Rheinmünster-Schwarzach", "Kinomobil Stuttgart - Festhalle Rheinmünster-Schwarzach", "A0802", Some("/kino/rheinmuenster/kinomobil-stuttgart-festhalle-schwarzach-41837"), "Rheinmünster"),
    ("Kulisse Ettlingen", "Kulisse Ettlingen", "A0876", Some("/kino/ettlingen/kulisse-48517"), "Ettlingen")
  ))
  private def p_rathenow: R = ("rathenow", "rathenow-brandenburg", "Rathenow", "Brandenburg", 52.6066, 12.337, false, Seq("Rathenow"), Seq(
    ("Haveltor Kino", "Haveltor Kino", "A0603", Some("/kino/rathenow/haveltorkino-rathenow-55136"), "Rathenow")
  ))
  private def p_ratingen: R = ("ratingen", "ratingen-nordrhein-westfalen", "Ratingen", "Nordrhein-Westfalen", 51.2972, 6.8493, true, Seq("Ratingen", "Velbert", "Meerbusch - Lank", "Kaarst", "Mettmann"), Seq(
    ("Kino am Forum Wasserturm", "Kino am Forum Wasserturm", "A2883", Some("/kino/meerbusch/kino-wasserturm-31860"), "Meerbusch - Lank"),
    ("Kino Kaarst", "Kino Kaarst", "A0742", Some("/kino/kaarst/kino-kaarst-36522"), "Kaarst"),
    ("Studiokino Ratingen", "Studiokino Ratingen", "A2804", Some("/kino/ratingen/studiokino-33769"), "Ratingen"),
    ("Weltspiegel-Kino-Center", "Weltspiegel-Kino-Center", "A1274", Some("/kino/mettmann/weltspiegelkinocenter-31862"), "Mettmann"),
    ("Forum Velbert", "Forum Velbert", "kinoheld:3581", None, "Velbert")
  ))
  private def p_ravensburg: R = ("ravensburg", "ravensburg-baden-wuerttemberg", "Ravensburg", "Baden-Württemberg", 47.782, 9.6106, true, Seq("Ravensburg", "Weingarten"), Seq(
    ("CineParC Ravensburg", "CineParC Ravensburg", "A1393", None, "Ravensburg"),
    ("Kinozentrum Frauentor", "Kinozentrum Frauentor", "A0845", None, "Ravensburg"),
    ("Kulturzentrum Linse", "Kulturzentrum Linse", "A1839", Some("/kino/weingarten/kulturzentrum-linse-e.-v.-72297"), "Weingarten"),
    ("Open Air Kino im Schlösse Hof", "Open Air Kino im Schlösse Hof", "A2496", Some("/kino/weingarten/open-air-kino-im-schloessle-hof-76023"), "Weingarten")
  ))
  private def p_rendsburg: R = ("rendsburg", "rendsburg-schleswig-holstein", "Rendsburg", "Schleswig-Holstein", 54.3018, 9.6717, true, Seq("Rendsburg", "Schleswig", "Eckernförde", "Kappeln"), Seq(
    ("Capitol Theater Kappeln", "Capitol Theater Kappeln", "A1664", None, "Kappeln"),
    ("Das Haus", "Das Haus", "A0866", None, "Eckernförde"),
    ("Filmtheater Capitol Schleswig", "Filmtheater Capitol Schleswig", "A1126", Some("/kino/schleswig/capitol-filmpalast-53557"), "Schleswig"),
    ("Schauburg Rendsburg", "Schauburg Rendsburg", "A1623", Some("/kino/rendsburg/schauburg-33966"), "Rendsburg")
  ))
  private def p_rheine: R = ("rheine", "rheine-nordrhein-westfalen", "Rheine", "Nordrhein-Westfalen", 52.2851, 7.4405, false, Seq("Rheine"), Seq(
    ("Cinetech das Erlebniskino Rheine", "Cinetech das Erlebniskino Rheine", "A0383", Some("/kino/rheine/cinetech-31901"), "Rheine"),
    ("Zinema City", "Zinema City", "A0398", Some("/kino/rheine/zinema-city-rheine-49156"), "Rheine")
  ))
  private def p_rheinfelden: R = ("rheinfelden", "rheinfelden-baden-wuerttemberg", "Rheinfelden", "Baden-Württemberg", 47.5601, 7.7871, true, Seq("Rheinfelden", "Schopfheim", "Bad Säckingen", "Kandern"), Seq(
    ("Gloria Bad Säckingen", "Gloria Bad Säckingen", "A1438", None, "Bad Säckingen"),
    ("Kino Kandern", "Kino Kandern", "A0014", None, "Kandern"),
    ("Rheinflimmern", "Rheinflimmern", "A0032", Some("/kino/rheinfelden/rheinflimmern-75376"), "Rheinfelden"),
    ("Scala Schopfheim", "Scala Schopfheim", "A1107", Some("/kino/schopfheim/scala-kino-32251"), "Schopfheim")
  ))
  private def p_riedlingen: R = ("riedlingen", "riedlingen-baden-wuerttemberg", "Riedlingen", "Baden-Württemberg", 48.1546, 9.4756, true, Seq("Riedlingen", "Mengen"), Seq(
    ("Kinocenter Mengen", "Kinocenter Mengen", "A1502", Some("/kino/mengen/kinocenter-32847"), "Mengen"),
    ("Lichtspielhaus Riedlingen", "Lichtspielhaus Riedlingen", "A1148", Some("/kino/riedlingen/lichtspielhaus-87299"), "Riedlingen")
  ))
  private def p_riesa: R = ("riesa", "riesa-sachsen", "Riesa", "Sachsen", 51.3078, 13.2917, true, Seq("Riesa", "Grossenhain", "Gröditz"), Seq(
    ("Castello", "Castello", "A0182", Some("/kino/groeditz-bei-riesa/castello-65477"), "Gröditz"),
    ("Filmgalerie Großenhain", "Filmgalerie Großenhain", "A0496", Some("/kino/grossenhain-sachsen/filmgalerie-39677"), "Grossenhain"),
    ("Filmpalast Capital Riesa", "Filmpalast Capital Riesa", "A0515", Some("/kino/riesa/filmpalast-capitol-31648"), "Riesa")
  ))
  private def p_rietschen: R = ("rietschen", "rietschen-sachsen", "Rietschen", "Sachsen", 51.4, 14.7833, false, Seq("Rietschen"), Seq(
    ("Kino-Cafe", "Kino-Cafe", "A0759", Some("/kino/rietschen/kinocaf%C3%A9-rietschen-e.v.-31649"), "Rietschen")
  ))
  private def p_rinteln: R = ("rinteln", "rinteln-niedersachsen", "Rinteln", "Niedersachsen", 52.186, 9.0792, false, Seq("Rinteln"), Seq(
    ("Kinocenter Rinteln", "Kinocenter Rinteln", "A1503", Some("/kino/rinteln/kinocenter-32598"), "Rinteln"),
    ("Metropol-Theater Rinteln", "Metropol-Theater Rinteln", "A1566", None, "Rinteln")
  ))
  private def p_rodgau: R = ("rodgau", "rodgau-hessen", "Rodgau", "Hessen", 50.0263, 8.8859, true, Seq("Rodgau", "Dietzenbach", "Seligenstadt", "Rödermark"), Seq(
    ("Kronen-Lichtspiele Rodgau", "Kronen-Lichtspiele Rodgau", "A0256", Some("/kino/rodgau/kronenlichtspiele-39857"), "Rodgau"),
    ("Saalbau-Lichtspiele Rodgau", "Saalbau-Lichtspiele Rodgau", "A1099", Some("/kino/rodgau/saalbaulichtspiele-32228"), "Rodgau"),
    ("Main Kino D", "Main Kino D", "A0658", None, "Dietzenbach"),
    ("Maingau - Open - Air - Kino", "Maingau - Open - Air - Kino", "A2627", None, "Dietzenbach"),
    ("Neue Lichtspiele", "Neue Lichtspiele", "A1571", Some("/kino/roedermark/neue-lichtspiele-32227"), "Rödermark"),
    ("Turmpalast", "Turmpalast", "A1650", Some("/kino/seligenstadt/turmpalast-32261"), "Seligenstadt")
  ))
  private def p_rothenburg: R = ("rothenburg", "rothenburg-bayern", "Rothenburg", "Bayern", 49.3788, 10.1871, true, Seq("Rothenburg", "Uffenheim", "Schrozberg", "Burgbernheim"), Seq(
    ("Filmpalast im Forum", "Filmpalast im Forum", "A2646", Some("/kino/rothenburg-ob-der-tauber/forum-79938"), "Rothenburg"),
    ("Open Air Kino&quot;Am Kappelenberg&quot;", "Open Air Kino&quot;Am Kappelenberg&quot;", "A2058", Some("/kino/burgbernheim/open-air-kino-am-kappelenberg-68517"), "Burgbernheim"),
    ("Open Air Rudolzhofen Kino", "Open Air Rudolzhofen Kino", "A2560", Some("/kino/uffenheim/open-air-kino-rudolzhofen-76399"), "Uffenheim"),
    ("Open Air Schrozberg", "Open Air Schrozberg", "A2701", None, "Schrozberg")
  ))
  private def p_rottenburg_am_neckar: R = ("rottenburg-am-neckar", "rottenburg-am-neckar-baden-wuerttemberg", "Rottenburg am Neckar", "Baden-Württemberg", 48.4763, 8.9353, true, Seq("Rottenburg am Neckar", "Mössingen"), Seq(
    ("Kino im Waldhorn", "Kino im Waldhorn", "A0732", Some("/kino/rottenburg-am-neckar/kino-im-waldhorn-32968"), "Rottenburg am Neckar"),
    ("Lichtspiele Mössingen", "Lichtspiele Mössingen", "A1536", Some("/kino/moessingen/lichtspiele-32853"), "Mössingen")
  ))
  private def p_rottweil: R = ("rottweil", "rottweil-baden-wuerttemberg", "Rottweil", "Baden-Württemberg", 48.1678, 8.6272, true, Seq("Rottweil", "Schramberg"), Seq(
    ("Central-Kino Rottweil", "Central-Kino Rottweil", "A0199", Some("/kino/rottweil/centralkino-32970"), "Rottweil"),
    ("Subiaco Schramberg", "Subiaco Schramberg", "A1169", Some("/kino/schramberg/subiaco-schramberg-54037"), "Schramberg")
  ))
  private def p_rudolstadt: R = ("rudolstadt", "rudolstadt-thueringen", "Rudolstadt", "Thüringen", 50.7204, 11.3405, false, Seq("Rudolstadt"), Seq(
    ("Cineplex Rudolstadt", "Cineplex Rudolstadt", "A0307", Some("/kino/rudolstadt/cineplex-42817"), "Rudolstadt"),
    ("Freiluftkino im Garten des Schillerhauses", "Freiluftkino im Garten des Schillerhauses", "A2443", None, "Rudolstadt"),
    ("Uferpalast Rudolstadt", "Uferpalast Rudolstadt", "A2390", None, "Rudolstadt")
  ))
  private def p_saalfeld: R = ("saalfeld", "saalfeld-thueringen", "Saalfeld", "Thüringen", 50.6483, 11.3654, false, Seq("Saalfeld"), Seq(
    ("Cineplex Saalfeld", "Cineplex Saalfeld", "A0157", Some("/kino/saalfeld-saale/cineplex-57836"), "Saalfeld")
  ))
  private def p_salzwedel: R = ("salzwedel", "salzwedel-sachsen-anhalt", "Salzwedel", "Sachsen-Anhalt", 52.853, 11.1529, true, Seq("Salzwedel", "Lüchow", "Jameln"), Seq(
    ("Cafe Grenzbereiche", "Cafe Grenzbereiche", "A0133", Some("/kino/jameln/kulturverein-platenlaase-caf%C3%A9-grenzbereiche-65483"), "Jameln"),
    ("Filmpalast Salzwedel (Hansestadt)", "Filmpalast Salzwedel (Hansestadt)", "A1347", None, "Salzwedel"),
    ("Kino Alte Brennerei", "Kino Alte Brennerei", "A0699", Some("/kino/luechow-bei-sandesneben/kino-alte-brennerei-32538"), "Lüchow")
  ))
  private def p_sangerhausen: R = ("sangerhausen", "sangerhausen-sachsen-anhalt", "Sangerhausen", "Sachsen-Anhalt", 51.4722, 11.2953, false, Seq("Sangerhausen"), Seq(
    ("Movie Star Sangerhausen", "Movie Star Sangerhausen", "A0188", Some("/kino/sangerhausen/movie-star-35336"), "Sangerhausen")
  ))
  private def p_schleiz: R = ("schleiz", "schleiz-thueringen", "Schleiz", "Thüringen", 50.5787, 11.8102, true, Seq("Schleiz", "Bad Lobenstein", "Wurzbach"), Seq(
    ("Kino am Park", "Kino am Park", "A2656", Some("/kino/bad-lobenstein/kino-am-park-31590"), "Bad Lobenstein"),
    ("Kino Wurzbach", "Kino Wurzbach", "A0758", Some("/kino/wurzbach/kino-wurzbach-31709"), "Wurzbach"),
    ("Neues Kino im Hörsaal", "Neues Kino im Hörsaal", "A0989", Some("/kino/schleiz/neues-kino-im-hoersaal-61457"), "Schleiz")
  ))
  private def p_schloss_holte_stukenbrock: R = ("schloss-holte-stukenbrock", "schloss-holte-stukenbrock-nordrhein-westfalen", "Schloss Holte-Stukenbrock", "Nordrhein-Westfalen", 51.9, 8.6167, true, Seq("Schloss Holte-Stukenbrock", "Bad Lippspringe"), Seq(
    ("Odins Filmtheater", "Odins Filmtheater", "A1008", Some("/kino/bad-lippspringe/odins-filmtheater-65557"), "Bad Lippspringe"),
    ("Rhythmus-Filmtheater", "Rhythmus-Filmtheater", "A2365", None, "Schloss Holte-Stukenbrock")
  ))
  private def p_schongau: R = ("schongau", "schongau-bayern", "Schongau", "Bayern", 47.8124, 10.8966, true, Seq("Schongau", "Murnau am Staffelsee", "Kochel am See"), Seq(
    ("Filmstudio Kochel", "Filmstudio Kochel", "A0532", None, "Kochel am See"),
    ("Kino im Griesbräu", "Kino im Griesbräu", "A0593", None, "Murnau am Staffelsee"),
    ("Lagerhauskino", "Lagerhauskino", "A0892", Some("/kino/schongau/lagerhauskino-32975"), "Schongau")
  ))
  private def p_schorndorf: R = ("schorndorf", "schorndorf-baden-wuerttemberg", "Schorndorf", "Baden-Württemberg", 48.8054, 9.5272, true, Seq("Schorndorf", "Rudersberg"), Seq(
    ("Club Manufaktur", "Club Manufaktur", "A0849", Some("/kino/schorndorf/club-manufaktur-kino-kleine-fluchten-32976"), "Schorndorf"),
    ("Traumpalast Schorndorf", "Traumpalast Schorndorf", "A0067", Some("/kino/schorndorf/traumpalast-schorndorf-66137"), "Schorndorf"),
    ("Löwenlichtspiele Rudersberg", "Löwenlichtspiele Rudersberg", "A0674", Some("/kino/rudersberg/loewenlichtspiele-77336"), "Rudersberg")
  ))
  private def p_schwabach: R = ("schwabach", "schwabach-bayern", "Schwabach", "Bayern", 49.3305, 11.0235, true, Seq("Schwabach", "Freystadt", "Grosshabersdorf"), Seq(
    ("Kino im Cafe Restaurant Beck", "Kino im Cafe Restaurant Beck", "A1485", None, "Freystadt"),
    ("Lichtspiele Großhabersdorf", "Lichtspiele Großhabersdorf", "A1365", Some("/kino/grosshabersdorf/lichtspiele-32781"), "Grosshabersdorf"),
    ("Luna Theater", "Luna Theater", "A0929", Some("/kino/schwabach/luna-theater-65197"), "Schwabach")
  ))
  private def p_schwaebisch_gmuend: R = ("schwaebisch-gmuend", "schwaebisch-gmuend-baden-wuerttemberg", "Schwäbisch Gmünd", "Baden-Württemberg", 48.7995, 9.7981, false, Seq("Schwäbisch Gmünd"), Seq(
    ("Brazil", "Brazil", "A0105", Some("/kino/schwaebisch-gmuend/brazil-32981"), "Schwäbisch Gmünd"),
    ("Traumpalast Schwäbisch Gmünd", "Traumpalast Schwäbisch Gmünd", "A1193", Some("/kino/schwaebisch-gmuend/traumpalast-50596"), "Schwäbisch Gmünd")
  ))
  private def p_schwaebisch_hall: R = ("schwaebisch-hall", "schwaebisch-hall-baden-wuerttemberg", "Schwäbisch Hall", "Baden-Württemberg", 49.1113, 9.7391, false, Seq("Schwäbisch Hall"), Seq(
    ("Kino im Schafstall", "Kino im Schafstall", "A1497", Some("/kino/schwaebisch-hall/kino-im-schafstall-32984"), "Schwäbisch Hall"),
    ("Lichtspielhaus Schwäbisch Hall", "Lichtspielhaus Schwäbisch Hall", "A0585", None, "Schwäbisch Hall"),
    ("Kinostar Schwäbisch Hall – GLORIA", "Kinostar Schwäbisch Hall – GLORIA", "cineamo:773", None, "Schwäbisch Hall")
  ))
  private def p_schwalmstadt: R = ("schwalmstadt", "schwalmstadt-hessen", "Schwalmstadt", "Hessen", 50.9333, 9.2167, true, Seq("Schwalmstadt", "Alsfeld", "Lauterbach"), Seq(
    ("Burgtheater Treysa", "Burgtheater Treysa", "A0123", None, "Schwalmstadt"),
    ("Kinocenter Alsfeld", "Kinocenter Alsfeld", "A1505", Some("/kino/alsfeld/kinocenter-31946"), "Alsfeld"),
    ("Lichtspielhaus Lauterbach (Hessen)", "Lichtspielhaus Lauterbach (Hessen)", "A0911", Some("/kino/lauterbach/lichtspielhaus-32145"), "Lauterbach")
  ))
  private def p_schwedt: R = ("schwedt", "schwedt-brandenburg", "Schwedt", "Brandenburg", 53.0596, 14.2815, false, Seq("Schwedt"), Seq(
    ("FilmforUM Schwedt", "FilmforUM Schwedt", "A0333", Some("/kino/schwedt-oder/filmforum-31665"), "Schwedt")
  ))
  private def p_schweinfurt: R = ("schweinfurt", "schweinfurt-bayern", "Schweinfurt", "Bayern", 50.0494, 10.2218, false, Seq("Schweinfurt"), Seq(
    ("Filmwelt Schweinfurt", "Filmwelt Schweinfurt", "A2475", Some("/kino/schweinfurt/filmwelt-75938"), "Schweinfurt"),
    ("KuK Kino und Kneipe", "KuK Kino und Kneipe", "A0874", Some("/kino/schweinfurt/kuk-kino-und-kneipe-54356"), "Schweinfurt"),
    ("Open Air am KuK", "Open Air am KuK", "A2532", None, "Schweinfurt"),
    ("Weltbio Kinocenter", "Weltbio Kinocenter", "A1272", Some("/kino/schweinfurt/weltbio-kinocenter-32259"), "Schweinfurt")
  ))
  private def p_schwerin: R = ("schwerin", "schwerin-mecklenburg-vorpommern", "Schwerin", "Mecklenburg-Vorpommern", 53.6294, 11.4132, false, Seq("Schwerin"), Seq(
    ("Capitol Kino Schwerin", "Capitol Kino Schwerin", "A0149", Some("/kino/schwerin-bei-koenigs-wusterhausen/filmpalast-capitol-31667"), "Schwerin"),
    ("Kino unterm Dach", "Kino unterm Dach", "A0559", Some("/kino/schwerin-bei-koenigs-wusterhausen/kino-unterm-dach-e.v.-93102"), "Schwerin"),
    ("Multiplex Mega Movies", "Multiplex Mega Movies", "A0977", Some("/kino/schwerin-bei-koenigs-wusterhausen/mega-movies-31668"), "Schwerin"),
    ("Open-Air-Kino Schwerin", "Open-Air-Kino Schwerin", "A2190", None, "Schwerin")
  ))
  private def p_senftenberg: R = ("senftenberg", "senftenberg-brandenburg", "Senftenberg", "Brandenburg", 51.5252, 14.0016, true, Seq("Senftenberg", "Finsterwalde", "Schwarzheide"), Seq(
    ("Extra Kinowelt", "Extra Kinowelt", "A0477", Some("/kino/schwarzheide/extrakinowelt-im-freizeitpark-wandelhof-65537"), "Schwarzheide"),
    ("Kino am See", "Kino am See", "A1887", None, "Senftenberg"),
    ("Weltspiegel Finsterwalde", "Weltspiegel Finsterwalde", "A1273", Some("/kino/finsterwalde/weltspiegel-31520"), "Finsterwalde")
  ))
  private def p_siegburg: R = ("siegburg", "siegburg-nordrhein-westfalen", "Siegburg", "Nordrhein-Westfalen", 50.8002, 7.2077, true, Seq("Siegburg", "Troisdorf", "Sankt Augustin", "Hennef"), Seq(
    ("Capitol Siegburg", "Capitol Siegburg", "A0147", Some("/kino/siegburg/kinocenter-capitol-31911"), "Siegburg"),
    ("Cineplex Siegburg", "Cineplex Siegburg", "A0228", Some("/kino/siegburg/cineplex-39605"), "Siegburg"),
    ("Cineplex Troisdorf", "Cineplex Troisdorf", "A1471", Some("/kino/troisdorf/cineplex-31927"), "Troisdorf"),
    ("Kur-Theater Hennef", "Kur-Theater Hennef", "A1480", Some("/kino/hennef-sieg/kurtheater-31816"), "Hennef"),
    ("Studio", "Studio", "A1161", None, "Sankt Augustin")
  ))
  private def p_simbach_am_inn: R = ("simbach-am-inn", "simbach-am-inn-bayern", "Simbach am Inn", "Bayern", 48.2655, 13.0231, false, Seq("Simbach am Inn"), Seq(
    ("Bavaria-Kino-Center Simbach", "Bavaria-Kino-Center Simbach", "A1314", Some("/kino/simbach-am-inn/bavariakinocenter-32993"), "Simbach am Inn")
  ))
  private def p_sindelfingen: R = ("sindelfingen", "sindelfingen-baden-wuerttemberg", "Sindelfingen", "Baden-Württemberg", 48.7, 9.0167, true, Seq("Sindelfingen", "Leonberg", "Waldenbuch"), Seq(
    ("CinemaxX Sindelfingen", "CinemaxX Sindelfingen", "A2875", Some("/kino/sindelfingen/cinemaxx-sindelfingen-38488"), "Sindelfingen"),
    ("Kinomobil Stuttgart - Haus der Begegnung Waldenbuch", "Kinomobil Stuttgart - Haus der Begegnung Waldenbuch", "A2171", None, "Waldenbuch"),
    ("Traumpalast Leonberg", "Traumpalast Leonberg", "A2738", Some("/kino/leonberg/traumpalast-leonberg-88177"), "Leonberg")
  ))
  private def p_singen_am_hohentwiel: R = ("singen-am-hohentwiel", "singen-am-hohentwiel-baden-wuerttemberg", "Singen am Hohentwiel", "Baden-Württemberg", 47.7593, 8.8403, true, Seq("Singen am Hohentwiel", "Radolfzell am Bodensee", "Gottmadingen", "Reichenau"), Seq(
    ("Cineplex Singen", "Cineplex Singen", "A0323", Some("/kino/singen/cineplex-42605"), "Singen am Hohentwiel"),
    ("Kino in der Gems", "Kino in der Gems", "A0570", None, "Singen am Hohentwiel"),
    ("Kinomobil Stuttgart - Hochwart-Wiese", "Kinomobil Stuttgart - Hochwart-Wiese", "A1953", None, "Reichenau"),
    ("Kinomobil Stuttgart - Neues Rathaus", "Kinomobil Stuttgart - Neues Rathaus", "A1939", None, "Gottmadingen"),
    ("Universum-Nostalgiekino", "Universum-Nostalgiekino", "A2916", None, "Radolfzell am Bodensee")
  ))
  private def p_sinsheim: R = ("sinsheim", "sinsheim-baden-wuerttemberg", "Sinsheim", "Baden-Württemberg", 49.2529, 8.8787, false, Seq("Sinsheim"), Seq(
    ("Citydome Sinsheim", "Citydome Sinsheim", "A0402", Some("/kino/sinsheim/citydome-39039"), "Sinsheim"),
    ("IMAX 3D Laser 4k Kino Sinsheim", "IMAX 3D Laser 4k Kino Sinsheim", "A0622", Some("/kino/sinsheim/imax-3d-32267"), "Sinsheim")
  ))
  private def p_soest: R = ("soest", "soest-nordrhein-westfalen", "Soest", "Nordrhein-Westfalen", 51.5756, 8.1062, false, Seq("Soest"), Seq(
    ("Bürgerzentrum Alter Schlachthof", "Bürgerzentrum Alter Schlachthof", "A0117", None, "Soest"),
    ("Neues Universum", "Neues Universum", "A0994", Some("/kino/soest/universum-31916"), "Soest")
  ))
  private def p_speyer: R = ("speyer", "speyer-rheinland-pfalz", "Speyer", "Rheinland-Pfalz", 49.3208, 8.4311, true, Seq("Speyer", "Schifferstadt", "Walldorf", "Ketsch", "Limburgerhof"), Seq(
    ("Capitol LichtspielTheater Limburgerhof", "Capitol LichtspielTheater Limburgerhof", "A1336", Some("/kino/limburgerhof/capitol-lichtspieltheater-32150"), "Limburgerhof"),
    ("Central Filmtheater Ketsch", "Central Filmtheater Ketsch", "A1345", Some("/kino/ketsch-rhein/central-filmtheater-32125"), "Ketsch"),
    ("IMAX Speyer", "IMAX Speyer", "A0624", None, "Speyer"),
    ("Kinocenter Theaterhaus Speyer", "Kinocenter Theaterhaus Speyer", "A1647", None, "Speyer"),
    ("Luxor Filmpalast Walldorf", "Luxor Filmpalast Walldorf", "A1763", Some("/kino/walldorf-baden/luxor-filmpalast-70737"), "Walldorf"),
    ("Rex Kino-Center Schifferstadt", "Rex Kino-Center Schifferstadt", "A1072", Some("/kino/schifferstadt/rexkinocenter-85977"), "Schifferstadt")
  ))
  private def p_spremberg: R = ("spremberg", "spremberg-brandenburg", "Spremberg", "Brandenburg", 51.5696, 14.3738, false, Seq("Spremberg"), Seq(
    ("Spreekino", "Spreekino", "A1143", Some("/kino/spremberg-niederlausitz/spreekino-39157"), "Spremberg"),
    ("Spremberger Filmnächte", "Spremberger Filmnächte", "A2758", None, "Spremberg")
  ))
  private def p_stade: R = ("stade", "stade-niedersachsen", "Stade", "Niedersachsen", 53.5941, 9.473, true, Seq("Stade", "Buxtehude", "Harsefeld"), Seq(
    ("CineStar Stade", "CineStar Stade", "A0329", Some("/kino/stade/cinestar-38679"), "Stade"),
    ("City Kino Buxtehude", "City Kino Buxtehude", "A1381", Some("/kino/buxtehude/city-kino-buxtehude-32382"), "Buxtehude"),
    ("Harsefelder Lichtspiele", "Harsefelder Lichtspiele", "A0602", Some("/kino/harsefeld/harsefelder-lichtspiele-32479"), "Harsefeld")
  ))
  private def p_stendal: R = ("stendal", "stendal-sachsen-anhalt", "Stendal", "Sachsen-Anhalt", 52.6058, 11.8609, false, Seq("Stendal"), Seq(
    ("Uppstall Kinos", "Uppstall Kinos", "A0664", Some("/kino/stendal/uppstall-kinos-41696"), "Stendal")
  ))
  private def p_stralsund: R = ("stralsund", "stralsund-mecklenburg-vorpommern", "Stralsund", "Mecklenburg-Vorpommern", 54.3091, 13.0818, true, Seq("Stralsund", "Bergen auf Rügen", "Sassnitz", "Göhren", "Vitte"), Seq(
    ("CineStar Stralsund", "CineStar Stralsund", "A0359", Some("/kino/stralsund/cinestar-der-filmpalast-31678"), "Stralsund"),
    ("Kino Bergen auf Rügen", "Kino Bergen auf Rügen", "A0709", None, "Bergen auf Rügen"),
    ("Lichtspiele Sassnitz", "Lichtspiele Sassnitz", "A2711", Some("/kino/sassnitz/lichtspiele-87266"), "Sassnitz"),
    ("Ostseebad Göhren Kinohalle - Regenbogencamp", "Ostseebad Göhren Kinohalle - Regenbogencamp", "A2815", None, "Göhren"),
    ("Zeltkino Hiddensee", "Zeltkino Hiddensee", "A2097", None, "Vitte")
  ))
  private def p_straubing: R = ("straubing", "straubing-bayern", "Straubing", "Bayern", 48.8813, 12.5739, true, Seq("Straubing", "Wörth an der Donau"), Seq(
    ("Citydom", "Citydom", "A0400", Some("/kino/straubing/citydom-33003"), "Straubing"),
    ("Donau-Lichtspiele", "Donau-Lichtspiele", "A1396", Some("/kino/woerth-an-der-donau/donaulichtspiele-33071"), "Wörth an der Donau")
  ))
  private def p_suhl: R = ("suhl", "suhl-thueringen", "Suhl", "Thüringen", 50.6091, 10.694, true, Seq("Suhl", "Zella-Mehlis", "Masserberg"), Seq(
    ("Cineplex Suhl", "Cineplex Suhl", "A0304", Some("/kino/suhl/cineplex-41357"), "Suhl"),
    ("Clubkino Zella-Mehlis", "Clubkino Zella-Mehlis", "A2753", None, "Zella-Mehlis"),
    ("Kino im Badehaus Masserberg", "Kino im Badehaus Masserberg", "kinoheld:1372", None, "Masserberg")
  ))
  private def p_tauberbischofsheim: R = ("tauberbischofsheim", "tauberbischofsheim-baden-wuerttemberg", "Tauberbischofsheim", "Baden-Württemberg", 49.6247, 9.6628, true, Seq("Tauberbischofsheim", "Lauda-Königshofen", "Walldürn"), Seq(
    ("Filmtheater Badischer Hof", "Filmtheater Badischer Hof", "A0541", None, "Tauberbischofsheim"),
    ("Open Air Kino Rotary-Benefiz", "Open Air Kino Rotary-Benefiz", "A1995", None, "Tauberbischofsheim"),
    ("Kinomobil Stuttgart - Lauda Sternen Filmtheater", "Kinomobil Stuttgart - Lauda Sternen Filmtheater", "A0794", Some("/kino/laudakoenigshofen/kinomobil-stuttgart-stern-filmtheater-40617"), "Lauda-Königshofen"),
    ("Löwenlichtspiele Walldürn", "Löwenlichtspiele Walldürn", "A0922", Some("/kino/wallduern/loewenlichtspiele-32291"), "Walldürn")
  ))
  private def p_taucha: R = ("taucha", "taucha-sachsen", "Taucha", "Sachsen", 51.3833, 12.4833, false, Seq("Taucha"), Seq(
    ("CT-Lichtspiele", "CT-Lichtspiele", "A0429", Some("/kino/taucha-bei-leipzig/ctlichtspiele-75777"), "Taucha")
  ))
  private def p_templin: R = ("templin", "templin-brandenburg", "Templin", "Brandenburg", 53.1187, 13.5022, true, Seq("Templin", "Feldberger Seenlandschaft", "Lychen"), Seq(
    ("Altes Kino", "Altes Kino", "A2634", Some("/kino/lychen/altes-kino-31599"), "Lychen"),
    ("Clubkino Feldberg", "Clubkino Feldberg", "A2761", None, "Feldberger Seenlandschaft"),
    ("Kino im Multikulturellen Centrum", "Kino im Multikulturellen Centrum", "A0016", Some("/kino/templin/kino-im-multikulturellen-centrum-31685"), "Templin")
  ))
  private def p_torgau: R = ("torgau", "torgau-sachsen", "Torgau", "Sachsen", 51.5602, 12.9962, false, Seq("Torgau"), Seq(
    ("KAP-Torgau e.V.", "KAP-Torgau e.V.", "A0646", Some("/kino/torgau/kaptorgau-e.v.-42677"), "Torgau")
  ))
  private def p_traunreut: R = ("traunreut", "traunreut-bayern", "Traunreut", "Bayern", 47.9627, 12.5923, true, Seq("Traunreut", "Trostberg"), Seq(
    ("Cine Chiemgau Traunreut", "Cine Chiemgau Traunreut", "A1191", Some("/kino/traunreut/cine-chiemgau-61097"), "Traunreut"),
    ("Stadtkino Trostberg", "Stadtkino Trostberg", "A1620", Some("/kino/trostberg/stadtkino-33028"), "Trostberg")
  ))
  private def p_traunstein: R = ("traunstein", "traunstein-bayern", "Traunstein", "Bayern", 47.8683, 12.6433, false, Seq("Traunstein"), Seq(
    ("Cine Chiemgau Traunstein", "Cine Chiemgau Traunstein", "A0177", Some("/kino/traunstein/cine-chiemgau-33025"), "Traunstein"),
    ("Kinos am Bahnhof", "Kinos am Bahnhof", "A0136", Some("/kino/traunstein/kinos-am-bahnhof-38757"), "Traunstein")
  ))
  private def p_tuebingen: R = ("tuebingen", "tuebingen-baden-wuerttemberg", "Tübingen", "Baden-Württemberg", 48.5227, 9.0522, false, Seq("Tübingen"), Seq(
    ("Atelier Tübingen", "Atelier Tübingen", "A1301", Some("/kino/tuebingen/atelier-33122"), "Tübingen"),
    ("Filmtheater Blaue Brücke", "Filmtheater Blaue Brücke", "A0683", Some("/kino/tuebingen/blaue-bruecke-70798"), "Tübingen"),
    ("Museum-Lichtspiele Tübingen", "Museum-Lichtspiele Tübingen", "A0691", Some("/kino/tuebingen/kino-museum-34641"), "Tübingen"),
    ("Sommernachtskino Tübingen", "Sommernachtskino Tübingen", "A1915", Some("/kino/tuebingen/sommernachtskino-55519"), "Tübingen")
  ))
  private def p_tuttlingen: R = ("tuttlingen", "tuttlingen-baden-wuerttemberg", "Tuttlingen", "Baden-Württemberg", 47.9846, 8.8177, true, Seq("Tuttlingen", "Trossingen", "Emmingen-Liptingen", "Kolbingen"), Seq(
    ("Kinomobil Stuttgart - Kolbingen Dorfplatz", "Kinomobil Stuttgart - Kolbingen Dorfplatz", "A0811", None, "Kolbingen"),
    ("Kinomobil Stuttgart - Musiksaal der Witthohschule", "Kinomobil Stuttgart - Musiksaal der Witthohschule", "A1941", None, "Emmingen-Liptingen"),
    ("Kommunales Kino Trossingen", "Kommunales Kino Trossingen", "A2855", None, "Trossingen"),
    ("Scala Tuttlingen", "Scala Tuttlingen", "A1109", Some("/kino/tuttlingen/scala-33035"), "Tuttlingen")
  ))
  private def p_ueberlingen: R = ("ueberlingen", "ueberlingen-baden-wuerttemberg", "Überlingen", "Baden-Württemberg", 47.7698, 9.1714, false, Seq("Überlingen"), Seq(
    ("Cinegreth", "Cinegreth", "A0226", Some("/kino/ueberlingen/cinegreth-37137"), "Überlingen"),
    ("Kammer-Lichtspiele Überlingen", "Kammer-Lichtspiele Überlingen", "A0639", Some("/kino/ueberlingen/kammerlichtspiele-32279"), "Überlingen")
  ))
  private def p_ueckermuende: R = ("ueckermuende", "ueckermuende-mecklenburg-vorpommern", "Ueckermünde", "Mecklenburg-Vorpommern", 53.7379, 14.0447, false, Seq("Ueckermünde"), Seq(
    ("Volksbühne", "Volksbühne", "A1262", Some("/kino/ueckermuende/volksbuehne-31693"), "Ueckermünde")
  ))
  private def p_uelzen: R = ("uelzen", "uelzen-niedersachsen", "Uelzen", "Niedersachsen", 52.9645, 10.567, false, Seq("Uelzen"), Seq(
    ("Central-Theater Uelzen", "Central-Theater Uelzen", "A0206", Some("/kino/uelzen/centraltheater-34348"), "Uelzen")
  ))
  private def p_unterschleissheim: R = ("unterschleissheim", "unterschleissheim-bayern", "Unterschleissheim", "Bayern", 48.2804, 11.5768, true, Seq("Unterschleissheim", "Aschheim"), Seq(
    ("Capitol Unterschleißheim", "Capitol Unterschleißheim", "A1328", Some("/kino/unterschleissheim/capitol-33043"), "Unterschleissheim"),
    ("Drive In Autokino Aschheim", "Drive In Autokino Aschheim", "A0452", Some("/kino/aschheim/drive-in-autokino-aschheim-32667"), "Aschheim")
  ))
  private def p_viersen: R = ("viersen", "viersen-nordrhein-westfalen", "Viersen", "Nordrhein-Westfalen", 51.2544, 6.3944, true, Seq("Viersen", "Willich", "Nettetal", "Kempen"), Seq(
    ("Corso Film Casino", "Corso Film Casino", "A0423", Some("/kino/nettetal/corso-film-casino-33189"), "Nettetal"),
    ("Kempener Lichtspiele", "Kempener Lichtspiele", "A0650", Some("/kino/kempen/kempener-lichtspiele-31829"), "Kempen"),
    ("Königsburg Süchteln", "Königsburg Süchteln", "kinoheld:3769", None, "Viersen"),
    ("Lichtspieltheater Willich", "Lichtspieltheater Willich", "cineamo:2981", None, "Willich")
  ))
  private def p_villingen_schwenningen: R = ("villingen-schwenningen", "villingen-schwenningen-baden-wuerttemberg", "Villingen-Schwenningen", "Baden-Württemberg", 48.0623, 8.4936, false, Seq("Villingen-Schwenningen"), Seq(
    ("Blue Boxx", "Blue Boxx", "A0102", Some("/kino/villingenschwenningen/blue-boxx-65796"), "Villingen-Schwenningen"),
    ("CineStar Villingen-Schwenningen", "CineStar Villingen-Schwenningen", "A2887", Some("/kino/villingenschwenningen/cinestar-villingenschwenningen-46256"), "Villingen-Schwenningen"),
    ("Kommunales Kino Guckloch Schwarzwald-Baar-Kreis", "Kommunales Kino Guckloch Schwarzwald-Baar-Kreis", "A0863", Some("/kino/villingenschwenningen/kommunales-kino-guckloch-65297"), "Villingen-Schwenningen")
  ))
  private def p_vilsbiburg: R = ("vilsbiburg", "vilsbiburg-bayern", "Vilsbiburg", "Bayern", 48.453, 12.356, true, Seq("Vilsbiburg", "Gangkofen"), Seq(
    ("Cineplex Vilsbiburg", "Cineplex Vilsbiburg", "A0896", Some("/kino/vilsbiburg/cineplex-vilsbiburg-33046"), "Vilsbiburg"),
    ("Phantasia", "Phantasia", "A2587", Some("/kino/gangkofen/phantasia-32765"), "Gangkofen")
  ))
  private def p_voelklingen: R = ("voelklingen", "voelklingen-saarland", "Völklingen", "Saarland", 49.2503, 6.8592, true, Seq("Völklingen", "Saarlouis", "Lebach", "Illingen", "Bous", "Rehlingen-Siersburg"), Seq(
    ("City-Filmstudio", "City-Filmstudio", "A1379", Some("/kino/lebach/cityfilmstudio-32146"), "Lebach"),
    ("Kino auf der Burg", "Kino auf der Burg", "A2513", Some("/kino/rehlingensiersburg/kino-auf-der-burg-76122"), "Rehlingen-Siersburg"),
    ("Movie-World", "Movie-World", "A0976", Some("/kino/saarlouis/movieworld-32237"), "Saarlouis"),
    ("Residenz", "Residenz", "A2298", None, "Völklingen"),
    ("Thalia-Lichtspiele", "Thalia-Lichtspiele", "A1646", Some("/kino/bous/thalialichtspiele-31993"), "Bous"),
    ("Union Theater Illingen", "Union Theater Illingen", "A1235", Some("/kino/illingen-saar/union-theater-32100"), "Illingen")
  ))
  private def p_wadern: R = ("wadern", "wadern-saarland", "Wadern", "Saarland", 49.5412, 6.8877, true, Seq("Wadern", "Nonnweiler", "Hoppstädten-Weiersbach"), Seq(
    ("Central-Filmtheater Nonnweiler", "Central-Filmtheater Nonnweiler", "A0190", Some("/kino/nonnweiler/centralfilmtheater-32204"), "Nonnweiler"),
    ("Movietown Neubrücke", "Movietown Neubrücke", "A0973", Some("/kino/hoppstaedtenweiersbach/movietown-47597"), "Hoppstädten-Weiersbach"),
    ("Starlight-Kino", "Starlight-Kino", "A1150", None, "Wadern")
  ))
  private def p_waldkraiburg: R = ("waldkraiburg", "waldkraiburg-bayern", "Waldkraiburg", "Bayern", 48.2085, 12.3989, true, Seq("Waldkraiburg", "Mühldorf"), Seq(
    ("Cineplex Waldkraiburg", "Cineplex Waldkraiburg", "A0386", Some("/kino/waldkraiburg/cineplex-38977"), "Waldkraiburg"),
    ("Hollywood am Inn", "Hollywood am Inn", "A1675", Some("/kino/muehldorf-am-inn/hollywood-70058"), "Mühldorf")
  ))
  private def p_waldshut_tiengen: R = ("waldshut-tiengen", "waldshut-tiengen-baden-wuerttemberg", "Waldshut-Tiengen", "Baden-Württemberg", 47.6232, 8.2172, false, Seq("Waldshut-Tiengen"), Seq(
    ("Albrecht Kino Waldshut", "Albrecht Kino Waldshut", "A2786", Some("/kino/waldshuttiengen/albrecht-kino-waldshut-75377"), "Waldshut-Tiengen"),
    ("Open Air-Kino Waldshut", "Open Air-Kino Waldshut", "A2225", None, "Waldshut-Tiengen")
  ))
  private def p_walsrode: R = ("walsrode", "walsrode-niedersachsen", "Walsrode", "Niedersachsen", 52.861, 9.5928, true, Seq("Walsrode", "Soltau", "Schneverdingen"), Seq(
    ("Capitol-Theater Walsrode", "Capitol-Theater Walsrode", "A0167", Some("/kino/walsrode/capitoltheater-78545"), "Walsrode"),
    ("Gloria Kino Center Soltau", "Gloria Kino Center Soltau", "A1441", None, "Soltau"),
    ("LichtSpiel e.V. Schneverdingen", "LichtSpiel e.V. Schneverdingen", "A2673", Some("/kino/schneverdingen/lichtspiel-e.v.-85637"), "Schneverdingen")
  ))
  private def p_wangen_im_allgaeu: R = ("wangen-im-allgaeu", "wangen-im-allgaeu-baden-wuerttemberg", "Wangen im Allgäu", "Baden-Württemberg", 47.6895, 9.8325, true, Seq("Wangen im Allgäu", "Isny im Allgäu", "Lindenberg im Allgäu", "Argenbühl"), Seq(
    ("Kinomobil Stuttgart - Bürgersaal Rathaus", "Kinomobil Stuttgart - Bürgersaal Rathaus", "A1762", Some("/kino/argenbuehl/kinomobil-stuttgart-buergersaal-rathaus-42551"), "Argenbühl"),
    ("Lichtspielhaus Wangen im Allgäu", "Lichtspielhaus Wangen im Allgäu", "A1163", None, "Wangen im Allgäu"),
    ("Neues Krone Kino", "Neues Krone Kino", "A1574", None, "Lindenberg im Allgäu"),
    ("Neues Ringtheater", "Neues Ringtheater", "A0992", None, "Isny im Allgäu")
  ))
  private def p_warburg: R = ("warburg", "warburg-nordrhein-westfalen", "Warburg", "Nordrhein-Westfalen", 51.4901, 9.1464, true, Seq("Warburg", "Hofgeismar", "Borgentreich"), Seq(
    ("Central-Kino Borgentreich", "Central-Kino Borgentreich", "A0201", Some("/kino/borgentreich/centralkino-31778"), "Borgentreich"),
    ("Cineplex Warburg", "Cineplex Warburg", "A0311", Some("/kino/warburg/cineplex-36756"), "Warburg"),
    ("Open-Air-Kino Hofgeismar", "Open-Air-Kino Hofgeismar", "A2289", Some("/kino/hofgeismar/openairkino-hofgeismar-73663"), "Hofgeismar")
  ))
  private def p_waren_mueritz: R = ("waren-mueritz", "waren-mueritz-mecklenburg-vorpommern", "Waren (Müritz)", "Mecklenburg-Vorpommern", 53.5199, 12.6813, true, Seq("Waren (Müritz)", "Malchin", "Malchow"), Seq(
    ("CineStar Waren (Müritz)", "CineStar Waren (Müritz)", "A2794", Some("/kino/waren/cinestar-38780"), "Waren (Müritz)"),
    ("Filmbühne Malchin", "Filmbühne Malchin", "A0660", None, "Malchin"),
    ("Kino Malchow", "Kino Malchow", "A0535", Some("/kino/malchow/filmtheater-35180"), "Malchow")
  ))
  private def p_warendorf: R = ("warendorf", "warendorf-nordrhein-westfalen", "Warendorf", "Nordrhein-Westfalen", 51.9511, 7.9876, true, Seq("Warendorf", "Versmold"), Seq(
    ("Kulturbühne Versmold", "Kulturbühne Versmold", "G02OV", None, "Versmold"),
    ("Scala und Studio", "Scala und Studio", "A1113", None, "Warendorf")
  ))
  private def p_wasserburg_am_inn: R = ("wasserburg-am-inn", "wasserburg-am-inn-bayern", "Wasserburg am Inn", "Bayern", 48.0525, 12.2234, false, Seq("Wasserburg am Inn"), Seq(
    ("Kino Utopia", "Kino Utopia", "A1658", Some("/kino/wasserburg-am-inn/kino-utopia-33054"), "Wasserburg am Inn"),
    ("Open Air im STrandpark Wasserburg", "Open Air im STrandpark Wasserburg", "A2561", None, "Wasserburg am Inn")
  ))
  private def p_weiden_in_der_oberpfalz: R = ("weiden-in-der-oberpfalz", "weiden-in-der-oberpfalz-bayern", "Weiden in der Oberpfalz", "Bayern", 49.6867, 12.1617, false, Seq("Weiden in der Oberpfalz"), Seq(
    ("Neue Welt Kinocenter", "Neue Welt Kinocenter", "A0985", None, "Weiden in der Oberpfalz")
  ))
  private def p_weil_am_rhein: R = ("weil-am-rhein", "weil-am-rhein-baden-wuerttemberg", "Weil am Rhein", "Baden-Württemberg", 47.5933, 7.6208, false, Seq("Weil am Rhein"), Seq(
    ("Kinopalast am Rheincenter", "Kinopalast am Rheincenter", "A0821", Some("/kino/weil-am-rhein/kinopalast-im-rheincenter-60897"), "Weil am Rhein"),
    ("Open Air Kieswerk", "Open Air Kieswerk", "A2516", Some("/kino/weil-am-rhein/kieswerk-open-air-91914"), "Weil am Rhein")
  ))
  private def p_weil_der_stadt: R = ("weil-der-stadt", "weil-der-stadt-baden-wuerttemberg", "Weil der Stadt", "Baden-Württemberg", 48.7495, 8.8718, true, Seq("Weil der Stadt", "Renningen", "Althengstett"), Seq(
    ("Kinocenter", "Kinocenter", "A2771", None, "Weil der Stadt"),
    ("Kulisse", "Kulisse", "A2806", Some("/kino/weil-der-stadt/kino-kulisse-69417"), "Weil der Stadt"),
    ("Kinomobil Stuttgart - Festhalle Althengstett", "Kinomobil Stuttgart - Festhalle Althengstett", "A0799", Some("/kino/althengstett/kinomobil-stuttgart-festhalle-41783"), "Althengstett"),
    ("Kinomobil Stuttgart - Festhalle Stegwiesen", "Kinomobil Stuttgart - Festhalle Stegwiesen", "A0792", Some("/kino/renningen/kinomobil-stuttgart-festhalle-stegwiesen-41591"), "Renningen")
  ))
  private def p_weilheim_in_oberbayern: R = ("weilheim-in-oberbayern", "weilheim-in-oberbayern-bayern", "Weilheim in Oberbayern", "Bayern", 47.8415, 11.1548, false, Seq("Weilheim in Oberbayern"), Seq(
    ("Starlight", "Starlight", "A1633", None, "Weilheim in Oberbayern"),
    ("Trifthof Kinocenter", "Trifthof Kinocenter", "A1649", Some("/kino/weilheim/kinocenter-trifthof-33074"), "Weilheim in Oberbayern")
  ))
  private def p_weimar: R = ("weimar", "weimar-thueringen", "Weimar", "Thüringen", 50.9803, 11.329, false, Seq("Weimar"), Seq(
    ("CineStar Weimar", "CineStar Weimar", "A0360", Some("/kino/weimar/cinestar-der-filmpalast-31695"), "Weimar"),
    ("Kommunales Kino im mon ami", "Kommunales Kino im mon ami", "A0867", None, "Weimar"),
    ("Lichthaus Kino im Straßenbahndepot/e-werk", "Lichthaus Kino im Straßenbahndepot/e-werk", "A0906", Some("/kino/weimar/lichthaus-kino-im-strassenbahndepotewerk-53997"), "Weimar"),
    ("Open Air am Lichthaus-Kino", "Open Air am Lichthaus-Kino", "A2783", None, "Weimar")
  ))
  private def p_weinheim: R = ("weinheim", "weinheim-baden-wuerttemberg", "Weinheim", "Baden-Württemberg", 49.5489, 8.667, true, Seq("Weinheim", "Hemsbach", "Hirschberg"), Seq(
    ("Brennessel-Programmkino", "Brennessel-Programmkino", "A0106", Some("/kino/hemsbach-bergstrasse/brennessel-programmkino-32086"), "Hemsbach"),
    ("Modernes Theater", "Modernes Theater", "A1567", Some("/kino/weinheim/modernes-theater-32293"), "Weinheim"),
    ("Olympia Hirschberg", "Olympia Hirschberg", "A1580", None, "Hirschberg")
  ))
  private def p_weissenburg: R = ("weissenburg", "weissenburg-bayern", "Weissenburg", "Bayern", 49.0309, 10.9722, true, Seq("Weissenburg", "Gunzenhausen", "Treuchtlingen", "Wemding"), Seq(
    ("Central Kino und Kultur Treuchtlingen", "Central Kino und Kultur Treuchtlingen", "A0189", Some("/kino/treuchtlingen/central-kino-65597"), "Treuchtlingen"),
    ("Kinocenter Weißenburg in Bayern", "Kinocenter Weißenburg in Bayern", "A1506", Some("/kino/weissenburg-in-bayern/kinocenter-33061"), "Weissenburg"),
    ("Movieworld", "Movieworld", "A0975", Some("/kino/gunzenhausen/movieworld-64637"), "Gunzenhausen"),
    ("Wemdinger Lichtspiele", "Wemdinger Lichtspiele", "A1275", Some("/kino/wemding/wemdinger-lichtspiele-33063"), "Wemding")
  ))
  private def p_weiterstadt: R = ("weiterstadt", "weiterstadt-hessen", "Weiterstadt", "Hessen", 49.9039, 8.5887, true, Seq("Weiterstadt", "Pfungstadt", "Gross-Gerau", "Seeheim-Jugenheim", "Ginsheim-Gustavsburg"), Seq(
    ("Burg-Lichtspiele (Ginsheim-Gustavsburg)", "Burg-Lichtspiele (Ginsheim-Gustavsburg)", "A0868", Some("/kino/ginsheimgustavsburg/burglichtspiele-32066"), "Ginsheim-Gustavsburg"),
    ("Filmseher Open Air", "Filmseher Open Air", "A2797", None, "Seeheim-Jugenheim"),
    ("Kommunales Kino Weiterstadt im Bürgerzentrum", "Kommunales Kino Weiterstadt im Bürgerzentrum", "A0858", Some("/kino/weiterstadt/kommunales-kino-im-buergerzentrum-34894"), "Weiterstadt"),
    ("Lichtspielhaus Groß-Gerau", "Lichtspielhaus Groß-Gerau", "A0913", None, "Gross-Gerau"),
    ("Saalbau-Lichtspiele Pfungstadt", "Saalbau-Lichtspiele Pfungstadt", "A1098", Some("/kino/pfungstadt/saalbau-kino-32219"), "Pfungstadt")
  ))
  private def p_werder: R = ("werder", "werder-brandenburg", "Werder", "Brandenburg", 52.3787, 12.934, true, Seq("Werder", "Beelitz"), Seq(
    ("Scala Kulturpalast Werder", "Scala Kulturpalast Werder", "A0555", Some("/kino/werder-havel/scala-kulturpalast-84477"), "Werder"),
    ("Neues Lichtspielhaus Beelitz", "Neues Lichtspielhaus Beelitz", "kinoheld:3619", None, "Beelitz")
  ))
  private def p_wermelskirchen: R = ("wermelskirchen", "wermelskirchen-nordrhein-westfalen", "Wermelskirchen", "Nordrhein-Westfalen", 51.1397, 7.2158, true, Seq("Wermelskirchen", "Radevormwald"), Seq(
    ("Corso Kinocenter", "Corso Kinocenter", "A0425", Some("/kino/radevormwald/corso-kinocenter-756"), "Radevormwald"),
    ("Film-Eck", "Film-Eck", "A1414", Some("/kino/wermelskirchen/filmeck-31938"), "Wermelskirchen")
  ))
  private def p_wertheim_am_main: R = ("wertheim-am-main", "wertheim-am-main-baden-wuerttemberg", "Wertheim am Main", "Baden-Württemberg", 49.759, 9.5085, true, Seq("Wertheim am Main", "Marktheidenfeld", "Miltenberg"), Seq(
    ("Movie im Luitpoldhaus", "Movie im Luitpoldhaus", "A0965", Some("/kino/marktheidenfeld/movie-im-luitpoldhaus-37691"), "Marktheidenfeld"),
    ("Roxy Wertheim", "Roxy Wertheim", "A1087", Some("/kino/wertheim-am-main/roxy-33911"), "Wertheim am Main"),
    ("Schloß-Theater", "Schloß-Theater", "A1131", Some("/kino/miltenberg/schlosstheater-32183"), "Miltenberg")
  ))
  private def p_wesenberg: R = ("wesenberg", "wesenberg-mecklenburg-vorpommern", "Wesenberg", "Mecklenburg-Vorpommern", 53.2803, 12.9694, false, Seq("Wesenberg"), Seq(
    ("Kino Wesenberg", "Kino Wesenberg", "A0757", None, "Wesenberg")
  ))
  private def p_westerland: R = ("westerland", "westerland-schleswig-holstein", "Westerland", "Schleswig-Holstein", 54.9079, 8.3033, false, Seq("Westerland"), Seq(
    ("Kinowelt Westerland", "Kinowelt Westerland", "A0843", Some("/kino/westerland/kinowelt-westerland-44317"), "Westerland")
  ))
  private def p_wildeshausen: R = ("wildeshausen", "wildeshausen-niedersachsen", "Wildeshausen", "Niedersachsen", 52.8945, 8.4337, true, Seq("Wildeshausen", "Twistringen"), Seq(
    ("Filmtheater Twistringen", "Filmtheater Twistringen", "A1358", Some("/kino/twistringen/filmtheater-32622"), "Twistringen"),
    ("Lindenhof-Lichtspiele", "Lindenhof-Lichtspiele", "A0919", None, "Wildeshausen")
  ))
  private def p_wilhelmshaven: R = ("wilhelmshaven", "wilhelmshaven-niedersachsen", "Wilhelmshaven", "Niedersachsen", 53.5476, 8.1039, true, Seq("Wilhelmshaven", "Zetel"), Seq(
    ("UCI Wilhelmshaven", "UCI Wilhelmshaven", "A0043", Some("/kino/wilhelmshaven/uci-kinowelt-wilhelmshaven-46376"), "Wilhelmshaven"),
    ("Zeli - Zeteler Lichtspiele e.V.", "Zeli - Zeteler Lichtspiele e.V.", "A1660", Some("/kino/zetel/zeli-zeteler-lichtspiele-e.v.-32651"), "Zetel")
  ))
  private def p_winsen: R = ("winsen", "winsen-niedersachsen", "Winsen", "Niedersachsen", 53.3578, 10.2116, true, Seq("Winsen", "Geesthacht"), Seq(
    ("Kinocenter Winsen", "Kinocenter Winsen", "A1504", None, "Winsen"),
    ("Kleines Theater Schillerstraße", "Kleines Theater Schillerstraße", "A0851", Some("/kino/geesthacht/kleines-theater-schillerstrasse-32423"), "Geesthacht")
  ))
  private def p_wismar: R = ("wismar", "wismar-mecklenburg-vorpommern", "Wismar", "Mecklenburg-Vorpommern", 53.8922, 11.4556, false, Seq("Wismar"), Seq(
    ("CineStar Wismar", "CineStar Wismar", "A0361", Some("/kino/wismar-mecklenburg/cinestar-31702"), "Wismar")
  ))
  private def p_witten: R = ("witten", "witten-nordrhein-westfalen", "Witten", "Nordrhein-Westfalen", 51.4436, 7.3526, true, Seq("Witten", "Gevelsberg", "Schwelm", "Wetter"), Seq(
    ("Die Burg Witten", "Die Burg Witten", "A2888", Some("/kino/witten/die-burg-876"), "Witten"),
    ("Filmriss Kino", "Filmriss Kino", "A0524", Some("/kino/gevelsberg/filmriss-kino-66337"), "Gevelsberg"),
    ("Kinocenter Schwelm", "Kinocenter Schwelm", "A0776", Some("/kino/schwelm/kinocenter-schwelm-798"), "Schwelm"),
    ("Kulturzentrum Lichtburg", "Kulturzentrum Lichtburg", "A0879", Some("/kino/wetter/kulturzentrum-lichtburg-874"), "Wetter")
  ))
  private def p_wittenberge: R = ("wittenberge", "wittenberge-brandenburg", "Wittenberge", "Brandenburg", 53.0001, 11.7494, true, Seq("Wittenberge", "Perleberg"), Seq(
    ("Movie Star Perleberg", "Movie Star Perleberg", "A1234", Some("/kino/perleberg/movie-star-65579"), "Perleberg"),
    ("Movie Star Wittenberge", "Movie Star Wittenberge", "A0968", Some("/kino/wittenberge/movie-star-38202"), "Wittenberge")
  ))
  private def p_wittingen: R = ("wittingen", "wittingen-niedersachsen", "Wittingen", "Niedersachsen", 52.7269, 10.7361, false, Seq("Wittingen"), Seq(
    ("Wittinger Lichtspiele", "Wittinger Lichtspiele", "A0676", Some("/kino/wittingen/wittinger-lichtspiele-32644"), "Wittingen")
  ))
  private def p_wittlich: R = ("wittlich", "wittlich-rheinland-pfalz", "Wittlich", "Rheinland-Pfalz", 49.986, 6.8931, true, Seq("Wittlich", "Bernkastel-Kues"), Seq(
    ("Kinopalast Eifel Mosel Hunsrück", "Kinopalast Eifel Mosel Hunsrück", "G011D", Some("/kino/wittlich/kinopalast-eifelmoselhunsrueck-92635"), "Wittlich"),
    ("Mosel-Kino", "Mosel-Kino", "A0180", Some("/kino/bernkastelkues/moselkino-31753"), "Bernkastel-Kues")
  ))
  private def p_wittstock: R = ("wittstock", "wittstock-brandenburg", "Wittstock", "Brandenburg", 53.1612, 12.4829, true, Seq("Wittstock", "Pritzwalk", "Zempow"), Seq(
    ("Astoria Wittstock", "Astoria Wittstock", "A2377", None, "Wittstock"),
    ("Autokino Zempow", "Autokino Zempow", "A0071", None, "Zempow"),
    ("Kulturhaus Pritzwalk", "Kulturhaus Pritzwalk", "A2625", Some("/kino/pritzwalk/kulturhaus-pritzwalk-81799"), "Pritzwalk")
  ))
  private def p_witzenhausen: R = ("witzenhausen", "witzenhausen-hessen", "Witzenhausen", "Hessen", 51.341, 9.8554, false, Seq("Witzenhausen"), Seq(
    ("Capitol-Kino", "Capitol-Kino", "A1335", Some("/kino/witzenhausen/capitol-32308"), "Witzenhausen")
  ))
  private def p_wolfenbuettel: R = ("wolfenbuettel", "wolfenbuettel-niedersachsen", "Wolfenbüttel", "Niedersachsen", 52.1644, 10.541, false, Seq("Wolfenbüttel"), Seq(
    ("Filmpalast Wolfenbüttel (Juliusstadt)", "Filmpalast Wolfenbüttel (Juliusstadt)", "A1422", Some("/kino/wolfenbuettel-niedersachsen/filmpalast-32646"), "Wolfenbüttel")
  ))
  private def p_wolfratshausen: R = ("wolfratshausen", "wolfratshausen-bayern", "Wolfratshausen", "Bayern", 47.9129, 11.4217, true, Seq("Wolfratshausen", "Penzberg", "Tutzing"), Seq(
    ("Kino P.", "Kino P.", "A0819", Some("/kino/penzberg/kino-p.-61777"), "Penzberg"),
    ("Kinocenter Wolfratshausen", "Kinocenter Wolfratshausen", "A1171", Some("/kino/wolfratshausen/kinocenter-33068"), "Wolfratshausen"),
    ("Kurtheater Tutzing", "Kurtheater Tutzing", "A0885", Some("/kino/tutzing/kulturtheater-33037"), "Tutzing")
  ))
  private def p_worms: R = ("worms", "worms-rheinland-pfalz", "Worms", "Rheinland-Pfalz", 49.6328, 8.3592, true, Seq("Worms", "Bensheim", "Viernheim", "Heppenheim", "Biblis"), Seq(
    ("Arkaden Lichtspiele", "Arkaden Lichtspiele", "A0891", None, "Worms"),
    ("Die Filminsel", "Die Filminsel", "A1779", Some("/kino/biblis/die-filminsel-31987"), "Biblis"),
    ("Kinopolis Rhein-Neckar", "Kinopolis Rhein-Neckar", "A0833", Some("/kino/viernheim/kinopolis-32280"), "Viernheim"),
    ("Luxor Filmpalast Bensheim", "Luxor Filmpalast Bensheim", "A1326", Some("/kino/bensheim/luxor-filmpalast-31986"), "Bensheim"),
    ("Saalbau-Filmtheater", "Saalbau-Filmtheater", "A1097", Some("/kino/heppenheim/saalbaufilmtheater-32087"), "Heppenheim")
  ))
  private def p_wusterhausen: R = ("wusterhausen", "wusterhausen-brandenburg", "Wusterhausen", "Brandenburg", 52.8912, 12.4602, false, Seq("Wusterhausen"), Seq(
    ("Lindenkino", "Lindenkino", "A0920", None, "Wusterhausen")
  ))
  private def p_zeil_am_main: R = ("zeil-am-main", "zeil-am-main-bayern", "Zeil am Main", "Bayern", 50.0099, 10.5947, false, Seq("Zeil am Main"), Seq(
    ("Capitol-Theater (Foto Kino Schneyer)", "Capitol-Theater (Foto Kino Schneyer)", "A0166", Some("/kino/zeil-am-main/capitoltheater-foto-kino-schneyer-32320"), "Zeil am Main")
  ))
  private def p_zeven: R = ("zeven", "zeven-niedersachsen", "Zeven", "Niedersachsen", 53.2957, 9.2756, false, Seq("Zeven"), Seq(
    ("Central Theater Zeven", "Central Theater Zeven", "A2354", Some("/kino/zeven/central-theater-32652"), "Zeven")
  ))
  private def p_zittau: R = ("zittau", "zittau-sachsen", "Zittau", "Sachsen", 50.8977, 14.8076, false, Seq("Zittau"), Seq(
    ("Filmpalast Zittau", "Filmpalast Zittau", "A0509", Some("/kino/zittau/filmpalast-31720"), "Zittau"),
    ("Kronenkino", "Kronenkino", "A2685", Some("/kino/zittau/kronenkino-86157"), "Zittau"),
    ("Zittauer Filmnächte", "Zittauer Filmnächte", "A2491", None, "Zittau")
  ))
  private def p_zwickau: R = ("zwickau", "zwickau-sachsen", "Zwickau", "Sachsen", 50.7272, 12.4884, true, Seq("Zwickau", "Werdau"), Seq(
    ("Auto- und Freilichtkino Langenhessen", "Auto- und Freilichtkino Langenhessen", "A2695", None, "Werdau"),
    ("Casablanca Zwickau", "Casablanca Zwickau", "A2378", Some("/kino/zwickau/casablanca-%22alter-gasometer%22-75097"), "Zwickau"),
    ("Filmpalast Astoria Zwickau", "Filmpalast Astoria Zwickau", "A0368", Some("/kino/zwickau/filmpalast-astoria-31722"), "Zwickau")
  ))

  private def chunk0: Seq[R] = Seq(p_berlin, p_hamburg, p_muenchen, p_koeln, p_frankfurt_am_main, p_duesseldorf, p_stuttgart, p_essen, p_dortmund, p_dresden, p_bremen, p_nuernberg, p_hannover, p_leipzig, p_duisburg, p_bochum, p_wuppertal, p_bielefeld, p_bonn, p_muenster, p_mannheim, p_augsburg, p_wiesbaden, p_karlsruhe, p_gelsenkirchen, p_aachen, p_moenchengladbach, p_kiel, p_chemnitz, p_braunschweig, p_magdeburg, p_krefeld, p_halle, p_freiburg, p_mainz, p_oberhausen, p_erfurt, p_luebeck, p_hagen, p_rostock)
  private def chunk1: Seq[R] = Seq(p_kassel, p_potsdam, p_saarbruecken, p_hamm, p_muelheim_an_der_ruhr, p_herne, p_darmstadt, p_osnabrueck, p_solingen, p_leverkusen, p_oldenburg, p_neuss, p_regensburg, p_heidelberg, p_paderborn, p_wuerzburg, p_fuerth, p_wolfsburg, p_recklinghausen, p_goettingen, p_heilbronn, p_ingolstadt, p_ulm, p_bottrop, p_pforzheim, p_offenbach_am_main, p_bremerhaven, p_remscheid, p_reutlingen, p_koblenz, p_siegen, p_bergisch_gladbach, p_salzgitter, p_jena, p_gera, p_moers, p_hildesheim, p_erlangen, p_trier, p_aalen)
  private def chunk2: Seq[R] = Seq(p_achim, p_ahaus, p_ahlen, p_aichach, p_albstadt, p_alfeld, p_alpirsbach, p_alsdorf, p_altenburg, p_amberg, p_anklam, p_annaberg_buchholz, p_ansbach, p_arnsberg, p_asbach, p_aschaffenburg, p_auerbach, p_aurich, p_backnang, p_bad_aibling, p_bad_belzig, p_bad_berleburg, p_bad_driburg, p_bad_fuessing, p_bad_hersfeld, p_bad_kissingen, p_bad_kreuznach, p_bad_mergentheim, p_bad_neuenahr_ahrweiler, p_bad_neustadt_an_der_saale, p_bad_oldesloe, p_bad_reichenhall, p_bad_saulgau, p_bad_schwalbach, p_bad_toelz, p_bad_urach, p_bad_vilbel, p_bad_wildbad, p_bad_woerishofen, p_baden_baden)
  private def chunk3: Seq[R] = Seq(p_bamberg, p_baunatal, p_bautzen, p_bayreuth, p_bernau, p_bernburg, p_besigheim, p_biberach_an_der_riss, p_bocholt, p_boeblingen, p_bramsche, p_brandenburg, p_bruchsal, p_brunsbuettel, p_buchholz_in_der_nordheide, p_buedingen, p_buende, p_burg, p_burghausen, p_butzbach, p_celle, p_cham, p_cloppenburg, p_coburg, p_cochem, p_cottbus, p_crailsheim, p_cuxhaven, p_dahme, p_daun, p_deggendorf, p_delmenhorst, p_demmin, p_dessau_rosslau, p_detmold, p_diessen_am_ammersee, p_dillenburg, p_dillingen_an_der_donau, p_dingolfing, p_dinslaken)
  private def chunk4: Seq[R] = Seq(p_donaueschingen, p_dorfen, p_dreieich, p_duderstadt, p_duelmen, p_dueren, p_ebersbach, p_eberswalde_finow, p_eckental, p_eggenfelden, p_einbeck, p_eisenach, p_elmshorn, p_emden, p_emmendingen, p_enkenbach_alsenborn, p_erbach_im_odenwald, p_erding, p_erdmannhausen, p_eschborn, p_espelkamp, p_esslingen_am_neckar, p_fehmarn, p_fellbach, p_flensburg, p_forchheim, p_frankenthal, p_frankfurt_an_der_oder, p_freiberg, p_freudenstadt, p_freyung, p_friedrichsdorf, p_friedrichshafen, p_fuerstenfeldbruck, p_fuerstenwalde_an_der_spree, p_fuessen, p_fulda, p_furtwangen_im_schwarzwald, p_garbsen, p_garmisch_partenkirchen)
  private def chunk5: Seq[R] = Seq(p_genthin, p_germering, p_giessen, p_gifhorn, p_goeppingen, p_goerlitz, p_goslar, p_gotha, p_grafing_bei_muenchen, p_greifswald, p_grevenbroich, p_grimma, p_gronau_westf, p_gruenberg, p_gruenstadt, p_guestrow, p_guetersloh, p_gummersbach, p_halberstadt, p_hameln, p_hanau, p_hechingen, p_heide, p_heidenheim, p_heinsberg, p_helgoland, p_helmstedt, p_herford, p_herrenberg, p_hof, p_hofheim_am_taunus, p_holzkirchen, p_holzminden, p_homburg, p_hoyerswerda, p_huerth, p_husum, p_ibbenbueren, p_ilmenau, p_iserlohn)
  private def chunk6: Seq[R] = Seq(p_juelich, p_kaiserslautern, p_kaufbeuren, p_kaufering, p_kehl, p_kelheim, p_kempten_im_allgaeu, p_kerpen, p_kirchheim_unter_teck, p_kitzingen, p_kleve, p_kloeden, p_koenigs_wusterhausen, p_koenigsbrunn, p_konstanz, p_kuehlungsborn, p_lahnstein, p_lahr_am_schwarzwald, p_landau_in_der_pfalz, p_landsberg_am_lech, p_landshut, p_langenfeld, p_lehrte, p_letschin, p_leutkirch_im_allgaeu, p_limbach_oberfrohna, p_limburg, p_lindau, p_lingen, p_lippstadt, p_loeningen, p_loerrach, p_lohr_am_main, p_luckenwalde, p_ludwigsburg, p_ludwigslust, p_luebben, p_luedenscheid, p_lueneburg, p_luenen)
  private def chunk7: Seq[R] = Seq(p_lutherstadt_wittenberg, p_marburg, p_marktredwitz, p_marl, p_mayen, p_meiningen, p_meissen, p_memmingen, p_meppen, p_merseburg, p_merzig, p_meschede, p_minden, p_moelln, p_mosbach, p_muehlacker, p_muellheim, p_murrhardt, p_nagold, p_naumburg, p_neckarsulm, p_neresheim, p_neu_anspach, p_neu_ulm, p_neubrandenburg, p_neuburg_an_der_donau, p_neumarkt_in_der_oberpfalz, p_neumuenster, p_neunkirchen, p_neuruppin, p_neustadt_an_der_aisch, p_neustadt_an_der_weinstrasse, p_neustadt_in_holstein, p_neustrelitz, p_neuwied, p_niebuell, p_nienburg, p_noerdlingen, p_norddorf_auf_amrum, p_norderney)
  private def chunk8: Seq[R] = Seq(p_nordhausen, p_nordhorn, p_northeim, p_nuertingen, p_oberndorf_am_neckar, p_oberstdorf, p_oberviechtach, p_oehringen, p_offenburg, p_oldenburg_in_holstein, p_oranienburg, p_osterholz_scharmbeck, p_ottobrunn, p_papenburg, p_parchim, p_passau, p_pfaffenhofen_an_der_ilm, p_pirmasens, p_pirna, p_plauen, p_plettenberg, p_preetz, p_prenzlau, p_prerow, p_prien_am_chiemsee, p_quickborn, p_rastatt, p_rathenow, p_ratingen, p_ravensburg, p_rendsburg, p_rheine, p_rheinfelden, p_riedlingen, p_riesa, p_rietschen, p_rinteln, p_rodgau, p_rothenburg, p_rottenburg_am_neckar)
  private def chunk9: Seq[R] = Seq(p_rottweil, p_rudolstadt, p_saalfeld, p_salzwedel, p_sangerhausen, p_schleiz, p_schloss_holte_stukenbrock, p_schongau, p_schorndorf, p_schwabach, p_schwaebisch_gmuend, p_schwaebisch_hall, p_schwalmstadt, p_schwedt, p_schweinfurt, p_schwerin, p_senftenberg, p_siegburg, p_simbach_am_inn, p_sindelfingen, p_singen_am_hohentwiel, p_sinsheim, p_soest, p_speyer, p_spremberg, p_stade, p_stendal, p_stralsund, p_straubing, p_suhl, p_tauberbischofsheim, p_taucha, p_templin, p_torgau, p_traunreut, p_traunstein, p_tuebingen, p_tuttlingen, p_ueberlingen, p_ueckermuende)
  private def chunk10: Seq[R] = Seq(p_uelzen, p_unterschleissheim, p_viersen, p_villingen_schwenningen, p_vilsbiburg, p_voelklingen, p_wadern, p_waldkraiburg, p_waldshut_tiengen, p_walsrode, p_wangen_im_allgaeu, p_warburg, p_waren_mueritz, p_warendorf, p_wasserburg_am_inn, p_weiden_in_der_oberpfalz, p_weil_am_rhein, p_weil_der_stadt, p_weilheim_in_oberbayern, p_weimar, p_weinheim, p_weissenburg, p_weiterstadt, p_werder, p_wermelskirchen, p_wertheim_am_main, p_wesenberg, p_westerland, p_wildeshausen, p_wilhelmshaven, p_winsen, p_wismar, p_witten, p_wittenberge, p_wittingen, p_wittlich, p_wittstock, p_witzenhausen, p_wolfenbuettel, p_wolfratshausen)
  private def chunk11: Seq[R] = Seq(p_worms, p_wusterhausen, p_zeil_am_main, p_zeven, p_zittau, p_zwickau)
  val pages: Seq[R] = chunk0 ++ chunk1 ++ chunk2 ++ chunk3 ++ chunk4 ++ chunk5 ++ chunk6 ++ chunk7 ++ chunk8 ++ chunk9 ++ chunk10 ++ chunk11

  /** Pages that no longer exist — the regions Germany's pages were until 2026-10
   *  among them — and the page now holding most of their venues: (slug, slug
   *  qualified with its Bundesland, the page's slug). */
  val retired: Seq[(String, String, String)] = Seq(
    ("altensteig", "altensteig-baden-wuerttemberg", "nagold"),
    ("asperg", "asperg-baden-wuerttemberg", "besigheim"),
    ("bad-doberan", "bad-doberan-mecklenburg-vorpommern", "kuehlungsborn"),
    ("bad-lobenstein", "bad-lobenstein-thueringen", "schleiz"),
    ("berchtesgaden", "berchtesgaden-bayern", "bad-reichenhall"),
    ("bernkastel-kues", "bernkastel-kues-rheinland-pfalz", "wittlich"),
    ("dorsten", "dorsten-nordrhein-westfalen", "marl"),
    ("ehingen", "ehingen-baden-wuerttemberg", "biberach-an-der-riss"),
    ("eschwege", "eschwege-hessen", "eisenach"),
    ("finsterwalde", "finsterwalde-brandenburg", "senftenberg"),
    ("fischbach-bei-dahn", "fischbach-bei-dahn-rheinland-pfalz", "pirmasens"),
    ("ginsheim-gustavsburg", "ginsheim-gustavsburg-hessen", "weiterstadt"),
    ("hachenburg", "hachenburg-rheinland-pfalz", "asbach"),
    ("itzehoe", "itzehoe-schleswig-holstein", "elmshorn"),
    ("kelkheim", "kelkheim-hessen", "hofheim-am-taunus"),
    ("korbach", "korbach-hessen", "meschede"),
    ("langenhagen", "langenhagen-niedersachsen", "garbsen"),
    ("langeoog", "langeoog-niedersachsen", "norderney"),
    ("lychen", "lychen-brandenburg", "templin"),
    ("marktoberdorf", "marktoberdorf-bayern", "kempten-im-allgaeu"),
    ("nettetal", "nettetal-nordrhein-westfalen", "viersen"),
    ("nonnweiler", "nonnweiler-saarland", "wadern"),
    ("regen", "regen-bayern", "deggendorf"),
    ("rotenburg-an-der-wuemme", "rotenburg-an-der-wuemme-niedersachsen", "achim"),
    ("sassnitz", "sassnitz-mecklenburg-vorpommern", "stralsund"),
    ("schleswig", "schleswig-schleswig-holstein", "rendsburg"),
    ("schluechtern", "schluechtern-hessen", "bad-kissingen"),
    ("schmelz", "schmelz-saarland", "merzig"),
    ("schwandorf", "schwandorf-bayern", "amberg"),
    ("sigmaringen", "sigmaringen-baden-wuerttemberg", "tuttlingen"),
    ("sosa", "sosa-sachsen", "auerbach"),
    ("uffenheim", "uffenheim-bayern", "rothenburg"),
    ("vechta", "vechta-niedersachsen", "cloppenburg"),
    ("versmold", "versmold-nordrhein-westfalen", "warendorf"),
    ("zeitz", "zeitz-sachsen-anhalt", "naumburg"),
  )

  /** The venues that screen only in season — open-air, summer and drive-in screens, by name
   *  (data/scripts/seasonal_venues.py): their display names as generated above. */
  val seasonal: Set[String] = Set(
    "ARTE Freiluftkino Kulturforum",
    "Autokino Bad Saulgau am Fesplatz",
    "Autokino Bünder Lichtspiele am Festplatz",
    "Autokino Dillingen",
    "Autokino Esslingen Neckar Center",
    "Autokino Greifensteine",
    "Autokino Usedom",
    "Autokino Zempow",
    "Autokino in Grasleben",
    "Bad Uracher Sommer Open Air",
    "Camillo-Sommerkino im Rathaushof",
    "Cinecittá Open Air",
    "Corona KinoPlex - Open Air",
    "Fellbacher Sommernachts-Open-Air-Kino",
    "Filmarena Open-Air Kino Theatervorplatz",
    "Filmnächte am Elbufer",
    "Filmnächte auf dem Theaterplatz",
    "Filmseher Open Air",
    "Freilichtbühne Sosa",
    "Freilichtbühne im Fürther Stadtpark",
    "Freiluftkino Friedrichshain",
    "Freiluftkino Hasenheide",
    "Freiluftkino Kreuzberg",
    "Freiluftkino Naturtheater Friedrichshagen",
    "Freiluftkino Neue Zukunft",
    "Freiluftkino Parkbühne Biesdorf",
    "Freiluftkino Rehberge",
    "Freiluftkino Waschhaus",
    "Freiluftkino am Spreebogen Bautzen",
    "Freiluftkino im Garten des Schillerhauses",
    "Gerniale Kino Open-Air",
    "Kasseler Open Air Sommerfilm",
    "Kino Central Open Air (Berlin)",
    "Kino Open Air Ingolstadt",
    "Kino-Open-Air",
    "LOTTO Hamburg SchanzenKino Open Air",
    "Lichtburg Open Air Kino",
    "Ludwigsburger Sommernachts Open Air Kino",
    "OPEN AIR KINO in der Erkenbert Ruine",
    "OPEN AIR auf dem Campus FH",
    "Odonisches Sommerkino",
    "Open Air",
    "Open Air Böblinger See",
    "Open Air Erlangen",
    "Open Air Fränkisches Museumseisenbahn",
    "Open Air Kieswerk",
    "Open Air Kino",
    "Open Air Kino Burg Hohenzollern",
    "Open Air Kino Freilichtbühne Desi",
    "Open Air Kino Furtwangen",
    "Open Air Kino Hainbad",
    "Open Air Kino Herrenberg",
    "Open Air Kino Im Rheinauenhafen",
    "Open Air Kino In der Kaiserpfalz Forchheim",
    "Open Air Kino Jülich",
    "Open Air Kino Katharinenruine",
    "Open Air Kino Krefelder Rennbahn",
    "Open Air Kino Landsberg",
    "Open Air Kino Lechflimmern Familienbad am Plärrer",
    "Open Air Kino Luna im Ravensberger Park",
    "Open Air Kino Marburg",
    "Open Air Kino Mauerflimmern",
    "Open Air Kino Naturgarten Bad",
    "Open Air Kino Ochsenzwinger Nördlingen",
    "Open Air Kino Parkbühne",
    "Open Air Kino Pellerhaus",
    "Open Air Kino Reisinger Anlage",
    "Open Air Kino Remscheid",
    "Open Air Kino Rotary-Benefiz",
    "Open Air Kino Schloss Wilhelmsburg",
    "Open Air Kino Spandau Innenhof Stadtbibliothek",
    "Open Air Kino Spitalhof Reutlingen",
    "Open Air Kino Winzerkeller Sommerach",
    "Open Air Kino am Mercedes-Benz Museum",
    "Open Air Kino am Olympiasee",
    "Open Air Kino am Rathaus Walldorf",
    "Open Air Kino am Schloß Gottesaue",
    "Open Air Kino an der Sportanlage Schneewinkl am Königssee",
    "Open Air Kino auf dem Dreiecksplatz",
    "Open Air Kino auf dem Marktplatz",
    "Open Air Kino im Bad Vilbeler Freibad",
    "Open Air Kino im Freibad",
    "Open Air Kino im Hof der Ortenburg",
    "Open Air Kino im Innenhof der Domschule Giro Live",
    "Open Air Kino im Kreuzgarten",
    "Open Air Kino im Schlösse Hof",
    "Open Air Kino im Stadion",
    "Open Air Kino in der Hetzel-Galerie",
    "Open Air Kino zwischen den Schanzen/Erdinger Arena",
    "Open Air Kino&quot;Am Kappelenberg&quot;",
    "Open Air Kino, Mond &amp; Sterne",
    "Open Air Krafftscher Hof",
    "Open Air Radrennbahn am Reichelsdorfer Keller",
    "Open Air Rudolzhofen Kino",
    "Open Air Schrozberg",
    "Open Air Seebühne",
    "Open Air Strandkorbkino am Bootshafen",
    "Open Air am B&amp;O Parkhotel",
    "Open Air am KuK",
    "Open Air am Landhaus Dessau",
    "Open Air am Lichthaus-Kino",
    "Open Air am Neuwerk",
    "Open Air an der Bleiche",
    "Open Air an der Gelben Villa",
    "Open Air an der schwarzen Mühle",
    "Open Air auf Schloss Freudenstein",
    "Open Air auf dem Theaterschiff",
    "Open Air im Badepark Nagold",
    "Open Air im Comma-Garten",
    "Open Air im Freibad Friedrichsdorf",
    "Open Air im Freilandmuseum",
    "Open Air im Hof",
    "Open Air im Kulturhof Krönbacken",
    "Open Air im Kurfürstlichen Schloss",
    "Open Air im STrandpark Wasserburg",
    "Open Air im Stadtpark",
    "Open Air im Theaterhof",
    "Open Air-Kino Waldshut",
    "Open-Air Kino Ballmertshofer Filmfest",
    "Open-Air Kino Schloss Strünkede",
    "Open-Air Kino Schwimmbad Neu-Ansbach",
    "Open-Air Kino im Freihof Gedenkstätte",
    "Open-Air Kino im Schlossgarten",
    "Open-Air Marktplatz Neue Stadtmitte",
    "Open-Air Mondscheinkino Waldbühne",
    "Open-Air Oberkirch",
    "Open-Air Spielwiese neben dem Rathaus",
    "Open-Air auf der Burg",
    "Open-Air-Kino - Waldbühne Neuwürschnitz",
    "Open-Air-Kino Brentanobad",
    "Open-Air-Kino Forchtenberg",
    "Open-Air-Kino Hofgeismar",
    "Open-Air-Kino Schloss-Innenhof",
    "Open-Air-Kino Schwerin",
    "Open-Air-Kino Vier Linden",
    "Open-Air-Kino auf der Burg",
    "Open-Air-Kino im Kulturhaus Osterfeld",
    "Open-Air-Kino im Landgrafenschloss",
    "Open-Air-Kino im Museumshof",
    "Open-Air-Kino im Nilkheimer Park",
    "Open-Air-Kino im TuFa-Hof",
    "Open-Air-Kino-Heilbronn",
    "Openair Kino",
    "Openair Zentralgasthof Weinböhla GmbH",
    "Outdoor Cine - Das Open Air Kino im Völkerkunde-Museum",
    "PSD Bank Autokino Dortmund",
    "Ringlokschuppen Open-Air-Kino auf der Drehscheibe",
    "Sommer Kino Nächte Ortenau",
    "SommerNachtFilmFestival - Marienbergpark",
    "Sommerkino Donzdorf",
    "Sommerkino Güstrow",
    "Sommerkino Heringsdorf",
    "Sommerkino Klöden",
    "Sommerkino Schauportal",
    "Sommerkino Zempin (SAK)",
    "Sommerkino auf dem Alsterdorfer Markt",
    "Sommerkino auf dem Cranach-Hof",
    "Sommerkino im Schloss Geyerswörth",
    "Sommernachts-Kino",
    "Sommernachtskino (Kirchheim unter Teck)",
    "Sommernachtskino (Löhne)",
    "Sommernachtskino Tübingen",
    "Sommernachtskino im Klosterhof in Oberndorf",
    "Sommernachtskino im Van-Ameren Bad",
    "Spremberger Filmnächte",
    "Stadtwerke Sommerkino",
    "Talflimmern Open-Air-Kino",
    "Volksplatz Borna Open Air Kino",
    "Zittauer Filmnächte",
  )
}
