// GENERATED from data/germany/regions.json by the DE roster generator — do NOT edit by hand.
// Full German cinema roster: 158 regions over 16 Bundesländer /
// 1,518 cinemas (Filmstarts). Regenerate with
// the generator in data/germany/scripts after re-harvesting; see data/germany/README.md.
package models

private[models] object GermanRosterData {
  // (displayName, pillName, filmstarts theaterId, kinoprogramm.com fallback path)
  type C = (String, String, String, Option[String])
  // (slug, name, bundesland, lat, lon, cities, cinemas)
  type R = (String, String, String, Double, Double, Seq[String], Seq[C])

  private def r_berlin: R = ("berlin", "Berlin", "Berlin", 52.52437, 13.41053, Seq("Berlin", "Potsdam", "Oranienburg", "Buckow", "Falkensee", "Bernau", "Königs Wusterhausen", "Kleinmachnow", "Erkner", "Wildau"), Seq(
    ("ARTE Freiluftkino Kulturforum", "ARTE Freiluftkino Kulturforum", "A1807", None),
    ("Acud Kino", "Acud Kino", "A0006", Some("/kino/berlin/acud-kino-31396")),
    ("Adria Filmtheater Steglitz", "Adria Filmtheater Steglitz", "A0008", Some("/kino/berlin/adria-filmtheater-31359")),
    ("Akademie der Künste", "Akademie der Künste", "A2830", None),
    ("Arsenal Berlin", "Arsenal Berlin", "A0050", Some("/kino/berlin/arsenal-institut-fuer-film-und-videokunst-e.v.-31387")),
    ("Astor Film Lounge", "Astor Film Lounge", "A0514", Some("/kino/berlin/astor-film-lounge-31385")),
    ("Astra-Filmpalast", "Astra-Filmpalast", "A0056", Some("/kino/berlin/astrafilmpalast-31400")),
    ("B-Ware Ladenkino", "B-Ware Ladenkino", "A2620", Some("/kino/berlin/bware-ladenkino-80720")),
    ("Babylon (Kreuzberg)", "Babylon (Kreuzberg)", "A0075", Some("/kino/berlin/babylon-kreuzberg-31447")),
    ("Babylon (Mitte)", "Babylon (Mitte)", "A1489", Some("/kino/berlin/babylon-64256")),
    ("Bali-Kino", "Bali-Kino", "A0079", Some("/kino/berlin/balikino-31391")),
    ("Blauer Stern", "Blauer Stern", "A0099", Some("/kino/berlin/blauer-stern-31389")),
    ("Brotfabrik im Kunst- &amp; Kulturzentrum", "Brotfabrik im Kunst- &amp; Kulturzentrum", "A0111", Some("/kino/berlin/brotfabrikkino-31392")),
    ("Bundesplatz- Studio", "Bundesplatz- Studio", "A0114", Some("/kino/berlin/bundesplatzkino-31366")),
    ("Capitol Dahlem", "Capitol Dahlem", "A0151", Some("/kino/berlin/capitol-dahlem-31438")),
    ("Casablanca Berlin", "Casablanca Berlin", "A0171", Some("/kino/berlin/casablanca-34710")),
    ("CineStar Berlin - Cubix am Alexanderplatz", "CineStar Berlin - Cubix am Alexanderplatz", "A0332", Some("/kino/berlin/cinestar-cubix-am-alexanderplatz-46796")),
    ("CineStar Berlin - Hellersdorf", "CineStar Berlin - Hellersdorf", "A0373", Some("/kino/berlin/cinestar-hellersdorf-34961")),
    ("CineStar Berlin - Kino in der Kulturbrauerei", "CineStar Berlin - Kino in der Kulturbrauerei", "A0738", Some("/kino/berlin/cinestar-kino-in-der-kulturbrauerei-39607")),
    ("CineStar Berlin - Tegel", "CineStar Berlin - Tegel", "A0369", Some("/kino/berlin/cinestar-berlin-tegel-37654")),
    ("Cinema Paris", "Cinema Paris", "A0252", Some("/kino/berlin/cinema-paris-31368")),
    ("CinemaxX Potsdamer Platz", "CinemaxX Potsdamer Platz", "A0275", Some("/kino/berlin/cinemaxx-36643")),
    ("Cineplex Alhambra", "Cineplex Alhambra", "A0015", None),
    ("Cineplex Neukölln", "Cineplex Neukölln", "A0647", Some("/kino/berlin/cineplex-neukoelln-arcaden-52857")),
    ("Cineplex Spandau", "Cineplex Spandau", "A0316", Some("/kino/berlin/cineplex-spandau-52876")),
    ("Cineplex Titania", "Cineplex Titania", "A1184", Some("/kino/berlin/cineplex-titania-72776")),
    ("City Kino Wedding", "City Kino Wedding", "A2672", Some("/kino/berlin/city-kino-wedding-85657")),
    ("Colosseum Filmtheater Berlin", "Colosseum Filmtheater Berlin", "A1207", Some("/kino/berlin/colosseum-kino-35184")),
    ("Cosima-Filmtheater", "Cosima-Filmtheater", "A0427", Some("/kino/berlin/cosima-31367")),
    ("Czech Centre / Tschechische Zentrum Berlin", "Czech Centre / Tschechische Zentrum Berlin", "A2834", None),
    ("Delphi Filmpalast am Zoo", "Delphi Filmpalast am Zoo", "A0435", Some("/kino/berlin/delphi-filmpalast-am-zoo-31370")),
    ("Delphi LUX", "Delphi LUX", "A2870", Some("/kino/berlin/delphi-lux-90397")),
    ("Eva-Lichtspiele", "Eva-Lichtspiele", "A0476", Some("/kino/berlin/evalichtspiele-31410")),
    ("Filmkunst 66", "Filmkunst 66", "A0503", Some("/kino/berlin/filmkunst-66-31357")),
    ("Filmrauschpalast im Kulturfabrik Moabit", "Filmrauschpalast im Kulturfabrik Moabit", "A0523", Some("/kino/berlin/filmrauschpalast-im-kulturfabrik-moabit-31382")),
    ("Filmtheater am Friedrichshain", "Filmtheater am Friedrichshain", "A0479", Some("/kino/berlin/filmtheater-am-friedrichshain-31436")),
    ("Freiheit fünfzehn - kino im liegestuh", "Freiheit fünfzehn - kino im liegestuh", "A2840", None),
    ("Freiluftkino Friedrichshain", "Freiluftkino Friedrichshain", "A1805", None),
    ("Freiluftkino Hasenheide", "Freiluftkino Hasenheide", "A1784", Some("/kino/berlin/freiluftkino-hasenheide-61899")),
    ("Freiluftkino Kreuzberg", "Freiluftkino Kreuzberg", "A2198", Some("/kino/berlin/freiluftkino-kreuzberg-31380")),
    ("Freiluftkino Naturtheater Friedrichshagen", "Freiluftkino Naturtheater Friedrichshagen", "A1808", None),
    ("Freiluftkino Neue Zukunft", "Freiluftkino Neue Zukunft", "G02A8", Some("/kino/berlin/freiluftkino-neue-zukunft-93119")),
    ("Freiluftkino Parkbühne Biesdorf", "Freiluftkino Parkbühne Biesdorf", "A1809", None),
    ("Freiluftkino Rehberge", "Freiluftkino Rehberge", "A2739", Some("/kino/berlin/freiluftkino-rehberge-76002")),
    ("Hackesche Höfe Kino", "Hackesche Höfe Kino", "A0597", Some("/kino/berlin/hackesche-hoefe-kino-34705")),
    ("IL KINO", "IL KINO", "A2674", Some("/kino/berlin/il-kino-85697")),
    ("Instituto Cervantes - Berlin", "Instituto Cervantes - Berlin", "A2907", None),
    ("Kant Kino Charlottenburg", "Kant Kino Charlottenburg", "A0645", None),
    ("Kino Central Berlin", "Kino Central Berlin", "A0185", Some("/kino/berlin/centralkino-31390")),
    ("Kino Central Open Air (Berlin)", "Kino Central Open Air (Berlin)", "A2760", Some("/kino/berlin/centralkino-open-air-berlin-90497")),
    ("Kino International Berlin", "Kino International Berlin", "A0625", Some("/kino/berlin/international-31440")),
    ("Kino Intimes", "Kino Intimes", "A0627", Some("/kino/berlin/kino-intimes-34709")),
    ("Kino Krokodil", "Kino Krokodil", "A0745", Some("/kino/berlin/kino-krokodil-66296")),
    ("Kino Spreehöfe Berlin", "Kino Spreehöfe Berlin", "A0753", None),
    ("Kino Zukunft", "Kino Zukunft", "A2660", Some("/kino/berlin/kino-zukunft-92976")),
    ("Kino im Kulturhaus Spandau", "Kino im Kulturhaus Spandau", "A1734", Some("/kino/berlin/kino-im-kulturhaus-spandau-60817")),
    ("Klick Kino", "Klick Kino", "A2751", Some("/kino/berlin/klick-kino-89117")),
    ("Lichtblick-Kino", "Lichtblick-Kino", "A0901", Some("/kino/berlin/lichtblickkino-37315")),
    ("Moviemento", "Moviemento", "A0970", Some("/kino/berlin/moviemento-31412")),
    ("Neues Off", "Neues Off", "A0990", Some("/kino/berlin/neues-off-31449")),
    ("Odeon", "Odeon", "A1577", Some("/kino/berlin/odeon-31445")),
    ("Open Air Kino Spandau Innenhof Stadtbibliothek", "Open Air Kino Spandau Innenhof Stadtbibliothek", "A2276", None),
    ("Passage Kino Neukölln", "Passage Kino Neukölln", "A1035", Some("/kino/berlin/passage-kinos-31448")),
    ("Regenbogen Kino", "Regenbogen Kino", "A1049", Some("/kino/berlin/regenbogen-kino-36313")),
    ("Rollberg Kino", "Rollberg Kino", "A1083", Some("/kino/berlin/rollberg-kino-31437")),
    ("Sputnik Südstern", "Sputnik Südstern", "A1145", Some("/kino/berlin/sputnik-suedstern-73680")),
    ("Thalia Berlin", "Thalia Berlin", "A1177", Some("/kino/berlin/thalia-53676")),
    ("Tilsiter-Lichtspiele", "Tilsiter-Lichtspiele", "A1183", Some("/kino/berlin/tilsiterlichtspiele-31421")),
    ("Toni und Tonino", "Toni und Tonino", "A1187", Some("/kino/berlin/toni-und-tonino-31431")),
    ("UCI East Side Gallery", "UCI East Side Gallery", "A2906", Some("/kino/berlin/uci-kinowelt-berlin-east-side-gallery-%7C-luxe-91577")),
    ("UCI Gropius Passagen", "UCI Gropius Passagen", "A1210", Some("/kino/berlin/uci-kinowelt-gropius-passagen-31427")),
    ("UCI am Eastgate", "UCI am Eastgate", "A0667", Some("/kino/berlin/uci-kinowelt-am-eastgate-37709")),
    ("Union Filmtheater Friedrichshagen", "Union Filmtheater Friedrichshagen", "A1230", Some("/kino/berlin/union-filmtheater-friedrichshagen-34712")),
    ("Urania-Filmbühne", "Urania-Filmbühne", "A1248", Some("/kino/berlin/uraniafilmbuehne-39646")),
    ("Wolf Kino", "Wolf Kino", "A2749", Some("/kino/berlin/wolf-kino-berlin-89057")),
    ("Xenon", "Xenon", "A1284", Some("/kino/berlin/xenon-31435")),
    ("Yorck Kino (Kreuzberg)", "Yorck Kino (Kreuzberg)", "A0995", Some("/kino/berlin/yorck-kinos-31446")),
    ("Z-inema im Z-Bar", "Z-inema im Z-Bar", "A2374", Some("/kino/berlin/zinema-im-zbar-75057")),
    ("ZOO PALAST Berlin", "ZOO PALAST Berlin", "A1215", Some("/kino/berlin/zoopalast-31428")),
    ("Zeughauskino - Deutsches Historisches Museum", "Zeughauskino - Deutsches Historisches Museum", "A1290", Some("/kino/berlin/zeughauskino-deutsches-historisches-museum-31375")),
    ("fsk am Oranienplatz", "fsk am Oranienplatz", "A0564", Some("/kino/berlin/fsk-am-oranienplatz-31388")),
    ("CineMotion Berlin Hohenschönhausen", "CineMotion Berlin Hohenschönhausen", "A0290", Some("/kino/berlin/cinemotion-berlin-hohenschoenhausen-37070")),
    ("Filmmuseum Potsdam", "Filmmuseum Potsdam", "A0506", Some("/kino/potsdam/filmmuseum-31635")),
    ("Freilichtkino in der Russischen Kolonie Alexandrowka", "Freilichtkino in der Russischen Kolonie Alexandrowka", "A2152", None),
    ("Freiluftkino Waschhaus", "Freiluftkino Waschhaus", "A1811", Some("/kino/potsdam/freiluftkino-waschhaus-67017")),
    ("Inselkino auf der Freundschaftsinsel", "Inselkino auf der Freundschaftsinsel", "A1932", None),
    ("Thalia Arthouse-Kino", "Thalia Arthouse-Kino", "A1175", Some("/kino/potsdam/thalia-arthousekino-babelsberg-52736")),
    ("UCI Potsdam", "UCI Potsdam", "A1206", Some("/kino/potsdam/uci-kinowelt-potsdam-43185")),
    ("Filmpalast Oranienburg", "Filmpalast Oranienburg", "A0518", Some("/kino/oranienburg/filmpalast-31627")),
    ("Park-Theater Buckow", "Park-Theater Buckow", "A1491", None),
    ("ALA Kino am Falkensee", "ALA Kino am Falkensee", "A0013", None),
    ("Filmpalast Bernau bei Berlin", "Filmpalast Bernau bei Berlin", "A0512", Some("/kino/bernau/filmpalast-31452")),
    ("Capitol Königs Wusterhausen", "Capitol Königs Wusterhausen", "A0144", Some("/kino/koenigs-wusterhausen/capitol-das-kulturkino-61765")),
    ("Kammerspiele Kleinmachnow", "Kammerspiele Kleinmachnow", "A0642", Some("/kino/kleinmachnow/neue-kammerspiele-31570")),
    ("Movieland", "Movieland", "A0671", Some("/kino/erkner/movieland-76362")),
    ("CineStar Wildau", "CineStar Wildau", "A0330", Some("/kino/wildau-bei-koenigs-wusterhausen/cinestar-38777"))
  ))
  private def r_frankfurt_am_main: R = ("frankfurt-am-main", "Frankfurt am Main", "Hessen", 50.11552, 8.68417, Seq("Frankfurt am Main", "Wiesbaden", "Mainz", "Darmstadt", "Offenbach am Main", "Rodgau", "Dreieich", "Hofheim am Taunus", "Bad Vilbel", "Dietzenbach", "Mörfelden-Walldorf", "Kelkheim", "Friedrichsdorf", "Ginsheim-Gustavsburg", "Frankfurt", "Hanau", "Langen", "Neu-Isenburg", "Kelkheim (Taunus)", "Weiterstadt", "Idstein", "Pfungstadt", "Gross-Gerau", "Bad Soden am Taunus", "Eschborn", "Karben", "Nidderau", "Kronberg im Taunus", "Eppstein", "Rödermark"), Seq(
    ("ASTOR Film Lounge MyZeil", "ASTOR Film Lounge MyZeil", "A2913", Some("/kino/frankfurt-am-main/astor-film-lounge-myzeill-81761")),
    ("CineStar Frankfurt am Main - Metropolis", "CineStar Frankfurt am Main - Metropolis", "A0378", Some("/kino/frankfurt-am-main/cinestar-metropolis-43378")),
    ("Cinema am Rossmarkt", "Cinema am Rossmarkt", "A0261", None),
    ("Eldorado Frankfurt am Main", "Eldorado Frankfurt am Main", "A0460", Some("/kino/frankfurt-am-main/eldorado-32028")),
    ("Filmforum Höchst", "Filmforum Höchst", "A1416", Some("/kino/frankfurt-am-main/filmforum-hoechst-32038")),
    ("Harmonie in Sachsenhausen", "Harmonie in Sachsenhausen", "A1450", Some("/kino/frankfurt-am-main/harmonie-32026")),
    ("Kino im DFF (Filmmuseum)", "Kino im DFF (Filmmuseum)", "A0718", Some("/kino/frankfurt-am-main/kino-im-dff-deutsches-filminstitut-und-filmmuseum-32025")),
    ("Mal seh&#039;n Kino", "Mal seh&#039;n Kino", "A1469", None),
    ("Open-Air-Kino Brentanobad", "Open-Air-Kino Brentanobad", "A1853", None),
    ("Orfeos Erben", "Orfeos Erben", "A1019", Some("/kino/frankfurt-am-main/orfeos-erben-39664")),
    ("Pupille e.V. - Kino in der Uni", "Pupille e.V. - Kino in der Uni", "A1048", Some("/kino/frankfurt-am-main/pupille-kino-in-der-uni-36896")),
    ("naxos.Kino im Theater Willy Praml", "naxos.Kino im Theater Willy Praml", "A2632", Some("/kino/frankfurt-am-main/naxos.kino-theater-willy-praml-80757")),
    ("Caligari FilmBühne Wiesbaden", "Caligari FilmBühne Wiesbaden", "A0134", Some("/kino/wiesbaden/caligari-filmbuehne-40177")),
    ("Cineplex Apollo-Center Wiesbaden", "Cineplex Apollo-Center Wiesbaden", "A0019", None),
    ("Cineplex Arkaden Wiesbaden", "Cineplex Arkaden Wiesbaden", "A0007", None),
    ("Filme im Schloss", "Filme im Schloss", "A2320", Some("/kino/wiesbaden/filme-im-schloss-74457")),
    ("Murnau-Filmtheater", "Murnau-Filmtheater", "A2631", Some("/kino/wiesbaden/murnaufilmtheater-77605")),
    ("Open Air Kino Reisinger Anlage", "Open Air Kino Reisinger Anlage", "A2424", Some("/kino/wiesbaden/open-air-kino-in-den-reisingeranlagen-75457")),
    ("Capitol Arthouse Mainz", "Capitol Arthouse Mainz", "A1331", Some("/kino/mainz-am-rhein/capitol-32160")),
    ("CineStar Mainz", "CineStar Mainz", "A0352", None),
    ("Cinémayence", "Cinémayence", "A0018", Some("/kino/mainz-am-rhein/cin%C3%A9mayence-32158")),
    ("KlubKino", "KlubKino", "A2853", None),
    ("Open Air im Kurfürstlichen Schloss", "Open Air im Kurfürstlichen Schloss", "A2866", None),
    ("Citydome Darmstadt", "Citydome Darmstadt", "A0483", Some("/kino/darmstadt/citydome-32006")),
    ("Kinopolis Darmstadt", "Kinopolis Darmstadt", "A0268", Some("/kino/darmstadt/kinopolis-39358")),
    ("Studentischer Filmkreis im Audimaxx der TU Darmstadt", "Studentischer Filmkreis im Audimaxx der TU Darmstadt", "A2665", None),
    ("programmkino rex", "programmkino rex", "A1597", None),
    ("CinemaxX Offenbach", "CinemaxX Offenbach", "A0264", Some("/kino/offenbach-am-main/cinemaxx-38829")),
    ("Hafenkino - Programmkino im Kulturzentrum Hafen 2", "Hafenkino - Programmkino im Kulturzentrum Hafen 2", "A2691", Some("/kino/offenbach-am-main/hafenkino-programmkino-im-kulturzentrum-hafen-2-86217")),
    ("Kronen-Lichtspiele Rodgau", "Kronen-Lichtspiele Rodgau", "A0256", Some("/kino/rodgau/kronenlichtspiele-39857")),
    ("Saalbau-Lichtspiele Rodgau", "Saalbau-Lichtspiele Rodgau", "A1099", Some("/kino/rodgau/saalbaulichtspiele-32228")),
    ("Rex-Palast", "Rex-Palast", "A1074", Some("/kino/dreieich/rex-palast-32013")),
    ("Viktoria-Theater", "Viktoria-Theater", "A1257", Some("/kino/dreieich/viktoriatheater-53758")),
    ("Filmpalast Hofheim", "Filmpalast Hofheim", "A2641", Some("/kino/hofheim-am-taunus/filmpalast-78837")),
    ("Open-Air Spielwiese neben dem Rathaus", "Open-Air Spielwiese neben dem Rathaus", "A2458", None),
    ("Kino Alte Mühle", "Kino Alte Mühle", "A0700", Some("/kino/bad-vilbel/kino-alte-muehle-44496")),
    ("Open Air Kino im Bad Vilbeler Freibad", "Open Air Kino im Bad Vilbeler Freibad", "A1933", Some("/kino/bad-vilbel/open-air-kino-im-bad-vilbeler-freibad-36307")),
    ("Main Kino D", "Main Kino D", "A0658", None),
    ("Maingau - Open - Air - Kino", "Maingau - Open - Air - Kino", "A2627", None),
    ("LichtBlick Mörfelden-Walldorf", "LichtBlick Mörfelden-Walldorf", "A0900", Some("/kino/moerfeldenwalldorf/lichtblick-32185")),
    ("Open Air Kino am Rathaus Walldorf", "Open Air Kino am Rathaus Walldorf", "A2256", None),
    ("Kino-Kelkheim", "Kino-Kelkheim", "A1510", Some("/kino/kelkheim/kinokelkheim-32123")),
    ("Open Air Kelkheim", "Open Air Kelkheim", "A2767", None),
    ("Filmtheater Friedrichsdorf", "Filmtheater Friedrichsdorf", "A1427", Some("/kino/friedrichsdorf/filmtheater-32051")),
    ("Open Air im Freibad Friedrichsdorf", "Open Air im Freibad Friedrichsdorf", "A2281", Some("/kino/friedrichsdorf/open-air-im-freibad-friedrichsdorf-55257")),
    ("Burg-Lichtspiele (Ginsheim-Gustavsburg)", "Burg-Lichtspiele (Ginsheim-Gustavsburg)", "A0868", Some("/kino/ginsheimgustavsburg/burglichtspiele-32066")),
    ("Open Air Kino Ginsheim-Gustavsburg", "Open Air Kino Ginsheim-Gustavsburg", "A2084", None),
    ("Instituto Cervantes - Frankfurt", "Instituto Cervantes - Frankfurt", "A2908", None),
    ("Kinopolis Hanau", "Kinopolis Hanau", "A2613", Some("/kino/hanau/kinopolis-80297")),
    ("Lichtburg Kinos", "Lichtburg Kinos", "A1534", None),
    ("Drive In Autokino Gravenbruch", "Drive In Autokino Gravenbruch", "A1398", Some("/kino/neuisenburg/drive-in-autokino-frankfurt-gravenbruch-32193")),
    ("Open-Air Marktplatz Neue Stadtmitte", "Open-Air Marktplatz Neue Stadtmitte", "A2204", None),
    ("Kommunales Kino Weiterstadt im Bürgerzentrum", "Kommunales Kino Weiterstadt im Bürgerzentrum", "A0858", Some("/kino/weiterstadt/kommunales-kino-im-buergerzentrum-34894")),
    ("Saalbau-Lichtspiele Pfungstadt", "Saalbau-Lichtspiele Pfungstadt", "A1098", Some("/kino/pfungstadt/saalbau-kino-32219")),
    ("Lichtspielhaus Groß-Gerau", "Lichtspielhaus Groß-Gerau", "A0913", None),
    ("CasaBlanca Art House", "CasaBlanca Art House", "A1531", Some("/kino/bad-soden-am-taunus/kino-casablanca-art-house-33116")),
    ("Eschborn K", "Eschborn K", "A2898", Some("/kino/eschborn/eschborn-k-75177")),
    ("Cinepark Karben", "Cinepark Karben", "A0303", Some("/kino/karben/cinepark-38103")),
    ("Luxor Filmpalast Nidderau", "Luxor Filmpalast Nidderau", "A0934", Some("/kino/nidderau/luxor-filmpalast-65497")),
    ("Kronberger Lichtspiele", "Kronberger Lichtspiele", "A0872", Some("/kino/kronberg-im-taunus/kronberger-lichtspiele-81117")),
    ("Open-Air auf der Burg", "Open-Air auf der Burg", "A2283", None),
    ("Neue Lichtspiele", "Neue Lichtspiele", "A1571", Some("/kino/roedermark/neue-lichtspiele-32227"))
  ))
  private def r_stuttgart: R = ("stuttgart", "Stuttgart", "Baden-Württemberg", 48.78232, 9.17702, Seq("Stuttgart", "Ludwigsburg", "Tübingen", "Reutlingen", "Böblingen", "Kirchheim unter Teck", "Esslingen", "Esslingen am Neckar", "Fellbach", "Backnang", "Kornwestheim", "Weil der Stadt", "Erdmannhausen", "Sindelfingen", "Waiblingen", "Leonberg", "Winnenden", "Weinstadt", "Mühlacker", "Renningen", "Marbach am Neckar", "Plochingen", "Asperg", "Korb", "Waldenbuch", "Allmersbach im Tal", "Oberriexingen"), Seq(
    ("Atelier am Bollwerk", "Atelier am Bollwerk", "A0063", Some("/kino/stuttgart/atelier-am-bollwerk-34850")),
    ("Cinema Stuttgart", "Cinema Stuttgart", "A0233", Some("/kino/stuttgart/cinema-62218")),
    ("CinemaxX SI-Centrum Stuttgart", "CinemaxX SI-Centrum Stuttgart", "A0942", Some("/kino/stuttgart/cinemaxx-im-sicentrum-35145")),
    ("Cinemaxx Liederhalle Stuttgart", "Cinemaxx Liederhalle Stuttgart", "A0297", Some("/kino/stuttgart/cinemaxx-stuttgart-an-der-liederhalle-42697")),
    ("Corso Stuttgart", "Corso Stuttgart", "A1386", Some("/kino/stuttgart/corso-33017")),
    ("Delphi 1+2", "Delphi 1+2", "A1389", None),
    ("EM", "EM", "A1402", Some("/kino/stuttgart/em-33452")),
    ("Gloria Stuttgart", "Gloria Stuttgart", "A1437", Some("/kino/stuttgart/gloria-33011")),
    ("Kinothek", "Kinothek", "A0840", Some("/kino/stuttgart/kinothek-72656")),
    ("Metropol Stuttgart", "Metropol Stuttgart", "A0948", Some("/kino/stuttgart/metropol-39257")),
    ("Open Air Kino am Mercedes-Benz Museum", "Open Air Kino am Mercedes-Benz Museum", "A2811", Some("/kino/stuttgart/open-air-kino-im-mercedesbenz-museum-90417")),
    ("Caligari Ludwigsburg", "Caligari Ludwigsburg", "A1324", Some("/kino/ludwigsburg-wuerttemberg/caligari-32837")),
    ("Central Theater Ludwigsburg", "Central Theater Ludwigsburg", "A0195", Some("/kino/ludwigsburg-wuerttemberg/central-theater-32839")),
    ("Ludwigsburger Sommernachts Open Air Kino", "Ludwigsburger Sommernachts Open Air Kino", "A1944", Some("/kino/ludwigsburg-wuerttemberg/ludwigsburger-sommernachts-open-air-kino-76083")),
    ("Luna Lichtspieltheater", "Luna Lichtspieltheater", "A0928", Some("/kino/ludwigsburg-wuerttemberg/luna-lichtspieltheater-39661")),
    ("Scala-Kino Ludwigsburg", "Scala-Kino Ludwigsburg", "A1108", Some("/kino/ludwigsburg-wuerttemberg/scalakino-32838")),
    ("Union-Theater Ludwigsburg", "Union-Theater Ludwigsburg", "A1238", Some("/kino/ludwigsburg-wuerttemberg/uniontheater-32840")),
    ("Atelier Tübingen", "Atelier Tübingen", "A1301", Some("/kino/tuebingen/atelier-33122")),
    ("Filmtheater Blaue Brücke", "Filmtheater Blaue Brücke", "A0683", Some("/kino/tuebingen/blaue-bruecke-70798")),
    ("Museum-Lichtspiele Tübingen", "Museum-Lichtspiele Tübingen", "A0691", Some("/kino/tuebingen/kino-museum-34641")),
    ("Sommernachtskino Tübingen", "Sommernachtskino Tübingen", "A1915", Some("/kino/tuebingen/sommernachtskino-55519")),
    ("Cineplex Planie Reutlingen", "Cineplex Planie Reutlingen", "A1038", Some("/kino/reutlingen/cineplex-planie-32958")),
    ("Kamino", "Kamino", "A2717", Some("/kino/reutlingen/kamino-87097")),
    ("Open Air Kino Spitalhof Reutlingen", "Open Air Kino Spitalhof Reutlingen", "A2518", None),
    ("Filmzentrum Bären", "Filmzentrum Bären", "A0953", Some("/kino/boeblingen/filmzentrum-baeren-32704")),
    ("Metropol am Postplatz", "Metropol am Postplatz", "A2741", Some("/kino/boeblingen/metropol-am-postplatz-88557")),
    ("Open Air Böblinger See", "Open Air Böblinger See", "A2035", None),
    ("Kommunales Kino Kirchheim unter Teck", "Kommunales Kino Kirchheim unter Teck", "A1692", None),
    ("Sommernachtskino (Kirchheim unter Teck)", "Sommernachtskino (Kirchheim unter Teck)", "A2823", None),
    ("Tyroler-Lichtspiele", "Tyroler-Lichtspiele", "A1197", Some("/kino/kirchheim-unter-teck/tyrolerlichtspiele-32813")),
    ("Autokino Esslingen Neckar Center", "Autokino Esslingen Neckar Center", "A2928", Some("/kino/esslingen-am-neckar/autokino-im-neckar-center-92153")),
    ("Open-Air-Kino auf der Burg", "Open-Air-Kino auf der Burg", "A1847", Some("/kino/esslingen-am-neckar/openairkino-auf-der-burg-36365")),
    ("Kommunales Kino Esslingen", "Kommunales Kino Esslingen", "A1520", Some("/kino/esslingen-am-neckar/kommunales-kino-32745")),
    ("Traumpalast Esslingen", "Traumpalast Esslingen", "A0076", Some("/kino/esslingen-am-neckar/traumpalast-66136")),
    ("Fellbacher Sommernachts-Open-Air-Kino", "Fellbacher Sommernachts-Open-Air-Kino", "A1925", None),
    ("Kinokult Orfeo-Kino", "Kinokult Orfeo-Kino", "A1021", Some("/kino/fellbach/orfeo-36550")),
    ("Kino Universum Backnang", "Kino Universum Backnang", "A1656", Some("/kino/backnang/kino-universum-32680")),
    ("Traumpalast Backnang", "Traumpalast Backnang", "A1430", Some("/kino/backnang/traumpalast-backnang-32679")),
    ("Capitol Lichtspiele Kornwestheim", "Capitol Lichtspiele Kornwestheim", "A0145", Some("/kino/kornwestheim/capitol-lichtspiele-33126")),
    ("Drive In Autokino Kornwestheim", "Drive In Autokino Kornwestheim", "A1921", Some("/kino/kornwestheim/drive-in-autokino-32814")),
    ("Kinocenter", "Kinocenter", "A2771", None),
    ("Kulisse", "Kulisse", "A2806", Some("/kino/weil-der-stadt/kino-kulisse-69417")),
    ("Kinomobil Stuttgart - Astrid-Lindgren-Schule", "Kinomobil Stuttgart - Astrid-Lindgren-Schule", "A0813", None),
    ("Kinomobil Stuttgart - Schulhof Astrid-Lindgren-Schule", "Kinomobil Stuttgart - Schulhof Astrid-Lindgren-Schule", "A1952", None),
    ("CinemaxX Sindelfingen", "CinemaxX Sindelfingen", "A2875", Some("/kino/sindelfingen/cinemaxx-sindelfingen-38488")),
    ("Traumpalast Waiblingen", "Traumpalast Waiblingen", "A0480", Some("/kino/waiblingen/traumpalast-66138")),
    ("Traumpalast Leonberg", "Traumpalast Leonberg", "A2738", Some("/kino/leonberg/traumpalast-leonberg-88177")),
    ("Olympia Winnenden", "Olympia Winnenden", "A1578", Some("/kino/winnenden/olympia-33066")),
    ("Kommunales Kino Weinstadt", "Kommunales Kino Weinstadt", "A2856", None),
    ("Scala Filmtheater Mühlacker", "Scala Filmtheater Mühlacker", "A1617", Some("/kino/muehlacker/scalafilmtheater-32854")),
    ("Kinomobil Stuttgart - Festhalle Stegwiesen", "Kinomobil Stuttgart - Festhalle Stegwiesen", "A0792", Some("/kino/renningen/kinomobil-stuttgart-festhalle-stegwiesen-41591")),
    ("Kinomobil Stuttgart - Jugend-Kultur-Haus planet x", "Kinomobil Stuttgart - Jugend-Kultur-Haus planet x", "A0784", None),
    ("Union-Theater Plochingen", "Union-Theater Plochingen", "A1654", Some("/kino/plochingen/uniontheater-32941")),
    ("MoKi Ludwigsburg", "MoKi Ludwigsburg", "A2859", None),
    ("Kinomobil Stuttgart - Alte Kelter Korb", "Kinomobil Stuttgart - Alte Kelter Korb", "A0796", Some("/kino/korb/kinomobil-stuttgart-alte-kelter-40623")),
    ("Kinomobil Stuttgart - Haus der Begegnung Waldenbuch", "Kinomobil Stuttgart - Haus der Begegnung Waldenbuch", "A2171", None),
    ("Kinomobil Stuttgart - Bürgersaal", "Kinomobil Stuttgart - Bürgersaal", "A2557", None),
    ("Kinomobil Stuttgart - Festhalle Oberriexingen", "Kinomobil Stuttgart - Festhalle Oberriexingen", "A1938", None)
  ))
  private def r_koeln: R = ("koeln", "Köln", "Nordrhein-Westfalen", 50.93333, 6.95, Seq("Köln", "Düsseldorf", "Bonn", "Leverkusen", "Hürth", "Kerpen", "Langenfeld", "Siegburg", "Bergisch Gladbach", "Troisdorf", "Sankt Augustin", "Euskirchen", "Frechen", "Hennef", "Brühl", "Monheim am Rhein", "Kalk"), Seq(
    ("Cinedom", "Cinedom", "A0027", Some("/kino/koeln/cinedom-31189")),
    ("Cinenova", "Cinenova", "A0298", Some("/kino/koeln/cinenova-33237")),
    ("Cineplex Filmpalast Köln", "Cineplex Filmpalast Köln", "A0031", Some("/kino/koeln/cineplex-koeln-88897")),
    ("Filmclub 813", "Filmclub 813", "A0737", Some("/kino/koeln/filmclub-813-61197")),
    ("Filmforum im Museum Ludwig", "Filmforum im Museum Ludwig", "A2139", Some("/kino/koeln/filmforum-im-museum-ludwig-73057")),
    ("Filmhaus Kino Köln", "Filmhaus Kino Köln", "A0499", Some("/kino/koeln/filmhaus-kino-30858")),
    ("Filmpalette", "Filmpalette", "A0522", Some("/kino/koeln/filmpalette-31837")),
    ("Japanisches Kulturinstitut", "Japanisches Kulturinstitut", "A0629", Some("/kino/koeln/japanisches-kulturinstitut-65837")),
    ("Kino Weißhaus", "Kino Weißhaus", "A0756", Some("/kino/koeln/kino-weisshaus-31843")),
    ("Metropolis Köln", "Metropolis Köln", "A0959", Some("/kino/koeln/metropolis-31845")),
    ("Odeon Köln", "Odeon Köln", "A1004", Some("/kino/koeln/odeon-31847")),
    ("Odonisches Sommerkino", "Odonisches Sommerkino", "A2175", None),
    ("Off Broadway", "Off Broadway", "A1009", Some("/kino/koeln/off-broadway-31839")),
    ("Open Air Kino Im Rheinauenhafen", "Open Air Kino Im Rheinauenhafen", "A2436", None),
    ("Residenz - Astor Filmlounge", "Residenz - Astor Filmlounge", "A2616", Some("/kino/koeln/residenz-34498")),
    ("Rex am Ring", "Rex am Ring", "A1704", None),
    ("Turistarama", "Turistarama", "A2687", Some("/kino/koeln/turistarama-86197")),
    ("Atelier Kino im Savoy-Theater", "Atelier Kino im Savoy-Theater", "A0064", Some("/kino/duesseldorf/atelier-kino-im-savoytheater-365")),
    ("Bambi", "Bambi", "A0082", Some("/kino/duesseldorf/bambi-343")),
    ("Black Box im Filmmuseum", "Black Box im Filmmuseum", "A0098", Some("/kino/duesseldorf/black-box-im-filmmuseum-1244")),
    ("Cinema Düsseldorf", "Cinema Düsseldorf", "A0231", Some("/kino/duesseldorf/cinema-36408")),
    ("Cinestar Düsseldorf", "Cinestar Düsseldorf", "A0379", Some("/kino/duesseldorf/cinestar-37176")),
    ("Frankenheim Kino", "Frankenheim Kino", "A1884", None),
    ("Kino Süd", "Kino Süd", "A0755", None),
    ("Metropol Düsseldorf", "Metropol Düsseldorf", "A0950", Some("/kino/duesseldorf/metropol-30040")),
    ("Open-Air-Kino Vier Linden", "Open-Air-Kino Vier Linden", "A1764", Some("/kino/duesseldorf/openairkino-vier-linden-36050")),
    ("UCI Düsseldorf", "UCI Düsseldorf", "A1200", Some("/kino/duesseldorf/uci-kinowelt-duesseldorf-37143")),
    ("UFA Palast Düsseldorf", "UFA Palast Düsseldorf", "A1221", Some("/kino/duesseldorf/ufapalast-35116")),
    ("Arkadenhof der Bonner Universität - Am Hof", "Arkadenhof der Bonner Universität - Am Hof", "A2067", Some("/kino/bonn/arkadenhof-der-bonner-universitaet-am-hof-55937")),
    ("Bundeskunsthalle", "Bundeskunsthalle", "A2638", Some("/kino/bonn/bundeskunsthalle-81737")),
    ("CineStar Bonn - Sternlichtspiele", "CineStar Bonn - Sternlichtspiele", "A0417", Some("/kino/bonn/cinestar-bonn-sternenlichtspiele-61716")),
    ("Kino in der Brotfabrik", "Kino in der Brotfabrik", "A0736", Some("/kino/bonn/kino-in-der-brotfabrik-31768")),
    ("Neue Filmbühne Bonn", "Neue Filmbühne Bonn", "A0982", Some("/kino/bonn/neue-filmbuehne-31770")),
    ("Rex-Lichtspieltheater", "Rex-Lichtspieltheater", "A1073", Some("/kino/bonn/rexlichtspieltheater-31777")),
    ("Woki", "Woki", "A1282", Some("/kino/bonn/woki-31772")),
    ("Cineplex Kinopolis Leverkusen", "Cineplex Kinopolis Leverkusen", "A0826", None),
    ("Kommunales Kino Leverkusen", "Kommunales Kino Leverkusen", "A0859", Some("/kino/leverkusen/kommunales-kino-69959")),
    ("Scala Leverkusen", "Scala Leverkusen", "A2358", Some("/kino/leverkusen/scala-31187")),
    ("CP Lichtspielfreunde Hürth von 2016 e.V.", "CP Lichtspielfreunde Hürth von 2016 e.V.", "A2754", None),
    ("Open Air Kino Kloster Burbach", "Open Air Kino Kloster Burbach", "A2488", None),
    ("UCI Hürth Park", "UCI Hürth Park", "A1198", Some("/kino/huerth/uci-kinowelt-huerth-park-40814")),
    ("Capitol-Theater Kerpen", "Capitol-Theater Kerpen", "A1340", Some("/kino/kerpen/capitoltheater-33457")),
    ("Euromax - Cinemas", "Euromax - Cinemas", "A0471", Some("/kino/kerpen/euromax-cinemas-31830")),
    ("Programmkino im Schaustall", "Programmkino im Schaustall", "A1043", Some("/kino/langenfeld/programmkino-im-schaustall-54937")),
    ("Rex Langenfeld", "Rex Langenfeld", "A1067", Some("/kino/langenfeld/rex-31853")),
    ("Capitol Siegburg", "Capitol Siegburg", "A0147", Some("/kino/siegburg/kinocenter-capitol-31911")),
    ("Cineplex Siegburg", "Cineplex Siegburg", "A0228", Some("/kino/siegburg/cineplex-39605")),
    ("Cineplex Bensberg", "Cineplex Bensberg", "A2715", Some("/kino/bergisch-gladbach/cineplex-bensberg-31750")),
    ("Cineplex Troisdorf", "Cineplex Troisdorf", "A1471", Some("/kino/troisdorf/cineplex-31927")),
    ("Studio", "Studio", "A1161", None),
    ("Cineplex Euskirchen", "Cineplex Euskirchen", "A2792", Some("/kino/euskirchen/cineplex-in-der-galleria-31798")),
    ("Linden Theater Frechen", "Linden Theater Frechen", "A0918", Some("/kino/frechen/lindentheater-31800")),
    ("Kur-Theater Hennef", "Kur-Theater Hennef", "A1480", Some("/kino/hennef-sieg/kurtheater-31816")),
    ("Zoom-Kino", "Zoom-Kino", "A1291", Some("/kino/bruehl/zoomkino-31781")),
    ("Emotion Kino", "Emotion Kino", "G02P3", Some("/kino/monheim-am-rhein/emotion-kino-93182")),
    ("Lichtspiele Kalk", "Lichtspiele Kalk", "A2915", None)
  ))
  private def r_muenchen: R = ("muenchen", "München", "Bayern", 48.13743, 11.57549, Seq("München", "Fürstenfeldbruck", "Ottobrunn", "Germering", "Dachau", "Erding", "Unterschleissheim", "Starnberg", "Gauting", "Gröbenzell", "Neufahrn", "Taufkirchen (Vils)", "Haar", "Gilching", "Gräfelfing", "Aschheim", "Weßling"), Seq(
    ("ABC-Kino", "ABC-Kino", "A1476", Some("/kino/muenchen/abckino-32873")),
    ("Astor Cinema Lounge", "Astor Cinema Lounge", "A2636", None),
    ("Astor Film Lounge im Arri", "Astor Film Lounge im Arri", "A1298", Some("/kino/muenchen/astor-film-lounge-im-arri-32867")),
    ("Cadillac Filmtheater", "Cadillac Filmtheater", "A0131", Some("/kino/muenchen/cadillac-und-veranda-kino-32857")),
    ("Cincinnati", "Cincinnati", "A0216", Some("/kino/muenchen/cincinnati-32870")),
    ("Cinema München", "Cinema München", "A2186", Some("/kino/muenchen/cinema-32869")),
    ("CinemaxX München", "CinemaxX München", "A0943", Some("/kino/muenchen/cinemaxx-33802")),
    ("City-Kinos - München", "City-Kinos - München", "A0397", None),
    ("Filmmuseum München", "Filmmuseum München", "A1419", Some("/kino/muenchen/filmmuseum-32872")),
    ("Forum 2 - Kulturverein Olympiadorf e.V.", "Forum 2 - Kulturverein Olympiadorf e.V.", "A0654", Some("/kino/muenchen/forum-2-kulturverein-olympiadorf-e.v.-32868")),
    ("Gabriel Filmtheater", "Gabriel Filmtheater", "A0988", Some("/kino/muenchen/gabriel-filmtheater-32862")),
    ("Gloria Palast", "Gloria Palast", "A1443", Some("/kino/muenchen/gloriapalast-32885")),
    ("Instituto Cervantes - München", "Instituto Cervantes - München", "A2832", None),
    ("Istituto Italiano di Cultura - München", "Istituto Italiano di Cultura - München", "A2847", None),
    ("Kino Solln", "Kino Solln", "A1499", Some("/kino/muenchen/kino-solln-32880")),
    ("Kino in der Hochschule für Fernsehen und Film München", "Kino in der Hochschule für Fernsehen und Film München", "A2743", None),
    ("Leopold", "Leopold", "A0895", Some("/kino/muenchen/leopold-32875")),
    ("Mathäser Filmpalast", "Mathäser Filmpalast", "A0025", Some("/kino/muenchen/mathaeser-filmpalast-54158")),
    ("Monopol", "Monopol", "A0963", Some("/kino/muenchen/monopol-65618")),
    ("Museum-Lichtspiele München", "Museum-Lichtspiele München", "A1570", Some("/kino/muenchen/museumlichtspiele-32866")),
    ("Neues Arena", "Neues Arena", "A1573", Some("/kino/muenchen/arena-filmtheater-32856")),
    ("Neues Maxim Kino München", "Neues Maxim Kino München", "A0941", Some("/kino/muenchen/neues-maxim-32859")),
    ("Neues Rex - Filmtheater", "Neues Rex - Filmtheater", "A0991", Some("/kino/muenchen/neues-rex-filmtheater-32879")),
    ("Neues Rottmann", "Neues Rottmann", "A1575", Some("/kino/muenchen/neues-rottmann-32863")),
    ("Open Air Kino am Olympiasee", "Open Air Kino am Olympiasee", "A2425", Some("/kino/muenchen/open-air-kino-am-olympiasee-75479")),
    ("Open Air Kino, Mond &amp; Sterne", "Open Air Kino, Mond &amp; Sterne", "A1815", None),
    ("Programm-Kino im Viehhof-Zelt", "Programm-Kino im Viehhof-Zelt", "A2721", None),
    ("Rio Filmpalast", "Rio Filmpalast", "A1603", Some("/kino/muenchen/rio-filmpalast-32884")),
    ("Royal Filmpalast", "Royal Filmpalast", "A1610", Some("/kino/muenchen/royal-filmpalast-32892")),
    ("Studio Isabella", "Studio Isabella", "A1642", Some("/kino/muenchen/studio-isabella-32864")),
    ("Technische Universität", "Technische Universität", "A2600", None),
    ("Theatiner Film", "Theatiner Film", "A1179", Some("/kino/muenchen/theatiner-film-32871")),
    ("Werkstattkino e.V.", "Werkstattkino e.V.", "A1276", Some("/kino/muenchen/werkstattkino-e.v.-39386")),
    ("Kino-Open-Air", "Kino-Open-Air", "A2210", Some("/kino/fuerstenfeldbruck/open-air-kino-33568")),
    ("Lichtspielhaus Fürstenfeldbruck", "Lichtspielhaus Fürstenfeldbruck", "A1539", Some("/kino/fuerstenfeldbruck/lichtspielhaus-32760")),
    ("Scala Kino Fürstenfeldbruck", "Scala Kino Fürstenfeldbruck", "A1110", Some("/kino/fuerstenfeldbruck/scala-64657")),
    ("Filmstudio Ottobrunn", "Filmstudio Ottobrunn", "A0528", Some("/kino/ottobrunn/filmstudio-32927")),
    ("Ottobrunner Kinos", "Ottobrunner Kinos", "A0969", Some("/kino/ottobrunn/ottobrunner-kinos-32928")),
    ("Cineplex Germering", "Cineplex Germering", "A2705", Some("/kino/germering/cineplex-87221")),
    ("Cinema Dachau", "Cinema Dachau", "A1357", Some("/kino/dachau/cinema-dachau-32719")),
    ("Cineplex Erding", "Cineplex Erding", "A0897", Some("/kino/erding/cineplex-erding-41159")),
    ("Capitol Unterschleißheim", "Capitol Unterschleißheim", "A1328", Some("/kino/unterschleissheim/capitol-33043")),
    ("Kino Breitwand Starnberg", "Kino Breitwand Starnberg", "A0712", Some("/kino/starnberg/kino-breitwand-50617")),
    ("Kino Breitwand Gauting", "Kino Breitwand Gauting", "A1412", Some("/kino/gauting/kino-breitwand-gauting-88757")),
    ("Gröben-Lichtspiele", "Gröben-Lichtspiele", "A1445", Some("/kino/groebenzell/groebenlichtspiele-32780")),
    ("Cineplex Neufahrn", "Cineplex Neufahrn", "A2112", Some("/kino/neufahrn/cineplex-neufahrn-72758")),
    ("Kinocafe", "Kinocafe", "A0393", Some("/kino/taufkirchen-vils/kinocafe-33021")),
    ("Haarer Kinos", "Haarer Kinos", "A1669", Some("/kino/haar/haarer-kinos-70017")),
    ("Filmstation", "Filmstation", "A0526", Some("/kino/gilching/filmstation-37733")),
    ("Filmeck im Bürgerhaus", "Filmeck im Bürgerhaus", "A1415", Some("/kino/graefelfing/filmeck-im-buergerhaus-32777")),
    ("Drive In Autokino Aschheim", "Drive In Autokino Aschheim", "A0452", Some("/kino/aschheim/drive-in-autokino-aschheim-32667")),
    ("Kino Pfarrstadel", "Kino Pfarrstadel", "A2659", Some("/kino/wessling/kino-pfarrstadel-84537"))
  ))
  private def r_hamburg: R = ("hamburg", "Hamburg", "Hamburg", 53.55073, 9.99302, Seq("Hamburg", "Buchholz in der Nordheide", "Quickborn", "Altona", "Norderstedt", "Elmshorn", "Stade", "Buxtehude", "Wedel", "Neu Wulmstorf", "Uetersen", "Bargteheide", "Harsefeld"), Seq(
    ("3001 Kino", "3001 Kino", "A0002", Some("/kino/hamburg/3001-kino-32436")),
    ("Abaton", "Abaton", "A0003", Some("/kino/hamburg/abaton-32437")),
    ("Alabama-Kino", "Alabama-Kino", "A0012", Some("/kino/hamburg/alabamakino-32438")),
    ("Astor Film Lounge HafenCity Hamburg", "Astor Film Lounge HafenCity Hamburg", "A2900", Some("/kino/hamburg/astor-film-lounge-hafencity-91617")),
    ("B-Movie", "B-Movie", "A1907", Some("/kino/hamburg/bmovie-66237")),
    ("Blankeneser Kino", "Blankeneser Kino", "A0677", Some("/kino/hamburg/blankeneser-kino-32464")),
    ("CinemaxX Dammtor", "CinemaxX Dammtor", "A1467", Some("/kino/hamburg/cinemaxx-dammtor-33089")),
    ("CinemaxX Harburg", "CinemaxX Harburg", "A0287", Some("/kino/hamburg/cinemaxx-harburg-38443")),
    ("CinemaxX Wandsbek", "CinemaxX Wandsbek", "A0282", Some("/kino/hamburg/cinemaxx-wandsbek-42017")),
    ("Elbe-Kino", "Elbe-Kino", "A1400", Some("/kino/hamburg/elbekino-32453")),
    ("FilmRaum", "FilmRaum", "A2730", Some("/kino/hamburg/filmraum-88038")),
    ("Hansa Studio Bergedorf", "Hansa Studio Bergedorf", "A1449", Some("/kino/hamburg/hansafilmstudio-bergedorf-32462")),
    ("Holi", "Holi", "A0614", Some("/kino/hamburg/holi-32439")),
    ("Instituto Cervantes - Hamburg", "Instituto Cervantes - Hamburg", "A2846", None),
    ("Kinopolis HafenCity", "Kinopolis HafenCity", "G02MI", Some("/kino/hamburg/kinopolis-hafencity-93333")),
    ("Koralle", "Koralle", "A0869", Some("/kino/hamburg/koralle-49596")),
    ("LOTTO Hamburg SchanzenKino Open Air", "LOTTO Hamburg SchanzenKino Open Air", "A2296", Some("/kino/hamburg/lotto-hamburg-schanzenkino-open-air-59037")),
    ("Lichtmess", "Lichtmess", "A2604", Some("/kino/hamburg/lichtmess-77077")),
    ("Magazin", "Magazin", "A1553", Some("/kino/hamburg/magazin-32445")),
    ("Metropolis Kino", "Metropolis Kino", "A2712", Some("/kino/hamburg/metropolis-kino-32449")),
    ("Outdoor Cine - Das Open Air Kino im Völkerkunde-Museum", "Outdoor Cine - Das Open Air Kino im Völkerkunde-Museum", "A1913", None),
    ("Passage Hamburg", "Passage Hamburg", "A1034", Some("/kino/hamburg/passage-kino-hamburg-32443")),
    ("Savoy (Hamburg)", "Savoy (Hamburg)", "A0958", Some("/kino/hamburg/savoy-82236")),
    ("SchanzenKino 73 (zweisprachig)", "SchanzenKino 73 (zweisprachig)", "A2747", None),
    ("Sommerkino auf dem Alsterdorfer Markt", "Sommerkino auf dem Alsterdorfer Markt", "A2089", Some("/kino/hamburg/sommerkino-auf-dem-alsterdorfer-markt-32451")),
    ("UCI Mundsburg", "UCI Mundsburg", "A1203", Some("/kino/hamburg/uci-kinowelt-mundsburg-37115")),
    ("UCI Wandsbek", "UCI Wandsbek", "A1214", Some("/kino/hamburg/uci-kinowelt-wandsbek-43194")),
    ("Zeise Kinos", "Zeise Kinos", "A1464", Some("/kino/hamburg/zeise-kinos-32465")),
    ("Movieplexx Autokino", "Movieplexx Autokino", "A2945", None),
    ("Movieplexx Delhi - Center", "Movieplexx Delhi - Center", "A0971", None),
    ("Beluga Kino", "Beluga Kino", "A0088", Some("/kino/quickborn/beluga-kino-61337")),
    ("Openair Kino", "Openair Kino", "A2182", Some("/kino/quickborn/openair-kino-73586")),
    ("LOTTO Hamburg Auto- und Open Air Kino", "LOTTO Hamburg Auto- und Open Air Kino", "A2949", None),
    ("Spectrum Kino Norderstedt", "Spectrum Kino Norderstedt", "A1142", Some("/kino/norderstedt/spectrum-34888")),
    ("Cineplex Elmshorn", "Cineplex Elmshorn", "A0317", Some("/kino/elmshorn/cineplex-elmshorn-42557")),
    ("CineStar Stade", "CineStar Stade", "A0329", Some("/kino/stade/cinestar-38679")),
    ("City Kino Buxtehude", "City Kino Buxtehude", "A1381", Some("/kino/buxtehude/city-kino-buxtehude-32382")),
    ("Open Air auf dem Theaterschiff", "Open Air auf dem Theaterschiff", "A2723", None),
    ("Das Kino - Neu Wulmstorf", "Das Kino - Neu Wulmstorf", "A1334", Some("/kino/neu-wulmstorf/das-kino-32550")),
    ("Burg-Theater Uetersen", "Burg-Theater Uetersen", "A1322", Some("/kino/uetersen/burgtheater-32625")),
    ("Kleines Theater", "Kleines Theater", "A0850", Some("/kino/bargteheide/kleines-theater-32343")),
    ("Harsefelder Lichtspiele", "Harsefelder Lichtspiele", "A0602", Some("/kino/harsefeld/harsefelder-lichtspiele-32479"))
  ))
  private def r_nuernberg: R = ("nuernberg", "Nürnberg", "Bayern", 49.45421, 11.07752, Seq("Nürnberg", "Erlangen", "Fürth", "Neumarkt", "Neumarkt in der Oberpfalz", "Schwabach", "Eckental", "Grafenberg"), Seq(
    ("Admiral Filmpalast Nürnberg", "Admiral Filmpalast Nürnberg", "A0661", Some("/kino/nuernberg-mittelfranken/admiral-filmpalast-32906")),
    ("Casablanca Nürnberg", "Casablanca Nürnberg", "A0172", Some("/kino/nuernberg-mittelfranken/casablanca-32914")),
    ("Cinecitta", "Cinecitta", "A0049", Some("/kino/nuernberg-mittelfranken/cinecitta-34569")),
    ("Cinecittá Open Air", "Cinecittá Open Air", "A2779", None),
    ("Filmfabrik KommKino", "Filmfabrik KommKino", "A0854", None),
    ("Filmhauskino im Künstlerhaus", "Filmhauskino im Künstlerhaus", "A1418", Some("/kino/nuernberg-mittelfranken/filmhauskino-67658")),
    ("Kino in den Felsengängen", "Kino in den Felsengängen", "A1980", None),
    ("Meisengeige", "Meisengeige", "A1557", Some("/kino/nuernberg-mittelfranken/meisengeige-32912")),
    ("Metropolis Nürnberg", "Metropolis Nürnberg", "A0956", Some("/kino/nuernberg-mittelfranken/metropolis-32915")),
    ("Open Air Fränkisches Museumseisenbahn", "Open Air Fränkisches Museumseisenbahn", "A2489", None),
    ("Open Air Kino Freilichtbühne Desi", "Open Air Kino Freilichtbühne Desi", "A1981", None),
    ("Open Air Kino Katharinenruine", "Open Air Kino Katharinenruine", "A2179", Some("/kino/nuernberg-mittelfranken/open-air-kino-katharinenruine-73363")),
    ("Open Air Kino Naturgarten Bad", "Open Air Kino Naturgarten Bad", "A2498", Some("/kino/nuernberg-mittelfranken/open-air-naturgartenbad-86881")),
    ("Open Air Kino Pellerhaus", "Open Air Kino Pellerhaus", "A2180", Some("/kino/nuernberg-mittelfranken/open-air-pellerhaus-91281")),
    ("Open Air Krafftscher Hof", "Open Air Krafftscher Hof", "A2500", Some("/kino/nuernberg-mittelfranken/open-air-krafftscher-hof-75946")),
    ("Open Air Radrennbahn am Reichelsdorfer Keller", "Open Air Radrennbahn am Reichelsdorfer Keller", "A1956", None),
    ("Rio Palast", "Rio Palast", "A1604", Some("/kino/nuernberg-mittelfranken/rio-palast-32905")),
    ("Roxy-Fremdsprachenkino", "Roxy-Fremdsprachenkino", "A1092", None),
    ("SommerNachtFilmFestival - Marienbergpark", "SommerNachtFilmFestival - Marienbergpark", "A2822", None),
    ("CineStar Erlangen", "CineStar Erlangen", "A0355", Some("/kino/erlangen/cinestar-erlangen-35135")),
    ("Kino im E-Werk", "Kino im E-Werk", "A0720", Some("/kino/erlangen/kino-im-ewerk-37549")),
    ("Lamm-Lichtspiele", "Lamm-Lichtspiele", "A1532", Some("/kino/erlangen/lammlichtspiele-32739")),
    ("Manhattan Deluxe – Premiumkino", "Manhattan Deluxe – Premiumkino", "A1554", Some("/kino/erlangen/manhattan-deluxe-32737")),
    ("Open Air Erlangen", "Open Air Erlangen", "A1943", None),
    ("Open Air an der Bleiche", "Open Air an der Bleiche", "A2864", None),
    ("Babylon Kino am Stadtpark", "Babylon Kino am Stadtpark", "A0657", Some("/kino/fuerth/babylon-kino-am-stadtpark-32762")),
    ("Cineplex Fürth", "Cineplex Fürth", "A2727", Some("/kino/fuerth/cineplex-87698")),
    ("Freilichtbühne im Fürther Stadtpark", "Freilichtbühne im Fürther Stadtpark", "A1963", None),
    ("Open Air Kino Mauerflimmern", "Open Air Kino Mauerflimmern", "A2780", Some("/kino/fuerth/open-air-kino-im-kulturforum-fuerth-%22mauerflimmern%22-88137")),
    ("Uferpalast Führt", "Uferpalast Führt", "A1223", Some("/kino/fuerth/uferpalast-34559")),
    ("Rialto Palast", "Rialto Palast", "A1077", Some("/kino/neumarkt/rialto-palast-32899")),
    ("Cineplex Neumarkt", "Cineplex Neumarkt", "A2716", None),
    ("Luna Theater", "Luna Theater", "A0929", Some("/kino/schwabach/luna-theater-65197")),
    ("Casino Lichtspiele Eckental", "Casino Lichtspiele Eckental", "A1343", Some("/kino/eckental/casino-lichtspiele-32728")),
    ("Kinomobil Stuttgart - Historische Kelter", "Kinomobil Stuttgart - Historische Kelter", "A2134", None)
  ))
  private def r_dortmund: R = ("dortmund", "Dortmund", "Nordrhein-Westfalen", 51.51494, 7.466, Seq("Dortmund", "Essen", "Gelsenkirchen", "Hagen", "Herne", "Recklinghausen", "Iserlohn", "Witten", "Marl", "Lünen", "Unna", "Werne", "Wetter", "Herdecke", "Altena"), Seq(
    ("CineStar Dortmund", "CineStar Dortmund", "A0029", Some("/kino/dortmund/cinestar-35057")),
    ("Filmbühne Zur Postkutsche", "Filmbühne Zur Postkutsche", "A0490", Some("/kino/dortmund/filmbuehne-zur-postkutsche-30028")),
    ("Kino im U", "Kino im U", "A2615", Some("/kino/dortmund/kino-im-u-80817")),
    ("Open Air Kino im Stadion", "Open Air Kino im Stadion", "A1898", None),
    ("PSD Bank Autokino Dortmund", "PSD Bank Autokino Dortmund", "A2947", None),
    ("Roxy Dortmund", "Roxy Dortmund", "A1088", None),
    ("Schauburg Dortmund", "Schauburg Dortmund", "A1122", Some("/kino/dortmund/lichtspiel-und-kunsttheater-schauburg-252")),
    ("sweetSixteen-Kino", "sweetSixteen-Kino", "A2590", Some("/kino/dortmund/sweetsixteen-76817")),
    ("Astra-Theater &amp; Luna", "Astra-Theater &amp; Luna", "A0054", Some("/kino/essen/astratheater-und-luna-374")),
    ("CinemaxX Essen", "CinemaxX Essen", "A0273", Some("/kino/essen/cinemaxx-383")),
    ("Drive In Autokino Essen", "Drive In Autokino Essen", "A0451", Some("/kino/essen/drive-in-autokino-30051")),
    ("Eulenspiegel Filmtheater", "Eulenspiegel Filmtheater", "A0470", Some("/kino/essen/eulenspiegel-filmtheater-400")),
    ("Filmstudio Glückauf", "Filmstudio Glückauf", "A0531", Some("/kino/essen/filmstudio-glueckauf-1319")),
    ("Galerie Cinema", "Galerie Cinema", "A0567", Some("/kino/essen/galerie-cinema-1326")),
    ("Lichtburg und Sabu", "Lichtburg und Sabu", "A0902", Some("/kino/essen/lichtburg-und-sabu-41636")),
    ("Multiplex Gelsenkirchen", "Multiplex Gelsenkirchen", "A1260", Some("/kino/gelsenkirchen/apollo-cinemas-multiplex-gelsenkirchen-496")),
    ("Schauburg Filmpalast", "Schauburg Filmpalast", "A1123", Some("/kino/gelsenkirchen/schauburg-filmpalast-489")),
    ("Babylon Hagen", "Babylon Hagen", "A0705", Some("/kino/hagen/kino-babylon-im-kulturzentrum-pelmke-38405")),
    ("CineStar Hagen", "CineStar Hagen", "A0341", Some("/kino/hagen/cinestar-35181")),
    ("Filmwelt Herne", "Filmwelt Herne", "A0547", Some("/kino/herne-westfalen/filmwelt-60717")),
    ("Open-Air Kino Schloss Strünkede", "Open-Air Kino Schloss Strünkede", "A2008", None),
    ("Cineworld", "Cineworld", "A0388", Some("/kino/recklinghausen/cineworld-37763")),
    ("Filmpalast Iserlohn", "Filmpalast Iserlohn", "A2796", Some("/kino/iserlohn/filmpalast-39711")),
    ("Die Burg Witten", "Die Burg Witten", "A2888", Some("/kino/witten/die-burg-876")),
    ("Loe Studios Marl", "Loe Studios Marl", "A2910", Some("/kino/marl/loe-studios-91739")),
    ("Cineworld Lünen", "Cineworld Lünen", "A0391", Some("/kino/luenen/cineworld-47639")),
    ("Kinorama Unna", "Kinorama Unna", "A0491", Some("/kino/unna/kinorama-13450")),
    ("Capitol-Cinema-Center", "Capitol-Cinema-Center", "A0155", Some("/kino/werne/capitolcinemacenter-31939")),
    ("Kulturzentrum Lichtburg", "Kulturzentrum Lichtburg", "A0879", Some("/kino/wetter/kulturzentrum-lichtburg-874")),
    ("Onikon", "Onikon", "A1014", Some("/kino/herdecke/onikon-605")),
    ("Apollo Service Kino", "Apollo Service Kino", "A1297", Some("/kino/altena/apollo-service-kino-30"))
  ))
  private def r_mannheim: R = ("mannheim", "Mannheim", "Baden-Württemberg", 49.4891, 8.46694, Seq("Mannheim", "Heidelberg", "Neustadt an der Weinstraße", "Speyer", "Frankenthal", "Grünstadt", "Worms", "Neustadt an der Weinstrasse", "Weinheim", "Bensheim", "Viernheim", "Heppenheim", "Schwetzingen", "Schifferstadt", "Seeheim-Jugenheim", "Walldorf", "Ketsch", "Hemsbach", "Limburgerhof", "Biblis"), Seq(
    ("Atlantis Mannheim", "Atlantis Mannheim", "A1305", Some("/kino/mannheim-universitaetsstadt/atlantis-32168")),
    ("Cinema Quadrat e.V.", "Cinema Quadrat e.V.", "A1368", Some("/kino/mannheim-universitaetsstadt/cinema-quadrat-e.v-32170")),
    ("Cineplex Mannheim", "Cineplex Mannheim", "A0270", Some("/kino/mannheim-universitaetsstadt/cineplex-37050")),
    ("Odeon Mannheim", "Odeon Mannheim", "A1002", Some("/kino/mannheim-universitaetsstadt/odeon-32169")),
    ("Planken Lichtspiele Mannheim", "Planken Lichtspiele Mannheim", "A1376", Some("/kino/mannheim-universitaetsstadt/planken-lichtspiele-32172")),
    ("Gloria &amp; Gloriette", "Gloria &amp; Gloriette", "A0578", Some("/kino/heidelberg-neckar/gloria-und-gloriette-32077")),
    ("Kamera Heidelberg", "Kamera Heidelberg", "A0635", Some("/kino/heidelberg-neckar/kamera-54296")),
    ("Karlstorkino", "Karlstorkino", "A0648", Some("/kino/heidelberg-neckar/karlstorkino-54216")),
    ("Luxor Filmpalast Heidelberg", "Luxor Filmpalast Heidelberg", "A2874", Some("/kino/heidelberg-neckar/luxorfilmpalast-89317")),
    ("Cineplex Neustadt", "Cineplex Neustadt", "A2752", Some("/kino/neustadt-an-der-weinstrasse/cineplex-89157")),
    ("Open Air Kino in der Hetzel-Galerie", "Open Air Kino in der Hetzel-Galerie", "A2768", Some("/kino/neustadt-an-der-weinstrasse/open-air-kino-der-roxy-kinos-und-der-hetzel-galerie-63758")),
    ("IMAX Speyer", "IMAX Speyer", "A0624", None),
    ("Kinocenter Theaterhaus Speyer", "Kinocenter Theaterhaus Speyer", "A1647", None),
    ("Lux im Dathenushaus", "Lux im Dathenushaus", "A0930", None),
    ("OPEN AIR KINO in der Erkenbert Ruine", "OPEN AIR KINO in der Erkenbert Ruine", "A1990", Some("/kino/frankenthal-pfalz/open-air-kino-in-der-erkenbert-ruine-71540")),
    ("Europa-Theater Grünstadt", "Europa-Theater Grünstadt", "A1404", Some("/kino/gruenstadt/europa-kinos-32070")),
    ("Filmwelt Grünstadt", "Filmwelt Grünstadt", "A2700", Some("/kino/gruenstadt/filmwelt-gruenstadt-86677")),
    ("Arkaden Lichtspiele", "Arkaden Lichtspiele", "A0891", None),
    ("Roxy Kinos", "Roxy Kinos", "A1085", Some("/kino/neustadt-an-der-weinstrasse/roxy-kinos-44479")),
    ("Modernes Theater", "Modernes Theater", "A1567", Some("/kino/weinheim/modernes-theater-32293")),
    ("Luxor Filmpalast Bensheim", "Luxor Filmpalast Bensheim", "A1326", Some("/kino/bensheim/luxor-filmpalast-31986")),
    ("Kinopolis Rhein-Neckar", "Kinopolis Rhein-Neckar", "A0833", Some("/kino/viernheim/kinopolis-32280")),
    ("Saalbau-Filmtheater", "Saalbau-Filmtheater", "A1097", Some("/kino/heppenheim/saalbaufilmtheater-32087")),
    ("Luxor Filmpalast Schwetzingen", "Luxor Filmpalast Schwetzingen", "A1551", Some("/kino/schwetzingen/luxor-32260")),
    ("Rex Kino-Center Schifferstadt", "Rex Kino-Center Schifferstadt", "A1072", Some("/kino/schifferstadt/rexkinocenter-85977")),
    ("Filmseher Open Air", "Filmseher Open Air", "A2797", None),
    ("Luxor Filmpalast Walldorf", "Luxor Filmpalast Walldorf", "A1763", Some("/kino/walldorf-baden/luxor-filmpalast-70737")),
    ("Central Filmtheater Ketsch", "Central Filmtheater Ketsch", "A1345", Some("/kino/ketsch-rhein/central-filmtheater-32125")),
    ("Brennessel-Programmkino", "Brennessel-Programmkino", "A0106", Some("/kino/hemsbach-bergstrasse/brennessel-programmkino-32086")),
    ("Capitol LichtspielTheater Limburgerhof", "Capitol LichtspielTheater Limburgerhof", "A1336", Some("/kino/limburgerhof/capitol-lichtspieltheater-32150")),
    ("Die Filminsel", "Die Filminsel", "A1779", Some("/kino/biblis/die-filminsel-31987"))
  ))
  private def r_krefeld: R = ("krefeld", "Krefeld", "Nordrhein-Westfalen", 51.33645, 6.55381, Seq("Krefeld", "Oberhausen", "Duisburg", "Mülheim an der Ruhr", "Mönchengladbach", "Neuss", "Moers", "Bottrop", "Ratingen", "Meerbusch - Lank", "Nettetal", "Kaarst", "Kamp-Lintfort", "Kempen", "Geldern"), Seq(
    ("CinemaxX Krefeld", "CinemaxX Krefeld", "A0278", Some("/kino/krefeld/cinemaxx-34929")),
    ("Fabrik Heeder", "Fabrik Heeder", "A0594", Some("/kino/krefeld/fabrik-heeder-927")),
    ("Open Air Kino Krefelder Rennbahn", "Open Air Kino Krefelder Rennbahn", "A2812", None),
    ("Primus-Palast", "Primus-Palast", "A0242", Some("/kino/krefeld/primus-palast-43576")),
    ("CineStar Oberhausen - Filmpalast im Centro", "CineStar Oberhausen - Filmpalast im Centro", "A1261", Some("/kino/oberhausen/cinestar-der-filmpalast-oberhausen-im-centro-30754")),
    ("Kino im Druckluft", "Kino im Druckluft", "A0719", Some("/kino/oberhausen/kino-im-druckluft-45421")),
    ("Lichtburg-Filmpalast", "Lichtburg-Filmpalast", "A0905", Some("/kino/oberhausen/lichtburg-filmpalast-31021")),
    ("Walzenlager-Zentrum Altenberg", "Walzenlager-Zentrum Altenberg", "A0733", Some("/kino/oberhausen/kino-im-walzenlager-zentrum-altenberg-31106")),
    ("Filmforum", "Filmforum", "A2750", Some("/kino/duisburg/filmforum-298")),
    ("Stadtwerke Sommerkino", "Stadtwerke Sommerkino", "A1862", Some("/kino/duisburg/stadtwerke-sommerkino-71280")),
    ("UCI Duisburg", "UCI Duisburg", "A0663", Some("/kino/duisburg/uci-kinowelt-duisburg-35240")),
    ("CinemaxX Mülheim", "CinemaxX Mülheim", "A0277", Some("/kino/muelheim-an-der-ruhr/cinemaxx-37082")),
    ("Ringlokschuppen Open-Air-Kino auf der Drehscheibe", "Ringlokschuppen Open-Air-Kino auf der Drehscheibe", "A1870", Some("/kino/muelheim-an-der-ruhr/open-air-kino-ringlokschuppendrehscheibe-63077")),
    ("Rio im Medienhaus", "Rio im Medienhaus", "A1082", Some("/kino/muelheim-an-der-ruhr/rio-filmtheater-im-medienhaus-27")),
    ("Cinefactory im Haus Zoar", "Cinefactory im Haus Zoar", "A2651", Some("/kino/moenchengladbach/cinefactory-im-haus-zoar-84518")),
    ("Comet-Cine-Center", "Comet-Cine-Center", "A0419", Some("/kino/moenchengladbach/comet-cine-center-36603")),
    ("Hitch", "Hitch", "A0607", Some("/kino/neuss/hitch-30126")),
    ("UCI Neuss", "UCI Neuss", "A1201", Some("/kino/neuss/uci-kinowelt-neuss-38582")),
    ("Atlantic Kinocenter", "Atlantic Kinocenter", "A2328", None),
    ("Movie Center Moers", "Movie Center Moers", "A2789", Some("/kino/moers/movie-center-74636")),
    ("Filmforum der VHS", "Filmforum der VHS", "A1475", Some("/kino/bottrop/filmforum-der-vhs-143")),
    ("Studiokino Ratingen", "Studiokino Ratingen", "A2804", Some("/kino/ratingen/studiokino-33769")),
    ("Kino am Forum Wasserturm", "Kino am Forum Wasserturm", "A2883", Some("/kino/meerbusch/kino-wasserturm-31860")),
    ("Corso Film Casino", "Corso Film Casino", "A0423", Some("/kino/nettetal/corso-film-casino-33189")),
    ("Kino Kaarst", "Kino Kaarst", "A0742", Some("/kino/kaarst/kino-kaarst-36522")),
    ("Hall of Fame Kamp-Lintfort", "Hall of Fame Kamp-Lintfort", "A2899", Some("/kino/kamplintfort/hall-of-fame-91817")),
    ("Kempener Lichtspiele", "Kempener Lichtspiele", "A0650", Some("/kino/kempen/kempener-lichtspiele-31829")),
    ("Herzog-Theater", "Herzog-Theater", "A0687", Some("/kino/geldern/herzog-theater-48156"))
  ))
  private def r_bielefeld: R = ("bielefeld", "Bielefeld", "Nordrhein-Westfalen", 52.03333, 8.53333, Seq("Bielefeld", "Gütersloh", "Bünde", "Detmold", "Herford", "Bad Salzuflen", "Lemgo", "Lage", "Schloss Holte-Stukenbrock", "Versmold", "Kirchlengern", "Spenge", "Rödinghausen"), Seq(
    ("AJZ Kino", "AJZ Kino", "A0010", Some("/kino/bielefeld/ajz-kino-61137")),
    ("CinemaxX Bielefeld", "CinemaxX Bielefeld", "A0271", Some("/kino/bielefeld/cinemaxx-37117")),
    ("Kamera Bielefeld", "Kamera Bielefeld", "A0634", Some("/kino/bielefeld/kamera-31762")),
    ("Lichtwerk im Ravensberger Park", "Lichtwerk im Ravensberger Park", "A0915", Some("/kino/bielefeld/lichtwerk-im-ravensberger-park-31756")),
    ("Melodie Filmtheater", "Melodie Filmtheater", "A2363", Some("/kino/bielefeld/melodie-filmtheater-75017")),
    ("Offkino Im Filmhaus Bielefeld", "Offkino Im Filmhaus Bielefeld", "A2838", Some("/kino/bielefeld/offkino-im-filmhaus-78518")),
    ("Open Air Kino Luna im Ravensberger Park", "Open Air Kino Luna im Ravensberger Park", "A2278", Some("/kino/bielefeld/open-air-kino-luna-im-ravensberger-park-36308")),
    ("Bambi &amp; Löwenherz", "Bambi &amp; Löwenherz", "A0081", Some("/kino/guetersloh/bambi-und-loewenherz-76656")),
    ("Filmwerk Gutersloh", "Filmwerk Gutersloh", "G01IT", None),
    ("Open Air Kino auf dem Dreiecksplatz", "Open Air Kino auf dem Dreiecksplatz", "A2434", Some("/kino/guetersloh/open-air-kino-auf-dem-dreiecksplatz-75548")),
    ("Autokino Bünder Lichtspiele am Festplatz", "Autokino Bünder Lichtspiele am Festplatz", "A2923", None),
    ("Bünder Lichtspiele", "Bünder Lichtspiele", "A0113", Some("/kino/buende/buender-lichtspiele-31782")),
    ("Universum Bünde", "Universum Bünde", "A1242", Some("/kino/buende/universum-45090")),
    ("Kaiserhof+ Einfach Gutes Kino", "Kaiserhof+ Einfach Gutes Kino", "A0632", None),
    ("Open-Air Mondscheinkino Waldbühne", "Open-Air Mondscheinkino Waldbühne", "A2010", Some("/kino/detmold/openair-mondscheinkino-waldbuehne-am-hermannsdenkmal-71538")),
    ("Capitol Herford", "Capitol Herford", "A0148", Some("/kino/herford/capitol-3d-kino-herford-31817")),
    ("Filmbühne Kinocenter", "Filmbühne Kinocenter", "A0488", Some("/kino/bad-salzuflen/filmbuehne-kinocenter-32339")),
    ("Hansa-Kino", "Hansa-Kino", "A0599", Some("/kino/lemgo/hansakino-35213")),
    ("Filmwelt Lippe", "Filmwelt Lippe", "A2647", Some("/kino/lage/filmwelt-lippe-83457")),
    ("Rhythmus-Filmtheater", "Rhythmus-Filmtheater", "A2365", None),
    ("Kulturbühne Versmold", "Kulturbühne Versmold", "G02OV", None),
    ("LichtBlick Kirchlengern", "LichtBlick Kirchlengern", "A0898", Some("/kino/kirchlengern/lichtblick-65581")),
    ("Zentral-Theater Spenge", "Zentral-Theater Spenge", "A1288", Some("/kino/spenge/zentraltheater-31918")),
    ("Else-Lichtspiele", "Else-Lichtspiele", "A0463", Some("/kino/roedinghausen/elselichtspiele-31905"))
  ))
  private def r_chemnitz: R = ("chemnitz", "Chemnitz", "Sachsen", 50.8357, 12.92922, Seq("Chemnitz", "Zwickau", "Freiberg", "Limbach-Oberfrohna", "Annaberg-Buchholz", "Aue", "Schneeberg", "Marienberg", "Mittweida", "Hohenstein-Ernstthal", "Schwarzenberg/Erzgebirge", "Lichtenstein", "Gelenau", "Geyer"), Seq(
    ("Campusfilmnächte auf dem Sportplatz", "Campusfilmnächte auf dem Sportplatz", "A2831", None),
    ("CineStar Chemnitz - am Roten Turm", "CineStar Chemnitz - am Roten Turm", "A0365", Some("/kino/chemnitz-sachsen/cinestar-der-filmpalast-am-roten-turm-47696")),
    ("Clubkino Siegmar", "Clubkino Siegmar", "A0415", Some("/kino/chemnitz-sachsen/clubkino-siegmar-57856")),
    ("Filmclub Mittendrin", "Filmclub Mittendrin", "A2356", Some("/kino/chemnitz-sachsen/filmclub-mittendrin-74977")),
    ("Filmnächte auf dem Theaterplatz", "Filmnächte auf dem Theaterplatz", "A2645", Some("/kino/chemnitz-sachsen/filmnaechte-auf-dem-theaterplatz-79319")),
    ("M54 / AJZ", "M54 / AJZ", "A2858", None),
    ("Metropol Chemnitz", "Metropol Chemnitz", "A0951", Some("/kino/chemnitz-sachsen/metropol-31469")),
    ("Weltecho", "Weltecho", "A2756", Some("/kino/chemnitz-sachsen/weltecho-93080")),
    ("Casablanca Zwickau", "Casablanca Zwickau", "A2378", Some("/kino/zwickau/casablanca-%22alter-gasometer%22-75097")),
    ("Filmpalast Astoria Zwickau", "Filmpalast Astoria Zwickau", "A0368", Some("/kino/zwickau/filmpalast-astoria-31722")),
    ("Kinopolis Freiberg", "Kinopolis Freiberg", "A0825", Some("/kino/freiberg/kinopolis-35167")),
    ("Open Air auf Schloss Freudenstein", "Open Air auf Schloss Freudenstein", "A2567", None),
    ("Filmtheater &quot;Apollo&quot;", "Filmtheater &quot;Apollo&quot;", "A0540", Some("/kino/limbachoberfrohna/apollo-filmtheater-31589")),
    ("Gloria Filmpalast Annaberg-Buchholz", "Gloria Filmpalast Annaberg-Buchholz", "A0580", Some("/kino/annabergbuchholz/gloria-filmpalast-35370")),
    ("Kino-Center Nickel-Odeon", "Kino-Center Nickel-Odeon", "A0996", Some("/kino/aue/nickelodeonfilmtheater-37556")),
    ("Union Filmtheater Schneeberg", "Union Filmtheater Schneeberg", "A1228", Some("/kino/schneeberg/union-filmtheater-31660")),
    ("Filmtheater Movie", "Filmtheater Movie", "A0544", Some("/kino/marienberg/kinocenter-movie-31604")),
    ("Filmbühne Mittweida", "Filmbühne Mittweida", "A0487", Some("/kino/mittweida/filmbuehne-31611")),
    ("Capitol Hohenstein-Ernstthal", "Capitol Hohenstein-Ernstthal", "A0153", Some("/kino/hohensteinernstthal/filmtheater-capitol-31565")),
    ("Olympia Schwarzenberg", "Olympia Schwarzenberg", "A1011", None),
    ("Clubkino &quot;Capitol&quot;", "Clubkino &quot;Capitol&quot;", "A0414", None),
    ("Clubkino", "Clubkino", "A0412", Some("/kino/gelenau-erzgebirge/clubkino-31529")),
    ("Autokino Greifensteine", "Autokino Greifensteine", "A2184", Some("/kino/geyer/autokino-greifensteine-73917"))
  ))
  private def r_leipzig: R = ("leipzig", "Leipzig", "Sachsen", 51.33962, 12.37129, Seq("Leipzig", "Halle", "Merseburg", "Grimma", "Taucha", "Groitzsch", "Leuna"), Seq(
    ("CineStar Leipzig", "CineStar Leipzig", "A0345", Some("/kino/leipzig/cinestar-der-filmpalast-43338")),
    ("Cineding", "Cineding", "A0225", Some("/kino/leipzig/cineding-56717")),
    ("Cinémathèque in der naTo Leipzig e.V.", "Cinémathèque in der naTo Leipzig e.V.", "A0445", Some("/kino/leipzig/cin%C3%A9math%C3%A8que-in-der-nato-leipzig-e.v.-65357")),
    ("Filmclub KassaBlanka", "Filmclub KassaBlanka", "A2361", None),
    ("Freilichtkino auf der Pferderennbahn", "Freilichtkino auf der Pferderennbahn", "A0561", None),
    ("Kinobar &quot;Prager Frühling&quot;", "Kinobar &quot;Prager Frühling&quot;", "A1500", Some("/kino/leipzig/kinobar-%22prager-fruehling%22-33663")),
    ("Luru Kino in der Spinnerei", "Luru Kino in der Spinnerei", "A2591", Some("/kino/leipzig/lurukino-in-der-spinnerei-76897")),
    ("Passage Kinos Leipzig", "Passage Kinos Leipzig", "A1037", Some("/kino/leipzig/passage-kinos-36692")),
    ("Regina-Palast", "Regina-Palast", "A1056", Some("/kino/leipzig/reginapalast-31586")),
    ("Schauburg Leipzig", "Schauburg Leipzig", "A2586", Some("/kino/leipzig/schauburg-31585")),
    ("Schaubühne Lindenfels", "Schaubühne Lindenfels", "A1116", Some("/kino/leipzig/schaubuehne-lindenfels-31581")),
    ("Sommerkino Schauportal", "Sommerkino Schauportal", "A1858", None),
    ("PUSCHKINO", "PUSCHKINO", "A0932", Some("/kino/halle-saale/puschkino-62017")),
    ("CinemaxX Halle-Charlottencenter", "CinemaxX Halle-Charlottencenter", "A0280", Some("/kino/halle-saale/cinemaxx-hallecharlottencenter-31556")),
    ("Luchs Kino am Zoo", "Luchs Kino am Zoo", "A0931", Some("/kino/halle-saale/luchs.kino-am-zoo-43039")),
    ("Prisma Cinema", "Prisma Cinema", "A0295", Some("/kino/halle-saale/prisma-cinema-53096")),
    ("Zazie", "Zazie", "A1286", Some("/kino/halle-saale/zazie-62577")),
    ("Domstadt Kino", "Domstadt Kino", "A0450", Some("/kino/merseburg-saale/domstadt-kino-65317")),
    ("Central Theater Grimma", "Central Theater Grimma", "A0194", Some("/kino/grimma/central-theater-31545")),
    ("CT-Lichtspiele", "CT-Lichtspiele", "A0429", Some("/kino/taucha-bei-leipzig/ctlichtspiele-75777")),
    ("Bürgerhaus Kino", "Bürgerhaus Kino", "A1320", Some("/kino/groitzsch-bei-pegau/buergerhaus-kino-31546")),
    ("UCI Nova Eventis", "UCI Nova Eventis", "A1212", None)
  ))
  private def r_karlsruhe: R = ("karlsruhe", "Karlsruhe", "Baden-Württemberg", 49.00937, 8.40444, Seq("Karlsruhe", "Pforzheim", "Landau in der Pfalz", "Baden-Baden", "Rastatt", "Bruchsal", "Landau", "Ettlingen", "Bretten", "Gaggenau", "Gernsbach"), Seq(
    ("Die Kurbel Karlsruhe", "Die Kurbel Karlsruhe", "A2614", Some("/kino/karlsruhe-baden/die-kurbel-78682")),
    ("Filmpalast am ZKM - Karlsruhe", "Filmpalast am ZKM - Karlsruhe", "A0513", Some("/kino/karlsruhe-baden/filmpalast-am-zkm-39797")),
    ("Kinemathek Karlsruhe", "Kinemathek Karlsruhe", "A1711", Some("/kino/karlsruhe-baden/kinemathek-48097")),
    ("Open Air Kino am Schloß Gottesaue", "Open Air Kino am Schloß Gottesaue", "A1976", Some("/kino/karlsruhe-baden/open-air-kino-am-schloss-gottesaue-34538")),
    ("Schauburg Karlsruhe", "Schauburg Karlsruhe", "A1626", Some("/kino/karlsruhe-baden/schauburg-32107")),
    ("Universum-City Karlsruhe", "Universum-City Karlsruhe", "A1246", Some("/kino/karlsruhe-baden/universumcitykinocenter-34235")),
    ("Cinemoon Pforzheim", "Cinemoon Pforzheim", "A0309", Some("/kino/pforzheim/cinemoon-47956")),
    ("Kommunales Kino Pforzheim", "Kommunales Kino Pforzheim", "A0857", Some("/kino/pforzheim/kommunales-kino-36754")),
    ("Open-Air-Kino im Kulturhaus Osterfeld", "Open-Air-Kino im Kulturhaus Osterfeld", "A1911", Some("/kino/pforzheim/openairkino-im-kulturhaus-osterfeld-44536")),
    ("rex Filmpalast Pforzheim", "rex Filmpalast Pforzheim", "A1068", Some("/kino/pforzheim/rex-filmpalast-56736")),
    ("Filmwelt Landau", "Filmwelt Landau", "A0548", None),
    ("Open Air Kino Landau", "Open Air Kino Landau", "A2564", None),
    ("Universum Kinocenter", "Universum Kinocenter", "A1245", None),
    ("Cineplex Baden-Baden", "Cineplex Baden-Baden", "A2689", Some("/kino/badenbaden/cineplex-86277")),
    ("moviac - Kino im Kaiserhof", "moviac - Kino im Kaiserhof", "A2628", Some("/kino/badenbaden/moviac-kino-im-kaiserhof-82097")),
    ("FORUM Rastatt", "FORUM Rastatt", "A2731", Some("/kino/rastatt/forum-87757")),
    ("Cineplex Bruchsal", "Cineplex Bruchsal", "A1373", Some("/kino/bruchsal/cineplex-bruchsal-31996")),
    ("KuKi - Das Kultkino", "KuKi - Das Kultkino", "A2623", None),
    ("Kulisse Ettlingen", "Kulisse Ettlingen", "A0876", Some("/kino/ettlingen/kulisse-48517")),
    ("Kinostar Filmwelt Bretten", "Kinostar Filmwelt Bretten", "A0508", Some("/kino/bretten/kinostar-filmwelt-34431")),
    ("Merkur-Kino-Center", "Merkur-Kino-Center", "A0944", Some("/kino/gaggenau/merkurkinocenter-32055")),
    ("Kinocenter Gernsbach", "Kinocenter Gernsbach", "A0771", Some("/kino/gernsbach/kinocenter-32061"))
  ))
  private def r_saarbruecken: R = ("saarbruecken", "Saarbrücken", "Saarland", 49.23262, 7.00982, Seq("Saarbrücken", "Sankt Ingbert", "Regionalverband Saarbrücken", "Neunkirchen", "Neunkirchen (Innenstadt)", "Homburg", "Saarlouis", "Zweibrücken", "Sankt Wendel", "Lebach", "Illingen", "Schmelz", "Bous", "Rehlingen-Siersburg"), Seq(
    ("CineStar Saarbrücken", "CineStar Saarbrücken", "A0340", Some("/kino/saarbruecken/cinestar-saarbruecken-42137")),
    ("Kino im Filmhaus", "Kino im Filmhaus", "A1125", Some("/kino/saarbruecken/filmhaus-32232")),
    ("Passage Saarbrücken", "Passage Saarbrücken", "A1586", Some("/kino/saarbruecken/passage-32234")),
    ("UT-Kino-Center", "UT-Kino-Center", "A1249", Some("/kino/saarbruecken/utkinocenter-32236")),
    ("Unifilm im AudiMax", "Unifilm im AudiMax", "A2323", Some("/kino/saarbruecken/unifilm-im-audimax-74517")),
    ("camera zwo - das arthouse kino", "camera zwo - das arthouse kino", "A0137", Some("/kino/saarbruecken/camera-zwo-das-arthouse-kino-51537")),
    ("kino achteinhalb", "kino achteinhalb", "A0698", Some("/kino/saarbruecken/kino-achteinhalb-34564")),
    ("Kinowerkstatt", "Kinowerkstatt", "A0844", Some("/kino/sankt-ingbert/kinowerkstatt-32242")),
    ("Neues Regina", "Neues Regina", "A1057", Some("/kino/sankt-ingbert/regina-kino-32241")),
    ("Residenz", "Residenz", "A2298", None),
    ("Cinetower", "Cinetower", "A0385", None),
    ("Neues Eden", "Neues Eden", "A0987", None),
    ("Eden Cinehouse", "Eden Cinehouse", "A1399", Some("/kino/homburg-saar/eden-cinehouse-32096")),
    ("Movie-World", "Movie-World", "A0976", Some("/kino/saarlouis/movieworld-32237")),
    ("Cinema Europa", "Cinema Europa", "A0250", Some("/kino/zweibruecken/cinema-europa-37628")),
    ("Neues Theater Sankt Wendel", "Neues Theater Sankt Wendel", "A0999", Some("/kino/sankt-wendel/neues-theater-32244")),
    ("City-Filmstudio", "City-Filmstudio", "A1379", Some("/kino/lebach/cityfilmstudio-32146")),
    ("Union Theater Illingen", "Union Theater Illingen", "A1235", Some("/kino/illingen-saar/union-theater-32100")),
    ("Schmelzer Lichtspiele", "Schmelzer Lichtspiele", "A0909", Some("/kino/schmelz-saar/schmelzer-lichtspiele-69857")),
    ("Thalia-Lichtspiele", "Thalia-Lichtspiele", "A1646", Some("/kino/bous/thalialichtspiele-31993")),
    ("Kino auf der Burg", "Kino auf der Burg", "A2513", Some("/kino/rehlingensiersburg/kino-auf-der-burg-76122"))
  ))
  private def r_bremen: R = ("bremen", "Bremen", "Bremen", 53.07582, 8.80717, Seq("Bremen", "Osterholz-Scharmbeck", "Delmenhorst", "Achim", "Syke", "Schwanewede", "Ritterhude", "Worpswede"), Seq(
    ("CITY 46 / Kommunalkino Bremen e.V.", "CITY 46 / Kommunalkino Bremen e.V.", "A0696", Some("/kino/bremen/city-46kommunalkino-bremen-e.v.-32359")),
    ("CineStar Bremen - Kristall-Palast", "CineStar Bremen - Kristall-Palast", "A0376", Some("/kino/bremen/cinestar-kristallpalast-38717")),
    ("Cinema Ostertor", "Cinema Ostertor", "A1366", Some("/kino/bremen/cinema-im-ostertor-32358")),
    ("CinemaxX Bremen", "CinemaxX Bremen", "A0274", Some("/kino/bremen/cinemaxx-35447")),
    ("Cineplex Cinespace Bremen", "Cineplex Cinespace Bremen", "A0326", Some("/kino/bremen/cineplex-cinespace-61677")),
    ("City-Filmtheater", "City-Filmtheater", "A1378", None),
    ("Filmkunsttheater Atlantis", "Filmkunsttheater Atlantis", "A0066", Some("/kino/bremen/atlantis-filmttheater-32364")),
    ("Filmkunsttheater Gondel", "Filmkunsttheater Gondel", "A0588", Some("/kino/bremen/gondel-32366")),
    ("Instituto Cervantes - Bremen", "Instituto Cervantes - Bremen", "A2845", None),
    ("Kulturkirche St. Stephani", "Kulturkirche St. Stephani", "A2681", Some("/kino/bremen/kulturkirche-st.-stephani-86057")),
    ("Schauburg Bremen", "Schauburg Bremen", "A1625", Some("/kino/bremen/schauburg-32361")),
    ("Central Theater Osterholz-Scharmbeck", "Central Theater Osterholz-Scharmbeck", "A1348", Some("/kino/osterholzscharmbeck/central-theater-32583")),
    ("Oscar Kulturspielhaus", "Oscar Kulturspielhaus", "A2746", Some("/kino/osterholzscharmbeck/kino-im-oscar-kulturspielhaus-88877")),
    ("MaxX", "MaxX", "A0266", Some("/kino/delmenhorst/maxx-57896")),
    ("Koki Kommunales Kino", "Koki Kommunales Kino", "A0853", None),
    ("Hansa Kino", "Hansa Kino", "A1447", Some("/kino/syke/hansa-kino-32621")),
    ("Film Palast", "Film Palast", "A1407", Some("/kino/schwanewede/filmpalast-32612")),
    ("Ritterhuder-Lichtspiele", "Ritterhuder-Lichtspiele", "A1605", Some("/kino/ritterhude/ritterhuder-lichtspiele-32599")),
    ("Music Hall Worpswede", "Music Hall Worpswede", "A0979", Some("/kino/worpswede/music-hall-73698"))
  ))
  private def r_heilbronn: R = ("heilbronn", "Heilbronn", "Baden-Württemberg", 49.13995, 9.22054, Seq("Heilbronn", "Sinsheim", "Neckarsulm", "Öhringen", "Besigheim", "Mosbach", "Lauffen", "Grossbottwar", "Löchgau", "Kirchheim am Neckar", "Forchtenberg", "Hardthausen am Kocher", "Erligheim"), Seq(
    ("CinemaxX Heilbronn", "CinemaxX Heilbronn", "A2894", Some("/kino/heilbronn/cinemaxx-42206")),
    ("Kinostar Arthaus Heilbronn", "Kinostar Arthaus Heilbronn", "A2748", Some("/kino/heilbronn/kinostar-arthaus-32085")),
    ("Open-Air-Kino-Heilbronn", "Open-Air-Kino-Heilbronn", "A2814", Some("/kino/heilbronn/openairkino-92805")),
    ("Citydome Sinsheim", "Citydome Sinsheim", "A0402", Some("/kino/sinsheim/citydome-39039")),
    ("IMAX 3D Laser 4k Kino Sinsheim", "IMAX 3D Laser 4k Kino Sinsheim", "A0622", Some("/kino/sinsheim/imax-3d-32267")),
    ("Cineplex Neckarsulm", "Cineplex Neckarsulm", "A0475", Some("/kino/neckarsulm/cineplex-35134")),
    ("Kinostar Scala + Scala-Keller", "Kinostar Scala + Scala-Keller", "A0839", Some("/kino/neckarsulm/kinostar-scala-58176")),
    ("Holi Filmtheater", "Holi Filmtheater", "A2203", Some("/kino/oehringen/holi-filmtheater-73958")),
    ("Scala Filmtheater Öhringen", "Scala Filmtheater Öhringen", "A0837", Some("/kino/oehringen/scala-filmtheater-32212")),
    ("Kinomobil Stuttgart - Alte Kelter", "Kinomobil Stuttgart - Alte Kelter", "A0793", Some("/kino/besigheim/kinomobil-stuttgart-stadthalle-alte-kelter-41595")),
    ("Kinostar Filmwelt Mosbach", "Kinostar Filmwelt Mosbach", "A1513", None),
    ("Kinomobil Stuttgart - Stadthalle Lauffen", "Kinomobil Stuttgart - Stadthalle Lauffen", "A0788", None),
    ("Kinomobil Stuttgart - Kelter Winzerhausen", "Kinomobil Stuttgart - Kelter Winzerhausen", "A2132", None),
    ("Kinomobil Stuttgart - Gemeindehalle Löchgau", "Kinomobil Stuttgart - Gemeindehalle Löchgau", "A0797", Some("/kino/loechgau/kinomobil-stuttgart-gemeindehalle-41601")),
    ("Kinomobil Stuttgart - Kulturzentrum Alte Schule", "Kinomobil Stuttgart - Kulturzentrum Alte Schule", "A0785", None),
    ("Open-Air-Kino Forchtenberg", "Open-Air-Kino Forchtenberg", "A2440", None),
    ("Kinomobil Stuttgart - Gewölbekeller im Rathaus", "Kinomobil Stuttgart - Gewölbekeller im Rathaus", "A0786", None),
    ("Kinomobil Stuttgart - Bürgerhaus Vordere Kelter", "Kinomobil Stuttgart - Bürgerhaus Vordere Kelter", "A0807", Some("/kino/erligheim/kinomobil-stuttgart-buergerhaus-vordere-kelter-42549"))
  ))
  private def r_schwaebisch_gmuend: R = ("schwaebisch-gmuend", "Schwäbisch Gmünd", "Baden-Württemberg", 48.79947, 9.79809, Seq("Schwäbisch Gmünd", "Heidenheim", "Schorndorf", "Schwäbisch Hall", "Göppingen", "Geislingen an der Steige", "Ebersbach", "Murrhardt", "Gaildorf", "Rudersberg", "Donzdorf", "Königsbronn", "Deggingen", "Schlat"), Seq(
    ("Brazil", "Brazil", "A0105", Some("/kino/schwaebisch-gmuend/brazil-32981")),
    ("Traumpalast Schwäbisch Gmünd", "Traumpalast Schwäbisch Gmünd", "A1193", Some("/kino/schwaebisch-gmuend/traumpalast-50596")),
    ("Capitol Heidenheim", "Capitol Heidenheim", "A0140", Some("/kino/heidenheim/capitol-32788")),
    ("Kino-Center Heidenheim", "Kino-Center Heidenheim", "A0761", Some("/kino/heidenheim/kinocenter-32789")),
    ("Club Manufaktur", "Club Manufaktur", "A0849", Some("/kino/schorndorf/club-manufaktur-kino-kleine-fluchten-32976")),
    ("Traumpalast Schorndorf", "Traumpalast Schorndorf", "A0067", Some("/kino/schorndorf/traumpalast-schorndorf-66137")),
    ("Kino im Schafstall", "Kino im Schafstall", "A1497", Some("/kino/schwaebisch-hall/kino-im-schafstall-32984")),
    ("Lichtspielhaus Schwäbisch Hall", "Lichtspielhaus Schwäbisch Hall", "A0585", None),
    ("Staufen-Movieplex", "Staufen-Movieplex", "A1151", None),
    ("Gloria Kino Center Geislingen an der Steige", "Gloria Kino Center Geislingen an der Steige", "A0582", Some("/kino/geislingen-an-der-steige/gloria-kino-center-32769")),
    ("Film Theater Ebersbach", "Film Theater Ebersbach", "A0539", None),
    ("Kommunales Kino Murrhardt e.V.", "Kommunales Kino Murrhardt e.V.", "A1459", None),
    ("Sonnen-Lichtspiele", "Sonnen-Lichtspiele", "A1140", Some("/kino/gaildorf/sonnenlichtspiele-32764")),
    ("Löwenlichtspiele Rudersberg", "Löwenlichtspiele Rudersberg", "A0674", Some("/kino/rudersberg/loewenlichtspiele-77336")),
    ("Sommerkino Donzdorf", "Sommerkino Donzdorf", "A2544", None),
    ("Kinomobil Stuttgart - Ketteler-Haus", "Kinomobil Stuttgart - Ketteler-Haus", "A0809", Some("/kino/koenigsbronn/kinomobil-stuttgart-ketteler-haus-53339")),
    ("Kinomobil Stuttgart - Feuerwehrhaus Deggingen", "Kinomobil Stuttgart - Feuerwehrhaus Deggingen", "A2128", None),
    ("Kinomobil Stuttgart - Bürgerhaus", "Kinomobil Stuttgart - Bürgerhaus", "A0808", None)
  ))
  private def r_hannover: R = ("hannover", "Hannover", "Niedersachsen", 52.37052, 9.73322, Seq("Hannover", "Hildesheim", "Lehrte", "Langenhagen", "Neustadt am Rübenberge", "Bad Nenndorf"), Seq(
    ("Apollo Hannover", "Apollo Hannover", "A0035", Some("/kino/hannover/apollo-32473")),
    ("Astor Grand Cinema", "Astor Grand Cinema", "A1370", Some("/kino/hannover/astor-grand-cinema-85797")),
    ("CinemaxX Raschplatz", "CinemaxX Raschplatz", "A0296", Some("/kino/hannover/cinemaxx-raschplatz-42357")),
    ("Hochhaus Lichtspiele", "Hochhaus Lichtspiele", "A0608", Some("/kino/hannover/hochhaus-lichtspiele-32470")),
    ("Kino am Raschplatz", "Kino am Raschplatz", "A0590", Some("/kino/hannover/kino-am-raschplatz-60676")),
    ("Kino im Sprengel", "Kino im Sprengel", "A1015", Some("/kino/hannover/kino-im-sprengel-32469")),
    ("Kommunales Kino im Künstlerhaus", "Kommunales Kino im Künstlerhaus", "A1495", Some("/kino/hannover/kommunales-kino-im-kuenstlerhaus-32475")),
    ("Open Air im Theaterhof", "Open Air im Theaterhof", "A2868", None),
    ("Puschenkino Puki", "Puschenkino Puki", "A2818", None),
    ("Seh-Fest Gilde Parkbühne", "Seh-Fest Gilde Parkbühne", "A2472", None),
    ("Hochschulkino im Audimax", "Hochschulkino im Audimax", "A2876", None),
    ("Thega Filmpalast Hildesheim", "Thega Filmpalast Hildesheim", "A1181", Some("/kino/hildesheim/thegafilmpalast-32491")),
    ("Das Andere Kino", "Das Andere Kino", "A0431", Some("/kino/lehrte/das-andere-kino-32524")),
    ("Open Air im Stadtpark", "Open Air im Stadtpark", "A2867", None),
    ("CineMotion Langenhagen", "CineMotion Langenhagen", "A1251", Some("/kino/langenhagen-hannover/cinemotion-37927")),
    ("Cinema im Leinepark", "Cinema im Leinepark", "A2873", Some("/kino/neustadt-am-ruebenberge/cinema-im-leinepark-93073")),
    ("Phoenix Kurlichtspiele Bad Nenndorf", "Phoenix Kurlichtspiele Bad Nenndorf", "A0707", Some("/kino/bad-nenndorf/phoenix-kurlichtspiele-56637"))
  ))
  private def r_braunschweig: R = ("braunschweig", "Braunschweig", "Niedersachsen", 52.26594, 10.52673, Seq("Braunschweig", "Wolfsburg", "Salzgitter", "Helmstedt", "Wolfenbüttel", "Peine", "Gifhorn", "Königslutter am Elm", "Grasleben"), Seq(
    ("C1 Cinema Braunschweig", "C1 Cinema Braunschweig", "A0284", None),
    ("Roter Saal", "Roter Saal", "A2389", Some("/kino/braunschweig/roter-saal-75108")),
    ("SchunterKino", "SchunterKino", "A2745", Some("/kino/braunschweig/schunterkino-88539")),
    ("Universum Filmtheater", "Universum Filmtheater", "A1240", Some("/kino/braunschweig/kino-universum-53716")),
    ("CinemaxX Wolfsburg", "CinemaxX Wolfsburg", "A0283", Some("/kino/wolfsburg/cinemaxx-42023")),
    ("Delphin Palast", "Delphin Palast", "A0437", Some("/kino/wolfsburg/delphin-palast-65736")),
    ("Kino im Hallenbad - Wolfsburg", "Kino im Hallenbad - Wolfsburg", "A2385", Some("/kino/wolfsburg/kino-im-hallenbad-wolfsburg-75112")),
    ("Metropol Theater Fallersleben", "Metropol Theater Fallersleben", "A0949", Some("/kino/wolfsburg/metropol-theater-fallersleben-32647")),
    ("Cinema in der Angerpassage", "Cinema in der Angerpassage", "A0253", None),
    ("City-Theater Kultiplex", "City-Theater Kultiplex", "A1377", Some("/kino/salzgitter/kultiplex-32602")),
    ("Camera am Holzberg", "Camera am Holzberg", "A1327", Some("/kino/helmstedt/camera-am-holzberg-32485")),
    ("Roxy- Theater -Lichtspiele", "Roxy- Theater -Lichtspiele", "A1093", Some("/kino/helmstedt/roxy-theater-lichtspiele-32486")),
    ("Filmpalast Wolfenbüttel (Juliusstadt)", "Filmpalast Wolfenbüttel (Juliusstadt)", "A1422", Some("/kino/wolfenbuettel-niedersachsen/filmpalast-32646")),
    ("Astoria-Filmtheater", "Astoria-Filmtheater", "A0020", Some("/kino/peine/astoriafilmtheater-72276")),
    ("Kinocenter am Steinweg", "Kinocenter am Steinweg", "A0574", Some("/kino/gifhorn/kinocenter-am-steinweg-71836")),
    ("Kammerlichtspiele Könnigslutter am Elm", "Kammerlichtspiele Könnigslutter am Elm", "A0637", None),
    ("Autokino in Grasleben", "Autokino in Grasleben", "A2937", None)
  ))
  private def r_villingen_schwenningen: R = ("villingen-schwenningen", "Villingen-Schwenningen", "Baden-Württemberg", 48.06226, 8.49358, Seq("Villingen-Schwenningen", "Alpirsbach", "Tuttlingen", "Oberndorf am Neckar", "Furtwangen im Schwarzwald", "Rottweil", "Donaueschingen", "Schramberg", "Trossingen", "Triberg im Schwarzwald"), Seq(
    ("Blue Boxx", "Blue Boxx", "A0102", Some("/kino/villingenschwenningen/blue-boxx-65796")),
    ("CineStar Villingen-Schwenningen", "CineStar Villingen-Schwenningen", "A2887", Some("/kino/villingenschwenningen/cinestar-villingenschwenningen-46256")),
    ("Kommunales Kino Guckloch Schwarzwald-Baar-Kreis", "Kommunales Kino Guckloch Schwarzwald-Baar-Kreis", "A0863", Some("/kino/villingenschwenningen/kommunales-kino-guckloch-65297")),
    ("Kino unterm Sternenhimmel Alpirsbach", "Kino unterm Sternenhimmel Alpirsbach", "A2486", None),
    ("Open Air Kino im Kreuzgarten", "Open Air Kino im Kreuzgarten", "A1818", Some("/kino/alpirsbach/open-air-kino-im-kreuzgarten-des-klosters-63816")),
    ("Subiaco Galerie", "Subiaco Galerie", "A0566", None),
    ("Open Air Kino Tuttlingen", "Open Air Kino Tuttlingen", "A1999", None),
    ("Scala Tuttlingen", "Scala Tuttlingen", "A1109", Some("/kino/tuttlingen/scala-33035")),
    ("KKK-Filmtheater", "KKK-Filmtheater", "A0848", None),
    ("Sommernachtskino im Klosterhof in Oberndorf", "Sommernachtskino im Klosterhof in Oberndorf", "A1987", None),
    ("Kommunales Guckloch-Kino Furtwangen e.V.", "Kommunales Guckloch-Kino Furtwangen e.V.", "A0855", Some("/kino/furtwangen-im-schwarzwald/kommunales-gucklochkino-furtwangen-e.v.-69579")),
    ("Open Air Kino Furtwangen", "Open Air Kino Furtwangen", "A1774", Some("/kino/furtwangen-im-schwarzwald/open-air-kino-furtwangen-70859")),
    ("Central-Kino Rottweil", "Central-Kino Rottweil", "A0199", Some("/kino/rottweil/centralkino-32970")),
    ("Kommunales Kino Guckloch Donaueschingen", "Kommunales Kino Guckloch Donaueschingen", "A0864", Some("/kino/donaueschingen/kommunales-kino-guckloch-93095")),
    ("Subiaco Schramberg", "Subiaco Schramberg", "A1169", Some("/kino/schramberg/subiaco-schramberg-54037")),
    ("Kommunales Kino Trossingen", "Kommunales Kino Trossingen", "A2855", None),
    ("Kronen-Lichtspiele Triberg im Schwarzwald", "Kronen-Lichtspiele Triberg im Schwarzwald", "A1526", None)
  ))
  private def r_dresden: R = ("dresden", "Dresden", "Sachsen", 51.05089, 13.73832, Seq("Dresden", "Pirna", "Weinböhla", "Radeburg"), Seq(
    ("CLUB PASSAGE", "CLUB PASSAGE", "A0410", Some("/kino/dresden/club-passage-31498")),
    ("CinemaxX Dresden", "CinemaxX Dresden", "A0285", Some("/kino/dresden/cinemaxx-42197")),
    ("Cineplex Kristallpalast Dresden", "Cineplex Kristallpalast Dresden", "A1222", Some("/kino/dresden/cineplex-kristallpalast-31503")),
    ("Filmnächte am Elbufer", "Filmnächte am Elbufer", "A1801", Some("/kino/dresden/filmnaechte-am-elbufer-31486")),
    ("Filmtheater Schauburg", "Filmtheater Schauburg", "A1477", Some("/kino/dresden/filmtheater-schauburg-31491")),
    ("Kino im Kasten - Das Studentenkino", "Kino im Kasten - Das Studentenkino", "A0723", Some("/kino/dresden/kino-im-kasten-das-studentenkino-31501")),
    ("Kino in der Scheune Dresden", "Kino in der Scheune Dresden", "A1700", Some("/kino/dresden/kino-in-der-scheune-39501")),
    ("Programmkino Ost", "Programmkino Ost", "A1045", Some("/kino/dresden/programmkino-ost-31492")),
    ("Rundkino Dresden", "Rundkino Dresden", "A2018", Some("/kino/dresden/rundkino-dresden-71841")),
    ("Thalia Dresden", "Thalia Dresden", "A1173", Some("/kino/dresden/thalia-69577")),
    ("UCI Elbe Park", "UCI Elbe Park", "A1208", Some("/kino/dresden/uci-kinowelt-elbe-park-40872")),
    ("Zentralkino Dresden", "Zentralkino Dresden", "G011F", Some("/kino/dresden/zentralkino-92541")),
    ("k.i.d. - Kino im Dach", "k.i.d. - Kino im Dach", "A0651", Some("/kino/dresden/k.i.d.-kino-im-dach-31487")),
    ("Filmpalast Pirna", "Filmpalast Pirna", "A0511", Some("/kino/pirna/filmpalast-31630")),
    ("Openair Zentralgasthof Weinböhla GmbH", "Openair Zentralgasthof Weinböhla GmbH", "A2202", None),
    ("Kinobar", "Kinobar", "A1759", None)
  ))
  private def r_jena: R = ("jena", "Jena", "Thüringen", 50.92878, 11.5899, Seq("Jena", "Weimar", "Gera", "Rudolstadt", "Saalfeld", "Bad Klosterlausnitz"), Seq(
    ("CineStar Jena", "CineStar Jena", "A0328", Some("/kino/jena/cinestar-37109")),
    ("Filmarena Open-Air Kino Theatervorplatz", "Filmarena Open-Air Kino Theatervorplatz", "A1844", Some("/kino/jena/filmarena-openair-kino-theatervorplatz-40658")),
    ("Kino am Markt", "Kino am Markt", "A2732", Some("/kino/jena/kino-am-markt-87777")),
    ("Kino im Schillerhof", "Kino im Schillerhof", "A0729", Some("/kino/jena/kino-im-schillerhof-71316")),
    ("CineStar Weimar", "CineStar Weimar", "A0360", Some("/kino/weimar/cinestar-der-filmpalast-31695")),
    ("Kommunales Kino im mon ami", "Kommunales Kino im mon ami", "A0867", None),
    ("Lichthaus Kino im Straßenbahndepot/e-werk", "Lichthaus Kino im Straßenbahndepot/e-werk", "A0906", Some("/kino/weimar/lichthaus-kino-im-strassenbahndepotewerk-53997")),
    ("Open Air am Lichthaus-Kino", "Open Air am Lichthaus-Kino", "A2783", None),
    ("Kino im COMMA", "Kino im COMMA", "A0416", None),
    ("Metropol", "Metropol", "A2677", Some("/kino/gera/metropol-kino-31532")),
    ("Open Air im Comma-Garten", "Open Air im Comma-Garten", "A2759", None),
    ("Cineplex Rudolstadt", "Cineplex Rudolstadt", "A0307", Some("/kino/rudolstadt/cineplex-42817")),
    ("Freiluftkino im Garten des Schillerhauses", "Freiluftkino im Garten des Schillerhauses", "A2443", None),
    ("Uferpalast Rudolstadt", "Uferpalast Rudolstadt", "A2390", None),
    ("Cineplex Saalfeld", "Cineplex Saalfeld", "A0157", Some("/kino/saalfeld-saale/cineplex-57836")),
    ("Holzlandkino", "Holzlandkino", "A0618", Some("/kino/bad-klosterlausnitz/holzlandkino-31345"))
  ))
  private def r_schweinfurt: R = ("schweinfurt", "Schweinfurt", "Bayern", 50.04937, 10.22175, Seq("Schweinfurt", "Würzburg", "Bad Neustadt an der Saale", "Kitzingen", "Bad Kissingen", "Karlstadt am Main", "Hammelburg", "Bad Königshofen im Grabfeld", "Dettelbach", "Sommerach"), Seq(
    ("Filmwelt Schweinfurt", "Filmwelt Schweinfurt", "A2475", Some("/kino/schweinfurt/filmwelt-75938")),
    ("KuK Kino und Kneipe", "KuK Kino und Kneipe", "A0874", Some("/kino/schweinfurt/kuk-kino-und-kneipe-54356")),
    ("Open Air am KuK", "Open Air am KuK", "A2532", None),
    ("Weltbio Kinocenter", "Weltbio Kinocenter", "A1272", Some("/kino/schweinfurt/weltbio-kinocenter-32259")),
    ("Central Programmkino", "Central Programmkino", "A2621", Some("/kino/wuerzburg/central-78717")),
    ("Central im Bürgerbräu", "Central im Bürgerbräu", "A2742", Some("/kino/wuerzburg/central-im-buergerbraeu-88817")),
    ("CinemaxX Würzburg", "CinemaxX Würzburg", "A0263", Some("/kino/wuerzburg/cinemaxx-38998")),
    ("Rex Kinos", "Rex Kinos", "A1065", Some("/kino/bad-neustadt-an-der-saale/rex-kinos-31969")),
    ("Starlight Kinos", "Starlight Kinos", "A1149", Some("/kino/bad-neustadt-an-der-saale/starlight-kinos-31970")),
    ("Roxy Kino Kitzingen", "Roxy Kino Kitzingen", "A1738", Some("/kino/kitzingen/roxy-91837")),
    ("Universum Kino Palast", "Universum Kino Palast", "A1657", Some("/kino/bad-kissingen/universum-kino-palast-31962")),
    ("Burg-Lichtspiele (Karlstadt am Main)", "Burg-Lichtspiele (Karlstadt am Main)", "A0118", None),
    ("Kino in der Stadtbibliothek Hammelburg", "Kino in der Stadtbibliothek Hammelburg", "A2348", None),
    ("Stadtsaal-Lichtspiele", "Stadtsaal-Lichtspiele", "A1463", None),
    ("Cineworld Mainfrankenpark Dettelbach", "Cineworld Mainfrankenpark Dettelbach", "A0387", Some("/kino/dettelbach/cineworld-erlebniskino-mainfrankenpark-39999")),
    ("Open Air Kino Winzerkeller Sommerach", "Open Air Kino Winzerkeller Sommerach", "A2277", Some("/kino/sommerach/open-air-kino-im-winzerkeller-73921"))
  ))
  private def r_freiburg: R = ("freiburg", "Freiburg", "Baden-Württemberg", 47.9959, 7.85222, Seq("Freiburg", "Freiburg im Breisgau", "Emmendingen", "Waldkirch", "Bad Krozingen", "Breisach am Rhein", "Titisee-Neustadt", "Kenzingen", "Schallstadt", "Buggingen"), Seq(
    ("CinemaxX Freiburg", "CinemaxX Freiburg", "A0291", Some("/kino/freiburg-im-breisgau/cinemaxx-35079")),
    ("Friedrichsbau-Apollo", "Friedrichsbau-Apollo", "A0040", Some("/kino/freiburg-im-breisgau/friedrichsbauapollo-65198")),
    ("Harmonie Freiburg", "Harmonie Freiburg", "A1218", Some("/kino/freiburg-im-breisgau/harmonie-34481")),
    ("Kandelhof", "Kandelhof", "A0644", Some("/kino/freiburg-im-breisgau/kandelhof-32046")),
    ("Kommunales Kino - Im Alten Wiehrebahnhof", "Kommunales Kino - Im Alten Wiehrebahnhof", "A0862", None),
    ("Sommernachts-Kino", "Sommernachts-Kino", "A2036", None),
    ("aka-Filmclub", "aka-Filmclub", "A1474", None),
    ("Maja", "Maja", "A2808", None),
    ("Kommunales Kino Klappe 11", "Kommunales Kino Klappe 11", "A2805", None),
    ("Joki Kino am Bahnhof", "Joki Kino am Bahnhof", "A0722", None),
    ("Engel-Lichtspiele", "Engel-Lichtspiele", "A1403", None),
    ("Krone-Theater", "Krone-Theater", "A0873", Some("/kino/titiseeneustadt/kronetheater-32277")),
    ("Kinomobil Stuttgart - Johann-Philipp-Glock-Schule", "Kinomobil Stuttgart - Johann-Philipp-Glock-Schule", "A2100", None),
    ("Kino im Rathaus", "Kino im Rathaus", "A0728", Some("/kino/buggingen/kino-im-rathaus-32000"))
  ))
  private def r_wuppertal: R = ("wuppertal", "Wuppertal", "Nordrhein-Westfalen", 51.25627, 7.14816, Seq("Wuppertal", "Remscheid", "Lüdenscheid", "Hilden", "Mettmann", "Wermelskirchen", "Gevelsberg", "Schwelm", "Radevormwald"), Seq(
    ("Cinema Wuppertal", "Cinema Wuppertal", "A0381", Some("/kino/wuppertal/cinema-13463")),
    ("CinemaxX Wuppertal", "CinemaxX Wuppertal", "A0292", Some("/kino/wuppertal/cinemaxx-35200")),
    ("Rex Wuppertal", "Rex Wuppertal", "A2680", Some("/kino/wuppertal/rex-86017")),
    ("Talflimmern Open-Air-Kino", "Talflimmern Open-Air-Kino", "A1918", Some("/kino/wuppertal/talflimmern-openairkino-alte-feuerwache-58797")),
    ("CineStar Remscheid", "CineStar Remscheid", "A2901", Some("/kino/remscheid/cinestar-91597")),
    ("Open Air Kino Remscheid", "Open Air Kino Remscheid", "A2826", None),
    ("Open Air an der Gelben Villa", "Open Air an der Gelben Villa", "A2810", None),
    ("Filmpalast Lüdenscheid", "Filmpalast Lüdenscheid", "A0519", Some("/kino/luedenscheid/filmpalast-5201")),
    ("Park-Theater Lüdenscheid", "Park-Theater Lüdenscheid", "A1030", Some("/kino/luedenscheid/parktheater-30120")),
    ("Lux Lichtspiele", "Lux Lichtspiele", "A0933", Some("/kino/hilden/lux-lichtspiele-946")),
    ("Weltspiegel-Kino-Center", "Weltspiegel-Kino-Center", "A1274", Some("/kino/mettmann/weltspiegelkinocenter-31862")),
    ("Film-Eck", "Film-Eck", "A1414", Some("/kino/wermelskirchen/filmeck-31938")),
    ("Filmriss Kino", "Filmriss Kino", "A0524", Some("/kino/gevelsberg/filmriss-kino-66337")),
    ("Kinocenter Schwelm", "Kinocenter Schwelm", "A0776", Some("/kino/schwelm/kinocenter-schwelm-798")),
    ("Corso Kinocenter", "Corso Kinocenter", "A0425", Some("/kino/radevormwald/corso-kinocenter-756"))
  ))
  private def r_ravensburg: R = ("ravensburg", "Ravensburg", "Baden-Württemberg", 47.78198, 9.61062, Seq("Ravensburg", "Lindau", "Weingarten", "Bad Saulgau", "Wangen im Allgäu", "Bad Waldsee", "Tettnang", "Isny im Allgäu", "Lindenberg im Allgäu", "Kressbronn", "Argenbühl"), Seq(
    ("CineParC Ravensburg", "CineParC Ravensburg", "A1393", None),
    ("Kinozentrum Frauentor", "Kinozentrum Frauentor", "A0845", None),
    ("Club Vaudeville", "Club Vaudeville", "A0411", Some("/kino/lindau/club-vaudeville-lindau-e.v.-69737")),
    ("Parktheater und Studio", "Parktheater und Studio", "A1031", Some("/kino/lindau/parktheater-und-studio-32834")),
    ("Kulturzentrum Linse", "Kulturzentrum Linse", "A1839", Some("/kino/weingarten/kulturzentrum-linse-e.-v.-72297")),
    ("Open Air Kino im Schlösse Hof", "Open Air Kino im Schlösse Hof", "A2496", Some("/kino/weingarten/open-air-kino-im-schloessle-hof-76023")),
    ("Autokino Bad Saulgau am Fesplatz", "Autokino Bad Saulgau am Fesplatz", "A2922", None),
    ("Kino Saulgau", "Kino Saulgau", "A0750", Some("/kino/bad-saulgau/kino-saulgau-32973")),
    ("Lichtspielhaus Wangen im Allgäu", "Lichtspielhaus Wangen im Allgäu", "A1163", None),
    ("seenema - Stadtkino Bad Waldsee eG", "seenema - Stadtkino Bad Waldsee eG", "A2722", Some("/kino/bad-waldsee/seenema-stadtkino-bad-waldsee-eg-87297")),
    ("KiTT - Kino und Kleinkunst Tettnang e.V.", "KiTT - Kino und Kleinkunst Tettnang e.V.", "A1518", None),
    ("Neues Ringtheater", "Neues Ringtheater", "A0992", None),
    ("Neues Krone Kino", "Neues Krone Kino", "A1574", None),
    ("Kinomobil Stuttgart - Lände-Cafe", "Kinomobil Stuttgart - Lände-Cafe", "A1761", Some("/kino/kressbronn/kinomobil-stuttgart-cafe-laende-41859")),
    ("Kinomobil Stuttgart - Bürgersaal Rathaus", "Kinomobil Stuttgart - Bürgersaal Rathaus", "A1762", Some("/kino/argenbuehl/kinomobil-stuttgart-buergersaal-rathaus-42551"))
  ))
  private def r_regensburg: R = ("regensburg", "Regensburg", "Bayern", 49.01513, 12.10161, Seq("Regensburg", "Kelheim", "Abensberg", "Burglengenfeld", "Maxhütte-Haidhof", "Nittenau", "Wald"), Seq(
    ("CinemaxX Regensburg", "CinemaxX Regensburg", "A0289", Some("/kino/regensburg/cinemaxx-37053")),
    ("Filmgalerie im Leeren Beutel", "Filmgalerie im Leeren Beutel", "A0497", Some("/kino/regensburg/filmgalerie-im-leeren-beutel-60781")),
    ("Garbo", "Garbo", "A1436", Some("/kino/regensburg/garbo-32949")),
    ("Kinos im Andreasstadel", "Kinos im Andreasstadel", "A1280", Some("/kino/regensburg/kinos-im-andreasstadl-66757")),
    ("Open Air Kino Auf Schloss Pürkelgut", "Open Air Kino Auf Schloss Pürkelgut", "A1876", None),
    ("Ostentor", "Ostentor", "A1581", Some("/kino/regensburg/ostentor-32948")),
    ("Regina Filmtheater Regensburg", "Regina Filmtheater Regensburg", "A1052", Some("/kino/regensburg/regina-filmtheater-32950")),
    ("Turm-Theater Regensburg", "Turm-Theater Regensburg", "A1195", None),
    ("Kelheimer Lichtspiele", "Kelheimer Lichtspiele", "A1462", None),
    ("Roxy Kino", "Roxy Kino", "A1086", Some("/kino/abensberg/roxy-kino-32654")),
    ("Starmexx - Erlebniskino", "Starmexx - Erlebniskino", "A2664", Some("/kino/burglengenfeld/starmexx-erlebniskino-80257")),
    ("Kinocenter Maxhütte-Haidhof", "Kinocenter Maxhütte-Haidhof", "A0656", None),
    ("Kino-Center Nittenau", "Kino-Center Nittenau", "A1461", Some("/kino/nittenau/kinocenter-32903")),
    ("Kinomobil Stuttgart - Feuerwehrhaus Wald", "Kinomobil Stuttgart - Feuerwehrhaus Wald", "A1937", None)
  ))
  private def r_kiel: R = ("kiel", "Kiel", "Schleswig-Holstein", 54.32133, 10.13489, Seq("Kiel", "Neumünster", "Rendsburg", "Eckernförde", "Preetz", "Plön", "Bordesholm"), Seq(
    ("CinemaxX Kiel", "CinemaxX Kiel", "A0279", Some("/kino/kiel/cinemaxx-32509")),
    ("Hansafilmpalast", "Hansafilmpalast", "A2842", None),
    ("Kommunales Kino in der Pumpe", "Kommunales Kino in der Pumpe", "A1522", Some("/kino/kiel/kommunales-kino-in-der-pumpe-32510")),
    ("Metro-Kino im Schloßhof", "Metro-Kino im Schloßhof", "A1750", Some("/kino/kiel/metrokino-im-schlosshof-72536")),
    ("Studio - Filmtheater am Dreiecksplatz", "Studio - Filmtheater am Dreiecksplatz", "A0993", Some("/kino/kiel/studio-filmtheater-am-dreiecksplatz-44339")),
    ("Traum-Kino", "Traum-Kino", "A0694", Some("/kino/kiel/traumkino-32511")),
    ("CineStar Neumünster", "CineStar Neumünster", "A1372", Some("/kino/neumuenster/cinestar-neumuenster-90577")),
    ("KDW Neumünster", "KDW Neumünster", "A2655", Some("/kino/neumuenster/kdwneumuenster-81837")),
    ("Kino-Center Rendsburg", "Kino-Center Rendsburg", "A0775", None),
    ("Schauburg Rendsburg", "Schauburg Rendsburg", "A1623", Some("/kino/rendsburg/schauburg-33966")),
    ("Das Haus", "Das Haus", "A0866", None),
    ("Captiol Cine Center", "Captiol Cine Center", "A1747", Some("/kino/preetz-holstein/capitol-cine-center-32591")),
    ("Astra-Filmtheater", "Astra-Filmtheater", "A1300", Some("/kino/ploen-holstein/astrafilmtheater-32590")),
    ("Savoy (Bordesholm)", "Savoy (Bordesholm)", "A1614", Some("/kino/bordesholm/savoy-32346"))
  ))
  private def r_konstanz: R = ("konstanz", "Konstanz", "Baden-Württemberg", 47.66033, 9.17582, Seq("Konstanz", "Friedrichshafen", "Überlingen", "Singen", "Singen am Hohentwiel", "Radolfzell am Bodensee", "Markdorf", "Gottmadingen", "Immenstaad am Bodensee", "Reichenau"), Seq(
    ("CineStar Konstanz", "CineStar Konstanz", "A0338", Some("/kino/konstanz-universitaetsstadt/cinestar-konstanz-60996")),
    ("Open Air am Neuwerk", "Open Air am Neuwerk", "A2774", None),
    ("Zebra", "Zebra", "A1287", Some("/kino/konstanz-universitaetsstadt/zebra-kommunales-kino-konstanz-e.v.-32128")),
    ("Cineplex Friedrichshafen", "Cineplex Friedrichshafen", "A0313", Some("/kino/friedrichshafen/cineplex-friedrichshafen-65397")),
    ("Kulturhaus Caserne Kino Studio 17", "Kulturhaus Caserne Kino Studio 17", "A0754", Some("/kino/friedrichshafen/kulturhaus-caserne-kino-66317")),
    ("Cinegreth", "Cinegreth", "A0226", Some("/kino/ueberlingen/cinegreth-37137")),
    ("Kammer-Lichtspiele Überlingen", "Kammer-Lichtspiele Überlingen", "A0639", Some("/kino/ueberlingen/kammerlichtspiele-32279")),
    ("Cineplex Singen", "Cineplex Singen", "A0323", Some("/kino/singen/cineplex-42605")),
    ("Kino in der Gems", "Kino in der Gems", "A0570", None),
    ("Universum-Nostalgiekino", "Universum-Nostalgiekino", "A2916", None),
    ("Theaterstadel", "Theaterstadel", "A1178", Some("/kino/markdorf/theaterstadel-32177")),
    ("Kinomobil Stuttgart - Neues Rathaus", "Kinomobil Stuttgart - Neues Rathaus", "A1939", None),
    ("Kinomobil Stuttgart - Strandbad Aquastaad", "Kinomobil Stuttgart - Strandbad Aquastaad", "A1951", None),
    ("Kinomobil Stuttgart - Hochwart-Wiese", "Kinomobil Stuttgart - Hochwart-Wiese", "A1953", None)
  ))
  private def r_landsberg_am_lech: R = ("landsberg-am-lech", "Landsberg am Lech", "Bayern", 48.04819, 10.88282, Seq("Landsberg am Lech", "Bad Wörishofen", "Diessen am Ammersee", "Weilheim", "Weilheim in Oberbayern", "Kaufering", "Seefeld", "Türkheim", "Inning-Stegen", "Penzing"), Seq(
    ("Filmforum im Stadttheater Landsberg", "Filmforum im Stadttheater Landsberg", "A2795", Some("/kino/landsberg-am-lech/filmforum-im-stadttheater-landsberg-93063")),
    ("Olympia Filmtheater Landsberg", "Olympia Filmtheater Landsberg", "A1579", Some("/kino/landsberg-am-lech/olympia-filmtheater-32822")),
    ("Open Air Kino Landsberg", "Open Air Kino Landsberg", "A2207", None),
    ("Filmhaus Bad Wörishofen - Lichtspiele am Bahnhof", "Filmhaus Bad Wörishofen - Lichtspiele am Bahnhof", "A1537", None),
    ("Open Air Kino Unter den Linden", "Open Air Kino Unter den Linden", "A2543", None),
    ("Cinema Augustinum Diessen am Ammersee", "Cinema Augustinum Diessen am Ammersee", "A2714", None),
    ("Kinowelt am Ammersee", "Kinowelt am Ammersee", "A0841", None),
    ("Trifthof Kinocenter", "Trifthof Kinocenter", "A1649", Some("/kino/weilheim/kinocenter-trifthof-33074")),
    ("Starlight", "Starlight", "A1633", None),
    ("Filmpalast Kaufering", "Filmpalast Kaufering", "A2617", Some("/kino/kaufering/filmpalast-kaufering-78957")),
    ("Kino Breitwand im Schloß Seefeld", "Kino Breitwand im Schloß Seefeld", "A0713", Some("/kino/seefeld/kino-breitwand-im-schloss-seefeld-37701")),
    ("Filmhaus", "Filmhaus", "A1524", Some("/kino/tuerkheim/filmhaushuber-in-tuerkheim-33034")),
    ("Kino in der Alten Brauerei", "Kino in der Alten Brauerei", "A0735", None),
    ("Cineplex Penzing", "Cineplex Penzing", "A2643", Some("/kino/penzing/cineplex-82997"))
  ))
  private def r_augsburg: R = ("augsburg", "Augsburg", "Bayern", 48.37154, 10.89851, Seq("Augsburg", "Friedberg (Hessen)", "Königsbrunn", "Aichach", "Gersthofen", "Meitingen", "Wertingen"), Seq(
    ("CineStar Augsburg", "CineStar Augsburg", "A0350", Some("/kino/augsburg-bayern/cinestar-37095")),
    ("CinemaxX Augsburg", "CinemaxX Augsburg", "A0276", Some("/kino/augsburg-bayern/cinemaxx-39119")),
    ("Liliom", "Liliom", "A1541", Some("/kino/augsburg-bayern/liliom-32675")),
    ("Open Air Kino Lechflimmern Familienbad am Plärrer", "Open Air Kino Lechflimmern Familienbad am Plärrer", "A1800", Some("/kino/augsburg-bayern/open-air-kino-lechflimmern-familienbad-am-plaerrer-78016")),
    ("Savoy Kino", "Savoy Kino", "A1105", Some("/kino/augsburg-bayern/savoy-kino-32677")),
    ("Thalia Augsburg", "Thalia Augsburg", "A1645", Some("/kino/augsburg-bayern/thalia-32673")),
    ("Cineplex Königsbrunn", "Cineplex Königsbrunn", "A0312", Some("/kino/koenigsbrunn-bei-augsburg/cineplex-35123")),
    ("Cineplex Aichach", "Cineplex Aichach", "A1374", Some("/kino/aichach/cineplex-32655")),
    ("Open Air Seebühne", "Open Air Seebühne", "A2539", None),
    ("Cineplex Meitingen", "Cineplex Meitingen", "A0217", Some("/kino/meitingen/cineplex-32844")),
    ("Filmtheater Wertingen", "Filmtheater Wertingen", "A2120", Some("/kino/wertingen/filmtheater-33064"))
  ))
  private def r_offenburg: R = ("offenburg", "Offenburg", "Baden-Württemberg", 48.47377, 7.94495, Seq("Offenburg", "Freudenstadt", "Lahr", "Lahr am Schwarzwald", "Kehl", "Achern", "Oberkirch", "Haslach im Kinzigtal", "Rust", "Rheinmünster"), Seq(
    ("FORUM Offenburg", "FORUM Offenburg", "A2334", Some("/kino/offenburg/forum-74717")),
    ("Kommunales Kino im KiK", "Kommunales Kino im KiK", "A2854", None),
    ("Sommer Kino Nächte Ortenau", "Sommer Kino Nächte Ortenau", "A2002", None),
    ("Central Freudenstadt", "Central Freudenstadt", "A0186", Some("/kino/freudenstadt/central-70397")),
    ("Subiaco im Kurhaus", "Subiaco im Kurhaus", "A1170", Some("/kino/freudenstadt/subiaco-im-kurhaus-58436")),
    ("FORUM Lahr", "FORUM Lahr", "A2648", Some("/kino/lahr/forum-84797")),
    ("Open Air Kino im Innenhof Schlachthof", "Open Air Kino im Innenhof Schlachthof", "A1986", None),
    ("Kino-Center Kehl", "Kino-Center Kehl", "A1507", Some("/kino/kehl/kinocenter-32122")),
    ("Tivoli-Filmtheater", "Tivoli-Filmtheater", "A1185", Some("/kino/achern/tivoli-kommunales-kino-31945")),
    ("Open-Air Oberkirch", "Open-Air Oberkirch", "A2525", Some("/kino/oberkirch/open-air-kino-76097")),
    ("Rio + Scala", "Rio + Scala", "A1081", None),
    ("Magic Cinema im Europa Park", "Magic Cinema im Europa Park", "A0936", None),
    ("Kinomobil Stuttgart - Festhalle Rheinmünster-Schwarzach", "Kinomobil Stuttgart - Festhalle Rheinmünster-Schwarzach", "A0802", Some("/kino/rheinmuenster/kinomobil-stuttgart-festhalle-schwarzach-41837"))
  ))
  private def r_tauberbischofsheim: R = ("tauberbischofsheim", "Tauberbischofsheim", "Baden-Württemberg", 49.62472, 9.66278, Seq("Tauberbischofsheim", "Wertheim am Main", "Bad Mergentheim", "Lauda-Königshofen", "Walldürn", "Ochsenfurt", "Marktheidenfeld", "Weikersheim", "Boxberg", "Dörzbach", "Assamstadt"), Seq(
    ("Filmtheater Badischer Hof", "Filmtheater Badischer Hof", "A0541", None),
    ("Open Air Kino Rotary-Benefiz", "Open Air Kino Rotary-Benefiz", "A1995", None),
    ("Roxy Wertheim", "Roxy Wertheim", "A1087", Some("/kino/wertheim-am-main/roxy-33911")),
    ("Movies Bad Mergentheim", "Movies Bad Mergentheim", "A2629", Some("/kino/bad-mergentheim/movies-81656")),
    ("Kinomobil Stuttgart - Lauda Sternen Filmtheater", "Kinomobil Stuttgart - Lauda Sternen Filmtheater", "A0794", Some("/kino/laudakoenigshofen/kinomobil-stuttgart-stern-filmtheater-40617")),
    ("Löwenlichtspiele Walldürn", "Löwenlichtspiele Walldürn", "A0922", Some("/kino/wallduern/loewenlichtspiele-32291")),
    ("Casablanca Ochsenfurt", "Casablanca Ochsenfurt", "A0173", Some("/kino/ochsenfurt/casablanca-32206")),
    ("Movie im Luitpoldhaus", "Movie im Luitpoldhaus", "A0965", Some("/kino/marktheidenfeld/movie-im-luitpoldhaus-37691")),
    ("Kinomobil Stuttgart - club-w-71", "Kinomobil Stuttgart - club-w-71", "A0798", Some("/kino/weikersheim/kinomobil-stuttgart-clubw71-41779")),
    ("Kinomobil Stuttgart - Medien und Kulturzentrum", "Kinomobil Stuttgart - Medien und Kulturzentrum", "A0783", None),
    ("Kinomobil Stuttgart - Schlosshof Schloss Eyb", "Kinomobil Stuttgart - Schlosshof Schloss Eyb", "A2227", None),
    ("Kinomobil Stuttgart - Grundschule Assamstadt", "Kinomobil Stuttgart - Grundschule Assamstadt", "A1936", None)
  ))
  private def r_kassel: R = ("kassel", "Kassel", "Hessen", 51.31667, 9.5, Seq("Kassel", "Baunatal", "Hofgeismar", "Fritzlar", "Melsungen", "Borgentreich"), Seq(
    ("Bali-Kinos im KulturBahnhof Kassel", "Bali-Kinos im KulturBahnhof Kassel", "A0078", None),
    ("Cineplex Capitol Kassel", "Cineplex Capitol Kassel", "A1375", Some("/kino/kassel-hessen/cineplex-capitol-32116")),
    ("Filmladen Kassel e.V.", "Filmladen Kassel e.V.", "A0505", Some("/kino/kassel-hessen/filmladen-32114")),
    ("Filmpalast Kassel", "Filmpalast Kassel", "A0354", Some("/kino/kassel-hessen/filmpalast-39197")),
    ("GLORIA-Kino am Ständeplatz", "GLORIA-Kino am Ständeplatz", "A0577", Some("/kino/kassel-hessen/gloria-32118")),
    ("Kasseler Open Air Sommerfilm", "Kasseler Open Air Sommerfilm", "A2063", None),
    ("Cineplex Baunatal", "Cineplex Baunatal", "A2704", Some("/kino/baunatal/cineplex-87219")),
    ("Open-Air-Kino Hofgeismar", "Open-Air-Kino Hofgeismar", "A2289", Some("/kino/hofgeismar/openairkino-hofgeismar-73663")),
    ("Cineplex Royal Fritzlar", "Cineplex Royal Fritzlar", "A0221", Some("/kino/fritzlar/cineplex-royal-38939")),
    ("Freilichtkino Melsungen", "Freilichtkino Melsungen", "A2193", Some("/kino/melsungen/freilichtkino-melsungen-73674")),
    ("Central-Kino Borgentreich", "Central-Kino Borgentreich", "A0201", Some("/kino/borgentreich/centralkino-31778"))
  ))
  private def r_ingolstadt: R = ("ingolstadt", "Ingolstadt", "Bayern", 48.76508, 11.42372, Seq("Ingolstadt", "Neuburg an der Donau", "Pfaffenhofen an der Ilm", "Schrobenhausen", "Eichstätt", "Wolnzach", "Rennertshofen"), Seq(
    ("Altstadtkinos Ingolstadt - Cinema", "Altstadtkinos Ingolstadt - Cinema", "A1369", Some("/kino/ingolstadt/altstadtkinos-cinema-32798")),
    ("Altstadtkinos Ingolstadt - Union", "Altstadtkinos Ingolstadt - Union", "A1225", Some("/kino/ingolstadt/altstadtkinos-union-32800")),
    ("Audi Programmkino", "Audi Programmkino", "A2784", Some("/kino/ingolstadt/audi-programmkino-im-audi-forum-ingolstadt-66437")),
    ("CineStar Ingolstadt", "CineStar Ingolstadt", "A0339", Some("/kino/ingolstadt/cinestar-der-filmpalast-61036")),
    ("Kino Open Air Ingolstadt", "Kino Open Air Ingolstadt", "A2024", None),
    ("Kinopalast Neuburg an der Donau", "Kinopalast Neuburg an der Donau", "A0820", Some("/kino/neuburg-an-der-donau/kinopalast-60557")),
    ("Cineplex Pfaffenhofen", "Cineplex Pfaffenhofen", "A0325", Some("/kino/pfaffenhofen-an-der-ilm/cineplex-32938")),
    ("Herzog-Filmtheater", "Herzog-Filmtheater", "A0605", Some("/kino/schrobenhausen/herzogfilmtheater-32978")),
    ("Filmstudio im alten Stadttheater", "Filmstudio im alten Stadttheater", "A2119", Some("/kino/eichstaett/filmstudio-im-alten-stadttheater-32732")),
    ("Amper-Lichtspiele", "Amper-Lichtspiele", "A1466", Some("/kino/wolnzach/amperlichtspiele-33069")),
    ("Cinema Kulturtreff Rennertshofen", "Cinema Kulturtreff Rennertshofen", "A0232", None)
  ))
  private def r_hechingen: R = ("hechingen", "Hechingen", "Baden-Württemberg", 48.35149, 8.96317, Seq("Hechingen", "Herrenberg", "Nagold", "Albstadt", "Rottenburg am Neckar", "Balingen", "Mössingen"), Seq(
    ("Burgtheater-Kinos", "Burgtheater-Kinos", "A2787", Some("/kino/hechingen/burgtheater-32786")),
    ("Open Air Kino Burg Hohenzollern", "Open Air Kino Burg Hohenzollern", "A2053", None),
    ("Schwanen-Kinos", "Schwanen-Kinos", "A2819", Some("/kino/hechingen/schwanenkino-32787")),
    ("Kommunales Kino Herrenberg", "Kommunales Kino Herrenberg", "A0860", Some("/kino/herrenberg/kommunales-kino-69497")),
    ("Open Air Kino Herrenberg", "Open Air Kino Herrenberg", "A2770", None),
    ("Krone-Lichtspiele", "Krone-Lichtspiele", "A0693", Some("/kino/nagold/kronelichtspiele-32896")),
    ("Open Air im Badepark Nagold", "Open Air im Badepark Nagold", "A2235", None),
    ("Capitol Filmpalast - Albstadt", "Capitol Filmpalast - Albstadt", "A2878", Some("/kino/albstadt/capitol-filmpalast-32657")),
    ("Kino im Waldhorn", "Kino im Waldhorn", "A0732", Some("/kino/rottenburg-am-neckar/kino-im-waldhorn-32968")),
    ("Bali Kino-Palast", "Bali Kino-Palast", "A1308", Some("/kino/balingen/bali-kinopalast-32691")),
    ("Lichtspiele Mössingen", "Lichtspiele Mössingen", "A1536", Some("/kino/moessingen/lichtspiele-32853"))
  ))
  private def r_juelich: R = ("juelich", "Jülich", "Nordrhein-Westfalen", 50.92149, 6.36267, Seq("Jülich", "Düren", "Grevenbroich", "Eschweiler", "Alsdorf", "Erkelenz", "Heinsberg", "Hückelhoven", "Würselen", "Walheim"), Seq(
    ("KuBa", "KuBa", "A1680", Some("/kino/juelich/kuba-70097")),
    ("Open Air Kino Jülich", "Open Air Kino Jülich", "A2765", None),
    ("Das Lumen Filmtheater", "Das Lumen Filmtheater", "A0418", Some("/kino/dueren-rheinland/das-lumen-filmtheater-35439")),
    ("Grefi Kino Grevenbroich", "Grefi Kino Grevenbroich", "A0591", Some("/kino/grevenbroich/grefikinocenter-33182")),
    ("Primus-Kinocenter", "Primus-Kinocenter", "A1041", Some("/kino/eschweiler/primuskinocenter-31796")),
    ("Cinetower Alsdorf", "Cinetower Alsdorf", "A0069", Some("/kino/alsdorf/cinetower-kinopark-alsdorf-58018")),
    ("Gloria Filmpalast Erkelenz", "Gloria Filmpalast Erkelenz", "A0581", Some("/kino/erkelenz/gloria-filmpalast-31794")),
    ("Roxy Filmtheater Heinsberg", "Roxy Filmtheater Heinsberg", "A0248", Some("/kino/heinsberg/roxy-31814")),
    ("Corso-Filmpalast Hilfarth", "Corso-Filmpalast Hilfarth", "A0426", Some("/kino/hueckelhoven/corsofilmpalast-hilfarth-33152")),
    ("Metropolis Würselen", "Metropolis Würselen", "A1563", Some("/kino/wuerselen/metropolis-31943")),
    ("Kinomobil Stuttgart - Römerhaus", "Kinomobil Stuttgart - Römerhaus", "A1954", None)
  ))
  private def r_butzbach: R = ("butzbach", "Butzbach", "Hessen", 50.43395, 8.67122, Seq("Butzbach", "Neu-Anspach", "Grünberg", "Bad Nauheim", "Nidda", "Weilburg", "Lich", "Weilmünster"), Seq(
    ("Butzbacher Filmtheater", "Butzbacher Filmtheater", "A0154", Some("/kino/butzbach/butzbacher-filmtheater-32003")),
    ("Open-Air-Kino im Landgrafenschloss", "Open-Air-Kino im Landgrafenschloss", "A2013", Some("/kino/butzbach/openairkino-im-landgrafenschloss-51458")),
    ("Kino Neu Anspach", "Kino Neu Anspach", "A1612", Some("/kino/neuanspach/kino-32194")),
    ("Open-Air Kino Schwimmbad Neu-Ansbach", "Open-Air Kino Schwimmbad Neu-Ansbach", "A2107", Some("/kino/neuanspach/openair-kino-schwimmbad-neuansbach-37864")),
    ("Lichtspiele Grünberg", "Lichtspiele Grünberg", "A0044", Some("/kino/gruenberg/lichtspiele-gruenberg-57696")),
    ("Open Air Kino auf dem Marktplatz", "Open Air Kino auf dem Marktplatz", "A2565", None),
    ("Filmbühne Bad Nauheim", "Filmbühne Bad Nauheim", "G01HV", Some("/kino/bad-nauheim/filmbuehne-31968")),
    ("Lumos Lichtspiele &amp; Lounge", "Lumos Lichtspiele &amp; Lounge", "A2683", Some("/kino/nidda/lumos-lichtspiel-und-lounge-86137")),
    ("Delphi - Filmtheater", "Delphi - Filmtheater", "A0434", Some("/kino/weilburg/delphi-filmtheater-42237")),
    ("Traumstern", "Traumstern", "A1190", Some("/kino/lich/traumstern-32148")),
    ("Saalbau-Lichtspiele Weilmünster", "Saalbau-Lichtspiele Weilmünster", "A1611", None)
  ))
  private def r_luebeck: R = ("luebeck", "Lübeck", "Schleswig-Holstein", 53.86893, 10.68729, Seq("Lübeck", "Bad Oldesloe", "Bad Schwartau", "Mölln", "Bad Segeberg", "Neustadt in Holstein", "Ratzeburg", "Schönberg"), Seq(
    ("CineStar Lübeck - Filmhaus", "CineStar Lübeck - Filmhaus", "A0498", Some("/kino/luebeck/cinestar-filmhaus-32535")),
    ("CineStar Lübeck - Stadthalle", "CineStar Lübeck - Stadthalle", "A0367", Some("/kino/luebeck/cinestar-stadthalle-32534")),
    ("Kino Koki - Kommunales Kino Lübeck", "Kino Koki - Kommunales Kino Lübeck", "A2241", Some("/kino/luebeck/kino-koki-kommunales-kino-32532")),
    ("OHO-Kinocenter", "OHO-Kinocenter", "A1010", Some("/kino/bad-oldesloe/ohokinocenter-32336")),
    ("Movie Star Bad Schwartau", "Movie Star Bad Schwartau", "A2326", Some("/kino/bad-schwartau/movie-star-74557")),
    ("Eulenspiegelkino Mölln", "Eulenspiegelkino Mölln", "A0741", None),
    ("Cine Planet 5", "Cine Planet 5", "A1353", Some("/kino/bad-segeberg/cineplanet-5-32341")),
    ("Kino-Center Kremper Tor", "Kino-Center Kremper Tor", "A0060", None),
    ("Burgtheater Ratzeburg", "Burgtheater Ratzeburg", "A0120", Some("/kino/ratzeburg/burgtheater-32593")),
    ("Blitz-Lichtspiele Schönberg", "Blitz-Lichtspiele Schönberg", "A1318", None)
  ))
  private def r_arnsberg: R = ("arnsberg", "Arnsberg", "Nordrhein-Westfalen", 51.38333, 8.08333, Seq("Arnsberg", "Soest", "Meschede", "Plettenberg", "Lennestadt", "Brilon", "Schmallenberg"), Seq(
    ("Apollo Arnsberg-Neheim", "Apollo Arnsberg-Neheim", "A0038", None),
    ("Centra-Theater Arnsberg", "Centra-Theater Arnsberg", "A0184", Some("/kino/arnsberg/centraltheater-31738")),
    ("Residenz Kino-Center", "Residenz Kino-Center", "A1062", Some("/kino/arnsberg/residenz-kinocenter-30866")),
    ("Bürgerzentrum Alter Schlachthof", "Bürgerzentrum Alter Schlachthof", "A0117", None),
    ("Neues Universum", "Neues Universum", "A0994", Some("/kino/soest/universum-31916")),
    ("Linden-Theater", "Linden-Theater", "A1701", None),
    ("Weidenhof Kino", "Weidenhof Kino", "A1270", Some("/kino/plettenberg/weidenhof-kino-31895")),
    ("Lichtspielhaus Lennestadt", "Lichtspielhaus Lennestadt", "A0912", Some("/kino/lennestadt/lichtspielhaus-31856")),
    ("Cineplex Brilon", "Cineplex Brilon", "G00YX", Some("/kino/brilon/cineplex-brilon-92576"))
  ))
  private def r_loerrach: R = ("loerrach", "Lörrach", "Baden-Württemberg", 47.61497, 7.66457, Seq("Lörrach", "Weil am Rhein", "Rheinfelden", "Schopfheim", "Müllheim", "Neuenburg am Rhein", "Kandern"), Seq(
    ("Cineplex Lörrach", "Cineplex Lörrach", "A0321", Some("/kino/loerrach/cineplex-32153")),
    ("Kino Free Cinema", "Kino Free Cinema", "A0560", Some("/kino/loerrach/kino-free-cinema-69696")),
    ("Open Air im Hof", "Open Air im Hof", "A2865", None),
    ("Kinopalast am Rheincenter", "Kinopalast am Rheincenter", "A0821", Some("/kino/weil-am-rhein/kinopalast-im-rheincenter-60897")),
    ("Open Air Kieswerk", "Open Air Kieswerk", "A2516", Some("/kino/weil-am-rhein/kieswerk-open-air-91914")),
    ("Rheinflimmern", "Rheinflimmern", "A0032", Some("/kino/rheinfelden/rheinflimmern-75376")),
    ("Scala Schopfheim", "Scala Schopfheim", "A1107", Some("/kino/schopfheim/scala-kino-32251")),
    ("Centra-Theater Müllheim", "Centra-Theater Müllheim", "A0207", Some("/kino/muellheim/centraltheater-32188")),
    ("Kino Im Stadthaus", "Kino Im Stadthaus", "A1486", None),
    ("Kino Kandern", "Kino Kandern", "A0014", None)
  ))
  private def r_memmingen: R = ("memmingen", "Memmingen", "Bayern", 47.98372, 10.18527, Seq("Memmingen", "Kaufbeuren", "Leutkirch im Allgäu", "Kempten im Allgäu", "Krumbach"), Seq(
    ("Cineplex Memmingen", "Cineplex Memmingen", "A2113", Some("/kino/memmingen/cineplex-memmingen-72457")),
    ("Kaminwerk", "Kaminwerk", "A2848", Some("/kino/memmingen/kaminwerk-93104")),
    ("Rex Palast Memmingen", "Rex Palast Memmingen", "G0GK4", None),
    ("CICO Kaufbeuren", "CICO Kaufbeuren", "A0422", None),
    ("Corona KinoPlex - Open Air", "Corona KinoPlex - Open Air", "A2893", Some("/kino/kaufbeuren/corona-kinoplex-open-air-38671")),
    ("Melodrom-Filmtheater", "Melodrom-Filmtheater", "A1558", Some("/kino/kaufbeuren/melodromfilmtheater-32804")),
    ("Centraltheater", "Centraltheater", "A0205", None),
    ("Open-Air-Kino im Museumshof", "Open-Air-Kino im Museumshof", "A2548", None),
    ("Colosseum-Center", "Colosseum-Center", "A1382", None),
    ("CinePark Krumbach (Schwaben)", "CinePark Krumbach (Schwaben)", "A0299", Some("/kino/krumbach/cinepark-34801"))
  ))
  private def r_noerdlingen: R = ("noerdlingen", "Nördlingen", "Bayern", 48.85122, 10.48868, Seq("Nördlingen", "Aalen", "Dillingen an der Donau", "Neresheim", "Wemding", "Dischingen"), Seq(
    ("Movieworld Kino Nördlingen", "Movieworld Kino Nördlingen", "A2630", Some("/kino/noerdlingen/movieworld-77678")),
    ("Open Air Kino Ochsenzwinger Nördlingen", "Open Air Kino Ochsenzwinger Nördlingen", "A2484", None),
    ("Ries Theater", "Ries Theater", "A1078", Some("/kino/noerdlingen/ries-theater-32904")),
    ("Kino am Kocher", "Kino am Kocher", "A2023", Some("/kino/aalen/kino-am-kocher-71741")),
    ("Kinopark Aalen", "Kinopark Aalen", "A0823", Some("/kino/aalen/kinopark-39656")),
    ("Autokino Dillingen", "Autokino Dillingen", "A2926", None),
    ("Filmcenter Dillingen", "Filmcenter Dillingen", "A0492", Some("/kino/dillingen-an-der-donau/filmcenter-dillingen-32722")),
    ("Kinomobil Stuttgart - Härtsfeldhalle", "Kinomobil Stuttgart - Härtsfeldhalle", "A0817", None),
    ("Wemdinger Lichtspiele", "Wemdinger Lichtspiele", "A1275", Some("/kino/wemding/wemdinger-lichtspiele-33063")),
    ("Open-Air Kino Ballmertshofer Filmfest", "Open-Air Kino Ballmertshofer Filmfest", "A2285", Some("/kino/dischingen/openair-kino-ballmertshofer-filmfest-73657"))
  ))
  private def r_bautzen: R = ("bautzen", "Bautzen", "Sachsen", 51.18035, 14.43494, Seq("Bautzen", "Hoyerswerda", "Neustadt in Sachsen"), Seq(
    ("Filmpalast Bautzen", "Filmpalast Bautzen", "A0507", Some("/kino/bautzen/filmpalast-31354")),
    ("Freiluftkino am Spreebogen Bautzen", "Freiluftkino am Spreebogen Bautzen", "A2697", Some("/kino/bautzen/freiluftkino-am-spreebogen-86597")),
    ("Open Air Kino im Hof der Ortenburg", "Open Air Kino im Hof der Ortenburg", "A2237", None),
    ("Open-Air Kino im Freihof Gedenkstätte", "Open-Air Kino im Freihof Gedenkstätte", "A2827", Some("/kino/bautzen/openair-kino-im-freihof-gedenkstaette-79317")),
    ("Steinhaus", "Steinhaus", "A2231", Some("/kino/bautzen/steinhaus-73647")),
    ("CineMotion Hoyerswerda", "CineMotion Hoyerswerda", "A0363", Some("/kino/hoyerswerda/cinemotion-hoyerswerda-31566")),
    ("Kulturfabrik Hoyerswerda e. V.", "Kulturfabrik Hoyerswerda e. V.", "A2624", Some("/kino/hoyerswerda/blow-up-kino-in-der-kulturfabrik-hoyerswerda-81959")),
    ("Open Air an der schwarzen Mühle", "Open Air an der schwarzen Mühle", "A2526", None),
    ("Grenzland Lichtspiele", "Grenzland Lichtspiele", "A0592", None)
  ))
  private def r_rostock: R = ("rostock", "Rostock", "Mecklenburg-Vorpommern", 54.0887, 12.14049, Seq("Rostock", "Güstrow", "Kühlungsborn", "Bad Doberan"), Seq(
    ("CineStar Rostock - Capitol", "CineStar Rostock - Capitol", "A0370", Some("/kino/rostock/cinestar-capitol-31653")),
    ("CineStar Rostock - Lütten Klein", "CineStar Rostock - Lütten Klein", "A0358", None),
    ("Lichtspieltheater Wundervoll (Frieda 23)", "Lichtspieltheater Wundervoll (Frieda 23)", "A2649", None),
    ("Lichtspieltheater Wundervoll (Metropol)", "Lichtspieltheater Wundervoll (Metropol)", "A0914", None),
    ("Movie Star Gustrow", "Movie Star Gustrow", "A0972", None),
    ("Sommerkino Güstrow", "Sommerkino Güstrow", "A2933", None),
    ("Open Air Strandkorbkino am Bootshafen", "Open Air Strandkorbkino am Bootshafen", "A2059", None),
    ("Ostseekino Kühlungsborn", "Ostseekino Kühlungsborn", "A1484", Some("/kino/kuehlungsborn-ostseebad/ostseekino-60856")),
    ("Kino- und Kulturverein", "Kino- und Kulturverein", "A0643", Some("/kino/bad-doberan/kino-und-kulturverein-31342"))
  ))
  private def r_trier: R = ("trier", "Trier", "Rheinland-Pfalz", 49.75565, 6.63935, Seq("Trier", "Merzig", "Wittlich", "Wadern", "Nonnweiler"), Seq(
    ("Broadway Trier", "Broadway Trier", "A0109", Some("/kino/trier/broadway-31923")),
    ("CineAStA", "CineAStA", "A2728", Some("/kino/trier/cineasta-der-uni-trier-87857")),
    ("CinemaxX Trier", "CinemaxX Trier", "A0288", Some("/kino/trier/cinemaxx-39458")),
    ("Open-Air-Kino im TuFa-Hof", "Open-Air-Kino im TuFa-Hof", "A2829", None),
    ("Kino am Seffersbach", "Kino am Seffersbach", "A2800", None),
    ("Odeon Kino Merzig", "Odeon Kino Merzig", "A0679", Some("/kino/merzig-saar/odeon-kinocenter-65016")),
    ("Kinopalast Eifel Mosel Hunsrück", "Kinopalast Eifel Mosel Hunsrück", "G011D", Some("/kino/wittlich/kinopalast-eifelmoselhunsrueck-92635")),
    ("Starlight-Kino", "Starlight-Kino", "A1150", None),
    ("Central-Filmtheater Nonnweiler", "Central-Filmtheater Nonnweiler", "A0190", Some("/kino/nonnweiler/centralfilmtheater-32204"))
  ))
  private def r_aschaffenburg: R = ("aschaffenburg", "Aschaffenburg", "Bayern", 49.97704, 9.15214, Seq("Aschaffenburg", "Büdingen", "Gelnhausen", "Seligenstadt", "Babenhausen", "Erlenbach am Main"), Seq(
    ("Casino Aschaffenburg", "Casino Aschaffenburg", "A1342", Some("/kino/aschaffenburg/casino-31952")),
    ("Kinopolis Aschaffenburg", "Kinopolis Aschaffenburg", "A0827", Some("/kino/aschaffenburg/kinopolis-aschaffenburg-35109")),
    ("OPEN AIR auf dem Campus FH", "OPEN AIR auf dem Campus FH", "A1959", Some("/kino/aschaffenburg/open-air-auf-dem-campus-fh-71531")),
    ("Open-Air-Kino im Nilkheimer Park", "Open-Air-Kino im Nilkheimer Park", "A2813", Some("/kino/aschaffenburg/openairkino-im-nilkheimer-park-40457")),
    ("Novum Kino", "Novum Kino", "A0112", Some("/kino/buedingen/novum-kino-buedingen-31999")),
    ("Kino Gelnhausen (Pali und Casino)", "Kino Gelnhausen (Pali und Casino)", "A1028", Some("/kino/gelnhausen/casino-32058")),
    ("Turmpalast", "Turmpalast", "A1650", Some("/kino/seligenstadt/turmpalast-32261")),
    ("Passage Erlenbach am Main", "Passage Erlenbach am Main", "A1032", Some("/kino/erlenbach-am-main/kino-passage-32019"))
  ))
  private def r_goerlitz: R = ("goerlitz", "Görlitz", "Sachsen", 51.15518, 14.98853, Seq("Görlitz", "Zittau", "Mittelherwigsdorf", "Rietschen"), Seq(
    ("Camillo-Sommerkino im Rathaushof", "Camillo-Sommerkino im Rathaushof", "A2090", Some("/kino/goerlitz-neisse/camillosommerkino-im-rathaushof-56457")),
    ("CamilloKino", "CamilloKino", "A0138", Some("/kino/goerlitz-neisse/camillokino-53777")),
    ("Filmpalast Görlitz", "Filmpalast Görlitz", "A0510", Some("/kino/goerlitz-neisse/filmpalast-31536")),
    ("Offkino Klappe die Zweite", "Offkino Klappe die Zweite", "A2142", None),
    ("Filmpalast Zittau", "Filmpalast Zittau", "A0509", Some("/kino/zittau/filmpalast-31720")),
    ("Kronenkino", "Kronenkino", "A2685", Some("/kino/zittau/kronenkino-86157")),
    ("Zittauer Filmnächte", "Zittauer Filmnächte", "A2491", None),
    ("Kulturfabrik Meda", "Kulturfabrik Meda", "A2710", Some("/kino/mittelherwigsdorf/kulturfabrik-meda-87268")),
    ("Kino-Cafe", "Kino-Cafe", "A0759", Some("/kino/rietschen/kinocaf%C3%A9-rietschen-e.v.-31649"))
  ))
  private def r_goettingen: R = ("goettingen", "Göttingen", "Niedersachsen", 51.53443, 9.93228, Seq("Göttingen", "Einbeck", "Northeim", "Duderstadt", "Witzenhausen", "Bad Sooden-Allendorf"), Seq(
    ("CinemaxX Göttingen", "CinemaxX Göttingen", "A1293", Some("/kino/goettingen/cinemaxx-32426")),
    ("Lumière", "Lumière", "A1548", Some("/kino/goettingen/lumi%C3%A8re-32427")),
    ("Open Air Kino im Freibad", "Open Air Kino im Freibad", "A1788", Some("/kino/goettingen/open-air-kino-im-freibad-37803")),
    ("Deli Kino", "Deli Kino", "A1388", Some("/kino/einbeck/delikino-32405")),
    ("Welt -Theater", "Welt -Theater", "A1271", Some("/kino/einbeck/welttheater-32406")),
    ("Neue Schauburg Northeim", "Neue Schauburg Northeim", "A0984", Some("/kino/northeim/neue-schauburg-32568")),
    ("Movietown Eichsfeld", "Movietown Eichsfeld", "A1624", Some("/kino/duderstadt/movietown-eichsfeld-32403")),
    ("Capitol-Kino", "Capitol-Kino", "A1335", Some("/kino/witzenhausen/capitol-32308")),
    ("Kurtheater Bad Sooden-Allendorf", "Kurtheater Bad Sooden-Allendorf", "A0890", Some("/kino/bad-soodenallendorf/kurtheater-31978"))
  ))
  private def r_rheine: R = ("rheine", "Rheine", "Nordrhein-Westfalen", 52.28509, 7.44055, Seq("Rheine", "Lingen", "Nordhorn", "Ibbenbüren", "Gronau", "Emsdetten", "Steinfurt"), Seq(
    ("Cinetech das Erlebniskino Rheine", "Cinetech das Erlebniskino Rheine", "A0383", Some("/kino/rheine/cinetech-31901")),
    ("Zinema City", "Zinema City", "A0398", Some("/kino/rheine/zinema-city-rheine-49156")),
    ("Central-Kino Emsland (Lingen)", "Central-Kino Emsland (Lingen)", "A0200", Some("/kino/lingen/centralkino-32529")),
    ("Filmpalast Cine-World", "Filmpalast Cine-World", "G02Q9", Some("/kino/lingen/filmpalast-cineworld-37227")),
    ("UCI LUXE Nordhorn", "UCI LUXE Nordhorn", "A2895", None),
    ("Apollo Kino Center", "Apollo Kino Center", "A0045", Some("/kino/ibbenbueren/apollo-kino-31827")),
    ("Gronauer-Lichtspiele", "Gronauer-Lichtspiele", "A1446", None),
    ("Metropolis Kino Emsdetten", "Metropolis Kino Emsdetten", "A2809", None),
    ("Kino Steinfurt", "Kino Steinfurt", "A0551", None)
  ))
  private def r_plauen: R = ("plauen", "Plauen", "Sachsen", 50.4973, 12.13782, Seq("Plauen", "Hof", "Greiz", "Auerbach", "Oelsnitz", "Schleiz", "Markneukirchen"), Seq(
    ("Capitol-Kino Plauen", "Capitol-Kino Plauen", "A0156", Some("/kino/plauen-vogtland/capitolkino-31631")),
    ("Malzhaus", "Malzhaus", "A2375", Some("/kino/plauen-vogtland/malzhaus-75077")),
    ("Central-Kino Hof", "Central-Kino Hof", "A1346", Some("/kino/hof/centralkino-32794")),
    ("Scala Filmtheater Hof", "Scala Filmtheater Hof", "A1051", Some("/kino/hof/scala-filmtheater-32793")),
    ("UT99 Kinocenter", "UT99 Kinocenter", "A1250", None),
    ("Rekord-Lichtspiele", "Rekord-Lichtspiele", "A1058", Some("/kino/auerbach/rekord-lichtspiele-37051")),
    ("Open-Air-Kino - Waldbühne Neuwürschnitz", "Open-Air-Kino - Waldbühne Neuwürschnitz", "A1846", None),
    ("Neues Kino im Hörsaal", "Neues Kino im Hörsaal", "A0989", Some("/kino/schleiz/neues-kino-im-hoersaal-61457")),
    ("Harmonie Lichtspiele Markneukirchen", "Harmonie Lichtspiele Markneukirchen", "A0600", Some("/kino/markneukirchen/harmonie-lichtspiele-31605"))
  ))
  private def r_crailsheim: R = ("crailsheim", "Crailsheim", "Baden-Württemberg", 49.13444, 10.07193, Seq("Crailsheim", "Ellwangen", "Künzelsau", "Feuchtwangen", "Rothenburg", "Schrozberg", "Ilshofen", "Kirchberg an der Jagst"), Seq(
    ("Cinecity", "Cinecity", "A0224", Some("/kino/crailsheim/cinecity-und-kammer-filmtheater-42476")),
    ("Kammer-Filmtheater Premium-Kino Crailsheim", "Kammer-Filmtheater Premium-Kino Crailsheim", "A0641", Some("/kino/crailsheim/kammerfilmtheater-premiumkino-88777")),
    ("Kino Regina Ellwangen", "Kino Regina Ellwangen", "A1590", Some("/kino/ellwangen/kino-regina-32734")),
    ("Prestige", "Prestige", "A1040", Some("/kino/kuenzelsau/prestige-53416")),
    ("KulturKino Feuchtwangen", "KulturKino Feuchtwangen", "A1054", None),
    ("Filmpalast im Forum", "Filmpalast im Forum", "A2646", Some("/kino/rothenburg-ob-der-tauber/forum-79938")),
    ("Open Air Schrozberg", "Open Air Schrozberg", "A2701", None),
    ("Kinomobil Stuttgart - Roland-Wurmthaler-Halle", "Kinomobil Stuttgart - Roland-Wurmthaler-Halle", "A0789", None),
    ("Kino Klappe", "Kino Klappe", "A0744", None)
  ))
  private def r_bad_aibling: R = ("bad-aibling", "Bad Aibling", "Bayern", 47.8638, 12.01055, Seq("Bad Aibling", "Wasserburg am Inn", "Rosenheim", "Grafing bei München", "Ebersberg", "Hausham", "Bad Endorf"), Seq(
    ("Aibvision Filmtheater und Lindenkino", "Aibvision Filmtheater und Lindenkino", "A1543", Some("/kino/bad-aibling/aibvision-filmtheater-32681")),
    ("Open Air am B&amp;O Parkhotel", "Open Air am B&amp;O Parkhotel", "A2862", Some("/kino/bad-aibling/open-air-am-bundo-parkhotel-82462")),
    ("Kino Utopia", "Kino Utopia", "A1658", Some("/kino/wasserburg-am-inn/kino-utopia-33054")),
    ("Open Air im STrandpark Wasserburg", "Open Air im STrandpark Wasserburg", "A2561", None),
    ("Kinopolis Rosenheim", "Kinopolis Rosenheim", "A0401", None),
    ("Capitol Theater Grafing bei München", "Capitol Theater Grafing bei München", "A0159", None),
    ("Kino im Alten Kino", "Kino im Alten Kino", "A2850", None),
    ("Oberland Kinocenter", "Oberland Kinocenter", "A1001", Some("/kino/hausham/oberland-kinocenter-32785")),
    ("Marias Kino", "Marias Kino", "A1555", Some("/kino/bad-endorf/marias-kino-32682"))
  ))
  private def r_osnabrueck: R = ("osnabrueck", "Osnabrück", "Niedersachsen", 52.27264, 8.0498, Seq("Osnabrück", "Bramsche", "Ankum"), Seq(
    ("Cinema-Arthouse", "Cinema-Arthouse", "A0257", Some("/kino/osnabrueck/cinemaarthouse-46624")),
    ("Filmtheater Hasetor", "Filmtheater Hasetor", "A1431", Some("/kino/osnabrueck/filmtheater-hasetor-32581")),
    ("HALL OF FAME - Kino de Luxe", "HALL OF FAME - Kino de Luxe", "A0348", Some("/kino/osnabrueck/hall-of-fame-92002")),
    ("Kino in der Lagerhalle", "Kino in der Lagerhalle", "A0739", Some("/kino/osnabrueck/kino-in-der-lagerhalle-32578")),
    ("Open Air Kino im Innenhof der Domschule Giro Live", "Open Air Kino im Innenhof der Domschule Giro Live", "A1961", Some("/kino/osnabrueck/open-air-kino-im-innenhof-der-domschule-giro-live-67997")),
    ("Open-Air-Kino Schloss-Innenhof", "Open-Air-Kino Schloss-Innenhof", "A2504", None),
    ("Universum e.V.", "Universum e.V.", "A1655", Some("/kino/bramsche/filmtheater-universum-33140")),
    ("Gloria Kinocenter", "Gloria Kinocenter", "A0576", Some("/kino/ankum/gloria-kinocenter-32326"))
  ))
  private def r_muenster: R = ("muenster", "Münster", "Nordrhein-Westfalen", 51.96236, 7.62571, Seq("Münster", "Munster", "Ahlen", "Dülmen", "Coesfeld"), Seq(
    ("Cinema &amp; Kurbelkiste Münster", "Cinema &amp; Kurbelkiste Münster", "A0243", Some("/kino/muenster/cinema-und-kurbelkiste-31879")),
    ("Kaisersaal-Lichtspiele", "Kaisersaal-Lichtspiele", "A0633", None),
    ("Schlosstheater Münster", "Schlosstheater Münster", "A1130", Some("/kino/muenster/schlosstheater-31880")),
    ("Cineplex Münster", "Cineplex Münster", "A0305", Some("/kino/muenster/cineplex-49196")),
    ("CinemAhlen", "CinemAhlen", "A2879", None),
    ("Cinema Coesfeld (Dülmen)", "Cinema Coesfeld (Dülmen)", "A0249", None),
    ("Cinema Coesfeld (Coesfeld)", "Cinema Coesfeld (Coesfeld)", "A0229", Some("/kino/coesfeld/cinema-64898"))
  ))
  private def r_erfurt: R = ("erfurt", "Erfurt", "Thüringen", 50.97734, 11.03536, Seq("Erfurt", "Gotha", "Ilmenau"), Seq(
    ("CineStar Erfurt", "CineStar Erfurt", "A0344", Some("/kino/erfurt/cinestar-der-filmpalast-43336")),
    ("Kinoklub am Hirschlachufer", "Kinoklub am Hirschlachufer", "A0778", Some("/kino/erfurt/kinoklub-am-hirschlachufer-31514")),
    ("Open Air Kino egapark", "Open Air Kino egapark", "A2261", None),
    ("Open Air im Kulturhof Krönbacken", "Open Air im Kulturhof Krönbacken", "A1829", None),
    ("Cineplex Gotha", "Cineplex Gotha", "A2675", Some("/kino/gotha-thueringen/cineplex-85777")),
    ("Hochschulfilmclub TU Ilmenau", "Hochschulfilmclub TU Ilmenau", "A0610", Some("/kino/ilmenau-thueringen/hochschulfilmclub-43299")),
    ("Linden Lichtspiele", "Linden Lichtspiele", "A0917", Some("/kino/ilmenau-thueringen/linden-lichtspiele-31567"))
  ))
  private def r_ulm: R = ("ulm", "Ulm", "Baden-Württemberg", 48.39841, 9.99155, Seq("Ulm", "Neu-Ulm", "Ehingen", "Günzburg", "Sontheim", "Offingen"), Seq(
    ("Mephisto Ulm", "Mephisto Ulm", "A1559", Some("/kino/ulm/mephisto-33040")),
    ("Obscura Ulm", "Obscura Ulm", "A0692", Some("/kino/ulm/obscura-33039")),
    ("Xinedome", "Xinedome", "A1285", Some("/kino/ulm/xinedome-57656")),
    ("Dietrich Theater Neu-Ulm", "Dietrich Theater Neu-Ulm", "A1741", Some("/kino/neuulm/dietrichtheater-70577")),
    ("Central Kino Center", "Central Kino Center", "A0187", Some("/kino/ehingen/central-kino-center-64336")),
    ("Kino Biigz", "Kino Biigz", "A2653", Some("/kino/guenzburg/biigz-78877")),
    ("Kino in der Dampfsäg", "Kino in der Dampfsäg", "A2588", None),
    ("Donaulichtspiele", "Donaulichtspiele", "A1395", Some("/kino/offingen/donaulichtspiele-32925"))
  ))
  private def r_koblenz: R = ("koblenz", "Koblenz", "Rheinland-Pfalz", 50.35357, 7.57883, Seq("Koblenz", "Neuwied", "Limburg", "Lahnstein", "Boppard", "Montabaur"), Seq(
    ("Kinopolis Koblenz", "Kinopolis Koblenz", "A0830", Some("/kino/koblenz-am-rhein/kinopolis-57476")),
    ("Odeon-Kinocenter", "Odeon-Kinocenter", "A0062", Some("/kino/koblenz-am-rhein/odeon-kinocenter-31835")),
    ("Metropol-Kino-Center", "Metropol-Kino-Center", "A1564", Some("/kino/neuwied/metropolkinocenter-33987")),
    ("Schauburg-Theater", "Schauburg-Theater", "A1124", Some("/kino/neuwied/schauburgtheater-31887")),
    ("Cineplex Limburg", "Cineplex Limburg", "A0319", Some("/kino/limburg-an-der-lahn/cineplex-32149")),
    ("Kino Lahnstein", "Kino Lahnstein", "A1196", Some("/kino/lahnstein/kino-lahnstein-32136")),
    ("Cinema Boppard", "Cinema Boppard", "A1360", None),
    ("Capitol-Kinocenter Montabaur", "Capitol-Kinocenter Montabaur", "A0163", Some("/kino/montabaur/capitolkinocenter-32184"))
  ))
  private def r_bad_toelz: R = ("bad-toelz", "Bad Tölz", "Bayern", 47.76111, 11.5589, Seq("Bad Tölz", "Wolfratshausen", "Holzkirchen", "Penzberg", "Tutzing", "Rottach-Egern", "Kochel am See"), Seq(
    ("Capitol Filmtheater Bad Tölz", "Capitol Filmtheater Bad Tölz", "A0142", None),
    ("Isar-Kinocenter", "Isar-Kinocenter", "A0628", Some("/kino/bad-toelz/isarkinocenter-37943")),
    ("Kinocenter Wolfratshausen", "Kinocenter Wolfratshausen", "A1171", Some("/kino/wolfratshausen/kinocenter-33068")),
    ("KULTUR im Oberbräu", "KULTUR im Oberbräu", "A2357", None),
    ("Kino P.", "Kino P.", "A0819", Some("/kino/penzberg/kino-p.-61777")),
    ("Kurtheater Tutzing", "Kurtheater Tutzing", "A0885", Some("/kino/tutzing/kulturtheater-33037")),
    ("Kino am Tegernsee", "Kino am Tegernsee", "A0093", None),
    ("Filmstudio Kochel", "Filmstudio Kochel", "A0532", None)
  ))
  private def r_minden: R = ("minden", "Minden", "Nordrhein-Westfalen", 52.28953, 8.91455, Seq("Minden", "Bad Oeynhausen", "Lohne", "Löhne", "Espelkamp", "Stadthagen", "Bückeburg", "Rahden"), Seq(
    ("Filmtheater &quot;Die Birke&quot;", "Filmtheater &quot;Die Birke&quot;", "A2798", Some("/kino/minden-westfalen/die-birke-31866")),
    ("UCI Bad Oeynhausen", "UCI Bad Oeynhausen", "A2666", Some("/kino/bad-oeynhausen/uci-kinowelt-bad-oeynhausen-74656")),
    ("Capitol-Kinocenter Lohne (Oldenburg)", "Capitol-Kinocenter Lohne (Oldenburg)", "A0162", None),
    ("Sommernachtskino (Löhne)", "Sommernachtskino (Löhne)", "A2824", None),
    ("Elite", "Elite", "A0462", Some("/kino/espelkamp/elite-31797")),
    ("Kinocenter Stadthagen", "Kinocenter Stadthagen", "A1632", Some("/kino/stadthagen/kinocenter-32618")),
    ("Residenz-Kino-Center", "Residenz-Kino-Center", "A1061", Some("/kino/bueckeburg/residenzkinocenter-32379")),
    ("Kinocenter Rahden", "Kinocenter Rahden", "A1730", None)
  ))
  private def r_vechta: R = ("vechta", "Vechta", "Niedersachsen", 52.73064, 8.28968, Seq("Vechta", "Wildeshausen", "Diepholz", "Damme", "Quakenbrück", "Twistringen", "Lemförde", "Quernheim"), Seq(
    ("Schauburg Cineworld Vechta", "Schauburg Cineworld Vechta", "A1118", Some("/kino/vechta/schauburg-cineworld-33163")),
    ("Lindenhof-Lichtspiele", "Lindenhof-Lichtspiele", "A0919", None),
    ("Central Diepholz", "Central Diepholz", "A1344", None),
    ("Dersa Kino-Center", "Dersa Kino-Center", "A0438", Some("/kino/damme-duemmer/dersa-kinocenter-32392")),
    ("Schauburg Filmtheater", "Schauburg Filmtheater", "A1120", Some("/kino/quakenbrueck/schauburg-filmtheater-33162")),
    ("Filmtheater Twistringen", "Filmtheater Twistringen", "A1358", Some("/kino/twistringen/filmtheater-32622")),
    ("Lichtburg Open Air Kino", "Lichtburg Open Air Kino", "A1865", None),
    ("Lichtburg Lemförde", "Lichtburg Lemförde", "A0689", Some("/kino/quernheim-bei-diepholz/lichtburg-34805"))
  ))
  private def r_schwerin: R = ("schwerin", "Schwerin", "Mecklenburg-Vorpommern", 53.62937, 11.41316, Seq("Schwerin", "Ludwigslust", "Wismar"), Seq(
    ("Capitol Kino Schwerin", "Capitol Kino Schwerin", "A0149", Some("/kino/schwerin-bei-koenigs-wusterhausen/filmpalast-capitol-31667")),
    ("Kino unterm Dach", "Kino unterm Dach", "A0559", Some("/kino/schwerin-bei-koenigs-wusterhausen/kino-unterm-dach-e.v.-93102")),
    ("Multiplex Mega Movies", "Multiplex Mega Movies", "A0977", Some("/kino/schwerin-bei-koenigs-wusterhausen/mega-movies-31668")),
    ("Open-Air-Kino Schwerin", "Open-Air-Kino Schwerin", "A2190", None),
    ("Luna Filmtheater", "Luna Filmtheater", "A1732", Some("/kino/ludwigslust-mecklenburg/luna-filmtheater-70497")),
    ("Open Air Kino am Schweizerhaus", "Open Air Kino am Schweizerhaus", "A1992", None),
    ("CineStar Wismar", "CineStar Wismar", "A0361", Some("/kino/wismar-mecklenburg/cinestar-31702"))
  ))
  private def r_cottbus: R = ("cottbus", "Cottbus", "Brandenburg", 51.75769, 14.32888, Seq("Cottbus", "Spremberg", "Senftenberg"), Seq(
    ("Kino in der Stadhalle Cottbus", "Kino in der Stadhalle Cottbus", "A2370", Some("/kino/cottbus/kino-in-der-stadthalle-75063")),
    ("Obenkino im Glad-House", "Obenkino im Glad-House", "A1000", Some("/kino/cottbus/obenkino-im-gladhouse-43956")),
    ("UCI am Lausitz Park", "UCI am Lausitz Park", "A1204", Some("/kino/cottbus/uci-kinowelt-am-lausitz-park-43176")),
    ("Weltspiegel Cottbus", "Weltspiegel Cottbus", "A1663", Some("/kino/cottbus/weltspiegel-31475")),
    ("Spreekino", "Spreekino", "A1143", Some("/kino/spremberg-niederlausitz/spreekino-39157")),
    ("Spremberger Filmnächte", "Spremberger Filmnächte", "A2758", None),
    ("Kino am See", "Kino am See", "A1887", None)
  ))
  private def r_bamberg: R = ("bamberg", "Bamberg", "Bayern", 49.89873, 10.90067, Seq("Bamberg", "Forchheim", "Zeil am Main"), Seq(
    ("Lichtspiel, Kino &amp; Café", "Lichtspiel, Kino &amp; Café", "A1535", Some("/kino/bamberg/lichtspiel-kino-und-caf%C3%A9-32694")),
    ("Odeon Lichtspiel, Kino &amp; Café", "Odeon Lichtspiel, Kino &amp; Café", "A1005", Some("/kino/bamberg/odeon-lichtspiel-kino-und-caf%C3%A9-48482")),
    ("Open Air Kino Hainbad", "Open Air Kino Hainbad", "A1983", None),
    ("Sommerkino im Schloss Geyerswörth", "Sommerkino im Schloss Geyerswörth", "A2821", Some("/kino/bamberg/sommerkino-im-schloss-geyerswoerth-81417")),
    ("Kino-Center Forchheim", "Kino-Center Forchheim", "A0034", Some("/kino/forchheim/kinocenter-54076")),
    ("Open Air Kino In der Kaiserpfalz Forchheim", "Open Air Kino In der Kaiserpfalz Forchheim", "A1968", None),
    ("Capitol-Theater (Foto Kino Schneyer)", "Capitol-Theater (Foto Kino Schneyer)", "A0166", Some("/kino/zeil-am-main/capitoltheater-foto-kino-schneyer-32320"))
  ))
  private def r_landshut: R = ("landshut", "Landshut", "Bayern", 48.52961, 12.16179, Seq("Landshut", "Moosburg", "Dorfen", "Vilsbiburg", "Gangkofen"), Seq(
    ("Filmzentrum e.V. Kinoptikum", "Filmzentrum e.V. Kinoptikum", "A0834", Some("/kino/landshut-isar/filmzentrum-e.v.-kinoptikum-34563")),
    ("Kinopolis Landshut", "Kinopolis Landshut", "A0026", Some("/kino/landshut-isar/kinopolis-57317")),
    ("Kleines Theater Landshut", "Kleines Theater Landshut", "A2852", None),
    ("Rosenhof-Lichtspiele", "Rosenhof-Lichtspiele", "A1606", Some("/kino/moosburg/rosenhoflichtspiele-32852")),
    ("sKino im Jakobmayer", "sKino im Jakobmayer", "A2671", Some("/kino/dorfen/skino-im-jakobmayer-80078")),
    ("Cineplex Vilsbiburg", "Cineplex Vilsbiburg", "A0896", Some("/kino/vilsbiburg/cineplex-vilsbiburg-33046")),
    ("Phantasia", "Phantasia", "A2587", Some("/kino/gangkofen/phantasia-32765"))
  ))
  private def r_kaiserslautern: R = ("kaiserslautern", "Kaiserslautern", "Rheinland-Pfalz", 49.443, 7.77161, Seq("Kaiserslautern", "Pirmasens", "Landstuhl", "Annweiler am Trifels", "Enkenbach Alsenborn", "Kusel"), Seq(
    ("UCI Kaiserslautern", "UCI Kaiserslautern", "A0669", Some("/kino/kaiserslautern/uci-kinowelt-kaiserslautern-37896")),
    ("Union - Studio für Filmkunst", "Union - Studio für Filmkunst", "A1226", Some("/kino/kaiserslautern/union-studio-fuer-filmkunst-32105")),
    ("Walhalla Kinocenter", "Walhalla Kinocenter", "A1659", Some("/kino/pirmasens/walhalla-kinocenter-32220")),
    ("Broadway Ramstein-Miesenbach", "Broadway Ramstein-Miesenbach", "A1319", None),
    ("Kino Digital im Hohenstaufensaal", "Kino Digital im Hohenstaufensaal", "A2619", Some("/kino/annweiler-am-trifels/kino-im-hohenstaufensaal-32335")),
    ("Provinzkino", "Provinzkino", "A1588", None),
    ("Kinett", "Kinett", "A1458", Some("/kino/kusel/kinett-kino-32134"))
  ))
  private def r_lueneburg: R = ("lueneburg", "Lüneburg", "Niedersachsen", 53.25122, 10.41548, Seq("Lüneburg", "Uelzen", "Winsen", "Geesthacht", "Schwarzenbek", "Boizenburg"), Seq(
    ("Filmpalast Lüneburg", "Filmpalast Lüneburg", "A0327", Some("/kino/lueneburg/filmpalast-41917")),
    ("Scala Kinocenter Lüneburg", "Scala Kinocenter Lüneburg", "A1616", Some("/kino/lueneburg/scala-kinocenter-32539")),
    ("Central-Theater Uelzen", "Central-Theater Uelzen", "A0206", Some("/kino/uelzen/centraltheater-34348")),
    ("Kinocenter Winsen", "Kinocenter Winsen", "A1504", None),
    ("Kleines Theater Schillerstraße", "Kleines Theater Schillerstraße", "A0851", Some("/kino/geesthacht/kleines-theater-schillerstrasse-32423")),
    ("Kino Grimm", "Kino Grimm", "A0695", Some("/kino/schwarzenbek/kino-grimm-32613")),
    ("Kino Boizenburg", "Kino Boizenburg", "A0710", Some("/kino/boizenburg/kino-boizenburg-65421"))
  ))
  private def r_hameln: R = ("hameln", "Hameln", "Niedersachsen", 52.10397, 9.35623, Seq("Hameln", "Rinteln", "Bad Pyrmont", "Holzminden", "Alfeld"), Seq(
    ("Maxx Hameln", "Maxx Hameln", "A1715", Some("/kino/hameln/maxx-hameln-55117")),
    ("Sumpfblume", "Sumpfblume", "A2825", Some("/kino/hameln/sumpfblume-32466")),
    ("Kinocenter Rinteln", "Kinocenter Rinteln", "A1503", Some("/kino/rinteln/kinocenter-32598")),
    ("Metropol-Theater Rinteln", "Metropol-Theater Rinteln", "A1566", None),
    ("Kronenlichtspiele", "Kronenlichtspiele", "A1525", Some("/kino/bad-pyrmont/kronenlichtspiele-32337")),
    ("Roxy Filmcentrum", "Roxy Filmcentrum", "A0697", None),
    ("Kinowelt Alfeld", "Kinowelt Alfeld", "A1515", None)
  ))
  private def r_burghausen: R = ("burghausen", "Burghausen", "Bayern", 48.16925, 12.83139, Seq("Burghausen", "Eggenfelden", "Waldkraiburg", "Mühldorf", "Simbach am Inn"), Seq(
    ("Anker-Filmtheater", "Anker-Filmtheater", "A1490", None),
    ("Quadroscope", "Quadroscope", "A1147", Some("/kino/burghausen/quadroscope-45538")),
    ("Gerniale Kino Open-Air", "Gerniale Kino Open-Air", "A2200", Some("/kino/eggenfelden/gerniale-kino-openair-73783")),
    ("Kinocenter Eggenfelden", "Kinocenter Eggenfelden", "A1312", None),
    ("Cineplex Waldkraiburg", "Cineplex Waldkraiburg", "A0386", Some("/kino/waldkraiburg/cineplex-38977")),
    ("Hollywood am Inn", "Hollywood am Inn", "A1675", Some("/kino/muehldorf-am-inn/hollywood-70058")),
    ("Bavaria-Kino-Center Simbach", "Bavaria-Kino-Center Simbach", "A1314", Some("/kino/simbach-am-inn/bavariakinocenter-32993"))
  ))
  private def r_bad_kreuznach: R = ("bad-kreuznach", "Bad Kreuznach", "Rheinland-Pfalz", 49.8414, 7.86713, Seq("Bad Kreuznach", "Bingen am Rhein", "Ingelheim am Rhein", "Alzey", "Geisenheim", "Simmern", "Bad Sobernheim"), Seq(
    ("Cineplex Bad Kreuznach", "Cineplex Bad Kreuznach", "A0308", Some("/kino/bad-kreuznach/cineplex-47137")),
    ("KiKuBi", "KiKuBi", "A0251", None),
    ("Casablanca Ingelheim am Rhein", "Casablanca Ingelheim am Rhein", "A0024", None),
    ("Bali", "Bali", "A1309", Some("/kino/alzey/bali-31948")),
    ("Linden Theater Geisenheim", "Linden Theater Geisenheim", "A1542", Some("/kino/geisenheim/lindentheater-32057")),
    ("Pro-Winzkino", "Pro-Winzkino", "A1589", Some("/kino/simmern-hunsrueck/prowinzkino-32262")),
    ("Rex Bad Sobernheim", "Rex Bad Sobernheim", "A1071", None)
  ))
  private def r_magdeburg: R = ("magdeburg", "Magdeburg", "Sachsen-Anhalt", 52.13129, 11.63189, Seq("Magdeburg", "Burg"), Seq(
    ("CineStar Magdeburg", "CineStar Magdeburg", "A1259", Some("/kino/magdeburg/cinestar-36260")),
    ("CinemaxX Magdeburg", "CinemaxX Magdeburg", "A0293", Some("/kino/magdeburg/cinemaxx-31600")),
    ("Kulturzentrum auf dem Moritzhof", "Kulturzentrum auf dem Moritzhof", "A1709", Some("/kino/magdeburg/kulturzentrum-moritzhof-71957")),
    ("Oli Lichtspiele", "Oli Lichtspiele", "A1708", None),
    ("Studio-Kino", "Studio-Kino", "A1165", Some("/kino/magdeburg/studiokino-43159")),
    ("Burg-Theater Burg", "Burg-Theater Burg", "A0124", None)
  ))
  private def r_marburg: R = ("marburg", "Marburg", "Hessen", 50.80904, 8.77069, Seq("Marburg", "Dillenburg", "Schwalmstadt"), Seq(
    ("Capitol Marburg", "Capitol Marburg", "A1332", Some("/kino/marburg/capitol-32175")),
    ("Cineplex Marburg", "Cineplex Marburg", "A0320", Some("/kino/marburg/cineplex-42577")),
    ("Kino Cafe Trauma", "Kino Cafe Trauma", "A0714", Some("/kino/marburg/kino-cafe-trauma-69717")),
    ("Open Air Kino Marburg", "Open Air Kino Marburg", "A1855", None),
    ("Movie Star Dillenburg", "Movie Star Dillenburg", "A0579", Some("/kino/dillenburg/movie-star-77878")),
    ("Burgtheater Treysa", "Burgtheater Treysa", "A0123", None)
  ))
  private def r_oldenburg: R = ("oldenburg", "Oldenburg", "Niedersachsen", 53.14039, 8.21479, Seq("Oldenburg", "Oldenburg (Oldenburg)", "Cloppenburg", "Zetel"), Seq(
    ("Casablanca-Programmkino", "Casablanca-Programmkino", "A0097", Some("/kino/oldenburg-in-oldenburg/casablancaprogrammkino-58136")),
    ("CinemaxX Oldenburg", "CinemaxX Oldenburg", "A0281", Some("/kino/oldenburg-in-oldenburg/cinemaxx-37682")),
    ("Studentisches Kino Gegenlicht", "Studentisches Kino Gegenlicht", "A0569", Some("/kino/oldenburg-in-oldenburg/gegenlicht-35074")),
    ("Cine K Oldenburg", "Cine K Oldenburg", "A0220", Some("/kino/oldenburg-in-oldenburg/cine-k-69957")),
    ("CineCenter Cloppenburg", "CineCenter Cloppenburg", "A1354", Some("/kino/cloppenburg/cinecenter-33130")),
    ("Zeli - Zeteler Lichtspiele e.V.", "Zeli - Zeteler Lichtspiele e.V.", "A1660", Some("/kino/zetel/zeli-zeteler-lichtspiele-e.v.-32651"))
  ))
  private def r_fulda: R = ("fulda", "Fulda", "Hessen", 50.55162, 9.67518, Seq("Fulda", "Schlüchtern", "Lauterbach", "Bad Brückenau"), Seq(
    ("CineStar Fulda", "CineStar Fulda", "A0366", Some("/kino/fulda/cinestar-fulda-38822")),
    ("Kinoinitiative 35", "Kinoinitiative 35", "A2661", Some("/kino/fulda/kinoinitiative-35-79917")),
    ("Museumscafé", "Museumscafé", "A2860", None),
    ("KUKI Kino", "KUKI Kino", "A0875", None),
    ("Lichtspielhaus Lauterbach (Hessen)", "Lichtspielhaus Lauterbach (Hessen)", "A0911", Some("/kino/lauterbach/lichtspielhaus-32145")),
    ("Rhön-Lichtspiele", "Rhön-Lichtspiele", "A2314", Some("/kino/bad-brueckenau/rhoenlichtspiele-31955"))
  ))
  private def r_passau: R = ("passau", "Passau", "Bayern", 48.5665, 13.43122, Seq("Passau", "Bad Füssing", "Freyung"), Seq(
    ("Cineplex Passau", "Cineplex Passau", "A0322", Some("/kino/passau/cineplex-69823")),
    ("ProLi Cinema", "ProLi Cinema", "G011E", None),
    ("ScharfrichterKino", "ScharfrichterKino", "A1622", Some("/kino/passau/scharfrichterkino-32933")),
    ("Filmgalerie Bad Füssing", "Filmgalerie Bad Füssing", "A1417", Some("/kino/bad-fuessing/filmgalerie-33684")),
    ("Kino im großen Kurhaus", "Kino im großen Kurhaus", "A1482", Some("/kino/bad-fuessing/kino-im-grossen-kurhaus-32684")),
    ("Cineplex Freyung", "Cineplex Freyung", "A2642", Some("/kino/freyung/cineplex-82999"))
  ))
  private def r_ansbach: R = ("ansbach", "Ansbach", "Bayern", 49.30481, 10.5931, Seq("Ansbach", "Neustadt an der Aisch", "Bad Windsheim", "Grosshabersdorf"), Seq(
    ("Capitol Kinocenter", "Capitol Kinocenter", "A1338", Some("/kino/ansbach/capitol-kinocenter-32665")),
    ("Kammerspiele Ansbach", "Kammerspiele Ansbach", "A1733", Some("/kino/ansbach/kammerspiele-32664")),
    ("Kultur am Schloss", "Kultur am Schloss", "A1628", Some("/kino/ansbach/kultur-am-schloss-32666")),
    ("Kino NEA", "Kino NEA", "A2657", Some("/kino/neustadt-an-der-aisch/kino-nea-34902")),
    ("Open Air im Freilandmuseum", "Open Air im Freilandmuseum", "A1828", Some("/kino/bad-windsheim/open-air-kino-im-fraenkischen-freilandmuseum-43701")),
    ("Lichtspiele Großhabersdorf", "Lichtspiele Großhabersdorf", "A1365", Some("/kino/grosshabersdorf/lichtspiele-32781"))
  ))
  private def r_paderborn: R = ("paderborn", "Paderborn", "Nordrhein-Westfalen", 51.71905, 8.75439, Seq("Paderborn", "Paderborn (Kernstadt)", "Bad Driburg", "Brakel", "Bad Lippspringe"), Seq(
    ("Pollux by Cineplex Paderborn", "Pollux by Cineplex Paderborn", "A0315", None),
    ("UCI Paderborn", "UCI Paderborn", "A0832", Some("/kino/paderborn/uci-kinowelt-paderborn-74676")),
    ("Programmkino Lichtblick e.V.", "Programmkino Lichtblick e.V.", "A1044", Some("/kino/paderborn/programmkino-lichtblick-e.v.-66276")),
    ("Kino Bad Driburg", "Kino Bad Driburg", "A0706", Some("/kino/bad-driburg/kino-bad-driburg-31741")),
    ("Kino Brakel", "Kino Brakel", "A0711", Some("/kino/brakel/kino-brakel-66537")),
    ("Odins Filmtheater", "Odins Filmtheater", "A1008", Some("/kino/bad-lippspringe/odins-filmtheater-65557"))
  ))
  private def r_traunstein: R = ("traunstein", "Traunstein", "Bayern", 47.86825, 12.64335, Seq("Traunstein", "Traunreut", "Bad Reichenhall", "Trostberg", "Prien am Chiemsee"), Seq(
    ("Cine Chiemgau Traunstein", "Cine Chiemgau Traunstein", "A0177", Some("/kino/traunstein/cine-chiemgau-33025")),
    ("Kinos am Bahnhof", "Kinos am Bahnhof", "A0136", Some("/kino/traunstein/kinos-am-bahnhof-38757")),
    ("Cine Chiemgau Traunreut", "Cine Chiemgau Traunreut", "A1191", Some("/kino/traunreut/cine-chiemgau-61097")),
    ("Park-Kino", "Park-Kino", "A1585", Some("/kino/bad-reichenhall/parkkino-32686")),
    ("Stadtkino Trostberg", "Stadtkino Trostberg", "A1620", Some("/kino/trostberg/stadtkino-33028")),
    ("Mikes Kino", "Mikes Kino", "A0961", Some("/kino/prien-am-chiemsee/mikes-kino-65516"))
  ))
  private def r_brandenburg: R = ("brandenburg", "Brandenburg", "Brandenburg", 52.41667, 12.55, Seq("Brandenburg", "Brandenburg an der Havel", "Rathenow", "Werder", "Genthin", "Bad Belzig"), Seq(
    ("Concerthaus Kino", "Concerthaus Kino", "A0421", Some("/kino/brandenburg-an-der-havel/concerthaus-kino-31464")),
    ("Fontane Klub", "Fontane Klub", "A1930", Some("/kino/brandenburg-an-der-havel/fontane-klub-71552")),
    ("Haveltor Kino", "Haveltor Kino", "A0603", Some("/kino/rathenow/haveltorkino-rathenow-55136")),
    ("Scala Kulturpalast Werder", "Scala Kulturpalast Werder", "A0555", Some("/kino/werder-havel/scala-kulturpalast-84477")),
    ("Union Kino Genthin", "Union Kino Genthin", "A1239", Some("/kino/genthin/union-kino-31530")),
    ("Hofgarten Kino", "Hofgarten Kino", "A0611", None)
  ))
  private def r_altenburg: R = ("altenburg", "Altenburg", "Thüringen", 50.98763, 12.43684, Seq("Altenburg", "Zeitz", "Glauchau", "Werdau", "Borna", "Geithain"), Seq(
    ("Capitol Altenburg", "Capitol Altenburg", "A0143", Some("/kino/altenburg/capitol-49436")),
    ("Brühl Cinema Zeitz", "Brühl Cinema Zeitz", "A2637", Some("/kino/zeitz-elster/bruehl-cinema-81897")),
    ("Clubkino Glauchau e.V.", "Clubkino Glauchau e.V.", "A2315", Some("/kino/glauchau/clubkino-glauchau-e.v.-74299")),
    ("Auto- und Freilichtkino Langenhessen", "Auto- und Freilichtkino Langenhessen", "A2695", None),
    ("Volksplatz Borna Open Air Kino", "Volksplatz Borna Open Air Kino", "A1804", None),
    ("Kino im Bürgerhaus Geithain", "Kino im Bürgerhaus Geithain", "A2944", None)
  ))
  private def r_riesa: R = ("riesa", "Riesa", "Sachsen", 51.30777, 13.29168, Seq("Riesa", "Meissen", "Döbeln", "Torgau", "Grossenhain", "Gröditz"), Seq(
    ("Filmpalast Capital Riesa", "Filmpalast Capital Riesa", "A0515", Some("/kino/riesa/filmpalast-capitol-31648")),
    ("Filmpalast Meißen", "Filmpalast Meißen", "A0371", Some("/kino/meissen-sachsen/filmpalast-38490")),
    ("Cinema Döbeln", "Cinema Döbeln", "A0215", Some("/kino/doebeln/cinema-37643")),
    ("KAP-Torgau e.V.", "KAP-Torgau e.V.", "A0646", Some("/kino/torgau/kaptorgau-e.v.-42677")),
    ("Filmgalerie Großenhain", "Filmgalerie Großenhain", "A0496", Some("/kino/grossenhain-sachsen/filmgalerie-39677")),
    ("Castello", "Castello", "A0182", Some("/kino/groeditz-bei-riesa/castello-65477"))
  ))
  private def r_sigmaringen: R = ("sigmaringen", "Sigmaringen", "Baden-Württemberg", 48.08829, 9.23033, Seq("Sigmaringen", "Riedlingen", "Mengen", "Gammertingen", "Emmingen-Liptingen", "Kolbingen"), Seq(
    ("Lichtspielhaus Riedlingen", "Lichtspielhaus Riedlingen", "A1148", Some("/kino/riedlingen/lichtspielhaus-87299")),
    ("Kinocenter Mengen", "Kinocenter Mengen", "A1502", Some("/kino/mengen/kinocenter-32847")),
    ("Open Air", "Open Air", "A2772", Some("/kino/gammertingen/openair-kino-gammertingen-85497")),
    ("Kinomobil Stuttgart - Musiksaal der Witthohschule", "Kinomobil Stuttgart - Musiksaal der Witthohschule", "A1941", None),
    ("Kinomobil Stuttgart - Kolbingen Dorfplatz", "Kinomobil Stuttgart - Kolbingen Dorfplatz", "A0811", None)
  ))
  private def r_anklam: R = ("anklam", "Anklam", "Mecklenburg-Vorpommern", 53.85637, 13.68965, Seq("Anklam", "Ueckermünde", "Zinnowitz", "Heringsdorf", "Koserow", "Zempin"), Seq(
    ("Kino-Center Anklam", "Kino-Center Anklam", "A0703", Some("/kino/anklam/kinocenter-39957")),
    ("Volksbühne", "Volksbühne", "A1262", Some("/kino/ueckermuende/volksbuehne-31693")),
    ("Club-Kino", "Club-Kino", "A0413", Some("/kino/zinnowitz-ostseebad/clubkino-31719")),
    ("Sommerkino Heringsdorf", "Sommerkino Heringsdorf", "A0751", Some("/kino/heringsdorf/sommerkino-heringsdorf-89417")),
    ("Autokino Usedom", "Autokino Usedom", "A0074", Some("/kino/koserow/autokino-usedom-36212")),
    ("Sommerkino Zempin (SAK)", "Sommerkino Zempin (SAK)", "A2092", Some("/kino/zempin/sommerkino-zempin-sak-31715"))
  ))
  private def r_altensteig: R = ("altensteig", "Altensteig", "Baden-Württemberg", 48.58649, 8.60395, Seq("Altensteig", "Bad Wildbad", "Schömberg", "Althengstett", "Lossburg", "Haiterbach"), Seq(
    ("Open-Air Kino im Schlossgarten", "Open-Air Kino im Schlossgarten", "A1934", None),
    ("KiWi-Kino", "KiWi-Kino", "A0847", Some("/kino/bad-wildbad/kiwi-kino-86258")),
    ("Kurtheater Schömberg", "Kurtheater Schömberg", "A0888", Some("/kino/schoemberg/kurtheater-32249")),
    ("Kinomobil Stuttgart - Festhalle Althengstett", "Kinomobil Stuttgart - Festhalle Althengstett", "A0799", Some("/kino/althengstett/kinomobil-stuttgart-festhalle-41783")),
    ("Kino unterm Sternenhimmel Loßburg", "Kino unterm Sternenhimmel Loßburg", "A2083", Some("/kino/lossburg/kino-unterm-sternenhimmel-51021")),
    ("Kinomobil Stuttgart - Festhalle Haiterbach", "Kinomobil Stuttgart - Festhalle Haiterbach", "A0804", Some("/kino/haiterbach/kinomobil-stuttgart-festhalle-41867"))
  ))
  private def r_celle: R = ("celle", "Celle", "Niedersachsen", 52.62264, 10.08047, Seq("Celle", "Burgdorf", "Wathlingen"), Seq(
    ("Achteinhalb", "Achteinhalb", "A0005", Some("/kino/celle/kino-achteinhalb-46417")),
    ("Kammer-Lichtspiele / Filmpalast Celle", "Kammer-Lichtspiele / Filmpalast Celle", "A0638", Some("/kino/celle/kammerlichtspiele-37219")),
    ("Open Air Kino", "Open Air Kino", "A2869", None),
    ("Neue Schauburg Burgdorf", "Neue Schauburg Burgdorf", "A1572", Some("/kino/burgdorf/neue-schauburg-32381")),
    ("Kleines Kino Wathlingen", "Kleines Kino Wathlingen", "A2709", None)
  ))
  private def r_neubrandenburg: R = ("neubrandenburg", "Neubrandenburg", "Mecklenburg-Vorpommern", 53.55735, 13.26105, Seq("Neubrandenburg", "Neustrelitz", "Feldberger Seenlandschaft"), Seq(
    ("CineStar Neubrandenburg", "CineStar Neubrandenburg", "A0331", Some("/kino/neubrandenburg-mecklenburg/cinestar-31615")),
    ("Latücht - Kommunales Kino", "Latücht - Kommunales Kino", "A0894", Some("/kino/neubrandenburg-mecklenburg/kino-latuecht-46637")),
    ("Basiskulturfabrik", "Basiskulturfabrik", "A0084", None),
    ("Movie Star Neustrelitz", "Movie Star Neustrelitz", "A2333", Some("/kino/neustrelitz/movie-star-74697")),
    ("Clubkino Feldberg", "Clubkino Feldberg", "A2761", None)
  ))
  private def r_dessau_rosslau: R = ("dessau-rosslau", "Dessau-Roßlau", "Sachsen-Anhalt", 51.83864, 12.24555, Seq("Dessau-Roßlau", "Dessau", "Lutherstadt Wittenberg", "Wittenberg"), Seq(
    ("Kiez-Kino", "Kiez-Kino", "A2849", None),
    ("Open Air am Landhaus Dessau", "Open Air am Landhaus Dessau", "A2702", None),
    ("UCI Dessau", "UCI Dessau", "A2890", Some("/kino/dessau-anhalt/uci-kinowelt-dessau-39078")),
    ("Sommerkino auf dem Cranach-Hof", "Sommerkino auf dem Cranach-Hof", "A2507", Some("/kino/lutherstadt-wittenberg/sommerkino-auf-dem-cranachhof-75996")),
    ("Centralkino Wittenberg", "Centralkino Wittenberg", "A0209", Some("/kino/wittenberg/central-kino-31703"))
  ))
  private def r_emden: R = ("emden", "Emden", "Niedersachsen", 53.36592, 7.20846, Seq("Emden", "Aurich", "Papenburg", "Leer"), Seq(
    ("CineStar Emden", "CineStar Emden", "A0349", Some("/kino/emden-ostfriesland/cinestar-48578")),
    ("Sommernachtskino im Van-Ameren Bad", "Sommernachtskino im Van-Ameren Bad", "A1859", Some("/kino/emden-ostfriesland/sommernachtskino-im-vanameren-bad-67897")),
    ("Kino Aurich", "Kino Aurich", "A2911", Some("/kino/aurich/kino-aurich-57096")),
    ("Kino Papenburg", "Kino Papenburg", "A0466", Some("/kino/papenburg/kino-papenburg-57136")),
    ("Kino Leer (Ostfriesland)", "Kino Leer (Ostfriesland)", "A0675", None)
  ))
  private def r_waldshut_tiengen: R = ("waldshut-tiengen", "Waldshut-Tiengen", "Baden-Württemberg", 47.62323, 8.21717, Seq("Waldshut-Tiengen", "Bad Säckingen", "Lenzkirch", "Sankt Blasien"), Seq(
    ("Albrecht Kino Waldshut", "Albrecht Kino Waldshut", "A2786", Some("/kino/waldshuttiengen/albrecht-kino-waldshut-75377")),
    ("Open Air-Kino Waldshut", "Open Air-Kino Waldshut", "A2225", None),
    ("Gloria Bad Säckingen", "Gloria Bad Säckingen", "A1438", None),
    ("Kultur im Kino", "Kultur im Kino", "A0889", None),
    ("Kino im Kursaal", "Kino im Kursaal", "A2802", None)
  ))
  private def r_bad_urach: R = ("bad-urach", "Bad Urach", "Baden-Württemberg", 48.49107, 9.40009, Seq("Bad Urach", "Nürtingen", "Metzingen", "Ensingen"), Seq(
    ("Bad Uracher Sommer Open Air", "Bad Uracher Sommer Open Air", "A2534", None),
    ("Forum 22", "Forum 22", "A1731", Some("/kino/bad-urach/forum-22-32688")),
    ("Traumpalast Nürtingen", "Traumpalast Nürtingen", "A1511", Some("/kino/nuertingen/traumpalast-nuertingen-32917")),
    ("Luna-Filmtheater", "Luna-Filmtheater", "A1549", Some("/kino/metzingen/lunafilmtheater-32849")),
    ("Kinomobil Stuttgart - BIO-Café Zimt und Zunder", "Kinomobil Stuttgart - BIO-Café Zimt und Zunder", "A0814", None)
  ))
  private def r_frankfurt_an_der_oder: R = ("frankfurt-an-der-oder", "Frankfurt an der Oder", "Brandenburg", 52.34714, 14.55062, Seq("Frankfurt an der Oder", "Frankfurt (Oder)", "Fürstenwalde", "Fürstenwalde an der Spree", "Beeskow", "Bad Saarow"), Seq(
    ("CineStar Frankfurt (Oder)", "CineStar Frankfurt (Oder)", "A0343", Some("/kino/frankfurt-oder/cinestar-36056")),
    ("Kleines Kino", "Kleines Kino", "A2371", None),
    ("Filmtheater Union", "Filmtheater Union", "A2383", Some("/kino/fuerstenwalde-spree/filmtheater-union-31527")),
    ("Open Air Kino Parkbühne", "Open Air Kino Parkbühne", "A2449", None),
    ("Schukurama", "Schukurama", "A2366", Some("/kino/beeskow/schukurama-75023")),
    ("Cinema by Velotel", "Cinema by Velotel", "G0119", None)
  ))
  private def r_amberg: R = ("amberg", "Amberg", "Bayern", 49.44287, 11.86267, Seq("Amberg", "Weiden in der Oberpfalz", "Schwandorf", "Sulzbach", "Sulzbach-Rosenberg"), Seq(
    ("Cineplex Amberg", "Cineplex Amberg", "A2686", Some("/kino/amberg/cineplex-86179")),
    ("Neue Welt Kinocenter", "Neue Welt Kinocenter", "A0985", None),
    ("Lichtwerk Kino Schwandorf", "Lichtwerk Kino Schwandorf", "A2096", Some("/kino/schwandorf/lichtwerk-73216")),
    ("Kinopolis Main-Taunus", "Kinopolis Main-Taunus", "A0831", None),
    ("Lu-Li Sommernachtskino", "Lu-Li Sommernachtskino", "A2533", None)
  ))
  private def r_straubing: R = ("straubing", "Straubing", "Bayern", 48.88126, 12.57385, Seq("Straubing", "Deggendorf", "Dingolfing", "Plattling", "Wörth an der Donau"), Seq(
    ("Citydom", "Citydom", "A0400", Some("/kino/straubing/citydom-33003")),
    ("Lichtspielhaus Deggendorf", "Lichtspielhaus Deggendorf", "A0910", Some("/kino/deggendorf/lichtspielhaus-32720")),
    ("Cinema Filmpalais", "Cinema Filmpalais", "A1363", Some("/kino/dingolfing/cinema-filmpalais-32723")),
    ("Focus Cinemas Plattling", "Focus Cinemas Plattling", "A0552", Some("/kino/plattling/focus-cinemas-37107")),
    ("Donau-Lichtspiele", "Donau-Lichtspiele", "A1396", Some("/kino/woerth-an-der-donau/donaulichtspiele-33071"))
  ))
  private def r_suhl: R = ("suhl", "Suhl", "Thüringen", 50.60911, 10.69401, Seq("Suhl", "Meiningen", "Schmalkalden", "Zella-Mehlis", "Masserberg"), Seq(
    ("Cineplex Suhl", "Cineplex Suhl", "A0304", Some("/kino/suhl/cineplex-41357")),
    ("Casino Lichtspiele Meiningen", "Casino Lichtspiele Meiningen", "A0181", Some("/kino/meiningen/casino-lichtspiele-31607")),
    ("Open Air Kino Schloss Wilhelmsburg", "Open Air Kino Schloss Wilhelmsburg", "A2246", None),
    ("Clubkino Zella-Mehlis", "Clubkino Zella-Mehlis", "A2753", None)
  ))
  private def r_eisenach: R = ("eisenach", "Eisenach", "Thüringen", 50.9807, 10.31522, Seq("Eisenach", "Mühlhausen", "Eschwege", "Bad Langensalza", "Bad Salzungen"), Seq(
    ("Capitol Eisenach", "Capitol Eisenach", "A0161", Some("/kino/eisenach-bei-trier/capitol-eisenach-31511")),
    ("Filmpalast Central Mühlhausen", "Filmpalast Central Mühlhausen", "A0198", Some("/kino/muehlhausen/filmpalast-31612")),
    ("Cinemagic Eschwege", "Cinemagic Eschwege", "A0260", Some("/kino/eschwege/cinemagic-eschwege-32020")),
    ("Burgtheater Bad Langensalza", "Burgtheater Bad Langensalza", "A0121", Some("/kino/bad-langensalza/burgtheater-31347")),
    ("PAB Kinocenter", "PAB Kinocenter", "A0001", Some("/kino/bad-salzungen/%22pab%22-kinocenter-37280"))
  ))
  private def r_nienburg: R = ("nienburg", "Nienburg", "Niedersachsen", 52.64437, 9.21658, Seq("Nienburg", "Verden an der Aller", "Walsrode", "Sulingen", "Hoya"), Seq(
    ("Filmpalast Nienburg", "Filmpalast Nienburg", "A1468", Some("/kino/nienburg-weser/filmpalast-am-hafen-58736")),
    ("Cine City", "Cine City", "A0219", None),
    ("Capitol-Theater Walsrode", "Capitol-Theater Walsrode", "A0167", Some("/kino/walsrode/capitoltheater-78545")),
    ("Filmpalast Sulingen", "Filmpalast Sulingen", "A1423", Some("/kino/sulingen/filmpalast-32620")),
    ("Filmhof Hoya", "Filmhof Hoya", "A0500", Some("/kino/hoya-weser/filmhof-hoya-32496"))
  ))
  private def r_korbach: R = ("korbach", "Korbach", "Hessen", 51.27561, 8.873, Seq("Korbach", "Warburg", "Bad Wildungen", "Wolfhagen", "Willingen"), Seq(
    ("Cine K Korbach", "Cine K Korbach", "A2902", Some("/kino/korbach/cinek-91980")),
    ("Cineplex Warburg", "Cineplex Warburg", "A0311", Some("/kino/warburg/cineplex-36756")),
    ("Wandelhalle Reinhardshausen", "Wandelhalle Reinhardshausen", "A2726", Some("/kino/bad-wildungen/wandelhalle-reinhardshausen-87357")),
    ("Cinema Wolfhagen", "Cinema Wolfhagen", "A2790", Some("/kino/wolfhagen/cinema-32310")),
    ("Kino Studio Willingen", "Kino Studio Willingen", "A0096", Some("/kino/willingen/kino-studio-60757"))
  ))
  private def r_daun: R = ("daun", "Daun", "Rheinland-Pfalz", 50.19716, 6.82942, Seq("Daun", "Prüm", "Cochem", "Hillesheim", "Nürburg"), Seq(
    ("Kinopalast Vulkaneifel", "Kinopalast Vulkaneifel", "A0822", Some("/kino/daun/kinopalast-vulkaneifel-31786")),
    ("Eifel-Kinocenter", "Eifel-Kinocenter", "A0407", Some("/kino/pruem/eifelkinocenter-31896")),
    ("Apollo Cochern", "Apollo Cochern", "A0041", Some("/kino/cochem/apollo-31783")),
    ("Eifel-Film-Bühne", "Eifel-Film-Bühne", "A0458", Some("/kino/hillesheim/eifelfilmbuehne-31822")),
    ("ring°kino", "ring°kino", "A2670", Some("/kino/nuerburg/ring%C2%B0kino-78917"))
  ))
  private def r_oberstdorf: R = ("oberstdorf", "Oberstdorf", "Bayern", 47.40724, 10.27939, Seq("Oberstdorf", "Immenstadt im Allgäu"), Seq(
    ("Kurfilmtheater Oberstdorf", "Kurfilmtheater Oberstdorf", "A0883", Some("/kino/oberstdorf/kurfilmtheater-66638")),
    ("LOFT Oberstdorf", "LOFT Oberstdorf", "A0222", Some("/kino/oberstdorf/loft-65776")),
    ("Open Air Kino zwischen den Schanzen/Erdinger Arena", "Open Air Kino zwischen den Schanzen/Erdinger Arena", "A2551", Some("/kino/oberstdorf/open-air-kino-zwischen-den-schanzenerdinger-arena-76344")),
    ("Union Filmtheater Immenstadt im Allgäu", "Union Filmtheater Immenstadt im Allgäu", "A1227", None)
  ))
  private def r_flensburg: R = ("flensburg", "Flensburg", "Schleswig-Holstein", 54.78805, 9.43722, Seq("Flensburg", "Schleswig", "Kappeln"), Seq(
    ("Kino 51 Stufen", "Kino 51 Stufen", "A1481", Some("/kino/flensburg/kino-51-stufen-32417")),
    ("UCI Flensburg", "UCI Flensburg", "A0829", Some("/kino/flensburg/uci-kinowelt-flensburg-74576")),
    ("Filmtheater Capitol Schleswig", "Filmtheater Capitol Schleswig", "A1126", Some("/kino/schleswig/capitol-filmpalast-53557")),
    ("Capitol Theater Kappeln", "Capitol Theater Kappeln", "A1664", None)
  ))
  private def r_gummersbach: R = ("gummersbach", "Gummersbach", "Nordrhein-Westfalen", 51.02608, 7.56473, Seq("Gummersbach", "Olpe", "Attendorn"), Seq(
    ("SEVEN Kinocenter Gummersbach", "SEVEN Kinocenter Gummersbach", "A2912", Some("/kino/gummersbach/seven-kinocenter-91926")),
    ("Cineplex Olpe", "Cineplex Olpe", "A2115", Some("/kino/olpe/cineplex-72377")),
    ("JAC Kino Attendorn", "JAC Kino Attendorn", "A2914", Some("/kino/attendorn/jac-kino-attendorn-91983"))
  ))
  private def r_garmisch_partenkirchen: R = ("garmisch-partenkirchen", "Garmisch-Partenkirchen", "Bayern", 47.49209, 11.09576, Seq("Garmisch-Partenkirchen", "Murnau am Staffelsee", "Oberammergau"), Seq(
    ("Hochland-Kino", "Hochland-Kino", "A0609", Some("/kino/garmischpartenkirchen/hochlandkino-32766")),
    ("Kinocenter Garmisch &amp; Aspen im Lamm", "Kinocenter Garmisch &amp; Aspen im Lamm", "A1508", Some("/kino/garmischpartenkirchen/kinocenter-garmisch-und-aspen-im-lamm-32767")),
    ("Kino im Griesbräu", "Kino im Griesbräu", "A0593", None),
    ("Heimgarten Kino", "Heimgarten Kino", "A1451", Some("/kino/oberammergau/heimgarten-kino-33078"))
  ))
  private def r_dorsten: R = ("dorsten", "Dorsten", "Nordrhein-Westfalen", 51.66166, 6.96514, Seq("Dorsten", "Gladbeck", "Dinslaken", "Wesel"), Seq(
    ("Central Kinocenter Dorsten", "Central Kinocenter Dorsten", "A2788", Some("/kino/dorsten/central-kinocenter-186")),
    ("Kommunales Kino im Studio der Stadtbücherei", "Kommunales Kino im Studio der Stadtbücherei", "A0861", None),
    ("Lichtburg Dinslaken", "Lichtburg Dinslaken", "A2882", Some("/kino/dinslaken/lichtburg-center-177")),
    ("Comet Kinos Wesel", "Comet Kinos Wesel", "A0420", Some("/kino/wesel/comet-cine-center-49916"))
  ))
  private def r_stralsund: R = ("stralsund", "Stralsund", "Mecklenburg-Vorpommern", 54.30911, 13.0818, Seq("Stralsund", "Greifswald", "Bergen auf Rügen", "Vitte"), Seq(
    ("CineStar Stralsund", "CineStar Stralsund", "A0359", Some("/kino/stralsund/cinestar-der-filmpalast-31678")),
    ("CineStar Greifswald", "CineStar Greifswald", "A0362", Some("/kino/greifswald-hansestadt/cinestar-der-filmpalast-31541")),
    ("Kino Bergen auf Rügen", "Kino Bergen auf Rügen", "A0709", None),
    ("Zeltkino Hiddensee", "Zeltkino Hiddensee", "A2097", None)
  ))
  private def r_cuxhaven: R = ("cuxhaven", "Cuxhaven", "Niedersachsen", 53.86828, 8.69902, Seq("Cuxhaven", "Brunsbüttel", "Meldorf", "Büsum"), Seq(
    ("Bali-Service-Kino", "Bali-Service-Kino", "A0009", None),
    ("Metropol Brunsbüttel 1", "Metropol Brunsbüttel 1", "A1565", None),
    ("Deutsches Haus-Lichtspiele", "Deutsches Haus-Lichtspiele", "A1390", Some("/kino/meldorf-holstein/deutsches-hauslichtspiele-32543")),
    ("Lichtblick Büsum", "Lichtblick Büsum", "A2909", Some("/kino/buesum/lichtblick-filmtheater-91985"))
  ))
  private def r_nordhausen: R = ("nordhausen", "Nordhausen", "Thüringen", 51.5018, 10.7957, Seq("Nordhausen", "Sangerhausen", "Sondershausen", "Bleicherode"), Seq(
    ("Filmpalast Nordhausen", "Filmpalast Nordhausen", "A0517", Some("/kino/nordhausen/filmpalast-31624")),
    ("Movie Star Sangerhausen", "Movie Star Sangerhausen", "A0188", Some("/kino/sangerhausen/movie-star-35336")),
    ("Cinema 64", "Cinema 64", "A2367", Some("/kino/sondershausen-thueringen/cinema-64-31672")),
    ("Filmtheater Bleicherode", "Filmtheater Bleicherode", "A0534", Some("/kino/bleicherode/filmtheater-35157"))
  ))
  private def r_coburg: R = ("coburg", "Coburg", "Bayern", 50.25937, 10.96384, Seq("Coburg", "Sonneberg", "Lichtenfels", "Kronach"), Seq(
    ("Utopolis", "Utopolis", "A1252", Some("/kino/coburg/utopolis-43397")),
    ("Kammer Lichtspiele", "Kammer Lichtspiele", "A0640", Some("/kino/sonneberg-thueringen/kammer-lichtspiele-31673")),
    ("Neue Filmbühne Lichtenfels", "Neue Filmbühne Lichtenfels", "A0981", Some("/kino/lichtenfels/neue-filmbuehne-38563")),
    ("Filmburg Kronach", "Filmburg Kronach", "A1410", Some("/kino/kronach/filmburg-32817"))
  ))
  private def r_halberstadt: R = ("halberstadt", "Halberstadt", "Sachsen-Anhalt", 51.89562, 11.05622, Seq("Halberstadt", "Wernigerode", "Quedlinburg", "Thale"), Seq(
    ("Zuckerfabrik Kinopark", "Zuckerfabrik Kinopark", "A1013", Some("/kino/halberstadt/zuckerfabrik-kinopark-35338")),
    ("Volkslichtspiele Wernigerode", "Volkslichtspiele Wernigerode", "A1265", None),
    ("Studiokino Eisenstein im Kulturzentrum", "Studiokino Eisenstein im Kulturzentrum", "A1167", Some("/kino/quedlinburg/studiokino-eisenstein-im-kulturzentrum-65577")),
    ("Central Theater Thale", "Central Theater Thale", "A0208", Some("/kino/thale/central-theater-31687"))
  ))
  private def r_meppen: R = ("meppen", "Meppen", "Niedersachsen", 52.69064, 7.29097, Seq("Meppen", "Haren", "Löningen", "Haselünne"), Seq(
    ("Kino Meppen", "Kino Meppen", "A0573", Some("/kino/meppen/kino-meppen-34288")),
    ("Ferienzentrum Schloss Dankern", "Ferienzentrum Schloss Dankern", "A1387", Some("/kino/haren-ems/schloss-dankern-kino-33248")),
    ("Li-Lo-Lichtspiele", "Li-Lo-Lichtspiele", "A0916", Some("/kino/loeningen/lilolichtspiele-32531")),
    ("Hasetor-Lichtspiele", "Hasetor-Lichtspiele", "A2650", None)
  ))
  private def r_neuruppin: R = ("neuruppin", "Neuruppin", "Brandenburg", 52.92815, 12.80311, Seq("Neuruppin", "Wittstock", "Wusterhausen", "Zempow"), Seq(
    ("Union Filmtheater Neuruppin", "Union Filmtheater Neuruppin", "A0685", Some("/kino/neuruppin/union-filmtheater-31621")),
    ("Astoria Wittstock", "Astoria Wittstock", "A2377", None),
    ("Lindenkino", "Lindenkino", "A0920", None),
    ("Autokino Zempow", "Autokino Zempow", "A0071", None)
  ))
  private def r_bad_neuenahr_ahrweiler: R = ("bad-neuenahr-ahrweiler", "Bad Neuenahr-Ahrweiler", "Rheinland-Pfalz", 50.54322, 7.1113, Seq("Bad Neuenahr-Ahrweiler", "Wachtberg", "Mayen", "Asbach"), Seq(
    ("Kino-Center Rhein-Ahr", "Kino-Center Rhein-Ahr", "A1115", None),
    ("Drehwerk 1719", "Drehwerk 1719", "A2117", Some("/kino/wachtberg/drehwerk-1719-72977")),
    ("Corso Kino", "Corso Kino", "A0424", Some("/kino/mayen/corso-kino-58538")),
    ("Cine 5", "Cine 5", "A2640", Some("/kino/asbach/cine-5-78858"))
  ))
  private def r_rotenburg_an_der_wuemme: R = ("rotenburg-an-der-wuemme", "Rotenburg an der Wümme", "Niedersachsen", 53.11032, 9.4036, Seq("Rotenburg an der Wümme", "Soltau", "Schneverdingen", "Zeven"), Seq(
    ("Stadtkino Rotenburg", "Stadtkino Rotenburg", "A2372", None),
    ("Gloria Kino Center Soltau", "Gloria Kino Center Soltau", "A1441", None),
    ("LichtSpiel e.V. Schneverdingen", "LichtSpiel e.V. Schneverdingen", "A2673", Some("/kino/schneverdingen/lichtspiel-e.v.-85637")),
    ("Central Theater Zeven", "Central Theater Zeven", "A2354", Some("/kino/zeven/central-theater-32652"))
  ))
  private def r_salzwedel: R = ("salzwedel", "Salzwedel", "Sachsen-Anhalt", 52.85297, 11.15287, Seq("Salzwedel", "Wittingen", "Lüchow", "Jameln"), Seq(
    ("Filmpalast Salzwedel (Hansestadt)", "Filmpalast Salzwedel (Hansestadt)", "A1347", None),
    ("Wittinger Lichtspiele", "Wittinger Lichtspiele", "A0676", Some("/kino/wittingen/wittinger-lichtspiele-32644")),
    ("Kino Alte Brennerei", "Kino Alte Brennerei", "A0699", Some("/kino/luechow-bei-sandesneben/kino-alte-brennerei-32538")),
    ("Cafe Grenzbereiche", "Cafe Grenzbereiche", "A0133", Some("/kino/jameln/kulturverein-platenlaase-caf%C3%A9-grenzbereiche-65483"))
  ))
  private def r_bad_berleburg: R = ("bad-berleburg", "Bad Berleburg", "Nordrhein-Westfalen", 51.05224, 8.39227, Seq("Bad Berleburg", "Hilchenbach", "Bad Laasphe", "Winterberg"), Seq(
    ("Capitol Bad Berleburg", "Capitol Bad Berleburg", "A0165", Some("/kino/bad-berleburg/capitol-31740")),
    ("Viktoria Filmtheater", "Viktoria Filmtheater", "A1256", Some("/kino/hilchenbach/viktoria-filmtheater-31821")),
    ("Residenztheater", "Residenztheater", "A1063", Some("/kino/bad-laasphe/residenztheater-41902")),
    ("Filmtheater Winterberg", "Filmtheater Winterberg", "A0543", Some("/kino/winterberg/filmtheater-31940"))
  ))
  private def r_weissenburg: R = ("weissenburg", "Weissenburg", "Bayern", 49.03095, 10.97221, Seq("Weissenburg", "Gunzenhausen", "Treuchtlingen", "Freystadt"), Seq(
    ("Kinocenter Weißenburg in Bayern", "Kinocenter Weißenburg in Bayern", "A1506", Some("/kino/weissenburg-in-bayern/kinocenter-33061")),
    ("Movieworld", "Movieworld", "A0975", Some("/kino/gunzenhausen/movieworld-64637")),
    ("Central Kino und Kultur Treuchtlingen", "Central Kino und Kultur Treuchtlingen", "A0189", Some("/kino/treuchtlingen/central-kino-65597")),
    ("Kino im Cafe Restaurant Beck", "Kino im Cafe Restaurant Beck", "A1485", None)
  ))
  private def r_marktredwitz: R = ("marktredwitz", "Marktredwitz", "Bayern", 50.00443, 12.08593, Seq("Marktredwitz", "Selb", "Tirschenreuth", "Mitterteich"), Seq(
    ("Cineplanet Marktredwitz", "Cineplanet Marktredwitz", "A0870", Some("/kino/marktredwitz/cineplanet-45477")),
    ("Cineplex Kinocenter Selb", "Cineplex Kinocenter Selb", "A0763", None),
    ("Cineplanet Tirschenreuth", "Cineplanet Tirschenreuth", "A0302", Some("/kino/tirschenreuth/cineplanet-38303")),
    ("Angerlichtspiele Mitterteich", "Angerlichtspiele Mitterteich", "A2635", Some("/kino/mitterteich/angerlichtspiele-32851"))
  ))
  private def r_bremerhaven: R = ("bremerhaven", "Bremerhaven", "Bremen", 53.55357, 8.57553, Seq("Bremerhaven", "Wilhelmshaven", "Brake"), Seq(
    ("CineMotion Bremerhaven", "CineMotion Bremerhaven", "A1773", Some("/kino/bremerhaven/cinemotion-im-havenhaus-bremerhaven-70856")),
    ("UCI Wilhelmshaven", "UCI Wilhelmshaven", "A0043", Some("/kino/wilhelmshaven/uci-kinowelt-wilhelmshaven-46376")),
    ("Centraltheater Brake", "Centraltheater Brake", "A2694", Some("/kino/brake/centraltheater-brake-86458"))
  ))
  private def r_bayreuth: R = ("bayreuth", "Bayreuth", "Bayern", 49.94782, 11.57893, Seq("Bayreuth", "Kulmbach", "Hollfeld"), Seq(
    ("Cineplex Bayreuth", "Cineplex Bayreuth", "A0314", Some("/kino/bayreuth/cineplex-53296")),
    ("Cineplex Kulmbach", "Cineplex Kulmbach", "A0764", Some("/kino/kulmbach/cineplex-32819")),
    ("Kintopp", "Kintopp", "A1517", Some("/kino/hollfeld/kintopp-32795"))
  ))
  private def r_goslar: R = ("goslar", "Goslar", "Niedersachsen", 51.90425, 10.42766, Seq("Goslar", "Herzberg am Harz", "Bad Gandersheim"), Seq(
    ("Cineplex Goslar", "Cineplex Goslar", "A0318", Some("/kino/goslar/cineplex-goslar-50417")),
    ("Central-Lichtspiele Herzberg am Harz", "Central-Lichtspiele Herzberg am Harz", "A1351", Some("/kino/herzberg-am-harz/centrallichtspiele-32488")),
    ("Kino Gandeon", "Kino Gandeon", "A2707", Some("/kino/bad-gandersheim/kino-gandeon-87077"))
  ))
  private def r_bernburg: R = ("bernburg", "Bernburg", "Sachsen-Anhalt", 51.79464, 11.7401, Seq("Bernburg", "Köthen", "Aschersleben"), Seq(
    ("Filmtheater Capitol Bernburg", "Filmtheater Capitol Bernburg", "A0542", Some("/kino/bernburg-saale/filmtheater-capitol-31453")),
    ("Cine Circus", "Cine Circus", "A0218", None),
    ("Filmpalast Aschersleben", "Filmpalast Aschersleben", "A0356", Some("/kino/aschersleben/filmpalast-31339"))
  ))
  private def r_bad_hersfeld: R = ("bad-hersfeld", "Bad Hersfeld", "Hessen", 50.87197, 9.70891, Seq("Bad Hersfeld", "Alsfeld", "Bebra"), Seq(
    ("Cineplex Bad Hersfeld", "Cineplex Bad Hersfeld", "A0765", Some("/kino/bad-hersfeld/cineplex-31958")),
    ("Kinocenter Alsfeld", "Kinocenter Alsfeld", "A1505", Some("/kino/alsfeld/kinocenter-31946")),
    ("Biber-Kino-Center", "Biber-Kino-Center", "A0094", None)
  ))
  private def r_waren_mueritz: R = ("waren-mueritz", "Waren (Müritz)", "Mecklenburg-Vorpommern", 53.51986, 12.68128, Seq("Waren (Müritz)", "Malchin", "Malchow"), Seq(
    ("CineStar Waren (Müritz)", "CineStar Waren (Müritz)", "A2794", Some("/kino/waren/cinestar-38780")),
    ("Filmbühne Malchin", "Filmbühne Malchin", "A0660", None),
    ("Kino Malchow", "Kino Malchow", "A0535", Some("/kino/malchow/filmtheater-35180"))
  ))
  private def r_wittenberge: R = ("wittenberge", "Wittenberge", "Brandenburg", 53.00005, 11.74944, Seq("Wittenberge", "Pritzwalk", "Perleberg"), Seq(
    ("Movie Star Wittenberge", "Movie Star Wittenberge", "A0968", Some("/kino/wittenberge/movie-star-38202")),
    ("Kulturhaus Pritzwalk", "Kulturhaus Pritzwalk", "A2625", Some("/kino/pritzwalk/kulturhaus-pritzwalk-81799")),
    ("Movie Star Perleberg", "Movie Star Perleberg", "A1234", Some("/kino/perleberg/movie-star-65579"))
  ))
  private def r_marktoberdorf: R = ("marktoberdorf", "Marktoberdorf", "Bayern", 47.77964, 10.61713, Seq("Marktoberdorf", "Füssen", "Schongau"), Seq(
    ("Filmburg - Das Theaterkino", "Filmburg - Das Theaterkino", "A1409", Some("/kino/marktoberdorf/filmburg-das-theaterkino-32841")),
    ("Alpenfilmtheater Füssen", "Alpenfilmtheater Füssen", "A1472", Some("/kino/fuessen/alpenfilmtheater-32763")),
    ("Lagerhauskino", "Lagerhauskino", "A0892", Some("/kino/schongau/lagerhauskino-32975"))
  ))
  private def r_erbach_im_odenwald: R = ("erbach-im-odenwald", "Erbach im Odenwald", "Baden-Württemberg", 49.66148, 8.99402, Seq("Erbach im Odenwald", "Höchst im Odenwald", "Miltenberg"), Seq(
    ("Erbacher Lichtspiele", "Erbacher Lichtspiele", "A0690", None),
    ("Höchster Lichtspiele", "Höchster Lichtspiele", "A0240", None),
    ("Schloß-Theater", "Schloß-Theater", "A1131", Some("/kino/miltenberg/schlosstheater-32183"))
  ))
  private def r_regen: R = ("regen", "Regen", "Bayern", 48.9719, 13.12824, Seq("Regen", "Zwiesel", "Viechtach"), Seq(
    ("CinePalast Regen", "CinePalast Regen", "A1460", Some("/kino/regen/cinepalast-32946")),
    ("Filmtheater Zwiesel", "Filmtheater Zwiesel", "A0704", Some("/kino/zwiesel/filmtheater-57576")),
    ("Neue Post Lichtspiele", "Neue Post Lichtspiele", "A0983", None)
  ))
  private def r_bad_schwalbach: R = ("bad-schwalbach", "Bad Schwalbach", "Hessen", 50.14196, 8.06964, Seq("Bad Schwalbach", "Nastätten", "Hahnstätten"), Seq(
    ("Bambi &amp; Camera", "Bambi &amp; Camera", "A1311", Some("/kino/bad-schwalbach/bambi-und-camera-88337")),
    ("Kinocenter Nastätten", "Kinocenter Nastätten", "A0774", None),
    ("Kreml Kulturhaus", "Kreml Kulturhaus", "A2755", Some("/kino/hahnstaetten/kreml-kulturhaus-66339"))
  ))
  private def r_niebuell: R = ("niebuell", "Niebüll", "Schleswig-Holstein", 54.78663, 8.82854, Seq("Niebüll", "Leck", "Wyk auf Föhr"), Seq(
    ("Eck&#039; s Kino", "Eck&#039; s Kino", "A1697", None),
    ("Deli-Kino", "Deli-Kino", "A2368", Some("/kino/leck/delikino-32522")),
    ("Filmtheater am Sandwall", "Filmtheater am Sandwall", "G011C", Some("/kino/wyk-auf-foehr/filmtheater-am-sandwall-32650"))
  ))
  private def r_uffenheim: R = ("uffenheim", "Uffenheim", "Bayern", 49.54415, 10.23286, Seq("Uffenheim", "Creglingen", "Burgbernheim"), Seq(
    ("Open Air Rudolzhofen Kino", "Open Air Rudolzhofen Kino", "A2560", Some("/kino/uffenheim/open-air-kino-rudolzhofen-76399")),
    ("Kinomobil Stuttgart - Kommunales Kino Creglingen", "Kinomobil Stuttgart - Kommunales Kino Creglingen", "A0800", Some("/kino/creglingen/kinomobil-stuttgart-41787")),
    ("Open Air Kino&quot;Am Kappelenberg&quot;", "Open Air Kino&quot;Am Kappelenberg&quot;", "A2058", Some("/kino/burgbernheim/open-air-kino-am-kappelenberg-68517"))
  ))
  private def r_bad_lobenstein: R = ("bad-lobenstein", "Bad Lobenstein", "Thüringen", 50.45223, 11.6393, Seq("Bad Lobenstein", "Wurzbach", "Hirschberg"), Seq(
    ("Kino am Park", "Kino am Park", "A2656", Some("/kino/bad-lobenstein/kino-am-park-31590")),
    ("Kino Wurzbach", "Kino Wurzbach", "A0758", Some("/kino/wurzbach/kino-wurzbach-31709")),
    ("Olympia Hirschberg", "Olympia Hirschberg", "A1580", None)
  ))
  private def r_langeoog: R = ("langeoog", "Langeoog", "Niedersachsen", 53.74552, 7.48175, Seq("Langeoog", "Spiekeroog", "Baltrum"), Seq(
    ("Windlicht", "Windlicht", "A2757", Some("/kino/langeoog/kino-windlicht-32518")),
    ("Inselkino Spiekeroog", "Inselkino Spiekeroog", "A2884", Some("/kino/spiekeroog/inselkino-32615"))
  ))
  private def r_kleve: R = ("kleve", "Kleve", "Schleswig-Holstein", 51.78826, 6.13865, Seq("Kleve", "Goch"), Seq(
    ("Tichelpark Kleve", "Tichelpark Kleve", "A1182", Some("/kino/kleve/tichelpark-kleve-30475")),
    ("Goli Theater Goch", "Goli Theater Goch", "A2597", Some("/kino/goch/goli-theater-goch-76617"))
  ))
  private def r_luckenwalde: R = ("luckenwalde", "Luckenwalde", "Brandenburg", 52.09029, 13.16772, Seq("Luckenwalde", "Dahme"), Seq(
    ("Union Kino-Center", "Union Kino-Center", "A1233", Some("/kino/luckenwalde/union-kinocenter-54237")),
    ("Kino-Cafe Dahme", "Kino-Cafe Dahme", "A0836", Some("/kino/dahme-holstein/kino-cafe-31477"))
  ))
  private def r_prenzlau: R = ("prenzlau", "Prenzlau", "Brandenburg", 53.31702, 13.86397, Seq("Prenzlau", "Templin"), Seq(
    ("Union Filmtheater Prenzlau", "Union Filmtheater Prenzlau", "A0666", Some("/kino/prenzlau/union-filmtheater-prenzlau-64357")),
    ("Kino im Multikulturellen Centrum", "Kino im Multikulturellen Centrum", "A0016", Some("/kino/templin/kino-im-multikulturellen-centrum-31685"))
  ))
  private def r_husum: R = ("husum", "Husum", "Schleswig-Holstein", 54.4858, 9.05239, Seq("Husum", "Heide"), Seq(
    ("Kino-Center Husum", "Kino-Center Husum", "A0762", Some("/kino/husum/kinocenter-32498")),
    ("LichtBlick Heide", "LichtBlick Heide", "A0265", Some("/kino/heide-holstein/lichtblick-32484"))
  ))
  private def r_finsterwalde: R = ("finsterwalde", "Finsterwalde", "Brandenburg", 51.63388, 13.70662, Seq("Finsterwalde", "Schwarzheide"), Seq(
    ("Weltspiegel Finsterwalde", "Weltspiegel Finsterwalde", "A1273", Some("/kino/finsterwalde/weltspiegel-31520")),
    ("Extra Kinowelt", "Extra Kinowelt", "A0477", Some("/kino/schwarzheide/extrakinowelt-im-freizeitpark-wandelhof-65537"))
  ))
  private def r_cham: R = ("cham", "Cham", "Bayern", 49.22565, 12.65501, Seq("Cham", "Oberviechtach"), Seq(
    ("Cine-World", "Cine-World", "A0389", Some("/kino/cham/cineworld-e.k.-35162")),
    ("Kino in OVI", "Kino in OVI", "A1538", Some("/kino/oberviechtach/kino-in-ovi-32921"))
  ))
  private def r_fehmarn: R = ("fehmarn", "Fehmarn", "Schleswig-Holstein", 54.4378, 11.19352, Seq("Fehmarn", "Oldenburg in Holstein"), Seq(
    ("Burg-Film-Theater", "Burg-Film-Theater", "A2729", None),
    ("Lichtblick Filmtheater", "Lichtblick Filmtheater", "A0899", Some("/kino/oldenburg-in-holstein/lichtblick-filmtheater-65499"))
  ))
  private def r_westerland: R = ("westerland", "Westerland", "Schleswig-Holstein", 54.9079, 8.30326, Seq("Westerland", "Norddorf auf Amrum"), Seq(
    ("Kinowelt Westerland", "Kinowelt Westerland", "A0843", Some("/kino/westerland/kinowelt-westerland-44317")),
    ("LichtBlick InselKino", "LichtBlick InselKino", "A2807", None)
  ))
  private def r_sassnitz: R = ("sassnitz", "Sassnitz", "Mecklenburg-Vorpommern", 54.5157, 13.64451, Seq("Sassnitz", "Göhren"), Seq(
    ("Lichtspiele Sassnitz", "Lichtspiele Sassnitz", "A2711", Some("/kino/sassnitz/lichtspiele-87266")),
    ("Ostseebad Göhren Kinohalle - Regenbogencamp", "Ostseebad Göhren Kinohalle - Regenbogencamp", "A2815", None)
  ))
  private def r_berchtesgaden: R = ("berchtesgaden", "Berchtesgaden", "Bayern", 47.63236, 13.00187, Seq("Berchtesgaden", "Schönau am Königssee"), Seq(
    ("Kurkino im Kurhaus", "Kurkino im Kurhaus", "A0886", None),
    ("Open Air Kino an der Sportanlage Schneewinkl am Königssee", "Open Air Kino an der Sportanlage Schneewinkl am Königssee", "A1977", Some("/kino/schoenau-am-koenigssee/open-air-kino-an-der-sportanlage-schneewinkl-am-koenigssee-68958"))
  ))
  private def r_bernkastel_kues: R = ("bernkastel-kues", "Bernkastel-Kues", "Rheinland-Pfalz", 49.91602, 7.07664, Seq("Bernkastel-Kues", "Hoppstädten-Weiersbach"), Seq(
    ("Mosel-Kino", "Mosel-Kino", "A0180", Some("/kino/bernkastelkues/moselkino-31753")),
    ("Movietown Neubrücke", "Movietown Neubrücke", "A0973", Some("/kino/hoppstaedtenweiersbach/movietown-47597"))
  ))
  private def r_hachenburg: R = ("hachenburg", "Hachenburg", "Rheinland-Pfalz", 50.65998, 7.82276, Seq("Hachenburg", "Neitersen"), Seq(
    ("Cinexx", "Cinexx", "A0392", Some("/kino/hachenburg/cinexx-31812")),
    ("Wied Scala", "Wied Scala", "A1278", Some("/kino/neitersen-westerwald/wied-scala-31882"))
  ))
  private def r_lychen: R = ("lychen", "Lychen", "Brandenburg", 53.21089, 13.31556, Seq("Lychen", "Wesenberg"), Seq(
    ("Altes Kino", "Altes Kino", "A2634", Some("/kino/lychen/altes-kino-31599")),
    ("Kino Wesenberg", "Kino Wesenberg", "A0757", None)
  ))
  private def r_eberswalde_finow: R = ("eberswalde-finow", "Eberswalde-Finow", "Brandenburg", 52.83492, 13.81951, Seq("Eberswalde-Finow"), Seq(
    ("Movie Magic", "Movie Magic", "A2891", None)
  ))
  private def r_stendal: R = ("stendal", "Stendal", "Sachsen-Anhalt", 52.60578, 11.86091, Seq("Stendal"), Seq(
    ("Uppstall Kinos", "Uppstall Kinos", "A0664", Some("/kino/stendal/uppstall-kinos-41696"))
  ))
  private def r_schwedt: R = ("schwedt", "Schwedt", "Brandenburg", 53.05963, 14.28154, Seq("Schwedt"), Seq(
    ("FilmforUM Schwedt", "FilmforUM Schwedt", "A0333", Some("/kino/schwedt-oder/filmforum-31665"))
  ))
  private def r_itzehoe: R = ("itzehoe", "Itzehoe", "Schleswig-Holstein", 53.92099, 9.51529, Seq("Itzehoe"), Seq(
    ("CineMotion Itzehoe", "CineMotion Itzehoe", "A0125", Some("/kino/itzehoe/cinemotion-54256"))
  ))
  private def r_parchim: R = ("parchim", "Parchim", "Mecklenburg-Vorpommern", 53.42631, 11.84875, Seq("Parchim"), Seq(
    ("Movie Star Parchim", "Movie Star Parchim", "A0967", Some("/kino/parchim/movie-star-43517"))
  ))
  private def r_luebben: R = ("luebben", "Lübben", "Brandenburg", 51.93814, 13.88826, Seq("Lübben"), Seq(
    ("Spreewald Lichtspiele Lübben", "Spreewald Lichtspiele Lübben", "A1144", None)
  ))
  private def r_demmin: R = ("demmin", "Demmin", "Mecklenburg-Vorpommern", 53.90762, 13.03142, Seq("Demmin"), Seq(
    ("Filmeck Demmin", "Filmeck Demmin", "A2382", Some("/kino/demmin/filmeck-31479"))
  ))
  private def r_letschin: R = ("letschin", "Letschin", "Brandenburg", 52.64379, 14.36007, Seq("Letschin"), Seq(
    ("Haus Lichtblick", "Haus Lichtblick", "A2362", Some("/kino/letschin/lichtblick-75021"))
  ))
  private def r_sosa: R = ("sosa", "Sosa", "Sachsen", 50.49917, 12.6512, Seq("Sosa"), Seq(
    ("Freilichtbühne Sosa", "Freilichtbühne Sosa", "A2191", None)
  ))
  private def r_prerow: R = ("prerow", "Prerow", "Mecklenburg-Vorpommern", 54.44469, 12.57677, Seq("Prerow"), Seq(
    ("Cinema Ostseebad Prerow", "Cinema Ostseebad Prerow", "A0241", None)
  ))
  private def r_fischbach_bei_dahn: R = ("fischbach-bei-dahn", "Fischbach bei Dahn", "Rheinland-Pfalz", 49.08771, 7.7116, Seq("Fischbach bei Dahn"), Seq(
    ("Wasgau-Theater", "Wasgau-Theater", "A1269", None)
  ))
  private def r_helgoland: R = ("helgoland", "Helgoland", "Schleswig-Holstein", 54.18143, 7.8863, Seq("Helgoland"), Seq(
    ("Hochseekino Helgoland", "Hochseekino Helgoland", "A2652", Some("/kino/helgoland/hochseekino-84577"))
  ))
  private def r_kloeden: R = ("kloeden", "Klöden", "Sachsen-Anhalt", 51.76178, 12.83169, Seq("Klöden"), Seq(
    ("Sommerkino Klöden", "Sommerkino Klöden", "A2508", None)
  ))

  private def chunk0: Seq[R] = Seq(r_berlin, r_frankfurt_am_main, r_stuttgart, r_koeln, r_muenchen, r_hamburg, r_nuernberg, r_dortmund, r_mannheim, r_krefeld, r_bielefeld, r_chemnitz, r_leipzig, r_karlsruhe, r_saarbruecken, r_bremen, r_heilbronn, r_schwaebisch_gmuend, r_hannover, r_braunschweig, r_villingen_schwenningen, r_dresden, r_jena, r_schweinfurt, r_freiburg, r_wuppertal, r_ravensburg, r_regensburg, r_kiel, r_konstanz, r_landsberg_am_lech, r_augsburg, r_offenburg, r_tauberbischofsheim, r_kassel, r_ingolstadt, r_hechingen, r_juelich, r_butzbach, r_luebeck)
  private def chunk1: Seq[R] = Seq(r_arnsberg, r_loerrach, r_memmingen, r_noerdlingen, r_bautzen, r_rostock, r_trier, r_aschaffenburg, r_goerlitz, r_goettingen, r_rheine, r_plauen, r_crailsheim, r_bad_aibling, r_osnabrueck, r_muenster, r_erfurt, r_ulm, r_koblenz, r_bad_toelz, r_minden, r_vechta, r_schwerin, r_cottbus, r_bamberg, r_landshut, r_kaiserslautern, r_lueneburg, r_hameln, r_burghausen, r_bad_kreuznach, r_magdeburg, r_marburg, r_oldenburg, r_fulda, r_passau, r_ansbach, r_paderborn, r_traunstein, r_brandenburg)
  private def chunk2: Seq[R] = Seq(r_altenburg, r_riesa, r_sigmaringen, r_anklam, r_altensteig, r_celle, r_neubrandenburg, r_dessau_rosslau, r_emden, r_waldshut_tiengen, r_bad_urach, r_frankfurt_an_der_oder, r_amberg, r_straubing, r_suhl, r_eisenach, r_nienburg, r_korbach, r_daun, r_oberstdorf, r_flensburg, r_gummersbach, r_garmisch_partenkirchen, r_dorsten, r_stralsund, r_cuxhaven, r_nordhausen, r_coburg, r_halberstadt, r_meppen, r_neuruppin, r_bad_neuenahr_ahrweiler, r_rotenburg_an_der_wuemme, r_salzwedel, r_bad_berleburg, r_weissenburg, r_marktredwitz, r_bremerhaven, r_bayreuth, r_goslar)
  private def chunk3: Seq[R] = Seq(r_bernburg, r_bad_hersfeld, r_waren_mueritz, r_wittenberge, r_marktoberdorf, r_erbach_im_odenwald, r_regen, r_bad_schwalbach, r_niebuell, r_uffenheim, r_bad_lobenstein, r_langeoog, r_kleve, r_luckenwalde, r_prenzlau, r_husum, r_finsterwalde, r_cham, r_fehmarn, r_westerland, r_sassnitz, r_berchtesgaden, r_bernkastel_kues, r_hachenburg, r_lychen, r_eberswalde_finow, r_stendal, r_schwedt, r_itzehoe, r_parchim, r_luebben, r_demmin, r_letschin, r_sosa, r_prerow, r_fischbach_bei_dahn, r_helgoland, r_kloeden)
  val regions: Seq[R] = chunk0 ++ chunk1 ++ chunk2 ++ chunk3
}
