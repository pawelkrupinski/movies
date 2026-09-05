// GENERATED from data/germany/regions.json by the DE roster generator — do NOT edit by hand.
// Full German cinema roster: 158 regions over 16 Bundesländer /
// 1,529 cinemas (Filmstarts). Regenerate with
// the generator in data/germany/scripts after re-harvesting; see data/germany/README.md.
package models

private[models] object GermanRosterData {
  // (displayName, pillName, filmstarts theaterId)
  type C = (String, String, String)
  // (slug, name, bundesland, lat, lon, cities, cinemas)
  type R = (String, String, String, Double, Double, Seq[String], Seq[C])

  private def r_berlin: R = ("berlin", "Berlin", "Berlin", 52.52437, 13.41053, Seq("Berlin", "Potsdam", "Oranienburg", "Buckow", "Falkensee", "Bernau", "Königs Wusterhausen", "Kleinmachnow", "Erkner", "Wildau"), Seq(
    ("ARTE Freiluftkino Kulturforum", "ARTE Freiluftkino Kulturforum", "A1807"),
    ("Acud Kino", "Acud Kino", "A0006"),
    ("Adria Filmtheater Steglitz", "Adria Filmtheater Steglitz", "A0008"),
    ("Akademie der Künste", "Akademie der Künste", "A2830"),
    ("Arsenal Berlin", "Arsenal Berlin", "A0050"),
    ("Astor Film Lounge", "Astor Film Lounge", "A0514"),
    ("Astra-Filmpalast", "Astra-Filmpalast", "A0056"),
    ("B-Ware Ladenkino", "B-Ware Ladenkino", "A2620"),
    ("Babylon (Kreuzberg)", "Babylon (Kreuzberg)", "A0075"),
    ("Babylon (Mitte)", "Babylon (Mitte)", "A1489"),
    ("Bali-Kino", "Bali-Kino", "A0079"),
    ("Blauer Stern", "Blauer Stern", "A0099"),
    ("Brotfabrik im Kunst- &amp; Kulturzentrum", "Brotfabrik im Kunst- &amp; Kulturzentrum", "A0111"),
    ("Bundesplatz- Studio", "Bundesplatz- Studio", "A0114"),
    ("Capitol Dahlem", "Capitol Dahlem", "A0151"),
    ("Casablanca Berlin", "Casablanca Berlin", "A0171"),
    ("CineStar Berlin - Cubix am Alexanderplatz", "CineStar Berlin - Cubix am Alexanderplatz", "A0332"),
    ("CineStar Berlin - Hellersdorf", "CineStar Berlin - Hellersdorf", "A0373"),
    ("CineStar Berlin - Kino in der Kulturbrauerei", "CineStar Berlin - Kino in der Kulturbrauerei", "A0738"),
    ("CineStar Berlin - Tegel", "CineStar Berlin - Tegel", "A0369"),
    ("Cinema Paris", "Cinema Paris", "A0252"),
    ("CinemaxX Potsdamer Platz", "CinemaxX Potsdamer Platz", "A0275"),
    ("Cineplex Alhambra", "Cineplex Alhambra", "A0015"),
    ("Cineplex Neukölln", "Cineplex Neukölln", "A0647"),
    ("Cineplex Spandau", "Cineplex Spandau", "A0316"),
    ("Cineplex Titania", "Cineplex Titania", "A1184"),
    ("City Kino Wedding", "City Kino Wedding", "A2672"),
    ("Colosseum Filmtheater Berlin", "Colosseum Filmtheater Berlin", "A1207"),
    ("Cosima-Filmtheater", "Cosima-Filmtheater", "A0427"),
    ("Czech Centre / Tschechische Zentrum Berlin", "Czech Centre / Tschechische Zentrum Berlin", "A2834"),
    ("Delphi Filmpalast am Zoo", "Delphi Filmpalast am Zoo", "A0435"),
    ("Delphi LUX", "Delphi LUX", "A2870"),
    ("Eva-Lichtspiele", "Eva-Lichtspiele", "A0476"),
    ("Filmkunst 66", "Filmkunst 66", "A0503"),
    ("Filmrauschpalast im Kulturfabrik Moabit", "Filmrauschpalast im Kulturfabrik Moabit", "A0523"),
    ("Filmtheater am Friedrichshain", "Filmtheater am Friedrichshain", "A0479"),
    ("Freiheit fünfzehn - kino im liegestuh", "Freiheit fünfzehn - kino im liegestuh", "A2840"),
    ("Freiluftkino Friedrichshain", "Freiluftkino Friedrichshain", "A1805"),
    ("Freiluftkino Hasenheide", "Freiluftkino Hasenheide", "A1784"),
    ("Freiluftkino Kreuzberg", "Freiluftkino Kreuzberg", "A2198"),
    ("Freiluftkino Naturtheater Friedrichshagen", "Freiluftkino Naturtheater Friedrichshagen", "A1808"),
    ("Freiluftkino Neue Zukunft", "Freiluftkino Neue Zukunft", "G02A8"),
    ("Freiluftkino Parkbühne Biesdorf", "Freiluftkino Parkbühne Biesdorf", "A1809"),
    ("Freiluftkino Rehberge", "Freiluftkino Rehberge", "A2739"),
    ("Hackesche Höfe Kino", "Hackesche Höfe Kino", "A0597"),
    ("IL KINO", "IL KINO", "A2674"),
    ("Instituto Cervantes - Berlin", "Instituto Cervantes - Berlin", "A2907"),
    ("Kant Kino Charlottenburg", "Kant Kino Charlottenburg", "A0645"),
    ("Kino Central Berlin", "Kino Central Berlin", "A0185"),
    ("Kino Central Open Air (Berlin)", "Kino Central Open Air (Berlin)", "A2760"),
    ("Kino International Berlin", "Kino International Berlin", "A0625"),
    ("Kino Intimes", "Kino Intimes", "A0627"),
    ("Kino Krokodil", "Kino Krokodil", "A0745"),
    ("Kino Spreehöfe Berlin", "Kino Spreehöfe Berlin", "A0753"),
    ("Kino Zukunft", "Kino Zukunft", "A2660"),
    ("Kino im Kulturhaus Spandau", "Kino im Kulturhaus Spandau", "A1734"),
    ("Klick Kino", "Klick Kino", "A2751"),
    ("Lichtblick-Kino", "Lichtblick-Kino", "A0901"),
    ("Moviemento", "Moviemento", "A0970"),
    ("Neues Off", "Neues Off", "A0990"),
    ("Odeon", "Odeon", "A1577"),
    ("Open Air Kino Spandau Innenhof Stadtbibliothek", "Open Air Kino Spandau Innenhof Stadtbibliothek", "A2276"),
    ("Passage Kino Neukölln", "Passage Kino Neukölln", "A1035"),
    ("Regenbogen Kino", "Regenbogen Kino", "A1049"),
    ("Rollberg Kino", "Rollberg Kino", "A1083"),
    ("Sputnik Südstern", "Sputnik Südstern", "A1145"),
    ("Thalia Berlin", "Thalia Berlin", "A1177"),
    ("Tilsiter-Lichtspiele", "Tilsiter-Lichtspiele", "A1183"),
    ("Toni und Tonino", "Toni und Tonino", "A1187"),
    ("UCI East Side Gallery", "UCI East Side Gallery", "A2906"),
    ("UCI Gropius Passagen", "UCI Gropius Passagen", "A1210"),
    ("UCI am Eastgate", "UCI am Eastgate", "A0667"),
    ("Union Filmtheater Friedrichshagen", "Union Filmtheater Friedrichshagen", "A1230"),
    ("Urania-Filmbühne", "Urania-Filmbühne", "A1248"),
    ("Wolf Kino", "Wolf Kino", "A2749"),
    ("Xenon", "Xenon", "A1284"),
    ("Yorck Kino (Kreuzberg)", "Yorck Kino (Kreuzberg)", "A0995"),
    ("Z-inema im Z-Bar", "Z-inema im Z-Bar", "A2374"),
    ("ZOO PALAST Berlin", "ZOO PALAST Berlin", "A1215"),
    ("Zeughauskino - Deutsches Historisches Museum", "Zeughauskino - Deutsches Historisches Museum", "A1290"),
    ("fsk am Oranienplatz", "fsk am Oranienplatz", "A0564"),
    ("CineMotion Berlin Hohenschönhausen", "CineMotion Berlin Hohenschönhausen", "A0290"),
    ("Filmmuseum Potsdam", "Filmmuseum Potsdam", "A0506"),
    ("Freilichtkino in der Russischen Kolonie Alexandrowka", "Freilichtkino in der Russischen Kolonie Alexandrowka", "A2152"),
    ("Freiluftkino Waschhaus", "Freiluftkino Waschhaus", "A1811"),
    ("Inselkino auf der Freundschaftsinsel", "Inselkino auf der Freundschaftsinsel", "A1932"),
    ("Thalia Arthouse-Kino", "Thalia Arthouse-Kino", "A1175"),
    ("UCI Potsdam", "UCI Potsdam", "A1206"),
    ("Filmpalast Oranienburg", "Filmpalast Oranienburg", "A0518"),
    ("Park-Theater Buckow", "Park-Theater Buckow", "A1491"),
    ("ALA Kino am Falkensee", "ALA Kino am Falkensee", "A0013"),
    ("Filmpalast Bernau bei Berlin", "Filmpalast Bernau bei Berlin", "A0512"),
    ("Capitol Königs Wusterhausen", "Capitol Königs Wusterhausen", "A0144"),
    ("Kammerspiele Kleinmachnow", "Kammerspiele Kleinmachnow", "A0642"),
    ("Movieland", "Movieland", "A0671"),
    ("CineStar Wildau", "CineStar Wildau", "A0330")
  ))
  private def r_frankfurt_am_main: R = ("frankfurt-am-main", "Frankfurt am Main", "Hessen", 50.11552, 8.68417, Seq("Frankfurt am Main", "Wiesbaden", "Mainz", "Darmstadt", "Offenbach am Main", "Rodgau", "Dreieich", "Hofheim am Taunus", "Bad Vilbel", "Dietzenbach", "Mörfelden-Walldorf", "Kelkheim", "Friedrichsdorf", "Ginsheim-Gustavsburg", "Frankfurt", "Frankfurt (Oder)", "Hanau", "Langen", "Neu-Isenburg", "Kelkheim (Taunus)", "Weiterstadt", "Idstein", "Pfungstadt", "Gross-Gerau", "Bad Soden am Taunus", "Eschborn", "Karben", "Nidderau", "Kronberg im Taunus", "Eppstein", "Rödermark"), Seq(
    ("ASTOR Film Lounge MyZeil", "ASTOR Film Lounge MyZeil", "A2913"),
    ("CineStar Frankfurt am Main - Metropolis", "CineStar Frankfurt am Main - Metropolis", "A0378"),
    ("Cinema am Rossmarkt", "Cinema am Rossmarkt", "A0261"),
    ("Eldorado Frankfurt am Main", "Eldorado Frankfurt am Main", "A0460"),
    ("Filmforum Höchst", "Filmforum Höchst", "A1416"),
    ("Harmonie in Sachsenhausen", "Harmonie in Sachsenhausen", "A1450"),
    ("Kino im DFF (Filmmuseum)", "Kino im DFF (Filmmuseum)", "A0718"),
    ("Mal seh&#039;n Kino", "Mal seh&#039;n Kino", "A1469"),
    ("Open-Air-Kino Brentanobad", "Open-Air-Kino Brentanobad", "A1853"),
    ("Orfeos Erben", "Orfeos Erben", "A1019"),
    ("Pupille e.V. - Kino in der Uni", "Pupille e.V. - Kino in der Uni", "A1048"),
    ("naxos.Kino im Theater Willy Praml", "naxos.Kino im Theater Willy Praml", "A2632"),
    ("Caligari FilmBühne Wiesbaden", "Caligari FilmBühne Wiesbaden", "A0134"),
    ("Cineplex Apollo-Center Wiesbaden", "Cineplex Apollo-Center Wiesbaden", "A0019"),
    ("Cineplex Arkaden Wiesbaden", "Cineplex Arkaden Wiesbaden", "A0007"),
    ("Filme im Schloss", "Filme im Schloss", "A2320"),
    ("Murnau-Filmtheater", "Murnau-Filmtheater", "A2631"),
    ("Open Air Kino Reisinger Anlage", "Open Air Kino Reisinger Anlage", "A2424"),
    ("Capitol Arthouse Mainz", "Capitol Arthouse Mainz", "A1331"),
    ("CineStar Mainz", "CineStar Mainz", "A0352"),
    ("Cinémayence", "Cinémayence", "A0018"),
    ("KlubKino", "KlubKino", "A2853"),
    ("Open Air im Kurfürstlichen Schloss", "Open Air im Kurfürstlichen Schloss", "A2866"),
    ("Citydome Darmstadt", "Citydome Darmstadt", "A0483"),
    ("Kinopolis Darmstadt", "Kinopolis Darmstadt", "A0268"),
    ("Studentischer Filmkreis im Audimaxx der TU Darmstadt", "Studentischer Filmkreis im Audimaxx der TU Darmstadt", "A2665"),
    ("programmkino rex", "programmkino rex", "A1597"),
    ("CinemaxX Offenbach", "CinemaxX Offenbach", "A0264"),
    ("Hafenkino - Programmkino im Kulturzentrum Hafen 2", "Hafenkino - Programmkino im Kulturzentrum Hafen 2", "A2691"),
    ("Kronen-Lichtspiele Rodgau", "Kronen-Lichtspiele Rodgau", "A0256"),
    ("Saalbau-Lichtspiele Rodgau", "Saalbau-Lichtspiele Rodgau", "A1099"),
    ("Rex-Palast", "Rex-Palast", "A1074"),
    ("Viktoria-Theater", "Viktoria-Theater", "A1257"),
    ("Filmpalast Hofheim", "Filmpalast Hofheim", "A2641"),
    ("Open-Air Spielwiese neben dem Rathaus", "Open-Air Spielwiese neben dem Rathaus", "A2458"),
    ("Kino Alte Mühle", "Kino Alte Mühle", "A0700"),
    ("Open Air Kino im Bad Vilbeler Freibad", "Open Air Kino im Bad Vilbeler Freibad", "A1933"),
    ("Main Kino D", "Main Kino D", "A0658"),
    ("Maingau - Open - Air - Kino", "Maingau - Open - Air - Kino", "A2627"),
    ("LichtBlick Mörfelden-Walldorf", "LichtBlick Mörfelden-Walldorf", "A0900"),
    ("Open Air Kino am Rathaus Walldorf", "Open Air Kino am Rathaus Walldorf", "A2256"),
    ("Kino-Kelkheim", "Kino-Kelkheim", "A1510"),
    ("Open Air Kelkheim", "Open Air Kelkheim", "A2767"),
    ("Filmtheater Friedrichsdorf", "Filmtheater Friedrichsdorf", "A1427"),
    ("Open Air im Freibad Friedrichsdorf", "Open Air im Freibad Friedrichsdorf", "A2281"),
    ("Burg-Lichtspiele (Ginsheim-Gustavsburg)", "Burg-Lichtspiele (Ginsheim-Gustavsburg)", "A0868"),
    ("Open Air Kino Ginsheim-Gustavsburg", "Open Air Kino Ginsheim-Gustavsburg", "A2084"),
    ("Instituto Cervantes - Frankfurt", "Instituto Cervantes - Frankfurt", "A2908"),
    ("CineStar Frankfurt (Oder)", "CineStar Frankfurt (Oder)", "A0343"),
    ("Kinopolis Hanau", "Kinopolis Hanau", "A2613"),
    ("Lichtburg Kinos", "Lichtburg Kinos", "A1534"),
    ("Drive In Autokino Gravenbruch", "Drive In Autokino Gravenbruch", "A1398"),
    ("Open-Air Marktplatz Neue Stadtmitte", "Open-Air Marktplatz Neue Stadtmitte", "A2204"),
    ("Kommunales Kino Weiterstadt im Bürgerzentrum", "Kommunales Kino Weiterstadt im Bürgerzentrum", "A0858"),
    ("Kino Idstein", "Kino Idstein", "A0680"),
    ("Saalbau-Lichtspiele Pfungstadt", "Saalbau-Lichtspiele Pfungstadt", "A1098"),
    ("Lichtspielhaus Groß-Gerau", "Lichtspielhaus Groß-Gerau", "A0913"),
    ("CasaBlanca Art House", "CasaBlanca Art House", "A1531"),
    ("Eschborn K", "Eschborn K", "A2898"),
    ("Cinepark Karben", "Cinepark Karben", "A0303"),
    ("Luxor Filmpalast Nidderau", "Luxor Filmpalast Nidderau", "A0934"),
    ("Kronberger Lichtspiele", "Kronberger Lichtspiele", "A0872"),
    ("Open-Air auf der Burg", "Open-Air auf der Burg", "A2283"),
    ("Neue Lichtspiele", "Neue Lichtspiele", "A1571")
  ))
  private def r_stuttgart: R = ("stuttgart", "Stuttgart", "Baden-Württemberg", 48.78232, 9.17702, Seq("Stuttgart", "Ludwigsburg", "Tübingen", "Reutlingen", "Böblingen", "Kirchheim unter Teck", "Esslingen", "Esslingen am Neckar", "Fellbach", "Backnang", "Kornwestheim", "Weil der Stadt", "Erdmannhausen", "Sindelfingen", "Waiblingen", "Leonberg", "Winnenden", "Weinstadt", "Mühlacker", "Renningen", "Marbach am Neckar", "Plochingen", "Asperg", "Korb", "Waldenbuch", "Allmersbach im Tal", "Oberriexingen"), Seq(
    ("Atelier am Bollwerk", "Atelier am Bollwerk", "A0063"),
    ("Cinema Stuttgart", "Cinema Stuttgart", "A0233"),
    ("CinemaxX SI-Centrum Stuttgart", "CinemaxX SI-Centrum Stuttgart", "A0942"),
    ("Cinemaxx Liederhalle Stuttgart", "Cinemaxx Liederhalle Stuttgart", "A0297"),
    ("Corso Stuttgart", "Corso Stuttgart", "A1386"),
    ("Delphi 1+2", "Delphi 1+2", "A1389"),
    ("EM", "EM", "A1402"),
    ("Gloria Stuttgart", "Gloria Stuttgart", "A1437"),
    ("Kinothek", "Kinothek", "A0840"),
    ("Metropol Stuttgart", "Metropol Stuttgart", "A0948"),
    ("Open Air Kino am Mercedes-Benz Museum", "Open Air Kino am Mercedes-Benz Museum", "A2811"),
    ("Caligari Ludwigsburg", "Caligari Ludwigsburg", "A1324"),
    ("Central Theater Ludwigsburg", "Central Theater Ludwigsburg", "A0195"),
    ("Ludwigsburger Sommernachts Open Air Kino", "Ludwigsburger Sommernachts Open Air Kino", "A1944"),
    ("Luna Lichtspieltheater", "Luna Lichtspieltheater", "A0928"),
    ("Scala-Kino Ludwigsburg", "Scala-Kino Ludwigsburg", "A1108"),
    ("Union-Theater Ludwigsburg", "Union-Theater Ludwigsburg", "A1238"),
    ("Atelier Tübingen", "Atelier Tübingen", "A1301"),
    ("Filmtheater Blaue Brücke", "Filmtheater Blaue Brücke", "A0683"),
    ("Museum-Lichtspiele Tübingen", "Museum-Lichtspiele Tübingen", "A0691"),
    ("Sommernachtskino Tübingen", "Sommernachtskino Tübingen", "A1915"),
    ("Cineplex Planie Reutlingen", "Cineplex Planie Reutlingen", "A1038"),
    ("Kamino", "Kamino", "A2717"),
    ("Open Air Kino Spitalhof Reutlingen", "Open Air Kino Spitalhof Reutlingen", "A2518"),
    ("Filmzentrum Bären", "Filmzentrum Bären", "A0953"),
    ("Metropol am Postplatz", "Metropol am Postplatz", "A2741"),
    ("Open Air Böblinger See", "Open Air Böblinger See", "A2035"),
    ("Kommunales Kino Kirchheim unter Teck", "Kommunales Kino Kirchheim unter Teck", "A1692"),
    ("Sommernachtskino (Kirchheim unter Teck)", "Sommernachtskino (Kirchheim unter Teck)", "A2823"),
    ("Tyroler-Lichtspiele", "Tyroler-Lichtspiele", "A1197"),
    ("Autokino Esslingen Neckar Center", "Autokino Esslingen Neckar Center", "A2928"),
    ("Open-Air-Kino auf der Burg", "Open-Air-Kino auf der Burg", "A1847"),
    ("Kommunales Kino Esslingen", "Kommunales Kino Esslingen", "A1520"),
    ("Traumpalast Esslingen", "Traumpalast Esslingen", "A0076"),
    ("Fellbacher Sommernachts-Open-Air-Kino", "Fellbacher Sommernachts-Open-Air-Kino", "A1925"),
    ("Kinokult Orfeo-Kino", "Kinokult Orfeo-Kino", "A1021"),
    ("Kino Universum Backnang", "Kino Universum Backnang", "A1656"),
    ("Traumpalast Backnang", "Traumpalast Backnang", "A1430"),
    ("Capitol Lichtspiele Kornwestheim", "Capitol Lichtspiele Kornwestheim", "A0145"),
    ("Drive In Autokino Kornwestheim", "Drive In Autokino Kornwestheim", "A1921"),
    ("Kinocenter", "Kinocenter", "A2771"),
    ("Kulisse", "Kulisse", "A2806"),
    ("Kinomobil Stuttgart - Astrid-Lindgren-Schule", "Kinomobil Stuttgart - Astrid-Lindgren-Schule", "A0813"),
    ("Kinomobil Stuttgart - Schulhof Astrid-Lindgren-Schule", "Kinomobil Stuttgart - Schulhof Astrid-Lindgren-Schule", "A1952"),
    ("CinemaxX Sindelfingen", "CinemaxX Sindelfingen", "A2875"),
    ("Traumpalast Waiblingen", "Traumpalast Waiblingen", "A0480"),
    ("Traumpalast Leonberg", "Traumpalast Leonberg", "A2738"),
    ("Olympia Winnenden", "Olympia Winnenden", "A1578"),
    ("Kommunales Kino Weinstadt", "Kommunales Kino Weinstadt", "A2856"),
    ("Scala Filmtheater Mühlacker", "Scala Filmtheater Mühlacker", "A1617"),
    ("Kinomobil Stuttgart - Festhalle Stegwiesen", "Kinomobil Stuttgart - Festhalle Stegwiesen", "A0792"),
    ("Kinomobil Stuttgart - Jugend-Kultur-Haus planet x", "Kinomobil Stuttgart - Jugend-Kultur-Haus planet x", "A0784"),
    ("Union-Theater Plochingen", "Union-Theater Plochingen", "A1654"),
    ("MoKi Ludwigsburg", "MoKi Ludwigsburg", "A2859"),
    ("Kinomobil Stuttgart - Alte Kelter Korb", "Kinomobil Stuttgart - Alte Kelter Korb", "A0796"),
    ("Kinomobil Stuttgart - Haus der Begegnung Waldenbuch", "Kinomobil Stuttgart - Haus der Begegnung Waldenbuch", "A2171"),
    ("Kinomobil Stuttgart - Bürgersaal", "Kinomobil Stuttgart - Bürgersaal", "A2557"),
    ("Kinomobil Stuttgart - Festhalle Oberriexingen", "Kinomobil Stuttgart - Festhalle Oberriexingen", "A1938")
  ))
  private def r_koeln: R = ("koeln", "Köln", "Nordrhein-Westfalen", 50.93333, 6.95, Seq("Köln", "Düsseldorf", "Bonn", "Leverkusen", "Hürth", "Kerpen", "Langenfeld", "Siegburg", "Bergisch Gladbach", "Troisdorf", "Sankt Augustin", "Euskirchen", "Frechen", "Hennef", "Brühl", "Monheim am Rhein", "Kalk"), Seq(
    ("Cinedom", "Cinedom", "A0027"),
    ("Cinenova", "Cinenova", "A0298"),
    ("Cineplex Filmpalast Köln", "Cineplex Filmpalast Köln", "A0031"),
    ("Filmclub 813", "Filmclub 813", "A0737"),
    ("Filmforum im Museum Ludwig", "Filmforum im Museum Ludwig", "A2139"),
    ("Filmhaus Kino Köln", "Filmhaus Kino Köln", "A0499"),
    ("Filmpalette", "Filmpalette", "A0522"),
    ("Japanisches Kulturinstitut", "Japanisches Kulturinstitut", "A0629"),
    ("Kino Weißhaus", "Kino Weißhaus", "A0756"),
    ("Metropolis Köln", "Metropolis Köln", "A0959"),
    ("Odeon Köln", "Odeon Köln", "A1004"),
    ("Odonisches Sommerkino", "Odonisches Sommerkino", "A2175"),
    ("Off Broadway", "Off Broadway", "A1009"),
    ("Open Air Kino Im Rheinauenhafen", "Open Air Kino Im Rheinauenhafen", "A2436"),
    ("Residenz - Astor Filmlounge", "Residenz - Astor Filmlounge", "A2616"),
    ("Rex am Ring", "Rex am Ring", "A1704"),
    ("Turistarama", "Turistarama", "A2687"),
    ("Atelier Kino im Savoy-Theater", "Atelier Kino im Savoy-Theater", "A0064"),
    ("Bambi", "Bambi", "A0082"),
    ("Black Box im Filmmuseum", "Black Box im Filmmuseum", "A0098"),
    ("Cinema Düsseldorf", "Cinema Düsseldorf", "A0231"),
    ("Cinestar Düsseldorf", "Cinestar Düsseldorf", "A0379"),
    ("Frankenheim Kino", "Frankenheim Kino", "A1884"),
    ("Kino Süd", "Kino Süd", "A0755"),
    ("Metropol Düsseldorf", "Metropol Düsseldorf", "A0950"),
    ("Open-Air-Kino Vier Linden", "Open-Air-Kino Vier Linden", "A1764"),
    ("UCI Düsseldorf", "UCI Düsseldorf", "A1200"),
    ("UFA Palast Düsseldorf", "UFA Palast Düsseldorf", "A1221"),
    ("Arkadenhof der Bonner Universität - Am Hof", "Arkadenhof der Bonner Universität - Am Hof", "A2067"),
    ("Bundeskunsthalle", "Bundeskunsthalle", "A2638"),
    ("CineStar Bonn - Sternlichtspiele", "CineStar Bonn - Sternlichtspiele", "A0417"),
    ("Kino in der Brotfabrik", "Kino in der Brotfabrik", "A0736"),
    ("Neue Filmbühne Bonn", "Neue Filmbühne Bonn", "A0982"),
    ("Rex-Lichtspieltheater", "Rex-Lichtspieltheater", "A1073"),
    ("Woki", "Woki", "A1282"),
    ("Cineplex Kinopolis Leverkusen", "Cineplex Kinopolis Leverkusen", "A0826"),
    ("Kommunales Kino Leverkusen", "Kommunales Kino Leverkusen", "A0859"),
    ("Scala Leverkusen", "Scala Leverkusen", "A2358"),
    ("CP Lichtspielfreunde Hürth von 2016 e.V.", "CP Lichtspielfreunde Hürth von 2016 e.V.", "A2754"),
    ("Open Air Kino Kloster Burbach", "Open Air Kino Kloster Burbach", "A2488"),
    ("UCI Hürth Park", "UCI Hürth Park", "A1198"),
    ("Capitol-Theater Kerpen", "Capitol-Theater Kerpen", "A1340"),
    ("Euromax - Cinemas", "Euromax - Cinemas", "A0471"),
    ("Programmkino im Schaustall", "Programmkino im Schaustall", "A1043"),
    ("Rex Langenfeld", "Rex Langenfeld", "A1067"),
    ("Capitol Siegburg", "Capitol Siegburg", "A0147"),
    ("Cineplex Siegburg", "Cineplex Siegburg", "A0228"),
    ("Cineplex Bensberg", "Cineplex Bensberg", "A2715"),
    ("Cineplex Troisdorf", "Cineplex Troisdorf", "A1471"),
    ("Studio", "Studio", "A1161"),
    ("Cineplex Euskirchen", "Cineplex Euskirchen", "A2792"),
    ("Linden Theater Frechen", "Linden Theater Frechen", "A0918"),
    ("Kur-Theater Hennef", "Kur-Theater Hennef", "A1480"),
    ("Zoom-Kino", "Zoom-Kino", "A1291"),
    ("Emotion Kino", "Emotion Kino", "G02P3"),
    ("Lichtspiele Kalk", "Lichtspiele Kalk", "A2915")
  ))
  private def r_muenchen: R = ("muenchen", "München", "Bayern", 48.13743, 11.57549, Seq("München", "Fürstenfeldbruck", "Ottobrunn", "Germering", "Dachau", "Erding", "Unterschleissheim", "Starnberg", "Gauting", "Gröbenzell", "Neufahrn", "Taufkirchen (Vils)", "Haar", "Gilching", "Gräfelfing", "Aschheim", "Weßling"), Seq(
    ("ABC-Kino", "ABC-Kino", "A1476"),
    ("Astor Cinema Lounge", "Astor Cinema Lounge", "A2636"),
    ("Astor Film Lounge im Arri", "Astor Film Lounge im Arri", "A1298"),
    ("Cadillac Filmtheater", "Cadillac Filmtheater", "A0131"),
    ("Cincinnati", "Cincinnati", "A0216"),
    ("Cinema München", "Cinema München", "A2186"),
    ("CinemaxX München", "CinemaxX München", "A0943"),
    ("City-Kinos - München", "City-Kinos - München", "A0397"),
    ("Filmmuseum München", "Filmmuseum München", "A1419"),
    ("Forum 2 - Kulturverein Olympiadorf e.V.", "Forum 2 - Kulturverein Olympiadorf e.V.", "A0654"),
    ("Gabriel Filmtheater", "Gabriel Filmtheater", "A0988"),
    ("Gloria Palast", "Gloria Palast", "A1443"),
    ("Instituto Cervantes - München", "Instituto Cervantes - München", "A2832"),
    ("Istituto Italiano di Cultura - München", "Istituto Italiano di Cultura - München", "A2847"),
    ("Kino Solln", "Kino Solln", "A1499"),
    ("Kino in der Hochschule für Fernsehen und Film München", "Kino in der Hochschule für Fernsehen und Film München", "A2743"),
    ("Leopold", "Leopold", "A0895"),
    ("Mathäser Filmpalast", "Mathäser Filmpalast", "A0025"),
    ("Monopol", "Monopol", "A0963"),
    ("Museum-Lichtspiele München", "Museum-Lichtspiele München", "A1570"),
    ("Neues Arena", "Neues Arena", "A1573"),
    ("Neues Maxim Kino München", "Neues Maxim Kino München", "A0941"),
    ("Neues Rex - Filmtheater", "Neues Rex - Filmtheater", "A0991"),
    ("Neues Rottmann", "Neues Rottmann", "A1575"),
    ("Open Air Kino am Olympiasee", "Open Air Kino am Olympiasee", "A2425"),
    ("Open Air Kino, Mond &amp; Sterne", "Open Air Kino, Mond &amp; Sterne", "A1815"),
    ("Programm-Kino im Viehhof-Zelt", "Programm-Kino im Viehhof-Zelt", "A2721"),
    ("Rio Filmpalast", "Rio Filmpalast", "A1603"),
    ("Royal Filmpalast", "Royal Filmpalast", "A1610"),
    ("Studio Isabella", "Studio Isabella", "A1642"),
    ("Technische Universität", "Technische Universität", "A2600"),
    ("Theatiner Film", "Theatiner Film", "A1179"),
    ("Werkstattkino e.V.", "Werkstattkino e.V.", "A1276"),
    ("Kino-Open-Air", "Kino-Open-Air", "A2210"),
    ("Lichtspielhaus Fürstenfeldbruck", "Lichtspielhaus Fürstenfeldbruck", "A1539"),
    ("Scala Kino Fürstenfeldbruck", "Scala Kino Fürstenfeldbruck", "A1110"),
    ("Filmstudio Ottobrunn", "Filmstudio Ottobrunn", "A0528"),
    ("Ottobrunner Kinos", "Ottobrunner Kinos", "A0969"),
    ("Cineplex Germering", "Cineplex Germering", "A2705"),
    ("Cinema Dachau", "Cinema Dachau", "A1357"),
    ("Cineplex Erding", "Cineplex Erding", "A0897"),
    ("Capitol Unterschleißheim", "Capitol Unterschleißheim", "A1328"),
    ("Kino Breitwand Starnberg", "Kino Breitwand Starnberg", "A0712"),
    ("Kino Breitwand Gauting", "Kino Breitwand Gauting", "A1412"),
    ("Gröben-Lichtspiele", "Gröben-Lichtspiele", "A1445"),
    ("Cineplex Neufahrn", "Cineplex Neufahrn", "A2112"),
    ("Kinocafe", "Kinocafe", "A0393"),
    ("Haarer Kinos", "Haarer Kinos", "A1669"),
    ("Filmstation", "Filmstation", "A0526"),
    ("Filmeck im Bürgerhaus", "Filmeck im Bürgerhaus", "A1415"),
    ("Drive In Autokino Aschheim", "Drive In Autokino Aschheim", "A0452"),
    ("Kino Pfarrstadel", "Kino Pfarrstadel", "A2659")
  ))
  private def r_hamburg: R = ("hamburg", "Hamburg", "Hamburg", 53.55073, 9.99302, Seq("Hamburg", "Buchholz in der Nordheide", "Quickborn", "Altona", "Norderstedt", "Elmshorn", "Stade", "Buxtehude", "Wedel", "Neu Wulmstorf", "Uetersen", "Bargteheide", "Harsefeld"), Seq(
    ("3001 Kino", "3001 Kino", "A0002"),
    ("Abaton", "Abaton", "A0003"),
    ("Alabama-Kino", "Alabama-Kino", "A0012"),
    ("Astor Film Lounge HafenCity Hamburg", "Astor Film Lounge HafenCity Hamburg", "A2900"),
    ("B-Movie", "B-Movie", "A1907"),
    ("Blankeneser Kino", "Blankeneser Kino", "A0677"),
    ("CinemaxX Dammtor", "CinemaxX Dammtor", "A1467"),
    ("CinemaxX Harburg", "CinemaxX Harburg", "A0287"),
    ("CinemaxX Wandsbek", "CinemaxX Wandsbek", "A0282"),
    ("Elbe-Kino", "Elbe-Kino", "A1400"),
    ("FilmRaum", "FilmRaum", "A2730"),
    ("Hansa Studio Bergedorf", "Hansa Studio Bergedorf", "A1449"),
    ("Holi", "Holi", "A0614"),
    ("Instituto Cervantes - Hamburg", "Instituto Cervantes - Hamburg", "A2846"),
    ("Kinopolis HafenCity", "Kinopolis HafenCity", "G02MI"),
    ("Koralle", "Koralle", "A0869"),
    ("LOTTO Hamburg SchanzenKino Open Air", "LOTTO Hamburg SchanzenKino Open Air", "A2296"),
    ("Lichtmess", "Lichtmess", "A2604"),
    ("Magazin", "Magazin", "A1553"),
    ("Metropolis Kino", "Metropolis Kino", "A2712"),
    ("Outdoor Cine - Das Open Air Kino im Völkerkunde-Museum", "Outdoor Cine - Das Open Air Kino im Völkerkunde-Museum", "A1913"),
    ("Passage Hamburg", "Passage Hamburg", "A1034"),
    ("Savoy (Hamburg)", "Savoy (Hamburg)", "A0958"),
    ("SchanzenKino 73 (zweisprachig)", "SchanzenKino 73 (zweisprachig)", "A2747"),
    ("Sommerkino auf dem Alsterdorfer Markt", "Sommerkino auf dem Alsterdorfer Markt", "A2089"),
    ("UCI Mundsburg", "UCI Mundsburg", "A1203"),
    ("UCI Wandsbek", "UCI Wandsbek", "A1214"),
    ("Zeise Kinos", "Zeise Kinos", "A1464"),
    ("Movieplexx Autokino", "Movieplexx Autokino", "A2945"),
    ("Movieplexx Delhi - Center", "Movieplexx Delhi - Center", "A0971"),
    ("Beluga Kino", "Beluga Kino", "A0088"),
    ("Openair Kino", "Openair Kino", "A2182"),
    ("LOTTO Hamburg Auto- und Open Air Kino", "LOTTO Hamburg Auto- und Open Air Kino", "A2949"),
    ("Spectrum Kino Norderstedt", "Spectrum Kino Norderstedt", "A1142"),
    ("Cineplex Elmshorn", "Cineplex Elmshorn", "A0317"),
    ("CineStar Stade", "CineStar Stade", "A0329"),
    ("City Kino Buxtehude", "City Kino Buxtehude", "A1381"),
    ("Open Air auf dem Theaterschiff", "Open Air auf dem Theaterschiff", "A2723"),
    ("Das Kino - Neu Wulmstorf", "Das Kino - Neu Wulmstorf", "A1334"),
    ("Burg-Theater Uetersen", "Burg-Theater Uetersen", "A1322"),
    ("Kleines Theater", "Kleines Theater", "A0850"),
    ("Harsefelder Lichtspiele", "Harsefelder Lichtspiele", "A0602")
  ))
  private def r_nuernberg: R = ("nuernberg", "Nürnberg", "Bayern", 49.45421, 11.07752, Seq("Nürnberg", "Erlangen", "Fürth", "Neumarkt", "Neumarkt in der Oberpfalz", "Schwabach", "Eckental", "Grafenberg"), Seq(
    ("Admiral Filmpalast Nürnberg", "Admiral Filmpalast Nürnberg", "A0661"),
    ("Casablanca Nürnberg", "Casablanca Nürnberg", "A0172"),
    ("Cinecitta", "Cinecitta", "A0049"),
    ("Cinecittá Open Air", "Cinecittá Open Air", "A2779"),
    ("Filmfabrik KommKino", "Filmfabrik KommKino", "A0854"),
    ("Filmhauskino im Künstlerhaus", "Filmhauskino im Künstlerhaus", "A1418"),
    ("Kino in den Felsengängen", "Kino in den Felsengängen", "A1980"),
    ("Meisengeige", "Meisengeige", "A1557"),
    ("Metropolis Nürnberg", "Metropolis Nürnberg", "A0956"),
    ("Open Air Fränkisches Museumseisenbahn", "Open Air Fränkisches Museumseisenbahn", "A2489"),
    ("Open Air Kino Freilichtbühne Desi", "Open Air Kino Freilichtbühne Desi", "A1981"),
    ("Open Air Kino Katharinenruine", "Open Air Kino Katharinenruine", "A2179"),
    ("Open Air Kino Naturgarten Bad", "Open Air Kino Naturgarten Bad", "A2498"),
    ("Open Air Kino Pellerhaus", "Open Air Kino Pellerhaus", "A2180"),
    ("Open Air Krafftscher Hof", "Open Air Krafftscher Hof", "A2500"),
    ("Open Air Radrennbahn am Reichelsdorfer Keller", "Open Air Radrennbahn am Reichelsdorfer Keller", "A1956"),
    ("Rio Palast", "Rio Palast", "A1604"),
    ("Roxy-Fremdsprachenkino", "Roxy-Fremdsprachenkino", "A1092"),
    ("SommerNachtFilmFestival - Marienbergpark", "SommerNachtFilmFestival - Marienbergpark", "A2822"),
    ("CineStar Erlangen", "CineStar Erlangen", "A0355"),
    ("Kino im E-Werk", "Kino im E-Werk", "A0720"),
    ("Lamm-Lichtspiele", "Lamm-Lichtspiele", "A1532"),
    ("Manhattan Deluxe – Premiumkino", "Manhattan Deluxe – Premiumkino", "A1554"),
    ("Open Air Erlangen", "Open Air Erlangen", "A1943"),
    ("Open Air an der Bleiche", "Open Air an der Bleiche", "A2864"),
    ("Babylon Kino am Stadtpark", "Babylon Kino am Stadtpark", "A0657"),
    ("Cineplex Fürth", "Cineplex Fürth", "A2727"),
    ("Freilichtbühne im Fürther Stadtpark", "Freilichtbühne im Fürther Stadtpark", "A1963"),
    ("Open Air Kino Mauerflimmern", "Open Air Kino Mauerflimmern", "A2780"),
    ("Uferpalast Führt", "Uferpalast Führt", "A1223"),
    ("Rialto Palast", "Rialto Palast", "A1077"),
    ("Cineplex Neumarkt", "Cineplex Neumarkt", "A2716"),
    ("Luna Theater", "Luna Theater", "A0929"),
    ("Casino Lichtspiele Eckental", "Casino Lichtspiele Eckental", "A1343"),
    ("Kinomobil Stuttgart - Historische Kelter", "Kinomobil Stuttgart - Historische Kelter", "A2134")
  ))
  private def r_dortmund: R = ("dortmund", "Dortmund", "Nordrhein-Westfalen", 51.51494, 7.466, Seq("Dortmund", "Essen", "Gelsenkirchen", "Hagen", "Herne", "Recklinghausen", "Iserlohn", "Witten", "Marl", "Lünen", "Unna", "Werne", "Wetter", "Herdecke", "Altena"), Seq(
    ("CineStar Dortmund", "CineStar Dortmund", "A0029"),
    ("Filmbühne Zur Postkutsche", "Filmbühne Zur Postkutsche", "A0490"),
    ("Kino im U", "Kino im U", "A2615"),
    ("Open Air Kino im Stadion", "Open Air Kino im Stadion", "A1898"),
    ("PSD Bank Autokino Dortmund", "PSD Bank Autokino Dortmund", "A2947"),
    ("Roxy Dortmund", "Roxy Dortmund", "A1088"),
    ("Schauburg Dortmund", "Schauburg Dortmund", "A1122"),
    ("sweetSixteen-Kino", "sweetSixteen-Kino", "A2590"),
    ("Astra-Theater &amp; Luna", "Astra-Theater &amp; Luna", "A0054"),
    ("CinemaxX Essen", "CinemaxX Essen", "A0273"),
    ("Drive In Autokino Essen", "Drive In Autokino Essen", "A0451"),
    ("Eulenspiegel Filmtheater", "Eulenspiegel Filmtheater", "A0470"),
    ("Filmstudio Glückauf", "Filmstudio Glückauf", "A0531"),
    ("Galerie Cinema", "Galerie Cinema", "A0567"),
    ("Lichtburg und Sabu", "Lichtburg und Sabu", "A0902"),
    ("Multiplex Gelsenkirchen", "Multiplex Gelsenkirchen", "A1260"),
    ("Schauburg Filmpalast", "Schauburg Filmpalast", "A1123"),
    ("Babylon Hagen", "Babylon Hagen", "A0705"),
    ("CineStar Hagen", "CineStar Hagen", "A0341"),
    ("Filmwelt Herne", "Filmwelt Herne", "A0547"),
    ("Open-Air Kino Schloss Strünkede", "Open-Air Kino Schloss Strünkede", "A2008"),
    ("Cineworld", "Cineworld", "A0388"),
    ("Filmpalast Iserlohn", "Filmpalast Iserlohn", "A2796"),
    ("Die Burg Witten", "Die Burg Witten", "A2888"),
    ("Loe Studios Marl", "Loe Studios Marl", "A2910"),
    ("Cineworld Lünen", "Cineworld Lünen", "A0391"),
    ("Kinorama Unna", "Kinorama Unna", "A0491"),
    ("Capitol-Cinema-Center", "Capitol-Cinema-Center", "A0155"),
    ("Kulturzentrum Lichtburg", "Kulturzentrum Lichtburg", "A0879"),
    ("Onikon", "Onikon", "A1014"),
    ("Apollo Service Kino", "Apollo Service Kino", "A1297")
  ))
  private def r_mannheim: R = ("mannheim", "Mannheim", "Baden-Württemberg", 49.4891, 8.46694, Seq("Mannheim", "Heidelberg", "Neustadt an der Weinstraße", "Speyer", "Frankenthal", "Grünstadt", "Worms", "Neustadt an der Weinstrasse", "Weinheim", "Bensheim", "Viernheim", "Heppenheim", "Schwetzingen", "Schifferstadt", "Seeheim-Jugenheim", "Walldorf", "Ketsch", "Hemsbach", "Limburgerhof", "Biblis"), Seq(
    ("Atlantis Mannheim", "Atlantis Mannheim", "A1305"),
    ("Cinema Quadrat e.V.", "Cinema Quadrat e.V.", "A1368"),
    ("Cineplex Mannheim", "Cineplex Mannheim", "A0270"),
    ("Odeon Mannheim", "Odeon Mannheim", "A1002"),
    ("Planken Lichtspiele Mannheim", "Planken Lichtspiele Mannheim", "A1376"),
    ("Gloria &amp; Gloriette", "Gloria &amp; Gloriette", "A0578"),
    ("Kamera Heidelberg", "Kamera Heidelberg", "A0635"),
    ("Karlstorkino", "Karlstorkino", "A0648"),
    ("Luxor Filmpalast Heidelberg", "Luxor Filmpalast Heidelberg", "A2874"),
    ("Cineplex Neustadt", "Cineplex Neustadt", "A2752"),
    ("Open Air Kino in der Hetzel-Galerie", "Open Air Kino in der Hetzel-Galerie", "A2768"),
    ("IMAX Speyer", "IMAX Speyer", "A0624"),
    ("Kinocenter Theaterhaus Speyer", "Kinocenter Theaterhaus Speyer", "A1647"),
    ("Lux im Dathenushaus", "Lux im Dathenushaus", "A0930"),
    ("OPEN AIR KINO in der Erkenbert Ruine", "OPEN AIR KINO in der Erkenbert Ruine", "A1990"),
    ("Europa-Theater Grünstadt", "Europa-Theater Grünstadt", "A1404"),
    ("Filmwelt Grünstadt", "Filmwelt Grünstadt", "A2700"),
    ("Arkaden Lichtspiele", "Arkaden Lichtspiele", "A0891"),
    ("Roxy Kinos", "Roxy Kinos", "A1085"),
    ("Modernes Theater", "Modernes Theater", "A1567"),
    ("Luxor Filmpalast Bensheim", "Luxor Filmpalast Bensheim", "A1326"),
    ("Kinopolis Rhein-Neckar", "Kinopolis Rhein-Neckar", "A0833"),
    ("Saalbau-Filmtheater", "Saalbau-Filmtheater", "A1097"),
    ("Luxor Filmpalast Schwetzingen", "Luxor Filmpalast Schwetzingen", "A1551"),
    ("Rex Kino-Center Schifferstadt", "Rex Kino-Center Schifferstadt", "A1072"),
    ("Filmseher Open Air", "Filmseher Open Air", "A2797"),
    ("Luxor Filmpalast Walldorf", "Luxor Filmpalast Walldorf", "A1763"),
    ("Central Filmtheater Ketsch", "Central Filmtheater Ketsch", "A1345"),
    ("Brennessel-Programmkino", "Brennessel-Programmkino", "A0106"),
    ("Capitol LichtspielTheater Limburgerhof", "Capitol LichtspielTheater Limburgerhof", "A1336"),
    ("Die Filminsel", "Die Filminsel", "A1779")
  ))
  private def r_krefeld: R = ("krefeld", "Krefeld", "Nordrhein-Westfalen", 51.33645, 6.55381, Seq("Krefeld", "Oberhausen", "Duisburg", "Mülheim an der Ruhr", "Mönchengladbach", "Neuss", "Moers", "Bottrop", "Ratingen", "Meerbusch - Lank", "Nettetal", "Kaarst", "Kamp-Lintfort", "Kempen", "Geldern"), Seq(
    ("CinemaxX Krefeld", "CinemaxX Krefeld", "A0278"),
    ("Fabrik Heeder", "Fabrik Heeder", "A0594"),
    ("Open Air Kino Krefelder Rennbahn", "Open Air Kino Krefelder Rennbahn", "A2812"),
    ("Primus-Palast", "Primus-Palast", "A0242"),
    ("CineStar Oberhausen - Filmpalast im Centro", "CineStar Oberhausen - Filmpalast im Centro", "A1261"),
    ("Kino im Druckluft", "Kino im Druckluft", "A0719"),
    ("Lichtburg-Filmpalast", "Lichtburg-Filmpalast", "A0905"),
    ("Walzenlager-Zentrum Altenberg", "Walzenlager-Zentrum Altenberg", "A0733"),
    ("Filmforum", "Filmforum", "A2750"),
    ("Stadtwerke Sommerkino", "Stadtwerke Sommerkino", "A1862"),
    ("UCI Duisburg", "UCI Duisburg", "A0663"),
    ("CinemaxX Mülheim", "CinemaxX Mülheim", "A0277"),
    ("Ringlokschuppen Open-Air-Kino auf der Drehscheibe", "Ringlokschuppen Open-Air-Kino auf der Drehscheibe", "A1870"),
    ("Rio im Medienhaus", "Rio im Medienhaus", "A1082"),
    ("Cinefactory im Haus Zoar", "Cinefactory im Haus Zoar", "A2651"),
    ("Comet-Cine-Center", "Comet-Cine-Center", "A0419"),
    ("Hitch", "Hitch", "A0607"),
    ("UCI Neuss", "UCI Neuss", "A1201"),
    ("Atlantic Kinocenter", "Atlantic Kinocenter", "A2328"),
    ("Movie Center Moers", "Movie Center Moers", "A2789"),
    ("Filmforum der VHS", "Filmforum der VHS", "A1475"),
    ("Studiokino Ratingen", "Studiokino Ratingen", "A2804"),
    ("Kino am Forum Wasserturm", "Kino am Forum Wasserturm", "A2883"),
    ("Corso Film Casino", "Corso Film Casino", "A0423"),
    ("Kino Kaarst", "Kino Kaarst", "A0742"),
    ("Hall of Fame Kamp-Lintfort", "Hall of Fame Kamp-Lintfort", "A2899"),
    ("Kempener Lichtspiele", "Kempener Lichtspiele", "A0650"),
    ("Herzog-Theater", "Herzog-Theater", "A0687")
  ))
  private def r_bielefeld: R = ("bielefeld", "Bielefeld", "Nordrhein-Westfalen", 52.03333, 8.53333, Seq("Bielefeld", "Gütersloh", "Bünde", "Detmold", "Herford", "Bad Salzuflen", "Lemgo", "Lage", "Schloss Holte-Stukenbrock", "Versmold", "Kirchlengern", "Spenge", "Rödinghausen"), Seq(
    ("AJZ Kino", "AJZ Kino", "A0010"),
    ("CinemaxX Bielefeld", "CinemaxX Bielefeld", "A0271"),
    ("Kamera Bielefeld", "Kamera Bielefeld", "A0634"),
    ("Lichtwerk im Ravensberger Park", "Lichtwerk im Ravensberger Park", "A0915"),
    ("Melodie Filmtheater", "Melodie Filmtheater", "A2363"),
    ("Offkino Im Filmhaus Bielefeld", "Offkino Im Filmhaus Bielefeld", "A2838"),
    ("Open Air Kino Luna im Ravensberger Park", "Open Air Kino Luna im Ravensberger Park", "A2278"),
    ("Bambi &amp; Löwenherz", "Bambi &amp; Löwenherz", "A0081"),
    ("Filmwerk Gutersloh", "Filmwerk Gutersloh", "G01IT"),
    ("Open Air Kino auf dem Dreiecksplatz", "Open Air Kino auf dem Dreiecksplatz", "A2434"),
    ("Autokino Bünder Lichtspiele am Festplatz", "Autokino Bünder Lichtspiele am Festplatz", "A2923"),
    ("Bünder Lichtspiele", "Bünder Lichtspiele", "A0113"),
    ("Universum Bünde", "Universum Bünde", "A1242"),
    ("Kaiserhof+ Einfach Gutes Kino", "Kaiserhof+ Einfach Gutes Kino", "A0632"),
    ("Open-Air Mondscheinkino Waldbühne", "Open-Air Mondscheinkino Waldbühne", "A2010"),
    ("Capitol Herford", "Capitol Herford", "A0148"),
    ("Filmbühne Kinocenter", "Filmbühne Kinocenter", "A0488"),
    ("Hansa-Kino", "Hansa-Kino", "A0599"),
    ("Filmwelt Lippe", "Filmwelt Lippe", "A2647"),
    ("Rhythmus-Filmtheater", "Rhythmus-Filmtheater", "A2365"),
    ("Kulturbühne Versmold", "Kulturbühne Versmold", "G02OV"),
    ("LichtBlick Kirchlengern", "LichtBlick Kirchlengern", "A0898"),
    ("Zentral-Theater Spenge", "Zentral-Theater Spenge", "A1288"),
    ("Else-Lichtspiele", "Else-Lichtspiele", "A0463")
  ))
  private def r_chemnitz: R = ("chemnitz", "Chemnitz", "Sachsen", 50.8357, 12.92922, Seq("Chemnitz", "Zwickau", "Freiberg", "Limbach-Oberfrohna", "Annaberg-Buchholz", "Aue", "Schneeberg", "Marienberg", "Mittweida", "Hohenstein-Ernstthal", "Schwarzenberg/Erzgebirge", "Lichtenstein", "Gelenau", "Geyer"), Seq(
    ("Campusfilmnächte auf dem Sportplatz", "Campusfilmnächte auf dem Sportplatz", "A2831"),
    ("CineStar Chemnitz - am Roten Turm", "CineStar Chemnitz - am Roten Turm", "A0365"),
    ("Clubkino Siegmar", "Clubkino Siegmar", "A0415"),
    ("Filmclub Mittendrin", "Filmclub Mittendrin", "A2356"),
    ("Filmnächte auf dem Theaterplatz", "Filmnächte auf dem Theaterplatz", "A2645"),
    ("M54 / AJZ", "M54 / AJZ", "A2858"),
    ("Metropol Chemnitz", "Metropol Chemnitz", "A0951"),
    ("Weltecho", "Weltecho", "A2756"),
    ("Casablanca Zwickau", "Casablanca Zwickau", "A2378"),
    ("Filmpalast Astoria Zwickau", "Filmpalast Astoria Zwickau", "A0368"),
    ("Kinopolis Freiberg", "Kinopolis Freiberg", "A0825"),
    ("Open Air auf Schloss Freudenstein", "Open Air auf Schloss Freudenstein", "A2567"),
    ("Filmtheater &quot;Apollo&quot;", "Filmtheater &quot;Apollo&quot;", "A0540"),
    ("Gloria Filmpalast Annaberg-Buchholz", "Gloria Filmpalast Annaberg-Buchholz", "A0580"),
    ("Kino-Center Nickel-Odeon", "Kino-Center Nickel-Odeon", "A0996"),
    ("Union Filmtheater Schneeberg", "Union Filmtheater Schneeberg", "A1228"),
    ("Filmtheater Movie", "Filmtheater Movie", "A0544"),
    ("Filmbühne Mittweida", "Filmbühne Mittweida", "A0487"),
    ("Capitol Hohenstein-Ernstthal", "Capitol Hohenstein-Ernstthal", "A0153"),
    ("Olympia Schwarzenberg", "Olympia Schwarzenberg", "A1011"),
    ("Clubkino &quot;Capitol&quot;", "Clubkino &quot;Capitol&quot;", "A0414"),
    ("Clubkino", "Clubkino", "A0412"),
    ("Autokino Greifensteine", "Autokino Greifensteine", "A2184")
  ))
  private def r_leipzig: R = ("leipzig", "Leipzig", "Sachsen", 51.33962, 12.37129, Seq("Leipzig", "Halle", "Merseburg", "Grimma", "Taucha", "Groitzsch", "Leuna"), Seq(
    ("CineStar Leipzig", "CineStar Leipzig", "A0345"),
    ("Cineding", "Cineding", "A0225"),
    ("Cinémathèque in der naTo Leipzig e.V.", "Cinémathèque in der naTo Leipzig e.V.", "A0445"),
    ("Filmclub KassaBlanka", "Filmclub KassaBlanka", "A2361"),
    ("Freilichtkino auf der Pferderennbahn", "Freilichtkino auf der Pferderennbahn", "A0561"),
    ("Kinobar &quot;Prager Frühling&quot;", "Kinobar &quot;Prager Frühling&quot;", "A1500"),
    ("Luru Kino in der Spinnerei", "Luru Kino in der Spinnerei", "A2591"),
    ("Passage Kinos Leipzig", "Passage Kinos Leipzig", "A1037"),
    ("Regina-Palast", "Regina-Palast", "A1056"),
    ("Schauburg Leipzig", "Schauburg Leipzig", "A2586"),
    ("Schaubühne Lindenfels", "Schaubühne Lindenfels", "A1116"),
    ("Sommerkino Schauportal", "Sommerkino Schauportal", "A1858"),
    ("PUSCHKINO", "PUSCHKINO", "A0932"),
    ("CinemaxX Halle-Charlottencenter", "CinemaxX Halle-Charlottencenter", "A0280"),
    ("Luchs Kino am Zoo", "Luchs Kino am Zoo", "A0931"),
    ("Prisma Cinema", "Prisma Cinema", "A0295"),
    ("Zazie", "Zazie", "A1286"),
    ("Domstadt Kino", "Domstadt Kino", "A0450"),
    ("Central Theater Grimma", "Central Theater Grimma", "A0194"),
    ("CT-Lichtspiele", "CT-Lichtspiele", "A0429"),
    ("Bürgerhaus Kino", "Bürgerhaus Kino", "A1320"),
    ("UCI Nova Eventis", "UCI Nova Eventis", "A1212")
  ))
  private def r_karlsruhe: R = ("karlsruhe", "Karlsruhe", "Baden-Württemberg", 49.00937, 8.40444, Seq("Karlsruhe", "Pforzheim", "Landau in der Pfalz", "Baden-Baden", "Rastatt", "Bruchsal", "Landau", "Ettlingen", "Bretten", "Gaggenau", "Gernsbach"), Seq(
    ("Die Kurbel Karlsruhe", "Die Kurbel Karlsruhe", "A2614"),
    ("Filmpalast am ZKM - Karlsruhe", "Filmpalast am ZKM - Karlsruhe", "A0513"),
    ("Kinemathek Karlsruhe", "Kinemathek Karlsruhe", "A1711"),
    ("Open Air Kino am Schloß Gottesaue", "Open Air Kino am Schloß Gottesaue", "A1976"),
    ("Schauburg Karlsruhe", "Schauburg Karlsruhe", "A1626"),
    ("Universum-City Karlsruhe", "Universum-City Karlsruhe", "A1246"),
    ("Cinemoon Pforzheim", "Cinemoon Pforzheim", "A0309"),
    ("Kommunales Kino Pforzheim", "Kommunales Kino Pforzheim", "A0857"),
    ("Open-Air-Kino im Kulturhaus Osterfeld", "Open-Air-Kino im Kulturhaus Osterfeld", "A1911"),
    ("rex Filmpalast Pforzheim", "rex Filmpalast Pforzheim", "A1068"),
    ("Filmwelt Landau", "Filmwelt Landau", "A0548"),
    ("Open Air Kino Landau", "Open Air Kino Landau", "A2564"),
    ("Universum Kinocenter", "Universum Kinocenter", "A1245"),
    ("Cineplex Baden-Baden", "Cineplex Baden-Baden", "A2689"),
    ("moviac - Kino im Kaiserhof", "moviac - Kino im Kaiserhof", "A2628"),
    ("FORUM Rastatt", "FORUM Rastatt", "A2731"),
    ("Cineplex Bruchsal", "Cineplex Bruchsal", "A1373"),
    ("KuKi - Das Kultkino", "KuKi - Das Kultkino", "A2623"),
    ("Kulisse Ettlingen", "Kulisse Ettlingen", "A0876"),
    ("Kinostar Filmwelt Bretten", "Kinostar Filmwelt Bretten", "A0508"),
    ("Merkur-Kino-Center", "Merkur-Kino-Center", "A0944"),
    ("Kinocenter Gernsbach", "Kinocenter Gernsbach", "A0771")
  ))
  private def r_saarbruecken: R = ("saarbruecken", "Saarbrücken", "Saarland", 49.23262, 7.00982, Seq("Saarbrücken", "Sankt Ingbert", "Regionalverband Saarbrücken", "Neunkirchen", "Neunkirchen (Innenstadt)", "Homburg", "Saarlouis", "Zweibrücken", "Sankt Wendel", "Lebach", "Illingen", "Schmelz", "Bous", "Rehlingen-Siersburg"), Seq(
    ("CineStar Saarbrücken", "CineStar Saarbrücken", "A0340"),
    ("Kino im Filmhaus", "Kino im Filmhaus", "A1125"),
    ("Passage Saarbrücken", "Passage Saarbrücken", "A1586"),
    ("UT-Kino-Center", "UT-Kino-Center", "A1249"),
    ("Unifilm im AudiMax", "Unifilm im AudiMax", "A2323"),
    ("camera zwo - das arthouse kino", "camera zwo - das arthouse kino", "A0137"),
    ("kino achteinhalb", "kino achteinhalb", "A0698"),
    ("Kinowerkstatt", "Kinowerkstatt", "A0844"),
    ("Neues Regina", "Neues Regina", "A1057"),
    ("Residenz", "Residenz", "A2298"),
    ("Cinetower", "Cinetower", "A0385"),
    ("Neues Eden", "Neues Eden", "A0987"),
    ("Eden Cinehouse", "Eden Cinehouse", "A1399"),
    ("Movie-World", "Movie-World", "A0976"),
    ("Cinema Europa", "Cinema Europa", "A0250"),
    ("Neues Theater Sankt Wendel", "Neues Theater Sankt Wendel", "A0999"),
    ("City-Filmstudio", "City-Filmstudio", "A1379"),
    ("Union Theater Illingen", "Union Theater Illingen", "A1235"),
    ("Schmelzer Lichtspiele", "Schmelzer Lichtspiele", "A0909"),
    ("Thalia-Lichtspiele", "Thalia-Lichtspiele", "A1646"),
    ("Kino auf der Burg", "Kino auf der Burg", "A2513")
  ))
  private def r_bremen: R = ("bremen", "Bremen", "Bremen", 53.07582, 8.80717, Seq("Bremen", "Osterholz-Scharmbeck", "Delmenhorst", "Achim", "Syke", "Schwanewede", "Ritterhude", "Worpswede"), Seq(
    ("CITY 46 / Kommunalkino Bremen e.V.", "CITY 46 / Kommunalkino Bremen e.V.", "A0696"),
    ("CineStar Bremen - Kristall-Palast", "CineStar Bremen - Kristall-Palast", "A0376"),
    ("Cinema Ostertor", "Cinema Ostertor", "A1366"),
    ("CinemaxX Bremen", "CinemaxX Bremen", "A0274"),
    ("Cineplex Cinespace Bremen", "Cineplex Cinespace Bremen", "A0326"),
    ("City-Filmtheater", "City-Filmtheater", "A1378"),
    ("Filmkunsttheater Atlantis", "Filmkunsttheater Atlantis", "A0066"),
    ("Filmkunsttheater Gondel", "Filmkunsttheater Gondel", "A0588"),
    ("Instituto Cervantes - Bremen", "Instituto Cervantes - Bremen", "A2845"),
    ("Kulturkirche St. Stephani", "Kulturkirche St. Stephani", "A2681"),
    ("Schauburg Bremen", "Schauburg Bremen", "A1625"),
    ("Central Theater Osterholz-Scharmbeck", "Central Theater Osterholz-Scharmbeck", "A1348"),
    ("Oscar Kulturspielhaus", "Oscar Kulturspielhaus", "A2746"),
    ("MaxX", "MaxX", "A0266"),
    ("Koki Kommunales Kino", "Koki Kommunales Kino", "A0853"),
    ("Hansa Kino", "Hansa Kino", "A1447"),
    ("Film Palast", "Film Palast", "A1407"),
    ("Ritterhuder-Lichtspiele", "Ritterhuder-Lichtspiele", "A1605"),
    ("Music Hall Worpswede", "Music Hall Worpswede", "A0979")
  ))
  private def r_heilbronn: R = ("heilbronn", "Heilbronn", "Baden-Württemberg", 49.13995, 9.22054, Seq("Heilbronn", "Sinsheim", "Neckarsulm", "Öhringen", "Besigheim", "Mosbach", "Lauffen", "Grossbottwar", "Löchgau", "Kirchheim am Neckar", "Forchtenberg", "Hardthausen am Kocher", "Erligheim"), Seq(
    ("CinemaxX Heilbronn", "CinemaxX Heilbronn", "A2894"),
    ("Kinostar Arthaus Heilbronn", "Kinostar Arthaus Heilbronn", "A2748"),
    ("Open-Air-Kino-Heilbronn", "Open-Air-Kino-Heilbronn", "A2814"),
    ("Citydome Sinsheim", "Citydome Sinsheim", "A0402"),
    ("IMAX 3D Laser 4k Kino Sinsheim", "IMAX 3D Laser 4k Kino Sinsheim", "A0622"),
    ("Cineplex Neckarsulm", "Cineplex Neckarsulm", "A0475"),
    ("Kinostar Scala + Scala-Keller", "Kinostar Scala + Scala-Keller", "A0839"),
    ("Holi Filmtheater", "Holi Filmtheater", "A2203"),
    ("Scala Filmtheater Öhringen", "Scala Filmtheater Öhringen", "A0837"),
    ("Kinomobil Stuttgart - Alte Kelter", "Kinomobil Stuttgart - Alte Kelter", "A0793"),
    ("Kinomobil Stuttgart - Alte Kelter Besigheim", "Kinomobil Stuttgart - Alte Kelter Besigheim", "A1824"),
    ("Kinostar Filmwelt Mosbach", "Kinostar Filmwelt Mosbach", "A1513"),
    ("Kinomobil Stuttgart - Stadthalle Lauffen", "Kinomobil Stuttgart - Stadthalle Lauffen", "A0788"),
    ("Kinomobil Stuttgart - Kelter Winzerhausen", "Kinomobil Stuttgart - Kelter Winzerhausen", "A2132"),
    ("Kinomobil Stuttgart - Gemeindehalle Löchgau", "Kinomobil Stuttgart - Gemeindehalle Löchgau", "A0797"),
    ("Kinomobil Stuttgart - Kulturzentrum Alte Schule", "Kinomobil Stuttgart - Kulturzentrum Alte Schule", "A0785"),
    ("Open-Air-Kino Forchtenberg", "Open-Air-Kino Forchtenberg", "A2440"),
    ("Kinomobil Stuttgart - Gewölbekeller im Rathaus", "Kinomobil Stuttgart - Gewölbekeller im Rathaus", "A0786"),
    ("Kinomobil Stuttgart - Bürgerhaus Vordere Kelter", "Kinomobil Stuttgart - Bürgerhaus Vordere Kelter", "A0807")
  ))
  private def r_schwaebisch_gmuend: R = ("schwaebisch-gmuend", "Schwäbisch Gmünd", "Baden-Württemberg", 48.79947, 9.79809, Seq("Schwäbisch Gmünd", "Heidenheim", "Schorndorf", "Schwäbisch Hall", "Göppingen", "Geislingen an der Steige", "Ebersbach", "Murrhardt", "Gaildorf", "Rudersberg", "Donzdorf", "Königsbronn", "Deggingen", "Schlat"), Seq(
    ("Brazil", "Brazil", "A0105"),
    ("Traumpalast Schwäbisch Gmünd", "Traumpalast Schwäbisch Gmünd", "A1193"),
    ("Capitol Heidenheim", "Capitol Heidenheim", "A0140"),
    ("Kino-Center Heidenheim", "Kino-Center Heidenheim", "A0761"),
    ("Club Manufaktur", "Club Manufaktur", "A0849"),
    ("Traumpalast Schorndorf", "Traumpalast Schorndorf", "A0067"),
    ("Kino im Schafstall", "Kino im Schafstall", "A1497"),
    ("Lichtspielhaus Schwäbisch Hall", "Lichtspielhaus Schwäbisch Hall", "A0585"),
    ("Staufen-Movieplex", "Staufen-Movieplex", "A1151"),
    ("Gloria Kino Center Geislingen an der Steige", "Gloria Kino Center Geislingen an der Steige", "A0582"),
    ("Film Theater Ebersbach", "Film Theater Ebersbach", "A0539"),
    ("Kommunales Kino Murrhardt e.V.", "Kommunales Kino Murrhardt e.V.", "A1459"),
    ("Sonnen-Lichtspiele", "Sonnen-Lichtspiele", "A1140"),
    ("Löwenlichtspiele Rudersberg", "Löwenlichtspiele Rudersberg", "A0674"),
    ("Sommerkino Donzdorf", "Sommerkino Donzdorf", "A2544"),
    ("Kinomobil Stuttgart - Ketteler-Haus", "Kinomobil Stuttgart - Ketteler-Haus", "A0809"),
    ("Kinomobil Stuttgart - Feuerwehrhaus Deggingen", "Kinomobil Stuttgart - Feuerwehrhaus Deggingen", "A2128"),
    ("Kinomobil Stuttgart - Bürgerhaus", "Kinomobil Stuttgart - Bürgerhaus", "A0808")
  ))
  private def r_hannover: R = ("hannover", "Hannover", "Niedersachsen", 52.37052, 9.73322, Seq("Hannover", "Hildesheim", "Lehrte", "Langenhagen", "Neustadt am Rübenberge", "Bad Nenndorf"), Seq(
    ("Apollo Hannover", "Apollo Hannover", "A0035"),
    ("Astor Grand Cinema", "Astor Grand Cinema", "A1370"),
    ("CinemaxX Raschplatz", "CinemaxX Raschplatz", "A0296"),
    ("Hochhaus Lichtspiele", "Hochhaus Lichtspiele", "A0608"),
    ("Kino am Raschplatz", "Kino am Raschplatz", "A0590"),
    ("Kino im Sprengel", "Kino im Sprengel", "A1015"),
    ("Kommunales Kino im Künstlerhaus", "Kommunales Kino im Künstlerhaus", "A1495"),
    ("Open Air im Theaterhof", "Open Air im Theaterhof", "A2868"),
    ("Puschenkino Puki", "Puschenkino Puki", "A2818"),
    ("Seh-Fest Gilde Parkbühne", "Seh-Fest Gilde Parkbühne", "A2472"),
    ("Hochschulkino im Audimax", "Hochschulkino im Audimax", "A2876"),
    ("Thega Filmpalast Hildesheim", "Thega Filmpalast Hildesheim", "A1181"),
    ("Das Andere Kino", "Das Andere Kino", "A0431"),
    ("Open Air im Stadtpark", "Open Air im Stadtpark", "A2867"),
    ("CineMotion Langenhagen", "CineMotion Langenhagen", "A1251"),
    ("Cinema im Leinepark", "Cinema im Leinepark", "A2873"),
    ("Phoenix Kurlichtspiele Bad Nenndorf", "Phoenix Kurlichtspiele Bad Nenndorf", "A0707")
  ))
  private def r_braunschweig: R = ("braunschweig", "Braunschweig", "Niedersachsen", 52.26594, 10.52673, Seq("Braunschweig", "Wolfsburg", "Salzgitter", "Helmstedt", "Wolfenbüttel", "Peine", "Gifhorn", "Königslutter am Elm", "Grasleben"), Seq(
    ("C1 Cinema Braunschweig", "C1 Cinema Braunschweig", "A0284"),
    ("Roter Saal", "Roter Saal", "A2389"),
    ("SchunterKino", "SchunterKino", "A2745"),
    ("Universum Filmtheater", "Universum Filmtheater", "A1240"),
    ("CinemaxX Wolfsburg", "CinemaxX Wolfsburg", "A0283"),
    ("Delphin Palast", "Delphin Palast", "A0437"),
    ("Kino im Hallenbad - Wolfsburg", "Kino im Hallenbad - Wolfsburg", "A2385"),
    ("Metropol Theater Fallersleben", "Metropol Theater Fallersleben", "A0949"),
    ("Cinema in der Angerpassage", "Cinema in der Angerpassage", "A0253"),
    ("City-Theater Kultiplex", "City-Theater Kultiplex", "A1377"),
    ("Camera am Holzberg", "Camera am Holzberg", "A1327"),
    ("Roxy- Theater -Lichtspiele", "Roxy- Theater -Lichtspiele", "A1093"),
    ("Filmpalast Wolfenbüttel (Juliusstadt)", "Filmpalast Wolfenbüttel (Juliusstadt)", "A1422"),
    ("Astoria-Filmtheater", "Astoria-Filmtheater", "A0020"),
    ("Kinocenter am Steinweg", "Kinocenter am Steinweg", "A0574"),
    ("Kammerlichtspiele Könnigslutter am Elm", "Kammerlichtspiele Könnigslutter am Elm", "A0637"),
    ("Autokino in Grasleben", "Autokino in Grasleben", "A2937")
  ))
  private def r_villingen_schwenningen: R = ("villingen-schwenningen", "Villingen-Schwenningen", "Baden-Württemberg", 48.06226, 8.49358, Seq("Villingen-Schwenningen", "Alpirsbach", "Tuttlingen", "Oberndorf am Neckar", "Furtwangen im Schwarzwald", "Rottweil", "Donaueschingen", "Schramberg", "Trossingen", "Triberg im Schwarzwald"), Seq(
    ("Blue Boxx", "Blue Boxx", "A0102"),
    ("CineStar Villingen-Schwenningen", "CineStar Villingen-Schwenningen", "A2887"),
    ("Kommunales Kino Guckloch Schwarzwald-Baar-Kreis", "Kommunales Kino Guckloch Schwarzwald-Baar-Kreis", "A0863"),
    ("Kino unterm Sternenhimmel Alpirsbach", "Kino unterm Sternenhimmel Alpirsbach", "A2486"),
    ("Open Air Kino im Kreuzgarten", "Open Air Kino im Kreuzgarten", "A1818"),
    ("Subiaco Galerie", "Subiaco Galerie", "A0566"),
    ("Open Air Kino Tuttlingen", "Open Air Kino Tuttlingen", "A1999"),
    ("Scala Tuttlingen", "Scala Tuttlingen", "A1109"),
    ("KKK-Filmtheater", "KKK-Filmtheater", "A0848"),
    ("Sommernachtskino im Klosterhof in Oberndorf", "Sommernachtskino im Klosterhof in Oberndorf", "A1987"),
    ("Kommunales Guckloch-Kino Furtwangen e.V.", "Kommunales Guckloch-Kino Furtwangen e.V.", "A0855"),
    ("Open Air Kino Furtwangen", "Open Air Kino Furtwangen", "A1774"),
    ("Central-Kino Rottweil", "Central-Kino Rottweil", "A0199"),
    ("Kommunales Kino Guckloch Donaueschingen", "Kommunales Kino Guckloch Donaueschingen", "A0864"),
    ("Subiaco Schramberg", "Subiaco Schramberg", "A1169"),
    ("Kommunales Kino Trossingen", "Kommunales Kino Trossingen", "A2855"),
    ("Kronen-Lichtspiele Triberg im Schwarzwald", "Kronen-Lichtspiele Triberg im Schwarzwald", "A1526")
  ))
  private def r_dresden: R = ("dresden", "Dresden", "Sachsen", 51.05089, 13.73832, Seq("Dresden", "Pirna", "Weinböhla", "Radeburg"), Seq(
    ("CLUB PASSAGE", "CLUB PASSAGE", "A0410"),
    ("CinemaxX Dresden", "CinemaxX Dresden", "A0285"),
    ("Cineplex Kristallpalast Dresden", "Cineplex Kristallpalast Dresden", "A1222"),
    ("Filmnächte am Elbufer", "Filmnächte am Elbufer", "A1801"),
    ("Filmtheater Schauburg", "Filmtheater Schauburg", "A1477"),
    ("Kino im Kasten - Das Studentenkino", "Kino im Kasten - Das Studentenkino", "A0723"),
    ("Kino in der Scheune Dresden", "Kino in der Scheune Dresden", "A1700"),
    ("Programmkino Ost", "Programmkino Ost", "A1045"),
    ("Rundkino Dresden", "Rundkino Dresden", "A2018"),
    ("Thalia Dresden", "Thalia Dresden", "A1173"),
    ("UCI Elbe Park", "UCI Elbe Park", "A1208"),
    ("Zentralkino Dresden", "Zentralkino Dresden", "G011F"),
    ("k.i.d. - Kino im Dach", "k.i.d. - Kino im Dach", "A0651"),
    ("Filmpalast Pirna", "Filmpalast Pirna", "A0511"),
    ("Openair Zentralgasthof Weinböhla GmbH", "Openair Zentralgasthof Weinböhla GmbH", "A2202"),
    ("Kinobar", "Kinobar", "A1759")
  ))
  private def r_jena: R = ("jena", "Jena", "Thüringen", 50.92878, 11.5899, Seq("Jena", "Weimar", "Gera", "Rudolstadt", "Saalfeld", "Bad Klosterlausnitz"), Seq(
    ("CineStar Jena", "CineStar Jena", "A0328"),
    ("Filmarena Open-Air Kino Theatervorplatz", "Filmarena Open-Air Kino Theatervorplatz", "A1844"),
    ("Kino am Markt", "Kino am Markt", "A2732"),
    ("Kino im Schillerhof", "Kino im Schillerhof", "A0729"),
    ("CineStar Weimar", "CineStar Weimar", "A0360"),
    ("Kommunales Kino im mon ami", "Kommunales Kino im mon ami", "A0867"),
    ("Lichthaus Kino im Straßenbahndepot/e-werk", "Lichthaus Kino im Straßenbahndepot/e-werk", "A0906"),
    ("Open Air am Lichthaus-Kino", "Open Air am Lichthaus-Kino", "A2783"),
    ("Kino im COMMA", "Kino im COMMA", "A0416"),
    ("Metropol", "Metropol", "A2677"),
    ("Open Air im Comma-Garten", "Open Air im Comma-Garten", "A2759"),
    ("Cineplex Rudolstadt", "Cineplex Rudolstadt", "A0307"),
    ("Freiluftkino im Garten des Schillerhauses", "Freiluftkino im Garten des Schillerhauses", "A2443"),
    ("Uferpalast Rudolstadt", "Uferpalast Rudolstadt", "A2390"),
    ("Cineplex Saalfeld", "Cineplex Saalfeld", "A0157"),
    ("Holzlandkino", "Holzlandkino", "A0618")
  ))
  private def r_schweinfurt: R = ("schweinfurt", "Schweinfurt", "Bayern", 50.04937, 10.22175, Seq("Schweinfurt", "Würzburg", "Bad Neustadt an der Saale", "Kitzingen", "Bad Kissingen", "Karlstadt am Main", "Hammelburg", "Bad Königshofen im Grabfeld", "Dettelbach", "Sommerach"), Seq(
    ("Filmwelt Schweinfurt", "Filmwelt Schweinfurt", "A2475"),
    ("KuK Kino und Kneipe", "KuK Kino und Kneipe", "A0874"),
    ("Open Air am KuK", "Open Air am KuK", "A2532"),
    ("Weltbio Kinocenter", "Weltbio Kinocenter", "A1272"),
    ("Central Programmkino", "Central Programmkino", "A2621"),
    ("Central im Bürgerbräu", "Central im Bürgerbräu", "A2742"),
    ("CinemaxX Würzburg", "CinemaxX Würzburg", "A0263"),
    ("Rex Kinos", "Rex Kinos", "A1065"),
    ("Starlight Kinos", "Starlight Kinos", "A1149"),
    ("Roxy Kino Kitzingen", "Roxy Kino Kitzingen", "A1738"),
    ("Universum Kino Palast", "Universum Kino Palast", "A1657"),
    ("Burg-Lichtspiele (Karlstadt am Main)", "Burg-Lichtspiele (Karlstadt am Main)", "A0118"),
    ("Kino in der Stadtbibliothek Hammelburg", "Kino in der Stadtbibliothek Hammelburg", "A2348"),
    ("Stadtsaal-Lichtspiele", "Stadtsaal-Lichtspiele", "A1463"),
    ("Cineworld Mainfrankenpark Dettelbach", "Cineworld Mainfrankenpark Dettelbach", "A0387"),
    ("Open Air Kino Winzerkeller Sommerach", "Open Air Kino Winzerkeller Sommerach", "A2277")
  ))
  private def r_freiburg: R = ("freiburg", "Freiburg", "Baden-Württemberg", 47.9959, 7.85222, Seq("Freiburg", "Freiburg im Breisgau", "Emmendingen", "Waldkirch", "Bad Krozingen", "Breisach am Rhein", "Titisee-Neustadt", "Kenzingen", "Schallstadt", "Buggingen"), Seq(
    ("CinemaxX Freiburg", "CinemaxX Freiburg", "A0291"),
    ("Friedrichsbau-Apollo", "Friedrichsbau-Apollo", "A0040"),
    ("Harmonie Freiburg", "Harmonie Freiburg", "A1218"),
    ("Kandelhof", "Kandelhof", "A0644"),
    ("Kommunales Kino - Im Alten Wiehrebahnhof", "Kommunales Kino - Im Alten Wiehrebahnhof", "A0862"),
    ("Sommernachts-Kino", "Sommernachts-Kino", "A2036"),
    ("aka-Filmclub", "aka-Filmclub", "A1474"),
    ("Maja", "Maja", "A2808"),
    ("Kommunales Kino Klappe 11", "Kommunales Kino Klappe 11", "A2805"),
    ("Joki Kino am Bahnhof", "Joki Kino am Bahnhof", "A0722"),
    ("Engel-Lichtspiele", "Engel-Lichtspiele", "A1403"),
    ("Krone-Theater", "Krone-Theater", "A0873"),
    ("Löwen-Lichtspiele Kenzingen", "Löwen-Lichtspiele Kenzingen", "A1547"),
    ("Kinomobil Stuttgart - Johann-Philipp-Glock-Schule", "Kinomobil Stuttgart - Johann-Philipp-Glock-Schule", "A2100"),
    ("Kino im Rathaus", "Kino im Rathaus", "A0728")
  ))
  private def r_wuppertal: R = ("wuppertal", "Wuppertal", "Nordrhein-Westfalen", 51.25627, 7.14816, Seq("Wuppertal", "Remscheid", "Lüdenscheid", "Hilden", "Mettmann", "Wermelskirchen", "Gevelsberg", "Schwelm", "Radevormwald"), Seq(
    ("Cinema Wuppertal", "Cinema Wuppertal", "A0381"),
    ("CinemaxX Wuppertal", "CinemaxX Wuppertal", "A0292"),
    ("Rex Wuppertal", "Rex Wuppertal", "A2680"),
    ("Talflimmern Open-Air-Kino", "Talflimmern Open-Air-Kino", "A1918"),
    ("CineStar Remscheid", "CineStar Remscheid", "A2901"),
    ("Open Air Kino Remscheid", "Open Air Kino Remscheid", "A2826"),
    ("Open Air an der Gelben Villa", "Open Air an der Gelben Villa", "A2810"),
    ("Filmpalast Lüdenscheid", "Filmpalast Lüdenscheid", "A0519"),
    ("Park-Theater Lüdenscheid", "Park-Theater Lüdenscheid", "A1030"),
    ("Lux Lichtspiele", "Lux Lichtspiele", "A0933"),
    ("Weltspiegel-Kino-Center", "Weltspiegel-Kino-Center", "A1274"),
    ("Film-Eck", "Film-Eck", "A1414"),
    ("Filmriss Kino", "Filmriss Kino", "A0524"),
    ("Kinocenter Schwelm", "Kinocenter Schwelm", "A0776"),
    ("Corso Kinocenter", "Corso Kinocenter", "A0425")
  ))
  private def r_ravensburg: R = ("ravensburg", "Ravensburg", "Baden-Württemberg", 47.78198, 9.61062, Seq("Ravensburg", "Lindau", "Weingarten", "Bad Saulgau", "Wangen im Allgäu", "Bad Waldsee", "Tettnang", "Isny im Allgäu", "Lindenberg im Allgäu", "Kressbronn", "Argenbühl"), Seq(
    ("CineParC Ravensburg", "CineParC Ravensburg", "A1393"),
    ("Kinozentrum Frauentor", "Kinozentrum Frauentor", "A0845"),
    ("Club Vaudeville", "Club Vaudeville", "A0411"),
    ("Parktheater und Studio", "Parktheater und Studio", "A1031"),
    ("Kulturzentrum Linse", "Kulturzentrum Linse", "A1839"),
    ("Open Air Kino im Schlösse Hof", "Open Air Kino im Schlösse Hof", "A2496"),
    ("Autokino Bad Saulgau am Fesplatz", "Autokino Bad Saulgau am Fesplatz", "A2922"),
    ("Kino Saulgau", "Kino Saulgau", "A0750"),
    ("Lichtspielhaus Wangen im Allgäu", "Lichtspielhaus Wangen im Allgäu", "A1163"),
    ("seenema - Stadtkino Bad Waldsee eG", "seenema - Stadtkino Bad Waldsee eG", "A2722"),
    ("KiTT - Kino und Kleinkunst Tettnang e.V.", "KiTT - Kino und Kleinkunst Tettnang e.V.", "A1518"),
    ("Neues Ringtheater", "Neues Ringtheater", "A0992"),
    ("Neues Krone Kino", "Neues Krone Kino", "A1574"),
    ("Kinomobil Stuttgart - Lände-Cafe", "Kinomobil Stuttgart - Lände-Cafe", "A1761"),
    ("Kinomobil Stuttgart - Bürgersaal Rathaus", "Kinomobil Stuttgart - Bürgersaal Rathaus", "A1762")
  ))
  private def r_regensburg: R = ("regensburg", "Regensburg", "Bayern", 49.01513, 12.10161, Seq("Regensburg", "Kelheim", "Abensberg", "Burglengenfeld", "Maxhütte-Haidhof", "Nittenau", "Wald"), Seq(
    ("CinemaxX Regensburg", "CinemaxX Regensburg", "A0289"),
    ("Filmgalerie im Leeren Beutel", "Filmgalerie im Leeren Beutel", "A0497"),
    ("Garbo", "Garbo", "A1436"),
    ("Kinos im Andreasstadel", "Kinos im Andreasstadel", "A1280"),
    ("Open Air Kino Auf Schloss Pürkelgut", "Open Air Kino Auf Schloss Pürkelgut", "A1876"),
    ("Ostentor", "Ostentor", "A1581"),
    ("Regina Filmtheater Regensburg", "Regina Filmtheater Regensburg", "A1052"),
    ("Turm-Theater Regensburg", "Turm-Theater Regensburg", "A1195"),
    ("Kelheimer Lichtspiele", "Kelheimer Lichtspiele", "A1462"),
    ("Roxy Kino", "Roxy Kino", "A1086"),
    ("Starmexx - Erlebniskino", "Starmexx - Erlebniskino", "A2664"),
    ("Kinocenter Maxhütte-Haidhof", "Kinocenter Maxhütte-Haidhof", "A0656"),
    ("Kino-Center Nittenau", "Kino-Center Nittenau", "A1461"),
    ("Kinomobil Stuttgart - Feuerwehrhaus Wald", "Kinomobil Stuttgart - Feuerwehrhaus Wald", "A1937")
  ))
  private def r_kiel: R = ("kiel", "Kiel", "Schleswig-Holstein", 54.32133, 10.13489, Seq("Kiel", "Neumünster", "Rendsburg", "Eckernförde", "Preetz", "Plön", "Bordesholm"), Seq(
    ("CinemaxX Kiel", "CinemaxX Kiel", "A0279"),
    ("Hansafilmpalast", "Hansafilmpalast", "A2842"),
    ("Kommunales Kino in der Pumpe", "Kommunales Kino in der Pumpe", "A1522"),
    ("Metro-Kino im Schloßhof", "Metro-Kino im Schloßhof", "A1750"),
    ("Studio - Filmtheater am Dreiecksplatz", "Studio - Filmtheater am Dreiecksplatz", "A0993"),
    ("Traum-Kino", "Traum-Kino", "A0694"),
    ("CineStar Neumünster", "CineStar Neumünster", "A1372"),
    ("KDW Neumünster", "KDW Neumünster", "A2655"),
    ("Kino-Center Rendsburg", "Kino-Center Rendsburg", "A0775"),
    ("Schauburg Rendsburg", "Schauburg Rendsburg", "A1623"),
    ("Das Haus", "Das Haus", "A0866"),
    ("Captiol Cine Center", "Captiol Cine Center", "A1747"),
    ("Astra-Filmtheater", "Astra-Filmtheater", "A1300"),
    ("Savoy (Bordesholm)", "Savoy (Bordesholm)", "A1614")
  ))
  private def r_konstanz: R = ("konstanz", "Konstanz", "Baden-Württemberg", 47.66033, 9.17582, Seq("Konstanz", "Friedrichshafen", "Überlingen", "Singen", "Singen am Hohentwiel", "Radolfzell am Bodensee", "Markdorf", "Gottmadingen", "Immenstaad am Bodensee", "Reichenau"), Seq(
    ("CineStar Konstanz", "CineStar Konstanz", "A0338"),
    ("Open Air am Neuwerk", "Open Air am Neuwerk", "A2774"),
    ("Zebra", "Zebra", "A1287"),
    ("Cineplex Friedrichshafen", "Cineplex Friedrichshafen", "A0313"),
    ("Kulturhaus Caserne Kino Studio 17", "Kulturhaus Caserne Kino Studio 17", "A0754"),
    ("Cinegreth", "Cinegreth", "A0226"),
    ("Kammer-Lichtspiele Überlingen", "Kammer-Lichtspiele Überlingen", "A0639"),
    ("Cineplex Singen", "Cineplex Singen", "A0323"),
    ("Kino in der Gems", "Kino in der Gems", "A0570"),
    ("Universum-Nostalgiekino", "Universum-Nostalgiekino", "A2916"),
    ("Theaterstadel", "Theaterstadel", "A1178"),
    ("Kinomobil Stuttgart - Neues Rathaus", "Kinomobil Stuttgart - Neues Rathaus", "A1939"),
    ("Kinomobil Stuttgart - Strandbad Aquastaad", "Kinomobil Stuttgart - Strandbad Aquastaad", "A1951"),
    ("Kinomobil Stuttgart - Hochwart-Wiese", "Kinomobil Stuttgart - Hochwart-Wiese", "A1953")
  ))
  private def r_landsberg_am_lech: R = ("landsberg-am-lech", "Landsberg am Lech", "Bayern", 48.04819, 10.88282, Seq("Landsberg am Lech", "Bad Wörishofen", "Diessen am Ammersee", "Weilheim", "Weilheim in Oberbayern", "Kaufering", "Seefeld", "Türkheim", "Inning-Stegen", "Penzing"), Seq(
    ("Filmforum im Stadttheater Landsberg", "Filmforum im Stadttheater Landsberg", "A2795"),
    ("Olympia Filmtheater Landsberg", "Olympia Filmtheater Landsberg", "A1579"),
    ("Open Air Kino Landsberg", "Open Air Kino Landsberg", "A2207"),
    ("Filmhaus Bad Wörishofen - Lichtspiele am Bahnhof", "Filmhaus Bad Wörishofen - Lichtspiele am Bahnhof", "A1537"),
    ("Open Air Kino Unter den Linden", "Open Air Kino Unter den Linden", "A2543"),
    ("Cinema Augustinum Diessen am Ammersee", "Cinema Augustinum Diessen am Ammersee", "A2714"),
    ("Kinowelt am Ammersee", "Kinowelt am Ammersee", "A0841"),
    ("Trifthof Kinocenter", "Trifthof Kinocenter", "A1649"),
    ("Starlight", "Starlight", "A1633"),
    ("Filmpalast Kaufering", "Filmpalast Kaufering", "A2617"),
    ("Kino Breitwand im Schloß Seefeld", "Kino Breitwand im Schloß Seefeld", "A0713"),
    ("Filmhaus", "Filmhaus", "A1524"),
    ("Kino in der Alten Brauerei", "Kino in der Alten Brauerei", "A0735"),
    ("Cineplex Penzing", "Cineplex Penzing", "A2643")
  ))
  private def r_augsburg: R = ("augsburg", "Augsburg", "Bayern", 48.37154, 10.89851, Seq("Augsburg", "Friedberg (Hessen)", "Königsbrunn", "Aichach", "Gersthofen", "Meitingen", "Wertingen"), Seq(
    ("CineStar Augsburg", "CineStar Augsburg", "A0350"),
    ("CinemaxX Augsburg", "CinemaxX Augsburg", "A0276"),
    ("Liliom", "Liliom", "A1541"),
    ("Mephisto Augsburg", "Mephisto Augsburg", "A1560"),
    ("Open Air Kino Lechflimmern Familienbad am Plärrer", "Open Air Kino Lechflimmern Familienbad am Plärrer", "A1800"),
    ("Savoy Kino", "Savoy Kino", "A1105"),
    ("Thalia Augsburg", "Thalia Augsburg", "A1645"),
    ("Kino Center", "Kino Center", "A0100"),
    ("Cineplex Königsbrunn", "Cineplex Königsbrunn", "A0312"),
    ("Cineplex Aichach", "Cineplex Aichach", "A1374"),
    ("Open Air Seebühne", "Open Air Seebühne", "A2539"),
    ("Cineplex Meitingen", "Cineplex Meitingen", "A0217"),
    ("Filmtheater Wertingen", "Filmtheater Wertingen", "A2120")
  ))
  private def r_offenburg: R = ("offenburg", "Offenburg", "Baden-Württemberg", 48.47377, 7.94495, Seq("Offenburg", "Freudenstadt", "Lahr", "Lahr am Schwarzwald", "Kehl", "Achern", "Oberkirch", "Haslach im Kinzigtal", "Rust", "Rheinmünster"), Seq(
    ("FORUM Offenburg", "FORUM Offenburg", "A2334"),
    ("Kommunales Kino im KiK", "Kommunales Kino im KiK", "A2854"),
    ("Sommer Kino Nächte Ortenau", "Sommer Kino Nächte Ortenau", "A2002"),
    ("Central Freudenstadt", "Central Freudenstadt", "A0186"),
    ("Subiaco im Kurhaus", "Subiaco im Kurhaus", "A1170"),
    ("FORUM Lahr", "FORUM Lahr", "A2648"),
    ("Open Air Kino im Innenhof Schlachthof", "Open Air Kino im Innenhof Schlachthof", "A1986"),
    ("Kino-Center Kehl", "Kino-Center Kehl", "A1507"),
    ("Tivoli-Filmtheater", "Tivoli-Filmtheater", "A1185"),
    ("Open-Air Oberkirch", "Open-Air Oberkirch", "A2525"),
    ("Rio + Scala", "Rio + Scala", "A1081"),
    ("Magic Cinema im Europa Park", "Magic Cinema im Europa Park", "A0936"),
    ("Kinomobil Stuttgart - Festhalle Rheinmünster-Schwarzach", "Kinomobil Stuttgart - Festhalle Rheinmünster-Schwarzach", "A0802")
  ))
  private def r_tauberbischofsheim: R = ("tauberbischofsheim", "Tauberbischofsheim", "Baden-Württemberg", 49.62472, 9.66278, Seq("Tauberbischofsheim", "Wertheim am Main", "Bad Mergentheim", "Lauda-Königshofen", "Walldürn", "Ochsenfurt", "Marktheidenfeld", "Weikersheim", "Boxberg", "Dörzbach", "Assamstadt"), Seq(
    ("Filmtheater Badischer Hof", "Filmtheater Badischer Hof", "A0541"),
    ("Open Air Kino Rotary-Benefiz", "Open Air Kino Rotary-Benefiz", "A1995"),
    ("Roxy Wertheim", "Roxy Wertheim", "A1087"),
    ("Movies Bad Mergentheim", "Movies Bad Mergentheim", "A2629"),
    ("Kinomobil Stuttgart - Lauda Sternen Filmtheater", "Kinomobil Stuttgart - Lauda Sternen Filmtheater", "A0794"),
    ("Löwenlichtspiele Walldürn", "Löwenlichtspiele Walldürn", "A0922"),
    ("Casablanca Ochsenfurt", "Casablanca Ochsenfurt", "A0173"),
    ("Movie im Luitpoldhaus", "Movie im Luitpoldhaus", "A0965"),
    ("Kinomobil Stuttgart - club-w-71", "Kinomobil Stuttgart - club-w-71", "A0798"),
    ("Kinomobil Stuttgart - Medien und Kulturzentrum", "Kinomobil Stuttgart - Medien und Kulturzentrum", "A0783"),
    ("Kinomobil Stuttgart - Schlosshof Schloss Eyb", "Kinomobil Stuttgart - Schlosshof Schloss Eyb", "A2227"),
    ("Kinomobil Stuttgart - Grundschule Assamstadt", "Kinomobil Stuttgart - Grundschule Assamstadt", "A1936")
  ))
  private def r_kassel: R = ("kassel", "Kassel", "Hessen", 51.31667, 9.5, Seq("Kassel", "Baunatal", "Hofgeismar", "Fritzlar", "Melsungen", "Borgentreich"), Seq(
    ("Bali-Kinos im KulturBahnhof Kassel", "Bali-Kinos im KulturBahnhof Kassel", "A0078"),
    ("Cineplex Capitol Kassel", "Cineplex Capitol Kassel", "A1375"),
    ("Filmladen Kassel e.V.", "Filmladen Kassel e.V.", "A0505"),
    ("Filmpalast Kassel", "Filmpalast Kassel", "A0354"),
    ("GLORIA-Kino am Ständeplatz", "GLORIA-Kino am Ständeplatz", "A0577"),
    ("Kasseler Open Air Sommerfilm", "Kasseler Open Air Sommerfilm", "A2063"),
    ("Cineplex Baunatal", "Cineplex Baunatal", "A2704"),
    ("Open-Air-Kino Hofgeismar", "Open-Air-Kino Hofgeismar", "A2289"),
    ("Cineplex Royal Fritzlar", "Cineplex Royal Fritzlar", "A0221"),
    ("Freilichtkino Melsungen", "Freilichtkino Melsungen", "A2193"),
    ("Central-Kino Borgentreich", "Central-Kino Borgentreich", "A0201")
  ))
  private def r_ingolstadt: R = ("ingolstadt", "Ingolstadt", "Bayern", 48.76508, 11.42372, Seq("Ingolstadt", "Neuburg an der Donau", "Pfaffenhofen an der Ilm", "Schrobenhausen", "Eichstätt", "Wolnzach", "Rennertshofen"), Seq(
    ("Altstadtkinos Ingolstadt - Cinema", "Altstadtkinos Ingolstadt - Cinema", "A1369"),
    ("Altstadtkinos Ingolstadt - Union", "Altstadtkinos Ingolstadt - Union", "A1225"),
    ("Audi Programmkino", "Audi Programmkino", "A2784"),
    ("CineStar Ingolstadt", "CineStar Ingolstadt", "A0339"),
    ("Kino Open Air Ingolstadt", "Kino Open Air Ingolstadt", "A2024"),
    ("Kinopalast Neuburg an der Donau", "Kinopalast Neuburg an der Donau", "A0820"),
    ("Cineplex Pfaffenhofen", "Cineplex Pfaffenhofen", "A0325"),
    ("Herzog-Filmtheater", "Herzog-Filmtheater", "A0605"),
    ("Filmstudio im alten Stadttheater", "Filmstudio im alten Stadttheater", "A2119"),
    ("Amper-Lichtspiele", "Amper-Lichtspiele", "A1466"),
    ("Cinema Kulturtreff Rennertshofen", "Cinema Kulturtreff Rennertshofen", "A0232")
  ))
  private def r_hechingen: R = ("hechingen", "Hechingen", "Baden-Württemberg", 48.35149, 8.96317, Seq("Hechingen", "Herrenberg", "Nagold", "Albstadt", "Rottenburg am Neckar", "Balingen", "Mössingen"), Seq(
    ("Burgtheater-Kinos", "Burgtheater-Kinos", "A2787"),
    ("Open Air Kino Burg Hohenzollern", "Open Air Kino Burg Hohenzollern", "A2053"),
    ("Schwanen-Kinos", "Schwanen-Kinos", "A2819"),
    ("Kommunales Kino Herrenberg", "Kommunales Kino Herrenberg", "A0860"),
    ("Open Air Kino Herrenberg", "Open Air Kino Herrenberg", "A2770"),
    ("Krone-Lichtspiele", "Krone-Lichtspiele", "A0693"),
    ("Open Air im Badepark Nagold", "Open Air im Badepark Nagold", "A2235"),
    ("Capitol Filmpalast - Albstadt", "Capitol Filmpalast - Albstadt", "A2878"),
    ("Kino im Waldhorn", "Kino im Waldhorn", "A0732"),
    ("Bali Kino-Palast", "Bali Kino-Palast", "A1308"),
    ("Lichtspiele Mössingen", "Lichtspiele Mössingen", "A1536")
  ))
  private def r_juelich: R = ("juelich", "Jülich", "Nordrhein-Westfalen", 50.92149, 6.36267, Seq("Jülich", "Düren", "Grevenbroich", "Eschweiler", "Alsdorf", "Erkelenz", "Heinsberg", "Hückelhoven", "Würselen", "Walheim"), Seq(
    ("KuBa", "KuBa", "A1680"),
    ("Open Air Kino Jülich", "Open Air Kino Jülich", "A2765"),
    ("Das Lumen Filmtheater", "Das Lumen Filmtheater", "A0418"),
    ("Grefi Kino Grevenbroich", "Grefi Kino Grevenbroich", "A0591"),
    ("Primus-Kinocenter", "Primus-Kinocenter", "A1041"),
    ("Cinetower Alsdorf", "Cinetower Alsdorf", "A0069"),
    ("Gloria Filmpalast Erkelenz", "Gloria Filmpalast Erkelenz", "A0581"),
    ("Roxy Filmtheater Heinsberg", "Roxy Filmtheater Heinsberg", "A0248"),
    ("Corso-Filmpalast Hilfarth", "Corso-Filmpalast Hilfarth", "A0426"),
    ("Metropolis Würselen", "Metropolis Würselen", "A1563"),
    ("Kinomobil Stuttgart - Römerhaus", "Kinomobil Stuttgart - Römerhaus", "A1954")
  ))
  private def r_butzbach: R = ("butzbach", "Butzbach", "Hessen", 50.43395, 8.67122, Seq("Butzbach", "Neu-Anspach", "Grünberg", "Bad Nauheim", "Nidda", "Weilburg", "Lich", "Weilmünster"), Seq(
    ("Butzbacher Filmtheater", "Butzbacher Filmtheater", "A0154"),
    ("Open-Air-Kino im Landgrafenschloss", "Open-Air-Kino im Landgrafenschloss", "A2013"),
    ("Kino Neu Anspach", "Kino Neu Anspach", "A1612"),
    ("Open-Air Kino Schwimmbad Neu-Ansbach", "Open-Air Kino Schwimmbad Neu-Ansbach", "A2107"),
    ("Lichtspiele Grünberg", "Lichtspiele Grünberg", "A0044"),
    ("Open Air Kino auf dem Marktplatz", "Open Air Kino auf dem Marktplatz", "A2565"),
    ("Filmbühne Bad Nauheim", "Filmbühne Bad Nauheim", "G01HV"),
    ("Lumos Lichtspiele &amp; Lounge", "Lumos Lichtspiele &amp; Lounge", "A2683"),
    ("Delphi - Filmtheater", "Delphi - Filmtheater", "A0434"),
    ("Traumstern", "Traumstern", "A1190"),
    ("Saalbau-Lichtspiele Weilmünster", "Saalbau-Lichtspiele Weilmünster", "A1611")
  ))
  private def r_luebeck: R = ("luebeck", "Lübeck", "Schleswig-Holstein", 53.86893, 10.68729, Seq("Lübeck", "Bad Oldesloe", "Bad Schwartau", "Mölln", "Bad Segeberg", "Neustadt in Holstein", "Ratzeburg", "Schönberg"), Seq(
    ("CineStar Lübeck - Filmhaus", "CineStar Lübeck - Filmhaus", "A0498"),
    ("CineStar Lübeck - Stadthalle", "CineStar Lübeck - Stadthalle", "A0367"),
    ("Kino Koki - Kommunales Kino Lübeck", "Kino Koki - Kommunales Kino Lübeck", "A2241"),
    ("OHO-Kinocenter", "OHO-Kinocenter", "A1010"),
    ("Movie Star Bad Schwartau", "Movie Star Bad Schwartau", "A2326"),
    ("Eulenspiegelkino Mölln", "Eulenspiegelkino Mölln", "A0741"),
    ("Cine Planet 5", "Cine Planet 5", "A1353"),
    ("Kino-Center Kremper Tor", "Kino-Center Kremper Tor", "A0060"),
    ("Burgtheater Ratzeburg", "Burgtheater Ratzeburg", "A0120"),
    ("Blitz-Lichtspiele Schönberg", "Blitz-Lichtspiele Schönberg", "A1318")
  ))
  private def r_arnsberg: R = ("arnsberg", "Arnsberg", "Nordrhein-Westfalen", 51.38333, 8.08333, Seq("Arnsberg", "Soest", "Meschede", "Plettenberg", "Lennestadt", "Brilon", "Schmallenberg"), Seq(
    ("Apollo Arnsberg-Neheim", "Apollo Arnsberg-Neheim", "A0038"),
    ("Centra-Theater Arnsberg", "Centra-Theater Arnsberg", "A0184"),
    ("Residenz Kino-Center", "Residenz Kino-Center", "A1062"),
    ("Bürgerzentrum Alter Schlachthof", "Bürgerzentrum Alter Schlachthof", "A0117"),
    ("Neues Universum", "Neues Universum", "A0994"),
    ("Linden-Theater", "Linden-Theater", "A1701"),
    ("Weidenhof Kino", "Weidenhof Kino", "A1270"),
    ("Lichtspielhaus Lennestadt", "Lichtspielhaus Lennestadt", "A0912"),
    ("Cineplex Brilon", "Cineplex Brilon", "G00YX"),
    ("Lichtwerk", "Lichtwerk", "A1688")
  ))
  private def r_loerrach: R = ("loerrach", "Lörrach", "Baden-Württemberg", 47.61497, 7.66457, Seq("Lörrach", "Weil am Rhein", "Rheinfelden", "Schopfheim", "Müllheim", "Neuenburg am Rhein", "Kandern"), Seq(
    ("Cineplex Lörrach", "Cineplex Lörrach", "A0321"),
    ("Kino Free Cinema", "Kino Free Cinema", "A0560"),
    ("Open Air im Hof", "Open Air im Hof", "A2865"),
    ("Kinopalast am Rheincenter", "Kinopalast am Rheincenter", "A0821"),
    ("Open Air Kieswerk", "Open Air Kieswerk", "A2516"),
    ("Rheinflimmern", "Rheinflimmern", "A0032"),
    ("Scala Schopfheim", "Scala Schopfheim", "A1107"),
    ("Centra-Theater Müllheim", "Centra-Theater Müllheim", "A0207"),
    ("Kino Im Stadthaus", "Kino Im Stadthaus", "A1486"),
    ("Kino Kandern", "Kino Kandern", "A0014")
  ))
  private def r_memmingen: R = ("memmingen", "Memmingen", "Bayern", 47.98372, 10.18527, Seq("Memmingen", "Kaufbeuren", "Leutkirch im Allgäu", "Kempten im Allgäu", "Krumbach"), Seq(
    ("Cineplex Memmingen", "Cineplex Memmingen", "A2113"),
    ("Kaminwerk", "Kaminwerk", "A2848"),
    ("Rex Palast Memmingen", "Rex Palast Memmingen", "G0GK4"),
    ("CICO Kaufbeuren", "CICO Kaufbeuren", "A0422"),
    ("Corona KinoPlex - Open Air", "Corona KinoPlex - Open Air", "A2893"),
    ("Melodrom-Filmtheater", "Melodrom-Filmtheater", "A1558"),
    ("Centraltheater", "Centraltheater", "A0205"),
    ("Open-Air-Kino im Museumshof", "Open-Air-Kino im Museumshof", "A2548"),
    ("Colosseum-Center", "Colosseum-Center", "A1382"),
    ("CinePark Krumbach (Schwaben)", "CinePark Krumbach (Schwaben)", "A0299")
  ))
  private def r_noerdlingen: R = ("noerdlingen", "Nördlingen", "Bayern", 48.85122, 10.48868, Seq("Nördlingen", "Aalen", "Dillingen an der Donau", "Neresheim", "Wemding", "Dischingen"), Seq(
    ("Movieworld Kino Nördlingen", "Movieworld Kino Nördlingen", "A2630"),
    ("Open Air Kino Ochsenzwinger Nördlingen", "Open Air Kino Ochsenzwinger Nördlingen", "A2484"),
    ("Ries Theater", "Ries Theater", "A1078"),
    ("Kino am Kocher", "Kino am Kocher", "A2023"),
    ("Kinopark Aalen", "Kinopark Aalen", "A0823"),
    ("Autokino Dillingen", "Autokino Dillingen", "A2926"),
    ("Filmcenter Dillingen", "Filmcenter Dillingen", "A0492"),
    ("Kinomobil Stuttgart - Härtsfeldhalle", "Kinomobil Stuttgart - Härtsfeldhalle", "A0817"),
    ("Wemdinger Lichtspiele", "Wemdinger Lichtspiele", "A1275"),
    ("Open-Air Kino Ballmertshofer Filmfest", "Open-Air Kino Ballmertshofer Filmfest", "A2285")
  ))
  private def r_bautzen: R = ("bautzen", "Bautzen", "Sachsen", 51.18035, 14.43494, Seq("Bautzen", "Hoyerswerda", "Neustadt in Sachsen"), Seq(
    ("Filmpalast Bautzen", "Filmpalast Bautzen", "A0507"),
    ("Freiluftkino am Spreebogen Bautzen", "Freiluftkino am Spreebogen Bautzen", "A2697"),
    ("Open Air Kino im Hof der Ortenburg", "Open Air Kino im Hof der Ortenburg", "A2237"),
    ("Open-Air Kino im Freihof Gedenkstätte", "Open-Air Kino im Freihof Gedenkstätte", "A2827"),
    ("Steinhaus", "Steinhaus", "A2231"),
    ("CineMotion Hoyerswerda", "CineMotion Hoyerswerda", "A0363"),
    ("Kulturfabrik Hoyerswerda e. V.", "Kulturfabrik Hoyerswerda e. V.", "A2624"),
    ("Open Air an der schwarzen Mühle", "Open Air an der schwarzen Mühle", "A2526"),
    ("Grenzland Lichtspiele", "Grenzland Lichtspiele", "A0592")
  ))
  private def r_rostock: R = ("rostock", "Rostock", "Mecklenburg-Vorpommern", 54.0887, 12.14049, Seq("Rostock", "Güstrow", "Kühlungsborn", "Bad Doberan"), Seq(
    ("CineStar Rostock - Capitol", "CineStar Rostock - Capitol", "A0370"),
    ("CineStar Rostock - Lütten Klein", "CineStar Rostock - Lütten Klein", "A0358"),
    ("Lichtspieltheater Wundervoll (Frieda 23)", "Lichtspieltheater Wundervoll (Frieda 23)", "A2649"),
    ("Lichtspieltheater Wundervoll (Metropol)", "Lichtspieltheater Wundervoll (Metropol)", "A0914"),
    ("Movie Star Gustrow", "Movie Star Gustrow", "A0972"),
    ("Sommerkino Güstrow", "Sommerkino Güstrow", "A2933"),
    ("Open Air Strandkorbkino am Bootshafen", "Open Air Strandkorbkino am Bootshafen", "A2059"),
    ("Ostseekino Kühlungsborn", "Ostseekino Kühlungsborn", "A1484"),
    ("Kino- und Kulturverein", "Kino- und Kulturverein", "A0643")
  ))
  private def r_trier: R = ("trier", "Trier", "Rheinland-Pfalz", 49.75565, 6.63935, Seq("Trier", "Merzig", "Wittlich", "Wadern", "Nonnweiler"), Seq(
    ("Broadway Trier", "Broadway Trier", "A0109"),
    ("CineAStA", "CineAStA", "A2728"),
    ("CinemaxX Trier", "CinemaxX Trier", "A0288"),
    ("Open-Air-Kino im TuFa-Hof", "Open-Air-Kino im TuFa-Hof", "A2829"),
    ("Kino am Seffersbach", "Kino am Seffersbach", "A2800"),
    ("Odeon Kino Merzig", "Odeon Kino Merzig", "A0679"),
    ("Kinopalast Eifel Mosel Hunsrück", "Kinopalast Eifel Mosel Hunsrück", "G011D"),
    ("Starlight-Kino", "Starlight-Kino", "A1150"),
    ("Central-Filmtheater Nonnweiler", "Central-Filmtheater Nonnweiler", "A0190")
  ))
  private def r_aschaffenburg: R = ("aschaffenburg", "Aschaffenburg", "Bayern", 49.97704, 9.15214, Seq("Aschaffenburg", "Büdingen", "Gelnhausen", "Seligenstadt", "Babenhausen", "Erlenbach am Main"), Seq(
    ("Casino Aschaffenburg", "Casino Aschaffenburg", "A1342"),
    ("Kinopolis Aschaffenburg", "Kinopolis Aschaffenburg", "A0827"),
    ("OPEN AIR auf dem Campus FH", "OPEN AIR auf dem Campus FH", "A1959"),
    ("Open-Air-Kino im Nilkheimer Park", "Open-Air-Kino im Nilkheimer Park", "A2813"),
    ("Novum Kino", "Novum Kino", "A0112"),
    ("Kino Gelnhausen (Pali und Casino)", "Kino Gelnhausen (Pali und Casino)", "A1028"),
    ("Turmpalast", "Turmpalast", "A1650"),
    ("Passage Erlenbach am Main", "Passage Erlenbach am Main", "A1032")
  ))
  private def r_goerlitz: R = ("goerlitz", "Görlitz", "Sachsen", 51.15518, 14.98853, Seq("Görlitz", "Zittau", "Mittelherwigsdorf", "Rietschen"), Seq(
    ("Camillo-Sommerkino im Rathaushof", "Camillo-Sommerkino im Rathaushof", "A2090"),
    ("CamilloKino", "CamilloKino", "A0138"),
    ("Filmpalast Görlitz", "Filmpalast Görlitz", "A0510"),
    ("Offkino Klappe die Zweite", "Offkino Klappe die Zweite", "A2142"),
    ("Filmpalast Zittau", "Filmpalast Zittau", "A0509"),
    ("Kronenkino", "Kronenkino", "A2685"),
    ("Zittauer Filmnächte", "Zittauer Filmnächte", "A2491"),
    ("Kulturfabrik Meda", "Kulturfabrik Meda", "A2710"),
    ("Kino-Cafe", "Kino-Cafe", "A0759")
  ))
  private def r_goettingen: R = ("goettingen", "Göttingen", "Niedersachsen", 51.53443, 9.93228, Seq("Göttingen", "Einbeck", "Northeim", "Duderstadt", "Witzenhausen", "Bad Sooden-Allendorf"), Seq(
    ("CinemaxX Göttingen", "CinemaxX Göttingen", "A1293"),
    ("Lumière", "Lumière", "A1548"),
    ("Open Air Kino im Freibad", "Open Air Kino im Freibad", "A1788"),
    ("Deli Kino", "Deli Kino", "A1388"),
    ("Welt -Theater", "Welt -Theater", "A1271"),
    ("Neue Schauburg Northeim", "Neue Schauburg Northeim", "A0984"),
    ("Movietown Eichsfeld", "Movietown Eichsfeld", "A1624"),
    ("Capitol-Kino", "Capitol-Kino", "A1335"),
    ("Kurtheater Bad Sooden-Allendorf", "Kurtheater Bad Sooden-Allendorf", "A0890")
  ))
  private def r_rheine: R = ("rheine", "Rheine", "Nordrhein-Westfalen", 52.28509, 7.44055, Seq("Rheine", "Lingen", "Nordhorn", "Ibbenbüren", "Gronau", "Emsdetten", "Steinfurt"), Seq(
    ("Cinetech das Erlebniskino Rheine", "Cinetech das Erlebniskino Rheine", "A0383"),
    ("Zinema City", "Zinema City", "A0398"),
    ("Central-Kino Emsland (Lingen)", "Central-Kino Emsland (Lingen)", "A0200"),
    ("Filmpalast Cine-World", "Filmpalast Cine-World", "G02Q9"),
    ("UCI LUXE Nordhorn", "UCI LUXE Nordhorn", "A2895"),
    ("Apollo Kino Center", "Apollo Kino Center", "A0045"),
    ("Gronauer-Lichtspiele", "Gronauer-Lichtspiele", "A1446"),
    ("Metropolis Kino Emsdetten", "Metropolis Kino Emsdetten", "A2809"),
    ("Kino Steinfurt", "Kino Steinfurt", "A0551")
  ))
  private def r_plauen: R = ("plauen", "Plauen", "Sachsen", 50.4973, 12.13782, Seq("Plauen", "Hof", "Greiz", "Auerbach", "Oelsnitz", "Schleiz", "Markneukirchen"), Seq(
    ("Capitol-Kino Plauen", "Capitol-Kino Plauen", "A0156"),
    ("Malzhaus", "Malzhaus", "A2375"),
    ("Central-Kino Hof", "Central-Kino Hof", "A1346"),
    ("Scala Filmtheater Hof", "Scala Filmtheater Hof", "A1051"),
    ("UT99 Kinocenter", "UT99 Kinocenter", "A1250"),
    ("Rekord-Lichtspiele", "Rekord-Lichtspiele", "A1058"),
    ("Open-Air-Kino - Waldbühne Neuwürschnitz", "Open-Air-Kino - Waldbühne Neuwürschnitz", "A1846"),
    ("Neues Kino im Hörsaal", "Neues Kino im Hörsaal", "A0989"),
    ("Harmonie Lichtspiele Markneukirchen", "Harmonie Lichtspiele Markneukirchen", "A0600")
  ))
  private def r_crailsheim: R = ("crailsheim", "Crailsheim", "Baden-Württemberg", 49.13444, 10.07193, Seq("Crailsheim", "Ellwangen", "Künzelsau", "Feuchtwangen", "Rothenburg", "Schrozberg", "Ilshofen", "Kirchberg an der Jagst"), Seq(
    ("Cinecity", "Cinecity", "A0224"),
    ("Kammer-Filmtheater Premium-Kino Crailsheim", "Kammer-Filmtheater Premium-Kino Crailsheim", "A0641"),
    ("Kino Regina Ellwangen", "Kino Regina Ellwangen", "A1590"),
    ("Prestige", "Prestige", "A1040"),
    ("KulturKino Feuchtwangen", "KulturKino Feuchtwangen", "A1054"),
    ("Filmpalast im Forum", "Filmpalast im Forum", "A2646"),
    ("Open Air Schrozberg", "Open Air Schrozberg", "A2701"),
    ("Kinomobil Stuttgart - Roland-Wurmthaler-Halle", "Kinomobil Stuttgart - Roland-Wurmthaler-Halle", "A0789"),
    ("Kino Klappe", "Kino Klappe", "A0744")
  ))
  private def r_bad_aibling: R = ("bad-aibling", "Bad Aibling", "Bayern", 47.8638, 12.01055, Seq("Bad Aibling", "Wasserburg am Inn", "Rosenheim", "Grafing bei München", "Ebersberg", "Hausham", "Bad Endorf"), Seq(
    ("Aibvision Filmtheater und Lindenkino", "Aibvision Filmtheater und Lindenkino", "A1543"),
    ("Open Air am B&amp;O Parkhotel", "Open Air am B&amp;O Parkhotel", "A2862"),
    ("Kino Utopia", "Kino Utopia", "A1658"),
    ("Open Air im STrandpark Wasserburg", "Open Air im STrandpark Wasserburg", "A2561"),
    ("Kinopolis Rosenheim", "Kinopolis Rosenheim", "A0401"),
    ("Capitol Theater Grafing bei München", "Capitol Theater Grafing bei München", "A0159"),
    ("Kino im Alten Kino", "Kino im Alten Kino", "A2850"),
    ("Oberland Kinocenter", "Oberland Kinocenter", "A1001"),
    ("Marias Kino", "Marias Kino", "A1555")
  ))
  private def r_osnabrueck: R = ("osnabrueck", "Osnabrück", "Niedersachsen", 52.27264, 8.0498, Seq("Osnabrück", "Bramsche", "Ankum"), Seq(
    ("Cinema-Arthouse", "Cinema-Arthouse", "A0257"),
    ("Filmtheater Hasetor", "Filmtheater Hasetor", "A1431"),
    ("HALL OF FAME - Kino de Luxe", "HALL OF FAME - Kino de Luxe", "A0348"),
    ("Kino in der Lagerhalle", "Kino in der Lagerhalle", "A0739"),
    ("Open Air Kino im Innenhof der Domschule Giro Live", "Open Air Kino im Innenhof der Domschule Giro Live", "A1961"),
    ("Open-Air-Kino Schloss-Innenhof", "Open-Air-Kino Schloss-Innenhof", "A2504"),
    ("Universum e.V.", "Universum e.V.", "A1655"),
    ("Gloria Kinocenter", "Gloria Kinocenter", "A0576")
  ))
  private def r_muenster: R = ("muenster", "Münster", "Nordrhein-Westfalen", 51.96236, 7.62571, Seq("Münster", "Munster", "Ahlen", "Dülmen", "Coesfeld"), Seq(
    ("Cinema &amp; Kurbelkiste Münster", "Cinema &amp; Kurbelkiste Münster", "A0243"),
    ("Kaisersaal-Lichtspiele", "Kaisersaal-Lichtspiele", "A0633"),
    ("Schlosstheater Münster", "Schlosstheater Münster", "A1130"),
    ("Cineplex Münster", "Cineplex Münster", "A0305"),
    ("Kino Deutsches Haus", "Kino Deutsches Haus", "A0908"),
    ("CinemAhlen", "CinemAhlen", "A2879"),
    ("Cinema Coesfeld (Dülmen)", "Cinema Coesfeld (Dülmen)", "A0249"),
    ("Cinema Coesfeld (Coesfeld)", "Cinema Coesfeld (Coesfeld)", "A0229")
  ))
  private def r_erfurt: R = ("erfurt", "Erfurt", "Thüringen", 50.97734, 11.03536, Seq("Erfurt", "Gotha", "Ilmenau"), Seq(
    ("CineStar Erfurt", "CineStar Erfurt", "A0344"),
    ("Kinoklub am Hirschlachufer", "Kinoklub am Hirschlachufer", "A0778"),
    ("Open Air Kino egapark", "Open Air Kino egapark", "A2261"),
    ("Open Air im Kulturhof Krönbacken", "Open Air im Kulturhof Krönbacken", "A1829"),
    ("Cineplex Gotha", "Cineplex Gotha", "A2675"),
    ("Kulturhaus Gotha", "Kulturhaus Gotha", "A0726"),
    ("Hochschulfilmclub TU Ilmenau", "Hochschulfilmclub TU Ilmenau", "A0610"),
    ("Linden Lichtspiele", "Linden Lichtspiele", "A0917")
  ))
  private def r_ulm: R = ("ulm", "Ulm", "Baden-Württemberg", 48.39841, 9.99155, Seq("Ulm", "Neu-Ulm", "Ehingen", "Günzburg", "Sontheim", "Offingen"), Seq(
    ("Mephisto Ulm", "Mephisto Ulm", "A1559"),
    ("Obscura Ulm", "Obscura Ulm", "A0692"),
    ("Xinedome", "Xinedome", "A1285"),
    ("Dietrich Theater Neu-Ulm", "Dietrich Theater Neu-Ulm", "A1741"),
    ("Central Kino Center", "Central Kino Center", "A0187"),
    ("Kino Biigz", "Kino Biigz", "A2653"),
    ("Kino in der Dampfsäg", "Kino in der Dampfsäg", "A2588"),
    ("Donaulichtspiele", "Donaulichtspiele", "A1395")
  ))
  private def r_koblenz: R = ("koblenz", "Koblenz", "Rheinland-Pfalz", 50.35357, 7.57883, Seq("Koblenz", "Neuwied", "Limburg", "Lahnstein", "Boppard", "Montabaur"), Seq(
    ("Kinopolis Koblenz", "Kinopolis Koblenz", "A0830"),
    ("Odeon-Kinocenter", "Odeon-Kinocenter", "A0062"),
    ("Metropol-Kino-Center", "Metropol-Kino-Center", "A1564"),
    ("Schauburg-Theater", "Schauburg-Theater", "A1124"),
    ("Cineplex Limburg", "Cineplex Limburg", "A0319"),
    ("Kino Lahnstein", "Kino Lahnstein", "A1196"),
    ("Cinema Boppard", "Cinema Boppard", "A1360"),
    ("Capitol-Kinocenter Montabaur", "Capitol-Kinocenter Montabaur", "A0163")
  ))
  private def r_bad_toelz: R = ("bad-toelz", "Bad Tölz", "Bayern", 47.76111, 11.5589, Seq("Bad Tölz", "Wolfratshausen", "Holzkirchen", "Penzberg", "Tutzing", "Rottach-Egern", "Kochel am See"), Seq(
    ("Capitol Filmtheater Bad Tölz", "Capitol Filmtheater Bad Tölz", "A0142"),
    ("Isar-Kinocenter", "Isar-Kinocenter", "A0628"),
    ("Kinocenter Wolfratshausen", "Kinocenter Wolfratshausen", "A1171"),
    ("KULTUR im Oberbräu", "KULTUR im Oberbräu", "A2357"),
    ("Kino P.", "Kino P.", "A0819"),
    ("Kurtheater Tutzing", "Kurtheater Tutzing", "A0885"),
    ("Kino am Tegernsee", "Kino am Tegernsee", "A0093"),
    ("Filmstudio Kochel", "Filmstudio Kochel", "A0532")
  ))
  private def r_minden: R = ("minden", "Minden", "Nordrhein-Westfalen", 52.28953, 8.91455, Seq("Minden", "Bad Oeynhausen", "Lohne", "Löhne", "Espelkamp", "Stadthagen", "Bückeburg", "Rahden"), Seq(
    ("Filmtheater &quot;Die Birke&quot;", "Filmtheater &quot;Die Birke&quot;", "A2798"),
    ("UCI Bad Oeynhausen", "UCI Bad Oeynhausen", "A2666"),
    ("Capitol-Kinocenter Lohne (Oldenburg)", "Capitol-Kinocenter Lohne (Oldenburg)", "A0162"),
    ("Sommernachtskino (Löhne)", "Sommernachtskino (Löhne)", "A2824"),
    ("Elite", "Elite", "A0462"),
    ("Kinocenter Stadthagen", "Kinocenter Stadthagen", "A1632"),
    ("Residenz-Kino-Center", "Residenz-Kino-Center", "A1061"),
    ("Kinocenter Rahden", "Kinocenter Rahden", "A1730")
  ))
  private def r_vechta: R = ("vechta", "Vechta", "Niedersachsen", 52.73064, 8.28968, Seq("Vechta", "Wildeshausen", "Diepholz", "Damme", "Quakenbrück", "Twistringen", "Lemförde", "Quernheim"), Seq(
    ("Schauburg Cineworld Vechta", "Schauburg Cineworld Vechta", "A1118"),
    ("Lindenhof-Lichtspiele", "Lindenhof-Lichtspiele", "A0919"),
    ("Central Diepholz", "Central Diepholz", "A1344"),
    ("Dersa Kino-Center", "Dersa Kino-Center", "A0438"),
    ("Schauburg Filmtheater", "Schauburg Filmtheater", "A1120"),
    ("Filmtheater Twistringen", "Filmtheater Twistringen", "A1358"),
    ("Lichtburg Open Air Kino", "Lichtburg Open Air Kino", "A1865"),
    ("Lichtburg Lemförde", "Lichtburg Lemförde", "A0689")
  ))
  private def r_schwerin: R = ("schwerin", "Schwerin", "Mecklenburg-Vorpommern", 53.62937, 11.41316, Seq("Schwerin", "Ludwigslust", "Wismar"), Seq(
    ("Capitol Kino Schwerin", "Capitol Kino Schwerin", "A0149"),
    ("Kino unterm Dach", "Kino unterm Dach", "A0559"),
    ("Multiplex Mega Movies", "Multiplex Mega Movies", "A0977"),
    ("Open-Air-Kino Schwerin", "Open-Air-Kino Schwerin", "A2190"),
    ("Luna Filmtheater", "Luna Filmtheater", "A1732"),
    ("Open Air Kino am Schweizerhaus", "Open Air Kino am Schweizerhaus", "A1992"),
    ("CineStar Wismar", "CineStar Wismar", "A0361")
  ))
  private def r_cottbus: R = ("cottbus", "Cottbus", "Brandenburg", 51.75769, 14.32888, Seq("Cottbus", "Spremberg", "Senftenberg"), Seq(
    ("Kino in der Stadhalle Cottbus", "Kino in der Stadhalle Cottbus", "A2370"),
    ("Obenkino im Glad-House", "Obenkino im Glad-House", "A1000"),
    ("UCI am Lausitz Park", "UCI am Lausitz Park", "A1204"),
    ("Weltspiegel Cottbus", "Weltspiegel Cottbus", "A1663"),
    ("Spreekino", "Spreekino", "A1143"),
    ("Spremberger Filmnächte", "Spremberger Filmnächte", "A2758"),
    ("Kino am See", "Kino am See", "A1887")
  ))
  private def r_bamberg: R = ("bamberg", "Bamberg", "Bayern", 49.89873, 10.90067, Seq("Bamberg", "Forchheim", "Zeil am Main"), Seq(
    ("Lichtspiel, Kino &amp; Café", "Lichtspiel, Kino &amp; Café", "A1535"),
    ("Odeon Lichtspiel, Kino &amp; Café", "Odeon Lichtspiel, Kino &amp; Café", "A1005"),
    ("Open Air Kino Hainbad", "Open Air Kino Hainbad", "A1983"),
    ("Sommerkino im Schloss Geyerswörth", "Sommerkino im Schloss Geyerswörth", "A2821"),
    ("Kino-Center Forchheim", "Kino-Center Forchheim", "A0034"),
    ("Open Air Kino In der Kaiserpfalz Forchheim", "Open Air Kino In der Kaiserpfalz Forchheim", "A1968"),
    ("Capitol-Theater (Foto Kino Schneyer)", "Capitol-Theater (Foto Kino Schneyer)", "A0166")
  ))
  private def r_landshut: R = ("landshut", "Landshut", "Bayern", 48.52961, 12.16179, Seq("Landshut", "Moosburg", "Dorfen", "Vilsbiburg", "Gangkofen"), Seq(
    ("Filmzentrum e.V. Kinoptikum", "Filmzentrum e.V. Kinoptikum", "A0834"),
    ("Kinopolis Landshut", "Kinopolis Landshut", "A0026"),
    ("Kleines Theater Landshut", "Kleines Theater Landshut", "A2852"),
    ("Rosenhof-Lichtspiele", "Rosenhof-Lichtspiele", "A1606"),
    ("sKino im Jakobmayer", "sKino im Jakobmayer", "A2671"),
    ("Cineplex Vilsbiburg", "Cineplex Vilsbiburg", "A0896"),
    ("Phantasia", "Phantasia", "A2587")
  ))
  private def r_kaiserslautern: R = ("kaiserslautern", "Kaiserslautern", "Rheinland-Pfalz", 49.443, 7.77161, Seq("Kaiserslautern", "Pirmasens", "Landstuhl", "Annweiler am Trifels", "Enkenbach Alsenborn", "Kusel"), Seq(
    ("UCI Kaiserslautern", "UCI Kaiserslautern", "A0669"),
    ("Union - Studio für Filmkunst", "Union - Studio für Filmkunst", "A1226"),
    ("Walhalla Kinocenter", "Walhalla Kinocenter", "A1659"),
    ("Broadway Ramstein-Miesenbach", "Broadway Ramstein-Miesenbach", "A1319"),
    ("Kino Digital im Hohenstaufensaal", "Kino Digital im Hohenstaufensaal", "A2619"),
    ("Provinzkino", "Provinzkino", "A1588"),
    ("Kinett", "Kinett", "A1458")
  ))
  private def r_lueneburg: R = ("lueneburg", "Lüneburg", "Niedersachsen", 53.25122, 10.41548, Seq("Lüneburg", "Uelzen", "Winsen", "Geesthacht", "Schwarzenbek", "Boizenburg"), Seq(
    ("Filmpalast Lüneburg", "Filmpalast Lüneburg", "A0327"),
    ("Scala Kinocenter Lüneburg", "Scala Kinocenter Lüneburg", "A1616"),
    ("Central-Theater Uelzen", "Central-Theater Uelzen", "A0206"),
    ("Kinocenter Winsen", "Kinocenter Winsen", "A1504"),
    ("Kleines Theater Schillerstraße", "Kleines Theater Schillerstraße", "A0851"),
    ("Kino Grimm", "Kino Grimm", "A0695"),
    ("Kino Boizenburg", "Kino Boizenburg", "A0710")
  ))
  private def r_hameln: R = ("hameln", "Hameln", "Niedersachsen", 52.10397, 9.35623, Seq("Hameln", "Rinteln", "Bad Pyrmont", "Holzminden", "Alfeld"), Seq(
    ("Maxx Hameln", "Maxx Hameln", "A1715"),
    ("Sumpfblume", "Sumpfblume", "A2825"),
    ("Kinocenter Rinteln", "Kinocenter Rinteln", "A1503"),
    ("Metropol-Theater Rinteln", "Metropol-Theater Rinteln", "A1566"),
    ("Kronenlichtspiele", "Kronenlichtspiele", "A1525"),
    ("Roxy Filmcentrum", "Roxy Filmcentrum", "A0697"),
    ("Kinowelt Alfeld", "Kinowelt Alfeld", "A1515")
  ))
  private def r_burghausen: R = ("burghausen", "Burghausen", "Bayern", 48.16925, 12.83139, Seq("Burghausen", "Eggenfelden", "Waldkraiburg", "Mühldorf", "Simbach am Inn"), Seq(
    ("Anker-Filmtheater", "Anker-Filmtheater", "A1490"),
    ("Quadroscope", "Quadroscope", "A1147"),
    ("Gerniale Kino Open-Air", "Gerniale Kino Open-Air", "A2200"),
    ("Kinocenter Eggenfelden", "Kinocenter Eggenfelden", "A1312"),
    ("Cineplex Waldkraiburg", "Cineplex Waldkraiburg", "A0386"),
    ("Hollywood am Inn", "Hollywood am Inn", "A1675"),
    ("Bavaria-Kino-Center Simbach", "Bavaria-Kino-Center Simbach", "A1314")
  ))
  private def r_bad_kreuznach: R = ("bad-kreuznach", "Bad Kreuznach", "Rheinland-Pfalz", 49.8414, 7.86713, Seq("Bad Kreuznach", "Bingen am Rhein", "Ingelheim am Rhein", "Alzey", "Geisenheim", "Simmern", "Bad Sobernheim"), Seq(
    ("Cineplex Bad Kreuznach", "Cineplex Bad Kreuznach", "A0308"),
    ("KiKuBi", "KiKuBi", "A0251"),
    ("Casablanca Ingelheim am Rhein", "Casablanca Ingelheim am Rhein", "A0024"),
    ("Bali", "Bali", "A1309"),
    ("Linden Theater Geisenheim", "Linden Theater Geisenheim", "A1542"),
    ("Pro-Winzkino", "Pro-Winzkino", "A1589"),
    ("Rex Bad Sobernheim", "Rex Bad Sobernheim", "A1071")
  ))
  private def r_magdeburg: R = ("magdeburg", "Magdeburg", "Sachsen-Anhalt", 52.13129, 11.63189, Seq("Magdeburg", "Burg"), Seq(
    ("CineStar Magdeburg", "CineStar Magdeburg", "A1259"),
    ("CinemaxX Magdeburg", "CinemaxX Magdeburg", "A0293"),
    ("Kulturzentrum auf dem Moritzhof", "Kulturzentrum auf dem Moritzhof", "A1709"),
    ("Oli Lichtspiele", "Oli Lichtspiele", "A1708"),
    ("Studio-Kino", "Studio-Kino", "A1165"),
    ("Burg-Theater Burg", "Burg-Theater Burg", "A0124")
  ))
  private def r_marburg: R = ("marburg", "Marburg", "Hessen", 50.80904, 8.77069, Seq("Marburg", "Dillenburg", "Schwalmstadt"), Seq(
    ("Capitol Marburg", "Capitol Marburg", "A1332"),
    ("Cineplex Marburg", "Cineplex Marburg", "A0320"),
    ("Kino Cafe Trauma", "Kino Cafe Trauma", "A0714"),
    ("Open Air Kino Marburg", "Open Air Kino Marburg", "A1855"),
    ("Movie Star Dillenburg", "Movie Star Dillenburg", "A0579"),
    ("Burgtheater Treysa", "Burgtheater Treysa", "A0123")
  ))
  private def r_oldenburg: R = ("oldenburg", "Oldenburg", "Niedersachsen", 53.14039, 8.21479, Seq("Oldenburg", "Oldenburg (Oldenburg)", "Cloppenburg", "Zetel"), Seq(
    ("Casablanca-Programmkino", "Casablanca-Programmkino", "A0097"),
    ("CinemaxX Oldenburg", "CinemaxX Oldenburg", "A0281"),
    ("Studentisches Kino Gegenlicht", "Studentisches Kino Gegenlicht", "A0569"),
    ("Cine K Oldenburg", "Cine K Oldenburg", "A0220"),
    ("CineCenter Cloppenburg", "CineCenter Cloppenburg", "A1354"),
    ("Zeli - Zeteler Lichtspiele e.V.", "Zeli - Zeteler Lichtspiele e.V.", "A1660")
  ))
  private def r_fulda: R = ("fulda", "Fulda", "Hessen", 50.55162, 9.67518, Seq("Fulda", "Schlüchtern", "Lauterbach", "Bad Brückenau"), Seq(
    ("CineStar Fulda", "CineStar Fulda", "A0366"),
    ("Kinoinitiative 35", "Kinoinitiative 35", "A2661"),
    ("Museumscafé", "Museumscafé", "A2860"),
    ("KUKI Kino", "KUKI Kino", "A0875"),
    ("Lichtspielhaus Lauterbach (Hessen)", "Lichtspielhaus Lauterbach (Hessen)", "A0911"),
    ("Rhön-Lichtspiele", "Rhön-Lichtspiele", "A2314")
  ))
  private def r_passau: R = ("passau", "Passau", "Bayern", 48.5665, 13.43122, Seq("Passau", "Bad Füssing", "Freyung"), Seq(
    ("Cineplex Passau", "Cineplex Passau", "A0322"),
    ("ProLi Cinema", "ProLi Cinema", "G011E"),
    ("ScharfrichterKino", "ScharfrichterKino", "A1622"),
    ("Filmgalerie Bad Füssing", "Filmgalerie Bad Füssing", "A1417"),
    ("Kino im großen Kurhaus", "Kino im großen Kurhaus", "A1482"),
    ("Cineplex Freyung", "Cineplex Freyung", "A2642")
  ))
  private def r_ansbach: R = ("ansbach", "Ansbach", "Bayern", 49.30481, 10.5931, Seq("Ansbach", "Neustadt an der Aisch", "Bad Windsheim", "Grosshabersdorf"), Seq(
    ("Capitol Kinocenter", "Capitol Kinocenter", "A1338"),
    ("Kammerspiele Ansbach", "Kammerspiele Ansbach", "A1733"),
    ("Kultur am Schloss", "Kultur am Schloss", "A1628"),
    ("Kino NEA", "Kino NEA", "A2657"),
    ("Open Air im Freilandmuseum", "Open Air im Freilandmuseum", "A1828"),
    ("Lichtspiele Großhabersdorf", "Lichtspiele Großhabersdorf", "A1365")
  ))
  private def r_paderborn: R = ("paderborn", "Paderborn", "Nordrhein-Westfalen", 51.71905, 8.75439, Seq("Paderborn", "Paderborn (Kernstadt)", "Bad Driburg", "Brakel", "Bad Lippspringe"), Seq(
    ("Pollux by Cineplex Paderborn", "Pollux by Cineplex Paderborn", "A0315"),
    ("UCI Paderborn", "UCI Paderborn", "A0832"),
    ("Programmkino Lichtblick e.V.", "Programmkino Lichtblick e.V.", "A1044"),
    ("Kino Bad Driburg", "Kino Bad Driburg", "A0706"),
    ("Kino Brakel", "Kino Brakel", "A0711"),
    ("Odins Filmtheater", "Odins Filmtheater", "A1008")
  ))
  private def r_traunstein: R = ("traunstein", "Traunstein", "Bayern", 47.86825, 12.64335, Seq("Traunstein", "Traunreut", "Bad Reichenhall", "Trostberg", "Prien am Chiemsee"), Seq(
    ("Cine Chiemgau Traunstein", "Cine Chiemgau Traunstein", "A0177"),
    ("Kinos am Bahnhof", "Kinos am Bahnhof", "A0136"),
    ("Cine Chiemgau Traunreut", "Cine Chiemgau Traunreut", "A1191"),
    ("Park-Kino", "Park-Kino", "A1585"),
    ("Stadtkino Trostberg", "Stadtkino Trostberg", "A1620"),
    ("Mikes Kino", "Mikes Kino", "A0961")
  ))
  private def r_brandenburg: R = ("brandenburg", "Brandenburg", "Brandenburg", 52.41667, 12.55, Seq("Brandenburg", "Brandenburg an der Havel", "Rathenow", "Werder", "Genthin", "Bad Belzig"), Seq(
    ("Concerthaus Kino", "Concerthaus Kino", "A0421"),
    ("Fontane Klub", "Fontane Klub", "A1930"),
    ("Haveltor Kino", "Haveltor Kino", "A0603"),
    ("Scala Kulturpalast Werder", "Scala Kulturpalast Werder", "A0555"),
    ("Union Kino Genthin", "Union Kino Genthin", "A1239"),
    ("Hofgarten Kino", "Hofgarten Kino", "A0611")
  ))
  private def r_altenburg: R = ("altenburg", "Altenburg", "Thüringen", 50.98763, 12.43684, Seq("Altenburg", "Zeitz", "Glauchau", "Werdau", "Borna", "Geithain"), Seq(
    ("Capitol Altenburg", "Capitol Altenburg", "A0143"),
    ("Brühl Cinema Zeitz", "Brühl Cinema Zeitz", "A2637"),
    ("Clubkino Glauchau e.V.", "Clubkino Glauchau e.V.", "A2315"),
    ("Auto- und Freilichtkino Langenhessen", "Auto- und Freilichtkino Langenhessen", "A2695"),
    ("Volksplatz Borna Open Air Kino", "Volksplatz Borna Open Air Kino", "A1804"),
    ("Kino im Bürgerhaus Geithain", "Kino im Bürgerhaus Geithain", "A2944")
  ))
  private def r_riesa: R = ("riesa", "Riesa", "Sachsen", 51.30777, 13.29168, Seq("Riesa", "Meissen", "Döbeln", "Torgau", "Grossenhain", "Gröditz"), Seq(
    ("Filmpalast Capital Riesa", "Filmpalast Capital Riesa", "A0515"),
    ("Filmpalast Meißen", "Filmpalast Meißen", "A0371"),
    ("Cinema Döbeln", "Cinema Döbeln", "A0215"),
    ("KAP-Torgau e.V.", "KAP-Torgau e.V.", "A0646"),
    ("Filmgalerie Großenhain", "Filmgalerie Großenhain", "A0496"),
    ("Castello", "Castello", "A0182")
  ))
  private def r_sigmaringen: R = ("sigmaringen", "Sigmaringen", "Baden-Württemberg", 48.08829, 9.23033, Seq("Sigmaringen", "Riedlingen", "Mengen", "Gammertingen", "Emmingen-Liptingen", "Kolbingen"), Seq(
    ("Hof-Theater", "Hof-Theater", "A0613"),
    ("Lichtspielhaus Riedlingen", "Lichtspielhaus Riedlingen", "A1148"),
    ("Kinocenter Mengen", "Kinocenter Mengen", "A1502"),
    ("Open Air", "Open Air", "A2772"),
    ("Kinomobil Stuttgart - Musiksaal der Witthohschule", "Kinomobil Stuttgart - Musiksaal der Witthohschule", "A1941"),
    ("Kinomobil Stuttgart - Kolbingen Dorfplatz", "Kinomobil Stuttgart - Kolbingen Dorfplatz", "A0811")
  ))
  private def r_anklam: R = ("anklam", "Anklam", "Mecklenburg-Vorpommern", 53.85637, 13.68965, Seq("Anklam", "Ueckermünde", "Zinnowitz", "Heringsdorf", "Koserow", "Zempin"), Seq(
    ("Kino-Center Anklam", "Kino-Center Anklam", "A0703"),
    ("Volksbühne", "Volksbühne", "A1262"),
    ("Club-Kino", "Club-Kino", "A0413"),
    ("Sommerkino Heringsdorf", "Sommerkino Heringsdorf", "A0751"),
    ("Autokino Usedom", "Autokino Usedom", "A0074"),
    ("Sommerkino Zempin (SAK)", "Sommerkino Zempin (SAK)", "A2092")
  ))
  private def r_altensteig: R = ("altensteig", "Altensteig", "Baden-Württemberg", 48.58649, 8.60395, Seq("Altensteig", "Bad Wildbad", "Schömberg", "Althengstett", "Lossburg", "Haiterbach"), Seq(
    ("Open-Air Kino im Schlossgarten", "Open-Air Kino im Schlossgarten", "A1934"),
    ("KiWi-Kino", "KiWi-Kino", "A0847"),
    ("Kurtheater Schömberg", "Kurtheater Schömberg", "A0888"),
    ("Kinomobil Stuttgart - Festhalle Althengstett", "Kinomobil Stuttgart - Festhalle Althengstett", "A0799"),
    ("Kino unterm Sternenhimmel Loßburg", "Kino unterm Sternenhimmel Loßburg", "A2083"),
    ("Kinomobil Stuttgart - Festhalle Haiterbach", "Kinomobil Stuttgart - Festhalle Haiterbach", "A0804")
  ))
  private def r_celle: R = ("celle", "Celle", "Niedersachsen", 52.62264, 10.08047, Seq("Celle", "Burgdorf", "Wathlingen"), Seq(
    ("Achteinhalb", "Achteinhalb", "A0005"),
    ("Kammer-Lichtspiele / Filmpalast Celle", "Kammer-Lichtspiele / Filmpalast Celle", "A0638"),
    ("Open Air Kino", "Open Air Kino", "A2869"),
    ("Neue Schauburg Burgdorf", "Neue Schauburg Burgdorf", "A1572"),
    ("Kleines Kino Wathlingen", "Kleines Kino Wathlingen", "A2709")
  ))
  private def r_neubrandenburg: R = ("neubrandenburg", "Neubrandenburg", "Mecklenburg-Vorpommern", 53.55735, 13.26105, Seq("Neubrandenburg", "Neustrelitz", "Feldberger Seenlandschaft"), Seq(
    ("CineStar Neubrandenburg", "CineStar Neubrandenburg", "A0331"),
    ("Latücht - Kommunales Kino", "Latücht - Kommunales Kino", "A0894"),
    ("Basiskulturfabrik", "Basiskulturfabrik", "A0084"),
    ("Movie Star Neustrelitz", "Movie Star Neustrelitz", "A2333"),
    ("Clubkino Feldberg", "Clubkino Feldberg", "A2761")
  ))
  private def r_dessau_rosslau: R = ("dessau-rosslau", "Dessau-Roßlau", "Sachsen-Anhalt", 51.83864, 12.24555, Seq("Dessau-Roßlau", "Dessau", "Lutherstadt Wittenberg", "Wittenberg"), Seq(
    ("Kiez-Kino", "Kiez-Kino", "A2849"),
    ("Open Air am Landhaus Dessau", "Open Air am Landhaus Dessau", "A2702"),
    ("UCI Dessau", "UCI Dessau", "A2890"),
    ("Sommerkino auf dem Cranach-Hof", "Sommerkino auf dem Cranach-Hof", "A2507"),
    ("Centralkino Wittenberg", "Centralkino Wittenberg", "A0209")
  ))
  private def r_emden: R = ("emden", "Emden", "Niedersachsen", 53.36592, 7.20846, Seq("Emden", "Aurich", "Papenburg", "Leer"), Seq(
    ("CineStar Emden", "CineStar Emden", "A0349"),
    ("Sommernachtskino im Van-Ameren Bad", "Sommernachtskino im Van-Ameren Bad", "A1859"),
    ("Kino Aurich", "Kino Aurich", "A2911"),
    ("Kino Papenburg", "Kino Papenburg", "A0466"),
    ("Kino Leer (Ostfriesland)", "Kino Leer (Ostfriesland)", "A0675")
  ))
  private def r_waldshut_tiengen: R = ("waldshut-tiengen", "Waldshut-Tiengen", "Baden-Württemberg", 47.62323, 8.21717, Seq("Waldshut-Tiengen", "Bad Säckingen", "Lenzkirch", "Sankt Blasien"), Seq(
    ("Albrecht Kino Waldshut", "Albrecht Kino Waldshut", "A2786"),
    ("Open Air-Kino Waldshut", "Open Air-Kino Waldshut", "A2225"),
    ("Gloria Bad Säckingen", "Gloria Bad Säckingen", "A1438"),
    ("Kultur im Kino", "Kultur im Kino", "A0889"),
    ("Kino im Kursaal", "Kino im Kursaal", "A2802")
  ))
  private def r_bad_urach: R = ("bad-urach", "Bad Urach", "Baden-Württemberg", 48.49107, 9.40009, Seq("Bad Urach", "Nürtingen", "Metzingen", "Ensingen"), Seq(
    ("Bad Uracher Sommer Open Air", "Bad Uracher Sommer Open Air", "A2534"),
    ("Forum 22", "Forum 22", "A1731"),
    ("Traumpalast Nürtingen", "Traumpalast Nürtingen", "A1511"),
    ("Luna-Filmtheater", "Luna-Filmtheater", "A1549"),
    ("Kinomobil Stuttgart - BIO-Café Zimt und Zunder", "Kinomobil Stuttgart - BIO-Café Zimt und Zunder", "A0814")
  ))
  private def r_frankfurt_an_der_oder: R = ("frankfurt-an-der-oder", "Frankfurt an der Oder", "Brandenburg", 52.34714, 14.55062, Seq("Frankfurt an der Oder", "Fürstenwalde", "Fürstenwalde an der Spree", "Beeskow", "Bad Saarow"), Seq(
    ("Kleines Kino", "Kleines Kino", "A2371"),
    ("Filmtheater Union", "Filmtheater Union", "A2383"),
    ("Open Air Kino Parkbühne", "Open Air Kino Parkbühne", "A2449"),
    ("Schukurama", "Schukurama", "A2366"),
    ("Cinema by Velotel", "Cinema by Velotel", "G0119")
  ))
  private def r_amberg: R = ("amberg", "Amberg", "Bayern", 49.44287, 11.86267, Seq("Amberg", "Weiden in der Oberpfalz", "Schwandorf", "Sulzbach", "Sulzbach-Rosenberg"), Seq(
    ("Cineplex Amberg", "Cineplex Amberg", "A2686"),
    ("Neue Welt Kinocenter", "Neue Welt Kinocenter", "A0985"),
    ("Lichtwerk Kino Schwandorf", "Lichtwerk Kino Schwandorf", "A2096"),
    ("Kinopolis Main-Taunus", "Kinopolis Main-Taunus", "A0831"),
    ("Lu-Li Sommernachtskino", "Lu-Li Sommernachtskino", "A2533")
  ))
  private def r_straubing: R = ("straubing", "Straubing", "Bayern", 48.88126, 12.57385, Seq("Straubing", "Deggendorf", "Dingolfing", "Plattling", "Wörth an der Donau"), Seq(
    ("Citydom", "Citydom", "A0400"),
    ("Lichtspielhaus Deggendorf", "Lichtspielhaus Deggendorf", "A0910"),
    ("Cinema Filmpalais", "Cinema Filmpalais", "A1363"),
    ("Focus Cinemas Plattling", "Focus Cinemas Plattling", "A0552"),
    ("Donau-Lichtspiele", "Donau-Lichtspiele", "A1396")
  ))
  private def r_suhl: R = ("suhl", "Suhl", "Thüringen", 50.60911, 10.69401, Seq("Suhl", "Meiningen", "Schmalkalden", "Zella-Mehlis", "Masserberg"), Seq(
    ("Cineplex Suhl", "Cineplex Suhl", "A0304"),
    ("Casino Lichtspiele Meiningen", "Casino Lichtspiele Meiningen", "A0181"),
    ("Open Air Kino Schloss Wilhelmsburg", "Open Air Kino Schloss Wilhelmsburg", "A2246"),
    ("Clubkino Zella-Mehlis", "Clubkino Zella-Mehlis", "A2753"),
    ("Kino im Badehaus Masserberg", "Kino im Badehaus Masserberg", "A2708")
  ))
  private def r_eisenach: R = ("eisenach", "Eisenach", "Thüringen", 50.9807, 10.31522, Seq("Eisenach", "Mühlhausen", "Eschwege", "Bad Langensalza", "Bad Salzungen"), Seq(
    ("Capitol Eisenach", "Capitol Eisenach", "A0161"),
    ("Filmpalast Central Mühlhausen", "Filmpalast Central Mühlhausen", "A0198"),
    ("Cinemagic Eschwege", "Cinemagic Eschwege", "A0260"),
    ("Burgtheater Bad Langensalza", "Burgtheater Bad Langensalza", "A0121"),
    ("PAB Kinocenter", "PAB Kinocenter", "A0001")
  ))
  private def r_nienburg: R = ("nienburg", "Nienburg", "Niedersachsen", 52.64437, 9.21658, Seq("Nienburg", "Verden an der Aller", "Walsrode", "Sulingen", "Hoya"), Seq(
    ("Filmpalast Nienburg", "Filmpalast Nienburg", "A1468"),
    ("Cine City", "Cine City", "A0219"),
    ("Capitol-Theater Walsrode", "Capitol-Theater Walsrode", "A0167"),
    ("Filmpalast Sulingen", "Filmpalast Sulingen", "A1423"),
    ("Filmhof Hoya", "Filmhof Hoya", "A0500")
  ))
  private def r_korbach: R = ("korbach", "Korbach", "Hessen", 51.27561, 8.873, Seq("Korbach", "Warburg", "Bad Wildungen", "Wolfhagen", "Willingen"), Seq(
    ("Cine K Korbach", "Cine K Korbach", "A2902"),
    ("Cineplex Warburg", "Cineplex Warburg", "A0311"),
    ("Wandelhalle Reinhardshausen", "Wandelhalle Reinhardshausen", "A2726"),
    ("Cinema Wolfhagen", "Cinema Wolfhagen", "A2790"),
    ("Kino Studio Willingen", "Kino Studio Willingen", "A0096")
  ))
  private def r_daun: R = ("daun", "Daun", "Rheinland-Pfalz", 50.19716, 6.82942, Seq("Daun", "Prüm", "Cochem", "Hillesheim", "Nürburg"), Seq(
    ("Kinopalast Vulkaneifel", "Kinopalast Vulkaneifel", "A0822"),
    ("Eifel-Kinocenter", "Eifel-Kinocenter", "A0407"),
    ("Apollo Cochern", "Apollo Cochern", "A0041"),
    ("Eifel-Film-Bühne", "Eifel-Film-Bühne", "A0458"),
    ("ring°kino", "ring°kino", "A2670")
  ))
  private def r_oberstdorf: R = ("oberstdorf", "Oberstdorf", "Bayern", 47.40724, 10.27939, Seq("Oberstdorf", "Immenstadt im Allgäu"), Seq(
    ("Kurfilmtheater Oberstdorf", "Kurfilmtheater Oberstdorf", "A0883"),
    ("LOFT Oberstdorf", "LOFT Oberstdorf", "A0222"),
    ("Open Air Kino zwischen den Schanzen/Erdinger Arena", "Open Air Kino zwischen den Schanzen/Erdinger Arena", "A2551"),
    ("Union Filmtheater Immenstadt im Allgäu", "Union Filmtheater Immenstadt im Allgäu", "A1227")
  ))
  private def r_flensburg: R = ("flensburg", "Flensburg", "Schleswig-Holstein", 54.78805, 9.43722, Seq("Flensburg", "Schleswig", "Kappeln"), Seq(
    ("Kino 51 Stufen", "Kino 51 Stufen", "A1481"),
    ("UCI Flensburg", "UCI Flensburg", "A0829"),
    ("Filmtheater Capitol Schleswig", "Filmtheater Capitol Schleswig", "A1126"),
    ("Capitol Theater Kappeln", "Capitol Theater Kappeln", "A1664")
  ))
  private def r_gummersbach: R = ("gummersbach", "Gummersbach", "Nordrhein-Westfalen", 51.02608, 7.56473, Seq("Gummersbach", "Olpe", "Attendorn"), Seq(
    ("Burgtheater Gummersbach", "Burgtheater Gummersbach", "A0119"),
    ("SEVEN Kinocenter Gummersbach", "SEVEN Kinocenter Gummersbach", "A2912"),
    ("Cineplex Olpe", "Cineplex Olpe", "A2115"),
    ("JAC Kino Attendorn", "JAC Kino Attendorn", "A2914")
  ))
  private def r_garmisch_partenkirchen: R = ("garmisch-partenkirchen", "Garmisch-Partenkirchen", "Bayern", 47.49209, 11.09576, Seq("Garmisch-Partenkirchen", "Murnau am Staffelsee", "Oberammergau"), Seq(
    ("Hochland-Kino", "Hochland-Kino", "A0609"),
    ("Kinocenter Garmisch &amp; Aspen im Lamm", "Kinocenter Garmisch &amp; Aspen im Lamm", "A1508"),
    ("Kino im Griesbräu", "Kino im Griesbräu", "A0593"),
    ("Heimgarten Kino", "Heimgarten Kino", "A1451")
  ))
  private def r_dorsten: R = ("dorsten", "Dorsten", "Nordrhein-Westfalen", 51.66166, 6.96514, Seq("Dorsten", "Gladbeck", "Dinslaken", "Wesel"), Seq(
    ("Central Kinocenter Dorsten", "Central Kinocenter Dorsten", "A2788"),
    ("Kommunales Kino im Studio der Stadtbücherei", "Kommunales Kino im Studio der Stadtbücherei", "A0861"),
    ("Lichtburg Dinslaken", "Lichtburg Dinslaken", "A2882"),
    ("Comet Kinos Wesel", "Comet Kinos Wesel", "A0420")
  ))
  private def r_stralsund: R = ("stralsund", "Stralsund", "Mecklenburg-Vorpommern", 54.30911, 13.0818, Seq("Stralsund", "Greifswald", "Bergen auf Rügen", "Vitte"), Seq(
    ("CineStar Stralsund", "CineStar Stralsund", "A0359"),
    ("CineStar Greifswald", "CineStar Greifswald", "A0362"),
    ("Kino Bergen auf Rügen", "Kino Bergen auf Rügen", "A0709"),
    ("Zeltkino Hiddensee", "Zeltkino Hiddensee", "A2097")
  ))
  private def r_cuxhaven: R = ("cuxhaven", "Cuxhaven", "Niedersachsen", 53.86828, 8.69902, Seq("Cuxhaven", "Brunsbüttel", "Meldorf", "Büsum"), Seq(
    ("Bali-Service-Kino", "Bali-Service-Kino", "A0009"),
    ("Metropol Brunsbüttel 1", "Metropol Brunsbüttel 1", "A1565"),
    ("Deutsches Haus-Lichtspiele", "Deutsches Haus-Lichtspiele", "A1390"),
    ("Lichtblick Büsum", "Lichtblick Büsum", "A2909")
  ))
  private def r_nordhausen: R = ("nordhausen", "Nordhausen", "Thüringen", 51.5018, 10.7957, Seq("Nordhausen", "Sangerhausen", "Sondershausen", "Bleicherode"), Seq(
    ("Filmpalast Nordhausen", "Filmpalast Nordhausen", "A0517"),
    ("Movie Star Sangerhausen", "Movie Star Sangerhausen", "A0188"),
    ("Cinema 64", "Cinema 64", "A2367"),
    ("Filmtheater Bleicherode", "Filmtheater Bleicherode", "A0534")
  ))
  private def r_coburg: R = ("coburg", "Coburg", "Bayern", 50.25937, 10.96384, Seq("Coburg", "Sonneberg", "Lichtenfels", "Kronach"), Seq(
    ("Utopolis", "Utopolis", "A1252"),
    ("Kammer Lichtspiele", "Kammer Lichtspiele", "A0640"),
    ("Neue Filmbühne Lichtenfels", "Neue Filmbühne Lichtenfels", "A0981"),
    ("Filmburg Kronach", "Filmburg Kronach", "A1410")
  ))
  private def r_halberstadt: R = ("halberstadt", "Halberstadt", "Sachsen-Anhalt", 51.89562, 11.05622, Seq("Halberstadt", "Wernigerode", "Quedlinburg", "Thale"), Seq(
    ("Zuckerfabrik Kinopark", "Zuckerfabrik Kinopark", "A1013"),
    ("Volkslichtspiele Wernigerode", "Volkslichtspiele Wernigerode", "A1265"),
    ("Studiokino Eisenstein im Kulturzentrum", "Studiokino Eisenstein im Kulturzentrum", "A1167"),
    ("Central Theater Thale", "Central Theater Thale", "A0208")
  ))
  private def r_meppen: R = ("meppen", "Meppen", "Niedersachsen", 52.69064, 7.29097, Seq("Meppen", "Haren", "Löningen", "Haselünne"), Seq(
    ("Kino Meppen", "Kino Meppen", "A0573"),
    ("Ferienzentrum Schloss Dankern", "Ferienzentrum Schloss Dankern", "A1387"),
    ("Li-Lo-Lichtspiele", "Li-Lo-Lichtspiele", "A0916"),
    ("Hasetor-Lichtspiele", "Hasetor-Lichtspiele", "A2650")
  ))
  private def r_neuruppin: R = ("neuruppin", "Neuruppin", "Brandenburg", 52.92815, 12.80311, Seq("Neuruppin", "Wittstock", "Wusterhausen", "Zempow"), Seq(
    ("Union Filmtheater Neuruppin", "Union Filmtheater Neuruppin", "A0685"),
    ("Astoria Wittstock", "Astoria Wittstock", "A2377"),
    ("Lindenkino", "Lindenkino", "A0920"),
    ("Autokino Zempow", "Autokino Zempow", "A0071")
  ))
  private def r_bad_neuenahr_ahrweiler: R = ("bad-neuenahr-ahrweiler", "Bad Neuenahr-Ahrweiler", "Rheinland-Pfalz", 50.54322, 7.1113, Seq("Bad Neuenahr-Ahrweiler", "Wachtberg", "Mayen", "Asbach"), Seq(
    ("Kino-Center Rhein-Ahr", "Kino-Center Rhein-Ahr", "A1115"),
    ("Drehwerk 1719", "Drehwerk 1719", "A2117"),
    ("Corso Kino", "Corso Kino", "A0424"),
    ("Cine 5", "Cine 5", "A2640")
  ))
  private def r_rotenburg_an_der_wuemme: R = ("rotenburg-an-der-wuemme", "Rotenburg an der Wümme", "Niedersachsen", 53.11032, 9.4036, Seq("Rotenburg an der Wümme", "Soltau", "Schneverdingen", "Zeven"), Seq(
    ("Stadtkino Rotenburg", "Stadtkino Rotenburg", "A2372"),
    ("Gloria Kino Center Soltau", "Gloria Kino Center Soltau", "A1441"),
    ("LichtSpiel e.V. Schneverdingen", "LichtSpiel e.V. Schneverdingen", "A2673"),
    ("Central Theater Zeven", "Central Theater Zeven", "A2354")
  ))
  private def r_salzwedel: R = ("salzwedel", "Salzwedel", "Sachsen-Anhalt", 52.85297, 11.15287, Seq("Salzwedel", "Wittingen", "Lüchow", "Jameln"), Seq(
    ("Filmpalast Salzwedel (Hansestadt)", "Filmpalast Salzwedel (Hansestadt)", "A1347"),
    ("Wittinger Lichtspiele", "Wittinger Lichtspiele", "A0676"),
    ("Kino Alte Brennerei", "Kino Alte Brennerei", "A0699"),
    ("Cafe Grenzbereiche", "Cafe Grenzbereiche", "A0133")
  ))
  private def r_bad_berleburg: R = ("bad-berleburg", "Bad Berleburg", "Nordrhein-Westfalen", 51.05224, 8.39227, Seq("Bad Berleburg", "Hilchenbach", "Bad Laasphe", "Winterberg"), Seq(
    ("Capitol Bad Berleburg", "Capitol Bad Berleburg", "A0165"),
    ("Viktoria Filmtheater", "Viktoria Filmtheater", "A1256"),
    ("Residenztheater", "Residenztheater", "A1063"),
    ("Filmtheater Winterberg", "Filmtheater Winterberg", "A0543")
  ))
  private def r_weissenburg: R = ("weissenburg", "Weissenburg", "Bayern", 49.03095, 10.97221, Seq("Weissenburg", "Gunzenhausen", "Treuchtlingen", "Freystadt"), Seq(
    ("Kinocenter Weißenburg in Bayern", "Kinocenter Weißenburg in Bayern", "A1506"),
    ("Movieworld", "Movieworld", "A0975"),
    ("Central Kino und Kultur Treuchtlingen", "Central Kino und Kultur Treuchtlingen", "A0189"),
    ("Kino im Cafe Restaurant Beck", "Kino im Cafe Restaurant Beck", "A1485")
  ))
  private def r_marktredwitz: R = ("marktredwitz", "Marktredwitz", "Bayern", 50.00443, 12.08593, Seq("Marktredwitz", "Selb", "Tirschenreuth", "Mitterteich"), Seq(
    ("Cineplanet Marktredwitz", "Cineplanet Marktredwitz", "A0870"),
    ("Cineplex Kinocenter Selb", "Cineplex Kinocenter Selb", "A0763"),
    ("Cineplanet Tirschenreuth", "Cineplanet Tirschenreuth", "A0302"),
    ("Angerlichtspiele Mitterteich", "Angerlichtspiele Mitterteich", "A2635")
  ))
  private def r_bremerhaven: R = ("bremerhaven", "Bremerhaven", "Bremen", 53.55357, 8.57553, Seq("Bremerhaven", "Wilhelmshaven", "Brake"), Seq(
    ("CineMotion Bremerhaven", "CineMotion Bremerhaven", "A1773"),
    ("UCI Wilhelmshaven", "UCI Wilhelmshaven", "A0043"),
    ("Centraltheater Brake", "Centraltheater Brake", "A2694")
  ))
  private def r_bayreuth: R = ("bayreuth", "Bayreuth", "Bayern", 49.94782, 11.57893, Seq("Bayreuth", "Kulmbach", "Hollfeld"), Seq(
    ("Cineplex Bayreuth", "Cineplex Bayreuth", "A0314"),
    ("Cineplex Kulmbach", "Cineplex Kulmbach", "A0764"),
    ("Kintopp", "Kintopp", "A1517")
  ))
  private def r_goslar: R = ("goslar", "Goslar", "Niedersachsen", 51.90425, 10.42766, Seq("Goslar", "Herzberg am Harz", "Bad Gandersheim"), Seq(
    ("Cineplex Goslar", "Cineplex Goslar", "A0318"),
    ("Central-Lichtspiele Herzberg am Harz", "Central-Lichtspiele Herzberg am Harz", "A1351"),
    ("Kino Gandeon", "Kino Gandeon", "A2707")
  ))
  private def r_bernburg: R = ("bernburg", "Bernburg", "Sachsen-Anhalt", 51.79464, 11.7401, Seq("Bernburg", "Köthen", "Aschersleben"), Seq(
    ("Filmtheater Capitol Bernburg", "Filmtheater Capitol Bernburg", "A0542"),
    ("Cine Circus", "Cine Circus", "A0218"),
    ("Filmpalast Aschersleben", "Filmpalast Aschersleben", "A0356")
  ))
  private def r_bad_hersfeld: R = ("bad-hersfeld", "Bad Hersfeld", "Hessen", 50.87197, 9.70891, Seq("Bad Hersfeld", "Alsfeld", "Bebra"), Seq(
    ("Cineplex Bad Hersfeld", "Cineplex Bad Hersfeld", "A0765"),
    ("Kinocenter Alsfeld", "Kinocenter Alsfeld", "A1505"),
    ("Biber-Kino-Center", "Biber-Kino-Center", "A0094")
  ))
  private def r_waren_mueritz: R = ("waren-mueritz", "Waren (Müritz)", "Mecklenburg-Vorpommern", 53.51986, 12.68128, Seq("Waren (Müritz)", "Malchin", "Malchow"), Seq(
    ("CineStar Waren (Müritz)", "CineStar Waren (Müritz)", "A2794"),
    ("Filmbühne Malchin", "Filmbühne Malchin", "A0660"),
    ("Kino Malchow", "Kino Malchow", "A0535")
  ))
  private def r_wittenberge: R = ("wittenberge", "Wittenberge", "Brandenburg", 53.00005, 11.74944, Seq("Wittenberge", "Pritzwalk", "Perleberg"), Seq(
    ("Movie Star Wittenberge", "Movie Star Wittenberge", "A0968"),
    ("Kulturhaus Pritzwalk", "Kulturhaus Pritzwalk", "A2625"),
    ("Movie Star Perleberg", "Movie Star Perleberg", "A1234")
  ))
  private def r_marktoberdorf: R = ("marktoberdorf", "Marktoberdorf", "Bayern", 47.77964, 10.61713, Seq("Marktoberdorf", "Füssen", "Schongau"), Seq(
    ("Filmburg - Das Theaterkino", "Filmburg - Das Theaterkino", "A1409"),
    ("Alpenfilmtheater Füssen", "Alpenfilmtheater Füssen", "A1472"),
    ("Lagerhauskino", "Lagerhauskino", "A0892")
  ))
  private def r_erbach_im_odenwald: R = ("erbach-im-odenwald", "Erbach im Odenwald", "Baden-Württemberg", 49.66148, 8.99402, Seq("Erbach im Odenwald", "Höchst im Odenwald", "Miltenberg"), Seq(
    ("Erbacher Lichtspiele", "Erbacher Lichtspiele", "A0690"),
    ("Höchster Lichtspiele", "Höchster Lichtspiele", "A0240"),
    ("Schloß-Theater", "Schloß-Theater", "A1131")
  ))
  private def r_regen: R = ("regen", "Regen", "Bayern", 48.9719, 13.12824, Seq("Regen", "Zwiesel", "Viechtach"), Seq(
    ("CinePalast Regen", "CinePalast Regen", "A1460"),
    ("Filmtheater Zwiesel", "Filmtheater Zwiesel", "A0704"),
    ("Neue Post Lichtspiele", "Neue Post Lichtspiele", "A0983")
  ))
  private def r_bad_schwalbach: R = ("bad-schwalbach", "Bad Schwalbach", "Hessen", 50.14196, 8.06964, Seq("Bad Schwalbach", "Nastätten", "Hahnstätten"), Seq(
    ("Bambi &amp; Camera", "Bambi &amp; Camera", "A1311"),
    ("Kinocenter Nastätten", "Kinocenter Nastätten", "A0774"),
    ("Kreml Kulturhaus", "Kreml Kulturhaus", "A2755")
  ))
  private def r_niebuell: R = ("niebuell", "Niebüll", "Schleswig-Holstein", 54.78663, 8.82854, Seq("Niebüll", "Leck", "Wyk auf Föhr"), Seq(
    ("Eck&#039; s Kino", "Eck&#039; s Kino", "A1697"),
    ("Deli-Kino", "Deli-Kino", "A2368"),
    ("Filmtheater am Sandwall", "Filmtheater am Sandwall", "G011C")
  ))
  private def r_uffenheim: R = ("uffenheim", "Uffenheim", "Bayern", 49.54415, 10.23286, Seq("Uffenheim", "Creglingen", "Burgbernheim"), Seq(
    ("Open Air Rudolzhofen Kino", "Open Air Rudolzhofen Kino", "A2560"),
    ("Kinomobil Stuttgart - Kommunales Kino Creglingen", "Kinomobil Stuttgart - Kommunales Kino Creglingen", "A0800"),
    ("Open Air Kino&quot;Am Kappelenberg&quot;", "Open Air Kino&quot;Am Kappelenberg&quot;", "A2058")
  ))
  private def r_bad_lobenstein: R = ("bad-lobenstein", "Bad Lobenstein", "Thüringen", 50.45223, 11.6393, Seq("Bad Lobenstein", "Wurzbach", "Hirschberg"), Seq(
    ("Kino am Park", "Kino am Park", "A2656"),
    ("Kino Wurzbach", "Kino Wurzbach", "A0758"),
    ("Olympia Hirschberg", "Olympia Hirschberg", "A1580")
  ))
  private def r_langeoog: R = ("langeoog", "Langeoog", "Niedersachsen", 53.74552, 7.48175, Seq("Langeoog", "Spiekeroog", "Baltrum"), Seq(
    ("Windlicht", "Windlicht", "A2757"),
    ("Inselkino Spiekeroog", "Inselkino Spiekeroog", "A2884")
  ))
  private def r_kleve: R = ("kleve", "Kleve", "Schleswig-Holstein", 51.78826, 6.13865, Seq("Kleve", "Goch"), Seq(
    ("Tichelpark Kleve", "Tichelpark Kleve", "A1182"),
    ("Goli Theater Goch", "Goli Theater Goch", "A2597")
  ))
  private def r_luckenwalde: R = ("luckenwalde", "Luckenwalde", "Brandenburg", 52.09029, 13.16772, Seq("Luckenwalde", "Dahme"), Seq(
    ("Union Kino-Center", "Union Kino-Center", "A1233"),
    ("Kino-Cafe Dahme", "Kino-Cafe Dahme", "A0836")
  ))
  private def r_prenzlau: R = ("prenzlau", "Prenzlau", "Brandenburg", 53.31702, 13.86397, Seq("Prenzlau", "Templin"), Seq(
    ("Union Filmtheater Prenzlau", "Union Filmtheater Prenzlau", "A0666"),
    ("Kino im Multikulturellen Centrum", "Kino im Multikulturellen Centrum", "A0016")
  ))
  private def r_husum: R = ("husum", "Husum", "Schleswig-Holstein", 54.4858, 9.05239, Seq("Husum", "Heide"), Seq(
    ("Kino-Center Husum", "Kino-Center Husum", "A0762"),
    ("LichtBlick Heide", "LichtBlick Heide", "A0265")
  ))
  private def r_finsterwalde: R = ("finsterwalde", "Finsterwalde", "Brandenburg", 51.63388, 13.70662, Seq("Finsterwalde", "Schwarzheide"), Seq(
    ("Weltspiegel Finsterwalde", "Weltspiegel Finsterwalde", "A1273"),
    ("Extra Kinowelt", "Extra Kinowelt", "A0477")
  ))
  private def r_cham: R = ("cham", "Cham", "Bayern", 49.22565, 12.65501, Seq("Cham", "Oberviechtach"), Seq(
    ("Cine-World", "Cine-World", "A0389"),
    ("Kino in OVI", "Kino in OVI", "A1538")
  ))
  private def r_fehmarn: R = ("fehmarn", "Fehmarn", "Schleswig-Holstein", 54.4378, 11.19352, Seq("Fehmarn", "Oldenburg in Holstein"), Seq(
    ("Burg-Film-Theater", "Burg-Film-Theater", "A2729"),
    ("Lichtblick Filmtheater", "Lichtblick Filmtheater", "A0899")
  ))
  private def r_westerland: R = ("westerland", "Westerland", "Schleswig-Holstein", 54.9079, 8.30326, Seq("Westerland", "Norddorf auf Amrum"), Seq(
    ("Kinowelt Westerland", "Kinowelt Westerland", "A0843"),
    ("LichtBlick InselKino", "LichtBlick InselKino", "A2807")
  ))
  private def r_sassnitz: R = ("sassnitz", "Sassnitz", "Mecklenburg-Vorpommern", 54.5157, 13.64451, Seq("Sassnitz", "Göhren"), Seq(
    ("Lichtspiele Sassnitz", "Lichtspiele Sassnitz", "A2711"),
    ("Ostseebad Göhren Kinohalle - Regenbogencamp", "Ostseebad Göhren Kinohalle - Regenbogencamp", "A2815")
  ))
  private def r_berchtesgaden: R = ("berchtesgaden", "Berchtesgaden", "Bayern", 47.63236, 13.00187, Seq("Berchtesgaden", "Schönau am Königssee"), Seq(
    ("Kurkino im Kurhaus", "Kurkino im Kurhaus", "A0886"),
    ("Open Air Kino an der Sportanlage Schneewinkl am Königssee", "Open Air Kino an der Sportanlage Schneewinkl am Königssee", "A1977")
  ))
  private def r_bernkastel_kues: R = ("bernkastel-kues", "Bernkastel-Kues", "Rheinland-Pfalz", 49.91602, 7.07664, Seq("Bernkastel-Kues", "Hoppstädten-Weiersbach"), Seq(
    ("Mosel-Kino", "Mosel-Kino", "A0180"),
    ("Movietown Neubrücke", "Movietown Neubrücke", "A0973")
  ))
  private def r_hachenburg: R = ("hachenburg", "Hachenburg", "Rheinland-Pfalz", 50.65998, 7.82276, Seq("Hachenburg", "Neitersen"), Seq(
    ("Cinexx", "Cinexx", "A0392"),
    ("Wied Scala", "Wied Scala", "A1278")
  ))
  private def r_lychen: R = ("lychen", "Lychen", "Brandenburg", 53.21089, 13.31556, Seq("Lychen", "Wesenberg"), Seq(
    ("Altes Kino", "Altes Kino", "A2634"),
    ("Kino Wesenberg", "Kino Wesenberg", "A0757")
  ))
  private def r_eberswalde_finow: R = ("eberswalde-finow", "Eberswalde-Finow", "Brandenburg", 52.83492, 13.81951, Seq("Eberswalde-Finow"), Seq(
    ("Movie Magic", "Movie Magic", "A2891")
  ))
  private def r_stendal: R = ("stendal", "Stendal", "Sachsen-Anhalt", 52.60578, 11.86091, Seq("Stendal"), Seq(
    ("Uppstall Kinos", "Uppstall Kinos", "A0664")
  ))
  private def r_schwedt: R = ("schwedt", "Schwedt", "Brandenburg", 53.05963, 14.28154, Seq("Schwedt"), Seq(
    ("FilmforUM Schwedt", "FilmforUM Schwedt", "A0333")
  ))
  private def r_itzehoe: R = ("itzehoe", "Itzehoe", "Schleswig-Holstein", 53.92099, 9.51529, Seq("Itzehoe"), Seq(
    ("CineMotion Itzehoe", "CineMotion Itzehoe", "A0125")
  ))
  private def r_parchim: R = ("parchim", "Parchim", "Mecklenburg-Vorpommern", 53.42631, 11.84875, Seq("Parchim"), Seq(
    ("Movie Star Parchim", "Movie Star Parchim", "A0967")
  ))
  private def r_luebben: R = ("luebben", "Lübben", "Brandenburg", 51.93814, 13.88826, Seq("Lübben"), Seq(
    ("Spreewald Lichtspiele Lübben", "Spreewald Lichtspiele Lübben", "A1144")
  ))
  private def r_demmin: R = ("demmin", "Demmin", "Mecklenburg-Vorpommern", 53.90762, 13.03142, Seq("Demmin"), Seq(
    ("Filmeck Demmin", "Filmeck Demmin", "A2382")
  ))
  private def r_letschin: R = ("letschin", "Letschin", "Brandenburg", 52.64379, 14.36007, Seq("Letschin"), Seq(
    ("Haus Lichtblick", "Haus Lichtblick", "A2362")
  ))
  private def r_sosa: R = ("sosa", "Sosa", "Sachsen", 50.49917, 12.6512, Seq("Sosa"), Seq(
    ("Freilichtbühne Sosa", "Freilichtbühne Sosa", "A2191")
  ))
  private def r_prerow: R = ("prerow", "Prerow", "Mecklenburg-Vorpommern", 54.44469, 12.57677, Seq("Prerow"), Seq(
    ("Cinema Ostseebad Prerow", "Cinema Ostseebad Prerow", "A0241")
  ))
  private def r_fischbach_bei_dahn: R = ("fischbach-bei-dahn", "Fischbach bei Dahn", "Rheinland-Pfalz", 49.08771, 7.7116, Seq("Fischbach bei Dahn"), Seq(
    ("Wasgau-Theater", "Wasgau-Theater", "A1269")
  ))
  private def r_helgoland: R = ("helgoland", "Helgoland", "Schleswig-Holstein", 54.18143, 7.8863, Seq("Helgoland"), Seq(
    ("Hochseekino Helgoland", "Hochseekino Helgoland", "A2652")
  ))
  private def r_kloeden: R = ("kloeden", "Klöden", "Sachsen-Anhalt", 51.76178, 12.83169, Seq("Klöden"), Seq(
    ("Sommerkino Klöden", "Sommerkino Klöden", "A2508")
  ))

  private def chunk0: Seq[R] = Seq(r_berlin, r_frankfurt_am_main, r_stuttgart, r_koeln, r_muenchen, r_hamburg, r_nuernberg, r_dortmund, r_mannheim, r_krefeld, r_bielefeld, r_chemnitz, r_leipzig, r_karlsruhe, r_saarbruecken, r_bremen, r_heilbronn, r_schwaebisch_gmuend, r_hannover, r_braunschweig, r_villingen_schwenningen, r_dresden, r_jena, r_schweinfurt, r_freiburg, r_wuppertal, r_ravensburg, r_regensburg, r_kiel, r_konstanz, r_landsberg_am_lech, r_augsburg, r_offenburg, r_tauberbischofsheim, r_kassel, r_ingolstadt, r_hechingen, r_juelich, r_butzbach, r_luebeck)
  private def chunk1: Seq[R] = Seq(r_arnsberg, r_loerrach, r_memmingen, r_noerdlingen, r_bautzen, r_rostock, r_trier, r_aschaffenburg, r_goerlitz, r_goettingen, r_rheine, r_plauen, r_crailsheim, r_bad_aibling, r_osnabrueck, r_muenster, r_erfurt, r_ulm, r_koblenz, r_bad_toelz, r_minden, r_vechta, r_schwerin, r_cottbus, r_bamberg, r_landshut, r_kaiserslautern, r_lueneburg, r_hameln, r_burghausen, r_bad_kreuznach, r_magdeburg, r_marburg, r_oldenburg, r_fulda, r_passau, r_ansbach, r_paderborn, r_traunstein, r_brandenburg)
  private def chunk2: Seq[R] = Seq(r_altenburg, r_riesa, r_sigmaringen, r_anklam, r_altensteig, r_celle, r_neubrandenburg, r_dessau_rosslau, r_emden, r_waldshut_tiengen, r_bad_urach, r_frankfurt_an_der_oder, r_amberg, r_straubing, r_suhl, r_eisenach, r_nienburg, r_korbach, r_daun, r_oberstdorf, r_flensburg, r_gummersbach, r_garmisch_partenkirchen, r_dorsten, r_stralsund, r_cuxhaven, r_nordhausen, r_coburg, r_halberstadt, r_meppen, r_neuruppin, r_bad_neuenahr_ahrweiler, r_rotenburg_an_der_wuemme, r_salzwedel, r_bad_berleburg, r_weissenburg, r_marktredwitz, r_bremerhaven, r_bayreuth, r_goslar)
  private def chunk3: Seq[R] = Seq(r_bernburg, r_bad_hersfeld, r_waren_mueritz, r_wittenberge, r_marktoberdorf, r_erbach_im_odenwald, r_regen, r_bad_schwalbach, r_niebuell, r_uffenheim, r_bad_lobenstein, r_langeoog, r_kleve, r_luckenwalde, r_prenzlau, r_husum, r_finsterwalde, r_cham, r_fehmarn, r_westerland, r_sassnitz, r_berchtesgaden, r_bernkastel_kues, r_hachenburg, r_lychen, r_eberswalde_finow, r_stendal, r_schwedt, r_itzehoe, r_parchim, r_luebben, r_demmin, r_letschin, r_sosa, r_prerow, r_fischbach_bei_dahn, r_helgoland, r_kloeden)
  val regions: Seq[R] = chunk0 ++ chunk1 ++ chunk2 ++ chunk3
}
