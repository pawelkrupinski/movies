// GENERATED from data/spain/pages.json by data/spain/scripts/generate_roster.py
// — do NOT edit by hand. Full Spanish cinema roster: 240 pages / 602 cinemas (SensaCine, plus the Ocine
// venues it does not list, from data/spain/ocine.json).
// Regenerate with `python3 data/spain/scripts/generate_roster.py` after re-clustering;
// see data/spain/README.md.
package models

private[models] object SpanishRosterData {
  // (displayName, pillName, SensaCine theaterId, Ocine ticketing server) — a venue
  // SensaCine does not list has no theaterId and is scraped off its own server
  type C = (String, String, Option[String], Option[String])
  // (slug, slug qualified with its autonomous community, name, province, lat, lon,
  //  zoneId, multiTown, towns, cinemas)
  type R = (String, String, String, String, Double, Double, String, Boolean, Seq[String], Seq[C])

  private def p_madrid: R = ("madrid", "madrid-comunidad-de-madrid", "Madrid", "Madrid", 40.4165, -3.7026, "Europe/Madrid", false, Seq("Madrid"), Seq(
    ("Cines Princesa", "Cines Princesa", Some("E0364"), None),
    ("Cinesa La Gavia", "Cinesa La Gavia", Some("E0731"), None),
    ("Cinesa Manoteras", "Cinesa Manoteras", Some("E0646"), None),
    ("Cinesa Méndez Álvaro", "Cinesa Méndez Álvaro", Some("E0247"), None),
    ("Cinesa Príncipe Pío", "Cinesa Príncipe Pío", Some("E0401"), None),
    ("Yelmo Cines Ideal", "Yelmo Cines Ideal", Some("E0621"), None),
    ("Yelmo Cines Islazul", "Yelmo Cines Islazul", Some("E0681"), None),
    ("Yelmo Cines La Vaguada", "Yelmo Cines La Vaguada", Some("E0459"), None),
    ("Yelmo Cines Plenilunio", "Yelmo Cines Plenilunio", Some("E0475"), None),
    ("mk2 Palacio de Hielo (antiguos Cines Dreams)", "mk2 Palacio de Hielo (antiguos Cines Dreams)", Some("E0432"), None),
    ("Ocine Urban Caleido", "Ocine Urban Caleido", None, Some("tickets.ocineurbancaleido.es"))
  ))
  private def p_barcelona: R = ("barcelona", "barcelona-cataluna", "Barcelona", "Barcelona", 41.3888, 2.159, "Europe/Madrid", false, Seq("Barcelona"), Seq(
    ("Arenas Multicines 3D", "Arenas Multicines 3D", Some("E0764"), None),
    ("Aribau Multicines", "Aribau Multicines", Some("E0091"), None),
    ("Balmes Multicines", "Balmes Multicines", Some("E0808"), None),
    ("Bosque Multicines", "Bosque Multicines", Some("E0136"), None),
    ("Cinesa Diagonal", "Cinesa Diagonal", Some("E0381"), None),
    ("Cinesa Diagonal Mar", "Cinesa Diagonal Mar", Some("E0382"), None),
    ("Cinesa SOM Multiespai", "Cinesa SOM Multiespai", Some("E0388"), None),
    ("Glòries Multicines", "Glòries Multicines", Some("E0442"), None),
    ("Gran Sarrià Multicines", "Gran Sarrià Multicines", Some("E0447"), None),
    ("Renoir Floridablanca", "Renoir Floridablanca", Some("E0581"), None)
  ))
  private def p_valencia: R = ("valencia", "valencia-comunidad-valenciana", "Valencia", "Valencia", 39.4739, -0.3797, "Europe/Madrid", false, Seq("Valencia"), Seq(
    ("Abc El Saler", "Abc El Saler", Some("E0034"), None),
    ("Abc Park", "Abc Park", Some("E0040"), None),
    ("Autocine Star", "Autocine Star", Some("E0104"), None),
    ("Cines Babel", "Cines Babel", Some("E0119"), None),
    ("Cines Lys", "Cines Lys", Some("E0187"), None),
    ("Cinestudio D´or", "Cinestudio D´or", Some("E0407"), None),
    ("Ocine Premium Aqua", "Ocine Premium Aqua", Some("E0474"), Some("tickets.ocinepremiumaqua.es")),
    ("Teatro Flumen", "Teatro Flumen", Some("E0967"), None),
    ("Yelmo Cines Campanar", "Yelmo Cines Campanar", Some("E0248"), None),
    ("Yelmo Cines Mercado de Campanar", "Yelmo Cines Mercado de Campanar", Some("E0773"), None)
  ))
  private def p_sevilla: R = ("sevilla", "sevilla-andalucia", "Sevilla", "Sevilla", 37.3828, -5.9732, "Europe/Madrid", false, Seq("Sevilla"), Seq(
    ("Avenida 5 Cines", "Avenida 5 Cines", Some("E0112"), None),
    ("Los Arcos Multicines", "Los Arcos Multicines", Some("E0222"), None),
    ("Odeon Multicines Plaza de Armas", "Odeon Multicines Plaza de Armas", Some("E0400"), None),
    ("Yelmo Cines Premium Lagoh", "Yelmo Cines Premium Lagoh", Some("E1002"), None),
    ("Zona Este", "Zona Este", Some("E0476"), None),
    ("mk2 Nervión Plaza", "mk2 Nervión Plaza", Some("E0415"), None)
  ))
  private def p_zaragoza: R = ("zaragoza", "zaragoza-aragon", "Zaragoza", "Zaragoza", 41.6561, -0.8773, "Europe/Madrid", false, Seq("Zaragoza"), Seq(
    ("Artesiete La Torre", "Artesiete La Torre", Some("E1041"), None),
    ("Cine Palafox Zaragoza", "Cine Palafox Zaragoza", Some("E0264"), None),
    ("Cine Sala Cervantes", "Cine Sala Cervantes", Some("E0711"), None),
    ("Cines Aragonia", "Cines Aragonia", Some("E0732"), None),
    ("Cinesa Grancasa", "Cinesa Grancasa", Some("E0387"), None),
    ("Cinesa Puerto Venecia", "Cinesa Puerto Venecia", Some("E0790"), None)
  ))
  private def p_malaga: R = ("malaga", "malaga-andalucia", "Málaga", "Málaga", 36.7202, -4.4203, "Europe/Madrid", false, Seq("Málaga"), Seq(
    ("Alameda Multicines Malaga", "Alameda Multicines Malaga", Some("E0048"), None),
    ("Cine Albéniz", "Cine Albéniz", Some("E0195"), None),
    ("Multicines Rosaleda", "Multicines Rosaleda", Some("E0589"), None),
    ("mk2 Malaga Nostrum", "mk2 Malaga Nostrum", Some("E0413"), None)
  ))
  private def p_murcia: R = ("murcia", "murcia-region-de-murcia", "Murcia", "Murcia", 37.987, -1.13, "Europe/Madrid", false, Seq("Murcia"), Seq(
    ("Cinesa Nueva Condomina", "Cinesa Nueva Condomina", Some("E0656"), None),
    ("Neocine Centrofama", "Neocine Centrofama", Some("E0193"), None),
    ("Neocine El Tiro", "Neocine El Tiro", Some("E0774"), None),
    ("Neocine Rex", "Neocine Rex", Some("E0690"), None),
    ("Neocine Thader", "Neocine Thader", Some("E0547"), None)
  ))
  private def p_palma_de_mallorca: R = ("palma-de-mallorca", "palma-de-mallorca-islas-baleares", "Palma de Mallorca", "Islas Baleares", 39.5694, 2.6502, "Europe/Madrid", false, Seq("Palma de Mallorca"), Seq(
    ("Artesiete Fan", "Artesiete Fan", Some("E0863"), None),
    ("CineCiutat", "CineCiutat", Some("E0365"), None),
    ("Cines Ocimax", "Cines Ocimax", Some("E0360"), None),
    ("Multicines Rivoli", "Multicines Rivoli", Some("E0533"), None),
    ("Sala Augusta", "Sala Augusta", Some("E0593"), None),
    ("Ocine Premium Porto Pi", "Ocine Premium Porto Pi", None, Some("tickets.ocinepremiumportopi.es:8444"))
  ))
  private def p_las_palmas_de_gran_canaria: R = ("las-palmas-de-gran-canaria", "las-palmas-de-gran-canaria-canarias", "Las Palmas de Gran Canaria", "Las Palmas", 28.1018, -15.4157, "Atlantic/Canary", false, Seq("Las Palmas de Gran Canaria"), Seq(
    ("Yelmo Cines Las Arenas", "Yelmo Cines Las Arenas", Some("E0754"), None),
    ("Yelmo Cines Premium Alisios", "Yelmo Cines Premium Alisios", Some("E0972"), None),
    ("Ocine Premium 7 Palmas", "Ocine Premium 7 Palmas", None, Some("tickets.ocinepremium7palmas.es"))
  ))
  private def p_bilbao: R = ("bilbao", "bilbao-pais-vasco", "Bilbao", "Vizcaya", 43.2627, -2.9253, "Europe/Madrid", false, Seq("Bilbao"), Seq(
    ("Cinesa Zubiarte", "Cinesa Zubiarte", Some("E0425"), None),
    ("Golem Alhóndiga", "Golem Alhóndiga", Some("E0737"), None),
    ("Multicines 7 Bilbao", "Multicines 7 Bilbao", Some("E0488"), None)
  ))
  private def p_alicante: R = ("alicante", "alicante-comunidad-valenciana", "Alicante", "Alicante", 38.3452, -0.4815, "Europe/Madrid", false, Seq("Alicante"), Seq(
    ("Cine Aana Alicante", "Cine Aana Alicante", Some("E0008"), None),
    ("Cine Navas", "Cine Navas", Some("E0545"), None),
    ("Cinebox Plaza Mar 2", "Cinebox Plaza Mar 2", Some("E0293"), None),
    ("Cines Axion Playa de San Juan ", "Cines Axion Playa de San Juan ", Some("E0884"), None),
    ("Cines Costa", "Cines Costa", Some("E0951"), None),
    ("Cines Panoramis", "Cines Panoramis", Some("E0397"), None),
    ("Kinépolis Alicante", "Kinépolis Alicante", Some("E0819"), None),
    ("Yelmo Cines Puerta De Alicante", "Yelmo Cines Puerta De Alicante", Some("E0631"), None)
  ))
  private def p_cordoba: R = ("cordoba", "cordoba-andalucia", "Córdoba", "Córdoba", 37.8916, -4.7728, "Europe/Madrid", false, Seq("Córdoba"), Seq(
    ("Cine Delicias", "Cine Delicias", Some("E0948"), None),
    ("Guadalquivir Cinemas 10", "Guadalquivir Cinemas 10", Some("E0512"), None),
    ("mk2 El Tablero", "mk2 El Tablero", Some("E0409"), None)
  ))
  private def p_valladolid: R = ("valladolid", "valladolid-castilla-y-leon", "Valladolid", "Valladolid", 41.6554, -4.7235, "Europe/Madrid", false, Seq("Valladolid"), Seq(
    ("Cine Casablanca", "Cine Casablanca", Some("E0243"), None),
    ("Cines Broadway", "Cines Broadway", Some("E0333"), None),
    ("Cines Manhattan", "Cines Manhattan", Some("E0357"), None),
    ("Yelmo Cines Premium VallSur", "Yelmo Cines Premium VallSur", Some("E0297"), None)
  ))
  private def p_vigo: R = ("vigo", "vigo-galicia", "Vigo", "Pontevedra", 42.2328, -8.7226, "Europe/Madrid", false, Seq("Vigo"), Seq(
    ("Cines Tamberlick Plaza Elíptica", "Cines Tamberlick Plaza Elíptica", Some("E0739"), None),
    ("Multicines Norte", "Multicines Norte", Some("E0525"), None),
    ("Teatro Salesianos", "Teatro Salesianos", Some("E0602"), None),
    ("Yelmo Cines Premium Vialia Vigo", "Yelmo Cines Premium Vialia Vigo", Some("E2902"), None),
    ("Yelmo Cines Travesía Vigo", "Yelmo Cines Travesía Vigo", Some("E0635"), None),
    ("Ocine Premium Gran Vía de Vigo", "Ocine Premium Gran Vía de Vigo", None, Some("tickets.ocinepremiumgranvia.es"))
  ))
  private def p_gijon: R = ("gijon", "gijon-asturias", "Gijón", "Asturias", 43.5357, -5.6615, "Europe/Madrid", false, Seq("Gijón"), Seq(
    ("Autocine Gijón", "Autocine Gijón", Some("E0784"), None),
    ("Ocine Premium Los Fresnos", "Ocine Premium Los Fresnos", None, Some("tickets.ocinepremiumlosfresnos.es"))
  ))
  private def p_l_hospitalet_de_llobregat: R = ("l-hospitalet-de-llobregat", "l-hospitalet-de-llobregat-cataluna", "L'Hospitalet de Llobregat", "Barcelona", 41.3597, 2.1003, "Europe/Madrid", false, Seq("L'Hospitalet de Llobregat"), Seq(
    ("Cinesa La Farga", "Cinesa La Farga", Some("E0391"), None),
    ("Filmax Gran Via 3D", "Filmax Gran Via 3D", Some("E0439"), None)
  ))
  private def p_a_coruna: R = ("a-coruna", "a-coruna-galicia", "A Coruña", "A Coruña", 43.3713, -8.396, "Europe/Madrid", false, Seq("A Coruña"), Seq(
    ("Cantones Cines", "Cantones Cines", Some("E0437"), None),
    ("Cines Forum Metropolitano", "Cines Forum Metropolitano", Some("E0441"), None),
    ("Cinesa Marineda City", "Cinesa Marineda City", Some("E0770"), None),
    ("Yelmo Cines Espacio Coruña", "Yelmo Cines Espacio Coruña", Some("E0734"), None)
  ))
  private def p_vitoria_gasteiz: R = ("vitoria-gasteiz", "vitoria-gasteiz-pais-vasco", "Vitoria-Gasteiz", "Álava", 42.85, -2.6727, "Europe/Madrid", false, Seq("Vitoria-Gasteiz"), Seq(
    ("Cines Florida", "Cines Florida", Some("E0346"), None),
    ("Cines Guridi", "Cines Guridi", Some("E0763"), None),
    ("Yelmo Cines Boulevard", "Yelmo Cines Boulevard", Some("E0786"), None)
  ))
  private def p_granada: R = ("granada", "granada-andalucia", "Granada", "Granada", 37.1882, -3.6067, "Europe/Madrid", false, Seq("Granada"), Seq(
    ("Cine Madrigal", "Cine Madrigal", Some("E0689"), None),
    ("Megarama Granada", "Megarama Granada", Some("E0301"), None),
    ("Ocine Serrallo", "Ocine Serrallo", Some("E0787"), Some("tickets.ocineserrallo.es")),
    ("Teatro Isabel La Catolica", "Teatro Isabel La Catolica", Some("E0712"), None)
  ))
  private def p_elche: R = ("elche", "elche-comunidad-valenciana", "Elche", "Alicante", 38.2622, -0.7011, "Europe/Madrid", false, Seq("Elche"), Seq(
    ("Abc Elx", "Abc Elx", Some("E0035"), None),
    ("Cines Odeón", "Cines Odeón", Some("E0853"), None)
  ))
  private def p_oviedo: R = ("oviedo", "oviedo-asturias", "Oviedo", "Asturias", 43.3603, -5.8448, "Europe/Madrid", false, Seq("Oviedo"), Seq(
    ("Yelmo Cines Los Prados", "Yelmo Cines Los Prados", Some("E0623"), None)
  ))
  private def p_badalona: R = ("badalona", "badalona-cataluna", "Badalona", "Barcelona", 41.45, 2.2474, "Europe/Madrid", false, Seq("Badalona"), Seq(
    ("Ocine Màgic", "Ocine Màgic", Some("E0713"), Some("tickets.ocinemagic.es"))
  ))
  private def p_terrassa: R = ("terrassa", "terrassa-cataluna", "Terrassa", "Barcelona", 41.5667, 2.0167, "Europe/Madrid", false, Seq("Terrassa"), Seq(
    ("Cinema Catalunya", "Cinema Catalunya", Some("E0304"), None),
    ("Cinesa Parc Vallès", "Cinesa Parc Vallès", Some("E0374"), None),
    ("Club Catalunya", "Club Catalunya", Some("E0420"), None)
  ))
  private def p_cartagena: R = ("cartagena", "cartagena-region-de-murcia", "Cartagena", "Murcia", 37.602, -0.984, "Europe/Madrid", false, Seq("Cartagena"), Seq(
    ("Cine La Manga", "Cine La Manga", Some("E0946"), None),
    ("Cine Sirenas", "Cine Sirenas", Some("E0947"), None),
    ("NeoCine Espacio Mediterraneo", "NeoCine Espacio Mediterraneo", Some("E0663"), None),
    ("Neocine Mandarache", "Neocine Mandarache", Some("E0546"), None),
    ("Nuevos Cines Cabos de Palos ", "Nuevos Cines Cabos de Palos ", Some("E0953"), None)
  ))
  private def p_jerez_de_la_frontera: R = ("jerez-de-la-frontera", "jerez-de-la-frontera-andalucia", "Jerez de la Frontera", "Cádiz", 36.6865, -6.1361, "Europe/Madrid", false, Seq("Jerez de la Frontera"), Seq(
    ("Multicines Jerez UCC", "Multicines Jerez UCC", Some("E1044"), None),
    ("Yelmo Cines Área Sur", "Yelmo Cines Área Sur", Some("E0669"), None)
  ))
  private def p_sabadell: R = ("sabadell", "sabadell-cataluna", "Sabadell", "Barcelona", 41.5433, 2.1094, "Europe/Madrid", false, Seq("Sabadell"), Seq(
    ("Cines Imperial", "Cines Imperial", Some("E0351"), None),
    ("Multicines Eix Macià", "Multicines Eix Macià", Some("E0504"), None)
  ))
  private def p_santa_cruz_de_tenerife: R = ("santa-cruz-de-tenerife", "santa-cruz-de-tenerife-canarias", "Santa Cruz de Tenerife", "Santa Cruz de Tenerife", 28.4682, -16.2546, "Atlantic/Canary", false, Seq("Santa Cruz de Tenerife"), Seq(
    ("Cines Price Prime", "Cines Price Prime", Some("E0583"), None),
    ("Yelmo Cines Meridiano", "Yelmo Cines Meridiano", Some("E0627"), None)
  ))
  private def p_mostoles: R = ("mostoles", "mostoles-comunidad-de-madrid", "Móstoles", "Madrid", 40.3223, -3.865, "Europe/Madrid", false, Seq("Móstoles"), Seq(
    ("Cines Dos de Mayo", "Cines Dos de Mayo", Some("E0761"), None)
  ))
  private def p_alcala_de_henares: R = ("alcala-de-henares", "alcala-de-henares-comunidad-de-madrid", "Alcalá de Henares", "Madrid", 40.4821, -3.36, "Europe/Madrid", false, Seq("Alcalá de Henares"), Seq(
    ("Multicines Cisneros", "Multicines Cisneros", Some("E0498"), None),
    ("Ocine Quadernillos", "Ocine Quadernillos", None, Some("tickets.ocinequadernillos.es"))
  ))
  private def p_pamplona: R = ("pamplona", "pamplona-navarra", "Pamplona", "Navarra", 42.8169, -1.6432, "Europe/Madrid", false, Seq("Pamplona"), Seq(
    ("Golem Baiona", "Golem Baiona", Some("E0444"), None),
    ("Golem Yamaguchi", "Golem Yamaguchi", Some("E0445"), None)
  ))
  private def p_fuenlabrada: R = ("fuenlabrada", "fuenlabrada-comunidad-de-madrid", "Fuenlabrada", "Madrid", 40.2842, -3.7942, "Europe/Madrid", false, Seq("Fuenlabrada"), Seq(
    ("Cinesa Plaza Loranca 2", "Cinesa Plaza Loranca 2", Some("E0394"), None)
  ))
  private def p_almeria: R = ("almeria", "almeria-andalucia", "Almería", "Almería", 36.8381, -2.4597, "Europe/Madrid", false, Seq("Almería"), Seq(
    ("Kinépolis Almería Mediterráneo", "Kinépolis Almería Mediterráneo", Some("E0359"), None),
    ("Yelmo Cines Torrecárdenas", "Yelmo Cines Torrecárdenas", Some("E0909"), None)
  ))
  private def p_leganes: R = ("leganes", "leganes-comunidad-de-madrid", "Leganés", "Madrid", 40.3272, -3.7635, "Europe/Madrid", false, Seq("Leganés"), Seq(
    ("Cinesa Parquesur", "Cinesa Parquesur", Some("E0399"), None),
    ("Odeon Multicines Sambil Dolby Atmos", "Odeon Multicines Sambil Dolby Atmos", Some("E0877"), None)
  ))
  private def p_san_sebastian: R = ("san-sebastian", "san-sebastian-pais-vasco", "San Sebastián", "Guipúzcoa", 43.3128, -1.975, "Europe/Madrid", false, Seq("San Sebastián"), Seq(
    ("Cine Príncipe", "Cine Príncipe", Some("E0570"), None),
    ("Cine Trueba", "Cine Trueba", Some("E0603"), None),
    ("Cines Antiguo Berri", "Cines Antiguo Berri", Some("E0329"), None)
  ))
  private def p_getafe: R = ("getafe", "getafe-comunidad-de-madrid", "Getafe", "Madrid", 40.3057, -3.733, "Europe/Madrid", false, Seq("Getafe"), Seq(
    ("Cinesa Nassica", "Cinesa Nassica", Some("E0246"), None)
  ))
  private def p_castellon_de_la_plana: R = ("castellon-de-la-plana", "castellon-de-la-plana-comunidad-valenciana", "Castellón de la Plana", "Castellón", 39.9857, -0.0493, "Europe/Madrid", false, Seq("Castellón de la Plana"), Seq(
    ("Cinesa Salera", "Cinesa Salera", Some("E0654"), None),
    ("Neocine Puerto Azahar", "Neocine Puerto Azahar", Some("E0571"), None),
    ("Ocine Premium Estepark", "Ocine Premium Estepark", Some("E0925"), Some("tickets.ocinepremiumestepark.es"))
  ))
  private def p_burgos: R = ("burgos", "burgos-castilla-y-leon", "Burgos", "Burgos", 42.3411, -3.7018, "Europe/Madrid", false, Seq("Burgos"), Seq(
    ("Cines Van Golem Arlanzón", "Cines Van Golem Arlanzón", Some("E0370"), None),
    ("Odeon Multicines Burgos", "Odeon Multicines Burgos", Some("E0279"), None)
  ))
  private def p_santander: R = ("santander", "santander-cantabria", "Santander", "Cantabria", 43.4659, -3.8049, "Europe/Madrid", false, Seq("Santander"), Seq(
    ("Cine Los Ángeles", "Cine Los Ángeles", Some("E0688"), None),
    ("Cines Embajadores Santander", "Cines Embajadores Santander", Some("E0349"), None),
    ("Cinesa Bahía de Santander", "Cinesa Bahía de Santander", Some("E0123"), None),
    ("Filmoteca de Cantabria - Santander", "Filmoteca de Cantabria - Santander", Some("E0979"), None),
    ("Palacios De Festivales", "Palacios De Festivales", Some("E0560"), None),
    ("Yelmo Cines Premium Peñacastillo", "Yelmo Cines Premium Peñacastillo", Some("E0565"), None)
  ))
  private def p_albacete: R = ("albacete", "albacete-castilla-la-mancha", "Albacete", "Albacete", 38.9942, -1.8564, "Europe/Madrid", false, Seq("Albacete"), Seq(
    ("Yelmo Cines Imaginalia", "Yelmo Cines Imaginalia", Some("E0205"), None)
  ))
  private def p_alcorcon: R = ("alcorcon", "alcorcon-comunidad-de-madrid", "Alcorcón", "Madrid", 40.3458, -3.8249, "Europe/Madrid", false, Seq("Alcorcón"), Seq(
    ("Ocine Urban X-Madrid", "Ocine Urban X-Madrid", Some("E1004"), Some("tickets.ocineurbanxmadrid.es")),
    ("Yelmo Cines TresAguas", "Yelmo Cines TresAguas", Some("E0207"), None)
  ))
  private def p_san_cristobal_de_la_laguna: R = ("san-cristobal-de-la-laguna", "san-cristobal-de-la-laguna-canarias", "San Cristóbal de La Laguna", "Santa Cruz de Tenerife", 28.4853, -16.3201, "Atlantic/Canary", false, Seq("San Cristóbal de La Laguna"), Seq(
    ("Multicines Tenerife", "Multicines Tenerife", Some("E0284"), None)
  ))
  private def p_salamanca: R = ("salamanca", "salamanca-castilla-y-leon", "Salamanca", "Salamanca", 40.9688, -5.6639, "Europe/Madrid", false, Seq("Salamanca"), Seq(
    ("Cines Van Dyck", "Cines Van Dyck", Some("E0606"), None),
    ("Megarama Salamanca", "Megarama Salamanca", Some("E0299"), None)
  ))
  private def p_logrono: R = ("logrono", "logrono-la-rioja", "Logroño", "La Rioja", 42.4661, -2.4512, "Europe/Madrid", false, Seq("Logroño"), Seq(
    ("Cines 7 Infantes", "Cines 7 Infantes", Some("E0804"), None),
    ("Cines Moderno", "Cines Moderno", Some("E0358"), None),
    ("Yelmo Cines Premium Berceo", "Yelmo Cines Premium Berceo", Some("E0733"), None)
  ))
  private def p_adeje: R = ("adeje", "adeje-canarias", "Adeje", "Santa Cruz de Tenerife", 28.1227, -16.726, "Atlantic/Canary", true, Seq("Adeje", "Arona"), Seq(
    ("Autocinema Tenerife", "Autocinema Tenerife", Some("E2912"), None),
    ("Multicines Zentral Center", "Multicines Zentral Center", Some("E0541"), None),
    ("X-Sur Cine", "X-Sur Cine", Some("E0700"), None)
  ))
  private def p_aguilar_de_campoo: R = ("aguilar-de-campoo", "aguilar-de-campoo-castilla-y-leon", "Aguilar de Campoo", "Palencia", 42.7945, -4.2589, "Europe/Madrid", false, Seq("Aguilar de Campoo"), Seq(
    ("Cines Campoo", "Cines Campoo", Some("E0334"), None),
    ("Cines Campoo 3D", "Cines Campoo 3D", Some("E1003"), None)
  ))
  private def p_aguilas: R = ("aguilas", "aguilas-region-de-murcia", "Águilas", "Murcia", 37.4063, -1.5829, "Europe/Madrid", true, Seq("Águilas", "Huércal-Overa", "Vera", "Albox", "Garrucha"), Seq(
    ("Cine Albox", "Cine Albox", Some("E0865"), None),
    ("Cine Tenis", "Cine Tenis", Some("E0975"), None),
    ("Cine Terraza de Verano de Vera", "Cine Terraza de Verano de Vera", Some("E0911"), None),
    ("Cine Municipal Huércal-Overa", "Cine Municipal Huércal-Overa", Some("E1012"), None),
    ("Multicines El Hornillo", "Multicines El Hornillo", Some("E0506"), None)
  ))
  private def p_alcala_de_xivert: R = ("alcala-de-xivert", "alcala-de-xivert-comunidad-valenciana", "Alcalà de Xivert", "Castellón", 40.3, 0.2333, "Europe/Madrid", false, Seq("Alcalà de Xivert"), Seq(
    ("Cine Terraza Avenida", "Cine Terraza Avenida", Some("E0941"), None),
    ("Terraza Neptuno", "Terraza Neptuno", Some("E0926"), None)
  ))
  private def p_alcala_la_real: R = ("alcala-la-real", "alcala-la-real-andalucia", "Alcalá la Real", "Jaén", 37.4614, -3.923, "Europe/Madrid", false, Seq("Alcalá la Real"), Seq(
    ("Cine Teatro Martínez Montañés", "Cine Teatro Martínez Montañés", Some("E1025"), None)
  ))
  private def p_alcaniz: R = ("alcaniz", "alcaniz-aragon", "Alcañiz", "Teruel", 41.05, -0.1333, "Europe/Madrid", true, Seq("Alcañiz", "Caspe", "Arens de Lledo"), Seq(
    ("Cine Arens de Lledó", "Cine Arens de Lledó", Some("E0810"), None),
    ("Cines Alcañiz", "Cines Alcañiz", Some("E0653"), None),
    ("Teatro Cine Goya", "Teatro Cine Goya", Some("E0668"), None)
  ))
  private def p_alcazar_de_san_juan: R = ("alcazar-de-san-juan", "alcazar-de-san-juan-castilla-la-mancha", "Alcázar de San Juan", "Ciudad Real", 39.3901, -3.2083, "Europe/Madrid", true, Seq("Alcázar de San Juan", "Quintanar de la Orden", "Villacañas", "Pedro Muñoz"), Seq(
    ("Cine Teatro Municipal Pedro Muñoz", "Cine Teatro Municipal Pedro Muñoz", Some("E1023"), None),
    ("Multicines Cinemancha", "Multicines Cinemancha", Some("E0496"), None),
    ("Cine Princesa", "Cine Princesa", Some("E0837"), None),
    ("Quintanar Cinema", "Quintanar Cinema", Some("E0870"), None)
  ))
  private def p_alcobendas: R = ("alcobendas", "alcobendas-comunidad-de-madrid", "Alcobendas", "Madrid", 40.5475, -3.642, "Europe/Madrid", true, Seq("Alcobendas", "Tres Cantos", "San Sebastián de los Reyes"), Seq(
    ("Cinebox 3 C", "Cinebox 3 C", Some("E0199"), None),
    ("Odeon Multicines Tres Cantos", "Odeon Multicines Tres Cantos", Some("E0815"), None),
    ("Cinesa La Moraleja", "Cinesa La Moraleja", Some("E0392"), None),
    ("Kinépolis Madrid Diversia", "Kinépolis Madrid Diversia", Some("E0209"), None),
    ("Yelmo Cines Plaza Norte 2", "Yelmo Cines Plaza Norte 2", Some("E2916"), None)
  ))
  private def p_alcoy: R = ("alcoy", "alcoy-comunidad-valenciana", "Alcoy", "Alicante", 38.7054, -0.4743, "Europe/Madrid", true, Seq("Alcoy", "Ontinyent", "Cocentaina"), Seq(
    ("Cine BIC", "Cine BIC", Some("E0033"), None),
    ("Multicines El Altet", "Multicines El Altet", Some("E0721"), None),
    ("Cineapolis El Teler", "Cineapolis El Teler", Some("E0617"), None)
  ))
  private def p_algeciras: R = ("algeciras", "algeciras-andalucia", "Algeciras", "Cádiz", 36.1333, -5.4505, "Europe/Madrid", true, Seq("Algeciras", "Los Barrios"), Seq(
    ("Odeon Bahía Plaza", "Odeon Bahía Plaza", Some("E0245"), None),
    ("Yelmo Cines Premium Puerta Europa ", "Yelmo Cines Premium Puerta Europa ", Some("E0910"), None)
  ))
  private def p_alhaurin_el_grande: R = ("alhaurin-el-grande", "alhaurin-el-grande-andalucia", "Alhaurín el Grande", "Málaga", 36.643, -4.6873, "Europe/Madrid", true, Seq("Alhaurín el Grande", "Coín"), Seq(
    ("Cine Pixel", "Cine Pixel", Some("E0215"), None),
    ("Cine San Francisco", "Cine San Francisco", Some("E1005"), None)
  ))
  private def p_almazan: R = ("almazan", "almazan-castilla-y-leon", "Almazán", "Soria", 41.4865, -2.5309, "Europe/Madrid", false, Seq("Almazán"), Seq(
    ("Cine Calderón - Almazán", "Cine Calderón - Almazán", Some("E0989"), None)
  ))
  private def p_almendralejo: R = ("almendralejo", "almendralejo-extremadura", "Almendralejo", "Badajoz", 38.6832, -6.4075, "Europe/Madrid", true, Seq("Almendralejo", "Zafra", "Fuente de Cantos"), Seq(
    ("Cine La Fábrica", "Cine La Fábrica", Some("E1022"), None),
    ("Cines Victoria Almendralejo", "Cines Victoria Almendralejo", Some("E0719"), None),
    ("Multicines España", "Multicines España", Some("E0508"), None)
  ))
  private def p_alzira: R = ("alzira", "alzira-comunidad-valenciana", "Alzira", "Valencia", 39.15, -0.4333, "Europe/Madrid", true, Seq("Alzira", "Xàtiva", "Sueca", "Cullera", "Tavernes de la Valldigna"), Seq(
    ("Cine Avenida El Perelló", "Cine Avenida El Perelló", Some("E2914"), None),
    ("Cine Terraza Olimpo", "Cine Terraza Olimpo", Some("E0928"), None),
    ("Cines Axion de Xàtiva", "Cines Axion de Xàtiva", Some("E0664"), None),
    ("Cines Victoria Cullera", "Cines Victoria Cullera", Some("E0210"), None),
    ("Kinepolis Alzira", "Kinepolis Alzira", Some("E0434"), None)
  ))
  private def p_amposta: R = ("amposta", "amposta-cataluna", "Amposta", "Tarragona", 40.7099, 0.5786, "Europe/Madrid", true, Seq("Amposta", "Roquetes"), Seq(
    ("Cinemes Amposta", "Cinemes Amposta", Some("E0076"), None),
    ("Ocine Roquetes", "Ocine Roquetes", Some("E0556"), Some("tickets.ocineroquetes.es"))
  ))
  private def p_andujar: R = ("andujar", "andujar-andalucia", "Andújar", "Jaén", 38.0392, -4.0508, "Europe/Madrid", false, Seq("Andújar"), Seq(
    ("Europa Pantallas 8", "Europa Pantallas 8", Some("E0436"), None),
    ("París Multicines", "París Multicines", Some("E0822"), None)
  ))
  private def p_antequera: R = ("antequera", "antequera-andalucia", "Antequera", "Málaga", 37.0194, -4.5612, "Europe/Madrid", false, Seq("Antequera"), Seq(
    ("Cines La Verónica", "Cines La Verónica", Some("E0410"), None)
  ))
  private def p_aranda_de_duero: R = ("aranda-de-duero", "aranda-de-duero-castilla-y-leon", "Aranda de Duero", "Burgos", 41.6704, -3.6892, "Europe/Madrid", false, Seq("Aranda de Duero"), Seq(
    ("Cines Victoria Ribera de Duero", "Cines Victoria Ribera de Duero", Some("E0777"), None)
  ))
  private def p_arcos_de_la_frontera: R = ("arcos-de-la-frontera", "arcos-de-la-frontera-andalucia", "Arcos de la Frontera", "Cádiz", 36.7507, -5.8106, "Europe/Madrid", true, Seq("Arcos de la Frontera", "Lebrija", "Las Cabezas de San Juan"), Seq(
    ("Arcos Cinema", "Arcos Cinema", Some("E0905"), None),
    ("Teatro Municipal Juan Bernabé", "Teatro Municipal Juan Bernabé", Some("E0984"), None),
    ("Teatro Municipal Las Cabezas de San Juan", "Teatro Municipal Las Cabezas de San Juan", Some("E1020"), None)
  ))
  private def p_arenas_de_san_pedro: R = ("arenas-de-san-pedro", "arenas-de-san-pedro-castilla-y-leon", "Arenas de San Pedro", "Ávila", 40.2104, -5.0869, "Europe/Madrid", true, Seq("Arenas de San Pedro", "Candeleda"), Seq(
    ("Cine Arenas", "Cine Arenas", Some("E0828"), None),
    ("Cine Candeleda", "Cine Candeleda", Some("E0861"), None)
  ))
  private def p_arrecife: R = ("arrecife", "arrecife-canarias", "Arrecife", "Las Palmas", 28.963, -13.5477, "Atlantic/Canary", false, Seq("Arrecife"), Seq(
    ("Deiland Multicines", "Deiland Multicines", Some("E0785"), None),
    ("Multicine Atlántida", "Multicine Atlántida", Some("E0484"), None),
    ("Multicines Deiland", "Multicines Deiland", Some("E0485"), None)
  ))
  private def p_arroyo_de_la_encomienda: R = ("arroyo-de-la-encomienda", "arroyo-de-la-encomienda-castilla-y-leon", "Arroyo de la Encomienda", "Valladolid", 41.6096, -4.7969, "Europe/Madrid", false, Seq("Arroyo de la Encomienda"), Seq(
    ("Ocine Rio Shopping", "Ocine Rio Shopping", Some("E0796"), Some("tickets.ocinerioshopping.es"))
  ))
  private def p_astorga: R = ("astorga", "astorga-castilla-y-leon", "Astorga", "León", 42.4588, -6.056, "Europe/Madrid", false, Seq("Astorga"), Seq(
    ("Cine Velasco", "Cine Velasco", Some("E0274"), None)
  ))
  private def p_avila: R = ("avila", "avila-castilla-y-leon", "Ávila", "Ávila", 40.6572, -4.6995, "Europe/Madrid", false, Seq("Ávila"), Seq(
    ("Cines Bulevar", "Cines Bulevar", Some("E0344"), None)
  ))
  private def p_ayamonte: R = ("ayamonte", "ayamonte-andalucia", "Ayamonte", "Huelva", 37.2133, -7.4081, "Europe/Madrid", true, Seq("Ayamonte", "Lepe", "Isla Cristina"), Seq(
    ("Cine 3D Ayamonte", "Cine 3D Ayamonte", Some("E0788"), None),
    ("La Dehesa Ayamonte", "La Dehesa Ayamonte", Some("E0945"), None),
    ("Cine Vip 3d Lepe", "Cine Vip 3d Lepe", Some("E0765"), None),
    ("Multicines La Dehesa - Islantilla", "Multicines La Dehesa - Islantilla", Some("E0516"), None)
  ))
  private def p_badajoz: R = ("badajoz", "badajoz-extremadura", "Badajoz", "Badajoz", 38.8779, -6.9706, "Europe/Madrid", false, Seq("Badajoz"), Seq(
    ("Yelmo Cines Premium El Faro", "Yelmo Cines Premium El Faro", Some("E1038"), None),
    ("mk2 Conquistadores", "mk2 Conquistadores", Some("E0408"), None)
  ))
  private def p_barakaldo: R = ("barakaldo", "barakaldo-pais-vasco", "Barakaldo", "Vizcaya", 43.2964, -2.9881, "Europe/Madrid", true, Seq("Barakaldo", "Getxo", "Santurtzi", "Leioa"), Seq(
    ("Autocine Getxo", "Autocine Getxo", Some("E0880"), None),
    ("Getxo Zinemak", "Getxo Zinemak", Some("E0464"), None),
    ("Cinesa Max Ocio", "Cinesa Max Ocio", Some("E0424"), None),
    ("Yelmo Cines Megapark", "Yelmo Cines Megapark", Some("E0626"), None),
    ("Serantes Kultur Aretoa", "Serantes Kultur Aretoa", Some("E0598"), None),
    ("Yelmo Cines Artea", "Yelmo Cines Artea", Some("E0376"), None)
  ))
  private def p_barbastro: R = ("barbastro", "barbastro-aragon", "Barbastro", "Huesca", 42.0356, 0.1269, "Europe/Madrid", true, Seq("Barbastro", "Monzón"), Seq(
    ("Cine Cortés", "Cine Cortés", Some("E0250"), None),
    ("Cine Teatro Victoria", "Cine Teatro Victoria", Some("E0271"), None)
  ))
  private def p_barbate: R = ("barbate", "barbate-andalucia", "Barbate", "Cádiz", 36.1924, -5.9219, "Europe/Madrid", true, Seq("Barbate", "Vejer de la Frontera"), Seq(
    ("Cine de Verano La Muralla", "Cine de Verano La Muralla", Some("E0999"), None),
    ("Teatro San Francisco", "Teatro San Francisco", Some("E1018"), None)
  ))
  private def p_baza: R = ("baza", "baza-andalucia", "Baza", "Granada", 37.4907, -2.7726, "Europe/Madrid", false, Seq("Baza"), Seq(
    ("Salón Cine Ideal", "Salón Cine Ideal", Some("E0849"), None)
  ))
  private def p_beasain: R = ("beasain", "beasain-pais-vasco", "Beasain", "Guipúzcoa", 43.0502, -2.2009, "Europe/Madrid", true, Seq("Beasain", "Azkoitia", "Oñati", "Ordizia", "Ibarra"), Seq(
    ("Baztartxo Antzokia", "Baztartxo Antzokia", Some("E0227"), None),
    ("Herri Antzokia ", "Herri Antzokia ", Some("E0890"), None),
    ("Leidor Zinema", "Leidor Zinema", Some("E0472"), None),
    ("Oñatiko Zinea", "Oñatiko Zinea", Some("E0551"), None),
    ("Usurbe Antzokia", "Usurbe Antzokia", Some("E0605"), None)
  ))
  private def p_bejar: R = ("bejar", "bejar-castilla-y-leon", "Béjar", "Salamanca", 40.3864, -5.7634, "Europe/Madrid", true, Seq("Béjar", "El Barco de Ávila"), Seq(
    ("Multicines Béjar", "Multicines Béjar", Some("E0492"), None),
    ("Cine-Teatro Lagasca", "Cine-Teatro Lagasca", Some("E0991"), None)
  ))
  private def p_benidorm: R = ("benidorm", "benidorm-comunidad-valenciana", "Benidorm", "Alicante", 38.5382, -0.131, "Europe/Madrid", true, Seq("Benidorm", "L'Alfàs del Pi", "Finestrat"), Seq(
    ("Cinema Roma", "Cinema Roma", Some("E0268"), None),
    ("Cines Colci ", "Cines Colci ", Some("E0422"), None),
    ("Cines Colci Rincón", "Cines Colci Rincón", Some("E0423"), None),
    ("Colci Suyma", "Colci Suyma", Some("E0957"), None)
  ))
  private def p_berga: R = ("berga", "berga-cataluna", "Berga", "Barcelona", 42.1043, 1.8463, "Europe/Madrid", false, Seq("Berga"), Seq(
    ("Multicines Catalunya", "Multicines Catalunya", Some("E0495"), None)
  ))
  private def p_binefar: R = ("binefar", "binefar-aragon", "Binéfar", "Huesca", 41.8514, 0.2943, "Europe/Madrid", false, Seq("Binéfar"), Seq(
    ("Cine La Paz", "Cine La Paz", Some("E0256"), None),
    ("Teatro Municipal Los Titiriteros", "Teatro Municipal Los Titiriteros", Some("E0964"), None)
  ))
  private def p_blanes: R = ("blanes", "blanes-cataluna", "Blanes", "Girona", 41.6742, 2.7904, "Europe/Madrid", true, Seq("Blanes", "Calella", "Arenys de Mar"), Seq(
    ("Cinema Sala Mozart", "Cinema Sala Mozart", Some("E0314"), None),
    ("Ocine Arenys", "Ocine Arenys", Some("E0651"), Some("tickets.ocinearenys.es")),
    ("Ocine Blanes", "Ocine Blanes", Some("E0462"), Some("tickets.ocineblanes.es"))
  ))
  private def p_boltana: R = ("boltana", "boltana-aragon", "Boltaña", "Huesca", 42.4455, 0.068, "Europe/Madrid", false, Seq("Boltaña"), Seq(
    ("Palacio De Congresos Boltaña", "Palacio De Congresos Boltaña", Some("E0667"), None)
  ))
  private def p_bunol: R = ("bunol", "bunol-comunidad-valenciana", "Buñol", "Valencia", 39.4167, -0.7833, "Europe/Madrid", false, Seq("Buñol"), Seq(
    ("Cine Montecarlo", "Cine Montecarlo", Some("E0883"), None),
    ("Cine Palacio de la Música de Buñol", "Cine Palacio de la Música de Buñol", Some("E0993"), None)
  ))
  private def p_burgo_de_osma: R = ("burgo-de-osma", "burgo-de-osma-castilla-y-leon", "Burgo de Osma", "Soria", 41.5862, -3.0652, "Europe/Madrid", false, Seq("Burgo de Osma"), Seq(
    ("Cine Palafox Burgo de Osma", "Cine Palafox Burgo de Osma", Some("E0265"), None)
  ))
  private def p_caceres: R = ("caceres", "caceres-extremadura", "Cáceres", "Cáceres", 39.4765, -6.3722, "Europe/Madrid", true, Seq("Cáceres", "Arroyo de la Luz"), Seq(
    ("Cine Arroyo de la Luz", "Cine Arroyo de la Luz", Some("E0772"), None),
    ("Multicines Cáceres", "Multicines Cáceres", Some("E0143"), None)
  ))
  private def p_cadiz: R = ("cadiz", "cadiz-andalucia", "Cádiz", "Cádiz", 36.5267, -6.2891, "Europe/Madrid", false, Seq("Cádiz"), Seq(
    ("Al-Andalus Cádiz", "Al-Andalus Cádiz", Some("E0902"), None),
    ("Multicines el Centro", "Multicines el Centro", Some("E0171"), None),
    ("mk2 Bahía de Cádiz", "mk2 Bahía de Cádiz", Some("E0332"), None)
  ))
  private def p_calahorra: R = ("calahorra", "calahorra-la-rioja", "Calahorra", "La Rioja", 42.3051, -1.9652, "Europe/Madrid", true, Seq("Calahorra", "Rincón de Soto"), Seq(
    ("Cine Avenida Rincón de Soto", "Cine Avenida Rincón de Soto", Some("E0829"), None),
    ("Cines Arcca", "Cines Arcca", Some("E0801"), None)
  ))
  private def p_calatayud: R = ("calatayud", "calatayud-aragon", "Calatayud", "Zaragoza", 41.3535, -1.6432, "Europe/Madrid", false, Seq("Calatayud"), Seq(
    ("Teatro Capitol", "Teatro Capitol", Some("E1007"), None)
  ))
  private def p_calpe: R = ("calpe", "calpe-comunidad-valenciana", "Calpe", "Alicante", 38.6447, 0.0445, "Europe/Madrid", true, Seq("Calpe", "Ondara"), Seq(
    ("Cine Calp", "Cine Calp", Some("E0924"), None),
    ("Cine Imf Ondara", "Cine Imf Ondara", Some("E0658"), None)
  ))
  private def p_camargo: R = ("camargo", "camargo-cantabria", "Camargo", "Cantabria", 43.4074, -3.885, "Europe/Madrid", true, Seq("Camargo", "El Astillero", "Los Corrales de Buelna"), Seq(
    ("Cine La Vidriera", "Cine La Vidriera", Some("E0257"), None),
    ("Ocine Premium Bahía Real", "Ocine Premium Bahía Real", Some("E1045"), Some("tickets.ocinepremiumbahiareal.es")),
    ("Sala Bretón", "Sala Bretón", Some("E0594"), None),
    ("Teatro De Los Corrales De Buelna", "Teatro De Los Corrales De Buelna", Some("E0599"), None)
  ))
  private def p_carballo: R = ("carballo", "carballo-galicia", "Carballo", "A Coruña", 43.213, -8.691, "Europe/Madrid", false, Seq("Carballo"), Seq(
    ("Multicines Bergantiños", "Multicines Bergantiños", Some("E0494"), None)
  ))
  private def p_cee: R = ("cee", "cee-galicia", "Cee", "A Coruña", 42.9547, -9.188, "Europe/Madrid", false, Seq("Cee"), Seq(
    ("Cines Xunqueira", "Cines Xunqueira", Some("E0694"), None)
  ))
  private def p_ceuta: R = ("ceuta", "ceuta-ceuta", "Ceuta", "Ceuta", 35.8892, -5.3204, "Europe/Madrid", false, Seq("Ceuta"), Seq(
    ("Marina Cinemas 7", "Marina Cinemas 7", Some("E0478"), None)
  ))
  private def p_ciudad_real: R = ("ciudad-real", "ciudad-real-castilla-la-mancha", "Ciudad Real", "Ciudad Real", 38.9863, -3.9291, "Europe/Madrid", false, Seq("Ciudad Real"), Seq(
    ("Parque De Ocio Las Vías", "Parque De Ocio Las Vías", Some("E0562"), None)
  ))
  private def p_ciudad_rodrigo: R = ("ciudad-rodrigo", "ciudad-rodrigo-castilla-y-leon", "Ciudad Rodrigo", "Salamanca", 40.6, -6.5333, "Europe/Madrid", false, Seq("Ciudad Rodrigo"), Seq(
    ("Cine Juventud", "Cine Juventud", Some("E0800"), None)
  ))
  private def p_ciutadella_de_menorca: R = ("ciutadella-de-menorca", "ciutadella-de-menorca-islas-baleares", "Ciutadella de Menorca", "Islas Baleares", 40.0011, 3.8414, "Europe/Madrid", false, Seq("Ciutadella de Menorca"), Seq(
    ("Cinema Ca-Los", "Cinema Ca-Los", Some("E0229"), None),
    ("Cinemes Moix Negre", "Cinemes Moix Negre", Some("E0782"), None)
  ))
  private def p_coria: R = ("coria", "coria-extremadura", "Coria", "Cáceres", 39.9841, -6.536, "Europe/Madrid", false, Seq("Coria"), Seq(
    ("Cine Coria", "Cine Coria", Some("E0832"), None)
  ))
  private def p_cornella_de_llobregat: R = ("cornella-de-llobregat", "cornella-de-llobregat-cataluna", "Cornellà de Llobregat", "Barcelona", 41.35, 2.0833, "Europe/Madrid", false, Seq("Cornellà de Llobregat"), Seq(
    ("Cinesa Llobregat Centre", "Cinesa Llobregat Centre", Some("E0857"), None),
    ("Kinépolis Barcelona Full Splau", "Kinépolis Barcelona Full Splau", Some("E0756"), None),
    ("Odeon Multicines Llobregat", "Odeon Multicines Llobregat", Some("E0521"), None)
  ))
  private def p_cortegana: R = ("cortegana", "cortegana-andalucia", "Cortegana", "Huelva", 37.8104, -6.9868, "Europe/Madrid", false, Seq("Cortegana"), Seq(
    ("Cortegana Cinema", "Cortegana Cinema", Some("E2913"), None)
  ))
  private def p_corvera_de_asturias: R = ("corvera-de-asturias", "corvera-de-asturias-asturias", "Corvera de Asturias", "Asturias", 43.5355, -5.8889, "Europe/Madrid", false, Seq("Corvera de Asturias"), Seq(
    ("Cinebox Parque Astur", "Cinebox Parque Astur", Some("E0290"), None),
    ("Odeon Multicines Parque Astur", "Odeon Multicines Parque Astur", Some("E0814"), None)
  ))
  private def p_coslada: R = ("coslada", "coslada-comunidad-de-madrid", "Coslada", "Madrid", 40.4238, -3.5613, "Europe/Madrid", true, Seq("Coslada", "Torrejón de Ardoz", "Rivas-Vaciamadrid"), Seq(
    ("Cines La Rambla", "Cines La Rambla", Some("E0353"), None),
    ("Cines Plaza Coslada", "Cines Plaza Coslada", Some("E2910"), None),
    ("Yelmo Cines Premium Parque Corredor", "Yelmo Cines Premium Parque Corredor", Some("E0291"), None),
    ("Yelmo Cines Rivas H2O", "Yelmo Cines Rivas H2O", Some("E0671"), None)
  ))
  private def p_cuenca: R = ("cuenca", "cuenca-castilla-la-mancha", "Cuenca", "Cuenca", 40.0667, -2.1333, "Europe/Madrid", false, Seq("Cuenca"), Seq(
    ("Abaco Cuenca", "Abaco Cuenca", Some("E0020"), None),
    ("Odeon Multicines Cuenca", "Odeon Multicines Cuenca", Some("E0502"), None),
    ("Odeon Multicines Mirador", "Odeon Multicines Mirador", Some("E0852"), None)
  ))
  private def p_daimiel: R = ("daimiel", "daimiel-castilla-la-mancha", "Daimiel", "Ciudad Real", 39.07, -3.615, "Europe/Madrid", false, Seq("Daimiel"), Seq(
    ("Daimiel Cinema", "Daimiel Cinema", Some("E0990"), None)
  ))
  private def p_don_benito: R = ("don-benito", "don-benito-extremadura", "Don Benito", "Badajoz", 38.9563, -5.8616, "Europe/Madrid", false, Seq("Don Benito"), Seq(
    ("Cines Victoria Don Benito", "Cines Victoria Don Benito", Some("E0372"), None)
  ))
  private def p_dos_hermanas: R = ("dos-hermanas", "dos-hermanas-andalucia", "Dos Hermanas", "Sevilla", 37.2829, -5.9209, "Europe/Madrid", true, Seq("Dos Hermanas", "Alcalá de Guadaíra", "Utrera"), Seq(
    ("Cineapolis Dos Hermanas 3D", "Cineapolis Dos Hermanas 3D", Some("E0191"), None),
    ("Cineapolis WAY", "Cineapolis WAY", Some("E1040"), None),
    ("Cineapolis Utrera", "Cineapolis Utrera", Some("E1039"), None),
    ("mk2 Alcores", "mk2 Alcores", Some("E0411"), None)
  ))
  private def p_ecija: R = ("ecija", "ecija-andalucia", "Écija", "Sevilla", 37.5422, -5.0826, "Europe/Madrid", false, Seq("Écija"), Seq(
    ("Artesiete Écija", "Artesiete Écija", Some("E0720"), None)
  ))
  private def p_eibar: R = ("eibar", "eibar-pais-vasco", "Eibar", "Guipúzcoa", 43.1849, -2.4716, "Europe/Madrid", true, Seq("Eibar", "Ermua"), Seq(
    ("Cine Modelo", "Cine Modelo", Some("E0263"), None),
    ("Teatro Coliseo", "Teatro Coliseo", Some("E0769"), None),
    ("Ermua Antzokia", "Ermua Antzokia", Some("E0903"), None)
  ))
  private def p_el_ejido: R = ("el-ejido", "el-ejido-andalucia", "El Ejido", "Almería", 36.7006, -2.7898, "Europe/Madrid", true, Seq("El Ejido", "Berja"), Seq(
    ("Cine Berja", "Cine Berja", Some("E0965"), None),
    ("Ocine Copo", "Ocine Copo", None, Some("tickets.ocinecopo.es"))
  ))
  private def p_el_pont_de_suert: R = ("el-pont-de-suert", "el-pont-de-suert-cataluna", "El Pont de Suert", "Lérida", 42.4081, 0.7417, "Europe/Madrid", false, Seq("El Pont de Suert"), Seq(
    ("Cinema Ribagorza", "Cinema Ribagorza", Some("E0313"), None)
  ))
  private def p_el_puerto_de_santa_maria: R = ("el-puerto-de-santa-maria", "el-puerto-de-santa-maria-andalucia", "El Puerto de Santa María", "Cádiz", 36.5939, -6.233, "Europe/Madrid", true, Seq("El Puerto de Santa María", "Sanlúcar de Barrameda", "Chipiona"), Seq(
    ("Al-Andalus Sanlucar", "Al-Andalus Sanlucar", Some("E0218"), None),
    ("Cine Alba Chipiona", "Cine Alba Chipiona", Some("E0912"), None),
    ("Multicines Bahia Mar", "Multicines Bahia Mar", Some("E0491"), None)
  ))
  private def p_el_vendrell: R = ("el-vendrell", "el-vendrell-cataluna", "El Vendrell", "Tarragona", 41.2167, 1.5333, "Europe/Madrid", true, Seq("El Vendrell", "Calafell"), Seq(
    ("MCB Calafell", "MCB Calafell", Some("E0479"), None),
    ("Ocine El Vendrell", "Ocine El Vendrell", None, Some("tickets.ocinevendrell.es"))
  ))
  private def p_estella_lizarra: R = ("estella-lizarra", "estella-lizarra-navarra", "Estella-Lizarra", "Navarra", 42.6718, -2.0323, "Europe/Madrid", false, Seq("Estella-Lizarra"), Seq(
    ("Cines Los Llanos Zinemak", "Cines Los Llanos Zinemak", Some("E0259"), None)
  ))
  private def p_estepa: R = ("estepa", "estepa-andalucia", "Estepa", "Sevilla", 37.2926, -4.879, "Europe/Madrid", false, Seq("Estepa"), Seq(
    ("Cine Méliès Estepa", "Cine Méliès Estepa", Some("E0996"), None)
  ))
  private def p_ferrol: R = ("ferrol", "ferrol-galicia", "Ferrol", "A Coruña", 43.4845, -8.2329, "Europe/Madrid", true, Seq("Ferrol", "Narón"), Seq(
    ("Cine Duplex", "Cine Duplex", Some("E0741"), None),
    ("Odeon Multicines Narón", "Odeon Multicines Narón", Some("E0789"), None)
  ))
  private def p_figueres: R = ("figueres", "figueres-cataluna", "Figueres", "Girona", 42.2664, 2.9616, "Europe/Madrid", true, Seq("Figueres", "Roses"), Seq(
    ("Cat Cinemes", "Cat Cinemes", Some("E0345"), None),
    ("Cinemes Roses", "Cinemes Roses", Some("E0324"), None)
  ))
  private def p_fuengirola: R = ("fuengirola", "fuengirola-andalucia", "Fuengirola", "Málaga", 36.54, -4.6247, "Europe/Madrid", false, Seq("Fuengirola"), Seq(
    ("Multicines Alfil 3D", "Multicines Alfil 3D", Some("E0059"), None),
    ("mk2 Miramar", "mk2 Miramar", Some("E0414"), None)
  ))
  private def p_galdakao: R = ("galdakao", "galdakao-pais-vasco", "Galdakao", "Vizcaya", 43.2307, -2.8429, "Europe/Madrid", true, Seq("Galdakao", "Durango", "Amorebieta-Etxano", "Llodio", "Gernika-Lumo", "Zalla", "Zigoitia"), Seq(
    ("Cine Torrezabal", "Cine Torrezabal", Some("E0923"), None),
    ("Cine Zugaza", "Cine Zugaza", Some("E0768"), None),
    ("Liceo Antzokia", "Liceo Antzokia", Some("E0894"), None),
    ("Zalla Zine - Antzokia ", "Zalla Zine - Antzokia ", Some("E0874"), None),
    ("Zornotza Aretoa", "Zornotza Aretoa", Some("E0904"), None),
    ("Cine Municipal Llodio", "Cine Municipal Llodio", Some("E0821"), None),
    ("Cines Gorbeia Zinemak ", "Cines Gorbeia Zinemak ", Some("E0885"), None)
  ))
  private def p_gandia: R = ("gandia", "gandia-comunidad-valenciana", "Gandia", "Valencia", 38.9667, -0.1833, "Europe/Madrid", false, Seq("Gandia"), Seq(
    ("Abc Gandia", "Abc Gandia", Some("E0036"), None),
    ("Cine de Verano Tugar", "Cine de Verano Tugar", Some("E0929"), None),
    ("Cines Axion Premium Gandía", "Cines Axion Premium Gandía", Some("E1026"), None),
    ("Ozone Gandía", "Ozone Gandía", Some("E0282"), None)
  ))
  private def p_girona: R = ("girona", "girona-cataluna", "Girona", "Girona", 41.9831, 2.8249, "Europe/Madrid", true, Seq("Girona", "Salt"), Seq(
    ("Cinema Truffaut", "Cinema Truffaut", Some("E0316"), None),
    ("Ocine Girona", "Ocine Girona", Some("E0362"), Some("tickets.ocinegirona.es")),
    ("Odeon Multicines Girona", "Odeon Multicines Girona", Some("E0281"), None)
  ))
  private def p_golmayo: R = ("golmayo", "golmayo-castilla-y-leon", "Golmayo", "Soria", 41.7662, -2.5227, "Europe/Madrid", false, Seq("Golmayo"), Seq(
    ("Cines Lara", "Cines Lara", Some("E0356"), None)
  ))
  private def p_granollers: R = ("granollers", "granollers-cataluna", "Granollers", "Barcelona", 41.608, 2.2877, "Europe/Madrid", true, Seq("Granollers", "Sant Celoni", "La Garriga", "Cànoves i Samalús"), Seq(
    ("Cine Alhambra", "Cine Alhambra", Some("E0827"), None),
    ("Cinema Edison", "Cinema Edison", Some("G02RB"), None),
    ("Ocine Granollers", "Ocine Granollers", Some("E0507"), Some("tickets.ocinegranollers.es")),
    ("Cinema Esbarjo", "Cinema Esbarjo", Some("E0889"), None),
    ("Ocine Sant Celoni Altrium", "Ocine Sant Celoni Altrium", Some("E0745"), None)
  ))
  private def p_guadalajara: R = ("guadalajara", "guadalajara-castilla-la-mancha", "Guadalajara", "Guadalajara", 40.6286, -3.1618, "Europe/Madrid", true, Seq("Guadalajara", "Azuqueca de Henares"), Seq(
    ("Cultura Azuqueca", "Cultura Azuqueca", Some("E0955"), None),
    ("Multicines Guadalajara", "Multicines Guadalajara", Some("E0511"), None)
  ))
  private def p_guardo: R = ("guardo", "guardo-castilla-y-leon", "Guardo", "Palencia", 42.7897, -4.8482, "Europe/Madrid", true, Seq("Guardo", "Cistierna"), Seq(
    ("Cine Marí", "Cine Marí", Some("E0901"), None),
    ("Cine AMGu", "Cine AMGu", Some("E0998"), None)
  ))
  private def p_herrera_del_duque: R = ("herrera-del-duque", "herrera-del-duque-extremadura", "Herrera del Duque", "Badajoz", 39.1684, -5.0505, "Europe/Madrid", false, Seq("Herrera del Duque"), Seq(
    ("Cine Municipal Herrera del Duque", "Cine Municipal Herrera del Duque", Some("E1027"), None)
  ))
  private def p_huarte: R = ("huarte", "huarte-navarra", "Huarte", "Navarra", 42.8304, -1.5909, "Europe/Madrid", true, Seq("Huarte", "Galar"), Seq(
    ("Golem La Morea", "Golem La Morea", Some("E0443"), None),
    ("Yelmo Cines Itaroa", "Yelmo Cines Itaroa", Some("E0283"), None)
  ))
  private def p_huelva: R = ("huelva", "huelva-andalucia", "Huelva", "Huelva", 37.2664, -6.94, "Europe/Madrid", true, Seq("Huelva", "Punta Umbría", "Palos de la Frontera"), Seq(
    ("Al-Andalus Punta Umbría 3D", "Al-Andalus Punta Umbría 3D", Some("E0641"), None),
    ("Artesiete Holea", "Artesiete Holea", Some("E0805"), None),
    ("Cines Aqualón", "Cines Aqualón", Some("E0278"), None),
    ("Cine Alba Mazagón", "Cine Alba Mazagón", Some("E0995"), None)
  ))
  private def p_huesca: R = ("huesca", "huesca-aragon", "Huesca", "Huesca", 42.1362, -0.4087, "Europe/Madrid", false, Seq("Huesca"), Seq(
    ("CineMundo Huesca", "CineMundo Huesca", Some("E0497"), None)
  ))
  private def p_huetor_tajar: R = ("huetor-tajar", "huetor-tajar-andalucia", "Huétor-Tájar", "Granada", 37.1983, -4.0469, "Europe/Madrid", false, Seq("Huétor-Tájar"), Seq(
    ("Huétor Cinema", "Huétor Cinema", Some("E0981"), None)
  ))
  private def p_ibiza: R = ("ibiza", "ibiza-islas-baleares", "Ibiza", "Islas Baleares", 38.9088, 1.433, "Europe/Madrid", true, Seq("Ibiza", "Santa Eulària des Riu", "Sant Antoni de Portmany"), Seq(
    ("Cine Regio", "Cine Regio", Some("E0839"), None),
    ("Multicines Eivissa", "Multicines Eivissa", Some("E0503"), None),
    ("Teatro España", "Teatro España", Some("E0755"), None)
  ))
  private def p_iniesta: R = ("iniesta", "iniesta-castilla-la-mancha", "Iniesta", "Cuenca", 39.4333, -1.75, "Europe/Madrid", false, Seq("Iniesta"), Seq(
    ("Cine Iniesta", "Cine Iniesta", Some("E1031"), None)
  ))
  private def p_irun: R = ("irun", "irun-pais-vasco", "Irun", "Guipúzcoa", 43.339, -1.7894, "Europe/Madrid", true, Seq("Irun", "Renteria", "Usurbil"), Seq(
    ("Cinesa Urbil", "Cinesa Urbil", Some("E0296"), None),
    ("Multicines Niessen Zinemak", "Multicines Niessen Zinemak", Some("E0637"), None),
    ("Ocine Mendibil", "Ocine Mendibil", Some("E0537"), Some("tickets.ocinemendibil.es"))
  ))
  private def p_jaraiz_de_la_vera: R = ("jaraiz-de-la-vera", "jaraiz-de-la-vera-extremadura", "Jaraíz de la Vera", "Cáceres", 40.06, -5.7543, "Europe/Madrid", false, Seq("Jaraíz de la Vera"), Seq(
    ("Cine Avenida Jaraíz", "Cine Avenida Jaraíz", Some("E0867"), None)
  ))
  private def p_javea: R = ("javea", "javea-comunidad-valenciana", "Javea", "Alicante", 38.7833, 0.1667, "Europe/Madrid", true, Seq("Javea", "Dénia"), Seq(
    ("Auto Cine Drive In", "Auto Cine Drive In", Some("E0197"), None),
    ("Cine Club Xábia", "Cine Club Xábia", Some("E0249"), None),
    ("Cine Jayan", "Cine Jayan", Some("E0254"), None)
  ))
  private def p_la_orotava: R = ("la-orotava", "la-orotava-canarias", "La Orotava", "Santa Cruz de Tenerife", 28.3908, -16.5231, "Atlantic/Canary", true, Seq("La Orotava", "Los Realejos", "Candelaria"), Seq(
    ("Cine Realejos", "Cine Realejos", Some("E0267"), None),
    ("Multicines Puntalarga", "Multicines Puntalarga", Some("E0532"), None),
    ("Yelmo Cines La Villa de Orotava", "Yelmo Cines La Villa de Orotava", Some("E0622"), None)
  ))
  private def p_la_palma_del_condado: R = ("la-palma-del-condado", "la-palma-del-condado-andalucia", "La Palma del Condado", "Huelva", 37.386, -6.5523, "Europe/Madrid", false, Seq("La Palma del Condado"), Seq(
    ("Condado Cinemas 7", "Condado Cinemas 7", Some("E0429"), None)
  ))
  private def p_la_seu_d_urgell: R = ("la-seu-d-urgell", "la-seu-d-urgell-cataluna", "La Seu d'Urgell", "Lérida", 42.3588, 1.4614, "Europe/Madrid", false, Seq("La Seu d'Urgell"), Seq(
    ("Cinemes Guiu", "Cinemes Guiu", Some("E0319"), None)
  ))
  private def p_la_zubia: R = ("la-zubia", "la-zubia-andalucia", "La Zubia", "Granada", 37.112, -3.5714, "Europe/Madrid", true, Seq("La Zubia", "Armilla", "Pulianas"), Seq(
    ("Artesiete Alhsur", "Artesiete Alhsur", Some("E0722"), None),
    ("Cine Liszt Terraza de verano", "Cine Liszt Terraza de verano", Some("E0743"), None),
    ("Kinépolis Granada", "Kinépolis Granada", Some("E0452"), None),
    ("Kinépolis Nevada", "Kinépolis Nevada", Some("E0866"), None)
  ))
  private def p_laredo: R = ("laredo", "laredo-cantabria", "Laredo", "Cantabria", 43.4098, -3.4161, "Europe/Madrid", true, Seq("Laredo", "Santoña", "Noja"), Seq(
    ("Casa de Cultura Doctor Velasco ", "Casa de Cultura Doctor Velasco ", Some("E0917"), None),
    ("Cine Playa Dorada", "Cine Playa Dorada", Some("E0567"), None),
    ("Teatro Casino Liceo de Santoña", "Teatro Casino Liceo de Santoña", Some("E0918"), None)
  ))
  private def p_leiro: R = ("leiro", "leiro-galicia", "Leiro", "Ourense", 42.35, -8.1333, "Europe/Madrid", false, Seq("Leiro"), Seq(
    ("NovoCine Leiro 3D", "NovoCine Leiro 3D", Some("E0847"), None)
  ))
  private def p_leon: R = ("leon", "leon-castilla-y-leon", "León", "León", 42.6, -5.5703, "Europe/Madrid", false, Seq("León"), Seq(
    ("Cines Van Gogh", "Cines Van Gogh", Some("E0369"), None),
    ("Odeon Multicines León", "Odeon Multicines León", Some("E0280"), None)
  ))
  private def p_linares: R = ("linares", "linares-andalucia", "Linares", "Jaén", 38.0952, -3.636, "Europe/Madrid", true, Seq("Linares", "Úbeda", "La Carolina"), Seq(
    ("Multicines Bowling", "Multicines Bowling", Some("E0239"), None),
    ("Multicines Carolina", "Multicines Carolina", Some("E0823"), None),
    ("Multicines Úbeda", "Multicines Úbeda", Some("E0538"), None)
  ))
  private def p_lleida: R = ("lleida", "lleida-cataluna", "Lleida", "Lérida", 41.6167, 0.6222, "Europe/Madrid", true, Seq("Lleida", "Balaguer", "Almacelles", "Alpicat"), Seq(
    ("Cinema El Casal", "Cinema El Casal", Some("E0650"), None),
    ("Jca Cinemes Alpicat", "Jca Cinemes Alpicat", Some("E0652"), None),
    ("Sala d''actes Ajuntament", "Sala d''actes Ajuntament", Some("E0985"), None),
    ("Ocine Premium Lleida", "Ocine Premium Lleida", None, Some("tickets.ocinepremiumlleida.es"))
  ))
  private def p_lorca: R = ("lorca", "lorca-region-de-murcia", "Lorca", "Murcia", 37.6712, -1.7017, "Europe/Madrid", false, Seq("Lorca"), Seq(
    ("Cines Almenara Lorca", "Cines Almenara Lorca", Some("E0751"), None)
  ))
  private def p_los_llanos_de_aridane: R = ("los-llanos-de-aridane", "los-llanos-de-aridane-canarias", "Los Llanos de Aridane", "Santa Cruz de Tenerife", 28.6585, -17.9182, "Atlantic/Canary", true, Seq("Los Llanos de Aridane", "Santa Cruz de la Palma"), Seq(
    ("Multicines Millennium", "Multicines Millennium", Some("E0940"), None),
    ("Teatro Chico", "Teatro Chico", Some("E0900"), None)
  ))
  private def p_lucena: R = ("lucena", "lucena-andalucia", "Lucena", "Córdoba", 37.4088, -4.4852, "Europe/Madrid", true, Seq("Lucena", "Cabra", "Baena"), Seq(
    ("Artesiete Lucena", "Artesiete Lucena", Some("E0489"), None),
    ("Cine Baena", "Cine Baena", Some("E0915"), None),
    ("Cinestudio Municipal Cabra", "Cinestudio Municipal Cabra", Some("E0868"), None)
  ))
  private def p_lugo: R = ("lugo", "lugo-galicia", "Lugo", "Lugo", 43.0099, -7.556, "Europe/Madrid", false, Seq("Lugo"), Seq(
    ("Multicines Cristal", "Multicines Cristal", Some("E0501"), None),
    ("Yelmo Cines As Termas", "Yelmo Cines As Termas", Some("E0684"), None)
  ))
  private def p_mairena_del_aljarafe: R = ("mairena-del-aljarafe", "mairena-del-aljarafe-andalucia", "Mairena del Aljarafe", "Sevilla", 37.3446, -6.0639, "Europe/Madrid", true, Seq("Mairena del Aljarafe", "Coria del Río", "Camas", "Tomares", "Bormujos"), Seq(
    ("Centro Cultural de la Villa - Pastora Soler", "Centro Cultural de la Villa - Pastora Soler", Some("E1016"), None),
    ("Al-Andalus Mega Ocio", "Al-Andalus Mega Ocio", Some("E0217"), None),
    ("Cinema Tomares", "Cinema Tomares", Some("E0676"), None),
    ("Cinesa Camas", "Cinesa Camas", Some("E0027"), None),
    ("Metromar Cinemas 12", "Metromar Cinemas 12", Some("E0666"), None)
  ))
  private def p_majadahonda: R = ("majadahonda", "majadahonda-comunidad-de-madrid", "Majadahonda", "Madrid", 40.4735, -3.8718, "Europe/Madrid", true, Seq("Majadahonda", "Boadilla del Monte", "Las Rozas de Madrid", "Pozuelo de Alarcón"), Seq(
    ("Cine Los Molinos", "Cine Los Molinos", Some("E0729"), None),
    ("Cines Boadilla", "Cines Boadilla", Some("E0760"), None),
    ("Cines Zoco Majadahonda", "Cines Zoco Majadahonda", Some("E0582"), None),
    ("Cinesa Equinoccio", "Cinesa Equinoccio", Some("E0385"), None),
    ("Cinesa Heron City Las Rozas", "Cinesa Heron City Las Rozas", Some("E0389"), None),
    ("Kinépolis Madrid", "Kinépolis Madrid", Some("E0453"), None)
  ))
  private def p_manacor: R = ("manacor", "manacor-islas-baleares", "Manacor", "Islas Baleares", 39.5696, 3.2096, "Europe/Madrid", false, Seq("Manacor"), Seq(
    ("Multicines Manacor", "Multicines Manacor", Some("E0522"), None)
  ))
  private def p_manresa: R = ("manresa", "manresa-cataluna", "Manresa", "Barcelona", 41.7281, 1.824, "Europe/Madrid", true, Seq("Manresa", "Igualada", "Santa Margarida de Montbui"), Seq(
    ("Ateneu Cinema ", "Ateneu Cinema ", Some("E0906"), None),
    ("Mont-Àgora Cinemes", "Mont-Àgora Cinemes", Some("E1006"), None),
    ("Multicines Bages 3D", "Multicines Bages 3D", Some("E0120"), None)
  ))
  private def p_mao: R = ("mao", "mao-islas-baleares", "Maó", "Islas Baleares", 39.8885, 4.2658, "Europe/Madrid", false, Seq("Maó"), Seq(
    ("Ocimax Multisalas", "Ocimax Multisalas", Some("E0639"), None)
  ))
  private def p_marbella: R = ("marbella", "marbella-andalucia", "Marbella", "Málaga", 36.5154, -4.8858, "Europe/Madrid", false, Seq("Marbella"), Seq(
    ("Cines Gran Marbella", "Cines Gran Marbella", Some("E0427"), None),
    ("Kinépolis La Cañada", "Kinépolis La Cañada", Some("E0390"), None),
    ("Red Dog Cinemas", "Red Dog Cinemas", Some("E0845"), None)
  ))
  private def p_marchena: R = ("marchena", "marchena-andalucia", "Marchena", "Sevilla", 37.329, -5.4168, "Europe/Madrid", false, Seq("Marchena"), Seq(
    ("Cine Planelles", "Cine Planelles", Some("E0724"), None)
  ))
  private def p_marratxi: R = ("marratxi", "marratxi-islas-baleares", "Marratxí", "Islas Baleares", 39.6214, 2.7253, "Europe/Madrid", false, Seq("Marratxí"), Seq(
    ("Cinesa Festival Park", "Cinesa Festival Park", Some("E0386"), None)
  ))
  private def p_martos: R = ("martos", "martos-andalucia", "Martos", "Jaén", 37.7211, -3.9726, "Europe/Madrid", false, Seq("Martos"), Seq(
    ("Teatro Maestro Álvarez Alonso", "Teatro Maestro Álvarez Alonso", Some("E1017"), None)
  ))
  private def p_mazarron: R = ("mazarron", "mazarron-region-de-murcia", "Mazarrón", "Murcia", 37.5992, -1.3149, "Europe/Madrid", false, Seq("Mazarrón"), Seq(
    ("Cine Bahía", "Cine Bahía", Some("E0977"), None)
  ))
  private def p_medina_de_rioseco: R = ("medina-de-rioseco", "medina-de-rioseco-castilla-y-leon", "Medina de Ríoseco", "Valladolid", 41.8833, -5.0441, "Europe/Madrid", false, Seq("Medina de Ríoseco"), Seq(
    ("Teatro Principal", "Teatro Principal", Some("E0600"), None)
  ))
  private def p_medina_del_campo: R = ("medina-del-campo", "medina-del-campo-castilla-y-leon", "Medina del Campo", "Valladolid", 41.3124, -4.9141, "Europe/Madrid", false, Seq("Medina del Campo"), Seq(
    ("Multicines Coliseo", "Multicines Coliseo", Some("E0698"), None)
  ))
  private def p_melilla: R = ("melilla", "melilla-melilla", "Melilla", "Melilla", 35.2937, -2.9383, "Europe/Madrid", false, Seq("Melilla"), Seq(
    ("Cine Teatro Perelló", "Cine Teatro Perelló", Some("E0841"), None)
  ))
  private def p_mequinenza: R = ("mequinenza", "mequinenza-aragon", "Mequinenza", "Zaragoza", 41.3721, 0.3017, "Europe/Madrid", false, Seq("Mequinenza"), Seq(
    ("Sala Goya", "Sala Goya", Some("E0595"), None)
  ))
  private def p_merida: R = ("merida", "merida-extremadura", "Mérida", "Badajoz", 38.918, -6.3429, "Europe/Madrid", false, Seq("Mérida"), Seq(
    ("Cines Victoria Mérida", "Cines Victoria Mérida", Some("E0383"), None)
  ))
  private def p_miranda_de_ebro: R = ("miranda-de-ebro", "miranda-de-ebro-castilla-y-leon", "Miranda de Ebro", "Burgos", 42.6865, -2.947, "Europe/Madrid", true, Seq("Miranda de Ebro", "Haro", "Santo Domingo de la Calzada"), Seq(
    ("Cine Novedades Miranda de Ebro", "Cine Novedades Miranda de Ebro", Some("E0647"), None),
    ("Cine Avenida Santo Domingo", "Cine Avenida Santo Domingo", Some("E0830"), None),
    ("Teatro Bretón", "Teatro Bretón", Some("E0986"), None)
  ))
  private def p_molina_de_segura: R = ("molina-de-segura", "molina-de-segura-region-de-murcia", "Molina de Segura", "Murcia", 38.0546, -1.2076, "Europe/Madrid", true, Seq("Molina de Segura", "Archena", "Mula", "Abarán"), Seq(
    ("Cine de Verano Abarán", "Cine de Verano Abarán", Some("E0938"), None),
    ("Cine de Verano de Archena", "Cine de Verano de Archena", Some("E0962"), None),
    ("Neocine HD Digital Vega Plaza", "Neocine HD Digital Vega Plaza", Some("E0660"), None),
    ("Terraza Centro Joven", "Terraza Centro Joven", Some("E0988"), None)
  ))
  private def p_mollerussa: R = ("mollerussa", "mollerussa-cataluna", "Mollerussa", "Lérida", 41.6333, 0.9, "Europe/Madrid", false, Seq("Mollerussa"), Seq(
    ("Autocine Resquitx - Golmés", "Autocine Resquitx - Golmés", Some("E1033"), None),
    ("Cinema Mollerussa", "Cinema Mollerussa", Some("E0973"), None),
    ("Cinemes Urgell", "Cinemes Urgell", Some("E0325"), None)
  ))
  private def p_monforte_de_lemos: R = ("monforte-de-lemos", "monforte-de-lemos-galicia", "Monforte de Lemos", "Lugo", 42.5217, -7.5142, "Europe/Madrid", false, Seq("Monforte de Lemos"), Seq(
    ("Multicines Hollywood", "Multicines Hollywood", Some("E0513"), None)
  ))
  private def p_motril: R = ("motril", "motril-andalucia", "Motril", "Granada", 36.7507, -3.5179, "Europe/Madrid", true, Seq("Motril", "Almuñécar", "Salobreña"), Seq(
    ("Cañaveral Cinema", "Cañaveral Cinema", Some("E0960"), None),
    ("Cine San Cristobal", "Cine San Cristobal", Some("E0952"), None),
    ("Motril Cinema", "Motril Cinema", Some("E0869"), None)
  ))
  private def p_mungia: R = ("mungia", "mungia-pais-vasco", "Mungia", "Vizcaya", 43.3546, -2.8452, "Europe/Madrid", false, Seq("Mungia"), Seq(
    ("Cine Torrebillela", "Cine Torrebillela", Some("E0767"), None),
    ("Olalde Aretoa", "Olalde Aretoa", Some("E1021"), None)
  ))
  private def p_navalmoral_de_la_mata: R = ("navalmoral-de-la-mata", "navalmoral-de-la-mata-extremadura", "Navalmoral de la Mata", "Cáceres", 39.8916, -5.5406, "Europe/Madrid", false, Seq("Navalmoral de la Mata"), Seq(
    ("Cines Navalmoral", "Cines Navalmoral", Some("E0886"), None)
  ))
  private def p_navia: R = ("navia", "navia-asturias", "Navia", "Asturias", 43.5354, -6.7194, "Europe/Madrid", false, Seq("Navia"), Seq(
    ("Cine Fantasio Navia", "Cine Fantasio Navia", Some("E1037"), None)
  ))
  private def p_oliva: R = ("oliva", "oliva-comunidad-valenciana", "Oliva", "Valencia", 38.9197, -0.1193, "Europe/Madrid", true, Seq("Oliva", "Guardamar de la Safor"), Seq(
    ("Cine Terraza Charly", "Cine Terraza Charly", Some("E0927"), None),
    ("Terraza de Verano Oliva", "Terraza de Verano Oliva", Some("E0730"), None)
  ))
  private def p_olot: R = ("olot", "olot-cataluna", "Olot", "Girona", 42.181, 2.4901, "Europe/Madrid", true, Seq("Olot", "Ripoll"), Seq(
    ("Cinema Teatre Comtal", "Cinema Teatre Comtal", Some("E0315"), None),
    ("Multicines Olot", "Multicines Olot", Some("E0323"), None)
  ))
  private def p_orihuela: R = ("orihuela", "orihuela-comunidad-valenciana", "Orihuela", "Alicante", 38.0848, -0.944, "Europe/Madrid", true, Seq("Orihuela", "Callosa de Segura"), Seq(
    ("Cine Navia", "Cine Navia", Some("E0970"), None),
    ("Cines Axion de Orihuela", "Cines Axion de Orihuela", Some("E0552"), None),
    ("Terraza Imperial - Cine de Verano", "Terraza Imperial - Cine de Verano", Some("E0950"), None)
  ))
  private def p_ourense: R = ("ourense", "ourense-galicia", "Ourense", "Ourense", 42.3367, -7.8641, "Europe/Madrid", false, Seq("Ourense"), Seq(
    ("Cinebox Ourense", "Cinebox Ourense", Some("E0289"), None),
    ("Multicines Ponte Vella", "Multicines Ponte Vella", Some("E0813"), None)
  ))
  private def p_palafrugell: R = ("palafrugell", "palafrugell-cataluna", "Palafrugell", "Girona", 41.9174, 3.1631, "Europe/Madrid", true, Seq("Palafrugell", "Palamós", "Castell-Platja d'Aro", "Begur", "Fontanilles"), Seq(
    ("Cinema Casino", "Cinema Casino", Some("E0969"), None),
    ("Cinema Kyton", "Cinema Kyton", Some("E0456"), None),
    ("Cinema Montgrí", "Cinema Montgrí", Some("E0896"), None),
    ("Ocine Platja d'Aro", "Ocine Platja d'Aro", Some("E0554"), Some("tickets.ocineplatjadaro.es")),
    ("Teatro Municipal de Palafrugel", "Teatro Municipal de Palafrugel", Some("E0978"), None)
  ))
  private def p_palencia: R = ("palencia", "palencia-castilla-y-leon", "Palencia", "Palencia", 42.0095, -4.5241, "Europe/Madrid", false, Seq("Palencia"), Seq(
    ("Cines Ortega", "Cines Ortega", Some("E0361"), None),
    ("Multicines Avenida", "Multicines Avenida", Some("E0331"), None)
  ))
  private def p_paterna: R = ("paterna", "paterna-comunidad-valenciana", "Paterna", "Valencia", 39.5026, -0.4408, "Europe/Madrid", true, Seq("Paterna", "Burjassot", "Aldaia", "Xirivella", "Llíria", "Alboraya", "Alfafar", "L'Eliana", "Benetússer", "Almussafes", "Serra"), Seq(
    ("Abc Gran Turia", "Abc Gran Turia", Some("E0037"), None),
    ("Centre Cultural Almassafes", "Centre Cultural Almassafes", Some("E0759"), None),
    ("Centre Cultural Benetússer El Molí", "Centre Cultural Benetússer El Molí", Some("E0758"), None),
    ("Cine La Unió Musical", "Cine La Unió Musical", Some("E0992"), None),
    ("Cine Tívoli", "Cine Tívoli", Some("E0645"), None),
    ("Cine de Verano Serra", "Cine de Verano Serra", Some("E0930"), None),
    ("Cines MN4", "Cines MN4", Some("E0287"), None),
    ("Cinesa Bonaire", "Cinesa Bonaire", Some("E0405"), None),
    ("Kinépolis Valencia", "Kinépolis Valencia", Some("E0454"), None),
    ("Terraza Lumiere", "Terraza Lumiere", Some("E0931"), None),
    ("Terraza de Verano", "Terraza de Verano", Some("E0987"), None)
  ))
  private def p_pedrajas_de_san_esteban: R = ("pedrajas-de-san-esteban", "pedrajas-de-san-esteban-castilla-y-leon", "Pedrajas de San Esteban", "Valladolid", 41.3415, -4.5823, "Europe/Madrid", false, Seq("Pedrajas de San Esteban"), Seq(
    ("Cine Avenida", "Cine Avenida", Some("E0235"), None)
  ))
  private def p_penaranda_de_bracamonte: R = ("penaranda-de-bracamonte", "penaranda-de-bracamonte-castilla-y-leon", "Peñaranda de Bracamonte", "Salamanca", 40.9011, -5.2003, "Europe/Madrid", false, Seq("Peñaranda de Bracamonte"), Seq(
    ("Cine Calderón", "Cine Calderón", Some("E0793"), None)
  ))
  private def p_penarroya_pueblonuevo: R = ("penarroya-pueblonuevo", "penarroya-pueblonuevo-andalucia", "Peñarroya-Pueblonuevo", "Córdoba", 38.3, -5.2667, "Europe/Madrid", false, Seq("Peñarroya-Pueblonuevo"), Seq(
    ("Peñarroya Cinema", "Peñarroya Cinema", Some("E1009"), None)
  ))
  private def p_petrer: R = ("petrer", "petrer-comunidad-valenciana", "Petrer", "Alicante", 38.4913, -0.7384, "Europe/Madrid", false, Seq("Petrer"), Seq(
    ("CinesMax 3D Petrer", "CinesMax 3D Petrer", Some("E0728"), None),
    ("Yelmo Cines Vinalopo", "Yelmo Cines Vinalopo", Some("E0636"), None)
  ))
  private def p_pilar_de_la_horadada: R = ("pilar-de-la-horadada", "pilar-de-la-horadada-comunidad-valenciana", "Pilar de la Horadada", "Alicante", 37.8659, -0.7926, "Europe/Madrid", true, Seq("Pilar de la Horadada", "Torrevieja", "San Pedro del Pinatar"), Seq(
    ("Cine Horadada", "Cine Horadada", Some("E0968"), None),
    ("Cine Las Villas", "Cine Las Villas", Some("E0971"), None),
    ("Cine Imf Torrevieja", "Cine Imf Torrevieja", Some("E0451"), None),
    ("Cine Acapulco", "Cine Acapulco", Some("E0942"), None)
  ))
  private def p_plasencia: R = ("plasencia", "plasencia-extremadura", "Plasencia", "Cáceres", 40.0312, -6.0884, "Europe/Madrid", false, Seq("Plasencia"), Seq(
    ("Multicines Alkázar", "Multicines Alkázar", Some("E0067"), None)
  ))
  private def p_ponferrada: R = ("ponferrada", "ponferrada-castilla-y-leon", "Ponferrada", "León", 42.5466, -6.5962, "Europe/Madrid", false, Seq("Ponferrada"), Seq(
    ("La Dehesa Ponferrada", "La Dehesa Ponferrada", Some("E0458"), None)
  ))
  private def p_pontevedra: R = ("pontevedra", "pontevedra-galicia", "Pontevedra", "Pontevedra", 42.431, -8.6443, "Europe/Madrid", true, Seq("Pontevedra", "Marín", "Nigrán"), Seq(
    ("Cine Club Pontevedra", "Cine Club Pontevedra", Some("E0875"), None),
    ("Multicines Cinexpo", "Multicines Cinexpo", Some("E0298"), None),
    ("Cine Imperial", "Cine Imperial", Some("E0835"), None),
    ("Cine Seixo", "Cine Seixo", Some("E0899"), None)
  ))
  private def p_pozoblanco: R = ("pozoblanco", "pozoblanco-andalucia", "Pozoblanco", "Córdoba", 38.3791, -4.8483, "Europe/Madrid", false, Seq("Pozoblanco"), Seq(
    ("Cine Pósito", "Cine Pósito", Some("E0859"), None)
  ))
  private def p_premia_de_mar: R = ("premia-de-mar", "premia-de-mar-cataluna", "Premià de Mar", "Barcelona", 41.4921, 2.3652, "Europe/Madrid", true, Seq("Premià de Mar", "Mataró", "El Masnou"), Seq(
    ("Cine la Calandria", "Cine la Calandria", Some("E0836"), None),
    ("Cinema Teatre Patronat", "Cinema Teatre Patronat", Some("E0858"), None),
    ("Espai L'Amistat", "Espai L'Amistat", Some("E0919"), None),
    ("Kinépolis Mataró Parc", "Kinépolis Mataró Parc", Some("E0396"), None)
  ))
  private def p_puerto_del_rosario: R = ("puerto-del-rosario", "puerto-del-rosario-canarias", "Puerto del Rosario", "Las Palmas", 28.5004, -13.8627, "Atlantic/Canary", true, Seq("Puerto del Rosario", "Antigua"), Seq(
    ("Odeón Puerto del Rosario", "Odeón Puerto del Rosario", Some("E0898"), None),
    ("Yelmo Cines Fuerteventura", "Yelmo Cines Fuerteventura", Some("E0618"), None)
  ))
  private def p_puertollano: R = ("puertollano", "puertollano-castilla-la-mancha", "Puertollano", "Ciudad Real", 38.6871, -4.1073, "Europe/Madrid", false, Seq("Puertollano"), Seq(
    ("Multicines Ortega", "Multicines Ortega", Some("E0526"), None)
  ))
  private def p_requena: R = ("requena", "requena-comunidad-valenciana", "Requena", "Valencia", 39.4883, -1.1004, "Europe/Madrid", false, Seq("Requena"), Seq(
    ("Cine Teatro Principal Requena", "Cine Teatro Principal Requena", Some("E1029"), None),
    ("Teatro García Berlanga", "Teatro García Berlanga", Some("E1030"), None)
  ))
  private def p_reus: R = ("reus", "reus-cataluna", "Reus", "Tarragona", 41.1561, 1.1069, "Europe/Madrid", true, Seq("Reus", "Cambrils", "Valls", "Montblanc", "Altafulla"), Seq(
    ("Cinema Casal Montblanquí", "Cinema Casal Montblanquí", Some("E0161"), None),
    ("Cines Axion Reus", "Cines Axion Reus", Some("E0920"), None),
    ("JCA Cinemes Tarragona Valls", "JCA Cinemes Tarragona Valls", Some("E0908"), None),
    ("MCB Altafulla - Les Bruixes", "MCB Altafulla - Les Bruixes", Some("E0320"), None),
    ("Rambla de L'art", "Rambla de L'art", Some("E0811"), None)
  ))
  private def p_ribadeo: R = ("ribadeo", "ribadeo-galicia", "Ribadeo", "Lugo", 43.537, -7.0409, "Europe/Madrid", false, Seq("Ribadeo"), Seq(
    ("Cine Ribadeo", "Cine Ribadeo", Some("E0740"), None)
  ))
  private def p_ronda: R = ("ronda", "ronda-andalucia", "Ronda", "Málaga", 36.7423, -5.1671, "Europe/Madrid", false, Seq("Ronda"), Seq(
    ("Multicines Ronda", "Multicines Ronda", Some("E0534"), None)
  ))
  private def p_roquetas_de_mar: R = ("roquetas-de-mar", "roquetas-de-mar-andalucia", "Roquetas de Mar", "Almería", 36.7642, -2.6147, "Europe/Madrid", false, Seq("Roquetas de Mar"), Seq(
    ("Cine de verano Aguadulce", "Cine de verano Aguadulce", Some("E0943"), None),
    ("Yelmo Cines Roquetas", "Yelmo Cines Roquetas", Some("E0620"), None)
  ))
  private def p_rota: R = ("rota", "rota-andalucia", "Rota", "Cádiz", 36.6236, -6.36, "Europe/Madrid", false, Seq("Rota"), Seq(
    ("Cines Victoria Rota", "Cines Victoria Rota", Some("E0766"), None),
    ("Portalejo Cinemas", "Portalejo Cinemas", Some("E0568"), None)
  ))
  private def p_sabinanigo: R = ("sabinanigo", "sabinanigo-aragon", "Sabiñánigo", "Huesca", 42.5192, -0.3661, "Europe/Madrid", false, Seq("Sabiñánigo"), Seq(
    ("Auditorio La Colina", "Auditorio La Colina", Some("E0643"), None)
  ))
  private def p_sagunto: R = ("sagunto", "sagunto-comunidad-valenciana", "Sagunto", "Valencia", 39.6833, -0.2667, "Europe/Madrid", false, Seq("Sagunto"), Seq(
    ("Alucine Sagunto", "Alucine Sagunto", Some("E0071"), None),
    ("Yelmo Cines VidaNova Parc", "Yelmo Cines VidaNova Parc", Some("E0932"), None)
  ))
  private def p_san_fernando: R = ("san-fernando", "san-fernando-andalucia", "San Fernando", "Cádiz", 36.4759, -6.1982, "Europe/Madrid", true, Seq("San Fernando", "Chiclana de la Frontera"), Seq(
    ("Cines Plaza San Fernando", "Cines Plaza San Fernando", Some("E0212"), None),
    ("Yelmo Cines Premium Bahía Sur", "Yelmo Cines Premium Bahía Sur", Some("E1042"), None),
    ("Multicines Las Salinas", "Multicines Las Salinas", Some("E0520"), None)
  ))
  private def p_san_javier: R = ("san-javier", "san-javier-region-de-murcia", "San Javier", "Murcia", 37.8063, -0.8374, "Europe/Madrid", false, Seq("San Javier"), Seq(
    ("Cines IMF Galán", "Cines IMF Galán", Some("E0956"), None),
    ("Neocine Dos Mares", "Neocine Dos Mares", Some("E0431"), None),
    ("Terraza España", "Terraza España", Some("E0949"), None)
  ))
  private def p_san_martin_de_valdeiglesias: R = ("san-martin-de-valdeiglesias", "san-martin-de-valdeiglesias-comunidad-de-madrid", "San Martín de Valdeiglesias", "Madrid", 40.3618, -4.3983, "Europe/Madrid", true, Seq("San Martín de Valdeiglesias", "Villa del Prado", "Sotillo de la Adrada", "Pelayos de la Presa", "Navaluenga"), Seq(
    ("Cine Teatro Municipal", "Cine Teatro Municipal", Some("E0419"), None),
    ("Cine de Verano El Molino", "Cine de Verano El Molino", Some("E0674"), None),
    ("Cines Villa", "Cines Villa", Some("E0190"), None),
    ("Cine Blasco", "Cine Blasco", Some("E0980"), None),
    ("Cine Rueda", "Cine Rueda", Some("E0966"), None)
  ))
  private def p_san_vicente_del_raspeig: R = ("san-vicente-del-raspeig", "san-vicente-del-raspeig-comunidad-valenciana", "San Vicente del Raspeig", "Alicante", 38.4343, -0.5496, "Europe/Madrid", true, Seq("San Vicente del Raspeig", "Santa Pola", "Mutxamel", "San Juan de Alicante"), Seq(
    ("Autocine El Sur", "Autocine El Sur", Some("E0783"), None),
    ("Cine Aana San Juan", "Cine Aana San Juan", Some("E0009"), None),
    ("Cine La Esperanza", "Cine La Esperanza", Some("E0961"), None),
    ("Odeon Multicines Alicante", "Odeon Multicines Alicante", Some("E0213"), None),
    ("Cines Axion de Santa Pola", "Cines Axion de Santa Pola", Some("E0752"), None)
  ))
  private def p_sant_boi_de_llobregat: R = ("sant-boi-de-llobregat", "sant-boi-de-llobregat-cataluna", "Sant Boi de Llobregat", "Barcelona", 41.3436, 2.0366, "Europe/Madrid", true, Seq("Sant Boi de Llobregat", "El Prat de Llobregat", "Castelldefels", "Gavà", "Sant Feliu de Llobregat", "Sant Vicenç dels Horts", "Abrera"), Seq(
    ("Cine Capri", "Cine Capri", Some("E0230"), None),
    ("Cinebaix", "Cinebaix", Some("E0276"), None),
    ("Cinemes Can Castellet", "Cinemes Can Castellet", Some("E0748"), None),
    ("Cinesa Barnasud", "Cinesa Barnasud", Some("E0661"), None),
    ("Multicinemes La Vailet", "Multicinemes La Vailet", Some("E0714"), None),
    ("Yelmo Cines Abrera", "Yelmo Cines Abrera", Some("E0735"), None),
    ("Yelmo Cines Premium Castelldefels", "Yelmo Cines Premium Castelldefels", Some("E0806"), None)
  ))
  private def p_sant_cugat_del_valles: R = ("sant-cugat-del-valles", "sant-cugat-del-valles-cataluna", "Sant Cugat del Vallès", "Barcelona", 41.4706, 2.0861, "Europe/Madrid", true, Seq("Sant Cugat del Vallès", "Montcada i Reixac", "Barberà del Vallès"), Seq(
    ("Cinemes Sant Cugat", "Cinemes Sant Cugat", Some("E0403"), None),
    ("Yelmo Cines Premium Sant Cugat", "Yelmo Cines Premium Sant Cugat", Some("E0633"), None),
    ("Cines Montcada", "Cines Montcada", Some("E0655"), None),
    ("Yelmo Cines Baricentro", "Yelmo Cines Baricentro", Some("E0615"), None)
  ))
  private def p_santa_maria_del_paramo: R = ("santa-maria-del-paramo", "santa-maria-del-paramo-castilla-y-leon", "Santa María del Páramo", "León", 42.3551, -5.7515, "Europe/Madrid", false, Seq("Santa María del Páramo"), Seq(
    ("Cine Paramés", "Cine Paramés", Some("E0854"), None)
  ))
  private def p_santa_marta_de_tormes: R = ("santa-marta-de-tormes", "santa-marta-de-tormes-castilla-y-leon", "Santa Marta de Tormes", "Salamanca", 40.9507, -5.6272, "Europe/Madrid", false, Seq("Santa Marta de Tormes"), Seq(
    ("Cines Van Dyck Tormes", "Cines Van Dyck Tormes", Some("E0368"), None)
  ))
  private def p_santiago_de_compostela: R = ("santiago-de-compostela", "santiago-de-compostela-galicia", "Santiago de Compostela", "A Coruña", 42.8805, -8.5457, "Europe/Madrid", false, Seq("Santiago de Compostela"), Seq(
    ("Cinesa As Cancelas", "Cinesa As Cancelas", Some("E0795"), None),
    ("Multicines Compostela", "Multicines Compostela", Some("E0762"), None),
    ("Numax", "Numax", Some("E0848"), None)
  ))
  private def p_segovia: R = ("segovia", "segovia-castilla-y-leon", "Segovia", "Segovia", 40.9481, -4.1184, "Europe/Madrid", false, Seq("Segovia"), Seq(
    ("Artesiete Segovia", "Artesiete Segovia", Some("E0716"), None),
    ("Cines Luz de Castilla", "Cines Luz de Castilla", Some("E0285"), None)
  ))
  private def p_siero: R = ("siero", "siero-asturias", "Siero", "Asturias", 43.4027, -5.8121, "Europe/Madrid", false, Seq("Siero"), Seq(
    ("Cinesa Parque Principado", "Cinesa Parque Principado", Some("E0398"), None)
  ))
  private def p_sitges: R = ("sitges", "sitges-cataluna", "Sitges", "Barcelona", 41.2351, 1.8119, "Europe/Madrid", true, Seq("Sitges", "Vilanova i la Geltrú", "Sant Pere de Ribes"), Seq(
    ("Cinema Prado", "Cinema Prado", Some("E0311"), None),
    ("Cinema Retiro", "Cinema Retiro", Some("E0312"), None),
    ("Cinema Ribes", "Cinema Ribes", Some("E0907"), None),
    ("Odeon Multicines Vilanova", "Odeon Multicines Vilanova", Some("E2908"), None)
  ))
  private def p_solsona: R = ("solsona", "solsona-cataluna", "Solsona", "Lérida", 41.9939, 1.5171, "Europe/Madrid", false, Seq("Solsona"), Seq(
    ("Cinema Paris", "Cinema Paris", Some("E0309"), None)
  ))
  private def p_talavera_de_la_reina: R = ("talavera-de-la-reina", "talavera-de-la-reina-castilla-la-mancha", "Talavera de la Reina", "Toledo", 39.9635, -4.8308, "Europe/Madrid", false, Seq("Talavera de la Reina"), Seq(
    ("Artesiete Los Alfares", "Artesiete Los Alfares", Some("E0803"), None)
  ))
  private def p_tarancon: R = ("tarancon", "tarancon-castilla-la-mancha", "Tarancón", "Cuenca", 40.0085, -3.0073, "Europe/Madrid", false, Seq("Tarancón"), Seq(
    ("Cine de Verano Tarancón", "Cine de Verano Tarancón", Some("E0914"), None)
  ))
  private def p_tarragona: R = ("tarragona", "tarragona-cataluna", "Tarragona", "Tarragona", 41.1191, 1.2454, "Europe/Madrid", true, Seq("Tarragona", "Vila-seca"), Seq(
    ("Ocine Gavarres", "Ocine Gavarres", Some("E0509"), Some("tickets.ocinegavarres.es")),
    ("Yelmo Cines Parc Central", "Yelmo Cines Parc Central", Some("E0807"), None),
    ("Ocine Vila-seca", "Ocine Vila-seca", Some("E0727"), Some("tickets.ocinevilaseca.es"))
  ))
  private def p_tarrega: R = ("tarrega", "tarrega-cataluna", "Tàrrega", "Lérida", 41.647, 1.1396, "Europe/Madrid", true, Seq("Tàrrega", "Agramunt", "Bellpuig", "Linyola"), Seq(
    ("Cinema Armengol", "Cinema Armengol", Some("E0302"), None),
    ("Cinema Casal Agramunt", "Cinema Casal Agramunt", Some("E0303"), None),
    ("Cinema Planell", "Cinema Planell", Some("E0310"), None),
    ("Cinemes Majestic", "Cinemes Majestic", Some("E0799"), None)
  ))
  private def p_telde: R = ("telde", "telde-canarias", "Telde", "Las Palmas", 27.9924, -15.4192, "Atlantic/Canary", true, Seq("Telde", "Santa Lucía de Tirajana"), Seq(
    ("Artesiete Las Terrazas", "Artesiete Las Terrazas", Some("E0723"), None),
    ("Yelmo Cines Vecindario", "Yelmo Cines Vecindario", Some("E0634"), None)
  ))
  private def p_teruel: R = ("teruel", "teruel-aragon", "Teruel", "Teruel", 40.3456, -1.1065, "Europe/Madrid", false, Seq("Teruel"), Seq(
    ("Cine Maravillas", "Cine Maravillas", Some("E0697"), None)
  ))
  private def p_toledo: R = ("toledo", "toledo-castilla-la-mancha", "Toledo", "Toledo", 39.8581, -4.0226, "Europe/Madrid", true, Seq("Toledo", "Torrijos", "Sonseca", "Olías del Rey"), Seq(
    ("Cine Central 3D", "Cine Central 3D", Some("E0831"), None),
    ("Cines Redux", "Cines Redux", Some("E0872"), None),
    ("Real Cinema De Olías", "Real Cinema De Olías", Some("E0574"), None),
    ("mk2 Luz del Tajo", "mk2 Luz del Tajo", Some("E0412"), None)
  ))
  private def p_tomelloso: R = ("tomelloso", "tomelloso-castilla-la-mancha", "Tomelloso", "Ciudad Real", 39.1576, -3.0216, "Europe/Madrid", true, Seq("Tomelloso", "Socuéllamos"), Seq(
    ("Cine Reina Sofía Socuéllamos", "Cine Reina Sofía Socuéllamos", Some("E1024"), None),
    ("La Dehesa Tomelloso", "La Dehesa Tomelloso", Some("E0659"), None)
  ))
  private def p_torrelodones: R = ("torrelodones", "torrelodones-comunidad-de-madrid", "Torrelodones", "Madrid", 40.5765, -3.9266, "Europe/Madrid", true, Seq("Torrelodones", "Collado Villalba", "Guadarrama", "Soto del Real"), Seq(
    ("Casa de Cultura Guadarrama", "Casa de Cultura Guadarrama", Some("E0936"), None),
    ("Centro de Arte y Cine de Verano Soto del Real", "Centro de Arte y Cine de Verano Soto del Real", Some("E0937"), None),
    ("Sala Babel", "Sala Babel", Some("E0818"), None),
    ("Teatro Fernández-Baldor", "Teatro Fernández-Baldor", Some("E1000"), None),
    ("Yelmo Cines Planetocio", "Yelmo Cines Planetocio", Some("E0630"), None)
  ))
  private def p_totana: R = ("totana", "totana-region-de-murcia", "Totana", "Murcia", 37.7688, -1.5023, "Europe/Madrid", false, Seq("Totana"), Seq(
    ("Cinema Velasco Totana", "Cinema Velasco Totana", Some("E0921"), None),
    ("Terraza Auditorio Parque Municipal", "Terraza Auditorio Parque Municipal", Some("E0922"), None)
  ))
  private def p_tremp: R = ("tremp", "tremp-cataluna", "Tremp", "Lérida", 42.167, 0.8949, "Europe/Madrid", false, Seq("Tremp"), Seq(
    ("Cinema La Lira", "Cinema La Lira", Some("E0305"), None)
  ))
  private def p_tudela: R = ("tudela", "tudela-navarra", "Tudela", "Navarra", 42.0617, -1.6045, "Europe/Madrid", false, Seq("Tudela"), Seq(
    ("Ocine Tudela", "Ocine Tudela", Some("E0317"), None)
  ))
  private def p_ubrique: R = ("ubrique", "ubrique-andalucia", "Ubrique", "Cádiz", 36.6778, -5.446, "Europe/Madrid", false, Seq("Ubrique"), Seq(
    ("Teatro Maestro Francisco Fatou", "Teatro Maestro Francisco Fatou", Some("E1019"), None)
  ))
  private def p_valdemorillo: R = ("valdemorillo", "valdemorillo-comunidad-de-madrid", "Valdemorillo", "Madrid", 40.5006, -4.0671, "Europe/Madrid", false, Seq("Valdemorillo"), Seq(
    ("Cine Giralt Laporta", "Cine Giralt Laporta", Some("E0753"), None),
    ("Cine de Verano Juan Falco", "Cine de Verano Juan Falco", Some("E0750"), None)
  ))
  private def p_valdemoro: R = ("valdemoro", "valdemoro-comunidad-de-madrid", "Valdemoro", "Madrid", 40.1908, -3.6789, "Europe/Madrid", true, Seq("Valdemoro", "Parla", "Aranjuez", "Pinto"), Seq(
    ("Cine Aranjuez", "Cine Aranjuez", Some("E0233"), None),
    ("Cine de Verano Valdemoro", "Cine de Verano Valdemoro", Some("E0675"), None),
    ("Restón Cinema", "Restón Cinema", Some("E0584"), None),
    ("Ocine Plaza Éboli", "Ocine Plaza Éboli", Some("E2900"), Some("tickets.ocineplazaeboli.es")),
    ("Spazio Cines", "Spazio Cines", Some("E0935"), None)
  ))
  private def p_valdepenas: R = ("valdepenas", "valdepenas-castilla-la-mancha", "Valdepeñas", "Ciudad Real", 38.7621, -3.3848, "Europe/Madrid", false, Seq("Valdepeñas"), Seq(
    ("Multicines Valdepeñas", "Multicines Valdepeñas", Some("E0701"), None)
  ))
  private def p_velez_malaga: R = ("velez-malaga", "velez-malaga-andalucia", "Vélez-Málaga", "Málaga", 36.7811, -4.1027, "Europe/Madrid", true, Seq("Vélez-Málaga", "Rincón de la Victoria", "Nerja"), Seq(
    ("Centro Cultural Villa de Nerja", "Centro Cultural Villa de Nerja", Some("E0976"), None),
    ("Yelmo Cines Rincón De La Victoria", "Yelmo Cines Rincón De La Victoria", Some("E0632"), None),
    ("mk2 El Ingenio", "mk2 El Ingenio", Some("E0433"), None)
  ))
  private def p_viana: R = ("viana", "viana-navarra", "Viaña", "Navarra", 42.4944, -2.3495, "Europe/Madrid", false, Seq("Viaña"), Seq(
    ("Cines Las Cañas Viana", "Cines Las Cañas Viana", Some("E0371"), None)
  ))
  private def p_vic: R = ("vic", "vic-cataluna", "Vic", "Barcelona", 41.9301, 2.2549, "Europe/Madrid", false, Seq("Vic"), Seq(
    ("Cine Vigatà", "Cine Vigatà", Some("E0612"), None),
    ("Multicines Sucre", "Multicines Sucre", Some("E0535"), None)
  ))
  private def p_vielha: R = ("vielha", "vielha-cataluna", "Vielha", "Lérida", 42.702, 0.7956, "Europe/Madrid", false, Seq("Vielha"), Seq(
    ("Cinema Era Audiovisuau", "Cinema Era Audiovisuau", Some("E0232"), None)
  ))
  private def p_vila_real: R = ("vila-real", "vila-real-comunidad-valenciana", "Vila-real", "Castellón", 39.9383, -0.1009, "Europe/Madrid", true, Seq("Vila-real", "La Vall d'Uixó"), Seq(
    ("Cines Sucre", "Cines Sucre", Some("E0366"), None),
    ("Teatre Municipal Carmen Tur - Antic Cine España", "Teatre Municipal Carmen Tur - Antic Cine España", Some("E0791"), None)
  ))
  private def p_vilafranca_del_penedes: R = ("vilafranca-del-penedes", "vilafranca-del-penedes-cataluna", "Vilafranca del Penedès", "Barcelona", 41.3462, 1.6971, "Europe/Madrid", false, Seq("Vilafranca del Penedès"), Seq(
    ("Cine Kubrick", "Cine Kubrick", Some("E0757"), None),
    ("Cineclub Vilafranca - Sala Zazie-Casa", "Cineclub Vilafranca - Sala Zazie-Casa", Some("E0692"), None)
  ))
  private def p_vilagarcia_de_arousa: R = ("vilagarcia-de-arousa", "vilagarcia-de-arousa-galicia", "Vilagarcía de Arousa", "Pontevedra", 42.5963, -8.7643, "Europe/Madrid", true, Seq("Vilagarcía de Arousa", "Ribeira", "A Estrada", "Caldas de Reis"), Seq(
    ("Barbanza Multicines", "Barbanza Multicines", Some("E0124"), None),
    ("Cines Avenida 3D", "Cines Avenida 3D", Some("E0742"), None),
    ("Minicines Central ", "Minicines Central ", Some("E0895"), None),
    ("Multicines Gran Arousa", "Multicines Gran Arousa", Some("E0510"), None)
  ))
  private def p_villablino: R = ("villablino", "villablino-castilla-y-leon", "Villablino", "León", 42.9393, -6.3194, "Europe/Madrid", false, Seq("Villablino"), Seq(
    ("El cine Villablino", "El cine Villablino", Some("E0888"), None)
  ))
  private def p_villarrobledo: R = ("villarrobledo", "villarrobledo-castilla-la-mancha", "Villarrobledo", "Albacete", 39.2699, -2.6012, "Europe/Madrid", false, Seq("Villarrobledo"), Seq(
    ("Gran Teatro de Villarrobledo", "Gran Teatro de Villarrobledo", Some("E0448"), None)
  ))
  private def p_villaviciosa_de_odon: R = ("villaviciosa-de-odon", "villaviciosa-de-odon-comunidad-de-madrid", "Villaviciosa de Odón", "Madrid", 40.3581, -3.9043, "Europe/Madrid", true, Seq("Villaviciosa de Odón", "Arroyomolinos"), Seq(
    ("Cine Colíseo de la Cultura", "Cine Colíseo de la Cultura", Some("E0707"), None),
    ("Cine de verano el castillo", "Cine de verano el castillo", Some("E0672"), None),
    ("Cinesa Intu Xanadú", "Cinesa Intu Xanadú", Some("E0406"), None)
  ))
  private def p_villena: R = ("villena", "villena-comunidad-valenciana", "Villena", "Alicante", 38.6373, -0.8657, "Europe/Madrid", true, Seq("Villena", "Yecla", "Almansa"), Seq(
    ("Cines Coliseum", "Cines Coliseum", Some("E0710"), None),
    ("Cine Club Villena", "Cine Club Villena", Some("E2915"), None),
    ("Cine Pya", "Cine Pya", Some("E0838"), None)
  ))
  private def p_vinaros: R = ("vinaros", "vinaros-comunidad-valenciana", "Vinaròs", "Castellón", 40.4703, 0.4756, "Europe/Madrid", true, Seq("Vinaròs", "Benicarló"), Seq(
    ("Cines Axion Benicarló", "Cines Axion Benicarló", Some("E0493"), None),
    ("JJ Cinema ", "JJ Cinema ", Some("E0892"), None)
  ))
  private def p_viveiro: R = ("viveiro", "viveiro-galicia", "Viveiro", "Lugo", 43.6623, -7.5934, "Europe/Madrid", false, Seq("Viveiro"), Seq(
    ("Cines Viveiro 3D", "Cines Viveiro 3D", Some("E0846"), None)
  ))
  private def p_xinzo_de_limia: R = ("xinzo-de-limia", "xinzo-de-limia-galicia", "Xinzo de Limia", "Ourense", 42.0635, -7.7246, "Europe/Madrid", false, Seq("Xinzo de Limia"), Seq(
    ("Cine Gesma", "Cine Gesma", Some("E0834"), None)
  ))
  private def p_zamora: R = ("zamora", "zamora-castilla-y-leon", "Zamora", "Zamora", 41.5063, -5.7446, "Europe/Madrid", false, Seq("Zamora"), Seq(
    ("Multicines Zamora", "Multicines Zamora", Some("E0540"), None)
  ))
  private def p_zuera: R = ("zuera", "zuera-aragon", "Zuera", "Zaragoza", 41.8678, -0.7898, "Europe/Madrid", false, Seq("Zuera"), Seq(
    ("Teatro Reina Sofía", "Teatro Reina Sofía", Some("E1008"), None)
  ))
  private def p_zumaia: R = ("zumaia", "zumaia-pais-vasco", "Zumaia", "Guipúzcoa", 43.2947, -2.2534, "Europe/Madrid", true, Seq("Zumaia", "Lekeitio"), Seq(
    ("Aita Mari Zinema", "Aita Mari Zinema", Some("E0214"), None),
    ("Ikusgarri Zinema", "Ikusgarri Zinema", Some("E0891"), None)
  ))

  private def chunk0: Seq[R] = Seq(p_madrid, p_barcelona, p_valencia, p_sevilla, p_zaragoza, p_malaga, p_murcia, p_palma_de_mallorca, p_las_palmas_de_gran_canaria, p_bilbao, p_alicante, p_cordoba, p_valladolid, p_vigo, p_gijon, p_l_hospitalet_de_llobregat, p_a_coruna, p_vitoria_gasteiz, p_granada, p_elche, p_oviedo, p_badalona, p_terrassa, p_cartagena, p_jerez_de_la_frontera, p_sabadell, p_santa_cruz_de_tenerife, p_mostoles, p_alcala_de_henares, p_pamplona, p_fuenlabrada, p_almeria, p_leganes, p_san_sebastian, p_getafe, p_castellon_de_la_plana, p_burgos, p_santander, p_albacete, p_alcorcon)
  private def chunk1: Seq[R] = Seq(p_san_cristobal_de_la_laguna, p_salamanca, p_logrono, p_adeje, p_aguilar_de_campoo, p_aguilas, p_alcala_de_xivert, p_alcala_la_real, p_alcaniz, p_alcazar_de_san_juan, p_alcobendas, p_alcoy, p_algeciras, p_alhaurin_el_grande, p_almazan, p_almendralejo, p_alzira, p_amposta, p_andujar, p_antequera, p_aranda_de_duero, p_arcos_de_la_frontera, p_arenas_de_san_pedro, p_arrecife, p_arroyo_de_la_encomienda, p_astorga, p_avila, p_ayamonte, p_badajoz, p_barakaldo, p_barbastro, p_barbate, p_baza, p_beasain, p_bejar, p_benidorm, p_berga, p_binefar, p_blanes, p_boltana)
  private def chunk2: Seq[R] = Seq(p_bunol, p_burgo_de_osma, p_caceres, p_cadiz, p_calahorra, p_calatayud, p_calpe, p_camargo, p_carballo, p_cee, p_ceuta, p_ciudad_real, p_ciudad_rodrigo, p_ciutadella_de_menorca, p_coria, p_cornella_de_llobregat, p_cortegana, p_corvera_de_asturias, p_coslada, p_cuenca, p_daimiel, p_don_benito, p_dos_hermanas, p_ecija, p_eibar, p_el_ejido, p_el_pont_de_suert, p_el_puerto_de_santa_maria, p_el_vendrell, p_estella_lizarra, p_estepa, p_ferrol, p_figueres, p_fuengirola, p_galdakao, p_gandia, p_girona, p_golmayo, p_granollers, p_guadalajara)
  private def chunk3: Seq[R] = Seq(p_guardo, p_herrera_del_duque, p_huarte, p_huelva, p_huesca, p_huetor_tajar, p_ibiza, p_iniesta, p_irun, p_jaraiz_de_la_vera, p_javea, p_la_orotava, p_la_palma_del_condado, p_la_seu_d_urgell, p_la_zubia, p_laredo, p_leiro, p_leon, p_linares, p_lleida, p_lorca, p_los_llanos_de_aridane, p_lucena, p_lugo, p_mairena_del_aljarafe, p_majadahonda, p_manacor, p_manresa, p_mao, p_marbella, p_marchena, p_marratxi, p_martos, p_mazarron, p_medina_de_rioseco, p_medina_del_campo, p_melilla, p_mequinenza, p_merida, p_miranda_de_ebro)
  private def chunk4: Seq[R] = Seq(p_molina_de_segura, p_mollerussa, p_monforte_de_lemos, p_motril, p_mungia, p_navalmoral_de_la_mata, p_navia, p_oliva, p_olot, p_orihuela, p_ourense, p_palafrugell, p_palencia, p_paterna, p_pedrajas_de_san_esteban, p_penaranda_de_bracamonte, p_penarroya_pueblonuevo, p_petrer, p_pilar_de_la_horadada, p_plasencia, p_ponferrada, p_pontevedra, p_pozoblanco, p_premia_de_mar, p_puerto_del_rosario, p_puertollano, p_requena, p_reus, p_ribadeo, p_ronda, p_roquetas_de_mar, p_rota, p_sabinanigo, p_sagunto, p_san_fernando, p_san_javier, p_san_martin_de_valdeiglesias, p_san_vicente_del_raspeig, p_sant_boi_de_llobregat, p_sant_cugat_del_valles)
  private def chunk5: Seq[R] = Seq(p_santa_maria_del_paramo, p_santa_marta_de_tormes, p_santiago_de_compostela, p_segovia, p_siero, p_sitges, p_solsona, p_talavera_de_la_reina, p_tarancon, p_tarragona, p_tarrega, p_telde, p_teruel, p_toledo, p_tomelloso, p_torrelodones, p_totana, p_tremp, p_tudela, p_ubrique, p_valdemorillo, p_valdemoro, p_valdepenas, p_velez_malaga, p_viana, p_vic, p_vielha, p_vila_real, p_vilafranca_del_penedes, p_vilagarcia_de_arousa, p_villablino, p_villarrobledo, p_villaviciosa_de_odon, p_villena, p_vinaros, p_viveiro, p_xinzo_de_limia, p_zamora, p_zuera, p_zumaia)
  val pages: Seq[R] = chunk0 ++ chunk1 ++ chunk2 ++ chunk3 ++ chunk4 ++ chunk5

  /** Pages that no longer exist — the provinces Spain's pages were until 2026-10
   *  among them — and the page now holding most of their venues: (slug, slug
   *  qualified with its autonomous community, the page's slug). */
  val retired: Seq[(String, String, String)] = Seq(
    ("alava", "alava-pais-vasco", "vitoria-gasteiz"),
    ("asturias", "asturias-asturias", "gijon"),
    ("cantabria", "cantabria-cantabria", "santander"),
    ("castellon", "castellon-comunidad-valenciana", "castellon-de-la-plana"),
    ("guipuzcoa", "guipuzcoa-pais-vasco", "beasain"),
    ("islas-baleares", "islas-baleares-islas-baleares", "palma-de-mallorca"),
    ("jaen", "jaen-andalucia", "linares"),
    ("la-rioja", "la-rioja-la-rioja", "logrono"),
    ("las-palmas", "las-palmas-canarias", "las-palmas-de-gran-canaria"),
    ("lerida", "lerida-cataluna", "lleida"),
    ("navarra", "navarra-navarra", "pamplona"),
    ("soria", "soria-castilla-y-leon", "golmayo"),
    ("vizcaya", "vizcaya-pais-vasco", "barakaldo"),
  )
}
