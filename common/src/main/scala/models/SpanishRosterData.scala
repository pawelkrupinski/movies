// GENERATED from data/spain/provinces.json by data/spain/scripts/generate_roster.py
// — do NOT edit by hand. Full Spanish cinema roster: 52 provinces / 595 cinemas (SensaCine, plus the Ocine
// venues it does not list, from data/spain/ocine.json).
// Regenerate with `python3 data/spain/scripts/generate_roster.py` after re-harvesting;
// see data/spain/README.md.
package models

private[models] object SpanishRosterData {
  // (displayName, pillName, SensaCine theaterId, Ocine ticketing slug) — a venue
  // SensaCine does not list has no theaterId and is scraped off its own server
  type C = (String, String, Option[String], Option[String])
  // (slug, name, autonomous community, lat, lon, zoneId, towns, cinemas)
  type R = (String, String, String, Double, Double, String, Seq[String], Seq[C])

  private def p_a_coruna: R = ("a-coruna", "A Coruña", "Galicia", 43.37135, -8.396, "Europe/Madrid", Seq("A Coruña", "Santiago de Compostela", "Carballo", "Cee", "Ferrol", "Narón", "Ribeira"), Seq(
    ("Barbanza Multicines", "Barbanza Multicines", Some("E0124"), None),
    ("Cantones Cines", "Cantones Cines", Some("E0437"), None),
    ("Cine Duplex", "Cine Duplex", Some("E0741"), None),
    ("Cines Forum Metropolitano", "Cines Forum Metropolitano", Some("E0441"), None),
    ("Cines Xunqueira", "Cines Xunqueira", Some("E0694"), None),
    ("Cinesa As Cancelas", "Cinesa As Cancelas", Some("E0795"), None),
    ("Cinesa Marineda City", "Cinesa Marineda City", Some("E0770"), None),
    ("Multicines Bergantiños", "Multicines Bergantiños", Some("E0494"), None),
    ("Multicines Compostela", "Multicines Compostela", Some("E0762"), None),
    ("Numax", "Numax", Some("E0848"), None),
    ("Odeon Multicines Narón", "Odeon Multicines Narón", Some("E0789"), None),
    ("Yelmo Cines Espacio Coruña", "Yelmo Cines Espacio Coruña", Some("E0734"), None)
  ))
  private def p_albacete: R = ("albacete", "Albacete", "Castilla-La Mancha", 38.99424, -1.85643, "Europe/Madrid", Seq("Albacete", "Almansa"), Seq(
    ("Cines Coliseum", "Cines Coliseum", Some("E0710"), None),
    ("Gran Teatro de Villarrobledo", "Gran Teatro de Villarrobledo", Some("E0448"), None),
    ("Yelmo Cines Imaginalia", "Yelmo Cines Imaginalia", Some("E0205"), None),
    ("Yelmo Cines Vialia Albacete", "Yelmo Cines Vialia Albacete", Some("E0778"), None)
  ))
  private def p_alicante: R = ("alicante", "Alicante", "Comunidad Valenciana", 38.34517, -0.48149, "Europe/Madrid", Seq("Alicante", "Benidorm", "Javea", "Marina", "Petrer", "San Vicente del Raspeig", "Alcoy", "Alfas del Pi", "Callosa de Segura", "Calpe", "Cocentaina", "Dehesa de Campoamor", "Denia", "Elche", "Finestrat", "Maitino", "Mutxamel", "Ondara", "Orihuela", "San Juan de Alicante", "Santa Pola", "Torrevieja", "Villena"), Seq(
    ("Abc Elx", "Abc Elx", Some("E0035"), None),
    ("Auto Cine Drive In", "Auto Cine Drive In", Some("E0197"), None),
    ("Autocine El Sur", "Autocine El Sur", Some("E0783"), None),
    ("Cine Aana Alicante", "Cine Aana Alicante", Some("E0008"), None),
    ("Cine Aana San Juan", "Cine Aana San Juan", Some("E0009"), None),
    ("Cine BIC", "Cine BIC", Some("E0033"), None),
    ("Cine Calp", "Cine Calp", Some("E0924"), None),
    ("Cine Club Villena", "Cine Club Villena", Some("E2915"), None),
    ("Cine Club Xábia", "Cine Club Xábia", Some("E0249"), None),
    ("Cine Horadada", "Cine Horadada", Some("E0968"), None),
    ("Cine Imf Ondara", "Cine Imf Ondara", Some("E0658"), None),
    ("Cine Imf Torrevieja", "Cine Imf Torrevieja", Some("E0451"), None),
    ("Cine Jayan", "Cine Jayan", Some("E0254"), None),
    ("Cine La Esperanza", "Cine La Esperanza", Some("E0961"), None),
    ("Cine Las Villas", "Cine Las Villas", Some("E0971"), None),
    ("Cine Navas", "Cine Navas", Some("E0545"), None),
    ("Cine Navia", "Cine Navia", Some("E0970"), None),
    ("Cinebox Plaza Mar 2", "Cinebox Plaza Mar 2", Some("E0293"), None),
    ("Cinema Roma", "Cinema Roma", Some("E0268"), None),
    ("Cines Axion Playa de San Juan ", "Cines Axion Playa de San Juan ", Some("E0884"), None),
    ("Cines Axion de Orihuela", "Cines Axion de Orihuela", Some("E0552"), None),
    ("Cines Axion de Santa Pola", "Cines Axion de Santa Pola", Some("E0752"), None),
    ("Cines Colci ", "Cines Colci ", Some("E0422"), None),
    ("Cines Colci Rincón", "Cines Colci Rincón", Some("E0423"), None),
    ("Cines Costa", "Cines Costa", Some("E0951"), None),
    ("Cines Odeón", "Cines Odeón", Some("E0853"), None),
    ("Cines Panoramis", "Cines Panoramis", Some("E0397"), None),
    ("CinesMax 3D Petrer", "CinesMax 3D Petrer", Some("E0728"), None),
    ("Colci Suyma", "Colci Suyma", Some("E0957"), None),
    ("Kinépolis Alicante", "Kinépolis Alicante", Some("E0819"), None),
    ("Multicines El Altet", "Multicines El Altet", Some("E0721"), None),
    ("Odeon Multicines Alicante", "Odeon Multicines Alicante", Some("E0213"), None),
    ("Terraza Imperial - Cine de Verano", "Terraza Imperial - Cine de Verano", Some("E0950"), None),
    ("Yelmo Cines Puerta De Alicante", "Yelmo Cines Puerta De Alicante", Some("E0631"), None),
    ("Yelmo Cines Vinalopo", "Yelmo Cines Vinalopo", Some("E0636"), None)
  ))
  private def p_almeria: R = ("almeria", "Almería", "Andalucía", 36.83814, -2.45974, "Europe/Madrid", Seq("Almería", "Albox", "Berja", "Garrucha", "Parador Hortichuelas", "Roquetas de Mar", "Vera"), Seq(
    ("Cine Albox", "Cine Albox", Some("E0865"), None),
    ("Cine Berja", "Cine Berja", Some("E0965"), None),
    ("Cine Tenis", "Cine Tenis", Some("E0975"), None),
    ("Cine Terraza de Verano de Vera", "Cine Terraza de Verano de Vera", Some("E0911"), None),
    ("Cine de verano Aguadulce", "Cine de verano Aguadulce", Some("E0943"), None),
    ("Kinépolis Almería Mediterráneo", "Kinépolis Almería Mediterráneo", Some("E0359"), None),
    ("Yelmo Cines Roquetas", "Yelmo Cines Roquetas", Some("E0620"), None),
    ("Yelmo Cines Torrecárdenas", "Yelmo Cines Torrecárdenas", Some("E0909"), None)
  ))
  private def p_asturias: R = ("asturias", "Asturias", "Asturias", 43.36029, -5.84476, "Europe/Madrid", Seq("Corvera de Asturias", "Gijón", "Navia", "Oviedo", "Siero"), Seq(
    ("Autocine Gijón", "Autocine Gijón", Some("E0784"), None),
    ("Cine Fantasio Navia", "Cine Fantasio Navia", Some("E1037"), None),
    ("Cinebox Parque Astur", "Cinebox Parque Astur", Some("E0290"), None),
    ("Cinesa Parque Principado", "Cinesa Parque Principado", Some("E0398"), None),
    ("Odeon Multicines Parque Astur", "Odeon Multicines Parque Astur", Some("E0814"), None),
    ("Yelmo Cines Los Prados", "Yelmo Cines Los Prados", Some("E0623"), None)
  ))
  private def p_badajoz: R = ("badajoz", "Badajoz", "Extremadura", 38.87789, -6.97061, "Europe/Madrid", Seq("Badajoz", "Almendralejo", "Don Benito", "Fuente de Cantos", "Herrera del Duque", "Mérida", "Zafra"), Seq(
    ("Cine La Fábrica", "Cine La Fábrica", Some("E1022"), None),
    ("Cine Municipal Herrera del Duque", "Cine Municipal Herrera del Duque", Some("E1027"), None),
    ("Cines Victoria Almendralejo", "Cines Victoria Almendralejo", Some("E0719"), None),
    ("Cines Victoria Don Benito", "Cines Victoria Don Benito", Some("E0372"), None),
    ("Cines Victoria Mérida", "Cines Victoria Mérida", Some("E0383"), None),
    ("Multicines España", "Multicines España", Some("E0508"), None),
    ("Yelmo Cines Premium El Faro", "Yelmo Cines Premium El Faro", Some("E1038"), None),
    ("mk2 Conquistadores", "mk2 Conquistadores", Some("E0408"), None)
  ))
  private def p_barcelona: R = ("barcelona", "Barcelona", "Cataluña", 41.38879, 2.15899, "Europe/Madrid", Seq("Barcelona", "Cornellà de Llobregat", "Terrassa", "Granollers", "L'Hospitalet de Llobregat", "Premià de Mar", "Sabadell", "Sant Cugat del Vallès", "Sitges", "Vic", "Vilafranca del Penedès", "Abrera", "Arenys de Mar", "Badalona", "Barberà del Vallès", "Berga", "Calella", "Canoves", "Castelldefels", "Gavà", "Igualada", "Manresa", "Masnou", "Mataró", "Montcada", "Olivella-Urbanitzacio", "Prat de Llobregat", "Samalús", "Sant Boi de Llobregat", "Sant Celoni", "Sant Feliu de Llobregat", "Sant Vicenç dels Horts", "Santa Margarida Montbui", "Vilanova i la Geltrú"), Seq(
    ("Arenas Multicines 3D", "Arenas Multicines 3D", Some("E0764"), None),
    ("Aribau Multicines", "Aribau Multicines", Some("E0091"), None),
    ("Ateneu Cinema ", "Ateneu Cinema ", Some("E0906"), None),
    ("Balmes Multicines", "Balmes Multicines", Some("E0808"), None),
    ("Bosque Multicines", "Bosque Multicines", Some("E0136"), None),
    ("Cine Alhambra", "Cine Alhambra", Some("E0827"), None),
    ("Cine Capri", "Cine Capri", Some("E0230"), None),
    ("Cine Kubrick", "Cine Kubrick", Some("E0757"), None),
    ("Cine Vigatà", "Cine Vigatà", Some("E0612"), None),
    ("Cine la Calandria", "Cine la Calandria", Some("E0836"), None),
    ("Cinebaix", "Cinebaix", Some("E0276"), None),
    ("Cineclub Vilafranca - Sala Zazie-Casa", "Cineclub Vilafranca - Sala Zazie-Casa", Some("E0692"), None),
    ("Cinema Catalunya", "Cinema Catalunya", Some("E0304"), None),
    ("Cinema Edison", "Cinema Edison", Some("G02RB"), None),
    ("Cinema Esbarjo", "Cinema Esbarjo", Some("E0889"), None),
    ("Cinema Prado", "Cinema Prado", Some("E0311"), None),
    ("Cinema Retiro", "Cinema Retiro", Some("E0312"), None),
    ("Cinema Ribes", "Cinema Ribes", Some("E0907"), None),
    ("Cinema Sala Mozart", "Cinema Sala Mozart", Some("E0314"), None),
    ("Cinema Teatre Patronat", "Cinema Teatre Patronat", Some("E0858"), None),
    ("Cinemes Can Castellet", "Cinemes Can Castellet", Some("E0748"), None),
    ("Cinemes Sant Cugat", "Cinemes Sant Cugat", Some("E0403"), None),
    ("Cines Imperial", "Cines Imperial", Some("E0351"), None),
    ("Cines Montcada", "Cines Montcada", Some("E0655"), None),
    ("Cinesa Barnasud", "Cinesa Barnasud", Some("E0661"), None),
    ("Cinesa Diagonal", "Cinesa Diagonal", Some("E0381"), None),
    ("Cinesa Diagonal Mar", "Cinesa Diagonal Mar", Some("E0382"), None),
    ("Cinesa La Farga", "Cinesa La Farga", Some("E0391"), None),
    ("Cinesa Llobregat Centre", "Cinesa Llobregat Centre", Some("E0857"), None),
    ("Cinesa Parc Vallès", "Cinesa Parc Vallès", Some("E0374"), None),
    ("Cinesa SOM Multiespai", "Cinesa SOM Multiespai", Some("E0388"), None),
    ("Club Catalunya", "Club Catalunya", Some("E0420"), None),
    ("Espai L'Amistat", "Espai L'Amistat", Some("E0919"), None),
    ("Filmax Gran Via 3D", "Filmax Gran Via 3D", Some("E0439"), None),
    ("Glòries Multicines", "Glòries Multicines", Some("E0442"), None),
    ("Gran Sarrià Multicines", "Gran Sarrià Multicines", Some("E0447"), None),
    ("Kinépolis Barcelona Full Splau", "Kinépolis Barcelona Full Splau", Some("E0756"), None),
    ("Kinépolis Mataró Parc", "Kinépolis Mataró Parc", Some("E0396"), None),
    ("Mont-Àgora Cinemes", "Mont-Àgora Cinemes", Some("E1006"), None),
    ("Multicinemes La Vailet", "Multicinemes La Vailet", Some("E0714"), None),
    ("Multicines Bages 3D", "Multicines Bages 3D", Some("E0120"), None),
    ("Multicines Catalunya", "Multicines Catalunya", Some("E0495"), None),
    ("Multicines Eix Macià", "Multicines Eix Macià", Some("E0504"), None),
    ("Multicines Sucre", "Multicines Sucre", Some("E0535"), None),
    ("Ocine Arenys", "Ocine Arenys", Some("E0651"), Some("arenys")),
    ("Ocine Granollers", "Ocine Granollers", Some("E0507"), Some("granollers")),
    ("Ocine Màgic", "Ocine Màgic", Some("E0713"), Some("magic")),
    ("Ocine Sant Celoni Altrium", "Ocine Sant Celoni Altrium", Some("E0745"), None),
    ("Odeon Multicines Llobregat", "Odeon Multicines Llobregat", Some("E0521"), None),
    ("Odeon Multicines Vilanova", "Odeon Multicines Vilanova", Some("E2908"), None),
    ("Renoir Floridablanca", "Renoir Floridablanca", Some("E0581"), None),
    ("Yelmo Cines Abrera", "Yelmo Cines Abrera", Some("E0735"), None),
    ("Yelmo Cines Baricentro", "Yelmo Cines Baricentro", Some("E0615"), None),
    ("Yelmo Cines Premium Castelldefels", "Yelmo Cines Premium Castelldefels", Some("E0806"), None),
    ("Yelmo Cines Premium Sant Cugat", "Yelmo Cines Premium Sant Cugat", Some("E0633"), None)
  ))
  private def p_burgos: R = ("burgos", "Burgos", "Castilla y León", 42.34106, -3.70184, "Europe/Madrid", Seq("Burgos", "Aranda de Duero", "Miranda de Ebro"), Seq(
    ("Cine Novedades Miranda de Ebro", "Cine Novedades Miranda de Ebro", Some("E0647"), None),
    ("Cines Van Golem Arlanzón", "Cines Van Golem Arlanzón", Some("E0370"), None),
    ("Cines Victoria Ribera de Duero", "Cines Victoria Ribera de Duero", Some("E0777"), None),
    ("Odeon Multicines Burgos", "Odeon Multicines Burgos", Some("E0279"), None)
  ))
  private def p_cantabria: R = ("cantabria", "Cantabria", "Cantabria", 43.46589, -3.80493, "Europe/Madrid", Seq("Santander", "Camargo", "Astillero", "Corrales", "Laredo", "Noja", "Santoña"), Seq(
    ("Casa de Cultura Doctor Velasco ", "Casa de Cultura Doctor Velasco ", Some("E0917"), None),
    ("Cine La Vidriera", "Cine La Vidriera", Some("E0257"), None),
    ("Cine Los Ángeles", "Cine Los Ángeles", Some("E0688"), None),
    ("Cine Playa Dorada", "Cine Playa Dorada", Some("E0567"), None),
    ("Cines Embajadores Santander", "Cines Embajadores Santander", Some("E0349"), None),
    ("Cinesa Bahía de Santander", "Cinesa Bahía de Santander", Some("E0123"), None),
    ("Filmoteca de Cantabria - Santander", "Filmoteca de Cantabria - Santander", Some("E0979"), None),
    ("Ocine Premium Bahía Real", "Ocine Premium Bahía Real", Some("E1045"), Some("premiumbahiareal")),
    ("Palacios De Festivales", "Palacios De Festivales", Some("E0560"), None),
    ("Sala Bretón", "Sala Bretón", Some("E0594"), None),
    ("Teatro Casino Liceo de Santoña", "Teatro Casino Liceo de Santoña", Some("E0918"), None),
    ("Teatro De Los Corrales De Buelna", "Teatro De Los Corrales De Buelna", Some("E0599"), None),
    ("Yelmo Cines Premium Peñacastillo", "Yelmo Cines Premium Peñacastillo", Some("E0565"), None)
  ))
  private def p_castellon: R = ("castellon", "Castellón", "Comunidad Valenciana", 39.98567, -0.04935, "Europe/Madrid", Seq("Castellón de la plana", "Calzada", "Benicarló", "Vall D'uixo", "Villarreal", "Vinaròs"), Seq(
    ("Cine Terraza Avenida", "Cine Terraza Avenida", Some("E0941"), None),
    ("Cines Axion Benicarló", "Cines Axion Benicarló", Some("E0493"), None),
    ("Cines Sucre", "Cines Sucre", Some("E0366"), None),
    ("Cinesa Salera", "Cinesa Salera", Some("E0654"), None),
    ("JJ Cinema ", "JJ Cinema ", Some("E0892"), None),
    ("Neocine Puerto Azahar", "Neocine Puerto Azahar", Some("E0571"), None),
    ("Ocine Premium Estepark", "Ocine Premium Estepark", Some("E0925"), Some("premiumestepark")),
    ("Teatre Municipal Carmen Tur - Antic Cine España", "Teatre Municipal Carmen Tur - Antic Cine España", Some("E0791"), None),
    ("Terraza Neptuno", "Terraza Neptuno", Some("E0926"), None)
  ))
  private def p_ceuta: R = ("ceuta", "Ceuta", "Ceuta", 35.88919, -5.32042, "Europe/Madrid", Seq("Ceuta"), Seq(
    ("Marina Cinemas 7", "Marina Cinemas 7", Some("E0478"), None)
  ))
  private def p_ciudad_real: R = ("ciudad-real", "Ciudad Real", "Castilla-La Mancha", 38.98626, -3.92907, "Europe/Madrid", Seq("Alcázar de San Juan", "Ciudad Real", "Daimiel", "Pedro Muñoz", "Puertollano", "Socuéllamos", "Tomelloso", "Valdepeñas"), Seq(
    ("Cine Reina Sofía Socuéllamos", "Cine Reina Sofía Socuéllamos", Some("E1024"), None),
    ("Cine Teatro Municipal Pedro Muñoz", "Cine Teatro Municipal Pedro Muñoz", Some("E1023"), None),
    ("Daimiel Cinema", "Daimiel Cinema", Some("E0990"), None),
    ("La Dehesa Tomelloso", "La Dehesa Tomelloso", Some("E0659"), None),
    ("Multicines Cinemancha", "Multicines Cinemancha", Some("E0496"), None),
    ("Multicines Ortega", "Multicines Ortega", Some("E0526"), None),
    ("Multicines Valdepeñas", "Multicines Valdepeñas", Some("E0701"), None),
    ("Parque De Ocio Las Vías", "Parque De Ocio Las Vías", Some("E0562"), None)
  ))
  private def p_cuenca: R = ("cuenca", "Cuenca", "Castilla-La Mancha", 40.06667, -2.13333, "Europe/Madrid", Seq("Cuenca", "Iniesta", "Tarancón"), Seq(
    ("Abaco Cuenca", "Abaco Cuenca", Some("E0020"), None),
    ("Cine Iniesta", "Cine Iniesta", Some("E1031"), None),
    ("Cine de Verano Tarancón", "Cine de Verano Tarancón", Some("E0914"), None),
    ("Odeon Multicines Cuenca", "Odeon Multicines Cuenca", Some("E0502"), None),
    ("Odeon Multicines Mirador", "Odeon Multicines Mirador", Some("E0852"), None)
  ))
  private def p_caceres: R = ("caceres", "Cáceres", "Extremadura", 39.47649, -6.37224, "Europe/Madrid", Seq("Arroyo de la Luz", "Cáceres", "Guijo de Coria", "Jaraíz de la Vera", "Navalmoral de la Mata", "Plasencia"), Seq(
    ("Cine Arroyo de la Luz", "Cine Arroyo de la Luz", Some("E0772"), None),
    ("Cine Avenida Jaraíz", "Cine Avenida Jaraíz", Some("E0867"), None),
    ("Cine Coria", "Cine Coria", Some("E0832"), None),
    ("Cines Navalmoral", "Cines Navalmoral", Some("E0886"), None),
    ("Multicines Alkázar", "Multicines Alkázar", Some("E0067"), None),
    ("Multicines Cáceres", "Multicines Cáceres", Some("E0143"), None)
  ))
  private def p_cadiz: R = ("cadiz", "Cádiz", "Andalucía", 36.52672, -6.2891, "Europe/Madrid", Seq("Cadiz", "Jerez de la Frontera", "Rota", "San Fernando", "Algeciras", "Arcos de la Frontera", "Barrios", "Chiclana de la Frontera", "Chipiona", "El Puerto de Santa María", "Manzanete", "Sanlúcar de Barrameda", "Ubrique", "Zahara de los Atunes"), Seq(
    ("Al-Andalus Cádiz", "Al-Andalus Cádiz", Some("E0902"), None),
    ("Al-Andalus Sanlucar", "Al-Andalus Sanlucar", Some("E0218"), None),
    ("Arcos Cinema", "Arcos Cinema", Some("E0905"), None),
    ("Cine Alba Chipiona", "Cine Alba Chipiona", Some("E0912"), None),
    ("Cine de Verano La Muralla", "Cine de Verano La Muralla", Some("E0999"), None),
    ("Cines Plaza San Fernando", "Cines Plaza San Fernando", Some("E0212"), None),
    ("Cines Victoria Rota", "Cines Victoria Rota", Some("E0766"), None),
    ("Multicines Bahia Mar", "Multicines Bahia Mar", Some("E0491"), None),
    ("Multicines Jerez UCC", "Multicines Jerez UCC", Some("E1044"), None),
    ("Multicines Las Salinas", "Multicines Las Salinas", Some("E0520"), None),
    ("Multicines el Centro", "Multicines el Centro", Some("E0171"), None),
    ("Odeon Bahía Plaza", "Odeon Bahía Plaza", Some("E0245"), None),
    ("Portalejo Cinemas", "Portalejo Cinemas", Some("E0568"), None),
    ("Teatro Maestro Francisco Fatou", "Teatro Maestro Francisco Fatou", Some("E1019"), None),
    ("Teatro San Francisco", "Teatro San Francisco", Some("E1018"), None),
    ("Yelmo Cines Premium Bahía Sur", "Yelmo Cines Premium Bahía Sur", Some("E1042"), None),
    ("Yelmo Cines Premium Puerta Europa ", "Yelmo Cines Premium Puerta Europa ", Some("E0910"), None),
    ("Yelmo Cines Área Sur", "Yelmo Cines Área Sur", Some("E0669"), None),
    ("mk2 Bahía de Cádiz", "mk2 Bahía de Cádiz", Some("E0332"), None)
  ))
  private def p_cordoba: R = ("cordoba", "Córdoba", "Andalucía", 37.89155, -4.77275, "Europe/Madrid", Seq("Estacion de Espiel", "Córdoba", "Baena", "Cabra", "Lucena", "Pozoblanco"), Seq(
    ("Artesiete Lucena", "Artesiete Lucena", Some("E0489"), None),
    ("Centro Cultural de la Villa - Pastora Soler", "Centro Cultural de la Villa - Pastora Soler", Some("E1016"), None),
    ("Cine Baena", "Cine Baena", Some("E0915"), None),
    ("Cine Delicias", "Cine Delicias", Some("E0948"), None),
    ("Cine Mota del Cuervo", "Cine Mota del Cuervo", Some("E1013"), None),
    ("Cine Municipal Huércal-Overa", "Cine Municipal Huércal-Overa", Some("E1012"), None),
    ("Cine Pósito", "Cine Pósito", Some("E0859"), None),
    ("Cinestudio Municipal Cabra", "Cinestudio Municipal Cabra", Some("E0868"), None),
    ("Guadalquivir Cinemas 10", "Guadalquivir Cinemas 10", Some("E0512"), None),
    ("Peñarroya Cinema", "Peñarroya Cinema", Some("E1009"), None),
    ("mk2 El Tablero", "mk2 El Tablero", Some("E0409"), None)
  ))
  private def p_girona: R = ("girona", "Girona", "Cataluña", 41.98311, 2.82493, "Europe/Madrid", Seq("Girona", "Begur", "Blanes", "Figueres", "Fontanilles", "Olot", "Palafrugell", "Palamós", "Platja d'Aro", "Ripoll", "Roses", "Salt"), Seq(
    ("Cat Cinemes", "Cat Cinemes", Some("E0345"), None),
    ("Cinema Casino", "Cinema Casino", Some("E0969"), None),
    ("Cinema Kyton", "Cinema Kyton", Some("E0456"), None),
    ("Cinema Montgrí", "Cinema Montgrí", Some("E0896"), None),
    ("Cinema Teatre Comtal", "Cinema Teatre Comtal", Some("E0315"), None),
    ("Cinema Truffaut", "Cinema Truffaut", Some("E0316"), None),
    ("Cinemes Roses", "Cinemes Roses", Some("E0324"), None),
    ("Multicines Olot", "Multicines Olot", Some("E0323"), None),
    ("Ocine Blanes", "Ocine Blanes", Some("E0462"), Some("blanes")),
    ("Ocine Girona", "Ocine Girona", Some("E0362"), Some("girona")),
    ("Ocine Platja d'Aro", "Ocine Platja d'Aro", Some("E0554"), Some("platjadaro")),
    ("Odeon Multicines Girona", "Odeon Multicines Girona", Some("E0281"), None),
    ("Teatro Municipal de Palafrugel", "Teatro Municipal de Palafrugel", Some("E0978"), None)
  ))
  private def p_granada: R = ("granada", "Granada", "Andalucía", 37.18817, -3.60667, "Europe/Madrid", Seq("Granada", "Zubia", "Almuñécar", "Armilla", "Baza", "Gorgoracha", "Huétor-Tájar", "Pulianas", "Salobreña"), Seq(
    ("Artesiete Alhsur", "Artesiete Alhsur", Some("E0722"), None),
    ("Cañaveral Cinema", "Cañaveral Cinema", Some("E0960"), None),
    ("Cine Liszt Terraza de verano", "Cine Liszt Terraza de verano", Some("E0743"), None),
    ("Cine Madrigal", "Cine Madrigal", Some("E0689"), None),
    ("Cine San Cristobal", "Cine San Cristobal", Some("E0952"), None),
    ("Huétor Cinema", "Huétor Cinema", Some("E0981"), None),
    ("Kinépolis Granada", "Kinépolis Granada", Some("E0452"), None),
    ("Kinépolis Nevada", "Kinépolis Nevada", Some("E0866"), None),
    ("Megarama Granada", "Megarama Granada", Some("E0301"), None),
    ("Motril Cinema", "Motril Cinema", Some("E0869"), None),
    ("Ocine Serrallo", "Ocine Serrallo", Some("E0787"), Some("serrallo")),
    ("Salón Cine Ideal", "Salón Cine Ideal", Some("E0849"), None),
    ("Teatro Isabel La Catolica", "Teatro Isabel La Catolica", Some("E0712"), None)
  ))
  private def p_guadalajara: R = ("guadalajara", "Guadalajara", "Castilla-La Mancha", 40.62862, -3.16185, "Europe/Madrid", Seq("Azuqueca de Henares", "Guadalajara"), Seq(
    ("Cultura Azuqueca", "Cultura Azuqueca", Some("E0955"), None),
    ("Multicines Guadalajara", "Multicines Guadalajara", Some("E0511"), None)
  ))
  private def p_guipuzcoa: R = ("guipuzcoa", "Guipúzcoa", "País Vasco", 43.31283, -1.97499, "Europe/Madrid", Seq("San Sebastián", "Eibar", "Azkoitia", "Beasain", "Ibarra", "Irun", "Ordizia", "Oñati", "Renteria", "Usurbil", "Zumaia"), Seq(
    ("Aita Mari Zinema", "Aita Mari Zinema", Some("E0214"), None),
    ("Baztartxo Antzokia", "Baztartxo Antzokia", Some("E0227"), None),
    ("Cine Modelo", "Cine Modelo", Some("E0263"), None),
    ("Cine Príncipe", "Cine Príncipe", Some("E0570"), None),
    ("Cine Trueba", "Cine Trueba", Some("E0603"), None),
    ("Cines Antiguo Berri", "Cines Antiguo Berri", Some("E0329"), None),
    ("Cinesa Urbil", "Cinesa Urbil", Some("E0296"), None),
    ("Herri Antzokia ", "Herri Antzokia ", Some("E0890"), None),
    ("Leidor Zinema", "Leidor Zinema", Some("E0472"), None),
    ("Multicines Niessen Zinemak", "Multicines Niessen Zinemak", Some("E0637"), None),
    ("Ocine Mendibil", "Ocine Mendibil", Some("E0537"), Some("mendibil")),
    ("Oñatiko Zinea", "Oñatiko Zinea", Some("E0551"), None),
    ("Teatro Coliseo", "Teatro Coliseo", Some("E0769"), None),
    ("Usurbe Antzokia", "Usurbe Antzokia", Some("E0605"), None)
  ))
  private def p_huelva: R = ("huelva", "Huelva", "Andalucía", 37.26638, -6.94004, "Europe/Madrid", Seq("Ayamonte", "Huelva", "Cortegana", "Isla-Cristina", "Lepe", "Mazagón", "Palma del Condado", "Punta Umbría"), Seq(
    ("Al-Andalus Punta Umbría 3D", "Al-Andalus Punta Umbría 3D", Some("E0641"), None),
    ("Artesiete Holea", "Artesiete Holea", Some("E0805"), None),
    ("Cine 3D Ayamonte", "Cine 3D Ayamonte", Some("E0788"), None),
    ("Cine Alba Mazagón", "Cine Alba Mazagón", Some("E0995"), None),
    ("Cine Vip 3d Lepe", "Cine Vip 3d Lepe", Some("E0765"), None),
    ("Cines Aqualón", "Cines Aqualón", Some("E0278"), None),
    ("Condado Cinemas 7", "Condado Cinemas 7", Some("E0429"), None),
    ("Cortegana Cinema", "Cortegana Cinema", Some("E2913"), None),
    ("La Dehesa Ayamonte", "La Dehesa Ayamonte", Some("E0945"), None),
    ("Multicines La Dehesa - Islantilla", "Multicines La Dehesa - Islantilla", Some("E0516"), None)
  ))
  private def p_huesca: R = ("huesca", "Huesca", "Aragón", 42.13615, -0.4087, "Europe/Madrid", Seq("Binéfar", "Barbastro", "Boltaña", "Huesca", "Monzón", "Sabiñánigo"), Seq(
    ("Auditorio La Colina", "Auditorio La Colina", Some("E0643"), None),
    ("Cine Cortés", "Cine Cortés", Some("E0250"), None),
    ("Cine La Paz", "Cine La Paz", Some("E0256"), None),
    ("Cine Teatro Victoria", "Cine Teatro Victoria", Some("E0271"), None),
    ("CineMundo Huesca", "CineMundo Huesca", Some("E0497"), None),
    ("Palacio De Congresos Boltaña", "Palacio De Congresos Boltaña", Some("E0667"), None),
    ("Teatro Municipal Los Titiriteros", "Teatro Municipal Los Titiriteros", Some("E0964"), None)
  ))
  private def p_islas_baleares: R = ("islas-baleares", "Islas Baleares", "Islas Baleares", 39.56939, 2.65024, "Europe/Madrid", Seq("Palma de Mallorca", "Ciutadella de Menorca", "Ibiza", "Manacor", "Marratxí", "Maó", "Sant Antoni de Portmany", "Santa Eulalia"), Seq(
    ("Artesiete Fan", "Artesiete Fan", Some("E0863"), None),
    ("Cine Regio", "Cine Regio", Some("E0839"), None),
    ("CineCiutat", "CineCiutat", Some("E0365"), None),
    ("Cinema Ca-Los", "Cinema Ca-Los", Some("E0229"), None),
    ("Cinemes Moix Negre", "Cinemes Moix Negre", Some("E0782"), None),
    ("Cines Ocimax", "Cines Ocimax", Some("E0360"), None),
    ("Cinesa Festival Park", "Cinesa Festival Park", Some("E0386"), None),
    ("Multicines Eivissa", "Multicines Eivissa", Some("E0503"), None),
    ("Multicines Manacor", "Multicines Manacor", Some("E0522"), None),
    ("Multicines Rivoli", "Multicines Rivoli", Some("E0533"), None),
    ("Ocimax Multisalas", "Ocimax Multisalas", Some("E0639"), None),
    ("Sala Augusta", "Sala Augusta", Some("E0593"), None),
    ("Teatro España", "Teatro España", Some("E0755"), None)
  ))
  private def p_jaen: R = ("jaen", "Jaén", "Andalucía", 37.76922, -3.79028, "Europe/Madrid", Seq("Andújar", "Alcalá la Real", "Carolina", "Fraile", "Linares", "Martos", "Úbeda"), Seq(
    ("Autocinema Tenerife", "Autocinema Tenerife", Some("E2912"), None),
    ("Cine Teatro Martínez Montañés", "Cine Teatro Martínez Montañés", Some("E1025"), None),
    ("Europa Pantallas 8", "Europa Pantallas 8", Some("E0436"), None),
    ("Multicines Bowling", "Multicines Bowling", Some("E0239"), None),
    ("Multicines Carolina", "Multicines Carolina", Some("E0823"), None),
    ("Multicines Úbeda", "Multicines Úbeda", Some("E0538"), None),
    ("París Multicines", "París Multicines", Some("E0822"), None),
    ("Teatro Maestro Álvarez Alonso", "Teatro Maestro Álvarez Alonso", Some("E1017"), None)
  ))
  private def p_la_rioja: R = ("la-rioja", "La Rioja", "La Rioja", 42.46615, -2.45115, "Europe/Madrid", Seq("Logroño", "Calahorra", "Haro", "Rincón de Soto", "Sto Domingo de la Calzada"), Seq(
    ("Cine Avenida Rincón de Soto", "Cine Avenida Rincón de Soto", Some("E0829"), None),
    ("Cine Avenida Santo Domingo", "Cine Avenida Santo Domingo", Some("E0830"), None),
    ("Cines 7 Infantes", "Cines 7 Infantes", Some("E0804"), None),
    ("Cines Arcca", "Cines Arcca", Some("E0801"), None),
    ("Cines Moderno", "Cines Moderno", Some("E0358"), None),
    ("Teatro Bretón", "Teatro Bretón", Some("E0986"), None),
    ("Yelmo Cines Premium Berceo", "Yelmo Cines Premium Berceo", Some("E0733"), None)
  ))
  private def p_las_palmas: R = ("las-palmas", "Las Palmas", "Canarias", 28.10178, -15.41573, "Atlantic/Canary", Seq("Arrecife", "Las Palmas de Gran Canaria", "Antigua", "Puerto del Rosario", "Santa Lucia de Tirajana", "Telde"), Seq(
    ("Artesiete Las Terrazas", "Artesiete Las Terrazas", Some("E0723"), None),
    ("Deiland Multicines", "Deiland Multicines", Some("E0785"), None),
    ("Multicine Atlántida", "Multicine Atlántida", Some("E0484"), None),
    ("Multicines Deiland", "Multicines Deiland", Some("E0485"), None),
    ("Odeón Puerto del Rosario", "Odeón Puerto del Rosario", Some("E0898"), None),
    ("Yelmo Cines Fuerteventura", "Yelmo Cines Fuerteventura", Some("E0618"), None),
    ("Yelmo Cines Las Arenas", "Yelmo Cines Las Arenas", Some("E0754"), None),
    ("Yelmo Cines Premium Alisios", "Yelmo Cines Premium Alisios", Some("E0972"), None),
    ("Yelmo Cines Vecindario", "Yelmo Cines Vecindario", Some("E0634"), None)
  ))
  private def p_leon: R = ("leon", "León", "Castilla y León", 42.60003, -5.57032, "Europe/Madrid", Seq("León", "Astorga", "Cistierna", "Ponferrada", "Santa María del Páramo", "Villablino"), Seq(
    ("Cine Marí", "Cine Marí", Some("E0901"), None),
    ("Cine Paramés", "Cine Paramés", Some("E0854"), None),
    ("Cine Velasco", "Cine Velasco", Some("E0274"), None),
    ("Cines Van Gogh", "Cines Van Gogh", Some("E0369"), None),
    ("El cine Villablino", "El cine Villablino", Some("E0888"), None),
    ("La Dehesa Ponferrada", "La Dehesa Ponferrada", Some("E0458"), None),
    ("Odeon Multicines León", "Odeon Multicines León", Some("E0280"), None)
  ))
  private def p_lugo: R = ("lugo", "Lugo", "Galicia", 43.00992, -7.55602, "Europe/Madrid", Seq("Lugo", "Monforte de Lemos", "Ribadeo", "Viveiro"), Seq(
    ("Cine Ribadeo", "Cine Ribadeo", Some("E0740"), None),
    ("Cines Viveiro 3D", "Cines Viveiro 3D", Some("E0846"), None),
    ("Multicines Cristal", "Multicines Cristal", Some("E0501"), None),
    ("Multicines Hollywood", "Multicines Hollywood", Some("E0513"), None),
    ("Yelmo Cines As Termas", "Yelmo Cines As Termas", Some("E0684"), None)
  ))
  private def p_lerida: R = ("lerida", "Lérida", "Cataluña", 41.61674, 0.62218, "Europe/Madrid", Seq("Mollerussa", "Agramunt", "Almacelles", "Alpicat", "Balaguer", "Bellpuig", "La Seu d'Urgell", "Linyola", "Pont de Suert", "Solsona", "Tremp", "Tàrrega", "Vielha"), Seq(
    ("Autocine Resquitx - Golmés", "Autocine Resquitx - Golmés", Some("E1033"), None),
    ("Cinema Armengol", "Cinema Armengol", Some("E0302"), None),
    ("Cinema Casal Agramunt", "Cinema Casal Agramunt", Some("E0303"), None),
    ("Cinema El Casal", "Cinema El Casal", Some("E0650"), None),
    ("Cinema Era Audiovisuau", "Cinema Era Audiovisuau", Some("E0232"), None),
    ("Cinema La Lira", "Cinema La Lira", Some("E0305"), None),
    ("Cinema Mollerussa", "Cinema Mollerussa", Some("E0973"), None),
    ("Cinema Paris", "Cinema Paris", Some("E0309"), None),
    ("Cinema Planell", "Cinema Planell", Some("E0310"), None),
    ("Cinema Ribagorza", "Cinema Ribagorza", Some("E0313"), None),
    ("Cinemes Guiu", "Cinemes Guiu", Some("E0319"), None),
    ("Cinemes Majestic", "Cinemes Majestic", Some("E0799"), None),
    ("Cinemes Urgell", "Cinemes Urgell", Some("E0325"), None),
    ("Jca Cinemes Alpicat", "Jca Cinemes Alpicat", Some("E0652"), None),
    ("Sala d''actes Ajuntament", "Sala d''actes Ajuntament", Some("E0985"), None)
  ))
  private def p_madrid: R = ("madrid", "Madrid", "Comunidad de Madrid", 40.4165, -3.70256, "Europe/Madrid", Seq("Madrid", "Alcobendas", "Alcorcón", "Boadilla del Monte", "Coslada", "Fuente la Teja", "Leganés", "Majadahonda", "Tres Cantos", "Valdemorillo", "Valdemoro", "Villaviciosa de Odón", "Alcalá de Henares", "Aranjuez", "Arroyomolinos", "Collado Villalba", "Fuenlabrada", "Getafe", "Guadarrama", "Las Rozas de Madrid", "Móstoles", "Parla", "Pelayos de la Presa", "Pinto", "Pozuelo de Alarcón", "Rivas-Vaciamadrid", "San Martín de Valdeiglesias", "San Sebastián de los Reyes", "Soto del Real", "Torrejón de Ardoz", "Villa del Prado"), Seq(
    ("Casa de Cultura Guadarrama", "Casa de Cultura Guadarrama", Some("E0936"), None),
    ("Centro de Arte y Cine de Verano Soto del Real", "Centro de Arte y Cine de Verano Soto del Real", Some("E0937"), None),
    ("Cine Aranjuez", "Cine Aranjuez", Some("E0233"), None),
    ("Cine Colíseo de la Cultura", "Cine Colíseo de la Cultura", Some("E0707"), None),
    ("Cine Giralt Laporta", "Cine Giralt Laporta", Some("E0753"), None),
    ("Cine Los Molinos", "Cine Los Molinos", Some("E0729"), None),
    ("Cine Teatro Municipal", "Cine Teatro Municipal", Some("E0419"), None),
    ("Cine de Verano El Molino", "Cine de Verano El Molino", Some("E0674"), None),
    ("Cine de Verano Juan Falco", "Cine de Verano Juan Falco", Some("E0750"), None),
    ("Cine de Verano Valdemoro", "Cine de Verano Valdemoro", Some("E0675"), None),
    ("Cine de verano el castillo", "Cine de verano el castillo", Some("E0672"), None),
    ("Cinebox 3 C", "Cinebox 3 C", Some("E0199"), None),
    ("Cines Boadilla", "Cines Boadilla", Some("E0760"), None),
    ("Cines Dos de Mayo", "Cines Dos de Mayo", Some("E0761"), None),
    ("Cines La Rambla", "Cines La Rambla", Some("E0353"), None),
    ("Cines Plaza Coslada", "Cines Plaza Coslada", Some("E2910"), None),
    ("Cines Princesa", "Cines Princesa", Some("E0364"), None),
    ("Cines Villa", "Cines Villa", Some("E0190"), None),
    ("Cines Zoco Majadahonda", "Cines Zoco Majadahonda", Some("E0582"), None),
    ("Cinesa Equinoccio", "Cinesa Equinoccio", Some("E0385"), None),
    ("Cinesa Heron City Las Rozas", "Cinesa Heron City Las Rozas", Some("E0389"), None),
    ("Cinesa Intu Xanadú", "Cinesa Intu Xanadú", Some("E0406"), None),
    ("Cinesa La Gavia", "Cinesa La Gavia", Some("E0731"), None),
    ("Cinesa La Moraleja", "Cinesa La Moraleja", Some("E0392"), None),
    ("Cinesa Manoteras", "Cinesa Manoteras", Some("E0646"), None),
    ("Cinesa Méndez Álvaro", "Cinesa Méndez Álvaro", Some("E0247"), None),
    ("Cinesa Nassica", "Cinesa Nassica", Some("E0246"), None),
    ("Cinesa Parquesur", "Cinesa Parquesur", Some("E0399"), None),
    ("Cinesa Plaza Loranca 2", "Cinesa Plaza Loranca 2", Some("E0394"), None),
    ("Cinesa Príncipe Pío", "Cinesa Príncipe Pío", Some("E0401"), None),
    ("Kinépolis Madrid", "Kinépolis Madrid", Some("E0453"), None),
    ("Kinépolis Madrid Diversia", "Kinépolis Madrid Diversia", Some("E0209"), None),
    ("Multicines Cisneros", "Multicines Cisneros", Some("E0498"), None),
    ("Ocine Plaza Éboli", "Ocine Plaza Éboli", Some("E2900"), Some("plazaeboli")),
    ("Ocine Urban X-Madrid", "Ocine Urban X-Madrid", Some("E1004"), Some("urbanxmadrid")),
    ("Odeon Multicines Sambil Dolby Atmos", "Odeon Multicines Sambil Dolby Atmos", Some("E0877"), None),
    ("Odeon Multicines Tres Cantos", "Odeon Multicines Tres Cantos", Some("E0815"), None),
    ("Restón Cinema", "Restón Cinema", Some("E0584"), None),
    ("Sala Babel", "Sala Babel", Some("E0818"), None),
    ("Spazio Cines", "Spazio Cines", Some("E0935"), None),
    ("Teatro Fernández-Baldor", "Teatro Fernández-Baldor", Some("E1000"), None),
    ("Yelmo Cines Ideal", "Yelmo Cines Ideal", Some("E0621"), None),
    ("Yelmo Cines Islazul", "Yelmo Cines Islazul", Some("E0681"), None),
    ("Yelmo Cines La Vaguada", "Yelmo Cines La Vaguada", Some("E0459"), None),
    ("Yelmo Cines Planetocio", "Yelmo Cines Planetocio", Some("E0630"), None),
    ("Yelmo Cines Plaza Norte 2", "Yelmo Cines Plaza Norte 2", Some("E2916"), None),
    ("Yelmo Cines Plenilunio", "Yelmo Cines Plenilunio", Some("E0475"), None),
    ("Yelmo Cines Premium Parque Corredor", "Yelmo Cines Premium Parque Corredor", Some("E0291"), None),
    ("Yelmo Cines Rivas H2O", "Yelmo Cines Rivas H2O", Some("E0671"), None),
    ("Yelmo Cines TresAguas", "Yelmo Cines TresAguas", Some("E0207"), None),
    ("mk2 Palacio de Hielo (antiguos Cines Dreams)", "mk2 Palacio de Hielo (antiguos Cines Dreams)", Some("E0432"), None)
  ))
  private def p_melilla: R = ("melilla", "Melilla", "Melilla", 35.29369, -2.93833, "Europe/Madrid", Seq("Melilla"), Seq(
    ("Cine Teatro Perelló", "Cine Teatro Perelló", Some("E0841"), None)
  ))
  private def p_murcia: R = ("murcia", "Murcia", "Región de Murcia", 37.98704, -1.13004, "Europe/Madrid", Seq("Murcia", "Cartagena", "San Jose de la Montaña", "Abarán", "Archena", "Colonia Julio Ruiz Alda", "Gorguel", "Islas Menores", "Lorca", "Molina de Segura", "Mula", "Playa Honda", "Puerto de Mazarron", "San Javier", "San Pedro del Pinatar", "Yecla", "Águilas"), Seq(
    ("Cine Acapulco", "Cine Acapulco", Some("E0942"), None),
    ("Cine Bahía", "Cine Bahía", Some("E0977"), None),
    ("Cine La Manga", "Cine La Manga", Some("E0946"), None),
    ("Cine Pya", "Cine Pya", Some("E0838"), None),
    ("Cine Sirenas", "Cine Sirenas", Some("E0947"), None),
    ("Cine de Verano Abarán", "Cine de Verano Abarán", Some("E0938"), None),
    ("Cine de Verano de Archena", "Cine de Verano de Archena", Some("E0962"), None),
    ("Cinema Velasco Totana", "Cinema Velasco Totana", Some("E0921"), None),
    ("Cines Almenara Lorca", "Cines Almenara Lorca", Some("E0751"), None),
    ("Cines IMF Galán", "Cines IMF Galán", Some("E0956"), None),
    ("Cinesa Nueva Condomina", "Cinesa Nueva Condomina", Some("E0656"), None),
    ("Multicines El Hornillo", "Multicines El Hornillo", Some("E0506"), None),
    ("NeoCine Espacio Mediterraneo", "NeoCine Espacio Mediterraneo", Some("E0663"), None),
    ("Neocine Centrofama", "Neocine Centrofama", Some("E0193"), None),
    ("Neocine Dos Mares", "Neocine Dos Mares", Some("E0431"), None),
    ("Neocine El Tiro", "Neocine El Tiro", Some("E0774"), None),
    ("Neocine HD Digital Vega Plaza", "Neocine HD Digital Vega Plaza", Some("E0660"), None),
    ("Neocine Mandarache", "Neocine Mandarache", Some("E0546"), None),
    ("Neocine Rex", "Neocine Rex", Some("E0690"), None),
    ("Neocine Thader", "Neocine Thader", Some("E0547"), None),
    ("Nuevos Cines Cabos de Palos ", "Nuevos Cines Cabos de Palos ", Some("E0953"), None),
    ("Terraza Auditorio Parque Municipal", "Terraza Auditorio Parque Municipal", Some("E0922"), None),
    ("Terraza Centro Joven", "Terraza Centro Joven", Some("E0988"), None),
    ("Terraza España", "Terraza España", Some("E0949"), None)
  ))
  private def p_malaga: R = ("malaga", "Málaga", "Andalucía", 36.72016, -4.42034, "Europe/Madrid", Seq("Málaga", "Marbella", "Fuengirola", "Alhaurín el Grande", "Antequera", "Coín", "Nerja", "Rincón de la Victoria", "Ronda", "Vélez-Málaga"), Seq(
    ("Alameda Multicines Malaga", "Alameda Multicines Malaga", Some("E0048"), None),
    ("Centro Cultural Villa de Nerja", "Centro Cultural Villa de Nerja", Some("E0976"), None),
    ("Cine Albéniz", "Cine Albéniz", Some("E0195"), None),
    ("Cine Pixel", "Cine Pixel", Some("E0215"), None),
    ("Cine San Francisco", "Cine San Francisco", Some("E1005"), None),
    ("Cines Gran Marbella", "Cines Gran Marbella", Some("E0427"), None),
    ("Cines La Verónica", "Cines La Verónica", Some("E0410"), None),
    ("Kinépolis La Cañada", "Kinépolis La Cañada", Some("E0390"), None),
    ("Multicines Alfil 3D", "Multicines Alfil 3D", Some("E0059"), None),
    ("Multicines Ronda", "Multicines Ronda", Some("E0534"), None),
    ("Multicines Rosaleda", "Multicines Rosaleda", Some("E0589"), None),
    ("Red Dog Cinemas", "Red Dog Cinemas", Some("E0845"), None),
    ("Yelmo Cines Rincón De La Victoria", "Yelmo Cines Rincón De La Victoria", Some("E0632"), None),
    ("mk2 El Ingenio", "mk2 El Ingenio", Some("E0433"), None),
    ("mk2 Malaga Nostrum", "mk2 Malaga Nostrum", Some("E0413"), None),
    ("mk2 Miramar", "mk2 Miramar", Some("E0414"), None)
  ))
  private def p_navarra: R = ("navarra", "Navarra", "Navarra", 42.81687, -1.64323, "Europe/Madrid", Seq("Pamplona", "Cordovilla", "Estella", "Huarte", "Tudela", "Viaña"), Seq(
    ("Cines Las Cañas Viana", "Cines Las Cañas Viana", Some("E0371"), None),
    ("Cines Los Llanos Zinemak", "Cines Los Llanos Zinemak", Some("E0259"), None),
    ("Golem Baiona", "Golem Baiona", Some("E0444"), None),
    ("Golem La Morea", "Golem La Morea", Some("E0443"), None),
    ("Golem Yamaguchi", "Golem Yamaguchi", Some("E0445"), None),
    ("Ocine Tudela", "Ocine Tudela", Some("E0317"), None),
    ("Yelmo Cines Itaroa", "Yelmo Cines Itaroa", Some("E0283"), None)
  ))
  private def p_ourense: R = ("ourense", "Ourense", "Galicia", 42.33669, -7.86407, "Europe/Madrid", Seq("Ourense", "Agra", "Xinzo de Limia"), Seq(
    ("Cine Gesma", "Cine Gesma", Some("E0834"), None),
    ("Cinebox Ourense", "Cinebox Ourense", Some("E0289"), None),
    ("Multicines Ponte Vella", "Multicines Ponte Vella", Some("E0813"), None),
    ("NovoCine Leiro 3D", "NovoCine Leiro 3D", Some("E0847"), None)
  ))
  private def p_palencia: R = ("palencia", "Palencia", "Castilla y León", 42.00955, -4.52406, "Europe/Madrid", Seq("Aguilar de Campo", "Palencia", "Guardo"), Seq(
    ("Cine AMGu", "Cine AMGu", Some("E0998"), None),
    ("Cines Campoo", "Cines Campoo", Some("E0334"), None),
    ("Cines Campoo 3D", "Cines Campoo 3D", Some("E1003"), None),
    ("Cines Ortega", "Cines Ortega", Some("E0361"), None),
    ("Multicines Avenida", "Multicines Avenida", Some("E0331"), None)
  ))
  private def p_pontevedra: R = ("pontevedra", "Pontevedra", "Galicia", 42.431, -8.64435, "Europe/Madrid", Seq("Vigo", "Pontevedra", "Aguete", "Caldas de Reis", "Estrada", "Ramallosa", "Vilagarcia"), Seq(
    ("Cine Club Pontevedra", "Cine Club Pontevedra", Some("E0875"), None),
    ("Cine Imperial", "Cine Imperial", Some("E0835"), None),
    ("Cine Seixo", "Cine Seixo", Some("E0899"), None),
    ("Cines Avenida 3D", "Cines Avenida 3D", Some("E0742"), None),
    ("Cines Tamberlick Plaza Elíptica", "Cines Tamberlick Plaza Elíptica", Some("E0739"), None),
    ("Minicines Central ", "Minicines Central ", Some("E0895"), None),
    ("Multicines Cinexpo", "Multicines Cinexpo", Some("E0298"), None),
    ("Multicines Gran Arousa", "Multicines Gran Arousa", Some("E0510"), None),
    ("Multicines Norte", "Multicines Norte", Some("E0525"), None),
    ("Teatro Salesianos", "Teatro Salesianos", Some("E0602"), None),
    ("Yelmo Cines Premium Vialia Vigo", "Yelmo Cines Premium Vialia Vigo", Some("E2902"), None),
    ("Yelmo Cines Travesía Vigo", "Yelmo Cines Travesía Vigo", Some("E0635"), None)
  ))
  private def p_salamanca: R = ("salamanca", "Salamanca", "Castilla y León", 40.42972, -3.67975, "Europe/Madrid", Seq("Salamanca", "Béjar", "Ciudad Rodrigo", "Peñaranda de Bracamonte"), Seq(
    ("Cine Calderón", "Cine Calderón", Some("E0793"), None),
    ("Cine Juventud", "Cine Juventud", Some("E0800"), None),
    ("Cines Van Dyck", "Cines Van Dyck", Some("E0606"), None),
    ("Cines Van Dyck Tormes", "Cines Van Dyck Tormes", Some("E0368"), None),
    ("Megarama Salamanca", "Megarama Salamanca", Some("E0299"), None),
    ("Multicines Béjar", "Multicines Béjar", Some("E0492"), None)
  ))
  private def p_santa_cruz_de_tenerife: R = ("santa-cruz-de-tenerife", "Santa Cruz de Tenerife", "Canarias", 28.46824, -16.25462, "Atlantic/Canary", Seq("Adeje", "Santa Cruz de Tenerife", "Candelaria", "Llanos de Aridane", "Orotava", "Realejos", "San Cristobal de la Laguna", "Santa Cruz de la Palma"), Seq(
    ("Cine Realejos", "Cine Realejos", Some("E0267"), None),
    ("Cines Price Prime", "Cines Price Prime", Some("E0583"), None),
    ("Multicines Millennium", "Multicines Millennium", Some("E0940"), None),
    ("Multicines Puntalarga", "Multicines Puntalarga", Some("E0532"), None),
    ("Multicines Tenerife", "Multicines Tenerife", Some("E0284"), None),
    ("Multicines Zentral Center", "Multicines Zentral Center", Some("E0541"), None),
    ("Teatro Chico", "Teatro Chico", Some("E0900"), None),
    ("X-Sur Cine", "X-Sur Cine", Some("E0700"), None),
    ("Yelmo Cines La Villa de Orotava", "Yelmo Cines La Villa de Orotava", Some("E0622"), None),
    ("Yelmo Cines Meridiano", "Yelmo Cines Meridiano", Some("E0627"), None)
  ))
  private def p_segovia: R = ("segovia", "Segovia", "Castilla y León", 40.94808, -4.11839, "Europe/Madrid", Seq("Segovia"), Seq(
    ("Artesiete Segovia", "Artesiete Segovia", Some("E0716"), None),
    ("Cines Luz de Castilla", "Cines Luz de Castilla", Some("E0285"), None)
  ))
  private def p_sevilla: R = ("sevilla", "Sevilla", "Andalucía", 37.38283, -5.97317, "Europe/Madrid", Seq("Sevilla", "Dos Hermanas", "Alcalá de Guadaira", "Bormujos", "Cabezas de San Juan", "Camas", "Estepa", "Lebrija", "Mairena del Aljarafe", "Marchena", "Tomares", "Utrera", "Écija"), Seq(
    ("Al-Andalus Mega Ocio", "Al-Andalus Mega Ocio", Some("E0217"), None),
    ("Artesiete Écija", "Artesiete Écija", Some("E0720"), None),
    ("Avenida 5 Cines", "Avenida 5 Cines", Some("E0112"), None),
    ("Cine Méliès Estepa", "Cine Méliès Estepa", Some("E0996"), None),
    ("Cine Planelles", "Cine Planelles", Some("E0724"), None),
    ("Cineapolis Dos Hermanas 3D", "Cineapolis Dos Hermanas 3D", Some("E0191"), None),
    ("Cineapolis Utrera", "Cineapolis Utrera", Some("E1039"), None),
    ("Cineapolis WAY", "Cineapolis WAY", Some("E1040"), None),
    ("Cinema Tomares", "Cinema Tomares", Some("E0676"), None),
    ("Cinesa Camas", "Cinesa Camas", Some("E0027"), None),
    ("Los Arcos Multicines", "Los Arcos Multicines", Some("E0222"), None),
    ("Metromar Cinemas 12", "Metromar Cinemas 12", Some("E0666"), None),
    ("Odeon Multicines Plaza de Armas", "Odeon Multicines Plaza de Armas", Some("E0400"), None),
    ("Teatro Municipal Juan Bernabé", "Teatro Municipal Juan Bernabé", Some("E0984"), None),
    ("Teatro Municipal Las Cabezas de San Juan", "Teatro Municipal Las Cabezas de San Juan", Some("E1020"), None),
    ("Yelmo Cines Premium Lagoh", "Yelmo Cines Premium Lagoh", Some("E1002"), None),
    ("Zona Este", "Zona Este", Some("E0476"), None),
    ("mk2 Alcores", "mk2 Alcores", Some("E0411"), None),
    ("mk2 Nervión Plaza", "mk2 Nervión Plaza", Some("E0415"), None)
  ))
  private def p_soria: R = ("soria", "Soria", "Castilla y León", 41.76401, -2.46883, "Europe/Madrid", Seq("Almazán", "Burgo de Osma", "Golmayo"), Seq(
    ("Cine Calderón - Almazán", "Cine Calderón - Almazán", Some("E0989"), None),
    ("Cine Palafox Burgo de Osma", "Cine Palafox Burgo de Osma", Some("E0265"), None),
    ("Cines Lara", "Cines Lara", Some("E0356"), None)
  ))
  private def p_tarragona: R = ("tarragona", "Tarragona", "Cataluña", 41.11905, 1.24544, "Europe/Madrid", Seq("Tarragona", "Altafulla", "Amposta", "Calafell", "Cambrils", "Montblanc", "Reus", "Roquetes", "Valls", "Vila-seca"), Seq(
    ("Cinema Casal Montblanquí", "Cinema Casal Montblanquí", Some("E0161"), None),
    ("Cinemes Amposta", "Cinemes Amposta", Some("E0076"), None),
    ("Cines Axion Reus", "Cines Axion Reus", Some("E0920"), None),
    ("JCA Cinemes Tarragona Valls", "JCA Cinemes Tarragona Valls", Some("E0908"), None),
    ("MCB Altafulla - Les Bruixes", "MCB Altafulla - Les Bruixes", Some("E0320"), None),
    ("MCB Calafell", "MCB Calafell", Some("E0479"), None),
    ("Ocine Gavarres", "Ocine Gavarres", Some("E0509"), Some("gavarres")),
    ("Ocine Roquetes", "Ocine Roquetes", Some("E0556"), Some("roquetes")),
    ("Ocine Vila-seca", "Ocine Vila-seca", Some("E0727"), Some("vilaseca")),
    ("Rambla de L'art", "Rambla de L'art", Some("E0811"), None),
    ("Yelmo Cines Parc Central", "Yelmo Cines Parc Central", Some("E0807"), None)
  ))
  private def p_teruel: R = ("teruel", "Teruel", "Aragón", 40.3456, -1.10646, "Europe/Madrid", Seq("Alcañiz", "Arens de Lledo", "Teruel"), Seq(
    ("Cine Arens de Lledó", "Cine Arens de Lledó", Some("E0810"), None),
    ("Cine Maravillas", "Cine Maravillas", Some("E0697"), None),
    ("Cines Alcañiz", "Cines Alcañiz", Some("E0653"), None)
  ))
  private def p_toledo: R = ("toledo", "Toledo", "Castilla-La Mancha", 39.8581, -4.02263, "Europe/Madrid", Seq("Casalgordo", "Olías del Rey", "Quintanar de la Orden", "Talavera de la Reina", "Toledo", "Torrijos", "Villacañas"), Seq(
    ("Artesiete Los Alfares", "Artesiete Los Alfares", Some("E0803"), None),
    ("Cine Central 3D", "Cine Central 3D", Some("E0831"), None),
    ("Cine Princesa", "Cine Princesa", Some("E0837"), None),
    ("Cines Redux", "Cines Redux", Some("E0872"), None),
    ("Quintanar Cinema", "Quintanar Cinema", Some("E0870"), None),
    ("Real Cinema De Olías", "Real Cinema De Olías", Some("E0574"), None),
    ("mk2 Luz del Tajo", "mk2 Luz del Tajo", Some("E0412"), None)
  ))
  private def p_valencia: R = ("valencia", "Valencia", "Comunidad Valenciana", 39.47391, -0.37966, "Europe/Madrid", Seq("Valencia", "Buñol", "Gandia", "Grau i Platja", "Sagunto", "Alboraya", "Aldaia", "Alfafar", "Almussafes", "Alzira", "Benetússer", "Burjassot", "Cofrentes", "Cullera", "Eliana", "Guardamar de la Safor", "Llíria", "Oliva", "Ontinyent", "Pai i Capellans", "Paterna", "Perello", "Porta Coeli", "Tavernes de la Valldigna", "Xirivella", "Xàtiva"), Seq(
    ("Abc El Saler", "Abc El Saler", Some("E0034"), None),
    ("Abc Gandia", "Abc Gandia", Some("E0036"), None),
    ("Abc Gran Turia", "Abc Gran Turia", Some("E0037"), None),
    ("Abc Park", "Abc Park", Some("E0040"), None),
    ("Alucine Sagunto", "Alucine Sagunto", Some("E0071"), None),
    ("Autocine Star", "Autocine Star", Some("E0104"), None),
    ("Centre Cultural Almassafes", "Centre Cultural Almassafes", Some("E0759"), None),
    ("Centre Cultural Benetússer El Molí", "Centre Cultural Benetússer El Molí", Some("E0758"), None),
    ("Cine Avenida El Perelló", "Cine Avenida El Perelló", Some("E2914"), None),
    ("Cine La Unió Musical", "Cine La Unió Musical", Some("E0992"), None),
    ("Cine Montecarlo", "Cine Montecarlo", Some("E0883"), None),
    ("Cine Palacio de la Música de Buñol", "Cine Palacio de la Música de Buñol", Some("E0993"), None),
    ("Cine Teatro Principal Requena", "Cine Teatro Principal Requena", Some("E1029"), None),
    ("Cine Terraza Charly", "Cine Terraza Charly", Some("E0927"), None),
    ("Cine Terraza Olimpo", "Cine Terraza Olimpo", Some("E0928"), None),
    ("Cine Tívoli", "Cine Tívoli", Some("E0645"), None),
    ("Cine de Verano Serra", "Cine de Verano Serra", Some("E0930"), None),
    ("Cine de Verano Tugar", "Cine de Verano Tugar", Some("E0929"), None),
    ("Cineapolis El Teler", "Cineapolis El Teler", Some("E0617"), None),
    ("Cines Axion Premium Gandía", "Cines Axion Premium Gandía", Some("E1026"), None),
    ("Cines Axion de Xàtiva", "Cines Axion de Xàtiva", Some("E0664"), None),
    ("Cines Babel", "Cines Babel", Some("E0119"), None),
    ("Cines Lys", "Cines Lys", Some("E0187"), None),
    ("Cines MN4", "Cines MN4", Some("E0287"), None),
    ("Cines Victoria Cullera", "Cines Victoria Cullera", Some("E0210"), None),
    ("Cinesa Bonaire", "Cinesa Bonaire", Some("E0405"), None),
    ("Cinestudio D´or", "Cinestudio D´or", Some("E0407"), None),
    ("Kinepolis Alzira", "Kinepolis Alzira", Some("E0434"), None),
    ("Kinépolis Valencia", "Kinépolis Valencia", Some("E0454"), None),
    ("Ocine Premium Aqua", "Ocine Premium Aqua", Some("E0474"), Some("premiumaqua")),
    ("Ozone Gandía", "Ozone Gandía", Some("E0282"), None),
    ("Teatro Flumen", "Teatro Flumen", Some("E0967"), None),
    ("Teatro García Berlanga", "Teatro García Berlanga", Some("E1030"), None),
    ("Terraza Lumiere", "Terraza Lumiere", Some("E0931"), None),
    ("Terraza de Verano", "Terraza de Verano", Some("E0987"), None),
    ("Terraza de Verano Oliva", "Terraza de Verano Oliva", Some("E0730"), None),
    ("Yelmo Cines Campanar", "Yelmo Cines Campanar", Some("E0248"), None),
    ("Yelmo Cines Mercado de Campanar", "Yelmo Cines Mercado de Campanar", Some("E0773"), None),
    ("Yelmo Cines VidaNova Parc", "Yelmo Cines VidaNova Parc", Some("E0932"), None)
  ))
  private def p_valladolid: R = ("valladolid", "Valladolid", "Castilla y León", 41.65541, -4.72353, "Europe/Madrid", Seq("Valladolid", "Arroyo de la Encomienda", "Medina de Ríoseco", "Medina del Campo", "Pedrajas de San Esteban"), Seq(
    ("Cine Avenida", "Cine Avenida", Some("E0235"), None),
    ("Cine Casablanca", "Cine Casablanca", Some("E0243"), None),
    ("Cines Broadway", "Cines Broadway", Some("E0333"), None),
    ("Cines Manhattan", "Cines Manhattan", Some("E0357"), None),
    ("Multicines Coliseo", "Multicines Coliseo", Some("E0698"), None),
    ("Ocine Rio Shopping", "Ocine Rio Shopping", Some("E0796"), Some("rioshopping")),
    ("Teatro Principal", "Teatro Principal", Some("E0600"), None),
    ("Yelmo Cines Premium VallSur", "Yelmo Cines Premium VallSur", Some("E0297"), None)
  ))
  private def p_vizcaya: R = ("vizcaya", "Vizcaya", "País Vasco", 43.26271, -2.92528, "Europe/Madrid", Seq("Bilbao", "Barakaldo", "Mungia", "Amorebieta", "Andra Mari", "Durango", "Ermua", "Gernika-Lumo", "Getxo", "Kurtzea", "Leioa", "Lekeitio", "Mimetiz", "Santurtzi"), Seq(
    ("Autocine Getxo", "Autocine Getxo", Some("E0880"), None),
    ("Cine Torrebillela", "Cine Torrebillela", Some("E0767"), None),
    ("Cine Torrezabal", "Cine Torrezabal", Some("E0923"), None),
    ("Cine Zugaza", "Cine Zugaza", Some("E0768"), None),
    ("Cinesa Max Ocio", "Cinesa Max Ocio", Some("E0424"), None),
    ("Cinesa Zubiarte", "Cinesa Zubiarte", Some("E0425"), None),
    ("Ermua Antzokia", "Ermua Antzokia", Some("E0903"), None),
    ("Getxo Zinemak", "Getxo Zinemak", Some("E0464"), None),
    ("Golem Alhóndiga", "Golem Alhóndiga", Some("E0737"), None),
    ("Ikusgarri Zinema", "Ikusgarri Zinema", Some("E0891"), None),
    ("Liceo Antzokia", "Liceo Antzokia", Some("E0894"), None),
    ("Multicines 7 Bilbao", "Multicines 7 Bilbao", Some("E0488"), None),
    ("Olalde Aretoa", "Olalde Aretoa", Some("E1021"), None),
    ("Serantes Kultur Aretoa", "Serantes Kultur Aretoa", Some("E0598"), None),
    ("Yelmo Cines Artea", "Yelmo Cines Artea", Some("E0376"), None),
    ("Yelmo Cines Megapark", "Yelmo Cines Megapark", Some("E0626"), None),
    ("Zalla Zine - Antzokia ", "Zalla Zine - Antzokia ", Some("E0874"), None),
    ("Zornotza Aretoa", "Zornotza Aretoa", Some("E0904"), None)
  ))
  private def p_zamora: R = ("zamora", "Zamora", "Castilla y León", 41.50633, -5.74456, "Europe/Madrid", Seq("Zamora"), Seq(
    ("Multicines Zamora", "Multicines Zamora", Some("E0540"), None)
  ))
  private def p_zaragoza: R = ("zaragoza", "Zaragoza", "Aragón", 41.65606, -0.87734, "Europe/Madrid", Seq("Zaragoza", "Calatayud", "Caspe", "Mequinenza", "Zuera"), Seq(
    ("Artesiete La Torre", "Artesiete La Torre", Some("E1041"), None),
    ("Cine Palafox Zaragoza", "Cine Palafox Zaragoza", Some("E0264"), None),
    ("Cine Sala Cervantes", "Cine Sala Cervantes", Some("E0711"), None),
    ("Cines Aragonia", "Cines Aragonia", Some("E0732"), None),
    ("Cinesa Grancasa", "Cinesa Grancasa", Some("E0387"), None),
    ("Cinesa Puerto Venecia", "Cinesa Puerto Venecia", Some("E0790"), None),
    ("Sala Goya", "Sala Goya", Some("E0595"), None),
    ("Teatro Capitol", "Teatro Capitol", Some("E1007"), None),
    ("Teatro Cine Goya", "Teatro Cine Goya", Some("E0668"), None),
    ("Teatro Reina Sofía", "Teatro Reina Sofía", Some("E1008"), None)
  ))
  private def p_alava: R = ("alava", "Álava", "País Vasco", 42.84998, -2.67268, "Europe/Madrid", Seq("Vitoria", "Etxabarri-Ibiña", "Laudio/Llodio"), Seq(
    ("Cine Municipal Llodio", "Cine Municipal Llodio", Some("E0821"), None),
    ("Cines Florida", "Cines Florida", Some("E0346"), None),
    ("Cines Gorbeia Zinemak ", "Cines Gorbeia Zinemak ", Some("E0885"), None),
    ("Cines Guridi", "Cines Guridi", Some("E0763"), None),
    ("Yelmo Cines Boulevard", "Yelmo Cines Boulevard", Some("E0786"), None)
  ))
  private def p_avila: R = ("avila", "Ávila", "Castilla y León", 40.65724, -4.69951, "Europe/Madrid", Seq("Arenas de San Pedro", "Barco de Avila", "Candeleda", "Fuente de la Salud", "Navaluenga", "Ávila"), Seq(
    ("Cine Arenas", "Cine Arenas", Some("E0828"), None),
    ("Cine Blasco", "Cine Blasco", Some("E0980"), None),
    ("Cine Candeleda", "Cine Candeleda", Some("E0861"), None),
    ("Cine Rueda", "Cine Rueda", Some("E0966"), None),
    ("Cine-Teatro Lagasca", "Cine-Teatro Lagasca", Some("E0991"), None),
    ("Cines Bulevar", "Cines Bulevar", Some("E0344"), None)
  ))

  private def chunk0: Seq[R] = Seq(p_a_coruna, p_albacete, p_alicante, p_almeria, p_asturias, p_badajoz, p_barcelona, p_burgos, p_cantabria, p_castellon, p_ceuta, p_ciudad_real, p_cuenca, p_caceres, p_cadiz, p_cordoba, p_girona, p_granada, p_guadalajara, p_guipuzcoa, p_huelva, p_huesca, p_islas_baleares, p_jaen, p_la_rioja, p_las_palmas, p_leon, p_lugo, p_lerida, p_madrid, p_melilla, p_murcia, p_malaga, p_navarra, p_ourense, p_palencia, p_pontevedra, p_salamanca, p_santa_cruz_de_tenerife, p_segovia)
  private def chunk1: Seq[R] = Seq(p_sevilla, p_soria, p_tarragona, p_teruel, p_toledo, p_valencia, p_valladolid, p_vizcaya, p_zamora, p_zaragoza, p_alava, p_avila)
  val provinces: Seq[R] = chunk0 ++ chunk1
}
