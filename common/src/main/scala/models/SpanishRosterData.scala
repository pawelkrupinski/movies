// GENERATED from data/spain/provinces.json by data/spain/scripts/generate_roster.py
// — do NOT edit by hand. Full Spanish cinema roster: 52 provinces / 595 cinemas (SensaCine).
// Regenerate with `python3 data/spain/scripts/generate_roster.py` after re-harvesting;
// see data/spain/README.md.
package models

private[models] object SpanishRosterData {
  // (displayName, pillName, sensacine theaterId)
  type C = (String, String, String)
  // (slug, name, autonomous community, lat, lon, zoneId, cinemas)
  type R = (String, String, String, Double, Double, String, Seq[C])

  private def p_a_coruna: R = ("a-coruna", "A Coruña", "Galicia", 43.37135, -8.396, "Europe/Madrid", Seq(
    ("Barbanza Multicines", "Barbanza Multicines", "E0124"),
    ("Cantones Cines", "Cantones Cines", "E0437"),
    ("Cine Duplex", "Cine Duplex", "E0741"),
    ("Cines Forum Metropolitano", "Cines Forum Metropolitano", "E0441"),
    ("Cines Xunqueira", "Cines Xunqueira", "E0694"),
    ("Cinesa As Cancelas", "Cinesa As Cancelas", "E0795"),
    ("Cinesa Marineda City", "Cinesa Marineda City", "E0770"),
    ("Multicines Bergantiños", "Multicines Bergantiños", "E0494"),
    ("Multicines Compostela", "Multicines Compostela", "E0762"),
    ("Numax", "Numax", "E0848"),
    ("Odeon Multicines Narón", "Odeon Multicines Narón", "E0789"),
    ("Yelmo Cines Espacio Coruña", "Yelmo Cines Espacio Coruña", "E0734")
  ))
  private def p_albacete: R = ("albacete", "Albacete", "Castilla-La Mancha", 38.99424, -1.85643, "Europe/Madrid", Seq(
    ("Cines Coliseum", "Cines Coliseum", "E0710"),
    ("Gran Teatro de Villarrobledo", "Gran Teatro de Villarrobledo", "E0448"),
    ("Yelmo Cines Imaginalia", "Yelmo Cines Imaginalia", "E0205"),
    ("Yelmo Cines Vialia Albacete", "Yelmo Cines Vialia Albacete", "E0778")
  ))
  private def p_alicante: R = ("alicante", "Alicante", "Comunidad Valenciana", 38.34517, -0.48149, "Europe/Madrid", Seq(
    ("Abc Elx", "Abc Elx", "E0035"),
    ("Auto Cine Drive In", "Auto Cine Drive In", "E0197"),
    ("Autocine El Sur", "Autocine El Sur", "E0783"),
    ("Cine Aana Alicante", "Cine Aana Alicante", "E0008"),
    ("Cine Aana San Juan", "Cine Aana San Juan", "E0009"),
    ("Cine BIC", "Cine BIC", "E0033"),
    ("Cine Calp", "Cine Calp", "E0924"),
    ("Cine Club Villena", "Cine Club Villena", "E2915"),
    ("Cine Club Xábia", "Cine Club Xábia", "E0249"),
    ("Cine Horadada", "Cine Horadada", "E0968"),
    ("Cine Imf Ondara", "Cine Imf Ondara", "E0658"),
    ("Cine Imf Torrevieja", "Cine Imf Torrevieja", "E0451"),
    ("Cine Jayan", "Cine Jayan", "E0254"),
    ("Cine La Esperanza", "Cine La Esperanza", "E0961"),
    ("Cine Las Villas", "Cine Las Villas", "E0971"),
    ("Cine Navas", "Cine Navas", "E0545"),
    ("Cine Navia", "Cine Navia", "E0970"),
    ("Cinebox Plaza Mar 2", "Cinebox Plaza Mar 2", "E0293"),
    ("Cinema Roma", "Cinema Roma", "E0268"),
    ("Cines Axion Playa de San Juan ", "Cines Axion Playa de San Juan ", "E0884"),
    ("Cines Axion de Orihuela", "Cines Axion de Orihuela", "E0552"),
    ("Cines Axion de Santa Pola", "Cines Axion de Santa Pola", "E0752"),
    ("Cines Colci ", "Cines Colci ", "E0422"),
    ("Cines Colci Rincón", "Cines Colci Rincón", "E0423"),
    ("Cines Costa", "Cines Costa", "E0951"),
    ("Cines Odeón", "Cines Odeón", "E0853"),
    ("Cines Panoramis", "Cines Panoramis", "E0397"),
    ("CinesMax 3D Petrer", "CinesMax 3D Petrer", "E0728"),
    ("Colci Suyma", "Colci Suyma", "E0957"),
    ("Kinépolis Alicante", "Kinépolis Alicante", "E0819"),
    ("Multicines El Altet", "Multicines El Altet", "E0721"),
    ("Odeon Multicines Alicante", "Odeon Multicines Alicante", "E0213"),
    ("Terraza Imperial - Cine de Verano", "Terraza Imperial - Cine de Verano", "E0950"),
    ("Yelmo Cines Puerta De Alicante", "Yelmo Cines Puerta De Alicante", "E0631"),
    ("Yelmo Cines Vinalopo", "Yelmo Cines Vinalopo", "E0636")
  ))
  private def p_almeria: R = ("almeria", "Almería", "Andalucía", 36.83814, -2.45974, "Europe/Madrid", Seq(
    ("Cine Albox", "Cine Albox", "E0865"),
    ("Cine Berja", "Cine Berja", "E0965"),
    ("Cine Tenis", "Cine Tenis", "E0975"),
    ("Cine Terraza de Verano de Vera", "Cine Terraza de Verano de Vera", "E0911"),
    ("Cine de verano Aguadulce", "Cine de verano Aguadulce", "E0943"),
    ("Kinépolis Almería Mediterráneo", "Kinépolis Almería Mediterráneo", "E0359"),
    ("Yelmo Cines Roquetas", "Yelmo Cines Roquetas", "E0620"),
    ("Yelmo Cines Torrecárdenas", "Yelmo Cines Torrecárdenas", "E0909")
  ))
  private def p_asturias: R = ("asturias", "Asturias", "Asturias", 43.36029, -5.84476, "Europe/Madrid", Seq(
    ("Autocine Gijón", "Autocine Gijón", "E0784"),
    ("Cine Fantasio Navia", "Cine Fantasio Navia", "E1037"),
    ("Cinebox Parque Astur", "Cinebox Parque Astur", "E0290"),
    ("Cinesa Parque Principado", "Cinesa Parque Principado", "E0398"),
    ("Odeon Multicines Parque Astur", "Odeon Multicines Parque Astur", "E0814"),
    ("Yelmo Cines Los Prados", "Yelmo Cines Los Prados", "E0623")
  ))
  private def p_badajoz: R = ("badajoz", "Badajoz", "Extremadura", 38.87789, -6.97061, "Europe/Madrid", Seq(
    ("Cine La Fábrica", "Cine La Fábrica", "E1022"),
    ("Cine Municipal Herrera del Duque", "Cine Municipal Herrera del Duque", "E1027"),
    ("Cines Victoria Almendralejo", "Cines Victoria Almendralejo", "E0719"),
    ("Cines Victoria Don Benito", "Cines Victoria Don Benito", "E0372"),
    ("Cines Victoria Mérida", "Cines Victoria Mérida", "E0383"),
    ("Multicines España", "Multicines España", "E0508"),
    ("Yelmo Cines Premium El Faro", "Yelmo Cines Premium El Faro", "E1038"),
    ("mk2 Conquistadores", "mk2 Conquistadores", "E0408")
  ))
  private def p_barcelona: R = ("barcelona", "Barcelona", "Cataluña", 41.38879, 2.15899, "Europe/Madrid", Seq(
    ("Arenas Multicines 3D", "Arenas Multicines 3D", "E0764"),
    ("Aribau Multicines", "Aribau Multicines", "E0091"),
    ("Ateneu Cinema ", "Ateneu Cinema ", "E0906"),
    ("Balmes Multicines", "Balmes Multicines", "E0808"),
    ("Bosque Multicines", "Bosque Multicines", "E0136"),
    ("Cine Alhambra", "Cine Alhambra", "E0827"),
    ("Cine Capri", "Cine Capri", "E0230"),
    ("Cine Kubrick", "Cine Kubrick", "E0757"),
    ("Cine Vigatà", "Cine Vigatà", "E0612"),
    ("Cine la Calandria", "Cine la Calandria", "E0836"),
    ("Cinebaix", "Cinebaix", "E0276"),
    ("Cineclub Vilafranca - Sala Zazie-Casa", "Cineclub Vilafranca - Sala Zazie-Casa", "E0692"),
    ("Cinema Catalunya", "Cinema Catalunya", "E0304"),
    ("Cinema Edison", "Cinema Edison", "G02RB"),
    ("Cinema Esbarjo", "Cinema Esbarjo", "E0889"),
    ("Cinema Prado", "Cinema Prado", "E0311"),
    ("Cinema Retiro", "Cinema Retiro", "E0312"),
    ("Cinema Ribes", "Cinema Ribes", "E0907"),
    ("Cinema Sala Mozart", "Cinema Sala Mozart", "E0314"),
    ("Cinema Teatre Patronat", "Cinema Teatre Patronat", "E0858"),
    ("Cinemes Can Castellet", "Cinemes Can Castellet", "E0748"),
    ("Cinemes Sant Cugat", "Cinemes Sant Cugat", "E0403"),
    ("Cines Imperial", "Cines Imperial", "E0351"),
    ("Cines Montcada", "Cines Montcada", "E0655"),
    ("Cinesa Barnasud", "Cinesa Barnasud", "E0661"),
    ("Cinesa Diagonal", "Cinesa Diagonal", "E0381"),
    ("Cinesa Diagonal Mar", "Cinesa Diagonal Mar", "E0382"),
    ("Cinesa La Farga", "Cinesa La Farga", "E0391"),
    ("Cinesa Llobregat Centre", "Cinesa Llobregat Centre", "E0857"),
    ("Cinesa Parc Vallès", "Cinesa Parc Vallès", "E0374"),
    ("Cinesa SOM Multiespai", "Cinesa SOM Multiespai", "E0388"),
    ("Club Catalunya", "Club Catalunya", "E0420"),
    ("Espai L'Amistat", "Espai L'Amistat", "E0919"),
    ("Filmax Gran Via 3D", "Filmax Gran Via 3D", "E0439"),
    ("Glòries Multicines", "Glòries Multicines", "E0442"),
    ("Gran Sarrià Multicines", "Gran Sarrià Multicines", "E0447"),
    ("Kinépolis Barcelona Full Splau", "Kinépolis Barcelona Full Splau", "E0756"),
    ("Kinépolis Mataró Parc", "Kinépolis Mataró Parc", "E0396"),
    ("Mont-Àgora Cinemes", "Mont-Àgora Cinemes", "E1006"),
    ("Multicinemes La Vailet", "Multicinemes La Vailet", "E0714"),
    ("Multicines Bages 3D", "Multicines Bages 3D", "E0120"),
    ("Multicines Catalunya", "Multicines Catalunya", "E0495"),
    ("Multicines Eix Macià", "Multicines Eix Macià", "E0504"),
    ("Multicines Sucre", "Multicines Sucre", "E0535"),
    ("Ocine Arenys", "Ocine Arenys", "E0651"),
    ("Ocine Granollers", "Ocine Granollers", "E0507"),
    ("Ocine Màgic", "Ocine Màgic", "E0713"),
    ("Ocine Sant Celoni Altrium", "Ocine Sant Celoni Altrium", "E0745"),
    ("Odeon Multicines Llobregat", "Odeon Multicines Llobregat", "E0521"),
    ("Odeon Multicines Vilanova", "Odeon Multicines Vilanova", "E2908"),
    ("Renoir Floridablanca", "Renoir Floridablanca", "E0581"),
    ("Yelmo Cines Abrera", "Yelmo Cines Abrera", "E0735"),
    ("Yelmo Cines Baricentro", "Yelmo Cines Baricentro", "E0615"),
    ("Yelmo Cines Premium Castelldefels", "Yelmo Cines Premium Castelldefels", "E0806"),
    ("Yelmo Cines Premium Sant Cugat", "Yelmo Cines Premium Sant Cugat", "E0633")
  ))
  private def p_burgos: R = ("burgos", "Burgos", "Castilla y León", 42.34106, -3.70184, "Europe/Madrid", Seq(
    ("Cine Novedades Miranda de Ebro", "Cine Novedades Miranda de Ebro", "E0647"),
    ("Cines Van Golem Arlanzón", "Cines Van Golem Arlanzón", "E0370"),
    ("Cines Victoria Ribera de Duero", "Cines Victoria Ribera de Duero", "E0777"),
    ("Odeon Multicines Burgos", "Odeon Multicines Burgos", "E0279")
  ))
  private def p_cantabria: R = ("cantabria", "Cantabria", "Cantabria", 43.46589, -3.80493, "Europe/Madrid", Seq(
    ("Casa de Cultura Doctor Velasco ", "Casa de Cultura Doctor Velasco ", "E0917"),
    ("Cine La Vidriera", "Cine La Vidriera", "E0257"),
    ("Cine Los Ángeles", "Cine Los Ángeles", "E0688"),
    ("Cine Playa Dorada", "Cine Playa Dorada", "E0567"),
    ("Cines Embajadores Santander", "Cines Embajadores Santander", "E0349"),
    ("Cinesa Bahía de Santander", "Cinesa Bahía de Santander", "E0123"),
    ("Filmoteca de Cantabria - Santander", "Filmoteca de Cantabria - Santander", "E0979"),
    ("Ocine Premium Bahía Real", "Ocine Premium Bahía Real", "E1045"),
    ("Palacios De Festivales", "Palacios De Festivales", "E0560"),
    ("Sala Bretón", "Sala Bretón", "E0594"),
    ("Teatro Casino Liceo de Santoña", "Teatro Casino Liceo de Santoña", "E0918"),
    ("Teatro De Los Corrales De Buelna", "Teatro De Los Corrales De Buelna", "E0599"),
    ("Yelmo Cines Premium Peñacastillo", "Yelmo Cines Premium Peñacastillo", "E0565")
  ))
  private def p_castellon: R = ("castellon", "Castellón", "Comunidad Valenciana", 39.98567, -0.04935, "Europe/Madrid", Seq(
    ("Cine Terraza Avenida", "Cine Terraza Avenida", "E0941"),
    ("Cines Axion Benicarló", "Cines Axion Benicarló", "E0493"),
    ("Cines Sucre", "Cines Sucre", "E0366"),
    ("Cinesa Salera", "Cinesa Salera", "E0654"),
    ("JJ Cinema ", "JJ Cinema ", "E0892"),
    ("Neocine Puerto Azahar", "Neocine Puerto Azahar", "E0571"),
    ("Ocine Premium Estepark", "Ocine Premium Estepark", "E0925"),
    ("Teatre Municipal Carmen Tur - Antic Cine España", "Teatre Municipal Carmen Tur - Antic Cine España", "E0791"),
    ("Terraza Neptuno", "Terraza Neptuno", "E0926")
  ))
  private def p_ceuta: R = ("ceuta", "Ceuta", "Ceuta", 35.88919, -5.32042, "Europe/Madrid", Seq(
    ("Marina Cinemas 7", "Marina Cinemas 7", "E0478")
  ))
  private def p_ciudad_real: R = ("ciudad-real", "Ciudad Real", "Castilla-La Mancha", 38.98626, -3.92907, "Europe/Madrid", Seq(
    ("Cine Reina Sofía Socuéllamos", "Cine Reina Sofía Socuéllamos", "E1024"),
    ("Cine Teatro Municipal Pedro Muñoz", "Cine Teatro Municipal Pedro Muñoz", "E1023"),
    ("Daimiel Cinema", "Daimiel Cinema", "E0990"),
    ("La Dehesa Tomelloso", "La Dehesa Tomelloso", "E0659"),
    ("Multicines Cinemancha", "Multicines Cinemancha", "E0496"),
    ("Multicines Ortega", "Multicines Ortega", "E0526"),
    ("Multicines Valdepeñas", "Multicines Valdepeñas", "E0701"),
    ("Parque De Ocio Las Vías", "Parque De Ocio Las Vías", "E0562")
  ))
  private def p_cuenca: R = ("cuenca", "Cuenca", "Castilla-La Mancha", 40.06667, -2.13333, "Europe/Madrid", Seq(
    ("Abaco Cuenca", "Abaco Cuenca", "E0020"),
    ("Cine Iniesta", "Cine Iniesta", "E1031"),
    ("Cine de Verano Tarancón", "Cine de Verano Tarancón", "E0914"),
    ("Odeon Multicines Cuenca", "Odeon Multicines Cuenca", "E0502"),
    ("Odeon Multicines Mirador", "Odeon Multicines Mirador", "E0852")
  ))
  private def p_caceres: R = ("caceres", "Cáceres", "Extremadura", 39.47649, -6.37224, "Europe/Madrid", Seq(
    ("Cine Arroyo de la Luz", "Cine Arroyo de la Luz", "E0772"),
    ("Cine Avenida Jaraíz", "Cine Avenida Jaraíz", "E0867"),
    ("Cine Coria", "Cine Coria", "E0832"),
    ("Cines Navalmoral", "Cines Navalmoral", "E0886"),
    ("Multicines Alkázar", "Multicines Alkázar", "E0067"),
    ("Multicines Cáceres", "Multicines Cáceres", "E0143")
  ))
  private def p_cadiz: R = ("cadiz", "Cádiz", "Andalucía", 36.52672, -6.2891, "Europe/Madrid", Seq(
    ("Al-Andalus Cádiz", "Al-Andalus Cádiz", "E0902"),
    ("Al-Andalus Sanlucar", "Al-Andalus Sanlucar", "E0218"),
    ("Arcos Cinema", "Arcos Cinema", "E0905"),
    ("Cine Alba Chipiona", "Cine Alba Chipiona", "E0912"),
    ("Cine de Verano La Muralla", "Cine de Verano La Muralla", "E0999"),
    ("Cines Plaza San Fernando", "Cines Plaza San Fernando", "E0212"),
    ("Cines Victoria Rota", "Cines Victoria Rota", "E0766"),
    ("Multicines Bahia Mar", "Multicines Bahia Mar", "E0491"),
    ("Multicines Jerez UCC", "Multicines Jerez UCC", "E1044"),
    ("Multicines Las Salinas", "Multicines Las Salinas", "E0520"),
    ("Multicines el Centro", "Multicines el Centro", "E0171"),
    ("Odeon Bahía Plaza", "Odeon Bahía Plaza", "E0245"),
    ("Portalejo Cinemas", "Portalejo Cinemas", "E0568"),
    ("Teatro Maestro Francisco Fatou", "Teatro Maestro Francisco Fatou", "E1019"),
    ("Teatro San Francisco", "Teatro San Francisco", "E1018"),
    ("Yelmo Cines Premium Bahía Sur", "Yelmo Cines Premium Bahía Sur", "E1042"),
    ("Yelmo Cines Premium Puerta Europa ", "Yelmo Cines Premium Puerta Europa ", "E0910"),
    ("Yelmo Cines Área Sur", "Yelmo Cines Área Sur", "E0669"),
    ("mk2 Bahía de Cádiz", "mk2 Bahía de Cádiz", "E0332")
  ))
  private def p_cordoba: R = ("cordoba", "Córdoba", "Andalucía", 37.89155, -4.77275, "Europe/Madrid", Seq(
    ("Artesiete Lucena", "Artesiete Lucena", "E0489"),
    ("Centro Cultural de la Villa - Pastora Soler", "Centro Cultural de la Villa - Pastora Soler", "E1016"),
    ("Cine Baena", "Cine Baena", "E0915"),
    ("Cine Delicias", "Cine Delicias", "E0948"),
    ("Cine Mota del Cuervo", "Cine Mota del Cuervo", "E1013"),
    ("Cine Municipal Huércal-Overa", "Cine Municipal Huércal-Overa", "E1012"),
    ("Cine Pósito", "Cine Pósito", "E0859"),
    ("Cinestudio Municipal Cabra", "Cinestudio Municipal Cabra", "E0868"),
    ("Guadalquivir Cinemas 10", "Guadalquivir Cinemas 10", "E0512"),
    ("Peñarroya Cinema", "Peñarroya Cinema", "E1009"),
    ("mk2 El Tablero", "mk2 El Tablero", "E0409")
  ))
  private def p_girona: R = ("girona", "Girona", "Cataluña", 41.98311, 2.82493, "Europe/Madrid", Seq(
    ("Cat Cinemes", "Cat Cinemes", "E0345"),
    ("Cinema Casino", "Cinema Casino", "E0969"),
    ("Cinema Kyton", "Cinema Kyton", "E0456"),
    ("Cinema Montgrí", "Cinema Montgrí", "E0896"),
    ("Cinema Teatre Comtal", "Cinema Teatre Comtal", "E0315"),
    ("Cinema Truffaut", "Cinema Truffaut", "E0316"),
    ("Cinemes Roses", "Cinemes Roses", "E0324"),
    ("Multicines Olot", "Multicines Olot", "E0323"),
    ("Ocine Blanes", "Ocine Blanes", "E0462"),
    ("Ocine Girona", "Ocine Girona", "E0362"),
    ("Ocine Platja d'Aro", "Ocine Platja d'Aro", "E0554"),
    ("Odeon Multicines Girona", "Odeon Multicines Girona", "E0281"),
    ("Teatro Municipal de Palafrugel", "Teatro Municipal de Palafrugel", "E0978")
  ))
  private def p_granada: R = ("granada", "Granada", "Andalucía", 37.18817, -3.60667, "Europe/Madrid", Seq(
    ("Artesiete Alhsur", "Artesiete Alhsur", "E0722"),
    ("Cañaveral Cinema", "Cañaveral Cinema", "E0960"),
    ("Cine Liszt Terraza de verano", "Cine Liszt Terraza de verano", "E0743"),
    ("Cine Madrigal", "Cine Madrigal", "E0689"),
    ("Cine San Cristobal", "Cine San Cristobal", "E0952"),
    ("Huétor Cinema", "Huétor Cinema", "E0981"),
    ("Kinépolis Granada", "Kinépolis Granada", "E0452"),
    ("Kinépolis Nevada", "Kinépolis Nevada", "E0866"),
    ("Megarama Granada", "Megarama Granada", "E0301"),
    ("Motril Cinema", "Motril Cinema", "E0869"),
    ("Ocine Serrallo", "Ocine Serrallo", "E0787"),
    ("Salón Cine Ideal", "Salón Cine Ideal", "E0849"),
    ("Teatro Isabel La Catolica", "Teatro Isabel La Catolica", "E0712")
  ))
  private def p_guadalajara: R = ("guadalajara", "Guadalajara", "Castilla-La Mancha", 40.62862, -3.16185, "Europe/Madrid", Seq(
    ("Cultura Azuqueca", "Cultura Azuqueca", "E0955"),
    ("Multicines Guadalajara", "Multicines Guadalajara", "E0511")
  ))
  private def p_guipuzcoa: R = ("guipuzcoa", "Guipúzcoa", "País Vasco", 43.31283, -1.97499, "Europe/Madrid", Seq(
    ("Aita Mari Zinema", "Aita Mari Zinema", "E0214"),
    ("Baztartxo Antzokia", "Baztartxo Antzokia", "E0227"),
    ("Cine Modelo", "Cine Modelo", "E0263"),
    ("Cine Príncipe", "Cine Príncipe", "E0570"),
    ("Cine Trueba", "Cine Trueba", "E0603"),
    ("Cines Antiguo Berri", "Cines Antiguo Berri", "E0329"),
    ("Cinesa Urbil", "Cinesa Urbil", "E0296"),
    ("Herri Antzokia ", "Herri Antzokia ", "E0890"),
    ("Leidor Zinema", "Leidor Zinema", "E0472"),
    ("Multicines Niessen Zinemak", "Multicines Niessen Zinemak", "E0637"),
    ("Ocine Mendibil", "Ocine Mendibil", "E0537"),
    ("Oñatiko Zinea", "Oñatiko Zinea", "E0551"),
    ("Teatro Coliseo", "Teatro Coliseo", "E0769"),
    ("Usurbe Antzokia", "Usurbe Antzokia", "E0605")
  ))
  private def p_huelva: R = ("huelva", "Huelva", "Andalucía", 37.26638, -6.94004, "Europe/Madrid", Seq(
    ("Al-Andalus Punta Umbría 3D", "Al-Andalus Punta Umbría 3D", "E0641"),
    ("Artesiete Holea", "Artesiete Holea", "E0805"),
    ("Cine 3D Ayamonte", "Cine 3D Ayamonte", "E0788"),
    ("Cine Alba Mazagón", "Cine Alba Mazagón", "E0995"),
    ("Cine Vip 3d Lepe", "Cine Vip 3d Lepe", "E0765"),
    ("Cines Aqualón", "Cines Aqualón", "E0278"),
    ("Condado Cinemas 7", "Condado Cinemas 7", "E0429"),
    ("Cortegana Cinema", "Cortegana Cinema", "E2913"),
    ("La Dehesa Ayamonte", "La Dehesa Ayamonte", "E0945"),
    ("Multicines La Dehesa - Islantilla", "Multicines La Dehesa - Islantilla", "E0516")
  ))
  private def p_huesca: R = ("huesca", "Huesca", "Aragón", 42.13615, -0.4087, "Europe/Madrid", Seq(
    ("Auditorio La Colina", "Auditorio La Colina", "E0643"),
    ("Cine Cortés", "Cine Cortés", "E0250"),
    ("Cine La Paz", "Cine La Paz", "E0256"),
    ("Cine Teatro Victoria", "Cine Teatro Victoria", "E0271"),
    ("CineMundo Huesca", "CineMundo Huesca", "E0497"),
    ("Palacio De Congresos Boltaña", "Palacio De Congresos Boltaña", "E0667"),
    ("Teatro Municipal Los Titiriteros", "Teatro Municipal Los Titiriteros", "E0964")
  ))
  private def p_islas_baleares: R = ("islas-baleares", "Islas Baleares", "Islas Baleares", 39.56939, 2.65024, "Europe/Madrid", Seq(
    ("Artesiete Fan", "Artesiete Fan", "E0863"),
    ("Cine Regio", "Cine Regio", "E0839"),
    ("CineCiutat", "CineCiutat", "E0365"),
    ("Cinema Ca-Los", "Cinema Ca-Los", "E0229"),
    ("Cinemes Moix Negre", "Cinemes Moix Negre", "E0782"),
    ("Cines Ocimax", "Cines Ocimax", "E0360"),
    ("Cinesa Festival Park", "Cinesa Festival Park", "E0386"),
    ("Multicines Eivissa", "Multicines Eivissa", "E0503"),
    ("Multicines Manacor", "Multicines Manacor", "E0522"),
    ("Multicines Rivoli", "Multicines Rivoli", "E0533"),
    ("Ocimax Multisalas", "Ocimax Multisalas", "E0639"),
    ("Sala Augusta", "Sala Augusta", "E0593"),
    ("Teatro España", "Teatro España", "E0755")
  ))
  private def p_jaen: R = ("jaen", "Jaén", "Andalucía", 37.76922, -3.79028, "Europe/Madrid", Seq(
    ("Autocinema Tenerife", "Autocinema Tenerife", "E2912"),
    ("Cine Teatro Martínez Montañés", "Cine Teatro Martínez Montañés", "E1025"),
    ("Europa Pantallas 8", "Europa Pantallas 8", "E0436"),
    ("Multicines Bowling", "Multicines Bowling", "E0239"),
    ("Multicines Carolina", "Multicines Carolina", "E0823"),
    ("Multicines Úbeda", "Multicines Úbeda", "E0538"),
    ("París Multicines", "París Multicines", "E0822"),
    ("Teatro Maestro Álvarez Alonso", "Teatro Maestro Álvarez Alonso", "E1017")
  ))
  private def p_la_rioja: R = ("la-rioja", "La Rioja", "La Rioja", 42.46615, -2.45115, "Europe/Madrid", Seq(
    ("Cine Avenida Rincón de Soto", "Cine Avenida Rincón de Soto", "E0829"),
    ("Cine Avenida Santo Domingo", "Cine Avenida Santo Domingo", "E0830"),
    ("Cines 7 Infantes", "Cines 7 Infantes", "E0804"),
    ("Cines Arcca", "Cines Arcca", "E0801"),
    ("Cines Moderno", "Cines Moderno", "E0358"),
    ("Teatro Bretón", "Teatro Bretón", "E0986"),
    ("Yelmo Cines Premium Berceo", "Yelmo Cines Premium Berceo", "E0733")
  ))
  private def p_las_palmas: R = ("las-palmas", "Las Palmas", "Canarias", 28.10178, -15.41573, "Atlantic/Canary", Seq(
    ("Artesiete Las Terrazas", "Artesiete Las Terrazas", "E0723"),
    ("Deiland Multicines", "Deiland Multicines", "E0785"),
    ("Multicine Atlántida", "Multicine Atlántida", "E0484"),
    ("Multicines Deiland", "Multicines Deiland", "E0485"),
    ("Odeón Puerto del Rosario", "Odeón Puerto del Rosario", "E0898"),
    ("Yelmo Cines Fuerteventura", "Yelmo Cines Fuerteventura", "E0618"),
    ("Yelmo Cines Las Arenas", "Yelmo Cines Las Arenas", "E0754"),
    ("Yelmo Cines Premium Alisios", "Yelmo Cines Premium Alisios", "E0972"),
    ("Yelmo Cines Vecindario", "Yelmo Cines Vecindario", "E0634")
  ))
  private def p_leon: R = ("leon", "León", "Castilla y León", 42.60003, -5.57032, "Europe/Madrid", Seq(
    ("Cine Marí", "Cine Marí", "E0901"),
    ("Cine Paramés", "Cine Paramés", "E0854"),
    ("Cine Velasco", "Cine Velasco", "E0274"),
    ("Cines Van Gogh", "Cines Van Gogh", "E0369"),
    ("El cine Villablino", "El cine Villablino", "E0888"),
    ("La Dehesa Ponferrada", "La Dehesa Ponferrada", "E0458"),
    ("Odeon Multicines León", "Odeon Multicines León", "E0280")
  ))
  private def p_lugo: R = ("lugo", "Lugo", "Galicia", 43.00992, -7.55602, "Europe/Madrid", Seq(
    ("Cine Ribadeo", "Cine Ribadeo", "E0740"),
    ("Cines Viveiro 3D", "Cines Viveiro 3D", "E0846"),
    ("Multicines Cristal", "Multicines Cristal", "E0501"),
    ("Multicines Hollywood", "Multicines Hollywood", "E0513"),
    ("Yelmo Cines As Termas", "Yelmo Cines As Termas", "E0684")
  ))
  private def p_lerida: R = ("lerida", "Lérida", "Cataluña", 41.61674, 0.62218, "Europe/Madrid", Seq(
    ("Autocine Resquitx - Golmés", "Autocine Resquitx - Golmés", "E1033"),
    ("Cinema Armengol", "Cinema Armengol", "E0302"),
    ("Cinema Casal Agramunt", "Cinema Casal Agramunt", "E0303"),
    ("Cinema El Casal", "Cinema El Casal", "E0650"),
    ("Cinema Era Audiovisuau", "Cinema Era Audiovisuau", "E0232"),
    ("Cinema La Lira", "Cinema La Lira", "E0305"),
    ("Cinema Mollerussa", "Cinema Mollerussa", "E0973"),
    ("Cinema Paris", "Cinema Paris", "E0309"),
    ("Cinema Planell", "Cinema Planell", "E0310"),
    ("Cinema Ribagorza", "Cinema Ribagorza", "E0313"),
    ("Cinemes Guiu", "Cinemes Guiu", "E0319"),
    ("Cinemes Majestic", "Cinemes Majestic", "E0799"),
    ("Cinemes Urgell", "Cinemes Urgell", "E0325"),
    ("Jca Cinemes Alpicat", "Jca Cinemes Alpicat", "E0652"),
    ("Sala d''actes Ajuntament", "Sala d''actes Ajuntament", "E0985")
  ))
  private def p_madrid: R = ("madrid", "Madrid", "Comunidad de Madrid", 40.4165, -3.70256, "Europe/Madrid", Seq(
    ("Casa de Cultura Guadarrama", "Casa de Cultura Guadarrama", "E0936"),
    ("Centro de Arte y Cine de Verano Soto del Real", "Centro de Arte y Cine de Verano Soto del Real", "E0937"),
    ("Cine Aranjuez", "Cine Aranjuez", "E0233"),
    ("Cine Colíseo de la Cultura", "Cine Colíseo de la Cultura", "E0707"),
    ("Cine Giralt Laporta", "Cine Giralt Laporta", "E0753"),
    ("Cine Los Molinos", "Cine Los Molinos", "E0729"),
    ("Cine Teatro Municipal", "Cine Teatro Municipal", "E0419"),
    ("Cine de Verano El Molino", "Cine de Verano El Molino", "E0674"),
    ("Cine de Verano Juan Falco", "Cine de Verano Juan Falco", "E0750"),
    ("Cine de Verano Valdemoro", "Cine de Verano Valdemoro", "E0675"),
    ("Cine de verano el castillo", "Cine de verano el castillo", "E0672"),
    ("Cinebox 3 C", "Cinebox 3 C", "E0199"),
    ("Cines Boadilla", "Cines Boadilla", "E0760"),
    ("Cines Dos de Mayo", "Cines Dos de Mayo", "E0761"),
    ("Cines La Rambla", "Cines La Rambla", "E0353"),
    ("Cines Plaza Coslada", "Cines Plaza Coslada", "E2910"),
    ("Cines Princesa", "Cines Princesa", "E0364"),
    ("Cines Villa", "Cines Villa", "E0190"),
    ("Cines Zoco Majadahonda", "Cines Zoco Majadahonda", "E0582"),
    ("Cinesa Equinoccio", "Cinesa Equinoccio", "E0385"),
    ("Cinesa Heron City Las Rozas", "Cinesa Heron City Las Rozas", "E0389"),
    ("Cinesa Intu Xanadú", "Cinesa Intu Xanadú", "E0406"),
    ("Cinesa La Gavia", "Cinesa La Gavia", "E0731"),
    ("Cinesa La Moraleja", "Cinesa La Moraleja", "E0392"),
    ("Cinesa Manoteras", "Cinesa Manoteras", "E0646"),
    ("Cinesa Méndez Álvaro", "Cinesa Méndez Álvaro", "E0247"),
    ("Cinesa Nassica", "Cinesa Nassica", "E0246"),
    ("Cinesa Parquesur", "Cinesa Parquesur", "E0399"),
    ("Cinesa Plaza Loranca 2", "Cinesa Plaza Loranca 2", "E0394"),
    ("Cinesa Príncipe Pío", "Cinesa Príncipe Pío", "E0401"),
    ("Kinépolis Madrid", "Kinépolis Madrid", "E0453"),
    ("Kinépolis Madrid Diversia", "Kinépolis Madrid Diversia", "E0209"),
    ("Multicines Cisneros", "Multicines Cisneros", "E0498"),
    ("Ocine Plaza Éboli", "Ocine Plaza Éboli", "E2900"),
    ("Ocine Urban X-Madrid", "Ocine Urban X-Madrid", "E1004"),
    ("Odeon Multicines Sambil Dolby Atmos", "Odeon Multicines Sambil Dolby Atmos", "E0877"),
    ("Odeon Multicines Tres Cantos", "Odeon Multicines Tres Cantos", "E0815"),
    ("Restón Cinema", "Restón Cinema", "E0584"),
    ("Sala Babel", "Sala Babel", "E0818"),
    ("Spazio Cines", "Spazio Cines", "E0935"),
    ("Teatro Fernández-Baldor", "Teatro Fernández-Baldor", "E1000"),
    ("Yelmo Cines Ideal", "Yelmo Cines Ideal", "E0621"),
    ("Yelmo Cines Islazul", "Yelmo Cines Islazul", "E0681"),
    ("Yelmo Cines La Vaguada", "Yelmo Cines La Vaguada", "E0459"),
    ("Yelmo Cines Planetocio", "Yelmo Cines Planetocio", "E0630"),
    ("Yelmo Cines Plaza Norte 2", "Yelmo Cines Plaza Norte 2", "E2916"),
    ("Yelmo Cines Plenilunio", "Yelmo Cines Plenilunio", "E0475"),
    ("Yelmo Cines Premium Parque Corredor", "Yelmo Cines Premium Parque Corredor", "E0291"),
    ("Yelmo Cines Rivas H2O", "Yelmo Cines Rivas H2O", "E0671"),
    ("Yelmo Cines TresAguas", "Yelmo Cines TresAguas", "E0207"),
    ("mk2 Palacio de Hielo (antiguos Cines Dreams)", "mk2 Palacio de Hielo (antiguos Cines Dreams)", "E0432")
  ))
  private def p_melilla: R = ("melilla", "Melilla", "Melilla", 35.29369, -2.93833, "Europe/Madrid", Seq(
    ("Cine Teatro Perelló", "Cine Teatro Perelló", "E0841")
  ))
  private def p_murcia: R = ("murcia", "Murcia", "Región de Murcia", 37.98704, -1.13004, "Europe/Madrid", Seq(
    ("Cine Acapulco", "Cine Acapulco", "E0942"),
    ("Cine Bahía", "Cine Bahía", "E0977"),
    ("Cine La Manga", "Cine La Manga", "E0946"),
    ("Cine Pya", "Cine Pya", "E0838"),
    ("Cine Sirenas", "Cine Sirenas", "E0947"),
    ("Cine de Verano Abarán", "Cine de Verano Abarán", "E0938"),
    ("Cine de Verano de Archena", "Cine de Verano de Archena", "E0962"),
    ("Cinema Velasco Totana", "Cinema Velasco Totana", "E0921"),
    ("Cines Almenara Lorca", "Cines Almenara Lorca", "E0751"),
    ("Cines IMF Galán", "Cines IMF Galán", "E0956"),
    ("Cinesa Nueva Condomina", "Cinesa Nueva Condomina", "E0656"),
    ("Multicines El Hornillo", "Multicines El Hornillo", "E0506"),
    ("NeoCine Espacio Mediterraneo", "NeoCine Espacio Mediterraneo", "E0663"),
    ("Neocine Centrofama", "Neocine Centrofama", "E0193"),
    ("Neocine Dos Mares", "Neocine Dos Mares", "E0431"),
    ("Neocine El Tiro", "Neocine El Tiro", "E0774"),
    ("Neocine HD Digital Vega Plaza", "Neocine HD Digital Vega Plaza", "E0660"),
    ("Neocine Mandarache", "Neocine Mandarache", "E0546"),
    ("Neocine Rex", "Neocine Rex", "E0690"),
    ("Neocine Thader", "Neocine Thader", "E0547"),
    ("Nuevos Cines Cabos de Palos ", "Nuevos Cines Cabos de Palos ", "E0953"),
    ("Terraza Auditorio Parque Municipal", "Terraza Auditorio Parque Municipal", "E0922"),
    ("Terraza Centro Joven", "Terraza Centro Joven", "E0988"),
    ("Terraza España", "Terraza España", "E0949")
  ))
  private def p_malaga: R = ("malaga", "Málaga", "Andalucía", 36.72016, -4.42034, "Europe/Madrid", Seq(
    ("Alameda Multicines Malaga", "Alameda Multicines Malaga", "E0048"),
    ("Centro Cultural Villa de Nerja", "Centro Cultural Villa de Nerja", "E0976"),
    ("Cine Albéniz", "Cine Albéniz", "E0195"),
    ("Cine Pixel", "Cine Pixel", "E0215"),
    ("Cine San Francisco", "Cine San Francisco", "E1005"),
    ("Cines Gran Marbella", "Cines Gran Marbella", "E0427"),
    ("Cines La Verónica", "Cines La Verónica", "E0410"),
    ("Kinépolis La Cañada", "Kinépolis La Cañada", "E0390"),
    ("Multicines Alfil 3D", "Multicines Alfil 3D", "E0059"),
    ("Multicines Ronda", "Multicines Ronda", "E0534"),
    ("Multicines Rosaleda", "Multicines Rosaleda", "E0589"),
    ("Red Dog Cinemas", "Red Dog Cinemas", "E0845"),
    ("Yelmo Cines Rincón De La Victoria", "Yelmo Cines Rincón De La Victoria", "E0632"),
    ("mk2 El Ingenio", "mk2 El Ingenio", "E0433"),
    ("mk2 Malaga Nostrum", "mk2 Malaga Nostrum", "E0413"),
    ("mk2 Miramar", "mk2 Miramar", "E0414")
  ))
  private def p_navarra: R = ("navarra", "Navarra", "Navarra", 42.81687, -1.64323, "Europe/Madrid", Seq(
    ("Cines Las Cañas Viana", "Cines Las Cañas Viana", "E0371"),
    ("Cines Los Llanos Zinemak", "Cines Los Llanos Zinemak", "E0259"),
    ("Golem Baiona", "Golem Baiona", "E0444"),
    ("Golem La Morea", "Golem La Morea", "E0443"),
    ("Golem Yamaguchi", "Golem Yamaguchi", "E0445"),
    ("Ocine Tudela", "Ocine Tudela", "E0317"),
    ("Yelmo Cines Itaroa", "Yelmo Cines Itaroa", "E0283")
  ))
  private def p_ourense: R = ("ourense", "Ourense", "Galicia", 42.33669, -7.86407, "Europe/Madrid", Seq(
    ("Cine Gesma", "Cine Gesma", "E0834"),
    ("Cinebox Ourense", "Cinebox Ourense", "E0289"),
    ("Multicines Ponte Vella", "Multicines Ponte Vella", "E0813"),
    ("NovoCine Leiro 3D", "NovoCine Leiro 3D", "E0847")
  ))
  private def p_palencia: R = ("palencia", "Palencia", "Castilla y León", 42.00955, -4.52406, "Europe/Madrid", Seq(
    ("Cine AMGu", "Cine AMGu", "E0998"),
    ("Cines Campoo", "Cines Campoo", "E0334"),
    ("Cines Campoo 3D", "Cines Campoo 3D", "E1003"),
    ("Cines Ortega", "Cines Ortega", "E0361"),
    ("Multicines Avenida", "Multicines Avenida", "E0331")
  ))
  private def p_pontevedra: R = ("pontevedra", "Pontevedra", "Galicia", 42.431, -8.64435, "Europe/Madrid", Seq(
    ("Cine Club Pontevedra", "Cine Club Pontevedra", "E0875"),
    ("Cine Imperial", "Cine Imperial", "E0835"),
    ("Cine Seixo", "Cine Seixo", "E0899"),
    ("Cines Avenida 3D", "Cines Avenida 3D", "E0742"),
    ("Cines Tamberlick Plaza Elíptica", "Cines Tamberlick Plaza Elíptica", "E0739"),
    ("Minicines Central ", "Minicines Central ", "E0895"),
    ("Multicines Cinexpo", "Multicines Cinexpo", "E0298"),
    ("Multicines Gran Arousa", "Multicines Gran Arousa", "E0510"),
    ("Multicines Norte", "Multicines Norte", "E0525"),
    ("Teatro Salesianos", "Teatro Salesianos", "E0602"),
    ("Yelmo Cines Premium Vialia Vigo", "Yelmo Cines Premium Vialia Vigo", "E2902"),
    ("Yelmo Cines Travesía Vigo", "Yelmo Cines Travesía Vigo", "E0635")
  ))
  private def p_salamanca: R = ("salamanca", "Salamanca", "Castilla y León", 40.42972, -3.67975, "Europe/Madrid", Seq(
    ("Cine Calderón", "Cine Calderón", "E0793"),
    ("Cine Juventud", "Cine Juventud", "E0800"),
    ("Cines Van Dyck", "Cines Van Dyck", "E0606"),
    ("Cines Van Dyck Tormes", "Cines Van Dyck Tormes", "E0368"),
    ("Megarama Salamanca", "Megarama Salamanca", "E0299"),
    ("Multicines Béjar", "Multicines Béjar", "E0492")
  ))
  private def p_santa_cruz_de_tenerife: R = ("santa-cruz-de-tenerife", "Santa Cruz de Tenerife", "Canarias", 28.46824, -16.25462, "Atlantic/Canary", Seq(
    ("Cine Realejos", "Cine Realejos", "E0267"),
    ("Cines Price Prime", "Cines Price Prime", "E0583"),
    ("Multicines Millennium", "Multicines Millennium", "E0940"),
    ("Multicines Puntalarga", "Multicines Puntalarga", "E0532"),
    ("Multicines Tenerife", "Multicines Tenerife", "E0284"),
    ("Multicines Zentral Center", "Multicines Zentral Center", "E0541"),
    ("Teatro Chico", "Teatro Chico", "E0900"),
    ("X-Sur Cine", "X-Sur Cine", "E0700"),
    ("Yelmo Cines La Villa de Orotava", "Yelmo Cines La Villa de Orotava", "E0622"),
    ("Yelmo Cines Meridiano", "Yelmo Cines Meridiano", "E0627")
  ))
  private def p_segovia: R = ("segovia", "Segovia", "Castilla y León", 40.94808, -4.11839, "Europe/Madrid", Seq(
    ("Artesiete Segovia", "Artesiete Segovia", "E0716"),
    ("Cines Luz de Castilla", "Cines Luz de Castilla", "E0285")
  ))
  private def p_sevilla: R = ("sevilla", "Sevilla", "Andalucía", 37.38283, -5.97317, "Europe/Madrid", Seq(
    ("Al-Andalus Mega Ocio", "Al-Andalus Mega Ocio", "E0217"),
    ("Artesiete Écija", "Artesiete Écija", "E0720"),
    ("Avenida 5 Cines", "Avenida 5 Cines", "E0112"),
    ("Cine Méliès Estepa", "Cine Méliès Estepa", "E0996"),
    ("Cine Planelles", "Cine Planelles", "E0724"),
    ("Cineapolis Dos Hermanas 3D", "Cineapolis Dos Hermanas 3D", "E0191"),
    ("Cineapolis Utrera", "Cineapolis Utrera", "E1039"),
    ("Cineapolis WAY", "Cineapolis WAY", "E1040"),
    ("Cinema Tomares", "Cinema Tomares", "E0676"),
    ("Cinesa Camas", "Cinesa Camas", "E0027"),
    ("Los Arcos Multicines", "Los Arcos Multicines", "E0222"),
    ("Metromar Cinemas 12", "Metromar Cinemas 12", "E0666"),
    ("Odeon Multicines Plaza de Armas", "Odeon Multicines Plaza de Armas", "E0400"),
    ("Teatro Municipal Juan Bernabé", "Teatro Municipal Juan Bernabé", "E0984"),
    ("Teatro Municipal Las Cabezas de San Juan", "Teatro Municipal Las Cabezas de San Juan", "E1020"),
    ("Yelmo Cines Premium Lagoh", "Yelmo Cines Premium Lagoh", "E1002"),
    ("Zona Este", "Zona Este", "E0476"),
    ("mk2 Alcores", "mk2 Alcores", "E0411"),
    ("mk2 Nervión Plaza", "mk2 Nervión Plaza", "E0415")
  ))
  private def p_soria: R = ("soria", "Soria", "Castilla y León", 41.76401, -2.46883, "Europe/Madrid", Seq(
    ("Cine Calderón - Almazán", "Cine Calderón - Almazán", "E0989"),
    ("Cine Palafox Burgo de Osma", "Cine Palafox Burgo de Osma", "E0265"),
    ("Cines Lara", "Cines Lara", "E0356")
  ))
  private def p_tarragona: R = ("tarragona", "Tarragona", "Cataluña", 41.11905, 1.24544, "Europe/Madrid", Seq(
    ("Cinema Casal Montblanquí", "Cinema Casal Montblanquí", "E0161"),
    ("Cinemes Amposta", "Cinemes Amposta", "E0076"),
    ("Cines Axion Reus", "Cines Axion Reus", "E0920"),
    ("JCA Cinemes Tarragona Valls", "JCA Cinemes Tarragona Valls", "E0908"),
    ("MCB Altafulla - Les Bruixes", "MCB Altafulla - Les Bruixes", "E0320"),
    ("MCB Calafell", "MCB Calafell", "E0479"),
    ("Ocine Gavarres", "Ocine Gavarres", "E0509"),
    ("Ocine Roquetes", "Ocine Roquetes", "E0556"),
    ("Ocine Vila-seca", "Ocine Vila-seca", "E0727"),
    ("Rambla de L'art", "Rambla de L'art", "E0811"),
    ("Yelmo Cines Parc Central", "Yelmo Cines Parc Central", "E0807")
  ))
  private def p_teruel: R = ("teruel", "Teruel", "Aragón", 40.3456, -1.10646, "Europe/Madrid", Seq(
    ("Cine Arens de Lledó", "Cine Arens de Lledó", "E0810"),
    ("Cine Maravillas", "Cine Maravillas", "E0697"),
    ("Cines Alcañiz", "Cines Alcañiz", "E0653")
  ))
  private def p_toledo: R = ("toledo", "Toledo", "Castilla-La Mancha", 39.8581, -4.02263, "Europe/Madrid", Seq(
    ("Artesiete Los Alfares", "Artesiete Los Alfares", "E0803"),
    ("Cine Central 3D", "Cine Central 3D", "E0831"),
    ("Cine Princesa", "Cine Princesa", "E0837"),
    ("Cines Redux", "Cines Redux", "E0872"),
    ("Quintanar Cinema", "Quintanar Cinema", "E0870"),
    ("Real Cinema De Olías", "Real Cinema De Olías", "E0574"),
    ("mk2 Luz del Tajo", "mk2 Luz del Tajo", "E0412")
  ))
  private def p_valencia: R = ("valencia", "Valencia", "Comunidad Valenciana", 39.47391, -0.37966, "Europe/Madrid", Seq(
    ("Abc El Saler", "Abc El Saler", "E0034"),
    ("Abc Gandia", "Abc Gandia", "E0036"),
    ("Abc Gran Turia", "Abc Gran Turia", "E0037"),
    ("Abc Park", "Abc Park", "E0040"),
    ("Alucine Sagunto", "Alucine Sagunto", "E0071"),
    ("Autocine Star", "Autocine Star", "E0104"),
    ("Centre Cultural Almassafes", "Centre Cultural Almassafes", "E0759"),
    ("Centre Cultural Benetússer El Molí", "Centre Cultural Benetússer El Molí", "E0758"),
    ("Cine Avenida El Perelló", "Cine Avenida El Perelló", "E2914"),
    ("Cine La Unió Musical", "Cine La Unió Musical", "E0992"),
    ("Cine Montecarlo", "Cine Montecarlo", "E0883"),
    ("Cine Palacio de la Música de Buñol", "Cine Palacio de la Música de Buñol", "E0993"),
    ("Cine Teatro Principal Requena", "Cine Teatro Principal Requena", "E1029"),
    ("Cine Terraza Charly", "Cine Terraza Charly", "E0927"),
    ("Cine Terraza Olimpo", "Cine Terraza Olimpo", "E0928"),
    ("Cine Tívoli", "Cine Tívoli", "E0645"),
    ("Cine de Verano Serra", "Cine de Verano Serra", "E0930"),
    ("Cine de Verano Tugar", "Cine de Verano Tugar", "E0929"),
    ("Cineapolis El Teler", "Cineapolis El Teler", "E0617"),
    ("Cines Axion Premium Gandía", "Cines Axion Premium Gandía", "E1026"),
    ("Cines Axion de Xàtiva", "Cines Axion de Xàtiva", "E0664"),
    ("Cines Babel", "Cines Babel", "E0119"),
    ("Cines Lys", "Cines Lys", "E0187"),
    ("Cines MN4", "Cines MN4", "E0287"),
    ("Cines Victoria Cullera", "Cines Victoria Cullera", "E0210"),
    ("Cinesa Bonaire", "Cinesa Bonaire", "E0405"),
    ("Cinestudio D´or", "Cinestudio D´or", "E0407"),
    ("Kinepolis Alzira", "Kinepolis Alzira", "E0434"),
    ("Kinépolis Valencia", "Kinépolis Valencia", "E0454"),
    ("Ocine Premium Aqua", "Ocine Premium Aqua", "E0474"),
    ("Ozone Gandía", "Ozone Gandía", "E0282"),
    ("Teatro Flumen", "Teatro Flumen", "E0967"),
    ("Teatro García Berlanga", "Teatro García Berlanga", "E1030"),
    ("Terraza Lumiere", "Terraza Lumiere", "E0931"),
    ("Terraza de Verano", "Terraza de Verano", "E0987"),
    ("Terraza de Verano Oliva", "Terraza de Verano Oliva", "E0730"),
    ("Yelmo Cines Campanar", "Yelmo Cines Campanar", "E0248"),
    ("Yelmo Cines Mercado de Campanar", "Yelmo Cines Mercado de Campanar", "E0773"),
    ("Yelmo Cines VidaNova Parc", "Yelmo Cines VidaNova Parc", "E0932")
  ))
  private def p_valladolid: R = ("valladolid", "Valladolid", "Castilla y León", 41.65541, -4.72353, "Europe/Madrid", Seq(
    ("Cine Avenida", "Cine Avenida", "E0235"),
    ("Cine Casablanca", "Cine Casablanca", "E0243"),
    ("Cines Broadway", "Cines Broadway", "E0333"),
    ("Cines Manhattan", "Cines Manhattan", "E0357"),
    ("Multicines Coliseo", "Multicines Coliseo", "E0698"),
    ("Ocine Rio Shopping", "Ocine Rio Shopping", "E0796"),
    ("Teatro Principal", "Teatro Principal", "E0600"),
    ("Yelmo Cines Premium VallSur", "Yelmo Cines Premium VallSur", "E0297")
  ))
  private def p_vizcaya: R = ("vizcaya", "Vizcaya", "País Vasco", 43.26271, -2.92528, "Europe/Madrid", Seq(
    ("Autocine Getxo", "Autocine Getxo", "E0880"),
    ("Cine Torrebillela", "Cine Torrebillela", "E0767"),
    ("Cine Torrezabal", "Cine Torrezabal", "E0923"),
    ("Cine Zugaza", "Cine Zugaza", "E0768"),
    ("Cinesa Max Ocio", "Cinesa Max Ocio", "E0424"),
    ("Cinesa Zubiarte", "Cinesa Zubiarte", "E0425"),
    ("Ermua Antzokia", "Ermua Antzokia", "E0903"),
    ("Getxo Zinemak", "Getxo Zinemak", "E0464"),
    ("Golem Alhóndiga", "Golem Alhóndiga", "E0737"),
    ("Ikusgarri Zinema", "Ikusgarri Zinema", "E0891"),
    ("Liceo Antzokia", "Liceo Antzokia", "E0894"),
    ("Multicines 7 Bilbao", "Multicines 7 Bilbao", "E0488"),
    ("Olalde Aretoa", "Olalde Aretoa", "E1021"),
    ("Serantes Kultur Aretoa", "Serantes Kultur Aretoa", "E0598"),
    ("Yelmo Cines Artea", "Yelmo Cines Artea", "E0376"),
    ("Yelmo Cines Megapark", "Yelmo Cines Megapark", "E0626"),
    ("Zalla Zine - Antzokia ", "Zalla Zine - Antzokia ", "E0874"),
    ("Zornotza Aretoa", "Zornotza Aretoa", "E0904")
  ))
  private def p_zamora: R = ("zamora", "Zamora", "Castilla y León", 41.50633, -5.74456, "Europe/Madrid", Seq(
    ("Multicines Zamora", "Multicines Zamora", "E0540")
  ))
  private def p_zaragoza: R = ("zaragoza", "Zaragoza", "Aragón", 41.65606, -0.87734, "Europe/Madrid", Seq(
    ("Artesiete La Torre", "Artesiete La Torre", "E1041"),
    ("Cine Palafox Zaragoza", "Cine Palafox Zaragoza", "E0264"),
    ("Cine Sala Cervantes", "Cine Sala Cervantes", "E0711"),
    ("Cines Aragonia", "Cines Aragonia", "E0732"),
    ("Cinesa Grancasa", "Cinesa Grancasa", "E0387"),
    ("Cinesa Puerto Venecia", "Cinesa Puerto Venecia", "E0790"),
    ("Sala Goya", "Sala Goya", "E0595"),
    ("Teatro Capitol", "Teatro Capitol", "E1007"),
    ("Teatro Cine Goya", "Teatro Cine Goya", "E0668"),
    ("Teatro Reina Sofía", "Teatro Reina Sofía", "E1008")
  ))
  private def p_alava: R = ("alava", "Álava", "País Vasco", 42.84998, -2.67268, "Europe/Madrid", Seq(
    ("Cine Municipal Llodio", "Cine Municipal Llodio", "E0821"),
    ("Cines Florida", "Cines Florida", "E0346"),
    ("Cines Gorbeia Zinemak ", "Cines Gorbeia Zinemak ", "E0885"),
    ("Cines Guridi", "Cines Guridi", "E0763"),
    ("Yelmo Cines Boulevard", "Yelmo Cines Boulevard", "E0786")
  ))
  private def p_avila: R = ("avila", "Ávila", "Castilla y León", 40.65724, -4.69951, "Europe/Madrid", Seq(
    ("Cine Arenas", "Cine Arenas", "E0828"),
    ("Cine Blasco", "Cine Blasco", "E0980"),
    ("Cine Candeleda", "Cine Candeleda", "E0861"),
    ("Cine Rueda", "Cine Rueda", "E0966"),
    ("Cine-Teatro Lagasca", "Cine-Teatro Lagasca", "E0991"),
    ("Cines Bulevar", "Cines Bulevar", "E0344")
  ))

  private def chunk0: Seq[R] = Seq(p_a_coruna, p_albacete, p_alicante, p_almeria, p_asturias, p_badajoz, p_barcelona, p_burgos, p_cantabria, p_castellon, p_ceuta, p_ciudad_real, p_cuenca, p_caceres, p_cadiz, p_cordoba, p_girona, p_granada, p_guadalajara, p_guipuzcoa, p_huelva, p_huesca, p_islas_baleares, p_jaen, p_la_rioja, p_las_palmas, p_leon, p_lugo, p_lerida, p_madrid, p_melilla, p_murcia, p_malaga, p_navarra, p_ourense, p_palencia, p_pontevedra, p_salamanca, p_santa_cruz_de_tenerife, p_segovia)
  private def chunk1: Seq[R] = Seq(p_sevilla, p_soria, p_tarragona, p_teruel, p_toledo, p_valencia, p_valladolid, p_vizcaya, p_zamora, p_zaragoza, p_alava, p_avila)
  val provinces: Seq[R] = chunk0 ++ chunk1
}
