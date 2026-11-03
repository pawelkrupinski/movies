// GENERATED from data/spain/taquilla.json + data/spain/artesiete.json by
// data/spain/scripts/generate_own_sources.py -- do NOT edit by hand.
// 60 venues off taquilla.com, 10 off Artesiete's own pages; see data/spain/README.md.
package services.cinemas.es

private[cinemas] object SpanishOwnSourcesData {
  /** SensaCine theaterId -> the venue on taquilla.com. */
  val taquilla: Map[String, TaquillaVenue] = Map(
    "E0193" -> TaquillaVenue("53439", "/murcia/neocine-centrofama-murcia"),   // Neocine Centrofama
    "E0205" -> TaquillaVenue("33189", "/albacete/yelmo-cines-imaginalia-albacete"),   // Yelmo Cines Imaginalia
    "E0209" -> TaquillaVenue("35590", "/alcobendas/cines-kinepolis-diversia-abaco-alcobendas"),   // Kinépolis Madrid Diversia
    "E0283" -> TaquillaVenue("90802", "/huarte/yelmo-cines-itaroa-huarte-uharte-navarra"),   // Yelmo Cines Itaroa
    "E0291" -> TaquillaVenue("177432", "/madrid/yelmo-cines-premium-parque-corredor-madrid"),   // Yelmo Cines Premium Parque Corredor
    "E0332" -> TaquillaVenue("33147", "/cadiz/mk2-bahia-de-cadiz-cines-premium"),   // mk2 Bahía de Cádiz
    "E0359" -> TaquillaVenue("364308", "/almeria/kinepolis-almeria-mediterraneo"),   // Kinépolis Almería Mediterráneo
    "E0390" -> TaquillaVenue("364307", "/marbella/kinepolis-marbella-la-canada"),   // Kinépolis La Cañada
    "E0396" -> TaquillaVenue("228149", "/mataro/kinepolis-mataro-parc"),   // Kinépolis Mataró Parc
    "E0411" -> TaquillaVenue("33176", "/alcala-de-guadaira/mk2-cines-los-alcores-alcala-de-guadaira"),   // mk2 Alcores
    "E0412" -> TaquillaVenue("33184", "/toledo/mk2-cines-luz-del-tajo-toledo"),   // mk2 Luz del Tajo
    "E0413" -> TaquillaVenue("33188", "/malaga/mk2-malaga-nostrum-cines-premium"),   // mk2 Malaga Nostrum
    "E0415" -> TaquillaVenue("54634", "/sevilla/mk2-cines-nervion-plaza-sevilla"),   // mk2 Nervión Plaza
    "E0431" -> TaquillaVenue("33180", "/san-javier/neocine-dos-mares-san-javier"),   // Neocine Dos Mares
    "E0433" -> TaquillaVenue("33333", "/velez-malaga/mk2-cines-el-ingenio-velez-malaga"),   // mk2 El Ingenio
    "E0434" -> TaquillaVenue("87355", "/valencia/kinepolis-alzira-valencia"),   // Kinepolis Alzira
    "E0452" -> TaquillaVenue("52323", "/granada/kinepolis-granada"),   // Kinépolis Granada
    "E0454" -> TaquillaVenue("53455", "/paterna/kinepolis-valencia-paterna"),   // Kinépolis Valencia
    "E0459" -> TaquillaVenue("200595", "/madrid/cine-yelmo-la-vaguada-madrid"),   // Yelmo Cines La Vaguada
    "E0475" -> TaquillaVenue("33194", "/madrid/yelmo-cines-plenilunio-madrid"),   // Yelmo Cines Plenilunio
    "E0506" -> TaquillaVenue("53438", "/aguilas/multicines-el-hornillo-aguilas"),   // Multicines El Hornillo
    "E0546" -> TaquillaVenue("33197", "/cartagena/neocine-mandarache-cartagena"),   // Neocine Mandarache
    "E0547" -> TaquillaVenue("33199", "/murcia/neocine-thader-murcia"),   // Neocine Thader
    "E0565" -> TaquillaVenue("151546", "/santander/yelmo-cines-premium-penacastillo-cantabria"),   // Yelmo Cines Premium Peñacastillo
    "E0615" -> TaquillaVenue("33166", "/barbera-del-valles/yelmo-cines-baricentro-barbera-del-valles"),   // Yelmo Cines Baricentro
    "E0618" -> TaquillaVenue("33171", "/antigua/yelmo-cines-fuerteventura-antigua"),   // Yelmo Cines Fuerteventura
    "E0620" -> TaquillaVenue("33172", "/roquetas-de-mar/yelmo-cines-roquetas-de-mar-roquetas-de-mar"),   // Yelmo Cines Roquetas
    "E0621" -> TaquillaVenue("33183", "/madrid/yelmo-cines-ideal-madrid"),   // Yelmo Cines Ideal
    "E0622" -> TaquillaVenue("33158", "/la-orotava/yelmo-cines-la-villa-orotava-orotava-la"),   // Yelmo Cines La Villa de Orotava
    "E0623" -> TaquillaVenue("33164", "/oviedo/yelmo-cines-los-prados-oviedo"),   // Yelmo Cines Los Prados
    "E0626" -> TaquillaVenue("33195", "/barakaldo/yelmo-cines-megapark-barakaldo"),   // Yelmo Cines Megapark
    "E0627" -> TaquillaVenue("33170", "/santa-cruz-de-tenerife/yelmo-cines-meridiano-santa-cruz-de-tenerife"),   // Yelmo Cines Meridiano
    "E0630" -> TaquillaVenue("33165", "/collado-villalba/yelmo-cines-planetocio-collado-villalba"),   // Yelmo Cines Planetocio
    "E0631" -> TaquillaVenue("56353", "/alicante/yelmo-cines-puerta-de-alicante-alicante-alacant"),   // Yelmo Cines Puerta De Alicante
    "E0632" -> TaquillaVenue("33156", "/rincon-de-la-victoria/yelmo-cines-rincon-de-la-victoria-rincon-de-la-victoria"),   // Yelmo Cines Rincón De La Victoria
    "E0635" -> TaquillaVenue("33167", "/vigo/yelmo-cines-vigo"),   // Yelmo Cines Travesía Vigo
    "E0636" -> TaquillaVenue("33162", "/petrer/yelmo-cines-vinalopo-petrer"),   // Yelmo Cines Vinalopo
    "E0658" -> TaquillaVenue("33209", "/ondara/imf-ondara"),   // Cine Imf Ondara
    "E0660" -> TaquillaVenue("89683", "/molina-de-segura/neocine-hd-digital-vega-plaza-molina-de-segura"),   // Neocine HD Digital Vega Plaza
    "E0663" -> TaquillaVenue("33205", "/cartagena/neocine-espacio-mediterraneo-cartagena"),   // NeoCine Espacio Mediterraneo
    "E0681" -> TaquillaVenue("33215", "/madrid/yelmo-cines-islazul-madrid"),   // Yelmo Cines Islazul
    "E0684" -> TaquillaVenue("88088", "/lugo/yelmo-as-termas-lugo"),   // Yelmo Cines As Termas
    "E0733" -> TaquillaVenue("170586", "/logrono/yelmo-cines-premium-berceo-la-rioja"),   // Yelmo Cines Premium Berceo
    "E0735" -> TaquillaVenue("33222", "/abrera/yelmo-cines-abrera"),   // Yelmo Cines Abrera
    "E0754" -> TaquillaVenue("33231", "/las-palmas-de-gran-canaria/yelmo-cines-las-arenas-palmas-de-gran-canaria-las"),   // Yelmo Cines Las Arenas
    "E0756" -> TaquillaVenue("346235", "/barcelona/kinepolis-barcelona-splau"),   // Kinépolis Barcelona Full Splau
    "E0773" -> TaquillaVenue("49994", "/valencia/yelmo-campanar-valencia"),   // Yelmo Cines Mercado de Campanar
    "E0786" -> TaquillaVenue("33221", "/vitoria-gasteiz/yelmo-cines-boulevard-vitoria-gasteiz"),   // Yelmo Cines Boulevard
    "E0806" -> TaquillaVenue("91238", "/castelldefels/yelmo-cines-castelldefels-castelldefels-barcelona"),   // Yelmo Cines Premium Castelldefels
    "E0807" -> TaquillaVenue("33217", "/tarragona/yelmo-cines-parc-central-tarragona"),   // Yelmo Cines Parc Central
    "E0819" -> TaquillaVenue("53181", "/alicante/kinepolis-plaza-mar-2-alicante"),   // Kinépolis Alicante
    "E0866" -> TaquillaVenue("61102", "/armilla/kinepolis-nevada-armilla"),   // Kinépolis Nevada
    "E0909" -> TaquillaVenue("84435", "/almeria/yelmo-torrecardenas-almeria"),   // Yelmo Cines Torrecárdenas
    "E0910" -> TaquillaVenue("84417", "/algeciras/yelmo-cines-premium-puerta-europa-algeciras"),   // Yelmo Cines Premium Puerta Europa 
    "E0972" -> TaquillaVenue("78555", "/las-palmas-de-gran-canaria/yelmo-cines-premium-alisios-las-palmas-de-gran-canaria"),   // Yelmo Cines Premium Alisios
    "E1002" -> TaquillaVenue("98159", "/sevilla/cine-yelmo-premium-lagoh-sevilla"),   // Yelmo Cines Premium Lagoh
    "E1038" -> TaquillaVenue("96556", "/badajoz/yelmo-cines-premium-el-faro-badajoz"),   // Yelmo Cines Premium El Faro
    "E1042" -> TaquillaVenue("96559", "/cadiz/yelmo-cines-premium-bahia-sur-cadiz"),   // Yelmo Cines Premium Bahía Sur
    "E2902" -> TaquillaVenue("96746", "/pontevedra/yelmo-cines-premium-vialia-vigo-pontevedra"),   // Yelmo Cines Premium Vialia Vigo
    "E2916" -> TaquillaVenue("33225", "/san-sebastian-de-los-reyes/yelmo-cines-plaza-norte-2-san-sebastian-de-los-reyes"),   // Yelmo Cines Plaza Norte 2
  )

  /** SensaCine theaterId -> the venue's page on its own Artesiete subdomain. */
  val artesiete: Map[String, String] = Map(
    "E0489" -> "https://lucena.artesiete.es/Cine/19/Artesiete-Lucena",   // Artesiete Lucena
    "E0491" -> "https://bahia.artesiete.es/Cine/33/Artesiete-Bahia",   // Multicines Bahia Mar
    "E0538" -> "https://ubeda.artesiete.es/Cine/20/Cines-Ubeda",   // Multicines Úbeda
    "E0716" -> "https://segovia.artesiete.es/Cine/13/Artesiete-Segovia",   // Artesiete Segovia
    "E0720" -> "https://ecija.artesiete.es/Cine/11/Artesiete-Ecija",   // Artesiete Écija
    "E0723" -> "https://terrazas.artesiete.es/Cine/1/Artesiete-terrazas",   // Artesiete Las Terrazas
    "E0803" -> "https://alfares.artesiete.es/Cine/8/Artesiete-Alfares",   // Artesiete Los Alfares
    "E0805" -> "https://holea.artesiete.es/Cine/2/Artesiete-Holea",   // Artesiete Holea
    "E0863" -> "https://fan.artesiete.es/Cine/4/Artesiete-Fan",   // Artesiete Fan
    "E1041" -> "https://latorre.artesiete.es/Cine/21/Artesiete-La-Torre",   // Artesiete La Torre
  )

  /** Whether the venue is read off a Spanish own source rather than SensaCine. */
  def ownSourced(theaterId: String): Boolean =
    taquilla.contains(theaterId) || artesiete.contains(theaterId)
}
