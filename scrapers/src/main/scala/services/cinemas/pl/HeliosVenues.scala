package services.cinemas.pl

import models.{Helios, HeliosAlejaBielany, HeliosAlfa, HeliosBelchatow, HeliosBiala, HeliosBielskoBiala, HeliosBlueCity, HeliosBydgoszcz, HeliosDabrowaGornicza, HeliosForum, HeliosGniezno, HeliosGorzow, HeliosGrudziadz, HeliosJeleniaGora, HeliosJurowiecka, HeliosKalisz, HeliosKatowice, HeliosKedzierzynKozle, HeliosKielce, HeliosKonin, HeliosKoszalin, HeliosKrosno, HeliosLegionowo, HeliosLegnica, HeliosLodz, HeliosLomza, HeliosLubin, HeliosMagnolia, HeliosMetropolia, HeliosNowySacz, HeliosOlsztyn, HeliosOpoleKarolinka, HeliosOpoleSolaris, HeliosOstrowWlkp, HeliosOutletPark, HeliosPabianice, HeliosPila, HeliosPiotrkow, HeliosPlock, HeliosPrzemysl, HeliosRadom, HeliosRiviera, HeliosRzeszow, HeliosSiedlce, HeliosSosnowiec, HeliosStalowaWola, HeliosStarachowice, HeliosSzczecin, HeliosTczew, HeliosTomaszow, HeliosWolomin, HeliosZory}

/** The Helios venues this app scrapes: a registry, one [[HeliosCinema]] a venue, each keyed by its REST source UUID
 *  (verified live against restapi.helios.pl) and page slug. The town rosters (`pl.venues.*Venues`) wire them. */
object HeliosVenues {
  val Poznan       = HeliosCinema(Helios,             "poznan",   "kino-helios",                "815face9-2a1d-4c62-9b2f-a361574b79a2")
  val Magnolia     = HeliosCinema(HeliosMagnolia,     "wroclaw",  "kino-helios-magnolia",       "c21c6e3b-874c-4432-b44e-a155ec9102cd")
  val AlejaBielany = HeliosCinema(HeliosAlejaBielany, "wroclaw",  "kino-helios-aleja-bielany",  "7582cee7-2815-4897-8715-e90a5b99d2e4")
  val BlueCity     = HeliosCinema(HeliosBlueCity,     "warszawa", "kino-helios-blue-city",      "4ca060df-c4f2-4157-8905-bf46527aae58")
  val Metropolia   = HeliosCinema(HeliosMetropolia,   "gdansk",   "kino-helios-metropolia",     "d09e2607-693f-479f-8deb-59a18add40eb")
  val Forum        = HeliosCinema(HeliosForum,        "gdansk",   "kino-helios-forum",          "60cfe883-55aa-4e28-be7f-233f589a34cb")
  val Riviera      = HeliosCinema(HeliosRiviera,      "gdynia",   "kino-helios",                "0bdb21cd-cf7d-4efb-829f-d1913a666b83")
  val Lodz         = HeliosCinema(HeliosLodz,         "lodz",     "kino-helios",                "46055d88-5f34-44a0-9584-b041caa71e26")
  val Alfa         = HeliosCinema(HeliosAlfa,         "bialystok", "kino-helios-alfa",          "3e616682-1048-46d8-954d-a2c8de8e7ee4")
  val Biala        = HeliosCinema(HeliosBiala,        "bialystok", "kino-helios-biala",         "d516a0dd-b0ab-4e15-85b2-0286eaf6dde6")
  val Jurowiecka   = HeliosCinema(HeliosJurowiecka,   "bialystok", "kino-helios-jurowiecka",    "5c699a45-d219-4f4e-90f4-4aac0ca44a88")
  val Bydgoszcz    = HeliosCinema(HeliosBydgoszcz,    "bydgoszcz","kino-helios",                "8acb28a0-2e4b-4beb-a13a-bb97e17085cc")
  // Katowice's REST sourceId is KATOWICE Libero (b661164e…). The old value
  // 6f3ef265… is in fact Olsztyn Galeria Aura — a copy-paste that silently fed
  // Katowice's room/format/synopsis enrichment (and restOnly films) from Olsztyn.
  val Katowice     = HeliosCinema(HeliosKatowice,     "katowice", "kino-helios",                "b661164e-04d4-41af-bdca-0f9a745c3898")
  val Szczecin     = HeliosCinema(HeliosSzczecin,     "szczecin", "kino-helios-chr-kupiec",     "3e60a454-c438-4c9b-b24f-7c6feff3b5ed")
  val SzczecinOutletPark = HeliosCinema(HeliosOutletPark, "szczecin", "helios-outlet-park",     "b898bbed-7ab6-428f-a57b-26d6b6e6a04b")
  // helios.pl renamed these two venues' page slugs to the chain-default
  // `kino-helios` (mid-June 2026); the old `kino-helios-<city>` slugs now
  // 302-redirect to the films-less homepage, which `HeliosClient` correctly
  // treats as a degraded scrape — so a stale slug silently empties the venue.
  val Radom        = HeliosCinema(HeliosRadom,        "radom",    "kino-helios",                "287fc8f4-7bee-49b4-afcd-b22b52e520b9")
  val Sosnowiec    = HeliosCinema(HeliosSosnowiec,    "sosnowiec","kino-helios",                "880f2a3a-e9e8-447c-95ca-72c49ca38a1e")
  val Kielce       = HeliosCinema(HeliosKielce,       "kielce",   "kino-helios",                "d01162e6-31a4-41b9-97cd-1ef7707522fe")
  val Rzeszow      = HeliosCinema(HeliosRzeszow,      "rzeszow",  "kino-helios-galeria",        "409eac78-9718-4fa3-b590-d1e1fb60892d")

  // New mid-size-city venues. citySlug/cinemaSlug verified to render a NUXT
  // repertoire page; sourceId verified against restapi.helios.pl/api/cinema.
  val Olsztyn      = HeliosCinema(HeliosOlsztyn,      "olsztyn",  "kino-helios",                "6f3ef265-ef99-48e9-84f4-ed862badb756")
  val BielskoBiala = HeliosCinema(HeliosBielskoBiala, "bielsko-biala", "kino-helios",           "cb932ba4-3dfd-4946-b3fd-1687603f78f7")
  val OpoleKarolinka = HeliosCinema(HeliosOpoleKarolinka, "opole", "kino-helios-karolinka",     "ec0ba789-c70b-42e7-8adf-8c9595888672")
  val OpoleSolaris = HeliosCinema(HeliosOpoleSolaris, "opole",    "kino-helios-solaris",        "d01e6010-d098-4899-951b-a7b7208df75a")
  val Gorzow       = HeliosCinema(HeliosGorzow,       "gorzow-wielkopolski", "kino-helios",     "6f51c391-4d6d-4aeb-bf78-6c61b676865f")
  val Koszalin     = HeliosCinema(HeliosKoszalin,     "koszalin", "kino-helios-galeria-emka",   "4b58d474-bf12-405f-b76d-f1f88cae2b07")
  val Kalisz       = HeliosCinema(HeliosKalisz,       "kalisz",   "kino-helios",                "99d1a365-3dad-4c9b-a9e9-6e6177630708")
  val Legnica      = HeliosCinema(HeliosLegnica,      "legnica",  "kino-helios",                "34b23726-7c53-483b-a0e5-13dfca6075ba")
  val Plock        = HeliosCinema(HeliosPlock,        "plock",    "kino-helios",                "4623d4a3-d0be-49d8-82d8-ef16a67a848c")
  val DabrowaGornicza = HeliosCinema(HeliosDabrowaGornicza, "dabrowa-gornicza", "kino-helios",  "d61fc6d9-bac1-44f0-a34a-00868e694c9b")
  val NowySacz     = HeliosCinema(HeliosNowySacz,     "nowy-sacz", "kino-helios",               "223cd021-5821-481a-9d04-31511fb44042")
  val JeleniaGora  = HeliosCinema(HeliosJeleniaGora,  "jelenia-gora", "kino-helios",            "88c70097-4f24-49b5-bd35-c752e3543d4e")
  val Przemysl     = HeliosCinema(HeliosPrzemysl,     "przemysl", "kino-helios",                "19a14011-0e44-43cd-9d12-8e8a16e9d744")
  val Konin        = HeliosCinema(HeliosKonin,        "konin",    "kino-helios",                "0fe6ba65-dc83-49c1-8f01-bef8675b9927")
  // Moved off Filmweb onto the native Helios REST feed.
  val Starachowice    = HeliosCinema(HeliosStarachowice,    "starachowice",       "kino-helios", "9906f18b-ed73-41d8-a35e-bfcda930b2d3")
  val Krosno          = HeliosCinema(HeliosKrosno,          "krosno",             "kino-helios", "be832e9d-5b79-47ef-a0b0-1b465349ce89")
  val Tczew           = HeliosCinema(HeliosTczew,           "tczew",              "kino-helios", "58fbf23b-c605-4185-b1e9-6a3c5d7878ce")
  val Zory            = HeliosCinema(HeliosZory,            "zory",               "kino-helios", "d116bcc0-8675-41d8-b0b0-3e7155df9ffc")
  val Lubin           = HeliosCinema(HeliosLubin,           "lubin",              "kino-helios", "71192dbe-94ef-43c0-9079-ae7c9747a2f4")
  val OstrowWielkopolski = HeliosCinema(HeliosOstrowWlkp,   "ostrow-wielkopolski", "kino-helios", "1a433348-f89c-4b18-bfa4-4332e3bbf0c8")
  val KedzierzynKozle = HeliosCinema(HeliosKedzierzynKozle, "kedzierzyn-kozle",   "kino-helios", "cf760fa9-32ba-4541-ae07-ef14532a911d")
  // Nearby-town and regional-hub venues (2026-09 sweep); sourceId verified against
  // restapi.helios.pl/api/cinema, page slug verified to render a NUXT repertoire.
  val Wolomin             = HeliosCinema(HeliosWolomin,             "wolomin",               "kino-helios", "4ff96e13-2d06-484d-be4c-870603e3c6a8")
  val Legionowo           = HeliosCinema(HeliosLegionowo,           "legionowo",             "kino-helios", "326a5265-593c-4457-a85e-c79e8574a5b6")
  val Pabianice           = HeliosCinema(HeliosPabianice,           "pabianice",             "kino-helios", "540264b4-46d6-47a2-9c1b-519c0056bf47")
  val Grudziadz           = HeliosCinema(HeliosGrudziadz,           "grudziadz",             "kino-helios", "7f651f6f-9068-410f-96a0-e5b6f1545aed")
  val PiotrkowTrybunalski = HeliosCinema(HeliosPiotrkow,            "piotrkow-trybunalski",  "kino-helios", "8f7c0839-2917-4cb9-bc3b-399d704c1323")
  val Belchatow           = HeliosCinema(HeliosBelchatow,           "belchatow",             "kino-helios", "2c4b802a-5e94-4a96-a899-6fe92fb4bf13")
  val TomaszowMazowiecki  = HeliosCinema(HeliosTomaszow,            "tomaszow-mazowiecki",   "kino-helios", "22c9c999-41fc-4637-834d-a34f9286e296")
  val Siedlce             = HeliosCinema(HeliosSiedlce,             "siedlce",               "kino-helios", "1da497b7-59af-476d-ac2b-246493650f7e")
  val Pila                = HeliosCinema(HeliosPila,                "pila",                  "kino-helios", "ee94a20b-e818-43f3-bb84-ec970b336a22")
  val Gniezno             = HeliosCinema(HeliosGniezno,             "gniezno",               "kino-helios", "2129a02f-c5cc-4da2-9293-daacd11c07ca")
  val StalowaWola         = HeliosCinema(HeliosStalowaWola,         "stalowa-wola",          "kino-helios", "01375340-d1ba-41d0-a00f-37c8acff6e7a")
  val Lomza               = HeliosCinema(HeliosLomza,               "lomza",                 "kino-helios", "46b9b050-9ba9-4817-9bd8-96ccfc68f1b8")
}
