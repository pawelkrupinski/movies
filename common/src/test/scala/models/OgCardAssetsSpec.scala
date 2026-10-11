package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

/**
 * Every shareable page names a committed Open Graph card, and this pins that
 * the named file actually EXISTS — for every country, not just the one whose
 * cards someone remembered to generate.
 *
 * The city index emits `og-{slug}.jpg` and the `/` landing emits
 * [[Country.homeOgImage]], both as absolute prod URLs (see `_ogTagsApp`). A
 * country whose cards were never generated therefore doesn't degrade to a
 * generic card — it points Facebook, Messenger, Slack and X at a 404 and the
 * link preview comes back with no image at all. That is exactly what Germany
 * (158 regions, as it then was) and the United States (55 states + their metros) shipped with:
 * `og-*.png` covered Poland and the UK alone, because the weekly refresh
 * workflow only ever ran those two legs, and nothing failed when the other two
 * countries went live.
 *
 * A city added by a commit cannot have its card in that same commit: the
 * generator SCREENSHOTS the live page, and the new city's page does not exist
 * upstream until the commit creating it has deployed — and the deploy is gated
 * on this suite, so a red run here would block the very page the card needs.
 * A roster change that adds cities therefore lists their cards in a temporary,
 * exact `awaitingFirstDeploy` set (see 6c45248c0 for the shape); once deployed,
 * generate them with `KINOWO_COUNTRY=<code> sbt "web/PageTest/runMain
 * tools.OgCardGenerator <slug>…"` (about five seconds a card against prod) and
 * delete the set in the same commit as the cards.
 *
 * Runs off the filenames rather than the rendered HTML on purpose: the page
 * specs (`RepertoirePreviewMetaSpec`, `LandingViewSpec`) already pin the
 * URL a page emits, so what is left to prove is that the other end of that URL
 * is on disk — cheap to check for all 739 cards, where rendering 739 pages
 * would not be.
 */
class OgCardAssetsSpec extends AnyFlatSpec with Matchers {

  private val cards: File = testsupport.RepoRoot.file("web/src/main/assets/img")

  private def missing(names: Seq[String]): Seq[String] = names.filterNot(new File(cards, _).exists())

  "every country" should "have the landing card its `/` page names" in {
    missing(Country.all.map(_.homeOgImage)) shouldBe empty
  }

  /** Cities whose card cannot exist yet: their page is not live until the
   *  commit creating them has deployed, and the deploy is gated on this suite.
   *  Asserted to be EXACTLY the set still missing, so a generated card fails
   *  here until its entry is deleted — the list cannot become the place missing
   *  cards quietly go.
   *
   *  2026-10-09 — Spain's 201 new town and cluster pages (provinces → towns).
 *  2026-10-11 — Germany's 297 new town and cluster pages (regions → towns).
   *  2026-10-11 — the United States' 1,677 new town and cluster pages (metros → towns). */
  private val awaitingFirstDeploy: Set[String] = Seq(
    "adeje", "aguilar-de-campoo", "aguilas", "alcala-de-henares", "alcala-de-xivert", "alcala-la-real",
    "alcaniz", "alcazar-de-san-juan", "alcobendas", "alcorcon", "alcoy", "algeciras", "alhaurin-el-grande",
    "almazan", "almendralejo", "alzira", "amposta", "andujar", "antequera", "aranda-de-duero",
    "arcos-de-la-frontera", "arenas-de-san-pedro", "arrecife", "arroyo-de-la-encomienda", "astorga",
    "ayamonte", "badalona", "barakaldo", "barbastro", "barbate", "baza", "beasain", "bejar", "benidorm",
    "berga", "bilbao", "binefar", "blanes", "boltana", "bunol", "burgo-de-osma", "calahorra", "calatayud",
    "calpe", "camargo", "carballo", "cartagena", "castellon-de-la-plana", "cee", "ciudad-rodrigo",
    "ciutadella-de-menorca", "coria", "cornella-de-llobregat", "cortegana", "corvera-de-asturias",
    "coslada", "daimiel", "don-benito", "dos-hermanas", "ecija", "eibar", "el-ejido", "el-pont-de-suert",
    "el-puerto-de-santa-maria", "el-vendrell", "elche", "estella-lizarra", "estepa", "ferrol", "figueres",
    "fuengirola", "fuenlabrada", "galdakao", "gandia", "getafe", "gijon", "golmayo", "granollers", "guardo",
    "herrera-del-duque", "huarte", "huetor-tajar", "ibiza", "iniesta", "irun", "jaraiz-de-la-vera", "javea",
    "jerez-de-la-frontera", "l-hospitalet-de-llobregat", "la-orotava", "la-palma-del-condado",
    "la-seu-d-urgell", "la-zubia", "laredo-cantabria", "las-palmas-de-gran-canaria", "leganes", "leiro", "linares", "lleida",
    "logrono", "lorca", "los-llanos-de-aridane", "lucena", "mairena-del-aljarafe", "majadahonda", "manacor",
    "manresa", "mao", "marbella", "marchena", "marratxi", "martos", "mazarron", "medina-de-rioseco",
    "medina-del-campo", "mequinenza", "merida", "miranda-de-ebro", "molina-de-segura", "mollerussa",
    "monforte-de-lemos", "mostoles", "motril", "mungia", "navalmoral-de-la-mata", "navia", "oliva", "olot",
    "orihuela", "oviedo", "palafrugell", "palma-de-mallorca", "pamplona", "paterna",
    "pedrajas-de-san-esteban", "penaranda-de-bracamonte", "penarroya-pueblonuevo", "petrer",
    "pilar-de-la-horadada", "plasencia", "ponferrada", "pozoblanco", "premia-de-mar", "puerto-del-rosario",
    "puertollano", "requena", "reus", "ribadeo", "ronda", "roquetas-de-mar", "rota", "sabadell",
    "sabinanigo", "sagunto", "san-cristobal-de-la-laguna", "san-fernando", "san-javier",
    "san-martin-de-valdeiglesias", "san-sebastian", "san-vicente-del-raspeig", "sant-boi-de-llobregat",
    "sant-cugat-del-valles", "santa-maria-del-paramo", "santa-marta-de-tormes", "santander",
    "santiago-de-compostela", "siero", "sitges", "solsona", "talavera-de-la-reina", "tarancon", "tarrega",
    "telde", "terrassa", "tomelloso", "torrelodones", "totana", "tremp", "tudela", "ubrique",
    "valdemorillo", "valdemoro", "valdepenas", "velez-malaga", "viana", "vic", "vielha", "vigo",
    "vila-real", "vilafranca-del-penedes", "vilagarcia-de-arousa", "villablino", "villarrobledo",
    "villaviciosa-de-odon", "villena", "vinaros", "vitoria-gasteiz", "viveiro", "xinzo-de-limia", "zuera",
    "zumaia",
    // Germany's 297 new town and cluster pages (regions → towns, 2026-10).
    "aalen", "achim", "aichach", "albstadt", "alfeld", "alpirsbach", "alsdorf", "annaberg-buchholz", "asbach",
    "asperg", "auerbach", "aurich", "backnang", "bad-belzig", "bad-driburg", "bad-fuessing",
    "bad-kissingen", "bad-mergentheim", "bad-neustadt-an-der-saale", "bad-oldesloe", "bad-reichenhall",
    "bad-saulgau", "bad-vilbel", "bad-wildbad", "bad-woerishofen", "baden-baden", "baunatal",
    "bergisch-gladbach", "bernau", "boeblingen", "bonn", "bottrop", "bramsche", "bruchsal", "brunsbuettel",
    "buchholz-in-der-nordheide", "buedingen", "buende", "burg", "cloppenburg", "dahme", "darmstadt",
    "deggendorf", "delmenhorst", "detmold", "diessen-am-ammersee", "dillenburg", "dillingen-an-der-donau",
    "dingolfing", "dinslaken", "donaueschingen", "dreieich", "duderstadt", "duelmen", "dueren", "duesseldorf",
    "duisburg", "ebersbach", "eckental", "eggenfelden", "einbeck", "elmshorn", "emmendingen",
    "enkenbach-alsenborn", "erding", "erdmannhausen", "erlangen", "eschborn", "eschwege", "espelkamp",
    "essen", "esslingen-am-neckar", "fellbach", "forchheim", "frankenthal", "freiberg", "freudenstadt",
    "freyung", "friedrichsdorf", "friedrichshafen", "fuerstenfeldbruck", "fuerstenwalde-an-der-spree",
    "fuerth", "fuessen", "furtwangen-im-schwarzwald", "gelsenkirchen", "genthin", "gera", "germering",
    "gifhorn", "ginsheim-gustavsburg", "goeppingen", "gotha", "grafing-bei-muenchen", "greifswald",
    "grevenbroich", "grimma", "gruenberg", "gruenstadt", "guestrow", "guetersloh", "hagen", "halle", "hanau",
    "heide", "heidelberg", "heidenheim", "heinsberg", "helmstedt", "herford", "herne", "herrenberg",
    "hildesheim", "hof", "hofheim-am-taunus", "holzkirchen", "holzminden", "homburg", "hoyerswerda", "huerth",
    "ibbenbueren", "ilmenau", "iserlohn", "kaufbeuren", "kaufering", "kehl", "kelheim", "kelkheim",
    "kempten-im-allgaeu", "kerpen", "kirchheim-unter-teck", "kitzingen", "koenigs-wusterhausen",
    "koenigsbrunn", "kuehlungsborn", "lahnstein", "lahr-am-schwarzwald", "landau-in-der-pfalz", "langenfeld",
    "lehrte", "leutkirch-im-allgaeu", "leverkusen", "limbach-oberfrohna", "limburg", "lindau",
    "lingen", "loeningen", "ludwigsburg", "ludwigslust", "luedenscheid", "luenen", "lutherstadt-wittenberg",
    "mainz", "marl", "mayen", "meiningen", "meissen", "merseburg", "merzig", "meschede", "moelln",
    "moenchengladbach", "moers", "mosbach", "muehlacker", "muelheim-an-der-ruhr", "muellheim", "murrhardt",
    "nagold", "neckarsulm", "neresheim", "neu-anspach", "neu-ulm", "neuburg-an-der-donau",
    "neumarkt-in-der-oberpfalz", "neumuenster", "neunkirchen", "neuss", "neustadt-an-der-aisch",
    "neustadt-an-der-weinstrasse", "neustadt-in-holstein", "neustrelitz", "neuwied", "nonnweiler",
    "norddorf-auf-amrum", "nordhorn", "northeim", "nuertingen", "oberhausen", "oberndorf-am-neckar",
    "oehringen", "offenbach-am-main", "oldenburg-in-holstein", "oranienburg", "osterholz-scharmbeck",
    "ottobrunn", "papenburg", "pfaffenhofen-an-der-ilm", "pforzheim", "pirmasens", "pirna", "plettenberg",
    "potsdam", "preetz", "prien-am-chiemsee", "quickborn", "rastatt", "rathenow", "ratingen",
    "recklinghausen", "remscheid", "rendsburg", "reutlingen", "rheinfelden", "rietschen", "rinteln", "rodgau",
    "rothenburg", "rottenburg-am-neckar", "rottweil", "rudolstadt", "saalfeld", "salzgitter", "sangerhausen",
    "schleiz", "schleswig", "schloss-holte-stukenbrock", "schluechtern", "schmelz", "schongau", "schorndorf",
    "schwabach", "schwaebisch-hall", "schwalmstadt", "schwandorf", "senftenberg", "siegburg",
    "simbach-am-inn", "sindelfingen", "singen-am-hohentwiel", "sinsheim", "soest", "speyer", "spremberg",
    "stade", "taucha", "templin", "torgau", "traunreut", "tuebingen", "tuttlingen", "ueberlingen",
    "ueckermuende", "uelzen", "unterschleissheim", "vilsbiburg", "voelklingen", "waldkraiburg",
    "walsrode", "wangen-im-allgaeu", "warburg", "wasserburg-am-inn", "weiden-in-der-oberpfalz",
    "weil-am-rhein", "weil-der-stadt", "weilheim-in-oberbayern", "weimar", "weinheim", "weiterstadt",
    "wermelskirchen", "wertheim-am-main", "wesenberg", "wiesbaden", "wildeshausen", "wilhelmshaven", "winsen",
    "wismar", "witten", "wittingen", "wittlich", "wittstock", "wolfenbuettel", "wolfratshausen", "wolfsburg",
    "worms", "wuerzburg", "wusterhausen", "zeil-am-main", "zeven", "zittau", "zwickau",
    // 2026-10-11 — the pages of the cinemas Cineamo, kinoheld and the Filmstarts id sweep found.
    "aachen", "ahaus", "ahlen", "biberach-an-der-riss", "bocholt", "bochum", "cochem", "dorfen", "garbsen",
    "giessen", "gronau-westf", "hamm", "lippstadt", "lohr-am-main", "naumburg", "norderney", "riedlingen",
    "siegen", "solingen", "viersen", "warendorf", "werder",
    // 2026-10-11 — the United States' new town and cluster pages (metros → towns).
    "aberdeen-maryland", "aberdeen-mississippi", "abingdon", "ada", "adams", "afton", "agawam", "aguadilla",
    "ainsworth", "airway-heights", "aitkin", "akron", "alamogordo", "albany-oregon", "albemarle",
    "albertville", "albion", "albion-nebraska", "alexander-city", "alexandria-bay", "alexandria-louisiana",
    "alexandria-virginia", "algona", "alhambra", "alice", "allen", "allentown", "alliance", "alma",
    "alpena", "alpharetta", "alpine", "alturas", "altus", "alva", "amelia", "amenia", "ames", "amherst",
    "amherst-massachusetts", "anaconda", "anadarko", "anaheim", "andalusia", "anderson", "andover",
    "andrews", "ann-arbor", "annapolis", "anniston", "antigo", "antlers", "appleton", "arapahoe", "ardmore",
    "arecibo", "arlington", "arlington-virginia", "arnold", "arnold-nebraska", "arroyo-grande", "arvada",
    "ashburn", "asheboro", "ashland-ohio", "ashland-wisconsin", "ashtabula", "astoria", "atascadero",
    "athens-ohio", "athens-tennessee", "athens-texas", "athol", "atlantic-city", "atmore", "atwood",
    "auburn", "auburn-alabama", "auburn-california", "auburn-maine", "auburn-washington", "aurora",
    "aurora-illinois", "aurora-nebraska", "austin-minnesota", "ava", "avalon", "avon", "avondale",
    "bad-axe", "bainbridge-island", "baker-city", "bar-harbor", "barberton", "barrington", "bartlesville",
    "bastrop", "batesville", "battle-creek", "baudette", "baxter", "bay-city", "bayamon", "baytown",
    "beacon", "beatrice", "beaumont", "beavercreek", "beaverton", "bedford", "bedford-indiana", "beeville",
    "belfield", "belle-plaine", "belleville", "bellevue", "bellmore", "beloit", "belton", "benkelman",
    "bennington", "bensalem", "benson", "benton", "benton-harbor", "berkeley", "berryville", "bethany",
    "bethel-maine", "bethesda", "bethlehem", "beverly-hills", "bicknell", "big-bear-lake", "big-rapids",
    "big-sky", "big-spring", "big-timber", "birmingham-michigan", "bishop", "bismarck", "blackduck",
    "blacksburg", "bloomington-illinois", "bloomington-minnesota", "bluefield", "blytheville", "boca-raton",
    "boerne", "bolingbrook", "bolivar", "bolivar-tennessee", "bonita-springs", "bonners-ferry", "boone",
    "borger", "boscobel", "bossier-city", "bottineau", "boulder", "bowman", "bradenton", "brady", "brandon",
    "branson", "brattleboro", "breckenridge", "bremerton", "brenham", "brentwood", "bridgeport", "bridgton",
    "brigham-city", "bristol-tennessee", "britton", "broken-arrow", "broken-bow", "brookfield-wisconsin",
    "brookhaven", "brookings", "brookings-oregon", "brownsburg", "brownsville", "brunswick", "bryan",
    "bryant", "buchanan", "buena-park", "buena-vista", "buffalo", "buffalo-oklahoma", "buffalo-wyoming",
    "burbank", "burley", "burlington-colorado", "burlington-north-carolina", "burlington-vermont",
    "burlington-wisconsin", "burwell", "butler", "cabot", "cadillac", "calabasas", "calais", "caldwell",
    "california", "calvert-city", "camarillo", "camas", "cambridge", "cambridge-ohio", "camden", "cameron",
    "campbellsville", "canby", "cando", "canon-city", "canton", "canton-illinois", "cape-coral",
    "carbondale", "carbondale-colorado", "carlisle", "carlsbad", "carlsbad-new-mexico", "carmel",
    "carolina", "carrington", "carrollton", "carson-city", "carthage", "cary", "casa-grande", "cascade",
    "castle-rock", "cavalier", "cedar-bluff", "cedar-park", "celina", "centennial", "center", "centerville",
    "centerville-tennessee", "central-city", "centralia", "centre", "centreville", "chadron", "chamberlain",
    "champaign", "chandler", "chandler-oklahoma", "chanute", "chapel-hill", "charles-city", "cheboygan",
    "checotah", "chelan", "cherry-hill", "chesapeake", "chester", "cheyenne", "cheyenne-wells", "chickasha",
    "chillicothe", "chillicothe-missouri", "chilton", "chino", "chippewa-falls", "choteau", "chula-vista",
    "cicero", "circle", "clanton", "clarion", "clarion-iowa", "clarksville", "clay-center", "clayton",
    "clayton-georgia", "cle-elum", "clearfield", "clearlake", "clearwater", "cleveland-heights",
    "cleveland-tennessee", "clifton", "clifton-park", "clinton", "clinton-arkansas", "clinton-illinois",
    "clinton-missouri", "clinton-north-carolina", "clinton-township", "cloquet", "clovis",
    "clovis-new-mexico", "coalinga", "cody", "coeur-d-alene", "colby", "coldwater", "coldwater-kansas",
    "colorado-city", "columbia-maryland", "columbia-mississippi", "columbus-indiana",
    "columbus-mississippi", "columbus-nebraska", "commerce-city", "concord", "concord-new-hampshire",
    "concord-north-carolina", "concordia", "concrete", "conrad", "conway", "conway-south-carolina", "cook",
    "cookeville", "cooperstown", "coos-bay", "copperas-cove", "coral-gables", "coral-springs", "cordele",
    "cordell", "cordova", "corinth", "corning", "corning-iowa", "corona", "corsicana", "cortez", "cortland",
    "corvallis", "coshocton", "costa-mesa", "coudersport", "council-bluffs", "covington",
    "cranberry-township", "crawfordsville", "crescent-city", "crested-butte", "crestview", "crete",
    "creve-coeur", "crookston", "crosby", "crowley", "crystal-lake", "cullman", "culpeper", "curtis",
    "cut-bank", "cypress", "dacula", "dallas", "dallas-oregon", "dalton", "daly-city", "danbury",
    "danville", "danville-kentucky", "daphne", "davenport", "davie", "davis", "dayton-washington",
    "de-pere", "de-soto", "dearborn", "decatur", "decatur-alabama", "decatur-georgia", "decatur-texas",
    "dededo-village", "defiance", "del-rio", "delano", "dell-rapids", "delray-beach", "delta", "delta-utah",
    "deming", "demopolis", "denison", "dennis", "denton", "derby", "detroit-lakes", "dexter", "dickson",
    "dillon", "dodgeville", "dorado", "douglas", "douglas-wyoming", "dover-foxcroft", "downey", "dublin",
    "dublin-georgia", "duluth-georgia", "dumas", "duncan", "dunkirk", "durham", "eagle-butte", "eagle-pass",
    "early", "easley", "east-grand-forks", "east-hampton", "east-lansing", "east-stroudsburg", "east-tawas",
    "east-windham", "eastland", "easton", "eden", "edenton", "edinburg", "edison", "edmond", "edwards",
    "edwardsville", "el-cajon", "el-dorado", "el-dorado-kansas", "el-dorado-springs", "el-segundo",
    "elberton", "elgin", "elizabeth", "elizabethtown", "elk-city", "elk-grove", "elk-rapids", "elkader",
    "elkhart", "ellensburg", "ellijay", "elroy", "ely-minnesota", "emeryville", "emmett", "emporia",
    "enterprise", "ephraim", "escanaba", "escondido", "estes-park", "eugene", "eunice", "eureka",
    "eureka-montana", "evanston", "evanston-wyoming", "everett", "fairbury", "fairfax", "fairfield",
    "fairmont", "fairview", "fajardo", "fall-river", "farmington", "farmington-maine",
    "farmington-missouri", "farmville", "faulkton", "fayetteville-arkansas", "fayetteville-georgia",
    "fergus-falls", "findlay", "fish-creek", "fitchburg", "fitzgerald", "flagler", "florence-kentucky",
    "florence-oregon", "floresville", "folsom", "fond-du-lac", "fontana", "forest-city", "forest-grove",
    "forney", "forrest-city", "forsyth", "fort-bliss", "fort-bragg", "fort-lauderdale", "fort-lee",
    "fort-moore", "fort-morgan", "fort-myers", "fort-pierre", "fort-scott", "fort-stockton", "fort-worth",
    "fowler", "framingham", "frankfort", "frankfort-michigan", "franklin", "franklin-west-virginia",
    "frederick", "fredericksburg", "freeport", "fremont", "fremont-nebraska", "friday-harbor",
    "friendswood", "frisco", "front-royal", "frostburg", "fullerton", "fulton", "gainesville",
    "gainesville-georgia", "gainesville-texas", "gaithersburg", "galax", "galesburg", "gallipolis",
    "garden-city", "garden-grove", "garland", "garrett", "garrison", "gastonia", "gatesville", "geneva",
    "geneva-nebraska", "georgetown", "georgetown-south-carolina", "gettysburg", "gilbert", "gilford",
    "gillette", "glasgow-kentucky", "glen-allen", "glen-burnie", "glendale", "glendale-california",
    "glendive", "globe", "gold-beach", "goldsboro", "goleta", "gonzales", "gonzales-texas", "goodland",
    "gordon", "grafton", "granbury", "grand-canyon-village", "grand-prairie", "grand-rapids-minnesota",
    "grandville", "grangeville", "granite-city", "grants", "grants-pass", "grapevine", "grass-valley",
    "grayling", "great-bend", "greece", "greeley", "greenacres-city", "greencastle", "greeneville",
    "greenfield", "greensboro", "greensboro-georgia", "greensburg", "greensburg-indiana",
    "greensburg-kansas", "greenville-alabama", "greenville-mississippi", "greenville-north-carolina",
    "greenwood-indiana", "gregory", "grenada", "gresham", "grinnell", "grove", "grundy", "guayama",
    "guaynabo", "guin", "gulf-shores", "gulfport", "gunnison", "gunnison-utah", "hadensville", "hagerstown",
    "haleyville", "hamilton", "hammond", "hampton", "hampton-bays", "hampton-south-carolina", "hannibal",
    "hanover", "hanover-pennsylvania", "hardin", "hardy", "harlan", "harlingen", "harlowton",
    "harrisburg-illinois", "harrison", "harrisonburg", "hartford-kentucky", "hastings", "hastings-michigan",
    "havelock", "hayward", "hayward-wisconsin", "hazard", "hazen", "hazleton", "healdsburg", "heath",
    "heber-city", "heber-springs", "hebron", "hebron-north-dakota", "henderson", "henderson-kentucky",
    "henderson-north-carolina", "hendersonville", "hendersonville-north-carolina", "hereford", "hermiston",
    "hesperia", "hettinger", "hialeah", "hiawatha", "hibbing", "hickory", "high-point", "highland",
    "highland-arkansas", "hill-city", "hillsboro", "hilo", "hilton-head-island", "hinckley", "hinesville",
    "hingham", "hobbs", "holdrege", "holland", "holly", "hollywood", "homer", "homestead", "hondo",
    "honoka-a", "honolulu", "hoopeston", "hoover", "hopkinsville", "hoquiam", "hornell", "hot-springs",
    "hot-springs-south-dakota", "houghton", "houghton-lake", "houlton", "houma", "houston-missouri",
    "howell", "hudson", "hugoton", "humboldt", "huntersville", "huntington", "huntington-beach",
    "huntington-indiana", "huntsville-texas", "huron", "hurricane", "hutchinson-minnesota", "ida-grove",
    "idabel", "imperial", "independence", "independence-kansas", "indian-trail", "indiana", "inez",
    "inglewood", "international-falls", "inverness", "ionia", "iowa-city", "iowa-falls", "irvine", "irving",
    "issaquah", "ithaca", "jackson-california", "jackson-michigan", "jackson-ohio", "jackson-wyoming",
    "jacksonville-illinois", "jacksonville-texas", "jamestown-tennessee", "janesville", "jasper",
    "jefferson-city", "jeffersonville", "jersey-city", "jerseyville", "jesup", "johnson-city", "johnstown",
    "johnstown-new-york", "joliet", "jonesville", "julesburg", "jupiter", "kahului", "kailua-kona", "kane",
    "kanopolis", "kapolei", "katy", "kaunakakai", "kayenta", "keene", "kelso", "kemmerer", "kenai",
    "kenmare", "kenner", "kennesaw", "kennett", "kennewick", "kenosha", "kent-washington", "kenton",
    "keokuk", "kernersville", "kerrville", "kewanee", "key-west", "kill-devil-hills", "killeen",
    "kilmarnock", "kimball", "kingfisher", "kingman", "kingman-kansas", "kingston",
    "kingston-massachusetts", "kinsley", "kinston", "kirkland", "kirksville", "kissimmee", "klamath-falls",
    "knox", "knoxville-iowa", "kutztown", "kyle", "la-grande", "la-jolla", "la-junta", "ladysmith",
    "lafayette-indiana", "lagrange", "laguna-niguel", "lake-charles", "lake-city", "lake-city-colorado",
    "lake-elsinore", "lake-jackson", "lake-ozark", "lake-placid", "lakeland", "lakeport", "lakewood",
    "lakewood-washington", "lamar", "lamar-colorado", "lamesa", "lamoni", "lanai-city",
    "lancaster-california", "lancaster-new-hampshire", "lancaster-new-york", "lancaster-ohio",
    "lancaster-texas", "lancaster-wisconsin", "lander", "langdon", "lansdale", "lansing", "lapeer",
    "laramie", "largo", "larkspur", "larned", "las-vegas-new-mexico", "laughlin", "laurel", "laurinburg",
    "lawrence", "lawrenceburg", "lawrenceville", "layton", "le-mars", "leachville", "lebanon",
    "lebanon-missouri", "lebanon-ohio", "lee-s-summit", "leesville", "lemmon", "lemoore", "leominster",
    "lewisburg-west-virginia", "lewistown-pennsylvania", "lewisville", "lexington-nebraska",
    "lexington-south-carolina", "lexington-tennessee", "libby", "liberal", "limon", "lincoln-city",
    "lincoln-illinois", "lincoln-kansas", "lincoln-new-hampshire", "linn", "linton", "lisbon", "litchfield",
    "litchfield-minnesota", "lititz", "little-falls", "littleton-colorado", "livermore", "livingston",
    "livingston-alabama", "llano", "lockport", "lodi", "logan-west-virginia", "logansport", "lompoc",
    "london-kentucky", "long-beach", "long-beach-washington", "longmont", "longview", "lorain",
    "los-alamos", "los-banos", "los-lunas", "loveland", "lovell", "lovington", "lowell", "lucas",
    "ludington", "lumberton", "luverne", "lyman", "lynchburg", "macomb", "macon-missouri", "madawaska",
    "madera", "madison-connecticut", "madison-indiana", "madison-south-dakota", "madisonville", "madras",
    "malta", "mammoth-lakes", "manchester-connecticut", "manchester-iowa", "manistee", "manistique",
    "manitowoc", "mansfield", "mansfield-city", "maquoketa", "marathon", "marble-falls", "marbleton",
    "marco-island", "marianna", "maricopa", "marietta", "marinette", "marion", "marion-illinois",
    "marion-iowa", "marion-north-carolina", "marion-virginia", "marksville", "marquette", "marshall",
    "marshall-arkansas", "marshall-missouri", "marshalltown", "marshfield", "martinsburg", "martinsville",
    "martinsville-indiana", "marysville", "marysville-kansas", "maryville", "maryville-missouri", "mashpee",
    "mason", "massena", "matamoras", "mattoon", "mattydale", "maumee", "mayfield", "maysville", "mayville",
    "mcallen", "mcconnelsville", "mcdonough", "mclean", "mcminnville", "mcminnville-tennessee", "meadville",
    "medford-wisconsin", "melbourne", "mena", "menomonie", "mentor", "merced", "meridian",
    "meridian-mississippi", "merrill", "mesa", "mesilla", "mesquite", "mesquite-nevada", "metairie",
    "metaline-falls", "methuen", "mexico", "miami-beach", "miami-oklahoma", "miamisburg", "michigan-city",
    "middlebury-village", "middlesboro", "middletown", "middletown-connecticut", "middletown-delaware",
    "midland-texas", "midlothian", "milbank", "milbridge", "milford", "milford-delaware",
    "milford-new-hampshire", "millbury", "milledgeville", "miller", "millsboro", "mineola", "mineral-wells",
    "minneapolis", "mishawaka", "missouri-city", "moberly", "moline", "monett", "monetta",
    "monroe-wisconsin", "monterey", "monticello", "monticello-arkansas", "montpelier", "montpelier-idaho",
    "moore", "moorefield", "mora", "morehead", "morenci", "moreno-valley", "morgantown", "morris",
    "morristown", "morrisville", "morton", "moscow", "moses-lake", "mott", "moultrie", "mount-airy",
    "mount-airy-georgia", "mount-gilead", "mount-lebanon", "mount-pleasant", "mount-pleasant-iowa",
    "mount-pleasant-michigan", "mount-pleasant-texas", "mount-pleasant-utah", "mount-shasta",
    "mount-sterling", "mount-vernon", "mount-vernon-illinois", "mount-vernon-ohio", "mountain-grove",
    "mountain-home", "mountain-view", "mountain-view-arkansas", "muncie", "murray", "murrieta", "muscatine",
    "muskegon", "muskogee", "nantucket", "naperville", "nashville-illinois", "nashville-indiana",
    "national-city", "nebraska-city", "nederland", "neligh", "neosho", "nephi", "new-bern", "new-braunfels",
    "new-brunswick", "new-caney", "new-castle", "new-lenox", "new-london", "new-london-wisconsin",
    "new-martinsville", "new-paltz", "new-philadelphia", "new-ulm", "newark", "newark-delaware", "newberg",
    "newberry", "newcastle", "newport", "newport-beach", "newport-news", "newport-vermont",
    "newport-washington", "newton", "newton-illinois", "newton-kansas", "niagara-falls", "nogales",
    "norfolk-virginia", "norman", "north-attleborough-center", "north-charleston", "north-conway",
    "north-fort-myers", "north-las-vegas", "north-little-rock", "north-portland", "north-virginia-beach",
    "north-wilkesboro", "northfield", "norton", "norton-kansas", "norwalk", "norwalk-connecticut",
    "norwood", "o-fallon", "oak-bluffs", "oak-grove", "oak-harbor", "oak-ridge", "oakes", "oakhurst",
    "oakland", "oakland-maryland", "oakley", "oberlin", "oceanside", "ocoee", "odessa", "ogallala", "ogden",
    "okemah", "olathe", "old-forge", "omak", "onancock", "onarga", "onawa", "oneida", "oneida-tennessee",
    "oneonta", "ontario-california", "ontario-ohio", "opelousas", "orange", "orangeburg", "orem",
    "oro-valley", "orofino", "oroville", "oshkosh", "oskaloosa", "oswego", "oswego-new-york", "othello",
    "ottawa", "ottawa-kansas", "overland-park", "owasso", "owatonna", "owings-mills", "oxford", "oxnard",
    "ozark", "pace", "page", "page-north-dakota", "pageland", "pago-pago", "pagosa-springs", "palestine",
    "palm-bay", "palm-coast", "palmdale", "pampa", "panama-city", "panguitch", "paonia", "paris",
    "paris-illinois", "park-falls", "park-rapids", "parker", "parkersburg", "parsons", "pasadena",
    "pasadena-california", "pauls-valley", "pawhuska", "payson", "pearl-city", "pearland", "pell-city",
    "pembroke-pines", "pendleton", "peoria-arizona", "perham", "perry", "perry-new-york", "perryton",
    "perryville", "petaluma", "petersburg", "petoskey", "pflugerville", "philadelphia-mississippi",
    "philipsburg", "piedmont", "pikeville", "pinecrest", "pittsburg", "pittsfield", "plano", "plantation",
    "platte", "platteville", "plattsburgh", "pleasanton", "plymouth", "plymouth-indiana",
    "plymouth-michigan", "pocahontas", "point-arena", "polson", "pompano-beach", "ponce", "ponderay",
    "pontiac", "pooler", "poplar-bluff", "port-angeles", "port-arthur", "port-charlotte", "port-lavaca",
    "port-saint-lucie", "port-townsend", "portage", "portage-pennsylvania", "porterville", "portland-texas",
    "poteau", "potsdam-new-york", "pottstown", "poughkeepsie", "powell", "prairie-du-chien", "pratt",
    "prattville", "prescott-valley", "preston", "price", "princeton", "princeton-illinois",
    "princeton-indiana", "prineville", "provo", "pryor", "queen-creek", "queensbury", "quincy-california",
    "quinnesec", "raleigh", "rancho-cucamonga", "rancho-mirage", "randolph", "rangeley", "ravenna",
    "rawlins", "reading", "red-bank", "red-bank-tennessee", "red-bluff", "red-lodge", "red-oak", "red-wing",
    "redding", "redfield", "redlands", "redmond", "rensselaer", "renton", "rexburg", "rialto", "rice-lake",
    "richardson", "richland-center", "richmond-california", "richmond-kentucky", "richmond-missouri",
    "richmond-texas", "ridgecrest", "ridgeland", "rifle", "rio-grande-city", "rio-rancho", "ripley",
    "ritzville", "riverside", "riverton", "riverview", "roanoke-rapids", "robinson", "rock-hill",
    "rock-springs", "rockland", "rocky-mount", "rogers", "rogers-city", "rolla-north-dakota", "rome",
    "rome-georgia", "roosevelt", "roseau", "roseburg", "roseville", "roswell-georgia", "rothschild",
    "round-rock", "roxboro", "rugby", "ruidoso", "rushville", "russell", "russellville", "ruston", "saco",
    "safford", "saginaw", "saguache", "saint-charles", "saint-clairsville", "saint-croix",
    "saint-croix-falls", "saint-francis", "saint-helens", "saint-johns", "saint-joseph", "saint-marys",
    "saint-michael", "saint-paul", "salem", "salem-massachusetts", "salem-ohio", "salinas", "salisbury",
    "san-angelo", "san-bernardino", "san-francisco", "san-jose", "san-marcos", "san-marcos-texas",
    "san-mateo", "san-ramon", "sandusky-michigan", "sandy", "sanford", "sanford-maine", "santa-ana",
    "santa-clara", "santa-clarita", "santa-cruz", "santa-maria", "santa-monica", "santa-rosa-new-mexico",
    "sapulpa", "sarasota", "savannah-tennessee", "sayre", "schenectady", "schererville", "schertz",
    "schroon-lake", "scobey", "scottsboro", "scottsdale", "scranton", "seaside", "sebring", "sedalia",
    "sedan", "selinsgrove", "selma", "selma-alabama", "seminole", "senatobia", "seneca", "seneca-kansas",
    "sevierville", "seward", "seymour", "shallotte", "shamrock", "sharon-springs", "shattuck", "shawano",
    "shawnee", "shawnee-kansas", "sheboygan", "shelby", "shelby-montana", "shelbyville",
    "shelbyville-illinois", "shelbyville-tennessee", "shelton", "sherman", "sidney", "sidney-montana",
    "sidney-nebraska", "sierra-vista", "sikeston", "silver-city", "silver-spring", "silverton",
    "simi-valley", "simpsonville", "sioux-center", "siren", "sisters", "sitka", "skowhegan", "slidell",
    "smith-center", "smithfield", "snyder", "socorro", "soda-springs", "soledad", "somerville", "sonora",
    "south-barrington", "south-bend", "south-boston", "south-fallsburg", "south-jordan", "south-kingstown",
    "south-taft", "southern-pines", "southport", "spanish-fork", "sparks", "sparta", "sparta-wisconsin",
    "spartanburg", "spearfish", "spearman", "spencer-indiana", "spencer-west-virginia", "spirit-lake",
    "spring", "spring-hill", "spring-hill-tennessee", "spring-valley", "springerville",
    "springfield-colorado", "springfield-georgia", "springfield-massachusetts", "springfield-ohio",
    "springfield-oregon", "springfield-vermont", "springhill", "springville", "st-johns", "st-johnsbury",
    "st-petersburg", "stafford", "stamford-texas", "stanley", "starke", "starkville", "statesboro",
    "steamboat-springs", "sterling-heights", "sterling-illinois", "stevens-point", "stigler", "stillwater",
    "stockton-california", "stony-brook", "storm-lake", "stowe", "strongsville", "stuart-florida",
    "sturgeon-bay", "sturgis", "stuttgart-arkansas", "suffern", "suffolk-virginia", "sullivan",
    "sulphur-springs", "summerlin-south", "summersville", "summerville", "sumter", "sun-prairie",
    "sunnyside", "sunnyvale", "sunrise", "superior", "surprise", "sutton", "syracuse-kansas", "tabor-city",
    "tacoma", "tahlequah", "tampa", "taos", "tavernier", "taylorville", "teays-valley", "tehachapi",
    "telluride", "temecula", "tempe", "texarkana", "the-colony", "the-villages", "the-woodlands",
    "thermopolis", "thief-river-falls", "thomaston", "thomasville", "thompson-falls", "thomson",
    "thousand-oaks", "three-rivers", "tiffin", "tigard", "tillamook", "tioga", "tiptonville", "titusville",
    "tomball", "toms-river", "tonkawa", "tooele", "topeka", "torrance", "torrington", "torrington-wyoming",
    "tracy", "traverse-city", "trenton", "tribune", "trinidad", "troy", "truckee", "tuba-city", "tulare",
    "tupper-lake", "tutu", "twentynine-palms", "two-harbors", "tyler", "ulysses", "uniontown",
    "universal-city", "utica", "uvalde", "vacaville", "valdez", "valdosta", "vallejo", "valley-city",
    "valparaiso", "van-buren", "van-wert", "vancouver", "vandalia", "vandergrift", "vashon", "vassar",
    "venice", "ventura", "vernal", "vernon", "vero-beach", "viborg", "vicksburg", "victor", "victorville",
    "vidalia", "vienna-bend", "vincennes", "vineland", "vineland-minnesota", "virginia", "virginia-beach",
    "viroqua", "visalia", "vista", "wabash", "waconia", "wadena", "wagner", "waikoloa", "waite-park",
    "waldorf", "waldron", "walla-walla", "walsenburg", "walterboro", "walton", "warren", "warren-minnesota",
    "warren-ohio", "warrensburg", "warwick", "washington", "washington-illinois", "washington-indiana",
    "washington-iowa", "washington-missouri", "wasilla", "waterbury", "waterford", "watertown-new-york",
    "waterville", "watford-city", "watsonville", "wauseon", "wauwatosa", "waycross", "wayne",
    "wayne-nebraska", "waynesboro", "waynesville", "waynesville-missouri", "weatherford",
    "weatherford-oklahoma", "weaverville", "webster", "weirton", "welch", "wellfleet", "wellington",
    "wellington-texas", "wells", "wellsboro", "wesley-chapel", "west-bend", "west-chester", "west-covina",
    "west-des-moines", "west-jefferson", "west-jordan", "west-monroe", "west-plains", "west-point",
    "west-valley-city", "west-yellowstone", "westbrook", "westcliffe", "westminster", "westons-mills",
    "wheatland", "wheaton", "white-plains", "whitehall", "whitewright", "wickenburg", "wildwood",
    "wilkes-barre", "willcox", "williamsburg", "williamsport", "willits", "willmar", "wilmington-delaware",
    "winchester", "winchester-indiana", "winchester-tennessee", "windom", "window-rock", "winfield", "wing",
    "winner", "winona", "winslow", "winston-salem", "winter-haven", "winter-park", "winthrop", "wolf-point",
    "woodbridge", "woodbury", "woodland-park", "woodward", "wooster", "worcester", "worland", "worthington",
    "wrangell", "wray", "yankton", "yazoo-city", "yelm", "yerington", "yonkers", "york", "york-nebraska",
    "youngstown", "yuba-city", "yucca-valley", "yulee", "yuma-colorado", "zanesville", "zumbrota",
  ).map(slug => s"og-$slug.jpg").toSet

  "every city, in every country" should "have the card its index page names" in {
    val absent = missing(Country.all.flatMap(_.cities).map(_.shareImage))
    // Only the first few names, or a country that was never swept prints its
    // whole roster — 546 filenames on one assertion line, in the run that
    // introduced this spec.
    withClue(s"${absent.size} cities have no committed share card; first: ") {
      absent.filterNot(awaitingFirstDeploy).take(8) shouldBe empty
    }
    withClue("cards listed as awaiting their first deploy that now exist — delete them from the list: ") {
      awaitingFirstDeploy.diff(absent.toSet) shouldBe empty
    }
  }

  "the cards" should "be the JPEGs the pages name, with no PNG left behind" in {
    // The cards were PNG until the sweep grew from 122 to 739 of them: at
    // ~810 KB each, and rewritten by every weekly refresh, PNG cost ~600 MB a
    // run against JPEG's 77 MB for the same pixels. A leftover `og-*.png` is
    // dead weight nothing serves.
    val strays = Option(cards.listFiles((_, n) => n.startsWith("og-") && n.endsWith(".png"))).toSeq.flatten.map(_.getName)
    withClue(s"${strays.size} stale PNG cards; first: ") { strays.take(8) shouldBe empty }
  }
}
