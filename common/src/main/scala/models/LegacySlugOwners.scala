package models

/**
 * TEMPORARY: preserves pre-per-country-slug resolution; drop later — drop when
 * `legacy:front_door_old_owner` (`kinowo_web_legacy_address_last_called_seconds`)
 * shows no human or app calls over 30 days.
 *
 * Before every country owned its own slugs, all of them shared one namespace and
 * each slug had exactly one country. This is that namespace — every live and
 * every renamed slug of origin/main 7fdc618ca's catalog (3,152), by the country
 * that owned it — so a bare `showtimes.cc/{slug}/` or `kinowo://{slug}` link
 * minted then still opens where it did: `york` stays York, Pennsylvania, though
 * the UK has a York page now; `kent`, `somerset`, `suffolk` stay the UK's.
 * Generated ONCE from that commit's `ios/Kinowo/catalog-seed.json` (each city's
 * `slug` and `formerSlugs`); never regenerated. Read through
 * [[Country.bareSlugTarget]]; shipped to the apps in the catalog.
 */
object LegacySlugOwners {

  private val pl: String =
    "augustow belchatow biala-podlaska bialystok bielsk-podlaski bielsko-biala bilgoraj bochnia " +
    "boleslawiec braniewo brzeg brzeg-dolny busko-zdroj bydgoszcz bytom chelm chojnice choszczno " +
    "ciechanow czechowice-dziedzice czestochowa dabrowa-gornicza dabrowa-tarnowska elblag elk gizycko " +
    "gliwice glogow gniezno goldap gorzow-wielkopolski grajewo grudziadz hrubieszow ilawa inowroclaw " +
    "jaroslaw jaslo jastrzebie-zdroj jaworzno jelenia-gora kalisz katowice kedzierzyn-kozle kepno ketrzyn " +
    "kielce klodzko kluczbork kolobrzeg konin koscierzyna koszalin krakow krapkowice krasnik krasnystaw " +
    "krosno krotoszyn krynica-zdroj kutno lapy leba legionowo legnica leszno lidzbark-warminski lipno " +
    "lodz lomza lubin lublin lubliniec lukow miechow miedzyrzec-podlaski miedzyrzecz miedzyzdroje mielec " +
    "mlawa morag myszkow naklo-nad-notecia nowa-sol nowy-sacz nowy-targ nowy-tomysl nysa olawa olecko " +
    "olesnica olsztyn olsztynek opoczno opole ostrow-mazowiecka ostrow-wielkopolski " +
    "ostrowiec-swietokrzyski oswiecim otwock pabianice parczew pila piotrkow-trybunalski plock police " +
    "polkowice poznan pruszkow przemysl przeworsk pulawy pultusk radom radomsko rawicz ruda-slaska rybnik " +
    "rypin rzeszow sanok siedlce sierpc skierniewice slawno slubice slupca slupsk sokolka " +
    "sokolow-podlaski sosnowiec srem stalowa-wola stara-blotnica starachowice stargard starogard-gdanski " +
    "strzelin suwalki swidnica swidnik swiebodzin szamotuly szczecin szczecinek szczytno tarnow tczew " +
    "tomaszow-lubelski tomaszow-mazowiecki torun trojmiasto turek tychy wadowice wagrowiec walbrzych " +
    "warszawa wejherowo wielen wieliczka wielun wloclawek wlodawa wloszczowa wolomin wroclaw " +
    "wysokie-mazowieckie wyszkow zabrze zakopane zamosc zary zawiercie zdunska-wola zgierz zgorzelec " +
    "zielona-gora zlocieniec zlotow zory zyrardow zywiec"

  private val uk: String =
    "aberdeenshire antrim armagh ayrshire-and-arran bedfordshire belfast berkshire birmingham bristol " +
    "buckinghamshire cambridgeshire cardiff central-scotland cheshire clwyd cornwall county-durham " +
    "cumbria derbyshire devon dorset down dudley dumfries-and-galloway dunbartonshire-argyll-bute dyfed " +
    "east-sussex east-yorkshire edinburgh-and-lothians essex fermanagh fife glamorgan glasgow " +
    "gloucestershire guernsey gwent gwynedd hampshire herefordshire hertfordshire highlands-and-islands " +
    "isle-of-man isle-of-wight jersey kent lanarkshire lancashire leicestershire lincolnshire liverpool " +
    "london londonderry manchester north-yorkshire northamptonshire northumberland norwich " +
    "nottinghamshire oxfordshire powys renfrewshire roxburgh-ettrick-and-lauderdale sandwell shropshire " +
    "somerset south-yorkshire staffordshire suffolk surrey tayside tyne-and-wear tyrone warwickshire " +
    "west-sussex west-yorkshire wiltshire worcestershire yorkshire"

  private val de: String =
    "aachen aalen achim ahaus ahlen aichach albstadt alfeld alpirsbach alsdorf altenburg altensteig " +
    "amberg anklam annaberg-buchholz ansbach arnsberg asbach aschaffenburg asperg auerbach augsburg " +
    "aurich backnang bad-aibling bad-belzig bad-berleburg bad-doberan bad-driburg bad-fuessing " +
    "bad-hersfeld bad-kissingen bad-kreuznach bad-lobenstein bad-mergentheim bad-neuenahr-ahrweiler " +
    "bad-neustadt-an-der-saale bad-oldesloe bad-reichenhall bad-saulgau bad-schwalbach bad-toelz " +
    "bad-urach bad-vilbel bad-wildbad bad-woerishofen baden-baden bamberg baunatal bautzen bayreuth " +
    "berchtesgaden bergisch-gladbach berlin bernau bernburg bernkastel-kues besigheim " +
    "biberach-an-der-riss bielefeld bocholt bochum boeblingen bonn bottrop bramsche brandenburg " +
    "braunschweig bremen bremerhaven bruchsal brunsbuettel buchholz-in-der-nordheide buedingen buende " +
    "burg burghausen butzbach celle cham chemnitz cloppenburg coburg cochem cottbus crailsheim cuxhaven " +
    "dahme darmstadt daun deggendorf delmenhorst demmin dessau-rosslau detmold diessen-am-ammersee " +
    "dillenburg dillingen-an-der-donau dingolfing dinslaken donaueschingen dorfen dorsten dortmund " +
    "dreieich dresden duderstadt duelmen dueren duesseldorf duisburg ebersbach eberswalde-finow eckental " +
    "eggenfelden ehingen einbeck eisenach elmshorn emden emmendingen enkenbach-alsenborn " +
    "erbach-im-odenwald erding erdmannhausen erfurt erlangen eschborn eschwege espelkamp essen " +
    "esslingen-am-neckar fehmarn fellbach finsterwalde fischbach-bei-dahn flensburg forchheim frankenthal " +
    "frankfurt-am-main frankfurt-an-der-oder freiberg freiburg freudenstadt freyung friedrichsdorf " +
    "friedrichshafen fuerstenfeldbruck fuerstenwalde-an-der-spree fuerth fuessen fulda " +
    "furtwangen-im-schwarzwald garbsen garmisch-partenkirchen gelsenkirchen genthin gera germering " +
    "giessen gifhorn ginsheim-gustavsburg goeppingen goerlitz goettingen goslar gotha " +
    "grafing-bei-muenchen greifswald grevenbroich grimma gronau-westf gruenberg gruenstadt guestrow " +
    "guetersloh gummersbach hachenburg hagen halberstadt halle hamburg hameln hamm hanau hannover " +
    "hechingen heide heidelberg heidenheim heilbronn heinsberg helgoland helmstedt herford herne " +
    "herrenberg hildesheim hof hofheim-am-taunus holzkirchen holzminden homburg hoyerswerda huerth husum " +
    "ibbenbueren ilmenau ingolstadt iserlohn itzehoe jena juelich kaiserslautern karlsruhe kassel " +
    "kaufbeuren kaufering kehl kelheim kelkheim kempten-im-allgaeu kerpen kiel kirchheim-unter-teck " +
    "kitzingen kleve kloeden koblenz koeln koenigs-wusterhausen koenigsbrunn konstanz korbach krefeld " +
    "kuehlungsborn lahnstein lahr-am-schwarzwald landau-in-der-pfalz landsberg-am-lech landshut " +
    "langenfeld langenhagen langeoog lehrte leipzig letschin leutkirch-im-allgaeu leverkusen " +
    "limbach-oberfrohna limburg lindau lingen lippstadt loeningen loerrach lohr-am-main luckenwalde " +
    "ludwigsburg ludwigslust luebben luebeck luedenscheid lueneburg luenen lutherstadt-wittenberg lychen " +
    "magdeburg mainz mannheim marburg marktoberdorf marktredwitz marl mayen meiningen meissen memmingen " +
    "meppen merseburg merzig meschede minden moelln moenchengladbach moers mosbach muehlacker " +
    "muelheim-an-der-ruhr muellheim muenchen muenster murrhardt nagold naumburg neckarsulm neresheim " +
    "nettetal neu-anspach neu-ulm neubrandenburg neuburg-an-der-donau neumarkt-in-der-oberpfalz " +
    "neumuenster neunkirchen neuruppin neuss neustadt-an-der-aisch neustadt-an-der-weinstrasse " +
    "neustadt-in-holstein neustrelitz neuwied niebuell nienburg noerdlingen nonnweiler norddorf-auf-amrum " +
    "norderney nordhausen nordhorn northeim nuernberg nuertingen oberhausen oberndorf-am-neckar " +
    "oberstdorf oberviechtach oehringen offenbach-am-main offenburg oldenburg oldenburg-in-holstein " +
    "oranienburg osnabrueck osterholz-scharmbeck ottobrunn paderborn papenburg parchim passau " +
    "pfaffenhofen-an-der-ilm pforzheim pirmasens pirna plauen plettenberg potsdam preetz prenzlau prerow " +
    "prien-am-chiemsee quickborn rastatt rathenow ratingen ravensburg recklinghausen regen regensburg " +
    "remscheid rendsburg reutlingen rheine rheinfelden riedlingen riesa rietschen rinteln rodgau rostock " +
    "rotenburg-an-der-wuemme rothenburg rottenburg-am-neckar rottweil rudolstadt saalfeld saarbruecken " +
    "salzgitter salzwedel sangerhausen sassnitz schleiz schleswig schloss-holte-stukenbrock schluechtern " +
    "schmelz schongau schorndorf schwabach schwaebisch-gmuend schwaebisch-hall schwalmstadt schwandorf " +
    "schwedt schweinfurt schwerin senftenberg siegburg siegen sigmaringen simbach-am-inn sindelfingen " +
    "singen-am-hohentwiel sinsheim soest solingen sosa speyer spremberg stade stendal stralsund straubing " +
    "stuttgart suhl tauberbischofsheim taucha templin torgau traunreut traunstein trier tuebingen " +
    "tuttlingen ueberlingen ueckermuende uelzen uffenheim ulm unterschleissheim vechta versmold viersen " +
    "villingen-schwenningen vilsbiburg voelklingen wadern waldkraiburg waldshut-tiengen walsrode " +
    "wangen-im-allgaeu warburg waren-mueritz warendorf wasserburg-am-inn weiden-in-der-oberpfalz " +
    "weil-am-rhein weil-der-stadt weilheim-in-oberbayern weimar weinheim weissenburg weiterstadt werder " +
    "wermelskirchen wertheim-am-main wesenberg westerland wiesbaden wildeshausen wilhelmshaven winsen " +
    "wismar witten wittenberge wittingen wittlich wittstock witzenhausen wolfenbuettel wolfratshausen " +
    "wolfsburg worms wuerzburg wuppertal wusterhausen zeil-am-main zeitz zeven zittau zwickau"

  private val us: String =
    "aberdeen aberdeen-maryland aberdeen-mississippi abilene abingdon ada adams adrian afton agawam " +
    "aguadilla ainsworth airway-heights aitkin akron alamogordo alamosa alaska albany albany-georgia " +
    "albany-oregon albemarle albertville albion albion-nebraska albuquerque alexander-city alexandria " +
    "alexandria-bay alexandria-louisiana alexandria-virginia algona alhambra alice allen allentown " +
    "alliance alma alpena alpharetta alpine altoona-johnstown alturas altus alva amarillo amelia amenia " +
    "american-samoa ames amherst amherst-massachusetts anaconda anadarko anaheim anchorage andalusia " +
    "anderson andover andrews angels-camp ann-arbor annapolis anniston antigo antlers appleton arapahoe " +
    "ardmore arecibo arlington arlington-virginia arnold arnold-nebraska arroyo-grande arvada ashburn " +
    "asheboro asheville ashland ashland-ohio ashland-wisconsin ashtabula aspen astoria atascadero " +
    "atchison athens athens-ohio athens-tennessee athens-texas athol atlanta atlantic-city atmore atwood " +
    "auburn auburn-alabama auburn-california auburn-maine auburn-washington augusta augusta-georgia " +
    "aurora aurora-illinois aurora-nebraska austin austin-minnesota ava avalon avon avondale bad-axe " +
    "bainbridge-island baker-city bakersfield baltimore bangor bar-harbor barberton barrington barstow " +
    "bartlesville bastrop batesville baton-rouge battle-creek baudette baxter bay-city bayamon baytown " +
    "beacon beatrice beaumont beaumont-port-arthur beavercreek beaverton beckley bedford bedford-indiana " +
    "beeville belfield belle-plaine belleville bellevue bellingham bellmore beloit belton bemidji bend " +
    "benkelman bennington bensalem benson benton benton-harbor berkeley berryville bethany bethel " +
    "bethel-maine bethesda bethlehem beverly-hills bicknell big-bear-lake big-rapids big-sky big-spring " +
    "big-timber billings binghamton birmingham-alabama birmingham-michigan bishop bismarck blackduck " +
    "blacksburg bloomington bloomington-illinois bloomington-minnesota bloomsburg bluefield " +
    "bluefield-princeton blytheville boca-raton boerne boise bolingbrook bolivar bolivar-tennessee " +
    "bonita-springs bonners-ferry boone borger boscobel bossier-city boston bottineau boulder " +
    "bowling-green bowman bozeman bradenton brady brainerd-lakes brandon branson brattleboro breckenridge " +
    "bremerton brenham brentwood bridgeport bridgton brigham-city bristol-tennessee britton broken-arrow " +
    "broken-bow brookfield brookfield-wisconsin brookhaven brookings brookings-oregon brownsburg " +
    "brownsville brunswick brunswick-golden-isles bryan bryant buchanan buena-park buena-vista buffalo " +
    "buffalo-niagara buffalo-oklahoma buffalo-wyoming burbank burley burlington burlington-colorado " +
    "burlington-north-carolina burlington-vermont burlington-wisconsin burns burwell butler butte cabot " +
    "cadet cadillac calabasas calais caldwell california calumet calvert-city camarillo camas cambridge " +
    "cambridge-ohio camden cameron campbellsville canadian canby cando canon-city canton canton-illinois " +
    "cape-cod cape-coral cape-girardeau carbondale carbondale-colorado carlisle carlsbad " +
    "carlsbad-new-mexico carmel carnegie carolina carrington carroll carrollton carson-city carthage cary " +
    "casa-grande cascade casper castle-rock cavalier cedar-bluff cedar-city cedar-park cedar-rapids " +
    "celina centennial center centerville centerville-tennessee central-city central-coast " +
    "central-wyoming centralia centre centreville chadron chamberlain champaign champaign-urbana chandler " +
    "chandler-oklahoma chanute chapel-hill charles-city charleston charleston-south-carolina charlotte " +
    "charlotte-south-carolina charlottesville chattanooga cheboygan checotah chelan cherry-hill " +
    "chesapeake chester cheyenne cheyenne-laramie cheyenne-wells chicago chickasha chico chillicothe " +
    "chillicothe-missouri chilton chino chippewa-falls choteau chula-vista cicero cincinnati " +
    "cincinnati-kentucky circle clanton clarendon clarion clarion-iowa clarksburg clarksville clay-center " +
    "clayton clayton-georgia cle-elum clearfield clearlake clearwater cleveland cleveland-heights " +
    "cleveland-tennessee clifton clifton-park clinton clinton-arkansas clinton-illinois clinton-missouri " +
    "clinton-north-carolina clinton-township cloquet clovis clovis-new-mexico coalinga cody coeur-d-alene " +
    "coeur-dalene colby coldwater coldwater-kansas college-station colorado-city colorado-springs " +
    "columbia columbia-maryland columbia-mississippi columbia-missouri columbus columbus-georgia " +
    "columbus-indiana columbus-mississippi columbus-nebraska colville commerce-city concord " +
    "concord-new-hampshire concord-north-carolina concordia concrete conrad conway conway-south-carolina " +
    "cook cookeville cooperstown coos-bay copperas-cove coral-gables coral-springs cordele cordell " +
    "cordova corinth corning corning-elmira corning-iowa corona corpus-christi corsicana cortez cortland " +
    "corvallis coshocton costa-mesa coudersport council-bluffs covington craig cranberry-township " +
    "crawfordsville crescent-city crested-butte crestview crete creve-coeur crookston crosby crowley " +
    "crystal-lake cullman culpeper curtis cut-bank cypress dacula daingerfield dallas dallas-fort-worth " +
    "dallas-oregon dalton daly-city danbury danville danville-kentucky daphne davenport davie davis " +
    "dayton dayton-washington daytona-beach de-pere de-soto dearborn decatur decatur-alabama " +
    "decatur-georgia decatur-texas decorah dededo-village defiance dekalb del-rio delano delaware " +
    "dell-rapids delray-beach delta delta-utah deming demopolis denison dennis denton denver derby " +
    "des-moines destin-fort-walton-beach detroit detroit-lakes devils-lake dexter dickinson dickson " +
    "dillon district-of-columbia dodge-city dodgeville dorado dothan douglas douglas-wyoming " +
    "dover-foxcroft downey dublin dublin-georgia dubuque duluth duluth-georgia dumas duncan dunkirk " +
    "durango durant durham eads eagle-butte eagle-pass early easley east-grand-forks east-hampton " +
    "east-lansing east-stroudsburg east-tawas east-texas east-windham eastern-arizona eastern-kentucky " +
    "eastern-oregon eastern-panhandle eastern-sierra eastland easton eau-claire eden edenton edinburg " +
    "edison edmond edwards edwardsville effingham el-cajon el-centro el-dorado el-dorado-kansas " +
    "el-dorado-springs el-paso el-segundo elberton elgin elizabeth elizabeth-city elizabethtown elk-city " +
    "elk-grove elk-rapids elkader elkhart elkins elko ellensburg ellijay elroy ely ely-minnesota " +
    "emeryville emmett emporia ennis enterprise ephraim erie escanaba escondido estes-park eugene eunice " +
    "eureka eureka-arcata eureka-montana evanston evanston-wyoming evansville everett fairbanks fairbury " +
    "fairfax fairfield fairmont fairview fajardo fall-river fallon fargo farmington farmington-maine " +
    "farmington-missouri farmville faulkton fayetteville fayetteville-arkansas fayetteville-georgia " +
    "fergus-falls findlay fish-creek fitchburg fitzgerald flagler flagstaff flint florence " +
    "florence-alabama florence-kentucky florence-oregon floresville florida-keys folsom fond-du-lac " +
    "fontana forest-city forest-grove forney forrest-city forsyth fort-bliss fort-bragg fort-collins " +
    "fort-dodge fort-lauderdale fort-lee fort-moore fort-morgan fort-myers fort-pierre fort-scott " +
    "fort-smith fort-stockton fort-wayne fort-worth fowler fox-valley framingham frankfort " +
    "frankfort-michigan franklin franklin-west-virginia frederick fredericksburg freeport fremont " +
    "fremont-nebraska fresno friday-harbor friendswood frisco front-royal frostburg fullerton fulton " +
    "gadsden gainesville gainesville-georgia gainesville-texas gaithersburg galax galesburg gallipolis " +
    "gallup galveston garden-city garden-grove garland garrett garrison gastonia gatesville gaylord " +
    "geneva geneva-nebraska georgetown georgetown-south-carolina gettysburg gilbert gilford gillette " +
    "glasgow-kentucky glasgow-montana glen-allen glen-burnie glendale glendale-california glendive globe " +
    "gold-beach goldsboro goleta gonzales gonzales-texas goodland gordon grafton graham granbury " +
    "grand-canyon-village grand-forks grand-forks-north-dakota grand-island grand-junction grand-prairie " +
    "grand-rapids grand-rapids-minnesota grandville grangeville granite-city grants grants-pass grapevine " +
    "grass-valley grayling great-barrington great-bend great-falls greece greeley green-bay " +
    "greenacres-city greencastle greeneville greenfield greensboro greensboro-georgia greensburg " +
    "greensburg-indiana greensburg-kansas greenville greenville-alabama greenville-mississippi " +
    "greenville-north-carolina greenwood greenwood-indiana gregory grenada gresham grinnell grove " +
    "grove-city grundy guam guayama guaynabo guin gulf-coast gulf-shores gulfport gunnison gunnison-utah " +
    "guymon hackettstown hadensville hagerstown haleyville hamilton hammond hampton hampton-bays " +
    "hampton-roads hampton-south-carolina hannibal hanover hanover-pennsylvania hardin hardy harlan " +
    "harlingen harlowton harrisburg harrisburg-illinois harrison harrisonburg hartford hartford-kentucky " +
    "harvey hastings hastings-michigan hattiesburg havelock havre hawaii hawaii-big-island hayward " +
    "hayward-wisconsin hazard hazen hazleton healdsburg heath heber-city heber-springs hebron " +
    "hebron-north-dakota helena henderson henderson-kentucky henderson-north-carolina hendersonville " +
    "hendersonville-north-carolina hereford hermiston hesperia hettinger hialeah hiawatha hibbing hickory " +
    "higginsville high-point highland highland-arkansas highlands hill-city hillsboro hilo hilton-head " +
    "hilton-head-island hinckley hinesville hingham hobbs holbrook holdrege holland holly holly-springs " +
    "hollywood holyoke homer homestead hondo honoka-a honolulu hood-river hoopeston hoover hopkinsville " +
    "hoquiam hornell hot-springs hot-springs-south-dakota houghton houghton-lake houlton houma houston " +
    "houston-missouri howell hudson hudson-valley hugoton humboldt huntersville huntington " +
    "huntington-beach huntington-indiana huntsville huntsville-texas huron hurricane hutchinson " +
    "hutchinson-minnesota ida-grove idabel idaho-falls imperial independence independence-kansas " +
    "indian-lake indian-trail indiana indianapolis inez inglewood inland-empire international-falls " +
    "inverness ionia iowa-city iowa-falls iron-range ironwood irvine irving issaquah ithaca jackson " +
    "jackson-california jackson-michigan jackson-mississippi jackson-ohio jackson-wyoming jacksonville " +
    "jacksonville-illinois jacksonville-north-carolina jacksonville-texas jamestown jamestown-new-york " +
    "jamestown-tennessee janesville jasper jefferson jefferson-city jeffersonville jersey-city " +
    "jerseyville jesup johnson-city johnstown johnstown-new-york joliet jonesboro jonesville joplin " +
    "julesburg juneau jupiter kahului kailua-kona kalamazoo kalispell kane kankakee kanopolis kansas-city " +
    "kansas-city-kansas kapolei katy kaunakakai kayenta kearney keene kelso kemmerer kenai " +
    "kenai-peninsula kenmare kenner kennesaw kennett kennewick kenosha kent-washington kenton keokuk " +
    "kernersville kerrville ketchum kewanee key-west kill-devil-hills killeen killeen-temple kilmarnock " +
    "kimball kingfisher kingman kingman-kansas kingston kingston-massachusetts kinsley kinston kirkland " +
    "kirksville kissimmee klamath-falls knox knoxville knoxville-iowa kodiak kokomo kutztown kyle " +
    "la-crosse la-grande la-jolla la-junta ladysmith lafayette lafayette-indiana lafayette-tennessee " +
    "lafayette-west-lafayette lagrange laguna-niguel lake-charles lake-city lake-city-colorado " +
    "lake-elsinore lake-havasu-city lake-jackson lake-of-the-ozarks lake-ozark lake-placid lakeland " +
    "lakeport lakewood lakewood-washington lamar lamar-colorado lamesa lamoni lanai-city lancaster " +
    "lancaster-california lancaster-new-hampshire lancaster-new-york lancaster-ohio lancaster-texas " +
    "lancaster-wisconsin lander langdon lansdale lansing lapeer laramie laredo largo larkspur larned " +
    "las-cruces las-vegas las-vegas-new-mexico laughlin laurel laurinburg lawrence lawrenceburg " +
    "lawrenceville lawton layton le-mars leachville lebanon lebanon-hanover lebanon-missouri lebanon-ohio " +
    "lee-s-summit leesville lehigh-valley lemmon lemoore leominster lewisburg lewisburg-west-virginia " +
    "lewiston lewistown lewistown-pennsylvania lewisville lexington lexington-nebraska " +
    "lexington-south-carolina lexington-tennessee lexington-virginia libby liberal lima limon lincoln " +
    "lincoln-city lincoln-illinois lincoln-kansas lincoln-new-hampshire linn linton lisbon litchfield " +
    "litchfield-minnesota lititz little-falls little-rock littleton littleton-colorado livermore " +
    "livingston livingston-alabama llano lockport lodi logan logan-west-virginia logansport lompoc " +
    "london-kentucky long-beach long-beach-washington long-island longmont longview lorain los-alamos " +
    "los-angeles los-banos los-lunas louisville louisville-kentucky loveland lovell lovington lowell " +
    "lubbock lucas ludington lufkin lumberton luverne lyman lynchburg macomb macon macon-missouri " +
    "madawaska madera madison madison-connecticut madison-indiana madison-minnesota madison-south-dakota " +
    "madisonville madras malta mammoth-lakes manchester-connecticut manchester-iowa " +
    "manchester-new-hampshire manhattan manistee manistique manitowoc mankato mansfield mansfield-city " +
    "maquoketa marathon marble-falls marbleton marco-island marianna maricopa marietta marinette marion " +
    "marion-illinois marion-iowa marion-north-carolina marion-virginia marksville marquette marshall " +
    "marshall-arkansas marshall-missouri marshalltown marshfield martin martinsburg martinsville " +
    "martinsville-indiana marysville marysville-kansas maryville maryville-missouri mashpee mason " +
    "mason-city massena matamoras mattituck mattoon mattydale maui maumee mayaguez mayfield maysville " +
    "mayville mcalester mcallen mcconnelsville mccook mcdonough mckinney mclean mcminnville " +
    "mcminnville-tennessee mcpherson meadville medford medford-wisconsin melbourne memphis " +
    "memphis-mississippi mena menomonie mentor merced meridian meridian-mississippi merrill mesa mesilla " +
    "mesquite mesquite-nevada metairie metaline-falls methuen mexico miami miami-beach miami-oklahoma " +
    "miamisburg michigan-city middle-river middlebury-village middlesboro middletown " +
    "middletown-connecticut middletown-delaware midland midland-odessa midland-texas midlothian milbank " +
    "milbridge miles-city milford milford-delaware milford-new-hampshire millbury milledgeville miller " +
    "millsboro milwaukee mineola mineral-wells minneapolis minneapolis-st-paul " +
    "minneapolis-st-paul-wisconsin minot mishawaka missoula missouri-city mitchell moab moberly mobile " +
    "mobridge modesto moline monett monetta monroe monroe-wisconsin monterey monterey-county montevideo " +
    "montgomery monticello monticello-arkansas montpelier montpelier-idaho montrose moore moorefield " +
    "moose-lake mora morehead morenci moreno-valley morgantown morris morristown morrisville morton " +
    "moscow moses-lake mott moultrie mount-airy mount-airy-georgia mount-ayr mount-gilead mount-lebanon " +
    "mount-pleasant mount-pleasant-iowa mount-pleasant-michigan mount-pleasant-texas mount-pleasant-utah " +
    "mount-shasta mount-sterling mount-vernon mount-vernon-illinois mount-vernon-ohio mountain-grove " +
    "mountain-home mountain-view mountain-view-arkansas muncie murfreesboro murray murrieta muscatine " +
    "muskegon muskogee myrtle-beach nantucket naperville naples nashville nashville-illinois " +
    "nashville-indiana national-city nebraska-city nederland neligh neosho nephi new-bern new-braunfels " +
    "new-brunswick new-caney new-castle new-haven new-lenox new-london new-london-wisconsin " +
    "new-martinsville new-orleans new-paltz new-philadelphia new-ulm new-york new-york-new-jersey newark " +
    "newark-delaware newberg newberry newcastle newport newport-beach newport-news newport-vermont " +
    "newport-washington newton newton-illinois newton-kansas niagara-falls nogales nome norfolk " +
    "norfolk-virginia norman north-attleborough-center north-central-arkansas north-central-ohio " +
    "north-charleston north-coast north-conway north-fort-myers north-georgia north-las-vegas " +
    "north-little-rock north-missouri north-platte north-portland north-virginia-beach north-wilkesboro " +
    "northeast-wyoming northern-california northern-michigan northfield northwest-arkansas " +
    "northwest-kansas northwest-oklahoma northwest-wisconsin norton norton-kansas norwalk " +
    "norwalk-connecticut norwood o-fallon oahu oak-bluffs oak-grove oak-harbor oak-ridge oakes oakhurst " +
    "oakland oakland-maryland oakley oberlin ocala ocean-city oceanside ocoee odessa oelwein ogallala " +
    "ogden okemah oklahoma-city olathe old-forge old-valdez olympia omaha omaha-iowa omak onancock onarga " +
    "onawa oneida oneida-tennessee oneonta ontario ontario-california ontario-ohio opelika opelousas " +
    "orange orangeburg orem orlando oro-valley orofino oroville osage oshkosh oskaloosa oswego " +
    "oswego-new-york othello ottawa ottawa-kansas ottumwa overland-park owasso owatonna owego owensboro " +
    "owings-mills oxford oxnard ozark pace paducah page page-north-dakota pageland pago-pago " +
    "pagosa-springs palestine palm-bay palm-coast palm-springs palmdale pampa panama-city panguitch " +
    "paonia paris paris-illinois park-falls park-rapids parker parkersburg parsons pasadena " +
    "pasadena-california paullina pauls-valley pawhuska payson pearl-city pearland pell-city " +
    "pembroke-pines pendleton pensacola peoria peoria-arizona perham perry perry-new-york perryton " +
    "perryville petaluma petersburg petoskey pflugerville philadelphia philadelphia-mississippi " +
    "philadelphia-new-jersey philip philipsburg phoenix piedmont pierre pikeville pinecrest pittsburg " +
    "pittsburgh pittsfield plains plano plantation platte platteville plattsburgh " +
    "plattsburgh-north-country pleasanton plentywood plymouth plymouth-indiana plymouth-michigan " +
    "pocahontas pocatello point-arena polson pompano-beach ponca-city ponce ponderay pontiac pooler " +
    "poplar-bluff port-angeles port-arthur port-charlotte port-huron port-lavaca port-saint-lucie " +
    "port-townsend portage portage-pennsylvania porterville portland portland-maine portland-texas " +
    "portland-washington portsmouth portsmouth-new-hampshire poteau potsdam-new-york pottstown " +
    "poughkeepsie powell prairie-du-chien pratt prattville prescott-valley presque-isle preston price " +
    "princeton princeton-illinois princeton-indiana prineville providence provo pryor pueblo quad-cities " +
    "queen-creek queensbury quincy quincy-california quinnesec raleigh rancho-cucamonga rancho-mirage " +
    "randolph rangeley rapid-city ravenna rawlins reading red-bank red-bank-tennessee red-bluff red-lodge " +
    "red-oak red-wing redding redfield redlands redmond reno rensselaer renton rexburg rhinelander " +
    "rhode-island rialto rice-lake richardson richfield richland-center richmond richmond-california " +
    "richmond-indiana richmond-kentucky richmond-missouri richmond-texas ridgecrest ridgeland rifle " +
    "rio-grande-city rio-grande-valley rio-rancho ripley ritzville riverside riverton riverview roanoke " +
    "roanoke-rapids robinson rochester rochester-minnesota rock-hill rock-springs rockford rockland " +
    "rocky-mount rocky-mount-wilson rogers rogers-city rolla rolla-north-dakota rome rome-georgia " +
    "roosevelt roseau roseburg roseville roswell roswell-georgia rothschild round-rock roxboro rugby " +
    "ruidoso rushville russell russellville ruston saco sacramento safford saginaw saginaw-bay-city " +
    "saguache saint-charles saint-clairsville saint-croix saint-croix-falls saint-francis saint-george " +
    "saint-helens saint-johns saint-joseph saint-marys saint-michael saint-paul salem salem-massachusetts " +
    "salem-ohio salina salinas salisbury salmon salt-lake-city san-angelo san-antonio san-bernardino " +
    "san-diego san-francisco san-francisco-bay-area san-jose san-juan san-luis-obispo san-marcos " +
    "san-marcos-texas san-mateo san-ramon sandusky sandusky-michigan sandy sanford sanford-maine " +
    "sanpete-valley santa-ana santa-barbara santa-clara santa-clarita santa-cruz santa-fe santa-maria " +
    "santa-monica santa-rosa santa-rosa-new-mexico sapulpa sarasota sarasota-bradenton savannah " +
    "savannah-tennessee sayre sayville schenectady schererville schertz schroon-lake scobey scottsbluff " +
    "scottsboro scottsdale scranton scranton-wilkes-barre searcy seaside seattle sebring sedalia sedan " +
    "sedona selinsgrove selma selma-alabama seminole senatobia seneca seneca-kansas sevierville seward " +
    "seymour shallotte shamrock sharon-springs shattuck shawano shawnee shawnee-kansas sheboygan shelbina " +
    "shelby shelby-montana shelbyville shelbyville-illinois shelbyville-tennessee shelton " +
    "shenandoah-valley sheridan sherman show-low shreveport sidney sidney-montana sidney-nebraska " +
    "sierra-foothills sierra-vista sikeston silver-city silver-spring silverton simi-valley simpsonville " +
    "sioux-center sioux-city sioux-falls siren sisters sitka skowhegan slidell smith-center smithfield " +
    "snyder socorro soda-springs soledad somerset-kentucky somerville sonora south-barrington south-bend " +
    "south-bend-elkhart south-boston south-fallsburg south-jordan south-kingstown south-taft " +
    "southeast-alaska southeast-kansas southern-illinois southern-pines southport southwest-kansas " +
    "southwest-michigan southwest-minnesota southwest-virginia southwest-washington southwest-wisconsin " +
    "southwest-wyoming space-coast spanish-fork sparks sparta sparta-wisconsin spartanburg spearfish " +
    "spearman spencer spencer-indiana spencer-west-virginia spirit-lake spokane spring spring-hill " +
    "spring-hill-tennessee spring-valley springerville springfield springfield-colorado " +
    "springfield-georgia springfield-massachusetts springfield-missouri springfield-ohio " +
    "springfield-oregon springfield-vermont springhill springville st-cloud st-johns st-johnsbury " +
    "st-louis st-louis-illinois st-petersburg stafford stamford stamford-texas stanley star-valley starke " +
    "starkville state-college statesboro steamboat-springs stephenville sterling sterling-heights " +
    "sterling-illinois stevens-point stigler stillwater stockton stockton-california stony-brook " +
    "storm-lake stowe strongsville stuart stuart-florida sturgeon-bay sturgis stuttgart-arkansas suffern " +
    "suffolk-virginia sullivan sulphur-springs summerlin-south summersville summerville sumter " +
    "sun-prairie sunnyside sunnyvale sunrise superior surprise susanville sutton syracuse syracuse-kansas " +
    "tabor-city tacoma tahlequah tallahassee tampa tampa-bay taos tavernier taylorville teays-valley " +
    "tehachapi telluride temecula tempe terre-haute texarkana the-colony the-villages the-woodlands " +
    "thermopolis thief-river-falls thomaston thomasville thompson-falls thomson thousand-oaks " +
    "three-rivers tiffin tigard tillamook tioga tiptonville titusville toledo tomball toms-river tonkawa " +
    "tooele topeka torrance torrington torrington-wyoming towanda tracy traverse-city treasure-coast " +
    "trenton tri-cities tri-cities-tennessee triad triangle tribune trinidad troy truckee " +
    "truth-or-consequences tuba-city tucson tucumcari tulare tulsa tupelo tupper-lake tuscaloosa tutu " +
    "twentynine-palms twin-falls two-harbors tyler uinta-basin ukiah ulysses uniontown universal-city " +
    "upper-peninsula utica utica-rome uvalde vacaville valdez valdosta vallejo valley-city valparaiso " +
    "van-buren van-wert vancouver vandalia vandergrift vashon vassar venice ventura vermillion vermont " +
    "vernal vernon vero-beach viborg vicksburg victor victoria victorville vidalia vienna-bend vincennes " +
    "vineland vineland-minnesota vinita virgin-islands virginia virginia-beach virginia-eastern-shore " +
    "viroqua visalia vista wabash waco waconia wadena wagner waikoloa waimea waite-park waldorf waldron " +
    "walla-walla walsenburg walterboro walton warner-robins warren warren-minnesota warren-ohio " +
    "warrensburg warwick washington washington-dc washington-dc-maryland washington-illinois " +
    "washington-indiana washington-iowa washington-missouri wasilla waterbury waterford waterloo " +
    "watertown watertown-new-york watertown-north-country waterville watford-city watsonville " +
    "wausau-stevens-point wauseon wauwatosa waycross wayne wayne-nebraska waynesboro waynesville " +
    "waynesville-missouri weatherford weatherford-oklahoma weaverville webster weirton welch wellfleet " +
    "wellington wellington-texas wells wellsboro wenatchee wesley-chapel west-bend west-branch " +
    "west-central-minnesota west-chester west-covina west-des-moines west-jefferson west-jordan " +
    "west-monroe west-palm-beach west-plains west-point west-texas west-valley-city west-yellowstone " +
    "westbrook westcliffe western-maryland western-massachusetts western-nc-mountains western-pa " +
    "westminster westminster-maryland westons-mills wheatland wheaton white-plains whitehall whitewright " +
    "wichita wichita-falls wickenburg wildwood wilkes-barre willamette-valley willcox williamsburg " +
    "williamsport williston willits willmar wilmington wilmington-delaware winchester winchester-indiana " +
    "winchester-tennessee windom window-rock winfield wing winnemucca winner winona winslow winston-salem " +
    "winter-haven winter-park winthrop wolf-point woodbridge woodbury woodland-park woodward wooster " +
    "worcester worland worthington wrangell wray wytheville yakima yankton yazoo-city yellowstone-gateway " +
    "yelm yerington yonkers york york-nebraska youngstown yuba-city yucca-valley yulee yuma yuma-colorado " +
    "zanesville zumbrota"

  private val es: String =
    "a-coruna adeje aguilar-de-campoo aguilas alava albacete alcala-de-henares alcala-de-xivert " +
    "alcala-la-real alcaniz alcazar-de-san-juan alcobendas alcorcon alcoy algeciras alhaurin-el-grande " +
    "alicante almazan almendralejo almeria alzira amposta andujar antequera aranda-de-duero " +
    "arcos-de-la-frontera arenas-de-san-pedro arrecife arroyo-de-la-encomienda astorga asturias avila " +
    "ayamonte badajoz badalona barakaldo barbastro barbate barcelona baza beasain bejar benidorm berga " +
    "bilbao binefar blanes boltana bunol burgo-de-osma burgos caceres cadiz calahorra calatayud calpe " +
    "camargo cantabria carballo cartagena castellon castellon-de-la-plana cee ceuta ciudad-real " +
    "ciudad-rodrigo ciutadella-de-menorca cordoba coria cornella-de-llobregat cortegana " +
    "corvera-de-asturias coslada cuenca daimiel don-benito dos-hermanas ecija eibar el-ejido " +
    "el-pont-de-suert el-puerto-de-santa-maria el-vendrell elche estella-lizarra estepa ferrol figueres " +
    "fuengirola fuenlabrada galdakao gandia getafe gijon girona golmayo granada granollers guadalajara " +
    "guardo guipuzcoa herrera-del-duque huarte huelva huesca huetor-tajar ibiza iniesta irun " +
    "islas-baleares jaen jaraiz-de-la-vera javea jerez-de-la-frontera l-hospitalet-de-llobregat " +
    "la-orotava la-palma-del-condado la-rioja la-seu-d-urgell la-zubia laredo-cantabria las-palmas " +
    "las-palmas-de-gran-canaria leganes leiro leon lerida linares lleida logrono lorca " +
    "los-llanos-de-aridane lucena lugo madrid mairena-del-aljarafe majadahonda malaga manacor manresa mao " +
    "marbella marchena marratxi martos mazarron medina-de-rioseco medina-del-campo melilla mequinenza " +
    "merida miranda-de-ebro molina-de-segura mollerussa monforte-de-lemos mostoles motril mungia murcia " +
    "navalmoral-de-la-mata navarra navia oliva olot orihuela ourense oviedo palafrugell palencia " +
    "palma-de-mallorca pamplona paterna pedrajas-de-san-esteban penaranda-de-bracamonte " +
    "penarroya-pueblonuevo petrer pilar-de-la-horadada plasencia ponferrada pontevedra pozoblanco " +
    "premia-de-mar puerto-del-rosario puertollano requena reus ribadeo ronda roquetas-de-mar rota " +
    "sabadell sabinanigo sagunto salamanca san-cristobal-de-la-laguna san-fernando san-javier " +
    "san-martin-de-valdeiglesias san-sebastian san-vicente-del-raspeig sant-boi-de-llobregat " +
    "sant-cugat-del-valles santa-cruz-de-tenerife santa-maria-del-paramo santa-marta-de-tormes santander " +
    "santiago-de-compostela segovia sevilla siero sitges solsona soria talavera-de-la-reina tarancon " +
    "tarragona tarrega telde terrassa teruel toledo-castilla-la-mancha tomelloso torrelodones totana " +
    "tremp tudela ubrique valdemorillo valdemoro valdepenas valencia valladolid velez-malaga viana vic " +
    "vielha vigo vila-real vilafranca-del-penedes vilagarcia-de-arousa villablino villarrobledo " +
    "villaviciosa-de-odon villena vinaros vitoria-gasteiz viveiro vizcaya xinzo-de-limia zamora zaragoza " +
    "zuera zumaia"

  /** Slug → the code of the country that owned it before slugs were per country. */
  val owners: Map[String, String] =
    Seq("pl" -> pl, "uk" -> uk, "de" -> de, "us" -> us, "es" -> es)
      .flatMap { case (code, slugs) => slugs.split(' ').map(_ -> code) }.toMap
}
