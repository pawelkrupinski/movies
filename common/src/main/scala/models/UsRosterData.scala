// GENERATED from data/us/pages.json by data/us/scripts/generate_roster.py
// — do NOT edit by hand. Full US cinema roster: 2,001 pages over 55 states and
// territories / 5,009 cinemas (Flicks, www.flicks.us). Regenerate after re-harvesting or
// re-clustering; see data/us/README.md.
package models

private[models] object UsRosterData {
  // (displayName, pillName, flicks cinema slug, the venue's state, the venue's own town)
  type C = (String, String, String, String, String)
  // (slug, slug qualified with its state, name, state, lat, lon, zoneId, multiTown, towns, cinemas)
  type R = (String, String, String, String, Double, Double, String, Boolean, Seq[String], Seq[C])

  private def p_new_york: R = ("new-york", "new-york-new-york", "New York", "New York", 40.7143, -74.006, "America/New_York", false, Seq("New York"), Seq(
    ("Alamo Drafthouse Brooklyn", "Alamo Drafthouse Brooklyn", "alamo-drafthouse-brooklyn", "New York", "Brooklyn"),
    ("Alamo Drafthouse Lower Manhattan", "Alamo Drafthouse Lower Manhattan", "alamo-drafthouse-lower-manhattan", "New York", "New York"),
    ("Alamo Drafthouse Staten Island", "Alamo Drafthouse Staten Island", "alamo-drafthouse-staten-island", "New York", "Staten Island"),
    ("Alpine Cinema Brooklyn", "Alpine Cinema Brooklyn", "alpine-cinema-brooklyn", "New York", "Brooklyn"),
    ("AMC 19th St. East 6", "AMC 19th St. East 6", "amc-19th-st-east-6", "New York", "New York"),
    ("AMC 34th Street 14", "AMC 34th Street 14", "amc-34th-street-14", "New York", "New York"),
    ("AMC 84th Street 6", "AMC 84th Street 6", "amc-84th-street-6", "New York", "New York"),
    ("AMC Bay Plaza 13 Bronx", "AMC Bay Plaza 13 Bronx", "amc-bay-plaza-13-bronx", "New York", "The Bronx"),
    ("AMC Bay Terrace 6", "AMC Bay Terrace 6", "amc-bay-terrace-6", "New York", "Bayside"),
    ("AMC Empire 25", "AMC Empire 25", "amc-empire-25", "New York", "New York"),
    ("AMC Fresh Meadows 7", "AMC Fresh Meadows 7", "amc-fresh-meadows-7", "New York", "Fresh Meadows"),
    ("AMC Kips Bay 15 New York", "AMC Kips Bay 15 New York", "amc-kips-bay-15-new-york", "New York", "New York"),
    ("AMC Lincoln Square 13", "AMC Lincoln Square 13", "amc-lincoln-square-13", "New York", "New York"),
    ("AMC Magic Johnson Harlem 9", "AMC Magic Johnson Harlem 9", "amc-magic-johnson-harlem-9", "New York", "New York"),
    ("AMC Orpheum 7", "AMC Orpheum 7", "amc-orpheum-7", "New York", "New York"),
    ("AMC Staten Island Mall 11", "AMC Staten Island Mall 11", "amc-staten-island-mall-11", "New York", "Staten Island"),
    ("AMC Village 7", "AMC Village 7", "amc-village-7", "New York", "New York"),
    ("Angelika Film Center New York", "Angelika Film Center New York", "angelika-film-center-new-york", "New York", "New York"),
    ("Anthology Film Archives New York", "Anthology Film Archives New York", "anthology-film-archives-new-york", "New York", "New York"),
    ("Atrium Stadium Cinema Staten Island", "Atrium Stadium Cinema Staten Island", "atrium-stadium-cinema-staten-island", "New York", "Staten Island"),
    ("BAM Rose Cinemas", "BAM Rose Cinemas", "bam-rose-cinemas", "New York", "Brooklyn"),
    ("Cinema 123 by Angelika New York", "Cinema 123 by Angelika New York", "cinema-123-by-angelika-new-york", "New York", "New York"),
    ("Cinema Village New York", "Cinema Village New York", "cinema-village-new-york", "New York", "New York"),
    ("Cinemart Cinemas Forest Hills", "Cinemart Cinemas Forest Hills", "cinemart-theatre-forest-hills-5", "New York", "Forest Hills"),
    ("Cobble Hill Cinema Brooklyn", "Cobble Hill Cinema Brooklyn", "cobble-hill-cinema-brooklyn", "New York", "Brooklyn"),
    ("Film at Lincoln Center (Elinor Bunin Munroe Film Center)", "Film at Lincoln Center (Elinor Bunin Munroe Film Center)", "film-at-lincoln-center-elinor-bunin-munroe-film-center", "New York", "New York"),
    ("Film Forum New York", "Film Forum New York", "film-forum-new-york", "New York", "New York"),
    ("IFC Center New York", "IFC Center New York", "ifc-center-new-york", "New York", "New York"),
    ("IPIC Cinemas Fulton Market", "IPIC Cinemas Fulton Market", "ipic-cinemas-fulton-market", "New York", "New York"),
    ("Kent Triplex Theater Brooklyn", "Kent Triplex Theater Brooklyn", "kent-triplex-theater-brooklyn", "New York", "Brooklyn"),
    ("Kew Gardens Cinemas", "Kew Gardens Cinemas", "kew-gardens-cinemas", "New York", "Kew Gardens"),
    ("Metro Prive Cinema Chelsea", "Metro Prive Cinema Chelsea", "metro-prive-cinema-chelsea", "New York", "New York"),
    ("Metrograph Cinemas New York", "Metrograph Cinemas New York", "metrograph-cinemas-new-york", "New York", "New York"),
    ("MoMA Screening Rooms", "MoMA Screening Rooms", "moma-screening-rooms", "New York", "New York"),
    ("Museum of the Moving Image Astoria", "Museum of the Moving Image Astoria", "museum-of-the-moving-image-astoria", "New York", "Astoria"),
    ("New Plaza Cinema New York", "New Plaza Cinema New York", "new-plaza-cinema-new-york", "New York", "New York"),
    ("Nitehawk Cinema Williamsburg", "Nitehawk Cinema Williamsburg", "nitehawk-cinema-williamsburg", "New York", "Brooklyn"),
    ("Nitehawk Prospect Park", "Nitehawk Prospect Park", "nitehawk-prospect-park", "New York", "Brooklyn"),
    ("Paris Theater New York", "Paris Theater New York", "paris-theater-new-york", "New York", "New York"),
    ("Quad Cinema New York", "Quad Cinema New York", "quad-cinema-new-york", "New York", "New York"),
    ("Regal Atlas Park Glendale", "Regal Atlas Park Glendale", "regal-atlas-park-glendale", "New York", "Glendale"),
    ("Regal Battery Park", "Regal Battery Park", "regal-battery-park", "New York", "New York"),
    ("Regal Bricktown Charleston", "Regal Bricktown Charleston", "regal-bricktown-charleston", "New York", "Staten Island"),
    ("Regal Concourse", "Regal Concourse", "regal-concourse", "New York", "The Bronx"),
    ("Regal Essex Crossing New York", "Regal Essex Crossing New York", "regal-essex-crossing-new-york", "New York", "New York"),
    ("Regal Tangram", "Regal Tangram", "regal-tangram", "New York", "Flushing"),
    ("Regal Times Square New York", "Regal Times Square New York", "regal-times-square-new-york", "New York", "New York"),
    ("Regal UA Kaufman Astoria", "Regal UA Kaufman Astoria", "regal-ua-kaufman-astoria", "New York", "Long Island City"),
    ("Regal UA Midway", "Regal UA Midway", "regal-midway-stadium-forest-hills", "New York", "Forest Hills"),
    ("Regal UA Sheepshead Bay", "Regal UA Sheepshead Bay", "regal-ua-sheepshead-bay", "New York", "Brooklyn"),
    ("Regal Union Square New York", "Regal Union Square New York", "regal-union-square-new-york", "New York", "New York"),
    ("Rooftop Cinema New York", "Rooftop Cinema New York", "rooftop-cinema-new-york", "New York", "New York"),
    ("Stuart Cinema Brooklyn", "Stuart Cinema Brooklyn", "stuart-cinema-brooklyn", "New York", "Brooklyn"),
    ("Syndicated Bar & Theatre", "Syndicated Bar & Theatre", "syndicated-bar-theatre", "New York", "Brooklyn"),
    ("Tribeca Cinema New York", "Tribeca Cinema New York", "tribeca-cinema-new-york", "New York", "New York"),
    ("Village East by Angelika", "Village East by Angelika", "village-east-by-angelika", "New York", "New York"),
    ("Williamsburg Cinemas Brooklyn", "Williamsburg Cinemas Brooklyn", "williamsburg-cinemas-brooklyn", "New York", "Brooklyn"),
    ("92nd Street Cinema New York", "92nd Street Cinema New York", "92nd-street-cinema-new-york", "New York", "New York"),
    ("Firehouse Cinema New York", "Firehouse Cinema New York", "firehouse-cinema-new-york", "New York", "New York"),
    ("Low Cinema", "Low Cinema", "low-cinema", "New York", "Ridgewood"),
    ("Neue Galerie New York", "Neue Galerie New York", "neue-galerie-new-york", "New York", "New York"),
    ("North Shore Towers Cinema", "North Shore Towers Cinema", "north-shore-towers-cinema", "New York", "Queens"),
    ("Roxy Cinema New York", "Roxy Cinema New York", "roxy-cinema-new-york", "New York", "New York"),
    ("Skyline Drive-In NYC", "Skyline Drive-In NYC", "skyline-drive-in-nyc", "New York", "Brooklyn"),
    ("Symphony Space", "Symphony Space", "symphony-space", "New York", "New York"),
    ("United Palace New York", "United Palace New York", "united-palace-new-york", "New York", "New York"),
    ("West End Theatre", "West End Theatre", "west-end-theatre", "New York", "New York")
  ))
  private def p_los_angeles: R = ("los-angeles", "los-angeles-california", "Los Angeles", "California", 34.0522, -118.2437, "America/Los_Angeles", false, Seq("Los Angeles"), Seq(
    ("Alamo Drafthouse Downtown LA", "Alamo Drafthouse Downtown LA", "alamo-drafthouse-downtown-la", "California", "Los Angeles"),
    ("AMC Century City 15", "AMC Century City 15", "amc-century-city-15", "California", "Century City"),
    ("AMC DINE-IN Topanga 12", "AMC DINE-IN Topanga 12", "amc-dine-in-topanga-12", "California", "Canoga Park"),
    ("AMC Fallbrook 7", "AMC Fallbrook 7", "amc-fallbrook-7", "California", "West Hills"),
    ("AMC Northridge 10", "AMC Northridge 10", "amc-northridge-10", "California", "Northridge"),
    ("AMC Porter Ranch 9", "AMC Porter Ranch 9", "amc-porter-ranch-9", "California", "Porter Ranch"),
    ("AMC The Grove 14", "AMC The Grove 14", "amc-the-grove-14", "California", "Los Angeles"),
    ("Brain Dead Studios Fairfax", "Brain Dead Studios Fairfax", "brain-dead-studios-fairfax", "California", "Los Angeles"),
    ("Chinese Theatre Hollywood", "Chinese Theatre Hollywood", "chinese-theatre-hollywood", "California", "Hollywood"),
    ("Cinemark Howard Hughes Los Angeles", "Cinemark Howard Hughes Los Angeles", "cinemark-howard-hughes-los-angeles", "California", "Los Angeles"),
    ("Cinemark North Hollywood", "Cinemark North Hollywood", "cinemark-north-hollywood", "California", "North Hollywood"),
    ("Cinemark Playa Vista", "Cinemark Playa Vista", "cinemark-playa-vista", "California", "Los Angeles"),
    ("Egyptian Theatre Los Angeles", "Egyptian Theatre Los Angeles", "egyptian-theatre-los-angeles", "California", "Los Angeles"),
    ("El Capitan Theatre Hollywood", "El Capitan Theatre Hollywood", "el-capitan-theatre-hollywood", "California", "Hollywood"),
    ("IPIC Westwood", "IPIC Westwood", "ipic-westwood", "California", "Los Angeles"),
    ("Laemmle NoHo 7", "Laemmle NoHo 7", "laemmle-noho-7", "California", "North Hollywood"),
    ("Laemmle Royal", "Laemmle Royal", "laemmle-royal", "California", "Los Angeles"),
    ("Laemmle Town Center 5", "Laemmle Town Center 5", "laemmle-town-center-5", "California", "Encino"),
    ("Landmark Nuart Theatre", "Landmark Nuart Theatre", "landmark-nuart-theatre", "California", "Los Angeles"),
    ("Los Feliz 3 Theatre", "Los Feliz 3 Theatre", "los-feliz-3-theatre", "California", "Los Angeles"),
    ("New Beverly Cinema", "New Beverly Cinema", "new-beverly-cinema", "California", "Los Angeles"),
    ("Regal LA Live", "Regal LA Live", "regal-la-live", "California", "Los Angeles"),
    ("Regal North Hollywood", "Regal North Hollywood", "regal-north-hollywood", "California", "North Hollywood"),
    ("Regal Sherman Oaks", "Regal Sherman Oaks", "regal-sherman-oaks", "California", "Sherman Oaks"),
    ("Regency Cinema Granada Hills 9", "Regency Cinema Granada Hills 9", "regency-cinema-granada-hills-9", "California", "Granada Hills"),
    ("Regency Koreatown", "Regency Koreatown", "regency-koreatown", "California", "Los Angeles"),
    ("Regency Van Nuys Plant 16", "Regency Van Nuys Plant 16", "regency-van-nuys-plant-16", "California", "Van Nuys"),
    ("Rooftop Cinema Los Angeles", "Rooftop Cinema Los Angeles", "rooftop-cinema-los-angeles", "California", "Los Angeles"),
    ("The Landmark Westwood", "The Landmark Westwood", "the-landmark-westwood", "California", "Los Angeles"),
    ("VIDIOTS Los Angeles", "VIDIOTS Los Angeles", "vidiots-los-angeles", "California", "Los Angeles"),
    ("Vista Theater Hollywood", "Vista Theater Hollywood", "vista-theater-hollywood", "California", "Los Angeles"),
    ("Bay Theater 5", "Bay Theater 5", "bay-theater-5", "California", "Pacific Palisades"),
    ("Billy Wilder Theater at the Hammer Museum", "Billy Wilder Theater at the Hammer Museum", "billy-wilder-theater-at-the-hammer-museum", "California", "Los Angeles"),
    ("Cinelounge Sunset Hollywood", "Cinelounge Sunset Hollywood", "cinelounge-sunset-hollywood", "California", "Los Angeles"),
    ("Hollywood Legion Theatre", "Hollywood Legion Theatre", "hollywood-legion-theatre", "California", "Los Angeles"),
    ("Linwood Dunn Theatre Los Angeles", "Linwood Dunn Theatre Los Angeles", "linwood-dunn-theatre-los-angeles", "California", "Los Angeles"),
    ("Montalbán Theatre", "Montalbán Theatre", "montalbn-theatre", "California", "Hollywood"),
    ("Now Instant Image Hall", "Now Instant Image Hall", "now-instant-image-hall", "California", "Los Angeles"),
    ("Panavision screening room", "Panavision screening room", "panavision-screening-room", "California", "Woodland Hills"),
    ("Wells Fargo Theater", "Wells Fargo Theater", "wells-fargo-theater", "California", "Los Angeles")
  ))
  private def p_chicago: R = ("chicago", "chicago-illinois", "Chicago", "Illinois", 41.85, -87.65, "America/Chicago", false, Seq("Chicago"), Seq(
    ("400 Theater", "400 Theater", "400-theater", "Illinois", "Chicago"),
    ("ACX Harper Theater Hyde Park", "ACX Harper Theater Hyde Park", "acx-harper-theater-hyde-park", "Illinois", "Chicago"),
    ("Alamo Drafthouse Wrigleyville", "Alamo Drafthouse Wrigleyville", "alamo-drafthouse-wrigleyville", "Illinois", "Chicago"),
    ("AMC 600 North Michigan 9", "AMC 600 North Michigan 9", "amc-600-north-michigan-9", "Illinois", "Chicago"),
    ("AMC DINE-IN Block 37 Chicago", "AMC DINE-IN Block 37 Chicago", "amc-dine-in-block-37-chicago", "Illinois", "Chicago"),
    ("AMC Ford City 14 Chicago", "AMC Ford City 14 Chicago", "amc-ford-city-14-chicago", "Illinois", "Chicago"),
    ("AMC Galewood Crossings 14", "AMC Galewood Crossings 14", "amc-galewood-crossings-14", "Illinois", "Chicago"),
    ("AMC Newcity 14 Chicago", "AMC Newcity 14 Chicago", "amc-newcity-14-chicago", "Illinois", "Chicago"),
    ("AMC River East Chicago 21", "AMC River East Chicago 21", "amc-river-east-chicago-21", "Illinois", "Chicago"),
    ("AMC Roosevelt Collection 16", "AMC Roosevelt Collection 16", "amc-roosevelt-collection-16", "Illinois", "Chicago"),
    ("Davis Theater Chicago", "Davis Theater Chicago", "davis-theater-chicago", "Illinois", "Chicago"),
    ("Facets Cinematheque", "Facets Cinematheque", "facets-cinematheque", "Illinois", "Chicago"),
    ("Gene Siskel Film Center Chicago", "Gene Siskel Film Center Chicago", "gene-siskel-film-center-chicago", "Illinois", "Chicago"),
    ("Landmark Century Centre Chicago", "Landmark Century Centre Chicago", "landmark-century-centre-chicago", "Illinois", "Chicago"),
    ("Logan Theatre Chicago", "Logan Theatre Chicago", "logan-theatre-chicago", "Illinois", "Chicago"),
    ("Music Box Theatre Chicago", "Music Box Theatre Chicago", "music-box-theatre-chicago", "Illinois", "Chicago"),
    ("Regal City North", "Regal City North", "regal-city-north", "Illinois", "Chicago"),
    ("Regal Webster Place Chicago", "Regal Webster Place Chicago", "regal-webster-place-chicago", "Illinois", "Chicago"),
    ("Uprising Theater and Cafe", "Uprising Theater and Cafe", "uprising-theater-and-cafe", "Illinois", "Chicago"),
    ("Chicago Film Society", "Chicago Film Society", "chicago-film-society", "Illinois", "Chicago"),
    ("Doc Films – University of Chicago", "Doc Films – University of Chicago", "doc-films---university-of-chicago", "Illinois", "Chicago"),
    ("Rooftop Cinema Chicago", "Rooftop Cinema Chicago", "rooftop-cinema-chicago", "Illinois", "Chicago")
  ))
  private def p_houston: R = ("houston", "houston-texas", "Houston", "Texas", 29.7633, -95.3633, "America/Chicago", false, Seq("Houston"), Seq(
    ("AMC Gulf Pointe 30 Huston", "AMC Gulf Pointe 30 Huston", "amc-gulf-pointe-30-huston", "Texas", "Houston"),
    ("AMC Houston 8", "AMC Houston 8", "amc-houston-8", "Texas", "Houston"),
    ("AMC Willowbrook Houston 24", "AMC Willowbrook Houston 24", "amc-willowbrook-houston-24", "Texas", "Houston"),
    ("America Cinemas Sharpstown Center Houston", "America Cinemas Sharpstown Center Houston", "america-cinemas-sharpstown-center-houston", "Texas", "Houston"),
    ("Cinemark Memorial City", "Cinemark Memorial City", "cinemark-memorial-city", "Texas", "Houston"),
    ("Cinemark Tinseltown Houston 290", "Cinemark Tinseltown Houston 290", "cinemark-tinseltown-houston-290", "Texas", "Houston"),
    ("IPIC Houston", "IPIC Houston", "ipic-houston", "Texas", "Houston"),
    ("Moonstruck Drive-In Houston", "Moonstruck Drive-In Houston", "moonstruck-drive-in-houston", "Texas", "Houston"),
    ("Regal Edwards Houston Marq'E", "Regal Edwards Houston Marq'E", "regal-edwards-houston-marqe", "Texas", "Houston"),
    ("Regal Greenway Grand Palace Houston", "Regal Greenway Grand Palace Houston", "regal-greenway-grand-palace-houston", "Texas", "Houston"),
    ("River Oaks Theatre Houston", "River Oaks Theatre Houston", "river-oaks-theatre-houston", "Texas", "Houston"),
    ("Rooftop Cinema Houston", "Rooftop Cinema Houston", "rooftop-cinema-houston", "Texas", "Houston"),
    ("Showbiz Cinemas Fall Creek", "Showbiz Cinemas Fall Creek", "showbiz-cinemas-fall-creek", "Texas", "Houston"),
    ("Showbiz Cinemas Liberty Lakes Houston", "Showbiz Cinemas Liberty Lakes Houston", "showbiz-cinemas-liberty-lakes-houston", "Texas", "Houston"),
    ("Star Cinema Grill CITYCENTRE Houston", "Star Cinema Grill CITYCENTRE Houston", "star-cinema-grill-citycentre-houston", "Texas", "Houston"),
    ("Star Cinema Grill Vintage Park", "Star Cinema Grill Vintage Park", "star-cinema-grill-vintage-park", "Texas", "Houston"),
    ("Studio Movie Grill Pearland", "Studio Movie Grill Pearland", "studio-movie-grill-pearland", "Texas", "Houston"),
    ("Wortham Giant Screen Theatre", "Wortham Giant Screen Theatre", "wortham-giant-screen-theatre", "Texas", "Houston"),
    ("Brown Auditorium Theatre Houston", "Brown Auditorium Theatre Houston", "brown-auditorium-theatre-houston", "Texas", "Houston"),
    ("Lynn Watt Theatre Houston", "Lynn Watt Theatre Houston", "lynn-watt-theatre-houston", "Texas", "Houston"),
    ("Matchbox 2 Houston", "Matchbox 2 Houston", "matchbox-2-houston", "Texas", "Houston"),
    ("Rice Media Center", "Rice Media Center", "rice-media-center", "Texas", "Houston")
  ))
  private def p_phoenix: R = ("phoenix", "phoenix-arizona", "Phoenix", "Arizona", 33.4484, -112.074, "America/Phoenix", false, Seq("Phoenix"), Seq(
    ("AMC Ahwatukee 24", "AMC Ahwatukee 24", "amc-ahwatukee-24", "Arizona", "Phoenix"),
    ("AMC Arizona Center 14", "AMC Arizona Center 14", "amc-arizona-center-14", "Arizona", "Phoenix"),
    ("AMC Deer Valley 17", "AMC Deer Valley 17", "amc-deer-valley-17", "Arizona", "Phoenix"),
    ("AMC DINE-IN Desert Ridge 18", "AMC DINE-IN Desert Ridge 18", "amc-dine-in-desert-ridge-18", "Arizona", "Phoenix"),
    ("AMC DINE-IN Esplanade 14", "AMC DINE-IN Esplanade 14", "amc-dine-in-esplanade-14", "Arizona", "Phoenix"),
    ("America Cinemas Phoenix", "America Cinemas Phoenix", "america-cinemas-phoenix", "Arizona", "Phoenix"),
    ("Harkins Christown 14", "Harkins Christown 14", "harkins-christown-14", "Arizona", "Phoenix"),
    ("Harkins Norterra 14", "Harkins Norterra 14", "harkins-norterra-14", "Arizona", "Phoenix"),
    ("Harkins Scottsdale 101", "Harkins Scottsdale 101", "harkins-scottsdale-101", "Arizona", "Phoenix")
  ))
  private def p_philadelphia: R = ("philadelphia", "philadelphia-pennsylvania", "Philadelphia", "Pennsylvania", 39.9524, -75.1636, "America/New_York", false, Seq("Philadelphia"), Seq(
    ("AMC Broadstreet 7 Philadelphia", "AMC Broadstreet 7 Philadelphia", "amc-broadstreet-7-philadelphia", "Pennsylvania", "Philadelphia"),
    ("AMC DINE-IN Fashion District 8", "AMC DINE-IN Fashion District 8", "amc-dine-in-fashion-district-8", "Pennsylvania", "Philadelphia"),
    ("Cinemark University City Penn 6", "Cinemark University City Penn 6", "cinemark-university-city-penn-6", "Pennsylvania", "Philadelphia"),
    ("Landmark Ritz Five", "Landmark Ritz Five", "landmark-ritz-five", "Pennsylvania", "Philadelphia"),
    ("PFS Bourse Theater", "PFS Bourse Theater", "pfs-bourse-theater", "Pennsylvania", "Philadelphia"),
    ("PFS East", "PFS East", "pfs-east", "Pennsylvania", "Philadelphia"),
    ("Philadelphia Film Center / Prince Theatre", "Philadelphia Film Center / Prince Theatre", "philadelphia-film-center-prince-theatre", "Pennsylvania", "Philadelphia"),
    ("Regal UA Grant Plaza", "Regal UA Grant Plaza", "regal-ua-grant-plaza", "Pennsylvania", "Philadelphia"),
    ("Regal UA Main Street Cinema", "Regal UA Main Street Cinema", "regal-ua-main-street-cinema", "Pennsylvania", "Philadelphia")
  ))
  private def p_san_antonio: R = ("san-antonio", "san-antonio-texas", "San Antonio", "Texas", 29.4241, -98.4936, "America/Chicago", false, Seq("San Antonio"), Seq(
    ("Alamo Drafthouse Park North", "Alamo Drafthouse Park North", "alamo-drafthouse-park-north", "Texas", "San Antonio"),
    ("Alamo Drafthouse Stone Oak", "Alamo Drafthouse Stone Oak", "alamo-drafthouse-stone-oak", "Texas", "San Antonio"),
    ("Cinemark Mccreless San Antonio 10", "Cinemark Mccreless San Antonio 10", "cinemark-mccreless-san-antonio-10", "Texas", "San Antonio"),
    ("Cinemark San Antonio 16", "Cinemark San Antonio 16", "cinemark-san-antonio-16", "Texas", "San Antonio"),
    ("City Base Cinema San Antonio", "City Base Cinema San Antonio", "city-base-cinema-san-antonio", "Texas", "San Antonio"),
    ("Flix Brewhouse San Antonio", "Flix Brewhouse San Antonio", "flix-brewhouse-san-antonio", "Texas", "San Antonio"),
    ("Regal Alamo Quarry San Antonio", "Regal Alamo Quarry San Antonio", "regal-alamo-quarry-san-antonio", "Texas", "San Antonio"),
    ("Regal Cielo Vista San Antonio", "Regal Cielo Vista San Antonio", "regal-cielo-vista-san-antonio", "Texas", "San Antonio"),
    ("Regal Huebner Oaks San Antonio", "Regal Huebner Oaks San Antonio", "regal-huebner-oaks-san-antonio", "Texas", "San Antonio"),
    ("Regal Live Oak San Antonio", "Regal Live Oak San Antonio", "regal-live-oak-san-antonio", "Texas", "San Antonio"),
    ("Regal Northwoods San Antonio", "Regal Northwoods San Antonio", "regal-northwoods-san-antonio", "Texas", "San Antonio"),
    ("Santikos Casa Blanca San Antonio", "Santikos Casa Blanca San Antonio", "santikos-casa-blanca-san-antonio", "Texas", "San Antonio"),
    ("Santikos Embassy San Antonio", "Santikos Embassy San Antonio", "santikos-embassy-san-antonio", "Texas", "San Antonio"),
    ("Santikos Galaxy San Antonio", "Santikos Galaxy San Antonio", "santikos-galaxy-san-antonio", "Texas", "San Antonio"),
    ("Santikos Mayan Palace San Antonio", "Santikos Mayan Palace San Antonio", "santikos-mayan-palace-san-antonio", "Texas", "San Antonio"),
    ("Santikos Northwest", "Santikos Northwest", "santikos-northwest", "Texas", "San Antonio"),
    ("Santikos Palladium", "Santikos Palladium", "santikos-palladium", "Texas", "San Antonio"),
    ("Santikos Silverado", "Santikos Silverado", "santikos-silverado", "Texas", "San Antonio"),
    ("Santikos Westlakes", "Santikos Westlakes", "santikos-westlakes", "Texas", "San Antonio"),
    ("AMC Rivercenter 11 San Antonio", "AMC Rivercenter 11 San Antonio", "amc-rivercenter-11-san-antonio", "Texas", "San Antonio")
  ))
  private def p_san_diego: R = ("san-diego", "san-diego-california", "San Diego", "California", 32.7157, -117.1647, "America/Los_Angeles", false, Seq("San Diego"), Seq(
    ("AMC Fashion Valley 18 San Diego", "AMC Fashion Valley 18 San Diego", "amc-fashion-valley-18-san-diego", "California", "San Diego"),
    ("AMC Mission Valley 20 San Diego", "AMC Mission Valley 20 San Diego", "amc-mission-valley-20-san-diego", "California", "San Diego"),
    ("AMC Palm Promenade 24", "AMC Palm Promenade 24", "amc-palm-promenade-24", "California", "San Diego"),
    ("AMC UTC 14", "AMC UTC 14", "amc-utc-14", "California", "San Diego"),
    ("Angelika Film Center Carmel Mountain", "Angelika Film Center Carmel Mountain", "angelika-film-center-carmel-mountain", "California", "San Diego"),
    ("Cinema Under the Stars San Diego", "Cinema Under the Stars San Diego", "cinema-under-the-stars-san-diego", "California", "San Diego"),
    ("Cinepolis Del Mar", "Cinepolis Del Mar", "cinepolis-del-mar", "California", "San Diego"),
    ("Digital Gym Cinema San Diego", "Digital Gym Cinema San Diego", "digital-gym-cinema-san-diego", "California", "San Diego"),
    ("Fleet Science Center IMAX", "Fleet Science Center IMAX", "fleet-science-center-imax", "California", "San Diego"),
    ("Regal Mira Mesa Stadium San Diego", "Regal Mira Mesa Stadium San Diego", "regal-mira-mesa-stadium-san-diego", "California", "San Diego"),
    ("Rooftop Cinema San Diego", "Rooftop Cinema San Diego", "rooftop-cinema-san-diego", "California", "San Diego"),
    ("South Bay Drive-In Theatre", "South Bay Drive-In Theatre", "south-bay-drive-in-theatre", "California", "San Diego"),
    ("THE LOT Liberty Station", "THE LOT Liberty Station", "the-lot-liberty-station", "California", "San Diego"),
    ("UltraStar Mission Valley Cinemas-Hazard Center", "UltraStar Mission Valley Cinemas-Hazard Center", "ultrastar-mission-valley-cinemas-hazard-center", "California", "San Diego")
  ))
  private def p_dallas: R = ("dallas", "dallas-texas", "Dallas", "Texas", 32.7831, -96.8067, "America/Chicago", false, Seq("Dallas"), Seq(
    ("Alamo Drafthouse Cedars Dallas", "Alamo Drafthouse Cedars Dallas", "alamo-drafthouse-cedars-dallas", "Texas", "Dallas"),
    ("Alamo Drafthouse Lake Highlands", "Alamo Drafthouse Lake Highlands", "alamo-drafthouse-lake-highlands", "Texas", "Dallas"),
    ("AMC North Park 15 Dallas", "AMC North Park 15 Dallas", "amc-north-park-15-dallas", "Texas", "Dallas"),
    ("Angelika Film Center Dallas", "Angelika Film Center Dallas", "angelika-film-center-dallas", "Texas", "Dallas"),
    ("Cinemark Dallas 17", "Cinemark Dallas 17", "cinemark-dallas-17", "Texas", "Dallas"),
    ("Cinepolis Victory Park", "Cinepolis Victory Park", "cinepolis-victory-park", "Texas", "Dallas"),
    ("Hooky Entertainment Addison Dallas", "Hooky Entertainment Addison Dallas", "hooky-entertainment-addison-dallas", "Texas", "Dallas"),
    ("Landmark Inwood Theatre", "Landmark Inwood Theatre", "landmark-inwood-theatre", "Texas", "Dallas"),
    ("Regal UA Galaxy Dallas", "Regal UA Galaxy Dallas", "regal-ua-galaxy-dallas", "Texas", "Dallas"),
    ("Studio Movie Grill Royal Lane", "Studio Movie Grill Royal Lane", "studio-movie-grill-royal-lane", "Texas", "Dallas"),
    ("Studio Movie Grill Spring Valley", "Studio Movie Grill Spring Valley", "studio-movie-grill-spring-valley", "Texas", "Dallas"),
    ("Texas Theatre Dallas", "Texas Theatre Dallas", "texas-theatre-dallas", "Texas", "Dallas"),
    ("Violet Crown Dallas", "Violet Crown Dallas", "violet-crown-dallas", "Texas", "Dallas"),
    ("Majestic Theatre Dallas", "Majestic Theatre Dallas", "majestic-theatre-dallas", "Texas", "Dallas")
  ))
  private def p_jacksonville: R = ("jacksonville", "jacksonville-florida", "Jacksonville", "Florida", 30.3322, -81.6556, "America/New_York", false, Seq("Jacksonville"), Seq(
    ("AMC Regency 24 Jacksonville", "AMC Regency 24 Jacksonville", "amc-regency-24-jacksonville", "Florida", "Jacksonville"),
    ("Cinemark Tinseltown Jacksonville", "Cinemark Tinseltown Jacksonville", "cinemark-tinseltown-jacksonville", "Florida", "Jacksonville"),
    ("Epic Theatres Oakleaf", "Epic Theatres Oakleaf", "epic-theatres-oakleaf", "Florida", "Jacksonville"),
    ("Regal Avenues Jacksonville", "Regal Avenues Jacksonville", "regal-avenues-jacksonville", "Florida", "Jacksonville")
  ))
  private def p_fort_worth: R = ("fort-worth", "fort-worth-texas", "Fort Worth", "Texas", 32.7254, -97.3208, "America/Chicago", false, Seq("Fort Worth"), Seq(
    ("AMC Clearfork 8", "AMC Clearfork 8", "amc-clearfork-8", "Texas", "Fort Worth"),
    ("AMC Eastchase 9 Fort Worth", "AMC Eastchase 9 Fort Worth", "amc-eastchase-9-fort-worth", "Texas", "Fort Worth"),
    ("AMC Hulen 10", "AMC Hulen 10", "amc-hulen-10", "Texas", "Fort Worth"),
    ("AMC Lake Worth 14", "AMC Lake Worth 14", "amc-lake-worth-14", "Texas", "Fort Worth"),
    ("AMC Palace 9 Fort Worth", "AMC Palace 9 Fort Worth", "amc-palace-9-fort-worth", "Texas", "Fort Worth"),
    ("America Cinemas Fort Worth", "America Cinemas Fort Worth", "america-cinemas-fort-worth", "Texas", "Fort Worth"),
    ("Cinemark Alliance Town Center", "Cinemark Alliance Town Center", "cinemark-alliance-town-center", "Texas", "Fort Worth"),
    ("Cinemark Ridgmar Mall 13", "Cinemark Ridgmar Mall 13", "cinemark-ridgmar-mall-13", "Texas", "Fort Worth"),
    ("Coyote Drive-in Ft Worth", "Coyote Drive-in Ft Worth", "coyote-drive-in-ft-worth", "Texas", "Fort Worth"),
    ("Ft Worth Modern Museum Of Art", "Ft Worth Modern Museum Of Art", "ft-worth-modern-museum-of-art", "Texas", "Fort Worth"),
    ("Movie Tavern Hulen Ft Worth", "Movie Tavern Hulen Ft Worth", "movie-tavern-hulen-ft-worth", "Texas", "Fort Worth"),
    ("Movie Tavern West 7th Street", "Movie Tavern West 7th Street", "movie-tavern-west-7th-street", "Texas", "Fort Worth"),
    ("Regal Fossil Creek Fort Worth", "Regal Fossil Creek Fort Worth", "regal-fossil-creek-fort-worth", "Texas", "Fort Worth"),
    ("Rooftop Cinema Ft Worth", "Rooftop Cinema Ft Worth", "rooftop-cinema-ft-worth", "Texas", "Fort Worth"),
    ("Studio Movie Grill Chisholm Trail", "Studio Movie Grill Chisholm Trail", "studio-movie-grill-chisholm-trail", "Texas", "Fort Worth")
  ))
  private def p_san_jose: R = ("san-jose", "san-jose-california", "San Jose", "California", 37.3394, -121.895, "America/Los_Angeles", false, Seq("San Jose"), Seq(
    ("AMC Eastridge 15 San Jose", "AMC Eastridge 15 San Jose", "amc-eastridge-15-san-jose", "California", "San Jose"),
    ("AMC Saratoga 14", "AMC Saratoga 14", "amc-saratoga-14", "California", "San Jose"),
    ("CineLux Almaden San Jose", "CineLux Almaden San Jose", "cinelux-almaden-san-jose", "California", "San Jose"),
    ("Cinemark CinéArts Santana Row", "Cinemark CinéArts Santana Row", "cinemark-cinarts-santana-row", "California", "San Jose"),
    ("Cinemark San Jose Oakridge 20", "Cinemark San Jose Oakridge 20", "cinemark-san-jose-oakridge-20", "California", "San Jose"),
    ("West Wind San Jose", "West Wind San Jose", "west-wind-san-jose", "California", "San Jose"),
    ("3Below Theaters San Jose", "3Below Theaters San Jose", "3below-theaters-san-jose", "California", "San Jose"),
    ("IMAX Dome Theater San Jose", "IMAX Dome Theater San Jose", "imax-dome-theater-san-jose", "California", "San Jose")
  ))
  private def p_austin: R = ("austin", "austin-texas", "Austin", "Texas", 30.2672, -97.7431, "America/Chicago", false, Seq("Austin"), Seq(
    ("Alamo Drafthouse Lakeline", "Alamo Drafthouse Lakeline", "alamo-drafthouse-lakeline", "Texas", "Austin"),
    ("Alamo Drafthouse Mueller", "Alamo Drafthouse Mueller", "alamo-drafthouse-mueller", "Texas", "Austin"),
    ("Alamo Drafthouse Slaughter Lane", "Alamo Drafthouse Slaughter Lane", "alamo-drafthouse-slaughter-lane", "Texas", "Austin"),
    ("Alamo Drafthouse South Lamar", "Alamo Drafthouse South Lamar", "alamo-drafthouse-south-lamar", "Texas", "Austin"),
    ("Alamo Drafthouse Village Austin", "Alamo Drafthouse Village Austin", "alamo-drafthouse-village-austin", "Texas", "Austin"),
    ("AMC Barton Creek Mall Austin", "AMC Barton Creek Mall Austin", "amc-barton-creek-mall-austin", "Texas", "Austin"),
    ("AMC DINE-IN Tech Ridge 10", "AMC DINE-IN Tech Ridge 10", "amc-dine-in-tech-ridge-10", "Texas", "Austin"),
    ("Austin Film Society Cinema", "Austin Film Society Cinema", "austin-film-society-cinema", "Texas", "Austin"),
    ("Bullock Museum IMAX", "Bullock Museum IMAX", "bullock-museum-imax", "Texas", "Austin"),
    ("Cinemark Southpark Meadows", "Cinemark Southpark Meadows", "cinemark-southpark-mall-austin-14", "Texas", "Austin"),
    ("Cinepolis NW Austin", "Cinepolis NW Austin", "cinepolis-nw-austin", "Texas", "Austin"),
    ("Cinepolis SW Austin", "Cinepolis SW Austin", "cinepolis-sw-austin", "Texas", "Austin"),
    ("EVO Cinemas Belterra Village", "EVO Cinemas Belterra Village", "evo-cinemas-belterra-village", "Texas", "Austin"),
    ("Galaxy Theatres Austin", "Galaxy Theatres Austin", "galaxy-theares-austin", "Texas", "Austin"),
    ("IPIC Austin", "IPIC Austin", "ipic-austin", "Texas", "Austin"),
    ("Lake Creek 7 Austin", "Lake Creek 7 Austin", "lake-creek-7-austin", "Texas", "Austin"),
    ("Regal Gateway Austin", "Regal Gateway Austin", "regal-gateway-austin", "Texas", "Austin"),
    ("Regal Metropolitan Austin", "Regal Metropolitan Austin", "regal-metropolitan-austin", "Texas", "Austin"),
    ("Regal Westgate Austin", "Regal Westgate Austin", "regal-westgate-austin", "Texas", "Austin"),
    ("Violet Crown Austin", "Violet Crown Austin", "violet-crown-austin", "Texas", "Austin"),
    ("Blue Starlite Drive-In Central", "Blue Starlite Drive-In Central", "blue-starlite-drive-in-central", "Texas", "Austin"),
    ("Blue Starlite Drive-In Downtown", "Blue Starlite Drive-In Downtown", "blue-starlite-drive-in-downtown", "Texas", "Austin"),
    ("Eastside Cinema at the Millennium Austin", "Eastside Cinema at the Millennium Austin", "eastside-cinema-at-the-millennium-austin", "Texas", "Austin"),
    ("Paramount Theatre Austin", "Paramount Theatre Austin", "paramount-theatre-austin", "Texas", "Austin"),
    ("Soho House Austin", "Soho House Austin", "soho-house-austin", "Texas", "Austin"),
    ("We Luv Video Austin", "We Luv Video Austin", "we-luv-video-austin", "Texas", "Austin")
  ))
  private def p_columbus: R = ("columbus", "columbus-ohio", "Columbus", "Ohio", 39.9612, -82.9988, "America/New_York", false, Seq("Columbus"), Seq(
    ("AMC Columbus 10", "AMC Columbus 10", "amc-columbus-10", "Ohio", "Columbus"),
    ("AMC DINE-IN Easton Town Center 28", "AMC DINE-IN Easton Town Center 28", "amc-dine-in-easton-town-center-28", "Ohio", "Columbus"),
    ("Cinemark Polaris 18", "Cinemark Polaris 18", "cinemark-polaris-18", "Ohio", "Columbus"),
    ("Drexel Theatre Columbus", "Drexel Theatre Columbus", "drexel-theatre-columbus", "Ohio", "Columbus"),
    ("Gateway Film Center", "Gateway Film Center", "gateway-film-center", "Ohio", "Columbus"),
    ("Marcus Crosswoods Columbus", "Marcus Crosswoods Columbus", "marcus-crosswoods-columbus", "Ohio", "Columbus"),
    ("Phoenix Lennox Town Center 24", "Phoenix Lennox Town Center 24", "phoenix-lennox-town-center-24", "Ohio", "Columbus"),
    ("Studio 35 Cinema & Drafthouse", "Studio 35 Cinema & Drafthouse", "studio-35-cinema-drafthouse", "Ohio", "Columbus"),
    ("Wexner Center Columbus", "Wexner Center Columbus", "wexner-center-columbus", "Ohio", "Columbus"),
    ("Ohio Theatre Columbus", "Ohio Theatre Columbus", "ohio-theatre-columbus", "Ohio", "Columbus"),
    ("South Twin Drive-In", "South Twin Drive-In", "south-twin-drive-in", "Ohio", "Columbus")
  ))
  private def p_charlotte: R = ("charlotte", "charlotte-north-carolina", "Charlotte", "North Carolina", 35.2271, -80.8431, "America/New_York", false, Seq("Charlotte"), Seq(
    ("AMC Carolina Pavilions 22", "AMC Carolina Pavilions 22", "amc-carolina-pavilions-22", "North Carolina", "Charlotte"),
    ("AMC Northlake Mall 14", "AMC Northlake Mall 14", "amc-northlake-mall-14", "North Carolina", "Charlotte"),
    ("AMC Park Terrace 6", "AMC Park Terrace 6", "amc-park-terrace-6", "North Carolina", "Charlotte"),
    ("Ayrsley Grand Cinema Charlotte", "Ayrsley Grand Cinema Charlotte", "ayrsley-grand-cinema-charlotte", "North Carolina", "Charlotte"),
    ("Cinemark Bistro Charlotte", "Cinemark Bistro Charlotte", "cinemark-bistro-charlotte", "North Carolina", "Charlotte"),
    ("Cinergy Dine-In Charlotte 10", "Cinergy Dine-In Charlotte 10", "cinergy-dine-in-charlotte-10", "North Carolina", "Charlotte"),
    ("Independent Picture House Charlotte", "Independent Picture House Charlotte", "independent-picture-house-charlotte", "North Carolina", "Charlotte"),
    ("Regal Starlight Charlotte", "Regal Starlight Charlotte", "regal-starlight-charlotte", "North Carolina", "Charlotte"),
    ("Regal Stonecrest at Piper Glen Charlotte", "Regal Stonecrest at Piper Glen Charlotte", "regal-stonecrest-at-piper-glen-charlotte", "North Carolina", "Charlotte"),
    ("Carolina Theatre at Belk Place", "Carolina Theatre at Belk Place", "carolina-theatre-at-belk-place", "North Carolina", "Charlotte"),
    ("Discovery Place Charlotte", "Discovery Place Charlotte", "discovery-place-charlotte", "North Carolina", "Charlotte")
  ))
  private def p_indianapolis: R = ("indianapolis", "indianapolis-indiana", "Indianapolis", "Indiana", 39.7684, -86.158, "America/Indiana/Indianapolis", false, Seq("Indianapolis"), Seq(
    ("Alamo Drafthouse Indianapolis", "Alamo Drafthouse Indianapolis", "alamo-drafthouse-indianapolis", "Indiana", "Indianapolis"),
    ("AMC Castleton Indianapolis 14", "AMC Castleton Indianapolis 14", "amc-castleton-indianapolis-14", "Indiana", "Indianapolis"),
    ("AMC Indianapolis 17", "AMC Indianapolis 17", "amc-indianapolis-17", "Indiana", "Indianapolis"),
    ("AMC Traders Point 12", "AMC Traders Point 12", "amc-traders-point-12", "Indiana", "Indianapolis"),
    ("AMC Washington Square 12", "AMC Washington Square 12", "amc-washington-square-12", "Indiana", "Indianapolis"),
    ("Cinemark Greenwood Corner", "Cinemark Greenwood Corner", "cinemark-greenwood-corner", "Indiana", "Indianapolis"),
    ("IMAX Theatre at the Indiana State Museum", "IMAX Theatre at the Indiana State Museum", "imax-theatre-at-the-indiana-state-museum", "Indiana", "Indianapolis"),
    ("Kan-Kan Cinema Indianapolis", "Kan-Kan Cinema Indianapolis", "kan-kan-cinema-indianapolis", "Indiana", "Indianapolis"),
    ("Landmark Glendale 12", "Landmark Glendale 12", "landmark-glendale-12", "Indiana", "Indianapolis"),
    ("Landmark Keystone Art Cinema", "Landmark Keystone Art Cinema", "landmark-keystone-art-cinema", "Indiana", "Indianapolis"),
    ("Living Room Theaters Indianapolis", "Living Room Theaters Indianapolis", "living-room-theaters-indianapolis", "Indiana", "Indianapolis"),
    ("Regal Galaxy Indianapolis", "Regal Galaxy Indianapolis", "regal-galaxy-indianapolis", "Indiana", "Indianapolis"),
    ("Tibbs Drive-In", "Tibbs Drive-In", "tibbs-drive-in", "Indiana", "Indianapolis"),
    ("Newfields Indianapolis", "Newfields Indianapolis", "newfields-indianapolis", "Indiana", "Indianapolis")
  ))
  private def p_san_francisco: R = ("san-francisco", "san-francisco-california", "San Francisco", "California", 37.7749, -122.4194, "America/Los_Angeles", false, Seq("San Francisco"), Seq(
    ("4 Star Theatre San Francisco", "4 Star Theatre San Francisco", "4-star-theatre-san-francisco", "California", "San Francisco"),
    ("Alamo Drafthouse New Mission", "Alamo Drafthouse New Mission", "alamo-drafthouse-new-mission", "California", "San Francisco"),
    ("AMC Kabuki 8", "AMC Kabuki 8", "amc-kabuki-8", "California", "San Francisco"),
    ("AMC Metreon 16 San Francisco", "AMC Metreon 16 San Francisco", "amc-metreon-16-san-francisco", "California", "San Francisco"),
    ("Apple Cinemas Van Ness", "Apple Cinemas Van Ness", "apple-cinemas-van-ness", "California", "San Francisco"),
    ("Balboa Theatre San Francisco", "Balboa Theatre San Francisco", "balboa-theatre-san-francisco", "California", "San Francisco"),
    ("Castro Theatre", "Castro Theatre", "castro-theatre", "California", "San Francisco"),
    ("Foreign Cinema", "Foreign Cinema", "foreign-cinema", "California", "San Francisco"),
    ("Landmark Opera Plaza Cinema", "Landmark Opera Plaza Cinema", "landmark-opera-plaza-cinema", "California", "San Francisco"),
    ("Marina Theatre San Francisco", "Marina Theatre San Francisco", "marina-theatre-san-francisco", "California", "San Francisco"),
    ("Presidio Theatre San Francisco", "Presidio Theatre San Francisco", "presidio-theatre-san-francisco", "California", "San Francisco"),
    ("Regal Stonestown Galleria", "Regal Stonestown Galleria", "regal-stonestown-galleria", "California", "San Francisco"),
    ("Roxie Theatre", "Roxie Theatre", "roxie-theatre", "California", "San Francisco"),
    ("Vogue Theater San Francisco", "Vogue Theater San Francisco", "vogue-theater-san-francisco", "California", "San Francisco"),
    ("McKenna Theatre (SFSU)", "McKenna Theatre (SFSU)", "mckenna-theatre-sfsu", "California", "San Francisco")
  ))
  private def p_seattle: R = ("seattle", "seattle-washington", "Seattle", "Washington", 47.6062, -122.3321, "America/Los_Angeles", false, Seq("Seattle"), Seq(
    ("AMC Oak Tree 6 Seattle", "AMC Oak Tree 6 Seattle", "amc-oak-tree-6-seattle", "Washington", "Seattle"),
    ("AMC Pacific Place 11 Seattle", "AMC Pacific Place 11 Seattle", "amc-pacific-place-11-seattle", "Washington", "Seattle"),
    ("Boeing and PACCAR IMAX Theaters", "Boeing and PACCAR IMAX Theaters", "boeing-and-paccar-imax-theaters", "Washington", "Seattle"),
    ("Central Cinema Seattle", "Central Cinema Seattle", "central-cinema-seattle", "Washington", "Seattle"),
    ("Grand Illusion Cinema Seattle", "Grand Illusion Cinema Seattle", "grand-illusion-cinema-seattle", "Washington", "Seattle"),
    ("Historic Admiral Theater", "Historic Admiral Theater", "historic-admiral-theater", "Washington", "Seattle"),
    ("Majestic Bay Theatres", "Majestic Bay Theatres", "majestic-bay-theatres", "Washington", "Seattle"),
    ("Northwest Film Forum", "Northwest Film Forum", "northwest-film-forum", "Washington", "Seattle"),
    ("Regal Meridian Seattle", "Regal Meridian Seattle", "regal-meridian-seattle", "Washington", "Seattle"),
    ("Regal Thornton Place Seattle", "Regal Thornton Place Seattle", "regal-thornton-place-seattle", "Washington", "Seattle"),
    ("SIFF Cinema at The Film Center", "SIFF Cinema at The Film Center", "siff-cinema-at-the-film-center", "Washington", "Seattle"),
    ("SIFF Cinema at The Uptown", "SIFF Cinema at The Uptown", "siff-cinema-at-the-uptown", "Washington", "Seattle"),
    ("SIFF Cinema Downtown", "SIFF Cinema Downtown", "siff-cinema-downtown", "Washington", "Seattle"),
    ("Tasveer Film Arts Center", "Tasveer Film Arts Center", "tasveer-film-arts-center", "Washington", "Seattle"),
    ("The Beacon Seattle", "The Beacon Seattle", "the-beacon-seattle", "Washington", "Seattle"),
    ("Emma Baillargeon Stimson Aud", "Emma Baillargeon Stimson Aud", "emma-baillargeon-stimson-aud", "Washington", "Seattle"),
    ("PACCAR IMAX at PSC", "PACCAR IMAX at PSC", "paccar-imax-at-psc", "Washington", "Seattle"),
    ("Paramount Theatre Seattle", "Paramount Theatre Seattle", "paramount-theatre-seattle", "Washington", "Seattle"),
    ("SIFF Egyptian", "SIFF Egyptian", "siff-egyptian", "Washington", "Seattle"),
    ("Varsity Theater Seattle", "Varsity Theater Seattle", "varsity-theater-seattle", "Washington", "Seattle")
  ))
  private def p_denver: R = ("denver", "denver-colorado", "Denver", "Colorado", 39.7392, -104.9847, "America/Denver", false, Seq("Denver"), Seq(
    ("Alamo Drafthouse Sloans Lake", "Alamo Drafthouse Sloans Lake", "alamo-drafthouse-sloans-lake", "Colorado", "Denver"),
    ("AMC 9+CO 10 Denver", "AMC 9+CO 10 Denver", "amc-9co-10-denver", "Colorado", "Denver"),
    ("AMC DINE-IN Cherry Creek 8", "AMC DINE-IN Cherry Creek 8", "amc-dine-in-cherry-creek-8", "Colorado", "Denver"),
    ("Harkins Northfield 18", "Harkins Northfield 18", "harkins-northfield-18", "Colorado", "Denver"),
    ("Landmark Mayan Theatre Denver", "Landmark Mayan Theatre Denver", "landmark-mayan-theatre-denver", "Colorado", "Denver"),
    ("Regal Denver Pavilions", "Regal Denver Pavilions", "regal-denver-pavilions", "Colorado", "Denver"),
    ("Regal UA Colorado Center Denver", "Regal UA Colorado Center Denver", "regal-ua-colorado-center-denver", "Colorado", "Denver"),
    ("Sie FilmCenter Denver", "Sie FilmCenter Denver", "sie-filmcenter-denver", "Colorado", "Denver"),
    ("Bug Theatre Denver", "Bug Theatre Denver", "bug-theatre-denver", "Colorado", "Denver"),
    ("Infinity Theater", "Infinity Theater", "infinity-theater", "Colorado", "Denver")
  ))
  private def p_washington: R = ("washington", "washington-district-of-columbia", "Washington", "District of Columbia", 38.8951, -77.0364, "America/New_York", false, Seq("Washington"), Seq(
    ("Alamo Drafthouse DC Bryant Street", "Alamo Drafthouse DC Bryant Street", "alamo-drafthouse-dc-bryant-street", "District of Columbia", "Washington"),
    ("AMC Georgetown 14", "AMC Georgetown 14", "amc-georgetown-14", "District of Columbia", "Washington"),
    ("Angelika Pop-up at Union Market", "Angelika Pop-up at Union Market", "angelika-pop-up-at-union-market", "District of Columbia", "Washington"),
    ("Avalon Theatre Washington", "Avalon Theatre Washington", "avalon-theatre-washington", "District of Columbia", "Washington"),
    ("Cafritz Hall", "Cafritz Hall", "cafritz-hall", "District of Columbia", "Washington"),
    ("Landmark Atlantic Plumbing Cinema", "Landmark Atlantic Plumbing Cinema", "landmark-atlantic-plumbing-cinema", "District of Columbia", "Washington"),
    ("Miracle Theatre", "Miracle Theatre", "miracle-theatre", "District of Columbia", "Washington"),
    ("Regal Gallery Place Washington", "Regal Gallery Place Washington", "regal-gallery-place-washington", "District of Columbia", "Washington"),
    ("Suns Cinema Theater & Bar", "Suns Cinema Theater & Bar", "suns-cinema-theater-bar", "District of Columbia", "Washington"),
    ("French Ambassy", "French Ambassy", "french-ambassy", "District of Columbia", "Washington"),
    ("Meyer Auditorium-freer Sackler Gallery", "Meyer Auditorium-freer Sackler Gallery", "meyer-auditorium-freer-sackler-gallery", "District of Columbia", "Washington")
  ))
  private def p_nashville: R = ("nashville", "nashville-tennessee", "Nashville", "Tennessee", 36.1659, -86.7844, "America/Chicago", false, Seq("Nashville"), Seq(
    ("AMC Bellevue 12 Nashville", "AMC Bellevue 12 Nashville", "amc-bellevue-12-nashville", "Tennessee", "Nashville"),
    ("Belcourt Theatre Nashville", "Belcourt Theatre Nashville", "belcourt-theatre-nashville", "Tennessee", "Nashville"),
    ("Hooky Entertainment Nashville", "Hooky Entertainment Nashville", "hooky-entertainment-nashville", "Tennessee", "Nashville"),
    ("Regal Green Hills Nashville", "Regal Green Hills Nashville", "regal-green-hills-nashville", "Tennessee", "Nashville"),
    ("Regal Hollywood Nashville", "Regal Hollywood Nashville", "regal-hollywood-nashville", "Tennessee", "Nashville"),
    ("Regal Opry Mills Nashville", "Regal Opry Mills Nashville", "regal-opry-mills-nashville", "Tennessee", "Nashville"),
    ("SOHO HOUSE NASHVILLE", "SOHO HOUSE NASHVILLE", "soho-house-nashville", "Tennessee", "Nashville")
  ))
  private def p_oklahoma_city: R = ("oklahoma-city", "oklahoma-city-oklahoma", "Oklahoma City", "Oklahoma", 35.4676, -97.5164, "America/Chicago", false, Seq("Oklahoma City"), Seq(
    ("AMC Penn Square 10 Oklahoma City", "AMC Penn Square 10 Oklahoma City", "amc-penn-square-10-oklahoma-city", "Oklahoma", "Oklahoma City"),
    ("AMC Quail Springs Mall 24", "AMC Quail Springs Mall 24", "amc-quail-springs-mall-24", "Oklahoma", "Oklahoma City"),
    ("Cinemark Tinseltown Oklahoma City", "Cinemark Tinseltown Oklahoma City", "cinemark-tinseltown-oklahoma-city", "Oklahoma", "Oklahoma City"),
    ("Flix Brewhouse Oklahoma City", "Flix Brewhouse Oklahoma City", "flix-brewhouse-oklahoma-city", "Oklahoma", "Oklahoma City"),
    ("Harkins Theatres Bricktown 16", "Harkins Theatres Bricktown 16", "harkins-bricktown-16", "Oklahoma", "Oklahoma City"),
    ("Icon Cinemas OKC Windsor", "Icon Cinemas OKC Windsor", "icon-cinemas-okc-windsor", "Oklahoma", "Oklahoma City"),
    ("Oklahoma City Museum of Art (OKCMOA)", "Oklahoma City Museum of Art (OKCMOA)", "oklahoma-city-museum-of-art-okcmoa", "Oklahoma", "Oklahoma City"),
    ("Rodeo Cinema", "Rodeo Cinema", "rodeo-cinema", "Oklahoma", "Oklahoma City"),
    ("Winchester Drive-in Oklahoma City", "Winchester Drive-in Oklahoma City", "winchester-drive-in-oklahoma-city", "Oklahoma", "Oklahoma City")
  ))
  private def p_el_paso: R = ("el-paso", "el-paso-texas", "El Paso", "Texas", 31.7587, -106.4869, "America/Denver", false, Seq("El Paso"), Seq(
    ("AMC El Paso 16", "AMC El Paso 16", "amc-el-paso-16", "Texas", "El Paso"),
    ("Cinemark Cielo Vista Mall 14", "Cinemark Cielo Vista Mall 14", "cinemark-cielo-vista-mall-14", "Texas", "El Paso"),
    ("Cinemark East Montana El Paso", "Cinemark East Montana El Paso", "cinemark-east-montana-el-paso", "Texas", "El Paso"),
    ("Cinemark Tinseltown El Paso", "Cinemark Tinseltown El Paso", "cinemark-tinseltown-el-paso", "Texas", "El Paso"),
    ("Cinemark West El Paso 14", "Cinemark West El Paso 14", "cinemark-west-el-paso-14", "Texas", "El Paso"),
    ("Flix Brewhouse East El Paso", "Flix Brewhouse East El Paso", "alamo-drafthouse-east-el-paso", "Texas", "El Paso"),
    ("Flix Brewhouse El Paso", "Flix Brewhouse El Paso", "flix-brewhouse-el-paso", "Texas", "El Paso"),
    ("Flix Brewhouse Montecillo", "Flix Brewhouse Montecillo", "alamo-drafthouse-montecillo", "Texas", "El Paso"),
    ("Gamescape by Cinemark at El Paso", "Gamescape by Cinemark at El Paso", "gamescape-by-cinemark-at-el-paso", "Texas", "El Paso"),
    ("Premiere El Paso Basset", "Premiere El Paso Basset", "premiere-el-paso-basset", "Texas", "El Paso"),
    ("Plaza Theatre El Paso", "Plaza Theatre El Paso", "plaza-theatre-el-paso", "Texas", "El Paso")
  ))
  private def p_boston: R = ("boston", "boston-massachusetts", "Boston", "Massachusetts", 42.3584, -71.0598, "America/New_York", false, Seq("Boston"), Seq(
    ("Alamo Drafthouse Seaport Boston", "Alamo Drafthouse Seaport Boston", "alamo-drafthouse-seaport-boston", "Massachusetts", "Boston"),
    ("AMC Boston Common 19", "AMC Boston Common 19", "amc-boston-common-19", "Massachusetts", "Boston"),
    ("AMC Causeway 13 Boston", "AMC Causeway 13 Boston", "amc-causeway-13-boston", "Massachusetts", "Boston"),
    ("Mugar Omni Theater Boston", "Mugar Omni Theater Boston", "mugar-omni-theater-boston", "Massachusetts", "Boston")
  ))
  private def p_portland: R = ("portland", "portland-oregon", "Portland", "Oregon", 45.5234, -122.6762, "America/Los_Angeles", false, Seq("Portland"), Seq(
    ("Academy Theater Portland", "Academy Theater Portland", "academy-theater-portland", "Oregon", "Portland"),
    ("Cinema 21 Portland", "Cinema 21 Portland", "cinema-21-portland", "Oregon", "Portland"),
    ("Cinemagic Theater Portland", "Cinemagic Theater Portland", "cinemagic-theater-portland", "Oregon", "Portland"),
    ("Cinemark Eastport Portland 16", "Cinemark Eastport Portland 16", "cinemark-eastport-portland-16", "Oregon", "Portland"),
    ("Empirical Theater Portland", "Empirical Theater Portland", "empirical-theater-portland", "Oregon", "Portland"),
    ("Hollywood Theatre Portland", "Hollywood Theatre Portland", "hollywood-theatre-portland", "Oregon", "Portland"),
    ("Kennedy School", "Kennedy School", "kennedy-school", "Oregon", "Portland"),
    ("Living Room Theatres Portland", "Living Room Theatres Portland", "living-room-theatres-portland", "Oregon", "Portland"),
    ("Moreland Theatre", "Moreland Theatre", "moreland-theatre", "Oregon", "Portland"),
    ("PAMCut Tomorrow Theater Portland", "PAMCut Tomorrow Theater Portland", "pamcut-tomorrow-theater-portland", "Oregon", "Portland"),
    ("Regal Fox Tower Portland", "Regal Fox Tower Portland", "regal-fox-tower-portland", "Oregon", "Portland"),
    ("Regal Lloyd Center Portland", "Regal Lloyd Center Portland", "regal-lloyd-center-portland", "Oregon", "Portland"),
    ("Regal Pioneer Place Portland", "Regal Pioneer Place Portland", "regal-pioneer-place-portland", "Oregon", "Portland"),
    ("St. Johns Twin Cinema & Pub", "St. Johns Twin Cinema & Pub", "st-johns-twin-cinema-pub", "Oregon", "Portland"),
    ("Studio One Portland 7", "Studio One Portland 7", "studio-one-portland-7", "Oregon", "Portland"),
    ("5th Avenue Cinema (PSU)", "5th Avenue Cinema (PSU)", "5th-avenue-cinema-psu", "Oregon", "Portland"),
    ("Northwest Film Center Portland", "Northwest Film Center Portland", "northwest-film-center-portland", "Oregon", "Portland"),
    ("Portland State University", "Portland State University", "portland-state-university", "Oregon", "Portland")
  ))
  private def p_detroit: R = ("detroit", "detroit-michigan", "Detroit", "Michigan", 42.3314, -83.0457, "America/Detroit", false, Seq("Detroit"), Seq(
    ("Bel Air Cinema Detroit", "Bel Air Cinema Detroit", "bel-air-cinema-detroit", "Michigan", "Detroit"),
    ("Cinema Detroit", "Cinema Detroit", "cinema-detroit", "Michigan", "Detroit"),
    ("Redford Theatre Detroit", "Redford Theatre Detroit", "redford-theatre-detroit", "Michigan", "Detroit"),
    ("Senate Theater Detroit", "Senate Theater Detroit", "senate-theater-detroit", "Michigan", "Detroit")
  ))
  private def p_las_vegas: R = ("las-vegas", "las-vegas-nevada", "Las Vegas", "Nevada", 36.175, -115.1372, "America/Los_Angeles", false, Seq("Las Vegas"), Seq(
    ("AMC Rainbow Promenade 10", "AMC Rainbow Promenade 10", "amc-rainbow-promenade-10", "Nevada", "Las Vegas"),
    ("AMC Town Square 18", "AMC Town Square 18", "amc-town-square-18", "Nevada", "Las Vegas"),
    ("Beverly Theater Las Vegas", "Beverly Theater Las Vegas", "beverly-theater-las-vegas", "Nevada", "Las Vegas"),
    ("Cinemark Century Las Vegas Suncoast", "Cinemark Century Las Vegas Suncoast", "cinemark-century-las-vegas-suncoast", "Nevada", "Las Vegas"),
    ("Cinemark Century Orleans 18", "Cinemark Century Orleans 18", "cinemark-century-orleans-18", "Nevada", "Las Vegas"),
    ("Cinemark Century Sam's Town 18", "Cinemark Century Sam's Town 18", "cinemark-century-sams-town-18", "Nevada", "Las Vegas"),
    ("Cinemark Century Santa Fe Station 16", "Cinemark Century Santa Fe Station 16", "cinemark-century-santa-fe-station-16", "Nevada", "Las Vegas"),
    ("Cinemark Century South Point 16", "Cinemark Century South Point 16", "cinemark-century-south-point-16", "Nevada", "Las Vegas"),
    ("Galaxy Boulevard Mall Las Vegas", "Galaxy Boulevard Mall Las Vegas", "galaxy-boulevard-mall-las-vegas", "Nevada", "Las Vegas"),
    ("Maya North Las Vegas", "Maya North Las Vegas", "maya-north-las-vegas", "Nevada", "Las Vegas"),
    ("Regal Aliante", "Regal Aliante", "regal-aliante", "Nevada", "Las Vegas"),
    ("Regal Boulder Las Vegas", "Regal Boulder Las Vegas", "regal-boulder-las-vegas", "Nevada", "Las Vegas"),
    ("Regal Cinebarre Palace Station", "Regal Cinebarre Palace Station", "regal-cinebarre-palace-station", "Nevada", "Las Vegas"),
    ("Regal Red Rock Stadium Las Vegas", "Regal Red Rock Stadium Las Vegas", "regal-red-rock-stadium-las-vegas", "Nevada", "Las Vegas"),
    ("West Wind Las Vegas Drive-In", "West Wind Las Vegas Drive-In", "west-wind-las-vegas-drive-in", "Nevada", "Las Vegas"),
    ("Downtown Cinema Las Vegas", "Downtown Cinema Las Vegas", "downtown-cinema-las-vegas", "Nevada", "Las Vegas")
  ))
  private def p_memphis: R = ("memphis", "memphis-tennessee", "Memphis", "Tennessee", 35.1495, -90.049, "America/Chicago", false, Seq("Memphis"), Seq(
    ("Malco Paradiso Memphis 16", "Malco Paradiso Memphis 16", "malco-paradiso-memphis-16", "Tennessee", "Memphis"),
    ("Malco Powerhouse Cinema Grill", "Malco Powerhouse Cinema Grill", "malco-powerhouse-cinema-grill", "Tennessee", "Memphis"),
    ("Malco Ridgeway Cinema Grill Memphis", "Malco Ridgeway Cinema Grill Memphis", "malco-ridgeway-cinema-grill-memphis", "Tennessee", "Memphis"),
    ("Malco Stage Cinema Bartlett", "Malco Stage Cinema Bartlett", "malco-stage-cinema-bartlett", "Tennessee", "Memphis"),
    ("Malco Studio on the Square Memphis", "Malco Studio on the Square Memphis", "malco-studio-on-the-square-memphis", "Tennessee", "Memphis"),
    ("Malco Wolfchase 8 Galleria Memphis", "Malco Wolfchase 8 Galleria Memphis", "malco-wolfchase-8-galleria-memphis", "Tennessee", "Memphis"),
    ("Palace Cinema Memphis", "Palace Cinema Memphis", "palace-cinema-memphis", "Tennessee", "Memphis"),
    ("Pink Palace Museum Memphis", "Pink Palace Museum Memphis", "pink-palace-museum-memphis", "Tennessee", "Memphis")
  ))
  private def p_louisville: R = ("louisville", "louisville-kentucky", "Louisville", "Kentucky", 38.2542, -85.7594, "America/Kentucky/Louisville", false, Seq("Louisville"), Seq(
    ("AMC Stonybrook 20 Louisville", "AMC Stonybrook 20 Louisville", "amc-stonybrook-20-louisville", "Kentucky", "Louisville"),
    ("Cinemark Mall St. Matthews", "Cinemark Mall St. Matthews", "cinemark-mall-st-matthews", "Kentucky", "Louisville"),
    ("Cinemark Preston Crossings 16", "Cinemark Preston Crossings 16", "cinemark-preston-crossings-16", "Kentucky", "Louisville"),
    ("Cinemark Tinseltown Louisville", "Cinemark Tinseltown Louisville", "cinemark-tinseltown-louisville", "Kentucky", "Louisville"),
    ("Kentucky Science Center Louisville", "Kentucky Science Center Louisville", "kentucky-science-center-louisville", "Kentucky", "Louisville"),
    ("Speed Cinema – Art Museum", "Speed Cinema – Art Museum", "speed-cinema---art-museum", "Kentucky", "Louisville"),
    ("Xscape Blankenbaker 16", "Xscape Blankenbaker 16", "xscape-blankenbaker-16", "Kentucky", "Louisville"),
    ("Baxter Avenue Theatres", "Baxter Avenue Theatres", "baxter-avenue-theatres", "Kentucky", "Louisville")
  ))
  private def p_baltimore: R = ("baltimore", "baltimore-maryland", "Baltimore", "Maryland", 39.2904, -76.6122, "America/New_York", false, Seq("Baltimore"), Seq(
    ("AMC Security Square 8 Baltimore", "AMC Security Square 8 Baltimore", "amc-security-square-8-baltimore", "Maryland", "Baltimore"),
    ("AMC White Marsh 16", "AMC White Marsh 16", "amc-white-marsh-16", "Maryland", "Baltimore"),
    ("Charles Theatrea Baltimore", "Charles Theatrea Baltimore", "charles-theatrea-baltimore", "Maryland", "Baltimore"),
    ("Flagship Cinemas Eastpoint", "Flagship Cinemas Eastpoint", "flagship-cinemas-eastpoint", "Maryland", "Baltimore"),
    ("Harbor East Cinemas", "Harbor East Cinemas", "harbor-east-cinemas", "Maryland", "Baltimore"),
    ("IMAX at the Maryland Science Center", "IMAX at the Maryland Science Center", "imax-at-the-maryland-science-center", "Maryland", "Baltimore"),
    ("Senator Theatre Baltimore", "Senator Theatre Baltimore", "senator-theatre-baltimore", "Maryland", "Baltimore"),
    ("Warehouse Cinemas Rotunda", "Warehouse Cinemas Rotunda", "warehouse-cinemas-rotunda", "Maryland", "Baltimore"),
    ("Parkway Theatre Baltimore", "Parkway Theatre Baltimore", "parkway-theatre-baltimore", "Maryland", "Baltimore")
  ))
  private def p_albuquerque: R = ("albuquerque", "albuquerque-new-mexico", "Albuquerque", "New Mexico", 35.0845, -106.6511, "America/Denver", false, Seq("Albuquerque"), Seq(
    ("AMC CLASSIC Albuquerque 12", "AMC CLASSIC Albuquerque 12", "amc-classic-albuquerque-12", "New Mexico", "Albuquerque"),
    ("Cinemark Century Rio Plex 24", "Cinemark Century Rio Plex 24", "cinemark-century-rio-plex-24", "New Mexico", "Albuquerque"),
    ("Flix Brewhouse Albuquerque Coors", "Flix Brewhouse Albuquerque Coors", "flix-brewhouse-albuquerque-coors", "New Mexico", "Albuquerque"),
    ("Flix Brewhouse Albuquerque Tramway", "Flix Brewhouse Albuquerque Tramway", "flix-brewhouse-albuquerque-tramway", "New Mexico", "Albuquerque"),
    ("Guild Cinema Albuquerque", "Guild Cinema Albuquerque", "guild-cinema-albuquerque", "New Mexico", "Albuquerque"),
    ("Icon Cinemas Hills Albuquerque", "Icon Cinemas Hills Albuquerque", "icon-cinemas-hills-albuquerque", "New Mexico", "Albuquerque"),
    ("Icon Cinemas San Mateo", "Icon Cinemas San Mateo", "icon-cinemas-san-mateo", "New Mexico", "Albuquerque"),
    ("Regal UA Cottonwood", "Regal UA Cottonwood", "regal-ua-cottonwood", "New Mexico", "Albuquerque"),
    ("Regal Winrock Albuquerque", "Regal Winrock Albuquerque", "regal-winrock-albuquerque", "New Mexico", "Albuquerque")
  ))
  private def p_milwaukee: R = ("milwaukee", "milwaukee-wisconsin", "Milwaukee", "Wisconsin", 43.0389, -87.9065, "America/Chicago", false, Seq("Milwaukee"), Seq(
    ("Avalon Theater Milwaukee", "Avalon Theater Milwaukee", "avalon-theater-milwaukee", "Wisconsin", "Milwaukee"),
    ("Milwaukee Film Downer Theatre", "Milwaukee Film Downer Theatre", "milwaukee-film-downer-theatre", "Wisconsin", "Milwaukee"),
    ("Oriental Theatre Milwaukee", "Oriental Theatre Milwaukee", "oriental-theatre-milwaukee", "Wisconsin", "Milwaukee"),
    ("Times Cinema Milwaukee", "Times Cinema Milwaukee", "times-cinema-milwaukee", "Wisconsin", "Milwaukee"),
    ("UW-Milwaukee Union Cinema", "UW-Milwaukee Union Cinema", "uw-milwaukee-union-cinema", "Wisconsin", "Milwaukee")
  ))
  private def p_tucson: R = ("tucson", "tucson-arizona", "Tucson", "Arizona", 32.2217, -110.9265, "America/Phoenix", false, Seq("Tucson"), Seq(
    ("AMC Foothills 15 Tucson", "AMC Foothills 15 Tucson", "amc-foothills-15-tucson", "Arizona", "Tucson"),
    ("Century El Con 20", "Century El Con 20", "century-el-con-20", "Arizona", "Tucson"),
    ("Cinemark Century Park Place 20", "Cinemark Century Park Place 20", "cinemark-century-park-place-20", "Arizona", "Tucson"),
    ("Cinemark Century Tucson Marketplace", "Cinemark Century Tucson Marketplace", "cinemark-century-tucson-marketplace", "Arizona", "Tucson"),
    ("Galaxy Theatres Tucson", "Galaxy Theatres Tucson", "galaxy-theatres-tucson", "Arizona", "Tucson"),
    ("Harkins Arizona Pavilions 12", "Harkins Arizona Pavilions 12", "harkins-arizona-pavilions-12", "Arizona", "Tucson"),
    ("Harkins Tucson Spectrum 18", "Harkins Tucson Spectrum 18", "harkins-tucson-spectrum-18", "Arizona", "Tucson"),
    ("RoadHouse Tucson", "RoadHouse Tucson", "roadhouse-tucson", "Arizona", "Tucson"),
    ("The Loft Cinema", "The Loft Cinema", "the-loft-cinema", "Arizona", "Tucson"),
    ("Fox Tucson Theatre", "Fox Tucson Theatre", "fox-tucson-theatre", "Arizona", "Tucson")
  ))
  private def p_fresno: R = ("fresno", "fresno-california", "Fresno", "California", 36.7477, -119.7724, "America/Los_Angeles", false, Seq("Fresno"), Seq(
    ("Historic Crest Theatre Fresno", "Historic Crest Theatre Fresno", "historic-crest-theatre-fresno", "California", "Fresno"),
    ("Maya Fresno", "Maya Fresno", "maya-fresno", "California", "Fresno"),
    ("Regal Fresno Riverpark", "Regal Fresno Riverpark", "regal-fresno-riverpark", "California", "Fresno"),
    ("Regal Manchester Fresno", "Regal Manchester Fresno", "regal-manchester-fresno", "California", "Fresno"),
    ("Regal Marketplace at El Paseo", "Regal Marketplace at El Paseo", "regal-marketplace-at-el-paseo", "California", "Fresno"),
    ("Tower Theater Fresno", "Tower Theater Fresno", "tower-theater-fresno", "California", "Fresno"),
    ("Tower Theatre Fresno", "Tower Theatre Fresno", "tower-theatre-fresno", "California", "Fresno")
  ))
  private def p_sacramento: R = ("sacramento", "sacramento-california", "Sacramento", "California", 38.5816, -121.4944, "America/Los_Angeles", false, Seq("Sacramento"), Seq(
    ("Cinemark Century Arden", "Cinemark Century Arden", "cinemark-century-arden", "California", "Sacramento"),
    ("Cinemark Century Downtown Commons", "Cinemark Century Downtown Commons", "cinemark-century-downtown-commons", "California", "Sacramento"),
    ("Cinemark Century Greenback Lane 16", "Cinemark Century Greenback Lane 16", "cinemark-century-greenback-lane-16", "California", "Sacramento"),
    ("Country Club Cinema Sacramento", "Country Club Cinema Sacramento", "country-club-cinema-sacramento", "California", "Sacramento"),
    ("Crest Sacramento", "Crest Sacramento", "crest-sacramento", "California", "Sacramento"),
    ("Esquire IMAX Theatre Sacramento", "Esquire IMAX Theatre Sacramento", "esquire-imax-theatre-sacramento", "California", "Sacramento"),
    ("Regal Delta Shores Sacramento", "Regal Delta Shores Sacramento", "regal-delta-shores-sacramento", "California", "Sacramento"),
    ("Regal Laguna Village Sacramento", "Regal Laguna Village Sacramento", "regal-laguna-village-sacramento", "California", "Sacramento"),
    ("Regal Natomas Marketplace Sacramento", "Regal Natomas Marketplace Sacramento", "regal-natomas-marketplace-sacramento", "California", "Sacramento"),
    ("Tower Theatre by Angelika Sacramento", "Tower Theatre by Angelika Sacramento", "tower-theatre-by-angelika-sacramento", "California", "Sacramento"),
    ("West Wind Sacramento Drive-In", "West Wind Sacramento Drive-In", "west-wind-sacramento-drive-in", "California", "Sacramento"),
    ("Dreamland Cinema Sacramento", "Dreamland Cinema Sacramento", "dreamland-cinema-sacramento", "California", "Sacramento")
  ))
  private def p_atlanta: R = ("atlanta", "atlanta-georgia", "Atlanta", "Georgia", 33.749, -84.388, "America/New_York", false, Seq("Atlanta"), Seq(
    ("AMC Camp Creek 14", "AMC Camp Creek 14", "amc-camp-creek-14", "Georgia", "Atlanta"),
    ("AMC Madison Yards 8", "AMC Madison Yards 8", "amc-madison-yards-8", "Georgia", "Atlanta"),
    ("AMC Parkway Point 15 Atlanta", "AMC Parkway Point 15 Atlanta", "amc-parkway-point-15-atlanta", "Georgia", "Atlanta"),
    ("AMC Phipps Plaza 14 Atlanta", "AMC Phipps Plaza 14 Atlanta", "amc-phipps-plaza-14-atlanta", "Georgia", "Atlanta"),
    ("DeAnza Starlight Drive-In Atlanta 4", "DeAnza Starlight Drive-In Atlanta 4", "deanza-starlight-drive-in-atlanta-4", "Georgia", "Atlanta"),
    ("IPIC Cinemas Atlanta", "IPIC Cinemas Atlanta", "ipic-cinemas-atlanta", "Georgia", "Atlanta"),
    ("Landmark Midtown Art Atlanta", "Landmark Midtown Art Atlanta", "landmark-midtown-art-atlanta", "Georgia", "Atlanta"),
    ("LOOK Cinemas Brookhaven", "LOOK Cinemas Brookhaven", "look-cinemas-brookhaven", "Georgia", "Atlanta"),
    ("NCG Brookhaven Cinema", "NCG Brookhaven Cinema", "ncg-brookhaven-cinema", "Georgia", "Atlanta"),
    ("Plaza Theatre Atlanta", "Plaza Theatre Atlanta", "plaza-theatre-atlanta", "Georgia", "Atlanta"),
    ("Regal Atlantic Station", "Regal Atlantic Station", "regal-atlantic-station", "Georgia", "Atlanta"),
    ("Regal Perimeter Pointe Atlanta", "Regal Perimeter Pointe Atlanta", "regal-perimeter-pointe-atlanta", "Georgia", "Atlanta"),
    ("Silverspot @ Battery Atlanta", "Silverspot @ Battery Atlanta", "silverspot-battery-atlanta", "Georgia", "Atlanta"),
    ("Springs Cinema 8 Atlanta", "Springs Cinema 8 Atlanta", "springs-cinema-8-atlanta", "Georgia", "Atlanta"),
    ("The Tara Atlanta", "The Tara Atlanta", "the-tara-atlanta", "Georgia", "Atlanta"),
    ("Cinefest Film Theatre", "Cinefest Film Theatre", "cinefest-film-theatre", "Georgia", "Atlanta"),
    ("SCAD", "SCAD", "scad", "Georgia", "Atlanta")
  ))
  private def p_miami: R = ("miami", "miami-florida", "Miami", "Florida", 25.7743, -80.1937, "America/New_York", false, Seq("Miami"), Seq(
    ("AMC Tamiami 14", "AMC Tamiami 14", "amc-tamiami-14", "Florida", "Miami"),
    ("CMX Brickell City Centre", "CMX Brickell City Centre", "cmx-brickell-city-centre", "Florida", "Miami"),
    ("CMX Dolphin Miami", "CMX Dolphin Miami", "cmx-dolphin-miami", "Florida", "Miami"),
    ("LeJeune Cinema 6 Miami", "LeJeune Cinema 6 Miami", "lejeune-cinema-6-miami", "Florida", "Miami"),
    ("Nite Owl Drive-In", "Nite Owl Drive-In", "nite-owl-drive-in", "Florida", "Miami"),
    ("Regal Bistro at The Falls", "Regal Bistro at The Falls", "regal-bistro-at-the-falls", "Florida", "Miami"),
    ("Regal Kendall Village Miami", "Regal Kendall Village Miami", "regal-kendall-village-miami", "Florida", "Miami"),
    ("Regal Southland Mall Miami", "Regal Southland Mall Miami", "regal-southland-mall-miami", "Florida", "Miami"),
    ("Silverspot Miami", "Silverspot Miami", "silverspot-miami", "Florida", "Miami"),
    ("Koubek Center Theater (Miami Dade College)", "Koubek Center Theater (Miami Dade College)", "koubek-center-theater-miami-dade-college", "Florida", "Miami"),
    ("Tower Theater Miami", "Tower Theater Miami", "tower-theater-miami", "Florida", "Miami")
  ))
  private def p_omaha: R = ("omaha", "omaha-nebraska", "Omaha", "Nebraska", 41.2563, -95.9404, "America/Chicago", false, Seq("Omaha"), Seq(
    ("ACX Aksarben Cinema", "ACX Aksarben Cinema", "acx-aksarben-cinema", "Nebraska", "Omaha"),
    ("Alamo Drafthouse La Vista", "Alamo Drafthouse La Vista", "alamo-drafthouse-la-vista", "Nebraska", "Omaha"),
    ("AMC CLASSIC Westroads 14", "AMC CLASSIC Westroads 14", "amc-classic-westroads-14", "Nebraska", "Omaha"),
    ("B&B Omaha Oakview Plaza 14", "B&B Omaha Oakview Plaza 14", "bb-omaha-oakview-plaza-14", "Nebraska", "Omaha"),
    ("Film Streams' Dundee Theater", "Film Streams' Dundee Theater", "film-streams-dundee-theater", "Nebraska", "Omaha"),
    ("Film Streams' Ruth Sokolof Theater", "Film Streams' Ruth Sokolof Theater", "film-streams-ruth-sokolof-theater", "Nebraska", "Omaha"),
    ("Marcus Majestic of Omaha", "Marcus Majestic of Omaha", "marcus-majestic-of-omaha", "Nebraska", "Omaha"),
    ("Marcus Village Pointe Cinema Omaha", "Marcus Village Pointe Cinema Omaha", "marcus-village-pointe-cinema-omaha", "Nebraska", "Omaha")
  ))
  private def p_raleigh: R = ("raleigh", "raleigh-north-carolina", "Raleigh", "North Carolina", 35.7721, -78.6386, "America/New_York", false, Seq("Raleigh"), Seq(
    ("Alamo Drafthouse Raleigh", "Alamo Drafthouse Raleigh", "alamo-drafthouse-raleigh", "North Carolina", "Raleigh"),
    ("Cinemark Bistro Raleigh", "Cinemark Bistro Raleigh", "cinemark-bistro-raleigh", "North Carolina", "Raleigh"),
    ("Marbles IMAX", "Marbles IMAX", "marbles-imax", "North Carolina", "Caraleigh"),
    ("Marquee Wakefield Raleigh 12", "Marquee Wakefield Raleigh 12", "marquee-wakefield-raleigh-12", "North Carolina", "Raleigh"),
    ("Regal Brier Creek Raleigh", "Regal Brier Creek Raleigh", "regal-brier-creek-raleigh", "North Carolina", "Raleigh"),
    ("Regal North Hills Raleigh", "Regal North Hills Raleigh", "regal-north-hills-raleigh", "North Carolina", "Raleigh"),
    ("Rialto Raleigh", "Rialto Raleigh", "rialto-raleigh", "North Carolina", "Raleigh"),
    ("Triangle Drive In Raleigh", "Triangle Drive In Raleigh", "triangle-drive-in-raleigh", "North Carolina", "Raleigh"),
    ("North Carolina Museum of Art", "North Carolina Museum of Art", "north-carolina-museum-of-art", "North Carolina", "Raleigh"),
    ("Triangle Cinemas @ Six Forks Raleigh", "Triangle Cinemas @ Six Forks Raleigh", "triangle-cinemas-six-forks-raleigh", "North Carolina", "Raleigh")
  ))
  private def p_kansas_city: R = ("kansas-city", "kansas-city-missouri", "Kansas City", "Missouri", 39.0997, -94.5786, "America/Chicago", false, Seq("Kansas City"), Seq(
    ("AMC Barrywoods 24", "AMC Barrywoods 24", "amc-barrywoods-24", "Missouri", "Kansas City"),
    ("AMC Ward Parkway 14", "AMC Ward Parkway 14", "amc-ward-parkway-14", "Missouri", "Kansas City"),
    ("B&B Northland 14 Kansas City", "B&B Northland 14 Kansas City", "bb-northland-14-kansas-city", "Missouri", "Kansas City"),
    ("Rooftop Cinema Crossroads Kansas City", "Rooftop Cinema Crossroads Kansas City", "rooftop-cinema-crossroads-kansas-city", "Missouri", "Kansas City"),
    ("Screenland Armour Kansas City", "Screenland Armour Kansas City", "screenland-armour-kansas-city", "Missouri", "Kansas City"),
    ("Regnier Extreme Screen Theatre", "Regnier Extreme Screen Theatre", "regnier-extreme-screen-theatre", "Missouri", "Kansas City")
  ))
  private def p_long_beach: R = ("long-beach", "long-beach-california", "Long Beach", "California", 33.767, -118.1892, "America/Los_Angeles", false, Seq("Long Beach"), Seq(
    ("AMC Marina Pacifica 12 Long Beach", "AMC Marina Pacifica 12 Long Beach", "amc-marina-pacifica-12-long-beach", "California", "Long Beach"),
    ("Art Theatre of Long Beach", "Art Theatre of Long Beach", "art-theatre-of-long-beach", "California", "Long Beach"),
    ("Cinemark at The Pike Outlets", "Cinemark at The Pike Outlets", "cinemark-at-the-pike-outlets", "California", "Long Beach"),
    ("Regal Edwards Long Beach", "Regal Edwards Long Beach", "regal-edwards-long-beach", "California", "Long Beach")
  ))
  private def p_mesa: R = ("mesa", "mesa-arizona", "Mesa", "Arizona", 33.4223, -111.8226, "America/Phoenix", false, Seq("Mesa"), Seq(
    ("AMC Mesa Grande 14", "AMC Mesa Grande 14", "amc-mesa-grande-14", "Arizona", "Mesa"),
    ("AMC Superstition East 12", "AMC Superstition East 12", "amc-superstition-east-12", "Arizona", "Mesa"),
    ("Cinemark Mesa 16", "Cinemark Mesa 16", "cinemark-mesa-16", "Arizona", "Mesa"),
    ("Fat Cats Mesa", "Fat Cats Mesa", "fat-cats-mesa", "Arizona", "Mesa"),
    ("Harkins Superstition Springs 25", "Harkins Superstition Springs 25", "harkins-superstition-springs-25", "Arizona", "Mesa"),
    ("Picture Show Superstition Springs", "Picture Show Superstition Springs", "picture-show-superstition-springs", "Arizona", "Mesa")
  ))
  private def p_colorado_springs: R = ("colorado-springs", "colorado-springs-colorado", "Colorado Springs", "Colorado", 38.8339, -104.8214, "America/Denver", false, Seq("Colorado Springs"), Seq(
    ("AMC Chapel Hills 13", "AMC Chapel Hills 13", "amc-chapel-hills-13", "Colorado", "Colorado Springs"),
    ("Cinemark Carefree Circle", "Cinemark Carefree Circle", "cinemark-carefree-circle", "Colorado", "Colorado Springs"),
    ("Cinemark Tinseltown Colorado Springs", "Cinemark Tinseltown Colorado Springs", "cinemark-tinseltown-colorado-springs", "Colorado", "Colorado Springs"),
    ("Icon Cinemas Colorado Springs", "Icon Cinemas Colorado Springs", "icon-cinemas-colorado-springs", "Colorado", "Colorado Springs"),
    ("Picture Show at Citadel Crossing", "Picture Show at Citadel Crossing", "picture-show-at-citadel-crossing", "Colorado", "Colorado Springs"),
    ("Regal Interquest Colorado Springs", "Regal Interquest Colorado Springs", "regal-interquest-colorado-springs", "Colorado", "Colorado Springs"),
    ("RoadHouse Colorado Springs", "RoadHouse Colorado Springs", "roadhouse-colorado-springs", "Colorado", "Colorado Springs")
  ))
  private def p_virginia_beach: R = ("virginia-beach", "virginia-beach-virginia", "Virginia Beach", "Virginia", 36.8529, -75.978, "America/New_York", false, Seq("Virginia Beach"), Seq(
    ("AMC Lynnhaven 18", "AMC Lynnhaven 18", "amc-lynnhaven-18", "Virginia", "Virginia Beach"),
    ("Regal Columbus Virginia Beach", "Regal Columbus Virginia Beach", "regal-columbus-virginia-beach", "Virginia", "Virginia Beach"),
    ("Regal Strawbridge Virginia Beach", "Regal Strawbridge Virginia Beach", "regal-strawbridge-virginia-beach", "Virginia", "Virginia Beach")
  ))
  private def p_oakland: R = ("oakland", "oakland-california", "Oakland", "California", 37.8044, -122.2708, "America/Los_Angeles", false, Seq("Oakland"), Seq(
    ("Grand Lake Theatre Oakland", "Grand Lake Theatre Oakland", "grand-lake-theatre-oakland", "California", "Oakland"),
    ("Landmark Piedmont", "Landmark Piedmont", "landmark-piedmont", "California", "Oakland"),
    ("New Parkway Theater Oakland", "New Parkway Theater Oakland", "new-parkway-theater-oakland", "California", "Oakland"),
    ("Regal Jack London Oakland", "Regal Jack London Oakland", "regal-jack-london-oakland", "California", "Oakland")
  ))
  private def p_san_juan: R = ("san-juan", "san-juan-puerto-rico", "San Juan", "Puerto Rico", 18.4663, -66.1057, "America/Puerto_Rico", false, Seq("San Juan"), Seq(
    ("Caribbean Cinemas Distrito VIP Cinemas", "Caribbean Cinemas Distrito VIP Cinemas", "caribbean-cinemas-distrito-vip-cinemas", "Puerto Rico", "San Juan"),
    ("Caribbean Cinemas Metro", "Caribbean Cinemas Metro", "caribbean-cinemas-metro", "Puerto Rico", "Santurce"),
    ("Caribbean Cinemas Montehiedra", "Caribbean Cinemas Montehiedra", "caribbean-cinemas-montehiedra", "Puerto Rico", "San Juan"),
    ("Caribbean Cinemas Plaza Las Américas", "Caribbean Cinemas Plaza Las Américas", "caribbean-cinemas-plaza-las-am-ricas", "Puerto Rico", "San Juan"),
    ("Caribbean Popular Fine Arts", "Caribbean Popular Fine Arts", "caribbean-popular-fine-arts", "Puerto Rico", "San Juan")
  ))
  private def p_tampa: R = ("tampa", "tampa-florida", "Tampa", "Florida", 27.9475, -82.4584, "America/New_York", false, Seq("Tampa"), Seq(
    ("AMC Highwoods 20", "AMC Highwoods 20", "amc-highwoods-20", "Florida", "Tampa"),
    ("AMC Veterans 24", "AMC Veterans 24", "amc-veterans-24", "Florida", "Tampa"),
    ("AMC West Shore 14", "AMC West Shore 14", "amc-west-shore-14", "Florida", "Tampa"),
    ("CMX CinéBistro at Hyde Park Village", "CMX CinéBistro at Hyde Park Village", "cmx-cinbistro-at-hyde-park-village", "Florida", "Tampa"),
    ("NCG Citrus Park Tampa", "NCG Citrus Park Tampa", "ncg-citrus-park-tampa", "Florida", "Tampa"),
    ("Sun-Ray Cinema Tampa", "Sun-Ray Cinema Tampa", "sun-ray-cinema-tampa", "Florida", "Tampa"),
    ("Tampa Theatre", "Tampa Theatre", "tampa-theatre", "Florida", "Tampa")
  ))
  private def p_tulsa: R = ("tulsa", "tulsa-oklahoma", "Tulsa", "Oklahoma", 36.154, -95.9928, "America/Chicago", false, Seq("Tulsa"), Seq(
    ("Admiral Twin Drive-in Tulsa", "Admiral Twin Drive-in Tulsa", "admiral-twin-drive-in-tulsa", "Oklahoma", "Tulsa"),
    ("AMC CLASSIC Tulsa Hills 12", "AMC CLASSIC Tulsa Hills 12", "amc-classic-tulsa-hills-12", "Oklahoma", "Tulsa"),
    ("AMC Southroads 20 Tulsa", "AMC Southroads 20 Tulsa", "amc-southroads-20-tulsa", "Oklahoma", "Tulsa"),
    ("B&B Tulsa Starworld 20", "B&B Tulsa Starworld 20", "bb-tulsa-starworld-20", "Oklahoma", "Tulsa"),
    ("Cinemark Tulsa 17", "Cinemark Tulsa 17", "cinemark-tulsa-17", "Oklahoma", "Tulsa"),
    ("Cinergy Tulsa 8", "Cinergy Tulsa 8", "cinergy-tulsa-8", "Oklahoma", "Tulsa"),
    ("Circle Cinema Tulsa", "Circle Cinema Tulsa", "circle-cinema-tulsa", "Oklahoma", "Tulsa"),
    ("Eton Tulsa 6", "Eton Tulsa 6", "eton-tulsa-6", "Oklahoma", "Tulsa")
  ))
  private def p_minneapolis: R = ("minneapolis", "minneapolis-minnesota", "Minneapolis", "Minnesota", 44.98, -93.2638, "America/Chicago", false, Seq("Minneapolis"), Seq(
    ("Landmark Lagoon Cinema", "Landmark Lagoon Cinema", "landmark-lagoon-cinema", "Minnesota", "Minneapolis"),
    ("Main Cinema Minneapolis", "Main Cinema Minneapolis", "main-cinema-minneapolis", "Minnesota", "Minneapolis"),
    ("Parkway Theatre Minneapolis", "Parkway Theatre Minneapolis", "parkway-theatre-minneapolis", "Minnesota", "Minneapolis"),
    ("Riverview Theatre Minneapolis", "Riverview Theatre Minneapolis", "riverview-theatre-minneapolis", "Minnesota", "Minneapolis"),
    ("Trylon Cinema Minneapolis", "Trylon Cinema Minneapolis", "trylon-cinema-minneapolis", "Minnesota", "Minneapolis")
  ))
  private def p_wichita: R = ("wichita", "wichita-kansas", "Wichita", "Kansas", 37.6922, -97.3375, "America/Chicago", false, Seq("Wichita"), Seq(
    ("AMC Northrock 14 Wichita", "AMC Northrock 14 Wichita", "amc-northrock-14-wichita", "Kansas", "Wichita"),
    ("Boulevard at Towne West", "Boulevard at Towne West", "boulevard-at-towne-west", "Kansas", "Wichita"),
    ("Boulevard Theatres at Old Town", "Boulevard Theatres at Old Town", "boulevard-theatres-at-old-town", "Kansas", "Wichita"),
    ("Regal Warren East", "Regal Warren East", "regal-warren-east", "Kansas", "Wichita"),
    ("Regal Warren West", "Regal Warren West", "regal-warren-west", "Kansas", "Wichita"),
    ("Starlite Drive-in Wichita", "Starlite Drive-in Wichita", "starlite-drive-in-wichita", "Kansas", "Wichita"),
    ("Orpheum Theatre Wichita", "Orpheum Theatre Wichita", "orpheum-theatre-wichita", "Kansas", "Wichita"),
    ("Tallgrass Film Center Wichita", "Tallgrass Film Center Wichita", "tallgrass-film-center-wichita", "Kansas", "Wichita")
  ))
  private def p_arlington: R = ("arlington", "arlington-texas", "Arlington", "Texas", 32.7357, -97.1081, "America/Chicago", false, Seq("Arlington"), Seq(
    ("LOOK Cinemas Arlington", "LOOK Cinemas Arlington", "look-cinemas-arlington", "Texas", "Arlington"),
    ("Studio Movie Grill Arlington Lincoln Square", "Studio Movie Grill Arlington Lincoln Square", "studio-movie-grill-arlington-lincoln-square", "Texas", "Arlington")
  ))
  private def p_bakersfield: R = ("bakersfield", "bakersfield-california", "Bakersfield", "California", 35.3733, -119.0187, "America/Los_Angeles", false, Seq("Bakersfield"), Seq(
    ("AMC Bakersfield 6", "AMC Bakersfield 6", "amc-bakersfield-6", "California", "Bakersfield"),
    ("Maya Bakersfield", "Maya Bakersfield", "maya-bakersfield", "California", "Bakersfield"),
    ("Reading Cinemas Valley Plaza", "Reading Cinemas Valley Plaza", "reading-cinemas-valley-plaza", "California", "Bakersfield"),
    ("Regal Edwards Bakersfield", "Regal Edwards Bakersfield", "regal-edwards-bakersfield", "California", "Bakersfield"),
    ("Studio Movie Grill Bakersfield", "Studio Movie Grill Bakersfield", "studio-movie-grill-bakersfield", "California", "Bakersfield"),
    ("Historic Bakersfield Fox Theater", "Historic Bakersfield Fox Theater", "historic-bakersfield-fox-theater", "California", "Bakersfield")
  ))
  private def p_cleveland: R = ("cleveland", "cleveland-ohio", "Cleveland", "Ohio", 41.4995, -81.6954, "America/New_York", false, Seq("Cleveland"), Seq(
    ("Atlas Cinemas Shaker Square 6", "Atlas Cinemas Shaker Square 6", "atlas-cinemas-shaker-square-6", "Ohio", "Cleveland"),
    ("Capitol Theatre Cleveland", "Capitol Theatre Cleveland", "capitol-theatre-cleveland", "Ohio", "Cleveland"),
    ("Cleveland Institute Of Art Cinematheque", "Cleveland Institute Of Art Cinematheque", "cleveland-institute-of-art-cinematheque", "Ohio", "Cleveland")
  ))
  private def p_new_orleans: R = ("new-orleans", "new-orleans-louisiana", "New Orleans", "Louisiana", 29.9547, -90.0751, "America/Chicago", false, Seq("New Orleans"), Seq(
    ("Broad Theater New Orleans", "Broad Theater New Orleans", "broad-theater-new-orleans", "Louisiana", "New Orleans"),
    ("Prytania Theatres at Canal Place", "Prytania Theatres at Canal Place", "prytania-theatres-at-canal-place", "Louisiana", "New Orleans"),
    ("Prytania Uptown New Orleans", "Prytania Uptown New Orleans", "prytania-uptown-new-orleans", "Louisiana", "New Orleans")
  ))
  private def p_aurora: R = ("aurora", "aurora-colorado", "Aurora", "Colorado", 39.7294, -104.8319, "America/Denver", false, Seq("Aurora"), Seq(
    ("AMC Arapahoe Crossing 16", "AMC Arapahoe Crossing 16", "amc-arapahoe-crossing-16", "Colorado", "Aurora"),
    ("AMC Southlands 16 Aurora", "AMC Southlands 16 Aurora", "amc-southlands-16-aurora", "Colorado", "Aurora"),
    ("Cinemark Century Aurora", "Cinemark Century Aurora", "cinemark-century-aurora", "Colorado", "Aurora"),
    ("Movie Tavern Aurora", "Movie Tavern Aurora", "movie-tavern-aurora", "Colorado", "Aurora")
  ))
  private def p_honolulu: R = ("honolulu", "honolulu-hawaii", "Honolulu", "Hawaii", 21.3069, -157.8583, "Pacific/Honolulu", false, Seq("Honolulu"), Seq(
    ("Consolidated Theatres Kahala Honolulu", "Consolidated Theatres Kahala Honolulu", "consolidated-theatres-kahala-honolulu", "Hawaii", "Honolulu"),
    ("Consolidated Theatres Ward Honolulu", "Consolidated Theatres Ward Honolulu", "consolidated-theatres-ward-honolulu", "Hawaii", "Honolulu"),
    ("Regal Dole Cannery", "Regal Dole Cannery", "regal-dole-cannery", "Hawaii", "Honolulu")
  ))
  private def p_anaheim: R = ("anaheim", "anaheim-california", "Anaheim", "California", 33.8353, -117.9145, "America/Los_Angeles", false, Seq("Anaheim"), Seq(
    ("AMC Anaheim GardenWalk 6", "AMC Anaheim GardenWalk 6", "amc-anaheim-gardenwalk-6", "California", "Anaheim")
  ))
  private def p_orlando: R = ("orlando", "orlando-florida", "Orlando", "Florida", 28.5383, -81.3792, "America/New_York", false, Seq("Orlando"), Seq(
    ("Cinemark at Universal CityWalk Orlando", "Cinemark at Universal CityWalk Orlando", "cinemark-at-universal-citywalk-orlando", "Florida", "Orlando"),
    ("Cinemark Orlando", "Cinemark Orlando", "cinemark-orlando", "Florida", "Orlando"),
    ("Epic Theatres Lee Vista", "Epic Theatres Lee Vista", "epic-theatres-lee-vista", "Florida", "Orlando"),
    ("Regal Pointe Orlando", "Regal Pointe Orlando", "regal-pointe-orlando", "Florida", "Orlando"),
    ("Regal Waterford Lakes Orlando", "Regal Waterford Lakes Orlando", "regal-waterford-lakes-orlando", "Florida", "Orlando"),
    ("South Chase Orlando 7", "South Chase Orlando 7", "south-chase-orlando-7", "Florida", "Orlando"),
    ("CMX Plaza Cinema Cafe Orlando", "CMX Plaza Cinema Cafe Orlando", "cmx-plaza-cinema-cafe-orlando", "Florida", "Orlando"),
    ("Premiere 14 Fashion Square Orlando", "Premiere 14 Fashion Square Orlando", "premiere-14-fashion-square-orlando", "Florida", "Orlando")
  ))
  private def p_lexington: R = ("lexington", "lexington-kentucky", "Lexington", "Kentucky", 37.9887, -84.4777, "America/New_York", false, Seq("Lexington"), Seq(
    ("Cinemark Fayette Mall", "Cinemark Fayette Mall", "cinemark-fayette-mall", "Kentucky", "Lexington"),
    ("Kentucky Lexington", "Kentucky Lexington", "kentucky-lexington", "Kentucky", "Lexington"),
    ("LexLive 10", "LexLive 10", "lexlive-10", "Kentucky", "Lexington"),
    ("Regal Hamburg Pavilion Lexington", "Regal Hamburg Pavilion Lexington", "regal-hamburg-pavilion-lexington", "Kentucky", "Lexington"),
    ("Gatton Student Center Cinema", "Gatton Student Center Cinema", "gatton-student-center-cinema", "Kentucky", "Lexington")
  ))
  private def p_riverside: R = ("riverside", "riverside-california", "Riverside", "California", 33.9534, -117.3962, "America/Los_Angeles", false, Seq("Riverside"), Seq(
    ("AMC Tyler Galleria 16", "AMC Tyler Galleria 16", "amc-tyler-galleria-16", "California", "Riverside"),
    ("Galaxy Theatres Mission Grove", "Galaxy Theatres Mission Grove", "galaxy-theatres-mission-grove", "California", "Riverside"),
    ("Regal Riverside Plaza", "Regal Riverside Plaza", "regal-riverside-plaza", "California", "Riverside"),
    ("Rubidoux Drive-In Theatre", "Rubidoux Drive-In Theatre", "rubidoux-drive-in-theatre", "California", "Riverside"),
    ("Van Buren Drive-In Theatre", "Van Buren Drive-In Theatre", "van-buren-drive-in-theatre", "California", "Riverside")
  ))
  private def p_corpus_christi: R = ("corpus-christi", "corpus-christi-texas", "Corpus Christi", "Texas", 27.8006, -97.3964, "America/Chicago", false, Seq("Corpus Christi"), Seq(
    ("Alamo Drafthouse Corpus Christi", "Alamo Drafthouse Corpus Christi", "alamo-drafthouse-corpus-christi", "Texas", "Corpus Christi"),
    ("AMC Corpus Christi 16", "AMC Corpus Christi 16", "amc-corpus-christi-16", "Texas", "Corpus Christi"),
    ("Century Corpus Christi 16", "Century Corpus Christi 16", "century-corpus-christi-16", "Texas", "Corpus Christi"),
    ("Movies Inc Corpus Christi", "Movies Inc Corpus Christi", "movies-inc-corpus-christi", "Texas", "Corpus Christi")
  ))
  private def p_cincinnati: R = ("cincinnati", "cincinnati-ohio", "Cincinnati", "Ohio", 39.1271, -84.5144, "America/New_York", false, Seq("Cincinnati"), Seq(
    ("Cinemark Oakley Station Cincinnati", "Cinemark Oakley Station Cincinnati", "cinemark-oakley-station-cincinnati", "Ohio", "Cincinnati"),
    ("Cinemark Western Hills 14", "Cinemark Western Hills 14", "cinemark-western-hills-14", "Ohio", "Cincinnati"),
    ("Esquire Theatre Cincinnati", "Esquire Theatre Cincinnati", "esquire-theatre-cincinnati", "Ohio", "Cincinnati"),
    ("Kenwood Theatre Cincinnati", "Kenwood Theatre Cincinnati", "kenwood-theatre-cincinnati", "Ohio", "Cincinnati"),
    ("OMNIMAX Theatre – Robert D. Lindner Family", "OMNIMAX Theatre – Robert D. Lindner Family", "omnimax-theatre---robert-d-lindner-family", "Ohio", "Cincinnati"),
    ("Parkland Theatre Cincinnati", "Parkland Theatre Cincinnati", "parkland-theatre-cincinnati", "Ohio", "Cincinnati"),
    ("RJ Cinema and Distillery 8", "RJ Cinema and Distillery 8", "rj-cinema-and-distillery-8", "Ohio", "Cincinnati"),
    ("ACX Cinema Blue Ash 7+", "ACX Cinema Blue Ash 7+", "acx-cinema-blue-ash-7", "Ohio", "Cincinnati"),
    ("Cincinnati World Cinema – The Garfield Theatre", "Cincinnati World Cinema – The Garfield Theatre", "cincinnati-world-cinema-the-garfield-theatre", "Ohio", "Cincinnati"),
    ("Mariemont Theatre", "Mariemont Theatre", "mariemont-theatre", "Ohio", "Cincinnati")
  ))
  private def p_santa_ana: R = ("santa-ana", "santa-ana-california", "Santa Ana", "California", 33.7456, -117.8678, "America/Los_Angeles", false, Seq("Santa Ana"), Seq(
    ("Frida Cinema Santa Ana", "Frida Cinema Santa Ana", "frida-cinema-santa-ana", "California", "Santa Ana"),
    ("Picture Show at Main Place Mall", "Picture Show at Main Place Mall", "picture-show-at-main-place-mall", "California", "Santa Ana")
  ))
  private def p_stockton_california: R = ("stockton-california", "stockton-california", "Stockton", "California", 37.9577, -121.2908, "America/Los_Angeles", false, Seq("Stockton"), Seq(
    ("Regal Holiday Stockton", "Regal Holiday Stockton", "regal-holiday-stockton", "California", "Stockton"),
    ("Regal Stockton City Centre", "Regal Stockton City Centre", "regal-stockton-city-centre", "California", "Stockton"),
    ("Bob Hope Theatre Stockton", "Bob Hope Theatre Stockton", "bob-hope-theatre-stockton", "California", "Stockton")
  ))
  private def p_pittsburgh: R = ("pittsburgh", "pittsburgh-pennsylvania", "Pittsburgh", "Pennsylvania", 40.4406, -79.9959, "America/New_York", false, Seq("Pittsburgh"), Seq(
    ("AMC South Hills Village 10 Pittsburgh", "AMC South Hills Village 10 Pittsburgh", "amc-south-hills-village-10-pittsburgh", "Pennsylvania", "Pittsburgh"),
    ("Cinemark North Hills Pittsburgh", "Cinemark North Hills Pittsburgh", "cinemark-north-hills-pittsburgh", "Pennsylvania", "Pittsburgh"),
    ("Harris Theatre Pittsburgh", "Harris Theatre Pittsburgh", "harris-theatre-pittsburgh", "Pennsylvania", "Pittsburgh"),
    ("Manor Cinema Pittsburgh", "Manor Cinema Pittsburgh", "manor-cinema-pittsburgh", "Pennsylvania", "Pittsburgh"),
    ("Row House Cinema Lawrenceville", "Row House Cinema Lawrenceville", "row-house-cinema-lawrenceville", "Pennsylvania", "Pittsburgh"),
    ("Row House Cinemas Hollywood", "Row House Cinemas Hollywood", "row-house-cinemas-hollywood", "Pennsylvania", "Pittsburgh"),
    ("The Rangos Giant Cinema Pittsburgh", "The Rangos Giant Cinema Pittsburgh", "the-rangos-giant-cinema-pittsburgh", "Pennsylvania", "Pittsburgh"),
    ("Tuxedo Waterworks (formerly MovieScoop)", "Tuxedo Waterworks (formerly MovieScoop)", "moviescoop-waterworks", "Pennsylvania", "Pittsburgh"),
    ("Carnegie Mellon University", "Carnegie Mellon University", "carnegie-mellon-university", "Pennsylvania", "Pittsburgh")
  ))
  private def p_saint_paul: R = ("saint-paul", "saint-paul-minnesota", "Saint Paul", "Minnesota", 44.9444, -93.0933, "America/Chicago", false, Seq("Saint Paul"), Seq(
    ("Mann Grandview St Paul", "Mann Grandview St Paul", "mann-grandview-st-paul", "Minnesota", "Saint Paul"),
    ("Mann Highland St Paul", "Mann Highland St Paul", "mann-highland-st-paul", "Minnesota", "Saint Paul"),
    ("FilmNorth Saint Paul", "FilmNorth Saint Paul", "filmnorth-saint-paul", "Minnesota", "Saint Paul")
  ))
  private def p_lincoln: R = ("lincoln", "lincoln-nebraska", "Lincoln", "Nebraska", 40.8, -96.667, "America/Chicago", false, Seq("Lincoln"), Seq(
    ("Marcus East Park Cinema", "Marcus East Park Cinema", "marcus-east-park-cinema", "Nebraska", "Lincoln"),
    ("Marcus Edgewood Cinema", "Marcus Edgewood Cinema", "marcus-edgewood-cinema", "Nebraska", "Lincoln"),
    ("Marcus Lincoln Grand Cinema", "Marcus Lincoln Grand Cinema", "marcus-lincoln-grand-cinema", "Nebraska", "Lincoln"),
    ("Marcus South Pointe Cinema", "Marcus South Pointe Cinema", "marcus-south-pointe-cinema", "Nebraska", "Lincoln"),
    ("Mary Riepma Ross Media Arts Center", "Mary Riepma Ross Media Arts Center", "mary-riepma-ross-media-arts-center", "Nebraska", "Lincoln")
  ))
  private def p_anchorage: R = ("anchorage", "anchorage-alaska", "Anchorage", "Alaska", 61.2181, -149.9003, "America/Anchorage", false, Seq("Anchorage"), Seq(
    ("Bear Tooth Theatrepub", "Bear Tooth Theatrepub", "bear-tooth-theatrepub", "Alaska", "Anchorage"),
    ("Cinemark Century Anchorage 16", "Cinemark Century Anchorage 16", "cinemark-century-anchorage-16", "Alaska", "Anchorage"),
    ("Regal Dimond Center", "Regal Dimond Center", "regal-dimond-center", "Alaska", "Anchorage"),
    ("Regal Tikahtnu", "Regal Tikahtnu", "regal-tikahtnu", "Alaska", "Anchorage")
  ))
  private def p_henderson: R = ("henderson", "henderson-nevada", "Henderson", "Nevada", 36.0397, -114.9819, "America/Los_Angeles", false, Seq("Henderson"), Seq(
    ("Cinemark Cinedome Henderson 12", "Cinemark Cinedome Henderson 12", "cinemark-cinedome-henderson-12", "Nevada", "Henderson"),
    ("Galaxy Theatres Green Valley", "Galaxy Theatres Green Valley", "galaxy-theatres-green-valley", "Nevada", "Henderson"),
    ("Regal Ranch Green Valley Henderson", "Regal Ranch Green Valley Henderson", "regal-ranch-green-valley-henderson", "Nevada", "Henderson"),
    ("Regal Sunset Station Henderson", "Regal Sunset Station Henderson", "regal-sunset-station-henderson", "Nevada", "Henderson")
  ))
  private def p_greensboro: R = ("greensboro", "greensboro-north-carolina", "Greensboro", "North Carolina", 36.0726, -79.792, "America/New_York", false, Seq("Greensboro"), Seq(
    ("Golden Ticket Greensboro Ale House", "Golden Ticket Greensboro Ale House", "golden-ticket-greensboro-ale-house", "North Carolina", "Greensboro"),
    ("Regal Greensboro Grande", "Regal Greensboro Grande", "regal-greensboro-grande", "North Carolina", "Greensboro"),
    ("Santikos AmStar Four Seasons Greensboro", "Santikos AmStar Four Seasons Greensboro", "santikos-amstar-four-seasons-greensboro", "North Carolina", "Greensboro"),
    ("Carolina Theatre Greensboro", "Carolina Theatre Greensboro", "carolina-theatre-greensboro", "North Carolina", "Greensboro"),
    ("RED Cinemas Stadium 15 Greensboro", "RED Cinemas Stadium 15 Greensboro", "red-cinemas-stadium-15-greensboro", "North Carolina", "Greensboro")
  ))
  private def p_plano: R = ("plano", "plano-texas", "Plano", "Texas", 33.0198, -96.6989, "America/Chicago", false, Seq("Plano"), Seq(
    ("Cinemark Legacy Plano", "Cinemark Legacy Plano", "cinemark-legacy-plano", "Texas", "Plano"),
    ("Cinemark Plano 10", "Cinemark Plano 10", "cinemark-plano-10", "Texas", "Plano"),
    ("Cinemark West Plano 20", "Cinemark West Plano 20", "cinemark-west-plano-20", "Texas", "Plano"),
    ("Studio Movie Grill Plano", "Studio Movie Grill Plano", "studio-movie-grill-plano", "Texas", "Plano")
  ))
  private def p_newark: R = ("newark", "newark-new-jersey", "Newark", "New Jersey", 40.7357, -74.1724, "America/New_York", false, Seq("Newark"), Seq(
    ("Cityplex Cinema Newark", "Cityplex Cinema Newark", "cityplex-cinema-newark", "New Jersey", "Newark")
  ))
  private def p_madison: R = ("madison", "madison-wisconsin", "Madison", "Wisconsin", 43.0731, -89.4012, "America/Chicago", false, Seq("Madison"), Seq(
    ("Flix Brewhouse Madison", "Flix Brewhouse Madison", "flix-brewhouse-madison", "Wisconsin", "Madison"),
    ("Marcus Point Madison 16", "Marcus Point Madison 16", "marcus-point-madison-16", "Wisconsin", "Madison"),
    ("Marquee Cinema at Union South", "Marquee Cinema at Union South", "marquee-cinema-at-union-south", "Wisconsin", "Madison"),
    ("Cinematheque (UW)", "Cinematheque (UW)", "cinematheque-uw", "Wisconsin", "Madison")
  ))
  private def p_st_louis: R = ("st-louis", "st-louis-missouri", "St. Louis", "Missouri", 38.6273, -90.1979, "America/Chicago", false, Seq("St. Louis"), Seq(
    ("24:1 Cinema St Louis", "24:1 Cinema St Louis", "241-cinema-st-louis", "Missouri", "St. Louis"),
    ("Alamo Drafthouse City Foundry", "Alamo Drafthouse City Foundry", "alamo-drafthouse-city-foundry", "Missouri", "St. Louis"),
    ("AMC Esquire 7", "AMC Esquire 7", "amc-esquire-7", "Missouri", "St. Louis"),
    ("Arkadin Cinema and Bar", "Arkadin Cinema and Bar", "arkadin-cinema-and-bar", "Missouri", "St. Louis"),
    ("Chase Park Plaza Cinemas", "Chase Park Plaza Cinemas", "chase-park-plaza-cinemas", "Missouri", "St. Louis"),
    ("Galleria 6 Cinemas", "Galleria 6 Cinemas", "galleria-6-cinemas", "Missouri", "St. Louis"),
    ("Hi-Pointe Theatre", "Hi-Pointe Theatre", "hi-pointe-theatre", "Missouri", "St. Louis"),
    ("Marcus Ronnie's Cinema St. Louis", "Marcus Ronnie's Cinema St. Louis", "marcus-ronnies-cinema-st-louis", "Missouri", "St. Louis"),
    ("MX Movies & Bar St. Louis", "MX Movies & Bar St. Louis", "mx-movies-bar-st-louis", "Missouri", "Saint Louis"),
    ("Omnimax Theater St. Louis", "Omnimax Theater St. Louis", "omnimax-theater-st-louis", "Missouri", "St. Louis")
  ))
  private def p_chula_vista: R = ("chula-vista", "chula-vista-california", "Chula Vista", "California", 32.6401, -117.0842, "America/Los_Angeles", false, Seq("Chula Vista"), Seq(
    ("AMC Chula Vista 10", "AMC Chula Vista 10", "amc-chula-vista-10", "California", "Chula Vista"),
    ("AMC Otay Ranch 12", "AMC Otay Ranch 12", "amc-otay-ranch-12", "California", "Chula Vista"),
    ("Regal Rancho Del Rey Chula Vista", "Regal Rancho Del Rey Chula Vista", "regal-rancho-del-rey-chula-vista", "California", "Chula Vista")
  ))
  private def p_toledo: R = ("toledo", "toledo-ohio", "Toledo", "Ohio", 41.6639, -83.5552, "America/New_York", false, Seq("Toledo"), Seq(
    ("Cinemark Franklin Park 16", "Cinemark Franklin Park 16", "cinemark-franklin-park-16", "Ohio", "Toledo"),
    ("Valentine Toledo", "Valentine Toledo", "valentine-toledo", "Ohio", "East Toledo")
  ))
  private def p_jersey_city: R = ("jersey-city", "jersey-city-new-jersey", "Jersey City", "New Jersey", 40.7282, -74.0776, "America/New_York", false, Seq("Jersey City"), Seq(
    ("AMC Newport Centre 11 Jersey City", "AMC Newport Centre 11 Jersey City", "amc-newport-centre-11-jersey-city", "New Jersey", "Jersey City")
  ))
  private def p_reno: R = ("reno", "reno-nevada", "Reno", "Nevada", 39.5296, -119.8138, "America/Los_Angeles", false, Seq("Reno"), Seq(
    ("Cinemark Century Park Lane 16", "Cinemark Century Park Lane 16", "cinemark-century-park-lane-16", "Nevada", "Reno"),
    ("Cinemark Century Summit Sierra", "Cinemark Century Summit Sierra", "cinemark-century-summit-sierra", "Nevada", "Reno"),
    ("West Wind El Rancho Reno Drive-In", "West Wind El Rancho Reno Drive-In", "west-wind-el-rancho-reno-drive-in", "Nevada", "Reno")
  ))
  private def p_chandler: R = ("chandler", "chandler-arizona", "Chandler", "Arizona", 33.3062, -111.8413, "America/Phoenix", false, Seq("Chandler"), Seq(
    ("Harkins Chandler Crossroads 12", "Harkins Chandler Crossroads 12", "harkins-chandler-crossroads-12", "Arizona", "Chandler"),
    ("Harkins Chandler Fashion 20", "Harkins Chandler Fashion 20", "harkins-chandler-fashion-20", "Arizona", "Chandler"),
    ("Majestic Chandler", "Majestic Chandler", "majestic-chandler", "Arizona", "Chandler")
  ))
  private def p_fort_wayne: R = ("fort-wayne", "fort-wayne-indiana", "Fort Wayne", "Indiana", 41.1306, -85.1289, "America/Indiana/Indianapolis", false, Seq("Fort Wayne"), Seq(
    ("AMC CLASSIC Jefferson Point 18", "AMC CLASSIC Jefferson Point 18", "amc-classic-jefferson-point-18", "Indiana", "Fort Wayne"),
    ("Cinema Center Fort Wayne", "Cinema Center Fort Wayne", "cinema-center-fort-wayne", "Indiana", "Fort Wayne"),
    ("Regal Coldwater Crossing Fort Wayne", "Regal Coldwater Crossing Fort Wayne", "regal-coldwater-crossing-fort-wayne", "Indiana", "Fort Wayne")
  ))
  private def p_buffalo: R = ("buffalo", "buffalo-new-york", "Buffalo", "New York", 42.8865, -78.8784, "America/New_York", false, Seq("Buffalo"), Seq(
    ("Amherst Cinema Buffalo", "Amherst Cinema Buffalo", "amherst-cinema-buffalo", "New York", "Buffalo"),
    ("North Park Cinema Buffalo", "North Park Cinema Buffalo", "north-park-cinema-buffalo", "New York", "Buffalo"),
    ("Regal Elmwood Center Buffalo", "Regal Elmwood Center Buffalo", "regal-elmwood-center-buffalo", "New York", "Buffalo"),
    ("Regal Walden Galleria Stadium", "Regal Walden Galleria Stadium", "regal-walden-galleria-stadium", "New York", "Buffalo"),
    ("Scene One Market Arcade", "Scene One Market Arcade", "amc-market-arcade-8", "New York", "Buffalo"),
    ("McKinley Cinema Buffalo", "McKinley Cinema Buffalo", "mckinley-cinema-buffalo", "New York", "Buffalo")
  ))
  private def p_durham: R = ("durham", "durham-north-carolina", "Durham", "North Carolina", 35.994, -78.8986, "America/New_York", false, Seq("Durham"), Seq(
    ("AMC Southpoint 17", "AMC Southpoint 17", "amc-southpoint-17", "North Carolina", "Durham"),
    ("Carolina Theatre Durham", "Carolina Theatre Durham", "carolina-theatre-durham", "North Carolina", "Durham")
  ))
  private def p_st_petersburg: R = ("st-petersburg", "st-petersburg-florida", "St. Petersburg", "Florida", 27.7709, -82.6793, "America/New_York", false, Seq("St. Petersburg"), Seq(
    ("AMC Sundial 12", "AMC Sundial 12", "amc-sundial-12", "Florida", "St. Petersburg"),
    ("CMX Cinemas Tyrone Luxury 10", "CMX Cinemas Tyrone Luxury 10", "cmx-cinemas-tyrone-luxury-10", "Florida", "St. Petersburg"),
    ("Green Light Cinema", "Green Light Cinema", "green-light-cinema", "Florida", "St. Petersburg")
  ))
  private def p_irvine: R = ("irvine", "irvine-california", "Irvine", "California", 33.6695, -117.8231, "America/Los_Angeles", false, Seq("Irvine"), Seq(
    ("AMC Woodbridge Irvine 5", "AMC Woodbridge Irvine 5", "amc-woodbridge-irvine-5", "California", "Irvine"),
    ("Regal Irvine Spectrum", "Regal Irvine Spectrum", "regal-irvine-spectrum", "California", "Irvine"),
    ("Regal Market Place Irvine", "Regal Market Place Irvine", "regal-market-place-irvine", "California", "Irvine"),
    ("Regal University Town Center", "Regal University Town Center", "regal-university-town-center", "California", "Irvine")
  ))
  private def p_laredo: R = ("laredo", "laredo-texas", "Laredo", "Texas", 27.5064, -99.5075, "America/Chicago", false, Seq("Laredo"), Seq(
    ("Alamo Drafthouse Laredo", "Alamo Drafthouse Laredo", "alamo-drafthouse-laredo", "Texas", "Laredo"),
    ("Cinemark Mall Del Norte", "Cinemark Mall Del Norte", "cinemark-mall-del-norte", "Texas", "Laredo"),
    ("Sunset Drive-In Laredo", "Sunset Drive-In Laredo", "sunset-drive-in-laredo", "Texas", "Laredo")
  ))
  private def p_lubbock: R = ("lubbock", "lubbock-texas", "Lubbock", "Texas", 33.5779, -101.8552, "America/Chicago", false, Seq("Lubbock"), Seq(
    ("Cinemark Tinseltown Lubbock", "Cinemark Tinseltown Lubbock", "cinemark-tinseltown-lubbock", "Texas", "Lubbock"),
    ("Flix Brewhouse Lubbock", "Flix Brewhouse Lubbock", "alamo-drafthouse-lubbock", "Texas", "Lubbock"),
    ("Lubbock Premiere Cinema", "Lubbock Premiere Cinema", "lubbock-premiere-cinema", "Texas", "Lubbock"),
    ("Stars & Stripes Drive-in Lubbock", "Stars & Stripes Drive-in Lubbock", "stars-stripes-drive-in-lubbock", "Texas", "Lubbock")
  ))
  private def p_gilbert: R = ("gilbert", "gilbert-arizona", "Gilbert", "Arizona", 33.3528, -111.789, "America/Phoenix", false, Seq("Gilbert"), Seq(
    ("Fat Cats Gilbert", "Fat Cats Gilbert", "fat-cats-gilbert", "Arizona", "Gilbert"),
    ("Harkins SanTan Village 16", "Harkins SanTan Village 16", "harkins-santan-village-16", "Arizona", "Gilbert"),
    ("Majestic Gilbert", "Majestic Gilbert", "majestic-gilbert", "Arizona", "Gilbert"),
    ("Regal Gilbert", "Regal Gilbert", "regal-gilbert", "Arizona", "Gilbert")
  ))
  private def p_winston_salem: R = ("winston-salem", "winston-salem-north-carolina", "Winston-Salem", "North Carolina", 36.0999, -80.2442, "America/New_York", false, Seq("Winston-Salem"), Seq(
    ("a/perture cinema Winston-Salem", "a/perture cinema Winston-Salem", "aperture-cinema-winston-salem", "North Carolina", "Winston-Salem"),
    ("AMC Hanes 12 Winston-Salem", "AMC Hanes 12 Winston-Salem", "amc-hanes-12-winston-salem", "North Carolina", "Winston-Salem"),
    ("Marketplace Cinemas Winston-Salem", "Marketplace Cinemas Winston-Salem", "marketplace-cinemas-winston-salem", "North Carolina", "Winston-Salem"),
    ("Santikos Grand Winston-Salem", "Santikos Grand Winston-Salem", "santikos-grand-winston-salem", "North Carolina", "Winston-Salem"),
    ("ACE Theatre Complex", "ACE Theatre Complex", "ace-theatre-complex", "North Carolina", "Winston-Salem")
  ))
  private def p_glendale: R = ("glendale", "glendale-arizona", "Glendale", "Arizona", 33.5386, -112.186, "America/Phoenix", false, Seq("Glendale"), Seq(
    ("AMC Arrowhead 14", "AMC Arrowhead 14", "amc-arrowhead-14", "Arizona", "Glendale"),
    ("AMC Westgate 20", "AMC Westgate 20", "amc-westgate-20", "Arizona", "Glendale"),
    ("West Wind Glendale Drive-In", "West Wind Glendale Drive-In", "west-wind-glendale-drive-in", "Arizona", "Glendale")
  ))
  private def p_norfolk_virginia: R = ("norfolk-virginia", "norfolk-virginia", "Norfolk", "Virginia", 36.8468, -76.2852, "America/New_York", false, Seq("Norfolk"), Seq(
    ("Naro Expanded Cinema Norfolk", "Naro Expanded Cinema Norfolk", "naro-expanded-cinema-norfolk", "Virginia", "Norfolk"),
    ("Phoenix Main Gate Movies Norfolk", "Phoenix Main Gate Movies Norfolk", "phoenix-main-gate-movies-norfolk", "Virginia", "Norfolk"),
    ("Regal Macarthur Center Norfolk", "Regal Macarthur Center Norfolk", "regal-macarthur-center-norfolk", "Virginia", "Norfolk")
  ))
  private def p_hialeah: R = ("hialeah", "hialeah-florida", "Hialeah", "Florida", 25.8576, -80.2781, "America/New_York", false, Seq("Hialeah"), Seq(
    ("AMC Hialeah 12", "AMC Hialeah 12", "amc-hialeah-12", "Florida", "Hialeah")
  ))
  private def p_garland: R = ("garland", "garland-texas", "Garland", "Texas", 32.9126, -96.6389, "America/Chicago", false, Seq("Garland"), Seq(
    ("AMC Firewheel 18 Garland", "AMC Firewheel 18 Garland", "amc-firewheel-18-garland", "Texas", "Garland"),
    ("Cinemark Strike & Reel 8", "Cinemark Strike & Reel 8", "cinemark-strike-reel-8", "Texas", "Garland")
  ))
  private def p_scottsdale: R = ("scottsdale", "scottsdale-arizona", "Scottsdale", "Arizona", 33.5092, -111.899, "America/Phoenix", false, Seq("Scottsdale"), Seq(
    ("Harkins Camelview at Fashion Sq.", "Harkins Camelview at Fashion Sq.", "harkins-camelview-at-fashion-sq", "Arizona", "Scottsdale"),
    ("Harkins Shea 14", "Harkins Shea 14", "harkins-shea-14", "Arizona", "Scottsdale"),
    ("Landmark Scottsdale Quarter Theatre", "Landmark Scottsdale Quarter Theatre", "landmark-scottsdale-quarter-theatre", "Arizona", "Scottsdale"),
    ("RoadHouse Cinemas Scottsdale", "RoadHouse Cinemas Scottsdale", "roadhouse-cinemas-scottsdale", "Arizona", "Scottsdale")
  ))
  private def p_irving: R = ("irving", "irving-texas", "Irving", "Texas", 32.814, -96.9489, "America/Chicago", false, Seq("Irving"), Seq(
    ("Alamo Drafthouse Las Colinas", "Alamo Drafthouse Las Colinas", "alamo-drafthouse-las-colinas", "Texas", "Irving"),
    ("AMC Irving Mall 14", "AMC Irving Mall 14", "amc-irving-mall-14", "Texas", "Irving")
  ))
  private def p_boise: R = ("boise", "boise-idaho", "Boise", "Idaho", 43.6135, -116.2035, "America/Boise", false, Seq("Boise"), Seq(
    ("BoDo Cinema Boise", "BoDo Cinema Boise", "bodo-cinema-boise", "Idaho", "Boise"),
    ("Overland Park Cinemas Boise", "Overland Park Cinemas Boise", "overland-park-cinemas-boise", "Idaho", "Boise"),
    ("Regal Edwards Boise", "Regal Edwards Boise", "regal-edwards-boise", "Idaho", "Boise"),
    ("The Flicks Boise", "The Flicks Boise", "the-flicks-boise", "Idaho", "Boise"),
    ("Egyptian Theatre Boise", "Egyptian Theatre Boise", "egyptian-theatre-boise", "Idaho", "Boise")
  ))
  private def p_chesapeake: R = ("chesapeake", "chesapeake-virginia", "Chesapeake", "Virginia", 36.819, -76.2749, "America/New_York", false, Seq("Chesapeake"), Seq(
    ("Cinema Cafe Edinburgh Chesapeake", "Cinema Cafe Edinburgh Chesapeake", "cinema-cafe-edinburgh-chesapeake", "Virginia", "Chesapeake"),
    ("Cinema Cafe Greenbrier Mall Chesapeake", "Cinema Cafe Greenbrier Mall Chesapeake", "cinema-cafe-greenbrier-mall-chesapeake", "Virginia", "Chesapeake"),
    ("Cinemark Chesapeake Square", "Cinemark Chesapeake Square", "cinemark-chesapeake-square", "Virginia", "Chesapeake")
  ))
  private def p_north_las_vegas: R = ("north-las-vegas", "north-las-vegas-nevada", "North Las Vegas", "Nevada", 36.1989, -115.1175, "America/Los_Angeles", false, Seq("North Las Vegas"), Seq(
    ("Galaxy Cannery", "Galaxy Cannery", "galaxy-cannery", "Nevada", "North Las Vegas")
  ))
  private def p_fremont: R = ("fremont", "fremont-california", "Fremont", "California", 37.5483, -121.9886, "America/Los_Angeles", false, Seq("Fremont"), Seq(
    ("Cinemark Century at Pacific Commons", "Cinemark Century at Pacific Commons", "cinemark-century-at-pacific-commons", "California", "Fremont"),
    ("Cine Lounge Fremont 7", "Cine Lounge Fremont 7", "cine-lounge-fremont-7", "California", "Fremont")
  ))
  private def p_spokane: R = ("spokane", "spokane-washington", "Spokane", "Washington", 47.6597, -117.4291, "America/Los_Angeles", false, Seq("Spokane"), Seq(
    ("AMC River Park Sq 20 Spokane", "AMC River Park Sq 20 Spokane", "amc-river-park-sq-20-spokane", "Washington", "Spokane"),
    ("Magic Lantern Theatre Spokane", "Magic Lantern Theatre Spokane", "magic-lantern-theatre-spokane", "Washington", "Spokane"),
    ("Regal Northtown Mall Spokane", "Regal Northtown Mall Spokane", "regal-northtown-mall-spokane", "Washington", "Spokane"),
    ("Regal Spokane Valley", "Regal Spokane Valley", "regal-spokane-valley", "Washington", "Spokane"),
    ("Village Centre at Wandermere", "Village Centre at Wandermere", "village-centre-at-wandermere", "Washington", "Spokane")
  ))
  private def p_baton_rouge: R = ("baton-rouge", "baton-rouge-louisiana", "Baton Rouge", "Louisiana", 30.4433, -91.1875, "America/Chicago", false, Seq("Baton Rouge"), Seq(
    ("AMC Baton Rouge 16", "AMC Baton Rouge 16", "amc-baton-rouge-16", "Louisiana", "Baton Rouge"),
    ("AMC Mall Of Louisiana 15", "AMC Mall Of Louisiana 15", "amc-mall-of-louisiana-15", "Louisiana", "Baton Rouge"),
    ("Celebrity Theaters Baton Rouge 10", "Celebrity Theaters Baton Rouge 10", "celebrity-theaters-baton-rouge-10", "Louisiana", "Baton Rouge"),
    ("Cinemark Perkins Rowe", "Cinemark Perkins Rowe", "cinemark-perkins-rowe", "Louisiana", "Baton Rouge"),
    ("Movie Tavern Citiplace Baton Rouge", "Movie Tavern Citiplace Baton Rouge", "movie-tavern-citiplace-baton-rouge", "Louisiana", "Baton Rouge"),
    ("Manship Theatre Baton Rouge", "Manship Theatre Baton Rouge", "manship-theatre-baton-rouge", "Louisiana", "Baton Rouge")
  ))
  private def p_richmond: R = ("richmond", "richmond-virginia", "Richmond", "Virginia", 37.5538, -77.4603, "America/New_York", false, Seq("Richmond"), Seq(
    ("BTM Movieland at Boulevard Sq.", "BTM Movieland at Boulevard Sq.", "btm-movieland-at-boulevard-sq", "Virginia", "Richmond"),
    ("Byrd Theatre Richmond", "Byrd Theatre Richmond", "byrd-theatre-richmond", "Virginia", "Richmond"),
    ("Regal Short Pump", "Regal Short Pump", "regal-short-pump", "Virginia", "Richmond"),
    ("Regal West Tower Cinema Richmond", "Regal West Tower Cinema Richmond", "regal-west-tower-cinema-richmond", "Virginia", "Richmond"),
    ("The Dome, Science Museum of VA", "The Dome, Science Museum of VA", "the-dome-science-museum-of-va", "Virginia", "Richmond")
  ))
  private def p_tacoma: R = ("tacoma", "tacoma-washington", "Tacoma", "Washington", 47.2529, -122.4443, "America/Los_Angeles", false, Seq("Tacoma"), Seq(
    ("Blue Mouse Tacoma", "Blue Mouse Tacoma", "blue-mouse-tacoma", "Washington", "Old Tacoma"),
    ("Cinemark Century Point Ruston", "Cinemark Century Point Ruston", "cinemark-century-point-ruston", "Washington", "Tacoma"),
    ("Grand Cinema Tacoma", "Grand Cinema Tacoma", "grand-cinema-tacoma", "Washington", "Tacoma")
  ))
  private def p_san_bernardino: R = ("san-bernardino", "san-bernardino-california", "San Bernardino", "California", 34.1083, -117.2898, "America/Los_Angeles", false, Seq("San Bernardino"), Seq(
    ("Regal San Bernardino", "Regal San Bernardino", "regal-san-bernardino", "California", "San Bernardino"),
    ("Regency Sterling 6 San Bernardino", "Regency Sterling 6 San Bernardino", "regency-sterling-6-san-bernardino", "California", "San Bernardino")
  ))
  private def p_salt_lake_city: R = ("salt-lake-city", "salt-lake-city-utah", "Salt Lake City", "Utah", 40.7608, -111.8911, "America/Denver", false, Seq("Salt Lake City"), Seq(
    ("Brewvies Cinema Salt Lake City", "Brewvies Cinema Salt Lake City", "brewvies-cinema-salt-lake-city", "Utah", "Salt Lake City"),
    ("Broadway Salt Lake City 6", "Broadway Salt Lake City 6", "broadway-salt-lake-city-6", "Utah", "Salt Lake City"),
    ("Cinemark Century Salt Lake City 16", "Cinemark Century Salt Lake City 16", "cinemark-century-salt-lake-city-16", "Utah", "Salt Lake City"),
    ("Cinemark Sugarhouse Salt Lake City", "Cinemark Sugarhouse Salt Lake City", "cinemark-sugarhouse-salt-lake-city", "Utah", "Salt Lake City"),
    ("Megaplex The Gateway", "Megaplex The Gateway", "megaplex-the-gateway", "Utah", "Salt Lake City"),
    ("Redwood Drive-In Salt Lake City", "Redwood Drive-In Salt Lake City", "redwood-drive-in-salt-lake-city", "Utah", "Salt Lake City"),
    ("Rose Wagner Performing Arts Center", "Rose Wagner Performing Arts Center", "rose-wagner-performing-arts-center", "Utah", "Salt Lake City"),
    ("Tower Theatre Salt Lake City", "Tower Theatre Salt Lake City", "tower-theatre-salt-lake-city", "Utah", "Salt Lake City")
  ))
  private def p_huntsville: R = ("huntsville", "huntsville-alabama", "Huntsville", "Alabama", 34.7304, -86.5859, "America/Chicago", false, Seq("Huntsville"), Seq(
    ("AMC Valley Bend 18", "AMC Valley Bend 18", "amc-valley-bend-18", "Alabama", "Huntsville"),
    ("Cinemark Bridge Street", "Cinemark Bridge Street", "cinemark-bridge-street", "Alabama", "Huntsville")
  ))
  private def p_des_moines: R = ("des-moines", "des-moines-iowa", "Des Moines", "Iowa", 41.6005, -93.6091, "America/Chicago", false, Seq("Des Moines"), Seq(
    ("Des Moines Fleur Cinema & Café", "Des Moines Fleur Cinema & Café", "des-moines-fleur-cinema-caf", "Iowa", "Des Moines"),
    ("Flix Brewhouse Des Moines", "Flix Brewhouse Des Moines", "flix-brewhouse-des-moines", "Iowa", "Des Moines"),
    ("Varsity Cinema Des Moines", "Varsity Cinema Des Moines", "varsity-cinema-des-moines", "Iowa", "Des Moines")
  ))
  private def p_fontana: R = ("fontana", "fontana-california", "Fontana", "California", 34.0922, -117.4351, "America/Los_Angeles", false, Seq("Fontana"), Seq(
    ("Regency Cinema Fontana 8", "Regency Cinema Fontana 8", "regency-cinema-fontana-8", "California", "Fontana")
  ))
  private def p_modesto: R = ("modesto", "modesto-california", "Modesto", "California", 37.6391, -120.9969, "America/Los_Angeles", false, Seq("Modesto"), Seq(
    ("Brenden Modesto 18", "Brenden Modesto 18", "brenden-modesto-18", "California", "Modesto"),
    ("Regal Modesto", "Regal Modesto", "regal-modesto", "California", "Modesto"),
    ("State Theatre Modesto", "State Theatre Modesto", "state-theatre-modesto", "California", "Modesto")
  ))
  private def p_rochester: R = ("rochester", "rochester-new-york", "Rochester", "New York", 43.1548, -77.6156, "America/New_York", false, Seq("Rochester"), Seq(
    ("Apple Cinemas Pittsford", "Apple Cinemas Pittsford", "apple-cinemas-pittsford", "New York", "Rochester"),
    ("Cinemark Tinseltown Rochester", "Cinemark Tinseltown Rochester", "cinemark-tinseltown-rochester", "New York", "Rochester"),
    ("Dryden Theatre Rochester", "Dryden Theatre Rochester", "dryden-theatre-rochester", "New York", "Rochester"),
    ("Little Theatre Rochester", "Little Theatre Rochester", "little-theatre-rochester-5", "New York", "Rochester"),
    ("Movies Rochester 10", "Movies Rochester 10", "movies-rochester-10", "New York", "Rochester")
  ))
  private def p_arlington_virginia: R = ("arlington-virginia", "arlington-virginia", "Arlington", "Virginia", 38.881, -77.1043, "America/New_York", false, Seq("Arlington"), Seq(
    ("Alamo Drafthouse Crystal City", "Alamo Drafthouse Crystal City", "alamo-drafthouse-crystal-city", "Virginia", "Arlington"),
    ("AMC Courthouse Plaza 8", "AMC Courthouse Plaza 8", "amc-courthouse-plaza-8", "Virginia", "Arlington"),
    ("AMC Shirlington 7", "AMC Shirlington 7", "amc-shirlington-7", "Virginia", "Arlington"),
    ("Regal Ballston Quarter Arlington", "Regal Ballston Quarter Arlington", "regal-ballston-quarter-arlington", "Virginia", "Arlington"),
    ("Arlington Cinema & Drafthouse", "Arlington Cinema & Drafthouse", "arlington-cinema-drafthouse", "Virginia", "Arlington")
  ))
  private def p_oxnard: R = ("oxnard", "oxnard-california", "Oxnard", "California", 34.1975, -119.177, "America/Los_Angeles", false, Seq("Oxnard"), Seq(
    ("Cinemark Century River Park", "Cinemark Century River Park", "cinemark-century-river-park", "California", "Oxnard"),
    ("Plaza Cinemas 14", "Plaza Cinemas 14", "plaza-cinemas-14", "California", "Oxnard")
  ))
  private def p_columbus_georgia: R = ("columbus-georgia", "columbus-georgia", "Columbus", "Georgia", 32.461, -84.9877, "America/New_York", false, Seq("Columbus"), Seq(
    ("AMC CLASSIC Columbus Park 15", "AMC CLASSIC Columbus Park 15", "amc-classic-columbus-park-15", "Georgia", "Columbus"),
    ("AMC CLASSIC Ritz 13", "AMC CLASSIC Ritz 13", "amc-classic-ritz-13", "Georgia", "Columbus"),
    ("Giant Screen Theatre", "Giant Screen Theatre", "giant-screen-theatre", "Georgia", "Columbus")
  ))
  private def p_worcester: R = ("worcester", "worcester-massachusetts", "Worcester", "Massachusetts", 42.2626, -71.8023, "America/New_York", false, Seq("Worcester"), Seq(
    ("Worcester Polytechnic Institute", "Worcester Polytechnic Institute", "worcester-polytechnic-institute", "Massachusetts", "Worcester")
  ))
  private def p_moreno_valley: R = ("moreno-valley", "moreno-valley-california", "Moreno Valley", "California", 33.9375, -117.2306, "America/Los_Angeles", false, Seq("Moreno Valley"), Seq(
    ("Harkins Moreno Valley 16", "Harkins Moreno Valley 16", "harkins-moreno-valley-16", "California", "Moreno Valley"),
    ("Regency Towngate Moreno Valley 8", "Regency Towngate Moreno Valley 8", "regency-towngate-moreno-valley-8", "California", "Moreno Valley")
  ))
  private def p_bayamon: R = ("bayamon", "bayamon-puerto-rico", "Bayamón", "Puerto Rico", 18.3986, -66.1557, "America/Puerto_Rico", false, Seq("Bayamón"), Seq(
    ("Caribbean Cinemas Bayamón", "Caribbean Cinemas Bayamón", "caribbean-cinemas-rio-hondo-1", "Puerto Rico", "Bayamón"),
    ("Caribbean Cinemas Plaza del Sol", "Caribbean Cinemas Plaza del Sol", "caribbean-cinemas-plaza-del-sol", "Puerto Rico", "Bayamón")
  ))
  private def p_little_rock: R = ("little-rock", "little-rock-arkansas", "Little Rock", "Arkansas", 34.7465, -92.2896, "America/Chicago", false, Seq("Little Rock"), Seq(
    ("AMC Chenal 9", "AMC Chenal 9", "amc-chenal-9", "Arkansas", "Little Rock"),
    ("Cinemark Colonel Glenn", "Cinemark Colonel Glenn", "cinemark-colonel-glenn", "Arkansas", "Little Rock"),
    ("Film Alley Breckenridge", "Film Alley Breckenridge", "film-alley-breckenridge", "Arkansas", "Little Rock"),
    ("Movie Tavern Little Rock", "Movie Tavern Little Rock", "movie-tavern-little-rock", "Arkansas", "Little Rock"),
    ("Riverdale 10 VIP Cinema", "Riverdale 10 VIP Cinema", "riverdale-10-vip-cinema", "Arkansas", "Little Rock"),
    ("Ron Robinson Theater", "Ron Robinson Theater", "ron-robinson-theater", "Arkansas", "Little Rock"),
    ("Arkansas Arts Center", "Arkansas Arts Center", "arkansas-arts-center", "Arkansas", "Little Rock")
  ))
  private def p_fayetteville: R = ("fayetteville", "fayetteville-north-carolina", "Fayetteville", "North Carolina", 35.0527, -78.8784, "America/New_York", false, Seq("Fayetteville"), Seq(
    ("AMC Fayetteville 14", "AMC Fayetteville 14", "amc-fayetteville-14", "North Carolina", "Fayetteville"),
    ("AMC Market Fair 15", "AMC Market Fair 15", "amc-market-fair-15", "North Carolina", "Fayetteville"),
    ("Cameo Art House Fayetteville", "Cameo Art House Fayetteville", "cameo-art-house-theatre-fayetteville", "North Carolina", "Fayetteville"),
    ("Millstone 14 Cinemas Fayetteville 14", "Millstone 14 Cinemas Fayetteville 14", "millstone-14-cinemas-fayetteville-14", "North Carolina", "Fayetteville"),
    ("Omni Cinema Fayetteville 8", "Omni Cinema Fayetteville 8", "omni-cinema-fayetteville-8", "North Carolina", "Fayetteville")
  ))
  private def p_huntington_beach: R = ("huntington-beach", "huntington-beach-california", "Huntington Beach", "California", 33.6603, -117.9992, "America/Los_Angeles", false, Seq("Huntington Beach"), Seq(
    ("Cinemark Century Huntington Beach 20", "Cinemark Century Huntington Beach 20", "cinemark-century-huntington-beach-20", "California", "Huntington Beach")
  ))
  private def p_tallahassee: R = ("tallahassee", "tallahassee-florida", "Tallahassee", "Florida", 30.4383, -84.2807, "America/New_York", false, Seq("Tallahassee"), Seq(
    ("AMC Tallahassee 20", "AMC Tallahassee 20", "amc-tallahassee-20", "Florida", "Tallahassee"),
    ("CMX Cinemas Fallschase", "CMX Cinemas Fallschase", "cmx-cinemas-fallschase", "Florida", "Tallahassee"),
    ("Cap City Video Lounge", "Cap City Video Lounge", "cap-city-video-lounge", "Florida", "Tallahassee"),
    ("IMAX Challenger Learning Center of Tallahassee", "IMAX Challenger Learning Center of Tallahassee", "imax-challenger-learning-center-of-tallahassee", "Florida", "Tallahassee"),
    ("Student Life Cinema, Florida State University", "Student Life Cinema, Florida State University", "student-life-cinema-florida-state-university", "Florida", "Tallahassee")
  ))
  private def p_yonkers: R = ("yonkers", "yonkers-new-york", "Yonkers", "New York", 40.9304, -73.8979, "America/New_York", false, Seq("Yonkers"), Seq(
    ("Alamo Drafthouse Yonkers", "Alamo Drafthouse Yonkers", "alamo-drafthouse-yonkers", "New York", "Yonkers"),
    ("Showcase Cross County", "Showcase Cross County", "showcase-cross-county", "New York", "Yonkers"),
    ("Showcase Ridge Hill Yonkers", "Showcase Ridge Hill Yonkers", "showcase-ridge-hill-yonkers", "New York", "Yonkers")
  ))
  private def p_glendale_california: R = ("glendale-california", "glendale-california", "Glendale", "California", 34.1425, -118.2551, "America/Los_Angeles", false, Seq("Glendale"), Seq(
    ("Alex Theatre Glendale", "Alex Theatre Glendale", "alex-theatre-glendale", "California", "Glendale"),
    ("AMC Americana at Brand 18", "AMC Americana at Brand 18", "amc-americana-at-brand-18", "California", "Glendale"),
    ("Laemmle Glendale 5", "Laemmle Glendale 5", "laemmle-glendale-5", "California", "Glendale"),
    ("LOOK Cinemas Glendale", "LOOK Cinemas Glendale", "look-cinemas-glendale", "California", "Glendale"),
    ("Electric Dusk Drive-In Glendale", "Electric Dusk Drive-In Glendale", "electric-dusk-drive-in-glendale", "California", "Glendale")
  ))
  private def p_cypress: R = ("cypress", "cypress-texas", "Cypress", "Texas", 29.9691, -95.6972, "America/Chicago", false, Seq("Cypress"), Seq(
    ("Cinemark Cypress", "Cinemark Cypress", "cinemark-cypress", "Texas", "Cypress"),
    ("CUT! by Cinemark Cypress", "CUT! by Cinemark Cypress", "cut-by-cinemark-cypress", "Texas", "Cypress"),
    ("Star Cinema Grill Cypress", "Star Cinema Grill Cypress", "star-cinema-grill-cypress", "Texas", "Cypress")
  ))
  private def p_aurora_illinois: R = ("aurora-illinois", "aurora-illinois", "Aurora", "Illinois", 41.7606, -88.3201, "America/Chicago", false, Seq("Aurora"), Seq(
    ("Paramount Arts Centre Aurora", "Paramount Arts Centre Aurora", "paramount-arts-centre-aurora", "Illinois", "Aurora")
  ))
  private def p_amarillo: R = ("amarillo", "amarillo-texas", "Amarillo", "Texas", 35.222, -101.8313, "America/Chicago", false, Seq("Amarillo"), Seq(
    ("Cinemark Hollywood Amarillo 16", "Cinemark Hollywood Amarillo 16", "cinemark-hollywood-amarillo-16", "Texas", "Amarillo"),
    ("Cinergy Cinemas Amarillo 10", "Cinergy Cinemas Amarillo 10", "cinergy-cinemas-amarillo-10", "Texas", "Amarillo"),
    ("Tascosa Drive-in Amarillo", "Tascosa Drive-in Amarillo", "tascosa-drive-in-amarillo", "Texas", "Amarillo")
  ))
  private def p_akron: R = ("akron", "akron-ohio", "Akron", "Ohio", 41.0814, -81.519, "America/New_York", false, Seq("Akron"), Seq(
    ("Highland Theater Akron", "Highland Theater Akron", "highland-theater-akron", "Ohio", "Akron"),
    ("The Nightlight", "The Nightlight", "the-nightlight", "Ohio", "Akron"),
    ("House Three Thirty", "House Three Thirty", "house-three-thirty", "Ohio", "Akron"),
    ("Linda Theater Akron", "Linda Theater Akron", "linda-theater-akron", "Ohio", "Akron")
  ))
  private def p_vancouver: R = ("vancouver", "vancouver-washington", "Vancouver", "Washington", 45.6387, -122.6615, "America/Los_Angeles", false, Seq("Vancouver"), Seq(
    ("AMC Mill Plain 8", "AMC Mill Plain 8", "amc-mill-plain-8", "Washington", "Vancouver"),
    ("AMC Vancouver Mall 23", "AMC Vancouver Mall 23", "amc-vancouver-mall-23", "Washington", "Vancouver"),
    ("Kiggins Theatre", "Kiggins Theatre", "kiggins-theatre", "Washington", "Vancouver"),
    ("Regal Cascade Vancouver", "Regal Cascade Vancouver", "regal-cascade-vancouver", "Washington", "Vancouver"),
    ("Regal Cinema 99 Vancouver", "Regal Cinema 99 Vancouver", "regal-cinema-99-vancouver", "Washington", "Vancouver"),
    ("Regal City Center Vancouver", "Regal City Center Vancouver", "regal-city-center-vancouver", "Washington", "Vancouver"),
    ("Regal Vancouver Plaza", "Regal Vancouver Plaza", "regal-vancouver-plaza", "Washington", "Vancouver")
  ))
  private def p_birmingham: R = ("birmingham", "birmingham-alabama", "Birmingham", "Alabama", 33.5207, -86.8025, "America/Chicago", false, Seq("Birmingham"), Seq(
    ("AMC Summit 16", "AMC Summit 16", "amc-summit-16", "Alabama", "Birmingham"),
    ("Birmingham Premiere Lux Cine", "Birmingham Premiere Lux Cine", "birmingham-premiere-lux-cine", "Alabama", "Birmingham"),
    ("McWane Science Center IMAX", "McWane Science Center IMAX", "mcwane-science-center-imax", "Alabama", "Birmingham"),
    ("Sidewalk Film Center and Cinema", "Sidewalk Film Center and Cinema", "sidewalk-film-center-and-cinema", "Alabama", "Birmingham"),
    ("Alabama Theatre Birmingham", "Alabama Theatre Birmingham", "alabama-theatre-birmingham", "Alabama", "Birmingham")
  ))
  private def p_montgomery: R = ("montgomery", "montgomery-alabama", "Montgomery", "Alabama", 32.3668, -86.3, "America/Chicago", false, Seq("Montgomery"), Seq(
    ("AMC CLASSIC Chantilly 13", "AMC CLASSIC Chantilly 13", "amc-classic-chantilly-13", "Alabama", "Montgomery"),
    ("AMC Festival Plaza 16", "AMC Festival Plaza 16", "amc-festival-plaza-16", "Alabama", "Montgomery"),
    ("Capri Theatre Montgomery", "Capri Theatre Montgomery", "capri-theatre-montgomery", "Alabama", "Montgomery")
  ))
  private def p_grand_rapids: R = ("grand-rapids", "grand-rapids-michigan", "Grand Rapids", "Michigan", 42.9634, -85.6681, "America/Detroit", false, Seq("Grand Rapids"), Seq(
    ("AMC Grand Rapids 18", "AMC Grand Rapids 18", "amc-grand-rapids-18", "Michigan", "Grand Rapids"),
    ("Celebration! GR North", "Celebration! GR North", "celebration-gr-north", "Michigan", "Grand Rapids"),
    ("Celebration! GR South", "Celebration! GR South", "celebration-gr-south", "Michigan", "Grand Rapids"),
    ("Celebration! Studio Park", "Celebration! Studio Park", "celebration-studio-park", "Michigan", "Grand Rapids"),
    ("Phoenix Woodland Mall Grand Rapids", "Phoenix Woodland Mall Grand Rapids", "phoenix-woodland-mall-grand-rapids", "Michigan", "Grand Rapids"),
    ("Wealthy Theatre Grand Rapids", "Wealthy Theatre Grand Rapids", "wealthy-theatre-grand-rapids", "Michigan", "Grand Rapids")
  ))
  private def p_peoria_arizona: R = ("peoria-arizona", "peoria-arizona", "Peoria", "Arizona", 33.5806, -112.2374, "America/Phoenix", false, Seq("Peoria"), Seq(
    ("Harkins Arrowhead Fountains 18", "Harkins Arrowhead Fountains 18", "harkins-arrowhead-fountains-18", "Arizona", "Peoria"),
    ("Harkins Lake Pleasant Ciné Grill", "Harkins Lake Pleasant Ciné Grill", "harkins-lake-pleasant-cin-grill", "Arizona", "Peoria"),
    ("Harkins Park West 14", "Harkins Park West 14", "harkins-park-west-14", "Arizona", "Peoria")
  ))
  private def p_providence: R = ("providence", "providence-rhode-island", "Providence", "Rhode Island", 41.824, -71.4128, "America/New_York", false, Seq("Providence"), Seq(
    ("Apple Cinemas Providence Place", "Apple Cinemas Providence Place", "apple-cinemas-providence-place", "Rhode Island", "Providence"),
    ("Avon Cinema", "Avon Cinema", "avon-cinema", "Rhode Island", "Providence")
  ))
  private def p_knoxville: R = ("knoxville", "knoxville-tennessee", "Knoxville", "Tennessee", 35.9606, -83.9207, "America/New_York", false, Seq("Knoxville"), Seq(
    ("Central Cinema Knoxville", "Central Cinema Knoxville", "central-cinema-knoxville", "Tennessee", "Knoxville"),
    ("Regal Cinebarre West Town Mall Knoxville", "Regal Cinebarre West Town Mall Knoxville", "regal-cinebarre-west-town-mall-knoxville", "Tennessee", "Knoxville"),
    ("Regal Downtown West Knoxville", "Regal Downtown West Knoxville", "regal-downtown-west-knoxville", "Tennessee", "Knoxville"),
    ("Regal Pinnacle Knoxville", "Regal Pinnacle Knoxville", "regal-pinnacle-knoxville", "Tennessee", "Knoxville"),
    ("Regal Riviera Knoxville", "Regal Riviera Knoxville", "regal-riviera-knoxville", "Tennessee", "Knoxville"),
    ("Tennessee Theatre Knoxville", "Tennessee Theatre Knoxville", "tennessee-theatre-knoxville", "Tennessee", "Knoxville")
  ))
  private def p_grand_prairie: R = ("grand-prairie", "grand-prairie-texas", "Grand Prairie", "Texas", 32.746, -96.9978, "America/Chicago", false, Seq("Grand Prairie"), Seq(
    ("Cinemark Grand Prairie 15", "Cinemark Grand Prairie 15", "cinemark-grand-prairie-15", "Texas", "Grand Prairie"),
    ("Grand Prairie Premiere Lux Cine", "Grand Prairie Premiere Lux Cine", "grand-prairie-premiere-lux-cine", "Texas", "Grand Prairie")
  ))
  private def p_shreveport: R = ("shreveport", "shreveport-louisiana", "Shreveport", "Louisiana", 32.5251, -93.7502, "America/Chicago", false, Seq("Shreveport"), Seq(
    ("Cinemark Tinseltown Shreveport", "Cinemark Tinseltown Shreveport", "cinemark-tinseltown-shreveport", "Louisiana", "Shreveport"),
    ("Robinson Film Center", "Robinson Film Center", "robinson-film-center", "Louisiana", "Shreveport"),
    ("Sci-Port Discovery Center IMAX", "Sci-Port Discovery Center IMAX", "sci-port-discovery-center-imax", "Louisiana", "Shreveport")
  ))
  private def p_brownsville: R = ("brownsville", "brownsville-texas", "Brownsville", "Texas", 25.9017, -97.4975, "America/Chicago", false, Seq("Brownsville"), Seq(
    ("Cinemark Sunrise Mall Brownsville", "Cinemark Sunrise Mall Brownsville", "cinemark-sunrise-mall-brownsville", "Texas", "Brownsville")
  ))
  private def p_overland_park: R = ("overland-park", "overland-park-kansas", "Overland Park", "Kansas", 38.9822, -94.6708, "America/Chicago", false, Seq("Overland Park"), Seq(
    ("AMC Prairiefire 17", "AMC Prairiefire 17", "amc-dine-in-prairiefire-17", "Kansas", "Overland Park"),
    ("B&B Overland Park 16", "B&B Overland Park 16", "bb-overland-park-16", "Kansas", "Overland Park"),
    ("Glenwood Arts Theater", "Glenwood Arts Theater", "glenwood-arts-theater", "Kansas", "Overland Park")
  ))
  private def p_newport_news: R = ("newport-news", "newport-news-virginia", "Newport News", "Virginia", 36.9804, -76.4297, "America/New_York", false, Seq("Newport News"), Seq(
    ("Cinemark Newport News", "Cinemark Newport News", "cinemark-newport-news", "Virginia", "Newport News"),
    ("Regal Kiln Creek", "Regal Kiln Creek", "regal-kiln-creek", "Virginia", "Newport News")
  ))
  private def p_mobile: R = ("mobile", "mobile-alabama", "Mobile", "Alabama", 30.6944, -88.043, "America/Chicago", false, Seq("Mobile"), Seq(
    ("AMC Mobile 16", "AMC Mobile 16", "amc-mobile-16", "Alabama", "Mobile"),
    ("Nexus Cinema Dining", "Nexus Cinema Dining", "nexus-cinema-dining", "Alabama", "Mobile")
  ))
  private def p_fort_lauderdale: R = ("fort-lauderdale", "fort-lauderdale-florida", "Fort Lauderdale", "Florida", 26.1223, -80.1434, "America/New_York", false, Seq("Fort Lauderdale"), Seq(
    ("AMC DINE-IN Coral Ridge 10", "AMC DINE-IN Coral Ridge 10", "amc-dine-in-coral-ridge-10", "Florida", "Fort Lauderdale"),
    ("AutoNation IMAX Fort Lauderdale", "AutoNation IMAX Fort Lauderdale", "autonation-imax-fort-lauderdale", "Florida", "Fort Lauderdale"),
    ("Paradigm Cinemas Gateway", "Paradigm Cinemas Gateway", "paradigm-cinemas-gateway", "Florida", "Fort Lauderdale"),
    ("Savor Cinema Lauderdale", "Savor Cinema Lauderdale", "savor-cinema-lauderdale", "Florida", "Fort Lauderdale"),
    ("FLIFF Drive-In Cinema (Off-Site Special Events)", "FLIFF Drive-In Cinema (Off-Site Special Events)", "fliff-drive-in-cinema-off-site-special-events", "Florida", "Fort Lauderdale")
  ))
  private def p_santa_clarita: R = ("santa-clarita", "santa-clarita-california", "Santa Clarita", "California", 34.3917, -118.5426, "America/Los_Angeles", false, Seq("Santa Clarita"), Seq(
    ("Laemmle Newhall", "Laemmle Newhall", "laemmle-newhall", "California", "Santa Clarita"),
    ("Regal Edwards Canyon Country", "Regal Edwards Canyon Country", "regal-edwards-canyon-country", "California", "Santa Clarita"),
    ("Regal Valencia Stadium", "Regal Valencia Stadium", "regal-valencia-stadium", "California", "Santa Clarita")
  ))
  private def p_chattanooga: R = ("chattanooga", "chattanooga-tennessee", "Chattanooga", "Tennessee", 35.0456, -85.3097, "America/New_York", false, Seq("Chattanooga"), Seq(
    ("AMC Chattanooga 18", "AMC Chattanooga 18", "amc-chattanooga-18", "Tennessee", "Chattanooga"),
    ("AMC CLASSIC Majestic 12", "AMC CLASSIC Majestic 12", "amc-classic-majestic-12", "Tennessee", "Chattanooga"),
    ("Regal Hamilton Place Chattanooga", "Regal Hamilton Place Chattanooga", "regal-hamilton-place-chattanooga", "Tennessee", "Chattanooga"),
    ("Tennessee Aquarium Chattanooga", "Tennessee Aquarium Chattanooga", "tennessee-aquarium-chattanooga", "Tennessee", "Chattanooga")
  ))
  private def p_spring_valley: R = ("spring-valley", "spring-valley-nevada", "Spring Valley", "Nevada", 36.108, -115.245, "America/Los_Angeles", false, Seq("Spring Valley"), Seq(
    ("Brenden Las Vegas 14", "Brenden Las Vegas 14", "brenden-las-vegas-14", "Nevada", "East Las Vegas")
  ))
  private def p_santa_rosa: R = ("santa-rosa", "santa-rosa-california", "Santa Rosa", "California", 38.4405, -122.7144, "America/Los_Angeles", false, Seq("Santa Rosa"), Seq(
    ("Airport Santa Rosa 12", "Airport Santa Rosa 12", "airport-santa-rosa-12", "California", "Santa Rosa"),
    ("Roxy 14 Santa Rosa", "Roxy 14 Santa Rosa", "roxy-14-santa-rosa", "California", "Santa Rosa"),
    ("Summerfield Cinemas 5", "Summerfield Cinemas 5", "summerfield-cinemas-5", "California", "Santa Rosa")
  ))
  private def p_eugene: R = ("eugene", "eugene-oregon", "Eugene", "Oregon", 44.0521, -123.0867, "America/Los_Angeles", false, Seq("Eugene"), Seq(
    ("Broadway Metro Eugene", "Broadway Metro Eugene", "broadway-metro-eugene", "Oregon", "Eugene"),
    ("Eugene Art House", "Eugene Art House", "eugene-art-house", "Oregon", "Eugene"),
    ("Regal Valley River Eugene", "Regal Valley River Eugene", "regal-valley-river-eugene", "Oregon", "Eugene")
  ))
  private def p_tempe: R = ("tempe", "tempe-arizona", "Tempe", "Arizona", 33.4148, -111.9093, "America/Phoenix", false, Seq("Tempe"), Seq(
    ("AMC Centerpoint 11 Tempe", "AMC Centerpoint 11 Tempe", "amc-centerpoint-11-tempe", "Arizona", "Tempe"),
    ("Harkins Arizona Mills 18", "Harkins Arizona Mills 18", "harkins-arizona-mills-18", "Arizona", "Tempe"),
    ("Harkins Tempe Marketplace 16", "Harkins Tempe Marketplace 16", "harkins-tempe-marketplace-16", "Arizona", "Tempe"),
    ("Majestic Tempe", "Majestic Tempe", "majestic-tempe", "Arizona", "Tempe"),
    ("Pollack Cinemas Tempe", "Pollack Cinemas Tempe", "pollack-cinemas-tempe", "Arizona", "Tempe")
  ))
  private def p_oceanside: R = ("oceanside", "oceanside-california", "Oceanside", "California", 33.1959, -117.3795, "America/Los_Angeles", false, Seq("Oceanside"), Seq(
    ("Regal Mission Marketplace Oceanside", "Regal Mission Marketplace Oceanside", "regal-mission-marketplace-oceanside", "California", "Oceanside"),
    ("Regal Oceanside", "Regal Oceanside", "regal-oceanside", "California", "Oceanside")
  ))
  private def p_salem: R = ("salem", "salem-oregon", "Salem", "Oregon", 44.9429, -123.0351, "America/Los_Angeles", false, Seq("Salem"), Seq(
    ("Northern Lights Pub Salem 3", "Northern Lights Pub Salem 3", "northern-lights-pub-salem-3", "Oregon", "Salem"),
    ("Regal Santiam Salem", "Regal Santiam Salem", "regal-santiam-salem", "Oregon", "Salem"),
    ("Regal Willamette Town Center Salem", "Regal Willamette Town Center Salem", "regal-willamette-town-center-salem", "Oregon", "Salem"),
    ("Salem Cinema", "Salem Cinema", "salem-cinema", "Oregon", "Salem"),
    ("Elsinore Theatre Salem", "Elsinore Theatre Salem", "elsinore-theatre-salem", "Oregon", "Salem")
  ))
  private def p_garden_grove: R = ("garden-grove", "garden-grove-california", "Garden Grove", "California", 33.7739, -117.9415, "America/Los_Angeles", false, Seq("Garden Grove"), Seq(
    ("Regal Garden Grove", "Regal Garden Grove", "regal-garden-grove", "California", "Garden Grove"),
    ("Starlight West Grove Cinemas", "Starlight West Grove Cinemas", "starlight-west-grove-cinemas", "California", "Garden Grove")
  ))
  private def p_rancho_cucamonga: R = ("rancho-cucamonga", "rancho-cucamonga-california", "Rancho Cucamonga", "California", 34.1064, -117.5931, "America/Los_Angeles", false, Seq("Rancho Cucamonga"), Seq(
    ("AMC Victoria Gardens 12", "AMC Victoria Gardens 12", "amc-victoria-gardens-12", "California", "Rancho Cucamonga"),
    ("Starlight Terra Vista 6", "Starlight Terra Vista 6", "starlight-terra-vista-6", "California", "Rancho Cucamonga")
  ))
  private def p_cape_coral: R = ("cape-coral", "cape-coral-florida", "Cape Coral", "Florida", 26.5629, -81.9495, "America/New_York", false, Seq("Cape Coral"), Seq(
    ("Marquee Cinemas Cape Coral 10", "Marquee Cinemas Cape Coral 10", "marquee-cinemas-cape-coral-10", "Florida", "Cape Coral")
  ))
  private def p_sioux_falls: R = ("sioux-falls", "sioux-falls-south-dakota", "Sioux Falls", "South Dakota", 43.5437, -96.728, "America/Chicago", false, Seq("Sioux Falls"), Seq(
    ("Cinemark Century East at Dawley Farm", "Cinemark Century East at Dawley Farm", "cinemark-century-east-at-dawley-farm", "South Dakota", "Sioux Falls"),
    ("Cinemark Century Stadium 14", "Cinemark Century Stadium 14", "cinemark-century-stadium-14", "South Dakota", "Sioux Falls"),
    ("State Theatre Sioux Falls", "State Theatre Sioux Falls", "state-theatre-sioux-falls", "South Dakota", "Sioux Falls"),
    ("Wells Fargo CineDome Sioux Falls", "Wells Fargo CineDome Sioux Falls", "wells-fargo-cinedome-sioux-falls", "South Dakota", "Sioux Falls"),
    ("West Mall 7 Theatres", "West Mall 7 Theatres", "west-mall-7-theatres", "South Dakota", "Sioux Falls")
  ))
  private def p_ontario_california: R = ("ontario-california", "ontario-california", "Ontario", "California", 34.0633, -117.6509, "America/Los_Angeles", false, Seq("Ontario"), Seq(
    ("AMC Ontario Mills 30", "AMC Ontario Mills 30", "amc-ontario-mills-30", "California", "Ontario"),
    ("Regal Edwards Ontario Palace", "Regal Edwards Ontario Palace", "regal-edwards-ontario-palace", "California", "Ontario"),
    ("Regal Mountain Village Ontario", "Regal Mountain Village Ontario", "regal-mountain-village-ontario", "California", "Ontario")
  ))
  private def p_fort_collins: R = ("fort-collins", "fort-collins-colorado", "Fort Collins", "Colorado", 40.5853, -105.0844, "America/Denver", false, Seq("Fort Collins"), Seq(
    ("AMC CLASSIC Fort Collins 10", "AMC CLASSIC Fort Collins 10", "amc-classic-fort-collins-10", "Colorado", "Fort Collins"),
    ("Cinemark Bistro Fort Collins", "Cinemark Bistro Fort Collins", "cinemark-bistro-fort-collins", "Colorado", "Fort Collins"),
    ("Cinemark Fort Collins", "Cinemark Fort Collins", "cinemark-fort-collins", "Colorado", "Fort Collins"),
    ("Holiday Drive-in Fort Collins", "Holiday Drive-in Fort Collins", "holiday-drive-in-fort-collins", "Colorado", "Fort Collins"),
    ("The Lyric Fort Collins", "The Lyric Fort Collins", "the-lyric-fort-collins", "Colorado", "Fort Collins")
  ))
  private def p_carolina: R = ("carolina", "carolina-puerto-rico", "Carolina", "Puerto Rico", 18.3808, -65.9574, "America/Puerto_Rico", false, Seq("Carolina"), Seq(
    ("Caribbean Cinemas Plaza Carolina", "Caribbean Cinemas Plaza Carolina", "caribbean-cinemas-plaza-carolina", "Puerto Rico", "Carolina"),
    ("Caribbean Cinemas Plaza Escorial", "Caribbean Cinemas Plaza Escorial", "caribbean-cinemas-plaza-escorial", "Puerto Rico", "Carolina")
  ))
  private def p_springfield_missouri: R = ("springfield-missouri", "springfield-missouri", "Springfield", "Missouri", 37.2153, -93.2982, "America/Chicago", false, Seq("Springfield"), Seq(
    ("Alamo Drafthouse Springfield", "Alamo Drafthouse Springfield", "alamo-drafthouse-springfield", "Missouri", "Springfield"),
    ("AMC Springfield 11", "AMC Springfield 11", "amc-springfield-11", "Missouri", "Springfield"),
    ("College Station Theaters Springfield", "College Station Theaters Springfield", "college-station-theaters-springfield", "Missouri", "Springfield"),
    ("Moxie Cinema Springfield 2", "Moxie Cinema Springfield 2", "moxie-cinema-springfield-2", "Missouri", "Springfield")
  ))
  private def p_elk_grove: R = ("elk-grove", "elk-grove-california", "Elk Grove", "California", 38.4088, -121.3716, "America/Los_Angeles", false, Seq("Elk Grove"), Seq(
    ("Cinemark Century Laguna 16", "Cinemark Century Laguna 16", "cinemark-century-laguna-16", "California", "Elk Grove")
  ))
  private def p_clarksville: R = ("clarksville", "clarksville-tennessee", "Clarksville", "Tennessee", 36.5298, -87.3594, "America/Chicago", false, Seq("Clarksville"), Seq(
    ("Phoenix Theatres Governors Square", "Phoenix Theatres Governors Square", "phoenix-theatres-governors-square", "Tennessee", "Clarksville"),
    ("Regal Clarksville", "Regal Clarksville", "regal-clarksville", "Tennessee", "Clarksville")
  ))
  private def p_pembroke_pines: R = ("pembroke-pines", "pembroke-pines-florida", "Pembroke Pines", "Florida", 26.0032, -80.2239, "America/New_York", false, Seq("Pembroke Pines"), Seq(
    ("AMC Pembroke Lake 9", "AMC Pembroke Lake 9", "amc-pembroke-lake-9", "Florida", "Pembroke Pines"),
    ("Regal Westfork Plaza Pembroke Pines", "Regal Westfork Plaza Pembroke Pines", "regal-westfork-plaza-pembroke-pines", "Florida", "Pembroke Pines")
  ))
  private def p_murfreesboro: R = ("murfreesboro", "murfreesboro-tennessee", "Murfreesboro", "Tennessee", 35.8456, -86.3903, "America/Chicago", false, Seq("Murfreesboro"), Seq(
    ("AMC CLASSIC Murfreesboro 16", "AMC CLASSIC Murfreesboro 16", "amc-classic-murfreesboro-16", "Tennessee", "Murfreesboro"),
    ("AMC Stone River 9", "AMC Stone River 9", "amc-stone-river-9", "Tennessee", "Murfreesboro"),
    ("Premiere Murfreesboro 6", "Premiere Murfreesboro 6", "premiere-murfreesboro-6", "Tennessee", "Murfreesboro"),
    ("Middle Tennessee State University", "Middle Tennessee State University", "middle-tennessee-state-university", "Tennessee", "Murfreesboro")
  ))
  private def p_port_saint_lucie: R = ("port-saint-lucie", "port-saint-lucie-florida", "Port Saint Lucie", "Florida", 27.2939, -80.3503, "America/New_York", false, Seq("Port Saint Lucie"), Seq(
    ("AMC Port St Lucie 14", "AMC Port St Lucie 14", "amc-port-st-lucie-14", "Florida", "Port Saint Lucie")
  ))
  private def p_corona: R = ("corona", "corona-california", "Corona", "California", 33.8753, -117.5664, "America/Los_Angeles", false, Seq("Corona"), Seq(
    ("Regal Edwards Corona Crossings", "Regal Edwards Corona Crossings", "regal-edwards-corona-crossings", "California", "Corona"),
    ("Starlight Dos Lagos Theatres", "Starlight Dos Lagos Theatres", "starlight-dos-lagos-theatres", "California", "Corona")
  ))
  private def p_mckinney: R = ("mckinney", "mckinney-texas", "McKinney", "Texas", 33.1976, -96.6153, "America/Chicago", false, Seq("McKinney"), Seq(
    ("Cinemark North McKinney 14", "Cinemark North McKinney 14", "cinemark-north-mckinney-14", "Texas", "McKinney"),
    ("Cinepolis McKinney", "Cinepolis McKinney", "cinepolis-mckinney", "Texas", "McKinney")
  ))
  private def p_lancaster_california: R = ("lancaster-california", "lancaster-california", "Lancaster", "California", 34.698, -118.1367, "America/Los_Angeles", false, Seq("Lancaster"), Seq(
    ("Cinemark Lancaster", "Cinemark Lancaster", "cinemark-lancaster", "California", "Lancaster"),
    ("Regency BLVD Cinemas", "Regency BLVD Cinemas", "regency-blvd-cinemas", "California", "Lancaster")
  ))
  private def p_cary: R = ("cary", "cary-north-carolina", "Cary", "North Carolina", 35.7915, -78.7811, "America/New_York", false, Seq("Cary"), Seq(
    ("Cary Theater", "Cary Theater", "cary-theater", "North Carolina", "Cary"),
    ("CMX CinéBistro at Waverly Place", "CMX CinéBistro at Waverly Place", "cmx-cinbistro-at-waverly-place", "North Carolina", "Cary"),
    ("Hooky Entertainment Cary", "Hooky Entertainment Cary", "hooky-entertainment-cary", "North Carolina", "Cary"),
    ("Paragon Fenton", "Paragon Fenton", "paragon-fenton", "North Carolina", "Cary"),
    ("Regal Crossroads Cary", "Regal Crossroads Cary", "regal-crossroads-cary", "North Carolina", "Cary")
  ))
  private def p_alexandria_virginia: R = ("alexandria-virginia", "alexandria-virginia", "Alexandria", "Virginia", 38.8048, -77.0469, "America/New_York", false, Seq("Alexandria"), Seq(
    ("AMC Hoffman Center 22", "AMC Hoffman Center 22", "amc-hoffman-center-22", "Virginia", "Alexandria"),
    ("Regal Kingstowne", "Regal Kingstowne", "regal-kingstowne", "Virginia", "Alexandria")
  ))
  private def p_palmdale: R = ("palmdale", "palmdale-california", "Palmdale", "California", 34.5794, -118.1165, "America/Los_Angeles", false, Seq("Palmdale"), Seq(
    ("Cinemark Antelope Valley Mall", "Cinemark Antelope Valley Mall", "cinemark-antelope-valley-mall", "California", "Palmdale")
  ))
  private def p_hayward: R = ("hayward", "hayward-california", "Hayward", "California", 37.6688, -122.0808, "America/Los_Angeles", false, Seq("Hayward"), Seq(
    ("Cinemark Century at Hayward", "Cinemark Century at Hayward", "cinemark-century-at-hayward", "California", "Hayward"),
    ("Cinemark Century Southland Mall 5", "Cinemark Century Southland Mall 5", "cinemark-century-southland-mall-5", "California", "Hayward")
  ))
  private def p_salinas: R = ("salinas", "salinas-california", "Salinas", "California", 36.6777, -121.6555, "America/Los_Angeles", false, Seq("Salinas"), Seq(
    ("Cinemark Century Northridge Mall 14", "Cinemark Century Northridge Mall 14", "cinemark-century-northridge-mall-14", "California", "Salinas"),
    ("Maya Salinas", "Maya Salinas", "maya-salinas", "California", "Salinas")
  ))
  private def p_sunnyvale: R = ("sunnyvale", "sunnyvale-california", "Sunnyvale", "California", 37.3688, -122.0363, "America/Los_Angeles", false, Seq("Sunnyvale"), Seq(
    ("AMC DINE-IN Sunnyvale 12", "AMC DINE-IN Sunnyvale 12", "amc-dine-in-sunnyvale-12", "California", "Sunnyvale")
  ))
  private def p_frisco: R = ("frisco", "frisco-texas", "Frisco", "Texas", 33.1507, -96.8236, "America/Chicago", false, Seq("Frisco"), Seq(
    ("AMC DINE-IN Stonebriar 24", "AMC DINE-IN Stonebriar 24", "amc-dine-in-stonebriar-24", "Texas", "Frisco"),
    ("Cinemark Frisco Square", "Cinemark Frisco Square", "cinemark-frisco-square", "Texas", "Frisco"),
    ("CUT! by Cinemark Frisco", "CUT! by Cinemark Frisco", "cut-by-cinemark-frisco", "Texas", "Frisco"),
    ("Flix Brewhouse Frisco/Little Elm", "Flix Brewhouse Frisco/Little Elm", "flix-brewhouse-friscolittle-elm", "Texas", "Frisco")
  ))
  private def p_springfield_massachusetts: R = ("springfield-massachusetts", "springfield-massachusetts", "Springfield", "Massachusetts", 42.1015, -72.5898, "America/New_York", false, Seq("Springfield"), Seq(
    ("Regal MGM Springfield", "Regal MGM Springfield", "regal-mgm-springfield", "Massachusetts", "Springfield")
  ))
  private def p_pasadena: R = ("pasadena", "pasadena-texas", "Pasadena", "Texas", 29.6911, -95.2091, "America/Chicago", false, Seq("Pasadena"), Seq(
    ("Cinemark Hollywood Pasadena", "Cinemark Hollywood Pasadena", "cinemark-hollywood-pasadena", "Texas", "Pasadena"),
    ("America Cinemas Pasadena", "America Cinemas Pasadena", "america-cinemas-pasadena", "Texas", "Pasadena")
  ))
  private def p_jackson_mississippi: R = ("jackson-mississippi", "jackson-mississippi", "Jackson", "Mississippi", 32.2988, -90.1848, "America/Chicago", false, Seq("Jackson"), Seq(
    ("Capri Theatre Jackson", "Capri Theatre Jackson", "capri-theatre-jackson", "Mississippi", "Jackson")
  ))
  private def p_kansas_city_kansas: R = ("kansas-city-kansas", "kansas-city-kansas", "Kansas City", "Kansas", 39.1142, -94.6275, "America/Chicago", false, Seq("Kansas City"), Seq(
    ("AMC Legends 14 Kansas City", "AMC Legends 14 Kansas City", "amc-legends-14-kansas-city", "Kansas", "Kansas City"),
    ("Boulevard Drive-In Kansas City", "Boulevard Drive-In Kansas City", "boulevard-drive-in-kansas-city", "Kansas", "Kansas City")
  ))
  private def p_lakewood: R = ("lakewood", "lakewood-colorado", "Lakewood", "Colorado", 39.7047, -105.0814, "America/Denver", false, Seq("Lakewood"), Seq(
    ("Cinemark Century Bel Mar 16", "Cinemark Century Bel Mar 16", "cinemark-century-bel-mar-16", "Colorado", "Lakewood"),
    ("Regal UA Colorado Mills Lakewood", "Regal UA Colorado Mills Lakewood", "regal-ua-colorado-mills-lakewood", "Colorado", "Lakewood")
  ))
  private def p_escondido: R = ("escondido", "escondido-california", "Escondido", "California", 33.1192, -117.0864, "America/Los_Angeles", false, Seq("Escondido"), Seq(
    ("Regal Escondido", "Regal Escondido", "regal-escondido", "California", "Escondido")
  ))
  private def p_hollywood: R = ("hollywood", "hollywood-florida", "Hollywood", "Florida", 26.0112, -80.1495, "America/New_York", false, Seq("Hollywood"), Seq(
    ("Cinema Paradiso Hollywood Beach", "Cinema Paradiso Hollywood Beach", "cinema-paradiso-hollywood-beach", "Florida", "Hollywood Beach"),
    ("Flippers Cinema Hollywood", "Flippers Cinema Hollywood", "flippers-cinema-hollywood", "Florida", "Hollywood")
  ))
  private def p_rockford: R = ("rockford", "rockford-illinois", "Rockford", "Illinois", 42.2711, -89.094, "America/Chicago", false, Seq("Rockford"), Seq(
    ("AMC Rockford 16", "AMC Rockford 16", "amc-rockford-16", "Illinois", "Rockford")
  ))
  private def p_joliet: R = ("joliet", "joliet-illinois", "Joliet", "Illinois", 41.5252, -88.0834, "America/Chicago", false, Seq("Joliet"), Seq(
    ("Cinemark Louis Joliet Mall", "Cinemark Louis Joliet Mall", "cinemark-louis-joliet-mall", "Illinois", "Joliet")
  ))
  private def p_savannah: R = ("savannah", "savannah-georgia", "Savannah", "Georgia", 32.0835, -81.0998, "America/New_York", false, Seq("Savannah"), Seq(
    ("AMC CLASSIC Savannah 11", "AMC CLASSIC Savannah 11", "amc-classic-savannah-11", "Georgia", "Savannah"),
    ("NCG Savannah", "NCG Savannah", "ncg-savannah", "Georgia", "Savannah"),
    ("Lucas Theatre Savannah", "Lucas Theatre Savannah", "lucas-theatre-savannah", "Georgia", "Savannah"),
    ("Trustees Theatre Savannah", "Trustees Theatre Savannah", "trustees-theatre-savannah", "Georgia", "Savannah")
  ))
  private def p_naperville: R = ("naperville", "naperville-illinois", "Naperville", "Illinois", 41.7859, -88.1473, "America/Chicago", false, Seq("Naperville"), Seq(
    ("AMC Naperville 16", "AMC Naperville 16", "amc-naperville-16", "Illinois", "Naperville"),
    ("Hollywood Palms Cinema Naperville", "Hollywood Palms Cinema Naperville", "hollywood-palms-cinema-naperville", "Illinois", "Naperville")
  ))
  private def p_gainesville: R = ("gainesville", "gainesville-florida", "Gainesville", "Florida", 29.6516, -82.3248, "America/New_York", false, Seq("Gainesville"), Seq(
    ("Hippodrome Theatre Gainesville", "Hippodrome Theatre Gainesville", "hippodrome-theatre-gainesville", "Florida", "Gainesville"),
    ("Regal Butler Town Center Gainesville", "Regal Butler Town Center Gainesville", "regal-butler-town-center-gainesville", "Florida", "Gainesville"),
    ("Regal Celebration Pointe", "Regal Celebration Pointe", "regal-celebration-pointe", "Florida", "Gainesville"),
    ("Regal Royal Park Gainesville", "Regal Royal Park Gainesville", "regal-royal-park-gainesville", "Florida", "Gainesville")
  ))
  private def p_mesquite: R = ("mesquite", "mesquite-texas", "Mesquite", "Texas", 32.7668, -96.5992, "America/Chicago", false, Seq("Mesquite"), Seq(
    ("AMC Mesquite 30", "AMC Mesquite 30", "amc-mesquite-30", "Texas", "Mesquite")
  ))
  private def p_syracuse: R = ("syracuse", "syracuse-new-york", "Syracuse", "New York", 43.0481, -76.1474, "America/New_York", false, Seq("Syracuse"), Seq(
    ("Regal Destiny Mall Syracuse", "Regal Destiny Mall Syracuse", "regal-destiny-mall-syracuse", "New York", "Syracuse"),
    ("Palace Theatre Syracuse", "Palace Theatre Syracuse", "palace-theatre-syracuse", "New York", "Syracuse")
  ))
  private def p_torrance: R = ("torrance", "torrance-california", "Torrance", "California", 33.8359, -118.3406, "America/Los_Angeles", false, Seq("Torrance"), Seq(
    ("AMC Del Amo Torrance 18", "AMC Del Amo Torrance 18", "amc-del-amo-torrance-18", "California", "Torrance"),
    ("AMC Rolling Hills 20 Torrance", "AMC Rolling Hills 20 Torrance", "amc-rolling-hills-20-torrance", "California", "Torrance")
  ))
  private def p_surprise: R = ("surprise", "surprise-arizona", "Surprise", "Arizona", 33.6306, -112.3332, "America/Phoenix", false, Seq("Surprise"), Seq(
    ("AMC Surprise Pointe 14", "AMC Surprise Pointe 14", "amc-surprise-pointe-14", "Arizona", "Surprise"),
    ("Fat Cats Surprise Arizona", "Fat Cats Surprise Arizona", "fat-cats-surprise-arizona", "Arizona", "Surprise")
  ))
  private def p_columbia: R = ("columbia", "columbia-south-carolina", "Columbia", "South Carolina", 34.0007, -81.0348, "America/New_York", false, Seq("Columbia"), Seq(
    ("AMC Harbison 14", "AMC Harbison 14", "amc-harbison-14", "South Carolina", "Columbia"),
    ("Nickelodeon Columbia 2", "Nickelodeon Columbia 2", "nickelodeon-columbia-2", "South Carolina", "Columbia"),
    ("Regal Columbiana Grande", "Regal Columbiana Grande", "regal-columbiana-grande", "South Carolina", "Columbia"),
    ("Regal Sandhill Columbia", "Regal Sandhill Columbia", "regal-sandhill-columbia", "South Carolina", "Columbia"),
    ("Spotlight Cinemas Capitol 8", "Spotlight Cinemas Capitol 8", "spotlight-cinemas-capitol-8", "South Carolina", "Columbia")
  ))
  private def p_pasadena_california: R = ("pasadena-california", "pasadena-california", "Pasadena", "California", 34.1478, -118.1445, "America/Los_Angeles", false, Seq("Pasadena"), Seq(
    ("Academy Cinemas Pasadena", "Academy Cinemas Pasadena", "academy-cinemas-pasadena", "California", "Pasadena"),
    ("Landmark Pasadena (formerly Laemmle)", "Landmark Pasadena (formerly Laemmle)", "laemmle-pasadena", "California", "Pasadena"),
    ("Regal Paseo Pasadena", "Regal Paseo Pasadena", "regal-paseo-pasadena", "California", "Pasadena"),
    ("ArtCenter College of Design", "ArtCenter College of Design", "artcenter-college-of-design", "California", "Pasadena")
  ))
  private def p_orange: R = ("orange", "orange-california", "Orange", "California", 33.7878, -117.8531, "America/Los_Angeles", false, Seq("Orange"), Seq(
    ("AMC Orange 30", "AMC Orange 30", "amc-orange-30", "California", "Orange"),
    ("Cinemark Century Orange", "Cinemark Century Orange", "cinemark-century-orange", "California", "Orange")
  ))
  private def p_fullerton: R = ("fullerton", "fullerton-california", "Fullerton", "California", 33.8703, -117.9253, "America/Los_Angeles", false, Seq("Fullerton"), Seq(
    ("AMC Fullerton 20", "AMC Fullerton 20", "amc-dine-in-fullerton-20", "California", "Fullerton")
  ))
  private def p_killeen: R = ("killeen", "killeen-texas", "Killeen", "Texas", 31.1171, -97.7278, "America/Chicago", false, Seq("Killeen"), Seq(
    ("Regal Killeen Stadium", "Regal Killeen Stadium", "regal-killeen-stadium", "Texas", "Killeen")
  ))
  private def p_mcallen: R = ("mcallen", "mcallen-texas", "McAllen", "Texas", 26.2034, -98.23, "America/Chicago", false, Seq("McAllen"), Seq(
    ("Cinemark Hollywood USA McAllen", "Cinemark Hollywood USA McAllen", "cinemark-hollywood-usa-mcallen", "Texas", "McAllen"),
    ("Cinemark McAllen 6", "Cinemark McAllen 6", "cinemark-mcallen-6", "Texas", "McAllen"),
    ("Valley's Drive In Theater", "Valley's Drive In Theater", "valleys-drive-in-theater", "Texas", "McAllen")
  ))
  private def p_bellevue: R = ("bellevue", "bellevue-washington", "Bellevue", "Washington", 47.6104, -122.2007, "America/Los_Angeles", false, Seq("Bellevue"), Seq(
    ("AMC Factoria 8 Bellevue", "AMC Factoria 8 Bellevue", "amc-factoria-8-bellevue", "Washington", "Bellevue"),
    ("Cinemark Lincoln Square Cinemas", "Cinemark Lincoln Square Cinemas", "cinemark-lincoln-square-cinemas", "Washington", "Bellevue"),
    ("Regal Crossroads Bellevue", "Regal Crossroads Bellevue", "regal-crossroads-bellevue", "Washington", "Bellevue"),
    ("Cinemark Reserve Lincoln Square", "Cinemark Reserve Lincoln Square", "cinemark-reserve-lincoln-square", "Washington", "Bellevue")
  ))
  private def p_metairie: R = ("metairie", "metairie-louisiana", "Metairie", "Louisiana", 29.9841, -90.1529, "America/Chicago", false, Seq("Metairie"), Seq(
    ("AMC Clearview Palace 12", "AMC Clearview Palace 12", "amc-clearview-palace-12", "Louisiana", "Metairie")
  ))
  private def p_ponce: R = ("ponce", "ponce-puerto-rico", "Ponce", "Puerto Rico", 18.0103, -66.624, "America/Puerto_Rico", false, Seq("Ponce"), Seq(
    ("Caribbean Cinemas Plaza del Caribe", "Caribbean Cinemas Plaza del Caribe", "caribbean-cinemas-plaza-del-caribe", "Puerto Rico", "Ponce"),
    ("Caribbean Cinemas Ponce Towne", "Caribbean Cinemas Ponce Towne", "caribbean-cinemas-ponce-towne", "Puerto Rico", "Ponce")
  ))
  private def p_hampton: R = ("hampton", "hampton-virginia", "Hampton", "Virginia", 37.0299, -76.3452, "America/New_York", false, Seq("Hampton"), Seq(
    ("AMC Hampton Towne Centre 24", "AMC Hampton Towne Centre 24", "amc-hampton-towne-centre-24", "Virginia", "Hampton"),
    ("VIP Peninsula Town Center", "VIP Peninsula Town Center", "vip-peninsula-town-center", "Virginia", "Hampton")
  ))
  private def p_west_valley_city: R = ("west-valley-city", "west-valley-city-utah", "West Valley City", "Utah", 40.6916, -112.0011, "America/Denver", false, Seq("West Valley City"), Seq(
    ("Cinemark West Valley", "Cinemark West Valley", "cinemark-west-valley", "Utah", "West Valley City"),
    ("Megaplex Valley Fair", "Megaplex Valley Fair", "megaplex-valley-fair", "Utah", "West Valley City")
  ))
  private def p_dayton: R = ("dayton", "dayton-ohio", "Dayton", "Ohio", 39.7589, -84.1916, "America/New_York", false, Seq("Dayton"), Seq(
    ("Chakeres Theatres - Melody 49 Drive-In", "Chakeres Theatres - Melody 49 Drive-In", "chakeres-theatres---melody-49-drive-in", "Ohio", "Dayton"),
    ("Dixie Twin Drive-In Dayton", "Dixie Twin Drive-In Dayton", "dixie-twin-drive-in-dayton", "Ohio", "Dayton"),
    ("The Neon", "The Neon", "the-neon", "Ohio", "Dayton")
  ))
  private def p_olathe: R = ("olathe", "olathe-kansas", "Olathe", "Kansas", 38.8814, -94.8191, "America/Chicago", false, Seq("Olathe"), Seq(
    ("AMC DINE-IN Studio 28", "AMC DINE-IN Studio 28", "amc-dine-in-studio-28", "Kansas", "Olathe")
  ))
  private def p_warren: R = ("warren", "warren-michigan", "Warren", "Michigan", 42.4904, -83.013, "America/Detroit", false, Seq("Warren"), Seq(
    ("MJR Universal Grand Cinema", "MJR Universal Grand Cinema", "mjr-universal-grand-cinema", "Michigan", "Warren")
  ))
  private def p_charleston_south_carolina: R = ("charleston-south-carolina", "charleston-south-carolina", "Charleston", "South Carolina", 32.7763, -79.9327, "America/New_York", false, Seq("Charleston"), Seq(
    ("Citadel Mall Cinema 16 Charleston", "Citadel Mall Cinema 16 Charleston", "citadel-mall-cinema-16-charleston", "South Carolina", "Charleston"),
    ("Terrace Theater Charleston", "Terrace Theater Charleston", "terrace-theater-charleston", "South Carolina", "Charleston")
  ))
  private def p_midland_texas: R = ("midland-texas", "midland-texas", "Midland", "Texas", 31.9974, -102.0779, "America/Chicago", false, Seq("Midland"), Seq(
    ("Big Sky Drive-in Midland", "Big Sky Drive-in Midland", "big-sky-drive-in-midland", "Texas", "Midland"),
    ("Cinergy Midland 11", "Cinergy Midland 11", "cinergy-midland-11", "Texas", "Midland"),
    ("Cinergy Midland Tall City", "Cinergy Midland Tall City", "cinergy-midland-tall-city", "Texas", "Midland")
  ))
  private def p_waco: R = ("waco", "waco-texas", "Waco", "Texas", 31.5493, -97.1467, "America/Chicago", false, Seq("Waco"), Seq(
    ("AMC Galaxy 16", "AMC Galaxy 16", "amc-galaxy-16", "Texas", "Waco"),
    ("Cinemark Waco", "Cinemark Waco", "cinemark-waco", "Texas", "Waco"),
    ("Luv Creative at the Hippodrome", "Luv Creative at the Hippodrome", "luv-creative-at-the-hippodrome", "Texas", "Waco")
  ))
  private def p_sterling_heights: R = ("sterling-heights", "sterling-heights-michigan", "Sterling Heights", "Michigan", 42.5803, -83.0302, "America/Detroit", false, Seq("Sterling Heights"), Seq(
    ("AMC Forum 30 Sterling Heights", "AMC Forum 30 Sterling Heights", "amc-forum-30-sterling-heights", "Michigan", "Sterling Heights"),
    ("MJR Marketplace Cinema", "MJR Marketplace Cinema", "mjr-marketplace-cinema", "Michigan", "Sterling Heights")
  ))
  private def p_denton: R = ("denton", "denton-texas", "Denton", "Texas", 33.2148, -97.1331, "America/Chicago", false, Seq("Denton"), Seq(
    ("Alamo Drafthouse Denton", "Alamo Drafthouse Denton", "alamo-drafthouse-denton", "Texas", "Denton"),
    ("Cinemark Denton 14", "Cinemark Denton 14", "cinemark-denton-14", "Texas", "Denton"),
    ("Movie Tavern Denton Cinema", "Movie Tavern Denton Cinema", "movie-tavern-denton-cinema", "Texas", "Denton")
  ))
  private def p_cedar_rapids: R = ("cedar-rapids", "cedar-rapids-iowa", "Cedar Rapids", "Iowa", 42.0083, -91.6441, "America/Chicago", false, Seq("Cedar Rapids"), Seq(
    ("Marcus Cedar Rapids Cinema", "Marcus Cedar Rapids Cinema", "marcus-cedar-rapids-cinema", "Iowa", "Cedar Rapids")
  ))
  private def p_roseville: R = ("roseville", "roseville-california", "Roseville", "California", 38.7521, -121.288, "America/Los_Angeles", false, Seq("Roseville"), Seq(
    ("Cinemark Roseville Galleria Mall 14", "Cinemark Roseville Galleria Mall 14", "cinemark-roseville-galleria-mall-14", "California", "Roseville"),
    ("Cinemawest Roseville", "Cinemawest Roseville", "olympus-pointe-roseville", "California", "Roseville")
  ))
  private def p_visalia: R = ("visalia", "visalia-california", "Visalia", "California", 36.3302, -119.2921, "America/Los_Angeles", false, Seq("Visalia"), Seq(
    ("Regal Visalia", "Regal Visalia", "regal-visalia", "California", "Visalia")
  ))
  private def p_coral_springs: R = ("coral-springs", "coral-springs-florida", "Coral Springs", "Florida", 26.2712, -80.2706, "America/New_York", false, Seq("Coral Springs"), Seq(
    ("Paragon Theaters Coral Square", "Paragon Theaters Coral Square", "paragon-theaters-coral-square", "Florida", "Coral Springs"),
    ("Regal Magnolia Place Coral Springs", "Regal Magnolia Place Coral Springs", "regal-magnolia-place-coral-springs", "Florida", "Coral Springs")
  ))
  private def p_thousand_oaks: R = ("thousand-oaks", "thousand-oaks-california", "Thousand Oaks", "California", 34.1706, -118.8376, "America/Los_Angeles", false, Seq("Thousand Oaks"), Seq(
    ("AMC DINE-IN Thousand Oaks 14 with Dolby", "AMC DINE-IN Thousand Oaks 14 with Dolby", "amc-dine-in-thousand-oaks-14-with-dolby", "California", "Thousand Oaks"),
    ("Regal Janss Marketplace", "Regal Janss Marketplace", "regal-janss-marketplace", "California", "Thousand Oaks"),
    ("PARK CIRCUS", "PARK CIRCUS", "park-circus", "California", "Thousand Oaks")
  ))
  private def p_columbia_missouri: R = ("columbia-missouri", "columbia-missouri", "Columbia", "Missouri", 38.9517, -92.3341, "America/Chicago", false, Seq("Columbia"), Seq(
    ("GQT Forum 8", "GQT Forum 8", "gqt-forum-8", "Missouri", "Columbia"),
    ("Ragtag Columbia", "Ragtag Columbia", "ragtag-columbia", "Missouri", "Columbia"),
    ("Regal Columbia Stadium", "Regal Columbia Stadium", "regal-columbia-stadium", "Missouri", "Columbia")
  ))
  private def p_elizabeth: R = ("elizabeth", "elizabeth-new-jersey", "Elizabeth", "New Jersey", 40.664, -74.2107, "America/New_York", false, Seq("Elizabeth"), Seq(
    ("AMC Jersey Gardens 20", "AMC Jersey Gardens 20", "amc-jersey-gardens-20", "New Jersey", "Elizabeth")
  ))
  private def p_stamford: R = ("stamford", "stamford-connecticut", "Stamford", "Connecticut", 41.0534, -73.5387, "America/New_York", false, Seq("Stamford"), Seq(
    ("AMC Landmark 8", "AMC Landmark 8", "amc-landmark-8", "Connecticut", "Stamford"),
    ("AMC Majestic 6 Stamford", "AMC Majestic 6 Stamford", "amc-majestic-6-stamford", "Connecticut", "Stamford"),
    ("Avon Theatre Stamford", "Avon Theatre Stamford", "avon-theatre-stamford", "Connecticut", "Stamford")
  ))
  private def p_concord: R = ("concord", "concord-california", "Concord", "California", 37.978, -122.0311, "America/Los_Angeles", false, Seq("Concord"), Seq(
    ("Brenden Concord 14", "Brenden Concord 14", "brenden-concord-14", "California", "Concord"),
    ("Veranda Luxe Cinema", "Veranda Luxe Cinema", "veranda-luxe-cinema", "California", "Concord"),
    ("West Wind Solano Drive-In", "West Wind Solano Drive-In", "west-wind-solano-drive-in", "California", "Concord")
  ))
  private def p_norman: R = ("norman", "norman-oklahoma", "Norman", "Oklahoma", 35.2226, -97.4395, "America/Chicago", false, Seq("Norman"), Seq(
    ("AMC Robinson Crossing 6", "AMC Robinson Crossing 6", "amc-robinson-crossing-6", "Oklahoma", "Norman"),
    ("Regal Spotlight Norman", "Regal Spotlight Norman", "regal-spotlight-norman", "Oklahoma", "Norman")
  ))
  private def p_athens: R = ("athens", "athens-georgia", "Athens", "Georgia", 33.9609, -83.3779, "America/New_York", false, Seq("Athens"), Seq(
    ("B&B Athens 12", "B&B Athens 12", "bb-athens-12", "Georgia", "Athens"),
    ("Cine Athens", "Cine Athens", "cine-athens", "Georgia", "Athens"),
    ("GTC University 16 Athens", "GTC University 16 Athens", "gtc-university-16-athens", "Georgia", "Athens")
  ))
  private def p_kent: R = ("kent", "kent-washington", "Kent", "Washington", 47.3809, -122.2348, "America/Los_Angeles", false, Seq("Kent"), Seq(
    ("AMC Kent Station 14", "AMC Kent Station 14", "amc-kent-station-14", "Washington", "Kent")
  ))
  private def p_simi_valley: R = ("simi-valley", "simi-valley-california", "Simi Valley", "California", 34.2694, -118.7815, "America/Los_Angeles", false, Seq("Simi Valley"), Seq(
    ("Regal Simi Valley Civic Center", "Regal Simi Valley Civic Center", "regal-simi-valley-civic-center", "California", "Simi Valley"),
    ("Studio Movie Grill Simi Valley", "Studio Movie Grill Simi Valley", "studio-movie-grill-simi-valley", "California", "Simi Valley")
  ))
  private def p_santa_clara: R = ("santa-clara", "santa-clara-california", "Santa Clara", "California", 37.3541, -121.9552, "America/Los_Angeles", false, Seq("Santa Clara"), Seq(
    ("Alamo Drafthouse Valley Fair", "Alamo Drafthouse Valley Fair", "alamo-drafthouse-valley-fair", "California", "Santa Clara"),
    ("AMC Mercado 20 Santa Clara", "AMC Mercado 20 Santa Clara", "amc-mercado-20-santa-clara", "California", "Santa Clara")
  ))
  private def p_topeka: R = ("topeka", "topeka-kansas", "Topeka", "Kansas", 39.0483, -95.678, "America/Chicago", false, Seq("Topeka"), Seq(
    ("B&B Topeka Wheatfield 9", "B&B Topeka Wheatfield 9", "bb-topeka-wheatfield-9", "Kansas", "Topeka")
  ))
  private def p_abilene: R = ("abilene", "abilene-texas", "Abilene", "Texas", 32.4487, -99.7331, "America/Chicago", false, Seq("Abilene"), Seq(
    ("Abilene Premiere Cinema 10", "Abilene Premiere Cinema 10", "abilene-premiere-cinema-10", "Texas", "Abilene"),
    ("Cinemark Abilene 12", "Cinemark Abilene 12", "cinemark-abilene-12", "Texas", "Abilene")
  ))
  private def p_amherst: R = ("amherst", "amherst-new-york", "Amherst", "New York", 42.9784, -78.7998, "America/New_York", false, Seq("Amherst"), Seq(
    ("AMC Maple Ridge 8 Amherst", "AMC Maple Ridge 8 Amherst", "amc-maple-ridge-8-amherst", "New York", "Amherst")
  ))
  private def p_victorville: R = ("victorville", "victorville-california", "Victorville", "California", 34.5361, -117.2912, "America/Los_Angeles", false, Seq("Victorville"), Seq(
    ("Cinemark Victorville 16", "Cinemark Victorville 16", "cinemark-victorville-16", "California", "Victorville")
  ))
  private def p_vallejo: R = ("vallejo", "vallejo-california", "Vallejo", "California", 38.1041, -122.2566, "America/Los_Angeles", false, Seq("Vallejo"), Seq(
    ("Cinemark Century Vallejo 14", "Cinemark Century Vallejo 14", "cinemark-century-vallejo-14", "California", "Vallejo")
  ))
  private def p_lafayette: R = ("lafayette", "lafayette-louisiana", "Lafayette", "Louisiana", 30.2241, -92.0198, "America/Chicago", false, Seq("Lafayette"), Seq(
    ("Santikos Grand Ambassador", "Santikos Grand Ambassador", "santikos-grand-ambassador", "Louisiana", "Lafayette"),
    ("Santikos Grand Lafayette", "Santikos Grand Lafayette", "santikos-grand-lafayette", "Louisiana", "Lafayette")
  ))
  private def p_chico: R = ("chico", "chico-california", "Chico", "California", 39.7285, -121.8375, "America/Los_Angeles", false, Seq("Chico"), Seq(
    ("Cinemark Tinseltown Chico", "Cinemark Tinseltown Chico", "cinemark-tinseltown-chico", "California", "Chico"),
    ("Pageant Theatre Chico", "Pageant Theatre Chico", "pageant-theatre-chico", "California", "Chico")
  ))
  private def p_hartford: R = ("hartford", "hartford-connecticut", "Hartford", "Connecticut", 41.7637, -72.6851, "America/New_York", false, Seq("Hartford"), Seq(
    ("Cinestudio Hartford", "Cinestudio Hartford", "cinestudio-hartford", "Connecticut", "Hartford"),
    ("Apple Cinemas Xtreme Hartford", "Apple Cinemas Xtreme Hartford", "apple-cinemas-xtreme-hartford", "Connecticut", "Hartford"),
    ("Real Art Ways", "Real Art Ways", "real-art-ways", "Connecticut", "Hartford"),
    ("Wadsworth Atheneum Hartford", "Wadsworth Atheneum Hartford", "wadsworth-atheneum-hartford", "Connecticut", "Hartford")
  ))
  private def p_berkeley: R = ("berkeley", "berkeley-california", "Berkeley", "California", 37.8716, -122.2728, "America/Los_Angeles", false, Seq("Berkeley"), Seq(
    ("Berkeley Art Museum & Pacific Film Archive", "Berkeley Art Museum & Pacific Film Archive", "berkeley-art-museum-pacific-film-archive", "California", "Berkeley"),
    ("Rialto Cinemas Elmwood Berkeley", "Rialto Cinemas Elmwood Berkeley", "rialto-cinemas-elmwood-berkeley", "California", "Berkeley")
  ))
  private def p_west_palm_beach: R = ("west-palm-beach", "west-palm-beach-florida", "West Palm Beach", "Florida", 26.7153, -80.0534, "America/New_York", false, Seq("West Palm Beach"), Seq(
    ("The Society of the Four Arts", "The Society of the Four Arts", "the-society-of-the-four-arts", "Florida", "West Palm Beach")
  ))
  private def p_allentown: R = ("allentown", "allentown-pennsylvania", "Allentown", "Pennsylvania", 40.6084, -75.4902, "America/New_York", false, Seq("Allentown"), Seq(
    ("AMC Tilghman Square 8", "AMC Tilghman Square 8", "amc-tilghman-square-8", "Pennsylvania", "Allentown"),
    ("Movie Tavern Trexlertown", "Movie Tavern Trexlertown", "movie-tavern-trexlertown", "Pennsylvania", "Allentown"),
    ("Civic Theatre of Allentown", "Civic Theatre of Allentown", "civic-theatre-of-allentown", "Pennsylvania", "Allentown")
  ))
  private def p_evansville: R = ("evansville", "evansville-indiana", "Evansville", "Indiana", 37.9748, -87.5559, "America/Chicago", false, Seq("Evansville"), Seq(
    ("AMC Evansville 16", "AMC Evansville 16", "amc-evansville-16", "Indiana", "Evansville"),
    ("Showplace East Evansville 20", "Showplace East Evansville 20", "showplace-east-evansville-20", "Indiana", "Evansville"),
    ("Showplace North Evansville 9", "Showplace North Evansville 9", "showplace-north-evansville-9", "Indiana", "Evansville"),
    ("University of Southern Indiana", "University of Southern Indiana", "university-of-southern-indiana", "Indiana", "Evansville")
  ))
  private def p_palm_bay: R = ("palm-bay", "palm-bay-florida", "Palm Bay", "Florida", 28.0345, -80.5887, "America/New_York", false, Seq("Palm Bay"), Seq(
    ("NCG Palm Bay", "NCG Palm Bay", "ncg-palm-bay", "Florida", "Palm Bay")
  ))
  private def p_fargo: R = ("fargo", "fargo-north-dakota", "Fargo", "North Dakota", 46.8772, -96.7898, "America/Chicago", false, Seq("Fargo"), Seq(
    ("Fargo Theatre", "Fargo Theatre", "fargo-theatre", "North Dakota", "Fargo"),
    ("Marcus Century Fargo", "Marcus Century Fargo", "marcus-century-fargo", "North Dakota", "Fargo"),
    ("Marcus West Acres Cinema Fargo", "Marcus West Acres Cinema Fargo", "marcus-west-acres-cinema-fargo", "North Dakota", "Fargo")
  ))
  private def p_clearwater: R = ("clearwater", "clearwater-florida", "Clearwater", "Florida", 27.9658, -82.8001, "America/New_York", false, Seq("Clearwater"), Seq(
    ("CMX Countryside Clearwater", "CMX Countryside Clearwater", "cmx-countryside-clearwater", "Florida", "Clearwater"),
    ("Capitol Theatre Clearwater", "Capitol Theatre Clearwater", "capitol-theatre-clearwater", "Florida", "Clearwater")
  ))
  private def p_independence: R = ("independence", "independence-missouri", "Independence", "Missouri", 39.0911, -94.4155, "America/Chicago", false, Seq("Independence"), Seq(
    ("AMC Independence Commons 20", "AMC Independence Commons 20", "amc-independence-commons-20", "Missouri", "Independence"),
    ("Pharaoh Cinemas Independence", "Pharaoh Cinemas Independence", "pharaoh-cinemas-independence", "Missouri", "Independence")
  ))
  private def p_billings: R = ("billings", "billings-montana", "Billings", "Montana", 45.7833, -108.5007, "America/Denver", false, Seq("Billings"), Seq(
    ("AMC CLASSIC Billings 10", "AMC CLASSIC Billings 10", "amc-classic-billings-10", "Montana", "Billings"),
    ("AMC CLASSIC Shiloh 14", "AMC CLASSIC Shiloh 14", "amc-classic-shiloh-14", "Montana", "Billings"),
    ("Art House Cinema & Pub", "Art House Cinema & Pub", "art-house-cinema-pub", "Montana", "Billings"),
    ("The Babcock Theater", "The Babcock Theater", "the-babcock-theater", "Montana", "Billings")
  ))
  private def p_ann_arbor: R = ("ann-arbor", "ann-arbor-michigan", "Ann Arbor", "Michigan", 42.2776, -83.7409, "America/Detroit", false, Seq("Ann Arbor"), Seq(
    ("Michigan Theater Ann Arbor", "Michigan Theater Ann Arbor", "michigan-theater-ann-arbor", "Michigan", "Ann Arbor"),
    ("State Theater Ann Arbor", "State Theater Ann Arbor", "state-theater-ann-arbor", "Michigan", "Ann Arbor")
  ))
  private def p_westminster: R = ("westminster", "westminster-colorado", "Westminster", "Colorado", 39.8366, -105.0372, "America/Denver", false, Seq("Westminster"), Seq(
    ("Alamo Drafthouse Westminster", "Alamo Drafthouse Westminster", "alamo-drafthouse-westminster", "Colorado", "Westminster"),
    ("AMC Orchard 12 Westminster", "AMC Orchard 12 Westminster", "amc-orchard-12-westminster", "Colorado", "Westminster"),
    ("AMC Westminster Promenade 24", "AMC Westminster Promenade 24", "amc-westminster-promenade-24", "Colorado", "Westminster")
  ))
  private def p_round_rock: R = ("round-rock", "round-rock-texas", "Round Rock", "Texas", 30.5083, -97.6789, "America/Chicago", false, Seq("Round Rock"), Seq(
    ("Cinemark Round Rock 14", "Cinemark Round Rock 14", "cinemark-round-rock-14", "Texas", "Round Rock"),
    ("Flix Brewhouse Round Rock", "Flix Brewhouse Round Rock", "flix-brewhouse-round-rock", "Texas", "Round Rock")
  ))
  private def p_wilmington: R = ("wilmington", "wilmington-north-carolina", "Wilmington", "North Carolina", 34.2356, -77.946, "America/New_York", false, Seq("Wilmington"), Seq(
    ("Regal Mayfaire Wilmington", "Regal Mayfaire Wilmington", "regal-mayfaire-wilmington", "North Carolina", "Wilmington"),
    ("Thalian Hall Center for the Performing Arts", "Thalian Hall Center for the Performing Arts", "thalian-hall-center-for-the-performing-arts", "North Carolina", "Wilmington"),
    ("The Pointe 14", "The Pointe 14", "the-pointe-14", "North Carolina", "Wilmington")
  ))
  private def p_arvada: R = ("arvada", "arvada-colorado", "Arvada", "Colorado", 39.8028, -105.0875, "America/Denver", false, Seq("Arvada"), Seq(
    ("Harkins Arvada 14", "Harkins Arvada 14", "harkins-arvada-14", "Colorado", "Arvada"),
    ("Sonora Cinemas of Arvada", "Sonora Cinemas of Arvada", "sonora-cinemas-of-arvada", "Colorado", "Arvada")
  ))
  private def p_beaumont: R = ("beaumont", "beaumont-texas", "Beaumont", "Texas", 30.0861, -94.1018, "America/Chicago", false, Seq("Beaumont"), Seq(
    ("Cinemark Beaumont 15", "Cinemark Beaumont 15", "cinemark-beaumont-15", "Texas", "Beaumont"),
    ("Regal Star Beaumont", "Regal Star Beaumont", "regal-star-beaumont", "Texas", "Beaumont"),
    ("Jefferson Theatre Beaumont", "Jefferson Theatre Beaumont", "jefferson-theatre-beaumont", "Texas", "Beaumont")
  ))
  private def p_provo: R = ("provo", "provo-utah", "Provo", "Utah", 40.2338, -111.6585, "America/Denver", false, Seq("Provo"), Seq(
    ("AMC Provo 8", "AMC Provo 8", "amc-provo-8", "Utah", "Provo"),
    ("Cinemark Provo 16", "Cinemark Provo 16", "cinemark-provo-16", "Utah", "Provo")
  ))
  private def p_peoria: R = ("peoria", "peoria-illinois", "Peoria", "Illinois", 40.6936, -89.589, "America/Chicago", false, Seq("Peoria"), Seq(
    ("AMC CLASSIC Grand Prairie 18", "AMC CLASSIC Grand Prairie 18", "amc-classic-grand-prairie-18", "Illinois", "Peoria"),
    ("GQT Willow Knolls 14", "GQT Willow Knolls 14", "gqt-willow-knolls-14", "Illinois", "Peoria"),
    ("Peoria Riverfront Museum", "Peoria Riverfront Museum", "peoria-riverfront-museum", "Illinois", "Peoria")
  ))
  private def p_carlsbad: R = ("carlsbad", "carlsbad-california", "Carlsbad", "California", 33.1581, -117.3506, "America/Los_Angeles", false, Seq("Carlsbad"), Seq(
    ("Cinepolis La Costa Paseo Real", "Cinepolis La Costa Paseo Real", "cinepolis-la-costa-paseo-real", "California", "Carlsbad"),
    ("Cinepolis La Costa Town Square", "Cinepolis La Costa Town Square", "cinepolis-la-costa-town-square", "California", "Carlsbad"),
    ("Regal Carlsbad", "Regal Carlsbad", "regal-carlsbad", "California", "Carlsbad")
  ))
  private def p_odessa: R = ("odessa", "odessa-texas", "Odessa", "Texas", 31.8457, -102.3676, "America/Chicago", false, Seq("Odessa"), Seq(
    ("Cinemark Century Odessa 12", "Cinemark Century Odessa 12", "cinemark-century-odessa-12", "Texas", "Odessa"),
    ("Cinergy Odessa 14", "Cinergy Odessa 14", "cinergy-odessa-14", "Texas", "Odessa")
  ))
  private def p_springfield: R = ("springfield", "springfield-illinois", "Springfield", "Illinois", 39.8017, -89.6437, "America/Chicago", false, Seq("Springfield"), Seq(
    ("AMC Springfield 12", "AMC Springfield 12", "amc-springfield-12", "Illinois", "Springfield"),
    ("AMC Springfield 8", "AMC Springfield 8", "amc-springfield-8", "Illinois", "Springfield"),
    ("Route 66 Drive-In Springfield", "Route 66 Drive-In Springfield", "route-66-drive-in-springfield", "Illinois", "Springfield")
  ))
  private def p_downey: R = ("downey", "downey-california", "Downey", "California", 33.94, -118.1326, "America/Los_Angeles", false, Seq("Downey"), Seq(
    ("Cinemark Downey 14", "Cinemark Downey 14", "cinemark-downey-14", "California", "Downey")
  ))
  private def p_costa_mesa: R = ("costa-mesa", "costa-mesa-california", "Costa Mesa", "California", 33.6411, -117.9187, "America/Los_Angeles", false, Seq("Costa Mesa"), Seq(
    ("Regal Metro Pointe Stadium Costa Mesa", "Regal Metro Pointe Stadium Costa Mesa", "regal-metro-pointe-stadium-costa-mesa", "California", "Costa Mesa"),
    ("Starlight Triangle Square Cinemas", "Starlight Triangle Square Cinemas", "starlight-triangle-square-cinemas", "California", "Costa Mesa")
  ))
  private def p_fairfield: R = ("fairfield", "fairfield-california", "Fairfield", "California", 38.2494, -122.04, "America/Los_Angeles", false, Seq("Fairfield"), Seq(
    ("Regal Edwards Fairfield", "Regal Edwards Fairfield", "regal-edwards-fairfield", "California", "Fairfield")
  ))
  private def p_lansing: R = ("lansing", "lansing-michigan", "Lansing", "Michigan", 42.7325, -84.5555, "America/Detroit", false, Seq("Lansing"), Seq(
    ("Celebration! Lansing", "Celebration! Lansing", "celebration-lansing", "Michigan", "Lansing"),
    ("NCG Eastwood Cinema Lansing", "NCG Eastwood Cinema Lansing", "ncg-eastwood-cinema-lansing", "Michigan", "Lansing")
  ))
  private def p_rochester_minnesota: R = ("rochester-minnesota", "rochester-minnesota", "Rochester", "Minnesota", 44.0216, -92.4699, "America/Chicago", false, Seq("Rochester"), Seq(
    ("Marcus Rochester Cinema", "Marcus Rochester Cinema", "marcus-rochester-cinema", "Minnesota", "Rochester"),
    ("Pop's Art Theater", "Pop's Art Theater", "pops-art-theater", "Minnesota", "Rochester")
  ))
  private def p_elgin: R = ("elgin", "elgin-illinois", "Elgin", "Illinois", 42.0373, -88.2812, "America/Chicago", false, Seq("Elgin"), Seq(
    ("Marcus Elgin Cinema", "Marcus Elgin Cinema", "marcus-elgin-cinema", "Illinois", "Elgin")
  ))
  private def p_west_jordan: R = ("west-jordan", "west-jordan-utah", "West Jordan", "Utah", 40.6097, -111.9391, "America/Denver", false, Seq("West Jordan"), Seq(
    ("AMC West Jordan 12", "AMC West Jordan 12", "amc-west-jordan-12", "Utah", "West Jordan"),
    ("Cinemark West Jordan", "Cinemark West Jordan", "cinemark-west-jordan", "Utah", "West Jordan")
  ))
  private def p_inglewood: R = ("inglewood", "inglewood-california", "Inglewood", "California", 33.9617, -118.3531, "America/Los_Angeles", false, Seq("Inglewood"), Seq(
    ("Cinepolis Inglewood", "Cinepolis Inglewood", "cinepolis-inglewood", "California", "Inglewood"),
    ("COSM Hollywood Park", "COSM Hollywood Park", "cosm-hollywood-park", "California", "Inglewood")
  ))
  private def p_tuscaloosa: R = ("tuscaloosa", "tuscaloosa-alabama", "Tuscaloosa", "Alabama", 33.2098, -87.5692, "America/Chicago", false, Seq("Tuscaloosa"), Seq(
    ("CMX Hollywood Tuscaloosa", "CMX Hollywood Tuscaloosa", "cmx-hollywood-tuscaloosa", "Alabama", "Tuscaloosa")
  ))
  private def p_richardson: R = ("richardson", "richardson-texas", "Richardson", "Texas", 32.9482, -96.7297, "America/Chicago", false, Seq("Richardson"), Seq(
    ("Alamo Drafthouse Richardson", "Alamo Drafthouse Richardson", "alamo-drafthouse-richardson", "Texas", "Richardson")
  ))
  private def p_lowell: R = ("lowell", "lowell-massachusetts", "Lowell", "Massachusetts", 42.6334, -71.3162, "America/New_York", false, Seq("Lowell"), Seq(
    ("Showcase Lowell", "Showcase Lowell", "showcase-lowell", "Massachusetts", "Lowell"),
    ("The Owl Theatre at MCC", "The Owl Theatre at MCC", "the-owl-theatre-at-mcc", "Massachusetts", "Lowell")
  ))
  private def p_gresham: R = ("gresham", "gresham-oregon", "Gresham", "Oregon", 45.4982, -122.4315, "America/Los_Angeles", false, Seq("Gresham"), Seq(
    ("Gresham Cinema", "Gresham Cinema", "gresham-cinema", "Oregon", "Gresham"),
    ("Mt Hood Theatre Gresham", "Mt Hood Theatre Gresham", "mt-hood-theatre-gresham", "Oregon", "Gresham"),
    ("Regal Stark St Gresham", "Regal Stark St Gresham", "regal-stark-st-gresham", "Oregon", "Gresham")
  ))
  private def p_cambridge: R = ("cambridge", "cambridge-massachusetts", "Cambridge", "Massachusetts", 42.3751, -71.1056, "America/New_York", false, Seq("Cambridge"), Seq(
    ("Apple Cinemas Cambridge", "Apple Cinemas Cambridge", "apple-cinemas-cambridge", "Massachusetts", "Cambridge"),
    ("Brattle Theatre Cambridge", "Brattle Theatre Cambridge", "brattle-theatre-cambridge", "Massachusetts", "Cambridge"),
    ("Landmark Kendall Square", "Landmark Kendall Square", "landmark-kendall-square", "Massachusetts", "Cambridge"),
    ("Harvard Film Archive Cambridge", "Harvard Film Archive Cambridge", "harvard-film-archive-cambridge", "Massachusetts", "Cambridge"),
    ("MIT Lecture Series Committee", "MIT Lecture Series Committee", "mit-lecture-series-committee", "Massachusetts", "Cambridge")
  ))
  private def p_high_point: R = ("high-point", "high-point-north-carolina", "High Point", "North Carolina", 35.9557, -80.0053, "America/New_York", false, Seq("High Point"), Seq(
    ("AMC High Point 8", "AMC High Point 8", "amc-high-point-8", "North Carolina", "High Point"),
    ("Regal Palladium High Point", "Regal Palladium High Point", "regal-palladium-high-point", "North Carolina", "High Point")
  ))
  private def p_manchester: R = ("manchester", "manchester-new-hampshire", "Manchester", "New Hampshire", 42.9956, -71.4548, "America/New_York", false, Seq("Manchester"), Seq(
    ("Chunky's Manchester", "Chunky's Manchester", "chunkys-manchester", "New Hampshire", "Manchester")
  ))
  private def p_temecula: R = ("temecula", "temecula-california", "Temecula", "California", 33.4936, -117.1484, "America/Los_Angeles", false, Seq("Temecula"), Seq(
    ("AMC Temecula 10", "AMC Temecula 10", "amc-temecula-10", "California", "Temecula"),
    ("Regal Temecula", "Regal Temecula", "regal-temecula", "California", "Temecula"),
    ("Temeku Cinemas", "Temeku Cinemas", "temeku-cinemas", "California", "Temecula")
  ))
  private def p_murrieta: R = ("murrieta", "murrieta-california", "Murrieta", "California", 33.5539, -117.2139, "America/Los_Angeles", false, Seq("Murrieta"), Seq(
    ("Reading Cinemas Cal Oaks", "Reading Cinemas Cal Oaks", "reading-cinemas-cal-oaks", "California", "Murrieta")
  ))
  private def p_centennial: R = ("centennial", "centennial-colorado", "Centennial", "Colorado", 39.5792, -104.8769, "America/Denver", false, Seq("Centennial"), Seq(
    ("Regal SouthGlenn Centennial", "Regal SouthGlenn Centennial", "regal-southglenn-centennial", "Colorado", "Centennial"),
    ("FATHOM", "FATHOM", "fathom", "Colorado", "Centennial")
  ))
  private def p_richmond_california: R = ("richmond-california", "richmond-california", "Richmond", "California", 37.9358, -122.3478, "America/Los_Angeles", false, Seq("Richmond"), Seq(
    ("Cinemark Century Hilltop 16", "Cinemark Century Hilltop 16", "cinemark-century-hilltop-16", "California", "Richmond")
  ))
  private def p_pueblo: R = ("pueblo", "pueblo-colorado", "Pueblo", "Colorado", 38.2544, -104.6091, "America/Denver", false, Seq("Pueblo"), Seq(
    ("Cinemark Tinseltown Pueblo", "Cinemark Tinseltown Pueblo", "cinemark-tinseltown-pueblo", "Colorado", "Pueblo"),
    ("Mesa Drive-In", "Mesa Drive-In", "mesa-drive-in", "Colorado", "Pueblo")
  ))
  private def p_pearland: R = ("pearland", "pearland-texas", "Pearland", "Texas", 29.5636, -95.2861, "America/Chicago", false, Seq("Pearland"), Seq(
    ("Cinemark Pearland 12", "Cinemark Pearland 12", "cinemark-pearland-12", "Texas", "Pearland"),
    ("Pearland Premiere 6", "Pearland Premiere 6", "pearland-premiere-6", "Texas", "Pearland")
  ))
  private def p_waterbury: R = ("waterbury", "waterbury-connecticut", "Waterbury", "Connecticut", 41.5581, -73.0515, "America/New_York", false, Seq("Waterbury"), Seq(
    ("Apple Cinemas Brass Mill", "Apple Cinemas Brass Mill", "apple-cinemas-brass-mill", "Connecticut", "Waterbury")
  ))
  private def p_greeley: R = ("greeley", "greeley-colorado", "Greeley", "Colorado", 40.4233, -104.7091, "America/Denver", false, Seq("Greeley"), Seq(
    ("Cinemark Greeley Mall", "Cinemark Greeley Mall", "cinemark-greeley-mall", "Colorado", "Greeley"),
    ("Kress Cinema & Lounge", "Kress Cinema & Lounge", "kress-cinema-lounge", "Colorado", "Greeley")
  ))
  private def p_west_covina: R = ("west-covina", "west-covina-california", "West Covina", "California", 34.0686, -117.939, "America/Los_Angeles", false, Seq("West Covina"), Seq(
    ("Regal Edwards West Covina", "Regal Edwards West Covina", "regal-edwards-west-covina", "California", "West Covina")
  ))
  private def p_north_charleston: R = ("north-charleston", "north-charleston-south-carolina", "North Charleston", "South Carolina", 32.8546, -79.9748, "America/New_York", false, Seq("North Charleston"), Seq(
    ("Northwoods Stadium Cinema Charleston", "Northwoods Stadium Cinema Charleston", "northwoods-stadium-cinema-charleston", "South Carolina", "North Charleston")
  ))
  private def p_everett: R = ("everett", "everett-washington", "Everett", "Washington", 47.979, -122.2021, "America/Los_Angeles", false, Seq("Everett"), Seq(
    ("Regal Everett", "Regal Everett", "regal-everett", "Washington", "Everett")
  ))
  private def p_college_station: R = ("college-station", "college-station-texas", "College Station", "Texas", 30.628, -96.3344, "America/Chicago", false, Seq("College Station"), Seq(
    ("Cinemark College Station", "Cinemark College Station", "cinemark-college-station", "Texas", "College Station"),
    ("Star Cinema Grill College Station", "Star Cinema Grill College Station", "star-cinema-grill-college-station", "Texas", "College Station")
  ))
  private def p_pompano_beach: R = ("pompano-beach", "pompano-beach-florida", "Pompano Beach", "Florida", 26.2379, -80.1248, "America/New_York", false, Seq("Pompano Beach"), Seq(
    ("AMC Pompano Beach 18", "AMC Pompano Beach 18", "amc-pompano-beach-18", "Florida", "Pompano Beach")
  ))
  private def p_norwalk: R = ("norwalk", "norwalk-california", "Norwalk", "California", 33.9022, -118.0817, "America/Los_Angeles", false, Seq("Norwalk"), Seq(
    ("AMC Norwalk 20", "AMC Norwalk 20", "amc-norwalk-20", "California", "Norwalk"),
    ("Milagro Norwalk 8", "Milagro Norwalk 8", "milagro-norwalk-8", "California", "Norwalk")
  ))
  private def p_boulder: R = ("boulder", "boulder-colorado", "Boulder", "Colorado", 40.015, -105.2706, "America/Denver", false, Seq("Boulder"), Seq(
    ("Boedecker Boulder", "Boedecker Boulder", "boedecker-boulder", "Colorado", "Boulder"),
    ("Cinemark Century Boulder", "Cinemark Century Boulder", "cinemark-century-boulder", "Colorado", "Boulder"),
    ("Muenzinger Auditorium (IFS)", "Muenzinger Auditorium (IFS)", "muenzinger-auditorium-ifs", "Colorado", "Boulder"),
    ("Boulder Theatre", "Boulder Theatre", "boulder-theatre", "Colorado", "Boulder")
  ))
  private def p_broken_arrow: R = ("broken-arrow", "broken-arrow-oklahoma", "Broken Arrow", "Oklahoma", 36.0526, -95.7908, "America/Chicago", false, Seq("Broken Arrow"), Seq(
    ("Cinemark Broken Arrow", "Cinemark Broken Arrow", "cinemark-broken-arrow", "Oklahoma", "Broken Arrow"),
    ("Regal Warren Broken Arrow", "Regal Warren Broken Arrow", "regal-warren-broken-arrow", "Oklahoma", "Broken Arrow"),
    ("Geaux Shows Broken Arrow", "Geaux Shows Broken Arrow", "geaux-shows-broken-arrow", "Oklahoma", "Broken Arrow")
  ))
  private def p_daly_city: R = ("daly-city", "daly-city-california", "Daly City", "California", 37.7058, -122.4619, "America/Los_Angeles", false, Seq("Daly City"), Seq(
    ("Cinemark Century Daly City 20", "Cinemark Century Daly City 20", "cinemark-century-daly-city-20", "California", "Daly City")
  ))
  private def p_burbank: R = ("burbank", "burbank-california", "Burbank", "California", 34.1808, -118.309, "America/Los_Angeles", false, Seq("Burbank"), Seq(
    ("AMC Burbank 16", "AMC Burbank 16", "amc-burbank-16", "California", "Burbank"),
    ("AMC Burbank Town Center 6", "AMC Burbank Town Center 6", "amc-burbank-town-center-6", "California", "Burbank"),
    ("AMC Burbank Town Center 8", "AMC Burbank Town Center 8", "amc-burbank-town-center-8", "California", "Burbank"),
    ("DELUXE DIGITAL CINEMA", "DELUXE DIGITAL CINEMA", "deluxe-digital-cinema", "California", "Burbank"),
    ("ROUNDABOUT WEST", "ROUNDABOUT WEST", "roundabout-west", "California", "Burbank")
  ))
  private def p_green_bay: R = ("green-bay", "green-bay-wisconsin", "Green Bay", "Wisconsin", 44.5192, -88.0198, "America/Chicago", false, Seq("Green Bay"), Seq(
    ("Marcus Bay Park Green Bay", "Marcus Bay Park Green Bay", "marcus-bay-park-green-bay", "Wisconsin", "Green Bay"),
    ("Marcus Green Bay East Cinema", "Marcus Green Bay East Cinema", "marcus-green-bay-east-cinema", "Wisconsin", "Green Bay")
  ))
  private def p_santa_maria: R = ("santa-maria", "santa-maria-california", "Santa Maria", "California", 34.953, -120.4357, "America/Los_Angeles", false, Seq("Santa Maria"), Seq(
    ("Regal Santa Maria Town Center", "Regal Santa Maria Town Center", "regal-santa-maria-town-center", "California", "Santa Maria")
  ))
  private def p_universal_city: R = ("universal-city", "universal-city-california", "Universal City", "California", 34.1389, -118.3534, "America/Los_Angeles", false, Seq("Universal City"), Seq(
    ("AMC at CityWalk Hollywood", "AMC at CityWalk Hollywood", "amc-at-citywalk-hollywood", "California", "Universal City")
  ))
  private def p_wichita_falls: R = ("wichita-falls", "wichita-falls-texas", "Wichita Falls", "Texas", 33.9137, -98.4934, "America/Chicago", false, Seq("Wichita Falls"), Seq(
    ("AMC Sikes Senter 10", "AMC Sikes Senter 10", "amc-sikes-senter-10", "Texas", "Wichita Falls"),
    ("Cinemark Wichita Falls 14", "Cinemark Wichita Falls 14", "cinemark-wichita-falls-14", "Texas", "Wichita Falls")
  ))
  private def p_lakeland: R = ("lakeland", "lakeland-florida", "Lakeland", "Florida", 28.0395, -81.9498, "America/New_York", false, Seq("Lakeland"), Seq(
    ("Cinemark Lakeland Square Mall", "Cinemark Lakeland Square Mall", "cinemark-lakeland-square-mall", "Florida", "Lakeland"),
    ("CMX Lakeside", "CMX Lakeside", "cmx-lakeside", "Florida", "Lakeland"),
    ("Polk Theatre Lakeland", "Polk Theatre Lakeland", "polk-theatre-lakeland", "Florida", "Lakeland"),
    ("Silvermoon Drive-in Lakeland", "Silvermoon Drive-in Lakeland", "silvermoon-drive-in-lakeland", "Florida", "Lakeland")
  ))
  private def p_clovis: R = ("clovis", "clovis-california", "Clovis", "California", 36.8252, -119.7029, "America/Los_Angeles", false, Seq("Clovis"), Seq(
    ("Sierra Vista 16 Clovis", "Sierra Vista 16 Clovis", "sierra-vista-16-clovis", "California", "Clovis")
  ))
  private def p_lewisville: R = ("lewisville", "lewisville-texas", "Lewisville", "Texas", 33.0462, -96.9942, "America/Chicago", false, Seq("Lewisville"), Seq(
    ("Cinemark Lewisville", "Cinemark Lewisville", "cinemark-lewisville", "Texas", "Lewisville"),
    ("Studio Movie Grill The Colony", "Studio Movie Grill The Colony", "studio-movie-grill-the-colony", "Texas", "Lewisville")
  ))
  private def p_tyler: R = ("tyler", "tyler-texas", "Tyler", "Texas", 32.3513, -95.3011, "America/Chicago", false, Seq("Tyler"), Seq(
    ("AMC CLASSIC Tyler 14", "AMC CLASSIC Tyler 14", "amc-classic-tyler-14", "Texas", "Tyler"),
    ("Studio Movie Grill Tyler", "Studio Movie Grill Tyler", "studio-movie-grill-tyler", "Texas", "Tyler"),
    ("Times Square Grand Slam", "Times Square Grand Slam", "times-square-grand-slam", "Texas", "Tyler"),
    ("Liberty Hall Tyler", "Liberty Hall Tyler", "liberty-hall-tyler", "Texas", "Tyler")
  ))
  private def p_el_cajon: R = ("el-cajon", "el-cajon-california", "El Cajon", "California", 32.7948, -116.9625, "America/Los_Angeles", false, Seq("El Cajon"), Seq(
    ("Regal Edwards Rancho San Diego", "Regal Edwards Rancho San Diego", "regal-edwards-rancho-san-diego", "California", "El Cajon"),
    ("Regal Parkway Plaza El Cajon", "Regal Parkway Plaza El Cajon", "regal-parkway-plaza-el-cajon", "California", "El Cajon")
  ))
  private def p_san_mateo: R = ("san-mateo", "san-mateo-california", "San Mateo", "California", 37.563, -122.3255, "America/Los_Angeles", false, Seq("San Mateo"), Seq(
    ("Cinemark Century San Mateo 12", "Cinemark Century San Mateo 12", "cinemark-century-san-mateo-12", "California", "San Mateo"),
    ("Cinepolis San Mateo", "Cinepolis San Mateo", "cinepolis-san-mateo", "California", "San Mateo")
  ))
  private def p_brandon: R = ("brandon", "brandon-florida", "Brandon", "Florida", 27.9378, -82.2859, "America/New_York", false, Seq("Brandon"), Seq(
    ("AMC The Regency 20 Brandon", "AMC The Regency 20 Brandon", "amc-the-regency-20-brandon", "Florida", "Brandon")
  ))
  private def p_rialto: R = ("rialto", "rialto-california", "Rialto", "California", 34.1064, -117.3703, "America/Los_Angeles", false, Seq("Rialto"), Seq(
    ("Cinemark Rialto 13", "Cinemark Rialto 13", "cinemark-rialto-13", "California", "Rialto")
  ))
  private def p_davenport: R = ("davenport", "davenport-iowa", "Davenport", "Iowa", 41.5236, -90.5776, "America/Chicago", false, Seq("Davenport"), Seq(
    ("Cinemark Davenport 18", "Cinemark Davenport 18", "cinemark-davenport-18", "Iowa", "Davenport"),
    ("Last Picture House Davenport", "Last Picture House Davenport", "last-picture-house-davenport", "Iowa", "Davenport"),
    ("Putnam Museum & Science Center", "Putnam Museum & Science Center", "putnam-museum-science-center", "Iowa", "Davenport")
  ))
  private def p_edison: R = ("edison", "edison-new-jersey", "Edison", "New Jersey", 40.5187, -74.4121, "America/New_York", false, Seq("Edison"), Seq(
    ("AMC DINE-IN Menlo Park 12", "AMC DINE-IN Menlo Park 12", "amc-dine-in-menlo-park-12", "New Jersey", "Edison")
  ))
  private def p_hillsboro: R = ("hillsboro", "hillsboro-oregon", "Hillsboro", "Oregon", 45.5229, -122.9898, "America/Los_Angeles", false, Seq("Hillsboro"), Seq(
    ("Regal Evergreen Hillsboro", "Regal Evergreen Hillsboro", "regal-evergreen-hillsboro", "Oregon", "Hillsboro"),
    ("Regal Movies On TV Hillsboro", "Regal Movies On TV Hillsboro", "regal-movies-on-tv-hillsboro", "Oregon", "Hillsboro"),
    ("Laika Studios", "Laika Studios", "laika-studios", "Oregon", "Hillsboro")
  ))
  private def p_las_cruces: R = ("las-cruces", "las-cruces-new-mexico", "Las Cruces", "New Mexico", 32.3123, -106.7783, "America/Denver", false, Seq("Las Cruces"), Seq(
    ("Cineport Las Cruces 10", "Cineport Las Cruces 10", "cineport-las-cruces-10", "New Mexico", "Las Cruces"),
    ("Telshor Las Cruces 12", "Telshor Las Cruces 12", "telshor-las-cruces-12", "New Mexico", "Las Cruces")
  ))
  private def p_south_bend: R = ("south-bend", "south-bend-indiana", "South Bend", "Indiana", 41.6834, -86.25, "America/Indiana/Indianapolis", false, Seq("South Bend"), Seq(
    ("AMC South Bend 16", "AMC South Bend 16", "amc-south-bend-16", "Indiana", "South Bend")
  ))
  private def p_albany: R = ("albany", "albany-new-york", "Albany", "New York", 42.6526, -73.7562, "America/New_York", false, Seq("Albany"), Seq(
    ("Palace Theatre Albany", "Palace Theatre Albany", "palace-theatre-albany", "New York", "Albany"),
    ("Regal Colonie Center Albany", "Regal Colonie Center Albany", "regal-colonie-center-albany", "New York", "Albany"),
    ("Regal Crossgates", "Regal Crossgates", "regal-crossgates", "New York", "Albany"),
    ("Scene One Spectrum 8 Albany", "Scene One Spectrum 8 Albany", "scene-one-spectrum-8-albany", "New York", "Albany"),
    ("Madison Theatre Albany", "Madison Theatre Albany", "madison-theatre-albany", "New York", "Albany")
  ))
  private def p_vista: R = ("vista", "vista-california", "Vista", "California", 33.2, -117.2425, "America/Los_Angeles", false, Seq("Vista"), Seq(
    ("Cinepolis Vista", "Cinepolis Vista", "cinepolis-vista", "California", "Vista")
  ))
  private def p_davie: R = ("davie", "davie-florida", "Davie", "Florida", 26.0629, -80.2331, "America/New_York", false, Seq("Davie"), Seq(
    ("Cinemark Paradise 24", "Cinemark Paradise 24", "cinemark-paradise-24", "Florida", "Davie"),
    ("Paragon Theaters Ridge Davie", "Paragon Theaters Ridge Davie", "paragon-theaters-ridge-davie", "Florida", "Davie")
  ))
  private def p_renton: R = ("renton", "renton-washington", "Renton", "Washington", 47.4829, -122.2171, "America/Los_Angeles", false, Seq("Renton"), Seq(
    ("Regal The Landing Renton", "Regal The Landing Renton", "regal-the-landing-renton", "Washington", "Renton")
  ))
  private def p_roanoke: R = ("roanoke", "roanoke-virginia", "Roanoke", "Virginia", 37.271, -79.9414, "America/New_York", false, Seq("Roanoke"), Seq(
    ("AMC CLASSIC Roanoke 10", "AMC CLASSIC Roanoke 10", "amc-classic-roanoke-10", "Virginia", "Roanoke"),
    ("Grandin Theatre Foundation", "Grandin Theatre Foundation", "grandin-theatre-foundation", "Virginia", "Roanoke"),
    ("Regal Valley View Grande Roanoke", "Regal Valley View Grande Roanoke", "regal-valley-view-grande-roanoke", "Virginia", "Roanoke")
  ))
  private def p_aberdeen: R = ("aberdeen", "aberdeen-south-dakota", "Aberdeen", "South Dakota", 45.4647, -98.4865, "America/Chicago", false, Seq("Aberdeen"), Seq(
    ("Capitol Cinema Aberdeen", "Capitol Cinema Aberdeen", "capitol-cinema-aberdeen", "South Dakota", "Aberdeen"),
    ("Golden Ticket Cinemas Aberdeen", "Golden Ticket Cinemas Aberdeen", "golden-ticket-cinemas-aberdeen", "South Dakota", "Aberdeen")
  ))
  private def p_aberdeen_maryland: R = ("aberdeen-maryland", "aberdeen-maryland", "Aberdeen", "Maryland", 39.5096, -76.1641, "America/New_York", true, Seq("Aberdeen", "Havre de Grace", "Chestertown", "Perryville"), Seq(
    ("Horizon Cinemas Aberdeen", "Horizon Cinemas Aberdeen", "horizon-cinemas-aberdeen", "Maryland", "Aberdeen"),
    ("Flagship Cinemas Chestertown", "Flagship Cinemas Chestertown", "flagship-cinemas-chestertown", "Maryland", "Chestertown"),
    ("State Havre de Grace", "State Havre de Grace", "state-havre-de-grace", "Maryland", "Havre de Grace"),
    ("The Screening Room at Grounded", "The Screening Room at Grounded", "the-screening-room-at-grounded", "Maryland", "Perryville")
  ))
  private def p_aberdeen_mississippi: R = ("aberdeen-mississippi", "aberdeen-mississippi", "Aberdeen", "Mississippi", 33.8251, -88.5437, "America/Chicago", false, Seq("Aberdeen"), Seq(
    ("Elkin Aberdeen", "Elkin Aberdeen", "elkin-aberdeen", "Mississippi", "Aberdeen")
  ))
  private def p_abingdon: R = ("abingdon", "abingdon-virginia", "Abingdon", "Virginia", 36.7098, -81.9774, "America/New_York", false, Seq("Abingdon"), Seq(
    ("Abingdon Cinemall", "Abingdon Cinemall", "abingdon-cinemall", "Virginia", "Abingdon"),
    ("Moonlite Drive-in Abingdon", "Moonlite Drive-in Abingdon", "moonlite-drive-in-abingdon", "Virginia", "Abingdon")
  ))
  private def p_ada: R = ("ada", "ada-oklahoma", "Ada", "Oklahoma", 34.7745, -96.6783, "America/Chicago", false, Seq("Ada"), Seq(
    ("Cinemark North Hills Ada 8", "Cinemark North Hills Ada 8", "cinemark-north-hills-ada-8", "Oklahoma", "Ada")
  ))
  private def p_adams: R = ("adams", "adams-wisconsin", "Adams", "Wisconsin", 43.9561, -89.8182, "America/Chicago", false, Seq("Adams"), Seq(
    ("Adams Theatre", "Adams Theatre", "adams-theatre", "Wisconsin", "Adams")
  ))
  private def p_adrian: R = ("adrian", "adrian-michigan", "Adrian", "Michigan", 41.8976, -84.0372, "America/Detroit", true, Seq("Adrian", "Clinton", "Hudson", "Morenci"), Seq(
    ("MJR Adrian Cinema", "MJR Adrian Cinema", "mjr-adrian-cinema", "Michigan", "Adrian"),
    ("Clinton Theatre", "Clinton Theatre", "clinton-theatre", "Michigan", "Clinton"),
    ("Hudson Cinema", "Hudson Cinema", "hudson-cinema", "Michigan", "Hudson"),
    ("Rex Theatre Morenci", "Rex Theatre Morenci", "rex-theatre-morenci", "Michigan", "Morenci")
  ))
  private def p_afton: R = ("afton", "afton-wyoming", "Afton", "Wyoming", 42.7249, -110.9319, "America/Denver", false, Seq("Afton"), Seq(
    ("Salt River Cinema", "Salt River Cinema", "salt-river-cinema", "Wyoming", "Afton")
  ))
  private def p_agawam: R = ("agawam", "agawam-massachusetts", "Agawam", "Massachusetts", 42.0695, -72.6148, "America/New_York", true, Seq("Agawam", "West Springfield"), Seq(
    ("Agawam Cinemas", "Agawam Cinemas", "agawam-cinemas", "Massachusetts", "Agawam"),
    ("Cinemark West Springfield 15", "Cinemark West Springfield 15", "cinemark-west-springfield-15", "Massachusetts", "West Springfield")
  ))
  private def p_aguadilla: R = ("aguadilla", "aguadilla-puerto-rico", "Aguadilla", "Puerto Rico", 18.4275, -67.1541, "America/Puerto_Rico", true, Seq("Aguadilla", "Isabela"), Seq(
    ("Caribbean Cinemas Aguadilla Mall", "Caribbean Cinemas Aguadilla Mall", "caribbean-cinemas-aguadilla-mall", "Puerto Rico", "Aguadilla"),
    ("Caribbean Cinemas Plaza Isabela", "Caribbean Cinemas Plaza Isabela", "caribbean-cinemas-plaza-isabela", "Puerto Rico", "Isabela")
  ))
  private def p_ainsworth: R = ("ainsworth", "ainsworth-nebraska", "Ainsworth", "Nebraska", 42.55, -99.8626, "America/Chicago", false, Seq("Ainsworth"), Seq(
    ("Ainsworth Grand Theater", "Ainsworth Grand Theater", "ainsworth-grand-theater", "Nebraska", "Ainsworth")
  ))
  private def p_airway_heights: R = ("airway-heights", "airway-heights-washington", "Airway Heights", "Washington", 47.6446, -117.5933, "America/Los_Angeles", true, Seq("Airway Heights", "Town and Country"), Seq(
    ("B&B Airway Heights 8", "B&B Airway Heights 8", "bb-airway-heights-8", "Washington", "Airway Heights"),
    ("Garland Theater Spokane", "Garland Theater Spokane", "garland-theater-spokane", "Washington", "Fort Spokane")
  ))
  private def p_aitkin: R = ("aitkin", "aitkin-minnesota", "Aitkin", "Minnesota", 46.533, -93.7103, "America/Chicago", false, Seq("Aitkin"), Seq(
    ("Rialto Theatre Aitkin", "Rialto Theatre Aitkin", "rialto-theatre-aitkin", "Minnesota", "Aitkin")
  ))
  private def p_alamogordo: R = ("alamogordo", "alamogordo-new-mexico", "Alamogordo", "New Mexico", 32.8995, -105.9603, "America/Denver", false, Seq("Alamogordo"), Seq(
    ("Aviator Alamogordo 10", "Aviator Alamogordo 10", "aviator-alamogordo-10", "New Mexico", "Alamogordo")
  ))
  private def p_alamosa: R = ("alamosa", "alamosa-colorado", "Alamosa", "Colorado", 37.4695, -105.87, "America/Denver", true, Seq("Alamosa", "Monte Vista"), Seq(
    ("Ski-Hi Alamosa 6", "Ski-Hi Alamosa 6", "ski-hi-alamosa-6", "Colorado", "Alamosa"),
    ("Star Drive-in Monte Vista", "Star Drive-in Monte Vista", "star-drive-in-monte-vista", "Colorado", "Monte Vista")
  ))
  private def p_albany_georgia: R = ("albany-georgia", "albany-georgia", "Albany", "Georgia", 31.5785, -84.1557, "America/New_York", false, Seq("Albany"), Seq(
    ("AMC CLASSIC Albany 16", "AMC CLASSIC Albany 16", "amc-classic-albany-16", "Georgia", "Albany")
  ))
  private def p_albany_oregon: R = ("albany-oregon", "albany-oregon", "Albany", "Oregon", 44.6365, -123.1059, "America/Los_Angeles", true, Seq("Albany", "Lebanon", "Sweet Home"), Seq(
    ("Kuhn Theater Lebanon", "Kuhn Theater Lebanon", "kuhn-theater-lebanon", "Oregon", "Lebanon"),
    ("Pix Theatre Albany", "Pix Theatre Albany", "pix-theatre-albany", "Oregon", "Albany"),
    ("Regal Albany", "Regal Albany", "regal-albany", "Oregon", "Albany"),
    ("Rio Theater Sweethome", "Rio Theater Sweethome", "rio-theater-sweethome", "Oregon", "Sweet Home")
  ))
  private def p_albemarle: R = ("albemarle", "albemarle-north-carolina", "Albemarle", "North Carolina", 35.3501, -80.2001, "America/New_York", false, Seq("Albemarle"), Seq(
    ("Eastgate Cinemas Albemarle", "Eastgate Cinemas Albemarle", "eastgate-cinemas-albemarle", "North Carolina", "Albemarle"),
    ("Badin Road Drive-In Albemarle", "Badin Road Drive-In Albemarle", "badin-road-drive-in-albemarle", "North Carolina", "Albemarle")
  ))
  private def p_albertville: R = ("albertville", "albertville-alabama", "Albertville", "Alabama", 34.2678, -86.2088, "America/Chicago", false, Seq("Albertville"), Seq(
    ("Lucas Cinemas Albertville", "Lucas Cinemas Albertville", "lucas-cinemas-albertville", "Alabama", "Albertville")
  ))
  private def p_albion: R = ("albion", "albion-michigan", "Albion", "Michigan", 42.2431, -84.753, "America/Detroit", true, Seq("Albion", "Hillsdale", "Marshall"), Seq(
    ("UEC Theatres Hillsdale", "UEC Theatres Hillsdale", "uec-theatres-hillsdale", "Michigan", "Hillsdale"),
    ("Bohm Theatre Albion", "Bohm Theatre Albion", "bohm-theatre-albion", "Michigan", "Albion"),
    ("Bogar Theatre Marshall", "Bogar Theatre Marshall", "bogar-theatre-marshall", "Michigan", "Marshall")
  ))
  private def p_albion_nebraska: R = ("albion-nebraska", "albion-nebraska", "Albion", "Nebraska", 41.6908, -98.0037, "America/Chicago", false, Seq("Albion"), Seq(
    ("Gateway Theatre", "Gateway Theatre", "gateway-theatre", "Nebraska", "Albion")
  ))
  private def p_alexander_city: R = ("alexander-city", "alexander-city-alabama", "Alexander City", "Alabama", 32.944, -85.9539, "America/Chicago", false, Seq("Alexander City"), Seq(
    ("Playhouse Cinemas Theatre", "Playhouse Cinemas Theatre", "playhouse-cinemas-theatre", "Alabama", "Alexander City")
  ))
  private def p_alexandria: R = ("alexandria", "alexandria-minnesota", "Alexandria", "Minnesota", 45.8852, -95.3775, "America/Chicago", true, Seq("Alexandria", "Sauk Centre", "Long Prairie"), Seq(
    ("Long Prairie Drive-In", "Long Prairie Drive-In", "long-prairie-drive-in", "Minnesota", "Long Prairie"),
    ("Main Street Theatre Sauk Centre", "Main Street Theatre Sauk Centre", "main-street-theatre-sauk-centre", "Minnesota", "Sauk Centre"),
    ("Midway 9 Theatre Alexandria", "Midway 9 Theatre Alexandria", "midway-9-theatre-alexandria", "Minnesota", "Alexandria")
  ))
  private def p_alexandria_bay: R = ("alexandria-bay", "alexandria-bay-new-york", "Alexandria Bay", "New York", 44.3359, -75.9177, "America/New_York", false, Seq("Alexandria Bay"), Seq(
    ("Bay Drive-In Alexandria Bay", "Bay Drive-In Alexandria Bay", "bay-drive-in-alexandria-bay", "New York", "Alexandria Bay")
  ))
  private def p_alexandria_louisiana: R = ("alexandria-louisiana", "alexandria-louisiana", "Alexandria", "Louisiana", 31.3113, -92.4451, "America/Chicago", false, Seq("Alexandria"), Seq(
    ("Santikos Grand Alexandria", "Santikos Grand Alexandria", "santikos-grand-alexandria", "Louisiana", "Alexandria")
  ))
  private def p_algona: R = ("algona", "algona-iowa", "Algona", "Iowa", 43.07, -94.233, "America/Chicago", false, Seq("Algona"), Seq(
    ("Fridley Algone State 5", "Fridley Algone State 5", "fridley-algone-state-5", "Iowa", "Algona")
  ))
  private def p_alhambra: R = ("alhambra", "alhambra-california", "Alhambra", "California", 34.0953, -118.127, "America/Los_Angeles", true, Seq("Alhambra", "Monterey Park", "Arcadia", "La Cañada Flintridge", "Commerce"), Seq(
    ("AMC Atlantic Times Square 14", "AMC Atlantic Times Square 14", "amc-atlantic-times-square-14", "California", "Monterey Park"),
    ("AMC Santa Anita 16", "AMC Santa Anita 16", "amc-santa-anita-16", "California", "Arcadia"),
    ("Regal Edwards Alhambra Renaissance", "Regal Edwards Alhambra Renaissance", "regal-edwards-alhambra-renaissance", "California", "Alhambra"),
    ("Regal Flintridge La Canada", "Regal Flintridge La Canada", "regal-flintridge-la-canada", "California", "La Cañada Flintridge"),
    ("Regency Commerce Cinema 14", "Regency Commerce Cinema 14", "regency-commerce-cinema-14", "California", "Commerce")
  ))
  private def p_alice: R = ("alice", "alice-texas", "Alice", "Texas", 27.7523, -98.0697, "America/Chicago", false, Seq("Alice"), Seq(
    ("Rio Plaza IV Alice", "Rio Plaza IV Alice", "rio-plaza-iv-alice", "Texas", "Alice")
  ))
  private def p_allen: R = ("allen", "allen-texas", "Allen", "Texas", 33.1032, -96.6706, "America/Chicago", true, Seq("Allen", "Wylie", "Rockwall", "Fairview"), Seq(
    ("B&B Wylie 12", "B&B Wylie 12", "bb-wylie-12", "Texas", "Wylie"),
    ("Cinemark Allen 16", "Cinemark Allen 16", "cinemark-allen-16", "Texas", "Allen"),
    ("Cinemark Rockwall 12", "Cinemark Rockwall 12", "cinemark-rockwall-12", "Texas", "Rockwall"),
    ("IPIC Fairview", "IPIC Fairview", "ipic-fairview", "Texas", "Fairview")
  ))
  private def p_alliance: R = ("alliance", "alliance-nebraska", "Alliance", "Nebraska", 42.1016, -102.8722, "America/Denver", false, Seq("Alliance"), Seq(
    ("Alliance Theatre", "Alliance Theatre", "alliance-theatre", "Nebraska", "Alliance"),
    ("Sandhills Drive-In", "Sandhills Drive-In", "sandhills-drive-in", "Nebraska", "Alliance")
  ))
  private def p_alma: R = ("alma", "alma-georgia", "Alma", "Georgia", 31.5394, -82.4624, "America/New_York", false, Seq("Alma"), Seq(
    ("Bacon Theatre Alma", "Bacon Theatre Alma", "bacon-theatre-alma", "Georgia", "Alma")
  ))
  private def p_alpena: R = ("alpena", "alpena-michigan", "Alpena", "Michigan", 45.0617, -83.4327, "America/Detroit", false, Seq("Alpena"), Seq(
    ("Sanctuary Cinemas Alpena", "Sanctuary Cinemas Alpena", "sanctuary-cinemas-alpena", "Michigan", "Alpena")
  ))
  private def p_alpharetta: R = ("alpharetta", "alpharetta-georgia", "Alpharetta", "Georgia", 34.0754, -84.2941, "America/New_York", false, Seq("Alpharetta"), Seq(
    ("AMC North Point Mall 12 Alpharetta", "AMC North Point Mall 12 Alpharetta", "amc-north-point-mall-12-alpharetta", "Georgia", "Alpharetta"),
    ("Regal Avalon Alpharetta", "Regal Avalon Alpharetta", "regal-avalon-alpharetta", "Georgia", "Alpharetta"),
    ("Studio Movie Grill North Point", "Studio Movie Grill North Point", "studio-movie-grill-north-point", "Georgia", "Alpharetta")
  ))
  private def p_alpine: R = ("alpine", "alpine-texas", "Alpine", "Texas", 30.3586, -103.6621, "America/Chicago", false, Seq("Alpine"), Seq(
    ("Rangra Theatre Alpine", "Rangra Theatre Alpine", "rangra-theatre-alpine", "Texas", "Alpine")
  ))
  private def p_alturas: R = ("alturas", "alturas-california", "Alturas", "California", 41.4871, -120.5435, "America/Los_Angeles", false, Seq("Alturas"), Seq(
    ("Niles Theater Alturas", "Niles Theater Alturas", "niles-theater-alturas", "California", "Alturas")
  ))
  private def p_altus: R = ("altus", "altus-oklahoma", "Altus", "Oklahoma", 34.6381, -99.334, "America/Chicago", false, Seq("Altus"), Seq(
    ("Heritage Park Altus 7", "Heritage Park Altus 7", "heritage-park-altus-7", "Oklahoma", "Altus")
  ))
  private def p_alva: R = ("alva", "alva-oklahoma", "Alva", "Oklahoma", 36.805, -98.6672, "America/Chicago", true, Seq("Alva", "Kiowa"), Seq(
    ("Rialto Alva", "Rialto Alva", "rialto-alva", "Oklahoma", "Alva"),
    ("Chief Theater Kiowa", "Chief Theater Kiowa", "chief-theater-kiowa", "Kansas", "Kiowa")
  ))
  private def p_amelia: R = ("amelia", "amelia-ohio", "Amelia", "Ohio", 39.0284, -84.2177, "America/New_York", true, Seq("Amelia", "Forestville", "Milford"), Seq(
    ("AMC Anderson Towne Center 9", "AMC Anderson Towne Center 9", "amc-anderson-towne-center-9", "Ohio", "Forestville"),
    ("Cinemark Milford 16", "Cinemark Milford 16", "cinemark-milford-16", "Ohio", "Milford"),
    ("Pierce Point Cinema", "Pierce Point Cinema", "pierce-point-cinema", "Ohio", "Amelia"),
    ("Starlite Drive-in Amelia", "Starlite Drive-in Amelia", "starlite-drive-in-amelia", "Ohio", "Amelia")
  ))
  private def p_amenia: R = ("amenia", "amenia-new-york", "Amenia", "New York", 41.8499, -73.5577, "America/New_York", true, Seq("Amenia", "Millerton"), Seq(
    ("Four Brothers Drive-in", "Four Brothers Drive-in", "four-brothers-drive-in", "New York", "Amenia"),
    ("Moviehouse Millerton", "Moviehouse Millerton", "moviehouse-millerton", "New York", "Millerton")
  ))
  private def p_ames: R = ("ames", "ames-iowa", "Ames", "Iowa", 42.0347, -93.6199, "America/Chicago", true, Seq("Ames", "Boone", "Nevada", "Story City"), Seq(
    ("Boone Theater", "Boone Theater", "boone-theater", "Iowa", "Boone"),
    ("Cinemark Ames 12", "Cinemark Ames 12", "cinemark-ames-12", "Iowa", "Ames"),
    ("North Grand Cinema", "North Grand Cinema", "north-grand-cinema", "Iowa", "Ames"),
    ("Story Theater Grand Opera House", "Story Theater Grand Opera House", "story-theater-grand-opera-house", "Iowa", "Story City"),
    ("Camelot Theater Nevada", "Camelot Theater Nevada", "camelot-theater-nevada", "Iowa", "Nevada")
  ))
  private def p_amherst_massachusetts: R = ("amherst-massachusetts", "amherst-massachusetts", "Amherst", "Massachusetts", 42.3672, -72.5185, "America/New_York", true, Seq("Amherst", "Greenfield", "South Hadley", "Hadley"), Seq(
    ("Amherst Cinema (Amherst) (Massachusetts)", "Amherst Cinema (Amherst) (Massachusetts)", "amherst-cinema-ma", "Massachusetts", "Amherst"),
    ("Cinemark at Hampshire Mall", "Cinemark at Hampshire Mall", "cinemark-at-hampshire-mall", "Massachusetts", "Hadley"),
    ("Greenfield Garden Cinema 7", "Greenfield Garden Cinema 7", "greenfield-garden-cinema-7", "Massachusetts", "Greenfield"),
    ("South Hadley's Tower Theaters", "South Hadley's Tower Theaters", "south-hadleys-tower-theaters", "Massachusetts", "South Hadley")
  ))
  private def p_anaconda: R = ("anaconda", "anaconda-montana", "Anaconda", "Montana", 46.1285, -112.9423, "America/Denver", true, Seq("Anaconda", "Deer Lodge", "Bald Butte"), Seq(
    ("Rialto Theater Deer Lodge", "Rialto Theater Deer Lodge", "rialto-theater-deer-lodge", "Montana", "Deer Lodge"),
    ("Washoe Theatre Anaconda", "Washoe Theatre Anaconda", "washoe-theatre-anaconda", "Montana", "Anaconda"),
    ("Silver Bow Twin Drive-in", "Silver Bow Twin Drive-in", "silver-bow-twin-drive-in", "Montana", "Bald Butte")
  ))
  private def p_anadarko: R = ("anadarko", "anadarko-oklahoma", "Anadarko", "Oklahoma", 35.0726, -98.2437, "America/Chicago", false, Seq("Anadarko"), Seq(
    ("Redskin Anadarko", "Redskin Anadarko", "redskin-anadarko", "Oklahoma", "Anadarko")
  ))
  private def p_andalusia: R = ("andalusia", "andalusia-alabama", "Andalusia", "Alabama", 31.3081, -86.4824, "America/Chicago", false, Seq("Andalusia"), Seq(
    ("Clark Cinema Andalusia", "Clark Cinema Andalusia", "clark-cinema-andalusia", "Alabama", "Andalusia")
  ))
  private def p_anderson: R = ("anderson", "anderson-south-carolina", "Anderson", "South Carolina", 34.5034, -82.6501, "America/New_York", false, Seq("Anderson"), Seq(
    ("Regal Starlight Stdm Anderson", "Regal Starlight Stdm Anderson", "regal-starlight-stdm-anderson", "South Carolina", "Anderson"),
    ("Santikos AmStar Anderson", "Santikos AmStar Anderson", "santikos-amstar-anderson", "South Carolina", "Anderson")
  ))
  private def p_andover: R = ("andover", "andover-minnesota", "Andover", "Minnesota", 45.2333, -93.2913, "America/Chicago", true, Seq("Andover", "Champlin", "Mounds View", "East Bethel", "Cambridge"), Seq(
    ("AMC CLASSIC Mounds View 15", "AMC CLASSIC Mounds View 15", "amc-classic-mounds-view-15", "Minnesota", "Mounds View"),
    ("Andover Cinema", "Andover Cinema", "andover-cinema", "Minnesota", "Andover"),
    ("Emagine East Bethell", "Emagine East Bethell", "emagine-east-bethell", "Minnesota", "East Bethel"),
    ("GTI Theatres Cambridge", "GTI Theatres Cambridge", "gti-theatres-cambridge", "Minnesota", "Cambridge"),
    ("Mann Theatres Champlin 14", "Mann Theatres Champlin 14", "mann-theatres-champlin-14", "Minnesota", "Champlin")
  ))
  private def p_andrews: R = ("andrews", "andrews-north-carolina", "Andrews", "North Carolina", 35.2017, -83.8241, "America/New_York", true, Seq("Andrews", "Murphy", "Hiawassee", "Blairsville"), Seq(
    ("Blairsville Cinema", "Blairsville Cinema", "blairsville-cinema", "Georgia", "Blairsville"),
    ("VIP Fieldstone 6 Hiawassee", "VIP Fieldstone 6 Hiawassee", "vip-fieldstone-6-hiawassee", "Georgia", "Hiawassee"),
    ("Andrews Twin Cinema", "Andrews Twin Cinema", "andrews-twin-cinema", "North Carolina", "Andrews"),
    ("Henn Theatre Murphy", "Henn Theatre Murphy", "henn-theatre-murphy", "North Carolina", "Murphy")
  ))
  private def p_annapolis: R = ("annapolis", "annapolis-maryland", "Annapolis", "Maryland", 38.9786, -76.4918, "America/New_York", true, Seq("Annapolis", "Gambrills"), Seq(
    ("AMC Annapolis Mall 11", "AMC Annapolis Mall 11", "amc-annapolis-mall-11", "Maryland", "Annapolis"),
    ("Landmark at Annapolis Harbour", "Landmark at Annapolis Harbour", "landmark-at-annapolis-harbour", "Maryland", "Annapolis"),
    ("Regal Waugh Chapel", "Regal Waugh Chapel", "regal-waugh-chapel", "Maryland", "Gambrills")
  ))
  private def p_anniston: R = ("anniston", "anniston-alabama", "Anniston", "Alabama", 33.6598, -85.8316, "America/Chicago", false, Seq("Anniston"), Seq(
    ("Santikos AmStar Oxford", "Santikos AmStar Oxford", "santikos-amstar-oxford", "Alabama", "Anniston")
  ))
  private def p_antigo: R = ("antigo", "antigo-wisconsin", "Antigo", "Wisconsin", 45.1403, -89.1523, "America/Chicago", false, Seq("Antigo"), Seq(
    ("Palace Twin Antigo", "Palace Twin Antigo", "palace-twin-antigo", "Wisconsin", "Antigo")
  ))
  private def p_antlers: R = ("antlers", "antlers-oklahoma", "Antlers", "Oklahoma", 34.2312, -95.6202, "America/Chicago", false, Seq("Antlers"), Seq(
    ("Cinema One Antlers", "Cinema One Antlers", "cinema-one-antlers", "Oklahoma", "Antlers")
  ))
  private def p_appleton: R = ("appleton", "appleton-wisconsin", "Appleton", "Wisconsin", 44.2619, -88.4154, "America/Chicago", true, Seq("Appleton", "Kaukauna"), Seq(
    ("Marcus Hollywood Appleton", "Marcus Hollywood Appleton", "marcus-hollywood-appleton", "Wisconsin", "Appleton"),
    ("Marcus Valley Grand Cinema Appleton", "Marcus Valley Grand Cinema Appleton", "marcus-valley-grand-cinema-appleton", "Wisconsin", "Appleton"),
    ("Field of Scenes Drive-in Kaukauna", "Field of Scenes Drive-in Kaukauna", "field-of-scenes-drive-in-kaukauna", "Wisconsin", "Kaukauna")
  ))
  private def p_arapahoe: R = ("arapahoe", "arapahoe-nebraska", "Arapahoe", "Nebraska", 40.3042, -99.9004, "America/Chicago", false, Seq("Arapahoe"), Seq(
    ("Crystal Theatre Arapahoe", "Crystal Theatre Arapahoe", "crystal-theatre-arapahoe", "Nebraska", "Arapahoe")
  ))
  private def p_ardmore: R = ("ardmore", "ardmore-oklahoma", "Ardmore", "Oklahoma", 34.1743, -97.1436, "America/Chicago", false, Seq("Ardmore"), Seq(
    ("AMC CLASSIC Ardmore 8", "AMC CLASSIC Ardmore 8", "amc-classic-ardmore-8", "Oklahoma", "Ardmore")
  ))
  private def p_arecibo: R = ("arecibo", "arecibo-puerto-rico", "Arecibo", "Puerto Rico", 18.4724, -66.7157, "America/Puerto_Rico", true, Seq("Arecibo", "Barceloneta", "Hatillo"), Seq(
    ("Auto Cine Santana", "Auto Cine Santana", "auto-cine-santana", "Puerto Rico", "Arecibo"),
    ("Caribbean Cinemas Arecibo", "Caribbean Cinemas Arecibo", "caribbean-cinemas-arecibo", "Puerto Rico", "Arecibo"),
    ("Caribbean Cinemas Barceloneta", "Caribbean Cinemas Barceloneta", "caribbean-cinemas-barceloneta", "Puerto Rico", "Barceloneta"),
    ("Caribbean Cinemas Plaza del Norte", "Caribbean Cinemas Plaza del Norte", "caribbean-cinemas-plaza-del-norte", "Puerto Rico", "Hatillo")
  ))
  private def p_arnold: R = ("arnold", "arnold-missouri", "Arnold", "Missouri", 38.4328, -90.3776, "America/Chicago", true, Seq("Arnold", "Festus", "Waterloo"), Seq(
    ("B&B Festus 8", "B&B Festus 8", "bb-festus-8", "Missouri", "Festus"),
    ("Marcus Arnold Cinema", "Marcus Arnold Cinema", "marcus-arnold-cinema", "Missouri", "Arnold"),
    ("RMC Waterloo Cinema", "RMC Waterloo Cinema", "rmc-waterloo-cinema", "Illinois", "Waterloo")
  ))
  private def p_arnold_nebraska: R = ("arnold-nebraska", "arnold-nebraska", "Arnold", "Nebraska", 41.4224, -100.1951, "America/Chicago", false, Seq("Arnold"), Seq(
    ("Rialto Theatre Arnold", "Rialto Theatre Arnold", "rialto-theatre-arnold", "Nebraska", "Arnold")
  ))
  private def p_arroyo_grande: R = ("arroyo-grande", "arroyo-grande-california", "Arroyo Grande", "California", 35.1186, -120.5907, "America/Los_Angeles", false, Seq("Arroyo Grande"), Seq(
    ("Fair Oaks Arroyo Grande", "Fair Oaks Arroyo Grande", "fair-oaks-arroyo-grande", "California", "Arroyo Grande"),
    ("Regal Arroyo Grande", "Regal Arroyo Grande", "regal-arroyo-grande", "California", "Arroyo Grande")
  ))
  private def p_ashburn: R = ("ashburn", "ashburn-virginia", "Ashburn", "Virginia", 39.0437, -77.4875, "America/New_York", false, Seq("Ashburn"), Seq(
    ("Alamo Drafthouse One Loudoun", "Alamo Drafthouse One Loudoun", "alamo-drafthouse-one-loudoun", "Virginia", "Ashburn"),
    ("AMC Loudoun Station 11 Ashburn", "AMC Loudoun Station 11 Ashburn", "amc-loudoun-station-11-ashburn", "Virginia", "Ashburn"),
    ("Regal Fox Ashburn", "Regal Fox Ashburn", "regal-fox-ashburn", "Virginia", "Ashburn")
  ))
  private def p_asheboro: R = ("asheboro", "asheboro-north-carolina", "Asheboro", "North Carolina", 35.7079, -79.8136, "America/New_York", false, Seq("Asheboro"), Seq(
    ("Cinemark Asheboro 7", "Cinemark Asheboro 7", "cinemark-asheboro-7", "North Carolina", "Asheboro")
  ))
  private def p_asheville: R = ("asheville", "asheville-north-carolina", "Asheville", "North Carolina", 35.6009, -82.554, "America/New_York", false, Seq("Asheville"), Seq(
    ("AMC River Hills 10", "AMC River Hills 10", "amc-river-hills-10", "North Carolina", "Asheville"),
    ("Asheville Pizza & Brewing", "Asheville Pizza & Brewing", "asheville-pizza-brewing", "North Carolina", "Asheville"),
    ("Cinemark Carolina Asheville", "Cinemark Carolina Asheville", "cinemark-carolina-asheville", "North Carolina", "Asheville"),
    ("Fine Arts Asheville", "Fine Arts Asheville", "fine-arts-asheville", "North Carolina", "Asheville"),
    ("Regal Biltmore Grande Stadium Asheville", "Regal Biltmore Grande Stadium Asheville", "regal-biltmore-grande-stadium-asheville", "North Carolina", "Asheville"),
    ("Grail Moviehouse", "Grail Moviehouse", "grail-moviehouse", "North Carolina", "Asheville")
  ))
  private def p_ashland: R = ("ashland", "ashland-kentucky", "Ashland", "Kentucky", 38.4784, -82.6379, "America/New_York", true, Seq("Ashland", "Huntington", "Barboursville"), Seq(
    ("Cinema at Camp Landing 10", "Cinema at Camp Landing 10", "cinema-at-camp-landing-10", "Kentucky", "Ashland"),
    ("Cinemark Town Ashland", "Cinemark Town Ashland", "cinemark-town-ashland", "Kentucky", "Ashland"),
    ("Cinemark Huntington Mall", "Cinemark Huntington Mall", "cinemark-huntington-mall", "West Virginia", "Barboursville"),
    ("Marquee Pullman Square 16", "Marquee Pullman Square 16", "marquee-pullman-square-16", "West Virginia", "Huntington")
  ))
  private def p_ashland_ohio: R = ("ashland-ohio", "ashland-ohio", "Ashland", "Ohio", 40.8687, -82.3182, "America/New_York", true, Seq("Ashland", "Loudonville"), Seq(
    ("Ohio Theatre Loudonville", "Ohio Theatre Loudonville", "ohio-theatre-loudonville", "Ohio", "Loudonville"),
    ("The Ashland Schine Theater", "The Ashland Schine Theater", "the-ashland-schine-theater", "Ohio", "Ashland")
  ))
  private def p_ashland_wisconsin: R = ("ashland-wisconsin", "ashland-wisconsin", "Ashland", "Wisconsin", 46.5924, -90.8838, "America/Chicago", false, Seq("Ashland"), Seq(
    ("Ashland Bay Cinema", "Ashland Bay Cinema", "ashland-bay-cinema", "Wisconsin", "Ashland")
  ))
  private def p_ashtabula: R = ("ashtabula", "ashtabula-ohio", "Ashtabula", "Ohio", 41.865, -80.7898, "America/New_York", false, Seq("Ashtabula"), Seq(
    ("J-PAL Theater Ashtabula", "J-PAL Theater Ashtabula", "j-pal-theater-ashtabula", "Ohio", "Ashtabula")
  ))
  private def p_aspen: R = ("aspen", "aspen-colorado", "Aspen", "Colorado", 39.1911, -106.8175, "America/Denver", false, Seq("Aspen"), Seq(
    ("BTM Isis Theatre Aspen", "BTM Isis Theatre Aspen", "btm-isis-theatre-aspen", "Colorado", "Aspen"),
    ("Aspen Film", "Aspen Film", "aspen-film", "Colorado", "Aspen")
  ))
  private def p_astoria: R = ("astoria", "astoria-oregon", "Astoria", "Oregon", 46.1879, -123.8312, "America/Los_Angeles", false, Seq("Astoria"), Seq(
    ("Astoria Gateway Cinema", "Astoria Gateway Cinema", "astoria-gateway-cinema", "Oregon", "Astoria"),
    ("Columbian Astoria", "Columbian Astoria", "columbian-astoria", "Oregon", "Astoria")
  ))
  private def p_atascadero: R = ("atascadero", "atascadero-california", "Atascadero", "California", 35.4894, -120.6707, "America/Los_Angeles", true, Seq("Atascadero", "Paso Robles", "Morro Bay"), Seq(
    ("Bay Theatre Morro Bay", "Bay Theatre Morro Bay", "bay-theatre-morro-bay", "California", "Morro Bay"),
    ("Colony Cinemas Atascadero", "Colony Cinemas Atascadero", "colony-cinemas-atascadero", "California", "Atascadero"),
    ("Park Cinemas Paso Robles", "Park Cinemas Paso Robles", "park-cinemas-paso-robles", "California", "Paso Robles")
  ))
  private def p_atchison: R = ("atchison", "atchison-kansas", "Atchison", "Kansas", 39.563, -95.1216, "America/Chicago", false, Seq("Atchison"), Seq(
    ("Fox Theatre Atchison", "Fox Theatre Atchison", "fox-theatre-atchison", "Kansas", "Atchison")
  ))
  private def p_athens_ohio: R = ("athens-ohio", "athens-ohio", "Athens", "Ohio", 39.3292, -82.1013, "America/New_York", true, Seq("Athens", "Nelsonville"), Seq(
    ("Athena Cinema", "Athena Cinema", "athena-cinema", "Ohio", "Athens"),
    ("Movies Nelsonville 10", "Movies Nelsonville 10", "movies-nelsonville-10", "Ohio", "Nelsonville")
  ))
  private def p_athens_tennessee: R = ("athens-tennessee", "athens-tennessee", "Athens", "Tennessee", 35.4428, -84.593, "America/New_York", false, Seq("Athens"), Seq(
    ("Athens Movie Palace", "Athens Movie Palace", "athens-movie-palace", "Tennessee", "Athens"),
    ("Swingin' Midway Drive In", "Swingin' Midway Drive In", "swingin-midway-drive-in", "Tennessee", "Athens")
  ))
  private def p_athens_texas: R = ("athens-texas", "athens-texas", "Athens", "Texas", 32.2049, -95.8555, "America/Chicago", false, Seq("Athens"), Seq(
    ("Cinemark Athens Cinema 4", "Cinemark Athens Cinema 4", "cinemark-athens-cinema-4", "Texas", "Athens")
  ))
  private def p_athol: R = ("athol", "athol-massachusetts", "Athol", "Massachusetts", 42.5959, -72.2267, "America/New_York", true, Seq("Athol", "Jaffrey", "Hinsdale"), Seq(
    ("The Park Theatre Jaffrey", "The Park Theatre Jaffrey", "the-park-theatre-jaffrey", "New Hampshire", "Jaffrey"),
    ("Athol Cinemas 8", "Athol Cinemas 8", "athol-cinemas-8", "Massachusetts", "Athol"),
    ("Northfield Drive-in Hinsdale", "Northfield Drive-in Hinsdale", "northfield-drive-in-hinsdale", "New Hampshire", "Hinsdale")
  ))
  private def p_atlantic_city: R = ("atlantic-city", "atlantic-city-new-jersey", "Atlantic City", "New Jersey", 39.3642, -74.4231, "America/New_York", true, Seq("Atlantic City", "Ocean City", "Ventnor City", "Northfield", "Mays Landing"), Seq(
    ("IMAX Theatre At Tropicana", "IMAX Theatre At Tropicana", "imax-theatre-at-tropicana", "New Jersey", "Atlantic City"),
    ("OVATION HALL", "OVATION HALL", "ovation-hall", "New Jersey", "Atlantic City"),
    ("Tilton Theatre Northfield", "Tilton Theatre Northfield", "tilton-theatre-northfield", "New Jersey", "Northfield"),
    ("Ventnor Square Theatre", "Ventnor Square Theatre", "ventnor-square-theatre", "New Jersey", "Ventnor City"),
    ("Moorlyn Square Theatre", "Moorlyn Square Theatre", "moorlyn-square-theatre", "New Jersey", "Ocean City"),
    ("Regal Hamilton Commons", "Regal Hamilton Commons", "regal-hamilton-commons", "New Jersey", "Mays Landing")
  ))
  private def p_atmore: R = ("atmore", "atmore-alabama", "Atmore", "Alabama", 31.0238, -87.4939, "America/Chicago", false, Seq("Atmore"), Seq(
    ("Wind Creek Cinema", "Wind Creek Cinema", "wind-creek-cinema", "Alabama", "Atmore"),
    ("The Strand Theatre Atmore", "The Strand Theatre Atmore", "the-strand-theatre-atmore", "Alabama", "Atmore")
  ))
  private def p_atwood: R = ("atwood", "atwood-kansas", "Atwood", "Kansas", 39.8067, -101.0421, "America/Chicago", false, Seq("Atwood"), Seq(
    ("Jayhawk Theatre Atwood", "Jayhawk Theatre Atwood", "jayhawk-theatre-atwood", "Kansas", "Atwood")
  ))
  private def p_auburn: R = ("auburn", "auburn-new-york", "Auburn", "New York", 42.9317, -76.5661, "America/New_York", false, Seq("Auburn"), Seq(
    ("Auburn Movieplex", "Auburn Movieplex", "auburn-movieplex", "New York", "Auburn"),
    ("Fingerlake Drive-in Auburn", "Fingerlake Drive-in Auburn", "fingerlake-drive-in-auburn", "New York", "Auburn"),
    ("Track Cinema Auburn", "Track Cinema Auburn", "track-cinema-auburn", "New York", "Auburn")
  ))
  private def p_auburn_california: R = ("auburn-california", "auburn-california", "Auburn", "California", 38.8966, -121.0769, "America/Los_Angeles", false, Seq("Auburn"), Seq(
    ("Regal Auburn California", "Regal Auburn California", "regal-auburn-california", "California", "Auburn"),
    ("Auburn State Theatre", "Auburn State Theatre", "auburn-state-theatre", "California", "Auburn")
  ))
  private def p_auburn_maine: R = ("auburn-maine", "auburn-maine", "Auburn", "Maine", 44.0979, -70.2312, "America/New_York", true, Seq("Auburn", "Brunswick", "Topsham"), Seq(
    ("Flagship Cinemas Auburn", "Flagship Cinemas Auburn", "flagship-cinemas-auburn", "Maine", "Auburn"),
    ("Eveningstar Cinema", "Eveningstar Cinema", "eveningstar-cinema", "Maine", "Brunswick"),
    ("Smitty's 6 Cinema Topsham", "Smitty's 6 Cinema Topsham", "smittys-6-cinema-topsham", "Maine", "Topsham")
  ))
  private def p_auburn_washington: R = ("auburn-washington", "auburn-washington", "Auburn", "Washington", 47.3073, -122.2284, "America/Los_Angeles", true, Seq("Auburn", "Bonney Lake", "Enumclaw"), Seq(
    ("Chalet Cinema Enumclaw", "Chalet Cinema Enumclaw", "chalet-cinema-enumclaw", "Washington", "Enumclaw"),
    ("Regal Auburn Washington", "Regal Auburn Washington", "regal-auburn-washington", "Washington", "Auburn"),
    ("Regal Tall Firs Bonney Lake", "Regal Tall Firs Bonney Lake", "regal-tall-firs-bonney-lake", "Washington", "Bonney Lake")
  ))
  private def p_augusta: R = ("augusta", "augusta-maine", "Augusta", "Maine", 44.3106, -69.7795, "America/New_York", true, Seq("Augusta", "Damariscotta", "Boothbay Harbor"), Seq(
    ("Regal Augusta", "Regal Augusta", "regal-augusta", "Maine", "Augusta"),
    ("Harbor Theater (Boothbay Harbor)", "Harbor Theater (Boothbay Harbor)", "harbor-theater-me", "Maine", "Boothbay Harbor"),
    ("Lincoln Damariscotta", "Lincoln Damariscotta", "lincoln-damariscotta", "Maine", "Damariscotta")
  ))
  private def p_augusta_georgia: R = ("augusta-georgia", "augusta-georgia", "Augusta", "Georgia", 33.471, -81.9748, "America/New_York", true, Seq("Augusta", "Aiken", "Evans"), Seq(
    ("Regal Aiken Mall", "Regal Aiken Mall", "regal-aiken-mall", "South Carolina", "Aiken"),
    ("GTC Evans", "GTC Evans", "gtc-evans", "Georgia", "Evans"),
    ("GTC Riverwatch Cinemas Augusta", "GTC Riverwatch Cinemas Augusta", "gtc-riverwatch-cinemas-augusta", "Georgia", "Augusta"),
    ("Regal Augusta Exchange", "Regal Augusta Exchange", "regal-augusta-exchange", "Georgia", "Augusta")
  ))
  private def p_aurora_nebraska: R = ("aurora-nebraska", "aurora-nebraska", "Aurora", "Nebraska", 40.8672, -98.0042, "America/Chicago", false, Seq("Aurora"), Seq(
    ("12th Street Cinema Aurora", "12th Street Cinema Aurora", "12th-street-cinema-aurora", "Nebraska", "Aurora")
  ))
  private def p_austin_minnesota: R = ("austin-minnesota", "austin-minnesota", "Austin", "Minnesota", 43.6666, -92.9746, "America/Chicago", true, Seq("Austin", "Albert Lea", "Lake Mills", "Northwood"), Seq(
    ("Mills Theatre – Lake Mills", "Mills Theatre – Lake Mills", "mills-theatre--lake-mills", "Iowa", "Lake Mills"),
    ("Northwood Theatre", "Northwood Theatre", "northwood-theatre", "Iowa", "Northwood"),
    ("Mall Cinema 7 Albert Lea", "Mall Cinema 7 Albert Lea", "mall-cinema-7-albert-lea", "Minnesota", "Albert Lea"),
    ("Historic Paramount Theatre Austin", "Historic Paramount Theatre Austin", "historic-paramount-theatre-austin", "Minnesota", "Austin")
  ))
  private def p_ava: R = ("ava", "ava-missouri", "Ava", "Missouri", 36.952, -92.6604, "America/Chicago", false, Seq("Ava"), Seq(
    ("Ava TrueView Family Theater", "Ava TrueView Family Theater", "ava-trueview-family-theater", "Missouri", "Ava")
  ))
  private def p_avalon: R = ("avalon", "avalon-california", "Avalon", "California", 33.3428, -118.3278, "America/Los_Angeles", false, Seq("Avalon"), Seq(
    ("Avalon Theatre (Catalina Casino)", "Avalon Theatre (Catalina Casino)", "avalon-theatre-catalina-casino", "California", "Avalon")
  ))
  private def p_avon: R = ("avon", "avon-new-york", "Avon", "New York", 42.912, -77.7456, "America/New_York", true, Seq("Avon", "Geneseo", "Victor"), Seq(
    ("Geneseo Theaters", "Geneseo Theaters", "geneseo-theaters", "New York", "Geneseo"),
    ("Regal Eastview Victor", "Regal Eastview Victor", "regal-eastview-victor", "New York", "Victor"),
    ("Vintage Drive-In Avon", "Vintage Drive-In Avon", "vintage-drive-in-avon", "New York", "Avon"),
    ("Avon Park Theater", "Avon Park Theater", "avon-park-theater", "New York", "Avon")
  ))
  private def p_avondale: R = ("avondale", "avondale-arizona", "Avondale", "Arizona", 33.4356, -112.3496, "America/Phoenix", true, Seq("Avondale", "Goodyear"), Seq(
    ("Harkins Estrella Falls Goodyear", "Harkins Estrella Falls Goodyear", "harkins-estrella-falls-goodyear", "Arizona", "Goodyear"),
    ("Harkins Gateway Pavilions 18", "Harkins Gateway Pavilions 18", "harkins-gateway-pavilions-18", "Arizona", "Avondale")
  ))
  private def p_bad_axe: R = ("bad-axe", "bad-axe-michigan", "Bad Axe", "Michigan", 43.802, -83.0008, "America/Detroit", true, Seq("Bad Axe", "Cass City", "Harbor Beach"), Seq(
    ("Harbor Beach Community Theatre", "Harbor Beach Community Theatre", "harbor-beach-community-theatre", "Michigan", "Harbor Beach"),
    ("Cass Theatre", "Cass Theatre", "cass-theatre", "Michigan", "Cass City"),
    ("Bad Axe Theatre", "Bad Axe Theatre", "bad-axe-theatre", "Michigan", "Bad Axe")
  ))
  private def p_bainbridge_island: R = ("bainbridge-island", "bainbridge-island-washington", "Bainbridge Island", "Washington", 47.6262, -122.5212, "America/Los_Angeles", true, Seq("Bainbridge Island", "Silverdale", "Poulsbo", "Kingston"), Seq(
    ("AMC Kitsap 8", "AMC Kitsap 8", "amc-kitsap-8", "Washington", "Silverdale"),
    ("Bainbridge Cinemas", "Bainbridge Cinemas", "bainbridge-cinemas", "Washington", "Bainbridge Island"),
    ("Historic Lynwood Theatre", "Historic Lynwood Theatre", "historic-lynwood-theatre", "Washington", "Bainbridge Island"),
    ("Firehouse Theater Kingston", "Firehouse Theater Kingston", "firehouse-theater-kingston", "Washington", "Kingston"),
    ("Regal Poulsbo", "Regal Poulsbo", "regal-poulsbo", "Washington", "Poulsbo")
  ))
  private def p_baker_city: R = ("baker-city", "baker-city-oregon", "Baker City", "Oregon", 44.7749, -117.8344, "America/Los_Angeles", false, Seq("Baker City"), Seq(
    ("Eltrym Theater", "Eltrym Theater", "eltrym-theater", "Oregon", "Baker City")
  ))
  private def p_bangor: R = ("bangor", "bangor-maine", "Bangor", "Maine", 44.7988, -68.7726, "America/New_York", true, Seq("Bangor", "Orono", "Ellsworth", "Bucksport"), Seq(
    ("Alamo Theatre Bucksport", "Alamo Theatre Bucksport", "alamo-theatre-bucksport", "Maine", "Bucksport"),
    ("Bangor Mall Cinema 10", "Bangor Mall Cinema 10", "bangor-mall-cinema-10", "Maine", "Bangor"),
    ("Black Bear Cinemas", "Black Bear Cinemas", "black-bear-cinemas", "Maine", "Orono"),
    ("The Grand – Ellsworth", "The Grand – Ellsworth", "the-grand---ellsworth", "Maine", "Ellsworth")
  ))
  private def p_bar_harbor: R = ("bar-harbor", "bar-harbor-maine", "Bar Harbor", "Maine", 44.3876, -68.2039, "America/New_York", false, Seq("Bar Harbor"), Seq(
    ("Reel Pizza Cinerama", "Reel Pizza Cinerama", "reel-pizza-cinerama", "Maine", "Bar Harbor"),
    ("1932 Criterion Theatre", "1932 Criterion Theatre", "1932-criterion-theatre", "Maine", "Bar Harbor")
  ))
  private def p_barberton: R = ("barberton", "barberton-ohio", "Barberton", "Ohio", 41.0128, -81.6051, "America/New_York", true, Seq("Barberton", "Cuyahoga Falls", "Wadsworth"), Seq(
    ("Cinemark Cuyahoga", "Cinemark Cuyahoga", "cinemark-cuyahoga", "Ohio", "Cuyahoga Falls"),
    ("Lake Movies Barberton 8", "Lake Movies Barberton 8", "lake-8-movies-barberton-8", "Ohio", "Barberton"),
    ("Magic City Drive-in Barberton", "Magic City Drive-in Barberton", "magic-city-drive-in-barberton", "Ohio", "Barberton"),
    ("Blue Sky Drive-in Wadsworth", "Blue Sky Drive-in Wadsworth", "blue-sky-drive-in-wadsworth", "Ohio", "Wadsworth")
  ))
  private def p_barrington: R = ("barrington", "barrington-new-hampshire", "Barrington", "New Hampshire", 43.2229, -71.047, "America/New_York", true, Seq("Barrington", "Epping"), Seq(
    ("BarnZ's Barrington Cinema", "BarnZ's Barrington Cinema", "barnzs-barrington-cinema", "New Hampshire", "Barrington"),
    ("O'Neil Cinemas Epping", "O'Neil Cinemas Epping", "oneil-cinemas-epping", "New Hampshire", "Epping")
  ))
  private def p_barstow: R = ("barstow", "barstow-california", "Barstow", "California", 34.8986, -117.0228, "America/Los_Angeles", false, Seq("Barstow"), Seq(
    ("Barstow Station is D'Place", "Barstow Station is D'Place", "barstow-station-is-dplace", "California", "Barstow"),
    ("Skyline Drive-in Barstow", "Skyline Drive-in Barstow", "skyline-drive-in-barstow", "California", "Barstow")
  ))
  private def p_bartlesville: R = ("bartlesville", "bartlesville-oklahoma", "Bartlesville", "Oklahoma", 36.7473, -95.9808, "America/Chicago", false, Seq("Bartlesville"), Seq(
    ("Regal Washington Park Bartlesville", "Regal Washington Park Bartlesville", "regal-washington-park-bartlesville", "Oklahoma", "Bartlesville")
  ))
  private def p_bastrop: R = ("bastrop", "bastrop-texas", "Bastrop", "Texas", 30.1105, -97.3153, "America/Chicago", false, Seq("Bastrop"), Seq(
    ("Film Alley Bastrop", "Film Alley Bastrop", "film-alley-bastrop", "Texas", "Bastrop")
  ))
  private def p_batesville: R = ("batesville", "batesville-arkansas", "Batesville", "Arkansas", 35.7698, -91.641, "America/Chicago", false, Seq("Batesville"), Seq(
    ("Melba Theatre Batesville", "Melba Theatre Batesville", "melba-theatre-batesville", "Arkansas", "Batesville"),
    ("Oaks 7 VIP Cinema", "Oaks 7 VIP Cinema", "oaks-7-vip-cinema", "Arkansas", "Batesville")
  ))
  private def p_battle_creek: R = ("battle-creek", "battle-creek-michigan", "Battle Creek", "Michigan", 42.3173, -85.1782, "America/Detroit", false, Seq("Battle Creek"), Seq(
    ("GQT Lakeview 10", "GQT Lakeview 10", "ncg-battle-creek", "Michigan", "Battle Creek"),
    ("GQT W. Columbia 7", "GQT W. Columbia 7", "gqt-w-columbia-7", "Michigan", "Battle Creek"),
    ("JC Cinema Battle Creek", "JC Cinema Battle Creek", "jc-cinema-battle-creek", "Michigan", "Battle Creek")
  ))
  private def p_baudette: R = ("baudette", "baudette-minnesota", "Baudette", "Minnesota", 48.7125, -94.5999, "America/Chicago", false, Seq("Baudette"), Seq(
    ("Grand Theatre Baudette", "Grand Theatre Baudette", "grand-theatre-baudette", "Minnesota", "Baudette")
  ))
  private def p_baxter: R = ("baxter", "baxter-minnesota", "Baxter", "Minnesota", 46.3433, -94.2867, "America/Chicago", false, Seq("Baxter"), Seq(
    ("Mann Theatres Lakes 12", "Mann Theatres Lakes 12", "mann-theatres-lakes-12", "Minnesota", "Baxter")
  ))
  private def p_bay_city: R = ("bay-city", "bay-city-texas", "Bay City", "Texas", 28.9828, -95.9694, "America/Chicago", false, Seq("Bay City"), Seq(
    ("Schulman's Movie Bowl Grille Bay City", "Schulman's Movie Bowl Grille Bay City", "schulmans-movie-bowl-grille-bay-city", "Texas", "Bay City")
  ))
  private def p_baytown: R = ("baytown", "baytown-texas", "Baytown", "Texas", 29.7355, -94.9774, "America/Chicago", true, Seq("Baytown", "Webster", "Jacinto City"), Seq(
    ("Cinemark Tinseltown Jacinto City", "Cinemark Tinseltown Jacinto City", "cinemark-tinseltown-jacinto-city", "Texas", "Jacinto City"),
    ("Cinemark Webster 18", "Cinemark Webster 18", "cinemark-webster-18", "Texas", "Webster"),
    ("Hooky Entertainment Baytown", "Hooky Entertainment Baytown", "hooky-entertainment-baytown", "Texas", "Baytown")
  ))
  private def p_beacon: R = ("beacon", "beacon-new-york", "Beacon", "New York", 41.5048, -73.9696, "America/New_York", true, Seq("Beacon", "Monroe", "Lake Mohegan", "Chester"), Seq(
    ("Flagship Cinemas Chester", "Flagship Cinemas Chester", "flagship-cinemas-chester", "New York", "Chester"),
    ("Flagship Cinemas Monroe", "Flagship Cinemas Monroe", "flagship-cinemas-monroe", "New York", "Monroe"),
    ("The Beacon", "The Beacon", "the-beacon", "New York", "Beacon"),
    ("Regal Cortlandt Mohegan Lake", "Regal Cortlandt Mohegan Lake", "regal-cortlandt-mohegan-lake", "New York", "Mohegan Lake")
  ))
  private def p_beatrice: R = ("beatrice", "beatrice-nebraska", "Beatrice", "Nebraska", 40.2681, -96.747, "America/Chicago", false, Seq("Beatrice"), Seq(
    ("Beatrice Movies", "Beatrice Movies", "beatrice-movies", "Nebraska", "Beatrice")
  ))
  private def p_beavercreek: R = ("beavercreek", "beavercreek-ohio", "Beavercreek", "Ohio", 39.7092, -84.0633, "America/New_York", true, Seq("Beavercreek", "Huber Heights", "West Carrollton City", "Yellow Springs"), Seq(
    ("Cinemark Dayton South 16", "Cinemark Dayton South 16", "cinemark-dayton-south-16", "Ohio", "West Carrollton City"),
    ("Cinemark Huber Heights 16", "Cinemark Huber Heights 16", "cinemark-huber-heights-16", "Ohio", "Huber Heights"),
    ("Cinemark The Greene Beavercreek", "Cinemark The Greene Beavercreek", "cinemark-the-greene-beavercreek", "Ohio", "Beavercreek"),
    ("Regal Fairfield Commons Beavercreek", "Regal Fairfield Commons Beavercreek", "regal-fairfield-commons-beavercreek", "Ohio", "Beavercreek"),
    ("Little Art Theatre Yellow Springs", "Little Art Theatre Yellow Springs", "little-art-theatre-yellow-springs", "Ohio", "Yellow Springs")
  ))
  private def p_beaverton: R = ("beaverton", "beaverton-oregon", "Beaverton", "Oregon", 45.4871, -122.8037, "America/Los_Angeles", false, Seq("Beaverton"), Seq(
    ("AMC Progress Ridge 13", "AMC Progress Ridge 13", "amc-progress-ridge-13", "Oregon", "Beaverton"),
    ("Beaverton Wunderland", "Beaverton Wunderland", "beaverton-wunderland", "Oregon", "Beaverton"),
    ("Cinemark Cedar Hills 16", "Cinemark Cedar Hills 16", "cinemark-cedar-hills-16", "Oregon", "Beaverton")
  ))
  private def p_beckley: R = ("beckley", "beckley-west-virginia", "Beckley", "West Virginia", 37.7782, -81.1882, "America/New_York", true, Seq("Beckley", "Oak Hill", "Hinton", "Athens", "Meadow Bridge"), Seq(
    ("Marquee Cinemas Galleria 14", "Marquee Cinemas Galleria 14", "marquee-cinemas-galleria-14", "West Virginia", "Beckley"),
    ("Meadow Bridge Drive-In", "Meadow Bridge Drive-In", "meadow-bridge-drive-in", "West Virginia", "Meadow Bridge"),
    ("The Ritz Theatre Hinton", "The Ritz Theatre Hinton", "the-ritz-theatre-hinton", "West Virginia", "Hinton"),
    ("Pipestem Drive-In Theatre", "Pipestem Drive-In Theatre", "pipestem-drive-in-theatre", "West Virginia", "Athens"),
    ("Starlite Drive-In Oak Hill", "Starlite Drive-In Oak Hill", "starlite-drive-in-oak-hill", "West Virginia", "Oak Hill")
  ))
  private def p_bedford: R = ("bedford", "bedford-pennsylvania", "Bedford", "Pennsylvania", 40.0187, -78.5039, "America/New_York", false, Seq("Bedford"), Seq(
    ("Pitt 2 Theatre Bedford 2", "Pitt 2 Theatre Bedford 2", "pitt-2-theatre-bedford-2", "Pennsylvania", "Bedford"),
    ("Silver Lining Drive-In", "Silver Lining Drive-In", "silver-lining-drive-in", "Pennsylvania", "Bedford")
  ))
  private def p_bedford_indiana: R = ("bedford-indiana", "bedford-indiana", "Bedford", "Indiana", 38.8612, -86.4872, "America/Indiana/Indianapolis", true, Seq("Bedford", "Mitchell"), Seq(
    ("UEC Theatres Bedford", "UEC Theatres Bedford", "uec-theatres-bedford", "Indiana", "Bedford"),
    ("Holiday Drive-in Mitchell", "Holiday Drive-in Mitchell", "holiday-drive-in-mitchell", "Indiana", "Mitchell")
  ))
  private def p_beeville: R = ("beeville", "beeville-texas", "Beeville", "Texas", 28.401, -97.7497, "America/Chicago", false, Seq("Beeville"), Seq(
    ("Rio Beeville 6", "Rio Beeville 6", "rio-beeville-6", "Texas", "Beeville")
  ))
  private def p_belfield: R = ("belfield", "belfield-north-dakota", "Belfield", "North Dakota", 46.8853, -103.1996, "America/Denver", false, Seq("Belfield"), Seq(
    ("Belfield Theater and Performance Center", "Belfield Theater and Performance Center", "belfield-theater-and-performance-center", "North Dakota", "Belfield")
  ))
  private def p_belle_plaine: R = ("belle-plaine", "belle-plaine-iowa", "Belle Plaine", "Iowa", 41.8969, -92.2782, "America/Chicago", false, Seq("Belle Plaine"), Seq(
    ("King Theater Belle Plaine", "King Theater Belle Plaine", "king-theater-belle-plaine", "Iowa", "Belle Plaine")
  ))
  private def p_belleville: R = ("belleville", "belleville-illinois", "Belleville", "Illinois", 38.52, -89.984, "America/Chicago", false, Seq("Belleville"), Seq(
    ("Edge Theatres Belleville", "Edge Theatres Belleville", "edge-theatres-belleville", "Illinois", "Belleville"),
    ("Lincoln Theatre Belleville", "Lincoln Theatre Belleville", "lincoln-theatre-belleville", "Illinois", "Belleville"),
    ("Skyview Drive-in Belleville", "Skyview Drive-in Belleville", "skyview-drive-in-belleville", "Illinois", "Belleville")
  ))
  private def p_bellingham: R = ("bellingham", "bellingham-washington", "Bellingham", "Washington", 48.7595, -122.4882, "America/Los_Angeles", false, Seq("Bellingham"), Seq(
    ("Pickford Film Center Bay Street", "Pickford Film Center Bay Street", "pickford-film-center-bay-street", "Washington", "Bellingham"),
    ("Regal Barkley Village Bellingham", "Regal Barkley Village Bellingham", "regal-barkley-village-bellingham", "Washington", "Bellingham"),
    ("The Pickford on Grand (PFC)", "The Pickford on Grand (PFC)", "the-pickford-on-grand-pfc", "Washington", "Bellingham"),
    ("PFC's Limelight Cinema", "PFC's Limelight Cinema", "pfcs-limelight-cinema", "Washington", "Bellingham")
  ))
  private def p_bellmore: R = ("bellmore", "bellmore-new-york", "Bellmore", "New York", 40.6687, -73.5271, "America/New_York", true, Seq("Bellmore", "Rockville Centre", "Garden City", "Lynbrook", "Seaford"), Seq(
    ("AMC Fantasy 5", "AMC Fantasy 5", "amc-fantasy-5", "New York", "Rockville Centre"),
    ("AMC Roosevelt Field 8", "AMC Roosevelt Field 8", "amc-roosevelt-field-8", "New York", "Garden City"),
    ("Bellmore Movies", "Bellmore Movies", "bellmore-movies", "New York", "Bellmore"),
    ("Bellmore Playhouse", "Bellmore Playhouse", "bellmore-playhouse", "New York", "Bellmore"),
    ("Regal Lynbrook", "Regal Lynbrook", "regal-lynbrook", "New York", "Lynbrook"),
    ("Seaford Cinemas 8", "Seaford Cinemas 8", "seaford-cinemas-8", "New York", "Seaford")
  ))
  private def p_beloit: R = ("beloit", "beloit-kansas", "Beloit", "Kansas", 39.4561, -98.1062, "America/Chicago", false, Seq("Beloit"), Seq(
    ("Solomon Valley Cinema", "Solomon Valley Cinema", "solomon-valley-cinema", "Kansas", "Beloit")
  ))
  private def p_belton: R = ("belton", "belton-texas", "Belton", "Texas", 31.056, -97.4644, "America/Chicago", true, Seq("Belton", "Temple", "Harker Heights", "Bruceville-Eddy"), Seq(
    ("Cinemark Harker Heights", "Cinemark Harker Heights", "cinemark-harker-heights", "Texas", "Harker Heights"),
    ("Cinemark Temple 12", "Cinemark Temple 12", "cinemark-temple-12", "Texas", "Temple"),
    ("Grand Avenue Theaters Belton", "Grand Avenue Theaters Belton", "grand-avenue-theaters-belton", "Texas", "Belton"),
    ("The Beltonian Theatre", "The Beltonian Theatre", "the-beltonian-theatre", "Texas", "Belton"),
    ("C&C Drive-In", "C&C Drive-In", "cc-drive-in", "Texas", "Bruceville-Eddy")
  ))
  private def p_bemidji: R = ("bemidji", "bemidji-minnesota", "Bemidji", "Minnesota", 47.4736, -94.8803, "America/Chicago", false, Seq("Bemidji"), Seq(
    ("Bemidji Theatre", "Bemidji Theatre", "bemidji-theatre", "Minnesota", "Bemidji")
  ))
  private def p_bend: R = ("bend", "bend-oregon", "Bend", "Oregon", 44.0582, -121.3153, "America/Los_Angeles", false, Seq("Bend"), Seq(
    ("Old St. Francis", "Old St. Francis", "old-st-francis", "Oregon", "Bend"),
    ("Regal Old Mill Bend", "Regal Old Mill Bend", "regal-old-mill-bend", "Oregon", "Bend"),
    ("Tin Pan Theater", "Tin Pan Theater", "tin-pan-theater", "Oregon", "Bend")
  ))
  private def p_benkelman: R = ("benkelman", "benkelman-nebraska", "Benkelman", "Nebraska", 40.0492, -101.5329, "America/Denver", false, Seq("Benkelman"), Seq(
    ("Zorn Theater", "Zorn Theater", "zorn-theater", "Nebraska", "Benkelman")
  ))
  private def p_bennington: R = ("bennington", "bennington-vermont", "Bennington", "Vermont", 42.8781, -73.1968, "America/New_York", true, Seq("Bennington", "Hoosick Falls"), Seq(
    ("Bennington Cinemas", "Bennington Cinemas", "bennington-cinemas", "Vermont", "Bennington"),
    ("Hathaway's Twin Drive-In Theatre", "Hathaway's Twin Drive-In Theatre", "hathaways-twin-drive-in-theatre", "New York", "Hoosick Falls")
  ))
  private def p_bensalem: R = ("bensalem", "bensalem-pennsylvania", "Bensalem", "Pennsylvania", 40.1046, -74.9513, "America/New_York", true, Seq("Bensalem", "Horsham", "Moorestown-Lenola", "Burlington", "Jenkintown", "Warminster Heights", "Newtown", "Langhorne"), Seq(
    ("AMC Neshaminy 24", "AMC Neshaminy 24", "amc-neshaminy-24", "Pennsylvania", "Bensalem"),
    ("AMC Woodhaven 10", "AMC Woodhaven 10", "amc-woodhaven-10", "Pennsylvania", "Bensalem"),
    ("Hiway Theater Jenkintown", "Hiway Theater Jenkintown", "hiway-theater-jenkintown", "Pennsylvania", "Jenkintown"),
    ("Penn Cinema Huntingdon Valley", "Penn Cinema Huntingdon Valley", "penn-cinema-huntingdon-valley", "Pennsylvania", "Huntingdon Valley"),
    ("Regal Burlington", "Regal Burlington", "regal-burlington", "New Jersey", "Burlington"),
    ("Regal Moorestown", "Regal Moorestown", "regal-moorestown", "New Jersey", "Moorestown"),
    ("Regal Oxford Valley Langhorne", "Regal Oxford Valley Langhorne", "regal-oxford-valley-langhorne", "Pennsylvania", "Langhorne"),
    ("Regal Warrington Crossing", "Regal Warrington Crossing", "regal-warrington-crossing", "Pennsylvania", "Warrington"),
    ("Newtown Theatre", "Newtown Theatre", "newtown-theatre", "Pennsylvania", "Newtown")
  ))
  private def p_benson: R = ("benson", "benson-minnesota", "Benson", "Minnesota", 45.315, -95.6025, "America/Chicago", false, Seq("Benson"), Seq(
    ("De Marce Benson", "De Marce Benson", "de-marce-benson", "Minnesota", "Benson")
  ))
  private def p_benton: R = ("benton", "benton-arkansas", "Benton", "Arkansas", 34.5645, -92.5868, "America/Chicago", false, Seq("Benton"), Seq(
    ("Cinemark Tinseltown USA Benton", "Cinemark Tinseltown USA Benton", "cinemark-tinseltown-usa-benton", "Arkansas", "Benton")
  ))
  private def p_benton_harbor: R = ("benton-harbor", "benton-harbor-michigan", "Benton Harbor", "Michigan", 42.1167, -86.4542, "America/Detroit", true, Seq("Benton Harbor", "South Haven", "Coloma"), Seq(
    ("Celebration! Benton Harbor", "Celebration! Benton Harbor", "celebration-benton-harbor", "Michigan", "Benton Harbor"),
    ("Loma Theatre Coloma", "Loma Theatre Coloma", "loma-theatre-coloma", "Michigan", "Coloma"),
    ("Michigan Theatre South Haven", "Michigan Theatre South Haven", "michigan-theatre-south-haven", "Michigan", "South Haven")
  ))
  private def p_berryville: R = ("berryville", "berryville-arkansas", "Berryville", "Arkansas", 36.3648, -93.568, "America/Chicago", false, Seq("Berryville"), Seq(
    ("Main Theatre Berryville", "Main Theatre Berryville", "main-theatre-berryville", "Arkansas", "Berryville")
  ))
  private def p_bethany: R = ("bethany", "bethany-missouri", "Bethany", "Missouri", 40.2683, -94.0283, "America/Chicago", false, Seq("Bethany"), Seq(
    ("Bigtime Cinema Bethany", "Bigtime Cinema Bethany", "bigtime-cinema-bethany", "Missouri", "Bethany")
  ))
  private def p_bethel: R = ("bethel", "bethel-alaska", "Bethel", "Alaska", 60.7922, -161.7558, "America/Anchorage", false, Seq("Bethel"), Seq(
    ("Suurvik Cinema", "Suurvik Cinema", "suurvik-cinema", "Alaska", "Bethel")
  ))
  private def p_bethel_maine: R = ("bethel-maine", "bethel-maine", "Bethel", "Maine", 44.4042, -70.7906, "America/New_York", false, Seq("Bethel"), Seq(
    ("Gem Cinema Bethel", "Gem Cinema Bethel", "gem-cinema-bethel", "Maine", "Bethel")
  ))
  private def p_bethesda: R = ("bethesda", "bethesda-maryland", "Bethesda", "Maryland", 38.9807, -77.1003, "America/New_York", false, Seq("Bethesda"), Seq(
    ("AMC Montgomery 16", "AMC Montgomery 16", "amc-montgomery-16", "Maryland", "Bethesda"),
    ("IPIC Pike & Rose North Bethesda", "IPIC Pike & Rose North Bethesda", "ipic-pike-rose-north-bethesda", "Maryland", "Bethesda"),
    ("Landmark Bethesda Row Cinema", "Landmark Bethesda Row Cinema", "landmark-bethesda-row-cinema", "Maryland", "Bethesda")
  ))
  private def p_bethlehem: R = ("bethlehem", "bethlehem-pennsylvania", "Bethlehem", "Pennsylvania", 40.6259, -75.3705, "America/New_York", true, Seq("Bethlehem", "Easton", "Emmaus", "Northampton", "Quakertown", "Fountain Hill", "East Greenville", "Schnecksville", "Wind Gap", "Walnutport"), Seq(
    ("AMC Center Valley 16", "AMC Center Valley 16", "amc-center-valley-16", "Pennsylvania", "Center Valley"),
    ("Beckys Drive-In Walnutport", "Beckys Drive-In Walnutport", "beckys-drive-in-walnutport", "Pennsylvania", "Walnutport"),
    ("Emmaus Theatre", "Emmaus Theatre", "emmaus-theatre", "Pennsylvania", "Emmaus"),
    ("Frank Banko Alehouse Cinemas", "Frank Banko Alehouse Cinemas", "frank-banko-alehouse-cinemas", "Pennsylvania", "Bethlehem"),
    ("Grand Theater East Greenville", "Grand Theater East Greenville", "grand-theater-east-greenville", "Pennsylvania", "East Greenville"),
    ("R/C Richland Crossings Movies 12", "R/C Richland Crossings Movies 12", "rc-richland-crossings-movies-12", "Pennsylvania", "Quakertown"),
    ("Regal Northampton Easton", "Regal Northampton Easton", "regal-northampton-easton", "Pennsylvania", "Easton"),
    ("Roxy Theatre Northampton", "Roxy Theatre Northampton", "roxy-theatre-northampton", "Pennsylvania", "Northampton"),
    ("Shankweiler Drive-in Orefield", "Shankweiler Drive-in Orefield", "shankweiler-drive-in-orefield", "Pennsylvania", "Orefield"),
    ("The Gap Theatre", "The Gap Theatre", "the-gap-theatre", "Pennsylvania", "Wind Gap")
  ))
  private def p_beverly_hills: R = ("beverly-hills", "beverly-hills-california", "Beverly Hills", "California", 34.0736, -118.4004, "America/Los_Angeles", false, Seq("Beverly Hills"), Seq(
    ("Fine Arts Theatre Beverly Hills", "Fine Arts Theatre Beverly Hills", "fine-arts-theatre-beverly-hills", "California", "Beverly Hills"),
    ("Lumiere Music Hall Beverly Hills", "Lumiere Music Hall Beverly Hills", "lumiere-music-hall-beverly-hills", "California", "Beverly Hills"),
    ("Samuel Goldwyn Theater Beverly Hills", "Samuel Goldwyn Theater Beverly Hills", "samuel-goldwyn-theater-beverly-hills", "California", "Beverly Hills")
  ))
  private def p_bicknell: R = ("bicknell", "bicknell-utah", "Bicknell", "Utah", 38.3406, -111.5442, "America/Denver", false, Seq("Bicknell"), Seq(
    ("Bicknell Theater", "Bicknell Theater", "bicknell-theater", "Utah", "Bicknell")
  ))
  private def p_big_rapids: R = ("big-rapids", "big-rapids-michigan", "Big Rapids", "Michigan", 43.6981, -85.4837, "America/Detroit", false, Seq("Big Rapids"), Seq(
    ("Big Rapids Theater", "Big Rapids Theater", "big-rapids-theater", "Michigan", "Big Rapids")
  ))
  private def p_big_sky: R = ("big-sky", "big-sky-montana", "Big Sky", "Montana", 45.2846, -111.3683, "America/Denver", true, Seq("Big Sky", "Ennis"), Seq(
    ("Madison Theatre Ennis", "Madison Theatre Ennis", "madison-theatre-ennis", "Montana", "Ennis"),
    ("The Waypoint", "The Waypoint", "the-waypoint", "Montana", "Big Sky")
  ))
  private def p_big_spring: R = ("big-spring", "big-spring-texas", "Big Spring", "Texas", 32.2504, -101.4787, "America/Chicago", false, Seq("Big Spring"), Seq(
    ("Cinemark Big Spring 4", "Cinemark Big Spring 4", "cinemark-big-spring-4", "Texas", "Big Spring")
  ))
  private def p_big_timber: R = ("big-timber", "big-timber-montana", "Big Timber", "Montana", 45.8349, -109.9555, "America/Denver", false, Seq("Big Timber"), Seq(
    ("Sweet Grass Arts Theater", "Sweet Grass Arts Theater", "sweet-grass-arts-theater", "Montana", "Big Timber")
  ))
  private def p_binghamton: R = ("binghamton", "binghamton-new-york", "Binghamton", "New York", 42.0987, -75.918, "America/New_York", true, Seq("Binghamton", "Vestal", "Owego", "Montrose"), Seq(
    ("AMC Vestal Town Square 9", "AMC Vestal Town Square 9", "amc-vestal-town-square-9", "New York", "Vestal"),
    ("Regal Binghamton", "Regal Binghamton", "regal-binghamton", "New York", "Binghamton"),
    ("Tioga Theatre Owego", "Tioga Theatre Owego", "tioga-theatre-owego", "New York", "Owego"),
    ("Historic Montrose Theater", "Historic Montrose Theater", "historic-montrose-theater", "Pennsylvania", "Montrose")
  ))
  private def p_birmingham_michigan: R = ("birmingham-michigan", "birmingham-michigan", "Birmingham", "Michigan", 42.5467, -83.2113, "America/Detroit", true, Seq("Birmingham", "Farmington Hills", "Rochester Hills", "Auburn Hills", "Farmington"), Seq(
    ("AMC Star Great Lakes 25", "AMC Star Great Lakes 25", "amc-star-great-lakes-25", "Michigan", "Auburn Hills"),
    ("Birmingham 8", "Birmingham 8", "birmingham-8", "Michigan", "Birmingham"),
    ("Emagine Palladium Theatre", "Emagine Palladium Theatre", "emagine-palladium-theatre", "Michigan", "Birmingham"),
    ("Emagine Rochester Hills", "Emagine Rochester Hills", "emagine-rochester-hills", "Michigan", "Rochester Hills"),
    ("Emagine The Riviera", "Emagine The Riviera", "emagine-the-riviera", "Michigan", "Farmington Hills"),
    ("Farmington Civic Theater", "Farmington Civic Theater", "farmington-civic-theater", "Michigan", "Farmington")
  ))
  private def p_bishop: R = ("bishop", "bishop-california", "Bishop", "California", 37.3635, -118.3951, "America/Los_Angeles", false, Seq("Bishop"), Seq(
    ("Bishop Twin Theatre", "Bishop Twin Theatre", "bishop-twin-theatre", "California", "Bishop")
  ))
  private def p_bismarck: R = ("bismarck", "bismarck-north-dakota", "Bismarck", "North Dakota", 46.8083, -100.7837, "America/Chicago", false, Seq("Bismarck"), Seq(
    ("Grand 22 Theatres Bismarck", "Grand 22 Theatres Bismarck", "grand-22-theatres-bismarck", "North Dakota", "Bismarck")
  ))
  private def p_blackduck: R = ("blackduck", "blackduck-minnesota", "Blackduck", "Minnesota", 47.7351, -94.5485, "America/Chicago", false, Seq("Blackduck"), Seq(
    ("Blackduck Movie Theater", "Blackduck Movie Theater", "blackduck-movie-theater", "Minnesota", "Blackduck")
  ))
  private def p_blacksburg: R = ("blacksburg", "blacksburg-virginia", "Blacksburg", "Virginia", 37.2296, -80.4139, "America/New_York", true, Seq("Blacksburg", "Christiansburg", "Salem", "Radford", "Fairlawn"), Seq(
    ("B&B Blacksburg", "B&B Blacksburg", "bb-blacksburg", "Virginia", "Blacksburg"),
    ("Lyric Theatre Blacksburg", "Lyric Theatre Blacksburg", "lyric-theatre-blacksburg", "Virginia", "Blacksburg"),
    ("Radford Theatre", "Radford Theatre", "radford-theatre", "Virginia", "Radford"),
    ("Regal New River Valley Christiansburg", "Regal New River Valley Christiansburg", "regal-new-river-valley-christiansburg", "Virginia", "Christiansburg"),
    ("Starlite Drive-in Christiansburg", "Starlite Drive-in Christiansburg", "starlite-drive-in-christiansburg", "Virginia", "Christiansburg"),
    ("Scarrettes Plaza Cinema", "Scarrettes Plaza Cinema", "scarrettes-plaza-cinema", "Virginia", "Fairlawn"),
    ("AMC CLASSIC Salem Valley 8", "AMC CLASSIC Salem Valley 8", "amc-classic-salem-valley-8", "Virginia", "Salem")
  ))
  private def p_bloomington: R = ("bloomington", "bloomington-indiana", "Bloomington", "Indiana", 39.1653, -86.5264, "America/Indiana/Indianapolis", false, Seq("Bloomington"), Seq(
    ("AMC CLASSIC Bloomington 12", "AMC CLASSIC Bloomington 12", "amc-classic-bloomington-12", "Indiana", "Bloomington"),
    ("Buskirk-Chumley Theater", "Buskirk-Chumley Theater", "buskirk-chumley-theater", "Indiana", "Bloomington"),
    ("Starlite Drive In Bloomington", "Starlite Drive In Bloomington", "starlite-drive-in-bloomington", "Indiana", "Bloomington"),
    ("Cicada Cinema Bloomington", "Cicada Cinema Bloomington", "cicada-cinema-bloomington", "Indiana", "Bloomington"),
    ("Indiana University Cinema", "Indiana University Cinema", "indiana-university-cinema", "Indiana", "Bloomington")
  ))
  private def p_bloomington_illinois: R = ("bloomington-illinois", "bloomington-illinois", "Bloomington", "Illinois", 40.4842, -88.9937, "America/Chicago", true, Seq("Bloomington", "Normal"), Seq(
    ("AMC CLASSIC Normal 14", "AMC CLASSIC Normal 14", "amc-classic-normal-14", "Illinois", "Normal"),
    ("Normal Theater", "Normal Theater", "normal-theater", "Illinois", "Normal"),
    ("Golden Ticket Bloomington Ale House", "Golden Ticket Bloomington Ale House", "golden-ticket-bloomington-ale-house", "Illinois", "Bloomington"),
    ("Marcus Bloomington Cinema", "Marcus Bloomington Cinema", "marcus-bloomington-cinema", "Illinois", "Bloomington")
  ))
  private def p_bloomington_minnesota: R = ("bloomington-minnesota", "bloomington-minnesota", "Bloomington", "Minnesota", 44.8408, -93.2983, "America/Chicago", true, Seq("Bloomington", "Eagan", "Lakeville", "Shakopee", "Rosemount"), Seq(
    ("B&B Bloomington 13 at Mall of America", "B&B Bloomington 13 at Mall of America", "bb-bloomington-13-at-mall-of-america", "Minnesota", "Bloomington"),
    ("Emagine Eagan", "Emagine Eagan", "emagine-eagan", "Minnesota", "Eagan"),
    ("Emagine Lakeville", "Emagine Lakeville", "emagine-lakeville", "Minnesota", "Lakeville"),
    ("Marcus Rosemount Cinema", "Marcus Rosemount Cinema", "marcus-rosemount-cinema", "Minnesota", "Rosemount"),
    ("Marcus Southbridge Crossing Cinema", "Marcus Southbridge Crossing Cinema", "marcus-southbridge-crossing-cinema", "Minnesota", "Shakopee")
  ))
  private def p_bloomsburg: R = ("bloomsburg", "bloomsburg-pennsylvania", "Bloomsburg", "Pennsylvania", 41.0037, -76.4549, "America/New_York", false, Seq("Bloomsburg"), Seq(
    ("AMC CLASSIC Bloomsburg 11", "AMC CLASSIC Bloomsburg 11", "amc-classic-bloomsburg-11", "Pennsylvania", "Bloomsburg")
  ))
  private def p_bluefield: R = ("bluefield", "bluefield-west-virginia", "Bluefield", "West Virginia", 37.2698, -81.2223, "America/New_York", false, Seq("Bluefield"), Seq(
    ("Golden Ticket Cinemas Bluefield 8", "Golden Ticket Cinemas Bluefield 8", "golden-ticket-cinemas-bluefield-8", "West Virginia", "Bluefield"),
    ("Granada Theater", "Granada Theater", "granada-theater", "West Virginia", "Bluefield")
  ))
  private def p_blytheville: R = ("blytheville", "blytheville-arkansas", "Blytheville", "Arkansas", 35.9273, -89.919, "America/Chicago", false, Seq("Blytheville"), Seq(
    ("Ritz Theater Blytheville", "Ritz Theater Blytheville", "ritz-theater-blytheville", "Arkansas", "Blytheville")
  ))
  private def p_boca_raton: R = ("boca-raton", "boca-raton-florida", "Boca Raton", "Florida", 26.3587, -80.0831, "America/New_York", true, Seq("Boca Raton", "Coconut Creek", "Deerfield Beach", "Boynton Beach"), Seq(
    ("Silverspot Coconut Creek", "Silverspot Coconut Creek", "silverspot-coconut-creek", "Florida", "Coconut Creek"),
    ("Wynmoor Theatre Coconut Creek", "Wynmoor Theatre Coconut Creek", "wynmoor-theatre-coconut-creek", "Florida", "Coconut Creek"),
    ("Cinemark Boynton Beach 14", "Cinemark Boynton Beach 14", "cinemark-boynton-beach-14", "Florida", "Boynton Beach"),
    ("Cinemark Palace 20", "Cinemark Palace 20", "cinemark-palace-20", "Florida", "Boca Raton"),
    ("IPIC Cinemas Boca Raton", "IPIC Cinemas Boca Raton", "ipic-cinemas-boca-raton", "Florida", "Boca Raton"),
    ("Paragon Theaters Deerfield", "Paragon Theaters Deerfield", "paragon-theaters-deerfield", "Florida", "Deerfield Beach")
  ))
  private def p_boerne: R = ("boerne", "boerne-texas", "Boerne", "Texas", 29.7947, -98.732, "America/Chicago", false, Seq("Boerne"), Seq(
    ("AMC CLASSIC Boerne 11", "AMC CLASSIC Boerne 11", "amc-classic-boerne-11", "Texas", "Boerne")
  ))
  private def p_bolingbrook: R = ("bolingbrook", "bolingbrook-illinois", "Bolingbrook", "Illinois", 41.6986, -88.0684, "America/Chicago", true, Seq("Bolingbrook", "Woodridge", "Oak Brook", "Hodgkins", "Orland Park", "Wheaton", "Downers Grove", "Glen Ellyn", "Warrenville"), Seq(
    ("AMC Oakbrook Center 12", "AMC Oakbrook Center 12", "amc-oakbrook-center-12", "Illinois", "Oak Brook"),
    ("AMC Oakbrook Center 4", "AMC Oakbrook Center 4", "amc-oakbrook-center-4", "Illinois", "Oak Brook"),
    ("AMC Quarry Cinemas 14", "AMC Quarry Cinemas 14", "amc-quarry-cinemas-14", "Illinois", "Hodgkins"),
    ("Emagine Frankfort", "Emagine Frankfort", "emagine-frankfort", "Illinois", "Frankfort"),
    ("Cinemark Seven Bridges Woodridge", "Cinemark Seven Bridges Woodridge", "cinemark-seven-bridges-woodridge", "Illinois", "Woodridge"),
    ("Hollywood Blvd Cinema Woodridge", "Hollywood Blvd Cinema Woodridge", "hollywood-blvd-cinema-woodridge", "Illinois", "Woodridge"),
    ("Classic Cinemas Tivoli Downers Grove", "Classic Cinemas Tivoli Downers Grove", "classic-cinemas-tivoli-downers-grove", "Illinois", "Downers Grove"),
    ("Glen Art Theatre", "Glen Art Theatre", "glen-art-theatre", "Illinois", "Glen Ellyn"),
    ("Marcus Orland Park Cinema", "Marcus Orland Park Cinema", "marcus-orland-park-cinema", "Illinois", "Orland Park"),
    ("Phoenix Theatres Cantera Warrenville 17", "Phoenix Theatres Cantera Warrenville 17", "phoenix-theatres-cantera-warrenville-17", "Illinois", "Warrenville"),
    ("Regal Bolingbrook", "Regal Bolingbrook", "regal-bolingbrook", "Illinois", "Bolingbrook"),
    ("Star Cinema Grill Bolingbrook", "Star Cinema Grill Bolingbrook", "star-cinema-grill-bolingbrook", "Illinois", "Bolingbrook"),
    ("Studio Movie Grill Wheaton", "Studio Movie Grill Wheaton", "studio-movie-grill-wheaton", "Illinois", "Wheaton")
  ))
  private def p_bolivar: R = ("bolivar", "bolivar-missouri", "Bolivar", "Missouri", 37.6145, -93.4105, "America/Chicago", true, Seq("Bolivar", "Buffalo"), Seq(
    ("B&B Bolivar Cinema 5", "B&B Bolivar Cinema 5", "bb-bolivar-cinema-5", "Missouri", "Bolivar"),
    ("Twin Crescent Drive-in", "Twin Crescent Drive-in", "twin-crescent-drive-in", "Missouri", "Buffalo")
  ))
  private def p_bolivar_tennessee: R = ("bolivar-tennessee", "bolivar-tennessee", "Bolivar", "Tennessee", 35.2562, -88.9878, "America/Chicago", false, Seq("Bolivar"), Seq(
    ("Luez Theater", "Luez Theater", "luez-theater", "Tennessee", "Bolivar")
  ))
  private def p_bonita_springs: R = ("bonita-springs", "bonita-springs-florida", "Bonita Springs", "Florida", 26.3398, -81.7787, "America/New_York", false, Seq("Bonita Springs"), Seq(
    ("Prado Stadium 12", "Prado Stadium 12", "prado-stadium-12", "Florida", "Bonita Springs")
  ))
  private def p_bonners_ferry: R = ("bonners-ferry", "bonners-ferry-idaho", "Bonners Ferry", "Idaho", 48.6913, -116.3163, "America/Los_Angeles", false, Seq("Bonners Ferry"), Seq(
    ("Rex Theater Bonners Ferry", "Rex Theater Bonners Ferry", "rex-theater-bonners-ferry", "Idaho", "Bonners Ferry")
  ))
  private def p_boone: R = ("boone", "boone-north-carolina", "Boone", "North Carolina", 36.2168, -81.6745, "America/New_York", true, Seq("Boone", "Lenoir", "Morganton"), Seq(
    ("Golden Ticket Cinemas Lenoir Twin", "Golden Ticket Cinemas Lenoir Twin", "golden-ticket-cinemas-lenoir-twin", "North Carolina", "Lenoir"),
    ("Marquee Mimosa Morganton 7", "Marquee Mimosa Morganton 7", "marquee-mimosa-morganton-7", "North Carolina", "Morganton"),
    ("Regal Boone", "Regal Boone", "regal-boone", "North Carolina", "Boone")
  ))
  private def p_borger: R = ("borger", "borger-texas", "Borger", "Texas", 35.6678, -101.3974, "America/Chicago", false, Seq("Borger"), Seq(
    ("Morley Theatre Borger", "Morley Theatre Borger", "morley-theatre-borger", "Texas", "Borger")
  ))
  private def p_boscobel: R = ("boscobel", "boscobel-wisconsin", "Boscobel", "Wisconsin", 43.1344, -90.7054, "America/Chicago", false, Seq("Boscobel"), Seq(
    ("Blaine Theatre Boscobel", "Blaine Theatre Boscobel", "blaine-theatre-boscobel", "Wisconsin", "Boscobel")
  ))
  private def p_bossier_city: R = ("bossier-city", "bossier-city-louisiana", "Bossier City", "Louisiana", 32.516, -93.7321, "America/Chicago", false, Seq("Bossier City"), Seq(
    ("Regal Louisiana Boardwalk Bossier City", "Regal Louisiana Boardwalk Bossier City", "regal-louisiana-boardwalk-bossier-city", "Louisiana", "Bossier City")
  ))
  private def p_bottineau: R = ("bottineau", "bottineau-north-dakota", "Bottineau", "North Dakota", 48.8272, -100.4457, "America/Chicago", false, Seq("Bottineau"), Seq(
    ("Botno Theater", "Botno Theater", "botno-theater", "North Dakota", "Bottineau")
  ))
  private def p_bowling_green: R = ("bowling-green", "bowling-green-kentucky", "Bowling Green", "Kentucky", 36.9903, -86.4436, "America/Chicago", false, Seq("Bowling Green"), Seq(
    ("Regal Bowling Green", "Regal Bowling Green", "regal-bowling-green", "Kentucky", "Bowling Green"),
    ("Regal Greenwood Mall", "Regal Greenwood Mall", "regal-greenwood-mall", "Kentucky", "Bowling Green")
  ))
  private def p_bowman: R = ("bowman", "bowman-north-dakota", "Bowman", "North Dakota", 46.1831, -103.3949, "America/Denver", false, Seq("Bowman"), Seq(
    ("Bowman Theater", "Bowman Theater", "bowman-theater", "North Dakota", "Bowman")
  ))
  private def p_bozeman: R = ("bozeman", "bozeman-montana", "Bozeman", "Montana", 45.6797, -111.0386, "America/Denver", false, Seq("Bozeman"), Seq(
    ("Regal Gallatin Valley", "Regal Gallatin Valley", "regal-gallatin-valley", "Montana", "Bozeman")
  ))
  private def p_bradenton: R = ("bradenton", "bradenton-florida", "Bradenton", "Florida", 27.4989, -82.5748, "America/New_York", true, Seq("Bradenton", "The Meadows"), Seq(
    ("AMC Bradenton 20", "AMC Bradenton 20", "amc-bradenton-20", "Florida", "Bradenton"),
    ("Regal Oakmont", "Regal Oakmont", "regal-oakmont-bradenton", "Florida", "Bradenton"),
    ("Lakewood Ranch Cinemas 6", "Lakewood Ranch Cinemas 6", "lakewood-ranch-cinemas-6", "Florida", "Lakewood Ranch")
  ))
  private def p_brady: R = ("brady", "brady-texas", "Brady", "Texas", 31.1352, -99.3351, "America/Chicago", false, Seq("Brady"), Seq(
    ("Palace Theatre Brady", "Palace Theatre Brady", "palace-theatre-brady", "Texas", "Brady")
  ))
  private def p_branson: R = ("branson", "branson-missouri", "Branson", "Missouri", 36.6437, -93.2185, "America/Chicago", false, Seq("Branson"), Seq(
    ("Branson Meadows Cinemas 11", "Branson Meadows Cinemas 11", "branson-meadows-cinemas-11", "Missouri", "Branson"),
    ("Elite Cinema III", "Elite Cinema III", "elite-cinema-iii", "Missouri", "Branson")
  ))
  private def p_brattleboro: R = ("brattleboro", "brattleboro-vermont", "Brattleboro", "Vermont", 42.8509, -72.5579, "America/New_York", true, Seq("Brattleboro", "Shelburne Falls", "Dover"), Seq(
    ("Latchis Theatre Brattleboro", "Latchis Theatre Brattleboro", "latchis-theatre-brattleboro", "Vermont", "Brattleboro"),
    ("Mountain Park Cinema", "Mountain Park Cinema", "mountain-park-cinema", "Vermont", "West Dover"),
    ("Pothole Pictures Shelburne Falls", "Pothole Pictures Shelburne Falls", "pothole-pictures-shelburne-falls", "Massachusetts", "Shelburne Falls")
  ))
  private def p_breckenridge: R = ("breckenridge", "breckenridge-colorado", "Breckenridge", "Colorado", 39.4817, -106.0383, "America/Denver", true, Seq("Breckenridge", "Silverthorne", "Leadville"), Seq(
    ("Skyline 8 Cinemas Dillon", "Skyline 8 Cinemas Dillon", "skyline-8-cinemas-dillon", "Colorado", "Dillon"),
    ("The Eclipse Theater", "The Eclipse Theater", "the-eclipse-theater", "Colorado", "Breckenridge"),
    ("Tabor Opera House", "Tabor Opera House", "tabor-opera-house", "Colorado", "Leadville")
  ))
  private def p_bremerton: R = ("bremerton", "bremerton-washington", "Bremerton", "Washington", 47.5673, -122.6326, "America/Los_Angeles", false, Seq("Bremerton"), Seq(
    ("Rodeo Drive-In Theatre", "Rodeo Drive-In Theatre", "rodeo-drive-in-theatre", "Washington", "Bremerton"),
    ("Roxy Theatre Bremerton", "Roxy Theatre Bremerton", "roxy-theatre-bremerton", "Washington", "Bremerton"),
    ("SEEfilm Cinema", "SEEfilm Cinema", "seefilm-cinema", "Washington", "Bremerton"),
    ("Tracyton Moviehouse Bremerton", "Tracyton Moviehouse Bremerton", "tracyton-moviehouse-bremerton", "Washington", "Bremerton")
  ))
  private def p_brenham: R = ("brenham", "brenham-texas", "Brenham", "Texas", 30.1669, -96.3977, "America/Chicago", false, Seq("Brenham"), Seq(
    ("Westwood 6 Brenham", "Westwood 6 Brenham", "westwood-6-brenham", "Texas", "Brenham")
  ))
  private def p_brentwood: R = ("brentwood", "brentwood-california", "Brentwood", "California", 37.9319, -121.6958, "America/Los_Angeles", true, Seq("Brentwood", "Pittsburg"), Seq(
    ("AMC Brentwood 14", "AMC Brentwood 14", "amc-brentwood-14", "California", "Brentwood"),
    ("The Delta Brentwood", "The Delta Brentwood", "the-delta-brentwood", "California", "Brentwood"),
    ("Maya Pittsburg", "Maya Pittsburg", "maya-pittsburg", "California", "Pittsburg")
  ))
  private def p_bridgeport: R = ("bridgeport", "bridgeport-west-virginia", "Bridgeport", "West Virginia", 39.2865, -80.2562, "America/New_York", true, Seq("Bridgeport", "Buckhannon", "Shinnston"), Seq(
    ("Cinemark Meadowbrook Mall", "Cinemark Meadowbrook Mall", "cinemark-meadowbrook-mall", "West Virginia", "Bridgeport"),
    ("Upshur Cinema 6", "Upshur Cinema 6", "upshur-cinema-6", "West Virginia", "Buckhannon"),
    ("Sunset Drive-in Meadowbrook", "Sunset Drive-in Meadowbrook", "sunset-drive-in-meadowbrook", "West Virginia", "Meadowbrook")
  ))
  private def p_bridgton: R = ("bridgton", "bridgton-maine", "Bridgton", "Maine", 44.0548, -70.7128, "America/New_York", false, Seq("Bridgton"), Seq(
    ("Bridgton Drive-in", "Bridgton Drive-in", "bridgton-drive-in", "Maine", "Bridgton"),
    ("Magic Lantern Bridgton", "Magic Lantern Bridgton", "magic-lantern-bridgton", "Maine", "Bridgton")
  ))
  private def p_brigham_city: R = ("brigham-city", "brigham-city-utah", "Brigham City", "Utah", 41.5102, -112.0155, "America/Denver", true, Seq("Brigham City", "North Ogden", "Perry"), Seq(
    ("Capitol 2 Brigham City", "Capitol 2 Brigham City", "capitol-2-brigham-city", "Utah", "Brigham City"),
    ("Walker Cinemas 8 Perry", "Walker Cinemas 8 Perry", "walker-cinemas-8-perry", "Utah", "Perry"),
    ("Walker Cinemas 6 Ogden", "Walker Cinemas 6 Ogden", "walker-cinemas-6-ogden", "Utah", "North Ogden")
  ))
  private def p_bristol: R = ("bristol", "bristol-tennessee", "Bristol", "Tennessee", 36.5951, -82.1887, "America/New_York", true, Seq("Bristol"), Seq(
    ("Legacy Theaters Bristol", "Legacy Theaters Bristol", "legacy-theaters-bristol", "Virginia", "Bristol"),
    ("Marquee Pinnacle Bristol 12", "Marquee Pinnacle Bristol 12", "marquee-pinnacle-bristol-12", "Tennessee", "Bristol"),
    ("Twin City Drive-in Theatre Bristol", "Twin City Drive-in Theatre Bristol", "twin-city-drive-in-theatre-bristol", "Tennessee", "Bristol")
  ))
  private def p_britton: R = ("britton", "britton-south-dakota", "Britton", "South Dakota", 45.7916, -97.7509, "America/Chicago", false, Seq("Britton"), Seq(
    ("Strand Theatre Britton", "Strand Theatre Britton", "strand-theatre-britton", "South Dakota", "Britton")
  ))
  private def p_broken_bow: R = ("broken-bow", "broken-bow-nebraska", "Broken Bow", "Nebraska", 41.4019, -99.6393, "America/Chicago", false, Seq("Broken Bow"), Seq(
    ("Fox Theater Broken Bow", "Fox Theater Broken Bow", "fox-theater-broken-bow", "Nebraska", "Broken Bow")
  ))
  private def p_brookfield: R = ("brookfield", "brookfield-missouri", "Brookfield", "Missouri", 39.7845, -93.0735, "America/Chicago", false, Seq("Brookfield"), Seq(
    ("Reel Time Cinema Brookfield", "Reel Time Cinema Brookfield", "reel-time-cinema-brookfield", "Missouri", "Brookfield")
  ))
  private def p_brookfield_wisconsin: R = ("brookfield-wisconsin", "brookfield-wisconsin", "Brookfield", "Wisconsin", 43.0606, -88.1065, "America/Chicago", false, Seq("Brookfield"), Seq(
    ("Marcus Majestic Brookfield", "Marcus Majestic Brookfield", "marcus-majestic-brookfield", "Wisconsin", "Brookfield"),
    ("Movie Tavern Brookfield 8", "Movie Tavern Brookfield 8", "movie-tavern-brookfield-8", "Wisconsin", "Brookfield"),
    ("Sharon Lynne Wilson Center For The Arts", "Sharon Lynne Wilson Center For The Arts", "sharon-lynne-wilson-center-for-the-arts", "Wisconsin", "Brookfield")
  ))
  private def p_brookhaven: R = ("brookhaven", "brookhaven-mississippi", "Brookhaven", "Mississippi", 31.5791, -90.4407, "America/Chicago", false, Seq("Brookhaven"), Seq(
    ("Regal Westbrook Brookhaven", "Regal Westbrook Brookhaven", "regal-westbrook-brookhaven", "Mississippi", "Brookhaven")
  ))
  private def p_brookings: R = ("brookings", "brookings-south-dakota", "Brookings", "South Dakota", 44.3114, -96.7984, "America/Chicago", false, Seq("Brookings"), Seq(
    ("Brookings Cinema 8", "Brookings Cinema 8", "brookings-cinema-8", "South Dakota", "Brookings")
  ))
  private def p_brookings_oregon: R = ("brookings-oregon", "brookings-oregon", "Brookings", "Oregon", 42.0526, -124.284, "America/Los_Angeles", false, Seq("Brookings"), Seq(
    ("Redwood Theatre Brookings", "Redwood Theatre Brookings", "redwood-theatre-brookings", "Oregon", "Brookings")
  ))
  private def p_brownsburg: R = ("brownsburg", "brownsburg-indiana", "Brownsburg", "Indiana", 39.8434, -86.3978, "America/Indiana/Indianapolis", true, Seq("Brownsburg", "Avon", "Lebanon", "Danville"), Seq(
    ("GQT Brownsburg 8", "GQT Brownsburg 8", "gqt-brownsburg-8", "Indiana", "Brownsburg"),
    ("GQT Lebanon 7", "GQT Lebanon 7", "gqt-lebanon-7", "Indiana", "Lebanon"),
    ("Regal Shiloh Crossing Avon", "Regal Shiloh Crossing Avon", "regal-shiloh-crossing-avon", "Indiana", "Avon"),
    ("Royal Theater Danville", "Royal Theater Danville", "royal-theater-danville", "Indiana", "Danville")
  ))
  private def p_brunswick: R = ("brunswick", "brunswick-georgia", "Brunswick", "Georgia", 31.1501, -81.4915, "America/New_York", true, Seq("Brunswick", "Saint Simons Island"), Seq(
    ("GTC Glynn Place Cinemas", "GTC Glynn Place Cinemas", "gtc-glynn-place-cinemas", "Georgia", "Brunswick"),
    ("Ritz Theatre Brunswick", "Ritz Theatre Brunswick", "ritz-theatre-brunswick", "Georgia", "Brunswick"),
    ("GTC Island Cinemas", "GTC Island Cinemas", "gtc-island-cinemas", "Georgia", "Saint Simons Island")
  ))
  private def p_bryan: R = ("bryan", "bryan-texas", "Bryan", "Texas", 30.6744, -96.37, "America/Chicago", false, Seq("Bryan"), Seq(
    ("Bryan Premiere", "Bryan Premiere", "bryan-premiere", "Texas", "Bryan"),
    ("Queen Theatre Bryan", "Queen Theatre Bryan", "queen-theatre-bryan", "Texas", "Bryan")
  ))
  private def p_bryant: R = ("bryant", "bryant-south-dakota", "Bryant", "South Dakota", 44.5904, -97.4682, "America/Chicago", false, Seq("Bryant"), Seq(
    ("Bryant Cinema", "Bryant Cinema", "bryant-cinema", "South Dakota", "Bryant")
  ))
  private def p_buchanan: R = ("buchanan", "buchanan-virginia", "Buchanan", "Virginia", 37.5274, -79.6798, "America/New_York", false, Seq("Buchanan"), Seq(
    ("Buchanan Theatre", "Buchanan Theatre", "buchanan-theatre", "Virginia", "Buchanan")
  ))
  private def p_buena_park: R = ("buena-park", "buena-park-california", "Buena Park", "California", 33.8675, -117.9981, "America/Los_Angeles", true, Seq("Buena Park", "Paramount", "Cerritos", "Westminster", "Lakewood", "Tustin", "Yorba Linda", "Pico Rivera", "Montebello", "La Habra", "South Whittier", "La Mirada", "Rowland Heights", "Brea", "Seal Beach"), Seq(
    ("AMC Montebello 10", "AMC Montebello 10", "amc-montebello-10", "California", "Montebello"),
    ("AMC Puente Hills 20", "AMC Puente Hills 20", "amc-puente-hills-20", "California", "Industry"),
    ("Bianchi Theatres", "Bianchi Theatres", "bianchi-theatres", "California", "Paramount"),
    ("Paramount Drive-In Theatres", "Paramount Drive-In Theatres", "paramount-drive-in-theatres", "California", "Paramount"),
    ("Cinepolis Pico Rivera", "Cinepolis Pico Rivera", "cinepolis-pico-rivera", "California", "Pico Rivera"),
    ("Harkins Cerritos 16", "Harkins Cerritos 16", "harkins-cerritos-16", "California", "Cerritos"),
    ("Regal Edwards Cerritos", "Regal Edwards Cerritos", "regal-edwards-cerritos", "California", "Cerritos"),
    ("Starlight Lakewood Center", "Starlight Lakewood Center", "starlight-lakewood-center", "California", "Lakewood"),
    ("Starlight Whittier Village Cinemas", "Starlight Whittier Village Cinemas", "starlight-whittier-village-cinemas", "California", "South Whittier"),
    ("AMC La Mirada 7", "AMC La Mirada 7", "amc-la-mirada-7", "California", "La Mirada"),
    ("AMC Tustin 14 at The District", "AMC Tustin 14 at The District", "amc-tustin-14-at-the-district", "California", "Tustin"),
    ("Bay Theater Seal Beach", "Bay Theater Seal Beach", "bay-theater-seal-beach", "California", "Seal Beach"),
    ("CGV by Regency Buena Park", "CGV by Regency Buena Park", "cgv-by-regency-buena-park", "California", "Buena Park"),
    ("Krikorian Metroplex 18 Buena Park", "Krikorian Metroplex 18 Buena Park", "krikorian-metroplex-18-buena-park", "California", "Buena Park"),
    ("Regal Brea East", "Regal Brea East", "regal-brea-east", "California", "Brea"),
    ("Regal La Habra", "Regal La Habra", "regal-la-habra", "California", "La Habra"),
    ("Regal Yorba Linda", "Regal Yorba Linda", "regal-yorba-linda", "California", "Yorba Linda"),
    ("Regency Westminster 10", "Regency Westminster 10", "regency-westminster-10", "California", "Westminster")
  ))
  private def p_buena_vista: R = ("buena-vista", "buena-vista-colorado", "Buena Vista", "Colorado", 38.8422, -106.1311, "America/Denver", false, Seq("Buena Vista"), Seq(
    ("Comanche Drive-in Buena Vista", "Comanche Drive-in Buena Vista", "comanche-drive-in-buena-vista", "Colorado", "Buena Vista")
  ))
  private def p_buffalo_oklahoma: R = ("buffalo-oklahoma", "buffalo-oklahoma", "Buffalo", "Oklahoma", 36.8356, -99.6304, "America/Chicago", false, Seq("Buffalo"), Seq(
    ("Buffalo Theatre (Buffalo) (Oklahoma)", "Buffalo Theatre (Buffalo) (Oklahoma)", "buffalo-theatre-ok", "Oklahoma", "Buffalo")
  ))
  private def p_buffalo_wyoming: R = ("buffalo-wyoming", "buffalo-wyoming", "Buffalo", "Wyoming", 44.3483, -106.6989, "America/Denver", false, Seq("Buffalo"), Seq(
    ("Buffalo Theatre (Buffalo) (Wyoming)", "Buffalo Theatre (Buffalo) (Wyoming)", "buffalo-theatre-wy", "Wyoming", "Buffalo")
  ))
  private def p_burley: R = ("burley", "burley-idaho", "Burley", "Idaho", 42.5357, -113.7928, "America/Boise", false, Seq("Burley"), Seq(
    ("Century Stadium 5", "Century Stadium 5", "century-stadium-5", "Idaho", "Burley")
  ))
  private def p_burlington: R = ("burlington", "burlington-iowa", "Burlington", "Iowa", 40.8075, -91.1129, "America/Chicago", true, Seq("Burlington", "Fort Madison", "West Burlington"), Seq(
    ("Burlington Capitol Theater", "Burlington Capitol Theater", "burlington-capitol-theater", "Iowa", "Burlington"),
    ("Fox Theatre Fort Madison", "Fox Theatre Fort Madison", "fox-theatre-fort-madison", "Iowa", "Fort Madison"),
    ("Westland 10 Theatre West Burlington", "Westland 10 Theatre West Burlington", "westland-10-theatre-west-burlington", "Iowa", "West Burlington")
  ))
  private def p_burlington_colorado: R = ("burlington-colorado", "burlington-colorado", "Burlington", "Colorado", 39.3061, -102.2694, "America/Denver", false, Seq("Burlington"), Seq(
    ("Midway Theater Burlington", "Midway Theater Burlington", "midway-theater-burlington", "Colorado", "Burlington")
  ))
  private def p_burlington_north_carolina: R = ("burlington-north-carolina", "burlington-north-carolina", "Burlington", "North Carolina", 36.0957, -79.4378, "America/New_York", true, Seq("Burlington", "Graham"), Seq(
    ("Alamance Crossing Staduim 16 Burlington", "Alamance Crossing Staduim 16 Burlington", "alamance-crossing-staduim-16-burlington", "North Carolina", "Burlington"),
    ("Graham Cinema", "Graham Cinema", "graham-cinema", "North Carolina", "Graham")
  ))
  private def p_burlington_vermont: R = ("burlington-vermont", "burlington-vermont", "Burlington", "Vermont", 44.4759, -73.2121, "America/New_York", true, Seq("Burlington", "Colchester", "Essex Junction", "Williston", "Saint Albans"), Seq(
    ("Essex Cinemas", "Essex Cinemas", "essex-cinemas", "Vermont", "Essex Junction"),
    ("Majestic 10", "Majestic 10", "majestic-10", "Vermont", "Williston"),
    ("Partizanfilm Burlington", "Partizanfilm Burlington", "partizanfilm-burlington", "Vermont", "Burlington"),
    ("Sunset Drive-In Colchester", "Sunset Drive-In Colchester", "sunset-drive-in-colchester", "Vermont", "Colchester"),
    ("Welden Theatre St Albans", "Welden Theatre St Albans", "welden-theatre-st-albans", "Vermont", "Saint Albans")
  ))
  private def p_burlington_wisconsin: R = ("burlington-wisconsin", "burlington-wisconsin", "Burlington", "Wisconsin", 42.6781, -88.2762, "America/Chicago", true, Seq("Burlington", "Lake Geneva"), Seq(
    ("Emagine Lake Geneva", "Emagine Lake Geneva", "emagine-lake-geneva", "Wisconsin", "Lake Geneva"),
    ("Plaza Theater Burlington", "Plaza Theater Burlington", "plaza-theater-burlington", "Wisconsin", "Burlington")
  ))
  private def p_burns: R = ("burns", "burns-oregon", "Burns", "Oregon", 43.5863, -119.0541, "America/Los_Angeles", false, Seq("Burns"), Seq(
    ("Desert Historic Theatre", "Desert Historic Theatre", "desert-historic-theatre", "Oregon", "Burns")
  ))
  private def p_burwell: R = ("burwell", "burwell-nebraska", "Burwell", "Nebraska", 41.7817, -99.1332, "America/Chicago", false, Seq("Burwell"), Seq(
    ("Rodeo Theatre Burwell", "Rodeo Theatre Burwell", "rodeo-theatre-burwell", "Nebraska", "Burwell")
  ))
  private def p_butler: R = ("butler", "butler-pennsylvania", "Butler", "Pennsylvania", 40.8612, -79.8953, "America/New_York", false, Seq("Butler"), Seq(
    ("Tuxedo Moraine Pointe (formerly MovieScoop)", "Tuxedo Moraine Pointe (formerly MovieScoop)", "moviescoop-moraine-pointe", "Pennsylvania", "Butler"),
    ("Starlight Drive-In Butler", "Starlight Drive-In Butler", "starlight-drive-in-butler", "Pennsylvania", "Butler")
  ))
  private def p_cabot: R = ("cabot", "cabot-arkansas", "Cabot", "Arkansas", 34.9745, -92.0165, "America/Chicago", false, Seq("Cabot"), Seq(
    ("Cabot 8 VIP Cinema", "Cabot 8 VIP Cinema", "cabot-8-vip-cinema", "Arkansas", "Cabot")
  ))
  private def p_cadillac: R = ("cadillac", "cadillac-michigan", "Cadillac", "Michigan", 44.252, -85.4012, "America/Detroit", false, Seq("Cadillac"), Seq(
    ("GQT Cadillac 4", "GQT Cadillac 4", "gqt-cadillac-4", "Michigan", "Cadillac")
  ))
  private def p_calabasas: R = ("calabasas", "calabasas-california", "Calabasas", "California", 34.1578, -118.6384, "America/Los_Angeles", true, Seq("Calabasas", "Westlake Village"), Seq(
    ("Cinepolis Westlake", "Cinepolis Westlake", "cinepolis-westlake", "California", "Westlake Village"),
    ("Regency Calabasas Commons", "Regency Calabasas Commons", "regency-calabasas-commons", "California", "Calabasas")
  ))
  private def p_calais: R = ("calais", "calais-maine", "Calais", "Maine", 45.1838, -67.2766, "America/New_York", false, Seq("Calais"), Seq(
    ("State Cinema Calais", "State Cinema Calais", "state-cinema-calais", "Maine", "Calais")
  ))
  private def p_caldwell: R = ("caldwell", "caldwell-idaho", "Caldwell", "Idaho", 43.6629, -116.6874, "America/Boise", false, Seq("Caldwell"), Seq(
    ("Caldwell Luxe Reel Theatre", "Caldwell Luxe Reel Theatre", "caldwell-luxe-reel-theatre", "Idaho", "Caldwell"),
    ("Terrace Drive-In Theatre", "Terrace Drive-In Theatre", "terrace-drive-in-theatre", "Idaho", "Caldwell")
  ))
  private def p_california: R = ("california", "california-maryland", "California", "Maryland", 38.3004, -76.5075, "America/New_York", true, Seq("California", "Prince Frederick"), Seq(
    ("Flagship Cinemas Calvert Village", "Flagship Cinemas Calvert Village", "flagship-cinemas-calvert-village", "Maryland", "Prince Frederick"),
    ("R/C Lexington Exchange 12", "R/C Lexington Exchange 12", "rc-lexington-exchange-12", "Maryland", "California")
  ))
  private def p_calvert_city: R = ("calvert-city", "calvert-city-kentucky", "Calvert City", "Kentucky", 37.0334, -88.35, "America/Chicago", false, Seq("Calvert City"), Seq(
    ("Calvert Drive In", "Calvert Drive In", "calvert-drive-in", "Kentucky", "Calvert City")
  ))
  private def p_camarillo: R = ("camarillo", "camarillo-california", "Camarillo", "California", 34.2164, -119.0376, "America/Los_Angeles", true, Seq("Camarillo", "Santa Paula"), Seq(
    ("Regal Edwards Camarillo", "Regal Edwards Camarillo", "regal-edwards-camarillo", "California", "Camarillo"),
    ("Roxy Stadium Camarillo 11", "Roxy Stadium Camarillo 11", "roxy-stadium-camarillo-11", "California", "Camarillo"),
    ("Regency Santa Paula 7", "Regency Santa Paula 7", "regency-santa-paula-7", "California", "Santa Paula")
  ))
  private def p_camas: R = ("camas", "camas-washington", "Camas", "Washington", 45.5871, -122.3995, "America/Los_Angeles", true, Seq("Camas", "Battle Ground", "Troutdale", "Sandy"), Seq(
    ("Battle Ground Cinema 8", "Battle Ground Cinema 8", "battle-ground-cinema-8", "Washington", "Battle Ground"),
    ("Liberty Theatre Camas", "Liberty Theatre Camas", "liberty-theatre-camas", "Washington", "Camas"),
    ("Power Station", "Power Station", "power-station", "Oregon", "Troutdale"),
    ("Sandy Cinema 8", "Sandy Cinema 8", "sandy-cinema-8", "Oregon", "Sandy")
  ))
  private def p_cambridge_ohio: R = ("cambridge-ohio", "cambridge-ohio", "Cambridge", "Ohio", 40.0312, -81.5885, "America/New_York", false, Seq("Cambridge"), Seq(
    ("UEC Theatres Cambridge", "UEC Theatres Cambridge", "uec-theatres-cambridge", "Ohio", "Cambridge")
  ))
  private def p_camden: R = ("camden", "camden-south-carolina", "Camden", "South Carolina", 34.2465, -80.607, "America/New_York", false, Seq("Camden"), Seq(
    ("Little Theatre Camden", "Little Theatre Camden", "little-theatre-camden", "South Carolina", "Camden")
  ))
  private def p_cameron: R = ("cameron", "cameron-missouri", "Cameron", "Missouri", 39.7403, -94.2411, "America/Chicago", false, Seq("Cameron"), Seq(
    ("Tyrrell Regional Ymca", "Tyrrell Regional Ymca", "tyrrell-regional-ymca", "Missouri", "Cameron")
  ))
  private def p_campbellsville: R = ("campbellsville", "campbellsville-kentucky", "Campbellsville", "Kentucky", 37.3434, -85.3419, "America/Chicago", true, Seq("Campbellsville", "Russell Springs", "Liberty", "Greensburg"), Seq(
    ("Green River Cinema Campbellsville", "Green River Cinema Campbellsville", "green-river-cinema-campbellsville", "Kentucky", "Campbellsville"),
    ("Key Twin Russell Springs", "Key Twin Russell Springs", "key-twin-russell-springs", "Kentucky", "Russell Springs"),
    ("Lights Of Liberty", "Lights Of Liberty", "lights-of-liberty", "Kentucky", "Liberty"),
    ("Skyline Drive-in Greensburg", "Skyline Drive-in Greensburg", "skyline-drive-in-greensburg", "Kentucky", "Greensburg")
  ))
  private def p_canadian: R = ("canadian", "canadian-texas", "Canadian", "Texas", 35.9128, -100.3821, "America/Chicago", false, Seq("Canadian"), Seq(
    ("Palace Theatre Canadian", "Palace Theatre Canadian", "palace-theatre-canadian", "Texas", "Canadian")
  ))
  private def p_canby: R = ("canby", "canby-minnesota", "Canby", "Minnesota", 44.7088, -96.2764, "America/Chicago", true, Seq("Canby", "Madison", "Hendricks"), Seq(
    ("Canby Theater", "Canby Theater", "canby-theater", "Minnesota", "Canby"),
    ("Grand Theater Madison", "Grand Theater Madison", "grand-theater-madison", "Minnesota", "Madison"),
    ("Red Barn Theatre Hendricks", "Red Barn Theatre Hendricks", "red-barn-theatre-hendricks", "Minnesota", "Hendricks")
  ))
  private def p_cando: R = ("cando", "cando-north-dakota", "Cando", "North Dakota", 48.4867, -99.2099, "America/Chicago", false, Seq("Cando"), Seq(
    ("Audi Theatre Cando", "Audi Theatre Cando", "audi-theatre-cando", "North Dakota", "Cando")
  ))
  private def p_canon_city: R = ("canon-city", "canon-city-colorado", "Cañon City", "Colorado", 38.441, -105.2425, "America/Denver", false, Seq("Cañon City"), Seq(
    ("Cinema Lab The Cañon", "Cinema Lab The Cañon", "cinema-lab-the-caon", "Colorado", "Cañon City"),
    ("Skyline Theatre Canon City", "Skyline Theatre Canon City", "skyline-theatre-canon-city", "Colorado", "Cañon City")
  ))
  private def p_canton: R = ("canton", "canton-ohio", "Canton", "Ohio", 40.7989, -81.3785, "America/New_York", true, Seq("Canton", "Massillon", "North Canton"), Seq(
    ("Cinemark Bistro North Canton", "Cinemark Bistro North Canton", "cinemark-bistro-north-canton", "Ohio", "North Canton"),
    ("Cinemark Tinseltown North Canton", "Cinemark Tinseltown North Canton", "cinemark-tinseltown-north-canton", "Ohio", "North Canton"),
    ("Regal Massillon Great Escape", "Regal Massillon Great Escape", "regal-massillon-great-escape", "Ohio", "Massillon"),
    ("Lions Lincoln Theatre", "Lions Lincoln Theatre", "lions-lincoln-theatre", "Ohio", "Malvern"),
    ("Canton Palace", "Canton Palace", "canton-palace", "Ohio", "Canton"),
    ("The CUT Cinema", "The CUT Cinema", "the-cut-cinema", "Ohio", "Canton")
  ))
  private def p_canton_illinois: R = ("canton-illinois", "canton-illinois", "Canton", "Illinois", 40.5581, -90.0351, "America/Chicago", true, Seq("Canton", "Elmwood"), Seq(
    ("Majestic Theatre of Canton", "Majestic Theatre of Canton", "majestic-theatre-of-canton", "Illinois", "Canton"),
    ("The Palace Theater Elmwood", "The Palace Theater Elmwood", "the-palace-theater-elmwood", "Illinois", "Elmwood")
  ))
  private def p_cape_girardeau: R = ("cape-girardeau", "cape-girardeau-missouri", "Cape Girardeau", "Missouri", 37.3059, -89.5182, "America/Chicago", true, Seq("Cape Girardeau", "Chaffee"), Seq(
    ("Marcus Cape West 14", "Marcus Cape West 14", "marcus-cape-west-14", "Missouri", "Cape Girardeau"),
    ("Rock N Roll Drive-In", "Rock N Roll Drive-In", "rock-n-roll-drive-in", "Missouri", "Chaffee")
  ))
  private def p_carbondale: R = ("carbondale", "carbondale-illinois", "Carbondale", "Illinois", 37.7273, -89.2168, "America/Chicago", false, Seq("Carbondale"), Seq(
    ("AMC University Place 8", "AMC University Place 8", "amc-university-place-8", "Illinois", "Carbondale"),
    ("VIP Carbondale 8", "VIP Carbondale 8", "vip-carbondale-8", "Illinois", "Carbondale"),
    ("The Varsity", "The Varsity", "the-varsity", "Illinois", "Carbondale")
  ))
  private def p_carbondale_colorado: R = ("carbondale-colorado", "carbondale-colorado", "Carbondale", "Colorado", 39.4022, -107.2112, "America/Denver", false, Seq("Carbondale"), Seq(
    ("Crystal Theatre Carbondale", "Crystal Theatre Carbondale", "crystal-theatre-carbondale", "Colorado", "Carbondale"),
    ("BTM Movieland Basalt Carbondale", "BTM Movieland Basalt Carbondale", "btm-movieland-basalt-carbondale", "Colorado", "Carbondale")
  ))
  private def p_carlisle: R = ("carlisle", "carlisle-pennsylvania", "Carlisle", "Pennsylvania", 40.2015, -77.1889, "America/New_York", true, Seq("Carlisle", "Dillsburg", "Newville"), Seq(
    ("R/C Carlisle Commons 8", "R/C Carlisle Commons 8", "rc-carlisle-commons-8", "Pennsylvania", "Carlisle"),
    ("Carlisle Theatre", "Carlisle Theatre", "carlisle-theatre", "Pennsylvania", "Carlisle"),
    ("Cumberland Drive-in Newville", "Cumberland Drive-in Newville", "cumberland-drive-in-newville", "Pennsylvania", "Newville"),
    ("Haars Drive-in Dillsburg", "Haars Drive-in Dillsburg", "haars-drive-in-dillsburg", "Pennsylvania", "Dillsburg")
  ))
  private def p_carlsbad_new_mexico: R = ("carlsbad-new-mexico", "carlsbad-new-mexico", "Carlsbad", "New Mexico", 32.4207, -104.2288, "America/Denver", false, Seq("Carlsbad"), Seq(
    ("Allen La Cueva 6", "Allen La Cueva 6", "allen-la-cueva-6", "New Mexico", "Carlsbad")
  ))
  private def p_carmel: R = ("carmel", "carmel-indiana", "Carmel", "Indiana", 39.9784, -86.118, "America/Indiana/Indianapolis", true, Seq("Carmel", "Noblesville"), Seq(
    ("Emagine Noblesville", "Emagine Noblesville", "emagine-noblesville", "Indiana", "Noblesville"),
    ("Regal Noblesville", "Regal Noblesville", "regal-noblesville", "Indiana", "Noblesville"),
    ("Flix Brewhouse Carmel", "Flix Brewhouse Carmel", "flix-brewhouse-carmel", "Indiana", "Carmel"),
    ("Regal Village Park Carmel", "Regal Village Park Carmel", "regal-village-park-carmel", "Indiana", "Carmel")
  ))
  private def p_carnegie: R = ("carnegie", "carnegie-oklahoma", "Carnegie", "Oklahoma", 35.1037, -98.6037, "America/Chicago", false, Seq("Carnegie"), Seq(
    ("Liberty Theatre Carnegie", "Liberty Theatre Carnegie", "liberty-theatre-carnegie", "Oklahoma", "Carnegie")
  ))
  private def p_carrington: R = ("carrington", "carrington-north-dakota", "Carrington", "North Dakota", 47.4497, -99.1262, "America/Chicago", true, Seq("Carrington", "New Rockford"), Seq(
    ("Carrington Youth Center", "Carrington Youth Center", "carrington-youth-center", "North Dakota", "Carrington"),
    ("Rockford Theater", "Rockford Theater", "rockford-theater", "North Dakota", "New Rockford")
  ))
  private def p_carroll: R = ("carroll", "carroll-iowa", "Carroll", "Iowa", 42.0658, -94.8669, "America/Chicago", true, Seq("Carroll", "Audubon", "Lake City"), Seq(
    ("Fridley Carroll 5", "Fridley Carroll 5", "fridley-carroll-5", "Iowa", "Carroll"),
    ("Lake City Capri Theatre", "Lake City Capri Theatre", "lake-city-capri-theatre", "Iowa", "Lake City"),
    ("Rose Theater Audubon", "Rose Theater Audubon", "rose-theater-audubon", "Iowa", "Audubon")
  ))
  private def p_carrollton: R = ("carrollton", "carrollton-georgia", "Carrollton", "Georgia", 33.5801, -85.0766, "America/New_York", false, Seq("Carrollton"), Seq(
    ("Regal Carrollton", "Regal Carrollton", "regal-carrollton", "Georgia", "Carrollton")
  ))
  private def p_carson_city: R = ("carson-city", "carson-city-nevada", "Carson City", "Nevada", 39.1638, -119.7674, "America/Los_Angeles", true, Seq("Carson City", "Incline Village", "Minden"), Seq(
    ("Carson Valley Cinemas", "Carson Valley Cinemas", "carson-valley-cinemas", "Nevada", "Minden"),
    ("Galaxy Theatres Carson City", "Galaxy Theatres Carson City", "galaxy-theatres-carson-city", "Nevada", "Carson City"),
    ("Incline Village Cinema", "Incline Village Cinema", "incline-village-cinema", "Nevada", "Incline Village")
  ))
  private def p_carthage: R = ("carthage", "carthage-missouri", "Carthage", "Missouri", 37.1765, -94.3102, "America/Chicago", true, Seq("Carthage", "Webb City"), Seq(
    ("66 Drive-in Carthage", "66 Drive-in Carthage", "66-drive-in-carthage", "Missouri", "Carthage"),
    ("Route 66 Movie Theatre", "Route 66 Movie Theatre", "route-66-movie-theatre", "Missouri", "Webb City")
  ))
  private def p_casa_grande: R = ("casa-grande", "casa-grande-arizona", "Casa Grande", "Arizona", 32.8795, -111.7574, "America/Phoenix", false, Seq("Casa Grande"), Seq(
    ("Harkins Casa Grande 14", "Harkins Casa Grande 14", "harkins-casa-grande-14", "Arizona", "Casa Grande")
  ))
  private def p_cascade: R = ("cascade", "cascade-idaho", "Cascade", "Idaho", 44.5163, -116.0418, "America/Boise", false, Seq("Cascade"), Seq(
    ("The Roxy Cascade", "The Roxy Cascade", "the-roxy-cascade", "Idaho", "Cascade")
  ))
  private def p_casper: R = ("casper", "casper-wyoming", "Casper", "Wyoming", 42.8666, -106.3131, "America/Denver", false, Seq("Casper"), Seq(
    ("Studio City East Casper", "Studio City East Casper", "studio-city-east-casper", "Wyoming", "Casper"),
    ("Studio City Mesa Casper", "Studio City Mesa Casper", "studio-city-mesa-casper", "Wyoming", "Casper")
  ))
  private def p_castle_rock: R = ("castle-rock", "castle-rock-colorado", "Castle Rock", "Colorado", 39.3722, -104.8561, "America/Denver", true, Seq("Castle Rock", "Parker"), Seq(
    ("AMC Castle Rock 12", "AMC Castle Rock 12", "amc-castle-rock-12", "Colorado", "Castle Rock"),
    ("Emagine Parker", "Emagine Parker", "emagine-parker", "Colorado", "Parker")
  ))
  private def p_cavalier: R = ("cavalier", "cavalier-north-dakota", "Cavalier", "North Dakota", 48.7939, -97.6223, "America/Chicago", false, Seq("Cavalier"), Seq(
    ("Cavalier Cinema", "Cavalier Cinema", "cavalier-cinema", "North Dakota", "Cavalier")
  ))
  private def p_cedar_bluff: R = ("cedar-bluff", "cedar-bluff-virginia", "Cedar Bluff", "Virginia", 37.0876, -81.759, "America/New_York", false, Seq("Cedar Bluff"), Seq(
    ("Broadway Cedar Bluff", "Broadway Cedar Bluff", "broadway-cedar-bluff", "Virginia", "Cedar Bluff")
  ))
  private def p_cedar_city: R = ("cedar-city", "cedar-city-utah", "Cedar City", "Utah", 37.6775, -113.0619, "America/Denver", false, Seq("Cedar City"), Seq(
    ("Megaplex at Cedar City", "Megaplex at Cedar City", "megaplex-at-cedar-city", "Utah", "Cedar City"),
    ("Cedar Fun Center", "Cedar Fun Center", "cedar-fun-center", "Utah", "Cedar City")
  ))
  private def p_cedar_park: R = ("cedar-park", "cedar-park-texas", "Cedar Park", "Texas", 30.5052, -97.8203, "America/Chicago", true, Seq("Cedar Park", "Georgetown", "Bee Cave"), Seq(
    ("AMC Lakeline 9 Cedar Park", "AMC Lakeline 9 Cedar Park", "amc-lakeline-9-cedar-park", "Texas", "Cedar Park"),
    ("Cinemark Cedar Park 12", "Cinemark Cedar Park 12", "cinemark-cedar-park-12", "Texas", "Cedar Park"),
    ("Cinemark Hill Country Galleria", "Cinemark Hill Country Galleria", "cinemark-hill-country-galleria", "Texas", "Bee Cave"),
    ("Film Alley Georgetown", "Film Alley Georgetown", "film-alley-georgetown", "Texas", "Georgetown")
  ))
  private def p_celina: R = ("celina", "celina-ohio", "Celina", "Ohio", 40.5489, -84.5702, "America/New_York", true, Seq("Celina", "New Bremen", "Maria Stein"), Seq(
    ("Lock One New Bremen", "Lock One New Bremen", "lock-one-new-bremen", "Ohio", "New Bremen"),
    ("Starlight Drive-in", "Starlight Drive-in", "starlight-drive-in", "Ohio", "Maria Stein"),
    ("UEC Theatres Celina", "UEC Theatres Celina", "uec-theatres-celina", "Ohio", "Celina")
  ))
  private def p_center: R = ("center", "center-texas", "Center", "Texas", 31.7954, -94.1791, "America/Chicago", false, Seq("Center"), Seq(
    ("Rio Center", "Rio Center", "rio-center", "Texas", "Center")
  ))
  private def p_centerville: R = ("centerville", "centerville-iowa", "Centerville", "Iowa", 40.7342, -92.8741, "America/Chicago", false, Seq("Centerville"), Seq(
    ("Fridley Cinema 4 Centerville", "Fridley Cinema 4 Centerville", "fridley-cinema-4-centerville", "Iowa", "Centerville")
  ))
  private def p_centerville_tennessee: R = ("centerville-tennessee", "centerville-tennessee", "Centerville", "Tennessee", 35.779, -87.467, "America/Chicago", false, Seq("Centerville"), Seq(
    ("Pink Cadillac Drive-in Centerville", "Pink Cadillac Drive-in Centerville", "pink-cadillac-drive-in-centerville", "Tennessee", "Centerville")
  ))
  private def p_central_city: R = ("central-city", "central-city-nebraska", "Central City", "Nebraska", 41.1159, -98.0017, "America/Chicago", false, Seq("Central City"), Seq(
    ("Fox Theater – Central City", "Fox Theater – Central City", "fox-theater---central-city", "Nebraska", "Central City"),
    ("State Theater Foundation", "State Theater Foundation", "state-theater-foundation", "Nebraska", "Central City")
  ))
  private def p_centralia: R = ("centralia", "centralia-washington", "Centralia", "Washington", 46.7162, -122.9543, "America/Los_Angeles", true, Seq("Centralia", "Chehalis"), Seq(
    ("McMenamins Olympic Club", "McMenamins Olympic Club", "mcmenamins-olympic-club", "Washington", "Centralia"),
    ("Midway Cinema 10 Chehalis", "Midway Cinema 10 Chehalis", "midway-cinema-10-chehalis", "Washington", "Chehalis")
  ))
  private def p_centre: R = ("centre", "centre-alabama", "Centre", "Alabama", 34.152, -85.6788, "America/Chicago", false, Seq("Centre"), Seq(
    ("411 Twin Drive In Centre", "411 Twin Drive In Centre", "411-twin-drive-in-centre", "Alabama", "Centre")
  ))
  private def p_centreville: R = ("centreville", "centreville-virginia", "Centreville", "Virginia", 38.8404, -77.4289, "America/New_York", true, Seq("Centreville", "Reston", "Leesburg", "Herndon", "Chantilly", "Gainesville", "Dulles Town Center"), Seq(
    ("Airbus IMAX Theater", "Airbus IMAX Theater", "airbus-imax-theater", "Virginia", "Chantilly"),
    ("AMC Worldgate 9 Herndon", "AMC Worldgate 9 Herndon", "amc-worldgate-9-herndon", "Virginia", "Herndon"),
    ("Cinemark Centreville 12", "Cinemark Centreville 12", "cinemark-centreville-12", "Virginia", "Centreville"),
    ("LOOK Cinemas Reston", "LOOK Cinemas Reston", "look-cinemas-reston", "Virginia", "Reston"),
    ("Phoenix Village 14", "Phoenix Village 14", "cmx-village-14-leesburg", "Virginia", "Leesburg"),
    ("Regal Dulles Town Center", "Regal Dulles Town Center", "regal-dulles-town-center", "Virginia", "Dulles Town Center"),
    ("Regal Virginia Gateway Gainesville", "Regal Virginia Gateway Gainesville", "regal-virginia-gateway-gainesville", "Virginia", "Gainesville")
  ))
  private def p_chadron: R = ("chadron", "chadron-nebraska", "Chadron", "Nebraska", 42.8294, -102.9999, "America/Denver", false, Seq("Chadron"), Seq(
    ("Eagle Theatre Chadron", "Eagle Theatre Chadron", "eagle-theatre-chadron", "Nebraska", "Chadron")
  ))
  private def p_chamberlain: R = ("chamberlain", "chamberlain-south-dakota", "Chamberlain", "South Dakota", 43.8108, -99.3307, "America/Chicago", false, Seq("Chamberlain"), Seq(
    ("State Theatre & Video Chamberlain", "State Theatre & Video Chamberlain", "state-theatre-video-chamberlain", "South Dakota", "Chamberlain")
  ))
  private def p_champaign: R = ("champaign", "champaign-illinois", "Champaign", "Illinois", 40.1164, -88.2434, "America/Chicago", true, Seq("Champaign", "Savoy", "Gibson City"), Seq(
    ("AMC Champaign 13", "AMC Champaign 13", "amc-champaign-13", "Illinois", "Champaign"),
    ("Virginia Theatre Champaign", "Virginia Theatre Champaign", "virginia-theatre-champaign", "Illinois", "Champaign"),
    ("Harvest Moon Drive-In Theatre", "Harvest Moon Drive-In Theatre", "harvest-moon-drive-in-theatre", "Illinois", "Gibson City"),
    ("Phoenix Theatres Savoy 16", "Phoenix Theatres Savoy 16", "phoenix-theatres-savoy-16", "Illinois", "Savoy")
  ))
  private def p_chandler_oklahoma: R = ("chandler-oklahoma", "chandler-oklahoma", "Chandler", "Oklahoma", 35.7017, -96.8809, "America/Chicago", false, Seq("Chandler"), Seq(
    ("H&S Theatre Chandler", "H&S Theatre Chandler", "hs-theatre-chandler", "Oklahoma", "Chandler")
  ))
  private def p_chanute: R = ("chanute", "chanute-kansas", "Chanute", "Kansas", 37.6792, -95.4572, "America/Chicago", true, Seq("Chanute", "Iola"), Seq(
    ("Chanute Roxy Cinema 4", "Chanute Roxy Cinema 4", "chanute-roxy-cinema-4", "Kansas", "Chanute"),
    ("Sterling 6 Cinemas Iola", "Sterling 6 Cinemas Iola", "sterling-6-cinemas-iola", "Kansas", "Iola")
  ))
  private def p_chapel_hill: R = ("chapel-hill", "chapel-hill-north-carolina", "Chapel Hill", "North Carolina", 35.9132, -79.0558, "America/New_York", false, Seq("Chapel Hill"), Seq(
    ("Chelsea Theater", "Chelsea Theater", "chelsea-theater", "North Carolina", "Chapel Hill"),
    ("Lumina Theatre Chapel Hill", "Lumina Theatre Chapel Hill", "lumina-theatre-chapel-hill", "North Carolina", "Chapel Hill"),
    ("Silverspot Chapel Hill", "Silverspot Chapel Hill", "silverspot-chapel-hill", "North Carolina", "Chapel Hill"),
    ("Varsity Chapel Hill", "Varsity Chapel Hill", "varsity-chapel-hill", "North Carolina", "Chapel Hill"),
    ("Lumina Chapel Hill 5", "Lumina Chapel Hill 5", "lumina-chapel-hill-5", "North Carolina", "Chapel Hill")
  ))
  private def p_charles_city: R = ("charles-city", "charles-city-iowa", "Charles City", "Iowa", 43.0664, -92.6724, "America/Chicago", true, Seq("Charles City", "Osage"), Seq(
    ("Watts Theatre Osage", "Watts Theatre Osage", "watts-theatre-osage", "Iowa", "Osage"),
    ("Charles Theatre", "Charles Theatre", "charles-theatre", "Iowa", "Charles City")
  ))
  private def p_charleston: R = ("charleston", "charleston-west-virginia", "Charleston", "West Virginia", 38.3498, -81.6326, "America/New_York", false, Seq("Charleston"), Seq(
    ("Floralee Hark Cohen Cinema", "Floralee Hark Cohen Cinema", "floralee-hark-cohen-cinema", "West Virginia", "Charleston"),
    ("Marquee Southridge Charleston 12", "Marquee Southridge Charleston 12", "marquee-southridge-charleston-12", "West Virginia", "Charleston"),
    ("Park Place Stadium Cinemas", "Park Place Stadium Cinemas", "park-place-stadium-cinemas", "West Virginia", "Charleston")
  ))
  private def p_charlottesville: R = ("charlottesville", "charlottesville-virginia", "Charlottesville", "Virginia", 38.0293, -78.4767, "America/New_York", false, Seq("Charlottesville"), Seq(
    ("Alamo Drafthouse Charlottesville", "Alamo Drafthouse Charlottesville", "alamo-drafthouse-charlottesville", "Virginia", "Charlottesville"),
    ("Regal Stonefield", "Regal Stonefield", "regal-stonefield", "Virginia", "Charlottesville"),
    ("The Paramount Theater Charlottesville", "The Paramount Theater Charlottesville", "the-paramount-theater-charlottesville", "Virginia", "Charlottesville"),
    ("Violet Crown Charlottesville", "Violet Crown Charlottesville", "violet-crown-charlottesville", "Virginia", "Charlottesville")
  ))
  private def p_cheboygan: R = ("cheboygan", "cheboygan-michigan", "Cheboygan", "Michigan", 45.647, -84.4745, "America/Detroit", true, Seq("Cheboygan", "Mackinaw City"), Seq(
    ("Courtyard Cinema 5 Mackinaw City", "Courtyard Cinema 5 Mackinaw City", "courtyard-cinema-5-mackinaw-city", "Michigan", "Mackinaw City"),
    ("Kingston Theatre Cheboygan", "Kingston Theatre Cheboygan", "kingston-theatre-cheboygan", "Michigan", "Cheboygan")
  ))
  private def p_checotah: R = ("checotah", "checotah-oklahoma", "Checotah", "Oklahoma", 35.4701, -95.523, "America/Chicago", false, Seq("Checotah"), Seq(
    ("Gentry Cinema Checotah", "Gentry Cinema Checotah", "gentry-cinema-checotah", "Oklahoma", "Checotah")
  ))
  private def p_chelan: R = ("chelan", "chelan-washington", "Chelan", "Washington", 47.841, -120.0165, "America/Los_Angeles", false, Seq("Chelan"), Seq(
    ("Ruby Theatre Chelan", "Ruby Theatre Chelan", "ruby-theatre-chelan", "Washington", "Chelan")
  ))
  private def p_cherry_hill: R = ("cherry-hill", "cherry-hill-new-jersey", "Cherry Hill", "New Jersey", 39.9348, -75.0307, "America/New_York", true, Seq("Cherry Hill", "Sewell", "Springfield", "Echelon", "Marlton", "Conshohocken", "Ambler", "Plymouth Meeting", "Somerdale", "Glendora", "Flourtown", "Narberth", "Bryn Mawr", "Turnersville"), Seq(
    ("Ambler Theater", "Ambler Theater", "ambler-theater", "Pennsylvania", "Ambler"),
    ("AMC Cherry Hill 24", "AMC Cherry Hill 24", "amc-cherry-hill-24", "New Jersey", "Cherry Hill"),
    ("AMC Deptford 8", "AMC Deptford 8", "amc-deptford-8", "New Jersey", "West Deptford Township"),
    ("AMC Marlton Cinemas 8", "AMC Marlton Cinemas 8", "amc-marlton-cinemas-8", "New Jersey", "Marlton"),
    ("AMC Marple 10", "AMC Marple 10", "amc-marple-10", "Pennsylvania", "Springfield"),
    ("AMC Plymouth Meeting Mall 12", "AMC Plymouth Meeting Mall 12", "amc-plymouth-meeting-mall-12", "Pennsylvania", "Plymouth Meeting"),
    ("AMC Voorhees 16", "AMC Voorhees 16", "amc-voorhees-16", "New Jersey", "Voorhees"),
    ("Bryn Mawr Film Institute", "Bryn Mawr Film Institute", "bryn-mawr-film-institute", "Pennsylvania", "Bryn Mawr"),
    ("Cinemark Somerdale 16", "Cinemark Somerdale 16", "cinemark-somerdale-16", "New Jersey", "Somerdale"),
    ("Movie Tavern Flourtown", "Movie Tavern Flourtown", "movie-tavern-flourtown", "Pennsylvania", "Flourtown"),
    ("Reel Cinemas Narberth Stadium", "Reel Cinemas Narberth Stadium", "reel-cinemas-narberth-stadium", "Pennsylvania", "Narberth"),
    ("Regal Plymouth Meeting", "Regal Plymouth Meeting", "regal-plymouth-meeting", "Pennsylvania", "Conshohocken"),
    ("Regal Washington Township", "Regal Washington Township", "regal-washington-township", "New Jersey", "Sewell"),
    ("Regal Cross Keys Turnersville", "Regal Cross Keys Turnersville", "regal-cross-keys-turnersville", "New Jersey", "Turnersville")
  ))
  private def p_chester: R = ("chester", "chester-virginia", "Chester", "Virginia", 37.3568, -77.4416, "America/New_York", true, Seq("Chester", "Colonial Heights", "Bon Air", "Highland Springs"), Seq(
    ("Cinema Cafe Chester", "Cinema Cafe Chester", "cinema-cafe-chester", "Virginia", "Chester"),
    ("CMX CinéBistro Stony Point", "CMX CinéBistro Stony Point", "cinebistro-6-stony-point-richmond", "Virginia", "Port Richmond"),
    ("Regal Southpark Mall Colonial Heights", "Regal Southpark Mall Colonial Heights", "regal-southpark-mall-colonial-heights", "Virginia", "Colonial Heights"),
    ("Henrico Theatre", "Henrico Theatre", "henrico-theatre", "Virginia", "Henrico")
  ))
  private def p_cheyenne: R = ("cheyenne", "cheyenne-wyoming", "Cheyenne", "Wyoming", 41.14, -104.8203, "America/Denver", false, Seq("Cheyenne"), Seq(
    ("Capitol Cinema 16 Cheyenne", "Capitol Cinema 16 Cheyenne", "capitol-cinema-16-cheyenne", "Wyoming", "Cheyenne")
  ))
  private def p_cheyenne_wells: R = ("cheyenne-wells", "cheyenne-wells-colorado", "Cheyenne Wells", "Colorado", 38.8214, -102.3532, "America/Denver", false, Seq("Cheyenne Wells"), Seq(
    ("Wells Theater", "Wells Theater", "wells-theater", "Colorado", "Cheyenne Wells")
  ))
  private def p_chickasha: R = ("chickasha", "chickasha-oklahoma", "Chickasha", "Oklahoma", 35.0526, -97.9364, "America/Chicago", false, Seq("Chickasha"), Seq(
    ("Chief Drive-in Chickasha", "Chief Drive-in Chickasha", "chief-drive-in-chickasha", "Oklahoma", "Chickasha"),
    ("Heritage Park Chickasha 6", "Heritage Park Chickasha 6", "heritage-park-chickasha-6", "Oklahoma", "Chickasha")
  ))
  private def p_chillicothe: R = ("chillicothe", "chillicothe-ohio", "Chillicothe", "Ohio", 39.3331, -82.9824, "America/New_York", false, Seq("Chillicothe"), Seq(
    ("Chillicothe Cinema", "Chillicothe Cinema", "chillicothe-cinema", "Ohio", "Chillicothe")
  ))
  private def p_chillicothe_missouri: R = ("chillicothe-missouri", "chillicothe-missouri", "Chillicothe", "Missouri", 39.7953, -93.5524, "America/Chicago", false, Seq("Chillicothe"), Seq(
    ("B&B Chillicothe Grand 6", "B&B Chillicothe Grand 6", "bb-chillicothe-grand-6", "Missouri", "Chillicothe")
  ))
  private def p_chilton: R = ("chilton", "chilton-wisconsin", "Chilton", "Wisconsin", 44.0289, -88.1629, "America/Chicago", false, Seq("Chilton"), Seq(
    ("Chilton Cinema", "Chilton Cinema", "chilton-cinema", "Wisconsin", "Chilton"),
    ("Chilton Twilight Drive-In Theater", "Chilton Twilight Drive-In Theater", "chilton-twilight-drive-in-theater", "Wisconsin", "Chilton")
  ))
  private def p_chino: R = ("chino", "chino-california", "Chino", "California", 34.0122, -117.6889, "America/Los_Angeles", true, Seq("Chino", "Chino Hills", "Eastvale", "Glendora", "Azusa", "Covina", "Montclair", "Claremont", "La Verne"), Seq(
    ("AMC DINE-IN Montclair Place 12", "AMC DINE-IN Montclair Place 12", "amc-dine-in-montclair-place-12", "California", "Montclair"),
    ("Harkins Chino Hills 18", "Harkins Chino Hills 18", "harkins-chino-hills-18", "California", "Chino Hills"),
    ("Maya Cinemas Chino", "Maya Cinemas Chino", "maya-cinemas-chino", "California", "Chino"),
    ("Regal Edwards Eastvale Gateway", "Regal Edwards Eastvale Gateway", "regal-edwards-eastvale-gateway", "California", "Eastvale"),
    ("AMC Covina 17", "AMC Covina 17", "amc-covina-17", "California", "Covina"),
    ("AMC Glendora 12", "AMC Glendora 12", "amc-glendora-12", "California", "Glendora"),
    ("Regal La Verne", "Regal La Verne", "regal-la-verne", "California", "La Verne"),
    ("Regency Foothill Cinema 10", "Regency Foothill Cinema 10", "regency-foothill-cinema-10", "California", "Azusa"),
    ("Laemmle Claremont 5", "Laemmle Claremont 5", "laemmle-claremont-5", "California", "Claremont")
  ))
  private def p_chippewa_falls: R = ("chippewa-falls", "chippewa-falls-wisconsin", "Chippewa Falls", "Wisconsin", 44.9369, -91.3929, "America/Chicago", true, Seq("Chippewa Falls", "Stanley", "Gilman"), Seq(
    ("Micon Cinema Chippewa Falls 8", "Micon Cinema Chippewa Falls 8", "micon-cinema-chippewa-falls-8", "Wisconsin", "Chippewa Falls"),
    ("Stanley Theater", "Stanley Theater", "stanley-theater", "Wisconsin", "Stanley"),
    ("Grand Gilman Theater", "Grand Gilman Theater", "grand-gilman-theater", "Wisconsin", "Gilman")
  ))
  private def p_choteau: R = ("choteau", "choteau-montana", "Choteau", "Montana", 47.8124, -112.1836, "America/Denver", false, Seq("Choteau"), Seq(
    ("Roxy Theatre Choteau", "Roxy Theatre Choteau", "roxy-theatre-choteau", "Montana", "Choteau")
  ))
  private def p_cicero: R = ("cicero", "cicero-illinois", "Cicero", "Illinois", 41.8456, -87.7539, "America/Chicago", true, Seq("Cicero", "Elmhurst", "Lombard", "Addison", "Melrose Park", "Bensenville", "La Grange", "Chicago Ridge", "Crestwood", "North Riverside", "Whiting"), Seq(
    ("AMC Chicago Ridge 6", "AMC Chicago Ridge 6", "amc-chicago-ridge-6", "Illinois", "Chicago Ridge"),
    ("AMC Cicero 14", "AMC Cicero 14", "amc-cicero-14", "Illinois", "Cicero"),
    ("AMC Crestwood 18", "AMC Crestwood 18", "amc-crestwood-18", "Illinois", "Crestwood"),
    ("AMC DINE-IN Yorktown 18", "AMC DINE-IN Yorktown 18", "amc-dine-in-yorktown-18", "Illinois", "Lombard"),
    ("Bensenville Theatre", "Bensenville Theatre", "bensenville-theatre", "Illinois", "Bensenville"),
    ("Cinemark Melrose Park", "Cinemark Melrose Park", "cinemark-melrose-park", "Illinois", "Melrose Park"),
    ("Classic Cinemas La Grange", "Classic Cinemas La Grange", "classic-cinemas-la-grange", "Illinois", "La Grange"),
    ("Classic Cinemas North Riverside", "Classic Cinemas North Riverside", "classic-cinemas-north-riverside", "Illinois", "North Riverside"),
    ("Classic Cinemas York Elmhurst", "Classic Cinemas York Elmhurst", "classic-cinemas-york-elmhurst", "Illinois", "Elmhurst"),
    ("Marcus Addison", "Marcus Addison", "marcus-addison", "Illinois", "Addison"),
    ("Hoosier Theatre Whiting", "Hoosier Theatre Whiting", "hoosier-theatre-whiting", "Indiana", "Whiting")
  ))
  private def p_circle: R = ("circle", "circle-montana", "Circle", "Montana", 47.4167, -105.5922, "America/Denver", false, Seq("Circle"), Seq(
    ("Redwater Theatre", "Redwater Theatre", "redwater-theatre", "Montana", "Circle")
  ))
  private def p_clanton: R = ("clanton", "clanton-alabama", "Clanton", "Alabama", 32.8387, -86.6294, "America/Chicago", false, Seq("Clanton"), Seq(
    ("Peach Country Theater", "Peach Country Theater", "peach-country-theater", "Alabama", "Clanton")
  ))
  private def p_clarendon: R = ("clarendon", "clarendon-texas", "Clarendon", "Texas", 34.9378, -100.8882, "America/Chicago", false, Seq("Clarendon"), Seq(
    ("Mulkey Theatre", "Mulkey Theatre", "mulkey-theatre", "Texas", "Clarendon"),
    ("Sandell Drive-in Clarendon", "Sandell Drive-in Clarendon", "sandell-drive-in-clarendon", "Texas", "Clarendon")
  ))
  private def p_clarion: R = ("clarion", "clarion-pennsylvania", "Clarion", "Pennsylvania", 41.2148, -79.3853, "America/New_York", false, Seq("Clarion"), Seq(
    ("Golden Ticket Cinemas Clarion 7", "Golden Ticket Cinemas Clarion 7", "golden-ticket-cinemas-clarion-7", "Pennsylvania", "Clarion")
  ))
  private def p_clarion_iowa: R = ("clarion-iowa", "clarion-iowa", "Clarion", "Iowa", 42.7316, -93.733, "America/Chicago", true, Seq("Clarion", "Belmond"), Seq(
    ("Clarion Theatre", "Clarion Theatre", "clarion-theatre", "Iowa", "Clarion"),
    ("New Lyric Theatre", "New Lyric Theatre", "new-lyric-theatre", "Iowa", "Belmond")
  ))
  private def p_clay_center: R = ("clay-center", "clay-center-kansas", "Clay Center", "Kansas", 39.3769, -97.1247, "America/Chicago", false, Seq("Clay Center"), Seq(
    ("Rex Theater Clay Center", "Rex Theater Clay Center", "rex-theater-clay-center", "Kansas", "Clay Center")
  ))
  private def p_clayton: R = ("clayton", "clayton-new-mexico", "Clayton", "New Mexico", 36.4517, -103.1841, "America/Denver", false, Seq("Clayton"), Seq(
    ("Luna Theater Clayton", "Luna Theater Clayton", "luna-clayton", "New Mexico", "Clayton")
  ))
  private def p_clayton_georgia: R = ("clayton-georgia", "clayton-georgia", "Clayton", "Georgia", 34.8781, -83.401, "America/New_York", true, Seq("Clayton", "East Franklin", "Highlands"), Seq(
    ("Highlands Performing Arts Center", "Highlands Performing Arts Center", "highlands-performing-arts-center", "North Carolina", "Highlands"),
    ("Ruby Cinemas Franklin", "Ruby Cinemas Franklin", "ruby-cinemas-franklin", "North Carolina", "East Franklin"),
    ("Tiger Drive-In Theatre", "Tiger Drive-In Theatre", "tiger-drive-in-theatre", "Georgia", "Tiger")
  ))
  private def p_cle_elum: R = ("cle-elum", "cle-elum-washington", "Cle Elum", "Washington", 47.1954, -120.9393, "America/Los_Angeles", false, Seq("Cle Elum"), Seq(
    ("Roslyn Theatre", "Roslyn Theatre", "roslyn-theatre", "Washington", "Roslyn")
  ))
  private def p_clearfield: R = ("clearfield", "clearfield-pennsylvania", "Clearfield", "Pennsylvania", 41.0273, -78.4392, "America/New_York", false, Seq("Clearfield"), Seq(
    ("Clearfield Super 322 Drive-in", "Clearfield Super 322 Drive-in", "clearfield-super-322-drive-in", "Pennsylvania", "Clearfield"),
    ("Ritz Theatre Clearfield", "Ritz Theatre Clearfield", "ritz-theatre-clearfield", "Pennsylvania", "Clearfield")
  ))
  private def p_clearlake: R = ("clearlake", "clearlake-california", "Clearlake", "California", 38.9582, -122.6264, "America/Los_Angeles", false, Seq("Clearlake"), Seq(
    ("Cinema Clearlake", "Cinema Clearlake", "cinema-clearlake", "California", "Clearlake")
  ))
  private def p_cleveland_heights: R = ("cleveland-heights", "cleveland-heights-ohio", "Cleveland Heights", "Ohio", 41.52, -81.5562, "America/New_York", true, Seq("Cleveland Heights", "Macedonia", "Brooklyn", "Orange", "Valley View"), Seq(
    ("AMC Ridge Park Square 8", "AMC Ridge Park Square 8", "amc-ridge-park-square-8", "Ohio", "Brooklyn"),
    ("B&B Macedonia 12", "B&B Macedonia 12", "bb-macedonia-12", "Ohio", "Macedonia"),
    ("Cedar Lee Cleveland 6", "Cedar Lee Cleveland 6", "cedar-lee-cleveland-6", "Ohio", "Cleveland Heights"),
    ("Cinemark Valley View", "Cinemark Valley View", "cinemark-valley-view", "Ohio", "Valley View"),
    ("Silverspot Orange Village", "Silverspot Orange Village", "silverspot-orange-village", "Ohio", "Orange")
  ))
  private def p_cleveland_tennessee: R = ("cleveland-tennessee", "cleveland-tennessee", "Cleveland", "Tennessee", 35.1595, -84.8766, "America/New_York", false, Seq("Cleveland"), Seq(
    ("AMC CLASSIC Bradley Square 12", "AMC CLASSIC Bradley Square 12", "amc-classic-bradley-square-12", "Tennessee", "Cleveland"),
    ("UEC Theatres Cleveland", "UEC Theatres Cleveland", "uec-theatres-cleveland", "Tennessee", "Cleveland")
  ))
  private def p_clifton: R = ("clifton", "clifton-texas", "Clifton", "Texas", 31.7824, -97.5767, "America/Chicago", false, Seq("Clifton"), Seq(
    ("Cliftex Theatre Clifton", "Cliftex Theatre Clifton", "cliftex-theatre-clifton", "Texas", "Clifton")
  ))
  private def p_clifton_park: R = ("clifton-park", "clifton-park-new-york", "Clifton Park", "New York", 42.8656, -73.7709, "America/New_York", true, Seq("Clifton Park", "Saratoga Springs", "Wilton", "Rensselaer", "Delmar", "Scotia", "Ballston Spa", "Averill Park"), Seq(
    ("AMC Saratoga Springs 11", "AMC Saratoga Springs 11", "amc-saratoga-springs-11", "New York", "Saratoga Springs"),
    ("Jericho Drive-In", "Jericho Drive-In", "jericho-drive-in", "New York", "Glenmont"),
    ("Malta Drive-in", "Malta Drive-in", "malta-drive-in", "New York", "High Pointe of Malta"),
    ("Regal Clifton Park", "Regal Clifton Park", "regal-clifton-park", "New York", "Clifton Park"),
    ("Regal East Greenbush Rensselaer", "Regal East Greenbush Rensselaer", "regal-east-greenbush-rensselaer", "New York", "Rensselaer"),
    ("Scene One Wilton Mall Cinemas", "Scene One Wilton Mall Cinemas", "scene-one-wilton-mall-cinemas", "New York", "Wilton"),
    ("Scotia Cinema", "Scotia Cinema", "scotia-cinema", "New York", "Scotia"),
    ("Hollywood Drive-in Averill Park", "Hollywood Drive-in Averill Park", "hollywood-drive-in-averill-park", "New York", "Averill Park")
  ))
  private def p_clinton: R = ("clinton", "clinton-iowa", "Clinton", "Iowa", 41.8445, -90.1887, "America/Chicago", true, Seq("Clinton", "De Witt", "Savanna"), Seq(
    ("Clinton 8 Theatre", "Clinton 8 Theatre", "clinton-8-theatre", "Iowa", "Clinton"),
    ("DeWitt Operahouse Theatre", "DeWitt Operahouse Theatre", "dewitt-operahouse-theatre", "Iowa", "De Witt"),
    ("Times Theater Savanna", "Times Theater Savanna", "times-theater-savanna", "Illinois", "Savanna")
  ))
  private def p_clinton_arkansas: R = ("clinton-arkansas", "clinton-arkansas", "Clinton", "Arkansas", 35.5915, -92.4604, "America/Chicago", false, Seq("Clinton"), Seq(
    ("Gateway Twin Cinema", "Gateway Twin Cinema", "gateway-twin-cinema", "Arkansas", "Clinton")
  ))
  private def p_clinton_illinois: R = ("clinton-illinois", "clinton-illinois", "Clinton", "Illinois", 40.1536, -88.9645, "America/Chicago", true, Seq("Clinton", "Le Roy"), Seq(
    ("Princess Theatre Le Roy", "Princess Theatre Le Roy", "princess-theatre-le-roy", "Illinois", "Le Roy"),
    ("Clintonia Eagle Theater", "Clintonia Eagle Theater", "clintonia-eagle-theater", "Illinois", "Clinton")
  ))
  private def p_clinton_missouri: R = ("clinton-missouri", "clinton-missouri", "Clinton", "Missouri", 38.3686, -93.7783, "America/Chicago", false, Seq("Clinton"), Seq(
    ("B&B Clinton Missouri 6", "B&B Clinton Missouri 6", "bb-clinton-missouri-6", "Missouri", "Clinton")
  ))
  private def p_clinton_north_carolina: R = ("clinton-north-carolina", "clinton-north-carolina", "Clinton", "North Carolina", 34.998, -78.3233, "America/New_York", false, Seq("Clinton"), Seq(
    ("East Park Clinton 5", "East Park Clinton 5", "east-park-clinton-5", "North Carolina", "Clinton")
  ))
  private def p_clinton_township: R = ("clinton-township", "clinton-township-michigan", "Clinton Township", "Michigan", 42.587, -82.9199, "America/Detroit", true, Seq("Clinton Township", "Troy", "Shelby", "Madison Heights", "Romeo", "Chesterfield"), Seq(
    ("AMC John R 15", "AMC John R 15", "amc-john-r-15", "Michigan", "Madison Heights"),
    ("AMC Star Gratiot 15", "AMC Star Gratiot 15", "amc-star-gratiot-15", "Michigan", "Clinton Township"),
    ("MJR Partridge Creek Cinema", "MJR Partridge Creek Cinema", "mjr-partridge-creek-cinema", "Michigan", "Clinton Township"),
    ("Emagine Macomb", "Emagine Macomb", "emagine-macomb", "Michigan", "Macomb"),
    ("MJR Chesterfield Crossing", "MJR Chesterfield Crossing", "mjr-chesterfield-crossing", "Michigan", "Chesterfield"),
    ("MJR Troy 16 Cinema", "MJR Troy 16 Cinema", "mjr-troy-16-cinema", "Michigan", "Troy"),
    ("Romeo Theatre", "Romeo Theatre", "romeo-theatre", "Michigan", "Washington")
  ))
  private def p_cloquet: R = ("cloquet", "cloquet-minnesota", "Cloquet", "Minnesota", 46.7216, -92.4594, "America/Chicago", false, Seq("Cloquet"), Seq(
    ("Premiere Theatres Cloquet", "Premiere Theatres Cloquet", "premiere-theatres-cloquet", "Minnesota", "Cloquet")
  ))
  private def p_clovis_new_mexico: R = ("clovis-new-mexico", "clovis-new-mexico", "Clovis", "New Mexico", 34.4048, -103.2052, "America/Denver", false, Seq("Clovis"), Seq(
    ("Allen North Plains Clovis 7", "Allen North Plains Clovis 7", "allen-north-plains-clovis-7", "New Mexico", "Clovis")
  ))
  private def p_coalinga: R = ("coalinga", "coalinga-california", "Coalinga", "California", 36.1397, -120.3602, "America/Los_Angeles", false, Seq("Coalinga"), Seq(
    ("Reel Time Coalinga", "Reel Time Coalinga", "reel-time-coalinga", "California", "Coalinga")
  ))
  private def p_cody: R = ("cody", "cody-wyoming", "Cody", "Wyoming", 44.5263, -109.0565, "America/Denver", false, Seq("Cody"), Seq(
    ("Big Horn Cinemas Cody", "Big Horn Cinemas Cody", "big-horn-cinemas-cody", "Wyoming", "Cody")
  ))
  private def p_coeur_d_alene: R = ("coeur-d-alene", "coeur-d-alene-idaho", "Coeur d'Alene", "Idaho", 47.6777, -116.7805, "America/Los_Angeles", true, Seq("Coeur d'Alene", "Hayden"), Seq(
    ("Hayden Cinema", "Hayden Cinema", "hayden-cinema", "Idaho", "Hayden"),
    ("Regal Riverstone", "Regal Riverstone", "regal-riverstone", "Idaho", "Coeur d'Alene")
  ))
  private def p_colby: R = ("colby", "colby-kansas", "Colby", "Kansas", 39.3958, -101.0524, "America/Chicago", false, Seq("Colby"), Seq(
    ("Colby Cinema", "Colby Cinema", "colby-cinema", "Kansas", "Colby")
  ))
  private def p_coldwater: R = ("coldwater", "coldwater-michigan", "Coldwater", "Michigan", 41.9403, -85.0005, "America/Detroit", false, Seq("Coldwater"), Seq(
    ("Capri Drive-in Coldwater", "Capri Drive-in Coldwater", "capri-drive-in-coldwater", "Michigan", "Coldwater"),
    ("NCG Coldwater Cinema", "NCG Coldwater Cinema", "ncg-coldwater-cinema", "Michigan", "Coldwater")
  ))
  private def p_coldwater_kansas: R = ("coldwater-kansas", "coldwater-kansas", "Coldwater", "Kansas", 37.2689, -99.3268, "America/Chicago", false, Seq("Coldwater"), Seq(
    ("Chief Theater Coldwater", "Chief Theater Coldwater", "chief-theater-coldwater", "Kansas", "Coldwater")
  ))
  private def p_colorado_city: R = ("colorado-city", "colorado-city-texas", "Colorado City", "Texas", 32.3882, -100.8646, "America/Chicago", false, Seq("Colorado City"), Seq(
    ("3 M Palace Colorado City", "3 M Palace Colorado City", "3-m-palace-colorado-city", "Texas", "Colorado City")
  ))
  private def p_columbia_maryland: R = ("columbia-maryland", "columbia-maryland", "Columbia", "Maryland", 39.2404, -76.8394, "America/New_York", true, Seq("Columbia", "Glen Burnie", "Towson", "Hanover", "Owings Mills", "Arbutus"), Seq(
    ("AMC Columbia Maryland 14", "AMC Columbia Maryland 14", "amc-columbia-maryland-14", "Maryland", "Columbia"),
    ("AMC Owings Mills 17", "AMC Owings Mills 17", "amc-owings-mills-17", "Maryland", "Owings Mills"),
    ("Cinemark Egyptian 24", "Cinemark Egyptian 24", "cinemark-egyptian-24", "Maryland", "Hanover"),
    ("Cinemark Towson", "Cinemark Towson", "cinemark-towson", "Maryland", "Towson"),
    ("Horizon Cinemas Marley Station", "Horizon Cinemas Marley Station", "horizon-cinemas-marley-station", "Maryland", "Glen Burnie"),
    ("R/C Hollywood Cinema 4 Arbutus", "R/C Hollywood Cinema 4 Arbutus", "rc-hollywood-cinema-4-arbutus", "Maryland", "Arbutus")
  ))
  private def p_columbia_mississippi: R = ("columbia-mississippi", "columbia-mississippi", "Columbia", "Mississippi", 31.2518, -89.8376, "America/Chicago", false, Seq("Columbia"), Seq(
    ("Marion Columbia 2", "Marion Columbia 2", "marion-columbia-2", "Mississippi", "Columbia")
  ))
  private def p_columbus_indiana: R = ("columbus-indiana", "columbus-indiana", "Columbus", "Indiana", 39.2014, -85.9214, "America/Indiana/Indianapolis", false, Seq("Columbus"), Seq(
    ("Yes Cinema Columbus", "Yes Cinema Columbus", "yes-cinema-columbus", "Indiana", "Columbus"),
    ("AMC CLASSIC Columbus 12", "AMC CLASSIC Columbus 12", "amc-classic-columbus-12", "Indiana", "Columbus")
  ))
  private def p_columbus_mississippi: R = ("columbus-mississippi", "columbus-mississippi", "Columbus", "Mississippi", 33.4957, -88.4273, "America/Chicago", false, Seq("Columbus"), Seq(
    ("Malco Columbus 8", "Malco Columbus 8", "malco-columbus-8", "Mississippi", "Columbus")
  ))
  private def p_columbus_nebraska: R = ("columbus-nebraska", "columbus-nebraska", "Columbus", "Nebraska", 41.4297, -97.3684, "America/Chicago", true, Seq("Columbus", "Schuyler"), Seq(
    ("Center 7 Theatre Columbus", "Center 7 Theatre Columbus", "center-7-theatre-columbus", "Nebraska", "Columbus"),
    ("Colfax Theatre Schuyler", "Colfax Theatre Schuyler", "colfax-theatre-schuyler", "Nebraska", "Schuyler")
  ))
  private def p_colville: R = ("colville", "colville-washington", "Colville", "Washington", 48.5466, -117.9055, "America/Los_Angeles", false, Seq("Colville"), Seq(
    ("Alpine Colville", "Alpine Colville", "alpine-colville", "Washington", "Colville"),
    ("Auto Vue Drive-in Colville", "Auto Vue Drive-in Colville", "auto-vue-drive-in-colville", "Washington", "Colville")
  ))
  private def p_commerce_city: R = ("commerce-city", "commerce-city-colorado", "Commerce City", "Colorado", 39.8083, -104.9339, "America/Denver", true, Seq("Commerce City", "Brighton"), Seq(
    ("88 Drive-In Theatre Commerce City", "88 Drive-In Theatre Commerce City", "88-drive-in-theatre-commerce-city", "Colorado", "Commerce City"),
    ("AMC Brighton 12", "AMC Brighton 12", "amc-brighton-12", "Colorado", "Brighton")
  ))
  private def p_concord_new_hampshire: R = ("concord-new-hampshire", "concord-new-hampshire", "Concord", "New Hampshire", 43.2081, -71.5376, "America/New_York", true, Seq("Concord", "Hooksett", "Tilton"), Seq(
    ("Red River Theatres Concord", "Red River Theatres Concord", "red-river-theatres-concord", "New Hampshire", "Concord"),
    ("Smitty's 6 Cinema Pub Tilton 6", "Smitty's 6 Cinema Pub Tilton 6", "smittys-6-cinema-pub-tilton-6", "New Hampshire", "Tilton"),
    ("Apple Cinemas Hooksett", "Apple Cinemas Hooksett", "apple-cinemas-hooksett", "New Hampshire", "Hooksett")
  ))
  private def p_concord_north_carolina: R = ("concord-north-carolina", "concord-north-carolina", "Concord", "North Carolina", 35.4089, -80.5816, "America/New_York", true, Seq("Concord", "Kannapolis", "Salisbury"), Seq(
    ("AMC Concord Mills 24", "AMC Concord Mills 24", "amc-concord-mills-24", "North Carolina", "Concord"),
    ("Carolina Mall Cinema Concord", "Carolina Mall Cinema Concord", "carolina-mall-cinema-concord", "North Carolina", "Concord"),
    ("Cinemark Tinseltown Salisbury", "Cinemark Tinseltown Salisbury", "cinemark-tinseltown-salisbury", "North Carolina", "Salisbury"),
    ("Gem Cinema Kannapolis", "Gem Cinema Kannapolis", "gem-cinema-kannapolis", "North Carolina", "Kannapolis")
  ))
  private def p_concordia: R = ("concordia", "concordia-kansas", "Concordia", "Kansas", 39.5708, -97.6625, "America/Chicago", true, Seq("Concordia", "Belleville"), Seq(
    ("Blair Theater Belleville", "Blair Theater Belleville", "blair-theater-belleville", "Kansas", "Belleville"),
    ("VIP Majestic 4 Concordia", "VIP Majestic 4 Concordia", "vip-majestic-4-concordia", "Kansas", "Concordia")
  ))
  private def p_concrete: R = ("concrete", "concrete-washington", "Concrete", "Washington", 48.5388, -121.7463, "America/Los_Angeles", false, Seq("Concrete"), Seq(
    ("Concrete Theatre", "Concrete Theatre", "concrete-theatre", "Washington", "Concrete")
  ))
  private def p_conrad: R = ("conrad", "conrad-montana", "Conrad", "Montana", 48.1703, -111.9461, "America/Denver", false, Seq("Conrad"), Seq(
    ("Orpheum Theatre Conrad", "Orpheum Theatre Conrad", "orpheum-theatre-conrad", "Montana", "Conrad")
  ))
  private def p_conway: R = ("conway", "conway-arkansas", "Conway", "Arkansas", 35.0887, -92.4421, "America/Chicago", false, Seq("Conway"), Seq(
    ("Cinemark Towne Centre Conway", "Cinemark Towne Centre Conway", "cinemark-towne-centre-conway", "Arkansas", "Conway")
  ))
  private def p_conway_south_carolina: R = ("conway-south-carolina", "conway-south-carolina", "Conway", "South Carolina", 33.836, -79.0478, "America/New_York", false, Seq("Conway"), Seq(
    ("B&B Conway 12", "B&B Conway 12", "bb-conway-12", "South Carolina", "Conway")
  ))
  private def p_cook: R = ("cook", "cook-minnesota", "Cook", "Minnesota", 47.8524, -92.6887, "America/Chicago", false, Seq("Cook"), Seq(
    ("Comet Cook", "Comet Cook", "comet-cook", "Minnesota", "Cook")
  ))
  private def p_cookeville: R = ("cookeville", "cookeville-tennessee", "Cookeville", "Tennessee", 36.1628, -85.5016, "America/Chicago", true, Seq("Cookeville", "Sparta"), Seq(
    ("AMC CLASSIC Highland 12", "AMC CLASSIC Highland 12", "amc-classic-highland-12", "Tennessee", "Cookeville"),
    ("Sparta Drive-in", "Sparta Drive-in", "sparta-drive-in", "Tennessee", "Sparta")
  ))
  private def p_cooperstown: R = ("cooperstown", "cooperstown-north-dakota", "Cooperstown", "North Dakota", 47.4444, -98.124, "America/Chicago", false, Seq("Cooperstown"), Seq(
    ("Cooper Theatre", "Cooper Theatre", "cooper-theatre", "North Dakota", "Cooperstown")
  ))
  private def p_coos_bay: R = ("coos-bay", "coos-bay-oregon", "Coos Bay", "Oregon", 43.3665, -124.2179, "America/Los_Angeles", true, Seq("Coos Bay", "North Bend"), Seq(
    ("Pony Village Cinema North Bend", "Pony Village Cinema North Bend", "pony-village-cinema-north-bend", "Oregon", "North Bend"),
    ("Egyptian Coos Bay", "Egyptian Coos Bay", "egyptian-coos-bay", "Oregon", "Coos Bay")
  ))
  private def p_copperas_cove: R = ("copperas-cove", "copperas-cove-texas", "Copperas Cove", "Texas", 31.1241, -97.9031, "America/Chicago", false, Seq("Copperas Cove"), Seq(
    ("Cinergy Copperas Cove 8", "Cinergy Copperas Cove 8", "cinergy-copperas-cove-8", "Texas", "Copperas Cove"),
    ("The Cove Theater", "The Cove Theater", "the-cove-theater", "Texas", "Copperas Cove")
  ))
  private def p_coral_gables: R = ("coral-gables", "coral-gables-florida", "Coral Gables", "Florida", 25.7215, -80.2684, "America/New_York", false, Seq("Coral Gables"), Seq(
    ("Coral Gables Art Cinema", "Coral Gables Art Cinema", "coral-gables-art-cinema", "Florida", "Coral Gables"),
    ("The Landmark at Merrick Park", "The Landmark at Merrick Park", "the-landmark-at-merrick-park", "Florida", "Coral Gables"),
    ("Bill Cosford Cinema", "Bill Cosford Cinema", "bill-cosford-cinema", "Florida", "Coral Gables")
  ))
  private def p_cordele: R = ("cordele", "cordele-georgia", "Cordele", "Georgia", 31.9635, -83.7824, "America/New_York", false, Seq("Cordele"), Seq(
    ("Spotlight Theatres Cordele Square 5", "Spotlight Theatres Cordele Square 5", "spotlight-theatres-cordele-square-5", "Georgia", "Cordele")
  ))
  private def p_cordell: R = ("cordell", "cordell-oklahoma", "Cordell", "Oklahoma", 35.2906, -98.9884, "America/Chicago", false, Seq("Cordell"), Seq(
    ("Washita Cordell", "Washita Cordell", "washita-cordell", "Oklahoma", "Cordell")
  ))
  private def p_cordova: R = ("cordova", "cordova-tennessee", "Cordova", "Tennessee", 35.1557, -89.7762, "America/Chicago", true, Seq("Cordova", "Bartlett", "Southaven", "Collierville", "Germantown", "Olive Branch", "Atoka"), Seq(
    ("Hollywood Cinema Bartlett", "Hollywood Cinema Bartlett", "hollywood-cinema-bartlett", "Tennessee", "Bartlett"),
    ("Malco Cordova 16", "Malco Cordova 16", "malco-cordova-16", "Tennessee", "Cordova"),
    ("Malco Desoto Southaven 16", "Malco Desoto Southaven 16", "malco-desoto-southaven-16", "Mississippi", "Southaven"),
    ("Malco Forest Hill Germantown 8", "Malco Forest Hill Germantown 8", "malco-forest-hill-germantown-8", "Tennessee", "Germantown"),
    ("Malco Olive Branch Cinema 12", "Malco Olive Branch Cinema 12", "malco-olive-branch-cinema-12", "Mississippi", "Olive Branch"),
    ("Malco Towne Collierville 16", "Malco Towne Collierville 16", "malco-towne-collierville-16", "Tennessee", "Collierville"),
    ("Phoenix Theatres Atoka 16", "Phoenix Theatres Atoka 16", "phoenix-theatres-atoka-16", "Tennessee", "Atoka")
  ))
  private def p_corinth: R = ("corinth", "corinth-mississippi", "Corinth", "Mississippi", 34.9342, -88.5223, "America/Chicago", false, Seq("Corinth"), Seq(
    ("Malco Corinth 10", "Malco Corinth 10", "malco-corinth-10", "Mississippi", "Corinth"),
    ("Coliseum Civic Center", "Coliseum Civic Center", "coliseum-civic-center", "Mississippi", "Corinth")
  ))
  private def p_corning: R = ("corning", "corning-new-york", "Corning", "New York", 42.1429, -77.0547, "America/New_York", true, Seq("Corning", "Horseheads", "Big Flats", "Painted Post", "Watkins Glen"), Seq(
    ("Elmira Drive-In Theatre", "Elmira Drive-In Theatre", "elmira-drive-in-theatre", "New York", "Big Flats"),
    ("Regal Arnot Mall Horseheads", "Regal Arnot Mall Horseheads", "regal-arnot-mall-horseheads", "New York", "Horseheads"),
    ("Spotlight of Corning", "Spotlight of Corning", "spotlight-of-corning", "New York", "Painted Post"),
    ("Corning Palace Theatre", "Corning Palace Theatre", "corning-palace-theatre", "New York", "Corning"),
    ("Glen Theatre", "Glen Theatre", "glen-theatre", "New York", "Watkins Glen")
  ))
  private def p_corning_iowa: R = ("corning-iowa", "corning-iowa", "Corning", "Iowa", 40.99, -94.7408, "America/Chicago", false, Seq("Corning"), Seq(
    ("Corning American Theatre", "Corning American Theatre", "corning-american-theatre", "Iowa", "Corning")
  ))
  private def p_corsicana: R = ("corsicana", "corsicana-texas", "Corsicana", "Texas", 32.0954, -96.4689, "America/Chicago", true, Seq("Corsicana", "Ennis", "Gun Barrel City"), Seq(
    ("Galaxy Drive-In Ennis", "Galaxy Drive-In Ennis", "galaxy-drive-in-ennis", "Texas", "Ennis"),
    ("Hometown Cinemas Gun Barrel City", "Hometown Cinemas Gun Barrel City", "hometown-cinemas-gun-barrel-city", "Texas", "Gun Barrel City"),
    ("Schulman's Movie Bowl Corsicana", "Schulman's Movie Bowl Corsicana", "schulmans-movie-bowl-corsicana", "Texas", "Corsicana")
  ))
  private def p_cortez: R = ("cortez", "cortez-colorado", "Cortez", "Colorado", 37.3489, -108.5859, "America/Denver", false, Seq("Cortez"), Seq(
    ("Allen Fiesta Twin Cortez", "Allen Fiesta Twin Cortez", "allen-fiesta-twin-cortez", "Colorado", "Cortez")
  ))
  private def p_cortland: R = ("cortland", "cortland-new-york", "Cortland", "New York", 42.6012, -76.1805, "America/New_York", false, Seq("Cortland"), Seq(
    ("Crown City Cinemas Cortland", "Crown City Cinemas Cortland", "crown-city-cinemas-cortland", "New York", "Cortland")
  ))
  private def p_corvallis: R = ("corvallis", "corvallis-oregon", "Corvallis", "Oregon", 44.5646, -123.262, "America/Los_Angeles", false, Seq("Corvallis"), Seq(
    ("AMC CLASSIC Corvallis 12", "AMC CLASSIC Corvallis 12", "amc-classic-corvallis-12", "Oregon", "Corvallis"),
    ("Darkside Cinema Corvallis 4", "Darkside Cinema Corvallis 4", "darkside-cinema-corvallis-4", "Oregon", "Corvallis"),
    ("Whiteside Theatre Corvallis", "Whiteside Theatre Corvallis", "whiteside-theatre-corvallis", "Oregon", "Corvallis")
  ))
  private def p_coshocton: R = ("coshocton", "coshocton-ohio", "Coshocton", "Ohio", 40.272, -81.8596, "America/New_York", false, Seq("Coshocton"), Seq(
    ("Shelby Theatres Coshocton", "Shelby Theatres Coshocton", "shelby-theatres-coshocton", "Ohio", "Coshocton")
  ))
  private def p_coudersport: R = ("coudersport", "coudersport-pennsylvania", "Coudersport", "Pennsylvania", 41.7748, -78.0206, "America/New_York", false, Seq("Coudersport"), Seq(
    ("Coudersport Theatre", "Coudersport Theatre", "coudersport-theatre", "Pennsylvania", "Coudersport")
  ))
  private def p_council_bluffs: R = ("council-bluffs", "council-bluffs-iowa", "Council Bluffs", "Iowa", 41.2619, -95.8608, "America/Chicago", true, Seq("Council Bluffs", "Bellevue", "Elkhorn", "Glenwood", "Missouri Valley", "Mineola"), Seq(
    ("ACX Cinema Elkhorn 12+", "ACX Cinema Elkhorn 12+", "acx-cinema-elkhorn-12", "Nebraska", "Elkhorn"),
    ("AMC Council Bluffs 17", "AMC Council Bluffs 17", "amc-council-bluffs-17", "Iowa", "Council Bluffs"),
    ("Marcus Twin Creek Cinema", "Marcus Twin Creek Cinema", "marcus-twin-creek-cinema", "Nebraska", "Bellevue"),
    ("Rialto Missouri Valley", "Rialto Missouri Valley", "rialto-missouri-valley", "Iowa", "Missouri Valley"),
    ("Neola Phoenix Theater", "Neola Phoenix Theater", "neola-phoenix-theater", "Iowa", "Mineola"),
    ("Rex Theatre Glenwood", "Rex Theatre Glenwood", "rex-theatre-glenwood", "Iowa", "Glenwood")
  ))
  private def p_covington: R = ("covington", "covington-virginia", "Covington", "Virginia", 37.7935, -79.9939, "America/New_York", false, Seq("Covington"), Seq(
    ("R/C Covington Movies 3", "R/C Covington Movies 3", "rc-covington-movies-3", "Virginia", "Covington")
  ))
  private def p_craig: R = ("craig", "craig-colorado", "Craig", "Colorado", 40.5153, -107.5464, "America/Denver", false, Seq("Craig"), Seq(
    ("West Twin Cinema", "West Twin Cinema", "west-twin-cinema", "Colorado", "Craig")
  ))
  private def p_cranberry_township: R = ("cranberry-township", "cranberry-township-pennsylvania", "Cranberry Township", "Pennsylvania", 40.685, -80.1071, "America/New_York", true, Seq("Cranberry Township", "Monaca", "Zelienople"), Seq(
    ("Cinemark Monaca 12", "Cinemark Monaca 12", "cinemark-monaca-12", "Pennsylvania", "Monaca"),
    ("Tuxedo Cranberry Cinemas (formerly MovieScoop)", "Tuxedo Cranberry Cinemas (formerly MovieScoop)", "moviescoop-cranberry-cinemas", "Pennsylvania", "Cranberry"),
    ("Strand Theater Zelienople", "Strand Theater Zelienople", "strand-theater-zelienople", "Pennsylvania", "Zelienople")
  ))
  private def p_crawfordsville: R = ("crawfordsville", "crawfordsville-indiana", "Crawfordsville", "Indiana", 40.0412, -86.8744, "America/Indiana/Indianapolis", true, Seq("Crawfordsville", "Thorntown"), Seq(
    ("AMC CLASSIC Crawfordsville 8", "AMC CLASSIC Crawfordsville 8", "amc-classic-crawfordsville-8", "Indiana", "Crawfordsville"),
    ("M.E.L.S. at the Starlite Drive-In Theatre", "M.E.L.S. at the Starlite Drive-In Theatre", "mels-at-the-starlite-drive-in-theatre", "Indiana", "Thorntown")
  ))
  private def p_crescent_city: R = ("crescent-city", "crescent-city-california", "Crescent City", "California", 41.7559, -124.2018, "America/Los_Angeles", false, Seq("Crescent City"), Seq(
    ("Crescent City Cinemas", "Crescent City Cinemas", "crescent-city-cinemas", "California", "Crescent City")
  ))
  private def p_crested_butte: R = ("crested-butte", "crested-butte-colorado", "Crested Butte", "Colorado", 38.8697, -106.9878, "America/Denver", false, Seq("Crested Butte"), Seq(
    ("Majestic Crested Butte", "Majestic Crested Butte", "majestic-crested-butte", "Colorado", "Crested Butte")
  ))
  private def p_crestview: R = ("crestview", "crestview-florida", "Crestview", "Florida", 30.7621, -86.5705, "America/Chicago", true, Seq("Crestview", "Fort Walton Beach", "Destin", "Miramar Beach", "Mary Esther"), Seq(
    ("AMC CLASSIC Boulevard 10", "AMC CLASSIC Boulevard 10", "amc-classic-boulevard-10", "Florida", "Miramar Beach"),
    ("AMC Destin Commons 14", "AMC Destin Commons 14", "amc-destin-commons-14", "Florida", "Destin"),
    ("Marquis Crestview 10", "Marquis Crestview 10", "marquis-crestview-10", "Florida", "Crestview"),
    ("Regal Santa Rosa Mary Esther", "Regal Santa Rosa Mary Esther", "regal-santa-rosa-mary-esther", "Florida", "Mary Esther"),
    ("Suds N Cinema", "Suds N Cinema", "suds-n-cinema", "Florida", "Fort Walton Beach")
  ))
  private def p_crete: R = ("crete", "crete-nebraska", "Crete", "Nebraska", 40.6278, -96.9614, "America/Chicago", false, Seq("Crete"), Seq(
    ("ISIS Theatre Crete", "ISIS Theatre Crete", "isis-theatre-crete", "Nebraska", "Crete")
  ))
  private def p_creve_coeur: R = ("creve-coeur", "creve-coeur-missouri", "Creve Coeur", "Missouri", 38.6609, -90.4226, "America/Chicago", true, Seq("Creve Coeur", "Chesterfield", "Des Peres", "Frontenac"), Seq(
    ("AMC Creve Coeur 12", "AMC Creve Coeur 12", "amc-creve-coeur-12", "Missouri", "Creve Coeur"),
    ("B&B Creve Coeur West Olive 10", "B&B Creve Coeur West Olive 10", "bb-creve-coeur-west-olive-10", "Missouri", "Creve Coeur"),
    ("Marcus Chesterfield 14", "Marcus Chesterfield 14", "marcus-chesterfield-14", "Missouri", "Chesterfield"),
    ("Marcus Des Peres 14", "Marcus Des Peres 14", "marcus-des-peres-14", "Missouri", "Des Peres"),
    ("Landmark Plaza Frontenac Cinema", "Landmark Plaza Frontenac Cinema", "landmark-plaza-frontenac-cinema", "Missouri", "Frontenac")
  ))
  private def p_crookston: R = ("crookston", "crookston-minnesota", "Crookston", "Minnesota", 47.7741, -96.6081, "America/Chicago", false, Seq("Crookston"), Seq(
    ("Grand Theatre Crookston", "Grand Theatre Crookston", "grand-theatre-crookston", "Minnesota", "Crookston")
  ))
  private def p_crosby: R = ("crosby", "crosby-north-dakota", "Crosby", "North Dakota", 48.9142, -103.2949, "America/Chicago", false, Seq("Crosby"), Seq(
    ("Dakota Crosby", "Dakota Crosby", "dakota-crosby", "North Dakota", "Crosby")
  ))
  private def p_crowley: R = ("crowley", "crowley-louisiana", "Crowley", "Louisiana", 30.2141, -92.3746, "America/Chicago", false, Seq("Crowley"), Seq(
    ("Crowley Cinema", "Crowley Cinema", "crowley-cinema", "Louisiana", "Crowley")
  ))
  private def p_crystal_lake: R = ("crystal-lake", "crystal-lake-illinois", "Crystal Lake", "Illinois", 42.2411, -88.3162, "America/Chicago", true, Seq("Crystal Lake", "McHenry", "Woodstock", "Fox Lake"), Seq(
    ("Classic Cinemas Fox Lake", "Classic Cinemas Fox Lake", "classic-cinemas-fox-lake", "Illinois", "Fox Lake"),
    ("Classic Cinemas Woodstock", "Classic Cinemas Woodstock", "classic-cinemas-woodstock", "Illinois", "Woodstock"),
    ("Regal Crystal Lake Showplace", "Regal Crystal Lake Showplace", "regal-crystal-lake-showplace", "Illinois", "Crystal Lake"),
    ("McHenry Outdoor Theater", "McHenry Outdoor Theater", "mchenry-outdoor-theater", "Illinois", "McHenry")
  ))
  private def p_cullman: R = ("cullman", "cullman-alabama", "Cullman", "Alabama", 34.1748, -86.8436, "America/Chicago", false, Seq("Cullman"), Seq(
    ("AMC CLASSIC Marktplatz 10", "AMC CLASSIC Marktplatz 10", "amc-classic-marktplatz-10", "Alabama", "Cullman")
  ))
  private def p_culpeper: R = ("culpeper", "culpeper-virginia", "Culpeper", "Virginia", 38.4732, -77.9967, "America/New_York", false, Seq("Culpeper"), Seq(
    ("Regal Culpeper Movies", "Regal Culpeper Movies", "regal-culpeper-movies", "Virginia", "Culpeper")
  ))
  private def p_curtis: R = ("curtis", "curtis-nebraska", "Curtis", "Nebraska", 40.6318, -100.5158, "America/Chicago", false, Seq("Curtis"), Seq(
    ("Star Theater Curtis", "Star Theater Curtis", "star-theater-curtis", "Nebraska", "Curtis")
  ))
  private def p_cut_bank: R = ("cut-bank", "cut-bank-montana", "Cut Bank", "Montana", 48.633, -112.3262, "America/Denver", false, Seq("Cut Bank"), Seq(
    ("Glacier Cinemas Cut Bank", "Glacier Cinemas Cut Bank", "glacier-cinemas-cut-bank", "Montana", "Cut Bank")
  ))
  private def p_dacula: R = ("dacula", "dacula-georgia", "Dacula", "Georgia", 33.9887, -83.898, "America/New_York", true, Seq("Dacula", "Russell"), Seq(
    ("AMC CLASSIC Bethlehem 12", "AMC CLASSIC Bethlehem 12", "amc-classic-bethlehem-12", "Georgia", "Bethlehem"),
    ("Regal Hamilton Mill Dacula", "Regal Hamilton Mill Dacula", "regal-hamilton-mill-dacula", "Georgia", "Dacula")
  ))
  private def p_dallas_oregon: R = ("dallas-oregon", "dallas-oregon", "Dallas", "Oregon", 44.9193, -123.317, "America/Los_Angeles", true, Seq("Dallas", "Independence"), Seq(
    ("Dallas Cinema", "Dallas Cinema", "dallas-cinema", "Oregon", "Dallas"),
    ("Independence Cinema 8", "Independence Cinema 8", "independence-cinema-8", "Oregon", "Independence")
  ))
  private def p_dalton: R = ("dalton", "dalton-georgia", "Dalton", "Georgia", 34.7698, -84.9702, "America/New_York", false, Seq("Dalton"), Seq(
    ("AMC CLASSIC Walnut Square 12", "AMC CLASSIC Walnut Square 12", "amc-classic-walnut-square-12", "Georgia", "Dalton")
  ))
  private def p_danbury: R = ("danbury", "danbury-connecticut", "Danbury", "Connecticut", 41.3948, -73.454, "America/New_York", true, Seq("Danbury", "Trumbull", "Southbury", "Bethel", "New Milford", "Carmel", "Newtown", "Bedford"), Seq(
    ("AMC Marquis 16", "AMC Marquis 16", "amc-marquis-16", "Connecticut", "Trumbull"),
    ("AMC Danbury 16", "AMC Danbury 16", "amc-danbury-16", "Connecticut", "Danbury"),
    ("Bank Street Theater", "Bank Street Theater", "bank-street-theater", "Connecticut", "New Milford"),
    ("Greenwood Features Bethel", "Greenwood Features Bethel", "greenwood-features-bethel", "Connecticut", "Bethel"),
    ("Riverview Cinema 8", "Riverview Cinema 8", "riverview-cinema-8", "Connecticut", "Southbury"),
    ("Bedford Playhouse", "Bedford Playhouse", "bedford-playhouse", "New York", "Bedford"),
    ("Box Office Cinemas Carmel", "Box Office Cinemas Carmel", "carmel-cinema", "New York", "Carmel"),
    ("Edmond Town Hall Newtown", "Edmond Town Hall Newtown", "edmond-town-hall-newtown", "Connecticut", "Newtown")
  ))
  private def p_danville: R = ("danville", "danville-virginia", "Danville", "Virginia", 36.586, -79.395, "America/New_York", false, Seq("Danville"), Seq(
    ("GTC Danville Cinemas", "GTC Danville Cinemas", "gtc-danville-cinemas", "Virginia", "Danville")
  ))
  private def p_danville_kentucky: R = ("danville-kentucky", "danville-kentucky", "Danville", "Kentucky", 37.6456, -84.7722, "America/New_York", true, Seq("Danville", "Stanford"), Seq(
    ("Phoenix Theatres Danville 8", "Phoenix Theatres Danville 8", "phoenix-theatres-danville-8", "Kentucky", "Danville"),
    ("Stanford Drive-in", "Stanford Drive-in", "stanford-drive-in", "Kentucky", "Stanford")
  ))
  private def p_daphne: R = ("daphne", "daphne-alabama", "Daphne", "Alabama", 30.6035, -87.9036, "America/Chicago", true, Seq("Daphne", "Spanish Fort"), Seq(
    ("AMC CLASSIC Jubilee Square 12", "AMC CLASSIC Jubilee Square 12", "amc-classic-jubilee-square-12", "Alabama", "Daphne"),
    ("Premiere Eastern Shore Cinema 14", "Premiere Eastern Shore Cinema 14", "premiere-eastern-shore-cinema-14", "Alabama", "Spanish Fort")
  ))
  private def p_davis: R = ("davis", "davis-california", "Davis", "California", 38.5449, -121.7405, "America/Los_Angeles", true, Seq("Davis", "Woodland"), Seq(
    ("Davis Varsity Theatre", "Davis Varsity Theatre", "davis-varsity-theatre", "California", "Davis"),
    ("Regal Davis Holiday", "Regal Davis Holiday", "regal-davis-holiday", "California", "Davis"),
    ("State Theatre Woodland", "State Theatre Woodland", "state-theatre-woodland", "California", "Woodland")
  ))
  private def p_dayton_washington: R = ("dayton-washington", "dayton-washington", "Dayton", "Washington", 46.3237, -117.9724, "America/Los_Angeles", false, Seq("Dayton"), Seq(
    ("Liberty Theater Dayton", "Liberty Theater Dayton", "liberty-theater-dayton", "Washington", "Dayton")
  ))
  private def p_daytona_beach: R = ("daytona-beach", "daytona-beach-florida", "Daytona Beach", "Florida", 29.2108, -81.0228, "America/New_York", true, Seq("Daytona Beach", "Port Orange", "New Smyrna Beach"), Seq(
    ("AMC CLASSIC New Smyrna 12", "AMC CLASSIC New Smyrna 12", "amc-classic-new-smyrna-12", "Florida", "New Smyrna Beach"),
    ("Cinematique Of Daytona", "Cinematique Of Daytona", "cinematique-of-daytona", "Florida", "Daytona Beach"),
    ("CMX Cinemas Daytona", "CMX Cinemas Daytona", "cmx-cinemas-daytona", "Florida", "Daytona Beach"),
    ("Regal Pavilion Port Orange", "Regal Pavilion Port Orange", "regal-pavilion-port-orange", "Florida", "Port Orange")
  ))
  private def p_de_pere: R = ("de-pere", "de-pere-wisconsin", "De Pere", "Wisconsin", 44.4489, -88.0604, "America/Chicago", false, Seq("De Pere"), Seq(
    ("De Pere Cinema Cafe", "De Pere Cinema Cafe", "de-pere-cinema-cafe", "Wisconsin", "De Pere"),
    ("Discovery Cinema – Mulva Cultural Center De Pere", "Discovery Cinema – Mulva Cultural Center De Pere", "discovery-cinema---mulva-cultural-center-de-pere", "Wisconsin", "De Pere")
  ))
  private def p_de_soto: R = ("de-soto", "de-soto-missouri", "De Soto", "Missouri", 38.1395, -90.5551, "America/Chicago", true, Seq("De Soto", "Cadet"), Seq(
    ("Melba Theatre Desoto", "Melba Theatre Desoto", "melba-theatre-desoto", "Missouri", "De Soto"),
    ("Starlite Drive-in Cadet", "Starlite Drive-in Cadet", "starlite-drive-in-cadet", "Missouri", "Cadet")
  ))
  private def p_dearborn: R = ("dearborn", "dearborn-michigan", "Dearborn", "Michigan", 42.3223, -83.1763, "America/Detroit", true, Seq("Dearborn", "Livonia", "Westland", "Taylor", "Royal Oak", "Southgate", "Wayne", "Woodhaven", "Grosse Pointe Park"), Seq(
    ("AMC Livonia 19", "AMC Livonia 19", "amc-livonia-19", "Michigan", "Livonia"),
    ("Phoenix Theatres Laurel Park", "Phoenix Theatres Laurel Park", "phoenix-theatres-laurel-park", "Michigan", "Livonia"),
    ("Cinemark Southland Center", "Cinemark Southland Center", "cinemark-southland-center", "Michigan", "Taylor"),
    ("Emagine Royal Oak", "Emagine Royal Oak", "emagine-royal-oak", "Michigan", "Royal Oak"),
    ("Emagine Woodhaven", "Emagine Woodhaven", "emagine-woodhaven", "Michigan", "Woodhaven"),
    ("MJR Southgate Cinema", "MJR Southgate Cinema", "mjr-southgate-cinema", "Michigan", "Southgate"),
    ("MJR Westland Grand Cinema 16", "MJR Westland Grand Cinema 16", "mjr-westland-grand-cinema-16", "Michigan", "Westland"),
    ("Phoenix State Wayne", "Phoenix State Wayne", "phoenix-state-wayne", "Michigan", "Wayne"),
    ("Windmill Pointe Park", "Windmill Pointe Park", "windmill-pointe-park", "Michigan", "Grosse Pointe Park"),
    ("Ford Wyoming Drive In", "Ford Wyoming Drive In", "ford-wyoming-drive-in", "Michigan", "Dearborn"),
    ("Henry Ford Giant Screen Dearborn", "Henry Ford Giant Screen Dearborn", "henry-ford-giant-screen-dearborn", "Michigan", "Dearborn")
  ))
  private def p_decatur: R = ("decatur", "decatur-illinois", "Decatur", "Illinois", 39.8403, -88.9548, "America/Chicago", false, Seq("Decatur"), Seq(
    ("AMC CLASSIC Decatur 10", "AMC CLASSIC Decatur 10", "amc-classic-decatur-10", "Illinois", "Decatur"),
    ("Avon Theatre Decatur", "Avon Theatre Decatur", "avon-theatre-decatur", "Illinois", "Decatur")
  ))
  private def p_decatur_alabama: R = ("decatur-alabama", "decatur-alabama", "Decatur", "Alabama", 34.6059, -86.9833, "America/Chicago", true, Seq("Decatur", "Madison"), Seq(
    ("AMC CLASSIC Decatur 12", "AMC CLASSIC Decatur 12", "amc-classic-decatur-12", "Alabama", "Decatur"),
    ("Cineplanet 15", "Cineplanet 15", "cineplanet-15", "Alabama", "Madison")
  ))
  private def p_decatur_georgia: R = ("decatur-georgia", "decatur-georgia", "Decatur", "Georgia", 33.7748, -84.2963, "America/New_York", true, Seq("Decatur", "Stone Mountain"), Seq(
    ("AMC North Dekalb 16", "AMC North Dekalb 16", "amc-north-dekalb-16", "Georgia", "Decatur"),
    ("NCG Stone Mountain", "NCG Stone Mountain", "ncg-stone-mountain", "Georgia", "Stone Mountain")
  ))
  private def p_decatur_texas: R = ("decatur-texas", "decatur-texas", "Decatur", "Texas", 33.2343, -97.5861, "America/Chicago", false, Seq("Decatur"), Seq(
    ("Plaza Cinema IV Decatur", "Plaza Cinema IV Decatur", "plaza-cinema-iv-decatur", "Texas", "Decatur")
  ))
  private def p_decorah: R = ("decorah", "decorah-iowa", "Decorah", "Iowa", 43.3033, -91.7857, "America/Chicago", true, Seq("Decorah", "Cresco", "Waukon", "Spring Grove", "Harmony"), Seq(
    ("Cresco Theatre and Opera House", "Cresco Theatre and Opera House", "cresco-theatre-and-opera-house", "Iowa", "Cresco"),
    ("Decorah Viking 3", "Decorah Viking 3", "decorah-viking-3", "Iowa", "Decorah"),
    ("Town Theater & Pizza Pub", "Town Theater & Pizza Pub", "town-theater-pizza-pub", "Iowa", "Waukon"),
    ("Jem Theatre Harmony", "Jem Theatre Harmony", "jem-theatre-harmony", "Minnesota", "Harmony"),
    ("Spring Grove Cinema", "Spring Grove Cinema", "spring-grove-cinema", "Minnesota", "Spring Grove")
  ))
  private def p_dededo_village: R = ("dededo-village", "dededo-village-guam", "Dededo Village", "Guam", 13.5178, 144.8391, "Pacific/Guam", true, Seq("Dededo Village", "Apotgan"), Seq(
    ("MM Theatres", "MM Theatres", "mm-theatres", "Guam", "Dededo"),
    ("Regal Guam", "Regal Guam", "regal-guam", "Guam", "Apotgan")
  ))
  private def p_defiance: R = ("defiance", "defiance-ohio", "Defiance", "Ohio", 41.2845, -84.3558, "America/New_York", true, Seq("Defiance", "Bryan"), Seq(
    ("Bryan Cinema 3", "Bryan Cinema 3", "bryan-cinema-3", "Ohio", "Bryan"),
    ("Phoenix Northtowne Mall Defiance", "Phoenix Northtowne Mall Defiance", "phoenix-northtowne-mall-defiance", "Ohio", "Defiance")
  ))
  private def p_dekalb: R = ("dekalb", "dekalb-illinois", "DeKalb", "Illinois", 41.9295, -88.7504, "America/Chicago", true, Seq("DeKalb", "Sycamore"), Seq(
    ("AMC Market Square 10", "AMC Market Square 10", "amc-market-square-10", "Illinois", "DeKalb"),
    ("Egyptian DeKalb", "Egyptian DeKalb", "egyptian-dekalb", "Illinois", "DeKalb"),
    ("State Theater Sycamore", "State Theater Sycamore", "state-theater-sycamore", "Illinois", "Sycamore")
  ))
  private def p_del_rio: R = ("del-rio", "del-rio-texas", "Del Rio", "Texas", 29.3627, -100.8968, "America/Chicago", false, Seq("Del Rio"), Seq(
    ("Cinemark Del Rio 8", "Cinemark Del Rio 8", "cinemark-del-rio-8", "Texas", "Del Rio")
  ))
  private def p_delano: R = ("delano", "delano-california", "Delano", "California", 35.7688, -119.2471, "America/Los_Angeles", false, Seq("Delano"), Seq(
    ("Maya Cinemas Delano", "Maya Cinemas Delano", "maya-cinemas-delano", "California", "Delano")
  ))
  private def p_dell_rapids: R = ("dell-rapids", "dell-rapids-south-dakota", "Dell Rapids", "South Dakota", 43.8261, -96.7062, "America/Chicago", false, Seq("Dell Rapids"), Seq(
    ("Dells Theatre", "Dells Theatre", "dells-theatre", "South Dakota", "Dell Rapids")
  ))
  private def p_delray_beach: R = ("delray-beach", "delray-beach-florida", "Delray Beach", "Florida", 26.4615, -80.0728, "America/New_York", false, Seq("Delray Beach"), Seq(
    ("Hooky Entertainment Delray Beach", "Hooky Entertainment Delray Beach", "hooky-entertainment-delray-beach", "Florida", "Delray Beach"),
    ("IPIC Cinemas Delray Beach", "IPIC Cinemas Delray Beach", "ipic-cinemas-delray-beach", "Florida", "Delray Beach"),
    ("Movies of Delray", "Movies of Delray", "movies-of-delray", "Florida", "Delray Beach")
  ))
  private def p_delta: R = ("delta", "delta-colorado", "Delta", "Colorado", 38.7422, -108.069, "America/Denver", false, Seq("Delta"), Seq(
    ("Egyptian Theater Delta", "Egyptian Theater Delta", "egyptian-theater-delta", "Colorado", "Delta"),
    ("Tru Vu Drive-In Theatre", "Tru Vu Drive-In Theatre", "tru-vu-drive-in-theatre", "Colorado", "Delta")
  ))
  private def p_delta_utah: R = ("delta-utah", "delta-utah", "Delta", "Utah", 39.3522, -112.5772, "America/Denver", false, Seq("Delta"), Seq(
    ("T&T Twin Theaters", "T&T Twin Theaters", "tt-twin-theaters", "Utah", "Delta")
  ))
  private def p_deming: R = ("deming", "deming-new-mexico", "Deming", "New Mexico", 32.2687, -107.7586, "America/Denver", false, Seq("Deming"), Seq(
    ("Starmax Deming 6", "Starmax Deming 6", "starmax-deming-6", "New Mexico", "Deming")
  ))
  private def p_demopolis: R = ("demopolis", "demopolis-alabama", "Demopolis", "Alabama", 32.5176, -87.8364, "America/Chicago", false, Seq("Demopolis"), Seq(
    ("Warhouse Performing Arts Center", "Warhouse Performing Arts Center", "warhouse-performing-arts-center", "Alabama", "Demopolis"),
    ("Warhouse Drive-in", "Warhouse Drive-in", "warhouse-drive-in", "Alabama", "Demopolis")
  ))
  private def p_denison: R = ("denison", "denison-iowa", "Denison", "Iowa", 42.0178, -95.3553, "America/Chicago", false, Seq("Denison"), Seq(
    ("Crawford County Cinema IV", "Crawford County Cinema IV", "crawford-county-cinema-iv", "Iowa", "Denison")
  ))
  private def p_dennis: R = ("dennis", "dennis-massachusetts", "Dennis", "Massachusetts", 41.7354, -70.1939, "America/New_York", true, Seq("Dennis", "Hyannis", "South Dennis"), Seq(
    ("Cape Cinema Dennis", "Cape Cinema Dennis", "cape-cinema-dennis", "Massachusetts", "Dennis"),
    ("Cape Cod Museum of Art", "Cape Cod Museum of Art", "cape-cod-museum-of-art", "Massachusetts", "Dennis"),
    ("Entertainment Cinemas Hyannis", "Entertainment Cinemas Hyannis", "entertainment-cinemas-hyannis", "Massachusetts", "Hyannis"),
    ("Entertainment Cinemas South Dennis", "Entertainment Cinemas South Dennis", "entertainment-cinemas-south-dennis", "Massachusetts", "South Dennis")
  ))
  private def p_derby: R = ("derby", "derby-kansas", "Derby", "Kansas", 37.5456, -97.2689, "America/Chicago", false, Seq("Derby"), Seq(
    ("Derby Plaza Theaters", "Derby Plaza Theaters", "derby-plaza-theaters", "Kansas", "Derby")
  ))
  private def p_detroit_lakes: R = ("detroit-lakes", "detroit-lakes-minnesota", "Detroit Lakes", "Minnesota", 46.8172, -95.8453, "America/Chicago", false, Seq("Detroit Lakes"), Seq(
    ("Odyssey Detroit Lakes", "Odyssey Detroit Lakes", "odyssey-detroit-lakes", "Minnesota", "Detroit Lakes")
  ))
  private def p_devils_lake: R = ("devils-lake", "devils-lake-north-dakota", "Devils Lake", "North Dakota", 48.1128, -98.8651, "America/Chicago", false, Seq("Devils Lake"), Seq(
    ("Encore 301 Theater", "Encore 301 Theater", "encore-301-theater", "North Dakota", "Devils Lake")
  ))
  private def p_dexter: R = ("dexter", "dexter-missouri", "Dexter", "Missouri", 36.7959, -89.9579, "America/Chicago", false, Seq("Dexter"), Seq(
    ("Dexter Twin Cinema", "Dexter Twin Cinema", "dexter-twin-cinema", "Missouri", "Dexter")
  ))
  private def p_dickinson: R = ("dickinson", "dickinson-north-dakota", "Dickinson", "North Dakota", 46.8792, -102.7896, "America/Denver", false, Seq("Dickinson"), Seq(
    ("Golden Ticket Cinemas Dickinson 3", "Golden Ticket Cinemas Dickinson 3", "golden-ticket-cinemas-dickinson-3", "North Dakota", "Dickinson")
  ))
  private def p_dickson: R = ("dickson", "dickson-tennessee", "Dickson", "Tennessee", 36.077, -87.3878, "America/Chicago", true, Seq("Dickson", "Waverly", "Camden"), Seq(
    ("Roxy Dickson 8", "Roxy Dickson 8", "roxy-dickson-8", "Tennessee", "Dickson"),
    ("Birdsong Drive In Camden", "Birdsong Drive In Camden", "birdsong-drive-in-camden", "Tennessee", "Camden"),
    ("Mi-De-Ga Theatre Waverly", "Mi-De-Ga Theatre Waverly", "mi-de-ga-theatre-waverly", "Tennessee", "Waverly")
  ))
  private def p_dillon: R = ("dillon", "dillon-montana", "Dillon", "Montana", 45.2163, -112.6375, "America/Denver", false, Seq("Dillon"), Seq(
    ("Big Sky Cinema Dillon", "Big Sky Cinema Dillon", "big-sky-cinema-dillon", "Montana", "Dillon")
  ))
  private def p_dodge_city: R = ("dodge-city", "dodge-city-kansas", "Dodge City", "Kansas", 37.7528, -100.0171, "America/Chicago", false, Seq("Dodge City"), Seq(
    ("B&B Dodge City Village 8", "B&B Dodge City Village 8", "bb-dodge-city-village-8", "Kansas", "Dodge City"),
    ("South Drive-in Dodge City", "South Drive-in Dodge City", "south-drive-in-dodge-city", "Kansas", "Dodge City")
  ))
  private def p_dodgeville: R = ("dodgeville", "dodgeville-wisconsin", "Dodgeville", "Wisconsin", 42.9603, -90.1301, "America/Chicago", true, Seq("Dodgeville", "Mineral Point", "Spring Green"), Seq(
    ("Dodge Theatre", "Dodge Theatre", "dodge-theatre", "Wisconsin", "Dodgeville"),
    ("Gard Theater Spring Green", "Gard Theater Spring Green", "gard-theater-spring-green", "Wisconsin", "Spring Green"),
    ("Mineral Point Opera House", "Mineral Point Opera House", "mineral-point-opera-house", "Wisconsin", "Mineral Point")
  ))
  private def p_dorado: R = ("dorado", "dorado-puerto-rico", "Dorado", "Puerto Rico", 18.4588, -66.2677, "America/Puerto_Rico", true, Seq("Dorado", "Vega Alta"), Seq(
    ("Caribbean Cinemas Dorado", "Caribbean Cinemas Dorado", "caribbean-cinemas-dorado", "Puerto Rico", "Dorado"),
    ("Caribbean Cinemas Vega Alta", "Caribbean Cinemas Vega Alta", "caribbean-cinemas-vega-alta", "Puerto Rico", "Vega Alta")
  ))
  private def p_dothan: R = ("dothan", "dothan-alabama", "Dothan", "Alabama", 31.2232, -85.3905, "America/Chicago", false, Seq("Dothan"), Seq(
    ("AMC CLASSIC Dothan 6", "AMC CLASSIC Dothan 6", "amc-classic-dothan-6", "Alabama", "Dothan"),
    ("AMC CLASSIC Dothan Pavillion 12", "AMC CLASSIC Dothan Pavillion 12", "amc-classic-dothan-pavillion-12", "Alabama", "Dothan")
  ))
  private def p_douglas: R = ("douglas", "douglas-georgia", "Douglas", "Georgia", 31.5088, -82.8499, "America/New_York", false, Seq("Douglas"), Seq(
    ("Rockin 8 Cinemas Douglas", "Rockin 8 Cinemas Douglas", "rockin-8-cinemas-douglas", "Georgia", "Douglas")
  ))
  private def p_douglas_wyoming: R = ("douglas-wyoming", "douglas-wyoming", "Douglas", "Wyoming", 42.7597, -105.3822, "America/Denver", false, Seq("Douglas"), Seq(
    ("Princess Theatre Douglas", "Princess Theatre Douglas", "princess-theatre-douglas", "Wyoming", "Douglas")
  ))
  private def p_dover_foxcroft: R = ("dover-foxcroft", "dover-foxcroft-maine", "Dover-Foxcroft", "Maine", 45.1834, -69.227, "America/New_York", false, Seq("Dover-Foxcroft"), Seq(
    ("Center Theatre Dover-Foxcroft", "Center Theatre Dover-Foxcroft", "center-theatre-dover-foxcroft", "Maine", "Dover-Foxcroft")
  ))
  private def p_dublin: R = ("dublin", "dublin-ohio", "Dublin", "Ohio", 40.0992, -83.1141, "America/New_York", true, Seq("Dublin", "Grove City", "Delaware", "Gahanna", "Pickerington", "Grandview Heights"), Seq(
    ("AMC Dublin Village 18", "AMC Dublin Village 18", "amc-dublin-village-18", "Ohio", "Dublin"),
    ("AMC Grove City 14", "AMC Grove City 14", "amc-grove-city-14", "Ohio", "Grove City"),
    ("Cinemark Stoneridge Plaza Movies 16", "Cinemark Stoneridge Plaza Movies 16", "cinemark-stoneridge-plaza-movies-16", "Ohio", "Gahanna"),
    ("Grandview Theatre & Drafthouse", "Grandview Theatre & Drafthouse", "grandview-theatre-drafthouse", "Ohio", "Grandview"),
    ("Marcus Pickerington", "Marcus Pickerington", "marcus-pickerington", "Ohio", "Pickerington"),
    ("Strand Theatre Delaware", "Strand Theatre Delaware", "strand-theatre-delaware", "Ohio", "Delaware")
  ))
  private def p_dublin_georgia: R = ("dublin-georgia", "dublin-georgia", "Dublin", "Georgia", 32.5404, -82.9038, "America/New_York", false, Seq("Dublin"), Seq(
    ("Golden Ticket Cinemas Dublin 8", "Golden Ticket Cinemas Dublin 8", "golden-ticket-cinemas-dublin-8", "Georgia", "Dublin")
  ))
  private def p_dubuque: R = ("dubuque", "dubuque-iowa", "Dubuque", "Iowa", 42.5006, -90.6646, "America/Chicago", false, Seq("Dubuque"), Seq(
    ("AMC CLASSIC Dubuque 14", "AMC CLASSIC Dubuque 14", "amc-classic-dubuque-14", "Iowa", "Dubuque"),
    ("Phoenix Kennedy Mall Dubuque", "Phoenix Kennedy Mall Dubuque", "phoenix-kennedy-mall-dubuque", "Iowa", "Dubuque")
  ))
  private def p_duluth: R = ("duluth", "duluth-minnesota", "Duluth", "Minnesota", 46.7833, -92.1066, "America/Chicago", false, Seq("Duluth"), Seq(
    ("Marcus Duluth Cinema", "Marcus Duluth Cinema", "marcus-duluth-cinema", "Minnesota", "Duluth"),
    ("Zinema Duluth", "Zinema Duluth", "zinema-duluth", "Minnesota", "Duluth"),
    ("West Theater", "West Theater", "west-theater", "Minnesota", "Duluth")
  ))
  private def p_duluth_georgia: R = ("duluth-georgia", "duluth-georgia", "Duluth", "Georgia", 34.0029, -84.1446, "America/New_York", true, Seq("Duluth", "Tucker", "Snellville", "Suwanee", "Buford"), Seq(
    ("Movie Tavern Horizon Village Suwanee", "Movie Tavern Horizon Village Suwanee", "movie-tavern-horizon-village-suwanee", "Georgia", "Suwanee"),
    ("Movie Tavern Tucker Cinema", "Movie Tavern Tucker Cinema", "movie-tavern-tucker-cinema", "Georgia", "Tucker"),
    ("NCG Snellville", "NCG Snellville", "ncg-snellville", "Georgia", "Snellville"),
    ("Regal Mall Of Georgia", "Regal Mall Of Georgia", "regal-mall-of-georgia", "Georgia", "Buford"),
    ("Regal Medlock Crossing Duluth", "Regal Medlock Crossing Duluth", "regal-medlock-crossing-duluth", "Georgia", "Duluth"),
    ("Studio Movie Grill Duluth", "Studio Movie Grill Duluth", "studio-movie-grill-duluth", "Georgia", "Duluth")
  ))
  private def p_dumas: R = ("dumas", "dumas-texas", "Dumas", "Texas", 35.8656, -101.9732, "America/Chicago", false, Seq("Dumas"), Seq(
    ("Evelyn Theatre Dumas", "Evelyn Theatre Dumas", "evelyn-theatre-dumas", "Texas", "Dumas")
  ))
  private def p_duncan: R = ("duncan", "duncan-oklahoma", "Duncan", "Oklahoma", 34.5023, -97.9578, "America/Chicago", false, Seq("Duncan"), Seq(
    ("Heritage Park Duncan", "Heritage Park Duncan", "heritage-park-duncan", "Oklahoma", "Duncan"),
    ("Palace Theatre Duncan", "Palace Theatre Duncan", "palace-theatre-duncan", "Oklahoma", "Duncan")
  ))
  private def p_dunkirk: R = ("dunkirk", "dunkirk-new-york", "Dunkirk", "New York", 42.4795, -79.3339, "America/New_York", true, Seq("Dunkirk", "Fredonia", "Angola"), Seq(
    ("Dunkirk Multiplex", "Dunkirk Multiplex", "dunkirk-multiplex", "New York", "Dunkirk"),
    ("Van Buren Drive-In", "Van Buren Drive-In", "van-buren-drive-in", "New York", "Dunkirk"),
    ("Fredonia Opera House", "Fredonia Opera House", "fredonia-opera-house", "New York", "Fredonia"),
    ("New Angola Theatre", "New Angola Theatre", "new-angola-theatre", "New York", "Angola")
  ))
  private def p_durango: R = ("durango", "durango-colorado", "Durango", "Colorado", 37.2753, -107.8801, "America/Denver", false, Seq("Durango"), Seq(
    ("Allen Gaslight Twin Durango", "Allen Gaslight Twin Durango", "allen-gaslight-twin-durango", "Colorado", "Durango"),
    ("Allen Theatres Durango", "Allen Theatres Durango", "allen-theatres-durango", "Colorado", "Durango")
  ))
  private def p_durant: R = ("durant", "durant-oklahoma", "Durant", "Oklahoma", 33.994, -96.3708, "America/Chicago", true, Seq("Durant", "Caddo"), Seq(
    ("Rex II Theatre Caddo", "Rex II Theatre Caddo", "rex-ii-theatre-caddo", "Oklahoma", "Caddo"),
    ("The District Cinema Durant 4", "The District Cinema Durant 4", "the-district-cinema-durant-4", "Oklahoma", "Durant")
  ))
  private def p_eads: R = ("eads", "eads-colorado", "Eads", "Colorado", 38.4806, -102.7819, "America/Denver", false, Seq("Eads"), Seq(
    ("Plains Theater Eads", "Plains Theater Eads", "plains-theater-eads", "Colorado", "Eads")
  ))
  private def p_eagle_butte: R = ("eagle-butte", "eagle-butte-south-dakota", "Eagle Butte", "South Dakota", 45.0025, -101.2335, "America/Denver", false, Seq("Eagle Butte"), Seq(
    ("7th Generation Cinema", "7th Generation Cinema", "7th-generation-cinema", "South Dakota", "Eagle Butte")
  ))
  private def p_eagle_pass: R = ("eagle-pass", "eagle-pass-texas", "Eagle Pass", "Texas", 28.7091, -100.4995, "America/Chicago", false, Seq("Eagle Pass"), Seq(
    ("Cinemark Mall de Las Aguilas", "Cinemark Mall de Las Aguilas", "cinemark-mall-de-las-aguilas", "Texas", "Eagle Pass")
  ))
  private def p_early: R = ("early", "early-texas", "Early", "Texas", 31.7421, -98.9456, "America/Chicago", false, Seq("Early"), Seq(
    ("Regal Heartland Early", "Regal Heartland Early", "regal-heartland-early", "Texas", "Early")
  ))
  private def p_easley: R = ("easley", "easley-south-carolina", "Easley", "South Carolina", 34.8298, -82.6015, "America/New_York", false, Seq("Easley"), Seq(
    ("Easley Premiere 8", "Easley Premiere 8", "easley-premiere-8", "South Carolina", "Easley")
  ))
  private def p_east_grand_forks: R = ("east-grand-forks", "east-grand-forks-minnesota", "East Grand Forks", "Minnesota", 47.93, -97.0245, "America/Chicago", false, Seq("East Grand Forks"), Seq(
    ("River Cinema East Grand Forks", "River Cinema East Grand Forks", "river-cinema-east-grand-forks", "Minnesota", "East Grand Forks")
  ))
  private def p_east_hampton: R = ("east-hampton", "east-hampton-new-york", "East Hampton", "New York", 40.9634, -72.1848, "America/New_York", true, Seq("East Hampton", "Southampton", "Sag Harbor"), Seq(
    ("Regal East Hampton", "Regal East Hampton", "regal-east-hampton", "New York", "East Hampton"),
    ("Guild Hall", "Guild Hall", "guild-hall", "New York", "East Hampton"),
    ("Sag Harbor Cinema", "Sag Harbor Cinema", "sag-harbor-cinema", "New York", "Sag Harbor"),
    ("Southampton Playhouse", "Southampton Playhouse", "southampton-playhouse", "New York", "Southampton")
  ))
  private def p_east_lansing: R = ("east-lansing", "east-lansing-michigan", "East Lansing", "Michigan", 42.737, -84.4839, "America/Detroit", true, Seq("East Lansing", "Okemos", "Owosso", "Charlotte", "Grand Ledge", "Williamston"), Seq(
    ("Eaton Theatre Charlotte", "Eaton Theatre Charlotte", "eaton-theatre-charlotte", "Michigan", "Charlotte"),
    ("NCG Owosso Cinema", "NCG Owosso Cinema", "ncg-owosso-cinema", "Michigan", "Owosso"),
    ("Regal Lansing Mall", "Regal Lansing Mall", "regal-lansing-mall", "Michigan", "East Lansing"),
    ("Studio C! Meridian Mall", "Studio C! Meridian Mall", "studio-c-meridian-mall", "Michigan", "Okemos"),
    ("Sun Theatre Grand Ledge", "Sun Theatre Grand Ledge", "sun-theatre-grand-ledge", "Michigan", "Grand Ledge"),
    ("Sun Theatre Williamston", "Sun Theatre Williamston", "sun-theatre-williamston", "Michigan", "Williamston")
  ))
  private def p_east_stroudsburg: R = ("east-stroudsburg", "east-stroudsburg-pennsylvania", "East Stroudsburg", "Pennsylvania", 40.9995, -75.1813, "America/New_York", true, Seq("East Stroudsburg", "Stroudsburg"), Seq(
    ("Cinemark Stroud Mall", "Cinemark Stroud Mall", "cinemark-stroud-mall", "Pennsylvania", "Stroudsburg"),
    ("Pocono Community Theater", "Pocono Community Theater", "pocono-community-theater", "Pennsylvania", "East Stroudsburg")
  ))
  private def p_east_tawas: R = ("east-tawas", "east-tawas-michigan", "East Tawas", "Michigan", 44.2795, -83.4903, "America/Detroit", true, Seq("East Tawas", "Au Sable"), Seq(
    ("Family Theatre Tawas", "Family Theatre Tawas", "family-theatre-tawas", "Michigan", "East Tawas"),
    ("Lake Theatre Oscoda", "Lake Theatre Oscoda", "lake-theatre-oscoda", "Michigan", "Oscoda")
  ))
  private def p_east_windham: R = ("east-windham", "east-windham-new-york", "East Windham", "New York", 42.3064, -74.2525, "America/New_York", true, Seq("East Windham", "Fort Hunter"), Seq(
    ("Mountain Cinema Hunter", "Mountain Cinema Hunter", "mountain-cinema-hunter-2", "New York", "Fort Hunter"),
    ("Windham Theatre", "Windham Theatre", "windham-theatre", "New York", "East Windham")
  ))
  private def p_eastland: R = ("eastland", "eastland-texas", "Eastland", "Texas", 32.4015, -98.8176, "America/Chicago", false, Seq("Eastland"), Seq(
    ("Majestic Theatre Eastland", "Majestic Theatre Eastland", "majestic-theatre-eastland", "Texas", "Eastland")
  ))
  private def p_easton: R = ("easton", "easton-maryland", "Easton", "Maryland", 38.7743, -76.0763, "America/New_York", true, Seq("Easton", "Cambridge"), Seq(
    ("Flagship Cinemas Cambridge", "Flagship Cinemas Cambridge", "flagship-cinemas-cambridge", "Maryland", "Cambridge"),
    ("Flagship Cinemas Easton", "Flagship Cinemas Easton", "flagship-cinemas-easton", "Maryland", "Easton")
  ))
  private def p_eau_claire: R = ("eau-claire", "eau-claire-wisconsin", "Eau Claire", "Wisconsin", 44.8113, -91.4985, "America/Chicago", false, Seq("Eau Claire"), Seq(
    ("Micon Cinemas Eau Claire 7", "Micon Cinemas Eau Claire 7", "micon-cinemas-eau-claire-7", "Wisconsin", "Eau Claire"),
    ("Micon Cinemas Oakwood", "Micon Cinemas Oakwood", "micon-cinemas-oakwood", "Wisconsin", "Eau Claire"),
    ("Micon Downtown Cinema Eau Claire", "Micon Downtown Cinema Eau Claire", "micon-downtown-cinema-eau-claire", "Wisconsin", "Eau Claire")
  ))
  private def p_eden: R = ("eden", "eden-north-carolina", "Eden", "North Carolina", 36.4885, -79.7667, "America/New_York", false, Seq("Eden"), Seq(
    ("Eden Drive-In Twin", "Eden Drive-In Twin", "eden-drive-in-twin", "North Carolina", "Eden")
  ))
  private def p_edenton: R = ("edenton", "edenton-north-carolina", "Edenton", "North Carolina", 36.0579, -76.6077, "America/New_York", false, Seq("Edenton"), Seq(
    ("Taylor Edenton", "Taylor Edenton", "taylor-edenton", "North Carolina", "Edenton")
  ))
  private def p_edinburg: R = ("edinburg", "edinburg-texas", "Edinburg", "Texas", 26.3017, -98.1633, "America/Chicago", true, Seq("Edinburg", "Mission", "Pharr"), Seq(
    ("AMC Edinburg 18", "AMC Edinburg 18", "amc-edinburg-18", "Texas", "Edinburg"),
    ("Cinemark Bistro Edinburg", "Cinemark Bistro Edinburg", "cinemark-bistro-edinburg", "Texas", "Edinburg"),
    ("Cinemark Pharr Town Center", "Cinemark Pharr Town Center", "cinemark-pharr-town-center", "Texas", "Pharr"),
    ("Cinemark Tinseltown Mission", "Cinemark Tinseltown Mission", "cinemark-tinseltown-mission", "Texas", "Mission")
  ))
  private def p_edmond: R = ("edmond", "edmond-oklahoma", "Edmond", "Oklahoma", 35.6528, -97.4781, "America/Chicago", false, Seq("Edmond"), Seq(
    ("Icon Cinemas Edmond", "Icon Cinemas Edmond", "icon-cinemas-edmond", "Oklahoma", "Edmond"),
    ("Showbiz Cinemas Edmond", "Showbiz Cinemas Edmond", "showbiz-cinemas-edmond", "Oklahoma", "Edmond")
  ))
  private def p_edwards: R = ("edwards", "edwards-colorado", "Edwards", "Colorado", 39.645, -106.5942, "America/Denver", true, Seq("Edwards", "Eagle"), Seq(
    ("Capitol Theatre Eagle", "Capitol Theatre Eagle", "capitol-theatre-eagle", "Colorado", "Black Eagle Mill"),
    ("Riverwalk Theater Edwards", "Riverwalk Theater Edwards", "riverwalk-theater-edwards", "Colorado", "Edwards")
  ))
  private def p_edwardsville: R = ("edwardsville", "edwardsville-illinois", "Edwardsville", "Illinois", 38.8114, -89.9532, "America/Chicago", false, Seq("Edwardsville"), Seq(
    ("AMC Edwardsville 12", "AMC Edwardsville 12", "amc-edwardsville-12", "Illinois", "Edwardsville"),
    ("Wildey Theatre Edwardsville", "Wildey Theatre Edwardsville", "wildey-theatre-edwardsville", "Illinois", "Edwardsville")
  ))
  private def p_effingham: R = ("effingham", "effingham-illinois", "Effingham", "Illinois", 39.12, -88.5434, "America/Chicago", false, Seq("Effingham"), Seq(
    ("RMC Village Effingham", "RMC Village Effingham", "rmc-village-effingham", "Illinois", "Effingham")
  ))
  private def p_el_centro: R = ("el-centro", "el-centro-california", "El Centro", "California", 32.792, -115.5631, "America/Los_Angeles", true, Seq("El Centro", "Calexico", "Imperial"), Seq(
    ("Cinemark Century Imperial Valley Mall 14", "Cinemark Century Imperial Valley Mall 14", "cinemark-century-imperial-valley-mall-14", "California", "El Centro"),
    ("Movies Imperial", "Movies Imperial", "movies-imperial", "California", "Imperial"),
    ("Calexico 10 Theatre", "Calexico 10 Theatre", "calexico-10-theatre", "California", "Calexico")
  ))
  private def p_el_dorado: R = ("el-dorado", "el-dorado-arkansas", "El Dorado", "Arkansas", 33.2076, -92.6663, "America/Chicago", false, Seq("El Dorado"), Seq(
    ("Stars Theater El Dorado", "Stars Theater El Dorado", "stars-theater-el-dorado", "Arkansas", "El Dorado")
  ))
  private def p_el_dorado_kansas: R = ("el-dorado-kansas", "el-dorado-kansas", "El Dorado", "Kansas", 37.8172, -96.8623, "America/Chicago", true, Seq("El Dorado", "Augusta"), Seq(
    ("Central Cinema El Dorado", "Central Cinema El Dorado", "central-cinema-el-dorado", "Kansas", "El Dorado"),
    ("Augusta Historic Theatre", "Augusta Historic Theatre", "augusta-historic-theatre", "Kansas", "Augusta")
  ))
  private def p_el_dorado_springs: R = ("el-dorado-springs", "el-dorado-springs-missouri", "El Dorado Springs", "Missouri", 37.877, -94.0213, "America/Chicago", false, Seq("El Dorado Springs"), Seq(
    ("Opera House", "Opera House", "opera-house", "Missouri", "El Dorado Springs")
  ))
  private def p_el_segundo: R = ("el-segundo", "el-segundo-california", "El Segundo", "California", 33.9192, -118.4165, "America/Los_Angeles", true, Seq("El Segundo", "Marina del Rey", "South Gate", "Carson", "Redondo Beach", "Gardena", "Huntington Park", "Rancho Palos Verdes", "Culver City", "West Hollywood", "View Park-Windsor Hills", "Rolling Hills Estates"), Seq(
    ("AMC DINE-IN Theatres Marina Marina Del Rey 6", "AMC DINE-IN Theatres Marina Marina Del Rey 6", "amc-dine-in-theatres-marina-marina-del-rey-6", "California", "Marina del Rey"),
    ("AMC Marina Marketplace 6", "AMC Marina Marketplace 6", "amc-marina-marketplace-6", "California", "Marina del Rey"),
    ("AMC South Bay Galleria 16", "AMC South Bay Galleria 16", "amc-south-bay-galleria-16", "California", "Redondo Beach"),
    ("Beach Cities Cinemas El Segundo", "Beach Cities Cinemas El Segundo", "beach-cities-cinemas-el-segundo", "California", "El Segundo"),
    ("Old Town Music Hall El Segundo", "Old Town Music Hall El Segundo", "old-town-music-hall-el-segundo", "California", "El Segundo"),
    ("Cinemark Baldwin Hills", "Cinemark Baldwin Hills", "cinemark-baldwin-hills", "California", "Baldwin Hills"),
    ("Cinemark Century Carson", "Cinemark Century Carson", "cinemark-century-carson", "California", "Carson"),
    ("Culver Theater", "Culver Theater", "culver-theater", "California", "Culver City"),
    ("Gardena Cinema", "Gardena Cinema", "gardena-cinema", "California", "Gardena"),
    ("Landmark Theaters Sunset", "Landmark Theaters Sunset", "landmark-theaters-sunset", "California", "West Hollywood"),
    ("Park Twin Huntington Park", "Park Twin Huntington Park", "park-twin-huntington-park", "California", "Huntington Park"),
    ("Regal Promenade Rolling Hills Estates", "Regal Promenade Rolling Hills Estates", "regal-promenade-rolling-hills-estates", "California", "Rolling Hills Estates"),
    ("Regal South Gate", "Regal South Gate", "regal-south-gate", "California", "South Gate"),
    ("Starlight Terrace Cinemas 6", "Starlight Terrace Cinemas 6", "starlight-terrace-cinemas-6", "California", "Rancho Palos Verdes")
  ))
  private def p_elberton: R = ("elberton", "elberton-georgia", "Elberton", "Georgia", 34.1116, -82.8686, "America/New_York", false, Seq("Elberton"), Seq(
    ("Elbert Theatre Elberton", "Elbert Theatre Elberton", "elbert-theatre-elberton", "Georgia", "Elberton")
  ))
  private def p_elizabeth_city: R = ("elizabeth-city", "elizabeth-city-north-carolina", "Elizabeth City", "North Carolina", 36.2946, -76.2511, "America/New_York", false, Seq("Elizabeth City"), Seq(
    ("R/C Albemarle Movies 8", "R/C Albemarle Movies 8", "rc-albemarle-movies-8", "North Carolina", "Elizabeth City")
  ))
  private def p_elizabethtown: R = ("elizabethtown", "elizabethtown-kentucky", "Elizabethtown", "Kentucky", 37.694, -85.8591, "America/New_York", false, Seq("Elizabethtown"), Seq(
    ("Crowne Pointe Theatre 9", "Crowne Pointe Theatre 9", "crowne-pointe-theatre-9", "Kentucky", "Elizabethtown"),
    ("Historic State Theater Elizabethtown", "Historic State Theater Elizabethtown", "historic-state-theater-elizabethtown", "Kentucky", "Elizabethtown")
  ))
  private def p_elk_city: R = ("elk-city", "elk-city-oklahoma", "Elk City", "Oklahoma", 35.412, -99.4043, "America/Chicago", false, Seq("Elk City"), Seq(
    ("Flix On 6 Cinema Elk City", "Flix On 6 Cinema Elk City", "flix-on-6-cinema-elk-city", "Oklahoma", "Elk City")
  ))
  private def p_elk_rapids: R = ("elk-rapids", "elk-rapids-michigan", "Elk Rapids", "Michigan", 44.8956, -85.4165, "America/Detroit", true, Seq("Elk Rapids", "Bellaire", "Suttons Bay"), Seq(
    ("Bay Theatre Suttons Bay", "Bay Theatre Suttons Bay", "bay-theatre-suttons-bay", "Michigan", "Suttons Bay"),
    ("Bellaire Cinema", "Bellaire Cinema", "bellaire-cinema", "Michigan", "Bellaire"),
    ("Elk Rapids Cinema", "Elk Rapids Cinema", "elk-rapids-cinema", "Michigan", "Elk Rapids")
  ))
  private def p_elkader: R = ("elkader", "elkader-iowa", "Elkader", "Iowa", 42.8539, -91.4054, "America/Chicago", false, Seq("Elkader"), Seq(
    ("Elkader Cinema", "Elkader Cinema", "elkader-cinema", "Iowa", "Elkader")
  ))
  private def p_elkhart: R = ("elkhart", "elkhart-kansas", "Elkhart", "Kansas", 37.0081, -101.8902, "America/Chicago", false, Seq("Elkhart"), Seq(
    ("Doric Theatre Elkhart", "Doric Theatre Elkhart", "doric-theatre-elkhart", "Kansas", "Elkhart")
  ))
  private def p_elkins: R = ("elkins", "elkins-west-virginia", "Elkins", "West Virginia", 38.9259, -79.8467, "America/New_York", false, Seq("Elkins"), Seq(
    ("Elkins Cinema 8", "Elkins Cinema 8", "elkins-cinema-8", "West Virginia", "Elkins")
  ))
  private def p_elko: R = ("elko", "elko-nevada", "Elko", "Nevada", 40.8324, -115.7631, "America/Los_Angeles", false, Seq("Elko"), Seq(
    ("UEC Theatres Crystal 5", "UEC Theatres Crystal 5", "uec-theatres-crystal-5", "Nevada", "Elko"),
    ("UEC Theatres Elko 6", "UEC Theatres Elko 6", "uec-theatres-elko-6", "Nevada", "Elko")
  ))
  private def p_ellensburg: R = ("ellensburg", "ellensburg-washington", "Ellensburg", "Washington", 46.9965, -120.5478, "America/Los_Angeles", false, Seq("Ellensburg"), Seq(
    ("Grand Meridian Ellensburg", "Grand Meridian Ellensburg", "grand-meridian-ellensburg", "Washington", "Ellensburg")
  ))
  private def p_ellijay: R = ("ellijay", "ellijay-georgia", "Ellijay", "Georgia", 34.6948, -84.4822, "America/New_York", true, Seq("Ellijay", "Blue Ridge"), Seq(
    ("GTC Mountain Cinemas East Ellijay", "GTC Mountain Cinemas East Ellijay", "gtc-mountain-cinemas-east-ellijay", "Georgia", "East Ellijay"),
    ("Swan Drive-in Blue Ridge", "Swan Drive-in Blue Ridge", "swan-drive-in-blue-ridge", "Georgia", "Blue Ridge")
  ))
  private def p_elroy: R = ("elroy", "elroy-wisconsin", "Elroy", "Wisconsin", 43.7408, -90.2724, "America/Chicago", false, Seq("Elroy"), Seq(
    ("Elroy Theatre", "Elroy Theatre", "elroy-theatre", "Wisconsin", "Elroy")
  ))
  private def p_ely: R = ("ely", "ely-nevada", "Ely", "Nevada", 39.2474, -114.8886, "America/Los_Angeles", false, Seq("Ely"), Seq(
    ("Central Theatre Ely", "Central Theatre Ely", "central-theatre-ely", "Nevada", "Ely")
  ))
  private def p_ely_minnesota: R = ("ely-minnesota", "ely-minnesota", "Ely", "Minnesota", 47.9032, -91.8671, "America/Chicago", false, Seq("Ely"), Seq(
    ("Ely's Historic State Theater", "Ely's Historic State Theater", "elys-historic-state-theater", "Minnesota", "Ely")
  ))
  private def p_emeryville: R = ("emeryville", "emeryville-california", "Emeryville", "California", 37.8313, -122.2853, "America/Los_Angeles", true, Seq("Emeryville", "San Leandro", "Alameda", "San Bruno", "Orinda", "Moraga"), Seq(
    ("Alameda Theatre & Cineplex", "Alameda Theatre & Cineplex", "alameda-theatre-cineplex", "California", "Alameda"),
    ("AMC Bay Street 16 Emeryville", "AMC Bay Street 16 Emeryville", "amc-bay-street-16-emeryville", "California", "Emeryville"),
    ("Pixar Steve Jobs Theatre", "Pixar Steve Jobs Theatre", "pixar-steve-jobs-theatre", "California", "Emeryville"),
    ("Cinemark Century at Tanforan", "Cinemark Century at Tanforan", "cinemark-century-at-tanforan", "California", "San Bruno"),
    ("Cinemark Century Bayfair Mall 16", "Cinemark Century Bayfair Mall 16", "cinemark-century-bayfair-mall-16", "California", "San Leandro"),
    ("Orinda Theatre", "Orinda Theatre", "orinda-theatre", "California", "Orinda"),
    ("Rheem Theatre Moraga", "Rheem Theatre Moraga", "rheem-theatre-moraga", "California", "Moraga")
  ))
  private def p_emmett: R = ("emmett", "emmett-idaho", "Emmett", "Idaho", 43.8735, -116.4993, "America/Boise", false, Seq("Emmett"), Seq(
    ("Frontier Cinema", "Frontier Cinema", "frontier-cinema", "Idaho", "Emmett"),
    ("Frontier Drive-In at the Mill", "Frontier Drive-In at the Mill", "frontier-drive-in-at-the-mill", "Idaho", "Emmett")
  ))
  private def p_emporia: R = ("emporia", "emporia-kansas", "Emporia", "Kansas", 38.4039, -96.1817, "America/Chicago", false, Seq("Emporia"), Seq(
    ("Golden Ticket Cinemas Flinthills 8", "Golden Ticket Cinemas Flinthills 8", "bb-emporia-flinthills-8", "Kansas", "Emporia")
  ))
  private def p_enterprise: R = ("enterprise", "enterprise-alabama", "Enterprise", "Alabama", 31.3152, -85.8552, "America/Chicago", true, Seq("Enterprise", "Newton"), Seq(
    ("Clark Cinema Enterprise 10", "Clark Cinema Enterprise 10", "clark-cinema-enterprise-10", "Alabama", "Enterprise"),
    ("VIP Star-Lite Drive-In", "VIP Star-Lite Drive-In", "vip-star-lite-drive-in", "Alabama", "Newton")
  ))
  private def p_ephraim: R = ("ephraim", "ephraim-utah", "Ephraim", "Utah", 39.3597, -111.5863, "America/Denver", false, Seq("Ephraim"), Seq(
    ("Southtowne Twin Ephraim", "Southtowne Twin Ephraim", "southtowne-twin-ephraim", "Utah", "Ephraim"),
    ("Towne Theatre Ephraim", "Towne Theatre Ephraim", "towne-theatre-ephraim", "Utah", "Ephraim")
  ))
  private def p_erie: R = ("erie", "erie-pennsylvania", "Erie", "Pennsylvania", 42.1292, -80.0851, "America/New_York", false, Seq("Erie"), Seq(
    ("Cinemark Tinseltown Erie", "Cinemark Tinseltown Erie", "cinemark-tinseltown-erie", "Pennsylvania", "Erie"),
    ("TREC Big Green Screen Theater", "TREC Big Green Screen Theater", "trec-big-green-screen-theater", "Pennsylvania", "Erie")
  ))
  private def p_escanaba: R = ("escanaba", "escanaba-michigan", "Escanaba", "Michigan", 45.7452, -87.0646, "America/Detroit", false, Seq("Escanaba"), Seq(
    ("Willow Creek Cinema 8 Escanaba", "Willow Creek Cinema 8 Escanaba", "willow-creek-cinema-8-escanaba", "Michigan", "Escanaba"),
    ("Old Michigan Cinema", "Old Michigan Cinema", "old-michigan-cinema", "Michigan", "Escanaba")
  ))
  private def p_estes_park: R = ("estes-park", "estes-park-colorado", "Estes Park", "Colorado", 40.3772, -105.5217, "America/Denver", false, Seq("Estes Park"), Seq(
    ("Historic Park Theatre", "Historic Park Theatre", "historic-park-theatre", "Colorado", "Estes Park"),
    ("Reel Mountain Theater Estes Park", "Reel Mountain Theater Estes Park", "reel-mountain-theater-estes-park", "Colorado", "Estes Park")
  ))
  private def p_eunice: R = ("eunice", "eunice-louisiana", "Eunice", "Louisiana", 30.4944, -92.4176, "America/Chicago", false, Seq("Eunice"), Seq(
    ("Queen Cinema Eunice", "Queen Cinema Eunice", "queen-cinema-eunice", "Louisiana", "Eunice")
  ))
  private def p_eureka: R = ("eureka", "eureka-california", "Eureka", "California", 40.8021, -124.1637, "America/Los_Angeles", true, Seq("Eureka", "Arcata"), Seq(
    ("Arcata Theatre Lounge", "Arcata Theatre Lounge", "arcata-theatre-lounge", "California", "Arcata"),
    ("Minor Theatre Arcata", "Minor Theatre Arcata", "minor-theatre-arcata", "California", "Arcata"),
    ("Broadway Cinema Eureka", "Broadway Cinema Eureka", "broadway-cinema-eureka", "California", "Eureka"),
    ("Eureka Theater", "Eureka Theater", "eureka-theater", "California", "Eureka")
  ))
  private def p_eureka_montana: R = ("eureka-montana", "eureka-montana", "Eureka", "Montana", 48.88, -115.0535, "America/Denver", false, Seq("Eureka"), Seq(
    ("Majestic Theater Eureka", "Majestic Theater Eureka", "majestic-theater-eureka", "Montana", "Eureka")
  ))
  private def p_evanston: R = ("evanston", "evanston-illinois", "Evanston", "Illinois", 42.0411, -87.6901, "America/Chicago", true, Seq("Evanston", "Skokie", "Northbrook", "Niles", "Oak Park", "Glenview", "Park Ridge", "Highland Park", "Wilmette", "Lake Forest", "Norridge", "Rosemont"), Seq(
    ("AMC Niles 12", "AMC Niles 12", "amc-niles-12", "Illinois", "Niles"),
    ("Cine Lounge at Niles", "Cine Lounge at Niles", "cine-lounge-at-niles", "Illinois", "Niles"),
    ("AMC Norridge 6", "AMC Norridge 6", "amc-norridge-6", "Illinois", "Norridge"),
    ("AMC Northbrook Court 14", "AMC Northbrook Court 14", "amc-northbrook-court-14", "Illinois", "Northbrook"),
    ("Northbrook Public Library", "Northbrook Public Library", "northbrook-public-library", "Illinois", "Northbrook"),
    ("AMC Rosemont 12", "AMC Rosemont 12", "amc-rosemont-12", "Illinois", "Rosemont"),
    ("AMC Village Crossing 18", "AMC Village Crossing 18", "amc-village-crossing-18", "Illinois", "Skokie"),
    ("CMX CinéBistro Old Orchard", "CMX CinéBistro Old Orchard", "cmx-cinbistro-old-orchard", "Illinois", "Skokie"),
    ("Classic Cinemas Lake Oak Park", "Classic Cinemas Lake Oak Park", "classic-cinemas-lake-oak-park", "Illinois", "Oak Park"),
    ("Landmark at The Glen", "Landmark at The Glen", "landmark-at-the-glen", "Illinois", "Glenview"),
    ("Pickwick Theatre Park Ridge", "Pickwick Theatre Park Ridge", "pickwick-theatre-park-ridge", "Illinois", "Park Ridge"),
    ("Wayfarer Theaters Highland Park", "Wayfarer Theaters Highland Park", "wayfarer-theaters-highland-park", "Illinois", "Highland Park"),
    ("Wilmette Theatre", "Wilmette Theatre", "wilmette-theatre", "Illinois", "Wilmette"),
    ("AMC Evanston 12", "AMC Evanston 12", "amc-evanston-12", "Illinois", "Evanston"),
    ("Block Cinema - Northwestern University", "Block Cinema - Northwestern University", "block-cinema---northwestern-university", "Illinois", "Evanston"),
    ("Gorton Center Lake Forest", "Gorton Center Lake Forest", "gorton-center-lake-forest", "Illinois", "Lake Forest")
  ))
  private def p_evanston_wyoming: R = ("evanston-wyoming", "evanston-wyoming", "Evanston", "Wyoming", 41.2683, -110.9632, "America/Denver", false, Seq("Evanston"), Seq(
    ("Aspen Cinemas Evanston", "Aspen Cinemas Evanston", "aspen-cinemas-evanston", "Wyoming", "Evanston"),
    ("The Strand Evanston", "The Strand Evanston", "the-strand-evanston", "Wyoming", "Evanston")
  ))
  private def p_fairbanks: R = ("fairbanks", "fairbanks-alaska", "Fairbanks", "Alaska", 64.8378, -147.7164, "America/Anchorage", false, Seq("Fairbanks"), Seq(
    ("Regal Goldstream", "Regal Goldstream", "regal-goldstream", "Alaska", "Fairbanks")
  ))
  private def p_fairbury: R = ("fairbury", "fairbury-nebraska", "Fairbury", "Nebraska", 40.1372, -97.1806, "America/Chicago", false, Seq("Fairbury"), Seq(
    ("Bonham Theatre", "Bonham Theatre", "bonham-theatre", "Nebraska", "Fairbury")
  ))
  private def p_fairfax: R = ("fairfax", "fairfax-virginia", "Fairfax", "Virginia", 38.8462, -77.3064, "America/New_York", false, Seq("Fairfax"), Seq(
    ("Angelika Film Center at Mosaic", "Angelika Film Center at Mosaic", "angelika-film-center-at-mosaic", "Virginia", "Fairfax"),
    ("Cinema Arts Fairfax 6", "Cinema Arts Fairfax 6", "cinema-arts-fairfax-6", "Virginia", "Fairfax"),
    ("Cinemark Fairfax Corner 14", "Cinemark Fairfax Corner 14", "cinemark-fairfax-corner-14", "Virginia", "Fairfax"),
    ("Regal Fairfax Towne Center", "Regal Fairfax Towne Center", "regal-fairfax-towne-center", "Virginia", "Fairfax"),
    ("University Mall Fairfax", "University Mall Fairfax", "university-mall-fairfax", "Virginia", "Fairfax")
  ))
  private def p_fairmont: R = ("fairmont", "fairmont-minnesota", "Fairmont", "Minnesota", 43.6522, -94.4611, "America/Chicago", true, Seq("Fairmont", "Sherburn"), Seq(
    ("Fairmont Theatre 5", "Fairmont Theatre 5", "fairmont-theatre-5", "Minnesota", "Fairmont"),
    ("Sherburn Theatre", "Sherburn Theatre", "sherburn-theatre", "Minnesota", "Sherburn")
  ))
  private def p_fairview: R = ("fairview", "fairview-oklahoma", "Fairview", "Oklahoma", 36.2689, -98.4798, "America/Chicago", false, Seq("Fairview"), Seq(
    ("Royal Theatre Foundation Fairview", "Royal Theatre Foundation Fairview", "royal-theatre-foundation-fairview", "Oklahoma", "Fairview")
  ))
  private def p_fajardo: R = ("fajardo", "fajardo-puerto-rico", "Fajardo", "Puerto Rico", 18.3258, -65.6524, "America/Puerto_Rico", true, Seq("Fajardo", "Canovanas", "Las Piedras"), Seq(
    ("Caribbean Cinemas Fajardo", "Caribbean Cinemas Fajardo", "caribbean-cinemas-fajardo", "Puerto Rico", "Fajardo"),
    ("Caribbean Cinemas Las Piedras", "Caribbean Cinemas Las Piedras", "caribbean-cinemas-las-piedras", "Puerto Rico", "Las Piedras"),
    ("Caribbean Cinemas The Outlet 66", "Caribbean Cinemas The Outlet 66", "caribbean-cinemas-the-outlet-66", "Puerto Rico", "Canóvanas")
  ))
  private def p_fall_river: R = ("fall-river", "fall-river-massachusetts", "Fall River", "Massachusetts", 41.7015, -71.1551, "America/New_York", true, Seq("Fall River", "Newport", "Smith Mills"), Seq(
    ("JPT Film & Event Center", "JPT Film & Event Center", "jpt-film-event-center", "Rhode Island", "Newport"),
    ("AMC Dartmouth Mall 11", "AMC Dartmouth Mall 11", "amc-dartmouth-mall-11", "Massachusetts", "North Dartmouth"),
    ("Picture Show at SouthCoast Marketplace", "Picture Show at SouthCoast Marketplace", "picture-show-at-southcoast-marketplace", "Massachusetts", "Fall River")
  ))
  private def p_fallon: R = ("fallon", "fallon-nevada", "Fallon", "Nevada", 39.4735, -118.7774, "America/Los_Angeles", false, Seq("Fallon"), Seq(
    ("Fox Peak Cinema", "Fox Peak Cinema", "fox-peak-cinema", "Nevada", "Fallon"),
    ("Fallon Theatre", "Fallon Theatre", "fallon-theatre", "Nevada", "Fallon")
  ))
  private def p_farmington: R = ("farmington", "farmington-new-mexico", "Farmington", "New Mexico", 36.7281, -108.2187, "America/Denver", false, Seq("Farmington"), Seq(
    ("Allen Farmington 8", "Allen Farmington 8", "allen-farmington-8", "New Mexico", "Farmington"),
    ("Animas Farmington 10", "Animas Farmington 10", "animas-farmington-10", "New Mexico", "Farmington")
  ))
  private def p_farmington_maine: R = ("farmington-maine", "farmington-maine", "Farmington", "Maine", 44.6706, -70.1512, "America/New_York", false, Seq("Farmington"), Seq(
    ("Narrow Gauge Cinemas", "Narrow Gauge Cinemas", "narrow-gauge-cinemas", "Maine", "Farmington"),
    ("Narrow Gauge Drive-In", "Narrow Gauge Drive-In", "narrow-gauge-drive-in", "Maine", "Farmington")
  ))
  private def p_farmington_missouri: R = ("farmington-missouri", "farmington-missouri", "Farmington", "Missouri", 37.7809, -90.4218, "America/Chicago", false, Seq("Farmington"), Seq(
    ("AMC CLASSIC Farmington 4", "AMC CLASSIC Farmington 4", "amc-classic-farmington-4", "Missouri", "Farmington")
  ))
  private def p_farmville: R = ("farmville", "farmville-virginia", "Farmville", "Virginia", 37.3021, -78.3919, "America/New_York", false, Seq("Farmville"), Seq(
    ("Sunchase Cinema 8", "Sunchase Cinema 8", "sunchase-cinema-8", "Virginia", "Farmville")
  ))
  private def p_faulkton: R = ("faulkton", "faulkton-south-dakota", "Faulkton", "South Dakota", 45.035, -99.124, "America/Chicago", false, Seq("Faulkton"), Seq(
    ("Lyric Theatre Faulkton", "Lyric Theatre Faulkton", "lyric-theatre-faulkton", "South Dakota", "Faulkton")
  ))
  private def p_fayetteville_arkansas: R = ("fayetteville-arkansas", "fayetteville-arkansas", "Fayetteville", "Arkansas", 36.0626, -94.1574, "America/Chicago", true, Seq("Fayetteville", "Springdale", "Siloam Springs"), Seq(
    ("AMC Fiesta Square 12", "AMC Fiesta Square 12", "amc-fiesta-square-12", "Arkansas", "Fayetteville"),
    ("Malco Razorback Cinema Grill", "Malco Razorback Cinema Grill", "malco-razorback-cinema-grill", "Arkansas", "Fayetteville"),
    ("Malco Springdale Cinema Grill", "Malco Springdale Cinema Grill", "malco-springdale-cinema-grill", "Arkansas", "Springdale"),
    ("Center Cinema Siloam Springs 6", "Center Cinema Siloam Springs 6", "center-cinema-siloam-springs-6", "Arkansas", "Siloam Springs")
  ))
  private def p_fayetteville_georgia: R = ("fayetteville-georgia", "fayetteville-georgia", "Fayetteville", "Georgia", 33.4487, -84.4549, "America/New_York", true, Seq("Fayetteville", "Newnan", "Peachtree City", "Morrow"), Seq(
    ("AMC Southlake 24", "AMC Southlake 24", "amc-southlake-24", "Georgia", "Morrow"),
    ("Cinemark Tinseltown Fayetteville", "Cinemark Tinseltown Fayetteville", "cinemark-tinseltown-fayetteville", "Georgia", "Fayetteville"),
    ("GTC Trilith Cinemas", "GTC Trilith Cinemas", "gtc-trilith-cinemas", "Georgia", "Fayetteville"),
    ("NCG Peachtree City", "NCG Peachtree City", "ncg-peachtree-city", "Georgia", "Sharpsburg"),
    ("Regal Georgian", "Regal Georgian", "regal-georgian", "Georgia", "Newnan")
  ))
  private def p_fergus_falls: R = ("fergus-falls", "fergus-falls-minnesota", "Fergus Falls", "Minnesota", 46.283, -96.0776, "America/Chicago", false, Seq("Fergus Falls"), Seq(
    ("Westridge Cinema 5 Fergus Falls", "Westridge Cinema 5 Fergus Falls", "westridge-cinema-5-fergus-falls", "Minnesota", "Fergus Falls")
  ))
  private def p_findlay: R = ("findlay", "findlay-ohio", "Findlay", "Ohio", 41.0442, -83.6499, "America/New_York", true, Seq("Findlay", "Upper Sandusky", "Bluffton", "North Baltimore"), Seq(
    ("AMC CLASSIC Findlay 12", "AMC CLASSIC Findlay 12", "amc-classic-findlay-12", "Ohio", "Findlay"),
    ("Virginia Motion Pictures Baltimore", "Virginia Motion Pictures Baltimore", "virginia-motion-pictures-baltimore", "Ohio", "North Baltimore"),
    ("Shannon Cinema Bluffton", "Shannon Cinema Bluffton", "shannon-cinema-bluffton", "Ohio", "Bluffton"),
    ("Star Theatre Upper Sandusky", "Star Theatre Upper Sandusky", "star-theatre-upper-sandusky", "Ohio", "Upper Sandusky")
  ))
  private def p_fish_creek: R = ("fish-creek", "fish-creek-wisconsin", "Fish Creek", "Wisconsin", 45.1382, -87.1988, "America/Chicago", false, Seq("Fish Creek"), Seq(
    ("Skyway Drive-In Fish Creek", "Skyway Drive-In Fish Creek", "skyway-drive-in-fish-creek", "Wisconsin", "Fish Creek")
  ))
  private def p_fitchburg: R = ("fitchburg", "fitchburg-wisconsin", "Fitchburg", "Wisconsin", 42.9608, -89.4698, "America/Chicago", false, Seq("Fitchburg"), Seq(
    ("AMC Fitchburg 18", "AMC Fitchburg 18", "amc-fitchburg-18", "Wisconsin", "Fitchburg")
  ))
  private def p_fitzgerald: R = ("fitzgerald", "fitzgerald-georgia", "Fitzgerald", "Georgia", 31.7149, -83.2527, "America/New_York", false, Seq("Fitzgerald"), Seq(
    ("Grand Theatre Fitzgerald", "Grand Theatre Fitzgerald", "grand-theatre-fitzgerald", "Georgia", "Fitzgerald")
  ))
  private def p_flagler: R = ("flagler", "flagler-colorado", "Flagler", "Colorado", 39.2928, -103.0672, "America/Denver", false, Seq("Flagler"), Seq(
    ("Flagler Theatre", "Flagler Theatre", "flagler-theatre", "Colorado", "Flagler")
  ))
  private def p_flagstaff: R = ("flagstaff", "flagstaff-arizona", "Flagstaff", "Arizona", 35.1981, -111.6513, "America/Phoenix", false, Seq("Flagstaff"), Seq(
    ("Harkins Flagstaff 16", "Harkins Flagstaff 16", "harkins-flagstaff-16", "Arizona", "Flagstaff")
  ))
  private def p_flint: R = ("flint", "flint-michigan", "Flint", "Michigan", 43.0125, -83.6875, "America/Detroit", false, Seq("Flint"), Seq(
    ("Cinemark Flint West 14", "Cinemark Flint West 14", "cinemark-flint-west-14", "Michigan", "Flint"),
    ("US 23 Drive In Theatre", "US 23 Drive In Theatre", "us-23-drive-in-theatre", "Michigan", "Flint"),
    ("Flint Institute of Art", "Flint Institute of Art", "flint-institute-of-art", "Michigan", "Flint")
  ))
  private def p_florence: R = ("florence", "florence-south-carolina", "Florence", "South Carolina", 34.1954, -79.7626, "America/New_York", false, Seq("Florence"), Seq(
    ("Julia Cinemas Florence", "Julia Cinemas Florence", "julia-cinemas-florence", "South Carolina", "Florence"),
    ("Regal Swamp Fox", "Regal Swamp Fox", "regal-swamp-fox", "South Carolina", "Florence")
  ))
  private def p_florence_alabama: R = ("florence-alabama", "florence-alabama", "Florence", "Alabama", 34.7998, -87.6773, "America/Chicago", false, Seq("Florence"), Seq(
    ("AMC CLASSIC Florence 12", "AMC CLASSIC Florence 12", "amc-classic-florence-12", "Alabama", "Florence")
  ))
  private def p_florence_kentucky: R = ("florence-kentucky", "florence-kentucky", "Florence", "Kentucky", 38.999, -84.6266, "America/New_York", true, Seq("Florence", "Newport", "Greendale", "Wilder"), Seq(
    ("AMC Newport on the Levee 20", "AMC Newport on the Levee 20", "amc-newport-on-the-levee-20", "Kentucky", "Newport"),
    ("Cinemark Florence 14", "Cinemark Florence 14", "cinemark-florence-14", "Kentucky", "Florence"),
    ("Greendale Cinema", "Greendale Cinema", "greendale-cinema", "Indiana", "Greendale"),
    ("Regal Wilder", "Regal Wilder", "regal-wilder", "Kentucky", "Wilder")
  ))
  private def p_florence_oregon: R = ("florence-oregon", "florence-oregon", "Florence", "Oregon", 43.9826, -124.0998, "America/Los_Angeles", false, Seq("Florence"), Seq(
    ("City Lights Cinemas Florence", "City Lights Cinemas Florence", "city-lights-cinemas-florence", "Oregon", "Florence")
  ))
  private def p_floresville: R = ("floresville", "floresville-texas", "Floresville", "Texas", 29.1336, -98.1561, "America/Chicago", false, Seq("Floresville"), Seq(
    ("Arcadia Theatre Floresville", "Arcadia Theatre Floresville", "arcadia-theatre-floresville", "Texas", "Floresville")
  ))
  private def p_folsom: R = ("folsom", "folsom-california", "Folsom", "California", 38.678, -121.1761, "America/Los_Angeles", true, Seq("Folsom", "Rocklin", "El Dorado Hills", "Placerville"), Seq(
    ("Cinemark Century Blue Oaks", "Cinemark Century Blue Oaks", "cinemark-century-blue-oaks", "California", "Rocklin"),
    ("Studio Movie Grill Rocklin", "Studio Movie Grill Rocklin", "studio-movie-grill-rocklin", "California", "Rocklin"),
    ("Cinemark Century Folsom 14", "Cinemark Century Folsom 14", "cinemark-century-folsom-14", "California", "Folsom"),
    ("Palladio Folsom", "Palladio Folsom", "palladio-folsom", "California", "Folsom"),
    ("Placerville Cinema", "Placerville Cinema", "placerville-cinema", "California", "Placerville"),
    ("Regal El Dorado Hills", "Regal El Dorado Hills", "regal-el-dorado-hills", "California", "El Dorado Hills")
  ))
  private def p_fond_du_lac: R = ("fond-du-lac", "fond-du-lac-wisconsin", "Fond du Lac", "Wisconsin", 43.775, -88.4388, "America/Chicago", true, Seq("Fond du Lac", "Ripon", "North Fond du Lac"), Seq(
    ("Marcus Campus Ripon", "Marcus Campus Ripon", "marcus-campus-ripon", "Wisconsin", "Ripon"),
    ("Odyssey Cinemas Fond Du Lac", "Odyssey Cinemas Fond Du Lac", "odyssey-cinemas-fond-du-lac", "Wisconsin", "Fond du Lac"),
    ("North Fond Du Lac School District Theater", "North Fond Du Lac School District Theater", "north-fond-du-lac-school-district-theater", "Wisconsin", "North Fond du Lac")
  ))
  private def p_forest_city: R = ("forest-city", "forest-city-iowa", "Forest City", "Iowa", 43.2625, -93.6372, "America/Chicago", true, Seq("Forest City", "Garner"), Seq(
    ("Avery Theater", "Avery Theater", "avery-theater", "Iowa", "Garner"),
    ("Forest Theatre", "Forest Theatre", "forest-theatre", "Iowa", "Forest City")
  ))
  private def p_forest_grove: R = ("forest-grove", "forest-grove-oregon", "Forest Grove", "Oregon", 45.5198, -123.1107, "America/Los_Angeles", true, Seq("Forest Grove", "Cornelius"), Seq(
    ("Cornelius Cinemas", "Cornelius Cinemas", "cornelius-cinemas", "Oregon", "Cornelius"),
    ("Grand Lodge", "Grand Lodge", "grand-lodge", "Oregon", "Forest Grove")
  ))
  private def p_forney: R = ("forney", "forney-texas", "Forney", "Texas", 32.7482, -96.4719, "America/Chicago", true, Seq("Forney", "Terrell"), Seq(
    ("AMC CLASSIC Forney 12", "AMC CLASSIC Forney 12", "amc-classic-forney-12", "Texas", "Forney"),
    ("Film Alley Terrell", "Film Alley Terrell", "film-alley-terrell", "Texas", "Terrell Hills")
  ))
  private def p_forrest_city: R = ("forrest-city", "forrest-city-arkansas", "Forrest City", "Arkansas", 35.0082, -90.7898, "America/Chicago", false, Seq("Forrest City"), Seq(
    ("Broadway Cinema Forrest City", "Broadway Cinema Forrest City", "broadway-cinema-forrest-city", "Arkansas", "Forrest City")
  ))
  private def p_forsyth: R = ("forsyth", "forsyth-montana", "Forsyth", "Montana", 46.2664, -106.6778, "America/Denver", false, Seq("Forsyth"), Seq(
    ("Roxy Theatre Forsyth", "Roxy Theatre Forsyth", "roxy-theatre-forsyth", "Montana", "Forsyth")
  ))
  private def p_fort_bliss: R = ("fort-bliss", "fort-bliss-texas", "Fort Bliss", "Texas", 31.8136, -106.4122, "America/Denver", false, Seq("Fort Bliss"), Seq(
    ("Santikos Grand Fort Bliss", "Santikos Grand Fort Bliss", "santikos-grand-fort-bliss", "Texas", "Fort Bliss")
  ))
  private def p_fort_bragg: R = ("fort-bragg", "fort-bragg-california", "Fort Bragg", "California", 39.4457, -123.8053, "America/Los_Angeles", false, Seq("Fort Bragg"), Seq(
    ("Coast Cinemas Fort Bragg", "Coast Cinemas Fort Bragg", "coast-cinemas-fort-bragg", "California", "Fort Bragg")
  ))
  private def p_fort_dodge: R = ("fort-dodge", "fort-dodge-iowa", "Fort Dodge", "Iowa", 42.4975, -94.168, "America/Chicago", true, Seq("Fort Dodge", "Webster City", "Humboldt"), Seq(
    ("Humota Theater", "Humota Theater", "humota-theater", "Iowa", "Humboldt"),
    ("Webster Theatre Webster City", "Webster Theatre Webster City", "webster-theatre-webster-city", "Iowa", "Webster City"),
    ("Fort 8 Theatre", "Fort 8 Theatre", "fort-8-theatre", "Iowa", "Fort Dodge")
  ))
  private def p_fort_lee: R = ("fort-lee", "fort-lee-new-jersey", "Fort Lee", "New Jersey", 40.8509, -73.9701, "America/New_York", true, Seq("Fort Lee", "Westwood", "New Rochelle", "Hackensack", "Teaneck", "Secaucus", "Ridgefield Park", "Dobbs Ferry", "Closter", "Pelham", "Bronxville"), Seq(
    ("AMC Ridgefield Park 12", "AMC Ridgefield Park 12", "amc-ridgefield-park-12", "New Jersey", "Ridgefield Park"),
    ("AMC Shops at Riverside 9", "AMC Shops at Riverside 9", "amc-shops-at-riverside-9", "New Jersey", "Hackensack"),
    ("Barrymore Film Center", "Barrymore Film Center", "barrymore-film-center", "New Jersey", "Fort Lee"),
    ("IPIC Fort Lee (Hudson Lights)", "IPIC Fort Lee (Hudson Lights)", "ipic-fort-lee-hudson-lights", "New Jersey", "Fort Lee"),
    ("Landmark Closter Plaza", "Landmark Closter Plaza", "landmark-closter-plaza", "New Jersey", "Closter"),
    ("Regal Secaucus Showplace", "Regal Secaucus Showplace", "regal-secaucus-showplace", "New Jersey", "Secaucus"),
    ("Teaneck Cinemas", "Teaneck Cinemas", "teaneck-cinemas", "New Jersey", "Teaneck"),
    ("Westwood Cinemas", "Westwood Cinemas", "westwood-cinemas", "New Jersey", "Westwood"),
    ("Washington Township Cinema", "Washington Township Cinema", "washington-township-cinema", "New Jersey", "Washington Township"),
    ("LOOK Cinemas Dobbs Ferry", "LOOK Cinemas Dobbs Ferry", "look-cinemas-dobbs-ferry", "New York", "Dobbs Ferry"),
    ("Pelham Picture House", "Pelham Picture House", "pelham-picture-house", "New York", "Pelham"),
    ("Picture House Bronxville", "Picture House Bronxville", "picture-house-bronxville", "New York", "Bronxville"),
    ("Regal New Roc City", "Regal New Roc City", "regal-new-roc-city", "New York", "New Rochelle")
  ))
  private def p_fort_moore: R = ("fort-moore", "fort-moore-georgia", "Fort Moore", "Georgia", 32.3596, -84.9625, "America/New_York", false, Seq("Fort Moore"), Seq(
    ("Lucas Cinemas Fort Moore 10", "Lucas Cinemas Fort Moore 10", "lucas-cinemas-fort-moore-10", "Georgia", "Fort Moore")
  ))
  private def p_fort_morgan: R = ("fort-morgan", "fort-morgan-colorado", "Fort Morgan", "Colorado", 40.2503, -103.7999, "America/Denver", true, Seq("Fort Morgan", "Brush"), Seq(
    ("Cover Theatre Ft Morgan", "Cover Theatre Ft Morgan", "cover-theatre-ft-morgan", "Colorado", "Fort Morgan"),
    ("Sands Theatre Brush", "Sands Theatre Brush", "sands-theatre-brush", "Colorado", "Brush")
  ))
  private def p_fort_myers: R = ("fort-myers", "fort-myers-florida", "Fort Myers", "Florida", 26.6217, -81.8406, "America/New_York", false, Seq("Fort Myers"), Seq(
    ("Luxe 8 Flix Cinema", "Luxe 8 Flix Cinema", "luxe-8-flix-cinema", "Florida", "Fort Myers"),
    ("Regal Belltower Fort Myers", "Regal Belltower Fort Myers", "regal-belltower-fort-myers", "Florida", "Fort Myers"),
    ("Regal Gulf Coast Fort Myer", "Regal Gulf Coast Fort Myer", "regal-gulf-coast-fort-myer", "Florida", "Fort Myers")
  ))
  private def p_fort_pierre: R = ("fort-pierre", "fort-pierre-south-dakota", "Fort Pierre", "South Dakota", 44.3536, -100.3737, "America/Chicago", false, Seq("Fort Pierre"), Seq(
    ("State Theatres Pierre", "State Theatres Pierre", "state-theatres-pierre", "South Dakota", "Fort Pierre")
  ))
  private def p_fort_scott: R = ("fort-scott", "fort-scott-kansas", "Fort Scott", "Kansas", 37.8398, -94.7083, "America/Chicago", false, Seq("Fort Scott"), Seq(
    ("Fort Cinema", "Fort Cinema", "fort-cinema", "Kansas", "Fort Scott")
  ))
  private def p_fort_smith: R = ("fort-smith", "fort-smith-arkansas", "Fort Smith", "Arkansas", 35.3859, -94.3986, "America/Chicago", true, Seq("Fort Smith", "Van Buren"), Seq(
    ("Malco Fort Smith 16", "Malco Fort Smith 16", "malco-fort-smith-16", "Arkansas", "Fort Smith"),
    ("Malco Van Buren Cinema", "Malco Van Buren Cinema", "malco-van-buren-cinema", "Arkansas", "Van Buren")
  ))
  private def p_fort_stockton: R = ("fort-stockton", "fort-stockton-texas", "Fort Stockton", "Texas", 30.894, -102.8793, "America/Chicago", false, Seq("Fort Stockton"), Seq(
    ("West Texas Cinema Ft Stockton", "West Texas Cinema Ft Stockton", "west-texas-cinema-ft-stockton", "Texas", "Fort Stockton")
  ))
  private def p_fowler: R = ("fowler", "fowler-indiana", "Fowler", "Indiana", 40.6167, -87.3208, "America/Indiana/Indianapolis", false, Seq("Fowler"), Seq(
    ("Fowler Theatre", "Fowler Theatre", "fowler-theatre", "Indiana", "Fowler")
  ))
  private def p_framingham: R = ("framingham", "framingham-massachusetts", "Framingham", "Massachusetts", 42.2793, -71.4162, "America/New_York", true, Seq("Framingham", "Marlborough", "Natick", "Maynard"), Seq(
    ("AMC DINE-IN Framingham 16", "AMC DINE-IN Framingham 16", "amc-dine-in-framingham-16", "Massachusetts", "Framingham"),
    ("IMAX 3D Natick (Jordan's)", "IMAX 3D Natick (Jordan's)", "imax-3d-natick-jordans", "Massachusetts", "East Natick"),
    ("Maynard Fine Arts Theatre", "Maynard Fine Arts Theatre", "maynard-fine-arts-theatre", "Massachusetts", "Maynard"),
    ("Regal Solomon Pond", "Regal Solomon Pond", "regal-solomon-pond", "Massachusetts", "Marlborough"),
    ("TCAN Center for Arts", "TCAN Center for Arts", "tcan-center-for-arts", "Massachusetts", "Natick")
  ))
  private def p_frankfort: R = ("frankfort", "frankfort-kentucky", "Frankfort", "Kentucky", 38.2009, -84.8733, "America/New_York", false, Seq("Frankfort"), Seq(
    ("Franklin Square Cinema Frankfort", "Franklin Square Cinema Frankfort", "franklin-square-cinema-frankfort", "Kentucky", "Frankfort"),
    ("Grand Theatre Frankfort", "Grand Theatre Frankfort", "grand-theatre-frankfort", "Kentucky", "Frankfort")
  ))
  private def p_frankfort_michigan: R = ("frankfort-michigan", "frankfort-michigan", "Frankfort", "Michigan", 44.6336, -86.2345, "America/Detroit", true, Seq("Frankfort", "Beulah"), Seq(
    ("Cherry Bowl Drive-in Honor", "Cherry Bowl Drive-in Honor", "cherry-bowl-drive-in-honor", "Michigan", "Honor"),
    ("Garden Theater Frankfort", "Garden Theater Frankfort", "garden-theater-frankfort", "Michigan", "Frankfort")
  ))
  private def p_franklin: R = ("franklin", "franklin-tennessee", "Franklin", "Tennessee", 35.9251, -86.8689, "America/Chicago", false, Seq("Franklin"), Seq(
    ("AMC DINE-IN Thoroughbred Franklin", "AMC DINE-IN Thoroughbred Franklin", "amc-dine-in-thoroughbred-franklin", "Tennessee", "Franklin"),
    ("Franklin Theatre", "Franklin Theatre", "franklin-theatre", "Tennessee", "Franklin")
  ))
  private def p_franklin_west_virginia: R = ("franklin-west-virginia", "franklin-west-virginia", "Franklin", "West Virginia", 38.6429, -79.3311, "America/New_York", false, Seq("Franklin"), Seq(
    ("Warner's Drive In", "Warner's Drive In", "warners-drive-in", "West Virginia", "Franklin")
  ))
  private def p_frederick: R = ("frederick", "frederick-maryland", "Frederick", "Maryland", 39.4143, -77.4105, "America/New_York", false, Seq("Frederick"), Seq(
    ("Regal Westview Frederick", "Regal Westview Frederick", "regal-westview-frederick", "Maryland", "Frederick"),
    ("Warehouse Cinemas Frederick", "Warehouse Cinemas Frederick", "warehouse-cinemas-frederick", "Maryland", "Frederick")
  ))
  private def p_fredericksburg: R = ("fredericksburg", "fredericksburg-virginia", "Fredericksburg", "Virginia", 38.3032, -77.4605, "America/New_York", false, Seq("Fredericksburg"), Seq(
    ("Hooky Entertainment Fredericksburg", "Hooky Entertainment Fredericksburg", "hooky-entertainment-fredericksburg", "Virginia", "Fredericksburg"),
    ("Regal Fredericksburg", "Regal Fredericksburg", "regal-fredericksburg", "Virginia", "Fredericksburg")
  ))
  private def p_freeport: R = ("freeport", "freeport-illinois", "Freeport", "Illinois", 42.2967, -89.6212, "America/Chicago", false, Seq("Freeport"), Seq(
    ("Classic Cinemas Lindo Freeport", "Classic Cinemas Lindo Freeport", "classic-cinemas-lindo-freeport", "Illinois", "Freeport")
  ))
  private def p_fremont_nebraska: R = ("fremont-nebraska", "fremont-nebraska", "Fremont", "Nebraska", 41.4333, -96.4981, "America/Chicago", false, Seq("Fremont"), Seq(
    ("Fremont Theatres Nebraska", "Fremont Theatres Nebraska", "fremont-theatres-nebraska", "Nebraska", "Fremont"),
    ("Quasar Drive-In Fremont", "Quasar Drive-In Fremont", "quasar-drive-in-fremont", "Nebraska", "Fremont")
  ))
  private def p_friday_harbor: R = ("friday-harbor", "friday-harbor-washington", "Friday Harbor", "Washington", 48.5343, -123.0171, "America/Los_Angeles", true, Seq("Friday Harbor", "Eastsound"), Seq(
    ("Palace Theatre Friday Harbor", "Palace Theatre Friday Harbor", "palace-theatre-friday-harbor", "Washington", "Friday Harbor"),
    ("Sea View Theatre Eastsound", "Sea View Theatre Eastsound", "sea-view-theatre-eastsound", "Washington", "Eastsound")
  ))
  private def p_friendswood: R = ("friendswood", "friendswood-texas", "Friendswood", "Texas", 29.5294, -95.201, "America/Chicago", true, Seq("Friendswood", "Alvin"), Seq(
    ("Star Cinema Grill Baybrook", "Star Cinema Grill Baybrook", "star-cinema-grill-baybrook", "Texas", "Friendswood"),
    ("Wellborne Cinema Alvin", "Wellborne Cinema Alvin", "wellborne-cinema-alvin", "Texas", "Alvin")
  ))
  private def p_front_royal: R = ("front-royal", "front-royal-virginia", "Front Royal", "Virginia", 38.9182, -78.1944, "America/New_York", true, Seq("Front Royal", "Woodstock", "Luray"), Seq(
    ("Page Theatre Luray", "Page Theatre Luray", "page-theatre-luray", "Virginia", "Luray"),
    ("Royal Cinemas Front Royal", "Royal Cinemas Front Royal", "royal-cinemas-front-royal", "Virginia", "Front Royal"),
    ("Woodstock Community Theatre", "Woodstock Community Theatre", "woodstock-community-theatre", "Virginia", "Woodstock")
  ))
  private def p_frostburg: R = ("frostburg", "frostburg-maryland", "Frostburg", "Maryland", 39.6581, -78.9284, "America/New_York", true, Seq("Frostburg", "La Vale"), Seq(
    ("Golden Star Country Club Mall La Vale", "Golden Star Country Club Mall La Vale", "golden-star-country-club-mall-la-vale", "Maryland", "La Vale"),
    ("Palace Theatre Frostburg", "Palace Theatre Frostburg", "palace-theatre-frostburg", "Maryland", "Frostburg")
  ))
  private def p_fulton: R = ("fulton", "fulton-missouri", "Fulton", "Missouri", 38.8467, -91.948, "America/Chicago", false, Seq("Fulton"), Seq(
    ("Fulton Cinema 8", "Fulton Cinema 8", "fulton-cinema-8", "Missouri", "Fulton")
  ))
  private def p_gadsden: R = ("gadsden", "gadsden-alabama", "Gadsden", "Alabama", 34.0143, -86.0064, "America/Chicago", false, Seq("Gadsden"), Seq(
    ("Gadsden Premiere Cinema 16", "Gadsden Premiere Cinema 16", "gadsden-premiere-cinema-16", "Alabama", "Gadsden")
  ))
  private def p_gainesville_georgia: R = ("gainesville-georgia", "gainesville-georgia", "Gainesville", "Georgia", 34.2979, -83.8241, "America/New_York", true, Seq("Gainesville", "Cumming", "Dawsonville"), Seq(
    ("AMC Avenue Forsyth 12", "AMC Avenue Forsyth 12", "amc-avenue-forsyth-12", "Georgia", "Cumming"),
    ("Regal Dawsonville", "Regal Dawsonville", "regal-dawsonville", "Georgia", "Dawsonville"),
    ("Regal Hollywood Gainesville", "Regal Hollywood Gainesville", "regal-hollywood-gainesville", "Georgia", "Gainesville")
  ))
  private def p_gainesville_texas: R = ("gainesville-texas", "gainesville-texas", "Gainesville", "Texas", 33.6259, -97.1333, "America/Chicago", false, Seq("Gainesville"), Seq(
    ("Dmax Theatre Gainesville", "Dmax Theatre Gainesville", "dmax-theatre-gainesville", "Texas", "Gainesville")
  ))
  private def p_gaithersburg: R = ("gaithersburg", "gaithersburg-maryland", "Gaithersburg", "Maryland", 39.1434, -77.2014, "America/New_York", false, Seq("Gaithersburg"), Seq(
    ("AMC Rio 18", "AMC Rio 18", "amc-rio-18", "Maryland", "Gaithersburg"),
    ("Cinepolis Gaithersburg", "Cinepolis Gaithersburg", "cinepolis-gaithersburg", "Maryland", "Gaithersburg")
  ))
  private def p_galax: R = ("galax", "galax-virginia", "Galax", "Virginia", 36.6612, -80.924, "America/New_York", false, Seq("Galax"), Seq(
    ("Twin County Cinemas III Galax", "Twin County Cinemas III Galax", "twin-county-cinemas-iii-galax", "Virginia", "Galax")
  ))
  private def p_galesburg: R = ("galesburg", "galesburg-illinois", "Galesburg", "Illinois", 40.9478, -90.3712, "America/Chicago", false, Seq("Galesburg"), Seq(
    ("AMC CLASSIC Galesburg 8", "AMC CLASSIC Galesburg 8", "amc-classic-galesburg-8", "Illinois", "Galesburg")
  ))
  private def p_gallipolis: R = ("gallipolis", "gallipolis-ohio", "Gallipolis", "Ohio", 38.8098, -82.2024, "America/New_York", false, Seq("Gallipolis"), Seq(
    ("Silver Screen Gallipolis 7", "Silver Screen Gallipolis 7", "silver-screen-gallipolis-7", "Ohio", "Gallipolis")
  ))
  private def p_gallup: R = ("gallup", "gallup-new-mexico", "Gallup", "New Mexico", 35.5281, -108.7426, "America/Denver", false, Seq("Gallup"), Seq(
    ("Allen Red Rock Gallup 10", "Allen Red Rock Gallup 10", "allen-red-rock-gallup-10", "New Mexico", "Gallup"),
    ("El Morro Theater Gallup", "El Morro Theater Gallup", "el-morro-theater-gallup", "New Mexico", "Gallup")
  ))
  private def p_galveston: R = ("galveston", "galveston-texas", "Galveston", "Texas", 29.3013, -94.7977, "America/Chicago", true, Seq("Galveston", "Texas City"), Seq(
    ("Galveston Primetime", "Galveston Primetime", "galveston-primetime", "Texas", "Galveston"),
    ("Moody Gardens Theater Galveston", "Moody Gardens Theater Galveston", "moody-gardens-theater-galveston", "Texas", "Galveston"),
    ("Texas City Premiere Lux Cine", "Texas City Premiere Lux Cine", "texas-city-premiere-lux-cine", "Texas", "Texas City")
  ))
  private def p_garden_city: R = ("garden-city", "garden-city-kansas", "Garden City", "Kansas", 37.9717, -100.8727, "America/Chicago", false, Seq("Garden City"), Seq(
    ("Sequoyah Garden City 9", "Sequoyah Garden City 9", "sequoyah-garden-city-9", "Kansas", "Garden City")
  ))
  private def p_garrett: R = ("garrett", "garrett-indiana", "Garrett", "Indiana", 41.3495, -85.1355, "America/Indiana/Indianapolis", true, Seq("Garrett", "Auburn", "Kendallville", "Columbia City"), Seq(
    ("Bones Theatre Columbia City", "Bones Theatre Columbia City", "bones-theatre-columbia-city", "Indiana", "Columbia City"),
    ("NCG Auburn", "NCG Auburn", "ncg-auburn", "Indiana", "Auburn"),
    ("Silver Screen Garrett", "Silver Screen Garrett", "silver-screen-garrett", "Indiana", "Garrett"),
    ("Auburn Garrett Drive In", "Auburn Garrett Drive In", "auburn-garrett-drive-in", "Indiana", "Garrett"),
    ("Strand Kendallville", "Strand Kendallville", "strand-kendallville", "Indiana", "Kendallville")
  ))
  private def p_garrison: R = ("garrison", "garrison-north-dakota", "Garrison", "North Dakota", 47.6522, -101.4157, "America/Chicago", false, Seq("Garrison"), Seq(
    ("Saca Kota Theater", "Saca Kota Theater", "saca-kota-theater", "North Dakota", "Garrison")
  ))
  private def p_gastonia: R = ("gastonia", "gastonia-north-carolina", "Gastonia", "North Carolina", 35.2621, -81.1873, "America/New_York", true, Seq("Gastonia", "Belmont"), Seq(
    ("Regal Franklin Square Gastonia", "Regal Franklin Square Gastonia", "regal-franklin-square-gastonia", "North Carolina", "Gastonia"),
    ("Belmont Drive-In", "Belmont Drive-In", "belmont-drive-in", "North Carolina", "Belmont")
  ))
  private def p_gatesville: R = ("gatesville", "gatesville-texas", "Gatesville", "Texas", 31.4352, -97.7439, "America/Chicago", false, Seq("Gatesville"), Seq(
    ("Last Drive-in Picture Show Gatesville", "Last Drive-in Picture Show Gatesville", "last-drive-in-picture-show-gatesville", "Texas", "Gatesville")
  ))
  private def p_gaylord: R = ("gaylord", "gaylord-michigan", "Gaylord", "Michigan", 45.0275, -84.6748, "America/Detroit", false, Seq("Gaylord"), Seq(
    ("Gaylord Cinema West", "Gaylord Cinema West", "gaylord-cinema-west", "Michigan", "Gaylord")
  ))
  private def p_geneva: R = ("geneva", "geneva-new-york", "Geneva", "New York", 42.869, -76.9777, "America/New_York", true, Seq("Geneva", "Canandaigua", "Newark", "Penn Yan", "Lyons"), Seq(
    ("Canandaigua Theaters", "Canandaigua Theaters", "canandaigua-theaters", "New York", "Canandaigua"),
    ("Lake St Plaza Cinema Penn Yan", "Lake St Plaza Cinema Penn Yan", "lake-st-plaza-cinema-penn-yan", "New York", "Penn Yan"),
    ("Smith Center for the Arts Geneva", "Smith Center for the Arts Geneva", "smith-center-for-the-arts-geneva", "New York", "Geneva"),
    ("Ohmann Theatre Lyons", "Ohmann Theatre Lyons", "ohmann-theatre-lyons", "New York", "Lyons"),
    ("Rose City Drive In", "Rose City Drive In", "rose-city-drive-in", "New York", "Newark")
  ))
  private def p_geneva_nebraska: R = ("geneva-nebraska", "geneva-nebraska", "Geneva", "Nebraska", 40.5269, -97.5959, "America/Chicago", false, Seq("Geneva"), Seq(
    ("Rialto II Geneva", "Rialto II Geneva", "rialto-ii-geneva", "Nebraska", "Geneva")
  ))
  private def p_georgetown: R = ("georgetown", "georgetown-kentucky", "Georgetown", "Kentucky", 38.2098, -84.5588, "America/New_York", true, Seq("Georgetown", "Paris", "Cynthiana"), Seq(
    ("Georgetown Cinema", "Georgetown Cinema", "georgetown-cinema", "Kentucky", "Georgetown"),
    ("Rohs Opera House Cynthiana", "Rohs Opera House Cynthiana", "rohs-opera-house-cynthiana", "Kentucky", "Cynthiana"),
    ("Bourbon Drive-in Paris", "Bourbon Drive-in Paris", "bourbon-drive-in-paris", "Kentucky", "Paris")
  ))
  private def p_georgetown_south_carolina: R = ("georgetown-south-carolina", "georgetown-south-carolina", "Georgetown", "South Carolina", 33.3768, -79.2945, "America/New_York", false, Seq("Georgetown"), Seq(
    ("Strand Cinema Georgetown", "Strand Cinema Georgetown", "strand-cinema-georgetown", "South Carolina", "Georgetown")
  ))
  private def p_gettysburg: R = ("gettysburg", "gettysburg-pennsylvania", "Gettysburg", "Pennsylvania", 39.8309, -77.2311, "America/New_York", false, Seq("Gettysburg"), Seq(
    ("Majestic Theater Gettysburg", "Majestic Theater Gettysburg", "majestic-gettysburg", "Pennsylvania", "Gettysburg"),
    ("R/C Gateway Theater 8 Gettysburg", "R/C Gateway Theater 8 Gettysburg", "rc-gateway-theater-8-gettysburg", "Pennsylvania", "Gettysburg")
  ))
  private def p_gilford: R = ("gilford", "gilford-new-hampshire", "Gilford", "New Hampshire", 43.5476, -71.4067, "America/New_York", true, Seq("Gilford", "Plymouth", "Meredith"), Seq(
    ("BarnZ's Meredith Cinema", "BarnZ's Meredith Cinema", "barnzs-meredith-cinema", "New Hampshire", "Meredith"),
    ("Scene One Gilford Cinemas 8", "Scene One Gilford Cinemas 8", "scene-one-gilford-cinemas-8", "New Hampshire", "Gilford"),
    ("Flying Monkey Theatre Plymouth", "Flying Monkey Theatre Plymouth", "flying-monkey-theatre-plymouth", "New Hampshire", "Plymouth")
  ))
  private def p_gillette: R = ("gillette", "gillette-wyoming", "Gillette", "Wyoming", 44.2911, -105.5022, "America/Denver", false, Seq("Gillette"), Seq(
    ("Foothills Cinema 6 Gillette", "Foothills Cinema 6 Gillette", "foothills-cinema-6-gillette", "Wyoming", "Gillette")
  ))
  private def p_glasgow: R = ("glasgow", "glasgow-kentucky", "Glasgow", "Kentucky", 36.9959, -85.9119, "America/Chicago", false, Seq("Glasgow"), Seq(
    ("Marquee Highland Glasgow 8", "Marquee Highland Glasgow 8", "marquee-highland-glasgow-8", "Kentucky", "Glasgow")
  ))
  private def p_glasgow_montana: R = ("glasgow-montana", "glasgow-montana", "Glasgow", "Montana", 48.197, -106.6367, "America/Denver", false, Seq("Glasgow"), Seq(
    ("Valley Cinema Glasgow", "Valley Cinema Glasgow", "valley-cinema-glasgow", "Montana", "Glasgow")
  ))
  private def p_glen_allen: R = ("glen-allen", "glen-allen-virginia", "Glen Allen", "Virginia", 37.666, -77.5064, "America/New_York", true, Seq("Glen Allen", "Ashland"), Seq(
    ("Ashland Theatre", "Ashland Theatre", "ashland-theatre", "Virginia", "Ashland"),
    ("Regal Virginia Center Glen Allen", "Regal Virginia Center Glen Allen", "regal-virginia-center-glen-allen", "Virginia", "Glen Allen")
  ))
  private def p_glendive: R = ("glendive", "glendive-montana", "Glendive", "Montana", 47.1053, -104.7125, "America/Denver", false, Seq("Glendive"), Seq(
    ("Rose Theatre Glendive", "Rose Theatre Glendive", "rose-theatre-glendive", "Montana", "Glendive")
  ))
  private def p_globe: R = ("globe", "globe-arizona", "Globe", "Arizona", 33.3942, -110.7865, "America/Phoenix", false, Seq("Globe"), Seq(
    ("Hollis Cinema", "Hollis Cinema", "hollis-cinema", "Arizona", "Globe")
  ))
  private def p_gold_beach: R = ("gold-beach", "gold-beach-oregon", "Gold Beach", "Oregon", 42.4073, -124.4218, "America/Los_Angeles", false, Seq("Gold Beach"), Seq(
    ("Rogue Playhouse", "Rogue Playhouse", "rogue-playhouse", "Oregon", "Gold Beach")
  ))
  private def p_goldsboro: R = ("goldsboro", "goldsboro-north-carolina", "Goldsboro", "North Carolina", 35.3849, -77.9928, "America/New_York", false, Seq("Goldsboro"), Seq(
    ("UEC Theatres Goldsboro", "UEC Theatres Goldsboro", "uec-theatres-goldsboro", "North Carolina", "Goldsboro")
  ))
  private def p_goleta: R = ("goleta", "goleta-california", "Goleta", "California", 34.4358, -119.8276, "America/Los_Angeles", true, Seq("Goleta", "Isla Vista"), Seq(
    ("Camino Real Cinemas", "Camino Real Cinemas", "camino-real-cinemas", "California", "Goleta"),
    ("Fairview Theatre Goleta", "Fairview Theatre Goleta", "fairview-theatre-goleta", "California", "Goleta"),
    ("Isla Vista Theater", "Isla Vista Theater", "isla-vista-theater", "California", "Isla Vista")
  ))
  private def p_gonzales: R = ("gonzales", "gonzales-louisiana", "Gonzales", "Louisiana", 30.2385, -90.9201, "America/Chicago", true, Seq("Gonzales", "Denham Springs"), Seq(
    ("Malco Gonzales Cinema 11", "Malco Gonzales Cinema 11", "malco-gonzales-cinema-11", "Louisiana", "Gonzales"),
    ("Movie Tavern Juban Crossing", "Movie Tavern Juban Crossing", "movie-tavern-juban-crossing", "Louisiana", "Denham Springs")
  ))
  private def p_gonzales_texas: R = ("gonzales-texas", "gonzales-texas", "Gonzales", "Texas", 29.5016, -97.4525, "America/Chicago", true, Seq("Gonzales", "Yoakum", "Hallettsville"), Seq(
    ("Lynn Theater Gonzales", "Lynn Theater Gonzales", "lynn-theater-gonzales", "Texas", "Gonzales"),
    ("Grand Theater Yoakum", "Grand Theater Yoakum", "grand-theater-yoakum", "Texas", "Yoakum"),
    ("Cole Theatre Hallettsville", "Cole Theatre Hallettsville", "cole-theatre-hallettsville", "Texas", "Hallettsville")
  ))
  private def p_goodland: R = ("goodland", "goodland-kansas", "Goodland", "Kansas", 39.3508, -101.7102, "America/Denver", false, Seq("Goodland"), Seq(
    ("Sherman Theatre Goodland", "Sherman Theatre Goodland", "sherman-theatre-goodland", "Kansas", "Goodland")
  ))
  private def p_gordon: R = ("gordon", "gordon-nebraska", "Gordon", "Nebraska", 42.8047, -102.2032, "America/Denver", false, Seq("Gordon"), Seq(
    ("Gordon Community Theater", "Gordon Community Theater", "gordon-community-theater", "Nebraska", "Gordon")
  ))
  private def p_grafton: R = ("grafton", "grafton-north-dakota", "Grafton", "North Dakota", 48.4122, -97.4106, "America/Chicago", true, Seq("Grafton", "Park River"), Seq(
    ("Strand Theatre Seminole", "Strand Theatre Seminole", "strand-theatre-seminole", "North Dakota", "Camp Grafton"),
    ("Lyric Theatre Park River", "Lyric Theatre Park River", "lyric-theatre-park-river", "North Dakota", "Park River")
  ))
  private def p_graham: R = ("graham", "graham-texas", "Graham", "Texas", 33.1071, -98.5895, "America/Chicago", false, Seq("Graham"), Seq(
    ("National Graham 3", "National Graham 3", "national-graham-3", "Texas", "Graham"),
    ("Graham Drive-In", "Graham Drive-In", "graham-drive-in", "Texas", "Graham")
  ))
  private def p_granbury: R = ("granbury", "granbury-texas", "Granbury", "Texas", 32.4421, -97.7942, "America/Chicago", false, Seq("Granbury"), Seq(
    ("Cinergy Granbury 6", "Cinergy Granbury 6", "cinergy-granbury-6", "Texas", "Granbury"),
    ("Brazos Drive-in Granbury", "Brazos Drive-in Granbury", "brazos-drive-in-granbury", "Texas", "Granbury")
  ))
  private def p_grand_canyon_village: R = ("grand-canyon-village", "grand-canyon-village-arizona", "Grand Canyon Village", "Arizona", 36.0464, -112.1541, "America/Phoenix", false, Seq("Grand Canyon Village"), Seq(
    ("Grand Canyon IMAX Theater", "Grand Canyon IMAX Theater", "grand-canyon-imax-theater", "Arizona", "Grand Canyon Village")
  ))
  private def p_grand_island: R = ("grand-island", "grand-island-nebraska", "Grand Island", "Nebraska", 40.925, -98.342, "America/Chicago", false, Seq("Grand Island"), Seq(
    ("B&B Theatres Grand Island 7", "B&B Theatres Grand Island 7", "bb-theatres-grand-island-7", "Nebraska", "Grand Island"),
    ("Grand Theatre – Grand Island", "Grand Theatre – Grand Island", "grand-theatre--grand-island", "Nebraska", "Grand Island")
  ))
  private def p_grand_junction: R = ("grand-junction", "grand-junction-colorado", "Grand Junction", "Colorado", 39.0639, -108.5507, "America/Denver", false, Seq("Grand Junction"), Seq(
    ("Picture Show at Grand Junction", "Picture Show at Grand Junction", "picture-show-at-grand-junction", "Colorado", "Grand Junction"),
    ("Regal Canyon View Grand Junction", "Regal Canyon View Grand Junction", "regal-canyon-view-grand-junction", "Colorado", "Grand Junction"),
    ("Avalon Theatre Grand Junction", "Avalon Theatre Grand Junction", "avalon-theatre-grand-junction", "Colorado", "Grand Junction")
  ))
  private def p_grand_rapids_minnesota: R = ("grand-rapids-minnesota", "grand-rapids-minnesota", "Grand Rapids", "Minnesota", 47.2372, -93.5302, "America/Chicago", false, Seq("Grand Rapids"), Seq(
    ("Mann Theatres Grand Rapids 8", "Mann Theatres Grand Rapids 8", "mann-theatres-grand-rapids-8", "Minnesota", "Grand Rapids")
  ))
  private def p_grandville: R = ("grandville", "grandville-michigan", "Grandville", "Michigan", 42.9098, -85.7631, "America/Detroit", true, Seq("Grandville", "Rockford", "Cedar Springs"), Seq(
    ("Celebration! Rivertown", "Celebration! Rivertown", "celebration-rivertown", "Michigan", "Grandville"),
    ("Kent Theatre Cedar Springs", "Kent Theatre Cedar Springs", "kent-theatre-cedar-springs", "Michigan", "Cedar Springs"),
    ("Northstar Cinemas Rockford", "Northstar Cinemas Rockford", "northstar-cinemas-rockford", "Michigan", "Rockford")
  ))
  private def p_grangeville: R = ("grangeville", "grangeville-idaho", "Grangeville", "Idaho", 45.9265, -116.1224, "America/Los_Angeles", false, Seq("Grangeville"), Seq(
    ("Blue Fox Theatre Grangeville", "Blue Fox Theatre Grangeville", "blue-fox-theatre-grangeville", "Idaho", "Grangeville"),
    ("Sunset Auto Vue Drive-In Movie Theater", "Sunset Auto Vue Drive-In Movie Theater", "sunset-auto-vue-drive-in-movie-theater", "Idaho", "Grangeville")
  ))
  private def p_granite_city: R = ("granite-city", "granite-city-illinois", "Granite City", "Illinois", 38.7014, -90.1487, "America/Chicago", true, Seq("Granite City", "O'Fallon", "Alton"), Seq(
    ("Granite City Cinema", "Granite City Cinema", "granite-city-cinema", "Illinois", "Granite City"),
    ("Marcus O'Fallon Cinema", "Marcus O'Fallon Cinema", "marcus-ofallon-cinema", "Illinois", "O'Fallon"),
    ("NCG Alton", "NCG Alton", "ncg-alton", "Illinois", "Alton")
  ))
  private def p_grants: R = ("grants", "grants-new-mexico", "Grants", "New Mexico", 35.1476, -107.8526, "America/Denver", false, Seq("Grants"), Seq(
    ("West Grants", "West Grants", "west-grants", "New Mexico", "Grants")
  ))
  private def p_grants_pass: R = ("grants-pass", "grants-pass-oregon", "Grants Pass", "Oregon", 42.4393, -123.3307, "America/Los_Angeles", true, Seq("Grants Pass", "Merlin"), Seq(
    ("Southgate Cinemas 12", "Southgate Cinemas 12", "southgate-cinemas-12", "Oregon", "Grants Pass"),
    ("Grants Pass Drive-In", "Grants Pass Drive-In", "grants-pass-drive-in", "Oregon", "Merlin")
  ))
  private def p_grapevine: R = ("grapevine", "grapevine-texas", "Grapevine", "Texas", 32.9343, -97.0781, "America/Chicago", true, Seq("Grapevine", "Flower Mound", "North Richland Hills", "Euless", "Bedford", "Keller", "Hurst", "Southlake", "Highland Village", "Roanoke", "Lake Worth"), Seq(
    ("AMC Grapevine Mills 24", "AMC Grapevine Mills 24", "amc-grapevine-mills-24", "Texas", "Grapevine"),
    ("Cinemark Tinseltown Grapevine", "Cinemark Tinseltown Grapevine", "cinemark-tinseltown-grapevine", "Texas", "Grapevine"),
    ("AMC Highland Village 12", "AMC Highland Village 12", "amc-highland-village-12", "Texas", "Highland Village"),
    ("B&B North Richland Hills 8", "B&B North Richland Hills 8", "bb-north-richland-hills-8", "Texas", "North Richland Hills"),
    ("Cinemark Northeast Mall 18", "Cinemark Northeast Mall 18", "cinemark-northeast-mall-18", "Texas", "Hurst"),
    ("Cinemark Roanoke", "Cinemark Roanoke", "cinemark-roanoke", "Texas", "Roanoke"),
    ("Cinepolis Euless", "Cinepolis Euless", "cinepolis-euless", "Texas", "Euless"),
    ("Cinepolis Flower Mound", "Cinepolis Flower Mound", "cinepolis-flower-mound", "Texas", "Flower Mound"),
    ("Cinepolis Keller", "Cinepolis Keller", "cinepolis-keller", "Texas", "Kellerville"),
    ("Hooky Entertainment Southlake", "Hooky Entertainment Southlake", "hooky-entertainment-southlake", "Texas", "Southlake"),
    ("LOOK Cinemas Bedford", "LOOK Cinemas Bedford", "look-cinemas-bedford", "Texas", "Bedford"),
    ("Texas Movie Bistro Lake Worth 8", "Texas Movie Bistro Lake Worth 8", "texas-movie-bistro-lake-worth-8", "Texas", "Lake Worth")
  ))
  private def p_grass_valley: R = ("grass-valley", "grass-valley-california", "Grass Valley", "California", 39.2191, -121.0611, "America/Los_Angeles", true, Seq("Grass Valley", "Nevada City"), Seq(
    ("Del Oro Theatre", "Del Oro Theatre", "del-oro-theatre", "California", "Grass Valley"),
    ("Prime Cinemas Sutton", "Prime Cinemas Sutton", "prime-cinemas-sutton", "California", "Grass Valley"),
    ("Onyx Theatre Theatre", "Onyx Theatre Theatre", "onyx-theatre-theatre", "California", "Nevada City"),
    ("Nevada Theatre", "Nevada Theatre", "nevada-theatre", "California", "Nevada City")
  ))
  private def p_grayling: R = ("grayling", "grayling-michigan", "Grayling", "Michigan", 44.6614, -84.7147, "America/Detroit", false, Seq("Grayling"), Seq(
    ("Rialto Theatre Grayling", "Rialto Theatre Grayling", "rialto-theatre-grayling", "Michigan", "Grayling")
  ))
  private def p_great_barrington: R = ("great-barrington", "great-barrington-massachusetts", "Great Barrington", "Massachusetts", 42.1959, -73.3621, "America/New_York", false, Seq("Great Barrington"), Seq(
    ("Triplex Cinema Great Barrington", "Triplex Cinema Great Barrington", "triplex-cinema-great-barrington", "Massachusetts", "Great Barrington"),
    ("Mahaiwe Performing Arts Center", "Mahaiwe Performing Arts Center", "mahaiwe-performing-arts-center", "Massachusetts", "Great Barrington")
  ))
  private def p_great_bend: R = ("great-bend", "great-bend-kansas", "Great Bend", "Kansas", 38.3645, -98.7648, "America/Chicago", false, Seq("Great Bend"), Seq(
    ("Golden Belt Cinema 6", "Golden Belt Cinema 6", "golden-belt-cinema-6", "Kansas", "Great Bend")
  ))
  private def p_great_falls: R = ("great-falls", "great-falls-montana", "Great Falls", "Montana", 47.5002, -111.3008, "America/Denver", false, Seq("Great Falls"), Seq(
    ("AMC CLASSIC Great Falls 10", "AMC CLASSIC Great Falls 10", "amc-classic-great-falls-10", "Montana", "Great Falls")
  ))
  private def p_greece: R = ("greece", "greece-new-york", "Greece", "New York", 43.2098, -77.6931, "America/New_York", true, Seq("Greece", "Brockport", "East Rochester", "Webster"), Seq(
    ("AMC Webster 12", "AMC Webster 12", "amc-webster-12", "New York", "Webster"),
    ("Apple Cinemas Greece Ridge", "Apple Cinemas Greece Ridge", "apple-cinemas-greece-ridge", "New York", "Greece"),
    ("Brockport Strand Theater", "Brockport Strand Theater", "brockport-strand-theater", "New York", "Brockport"),
    ("ROC Cinema Rochester", "ROC Cinema Rochester", "roc-cinema-rochester", "New York", "East Rochester")
  ))
  private def p_greenacres_city: R = ("greenacres-city", "greenacres-city-florida", "Greenacres City", "Florida", 26.6237, -80.1253, "America/New_York", true, Seq("Greenacres City", "Wellington", "Royal Palm Beach", "Lake Worth Beach"), Seq(
    ("CMX Cinemas Wellington", "CMX Cinemas Wellington", "cmx-cinemas-wellington", "Florida", "Wellington"),
    ("Movies Of Lake Worth 6", "Movies Of Lake Worth 6", "movies-of-lake-worth-6", "Florida", "Lake Worth"),
    ("Phoenix Theatres Lake Worth 8", "Phoenix Theatres Lake Worth 8", "phoenix-theatres-lake-worth-8", "Florida", "Greenacres City"),
    ("Regal Royal Royal Palm Beach", "Regal Royal Royal Palm Beach", "regal-royal-royal-palm-beach", "Florida", "Royal Palm Beach"),
    ("Lake Worth Playhouse", "Lake Worth Playhouse", "lake-worth-playhouse", "Florida", "Lake Worth")
  ))
  private def p_greencastle: R = ("greencastle", "greencastle-indiana", "Greencastle", "Indiana", 39.6445, -86.8647, "America/Indiana/Indianapolis", true, Seq("Greencastle", "Brazil", "Rockville"), Seq(
    ("Ritz Theatre Rockville", "Ritz Theatre Rockville", "ritz-theatre-rockville", "Indiana", "Rockville"),
    ("Walnut Theatre Brazil", "Walnut Theatre Brazil", "walnut-theatre-brazil", "Indiana", "Brazil"),
    ("Ashley Square Cinema Greencastle", "Ashley Square Cinema Greencastle", "ashley-square-cinema-greencastle", "Indiana", "Greencastle")
  ))
  private def p_greeneville: R = ("greeneville", "greeneville-tennessee", "Greeneville", "Tennessee", 36.1632, -82.831, "America/New_York", false, Seq("Greeneville"), Seq(
    ("Golden Ticket Towne Crossing 8", "Golden Ticket Towne Crossing 8", "amc-classic-towne-crossing-8", "Tennessee", "Greeneville")
  ))
  private def p_greenfield: R = ("greenfield", "greenfield-iowa", "Greenfield", "Iowa", 41.3053, -94.4613, "America/Chicago", true, Seq("Greenfield", "Stuart"), Seq(
    ("Grand Theatre & Videos", "Grand Theatre & Videos", "grand-theatre-videos", "Iowa", "Greenfield"),
    ("Stuart Fremont Theatre", "Stuart Fremont Theatre", "stuart-fremont-theatre", "Iowa", "Stuart")
  ))
  private def p_greensboro_georgia: R = ("greensboro-georgia", "greensboro-georgia", "Greensboro", "Georgia", 33.5757, -83.1824, "America/New_York", false, Seq("Greensboro"), Seq(
    ("Parkside Main 8 Greensboro", "Parkside Main 8 Greensboro", "parkside-main-8-greensboro", "Georgia", "Greensboro")
  ))
  private def p_greensburg: R = ("greensburg", "greensburg-pennsylvania", "Greensburg", "Pennsylvania", 40.3015, -79.5389, "America/New_York", true, Seq("Greensburg", "Latrobe", "Mount Pleasant", "Ligonier"), Seq(
    ("AMC Greensburg 15", "AMC Greensburg 15", "amc-greensburg-15", "Pennsylvania", "Greensburg"),
    ("Evergreen Drive-in Mt Pleasant", "Evergreen Drive-in Mt Pleasant", "evergreen-drive-in-mt-pleasant", "Pennsylvania", "Mount Pleasant"),
    ("Latrobe 30 Theatre & Cafe", "Latrobe 30 Theatre & Cafe", "latrobe-30-theatre-cafe", "Pennsylvania", "Latrobe"),
    ("Diamond Theatre Ligonier", "Diamond Theatre Ligonier", "diamond-theatre-ligonier", "Pennsylvania", "Ligonier")
  ))
  private def p_greensburg_indiana: R = ("greensburg-indiana", "greensburg-indiana", "Greensburg", "Indiana", 39.3373, -85.4836, "America/Indiana/Indianapolis", true, Seq("Greensburg", "Batesville", "Rushville", "Versailles", "Osgood"), Seq(
    ("Bel Air Drive In Versailles", "Bel Air Drive In Versailles", "bel-air-drive-in-versailles", "Indiana", "Versailles"),
    ("Damm Theatre Osgood", "Damm Theatre Osgood", "damm-theatre-osgood", "Indiana", "Osgood"),
    ("Gibson Theatre Batesville", "Gibson Theatre Batesville", "gibson-theatre-batesville", "Indiana", "Batesville"),
    ("Princess Theatre Rushville Indiana", "Princess Theatre Rushville Indiana", "princess-theatre-rushville-indiana", "Indiana", "Rushville"),
    ("Wolf Theatres", "Wolf Theatres", "wolf-theatres", "Indiana", "Greensburg")
  ))
  private def p_greensburg_kansas: R = ("greensburg-kansas", "greensburg-kansas", "Greensburg", "Kansas", 37.6028, -99.2926, "America/Chicago", false, Seq("Greensburg"), Seq(
    ("Twilight Theatre & Community Auditorium", "Twilight Theatre & Community Auditorium", "twilight-theatre-community-auditorium", "Kansas", "Greensburg")
  ))
  private def p_greenville: R = ("greenville", "greenville-south-carolina", "Greenville", "South Carolina", 34.8526, -82.394, "America/New_York", false, Seq("Greenville"), Seq(
    ("Camelot Cinemas Greenville", "Camelot Cinemas Greenville", "camelot-cinemas-greenville", "South Carolina", "Greenville"),
    ("Regal Cherrydale Grand Greenville", "Regal Cherrydale Grand Greenville", "regal-cherrydale-grand-greenville", "South Carolina", "Greenville"),
    ("Regal Hollywood Greenville", "Regal Hollywood Greenville", "regal-hollywood-greenville", "South Carolina", "Greenville")
  ))
  private def p_greenville_alabama: R = ("greenville-alabama", "greenville-alabama", "Greenville", "Alabama", 31.8296, -86.6178, "America/Chicago", false, Seq("Greenville"), Seq(
    ("Edge Theatre Greenville", "Edge Theatre Greenville", "edge-theatre-greenville", "Alabama", "Greenville")
  ))
  private def p_greenville_mississippi: R = ("greenville-mississippi", "greenville-mississippi", "Greenville", "Mississippi", 33.409, -91.0598, "America/Chicago", false, Seq("Greenville"), Seq(
    ("UEC Theatres Greenville", "UEC Theatres Greenville", "uec-theatres-greenville", "Mississippi", "Greenville")
  ))
  private def p_greenville_north_carolina: R = ("greenville-north-carolina", "greenville-north-carolina", "Greenville", "North Carolina", 35.6127, -77.3663, "America/New_York", false, Seq("Greenville"), Seq(
    ("AMC Fire Tower 12", "AMC Fire Tower 12", "amc-fire-tower-12", "North Carolina", "Greenville"),
    ("Golden Ticket Greenville Grande", "Golden Ticket Greenville Grande", "golden-ticket-greenville-grande", "North Carolina", "Greenville")
  ))
  private def p_greenwood: R = ("greenwood", "greenwood-south-carolina", "Greenwood", "South Carolina", 34.1954, -82.1618, "America/New_York", false, Seq("Greenwood"), Seq(
    ("Premiere Greenwood LUX 10", "Premiere Greenwood LUX 10", "premiere-greenwood-lux-10", "South Carolina", "Greenwood"),
    ("Auto Drive-in Greenwood", "Auto Drive-in Greenwood", "auto-drive-in-greenwood", "South Carolina", "Greenwood")
  ))
  private def p_greenwood_indiana: R = ("greenwood-indiana", "greenwood-indiana", "Greenwood", "Indiana", 39.6137, -86.1067, "America/Indiana/Indianapolis", true, Seq("Greenwood", "Plainfield", "Franklin", "Greenfield", "Mooresville"), Seq(
    ("AMC Perry Crossing 18 Plainfield", "AMC Perry Crossing 18 Plainfield", "amc-perry-crossing-18-plainfield", "Indiana", "Plainfield"),
    ("Historic Artcraft Theatre Franklin", "Historic Artcraft Theatre Franklin", "historic-artcraft-theatre-franklin", "Indiana", "Franklin"),
    ("Regal Greenwood", "Regal Greenwood", "regal-greenwood", "Indiana", "Greenwood"),
    ("Showtime Mooresville 8", "Showtime Mooresville 8", "showtime-mooresville-8", "Indiana", "Mooresville"),
    ("VIP Legacy 9 Greenfield", "VIP Legacy 9 Greenfield", "vip-legacy-9-greenfield", "Indiana", "Greenfield")
  ))
  private def p_gregory: R = ("gregory", "gregory-south-dakota", "Gregory", "South Dakota", 43.2322, -99.4304, "America/Chicago", false, Seq("Gregory"), Seq(
    ("Hilltop Drive-In Theatre", "Hilltop Drive-In Theatre", "hilltop-drive-inn-theatre", "South Dakota", "Gregory"),
    ("Hipp Theatre", "Hipp Theatre", "hipp-theatre", "South Dakota", "Gregory")
  ))
  private def p_grenada: R = ("grenada", "grenada-mississippi", "Grenada", "Mississippi", 33.769, -89.8084, "America/Chicago", false, Seq("Grenada"), Seq(
    ("Plaza Grenada", "Plaza Grenada", "plaza-grenada", "Mississippi", "Grenada")
  ))
  private def p_grinnell: R = ("grinnell", "grinnell-iowa", "Grinnell", "Iowa", 41.743, -92.7224, "America/Chicago", true, Seq("Grinnell", "Brooklyn"), Seq(
    ("Fridley Strand Grinnell", "Fridley Strand Grinnell", "fridley-strand-grinnell", "Iowa", "Grinnell"),
    ("Brooklyn Opera House", "Brooklyn Opera House", "brooklyn-opera-house", "Iowa", "Brooklyn")
  ))
  private def p_grove: R = ("grove", "grove-oklahoma", "Grove", "Oklahoma", 36.5937, -94.7691, "America/Chicago", false, Seq("Grove"), Seq(
    ("Center Cinema Grove", "Center Cinema Grove", "center-cinema-grove", "Oklahoma", "Grove")
  ))
  private def p_grundy: R = ("grundy", "grundy-virginia", "Grundy", "Virginia", 37.2779, -82.099, "America/New_York", false, Seq("Grundy"), Seq(
    ("Grundy Community Center", "Grundy Community Center", "grundy-community-center", "Virginia", "Grundy")
  ))
  private def p_guayama: R = ("guayama", "guayama-puerto-rico", "Guayama", "Puerto Rico", 17.9841, -66.1138, "America/Puerto_Rico", true, Seq("Guayama", "Cayey", "Santa Isabel"), Seq(
    ("Caribbean Cinemas Plaza Cayey", "Caribbean Cinemas Plaza Cayey", "caribbean-cinemas-plaza-cayey", "Puerto Rico", "Cayey"),
    ("Caribbean Cinemas Plaza Guayama", "Caribbean Cinemas Plaza Guayama", "caribbean-cinemas-plaza-guayama", "Puerto Rico", "Guayama"),
    ("Caribbean Cinemas Santa Isabel", "Caribbean Cinemas Santa Isabel", "caribbean-cinemas-santa-isabel", "Puerto Rico", "Santa Isabel")
  ))
  private def p_guaynabo: R = ("guaynabo", "guaynabo-puerto-rico", "Guaynabo", "Puerto Rico", 18.3575, -66.111, "America/Puerto_Rico", true, Seq("Guaynabo", "Caguas"), Seq(
    ("Caribbean Cinemas Las Catalinas", "Caribbean Cinemas Las Catalinas", "caribbean-cinemas-las-catalinas", "Puerto Rico", "Caguas"),
    ("Caribbean Cinemas Plaza Guaynabo", "Caribbean Cinemas Plaza Guaynabo", "caribbean-cinemas-plaza-guaynabo", "Puerto Rico", "Guaynabo"),
    ("Caribbean Cinemas San Patricio", "Caribbean Cinemas San Patricio", "caribbean-cinemas-san-patricio", "Puerto Rico", "Guaynabo")
  ))
  private def p_guin: R = ("guin", "guin-alabama", "Guin", "Alabama", 33.9657, -87.9147, "America/Chicago", false, Seq("Guin"), Seq(
    ("Blue Moon Drive-In", "Blue Moon Drive-In", "blue-moon-drive-in", "Alabama", "Guin")
  ))
  private def p_gulf_shores: R = ("gulf-shores", "gulf-shores-alabama", "Gulf Shores", "Alabama", 30.246, -87.7008, "America/Chicago", true, Seq("Gulf Shores", "Orange Beach"), Seq(
    ("AMC CLASSIC Wharf 15", "AMC CLASSIC Wharf 15", "amc-classic-wharf-15", "Alabama", "Orange Beach"),
    ("CMX Cinemas Pinnacle 14", "CMX Cinemas Pinnacle 14", "cmx-cinemas-pinnacle-14", "Alabama", "Gulf Shores")
  ))
  private def p_gulfport: R = ("gulfport", "gulfport-mississippi", "Gulfport", "Mississippi", 30.3674, -89.0928, "America/Chicago", true, Seq("Gulfport", "Biloxi", "D'Iberville"), Seq(
    ("Biloxi Premiere 8", "Biloxi Premiere 8", "biloxi-premiere-8", "Mississippi", "Biloxi"),
    ("Cinemark Gulfport 16", "Cinemark Gulfport 16", "cinemark-gulfport-16", "Mississippi", "Gulfport"),
    ("Santikos Grand D'Iberville", "Santikos Grand D'Iberville", "santikos-grand-diberville", "Mississippi", "D'Iberville")
  ))
  private def p_gunnison: R = ("gunnison", "gunnison-colorado", "Gunnison", "Colorado", 38.5458, -106.9253, "America/Denver", false, Seq("Gunnison"), Seq(
    ("Blue Box Theatre, Gunnison Arts Center", "Blue Box Theatre, Gunnison Arts Center", "blue-box-theatre-gunnison-arts-center", "Colorado", "Gunnison")
  ))
  private def p_gunnison_utah: R = ("gunnison-utah", "gunnison-utah", "Gunnison", "Utah", 39.1552, -111.8183, "America/Denver", false, Seq("Gunnison"), Seq(
    ("Casino Star Theatre Gunnison", "Casino Star Theatre Gunnison", "casino-star-theatre-gunnison", "Utah", "Gunnison")
  ))
  private def p_guymon: R = ("guymon", "guymon-oklahoma", "Guymon", "Oklahoma", 36.6828, -101.4815, "America/Chicago", false, Seq("Guymon"), Seq(
    ("Northridge Guymon 8", "Northridge Guymon 8", "northridge-guymon-8", "Oklahoma", "Guymon")
  ))
  private def p_hadensville: R = ("hadensville", "hadensville-virginia", "Hadensville", "Virginia", 37.8447, -77.9893, "America/New_York", false, Seq("Hadensville"), Seq(
    ("Goochland Drive-n Theater Hadensville", "Goochland Drive-n Theater Hadensville", "goochland-drive-n-theater-hadensville", "Virginia", "Hadensville")
  ))
  private def p_hagerstown: R = ("hagerstown", "hagerstown-maryland", "Hagerstown", "Maryland", 39.6418, -77.72, "America/New_York", true, Seq("Hagerstown", "Waynesboro"), Seq(
    ("Regal Valley Mall Hagerstown", "Regal Valley Mall Hagerstown", "regal-valley-mall-hagerstown", "Maryland", "Hagerstown"),
    ("Warehouse Cinemas Leitersburg", "Warehouse Cinemas Leitersburg", "warehouse-cinemas-leitersburg", "Maryland", "Hagerstown"),
    ("Waynesboro Theatre", "Waynesboro Theatre", "waynesboro-theatre", "Pennsylvania", "Waynesboro")
  ))
  private def p_haleyville: R = ("haleyville", "haleyville-alabama", "Haleyville", "Alabama", 34.2265, -87.6214, "America/Chicago", false, Seq("Haleyville"), Seq(
    ("Dixie Theatre Haleyville", "Dixie Theatre Haleyville", "dixie-theatre-haleyville", "Alabama", "Haleyville")
  ))
  private def p_hamilton: R = ("hamilton", "hamilton-ohio", "Hamilton", "Ohio", 39.3995, -84.5613, "America/New_York", true, Seq("Hamilton", "Mason", "Springdale", "Beckett Ridge", "Brookville", "Wetherington", "East Hamilton"), Seq(
    ("AMC CLASSIC Hamilton 8", "AMC CLASSIC Hamilton 8", "amc-classic-hamilton-8", "Ohio", "Hamilton"),
    ("AMC West Chester 18", "AMC West Chester 18", "amc-west-chester-18", "Ohio", "West Chester"),
    ("Regal Deerfield Towne Center Mason", "Regal Deerfield Towne Center Mason", "regal-deerfield-towne-center-mason", "Ohio", "Mason"),
    ("Showcase Springdale", "Showcase Springdale", "showcase-springdale", "Ohio", "Springdale"),
    ("Brookville Theatre", "Brookville Theatre", "brookville-theatre", "Indiana", "Brookville"),
    ("CMX Liberty Luxury 15", "CMX Liberty Luxury 15", "cmx-liberty-luxury-15", "Ohio", "Liberty Township"),
    ("Holiday Drive-in Hamilton", "Holiday Drive-in Hamilton", "holiday-drive-in-hamilton", "Ohio", "East Hamilton")
  ))
  private def p_hammond: R = ("hammond", "hammond-louisiana", "Hammond", "Louisiana", 30.5046, -90.4629, "America/Chicago", false, Seq("Hammond"), Seq(
    ("AMC Hammond Palace 10", "AMC Hammond Palace 10", "amc-hammond-palace-10", "Louisiana", "Hammond")
  ))
  private def p_hampton_south_carolina: R = ("hampton-south-carolina", "hampton-south-carolina", "Hampton", "South Carolina", 32.8779, -81.1276, "America/New_York", false, Seq("Hampton"), Seq(
    ("Palmetto theater Theater", "Palmetto theater Theater", "palmetto-theater-theater", "South Carolina", "Hampton")
  ))
  private def p_hannibal: R = ("hannibal", "hannibal-missouri", "Hannibal", "Missouri", 39.7084, -91.3585, "America/Chicago", false, Seq("Hannibal"), Seq(
    ("B&B Hannibal Main Street Cinema 8", "B&B Hannibal Main Street Cinema 8", "bb-hannibal-main-street-cinema-8", "Missouri", "Hannibal")
  ))
  private def p_hanover: R = ("hanover", "hanover-new-hampshire", "Hanover", "New Hampshire", 43.7023, -72.2895, "America/New_York", true, Seq("Hanover", "Lebanon", "Woodstock", "Fairlee"), Seq(
    ("Entertainment Cinemas Lebanon", "Entertainment Cinemas Lebanon", "entertainment-cinemas-lebanon", "New Hampshire", "Lebanon"),
    ("Nugget Theater Hanover 4", "Nugget Theater Hanover 4", "nugget-theater-hanover-4", "New Hampshire", "Hanover"),
    ("Hopkins Center Hanover", "Hopkins Center Hanover", "hopkins-center-hanover", "New Hampshire", "Hanover"),
    ("Woodstock Town Hall Theatre", "Woodstock Town Hall Theatre", "woodstock-town-hall-theatre", "Vermont", "Woodstock"),
    ("Fairlee Drive-in", "Fairlee Drive-in", "fairlee-drive-in", "Vermont", "Fairlee")
  ))
  private def p_hardin: R = ("hardin", "hardin-montana", "Hardin", "Montana", 45.7325, -107.612, "America/Denver", false, Seq("Hardin"), Seq(
    ("Centre Cinema Hardin", "Centre Cinema Hardin", "centre-cinema-hardin", "Montana", "Hardin")
  ))
  private def p_hardy: R = ("hardy", "hardy-virginia", "Hardy", "Virginia", 37.1183, -79.7204, "America/New_York", false, Seq("Hardy"), Seq(
    ("Westlake Cinema Hardy", "Westlake Cinema Hardy", "westlake-cinema-hardy", "Virginia", "Hardy")
  ))
  private def p_harlan: R = ("harlan", "harlan-iowa", "Harlan", "Iowa", 41.653, -95.3256, "America/Chicago", false, Seq("Harlan"), Seq(
    ("Harlan Theatre", "Harlan Theatre", "harlan-theatre", "Iowa", "Harlan")
  ))
  private def p_harlingen: R = ("harlingen", "harlingen-texas", "Harlingen", "Texas", 26.1906, -97.6961, "America/Chicago", true, Seq("Harlingen", "Weslaco", "Mercedes"), Seq(
    ("Cinemark Harlingen 16", "Cinemark Harlingen 16", "cinemark-harlingen-16", "Texas", "Harlingen"),
    ("Cinemark Weslaco 10", "Cinemark Weslaco 10", "cinemark-weslaco-10", "Texas", "Weslaco"),
    ("Wes Mer Drive-in Mercedes", "Wes Mer Drive-in Mercedes", "wes-mer-drive-in-mercedes", "Texas", "Mercedes")
  ))
  private def p_harlowton: R = ("harlowton", "harlowton-montana", "Harlowton", "Montana", 46.4355, -109.8344, "America/Denver", false, Seq("Harlowton"), Seq(
    ("Harlo Theater", "Harlo Theater", "harlo-theater", "Montana", "Harlowton")
  ))
  private def p_harrisburg: R = ("harrisburg", "harrisburg-pennsylvania", "Harrisburg", "Pennsylvania", 40.2737, -76.8844, "America/New_York", true, Seq("Harrisburg", "Mechanicsburg", "Camp Hill", "New Cumberland"), Seq(
    ("AMC CLASSIC Camp Hill 12", "AMC CLASSIC Camp Hill 12", "amc-classic-camp-hill-12", "Pennsylvania", "Camp Hill"),
    ("AMC CLASSIC Hampden 8", "AMC CLASSIC Hampden 8", "amc-classic-hampden-8", "Pennsylvania", "Mechanicsburg"),
    ("Midtown Cinema", "Midtown Cinema", "midtown-cinema", "Pennsylvania", "Harrisburg"),
    ("Regal Harrisburg", "Regal Harrisburg", "regal-harrisburg", "Pennsylvania", "Harrisburg"),
    ("West Shore Theatre New Cumberland", "West Shore Theatre New Cumberland", "west-shore-theatre-new-cumberland", "Pennsylvania", "New Cumberland")
  ))
  private def p_harrisburg_illinois: R = ("harrisburg-illinois", "harrisburg-illinois", "Harrisburg", "Illinois", 37.7384, -88.5406, "America/Chicago", false, Seq("Harrisburg"), Seq(
    ("Showplace Cinemas Harrisburg 9", "Showplace Cinemas Harrisburg 9", "showplace-cinemas-harrisburg-9", "Illinois", "Harrisburg")
  ))
  private def p_harrison: R = ("harrison", "harrison-arkansas", "Harrison", "Arkansas", 36.2298, -93.1077, "America/Chicago", false, Seq("Harrison"), Seq(
    ("Golden Ticket Cinemas Harrison 8", "Golden Ticket Cinemas Harrison 8", "golden-ticket-cinemas-harrison-8", "Arkansas", "Harrison")
  ))
  private def p_harrisonburg: R = ("harrisonburg", "harrisonburg-virginia", "Harrisonburg", "Virginia", 38.4496, -78.8689, "America/New_York", true, Seq("Harrisonburg", "Bridgewater"), Seq(
    ("Regal Harrisonburg", "Regal Harrisonburg", "regal-harrisonburg", "Virginia", "Harrisonburg"),
    ("Court Square Theatre", "Court Square Theatre", "court-square-theatre", "Virginia", "Harrisonburg"),
    ("Sipe Center", "Sipe Center", "sipe-center", "Virginia", "Bridgewater")
  ))
  private def p_hartford_kentucky: R = ("hartford-kentucky", "hartford-kentucky", "Hartford", "Kentucky", 37.4512, -86.9092, "America/Chicago", false, Seq("Hartford"), Seq(
    ("Cinema 33 Hartford", "Cinema 33 Hartford", "cinema-33-hartford", "Kentucky", "Hartford")
  ))
  private def p_harvey: R = ("harvey", "harvey-north-dakota", "Harvey", "North Dakota", 47.7697, -99.9354, "America/Chicago", false, Seq("Harvey"), Seq(
    ("Harvey Central Cinema", "Harvey Central Cinema", "harvey-central-cinema", "North Dakota", "Harvey")
  ))
  private def p_hastings: R = ("hastings", "hastings-nebraska", "Hastings", "Nebraska", 40.5861, -98.3884, "America/Chicago", false, Seq("Hastings"), Seq(
    ("Golden Ticket Cinemas Hastings", "Golden Ticket Cinemas Hastings", "golden-ticket-cinemas-hastings", "Nebraska", "Hastings"),
    ("Rivoli 3 Hastings", "Rivoli 3 Hastings", "rivoli-3-hastings", "Nebraska", "Hastings"),
    ("Super Screen Hastings", "Super Screen Hastings", "super-screen-hastings", "Nebraska", "Hastings")
  ))
  private def p_hastings_michigan: R = ("hastings-michigan", "hastings-michigan", "Hastings", "Michigan", 42.6459, -85.2908, "America/Detroit", false, Seq("Hastings"), Seq(
    ("GQT Hastings 4", "GQT Hastings 4", "gqt-hastings-4", "Michigan", "Hastings")
  ))
  private def p_hattiesburg: R = ("hattiesburg", "hattiesburg-mississippi", "Hattiesburg", "Mississippi", 31.3271, -89.2903, "America/Chicago", false, Seq("Hattiesburg"), Seq(
    ("Santikos Grand Hattiesburg", "Santikos Grand Hattiesburg", "santikos-grand-hattiesburg", "Mississippi", "Hattiesburg"),
    ("Southwest Theaters Turtle Creek 9", "Southwest Theaters Turtle Creek 9", "southwest-theaters-turtle-creek-9", "Mississippi", "Hattiesburg")
  ))
  private def p_havelock: R = ("havelock", "havelock-north-carolina", "Havelock", "North Carolina", 34.879, -76.9013, "America/New_York", true, Seq("Havelock", "Emerald Isle", "Atlantic Beach"), Seq(
    ("AMC CLASSIC Havelock 6", "AMC CLASSIC Havelock 6", "amc-classic-havelock-6", "North Carolina", "Havelock"),
    ("Atlantic Station Cinema", "Atlantic Station Cinema", "atlantic-station-cinema", "North Carolina", "Atlantic Beach"),
    ("Emerald Plantation Cinemas", "Emerald Plantation Cinemas", "emerald-plantation-cinemas", "North Carolina", "Emerald Isle")
  ))
  private def p_havre: R = ("havre", "havre-montana", "Havre", "Montana", 48.55, -109.6841, "America/Denver", false, Seq("Havre"), Seq(
    ("Cottonwood Cinema Havre", "Cottonwood Cinema Havre", "cottonwood-cinema-havre", "Montana", "Havre")
  ))
  private def p_hayward_wisconsin: R = ("hayward-wisconsin", "hayward-wisconsin", "Hayward", "Wisconsin", 46.013, -91.4846, "America/Chicago", false, Seq("Hayward"), Seq(
    ("Hayward Cinema", "Hayward Cinema", "hayward-cinema", "Wisconsin", "Hayward")
  ))
  private def p_hazard: R = ("hazard", "hazard-kentucky", "Hazard", "Kentucky", 37.2495, -83.1932, "America/New_York", false, Seq("Hazard"), Seq(
    ("Hillside Theater Hazard", "Hillside Theater Hazard", "hillside-theater-hazard", "Kentucky", "Hazard")
  ))
  private def p_hazen: R = ("hazen", "hazen-north-dakota", "Hazen", "North Dakota", 47.2944, -101.6227, "America/North_Dakota/Beulah", false, Seq("Hazen"), Seq(
    ("Cinema Flix Hazen", "Cinema Flix Hazen", "cinema-flix-hazen", "North Dakota", "Hazen")
  ))
  private def p_hazleton: R = ("hazleton", "hazleton-pennsylvania", "Hazleton", "Pennsylvania", 40.9584, -75.9746, "America/New_York", true, Seq("Hazleton", "Berwick", "Lehighton", "West Hazleton", "Coaldale"), Seq(
    ("Angela Triplex Theatre Coaldale", "Angela Triplex Theatre Coaldale", "angela-triplex-theatre-coaldale", "Pennsylvania", "Coaldale"),
    ("Mahoning Drive-in", "Mahoning Drive-in", "mahoning-drive-in", "Pennsylvania", "Lehighton"),
    ("Regal Hazleton", "Regal Hazleton", "regal-hazleton", "Pennsylvania", "Hazleton"),
    ("Berwick Theater and Center for Community Arts", "Berwick Theater and Center for Community Arts", "berwick-theater-and-center-for-community-arts", "Pennsylvania", "Berwick"),
    ("Cinema & Drafthouse West Hazelton", "Cinema & Drafthouse West Hazelton", "cinema-drafthouse-west-hazelton", "Pennsylvania", "West Hazleton")
  ))
  private def p_healdsburg: R = ("healdsburg", "healdsburg-california", "Healdsburg", "California", 38.6105, -122.8692, "America/Los_Angeles", true, Seq("Healdsburg", "Monte Rio"), Seq(
    ("Monte Rio Theater & Extravaganza", "Monte Rio Theater & Extravaganza", "monte-rio-theater-extravaganza", "California", "Monte Rio"),
    ("True West Cinemas Healdsburg", "True West Cinemas Healdsburg", "true-west-cinemas-healdsburg", "California", "Healdsburg")
  ))
  private def p_heath: R = ("heath", "heath-ohio", "Heath", "Ohio", 40.0228, -82.4446, "America/New_York", false, Seq("Heath"), Seq(
    ("AMC Indian Mound Heath 9", "AMC Indian Mound Heath 9", "amc-indian-mound-heath-9", "Ohio", "Heath")
  ))
  private def p_heber_city: R = ("heber-city", "heber-city-utah", "Heber City", "Utah", 40.5069, -111.4132, "America/Denver", true, Seq("Heber City", "Park City"), Seq(
    ("Avon Theatre Heber City", "Avon Theatre Heber City", "avon-theatre-hebert-city", "Utah", "Heber City"),
    ("Ideal Theatre", "Ideal Theatre", "ideal-theatre", "Utah", "Heber City"),
    ("Megaplex Park City", "Megaplex Park City", "megaplex-park-city", "Utah", "Park City"),
    ("Park City Film Series/James Santy Auditorium", "Park City Film Series/James Santy Auditorium", "park-city-film-seriesjames-santy-auditorium", "Utah", "Park City")
  ))
  private def p_heber_springs: R = ("heber-springs", "heber-springs-arkansas", "Heber Springs", "Arkansas", 35.4915, -92.0313, "America/Chicago", false, Seq("Heber Springs"), Seq(
    ("Gem Theater Heber Springs", "Gem Theater Heber Springs", "gem-theater-heber-springs", "Arkansas", "Heber Springs")
  ))
  private def p_hebron: R = ("hebron", "hebron-nebraska", "Hebron", "Nebraska", 40.1664, -97.5859, "America/Chicago", false, Seq("Hebron"), Seq(
    ("Majestic Theatre Hebron", "Majestic Theatre Hebron", "majestic-theatre-hebron", "Nebraska", "Hebron")
  ))
  private def p_hebron_north_dakota: R = ("hebron-north-dakota", "hebron-north-dakota", "Hebron", "North Dakota", 46.9005, -102.0472, "America/North_Dakota/New_Salem", false, Seq("Hebron"), Seq(
    ("Mayer Theatre Hebron", "Mayer Theatre Hebron", "mayer-theatre-hebron", "North Dakota", "Hebron")
  ))
  private def p_helena: R = ("helena", "helena-montana", "Helena", "Montana", 46.5927, -112.0361, "America/Denver", false, Seq("Helena"), Seq(
    ("Cinemark Helena", "Cinemark Helena", "cinemark-helena", "Montana", "Helena"),
    ("The Myrna Loy", "The Myrna Loy", "the-myrna-loy", "Montana", "Helena")
  ))
  private def p_henderson_kentucky: R = ("henderson-kentucky", "henderson-kentucky", "Henderson", "Kentucky", 37.8362, -87.59, "America/Chicago", true, Seq("Henderson", "Newburgh"), Seq(
    ("Showplace Cinemas Henderson 8", "Showplace Cinemas Henderson 8", "showplace-cinemas-henderson-8", "Kentucky", "Henderson"),
    ("Showplace Cinemas Newburgh 10", "Showplace Cinemas Newburgh 10", "showplace-cinemas-newburgh-10", "Indiana", "Newburgh")
  ))
  private def p_henderson_north_carolina: R = ("henderson-north-carolina", "henderson-north-carolina", "Henderson", "North Carolina", 36.3296, -78.3992, "America/New_York", false, Seq("Henderson"), Seq(
    ("Raleigh Road Drive-In Henderson", "Raleigh Road Drive-In Henderson", "raleigh-road-drive-in-henderson", "North Carolina", "Henderson"),
    ("RCE Theaters Henderson", "RCE Theaters Henderson", "rce-theaters-henderson", "North Carolina", "Henderson")
  ))
  private def p_hendersonville: R = ("hendersonville", "hendersonville-tennessee", "Hendersonville", "Tennessee", 36.3048, -86.62, "America/Chicago", true, Seq("Hendersonville", "Gallatin", "Mount Juliet", "Antioch"), Seq(
    ("AMC Antioch 8", "AMC Antioch 8", "amc-antioch-8", "Tennessee", "Antioch"),
    ("NCG Gallatin", "NCG Gallatin", "ncg-gallatin", "Tennessee", "Gallatin"),
    ("Regal Providence Mt Juliet", "Regal Providence Mt Juliet", "regal-providence-mt-juliet", "Tennessee", "Mount Juliet"),
    ("Regal Streets Of Indian Lake", "Regal Streets Of Indian Lake", "regal-streets-of-indian-lake", "Tennessee", "Hendersonville")
  ))
  private def p_hendersonville_north_carolina: R = ("hendersonville-north-carolina", "hendersonville-north-carolina", "Hendersonville", "North Carolina", 35.3187, -82.4609, "America/New_York", true, Seq("Hendersonville", "Brevard", "East Flat Rock", "Tryon"), Seq(
    ("Epic Theatres Hendersonville", "Epic Theatres Hendersonville", "epic-theatres-hendersonville", "North Carolina", "Hendersonville"),
    ("Flat Rock Cinema", "Flat Rock Cinema", "flat-rock-cinema", "North Carolina", "East Flat Rock"),
    ("Tryon Theatre", "Tryon Theatre", "tryon-theatre", "North Carolina", "Tryon"),
    ("Co-Ed Cinema Brevard", "Co-Ed Cinema Brevard", "co-ed-cinema-brevard", "North Carolina", "Brevard")
  ))
  private def p_hereford: R = ("hereford", "hereford-texas", "Hereford", "Texas", 34.8152, -102.3993, "America/Chicago", false, Seq("Hereford"), Seq(
    ("Hereford Premiere Moovies 6", "Hereford Premiere Moovies 6", "hereford-premiere-moovies-6", "Texas", "Hereford")
  ))
  private def p_hermiston: R = ("hermiston", "hermiston-oregon", "Hermiston", "Oregon", 45.8404, -119.2895, "America/Los_Angeles", false, Seq("Hermiston"), Seq(
    ("Hermiston Cinema", "Hermiston Cinema", "hermiston-cinema", "Oregon", "Hermiston")
  ))
  private def p_hesperia: R = ("hesperia", "hesperia-california", "Hesperia", "California", 34.4264, -117.3009, "America/Los_Angeles", true, Seq("Hesperia", "Apple Valley"), Seq(
    ("Cinemark Jess Ranch", "Cinemark Jess Ranch", "cinemark-jess-ranch", "California", "Apple Valley"),
    ("Civic Plaza Cinemas Hesperia", "Civic Plaza Cinemas Hesperia", "civic-plaza-cinemas-hesperia", "California", "Hesperia")
  ))
  private def p_hettinger: R = ("hettinger", "hettinger-north-dakota", "Hettinger", "North Dakota", 46.0014, -102.6368, "America/Denver", false, Seq("Hettinger"), Seq(
    ("Hettinger Theatre", "Hettinger Theatre", "hettinger-theatre", "North Dakota", "New England")
  ))
  private def p_hiawatha: R = ("hiawatha", "hiawatha-kansas", "Hiawatha", "Kansas", 39.8525, -95.5358, "America/Chicago", false, Seq("Hiawatha"), Seq(
    ("Arrow Twin Theatre Hiawatha", "Arrow Twin Theatre Hiawatha", "arrow-twin-theatre-hiawatha", "Kansas", "Hiawatha")
  ))
  private def p_hibbing: R = ("hibbing", "hibbing-minnesota", "Hibbing", "Minnesota", 47.4271, -92.9377, "America/Chicago", false, Seq("Hibbing"), Seq(
    ("Mann Theatres Hibbing 8", "Mann Theatres Hibbing 8", "mann-theatres-hibbing-8", "Minnesota", "Hibbing")
  ))
  private def p_hickory: R = ("hickory", "hickory-north-carolina", "Hickory", "North Carolina", 35.7332, -81.3412, "America/New_York", true, Seq("Hickory", "Newton"), Seq(
    ("AMC Hickory 15", "AMC Hickory 15", "amc-hickory-15", "North Carolina", "Hickory"),
    ("Carolina Cinema Hickory", "Carolina Cinema Hickory", "carolina-cinema-hickory", "North Carolina", "Hickory"),
    ("State Cinema Newton", "State Cinema Newton", "state-cinema-newton", "North Carolina", "Newton")
  ))
  private def p_highland: R = ("highland", "highland-illinois", "Highland", "Illinois", 38.7395, -89.6712, "America/Chicago", true, Seq("Highland", "Greenville"), Seq(
    ("Globe Theater Greenville", "Globe Theater Greenville", "globe-theater-greenville", "Illinois", "Greenville"),
    ("The Lory Theater", "The Lory Theater", "the-lory-theater", "Illinois", "Highland")
  ))
  private def p_highland_arkansas: R = ("highland-arkansas", "highland-arkansas", "Highland", "Arkansas", 36.2759, -91.524, "America/Chicago", false, Seq("Highland"), Seq(
    ("Highland Twin Cinema", "Highland Twin Cinema", "highland-twin-cinema", "Arkansas", "Hardy")
  ))
  private def p_hill_city: R = ("hill-city", "hill-city-kansas", "Hill City", "Kansas", 39.3647, -99.8421, "America/Chicago", true, Seq("Hill City", "Palco"), Seq(
    ("McKenna Youth and Activity Center", "McKenna Youth and Activity Center", "mckenna-youth-and-activity-center", "Kansas", "Palco"),
    ("Frontier Stage Hill City", "Frontier Stage Hill City", "frontier-stage-hill-city", "Kansas", "Hill City")
  ))
  private def p_hilo: R = ("hilo", "hilo-hawaii", "Hilo", "Hawaii", 19.7299, -155.0907, "Pacific/Honolulu", false, Seq("Hilo"), Seq(
    ("Palace Theater Hilo", "Palace Theater Hilo", "palace-theater-hilo", "Hawaii", "Hilo"),
    ("Regal Prince Kuhio", "Regal Prince Kuhio", "regal-prince-kuhio", "Hawaii", "Hilo")
  ))
  private def p_hilton_head_island: R = ("hilton-head-island", "hilton-head-island-south-carolina", "Hilton Head Island", "South Carolina", 32.1938, -80.7382, "America/New_York", true, Seq("Hilton Head Island", "Hilton Head", "Bluffton", "Beaufort", "Tybee Island"), Seq(
    ("Cinemark Bluffton 12", "Cinemark Bluffton 12", "cinemark-bluffton-12", "South Carolina", "Bluffton"),
    ("Highway 21 Drive-In", "Highway 21 Drive-In", "highway-21-drive-in", "South Carolina", "Beaufort"),
    ("Northridge Cinema 10 Hilton Head Island", "Northridge Cinema 10 Hilton Head Island", "northridge-cinema-10-hilton-head-island", "South Carolina", "Hilton Head Island"),
    ("Park Plaza Hilton Head 5", "Park Plaza Hilton Head 5", "park-plaza-hilton-head-5", "South Carolina", "Hilton Head"),
    ("Tybee Post Theater", "Tybee Post Theater", "tybee-post-theater", "Georgia", "Tybee Island")
  ))
  private def p_hinckley: R = ("hinckley", "hinckley-minnesota", "Hinckley", "Minnesota", 46.0113, -92.9444, "America/Chicago", false, Seq("Hinckley"), Seq(
    ("Grand Cinema Hinckley", "Grand Cinema Hinckley", "grand-cinema-hinckley", "Minnesota", "Hinckley")
  ))
  private def p_hinesville: R = ("hinesville", "hinesville-georgia", "Hinesville", "Georgia", 31.8469, -81.596, "America/New_York", true, Seq("Hinesville", "Fort Stewart"), Seq(
    ("GTC Liberty Cinemas Hinesville", "GTC Liberty Cinemas Hinesville", "gtc-liberty-cinemas-hinesville", "Georgia", "Hinesville"),
    ("Blaze Theatre and Pizzeria", "Blaze Theatre and Pizzeria", "blaze-theatre-and-pizzeria", "Georgia", "Fort Stewart")
  ))
  private def p_hingham: R = ("hingham", "hingham-massachusetts", "Hingham", "Massachusetts", 42.2418, -70.8898, "America/New_York", true, Seq("Hingham", "Braintree", "Hanover", "Holbrook", "North Scituate"), Seq(
    ("AMC Braintree Cinema 10", "AMC Braintree Cinema 10", "amc-braintree-cinema-10", "Massachusetts", "Braintree"),
    ("Cameo S Weymouth", "Cameo S Weymouth", "cameo-s-weymouth", "Massachusetts", "South Weymouth"),
    ("Patriot Cinemas Hingham", "Patriot Cinemas Hingham", "patriot-cinemas-hingham", "Massachusetts", "Hingham"),
    ("Edgartown Cinemas", "Edgartown Cinemas", "edgartown-cinemas", "Massachusetts", "Hingham"),
    ("Reel Harbor Theater", "Reel Harbor Theater", "reel-harbor-theater", "Massachusetts", "North Scituate"),
    ("Showcase Hanover Crossing", "Showcase Hanover Crossing", "showcase-hanover-crossing", "Massachusetts", "Hanover")
  ))
  private def p_hobbs: R = ("hobbs", "hobbs-new-mexico", "Hobbs", "New Mexico", 32.7026, -103.136, "America/Denver", false, Seq("Hobbs"), Seq(
    ("Allen Eagle Hobbs 9", "Allen Eagle Hobbs 9", "allen-eagle-hobbs-9", "New Mexico", "Hobbs")
  ))
  private def p_holbrook: R = ("holbrook", "holbrook-arizona", "Holbrook", "Arizona", 34.9023, -110.1582, "America/Phoenix", false, Seq("Holbrook"), Seq(
    ("Roxy Theater Holbrook", "Roxy Theater Holbrook", "roxy-theater-holbrook", "Arizona", "Holbrook")
  ))
  private def p_holdrege: R = ("holdrege", "holdrege-nebraska", "Holdrege", "Nebraska", 40.4403, -99.3698, "America/Chicago", true, Seq("Holdrege", "Phillipsburg", "Alma", "Franklin", "Oxford"), Seq(
    ("Sun Theatre Holdrege", "Sun Theatre Holdrege", "sun-theatre-holdrege", "Nebraska", "Holdrege"),
    ("Granada Theater Oxford", "Granada Theater Oxford", "granada-theater-oxford", "Nebraska", "Oxford"),
    ("Parrot Theatre Alma", "Parrot Theatre Alma", "parrot-theatre-alma", "Nebraska", "Alma"),
    ("Rosebowl Playhouse & Theatre", "Rosebowl Playhouse & Theatre", "rosebowl-playhouse-theatre", "Nebraska", "Franklin"),
    ("Majestic Theatre Phillipsburg", "Majestic Theatre Phillipsburg", "majestic-theatre-phillipsburg", "Kansas", "Phillipsburg")
  ))
  private def p_holland: R = ("holland", "holland-michigan", "Holland", "Michigan", 42.7875, -86.1089, "America/Detroit", false, Seq("Holland"), Seq(
    ("AMC Holland 8", "AMC Holland 8", "amc-holland-8", "Michigan", "Holland"),
    ("GQT Holland 7", "GQT Holland 7", "gqt-holland-7", "Michigan", "Holland"),
    ("Sperry's Moviehouse Holland", "Sperry's Moviehouse Holland", "sperrys-moviehouse-holland", "Michigan", "Holland")
  ))
  private def p_holly: R = ("holly", "holly-colorado", "Holly", "Colorado", 38.0527, -102.1225, "America/Denver", false, Seq("Holly"), Seq(
    ("Holly Theater", "Holly Theater", "holly-theater", "Colorado", "Holly")
  ))
  private def p_holly_springs: R = ("holly-springs", "holly-springs-north-carolina", "Holly Springs", "North Carolina", 35.6513, -78.8336, "America/New_York", true, Seq("Holly Springs", "Sanford", "Garner", "Morrisville"), Seq(
    ("AMC DINE-IN Holly Springs 9", "AMC DINE-IN Holly Springs 9", "amc-dine-in-holly-springs-9", "North Carolina", "Holly Springs"),
    ("AMC Park Place 12", "AMC Park Place 12", "amc-park-place-12", "North Carolina", "Morrisville"),
    ("Regal White Oak Garner", "Regal White Oak Garner", "regal-white-oak-garner", "North Carolina", "Garner"),
    ("Spring Lane Stadium 10", "Spring Lane Stadium 10", "spring-lane-stadium-10", "North Carolina", "Sanford")
  ))
  private def p_holyoke: R = ("holyoke", "holyoke-colorado", "Holyoke", "Colorado", 40.5844, -102.3024, "America/Denver", false, Seq("Holyoke"), Seq(
    ("Peerless Theatre Holyoke", "Peerless Theatre Holyoke", "peerless-theatre-holyoke", "Colorado", "Holyoke")
  ))
  private def p_homer: R = ("homer", "homer-alaska", "Homer", "Alaska", 59.6425, -151.5494, "America/Anchorage", false, Seq("Homer"), Seq(
    ("Porcupine Theater", "Porcupine Theater", "porcupine-theater", "Alaska", "Homer")
  ))
  private def p_homestead: R = ("homestead", "homestead-florida", "Homestead", "Florida", 25.4687, -80.4776, "America/New_York", true, Seq("Homestead", "Key Largo"), Seq(
    ("Ocean Reef Theater", "Ocean Reef Theater", "ocean-reef-theater", "Florida", "Key Largo"),
    ("Hooky Entertainment Homestead", "Hooky Entertainment Homestead", "hooky-entertainment-homestead", "Florida", "Homestead")
  ))
  private def p_hondo: R = ("hondo", "hondo-texas", "Hondo", "Texas", 29.3475, -99.1414, "America/Chicago", true, Seq("Hondo", "Castroville"), Seq(
    ("RAINBOW THEATER", "RAINBOW THEATER", "rainbow-theater", "Texas", "Castroville"),
    ("Raye Theater Hondo", "Raye Theater Hondo", "raye-theater-hondo", "Texas", "Rio Hondo")
  ))
  private def p_honoka_a: R = ("honoka-a", "honoka-a-hawaii", "Honoka‘a", "Hawaii", 20.0793, -155.4669, "Pacific/Honolulu", false, Seq("Honoka‘a"), Seq(
    ("Honoka'a People's Theatre", "Honoka'a People's Theatre", "honokaa-peoples-theatre", "Hawaii", "Honoka‘a")
  ))
  private def p_hood_river: R = ("hood-river", "hood-river-oregon", "Hood River", "Oregon", 45.7054, -121.5215, "America/Los_Angeles", false, Seq("Hood River"), Seq(
    ("CineStars Hood River", "CineStars Hood River", "cinestars-hood-river", "Oregon", "Hood River"),
    ("Bistro Flix Hood River", "Bistro Flix Hood River", "bistro-flix-hood-river", "Oregon", "Hood River"),
    ("Columbia Center", "Columbia Center", "columbia-center", "Oregon", "Hood River")
  ))
  private def p_hoopeston: R = ("hoopeston", "hoopeston-illinois", "Hoopeston", "Illinois", 40.4673, -87.6684, "America/Chicago", false, Seq("Hoopeston"), Seq(
    ("Lorraine Theatre Hoopeston", "Lorraine Theatre Hoopeston", "lorraine-theatre-hoopeston", "Illinois", "Hoopeston")
  ))
  private def p_hoover: R = ("hoover", "hoover-alabama", "Hoover", "Alabama", 33.4054, -86.8114, "America/Chicago", true, Seq("Hoover", "Vestavia Hills", "Alabaster", "Bessemer", "Trussville", "Leeds"), Seq(
    ("AMC DINE-IN Vestavia Hills 10", "AMC DINE-IN Vestavia Hills 10", "amc-dine-in-vestavia-hills-10", "Alabama", "Vestavia Hills"),
    ("AMC Patton Creek 15", "AMC Patton Creek 15", "amc-patton-creek-15", "Alabama", "Hoover"),
    ("Bessemer Premiere 14 Promenade", "Bessemer Premiere 14 Promenade", "bessemer-premiere-14-promenade", "Alabama", "Bessemer"),
    ("Regal Trussville", "Regal Trussville", "regal-trussville", "Alabama", "Trussville"),
    ("Santikos AmStar Alabaster", "Santikos AmStar Alabaster", "santikos-amstar-alabaster", "Alabama", "Alabaster"),
    ("Grand River Drive-in", "Grand River Drive-in", "grand-river-drive-in", "Alabama", "Leeds")
  ))
  private def p_hopkinsville: R = ("hopkinsville", "hopkinsville-kentucky", "Hopkinsville", "Kentucky", 36.8656, -87.4912, "America/Chicago", false, Seq("Hopkinsville"), Seq(
    ("Showbox Hopkinsville", "Showbox Hopkinsville", "showbox-hopkinsville", "Kentucky", "Hopkinsville")
  ))
  private def p_hoquiam: R = ("hoquiam", "hoquiam-washington", "Hoquiam", "Washington", 46.9809, -123.8893, "America/Los_Angeles", true, Seq("Hoquiam", "Ocean Shores"), Seq(
    ("Ocean Shores 3", "Ocean Shores 3", "ocean-shores-3", "Washington", "Ocean Shores"),
    ("7th St Theatre Hoquiam", "7th St Theatre Hoquiam", "7th-st-theatre-hoquiam", "Washington", "East Hoquiam")
  ))
  private def p_hornell: R = ("hornell", "hornell-new-york", "Hornell", "New York", 42.3278, -77.6611, "America/New_York", true, Seq("Hornell", "Dansville"), Seq(
    ("Spotlight of Hornell", "Spotlight of Hornell", "spotlight-of-hornell", "New York", "Hornell"),
    ("Star Dansville", "Star Dansville", "star-dansville", "New York", "Dansville")
  ))
  private def p_hot_springs: R = ("hot-springs", "hot-springs-arkansas", "Hot Springs", "Arkansas", 34.5037, -93.0552, "America/Chicago", false, Seq("Hot Springs"), Seq(
    ("Central City 10 Hot Springs", "Central City 10 Hot Springs", "central-city-10-hot-springs", "Arkansas", "Hot Springs"),
    ("Hot Springs Mall Cinema 8", "Hot Springs Mall Cinema 8", "hot-springs-mall-cinema-8", "Arkansas", "Hot Springs")
  ))
  private def p_hot_springs_south_dakota: R = ("hot-springs-south-dakota", "hot-springs-south-dakota", "Hot Springs", "South Dakota", 43.4316, -103.4744, "America/Denver", false, Seq("Hot Springs"), Seq(
    ("Hot Springs Theatre", "Hot Springs Theatre", "hot-springs-theatre", "South Dakota", "Hot Springs")
  ))
  private def p_houghton: R = ("houghton", "houghton-michigan", "Houghton", "Michigan", 47.1219, -88.569, "America/Detroit", true, Seq("Houghton", "Laurium"), Seq(
    ("Rogers Cinema Houghton", "Rogers Cinema Houghton", "rogers-cinema-houghton", "Michigan", "Houghton"),
    ("Calumet Theatre", "Calumet Theatre", "calumet-theatre", "Michigan", "Calumet")
  ))
  private def p_houghton_lake: R = ("houghton-lake", "houghton-lake-michigan", "Houghton Lake", "Michigan", 44.3147, -84.7648, "America/Detroit", false, Seq("Houghton Lake"), Seq(
    ("Pines Theatre Houghton Lake", "Pines Theatre Houghton Lake", "pines-theatre-houghton-lake", "Michigan", "Houghton Lake")
  ))
  private def p_houlton: R = ("houlton", "houlton-maine", "Houlton", "Maine", 46.1262, -67.8403, "America/New_York", false, Seq("Houlton"), Seq(
    ("Temple Theatre Houlton", "Temple Theatre Houlton", "temple-theatre-houlton", "Maine", "Houlton")
  ))
  private def p_houma: R = ("houma", "houma-louisiana", "Houma", "Louisiana", 29.5958, -90.7195, "America/Chicago", false, Seq("Houma"), Seq(
    ("AMC Houma Palace 10", "AMC Houma Palace 10", "amc-houma-palace-10", "Louisiana", "Houma")
  ))
  private def p_houston_missouri: R = ("houston-missouri", "houston-missouri", "Houston", "Missouri", 37.3262, -91.956, "America/Chicago", false, Seq("Houston"), Seq(
    ("Phoenix Drive-in Houston", "Phoenix Drive-in Houston", "phoenix-drive-in-houston", "Missouri", "Houston"),
    ("Phoenix Theatres Houston", "Phoenix Theatres Houston", "phoenix-theatres-houston", "Missouri", "Houston")
  ))
  private def p_howell: R = ("howell", "howell-michigan", "Howell", "Michigan", 42.6073, -83.9294, "America/Detroit", true, Seq("Howell", "Brighton"), Seq(
    ("MJR Brighton Towne Square", "MJR Brighton Towne Square", "mjr-brighton-towne-square", "Michigan", "Brighton"),
    ("Howell Theater", "Howell Theater", "howell-theater", "Michigan", "Howell")
  ))
  private def p_hudson: R = ("hudson", "hudson-new-york", "Hudson", "New York", 42.2529, -73.791, "America/New_York", true, Seq("Hudson", "Catskill", "Coxsackie", "Chatham"), Seq(
    ("Community Catskill", "Community Catskill", "community-catskill", "New York", "Catskill"),
    ("Crandell Cinema", "Crandell Cinema", "crandell-cinema", "New York", "Chatham"),
    ("Hi-Way Drive-In Theatre", "Hi-Way Drive-In Theatre", "hi-way-drive-in-theatre", "New York", "Coxsackie"),
    ("Spotlight of Hudson", "Spotlight of Hudson", "spotlight-of-hudson", "New York", "Hudson"),
    ("Story Screen Cinema Hudson", "Story Screen Cinema Hudson", "story-screen-cinema-hudson", "New York", "Hudson")
  ))
  private def p_hugoton: R = ("hugoton", "hugoton-kansas", "Hugoton", "Kansas", 37.1753, -101.3496, "America/Chicago", false, Seq("Hugoton"), Seq(
    ("High Plains Theatre", "High Plains Theatre", "high-plains-theatre", "Kansas", "Hugoton")
  ))
  private def p_humboldt: R = ("humboldt", "humboldt-tennessee", "Humboldt", "Tennessee", 35.8198, -88.9159, "America/Chicago", true, Seq("Humboldt", "Milan", "Alamo"), Seq(
    ("Plaza Theatre Humboldt", "Plaza Theatre Humboldt", "plaza-theatre-humboldt", "Tennessee", "Humboldt"),
    ("Ritz Milan", "Ritz Milan", "ritz-milan", "Tennessee", "Milan"),
    ("TN Safari Drive In Theater", "TN Safari Drive In Theater", "tn-safari-drive-in-theater", "Tennessee", "Alamo")
  ))
  private def p_huntersville: R = ("huntersville", "huntersville-north-carolina", "Huntersville", "North Carolina", 35.4107, -80.8428, "America/New_York", true, Seq("Huntersville", "Mooresville", "Davidson"), Seq(
    ("Regal Birkdale Huntersville", "Regal Birkdale Huntersville", "regal-birkdale-huntersville", "North Carolina", "Huntersville"),
    ("Santikos Amstar Mooresville", "Santikos Amstar Mooresville", "santikos-amstar-mooresville", "North Carolina", "Mooresville"),
    ("Our Town Cinemas Davidson", "Our Town Cinemas Davidson", "our-town-cinemas-davidson", "North Carolina", "Davidson")
  ))
  private def p_huntington: R = ("huntington", "huntington-new-york", "Huntington", "New York", 40.8681, -73.4257, "America/New_York", true, Seq("Huntington", "Westbury", "Farmingdale", "Levittown", "West Babylon", "Deer Park", "Glen Cove", "Northport"), Seq(
    ("AMC DINE-IN Huntington Square 12", "AMC DINE-IN Huntington Square 12", "amc-dine-in-huntington-square-12", "New York", "Northport"),
    ("AMC DINE-IN Levittown 10", "AMC DINE-IN Levittown 10", "amc-dine-in-levittown-10", "New York", "Levittown"),
    ("AMC Glen Cove 6", "AMC Glen Cove 6", "amc-glen-cove-6", "New York", "Glen Cove"),
    ("AMC Raceway 10", "AMC Raceway 10", "amc-raceway-10", "New York", "Westbury"),
    ("Regal Westbury Stadium", "Regal Westbury Stadium", "regal-westbury-stadium", "New York", "Westbury"),
    ("AMC Shore 8 Huntington", "AMC Shore 8 Huntington", "amc-shore-8-huntington", "New York", "Huntington"),
    ("Cinema Arts Centre Huntington 3", "Cinema Arts Centre Huntington 3", "cinema-arts-centre-huntington-3", "New York", "Huntington"),
    ("Regal Deer Park", "Regal Deer Park", "regal-deer-park", "New York", "Deer Park"),
    ("Regal Farmingdale", "Regal Farmingdale", "regal-farmingdale", "New York", "Farmingdale"),
    ("Showcase Farmingdale", "Showcase Farmingdale", "showcase-farmingdale", "New York", "Farmingdale"),
    ("South Bay Cinemas West Babylon 5", "South Bay Cinemas West Babylon 5", "south-bay-cinemas-west-babylon-5", "New York", "West Babylon")
  ))
  private def p_huntington_indiana: R = ("huntington-indiana", "huntington-indiana", "Huntington", "Indiana", 40.8831, -85.4975, "America/Indiana/Indianapolis", false, Seq("Huntington"), Seq(
    ("GQT Huntington 7", "GQT Huntington 7", "gqt-huntington-7", "Indiana", "Huntington"),
    ("GQT Huntington Twin Drive-in", "GQT Huntington Twin Drive-in", "gqt-huntington-twin-drive-in", "Indiana", "Huntington")
  ))
  private def p_huntsville_texas: R = ("huntsville-texas", "huntsville-texas", "Huntsville", "Texas", 30.7235, -95.5508, "America/Chicago", false, Seq("Huntsville"), Seq(
    ("Cinestar of Huntsville", "Cinestar of Huntsville", "cinestar-of-huntsville", "Texas", "Huntsville")
  ))
  private def p_huron: R = ("huron", "huron-south-dakota", "Huron", "South Dakota", 44.3633, -98.2143, "America/Chicago", false, Seq("Huron"), Seq(
    ("Huron Luxury Cinemas", "Huron Luxury Cinemas", "huron-luxury-cinemas", "South Dakota", "Huron")
  ))
  private def p_hurricane: R = ("hurricane", "hurricane-utah", "Hurricane", "Utah", 37.1753, -113.29, "America/Denver", false, Seq("Hurricane"), Seq(
    ("Coral Cliffs Cinema 8 Hurricane", "Coral Cliffs Cinema 8 Hurricane", "coral-cliffs-cinema-8-hurricane", "Utah", "Hurricane")
  ))
  private def p_hutchinson: R = ("hutchinson", "hutchinson-kansas", "Hutchinson", "Kansas", 38.0608, -97.9298, "America/Chicago", false, Seq("Hutchinson"), Seq(
    ("B&B Hutchinson Mall 8", "B&B Hutchinson Mall 8", "bb-hutchinson-mall-8", "Kansas", "Hutchinson"),
    ("Carey Dome Hutchinson", "Carey Dome Hutchinson", "carey-dome-hutchinson", "Kansas", "Hutchinson"),
    ("Fox Theatre Hutchinson", "Fox Theatre Hutchinson", "fox-theatre-hutchinson", "Kansas", "Hutchinson")
  ))
  private def p_hutchinson_minnesota: R = ("hutchinson-minnesota", "hutchinson-minnesota", "Hutchinson", "Minnesota", 44.8877, -94.3697, "America/Chicago", false, Seq("Hutchinson"), Seq(
    ("Odyssey Century Hutchinson", "Odyssey Century Hutchinson", "odyssey-century-hutchinson", "Minnesota", "Hutchinson"),
    ("State Theatre Hutchinson", "State Theatre Hutchinson", "state-theatre-hutchinson", "Minnesota", "Hutchinson")
  ))
  private def p_ida_grove: R = ("ida-grove", "ida-grove-iowa", "Ida Grove", "Iowa", 42.345, -95.4717, "America/Chicago", false, Seq("Ida Grove"), Seq(
    ("King Theatre Ida Grove", "King Theatre Ida Grove", "king-theatre-ida-grove", "Iowa", "Ida Grove")
  ))
  private def p_idabel: R = ("idabel", "idabel-oklahoma", "Idabel", "Oklahoma", 33.8957, -94.8263, "America/Chicago", false, Seq("Idabel"), Seq(
    ("McCurtain Cinema Idabel", "McCurtain Cinema Idabel", "mccurtain-cinema-idabel", "Oklahoma", "Idabel")
  ))
  private def p_idaho_falls: R = ("idaho-falls", "idaho-falls-idaho", "Idaho Falls", "Idaho", 43.4666, -112.0341, "America/Boise", false, Seq("Idaho Falls"), Seq(
    ("Regal Edwards Grand Teton", "Regal Edwards Grand Teton", "regal-edwards-grand-teton", "Idaho", "Idaho Falls"),
    ("Centre Theatre Idaho Falls", "Centre Theatre Idaho Falls", "centre-theatre-idaho-falls", "Idaho", "Idaho Falls"),
    ("Motor Vu Drive-In", "Motor Vu Drive-In", "motor-vu-drive-in", "Idaho", "Idaho Falls"),
    ("Paramount Triplex Theatre", "Paramount Triplex Theatre", "paramount-triplex-theatre", "Idaho", "Idaho Falls")
  ))
  private def p_imperial: R = ("imperial", "imperial-nebraska", "Imperial", "Nebraska", 40.5169, -101.6432, "America/Denver", false, Seq("Imperial"), Seq(
    ("Imperial Theatre", "Imperial Theatre", "imperial-theatre", "Nebraska", "Imperial")
  ))
  private def p_independence_kansas: R = ("independence-kansas", "independence-kansas", "Independence", "Kansas", 37.2242, -95.7083, "America/Chicago", false, Seq("Independence"), Seq(
    ("Independence Cinemas", "Independence Cinemas", "independence-cinemas", "Kansas", "Independence")
  ))
  private def p_indian_lake: R = ("indian-lake", "indian-lake-new-york", "Indian Lake", "New York", 43.7822, -74.2671, "America/New_York", false, Seq("Indian Lake"), Seq(
    ("Indian Lake Theater", "Indian Lake Theater", "indian-lake-theater", "New York", "Indian Lake")
  ))
  private def p_indian_trail: R = ("indian-trail", "indian-trail-north-carolina", "Indian Trail", "North Carolina", 35.0768, -80.6692, "America/New_York", true, Seq("Indian Trail", "Monroe"), Seq(
    ("GTC Sun Valley Indian Trail", "GTC Sun Valley Indian Trail", "gtc-sun-valley-indian-trail", "North Carolina", "Indian Trail"),
    ("NCG Monroe", "NCG Monroe", "ncg-monroe", "North Carolina", "Monroe")
  ))
  private def p_indiana: R = ("indiana", "indiana-pennsylvania", "Indiana", "Pennsylvania", 40.6215, -79.1525, "America/New_York", false, Seq("Indiana"), Seq(
    ("Palace Gardens Drive-in Indiana", "Palace Gardens Drive-in Indiana", "palace-gardens-drive-in-indiana", "Pennsylvania", "Indiana"),
    ("Tuxedo Indiana Mall (formerly MovieScoop)", "Tuxedo Indiana Mall (formerly MovieScoop)", "moviescoop-indiana-mall", "Pennsylvania", "Indiana")
  ))
  private def p_inez: R = ("inez", "inez-kentucky", "Inez", "Kentucky", 37.8665, -82.5388, "America/New_York", false, Seq("Inez"), Seq(
    ("Movies on Main Inez", "Movies on Main Inez", "movies-on-main-inez", "Kentucky", "Inez")
  ))
  private def p_international_falls: R = ("international-falls", "international-falls-minnesota", "International Falls", "Minnesota", 48.6011, -93.411, "America/Chicago", false, Seq("International Falls"), Seq(
    ("Cine International Falls 5", "Cine International Falls 5", "cine-international-falls-5", "Minnesota", "International Falls")
  ))
  private def p_inverness: R = ("inverness", "inverness-florida", "Inverness", "Florida", 28.8358, -82.3304, "America/New_York", false, Seq("Inverness"), Seq(
    ("Regal Citrus Cinemas Inverness", "Regal Citrus Cinemas Inverness", "regal-citrus-cinemas-inverness", "Florida", "Inverness"),
    ("Valerie Theatre Inverness", "Valerie Theatre Inverness", "valerie-theatre-inverness", "Florida", "Inverness")
  ))
  private def p_ionia: R = ("ionia", "ionia-michigan", "Ionia", "Michigan", 42.9873, -85.0711, "America/Detroit", true, Seq("Ionia", "Greenville", "Lowell", "Crystal"), Seq(
    ("GQT Ada-Lowell 5", "GQT Ada-Lowell 5", "gqt-ada-lowell-5", "Michigan", "Lowell"),
    ("NCG Greenville Cinema", "NCG Greenville Cinema", "ncg-greenville-cinema", "Michigan", "Greenville"),
    ("Ionia Theatre", "Ionia Theatre", "ionia-theatre", "Michigan", "Ionia"),
    ("Crystal Box Office", "Crystal Box Office", "crystal-box-office", "Michigan", "Crystal")
  ))
  private def p_iowa_city: R = ("iowa-city", "iowa-city-iowa", "Iowa City", "Iowa", 41.6611, -91.5302, "America/Chicago", true, Seq("Iowa City", "Coralville"), Seq(
    ("FilmScene on The Ped Mall", "FilmScene on The Ped Mall", "filmscene-on-the-ped-mall", "Iowa", "Iowa City"),
    ("Marcus Sycamore Cinema Iowa City", "Marcus Sycamore Cinema Iowa City", "marcus-sycamore-cinema-iowa-city", "Iowa", "Iowa City"),
    ("Marcus Coral Ridge Cinema", "Marcus Coral Ridge Cinema", "marcus-coral-ridge-cinema", "Iowa", "Coralville")
  ))
  private def p_iowa_falls: R = ("iowa-falls", "iowa-falls-iowa", "Iowa Falls", "Iowa", 42.5225, -93.2513, "America/Chicago", true, Seq("Iowa Falls", "Hampton", "Eldora"), Seq(
    ("Metropolitan Opera House", "Metropolitan Opera House", "metropolitan-opera-house", "Iowa", "Iowa Falls"),
    ("Windsor Theatre Hampton", "Windsor Theatre Hampton", "windsor-theatre-hampton", "Iowa", "Hampton"),
    ("Grand Theatre Eldora", "Grand Theatre Eldora", "grand-theatre-eldora", "Iowa", "Eldora")
  ))
  private def p_ironwood: R = ("ironwood", "ironwood-michigan", "Ironwood", "Michigan", 46.4547, -90.171, "America/Menominee", false, Seq("Ironwood"), Seq(
    ("Ironwood Theatre", "Ironwood Theatre", "ironwood-theatre", "Michigan", "Ironwood")
  ))
  private def p_issaquah: R = ("issaquah", "issaquah-washington", "Issaquah", "Washington", 47.5301, -122.0326, "America/Los_Angeles", true, Seq("Issaquah", "Tukwila", "North Bend"), Seq(
    ("AMC Southcenter 16 Tukwila", "AMC Southcenter 16 Tukwila", "amc-southcenter-16-tukwila", "Washington", "Tukwila"),
    ("Big Picture Issaquah", "Big Picture Issaquah", "big-picture-issaquah", "Washington", "Issaquah"),
    ("Regal Issaquah Highland", "Regal Issaquah Highland", "regal-issaquah-highland", "Washington", "Issaquah"),
    ("North Bend Theatre", "North Bend Theatre", "north-bend-theatre", "Washington", "North Bend")
  ))
  private def p_ithaca: R = ("ithaca", "ithaca-new-york", "Ithaca", "New York", 42.4406, -76.4966, "America/New_York", false, Seq("Ithaca"), Seq(
    ("Cinemapolis Ithaca", "Cinemapolis Ithaca", "cinemapolis-ithaca", "New York", "Ithaca"),
    ("Cornell Cinema", "Cornell Cinema", "cornell-cinema", "New York", "Ithaca"),
    ("Regal Ithaca Mall Stadium", "Regal Ithaca Mall Stadium", "regal-ithaca-mall-stadium", "New York", "Ithaca")
  ))
  private def p_jackson: R = ("jackson", "jackson-tennessee", "Jackson", "Tennessee", 35.6145, -88.814, "America/Chicago", false, Seq("Jackson"), Seq(
    ("Empire Theatres Jackson", "Empire Theatres Jackson", "empire-theatres-jackson", "Tennessee", "Jackson"),
    ("Hollywood Cinema 16 Jackson", "Hollywood Cinema 16 Jackson", "hollywood-cinema-16-jackson", "Tennessee", "Jackson")
  ))
  private def p_jackson_california: R = ("jackson-california", "jackson-california", "Jackson", "California", 38.3488, -120.7741, "America/Los_Angeles", false, Seq("Jackson"), Seq(
    ("Jackson Cinema is D'Place", "Jackson Cinema is D'Place", "jackson-cinema-is-dplace", "California", "Jackson")
  ))
  private def p_jackson_michigan: R = ("jackson-michigan", "jackson-michigan", "Jackson", "Michigan", 42.2459, -84.4013, "America/Detroit", false, Seq("Jackson"), Seq(
    ("GQT Jackson 10", "GQT Jackson 10", "gqt-jackson-10", "Michigan", "Jackson"),
    ("Michigan Theatre of Jackson", "Michigan Theatre of Jackson", "michigan-theatre-of-jackson", "Michigan", "Jackson")
  ))
  private def p_jackson_ohio: R = ("jackson-ohio", "jackson-ohio", "Jackson", "Ohio", 39.052, -82.6365, "America/New_York", false, Seq("Jackson"), Seq(
    ("Tri City Jackson", "Tri City Jackson", "tri-city-jackson", "Ohio", "Jackson")
  ))
  private def p_jackson_wyoming: R = ("jackson-wyoming", "jackson-wyoming", "Jackson", "Wyoming", 43.4799, -110.7624, "America/Denver", false, Seq("Jackson"), Seq(
    ("Movieworks Cinema Jackson", "Movieworks Cinema Jackson", "movieworks-cinema-jackson", "Wyoming", "Jackson")
  ))
  private def p_jacksonville_illinois: R = ("jacksonville-illinois", "jacksonville-illinois", "Jacksonville", "Illinois", 39.7339, -90.229, "America/Chicago", false, Seq("Jacksonville"), Seq(
    ("Illinois Theatre Jacksonville", "Illinois Theatre Jacksonville", "illinois-theatre-jacksonville", "Illinois", "Jacksonville"),
    ("RMC Jacksonville", "RMC Jacksonville", "rmc-jacksonville", "Illinois", "Jacksonville")
  ))
  private def p_jacksonville_north_carolina: R = ("jacksonville-north-carolina", "jacksonville-north-carolina", "Jacksonville", "North Carolina", 34.754, -77.4302, "America/New_York", false, Seq("Jacksonville"), Seq(
    ("AMC CLASSIC Jacksonville 16", "AMC CLASSIC Jacksonville 16", "amc-classic-jacksonville-16", "North Carolina", "Jacksonville"),
    ("AMC CLASSIC Kalli 12", "AMC CLASSIC Kalli 12", "amc-classic-kalli-12", "North Carolina", "Jacksonville")
  ))
  private def p_jacksonville_texas: R = ("jacksonville-texas", "jacksonville-texas", "Jacksonville", "Texas", 31.9638, -95.2705, "America/Chicago", false, Seq("Jacksonville"), Seq(
    ("Hometown Cinemas Jacksonville", "Hometown Cinemas Jacksonville", "hometown-cinemas-jacksonville", "Texas", "Jacksonville")
  ))
  private def p_jamestown: R = ("jamestown", "jamestown-north-dakota", "Jamestown", "North Dakota", 46.9105, -98.7084, "America/Chicago", false, Seq("Jamestown"), Seq(
    ("Bison 6 Cinema", "Bison 6 Cinema", "bison-6-cinema", "North Dakota", "Jamestown")
  ))
  private def p_jamestown_new_york: R = ("jamestown-new-york", "jamestown-new-york", "Jamestown", "New York", 42.097, -79.2353, "America/New_York", true, Seq("Jamestown", "Warren", "Lakewood", "Mayville"), Seq(
    ("Chautauqua Cinema", "Chautauqua Cinema", "chautauqua-cinema", "New York", "Chautauqua"),
    ("Lakewood Cinema 8", "Lakewood Cinema 8", "lakewood-cinema-8", "New York", "Lakewood"),
    ("Silver Star Cinemas Warren", "Silver Star Cinemas Warren", "silver-star-cinemas-warren", "Pennsylvania", "Warren"),
    ("Reg Lenna Civic Center Jamestown", "Reg Lenna Civic Center Jamestown", "reg-lenna-civic-center-jamestown", "New York", "Jamestown")
  ))
  private def p_jamestown_tennessee: R = ("jamestown-tennessee", "jamestown-tennessee", "Jamestown", "Tennessee", 36.4276, -84.9319, "America/Chicago", false, Seq("Jamestown"), Seq(
    ("Castle Twin Jamestown", "Castle Twin Jamestown", "castle-twin-jamestown", "Tennessee", "East Jamestown")
  ))
  private def p_janesville: R = ("janesville", "janesville-wisconsin", "Janesville", "Wisconsin", 42.6828, -89.0187, "America/Chicago", true, Seq("Janesville", "Beloit", "Machesney Park", "Whitewater"), Seq(
    ("Classic Cinemas Beloit", "Classic Cinemas Beloit", "classic-cinemas-beloit", "Wisconsin", "Beloit"),
    ("Theatres Of Whitewater", "Theatres Of Whitewater", "theatres-of-whitewater", "Wisconsin", "Whitewater"),
    ("AMC Machesney Park 14", "AMC Machesney Park 14", "amc-machesney-park-14", "Illinois", "Machesney Park"),
    ("Movies Janesville", "Movies Janesville", "movies-janesville", "Wisconsin", "Janesville")
  ))
  private def p_jasper: R = ("jasper", "jasper-indiana", "Jasper", "Indiana", 38.3914, -86.9311, "America/Indiana/Vincennes", false, Seq("Jasper"), Seq(
    ("Showplace Cinemas Jasper Cinema", "Showplace Cinemas Jasper Cinema", "showplace-cinemas-jasper-cinema", "Indiana", "Jasper")
  ))
  private def p_jefferson_city: R = ("jefferson-city", "jefferson-city-missouri", "Jefferson City", "Missouri", 38.5767, -92.1735, "America/Chicago", false, Seq("Jefferson City"), Seq(
    ("GQT Capital 8", "GQT Capital 8", "gqt-capital-8", "Missouri", "Jefferson City"),
    ("Capitol City Cinema", "Capitol City Cinema", "capitol-city-cinema", "Missouri", "Jefferson City")
  ))
  private def p_jeffersonville: R = ("jeffersonville", "jeffersonville-indiana", "Jeffersonville", "Indiana", 38.2776, -85.7372, "America/Kentucky/Louisville", true, Seq("Jeffersonville", "New Albany", "La Grange", "Georgetown", "Corydon"), Seq(
    ("Corydon Cinemas", "Corydon Cinemas", "corydon-cinemas", "Indiana", "Corydon"),
    ("Georgetown Drive-in", "Georgetown Drive-in", "georgetown-drive-in", "Indiana", "Georgetown"),
    ("Phoenix Theatres New Albany 16", "Phoenix Theatres New Albany 16", "phoenix-theatres-new-albany-16", "Indiana", "New Albany"),
    ("Sauerbeck Family Drive-in", "Sauerbeck Family Drive-in", "sauerbeck-family-drive-in", "Kentucky", "La Grange"),
    ("Xscape Jeffersonville 12", "Xscape Jeffersonville 12", "xscape-jeffersonville-12", "Indiana", "Jeffersonville")
  ))
  private def p_jerseyville: R = ("jerseyville", "jerseyville-illinois", "Jerseyville", "Illinois", 39.12, -90.3285, "America/Chicago", false, Seq("Jerseyville"), Seq(
    ("Stadium Theater Jerseyville", "Stadium Theater Jerseyville", "stadium-theater-jerseyville", "Illinois", "Jerseyville")
  ))
  private def p_jesup: R = ("jesup", "jesup-georgia", "Jesup", "Georgia", 31.6078, -81.8863, "America/New_York", false, Seq("Jesup"), Seq(
    ("Historic Strand Dinner Cinema", "Historic Strand Dinner Cinema", "historic-strand-dinner-cinema", "Georgia", "Jesup"),
    ("Jesup Drive-in Theatre", "Jesup Drive-in Theatre", "jesup-drive-in-theatre", "Georgia", "Jesup")
  ))
  private def p_johnson_city: R = ("johnson-city", "johnson-city-tennessee", "Johnson City", "Tennessee", 36.3134, -82.3535, "America/New_York", true, Seq("Johnson City", "Kingsport", "Elizabethton"), Seq(
    ("AMC Johnson City 14", "AMC Johnson City 14", "amc-johnson-city-14", "Tennessee", "Johnson City"),
    ("Bud Frank Cinema", "Bud Frank Cinema", "bud-frank-cinema", "Tennessee", "Johnson City"),
    ("State Line Drive-in Elizabethton", "State Line Drive-in Elizabethton", "state-line-drive-in-elizabethton", "Tennessee", "Elizabethton"),
    ("NCG Kingsport", "NCG Kingsport", "ncg-kingsport", "Tennessee", "Kingsport")
  ))
  private def p_johnstown: R = ("johnstown", "johnstown-pennsylvania", "Johnstown", "Pennsylvania", 40.3267, -78.922, "America/New_York", false, Seq("Johnstown"), Seq(
    ("Richland Cinemas Johnstown 10", "Richland Cinemas Johnstown 10", "richland-cinemas-johnstown-10", "Pennsylvania", "Johnstown"),
    ("Silver Drive-in Johnstown", "Silver Drive-in Johnstown", "silver-drive-in-johnstown", "Pennsylvania", "Johnstown"),
    ("Westwood Plaza & Theatre Cafe Johnstown", "Westwood Plaza & Theatre Cafe Johnstown", "westwood-plaza-theatre-cafe-johnstown", "Pennsylvania", "Johnstown")
  ))
  private def p_johnstown_new_york: R = ("johnstown-new-york", "johnstown-new-york", "Johnstown", "New York", 43.0067, -74.3676, "America/New_York", true, Seq("Johnstown", "Cobleskill", "Canajoharie", "Broadalbin"), Seq(
    ("Johnstown Movieplex", "Johnstown Movieplex", "johnstown-movieplex", "New York", "Johnstown"),
    ("Park Theatre Cobleskill", "Park Theatre Cobleskill", "park-theatre-cobleskill", "New York", "Cobleskill"),
    ("El Rancho Drive-in Palatine Bridge", "El Rancho Drive-in Palatine Bridge", "el-rancho-drive-in-palatine-bridge", "New York", "Palatine Bridge"),
    ("Ozoner 29 Drive-in Broadalbin", "Ozoner 29 Drive-in Broadalbin", "ozoner-29-drive-in-broadalbin", "New York", "Broadalbin")
  ))
  private def p_jonesboro: R = ("jonesboro", "jonesboro-arkansas", "Jonesboro", "Arkansas", 35.8423, -90.7043, "America/Chicago", false, Seq("Jonesboro"), Seq(
    ("Malco Jonesboro Studio Cinema", "Malco Jonesboro Studio Cinema", "malco-jonesboro-studio-cinema", "Arkansas", "Jonesboro"),
    ("Malco Jonesboro Towne Cinema", "Malco Jonesboro Towne Cinema", "malco-jonesboro-towne-cinema", "Arkansas", "Jonesboro")
  ))
  private def p_jonesville: R = ("jonesville", "jonesville-north-carolina", "Jonesville", "North Carolina", 36.2393, -80.8445, "America/New_York", false, Seq("Jonesville"), Seq(
    ("Starmount Jonesville 5", "Starmount Jonesville 5", "starmount-jonesville-5", "North Carolina", "Jonesville")
  ))
  private def p_joplin: R = ("joplin", "joplin-missouri", "Joplin", "Missouri", 37.0842, -94.5133, "America/Chicago", false, Seq("Joplin"), Seq(
    ("B&B Theatres Joplin 7", "B&B Theatres Joplin 7", "bb-theatres-joplin-7", "Missouri", "Joplin"),
    ("Regal Northstar Joplin", "Regal Northstar Joplin", "regal-northstar-joplin", "Missouri", "Joplin"),
    ("Royale Cinema Lounge", "Royale Cinema Lounge", "royale-cinema-lounge", "Missouri", "Joplin")
  ))
  private def p_julesburg: R = ("julesburg", "julesburg-colorado", "Julesburg", "Colorado", 40.9883, -102.2643, "America/Denver", false, Seq("Julesburg"), Seq(
    ("Hippodrome Arts Centre Julesburg", "Hippodrome Arts Centre Julesburg", "hippodrome-arts-centre-julesburg", "Colorado", "Julesburg")
  ))
  private def p_juneau: R = ("juneau", "juneau-alaska", "Juneau", "Alaska", 58.3019, -134.4197, "America/Juneau", false, Seq("Juneau"), Seq(
    ("Glacier Cinemas Juneau", "Glacier Cinemas Juneau", "glacier-cinemas-juneau", "Alaska", "Juneau"),
    ("Gold Town Theater", "Gold Town Theater", "gold-town-theater", "Alaska", "Juneau")
  ))
  private def p_jupiter: R = ("jupiter", "jupiter-florida", "Jupiter", "Florida", 26.9342, -80.0942, "America/New_York", true, Seq("Jupiter", "Palm Beach Gardens"), Seq(
    ("Cinepolis Jupiter", "Cinepolis Jupiter", "cinepolis-jupiter", "Florida", "Jupiter"),
    ("CMX Downtown at The Gardens 16", "CMX Downtown at The Gardens 16", "cmx-downtown-at-the-gardens-16", "Florida", "Palm Beach Gardens")
  ))
  private def p_kahului: R = ("kahului", "kahului-hawaii", "Kahului", "Hawaii", 20.8893, -156.4729, "Pacific/Honolulu", true, Seq("Kahului", "Kīhei"), Seq(
    ("Regal Maui Mall Kahului", "Regal Maui Mall Kahului", "regal-maui-mall-kahului", "Hawaii", "Kahului"),
    ("Regency Kihei Cinemas", "Regency Kihei Cinemas", "regency-kihei-cinemas", "Hawaii", "Kīhei")
  ))
  private def p_kailua_kona: R = ("kailua-kona", "kailua-kona-hawaii", "Kailua-Kona", "Hawaii", 19.6402, -155.9991, "Pacific/Honolulu", false, Seq("Kailua-Kona"), Seq(
    ("Regal Keauhou", "Regal Keauhou", "regal-keauhou", "Hawaii", "Kailua-Kona")
  ))
  private def p_kalamazoo: R = ("kalamazoo", "kalamazoo-michigan", "Kalamazoo", "Michigan", 42.2917, -85.5872, "America/Detroit", true, Seq("Kalamazoo", "Portage", "Allegan", "Plainwell", "Paw Paw"), Seq(
    ("Celebration! Crossroads", "Celebration! Crossroads", "celebration-crossroads", "Michigan", "Portage"),
    ("GQT Kalamazoo 10", "GQT Kalamazoo 10", "gqt-kalamazoo-10", "Michigan", "Kalamazoo"),
    ("KP Cinemas Kalamazoo", "KP Cinemas Kalamazoo", "kp-cinemas-kalamazoo", "Michigan", "Kalamazoo"),
    ("M-89 Cinema Otsego", "M-89 Cinema Otsego", "m-89-cinema-otsego", "Michigan", "Plainwell"),
    ("Old Regent Theatre Allegan", "Old Regent Theatre Allegan", "old-regent-theatre-allegan", "Michigan", "Allegan"),
    ("Strand Theatre Paw Paw", "Strand Theatre Paw Paw", "strand-theatre-paw-paw", "Michigan", "Paw Paw")
  ))
  private def p_kalispell: R = ("kalispell", "kalispell-montana", "Kalispell", "Montana", 48.1958, -114.3129, "America/Denver", false, Seq("Kalispell"), Seq(
    ("Cinemark Signature Stadium Kalispell 14", "Cinemark Signature Stadium Kalispell 14", "cinemark-signature-stadium-kalispell-14", "Montana", "Kalispell")
  ))
  private def p_kane: R = ("kane", "kane-pennsylvania", "Kane", "Pennsylvania", 41.6628, -78.8111, "America/New_York", false, Seq("Kane"), Seq(
    ("Family Drive-in Theatre Kane", "Family Drive-in Theatre Kane", "family-drive-in-theatre-kane", "Pennsylvania", "East Kane")
  ))
  private def p_kankakee: R = ("kankakee", "kankakee-illinois", "Kankakee", "Illinois", 41.12, -87.8611, "America/Chicago", true, Seq("Kankakee", "Bourbonnais"), Seq(
    ("Cinemark Bourbonnais 10", "Cinemark Bourbonnais 10", "cinemark-bourbonnais-10", "Illinois", "Bourbonnais"),
    ("Classic Cinemas Meadowview", "Classic Cinemas Meadowview", "classic-cinemas-meadowview", "Illinois", "Kankakee"),
    ("Classic Cinemas Paramount Kankakee", "Classic Cinemas Paramount Kankakee", "classic-cinemas-paramount-kankakee", "Illinois", "Kankakee")
  ))
  private def p_kanopolis: R = ("kanopolis", "kanopolis-kansas", "Kanopolis", "Kansas", 38.7178, -98.1575, "America/Chicago", false, Seq("Kanopolis"), Seq(
    ("Kanopolis Drive-In Theatre", "Kanopolis Drive-In Theatre", "kanopolis-drive-in-theatre", "Kansas", "Kanopolis")
  ))
  private def p_kapolei: R = ("kapolei", "kapolei-hawaii", "Kapolei", "Hawaii", 21.3356, -158.0582, "Pacific/Honolulu", false, Seq("Kapolei"), Seq(
    ("Consolidated Theatres Kapolei", "Consolidated Theatres Kapolei", "consolidated-theatres-kapolei", "Hawaii", "Kapolei"),
    ("Consolidated Theatres Olino", "Consolidated Theatres Olino", "consolidated-theatres-olino", "Hawaii", "Kapolei"),
    ("Regal Kapolei Commons", "Regal Kapolei Commons", "regal-kapolei-commons", "Hawaii", "Kapolei")
  ))
  private def p_katy: R = ("katy", "katy-texas", "Katy", "Texas", 29.7858, -95.8244, "America/Chicago", false, Seq("Katy"), Seq(
    ("AMC Katy Mills 20", "AMC Katy Mills 20", "amc-katy-mills-20", "Texas", "Katy"),
    ("Cinemark Katy 19", "Cinemark Katy 19", "cinemark-katy-19", "Texas", "Katy"),
    ("Flix Brewhouse Katy", "Flix Brewhouse Katy", "alamo-drafthouse-lacenterra", "Texas", "Katy")
  ))
  private def p_kaunakakai: R = ("kaunakakai", "kaunakakai-hawaii", "Kaunakakai", "Hawaii", 21.0894, -157.0225, "Pacific/Honolulu", false, Seq("Kaunakakai"), Seq(
    ("The Mango Theater", "The Mango Theater", "the-mango-theater", "Hawaii", "Kaunakakai")
  ))
  private def p_kayenta: R = ("kayenta", "kayenta-arizona", "Kayenta", "Arizona", 36.7278, -110.2546, "America/Denver", false, Seq("Kayenta"), Seq(
    ("Black Mesa Twin Cinemas", "Black Mesa Twin Cinemas", "black-mesa-twin-cinemas", "Arizona", "Kayenta")
  ))
  private def p_kearney: R = ("kearney", "kearney-nebraska", "Kearney", "Nebraska", 40.6995, -99.0815, "America/Chicago", false, Seq("Kearney"), Seq(
    ("Golden Ticket Cinemas Hilltop 4", "Golden Ticket Cinemas Hilltop 4", "golden-ticket-cinemas-hilltop-4", "Nebraska", "Kearney"),
    ("The World Theatre Kearney", "The World Theatre Kearney", "the-world-theatre-kearney", "Nebraska", "Kearney")
  ))
  private def p_keene: R = ("keene", "keene-new-hampshire", "Keene", "New Hampshire", 42.9337, -72.2781, "America/New_York", false, Seq("Keene"), Seq(
    ("Keene Cinemas 6", "Keene Cinemas 6", "keene-cinemas-6", "New Hampshire", "Keene"),
    ("Colonial Theatre Keene", "Colonial Theatre Keene", "colonial-theatre-keene", "New Hampshire", "Keene"),
    ("Showroom Keene", "Showroom Keene", "showroom-keene", "New Hampshire", "South Keene")
  ))
  private def p_kelso: R = ("kelso", "kelso-washington", "Kelso", "Washington", 46.1468, -122.9084, "America/Los_Angeles", false, Seq("Kelso"), Seq(
    ("Kelso Pub", "Kelso Pub", "kelso-pub", "Washington", "Kelso"),
    ("Regal Three Rivers Kelso", "Regal Three Rivers Kelso", "regal-three-rivers-kelso", "Washington", "Kelso")
  ))
  private def p_kemmerer: R = ("kemmerer", "kemmerer-wyoming", "Kemmerer", "Wyoming", 41.7925, -110.5377, "America/Denver", false, Seq("Kemmerer"), Seq(
    ("Victory Theatre Kemmerer", "Victory Theatre Kemmerer", "victory-theatre-kemmerer", "Wyoming", "Kemmerer")
  ))
  private def p_kenai: R = ("kenai", "kenai-alaska", "Kenai", "Alaska", 60.5544, -151.2583, "America/Anchorage", true, Seq("Kenai", "Soldotna"), Seq(
    ("Kenai Cinemas", "Kenai Cinemas", "kenai-cinemas", "Alaska", "Kenai"),
    ("Orca Theater", "Orca Theater", "orca-theater", "Alaska", "Soldotna")
  ))
  private def p_kenmare: R = ("kenmare", "kenmare-north-dakota", "Kenmare", "North Dakota", 48.6748, -102.0827, "America/Chicago", false, Seq("Kenmare"), Seq(
    ("Kenmare Theatre", "Kenmare Theatre", "kenmare-theatre", "North Dakota", "Kenmare")
  ))
  private def p_kenner: R = ("kenner", "kenner-louisiana", "Kenner", "Louisiana", 29.9941, -90.2417, "America/Chicago", true, Seq("Kenner", "Harvey", "Harahan"), Seq(
    ("AMC Elmwood Palace 20", "AMC Elmwood Palace 20", "amc-elmwood-palace-20", "Louisiana", "Harahan"),
    ("AMC Westbank Palace 16", "AMC Westbank Palace 16", "amc-westbank-palace-16", "Louisiana", "Harvey"),
    ("Regal Grande Esplanade", "Regal Grande Esplanade", "regal-grande-esplanade", "Louisiana", "Kenner")
  ))
  private def p_kennesaw: R = ("kennesaw", "kennesaw-georgia", "Kennesaw", "Georgia", 34.0234, -84.6155, "America/New_York", true, Seq("Kennesaw", "Douglasville", "Canton", "Acworth", "Cartersville", "Hiram"), Seq(
    ("AMC Barrett Commons 24", "AMC Barrett Commons 24", "amc-barrett-commons-24", "Georgia", "Kennesaw"),
    ("AMC CLASSIC Cartersville 12", "AMC CLASSIC Cartersville 12", "amc-classic-cartersville-12", "Georgia", "Cartersville"),
    ("AMC CLASSIC Riverstone 15", "AMC CLASSIC Riverstone 15", "amc-classic-riverstone-15", "Georgia", "Canton"),
    ("AMC Hiram 14", "AMC Hiram 14", "amc-hiram-14", "Georgia", "Hiram"),
    ("NCG Acworth Cinema", "NCG Acworth Cinema", "ncg-acworth-cinema", "Georgia", "Acworth"),
    ("Regal Arbor Place Douglasville", "Regal Arbor Place Douglasville", "regal-arbor-place-douglasville", "Georgia", "Douglasville")
  ))
  private def p_kennett: R = ("kennett", "kennett-missouri", "Kennett", "Missouri", 36.2362, -90.0556, "America/Chicago", false, Seq("Kennett"), Seq(
    ("Palace Theater Kennett", "Palace Theater Kennett", "palace-theater-kennett", "Missouri", "Kennett")
  ))
  private def p_kennewick: R = ("kennewick", "kennewick-washington", "Kennewick", "Washington", 46.2112, -119.1372, "America/Los_Angeles", true, Seq("Kennewick", "Pasco", "Richland"), Seq(
    ("AMC CLASSIC Kennewick 12", "AMC CLASSIC Kennewick 12", "amc-classic-kennewick-12", "Washington", "Kennewick"),
    ("Fairchild Cinemas Southgate 10", "Fairchild Cinemas Southgate 10", "fairchild-cinemas-southgate-10", "Washington", "Kennewick"),
    ("Pasco Cinema 12", "Pasco Cinema 12", "pasco-cinema-12", "Washington", "Pasco"),
    ("Queensgate Cinemas Richland 12", "Queensgate Cinemas Richland 12", "queensgate-cinemas-richland-12", "Washington", "Richland")
  ))
  private def p_kenosha: R = ("kenosha", "kenosha-wisconsin", "Kenosha", "Wisconsin", 42.5847, -87.8212, "America/Chicago", true, Seq("Kenosha", "Gurnee", "Antioch", "Sturtevant"), Seq(
    ("Antioch Theatre", "Antioch Theatre", "antioch-theatre", "Illinois", "Antioch"),
    ("Cinemark Tinseltown USA Kenosha", "Cinemark Tinseltown USA Kenosha", "cinemark-tinseltown-usa-kenosha", "Wisconsin", "Kenosha"),
    ("Marcus Gurnee Mills Cinema", "Marcus Gurnee Mills Cinema", "marcus-gurnee-mills-cinema", "Illinois", "Gurnee"),
    ("Marcus Renaissance Sturtevant", "Marcus Renaissance Sturtevant", "marcus-renaissance-sturtevant", "Wisconsin", "Sturtevant")
  ))
  private def p_kenton: R = ("kenton", "kenton-ohio", "Kenton", "Ohio", 40.647, -83.6097, "America/New_York", false, Seq("Kenton"), Seq(
    ("Hi Road Drive-In Kenton", "Hi Road Drive-In Kenton", "hi-road-drive-in-kenton", "Ohio", "Kenton"),
    ("Kenton Theater", "Kenton Theater", "kenton-theater", "Ohio", "Kenton")
  ))
  private def p_keokuk: R = ("keokuk", "keokuk-iowa", "Keokuk", "Iowa", 40.3973, -91.3849, "America/Chicago", false, Seq("Keokuk"), Seq(
    ("The Revival Theater", "The Revival Theater", "the-revival-theater", "Iowa", "Keokuk")
  ))
  private def p_kernersville: R = ("kernersville", "kernersville-north-carolina", "Kernersville", "North Carolina", 36.1199, -80.0737, "America/New_York", false, Seq("Kernersville"), Seq(
    ("TREX Cinemas", "TREX Cinemas", "trex-cinemas", "North Carolina", "Kernersville")
  ))
  private def p_kerrville: R = ("kerrville", "kerrville-texas", "Kerrville", "Texas", 30.0474, -99.1403, "America/Chicago", false, Seq("Kerrville"), Seq(
    ("Rio Kerrville 10", "Rio Kerrville 10", "rio-kerrville-10", "Texas", "Kerrville")
  ))
  private def p_ketchum: R = ("ketchum", "ketchum-idaho", "Ketchum", "Idaho", 43.6807, -114.3637, "America/Boise", true, Seq("Ketchum", "Sun Valley"), Seq(
    ("Magic Lantern Cinema", "Magic Lantern Cinema", "magic-lantern-cinema", "Idaho", "Ketchum"),
    ("Argyros Performing Arts Center", "Argyros Performing Arts Center", "argyros-performing-arts-center", "Idaho", "Ketchum"),
    ("Sun Valley Opera House", "Sun Valley Opera House", "sun-valley-opera-house", "Idaho", "Sun Valley")
  ))
  private def p_kewanee: R = ("kewanee", "kewanee-illinois", "Kewanee", "Illinois", 41.2456, -89.9248, "America/Chicago", true, Seq("Kewanee", "Geneseo"), Seq(
    ("Central Cinema Geneseo", "Central Cinema Geneseo", "central-cinema-geneseo", "Illinois", "Geneseo"),
    ("Wanee Theatre Kewanee", "Wanee Theatre Kewanee", "wanee-theatre-kewanee", "Illinois", "Kewanee")
  ))
  private def p_key_west: R = ("key-west", "key-west-florida", "Key West", "Florida", 24.5552, -81.7816, "America/New_York", false, Seq("Key West"), Seq(
    ("Regal Key West", "Regal Key West", "regal-key-west", "Florida", "Key West"),
    ("Tropic Cinema", "Tropic Cinema", "tropic-cinema", "Florida", "Key West")
  ))
  private def p_kill_devil_hills: R = ("kill-devil-hills", "kill-devil-hills-north-carolina", "Kill Devil Hills", "North Carolina", 36.0307, -75.676, "America/New_York", true, Seq("Kill Devil Hills", "Manteo"), Seq(
    ("Pioneer Theatre Manteo", "Pioneer Theatre Manteo", "pioneer-theatre-manteo", "North Carolina", "Manteo"),
    ("R/C Kill Devil Hills 10", "R/C Kill Devil Hills 10", "rc-kill-devil-hills-10", "North Carolina", "Kill Devil Hills")
  ))
  private def p_kilmarnock: R = ("kilmarnock", "kilmarnock-virginia", "Kilmarnock", "Virginia", 37.7104, -76.3797, "America/New_York", false, Seq("Kilmarnock"), Seq(
    ("Compass Cinemas 6", "Compass Cinemas 6", "compass-cinemas-6", "Virginia", "Irvington")
  ))
  private def p_kimball: R = ("kimball", "kimball-nebraska", "Kimball", "Nebraska", 41.2358, -103.663, "America/Denver", false, Seq("Kimball"), Seq(
    ("Goodhand Theater Kimball", "Goodhand Theater Kimball", "goodhand-theater-kimball", "Nebraska", "Kimball")
  ))
  private def p_kingfisher: R = ("kingfisher", "kingfisher-oklahoma", "Kingfisher", "Oklahoma", 35.8614, -97.9317, "America/Chicago", false, Seq("Kingfisher"), Seq(
    ("89er Kingfisher", "89er Kingfisher", "89er-kingfisher", "Oklahoma", "Kingfisher")
  ))
  private def p_kingman: R = ("kingman", "kingman-arizona", "Kingman", "Arizona", 35.1894, -114.053, "America/Phoenix", false, Seq("Kingman"), Seq(
    ("Brenden Kingman 4", "Brenden Kingman 4", "brenden-kingman-4", "Arizona", "Kingman")
  ))
  private def p_kingman_kansas: R = ("kingman-kansas", "kingman-kansas", "Kingman", "Kansas", 37.6459, -98.1137, "America/Chicago", false, Seq("Kingman"), Seq(
    ("Kingman Theatre", "Kingman Theatre", "kingman-theatre", "Kansas", "Kingman")
  ))
  private def p_kingston: R = ("kingston", "kingston-new-york", "Kingston", "New York", 41.927, -73.9974, "America/New_York", true, Seq("Kingston", "Saugerties", "Rhinebeck", "Woodstock", "Red Hook", "Hyde Park", "Rosendale Village"), Seq(
    ("Lyceum Red Hook 7", "Lyceum Red Hook 7", "lyceum-red-hook-7", "New York", "Red Hook"),
    ("NCG Hudson Valley Mall Kingston", "NCG Hudson Valley Mall Kingston", "ncg-hudson-valley-mall-kingston", "New York", "Kingston"),
    ("Upstate Films Kingston", "Upstate Films Kingston", "upstate-films-kingston", "New York", "Kingston"),
    ("Orpheum Saugerties 3", "Orpheum Saugerties 3", "orpheum-saugerties-3", "New York", "Saugerties"),
    ("Roosevelt Theatre Hyde Park", "Roosevelt Theatre Hyde Park", "roosevelt-theatre-hyde-park", "New York", "Hyde Park"),
    ("Rosendale Theatre", "Rosendale Theatre", "rosendale-theatre", "New York", "Rosendale"),
    ("Starr Cinema Rhinebeck", "Starr Cinema Rhinebeck", "starr-cinema-rhinebeck", "New York", "Rhinebeck"),
    ("Tinker Street Cinema", "Tinker Street Cinema", "tinker-street-cinema", "New York", "Woodstock")
  ))
  private def p_kingston_massachusetts: R = ("kingston-massachusetts", "kingston-massachusetts", "Kingston", "Massachusetts", 41.9945, -70.7245, "America/New_York", true, Seq("Kingston", "Plymouth"), Seq(
    ("Plimoth Cinema", "Plimoth Cinema", "plimoth-cinema", "Massachusetts", "Plymouth"),
    ("Regal Independence Mall Kingston", "Regal Independence Mall Kingston", "regal-independence-mall-kingston", "Massachusetts", "Kingston")
  ))
  private def p_kinsley: R = ("kinsley", "kinsley-kansas", "Kinsley", "Kansas", 37.9231, -99.4098, "America/Chicago", false, Seq("Kinsley"), Seq(
    ("Palace Theatre Kinsley", "Palace Theatre Kinsley", "palace-theatre-kinsley", "Kansas", "Kinsley")
  ))
  private def p_kinston: R = ("kinston", "kinston-north-carolina", "Kinston", "North Carolina", 35.2627, -77.5816, "America/New_York", false, Seq("Kinston"), Seq(
    ("UEC Theatres Kinston", "UEC Theatres Kinston", "uec-theatres-kinston", "North Carolina", "Kinston")
  ))
  private def p_kirkland: R = ("kirkland", "kirkland-washington", "Kirkland", "Washington", 47.6815, -122.2087, "America/Los_Angeles", true, Seq("Kirkland", "Lynnwood", "Redmond", "Shoreline", "Bothell", "Edmonds", "Mountlake Terrace", "Woodinville"), Seq(
    ("AMC Alderwood Mall 16 Lynnwood", "AMC Alderwood Mall 16 Lynnwood", "amc-alderwood-mall-16-lynnwood", "Washington", "Lynnwood"),
    ("Regal Alderwood Lynnwood", "Regal Alderwood Lynnwood", "regal-alderwood-lynnwood", "Washington", "Lynnwood"),
    ("AMC Woodinville 12", "AMC Woodinville 12", "amc-woodinville-12", "Washington", "Woodinville"),
    ("Cinemark Totem Lake Kirkland", "Cinemark Totem Lake Kirkland", "cinemark-totem-lake-kirkland", "Washington", "Kirkland"),
    ("Cinemark Village at Totem Lake", "Cinemark Village at Totem Lake", "cinemark-village-at-totem-lake", "Washington", "Kirkland"),
    ("Landmark Crest Cinema Center", "Landmark Crest Cinema Center", "landmark-crest-cinema-center", "Washington", "Shoreline"),
    ("McMenamins Anderson School", "McMenamins Anderson School", "mcmenamins-anderson-school", "Washington", "Bothell"),
    ("Regal Bella Bottega Redmond", "Regal Bella Bottega Redmond", "regal-bella-bottega-redmond", "Washington", "Redmond"),
    ("Regal Cinebarre Mountlake", "Regal Cinebarre Mountlake", "regal-cinebarre-mountlake", "Washington", "Mountlake Terrace"),
    ("The Edmonds Theater", "The Edmonds Theater", "the-edmonds-theater", "Washington", "Edmonds")
  ))
  private def p_kirksville: R = ("kirksville", "kirksville-missouri", "Kirksville", "Missouri", 40.1947, -92.5833, "America/Chicago", false, Seq("Kirksville"), Seq(
    ("Downtown Cinema 8 Kirksville", "Downtown Cinema 8 Kirksville", "downtown-cinema-8-kirksville", "Missouri", "Kirksville")
  ))
  private def p_kissimmee: R = ("kissimmee", "kissimmee-florida", "Kissimmee", "Florida", 28.3047, -81.4167, "America/New_York", true, Seq("Kissimmee", "Saint Cloud", "Celebration"), Seq(
    ("AMC Disney Springs 24", "AMC Disney Springs 24", "amc-disney-springs-24", "Florida", "Lake Buena Vista"),
    ("Regal The Loop Kissimmee", "Regal The Loop Kissimmee", "regal-the-loop-kissimmee", "Florida", "Kissimmee"),
    ("Studio Movie Grill Sunset Walk", "Studio Movie Grill Sunset Walk", "studio-movie-grill-sunset-walk", "Florida", "Kissimmee"),
    ("St Cloud Twin", "St Cloud Twin", "st-cloud-twin", "Florida", "Saint Cloud")
  ))
  private def p_klamath_falls: R = ("klamath-falls", "klamath-falls-oregon", "Klamath Falls", "Oregon", 42.2249, -121.7817, "America/Los_Angeles", false, Seq("Klamath Falls"), Seq(
    ("Pelican Cinema Klamath Falls", "Pelican Cinema Klamath Falls", "pelican-cinema-klamath-falls", "Oregon", "Klamath Falls"),
    ("Ross Ragland Theater", "Ross Ragland Theater", "ross-ragland-theater", "Oregon", "Klamath Falls")
  ))
  private def p_knox: R = ("knox", "knox-indiana", "Knox", "Indiana", 41.2959, -86.625, "America/Indiana/Knox", true, Seq("Knox", "Winamac"), Seq(
    ("Isis Theatre Winamac", "Isis Theatre Winamac", "isis-theatre-winamac", "Indiana", "Winamac"),
    ("Melody Drive-In Theater Dayton", "Melody Drive-In Theater Dayton", "melody-drive-in-theater-dayton", "Indiana", "Knox")
  ))
  private def p_knoxville_iowa: R = ("knoxville-iowa", "knoxville-iowa", "Knoxville", "Iowa", 41.3208, -93.1094, "America/Chicago", true, Seq("Knoxville", "Osceola", "Chariton", "Corydon"), Seq(
    ("Lyric Theater Osceola", "Lyric Theater Osceola", "lyric-theater-osceola", "Iowa", "Osceola"),
    ("The Grand Theater Knoxville", "The Grand Theater Knoxville", "the-grand-theater-knoxville", "Iowa", "Knoxville"),
    ("Wayne Theatre Corydon", "Wayne Theatre Corydon", "wayne-theatre-corydon", "Iowa", "Corydon"),
    ("Vision II Movie Theatre", "Vision II Movie Theatre", "vision-ii-movie-theatre", "Iowa", "Chariton")
  ))
  private def p_kodiak: R = ("kodiak", "kodiak-alaska", "Kodiak", "Alaska", 57.7885, -152.4053, "America/Anchorage", false, Seq("Kodiak"), Seq(
    ("Orpheum Theatre Kodiak", "Orpheum Theatre Kodiak", "orpheum-theatre-kodiak", "Alaska", "Kodiak")
  ))
  private def p_kokomo: R = ("kokomo", "kokomo-indiana", "Kokomo", "Indiana", 40.4864, -86.1336, "America/Indiana/Indianapolis", true, Seq("Kokomo", "Tipton"), Seq(
    ("AMC CLASSIC Kokomo 12", "AMC CLASSIC Kokomo 12", "amc-classic-kokomo-12", "Indiana", "Kokomo"),
    ("Diana Theatre Tipton", "Diana Theatre Tipton", "diana-theatre-tipton", "Indiana", "Tipton")
  ))
  private def p_kutztown: R = ("kutztown", "kutztown-pennsylvania", "Kutztown", "Pennsylvania", 40.5173, -75.7774, "America/New_York", true, Seq("Kutztown", "Hamburg"), Seq(
    ("Strand Cinema Kutztown", "Strand Cinema Kutztown", "strand-cinema-kutztown", "Pennsylvania", "Kutztown"),
    ("Strand Theatre Hamburg", "Strand Theatre Hamburg", "strand-theatre-hamburg", "Pennsylvania", "Hamburg")
  ))
  private def p_kyle: R = ("kyle", "kyle-south-dakota", "Kyle", "South Dakota", 43.4211, -102.1705, "America/Denver", false, Seq("Kyle"), Seq(
    ("Nunpa Theatre Kyle", "Nunpa Theatre Kyle", "nunpa-theatre-kyle", "South Dakota", "Kyle")
  ))
  private def p_la_crosse: R = ("la-crosse", "la-crosse-wisconsin", "La Crosse", "Wisconsin", 43.8014, -91.2396, "America/Chicago", false, Seq("La Crosse"), Seq(
    ("Marcus La Crosse", "Marcus La Crosse", "marcus-la-crosse", "Wisconsin", "La Crosse"),
    ("Rivoli Theatre La Crosse", "Rivoli Theatre La Crosse", "rivoli-theatre-la-crosse", "Wisconsin", "La Crosse")
  ))
  private def p_la_grande: R = ("la-grande", "la-grande-oregon", "La Grande", "Oregon", 45.3246, -118.0877, "America/Los_Angeles", false, Seq("La Grande"), Seq(
    ("Granada La Grande", "Granada La Grande", "granada-la-grande", "Oregon", "La Grande"),
    ("La Grande Drive-in", "La Grande Drive-in", "la-grande-drive-in", "Oregon", "La Grande")
  ))
  private def p_la_jolla: R = ("la-jolla", "la-jolla-california", "La Jolla", "California", 32.8473, -117.2742, "America/Los_Angeles", false, Seq("La Jolla"), Seq(
    ("AMC La Jolla 12", "AMC La Jolla 12", "amc-la-jolla-12", "California", "La Jolla"),
    ("THE LOT La Jolla", "THE LOT La Jolla", "the-lot-la-jolla", "California", "La Jolla"),
    ("University of California San Diego", "University of California San Diego", "university-of-california-san-diego", "California", "La Jolla")
  ))
  private def p_la_junta: R = ("la-junta", "la-junta-colorado", "La Junta", "Colorado", 37.985, -103.5438, "America/Denver", true, Seq("La Junta", "Rocky Ford"), Seq(
    ("Fox Theatre La Junta", "Fox Theatre La Junta", "fox-theatre-la-junta", "Colorado", "La Junta"),
    ("Grand Theatre Rocky Ford", "Grand Theatre Rocky Ford", "grand-theatre-rocky-ford", "Colorado", "Rocky Ford")
  ))
  private def p_ladysmith: R = ("ladysmith", "ladysmith-wisconsin", "Ladysmith", "Wisconsin", 45.463, -91.104, "America/Chicago", false, Seq("Ladysmith"), Seq(
    ("Miner Theatre Ladysmith", "Miner Theatre Ladysmith", "miner-theatre-ladysmith", "Wisconsin", "Ladysmith")
  ))
  private def p_lafayette_indiana: R = ("lafayette-indiana", "lafayette-indiana", "Lafayette", "Indiana", 40.4167, -86.8753, "America/Indiana/Indianapolis", true, Seq("Lafayette", "West Lafayette", "Attica"), Seq(
    ("Devon Theatre Attica", "Devon Theatre Attica", "devon-theatre-attica", "Indiana", "Attica"),
    ("GQT Eastside 10", "GQT Eastside 10", "gqt-eastside-10", "Indiana", "Lafayette"),
    ("GQT Wabash Landing 9", "GQT Wabash Landing 9", "gqt-wabash-landing-9", "Indiana", "West Lafayette")
  ))
  private def p_lafayette_tennessee: R = ("lafayette-tennessee", "lafayette-tennessee", "Lafayette", "Tennessee", 36.5212, -86.0264, "America/Chicago", false, Seq("Lafayette"), Seq(
    ("Lafayette Cinema", "Lafayette Cinema", "lafayette-cinema", "Tennessee", "Lafayette"),
    ("Macon Drive-in Theatre Lafayette", "Macon Drive-in Theatre Lafayette", "macon-drive-in-theatre-lafayette", "Tennessee", "Lafayette")
  ))
  private def p_lagrange: R = ("lagrange", "lagrange-georgia", "LaGrange", "Georgia", 33.0393, -85.0313, "America/New_York", false, Seq("LaGrange"), Seq(
    ("AMC CLASSIC LaGrange 10", "AMC CLASSIC LaGrange 10", "amc-classic-lagrange-10", "Georgia", "LaGrange")
  ))
  private def p_laguna_niguel: R = ("laguna-niguel", "laguna-niguel-california", "Laguna Niguel", "California", 33.5225, -117.7075, "America/Los_Angeles", true, Seq("Laguna Niguel", "Mission Viejo", "San Clemente", "Aliso Viejo", "Rancho Santa Margarita", "San Joaquin Hills"), Seq(
    ("Cinepolis Laguna Niguel", "Cinepolis Laguna Niguel", "cinepolis-laguna-niguel", "California", "Laguna Niguel"),
    ("Director's Cut Cinema at Rancho Niguel", "Director's Cut Cinema at Rancho Niguel", "directors-cut-cinema-at-rancho-niguel", "California", "Laguna Niguel"),
    ("Cinepolis Rancho Santa Margarita", "Cinepolis Rancho Santa Margarita", "cinepolis-rancho-santa-margarita", "California", "Rancho Santa Margarita"),
    ("MetroLux Theatres San Clemente", "MetroLux Theatres San Clemente", "metrolux-theatres-san-clemente", "California", "San Clemente"),
    ("Regal Edwards Aliso Viejo", "Regal Edwards Aliso Viejo", "regal-edwards-aliso-viejo", "California", "Aliso Viejo"),
    ("Regal Edwards Kaleidoscope", "Regal Edwards Kaleidoscope", "regal-edwards-kaleidoscope", "California", "Mission Viejo"),
    ("New Port Theatre Corona Del Mar", "New Port Theatre Corona Del Mar", "new-port-theatre-corona-del-mar", "California", "Corona del Mar")
  ))
  private def p_lake_charles: R = ("lake-charles", "lake-charles-louisiana", "Lake Charles", "Louisiana", 30.2131, -93.2044, "America/Chicago", false, Seq("Lake Charles"), Seq(
    ("Cinemark Bistro Lake Charles", "Cinemark Bistro Lake Charles", "cinemark-bistro-lake-charles", "Louisiana", "Lake Charles"),
    ("Cinemark Lake Charles", "Cinemark Lake Charles", "cinemark-lake-charles", "Louisiana", "Lake Charles")
  ))
  private def p_lake_city: R = ("lake-city", "lake-city-florida", "Lake City", "Florida", 30.1897, -82.6393, "America/New_York", false, Seq("Lake City"), Seq(
    ("Regal UA 90 Lake City", "Regal UA 90 Lake City", "regal-ua-90-lake-city", "Florida", "Lake City")
  ))
  private def p_lake_city_colorado: R = ("lake-city-colorado", "lake-city-colorado", "Lake City", "Colorado", 38.03, -107.3153, "America/Denver", false, Seq("Lake City"), Seq(
    ("Mountaineer Theatre Lake City", "Mountaineer Theatre Lake City", "mountaineer-theatre-lake-city", "Colorado", "Lake City")
  ))
  private def p_lake_elsinore: R = ("lake-elsinore", "lake-elsinore-california", "Lake Elsinore", "California", 33.6681, -117.3273, "America/Los_Angeles", true, Seq("Lake Elsinore", "San Jacinto", "Banning", "Idyllwild"), Seq(
    ("Fox Cineplex is D'Place", "Fox Cineplex is D'Place", "fox-cineplex-is-dplace", "California", "Banning"),
    ("Regal Metro San Jacinto", "Regal Metro San Jacinto", "regal-metro-san-jacinto", "California", "San Jacinto"),
    ("Rustic Theatre Idyllwild", "Rustic Theatre Idyllwild", "rustic-theatre-idyllwild", "California", "Idyllwild"),
    ("UEC Theatres Diamond 8", "UEC Theatres Diamond 8", "uec-theatres-diamond-8", "California", "Lake Elsinore")
  ))
  private def p_lake_havasu_city: R = ("lake-havasu-city", "lake-havasu-city-arizona", "Lake Havasu City", "Arizona", 34.4839, -114.3225, "America/Phoenix", false, Seq("Lake Havasu City"), Seq(
    ("Movies Havasu", "Movies Havasu", "movies-havasu", "Arizona", "Lake Havasu City"),
    ("Star Cinemas Lake Havasu", "Star Cinemas Lake Havasu", "star-cinemas-lake-havasu", "Arizona", "Lake Havasu City")
  ))
  private def p_lake_jackson: R = ("lake-jackson", "lake-jackson-texas", "Lake Jackson", "Texas", 29.0339, -95.4344, "America/Chicago", false, Seq("Lake Jackson"), Seq(
    ("AMC CLASSIC Brazos Mall 14", "AMC CLASSIC Brazos Mall 14", "amc-classic-brazos-mall-14", "Texas", "Lake Jackson")
  ))
  private def p_lake_ozark: R = ("lake-ozark", "lake-ozark-missouri", "Lake Ozark", "Missouri", 38.1986, -92.6388, "America/Chicago", true, Seq("Lake Ozark", "Gravois Mills"), Seq(
    ("Marcus Eagles Landing 8", "Marcus Eagles Landing 8", "marcus-eagles-landing-8", "Missouri", "Lake Ozark"),
    ("VIP Lake West Cinemas Gravois Mills", "VIP Lake West Cinemas Gravois Mills", "vip-lake-west-cinemas-gravois-mills", "Missouri", "Gravois Mills")
  ))
  private def p_lake_placid: R = ("lake-placid", "lake-placid-new-york", "Lake Placid", "New York", 44.2796, -73.982, "America/New_York", false, Seq("Lake Placid"), Seq(
    ("Palace Theatre Lake Placid", "Palace Theatre Lake Placid", "palace-theatre-lake-placid", "New York", "Lake Placid")
  ))
  private def p_lakeport: R = ("lakeport", "lakeport-california", "Lakeport", "California", 39.0429, -122.9158, "America/Los_Angeles", false, Seq("Lakeport"), Seq(
    ("Lakeport Cinema 5", "Lakeport Cinema 5", "lakeport-cinema-5", "California", "Lakeport"),
    ("Lakeport Auto Movies Drive-In", "Lakeport Auto Movies Drive-In", "lakeport-auto-movies-drive-in", "California", "Lakeport")
  ))
  private def p_lakewood_washington: R = ("lakewood-washington", "lakewood-washington", "Lakewood", "Washington", 47.1718, -122.5185, "America/Los_Angeles", true, Seq("Lakewood", "Puyallup", "Gig Harbor", "North Puyallup"), Seq(
    ("Galaxy Theatres Gig Harbour", "Galaxy Theatres Gig Harbour", "galaxy-theatres-gig-harbour", "Washington", "Gig Harbor"),
    ("AMC Lakewood Mall 12", "AMC Lakewood Mall 12", "amc-lakewood-mall-12", "Washington", "Lakewood"),
    ("Regal Lakewood", "Regal Lakewood", "regal-lakewood", "Washington", "Lakewood"),
    ("Regal Longston Place Puyallup", "Regal Longston Place Puyallup", "regal-longston-place-puyallup", "Washington", "Puyallup"),
    ("Regal South Hill Puyallup", "Regal South Hill Puyallup", "regal-south-hill-puyallup", "Washington", "North Puyallup")
  ))
  private def p_lamar: R = ("lamar", "lamar-missouri", "Lamar", "Missouri", 37.495, -94.2766, "America/Chicago", false, Seq("Lamar"), Seq(
    ("Barco Drive-in Lamar", "Barco Drive-in Lamar", "barco-drive-in-lamar", "Missouri", "Lamar"),
    ("Plaza Theatre Lamar", "Plaza Theatre Lamar", "plaza-theatre-lamar", "Missouri", "Lamar")
  ))
  private def p_lamar_colorado: R = ("lamar-colorado", "lamar-colorado", "Lamar", "Colorado", 38.0872, -102.6208, "America/Denver", false, Seq("Lamar"), Seq(
    ("The LaMar", "The LaMar", "the-lamar", "Colorado", "Lamar")
  ))
  private def p_lamesa: R = ("lamesa", "lamesa-texas", "Lamesa", "Texas", 32.7376, -101.951, "America/Chicago", false, Seq("Lamesa"), Seq(
    ("Lamesa Movieland Theater", "Lamesa Movieland Theater", "lamesa-movieland-theater", "Texas", "Lamesa")
  ))
  private def p_lamoni: R = ("lamoni", "lamoni-iowa", "Lamoni", "Iowa", 40.6228, -93.9341, "America/Chicago", true, Seq("Lamoni", "Leon", "Mount Ayr"), Seq(
    ("Coliseum Theater Lamoni", "Coliseum Theater Lamoni", "coliseum-theater-lamoni", "Iowa", "Lamoni"),
    ("Princess Theater Mount Ayr", "Princess Theater Mount Ayr", "princess-theater-mount-ayr", "Iowa", "Mount Ayr"),
    ("SCIT Theatre Leon", "SCIT Theatre Leon", "scit-theatre-leon", "Iowa", "Leon")
  ))
  private def p_lanai_city: R = ("lanai-city", "lanai-city-hawaii", "Lanai City", "Hawaii", 20.8276, -156.924, "Pacific/Honolulu", false, Seq("Lanai City"), Seq(
    ("Hale Keaka Lana'i", "Hale Keaka Lana'i", "hale-keaka-lanai", "Hawaii", "Lanai City")
  ))
  private def p_lancaster: R = ("lancaster", "lancaster-pennsylvania", "Lancaster", "Pennsylvania", 40.0379, -76.3055, "America/New_York", false, Seq("Lancaster"), Seq(
    ("Reel Cinema Lancaster 7", "Reel Cinema Lancaster 7", "reel-cinema-lancaster-7", "Pennsylvania", "Lancaster"),
    ("Regal Manor Cinema Lancaster", "Regal Manor Cinema Lancaster", "regal-manor-cinema-lancaster", "Pennsylvania", "Lancaster"),
    ("Zoetropolis Cinema Stillhouse", "Zoetropolis Cinema Stillhouse", "zoetropolis-cinema-stillhouse", "Pennsylvania", "Lancaster")
  ))
  private def p_lancaster_new_hampshire: R = ("lancaster-new-hampshire", "lancaster-new-hampshire", "Lancaster", "New Hampshire", 44.489, -71.5692, "America/New_York", true, Seq("Lancaster", "Bethlehem"), Seq(
    ("Colonial Bethlehem", "Colonial Bethlehem", "colonial-bethlehem", "New Hampshire", "Bethlehem"),
    ("Rialto Theatre Lancaster", "Rialto Theatre Lancaster", "rialto-theatre-lancaster", "New Hampshire", "Lancaster")
  ))
  private def p_lancaster_new_york: R = ("lancaster-new-york", "lancaster-new-york", "Lancaster", "New York", 42.9006, -78.6703, "America/New_York", true, Seq("Lancaster", "Hamburg", "East Aurora", "Orchard Park"), Seq(
    ("Flix Stadium 10 Lancaster", "Flix Stadium 10 Lancaster", "flix-stadium-10-lancaster", "New York", "Lancaster"),
    ("Palace Theater Hamburg", "Palace Theater Hamburg", "palace-theater-hamburg", "New York", "Hamburg"),
    ("Regal Quaker Crossing", "Regal Quaker Crossing", "regal-quaker-crossing", "New York", "Orchard Park"),
    ("The Aurora Theatre", "The Aurora Theatre", "the-aurora-theatre", "New York", "East Aurora")
  ))
  private def p_lancaster_ohio: R = ("lancaster-ohio", "lancaster-ohio", "Lancaster", "Ohio", 39.7137, -82.5993, "America/New_York", false, Seq("Lancaster"), Seq(
    ("Cinemark River Valley Mall Lancaster", "Cinemark River Valley Mall Lancaster", "cinemark-river-valley-mall-lancaster", "Ohio", "Lancaster"),
    ("Skyview Drive-in Lancaster", "Skyview Drive-in Lancaster", "skyview-drive-in-lancaster", "Ohio", "Lancaster")
  ))
  private def p_lancaster_texas: R = ("lancaster-texas", "lancaster-texas", "Lancaster", "Texas", 32.5921, -96.7561, "America/Chicago", true, Seq("Lancaster", "Waxahachie", "Red Oak"), Seq(
    ("B&B Red Oak 12", "B&B Red Oak 12", "bb-red-oak-12", "Texas", "Red Oak"),
    ("Cinemark Lancaster 14", "Cinemark Lancaster 14", "cinemark-lancaster-14", "Texas", "Lancaster"),
    ("Hooky Entertainment Waxahachie", "Hooky Entertainment Waxahachie", "hooky-entertainment-waxahachie", "Texas", "Waxahachie")
  ))
  private def p_lancaster_wisconsin: R = ("lancaster-wisconsin", "lancaster-wisconsin", "Lancaster", "Wisconsin", 42.8475, -90.7107, "America/Chicago", false, Seq("Lancaster"), Seq(
    ("Grantland Theatre Lancaster", "Grantland Theatre Lancaster", "grantland-theatre-lancaster", "Wisconsin", "Lancaster")
  ))
  private def p_lander: R = ("lander", "lander-wyoming", "Lander", "Wyoming", 42.833, -108.7307, "America/Denver", false, Seq("Lander"), Seq(
    ("Grand Theatre Lander", "Grand Theatre Lander", "grand-theatre-lander", "Wyoming", "Lander")
  ))
  private def p_langdon: R = ("langdon", "langdon-north-dakota", "Langdon", "North Dakota", 48.76, -98.3682, "America/Chicago", false, Seq("Langdon"), Seq(
    ("Roxy Theatre Langdon", "Roxy Theatre Langdon", "roxy-theatre-langdon", "North Dakota", "Langdon")
  ))
  private def p_lansdale: R = ("lansdale", "lansdale-pennsylvania", "Lansdale", "Pennsylvania", 40.2415, -75.2838, "America/New_York", true, Seq("Lansdale", "Doylestown", "North Wales"), Seq(
    ("AMC 309 Cinema 9 North Wales", "AMC 309 Cinema 9 North Wales", "amc-309-cinema-9-north-wales", "Pennsylvania", "North Wales"),
    ("County Theater 3", "County Theater 3", "county-theater-3", "Pennsylvania", "Doylestown"),
    ("Flagship Cinemas Water Tower", "Flagship Cinemas Water Tower", "flagship-cinemas-water-tower", "Pennsylvania", "Lansdale")
  ))
  private def p_lapeer: R = ("lapeer", "lapeer-michigan", "Lapeer", "Michigan", 43.0514, -83.3188, "America/Detroit", true, Seq("Lapeer", "Grand Blanc"), Seq(
    ("NCG Grand Blanc", "NCG Grand Blanc", "ncg-grand-blanc", "Michigan", "Grand Blanc"),
    ("NCG Lapeer Cinema", "NCG Lapeer Cinema", "ncg-lapeer-cinema", "Michigan", "Lapeer")
  ))
  private def p_laramie: R = ("laramie", "laramie-wyoming", "Laramie", "Wyoming", 41.3114, -105.5911, "America/Denver", false, Seq("Laramie"), Seq(
    ("Regal Fox Laramie", "Regal Fox Laramie", "regal-fox-laramie", "Wyoming", "Fort Laramie"),
    ("Studio City Cinema Laramie", "Studio City Cinema Laramie", "studio-city-cinema-laramie", "Wyoming", "Laramie")
  ))
  private def p_largo: R = ("largo", "largo-florida", "Largo", "Florida", 27.9098, -82.7884, "America/New_York", true, Seq("Largo", "Palm Harbor", "Pinellas Park", "Seminole", "Oldsmar", "Saint Pete Beach"), Seq(
    ("Regal Largo Mall", "Regal Largo Mall", "regal-largo-mall", "Florida", "Largo"),
    ("Studio Movie Grill Seminole", "Studio Movie Grill Seminole", "studio-movie-grill-seminole", "Florida", "Seminole"),
    ("AMC Classic Palm Harbor 10", "AMC Classic Palm Harbor 10", "amc-classic-palm-harbor-10", "Florida", "Palm Harbor"),
    ("AMC Woodlands Square 20", "AMC Woodlands Square 20", "amc-woodlands-square-20", "Florida", "Oldsmar"),
    ("Regal Park Place Pinellas Park", "Regal Park Place Pinellas Park", "regal-park-place-pinellas-park", "Florida", "Pinellas Park"),
    ("Beach Theatre – St Pete Beach", "Beach Theatre – St Pete Beach", "beach-theatre--st-pete-beach", "Florida", "Saint Pete Beach")
  ))
  private def p_larkspur: R = ("larkspur", "larkspur-california", "Larkspur", "California", 37.9341, -122.5353, "America/Los_Angeles", true, Seq("Larkspur", "San Rafael", "Novato", "El Cerrito", "Mill Valley", "Tiburon", "Fairfax", "Woodacre"), Seq(
    ("Cinelounge Tiburon 3", "Cinelounge Tiburon 3", "cinelounge-tiburon-3", "California", "Tiburon"),
    ("Cinemark Century Rowland Plaza", "Cinemark Century Rowland Plaza", "cinemark-century-rowland-plaza", "California", "Novato"),
    ("Fairfax Theatre", "Fairfax Theatre", "fairfax-theatre", "California", "Fairfax"),
    ("Lark Theater", "Lark Theater", "lark-theater", "California", "Larkspur"),
    ("Larkspur Landing Cinema", "Larkspur Landing Cinema", "larkspur-landing-cinema", "California", "Larkspur"),
    ("Rialto Cinemas El Cerrito", "Rialto Cinemas El Cerrito", "rialto-cinemas-el-cerrito", "California", "El Cerrito"),
    ("Sequoia Cinema", "Sequoia Cinema", "sequoia-cinema", "California", "Mill Valley"),
    ("Skywalker Sound Nicasio", "Skywalker Sound Nicasio", "skywalker-sound-nicasio", "California", "Nicasio"),
    ("Smith Rafael Film Center", "Smith Rafael Film Center", "smith-rafael-film-center", "California", "San Rafael")
  ))
  private def p_larned: R = ("larned", "larned-kansas", "Larned", "Kansas", 38.1806, -99.0987, "America/Chicago", false, Seq("Larned"), Seq(
    ("State Theatre Larned", "State Theatre Larned", "state-theatre-larned", "Kansas", "Larned")
  ))
  private def p_las_vegas_new_mexico: R = ("las-vegas-new-mexico", "las-vegas-new-mexico", "Las Vegas", "New Mexico", 35.5939, -105.2239, "America/Denver", false, Seq("Las Vegas"), Seq(
    ("Fort Union Drive in Movie Theatre", "Fort Union Drive in Movie Theatre", "fort-union-drive-in-movie-theatre", "New Mexico", "Las Vegas"),
    ("Indigo Theater", "Indigo Theater", "indigo-theater", "New Mexico", "Las Vegas")
  ))
  private def p_laughlin: R = ("laughlin", "laughlin-nevada", "Laughlin", "Nevada", 35.1678, -114.573, "America/Los_Angeles", false, Seq("Laughlin"), Seq(
    ("Brenden Avi 8 Laughlin", "Brenden Avi 8 Laughlin", "brenden-avi-8-laughlin", "Nevada", "Laughlin"),
    ("Riverside Cinema 6", "Riverside Cinema 6", "riverside-cinema-6", "Nevada", "Laughlin")
  ))
  private def p_laurel: R = ("laurel", "laurel-mississippi", "Laurel", "Mississippi", 31.6941, -89.1306, "America/Chicago", false, Seq("Laurel"), Seq(
    ("Southwest Theaters Sawmill Square 5", "Southwest Theaters Sawmill Square 5", "southwest-theaters-sawmill-square-5", "Mississippi", "Laurel")
  ))
  private def p_laurinburg: R = ("laurinburg", "laurinburg-north-carolina", "Laurinburg", "North Carolina", 34.7741, -79.4628, "America/New_York", false, Seq("Laurinburg"), Seq(
    ("Cinema Laurinburg", "Cinema Laurinburg", "cinema-laurinburg", "North Carolina", "East Laurinburg")
  ))
  private def p_lawrence: R = ("lawrence", "lawrence-kansas", "Lawrence", "Kansas", 38.9717, -95.2352, "America/Chicago", false, Seq("Lawrence"), Seq(
    ("Liberty Hall Lawrence", "Liberty Hall Lawrence", "liberty-hall-lawrence", "Kansas", "Lawrence"),
    ("Regal Southwind Lawrence", "Regal Southwind Lawrence", "regal-southwind-lawrence", "Kansas", "Lawrence")
  ))
  private def p_lawrenceburg: R = ("lawrenceburg", "lawrenceburg-tennessee", "Lawrenceburg", "Tennessee", 35.2423, -87.3347, "America/Chicago", true, Seq("Lawrenceburg", "Pulaski"), Seq(
    ("Crockett Theatre Lawrenceburg", "Crockett Theatre Lawrenceburg", "crockett-theatre-lawrenceburg", "Tennessee", "Lawrenceburg"),
    ("Martin Movie Theater Pulaski", "Martin Movie Theater Pulaski", "martin-movie-theater-pulaski", "Tennessee", "Pulaski")
  ))
  private def p_lawrenceville: R = ("lawrenceville", "lawrenceville-georgia", "Lawrenceville", "Georgia", 33.9562, -83.988, "America/New_York", false, Seq("Lawrenceville"), Seq(
    ("AMC Colonial Lawrenceville 18", "AMC Colonial Lawrenceville 18", "amc-colonial-lawrenceville-18", "Georgia", "Lawrenceville"),
    ("AMC DINE-IN Webb Gin 11", "AMC DINE-IN Webb Gin 11", "amc-dine-in-webb-gin-11", "Georgia", "Lawrenceville"),
    ("AMC Sugarloaf Mills 18", "AMC Sugarloaf Mills 18", "amc-sugarloaf-mills-18", "Georgia", "Lawrenceville")
  ))
  private def p_lawton: R = ("lawton", "lawton-oklahoma", "Lawton", "Oklahoma", 34.6087, -98.3903, "America/Chicago", false, Seq("Lawton"), Seq(
    ("AMC CLASSIC Patriot 13", "AMC CLASSIC Patriot 13", "amc-classic-patriot-13", "Oklahoma", "Lawton"),
    ("Vaska Lawton", "Vaska Lawton", "vaska-lawton", "Oklahoma", "Lawton")
  ))
  private def p_layton: R = ("layton", "layton-utah", "Layton", "Utah", 41.0602, -111.9711, "America/Denver", true, Seq("Layton", "Kaysville", "Syracuse", "Farmington", "Clinton", "Centerville"), Seq(
    ("AMC Layton Hills 9", "AMC Layton Hills 9", "amc-layton-hills-9", "Utah", "East Layton Hills"),
    ("Cinemark Layton", "Cinemark Layton", "cinemark-layton", "Utah", "Layton"),
    ("Cinemark Farmington at Station Park", "Cinemark Farmington at Station Park", "cinemark-farmington-at-station-park", "Utah", "Farmington"),
    ("Fat Cats Clinton", "Fat Cats Clinton", "fat-cats-clinton", "Utah", "Clinton"),
    ("Kaysville Theatre", "Kaysville Theatre", "kaysville-theatre", "Utah", "Kaysville"),
    ("Megaplex Legacy Crossing Centervile", "Megaplex Legacy Crossing Centervile", "megaplex-legacy-crossing-centervile", "Utah", "Centerville"),
    ("UEC Theatres Syracuse", "UEC Theatres Syracuse", "uec-theatres-syracuse", "Utah", "Syracuse")
  ))
  private def p_le_mars: R = ("le-mars", "le-mars-iowa", "Le Mars", "Iowa", 42.7942, -96.1656, "America/Chicago", true, Seq("Le Mars", "Orange City"), Seq(
    ("Holland Plaza Theatre", "Holland Plaza Theatre", "holland-plaza-theatre", "Iowa", "Orange City"),
    ("Royal 3 Cinema Le Mars", "Royal 3 Cinema Le Mars", "royal-3-cinema-le-mars", "Iowa", "Le Mars")
  ))
  private def p_leachville: R = ("leachville", "leachville-arkansas", "Leachville", "Arkansas", 35.9359, -90.2579, "America/Chicago", false, Seq("Leachville"), Seq(
    ("Melody Drive-In Knox", "Melody Drive-In Knox", "melody-drive-in-knox", "Arkansas", "Leachville")
  ))
  private def p_lebanon: R = ("lebanon", "lebanon-tennessee", "Lebanon", "Tennessee", 36.2081, -86.2911, "America/Chicago", true, Seq("Lebanon", "Smyrna", "Watertown"), Seq(
    ("Malco Smyrna Cinema 14", "Malco Smyrna Cinema 14", "malco-smyrna-cinema-14", "Tennessee", "Smyrna"),
    ("Stardust Drive-In Watertown", "Stardust Drive-In Watertown", "stardust-drive-in-watertown", "Tennessee", "Watertown"),
    ("UEC Theatres Roxy Lebanon", "UEC Theatres Roxy Lebanon", "uec-theatres-roxy-lebanon", "Tennessee", "Lebanon"),
    ("The Capitol Theatre Lebanon", "The Capitol Theatre Lebanon", "the-capitol-theatre-lebanon", "Tennessee", "Lebanon")
  ))
  private def p_lebanon_missouri: R = ("lebanon-missouri", "lebanon-missouri", "Lebanon", "Missouri", 37.6806, -92.6638, "America/Chicago", false, Seq("Lebanon"), Seq(
    ("B&B Lebanon Ritz 8", "B&B Lebanon Ritz 8", "bb-lebanon-ritz-8", "Missouri", "Lebanon")
  ))
  private def p_lebanon_ohio: R = ("lebanon-ohio", "lebanon-ohio", "Lebanon", "Ohio", 39.4353, -84.203, "America/New_York", true, Seq("Lebanon", "Wilmington", "Blanchester"), Seq(
    ("Colony Sqaure 7 Lebanon", "Colony Sqaure 7 Lebanon", "colony-sqaure-7-lebanon", "Ohio", "Lebanon"),
    ("Movies Blanchester 3", "Movies Blanchester 3", "movies-blanchester-3", "Ohio", "Blanchester"),
    ("Wilmington Plaza Cinema 5", "Wilmington Plaza Cinema 5", "wilmington-plaza-cinema-5", "Ohio", "Wilmington")
  ))
  private def p_lee_s_summit: R = ("lee-s-summit", "lee-s-summit-missouri", "Lee's Summit", "Missouri", 38.9108, -94.3822, "America/Chicago", true, Seq("Lee's Summit", "Belton", "Grain Valley", "Harrisonville"), Seq(
    ("B&B Grain Valley Marketplace", "B&B Grain Valley Marketplace", "bb-grain-valley-marketplace", "Missouri", "Grain Valley"),
    ("B&B Harrisonville Cineplex", "B&B Harrisonville Cineplex", "bb-harrisonville-cineplex", "Missouri", "Harrisonville"),
    ("B&B Lee's Summit 16", "B&B Lee's Summit 16", "bb-lees-summit-16", "Missouri", "Lee's Summit"),
    ("B&B Lee's Summit Longview 7", "B&B Lee's Summit Longview 7", "bb-lees-summit-longview-7", "Missouri", "Lee's Summit"),
    ("Belton Cinema 8", "Belton Cinema 8", "belton-cinema-8", "Missouri", "Belton")
  ))
  private def p_leesville: R = ("leesville", "leesville-louisiana", "Leesville", "Louisiana", 31.1435, -93.261, "America/Chicago", false, Seq("Leesville"), Seq(
    ("Regal Lee Hills Leesville", "Regal Lee Hills Leesville", "regal-lee-hills-leesville", "Louisiana", "Leesville")
  ))
  private def p_lemmon: R = ("lemmon", "lemmon-south-dakota", "Lemmon", "South Dakota", 45.9408, -102.1593, "America/Denver", false, Seq("Lemmon"), Seq(
    ("Palace Theatre Lemmon", "Palace Theatre Lemmon", "palace-theatre-lemmon", "South Dakota", "Lemmon")
  ))
  private def p_lemoore: R = ("lemoore", "lemoore-california", "Lemoore", "California", 36.3008, -119.7829, "America/Los_Angeles", true, Seq("Lemoore", "Hanford"), Seq(
    ("Cinemark Hanford 8", "Cinemark Hanford 8", "cinemark-hanford-8", "California", "Hanford"),
    ("Coyote Entertainment Center", "Coyote Entertainment Center", "coyote-entertainment-center", "California", "Lemoore"),
    ("Lemoore Stadium Cinemas", "Lemoore Stadium Cinemas", "lemoore-stadium-cinemas", "California", "Lemoore")
  ))
  private def p_leominster: R = ("leominster", "leominster-massachusetts", "Leominster", "Massachusetts", 42.5251, -71.7598, "America/New_York", true, Seq("Leominster", "Gardner", "Littleton Common"), Seq(
    ("Entertainment Cinema Leominster", "Entertainment Cinema Leominster", "entertainment-cinema-leominster", "Massachusetts", "Leominster"),
    ("Gardner Cinemas", "Gardner Cinemas", "gardner-cinemas", "Massachusetts", "Gardner"),
    ("O'Neil Cinemas Littleton", "O'Neil Cinemas Littleton", "oneil-cinemas-littleton", "Massachusetts", "Littleton")
  ))
  private def p_lewisburg_west_virginia: R = ("lewisburg-west-virginia", "lewisburg-west-virginia", "Lewisburg", "West Virginia", 37.8018, -80.4456, "America/New_York", false, Seq("Lewisburg"), Seq(
    ("Lewisburg Cinema 8", "Lewisburg Cinema 8", "lewisburg-cinema-8", "West Virginia", "Lewisburg"),
    ("Lewis Theatre Lewisburg", "Lewis Theatre Lewisburg", "lewis-theatre-lewisburg", "West Virginia", "Lewisburg")
  ))
  private def p_lewiston: R = ("lewiston", "lewiston-idaho", "Lewiston", "Idaho", 46.4166, -117.0177, "America/Los_Angeles", false, Seq("Lewiston"), Seq(
    ("Golden Ticket Cinema Nez Perce Plaza", "Golden Ticket Cinema Nez Perce Plaza", "village-centre-at-nez-perce-plaza", "Idaho", "Lewiston")
  ))
  private def p_lewistown: R = ("lewistown", "lewistown-montana", "Lewistown", "Montana", 47.0625, -109.4282, "America/Denver", false, Seq("Lewistown"), Seq(
    ("Judith Cinema Lewistown", "Judith Cinema Lewistown", "judith-cinema-lewistown", "Montana", "Lewistown")
  ))
  private def p_lewistown_pennsylvania: R = ("lewistown-pennsylvania", "lewistown-pennsylvania", "Lewistown", "Pennsylvania", 40.5992, -77.5714, "America/New_York", true, Seq("Lewistown", "Huntingdon", "Mifflintown"), Seq(
    ("Clifton Huntingdon 5", "Clifton Huntingdon 5", "clifton-huntingdon-5", "Pennsylvania", "Huntingdon"),
    ("Miller Cinema Lewistown 6", "Miller Cinema Lewistown 6", "miller-cinema-lewistown-6", "Pennsylvania", "Lewistown"),
    ("Midway Drive-in Mifflintown", "Midway Drive-in Mifflintown", "midway-drive-in-mifflintown", "Pennsylvania", "Mifflintown")
  ))
  private def p_lexington_nebraska: R = ("lexington-nebraska", "lexington-nebraska", "Lexington", "Nebraska", 40.7808, -99.7415, "America/Chicago", true, Seq("Lexington", "Cozad", "Gothenburg"), Seq(
    ("Majestic Theatre Lexington", "Majestic Theatre Lexington", "majestic-theatre-lexington", "Nebraska", "Lexington"),
    ("Fox Theater Cozad", "Fox Theater Cozad", "fox-theater-cozad", "Nebraska", "Cozad"),
    ("Sun Theatre Gothenburg", "Sun Theatre Gothenburg", "sun-theatre-gothenburg", "Nebraska", "Gothenburg")
  ))
  private def p_lexington_south_carolina: R = ("lexington-south-carolina", "lexington-south-carolina", "Lexington", "South Carolina", 33.9815, -81.2362, "America/New_York", false, Seq("Lexington"), Seq(
    ("Regal Northlake Village Lexington", "Regal Northlake Village Lexington", "regal-northlake-village-lexington", "South Carolina", "Lexington")
  ))
  private def p_lexington_tennessee: R = ("lexington-tennessee", "lexington-tennessee", "Lexington", "Tennessee", 35.6509, -88.3934, "America/Chicago", false, Seq("Lexington"), Seq(
    ("Princess Theatre Lexington", "Princess Theatre Lexington", "princess-theatre-lexington", "Tennessee", "Lexington")
  ))
  private def p_lexington_virginia: R = ("lexington-virginia", "lexington-virginia", "Lexington", "Virginia", 37.784, -79.4428, "America/New_York", false, Seq("Lexington"), Seq(
    ("R/C State Cinema 3 Lexington", "R/C State Cinema 3 Lexington", "rc-state-cinema-3-lexington", "Virginia", "Lexington"),
    ("Hulls Drive-in Lexington", "Hulls Drive-in Lexington", "hulls-drive-in-lexington", "Virginia", "Lexington")
  ))
  private def p_libby: R = ("libby", "libby-montana", "Libby", "Montana", 48.3883, -115.556, "America/Denver", false, Seq("Libby"), Seq(
    ("Dome Theater Libby", "Dome Theater Libby", "dome-theater-libby", "Montana", "Libby")
  ))
  private def p_liberal: R = ("liberal", "liberal-kansas", "Liberal", "Kansas", 37.0431, -100.921, "America/Chicago", false, Seq("Liberal"), Seq(
    ("Southgate Cinema 6 Liberal", "Southgate Cinema 6 Liberal", "southgate-cinema-6-liberal", "Kansas", "Liberal")
  ))
  private def p_lima: R = ("lima", "lima-ohio", "Lima", "Ohio", 40.7426, -84.1052, "America/New_York", true, Seq("Lima", "Wapakoneta", "Ada"), Seq(
    ("Regal American Mall Lima", "Regal American Mall Lima", "regal-american-mall-lima", "Ohio", "Lima"),
    ("Wapa Theatre Wapakoneta", "Wapa Theatre Wapakoneta", "wapa-theatre-wapakoneta", "Ohio", "Wapakoneta"),
    ("Ada Theatre", "Ada Theatre", "ada-theatre", "Ohio", "Ada")
  ))
  private def p_limon: R = ("limon", "limon-colorado", "Limon", "Colorado", 39.2639, -103.6922, "America/Denver", false, Seq("Limon"), Seq(
    ("Lincoln Theatre Limon", "Lincoln Theatre Limon", "lincoln-theatre-limon", "Colorado", "Limon")
  ))
  private def p_lincoln_city: R = ("lincoln-city", "lincoln-city-oregon", "Lincoln City", "Oregon", 44.9582, -124.0179, "America/Los_Angeles", false, Seq("Lincoln City"), Seq(
    ("Bijou Lincoln City", "Bijou Lincoln City", "bijou-lincoln-city", "Oregon", "Lincoln City")
  ))
  private def p_lincoln_illinois: R = ("lincoln-illinois", "lincoln-illinois", "Lincoln", "Illinois", 40.1484, -89.3648, "America/Chicago", true, Seq("Lincoln", "Mason City"), Seq(
    ("Arlee Theater Mason City", "Arlee Theater Mason City", "arlee-theater-mason-city", "Illinois", "Mason City"),
    ("VIP Lincoln Grand", "VIP Lincoln Grand", "vip-lincoln-grand", "Illinois", "Lincoln")
  ))
  private def p_lincoln_kansas: R = ("lincoln-kansas", "lincoln-kansas", "Lincoln", "Kansas", 39.0408, -98.1448, "America/Chicago", false, Seq("Lincoln"), Seq(
    ("Finch Theatre Lincoln", "Finch Theatre Lincoln", "finch-theatre-lincoln", "Kansas", "Lincoln")
  ))
  private def p_lincoln_new_hampshire: R = ("lincoln-new-hampshire", "lincoln-new-hampshire", "Lincoln", "New Hampshire", 44.049, -71.6614, "America/New_York", false, Seq("Lincoln"), Seq(
    ("BarnZ's Lincoln Cinema", "BarnZ's Lincoln Cinema", "barnzs-lincoln-cinema", "New Hampshire", "Lincoln")
  ))
  private def p_linn: R = ("linn", "linn-missouri", "Linn", "Missouri", 38.4859, -91.8504, "America/Chicago", false, Seq("Linn"), Seq(
    ("State Tech Osage View", "State Tech Osage View", "state-tech-osage-view", "Missouri", "Linn")
  ))
  private def p_linton: R = ("linton", "linton-indiana", "Linton", "Indiana", 39.0348, -87.1659, "America/Indiana/Indianapolis", false, Seq("Linton"), Seq(
    ("Linton Cinema", "Linton Cinema", "linton-cinema", "Indiana", "Linton")
  ))
  private def p_lisbon: R = ("lisbon", "lisbon-north-dakota", "Lisbon", "North Dakota", 46.4416, -97.6812, "America/Chicago", false, Seq("Lisbon"), Seq(
    ("Scenic Theatre Lisbon", "Scenic Theatre Lisbon", "scenic-theatre-lisbon", "North Dakota", "Lisbon")
  ))
  private def p_litchfield: R = ("litchfield", "litchfield-illinois", "Litchfield", "Illinois", 39.1753, -89.6543, "America/Chicago", true, Seq("Litchfield", "Hillsboro", "Carlinville"), Seq(
    ("Marvel Theatre Carlinville", "Marvel Theatre Carlinville", "marvel-theatre-carlinville", "Illinois", "Carlinville"),
    ("Orpheum & Grand Theatres Hillsboro", "Orpheum & Grand Theatres Hillsboro", "orpheum-grand-theatres-hillsboro", "Illinois", "Hillsboro"),
    ("Westside Litchfield", "Westside Litchfield", "westside-litchfield", "Illinois", "Litchfield"),
    ("Skyview Drive-in Litchfield", "Skyview Drive-in Litchfield", "skyview-drive-in-litchfield", "Illinois", "Litchfield")
  ))
  private def p_litchfield_minnesota: R = ("litchfield-minnesota", "litchfield-minnesota", "Litchfield", "Minnesota", 45.1272, -94.528, "America/Chicago", false, Seq("Litchfield"), Seq(
    ("Hollywood Theatre Litchfield", "Hollywood Theatre Litchfield", "hollywood-theatre-litchfield", "Minnesota", "Litchfield"),
    ("Starlite Drive-in Litchfield", "Starlite Drive-in Litchfield", "starlite-drive-in-litchfield", "Minnesota", "Litchfield")
  ))
  private def p_lititz: R = ("lititz", "lititz-pennsylvania", "Lititz", "Pennsylvania", 40.1573, -76.3069, "America/New_York", true, Seq("Lititz", "Willow Street", "Palmyra", "Annville"), Seq(
    ("Flagship Cinemas Palmyra", "Flagship Cinemas Palmyra", "flagship-cinemas-palmyra", "Pennsylvania", "Palmyra"),
    ("Kendig Square Movies 6", "Kendig Square Movies 6", "kendig-square-movies-6", "Pennsylvania", "Willow Street"),
    ("Penn Cinema Lititz", "Penn Cinema Lititz", "penn-cinema-lititz", "Pennsylvania", "Lititz"),
    ("Allen Theatre Annville", "Allen Theatre Annville", "allen-theatre-annville", "Pennsylvania", "Annville")
  ))
  private def p_little_falls: R = ("little-falls", "little-falls-minnesota", "Little Falls", "Minnesota", 45.9763, -94.3625, "America/Chicago", false, Seq("Little Falls"), Seq(
    ("Falls Theatre", "Falls Theatre", "falls-theatre", "Minnesota", "Little Falls")
  ))
  private def p_littleton_colorado: R = ("littleton-colorado", "littleton-colorado", "Littleton", "Colorado", 39.6133, -105.0166, "America/Denver", true, Seq("Littleton", "Highlands Ranch", "Greenwood Village", "Sheridan"), Seq(
    ("Alamo Drafthouse Littleton", "Alamo Drafthouse Littleton", "alamo-drafthouse-littleton", "Colorado", "Littleton"),
    ("AMC Bowles Crossing 12", "AMC Bowles Crossing 12", "amc-bowles-crossing-12", "Colorado", "Littleton"),
    ("AMC Highlands Ranch 24", "AMC Highlands Ranch 24", "amc-highlands-ranch-24", "Colorado", "Highlands Ranch"),
    ("Landmark Greenwood Village", "Landmark Greenwood Village", "landmark-greenwood-village", "Colorado", "Greenwood Village"),
    ("Regal River Point Sheridan", "Regal River Point Sheridan", "regal-river-point-sheridan", "Colorado", "Sheridan")
  ))
  private def p_livermore: R = ("livermore", "livermore-california", "Livermore", "California", 37.6819, -121.768, "America/Los_Angeles", true, Seq("Livermore", "Dublin"), Seq(
    ("Livermore Cinemas", "Livermore Cinemas", "livermore-cinemas", "California", "Livermore"),
    ("Vine Cinema Livermore", "Vine Cinema Livermore", "vine-cinema-livermore", "California", "Livermore"),
    ("Regal Hacienda Crossings Dublin", "Regal Hacienda Crossings Dublin", "regal-hacienda-crossings-dublin", "California", "Dublin")
  ))
  private def p_livingston: R = ("livingston", "livingston-montana", "Livingston", "Montana", 45.6624, -110.561, "America/Denver", false, Seq("Livingston"), Seq(
    ("Empire Twin Theatres Livingston", "Empire Twin Theatres Livingston", "empire-twin-theatres-livingston", "Montana", "Livingston")
  ))
  private def p_livingston_alabama: R = ("livingston-alabama", "livingston-alabama", "Livingston", "Alabama", 32.5843, -88.1873, "America/Chicago", false, Seq("Livingston"), Seq(
    ("University Cinema – University of West Alabama", "University Cinema – University of West Alabama", "university-cinema---university-of-west-alabama", "Alabama", "Livingston")
  ))
  private def p_llano: R = ("llano", "llano-texas", "Llano", "Texas", 30.7594, -98.675, "America/Chicago", false, Seq("Llano"), Seq(
    ("Lantex Llano", "Lantex Llano", "lantex-llano", "Texas", "Llano")
  ))
  private def p_lockport: R = ("lockport", "lockport-new-york", "Lockport", "New York", 43.1706, -78.6903, "America/New_York", false, Seq("Lockport"), Seq(
    ("Transit Drive-in Lockport", "Transit Drive-in Lockport", "transit-drive-in-lockport", "New York", "Lockport"),
    ("Palace Theatre Lockport", "Palace Theatre Lockport", "palace-theatre-lockport", "New York", "Lockport")
  ))
  private def p_lodi: R = ("lodi", "lodi-california", "Lodi", "California", 38.1302, -121.2725, "America/Los_Angeles", false, Seq("Lodi"), Seq(
    ("Lodi Stadium 12 Cinemas", "Lodi Stadium 12 Cinemas", "lodi-stadium-12-cinemas", "California", "Lodi")
  ))
  private def p_logan: R = ("logan", "logan-utah", "Logan", "Utah", 41.7355, -111.8344, "America/Denver", true, Seq("Logan", "Providence"), Seq(
    ("Megaplex at University Logan", "Megaplex at University Logan", "megaplex-at-university-logan", "Utah", "Logan"),
    ("Utah Theatre Logan", "Utah Theatre Logan", "utah-theatre-logan", "Utah", "Logan"),
    ("Megaplex Providence", "Megaplex Providence", "megaplex-providence", "Utah", "Providence")
  ))
  private def p_logan_west_virginia: R = ("logan-west-virginia", "logan-west-virginia", "Logan", "West Virginia", 37.8487, -81.9935, "America/New_York", false, Seq("Logan"), Seq(
    ("Fountain Place Cinema 8", "Fountain Place Cinema 8", "fountain-place-cinema-8", "West Virginia", "Logan")
  ))
  private def p_logansport: R = ("logansport", "logansport-indiana", "Logansport", "Indiana", 40.7545, -86.3567, "America/Indiana/Indianapolis", true, Seq("Logansport", "Peru"), Seq(
    ("Mary Max Cinemas Logansport 5+", "Mary Max Cinemas Logansport 5+", "mary-max-cinemas-logansport-5", "Indiana", "Logansport"),
    ("Roxy Theater Peru", "Roxy Theater Peru", "roxy-theater-peru", "Indiana", "Peru")
  ))
  private def p_lompoc: R = ("lompoc", "lompoc-california", "Lompoc", "California", 34.6392, -120.4579, "America/Los_Angeles", false, Seq("Lompoc"), Seq(
    ("Movies Lompoc", "Movies Lompoc", "movies-lompoc", "California", "Lompoc")
  ))
  private def p_london: R = ("london", "london-kentucky", "London", "Kentucky", 37.129, -84.0833, "America/New_York", true, Seq("London", "Corbin", "Barbourville"), Seq(
    ("Regency Cinema London 8", "Regency Cinema London 8", "regency-cinema-london-8", "Kentucky", "London"),
    ("Tri-county Cineplex Corbin", "Tri-county Cineplex Corbin", "tri-county-cineplex-corbin", "Kentucky", "Corbin"),
    ("Knox Drive-In", "Knox Drive-In", "knox-drive-in", "Kentucky", "Barbourville")
  ))
  private def p_long_beach_washington: R = ("long-beach-washington", "long-beach-washington", "Long Beach", "Washington", 46.3523, -124.0543, "America/Los_Angeles", false, Seq("Long Beach"), Seq(
    ("Neptune Drive-In", "Neptune Drive-In", "neptune-drive-in-long-beach", "Washington", "Long Beach"),
    ("Neptune Movie House", "Neptune Movie House", "neptune-twin-long-beach-2", "Washington", "Long Beach")
  ))
  private def p_longmont: R = ("longmont", "longmont-colorado", "Longmont", "Colorado", 40.1672, -105.1019, "America/Denver", true, Seq("Longmont", "Broomfield"), Seq(
    ("Regal Village at The Peaks Longmont", "Regal Village at The Peaks Longmont", "regal-village-at-the-peaks-longmont", "Colorado", "Longmont"),
    ("AMC Flatiron Crossing 14", "AMC Flatiron Crossing 14", "amc-flatiron-crossing-14", "Colorado", "Broomfield")
  ))
  private def p_longview: R = ("longview", "longview-texas", "Longview", "Texas", 32.5007, -94.7405, "America/Chicago", true, Seq("Longview", "Marshall", "Kilgore"), Seq(
    ("4 Star Cinema Kilgore", "4 Star Cinema Kilgore", "4-star-cinema-kilgore", "Texas", "Kilgore"),
    ("AMC Longview 10", "AMC Longview 10", "amc-longview-10", "Texas", "Longview"),
    ("Film Alley Longview", "Film Alley Longview", "film-alley-longview", "Texas", "Longview"),
    ("Marshall 6 Texas", "Marshall 6 Texas", "marshall-6-texas", "Texas", "Marshall")
  ))
  private def p_lorain: R = ("lorain", "lorain-ohio", "Lorain", "Ohio", 41.4528, -82.1824, "America/New_York", true, Seq("Lorain", "Elyria", "North Ridgeville", "Westlake", "North Olmsted", "Rocky River", "Amherst", "Oberlin"), Seq(
    ("AMC Westwood Town Center 6", "AMC Westwood Town Center 6", "amc-westwood-town-center-6", "Ohio", "Rocky River"),
    ("Amherst Cinema (Amherst) (Ohio)", "Amherst Cinema (Amherst) (Ohio)", "amherst-cinema-oh", "Ohio", "Amherst"),
    ("Apollo Theatre Oberlin", "Apollo Theatre Oberlin", "apollo-theatre-oberlin", "Ohio", "Oberlin"),
    ("Aut-O-Rama Drive-In", "Aut-O-Rama Drive-In", "aut-o-rama-drive-in", "Ohio", "North Ridgeville"),
    ("Phoenix Great Northern Mall", "Phoenix Great Northern Mall", "phoenix-great-northern-mall", "Ohio", "North Olmsted"),
    ("Regal Cobblestone Square Elyria", "Regal Cobblestone Square Elyria", "regal-cobblestone-square-elyria", "Ohio", "Elyria"),
    ("Regal Crocker Park", "Regal Crocker Park", "regal-crocker-park", "Ohio", "Westlake"),
    ("Lorain Palace Theater", "Lorain Palace Theater", "lorain-palace-theater", "Ohio", "Lorain")
  ))
  private def p_los_alamos: R = ("los-alamos", "los-alamos-new-mexico", "Los Alamos", "New Mexico", 35.8881, -106.307, "America/Denver", true, Seq("Los Alamos", "Española"), Seq(
    ("Dreamcatcher 10 Española", "Dreamcatcher 10 Española", "dreamcatcher-10-espaola", "New Mexico", "Española"),
    ("SALA Los Alamos Event Center", "SALA Los Alamos Event Center", "reel-deal-los-alamos", "New Mexico", "Cañada de los Alamos")
  ))
  private def p_los_banos: R = ("los-banos", "los-banos-california", "Los Banos", "California", 37.0583, -120.8499, "America/Los_Angeles", false, Seq("Los Banos"), Seq(
    ("Premiere Cinemas Los Banos", "Premiere Cinemas Los Banos", "premiere-cinemas-los-banos", "California", "Los Banos")
  ))
  private def p_los_lunas: R = ("los-lunas", "los-lunas-new-mexico", "Los Lunas", "New Mexico", 34.8062, -106.7334, "America/Denver", false, Seq("Los Lunas"), Seq(
    ("Starlight Los Lunas 8", "Starlight Los Lunas 8", "starlight-los-lunas-8", "New Mexico", "Los Lunas")
  ))
  private def p_loveland: R = ("loveland", "loveland-colorado", "Loveland", "Colorado", 40.3978, -105.075, "America/Denver", false, Seq("Loveland"), Seq(
    ("MetroLux 12 @ Centerra", "MetroLux 12 @ Centerra", "metrolux-12-centerra", "Colorado", "Loveland"),
    ("MetroLux Dine-in @ The Foundry", "MetroLux Dine-in @ The Foundry", "metrolux-dine-in-the-foundry", "Colorado", "Loveland"),
    ("Rialto Theatre Loveland", "Rialto Theatre Loveland", "rialto-theatre-loveland", "Colorado", "Loveland")
  ))
  private def p_lovell: R = ("lovell", "lovell-wyoming", "Lovell", "Wyoming", 44.8374, -108.3896, "America/Denver", false, Seq("Lovell"), Seq(
    ("Hyart Theatre Lovell", "Hyart Theatre Lovell", "hyart-theatre-lovell", "Wyoming", "Lovell")
  ))
  private def p_lovington: R = ("lovington", "lovington-new-mexico", "Lovington", "New Mexico", 32.944, -103.3486, "America/Denver", false, Seq("Lovington"), Seq(
    ("Lea Theatre", "Lea Theatre", "lea-theatre", "New Mexico", "Lovington")
  ))
  private def p_lucas: R = ("lucas", "lucas-kansas", "Lucas", "Kansas", 39.0588, -98.5376, "America/Chicago", false, Seq("Lucas"), Seq(
    ("Lucas Area Community Theater", "Lucas Area Community Theater", "lucas-area-community-theater", "Kansas", "Lucas")
  ))
  private def p_ludington: R = ("ludington", "ludington-michigan", "Ludington", "Michigan", 43.9553, -86.4526, "America/Detroit", false, Seq("Ludington"), Seq(
    ("AMC CLASSIC Ludington 8", "AMC CLASSIC Ludington 8", "amc-classic-ludington-8", "Michigan", "Ludington")
  ))
  private def p_lufkin: R = ("lufkin", "lufkin-texas", "Lufkin", "Texas", 31.3382, -94.7291, "America/Chicago", false, Seq("Lufkin"), Seq(
    ("AMC Lufkin 9", "AMC Lufkin 9", "amc-lufkin-9", "Texas", "Lufkin"),
    ("Cinemark Lufkin 12", "Cinemark Lufkin 12", "cinemark-lufkin-12", "Texas", "Lufkin"),
    ("Pines Theater Lufkin", "Pines Theater Lufkin", "pines-theater-lufkin", "Texas", "Lufkin")
  ))
  private def p_lumberton: R = ("lumberton", "lumberton-north-carolina", "Lumberton", "North Carolina", 34.6183, -79.0105, "America/New_York", false, Seq("Lumberton"), Seq(
    ("Hometown Cinemas Lumberton", "Hometown Cinemas Lumberton", "hometown-cinemas-lumberton", "North Carolina", "Lumberton")
  ))
  private def p_luverne: R = ("luverne", "luverne-minnesota", "Luverne", "Minnesota", 43.6541, -96.2128, "America/Chicago", false, Seq("Luverne"), Seq(
    ("Palace Theatre Luverne", "Palace Theatre Luverne", "palace-theatre-luverne", "Minnesota", "Luverne"),
    ("Verne Drive-in Luverne", "Verne Drive-in Luverne", "verne-drive-in-luverne", "Minnesota", "Luverne")
  ))
  private def p_lyman: R = ("lyman", "lyman-wyoming", "Lyman", "Wyoming", 41.3274, -110.2929, "America/Denver", false, Seq("Lyman"), Seq(
    ("Valley Theatre Lyman", "Valley Theatre Lyman", "valley-theatre-lyman", "Wyoming", "Lyman")
  ))
  private def p_lynchburg: R = ("lynchburg", "lynchburg-virginia", "Lynchburg", "Virginia", 37.4138, -79.1423, "America/New_York", false, Seq("Lynchburg"), Seq(
    ("Regal River Ridge Lynchburg", "Regal River Ridge Lynchburg", "regal-river-ridge-lynchburg", "Virginia", "Lynchburg")
  ))
  private def p_macomb: R = ("macomb", "macomb-illinois", "Macomb", "Illinois", 40.4592, -90.6718, "America/Chicago", false, Seq("Macomb"), Seq(
    ("VIP Rialto Cinemas Macomb", "VIP Rialto Cinemas Macomb", "vip-rialto-cinemas-macomb", "Illinois", "Macomb")
  ))
  private def p_macon: R = ("macon", "macon-georgia", "Macon", "Georgia", 32.8407, -83.6324, "America/New_York", false, Seq("Macon"), Seq(
    ("Santikos Amstar Macon", "Santikos Amstar Macon", "santikos-amstar-macon", "Georgia", "Macon"),
    ("The Douglass Theatre Macon", "The Douglass Theatre Macon", "the-douglass-theatre-macon", "Georgia", "Macon")
  ))
  private def p_macon_missouri: R = ("macon-missouri", "macon-missouri", "Macon", "Missouri", 39.7423, -92.4727, "America/Chicago", false, Seq("Macon"), Seq(
    ("Cinema Macon", "Cinema Macon", "cinema-macon", "Missouri", "Macon")
  ))
  private def p_madawaska: R = ("madawaska", "madawaska-maine", "Madawaska", "Maine", 47.3553, -68.3217, "America/New_York", true, Seq("Madawaska", "Fort Kent"), Seq(
    ("Century Theatre Ft Kent", "Century Theatre Ft Kent", "century-theatre-ft-kent", "Maine", "Fort Kent"),
    ("Skylite Drive-in Madawaska", "Skylite Drive-in Madawaska", "skylite-drive-in-madawaska", "Maine", "Madawaska")
  ))
  private def p_madera: R = ("madera", "madera-california", "Madera", "California", 36.9613, -120.0607, "America/Los_Angeles", false, Seq("Madera"), Seq(
    ("Madera 2 Drive-in", "Madera 2 Drive-in", "madera-drive-in", "California", "Madera"),
    ("Movies Madera", "Movies Madera", "movies-madera", "California", "Madera")
  ))
  private def p_madison_connecticut: R = ("madison-connecticut", "madison-connecticut", "Madison", "Connecticut", 41.2795, -72.5984, "America/New_York", true, Seq("Madison", "Greenport"), Seq(
    ("North Fork Arts Center", "North Fork Arts Center", "north-fork-arts-center", "New York", "Greenport"),
    ("Madison Cinemas", "Madison Cinemas", "madison-cinemas", "Connecticut", "Madison")
  ))
  private def p_madison_indiana: R = ("madison-indiana", "madison-indiana", "Madison", "Indiana", 38.7359, -85.38, "America/Indiana/Indianapolis", false, Seq("Madison"), Seq(
    ("Madison 6 Theater", "Madison 6 Theater", "madison-6-theater", "Indiana", "Madison"),
    ("Ohio Theatre Madison", "Ohio Theatre Madison", "ohio-theatre-madison", "Indiana", "Madison")
  ))
  private def p_madison_south_dakota: R = ("madison-south-dakota", "madison-south-dakota", "Madison", "South Dakota", 44.0061, -97.114, "America/Chicago", false, Seq("Madison"), Seq(
    ("Dakota Cinema", "Dakota Cinema", "dakota-cinema", "South Dakota", "Madison")
  ))
  private def p_madisonville: R = ("madisonville", "madisonville-kentucky", "Madisonville", "Kentucky", 37.3281, -87.4989, "America/Chicago", false, Seq("Madisonville"), Seq(
    ("Golden Ticket Madisonville Capitol 8", "Golden Ticket Madisonville Capitol 8", "golden-ticket-madisonville-capitol-8", "Kentucky", "Madisonville")
  ))
  private def p_madras: R = ("madras", "madras-oregon", "Madras", "Oregon", 44.6335, -121.1295, "America/Los_Angeles", false, Seq("Madras"), Seq(
    ("Madras Cinema 5", "Madras Cinema 5", "madras-cinema-5", "Oregon", "Madras")
  ))
  private def p_malta: R = ("malta", "malta-montana", "Malta", "Montana", 48.3597, -107.8743, "America/Denver", false, Seq("Malta"), Seq(
    ("Villa Theatre Malta", "Villa Theatre Malta", "villa-theatre-malta", "Montana", "East Malta Colony")
  ))
  private def p_mammoth_lakes: R = ("mammoth-lakes", "mammoth-lakes-california", "Mammoth Lakes", "California", 37.6486, -118.9721, "America/Los_Angeles", false, Seq("Mammoth Lakes"), Seq(
    ("Minaret Cinema is D'Place", "Minaret Cinema is D'Place", "minaret-cinema-is-dplace", "California", "Mammoth Lakes")
  ))
  private def p_manchester_connecticut: R = ("manchester-connecticut", "manchester-connecticut", "Manchester", "Connecticut", 41.7759, -72.5215, "America/New_York", true, Seq("Manchester", "West Hartford"), Seq(
    ("Cinemark Buckland Hills 18", "Cinemark Buckland Hills 18", "cinemark-buckland-hills-18", "Connecticut", "Manchester"),
    ("Parkade Cinemas Manchester", "Parkade Cinemas Manchester", "parkade-cinemas-manchester", "Connecticut", "Manchester"),
    ("Cinepolis West Hartford", "Cinepolis West Hartford", "cinepolis-west-hartford", "Connecticut", "West Hartford")
  ))
  private def p_manchester_iowa: R = ("manchester-iowa", "manchester-iowa", "Manchester", "Iowa", 42.4841, -91.4554, "America/Chicago", false, Seq("Manchester"), Seq(
    ("Castle Theatre Iowa", "Castle Theatre Iowa", "castle-theatre-iowa", "Iowa", "Manchester")
  ))
  private def p_manhattan: R = ("manhattan", "manhattan-kansas", "Manhattan", "Kansas", 39.1836, -96.5717, "America/Chicago", true, Seq("Manhattan", "Junction City"), Seq(
    ("AMC Manhattan 13", "AMC Manhattan 13", "amc-dine-in-manhattan-town-center-13", "Kansas", "Manhattan"),
    ("B&B Junction City Gem", "B&B Junction City Gem", "bb-junction-city-gem", "Kansas", "Junction City")
  ))
  private def p_manistee: R = ("manistee", "manistee-michigan", "Manistee", "Michigan", 44.2445, -86.3243, "America/Detroit", false, Seq("Manistee"), Seq(
    ("Vogue Theatre Manistee", "Vogue Theatre Manistee", "vogue-theatre-manistee", "Michigan", "Manistee")
  ))
  private def p_manistique: R = ("manistique", "manistique-michigan", "Manistique", "Michigan", 45.9577, -86.2463, "America/Detroit", false, Seq("Manistique"), Seq(
    ("Cedar Street Cinemas Manistique", "Cedar Street Cinemas Manistique", "cedar-street-cinemas-manistique", "Michigan", "Manistique"),
    ("Highway 2 Community Drive-In Theater", "Highway 2 Community Drive-In Theater", "highway-2-community-drive-in-theater", "Michigan", "Manistique")
  ))
  private def p_manitowoc: R = ("manitowoc", "manitowoc-wisconsin", "Manitowoc", "Wisconsin", 44.0886, -87.6576, "America/Chicago", false, Seq("Manitowoc"), Seq(
    ("AMC CLASSIC Manitowoc 10", "AMC CLASSIC Manitowoc 10", "amc-classic-manitowoc-10", "Wisconsin", "Manitowoc"),
    ("Mikadow Theatre", "Mikadow Theatre", "mikadow-theatre", "Wisconsin", "Manitowoc")
  ))
  private def p_mankato: R = ("mankato", "mankato-minnesota", "Mankato", "Minnesota", 44.1591, -94.0092, "America/Chicago", false, Seq("Mankato"), Seq(
    ("Cinemark Mankato 8", "Cinemark Mankato 8", "cinemark-mankato-8", "Minnesota", "Mankato"),
    ("Spotlight Theatres Mankato", "Spotlight Theatres Mankato", "spotlight-theatres-mankato", "Minnesota", "Mankato")
  ))
  private def p_mansfield: R = ("mansfield", "mansfield-texas", "Mansfield", "Texas", 32.5632, -97.1417, "America/Chicago", true, Seq("Mansfield", "Burleson", "Dalworthington Gardens", "Cedar Hill", "Cleburne"), Seq(
    ("AMC Burleson 14", "AMC Burleson 14", "amc-burleson-14", "Texas", "Burleson"),
    ("Burleson Premiere Cinema 14", "Burleson Premiere Cinema 14", "burleson-premiere-cinema-14", "Texas", "Burleson"),
    ("AMC The Parks at Arlington 18", "AMC The Parks at Arlington 18", "amc-the-parks-at-arlington-18", "Texas", "Arlington Heights"),
    ("Studio Movie Grill Arlington", "Studio Movie Grill Arlington", "studio-movie-grill-arlington", "Texas", "Arlington Heights"),
    ("Cinemark Cedar Hill 14", "Cinemark Cedar Hill 14", "cinemark-cedar-hill-14", "Texas", "Cedar Hill"),
    ("Cinemark Cleburne Cinema 6", "Cinemark Cleburne Cinema 6", "cinemark-cleburne-cinema-6", "Texas", "Cleburne"),
    ("Cinemark Mansfield 12", "Cinemark Mansfield 12", "cinemark-mansfield-12", "Texas", "Mansfield"),
    ("Flix Brewhouse Mansfield", "Flix Brewhouse Mansfield", "flix-brewhouse-mansfield", "Texas", "Mansfield")
  ))
  private def p_mansfield_city: R = ("mansfield-city", "mansfield-city-connecticut", "Mansfield City", "Connecticut", 41.7659, -72.2337, "America/New_York", true, Seq("Mansfield City", "Willimantic", "Colchester"), Seq(
    ("Mansfield Movieplex", "Mansfield Movieplex", "mansfield-movieplex", "Connecticut", "Mansfield Center"),
    ("Gallery Cinema Colchester", "Gallery Cinema Colchester", "gallery-cinema-colchester", "Connecticut", "Colchester"),
    ("Mansfield Drive-in Theatre", "Mansfield Drive-in Theatre", "mansfield-drive-in-theatre", "Connecticut", "Mansfield Center")
  ))
  private def p_maquoketa: R = ("maquoketa", "maquoketa-iowa", "Maquoketa", "Iowa", 42.0689, -90.6657, "America/Chicago", true, Seq("Maquoketa", "Delmar"), Seq(
    ("61 Drive In Theatre", "61 Drive In Theatre", "61-drive-in-theatre", "Iowa", "Delmar"),
    ("Voy Maquoketa", "Voy Maquoketa", "voy-maquoketa", "Iowa", "Maquoketa")
  ))
  private def p_marathon: R = ("marathon", "marathon-florida", "Marathon", "Florida", 24.7138, -81.0904, "America/New_York", false, Seq("Marathon"), Seq(
    ("Marathon Community Cinema", "Marathon Community Cinema", "marathon-community-cinema", "Florida", "Marathon")
  ))
  private def p_marble_falls: R = ("marble-falls", "marble-falls-texas", "Marble Falls", "Texas", 30.5784, -98.2751, "America/Chicago", false, Seq("Marble Falls"), Seq(
    ("Cinergy Marble Falls 8", "Cinergy Marble Falls 8", "cinergy-marble-falls-8", "Texas", "Marble Falls")
  ))
  private def p_marbleton: R = ("marbleton", "marbleton-wyoming", "Marbleton", "Wyoming", 42.5536, -110.1093, "America/Denver", false, Seq("Marbleton"), Seq(
    ("Flicks N Pins Marbleton", "Flicks N Pins Marbleton", "flicks-n-pins-marbleton", "Wyoming", "Marbleton")
  ))
  private def p_marco_island: R = ("marco-island", "marco-island-florida", "Marco Island", "Florida", 25.9412, -81.7184, "America/New_York", false, Seq("Marco Island"), Seq(
    ("Marco Movies", "Marco Movies", "marco-movies", "Florida", "Marco Island")
  ))
  private def p_marianna: R = ("marianna", "marianna-florida", "Marianna", "Florida", 30.7744, -85.2269, "America/Chicago", false, Seq("Marianna"), Seq(
    ("Marianna Cinemas", "Marianna Cinemas", "marianna-cinemas", "Florida", "Marianna")
  ))
  private def p_maricopa: R = ("maricopa", "maricopa-arizona", "Maricopa", "Arizona", 33.0581, -112.0476, "America/Phoenix", false, Seq("Maricopa"), Seq(
    ("AK-Chin Circle Entertainment Center", "AK-Chin Circle Entertainment Center", "ak-chin-circle-entertainment-center", "Arizona", "Maricopa")
  ))
  private def p_marietta: R = ("marietta", "marietta-georgia", "Marietta", "Georgia", 33.9526, -84.5499, "America/New_York", false, Seq("Marietta"), Seq(
    ("GTC Merchants Walk Marietta", "GTC Merchants Walk Marietta", "gtc-merchants-walk-marietta", "Georgia", "Marietta"),
    ("NCG Marietta", "NCG Marietta", "ncg-marietta", "Georgia", "Marietta"),
    ("Picture Show at Merchant's Exchange", "Picture Show at Merchant's Exchange", "picture-show-at-merchants-exchange", "Georgia", "Marietta"),
    ("Studio Movie Grill Marietta", "Studio Movie Grill Marietta", "studio-movie-grill-marietta", "Georgia", "Marietta"),
    ("Earl and Rachel Smith Strand Theatre", "Earl and Rachel Smith Strand Theatre", "earl-and-rachel-smith-strand-theatre", "Georgia", "Marietta")
  ))
  private def p_marinette: R = ("marinette", "marinette-wisconsin", "Marinette", "Wisconsin", 45.1, -87.6307, "America/Chicago", false, Seq("Marinette"), Seq(
    ("Mariner Marinette 2", "Mariner Marinette 2", "mariner-marinette-2", "Wisconsin", "Marinette"),
    ("Phoenix Marinette Theatre", "Phoenix Marinette Theatre", "phoenix-marinette-theatre", "Wisconsin", "Marinette")
  ))
  private def p_marion: R = ("marion", "marion-ohio", "Marion", "Ohio", 40.5887, -83.1285, "America/New_York", false, Seq("Marion"), Seq(
    ("Centre Cinemas Marion", "Centre Cinemas Marion", "centre-cinemas-marion", "Ohio", "Marion"),
    ("Marion Palace Theatre", "Marion Palace Theatre", "marion-palace-theatre", "Ohio", "Marion")
  ))
  private def p_marion_illinois: R = ("marion-illinois", "marion-illinois", "Marion", "Illinois", 37.7306, -88.9331, "America/Chicago", false, Seq("Marion"), Seq(
    ("AMC CLASSIC Illinois Centre 8", "AMC CLASSIC Illinois Centre 8", "amc-classic-illinois-centre-8", "Illinois", "Marion")
  ))
  private def p_marion_iowa: R = ("marion-iowa", "marion-iowa", "Marion", "Iowa", 42.0342, -91.5977, "America/Chicago", true, Seq("Marion", "Vinton", "Mount Vernon"), Seq(
    ("Bijou Movie Theater", "Bijou Movie Theater", "bijou-movie-theater", "Iowa", "Mount Vernon"),
    ("Collins Road Theatres", "Collins Road Theatres", "collins-road-theatres", "Iowa", "Marion"),
    ("Palace Theatre Vinton", "Palace Theatre Vinton", "palace-theatre-vinton", "Iowa", "Vinton")
  ))
  private def p_marion_north_carolina: R = ("marion-north-carolina", "marion-north-carolina", "Marion", "North Carolina", 35.684, -82.0093, "America/New_York", false, Seq("Marion"), Seq(
    ("Hometown Cinemas Marion", "Hometown Cinemas Marion", "hometown-cinemas-marion", "North Carolina", "Marion")
  ))
  private def p_marion_virginia: R = ("marion-virginia", "marion-virginia", "Marion", "Virginia", 36.8348, -81.5148, "America/New_York", false, Seq("Marion"), Seq(
    ("Park Place Drive-in Marion", "Park Place Drive-in Marion", "park-place-drive-in-marion", "Virginia", "Marion")
  ))
  private def p_marksville: R = ("marksville", "marksville-louisiana", "Marksville", "Louisiana", 31.128, -92.0662, "America/Chicago", false, Seq("Marksville"), Seq(
    ("Paragon Theaters Marksville", "Paragon Theaters Marksville", "paragon-theaters-marksville", "Louisiana", "Marksville")
  ))
  private def p_marquette: R = ("marquette", "marquette-michigan", "Marquette", "Michigan", 46.5435, -87.3954, "America/Detroit", false, Seq("Marquette"), Seq(
    ("Marquette Cinemas", "Marquette Cinemas", "marquette-cinemas", "Michigan", "Marquette")
  ))
  private def p_marshall: R = ("marshall", "marshall-minnesota", "Marshall", "Minnesota", 44.4469, -95.7883, "America/Chicago", false, Seq("Marshall"), Seq(
    ("Marshall 6 Theatre", "Marshall 6 Theatre", "marshall-6-theatre", "Minnesota", "Marshall")
  ))
  private def p_marshall_arkansas: R = ("marshall-arkansas", "marshall-arkansas", "Marshall", "Arkansas", 35.909, -92.6313, "America/Chicago", false, Seq("Marshall"), Seq(
    ("Kenda Drive-In", "Kenda Drive-In", "kenda-drive-in", "Arkansas", "Marshall")
  ))
  private def p_marshall_missouri: R = ("marshall-missouri", "marshall-missouri", "Marshall", "Missouri", 39.1231, -93.1969, "America/Chicago", false, Seq("Marshall"), Seq(
    ("Cinema Marshall", "Cinema Marshall", "cinema-marshall", "Missouri", "Marshall")
  ))
  private def p_marshalltown: R = ("marshalltown", "marshalltown-iowa", "Marshalltown", "Iowa", 42.0494, -92.908, "America/Chicago", true, Seq("Marshalltown", "Grundy Center", "Toledo", "Traer", "Gladbrook"), Seq(
    ("Plaza Marshalltown", "Plaza Marshalltown", "plaza-marshalltown", "Iowa", "Marshalltown"),
    ("Wieting Toledo", "Wieting Toledo", "wieting-toledo", "Iowa", "Toledo"),
    ("Center Theatre Grundy", "Center Theatre Grundy", "center-theatre-grundy", "Iowa", "Grundy Center"),
    ("Gladbrook Theater", "Gladbrook Theater", "gladbrook-theater", "Iowa", "Gladbrook"),
    ("Traer Theatre", "Traer Theatre", "traer-theatre", "Iowa", "Traer")
  ))
  private def p_marshfield: R = ("marshfield", "marshfield-wisconsin", "Marshfield", "Wisconsin", 44.6688, -90.1718, "America/Chicago", false, Seq("Marshfield"), Seq(
    ("Rogers Cinema Marshfield", "Rogers Cinema Marshfield", "rogers-cinema-marshfield", "Wisconsin", "Marshfield")
  ))
  private def p_martin: R = ("martin", "martin-tennessee", "Martin", "Tennessee", 36.3434, -88.8503, "America/Chicago", false, Seq("Martin"), Seq(
    ("Cine Martin 6", "Cine Martin 6", "cine-martin-6", "Tennessee", "Martin")
  ))
  private def p_martinsburg: R = ("martinsburg", "martinsburg-west-virginia", "Martinsburg", "West Virginia", 39.4562, -77.9639, "America/New_York", true, Seq("Martinsburg", "Berkeley Springs"), Seq(
    ("Berkeley Plaza Theater 7", "Berkeley Plaza Theater 7", "berkeley-plaza-theater-7", "West Virginia", "Martinsburg"),
    ("Historic Star Theatre", "Historic Star Theatre", "historic-star-theatre", "West Virginia", "Berkeley Springs")
  ))
  private def p_martinsville: R = ("martinsville", "martinsville-virginia", "Martinsville", "Virginia", 36.6915, -79.8725, "America/New_York", false, Seq("Martinsville"), Seq(
    ("Hollywood Cinemas Martinsville", "Hollywood Cinemas Martinsville", "hollywood-cinemas-martinsville", "Virginia", "Martinsville"),
    ("Movie Town Martinsville 5", "Movie Town Martinsville 5", "movie-town-martinsville-5", "Virginia", "Martinsville")
  ))
  private def p_martinsville_indiana: R = ("martinsville-indiana", "martinsville-indiana", "Martinsville", "Indiana", 39.4278, -86.4283, "America/Indiana/Indianapolis", false, Seq("Martinsville"), Seq(
    ("Centerbrook Drive-In Martinsville", "Centerbrook Drive-In Martinsville", "centerbrook-drive-in-martinsville", "Indiana", "Martinsville"),
    ("Martinsville Entertainment Theatre", "Martinsville Entertainment Theatre", "martinsville-entertainment-theatre", "Indiana", "Martinsville")
  ))
  private def p_marysville: R = ("marysville", "marysville-washington", "Marysville", "Washington", 48.0518, -122.1771, "America/Los_Angeles", true, Seq("Marysville", "Monroe", "Stanwood", "Langley"), Seq(
    ("Clyde Theatre Langley", "Clyde Theatre Langley", "clyde-theatre-langley", "Washington", "Langley"),
    ("Galaxy Theatres Monroe", "Galaxy Theatres Monroe", "galaxy-theatres-monroe", "Washington", "Monroe"),
    ("Regal Marysville", "Regal Marysville", "regal-marysville", "Washington", "Marysville"),
    ("Stanwood Cinemas", "Stanwood Cinemas", "stanwood-cinemas", "Washington", "North Stanwood")
  ))
  private def p_marysville_kansas: R = ("marysville-kansas", "marysville-kansas", "Marysville", "Kansas", 39.8411, -96.6472, "America/Chicago", false, Seq("Marysville"), Seq(
    ("Astro 3 Theatre Marysville", "Astro 3 Theatre Marysville", "astro-3-theatre-marysville", "Kansas", "Marysville")
  ))
  private def p_maryville: R = ("maryville", "maryville-tennessee", "Maryville", "Tennessee", 35.7565, -83.9705, "America/New_York", false, Seq("Maryville"), Seq(
    ("AMC CLASSIC Foothills 12", "AMC CLASSIC Foothills 12", "amc-classic-foothills-12", "Tennessee", "Maryville"),
    ("Parkway Drive In Maryville", "Parkway Drive In Maryville", "parkway-drive-in-maryville", "Tennessee", "Maryville")
  ))
  private def p_maryville_missouri: R = ("maryville-missouri", "maryville-missouri", "Maryville", "Missouri", 40.3461, -94.8725, "America/Chicago", false, Seq("Maryville"), Seq(
    ("Hangar Cinema Maryville", "Hangar Cinema Maryville", "hangar-cinema-maryville", "Missouri", "Maryville")
  ))
  private def p_mashpee: R = ("mashpee", "mashpee-massachusetts", "Mashpee", "Massachusetts", 41.6484, -70.4811, "America/New_York", true, Seq("Mashpee", "Sandwich"), Seq(
    ("Heritage Theaters Sandwich", "Heritage Theaters Sandwich", "heritage-theaters-sandwich", "Massachusetts", "Sandwich"),
    ("Regal Mashpee Commons", "Regal Mashpee Commons", "regal-mashpee-commons", "Massachusetts", "Mashpee")
  ))
  private def p_mason: R = ("mason", "mason-texas", "Mason", "Texas", 30.7488, -99.2306, "America/Chicago", false, Seq("Mason"), Seq(
    ("Odeon Mason", "Odeon Mason", "odeon-mason", "Texas", "Mason")
  ))
  private def p_mason_city: R = ("mason-city", "mason-city-iowa", "Mason City", "Iowa", 43.1536, -93.201, "America/Chicago", true, Seq("Mason City", "Clear Lake"), Seq(
    ("Cinema West Theatre Mason City", "Cinema West Theatre Mason City", "cinema-west-theatre-mason-city", "Iowa", "Mason City"),
    ("Lake Theatre Clear Lake", "Lake Theatre Clear Lake", "lake-theatre-clear-lake", "Iowa", "Clear Lake")
  ))
  private def p_massena: R = ("massena", "massena-new-york", "Massena", "New York", 44.9281, -74.8919, "America/New_York", false, Seq("Massena"), Seq(
    ("56 Auto Drive-in Massena", "56 Auto Drive-in Massena", "56-auto-drive-in-massena", "New York", "Massena"),
    ("Massena Movieplex 8", "Massena Movieplex 8", "massena-movieplex-8", "New York", "Massena")
  ))
  private def p_matamoras: R = ("matamoras", "matamoras-pennsylvania", "Matamoras", "Pennsylvania", 41.3687, -74.7002, "America/New_York", true, Seq("Matamoras", "Milford"), Seq(
    ("Flagship Cinemas Matamoras", "Flagship Cinemas Matamoras", "flagship-cinemas-matamoras", "Pennsylvania", "Matamoras"),
    ("Milford Theater", "Milford Theater", "milford-theater", "Pennsylvania", "Milford")
  ))
  private def p_mattituck: R = ("mattituck", "mattituck-new-york", "Mattituck", "New York", 40.9912, -72.5343, "America/New_York", true, Seq("Mattituck", "Westhampton Beach"), Seq(
    ("Mattituck Theatre 8", "Mattituck Theatre 8", "mattituck-theatre-8", "New York", "Mattituck"),
    ("Sunset Theater Westhampton Beach", "Sunset Theater Westhampton Beach", "sunset-theater-westhampton-beach", "New York", "Westhampton Beach")
  ))
  private def p_mattoon: R = ("mattoon", "mattoon-illinois", "Mattoon", "Illinois", 39.4831, -88.3728, "America/Chicago", false, Seq("Mattoon"), Seq(
    ("VIP Mattoon 10", "VIP Mattoon 10", "vip-mattoon-10", "Illinois", "Mattoon")
  ))
  private def p_mattydale: R = ("mattydale", "mattydale-new-york", "Mattydale", "New York", 43.0978, -76.1452, "America/New_York", true, Seq("Mattydale", "Camillus"), Seq(
    ("Hollywood Theatre Mattydale", "Hollywood Theatre Mattydale", "hollywood-theatre-mattydale", "New York", "Mattydale"),
    ("Movie Tavern Syracuse Township", "Movie Tavern Syracuse Township", "movie-tavern-syracuse-township", "New York", "Camillus")
  ))
  private def p_maumee: R = ("maumee", "maumee-ohio", "Maumee", "Ohio", 41.5628, -83.6538, "America/New_York", true, Seq("Maumee", "Perrysburg", "Oregon"), Seq(
    ("Cinemark Fallen Timbers 14", "Cinemark Fallen Timbers 14", "cinemark-fallen-timbers-14", "Ohio", "Maumee"),
    ("Maumee Theatre", "Maumee Theatre", "maumee-theatre", "Ohio", "Maumee"),
    ("Cinemark Levis Commons 12", "Cinemark Levis Commons 12", "cinemark-levis-commons-12", "Ohio", "Perrysburg"),
    ("Sundance Drive-In Oregon", "Sundance Drive-In Oregon", "sundance-drive-in-oregon", "Ohio", "Oregon")
  ))
  private def p_mayaguez: R = ("mayaguez", "mayaguez-puerto-rico", "Mayagüez", "Puerto Rico", 18.2011, -67.1396, "America/Puerto_Rico", true, Seq("Mayagüez", "San Germán"), Seq(
    ("Caribbean Cinemas San Germán", "Caribbean Cinemas San Germán", "caribbean-cinemas-san-germ-n", "Puerto Rico", "San Germán"),
    ("Caribbean Cinemas Western Plaza", "Caribbean Cinemas Western Plaza", "caribbean-cinemas-western-plaza", "Puerto Rico", "Mayaguez"),
    ("El Cine Mayagüez Town Center", "El Cine Mayagüez Town Center", "el-cine-mayag-ez-town-center", "Puerto Rico", "Mayaguez")
  ))
  private def p_mayfield: R = ("mayfield", "mayfield-kentucky", "Mayfield", "Kentucky", 36.7417, -88.6367, "America/Chicago", false, Seq("Mayfield"), Seq(
    ("Princess Theatres Mayfield", "Princess Theatres Mayfield", "princess-theatres-mayfield", "Kentucky", "Mayfield")
  ))
  private def p_maysville: R = ("maysville", "maysville-kentucky", "Maysville", "Kentucky", 38.6412, -83.7444, "America/New_York", false, Seq("Maysville"), Seq(
    ("Lucas Cinemas Maysville", "Lucas Cinemas Maysville", "lucas-cinemas-maysville", "Kentucky", "Maysville")
  ))
  private def p_mayville: R = ("mayville", "mayville-north-dakota", "Mayville", "North Dakota", 47.498, -97.3245, "America/Chicago", false, Seq("Mayville"), Seq(
    ("Delchar Theater Mayville", "Delchar Theater Mayville", "delchar-theater-mayville", "North Dakota", "Mayville")
  ))
  private def p_mcalester: R = ("mcalester", "mcalester-oklahoma", "McAlester", "Oklahoma", 34.9334, -95.7697, "America/Chicago", false, Seq("McAlester"), Seq(
    ("Apex Cinema McAlester", "Apex Cinema McAlester", "apex-cinema-mcalester", "Oklahoma", "McAlester")
  ))
  private def p_mcconnelsville: R = ("mcconnelsville", "mcconnelsville-ohio", "McConnelsville", "Ohio", 39.6487, -81.8532, "America/New_York", false, Seq("McConnelsville"), Seq(
    ("Twin City Opera House", "Twin City Opera House", "twin-city-opera-house", "Ohio", "McConnelsville")
  ))
  private def p_mccook: R = ("mccook", "mccook-nebraska", "McCook", "Nebraska", 40.2019, -100.6257, "America/Chicago", false, Seq("McCook"), Seq(
    ("Carver Cinemas", "Carver Cinemas", "carver-cinemas", "Nebraska", "McCook")
  ))
  private def p_mcdonough: R = ("mcdonough", "mcdonough-georgia", "McDonough", "Georgia", 33.4473, -84.1469, "America/New_York", true, Seq("McDonough", "Griffin", "Conyers"), Seq(
    ("AMC Conyers Crossing 16", "AMC Conyers Crossing 16", "amc-conyers-crossing-16", "Georgia", "Conyers"),
    ("Regal Griffin", "Regal Griffin", "regal-griffin", "Georgia", "Griffin"),
    ("Regal McDonough", "Regal McDonough", "regal-mcdonough", "Georgia", "McDonough")
  ))
  private def p_mclean: R = ("mclean", "mclean-virginia", "McLean", "Virginia", 38.9343, -77.1775, "America/New_York", false, Seq("McLean"), Seq(
    ("AMC Tysons Corner 16", "AMC Tysons Corner 16", "amc-tysons-corner-16", "Virginia", "McLean"),
    ("CMX Tysons Galleria McLean", "CMX Tysons Galleria McLean", "cmx-tysons-galleria-mclean", "Virginia", "McLean"),
    ("LOOK Cinemas Tysons at the Boro", "LOOK Cinemas Tysons at the Boro", "look-cinemas-tysons-at-the-boro", "Virginia", "McLean")
  ))
  private def p_mcminnville: R = ("mcminnville", "mcminnville-oregon", "McMinnville", "Oregon", 45.2101, -123.1987, "America/Los_Angeles", false, Seq("McMinnville"), Seq(
    ("McMinnville Cinema 10", "McMinnville Cinema 10", "mcminnville-cinema-10", "Oregon", "McMinnville"),
    ("Evergreen Giant Screen Theater", "Evergreen Giant Screen Theater", "evergreen-giant-screen-theater", "Oregon", "McMinnville"),
    ("Moonlight Theatre McMinnville", "Moonlight Theatre McMinnville", "moonlight-theatre-mcminnville", "Oregon", "McMinnville")
  ))
  private def p_mcminnville_tennessee: R = ("mcminnville-tennessee", "mcminnville-tennessee", "McMinnville", "Tennessee", 35.6834, -85.77, "America/Chicago", false, Seq("McMinnville"), Seq(
    ("Three Star Cinema McMinnville", "Three Star Cinema McMinnville", "three-star-cinema-mcminnville", "Tennessee", "McMinnville")
  ))
  private def p_mcpherson: R = ("mcpherson", "mcpherson-kansas", "McPherson", "Kansas", 38.3708, -97.6642, "America/Chicago", false, Seq("McPherson"), Seq(
    ("McPherson Theater", "McPherson Theater", "mcpherson-theater", "Kansas", "McPherson"),
    ("McPherson Opera House", "McPherson Opera House", "mcpherson-opera-house", "Kansas", "McPherson")
  ))
  private def p_meadville: R = ("meadville", "meadville-pennsylvania", "Meadville", "Pennsylvania", 41.6414, -80.1514, "America/New_York", true, Seq("Meadville", "Waterford", "Cochranton"), Seq(
    ("Sunset Drive-in Waterford", "Sunset Drive-in Waterford", "sunset-drive-in-waterford", "Pennsylvania", "East Waterford"),
    ("Movies @ Meadville 10", "Movies @ Meadville 10", "movies-meadville-10", "Pennsylvania", "Meadville"),
    ("Iris Theatre Cochranton", "Iris Theatre Cochranton", "iris-theatre-cochranton", "Pennsylvania", "Cochranton")
  ))
  private def p_medford: R = ("medford", "medford-oregon", "Medford", "Oregon", 42.3265, -122.8756, "America/Los_Angeles", true, Seq("Medford", "Ashland", "White City"), Seq(
    ("Cinemark Tinseltown USA Medford", "Cinemark Tinseltown USA Medford", "cinemark-tinseltown-usa-medford", "Oregon", "Medford"),
    ("Crater Lake Cinema", "Crater Lake Cinema", "crater-lake-cinema", "Oregon", "White City"),
    ("Varsity Theatre Ashland", "Varsity Theatre Ashland", "varsity-theatre-ashland", "Oregon", "Ashland")
  ))
  private def p_medford_wisconsin: R = ("medford-wisconsin", "medford-wisconsin", "Medford", "Wisconsin", 45.1386, -90.3401, "America/Chicago", false, Seq("Medford"), Seq(
    ("Broadway Theatre Medford", "Broadway Theatre Medford", "broadway-theatre-medford", "Wisconsin", "Medford")
  ))
  private def p_melbourne: R = ("melbourne", "melbourne-florida", "Melbourne", "Florida", 28.0836, -80.6081, "America/New_York", true, Seq("Melbourne", "Merritt Island", "West Melbourne", "Satellite Beach"), Seq(
    ("AMC Avenue 16 Melbourne", "AMC Avenue 16 Melbourne", "amc-avenue-16-melbourne", "Florida", "Melbourne"),
    ("Oaks Melbourne 10", "Oaks Melbourne 10", "oaks-melbourne-10", "Florida", "Melbourne"),
    ("AMC CLASSIC West Melbourne 12", "AMC CLASSIC West Melbourne 12", "amc-classic-west-melbourne-12", "Florida", "West Melbourne"),
    ("GQT Merritt Square 16", "GQT Merritt Square 16", "gqt-merritt-square-16", "Florida", "Merritt Island"),
    ("Satellite Beach Cinemas", "Satellite Beach Cinemas", "satellite-beach-cinemas", "Florida", "Satellite Beach")
  ))
  private def p_mena: R = ("mena", "mena-arkansas", "Mena", "Arkansas", 34.5862, -94.2397, "America/Chicago", false, Seq("Mena"), Seq(
    ("Silver Screen Theater Mena", "Silver Screen Theater Mena", "silver-screen-theater-mena", "Arkansas", "Mena")
  ))
  private def p_menomonie: R = ("menomonie", "menomonie-wisconsin", "Menomonie", "Wisconsin", 44.8755, -91.9193, "America/Chicago", false, Seq("Menomonie"), Seq(
    ("Menomonie Stadium 7", "Menomonie Stadium 7", "menomonie-stadium-7", "Wisconsin", "Menomonie")
  ))
  private def p_mentor: R = ("mentor", "mentor-ohio", "Mentor", "Ohio", 41.6662, -81.3396, "America/New_York", true, Seq("Mentor", "Euclid", "Willoughby", "Chardon"), Seq(
    ("Atlas Diamond Center 16", "Atlas Diamond Center 16", "atlas-diamond-center-16", "Ohio", "Mentor"),
    ("Atlas Great Lakes Stadium 16", "Atlas Great Lakes Stadium 16", "atlas-great-lakes-stadium-16", "Ohio", "Mentor"),
    ("Atlas Lakeshore 7", "Atlas Lakeshore 7", "atlas-lakeshore-7", "Ohio", "Euclid"),
    ("Regal Willoughby Commons", "Regal Willoughby Commons", "regal-willoughby-commons", "Ohio", "Willoughby"),
    ("Mayfield Road Drive-In", "Mayfield Road Drive-In", "mayfield-road-drive-in", "Ohio", "Chardon")
  ))
  private def p_merced: R = ("merced", "merced-california", "Merced", "California", 37.3022, -120.483, "America/Los_Angeles", true, Seq("Merced", "Turlock", "Riverbank"), Seq(
    ("Regal Hollywood Merced", "Regal Hollywood Merced", "regal-hollywood-merced", "California", "Merced"),
    ("Galaxy Theatres Riverbank", "Galaxy Theatres Riverbank", "galaxy-theatres-riverbank", "California", "Riverbank"),
    ("Regal Turlock", "Regal Turlock", "regal-turlock", "California", "Turlock")
  ))
  private def p_meridian: R = ("meridian", "meridian-idaho", "Meridian", "Idaho", 43.6121, -116.3915, "America/Boise", true, Seq("Meridian", "Nampa", "Eagle"), Seq(
    ("Cinemark Majestic Cinemas Meridian", "Cinemark Majestic Cinemas Meridian", "cinemark-majestic-cinemas-meridian", "Idaho", "Meridian"),
    ("Village Cinema Meridian", "Village Cinema Meridian", "village-cinema-meridian", "Idaho", "Meridian"),
    ("Eagle Luxe Reel Theatre", "Eagle Luxe Reel Theatre", "eagle-luxe-reel-theatre", "Idaho", "Eagle"),
    ("Regal Edwards Nampa Spectrum", "Regal Edwards Nampa Spectrum", "regal-edwards-nampa-spectrum", "Idaho", "Nampa")
  ))
  private def p_meridian_mississippi: R = ("meridian-mississippi", "meridian-mississippi", "Meridian", "Mississippi", 32.3643, -88.7037, "America/Chicago", false, Seq("Meridian"), Seq(
    ("Golden Ticket Cinemas Meridian 9", "Golden Ticket Cinemas Meridian 9", "golden-ticket-cinemas-meridian-9", "Mississippi", "Meridian")
  ))
  private def p_merrill: R = ("merrill", "merrill-wisconsin", "Merrill", "Wisconsin", 45.1805, -89.6835, "America/Chicago", false, Seq("Merrill"), Seq(
    ("Cosmo Merrill", "Cosmo Merrill", "cosmo-merrill", "Wisconsin", "Merrill")
  ))
  private def p_mesilla: R = ("mesilla", "mesilla-new-mexico", "Mesilla", "New Mexico", 32.2701, -106.8008, "America/Denver", false, Seq("Mesilla"), Seq(
    ("Fountain Theatre Mesilla", "Fountain Theatre Mesilla", "fountain-theatre-mesilla", "New Mexico", "Mesilla")
  ))
  private def p_mesquite_nevada: R = ("mesquite-nevada", "mesquite-nevada", "Mesquite", "Nevada", 36.8055, -114.0672, "America/Los_Angeles", false, Seq("Mesquite"), Seq(
    ("Megaplex Mesquite", "Megaplex Mesquite", "megaplex-mesquite", "Nevada", "Mesquite")
  ))
  private def p_metaline_falls: R = ("metaline-falls", "metaline-falls-washington", "Metaline Falls", "Washington", 48.8625, -117.3705, "America/Los_Angeles", false, Seq("Metaline Falls"), Seq(
    ("Nuvu Theater Metaline Falls", "Nuvu Theater Metaline Falls", "nuvu-theater-metaline-falls", "Washington", "Metaline Falls")
  ))
  private def p_methuen: R = ("methuen", "methuen-massachusetts", "Methuen", "Massachusetts", 42.7262, -71.1909, "America/New_York", true, Seq("Methuen", "Salem", "Merrimack", "Danvers", "Newburyport", "North Reading", "Tyngsboro", "Londonderry", "Salisbury"), Seq(
    ("AMC Liberty Tree Mall 20", "AMC Liberty Tree Mall 20", "amc-liberty-tree-mall-20", "Massachusetts", "Danvers"),
    ("The Screening Room Newburyport", "The Screening Room Newburyport", "the-screening-room-newburyport", "Massachusetts", "Newburyport"),
    ("Vision Max Cinema", "Vision Max Cinema", "vision-max-cinema", "Massachusetts", "Salisbury"),
    ("Apple Cinemas Merrimack", "Apple Cinemas Merrimack", "apple-cinemas-merrimack", "New Hampshire", "Merrimack"),
    ("Cinemark Rockingham Park", "Cinemark Rockingham Park", "cinemark-rockingham-park", "New Hampshire", "Salem"),
    ("O'Neil Cinemas Londonderry", "O'Neil Cinemas Londonderry", "oneil-cinemas-londonderry", "New Hampshire", "Londonderry"),
    ("AMC Methuen 20 (at The Loop)", "AMC Methuen 20 (at The Loop)", "amc-methuen-20-at-the-loop", "Massachusetts", "Methuen"),
    ("AMC Tyngsboro 12", "AMC Tyngsboro 12", "amc-tyngsboro-12", "Massachusetts", "Tyngsboro"),
    ("IMAX 3D Reading (Jordan's)", "IMAX 3D Reading (Jordan's)", "imax-3d-reading-jordans", "Massachusetts", "North Reading")
  ))
  private def p_mexico: R = ("mexico", "mexico-missouri", "Mexico", "Missouri", 39.1698, -91.8829, "America/Chicago", false, Seq("Mexico"), Seq(
    ("Mexico Cinema III", "Mexico Cinema III", "mexico-cinema-iii", "Missouri", "Mexico")
  ))
  private def p_miami_beach: R = ("miami-beach", "miami-beach-florida", "Miami Beach", "Florida", 25.7906, -80.13, "America/New_York", true, Seq("Miami Beach", "Doral", "North Miami Beach", "Aventura", "Coconut Grove", "South Miami"), Seq(
    ("AMC Aventura 24", "AMC Aventura 24", "amc-aventura-24", "Florida", "Aventura"),
    ("AMC Sunset Place 24", "AMC Sunset Place 24", "amc-sunset-place-24", "Florida", "South Miami"),
    ("Cinebistro 7 @ Cityplace", "Cinebistro 7 @ Cityplace", "cinebistro-7-cityplace", "Florida", "Doral"),
    ("Cinepolis Coconut Grove", "Cinepolis Coconut Grove", "cinepolis-coconut-grove", "Florida", "Coconut Grove"),
    ("IPIC Cinemas North Miami Beach", "IPIC Cinemas North Miami Beach", "ipic-cinemas-north-miami-beach", "Florida", "North Miami Beach"),
    ("O Cinema South Beach", "O Cinema South Beach", "o-cinema-south-beach", "Florida", "Miami Beach"),
    ("Rooftop Cinema Miami", "Rooftop Cinema Miami", "rooftop-cinema-miami", "Florida", "Miami Beach")
  ))
  private def p_miami_oklahoma: R = ("miami-oklahoma", "miami-oklahoma", "Miami", "Oklahoma", 36.8745, -94.8775, "America/Chicago", false, Seq("Miami"), Seq(
    ("B&B Miami Cineplex", "B&B Miami Cineplex", "bb-miami-cineplex", "Oklahoma", "Miami")
  ))
  private def p_miamisburg: R = ("miamisburg", "miamisburg-ohio", "Miamisburg", "Ohio", 39.6428, -84.2866, "America/New_York", true, Seq("Miamisburg", "Englewood", "Germantown"), Seq(
    ("Cinepolis Dayton Austin Landing", "Cinepolis Dayton Austin Landing", "cinepolis-dayton-austin-landing", "Ohio", "Miamisburg"),
    ("Plaza Theatre Miamisburg", "Plaza Theatre Miamisburg", "plaza-theatre-miamisburg", "Ohio", "Miamisburg"),
    ("Englewood Cinema", "Englewood Cinema", "englewood-cinema", "Ohio", "Englewood"),
    ("By Jo Germantown", "By Jo Germantown", "by-jo-germantown", "Ohio", "Germantown")
  ))
  private def p_michigan_city: R = ("michigan-city", "michigan-city-indiana", "Michigan City", "Indiana", 41.7075, -86.895, "America/Chicago", true, Seq("Michigan City", "Three Oaks"), Seq(
    ("AMC CLASSIC Michigan City 14", "AMC CLASSIC Michigan City 14", "amc-classic-michigan-city-14", "Indiana", "Michigan City"),
    ("Vickers Theatre Three Oaks", "Vickers Theatre Three Oaks", "vickers-theatre-three-oaks", "Michigan", "Three Oaks")
  ))
  private def p_middle_river: R = ("middle-river", "middle-river-maryland", "Middle River", "Maryland", 39.3343, -76.4394, "America/New_York", true, Seq("Middle River", "Hunt Valley", "Bel Air", "Fallston"), Seq(
    ("Bengies Drive-In Middle River", "Bengies Drive-In Middle River", "bengies-drive-in-middle-river", "Maryland", "Middle River"),
    ("Flagship Cinemas Churchville", "Flagship Cinemas Churchville", "flagship-cinemas-churchville", "Maryland", "Bel Air"),
    ("Horizon Cinemas Fallston", "Horizon Cinemas Fallston", "horizon-cinemas-fallston", "Maryland", "Fallston"),
    ("Regal Hunt Valley", "Regal Hunt Valley", "regal-hunt-valley", "Maryland", "Hunt Valley")
  ))
  private def p_middlebury_village: R = ("middlebury-village", "middlebury-village-vermont", "Middlebury (village)", "Vermont", 44.0155, -73.1694, "America/New_York", false, Seq("Middlebury (village)"), Seq(
    ("Marquis Theatre Middlebury", "Marquis Theatre Middlebury", "marquis-theatre-middlebury", "Vermont", "Middlebury")
  ))
  private def p_middlesboro: R = ("middlesboro", "middlesboro-kentucky", "Middlesboro", "Kentucky", 36.6084, -83.7166, "America/New_York", false, Seq("Middlesboro"), Seq(
    ("Golden Ticket Cinemas Middlesboro 4", "Golden Ticket Cinemas Middlesboro 4", "golden-ticket-cinemas-middlesboro-4", "Kentucky", "Middlesboro")
  ))
  private def p_middletown: R = ("middletown", "middletown-new-york", "Middletown", "New York", 41.4459, -74.4229, "America/New_York", false, Seq("Middletown"), Seq(
    ("AMC Crystal Run 16 Middletown", "AMC Crystal Run 16 Middletown", "amc-crystal-run-16-middletown", "New York", "Middletown"),
    ("Middletown Cinemas 5", "Middletown Cinemas 5", "middletown-cinemas-5", "New York", "Middletown"),
    ("Paramount Cinema Middletown", "Paramount Cinema Middletown", "paramount-cinema-middletown", "New York", "Middletown")
  ))
  private def p_middletown_connecticut: R = ("middletown-connecticut", "middletown-connecticut", "Middletown", "Connecticut", 41.5623, -72.6506, "America/New_York", true, Seq("Middletown", "Southington", "Wallingford", "Plainville", "Kensington"), Seq(
    ("AMC Plainville 20", "AMC Plainville 20", "amc-plainville-20", "Connecticut", "Plainville"),
    ("AMC Southington 12", "AMC Southington 12", "amc-southington-12", "Connecticut", "Southington"),
    ("Southington Drive-In", "Southington Drive-In", "southington-drive-in", "Connecticut", "Plantsville"),
    ("Picture Show at Berlin", "Picture Show at Berlin", "picture-show-at-berlin", "Connecticut", "Berlin"),
    ("Holiday Cinemas Stadium 14", "Holiday Cinemas Stadium 14", "holiday-cinemas-stadium-14", "Connecticut", "Wallingford"),
    ("Metro Movies 12", "Metro Movies 12", "metro-movies-12", "Connecticut", "Middletown"),
    ("Wesleyan University Middletown", "Wesleyan University Middletown", "wesleyan-university-middletown", "Connecticut", "Middletown")
  ))
  private def p_middletown_delaware: R = ("middletown-delaware", "middletown-delaware", "Middletown", "Delaware", 39.4496, -75.7163, "America/New_York", false, Seq("Middletown"), Seq(
    ("Westown Movies Middletown", "Westown Movies Middletown", "westown-movies-middletown", "Delaware", "Middletown"),
    ("Everett Theatre Middletown", "Everett Theatre Middletown", "everett-theatre-middletown", "Delaware", "Middletown")
  ))
  private def p_midland: R = ("midland", "midland-michigan", "Midland", "Michigan", 43.6156, -84.2472, "America/Detroit", true, Seq("Midland", "Bay City", "Beaverton"), Seq(
    ("Gem Theatre Beaverton", "Gem Theatre Beaverton", "gem-theatre-beaverton", "Michigan", "Beaverton"),
    ("NCG Midland", "NCG Midland", "ncg-midland", "Michigan", "Midland"),
    ("Bay City Cinemas", "Bay City Cinemas", "bay-city-cinemas", "Michigan", "Bay City")
  ))
  private def p_midlothian: R = ("midlothian", "midlothian-virginia", "Midlothian", "Virginia", 37.506, -77.6492, "America/New_York", false, Seq("Midlothian"), Seq(
    ("AMC DINE-IN Midlothian 10", "AMC DINE-IN Midlothian 10", "amc-dine-in-midlothian-10", "Virginia", "Midlothian"),
    ("Regal Commonwealth", "Regal Commonwealth", "regal-commonwealth", "Virginia", "Midlothian"),
    ("Regal Westchester Commons Midlothian", "Regal Westchester Commons Midlothian", "regal-westchester-commons-midlothian", "Virginia", "Midlothian")
  ))
  private def p_milbank: R = ("milbank", "milbank-south-dakota", "Milbank", "South Dakota", 45.2191, -96.6356, "America/Chicago", false, Seq("Milbank"), Seq(
    ("Mill Theatres Milbank", "Mill Theatres Milbank", "mill-theatres-milbank", "South Dakota", "Milbank")
  ))
  private def p_milbridge: R = ("milbridge", "milbridge-maine", "Milbridge", "Maine", 44.5354, -67.8808, "America/New_York", false, Seq("Milbridge"), Seq(
    ("Milbridge Theatre & Community Arts Center", "Milbridge Theatre & Community Arts Center", "milbridge-theatre-community-arts-center", "Maine", "Milbridge")
  ))
  private def p_miles_city: R = ("miles-city", "miles-city-montana", "Miles City", "Montana", 46.4083, -105.8406, "America/Denver", false, Seq("Miles City"), Seq(
    ("Montana Theatre Miles City", "Montana Theatre Miles City", "montana-theatre-miles-city", "Montana", "Miles City")
  ))
  private def p_milford: R = ("milford", "milford-connecticut", "Milford", "Connecticut", 41.2223, -73.0565, "America/New_York", true, Seq("Milford", "North Haven"), Seq(
    ("Cinemark Connecticut Post 14", "Cinemark Connecticut Post 14", "cinemark-connecticut-post-14", "Connecticut", "Milford"),
    ("Cinemark North Haven", "Cinemark North Haven", "cinemark-north-haven", "Connecticut", "North Haven")
  ))
  private def p_milford_delaware: R = ("milford-delaware", "milford-delaware", "Milford", "Delaware", 38.9126, -75.428, "America/New_York", true, Seq("Milford", "Lewes"), Seq(
    ("Cinema Art Theater", "Cinema Art Theater", "cinema-art-theater", "Delaware", "Lewes"),
    ("Milford Movies 9", "Milford Movies 9", "milford-movies-9", "Delaware", "Milford")
  ))
  private def p_milford_new_hampshire: R = ("milford-new-hampshire", "milford-new-hampshire", "Milford", "New Hampshire", 42.8354, -71.649, "America/New_York", true, Seq("Milford", "Peterborough", "Wilton"), Seq(
    ("Wilton Town Hall Theatre", "Wilton Town Hall Theatre", "wilton-town-hall-theatre", "New Hampshire", "Wilton"),
    ("Peterborough Community Theatre", "Peterborough Community Theatre", "peterborough-community-theatre", "New Hampshire", "Peterborough"),
    ("Milford Drive-In Theater", "Milford Drive-In Theater", "milford-drive-in-theater", "New Hampshire", "Milford")
  ))
  private def p_millbury: R = ("millbury", "millbury-massachusetts", "Millbury", "Massachusetts", 42.194, -71.7601, "America/New_York", true, Seq("Millbury", "Mendon", "Sturbridge"), Seq(
    ("Mendon Twin Drive-in", "Mendon Twin Drive-in", "mendon-twin-drive-in", "Massachusetts", "Mendon"),
    ("Blackstone Valley 14 Millbury", "Blackstone Valley 14 Millbury", "blackstone-valley-14-millbury", "Massachusetts", "Millbury"),
    ("Elm Draught House Cinema Millbury", "Elm Draught House Cinema Millbury", "elm-draught-house-cinema-millbury", "Massachusetts", "Millbury"),
    ("Sturbridge Luxury Cinemas", "Sturbridge Luxury Cinemas", "sturbridge-luxury-cinemas", "Massachusetts", "Sturbridge")
  ))
  private def p_milledgeville: R = ("milledgeville", "milledgeville-georgia", "Milledgeville", "Georgia", 33.0801, -83.2321, "America/New_York", false, Seq("Milledgeville"), Seq(
    ("AMC CLASSIC Milledgeville 6", "AMC CLASSIC Milledgeville 6", "amc-classic-milledgeville-6", "Georgia", "Milledgeville")
  ))
  private def p_miller: R = ("miller", "miller-south-dakota", "Miller", "South Dakota", 44.5183, -98.9884, "America/Chicago", false, Seq("Miller"), Seq(
    ("Midway Drive In Miller", "Midway Drive In Miller", "midway-drive-in-miller", "South Dakota", "Miller")
  ))
  private def p_millsboro: R = ("millsboro", "millsboro-delaware", "Millsboro", "Delaware", 38.5915, -75.2913, "America/New_York", true, Seq("Millsboro", "Rehoboth Beach"), Seq(
    ("Clayton Theatre Dagsboro", "Clayton Theatre Dagsboro", "clayton-theatre-dagsboro", "Delaware", "Dagsboro"),
    ("Movies At Midway – Rehoboth Beach", "Movies At Midway – Rehoboth Beach", "movies-at-midway---rehoboth-beach", "Delaware", "Rehoboth Beach")
  ))
  private def p_mineola: R = ("mineola", "mineola-texas", "Mineola", "Texas", 32.6632, -95.4883, "America/Chicago", false, Seq("Mineola"), Seq(
    ("Lake Country Playhouse", "Lake Country Playhouse", "lake-country-playhouse", "Texas", "Mineola")
  ))
  private def p_mineral_wells: R = ("mineral-wells", "mineral-wells-texas", "Mineral Wells", "Texas", 32.8085, -98.1128, "America/Chicago", false, Seq("Mineral Wells"), Seq(
    ("Hometown Cinemas Mineral Wells", "Hometown Cinemas Mineral Wells", "hometown-cinemas-mineral-wells", "Texas", "Mineral Wells")
  ))
  private def p_minot: R = ("minot", "minot-north-dakota", "Minot", "North Dakota", 48.2325, -101.2963, "America/Chicago", false, Seq("Minot"), Seq(
    ("AMC Dakota Square 9", "AMC Dakota Square 9", "amc-dakota-square-9", "North Dakota", "Minot"),
    ("Oak Park Theater Minot", "Oak Park Theater Minot", "oak-park-theater-minot", "North Dakota", "Minot")
  ))
  private def p_mishawaka: R = ("mishawaka", "mishawaka-indiana", "Mishawaka", "Indiana", 41.662, -86.1586, "America/Indiana/Indianapolis", true, Seq("Mishawaka", "Goshen", "Niles", "Nappanee", "Notre Dame"), Seq(
    ("Browning Cinema", "Browning Cinema", "browning-cinema", "Indiana", "Notre Dame"),
    ("Cinemark South Bend", "Cinemark South Bend", "cinemark-south-bend", "Indiana", "Mishawaka"),
    ("Linway Cinema 14", "Linway Cinema 14", "linway-cinema-14", "Indiana", "Goshen"),
    ("Wonderland Cinema Niles Michigan", "Wonderland Cinema Niles Michigan", "wonderland-cinema-niles-michigan", "Michigan", "Niles"),
    ("Nappanee Theatre", "Nappanee Theatre", "nappanee-theatre", "Indiana", "Nappanee")
  ))
  private def p_missoula: R = ("missoula", "missoula-montana", "Missoula", "Montana", 46.8721, -113.994, "America/Denver", false, Seq("Missoula"), Seq(
    ("AMC DINE-IN Southgate 9", "AMC DINE-IN Southgate 9", "amc-dine-in-southgate-9", "Montana", "Missoula"),
    ("AMC Missoula 12", "AMC Missoula 12", "amc-missoula-12", "Montana", "Missoula"),
    ("The Roxy Theater Missoula", "The Roxy Theater Missoula", "the-roxy-theater-missoula", "Montana", "Missoula")
  ))
  private def p_missouri_city: R = ("missouri-city", "missouri-city-texas", "Missouri City", "Texas", 29.6186, -95.5377, "America/Chicago", true, Seq("Missouri City", "Sugar Land", "Stafford"), Seq(
    ("AMC First Colony 24", "AMC First Colony 24", "amc-first-colony-24", "Texas", "Sugar Land"),
    ("AMC Fountains 18 Stafford", "AMC Fountains 18 Stafford", "amc-fountains-18-stafford", "Texas", "Stafford"),
    ("Cinemark Missouri City 14", "Cinemark Missouri City 14", "cinemark-missouri-city-14", "Texas", "Missouri City"),
    ("Star Cinema Grill Missouri City", "Star Cinema Grill Missouri City", "star-cinema-grill-missouri-city", "Texas", "Missouri City")
  ))
  private def p_mitchell: R = ("mitchell", "mitchell-south-dakota", "Mitchell", "South Dakota", 43.7094, -98.0298, "America/Chicago", false, Seq("Mitchell"), Seq(
    ("Odyssey Luxury Mitchell 5", "Odyssey Luxury Mitchell 5", "odyssey-luxury-mitchell-5", "South Dakota", "Mitchell")
  ))
  private def p_moab: R = ("moab", "moab-utah", "Moab", "Utah", 38.5733, -109.5498, "America/Denver", false, Seq("Moab"), Seq(
    ("Slickrock Cinema Moab", "Slickrock Cinema Moab", "slickrock-cinema-moab", "Utah", "Moab")
  ))
  private def p_moberly: R = ("moberly", "moberly-missouri", "Moberly", "Missouri", 39.4184, -92.4382, "America/Chicago", false, Seq("Moberly"), Seq(
    ("B&B Moberly Five and Drive", "B&B Moberly Five and Drive", "bb-moberly-five-and-drive", "Missouri", "Moberly")
  ))
  private def p_mobridge: R = ("mobridge", "mobridge-south-dakota", "Mobridge", "South Dakota", 45.5372, -100.4279, "America/Chicago", false, Seq("Mobridge"), Seq(
    ("Pheasant Drive-In Mobridge", "Pheasant Drive-In Mobridge", "pheasant-drive-in-mobridge", "South Dakota", "Mobridge"),
    ("Mac Theatre Mobridge", "Mac Theatre Mobridge", "mac-theatre-mobridge", "South Dakota", "Mobridge")
  ))
  private def p_moline: R = ("moline", "moline-illinois", "Moline", "Illinois", 41.5067, -90.5151, "America/Chicago", true, Seq("Moline", "Blue Grass"), Seq(
    ("Blue Grass Drive-In Theater", "Blue Grass Drive-In Theater", "blue-grass-drive-in-theater", "Iowa", "Blue Grass"),
    ("VIP Moline 14", "VIP Moline 14", "vip-moline-14", "Illinois", "Moline")
  ))
  private def p_monett: R = ("monett", "monett-missouri", "Monett", "Missouri", 36.929, -93.9277, "America/Chicago", true, Seq("Monett", "Aurora"), Seq(
    ("B&B Monett Plaza 8", "B&B Monett Plaza 8", "bb-monett-plaza-8", "Missouri", "Monett"),
    ("Sunset Drive-in Aurora", "Sunset Drive-in Aurora", "sunset-drive-in-aurora", "Missouri", "Aurora")
  ))
  private def p_monetta: R = ("monetta", "monetta-south-carolina", "Monetta", "South Carolina", 33.8519, -81.5983, "America/New_York", false, Seq("Monetta"), Seq(
    ("Monetta Drive-in 3", "Monetta Drive-in 3", "monetta-drive-in-3", "South Carolina", "Monetta")
  ))
  private def p_monroe: R = ("monroe", "monroe-michigan", "Monroe", "Michigan", 41.9164, -83.3977, "America/Detroit", false, Seq("Monroe"), Seq(
    ("Phoenix Theatres Mall of Monroe", "Phoenix Theatres Mall of Monroe", "phoenix-theatres-mall-of-monroe", "Michigan", "Monroe"),
    ("Memory Lane Drive-in", "Memory Lane Drive-in", "memory-lane-drive-in", "Michigan", "Monroe")
  ))
  private def p_monroe_wisconsin: R = ("monroe-wisconsin", "monroe-wisconsin", "Monroe", "Wisconsin", 42.6011, -89.6385, "America/Chicago", false, Seq("Monroe"), Seq(
    ("Sky Vu Drive In Monroe", "Sky Vu Drive In Monroe", "sky-vu-drive-in-monroe", "Wisconsin", "Monroe")
  ))
  private def p_monterey: R = ("monterey", "monterey-california", "Monterey", "California", 36.6002, -121.8947, "America/Los_Angeles", true, Seq("Monterey", "Pacific Grove"), Seq(
    ("Cinemark Monterey 13", "Cinemark Monterey 13", "cinemark-monterey-13", "California", "Monterey"),
    ("Lighthouse Cinema", "Lighthouse Cinema", "lighthouse-cinema", "California", "Pacific Grove")
  ))
  private def p_montevideo: R = ("montevideo", "montevideo-minnesota", "Montevideo", "Minnesota", 44.948, -95.717, "America/Chicago", false, Seq("Montevideo"), Seq(
    ("Millennium Theater Montevideo", "Millennium Theater Montevideo", "millennium-theater-montevideo", "Minnesota", "Montevideo")
  ))
  private def p_monticello: R = ("monticello", "monticello-indiana", "Monticello", "Indiana", 40.7453, -86.7647, "America/Indiana/Indianapolis", false, Seq("Monticello"), Seq(
    ("Lakeshore Drive-in Monticello", "Lakeshore Drive-in Monticello", "lakeshore-drive-in-monticello", "Indiana", "Monticello"),
    ("Peacock Theatre Monticello", "Peacock Theatre Monticello", "peacock-theatre-monticello", "Indiana", "Monticello")
  ))
  private def p_monticello_arkansas: R = ("monticello-arkansas", "monticello-arkansas", "Monticello", "Arkansas", 33.629, -91.791, "America/Chicago", false, Seq("Monticello"), Seq(
    ("Malco Hollywood Monticello", "Malco Hollywood Monticello", "malco-hollywood-monticello", "Arkansas", "Monticello")
  ))
  private def p_montpelier: R = ("montpelier", "montpelier-vermont", "Montpelier", "Vermont", 44.2601, -72.5754, "America/New_York", true, Seq("Montpelier", "Barre", "Waitsfield"), Seq(
    ("Big Picture Theater Waitsfield", "Big Picture Theater Waitsfield", "big-picture-theater-waitsfield", "Vermont", "Waitsfield"),
    ("Capitol Theatre 5 Montpelier", "Capitol Theatre 5 Montpelier", "capitol-theatre-5-montpelier", "Vermont", "Montpelier"),
    ("Savoy Theatre Montpelier", "Savoy Theatre Montpelier", "savoy-theatre-montpelier", "Vermont", "Montpelier"),
    ("Paramount Theatre Barre", "Paramount Theatre Barre", "paramount-theatre-barre", "Vermont", "Barre")
  ))
  private def p_montpelier_idaho: R = ("montpelier-idaho", "montpelier-idaho", "Montpelier", "Idaho", 42.3222, -111.2977, "America/Boise", false, Seq("Montpelier"), Seq(
    ("Centre Theatre Montpelier", "Centre Theatre Montpelier", "centre-theatre-montpelier", "Idaho", "Montpelier")
  ))
  private def p_montrose: R = ("montrose", "montrose-colorado", "Montrose", "Colorado", 38.4783, -107.8762, "America/Denver", false, Seq("Montrose"), Seq(
    ("Fox Cinema Center", "Fox Cinema Center", "fox-cinema-center", "Colorado", "Montrose"),
    ("Star Drive-in Montrose", "Star Drive-in Montrose", "star-drive-in-montrose", "Colorado", "Montrose")
  ))
  private def p_moore: R = ("moore", "moore-oklahoma", "Moore", "Oklahoma", 35.3395, -97.4867, "America/Chicago", true, Seq("Moore", "Midwest City", "Yukon"), Seq(
    ("AMC West End Pointe 8", "AMC West End Pointe 8", "amc-west-end-pointe-8", "Oklahoma", "Yukon"),
    ("Regal Warren Midwest City", "Regal Warren Midwest City", "regal-warren-midwest-city", "Oklahoma", "Midwest City"),
    ("Regal Warren Moore", "Regal Warren Moore", "regal-warren-moore", "Oklahoma", "Moore")
  ))
  private def p_moorefield: R = ("moorefield", "moorefield-west-virginia", "Moorefield", "West Virginia", 39.0623, -78.9695, "America/New_York", false, Seq("Moorefield"), Seq(
    ("South Branch Cinema 6", "South Branch Cinema 6", "south-branch-cinema-6", "West Virginia", "Moorefield")
  ))
  private def p_moose_lake: R = ("moose-lake", "moose-lake-minnesota", "Moose Lake", "Minnesota", 46.4541, -92.7619, "America/Chicago", false, Seq("Moose Lake"), Seq(
    ("Lake Theater Moose Lake", "Lake Theater Moose Lake", "lake-theater-moose-lake", "Minnesota", "Moose Lake")
  ))
  private def p_mora: R = ("mora", "mora-minnesota", "Mora", "Minnesota", 45.8769, -93.2938, "America/Chicago", false, Seq("Mora"), Seq(
    ("Paradise Theatre Mora", "Paradise Theatre Mora", "paradise-theatre-mora", "Minnesota", "Mora")
  ))
  private def p_morehead: R = ("morehead", "morehead-kentucky", "Morehead", "Kentucky", 38.184, -83.4327, "America/New_York", false, Seq("Morehead"), Seq(
    ("Morehead Cinemas 6", "Morehead Cinemas 6", "morehead-cinemas-6", "Kentucky", "Morehead")
  ))
  private def p_morenci: R = ("morenci", "morenci-arizona", "Morenci", "Arizona", 33.052, -109.328, "America/Phoenix", false, Seq("Morenci"), Seq(
    ("Allen Theatres Morenci", "Allen Theatres Morenci", "allen-theatres-morenci", "Arizona", "Morenci")
  ))
  private def p_morgantown: R = ("morgantown", "morgantown-west-virginia", "Morgantown", "West Virginia", 39.6295, -79.9559, "America/New_York", true, Seq("Morgantown", "Monongah"), Seq(
    ("Tygart Valley Cinemas", "Tygart Valley Cinemas", "tygart-valley-cinemas", "West Virginia", "White Hall"),
    ("AMC Morgantown 12", "AMC Morgantown 12", "amc-morgantown-12", "West Virginia", "Morgantown")
  ))
  private def p_morris: R = ("morris", "morris-minnesota", "Morris", "Minnesota", 45.5861, -95.9139, "America/Chicago", false, Seq("Morris"), Seq(
    ("Morris Theatre", "Morris Theatre", "morris-theatre", "Minnesota", "Morris")
  ))
  private def p_morristown: R = ("morristown", "morristown-tennessee", "Morristown", "Tennessee", 36.214, -83.2949, "America/New_York", false, Seq("Morristown"), Seq(
    ("AMC CLASSIC College Square 12", "AMC CLASSIC College Square 12", "amc-classic-college-square-12", "Tennessee", "Morristown")
  ))
  private def p_morton: R = ("morton", "morton-washington", "Morton", "Washington", 46.5584, -122.2751, "America/Los_Angeles", false, Seq("Morton"), Seq(
    ("Roxy Theater Morton", "Roxy Theater Morton", "roxy-theater-morton", "Washington", "Morton")
  ))
  private def p_moscow: R = ("moscow", "moscow-idaho", "Moscow", "Idaho", 46.7324, -117.0002, "America/Los_Angeles", false, Seq("Moscow"), Seq(
    ("Golden Ticket Cinema Eastside", "Golden Ticket Cinema Eastside", "village-centre-at-eastside", "Idaho", "Moscow"),
    ("Kenworthy Performing Arts Centre", "Kenworthy Performing Arts Centre", "kenworthy-performing-arts-centre", "Idaho", "Moscow")
  ))
  private def p_moses_lake: R = ("moses-lake", "moses-lake-washington", "Moses Lake", "Washington", 47.1301, -119.2781, "America/Los_Angeles", true, Seq("Moses Lake", "Ephrata"), Seq(
    ("Fairchild Moses Lake 8", "Fairchild Moses Lake 8", "fairchild-moses-lake-8", "Washington", "Moses Lake"),
    ("Lee Theatre Ephrata", "Lee Theatre Ephrata", "lee-theatre-ephrata", "Washington", "Ephrata")
  ))
  private def p_mott: R = ("mott", "mott-north-dakota", "Mott", "North Dakota", 46.3725, -102.3271, "America/Denver", false, Seq("Mott"), Seq(
    ("Playhouse Theatre Mott", "Playhouse Theatre Mott", "playhouse-theatre-mott", "North Dakota", "Mott")
  ))
  private def p_moultrie: R = ("moultrie", "moultrie-georgia", "Moultrie", "Georgia", 31.1799, -83.7891, "America/New_York", false, Seq("Moultrie"), Seq(
    ("GTC Moultrie Cinemas", "GTC Moultrie Cinemas", "gtc-moultrie-cinemas", "Georgia", "Moultrie")
  ))
  private def p_mount_airy: R = ("mount-airy", "mount-airy-north-carolina", "Mount Airy", "North Carolina", 36.4993, -80.6073, "America/New_York", false, Seq("Mount Airy"), Seq(
    ("Earle Theatre Mt Airy", "Earle Theatre Mt Airy", "earle-theatre-mt-airy", "North Carolina", "Mount Airy"),
    ("Creekside Stadium 10", "Creekside Stadium 10", "creekside-stadium-10", "North Carolina", "Mount Airy")
  ))
  private def p_mount_airy_georgia: R = ("mount-airy-georgia", "mount-airy-georgia", "Mount Airy", "Georgia", 34.5187, -83.5007, "America/New_York", false, Seq("Mount Airy"), Seq(
    ("GTC Habersham Hills Cinemas", "GTC Habersham Hills Cinemas", "gtc-habersham-hills-cinemas", "Georgia", "Mount Airy")
  ))
  private def p_mount_gilead: R = ("mount-gilead", "mount-gilead-ohio", "Mount Gilead", "Ohio", 40.5492, -82.8274, "America/New_York", false, Seq("Mount Gilead"), Seq(
    ("Capitol Mt Gilead", "Capitol Mt Gilead", "capitol-mt-gilead", "Ohio", "Mount Gilead")
  ))
  private def p_mount_lebanon: R = ("mount-lebanon", "mount-lebanon-pennsylvania", "Mount Lebanon", "Pennsylvania", 40.3554, -80.0495, "America/New_York", true, Seq("Mount Lebanon", "Monroeville", "West Mifflin", "Oakmont", "McKees Rocks", "Coraopolis", "Bridgeville", "Sewickley", "Homestead", "Rennerdale"), Seq(
    ("AMC CLASSIC Mount Lebanon 6", "AMC CLASSIC Mount Lebanon 6", "amc-classic-mount-lebanon-6", "Pennsylvania", "Mount Lebanon"),
    ("AMC Waterfront 22", "AMC Waterfront 22", "amc-waterfront-22", "Pennsylvania", "Homestead"),
    ("Century Square Luxury Cinemas West Mifflin", "Century Square Luxury Cinemas West Mifflin", "century-square-luxury-cinemas-west-mifflin", "Pennsylvania", "West Mifflin"),
    ("Cinemark Monroeville Mall 12", "Cinemark Monroeville Mall 12", "cinemark-monroeville-mall-12", "Pennsylvania", "Monroeville"),
    ("Dependable Drive-In Coraopolis", "Dependable Drive-In Coraopolis", "dependable-drive-in-coraopolis", "Pennsylvania", "Coraopolis"),
    ("Oaks Theatre Oakmont", "Oaks Theatre Oakmont", "oaks-theatre-oakmont", "Pennsylvania", "East Oakmont"),
    ("Phoenix Theatres Chartiers 14", "Phoenix Theatres Chartiers 14", "phoenix-theatres-chartiers-14", "Pennsylvania", "Bridgeville"),
    ("Robinson Robinson Township", "Robinson Robinson Township", "robinson-robinson-township", "Pennsylvania", "Robinson Township"),
    ("The Lindsay Theater and Cultural Center", "The Lindsay Theater and Cultural Center", "the-lindsay-theater-and-cultural-center", "Pennsylvania", "Sewickley"),
    ("Parkway Theater & Film Lounge", "Parkway Theater & Film Lounge", "parkway-theater-film-lounge", "Pennsylvania", "McKees Rocks")
  ))
  private def p_mount_pleasant: R = ("mount-pleasant", "mount-pleasant-south-carolina", "Mount Pleasant", "South Carolina", 32.7941, -79.8626, "America/New_York", false, Seq("Mount Pleasant"), Seq(
    ("Regal Palmetto Grand Mt Pleasant", "Regal Palmetto Grand Mt Pleasant", "regal-palmetto-grand-mt-pleasant", "South Carolina", "Mount Pleasant")
  ))
  private def p_mount_pleasant_iowa: R = ("mount-pleasant-iowa", "mount-pleasant-iowa", "Mount Pleasant", "Iowa", 40.9636, -91.5579, "America/Chicago", false, Seq("Mount Pleasant"), Seq(
    ("Main Street Cinemas Mt Pleasant", "Main Street Cinemas Mt Pleasant", "main-street-cinemas-mt-pleasant", "Iowa", "Mount Pleasant")
  ))
  private def p_mount_pleasant_michigan: R = ("mount-pleasant-michigan", "mount-pleasant-michigan", "Mount Pleasant", "Michigan", 43.5978, -84.7675, "America/Detroit", true, Seq("Mount Pleasant", "Alma", "Clare"), Seq(
    ("Celebration! Mt. Pleasant", "Celebration! Mt. Pleasant", "celebration-mt-pleasant", "Michigan", "Mount Pleasant"),
    ("Ideal Theatre Clare", "Ideal Theatre Clare", "ideal-theatre-clare", "Michigan", "Clare"),
    ("NCG Alma Cinema", "NCG Alma Cinema", "ncg-alma-cinema", "Michigan", "Alma")
  ))
  private def p_mount_pleasant_texas: R = ("mount-pleasant-texas", "mount-pleasant-texas", "Mount Pleasant", "Texas", 33.1568, -94.9683, "America/Chicago", true, Seq("Mount Pleasant", "Daingerfield"), Seq(
    ("City Lights Mt Pleasant 6", "City Lights Mt Pleasant 6", "city-lights-mt-pleasant-6", "Texas", "Mount Pleasant"),
    ("Morris Daingerfield 2", "Morris Daingerfield 2", "morris-daingerfield-2", "Texas", "Daingerfield")
  ))
  private def p_mount_pleasant_utah: R = ("mount-pleasant-utah", "mount-pleasant-utah", "Mount Pleasant", "Utah", 39.5469, -111.4555, "America/Denver", false, Seq("Mount Pleasant"), Seq(
    ("Basin Drive-in Mt Pleasant", "Basin Drive-in Mt Pleasant", "basin-drive-in-mt-pleasant", "Utah", "Mount Pleasant")
  ))
  private def p_mount_shasta: R = ("mount-shasta", "mount-shasta-california", "Mount Shasta", "California", 41.3102, -122.3123, "America/Los_Angeles", false, Seq("Mount Shasta"), Seq(
    ("Mt. Shasta Cinema", "Mt. Shasta Cinema", "mt-shasta-cinema", "California", "Mount Shasta")
  ))
  private def p_mount_sterling: R = ("mount-sterling", "mount-sterling-kentucky", "Mount Sterling", "Kentucky", 38.0565, -83.9433, "America/New_York", false, Seq("Mount Sterling"), Seq(
    ("Tenth Frame Cinema Mt Sterling", "Tenth Frame Cinema Mt Sterling", "tenth-frame-cinema-mt-sterling", "Kentucky", "Mount Sterling"),
    ("Judy Drive-In Mt Sterling", "Judy Drive-In Mt Sterling", "judy-drive-in-mt-sterling", "Kentucky", "Mount Sterling")
  ))
  private def p_mount_vernon: R = ("mount-vernon", "mount-vernon-washington", "Mount Vernon", "Washington", 48.4212, -122.3341, "America/Los_Angeles", true, Seq("Mount Vernon", "Anacortes", "Burlington"), Seq(
    ("AMC Cascade Mall 14", "AMC Cascade Mall 14", "amc-cascade-mall-14", "Washington", "Burlington"),
    ("Anacortes Cinemas", "Anacortes Cinemas", "anacortes-cinemas", "Washington", "Anacortes"),
    ("Lincoln Theatre Mt Vernon", "Lincoln Theatre Mt Vernon", "lincoln-theatre-mt-vernon", "Washington", "Mount Vernon")
  ))
  private def p_mount_vernon_illinois: R = ("mount-vernon-illinois", "mount-vernon-illinois", "Mount Vernon", "Illinois", 38.3173, -88.9031, "America/Chicago", false, Seq("Mount Vernon"), Seq(
    ("AMC CLASSIC Mount Vernon 8", "AMC CLASSIC Mount Vernon 8", "amc-classic-mount-vernon-8", "Illinois", "Mount Vernon")
  ))
  private def p_mount_vernon_ohio: R = ("mount-vernon-ohio", "mount-vernon-ohio", "Mount Vernon", "Ohio", 40.3934, -82.4857, "America/New_York", false, Seq("Mount Vernon"), Seq(
    ("UEC Theatres Mount Vernon", "UEC Theatres Mount Vernon", "uec-theatres-mount-vernon", "Ohio", "Mount Vernon")
  ))
  private def p_mountain_grove: R = ("mountain-grove", "mountain-grove-missouri", "Mountain Grove", "Missouri", 37.1306, -92.2635, "America/Chicago", false, Seq("Mountain Grove"), Seq(
    ("Fun City 5 Cinemas", "Fun City 5 Cinemas", "fun-city-5-cinemas", "Missouri", "Mountain Grove")
  ))
  private def p_mountain_home: R = ("mountain-home", "mountain-home-arkansas", "Mountain Home", "Arkansas", 36.3353, -92.3852, "America/Chicago", false, Seq("Mountain Home"), Seq(
    ("Sun Valley Cinema", "Sun Valley Cinema", "sun-valley-cinema", "Arkansas", "Mountain Home")
  ))
  private def p_mountain_view: R = ("mountain-view", "mountain-view-california", "Mountain View", "California", 37.386, -122.0838, "America/Los_Angeles", true, Seq("Mountain View", "Campbell", "East Palo Alto", "Redwood City", "Milpitas", "Newark", "Los Gatos"), Seq(
    ("Alamo Drafthouse Mountain View", "Alamo Drafthouse Mountain View", "alamo-drafthouse-mountain-view", "California", "Mountain View"),
    ("Cinemark Century Mountain View 16", "Cinemark Century Mountain View 16", "cinemark-century-mountain-view-16", "California", "Mountain View"),
    ("CineLux Los Gatos Theatre", "CineLux Los Gatos Theatre", "cinelux-los-gatos-theatre", "California", "Los Gatos"),
    ("CineLux Plaza Theatre Campbell", "CineLux Plaza Theatre Campbell", "cinelux-plaza-theatre-campbell", "California", "Campbell"),
    ("Pruneyard Cinemas", "Pruneyard Cinemas", "pruneyard-cinemas", "California", "Campbell"),
    ("Cinemark Century Great Mall 20", "Cinemark Century Great Mall 20", "cinemark-century-great-mall-20", "California", "Milpitas"),
    ("Landmark Aquarius", "Landmark Aquarius", "landmark-aquarius", "California", "East Palo Alto"),
    ("Stanford Palo Alto", "Stanford Palo Alto", "stanford-palo-alto", "California", "East Palo Alto"),
    ("AMC Newpark 12", "AMC Newpark 12", "amc-newpark-newark-12", "California", "Newark"),
    ("Cinemark Century Redwood Downtown 20", "Cinemark Century Redwood Downtown 20", "cinemark-century-redwood-downtown-20", "California", "Redwood City")
  ))
  private def p_mountain_view_arkansas: R = ("mountain-view-arkansas", "mountain-view-arkansas", "Mountain View", "Arkansas", 35.8684, -92.1176, "America/Chicago", false, Seq("Mountain View"), Seq(
    ("Stone Drive-In Theatre", "Stone Drive-In Theatre", "stone-drive-in-theatre", "Arkansas", "Mountain View"),
    ("Stream Theater Mountain View", "Stream Theater Mountain View", "stream-theater-mountain-view", "Arkansas", "Mountain View")
  ))
  private def p_muncie: R = ("muncie", "muncie-indiana", "Muncie", "Indiana", 40.1934, -85.3864, "America/Indiana/Indianapolis", true, Seq("Muncie", "Alexandria"), Seq(
    ("Alexandria Movie Theatre", "Alexandria Movie Theatre", "alexandria-movie-theatre", "Indiana", "Alexandria"),
    ("AMC Muncie 12", "AMC Muncie 12", "amc-muncie-12", "Indiana", "Muncie")
  ))
  private def p_murray: R = ("murray", "murray-kentucky", "Murray", "Kentucky", 36.6103, -88.3148, "America/Chicago", true, Seq("Murray", "Paris", "McKenzie", "Huntingdon"), Seq(
    ("Court Theatre Huntingdon", "Court Theatre Huntingdon", "court-theatre-huntingdon", "Tennessee", "Huntingdon"),
    ("Parisian 6", "Parisian 6", "parisian-6", "Tennessee", "Paris"),
    ("Park Theatre McKenzie", "Park Theatre McKenzie", "park-theatre-mckenzie", "Tennessee", "McKenzie"),
    ("Cheri Theatres Murray", "Cheri Theatres Murray", "cheri-theatres-murray", "Kentucky", "Murray")
  ))
  private def p_muscatine: R = ("muscatine", "muscatine-iowa", "Muscatine", "Iowa", 41.4245, -91.0432, "America/Chicago", true, Seq("Muscatine", "West Liberty", "Aledo"), Seq(
    ("Aledo Opera House", "Aledo Opera House", "aledo-opera-house", "Illinois", "Aledo"),
    ("Fridley Muscatine Palms 10", "Fridley Muscatine Palms 10", "fridley-muscatine-palms-10", "Iowa", "Muscatine"),
    ("New Strand Theatre West Liberty", "New Strand Theatre West Liberty", "new-strand-theatre-west-liberty", "Iowa", "West Liberty")
  ))
  private def p_muskegon: R = ("muskegon", "muskegon-michigan", "Muskegon", "Michigan", 43.2342, -86.2484, "America/Detroit", true, Seq("Muskegon", "Grand Haven", "Fremont", "Whitehall"), Seq(
    ("Celebration! Carousel", "Celebration! Carousel", "celebration-carousel", "Michigan", "Muskegon"),
    ("Celebration! Getty Drive-in", "Celebration! Getty Drive-in", "celebration-getty-drive-in", "Michigan", "Muskegon"),
    ("Fremont Cinemas", "Fremont Cinemas", "fremont-cinemas", "Michigan", "Fremont"),
    ("GQT Grand Haven 9", "GQT Grand Haven 9", "gqt-grand-haven-9", "Michigan", "Grand Haven"),
    ("Northstar Cinemas Whitehall", "Northstar Cinemas Whitehall", "northstar-cinemas-whitehall", "Michigan", "South Whitehall")
  ))
  private def p_muskogee: R = ("muskogee", "muskogee-oklahoma", "Muskogee", "Oklahoma", 35.7479, -95.3697, "America/Chicago", false, Seq("Muskogee"), Seq(
    ("Apex Cinema Muskogee", "Apex Cinema Muskogee", "apex-cinema-muskogee", "Oklahoma", "Muskogee")
  ))
  private def p_myrtle_beach: R = ("myrtle-beach", "myrtle-beach-south-carolina", "Myrtle Beach", "South Carolina", 33.6891, -78.8867, "America/New_York", false, Seq("Myrtle Beach"), Seq(
    ("AMC CLASSIC Myrtle Beach 12", "AMC CLASSIC Myrtle Beach 12", "amc-classic-myrtle-beach-12", "South Carolina", "Myrtle Beach"),
    ("Cinemark Myrtle Beach 14", "Cinemark Myrtle Beach 14", "cinemark-myrtle-beach-14", "South Carolina", "Myrtle Beach"),
    ("GTC Market Common Cinemas", "GTC Market Common Cinemas", "gtc-market-common-cinemas", "South Carolina", "Myrtle Beach")
  ))
  private def p_nantucket: R = ("nantucket", "nantucket-massachusetts", "Nantucket", "Massachusetts", 41.2835, -70.0995, "America/New_York", true, Seq("Nantucket", "Siasconset"), Seq(
    ("Dreamland Nantucket", "Dreamland Nantucket", "dreamland-nantucket", "Massachusetts", "Nantucket"),
    ("Dreamland Drive In", "Dreamland Drive In", "dreamland-drive-in", "Massachusetts", "Nantucket"),
    ("Siasconset Casino Association Theater", "Siasconset Casino Association Theater", "siasconset-casino-association-theater", "Massachusetts", "Siasconset")
  ))
  private def p_naples: R = ("naples", "naples-florida", "Naples", "Florida", 26.1423, -81.796, "America/New_York", false, Seq("Naples"), Seq(
    ("CMX CinéBistro Coastland", "CMX CinéBistro Coastland", "cmx-cinbistro-coastland", "Florida", "Naples"),
    ("Paragon Theaters Pavilion Naples", "Paragon Theaters Pavilion Naples", "paragon-theaters-pavilion-naples", "Florida", "Naples"),
    ("Regal Hollywood Naples", "Regal Hollywood Naples", "regal-hollywood-naples", "Florida", "Naples")
  ))
  private def p_nashville_illinois: R = ("nashville-illinois", "nashville-illinois", "Nashville", "Illinois", 38.3437, -89.3806, "America/Chicago", false, Seq("Nashville"), Seq(
    ("State Theater Nashville", "State Theater Nashville", "state-theater-nashville", "Illinois", "Nashville")
  ))
  private def p_nashville_indiana: R = ("nashville-indiana", "nashville-indiana", "Nashville", "Indiana", 39.2073, -86.2511, "America/Indiana/Indianapolis", false, Seq("Nashville"), Seq(
    ("Brown County Playhouse Nashville", "Brown County Playhouse Nashville", "brown-county-playhouse-nashville", "Indiana", "Nashville")
  ))
  private def p_national_city: R = ("national-city", "national-city-california", "National City", "California", 32.6781, -117.0992, "America/Los_Angeles", true, Seq("National City", "La Mesa", "Coronado"), Seq(
    ("AMC Plaza Bonita 14", "AMC Plaza Bonita 14", "amc-plaza-bonita-14", "California", "National City"),
    ("Reading Cinemas Grossmont La Mesa", "Reading Cinemas Grossmont La Mesa", "reading-cinemas-grossmont-la-mesa", "California", "La Mesa"),
    ("Village Theater Coronado", "Village Theater Coronado", "village-theater-coronado", "California", "Coronado")
  ))
  private def p_nebraska_city: R = ("nebraska-city", "nebraska-city-nebraska", "Nebraska City", "Nebraska", 40.6767, -95.8592, "America/Chicago", true, Seq("Nebraska City", "Auburn", "Hamburg"), Seq(
    ("ACX Pioneer 3 Theater Nebraska City", "ACX Pioneer 3 Theater Nebraska City", "acx-pioneer-3-theater-nebraska-city", "Nebraska", "Nebraska City"),
    ("Colonial Theatre Hamburg", "Colonial Theatre Hamburg", "colonial-theatre-hamburg", "Iowa", "Hamburg"),
    ("State Theater Auburn", "State Theater Auburn", "state-theater-auburn", "Nebraska", "Auburn")
  ))
  private def p_nederland: R = ("nederland", "nederland-colorado", "Nederland", "Colorado", 39.9614, -105.5108, "America/Denver", true, Seq("Nederland", "Fraser"), Seq(
    ("Foundry Cinema & Bowl Fraser", "Foundry Cinema & Bowl Fraser", "foundry-cinema-bowl-fraser", "Colorado", "Fraser"),
    ("Backdoor Theatre Nederland", "Backdoor Theatre Nederland", "backdoor-theatre-nederland", "Colorado", "Nederland")
  ))
  private def p_neligh: R = ("neligh", "neligh-nebraska", "Neligh", "Nebraska", 42.1286, -98.0298, "America/Chicago", false, Seq("Neligh"), Seq(
    ("TK-Starlite Drive-In", "TK-Starlite Drive-In", "tk-starlite-drive-in", "Nebraska", "Neligh")
  ))
  private def p_neosho: R = ("neosho", "neosho-missouri", "Neosho", "Missouri", 36.869, -94.368, "America/Chicago", true, Seq("Neosho", "Anderson"), Seq(
    ("B&B Neosho 6", "B&B Neosho 6", "bb-neosho-6", "Missouri", "Neosho"),
    ("Flick Theatre Anderson", "Flick Theatre Anderson", "flick-theatre-anderson", "Missouri", "Anderson")
  ))
  private def p_nephi: R = ("nephi", "nephi-utah", "Nephi", "Utah", 39.7102, -111.8363, "America/Denver", false, Seq("Nephi"), Seq(
    ("Salt Creek Cinemas Nephi", "Salt Creek Cinemas Nephi", "salt-creek-cinemas-nephi", "Utah", "Nephi")
  ))
  private def p_new_bern: R = ("new-bern", "new-bern-north-carolina", "New Bern", "North Carolina", 35.1085, -77.0441, "America/New_York", false, Seq("New Bern"), Seq(
    ("Bear Town Cinema New Bern", "Bear Town Cinema New Bern", "bear-town-cinema-new-bern", "North Carolina", "New Bern"),
    ("Bruin Cinema", "Bruin Cinema", "bruin-cinema", "North Carolina", "New Bern")
  ))
  private def p_new_braunfels: R = ("new-braunfels", "new-braunfels-texas", "New Braunfels", "Texas", 29.703, -98.1244, "America/Chicago", false, Seq("New Braunfels"), Seq(
    ("EVO Cinemas Creekside Town Center", "EVO Cinemas Creekside Town Center", "evo-cinemas-creekside-town-center", "Texas", "New Braunfels"),
    ("Santikos New Braunfels", "Santikos New Braunfels", "santikos-new-braunfels", "Texas", "New Braunfels"),
    ("Stars & Stripes Drive In New Braunfels 4", "Stars & Stripes Drive In New Braunfels 4", "stars-stripes-drive-in-new-braunfels-4", "Texas", "New Braunfels")
  ))
  private def p_new_brunswick: R = ("new-brunswick", "new-brunswick-new-jersey", "New Brunswick", "New Jersey", 40.4862, -74.4518, "America/New_York", true, Seq("New Brunswick", "Piscataway", "East Brunswick", "Bridgewater", "Linden", "Rahway", "South Plainfield", "Cranford", "Freehold", "Manville", "Chatham", "Bernardsville", "Mountainside", "Watchung"), Seq(
    ("AMC Brunswick Square 13", "AMC Brunswick Square 13", "amc-brunswick-square-13", "New Jersey", "East Brunswick"),
    ("AMC New Brunswick 18", "AMC New Brunswick 18", "amc-new-brunswick-18", "New Jersey", "New Brunswick"),
    ("Reading Cinemas Manville", "Reading Cinemas Manville", "reading-cinemas-manville", "New Jersey", "Manville"),
    ("Rutgers Cinema Piscataway", "Rutgers Cinema Piscataway", "rutgers-cinema-piscataway", "New Jersey", "Piscataway"),
    ("AMC Freehold 14", "AMC Freehold 14", "amc-freehold-14", "New Jersey", "Freehold"),
    ("AMC Aviation 12", "AMC Aviation 12", "amc-aviation-12-linden", "New Jersey", "Linden"),
    ("AMC DINE-IN Bridgewater 7", "AMC DINE-IN Bridgewater 7", "amc-dine-in-bridgewater-7", "New Jersey", "Bridgewater"),
    ("AMC Mountainside 10", "AMC Mountainside 10", "amc-mountainside-10", "New Jersey", "Mountainside"),
    ("Bernardsville Cinema", "Bernardsville Cinema", "bernardsville-cinema", "New Jersey", "Bernardsville"),
    ("Chatham Hickory Cinema", "Chatham Hickory Cinema", "chatham-hickory-cinema", "New Jersey", "Chatham"),
    ("Cinemark Watchung", "Cinemark Watchung", "cinemark-watchung", "New Jersey", "Watchung"),
    ("Cranford Cinema", "Cranford Cinema", "cranford-cinema", "New Jersey", "Cranford"),
    ("Regal Hadley South Plainfield", "Regal Hadley South Plainfield", "regal-hadley-south-plainfield", "New Jersey", "South Plainfield"),
    ("Union County Arts Center", "Union County Arts Center", "union-county-arts-center", "New Jersey", "Rahway")
  ))
  private def p_new_caney: R = ("new-caney", "new-caney-texas", "New Caney", "Texas", 30.1552, -95.2113, "America/Chicago", true, Seq("New Caney", "Humble", "Kingwood"), Seq(
    ("AMC Deerbrook 24", "AMC Deerbrook 24", "amc-deerbrook-24", "Texas", "Humble"),
    ("Cinemark New Caney", "Cinemark New Caney", "cinemark-new-caney", "Texas", "New Caney"),
    ("Showbiz Cinemas Kingwood", "Showbiz Cinemas Kingwood", "showbiz-cinemas-kingwood", "Texas", "Kingwood")
  ))
  private def p_new_castle: R = ("new-castle", "new-castle-pennsylvania", "New Castle", "Pennsylvania", 41.0037, -80.347, "America/New_York", true, Seq("New Castle", "Hermitage", "Grove City"), Seq(
    ("Golden Star Theaters – Shenango Valley Cinemas", "Golden Star Theaters – Shenango Valley Cinemas", "golden-star-theaters---shenango-valley-cinemas", "Pennsylvania", "Hermitage"),
    ("Westgate Cinema New Castle 9", "Westgate Cinema New Castle 9", "westgate-cinema-new-castle-9", "Pennsylvania", "New Castle"),
    ("Guthrie Theatre Grove City", "Guthrie Theatre Grove City", "guthrie-theatre-grove-city", "Pennsylvania", "Grove City")
  ))
  private def p_new_lenox: R = ("new-lenox", "new-lenox-illinois", "New Lenox", "Illinois", 41.512, -87.9656, "America/Chicago", true, Seq("New Lenox", "Country Club Hills", "Wilmington"), Seq(
    ("AMC New Lenox 14", "AMC New Lenox 14", "amc-new-lenox-14", "Illinois", "New Lenox"),
    ("Mar Theatre Wilmington", "Mar Theatre Wilmington", "mar-theatre-wilmington", "Illinois", "South Wilmington"),
    ("Marcus Country Club Hills Cinema", "Marcus Country Club Hills Cinema", "marcus-country-club-hills-cinema", "Illinois", "Country Club Hills")
  ))
  private def p_new_london: R = ("new-london", "new-london-connecticut", "New London", "Connecticut", 41.3556, -72.0995, "America/New_York", true, Seq("New London", "Waterford", "Old Saybrook", "Lisbon", "Mystic", "Westbrook Center", "Fishers Island"), Seq(
    ("Westbrook Cinemas", "Westbrook Cinemas", "westbrook-cinemas", "Connecticut", "Westbrook"),
    ("AMC CLASSIC Lisbon 12", "AMC CLASSIC Lisbon 12", "amc-classic-lisbon-12", "Connecticut", "Lisbon"),
    ("Mystic Luxury Cinemas", "Mystic Luxury Cinemas", "mystic-luxury-cinemas", "Connecticut", "Mystic"),
    ("Regal Waterford", "Regal Waterford", "regal-waterford", "Connecticut", "Waterford"),
    ("Fishers Island Community Theater", "Fishers Island Community Theater", "fishers-island-community-theater", "New York", "Fishers Island"),
    ("Garde Arts Center New London", "Garde Arts Center New London", "garde-arts-center-new-london", "Connecticut", "New London"),
    ("Katharine Hepburn Cultural Arts Center", "Katharine Hepburn Cultural Arts Center", "katharine-hepburn-cultural-arts-center", "Connecticut", "Old Saybrook")
  ))
  private def p_new_london_wisconsin: R = ("new-london-wisconsin", "new-london-wisconsin", "New London", "Wisconsin", 44.3928, -88.7398, "America/Chicago", true, Seq("New London", "Waupaca"), Seq(
    ("Rogers Cinema Waupaca", "Rogers Cinema Waupaca", "rogers-cinema-waupaca", "Wisconsin", "Waupaca"),
    ("Grand New London", "Grand New London", "grand-new-london", "Wisconsin", "New London")
  ))
  private def p_new_martinsville: R = ("new-martinsville", "new-martinsville-west-virginia", "New Martinsville", "West Virginia", 39.6445, -80.8576, "America/New_York", false, Seq("New Martinsville"), Seq(
    ("Movie House New Martinsville", "Movie House New Martinsville", "movie-house-new-martinsville", "West Virginia", "New Martinsville"),
    ("Valley Cinema New Martinsville", "Valley Cinema New Martinsville", "valley-cinema-new-martinsville", "West Virginia", "New Martinsville")
  ))
  private def p_new_paltz: R = ("new-paltz", "new-paltz-new-york", "New Paltz", "New York", 41.7476, -74.0868, "America/New_York", false, Seq("New Paltz"), Seq(
    ("New Paltz Theatre 4", "New Paltz Theatre 4", "new-paltz-theatre-4", "New York", "New Paltz"),
    ("Denizen Theatre", "Denizen Theatre", "denizen-theatre", "New York", "New Paltz")
  ))
  private def p_new_philadelphia: R = ("new-philadelphia", "new-philadelphia-ohio", "New Philadelphia", "Ohio", 40.4898, -81.4457, "America/New_York", true, Seq("New Philadelphia", "Strasburg"), Seq(
    ("Lynn Auto Drive-In Strasburg", "Lynn Auto Drive-In Strasburg", "lynn-drive-in-strasburg", "Ohio", "New Strasburg"),
    ("Quaker Cinema New Philadelphia", "Quaker Cinema New Philadelphia", "quaker-cinema-new-philadelphia", "Ohio", "New Philadelphia")
  ))
  private def p_new_ulm: R = ("new-ulm", "new-ulm-minnesota", "New Ulm", "Minnesota", 44.3125, -94.4605, "America/Chicago", true, Seq("New Ulm", "Saint James", "Madelia"), Seq(
    ("Madelia Theater", "Madelia Theater", "madelia-theater", "Minnesota", "Madelia"),
    ("Phoenix Theatres Lyric 3", "Phoenix Theatres Lyric 3", "phoenix-theatres-lyric-3", "Minnesota", "New Ulm"),
    ("Princess Theater St James", "Princess Theater St James", "princess-theater-st-james", "Minnesota", "Saint James")
  ))
  private def p_newark_delaware: R = ("newark-delaware", "newark-delaware", "Newark", "Delaware", 39.6837, -75.7497, "America/New_York", false, Seq("Newark"), Seq(
    ("Cinemark Christiana", "Cinemark Christiana", "cinemark-christiana", "Delaware", "Newark"),
    ("Main Street Movies 5", "Main Street Movies 5", "main-street-movies-5", "Delaware", "Newark"),
    ("Regal Peoples Plaza", "Regal Peoples Plaza", "regal-peoples-plaza", "Delaware", "Newark")
  ))
  private def p_newberg: R = ("newberg", "newberg-oregon", "Newberg", "Oregon", 45.3001, -122.9732, "America/Los_Angeles", false, Seq("Newberg"), Seq(
    ("99 West Drive-In Newberg", "99 West Drive-In Newberg", "99-west-drive-in-newberg", "Oregon", "Newberg"),
    ("Cameo Newberg", "Cameo Newberg", "cameo-newberg", "Oregon", "Newberg")
  ))
  private def p_newberry: R = ("newberry", "newberry-michigan", "Newberry", "Michigan", 46.355, -85.5096, "America/Detroit", false, Seq("Newberry"), Seq(
    ("Tahqua-Land Theatre – Land Newberry", "Tahqua-Land Theatre – Land Newberry", "tahqua-land-theatre---land-newberry", "Michigan", "Newberry")
  ))
  private def p_newcastle: R = ("newcastle", "newcastle-wyoming", "Newcastle", "Wyoming", 43.8547, -104.2049, "America/Denver", false, Seq("Newcastle"), Seq(
    ("Dogie Theatre Newcastle", "Dogie Theatre Newcastle", "dogie-theatre-newcastle", "Wyoming", "Newcastle")
  ))
  private def p_newport: R = ("newport", "newport-oregon", "Newport", "Oregon", 44.6368, -124.0534, "America/Los_Angeles", false, Seq("Newport"), Seq(
    ("Newport Cinemas", "Newport Cinemas", "newport-cinemas", "Oregon", "Newport"),
    ("Newport Performing Arts", "Newport Performing Arts", "newport-performing-arts", "Oregon", "Newport")
  ))
  private def p_newport_beach: R = ("newport-beach", "newport-beach-california", "Newport Beach", "California", 33.6189, -117.929, "America/Los_Angeles", false, Seq("Newport Beach"), Seq(
    ("Lido Theatre Newport Beach", "Lido Theatre Newport Beach", "lido-theatre-newport-beach", "California", "Newport Beach"),
    ("Regal Big Newport Beach", "Regal Big Newport Beach", "regal-big-newport-beach", "California", "Newport Beach"),
    ("THE LOT Fashion Island", "THE LOT Fashion Island", "the-lot-fashion-island", "California", "Newport Beach")
  ))
  private def p_newport_vermont: R = ("newport-vermont", "newport-vermont", "Newport", "Vermont", 44.9364, -72.2051, "America/New_York", false, Seq("Newport"), Seq(
    ("City Cinemas Newport", "City Cinemas Newport", "city-cinemas-newport", "Vermont", "Newport")
  ))
  private def p_newport_washington: R = ("newport-washington", "newport-washington", "Newport", "Washington", 48.1796, -117.0433, "America/Los_Angeles", false, Seq("Newport"), Seq(
    ("Roxy Theater Newport", "Roxy Theater Newport", "roxy-theater-newport", "Washington", "Newport")
  ))
  private def p_newton: R = ("newton", "newton-iowa", "Newton", "Iowa", 41.6997, -93.048, "America/Chicago", false, Seq("Newton"), Seq(
    ("Capitol II Theatre Newton", "Capitol II Theatre Newton", "capitol-ii-theatre-newton", "Iowa", "Newton"),
    ("Valle Drive-in Newton", "Valle Drive-in Newton", "valle-drive-in-newton", "Iowa", "Newton")
  ))
  private def p_newton_illinois: R = ("newton-illinois", "newton-illinois", "Newton", "Illinois", 38.9909, -88.1625, "America/Chicago", false, Seq("Newton"), Seq(
    ("Drive 'N Theatre Newton", "Drive 'N Theatre Newton", "drive-n-theatre-newton", "Illinois", "Newton")
  ))
  private def p_newton_kansas: R = ("newton-kansas", "newton-kansas", "Newton", "Kansas", 38.0467, -97.345, "America/Chicago", false, Seq("Newton"), Seq(
    ("Chisholm Trail Newton", "Chisholm Trail Newton", "chisholm-trail-newton", "Kansas", "Newton")
  ))
  private def p_niagara_falls: R = ("niagara-falls", "niagara-falls-new-york", "Niagara Falls", "New York", 43.0945, -79.0567, "America/New_York", true, Seq("Niagara Falls", "North Tonawanda", "Williamsville"), Seq(
    ("Capitol Theatre Niagra Falls", "Capitol Theatre Niagra Falls", "capitol-theatre-niagra-falls", "New York", "Niagara Falls"),
    ("Regal Transit Center Cinema", "Regal Transit Center Cinema", "regal-transit-center-cinema", "New York", "Williamsville"),
    ("Riviera Theatre North Tonawanda", "Riviera Theatre North Tonawanda", "riviera-theatre-north-tonawanda", "New York", "North Tonawanda")
  ))
  private def p_nogales: R = ("nogales", "nogales-arizona", "Nogales", "Arizona", 31.3404, -110.9343, "America/Phoenix", false, Seq("Nogales"), Seq(
    ("Oasis Cinema 9 Nogales", "Oasis Cinema 9 Nogales", "oasis-cinema-9-nogales", "Arizona", "Nogales")
  ))
  private def p_nome: R = ("nome", "nome-alaska", "Nome", "Alaska", 64.5011, -165.4064, "America/Nome", false, Seq("Nome"), Seq(
    ("Gold Coast Cinema Nome", "Gold Coast Cinema Nome", "gold-coast-cinema-nome", "Alaska", "Nome")
  ))
  private def p_norfolk: R = ("norfolk", "norfolk-nebraska", "Norfolk", "Nebraska", 42.0283, -97.417, "America/Chicago", false, Seq("Norfolk"), Seq(
    ("Norfolk 7 Theatre", "Norfolk 7 Theatre", "norfolk-7-theatre", "Nebraska", "Norfolk")
  ))
  private def p_north_attleborough_center: R = ("north-attleborough-center", "north-attleborough-center-massachusetts", "North Attleborough Center", "Massachusetts", 41.9726, -71.3247, "America/New_York", true, Seq("North Attleborough Center", "Lincoln", "North Smithfield", "Foxborough", "Bellingham"), Seq(
    ("Phoenix Theatres Patriots Place", "Phoenix Theatres Patriots Place", "showcase-patriots-place-foxboro", "Massachusetts", "Foxborough"),
    ("Regal Bellingham", "Regal Bellingham", "regal-bellingham", "Massachusetts", "Bellingham"),
    ("Rustic Tri View Drive-In", "Rustic Tri View Drive-In", "rustic-tri-view-drive-in", "Rhode Island", "North Smithfield"),
    ("Showcase North Attleboro", "Showcase North Attleboro", "showcase-north-attleboro", "Massachusetts", "North Attleboro"),
    ("Route One Cinema Pub", "Route One Cinema Pub", "route-one-cinema-pub", "Massachusetts", "North Attleboro"),
    ("CW Theaters Lincoln Mall 16", "CW Theaters Lincoln Mall 16", "cw-theaters-lincoln-mall-16", "Rhode Island", "Lincoln")
  ))
  private def p_north_conway: R = ("north-conway", "north-conway-new-hampshire", "North Conway", "New Hampshire", 44.0537, -71.1284, "America/New_York", false, Seq("North Conway"), Seq(
    ("BarnZ's North Conway Cinema", "BarnZ's North Conway Cinema", "barnzs-north-conway-cinema", "New Hampshire", "North Conway")
  ))
  private def p_north_fort_myers: R = ("north-fort-myers", "north-fort-myers-florida", "North Fort Myers", "Florida", 26.6673, -81.8801, "America/New_York", false, Seq("North Fort Myers"), Seq(
    ("AMC Merchants Crossing 16", "AMC Merchants Crossing 16", "amc-merchants-crossing-16", "Florida", "North Fort Myers")
  ))
  private def p_north_little_rock: R = ("north-little-rock", "north-little-rock-arkansas", "North Little Rock", "Arkansas", 34.7695, -92.2671, "America/Chicago", false, Seq("North Little Rock"), Seq(
    ("Regal McCain Mall Little Rock", "Regal McCain Mall Little Rock", "regal-mccain-mall-little-rock", "Arkansas", "North Little Rock")
  ))
  private def p_north_platte: R = ("north-platte", "north-platte-nebraska", "North Platte", "Nebraska", 41.1239, -100.7654, "America/Chicago", false, Seq("North Platte"), Seq(
    ("Golden Ticket Cinemas Platte River 6", "Golden Ticket Cinemas Platte River 6", "golden-ticket-cinemas-platte-river-6", "Nebraska", "North Platte"),
    ("North Platte Community Playhouse", "North Platte Community Playhouse", "north-platte-community-playhouse", "Nebraska", "North Platte")
  ))
  private def p_north_portland: R = ("north-portland", "north-portland-oregon", "North Portland", "Oregon", 45.6104, -122.7034, "America/Los_Angeles", false, Seq("North Portland"), Seq(
    ("Avalon Theatre Portland", "Avalon Theatre Portland", "avalon-theatre-portland", "Oregon", "North Portland"),
    ("Clinton Street Portland", "Clinton Street Portland", "clinton-street-portland", "Oregon", "North Portland"),
    ("Jubitz Cinema", "Jubitz Cinema", "jubitz-cinema", "Oregon", "North Portland"),
    ("Laurelhurst Theater Portland", "Laurelhurst Theater Portland", "laurelhurst-theater-portland", "Oregon", "North Portland"),
    ("McMenamins Bagdad Theater", "McMenamins Bagdad Theater", "mcmenamins-bagdad-theater", "Oregon", "North Portland"),
    ("McMenamins St. Johns Theater", "McMenamins St. Johns Theater", "mcmenamins-st-johns-theater", "Oregon", "North Portland"),
    ("Oak Grove Portland 8", "Oak Grove Portland 8", "oak-grove-portland-8", "Oregon", "North Portland"),
    ("Wunderland Milwaukie Cinemas", "Wunderland Milwaukie Cinemas", "wunderland-milwaukie-cinemas", "Oregon", "North Portland"),
    ("Mission Theater", "Mission Theater", "mission-theater", "Oregon", "North Portland")
  ))
  private def p_north_virginia_beach: R = ("north-virginia-beach", "north-virginia-beach-virginia", "North Virginia Beach", "Virginia", 36.8331, -76.1526, "America/New_York", true, Seq("North Virginia Beach", "Portsmouth"), Seq(
    ("Cinema Cafe Kemps River", "Cinema Cafe Kemps River", "cinema-cafe-kemps-river", "Virginia", "North Virginia Beach"),
    ("Cinema Cafe Pembroke Meadows", "Cinema Cafe Pembroke Meadows", "cinema-cafe-pembroke-meadows", "Virginia", "North Virginia Beach"),
    ("Commodore Theatre Portsmouth", "Commodore Theatre Portsmouth", "commodore-theatre-portsmouth", "Virginia", "Portsmouth")
  ))
  private def p_north_wilkesboro: R = ("north-wilkesboro", "north-wilkesboro-north-carolina", "North Wilkesboro", "North Carolina", 36.1585, -81.1476, "America/New_York", false, Seq("North Wilkesboro"), Seq(
    ("Liberty N Wilkesboro", "Liberty N Wilkesboro", "liberty-n-wilkesboro", "North Carolina", "North Wilkesboro"),
    ("Two Rivers Cinema 6", "Two Rivers Cinema 6", "two-rivers-cinema-6", "North Carolina", "North Wilkesboro")
  ))
  private def p_northfield: R = ("northfield", "northfield-minnesota", "Northfield", "Minnesota", 44.4583, -93.1616, "America/Chicago", true, Seq("Northfield", "Elko New Market", "Dundas"), Seq(
    ("Cannon Valley Cinema 10", "Cannon Valley Cinema 10", "cannon-valley-cinema-10", "Minnesota", "Dundas"),
    ("Elko Drive-in", "Elko Drive-in", "elko-drive-in", "Minnesota", "Elko"),
    ("Weitz Center for Creativity", "Weitz Center for Creativity", "weitz-center-for-creativity", "Minnesota", "Northfield")
  ))
  private def p_norton: R = ("norton", "norton-virginia", "Norton", "Virginia", 36.9334, -82.6291, "America/New_York", false, Seq("Norton"), Seq(
    ("Norton Cinema City Stadium 9", "Norton Cinema City Stadium 9", "norton-cinema-city-stadium-9", "Virginia", "Norton"),
    ("Central Drive-In Norton", "Central Drive-In Norton", "central-drive-in-norton", "Virginia", "Norton"),
    ("Park Avenue Theatre Norton", "Park Avenue Theatre Norton", "park-avenue-theatre-norton", "Virginia", "Norton")
  ))
  private def p_norton_kansas: R = ("norton-kansas", "norton-kansas", "Norton", "Kansas", 39.8339, -99.8915, "America/Chicago", false, Seq("Norton"), Seq(
    ("Norton Theatre", "Norton Theatre", "norton-theatre", "Kansas", "Norton")
  ))
  private def p_norwalk_connecticut: R = ("norwalk-connecticut", "norwalk-connecticut", "Norwalk", "Connecticut", 41.1176, -73.4079, "America/New_York", true, Seq("Norwalk", "Ridgefield", "Fairfield", "New Canaan", "Old Greenwich"), Seq(
    ("Prospector Theater Ridgefield", "Prospector Theater Ridgefield", "prospector-theater-ridgefield", "Connecticut", "Ridgefield"),
    ("Ridgefield Playhouse", "Ridgefield Playhouse", "ridgefield-playhouse", "Connecticut", "Ridgefield"),
    ("AMC Royale 6", "AMC Royale 6", "amc-royale-6", "Connecticut", "Norwalk"),
    ("AMC SoNo 8", "AMC SoNo 8", "amc-sono-8", "Connecticut", "Norwalk"),
    ("Monoplex Old Greenwich", "Monoplex Old Greenwich", "monoplex-old-greenwich", "Connecticut", "Old Greenwich"),
    ("Scene One Playhouse", "Scene One Playhouse", "cinema-lab-the-playhouse", "Connecticut", "New Canaan"),
    ("Sacred Heart University Community Theatre", "Sacred Heart University Community Theatre", "sacred-heart-university-community-theatre", "Connecticut", "Fairfield")
  ))
  private def p_norwood: R = ("norwood", "norwood-massachusetts", "Norwood", "Massachusetts", 42.1945, -71.1995, "America/New_York", true, Seq("Norwood", "East Bridgewater"), Seq(
    ("E Bridgewater Cinema 5", "E Bridgewater Cinema 5", "e-bridgewater-cinema-5", "Massachusetts", "East Bridgewater"),
    ("Norwood Theatre", "Norwood Theatre", "norwood-theatre", "Massachusetts", "Norwood")
  ))
  private def p_o_fallon: R = ("o-fallon", "o-fallon-missouri", "O'Fallon", "Missouri", 38.8106, -90.6998, "America/Chicago", true, Seq("O'Fallon", "Saint Peters", "Wildwood", "Wentzville", "Dardenne Prairie"), Seq(
    ("B&B Wentzville Tower 12", "B&B Wentzville Tower 12", "bb-wentzville-tower-12", "Missouri", "Wentzville"),
    ("B&B Wildwood 10", "B&B Wildwood 10", "bb-wildwood-10", "Missouri", "Wildwood"),
    ("Marcus Mid-Rivers 14", "Marcus Mid-Rivers 14", "marcus-mid-rivers-14", "Missouri", "Saint Peters"),
    ("Marcus Town Square Cinema", "Marcus Town Square Cinema", "marcus-town-square-cinema", "Missouri", "Dardenne Prairie"),
    ("Ratterman Theatres", "Ratterman Theatres", "ratterman-theatres", "Missouri", "O'Fallon")
  ))
  private def p_oak_bluffs: R = ("oak-bluffs", "oak-bluffs-massachusetts", "Oak Bluffs", "Massachusetts", 41.4543, -70.562, "America/New_York", true, Seq("Oak Bluffs", "East Falmouth", "Edgartown", "West Tisbury", "Vineyard Haven"), Seq(
    ("Entertainment Cinemas Falmouth", "Entertainment Cinemas Falmouth", "entertainment-cinemas-falmouth", "Massachusetts", "East Falmouth"),
    ("Edgartown Cinema 2", "Edgartown Cinema 2", "edgartown-cinema-2", "Massachusetts", "Edgartown"),
    ("Marthas Vineyard Film Center", "Marthas Vineyard Film Center", "marthas-vineyard-film-center", "Massachusetts", "Vineyard Haven"),
    ("Grange Hall", "Grange Hall", "grange-hall", "Massachusetts", "West Tisbury"),
    ("Strand Theatre Oak Bluffs", "Strand Theatre Oak Bluffs", "strand-theatre-oak-bluffs", "Massachusetts", "Oak Bluffs"),
    ("The Vineyard's Drive-In", "The Vineyard's Drive-In", "the-vineyards-drive-in", "Massachusetts", "Oak Bluffs")
  ))
  private def p_oak_grove: R = ("oak-grove", "oak-grove-louisiana", "Oak Grove", "Louisiana", 32.861, -91.3885, "America/Chicago", false, Seq("Oak Grove"), Seq(
    ("Fiske Theatre Oak Grove", "Fiske Theatre Oak Grove", "fiske-theatre-oak-grove", "Louisiana", "Oak Grove")
  ))
  private def p_oak_harbor: R = ("oak-harbor", "oak-harbor-washington", "Oak Harbor", "Washington", 48.2932, -122.6432, "America/Los_Angeles", false, Seq("Oak Harbor"), Seq(
    ("Blue Fox Drive-in Oak Harbor", "Blue Fox Drive-in Oak Harbor", "blue-fox-drive-in-oak-harbor", "Washington", "Oak Harbor"),
    ("Oak Harbor Cinema", "Oak Harbor Cinema", "oak-harbor-cinema", "Washington", "Oak Harbor")
  ))
  private def p_oak_ridge: R = ("oak-ridge", "oak-ridge-tennessee", "Oak Ridge", "Tennessee", 36.0104, -84.2696, "America/New_York", true, Seq("Oak Ridge", "Clinton", "Harriman", "Loudon"), Seq(
    ("Cinemark Tinseltown Oak Ridge", "Cinemark Tinseltown Oak Ridge", "cinemark-tinseltown-oak-ridge", "Tennessee", "Oak Ridge"),
    ("Ritz Theatre Clinton", "Ritz Theatre Clinton", "ritz-theatre-clinton", "Tennessee", "Clinton"),
    ("LoCo Drive-in", "LoCo Drive-in", "loco-drive-in", "Tennessee", "Loudon"),
    ("Princess Theatre - Harriman", "Princess Theatre - Harriman", "princess-theatre---harriman", "Tennessee", "Harriman")
  ))
  private def p_oakes: R = ("oakes", "oakes-north-dakota", "Oakes", "North Dakota", 46.1386, -98.0904, "America/Chicago", false, Seq("Oakes"), Seq(
    ("Grand Theatre Oakes", "Grand Theatre Oakes", "grand-theatre-oakes", "North Dakota", "Oakes")
  ))
  private def p_oakhurst: R = ("oakhurst", "oakhurst-california", "Oakhurst", "California", 37.328, -119.6493, "America/Los_Angeles", false, Seq("Oakhurst"), Seq(
    ("Yosemite Cinema", "Yosemite Cinema", "yosemite-cinema", "California", "Oakhurst")
  ))
  private def p_oakland_maryland: R = ("oakland-maryland", "oakland-maryland", "Oakland", "Maryland", 39.4079, -79.4067, "America/New_York", false, Seq("Oakland"), Seq(
    ("Garrett Cinemas Oakland 8", "Garrett Cinemas Oakland 8", "garrett-cinemas-oakland-8", "Maryland", "Oakland")
  ))
  private def p_oakley: R = ("oakley", "oakley-kansas", "Oakley", "Kansas", 39.1333, -100.8638, "America/Chicago", false, Seq("Oakley"), Seq(
    ("Palace Community Theatre Oakley", "Palace Community Theatre Oakley", "palace-community-theatre-oakley", "Kansas", "Oakley")
  ))
  private def p_oberlin: R = ("oberlin", "oberlin-kansas", "Oberlin", "Kansas", 39.8183, -100.5282, "America/Chicago", false, Seq("Oberlin"), Seq(
    ("Sunflower Cinema Oberlin", "Sunflower Cinema Oberlin", "sunflower-cinema-oberlin", "Kansas", "Oberlin")
  ))
  private def p_ocala: R = ("ocala", "ocala-florida", "Ocala", "Florida", 29.1872, -82.1401, "America/New_York", false, Seq("Ocala"), Seq(
    ("Epic Theatres Ocala", "Epic Theatres Ocala", "epic-theatres-ocala", "Florida", "Ocala"),
    ("Marion Theatre Ocala", "Marion Theatre Ocala", "marion-theatre-ocala", "Florida", "Ocala"),
    ("Ocala Drive-In", "Ocala Drive-In", "ocala-drive-in", "Florida", "Ocala"),
    ("Regal Hollywood Ocala", "Regal Hollywood Ocala", "regal-hollywood-ocala", "Florida", "Ocala")
  ))
  private def p_ocean_city: R = ("ocean-city", "ocean-city-maryland", "Ocean City", "Maryland", 38.3365, -75.0849, "America/New_York", false, Seq("Ocean City"), Seq(
    ("Flagship Premium Cinemas Ocean City 7", "Flagship Premium Cinemas Ocean City 7", "flagship-premium-cinemas-ocean-city-7", "Maryland", "Ocean City"),
    ("Gold Coast Cinema Ocean City", "Gold Coast Cinema Ocean City", "gold-coast-cinema-ocean-city", "Maryland", "Ocean City")
  ))
  private def p_ocoee: R = ("ocoee", "ocoee-florida", "Ocoee", "Florida", 28.5692, -81.544, "America/New_York", true, Seq("Ocoee", "Apopka", "Winter Garden", "Clermont", "Maitland", "Mount Dora"), Seq(
    ("AMC West Oaks 14", "AMC West Oaks 14", "amc-west-oaks-14", "Florida", "Ocoee"),
    ("West Orange Cinema", "West Orange Cinema", "west-orange-cinema", "Florida", "Ocoee"),
    ("Cinepolis Hamlin", "Cinepolis Hamlin", "cinepolis-hamlin", "Florida", "Winter Garden"),
    ("Enzian Theater", "Enzian Theater", "enzian-theater", "Florida", "Maitland"),
    ("Epic Theatres Clermont", "Epic Theatres Clermont", "epic-theatres-clermont", "Florida", "Clermont"),
    ("Epic Theatres Mount Dora", "Epic Theatres Mount Dora", "epic-theatres-mount-dora", "Florida", "Mount Dora"),
    ("Regal Wekiva Apopka", "Regal Wekiva Apopka", "regal-wekiva-apopka", "Florida", "Apopka")
  ))
  private def p_oelwein: R = ("oelwein", "oelwein-iowa", "Oelwein", "Iowa", 42.6733, -91.9135, "America/Chicago", true, Seq("Oelwein", "Sumner"), Seq(
    ("Sunset Community Theatre Sumner", "Sunset Community Theatre Sumner", "sunset-community-theatre-sumner", "Iowa", "Sumner"),
    ("The Grand Theatre Oelwein", "The Grand Theatre Oelwein", "the-grand-theatre-oelwein", "Iowa", "Oelwein")
  ))
  private def p_ogallala: R = ("ogallala", "ogallala-nebraska", "Ogallala", "Nebraska", 41.1281, -101.7196, "America/Denver", false, Seq("Ogallala"), Seq(
    ("Prairie Theatre Ogallala", "Prairie Theatre Ogallala", "prairie-theatre-ogallala", "Nebraska", "Ogallala")
  ))
  private def p_ogden: R = ("ogden", "ogden-utah", "Ogden", "Utah", 41.223, -111.9738, "America/Denver", false, Seq("Ogden"), Seq(
    ("Cinemark Tinseltown Ogden", "Cinemark Tinseltown Ogden", "cinemark-tinseltown-ogden", "Utah", "Ogden"),
    ("Megaplex The Junction", "Megaplex The Junction", "megaplex-the-junction", "Utah", "Ogden"),
    ("Peery's Egyptian Theatre Ogden", "Peery's Egyptian Theatre Ogden", "peerys-egyptian-theatre-ogden", "Utah", "Ogden")
  ))
  private def p_okemah: R = ("okemah", "okemah-oklahoma", "Okemah", "Oklahoma", 35.4326, -96.305, "America/Chicago", false, Seq("Okemah"), Seq(
    ("Crystal Theater Okemah", "Crystal Theater Okemah", "crystal-theater-okemah", "Oklahoma", "Okemah")
  ))
  private def p_old_forge: R = ("old-forge", "old-forge-new-york", "Old Forge", "New York", 43.7043, -74.9821, "America/New_York", false, Seq("Old Forge"), Seq(
    ("Strand Of Old Forge", "Strand Of Old Forge", "strand-of-old-forge", "New York", "Old Forge")
  ))
  private def p_olympia: R = ("olympia", "olympia-washington", "Olympia", "Washington", 47.0449, -122.9017, "America/Los_Angeles", true, Seq("Olympia", "Lacey"), Seq(
    ("Capitol Theatre Olympia", "Capitol Theatre Olympia", "capitol-theatre-olympia", "Washington", "Olympia"),
    ("Cinemark Century Olympia", "Cinemark Century Olympia", "cinemark-century-olympia", "Washington", "Olympia"),
    ("Regal Martin Village Lacey", "Regal Martin Village Lacey", "regal-martin-village-lacey", "Washington", "Lacey")
  ))
  private def p_omak: R = ("omak", "omak-washington", "Omak", "Washington", 48.411, -119.5276, "America/Los_Angeles", false, Seq("Omak"), Seq(
    ("Mirage Omak 3", "Mirage Omak 3", "mirage-omak-3", "Washington", "North Omak"),
    ("Omak Theatre", "Omak Theatre", "omak-theatre", "Washington", "Omak")
  ))
  private def p_onancock: R = ("onancock", "onancock-virginia", "Onancock", "Virginia", 37.7118, -75.7491, "America/New_York", false, Seq("Onancock"), Seq(
    ("Roseland Theatre Onancock", "Roseland Theatre Onancock", "roseland-theatre-onancock", "Virginia", "Onancock")
  ))
  private def p_onarga: R = ("onarga", "onarga-illinois", "Onarga", "Illinois", 40.715, -88.0062, "America/Chicago", false, Seq("Onarga"), Seq(
    ("Onarga Theatre", "Onarga Theatre", "onarga-theatre", "Illinois", "Onarga")
  ))
  private def p_onawa: R = ("onawa", "onawa-iowa", "Onawa", "Iowa", 42.0266, -96.0972, "America/Chicago", false, Seq("Onawa"), Seq(
    ("Iowa Theater Onawa", "Iowa Theater Onawa", "iowa-theater-onawa", "Iowa", "Onawa")
  ))
  private def p_oneida: R = ("oneida", "oneida-new-york", "Oneida", "New York", 43.0926, -75.6513, "America/New_York", true, Seq("Oneida", "Manlius", "Hamilton"), Seq(
    ("Hamilton Theatre", "Hamilton Theatre", "hamilton-theatre", "New York", "East Hamilton"),
    ("Manlius Cinema", "Manlius Cinema", "manlius-cinema", "New York", "Manlius"),
    ("Movieplex Cinema Oneida", "Movieplex Cinema Oneida", "movieplex-cinema-oneida", "New York", "Oneida")
  ))
  private def p_oneida_tennessee: R = ("oneida-tennessee", "oneida-tennessee", "Oneida", "Tennessee", 36.4981, -84.5127, "America/New_York", false, Seq("Oneida"), Seq(
    ("Capitol 3 Oneida", "Capitol 3 Oneida", "capitol-3-oneida", "Tennessee", "Oneida")
  ))
  private def p_oneonta: R = ("oneonta", "oneonta-new-york", "Oneonta", "New York", 42.4529, -75.0638, "America/New_York", true, Seq("Oneonta", "Norwich", "Worcester", "Unadilla"), Seq(
    ("Unadilla Drive-in", "Unadilla Drive-in", "unadilla-drive-in", "New York", "South Unadilla"),
    ("Colonia Theatre Norwich", "Colonia Theatre Norwich", "colonia-theatre-norwich", "New York", "East Norwich"),
    ("Southside Mall Cinema Oneonta", "Southside Mall Cinema Oneonta", "southside-mall-cinema-oneonta", "New York", "Oneonta"),
    ("Wieting Theater Worcester", "Wieting Theater Worcester", "wieting-theater-worcester", "New York", "Worcester")
  ))
  private def p_ontario: R = ("ontario", "ontario-oregon", "Ontario", "Oregon", 44.0266, -116.9629, "America/Boise", true, Seq("Ontario", "Parma"), Seq(
    ("Parma Motor-Vu Drive-In Theatre", "Parma Motor-Vu Drive-In Theatre", "parma-motor-vu-drive-in-theatre", "Idaho", "Parma"),
    ("Ontario Reel Theatre 8", "Ontario Reel Theatre 8", "ontario-reel-theatre-8", "Oregon", "Ontario")
  ))
  private def p_ontario_ohio: R = ("ontario-ohio", "ontario-ohio", "Ontario", "Ohio", 40.7595, -82.5902, "America/New_York", false, Seq("Ontario"), Seq(
    ("Cinemark Ontario Towne Center", "Cinemark Ontario Towne Center", "cinemark-ontario-towne-center", "Ohio", "Ontario"),
    ("Springmill Drive-in Mansfield", "Springmill Drive-in Mansfield", "springmill-drive-in-mansfield", "Ohio", "East Mansfield")
  ))
  private def p_opelika: R = ("opelika", "opelika-alabama", "Opelika", "Alabama", 32.6454, -85.3783, "America/Chicago", false, Seq("Opelika"), Seq(
    ("AMC CLASSIC Tiger 13", "AMC CLASSIC Tiger 13", "amc-classic-tiger-13", "Alabama", "Opelika")
  ))
  private def p_opelousas: R = ("opelousas", "opelousas-louisiana", "Opelousas", "Louisiana", 30.5335, -92.0815, "America/Chicago", false, Seq("Opelousas"), Seq(
    ("Acadiana Cinemas Opelousas", "Acadiana Cinemas Opelousas", "acadiana-cinemas-opelousas", "Louisiana", "Opelousas")
  ))
  private def p_orangeburg: R = ("orangeburg", "orangeburg-south-carolina", "Orangeburg", "South Carolina", 33.4918, -80.8556, "America/New_York", false, Seq("Orangeburg"), Seq(
    ("Claflin Cinema", "Claflin Cinema", "claflin-cinema", "South Carolina", "Orangeburg")
  ))
  private def p_orem: R = ("orem", "orem-utah", "Orem", "Utah", 40.2969, -111.6946, "America/Denver", true, Seq("Orem", "Lehi", "Pleasant Grove", "American Fork", "Saratoga Springs", "Vineyard"), Seq(
    ("Cinemark American Fork 12", "Cinemark American Fork 12", "cinemark-american-fork-12", "Utah", "American Fork"),
    ("Cinemark University Mall Orem", "Cinemark University Mall Orem", "cinemark-university-mall-orem", "Utah", "Orem"),
    ("SCERA Center for the Arts", "SCERA Center for the Arts", "scera-center-for-the-arts", "Utah", "Orem"),
    ("Fat Cats Saratoga Springs", "Fat Cats Saratoga Springs", "fat-cats-saratoga-springs", "Utah", "Saratoga Springs"),
    ("Megaplex Geneva", "Megaplex Geneva", "megaplex-geneva", "Utah", "Vineyard"),
    ("Megaplex Pleasant Grove", "Megaplex Pleasant Grove", "megaplex-pleasant-grove", "Utah", "Pleasant Grove"),
    ("Megaplex Thanksgiving Point", "Megaplex Thanksgiving Point", "megaplex-thanksgiving-point", "Utah", "Lehi")
  ))
  private def p_oro_valley: R = ("oro-valley", "oro-valley-arizona", "Oro Valley", "Arizona", 32.3909, -110.9665, "America/Phoenix", false, Seq("Oro Valley"), Seq(
    ("Cinemark Century Oro Valley Marketplace", "Cinemark Century Oro Valley Marketplace", "cinemark-century-oro-valley-marketplace", "Arizona", "Oro Valley")
  ))
  private def p_orofino: R = ("orofino", "orofino-idaho", "Orofino", "Idaho", 46.4793, -116.2551, "America/Los_Angeles", false, Seq("Orofino"), Seq(
    ("Rex Theatre Orofino", "Rex Theatre Orofino", "rex-theatre-orofino", "Idaho", "Orofino")
  ))
  private def p_oroville: R = ("oroville", "oroville-california", "Oroville", "California", 39.5139, -121.5578, "America/Los_Angeles", false, Seq("Oroville"), Seq(
    ("Feather River Cinemas Oroville", "Feather River Cinemas Oroville", "feather-river-cinemas-oroville", "California", "Oroville")
  ))
  private def p_oshkosh: R = ("oshkosh", "oshkosh-wisconsin", "Oshkosh", "Wisconsin", 44.0247, -88.5426, "America/Chicago", false, Seq("Oshkosh"), Seq(
    ("Marcus Oshkosh Cinema", "Marcus Oshkosh Cinema", "marcus-oshkosh-cinema", "Wisconsin", "Oshkosh"),
    ("Time Community Theater Oshkosh", "Time Community Theater Oshkosh", "time-community-theater-oshkosh", "Wisconsin", "Oshkosh")
  ))
  private def p_oskaloosa: R = ("oskaloosa", "oskaloosa-iowa", "Oskaloosa", "Iowa", 41.2964, -92.6444, "America/Chicago", true, Seq("Oskaloosa", "Pella", "Albia", "New Sharon"), Seq(
    ("Capri Theatre New Sharon", "Capri Theatre New Sharon", "capri-theatre-new-sharon", "Iowa", "New Sharon"),
    ("Pella Cinemas", "Pella Cinemas", "pella-cinemas", "Iowa", "Pella"),
    ("King Opera House Albia", "King Opera House Albia", "king-opera-house-albia", "Iowa", "Albia"),
    ("TRIO Entertainment Complex Oskaloosa", "TRIO Entertainment Complex Oskaloosa", "trio-entertainment-complex-oskaloosa", "Iowa", "Oskaloosa")
  ))
  private def p_oswego: R = ("oswego", "oswego-illinois", "Oswego", "Illinois", 41.6828, -88.3515, "America/Chicago", true, Seq("Oswego", "St. Charles", "Batavia", "Yorkville", "North Aurora", "Sandwich"), Seq(
    ("Cinemark Tinseltown USA Aurora", "Cinemark Tinseltown USA Aurora", "cinemark-tinseltown-usa-aurora", "Illinois", "North Aurora"),
    ("Classic Cinemas Charlestowne", "Classic Cinemas Charlestowne", "classic-cinemas-charlestowne", "Illinois", "Saint Charles"),
    ("Classic Cinemas Kendall", "Classic Cinemas Kendall", "classic-cinemas-kendall", "Illinois", "Oswego"),
    ("Classic Cinemas Sandwich", "Classic Cinemas Sandwich", "classic-cinemas-sandwich", "Illinois", "Sandwich"),
    ("Emagine Batavia", "Emagine Batavia", "emagine-batavia", "Illinois", "Batavia"),
    ("NCG Yorkville Cinema", "NCG Yorkville Cinema", "ncg-yorkville-cinema", "Illinois", "Yorkville")
  ))
  private def p_oswego_new_york: R = ("oswego-new-york", "oswego-new-york", "Oswego", "New York", 43.4554, -76.5105, "America/New_York", true, Seq("Oswego", "Fulton", "Wolcott"), Seq(
    ("Oswego Cinema", "Oswego Cinema", "oswego-cinema", "New York", "Oswego"),
    ("Midway Drive-in Fulton", "Midway Drive-in Fulton", "midway-drive-in-fulton", "New York", "Fulton"),
    ("Palace Theatre Wolcott", "Palace Theatre Wolcott", "palace-theatre-wolcott", "New York", "North Wolcott")
  ))
  private def p_othello: R = ("othello", "othello-washington", "Othello", "Washington", 46.826, -119.1753, "America/Los_Angeles", true, Seq("Othello", "Connell"), Seq(
    ("Sunset Connell", "Sunset Connell", "sunset-connell", "Washington", "Connell"),
    ("The Showhouse Othello", "The Showhouse Othello", "the-showhouse-othello", "Washington", "Othello")
  ))
  private def p_ottawa: R = ("ottawa", "ottawa-illinois", "Ottawa", "Illinois", 41.3456, -88.8426, "America/Chicago", true, Seq("Ottawa", "Streator", "Peru", "Earlville"), Seq(
    ("34 Drive-In Earlville", "34 Drive-In Earlville", "34-drive-in-earlville", "Illinois", "Earlville"),
    ("AMC Peru Mall 8", "AMC Peru Mall 8", "amc-peru-mall-8", "Illinois", "Peru"),
    ("VIP Roxy Ottawa", "VIP Roxy Ottawa", "vip-roxy-ottawa", "Illinois", "Ottawa"),
    ("Streator Eagle Cinema 6", "Streator Eagle Cinema 6", "streator-eagle-cinema-6", "Illinois", "Streator")
  ))
  private def p_ottawa_kansas: R = ("ottawa-kansas", "ottawa-kansas", "Ottawa", "Kansas", 38.6156, -95.2678, "America/Chicago", false, Seq("Ottawa"), Seq(
    ("Plaza Cinema Ottawa", "Plaza Cinema Ottawa", "plaza-cinema-ottawa", "Kansas", "Ottawa")
  ))
  private def p_ottumwa: R = ("ottumwa", "ottumwa-iowa", "Ottumwa", "Iowa", 41.02, -92.4113, "America/Chicago", true, Seq("Ottumwa", "Fairfield", "Bloomfield"), Seq(
    ("Fairfield Arts & Convention Center", "Fairfield Arts & Convention Center", "fairfield-arts-convention-center", "Iowa", "Fairfield"),
    ("Iowa Theatre Bloomfield", "Iowa Theatre Bloomfield", "iowa-theatre-bloomfield", "Iowa", "Bloomfield"),
    ("Ottumwa 8 Theatre", "Ottumwa 8 Theatre", "ottumwa-8-theatre", "Iowa", "Ottumwa")
  ))
  private def p_owasso: R = ("owasso", "owasso-oklahoma", "Owasso", "Oklahoma", 36.2695, -95.8547, "America/Chicago", true, Seq("Owasso", "Claremore"), Seq(
    ("AMC CLASSIC Owasso 12", "AMC CLASSIC Owasso 12", "amc-classic-owasso-12", "Oklahoma", "Owasso"),
    ("B&B Claremore 8", "B&B Claremore 8", "bb-claremore-8", "Oklahoma", "Claremore")
  ))
  private def p_owatonna: R = ("owatonna", "owatonna-minnesota", "Owatonna", "Minnesota", 44.0838, -93.226, "America/Chicago", false, Seq("Owatonna"), Seq(
    ("Northwoods Cinema 10", "Northwoods Cinema 10", "northwoods-cinema-10", "Minnesota", "Owatonna")
  ))
  private def p_owensboro: R = ("owensboro", "owensboro-kentucky", "Owensboro", "Kentucky", 37.7742, -87.1133, "America/Chicago", true, Seq("Owensboro", "Tell City", "Rockport"), Seq(
    ("Cinema 66 Tell City", "Cinema 66 Tell City", "cinema-66-tell-city", "Indiana", "Tell City"),
    ("Holiday Drive-in Rockport", "Holiday Drive-in Rockport", "holiday-drive-in-rockport", "Indiana", "Rockport"),
    ("Malco Owensboro Cinema Grill", "Malco Owensboro Cinema Grill", "malco-owensboro-cinema-grill", "Kentucky", "Owensboro")
  ))
  private def p_oxford: R = ("oxford", "oxford-mississippi", "Oxford", "Mississippi", 34.3665, -89.5192, "America/Chicago", false, Seq("Oxford"), Seq(
    ("Malco Oxford Commons 8", "Malco Oxford Commons 8", "malco-oxford-commons-8", "Mississippi", "Oxford")
  ))
  private def p_ozark: R = ("ozark", "ozark-missouri", "Ozark", "Missouri", 37.0209, -93.206, "America/Chicago", false, Seq("Ozark"), Seq(
    ("B&B Ozark/Nixa 12", "B&B Ozark/Nixa 12", "bb-ozarknixa-12", "Missouri", "Ozark")
  ))
  private def p_pace: R = ("pace", "pace-florida", "Pace", "Florida", 30.5994, -87.1611, "America/Chicago", true, Seq("Pace", "Gulf Breeze"), Seq(
    ("reeze Cinema 8", "reeze Cinema 8", "reeze-cinema-8", "Florida", "Gulf Breeze"),
    ("The Ridge Cinema 8 Pace", "The Ridge Cinema 8 Pace", "the-ridge-cinema-8-pace", "Florida", "Pace")
  ))
  private def p_paducah: R = ("paducah", "paducah-kentucky", "Paducah", "Kentucky", 37.0834, -88.6, "America/Chicago", false, Seq("Paducah"), Seq(
    ("Cinemark Paducah", "Cinemark Paducah", "cinemark-paducah", "Kentucky", "Paducah"),
    ("Maiden Alley Cinema Paducah", "Maiden Alley Cinema Paducah", "maiden-alley-cinema-paducah", "Kentucky", "Paducah")
  ))
  private def p_page: R = ("page", "page-arizona", "Page", "Arizona", 36.9147, -111.4558, "America/Phoenix", false, Seq("Page"), Seq(
    ("Mesa Theatre", "Mesa Theatre", "mesa-theatre", "Arizona", "Page")
  ))
  private def p_page_north_dakota: R = ("page-north-dakota", "page-north-dakota", "Page", "North Dakota", 47.1597, -97.5712, "America/Chicago", false, Seq("Page"), Seq(
    ("Page Theater", "Page Theater", "page-theater", "North Dakota", "Page")
  ))
  private def p_pageland: R = ("pageland", "pageland-south-carolina", "Pageland", "South Carolina", 34.7732, -80.3917, "America/New_York", false, Seq("Pageland"), Seq(
    ("The Ball Theatre", "The Ball Theatre", "the-ball-theatre", "South Carolina", "Pageland")
  ))
  private def p_pago_pago: R = ("pago-pago", "pago-pago-american-samoa", "Pago Pago", "American Samoa", -14.2781, -170.7025, "Pacific/Pago_Pago", false, Seq("Pago Pago"), Seq(
    ("Regal Nu'uuli Place Twin", "Regal Nu'uuli Place Twin", "regal-nuuuli-place-twin", "American Samoa", "Pago Pago")
  ))
  private def p_pagosa_springs: R = ("pagosa-springs", "pagosa-springs-colorado", "Pagosa Springs", "Colorado", 37.2694, -107.0098, "America/Denver", false, Seq("Pagosa Springs"), Seq(
    ("Liberty Theatre Pagosa Springs", "Liberty Theatre Pagosa Springs", "liberty-theatre-pagosa-springs", "Colorado", "Pagosa Springs")
  ))
  private def p_palestine: R = ("palestine", "palestine-texas", "Palestine", "Texas", 31.7621, -95.6308, "America/Chicago", false, Seq("Palestine"), Seq(
    ("City Lights Palestine 6", "City Lights Palestine 6", "city-lights-palestine-6", "Texas", "Palestine")
  ))
  private def p_palm_coast: R = ("palm-coast", "palm-coast-florida", "Palm Coast", "Florida", 29.585, -81.2078, "America/New_York", false, Seq("Palm Coast"), Seq(
    ("Epic Theatres Palm Coast", "Epic Theatres Palm Coast", "epic-theatres-palm-coast", "Florida", "Palm Coast")
  ))
  private def p_palm_springs: R = ("palm-springs", "palm-springs-california", "Palm Springs", "California", 33.8303, -116.5453, "America/Los_Angeles", false, Seq("Palm Springs"), Seq(
    ("Camelot Theatres Palm Springs", "Camelot Theatres Palm Springs", "camelot-theatres-palm-springs", "California", "Palm Springs"),
    ("Festival Theaters Palm Springs", "Festival Theaters Palm Springs", "festival-theaters-palm-springs", "California", "Palm Springs"),
    ("Palm Springs Plaza Theatre", "Palm Springs Plaza Theatre", "palm-springs-plaza-theatre", "California", "Palm Springs")
  ))
  private def p_pampa: R = ("pampa", "pampa-texas", "Pampa", "Texas", 35.5362, -100.9599, "America/Chicago", false, Seq("Pampa"), Seq(
    ("Cinema 4 Pampa", "Cinema 4 Pampa", "cinema-4-pampa", "Texas", "Pampa")
  ))
  private def p_panama_city: R = ("panama-city", "panama-city-florida", "Panama City", "Florida", 30.1595, -85.6598, "America/Chicago", true, Seq("Panama City", "Panama City Beach"), Seq(
    ("Regal Regency Panama City", "Regal Regency Panama City", "regal-regency-panama-city", "Florida", "Panama City"),
    ("Santikos Grand Pier Park", "Santikos Grand Pier Park", "santikos-grand-pier-park", "Florida", "Panama City Beach")
  ))
  private def p_panguitch: R = ("panguitch", "panguitch-utah", "Panguitch", "Utah", 37.8228, -112.4358, "America/Denver", false, Seq("Panguitch"), Seq(
    ("Gem Theatre Panguitch", "Gem Theatre Panguitch", "gem-theatre-panguitch", "Utah", "Panguitch")
  ))
  private def p_paonia: R = ("paonia", "paonia-colorado", "Paonia", "Colorado", 38.8683, -107.592, "America/Denver", false, Seq("Paonia"), Seq(
    ("Paradise Theatre Paonia", "Paradise Theatre Paonia", "paradise-theatre-paonia", "Colorado", "Paonia")
  ))
  private def p_paris: R = ("paris", "paris-texas", "Paris", "Texas", 33.6609, -95.5555, "America/Chicago", false, Seq("Paris"), Seq(
    ("Cinemark Paris 8", "Cinemark Paris 8", "cinemark-paris-8", "Texas", "Paris")
  ))
  private def p_paris_illinois: R = ("paris-illinois", "paris-illinois", "Paris", "Illinois", 39.6112, -87.6961, "America/Chicago", false, Seq("Paris"), Seq(
    ("Paris Theaters", "Paris Theaters", "paris-theaters", "Illinois", "Paris")
  ))
  private def p_park_falls: R = ("park-falls", "park-falls-wisconsin", "Park Falls", "Wisconsin", 45.9344, -90.4416, "America/Chicago", true, Seq("Park Falls", "Phillips"), Seq(
    ("Cinema North Phillips", "Cinema North Phillips", "cinema-north-phillips", "Wisconsin", "Phillips"),
    ("Park Theatre Park Falls", "Park Theatre Park Falls", "park-theatre-park-falls", "Wisconsin", "Park Falls")
  ))
  private def p_park_rapids: R = ("park-rapids", "park-rapids-minnesota", "Park Rapids", "Minnesota", 46.9222, -95.0586, "America/Chicago", false, Seq("Park Rapids"), Seq(
    ("Park Theatre – Park Rapids", "Park Theatre – Park Rapids", "park-theatre---park-rapids", "Minnesota", "Park Rapids")
  ))
  private def p_parker: R = ("parker", "parker-arizona", "Parker", "Arizona", 34.15, -114.2891, "America/Phoenix", false, Seq("Parker"), Seq(
    ("Blue Water Cinemas", "Blue Water Cinemas", "blue-water-cinemas", "Arizona", "Parker")
  ))
  private def p_parkersburg: R = ("parkersburg", "parkersburg-west-virginia", "Parkersburg", "West Virginia", 39.2667, -81.5615, "America/New_York", true, Seq("Parkersburg", "Vienna"), Seq(
    ("Regal Grand Central Mall", "Regal Grand Central Mall", "regal-grand-central-mall", "West Virginia", "Vienna"),
    ("Jungle Drive-In Parkersburg", "Jungle Drive-In Parkersburg", "jungle-drive-in-parkersburg", "West Virginia", "Parkersburg")
  ))
  private def p_parsons: R = ("parsons", "parsons-kansas", "Parsons", "Kansas", 37.3403, -95.2611, "America/Chicago", false, Seq("Parsons"), Seq(
    ("Parsons Theatre", "Parsons Theatre", "parsons-theatre", "Kansas", "Parsons")
  ))
  private def p_pauls_valley: R = ("pauls-valley", "pauls-valley-oklahoma", "Pauls Valley", "Oklahoma", 34.7401, -97.2223, "America/Chicago", false, Seq("Pauls Valley"), Seq(
    ("Royal Theatre Pauls Valley", "Royal Theatre Pauls Valley", "royal-theatre-pauls-valley", "Oklahoma", "Pauls Valley")
  ))
  private def p_pawhuska: R = ("pawhuska", "pawhuska-oklahoma", "Pawhuska", "Oklahoma", 36.6678, -96.3372, "America/Chicago", false, Seq("Pawhuska"), Seq(
    ("Constantine Center Pawhuska", "Constantine Center Pawhuska", "constantine-center-pawhuska", "Oklahoma", "Pawhuska")
  ))
  private def p_payson: R = ("payson", "payson-arizona", "Payson", "Arizona", 34.2309, -111.3251, "America/Phoenix", false, Seq("Payson"), Seq(
    ("Sawmill Theatres Payson", "Sawmill Theatres Payson", "sawmill-theatres-payson", "Arizona", "Payson")
  ))
  private def p_pearl_city: R = ("pearl-city", "pearl-city-hawaii", "Pearl City", "Hawaii", 21.3973, -157.9752, "Pacific/Honolulu", true, Seq("Pearl City", "Kailua", "Kaneohe", "Mililani Town", "‘Aiea"), Seq(
    ("Consolidated Theatres Mililani", "Consolidated Theatres Mililani", "consolidated-theatres-mililani", "Hawaii", "Mililani Town"),
    ("Consolidated Theatres Pearlridge", "Consolidated Theatres Pearlridge", "consolidated-theatres-pearlridge", "Hawaii", "‘Aiea"),
    ("Kailua Cinemas", "Kailua Cinemas", "kailua-cinemas", "Hawaii", "Kailua"),
    ("Regal Pearl Highlands", "Regal Pearl Highlands", "regal-pearl-highlands", "Hawaii", "Pearl City"),
    ("Regal Windward Kaneohe", "Regal Windward Kaneohe", "regal-windward-kaneohe", "Hawaii", "Kaneohe")
  ))
  private def p_pell_city: R = ("pell-city", "pell-city-alabama", "Pell City", "Alabama", 33.5862, -86.2861, "America/Chicago", false, Seq("Pell City"), Seq(
    ("Pell City Premiere LUX Cine Bowl", "Pell City Premiere LUX Cine Bowl", "pell-city-premiere-lux-cine-bowl", "Alabama", "Pell City")
  ))
  private def p_pendleton: R = ("pendleton", "pendleton-oregon", "Pendleton", "Oregon", 45.6721, -118.7886, "America/Los_Angeles", false, Seq("Pendleton"), Seq(
    ("Wildhorse Cineplex (Casino Pendleton)", "Wildhorse Cineplex (Casino Pendleton)", "wildhorse-cineplex-casino-pendleton", "Oregon", "Pendleton")
  ))
  private def p_pensacola: R = ("pensacola", "pensacola-florida", "Pensacola", "Florida", 30.4213, -87.2169, "America/Chicago", false, Seq("Pensacola"), Seq(
    ("AMC Bayou 15 Pensacola", "AMC Bayou 15 Pensacola", "amc-bayou-15-pensacola", "Florida", "Pensacola"),
    ("AMC CLASSIC Pensacola 18", "AMC CLASSIC Pensacola 18", "amc-classic-pensacola-18", "Florida", "Pensacola"),
    ("Pensacola Cinema Art", "Pensacola Cinema Art", "pensacola-cinema-art", "Florida", "Pensacola"),
    ("Pensacola IMAX", "Pensacola IMAX", "pensacola-imax", "Florida", "Pensacola")
  ))
  private def p_perham: R = ("perham", "perham-minnesota", "Perham", "Minnesota", 46.5944, -95.5725, "America/Chicago", false, Seq("Perham"), Seq(
    ("Comet Perham", "Comet Perham", "comet-perham", "Minnesota", "Perham")
  ))
  private def p_perry: R = ("perry", "perry-iowa", "Perry", "Iowa", 41.8386, -94.1072, "America/Chicago", true, Seq("Perry", "Jefferson"), Seq(
    ("Grand Theatre Perry", "Grand Theatre Perry", "grand-theatre-perry", "Iowa", "Perry"),
    ("Sierra Community Theatre Jefferson", "Sierra Community Theatre Jefferson", "sierra-community-theatre-jefferson", "Iowa", "Jefferson")
  ))
  private def p_perry_new_york: R = ("perry-new-york", "perry-new-york", "Perry", "New York", 42.7156, -78.0056, "America/New_York", true, Seq("Perry", "Warsaw"), Seq(
    ("Silverlake Drive-in Theatre Perry", "Silverlake Drive-in Theatre Perry", "silverlake-drive-in-theatre-perry", "New York", "Perry"),
    ("Spotlight of Warsaw", "Spotlight of Warsaw", "spotlight-of-warsaw", "New York", "Warsaw")
  ))
  private def p_perryton: R = ("perryton", "perryton-texas", "Perryton", "Texas", 36.4, -100.8026, "America/Chicago", false, Seq("Perryton"), Seq(
    ("Ellis Theatre Perryton", "Ellis Theatre Perryton", "ellis-theatre-perryton", "Texas", "Perryton")
  ))
  private def p_perryville: R = ("perryville", "perryville-missouri", "Perryville", "Missouri", 37.7242, -89.8612, "America/Chicago", false, Seq("Perryville"), Seq(
    ("Perry Park Center", "Perry Park Center", "perry-park-center", "Missouri", "Perryville")
  ))
  private def p_petaluma: R = ("petaluma", "petaluma-california", "Petaluma", "California", 38.2324, -122.6367, "America/Los_Angeles", true, Seq("Petaluma", "Sonoma", "Sebastopol", "Saint Helena"), Seq(
    ("Boulevard Cinema Petaluma", "Boulevard Cinema Petaluma", "boulevard-cinema-petaluma", "California", "Petaluma"),
    ("Rialto Cinemas Sebastopol", "Rialto Cinemas Sebastopol", "rialto-cinemas-sebastopol", "California", "Sebastopol"),
    ("Sebastiani Theatre Sonoma", "Sebastiani Theatre Sonoma", "sebastiani-theatre-sonoma", "California", "Sonoma"),
    ("Cameo Cinema St Helena", "Cameo Cinema St Helena", "cameo-cinema-st-helena", "California", "Saint Helena")
  ))
  private def p_petersburg: R = ("petersburg", "petersburg-alaska", "Petersburg", "Alaska", 56.8125, -132.9556, "America/Sitka", false, Seq("Petersburg"), Seq(
    ("Northern Nights Theater", "Northern Nights Theater", "northern-nights-theater", "Alaska", "Petersburg")
  ))
  private def p_petoskey: R = ("petoskey", "petoskey-michigan", "Petoskey", "Michigan", 45.3733, -84.9553, "America/Detroit", true, Seq("Petoskey", "Charlevoix", "Harbor Springs"), Seq(
    ("Charlevoix Cinema III", "Charlevoix Cinema III", "charlevoix-cinema-iii", "Michigan", "Charlevoix"),
    ("Lyric Theatre Harbor Springs", "Lyric Theatre Harbor Springs", "lyric-theatre-harbor-springs", "Michigan", "Harbor Springs"),
    ("Petoskey Cinema", "Petoskey Cinema", "petoskey-cinema", "Michigan", "Petoskey")
  ))
  private def p_pflugerville: R = ("pflugerville", "pflugerville-texas", "Pflugerville", "Texas", 30.4394, -97.62, "America/Chicago", true, Seq("Pflugerville", "Hutto"), Seq(
    ("Cinemark Stone Hill Town Center", "Cinemark Stone Hill Town Center", "cinemark-stone-hill-town-center", "Texas", "Pflugerville"),
    ("Cinemark Tinseltown Pflugerville", "Cinemark Tinseltown Pflugerville", "cinemark-tinseltown-pflugerville", "Texas", "Pflugerville"),
    ("Hooky Entertainment Hutto", "Hooky Entertainment Hutto", "hooky-entertainment-hutto", "Texas", "Hutto")
  ))
  private def p_philadelphia_mississippi: R = ("philadelphia-mississippi", "philadelphia-mississippi", "Philadelphia", "Mississippi", 32.7715, -89.1167, "America/Chicago", false, Seq("Philadelphia"), Seq(
    ("Canal Place Philadelphia 7", "Canal Place Philadelphia 7", "canal-place-philadelphia-7", "Mississippi", "Philadelphia")
  ))
  private def p_philip: R = ("philip", "philip-south-dakota", "Philip", "South Dakota", 44.0394, -101.6651, "America/Denver", false, Seq("Philip"), Seq(
    ("Gem Theatre Philip", "Gem Theatre Philip", "gem-theatre-philip", "South Dakota", "Philip")
  ))
  private def p_philipsburg: R = ("philipsburg", "philipsburg-pennsylvania", "Philipsburg", "Pennsylvania", 40.8965, -78.2206, "America/New_York", false, Seq("Philipsburg"), Seq(
    ("Rowland Theatre Philipsburg", "Rowland Theatre Philipsburg", "rowland-theatre-philipsburg", "Pennsylvania", "Philipsburg")
  ))
  private def p_piedmont: R = ("piedmont", "piedmont-missouri", "Piedmont", "Missouri", 37.1545, -90.6957, "America/Chicago", false, Seq("Piedmont"), Seq(
    ("Pine Hill Drive In Theater", "Pine Hill Drive In Theater", "pine-hill-drive-in-theater", "Missouri", "Piedmont")
  ))
  private def p_pikeville: R = ("pikeville", "pikeville-kentucky", "Pikeville", "Kentucky", 37.4793, -82.5188, "America/New_York", false, Seq("Pikeville"), Seq(
    ("Riverfill Cinema Pikeville", "Riverfill Cinema Pikeville", "riverfill-cinema-pikeville", "Kentucky", "Pikeville")
  ))
  private def p_pinecrest: R = ("pinecrest", "pinecrest-california", "Pinecrest", "California", 38.1902, -119.9988, "America/Los_Angeles", false, Seq("Pinecrest"), Seq(
    ("Pinecrest Theater", "Pinecrest Theater", "pinecrest-theater", "California", "Pinecrest")
  ))
  private def p_pittsburg: R = ("pittsburg", "pittsburg-kansas", "Pittsburg", "Kansas", 37.4109, -94.705, "America/Chicago", false, Seq("Pittsburg"), Seq(
    ("Meadowbrook 8 Theatre", "Meadowbrook 8 Theatre", "amc-classic-pittsburg-8", "Kansas", "Pittsburg")
  ))
  private def p_pittsfield: R = ("pittsfield", "pittsfield-massachusetts", "Pittsfield", "Massachusetts", 42.4501, -73.2454, "America/New_York", true, Seq("Pittsfield", "Williamstown"), Seq(
    ("Images Cinema Williamstown", "Images Cinema Williamstown", "images-cinema-williamstown", "Massachusetts", "Williamstown"),
    ("Phoenix Beacon Cinema Pittsfield", "Phoenix Beacon Cinema Pittsfield", "phoenix-beacon-cinema-pittsfield", "Massachusetts", "Pittsfield")
  ))
  private def p_plains: R = ("plains", "plains-montana", "Plains", "Montana", 47.4602, -114.8829, "America/Denver", false, Seq("Plains"), Seq(
    ("Llano Theatre Plains", "Llano Theatre Plains", "llano-theatre-plains", "Montana", "Plains")
  ))
  private def p_plantation: R = ("plantation", "plantation-florida", "Plantation", "Florida", 26.1342, -80.2318, "America/New_York", true, Seq("Plantation", "Dania Beach", "Miami Lakes"), Seq(
    ("Regal Broward Plantation", "Regal Broward Plantation", "regal-broward-plantation", "Florida", "Plantation"),
    ("Regal Dania Pointe", "Regal Dania Pointe", "regal-dania-pointe", "Florida", "Dania Beach"),
    ("CMX Miami Lakes 17", "CMX Miami Lakes 17", "cmx-miami-lakes-17", "Florida", "Miami Lakes")
  ))
  private def p_platte: R = ("platte", "platte-south-dakota", "Platte", "South Dakota", 43.3869, -98.8445, "America/Chicago", false, Seq("Platte"), Seq(
    ("Lyric Theater Platte", "Lyric Theater Platte", "lyric-theater-platte", "South Dakota", "Platte")
  ))
  private def p_platteville: R = ("platteville", "platteville-wisconsin", "Platteville", "Wisconsin", 42.7342, -90.4785, "America/Chicago", false, Seq("Platteville"), Seq(
    ("Avalon Cinema Platteville", "Avalon Cinema Platteville", "avalon-cinema-platteville", "Wisconsin", "Platteville"),
    ("Millennium Cinema Platteville", "Millennium Cinema Platteville", "millennium-cinema-platteville", "Wisconsin", "Platteville")
  ))
  private def p_plattsburgh: R = ("plattsburgh", "plattsburgh-new-york", "Plattsburgh", "New York", 44.6995, -73.4529, "America/New_York", false, Seq("Plattsburgh"), Seq(
    ("Cumberland Plattsburgh 12", "Cumberland Plattsburgh 12", "cumberland-plattsburgh-12", "New York", "Plattsburgh"),
    ("Regal Champlain Cinemas Plattsburgh", "Regal Champlain Cinemas Plattsburgh", "regal-champlain-cinemas-plattsburgh", "New York", "Plattsburgh")
  ))
  private def p_pleasanton: R = ("pleasanton", "pleasanton-texas", "Pleasanton", "Texas", 28.9672, -98.4786, "America/Chicago", false, Seq("Pleasanton"), Seq(
    ("Plestex Pleasanton 4", "Plestex Pleasanton 4", "plestex-pleasanton-4", "Texas", "Pleasanton")
  ))
  private def p_plentywood: R = ("plentywood", "plentywood-montana", "Plentywood", "Montana", 48.7747, -104.5625, "America/Denver", false, Seq("Plentywood"), Seq(
    ("Orpheum Theatre Plentywood", "Orpheum Theatre Plentywood", "orpheum-theatre-plentywood", "Montana", "Plentywood")
  ))
  private def p_plymouth: R = ("plymouth", "plymouth-minnesota", "Plymouth", "Minnesota", 45.0105, -93.4555, "America/Chicago", true, Seq("Plymouth", "Edina", "Eden Prairie", "Coon Rapids", "Saint Louis Park", "New Hope", "Columbia Heights"), Seq(
    ("AMC Coon Rapids 16", "AMC Coon Rapids 16", "amc-coon-rapids-16", "Minnesota", "Coon Rapids"),
    ("AMC Eden Prairie 18", "AMC Eden Prairie 18", "amc-eden-prairie-18", "Minnesota", "Eden Prairie"),
    ("AMC Southdale 16 Edina", "AMC Southdale 16 Edina", "amc-southdale-16-edina", "Minnesota", "Edina"),
    ("Mann Theatres Edina 4", "Mann Theatres Edina 4", "mann-theatres-edina-4", "Minnesota", "Edina"),
    ("Cinema Grill New Hope", "Cinema Grill New Hope", "cinema-grill-new-hope", "Minnesota", "New Hope"),
    ("Emagine Willow Creek", "Emagine Willow Creek", "emagine-willow-creek", "Minnesota", "Plymouth"),
    ("Mann Theatres Plymouth 15", "Mann Theatres Plymouth 15", "mann-theatres-plymouth-15", "Minnesota", "Plymouth"),
    ("Heights Theatre – Columbia Heights", "Heights Theatre – Columbia Heights", "heights-theatre---columbia-heights", "Minnesota", "Columbia Heights"),
    ("Marcus West End Cinema St Louis Park", "Marcus West End Cinema St Louis Park", "marcus-west-end-cinema-st-louis-park", "Minnesota", "Saint Louis Park")
  ))
  private def p_plymouth_indiana: R = ("plymouth-indiana", "plymouth-indiana", "Plymouth", "Indiana", 41.3437, -86.3097, "America/Indiana/Indianapolis", true, Seq("Plymouth", "Warsaw", "Bremen"), Seq(
    ("Bremen Theatre", "Bremen Theatre", "bremen-theatre", "Indiana", "Bremen"),
    ("North Pointe Cinemas Warsaw", "North Pointe Cinemas Warsaw", "north-pointe-cinemas-warsaw", "Indiana", "Warsaw"),
    ("Showland Cinema Plymouth", "Showland Cinema Plymouth", "showland-cinema-plymouth", "Indiana", "Plymouth"),
    ("Tri-way Drive-in Plymouth", "Tri-way Drive-in Plymouth", "tri-way-drive-in-plymouth", "Indiana", "Plymouth")
  ))
  private def p_plymouth_michigan: R = ("plymouth-michigan", "plymouth-michigan", "Plymouth", "Michigan", 42.3714, -83.4702, "America/Detroit", true, Seq("Plymouth", "Canton", "Novi", "Ypsilanti", "Saline"), Seq(
    ("Cinemark Ann Arbor 20", "Cinemark Ann Arbor 20", "cinemark-ann-arbor-20", "Michigan", "Ypsilanti"),
    ("Emagine Saline", "Emagine Saline", "emagine-saline", "Michigan", "Saline"),
    ("Emagine Canton Theatre", "Emagine Canton Theatre", "emagine-canton-theatre", "Michigan", "Canton"),
    ("Emagine Novi", "Emagine Novi", "emagine-novi", "Michigan", "Novi"),
    ("Penn Theatre Plymouth", "Penn Theatre Plymouth", "penn-theatre-plymouth", "Michigan", "Plymouth"),
    ("Summer Drive-in Plymouth Township", "Summer Drive-in Plymouth Township", "summer-drive-in-plymouth-township", "Michigan", "Plymouth Township")
  ))
  private def p_pocahontas: R = ("pocahontas", "pocahontas-iowa", "Pocahontas", "Iowa", 42.7355, -94.6692, "America/Chicago", false, Seq("Pocahontas"), Seq(
    ("Rialto Theatre Pocahontas", "Rialto Theatre Pocahontas", "rialto-theatre-pocahontas", "Iowa", "Pocahontas")
  ))
  private def p_pocatello: R = ("pocatello", "pocatello-idaho", "Pocatello", "Idaho", 42.8713, -112.4455, "America/Boise", true, Seq("Pocatello", "Chubbuck", "Blackfoot"), Seq(
    ("Blackfoot Movie Mill", "Blackfoot Movie Mill", "blackfoot-movie-mill", "Idaho", "Blackfoot"),
    ("AMC CLASSIC Pine Ridge 11", "AMC CLASSIC Pine Ridge 11", "amc-classic-pine-ridge-11", "Idaho", "Chubbuck"),
    ("Pocatello Reel Theatre", "Pocatello Reel Theatre", "pocatello-reel-theatre", "Idaho", "Pocatello")
  ))
  private def p_point_arena: R = ("point-arena", "point-arena-california", "Point Arena", "California", 38.9093, -123.6928, "America/Los_Angeles", false, Seq("Point Arena"), Seq(
    ("Arena Theater", "Arena Theater", "arena-theater", "California", "Point Arena")
  ))
  private def p_polson: R = ("polson", "polson-montana", "Polson", "Montana", 47.6936, -114.1632, "America/Denver", false, Seq("Polson"), Seq(
    ("Showboat Cinema 6 Polson", "Showboat Cinema 6 Polson", "showboat-cinema-6-polson", "Montana", "Polson")
  ))
  private def p_ponca_city: R = ("ponca-city", "ponca-city-oklahoma", "Ponca City", "Oklahoma", 36.707, -97.0856, "America/Chicago", false, Seq("Ponca City"), Seq(
    ("Icon Cinemas Ponca City", "Icon Cinemas Ponca City", "icon-cinemas-ponca-city", "Oklahoma", "Ponca City"),
    ("Poncan Theatre", "Poncan Theatre", "poncan-theatre", "Oklahoma", "Ponca City")
  ))
  private def p_ponderay: R = ("ponderay", "ponderay-idaho", "Ponderay", "Idaho", 48.3055, -116.5338, "America/Los_Angeles", false, Seq("Ponderay"), Seq(
    ("Sandpoint Cinema Ponderay", "Sandpoint Cinema Ponderay", "sandpoint-cinema-ponderay", "Idaho", "Ponderay")
  ))
  private def p_pontiac: R = ("pontiac", "pontiac-illinois", "Pontiac", "Illinois", 40.8809, -88.6298, "America/Chicago", false, Seq("Pontiac"), Seq(
    ("Crescent Cinemas Pontiac", "Crescent Cinemas Pontiac", "crescent-cinemas-pontiac", "Illinois", "Pontiac")
  ))
  private def p_pooler: R = ("pooler", "pooler-georgia", "Pooler", "Georgia", 32.1155, -81.2471, "America/New_York", false, Seq("Pooler"), Seq(
    ("GTC Pooler Cinemas", "GTC Pooler Cinemas", "gtc-pooler-cinemas", "Georgia", "Pooler"),
    ("Royal Cinemas Pooler", "Royal Cinemas Pooler", "royal-cinemas-pooler", "Georgia", "Pooler")
  ))
  private def p_poplar_bluff: R = ("poplar-bluff", "poplar-bluff-missouri", "Poplar Bluff", "Missouri", 36.757, -90.3929, "America/Chicago", false, Seq("Poplar Bluff"), Seq(
    ("AMC CLASSIC Poplar Bluff 8", "AMC CLASSIC Poplar Bluff 8", "amc-classic-poplar-bluff-8", "Missouri", "Poplar Bluff")
  ))
  private def p_port_angeles: R = ("port-angeles", "port-angeles-washington", "Port Angeles", "Washington", 48.1181, -123.4307, "America/Los_Angeles", false, Seq("Port Angeles"), Seq(
    ("Sun Basin Deer Park Port Angeles 5", "Sun Basin Deer Park Port Angeles 5", "sun-basin-deer-park-port-angeles-5", "Washington", "Port Angeles")
  ))
  private def p_port_arthur: R = ("port-arthur", "port-arthur-texas", "Port Arthur", "Texas", 29.8852, -93.9423, "America/Chicago", false, Seq("Port Arthur"), Seq(
    ("B&B Port Arthur Central Mall 10", "B&B Port Arthur Central Mall 10", "bb-port-arthur-central-mall-10", "Texas", "Port Arthur")
  ))
  private def p_port_charlotte: R = ("port-charlotte", "port-charlotte-florida", "Port Charlotte", "Florida", 26.9762, -82.0906, "America/New_York", false, Seq("Port Charlotte"), Seq(
    ("Regal Town Center Port Charlotte", "Regal Town Center Port Charlotte", "regal-town-center-port-charlotte", "Florida", "Port Charlotte")
  ))
  private def p_port_huron: R = ("port-huron", "port-huron-michigan", "Port Huron", "Michigan", 42.9709, -82.4249, "America/Detroit", true, Seq("Port Huron", "Marine City", "Fort Gratiot"), Seq(
    ("Riverside Cinema Marine City", "Riverside Cinema Marine City", "riverside-cinema-marine-city", "Michigan", "Marine City"),
    ("AMC Birchwood 10", "AMC Birchwood 10", "amc-birchwood-10", "Michigan", "Fort Gratiot"),
    ("GQT Krafft 8", "GQT Krafft 8", "gqt-krafft-8", "Michigan", "Port Huron"),
    ("SC4 Cinema at Sperry's", "SC4 Cinema at Sperry's", "sperrys-moviehouse---port-huron", "Michigan", "Port Huron")
  ))
  private def p_port_lavaca: R = ("port-lavaca", "port-lavaca-texas", "Port Lavaca", "Texas", 28.615, -96.6261, "America/Chicago", false, Seq("Port Lavaca"), Seq(
    ("Twin Dolphin Port Lavaca", "Twin Dolphin Port Lavaca", "twin-dolphin-port-lavaca", "Texas", "Port Lavaca")
  ))
  private def p_port_townsend: R = ("port-townsend", "port-townsend-washington", "Port Townsend", "Washington", 48.1174, -122.7607, "America/Los_Angeles", false, Seq("Port Townsend"), Seq(
    ("Rose Theatre Port Townsend", "Rose Theatre Port Townsend", "rose-theatre-port-townsend", "Washington", "Port Townsend"),
    ("Rose Theatre Starlight Room", "Rose Theatre Starlight Room", "rose-theatre-starlight-room", "Washington", "Port Townsend"),
    ("Uptown Port Townsend", "Uptown Port Townsend", "uptown-port-townsend", "Washington", "Port Townsend"),
    ("Wheel-In-Motor Movie Drive In", "Wheel-In-Motor Movie Drive In", "wheel-in-motor-movie-drive-in", "Washington", "Port Townsend")
  ))
  private def p_portage: R = ("portage", "portage-wisconsin", "Portage", "Wisconsin", 43.5391, -89.4626, "America/Chicago", true, Seq("Portage", "Prairie du Sac", "Lake Delton", "Wisconsin Dells", "Montello"), Seq(
    ("Bonham Theatre & Video", "Bonham Theatre & Video", "bonham-theatre-video", "Wisconsin", "Prairie du Sac"),
    ("AMC CLASSIC Desert Star 15", "AMC CLASSIC Desert Star 15", "amc-classic-desert-star-15", "Wisconsin", "Lake Delton"),
    ("Big Sky Drive-in Wisconsin Dells", "Big Sky Drive-in Wisconsin Dells", "big-sky-drive-in-wisconsin-dells", "Wisconsin", "Wisconsin Dells"),
    ("Montello Theatre", "Montello Theatre", "montello-theatre", "Wisconsin", "Montello"),
    ("Portage Theatre", "Portage Theatre", "portage-theatre", "Wisconsin", "Portage")
  ))
  private def p_portage_pennsylvania: R = ("portage-pennsylvania", "portage-pennsylvania", "Portage", "Pennsylvania", 40.3887, -78.6722, "America/New_York", true, Seq("Portage", "Hastings", "Duncansville"), Seq(
    ("AMC CLASSIC Altoona 12", "AMC CLASSIC Altoona 12", "amc-classic-altoona-12", "Pennsylvania", "Duncansville"),
    ("Bar-Ann Drive-In Portage", "Bar-Ann Drive-In Portage", "bar-ann-drive-in-portage", "Pennsylvania", "Portage"),
    ("Carrolltown Drive-in", "Carrolltown Drive-in", "carrolltown-drive-in", "Pennsylvania", "Carrolltown")
  ))
  private def p_porterville: R = ("porterville", "porterville-california", "Porterville", "California", 36.0652, -119.0168, "America/Los_Angeles", false, Seq("Porterville"), Seq(
    ("Galaxy Theatre Porterville", "Galaxy Theatre Porterville", "galaxy-theatre-porterville", "California", "Porterville")
  ))
  private def p_portland_texas: R = ("portland-texas", "portland-texas", "Portland", "Texas", 27.8773, -97.3239, "America/Chicago", true, Seq("Portland", "Aransas Pass"), Seq(
    ("B&B Portland Northshore 8", "B&B Portland Northshore 8", "bb-portland-northshore-8", "Texas", "Portland"),
    ("Movies Inc Aransas", "Movies Inc Aransas", "movies-inc-aransas", "Texas", "Aransas Pass")
  ))
  private def p_portsmouth: R = ("portsmouth", "portsmouth-ohio", "Portsmouth", "Ohio", 38.7317, -82.9977, "America/New_York", true, Seq("Portsmouth", "Wheelersburg"), Seq(
    ("Portsmouth Cinemas", "Portsmouth Cinemas", "portsmouth-cinemas", "Ohio", "Portsmouth"),
    ("Wheelersburg Cinema", "Wheelersburg Cinema", "wheelersburg-cinema", "Ohio", "Wheelersburg")
  ))
  private def p_portsmouth_new_hampshire: R = ("portsmouth-new-hampshire", "portsmouth-new-hampshire", "Portsmouth", "New Hampshire", 43.077, -70.7577, "America/New_York", true, Seq("Portsmouth", "Durham"), Seq(
    ("Regal Fox Run Newington", "Regal Fox Run Newington", "regal-fox-run-newington", "New Hampshire", "Newington"),
    ("Music Hall Historic Theater Portsmouth", "Music Hall Historic Theater Portsmouth", "music-hall-historic-theater-portsmouth", "New Hampshire", "Portsmouth"),
    ("MUB Movies / University of New Hampshire", "MUB Movies / University of New Hampshire", "university-of-new-hampshire", "New Hampshire", "Durham")
  ))
  private def p_poteau: R = ("poteau", "poteau-oklahoma", "Poteau", "Oklahoma", 35.0537, -94.6236, "America/Chicago", false, Seq("Poteau"), Seq(
    ("Tower Drive-in Poteau", "Tower Drive-in Poteau", "tower-drive-in-poteau", "Oklahoma", "Poteau")
  ))
  private def p_potsdam: R = ("potsdam", "potsdam-new-york", "Potsdam", "New York", 44.6698, -74.9813, "America/New_York", true, Seq("Potsdam", "Canton"), Seq(
    ("American Theatre Canton", "American Theatre Canton", "american-theatre-canton", "New York", "Canton"),
    ("Roxy Potsdam 5", "Roxy Potsdam 5", "roxy-potsdam-5", "New York", "Potsdam")
  ))
  private def p_pottstown: R = ("pottstown", "pottstown-pennsylvania", "Pottstown", "Pennsylvania", 40.2454, -75.6496, "America/New_York", true, Seq("Pottstown", "King of Prussia", "Phoenixville", "Wyomissing", "Audubon", "Souderton", "Collegeville", "Boyertown", "Pottsgrove"), Seq(
    ("AMC CLASSIC Pottsgrove 12", "AMC CLASSIC Pottsgrove 12", "amc-classic-pottsgrove-12", "Pennsylvania", "Pottsgrove"),
    ("Broad Theater Souderton", "Broad Theater Souderton", "broad-theater-souderton", "Pennsylvania", "Souderton"),
    ("Colonial Theatre Phoenixville", "Colonial Theatre Phoenixville", "colonial-theatre-phoenixville", "Pennsylvania", "Phoenixville"),
    ("Flagship Premium Cinemas Pottstown 8", "Flagship Premium Cinemas Pottstown 8", "flagship-premium-cinemas-pottstown-8", "Pennsylvania", "Pottstown"),
    ("Movie Tavern Collegeville", "Movie Tavern Collegeville", "movie-tavern-collegeville", "Pennsylvania", "Collegeville"),
    ("Oaks Center Cinema", "Oaks Center Cinema", "oaks-center-cinema", "Pennsylvania", "Oaks"),
    ("Regal King Of Prussia", "Regal King Of Prussia", "regal-king-of-prussia", "Pennsylvania", "King of Prussia"),
    ("AMC DINE-IN Berkshire 8", "AMC DINE-IN Berkshire 8", "amc-dine-in-berkshire-8", "Pennsylvania", "Wyomissing"),
    ("State Theatre of Boyertown", "State Theatre of Boyertown", "state-theatre-of-boyertown", "Pennsylvania", "Boyertown")
  ))
  private def p_poughkeepsie: R = ("poughkeepsie", "poughkeepsie-new-york", "Poughkeepsie", "New York", 41.7004, -73.921, "America/New_York", false, Seq("Poughkeepsie"), Seq(
    ("Regal Galleria Mall Poughkeepsie", "Regal Galleria Mall Poughkeepsie", "regal-galleria-mall-poughkeepsie", "New York", "Poughkeepsie"),
    ("Bardavon Opera House", "Bardavon Opera House", "bardavon-opera-house", "New York", "Poughkeepsie"),
    ("Overlook Drive-in Poughkeepsie", "Overlook Drive-in Poughkeepsie", "overlook-drive-in-poughkeepsie", "New York", "Poughkeepsie")
  ))
  private def p_powell: R = ("powell", "powell-wyoming", "Powell", "Wyoming", 44.7538, -108.7574, "America/Denver", false, Seq("Powell"), Seq(
    ("American Dream Drive-in Powell", "American Dream Drive-in Powell", "american-dream-drive-in-powell", "Wyoming", "Powell")
  ))
  private def p_prairie_du_chien: R = ("prairie-du-chien", "prairie-du-chien-wisconsin", "Prairie du Chien", "Wisconsin", 43.0517, -91.1412, "America/Chicago", false, Seq("Prairie du Chien"), Seq(
    ("Prairie Cinema Prairie Du Chien 6", "Prairie Cinema Prairie Du Chien 6", "prairie-cinema-prairie-du-chien-6", "Wisconsin", "Prairie du Chien")
  ))
  private def p_pratt: R = ("pratt", "pratt-kansas", "Pratt", "Kansas", 37.6439, -98.7376, "America/Chicago", false, Seq("Pratt"), Seq(
    ("Barron Theatre Pratt", "Barron Theatre Pratt", "barron-theatre-pratt", "Kansas", "Pratt")
  ))
  private def p_prattville: R = ("prattville", "prattville-alabama", "Prattville", "Alabama", 32.464, -86.4597, "America/Chicago", false, Seq("Prattville"), Seq(
    ("AMC CLASSIC Prattville 12", "AMC CLASSIC Prattville 12", "amc-classic-prattville-12", "Alabama", "Prattville")
  ))
  private def p_prescott_valley: R = ("prescott-valley", "prescott-valley-arizona", "Prescott Valley", "Arizona", 34.61, -112.3157, "America/Phoenix", true, Seq("Prescott Valley", "Prescott"), Seq(
    ("Harkins Prescott Valley 14", "Harkins Prescott Valley 14", "harkins-prescott-valley-14", "Arizona", "Prescott Valley"),
    ("Picture Show at Frontier Village", "Picture Show at Frontier Village", "picture-show-at-frontier-village", "Arizona", "Prescott")
  ))
  private def p_presque_isle: R = ("presque-isle", "presque-isle-maine", "Presque Isle", "Maine", 46.6812, -68.0159, "America/New_York", true, Seq("Presque Isle", "Caribou"), Seq(
    ("Braden Cinema Presque Isle", "Braden Cinema Presque Isle", "braden-cinema-presque-isle", "Maine", "Presque Isle"),
    ("Caribou Theatre", "Caribou Theatre", "caribou-theatre", "Maine", "Caribou")
  ))
  private def p_preston: R = ("preston", "preston-idaho", "Preston", "Idaho", 42.0963, -111.8766, "America/Boise", true, Seq("Preston", "Lewiston"), Seq(
    ("Community Lewiston", "Community Lewiston", "community-lewiston", "Utah", "Lewiston"),
    ("Worm Creek Opera House", "Worm Creek Opera House", "worm-creek-opera-house", "Idaho", "Preston")
  ))
  private def p_price: R = ("price", "price-utah", "Price", "Utah", 39.5994, -110.8107, "America/Denver", false, Seq("Price"), Seq(
    ("King-Koal Theaters", "King-Koal Theaters", "king-koal-theaters", "Utah", "Price"),
    ("Price Theatre", "Price Theatre", "price-theatre", "Utah", "Price")
  ))
  private def p_princeton: R = ("princeton", "princeton-new-jersey", "Princeton", "New Jersey", 40.3487, -74.659, "America/New_York", true, Seq("Princeton", "North Brunswick", "Hillsborough", "White Horse", "Hightstown", "Ten Mile Run"), Seq(
    ("AMC Marketfair Princeton 10", "AMC Marketfair Princeton 10", "amc-marketfair-princeton-10", "New Jersey", "Princeton"),
    ("Princeton Garden Theatre", "Princeton Garden Theatre", "princeton-garden-theatre", "New Jersey", "Princeton"),
    ("Hillsborough Cinemas 10", "Hillsborough Cinemas 10", "hillsborough-cinemas-10", "New Jersey", "Hillsborough"),
    ("Montgomery Cinemas Skillman", "Montgomery Cinemas Skillman", "montgomery-cinemas-skillman", "New Jersey", "Skillman"),
    ("Picture Show at East Windsor", "Picture Show at East Windsor", "picture-show-at-east-windsor", "New Jersey", "East Windsor"),
    ("Regal Commerce Center North Brunswick", "Regal Commerce Center North Brunswick", "regal-commerce-center-north-brunswick", "New Jersey", "North Brunswick"),
    ("Regal Independence Plaza Hamilton", "Regal Independence Plaza Hamilton", "regal-independence-plaza-hamilton", "New Jersey", "Hamilton")
  ))
  private def p_princeton_illinois: R = ("princeton-illinois", "princeton-illinois", "Princeton", "Illinois", 41.3681, -89.4648, "America/Chicago", false, Seq("Princeton"), Seq(
    ("Apollo Theater Princeton", "Apollo Theater Princeton", "apollo-theater-princeton", "Illinois", "Old Princeton")
  ))
  private def p_princeton_indiana: R = ("princeton-indiana", "princeton-indiana", "Princeton", "Indiana", 38.3553, -87.5675, "America/Chicago", false, Seq("Princeton"), Seq(
    ("Showbiz Cinemas Princeton 8", "Showbiz Cinemas Princeton 8", "showbiz-cinemas-princeton-8", "Indiana", "Princeton")
  ))
  private def p_prineville: R = ("prineville", "prineville-oregon", "Prineville", "Oregon", 44.2998, -120.8345, "America/Los_Angeles", false, Seq("Prineville"), Seq(
    ("Pine Theatre", "Pine Theatre", "pine-theatre", "Oregon", "Prineville")
  ))
  private def p_pryor: R = ("pryor", "pryor-oklahoma", "Pryor", "Oklahoma", 36.3084, -95.3169, "America/Chicago", false, Seq("Pryor"), Seq(
    ("Allred 5 Theater Pryor", "Allred 5 Theater Pryor", "allred-5-theater-pryor", "Oklahoma", "Pryor")
  ))
  private def p_queen_creek: R = ("queen-creek", "queen-creek-arizona", "Queen Creek", "Arizona", 33.2487, -111.6343, "America/Phoenix", false, Seq("Queen Creek"), Seq(
    ("Fat Cats Queen Creek", "Fat Cats Queen Creek", "fat-cats-queen-creek", "Arizona", "Queen Creek"),
    ("Harkins Queen Creek 14", "Harkins Queen Creek 14", "harkins-queen-creek-14", "Arizona", "Queen Creek")
  ))
  private def p_queensbury: R = ("queensbury", "queensbury-new-york", "Queensbury", "New York", 43.3773, -73.6132, "America/New_York", false, Seq("Queensbury"), Seq(
    ("Glen Drive-in Theatre Queensbury", "Glen Drive-in Theatre Queensbury", "glen-drive-in-theatre-queensbury", "New York", "Queensbury"),
    ("Regal Aviation Mall Queensbury", "Regal Aviation Mall Queensbury", "regal-aviation-mall-queensbury", "New York", "Queensbury")
  ))
  private def p_quincy: R = ("quincy", "quincy-illinois", "Quincy", "Illinois", 39.9356, -91.4099, "America/Chicago", false, Seq("Quincy"), Seq(
    ("AMC CLASSIC Quincy 6", "AMC CLASSIC Quincy 6", "amc-classic-quincy-6", "Illinois", "Quincy"),
    ("VIP Quincy 3", "VIP Quincy 3", "vip-quincy-3", "Illinois", "Quincy")
  ))
  private def p_quincy_california: R = ("quincy-california", "quincy-california", "Quincy", "California", 39.9368, -120.9465, "America/Los_Angeles", false, Seq("Quincy"), Seq(
    ("Town Hall Theatre Quincy", "Town Hall Theatre Quincy", "town-hall-theatre-quincy", "California", "Quincy")
  ))
  private def p_quinnesec: R = ("quinnesec", "quinnesec-michigan", "Quinnesec", "Michigan", 45.8064, -87.9885, "America/Menominee", false, Seq("Quinnesec"), Seq(
    ("Tri-city Quinnesec 8", "Tri-city Quinnesec 8", "tri-city-quinnesec-8", "Michigan", "Quinnesec")
  ))
  private def p_rancho_mirage: R = ("rancho-mirage", "rancho-mirage-california", "Rancho Mirage", "California", 33.7397, -116.4128, "America/Los_Angeles", true, Seq("Rancho Mirage", "Cathedral City", "La Quinta"), Seq(
    ("Cinemark Century at the River", "Cinemark Century at the River", "cinemark-century-at-the-river", "California", "Rancho Mirage"),
    ("Regal Rancho Mirage", "Regal Rancho Mirage", "regal-rancho-mirage", "California", "Rancho Mirage"),
    ("Cinemark Century La Quinta", "Cinemark Century La Quinta", "cinemark-century-la-quinta", "California", "La Quinta"),
    ("Mary Pickford Theatre is D'Place", "Mary Pickford Theatre is D'Place", "mary-pickford-theatre-is-dplace", "California", "Cathedral City")
  ))
  private def p_randolph: R = ("randolph", "randolph-vermont", "Randolph", "Vermont", 43.9251, -72.6659, "America/New_York", true, Seq("Randolph", "Bethel"), Seq(
    ("Bethel Drive-in", "Bethel Drive-in", "bethel-drive-in", "Vermont", "Bethel"),
    ("Playhouse Movie Theatre Randolph", "Playhouse Movie Theatre Randolph", "playhouse-movie-theatre-randolph", "Vermont", "Randolph")
  ))
  private def p_rangeley: R = ("rangeley", "rangeley-maine", "Rangeley", "Maine", 44.9651, -70.6424, "America/New_York", false, Seq("Rangeley"), Seq(
    ("Lakeside Theatre Rangeley", "Lakeside Theatre Rangeley", "lakeside-theatre-rangeley", "Maine", "Rangeley")
  ))
  private def p_rapid_city: R = ("rapid-city", "rapid-city-south-dakota", "Rapid City", "South Dakota", 44.0805, -103.231, "America/Denver", false, Seq("Rapid City"), Seq(
    ("AMC CLASSIC Rapid City 10", "AMC CLASSIC Rapid City 10", "amc-classic-rapid-city-10", "South Dakota", "Rapid City"),
    ("Elks Theatre Rapid City", "Elks Theatre Rapid City", "elks-theatre-rapid-city", "South Dakota", "Rapid City"),
    ("Golden Ticket Cinemas Rushmore 7", "Golden Ticket Cinemas Rushmore 7", "golden-ticket-cinemas-rushmore-7", "South Dakota", "Rapid City")
  ))
  private def p_ravenna: R = ("ravenna", "ravenna-ohio", "Ravenna", "Ohio", 41.1576, -81.2421, "America/New_York", true, Seq("Ravenna", "Kent", "Hudson", "Aurora", "Garrettsville"), Seq(
    ("Garrettesville Cinema", "Garrettesville Cinema", "garrettesville-cinema", "Ohio", "Garrettsville"),
    ("Ravenna 7 Movies", "Ravenna 7 Movies", "ravenna-7-movies", "Ohio", "Ravenna"),
    ("Midway Twin Drive-In Ravenna", "Midway Twin Drive-In Ravenna", "midway-twin-drive-in-ravenna", "Ohio", "Ravenna"),
    ("Regal Hudson", "Regal Hudson", "regal-hudson", "Ohio", "Hudson"),
    ("Tuxedo Kent Plaza (formerly MovieScoop)", "Tuxedo Kent Plaza (formerly MovieScoop)", "moviescoop-kent-plaza", "Ohio", "Kent"),
    ("Atlas Cinemas Barrington 10", "Atlas Cinemas Barrington 10", "atlas-cinemas-barrington-10", "Ohio", "Aurora")
  ))
  private def p_rawlins: R = ("rawlins", "rawlins-wyoming", "Rawlins", "Wyoming", 41.7911, -107.2387, "America/Denver", false, Seq("Rawlins"), Seq(
    ("Movies Rawlins 3", "Movies Rawlins 3", "movies-rawlins-3", "Wyoming", "Rawlins")
  ))
  private def p_reading: R = ("reading", "reading-pennsylvania", "Reading", "Pennsylvania", 40.3357, -75.9269, "America/New_York", false, Seq("Reading"), Seq(
    ("AMC Fairgrounds 10", "AMC Fairgrounds 10", "amc-fairgrounds-10", "Pennsylvania", "Reading"),
    ("GoggleWorks Center for the Arts", "GoggleWorks Center for the Arts", "goggleworks-center-for-the-arts", "Pennsylvania", "Reading"),
    ("R/C Reading Movies 11", "R/C Reading Movies 11", "rc-reading-movies-11", "Pennsylvania", "Reading")
  ))
  private def p_red_bank: R = ("red-bank", "red-bank-new-jersey", "Red Bank", "New Jersey", 40.3471, -74.0643, "America/New_York", true, Seq("Red Bank", "Asbury Park", "Eatontown", "Keyport", "Atlantic Highlands"), Seq(
    ("AMC Monmouth Mall 15", "AMC Monmouth Mall 15", "amc-monmouth-mall-15", "New Jersey", "Eatontown"),
    ("Basie Center Cinemas", "Basie Center Cinemas", "basie-center-cinemas", "New Jersey", "Red Bank"),
    ("The Vogel", "The Vogel", "the-vogel", "New Jersey", "Red Bank"),
    ("Cinemark Hazlet 12", "Cinemark Hazlet 12", "cinemark-hazlet-12", "New Jersey", "Hazlet"),
    ("Showroom Cinema Asbury Park", "Showroom Cinema Asbury Park", "showroom-cinema-asbury-park", "New Jersey", "Asbury Park"),
    ("Smodcastle Cinemas", "Smodcastle Cinemas", "smodcastle-cinemas", "New Jersey", "Atlantic Highlands")
  ))
  private def p_red_bank_tennessee: R = ("red-bank-tennessee", "red-bank-tennessee", "Red Bank", "Tennessee", 35.1123, -85.2941, "America/New_York", true, Seq("Red Bank", "Fort Oglethorpe", "Trenton"), Seq(
    ("AMC CLASSIC Northgate 14", "AMC CLASSIC Northgate 14", "amc-classic-northgate-14", "Tennessee", "Hixson"),
    ("AMC CLASSIC Battlefield 10", "AMC CLASSIC Battlefield 10", "amc-classic-battlefield-10", "Georgia", "Fort Oglethorpe"),
    ("Wilderness Outdoor Theatre Trenton", "Wilderness Outdoor Theatre Trenton", "wilderness-outdoor-theatre-trenton", "Georgia", "Trenton")
  ))
  private def p_red_bluff: R = ("red-bluff", "red-bluff-california", "Red Bluff", "California", 40.1785, -122.2358, "America/Los_Angeles", false, Seq("Red Bluff"), Seq(
    ("Spotlight Cinemas Red Bluff", "Spotlight Cinemas Red Bluff", "spotlight-cinemas-red-bluff", "California", "Red Bluff"),
    ("State Theatre Red Bluff", "State Theatre Red Bluff", "state-theatre-red-bluff", "California", "Red Bluff")
  ))
  private def p_red_lodge: R = ("red-lodge", "red-lodge-montana", "Red Lodge", "Montana", 45.1858, -109.2468, "America/Denver", false, Seq("Red Lodge"), Seq(
    ("Roman Theater Red Lodge", "Roman Theater Red Lodge", "roman-theater-red-lodge", "Montana", "Red Lodge")
  ))
  private def p_red_oak: R = ("red-oak", "red-oak-iowa", "Red Oak", "Iowa", 41.0097, -95.2255, "America/Chicago", true, Seq("Red Oak", "Shenandoah", "Carson"), Seq(
    ("Dreamland Theatre Carson", "Dreamland Theatre Carson", "dreamland-theatre-carson", "Iowa", "Carson"),
    ("Legacy Theatre Shenandoah", "Legacy Theatre Shenandoah", "legacy-theatre-shenandoah", "Iowa", "Shenandoah"),
    ("Red Oak Grand Theatre", "Red Oak Grand Theatre", "red-oak-grand-theatre", "Iowa", "Red Oak")
  ))
  private def p_red_wing: R = ("red-wing", "red-wing-minnesota", "Red Wing", "Minnesota", 44.5625, -92.5338, "America/Chicago", false, Seq("Red Wing"), Seq(
    ("Red Wing Cinema 8", "Red Wing Cinema 8", "red-wing-cinema-8", "Minnesota", "Red Wing"),
    ("Sheldon Theatre Red Wing", "Sheldon Theatre Red Wing", "sheldon-theatre-red-wing", "Minnesota", "Red Wing")
  ))
  private def p_redding: R = ("redding", "redding-california", "Redding", "California", 40.5865, -122.3917, "America/Los_Angeles", false, Seq("Redding"), Seq(
    ("Cinemark Redding 14", "Cinemark Redding 14", "cinemark-redding-14", "California", "Redding")
  ))
  private def p_redfield: R = ("redfield", "redfield-south-dakota", "Redfield", "South Dakota", 44.8758, -98.5187, "America/Chicago", false, Seq("Redfield"), Seq(
    ("Pheasant City Drive-In Redfield", "Pheasant City Drive-In Redfield", "pheasant-city-drive-in-redfield", "South Dakota", "Redfield")
  ))
  private def p_redlands: R = ("redlands", "redlands-california", "Redlands", "California", 34.0556, -117.1825, "America/Los_Angeles", true, Seq("Redlands", "Beaumont", "Big Bear Lake"), Seq(
    ("2nd Street Cinema Beaumont", "2nd Street Cinema Beaumont", "2nd-street-cinema-beaumont", "California", "Beaumont"),
    ("Harkins Mountain Grove 16", "Harkins Mountain Grove 16", "harkins-mountain-grove-16", "California", "Redlands"),
    ("Village North Big Bear Lake", "Village North Big Bear Lake", "village-north-big-bear-lake", "California", "Big Bear Lake")
  ))
  private def p_redmond: R = ("redmond", "redmond-oregon", "Redmond", "Oregon", 44.2726, -121.1739, "America/Los_Angeles", false, Seq("Redmond"), Seq(
    ("Odem Theater Pub", "Odem Theater Pub", "odem-theater-pub", "Oregon", "Redmond"),
    ("Redmond Cinemas", "Redmond Cinemas", "redmond-cinemas", "Oregon", "Redmond")
  ))
  private def p_rensselaer: R = ("rensselaer", "rensselaer-indiana", "Rensselaer", "Indiana", 40.9367, -87.1509, "America/Chicago", false, Seq("Rensselaer"), Seq(
    ("Fountain Stone Theaters Rensselaer", "Fountain Stone Theaters Rensselaer", "fountain-stone-theaters-rensselaer", "Indiana", "Rensselaer"),
    ("Ritz Cinema Rensselaer", "Ritz Cinema Rensselaer", "ritz-cinema-rensselaer", "Indiana", "Rensselaer")
  ))
  private def p_rexburg: R = ("rexburg", "rexburg-idaho", "Rexburg", "Idaho", 43.826, -111.7897, "America/Boise", false, Seq("Rexburg"), Seq(
    ("Fat Cats Rexburg", "Fat Cats Rexburg", "fat-cats-rexburg", "Idaho", "Rexburg"),
    ("Paramount 5", "Paramount 5", "paramount-5", "Idaho", "Rexburg")
  ))
  private def p_rhinelander: R = ("rhinelander", "rhinelander-wisconsin", "Rhinelander", "Wisconsin", 45.6366, -89.4121, "America/Chicago", true, Seq("Rhinelander", "Tomahawk", "Eagle River", "Three Lakes", "Woodruff"), Seq(
    ("Lakeland Cinema Woodruff", "Lakeland Cinema Woodruff", "lakeland-cinema-woodruff", "Wisconsin", "Woodruff"),
    ("Rouman Cinema Rhinelander 6", "Rouman Cinema Rhinelander 6", "rouman-cinema-rhinelander-6", "Wisconsin", "Rhinelander"),
    ("Vilas Theatre Eagle River", "Vilas Theatre Eagle River", "vilas-theatre-eagle-river", "Wisconsin", "Eagle River"),
    ("Three Lakes Center for the Arts", "Three Lakes Center for the Arts", "three-lakes-center-for-the-arts", "Wisconsin", "Three Lakes"),
    ("Tomahawk Cinema", "Tomahawk Cinema", "tomahawk-cinema", "Wisconsin", "Lake Tomahawk")
  ))
  private def p_rice_lake: R = ("rice-lake", "rice-lake-wisconsin", "Rice Lake", "Wisconsin", 45.5061, -91.7382, "America/Chicago", true, Seq("Rice Lake", "Spooner", "Chetek"), Seq(
    ("Stardust Drive In Chetek", "Stardust Drive In Chetek", "stardust-drive-in-chetek", "Wisconsin", "Chetek"),
    ("Lake Rice Lake 7", "Lake Rice Lake 7", "lake-rice-lake-7", "Wisconsin", "Rice Lake"),
    ("Palace Theatre Spooner", "Palace Theatre Spooner", "palace-theatre-spooner", "Wisconsin", "Spooner")
  ))
  private def p_richfield: R = ("richfield", "richfield-utah", "Richfield", "Utah", 38.7725, -112.0841, "America/Denver", false, Seq("Richfield"), Seq(
    ("Huish Reel Theatres", "Huish Reel Theatres", "huish-reel-theatres", "Utah", "Richfield")
  ))
  private def p_richland_center: R = ("richland-center", "richland-center-wisconsin", "Richland Center", "Wisconsin", 43.3347, -90.3868, "America/Chicago", false, Seq("Richland Center"), Seq(
    ("Center Richland Center", "Center Richland Center", "center-richland-center", "Wisconsin", "Richland Center"),
    ("Starlite 14 Drive In", "Starlite 14 Drive In", "starlite-drive-in-richland-center", "Wisconsin", "Richland Center")
  ))
  private def p_richmond_indiana: R = ("richmond-indiana", "richmond-indiana", "Richmond", "Indiana", 39.8289, -84.8902, "America/Indiana/Indianapolis", true, Seq("Richmond", "Connersville", "Greenville", "Eaton"), Seq(
    ("Star Theatre @ Eagles Point", "Star Theatre @ Eagles Point", "star-theatre-eagles-point", "Ohio", "Eaton"),
    ("AMC CLASSIC Richmond 11", "AMC CLASSIC Richmond 11", "amc-classic-richmond-11", "Indiana", "Richmond"),
    ("Showplace Cinemas Connersville 8", "Showplace Cinemas Connersville 8", "showplace-cinemas-connersville-8", "Indiana", "Connersville"),
    ("The Wayne Theatre", "The Wayne Theatre", "the-wayne-theatre", "Ohio", "Greenville")
  ))
  private def p_richmond_kentucky: R = ("richmond-kentucky", "richmond-kentucky", "Richmond", "Kentucky", 37.7479, -84.2947, "America/New_York", true, Seq("Richmond", "Nicholasville", "Winchester", "Brodhead"), Seq(
    ("Cinemark at Richmond Centre", "Cinemark at Richmond Centre", "cinemark-at-richmond-centre", "Kentucky", "Richmond"),
    ("Malco Cinema Winchester", "Malco Cinema Winchester", "malco-cinema-winchester", "Kentucky", "Winchester"),
    ("Movie Tavern Brannon Crossing Cinema", "Movie Tavern Brannon Crossing Cinema", "movie-tavern-brannon-crossing-cinema", "Kentucky", "Nicholasville"),
    ("Hog Pen Sunset Drive-In", "Hog Pen Sunset Drive-In", "hog-pen-sunset-drive-in", "Kentucky", "Brodhead")
  ))
  private def p_richmond_missouri: R = ("richmond-missouri", "richmond-missouri", "Richmond", "Missouri", 39.2786, -93.9769, "America/Chicago", true, Seq("Richmond", "Higginsville", "Lexington"), Seq(
    ("VIP Cannonball 6 Lexington", "VIP Cannonball 6 Lexington", "vip-cannonball-6-lexington", "Missouri", "Lexington"),
    ("Davis Theatre Higginsville", "Davis Theatre Higginsville", "davis-theatre-higginsville", "Missouri", "Higginsville"),
    ("Farris Theatre Richmond", "Farris Theatre Richmond", "farris-theatre-richmond", "Missouri", "Richmond")
  ))
  private def p_richmond_texas: R = ("richmond-texas", "richmond-texas", "Richmond", "Texas", 29.5822, -95.7608, "America/Chicago", true, Seq("Richmond", "Rosenberg"), Seq(
    ("Cinemark Rosenberg 12", "Cinemark Rosenberg 12", "cinemark-rosenberg-12", "Texas", "Rosenberg"),
    ("Star Cinema Grill Richmond", "Star Cinema Grill Richmond", "star-cinema-grill-richmond", "Texas", "Richmond"),
    ("Xscape at Katy Fulshear", "Xscape at Katy Fulshear", "xscape-at-katy-fulshear", "Texas", "Richmond")
  ))
  private def p_ridgecrest: R = ("ridgecrest", "ridgecrest-california", "Ridgecrest", "California", 35.6225, -117.6709, "America/Los_Angeles", false, Seq("Ridgecrest"), Seq(
    ("Ridgecrest Cinemas", "Ridgecrest Cinemas", "ridgecrest-cinemas", "California", "Ridgecrest")
  ))
  private def p_ridgeland: R = ("ridgeland", "ridgeland-mississippi", "Ridgeland", "Mississippi", 32.4285, -90.1323, "America/Chicago", true, Seq("Ridgeland", "Pearl", "Madison", "Flowood"), Seq(
    ("B&B Northpark Ridgeland 14", "B&B Northpark Ridgeland 14", "bb-northpark-ridgeland-14", "Mississippi", "Ridgeland"),
    ("Malco Renaissance Cinema Grill", "Malco Renaissance Cinema Grill", "malco-renaissance-cinema-grill", "Mississippi", "Ridgeland"),
    ("Cinemark Tinseltown Pearl", "Cinemark Tinseltown Pearl", "cinemark-tinseltown-pearl", "Mississippi", "Pearl"),
    ("Legacy Theaters Parkway", "Legacy Theaters Parkway", "legacy-theaters-parkway", "Mississippi", "Flowood"),
    ("Malco Grandview Madison 17", "Malco Grandview Madison 17", "malco-grandview-madison-17", "Mississippi", "Madison")
  ))
  private def p_rifle: R = ("rifle", "rifle-colorado", "Rifle", "Colorado", 39.5347, -107.7831, "America/Denver", false, Seq("Rifle"), Seq(
    ("Brenden Rifle 7", "Brenden Rifle 7", "brenden-rifle-7", "Colorado", "Rifle")
  ))
  private def p_rio_grande_city: R = ("rio-grande-city", "rio-grande-city-texas", "Rio Grande City", "Texas", 26.3798, -98.8203, "America/Chicago", false, Seq("Rio Grande City"), Seq(
    ("AMC CLASSIC Rio Grande 10", "AMC CLASSIC Rio Grande 10", "amc-classic-rio-grande-10", "Texas", "Rio Grande City")
  ))
  private def p_rio_rancho: R = ("rio-rancho", "rio-rancho-new-mexico", "Rio Rancho", "New Mexico", 35.2334, -106.6645, "America/Denver", false, Seq("Rio Rancho"), Seq(
    ("Rio Rancho Premiere Cinema", "Rio Rancho Premiere Cinema", "rio-rancho-premiere-cinema", "New Mexico", "Rio Rancho")
  ))
  private def p_ripley: R = ("ripley", "ripley-mississippi", "Ripley", "Mississippi", 34.7298, -88.9506, "America/Chicago", false, Seq("Ripley"), Seq(
    ("Stage on Main at Dixie Theater", "Stage on Main at Dixie Theater", "stage-on-main-at-dixie-theater", "Mississippi", "Ripley")
  ))
  private def p_ritzville: R = ("ritzville", "ritzville-washington", "Ritzville", "Washington", 47.1275, -118.38, "America/Los_Angeles", false, Seq("Ritzville"), Seq(
    ("Ritz Theatre Ritzville", "Ritz Theatre Ritzville", "ritz-theatre-ritzville", "Washington", "Ritzville")
  ))
  private def p_riverton: R = ("riverton", "riverton-wyoming", "Riverton", "Wyoming", 43.025, -108.3801, "America/Denver", false, Seq("Riverton"), Seq(
    ("Gem Theatre Riverton", "Gem Theatre Riverton", "gem-theatre-riverton", "Wyoming", "Riverton")
  ))
  private def p_riverview: R = ("riverview", "riverview-florida", "Riverview", "Florida", 27.8661, -82.3265, "America/New_York", true, Seq("Riverview", "Plant City", "Ruskin", "Gibsonton"), Seq(
    ("AMC Riverview 14 Gibsonton", "AMC Riverview 14 Gibsonton", "amc-riverview-14-gibsonton", "Florida", "Gibsonton"),
    ("Plant City Premiere Lux 8", "Plant City Premiere Lux 8", "plant-city-premiere-lux-8", "Florida", "Plant City"),
    ("Ruskin Family Drive-In Theatre", "Ruskin Family Drive-In Theatre", "ruskin-family-drive-in-theatre", "Florida", "North Ruskin"),
    ("Xscape Riverview 14", "Xscape Riverview 14", "xscape-riverview-14", "Florida", "Riverview")
  ))
  private def p_roanoke_rapids: R = ("roanoke-rapids", "roanoke-rapids-north-carolina", "Roanoke Rapids", "North Carolina", 36.4615, -77.6542, "America/New_York", false, Seq("Roanoke Rapids"), Seq(
    ("RCE Theatres Roanoke Rapids", "RCE Theatres Roanoke Rapids", "rce-theatres-roanoke-rapids", "North Carolina", "Roanoke Rapids")
  ))
  private def p_robinson: R = ("robinson", "robinson-illinois", "Robinson", "Illinois", 39.0053, -87.7392, "America/Chicago", false, Seq("Robinson"), Seq(
    ("Eagle Theater Robinson", "Eagle Theater Robinson", "eagle-theater-robinson", "Illinois", "Robinson")
  ))
  private def p_rock_hill: R = ("rock-hill", "rock-hill-south-carolina", "Rock Hill", "South Carolina", 34.9249, -81.0251, "America/New_York", true, Seq("Rock Hill", "Lancaster", "York", "Indian Land"), Seq(
    ("Crown Cinema Lancaster", "Crown Cinema Lancaster", "crown-lancaster", "South Carolina", "Lancaster"),
    ("Red Stone 14 Cinemas", "Red Stone 14 Cinemas", "red-stone-14-cinemas", "South Carolina", "Indian Land"),
    ("Regal Manchester Rock Hill", "Regal Manchester Rock Hill", "regal-manchester-rock-hill", "South Carolina", "Rock Hill"),
    ("Sylvia Theater York", "Sylvia Theater York", "sylvia-theater-york", "South Carolina", "York")
  ))
  private def p_rock_springs: R = ("rock-springs", "rock-springs-wyoming", "Rock Springs", "Wyoming", 41.5875, -109.2029, "America/Denver", false, Seq("Rock Springs"), Seq(
    ("Star Stadium 11 Rock Springs", "Star Stadium 11 Rock Springs", "star-stadium-11-rock-springs", "Wyoming", "Rock Springs")
  ))
  private def p_rockland: R = ("rockland", "rockland-maine", "Rockland", "Maine", 44.1037, -69.1089, "America/New_York", true, Seq("Rockland", "Belfast", "Thomaston", "Waldoboro", "Stonington"), Seq(
    ("Colonial Theatre Belfast", "Colonial Theatre Belfast", "colonial-theatre-belfast", "Maine", "Belfast"),
    ("Flagship Cinemas Thomaston", "Flagship Cinemas Thomaston", "flagship-cinemas-thomaston", "Maine", "Thomaston"),
    ("The Strand Theatre Rockland", "The Strand Theatre Rockland", "the-strand-theatre-rockland", "Maine", "Rockland"),
    ("Opera House Arts Stonington", "Opera House Arts Stonington", "stonington-opera-house", "Maine", "Stonington"),
    ("Waldo Theatre", "Waldo Theatre", "waldo-theatre", "Maine", "Waldoboro")
  ))
  private def p_rocky_mount: R = ("rocky-mount", "rocky-mount-north-carolina", "Rocky Mount", "North Carolina", 35.9382, -77.7905, "America/New_York", true, Seq("Rocky Mount", "Wilson", "Tarboro"), Seq(
    ("Parkhill Tarboro 3", "Parkhill Tarboro 3", "parkhill-tarboro-3", "North Carolina", "Tarboro"),
    ("UEC Theatres Rocky Mount", "UEC Theatres Rocky Mount", "uec-theatres-rocky-mount", "North Carolina", "Rocky Mount"),
    ("AMC CLASSIC Wilson 10", "AMC CLASSIC Wilson 10", "amc-classic-wilson-10", "North Carolina", "Wilson")
  ))
  private def p_rogers: R = ("rogers", "rogers-arkansas", "Rogers", "Arkansas", 36.332, -94.1185, "America/Chicago", true, Seq("Rogers", "Bentonville"), Seq(
    ("Malco Pinnacle Hills Rogers 12", "Malco Pinnacle Hills Rogers 12", "malco-pinnacle-hills-rogers-12", "Arkansas", "Rogers"),
    ("Malco Rogers Towne Cinema", "Malco Rogers Towne Cinema", "malco-rogers-towne-cinema", "Arkansas", "Rogers"),
    ("Skylight Cinema Bentonville", "Skylight Cinema Bentonville", "skylight-cinema-bentonville", "Arkansas", "Bentonville")
  ))
  private def p_rogers_city: R = ("rogers-city", "rogers-city-michigan", "Rogers City", "Michigan", 45.4214, -83.8183, "America/Detroit", false, Seq("Rogers City"), Seq(
    ("Rogers Theatre", "Rogers Theatre", "rogers-theatre", "Michigan", "Rogers City")
  ))
  private def p_rolla: R = ("rolla", "rolla-missouri", "Rolla", "Missouri", 37.9514, -91.7713, "America/Chicago", false, Seq("Rolla"), Seq(
    ("Regal Forum Rolla", "Regal Forum Rolla", "regal-forum-rolla", "Missouri", "Rolla")
  ))
  private def p_rolla_north_dakota: R = ("rolla-north-dakota", "rolla-north-dakota", "Rolla", "North Dakota", 48.8578, -99.6179, "America/Chicago", false, Seq("Rolla"), Seq(
    ("Curt's Theater", "Curt's Theater", "curts-theater", "North Dakota", "Rolla")
  ))
  private def p_rome: R = ("rome", "rome-new-york", "Rome", "New York", 43.2129, -75.4557, "America/New_York", false, Seq("Rome"), Seq(
    ("Rome Cinema", "Rome Cinema", "rome-cinema", "New York", "Rome"),
    ("Capitol Arts Complex Rome", "Capitol Arts Complex Rome", "capitol-arts-complex-rome", "New York", "Rome")
  ))
  private def p_rome_georgia: R = ("rome-georgia", "rome-georgia", "Rome", "Georgia", 34.257, -85.1647, "America/New_York", true, Seq("Rome", "Cedartown"), Seq(
    ("West Cinema Cedartown", "West Cinema Cedartown", "west-cinema-cedartown", "Georgia", "Cedartown"),
    ("GTC Berry Square Rome", "GTC Berry Square Rome", "gtc-berry-square-rome", "Georgia", "Rome")
  ))
  private def p_roosevelt: R = ("roosevelt", "roosevelt-utah", "Roosevelt", "Utah", 40.2994, -109.9888, "America/Denver", false, Seq("Roosevelt"), Seq(
    ("Roosevelt Twin Theatre", "Roosevelt Twin Theatre", "roosevelt-twin-theatre", "Utah", "Roosevelt"),
    ("Uinta Theatre Roosevelt", "Uinta Theatre Roosevelt", "uinta-theatre-roosevelt", "Utah", "Roosevelt"),
    ("Echo Drive-in Roosevelt", "Echo Drive-in Roosevelt", "echo-drive-in-roosevelt", "Utah", "Roosevelt")
  ))
  private def p_roseau: R = ("roseau", "roseau-minnesota", "Roseau", "Minnesota", 48.8461, -95.7628, "America/Chicago", false, Seq("Roseau"), Seq(
    ("Roso Theater Roseau", "Roso Theater Roseau", "roso-theater-roseau", "Minnesota", "Roseau")
  ))
  private def p_roseburg: R = ("roseburg", "roseburg-oregon", "Roseburg", "Oregon", 43.2165, -123.3417, "America/Los_Angeles", false, Seq("Roseburg"), Seq(
    ("Roseburg Cinema", "Roseburg Cinema", "roseburg-cinema", "Oregon", "Roseburg")
  ))
  private def p_roswell: R = ("roswell", "roswell-new-mexico", "Roswell", "New Mexico", 33.3944, -104.5249, "America/Denver", false, Seq("Roswell"), Seq(
    ("Apex Cinema Roswell", "Apex Cinema Roswell", "apex-cinema-roswell", "New Mexico", "Roswell"),
    ("Galaxy Roswell 8", "Galaxy Roswell 8", "galaxy-roswell-8", "New Mexico", "Roswell")
  ))
  private def p_roswell_georgia: R = ("roswell-georgia", "roswell-georgia", "Roswell", "Georgia", 34.0232, -84.3616, "America/New_York", true, Seq("Roswell", "Peachtree Corners", "Woodstock"), Seq(
    ("Area 51 – Aurora Cineplex", "Area 51 – Aurora Cineplex", "area-51---aurora-cineplex", "Georgia", "Roswell"),
    ("Movie Tavern Roswell", "Movie Tavern Roswell", "movie-tavern-roswell", "Georgia", "Roswell"),
    ("NCG Peachtree Corners", "NCG Peachtree Corners", "ncg-peachtree-corners", "Georgia", "Peachtree Corners"),
    ("CMX CinéBistro Peachtree Corners", "CMX CinéBistro Peachtree Corners", "cmx-cinbistro-peachtree-corners", "Georgia", "Peachtree Corners"),
    ("Regal Cherokee Woodstock", "Regal Cherokee Woodstock", "regal-cherokee-woodstock", "Georgia", "Woodstock")
  ))
  private def p_rothschild: R = ("rothschild", "rothschild-wisconsin", "Rothschild", "Wisconsin", 44.8872, -89.6201, "America/Chicago", false, Seq("Rothschild"), Seq(
    ("Marcus Cedar Creek Mosinee", "Marcus Cedar Creek Mosinee", "marcus-cedar-creek-mosinee", "Wisconsin", "Rothschild")
  ))
  private def p_roxboro: R = ("roxboro", "roxboro-north-carolina", "Roxboro", "North Carolina", 36.3937, -78.9828, "America/New_York", false, Seq("Roxboro"), Seq(
    ("RCE Theaters Roxboro", "RCE Theaters Roxboro", "rce-theaters-roxboro", "North Carolina", "Roxboro")
  ))
  private def p_rugby: R = ("rugby", "rugby-north-dakota", "Rugby", "North Dakota", 48.3689, -99.9963, "America/Chicago", false, Seq("Rugby"), Seq(
    ("Lyric Cinema Rugby", "Lyric Cinema Rugby", "lyric-cinema-rugby", "North Dakota", "Rugby")
  ))
  private def p_ruidoso: R = ("ruidoso", "ruidoso-new-mexico", "Ruidoso", "New Mexico", 33.3317, -105.673, "America/Denver", false, Seq("Ruidoso"), Seq(
    ("Allen Sierra Ruidoso 3", "Allen Sierra Ruidoso 3", "allen-sierra-ruidoso-3", "New Mexico", "Ruidoso")
  ))
  private def p_rushville: R = ("rushville", "rushville-illinois", "Rushville", "Illinois", 40.1212, -90.5632, "America/Chicago", false, Seq("Rushville"), Seq(
    ("Princess Theater Rushville", "Princess Theater Rushville", "princess-theater-rushville", "Illinois", "Rushville")
  ))
  private def p_russell: R = ("russell", "russell-kansas", "Russell", "Kansas", 38.8953, -98.861, "America/Chicago", false, Seq("Russell"), Seq(
    ("Dream Theater Russell", "Dream Theater Russell", "dream-theater-russell", "Kansas", "Russell")
  ))
  private def p_russellville: R = ("russellville", "russellville-arkansas", "Russellville", "Arkansas", 35.2784, -93.1338, "America/Chicago", false, Seq("Russellville"), Seq(
    ("UEC Theatres Russellville", "UEC Theatres Russellville", "uec-theatres-russellville", "Arkansas", "Russellville")
  ))
  private def p_ruston: R = ("ruston", "ruston-louisiana", "Ruston", "Louisiana", 32.5232, -92.6379, "America/Chicago", false, Seq("Ruston"), Seq(
    ("Celebrity Ruston 8", "Celebrity Ruston 8", "celebrity-ruston-8", "Louisiana", "Ruston")
  ))
  private def p_saco: R = ("saco", "saco-maine", "Saco", "Maine", 43.5009, -70.4428, "America/New_York", false, Seq("Saco"), Seq(
    ("Apple Cinemas Saco", "Apple Cinemas Saco", "apple-cinemas-saco", "Maine", "Saco"),
    ("Saco Drive-in Theatre", "Saco Drive-in Theatre", "saco-drive-in-theatre", "Maine", "Saco")
  ))
  private def p_safford: R = ("safford", "safford-arizona", "Safford", "Arizona", 32.834, -109.7076, "America/Phoenix", false, Seq("Safford"), Seq(
    ("Allen Theatres Stargazer Safford", "Allen Theatres Stargazer Safford", "allen-theatres-stargazer-safford", "Arizona", "Safford"),
    ("Victory Fellowship Theatre", "Victory Fellowship Theatre", "victory-fellowship-theatre", "Arizona", "Safford")
  ))
  private def p_saginaw: R = ("saginaw", "saginaw-michigan", "Saginaw", "Michigan", 43.4195, -83.9508, "America/Detroit", false, Seq("Saginaw"), Seq(
    ("Quality 10 Saginaw", "Quality 10 Saginaw", "quality-10-saginaw", "Michigan", "Saginaw"),
    ("Court Street Theatre Saginaw", "Court Street Theatre Saginaw", "court-street-theatre-saginaw", "Michigan", "Saginaw")
  ))
  private def p_saguache: R = ("saguache", "saguache-colorado", "Saguache", "Colorado", 38.0875, -106.142, "America/Denver", false, Seq("Saguache"), Seq(
    ("Cozy Castle Cinema Saguache", "Cozy Castle Cinema Saguache", "cozy-castle-cinema-saguache", "Colorado", "Saguache")
  ))
  private def p_saint_charles: R = ("saint-charles", "saint-charles-missouri", "Saint Charles", "Missouri", 38.7839, -90.4812, "America/Chicago", false, Seq("Saint Charles"), Seq(
    ("AMC Streets of St. Charles 8", "AMC Streets of St. Charles 8", "amc-streets-of-st-charles-8", "Missouri", "Saint Charles"),
    ("Marcus St. Charles Stadium", "Marcus St. Charles Stadium", "marcus-st-charles-stadium", "Missouri", "Saint Charles"),
    ("St. Andrews Cinema", "St. Andrews Cinema", "st-andrews-cinema", "Missouri", "Saint Charles")
  ))
  private def p_saint_clairsville: R = ("saint-clairsville", "saint-clairsville-ohio", "Saint Clairsville", "Ohio", 40.0806, -80.9001, "America/New_York", true, Seq("Saint Clairsville", "Triadelphia"), Seq(
    ("Marquee Cinemas Highlands 14", "Marquee Cinemas Highlands 14", "marquee-cinemas-highlands-14", "West Virginia", "Triadelphia"),
    ("Golden Ticket St. Clairsville", "Golden Ticket St. Clairsville", "golden-ticket-st-clairsville", "Ohio", "St. Clairsville")
  ))
  private def p_saint_croix: R = ("saint-croix", "saint-croix-virgin-islands", "Saint Croix", "Virgin Islands", 17.7275, -64.747, "America/St_Thomas", false, Seq("Saint Croix"), Seq(
    ("Caribbean Cinemas Sunny Isle", "Caribbean Cinemas Sunny Isle", "caribbean-cinemas-sunny-isle", "Virgin Islands", "St Croix")
  ))
  private def p_saint_croix_falls: R = ("saint-croix-falls", "saint-croix-falls-wisconsin", "Saint Croix Falls", "Wisconsin", 45.41, -92.6397, "America/Chicago", false, Seq("Saint Croix Falls"), Seq(
    ("St Croix Falls Cinema 8", "St Croix Falls Cinema 8", "st-croix-falls-cinema-8", "Wisconsin", "Saint Croix Falls")
  ))
  private def p_saint_francis: R = ("saint-francis", "saint-francis-kansas", "Saint Francis", "Kansas", 39.7722, -101.7999, "America/Chicago", false, Seq("Saint Francis"), Seq(
    ("Cheyenne Theatre St Francis", "Cheyenne Theatre St Francis", "cheyenne-theatre-st-francis", "Kansas", "Saint Francis")
  ))
  private def p_saint_george: R = ("saint-george", "saint-george-utah", "Saint George", "Utah", 37.1041, -113.5841, "America/Denver", false, Seq("Saint George"), Seq(
    ("Megaplex Pineview St. George", "Megaplex Pineview St. George", "megaplex-pineview-st-george", "Utah", "Saint George"),
    ("Megaplex Sunset St. George", "Megaplex Sunset St. George", "megaplex-sunset-st-george", "Utah", "Saint George")
  ))
  private def p_saint_helens: R = ("saint-helens", "saint-helens-oregon", "Saint Helens", "Oregon", 45.864, -122.8065, "America/Los_Angeles", true, Seq("Saint Helens", "Scappoose"), Seq(
    ("Columbia St Helens", "Columbia St Helens", "columbia-st-helens", "Oregon", "Saint Helens"),
    ("Scappoose Cinema 7", "Scappoose Cinema 7", "scappoose-cinema-7", "Oregon", "Scappoose")
  ))
  private def p_saint_johns: R = ("saint-johns", "saint-johns-arizona", "Saint Johns", "Arizona", 34.5059, -109.3609, "America/Phoenix", false, Seq("Saint Johns"), Seq(
    ("Hinkson Memorial Hometown Theater", "Hinkson Memorial Hometown Theater", "hinkson-memorial-hometown-theater", "Arizona", "Saint Johns")
  ))
  private def p_saint_joseph: R = ("saint-joseph", "saint-joseph-missouri", "Saint Joseph", "Missouri", 39.7686, -94.8466, "America/Chicago", false, Seq("Saint Joseph"), Seq(
    ("Regal Hollywood St. Joseph", "Regal Hollywood St. Joseph", "regal-hollywood-st-joseph", "Missouri", "Saint Joseph"),
    ("Trail Theatre Saint Joseph", "Trail Theatre Saint Joseph", "trail-theatre-saint-joseph", "Missouri", "Saint Joseph")
  ))
  private def p_saint_marys: R = ("saint-marys", "saint-marys-pennsylvania", "Saint Marys", "Pennsylvania", 41.4278, -78.5609, "America/New_York", true, Seq("Saint Marys", "DuBois", "Punxsutawney", "Brookville"), Seq(
    ("Apollo Theatre Saint Marys", "Apollo Theatre Saint Marys", "apollo-theatre-saint-marys", "Pennsylvania", "Saint Marys"),
    ("Golden Ticket Cinemas DuBois 5", "Golden Ticket Cinemas DuBois 5", "golden-ticket-cinemas-dubois-5", "Pennsylvania", "DuBois"),
    ("Jackson Theatre Punxsutawney", "Jackson Theatre Punxsutawney", "jackson-theatre-punxsutawney", "Pennsylvania", "Punxsutawney"),
    ("Moonlite Drive-In Brookville", "Moonlite Drive-In Brookville", "moonlite-drive-in-brookville", "Pennsylvania", "Brookville")
  ))
  private def p_saint_michael: R = ("saint-michael", "saint-michael-minnesota", "Saint Michael", "Minnesota", 45.21, -93.665, "America/Chicago", true, Seq("Saint Michael", "Monticello", "Rogers"), Seq(
    ("Emagine Monticello", "Emagine Monticello", "emagine-monticello", "Minnesota", "Monticello"),
    ("Emagine Rogers", "Emagine Rogers", "emagine-rogers", "Minnesota", "Rogers"),
    ("St. Michael Cinema", "St. Michael Cinema", "st-michael-cinema", "Minnesota", "Saint Michael"),
    ("St Michael Drive In", "St Michael Drive In", "st-michael-drive-in", "Minnesota", "Saint Michael")
  ))
  private def p_salem_massachusetts: R = ("salem-massachusetts", "salem-massachusetts", "Salem", "Massachusetts", 42.5198, -70.8955, "America/New_York", true, Seq("Salem", "Beverly", "Gloucester", "Marblehead", "Rockport"), Seq(
    ("Cape Ann Community Cinema", "Cape Ann Community Cinema", "cape-ann-community-cinema", "Massachusetts", "Rockport"),
    ("Cinema Salem 3", "Cinema Salem 3", "cinema-salem-3", "Massachusetts", "Salem"),
    ("Gloucester Cinema", "Gloucester Cinema", "gloucester-cinema", "Massachusetts", "East Gloucester"),
    ("Warwick Cinema Marblehead", "Warwick Cinema Marblehead", "warwick-cinema-marblehead", "Massachusetts", "Marblehead"),
    ("Cabot Street Cinema Theatre", "Cabot Street Cinema Theatre", "cabot-street-cinema-theatre", "Massachusetts", "Beverly")
  ))
  private def p_salem_ohio: R = ("salem-ohio", "salem-ohio", "Salem", "Ohio", 40.9009, -80.8568, "America/New_York", true, Seq("Salem", "Columbiana"), Seq(
    ("Salem Twin Cinema", "Salem Twin Cinema", "salem-twin-cinema", "Ohio", "Salem"),
    ("Main Street Theater Columbiana", "Main Street Theater Columbiana", "main-street-theater-columbiana", "Ohio", "Columbiana")
  ))
  private def p_salina: R = ("salina", "salina-kansas", "Salina", "Kansas", 38.8403, -97.6114, "America/Chicago", false, Seq("Salina"), Seq(
    ("Salina Art Center", "Salina Art Center", "salina-art-center", "Kansas", "Salina")
  ))
  private def p_salisbury: R = ("salisbury", "salisbury-maryland", "Salisbury", "Maryland", 38.3607, -75.5994, "America/New_York", true, Seq("Salisbury", "Pocomoke City", "Chincoteague"), Seq(
    ("Regal Salisbury", "Regal Salisbury", "regal-salisbury", "Maryland", "Salisbury"),
    ("Island Theatre Chincoteague", "Island Theatre Chincoteague", "island-theatre-chincoteague", "Virginia", "Chincoteague"),
    ("Mar-Va Theater Performing Arts Center", "Mar-Va Theater Performing Arts Center", "mar-va-theater-performing-arts-center", "Maryland", "Pocomoke City")
  ))
  private def p_salmon: R = ("salmon", "salmon-idaho", "Salmon", "Idaho", 45.1758, -113.8959, "America/Boise", false, Seq("Salmon"), Seq(
    ("River Cinemas Salmon", "River Cinemas Salmon", "river-cinemas-salmon", "Idaho", "Salmon")
  ))
  private def p_san_angelo: R = ("san-angelo", "san-angelo-texas", "San Angelo", "Texas", 31.4638, -100.437, "America/Chicago", false, Seq("San Angelo"), Seq(
    ("Cinemark Tinseltown San Angelo", "Cinemark Tinseltown San Angelo", "cinemark-tinseltown-san-angelo", "Texas", "San Angelo"),
    ("Icon Cinemas San Angelo", "Icon Cinemas San Angelo", "icon-cinemas-san-angelo", "Texas", "San Angelo")
  ))
  private def p_san_luis_obispo: R = ("san-luis-obispo", "san-luis-obispo-california", "San Luis Obispo", "California", 35.2828, -120.6596, "America/Los_Angeles", false, Seq("San Luis Obispo"), Seq(
    ("Downtown Centre Cinemas – San Luis Obispo", "Downtown Centre Cinemas – San Luis Obispo", "downtown-centre-cinemas---san-luis-obispo", "California", "San Luis Obispo"),
    ("Palm Theatre San Luis Obispo", "Palm Theatre San Luis Obispo", "palm-theatre-san-luis-obispo", "California", "San Luis Obispo"),
    ("Sunset Drive-in San Luis Obispo", "Sunset Drive-in San Luis Obispo", "sunset-drive-in-san-luis-obispo", "California", "San Luis Obispo")
  ))
  private def p_san_marcos: R = ("san-marcos", "san-marcos-california", "San Marcos", "California", 33.1434, -117.1661, "America/Los_Angeles", true, Seq("San Marcos", "Encinitas", "Poway", "Bonsall"), Seq(
    ("AMC DINE-IN Poway 10", "AMC DINE-IN Poway 10", "amc-dine-in-poway-10", "California", "Poway"),
    ("La Paloma Theatre Encinitas", "La Paloma Theatre Encinitas", "la-paloma-theatre-encinitas", "California", "Encinitas"),
    ("Regal San Marcos", "Regal San Marcos", "regal-san-marcos", "California", "San Marcos"),
    ("River Village Cinema is D'Place", "River Village Cinema is D'Place", "river-village-cinema-is-dplace", "California", "Bonsall")
  ))
  private def p_san_marcos_texas: R = ("san-marcos-texas", "san-marcos-texas", "San Marcos", "Texas", 29.8833, -97.9414, "America/Chicago", true, Seq("San Marcos", "Kyle", "Buda"), Seq(
    ("Doc's Drive In", "Doc's Drive In", "docs-drive-in", "Texas", "Buda"),
    ("EVO Cinemas Kyle", "EVO Cinemas Kyle", "evo-cinemas-kyle", "Texas", "Kyle"),
    ("EVO Cinemas San Marcos", "EVO Cinemas San Marcos", "evo-cinemas-san-marcos", "Texas", "San Marcos")
  ))
  private def p_san_ramon: R = ("san-ramon", "san-ramon-california", "San Ramon", "California", 37.7799, -121.978, "America/Los_Angeles", true, Seq("San Ramon", "Union City", "Walnut Creek", "Castro Valley", "Martinez", "Pleasant Hill"), Seq(
    ("Chabot Theatre Castro Valley", "Chabot Theatre Castro Valley", "chabot-theatre-castro-valley", "California", "Castro Valley"),
    ("Cinemark Century Downtown Pleasant Hill 16", "Cinemark Century Downtown Pleasant Hill 16", "cinemark-century-downtown-pleasant-hill-16", "California", "Pleasant Hill"),
    ("Cinemark Century Union Landing 25", "Cinemark Century Union Landing 25", "cinemark-century-union-landing-25", "California", "Union City"),
    ("Cinemark Century Walnut Creek 14", "Cinemark Century Walnut Creek 14", "cinemark-century-walnut-creek-14", "California", "Walnut Creek"),
    ("Contra Costa Stadium Cinemas", "Contra Costa Stadium Cinemas", "contra-costa-stadium-cinemas", "California", "Martinez"),
    ("THE LOT City Center", "THE LOT City Center", "the-lot-city-center", "California", "San Ramon")
  ))
  private def p_sandusky: R = ("sandusky", "sandusky-ohio", "Sandusky", "Ohio", 41.4489, -82.708, "America/New_York", true, Seq("Sandusky", "Norwalk", "Fremont", "Monroeville"), Seq(
    ("Cinemark Sandusky Stadium 10", "Cinemark Sandusky Stadium 10", "cinemark-sandusky-stadium-10", "Ohio", "Sandusky"),
    ("Paramount Cinema Fremont 4", "Paramount Cinema Fremont 4", "paramount-cinema-fremont-4", "Ohio", "Fremont"),
    ("UEC Theatres Norwalk", "UEC Theatres Norwalk", "uec-theatres-norwalk", "Ohio", "Norwalk"),
    ("Starview Drive-in Norwalk", "Starview Drive-in Norwalk", "starview-drive-in-norwalk", "Ohio", "East Norwalk")
  ))
  private def p_sandusky_michigan: R = ("sandusky-michigan", "sandusky-michigan", "Sandusky", "Michigan", 43.4203, -82.8297, "America/Detroit", false, Seq("Sandusky"), Seq(
    ("Firebird Theater Sandusky", "Firebird Theater Sandusky", "firebird-theater-sandusky", "Michigan", "Sandusky"),
    ("Hi Way Drive-in Sandusky", "Hi Way Drive-in Sandusky", "hi-way-drive-in-sandusky", "Michigan", "Sandusky")
  ))
  private def p_sandy: R = ("sandy", "sandy-utah", "Sandy", "Utah", 40.5916, -111.8841, "America/Denver", false, Seq("Sandy"), Seq(
    ("Cinemark Century Sandy Union Heights 16", "Cinemark Century Sandy Union Heights 16", "cinemark-century-sandy-union-heights-16", "Utah", "Sandy"),
    ("Megaplex Jordan Commons", "Megaplex Jordan Commons", "megaplex-jordan-commons", "Utah", "Sandy"),
    ("King Otter Theater", "King Otter Theater", "king-otter-theater", "Utah", "Sandy")
  ))
  private def p_sanford_maine: R = ("sanford-maine", "sanford-maine", "Sanford", "Maine", 43.4393, -70.7742, "America/New_York", true, Seq("Sanford", "Wells Beach Station"), Seq(
    ("Flagship Cinemas Wells", "Flagship Cinemas Wells", "flagship-cinemas-wells", "Maine", "Wells"),
    ("Smitty's 6 Cinema Pub Sanford", "Smitty's 6 Cinema Pub Sanford", "smittys-6-cinema-pub-sanford", "Maine", "Sanford")
  ))
  private def p_santa_barbara: R = ("santa-barbara", "santa-barbara-california", "Santa Barbara", "California", 34.4208, -119.6982, "America/Los_Angeles", false, Seq("Santa Barbara"), Seq(
    ("Arlington Theatre Santa Barbara", "Arlington Theatre Santa Barbara", "arlington-theatre-santa-barbara", "California", "Santa Barbara"),
    ("Hitchcock Cinema Santa Barbara", "Hitchcock Cinema Santa Barbara", "hitchcock-cinema-santa-barbara", "California", "Santa Barbara"),
    ("Metro 4 Santa Barbara", "Metro 4 Santa Barbara", "metro-4-santa-barbara", "California", "Santa Barbara"),
    ("Paseo Nuevo Cinemas Santa Barbara", "Paseo Nuevo Cinemas Santa Barbara", "paseo-nuevo-cinemas-santa-barbara", "California", "Santa Barbara"),
    ("SBIFF Film Center Santa Barbara", "SBIFF Film Center Santa Barbara", "sbiff-film-center-santa-barbara", "California", "Santa Barbara"),
    ("SBIFF Riviera Theatre", "SBIFF Riviera Theatre", "sbiff-riviera-theatre", "California", "Santa Barbara"),
    ("Carsey-Wolf Theatre", "Carsey-Wolf Theatre", "carsey-wolf-theatre", "California", "Santa Barbara")
  ))
  private def p_santa_cruz: R = ("santa-cruz", "santa-cruz-california", "Santa Cruz", "California", 36.9741, -122.0308, "America/Los_Angeles", true, Seq("Santa Cruz", "Scotts Valley", "Capitola"), Seq(
    ("CineLux Capitola Cafe & Lounge", "CineLux Capitola Cafe & Lounge", "cinelux-capitola-cafe-lounge", "California", "Capitola"),
    ("CineLux Scotts Valley Cafe & Lounge", "CineLux Scotts Valley Cafe & Lounge", "cinelux-scotts-valley-cafe-lounge", "California", "Scotts Valley"),
    ("Landmark Del Mar Theatre", "Landmark Del Mar Theatre", "landmark-del-mar-theatre", "California", "Santa Cruz"),
    ("Santa Cruz Cinema", "Santa Cruz Cinema", "santa-cruz-cinema", "California", "Santa Cruz")
  ))
  private def p_santa_fe: R = ("santa-fe", "santa-fe-new-mexico", "Santa Fe", "New Mexico", 35.687, -105.9378, "America/Denver", false, Seq("Santa Fe"), Seq(
    ("EVO Cinemas Santa Fe", "EVO Cinemas Santa Fe", "evo-cinemas-santa-fe", "New Mexico", "Santa Fe"),
    ("Jean Cocteau Cinema", "Jean Cocteau Cinema", "jean-cocteau-cinema", "New Mexico", "Santa Fe"),
    ("Regal Santa Fe", "Regal Santa Fe", "regal-santa-fe", "New Mexico", "Santa Fe"),
    ("Sky Cinemas Santa Fe", "Sky Cinemas Santa Fe", "sky-cinemas-santa-fe", "New Mexico", "Santa Fe"),
    ("Center for Contemporary Arts Cinema", "Center for Contemporary Arts Cinema", "center-for-contemporary-arts-cinema", "New Mexico", "Santa Fe")
  ))
  private def p_santa_monica: R = ("santa-monica", "santa-monica-california", "Santa Monica", "California", 34.0195, -118.4914, "America/Los_Angeles", false, Seq("Santa Monica"), Seq(
    ("Aero Theatre Santa Monica", "Aero Theatre Santa Monica", "aero-theatre-santa-monica", "California", "Santa Monica"),
    ("AMC Santa Monica 7", "AMC Santa Monica 7", "amc-santa-monica-7", "California", "Santa Monica"),
    ("Laemmle Monica Film Center", "Laemmle Monica Film Center", "laemmle-monica-film-center", "California", "Santa Monica")
  ))
  private def p_santa_rosa_new_mexico: R = ("santa-rosa-new-mexico", "santa-rosa-new-mexico", "Santa Rosa", "New Mexico", 34.9387, -104.6825, "America/Denver", false, Seq("Santa Rosa"), Seq(
    ("Pecos Theatre", "Pecos Theatre", "pecos-theatre", "New Mexico", "Santa Rosa")
  ))
  private def p_sapulpa: R = ("sapulpa", "sapulpa-oklahoma", "Sapulpa", "Oklahoma", 35.9987, -96.1142, "America/Chicago", false, Seq("Sapulpa"), Seq(
    ("B&B Sapula Cinema 8", "B&B Sapula Cinema 8", "bb-sapula-cinema-8", "Oklahoma", "Sapulpa"),
    ("Tee Pee Drive-In Sapulpa", "Tee Pee Drive-In Sapulpa", "tee-pee-drive-in-sapulpa", "Oklahoma", "Sapulpa")
  ))
  private def p_sarasota: R = ("sarasota", "sarasota-florida", "Sarasota", "Florida", 27.3364, -82.5306, "America/New_York", false, Seq("Sarasota"), Seq(
    ("Burns Court Cinemas Sarasota", "Burns Court Cinemas Sarasota", "burns-court-cinemas-sarasota", "Florida", "Sarasota"),
    ("CMX CinéBistro Siesta Key", "CMX CinéBistro Siesta Key", "cmx-cinbistro-siesta-key", "Florida", "Sarasota"),
    ("Regal Hollywood Sarasota", "Regal Hollywood Sarasota", "regal-hollywood-sarasota", "Florida", "Sarasota"),
    ("Sarasota Opera House", "Sarasota Opera House", "sarasota-opera-house", "Florida", "Sarasota")
  ))
  private def p_savannah_tennessee: R = ("savannah-tennessee", "savannah-tennessee", "Savannah", "Tennessee", 35.2248, -88.2492, "America/Chicago", false, Seq("Savannah"), Seq(
    ("Southgate Savannah 6", "Southgate Savannah 6", "southgate-savannah-6", "Tennessee", "Savannah")
  ))
  private def p_sayre: R = ("sayre", "sayre-pennsylvania", "Sayre", "Pennsylvania", 41.979, -76.5155, "America/New_York", true, Seq("Sayre", "Towanda"), Seq(
    ("Keystone Theatre Towanda", "Keystone Theatre Towanda", "keystone-theatre-towanda", "Pennsylvania", "Towanda"),
    ("Sayre Theatre", "Sayre Theatre", "sayre-theatre", "Pennsylvania", "Sayre")
  ))
  private def p_sayville: R = ("sayville", "sayville-new-york", "Sayville", "New York", 40.7359, -73.0821, "America/New_York", true, Seq("Sayville", "Patchogue"), Seq(
    ("Plaza Cinema & Media Art Center Patchogue", "Plaza Cinema & Media Art Center Patchogue", "plaza-cinema-media-art-center-patchogue", "New York", "Patchogue"),
    ("Sayville Cinemas", "Sayville Cinemas", "sayville-cinemas", "New York", "Sayville")
  ))
  private def p_schenectady: R = ("schenectady", "schenectady-new-york", "Schenectady", "New York", 42.8142, -73.9396, "America/New_York", false, Seq("Schenectady"), Seq(
    ("BTM Movieland Schenectady", "BTM Movieland Schenectady", "btm-movieland-schenectady", "New York", "Schenectady"),
    ("Rotterdam Square Cinema", "Rotterdam Square Cinema", "rotterdam-square-cinema", "New York", "Schenectady"),
    ("Proctors Theatre Schenectady", "Proctors Theatre Schenectady", "proctors-theatre-schenectady", "New York", "Schenectady")
  ))
  private def p_schererville: R = ("schererville", "schererville-indiana", "Schererville", "Indiana", 41.4789, -87.4548, "America/Chicago", true, Seq("Schererville", "Merrillville", "Chicago Heights"), Seq(
    ("AMC CLASSIC Hobart 12", "AMC CLASSIC Hobart 12", "amc-classic-hobart-12", "Indiana", "Merrillville"),
    ("AMC Schererville 12", "AMC Schererville 12", "amc-schererville-12", "Indiana", "Schererville"),
    ("AMC Schererville 16", "AMC Schererville 16", "amc-schererville-16", "Indiana", "Schererville"),
    ("Marcus Chicago Heights Cinema", "Marcus Chicago Heights Cinema", "marcus-chicago-heights-cinema", "Illinois", "Chicago Heights")
  ))
  private def p_schertz: R = ("schertz", "schertz-texas", "Schertz", "Texas", 29.5522, -98.2697, "America/Chicago", true, Seq("Schertz", "Cibolo", "Seguin"), Seq(
    ("EVO Cinemas Schertz", "EVO Cinemas Schertz", "evo-cinemas-schertz", "Texas", "Schertz"),
    ("Hometown Cinemas Seguin", "Hometown Cinemas Seguin", "hometown-cinemas-seguin", "Texas", "Seguin"),
    ("Santikos Cibolo", "Santikos Cibolo", "santikos-cibolo", "Texas", "Cibolo")
  ))
  private def p_schroon_lake: R = ("schroon-lake", "schroon-lake-new-york", "Schroon Lake", "New York", 43.8374, -73.7612, "America/New_York", false, Seq("Schroon Lake"), Seq(
    ("Strand Theater Schroon Lake", "Strand Theater Schroon Lake", "strand-theater-schroon-lake", "New York", "Schroon Lake")
  ))
  private def p_scobey: R = ("scobey", "scobey-montana", "Scobey", "Montana", 48.7925, -105.4208, "America/Denver", false, Seq("Scobey"), Seq(
    ("Richardson Theatre", "Richardson Theatre", "richardson-theatre", "Montana", "Scobey")
  ))
  private def p_scottsbluff: R = ("scottsbluff", "scottsbluff-nebraska", "Scottsbluff", "Nebraska", 41.8666, -103.6672, "America/Denver", true, Seq("Scottsbluff", "Mitchell"), Seq(
    ("Golden Ticket Scottsbluff Reel Lux 6", "Golden Ticket Scottsbluff Reel Lux 6", "golden-ticket-scottsbluff-reel-lux-6", "Nebraska", "Scottsbluff"),
    ("Midwest Theatre Scottsbluff", "Midwest Theatre Scottsbluff", "midwest-theatre-scottsbluff", "Nebraska", "Scottsbluff"),
    ("Nile Theatre Mitchell", "Nile Theatre Mitchell", "nile-theatre-mitchell", "Nebraska", "Mitchell")
  ))
  private def p_scottsboro: R = ("scottsboro", "scottsboro-alabama", "Scottsboro", "Alabama", 34.6723, -86.0341, "America/Chicago", true, Seq("Scottsboro", "Henagar"), Seq(
    ("Hollywood 10 Cinema Scottsboro", "Hollywood 10 Cinema Scottsboro", "hollywood-10-cinema-scottsboro", "Alabama", "Scottsboro"),
    ("Ridgeview Drive-In", "Ridgeview Drive-In", "ridgeview-drive-in", "Alabama", "Henagar")
  ))
  private def p_scranton: R = ("scranton", "scranton-pennsylvania", "Scranton", "Pennsylvania", 41.4092, -75.6649, "America/New_York", true, Seq("Scranton", "Dickson City", "Moosic", "West Wyoming", "Tunkhannock"), Seq(
    ("Cinemark Moosic 20", "Cinemark Moosic 20", "cinemark-moosic-20", "Pennsylvania", "Moosic"),
    ("Dietrich Theater Tunkhannock", "Dietrich Theater Tunkhannock", "dietrich-theater-tunkhannock", "Pennsylvania", "Tunkhannock"),
    ("Moonlite Drive-in West Wyoming", "Moonlite Drive-in West Wyoming", "moonlite-drive-in-west-wyoming", "Pennsylvania", "West Wyoming"),
    ("Regal Dickson City", "Regal Dickson City", "regal-dickson-city", "Pennsylvania", "Dickson City"),
    ("Scranton Art Haus", "Scranton Art Haus", "scranton-art-haus", "Pennsylvania", "Scranton"),
    ("Circle Drive-in Scranton", "Circle Drive-in Scranton", "circle-drive-in-scranton", "Pennsylvania", "Scranton")
  ))
  private def p_searcy: R = ("searcy", "searcy-arkansas", "Searcy", "Arkansas", 35.2506, -91.7362, "America/Chicago", false, Seq("Searcy"), Seq(
    ("Searcy Cinema 8", "Searcy Cinema 8", "searcy-cinema-8", "Arkansas", "Searcy"),
    ("Rialto Theater Searcy", "Rialto Theater Searcy", "rialto-theater-searcy", "Arkansas", "Searcy")
  ))
  private def p_seaside: R = ("seaside", "seaside-oregon", "Seaside", "Oregon", 45.9932, -123.9226, "America/Los_Angeles", false, Seq("Seaside"), Seq(
    ("Seaside Cinemas 6", "Seaside Cinemas 6", "seaside-cinemas-6", "Oregon", "Seaside")
  ))
  private def p_sebring: R = ("sebring", "sebring-florida", "Sebring", "Florida", 27.4956, -81.4409, "America/New_York", false, Seq("Sebring"), Seq(
    ("AMC Lakeshore 8", "AMC Lakeshore 8", "amc-lakeshore-8", "Florida", "Sebring")
  ))
  private def p_sedalia: R = ("sedalia", "sedalia-missouri", "Sedalia", "Missouri", 38.7045, -93.2283, "America/Chicago", false, Seq("Sedalia"), Seq(
    ("Sedalia Galaxy 10", "Sedalia Galaxy 10", "sedalia-galaxy-10", "Missouri", "Sedalia")
  ))
  private def p_sedan: R = ("sedan", "sedan-kansas", "Sedan", "Kansas", 37.1267, -96.1869, "America/Chicago", false, Seq("Sedan"), Seq(
    ("Gregg Theatre Sedan", "Gregg Theatre Sedan", "gregg-theatre-sedan", "Kansas", "Sedan")
  ))
  private def p_sedona: R = ("sedona", "sedona-arizona", "Sedona", "Arizona", 34.8697, -111.761, "America/Phoenix", false, Seq("Sedona"), Seq(
    ("Harkins Sedona 6", "Harkins Sedona 6", "harkins-sedona-6", "Arizona", "Sedona"),
    ("Mary D. Fisher Theatre", "Mary D. Fisher Theatre", "mary-d-fisher-theatre", "Arizona", "Sedona")
  ))
  private def p_selinsgrove: R = ("selinsgrove", "selinsgrove-pennsylvania", "Selinsgrove", "Pennsylvania", 40.799, -76.8622, "America/New_York", true, Seq("Selinsgrove", "Lewisburg", "Northumberland", "Millersburg"), Seq(
    ("AMC CLASSIC Selinsgrove 12", "AMC CLASSIC Selinsgrove 12", "amc-classic-selinsgrove-12", "Pennsylvania", "Selinsgrove"),
    ("Campus Theatre Lewisburg", "Campus Theatre Lewisburg", "campus-theatre-lewisburg", "Pennsylvania", "Lewisburg"),
    ("Point Drive-in Northumberland", "Point Drive-in Northumberland", "point-drive-in-northumberland", "Pennsylvania", "Northumberland"),
    ("Twin Valley Players Colonnade Theatre Millersburg", "Twin Valley Players Colonnade Theatre Millersburg", "twin-valley-players-colonnade-theatre-millersburg", "Pennsylvania", "Millersburg")
  ))
  private def p_selma: R = ("selma", "selma-california", "Selma", "California", 36.5708, -119.6121, "America/Los_Angeles", true, Seq("Selma", "Dinuba"), Seq(
    ("Selma 6 Cinemas", "Selma 6 Cinemas", "selma-6-cinemas", "California", "Selma"),
    ("Dinuba Cinema is D'Place", "Dinuba Cinema is D'Place", "dinuba-cinema-is-dplace", "California", "Dinuba")
  ))
  private def p_selma_alabama: R = ("selma-alabama", "selma-alabama", "Selma", "Alabama", 32.4074, -87.0211, "America/Chicago", false, Seq("Selma"), Seq(
    ("Walton Theater Selma", "Walton Theater Selma", "walton-theater-selma", "Alabama", "Selma")
  ))
  private def p_seminole: R = ("seminole", "seminole-oklahoma", "Seminole", "Oklahoma", 35.2245, -96.6706, "America/Chicago", false, Seq("Seminole"), Seq(
    ("Strothers Seminole", "Strothers Seminole", "strothers-seminole", "Oklahoma", "Seminole")
  ))
  private def p_senatobia: R = ("senatobia", "senatobia-mississippi", "Senatobia", "Mississippi", 34.6176, -89.9687, "America/Chicago", false, Seq("Senatobia"), Seq(
    ("Tobie Cinema Senatobia", "Tobie Cinema Senatobia", "tobie-cinema-senatobia", "Mississippi", "Senatobia")
  ))
  private def p_seneca: R = ("seneca", "seneca-south-carolina", "Seneca", "South Carolina", 34.6857, -82.9532, "America/New_York", false, Seq("Seneca"), Seq(
    ("Seneca Premiere 8", "Seneca Premiere 8", "seneca-premiere-8", "South Carolina", "Seneca")
  ))
  private def p_seneca_kansas: R = ("seneca-kansas", "seneca-kansas", "Seneca", "Kansas", 39.8342, -96.0642, "America/Chicago", false, Seq("Seneca"), Seq(
    ("Seneca Twin Theatre", "Seneca Twin Theatre", "seneca-twin-theatre", "Kansas", "Seneca")
  ))
  private def p_sevierville: R = ("sevierville", "sevierville-tennessee", "Sevierville", "Tennessee", 35.8681, -83.5618, "America/New_York", true, Seq("Sevierville", "Pigeon Forge"), Seq(
    ("Governor's Crossing Sevierville", "Governor's Crossing Sevierville", "governors-crossing-sevierville", "Tennessee", "Sevierville"),
    ("The Forge Cinemas", "The Forge Cinemas", "the-forge-cinemas", "Tennessee", "Pigeon Forge")
  ))
  private def p_seward: R = ("seward", "seward-nebraska", "Seward", "Nebraska", 40.907, -97.0989, "America/Chicago", false, Seq("Seward"), Seq(
    ("Rivoli Theatre Seward", "Rivoli Theatre Seward", "rivoli-theatre-seward", "Nebraska", "Seward")
  ))
  private def p_seymour: R = ("seymour", "seymour-indiana", "Seymour", "Indiana", 38.9592, -85.8902, "America/Indiana/Indianapolis", true, Seq("Seymour", "Scottsburg", "North Vernon"), Seq(
    ("Park Theatre Civic Centre North Vernon", "Park Theatre Civic Centre North Vernon", "park-theatre-civic-centre-north-vernon", "Indiana", "North Vernon"),
    ("Showplace Cinemas Seymour", "Showplace Cinemas Seymour", "showplace-cinemas-seymour", "Indiana", "Seymour"),
    ("Ross Theatre Scottsburg", "Ross Theatre Scottsburg", "ross-theatre-scottsburg", "Indiana", "Scottsburg")
  ))
  private def p_shallotte: R = ("shallotte", "shallotte-north-carolina", "Shallotte", "North Carolina", 33.9732, -78.3858, "America/New_York", false, Seq("Shallotte"), Seq(
    ("Moonlight Drive-in @ Planet Fun", "Moonlight Drive-in @ Planet Fun", "moonlight-drive-in-planet-fun", "North Carolina", "Shallotte")
  ))
  private def p_shamrock: R = ("shamrock", "shamrock-texas", "Shamrock", "Texas", 35.2142, -100.249, "America/Chicago", false, Seq("Shamrock"), Seq(
    ("Texas Theatre Shamrock", "Texas Theatre Shamrock", "texas-theatre-shamrock", "Texas", "Shamrock")
  ))
  private def p_sharon_springs: R = ("sharon-springs", "sharon-springs-kansas", "Sharon Springs", "Kansas", 38.8978, -101.7521, "America/Denver", false, Seq("Sharon Springs"), Seq(
    ("Strand Theatre Sharon Springs", "Strand Theatre Sharon Springs", "strand-theatre-sharon-springs", "Kansas", "Sharon Springs")
  ))
  private def p_shattuck: R = ("shattuck", "shattuck-oklahoma", "Shattuck", "Oklahoma", 36.2761, -99.8828, "America/Chicago", false, Seq("Shattuck"), Seq(
    ("El-co Drive-in Shattuck", "El-co Drive-in Shattuck", "el-co-drive-in-shattuck", "Oklahoma", "Shattuck")
  ))
  private def p_shawano: R = ("shawano", "shawano-wisconsin", "Shawano", "Wisconsin", 44.7822, -88.609, "America/Chicago", false, Seq("Shawano"), Seq(
    ("Shawano Cinema", "Shawano Cinema", "shawano-cinema", "Wisconsin", "Shawano")
  ))
  private def p_shawnee: R = ("shawnee", "shawnee-oklahoma", "Shawnee", "Oklahoma", 35.3273, -96.9253, "America/Chicago", false, Seq("Shawnee"), Seq(
    ("AMC Shawnee Cinema 8", "AMC Shawnee Cinema 8", "amc-shawnee-cinema-8", "Oklahoma", "Shawnee"),
    ("Golden Ticket Cinemas Shawnee", "Golden Ticket Cinemas Shawnee", "golden-ticket-cinemas-shawnee", "Oklahoma", "Shawnee")
  ))
  private def p_shawnee_kansas: R = ("shawnee-kansas", "shawnee-kansas", "Shawnee", "Kansas", 39.0417, -94.7202, "America/Chicago", true, Seq("Shawnee", "Leavenworth", "Leawood", "Liberty", "Merriam"), Seq(
    ("AMC Town Center 20", "AMC Town Center 20", "amc-town-center-20", "Kansas", "Leawood"),
    ("B&B Leavenworth Landing 5", "B&B Leavenworth Landing 5", "bb-leavenworth-landing-5", "Kansas", "Leavenworth"),
    ("B&B Shawnee 18", "B&B Shawnee 18", "bb-shawnee-18", "Kansas", "Shawnee"),
    ("B&B Theatres Liberty Cinema 12", "B&B Theatres Liberty Cinema 12", "bb-theatres-liberty-cinema-12", "Missouri", "Liberty"),
    ("Cinemark Tinseltown Merriam", "Cinemark Tinseltown Merriam", "cinemark-tinseltown-merriam", "Kansas", "Merriam")
  ))
  private def p_sheboygan: R = ("sheboygan", "sheboygan-wisconsin", "Sheboygan", "Wisconsin", 43.7508, -87.7145, "America/Chicago", false, Seq("Sheboygan"), Seq(
    ("Marcus Sheboygan Cinema", "Marcus Sheboygan Cinema", "marcus-sheboygan-cinema", "Wisconsin", "Sheboygan"),
    ("Stefanie H. Weill Center", "Stefanie H. Weill Center", "stefanie-h-weill-center", "Wisconsin", "Sheboygan")
  ))
  private def p_shelbina: R = ("shelbina", "shelbina-missouri", "Shelbina", "Missouri", 39.6939, -92.043, "America/Chicago", false, Seq("Shelbina"), Seq(
    ("Hawkins Theatre Shelbina", "Hawkins Theatre Shelbina", "hawkins-theatre-shelbina", "Missouri", "Shelbina")
  ))
  private def p_shelby: R = ("shelby", "shelby-north-carolina", "Shelby", "North Carolina", 35.2923, -81.5357, "America/New_York", true, Seq("Shelby", "Forest City", "East Gaffney"), Seq(
    ("Forest City's Cinema IV", "Forest City's Cinema IV", "forest-citys-cinema-iv", "North Carolina", "Forest City"),
    ("AMC CLASSIC Shelby 10", "AMC CLASSIC Shelby 10", "amc-classic-shelby-10", "North Carolina", "Shelby"),
    ("The Big E Gaffney", "The Big E Gaffney", "the-big-e-gaffney", "South Carolina", "East Gaffney")
  ))
  private def p_shelby_montana: R = ("shelby-montana", "shelby-montana", "Shelby", "Montana", 48.5053, -111.857, "America/Denver", false, Seq("Shelby"), Seq(
    ("Roxy Theatre Shelby", "Roxy Theatre Shelby", "roxy-theatre-shelby", "Montana", "Shelby")
  ))
  private def p_shelbyville: R = ("shelbyville", "shelbyville-indiana", "Shelbyville", "Indiana", 39.5214, -85.7769, "America/Indiana/Indianapolis", false, Seq("Shelbyville"), Seq(
    ("Skyline Drive-In Shelbyville", "Skyline Drive-In Shelbyville", "skyline-drive-in-shelbyville", "Indiana", "Shelbyville"),
    ("Studio 10 Cinemas Shelbyville", "Studio 10 Cinemas Shelbyville", "studio-10-cinemas-shelbyville", "Indiana", "Shelbyville"),
    ("Strand Theatre Shelbyville REP", "Strand Theatre Shelbyville REP", "strand-theatre-shelbyville-rep", "Indiana", "Shelbyville")
  ))
  private def p_shelbyville_illinois: R = ("shelbyville-illinois", "shelbyville-illinois", "Shelbyville", "Illinois", 39.4064, -88.7901, "America/Chicago", false, Seq("Shelbyville"), Seq(
    ("Boarman's Roxy Theatre Shelbyville", "Boarman's Roxy Theatre Shelbyville", "boarmans-roxy-theatre-shelbyville", "Illinois", "Shelbyville")
  ))
  private def p_shelbyville_tennessee: R = ("shelbyville-tennessee", "shelbyville-tennessee", "Shelbyville", "Tennessee", 35.4834, -86.4603, "America/Chicago", true, Seq("Shelbyville", "Tullahoma", "Lewisburg", "Fayetteville"), Seq(
    ("Lincoln Theatre Fayetteville", "Lincoln Theatre Fayetteville", "lincoln-theatre-fayetteville", "Tennessee", "Fayetteville"),
    ("Regal Cinema Tullahoma", "Regal Cinema Tullahoma", "regal-cinema-tullahoma", "Tennessee", "Tullahoma"),
    ("Capri Theatre Shelbyville", "Capri Theatre Shelbyville", "capri-theatre-shelbyville", "Tennessee", "Shelbyville"),
    ("Hi-way 50 Drive-in Theatre Lewisburg", "Hi-way 50 Drive-in Theatre Lewisburg", "hi-way-50-drive-in-theatre-lewisburg", "Tennessee", "Lewisburg")
  ))
  private def p_shelton: R = ("shelton", "shelton-washington", "Shelton", "Washington", 47.2151, -123.1007, "America/Los_Angeles", false, Seq("Shelton"), Seq(
    ("Shelton Cinemas", "Shelton Cinemas", "shelton-cinemas", "Washington", "Shelton"),
    ("Skyline Drive-in Shelton", "Skyline Drive-in Shelton", "skyline-drive-in-shelton", "Washington", "Shelton")
  ))
  private def p_sheridan: R = ("sheridan", "sheridan-wyoming", "Sheridan", "Wyoming", 44.7972, -106.9562, "America/Denver", false, Seq("Sheridan"), Seq(
    ("Centennial Cinema Sheridan", "Centennial Cinema Sheridan", "centennial-cinema-sheridan", "Wyoming", "Sheridan"),
    ("WYO Theater", "WYO Theater", "wyo-theater", "Wyoming", "Sheridan")
  ))
  private def p_sherman: R = ("sherman", "sherman-texas", "Sherman", "Texas", 33.6357, -96.6089, "America/Chicago", false, Seq("Sherman"), Seq(
    ("Cinemark Sherman", "Cinemark Sherman", "cinemark-sherman", "Texas", "Sherman"),
    ("Schulman's Movie Bowl Grille Sherman", "Schulman's Movie Bowl Grille Sherman", "schulmans-movie-bowl-grille-sherman", "Texas", "Sherman")
  ))
  private def p_show_low: R = ("show-low", "show-low-arizona", "Show Low", "Arizona", 34.2542, -110.0298, "America/Phoenix", true, Seq("Show Low", "Pinetop-Lakeside"), Seq(
    ("WME Show Low 5", "WME Show Low 5", "wme-show-low-5", "Arizona", "Show Low"),
    ("WME Village 8", "WME Village 8", "wme-village-8", "Arizona", "Pinetop-Lakeside")
  ))
  private def p_sidney: R = ("sidney", "sidney-ohio", "Sidney", "Ohio", 40.2842, -84.1555, "America/New_York", true, Seq("Sidney", "Piqua", "Bellefontaine", "Jackson Center"), Seq(
    ("Cinemark Miami Valley Piqua 8", "Cinemark Miami Valley Piqua 8", "cinemark-miami-valley-piqua-8", "Ohio", "Piqua"),
    ("Elder Theatre Jackson Center", "Elder Theatre Jackson Center", "elder-theatre-jackson-center", "Ohio", "Jackson Center"),
    ("Bellefontaine Cinema 8", "Bellefontaine Cinema 8", "bellefontaine-cinema-8", "Ohio", "Bellefontaine"),
    ("Sidney Auto-Vue Drive-In", "Sidney Auto-Vue Drive-In", "sidney-auto-vue-drive-in", "Ohio", "Sidney"),
    ("The Historic Sidney Theatre", "The Historic Sidney Theatre", "the-historic-sidney-theatre", "Ohio", "Sidney")
  ))
  private def p_sidney_montana: R = ("sidney-montana", "sidney-montana", "Sidney", "Montana", 47.7167, -104.1563, "America/Denver", false, Seq("Sidney"), Seq(
    ("Centre Theatre Sidney", "Centre Theatre Sidney", "centre-theatre-sidney", "Montana", "Sidney")
  ))
  private def p_sidney_nebraska: R = ("sidney-nebraska", "sidney-nebraska", "Sidney", "Nebraska", 41.1428, -102.978, "America/Denver", false, Seq("Sidney"), Seq(
    ("Sidney Fox Theatre", "Sidney Fox Theatre", "sidney-fox-theatre", "Nebraska", "Sidney")
  ))
  private def p_sierra_vista: R = ("sierra-vista", "sierra-vista-arizona", "Sierra Vista", "Arizona", 31.5545, -110.3037, "America/Phoenix", false, Seq("Sierra Vista"), Seq(
    ("Cinemark Sierra Vista 10", "Cinemark Sierra Vista 10", "cinemark-sierra-vista-10", "Arizona", "Sierra Vista")
  ))
  private def p_sikeston: R = ("sikeston", "sikeston-missouri", "Sikeston", "Missouri", 36.8767, -89.5879, "America/Chicago", false, Seq("Sikeston"), Seq(
    ("Malco Sikeston 8 Cinema Grill", "Malco Sikeston 8 Cinema Grill", "malco-sikeston-8-cinema-grill", "Missouri", "Sikeston")
  ))
  private def p_silver_city: R = ("silver-city", "silver-city-new-mexico", "Silver City", "New Mexico", 32.7701, -108.2803, "America/Denver", false, Seq("Silver City"), Seq(
    ("The Silco Theater", "The Silco Theater", "the-silco-theater", "New Mexico", "Silver City")
  ))
  private def p_silver_spring: R = ("silver-spring", "silver-spring-maryland", "Silver Spring", "Maryland", 38.9907, -77.0261, "America/New_York", true, Seq("Silver Spring", "Greenbelt", "Rockville", "Downtown DC", "Wheaton", "Laurel", "Hyattsville", "Beltsville", "Falls Church", "Largo"), Seq(
    ("AFI Silver Theatre", "AFI Silver Theatre", "afi-silver-theatre", "Maryland", "Silver Spring"),
    ("Regal Majestic Silver Spring", "Regal Majestic Silver Spring", "regal-majestic-silver-spring", "Maryland", "Silver Spring"),
    ("AMC Academy 8 Greenbelt", "AMC Academy 8 Greenbelt", "amc-academy-8-greenbelt", "Maryland", "Greenbelt"),
    ("Old Greenbelt Theatre", "Old Greenbelt Theatre", "old-greenbelt-theatre", "Maryland", "Greenbelt"),
    ("AMC Center Park 8 Beltsville", "AMC Center Park 8 Beltsville", "amc-center-park-8-beltsville", "Maryland", "Beltsville"),
    ("AMC Magic Johnson Capital Center 12", "AMC Magic Johnson Capital Center 12", "amc-magic-johnson-capital-center-12", "Maryland", "Largo"),
    ("AMC Wheaton Mall 9", "AMC Wheaton Mall 9", "amc-wheaton-mall-9", "Maryland", "Wheaton"),
    ("Paragon Theaters Founders Row", "Paragon Theaters Founders Row", "paragon-theaters-founders-row", "Virginia", "Falls Church"),
    ("Regal Hyattsville Royale", "Regal Hyattsville Royale", "regal-hyattsville-royale", "Maryland", "Hyattsville"),
    ("Regal Laurel Town Center", "Regal Laurel Town Center", "regal-laurel-town-center", "Maryland", "Laurel"),
    ("Regal Rockville Center", "Regal Rockville Center", "regal-rockville-center", "Maryland", "Rockville"),
    ("National Gallery of Art", "National Gallery of Art", "national-gallery-of-art", "District of Columbia", "Washington City")
  ))
  private def p_silverton: R = ("silverton", "silverton-oregon", "Silverton", "Oregon", 45.0051, -122.7832, "America/Los_Angeles", true, Seq("Silverton", "Stayton"), Seq(
    ("Star Cinema Stayton", "Star Cinema Stayton", "star-cinema-stayton", "Oregon", "Stayton"),
    ("Palace Theatre Silverton", "Palace Theatre Silverton", "palace-theatre-silverton", "Oregon", "Silverton")
  ))
  private def p_simpsonville: R = ("simpsonville", "simpsonville-south-carolina", "Simpsonville", "South Carolina", 34.7371, -82.2543, "America/New_York", false, Seq("Simpsonville"), Seq(
    ("Regal Simpsonville", "Regal Simpsonville", "regal-simpsonville", "South Carolina", "Simpsonville")
  ))
  private def p_sioux_center: R = ("sioux-center", "sioux-center-iowa", "Sioux Center", "Iowa", 43.0797, -96.1756, "America/Chicago", true, Seq("Sioux Center", "Sheldon"), Seq(
    ("Fridley Cinema 5 Sioux Center", "Fridley Cinema 5 Sioux Center", "fridley-cinema-5-sioux-center", "Iowa", "Sioux Center"),
    ("Main Street 3 Sheldon", "Main Street 3 Sheldon", "main-street-3-sheldon", "Iowa", "Sheldon")
  ))
  private def p_sioux_city: R = ("sioux-city", "sioux-city-iowa", "Sioux City", "Iowa", 42.5, -96.4003, "America/Chicago", false, Seq("Sioux City"), Seq(
    ("AMC Southern Hills 12", "AMC Southern Hills 12", "amc-southern-hills-12", "Iowa", "Sioux City"),
    ("ACX Promenade Sioux City", "ACX Promenade Sioux City", "acx-promenade-sioux-city", "Iowa", "Sioux City")
  ))
  private def p_siren: R = ("siren", "siren-wisconsin", "Siren", "Wisconsin", 45.7858, -92.381, "America/Chicago", false, Seq("Siren"), Seq(
    ("Timbers Theater Siren", "Timbers Theater Siren", "timbers-theater-siren", "Wisconsin", "Siren")
  ))
  private def p_sisters: R = ("sisters", "sisters-oregon", "Sisters", "Oregon", 44.291, -121.5492, "America/Los_Angeles", false, Seq("Sisters"), Seq(
    ("Sisters Movie House", "Sisters Movie House", "sisters-movie-house", "Oregon", "Sisters")
  ))
  private def p_sitka: R = ("sitka", "sitka-alaska", "Sitka", "Alaska", 57.0532, -135.3309, "America/Sitka", false, Seq("Sitka"), Seq(
    ("Coliseum Theatre Sitka", "Coliseum Theatre Sitka", "coliseum-theatre-sitka", "Alaska", "Sitka")
  ))
  private def p_skowhegan: R = ("skowhegan", "skowhegan-maine", "Skowhegan", "Maine", 44.7651, -69.7192, "America/New_York", false, Seq("Skowhegan"), Seq(
    ("Strand Cinema Skowhegan", "Strand Cinema Skowhegan", "strand-cinema-skowhegan", "Maine", "Skowhegan")
  ))
  private def p_slidell: R = ("slidell", "slidell-louisiana", "Slidell", "Louisiana", 30.2752, -89.7812, "America/Chicago", true, Seq("Slidell", "Picayune", "Covington"), Seq(
    ("Acadiana Picayune Cinema 4", "Acadiana Picayune Cinema 4", "acadiana-picayune-cinema-4", "Mississippi", "Picayune"),
    ("Movie Tavern Covington", "Movie Tavern Covington", "movie-tavern-covington", "Louisiana", "Covington"),
    ("Santikos Grand Slidell", "Santikos Grand Slidell", "santikos-grand-slidell", "Louisiana", "Slidell")
  ))
  private def p_smith_center: R = ("smith-center", "smith-center-kansas", "Smith Center", "Kansas", 39.7792, -98.7851, "America/Chicago", false, Seq("Smith Center"), Seq(
    ("Center Smith Center", "Center Smith Center", "center-smith-center", "Kansas", "Smith Center")
  ))
  private def p_smithfield: R = ("smithfield", "smithfield-north-carolina", "Smithfield", "North Carolina", 35.5085, -78.3394, "America/New_York", false, Seq("Smithfield"), Seq(
    ("GTC Smithfield Cinemas", "GTC Smithfield Cinemas", "gtc-smithfield-cinemas", "North Carolina", "Smithfield")
  ))
  private def p_snyder: R = ("snyder", "snyder-texas", "Snyder", "Texas", 32.7179, -100.9176, "America/Chicago", false, Seq("Snyder"), Seq(
    ("Cinema Snyder", "Cinema Snyder", "cinema-snyder", "Texas", "Snyder")
  ))
  private def p_socorro: R = ("socorro", "socorro-new-mexico", "Socorro", "New Mexico", 34.0584, -106.8914, "America/Denver", false, Seq("Socorro"), Seq(
    ("Loma Theater", "Loma Theater", "loma-theater", "New Mexico", "Socorro")
  ))
  private def p_soda_springs: R = ("soda-springs", "soda-springs-idaho", "Soda Springs", "Idaho", 42.6544, -111.6047, "America/Boise", false, Seq("Soda Springs"), Seq(
    ("Idan Ha Theater", "Idan Ha Theater", "idan-ha-theater", "Idaho", "Soda Springs"),
    ("Idan-Ha Drive In Theater", "Idan-Ha Drive In Theater", "idan-ha-drive-in-theater", "Idaho", "Soda Springs")
  ))
  private def p_soledad: R = ("soledad", "soledad-california", "Soledad", "California", 36.4247, -121.3263, "America/Los_Angeles", true, Seq("Soledad", "King City"), Seq(
    ("King City Cinemas", "King City Cinemas", "king-city-cinemas", "California", "King City"),
    ("Premiere Cinemas Soledad", "Premiere Cinemas Soledad", "premiere-cinemas-soledad", "California", "Soledad")
  ))
  private def p_somerset: R = ("somerset", "somerset-kentucky", "Somerset", "Kentucky", 37.092, -84.6041, "America/New_York", false, Seq("Somerset"), Seq(
    ("Somerset Cinemas 8", "Somerset Cinemas 8", "somerset-cinemas-8", "Kentucky", "Somerset")
  ))
  private def p_somerville: R = ("somerville", "somerville-massachusetts", "Somerville", "Massachusetts", 42.3876, -71.0995, "America/New_York", true, Seq("Somerville", "Arlington", "Dedham", "Dorchester", "Waltham", "Brookline", "Woburn", "Watertown", "Lexington", "Burlington", "Chestnut Hill", "West Newton"), Seq(
    ("AMC Assembly Row 12", "AMC Assembly Row 12", "amc-assembly-row-12", "Massachusetts", "Somerville"),
    ("Somerville Theatre", "Somerville Theatre", "somerville-theatre", "Massachusetts", "Somerville"),
    ("AMC Burlington 10", "AMC Burlington 10", "amc-burlington-10", "Massachusetts", "Burlington"),
    ("AMC South Bay Center 12", "AMC South Bay Center 12", "amc-south-bay-center-12", "Massachusetts", "Dorchester"),
    ("Capitol Theatre Arlington", "Capitol Theatre Arlington", "capitol-theatre-arlington", "Massachusetts", "Arlington"),
    ("Regent Theatre", "Regent Theatre", "regent-theatre", "Massachusetts", "Arlington"),
    ("Coolidge Corner Theatre", "Coolidge Corner Theatre", "coolidge-corner-theatre", "Massachusetts", "Brookline"),
    ("CW Theaters Majestic Watertown 7", "CW Theaters Majestic Watertown 7", "cw-theaters-majestic-watertown-7", "Massachusetts", "Watertown"),
    ("Dedham Community Theatre", "Dedham Community Theatre", "dedham-community-theatre", "Massachusetts", "Dedham"),
    ("Showcase Legacy Place Dedham", "Showcase Legacy Place Dedham", "showcase-legacy-place-dedham", "Massachusetts", "Dedham"),
    ("Embassy Cinema Waltham", "Embassy Cinema Waltham", "embassy-cinema-waltham", "Massachusetts", "Waltham"),
    ("Lexington Venue", "Lexington Venue", "lexington-venue", "Massachusetts", "Lexington"),
    ("Showcase Chestnut Hill", "Showcase Chestnut Hill", "showcase-chestnut-hill", "Massachusetts", "Chestnut Hill"),
    ("Showcase Woburn", "Showcase Woburn", "showcase-woburn", "Massachusetts", "Woburn"),
    ("West Newton Cinema", "West Newton Cinema", "west-newton-cinema", "Massachusetts", "West Newton")
  ))
  private def p_sonora: R = ("sonora", "sonora-california", "Sonora", "California", 37.9841, -120.3821, "America/Los_Angeles", true, Seq("Sonora", "Angels Camp"), Seq(
    ("Angels Theatre", "Angels Theatre", "angels-theatre", "California", "Angels Camp"),
    ("Regal Sonora", "Regal Sonora", "regal-sonora", "California", "Sonora")
  ))
  private def p_south_barrington: R = ("south-barrington", "south-barrington-illinois", "South Barrington", "Illinois", 42.0914, -88.1217, "America/Chicago", true, Seq("South Barrington", "Schaumburg", "Mount Prospect", "Carpentersville", "Wheeling", "Elk Grove Village", "Lake in the Hills", "Vernon Hills", "Bloomingdale", "West Dundee", "Deer Park"), Seq(
    ("AMC Hawthorn 12 Vernon Hills", "AMC Hawthorn 12 Vernon Hills", "amc-hawthorn-12-vernon-hills", "Illinois", "Vernon Hills"),
    ("AMC Lake in the Hills 12", "AMC Lake in the Hills 12", "amc-lake-in-the-hills-12", "Illinois", "Lake in the Hills"),
    ("AMC Randhurst Mount Prospect 12", "AMC Randhurst Mount Prospect 12", "amc-randhurst-mount-prospect-12", "Illinois", "Mount Prospect"),
    ("AMC South Barrington 24", "AMC South Barrington 24", "amc-south-barrington-24", "Illinois", "South Barrington"),
    ("Star Cinema Grill South Barrington", "Star Cinema Grill South Barrington", "star-cinema-grill-south-barrington", "Illinois", "South Barrington"),
    ("AMC Streets Of Woodfield 20", "AMC Streets Of Woodfield 20", "amc-streets-of-woodfield-20", "Illinois", "Schaumburg"),
    ("Cinemark Century Deer Park 16", "Cinemark Century Deer Park 16", "cinemark-century-deer-park-16", "Illinois", "Deer Park"),
    ("Cinemark Spring Hill Mall", "Cinemark Spring Hill Mall", "cinemark-spring-hill-mall", "Illinois", "West Dundee"),
    ("Cinergy Wheeling 7", "Cinergy Wheeling 7", "cinergy-wheeling-7", "Illinois", "Wheeling"),
    ("Classic Cinemas Carpentersville", "Classic Cinemas Carpentersville", "classic-cinemas-carpentersville", "Illinois", "Carpentersville"),
    ("Classic Cinemas Elk Grove", "Classic Cinemas Elk Grove", "classic-cinemas-elk-grove", "Illinois", "Elk Grove"),
    ("Picture Show at Bloomingdale Court", "Picture Show at Bloomingdale Court", "picture-show-at-bloomingdale-court", "Illinois", "Bloomingdale")
  ))
  private def p_south_boston: R = ("south-boston", "south-boston-virginia", "South Boston", "Virginia", 36.6987, -78.9014, "America/New_York", false, Seq("South Boston"), Seq(
    ("Cinema South Boston", "Cinema South Boston", "cinema-south-boston", "Virginia", "South Boston")
  ))
  private def p_south_fallsburg: R = ("south-fallsburg", "south-fallsburg-new-york", "South Fallsburg", "New York", 41.708, -74.632, "America/New_York", true, Seq("South Fallsburg", "Callicoon", "East Middletown"), Seq(
    ("Callicoon Theatre", "Callicoon Theatre", "callicoon-theatre", "New York", "Callicoon"),
    ("Hurleyville Arts Centre", "Hurleyville Arts Centre", "hurleyville-arts-centre", "New York", "Hurleyville"),
    ("Fair Oaks Drive In Middletown", "Fair Oaks Drive In Middletown", "fair-oaks-drive-in-middletown", "New York", "East Middletown")
  ))
  private def p_south_jordan: R = ("south-jordan", "south-jordan-utah", "South Jordan", "Utah", 40.5622, -111.9297, "America/Denver", true, Seq("South Jordan", "Taylorsville", "Draper", "Riverton", "Holladay", "Bluffdale"), Seq(
    ("Cinemark Draper", "Cinemark Draper", "cinemark-draper", "Utah", "Draper"),
    ("Cinemark Riverton Ridgewood 14", "Cinemark Riverton Ridgewood 14", "cinemark-riverton-ridgewood-14", "Utah", "Riverton"),
    ("Fatcats Bluffdale", "Fatcats Bluffdale", "fatcats-bluffdale", "Utah", "Bluffdale"),
    ("Megaplex Cottonwood Holladay", "Megaplex Cottonwood Holladay", "megaplex-cottonwood-holladay", "Utah", "Holladay"),
    ("Megaplex South Jordan at Daybreak", "Megaplex South Jordan at Daybreak", "megaplex-south-jordan-at-daybreak", "Utah", "South Jordan"),
    ("Megaplex The District", "Megaplex The District", "megaplex-the-district", "Utah", "South Jordan"),
    ("Regal Crossroads Taylorsville", "Regal Crossroads Taylorsville", "regal-crossroads-taylorsville", "Utah", "Taylorsville")
  ))
  private def p_south_kingstown: R = ("south-kingstown", "south-kingstown-rhode-island", "South Kingstown", "Rhode Island", 41.4472, -71.5249, "America/New_York", true, Seq("South Kingstown", "Westerly", "New Shoreham"), Seq(
    ("All South County Cinemas", "All South County Cinemas", "all-south-county-cinemas", "Rhode Island", "South Kingstown"),
    ("United Theatre Westerly", "United Theatre Westerly", "the-united-theatre", "Rhode Island", "Westerly"),
    ("Empire Theatre New Shoreham", "Empire Theatre New Shoreham", "empire-theatre-new-shoreham", "Rhode Island", "New Shoreham")
  ))
  private def p_south_taft: R = ("south-taft", "south-taft-california", "South Taft", "California", 35.1347, -119.4562, "America/Los_Angeles", false, Seq("South Taft"), Seq(
    ("Fox Theatre Taft", "Fox Theatre Taft", "fox-theatre-taft", "California", "South Taft")
  ))
  private def p_southern_pines: R = ("southern-pines", "southern-pines-north-carolina", "Southern Pines", "North Carolina", 35.1741, -79.3923, "America/New_York", false, Seq("Southern Pines"), Seq(
    ("Sandhills Stadium 10", "Sandhills Stadium 10", "sandhills-stadium-10", "North Carolina", "Southern Pines"),
    ("Sunrise Theater", "Sunrise Theater", "sunrise-theater", "North Carolina", "Southern Pines")
  ))
  private def p_southport: R = ("southport", "southport-north-carolina", "Southport", "North Carolina", 33.9182, -78.0193, "America/New_York", false, Seq("Southport"), Seq(
    ("Surf Cinema Southport", "Surf Cinema Southport", "surf-cinema-southport", "North Carolina", "Southport")
  ))
  private def p_spanish_fork: R = ("spanish-fork", "spanish-fork-utah", "Spanish Fork", "Utah", 40.115, -111.6549, "America/Denver", true, Seq("Spanish Fork", "Payson"), Seq(
    ("Cinemark Spanish Fork", "Cinemark Spanish Fork", "cinemark-spanish-fork", "Utah", "Spanish Fork"),
    ("Wayfarer Theaters Payson", "Wayfarer Theaters Payson", "payson-stadium-cinema", "Utah", "Payson")
  ))
  private def p_sparks: R = ("sparks", "sparks-nevada", "Sparks", "Nevada", 39.5349, -119.7527, "America/Los_Angeles", false, Seq("Sparks"), Seq(
    ("Galaxy Legends Sparks", "Galaxy Legends Sparks", "galaxy-legends-sparks", "Nevada", "Sparks"),
    ("Galaxy Victorian Sparks", "Galaxy Victorian Sparks", "galaxy-victorian-sparks", "Nevada", "Sparks")
  ))
  private def p_sparta: R = ("sparta", "sparta-new-jersey", "Sparta", "New Jersey", 41.0334, -74.6385, "America/New_York", true, Seq("Sparta", "Morristown", "Hackettstown", "Rockaway"), Seq(
    ("AMC Headquarters Plaza 10", "AMC Headquarters Plaza 10", "amc-headquarters-plaza-10", "New Jersey", "Morristown"),
    ("AMC Rockaway 16", "AMC Rockaway 16", "amc-rockaway-16", "New Jersey", "Rockaway"),
    ("Box Office Cinemas Sparta", "Box Office Cinemas Sparta", "box-office-cinemas-sparta", "New Jersey", "Sparta"),
    ("Cinepolis Mansfield", "Cinepolis Mansfield", "cinepolis-mansfield", "New Jersey", "Hackettstown")
  ))
  private def p_sparta_wisconsin: R = ("sparta-wisconsin", "sparta-wisconsin", "Sparta", "Wisconsin", 43.9441, -90.8129, "America/Chicago", true, Seq("Sparta", "Tomah"), Seq(
    ("Ho-Chunk Cinema", "Ho-Chunk Cinema", "ho-chunk-cinema", "Wisconsin", "Tomah"),
    ("Sparta Cinema", "Sparta Cinema", "sparta-cinema", "Wisconsin", "Sparta")
  ))
  private def p_spartanburg: R = ("spartanburg", "spartanburg-south-carolina", "Spartanburg", "South Carolina", 34.9496, -81.9321, "America/New_York", false, Seq("Spartanburg"), Seq(
    ("NCG Spartanburg", "NCG Spartanburg", "ncg-spartanburg", "South Carolina", "Spartanburg"),
    ("Regal Spartan Spartanburg", "Regal Spartan Spartanburg", "regal-spartan-spartanburg", "South Carolina", "Spartanburg")
  ))
  private def p_spearfish: R = ("spearfish", "spearfish-south-dakota", "Spearfish", "South Dakota", 44.4908, -103.8594, "America/Denver", false, Seq("Spearfish"), Seq(
    ("Northern Hills Cinema Spearfish", "Northern Hills Cinema Spearfish", "northern-hills-cinema-spearfish", "South Dakota", "Spearfish")
  ))
  private def p_spearman: R = ("spearman", "spearman-texas", "Spearman", "Texas", 36.1984, -101.1924, "America/Chicago", false, Seq("Spearman"), Seq(
    ("Lyric Cinema Spearman", "Lyric Cinema Spearman", "lyric-cinema-spearman", "Texas", "Spearman")
  ))
  private def p_spencer: R = ("spencer", "spencer-iowa", "Spencer", "Iowa", 43.1414, -95.1444, "America/Chicago", true, Seq("Spencer", "Emmetsburg", "Milford", "Arnolds Park"), Seq(
    ("Riviera Emmetsburg", "Riviera Emmetsburg", "riviera-emmetsburg", "Iowa", "Emmetsburg"),
    ("Odyssey Great Lakes Cinema", "Odyssey Great Lakes Cinema", "odyssey-great-lakes-cinema", "Iowa", "Okoboji"),
    ("Pioneer Theatre Milford", "Pioneer Theatre Milford", "pioneer-theatre-milford", "Iowa", "Milford"),
    ("The Palms Theatres Southpark", "The Palms Theatres Southpark", "the-palms-theatres-southpark", "Iowa", "Spencer")
  ))
  private def p_spencer_indiana: R = ("spencer-indiana", "spencer-indiana", "Spencer", "Indiana", 39.2867, -86.7625, "America/Indiana/Indianapolis", false, Seq("Spencer"), Seq(
    ("Tivoli Theatre Spencer", "Tivoli Theatre Spencer", "tivoli-theatre-spencer", "Indiana", "Spencer"),
    ("Cinema 67 Drive-in Spencer", "Cinema 67 Drive-in Spencer", "cinema-67-drive-in-spencer", "Indiana", "Spencer")
  ))
  private def p_spencer_west_virginia: R = ("spencer-west-virginia", "spencer-west-virginia", "Spencer", "West Virginia", 38.802, -81.3509, "America/New_York", false, Seq("Spencer"), Seq(
    ("Robey Theatre", "Robey Theatre", "robey-theatre", "West Virginia", "Spencer")
  ))
  private def p_spirit_lake: R = ("spirit-lake", "spirit-lake-iowa", "Spirit Lake", "Iowa", 43.4222, -95.1022, "America/Chicago", true, Seq("Spirit Lake", "Jackson"), Seq(
    ("Superior 71 Drive-In Spirit Lake", "Superior 71 Drive-In Spirit Lake", "superior-71-drive-in-spirit-lake", "Iowa", "Spirit Lake"),
    ("Historic State Theatre Jackson", "Historic State Theatre Jackson", "historic-state-theatre-jackson", "Minnesota", "Jackson")
  ))
  private def p_spring: R = ("spring", "spring-texas", "Spring", "Texas", 30.0799, -95.4172, "America/Chicago", false, Seq("Spring"), Seq(
    ("AMC Spring 10", "AMC Spring 10", "amc-spring-10", "Texas", "Spring"),
    ("Cinemark Spring-Klein", "Cinemark Spring-Klein", "cinemark-spring-klein", "Texas", "Spring"),
    ("Regal Benders Landing", "Regal Benders Landing", "regal-benders-landing", "Texas", "Spring"),
    ("Star Cinema Grill Springwoods", "Star Cinema Grill Springwoods", "star-cinema-grill-springwoods", "Texas", "Spring")
  ))
  private def p_spring_hill: R = ("spring-hill", "spring-hill-florida", "Spring Hill", "Florida", 28.4769, -82.5255, "America/New_York", true, Seq("Spring Hill", "New Port Richey", "Brooksville", "Port Richey"), Seq(
    ("Cinema Port Richey 6", "Cinema Port Richey 6", "cinema-port-richey-6", "Florida", "New Port Richey"),
    ("GTC Beacon Brooksville Cinemas", "GTC Beacon Brooksville Cinemas", "gtc-beacon-brooksville-cinemas", "Florida", "Brooksville"),
    ("Regal Hollywood Port Richey", "Regal Hollywood Port Richey", "regal-hollywood-port-richey", "Florida", "Port Richey"),
    ("Touchstar Cinemas Spring Hill 8", "Touchstar Cinemas Spring Hill 8", "touchstar-cinemas---spring-hill-8", "Florida", "Spring Hill")
  ))
  private def p_spring_hill_tennessee: R = ("spring-hill-tennessee", "spring-hill-tennessee", "Spring Hill", "Tennessee", 35.7512, -86.93, "America/Chicago", false, Seq("Spring Hill"), Seq(
    ("AMC CLASSIC Spring Hill 12", "AMC CLASSIC Spring Hill 12", "amc-classic-spring-hill-12", "Tennessee", "Spring Hill")
  ))
  private def p_springerville: R = ("springerville", "springerville-arizona", "Springerville", "Arizona", 34.1335, -109.2883, "America/Phoenix", false, Seq("Springerville"), Seq(
    ("El Rio Theatre Springerville", "El Rio Theatre Springerville", "el-rio-theatre-springerville", "Arizona", "Springerville")
  ))
  private def p_springfield_colorado: R = ("springfield-colorado", "springfield-colorado", "Springfield", "Colorado", 37.4083, -102.6144, "America/Denver", false, Seq("Springfield"), Seq(
    ("Capitol Theatre Springfield", "Capitol Theatre Springfield", "capitol-theatre-springfield", "Colorado", "Springfield")
  ))
  private def p_springfield_georgia: R = ("springfield-georgia", "springfield-georgia", "Springfield", "Georgia", 32.3724, -81.3115, "America/New_York", false, Seq("Springfield"), Seq(
    ("Mars Theatre Springfield", "Mars Theatre Springfield", "mars-theatre-springfield", "Georgia", "Springfield")
  ))
  private def p_springfield_ohio: R = ("springfield-ohio", "springfield-ohio", "Springfield", "Ohio", 39.9242, -83.8088, "America/New_York", true, Seq("Springfield", "Urbana", "London"), Seq(
    ("State Theater London", "State Theater London", "state-theater-london", "Ohio", "London"),
    ("Gloria Theatre Urbana", "Gloria Theatre Urbana", "gloria-theatre-urbana", "Ohio", "Urbana"),
    ("UEC Theatres Springfield", "UEC Theatres Springfield", "uec-theatres-springfield", "Ohio", "Springfield")
  ))
  private def p_springfield_oregon: R = ("springfield-oregon", "springfield-oregon", "Springfield", "Oregon", 44.0462, -123.022, "America/Los_Angeles", false, Seq("Springfield"), Seq(
    ("Cinemark Springfield 17", "Cinemark Springfield 17", "cinemark-springfield-17", "Oregon", "Springfield")
  ))
  private def p_springfield_vermont: R = ("springfield-vermont", "springfield-vermont", "Springfield", "Vermont", 43.2984, -72.4823, "America/New_York", true, Seq("Springfield", "Bellows Falls"), Seq(
    ("Bellows Falls Opera House", "Bellows Falls Opera House", "bellows-falls-opera-house", "Vermont", "Bellows Falls"),
    ("Springfield Cinemas", "Springfield Cinemas", "springfield-cinemas", "Vermont", "Springfield")
  ))
  private def p_springhill: R = ("springhill", "springhill-louisiana", "Springhill", "Louisiana", 33.006, -93.4668, "America/Chicago", false, Seq("Springhill"), Seq(
    ("Spring Theatre Springhill", "Spring Theatre Springhill", "spring-theatre-springhill", "Louisiana", "Springhill")
  ))
  private def p_springville: R = ("springville", "springville-new-york", "Springville", "New York", 42.5084, -78.6672, "America/New_York", true, Seq("Springville", "Delevan"), Seq(
    ("Joylan Theatre Springville", "Joylan Theatre Springville", "joylan-theatre-springville", "New York", "New Springville"),
    ("Delevan Twin Drive-In", "Delevan Twin Drive-In", "delevan-twin-drive-in", "New York", "Delevan")
  ))
  private def p_st_johns: R = ("st-johns", "st-johns-florida", "St. Johns", "Florida", 30.0815, -81.5477, "America/New_York", true, Seq("St. Johns", "Fleming Island", "Jacksonville Beach", "Saint Augustine", "Orange Park"), Seq(
    ("AMC Fleming Island 12", "AMC Fleming Island 12", "amc-fleming-island-12", "Florida", "Fleming Island"),
    ("AMC Orange Park 24", "AMC Orange Park 24", "amc-orange-park-24", "Florida", "Orange Park"),
    ("Cinemark Atlantic North", "Cinemark Atlantic North", "cinemark-atlantic-north", "Florida", "Jacksonville Beach"),
    ("Cinemark Durbin Park", "Cinemark Durbin Park", "cinemark-durbin-park", "Florida", "Saint Johns Park"),
    ("Epic Theatres St. Augustine", "Epic Theatres St. Augustine", "epic-theatres-st-augustine", "Florida", "Saint Augustine")
  ))
  private def p_st_johnsbury: R = ("st-johnsbury", "st-johnsbury-vermont", "St Johnsbury", "Vermont", 44.4253, -72.0151, "America/New_York", true, Seq("St Johnsbury", "Littleton"), Seq(
    ("Jax Jr Cinemas Littleton", "Jax Jr Cinemas Littleton", "jax-jr-cinemas-littleton", "New Hampshire", "Littleton"),
    ("Star Theatre St Johnsbury", "Star Theatre St Johnsbury", "star-theatre-st-johnsbury", "Vermont", "St Johnsbury")
  ))
  private def p_stafford: R = ("stafford", "stafford-kansas", "Stafford", "Kansas", 37.9616, -98.6004, "America/Chicago", false, Seq("Stafford"), Seq(
    ("Ritz Theatre Stafford", "Ritz Theatre Stafford", "ritz-theatre-stafford", "Kansas", "Stafford")
  ))
  private def p_stamford_texas: R = ("stamford-texas", "stamford-texas", "Stamford", "Texas", 32.9454, -99.8029, "America/Chicago", false, Seq("Stamford"), Seq(
    ("Grand Theatre Stamford", "Grand Theatre Stamford", "grand-theatre-stamford", "Texas", "Stamford")
  ))
  private def p_stanley: R = ("stanley", "stanley-north-dakota", "Stanley", "North Dakota", 48.3172, -102.3905, "America/Chicago", false, Seq("Stanley"), Seq(
    ("Regis Theatre", "Regis Theatre", "regis-theatre", "North Dakota", "Stanley")
  ))
  private def p_starke: R = ("starke", "starke-florida", "Starke", "Florida", 29.9441, -82.1098, "America/New_York", false, Seq("Starke"), Seq(
    ("Florida Twin Theatre", "Florida Twin Theatre", "florida-twin-theatre", "Florida", "Starke")
  ))
  private def p_starkville: R = ("starkville", "starkville-mississippi", "Starkville", "Mississippi", 33.4505, -88.8196, "America/Chicago", false, Seq("Starkville"), Seq(
    ("UEC Theatres Starkville", "UEC Theatres Starkville", "uec-theatres-starkville", "Mississippi", "Starkville")
  ))
  private def p_state_college: R = ("state-college", "state-college-pennsylvania", "State College", "Pennsylvania", 40.7934, -77.86, "America/New_York", false, Seq("State College"), Seq(
    ("State Theatre (State College)", "State Theatre (State College)", "state-theatre-state-college", "Pennsylvania", "State College"),
    ("UEC Theatres College 9", "UEC Theatres College 9", "uec-theatres-college-9", "Pennsylvania", "State College"),
    ("UEC Theatres State College 12", "UEC Theatres State College 12", "uec-theatres-state-college-12", "Pennsylvania", "State College")
  ))
  private def p_statesboro: R = ("statesboro", "statesboro-georgia", "Statesboro", "Georgia", 32.4488, -81.7832, "America/New_York", false, Seq("Statesboro"), Seq(
    ("AMC CLASSIC Statesboro 12", "AMC CLASSIC Statesboro 12", "amc-classic-statesboro-12", "Georgia", "Statesboro")
  ))
  private def p_steamboat_springs: R = ("steamboat-springs", "steamboat-springs-colorado", "Steamboat Springs", "Colorado", 40.485, -106.8317, "America/Denver", false, Seq("Steamboat Springs"), Seq(
    ("Wildhorse Cinema + Arts", "Wildhorse Cinema + Arts", "wildhorse-cinema-arts", "Colorado", "Steamboat Springs")
  ))
  private def p_stephenville: R = ("stephenville", "stephenville-texas", "Stephenville", "Texas", 32.2207, -98.2023, "America/Chicago", false, Seq("Stephenville"), Seq(
    ("Cinemark Stephenville 6", "Cinemark Stephenville 6", "cinemark-stephenville-6", "Texas", "Stephenville")
  ))
  private def p_sterling: R = ("sterling", "sterling-colorado", "Sterling", "Colorado", 40.6255, -103.2077, "America/Denver", false, Seq("Sterling"), Seq(
    ("Fox 5 Theatre Sterling", "Fox 5 Theatre Sterling", "fox-5-theatre-sterling", "Colorado", "Sterling")
  ))
  private def p_sterling_illinois: R = ("sterling-illinois", "sterling-illinois", "Sterling", "Illinois", 41.7886, -89.6962, "America/Chicago", true, Seq("Sterling", "Mount Sterling"), Seq(
    ("AMC CLASSIC Sauk Valley 8", "AMC CLASSIC Sauk Valley 8", "amc-classic-sauk-valley-8", "Illinois", "Sterling"),
    ("Sterling Theater", "Sterling Theater", "sterling-theater", "Illinois", "Sterling"),
    ("Midway Drive-in Sterling", "Midway Drive-in Sterling", "midway-drive-in-sterling", "Illinois", "Mount Sterling")
  ))
  private def p_stevens_point: R = ("stevens-point", "stevens-point-wisconsin", "Stevens Point", "Wisconsin", 44.5236, -89.5746, "America/Chicago", true, Seq("Stevens Point", "Wisconsin Rapids"), Seq(
    ("Rogers Cinema Stevens Point 7", "Rogers Cinema Stevens Point 7", "rogers-cinema-stevens-point-7", "Wisconsin", "Stevens Point"),
    ("Rogers Cinema Wisconsin Rapids", "Rogers Cinema Wisconsin Rapids", "rogers-cinema-wisconsin-rapids", "Wisconsin", "Wisconsin Rapids")
  ))
  private def p_stigler: R = ("stigler", "stigler-oklahoma", "Stigler", "Oklahoma", 35.2537, -95.123, "America/Chicago", false, Seq("Stigler"), Seq(
    ("Time Theatre Stigler", "Time Theatre Stigler", "time-theatre-stigler", "Oklahoma", "Stigler")
  ))
  private def p_stillwater: R = ("stillwater", "stillwater-oklahoma", "Stillwater", "Oklahoma", 36.1156, -97.0584, "America/Chicago", true, Seq("Stillwater", "Cushing"), Seq(
    ("AMC Stillwater 10", "AMC Stillwater 10", "amc-stillwater-10", "Oklahoma", "Stillwater"),
    ("The Dunkin Theatre", "The Dunkin Theatre", "the-dunkin-theatre", "Oklahoma", "Cushing")
  ))
  private def p_stockton: R = ("stockton", "stockton-kansas", "Stockton", "Kansas", 39.4381, -99.2651, "America/Chicago", false, Seq("Stockton"), Seq(
    ("Nova Theatre Stockton", "Nova Theatre Stockton", "nova-theatre-stockton", "Kansas", "Stockton")
  ))
  private def p_stony_brook: R = ("stony-brook", "stony-brook-new-york", "Stony Brook", "New York", 40.9256, -73.1409, "America/New_York", true, Seq("Stony Brook", "Lake Ronkonkoma", "Holtsville", "Port Jefferson Station"), Seq(
    ("AMC Stony Brook 17", "AMC Stony Brook 17", "amc-stony-brook-17", "New York", "Stony Brook"),
    ("Staller Center For the Arts", "Staller Center For the Arts", "staller-center-for-the-arts", "New York", "Stony Brook"),
    ("Island 16 Holtsville", "Island 16 Holtsville", "island-16-holtsville", "New York", "Holtsville"),
    ("PJ Cinemas Port Jefferson 7", "PJ Cinemas Port Jefferson 7", "pj-cinemas-port-jefferson-7", "New York", "Port Jefferson Station"),
    ("Regal Ronkonkoma", "Regal Ronkonkoma", "regal-ronkonkoma", "New York", "Lake Ronkonkoma")
  ))
  private def p_storm_lake: R = ("storm-lake", "storm-lake-iowa", "Storm Lake", "Iowa", 42.6411, -95.2097, "America/Chicago", true, Seq("Storm Lake", "Cherokee", "Holstein", "Paullina"), Seq(
    ("Fridley American 3 Cherokee", "Fridley American 3 Cherokee", "fridley-american-3-cherokee", "Iowa", "Cherokee"),
    ("Vista 3 Storm Lake", "Vista 3 Storm Lake", "vista-3-storm-lake", "Iowa", "Storm Lake"),
    ("State Theatre Holstein", "State Theatre Holstein", "state-theatre-holstein", "Iowa", "Holstein"),
    ("Wonderland Theatre Paullina", "Wonderland Theatre Paullina", "wonderland-theatre-paullina", "Iowa", "Paullina")
  ))
  private def p_stowe: R = ("stowe", "stowe-vermont", "Stowe", "Vermont", 44.4653, -72.6846, "America/New_York", true, Seq("Stowe", "Morrisville"), Seq(
    ("Bijou Cineplex Morrisville", "Bijou Cineplex Morrisville", "bijou-cineplex-morrisville", "Vermont", "Morrisville"),
    ("Stowe CinemaPlex", "Stowe CinemaPlex", "stowe-cinemaplex", "Vermont", "Stowe")
  ))
  private def p_strongsville: R = ("strongsville", "strongsville-ohio", "Strongsville", "Ohio", 41.3145, -81.8357, "America/New_York", true, Seq("Strongsville", "Brunswick", "Medina"), Seq(
    ("Hickory Ridge Cinemas", "Hickory Ridge Cinemas", "hickory-ridge-cinemas", "Ohio", "Brunswick"),
    ("Regal Medina Stadium", "Regal Medina Stadium", "regal-medina-stadium", "Ohio", "Medina"),
    ("Cinemark Strongsville 14", "Cinemark Strongsville 14", "cinemark-strongsville-14", "Ohio", "Strongsville")
  ))
  private def p_stuart: R = ("stuart", "stuart-nebraska", "Stuart", "Nebraska", 42.5996, -99.1423, "America/Chicago", false, Seq("Stuart"), Seq(
    ("Murphy Theatre and Media Center", "Murphy Theatre and Media Center", "murphy-theatre-and-media-center", "Nebraska", "Stuart")
  ))
  private def p_stuart_florida: R = ("stuart-florida", "stuart-florida", "Stuart", "Florida", 27.1975, -80.2528, "America/New_York", true, Seq("Stuart", "Fort Pierce", "Jensen Beach"), Seq(
    ("Epic Theatres Stuart", "Epic Theatres Stuart", "epic-theatres-stuart", "Florida", "Stuart"),
    ("Lyric Theatre Stuart", "Lyric Theatre Stuart", "lyric-theatre-stuart", "Florida", "Stuart"),
    ("Regal Treasure Coast Jensen Beach", "Regal Treasure Coast Jensen Beach", "regal-treasure-coast-jensen-beach", "Florida", "Jensen Beach"),
    ("Sabal Palms Luxury 6", "Sabal Palms Luxury 6", "sabal-palms-luxury-6", "Florida", "Fort Pierce")
  ))
  private def p_sturgeon_bay: R = ("sturgeon-bay", "sturgeon-bay-wisconsin", "Sturgeon Bay", "Wisconsin", 44.8342, -87.377, "America/Chicago", false, Seq("Sturgeon Bay"), Seq(
    ("Sturgeon Bay Cinemas", "Sturgeon Bay Cinemas", "sturgeon-bay-cinemas", "Wisconsin", "Sturgeon Bay")
  ))
  private def p_sturgis: R = ("sturgis", "sturgis-michigan", "Sturgis", "Michigan", 41.7992, -85.4192, "America/Detroit", true, Seq("Sturgis", "Angola", "Edgerton"), Seq(
    ("Brokaw Movie House", "Brokaw Movie House", "brokaw-movie-house", "Indiana", "Angola"),
    ("Fleo's Flick's Drive-In Theatre", "Fleo's Flick's Drive-In Theatre", "fleos-flicks-drive-in-theatre", "Ohio", "Edgerton"),
    ("Strand Theatre Sturgis", "Strand Theatre Sturgis", "strand-theatre-sturgis", "Michigan", "Sturgis")
  ))
  private def p_stuttgart: R = ("stuttgart", "stuttgart-arkansas", "Stuttgart", "Arkansas", 34.5004, -91.5526, "America/Chicago", false, Seq("Stuttgart"), Seq(
    ("Stuttgart Twin Cinema", "Stuttgart Twin Cinema", "stuttgart-twin-cinema", "Arkansas", "Stuttgart")
  ))
  private def p_suffern: R = ("suffern", "suffern-new-york", "Suffern", "New York", 41.1148, -74.1496, "America/New_York", true, Seq("Suffern", "Warwick"), Seq(
    ("Lafayette Theatre", "Lafayette Theatre", "lafayette-theatre", "New York", "Suffern"),
    ("Warwick Drive-In Theater", "Warwick Drive-In Theater", "warwick-drive-in-theater", "New York", "Warwick")
  ))
  private def p_suffolk: R = ("suffolk", "suffolk-virginia", "Suffolk", "Virginia", 36.7284, -76.585, "America/New_York", false, Seq("Suffolk"), Seq(
    ("Regal Harbour View Grande Suffolk", "Regal Harbour View Grande Suffolk", "regal-harbour-view-grande-suffolk", "Virginia", "Suffolk")
  ))
  private def p_sullivan: R = ("sullivan", "sullivan-missouri", "Sullivan", "Missouri", 38.2081, -91.1604, "America/Chicago", false, Seq("Sullivan"), Seq(
    ("Sullivan 6 Cinema", "Sullivan 6 Cinema", "sullivan-6-cinema", "Missouri", "Sullivan")
  ))
  private def p_sulphur_springs: R = ("sulphur-springs", "sulphur-springs-texas", "Sulphur Springs", "Texas", 33.1384, -95.6011, "America/Chicago", false, Seq("Sulphur Springs"), Seq(
    ("Bright Star Cinemas", "Bright Star Cinemas", "bright-star-cinemas", "Texas", "Sulphur Springs")
  ))
  private def p_summerlin_south: R = ("summerlin-south", "summerlin-south-nevada", "Summerlin South", "Nevada", 36.1171, -115.33, "America/Los_Angeles", false, Seq("Summerlin South"), Seq(
    ("Regal Downtown Summerlin Las Vegas", "Regal Downtown Summerlin Las Vegas", "regal-downtown-summerlin-las-vegas", "Nevada", "East Las Vegas")
  ))
  private def p_summersville: R = ("summersville", "summersville-west-virginia", "Summersville", "West Virginia", 38.2812, -80.8526, "America/New_York", false, Seq("Summersville"), Seq(
    ("Marquee Summersville 4", "Marquee Summersville 4", "marquee-summersville-4", "West Virginia", "Summersville")
  ))
  private def p_summerville: R = ("summerville", "summerville-south-carolina", "Summerville", "South Carolina", 33.0185, -80.1757, "America/New_York", false, Seq("Summerville"), Seq(
    ("Cinemark Summerville 8", "Cinemark Summerville 8", "cinemark-summerville-8", "South Carolina", "Summerville"),
    ("Regal Azalea Square Summerville", "Regal Azalea Square Summerville", "regal-azalea-square-summerville", "South Carolina", "Summerville")
  ))
  private def p_sumter: R = ("sumter", "sumter-south-carolina", "Sumter", "South Carolina", 33.9204, -80.3415, "America/New_York", false, Seq("Sumter"), Seq(
    ("GTC Beacon Sumter Cinemas", "GTC Beacon Sumter Cinemas", "gtc-beacon-sumter-cinemas", "South Carolina", "Sumter")
  ))
  private def p_sun_prairie: R = ("sun-prairie", "sun-prairie-wisconsin", "Sun Prairie", "Wisconsin", 43.1836, -89.2137, "America/Chicago", true, Seq("Sun Prairie", "Watertown", "Beaver Dam", "Jefferson", "Delafield", "Johnson Creek"), Seq(
    ("Beaver Dam Cinema", "Beaver Dam Cinema", "beaver-dam-cinema", "Wisconsin", "Beaver Dam"),
    ("Highway 18 Outdoor Theatre Jefferson", "Highway 18 Outdoor Theatre Jefferson", "highway-18-outdoor-theatre-jefferson", "Wisconsin", "Jefferson"),
    ("Marcus Palace Cinema Sun Prairie", "Marcus Palace Cinema Sun Prairie", "marcus-palace-cinema-sun-prairie", "Wisconsin", "Sun Prairie"),
    ("AMC CLASSIC Johnson Creek 12", "AMC CLASSIC Johnson Creek 12", "amc-classic-johnson-creek-12", "Wisconsin", "Johnson Creek"),
    ("Marcus Hillside Delafield", "Marcus Hillside Delafield", "marcus-hillside-delafield", "Wisconsin", "Delafield"),
    ("Towne Cinema", "Towne Cinema", "towne-cinema", "Wisconsin", "Watertown")
  ))
  private def p_sunnyside: R = ("sunnyside", "sunnyside-washington", "Sunnyside", "Washington", 46.3237, -120.0087, "America/Los_Angeles", true, Seq("Sunnyside", "Toppenish", "Prosser"), Seq(
    ("Grand Cinemas Yakima Valley", "Grand Cinemas Yakima Valley", "grand-cinemas-yakima-valley", "Washington", "Sunnyside"),
    ("Heritage Theater Toppenish", "Heritage Theater Toppenish", "heritage-theater-toppenish", "Washington", "Toppenish"),
    ("Princess Theatre Prosser", "Princess Theatre Prosser", "princess-theatre-prosser", "Washington", "Prosser")
  ))
  private def p_sunrise: R = ("sunrise", "sunrise-florida", "Sunrise", "Florida", 26.134, -80.1131, "America/New_York", false, Seq("Sunrise"), Seq(
    ("AMC Sunrise 8", "AMC Sunrise 8", "amc-sunrise-8", "Florida", "Sunrise"),
    ("AMC Weston 8", "AMC Weston 8", "amc-weston-8", "Florida", "Sunrise"),
    ("Regal Sawgrass Sunrise", "Regal Sawgrass Sunrise", "regal-sawgrass-sunrise", "Florida", "Sunrise")
  ))
  private def p_superior: R = ("superior", "superior-nebraska", "Superior", "Nebraska", 40.0209, -98.07, "America/Chicago", true, Seq("Superior", "Mankato"), Seq(
    ("Crest Theatre Superior", "Crest Theatre Superior", "crest-theatre-superior", "Nebraska", "Superior"),
    ("Ute Theatre Mankato", "Ute Theatre Mankato", "ute-theatre-mankato", "Kansas", "Mankato")
  ))
  private def p_susanville: R = ("susanville", "susanville-california", "Susanville", "California", 40.4163, -120.653, "America/Los_Angeles", false, Seq("Susanville"), Seq(
    ("Uptown Cinemas Susanville", "Uptown Cinemas Susanville", "uptown-cinemas-susanville", "California", "Susanville"),
    ("Sierra Theatre Susanville", "Sierra Theatre Susanville", "sierra-theatre-susanville", "California", "Susanville")
  ))
  private def p_sutton: R = ("sutton", "sutton-west-virginia", "Sutton", "West Virginia", 38.6645, -80.7098, "America/New_York", false, Seq("Sutton"), Seq(
    ("Elk Theatre Sutton", "Elk Theatre Sutton", "elk-theatre-sutton", "West Virginia", "Sutton")
  ))
  private def p_syracuse_kansas: R = ("syracuse-kansas", "syracuse-kansas", "Syracuse", "Kansas", 37.9807, -101.7543, "America/Denver", false, Seq("Syracuse"), Seq(
    ("Northrup Theater Syracuse", "Northrup Theater Syracuse", "northrup-theater-syracuse", "Kansas", "Syracuse")
  ))
  private def p_tabor_city: R = ("tabor-city", "tabor-city-north-carolina", "Tabor City", "North Carolina", 34.1488, -78.8767, "America/New_York", false, Seq("Tabor City"), Seq(
    ("Stateline Movie Time", "Stateline Movie Time", "stateline-movie-time", "North Carolina", "Tabor City")
  ))
  private def p_tahlequah: R = ("tahlequah", "tahlequah-oklahoma", "Tahlequah", "Oklahoma", 35.9154, -94.97, "America/Chicago", false, Seq("Tahlequah"), Seq(
    ("Apex Cinema Tahlequah", "Apex Cinema Tahlequah", "apex-cinema-tahlequah", "Oklahoma", "Tahlequah")
  ))
  private def p_taos: R = ("taos", "taos-new-mexico", "Taos", "New Mexico", 36.4072, -105.5731, "America/Denver", true, Seq("Taos", "Ranchos de Taos"), Seq(
    ("Storyteller Cinema 7 Taos", "Storyteller Cinema 7 Taos", "storyteller-cinema-7-taos", "New Mexico", "Taos"),
    ("Taos Community Auditorium", "Taos Community Auditorium", "taos-community-auditorium", "New Mexico", "Ranchos de Taos")
  ))
  private def p_tavernier: R = ("tavernier", "tavernier-florida", "Tavernier", "Florida", 25.0115, -80.5151, "America/New_York", false, Seq("Tavernier"), Seq(
    ("Tavernier Cinema 5", "Tavernier Cinema 5", "tavernier-cinema-5", "Florida", "Tavernier")
  ))
  private def p_taylorville: R = ("taylorville", "taylorville-illinois", "Taylorville", "Illinois", 39.5489, -89.2945, "America/Chicago", true, Seq("Taylorville", "Pana"), Seq(
    ("Roseland Theatre Pana", "Roseland Theatre Pana", "roseland-theatre-pana", "Illinois", "Pana"),
    ("Taylorville Cinema", "Taylorville Cinema", "taylorville-cinema", "Illinois", "Taylorville")
  ))
  private def p_teays_valley: R = ("teays-valley", "teays-valley-west-virginia", "Teays Valley", "West Virginia", 38.4501, -81.9293, "America/New_York", true, Seq("Teays Valley", "South Charleston", "Cross Lanes"), Seq(
    ("Regal Nitro", "Regal Nitro", "regal-nitro", "West Virginia", "Cross Lanes"),
    ("Teays Valley Cinemas Teaneck", "Teays Valley Cinemas Teaneck", "teays-valley-cinemas-teaneck", "West Virginia", "Scott Depot"),
    ("LaBelle Theatre", "LaBelle Theatre", "labelle-theatre", "West Virginia", "South Charleston")
  ))
  private def p_tehachapi: R = ("tehachapi", "tehachapi-california", "Tehachapi", "California", 35.1322, -118.449, "America/Los_Angeles", false, Seq("Tehachapi"), Seq(
    ("Tehachapi Hitching Post Theater", "Tehachapi Hitching Post Theater", "tehachapi-hitching-post-theater", "California", "Tehachapi")
  ))
  private def p_telluride: R = ("telluride", "telluride-colorado", "Telluride", "Colorado", 37.9375, -107.8123, "America/Denver", true, Seq("Telluride", "Mountain Village", "Ouray"), Seq(
    ("Wright Opera House", "Wright Opera House", "wright-opera-house", "Colorado", "Ouray"),
    ("Nugget Theatre Telluride", "Nugget Theatre Telluride", "nugget-theatre-telluride", "Colorado", "Telluride"),
    ("Palm Arts", "Palm Arts", "palm-arts", "Colorado", "Telluride"),
    ("Telluride Conference Center", "Telluride Conference Center", "telluride-conference-center", "Colorado", "Mountain Village")
  ))
  private def p_terre_haute: R = ("terre-haute", "terre-haute-indiana", "Terre Haute", "Indiana", 39.4667, -87.4139, "America/Indiana/Indianapolis", false, Seq("Terre Haute"), Seq(
    ("AMC CLASSIC Terre Haute 12", "AMC CLASSIC Terre Haute 12", "amc-classic-terre-haute-12", "Indiana", "Terre Haute"),
    ("Moon Lite Drive-in", "Moon Lite Drive-in", "moon-lite-drive-in", "Indiana", "Terre Haute")
  ))
  private def p_texarkana: R = ("texarkana", "texarkana-texas", "Texarkana", "Texas", 33.4251, -94.0477, "America/Chicago", false, Seq("Texarkana"), Seq(
    ("Cinemark Texarkana 14", "Cinemark Texarkana 14", "cinemark-texarkana-14", "Texas", "Texarkana")
  ))
  private def p_the_colony: R = ("the-colony", "the-colony-texas", "The Colony", "Texas", 33.089, -96.8864, "America/Chicago", true, Seq("The Colony", "Addison"), Seq(
    ("AMC Village On The Parkway 9", "AMC Village On The Parkway 9", "amc-village-on-the-parkway-9", "Texas", "Addison"),
    ("Galaxy Theatres Grandscape", "Galaxy Theatres Grandscape", "galaxy-theatres-grandscape", "Texas", "The Colony"),
    ("COSM Grandscape", "COSM Grandscape", "cosm-grandscape", "Texas", "The Colony")
  ))
  private def p_the_villages: R = ("the-villages", "the-villages-florida", "The Villages", "Florida", 28.9341, -81.9599, "America/New_York", true, Seq("The Villages", "Leesburg"), Seq(
    ("Epic Old Mill Playhouse", "Epic Old Mill Playhouse", "epic-old-mill-playhouse", "Florida", "The Villages"),
    ("AMC Lake Square 12", "AMC Lake Square 12", "amc-lake-square-12", "Florida", "Leesburg")
  ))
  private def p_the_woodlands: R = ("the-woodlands", "the-woodlands-texas", "The Woodlands", "Texas", 30.158, -95.4894, "America/Chicago", true, Seq("The Woodlands", "Conroe", "Shenandoah", "Hockley"), Seq(
    ("AMC Metropark Square 10", "AMC Metropark Square 10", "amc-metropark-square-10", "Texas", "Shenandoah"),
    ("Cinemark The Woodlands", "Cinemark The Woodlands", "cinemark-the-woodlands", "Texas", "The Woodlands"),
    ("Reel Luxury Cinemas Woodlands", "Reel Luxury Cinemas Woodlands", "reel-luxury-cinemas-woodlands", "Texas", "The Woodlands"),
    ("Santikos Grand Conroe", "Santikos Grand Conroe", "santikos-grand-conroe", "Texas", "Conroe"),
    ("Xscape at 1488, Conroe", "Xscape at 1488, Conroe", "xscape-at-1488-conroe", "Texas", "Conroe"),
    ("Showboat Drive-in Hockley", "Showboat Drive-in Hockley", "showboat-drive-in-hockley", "Texas", "Hockley")
  ))
  private def p_thermopolis: R = ("thermopolis", "thermopolis-wyoming", "Thermopolis", "Wyoming", 43.6461, -108.212, "America/Denver", false, Seq("Thermopolis"), Seq(
    ("Micky's Movie Theater", "Micky's Movie Theater", "mickys-movie-theater", "Wyoming", "East Thermopolis")
  ))
  private def p_thief_river_falls: R = ("thief-river-falls", "thief-river-falls-minnesota", "Thief River Falls", "Minnesota", 48.1191, -96.1812, "America/Chicago", false, Seq("Thief River Falls"), Seq(
    ("Falls Cinema", "Falls Cinema", "falls-cinema", "Minnesota", "Thief River Falls")
  ))
  private def p_thomaston: R = ("thomaston", "thomaston-georgia", "Thomaston", "Georgia", 32.8882, -84.3266, "America/New_York", false, Seq("Thomaston"), Seq(
    ("Ritz Theatre Thomaston", "Ritz Theatre Thomaston", "ritz-theatre-thomaston", "Georgia", "Thomaston")
  ))
  private def p_thomasville: R = ("thomasville", "thomasville-georgia", "Thomasville", "Georgia", 30.8366, -83.9788, "America/New_York", true, Seq("Thomasville", "Cairo"), Seq(
    ("GTC Gateway Cinemas Thomasville", "GTC Gateway Cinemas Thomasville", "gtc-gateway-cinemas-thomasville", "Georgia", "Thomasville"),
    ("Zebulon Theatre Cairo", "Zebulon Theatre Cairo", "zebulon-theatre-cairo", "Georgia", "Cairo")
  ))
  private def p_thompson_falls: R = ("thompson-falls", "thompson-falls-montana", "Thompson Falls", "Montana", 47.5949, -115.3383, "America/Denver", false, Seq("Thompson Falls"), Seq(
    ("Rex Theater Stanley", "Rex Theater Stanley", "rex-theater-stanley", "Montana", "Thompson Falls")
  ))
  private def p_thomson: R = ("thomson", "thomson-georgia", "Thomson", "Georgia", 33.4707, -82.5046, "America/New_York", false, Seq("Thomson"), Seq(
    ("Thomson Twin Cinema", "Thomson Twin Cinema", "thomson-twin-cinema", "Georgia", "Thomson")
  ))
  private def p_three_rivers: R = ("three-rivers", "three-rivers-michigan", "Three Rivers", "Michigan", 41.9439, -85.6325, "America/Detroit", false, Seq("Three Rivers"), Seq(
    ("GQT Three Rivers 6", "GQT Three Rivers 6", "gqt-three-rivers-6", "Michigan", "Three Rivers"),
    ("Riviera Theatre Three Rivers", "Riviera Theatre Three Rivers", "riviera-theatre-three-rivers", "Michigan", "Three Rivers")
  ))
  private def p_tiffin: R = ("tiffin", "tiffin-ohio", "Tiffin", "Ohio", 41.1145, -83.178, "America/New_York", false, Seq("Tiffin"), Seq(
    ("Seneca Cinema and Event Center Tiffin", "Seneca Cinema and Event Center Tiffin", "seneca-cinema-and-event-center-tiffin", "Ohio", "Tiffin"),
    ("Tiffin Drive-In", "Tiffin Drive-In", "tiffin-drive-in", "Ohio", "Tiffin")
  ))
  private def p_tigard: R = ("tigard", "tigard-oregon", "Tigard", "Oregon", 45.4312, -122.7715, "America/Los_Angeles", true, Seq("Tigard", "Lake Oswego", "Oregon City", "Canby", "Clackamas"), Seq(
    ("Canby Cinema 8", "Canby Cinema 8", "canby-cinema-8", "Oregon", "Canby"),
    ("Cinemark Century Clackamas Town Center", "Cinemark Century Clackamas Town Center", "cinemark-century-clackamas-town-center", "Oregon", "Clackamas"),
    ("Joy Cinema & Pub Tigard", "Joy Cinema & Pub Tigard", "joy-cinema-pub-tigard", "Oregon", "Tigard"),
    ("Regal Bridgeport Village Tigard", "Regal Bridgeport Village Tigard", "regal-bridgeport-village-tigard", "Oregon", "Tigard"),
    ("Lake Twin Lake Oswego", "Lake Twin Lake Oswego", "lake-twin-lake-oswego", "Oregon", "Lake Oswego"),
    ("Regal Hilltop Oregon City", "Regal Hilltop Oregon City", "regal-hilltop-oregon-city", "Oregon", "Oregon City")
  ))
  private def p_tillamook: R = ("tillamook", "tillamook-oregon", "Tillamook", "Oregon", 45.4564, -123.8455, "America/Los_Angeles", false, Seq("Tillamook"), Seq(
    ("Coliseum Tillamook", "Coliseum Tillamook", "coliseum-tillamook", "Oregon", "Tillamook")
  ))
  private def p_tioga: R = ("tioga", "tioga-north-dakota", "Tioga", "North Dakota", 48.3972, -102.9382, "America/Chicago", false, Seq("Tioga"), Seq(
    ("Tioga Theatre", "Tioga Theatre", "tioga-theatre", "North Dakota", "Tioga")
  ))
  private def p_tiptonville: R = ("tiptonville", "tiptonville-tennessee", "Tiptonville", "Tennessee", 36.3784, -89.472, "America/Chicago", false, Seq("Tiptonville"), Seq(
    ("Strand Theatre of Tiptonville", "Strand Theatre of Tiptonville", "strand-theatre-of-tiptonville", "Tennessee", "Tiptonville")
  ))
  private def p_titusville: R = ("titusville", "titusville-florida", "Titusville", "Florida", 28.6122, -80.8076, "America/New_York", false, Seq("Titusville"), Seq(
    ("Epic Theatres Titusville", "Epic Theatres Titusville", "epic-theatres-titusville", "Florida", "Titusville")
  ))
  private def p_tomball: R = ("tomball", "tomball-texas", "Tomball", "Texas", 30.0972, -95.6161, "America/Chicago", false, Seq("Tomball"), Seq(
    ("Cinepolis Woodlands", "Cinepolis Woodlands", "cinepolis-woodlands", "Texas", "Tomball"),
    ("Regal Lone Star Tomball", "Regal Lone Star Tomball", "regal-lone-star-tomball", "Texas", "Tomball"),
    ("Tomball Premiere 7", "Tomball Premiere 7", "tomball-premiere-7", "Texas", "Tomball")
  ))
  private def p_toms_river: R = ("toms-river", "toms-river-new-jersey", "Toms River", "New Jersey", 39.9537, -74.1979, "America/New_York", true, Seq("Toms River", "Brick", "Manahawkin"), Seq(
    ("AMC Brick Plaza 10", "AMC Brick Plaza 10", "amc-brick-plaza-10", "New Jersey", "Brick"),
    ("Marquee Orchard Toms River 10", "Marquee Orchard Toms River 10", "marquee-orchard-toms-river-10", "New Jersey", "Toms River"),
    ("Regal Manahawkin Cinemas", "Regal Manahawkin Cinemas", "regal-manahawkin-cinemas", "New Jersey", "Manahawkin")
  ))
  private def p_tonkawa: R = ("tonkawa", "tonkawa-oklahoma", "Tonkawa", "Oklahoma", 36.6784, -97.31, "America/Chicago", false, Seq("Tonkawa"), Seq(
    ("The Hub Tonkawa", "The Hub Tonkawa", "the-hub-tonkawa", "Oklahoma", "Tonkawa")
  ))
  private def p_tooele: R = ("tooele", "tooele-utah", "Tooele", "Utah", 40.5308, -112.2983, "America/Denver", false, Seq("Tooele"), Seq(
    ("UEC Theatres Tooele", "UEC Theatres Tooele", "uec-theatres-tooele", "Utah", "Tooele"),
    ("Erda Drive In", "Erda Drive In", "erda-drive-in", "Utah", "Tooele"),
    ("Ritz Theatre Tooele", "Ritz Theatre Tooele", "ritz-theatre-tooele", "Utah", "Tooele")
  ))
  private def p_torrington: R = ("torrington", "torrington-connecticut", "Torrington", "Connecticut", 41.8006, -73.1212, "America/New_York", true, Seq("Torrington", "Winsted", "Canton Valley", "Litchfield"), Seq(
    ("Apple Cinemas Simsbury 8", "Apple Cinemas Simsbury 8", "apple-cinemas-simsbury-8", "Connecticut", "Simsbury"),
    ("Apple Cinemas Torrington 6", "Apple Cinemas Torrington 6", "apple-cinemas-torrington-6", "Connecticut", "Torrington"),
    ("Warner Theatre Torrington", "Warner Theatre Torrington", "warner-theatre-torrington", "Connecticut", "Torrington"),
    ("Bantam Cinema", "Bantam Cinema", "bantam-cinema", "Connecticut", "Bantam"),
    ("Gilson Café & Cinema Winsted", "Gilson Café & Cinema Winsted", "gilson-caf-cinema-winsted", "Connecticut", "Winsted")
  ))
  private def p_torrington_wyoming: R = ("torrington-wyoming", "torrington-wyoming", "Torrington", "Wyoming", 42.0625, -104.1844, "America/Denver", false, Seq("Torrington"), Seq(
    ("Torrington Cinemas", "Torrington Cinemas", "torrington-cinemas", "Wyoming", "South Torrington")
  ))
  private def p_tracy: R = ("tracy", "tracy-california", "Tracy", "California", 37.7399, -121.4262, "America/Los_Angeles", true, Seq("Tracy", "Manteca"), Seq(
    ("AMC Manteca 16", "AMC Manteca 16", "amc-manteca-16", "California", "Manteca"),
    ("Cinemark Tracy 14", "Cinemark Tracy 14", "cinemark-tracy-14", "California", "Tracy")
  ))
  private def p_traverse_city: R = ("traverse-city", "traverse-city-michigan", "Traverse City", "Michigan", 44.7631, -85.6206, "America/Detroit", false, Seq("Traverse City"), Seq(
    ("AMC Cherry Blossom 14", "AMC Cherry Blossom 14", "amc-cherry-blossom-14", "Michigan", "Traverse City"),
    ("State Theatre Traverse City", "State Theatre Traverse City", "state-theatre-traverse-city", "Michigan", "Traverse City"),
    ("Bijou by the Bay", "Bijou by the Bay", "bijou-by-the-bay", "Michigan", "Traverse City")
  ))
  private def p_trenton: R = ("trenton", "trenton-missouri", "Trenton", "Missouri", 40.0789, -93.6166, "America/Chicago", false, Seq("Trenton"), Seq(
    ("Trenton Cinema", "Trenton Cinema", "trenton-cinema", "Missouri", "Trenton")
  ))
  private def p_tribune: R = ("tribune", "tribune-kansas", "Tribune", "Kansas", 38.4697, -101.7527, "America/Denver", false, Seq("Tribune"), Seq(
    ("Star Tribune Tribune", "Star Tribune Tribune", "star-tribune-tribune", "Kansas", "Tribune")
  ))
  private def p_trinidad: R = ("trinidad", "trinidad-colorado", "Trinidad", "Colorado", 37.1695, -104.5005, "America/Denver", false, Seq("Trinidad"), Seq(
    ("Peak Cinemas", "Peak Cinemas", "peak-cinemas", "Colorado", "Trinidad")
  ))
  private def p_troy: R = ("troy", "troy-missouri", "Troy", "Missouri", 38.9795, -90.9807, "America/Chicago", true, Seq("Troy", "Elsberry"), Seq(
    ("Troy Movie House", "Troy Movie House", "troy-movie-house", "Missouri", "South Troy"),
    ("Senate Theatre Elsberry", "Senate Theatre Elsberry", "senate-theatre-elsberry", "Missouri", "Elsberry")
  ))
  private def p_truckee: R = ("truckee", "truckee-california", "Truckee", "California", 39.328, -120.1833, "America/Los_Angeles", true, Seq("Truckee", "Sunnyside-Tahoe City"), Seq(
    ("Tahoe Art Haus & Cinema", "Tahoe Art Haus & Cinema", "tahoe-art-haus-cinema", "California", "Sunnyside-Tahoe City"),
    ("Village Cinemas Truckee", "Village Cinemas Truckee", "village-cinemas-truckee", "California", "Truckee")
  ))
  private def p_truth_or_consequences: R = ("truth-or-consequences", "truth-or-consequences-new-mexico", "Truth or Consequences", "New Mexico", 33.1284, -107.2528, "America/Denver", false, Seq("Truth or Consequences"), Seq(
    ("El Cortez Truth or Consequences", "El Cortez Truth or Consequences", "el-cortez-truth-or-consequences", "New Mexico", "Truth or Consequences")
  ))
  private def p_tuba_city: R = ("tuba-city", "tuba-city-arizona", "Tuba City", "Arizona", 36.135, -111.2399, "America/Denver", false, Seq("Tuba City"), Seq(
    ("Tuba City Twin Cinema", "Tuba City Twin Cinema", "goen-cinemas-tuba-city", "Arizona", "Tuba City")
  ))
  private def p_tucumcari: R = ("tucumcari", "tucumcari-new-mexico", "Tucumcari", "New Mexico", 35.1719, -103.7269, "America/Denver", false, Seq("Tucumcari"), Seq(
    ("Odeon Tucumcari", "Odeon Tucumcari", "odeon-tucumcari", "New Mexico", "Tucumcari")
  ))
  private def p_tulare: R = ("tulare", "tulare-california", "Tulare", "California", 36.2077, -119.3473, "America/Los_Angeles", false, Seq("Tulare"), Seq(
    ("Galaxy Theatres Tulare", "Galaxy Theatres Tulare", "galaxy-theatres-tulare", "California", "Tulare")
  ))
  private def p_tupelo: R = ("tupelo", "tupelo-mississippi", "Tupelo", "Mississippi", 34.2581, -88.7046, "America/Chicago", false, Seq("Tupelo"), Seq(
    ("Cinemark Tupelo 8", "Cinemark Tupelo 8", "cinemark-tupelo-8", "Mississippi", "Tupelo"),
    ("Malco Tupelo Commons 10", "Malco Tupelo Commons 10", "malco-tupelo-commons-10", "Mississippi", "Tupelo")
  ))
  private def p_tupper_lake: R = ("tupper-lake", "tupper-lake-new-york", "Tupper Lake", "New York", 44.224, -74.4641, "America/New_York", false, Seq("Tupper Lake"), Seq(
    ("Adirondack State Theater", "Adirondack State Theater", "adirondack-state-theater", "New York", "Tupper Lake")
  ))
  private def p_tutu: R = ("tutu", "tutu-virgin-islands", "Tutu", "Virgin Islands", 18.3388, -64.8865, "America/St_Thomas", false, Seq("Tutu"), Seq(
    ("Caribbean Cinemas USVI Market Square", "Caribbean Cinemas USVI Market Square", "caribbean-cinemas-usvi-market-square", "Virgin Islands", "St Thomas")
  ))
  private def p_twentynine_palms: R = ("twentynine-palms", "twentynine-palms-california", "Twentynine Palms", "California", 34.1356, -116.0542, "America/Los_Angeles", false, Seq("Twentynine Palms"), Seq(
    ("Smith's Ranch Drive-In Theater", "Smith's Ranch Drive-In Theater", "smiths-ranch-drive-in-theater", "California", "Twentynine Palms")
  ))
  private def p_twin_falls: R = ("twin-falls", "twin-falls-idaho", "Twin Falls", "Idaho", 42.563, -114.4609, "America/Boise", false, Seq("Twin Falls"), Seq(
    ("Magic Valley Cinema Twin Falls", "Magic Valley Cinema Twin Falls", "magic-valley-cinema-twin-falls", "Idaho", "Twin Falls"),
    ("The Lamphouse Theatre", "The Lamphouse Theatre", "the-lamphouse-theatre", "Idaho", "Twin Falls")
  ))
  private def p_two_harbors: R = ("two-harbors", "two-harbors-minnesota", "Two Harbors", "Minnesota", 47.0227, -91.6707, "America/Chicago", false, Seq("Two Harbors"), Seq(
    ("Harbor Theater (Two Harbors)", "Harbor Theater (Two Harbors)", "harbor-theater-mi", "Minnesota", "Two Harbors")
  ))
  private def p_ukiah: R = ("ukiah", "ukiah-california", "Ukiah", "California", 39.1502, -123.2078, "America/Los_Angeles", false, Seq("Ukiah"), Seq(
    ("Regal Ukiah", "Regal Ukiah", "regal-ukiah", "California", "Ukiah")
  ))
  private def p_ulysses: R = ("ulysses", "ulysses-kansas", "Ulysses", "Kansas", 37.5814, -101.3552, "America/Chicago", false, Seq("Ulysses"), Seq(
    ("The Movies Ulysses", "The Movies Ulysses", "the-movies-ulysses", "Kansas", "Ulysses")
  ))
  private def p_uniontown: R = ("uniontown", "uniontown-pennsylvania", "Uniontown", "Pennsylvania", 39.9001, -79.7164, "America/New_York", true, Seq("Uniontown", "Connellsville", "Brownsville"), Seq(
    ("Brownsville Drive-In Grindstone", "Brownsville Drive-In Grindstone", "brownsville-drive-in-grindstone", "Pennsylvania", "Grindstone"),
    ("Erwin's Comet Drive-in Dunbar", "Erwin's Comet Drive-in Dunbar", "erwins-comet-drive-in-dunbar", "Pennsylvania", "Connellsville"),
    ("State Theatre (Center For The Arts)", "State Theatre (Center For The Arts)", "state-theatre-center-for-the-arts", "Pennsylvania", "Uniontown")
  ))
  private def p_utica: R = ("utica", "utica-new-york", "Utica", "New York", 43.1009, -75.2327, "America/New_York", true, Seq("Utica", "Little Falls", "New Hartford"), Seq(
    ("Marquee Orchard New Hartford 14", "Marquee Orchard New Hartford 14", "marquee-orchard-new-hartford-14", "New York", "New Hartford"),
    ("Valley Twins Cinema Little Falls", "Valley Twins Cinema Little Falls", "valley-twins-cinema-little-falls", "New York", "Little Falls"),
    ("Munson (Munson-Williams-Proctor Arts Institute)", "Munson (Munson-Williams-Proctor Arts Institute)", "munson-munson-williams-proctor-arts-institute", "New York", "Utica")
  ))
  private def p_uvalde: R = ("uvalde", "uvalde-texas", "Uvalde", "Texas", 29.2097, -99.7862, "America/Chicago", false, Seq("Uvalde"), Seq(
    ("Forum Uvalde 6", "Forum Uvalde 6", "forum-uvalde-6", "Texas", "Uvalde")
  ))
  private def p_vacaville: R = ("vacaville", "vacaville-california", "Vacaville", "California", 38.3566, -121.9877, "America/Los_Angeles", true, Seq("Vacaville", "Napa"), Seq(
    ("Brenden Vacaville 16", "Brenden Vacaville 16", "brenden-vacaville-16", "California", "Vacaville"),
    ("Cinemark Century Napa Valley 12", "Cinemark Century Napa Valley 12", "cinemark-century-napa-valley-12", "California", "Napa")
  ))
  private def p_valdez: R = ("valdez", "valdez-alaska", "Valdez", "Alaska", 61.1308, -146.3483, "America/Anchorage", false, Seq("Valdez"), Seq(
    ("Civic Center Valdez", "Civic Center Valdez", "civic-center-valdez", "Alaska", "Old Valdez")
  ))
  private def p_valdosta: R = ("valdosta", "valdosta-georgia", "Valdosta", "Georgia", 30.8333, -83.2803, "America/New_York", false, Seq("Valdosta"), Seq(
    ("GTC Valdosta Cinemas", "GTC Valdosta Cinemas", "gtc-valdosta-cinemas", "Georgia", "Valdosta")
  ))
  private def p_valley_city: R = ("valley-city", "valley-city-north-dakota", "Valley City", "North Dakota", 46.9233, -98.0032, "America/Chicago", false, Seq("Valley City"), Seq(
    ("Valley Twin Cinema", "Valley Twin Cinema", "valley-twin-cinema", "North Dakota", "Valley City")
  ))
  private def p_valparaiso: R = ("valparaiso", "valparaiso-indiana", "Valparaiso", "Indiana", 41.4731, -87.0611, "America/Chicago", true, Seq("Valparaiso", "Portage"), Seq(
    ("49er Drive In Valparaiso", "49er Drive In Valparaiso", "49er-drive-in-valparaiso", "Indiana", "Valparaiso"),
    ("Cinemark at Valparaiso", "Cinemark at Valparaiso", "cinemark-at-valparaiso", "Indiana", "Valparaiso"),
    ("Emagine Portage", "Emagine Portage", "emagine-portage", "Indiana", "Portage")
  ))
  private def p_van_buren: R = ("van-buren", "van-buren-missouri", "Van Buren", "Missouri", 36.9956, -91.0146, "America/Chicago", false, Seq("Van Buren"), Seq(
    ("21 Drive In Van Buren", "21 Drive In Van Buren", "21-drive-in-van-buren", "Missouri", "Van Buren")
  ))
  private def p_van_wert: R = ("van-wert", "van-wert-ohio", "Van Wert", "Ohio", 40.8695, -84.5841, "America/New_York", true, Seq("Van Wert", "Decatur", "Middle Point"), Seq(
    ("ABCinema", "ABCinema", "abcinema", "Indiana", "Decatur"),
    ("Van Del Drive-in Middle Point", "Van Del Drive-in Middle Point", "van-del-drive-in-middle-point", "Ohio", "Middle Point"),
    ("Van Wert Cinemas", "Van Wert Cinemas", "van-wert-cinemas", "Ohio", "Van Wert")
  ))
  private def p_vandalia: R = ("vandalia", "vandalia-illinois", "Vandalia", "Illinois", 38.9606, -89.0937, "America/Chicago", false, Seq("Vandalia"), Seq(
    ("Starmax Cinemas Vandalia", "Starmax Cinemas Vandalia", "starmax-cinemas-vandalia", "Illinois", "Vandalia")
  ))
  private def p_vandergrift: R = ("vandergrift", "vandergrift-pennsylvania", "Vandergrift", "Pennsylvania", 40.6028, -79.5648, "America/New_York", true, Seq("Vandergrift", "Tarentum", "Freeport"), Seq(
    ("GQT Pittsburgh Mills", "GQT Pittsburgh Mills", "gqt-pittsburgh-mills", "Pennsylvania", "Tarentum"),
    ("South Pike 10 Sarver", "South Pike 10 Sarver", "south-pike-10-sarver", "Pennsylvania", "Sarver"),
    ("Riverside Drive-in Vandergrift", "Riverside Drive-in Vandergrift", "riverside-drive-in-vandergrift", "Pennsylvania", "East Vandergrift")
  ))
  private def p_vashon: R = ("vashon", "vashon-washington", "Vashon", "Washington", 47.4473, -122.4599, "America/Los_Angeles", true, Seq("Vashon", "Federal Way"), Seq(
    ("Cinemark Century Federal Way", "Cinemark Century Federal Way", "cinemark-century-federal-way", "Washington", "Federal Way"),
    ("Vashon Theatre", "Vashon Theatre", "vashon-theatre", "Washington", "Vashon"),
    ("Night Light Drive In", "Night Light Drive In", "night-light-drive-in", "Washington", "Vashon")
  ))
  private def p_vassar: R = ("vassar", "vassar-michigan", "Vassar", "Michigan", 43.372, -83.5833, "America/Detroit", true, Seq("Vassar", "Birch Run"), Seq(
    ("Emagine Birch Run", "Emagine Birch Run", "emagine-birch-run", "Michigan", "Birch Run"),
    ("Vassar Theatre", "Vassar Theatre", "vassar-theatre", "Michigan", "Vassar")
  ))
  private def p_venice: R = ("venice", "venice-florida", "Venice", "Florida", 27.0998, -82.4543, "America/New_York", false, Seq("Venice"), Seq(
    ("Grand Luxury Cinema", "Grand Luxury Cinema", "grand-luxury-cinema", "Florida", "Venice")
  ))
  private def p_ventura: R = ("ventura", "ventura-california", "Ventura", "California", 34.2783, -119.2932, "America/Los_Angeles", true, Seq("Ventura", "Ojai"), Seq(
    ("Cinemark Century Ventura Downtown 10", "Cinemark Century Ventura Downtown 10", "cinemark-century-ventura-downtown-10", "California", "Ventura"),
    ("Ojai Playhouse", "Ojai Playhouse", "ojai-playhouse", "California", "Ojai")
  ))
  private def p_vermillion: R = ("vermillion", "vermillion-south-dakota", "Vermillion", "South Dakota", 42.7794, -96.9292, "America/Chicago", false, Seq("Vermillion"), Seq(
    ("Coyote Twin Theaters", "Coyote Twin Theaters", "coyote-twin-theaters", "South Dakota", "Vermillion"),
    ("Vermillion Theater", "Vermillion Theater", "vermillion-theater", "South Dakota", "Vermillion")
  ))
  private def p_vernal: R = ("vernal", "vernal-utah", "Vernal", "Utah", 40.4555, -109.5288, "America/Denver", false, Seq("Vernal"), Seq(
    ("Cinema Vernal 5", "Cinema Vernal 5", "cinema-vernal-5", "Utah", "Vernal")
  ))
  private def p_vernon: R = ("vernon", "vernon-texas", "Vernon", "Texas", 34.1554, -99.2663, "America/Chicago", false, Seq("Vernon"), Seq(
    ("Vernon Plaza", "Vernon Plaza", "vernon-plaza", "Texas", "Vernon")
  ))
  private def p_vero_beach: R = ("vero-beach", "vero-beach-florida", "Vero Beach", "Florida", 27.6386, -80.3973, "America/New_York", false, Seq("Vero Beach"), Seq(
    ("AMC Indian River 14", "AMC Indian River 14", "amc-indian-river-14", "Florida", "Vero Beach"),
    ("CW Theaters Majestic Vero Beach 11", "CW Theaters Majestic Vero Beach 11", "cw-theaters-majestic-vero-beach-11", "Florida", "Vero Beach")
  ))
  private def p_viborg: R = ("viborg", "viborg-south-dakota", "Viborg", "South Dakota", 43.1712, -97.082, "America/Chicago", false, Seq("Viborg"), Seq(
    ("Lund Theatre Viborg", "Lund Theatre Viborg", "lund-theatre-viborg", "South Dakota", "Viborg")
  ))
  private def p_vicksburg: R = ("vicksburg", "vicksburg-mississippi", "Vicksburg", "Mississippi", 32.3526, -90.8779, "America/Chicago", false, Seq("Vicksburg"), Seq(
    ("Strand Theatre Vicksburg", "Strand Theatre Vicksburg", "strand-theatre-vicksburg", "Mississippi", "Vicksburg")
  ))
  private def p_victor: R = ("victor", "victor-idaho", "Victor", "Idaho", 43.6027, -111.1113, "America/Boise", false, Seq("Victor"), Seq(
    ("Pierre's Theatre", "Pierre's Theatre", "pierres-theatre", "Idaho", "Victor")
  ))
  private def p_victoria: R = ("victoria", "victoria-texas", "Victoria", "Texas", 28.8053, -97.0036, "America/Chicago", false, Seq("Victoria"), Seq(
    ("Cinemark Victoria 12", "Cinemark Victoria 12", "cinemark-victoria-12", "Texas", "Victoria")
  ))
  private def p_vidalia: R = ("vidalia", "vidalia-georgia", "Vidalia", "Georgia", 32.2177, -82.4135, "America/New_York", false, Seq("Vidalia"), Seq(
    ("Pal Theatre Vidalia", "Pal Theatre Vidalia", "pal-theatre-vidalia", "Georgia", "Vidalia"),
    ("Sweet Onion Vidalia 5", "Sweet Onion Vidalia 5", "sweet-onion-vidalia-5", "Georgia", "Vidalia")
  ))
  private def p_vienna_bend: R = ("vienna-bend", "vienna-bend-louisiana", "Vienna Bend", "Louisiana", 31.7324, -93.041, "America/Chicago", false, Seq("Vienna Bend"), Seq(
    ("Acadiana Parkway Cinema 6", "Acadiana Parkway Cinema 6", "acadiana-parkway-cinema-6", "Louisiana", "East Natchitoches")
  ))
  private def p_vincennes: R = ("vincennes", "vincennes-indiana", "Vincennes", "Indiana", 38.6773, -87.5286, "America/Indiana/Vincennes", false, Seq("Vincennes"), Seq(
    ("AMC CLASSIC Vincennes 8", "AMC CLASSIC Vincennes 8", "amc-classic-vincennes-8", "Indiana", "Vincennes")
  ))
  private def p_vineland: R = ("vineland", "vineland-new-jersey", "Vineland", "New Jersey", 39.4862, -75.0257, "America/New_York", false, Seq("Vineland"), Seq(
    ("Delsea Drive-in Vineland", "Delsea Drive-in Vineland", "delsea-drive-in-vineland", "New Jersey", "Vineland"),
    ("Regal Cumberland Mall Vineland", "Regal Cumberland Mall Vineland", "regal-cumberland-mall-vineland", "New Jersey", "Vineland")
  ))
  private def p_vineland_minnesota: R = ("vineland-minnesota", "vineland-minnesota", "Vineland", "Minnesota", 46.1636, -93.7575, "America/Chicago", false, Seq("Vineland"), Seq(
    ("Grand Makwa Cinema Onamia", "Grand Makwa Cinema Onamia", "grand-makwa-cinema-onamia", "Minnesota", "Onamia")
  ))
  private def p_vinita: R = ("vinita", "vinita-oklahoma", "Vinita", "Oklahoma", 36.6387, -95.1541, "America/Chicago", false, Seq("Vinita"), Seq(
    ("Center Cinema Vinita", "Center Cinema Vinita", "center-cinema-vinita", "Oklahoma", "Vinita")
  ))
  private def p_virginia: R = ("virginia", "virginia-minnesota", "Virginia", "Minnesota", 47.5233, -92.5366, "America/Chicago", false, Seq("Virginia"), Seq(
    ("Cinema 6 Virginia", "Cinema 6 Virginia", "cinema-6-virginia", "Minnesota", "Virginia")
  ))
  private def p_viroqua: R = ("viroqua", "viroqua-wisconsin", "Viroqua", "Wisconsin", 43.5569, -90.8887, "America/Chicago", false, Seq("Viroqua"), Seq(
    ("Historic Temple Theatre of Viroqua", "Historic Temple Theatre of Viroqua", "historic-temple-theatre-of-viroqua", "Wisconsin", "Viroqua")
  ))
  private def p_wabash: R = ("wabash", "wabash-indiana", "Wabash", "Indiana", 40.7978, -85.8205, "America/Indiana/Indianapolis", false, Seq("Wabash"), Seq(
    ("13-24 Drive In Wabash", "13-24 Drive In Wabash", "13-24-drive-in-wabash", "Indiana", "Wabash"),
    ("Eagles Theatre Wabash", "Eagles Theatre Wabash", "eagles-theatre-wabash", "Indiana", "Wabash"),
    ("Honeywell Center", "Honeywell Center", "honeywell-center", "Indiana", "Wabash")
  ))
  private def p_waconia: R = ("waconia", "waconia-minnesota", "Waconia", "Minnesota", 44.8508, -93.7869, "America/Chicago", true, Seq("Waconia", "Delano", "Arlington"), Seq(
    ("Emagine Delano", "Emagine Delano", "emagine-delano", "Minnesota", "Delano"),
    ("Emagine Waconia", "Emagine Waconia", "emagine-waconia", "Minnesota", "Waconia"),
    ("Lido Theatre Arlington", "Lido Theatre Arlington", "lido-theatre-arlington", "Minnesota", "Arlington")
  ))
  private def p_wadena: R = ("wadena", "wadena-minnesota", "Wadena", "Minnesota", 46.4425, -95.1361, "America/Chicago", false, Seq("Wadena"), Seq(
    ("Cozy Wadena", "Cozy Wadena", "cozy-wadena", "Minnesota", "Wadena")
  ))
  private def p_wagner: R = ("wagner", "wagner-south-dakota", "Wagner", "South Dakota", 43.0797, -98.2931, "America/Chicago", true, Seq("Wagner", "Armour", "Lynch"), Seq(
    ("Wagner Theater", "Wagner Theater", "wagner-theater", "South Dakota", "Wagner"),
    ("Lorain Theatre Armour", "Lorain Theatre Armour", "lorain-theatre-armour", "South Dakota", "Armour"),
    ("Lynn Theatre Lynch", "Lynn Theatre Lynch", "lynn-theatre-lynch", "Nebraska", "Lynch")
  ))
  private def p_waikoloa: R = ("waikoloa", "waikoloa-hawaii", "Waikoloa", "Hawaii", 19.9394, -155.7893, "Pacific/Honolulu", false, Seq("Waikoloa"), Seq(
    ("Waikōloa Luxury Cinemas", "Waikōloa Luxury Cinemas", "waikloa-luxury-cinemas", "Hawaii", "Waikoloa")
  ))
  private def p_waimea: R = ("waimea", "waimea-hawaii", "Waimea", "Hawaii", 21.9571, -159.6689, "Pacific/Honolulu", false, Seq("Waimea"), Seq(
    ("Waimea Theatre", "Waimea Theatre", "waimea-theatre", "Hawaii", "Waimea")
  ))
  private def p_waite_park: R = ("waite-park", "waite-park-minnesota", "Waite Park", "Minnesota", 45.5572, -94.2242, "America/Chicago", true, Seq("Waite Park", "Cold Spring"), Seq(
    ("Marcus Parkwood Cinema", "Marcus Parkwood Cinema", "marcus-parkwood-cinema", "Minnesota", "Waite Park"),
    ("Quarry Cinema Cold Spring", "Quarry Cinema Cold Spring", "quarry-cinema-cold-spring", "Minnesota", "Cold Spring")
  ))
  private def p_waldorf: R = ("waldorf", "waldorf-maryland", "Waldorf", "Maryland", 38.6246, -76.9391, "America/New_York", true, Seq("Waldorf", "Oxon Hill", "Temple Hills", "Brandywine"), Seq(
    ("AMC Rivertowne 12", "AMC Rivertowne 12", "amc-rivertowne-12", "Maryland", "Oxon Hill"),
    ("AMC St. Charles Waldorf 9", "AMC St. Charles Waldorf 9", "amc-st-charles-waldorf-9", "Maryland", "Waldorf"),
    ("Phoenix Theatres Marlow 6", "Phoenix Theatres Marlow 6", "phoenix-theatres-marlow-6", "Maryland", "Temple Hills"),
    ("Xscape Brandywine 14", "Xscape Brandywine 14", "xscape-brandywine-14", "Maryland", "Brandywine")
  ))
  private def p_waldron: R = ("waldron", "waldron-arkansas", "Waldron", "Arkansas", 34.8984, -94.0908, "America/Chicago", false, Seq("Waldron"), Seq(
    ("Scott Theater Waldron", "Scott Theater Waldron", "scott-theater-waldron", "Arkansas", "Waldron")
  ))
  private def p_walla_walla: R = ("walla-walla", "walla-walla-washington", "Walla Walla", "Washington", 46.0646, -118.343, "America/Los_Angeles", true, Seq("Walla Walla", "Milton-Freewater"), Seq(
    ("M-f Drive-in Milton Freewater", "M-f Drive-in Milton Freewater", "m-f-drive-in-milton-freewater", "Oregon", "Milton-Freewater"),
    ("Cinemark Grand Cinema Walla Walla", "Cinemark Grand Cinema Walla Walla", "cinemark-grand-cinema-walla-walla", "Washington", "Walla Walla"),
    ("Gesa Power House Theatre", "Gesa Power House Theatre", "gesa-power-house-theatre", "Washington", "Walla Walla")
  ))
  private def p_walsenburg: R = ("walsenburg", "walsenburg-colorado", "Walsenburg", "Colorado", 37.6242, -104.7803, "America/Denver", false, Seq("Walsenburg"), Seq(
    ("Fox Theatre Walsenburg", "Fox Theatre Walsenburg", "fox-theatre-walsenburg", "Colorado", "Walsenburg")
  ))
  private def p_walterboro: R = ("walterboro", "walterboro-south-carolina", "Walterboro", "South Carolina", 32.9052, -80.6668, "America/New_York", false, Seq("Walterboro"), Seq(
    ("Ivanhoe Family Cinemas", "Ivanhoe Family Cinemas", "ivanhoe-family-cinemas", "South Carolina", "Walterboro")
  ))
  private def p_walton: R = ("walton", "walton-new-york", "Walton", "New York", 42.1695, -75.1293, "America/New_York", true, Seq("Walton", "Deposit"), Seq(
    ("State Theatre Deposit", "State Theatre Deposit", "state-theatre-deposit", "New York", "Deposit"),
    ("Walton Theatre", "Walton Theatre", "walton-theatre", "New York", "Walton")
  ))
  private def p_warner_robins: R = ("warner-robins", "warner-robins-georgia", "Warner Robins", "Georgia", 32.6157, -83.6266, "America/New_York", true, Seq("Warner Robins", "Centerville"), Seq(
    ("GTC Galleria Mall Cinemas", "GTC Galleria Mall Cinemas", "gtc-galleria-mall-cinemas", "Georgia", "Centerville"),
    ("GTC Houston Lakes", "GTC Houston Lakes", "gtc-houston-lakes", "Georgia", "Warner Robins")
  ))
  private def p_warren_minnesota: R = ("warren-minnesota", "warren-minnesota", "Warren", "Minnesota", 48.1966, -96.7728, "America/Chicago", false, Seq("Warren"), Seq(
    ("Sky Vu Drive-in Warren", "Sky Vu Drive-in Warren", "sky-vu-drive-in-warren", "Minnesota", "Warren")
  ))
  private def p_warren_ohio: R = ("warren-ohio", "warren-ohio", "Warren", "Ohio", 41.2376, -80.8184, "America/New_York", true, Seq("Warren", "Niles", "Reynolds Heights"), Seq(
    ("Regal Boulevard Centre Niles", "Regal Boulevard Centre Niles", "regal-boulevard-centre-niles", "Ohio", "Niles"),
    ("Skyway Twin Drive-in", "Skyway Twin Drive-in", "skyway-twin-drive-in", "Ohio", "Warren"),
    ("Elm Road Drive-in Warren", "Elm Road Drive-in Warren", "elm-road-drive-in-warren", "Ohio", "Warren"),
    ("Reynolds Drive-In", "Reynolds Drive-In", "reynolds-drive-in", "Pennsylvania", "Transfer")
  ))
  private def p_warrensburg: R = ("warrensburg", "warrensburg-missouri", "Warrensburg", "Missouri", 38.7628, -93.7361, "America/Chicago", false, Seq("Warrensburg"), Seq(
    ("B&B Warrensburg 10", "B&B Warrensburg 10", "bb-warrensburg-10", "Missouri", "Warrensburg")
  ))
  private def p_warwick: R = ("warwick", "warwick-rhode-island", "Warwick", "Rhode Island", 41.7001, -71.4162, "America/New_York", false, Seq("Warwick"), Seq(
    ("Apple Cinemas Warwick Mall", "Apple Cinemas Warwick Mall", "apple-cinemas-warwick-mall", "Rhode Island", "Warwick"),
    ("Showcase Warwick", "Showcase Warwick", "showcase-warwick", "Rhode Island", "Warwick")
  ))
  private def p_washington_illinois: R = ("washington-illinois", "washington-illinois", "Washington", "Illinois", 40.7037, -89.4073, "America/Chicago", true, Seq("Washington", "Morton"), Seq(
    ("AMC CLASSIC Sunnyland 10", "AMC CLASSIC Sunnyland 10", "amc-classic-sunnyland-10", "Illinois", "Washington"),
    ("VIP Morton Cinemas", "VIP Morton Cinemas", "vip-morton-cinemas", "Illinois", "Morton")
  ))
  private def p_washington_indiana: R = ("washington-indiana", "washington-indiana", "Washington", "Indiana", 38.6592, -87.1728, "America/Indiana/Vincennes", false, Seq("Washington"), Seq(
    ("Indiana Theater", "Indiana Theater", "indiana-theater", "Indiana", "Washington")
  ))
  private def p_washington_iowa: R = ("washington-iowa", "washington-iowa", "Washington", "Iowa", 41.2992, -91.6929, "America/Chicago", false, Seq("Washington"), Seq(
    ("Fridley State Washington", "Fridley State Washington", "fridley-state-washington", "Iowa", "North Washington")
  ))
  private def p_washington_missouri: R = ("washington-missouri", "washington-missouri", "Washington", "Missouri", 38.5581, -91.0121, "America/Chicago", true, Seq("Washington", "Union", "New Haven"), Seq(
    ("Cinema 1 Plus Washington", "Cinema 1 Plus Washington", "cinema-1-plus-washington", "Missouri", "Washington"),
    ("Great 8 Cinemas", "Great 8 Cinemas", "great-8-cinemas", "Missouri", "Union"),
    ("Walt Theatre New Haven", "Walt Theatre New Haven", "walt-theatre-new-haven", "Missouri", "New Haven")
  ))
  private def p_wasilla: R = ("wasilla", "wasilla-alaska", "Wasilla", "Alaska", 61.5809, -149.4415, "America/Anchorage", false, Seq("Wasilla"), Seq(
    ("The Valley Cinema", "The Valley Cinema", "the-valley-cinema", "Alaska", "Wasilla")
  ))
  private def p_waterford: R = ("waterford", "waterford-michigan", "Waterford", "Michigan", 42.693, -83.4118, "America/Detroit", true, Seq("Waterford", "Milford", "Oxford", "Hartland"), Seq(
    ("Emagine Hartland", "Emagine Hartland", "emagine-hartland", "Michigan", "Hartland"),
    ("GQT Oxford 7", "GQT Oxford 7", "gqt-oxford-7", "Michigan", "Oxford"),
    ("Milford Independent Cinema", "Milford Independent Cinema", "milford-independent-cinema", "Michigan", "Milford"),
    ("MJR Waterford Cinema", "MJR Waterford Cinema", "mjr-waterford-cinema", "Michigan", "Waterford")
  ))
  private def p_waterloo: R = ("waterloo", "waterloo-iowa", "Waterloo", "Iowa", 42.4928, -92.343, "America/Chicago", false, Seq("Waterloo"), Seq(
    ("Marcus Crossroads Waterloo", "Marcus Crossroads Waterloo", "marcus-crossroads-waterloo", "Iowa", "Waterloo")
  ))
  private def p_watertown: R = ("watertown", "watertown-south-dakota", "Watertown", "South Dakota", 44.8994, -97.1151, "America/Chicago", false, Seq("Watertown"), Seq(
    ("Odyssey Grand Theater Watertown", "Odyssey Grand Theater Watertown", "odyssey-grand-theater-watertown", "South Dakota", "Watertown")
  ))
  private def p_watertown_new_york: R = ("watertown-new-york", "watertown-new-york", "Watertown", "New York", 43.9748, -75.9108, "America/New_York", true, Seq("Watertown", "Lowville", "Black River"), Seq(
    ("Town Hall Theatre Lowville", "Town Hall Theatre Lowville", "town-hall-theatre-lowville", "New York", "Hollowville"),
    ("Black River Drive-in", "Black River Drive-in", "black-river-drive-in", "New York", "Black River"),
    ("Regal Salmon Run Watertown", "Regal Salmon Run Watertown", "regal-salmon-run-watertown", "New York", "Watertown")
  ))
  private def p_waterville: R = ("waterville", "waterville-maine", "Waterville", "Maine", 44.552, -69.6317, "America/New_York", false, Seq("Waterville"), Seq(
    ("Flagship Cinemas Waterville", "Flagship Cinemas Waterville", "flagship-cinemas-waterville", "Maine", "Waterville"),
    ("Maine Film Center Waterville", "Maine Film Center Waterville", "maine-film-center-waterville", "Maine", "Waterville")
  ))
  private def p_watford_city: R = ("watford-city", "watford-city-north-dakota", "Watford City", "North Dakota", 47.8022, -103.2832, "America/Chicago", false, Seq("Watford City"), Seq(
    ("Six Shooters Watford City", "Six Shooters Watford City", "six-shooters-watford-city", "North Dakota", "Watford City")
  ))
  private def p_watsonville: R = ("watsonville", "watsonville-california", "Watsonville", "California", 36.9102, -121.7569, "America/Los_Angeles", true, Seq("Watsonville", "Gilroy", "Morgan Hill", "Hollister", "Marina"), Seq(
    ("CineLux Green Valley Watsonville", "CineLux Green Valley Watsonville", "cinelux-green-valley-watsonville", "California", "Watsonville"),
    ("Cinemark Century Marina", "Cinemark Century Marina", "cinemark-century-marina", "California", "Marina"),
    ("Premiere Cinemas Hollister", "Premiere Cinemas Hollister", "premiere-cinemas-hollister", "California", "Hollister"),
    ("CineLux Gilroy Cafe & Lounge", "CineLux Gilroy Cafe & Lounge", "cinelux-gilroy-cafe-lounge", "California", "Gilroy"),
    ("CineLux Tennant Station Morgan Hill 11", "CineLux Tennant Station Morgan Hill 11", "cinelux-tennant-station-morgan-hill-11", "California", "Morgan Hill")
  ))
  private def p_wauseon: R = ("wauseon", "wauseon-ohio", "Wauseon", "Ohio", 41.5492, -84.1416, "America/New_York", true, Seq("Wauseon", "Liberty Center"), Seq(
    ("Skye Cinema Wauseon 6", "Skye Cinema Wauseon 6", "skye-cinema-wauseon-6", "Ohio", "Wauseon"),
    ("Field of Dreams Drive-in Liberty Center", "Field of Dreams Drive-in Liberty Center", "field-of-dreams-drive-in-liberty-center", "Ohio", "Liberty Center")
  ))
  private def p_wauwatosa: R = ("wauwatosa", "wauwatosa-wisconsin", "Wauwatosa", "Wisconsin", 43.0495, -88.0076, "America/Chicago", true, Seq("Wauwatosa", "New Berlin", "Menomonee Falls", "Oak Creek", "Mequon", "Greendale"), Seq(
    ("AMC Mayfair Mall 18 Wauwatosa", "AMC Mayfair Mall 18 Wauwatosa", "amc-mayfair-mall-18-wauwatosa", "Wisconsin", "Wauwatosa"),
    ("Rosebud Cinema Wauwatosa", "Rosebud Cinema Wauwatosa", "rosebud-cinema-wauwatosa", "Wisconsin", "Wauwatosa"),
    ("Marcus BistroPlex Southridge", "Marcus BistroPlex Southridge", "marcus-bistroplex-southridge", "Wisconsin", "Greendale"),
    ("Marcus Menomonee Falls", "Marcus Menomonee Falls", "marcus-menomonee-falls", "Wisconsin", "Menomonee Falls"),
    ("Marcus North Shore", "Marcus North Shore", "marcus-north-shore", "Wisconsin", "Mequon"),
    ("Marcus Ridge Cinema New Berlin", "Marcus Ridge Cinema New Berlin", "marcus-ridge-cinema-new-berlin", "Wisconsin", "New Berlin"),
    ("Marcus South Shore Oak Creek", "Marcus South Shore Oak Creek", "marcus-south-shore-oak-creek", "Wisconsin", "Oak Creek")
  ))
  private def p_waycross: R = ("waycross", "waycross-georgia", "Waycross", "Georgia", 31.2137, -82.3557, "America/New_York", false, Seq("Waycross"), Seq(
    ("GTC Mall Cinemas Waycross", "GTC Mall Cinemas Waycross", "gtc-mall-cinemas-waycross", "Georgia", "Waycross")
  ))
  private def p_wayne: R = ("wayne", "wayne-new-jersey", "Wayne", "New Jersey", 40.9254, -74.2765, "America/New_York", true, Seq("Wayne", "Montclair", "Clifton", "West Orange", "Paramus", "Hawthorne", "Rutherford", "South Orange", "East Hanover"), Seq(
    ("AMC Clifton Commons 16", "AMC Clifton Commons 16", "amc-clifton-commons-16", "New Jersey", "Clifton"),
    ("AMC DINE-IN Essex Green 9", "AMC DINE-IN Essex Green 9", "amc-dine-in-essex-green-9", "New Jersey", "West Orange"),
    ("AMC East Hanover 12", "AMC East Hanover 12", "amc-east-hanover-12", "New Jersey", "East Hanover"),
    ("AMC Garden State", "AMC Garden State", "amc-garden-state-15", "New Jersey", "Paramus"),
    ("AMC Wayne 12", "AMC Wayne 12", "amc-wayne-12", "New Jersey", "Wayne"),
    ("Cinemark Willowbrook Mall", "Cinemark Willowbrook Mall", "cinemark-willowbrook-mall", "New Jersey", "Wayne"),
    ("Bellevue Theatre Montclair", "Bellevue Theatre Montclair", "bellevue-theatre-montclair", "New Jersey", "Montclair"),
    ("The Clairidge Montclair", "The Clairidge Montclair", "the-clairidge-montclair", "New Jersey", "Montclair"),
    ("Cinema Underground Williams Center", "Cinema Underground Williams Center", "cinema-underground-williams-center", "New Jersey", "Rutherford"),
    ("Hawthorne Theaters", "Hawthorne Theaters", "hawthorne-theaters", "New Jersey", "Hawthorne"),
    ("The Village at SOPAC", "The Village at SOPAC", "the-village-at-sopac", "New Jersey", "South Orange")
  ))
  private def p_wayne_nebraska: R = ("wayne-nebraska", "wayne-nebraska", "Wayne", "Nebraska", 42.2306, -97.0178, "America/Chicago", false, Seq("Wayne"), Seq(
    ("Majestic Theater Wayne", "Majestic Theater Wayne", "majestic-theater-wayne", "Nebraska", "Wayne")
  ))
  private def p_waynesboro: R = ("waynesboro", "waynesboro-virginia", "Waynesboro", "Virginia", 38.0685, -78.8895, "America/New_York", true, Seq("Waynesboro", "Staunton"), Seq(
    ("Visulite Cinema Staunton", "Visulite Cinema Staunton", "visulite-cinema-staunton", "Virginia", "Staunton"),
    ("Zeus Digital Theater", "Zeus Digital Theater", "zeus-digital-theater", "Virginia", "Waynesboro"),
    ("Wayne Theatre Waynesboro", "Wayne Theatre Waynesboro", "wayne-theatre-waynesboro", "Virginia", "Waynesboro")
  ))
  private def p_waynesville: R = ("waynesville", "waynesville-north-carolina", "Waynesville", "North Carolina", 35.4887, -82.9887, "America/New_York", true, Seq("Waynesville", "Sylva", "Cherokee"), Seq(
    ("Catamount Cinemas", "Catamount Cinemas", "catamount-cinemas", "North Carolina", "Sylva"),
    ("Cherokee Cinemas & More", "Cherokee Cinemas & More", "cherokee-cinemas-more", "North Carolina", "Cherokee"),
    ("Smoky Mountain Cinema", "Smoky Mountain Cinema", "smoky-mountain-cinema", "North Carolina", "Waynesville")
  ))
  private def p_waynesville_missouri: R = ("waynesville-missouri", "waynesville-missouri", "Waynesville", "Missouri", 37.8287, -92.2007, "America/Chicago", false, Seq("Waynesville"), Seq(
    ("B&B Waynesville Patriot 12", "B&B Waynesville Patriot 12", "bb-waynesville-patriot-12", "Missouri", "Waynesville")
  ))
  private def p_weatherford: R = ("weatherford", "weatherford-texas", "Weatherford", "Texas", 32.7593, -97.7973, "America/Chicago", false, Seq("Weatherford"), Seq(
    ("Film Alley Weatherford", "Film Alley Weatherford", "film-alley-weatherford", "Texas", "Weatherford")
  ))
  private def p_weatherford_oklahoma: R = ("weatherford-oklahoma", "weatherford-oklahoma", "Weatherford", "Oklahoma", 35.5262, -98.7076, "America/Chicago", false, Seq("Weatherford"), Seq(
    ("Showest Weatherford 4", "Showest Weatherford 4", "showest-weatherford-4", "Oklahoma", "Weatherford")
  ))
  private def p_weaverville: R = ("weaverville", "weaverville-california", "Weaverville", "California", 40.731, -122.942, "America/Los_Angeles", false, Seq("Weaverville"), Seq(
    ("Trinity Theatre Weaverville", "Trinity Theatre Weaverville", "trinity-theatre-weaverville", "California", "Weaverville")
  ))
  private def p_webster: R = ("webster", "webster-south-dakota", "Webster", "South Dakota", 45.3322, -97.5201, "America/Chicago", false, Seq("Webster"), Seq(
    ("Webster Theater", "Webster Theater", "webster-theater", "South Dakota", "Webster")
  ))
  private def p_weirton: R = ("weirton", "weirton-west-virginia", "Weirton", "West Virginia", 40.419, -80.5895, "America/New_York", true, Seq("Weirton", "Chester"), Seq(
    ("Weirton Plaza Theatre", "Weirton Plaza Theatre", "weirton-plaza-theatre", "West Virginia", "Weirton"),
    ("Hilltop Drive-in Theatre Hazard", "Hilltop Drive-in Theatre Hazard", "hilltop-drive-in-theatre-hazard", "West Virginia", "Chester")
  ))
  private def p_welch: R = ("welch", "welch-west-virginia", "Welch", "West Virginia", 37.4329, -81.5845, "America/New_York", false, Seq("Welch"), Seq(
    ("The Pocahontas Theatre", "The Pocahontas Theatre", "the-pocahontas-theatre", "West Virginia", "Welch")
  ))
  private def p_wellfleet: R = ("wellfleet", "wellfleet-massachusetts", "Wellfleet", "Massachusetts", 41.9376, -70.0328, "America/New_York", true, Seq("Wellfleet", "Provincetown", "Chatham"), Seq(
    ("Orpheum Theater Chatham", "Orpheum Theater Chatham", "orpheum-theater-chatham", "Massachusetts", "Chatham"),
    ("Waters Edge Cinema Provincetown", "Waters Edge Cinema Provincetown", "waters-edge-cinema-provincetown", "Massachusetts", "Provincetown"),
    ("Wellfleet Cinemas & Drive-In", "Wellfleet Cinemas & Drive-In", "wellfleet-cinemas", "Massachusetts", "South Wellfleet")
  ))
  private def p_wellington: R = ("wellington", "wellington-kansas", "Wellington", "Kansas", 37.2653, -97.3717, "America/Chicago", false, Seq("Wellington"), Seq(
    ("Wellington Regent Theatre", "Wellington Regent Theatre", "wellington-regent-theatre", "Kansas", "Wellington")
  ))
  private def p_wellington_texas: R = ("wellington-texas", "wellington-texas", "Wellington", "Texas", 34.8562, -100.2137, "America/Chicago", false, Seq("Wellington"), Seq(
    ("Wellington Ritz Theatre, Inc", "Wellington Ritz Theatre, Inc", "wellington-ritz-theatre-inc", "Texas", "Wellington")
  ))
  private def p_wells: R = ("wells", "wells-minnesota", "Wells", "Minnesota", 43.7461, -93.7288, "America/Chicago", true, Seq("Wells", "Kiester"), Seq(
    ("Flame Theatre Wells", "Flame Theatre Wells", "flame-theatre-wells", "Minnesota", "Wells"),
    ("Kee Civic Theatre Kiester", "Kee Civic Theatre Kiester", "kee-civic-theatre-kiester", "Minnesota", "Kiester")
  ))
  private def p_wellsboro: R = ("wellsboro", "wellsboro-pennsylvania", "Wellsboro", "Pennsylvania", 41.7487, -77.3005, "America/New_York", true, Seq("Wellsboro", "Canton", "Blossburg", "Galeton"), Seq(
    ("Arcadia Theatre Wellsboro", "Arcadia Theatre Wellsboro", "arcadia-theatre-wellsboro", "Pennsylvania", "Wellsboro"),
    ("Rialto Theatre Canton", "Rialto Theatre Canton", "rialto-theatre-canton", "Pennsylvania", "Canton"),
    ("Lakeview Cinema Galeton", "Lakeview Cinema Galeton", "lakeview-cinema-galeton", "Pennsylvania", "Galeton"),
    ("Victoria Theatre Blossburg", "Victoria Theatre Blossburg", "victoria-theatre-blossburg", "Pennsylvania", "Blossburg")
  ))
  private def p_wenatchee: R = ("wenatchee", "wenatchee-washington", "Wenatchee", "Washington", 47.4235, -120.3103, "America/Los_Angeles", true, Seq("Wenatchee", "East Wenatchee"), Seq(
    ("Gateway Cinema Wenatchee", "Gateway Cinema Wenatchee", "gateway-cinema-wenatchee", "Washington", "East Wenatchee"),
    ("Liberty Cinema Wenatchee", "Liberty Cinema Wenatchee", "liberty-cinema-wenatchee", "Washington", "Wenatchee")
  ))
  private def p_wesley_chapel: R = ("wesley-chapel", "wesley-chapel-florida", "Wesley Chapel", "Florida", 28.2397, -82.3279, "America/New_York", true, Seq("Wesley Chapel", "Zephyrhills", "Dade City"), Seq(
    ("B&B Wesley Chapel The Grove 16", "B&B Wesley Chapel The Grove 16", "bb-wesley-chapel-the-grove-16", "Florida", "Wesley Chapel"),
    ("Joylan Drive-in Dade City", "Joylan Drive-in Dade City", "joylan-drive-in-dade-city", "Florida", "Dade City"),
    ("Zephyrhills Cinema 10", "Zephyrhills Cinema 10", "zephyrhills-cinema-10", "Florida", "Zephyrhills")
  ))
  private def p_west_bend: R = ("west-bend", "west-bend-wisconsin", "West Bend", "Wisconsin", 43.4253, -88.1834, "America/Chicago", true, Seq("West Bend", "Hartford", "Cedarburg"), Seq(
    ("Rivoli Theatre Cedarburg", "Rivoli Theatre Cedarburg", "rivoli-theatre-cedarburg", "Wisconsin", "Cedarburg"),
    ("Schubert's Hartford Theatre", "Schubert's Hartford Theatre", "schuberts-hartford-theatre", "Wisconsin", "Hartford"),
    ("West Bend Cinema", "West Bend Cinema", "west-bend-cinema", "Wisconsin", "West Bend"),
    ("The Bend Theater", "The Bend Theater", "the-bend-theater", "Wisconsin", "West Bend")
  ))
  private def p_west_branch: R = ("west-branch", "west-branch-michigan", "West Branch", "Michigan", 44.2764, -84.2386, "America/Detroit", false, Seq("West Branch"), Seq(
    ("West Branch Cinema 3", "West Branch Cinema 3", "west-branch-cinema-3", "Michigan", "West Branch")
  ))
  private def p_west_chester: R = ("west-chester", "west-chester-pennsylvania", "West Chester", "Pennsylvania", 39.961, -75.608, "America/New_York", true, Seq("West Chester", "Downingtown", "Exton", "Newtown Square"), Seq(
    ("AMC DINE-IN Painters Crossing West Chester 9", "AMC DINE-IN Painters Crossing West Chester 9", "amc-dine-in-painters-crossing-west-chester-9", "Pennsylvania", "West Chester"),
    ("Uptown! Knauer Performing Arts Center", "Uptown! Knauer Performing Arts Center", "uptown-knauer-performing-arts-center", "Pennsylvania", "West Chester"),
    ("Movie Tavern Exton", "Movie Tavern Exton", "movie-tavern-exton", "Pennsylvania", "Exton"),
    ("Regal Downingtown Cinema", "Regal Downingtown Cinema", "regal-downingtown-cinema", "Pennsylvania", "Downingtown"),
    ("Regal Edgmont Square", "Regal Edgmont Square", "regal-edgmont-square", "Pennsylvania", "Newtown Square")
  ))
  private def p_west_des_moines: R = ("west-des-moines", "west-des-moines-iowa", "West Des Moines", "Iowa", 41.5772, -93.7113, "America/Chicago", true, Seq("West Des Moines", "Ankeny", "Waukee", "Altoona", "Indianola", "Pleasant Hill", "Winterset"), Seq(
    ("B&B Ankeny 12", "B&B Ankeny 12", "bb-ankeny-12", "Iowa", "Ankeny"),
    ("Cinemark Altoona", "Cinemark Altoona", "cinemark-altoona", "Iowa", "Altoona"),
    ("Cinemark Century Jordan Creek 20", "Cinemark Century Jordan Creek 20", "cinemark-century-jordan-creek-20", "Iowa", "West Des Moines"),
    ("Copper Creek 9 Pleasant Hill", "Copper Creek 9 Pleasant Hill", "copper-creek-9-pleasant-hill", "Iowa", "Pleasant Hill"),
    ("Fridley Paramount 7 Indianola", "Fridley Paramount 7 Indianola", "fridley-paramount-7-indianola", "Iowa", "Indianola"),
    ("Iowa Theater Winterset", "Iowa Theater Winterset", "iowa-theater-winterset", "Iowa", "Winterset"),
    ("The Palms Theatres", "The Palms Theatres", "the-palms-theatres", "Iowa", "Waukee")
  ))
  private def p_west_jefferson: R = ("west-jefferson", "west-jefferson-north-carolina", "West Jefferson", "North Carolina", 36.4037, -81.4929, "America/New_York", false, Seq("West Jefferson"), Seq(
    ("Parkway Theater West Jefferson", "Parkway Theater West Jefferson", "parkway-theater-west-jefferson", "North Carolina", "West Jefferson"),
    ("Blue Ridge Movie Lounge", "Blue Ridge Movie Lounge", "blue-ridge-movie-lounge", "North Carolina", "West Jefferson")
  ))
  private def p_west_monroe: R = ("west-monroe", "west-monroe-louisiana", "West Monroe", "Louisiana", 32.5185, -92.1476, "America/Chicago", false, Seq("West Monroe"), Seq(
    ("Cinemark Tinseltown West Monroe", "Cinemark Tinseltown West Monroe", "cinemark-tinseltown-west-monroe", "Louisiana", "West Monroe")
  ))
  private def p_west_plains: R = ("west-plains", "west-plains-missouri", "West Plains", "Missouri", 36.7281, -91.8524, "America/Chicago", false, Seq("West Plains"), Seq(
    ("Glass Sword Cinema 6", "Glass Sword Cinema 6", "glass-sword-cinema-6", "Missouri", "West Plains")
  ))
  private def p_west_point: R = ("west-point", "west-point-nebraska", "West Point", "Nebraska", 41.8417, -96.7086, "America/Chicago", false, Seq("West Point"), Seq(
    ("West Point Community Theatre", "West Point Community Theatre", "west-point-community-theatre", "Nebraska", "West Point")
  ))
  private def p_west_yellowstone: R = ("west-yellowstone", "west-yellowstone-montana", "West Yellowstone", "Montana", 44.6621, -111.1041, "America/Denver", false, Seq("West Yellowstone"), Seq(
    ("Yellowstone Giant Screen Theatre", "Yellowstone Giant Screen Theatre", "yellowstone-giant-screen-theatre", "Montana", "West Yellowstone")
  ))
  private def p_westbrook: R = ("westbrook", "westbrook-maine", "Westbrook", "Maine", 43.677, -70.3712, "America/New_York", true, Seq("Westbrook", "Portland", "South Portland", "North Windham", "Falmouth"), Seq(
    ("Apple Cinemas Westbrook", "Apple Cinemas Westbrook", "apple-cinemas-westbrook", "Maine", "Westbrook"),
    ("Prides Corner Drive-In", "Prides Corner Drive-In", "prides-corner-drive-in", "Maine", "Westbrook"),
    ("Entertainment Cinemas Portland", "Entertainment Cinemas Portland", "entertainment-cinemas-portland", "Maine", "South Portland"),
    ("Flagship Cinemas Falmouth", "Flagship Cinemas Falmouth", "flagship-cinemas-falmouth", "Maine", "Falmouth"),
    ("Patriot Nickelodeon Portland", "Patriot Nickelodeon Portland", "patriot-nickelodeon-portland", "Maine", "Portland"),
    ("Smitty's Cinema Windham", "Smitty's Cinema Windham", "smittys-cinema-windham", "Maine", "North Windham")
  ))
  private def p_westcliffe: R = ("westcliffe", "westcliffe-colorado", "Westcliffe", "Colorado", 38.1347, -105.4658, "America/Denver", false, Seq("Westcliffe"), Seq(
    ("Jones Theater Westcliff", "Jones Theater Westcliff", "jones-theater-westcliff", "Colorado", "Westcliffe")
  ))
  private def p_westminster_maryland: R = ("westminster-maryland", "westminster-maryland", "Westminster", "Maryland", 39.5754, -76.9958, "America/New_York", true, Seq("Westminster", "Hanover"), Seq(
    ("R/C Westminster Movies 9", "R/C Westminster Movies 9", "rc-westminster-movies-9", "Maryland", "Westminster"),
    ("R/C Hanover Movies 16", "R/C Hanover Movies 16", "rc-hanover-movies-16", "Pennsylvania", "Hanover")
  ))
  private def p_westons_mills: R = ("westons-mills", "westons-mills-new-york", "Westons Mills", "New York", 42.0623, -78.3772, "America/New_York", true, Seq("Westons Mills", "Allegany"), Seq(
    ("AMC Allegany 8", "AMC Allegany 8", "amc-allegany-8", "New York", "Allegany"),
    ("Portville Drive-In", "Portville Drive-In", "portville-drive-in-olean", "New York", "Portville")
  ))
  private def p_wheatland: R = ("wheatland", "wheatland-wyoming", "Wheatland", "Wyoming", 42.0541, -104.953, "America/Denver", false, Seq("Wheatland"), Seq(
    ("Cinema West Wheatland", "Cinema West Wheatland", "cinema-west-wheatland", "Wyoming", "Wheatland")
  ))
  private def p_wheaton: R = ("wheaton", "wheaton-minnesota", "Wheaton", "Minnesota", 45.8044, -96.4992, "America/Chicago", false, Seq("Wheaton"), Seq(
    ("Gopher Theatre Wheaton", "Gopher Theatre Wheaton", "gopher-theatre-wheaton", "Minnesota", "Wheaton")
  ))
  private def p_white_plains: R = ("white-plains", "white-plains-new-york", "White Plains", "New York", 41.034, -73.7629, "America/New_York", true, Seq("White Plains", "Port Chester", "Mamaroneck", "Nanuet", "Mount Kisco", "Manhasset", "Pleasantville", "West Nyack"), Seq(
    ("AMC Palisades 21", "AMC Palisades 21", "amc-palisades-21", "New York", "West Nyack"),
    ("Regal Nanuet", "Regal Nanuet", "regal-nanuet", "New York", "Nanuet"),
    ("AB Cinemas Mt. Kisco", "AB Cinemas Mt. Kisco", "ab-cinemas-mt-kisco", "New York", "Mount Kisco"),
    ("AMC Port Chester 14", "AMC Port Chester 14", "amc-port-chester-14", "New York", "Port Chester"),
    ("Apple White Plains City Center", "Apple White Plains City Center", "apple-white-plains-city-center", "New York", "White Plains"),
    ("Jacob Burns Film Center Pleasantville", "Jacob Burns Film Center Pleasantville", "jacob-burns-film-center-pleasantville", "New York", "Pleasantville"),
    ("Mamaroneck Cinemas", "Mamaroneck Cinemas", "mamaroneck-cinemas", "New York", "Mamaroneck"),
    ("Manhasset Cinemas", "Manhasset Cinemas", "manhasset-cinemas", "New York", "Manhasset")
  ))
  private def p_whitehall: R = ("whitehall", "whitehall-montana", "Whitehall", "Montana", 45.8708, -112.0975, "America/Denver", false, Seq("Whitehall"), Seq(
    ("Star Theatre Whitehall", "Star Theatre Whitehall", "star-theatre-whitehall", "Montana", "Whitehall")
  ))
  private def p_whitewright: R = ("whitewright", "whitewright-texas", "Whitewright", "Texas", 33.5129, -96.3925, "America/Chicago", false, Seq("Whitewright"), Seq(
    ("Odeum Theatre", "Odeum Theatre", "odeum-theatre", "Texas", "Whitewright")
  ))
  private def p_wickenburg: R = ("wickenburg", "wickenburg-arizona", "Wickenburg", "Arizona", 33.9686, -112.7296, "America/Phoenix", false, Seq("Wickenburg"), Seq(
    ("Saguaro Theatre", "Saguaro Theatre", "saguaro-theatre", "Arizona", "Wickenburg")
  ))
  private def p_wildwood: R = ("wildwood", "wildwood-new-jersey", "Wildwood", "New Jersey", 38.9918, -74.8149, "America/New_York", true, Seq("Wildwood", "Rio Grande", "Stone Harbor"), Seq(
    ("Cape Square Cinema", "Cape Square Cinema", "cape-square-cinema", "New Jersey", "Rio Grande"),
    ("Harbor Square Theatre", "Harbor Square Theatre", "harbor-square-theatre", "New Jersey", "Stone Harbor"),
    ("Old Movies By The Sea", "Old Movies By The Sea", "old-movies-by-the-sea", "New Jersey", "Wildwood")
  ))
  private def p_wilkes_barre: R = ("wilkes-barre", "wilkes-barre-pennsylvania", "Wilkes-Barre", "Pennsylvania", 41.2459, -75.8813, "America/New_York", true, Seq("Wilkes-Barre", "Glen Lyon"), Seq(
    ("Garden Drive-in Hunlock Creek", "Garden Drive-in Hunlock Creek", "garden-drive-in-hunlock-creek", "Pennsylvania", "Hunlock Creek"),
    ("R/C Wilkes Barre Movies 14", "R/C Wilkes Barre Movies 14", "rc-wilkes-barre-movies-14", "Pennsylvania", "Wilkes-Barre")
  ))
  private def p_willcox: R = ("willcox", "willcox-arizona", "Willcox", "Arizona", 32.2529, -109.832, "America/Phoenix", false, Seq("Willcox"), Seq(
    ("Willcox Historic Theater", "Willcox Historic Theater", "willcox-historic-theater", "Arizona", "Willcox")
  ))
  private def p_williamsburg: R = ("williamsburg", "williamsburg-virginia", "Williamsburg", "Virginia", 37.2707, -76.7075, "America/New_York", true, Seq("Williamsburg", "Gloucester Point"), Seq(
    ("Movie Tavern Williamsburg", "Movie Tavern Williamsburg", "movie-tavern-williamsburg", "Virginia", "Williamsburg"),
    ("Regal New Town Williamsburg", "Regal New Town Williamsburg", "regal-new-town-williamsburg", "Virginia", "Williamsburg"),
    ("York River Crossing Cinemas", "York River Crossing Cinemas", "york-river-crossing-cinemas", "Virginia", "Gloucester")
  ))
  private def p_williamsport: R = ("williamsport", "williamsport-pennsylvania", "Williamsport", "Pennsylvania", 41.2412, -77.0011, "America/New_York", true, Seq("Williamsport", "Lock Haven", "Muncy", "Montgomery"), Seq(
    ("AMC CLASSIC Williamsport 11", "AMC CLASSIC Williamsport 11", "amc-classic-williamsport-11", "Pennsylvania", "Williamsport"),
    ("Roxy Theatre Lock Haven", "Roxy Theatre Lock Haven", "roxy-theatre-lock-haven", "Pennsylvania", "Lock Haven"),
    ("The District Cinema at Lycoming Valley", "The District Cinema at Lycoming Valley", "the-district-cinema-at-lycoming-valley", "Pennsylvania", "Pennsdale"),
    ("Pike Drive-in Montgomery", "Pike Drive-in Montgomery", "pike-drive-in-montgomery-3", "Pennsylvania", "Montgomery")
  ))
  private def p_williston: R = ("williston", "williston-north-dakota", "Williston", "North Dakota", 48.147, -103.618, "America/Chicago", false, Seq("Williston"), Seq(
    ("Williston Grand Theatre", "Williston Grand Theatre", "williston-grand-theatre", "North Dakota", "Williston")
  ))
  private def p_willits: R = ("willits", "willits-california", "Willits", "California", 39.4096, -123.3556, "America/Los_Angeles", false, Seq("Willits"), Seq(
    ("Noyo Theatre Willits", "Noyo Theatre Willits", "noyo-theatre-willits", "California", "Willits")
  ))
  private def p_willmar: R = ("willmar", "willmar-minnesota", "Willmar", "Minnesota", 45.1219, -95.0433, "America/Chicago", true, Seq("Willmar", "Paynesville", "Spicer"), Seq(
    ("Golden Ticket Willmar Kandi 6", "Golden Ticket Willmar Kandi 6", "golden-ticket-willmar-kandi-6", "Minnesota", "Willmar"),
    ("Spicer Cinema", "Spicer Cinema", "spicer-cinema", "Minnesota", "Spicer"),
    ("Koronis Theater", "Koronis Theater", "koronis-theater", "Minnesota", "Paynesville")
  ))
  private def p_wilmington_delaware: R = ("wilmington-delaware", "wilmington-delaware", "Wilmington", "Delaware", 39.746, -75.5466, "America/New_York", false, Seq("Wilmington"), Seq(
    ("Cinemark Wilmington 10", "Cinemark Wilmington 10", "cinemark-wilmington-10", "Delaware", "Wilmington"),
    ("Penn Cinema Riverfront Wilmington", "Penn Cinema Riverfront Wilmington", "penn-cinema-riverfront-wilmington", "Delaware", "Wilmington"),
    ("Regal Brandywine Town Center", "Regal Brandywine Town Center", "regal-brandywine-town-center", "Delaware", "Wilmington"),
    ("Screening Room at 1313", "Screening Room at 1313", "screening-room-at-1313", "Delaware", "Wilmington"),
    ("Theatre N at Nemours", "Theatre N at Nemours", "theatre-n-at-nemours", "Delaware", "Wilmington")
  ))
  private def p_winchester: R = ("winchester", "winchester-virginia", "Winchester", "Virginia", 39.1857, -78.1633, "America/New_York", true, Seq("Winchester", "Stephens City"), Seq(
    ("Alamo Drafthouse Winchester", "Alamo Drafthouse Winchester", "alamo-drafthouse-winchester", "Virginia", "Winchester"),
    ("AMC CLASSIC Apple Blossom 12", "AMC CLASSIC Apple Blossom 12", "amc-classic-apple-blossom-12", "Virginia", "Winchester"),
    ("Historic Route 11 Drive-In", "Historic Route 11 Drive-In", "family-drive-in-stephens-city", "Virginia", "Stephens City")
  ))
  private def p_winchester_indiana: R = ("winchester-indiana", "winchester-indiana", "Winchester", "Indiana", 40.172, -84.9814, "America/Indiana/Indianapolis", false, Seq("Winchester"), Seq(
    ("Hummel Drive In Winchester", "Hummel Drive In Winchester", "hummel-drive-in-winchester", "Indiana", "Winchester"),
    ("Towne Square Cinema", "Towne Square Cinema", "towne-square-cinema", "Indiana", "Winchester")
  ))
  private def p_winchester_tennessee: R = ("winchester-tennessee", "winchester-tennessee", "Winchester", "Tennessee", 35.1859, -86.1122, "America/Chicago", true, Seq("Winchester", "Estill Springs"), Seq(
    ("Montana Drive-in Estill Springs", "Montana Drive-in Estill Springs", "montana-drive-in-estill-springs", "Tennessee", "Estill Springs"),
    ("Oldham Cinema Winchester 2", "Oldham Cinema Winchester 2", "oldham-cinema-winchester-2", "Tennessee", "Winchester")
  ))
  private def p_windom: R = ("windom", "windom-minnesota", "Windom", "Minnesota", 43.8663, -95.1169, "America/Chicago", false, Seq("Windom"), Seq(
    ("State Theater Windom", "State Theater Windom", "state-theater-windom", "Minnesota", "Windom")
  ))
  private def p_window_rock: R = ("window-rock", "window-rock-arizona", "Window Rock", "Arizona", 35.6806, -109.0526, "America/Denver", false, Seq("Window Rock"), Seq(
    ("Window Rock Cinema", "Window Rock Cinema", "window-rock-cinema", "Arizona", "Window Rock")
  ))
  private def p_winfield: R = ("winfield", "winfield-kansas", "Winfield", "Kansas", 37.2398, -96.9956, "America/Chicago", false, Seq("Winfield"), Seq(
    ("Cowley Cinema 8 Winfield", "Cowley Cinema 8 Winfield", "cowley-cinema-8-winfield", "Kansas", "Winfield")
  ))
  private def p_wing: R = ("wing", "wing-north-dakota", "Wing", "North Dakota", 47.1412, -100.2794, "America/Chicago", false, Seq("Wing"), Seq(
    ("Wing Theater", "Wing Theater", "wing-theater", "North Dakota", "Wing")
  ))
  private def p_winnemucca: R = ("winnemucca", "winnemucca-nevada", "Winnemucca", "Nevada", 40.973, -117.7357, "America/Los_Angeles", false, Seq("Winnemucca"), Seq(
    ("Park Cinema Winnemucca", "Park Cinema Winnemucca", "park-cinema-winnemucca", "Nevada", "Winnemucca")
  ))
  private def p_winner: R = ("winner", "winner-south-dakota", "Winner", "South Dakota", 43.3767, -99.859, "America/Chicago", false, Seq("Winner"), Seq(
    ("Pix Theater Winner", "Pix Theater Winner", "pix-theater-winner", "South Dakota", "Winner"),
    ("Winner Drive In Theatre", "Winner Drive In Theatre", "winner-drive-in-theatre", "South Dakota", "Winner")
  ))
  private def p_winona: R = ("winona", "winona-minnesota", "Winona", "Minnesota", 44.05, -91.6393, "America/Chicago", false, Seq("Winona"), Seq(
    ("Winona 7", "Winona 7", "winona-7", "Minnesota", "Winona")
  ))
  private def p_winslow: R = ("winslow", "winslow-arizona", "Winslow", "Arizona", 35.0242, -110.6974, "America/Phoenix", false, Seq("Winslow"), Seq(
    ("The Winslow Theater", "The Winslow Theater", "the-winslow-theater", "Arizona", "West Winslow")
  ))
  private def p_winter_haven: R = ("winter-haven", "winter-haven-florida", "Winter Haven", "Florida", 28.0222, -81.7329, "America/New_York", true, Seq("Winter Haven", "Lake Wales", "Davenport"), Seq(
    ("Cinepolis Polk County", "Cinepolis Polk County", "cinepolis-polk-county", "Florida", "Davenport"),
    ("CMX Grand Winter Haven 10", "CMX Grand Winter Haven 10", "cmx-grand-winter-haven-10", "Florida", "Winter Haven"),
    ("Regal Eagle Ridge Mall", "Regal Eagle Ridge Mall", "regal-eagle-ridge-mall", "Florida", "Lake Wales")
  ))
  private def p_winter_park: R = ("winter-park", "winter-park-florida", "Winter Park", "Florida", 28.6, -81.3392, "America/New_York", true, Seq("Winter Park", "Deltona", "Altamonte Springs", "Oviedo", "Lake Mary"), Seq(
    ("Aloma Cinema Grill Winter Park", "Aloma Cinema Grill Winter Park", "aloma-cinema-grill-winter-park", "Florida", "Winter Park"),
    ("Regal Winter Park Village", "Regal Winter Park Village", "regal-winter-park-village", "Florida", "Winter Park"),
    ("AMC Altamonte Mall 18", "AMC Altamonte Mall 18", "amc-altamonte-mall-18", "Florida", "Altamonte Springs"),
    ("Epic Theatres West Volusia", "Epic Theatres West Volusia", "epic-theatres-west-volusia", "Florida", "Deltona"),
    ("Regal Oviedo Mall", "Regal Oviedo Mall", "regal-oviedo-mall", "Florida", "Oviedo"),
    ("Santikos Amstar Lake Mary", "Santikos Amstar Lake Mary", "santikos-amstar-lake-mary", "Florida", "Lake Mary")
  ))
  private def p_winthrop: R = ("winthrop", "winthrop-washington", "Winthrop", "Washington", 48.469, -120.1767, "America/Los_Angeles", false, Seq("Winthrop"), Seq(
    ("Barnyard Cinema Winthrop", "Barnyard Cinema Winthrop", "barnyard-cinema-winthrop", "Washington", "Winthrop")
  ))
  private def p_wolf_point: R = ("wolf-point", "wolf-point-montana", "Wolf Point", "Montana", 48.0906, -105.6406, "America/Denver", false, Seq("Wolf Point"), Seq(
    ("Prairie Cinema Wolf Point", "Prairie Cinema Wolf Point", "prairie-cinema-wolf-point", "Montana", "Wolf Point")
  ))
  private def p_woodbridge: R = ("woodbridge", "woodbridge-virginia", "Woodbridge", "Virginia", 38.6582, -77.2497, "America/New_York", true, Seq("Woodbridge", "Springfield", "Triangle"), Seq(
    ("Alamo Drafthouse Woodbridge", "Alamo Drafthouse Woodbridge", "alamo-drafthouse-woodbridge", "Virginia", "Woodbridge"),
    ("AMC Potomac Mills 18", "AMC Potomac Mills 18", "amc-potomac-mills-18", "Virginia", "Woodbridge"),
    ("Media Of Honor Theatre", "Media Of Honor Theatre", "media-of-honor-theatre", "Virginia", "Triangle"),
    ("Regal Springfield Town Center", "Regal Springfield Town Center", "regal-springfield-town-center", "Virginia", "Springfield")
  ))
  private def p_woodbury: R = ("woodbury", "woodbury-minnesota", "Woodbury", "Minnesota", 44.9239, -92.9594, "America/Chicago", true, Seq("Woodbury", "Roseville", "Inver Grove Heights", "Oakdale", "White Bear Lake", "River Falls", "Hudson"), Seq(
    ("Alamo Drafthouse Woodbury", "Alamo Drafthouse Woodbury", "alamo-drafthouse-woodbury", "Minnesota", "Woodbury"),
    ("Woodbury 10", "Woodbury 10", "woodbury-10", "Minnesota", "Woodbury"),
    ("AMC Inver Grove 16", "AMC Inver Grove 16", "amc-inver-grove-16", "Minnesota", "Inver Grove Heights"),
    ("AMC Rosedale 14", "AMC Rosedale 14", "amc-rosedale-14", "Minnesota", "Roseville"),
    ("Emagine White Bear", "Emagine White Bear", "emagine-white-bear", "Minnesota", "White Bear Lake"),
    ("Falls Theatre – River Falls", "Falls Theatre – River Falls", "falls-theatre---river-falls", "Wisconsin", "Black River Falls"),
    ("Hudson 12 Theatre", "Hudson 12 Theatre", "hudson-12-theatre", "Wisconsin", "Hudson"),
    ("Marcus Oakdale Cinema", "Marcus Oakdale Cinema", "marcus-oakdale-cinema", "Minnesota", "Oakdale")
  ))
  private def p_woodland_park: R = ("woodland-park", "woodland-park-colorado", "Woodland Park", "Colorado", 38.9939, -105.0569, "America/Denver", false, Seq("Woodland Park"), Seq(
    ("Gold Hill Theatres Woodland Park", "Gold Hill Theatres Woodland Park", "gold-hill-theatres-woodland-park", "Colorado", "Woodland Park")
  ))
  private def p_woodward: R = ("woodward", "woodward-oklahoma", "Woodward", "Oklahoma", 36.4337, -99.3904, "America/Chicago", false, Seq("Woodward"), Seq(
    ("Lakeside Woodward 6", "Lakeside Woodward 6", "lakeside-woodward-6", "Oklahoma", "Woodward")
  ))
  private def p_wooster: R = ("wooster", "wooster-ohio", "Wooster", "Ohio", 40.8052, -81.9365, "America/New_York", true, Seq("Wooster", "Hayesville"), Seq(
    ("Lyric Theater Wooster", "Lyric Theater Wooster", "lyric-theater-wooster", "Ohio", "Wooster"),
    ("Hayesville Opera House", "Hayesville Opera House", "hayesville-opera-house", "Ohio", "Hayesville")
  ))
  private def p_worland: R = ("worland", "worland-wyoming", "Worland", "Wyoming", 44.0169, -107.9554, "America/Denver", false, Seq("Worland"), Seq(
    ("Washakie Twin Cinemas", "Washakie Twin Cinemas", "washakie-twin-cinemas", "Wyoming", "Worland")
  ))
  private def p_worthington: R = ("worthington", "worthington-minnesota", "Worthington", "Minnesota", 43.62, -95.5964, "America/Chicago", true, Seq("Worthington", "Sibley"), Seq(
    ("Max Theatres Sibley", "Max Theatres Sibley", "max-theatres-sibley", "Iowa", "Sibley"),
    ("New Grand Theatre Worthington", "New Grand Theatre Worthington", "new-grand-theatre-worthington", "Minnesota", "Worthington")
  ))
  private def p_wrangell: R = ("wrangell", "wrangell-alaska", "Wrangell", "Alaska", 56.4708, -132.3767, "America/Sitka", false, Seq("Wrangell"), Seq(
    ("Nolan Center Theatre Wrangell", "Nolan Center Theatre Wrangell", "nolan-center-theatre-wrangell", "Alaska", "Wrangell")
  ))
  private def p_wray: R = ("wray", "wray-colorado", "Wray", "Colorado", 40.0758, -102.2232, "America/Denver", false, Seq("Wray"), Seq(
    ("Cliff Theater Wray", "Cliff Theater Wray", "cliff-theater-wray", "Colorado", "Wray")
  ))
  private def p_wytheville: R = ("wytheville", "wytheville-virginia", "Wytheville", "Virginia", 36.9485, -81.0848, "America/New_York", false, Seq("Wytheville"), Seq(
    ("Marquee Wytheville Commons 8", "Marquee Wytheville Commons 8", "marquee-wytheville-commons-8", "Virginia", "Wytheville"),
    ("Millwald Theatre", "Millwald Theatre", "millwald-theatre", "Virginia", "Wytheville")
  ))
  private def p_yakima: R = ("yakima", "yakima-washington", "Yakima", "Washington", 46.6021, -120.5059, "America/Los_Angeles", true, Seq("Yakima", "Union Gap"), Seq(
    ("Majestic Union Gap", "Majestic Union Gap", "majestic-union-gap", "Washington", "Union Gap"),
    ("Orion Cinema Yakima", "Orion Cinema Yakima", "orion-cinema-yakima", "Washington", "Yakima"),
    ("Yakima Cinema", "Yakima Cinema", "yakima-cinema", "Washington", "Yakima")
  ))
  private def p_yankton: R = ("yankton", "yankton-south-dakota", "Yankton", "South Dakota", 42.8711, -97.3973, "America/Chicago", false, Seq("Yankton"), Seq(
    ("Cinema Magic Yankton", "Cinema Magic Yankton", "cinema-magic-yankton", "South Dakota", "Yankton")
  ))
  private def p_yazoo_city: R = ("yazoo-city", "yazoo-city-mississippi", "Yazoo City", "Mississippi", 32.8551, -90.4056, "America/Chicago", false, Seq("Yazoo City"), Seq(
    ("Plaza Twin Cinema Yazoo City", "Plaza Twin Cinema Yazoo City", "plaza-twin-cinema-yazoo-city", "Mississippi", "Yazoo City")
  ))
  private def p_yelm: R = ("yelm", "yelm-washington", "Yelm", "Washington", 46.942, -122.606, "America/Los_Angeles", true, Seq("Yelm", "Eatonville"), Seq(
    ("Yelm Cinemas", "Yelm Cinemas", "yelm-cinemas", "Washington", "Yelm"),
    ("Roxy Theater Eatonville", "Roxy Theater Eatonville", "roxy-theater-eatonville", "Washington", "Eatonville")
  ))
  private def p_yerington: R = ("yerington", "yerington-nevada", "Yerington", "Nevada", 38.9858, -119.1629, "America/Los_Angeles", false, Seq("Yerington"), Seq(
    ("The Yerington Movie House", "The Yerington Movie House", "the-yerington-movie-house", "Nevada", "Yerington")
  ))
  private def p_york: R = ("york", "york-pennsylvania", "York", "Pennsylvania", 39.9626, -76.7277, "America/New_York", false, Seq("York"), Seq(
    ("R/C Queensgate Movies York 13", "R/C Queensgate Movies York 13", "rc-queensgate-movies-york-13", "Pennsylvania", "York"),
    ("Regal West Manchester York", "Regal West Manchester York", "regal-west-manchester-york", "Pennsylvania", "York"),
    ("Strand Theatre York", "Strand Theatre York", "strand-theatre-york", "Pennsylvania", "York")
  ))
  private def p_york_nebraska: R = ("york-nebraska", "york-nebraska", "York", "Nebraska", 40.8681, -97.592, "America/Chicago", false, Seq("York"), Seq(
    ("Sun Theatre York", "Sun Theatre York", "sun-theatre-york", "Nebraska", "York")
  ))
  private def p_youngstown: R = ("youngstown", "youngstown-ohio", "Youngstown", "Ohio", 41.0998, -80.6495, "America/New_York", false, Seq("Youngstown"), Seq(
    ("Cinemark Tinseltown Boardman", "Cinemark Tinseltown Boardman", "cinemark-tinseltown-boardman", "Ohio", "Youngstown"),
    ("Golden Star Austintown Cinema", "Golden Star Austintown Cinema", "golden-star-austintown-cinema", "Ohio", "Youngstown"),
    ("Regal South Youngstown", "Regal South Youngstown", "regal-south-youngstown", "Ohio", "Youngstown")
  ))
  private def p_yuba_city: R = ("yuba-city", "yuba-city-california", "Yuba City", "California", 39.1405, -121.6169, "America/Los_Angeles", false, Seq("Yuba City"), Seq(
    ("Cinemark Yuba City", "Cinemark Yuba City", "cinemark-yuba-city", "California", "Yuba City")
  ))
  private def p_yucca_valley: R = ("yucca-valley", "yucca-valley-california", "Yucca Valley", "California", 34.1142, -116.4322, "America/Los_Angeles", false, Seq("Yucca Valley"), Seq(
    ("Paradigm Cinemas Yucca Valley", "Paradigm Cinemas Yucca Valley", "paradigm-cinemas-yucca-valley", "California", "Yucca Valley")
  ))
  private def p_yulee: R = ("yulee", "yulee-florida", "Yulee", "Florida", 30.6319, -81.6065, "America/New_York", true, Seq("Yulee", "St Marys"), Seq(
    ("KingsBay Cinema 9", "KingsBay Cinema 9", "kingsbay-cinema-9", "Georgia", "Saint Marys Hills"),
    ("AMC CLASSIC Yulee 10", "AMC CLASSIC Yulee 10", "amc-classic-yulee-10", "Florida", "Yulee")
  ))
  private def p_yuma: R = ("yuma", "yuma-arizona", "Yuma", "Arizona", 32.7253, -114.6244, "America/Phoenix", false, Seq("Yuma"), Seq(
    ("Harkins Yuma Palms 14", "Harkins Yuma Palms 14", "harkins-yuma-palms-14", "Arizona", "Yuma"),
    ("Regency Main Street Cinemas Yuma", "Regency Main Street Cinemas Yuma", "regency-main-street-cinemas-yuma", "Arizona", "Yuma")
  ))
  private def p_yuma_colorado: R = ("yuma-colorado", "yuma-colorado", "Yuma", "Colorado", 40.1222, -102.7252, "America/Denver", false, Seq("Yuma"), Seq(
    ("Yuma Theatre", "Yuma Theatre", "yuma-theatre", "Colorado", "Yuma")
  ))
  private def p_zanesville: R = ("zanesville", "zanesville-ohio", "Zanesville", "Ohio", 39.9404, -82.0132, "America/New_York", false, Seq("Zanesville"), Seq(
    ("Cinemark Colony Square Mall", "Cinemark Colony Square Mall", "cinemark-colony-square-mall", "Ohio", "Zanesville")
  ))
  private def p_zumbrota: R = ("zumbrota", "zumbrota-minnesota", "Zumbrota", "Minnesota", 44.2941, -92.6691, "America/Chicago", false, Seq("Zumbrota"), Seq(
    ("State Zumbrota", "State Zumbrota", "state-zumbrota", "Minnesota", "Zumbrota")
  ))

  private def chunk0: Seq[R] = Seq(p_new_york, p_los_angeles, p_chicago, p_houston, p_phoenix, p_philadelphia, p_san_antonio, p_san_diego, p_dallas, p_jacksonville, p_fort_worth, p_san_jose, p_austin, p_columbus, p_charlotte, p_indianapolis, p_san_francisco, p_seattle, p_denver, p_washington, p_nashville, p_oklahoma_city, p_el_paso, p_boston, p_portland, p_detroit, p_las_vegas, p_memphis, p_louisville, p_baltimore, p_albuquerque, p_milwaukee, p_tucson, p_fresno, p_sacramento, p_atlanta, p_miami, p_omaha, p_raleigh, p_kansas_city)
  private def chunk1: Seq[R] = Seq(p_long_beach, p_mesa, p_colorado_springs, p_virginia_beach, p_oakland, p_san_juan, p_tampa, p_tulsa, p_minneapolis, p_wichita, p_arlington, p_bakersfield, p_cleveland, p_new_orleans, p_aurora, p_honolulu, p_anaheim, p_orlando, p_lexington, p_riverside, p_corpus_christi, p_cincinnati, p_santa_ana, p_stockton_california, p_pittsburgh, p_saint_paul, p_lincoln, p_anchorage, p_henderson, p_greensboro, p_plano, p_newark, p_madison, p_st_louis, p_chula_vista, p_toledo, p_jersey_city, p_reno, p_chandler, p_fort_wayne)
  private def chunk2: Seq[R] = Seq(p_buffalo, p_durham, p_st_petersburg, p_irvine, p_laredo, p_lubbock, p_gilbert, p_winston_salem, p_glendale, p_norfolk_virginia, p_hialeah, p_garland, p_scottsdale, p_irving, p_boise, p_chesapeake, p_north_las_vegas, p_fremont, p_spokane, p_baton_rouge, p_richmond, p_tacoma, p_san_bernardino, p_salt_lake_city, p_huntsville, p_des_moines, p_fontana, p_modesto, p_rochester, p_arlington_virginia, p_oxnard, p_columbus_georgia, p_worcester, p_moreno_valley, p_bayamon, p_little_rock, p_fayetteville, p_huntington_beach, p_tallahassee, p_yonkers)
  private def chunk3: Seq[R] = Seq(p_glendale_california, p_cypress, p_aurora_illinois, p_amarillo, p_akron, p_vancouver, p_birmingham, p_montgomery, p_grand_rapids, p_peoria_arizona, p_providence, p_knoxville, p_grand_prairie, p_shreveport, p_brownsville, p_overland_park, p_newport_news, p_mobile, p_fort_lauderdale, p_santa_clarita, p_chattanooga, p_spring_valley, p_santa_rosa, p_eugene, p_tempe, p_oceanside, p_salem, p_garden_grove, p_rancho_cucamonga, p_cape_coral, p_sioux_falls, p_ontario_california, p_fort_collins, p_carolina, p_springfield_missouri, p_elk_grove, p_clarksville, p_pembroke_pines, p_murfreesboro, p_port_saint_lucie)
  private def chunk4: Seq[R] = Seq(p_corona, p_mckinney, p_lancaster_california, p_cary, p_alexandria_virginia, p_palmdale, p_hayward, p_salinas, p_sunnyvale, p_frisco, p_springfield_massachusetts, p_pasadena, p_jackson_mississippi, p_kansas_city_kansas, p_lakewood, p_escondido, p_hollywood, p_rockford, p_joliet, p_savannah, p_naperville, p_gainesville, p_mesquite, p_syracuse, p_torrance, p_surprise, p_columbia, p_pasadena_california, p_orange, p_fullerton, p_killeen, p_mcallen, p_bellevue, p_metairie, p_ponce, p_hampton, p_west_valley_city, p_dayton, p_olathe, p_warren)
  private def chunk5: Seq[R] = Seq(p_charleston_south_carolina, p_midland_texas, p_waco, p_sterling_heights, p_denton, p_cedar_rapids, p_roseville, p_visalia, p_coral_springs, p_thousand_oaks, p_columbia_missouri, p_elizabeth, p_stamford, p_concord, p_norman, p_athens, p_kent, p_simi_valley, p_santa_clara, p_topeka, p_abilene, p_amherst, p_victorville, p_vallejo, p_lafayette, p_chico, p_hartford, p_berkeley, p_west_palm_beach, p_allentown, p_evansville, p_palm_bay, p_fargo, p_clearwater, p_independence, p_billings, p_ann_arbor, p_westminster, p_round_rock, p_wilmington)
  private def chunk6: Seq[R] = Seq(p_arvada, p_beaumont, p_provo, p_peoria, p_carlsbad, p_odessa, p_springfield, p_downey, p_costa_mesa, p_fairfield, p_lansing, p_rochester_minnesota, p_elgin, p_west_jordan, p_inglewood, p_tuscaloosa, p_richardson, p_lowell, p_gresham, p_cambridge, p_high_point, p_manchester, p_temecula, p_murrieta, p_centennial, p_richmond_california, p_pueblo, p_pearland, p_waterbury, p_greeley, p_west_covina, p_north_charleston, p_everett, p_college_station, p_pompano_beach, p_norwalk, p_boulder, p_broken_arrow, p_daly_city, p_burbank)
  private def chunk7: Seq[R] = Seq(p_green_bay, p_santa_maria, p_universal_city, p_wichita_falls, p_lakeland, p_clovis, p_lewisville, p_tyler, p_el_cajon, p_san_mateo, p_brandon, p_rialto, p_davenport, p_edison, p_hillsboro, p_las_cruces, p_south_bend, p_albany, p_vista, p_davie, p_renton, p_roanoke, p_aberdeen, p_aberdeen_maryland, p_aberdeen_mississippi, p_abingdon, p_ada, p_adams, p_adrian, p_afton, p_agawam, p_aguadilla, p_ainsworth, p_airway_heights, p_aitkin, p_alamogordo, p_alamosa, p_albany_georgia, p_albany_oregon, p_albemarle)
  private def chunk8: Seq[R] = Seq(p_albertville, p_albion, p_albion_nebraska, p_alexander_city, p_alexandria, p_alexandria_bay, p_alexandria_louisiana, p_algona, p_alhambra, p_alice, p_allen, p_alliance, p_alma, p_alpena, p_alpharetta, p_alpine, p_alturas, p_altus, p_alva, p_amelia, p_amenia, p_ames, p_amherst_massachusetts, p_anaconda, p_anadarko, p_andalusia, p_anderson, p_andover, p_andrews, p_annapolis, p_anniston, p_antigo, p_antlers, p_appleton, p_arapahoe, p_ardmore, p_arecibo, p_arnold, p_arnold_nebraska, p_arroyo_grande)
  private def chunk9: Seq[R] = Seq(p_ashburn, p_asheboro, p_asheville, p_ashland, p_ashland_ohio, p_ashland_wisconsin, p_ashtabula, p_aspen, p_astoria, p_atascadero, p_atchison, p_athens_ohio, p_athens_tennessee, p_athens_texas, p_athol, p_atlantic_city, p_atmore, p_atwood, p_auburn, p_auburn_california, p_auburn_maine, p_auburn_washington, p_augusta, p_augusta_georgia, p_aurora_nebraska, p_austin_minnesota, p_ava, p_avalon, p_avon, p_avondale, p_bad_axe, p_bainbridge_island, p_baker_city, p_bangor, p_bar_harbor, p_barberton, p_barrington, p_barstow, p_bartlesville, p_bastrop)
  private def chunk10: Seq[R] = Seq(p_batesville, p_battle_creek, p_baudette, p_baxter, p_bay_city, p_baytown, p_beacon, p_beatrice, p_beavercreek, p_beaverton, p_beckley, p_bedford, p_bedford_indiana, p_beeville, p_belfield, p_belle_plaine, p_belleville, p_bellingham, p_bellmore, p_beloit, p_belton, p_bemidji, p_bend, p_benkelman, p_bennington, p_bensalem, p_benson, p_benton, p_benton_harbor, p_berryville, p_bethany, p_bethel, p_bethel_maine, p_bethesda, p_bethlehem, p_beverly_hills, p_bicknell, p_big_rapids, p_big_sky, p_big_spring)
  private def chunk11: Seq[R] = Seq(p_big_timber, p_binghamton, p_birmingham_michigan, p_bishop, p_bismarck, p_blackduck, p_blacksburg, p_bloomington, p_bloomington_illinois, p_bloomington_minnesota, p_bloomsburg, p_bluefield, p_blytheville, p_boca_raton, p_boerne, p_bolingbrook, p_bolivar, p_bolivar_tennessee, p_bonita_springs, p_bonners_ferry, p_boone, p_borger, p_boscobel, p_bossier_city, p_bottineau, p_bowling_green, p_bowman, p_bozeman, p_bradenton, p_brady, p_branson, p_brattleboro, p_breckenridge, p_bremerton, p_brenham, p_brentwood, p_bridgeport, p_bridgton, p_brigham_city, p_bristol)
  private def chunk12: Seq[R] = Seq(p_britton, p_broken_bow, p_brookfield, p_brookfield_wisconsin, p_brookhaven, p_brookings, p_brookings_oregon, p_brownsburg, p_brunswick, p_bryan, p_bryant, p_buchanan, p_buena_park, p_buena_vista, p_buffalo_oklahoma, p_buffalo_wyoming, p_burley, p_burlington, p_burlington_colorado, p_burlington_north_carolina, p_burlington_vermont, p_burlington_wisconsin, p_burns, p_burwell, p_butler, p_cabot, p_cadillac, p_calabasas, p_calais, p_caldwell, p_california, p_calvert_city, p_camarillo, p_camas, p_cambridge_ohio, p_camden, p_cameron, p_campbellsville, p_canadian, p_canby)
  private def chunk13: Seq[R] = Seq(p_cando, p_canon_city, p_canton, p_canton_illinois, p_cape_girardeau, p_carbondale, p_carbondale_colorado, p_carlisle, p_carlsbad_new_mexico, p_carmel, p_carnegie, p_carrington, p_carroll, p_carrollton, p_carson_city, p_carthage, p_casa_grande, p_cascade, p_casper, p_castle_rock, p_cavalier, p_cedar_bluff, p_cedar_city, p_cedar_park, p_celina, p_center, p_centerville, p_centerville_tennessee, p_central_city, p_centralia, p_centre, p_centreville, p_chadron, p_chamberlain, p_champaign, p_chandler_oklahoma, p_chanute, p_chapel_hill, p_charles_city, p_charleston)
  private def chunk14: Seq[R] = Seq(p_charlottesville, p_cheboygan, p_checotah, p_chelan, p_cherry_hill, p_chester, p_cheyenne, p_cheyenne_wells, p_chickasha, p_chillicothe, p_chillicothe_missouri, p_chilton, p_chino, p_chippewa_falls, p_choteau, p_cicero, p_circle, p_clanton, p_clarendon, p_clarion, p_clarion_iowa, p_clay_center, p_clayton, p_clayton_georgia, p_cle_elum, p_clearfield, p_clearlake, p_cleveland_heights, p_cleveland_tennessee, p_clifton, p_clifton_park, p_clinton, p_clinton_arkansas, p_clinton_illinois, p_clinton_missouri, p_clinton_north_carolina, p_clinton_township, p_cloquet, p_clovis_new_mexico, p_coalinga)
  private def chunk15: Seq[R] = Seq(p_cody, p_coeur_d_alene, p_colby, p_coldwater, p_coldwater_kansas, p_colorado_city, p_columbia_maryland, p_columbia_mississippi, p_columbus_indiana, p_columbus_mississippi, p_columbus_nebraska, p_colville, p_commerce_city, p_concord_new_hampshire, p_concord_north_carolina, p_concordia, p_concrete, p_conrad, p_conway, p_conway_south_carolina, p_cook, p_cookeville, p_cooperstown, p_coos_bay, p_copperas_cove, p_coral_gables, p_cordele, p_cordell, p_cordova, p_corinth, p_corning, p_corning_iowa, p_corsicana, p_cortez, p_cortland, p_corvallis, p_coshocton, p_coudersport, p_council_bluffs, p_covington)
  private def chunk16: Seq[R] = Seq(p_craig, p_cranberry_township, p_crawfordsville, p_crescent_city, p_crested_butte, p_crestview, p_crete, p_creve_coeur, p_crookston, p_crosby, p_crowley, p_crystal_lake, p_cullman, p_culpeper, p_curtis, p_cut_bank, p_dacula, p_dallas_oregon, p_dalton, p_danbury, p_danville, p_danville_kentucky, p_daphne, p_davis, p_dayton_washington, p_daytona_beach, p_de_pere, p_de_soto, p_dearborn, p_decatur, p_decatur_alabama, p_decatur_georgia, p_decatur_texas, p_decorah, p_dededo_village, p_defiance, p_dekalb, p_del_rio, p_delano, p_dell_rapids)
  private def chunk17: Seq[R] = Seq(p_delray_beach, p_delta, p_delta_utah, p_deming, p_demopolis, p_denison, p_dennis, p_derby, p_detroit_lakes, p_devils_lake, p_dexter, p_dickinson, p_dickson, p_dillon, p_dodge_city, p_dodgeville, p_dorado, p_dothan, p_douglas, p_douglas_wyoming, p_dover_foxcroft, p_dublin, p_dublin_georgia, p_dubuque, p_duluth, p_duluth_georgia, p_dumas, p_duncan, p_dunkirk, p_durango, p_durant, p_eads, p_eagle_butte, p_eagle_pass, p_early, p_easley, p_east_grand_forks, p_east_hampton, p_east_lansing, p_east_stroudsburg)
  private def chunk18: Seq[R] = Seq(p_east_tawas, p_east_windham, p_eastland, p_easton, p_eau_claire, p_eden, p_edenton, p_edinburg, p_edmond, p_edwards, p_edwardsville, p_effingham, p_el_centro, p_el_dorado, p_el_dorado_kansas, p_el_dorado_springs, p_el_segundo, p_elberton, p_elizabeth_city, p_elizabethtown, p_elk_city, p_elk_rapids, p_elkader, p_elkhart, p_elkins, p_elko, p_ellensburg, p_ellijay, p_elroy, p_ely, p_ely_minnesota, p_emeryville, p_emmett, p_emporia, p_enterprise, p_ephraim, p_erie, p_escanaba, p_estes_park, p_eunice)
  private def chunk19: Seq[R] = Seq(p_eureka, p_eureka_montana, p_evanston, p_evanston_wyoming, p_fairbanks, p_fairbury, p_fairfax, p_fairmont, p_fairview, p_fajardo, p_fall_river, p_fallon, p_farmington, p_farmington_maine, p_farmington_missouri, p_farmville, p_faulkton, p_fayetteville_arkansas, p_fayetteville_georgia, p_fergus_falls, p_findlay, p_fish_creek, p_fitchburg, p_fitzgerald, p_flagler, p_flagstaff, p_flint, p_florence, p_florence_alabama, p_florence_kentucky, p_florence_oregon, p_floresville, p_folsom, p_fond_du_lac, p_forest_city, p_forest_grove, p_forney, p_forrest_city, p_forsyth, p_fort_bliss)
  private def chunk20: Seq[R] = Seq(p_fort_bragg, p_fort_dodge, p_fort_lee, p_fort_moore, p_fort_morgan, p_fort_myers, p_fort_pierre, p_fort_scott, p_fort_smith, p_fort_stockton, p_fowler, p_framingham, p_frankfort, p_frankfort_michigan, p_franklin, p_franklin_west_virginia, p_frederick, p_fredericksburg, p_freeport, p_fremont_nebraska, p_friday_harbor, p_friendswood, p_front_royal, p_frostburg, p_fulton, p_gadsden, p_gainesville_georgia, p_gainesville_texas, p_gaithersburg, p_galax, p_galesburg, p_gallipolis, p_gallup, p_galveston, p_garden_city, p_garrett, p_garrison, p_gastonia, p_gatesville, p_gaylord)
  private def chunk21: Seq[R] = Seq(p_geneva, p_geneva_nebraska, p_georgetown, p_georgetown_south_carolina, p_gettysburg, p_gilford, p_gillette, p_glasgow, p_glasgow_montana, p_glen_allen, p_glendive, p_globe, p_gold_beach, p_goldsboro, p_goleta, p_gonzales, p_gonzales_texas, p_goodland, p_gordon, p_grafton, p_graham, p_granbury, p_grand_canyon_village, p_grand_island, p_grand_junction, p_grand_rapids_minnesota, p_grandville, p_grangeville, p_granite_city, p_grants, p_grants_pass, p_grapevine, p_grass_valley, p_grayling, p_great_barrington, p_great_bend, p_great_falls, p_greece, p_greenacres_city, p_greencastle)
  private def chunk22: Seq[R] = Seq(p_greeneville, p_greenfield, p_greensboro_georgia, p_greensburg, p_greensburg_indiana, p_greensburg_kansas, p_greenville, p_greenville_alabama, p_greenville_mississippi, p_greenville_north_carolina, p_greenwood, p_greenwood_indiana, p_gregory, p_grenada, p_grinnell, p_grove, p_grundy, p_guayama, p_guaynabo, p_guin, p_gulf_shores, p_gulfport, p_gunnison, p_gunnison_utah, p_guymon, p_hadensville, p_hagerstown, p_haleyville, p_hamilton, p_hammond, p_hampton_south_carolina, p_hannibal, p_hanover, p_hardin, p_hardy, p_harlan, p_harlingen, p_harlowton, p_harrisburg, p_harrisburg_illinois)
  private def chunk23: Seq[R] = Seq(p_harrison, p_harrisonburg, p_hartford_kentucky, p_harvey, p_hastings, p_hastings_michigan, p_hattiesburg, p_havelock, p_havre, p_hayward_wisconsin, p_hazard, p_hazen, p_hazleton, p_healdsburg, p_heath, p_heber_city, p_heber_springs, p_hebron, p_hebron_north_dakota, p_helena, p_henderson_kentucky, p_henderson_north_carolina, p_hendersonville, p_hendersonville_north_carolina, p_hereford, p_hermiston, p_hesperia, p_hettinger, p_hiawatha, p_hibbing, p_hickory, p_highland, p_highland_arkansas, p_hill_city, p_hilo, p_hilton_head_island, p_hinckley, p_hinesville, p_hingham, p_hobbs)
  private def chunk24: Seq[R] = Seq(p_holbrook, p_holdrege, p_holland, p_holly, p_holly_springs, p_holyoke, p_homer, p_homestead, p_hondo, p_honoka_a, p_hood_river, p_hoopeston, p_hoover, p_hopkinsville, p_hoquiam, p_hornell, p_hot_springs, p_hot_springs_south_dakota, p_houghton, p_houghton_lake, p_houlton, p_houma, p_houston_missouri, p_howell, p_hudson, p_hugoton, p_humboldt, p_huntersville, p_huntington, p_huntington_indiana, p_huntsville_texas, p_huron, p_hurricane, p_hutchinson, p_hutchinson_minnesota, p_ida_grove, p_idabel, p_idaho_falls, p_imperial, p_independence_kansas)
  private def chunk25: Seq[R] = Seq(p_indian_lake, p_indian_trail, p_indiana, p_inez, p_international_falls, p_inverness, p_ionia, p_iowa_city, p_iowa_falls, p_ironwood, p_issaquah, p_ithaca, p_jackson, p_jackson_california, p_jackson_michigan, p_jackson_ohio, p_jackson_wyoming, p_jacksonville_illinois, p_jacksonville_north_carolina, p_jacksonville_texas, p_jamestown, p_jamestown_new_york, p_jamestown_tennessee, p_janesville, p_jasper, p_jefferson_city, p_jeffersonville, p_jerseyville, p_jesup, p_johnson_city, p_johnstown, p_johnstown_new_york, p_jonesboro, p_jonesville, p_joplin, p_julesburg, p_juneau, p_jupiter, p_kahului, p_kailua_kona)
  private def chunk26: Seq[R] = Seq(p_kalamazoo, p_kalispell, p_kane, p_kankakee, p_kanopolis, p_kapolei, p_katy, p_kaunakakai, p_kayenta, p_kearney, p_keene, p_kelso, p_kemmerer, p_kenai, p_kenmare, p_kenner, p_kennesaw, p_kennett, p_kennewick, p_kenosha, p_kenton, p_keokuk, p_kernersville, p_kerrville, p_ketchum, p_kewanee, p_key_west, p_kill_devil_hills, p_kilmarnock, p_kimball, p_kingfisher, p_kingman, p_kingman_kansas, p_kingston, p_kingston_massachusetts, p_kinsley, p_kinston, p_kirkland, p_kirksville, p_kissimmee)
  private def chunk27: Seq[R] = Seq(p_klamath_falls, p_knox, p_knoxville_iowa, p_kodiak, p_kokomo, p_kutztown, p_kyle, p_la_crosse, p_la_grande, p_la_jolla, p_la_junta, p_ladysmith, p_lafayette_indiana, p_lafayette_tennessee, p_lagrange, p_laguna_niguel, p_lake_charles, p_lake_city, p_lake_city_colorado, p_lake_elsinore, p_lake_havasu_city, p_lake_jackson, p_lake_ozark, p_lake_placid, p_lakeport, p_lakewood_washington, p_lamar, p_lamar_colorado, p_lamesa, p_lamoni, p_lanai_city, p_lancaster, p_lancaster_new_hampshire, p_lancaster_new_york, p_lancaster_ohio, p_lancaster_texas, p_lancaster_wisconsin, p_lander, p_langdon, p_lansdale)
  private def chunk28: Seq[R] = Seq(p_lapeer, p_laramie, p_largo, p_larkspur, p_larned, p_las_vegas_new_mexico, p_laughlin, p_laurel, p_laurinburg, p_lawrence, p_lawrenceburg, p_lawrenceville, p_lawton, p_layton, p_le_mars, p_leachville, p_lebanon, p_lebanon_missouri, p_lebanon_ohio, p_lee_s_summit, p_leesville, p_lemmon, p_lemoore, p_leominster, p_lewisburg_west_virginia, p_lewiston, p_lewistown, p_lewistown_pennsylvania, p_lexington_nebraska, p_lexington_south_carolina, p_lexington_tennessee, p_lexington_virginia, p_libby, p_liberal, p_lima, p_limon, p_lincoln_city, p_lincoln_illinois, p_lincoln_kansas, p_lincoln_new_hampshire)
  private def chunk29: Seq[R] = Seq(p_linn, p_linton, p_lisbon, p_litchfield, p_litchfield_minnesota, p_lititz, p_little_falls, p_littleton_colorado, p_livermore, p_livingston, p_livingston_alabama, p_llano, p_lockport, p_lodi, p_logan, p_logan_west_virginia, p_logansport, p_lompoc, p_london, p_long_beach_washington, p_longmont, p_longview, p_lorain, p_los_alamos, p_los_banos, p_los_lunas, p_loveland, p_lovell, p_lovington, p_lucas, p_ludington, p_lufkin, p_lumberton, p_luverne, p_lyman, p_lynchburg, p_macomb, p_macon, p_macon_missouri, p_madawaska)
  private def chunk30: Seq[R] = Seq(p_madera, p_madison_connecticut, p_madison_indiana, p_madison_south_dakota, p_madisonville, p_madras, p_malta, p_mammoth_lakes, p_manchester_connecticut, p_manchester_iowa, p_manhattan, p_manistee, p_manistique, p_manitowoc, p_mankato, p_mansfield, p_mansfield_city, p_maquoketa, p_marathon, p_marble_falls, p_marbleton, p_marco_island, p_marianna, p_maricopa, p_marietta, p_marinette, p_marion, p_marion_illinois, p_marion_iowa, p_marion_north_carolina, p_marion_virginia, p_marksville, p_marquette, p_marshall, p_marshall_arkansas, p_marshall_missouri, p_marshalltown, p_marshfield, p_martin, p_martinsburg)
  private def chunk31: Seq[R] = Seq(p_martinsville, p_martinsville_indiana, p_marysville, p_marysville_kansas, p_maryville, p_maryville_missouri, p_mashpee, p_mason, p_mason_city, p_massena, p_matamoras, p_mattituck, p_mattoon, p_mattydale, p_maumee, p_mayaguez, p_mayfield, p_maysville, p_mayville, p_mcalester, p_mcconnelsville, p_mccook, p_mcdonough, p_mclean, p_mcminnville, p_mcminnville_tennessee, p_mcpherson, p_meadville, p_medford, p_medford_wisconsin, p_melbourne, p_mena, p_menomonie, p_mentor, p_merced, p_meridian, p_meridian_mississippi, p_merrill, p_mesilla, p_mesquite_nevada)
  private def chunk32: Seq[R] = Seq(p_metaline_falls, p_methuen, p_mexico, p_miami_beach, p_miami_oklahoma, p_miamisburg, p_michigan_city, p_middle_river, p_middlebury_village, p_middlesboro, p_middletown, p_middletown_connecticut, p_middletown_delaware, p_midland, p_midlothian, p_milbank, p_milbridge, p_miles_city, p_milford, p_milford_delaware, p_milford_new_hampshire, p_millbury, p_milledgeville, p_miller, p_millsboro, p_mineola, p_mineral_wells, p_minot, p_mishawaka, p_missoula, p_missouri_city, p_mitchell, p_moab, p_moberly, p_mobridge, p_moline, p_monett, p_monetta, p_monroe, p_monroe_wisconsin)
  private def chunk33: Seq[R] = Seq(p_monterey, p_montevideo, p_monticello, p_monticello_arkansas, p_montpelier, p_montpelier_idaho, p_montrose, p_moore, p_moorefield, p_moose_lake, p_mora, p_morehead, p_morenci, p_morgantown, p_morris, p_morristown, p_morton, p_moscow, p_moses_lake, p_mott, p_moultrie, p_mount_airy, p_mount_airy_georgia, p_mount_gilead, p_mount_lebanon, p_mount_pleasant, p_mount_pleasant_iowa, p_mount_pleasant_michigan, p_mount_pleasant_texas, p_mount_pleasant_utah, p_mount_shasta, p_mount_sterling, p_mount_vernon, p_mount_vernon_illinois, p_mount_vernon_ohio, p_mountain_grove, p_mountain_home, p_mountain_view, p_mountain_view_arkansas, p_muncie)
  private def chunk34: Seq[R] = Seq(p_murray, p_muscatine, p_muskegon, p_muskogee, p_myrtle_beach, p_nantucket, p_naples, p_nashville_illinois, p_nashville_indiana, p_national_city, p_nebraska_city, p_nederland, p_neligh, p_neosho, p_nephi, p_new_bern, p_new_braunfels, p_new_brunswick, p_new_caney, p_new_castle, p_new_lenox, p_new_london, p_new_london_wisconsin, p_new_martinsville, p_new_paltz, p_new_philadelphia, p_new_ulm, p_newark_delaware, p_newberg, p_newberry, p_newcastle, p_newport, p_newport_beach, p_newport_vermont, p_newport_washington, p_newton, p_newton_illinois, p_newton_kansas, p_niagara_falls, p_nogales)
  private def chunk35: Seq[R] = Seq(p_nome, p_norfolk, p_north_attleborough_center, p_north_conway, p_north_fort_myers, p_north_little_rock, p_north_platte, p_north_portland, p_north_virginia_beach, p_north_wilkesboro, p_northfield, p_norton, p_norton_kansas, p_norwalk_connecticut, p_norwood, p_o_fallon, p_oak_bluffs, p_oak_grove, p_oak_harbor, p_oak_ridge, p_oakes, p_oakhurst, p_oakland_maryland, p_oakley, p_oberlin, p_ocala, p_ocean_city, p_ocoee, p_oelwein, p_ogallala, p_ogden, p_okemah, p_old_forge, p_olympia, p_omak, p_onancock, p_onarga, p_onawa, p_oneida, p_oneida_tennessee)
  private def chunk36: Seq[R] = Seq(p_oneonta, p_ontario, p_ontario_ohio, p_opelika, p_opelousas, p_orangeburg, p_orem, p_oro_valley, p_orofino, p_oroville, p_oshkosh, p_oskaloosa, p_oswego, p_oswego_new_york, p_othello, p_ottawa, p_ottawa_kansas, p_ottumwa, p_owasso, p_owatonna, p_owensboro, p_oxford, p_ozark, p_pace, p_paducah, p_page, p_page_north_dakota, p_pageland, p_pago_pago, p_pagosa_springs, p_palestine, p_palm_coast, p_palm_springs, p_pampa, p_panama_city, p_panguitch, p_paonia, p_paris, p_paris_illinois, p_park_falls)
  private def chunk37: Seq[R] = Seq(p_park_rapids, p_parker, p_parkersburg, p_parsons, p_pauls_valley, p_pawhuska, p_payson, p_pearl_city, p_pell_city, p_pendleton, p_pensacola, p_perham, p_perry, p_perry_new_york, p_perryton, p_perryville, p_petaluma, p_petersburg, p_petoskey, p_pflugerville, p_philadelphia_mississippi, p_philip, p_philipsburg, p_piedmont, p_pikeville, p_pinecrest, p_pittsburg, p_pittsfield, p_plains, p_plantation, p_platte, p_platteville, p_plattsburgh, p_pleasanton, p_plentywood, p_plymouth, p_plymouth_indiana, p_plymouth_michigan, p_pocahontas, p_pocatello)
  private def chunk38: Seq[R] = Seq(p_point_arena, p_polson, p_ponca_city, p_ponderay, p_pontiac, p_pooler, p_poplar_bluff, p_port_angeles, p_port_arthur, p_port_charlotte, p_port_huron, p_port_lavaca, p_port_townsend, p_portage, p_portage_pennsylvania, p_porterville, p_portland_texas, p_portsmouth, p_portsmouth_new_hampshire, p_poteau, p_potsdam, p_pottstown, p_poughkeepsie, p_powell, p_prairie_du_chien, p_pratt, p_prattville, p_prescott_valley, p_presque_isle, p_preston, p_price, p_princeton, p_princeton_illinois, p_princeton_indiana, p_prineville, p_pryor, p_queen_creek, p_queensbury, p_quincy, p_quincy_california)
  private def chunk39: Seq[R] = Seq(p_quinnesec, p_rancho_mirage, p_randolph, p_rangeley, p_rapid_city, p_ravenna, p_rawlins, p_reading, p_red_bank, p_red_bank_tennessee, p_red_bluff, p_red_lodge, p_red_oak, p_red_wing, p_redding, p_redfield, p_redlands, p_redmond, p_rensselaer, p_rexburg, p_rhinelander, p_rice_lake, p_richfield, p_richland_center, p_richmond_indiana, p_richmond_kentucky, p_richmond_missouri, p_richmond_texas, p_ridgecrest, p_ridgeland, p_rifle, p_rio_grande_city, p_rio_rancho, p_ripley, p_ritzville, p_riverton, p_riverview, p_roanoke_rapids, p_robinson, p_rock_hill)
  private def chunk40: Seq[R] = Seq(p_rock_springs, p_rockland, p_rocky_mount, p_rogers, p_rogers_city, p_rolla, p_rolla_north_dakota, p_rome, p_rome_georgia, p_roosevelt, p_roseau, p_roseburg, p_roswell, p_roswell_georgia, p_rothschild, p_roxboro, p_rugby, p_ruidoso, p_rushville, p_russell, p_russellville, p_ruston, p_saco, p_safford, p_saginaw, p_saguache, p_saint_charles, p_saint_clairsville, p_saint_croix, p_saint_croix_falls, p_saint_francis, p_saint_george, p_saint_helens, p_saint_johns, p_saint_joseph, p_saint_marys, p_saint_michael, p_salem_massachusetts, p_salem_ohio, p_salina)
  private def chunk41: Seq[R] = Seq(p_salisbury, p_salmon, p_san_angelo, p_san_luis_obispo, p_san_marcos, p_san_marcos_texas, p_san_ramon, p_sandusky, p_sandusky_michigan, p_sandy, p_sanford_maine, p_santa_barbara, p_santa_cruz, p_santa_fe, p_santa_monica, p_santa_rosa_new_mexico, p_sapulpa, p_sarasota, p_savannah_tennessee, p_sayre, p_sayville, p_schenectady, p_schererville, p_schertz, p_schroon_lake, p_scobey, p_scottsbluff, p_scottsboro, p_scranton, p_searcy, p_seaside, p_sebring, p_sedalia, p_sedan, p_sedona, p_selinsgrove, p_selma, p_selma_alabama, p_seminole, p_senatobia)
  private def chunk42: Seq[R] = Seq(p_seneca, p_seneca_kansas, p_sevierville, p_seward, p_seymour, p_shallotte, p_shamrock, p_sharon_springs, p_shattuck, p_shawano, p_shawnee, p_shawnee_kansas, p_sheboygan, p_shelbina, p_shelby, p_shelby_montana, p_shelbyville, p_shelbyville_illinois, p_shelbyville_tennessee, p_shelton, p_sheridan, p_sherman, p_show_low, p_sidney, p_sidney_montana, p_sidney_nebraska, p_sierra_vista, p_sikeston, p_silver_city, p_silver_spring, p_silverton, p_simpsonville, p_sioux_center, p_sioux_city, p_siren, p_sisters, p_sitka, p_skowhegan, p_slidell, p_smith_center)
  private def chunk43: Seq[R] = Seq(p_smithfield, p_snyder, p_socorro, p_soda_springs, p_soledad, p_somerset, p_somerville, p_sonora, p_south_barrington, p_south_boston, p_south_fallsburg, p_south_jordan, p_south_kingstown, p_south_taft, p_southern_pines, p_southport, p_spanish_fork, p_sparks, p_sparta, p_sparta_wisconsin, p_spartanburg, p_spearfish, p_spearman, p_spencer, p_spencer_indiana, p_spencer_west_virginia, p_spirit_lake, p_spring, p_spring_hill, p_spring_hill_tennessee, p_springerville, p_springfield_colorado, p_springfield_georgia, p_springfield_ohio, p_springfield_oregon, p_springfield_vermont, p_springhill, p_springville, p_st_johns, p_st_johnsbury)
  private def chunk44: Seq[R] = Seq(p_stafford, p_stamford_texas, p_stanley, p_starke, p_starkville, p_state_college, p_statesboro, p_steamboat_springs, p_stephenville, p_sterling, p_sterling_illinois, p_stevens_point, p_stigler, p_stillwater, p_stockton, p_stony_brook, p_storm_lake, p_stowe, p_strongsville, p_stuart, p_stuart_florida, p_sturgeon_bay, p_sturgis, p_stuttgart, p_suffern, p_suffolk, p_sullivan, p_sulphur_springs, p_summerlin_south, p_summersville, p_summerville, p_sumter, p_sun_prairie, p_sunnyside, p_sunrise, p_superior, p_susanville, p_sutton, p_syracuse_kansas, p_tabor_city)
  private def chunk45: Seq[R] = Seq(p_tahlequah, p_taos, p_tavernier, p_taylorville, p_teays_valley, p_tehachapi, p_telluride, p_terre_haute, p_texarkana, p_the_colony, p_the_villages, p_the_woodlands, p_thermopolis, p_thief_river_falls, p_thomaston, p_thomasville, p_thompson_falls, p_thomson, p_three_rivers, p_tiffin, p_tigard, p_tillamook, p_tioga, p_tiptonville, p_titusville, p_tomball, p_toms_river, p_tonkawa, p_tooele, p_torrington, p_torrington_wyoming, p_tracy, p_traverse_city, p_trenton, p_tribune, p_trinidad, p_troy, p_truckee, p_truth_or_consequences, p_tuba_city)
  private def chunk46: Seq[R] = Seq(p_tucumcari, p_tulare, p_tupelo, p_tupper_lake, p_tutu, p_twentynine_palms, p_twin_falls, p_two_harbors, p_ukiah, p_ulysses, p_uniontown, p_utica, p_uvalde, p_vacaville, p_valdez, p_valdosta, p_valley_city, p_valparaiso, p_van_buren, p_van_wert, p_vandalia, p_vandergrift, p_vashon, p_vassar, p_venice, p_ventura, p_vermillion, p_vernal, p_vernon, p_vero_beach, p_viborg, p_vicksburg, p_victor, p_victoria, p_vidalia, p_vienna_bend, p_vincennes, p_vineland, p_vineland_minnesota, p_vinita)
  private def chunk47: Seq[R] = Seq(p_virginia, p_viroqua, p_wabash, p_waconia, p_wadena, p_wagner, p_waikoloa, p_waimea, p_waite_park, p_waldorf, p_waldron, p_walla_walla, p_walsenburg, p_walterboro, p_walton, p_warner_robins, p_warren_minnesota, p_warren_ohio, p_warrensburg, p_warwick, p_washington_illinois, p_washington_indiana, p_washington_iowa, p_washington_missouri, p_wasilla, p_waterford, p_waterloo, p_watertown, p_watertown_new_york, p_waterville, p_watford_city, p_watsonville, p_wauseon, p_wauwatosa, p_waycross, p_wayne, p_wayne_nebraska, p_waynesboro, p_waynesville, p_waynesville_missouri)
  private def chunk48: Seq[R] = Seq(p_weatherford, p_weatherford_oklahoma, p_weaverville, p_webster, p_weirton, p_welch, p_wellfleet, p_wellington, p_wellington_texas, p_wells, p_wellsboro, p_wenatchee, p_wesley_chapel, p_west_bend, p_west_branch, p_west_chester, p_west_des_moines, p_west_jefferson, p_west_monroe, p_west_plains, p_west_point, p_west_yellowstone, p_westbrook, p_westcliffe, p_westminster_maryland, p_westons_mills, p_wheatland, p_wheaton, p_white_plains, p_whitehall, p_whitewright, p_wickenburg, p_wildwood, p_wilkes_barre, p_willcox, p_williamsburg, p_williamsport, p_williston, p_willits, p_willmar)
  private def chunk49: Seq[R] = Seq(p_wilmington_delaware, p_winchester, p_winchester_indiana, p_winchester_tennessee, p_windom, p_window_rock, p_winfield, p_wing, p_winnemucca, p_winner, p_winona, p_winslow, p_winter_haven, p_winter_park, p_winthrop, p_wolf_point, p_woodbridge, p_woodbury, p_woodland_park, p_woodward, p_wooster, p_worland, p_worthington, p_wrangell, p_wray, p_wytheville, p_yakima, p_yankton, p_yazoo_city, p_yelm, p_yerington, p_york, p_york_nebraska, p_youngstown, p_yuba_city, p_yucca_valley, p_yulee, p_yuma, p_yuma_colorado, p_zanesville)
  private def chunk50: Seq[R] = Seq(p_zumbrota)
  val pages: Seq[R] = chunk0 ++ chunk1 ++ chunk2 ++ chunk3 ++ chunk4 ++ chunk5 ++ chunk6 ++ chunk7 ++ chunk8 ++ chunk9 ++ chunk10 ++ chunk11 ++ chunk12 ++ chunk13 ++ chunk14 ++ chunk15 ++ chunk16 ++ chunk17 ++ chunk18 ++ chunk19 ++ chunk20 ++ chunk21 ++ chunk22 ++ chunk23 ++ chunk24 ++ chunk25 ++ chunk26 ++ chunk27 ++ chunk28 ++ chunk29 ++ chunk30 ++ chunk31 ++ chunk32 ++ chunk33 ++ chunk34 ++ chunk35 ++ chunk36 ++ chunk37 ++ chunk38 ++ chunk39 ++ chunk40 ++ chunk41 ++ chunk42 ++ chunk43 ++ chunk44 ++ chunk45 ++ chunk46 ++ chunk47 ++ chunk48 ++ chunk49 ++ chunk50

  /** (displayName, zoneId) — the venues whose own clock is not their page's. */
  val venueZones: Seq[(String, String)] = Seq(
    ("Green River Cinema Campbellsville", "America/New_York"),
    ("Isis Theatre Winamac", "America/Indiana/Winamac"),
    ("Lights Of Liberty", "America/New_York"),
    ("Vickers Theatre Three Oaks", "America/Detroit"),
  )

  /** Pages that no longer exist — the metros the US pages were until 2026-10 among
   *  them — and the page holding the town each was named after, else most of its
   *  venues: (slug, slug qualified with its state, the page's slug). */
  val retired: Seq[(String, String, String)] = Seq(
    ("alaska", "alaska", "anchorage"),
    ("altoona-johnstown", "altoona-johnstown", "johnstown"),
    ("american-samoa", "american-samoa", "pago-pago"),
    ("auburn-alabama", "auburn-alabama", "opelika"),
    ("beaumont-port-arthur", "beaumont-port-arthur", "beaumont"),
    ("big-bear-lake", "big-bear-lake-california", "redlands"),
    ("birmingham-alabama", "birmingham-alabama", "birmingham"),
    ("bluefield-princeton", "bluefield-princeton", "bluefield"),
    ("brainerd-lakes", "brainerd-lakes", "aitkin"),
    ("brunswick-golden-isles", "brunswick-golden-isles", "brunswick"),
    ("buffalo-niagara", "buffalo-niagara", "buffalo"),
    ("butte", "butte", "anaconda"),
    ("cadet", "cadet", "de-soto"),
    ("calumet", "calumet", "houghton"),
    ("cape-cod", "cape-cod", "oak-bluffs"),
    ("central-coast", "central-coast", "coos-bay"),
    ("central-wyoming", "central-wyoming", "riverton"),
    ("champaign-urbana", "champaign-urbana", "champaign"),
    ("charlotte-south-carolina", "charlotte-south-carolina", "charlotte"),
    ("cheyenne-laramie", "cheyenne-laramie", "laramie"),
    ("cincinnati-kentucky", "cincinnati-kentucky", "cincinnati"),
    ("clarksburg", "clarksburg", "new-martinsville"),
    ("coeur-dalene", "coeur-dalene", "coeur-d-alene"),
    ("corning-elmira", "corning-elmira", "corning"),
    ("daingerfield", "daingerfield", "mount-pleasant-texas"),
    ("dallas-fort-worth", "dallas-fort-worth", "dallas"),
    ("delaware", "delaware", "wilmington-delaware"),
    ("destin-fort-walton-beach", "destin-fort-walton-beach", "crestview"),
    ("district-of-columbia", "district-of-columbia", "washington"),
    ("east-texas", "east-texas", "tyler"),
    ("eastern-arizona", "eastern-arizona", "safford"),
    ("eastern-kentucky", "eastern-kentucky", "london"),
    ("eastern-oregon", "eastern-oregon", "la-grande"),
    ("eastern-panhandle", "eastern-panhandle", "martinsburg"),
    ("eastern-sierra", "eastern-sierra", "bishop"),
    ("ennis", "ennis", "corsicana"),
    ("eureka-arcata", "eureka-arcata", "eureka"),
    ("florida-keys", "florida-keys", "key-west"),
    ("fox-valley", "fox-valley", "appleton"),
    ("glen-burnie", "glen-burnie-maryland", "columbia-maryland"),
    ("grand-forks", "grand-forks", "thief-river-falls"),
    ("grand-forks-north-dakota", "grand-forks-north-dakota", "grafton"),
    ("grove-city", "grove-city", "new-castle"),
    ("guam", "guam", "dededo-village"),
    ("gulf-coast", "gulf-coast", "gulfport"),
    ("hackettstown", "hackettstown", "sparta"),
    ("hampton-bays", "hampton-bays-new-york", "mattituck"),
    ("hampton-roads", "hampton-roads", "norfolk-virginia"),
    ("hanover-pennsylvania", "hanover-pennsylvania", "westminster-maryland"),
    ("hawaii", "hawaii", "pearl-city"),
    ("hawaii-big-island", "hawaii-big-island", "hilo"),
    ("higginsville", "higginsville", "richmond-missouri"),
    ("highlands", "highlands", "clayton-georgia"),
    ("hilton-head", "hilton-head", "hilton-head-island"),
    ("hudson-valley", "hudson-valley", "kingston"),
    ("inland-empire", "inland-empire", "chino"),
    ("iron-range", "iron-range", "cook"),
    ("jefferson", "jefferson", "carroll"),
    ("kenai-peninsula", "kenai-peninsula", "kenai"),
    ("killeen-temple", "killeen-temple", "copperas-cove"),
    ("lafayette-west-lafayette", "lafayette-west-lafayette", "schererville"),
    ("lake-of-the-ozarks", "lake-of-the-ozarks", "lake-ozark"),
    ("lebanon-hanover", "lebanon-hanover", "hanover"),
    ("lehigh-valley", "lehigh-valley", "bethlehem"),
    ("lewisburg", "lewisburg", "shelbyville-tennessee"),
    ("littleton", "littleton", "st-johnsbury"),
    ("long-island", "long-island", "east-hampton"),
    ("louisville-kentucky", "louisville-kentucky", "louisville"),
    ("madison-minnesota", "madison-minnesota", "canby"),
    ("manchester-new-hampshire", "manchester-new-hampshire", "manchester"),
    ("maui", "maui", "kahului"),
    ("memphis-mississippi", "memphis-mississippi", "memphis"),
    ("midland-odessa", "midland-odessa", "midland-texas"),
    ("minneapolis-st-paul", "minneapolis-st-paul", "minneapolis"),
    ("minneapolis-st-paul-wisconsin", "minneapolis-st-paul-wisconsin", "minneapolis"),
    ("monterey-county", "monterey-county", "watsonville"),
    ("morrisville", "morrisville-north-carolina", "holly-springs"),
    ("mount-ayr", "mount-ayr", "lamoni"),
    ("new-haven", "new-haven", "washington-missouri"),
    ("new-york-new-jersey", "new-york-new-jersey", "new-york"),
    ("north-central-arkansas", "north-central-arkansas", "mountain-view-arkansas"),
    ("north-central-ohio", "north-central-ohio", "ontario-ohio"),
    ("north-coast", "north-coast", "astoria"),
    ("north-georgia", "north-georgia", "ellijay"),
    ("north-missouri", "north-missouri", "saint-joseph"),
    ("northeast-wyoming", "northeast-wyoming", "gillette"),
    ("northern-california", "northern-california", "redding"),
    ("northern-michigan", "northern-michigan", "traverse-city"),
    ("northwest-arkansas", "northwest-arkansas", "fayetteville-arkansas"),
    ("northwest-kansas", "northwest-kansas", "colby"),
    ("northwest-oklahoma", "northwest-oklahoma", "woodward"),
    ("northwest-wisconsin", "northwest-wisconsin", "ladysmith"),
    ("oahu", "oahu", "pearl-city"),
    ("old-valdez", "old-valdez", "valdez"),
    ("omaha-iowa", "omaha-iowa", "omaha"),
    ("osage", "osage", "mason-city"),
    ("owego", "owego", "binghamton"),
    ("owings-mills", "owings-mills-maryland", "columbia-maryland"),
    ("paullina", "paullina", "storm-lake"),
    ("philadelphia-new-jersey", "philadelphia-new-jersey", "philadelphia"),
    ("pierre", "pierre", "gregory"),
    ("plattsburgh-north-country", "plattsburgh-north-country", "massena"),
    ("portland-maine", "portland-maine", "westbrook"),
    ("portland-washington", "portland-washington", "portland"),
    ("quad-cities", "quad-cities", "davenport"),
    ("rhode-island", "rhode-island", "south-kingstown"),
    ("rio-grande-valley", "rio-grande-valley", "edinburg"),
    ("rocky-mount-wilson", "rocky-mount-wilson", "rocky-mount"),
    ("saginaw-bay-city", "saginaw-bay-city", "bad-axe"),
    ("san-francisco-bay-area", "san-francisco-bay-area", "san-francisco"),
    ("sanford", "sanford-north-carolina", "holly-springs"),
    ("sanpete-valley", "sanpete-valley", "ephraim"),
    ("sarasota-bradenton", "sarasota-bradenton", "sarasota"),
    ("scranton-wilkes-barre", "scranton-wilkes-barre", "scranton"),
    ("shenandoah-valley", "shenandoah-valley", "front-royal"),
    ("sierra-foothills", "sierra-foothills", "grass-valley"),
    ("somerset-kentucky", "somerset-kentucky", "somerset"),
    ("south-bend-elkhart", "south-bend-elkhart", "plymouth-indiana"),
    ("southeast-alaska", "southeast-alaska", "petersburg"),
    ("southeast-kansas", "southeast-kansas", "chanute"),
    ("southern-illinois", "southern-illinois", "carbondale"),
    ("southwest-kansas", "southwest-kansas", "ulysses"),
    ("southwest-michigan", "southwest-michigan", "benton-harbor"),
    ("southwest-minnesota", "southwest-minnesota", "luverne"),
    ("southwest-virginia", "southwest-virginia", "norton"),
    ("southwest-washington", "southwest-washington", "long-beach-washington"),
    ("southwest-wisconsin", "southwest-wisconsin", "dodgeville"),
    ("southwest-wyoming", "southwest-wyoming", "evanston-wyoming"),
    ("space-coast", "space-coast", "melbourne"),
    ("st-cloud", "st-cloud", "alexandria"),
    ("st-louis-illinois", "st-louis-illinois", "st-louis"),
    ("star-valley", "star-valley", "afton"),
    ("tampa-bay", "tampa-bay", "tampa"),
    ("towanda", "towanda", "sayre"),
    ("treasure-coast", "treasure-coast", "stuart-florida"),
    ("tri-cities", "tri-cities", "kennewick"),
    ("tri-cities-tennessee", "tri-cities-tennessee", "johnson-city"),
    ("triad", "triad", "greensboro"),
    ("triangle", "triangle", "raleigh"),
    ("uinta-basin", "uinta-basin", "roosevelt"),
    ("upper-peninsula", "upper-peninsula", "escanaba"),
    ("utica-rome", "utica-rome", "oneida"),
    ("vermont", "vermont", "burlington-vermont"),
    ("virgin-islands", "virgin-islands", "tutu"),
    ("virginia-eastern-shore", "virginia-eastern-shore", "onancock"),
    ("washington-dc", "washington-dc", "washington"),
    ("washington-dc-maryland", "washington-dc-maryland", "washington"),
    ("watertown-north-country", "watertown-north-country", "watertown-new-york"),
    ("wausau-stevens-point", "wausau-stevens-point", "stevens-point"),
    ("west-central-minnesota", "west-central-minnesota", "willmar"),
    ("west-texas", "west-texas", "eagle-pass"),
    ("western-maryland", "western-maryland", "frostburg"),
    ("western-massachusetts", "western-massachusetts", "amherst-massachusetts"),
    ("western-nc-mountains", "western-nc-mountains", "boone"),
    ("western-pa", "western-pa", "saint-marys"),
    ("willamette-valley", "willamette-valley", "albany-oregon"),
    ("yellowstone-gateway", "yellowstone-gateway", "powell"),
  )

  /** The venues that screen only in season — open-air, summer and drive-in screens, by name
   *  (data/scripts/seasonal_venues.py): their display names as generated above. */
  val seasonal: Set[String] = Set(
    "13-24 Drive In Wabash",
    "21 Drive In Van Buren",
    "34 Drive-In Earlville",
    "411 Twin Drive In Centre",
    "49er Drive In Valparaiso",
    "56 Auto Drive-in Massena",
    "66 Drive-in Carthage",
    "88 Drive-In Theatre Commerce City",
    "99 West Drive-In Newberg",
    "Admiral Twin Drive-in Tulsa",
    "American Dream Drive-in Powell",
    "Auburn Garrett Drive In",
    "Aut-O-Rama Drive-In",
    "Auto Drive-in Greenwood",
    "Auto Vue Drive-in Colville",
    "Badin Road Drive-In Albemarle",
    "Bar-Ann Drive-In Portage",
    "Barco Drive-in Lamar",
    "Basin Drive-in Mt Pleasant",
    "Bay Drive-In Alexandria Bay",
    "Beckys Drive-In Walnutport",
    "Bel Air Drive In Versailles",
    "Belmont Drive-In",
    "Bengies Drive-In Middle River",
    "Bethel Drive-in",
    "Big Sky Drive-in Midland",
    "Big Sky Drive-in Wisconsin Dells",
    "Birdsong Drive In Camden",
    "Black River Drive-in",
    "Blue Fox Drive-in Oak Harbor",
    "Blue Grass Drive-In Theater",
    "Blue Moon Drive-In",
    "Blue Sky Drive-in Wadsworth",
    "Blue Starlite Drive-In Central",
    "Blue Starlite Drive-In Downtown",
    "Boulevard Drive-In Kansas City",
    "Bourbon Drive-in Paris",
    "Brazos Drive-in Granbury",
    "Bridgton Drive-in",
    "Brownsville Drive-In Grindstone",
    "C&C Drive-In",
    "Calvert Drive In",
    "Capri Drive-in Coldwater",
    "Carrolltown Drive-in",
    "Celebration! Getty Drive-in",
    "Centerbrook Drive-In Martinsville",
    "Central Drive-In Norton",
    "Chakeres Theatres - Melody 49 Drive-In",
    "Cherry Bowl Drive-in Honor",
    "Chief Drive-in Chickasha",
    "Chilton Twilight Drive-In Theater",
    "Cinema 67 Drive-in Spencer",
    "Circle Drive-in Scranton",
    "Clearfield Super 322 Drive-in",
    "Comanche Drive-in Buena Vista",
    "Coyote Drive-in Ft Worth",
    "Cumberland Drive-in Newville",
    "DeAnza Starlight Drive-In Atlanta 4",
    "Delevan Twin Drive-In",
    "Delsea Drive-in Vineland",
    "Dependable Drive-In Coraopolis",
    "Dixie Twin Drive-In Dayton",
    "Doc's Drive In",
    "Dreamland Drive In",
    "Echo Drive-in Roosevelt",
    "Eden Drive-In Twin",
    "El Rancho Drive-in Palatine Bridge",
    "El-co Drive-in Shattuck",
    "Electric Dusk Drive-In Glendale",
    "Elko Drive-in",
    "Elm Road Drive-in Warren",
    "Elmira Drive-In Theatre",
    "Erda Drive In",
    "Erwin's Comet Drive-in Dunbar",
    "Evergreen Drive-in Mt Pleasant",
    "FLIFF Drive-In Cinema (Off-Site Special Events)",
    "Fair Oaks Drive In Middletown",
    "Fairlee Drive-in",
    "Family Drive-in Theatre Kane",
    "Field of Dreams Drive-in Liberty Center",
    "Field of Scenes Drive-in Kaukauna",
    "Fingerlake Drive-in Auburn",
    "Fleo's Flick's Drive-In Theatre",
    "Ford Wyoming Drive In",
    "Fort Union Drive in Movie Theatre",
    "Four Brothers Drive-in",
    "Frontier Drive-In at the Mill",
    "GQT Huntington Twin Drive-in",
    "Galaxy Drive-In Ennis",
    "Garden Drive-in Hunlock Creek",
    "Georgetown Drive-in",
    "Glen Drive-in Theatre Queensbury",
    "Graham Drive-In",
    "Grand River Drive-in",
    "Grants Pass Drive-In",
    "Haars Drive-in Dillsburg",
    "Harvest Moon Drive-In Theatre",
    "Hathaway's Twin Drive-In Theatre",
    "Hi Road Drive-In Kenton",
    "Hi Way Drive-in Sandusky",
    "Hi-Way Drive-In Theatre",
    "Hi-way 50 Drive-in Theatre Lewisburg",
    "Highway 18 Outdoor Theatre Jefferson",
    "Highway 2 Community Drive-In Theater",
    "Highway 21 Drive-In",
    "Hilltop Drive-In Theatre",
    "Hilltop Drive-in Theatre Hazard",
    "Historic Route 11 Drive-In",
    "Hog Pen Sunset Drive-In",
    "Holiday Drive-in Fort Collins",
    "Holiday Drive-in Hamilton",
    "Holiday Drive-in Mitchell",
    "Holiday Drive-in Rockport",
    "Hollywood Drive-in Averill Park",
    "Hulls Drive-in Lexington",
    "Hummel Drive In Winchester",
    "Idan-Ha Drive In Theater",
    "Jericho Drive-In",
    "Jesup Drive-in Theatre",
    "Joylan Drive-in Dade City",
    "Judy Drive-In Mt Sterling",
    "Jungle Drive-In Parkersburg",
    "Kanopolis Drive-In Theatre",
    "Kenda Drive-In",
    "Knox Drive-In",
    "La Grande Drive-in",
    "Lakeport Auto Movies Drive-In",
    "Lakeshore Drive-in Monticello",
    "Last Drive-in Picture Show Gatesville",
    "LoCo Drive-in",
    "Long Prairie Drive-In",
    "Lynn Auto Drive-In Strasburg",
    "M-f Drive-in Milton Freewater",
    "M.E.L.S. at the Starlite Drive-In Theatre",
    "Macon Drive-in Theatre Lafayette",
    "Madera 2 Drive-in",
    "Magic City Drive-in Barberton",
    "Mahoning Drive-in",
    "Malco Summer Drive-in Memphis",
    "Malta Drive-in",
    "Mansfield Drive-in Theatre",
    "Mayfield Road Drive-In",
    "McHenry Outdoor Theater",
    "Meadow Bridge Drive-In",
    "Melody Drive-In Knox",
    "Melody Drive-In Theater Dayton",
    "Memory Lane Drive-in",
    "Mendon Twin Drive-in",
    "Mesa Drive-In",
    "Midway Drive In Miller",
    "Midway Drive-in Fulton",
    "Midway Drive-in Mifflintown",
    "Midway Drive-in Sterling",
    "Midway Twin Drive-In Ravenna",
    "Milford Drive-In Theater",
    "Monetta Drive-in 3",
    "Montana Drive-in Estill Springs",
    "Moon Lite Drive-in",
    "Moonlight Drive-in @ Planet Fun",
    "Moonlite Drive-In Brookville",
    "Moonlite Drive-in Abingdon",
    "Moonlite Drive-in West Wyoming",
    "Moonstruck Drive-In Houston",
    "Motor Vu Drive-In",
    "Narrow Gauge Drive-In",
    "Neptune Drive-In",
    "Night Light Drive In",
    "Northfield Drive-in Hinsdale",
    "Ocala Drive-In",
    "Overlook Drive-in Poughkeepsie",
    "Ozoner 29 Drive-in Broadalbin",
    "Palace Gardens Drive-in Indiana",
    "Paramount Drive-In Theatres",
    "Park Place Drive-in Marion",
    "Parkway Drive In Maryville",
    "Parma Motor-Vu Drive-In Theatre",
    "Pheasant City Drive-In Redfield",
    "Pheasant Drive-In Mobridge",
    "Phoenix Drive-in Houston",
    "Pike Drive-in Montgomery",
    "Pine Hill Drive In Theater",
    "Pink Cadillac Drive-in Centerville",
    "Pipestem Drive-In Theatre",
    "Point Drive-in Northumberland",
    "Portville Drive-In",
    "Prides Corner Drive-In",
    "Raleigh Road Drive-In Henderson",
    "Redwood Drive-In Salt Lake City",
    "Reynolds Drive-In",
    "Ridgeview Drive-In",
    "Riverside Drive-in Vandergrift",
    "Rock N Roll Drive-In",
    "Rodeo Drive-In Theatre",
    "Rose City Drive In",
    "Route 66 Drive-In Springfield",
    "Rubidoux Drive-In Theatre",
    "Ruskin Family Drive-In Theatre",
    "Rustic Tri View Drive-In",
    "Saco Drive-in Theatre",
    "Sandell Drive-in Clarendon",
    "Sandhills Drive-In",
    "Sauerbeck Family Drive-in",
    "Shankweiler Drive-in Orefield",
    "Showboat Drive-in Hockley",
    "Sidney Auto-Vue Drive-In",
    "Silver Bow Twin Drive-in",
    "Silver Drive-in Johnstown",
    "Silver Lining Drive-In",
    "Silverlake Drive-in Theatre Perry",
    "Silvermoon Drive-in Lakeland",
    "Sky Vu Drive In Monroe",
    "Sky Vu Drive-in Warren",
    "Skyline Drive-In NYC",
    "Skyline Drive-in Barstow",
    "Skyline Drive-in Greensburg",
    "Skyline Drive-in Shelton",
    "Skylite Drive-in Madawaska",
    "Skyview Drive-in Belleville",
    "Skyview Drive-in Lancaster",
    "Skyview Drive-in Litchfield",
    "Skyway Drive-In Fish Creek",
    "Skyway Twin Drive-in",
    "Smith's Ranch Drive-In Theater",
    "South Bay Drive-In Theatre",
    "South Drive-in Dodge City",
    "South Twin Drive-In",
    "Southington Drive-In",
    "Sparta Drive-in",
    "Springmill Drive-in Mansfield",
    "St Michael Drive In",
    "Stanford Drive-in",
    "Star Drive-in Monte Vista",
    "Star Drive-in Montrose",
    "Stardust Drive In Chetek",
    "Stardust Drive-In Watertown",
    "Starlight Drive-In Butler",
    "Starlight Drive-in",
    "Starlite 14 Drive In",
    "Starlite Drive In Bloomington",
    "Starlite Drive-In Oak Hill",
    "Starlite Drive-in Amelia",
    "Starlite Drive-in Cadet",
    "Starlite Drive-in Christiansburg",
    "Starlite Drive-in Litchfield",
    "Starlite Drive-in Wichita",
    "Starview Drive-in Norwalk",
    "State Line Drive-in Elizabethton",
    "Stone Drive-In Theatre",
    "Summer Drive-in Plymouth Township",
    "Sundance Drive-In Oregon",
    "Sunset Auto Vue Drive-In Movie Theater",
    "Sunset Drive-In Colchester",
    "Sunset Drive-In Laredo",
    "Sunset Drive-in Aurora",
    "Sunset Drive-in Meadowbrook",
    "Sunset Drive-in San Luis Obispo",
    "Sunset Drive-in Waterford",
    "Superior 71 Drive-In Spirit Lake",
    "Swan Drive-in Blue Ridge",
    "Swingin' Midway Drive In",
    "TK-Starlite Drive-In",
    "TN Safari Drive In Theater",
    "Tascosa Drive-in Amarillo",
    "Tee Pee Drive-In Sapulpa",
    "Terrace Drive-In Theatre",
    "The Vineyard's Drive-In",
    "Tibbs Drive-In",
    "Tiffin Drive-In",
    "Tiger Drive-In Theatre",
    "Tower Drive-in Poteau",
    "Transit Drive-in Lockport",
    "Tri-way Drive-in Plymouth",
    "Tru Vu Drive-In Theatre",
    "Twin City Drive-in Theatre Bristol",
    "Twin Crescent Drive-in",
    "US 23 Drive In Theatre",
    "Unadilla Drive-in",
    "Valle Drive-in Newton",
    "Valley's Drive In Theater",
    "Van Buren Drive-In",
    "Van Buren Drive-In Theatre",
    "Van Del Drive-in Middle Point",
    "Verne Drive-in Luverne",
    "Vintage Drive-In Avon",
    "Warhouse Drive-in",
    "Warner's Drive In",
    "Warwick Drive-In Theater",
    "Wes Mer Drive-in Mercedes",
    "Wheel-In-Motor Movie Drive In",
    "Wilderness Outdoor Theatre Trenton",
    "Winchester Drive-in Oklahoma City",
    "Winner Drive In Theatre",
  )
}
