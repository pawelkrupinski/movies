"""US state/territory display name -> URL slug.

The list is the roster's spine: `generate_roster.py` emits one block per entry,
in slug order, and refuses a venue whose `state` is not a key here.

No timezone. A state's zone used to live here and reach every `City` cut out of
it, which put Knoxville on Central and El Paso on Eastern — 138 venues across
fifteen straddling states on a clock that was not theirs. The zone is now
resolved per METRO from its own coordinates (`cluster_metros.zone_for`), which is
the level a `City` is addressable at anyway.
"""

STATES = {
    "Alabama":           "alabama",
    "Alaska":            "alaska",
    "American Samoa":    "american-samoa",
    "Arizona":           "arizona",
    "Arkansas":          "arkansas",
    "California":        "california",
    "Colorado":          "colorado",
    "Connecticut":       "connecticut",
    "Delaware":          "delaware",
    "District of Columbia": "district-of-columbia",
    "Florida":           "florida",
    "Georgia":           "georgia",
    "Guam":              "guam",
    "Hawaii":            "hawaii",
    "Idaho":             "idaho",
    "Illinois":          "illinois",
    "Indiana":           "indiana",
    "Iowa":              "iowa",
    "Kansas":            "kansas",
    "Kentucky":          "kentucky",
    "Louisiana":         "louisiana",
    "Maine":             "maine",
    "Maryland":          "maryland",
    "Massachusetts":     "massachusetts",
    "Michigan":          "michigan",
    "Minnesota":         "minnesota",
    "Mississippi":       "mississippi",
    "Missouri":          "missouri",
    "Montana":           "montana",
    "Nebraska":          "nebraska",
    "Nevada":            "nevada",
    "New Hampshire":     "new-hampshire",
    "New Jersey":        "new-jersey",
    "New Mexico":        "new-mexico",
    "New York":          "new-york",
    "North Carolina":    "north-carolina",
    "North Dakota":      "north-dakota",
    "Ohio":              "ohio",
    "Oklahoma":          "oklahoma",
    "Oregon":            "oregon",
    "Pennsylvania":      "pennsylvania",
    "Puerto Rico":       "puerto-rico",
    "Rhode Island":      "rhode-island",
    "South Carolina":    "south-carolina",
    "South Dakota":      "south-dakota",
    "Tennessee":         "tennessee",
    "Texas":             "texas",
    "Utah":              "utah",
    "Vermont":           "vermont",
    "Virgin Islands":    "virgin-islands",
    "Virginia":          "virginia",
    "Washington":        "washington",
    "West Virginia":     "west-virginia",
    "Wisconsin":         "wisconsin",
    "Wyoming":           "wyoming",
}
