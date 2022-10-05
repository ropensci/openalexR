concept_abbrev <- tibble::tribble(
                                ~id,           ~display_name, ~abbreviation,
   "https://openalex.org/C41008148",      "Computer science",  "Comput Sci",
   "https://openalex.org/C71924100",              "Medicine",         "Med",
  "https://openalex.org/C185592680",             "Chemistry",        "Chem",
   "https://openalex.org/C15744967",            "Psychology",     "Psychol",
   "https://openalex.org/C86803240",               "Biology",        "Biol",
   "https://openalex.org/C17744445",     "Political science",   "Polit Sci",
  "https://openalex.org/C192562407",     "Materials science",   "Mater Sci",
  "https://openalex.org/C142362112",                   "Art",         "Art",
  "https://openalex.org/C144133560",              "Business",         "Bus",
  "https://openalex.org/C205649164",             "Geography",         "Geo",
  "https://openalex.org/C121332964",               "Physics",        "Phys",
   "https://openalex.org/C39432304", "Environmental science", "Environ Sci",
   "https://openalex.org/C33923547",           "Mathematics",        "Math",
  "https://openalex.org/C138885662",            "Philosophy",        "Phil",
  "https://openalex.org/C144024400",             "Sociology",      "Sociol",
   "https://openalex.org/C95457728",               "History",        "Hist",
  "https://openalex.org/C127313418",               "Geology",        "Geol",
  "https://openalex.org/C127413603",           "Engineering",         "Eng",
  "https://openalex.org/C162324750",             "Economics",        "Econ"
)

usethis::use_data(concept_abbrev, overwrite = TRUE)
