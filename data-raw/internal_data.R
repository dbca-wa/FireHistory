## Internal data

library(dplyr)
library(sf)


## set up district code tables
districts <- list(
  "GOLDFIELDS REGION" = c("GLD", "KAL"),
  "EAST KIMBERLEY" = c("EKM", "EKI"),
  "WEST KIMBERLEY" = c("WKM", "WKI"),
  "MURCHISON" = c("MUR", "GER", "GTN"),
  "TURQUOISE COAST" = c("TCD", "MOR", "MRA"),
  "GASCOYNE" = c("GAS", "SHB", "SHK"),
  "EXMOUTH" = c("EXM"),
  "PILBARA REGION" = c("PIL"),
  "ALBANY" = c("ALB"),
  "ESPERANCE" = c("ESP"),
  "WELLINGTON" = c("WTN", "WEL"),
  "BLACKWOOD" = c("BWD"),
  "PERTH HILLS" = c("PHS", "PHL"),
  "SWAN COASTAL" = c("SWC"),
  "DONNELLY" = c("DON"),
  "FRANKLAND" = c("FRK"),
  "WHEATBELT REGION" = c("WHB", "GRS", "GSN", "MER", "CWB", "SWB")
)

tenure_shp <- sf::st_read("V:/GIS1-Corporate/Data/GDB/SCDB_Tenure/State_Tenure/Tenure.gdb",
                      layer = "CPT_DBCA_LEGISLATED_TENURE") |>
  sf::st_make_valid() |>
  sf::st_transform(3577)

tenure <- unique(tenure_shp$LEG_NAME)

DBCA_tenure <- tenure[!grepl("^\\s+$", tenure)]

## create copy of districts shape file
districts_shp <- sf::st_read(dsn = "V:/GIS1-Corporate/Data/GDB/Administration__Boundaries/Administrative_Boundaries.gdb",
                             layer = "CPT_DBCA_DISTRICTS") |>
  sf::st_make_valid() |>
  sf::st_transform(3577)


usethis::use_data(districts, districts_shp, tenure_shp, DBCA_tenure, internal = TRUE,
                  overwrite = TRUE)
