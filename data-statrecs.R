# read in a and process statrecs shapefile

# read in statsq table
statrecs <- sf::read_sf("bootstrap/data/ICES-stat-rec/StatRec_map_Areas_Full_20170124.shp")

# format statsquare table
statrecs <-
  statrecs %>%
  select(
    ICESNAME, Area_27, stat_x, stat_y, geometry
  ) %>%
  rename(
    StatRec = ICESNAME,
    F_CODE = Area_27,
    Lat = stat_x,
    Lon = stat_y
  ) %>%
  filter(!is.na(F_CODE)) %>%
  mutate(
    F_CODE = F_CODE %>%
                 strsplit("[.]") %>%
                 lapply("[", 1:2) %>%
                 sapply(paste0, collapse = ".")
  ) %>%
  mutate(
    F_CODE = ifelse(F_CODE %in% c("3.b", "3.c"), "3.b, c", F_CODE)
  )

# write out
sf::write_sf(
  statrecs,
  "data/statrecs.csv",
  layer_options = "GEOMETRY=AS_WKT",
  delete_dsn = file.exists("data/statrecs.csv")
)
