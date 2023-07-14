# aoi for Wandoo National Park
aoi_example <- find_tenure(like = "wandoo")[1] |>
  DBCA_aoi(block = FALSE)
saveRDS(aoi_example, "./tests/testthat/fixtures/aoi_example.rds")

# fire history crs for make_wkt testing
fire_path <- "../corp_data/DBCA_Fire_History_DBCA_060.shp"
fname <- tools::file_path_sans_ext(basename(fire_path))
fquery <- paste0('SELECT * from ', fname, ' LIMIT 1')
fh_crs <- sf::st_crs(sf::read_sf(fire_path, query = fquery))
saveRDS(fh_crs, "./tests/testthat/fixtures/fh_crs.rds")

# assembled data for metrics testing
example_data <- assemble_data(fire_path, FYfrom = 2000, FYto = 2022, aoi = aoi_example, accessed_on = NULL)
saveRDS(example_data, "./tests/testthat/fixtures/example_data.rds")
