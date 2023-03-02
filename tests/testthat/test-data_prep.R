test_that("find_tenure returns a long vector of names", {
  expect_gt(length(find_tenure()), 1000)
})

test_that("find_block returns a long vector of names", {
  expect_gt(length(find_block()), 400)
})

test_that("DBCA_aoi works", {
  choice <- find_tenure(like = "wandoo")[1]
  expect_type(DBCA_aoi(choice, block = FALSE), "list")
  expect_match(DBCA_aoi(choice, block = FALSE)[["aoi_name"]], "Wandoo National Park")
  expect_s3_class(DBCA_aoi(choice, block = FALSE)[["aoi"]], "sf")
  expect_error(DBCA_aoi(choice, block = TRUE), "Can't find that ")
})

test_that("WKT conversion works", {
  fh_crs <- readRDS(test_path("fixtures", "fh_crs.rds"))
  aoi_example <- readRDS(test_path("fixtures", "aoi_example.rds"))
  expect_type(make_wkt(aoi_example, fh_crs), "character")

})

