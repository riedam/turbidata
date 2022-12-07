# load csv ext file
test_csv <- system.file("extdata", 'test.csv', package = "turbidata")
output_csv <- capture.output(test_csv <- turbidata:::.import(test_csv, force_update_cache = TRUE, create_cache_file = FALSE))
test_xlsx <- system.file("extdata", 'test.xlsx', package = "turbidata")
output_xlsx <- capture.output(test_xlsx <-  turbidata:::.import(test_xlsx, force_update_cache = TRUE, create_cache_file = FALSE))


test_that(".import csv", {
  # Test
  expect_equal(output_csv, 'Skip cache file if exist : force update')
  expect_snapshot(rlang::hash(test_csv))
})


test_that(".import xlsx", {
  # Test
  expect_equal(output_xlsx, 'Skip cache file if exist : force update')
  expect_snapshot(rlang::hash(test_xlsx))
})


test_that(".import indentical", {
  # Check if csv and xlsx importation are equals
  test_csv <- as_tibble(test_csv)
  test_xlsx <- as_tibble(test_xlsx)
  expect_equal(test_csv, test_xlsx)
})


test_that(".get_relative", {
  expect_snapshot(rlang::hash(turbidata:::.get_relative(test_csv)))
})


test_that(".pivot_longer", {
  expect_snapshot(rlang::hash(turbidata:::.pivot_longer(test_csv)))
})

