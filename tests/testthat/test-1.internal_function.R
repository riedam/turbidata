# load csv ext file
test_csv <- system.file("extdata", 'test.csv', package = "turbidata")
capture.output(test_csv <- turbidata:::.import(test_csv, force_update_cache = TRUE, create_cache_file = FALSE))

# load xlsx ext file
test_xlsx <- system.file("extdata", 'test.xlsx', package = "turbidata")
capture.output(test_xlsx <-  turbidata:::.import(test_xlsx, force_update_cache = TRUE, create_cache_file = FALSE))

test_that(".import", {
  # TODO: repare testthat
  # Check if there is no problem for importing csv file
  expect_snapshot(head(test_csv))

  # Check if there is no problem for importing xlsx file
  expect_snapshot(head(test_xlsx))

  # Check if csv and xlsx importation are equals
  test_csv <- as_tibble(test_csv)
  test_xlsx <- as_tibble(test_xlsx)
  expect_equal(test_csv, test_xlsx)

  # SKIP ON CRAN. Check if sha256 hach are identical.
  # This step is performed because the previous steps only checked the output and not the dataset
  skip_on_cran()
  skip_if_not_installed("digest", '0.6.30')
  expect_snapshot(digest::digest(test_csv, algo = 'sha256'))
  expect_snapshot(digest::digest(test_xlsx, algo = 'sha256'))
})


test_that(".get_relative", {
  expect_snapshot(head(turbidata:::.get_relative(test_csv)))

  # SKIP ON CRAN. Check if sha256 hach are identical.
  # This step is performed because the previous steps only checked the output and not the dataset
  skip_on_cran()
  skip_if_not_installed("digest", '0.6.30')
  expect_snapshot(digest::digest(turbidata:::.get_relative(test_csv)))
})


test_that(".pivot_longer", {
  expect_snapshot(head(turbidata:::.pivot_longer(test_csv)))

  # SKIP ON CRAN. Check if sha256 hach are identical.
  # This step is performed because the previous steps only checked the output and not the dataset
  skip_on_cran()
  skip_if_not_installed("digest", '0.6.30')
  expect_snapshot(digest::digest(turbidata:::.pivot_longer(test_csv)))
})

