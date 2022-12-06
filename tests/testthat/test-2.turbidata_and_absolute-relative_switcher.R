test_csv <- system.file("extdata", 'test.csv', package = "turbidata")
capture.output(data <- turbidata_class$new(test_csv, force_update_cache = TRUE, create_cache_file = FALSE))


test_that("class test", {
  expect_equal(data$name, 'test')
  data$name = 'test2'
  expect_equal(data$name, 'test2')

  expect_snapshot(print(data))
})


test_that("absolute/relative switcher", {
  # Function tested individually previously so should not normally change
  skip_on_cran()
  skip_if_not_installed("digest", '0.6.30')

  # Test importation
  data$use_relative()
  expect_equal(data$type, 'relative')
  expect_snapshot(digest::digest(data$data))

  # Test
  data$use_absolute()
  expect_equal(data$type, 'absolute')
  expect_snapshot(digest::digest(data$data))
})
