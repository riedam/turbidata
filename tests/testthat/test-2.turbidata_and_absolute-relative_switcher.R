test_csv <- system.file("extdata", 'test.csv', package = "turbidata")
output <- capture.output(data <- turbidata_class$new(test_csv, force_update_cache = TRUE, create_cache_file = FALSE))


test_that("class test", {
  # test name change
  expect_equal(data$name, 'test')
  data$name = 'test2'
  expect_equal(data$name, 'test2')

  #test print function
  expect_snapshot(print(data))

  #test output
  expect_equal(output, 'Skip cache file if exist : force update')
})


test_that("absolute/relative switcher", {
  # Function tested individually previously so should not normally change

  # Test importation
  data$use_relative()
  expect_equal(data$type, 'relative')
  expect_snapshot(rlang::hash(data$data))

  # Test
  data$use_absolute()
  expect_equal(data$type, 'absolute')
  expect_snapshot(rlang::hash(data$data))
})
