test_import <- function(file) {

  path <- system.file("extdata", file, package = "turbidata")
  capture.output(
    data_class <- turbidata(path, force_update_cache = TRUE, create_cache_file = FALSE)
  )

  vpath <- system.file("data", 'test_data.rda', package = "turbidata")
  load(vpath)

  test <- expect_equal(data_class$data, test_data$data)
  return(test)
}

test_that(".import", {
  # TODO: repare testthat
  # test_import('test.csv')
  # test_import('test.xlsx')
})
