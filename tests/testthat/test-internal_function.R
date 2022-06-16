test_import <- function(file) {

  path <- system.file("extdata", file, package = "turbidata")
  path <- substring(path,1, nchar(path)-nchar(file))
  capture.output(
    data_class <- turbidata(file, dir = path, force_update_cache = TRUE, create_cache_file = FALSE)
  )

  vpath <- system.file("data", paste(file, '.rda', sep = ''), package = "turbidata")
  load(vpath)

  test <- expect_equal(data_class$data, data$data)
  return(test)
}

test_that(".import", {
  # TODO: repare testthat
  #test_import('test.csv')
  #test_import('test.xlsx')
})
