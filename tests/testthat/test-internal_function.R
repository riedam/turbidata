test_import <- function(file) {
  path <- system.file("extdata", file, package = "turbidata")
  path <- substring(path,1, nchar(path)-nchar(file))
  data <- suppressMessages(.import(file, dir = path, force_update_cache = TRUE, create_cache_file = FALSE))
  tpath <- system.file("data", paste(file, '.rds', sep = ''), package = "turbidata")
  tdata <- readRDS(tpath)
  test <- expect_equal(data, tdata)
  return(test)
}

test_that(".import", {
  test_import('test.csv')
  test_import('test.xlsx')
})
