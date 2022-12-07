test_csv <- system.file("extdata", 'test.csv', package = "turbidata")
capture.output(data <- turbidata_class$new(test_csv, force_update_cache = TRUE, create_cache_file = FALSE))
data = data$average_area()
path <- tempdir()


test_that("average_area", {
  expect_snapshot(rlang::hash(data$data))
})


test_that("export", {

  output <- capture.output(data$export(
    format = "csv",
    output_dir = path,
    override_file = TRUE
  ))

  target_path <- paste('output: ', path, '/test.csv', sep = '')
  expect_equal(output, target_path)

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  announce_snapshot_file('test.csv')
  newpath <- normalizePath(paste(path, 'test.csv', sep = '/'))
  expect_snapshot_file(newpath, 'test-export.csv')
})


test_that("ggplot", {

  plot = data$ggplot()

  expect_snapshot(head(plot))

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()

  ggsave('test-ggplot.png',
         plot = plot,
         path = path,
         device = 'png',
         scale = 1,
         width = 1280,
         height = 720,
         units = 'px'
  )
  announce_snapshot_file('test-ggplot.png')
  newpath <- normalizePath(paste(path, 'test-ggplot.png', sep = '/'))
  expect_snapshot_file(newpath, 'test-ggplot.png')
})
