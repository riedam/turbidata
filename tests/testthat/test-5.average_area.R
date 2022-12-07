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

  announce_snapshot_file('test.csv')
  expect_snapshot_file(paste(path, 'test.csv', sep = '\\'), 'test-export.csv')
})


test_that("ggplot", {
  plot = data$ggplot()

  expect_snapshot(head(plot))

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
  expect_snapshot_file(paste(path, 'test-ggplot.png', sep = '\\'), 'test-ggplot.png')
})
