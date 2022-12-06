test_csv <- system.file("extdata", 'test.csv', package = "turbidata")
capture.output(data <- turbidata_class$new(test_csv, force_update_cache = TRUE, create_cache_file = FALSE))
data = data$average_area()


test_that("average_area", {
  expect_snapshot(head(data$data))
})


test_that("ggplot", {
  plot = data$ggplot()

  expect_snapshot(head(plot))

  path <- tempdir()
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
