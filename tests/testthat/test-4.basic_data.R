test_csv <- system.file("extdata", 'test.csv', package = "turbidata")
capture.output(data <- turbidata_class$new(test_csv, force_update_cache = TRUE, create_cache_file = FALSE))

path <- tempdir()


test_that("export", {
  capture.output(data$export(
    format = "csv",
    output_dir = path
  ))
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


test_that("gganim", {

  skip_on_cran()
  skip_if_not_installed("gganimate", '1.0.7')
  skip_if_not_installed('gifski', '1.6.6.1')

  capture.output(data$gganim(width = 196,
              height = 144,
              fps = 5,
              export_format = 'gif',
              filename = 'test-gganim.gif',
              output_dir = path
              ))
  announce_snapshot_file('test-gganim.gif')
  expect_snapshot_file(paste(path, 'test-gganim.gif', sep = '\\'), 'test-gganim.gif')
})
