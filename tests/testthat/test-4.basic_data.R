test_csv <- system.file("extdata", 'test.csv', package = "turbidata")
capture.output(data <- turbidata_class$new(test_csv, force_update_cache = TRUE, create_cache_file = FALSE))

path <- tempdir()


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


test_that("gganim", {

  skip_on_cran()
  skip_on_bioc()
  skip_on_ci()
  skip_if_not_installed("gganimate", '1.0.7')
  skip_if_not_installed('gifski', '1.6.6.1')

  output <- capture.output(data$gganim(width = 196,
              height = 144,
              fps = 5,
              export_format = 'gif',
              filename = 'test-gganim.gif',
              output_dir = path
              ))

  target_path <- paste('output: ', path, '/test-gganim.gif', sep = '')
  expect_equal(output, target_path)

  announce_snapshot_file('test-gganim.gif')
  newpath <- normalizePath(paste(path, 'test-gganim.gif', sep = '/'))
  expect_snapshot_file(newpath, 'test-gganim.gif')
})
