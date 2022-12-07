skipped <- c(FALSE, '')

test_that("no duplicate version", {
  skip_if_offline('api.github.com')

  request <- httr::GET('https://api.github.com/repos/riedam/turbidata/releases',  httr::accept_json())
  if (request$status_code != 200) {
    skipped <<- c(TRUE, request$status_code)
    content <- ''
    try({
      content <- httr::content(request)
      content <- paste('\n\n', content, sep='')
      },
      silent = TRUE
      )
    skip(paste('Http error', request$status_code, content))
  }

  latest <-  httr::content(request)[[1]]$tag_name

  curent <- as.character(utils::packageVersion('turbidata'))
  version_differency <- utils::compareVersion(latest, curent)
  if (version_differency > 0) {
    expect(FALSE,
           sprintf('The version %s of this development version is lower than the latest release (%s)',
                  curent,
                  latest
                  )
           )
  } else if (version_differency == 0) {
      expect(TRUE, 'It can be voluntary so accepted')
      warning(sprintf('The version %s of this development version is identical to the latest release (%s)',
                     curent,
                     latest
                     )
              )
  } else {
    expect(TRUE, 'Version ok')
  }

  # Check turbidata_version_check() function
  output <- capture.output(turbidata_version_check())
  expect_match(output, "^You are up to date, curent version : [0-9]+.[0-9]+.[0-9]+$")
})
