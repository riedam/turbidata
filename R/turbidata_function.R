#' turbidata_version_check
#' @description Check if package are up to date
#' @return NULL
#' @export
turbidata_version_check <- function() {
  curent <- as.character(utils::packageVersion('turbidata'))

  request <- httr::GET('https://api.github.com/repos/riedam/turbidata/releases',  httr::accept_json())
  if (request$status_code != 200) {stop('http error ', request$status_code)}

  latest <-  httr::content(request)[[1]]$tag_name

  if (utils::compareVersion(latest, curent) > 0) {
    cat('New version available! Curent version : ', curent, ', Latest version: ', latest, sep = '')
  } else {
    cat('You are up to date, curent version :', curent)
  }
}

#' turbidata
#' @description turbidata() : Wrapper for [Turbidata class][turbidata::turbidata_class]
#' @param ... Argument transfert to turbidata_class$new()
#' @export
turbidata <- function(...) {
  turbidata_class$new(...)
}
