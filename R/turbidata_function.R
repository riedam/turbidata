#' turbidata
#' @description turbidata() : Wrapper for [Turbidata class][turbidata::turbidata_class]
#' @param ... Argument transfert to turbidata_class$new()
#' @export
turbidata <- function(...) {
  turbidata:::turbidata_class$new(...)
}
