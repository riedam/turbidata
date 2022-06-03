#' TurbiDumpData
#' @description
#' R6 Class representing the raw values of the turbiscan
turbidata <- R6::R6Class(
  "TurbiDumpData",
  private = list(
    ..data = data.frame(),
    ..signal = c(".T", ".R"),
    ..name = character(),

    shared = {
      env <- new.env()
      env
    }

  ),
  public = list(
    #' initialize
    #' @description Create a new TurbiDumpData object.
    #' @param file \code{character}
    #' @param dir \code{character} (optional) The folder containing the data file (default \code{"data/"})
    #' @param ext \code{character} (optional) The data format. Value available : 'auto', .csv' and '.xls' (or 'xlsx' alias) (default \code{'csv'})
    #' @param cache_dir \code{character} (optional) The folder containing the cache file (default \code{"cache/"})
    #' @param force_update_cache \code{logical} (optional) Should we ignore the cache value and use the data file (default \code{FALSE})
    #' @param create_cache_file \code{logical} (optional) Should we create cache file (default \code{TRUE})
    #' @return A new \code{TurbiDumpData} object.
    initialize = function(file,
                          dir = "data/",
                          ext = 'auto',
                          cache_dir = "cache/",
                          force_update_cache = FALSE) {
      private$..data <-
        turbidata:::.import(
          file,
          dir = dir,
          ext = ext,
          cache_dir = cache_dir,
          force_update_cache = force_update_cache
        )
    },

    #' use_relative
    #' @description Allows you to calculate on the relative values of the data.frame
    #' @param r (optional) the time used as reference for the backscatter (default: \code{1})
    #' @param t (optional) the time used as reference for the transmission (default: \code{1})
    #' @return A new [TurbiRelativeData][turbidata::relative_turbidata] object.
    use_relative = function(r = 1, t = 1) {
      reference <-  c(".T" = t, ".R" = r)
      relative <-
        turbidata:::.get_relative(private$..data,
                                  reference = reference,
                                  signal = private$..signal)
      relative <- turbidata:::.pivot_longer(relative)
      return(turbidata::.relative_turbidata$new(relative, private$..signal))
    },

    #' use_absolute
    #' @description Allows you to calculate on the absolute values of the data.frame
    #' @param r (optional) the time used as reference for the backscatter (default: \code{1})
    #' @param t (optional) the time used as reference for the transmission (default: \code{1})
    #' @return A new [TurbiAbsoluteData][turbidata::relative_turbidata] object.
    use_absolute = function(r = 1, t = 1) {
      absolute <- turbidata:::.pivot_longer(private$..data)
      return(turbidata::.absolute_turbidata$new(absolute, private$..signal))
    }

  ),
  active = list(
    #' @field name \code{character} The name of the sample
    name = function(value) {
      if (missing(value))
        return(private$..name)
      else {
        stopifnot(is.character(value))
        private$..name <- value
      }
    }
  )
)

#' TurbiRelativeData
#' @description
#' R6 Class representing the data convert in relative value.
#' @section Warning:
#' Do not use $new() of this class directly : use [TurbiDumpData$use_relative()][turbidata::turbidata]
relative_turbidata <- R6::R6Class(
  "TurbiRelativeData",
  inherit = turbidata,
  public = list(
    #' initialize
    #' @description Create a new TurbiRelativeData object.
    #' @param data \code{data.frame} the relative data.frame
    #' @param signal \code{vector} A vector containing what value are contain in data.frame
    #' @return A new \code{TurbiRelativeData} object.
    initialize = function(data, signal) {
      private$..data <- data
      private$..signal <- signal
    }

  )
)

#' TurbiAbsoluteData
#' @description
#' R6 Class representing the data convert in relative value.
#' @section Warning:
#' Do not use $new() of this class directly : use [TurbiDumpData$use_absolute()][turbidata::turbidata]
absolute_turbidata <- R6::R6Class(
  "TurbiAbsoluteData",
  inherit = turbidata,
  public = list(
    #' initialize
    #' @description Create a new TurbiRelativeData object.
    #' @param data \code{data.frame} the relative data.frame
    #' @param signal \code{vector} A vector containing what value are contain in data.frame
    #' @return A new \code{TurbiRelativeData} object.
    initialize = function(data, signal) {
      private$..data <- data
      private$..signal <- signal
    }

  )
)
