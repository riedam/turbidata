#' TurbiDumpData
#' @description
#' R6 Class representing the raw values of the turbiscan
turbidata <- R6::R6Class(
  "TurbiDumpData",
  private = list(

    ..data = data.frame(),
    ..signal = c(".T", ".R"),
    ..name = character(),
    ..dump_data_fille = character(),

    # Check class of self object. If it's not an inherit class object : return file, else return dump data store in tmp file
    ..get_dumpdata = function() {
      if (class(self)[1] == "TurbiDumpData") {
        private$..dump_data_fille <- tempfile(pattern = paste(private$..name, '-', sep = ''), tmpdir = ".tmp", fileext = ".rds")
        saveRDS(private$..data, file = private$..dump_data_fille)
        return(private$..data)
      } else if (file.exists(private$..dump_data_fille)) {
          return(readRDS(private$..dump_data_fille))
      } else {
        stop("Error in load dump data. Please recreate new object with turbidata()")
      }
    }

  ),
  public = list(
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
      # Create directory if not exist
      if (!dir.exists('cache')) {
        dir.create('cache')
      }
      if (!dir.exists('.tmp')) {
        dir.create('.tmp')
      }
      # Importation of data
      private$..data <-
        turbidata:::.import(
          file,
          dir = dir,
          ext = ext,
          cache_dir = cache_dir,
          force_update_cache = force_update_cache
        )
    },

    #' @description Remove tmp dump data file
    #' @return \code{NULL}
    finalize = function() {
      unlink(private$..dump_data_fille)
    },

    #' @description Allows you to calculate on the relative values of the data.frame
    #' @param r (optional) the time used as reference for the backscatter (default: \code{1})
    #' @param t (optional) the time used as reference for the transmission (default: \code{1})
    #' @return A new [TurbiRelativeData][turbidata::relative_turbidata] object.
    use_relative = function(r = 1, t = 1) {
      # Get dump data, useful with inherit class
      dump_data <- private$..get_dumpdata()
      # Get relative data
      relative <-
        turbidata:::.get_relative(dump_data,
                                  reference = c(".T" = t, ".R" = r),
                                  signal = private$..signal)
      # Transform data.frame in linear data.frame
      relative <- turbidata:::.pivot_longer(relative)
      return(turbidata::relative_turbidata$new(relative, private$..signal, private$..name, private$..dump_data_fille))
    },

    #' @description Allows you to calculate on the absolute values of the data.frame
    #' @param r (optional) the time used as reference for the backscatter (default: \code{1})
    #' @param t (optional) the time used as reference for the transmission (default: \code{1})
    #' @return A new [TurbiAbsoluteData][turbidata::relative_turbidata] object.
    use_absolute = function(r = 1, t = 1) {
      # Get dump data, useful with inherit class
      dump_data <- private$..get_dumpdata()
      # Transform data.frame in linear data.frame
      absolute <- turbidata:::.pivot_longer(dump_data)
      return(turbidata::absolute_turbidata$new(absolute, private$..signal, private$..name, private$..dump_data_fille))
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
    },

    #' @field data The \code{data.frame} (read only)
    data = function() return(private$..data)
  )
)
