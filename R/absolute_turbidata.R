#' TurbiAbsoluteData
#' @description
#' R6 Class representing the data convert in relative value.
#' @section Warning:
#' Do not use $new() of this class directly : use [TurbiDumpData$use_absolute()][turbidata::turbidata]
absolute_turbidata <- R6::R6Class(
  "TurbiAbsoluteData",
  inherit = turbidata,
  public = list(
    #' @description Create a new TurbiRelativeData object.
    #' @param data \code{data.frame} the relative data.frame
    #' @param signal \code{vector} A vector containing what value are contain in data.frame
    #' @param name \code{character} The name of the sample
    #' @param dump_data_fille \code{character} The path of tmp dump data file
    #' @return A new \code{TurbiRelativeData} object.
    initialize = function(data, signal, name, dump_data_fille) {
      private$..data <- data
      private$..signal <- signal
      private$..name <- name
      private$..dump_data_fille <- dump_data_fille
    }

  )
)
