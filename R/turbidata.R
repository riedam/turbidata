#' Turbidata
#' @import cachem dplyr ggplot2
turbidata <- R6::R6Class(
  "TurbiData",
  private = list(

    ..data = data.frame(),
    ..dump_data_cache = cachem::cache_disk(destroy_on_finalize = TRUE),
    ..name = character(),
    ..type = NULL

  ),
  public = list(

    #' @description Create a new TurbiData object.
    #' @param file \code{character}
    #' @param dir \code{character} (optional) The folder containing the data
    #' @param ext \code{character} (optional) The data format. Value available : 'auto', .csv' and '.xls' (or 'xlsx' alias)
    #' @param cache_dir \code{character} (optional) The folder containing the cache file
    #' @param force_update_cache \code{logical} (optional) Should we ignore the cache value and use the data file
    #' @return A new \code{TurbiData} object.
    initialize = function(file,
                          dir = options()$turbidata.data_dir,
                          ext = "auto",
                          cache_dir = "cache/",
                          force_update_cache = FALSE) {
      # Create directory if not exist
      if (!dir.exists('cache')) {
        dir.create('cache')
      }
      # Importation of data
      dump_data <-
        turbidata:::.import(
          file,
          dir = dir,
          ext = ext,
          cache_dir = cache_dir,
          force_update_cache = force_update_cache
        )
      private$..dump_data_cache$set("dump_data", dump_data)
      self$use_absolute()
    },

    #' @description The function use to \code{print()} class
    #' @return \code{NULL}
    print = function() {
      cat("<TurbiData>\n")
      cat(paste("    Data of", private$..name, "file\n"))
      cat(paste("    Type:", private$..type), "\nData:\n")
      print(as_tibble(private$..data), n=3)
    },

    #' @description Allows you to calculate on the absolute values of the data.frame
    #' @return \code{self}
    use_absolute = function() {
      private$..data <- turbidata:::.pivot_longer(self$dump_data)
      private$..type <- "absolute"
      invisible(self)
    },

    #' @description Allows you to calculate on the relative values of the data.frame
    #' @param r (optional) the time used as reference for the backscatter
    #' @param t (optional) the time used as reference for the transmission
    #' @return \code{self}
    use_relative = function(r = 1, t = 1) {
      relative <- turbidata:::.get_relative(
        self$dump_data,
        reference = c(".T" = t, ".R" = r),
        signal = private$..signal
        )
      private$..data <- turbidata:::.pivot_longer(relative)
      private$..type <- "relative"
      invisible(self)
    },

    #' @description
    #' This function makes it possible to calculate the area of the average values located between \code{min} and \code{max}
    #' @param mini \code{number} (optional) The minimum height
    #' @param maxi \code{number} (optional) The maximum height
    #' @return A clone of self with the clean data
    average_area = function(mini=min(private$..data['height']),
                            maxi=max(private$..data['height'])
                            ) {
      stopifnot(mini %% 40 == 0 & maxi %% 40 == 0)
      c <- self$clone()
      a <- private$..data %>%
        filter(mini <= height & height <= maxi) %>%
        group_by(time, signal) %>%
        summarise(mean=mean(do, na.rm=TRUE), time=time, signal=signal, .groups = "keep") %>%
        distinct()
      c$.__enclos_env__$private$..data <- a
      c$.__enclos_env__$private$..type <- "average_area"
      return(c)
    },

    #' @description Make plot with data
    #' @param title \code{character} (optional) The name of plot
    #' @param x \code{character} (optional) The labs for x axis
    #' @param y \code{character} (optional) The labs for y axis
    #' @param color \code{character} (optional) The labs for color gradient
    ggplot = function(title = NULL,
                      x = paste("Height (","\uB5","m)", sep = ""),
                      y = paste("\u0394","DO", sep=""),
                      color = "Time (min)"
                      ) {
      data <- drop_na(private$..data)
      if (private$..type %in% c("relative", "absolute")) {
        p <- ggplot(data, aes(x = height, y = do, group = time)) +
          geom_line(size = 0.05, aes(color = time)) +
          facet_grid(signal ~ .) +
          labs(title = title, x = x, y = y, color = color) +
          scale_colour_gradient2(low = "blue",
                                 mid = "green",
                                 high = "red",
                                 midpoint = mean(data$time),
                                 space = "Lab"
          )
      } else if (private$..type == "average_area") {
        p <- ggplot(data, aes(x = time, y = mean)) +
          geom_point() +
          facet_grid(signal ~ .)
      } else {
        stop(paste(private$..type, "is not a valid value"))
      }
      return(p)
    }

  ),
  active = list(

    #' @field data The \code{data.frame} (read only)
    data = function() {private$..data},

    #' @field dump_data \code{data.frame} The dump_data (read only)
    dump_data = function() {
      private$..dump_data_cache$get("dump_data")
    },

    #' @field name \code{character} The name of the sample
    name = function(value) {
      if (missing(value))
        return(private$..name)
      else {
        stopifnot(is.character(value))
        private$..name <- value
      }
    },

    #' @field type \code{character} Can be \code{"absolute"} or \code{"relative"} : the type of actual data
    type = function() {private$..type}

  )
)
