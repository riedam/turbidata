#' .import
#' @description
#' Import and clear data from .csv file
#' @import dplyr readxl crayon
#' @param file \code{character}
#' @param dir \code{character} (optional) The folder containing the data file (default \code{"data/"})
#' @param ext \code{character} (optional) The data format. Value available : 'auto', .csv' and '.xls' (or 'xlsx' alias) (default \code{'csv"})
#' @param cache_dir \code{character} (optional) The folder containing the cache file (default \code{"cache/"})
#' @param force_update_cache \code{logical} (optional) Should we ignore the cache value and use the data file (default \code{FALSE})
#' @param create_cache_file \code{logical} (optional) Should we create cache file (default \code{TRUE})
#'
#' @return \code{data.frame} A data.frame containing data
#' @noRd
.import <- function(file = stop("'path' must be specified"),
                    dir = "data/",
                    ext = 'auto',
                    cache_dir = "cache/",
                    force_update_cache = FALSE,
                    create_cache_file = TRUE) {
  # Concatenate different path file
  path <- paste(dir, file, sep = '')
  cache_path <- paste(cache_dir, file, '.rds', sep = '')
  if (force_update_cache) {
    # If force_update_cache == TRUE : don't use cache files
    message(crayon::white('Skip cache file if exist : force update\n'))
  } else if (file.exists(cache_path)) {
    # If exist, the cache file are used
    message(crayon::white(paste("Using cache file to load", file, "\n")))
    data <- readRDS(cache_path, refhook = NULL)
    return(data)
  } else {
    # If cache file doesn't exist
    message(crayon::white(paste(
      "Missing cache file : Loading", file, "\n"
    )))
  }
  # Check if file exist
  if (!file.exists(path))
    stop(paste('file does not exist:', path))
  # This part are not execute if cache file are used
  if (ext == 'auto') {
    ext <- sub('.*(?=.{4}$)', '', file, perl = T)
  }
  if (ext == '.csv') {
    data <- utils::read.csv(
      file = path,
      header = TRUE,
      sep = ";",
      encoding = "UTF-8"
    )
    # Replace ',' by '.' to convert into number
    data <-
      data %>% mutate(across(everything(),  ~ gsub(",", ".", .)))
  } else if (ext == '.xls' | ext == 'xlsx') {
    data <- suppressMessages(suppressWarnings(read_excel(path)))
  } else {
    stop(paste(
      ext,
      "is not an available format, use 'csv' or 'xlsx' (or 'xls' alias)"
    ))
  }

  # Definition of rename range and new name
  # TODO: Ajouter un system pour check si T et R présent et check si col 1 = 0
  from_range <- 1:ncol(data)
  ln2 <- (ncol(data) - 1) / 2
  to <-
    c("height", paste(rep(0:(ln2 - 1), rep(2, ln2)) * 10, c("T", "R"), sep =
                        "."))

  # rename and mutate to numeric
  data <- data %>%
    rename_at(vars(all_of(from_range)), function(x)
      to) %>%
    filter(!row_number() %in% c(1)) %>%
    mutate_all(function(x)
      as.numeric(x)) %>%
    mutate(height = height * 40)

  message(crayon::white('Creation of cache file\n'))
  if (create_cache_file) {
    # Create directory if not exist
    if (!dir.exists('cache')) {
      dir.create('cache')
    }
    saveRDS(data, file = cache_path)
  }
  return(data)
}
