#' .pivot_longer
#' @description
#' Transform a double-entry table into a linear table
#' @import tidyr
#' @param table \code{list}: the double-entry table
#' @return \code{list}: a linear table
#' @noRd
.pivot_longer <- function(table) {
  # Transformation of a double-entry table into a linear table{
  table <- tidyr::pivot_longer(table,
                        cols = -1,
                        names_to = c("time", "signal"),
                        names_sep = "[^[:alnum:]]+",
                        values_to = "do")
  # convert time and do column into integer
  table$time <- as.integer(table$time)
  return(table)
}


#' .get_relative
#' @description
#' Get relative data frame compared to reference time
#' @import dplyr
#' @param data \code{list}: absolute data frame
#' @param reference \code{int} (optional): the time used as reference (default to \code{c(".T" = 1, ".R" = 1)})
#' @param signal \code{vector} (optional) A vector with signal, can be \code{c(".T")}, \code{c(".R")} or \code{c(".T", ".R)} (default: \code{c(".T", ".R)})
#' @return \code{list}: relative data frame
#' @noRd
.get_relative <- function(data, reference = c(".T" = 1, ".R" = 1), signal = c(".T", ".R")) {
  r <- data[, 1, drop = FALSE]
  for (i in c(".T", ".R")) {
    temp <- data %>%
      select(ends_with(i))
    n <- reference[i]
    temp[n:ncol(temp)] <- temp[n:ncol(temp)] - temp[[n]]
    r <- cbind(r, temp)
  }
  return(r)
}
