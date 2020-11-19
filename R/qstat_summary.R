#' General Statistical Summary
#'
#' @param x a numeric vector for which the statistical summary is to be computed. 'x' cannot be a list, and the input vector should not contain NA values unless the 'removeNA' argument is specified to be TRUE.
#' @param removeNA a logical argument for the handling of NA values in the input vector. The function will display an error message if NA values are contained in the input vector (default set to 'removeNA = FALSE'); specify 'removeNA = TRUE' to remove NA values from the input vector before summary statistics are calculated.
#' @param verbose a logical argument allowing the user to specify whether they would like feedback on the workings of the function, while the function is running on the input vector. Default is set to 'verbose = FALSE', so that no messages are displayed, if this argument has not been explicitly specified.
#' @param ... additional arguments that can be passed on to the mean, sd, max, min and quantile functions within the main function.
#'
#' @return
#' A tibble consisting of seven columns and a single row, where the entry for each column is a calculated general statistic (mean, standard deviation, extrema and quartiles) for the input numeric vector.
#' @export
#'
#' @examples
#' qstat_summary(c(1.2, 5.6, 11.2, 4.9, 3.8))
qstat_summary <- function (x, removeNA = FALSE, verbose = FALSE, ...) {
  if (verbose) message("Calculating summary statistics...")
  Mean <- mean(x, na.rm = removeNA, ...)
  `Standard Deviation` <- stats::sd(x, na.rm = removeNA)
  Minimum <- min(x, na.rm = removeNA, ...)
  `First Quartile` <- stats::quantile(x, probs = 0.25, na.rm = removeNA, ...)
  Median <- stats::quantile(x, probs = 0.5, na.rm = removeNA, ...)
  `Third Quartile` <- stats::quantile(x, probs = 0.75, na.rm = removeNA, ...)
  Maximum <- max(x, na.rm = removeNA, ...)
  if (verbose) message ("Creating statistical summary table...")
  stat_table <- tibble::tibble(Mean,
                       `Standard Deviation`,
                       Minimum,
                       `First Quartile`,
                       Median,
                       `Third Quartile`,
                       Maximum)
  return(stat_table)
}
