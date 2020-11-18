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
