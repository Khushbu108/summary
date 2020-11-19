# The following 'testthat' function contains three tests for the 'qstat_summary' function.
# The first test checks that the function outputs the expected statistic value in the right column of the output table (in this case, the 'mean' statistic).
# The second test checks that the NA handling of the function works properly
# The final test checks that an error is given for inputs that not numeric vectors (such as lists).

test_that("Testing the qstat_summary function", {
  expect_equal(mean(1:5), qstat_summary(1:5)[[1]])
  expect_equal(mean(1:5), qstat_summary(c(1:5, NA), removeNA = TRUE)[[1]])
  expect_error(qstat_summary(c(list(1:5, 6:10))), "not TRUE")
})
