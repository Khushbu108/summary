test_that("Testing the qstat_summary function", {
  expect_equal(mean(1:5), qstat_summary(1:5)[[1]])
  expect_equal(mean(1:5), qstat_summary(c(1:5, NA), removeNA = TRUE)[[1]])
  expect_error(qstat_summary(c(list(1:5, 6:10))), "not TRUE")
})
