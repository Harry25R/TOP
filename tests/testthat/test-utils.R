context("Utility functions")

test_that("pairwise_col_diff computes column differences", {
  mat <- matrix(1:8, nrow = 2, byrow = TRUE)
  colnames(mat) <- c("A", "B", "C", "D")
  result <- pairwise_col_diff(mat)
  expect_equal(ncol(result), choose(ncol(mat), 2))
  expect_equal(nrow(result), nrow(mat))
  expect_setequal(colnames(result), c("A--B", "A--C", "A--D", "B--C", "B--D", "C--D"))
})
