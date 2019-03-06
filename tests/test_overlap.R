library(testthat)

test_that('Overlap analysis works as expected', {
  M <- matrix(c(
      1, 1, 0,
      1, 0, 0,
      1, 0, 0,
      0, 0, 1,
      0, 1, 1), byrow = T, ncol = 3)

  testthat::expect_equal(all(eps(
    overlap_analysis(M, method = 'corrob'), 
    matrix(c(
        1.0, 0.333, 0,
        0.5, 1, 0.5,
        0, 0.5, 1.0), byrow = T, ncol = 3))), TRUE)

  testthat::expect_equal(all(eps(
    overlap_analysis(M, method = 'jaccard'), 
    matrix(c(
        1.0, 0.25, 0,
        0.25, 1, 0.333,
        0, 0.333, 1.0), byrow = T, ncol = 3))), TRUE)
})
