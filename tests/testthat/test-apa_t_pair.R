test_that("exists", {
  expect_true(is.function(demopkg::apa_t_pair))
  })

test_that("errors", {
  expect_error(apa_t_pair())
})

test_that("defaults", {
  xcol <- c(1,2,3,4,5)
  ycol <- c(2,3,2,5,6)

  result <- apa_t_pair(xcol, ycol)
  expected <- "A paired-samples t-test was conducted to compare dv between level 1 (M = 3.0, SD = 1.6) and level 2 (M = 3.6, SD = 1.8). There was a non-significant difference; t(4) = -1.50, p = 0.208."
  expect_equal(result, expected)
})

test_that("different vector numbers", {
  xcol <- c(1,2,3,4,5)
  ycol <- c(2,3,2,5,6,7)

  expect_error(apa_t_pair(xcol, ycol),
               "The arguments x and y need to have the same number of values.")
})

test_that("dv works", {
  xcol <- c(1,2,3,4,5)
  ycol <- c(2,3,2,5,6)
  dv <- "random"

  result <- apa_t_pair(xcol, ycol, dv)
  expected <- "A paired-samples t-test was conducted to compare random between level 1 (M = 3.0, SD = 1.6) and level 2 (M = 3.6, SD = 1.8). There was a non-significant difference; t(4) = -1.50, p = 0.208."
  expect_equal(result, expected)
})
