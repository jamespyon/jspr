test_that("barkjohn_correction() works", {
  expect_equal(barkjohn_correction(0.5, 30, warning = FALSE), 3.426)
})
