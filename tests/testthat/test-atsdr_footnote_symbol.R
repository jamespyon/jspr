test_that("atsdr_footnote_symbol() outputs single footnote", {
  expect_equal(atsdr_footnote_symbol(1), "*")
})

test_that("atsdr_footnote_symbol() outputs footnotes 1 through 10", {
  expect_equal(atsdr_footnote_symbol(1:10), c("*", "†", "‡", "§", "¶", "**", "††", "‡‡", "§§", "¶¶"))
})
