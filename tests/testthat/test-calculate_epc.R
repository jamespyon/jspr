test_that("calculate_epc() calculates epc", {
  results <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  nondetects <- results<8
  expect_equal(calculate_epc(obs = results, cen = nondetects)$epc, 10.090688)
})
