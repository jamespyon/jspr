test_that("calculate_epc() calculates epc", {
  results <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  nondetects <- results<8
  expect_equal(calculate_epc(obs = results, cen = nondetects)$epc, 10.090688)
})

test_that("calculate_epc() test with contaminants data", {
  data <- jspr::contaminants |>
    dplyr::mutate(non_detect = dplyr::case_when(detected_flag == 0 ~ TRUE,
                                                detected_flag == 1 ~ FALSE))
  output <- data |>
    dplyr::filter(exposure_unit != "BLDG 1000") |>
    dplyr::group_by(exposure_unit, media, contaminant) |>
    dplyr::summarise(epc = jspr::calculate_epc(concentration, non_detect)$epc)

  expect_equal(signif(output$epc, 2), c(1, 12, 5.6, 0.17, 7.40, 7.90, 9.80, 1900, 1200, 2.60, 0.21, 2.70, 1500))
})
